;;; agent-shell-feishu.el --- Feishu/Lark bridge for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A two-way bridge between `agent-shell' buffers and Feishu/Lark chats,
;; driven by the official Lark/Feishu CLI (`lark-cli').
;;
;; Model: chat-per-session.  Each bridged `agent-shell' buffer (one ACP
;; session) is bound to a distinct Feishu chat, and the chat_id is the
;; routing key.  A single shared `lark-cli event consume' process
;; receives every inbound message once and dispatches it to the buffer
;; bound to that chat.
;;
;; Outbound (Emacs -> Feishu): the agent's final message and errors are
;; relayed to the session's chat, optionally prefixed with a session
;; label so several chats stay legible.
;;
;; Inbound (Feishu -> Emacs): a message in a bound chat is either
;; injected as a new prompt or, when a permission request is pending for
;; that session, interpreted as the approval answer.
;;
;; This is a reply-based approval design ("Option A"): the approver
;; answers with a number or keyword (e.g. "1", "y", "reject").  It only
;; needs `im +messages-send' and `event consume im.message.receive_v1';
;; interactive card button approvals (`card.action.trigger') are also
;; consumable with `lark-cli' but not used, keeping the bridge simple.
;;
;; Binding a session to a chat:
;;
;;   - Explicit: `C-u M-x agent-shell-feishu-start' prompts for a
;;     chat_id.
;;   - Claim handshake: `M-x agent-shell-feishu-start' with no chat_id
;;     puts the buffer in "awaiting claim"; the next inbound message
;;     from an allowed sender in an unbound chat binds that chat to it.
;;
;; SECURITY: this lets a remote chat drive an agent that can run shell
;; commands.  Only senders in `agent-shell-feishu-allowed-open-ids' are
;; honored, and the list is empty (deny-all) by default.  Ignored
;; senders are logged so you can copy their open_id into the allowlist.
;;
;; Setup:
;;
;;   1. Configure `lark-cli' (`lark-cli config init', then
;;      `lark-cli auth login') with a bot app that has `im:message'
;;      send scope and the `im.message.receive_v1' event subscribed
;;      over long-connection.
;;   2. In an `agent-shell' buffer, add your open_id to
;;      `agent-shell-feishu-allowed-open-ids' and run
;;      `agent-shell-feishu-start'.
;;   3. Message the bot from Feishu to drive the shell.
;;
;; Prerequisites are the user's responsibility; this file only shells
;; out to `lark-cli' and cannot verify console configuration.
;;
;; This module targets stock agent-shell (>= 0.70.2) public events and
;; requires no core changes.  Thinking summaries are read from the
;; transcript at end of turn (no event streams thinking text), and
;; post-tool progress messages are approximated by accumulating
;; `agent-message-chunk' events between tool calls.

;;; Code:

(require 'agent-shell)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(eval-when-compile
  (require 'cl-lib))

;;; Customization

(defgroup agent-shell-feishu nil
  "Feishu/Lark bridge for `agent-shell'."
  :group 'agent-shell)

(defcustom agent-shell-feishu-cli-command "lark-cli"
  "Path to the official Lark/Feishu CLI (`lark-cli') executable."
  :type 'string
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-allowed-open-ids nil
  "List of sender open_ids (\"ou_...\") allowed to drive the shell.

Empty (the default) denies everyone.  Message the bot once and check
the bridge log (see `agent-shell-feishu-log-buffer-name') for the
ignored sender's open_id, then add it here.

This is the primary access control for the bridge: any allowed sender
can inject prompts and approve tool calls, so treat it like an
allowlist of trusted operators."
  :type '(repeat string)
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-command-timeout 20
  "Seconds to allow each outbound send before killing it."
  :type 'integer
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-max-message-length 24000
  "Maximum characters of agent output relayed to Feishu per message."
  :type 'integer
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-relay-turn-complete t
  "When non-nil, relay the agent's final message on each completed turn."
  :type 'boolean
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-message-type 'post
  "Message type used for outbound text-like bridge messages.

`text' sends plain text; `post' sends via `lark-cli''s --markdown,
which wraps content as a rich post."
  :type '(choice (const :tag "Plain text" text)
                 (const :tag "Rich post" post))
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-interrupt-commands
  '("/cc")
  "Inbound text commands that interrupt the bound agent-shell session."
  :type '(repeat string)
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-relay-tool-calls nil
  "When non-nil, relay tool-call status changes to the bound chat.

Each tool call is reported when its status changes (e.g. pending ->
in_progress -> completed), one message per transition.  This is off by
default because it can be chatty."
  :type 'boolean
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-relay-thoughts nil
  "When non-nil, relay a thinking summary to the bound chat at turn end.

No agent-shell event streams thinking text, so the summary is read
from the session transcript when the turn completes."
  :type 'boolean
  :group 'agent-shell-feishu)

(define-obsolete-variable-alias 'agent-shell-feishu-relay-post-tool-messages
  'agent-shell-feishu-relay-progress-messages "0.58")

(defcustom agent-shell-feishu-relay-progress-messages nil
  "When non-nil, relay assistant messages that follow tool use.

Approximated by accumulating `agent-message-chunk' text between tool
calls; a message segment is relayed when the next tool call closes it."
  :type 'boolean
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-label-outbound t
  "When non-nil, prefix outbound messages with a session label.

The label (see `agent-shell-feishu--session-label') keeps multiple
bridged sessions distinguishable, which matters most when they share a
chat.  With a dedicated chat per session it is merely informative."
  :type 'boolean
  :group 'agent-shell-feishu)

(defcustom agent-shell-feishu-log-buffer-name "*agent-shell-feishu-log*"
  "Name of the buffer collecting bridge diagnostics."
  :type 'string
  :group 'agent-shell-feishu)

;;; State

(defvar agent-shell-feishu--bridges nil
  "List of live bridge states, one per bridged shell buffer.

Each entry is the buffer-local `agent-shell-feishu--state' alist of an
active bridge.  Used to route inbound messages and permission requests
to the right session.")

(defvar agent-shell-feishu--consumer nil
  "The single shared `feishu-cli event consume' process, or nil.")

(defvar agent-shell-feishu--consumer-stderr nil
  "Stderr pipe process for the shared consumer, or nil.")

(defvar agent-shell-feishu--consumer-pending-line ""
  "Partial NDJSON line accumulator for the shared consumer.")

(defvar agent-shell-feishu--awaiting-claim nil
  "Shell buffer awaiting a chat binding via the claim handshake, or nil.")

(defvar-local agent-shell-feishu--state nil
  "Buffer-local bridge state for an `agent-shell' buffer.

An alist with keys:
  :buffer        - the bridged shell buffer
  :chat-id       - the Feishu chat_id bound to this session (or nil)
  :subscriptions - agent-shell event subscription tokens
  :pending       - the in-flight permission request, or nil")

;;; Logging

(defun agent-shell-feishu--log (format-string &rest args)
  "Append a timestamped line to the bridge log.

FORMAT-STRING and ARGS are passed to `format'."
  (let ((line (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create agent-shell-feishu-log-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ") line "\n"))))

;;; JSON helpers

(defun agent-shell-feishu--parse-json (string)
  "Parse JSON STRING into an alist, or nil on failure."
  (condition-case err
      (json-parse-string string
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error
     (agent-shell-feishu--log "JSON parse error: %s" (error-message-string err))
     nil)))

;;; Lifecycle

;;;###autoload
(defun agent-shell-feishu-start (&optional chat-id)
  "Start or re-point the Feishu bridge for the current `agent-shell' buffer.

With a prefix argument, prompt for CHAT-ID to bind this session to a
specific chat (taking it over from any other session that holds it).
Otherwise the session enters the claim handshake and is bound by the
next inbound message from an allowed sender.

Running this again in an already-bridged buffer does not error: it
re-points the same bridge.  Without a chat, it detaches the current
chat and waits for the next message to rebind; with a chat it rebinds
immediately.

On first use it subscribes to the shell's outbound events, registers
the reply-based permission responder, and ensures the shared inbound
consumer is running."
  (interactive (list (when current-prefix-arg
                       (read-string "Bind to chat_id: "))))
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (executable-find agent-shell-feishu-cli-command)
    (user-error "Cannot find `%s' on PATH" agent-shell-feishu-cli-command))
  (let* ((shell-buffer (current-buffer))
         (chat-id (and chat-id (not (string-empty-p (string-trim chat-id)))
                       (string-trim chat-id)))
         (state agent-shell-feishu--state)
         (fresh (null state)))
    (when fresh
      (setq state (list (cons :buffer shell-buffer)
                        (cons :chat-id nil)
                        (cons :subscriptions nil)
                        (cons :pending nil)
                        (cons :tool-call-status nil)
                        (cons :progress-text "")
                        (cons :progress-after-tool nil)
                        (cons :last-was-tool nil)))
      (setq agent-shell-feishu--state state)
      (agent-shell-feishu--subscribe shell-buffer state)
      (agent-shell-feishu--register-responder)
      (setq agent-shell-feishu--bridges
            (cons state (delq state agent-shell-feishu--bridges)))
      (agent-shell-feishu--ensure-consumer))
    (cond
     (chat-id
      (agent-shell-feishu--bind-chat state chat-id)
      (agent-shell-feishu--log "%s %s to chat %s"
                               (if fresh "Bridge started for" "Rebound")
                               (buffer-name shell-buffer) chat-id)
      (message "Feishu bridge bound to chat %s" chat-id))
     (t
      ;; No explicit chat: (re-)enter claim mode.  Detach any current chat
      ;; so the next inbound message rebinds this session.
      (when-let* ((current (map-elt state :chat-id)))
        (map-put! state :chat-id nil)
        (map-put! state :pending nil)
        (map-put! state :tool-call-status nil)
        (agent-shell-feishu--log "Detached chat %s from %s for re-claim"
                                 current (buffer-name shell-buffer)))
      (when (and (buffer-live-p agent-shell-feishu--awaiting-claim)
                 (not (eq agent-shell-feishu--awaiting-claim shell-buffer)))
        (agent-shell-feishu--log
         "Warning: replacing pending claim from %s"
         (buffer-name agent-shell-feishu--awaiting-claim)))
      (setq agent-shell-feishu--awaiting-claim shell-buffer)
      (agent-shell-feishu--log "%s %s, awaiting chat claim"
                               (if fresh "Bridge started for" "Re-claiming for")
                               (buffer-name shell-buffer))
      (message "Feishu bridge waiting: message the bot from the desired chat to bind it")))))

;;;###autoload
(defun agent-shell-feishu-stop ()
  "Stop the Feishu bridge for the current `agent-shell' buffer."
  (interactive)
  (let ((state agent-shell-feishu--state))
    (unless state
      (user-error "No Feishu bridge in this buffer"))
    (dolist (token (map-elt state :subscriptions))
      (ignore-errors (agent-shell-unsubscribe :subscription token)))
    (setq agent-shell-feishu--bridges (delq state agent-shell-feishu--bridges))
    (when (eq agent-shell-feishu--awaiting-claim (current-buffer))
      (setq agent-shell-feishu--awaiting-claim nil))
    (setq agent-shell-feishu--state nil)
    (when (null agent-shell-feishu--bridges)
      (agent-shell-feishu--stop-consumer)
      (when (eq agent-shell-permission-responder-function
                #'agent-shell-feishu--responder)
        (setq agent-shell-permission-responder-function nil)))
    (agent-shell-feishu--log "Bridge stopped for %s" (buffer-name))
    (message "Feishu bridge stopped")))

(defun agent-shell-feishu--bind-chat (state chat-id)
  "Bind CHAT-ID to STATE, detaching it from any other session first.

Any other bridge currently holding CHAT-ID has it cleared, then
CHAT-ID is set on STATE.  Both sides' pending approval and tool-call
dedupe tracking are reset so the move starts from a clean slate."
  (when-let* ((previous (agent-shell-feishu--bridge-for-chat chat-id)))
    (unless (eq previous state)
      (map-put! previous :chat-id nil)
      (map-put! previous :pending nil)
      (map-put! previous :tool-call-status nil)
      (agent-shell-feishu--log "Detached chat %s from %s"
                               chat-id
                               (buffer-name (map-elt previous :buffer)))))
  (map-put! state :chat-id chat-id)
  (map-put! state :pending nil)
  (map-put! state :tool-call-status nil))

;;;###autoload
(defun agent-shell-feishu-rebind (chat-id)
  "Bind CHAT-ID to the current buffer's bridge, moving it if needed.

Interactively, chats currently bound to any session are offered for
completion (annotated with their session); a raw chat_id may also be
typed.  If CHAT-ID is already bound to another session, it is detached
from that session and re-bound here.  The current buffer must already
have a bridge (see `agent-shell-feishu-start')."
  (interactive
   (progn
     (unless agent-shell-feishu--state
       (user-error "No Feishu bridge here; run `agent-shell-feishu-start' first"))
     (list (agent-shell-feishu--read-chat-id "Bind chat to this session: "))))
  (let ((state agent-shell-feishu--state)
        (chat-id (string-trim (or chat-id ""))))
    (unless state
      (user-error "No Feishu bridge in this buffer"))
    (when (string-empty-p chat-id)
      (user-error "No chat_id given"))
    (when (equal (map-elt state :chat-id) chat-id)
      (user-error "This session is already bound to %s" chat-id))
    (when (eq agent-shell-feishu--awaiting-claim (map-elt state :buffer))
      (setq agent-shell-feishu--awaiting-claim nil))
    (agent-shell-feishu--bind-chat state chat-id)
    (agent-shell-feishu--log "Rebound chat %s to %s"
                             chat-id (buffer-name (map-elt state :buffer)))
    (agent-shell-feishu--send
     state (format "\U0001F517 This chat now drives session %s"
                   (agent-shell-feishu--session-label (map-elt state :buffer))))
    (message "Feishu chat %s bound to %s"
             chat-id (buffer-name (map-elt state :buffer)))))

(defun agent-shell-feishu--read-chat-id (prompt)
  "Read a chat_id from the minibuffer using PROMPT.

Chats currently bound to a session are offered as completion
candidates, annotated with that session; a raw chat_id may also be
entered."
  (let* ((candidates
          (delq nil
                (seq-map (lambda (state)
                           (when-let* ((cid (map-elt state :chat-id)))
                             (cons (format "%s  (session: %s)"
                                           cid
                                           (buffer-name (map-elt state :buffer)))
                                   cid)))
                         agent-shell-feishu--bridges)))
         (choice (completing-read prompt (mapcar #'car candidates) nil nil)))
    (or (cdr (assoc choice candidates)) (string-trim choice))))

;;; Shared inbound consumer

(defun agent-shell-feishu--ensure-consumer ()
  "Start the shared `lark-cli event consume' process if not running.

Events arrive as NDJSON on stdout; `lark-cli' writes connection
banners and diagnostics to stderr, which is captured separately so it
never reaches the JSON filter."
  (unless (process-live-p agent-shell-feishu--consumer)
    (setq agent-shell-feishu--consumer-pending-line "")
    (setq agent-shell-feishu--consumer-stderr
          (make-pipe-process
           :name "agent-shell-feishu-consume-stderr"
           :buffer (get-buffer-create " *agent-shell-feishu-consume-stderr*")
           :filter #'agent-shell-feishu--stderr-filter
           :noquery t))
    (setq agent-shell-feishu--consumer
          (make-process
           :name "agent-shell-feishu-consume"
           :buffer (get-buffer-create " *agent-shell-feishu-consume*")
           :connection-type 'pipe
           :command (list agent-shell-feishu-cli-command
                          "event" "consume" "im.message.receive_v1"
                          "--as" "bot")
           :filter #'agent-shell-feishu--filter
           :stderr agent-shell-feishu--consumer-stderr
           :sentinel #'agent-shell-feishu--consumer-sentinel))
    (agent-shell-feishu--log "Shared consumer started")))

(defun agent-shell-feishu--stop-consumer ()
  "Stop the shared consumer process, if any."
  (when (process-live-p agent-shell-feishu--consumer)
    (delete-process agent-shell-feishu--consumer))
  (when (process-live-p agent-shell-feishu--consumer-stderr)
    (delete-process agent-shell-feishu--consumer-stderr))
  (setq agent-shell-feishu--consumer nil)
  (setq agent-shell-feishu--consumer-stderr nil)
  (setq agent-shell-feishu--consumer-pending-line "")
  (agent-shell-feishu--log "Shared consumer stopped"))

(defun agent-shell-feishu--consumer-sentinel (process change)
  "Log when the consume PROCESS exits.  CHANGE is the status string."
  (when (memq (process-status process) '(exit signal))
    (agent-shell-feishu--log "Consumer exited: %s" (string-trim change))))

(defun agent-shell-feishu--stderr-filter (_process output)
  "Log consumer diagnostics OUTPUT with a [consume] prefix."
  (dolist (line (split-string (string-trim output) "\n" t))
    (agent-shell-feishu--log "[consume] %s" (string-trim line))))

(defun agent-shell-feishu--filter (_process output)
  "Process filter accumulating OUTPUT into NDJSON lines.

Only lines that look like a JSON object are handled; anything else is
ignored so stray diagnostics never trip the parser."
  (let* ((pending (concat agent-shell-feishu--consumer-pending-line output))
         (lines (split-string pending "\n")))
    ;; The last element is an incomplete line (or "" when OUTPUT ended
    ;; with a newline); keep it for the next chunk.
    (setq agent-shell-feishu--consumer-pending-line (car (last lines)))
    (dolist (line (butlast lines))
      (setq line (string-trim line))
      (when (string-prefix-p "{" line)
        (agent-shell-feishu--handle-line line)))))

(defun agent-shell-feishu--handle-line (line)
  "Handle one NDJSON LINE, routing it to the bound session's buffer."
  (when-let* ((event (agent-shell-feishu--parse-json line))
              (message-data (map-nested-elt event '(event message)))
              (sender (map-nested-elt event '(event sender sender_id open_id))))
    (let ((chat-id (map-elt message-data 'chat_id)))
      (cond
       ((not (member sender agent-shell-feishu-allowed-open-ids))
        (agent-shell-feishu--log
         "Ignored message from unlisted sender: %s (add to `agent-shell-feishu-allowed-open-ids')"
         sender))
       ((null chat-id)
        (agent-shell-feishu--log "Incoming message without chat_id; ignoring"))
       (t
        (let ((state (agent-shell-feishu--bridge-for-chat chat-id)))
          (cond
           (state
            (agent-shell-feishu--dispatch-message state message-data))
           ((buffer-live-p agent-shell-feishu--awaiting-claim)
            (setq state (buffer-local-value 'agent-shell-feishu--state
                                            agent-shell-feishu--awaiting-claim))
            (map-put! state :chat-id chat-id)
            (setq agent-shell-feishu--awaiting-claim nil)
            (agent-shell-feishu--log "Bound chat %s to %s"
                                     chat-id
                                     (buffer-name (map-elt state :buffer)))
            (agent-shell-feishu--send
             state (format "\U0001F517 Bound to session %s"
                           (agent-shell-feishu--session-label
                            (map-elt state :buffer))))
            (agent-shell-feishu--dispatch-message state message-data))
           (t
            (agent-shell-feishu--log
             "Message in chat %s has no bound session (none awaiting claim); ignoring"
             chat-id)))))))))

(defun agent-shell-feishu--bridge-for-chat (chat-id)
  "Return the bridge state bound to CHAT-ID, or nil."
  (seq-find (lambda (state)
              (and (buffer-live-p (map-elt state :buffer))
                   (equal (map-elt state :chat-id) chat-id)))
            agent-shell-feishu--bridges))

(defun agent-shell-feishu--dispatch-message (state message-data)
  "Route MESSAGE-DATA for the session in STATE.

Non-text messages are declined; text is treated as the answer to a
pending approval, or otherwise injected as a prompt."
  (let ((message-type (map-elt message-data 'message_type))
        (text (agent-shell-feishu--message-text message-data)))
    (cond
     ((not (member message-type '("text" "post")))
      (agent-shell-feishu--send
       state (format "\u26A0 Only text/post messages are supported (got %s)."
                     message-type)))
     ((null text)
      (agent-shell-feishu--log "Empty text content on incoming message"))
     ((agent-shell-feishu--interrupt-command-p text)
      (agent-shell-feishu--interrupt state))
     ((map-elt state :pending)
      (agent-shell-feishu--answer-pending state text))
     (t
      (agent-shell-feishu--inject-prompt state text)))))

(defun agent-shell-feishu--interrupt-command-p (text)
  "Return non-nil if TEXT is configured as an interrupt command."
  (member (downcase (string-trim text))
          agent-shell-feishu-interrupt-commands))

(defun agent-shell-feishu--interrupt (state)
  "Interrupt STATE's bound `agent-shell' buffer from Feishu."
  (let ((shell-buffer (map-elt state :buffer)))
    (condition-case err
        (if (not (buffer-live-p shell-buffer))
            (agent-shell-feishu--send state "⚠ Session buffer is gone; cannot interrupt.")
          (with-current-buffer shell-buffer
            (agent-shell-interrupt t))
          (agent-shell-feishu--send state "⏹ Sent C-c C-c interrupt."))
      (error
       (agent-shell-feishu--send
        state (format "⚠ Could not interrupt: %s" (error-message-string err)))))))

(defun agent-shell-feishu--message-text (message-data)
  "Return the trimmed user text of MESSAGE-DATA, or nil.

MESSAGE-DATA is the `.event.message' alist.  Handles both `text'
messages (content {\"text\":\"hi\"}) and `post' rich-text messages
\\(as sent for group @mentions).  @mention placeholders and at-segments
are dropped."
  (when-let* ((content (map-elt message-data 'content))
              (parsed (agent-shell-feishu--parse-json content))
              (raw (pcase (map-elt message-data 'message_type)
                     ("text" (map-elt parsed 'text))
                     ("post" (agent-shell-feishu--post-plain-text parsed))
                     (_ nil))))
    (let ((cleaned (string-trim
                    (replace-regexp-in-string "@_user_[0-9]+" "" raw))))
      (unless (string-empty-p cleaned)
        cleaned))))

(defun agent-shell-feishu--post-plain-text (parsed)
  "Return plain text from a `post' message's PARSED content alist.

PARSED `content' is a list of paragraphs, each a list of segments.
Text is taken from `text'/`a' segments; `at' (mention) and other
segments are skipped."
  (let ((paragraphs (map-elt parsed 'content))
        (lines nil))
    (dolist (paragraph paragraphs)
      (let ((parts nil))
        (dolist (segment paragraph)
          (when (member (map-elt segment 'tag) '("text" "a"))
            (push (or (map-elt segment 'text) "") parts)))
        (push (string-join (nreverse parts) "") lines)))
    (string-join (nreverse lines) "\n")))

(defun agent-shell-feishu--inject-prompt (state text)
  "Inject TEXT as a submitted prompt into STATE's shell buffer."
  (let ((shell-buffer (map-elt state :buffer)))
    (condition-case err
        (progn
          (agent-shell--insert-to-shell-buffer
           :shell-buffer shell-buffer :text text :submit t :no-focus t)
          (agent-shell-feishu--log "Injected prompt into %s: %s"
                                   (buffer-name shell-buffer)
                                   (agent-shell-feishu--truncate text)))
      (error
       (agent-shell-feishu--send
        state (format "\u26A0 Could not submit prompt: %s"
                      (error-message-string err)))))))

;;; Outbound (agent-shell -> Feishu)

(defun agent-shell-feishu--subscribe (shell-buffer state)
  "Subscribe to SHELL-BUFFER events, storing tokens on STATE."
  (let ((tokens
         (list
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'turn-complete
           :on-event
           (lambda (event)
             (agent-shell-feishu--on-turn-complete state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'agent-message-chunk
           :on-event
           (lambda (event)
             (agent-shell-feishu--on-agent-message-chunk state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'tool-call-update
           :on-event
           (lambda (event)
             (agent-shell-feishu--on-tool-call-update state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'error
           :on-event
           (lambda (event)
             (agent-shell-feishu--on-error state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'clean-up
           :on-event
           (lambda (_event)
             (with-current-buffer shell-buffer
               (when agent-shell-feishu--state
                 (agent-shell-feishu-stop))))))))
    (map-put! state :subscriptions tokens)))

(defun agent-shell-feishu--on-turn-complete (state event)
  "Relay thoughts and the final agent message for STATE's session.

EVENT is the `turn-complete' event alist."
  (when agent-shell-feishu-relay-thoughts
    (when-let* ((summary (agent-shell-feishu--thought-last-paragraph
                          (agent-shell-feishu--last-thoughts
                           (map-elt state :buffer)))))
      (agent-shell-feishu--send
       state (concat agent-shell-thought-process-icon " " summary))))
  (map-put! state :progress-text "")
  (map-put! state :progress-after-tool nil)
  (map-put! state :last-was-tool nil)
  (when agent-shell-feishu-relay-turn-complete
    (let ((text (or (agent-shell-feishu--last-agent-message (map-elt state :buffer))
                    (format "(turn complete: %s)"
                            (map-nested-elt event '(:data :stop-reason))))))
      (agent-shell-feishu--send state (concat "\U0001F916 " text)))))

(defun agent-shell-feishu--last-thoughts (shell-buffer)
  "Return SHELL-BUFFER's last thoughts section from its transcript, or nil.

No agent-shell event streams thinking text, so thought relay reads the
transcript's final \"## Agent's Thoughts\" section at end of turn."
  (when-let* ((file (buffer-local-value 'agent-shell--transcript-file shell-buffer))
              ((stringp file))
              ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (when (re-search-backward "^## Agent's Thoughts (" nil t)
        (forward-line 1)
        (let ((start (point)))
          (buffer-substring-no-properties
           start
           (if (re-search-forward "^## " nil t)
               (match-beginning 0)
             (point-max))))))))

(defun agent-shell-feishu--on-agent-message-chunk (state event)
  "Accumulate EVENT's text chunk on STATE for progress relay.

Text that starts right after a tool call is marked as a progress
message; `agent-shell-feishu--on-tool-call-update' relays it when the
next tool call closes it."
  (when agent-shell-feishu-relay-progress-messages
    (when-let* ((chunk (map-nested-elt event '(:data :text-chunk))))
      (when (string-empty-p (or (map-elt state :progress-text) ""))
        (map-put! state :progress-after-tool (map-elt state :last-was-tool)))
      (map-put! state :last-was-tool nil)
      (map-put! state :progress-text
                (concat (map-elt state :progress-text) chunk)))))

(defun agent-shell-feishu--thought-last-paragraph (text)
  "Return the last non-empty paragraph from thinking TEXT."
  (when-let* ((clean (and text (string-trim text)))
              ((not (string-empty-p clean))))
    (let* ((paragraphs (split-string clean "\n[ \t]*\n" t))
           ;; Cursor's collapsed Thinking preview is usually the final
           ;; summary-like paragraph, so mirror that in Feishu.
           (last-block (car (last paragraphs)))
           (one-line (string-join (split-string (string-trim last-block)) " ")))
      (agent-shell-feishu--truncate one-line))))

(defun agent-shell-feishu--on-tool-call-update (state event)
  "Flush progress text and relay a tool-call status change for STATE.

EVENT is a `tool-call-update' event alist.  To avoid duplicate
messages (the event fires on both creation and each update), only
status transitions are relayed, tracked per tool-call id on STATE."
  ;; A tool call closes any assistant text preceding it; relay that text
  ;; when it itself followed a tool call (a progress message).
  (when (and agent-shell-feishu-relay-progress-messages
             (map-elt state :progress-after-tool))
    (when-let* ((text (string-trim (or (map-elt state :progress-text) "")))
                ((not (string-empty-p text))))
      (agent-shell-feishu--send
       state (concat "💬 " (agent-shell-feishu--truncate text)))))
  (map-put! state :progress-text "")
  (map-put! state :progress-after-tool nil)
  (map-put! state :last-was-tool t)
  (when agent-shell-feishu-relay-tool-calls
    (agent-shell-feishu--relay-tool-status state event)))

(defun agent-shell-feishu--relay-tool-status (state event)
  "Relay EVENT's tool-call status transition for STATE."
  (let* ((data (map-elt event :data))
         (id (map-elt data :tool-call-id))
         (tool-call (map-elt data :tool-call))
         (status (map-elt tool-call :status))
         (title (or (map-elt tool-call :title) "tool call"))
         (seen (map-elt state :tool-call-status)))
    (when (and id (not (equal (map-elt seen id) status)))
      (map-put! state :tool-call-status
                (cons (cons id status)
                      (assoc-delete-all id (copy-sequence seen))))
      (agent-shell-feishu--send
       state (format "%s %s%s"
                     (agent-shell-feishu--tool-status-icon status)
                     title
                     (if status (format " [%s]" status) ""))))))

(defun agent-shell-feishu--tool-status-icon (status)
  "Return an icon string for tool-call STATUS."
  (pcase status
    ("completed" "\u2705")
    ("failed" "\u274C")
    ("in_progress" "\u2699\uFE0F")
    (_ "\U0001F527")))

(defun agent-shell-feishu--on-error (state event)
  "Relay an ACP EVENT error to STATE's chat."
  (let ((message (map-nested-elt event '(:data :message))))
    (agent-shell-feishu--send
     state (format "\u26A0 Error: %s" (or message "unknown")))))

(defun agent-shell-feishu--last-agent-message (shell-buffer)
  "Return SHELL-BUFFER's last agent message from its transcript, or nil."
  (when-let* ((file (buffer-local-value 'agent-shell--transcript-file shell-buffer))
              ((stringp file))
              ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (when (re-search-backward "^## Agent (" nil t)
        (forward-line 1)
        (let ((text (string-trim (buffer-substring-no-properties
                                  (point) (point-max)))))
          (unless (string-empty-p text)
            (agent-shell-feishu--truncate text)))))))

(defun agent-shell-feishu--truncate (text)
  "Truncate TEXT to `agent-shell-feishu-max-message-length'."
  (if (> (length text) agent-shell-feishu-max-message-length)
      (concat (substring text 0 agent-shell-feishu-max-message-length)
              "\n… (truncated)")
    text))

(defun agent-shell-feishu--session-label (shell-buffer)
  "Return a short human label identifying SHELL-BUFFER's session."
  (buffer-name shell-buffer))

(defun agent-shell-feishu--send (state text)
  "Send TEXT to the Feishu chat bound to STATE.

Runs `lark-cli im +messages-send' asynchronously as the bot identity.
Does nothing when the session has no bound chat yet."
  (when-let* ((chat-id (map-elt state :chat-id)))
    (let ((labeled (if agent-shell-feishu-label-outbound
                       (format "\u300C%s\u300D\n%s"
                               (agent-shell-feishu--session-label
                                (map-elt state :buffer))
                               text)
                     text)))
      (make-process
       :name "agent-shell-feishu-send"
       :buffer (get-buffer-create " *agent-shell-feishu-send*")
       :connection-type 'pipe
       :command (list "timeout"
                      (number-to-string agent-shell-feishu-command-timeout)
                      agent-shell-feishu-cli-command
                      "im" "+messages-send"
                      "--as" "bot"
                      "--chat-id" chat-id
                      (if (eq agent-shell-feishu-message-type 'post)
                          "--markdown"
                        "--text")
                      labeled)
       :sentinel
       (lambda (process _change)
         (when (memq (process-status process) '(exit signal))
           (unless (zerop (process-exit-status process))
             (agent-shell-feishu--log
              "Send failed (exit %d): %s"
              (process-exit-status process)
              (with-current-buffer (process-buffer process)
                (string-trim (buffer-string)))))))))))

;;; Reply-based permission approvals

(defun agent-shell-feishu--register-responder ()
  "Install the bridge's permission responder if none conflicts.

Only installs `agent-shell-feishu--responder'; leaves any other
user-set responder untouched."
  (when (and agent-shell-permission-responder-function
             (not (eq agent-shell-permission-responder-function
                      #'agent-shell-feishu--responder)))
    (agent-shell-feishu--log
     "Warning: `agent-shell-permission-responder-function' already set to %s; not overriding"
     agent-shell-permission-responder-function))
  (unless agent-shell-permission-responder-function
    (setq agent-shell-permission-responder-function
          #'agent-shell-feishu--responder)))

(defun agent-shell-feishu--bridge-for-tool-call (tool-call)
  "Return the bridge state whose session owns TOOL-CALL, or nil.

The responder is buffer-agnostic, so the session is identified by the
tool-call object stored in each shell buffer's `agent-shell--state'.
Object identity is used because JSON-RPC request ids are only unique
per connection, not across sessions."
  (seq-find
   (lambda (state)
     (let ((buffer (map-elt state :buffer)))
       (and (buffer-live-p buffer)
            (seq-some
             (lambda (candidate) (eq candidate tool-call))
             (map-values
              (map-elt (buffer-local-value 'agent-shell--state buffer)
                       :tool-calls))))))
   agent-shell-feishu--bridges))

(defun agent-shell-feishu--responder (permission)
  "Relay PERMISSION to the owning session's Feishu chat.

Returns non-nil (handled, local UI skipped) when relayed to a bound
chat; nil to fall back to the interactive dialog."
  (when-let* ((tool-call (map-elt permission :tool-call))
              (state (agent-shell-feishu--bridge-for-tool-call tool-call))
              ((map-elt state :chat-id))
              (options (map-elt permission :options)))
    (let ((title (map-elt tool-call :title)))
      (map-put! state :pending
                (list (cons :respond (map-elt permission :respond))
                      (cons :options options)
                      (cons :title title)))
      (agent-shell-feishu--send
       state (agent-shell-feishu--approval-text title options))
      (agent-shell-feishu--log "Permission relayed to %s: %s"
                               (buffer-name (map-elt state :buffer)) title)
      t)))

(defun agent-shell-feishu--approval-text (title options)
  "Return the approval prompt text for TITLE and OPTIONS."
  (concat
   (format "\U0001F510 Permission needed: %s\n\nReply with a number:"
           (or title "tool call"))
   (let ((index 0))
     (mapconcat
      (lambda (option)
        (setq index (1+ index))
        (format "\n %d) %s" index (map-elt option :label)))
      options ""))
   "\n\n(shortcuts: y/allow, n/reject)"))

(defun agent-shell-feishu--answer-pending (state text)
  "Interpret TEXT as the answer to STATE's pending approval."
  (let* ((pending (map-elt state :pending))
         (options (map-elt pending :options))
         (respond (map-elt pending :respond))
         (choice (agent-shell-feishu--match-option text options)))
    (if (not choice)
        (agent-shell-feishu--send
         state (concat "\u26A0 Unrecognized answer. "
                       (agent-shell-feishu--approval-text
                        (map-elt pending :title) options)))
      (map-put! state :pending nil)
      (funcall respond (map-elt choice :option-id))
      (agent-shell-feishu--send
       state (format "\u2705 %s" (map-elt choice :label)))
      (agent-shell-feishu--log "Permission answered in %s: %s"
                               (buffer-name (map-elt state :buffer))
                               (map-elt choice :label)))))

(defun agent-shell-feishu--match-option (reply options)
  "Return the option in OPTIONS matching REPLY, or nil.

Matches a 1-based number, an option's single-key char, or the keywords
y/yes/allow/approve (first allow_* option) and n/no/reject/deny (first
reject_* option)."
  (let ((normalized (downcase (string-trim reply))))
    (or
     ;; Numeric selection (1-based).
     (when (string-match-p "\\`[0-9]+\\'" normalized)
       (let ((n (string-to-number normalized)))
         (when (and (>= n 1) (<= n (length options)))
           (nth (1- n) options))))
     ;; Single-key char (:char), when present on options.
     (seq-find (lambda (option)
                 (when-let* ((char (map-elt option :char))
                             (string (cond ((characterp char) (char-to-string char))
                                           ((stringp char) char))))
                   (equal normalized (downcase string))))
               options)
     ;; Allow keywords.
     (when (member normalized '("y" "yes" "allow" "approve" "ok"))
       (agent-shell-feishu--find-option-by-kind options "allow"))
     ;; Reject keywords.
     (when (member normalized '("n" "no" "reject" "deny" "cancel"))
       (agent-shell-feishu--find-option-by-kind options "reject")))))

(defun agent-shell-feishu--find-option-by-kind (options kind-prefix)
  "Return the first option in OPTIONS whose :kind has prefix KIND-PREFIX."
  (seq-find (lambda (option)
              (when-let* ((kind (map-elt option :kind)))
                (string-prefix-p kind-prefix kind)))
            options))

(provide 'agent-shell-feishu)

;;; agent-shell-feishu.el ends here
