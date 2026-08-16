;;; agent-shell-lark.el --- Lark bridge for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Zhixu Zhao

;; Author: Zhixu Zhao
;; URL: https://github.com/zhaozhixu/emacs.d

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
;; A two-way bridge between `agent-shell' buffers and Lark chats
;; (Lark is known as Feishu in China), driven by the official Lark
;; CLI (`lark-cli').
;;
;; Model: chat-per-session.  Each bridged `agent-shell' buffer (one ACP
;; session) is bound to a distinct Lark chat, and the chat_id is the
;; routing key.  A single shared `lark-cli event consume' process
;; receives every inbound message once and dispatches it to the buffer
;; bound to that chat.
;;
;; Outbound (Emacs -> Lark): the agent's final message and errors are
;; relayed to the session's chat, optionally prefixed with a session
;; label so several chats stay legible.
;;
;; Inbound (Lark -> Emacs): a message in a bound chat is either
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
;;   - Explicit: `C-u M-x agent-shell-lark-start' prompts for a
;;     chat_id.
;;   - Claim handshake: `M-x agent-shell-lark-start' with no chat_id
;;     puts the buffer in "awaiting claim"; the next inbound message
;;     from an allowed sender in an unbound chat binds that chat to it.
;;
;; SECURITY: this lets a remote chat drive an agent that can run shell
;; commands.  Only senders in `agent-shell-lark-allowed-open-ids' are
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
;;      `agent-shell-lark-allowed-open-ids' and run
;;      `agent-shell-lark-start'.
;;   3. Message the bot from Lark to drive the shell.
;;
;; Optional, for `agent-shell-lark-render-formulas' ($$...$$ blocks
;; sent as rendered images; Lark IM has no native formula support):
;;
;;   - node on PATH, plus a one-time `npm install' in
;;     `agent-shell-lark-formula-tools-directory' (~/.emacs.d/lark-tools,
;;     fetches MathJax for the bundled tex2svg.js).
;;   - rsvg-convert on PATH, to rasterize the SVG (the Feishu image API
;;     rejects SVG uploads).  Not shipped with macOS; it comes from
;;     librsvg, which Homebrew's emacs-plus already pulls in as a
;;     dependency (brew install librsvg otherwise; on Debian/Ubuntu:
;;     apt install librsvg2-bin).
;;
;;   When any piece is missing the feature degrades silently to
;;   sending the original text; rendered formulas and their uploaded
;;   image keys are cached by content hash under ~/.cache/agent-shell-lark.
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
(require 'agent-shell-prompt-queue)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(eval-when-compile
  (require 'cl-lib))

;;; Customization

(defgroup agent-shell-lark nil
  "Lark bridge for `agent-shell'."
  :group 'agent-shell)

(defcustom agent-shell-lark-cli-command "lark-cli"
  "Path to the official Lark CLI (`lark-cli') executable."
  :type 'string
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-allowed-open-ids nil
  "List of sender open_ids (\"ou_...\") allowed to drive the shell.

Empty (the default) denies everyone.  Message the bot once and check
the bridge log (see `agent-shell-lark-log-buffer-name') for the
ignored sender's open_id, then add it here.

This is the primary access control for the bridge: any allowed sender
can inject prompts and approve tool calls, so treat it like an
allowlist of trusted operators."
  :type '(repeat string)
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-command-timeout 20
  "Seconds to allow each outbound send before killing it."
  :type 'integer
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-max-message-length 24000
  "Maximum characters of agent output relayed to Lark per message."
  :type 'integer
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-relay-turn-complete t
  "When non-nil, relay the agent's final message on each completed turn."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-message-type 'post
  "Message type used for outbound text-like bridge messages.

`text' sends plain text; `post' sends via `lark-cli''s --markdown,
which wraps content as a rich post."
  :type '(choice (const :tag "Plain text" text)
                 (const :tag "Rich post" post))
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-interrupt-commands
  '("/cc")
  "Inbound text commands that interrupt the bound agent-shell session."
  :type '(repeat string)
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-relay-tool-calls nil
  "When non-nil, relay tool-call status changes to the bound chat.

Each tool call is reported when its status changes (e.g. pending ->
in_progress -> completed), one message per transition.  This is off by
default because it can be chatty."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-relay-thoughts nil
  "When non-nil, relay a thinking summary to the bound chat at turn end.

No agent-shell event streams thinking text, so the summary is read
from the session transcript when the turn completes."
  :type 'boolean
  :group 'agent-shell-lark)

(define-obsolete-variable-alias 'agent-shell-lark-relay-post-tool-messages
  'agent-shell-lark-relay-progress-messages "0.58")

(defcustom agent-shell-lark-relay-progress-messages t
  "When non-nil, relay assistant progress messages to the bound chat.

Progress messages are the text segments the assistant writes between
tool calls (typically the short summaries after a thinking block),
which otherwise stay invisible from Lark during long turns.
Approximated by accumulating `agent-message-chunk' text; a segment is
relayed when the next tool call closes it.  The turn's final segment
is covered by `agent-shell-lark-relay-turn-complete' instead."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-mention-on-turn-complete t
  "When non-nil, at-mention allowed users when a turn completes.

The mention (every open_id in `agent-shell-lark-allowed-open-ids')
is appended to the relayed final message -- at tags render as
mentions in both text and markdown messages.  When
`agent-shell-lark-relay-turn-complete' is nil, a small separate text
ping is sent instead."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-permission-cards t
  "When non-nil, relay permission requests as interactive cards.

Each request gets its own card with one button per option, so several
pending permissions stay unambiguous.  Answered cards are updated in
place to show the outcome.  Requires the app to have the
`card.action.trigger' callback subscribed in the developer console.
When nil, fall back to the reply-based flow (numbered text options
answered by replying in the chat)."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-render-formulas nil
  "When non-nil, render $$...$$ blocks in outbound text as images.

Each display formula is rendered locally (MathJax via node, then
rsvg-convert to PNG), uploaded once (cached by formula hash), and the
$$...$$ block is replaced with an inline image so the formula shows
rendered inside the normal markdown message.  Inline $...$ is left
alone (too easy to confuse with shell variables and code).  When the
tool chain is unavailable or any step fails, the text is sent
unchanged.  Requires node and rsvg-convert on PATH and a one-time
`npm install' in `agent-shell-lark-formula-tools-directory'."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-formula-tools-directory
  (expand-file-name "lark-tools" user-emacs-directory)
  "Directory holding tex2svg.js and its node_modules (MathJax)."
  :type 'directory
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-label-outbound t
  "When non-nil, prefix outbound messages with a session label.

The label (see `agent-shell-lark--session-label') keeps multiple
bridged sessions distinguishable, which matters most when they share a
chat.  With a dedicated chat per session it is merely informative."
  :type 'boolean
  :group 'agent-shell-lark)

(defcustom agent-shell-lark-log-buffer-name "*agent-shell-lark-log*"
  "Name of the buffer collecting bridge diagnostics."
  :type 'string
  :group 'agent-shell-lark)

;;; State

(defvar agent-shell-lark--bridges nil
  "List of live bridge states, one per bridged shell buffer.

Each entry is the buffer-local `agent-shell-lark--state' alist of an
active bridge.  Used to route inbound messages and permission requests
to the right session.")

(defvar agent-shell-lark--consumer nil
  "The single shared `lark-cli event consume' process, or nil.")

(defvar agent-shell-lark--consumer-stderr nil
  "Stderr pipe process for the shared consumer, or nil.")

(defvar agent-shell-lark--consumer-pending-line ""
  "Partial NDJSON line accumulator for the shared consumer.")

(defvar agent-shell-lark--card-consumer nil
  "The shared `card.action.trigger' consumer process, or nil.")

(defvar agent-shell-lark--card-consumer-stderr nil
  "Stderr pipe process for the card consumer, or nil.")

(defvar agent-shell-lark--card-consumer-pending-line ""
  "Partial NDJSON line accumulator for the card consumer.")

(defvar agent-shell-lark--pending-permissions nil
  "Alist of (PID . ENTRY) for permissions relayed as cards.

ENTRY is an alist with `:respond', `:options', `:state', and
`:title'.  PID strings are minted per request so answers from card
buttons are unambiguous even with several requests pending.")

(defvar agent-shell-lark--permission-counter 0
  "Monotonic counter used to mint permission ids.")

(defvar agent-shell-lark--awaiting-claim nil
  "Shell buffer awaiting a chat binding via the claim handshake, or nil.")

(defvar-local agent-shell-lark--state nil
  "Buffer-local bridge state for an `agent-shell' buffer.

An alist with keys:
  :buffer        - the bridged shell buffer
  :chat-id       - the Lark chat_id bound to this session (or nil)
  :subscriptions - agent-shell event subscription tokens
  :pending       - the in-flight permission request, or nil")

;;; Logging

(defun agent-shell-lark--log (format-string &rest args)
  "Append a timestamped line to the bridge log.

FORMAT-STRING and ARGS are passed to `format'."
  (let ((line (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create agent-shell-lark-log-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ") line "\n"))))

;;; JSON helpers

(defun agent-shell-lark--parse-json (string)
  "Parse JSON STRING into an alist, or nil on failure."
  (condition-case err
      (json-parse-string string
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error
     (agent-shell-lark--log "JSON parse error: %s" (error-message-string err))
     nil)))

;;; Lifecycle

;;;###autoload
(defun agent-shell-lark-start (&optional chat-id)
  "Start or re-point the Lark bridge for the current `agent-shell' buffer.

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
  (unless (executable-find agent-shell-lark-cli-command)
    (user-error "Cannot find `%s' on PATH" agent-shell-lark-cli-command))
  (let* ((shell-buffer (current-buffer))
         (chat-id (and chat-id (not (string-empty-p (string-trim chat-id)))
                       (string-trim chat-id)))
         (state agent-shell-lark--state)
         (fresh (null state)))
    (when fresh
      (setq state (list (cons :buffer shell-buffer)
                        (cons :chat-id nil)
                        (cons :subscriptions nil)
                        (cons :pending nil)
                        (cons :tool-call-status nil)
                        (cons :progress-text "")))
      (setq agent-shell-lark--state state)
      (agent-shell-lark--subscribe shell-buffer state)
      (agent-shell-lark--register-responder)
      (setq agent-shell-lark--bridges
            (cons state (delq state agent-shell-lark--bridges)))
      (agent-shell-lark--ensure-consumer)
      (when agent-shell-lark-permission-cards
        (agent-shell-lark--ensure-card-consumer)))
    (cond
     (chat-id
      (agent-shell-lark--bind-chat state chat-id)
      (agent-shell-lark--log "%s %s to chat %s"
                               (if fresh "Bridge started for" "Rebound")
                               (buffer-name shell-buffer) chat-id)
      (message "Lark bridge bound to chat %s" chat-id))
     (t
      ;; No explicit chat: (re-)enter claim mode.  Detach any current chat
      ;; so the next inbound message rebinds this session.
      (when-let* ((current (map-elt state :chat-id)))
        (map-put! state :chat-id nil)
        (map-put! state :pending nil)
        (map-put! state :tool-call-status nil)
        (agent-shell-lark--log "Detached chat %s from %s for re-claim"
                                 current (buffer-name shell-buffer)))
      (when (and (buffer-live-p agent-shell-lark--awaiting-claim)
                 (not (eq agent-shell-lark--awaiting-claim shell-buffer)))
        (agent-shell-lark--log
         "Warning: replacing pending claim from %s"
         (buffer-name agent-shell-lark--awaiting-claim)))
      (setq agent-shell-lark--awaiting-claim shell-buffer)
      (agent-shell-lark--log "%s %s, awaiting chat claim"
                               (if fresh "Bridge started for" "Re-claiming for")
                               (buffer-name shell-buffer))
      (message "Lark bridge waiting: message the bot from the desired chat to bind it")))))

;;;###autoload
(defun agent-shell-lark-stop ()
  "Stop the Lark bridge for the current `agent-shell' buffer."
  (interactive)
  (let ((state agent-shell-lark--state))
    (unless state
      (user-error "No Lark bridge in this buffer"))
    (dolist (token (map-elt state :subscriptions))
      (ignore-errors (agent-shell-unsubscribe :subscription token)))
    (setq agent-shell-lark--bridges (delq state agent-shell-lark--bridges))
    (when (eq agent-shell-lark--awaiting-claim (current-buffer))
      (setq agent-shell-lark--awaiting-claim nil))
    (setq agent-shell-lark--state nil)
    (when (null agent-shell-lark--bridges)
      (agent-shell-lark--stop-consumer)
      (when (eq agent-shell-permission-responder-function
                #'agent-shell-lark--responder)
        (setq agent-shell-permission-responder-function nil)))
    (agent-shell-lark--log "Bridge stopped for %s" (buffer-name))
    (message "Lark bridge stopped")))

(defun agent-shell-lark--bind-chat (state chat-id)
  "Bind CHAT-ID to STATE, detaching it from any other session first.

Any other bridge currently holding CHAT-ID has it cleared, then
CHAT-ID is set on STATE.  Both sides' pending approval and tool-call
dedupe tracking are reset so the move starts from a clean slate."
  (when-let* ((previous (agent-shell-lark--bridge-for-chat chat-id)))
    (unless (eq previous state)
      (map-put! previous :chat-id nil)
      (map-put! previous :pending nil)
      (map-put! previous :tool-call-status nil)
      (agent-shell-lark--log "Detached chat %s from %s"
                               chat-id
                               (buffer-name (map-elt previous :buffer)))))
  (map-put! state :chat-id chat-id)
  (map-put! state :pending nil)
  (map-put! state :tool-call-status nil))

;;;###autoload
(defun agent-shell-lark-rebind (chat-id)
  "Bind CHAT-ID to the current buffer's bridge, moving it if needed.

Interactively, chats currently bound to any session are offered for
completion (annotated with their session); a raw chat_id may also be
typed.  If CHAT-ID is already bound to another session, it is detached
from that session and re-bound here.  The current buffer must already
have a bridge (see `agent-shell-lark-start')."
  (interactive
   (progn
     (unless agent-shell-lark--state
       (user-error "No Lark bridge here; run `agent-shell-lark-start' first"))
     (list (agent-shell-lark--read-chat-id "Bind chat to this session: "))))
  (let ((state agent-shell-lark--state)
        (chat-id (string-trim (or chat-id ""))))
    (unless state
      (user-error "No Lark bridge in this buffer"))
    (when (string-empty-p chat-id)
      (user-error "No chat_id given"))
    (when (equal (map-elt state :chat-id) chat-id)
      (user-error "This session is already bound to %s" chat-id))
    (when (eq agent-shell-lark--awaiting-claim (map-elt state :buffer))
      (setq agent-shell-lark--awaiting-claim nil))
    (agent-shell-lark--bind-chat state chat-id)
    (agent-shell-lark--log "Rebound chat %s to %s"
                             chat-id (buffer-name (map-elt state :buffer)))
    (agent-shell-lark--send
     state (format "\U0001F517 This chat now drives session %s"
                   (agent-shell-lark--session-label (map-elt state :buffer))))
    (message "Lark chat %s bound to %s"
             chat-id (buffer-name (map-elt state :buffer)))))

(defun agent-shell-lark--read-chat-id (prompt)
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
                         agent-shell-lark--bridges)))
         (choice (completing-read prompt (mapcar #'car candidates) nil nil)))
    (or (cdr (assoc choice candidates)) (string-trim choice))))

;;; Shared inbound consumer

(defun agent-shell-lark--ensure-consumer ()
  "Start the shared `lark-cli event consume' process if not running.

Events arrive as NDJSON on stdout; `lark-cli' writes connection
banners and diagnostics to stderr, which is captured separately so it
never reaches the JSON filter."
  (unless (process-live-p agent-shell-lark--consumer)
    (setq agent-shell-lark--consumer-pending-line "")
    (setq agent-shell-lark--consumer-stderr
          (make-pipe-process
           :name "agent-shell-lark-consume-stderr"
           :buffer (get-buffer-create " *agent-shell-lark-consume-stderr*")
           :filter #'agent-shell-lark--stderr-filter
           :noquery t))
    (setq agent-shell-lark--consumer
          (make-process
           :name "agent-shell-lark-consume"
           :buffer (get-buffer-create " *agent-shell-lark-consume*")
           :connection-type 'pipe
           :command (list agent-shell-lark-cli-command
                          "event" "consume" "im.message.receive_v1"
                          "--as" "bot")
           :filter #'agent-shell-lark--filter
           :stderr agent-shell-lark--consumer-stderr
           :sentinel #'agent-shell-lark--consumer-sentinel))
    (agent-shell-lark--log "Shared consumer started")))

(defun agent-shell-lark--ensure-card-consumer ()
  "Start the shared `card.action.trigger' consumer if not running.

Only used when `agent-shell-lark-permission-cards' is non-nil.  The
app must have the `card.action.trigger' callback subscribed in the
developer console, or the process exits immediately (see the log)."
  (unless (process-live-p agent-shell-lark--card-consumer)
    (setq agent-shell-lark--card-consumer-pending-line "")
    (setq agent-shell-lark--card-consumer-stderr
          (make-pipe-process
           :name "agent-shell-lark-card-stderr"
           :buffer (get-buffer-create " *agent-shell-lark-card-stderr*")
           :filter #'agent-shell-lark--stderr-filter
           :noquery t))
    (setq agent-shell-lark--card-consumer
          (make-process
           :name "agent-shell-lark-card-consume"
           :buffer (get-buffer-create " *agent-shell-lark-card-consume*")
           :connection-type 'pipe
           :command (list agent-shell-lark-cli-command
                          "event" "consume" "card.action.trigger"
                          "--as" "bot")
           :filter #'agent-shell-lark--card-filter
           :stderr agent-shell-lark--card-consumer-stderr
           :sentinel #'agent-shell-lark--consumer-sentinel))
    (agent-shell-lark--log "Card consumer started")))

(defun agent-shell-lark--card-filter (_process output)
  "Accumulate OUTPUT into NDJSON lines for card actions."
  (let* ((pending (concat agent-shell-lark--card-consumer-pending-line output))
         (lines (split-string pending "\n")))
    (setq agent-shell-lark--card-consumer-pending-line (car (last lines)))
    (dolist (line (butlast lines))
      (setq line (string-trim line))
      (when (string-prefix-p "{" line)
        (agent-shell-lark--handle-card-line line)))))

(defun agent-shell-lark--stop-consumer ()
  "Stop the shared consumer processes, if any."
  (dolist (process (list agent-shell-lark--consumer
                         agent-shell-lark--consumer-stderr
                         agent-shell-lark--card-consumer
                         agent-shell-lark--card-consumer-stderr))
    (when (process-live-p process)
      (delete-process process)))
  (setq agent-shell-lark--consumer nil)
  (setq agent-shell-lark--consumer-stderr nil)
  (setq agent-shell-lark--consumer-pending-line "")
  (setq agent-shell-lark--card-consumer nil)
  (setq agent-shell-lark--card-consumer-stderr nil)
  (setq agent-shell-lark--card-consumer-pending-line "")
  (setq agent-shell-lark--pending-permissions nil)
  (agent-shell-lark--log "Shared consumers stopped"))

(defun agent-shell-lark--consumer-sentinel (process change)
  "Log when the consume PROCESS exits.  CHANGE is the status string."
  (when (memq (process-status process) '(exit signal))
    (agent-shell-lark--log "Consumer exited: %s" (string-trim change))))

(defun agent-shell-lark--stderr-filter (_process output)
  "Log consumer diagnostics OUTPUT with a [consume] prefix."
  (dolist (line (split-string (string-trim output) "\n" t))
    (agent-shell-lark--log "[consume] %s" (string-trim line))))

(defun agent-shell-lark--filter (_process output)
  "Process filter accumulating OUTPUT into NDJSON lines.

Only lines that look like a JSON object are handled; anything else is
ignored so stray diagnostics never trip the parser."
  (let* ((pending (concat agent-shell-lark--consumer-pending-line output))
         (lines (split-string pending "\n")))
    ;; The last element is an incomplete line (or "" when OUTPUT ended
    ;; with a newline); keep it for the next chunk.
    (setq agent-shell-lark--consumer-pending-line (car (last lines)))
    (dolist (line (butlast lines))
      (setq line (string-trim line))
      (when (string-prefix-p "{" line)
        (agent-shell-lark--handle-line line)))))

(defun agent-shell-lark--handle-line (line)
  "Handle one NDJSON LINE, routing it to the bound session's buffer.

`lark-cli event consume' emits a flat, agent-friendly format: `type',
`chat_id', `message_type', `sender_id' (the open_id), and `content'
are top-level keys."
  (when-let* ((event (agent-shell-lark--parse-json line))
              ((equal (map-elt event 'type) "im.message.receive_v1"))
              (sender (map-elt event 'sender_id)))
    (let ((chat-id (map-elt event 'chat_id)))
      (cond
       ((not (member sender agent-shell-lark-allowed-open-ids))
        (agent-shell-lark--log
         "Ignored message from unlisted sender: %s (add to `agent-shell-lark-allowed-open-ids')"
         sender))
       ((null chat-id)
        (agent-shell-lark--log "Incoming message without chat_id; ignoring"))
       (t
        (let ((state (agent-shell-lark--bridge-for-chat chat-id)))
          (cond
           (state
            (agent-shell-lark--dispatch-message state event))
           ((buffer-live-p agent-shell-lark--awaiting-claim)
            (setq state (buffer-local-value 'agent-shell-lark--state
                                            agent-shell-lark--awaiting-claim))
            (map-put! state :chat-id chat-id)
            (setq agent-shell-lark--awaiting-claim nil)
            (agent-shell-lark--log "Bound chat %s to %s"
                                     chat-id
                                     (buffer-name (map-elt state :buffer)))
            (agent-shell-lark--send
             state (format "\U0001F517 Bound to session %s"
                           (agent-shell-lark--session-label
                            (map-elt state :buffer))))
            (agent-shell-lark--dispatch-message state event))
           (t
            (agent-shell-lark--log
             "Message in chat %s has no bound session (none awaiting claim); ignoring"
             chat-id)))))))))

(defun agent-shell-lark--bridge-for-chat (chat-id)
  "Return the bridge state bound to CHAT-ID, or nil."
  (seq-find (lambda (state)
              (and (buffer-live-p (map-elt state :buffer))
                   (equal (map-elt state :chat-id) chat-id)))
            agent-shell-lark--bridges))

(defun agent-shell-lark--dispatch-message (state event)
  "Route EVENT for the session in STATE.

Non-text messages are declined; text is treated as the answer to a
pending approval, or otherwise injected as a prompt."
  (let ((message-type (map-elt event 'message_type))
        (text (agent-shell-lark--message-text event)))
    (cond
     ((not (member message-type '("text" "post")))
      (agent-shell-lark--send
       state (format "\u26A0 Only text/post messages are supported (got %s)."
                     message-type)))
     ((null text)
      (agent-shell-lark--log "Empty text content on incoming message"))
     ((agent-shell-lark--interrupt-command-p text)
      (agent-shell-lark--interrupt state))
     ((map-elt state :pending)
      (agent-shell-lark--answer-pending state text))
     (t
      (agent-shell-lark--inject-prompt state text)))))

(defun agent-shell-lark--interrupt-command-p (text)
  "Return non-nil if TEXT is configured as an interrupt command."
  (member (downcase (string-trim text))
          agent-shell-lark-interrupt-commands))

(defun agent-shell-lark--interrupt (state)
  "Interrupt STATE's bound `agent-shell' buffer from Lark."
  (let ((shell-buffer (map-elt state :buffer)))
    (condition-case err
        (if (not (buffer-live-p shell-buffer))
            (agent-shell-lark--send state "⚠ Session buffer is gone; cannot interrupt.")
          (with-current-buffer shell-buffer
            (agent-shell-interrupt t))
          (agent-shell-lark--send state "⏹ Sent C-c C-c interrupt."))
      (error
       (agent-shell-lark--send
        state (format "⚠ Could not interrupt: %s" (error-message-string err)))))))

(defun agent-shell-lark--message-text (event)
  "Return the trimmed user text of EVENT, or nil.

`lark-cli''s agent-friendly format carries the extracted message text
directly in `content'; when `content' still holds raw JSON (older
formats), fall back to parsing it.  @mention placeholders are dropped."
  (when-let* ((raw (map-elt event 'content))
              ((stringp raw)))
    (let* ((parsed (and (string-prefix-p "{" raw)
                        (agent-shell-lark--parse-json raw)))
           (text (cond ((and parsed (map-elt parsed 'text))
                        (map-elt parsed 'text))
                       (parsed (agent-shell-lark--post-plain-text parsed))
                       (t raw)))
           (cleaned (string-trim
                     (replace-regexp-in-string "@_user_[0-9]+" "" (or text "")))))
      (unless (string-empty-p cleaned)
        cleaned))))

(defun agent-shell-lark--post-plain-text (parsed)
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

(defun agent-shell-lark--inject-prompt (state text)
  "Queue or submit TEXT as a prompt into STATE's shell buffer.

Uses `agent-shell-prompt-queue', which submits immediately when the
shell is idle and otherwise queues TEXT to run when the current turn
completes."
  (let ((shell-buffer (map-elt state :buffer)))
    (condition-case err
        (with-current-buffer shell-buffer
          (let ((was-busy (shell-maker-busy)))
            (agent-shell-prompt-queue text)
            (agent-shell-lark--log "%s prompt into %s: %s"
                                     (if was-busy "Queued" "Injected")
                                     (buffer-name shell-buffer)
                                     (agent-shell-lark--truncate text))))
      (error
       (agent-shell-lark--send
        state (format "\u26A0 Could not submit prompt: %s"
                      (error-message-string err)))))))

;;; Outbound (agent-shell -> Lark)

(defun agent-shell-lark--subscribe (shell-buffer state)
  "Subscribe to SHELL-BUFFER events, storing tokens on STATE."
  (let ((tokens
         (list
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'turn-complete
           :on-event
           (lambda (event)
             (agent-shell-lark--on-turn-complete state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'agent-message-chunk
           :on-event
           (lambda (event)
             (agent-shell-lark--on-agent-message-chunk state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'tool-call-update
           :on-event
           (lambda (event)
             (agent-shell-lark--on-tool-call-update state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'permission-response
           :on-event
           (lambda (_event)
             (map-put! state :pending nil)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'error
           :on-event
           (lambda (event)
             (agent-shell-lark--on-error state event)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'clean-up
           :on-event
           (lambda (_event)
             (with-current-buffer shell-buffer
               (when agent-shell-lark--state
                 (agent-shell-lark-stop))))))))
    (map-put! state :subscriptions tokens)))

(defun agent-shell-lark--on-turn-complete (state event)
  "Relay thoughts and the final agent message for STATE's session.

EVENT is the `turn-complete' event alist."
  (when agent-shell-lark-relay-thoughts
    (when-let* ((summary (agent-shell-lark--thought-last-paragraph
                          (agent-shell-lark--last-thoughts
                           (map-elt state :buffer)))))
      (agent-shell-lark--send
       state (concat agent-shell-thought-process-icon " " summary))))
  (map-put! state :progress-text "")
  (let ((mention (and agent-shell-lark-mention-on-turn-complete
                      (agent-shell-lark--mention-string))))
    (if agent-shell-lark-relay-turn-complete
        (let ((text (or (agent-shell-lark--last-agent-message (map-elt state :buffer))
                        (format "(turn complete: %s)"
                                (map-nested-elt event '(:data :stop-reason))))))
          (agent-shell-lark--send
           state (concat "\U0001F916 " text
                         (if mention (concat "\n\n" mention " ✅") ""))))
      (when mention
        (agent-shell-lark--send-mention state)))))

(defun agent-shell-lark--last-thoughts (shell-buffer)
  "Return SHELL-BUFFER's last thoughts section from its transcript, or nil.

No agent-shell event streams thinking text, so thought relay reads the
transcript's final \"## Agent's Thoughts\" section at end of turn."
  (when-let* ((file (buffer-local-value 'agent-shell--transcript-file shell-buffer))
              ((stringp file))
              ((file-exists-p file)))
    (let ((coding-system-for-read 'utf-8))
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
               (point-max)))))))))

(defun agent-shell-lark--on-agent-message-chunk (state event)
  "Accumulate EVENT's text chunk on STATE for progress relay.

The accumulated segment is relayed by
`agent-shell-lark--on-tool-call-update' when the next tool call
closes it."
  (when agent-shell-lark-relay-progress-messages
    (when-let* ((chunk (map-nested-elt event '(:data :text-chunk))))
      (map-put! state :progress-text
                (concat (map-elt state :progress-text) chunk)))))

(defun agent-shell-lark--thought-last-paragraph (text)
  "Return the last non-empty paragraph from thinking TEXT."
  (when-let* ((clean (and text (string-trim text)))
              ((not (string-empty-p clean))))
    (let* ((paragraphs (split-string clean "\n[ \t]*\n" t))
           ;; Cursor's collapsed Thinking preview is usually the final
           ;; summary-like paragraph, so mirror that in Lark.
           (last-block (car (last paragraphs)))
           (one-line (string-join (split-string (string-trim last-block)) " ")))
      (agent-shell-lark--truncate one-line))))

(defun agent-shell-lark--on-tool-call-update (state event)
  "Flush progress text and relay a tool-call status change for STATE.

EVENT is a `tool-call-update' event alist.  To avoid duplicate
messages (the event fires on both creation and each update), only
status transitions are relayed, tracked per tool-call id on STATE."
  ;; A tool call closes any assistant text preceding it; relay that
  ;; segment as a progress message.
  (when agent-shell-lark-relay-progress-messages
    (when-let* ((text (string-trim (or (map-elt state :progress-text) "")))
                ((not (string-empty-p text))))
      (agent-shell-lark--send
       state (concat "💬 " (agent-shell-lark--truncate text)))))
  (map-put! state :progress-text "")
  (when agent-shell-lark-relay-tool-calls
    (agent-shell-lark--relay-tool-status state event)))

(defun agent-shell-lark--relay-tool-status (state event)
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
      (agent-shell-lark--send
       state (format "%s %s%s"
                     (agent-shell-lark--tool-status-icon status)
                     title
                     (if status (format " [%s]" status) ""))))))

(defun agent-shell-lark--tool-status-icon (status)
  "Return an icon string for tool-call STATUS."
  (pcase status
    ("completed" "\u2705")
    ("failed" "\u274C")
    ("in_progress" "\u2699\uFE0F")
    (_ "\U0001F527")))

(defun agent-shell-lark--on-error (state event)
  "Relay an ACP EVENT error to STATE's chat."
  (let ((message (map-nested-elt event '(:data :message))))
    (agent-shell-lark--send
     state (format "\u26A0 Error: %s" (or message "unknown")))))

(defun agent-shell-lark--last-agent-message (shell-buffer)
  "Return SHELL-BUFFER's last agent message from its transcript, or nil."
  (when-let* ((file (buffer-local-value 'agent-shell--transcript-file shell-buffer))
              ((stringp file))
              ((file-exists-p file)))
    (let ((coding-system-for-read 'utf-8))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (when (re-search-backward "^## Agent (" nil t)
          (forward-line 1)
          (let ((text (string-trim (buffer-substring-no-properties
                                    (point) (point-max)))))
            (unless (string-empty-p text)
              (agent-shell-lark--truncate text))))))))

(defun agent-shell-lark--truncate (text)
  "Truncate TEXT to `agent-shell-lark-max-message-length'."
  (if (> (length text) agent-shell-lark-max-message-length)
      (concat (substring text 0 agent-shell-lark-max-message-length)
              "\n… (truncated)")
    text))

(defun agent-shell-lark--session-label (shell-buffer)
  "Return a short human label identifying SHELL-BUFFER's session."
  (buffer-name shell-buffer))

;;; Formula rendering ($$...$$ -> embedded image)

(defconst agent-shell-lark--formula-regexp "\\$\\$\\([^$]+\\)\\$\\$"
  "Matches a display formula; group 1 is the TeX source.")

(defvar agent-shell-lark--formula-keys nil
  "Alist of (FORMULA-SHA1 . IMAGE-KEY) for uploaded formula images.")

(defun agent-shell-lark--formula-tools-ready-p ()
  "Non-nil when the formula rendering tool chain is available."
  (and (executable-find "node")
       (executable-find "rsvg-convert")
       (file-exists-p (expand-file-name
                       "tex2svg.js" agent-shell-lark-formula-tools-directory))
       (file-directory-p (expand-file-name
                          "node_modules/mathjax"
                          agent-shell-lark-formula-tools-directory))))

(defun agent-shell-lark--formula-cache-dir ()
  "Return the cache directory for rendered formulas, creating it."
  (let ((dir (expand-file-name "agent-shell-lark"
                               (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
    (make-directory dir t)
    dir))

(defun agent-shell-lark--formulas-in (text)
  "Return the unique TeX sources of display formulas in TEXT."
  (let ((start 0) (formulas ()))
    (while (string-match agent-shell-lark--formula-regexp text start)
      (push (match-string 1 text) formulas)
      (setq start (match-end 0)))
    (seq-uniq (nreverse formulas))))

(defun agent-shell-lark--replace-formulas (text)
  "Replace TEXT's display formulas with image references where known."
  (replace-regexp-in-string
   agent-shell-lark--formula-regexp
   (lambda (match)
     (let* ((formula (progn (string-match agent-shell-lark--formula-regexp match)
                            (match-string 1 match)))
            (key (cdr (assoc (sha1 formula) agent-shell-lark--formula-keys))))
       (if key (format "![formula](%s)" key) match)))
   text t t))

(defun agent-shell-lark--formula-ensure-key (formula callback)
  "Render and upload FORMULA, then call CALLBACK with non-nil on success.

The image_key is cached in `agent-shell-lark--formula-keys'; render
(node/MathJax), convert (rsvg-convert), and upload (lark-cli) each run
asynchronously with a timeout, so a failure degrades to sending the
original text rather than blocking."
  (let* ((hash (sha1 formula))
         (dir (agent-shell-lark--formula-cache-dir))
         (svg (expand-file-name (concat hash ".svg") dir))
         (png-name (concat hash ".png"))
         (png (expand-file-name png-name dir))
         (fail (lambda (step process)
                 (agent-shell-lark--log
                  "Formula %s failed at %s: %s" hash step
                  (with-current-buffer (process-buffer process)
                    (string-trim (buffer-string))))
                 (funcall callback nil))))
    (make-process
     :name "agent-shell-lark-formula-render"
     :buffer (generate-new-buffer " *lark-formula*")
     :connection-type 'pipe
     :command (list "timeout" "30" "node"
                    (expand-file-name "tex2svg.js"
                                      agent-shell-lark-formula-tools-directory)
                    formula)
     :sentinel
     (lambda (process _change)
       (when (memq (process-status process) '(exit signal))
         (if (not (zerop (process-exit-status process)))
             (funcall fail "render" process)
           (with-current-buffer (process-buffer process)
             (write-region (point-min) (point-max) svg nil 'silent))
           (make-process
            :name "agent-shell-lark-formula-convert"
            :buffer (generate-new-buffer " *lark-formula*")
            :connection-type 'pipe
            :command (list "timeout" "30" "rsvg-convert" "--zoom" "3"
                           "--background-color" "white" "-o" png svg)
            :sentinel
            (lambda (process _change)
              (when (memq (process-status process) '(exit signal))
                (if (not (zerop (process-exit-status process)))
                    (funcall fail "convert" process)
                  ;; lark-cli only accepts relative file paths.
                  (let ((default-directory dir))
                    (make-process
                     :name "agent-shell-lark-formula-upload"
                     :buffer (generate-new-buffer " *lark-formula*")
                     :connection-type 'pipe
                     :command (list "timeout"
                                    (number-to-string agent-shell-lark-command-timeout)
                                    agent-shell-lark-cli-command
                                    "im" "images" "create"
                                    "--data" "{\"image_type\":\"message\"}"
                                    "--file" (concat "./" png-name))
                     :sentinel
                     (lambda (process _change)
                       (when (memq (process-status process) '(exit signal))
                         (let ((output (with-current-buffer (process-buffer process)
                                         (buffer-string))))
                           (if (and (zerop (process-exit-status process))
                                    (string-match "\"image_key\"[: ]*\"\\([^\"]+\\)\"" output))
                               (progn
                                 (push (cons hash (match-string 1 output))
                                       agent-shell-lark--formula-keys)
                                 (funcall callback t))
                             (funcall fail "upload" process)))))))))))))))))

(defun agent-shell-lark--send-with-formulas (state text)
  "Send TEXT to STATE's chat with its display formulas embedded as images."
  (let* ((formulas (agent-shell-lark--formulas-in text))
         (missing (seq-remove (lambda (formula)
                                (assoc (sha1 formula) agent-shell-lark--formula-keys))
                              formulas))
         (pending (length missing))
         (failed nil)
         (finish (lambda ()
                   (agent-shell-lark--send-plain
                    state
                    (if failed text (agent-shell-lark--replace-formulas text))))))
    (if (zerop pending)
        (funcall finish)
      (dolist (formula missing)
        (agent-shell-lark--formula-ensure-key
         formula
         (lambda (ok)
           (unless ok (setq failed t))
           (setq pending (1- pending))
           (when (zerop pending)
             (funcall finish))))))))

(defun agent-shell-lark--mention-string ()
  "Return an at-mention string for all allowed users, or nil."
  (when agent-shell-lark-allowed-open-ids
    (mapconcat (lambda (id) (format "<at user_id=\"%s\"></at>" id))
               agent-shell-lark-allowed-open-ids " ")))

(defun agent-shell-lark--send-mention (state)
  "At-mention all allowed users in STATE's chat with a short text ping."
  (when-let* ((chat-id (map-elt state :chat-id))
              (mention (agent-shell-lark--mention-string)))
    (make-process
     :name "agent-shell-lark-send"
     :buffer (get-buffer-create " *agent-shell-lark-send*")
     :connection-type 'pipe
     :command (list "timeout"
                    (number-to-string agent-shell-lark-command-timeout)
                    agent-shell-lark-cli-command
                    "im" "+messages-send"
                    "--as" "bot"
                    "--chat-id" chat-id
                    "--text"
                    (concat mention " ✅ turn complete")))))

(defun agent-shell-lark--send (state text)
  "Send TEXT to the Lark chat bound to STATE.

When `agent-shell-lark-render-formulas' is enabled and TEXT contains
display formulas, they are rendered and embedded as images first."
  (if (and agent-shell-lark-render-formulas
           (string-match-p agent-shell-lark--formula-regexp text)
           (agent-shell-lark--formula-tools-ready-p))
      (agent-shell-lark--send-with-formulas state text)
    (agent-shell-lark--send-plain state text)))

(defun agent-shell-lark--send-plain (state text)
  "Send TEXT to the Lark chat bound to STATE as-is.

Runs `lark-cli im +messages-send' asynchronously as the bot identity.
Does nothing when the session has no bound chat yet."
  (when-let* ((chat-id (map-elt state :chat-id)))
    (let ((labeled (if agent-shell-lark-label-outbound
                       (format "\u300C%s\u300D\n%s"
                               (agent-shell-lark--session-label
                                (map-elt state :buffer))
                               text)
                     text)))
      (make-process
       :name "agent-shell-lark-send"
       :buffer (get-buffer-create " *agent-shell-lark-send*")
       :connection-type 'pipe
       :command (list "timeout"
                      (number-to-string agent-shell-lark-command-timeout)
                      agent-shell-lark-cli-command
                      "im" "+messages-send"
                      "--as" "bot"
                      "--chat-id" chat-id
                      (if (eq agent-shell-lark-message-type 'post)
                          "--markdown"
                        "--text")
                      labeled)
       :sentinel
       (lambda (process _change)
         (when (memq (process-status process) '(exit signal))
           (unless (zerop (process-exit-status process))
             (agent-shell-lark--log
              "Send failed (exit %d): %s"
              (process-exit-status process)
              (with-current-buffer (process-buffer process)
                (string-trim (buffer-string)))))))))))

;;; Reply-based permission approvals

(defun agent-shell-lark--register-responder ()
  "Install the bridge's permission responder if none conflicts.

Only installs `agent-shell-lark--responder'; leaves any other
user-set responder untouched."
  (when (and agent-shell-permission-responder-function
             (not (eq agent-shell-permission-responder-function
                      #'agent-shell-lark--responder)))
    (agent-shell-lark--log
     "Warning: `agent-shell-permission-responder-function' already set to %s; not overriding"
     agent-shell-permission-responder-function))
  (unless agent-shell-permission-responder-function
    (setq agent-shell-permission-responder-function
          #'agent-shell-lark--responder)))

(defun agent-shell-lark--bridge-for-tool-call (tool-call)
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
   agent-shell-lark--bridges))

(defun agent-shell-lark--responder (permission)
  "Relay PERMISSION to the owning session's Lark chat.

Returns nil so the interactive permission dialog is also shown locally."
  (when-let* ((tool-call (map-elt permission :tool-call))
              (state (agent-shell-lark--bridge-for-tool-call tool-call))
              ((map-elt state :chat-id))
              (options (map-elt permission :options)))
    (let ((title (map-elt tool-call :title)))
      (if agent-shell-lark-permission-cards
          (agent-shell-lark--send-permission-card
           state title options (map-elt permission :respond))
        (map-put! state :pending
                  (list (cons :respond (map-elt permission :respond))
                        (cons :options options)
                        (cons :title title)))
        (agent-shell-lark--send
         state (agent-shell-lark--approval-text title options)))
      (agent-shell-lark--log "Permission relayed to %s: %s"
                               (buffer-name (map-elt state :buffer)) title)
      nil)))

(defun agent-shell-lark--option-kind-allow-p (option)
  "Return non-nil when OPTION's :kind is an allow variant."
  (string-prefix-p "allow" (or (map-elt option :kind) "")))

(defun agent-shell-lark--permission-card (pid title options state)
  "Build the card alist for permission PID with TITLE and OPTIONS in STATE."
  `((schema . "2.0")
    (header . ((template . "orange")
               (title . ((tag . "plain_text")
                         (content . "\U0001F510 Permission needed")))))
    (body
     . ((elements
         . ,(vconcat
             (list `((tag . "markdown")
                     (content . ,(format "**%s**\nSession: %s"
                                         (or title "tool call")
                                         (agent-shell-lark--session-label
                                          (map-elt state :buffer))))))
             (mapcar
              (lambda (option)
                `((tag . "button")
                  (text . ((tag . "plain_text")
                           (content . ,(or (map-elt option :label) "Option"))))
                  (type . ,(cond
                            ((agent-shell-lark--option-kind-allow-p option)
                             "primary")
                            ((string-prefix-p "reject"
                                              (or (map-elt option :kind) ""))
                             "danger")
                            (t "default")))
                  (behaviors
                   . ,(vector
                       `((type . "callback")
                         (value . ((pid . ,pid)
                                   (option_id . ,(map-elt option :option-id)))))))))
              options)))))))

(defun agent-shell-lark--send-permission-card (state title options respond)
  "Send a permission card for TITLE/OPTIONS in STATE; wire RESPOND to it."
  (let ((pid (format "p%d" (setq agent-shell-lark--permission-counter
                                 (1+ agent-shell-lark--permission-counter)))))
    (push (cons pid (list (cons :respond respond)
                          (cons :options options)
                          (cons :state state)
                          (cons :title title)))
          agent-shell-lark--pending-permissions)
    (make-process
     :name "agent-shell-lark-card-send"
     :buffer (get-buffer-create " *agent-shell-lark-send*")
     :connection-type 'pipe
     :command (list "timeout"
                    (number-to-string agent-shell-lark-command-timeout)
                    agent-shell-lark-cli-command
                    "im" "+messages-send"
                    "--as" "bot"
                    "--chat-id" (map-elt state :chat-id)
                    "--msg-type" "interactive"
                    "--content" (json-encode
                                 (agent-shell-lark--permission-card
                                  pid title options state)))
     :sentinel
     (lambda (process _change)
       (when (memq (process-status process) '(exit signal))
         (unless (zerop (process-exit-status process))
           (agent-shell-lark--log
            "Card send failed (exit %d): %s"
            (process-exit-status process)
            (with-current-buffer (process-buffer process)
              (string-trim (buffer-string))))))))))

(defun agent-shell-lark--answered-card (title label allow-p)
  "Build the retired card for TITLE answered with LABEL; ALLOW-P colors it."
  `((schema . "2.0")
    (header . ((template . ,(if allow-p "green" "red"))
               (title . ((tag . "plain_text")
                         (content . ,(if allow-p
                                         "Permission granted"
                                       "Permission rejected"))))))
    (body . ((elements
              . ,(vector
                  `((tag . "markdown")
                    (content . ,(format "**%s**\n%s %s"
                                        (or title "tool call")
                                        (if allow-p "\u2705" "\u274C")
                                        (or label ""))))))))))

(defun agent-shell-lark--update-card (token card)
  "Update a sent card via TOKEN with the new CARD alist."
  (make-process
   :name "agent-shell-lark-card-update"
   :buffer (get-buffer-create " *agent-shell-lark-send*")
   :connection-type 'pipe
   :command (list "timeout"
                  (number-to-string agent-shell-lark-command-timeout)
                  agent-shell-lark-cli-command
                  "api" "POST" "/open-apis/interactive/v1/card/update"
                  "--as" "bot"
                  "--data" (json-encode `((token . ,token) (card . ,card))))
   :sentinel
   (lambda (process _change)
     (when (memq (process-status process) '(exit signal))
       (unless (zerop (process-exit-status process))
         (agent-shell-lark--log
          "Card update failed (exit %d): %s"
          (process-exit-status process)
          (with-current-buffer (process-buffer process)
            (string-trim (buffer-string)))))))))

(defun agent-shell-lark--handle-card-line (line)
  "Handle one `card.action.trigger' NDJSON LINE."
  (when-let* ((event (agent-shell-lark--parse-json line))
              ((equal (map-elt event 'type) "card.action.trigger"))
              (operator (map-elt event 'operator_id)))
    (if (not (member operator agent-shell-lark-allowed-open-ids))
        (agent-shell-lark--log "Ignored card action from unlisted operator: %s"
                                 operator)
      (let* ((value (and (stringp (map-elt event 'action_value))
                         (agent-shell-lark--parse-json
                          (map-elt event 'action_value))))
             (pid (map-elt value 'pid))
             (option-id (map-elt value 'option_id))
             (entry (cdr (assoc pid agent-shell-lark--pending-permissions)))
             (token (map-elt event 'token)))
        (cond
         ((null entry)
          (agent-shell-lark--log "Card action for unknown/expired pid %s" pid)
          (when token
            (agent-shell-lark--update-card
             token (agent-shell-lark--answered-card
                    "This request has expired" "" nil))))
         (t
          (setq agent-shell-lark--pending-permissions
                (assoc-delete-all pid agent-shell-lark--pending-permissions))
          (let* ((option (seq-find (lambda (option)
                                     (equal (map-elt option :option-id)
                                            option-id))
                                   (map-elt entry :options)))
                 (label (or (map-elt option :label) option-id))
                 (allow-p (and option
                               (agent-shell-lark--option-kind-allow-p option))))
            (condition-case err
                (funcall (map-elt entry :respond) option-id)
              (error
               (agent-shell-lark--log "Card respond failed for %s: %s"
                                        pid (error-message-string err))))
            (when token
              (agent-shell-lark--update-card
               token (agent-shell-lark--answered-card
                      (map-elt entry :title) label allow-p)))
            (agent-shell-lark--log
             "Permission %s answered via card in %s: %s"
             pid
             (buffer-name (map-elt (map-elt entry :state) :buffer))
             label))))))))

(defun agent-shell-lark--approval-text (title options)
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

(defun agent-shell-lark--answer-pending (state text)
  "Interpret TEXT as the answer to STATE's pending approval."
  (let* ((pending (map-elt state :pending))
         (options (map-elt pending :options))
         (respond (map-elt pending :respond))
         (choice (agent-shell-lark--match-option text options)))
    (if (not choice)
        (agent-shell-lark--send
         state (concat "\u26A0 Unrecognized answer. "
                       (agent-shell-lark--approval-text
                        (map-elt pending :title) options)))
      (map-put! state :pending nil)
      (funcall respond (map-elt choice :option-id))
      (agent-shell-lark--send
       state (format "\u2705 %s" (map-elt choice :label)))
      (agent-shell-lark--log "Permission answered in %s: %s"
                               (buffer-name (map-elt state :buffer))
                               (map-elt choice :label)))))

(defun agent-shell-lark--match-option (reply options)
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
       (agent-shell-lark--find-option-by-kind options "allow"))
     ;; Reject keywords.
     (when (member normalized '("n" "no" "reject" "deny" "cancel"))
       (agent-shell-lark--find-option-by-kind options "reject")))))

(defun agent-shell-lark--find-option-by-kind (options kind-prefix)
  "Return the first option in OPTIONS whose :kind has prefix KIND-PREFIX."
  (seq-find (lambda (option)
              (when-let* ((kind (map-elt option :kind)))
                (string-prefix-p kind-prefix kind)))
            options))

(provide 'agent-shell-lark)

;;; agent-shell-lark.el ends here
