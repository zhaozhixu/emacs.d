(require 'setup-common)
(require 'map)
(require 'seq)
(require 'subr-x)

;; Keep chat-style labels enabled.  Emacs can fail to allocate display space
;; for a live prompt rendered solely through an overlay `before-string' at
;; end-of-buffer.  Render that one label through `display' instead; unlike the
;; original arrangement, this gives the replacement text real screen height.
(setq agent-shell-chat-mode-enabled t)

(defun my/agent-shell-chat-fix-live-prompt (&rest _)
  "Keep the chat-mode live prompt visible at end-of-buffer.

`agent-shell-chat-mode' normally hides the real comint prompt with an empty
`display' string and places the visible \"Me / ❯\" label in `before-string'.
On some terminal frames that before-string collapses to one pixel at the end
of a large buffer.  Move the live prompt replacement into `display', which
Emacs lays out reliably.  Submitted prompts are left unchanged."
  (when (and (bound-and-true-p agent-shell-chat-mode)
             (not (bound-and-true-p agent-shell-prompt-bar-mode))
             comint-last-prompt)
    (let* ((beg (marker-position (car comint-last-prompt)))
           (end (marker-position (cdr comint-last-prompt)))
           (overlay
            (seq-find
             (lambda (item)
               (eq (overlay-get item 'category) 'agent-shell-chat-me))
             (overlays-in (max (point-min) (1- beg))
                          (min (point-max) (1+ end)))))
           (replacement (and overlay (overlay-get overlay 'before-string))))
      (when (and replacement (string-match-p "❯" replacement))
        (overlay-put overlay 'display replacement)
        (overlay-put overlay 'before-string nil)))))

(defun my/agent-shell-transcript-file-path ()
  "Return an `agent-shell' transcript path outside the project tree."
  (let* ((cwd (directory-file-name (file-truename (agent-shell-cwd))))
         (project-name (file-name-nondirectory cwd))
         (safe-name (replace-regexp-in-string
                     "[^[:alnum:]._-]+" "-"
                     (if (string-empty-p project-name)
                         "root"
                       project-name)))
         (project-key (format "%s-%s" safe-name
                              (substring (secure-hash 'sha1 cwd) 0 12)))
         (cache-root (or (getenv "XDG_CACHE_HOME")
                         (expand-file-name "~/.cache")))
         (dir (expand-file-name
               (concat "agent-shell/transcripts/" project-key "/")
               cache-root)))
    (make-directory dir t)
    (expand-file-name (format-time-string "%F-%H-%M-%S.md") dir)))

(defvar my/agent-shell-tool-output-display-max-chars 120000
  "Maximum characters of a tool output body rendered in agent-shell buffers.

This is display-only: transcripts still receive the complete tool output.")

(defvar my/agent-shell-tool-output-display-head-chars 80000
  "Characters kept from the start when truncating tool output for display.")

(defvar my/agent-shell-tool-output-display-tail-chars 40000
  "Characters kept from the end when truncating tool output for display.")

(defun my/agent-shell-tool-output-display-truncate (text transcript-file)
  "Return a display-only truncated copy of TEXT.

TRANSCRIPT-FILE is mentioned so users know where the full output lives."
  (let* ((length (length text))
         (head (min my/agent-shell-tool-output-display-head-chars length))
         (tail (min my/agent-shell-tool-output-display-tail-chars
                    (max 0 (- length head))))
         (omitted (- length head tail)))
    (concat
     (format "[agent-shell display truncated tool output]\nOriginal length: %d chars. Showing first %d and last %d chars.\nFull output is preserved in transcript: %s\n\n"
             length head tail (or transcript-file "<transcript disabled>"))
     (substring text 0 head)
     (format "\n\n... omitted %d chars from Emacs display only ...\n\n" omitted)
     (if (> tail 0)
         (substring text (- length tail))
       ""))))

(defun my/agent-shell-tool-output-display-cap (orig &rest args)
  "Cap large tool output bodies before ORIG renders them in Emacs.

This advice only changes the `:body' passed to `agent-shell--update-fragment'
for real tool-call fragments.  It deliberately leaves transcript writes and
ACP payloads untouched."
  (let* ((state (plist-get args :state))
         (block-id (plist-get args :block-id))
         (body (plist-get args :body))
         (tool-call (and state block-id
                         (map-nested-elt state `(:tool-calls ,block-id))))
         (limit my/agent-shell-tool-output-display-max-chars))
    (if (and tool-call
             (stringp body)
             (integerp limit)
             (> limit 0)
             (> (length body) limit)
             (not (string-prefix-p "[agent-shell display truncated tool output]" body)))
        (let* ((shell-buffer (map-elt state :buffer))
               (transcript-file (and (buffer-live-p shell-buffer)
                                     (buffer-local-value 'agent-shell--transcript-file
                                                         shell-buffer)))
               (new-args (copy-sequence args)))
          (setq new-args
                (plist-put new-args :body
                           (my/agent-shell-tool-output-display-truncate
                            body transcript-file)))
          (apply orig new-args))
      (apply orig args))))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind (("C-c c" . claude-code-ide-menu)) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package agent-shell
  :ensure t
  :config
  (require 'agent-shell-chat-mode)
  (setq agent-shell-transcript-file-path-function
        #'my/agent-shell-transcript-file-path)
  (unless (advice-member-p #'my/agent-shell-chat-fix-live-prompt
                           'agent-shell-chat--label-prompts)
    (advice-add 'agent-shell-chat--label-prompts
                :after #'my/agent-shell-chat-fix-live-prompt))
  (require 'agent-shell-openai)
  (require 'agent-shell-cursor)
  (unless (advice-member-p #'my/agent-shell-tool-output-display-cap
                           'agent-shell--update-fragment)
    (advice-add 'agent-shell--update-fragment
                :around #'my/agent-shell-tool-output-display-cap))
  (require 'agent-shell-anthropic)
  (require 'agent-shell-auto-retry)

  ;; Lark (Feishu) bridge: drive agent-shell sessions from Lark chats.
  ;; Uses the bot app credentials configured with `lark-cli config init'.
  ;; Subscribe to im.message.receive_v1 and card.action.trigger over
  ;; long-connection.  Senders must be in the private allowlist below.
  (require 'agent-shell-lark)
  ;; $$...$$ blocks go out as rendered images (needs node + rsvg-convert
  ;; and a one-time `npm install' in ~/.emacs.d/lark-tools/).
  (setq agent-shell-lark-render-formulas t)
  ;; Keep thought relay off by default.  Unlike final messages, thoughts are
  ;; not exposed by the current public event API, so relaying them still needs
  ;; reading the transcript at turn end.
  (setq agent-shell-lark-relay-thoughts nil)
  ;; Private settings (e.g. agent-shell-lark-allowed-open-ids) stay
  ;; out of this repo; keep them in ~/.emacs.private.el.
  (when (file-exists-p "~/.emacs.private.el")
    (load (expand-file-name "~/.emacs.private.el") nil t))

  (global-set-key (kbd "C-c a o") #'agent-shell-openai-start-codex)
  (global-set-key (kbd "C-c a u") #'agent-shell-cursor-start-agent)
  (global-set-key (kbd "C-c a l") #'agent-shell-anthropic-start-claude-code)
  (global-set-key (kbd "C-c a a") #'agent-shell)
  (global-set-key (kbd "C-c a s") #'agent-shell-switch-buffer)

  (defun my/agent-shell-fix-auth ()
    "Fix \"OAuth session expired and could not be refreshed\" in Claude shells.

The claude CLI refreshes credentials in ~/.claude/.credentials.json,
while the claude-agent-acp adapter reads the macOS Keychain; when the
two drift apart, Claude shells fail to authenticate even though claude
works in a terminal.  Sync the Keychain entry from the file, then
reload live Claude sessions (conversations are preserved).

If shells still fail after this, the file credentials are stale too:
run /login in a terminal Claude Code session, then run this again."
    (interactive)
    (let ((creds (expand-file-name "~/.claude/.credentials.json")))
      (unless (file-exists-p creds)
        (user-error "%s not found; run /login in a terminal Claude Code session" creds))
      (unless (zerop (call-process-shell-command
                      (format "security add-generic-password -U -s \"Claude Code-credentials\" -a \"$USER\" -w \"$(cat %s)\""
                              (shell-quote-argument creds))))
        (user-error "Keychain sync failed"))
      (let (reloaded)
        (dolist (buffer (agent-shell-buffers))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and (eq (map-nested-elt (agent-shell--state)
                                             '(:agent-config :identifier))
                             'claude-code)
                         (map-nested-elt (agent-shell--state) '(:session :id)))
                (let ((name (buffer-name)))
                  (agent-shell-reload)
                  (push name reloaded))))))
        (message "Keychain synced; reloaded: %s"
                 (if reloaded
                     (string-join (nreverse reloaded) ", ")
                   "no live Claude sessions"))))))

(provide 'setup-ai)
