(require 'setup-common)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind (("C-c c" . claude-code-ide-menu)) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package agent-shell
  :ensure t
  :config
  (require 'agent-shell-openai)
  (require 'agent-shell-cursor)
  (require 'agent-shell-anthropic)
  (require 'agent-shell-auto-retry)
  (my/agent-shell-global-retry-mode 1)

  ;; Lark (Feishu) bridge: drive agent-shell sessions from Lark chats.
  ;; Needs the official lark-cli on PATH (npm install -g @larksuite/cli),
  ;; configured via `lark-cli config init' and `lark-cli auth login' for
  ;; a bot app with im:message scope and im.message.receive_v1 over
  ;; long-connection.  Deny-all until the allowlist is set; see
  ;; commentary in agent-shell-lark.el.
  (require 'agent-shell-lark)
  ;; Stream progress summaries during long turns and ping with an
  ;; at-mention when a turn completes.
  (setq agent-shell-lark-relay-progress-messages t)
  (setq agent-shell-lark-mention-on-turn-complete t)
  ;; $$...$$ blocks go out as rendered images (needs node + rsvg-convert
  ;; and a one-time `npm install' in ~/.emacs.d/lark-tools/).
  (setq agent-shell-lark-render-formulas t)
  ;; Private settings (e.g. agent-shell-lark-allowed-open-ids) stay
  ;; out of this repo; keep them in ~/.emacs.private.el.
  (when (file-exists-p "~/.emacs.private.el")
    (load (expand-file-name "~/.emacs.private.el") nil t))

  (global-set-key (kbd "C-c a o") #'agent-shell-start-codex)
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
