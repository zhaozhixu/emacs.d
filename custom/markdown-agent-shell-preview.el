;;; markdown-agent-shell-preview.el --- Markdown previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Render a copy of a Markdown buffer with agent-shell.  C-c p opens the
;; preview; g refreshes it, and C-c p returns to the editable source.

;;; Code:

(declare-function agent-shell-markdown-replace-markup "agent-shell-markdown")
(declare-function agent-shell-markdown-rerender-tables "agent-shell-markdown")

(defvar markdown-ts-mode-map)
(defvar markdown-mode-map)

(defvar-local my/markdown-preview-buffer nil
  "Preview buffer associated with this Markdown source buffer.")

(defvar-local my/markdown-preview-source-buffer nil
  "Markdown source buffer associated with this preview.")

(defun my/markdown-preview-refresh (&rest _)
  "Refresh this preview from its source, including unsaved edits."
  (interactive)
  (unless (buffer-live-p my/markdown-preview-source-buffer)
    (user-error "The Markdown source buffer has been killed"))
  (let ((source my/markdown-preview-source-buffer)
        (position (point))
        (inhibit-read-only t))
    (setq default-directory (buffer-local-value 'default-directory source))
    (erase-buffer)
    (insert (with-current-buffer source
              (save-restriction
                (widen)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (agent-shell-markdown-replace-markup :force t)
    (goto-char (min position (point-max)))
    (set-buffer-modified-p nil)))

(defun my/markdown-preview-resize (_window)
  "Fit rendered tables to the preview window after it changes size."
  (agent-shell-markdown-rerender-tables))

(defun my/markdown-preview-source ()
  "Return from the preview to its editable Markdown source."
  (interactive)
  (unless (buffer-live-p my/markdown-preview-source-buffer)
    (user-error "The Markdown source buffer has been killed"))
  (pop-to-buffer-same-window my/markdown-preview-source-buffer))

(defvar my/markdown-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'my/markdown-preview-refresh)
    (define-key map (kbd "C-c p") #'my/markdown-preview-source)
    map)
  "Keymap for `my/markdown-preview-mode'.")

(define-derived-mode my/markdown-preview-mode special-mode "Markdown Preview"
  "Read-only Markdown rendered by agent-shell.
Use g to refresh, C-c p to return to the source, and q to quit."
  (setq-local revert-buffer-function #'my/markdown-preview-refresh)
  (setq-local header-line-format "Markdown preview: g refresh · C-c p source · q quit")
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (buffer-disable-undo)
  (add-hook 'window-size-change-functions #'my/markdown-preview-resize nil t))

;;;###autoload
(defun my/markdown-preview ()
  "Preview this Markdown buffer using agent-shell's renderer.
Render a separate, read-only copy, leaving the source untouched.
Repeated invocations reuse this source buffer's preview."
  (interactive)
  (require 'agent-shell-markdown)
  (let ((source (current-buffer)))
    (unless (buffer-live-p my/markdown-preview-buffer)
      (setq my/markdown-preview-buffer
            (generate-new-buffer (format "*Markdown preview: %s*" (buffer-name))))
      (with-current-buffer my/markdown-preview-buffer
        (my/markdown-preview-mode)
        (setq my/markdown-preview-source-buffer source)))
    ;; Display before rendering so tables use the destination window's width.
    (pop-to-buffer-same-window my/markdown-preview-buffer)
    (my/markdown-preview-refresh)))

(with-eval-after-load 'markdown-ts-mode
  (define-key markdown-ts-mode-map (kbd "C-c p") #'my/markdown-preview))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c p") #'my/markdown-preview))

(provide 'markdown-agent-shell-preview)
;;; markdown-agent-shell-preview.el ends here
