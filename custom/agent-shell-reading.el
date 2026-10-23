;;; agent-shell-reading.el --- Ask about code and Magit diffs -*- lexical-binding: t; -*-

;;; Commentary:
;; C-c a e prepares an explanation in agent-shell.  Edit the question to
;; request a review, then submit normally.  Prefix arguments follow
;; agent-shell-send-dwim: C-u starts a shell; C-u C-u picks an existing one.

;;; Code:

(require 'agent-shell)
(require 'magit-diff)
(require 'thingatpt)

(defun my/agent-shell-reading--context ()
  "Return an explanation prompt for the region or scope at point.
Use the major mode's defun navigation for functions and classes, or
the current hunk/file section in Magit.  Include actual buffer text."
  (save-excursion
    (let* ((region (use-region-p))
           (diff (derived-mode-p 'magit-mode))
           (section (and diff
                         (magit-section-at (if region
                                               (region-beginning)
                                             (point)))))
           (header (and section (magit-diff-file-header section)))
           (bounds
            (cond
             (region (cons (region-beginning) (region-end)))
             (diff
              (unless header
                (user-error "Move to a diff hunk or file, or select a region"))
              (when (oref section washer)
                (let ((hidden (oref section hidden)))
                  (magit-section-show section)
                  (when hidden (magit-section-hide section))))
              (cons (oref section start) (oref section end)))
             (t (or (bounds-of-thing-at-point 'defun)
                    (user-error "No function or class at point; select a region"))))))
      (when (and region section (magit-hunk-section-p section))
        (setq header
              (concat header
                      (buffer-substring-no-properties
                       (oref section start) (oref section content)))))
      (format "Explain this.\n\nSource: %s\nMode: %s\nBuffer lines: %d-%d\n%s\n%s"
              (or buffer-file-name (buffer-name)) major-mode
              (line-number-at-pos (car bounds) t)
              (line-number-at-pos (max (car bounds) (1- (cdr bounds))) t)
              (if diff
                  (format "Diff: %s\nRevision/range: %s\n%s"
                          (magit-diff-type section)
                          (or magit-buffer-diff-range magit-buffer-revision
                              "not specified; use the displayed diff")
                          (or header ""))
                "")
              (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun my/agent-shell-ask-dwim (&optional arg)
  "Prepare a question about the selected code or Magit diff.
Default to explaining the region, enclosing function/class, or Magit
hunk/file.  Edit the question in agent-shell, then submit normally.
Pass prefix ARG to `agent-shell-send-dwim': C-u starts a new shell;
C-u C-u chooses an existing shell."
  (interactive "P")
  (let* ((context (my/agent-shell-reading--context))
         (agent-shell-context-sources (list (lambda () context))))
    (agent-shell-send-dwim arg)))

(defvar my/agent-shell-reading-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a e") #'my/agent-shell-ask-dwim)
    map)
  "Keymap for `my/agent-shell-reading-mode'.")

(define-minor-mode my/agent-shell-reading-mode
  "Ask agent-shell about code or a Magit diff with C-c a e."
  :lighter " AI"
  :keymap my/agent-shell-reading-mode-map)

(add-hook 'prog-mode-hook #'my/agent-shell-reading-mode)
(add-hook 'magit-mode-hook #'my/agent-shell-reading-mode)

(provide 'agent-shell-reading)
;;; agent-shell-reading.el ends here
