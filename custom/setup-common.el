(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'wsm 'whitespace-mode)

(setq gc-cons-threshold (* 100 1024 1024)
      inhibit-startup-message t
      cjk-font-size 30
      ansi-font-size 30
      system-uses-terminfo nil
      term-buffer-maximum-size 81920
      tool-bar-mode nil
      warning-suppress-types '((emacs)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			              "[ \t\n]*$" "" (shell-command-to-string
					                      "$SHELL --login -i -c 'echo $PATH'"
						                  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(define-key key-translation-map (kbd "<menu>") #'event-apply-super-modifier) ; super key
(global-set-key (kbd "C-x C-b") 'ibuffer)
(windmove-default-keybindings)
(column-number-mode t)
;; (global-linum-mode t)
(put 'set-goal-column 'disabled nil) ; unknown usage

;; Custom functions
(defun my/global-map-and-set-key (key command &optional prefix suffix)
  "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
 PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
  (my/map-key key)
  (global-set-key (kbd (concat prefix key suffix)) command))

(defun my/map-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY.\""
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

(defun uname-r ()
  "Return shell command result of 'uname -r'"
  (let ((temp-buffer (get-buffer-create "temp-buffer"))
        ret-string)
    (shell-command "uname -r" temp-buffer)
    (save-excursion
      (set-buffer temp-buffer)
      (setq ret-string (buffer-substring (point-min) (point-max)))
      )
    (kill-buffer temp-buffer)
    ;; (substring ret-string 0 (- (length ret-string) 1))
    (replace-regexp-in-string "\n" "" ret-string)
    ))

(provide 'setup-common)
