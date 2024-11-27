(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'wsm 'whitespace-mode)

(setq gc-cons-threshold (* 1024 1024 1024)
      inhibit-startup-message t
      cjk-font-size 30
      ansi-font-size 30
      system-uses-terminfo nil
      term-buffer-maximum-size 81920
      tool-bar-mode nil
      tooltip-mode -1
      warning-suppress-types '((emacs)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

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
  (define-key function-key-map (concat "\e[emacs-" key "]") (kbd key)))

(my/global-map-and-set-key "C-;" 'iedit-mode)

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

(defun delete-window-by-buffer-name (buffer-name)
  "Delete the window displaying the buffer with the given name."
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (let ((window (get-buffer-window buffer t)))
          (if window
              (delete-window window)
            (message "No window found displaying buffer: %s" buffer-name)))
      (message "No buffer found with name: %s" buffer-name))))

(defun display-buffer-and-append-if-not-visible (buffer-or-name)
  "Display buffer at the bottom of the frame and move cursor to the end,
   but only if the buffer is not already visible in any window."
  (interactive "bBuffer to display: ")
  (let* ((buffer (get-buffer-create buffer-or-name))
         (window (or (get-buffer-window buffer t) (display-buffer buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (if window
          (set-window-point window (point-max))))))


(provide 'setup-common)
