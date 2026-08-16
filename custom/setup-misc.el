;; Appearance
(setq modus-themes-bold-constructs t
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold)))
      modus-themes-prompts '(semibold)
      modus-themes-headings
      '((1 . (bold 1.15))
        (2 . (semibold 1.1))
        (t . (semibold)))
      modus-themes-common-palette-overrides
      '((bg-mode-line-active bg-blue-subtle)
        (border-mode-line-active blue)
        (bg-mode-line-inactive bg-dim)
        (border-mode-line-inactive bg-dim)
        (bg-region bg-blue-subtle)
        (fringe unspecified)))

(defconst my-modern-theme 'modus-vivendi)
(defconst my-legacy-theme 'deeper-blue)

(defun my-apply-theme-frame-adjustments (frame)
  "Apply theme-specific adjustments to FRAME."
  (when (and (eq (car custom-enabled-themes) my-legacy-theme)
             (not (display-graphic-p frame)))
    ;; Preserve the old deeper-blue terminal appearance.
    (set-face-background 'default "black" frame)))

(defun my-apply-theme-adjustments-to-existing-frames ()
  "Apply theme-specific adjustments to all existing frames."
  (dolist (frame (frame-list))
    (my-apply-theme-frame-adjustments frame)))

(defun my-load-theme (theme)
  "Load THEME without blending it with an already enabled theme."
  (interactive
   (list (intern
          (completing-read "Theme: "
                           (mapcar #'symbol-name
                                   (list my-modern-theme my-legacy-theme))
                           nil t))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (my-apply-theme-adjustments-to-existing-frames)
  (message "Loaded theme: %s" theme))

(defun my-toggle-theme ()
  "Toggle between the modern and legacy dark themes."
  (interactive)
  (my-load-theme
   (if (eq (car custom-enabled-themes) my-modern-theme)
       my-legacy-theme
     my-modern-theme)))

(add-hook 'after-make-frame-functions #'my-apply-theme-frame-adjustments)
(add-hook 'emacs-startup-hook #'my-apply-theme-adjustments-to-existing-frames)
(my-load-theme my-legacy-theme)

(add-hook 'prog-mode-hook #'hl-line-mode)

(global-set-key [(f11)] 'loop-alpha)
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list))) ; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    ))

;; xterm
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; Dired
(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook 'dired-omit-mode)
(setq dired-omit-files "\\`[.]?#\\|\\`[.]?\\'\\|\\.bak"
      dired-listing-switches "-alh")

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; GDB
(setq gdb-many-windows t
      gdb-show-main t ; display source file containing the main routine at startup
      )

;; Package: semantic
(add-hook 'semantic-inhibit-functions
          (lambda () (member major-mode '(c-mode c++-mode python-mode go-mode lsd-mode))))

;; Package: zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Package: company
(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode))

;; Package: window-number
(use-package window-number
  :demand t
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1)
  ;; :bind
  ;; ("C-x o" . window-number-switch)
  )

;; Package: which-key-mode
(use-package which-key
  :diminish which-key-mode)
(which-key-mode)

;; Package: ansi-color
(require 'ansi-color)
(defun my-ansi-color (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Package: sr-speedbar
(use-package sr-speedbar
  :defer t
  :custom
  (sr-speedbar-default-width 30)
  (sr-speedbar-max-width 30)
  (sr-speedbar-right-side nil)
  (sr-speedbar-skip-other-window-p nil))
(add-hook 'speedbar-mode
          (lambda ()
            (local-set-key [94] (quote speedbar-up-directory))
            (local-set-key [102] (quote speedbar-edit-line))))

;; Package: projectile
(use-package projectile
  :hook (after-init . projectile-mode)
  :config (setq projectile-enable-caching t))

;; Package: helm-projectile
(use-package helm-projectile
  :after (projectile)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

;; Package: leetcode
(use-package leetcode
  :defer t
  :custom
  (leetcode-save-solutions t)
  (leetcode-directory "~/workspace/projects/leetcode")
  (leetcode-prefer-language "cpp"))

;; Other
(defun my-restart-lsp ()
  (interactive)
  (lsp-workspace-restart)
  (yas-minor-mode-on))

(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(setq flycheck-mode-line nil)
(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))

(if (not (version< emacs-version "29"))
    (pixel-scroll-precision-mode))

;; WSL clipboard fix
;; (setq interprogram-cut-function
;;       (lambda (text)
;;         (with-temp-buffer
;;           (insert text)
;;           (call-process-region (point-min) (point-max) "win32yank.exe" nil 0 nil "-i" "--crlf"))))

;; (setq interprogram-cut-function
;;       (lambda (text)
;;         (with-temp-buffer
;;           (insert text)
;;           (call-process-region (point-min) (point-max)
;;                                "ssh" nil 0 nil "-p" "2222" "zhixu@10.0.0.73"
;;                                "/mnt/c/ProgramData/chocolatey/bin/win32yank.exe -i --crlf"))))


;; (setq use-system-tooltips nil)
;; (setq use-dialog-box nil)
(setq tooltip-mode nil)

(provide 'setup-misc)
