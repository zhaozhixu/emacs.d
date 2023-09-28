(require 'setup-common)

;; lsp-mode
;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(setq read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-enable t
      lsp-enable-symbol-highlighting nil
      )

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(my/global-map-and-set-key "C-:" 'lsp-toggle-symbol-highlight)
(my/global-map-and-set-key "C-." 'lsp-format-buffer)

(use-package lsp-ui
  :ensure t)

;; C
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq
             lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error" "--header-insertion=never"
                                       "--enable-config" "--compile-commands-dir=build")
             lsp-ui-doc-enable nil
             ;; lsp-ui-peek-enable nil
             ;; lsp-ui-sideline-enable nil
             ;; lsp-ui-imenu-enable nil
             lsp-ui-flycheck-enable t
             lsp-enable-indentation nil
             lsp-enable-snippet t
             ;; lsp-enable-relative-indentation t
             ;; lsp-enable-on-type-formatting t
             )
            (lsp-deferred)
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux"
               (c-offsets-alist
                (arglist-cont-nonempty
                 c-lineup-gcc-asm-reg
                 c-lineup-arglist-tabs-only))
               (indent-tabs-mode . t)
               (show-trailing-whitespace . t)))
            (c-add-style
             "lightnet"
             '("k&r"
               (c-basic-offset . 4)
               (c-offsets-alist
                (defun-block-intro . 4) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (defun-open . 0)        ; Guessed value
                (statement . 0)         ; Guessed value
                (statement-cont . ++)   ; Guessed value
                (substatement . +)      ; Guessed value
                (topmost-intro . 0)     ; Guessed value
                (arglist-cont-nonempty
                 c-lineup-gcc-asm-reg
                 c-lineup-arglist))
               (indent-tabs-mode . nil)
               (show-trailing-whitespace . t)))
            (c-add-style
             "google-4-indent"
             '("Google"
               (c-basic-offset . 4)
               (c-offsets-alist
                (innamespace . -4))
               (show-trailing-whitespace . t)))
            (hs-minor-mode) ; folding source code
            ))

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "lightnet")
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (or (string-match (expand-file-name "~/source/linux")
                                           filename)
                             (string-match (expand-file-name "~/workspace/projects/kernels")
                                           filename)))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

;; C++
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "google-4-indent")
            (let ((filename (buffer-file-name)))
              (when (and filename
                         (or (string-match "onnxruntime/" filename)))
                (c-set-style "google")))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Python
(use-package lsp-pyright
  :ensure t
  :mode ("\\.py\\'" "\\BUILD\\'")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)
                         (setq lsp-ui-doc-enable nil)))
  :init
  (setq lsp-pyright-python-executable-cmd "python3")
  (setq lsp-file-watch-threshold 200000)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\workspace/test\\'")
  )

;; Go
(use-package lsp-go
  :mode "\\.go\\'"
  :hook (go-mode . (lambda ()
                     ;; ; Use goimports instead of go-fmt
                     ;; (setq gofmt-command "goimports")
                     ;; ; Call Gofmt before saving
                     ;; (add-hook 'before-save-hook 'gofmt-before-save)
                     (add-hook 'before-save-hook #'lsp-format-buffer t t)
                     (add-hook 'before-save-hook #'lsp-organize-imports t t)
                                        ; Customize compile command to run go build
                     (if (not (string-match "go" compile-command))
                         (set (make-local-variable 'compile-command)
                              "go test -v && go vet"))
                     (setq
                      lsp-gopls-staticcheck t
                      ;; lsp-eldoc-render-all t
                      lsp-gopls-complete-unimported t
                      )
                     (lsp-deferred))))

;; Octave
(use-package octave-mode
  :defer t
  :mode "\\.m\\'"
  :config
  (autoload 'octave-mode "octave-mod" nil t)
  (setq auto-mode-alist
        (cons '("\\.m$" . octave-mode) auto-mode-alist))

  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1))))
  (setq inferior-octave-prompt ">> ")
  (autoload 'run-octave "octave-inf" nil t))

;; Perl
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (cperl-set-style "PerlStyle")))

;; org-mode
(use-package org
  :defer t
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-c." 'org-time-stamp)
  (setq org-log-done t)
  (setq org-src-fontify-natively t) ; highlight code block
  ;; use minted to highlight code in latex
  ;; (require 'ox-latex)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (setq org-latex-listings 'minted)
  )

;; Markdown
(use-package markdown-mode
  :custom
  (markdown-command "/usr/bin/pandoc"))

;; LaTeX not used now
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook (lambda()
;;                              ;; 设置编译引擎为 XeTeX
;;                              (setq TeX-global-PDF-mode t TeX-engine 'xetex)
;;                              ;; 使用XeLaTeX作为默认程序来编译LaTeX
;;                              (add-to-list 'TeX-command-list '("XeLaTeX" "%'xelatex%(mode)%' %t"TeX-run-TeX nil t))
;;                              (setq TeX-command-default "XeLaTeX")
;;                              ))

;; web-mode
(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\'" "\\.php\\'" "\\.ctp\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.js\\'" "\\.html?\\'"))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . js-mode))

;; Racket
(use-package racket-mode
  :mode ("\\.rkt\\'" "\\.\\(scm\\|sls\\|sld\\|stk\\|ss\\|sch\\)\\'")
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-xp-mode-hook
            (lambda ()
              (remove-hook 'pre-redisplay-functions
                           #'racket-xp-pre-redisplay
                           t))))
;; (add-hook 'racket-mode-hook 'lsp-deferred)

;; other
;; (use-package lsp-dart
;;              :ensure t
;;              :hook (dart-mode . lsp))

(provide 'setup-lang)
