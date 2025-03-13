(require 'setup-common)

;; lsp-mode
;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
;; (require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(setq read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.2
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-enable t
      lsp-enable-symbol-highlighting nil
      lsp-lens-mode 0
      backup-by-copying t ;; or some imported paths will change to backup file
      )

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  ;; (yas-global-mode)
  ;; lsp seems to ring bells too often
  (setq ring-bell-function
        (lambda ()
	      (if (memq this-command
		            '(isearch-abort abort-recursive-edit keyboard-quit))
	        (ding))))
  ;; lsp seems to use this func even when no X
  (if (not (boundp 'x-hide-tip))
      (defun x-hide-tip () nil)))

(my/global-map-and-set-key "C-:" 'lsp-toggle-symbol-highlight)
(my/global-map-and-set-key "C-." 'lsp-format-buffer)
(my/global-map-and-set-key "M-p" 'lsp-execute-code-action)
(my/global-map-and-set-key "M-I" 'lsp-ui-doc-glance)

(use-package lsp-ui
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-function :before-while tree-sitter-hl-face-mapping-function
	      (lambda (capture-name)
		(not (or (string= capture-name "function.call")
			 (string= capture-name "method.call")
			 (string= capture-name "property")))))
)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

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
             ;; lsp-ui-doc-enable nil
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
(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
  ;; (setenv "WORKON_HOME" <your-pyworkon-venvs-folder>)
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode t))

(add-hook 'python-mode-hook
          (lambda ()
            (lsp-deferred)
            (setq lsp-pyright-python-executable-cmd "python3"
                  lsp-file-watch-threshold 200000
                  ;; lsp-ui-doc-enable nil
                  )
            (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\workspace/test\\'")))
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . python-mode))

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
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . js-mode))

;; Racket
(use-package racket-mode
  :mode ("\\.rkt\\'" "\\.\\(scm\\|sls\\|sld\\|stk\\|ss\\|sch\\)\\'")
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (flycheck-mode)
  ;; (add-hook 'racket-xp-mode-hook
  ;;           (lambda ()
  ;;             (remove-hook 'pre-redisplay-functions
  ;;                          #'racket-xp-pre-redisplay
  ;;                          t)))
  :custom
  (racket-show-functions '(racket-show-echo-area)))
;; (add-hook 'racket-mode-hook 'lsp-deferred)

;; Dart & flutter
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . (lambda ()
                       (lsp-deferred)
                       (setq
                        ;; lsp-ui-doc-enable nil
                        lsp-ui-doc-show-with-mouse nil
                        )
                       ;; workaround lsp-dart doesn't company after '.'
                       (advice-add 'lsp-completion--looking-back-trigger-characterp :around
                                   (defun lsp-completion--looking-back-trigger-characterp@fix-dart-trigger-characters (orig-fn trigger-characters)
                                     (funcall orig-fn
                                              (if (and (derived-mode-p 'dart-mode) (not trigger-characters))
                                                  ["." "=" "(" "$"]
                                                trigger-characters))))
                       (flutter-test-mode))))

(defun my-flutter-build-run-watch ()
  "Run `flutter pub run build_watch build`."
  (interactive)
  (flutter--from-project-root
   (let* ((buffer-name "*Flutter-build-runner-watch*")
          (buffer (flutter--get-buffer-create buffer-name))
          (alive (comint-check-proc buffer-name)))
     (unless alive
       (apply #'make-comint-in-buffer "Flutter-build-runner-watch" buffer (flutter-build-command) nil '("pub" "run" "build_runner" "watch"))
       (display-buffer buffer)))))

(defun my-flutter-run-or-hot-reload ()
  "Run 'flutter run' or perform a hot reload, and open the *Flutter* buffer."
  (interactive)
  (my-flutter-build-run-watch)
  (flutter-run-or-hot-reload)
  ;; (switch-to-buffer-other-window "*Flutter*")
  (display-buffer-and-append-if-not-visible flutter-buffer-name))

(defun my-flutter-delete-flutter-window ()
  (interactive)
  (delete-window-by-buffer-name "*Flutter*"))

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'my-flutter-run-or-hot-reload)
              ("C-M-c" . #'my-flutter-delete-flutter-window))
  :custom (flutter-sdk-path "/Users/zhixu/workspace/flutter"))

;; Rust
(add-hook 'rust-mode-hook (lambda ()
                            (lsp-deferred)
                            (setq lsp-rust-analyzer-lens-enable nil)))

;; Typescript
;; Have to manully install treesit library to get syntax highlight work:
;; (setq treesit-language-source-alist
;;       '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; May encounter: Error processing message (error "The package typescript is not installed.  Unable to find nil").
;; Have to install ts-ls server to workaround this problem:
;; M-x lsp-install-server ts-ls

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-disabled-clients '(ts-ls))
  :hook ((typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js2-mode . lsp-deferred))
  )

;; SQL
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-set-product "postgres")))
(use-package sql-indent
  :ensure t
  :after sql
  :hook (sql-mode . sqlind-minor-mode))

(provide 'setup-lang)
