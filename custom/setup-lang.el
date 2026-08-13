;;; setup-lang.el --- Language modes and LSP setup -*- lexical-binding: t; -*-

(require 'setup-common)

;;; Global editing / process tuning -------------------------------------------

(setq read-process-output-max (* 1024 1024) ; large LSP responses
      backup-by-copying t ; or some imported paths will change to backup file
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.2
      company-minimum-prefix-length 1)

;;; LSP core -------------------------------------------------------------------

;; Notes:
;; - TypeScript needs a manually installed treesit grammar for highlighting:
;;     (setq treesit-language-source-alist
;;           '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;             (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
;;     (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; - "The package typescript is not installed" errors: M-x lsp-install-server ts-ls
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-idle-delay 0.1)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-symbol-highlighting nil)
  ;; File watchers cost one fd per watched dir (kqueue); the fd limit
  ;; is raised via ec's ulimit and /Library/LaunchDaemons/limit.maxfiles.
  ;; The threshold acts as a circuit breaker well below the 65536 limit.
  (lsp-file-watch-threshold 20000)
  (lsp-lens-enable nil)
  (lsp-disabled-clients '(ts-ls))
  :hook (((typescript-ts-mode tsx-ts-mode js-ts-mode vue-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\workspace/test\\'")
  (require 'dap-cpptools)
  ;; lsp seems to ring bells too often
  (setq ring-bell-function
        (lambda ()
          (if (memq this-command
                    '(isearch-abort abort-recursive-edit keyboard-quit))
              (ding))))
  ;; lsp seems to use this func even when no X
  (if (not (boundp 'x-hide-tip))
      (defun x-hide-tip () nil)))

(use-package lsp-ui
  :ensure t)

(my/global-map-and-set-key "C-:" 'lsp-toggle-symbol-highlight)
(defun my/format-buffer-dwim ()
  "Format the buffer (or region): clang-format when the project has a
.clang-format, cc-mode style indentation as fallback in C/C++,
`lsp-format-buffer' elsewhere."
  (interactive)
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (if (and (derived-mode-p 'c-mode 'c++-mode)
             (not (or (locate-dominating-file default-directory ".clang-format")
                      (locate-dominating-file default-directory "_clang-format"))))
        (indent-region beg end)
      (if (use-region-p)
          (lsp-format-region beg end)
        (lsp-format-buffer)))))

(my/global-map-and-set-key "C-." 'my/format-buffer-dwim)
(my/global-map-and-set-key "M-p" 'lsp-execute-code-action)
(my/global-map-and-set-key "M-I" 'lsp-ui-doc-glance)

;;; Tree-sitter and indent guides ----------------------------------------------

;; Built-in treesit (Emacs 29+): grammar sources and ts-mode remaps.
;; Install/update grammars with M-x treesit-install-language-grammar.
;; C/C++ stay on cc-mode (custom c-add-style setups don't port).
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")))

;; Install missing grammars at startup (one-time per machine; needs
;; git and a C compiler, same as fresh package installs).
(when (treesit-available-p)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(dolist (remap '((python-mode . python-ts-mode)
                 (yaml-mode . yaml-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . js-ts-mode)
                 (js2-mode . js-ts-mode)
                 (go-mode . go-ts-mode)
                 (rust-mode . rust-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package indent-bars
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode)
  :custom
  ;; Clamp bars to real bracket nesting inside multi-line brackets.
  ;; (use-package silently drops duplicate :custom keys - keep one entry.)
  (indent-bars-no-descend-lists 'skip)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

;;; C / C++ ---------------------------------------------------------------------

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun my/c-common-setup ()
  "Shared cc-mode setup: clangd flags, styles, folding."
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error" "--header-insertion=never"
                                  "--enable-config" "--compile-commands-dir=build")
        lsp-ui-flycheck-enable t
        lsp-enable-indentation nil
        lsp-enable-snippet t)
  (lsp-deferred)
  ;; Linux kernel style
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
      (defun-block-intro . 4)
      (defun-close . 0)
      (defun-open . 0)
      (statement . 0)
      (statement-cont . ++)
      (substatement . +)
      (topmost-intro . 0)
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
  (hs-minor-mode)) ; folding source code

(defun my/c-setup ()
  "C style: lightnet, kernel style for kernel trees."
  (c-set-style "lightnet")
  (let ((filename (buffer-file-name)))
    (when (and filename
               (or (string-match (expand-file-name "~/workspace/source/linux") filename)
                   (string-match (expand-file-name "~/workspace/projects/kernels") filename)))
      (setq indent-tabs-mode t)
      (setq show-trailing-whitespace t)
      (c-set-style "linux-tabs-only"))))

(defun my/c++-setup ()
  "C++ style: google-4-indent, plain google for onnxruntime."
  (c-set-style "google-4-indent")
  (let ((filename (buffer-file-name)))
    (when (and filename
               (string-match "onnxruntime/" filename))
      (c-set-style "google"))))

(add-hook 'c-mode-common-hook #'my/c-common-setup)
(add-hook 'c-mode-hook #'my/c-setup)
(add-hook 'c++-mode-hook #'my/c++-setup)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;;; Python ----------------------------------------------------------------------

(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
  ;; (setenv "WORKON_HOME" <your-pyworkon-venvs-folder>)
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode t))

(defun my/python-setup ()
  "Python LSP via pyright."
  (setq lsp-pyright-python-executable-cmd "python3")
  (lsp-deferred))

(add-hook 'python-ts-mode-hook #'my/python-setup)
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . python-mode))

;;; Go --------------------------------------------------------------------------

(defun my/go-setup ()
  "Go LSP with format/imports on save."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (unless (string-match "go" compile-command)
    (set (make-local-variable 'compile-command)
         "go test -v && go vet"))
  (setq lsp-gopls-staticcheck t
        lsp-gopls-complete-unimported t)
  (lsp-deferred))

(use-package go-mode
  :mode "\\.go\\'")
(add-hook 'go-ts-mode-hook #'my/go-setup)

;;; Rust ------------------------------------------------------------------------

(defun my/rust-setup ()
  (lsp-deferred)
  (setq lsp-rust-analyzer-lens-enable nil))

(add-hook 'rust-ts-mode-hook #'my/rust-setup)

;;; Octave ----------------------------------------------------------------------

(use-package octave
  :mode ("\\.m\\'" . octave-mode)
  :custom
  (inferior-octave-prompt ">> ")
  :config
  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1))))

;;; Perl ------------------------------------------------------------------------

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (cperl-set-style "PerlStyle")))
(with-eval-after-load 'cperl-mode
  ;; cperl's electric braces fight with smartparens (typing "{" yields
  ;; "{{}"); let smartparens own the pairing keys.
  (define-key cperl-mode-map "{" nil)
  (define-key cperl-mode-map "}" nil))

;;; Racket ----------------------------------------------------------------------

(use-package racket-mode
  :mode ("\\.rkt\\'" "\\.\\(scm\\|sls\\|sld\\|stk\\|ss\\|sch\\)\\'")
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . flycheck-mode)
         (racket-mode . lsp-deferred))
  :custom
  (racket-show-functions '(racket-show-echo-area))
  :config
  (require 'racket-xp))

;;; Dart & Flutter --------------------------------------------------------------

(defun my/dart-setup ()
  "Dart LSP; fix company completion after '.'."
  (lsp-deferred)
  (setq lsp-ui-doc-show-with-mouse nil)
  ;; workaround: lsp-dart doesn't company after '.'
  (advice-add 'lsp-completion--looking-back-trigger-characterp :around
              (defun lsp-completion--looking-back-trigger-characterp@fix-dart-trigger-characters (orig-fn trigger-characters)
                (funcall orig-fn
                         (if (and (derived-mode-p 'dart-mode) (not trigger-characters))
                             ["." "=" "(" "$"]
                           trigger-characters))))
  (flutter-test-mode))

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . my/dart-setup))

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
  :custom (flutter-sdk-path "/Users/zhixu/workspace/tools/flutter"))

;;; Web / JS / TS ---------------------------------------------------------------

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\'" "\\.php\\'" "\\.ctp\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . js-mode))

;; Disable ugly background in vue-mode
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

;;; SQL -------------------------------------------------------------------------

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-set-product "postgres")))
(use-package sql-indent
  :ensure t
  :after sql
  :hook (sql-mode . sqlind-minor-mode))

;;; Org -------------------------------------------------------------------------

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

;;; Markdown --------------------------------------------------------------------

(use-package markdown-mode
  :custom
  (markdown-command "/usr/bin/pandoc"))

;;; LaTeX (not used now) ----------------------------------------------------------
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

(provide 'setup-lang)
