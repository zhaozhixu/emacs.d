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
  ;; Compiler-accurate identifier colors (clangd etc.) layered on top
  ;; of the mode's own font-lock.
  (lsp-semantic-tokens-enable t)
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

;; Semantic tokens paint every reference (calls, variables, operators,
;; struct/class members), which is too busy.  Drop those token faces so
;; the mode's own level-3 treesit fontification shows through
;; (python-like calm), and keep the high-value tokens: macros, types,
;; enum members, inactive #ifdef regions.  Definitions keep their
;; treesit colors.
;; (Both alists are defvar-local, hence setq-default; the face map is
;; cached per lsp workspace, so changes need `lsp-workspace-restart'.)
(with-eval-after-load 'lsp-semantic-tokens
  (setq-default lsp-semantic-token-faces
                (seq-remove (lambda (cell)
                              (member (car cell)
                                      '("function" "method" "variable"
                                        "parameter" "operator"
                                        "member" "property")))
                            (default-value 'lsp-semantic-token-faces)))
  (setq-default lsp-semantic-token-modifier-faces
                (seq-remove (lambda (cell)
                              (member (car cell)
                                      '("declaration" "definition"
                                        "implementation" "static")))
                            (default-value 'lsp-semantic-token-modifier-faces))))

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
    (if (and (derived-mode-p 'c-mode 'c++-mode 'c-ts-base-mode)
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
;; C/C++ remaps and styles live in the C/C++ section below.
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (python "https://github.com/tree-sitter/tree-sitter-python")
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

(defun my/c-lsp-setup ()
  "Shared C/C++ setup for cc-mode and c-ts-mode: clangd, lsp, folding."
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error" "--header-insertion=never"
                                  "--enable-config" "--compile-commands-dir=build")
        lsp-ui-flycheck-enable t
        lsp-enable-indentation nil
        lsp-enable-snippet t)
  (lsp-deferred)
  (hs-minor-mode)) ; folding source code

;; C/C++ on built-in c-ts-mode / c++-ts-mode (treesit).  Ported from
;; the old cc-mode styles (archived in custom/unused/setup-cc-mode.el):
;;   C default ("lightnet")        k&r rules, offset 4, spaces
;;   kernel trees                  linux rules, offset 8, tabs
;;   C++ default ("google-4-indent") k&r rules + namespace body not
;;     indented + access labels at half offset; offset 4
;;   onnxruntime                   same, offset 2 (plain google)

(defvar my/c-ts-half-offset 2
  "Buffer-local access-label indent: half of `c-ts-mode-indent-offset'.")

(defun my/kernel-tree-file-p ()
  "Non-nil when the current buffer visits a file in a kernel tree."
  (when-let* ((file buffer-file-name))
    (seq-some (lambda (tree)
                (string-prefix-p (expand-file-name tree) file))
              '("~/workspace/source/linux/" "~/workspace/projects/kernels/"))))

(defun my/c-ts-indent-style ()
  "Return per-buffer treesit indent rules (see the port table above)."
  (cond
   ((my/kernel-tree-file-p)
    (alist-get 'linux (c-ts-mode--indent-styles
                       (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))))
   ((derived-mode-p 'c++-ts-mode)
    ;; The regexp must be anchored: unanchored "declaration_list" also
    ;; matches field_declaration_list (class bodies).
    `(((node-is "access_specifier") parent-bol my/c-ts-half-offset)
      ((parent-is "\\`declaration_list\\'") parent-bol 0) ; namespace body
      ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))
   (t
    (alist-get 'k&r (c-ts-mode--indent-styles 'c)))))

(setq c-ts-mode-indent-style #'my/c-ts-indent-style)

(defun my/c-ts-setup ()
  "C/C++ treesit setup: lsp plus per-tree offsets and tabs."
  (my/c-lsp-setup)
  (cond ((my/kernel-tree-file-p)
         (setq-local c-ts-mode-indent-offset 8)
         (setq-local tab-width 8) ; kernel code is one 8-wide tab per level
         (setq indent-tabs-mode t))
        ((and (derived-mode-p 'c++-ts-mode)
              buffer-file-name
              (string-match-p "onnxruntime/" buffer-file-name))
         (setq-local c-ts-mode-indent-offset 2)
         (setq indent-tabs-mode nil))
        (t
         (setq-local c-ts-mode-indent-offset 4)
         (setq indent-tabs-mode nil)))
  (setq-local my/c-ts-half-offset (/ c-ts-mode-indent-offset 2))
  (setq show-trailing-whitespace t))

(add-hook 'c-ts-base-mode-hook #'my/c-ts-setup)

(dolist (remap '((c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Kernel headers are C, not C++: route kernel-tree .c/.h straight to
;; c-ts-mode, overriding the generic .h -> c++ entry above.
(dolist (tree '("~/workspace/source/linux" "~/workspace/projects/kernels"))
  (add-to-list 'auto-mode-alist
               (cons (concat "\\`" (regexp-quote (expand-file-name tree)) "/.*\\.[ch]\\'")
                     'c-ts-mode)))

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
