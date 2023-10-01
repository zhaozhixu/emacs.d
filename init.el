(require 'package)

(let ((pkg-official
       '(("melpa" . "https://melpa.org/packages/")
         ("gnu"   . "https://elpa.gnu.org/packages/")
         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
      (pkg-tuna
       '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
         ("nongnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
      (pkg-emacs-china
       '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
         ("melpa" . "http://elpa.emacs-china.org/melpa/")
         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))))
  (setq package-archives pkg-official))

(setq package-check-signature nil)
(package-initialize)
(setq warning-minimum-level :emergency)

(defconst my-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    clean-aindent-mode
    comment-dwim-2
    diminish
    bind-key
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    use-package
    zygospore
    window-number
    flycheck
    web-mode
    google-c-style
    protobuf-mode
    magit
    yaml-mode
    dockerfile-mode
    racket-mode
    go-mode
    lsp-mode
    lsp-treemacs
    lsp-pyright
    lsp-ui
    helm-lsp
    hydra
    avy
    which-key
    ;; helm-xref
    dap-mode
    leetcode
    csv-mode
    thrift
    markdown-mode
    cmake-mode
    sr-speedbar
    bison-mode
    message-view-patch
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))
(install-packages)

(add-to-list 'load-path "~/.emacs.d/custom")

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'setup-common)
(require 'setup-helm)
(require 'setup-editing)
(require 'setup-yasnippet)
(require 'setup-lang)
(require 'setup-misc)
(require 'setup-mu4e)

(setq custom-file "~/.emacs.d/custom/custom.el")
(load custom-file)
