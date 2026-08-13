;;; setup-cc-mode.el --- Old cc-mode C/C++ styles (replaced by c-ts-mode) -*- lexical-binding: t; -*-

;; Moved out of setup-lang.el in 2026-08 when C/C++ migrated to the
;; built-in treesit modes (c-ts-mode / c++-ts-mode).  Kept for
;; reference: the treesit port approximates these cc-mode styles
;; (lightnet -> k&r + offset 4; kernel trees -> linux + tabs 8;
;; google-4-indent -> namespace body 0, access labels offset/2).
;; To restore, load this file and re-add the hooks/auto-mode entries
;; at the bottom (also remove the c-mode/c++-mode entries from
;; `major-mode-remap-alist' in setup-lang.el).

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
  (my/c-lsp-setup)
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
     (show-trailing-whitespace . t))))

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

;; (add-hook 'c-mode-common-hook #'my/c-common-setup)
;; (add-hook 'c-mode-hook #'my/c-setup)
;; (add-hook 'c++-mode-hook #'my/c++-setup)
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
