(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   '("795ff44faa4aadccbbf844791241d4d35c7ae962e5ef9b7844a42546097e6b19" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab" default))
 '(dired-omit-extensions
   '(".o" "~" ".bin" ".lbin" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs"))
 '(ibuffer-saved-filter-groups
   '(("perl"
      ("Perl"
       (used-mode . cperl-mode)))
     ("m"
      ("Octave"
       (used-mode . octave-mode)))
     ("c"
      ("C"
       (used-mode . c-mode)
       (used-mode . c-mode)))
     ("C"
      ("C"
       (used-mode . c-mode)))
     ("tex-ve"
      ("Verilog"
       (used-mode . verilog-mode))
      ("LaTeX"
       (used-mode . latex-mode)))
     ("tex"
      ("LaTeX"
       (used-mode . latex-mode)))
     ("verilog"
      ("Verilog"
       (used-mode . verilog-mode)))
     ("c++"
      ("C++"
       (used-mode . c++-mode))
      ("Verilog"
       (used-mode . verilog-mode)))
     ("Verilog"
      ("Verilog"
       (used-mode . verilog-mode)))))
 '(ibuffer-saved-filters
   '(("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode))))))
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]debug\\'" "[/\\\\].cache\\'" "[/\\\\]__pycache__\\'"))
 '(lsp-file-watch-ignored-files
   '("[/\\\\]flycheck_[^/\\\\]+\\'" "[/\\\\]\\.#[^/\\\\]+\\'" "[/\\\\][^/\\\\]+~\\'" "[/\\\\][^/\\\\]+.bak\\'" "[/\\\\][^/\\\\]+.o\\'" "[/\\\\][^/\\\\]+.a\\'" "[/\\\\][^/\\\\]+.so\\'" "[/\\\\][^/\\\\]+.onnx\\'" "[/\\\\][^/\\\\]+.trt\\'"))
 '(org-agenda-files '("~/org-note/Agenda.org"))
 '(package-selected-packages '(vterm paren-face))
 '(project-linux-architecture-default "x86")
 '(project-linux-build-directory-default 'same)
 '(speedbar-supported-extension-expressions
   '(".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".php" ".ctp" ".sql"))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 102 :width normal))))
 '(header-line ((t (:inherit mode-line :background "grey20" :foreground "grey90" :inverse-video nil :underline t))))
 '(message-view-patch-commit-comment ((t nil)))
 '(message-view-patch-commit-message ((t nil)))
 '(racket-xp-unused-face ((t nil))))
(set-background-color "black")
