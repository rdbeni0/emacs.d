;; general-ggtags/helm-gtags:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(helm-gtags-mode-map ggtags-mode-map ggtags-global-mode-map sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map)
 :major-modes '(helm-gtags-mode ggtags-mode ggtags-global-mode sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode)
 :prefix ","
 "."  '(:ignore t :which-key "ggtags")
 ".." '(helm-gtags-find-pattern :which-key "hgt-find-pattern")
 ".f" '(ggtags-find-file  :which-key "ggt-find-file")
 ".F" '(helm-gtags-find-files :which-key "hgt-find-files")
 ".d" '(helm-gtags-dwim :which-key "hgt-dwim")
 ".D" '(helm-gtags-find-tag-other-window :which-key "hgt-oth-wind")
 ".s" '(helm-gtags-select :which-key "hgt-select")
 ".S" '(ggtags-view-search-history :which-key "ggt-show-history")
 ".r" '(ggtags-grep :which-key "ggt-grep")
 ".R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 ".a" '(helm-gtags-find-rtag :which-key "hgt-find-rtag")
 ".A" '(ggtags-find-reference :which-key "ggt-find-reference")
 ".b" '(helm-gtags-pop-stack :which-key "hgt-back")
 ".B" '(helm-gtags-show-stack :which-key "hgt-show-stack")
 ".t" '(helm-gtags-find-tag-from-here :which-key "hgt-find-tag-here")
 ".T" '(helm-gtags-find-tag :which-key "hgt-find-tag-def")
 ".z" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 ".Z" '(ggtags-find-definition :which-key "ggt-find-definition")
 ".l" '(helm-gtags-find-symbol :which-key "hgt-find-symbol")
 ".L" '(ggtags-show-definition :which-key "ggt-show-definition")
 ".%" '(ggtags-query-replace :which-key "ggt-query-replace")

 ;; helm-git-grep and xref remapping with dumb-jump as a backend:

 ".h" '(xref-find-definitions :which-key "dumb-jump-go")
 ".c" '(xref-pop-marker-stack :which-key "dumb-jump-back")
 ".H" '(xref-find-apropos :which-key "xref-apropos")
 ".C" '(xref-find-references :which-key "xref-find-ref")
 "./" '(cfg/helm-git-grep-at-point :which-key "helm-git-grep-at-point")
 ".'" '(cfg/helm-git-grep :which-key "helm-git-grep")

 ;; without evil:

 ".m" '(ggtags-mode :which-key "ggtags-mode")
 ".g" '(ggtags-create-tags :which-key "ggt-create-tags")
 ".u" '(ggtags-update-tags :which-key "ggt-update-tags")
 ".i" '(helm-gtags-tags-in-this-function :which-key "hgt-tags-in-func")
 ".e" '(helm-gtags-parse-file :which-key "hgt-parse-file")

  ;;;; helm-gtags : navigation and stack

 ".]" '(helm-gtags-next-history :which-key "hgt-next-hist")
 ".[" '(helm-gtags-previous-history :which-key "hgt-previous-hist")
 ".}" 'helm-gtags-next-history
 ".{" 'helm-gtags-previous-history
 ".k"  '(:ignore t :which-key "stack, cache")
 ".kk" '(helm-gtags-show-stack :which-key "hgt-show-stack")
 ".ka" '(helm-gtags-clear-cache :which-key "hgt-clear-cache")
 ".kA" '(helm-gtags-clear-all-cache :which-key "hgt-clear-all-cache")
 ".kq" '(helm-gtags-resume :which-key "hgt-gtags-resume")
 ".k0" '(helm-gtags-clear-stack :which-key "hgt-clear-stack")
 ".k)" '(helm-gtags-clear-all-stacks :which-key "hgt-clear-all-stacks")
 ".k-" '(helm-gtags-pop-stack :which-key "hgt-pop-stack")
 ".k_" '(helm-gtags-pop-stack :which-key "hgt-pop-stack")
 ".k=" '(helm-gtags-push-stack :which-key "hgt-push-stack")
 ".k+" '(helm-gtags-push-stack :which-key "hgt-push-stack"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(helm-gtags-mode-map ggtags-mode-map ggtags-global-mode-map sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map)
 :major-modes '(helm-gtags-mode ggtags-mode ggtags-global-mode sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode)
 "g." '(helm-gtags-find-pattern :which-key "hgt-find-pattern")
 "gf" '(ggtags-find-file :which-key "ggt-find-file")
 "gF" '(helm-gtags-find-files :which-key "hgt-find-files")
 "gd" '(helm-gtags-dwim :which-key "hgt-dwim") ;; dwim = do what i mean
 "gD" '(helm-gtags-find-tag-other-window :which-key "hgt-oth-wind") ;; other window
 "gs" '(helm-gtags-select :which-key "hgt-select")
 "gS" '(ggtags-view-search-history :which-key "ggt-show-shistory")
 "gr" '(ggtags-grep :which-key "ggt-grep")
 "gR" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 "ga" '(helm-gtags-find-rtag :which-key "hgt-find-rtag")
 "gA" '(ggtags-find-reference :which-key "ggt-find-reference")
 "gb" '(helm-gtags-pop-stack :which-key "hgt-back")
 "gB" '(helm-gtags-show-stack :which-key "hgt-show-stack")
 "gt" '(helm-gtags-find-tag-from-here :which-key "hgt-find-tag-here")
 "gT" '(helm-gtags-find-tag :which-key "hgt-find-tag-def")
 "gz" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "gZ" '(ggtags-find-definition :which-key "ggt-find-definition")
 "gl" '(helm-gtags-find-symbol :which-key "hgt-find-symbol")
 "gL" '(ggtags-show-definition :which-key "ggt-show-definition")
 "g%" '(ggtags-query-replace :which-key "ggt-query-replace")

 ;; dumb-jump:

 "gh" '(xref-find-definitions :which-key "dumb-jump-go")
 "gc" '(xref-pop-marker-stack :which-key "dumb-jump-back")
 "gH" '(xref-find-apropos :which-key "xref-apropos")
 "gC" '(xref-find-references :which-key "xref-find-ref"))
