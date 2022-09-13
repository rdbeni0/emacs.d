;; general-ggtags/helm-gtags:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode)
 :prefix ","
 "."  '(:ignore t :which-key "ggtags")
 ".." '(helm-gtags-find-pattern :which-key "find-pattern")
 ".f" '(ggtags-find-file  :which-key "find-file")
 ".F" '(helm-gtags-find-files :which-key "find-files")
 ".d" '(helm-gtags-dwim :which-key "gtags-dwim")
 ".D" '(ggtags-find-definition :which-key "find-definition")
 ".s" '(helm-gtags-select :which-key "gtags-select")
 ".S" '(ggtags-view-search-history :which-key "view-search-history")
 ".r" '(ggtags-grep :which-key "ggtags-grep")
 ".a" '(helm-gtags-find-rtag :which-key "find-refr")
 ".A" '(ggtags-find-reference :which-key "find-reference")
 ".t" '(helm-gtags-find-tag-from-here :which-key "find-tag-def-here")
 ".T" '(helm-gtags-find-tag :which-key "find-tag-def")
 ".b" '(ggtags-find-tag-regexp :which-key "find-tag-regexp")
 ".B" '(ggtags-find-tag-dwim :which-key "find-tag-dwim")
 ".l" '(helm-gtags-find-symbol :which-key "find-symbol")
 ".L" '(ggtags-show-definition :which-key "show-definition")
 ".%" '(ggtags-query-replace :which-key "query-replace")
 ".z" '(helm-gtags-parse-file :which-key "parse-file")

 ;; xref remapping with dumb-jump as a backend:

 ".h" '(xref-find-definitions :which-key "dumb-jump-go")
 ".c" '(xref-pop-marker-stack :which-key "dumb-jump-back")
 ".H" '(xref-find-apropos :which-key "xref-apropos")
 ".C" '(xref-find-references :which-key "xref-find-ref")

 ;; without evil:

 ".m" '(ggtags-mode :which-key "ggtags-mode")
 ".g" '(ggtags-create-tags :which-key "create-tags")
 ".u" '(ggtags-update-tags :which-key "update-tags")
 ".i" '(helm-gtags-tags-in-this-function :which-key "tags-in-func")

  ;;;; helm-gtags : navigation and stack

 ".]" '(helm-gtags-next-history :which-key "next-history")
 ".[" '(helm-gtags-previous-history :which-key "previous-history")
 ".}" 'helm-gtags-next-history
 ".{" 'helm-gtags-previous-history
 ".k"  '(:ignore t :which-key "stack, cache")
 ".ka" '(helm-gtags-clear-cache :which-key "clear-cache")
 ".kA" '(helm-gtags-clear-all-cache :which-key "clear-all-cache")
 ".kq" '(helm-gtags-resume :which-key "gtags-resume")
 ".k/" '(helm-gtags-show-stack :which-key "show-stack")
 ".k0" '(helm-gtags-clear-stack :which-key "clear-stack")
 ".k)" '(helm-gtags-clear-all-stacks :which-key "clear-all-stacks")
 ".k-" '(helm-gtags-pop-stack :which-key "pop-stack")
 ".k_" '(helm-gtags-pop-stack :which-key "pop-stack")
 ".k=" '(helm-gtags-push-stack :which-key "push-stack")
 ".k+" '(helm-gtags-push-stack :which-key "push-stack"))

(general-define-key
 :states '(normal visual emacs)
 ;; :keymaps '(helm-gtags-mode-map ggtags-mode-map ggtags-global-mode-map)
 ;; :major-modes '(helm-gtags-mode ggtags-mode ggtags-global-mode)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode)
 "g." '(helm-gtags-find-pattern :which-key "find-pattern")
 "gf" '(ggtags-find-file :which-key "ggtags-find-file")
 "gF" '(helm-gtags-find-files :which-key "helm-gtags-find-files")
 "gd" '(helm-gtags-dwim :which-key "gtags-dwim") ;; dwim = do what i mean
 "gD" '(ggtags-find-definition :which-key "find-definition")
 "gs" '(helm-gtags-select :which-key "gtags-select")
 "gS" '(ggtags-view-search-history :which-key "gtags-view-search-history")
 "gr" '(ggtags-grep :which-key "ggtags-grep")
 "ga" '(helm-gtags-find-rtag :which-key "find-reftag")
 "gA" '(ggtags-find-reference :which-key "find-reference")
 "gt" '(helm-gtags-find-tag-from-here :which-key "find-tag-def-here")
 "gT" '(helm-gtags-find-tag :which-key "find-tag-def")
 "gb" '(ggtags-find-tag-regexp :which-key "find-tag-regexp")
 "gB" '(ggtags-find-tag-dwim :which-key "find-tag-dwim")
 "gl" '(helm-gtags-find-symbol :which-key "find-symbol")
 "gL" '(ggtags-show-definition :which-key "ggtags-show-definition")
 "g%" '(ggtags-query-replace :which-key "ggtags-query-replace")
 "gz" '(helm-gtags-parse-file :which-key "parse-file")
 "gh" '(xref-find-definitions :which-key "dumb-jump-go")
 "gc" '(xref-pop-marker-stack :which-key "dumb-jump-back")
 "gH" '(xref-find-apropos :which-key "xref-apropos")
 "gC" '(xref-find-references :which-key "xref-find-ref"))
