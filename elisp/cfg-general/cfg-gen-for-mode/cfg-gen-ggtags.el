;; general-ggtags:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(ggtags-mode-map ggtags-global-mode-map sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map groovy-mode-map jenkinsfile-mode-map)
 :major-modes '(ggtags-mode ggtags-global-mode sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode groovy-mode jenkinsfile-mode)
 :prefix ","
 "."  '(:ignore t :which-key "ggtags")
 ".f" '(ggtags-find-file  :which-key "ggt-find-file")
 ".S" '(ggtags-view-search-history :which-key "ggt-show-history")
 ".r" '(ggtags-grep :which-key "ggt-grep")
 ".R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 ".A" '(ggtags-find-reference :which-key "ggt-find-reference")
 ".z" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 ".Z" '(ggtags-find-definition :which-key "ggt-find-definition")
 ".L" '(ggtags-show-definition :which-key "ggt-show-definition")
 ".%" '(ggtags-query-replace :which-key "ggt-query-replace")

 ;; xref remapping with dumb-jump as a backend:

 ".h" '(xref-find-definitions :which-key "dumb-jump-go")
 ".c" '(xref-pop-marker-stack :which-key "dumb-jump-back")
 ".H" '(xref-find-apropos :which-key "xref-apropos")
 ".C" '(xref-find-references :which-key "xref-find-ref")

 ;; without evil:

 ".m" '(ggtags-mode :which-key "ggtags-mode")
 ".g" '(ggtags-create-tags :which-key "ggt-create-tags")
 ".u" '(ggtags-update-tags :which-key "ggt-update-tags"))

 ;; without prefix:
 
 (general-define-key
  :states '(normal visual emacs)
  :keymaps '(ggtags-mode-map ggtags-global-mode-map sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map groovy-mode-map jenkinsfile-mode-map)
  :major-modes '(ggtags-mode ggtags-global-mode sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode groovy-mode jenkinsfile-mode)
  "gf" '(ggtags-find-file :which-key "ggt-find-file")
  "gS" '(ggtags-view-search-history :which-key "ggt-show-shistory")
  "gr" '(ggtags-grep :which-key "ggt-grep")
  "gR" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
  "gA" '(ggtags-find-reference :which-key "ggt-find-reference")
  "gz" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
  "gZ" '(ggtags-find-definition :which-key "ggt-find-definition")
  "gL" '(ggtags-show-definition :which-key "ggt-show-definition")
  "g%" '(ggtags-query-replace :which-key "ggt-query-replace")

  ;; dumb-jump:

  "gh" '(xref-find-definitions :which-key "dumb-jump-go")
  "gc" '(xref-pop-marker-stack :which-key "dumb-jump-back")
  "gH" '(xref-find-apropos :which-key "xref-apropos")
  "gC" '(xref-find-references :which-key "xref-find-ref"))
