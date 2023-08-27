;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags:

;; with prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-ggtags
 :major-modes list-gen-mode-ggtags
 :prefix ","
 "/"  '(:ignore t :which-key "ggtags")
 "/f" '(ggtags-find-file  :which-key "ggt-find-file")
 "/S" '(ggtags-view-search-history :which-key "ggt-show-history")
 "/'" '(ggtags-grep :which-key "ggt-grep")
 "/r" '(ggtags-find-reference :which-key "ggt-find-reference")
 "/b" '(ggtags-find-reference :which-key "ggt-find-reference")
 "/R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 "/z" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "/d" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "/h" '(ggtags-find-definition :which-key "ggt-find-definition")
 "/L" '(ggtags-show-definition :which-key "ggt-show-definition")
 "/%" '(ggtags-query-replace :which-key "ggt-query-replace")
 "/m" '(ggtags-mode :which-key "ggtags-mode")
 "/g" '(ggtags-create-tags :which-key "ggt-create-tags")
 "/u" '(ggtags-update-tags :which-key "ggt-update-tags"))

;; without prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode
 "g/"  '(:ignore t :which-key "ggtags")
 "g/f" '(ggtags-find-file :which-key "ggt-find-file")
 "g/S" '(ggtags-view-search-history :which-key "ggt-show-shistory")
 "g/'" '(ggtags-grep :which-key "ggt-grep")
 "g/r" '(ggtags-find-reference :which-key "ggt-find-reference")
 "g/b" '(ggtags-find-reference :which-key "ggt-find-reference")
 "g/R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 "g/z" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "g/d" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "g/h" '(ggtags-find-definition :which-key "ggt-find-definition")
 "g/L" '(ggtags-show-definition :which-key "ggt-show-definition")
 "g/%" '(ggtags-query-replace :which-key "ggt-query-replace")
 "g/m" '(ggtags-mode :which-key "ggtags-mode")
 "g/g" '(ggtags-create-tags :which-key "ggt-create-tags")
 "g/u" '(ggtags-update-tags :which-key "ggt-update-tags"))

;; SPC as prefix

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; search

 "s/"  '(:ignore t :which-key "ggtags")
 "s/f" '(ggtags-find-file  :which-key "ggt-find-file")
 "s/S" '(ggtags-view-search-history :which-key "ggt-show-history")
 "s/'" '(ggtags-grep :which-key "ggt-grep")
 "s/r" '(ggtags-find-reference :which-key "ggt-find-reference")
 "s/b" '(ggtags-find-reference :which-key "ggt-find-reference")
 "s/R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 "s/z" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "s/d" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "s/h" '(ggtags-find-definition :which-key "ggt-find-definition")
 "s/L" '(ggtags-show-definition :which-key "ggt-show-definition")
 "s/%" '(ggtags-query-replace :which-key "ggt-query-replace")
 "s/m" '(ggtags-mode :which-key "ggtags-mode")
 "s/g" '(ggtags-create-tags :which-key "ggt-create-tags")
 "s/u" '(ggtags-update-tags :which-key "ggt-update-tags"))
