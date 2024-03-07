;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags:

;; ggtags - list:
;; for more info see "cfg-gen-for-many-modes.el"

(setq list-gen-mode-map-ggtags (append list-gen-mode-map '(ggtags-mode-map ggtags-global-mode-map)))
(setq list-gen-mode-ggtags (append list-gen-mode '(ggtags-mode ggtags-global-mode)))


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
 "/b" '(xref-go-back :which-key "ggt-go-back")
 "/B" '(xref-go-forward :which-key "ggt-go-forward")
 "/R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
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
 "test-thing/face" '(ggtags-find-file :which-key "ggt-find-file")
 "g/S" '(ggtags-view-search-history :which-key "ggt-show-shistory")
 "g/'" '(ggtags-grep :which-key "ggt-grep")
 "g/r" '(ggtags-find-reference :which-key "ggt-find-reference")
 "g/b" '(xref-go-back :which-key "ggt-go-back")
 "g/B" '(xref-go-forward :which-key "ggt-go-forward")
 "g/R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 "g/d" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "g/h" '(ggtags-find-definition :which-key "ggt-find-definition")
 "g/L" '(ggtags-show-definition :which-key "ggt-show-definition")
 "g/%" '(ggtags-query-replace :which-key "ggt-query-replace")
 "g/m" '(ggtags-mode :which-key "ggtags-mode")
 "g/g" '(ggtags-create-tags :which-key "ggt-create-tags")
 "g/u" '(ggtags-update-tags :which-key "ggt-update-tags")

 ;; override
 "gd" '(xref-find-definitions :which-key "xref-def")
 "gr" '(xref-find-references :which-key "xref-ref"))

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
 "s/b" '(xref-go-back :which-key "ggt-go-back")
 "s/B" '(xref-go-forward :which-key "ggt-go-forward")
 "s/R" '(ggtags-find-tag-regexp :which-key "ggt-find-tag-regexp")
 "s/d" '(ggtags-find-tag-dwim :which-key "ggt-find-tag-dwim")
 "s/h" '(ggtags-find-definition :which-key "ggt-find-definition")
 "s/L" '(ggtags-show-definition :which-key "ggt-show-definition")
 "s/%" '(ggtags-query-replace :which-key "ggt-query-replace")
 "s/m" '(ggtags-mode :which-key "ggtags-mode")
 "s/g" '(ggtags-create-tags :which-key "ggt-create-tags")
 "s/u" '(ggtags-update-tags :which-key "ggt-update-tags"))

(provide 'cfg-gen-ggtags)
;;; cfg-gen-ggtags.el ends here
