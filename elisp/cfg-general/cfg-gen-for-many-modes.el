;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure lists - if you want to add any mode, do it here:
;; https://www.emacswiki.org/emacs/ListModification

;; common:

(setq list-gen-mode-map '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map c++-mode-map nxml-mode-map groovy-mode-map jenkinsfile-mode-map))

(setq list-gen-mode '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode ssh-config-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode c++-mode nxml-mode groovy-mode jenkinsfile-mode))

(delete-dups list-gen-mode) ;; remove duplicates
(delete-dups list-gen-mode-map)

;; ggtags and dumb-jump:

(setq list-gen-mode-map-ggtags (append list-gen-mode-map '(ggtags-mode-map ggtags-global-mode-map)))
(setq list-gen-mode-ggtags (append list-gen-mode '(ggtags-mode ggtags-global-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comments:

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode
 :prefix ","
 ","  '(ffap :which-key "act_ffap")
 "k"  '(:ignore t :which-key "comments")
 "kl" '(comment-dwim :which-key "comment-dwim")
 "kk" '(mark-defun :which-key "mark-fun")
 "km" '(comment-region :which-key "comment-region")
 "kn" '(uncomment-region :which-key "uncomment-region")
 "kj" '(comment-line :which-key "comment-line")
 "kt" '(comment-kill :which-key "comment-kill"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags and dumb-jump:

;; with prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-ggtags
 :major-modes list-gen-mode-ggtags
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
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F-keys (also in insert mode) - example:

;; (general-define-key
;;  :states '(normal visual emacs insert)
;;  :keymaps list-gen-mode-map
;;  :major-modes list-gen-mode
;;  "<f5>"     '(:ignore t :which-key "completions")
;;  "<f5><f4>" '(completion-at-point :which-key "completion-at-point-capf")
;;  )
