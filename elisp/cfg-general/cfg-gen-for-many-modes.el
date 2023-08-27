(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure lists - if you want to add any mode, do it here:
;; https://www.emacswiki.org/emacs/ListModification
;; to add - use append, to remove: seq-difference:

;; common:

(setq list-gen-mode '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode ssh-config-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode c++-mode nxml-mode groovy-mode jenkinsfile-mode))

(setq list-gen-mode-map '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map c++-mode-map nxml-mode-map groovy-mode-map jenkinsfile-mode-map))

(delete-dups list-gen-mode) ;; remove duplicates
(delete-dups list-gen-mode-map)

;; ggtags:

(setq list-gen-mode-map-ggtags (append list-gen-mode-map '(ggtags-mode-map ggtags-global-mode-map)))
(setq list-gen-mode-ggtags (append list-gen-mode '(ggtags-mode ggtags-global-mode)))

;; flycheck:

(setq list-gen-mode-flycheck (seq-difference list-gen-mode '(ssh-config-mode jenkinsfile-mode fish-mode)))
(setq list-gen-mode-map-flycheck (seq-difference list-gen-mode-map '(ssh-config-mode-map jenkinsfile-mode-map fish-mode-map)))

;; format

(setq list-gen-mode-format (seq-difference list-gen-mode '(ssh-config-mode)))
(setq list-gen-mode-map-format (seq-difference list-gen-mode-map '(ssh-config-mode-map)))

(setq list-gen-mode-format (append list-gen-mode '(json-mode markdown-mode)))
(setq list-gen-mode-map-format (append list-gen-mode-map '(json-mode-map markdown-mode-map)))

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
 "kt" '(comment-kill :which-key "comment-kill")

 ;; xref remapping with dumb-jump as a backend:
 "."  '(:ignore t :which-key "xref")
 ".b" '(xref-find-references :which-key "xref-ref")
 ".h" '(xref-find-definitions :which-key "xref-def")
 ".c" '(xref-pop-marker-stack :which-key "xref-jump-back")
 ".s" '(xref-find-apropos :which-key "xref-apropos")
 )

;; without prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode

 "gb" '(xref-find-references :which-key "xref-ref")
 "gh" '(xref-find-definitions :which-key "xref-def")
 "gc" '(xref-pop-marker-stack :which-key "xref-jump-back")
 "gs" '(xref-find-apropos :which-key "xref-apropos")
 "g."  '(:ignore t :which-key "xref")
 "g.b" '(xref-find-references :which-key "xref-ref")
 "g.h" '(xref-find-definitions :which-key "xref-def")
 "g.c" '(xref-pop-marker-stack :which-key "xref-jump-back")
 "g.s" '(xref-find-apropos :which-key "xref-apropos")
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F-keys (also in insert mode) - example:

;; (general-define-key
;;  :states '(normal visual emacs insert)
;;  :keymaps list-gen-mode-map
;;  :major-modes list-gen-mode
;;  "<f5>"     '(:ignore t :which-key "completions")
;;  "<f5><f4>" '(completion-at-point :which-key "completion-at-point-capf")
;;  )
