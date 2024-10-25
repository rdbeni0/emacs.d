;;; cfg-gen-co-xref-ffap.el --- general.el for xref -*- lexical-binding: t -*-

;; ffap
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode
 :prefix ","
 ","  '(ffap :which-key "act_ffap"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode
 :prefix ","

 ;; xref remapping with dumb-jump as a backend:
 "."  '(:ignore t :which-key "xref")
 ".r" '(xref-find-references :which-key "xref-ref")
 ".d" '(xref-find-definitions :which-key "xref-def")
 ".h" '(xref-find-definitions :which-key "xref-def")
 ".b" '(xref-go-back :which-key "xref-go-back")
 ".B" '(xref-go-forward :which-key "xref-go-forward")
 ".s" '(xref-find-apropos :which-key "xref-apropos"))

;; without prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map
 :major-modes list-gen-mode

 "gr" '(xref-find-references :which-key "xref-ref")
 "gd" '(xref-find-definitions :which-key "xref-def")
 "gh" '(xref-find-definitions :which-key "xref-def")
 "gb" '(xref-go-back :which-key "xref-go-back")
 "gB" '(xref-go-forward :which-key "xref-go-forward")
 "gs" '(xref-find-apropos :which-key "xref-apropos")

 "g."  '(:ignore t :which-key "xref")
 "g.r" '(xref-find-references :which-key "xref-ref")
 "g.d" '(xref-find-definitions :which-key "xref-def")
 "g.h" '(xref-find-definitions :which-key "xref-def")
 "g.b" '(xref-go-back :which-key "xref-go-back")
 "g.B" '(xref-go-forward :which-key "xref-go-forward")
 "g.s" '(xref-find-apropos :which-key "xref-apropos"))

(provide 'cfg-gen-co-xref-ffap)
;;; cfg-gen-co-xref-ffap.el ends here
