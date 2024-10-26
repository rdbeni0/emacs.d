;;; cfg-gen-co-xref-ffap.el --- general.el for xref -*- lexical-binding: t -*-

;; ffap
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-ffap
 :major-modes list-gen-mode-ffap
 :prefix ","
 "."  '(:ignore t :which-key "ffap")
 ".."  '(ffap :which-key "act_ffap"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map)
 :major-modes '(perl-mode cperl-mode)
 :prefix ","
 ".." '(cfg/ffap :which-key "ffap_perl"))

;; xref
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-xref
 :major-modes list-gen-mode-xref
 :prefix ","

 ;; xref remapping with dumb-jump as a backend:
 "."  '(:ignore t :which-key "xref")
 ".d" '(xref-find-definitions :which-key "xref-def")
 ".h" '(xref-find-references :which-key "xref-ref")
 ".b" '(xref-go-back :which-key "xref-go-back")
 ".B" '(xref-go-forward :which-key "xref-go-forward")
 ".s" '(xref-find-apropos :which-key "xref-apropos"))

;; without prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-xref
 :major-modes list-gen-mode-xref

 "gd" '(xref-find-definitions :which-key "xref-def")
 "gh" '(xref-find-references :which-key "xref-ref")
 "gb" '(xref-go-back :which-key "xref-go-back")
 "gB" '(xref-go-forward :which-key "xref-go-forward")
 "gs" '(xref-find-apropos :which-key "xref-apropos")

 "g."  '(:ignore t :which-key "xref")
 "g.d" '(xref-find-definitions :which-key "xref-def")
 "g.h" '(xref-find-references :which-key "xref-ref")
 "g.b" '(xref-go-back :which-key "xref-go-back")
 "g.B" '(xref-go-forward :which-key "xref-go-forward")
 "g.s" '(xref-find-apropos :which-key "xref-apropos"))

(provide 'cfg-gen-co-xref-ffap)
;;; cfg-gen-co-xref-ffap.el ends here
