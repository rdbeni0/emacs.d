;;; cfg-gen-op-python-anaconda-mode.el --- general.el for anaconda-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 :prefix ","
 "a"  '(:ignore t :which-key "anaconda-mode")
 "ab" '(xref-go-back :which-key "xref-go-back")
 "aB" '(xref-go-forward :which-key "xref-go-forward")
 "ae" '(cfg/enable-anaconda-mode :which-key "enable-anaconda-mode")
 "am" '(anaconda-mode :which-key "anaconda-mode")
 "aM" '(anaconda-eldoc-mode :which-key "anaconda-eldoc-mode"))

;; use below only if anaconda-mode is enabled:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'anaconda-mode-map
 :major-modes 'anaconda-mode
 :prefix ","
 "a?" '(anaconda-mode-show-doc :which-key "anaconda-mode-show-doc")
 "aa" '(anaconda-mode-find-assignments :which-key "find-assignments")
 "aA" '(anaconda-mode-find-assignments-other-window :which-key "find-assignments-ot-window")
 "ar" '(anaconda-mode-find-references :which-key "find-references")
 "aR" '(anaconda-mode-find-references-other-window :which-key "find-references-ot-window")
 "ad" '(anaconda-mode-find-definitions :which-key "find-definitions")
 "aD" '(anaconda-mode-find-definitions-other-window :which-key "find-definitions-ot-window"))

;; anaconda-mode - no prefix, it will override evil keymaps:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'anaconda-mode-map
 :major-modes 'anaconda-mode
 "g?" '(anaconda-mode-show-doc :which-key "show-doc")
 "ga" '(anaconda-mode-find-assignments :which-key "find-assignments")
 "gA" '(anaconda-mode-find-assignments-other-window :which-key "find-assignments-ot-window")
 "gr" '(anaconda-mode-find-references :which-key "find-references")
 "gR" '(anaconda-mode-find-references-other-window :which-key "find-references-ot-window")
 "gd" '(anaconda-mode-find-definitions :which-key "find-definitions")
 "gD" '(anaconda-mode-find-definitions-other-window :which-key "find-definitions-ot-window"))

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "bmP" '(anaconda-mode :which-key "anaconda-mode"))

(provide 'cfg-gen-op-python-anaconda-mode)
;;; cfg-gen-op-python-anaconda-mode.el ends here
