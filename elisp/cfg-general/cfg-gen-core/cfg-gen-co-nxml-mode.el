;;; cfg-gen-co-nxml-mode.el --- general.el for nxml-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'nxml-mode-map
 :major-modes 'nxml-mode
 :prefix ","
 ","  '(ffap :which-key "act_ffap")
 "v"  '(cfg/xml-xsd-validate :which-key "validate-xsd-xmllint"))

(provide 'cfg-gen-co-nxml-mode)
;;; cfg-gen-co-nxml-mode.el ends here
