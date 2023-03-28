;; general-json-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'nxml-mode-map
 :major-modes 'nxml-mode
 :prefix ","
 ","  '(ffap :which-key "act_ffap")
 "v"  '(cfg/xml-xsd-validate :which-key "validate-xsd-xmllint")
 "="  '(:ignore t :which-key "format")
 "==" '(xml-format-buffer :which-key "xmllint-format-buffer")
 "=]" '(xml-format-region :which-key "xmllint-format-region")
 "=r" '(format-all-region :which-key "format-all-region")
 "=b" '(format-all-buffer :which-key "format-all-buffer"))
