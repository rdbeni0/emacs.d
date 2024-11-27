;;; cfg-gen-co-txt-manipulations.el --- general.el for various text manipulations -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-txtman
 :major-modes list-gen-mode-txtman
 :prefix ","
 "."  '(:ignore t :which-key "txt-man")
 ".s"  '(sort-lines :which-key "sort-lines")
 ".T"  '(cfg/toggle-case-active :which-key "toggle-case-active")
 ".t"  '(cfg/toggle-case :which-key "toggle-case")
 ".c"  '(cfg/capitalize-region :which-key "capitalize")
 ".d"  '(cfg/downcase-region :which-key "downcase"))

(provide 'cfg-gen-co-txt-manipulations)
;;; cfg-gen-co-txt-manipulations.el ends here
