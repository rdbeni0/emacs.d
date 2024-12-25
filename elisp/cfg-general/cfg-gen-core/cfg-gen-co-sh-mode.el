;;; cfg-gen-co-sh-mode.el --- general.el for sh-mode -*- lexical-binding: t -*-

;; "shell-script-mode is an alias for ‘sh-mode’ in ‘sh-script.el’."
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'sh-mode-map
 :major-modes 'sh-mode
 :prefix ","
 "b" '(sh-backslash-region :which-key "backslash-region")
 "#"  '(sh-set-shell :which-key "set-shell")
 "!"  '(executable-interpret :which-key "exec-script")
 "c"  '(:ignore t :which-key "code_templates")
 "ci" '(sh-if :which-key "if")
 "co" '(sh-for :which-key "for")
 "cc" '(sh-case :which-key "case")
 "cw" '(sh-while :which-key "while")
 "cf" '(sh-function :which-key "function")
 "cu" '(sh-until :which-key "until")
 "ce" '(sh-indexed-loop :which-key "indexed-loop")
 "cr" '(sh-repeat :which-key "repeat")
 "cs" '(sh-select :which-key "select")
 "cg" '(sh-while-getopts :which-key "while-getopts"))

(provide 'cfg-gen-co-sh-mode)
;;; cfg-gen-co-sh-mode.el ends here
