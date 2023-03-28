;; general-sh-mode: shell-script-mode (for editing files)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map)
 :major-modes 'sh-mode
 :prefix ","
 "\\" '(sh-backslash-region :which-key "backslash-region")
 "#"  '(sh-set-shell :which-key "set-shell")
 "!"  '(executable-interpret :which-key "exec-script")
 "j"  '(imenu :which-key "imenu")
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
