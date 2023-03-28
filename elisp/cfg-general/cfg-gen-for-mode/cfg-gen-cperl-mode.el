;; general-cperl-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map)
 :major-modes '(perl-mode cperl-mode)
 :prefix ","
 "j"  '(imenu :which-key "imenu")
 "!"  '(executable-interpret :which-key "exec-script")
 "o"  '(cperl-perldoc-at-point :which-key "perldoc-at-point")
 "O"  '(cperl-perldoc :which-key "cperl-perldoc")
 "t"  '(:ignore t :which-key "toggle")
 "te" '(cperl-toggle-electric :which-key "toggle-electric")
 "tf" '(flycheck-mode :which-key "toggle-flycheck")
 "d"  '(cperl-db :which-key "debugger")
 "q"  '(:ignore t :which-key "quotes")
 "qi" '(perl-quote-single :which-key "quote-single")
 "qo" '(perl-quote-double :which-key "quote-double")
 "m"  '(:ignore t :which-key "modules")
 "ma" '(cfg/ffap :which-key "ffap_perl")
 "mm" '(cfg/find-perl-module :which-key "find-perl-module")
 "n"  '(mark-defun :which-key "mark-sub")
 "=p" '(cfg/perltidy-format-buffer :which-key "perltidy-format-buffer")
 "=o" '(cfg/perltidy-format :which-key "perltidy-format")
 "=f" '(cfg/perltidy-format-function :which-key "perltidy-format-function"))
