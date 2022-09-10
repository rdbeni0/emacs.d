;; general-flycheck:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode)
 :prefix ","
 "'"  '(:ignore t :which-key "flycheck")
 "'l" '(flycheck-list-errors :which-key "list-errors")
 "'," '(flycheck-display-error-at-point :which-key "display-error")
 "''" '(flycheck-mode :which-key "flycheck-mode")
 "''" '(flycheck-buffer :which-key "flycheck-buffer")
 "']" '(flycheck-next-error :which-key "next-error")
 "'[" '(flycheck-previous-error :which-key "previous-error")
 "'0" '(flycheck-first-error :which-key "first-error")
 "'v" '(flycheck-verify-setup :which-key "verify-setup")
 "'V" '(flycheck-verify-checker :which-key "verify-checker")
 "'s" '(flycheck-select-checker :which-key "select-checker")
 "'?" '(flycheck-describe-checker :which-key "describe-checker"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'flycheck-error-list-mode-map
 :major-modes 'flycheck-error-list-mode
 :prefix ","
 "s" '(flycheck-error-list-set-filter :which-key "set-filter")
 "r" '(flycheck-error-list-check-source :which-key "check-source")
 "S" '(flycheck-error-list-reset-filter :which-key "reset-filter")
 "x" '(flycheck-error-list-explain-error :which-key "explain-error")
 "]" '(flycheck-error-list-next-error :which-key "next-error")
 "[" '(flycheck-error-list-previous-error :which-key "previous-error"))
