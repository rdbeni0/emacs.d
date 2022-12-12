;; general-flycheck:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode)
 :prefix ","
 "1"  '(:ignore t :which-key "flycheck")
 "1l" '(flycheck-list-errors :which-key "list-errors")
 "1," '(flycheck-display-error-at-point :which-key "display-error")
 "1'" '(flycheck-mode :which-key "flycheck-mode")
 "1'" '(flycheck-buffer :which-key "flycheck-buffer")
 "1]" '(flycheck-next-error :which-key "next-error")
 "1[" '(flycheck-previous-error :which-key "previous-error")
 "10" '(flycheck-first-error :which-key "first-error")
 "1v" '(flycheck-verify-setup :which-key "verify-setup")
 "1V" '(flycheck-verify-checker :which-key "verify-checker")
 "1s" '(flycheck-select-checker :which-key "select-checker")
 "1?" '(flycheck-describe-checker :which-key "describe-checker"))

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
