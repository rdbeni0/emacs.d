;;; cfg-gen-op-flycheck-mode.el ---  general.el for flycheck -*- lexical-binding: t -*-
;; general-flycheck:

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-flycheck
 :major-modes list-gen-mode-flycheck
 :prefix ","
 "'"  '(:ignore t :which-key "flycheck")
 "'`" '(flycheck-buffer :which-key "flycheck-buffer")
 "'," '(flycheck-display-error-at-point :which-key "show-error-at-point")
 "'." '(flycheck-copy-errors-as-kill :which-key "copy-error-at-point")
 "'0" '(flycheck-first-error :which-key "first-error")
 "' <down>" '(flycheck-next-error :which-key "next-error")
 "' <up>" '(flycheck-previous-error :which-key "previous-error")
 "'?" '(flycheck-describe-checker :which-key "describe-checker")
 "'V" '(flycheck-verify-checker :which-key "verify-checker")
 "'[" '(flycheck-previous-error :which-key "previous-error")
 "']" '(flycheck-next-error :which-key "next-error")
 "'l" '(flycheck-list-errors :which-key "list-errors")
 "'d" '(flycheck-disable-checker :which-key "disable-checker")
 "'e" '(cfg/flycheck-enable-checker :which-key "enable-checker")
 "'f" '(flycheck-eglot-mode :which-key "toggle-flycheck-eglot")
 "'j" '(flycheck-next-error :which-key "next-error")
 "'k" '(flycheck-previous-error :which-key "previous-error")
 "''" '(cfg/flycheck-list-errors-below :which-key "list-errors-below")
 "'m" '(flycheck-mode :which-key "flycheck-mode")
 "'s" '(flycheck-select-checker :which-key "select-checker")
 "'t" '(flycheck-mode :which-key "toggle-flycheck")
 "'v" '(flycheck-verify-setup :which-key "verify-setup"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'flycheck-error-list-mode-map
 :major-modes 'flycheck-error-list-mode
 :prefix ","
 "<down>" '(flycheck-error-list-next-error :which-key "next-error")
 "<up>" '(flycheck-error-list-previous-error :which-key "previous-error")
 "Q" '(kill-buffer :which-key "kill-this-buffer")
 "S" '(flycheck-error-list-reset-filter :which-key "unfilter")
 "[" '(flycheck-error-list-previous-error :which-key "previous-error")
 "]" '(flycheck-error-list-next-error :which-key "next-error")
 "j" '(flycheck-error-list-next-error :which-key "next-error")
 "k" '(flycheck-error-list-previous-error :which-key "previous-error")
 "q" '(kill-buffer-and-window :which-key "quit")
 "r" '(flycheck-error-list-check-source :which-key "check-source")
 "s" '(flycheck-error-list-set-filter :which-key "set-filter")
 "u" '(flycheck-error-list-reset-filter :which-key "unfilter")
 "x" '(flycheck-error-list-explain-error :which-key "explain-error"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map)
 :major-modes '(perl-mode cperl-mode)
 :prefix ","
 "tf" '(flycheck-mode :which-key "toggle-flycheck"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'flycheck-error-list-mode-map
 :major-modes 'flycheck-error-list-mode
 "q" 'kill-buffer-and-window
 "j" 'flycheck-error-list-next-error
 "k" 'flycheck-error-list-previous-error)

;; global mode
;; no space + no which-key
(general-define-key
 :states '(normal visual emacs insert)
 :keymaps list-gen-mode-map-flycheck
 :major-modes list-gen-mode-flycheck
 "M-n" 'flycheck-previous-error
 "M-p" 'flycheck-next-error)

(provide 'cfg-gen-op-flycheck-mode)
;;; cfg-gen-op-flycheck-mode.el ends here
