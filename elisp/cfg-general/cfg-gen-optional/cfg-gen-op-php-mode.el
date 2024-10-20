;;; cfg-gen-op-php-mode.el --- general.el for php-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 :prefix ","
 "j"  '(imenu :which-key "imenu")
 "t"  '(:ignore t :which-key "phpunit")
 "tt" '(phpunit-current-test :which-key "phpunit-current-test")
 "tc" '(phpunit-current-class :which-key "phpunit-current-class")
 "tp" '(phpunit-current-project :which-key "phpunit-current-project")
 "tg" '(phpunit-group :which-key "phpunit-group")
 "="  '(:ignore t :which-key "format")
 "=o" '(cfg/php-custom-file-format :which-key "php-custom-format")
 "=c" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=f" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=s" '(php-set-style :which-key "php-mode-set-style")
 ".d" '(ac-php-find-symbol-at-point :which-key "php-def-dwim") ;; dwim = do what i mean
 "d"  '(ac-php-find-symbol-at-point :which-key "php-def-dwim") ;; dwim = do what i mean
 ".b" '(ac-php-location-stack-back :which-key "php-back")
 "b"  '(ac-php-location-stack-back :which-key "php-back")
 ".P" '(ac-php-remake-tags-all :which-key "php-remake-tags-all")
 ".p" '(ac-php-remake-tags :which-key "php-remake-tags"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 "gd" '(ac-php-find-symbol-at-point :which-key "php-def-dwim") ;; dwim = do what i mean
 "gb" '(ac-php-location-stack-back :which-key "php-back"))

(provide 'cfg-gen-op-php-mode)
;;; cfg-gen-op-php-mode.el ends here
