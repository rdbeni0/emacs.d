;;; cfg-gen-op-php-mode.el --- general.el for php-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 :prefix ","
 "t"  '(:ignore t :which-key "phpunit")
 "tt" '(phpunit-current-test :which-key "phpunit-current-test")
 "tc" '(phpunit-current-class :which-key "phpunit-current-class")
 "tp" '(phpunit-current-project :which-key "phpunit-current-project")
 "tg" '(phpunit-group :which-key "phpunit-group")
 "="  '(:ignore t :which-key "format")
 "=o" '(cfg/php-custom-file-format :which-key "php-custom-format")
 "=c" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=f" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=s" '(php-set-style :which-key "php-mode-set-style"))

(provide 'cfg-gen-op-php-mode)
;;; cfg-gen-op-php-mode.el ends here
