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
 "=s" '(php-set-style :which-key "php-mode-set-style")
 "c"  '(:ignore t :which-key "composer")
 "cc" '(composer :which-key "composer")
 "cd" '(cfg/composer-require-dev :which-key "require-dev")
 "cg" '(cfg/composer-global :which-key "composer-global")
 "ci" '(composer-install :which-key "install")
 "cj" '(composer-find-json-file :which-key "find-json-file")
 "cl" '(composer-find-lock-file :which-key "find-lock-file")
 "cp" '(composer-dump-autoload :which-key "dump-autoload")
 "cr" '(composer-require :which-key "require")
 "cs" '(composer-run-script :which-key "run-script")
 "ct" '(composer-list-packages :which-key "list-packages")
 "cu" '(composer-update :which-key "update")
 "cv" '(composer-run-vendor-bin-command :which-key "run-vendor-bin-command"))

(provide 'cfg-gen-op-php-mode)
;;; cfg-gen-op-php-mode.el ends here
