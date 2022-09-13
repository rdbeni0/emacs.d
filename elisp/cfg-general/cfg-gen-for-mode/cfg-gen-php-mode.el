;; general-php-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(cfg/php-custom-file-format :which-key "php-custom-file-format")
 "=b" '(format-all-buffer :which-key "format-all-buffer")
 "=c" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=f" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=s" '(php-set-style :which-key "php-mode-set-style"))
