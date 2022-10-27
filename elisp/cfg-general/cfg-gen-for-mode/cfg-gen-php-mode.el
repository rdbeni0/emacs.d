;; general-php-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 :prefix ","
 "j"  '(cfg/helm-jump-in-buffer :which-key "helm-jump-in-buffer")
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 "=b" '(format-all-buffer :which-key "format-all-buffer")
 "=o" '(cfg/php-custom-file-format :which-key "php-custom-format")
 "=c" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=f" '(php-cs-fixer-fix :which-key "php-cs-fixer")
 "=s" '(php-set-style :which-key "php-mode-set-style"))
