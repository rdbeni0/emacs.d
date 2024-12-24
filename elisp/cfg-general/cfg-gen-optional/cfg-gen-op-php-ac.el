;;; cfg-gen-op-php-ac.el --- general.el for ac-php -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 :prefix ","
 "/d" '(ac-php-find-symbol-at-point :which-key "php-def-dwim") ;; dwim = do what i mean
 "d"  '(ac-php-find-symbol-at-point :which-key "php-def-dwim") ;; dwim = do what i mean
 "/b" '(ac-php-location-stack-back :which-key "php-back")
 "b"  '(ac-php-location-stack-back :which-key "php-back")
 "/P" '(ac-php-remake-tags-all :which-key "php-remake-tags-all")
 "/p" '(ac-php-remake-tags :which-key "php-remake-tags"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'php-mode-map
 :major-modes 'php-mode
 "gd" '(ac-php-find-symbol-at-point :which-key "php-def-dwim") ;; dwim = do what i mean
 "gb" '(ac-php-location-stack-back :which-key "php-back"))

(provide 'cfg-gen-op-php-ac)
;;; cfg-gen-op-php-ac.el ends here
