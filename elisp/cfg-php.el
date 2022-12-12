;;; cfg-php.el --- configfuration for php programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with PHP programming
;; 2021 year: https://www.reddit.com/r/emacs/comments/ms8nvc/emacs_php_mode/
;;; Code:

(use-package php-mode
  :ensure t
  :init
  (add-hook 'php-mode-hook (lambda ()
			      (require 'company-php)
			      (ac-php-core-eldoc-setup)))
  )

;; https://github.com/OVYA/php-cs-fixer

(use-package php-cs-fixer
  :ensure t
  :after php-mode
  )

(provide 'cfg-php)
;;; cfg-php.el ends here
