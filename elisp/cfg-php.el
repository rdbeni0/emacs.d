;;; cfg-php.el --- configfuration for php programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with PHP programming
;; 2021 year: https://www.reddit.com/r/emacs/comments/ms8nvc/emacs_php_mode/
;;; Code:

(use-package php-mode
  :ensure t
  )

;; https://github.com/OVYA/php-cs-fixer
(use-package php-cs-fixer
  :ensure t
  :after php-mode
  :config
  (setq php-cs-fixer-rules-fixer-part-options '(
						"multiline_whitespace_before_semicolons"
						"concat_space"
						))
  )

;; https://melpa.org/#/psalm
(use-package psalm
  :ensure t
  :after php-mode
  )

;; https://melpa.org/#/flycheck-psalm
(use-package flycheck-psalm
  :ensure t
  :after php-mode
  :config
  (defun cfg/-my-php-mode-setup ()
    "My PHP-mode hook."
    (require 'flycheck-psalm)
    )
  (add-hook 'php-mode-hook 'cfg/-my-php-mode-setup))

;; https://github.com/nlamirault/phpunit.el
;; https://melpa.org/#/phpunit
(use-package phpunit
  :ensure t
  :after php-mode
  )


;; enable xref-find-references for php:
(require 'semantic/symref/grep)
(add-to-list 'semantic-symref-filepattern-alist '(php-mode "*.php"))

(provide 'cfg-php)
;;; cfg-php.el ends here
