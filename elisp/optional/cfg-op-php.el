;;; cfg-op-php.el --- configfuration for php programming -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with PHP programming
;; 2021 year: https://www.reddit.com/r/emacs/comments/ms8nvc/emacs_php_mode/
;; https://github.com/emacs-php
;;
;;; Code:

(use-package php-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-php-mode))

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
  :config
  (setq psalm-level 'max)
  )


;; https://github.com/emacs-php/phpstan.el
(use-package phpstan
  :ensure t
  :after php-mode
  :config
  (setq phpstan-level 'max)
  )

;; https://github.com/nlamirault/phpunit.el
;; https://melpa.org/#/phpunit
(use-package phpunit
  :ensure t
  :after php-mode
  )

(use-package neon-mode
  :ensure t
  )

;; enable xref-find-references for php:
(require 'semantic/symref/grep)
(add-to-list 'semantic-symref-filepattern-alist '(php-mode "*.php"))

(provide 'cfg-op-php)
;;; cfg-op-php.el ends here
