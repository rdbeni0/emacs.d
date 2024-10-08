;;; cfg-php.el --- configfuration for php programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with PHP programming
;; 2021 year: https://www.reddit.com/r/emacs/comments/ms8nvc/emacs_php_mode/
;; https://github.com/emacs-php
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
  :config
  (setq psalm-level 'max)
  )

;; https://melpa.org/#/flycheck-psalm
(use-package flycheck-psalm
  :ensure t
  )

;; https://github.com/emacs-php/phpstan.el
(use-package phpstan
  :ensure t
  :after php-mode
  :config
  (setq phpstan-level 'max)
  )

;; https://melpa.org/#/flycheck-phpstan
(use-package flycheck-phpstan
  :ensure t
  )

;; https://github.com/nlamirault/phpunit.el
;; https://melpa.org/#/phpunit
(use-package phpunit
  :ensure t
  :after php-mode
  )


(defun cfg/-my-php-mode-setup ()
  "My PHP-mode hook - flycheck integration."
  (require 'flycheck-phpstan)
  (require 'flycheck-psalm)

  ;;
  ;; In any given buffer where Flycheck is enabled, only one checker may be run at a time.
  ;;
  ;; However, any number of checkers can be run in sequence:
  ;; (defun flycheck-add-next-checker checker next &optional append)
  ;;
  ;; In such a sequence, after the first checker has finished running and its errors have been reported, the next checker of the sequence runs and its errors are reported, etc. until there are no more checkers in the sequence. This sequence is called a checker chain.

  ;; e.g. run "psalm" after "php":
  (flycheck-add-next-checker 'php 'psalm)
  ;; etc:
  (flycheck-add-next-checker 'psalm 'php-phpmd)

  ;; Next may also be a cons cell (level . next-checker), where next-checker is a symbol denoting the syntax checker to run after checker, and level is an error level. The next-checker will then only be run if there is no current error whose level is more severe than level.
  ;; If level is t, then next-checker is run regardless of the current errors.
  ;;
  ;; Run 'phpstan only if 'php-phpmd produced no errors (only warnings and info diagnostics):
  (flycheck-add-next-checker 'php-phpmd '(warning . phpstan))

  ;; (add-to-list 'flycheck-disabled-checkers 'phpstan)
  ;; (flycheck-mode t) ; not required, should be enabled globally
  )

(add-hook 'php-mode-hook 'cfg/-my-php-mode-setup)


(use-package neon-mode
  :ensure t
  )



;; enable xref-find-references for php:
(require 'semantic/symref/grep)
(add-to-list 'semantic-symref-filepattern-alist '(php-mode "*.php"))

(provide 'cfg-php)
;;; cfg-php.el ends here
