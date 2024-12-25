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

;; https://github.com/Fuco1/neon-mode
(use-package neon-mode
  :ensure t
  )

;;;; php
;; https://melpa.org/#/flycheck-phpstan
(use-package flycheck-phpstan
  :ensure t
  )

;; https://melpa.org/#/flycheck-psalm
(use-package flycheck-psalm
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacs-php/php-ts-mode

(if (require 'php-ts-mode nil 'noerror)
    (message "php-ts-mode is already installed")
  (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cfg/-my-php-mode-setup ()
  "My PHP-mode hook - integration with flycheck."
  (require 'flycheck-phpstan)
  (require 'flycheck-psalm)

  ;; Disable checkers above 5000 errors:
  ;; (setq-local flycheck-checker-error-threshold 5000)

  ;;
  ;; In any given buffer where Flycheck is enabled, only one checker may be run at a time.
  ;;
  ;; However, any number of checkers can be run in sequence:
  ;; (defun flycheck-add-next-checker checker next &optional append)
  ;;
  ;; In such a sequence, after the first checker has finished running and its errors have been reported, the next checker of the sequence runs and its errors are reported, etc. until there are no more checkers in the sequence. This sequence is called a "checker chain".

  ;; e.g. run "psalm" after "php":
  (flycheck-add-next-checker 'php 'psalm)
  ;; etc:
  (flycheck-add-next-checker 'psalm 'php-phpmd)

  ;; Next may also be a cons cell (level . next-checker), where next-checker is a symbol denoting the syntax checker to run after checker, and level is an error level. The next-checker will then only be run if there is no current error whose level is more severe than level.
  ;; If level is t, then next-checker is run regardless of the current errors.
  ;;
  ;; Run 'phpstan only if 'php-phpmd produced no errors (only warnings and info diagnostics):
  (flycheck-add-next-checker 'php-phpmd '(warning . phpstan))
  ;; (flycheck-add-next-checker 'phpstan 'eglot-check)

  ;; (add-to-list 'flycheck-disabled-checkers 'phpstan)
  ;; (flycheck-mode t) ; not required, should be enabled globally

  ;; and finally - select the first checked from the list:
  (flycheck-select-checker 'php)
  )

(add-hook 'php-mode-hook 'cfg/-my-php-mode-setup)

(provide 'cfg-op-php)
;;; cfg-op-php.el ends here
