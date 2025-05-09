;;; cfg-op-php-eglot.el --- configfuration for php programming via EGLOT -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/phpactor/phpactor
;; https://github.com/emacs-php/php-mode/wiki/LSP-Support
;;
;;; Code:

;; Please be aware that phpactor will create and use: ~/.cache/phpactor
(when (require 'php-mode nil 'noerror)
  (add-hook 'php-mode-hook
            (lambda ()
	      (setq flycheck-eglot-exclusive nil)
	      (add-to-list 'eglot-stay-out-of 'company)
	      ;; Should be menaged via "flycheck-eglot-mode":
	      ;; (add-to-list 'eglot-stay-out-of 'flycheck)
              (eglot-ensure)
	      )))

(provide 'cfg-op-php-eglot)
;;; cfg-op-php-eglot.el ends here
