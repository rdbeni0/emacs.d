;;; cfg-op-php-lsp.el --- configfuration for php programming, lsp -*- lexical-binding: t -*-
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
              (setq eglot-report-progress nil)
	      (setq eglot-events-buffer-size 0)
	      (setq eglot-autoshutdown t)
              (eglot-ensure))))

(provide 'cfg-op-php-lsp)
;;; cfg-op-php-lsp.el ends here
