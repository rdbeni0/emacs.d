;;; cfg-op-company-php.el --- configfuration for company-mode and PHP -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for company-php and ac: https://github.com/xcwen/ac-php
;; Which can be used as a lighter alternative to lsp-mode with php.
;;; Code:
;;

(when (require 'php-mode nil 'noerror)
  (progn
    (add-hook 'php-mode-hook (lambda ()
			       (set (make-local-variable 'company-backends) '())
			       ;; company-capf, company-gtags
			       (if (require 'php-mode nil 'noerror)
				   (add-to-list 'company-backends '(company-abbrev :separate company-ac-php-backend company-keywords company-dabbrev-code company-files company-dabbrev))
				 (add-to-list 'company-backends '(company-abbrev :separate company-keywords company-dabbrev-code company-files company-dabbrev)))
			       ))

    (use-package company-php
      :after company
      :ensure t
      :config
      ;; ac-php uses its own tags format. By default all tags located at ~/.ac-php/tags-<project-directory>. For example, if the real path of the project is /home/jim/ac-php/phptest, then tags will be placed at ~/.ac-php/tags-home-jim-ac-php-phptest/.
      ;; And you can redefine the base path (~/.ac-php) using ac-php-tags-path variable:
      (setq ac-php-tags-path (expand-file-name ".cache/.ac-php" user-emacs-directory))
      (add-hook 'php-mode-hook (lambda ()
				 (require 'company-php)
				 (ac-php-core-eldoc-setup))))
    ))



(provide 'cfg-op-company-php)
;;; cfg-op-company-php.el ends here
