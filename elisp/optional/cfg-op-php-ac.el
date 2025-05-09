;;; cfg-op-php-ac.el --- configfuration for ac-php -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for company-php and ac: https://github.com/xcwen/ac-php
;; Which can be used as a lighter alternative to lsp-mode/eglot with php.
;; Enable this module only if you don't want to use lsp & php (for various reasons).
;;
;;; Code:
;;

(when (require 'php-mode nil 'noerror)
  (progn
    (use-package company-php
      :after company
      :ensure t
      :config
      ;; ac-php uses its own tags format. By default all tags located at ~/.ac-php/tags-<project-directory>.
      ;; For example, if the real path of the project is /home/jim/ac-php/phptest, then tags will be placed at ~/.ac-php/tags-home-jim-ac-php-phptest/.
      ;; And you can redefine the base path (~/.ac-php) using ac-php-tags-path variable:
      (setq ac-php-tags-path (expand-file-name ".cache/.ac-php" user-emacs-directory))
      (add-hook 'php-mode-hook (lambda ()
				 (require 'company-php)
				 (ac-php-core-eldoc-setup)))

      ;; enable xref-find-references for php:
      ;; https://github.com/xcwen/ac-php/issues/75
      (require 'semantic/symref/grep)
      (add-to-list 'semantic-symref-filepattern-alist '(php-mode "*.php"))
      ;; load general.el and keybindings:
      (require 'cfg-gen-op-php-ac)
      )))

(provide 'cfg-op-php-ac)
;;; cfg-op-php-ac.el ends here
