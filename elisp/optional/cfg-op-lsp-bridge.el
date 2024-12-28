;;; cfg-op-php-lsp-bridge.el --- configfuration for lsp-bridge.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/manateelazycat/lsp-bridge
;; Experimental package. Installation:
;; When using lsp-bridge, please first disable other completion plugins, such as lsp-mode, eglot, company, corfu, etc.
;; lsp-bridge provides a complete solution from the completion backend, completion frontend to multi-backend integration
;;
;; installation via NixOS:
;; https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=lsp-bridge
;; https://github.com/manateelazycat/lsp-bridge/wiki/NixOS
;;
;; https://www.reddit.com/r/emacs/comments/vhqewj/lspbridge_is_good/
;;
;; acm - Asynchronous Completion Menu
;;; Code:

(use-package lsp-bridge
  :config
  (global-lsp-bridge-mode)
  (setq lsp-bridge-php-lsp-server "phpactor"))

;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#customize-language-server-configuration-file
;; A little tip, if you use a direnv setup (e.g. with nix-shell), make sure to use a hook like this (this is for the envrc package):
(add-hook 'envrc-mode-hook 'lsp-bridge-restart-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Manipulations with company-mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn off AUTOcomplete in company, but keep it optional to manually triggering:
(setq company-idle-delay nil)

(provide 'cfg-op-lsp-bridge)
;;; cfg-op-lsp-bridge.el ends here
