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
;;
;;; Code:

(use-package lsp-bridge
  :config
  (global-lsp-bridge-mode)
  (setq lsp-bridge-php-lsp-server "phpactor"))

;; acm - Asynchronous Completion Menu


;; Manipulations with company-mode:
;; turn off AUTOcomplete, but keep it optional to manually triggering:
(setq company-idle-delay nil)

(provide 'cfg-op-lsp-bridge)
;;; cfg-op-lsp-bridge.el ends here
