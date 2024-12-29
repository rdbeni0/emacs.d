;;; cfg-op-php-lsp-bridge.el --- configfuration for lsp-bridge -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/manateelazycat/lsp-bridge
;; Experimental package.
;; Installation on emacs:
;; When using lsp-bridge, please first disable other completion plugins, such as lsp-mode, eglot, company, corfu, etc.
;; lsp-bridge provides a complete solution from the completion backend, completion frontend to multi-backend integration.
;;
;; Installation via NixOS:
;; https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=lsp-bridge
;; https://github.com/manateelazycat/lsp-bridge/wiki/NixOS
;;
;; https://www.reddit.com/r/emacs/comments/vhqewj/lspbridge_is_good/
;;
;; "acm" - Asynchronous Completion Menu
;;; Code:

;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#lsp-server-options
;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#options

(use-package lsp-bridge
  :config
  ;; use in global-mode, but disable per particular mode:
  (global-lsp-bridge-mode)

  ;; popup menu seems to be broken, better to use standard completion at the bottom:
  (setq lsp-bridge-code-action-enable-popup-menu nil)

  ;; multiserver - custom user config and json files:
  (setq lsp-bridge-user-multiserver-dir (expand-file-name "data/lsp-bridge" user-emacs-directory))

  ;; optional debug if something is wrong:
  ;; (setq lsp-bridge-enable-log t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Exclusion from selected major-modes:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq lsp-bridge-default-mode-hooks
        (remove 'cperl-mode-hook lsp-bridge-default-mode-hooks))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; php
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq lsp-bridge-php-lsp-server "phpactor")

  ;; (add-hook 'php-mode-hook
  ;;           (lambda ()
  ;; 	      (lsp-bridge-mode)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; python
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; `basedpyright' is a fork of pyright with various type checking improvements, improved vscode support and pylance features built into the language server.
  ;; `basedpyright' seems to be better than `pyright', but also very similar
  ;; And as failback - we can try `pylsp'
  ;; pyright/basedpyright - currently "code actions" are broken, so must be executed with `ruff-lsp' (so as multiserver)

  (setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff") ;; so in summary - this is the best option for now

  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;; 	      (lsp-bridge-mode)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Load BIG list of keybindings:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'cfg-gen-op-lsp-bridge))

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
