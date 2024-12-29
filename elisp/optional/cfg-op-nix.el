;;; cfg-op-nix.el --- configfuration for nix and nixos -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with nix and NixOS.
;;
;;; Code:

(use-package nix-mode
  :ensure t
  :config

  (with-eval-after-load 'eglot
    (dolist (mode '((nix-mode . ("nixd"))))
      (add-to-list 'eglot-server-programs mode)))

  ;; optional integration with eglot:
  ;;
  ;; (add-hook 'nix-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'before-save-hook 'eglot-format nil t)
  ;; 	      (add-to-list 'eglot-stay-out-of 'company)
  ;;             (eglot-ensure)
  ;; 	      ))

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-nix-mode))

;; https://github.com/nix-community/nixd/blob/main/nixd/docs/editor-setup.md

(provide 'cfg-op-nix)
;;; cfg-op-nix.el ends here
