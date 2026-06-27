;;; cfg-op-nix.el --- configfuration for nix and NixOS -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/nix-community/nixd/blob/main/nixd/docs/editor-setup.md

(use-package nix-mode
  :ensure t
  :defines
  (eglot-server-programs)
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

(provide 'cfg-op-nix)
;;; cfg-op-nix.el ends here
