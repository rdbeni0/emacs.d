;;; cfg-op-nix.el --- configfuration for nix and nixos -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with nix and NixOS.
;;
;;; Code:

(use-package nix-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-nix-mode))

(provide 'cfg-op-nix)
;;; cfg-op-nix.el ends here
