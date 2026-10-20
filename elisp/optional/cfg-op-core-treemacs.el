;;; cfg-op-core-treemacs.el --- configuration for built-in treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package treemacs
  :config

  (setq treemacs-icons-directory
        (expand-file-name "data/assets/icons" user-emacs-directory))

  ;; ---------------------------------------------------------------------------
  ;; load general.el
  ;; Keybindings should be fully compatible with the standard treemacs.
  ;; ---------------------------------------------------------------------------
  (require 'cfg-gen-op-treemacs-mode))

(provide 'cfg-op-core-treemacs)
;;; cfg-op-core-treemacs.el ends here
