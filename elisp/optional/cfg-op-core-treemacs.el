;;; cfg-op-core-treemacs.el --- configuration for built-in treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "elisp/site-elisp/treemacs" user-emacs-directory))

(use-package treemacs
  :config
  ;; ---------------------------------------------------------------------------
  ;; load general.el
  ;; Keybindings should be fully compatible with the standard treemacs.
  ;; ---------------------------------------------------------------------------
  (require 'cfg-gen-op-treemacs-mode))

(provide 'cfg-op-core-treemacs)
;;; cfg-op-core-treemacs.el ends here
