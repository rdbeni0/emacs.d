;;; cfg-op-treemacs.el --- configfuration for treemacs and speedbar -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "elisp/site-elisp/treemacs" user-emacs-directory))

(use-package treemacs

  :config

  ;; ---------------------------------------------------------------------------
  ;; general.el keybindings
  ;; ---------------------------------------------------------------------------

  ;; Load user-defined Treemacs keybindings.
  (require 'cfg-gen-op-treemacs-mode))

(provide 'cfg-op-treemacs)
;;; cfg-op-treemacs.el ends here
