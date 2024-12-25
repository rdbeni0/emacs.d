;;; cfg-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with eglot for emacs.
;; https://github.com/joaotavora/eglot
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
;;
;;; Code:

(use-package eglot
  :config
  ;; https://www.gnu.org/software/emacs/manual/html_node/eglot/Customizing-Eglot.html
  (setq eglot-report-progress nil)
  (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t)

  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  ;; load keybindings from general.el framework:
  (require 'cfg-gen-co-eglot-mode))

(provide 'cfg-eglot)
;;; cfg-eglot.el ends here
