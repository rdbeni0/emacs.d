;;; cfg-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with eglot for emacs.
;; https://github.com/joaotavora/eglot
;;
;;; Code:

(use-package eglot
  :config
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  ;; load keybindings from general.el framework:
  (require 'cfg-gen-co-eglot-mode))

(provide 'cfg-eglot)
;;; cfg-eglot.el ends here
