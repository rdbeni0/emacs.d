;;; cfg-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with eglot for emacs.
;; https://github.com/joaotavora/eglot

;;; Code:

;; (require 'eglot)

(use-package eglot
  :ensure t
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load keybindings from general.el framework:
(require 'cfg-gen-eglot-mode)

(provide 'cfg-eglot)
;;; cfg-eglot.el ends here
