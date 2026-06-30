;;; cfg-op-emmet.el --- configuration for emmet -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure t
  :functions (emmet-mode)
  :config
  (dolist (hook '(web-mode-hook
                  html-ts-mode-hook
                  html-mode-hook
                  mhtml-mode-hook
                  sgml-mode-hook
                  css-mode-hook
                  css-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (emmet-mode)))))

;; load general.el and keybindings:
(use-package cfg-gen-op-emmet)

(provide 'cfg-op-emmet)
;;; cfg-op-emmet.el ends here
