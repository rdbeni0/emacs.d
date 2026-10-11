;;; cfg-op-elm.el --- configfuration for elm -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/jcollard/elm-mode
(use-package elm-mode
  :ensure t
  :config

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-elm-mode))

(provide 'cfg-op-elm)
;;; cfg-op-elm.el ends here
