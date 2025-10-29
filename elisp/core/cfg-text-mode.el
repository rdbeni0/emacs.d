;;; cfg-text-mode.el --- hideshow -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; js-mode is for js and json
(use-package text-mode
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-js-mode))

(provide 'cfg-text-mode)
;;; cfg-text-mode.el ends here
