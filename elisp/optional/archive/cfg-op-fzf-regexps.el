;;; cfg-op-regexps.el --- configfuration for regexps -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Various tools for Regular Expressions.
;;
;;; Code:

;; https://github.com/jwiegley/regex-tool
(use-package regex-tool
  :ensure t
  :config
  (setq regex-tool-backend "perl")
  ;; load keybindings from general.el framework:
  (require 'cfg-gen-op-fzf-regexps))

;; https://github.com/bling/fzf.el
(use-package fzf
  :ensure t)

(provide 'cfg-op-regexps)
;;; cfg-op-regexps.el ends here
