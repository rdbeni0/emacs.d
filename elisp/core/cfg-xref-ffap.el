;;; cfg-xref-ffap.el --- configfuration for xref and ffap  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package xref
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-xref-ffap))

(provide 'cfg-xref-ffap)
;;; cfg-xref-ffap.el ends here
