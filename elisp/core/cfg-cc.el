;;; cfg-cc.el --- configfuration for cc-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; "CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C, Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code."
;; cc-mode:
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;; https://wikemacs.org/wiki/CC-mode
;; https://cc-mode.sourceforge.net/html-manual/index.html
;;
;;; Code:

(use-package cc-mode
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-cc-mode))

(provide 'cfg-cc)
;;; cfg-cc.el ends here
