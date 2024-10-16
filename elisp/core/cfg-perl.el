;;; cfg-perl.el --- configfuration for perl -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with perl programming.
;;
;;; Code:

;; perl-quote
;; https://user42.tuxfamily.org/perl-quote/index.html
;; ^ latest, updated and patched version will be located in the "elisp/site-elisp":
(use-package perl-quote )

(use-package cperl-mode
  ;;     :demand t
  :init
  (defalias 'perl-mode 'cperl-mode) ;; change this alias if you want back to the native perl-mode
  (eval-after-load "ffap" '(require 'ffap-perl-module))
  :config
  (setq cperl-electric-keywords nil)
  (clear-abbrev-table cperl-mode-abbrev-table)
  (setq cperl-indent-level 4)
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-cperl-mode))

;; perl find modules
;; https://www.emacswiki.org/emacs/CPerlMode#h5o-9

(defun cfg/-perl-module-path (module-name)
  (let* ((file-name
	  (concat (replace-regexp-in-string "::" "/" module-name)
		  ".pm"))
	 (command-line
	  (concat "perl -M'"
		  module-name
		  "' -e'print $INC{q{"
		  file-name
		  "}}'"))
	 (path (shell-command-to-string command-line))
	 (cant-locate (string-match "^Can't locate " path)))
    (if cant-locate
	nil
      path)))

(defun cfg/find-perl-module (module-name)
  (interactive "sPerl module name: ")
  (let ((path (cfg/-perl-module-path module-name)))
    (if path
	(find-file path)
      (error "Module '%s' not found" module-name))))

(provide 'cfg-perl)
;;; cfg-perl.el ends here
