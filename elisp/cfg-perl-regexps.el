;;; cfg-perl-regexps.el --- configfuration for perl and regexps -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with perl programming and regular expressions

;;; Code:

;; perl-quote
;; https://user42.tuxfamily.org/perl-quote/index.html
;; ^ latest, updated and patched version will be located in the "site-elisp"

(use-package perl-quote
  ;; :load-path "site-elisp/"
  :config
  ;;
  )

;; perl find modules
;; https://www.emacswiki.org/emacs/CPerlMode#h5o-9

(defun cfg/perl-module-path (module-name)
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
      path))
  )

(defun cfg/find-perl-module (module-name)
  (interactive "sPerl module name: ")
  (let ((path (cfg/perl-module-path module-name)))
    (if path
	(find-file path)
      (error "Module '%s' not found" module-name)))
  )

;; perltidy

(defvar perl5-perltidy-executable "perltidy"
  "Location of perltidy executable.")

(defvar perl5-perltidy-options '("--quiet"
				 "--standard-error-output"
				 "--perl-best-practices"
				 "-l=185")
  "Command line options to pass to perltidy")

;; other and alternative option:
;;
;; (defun cfg/perltidy-format ()
;;     "Run perltidy on the current region."
;;    (interactive)
;;    (save-excursion
;;      (shell-command-on-region (point) (mark) "/usr/bin/perltidy -q" nil t)))

(defun cfg/perltidy-format ()
  "Format Perl5 code with perltidy.
   If region is active, operate on it, else operate on line."
  (interactive)
  (let ((old-point (point))
        (pos
         (if (use-region-p)
             (cons (region-beginning)
                   (if (char-equal ?\n (char-before (region-end)))
                       (region-end)
                     (save-excursion ;; must including terminating newline
                       (goto-char (region-end))
                       (1+ (line-end-position)))))
           (cons (line-beginning-position)
                 (1+ (line-end-position))))))
    (apply #'call-process-region (car pos) (cdr pos) perl5-perltidy-executable t '(t nil)
           perl5-perltidy-options)
    (goto-char old-point))
  )

(defun cfg/perltidy-format-buffer ()
  "Format current buffer with perltidy."
  (interactive)
  (mark-whole-buffer)
  (cfg/perltidy-format)
  )

(defun cfg/perltidy-format-function ()
  "Format current function (sub) with perltidy."
  (interactive)
  (mark-defun)
  (cfg/perltidy-format)
  )

(use-package cperl-mode
  ;;     :demand t
  :init
  (defalias 'perl-mode 'cperl-mode) ;; change this alias if you want back to the native perl-mode
  (eval-after-load "ffap" '(require 'ffap-perl-module))
  :config
  (setq cperl-electric-keywords nil)
  (clear-abbrev-table cperl-mode-abbrev-table)
  )

;; OPTIONAL package : https://github.com/aki2o/emacs-plsense
;;
;; (defun cfg/plsense-go()
;;   "Start plsense server and load buffer into it."
;;   (interactive)
;;   (plsense-server-start)
;;   (plsense-setup-current-buffer)
;;   )

;; Tools for Regular Expressions : https://github.com/jwiegley/regex-tool

(use-package regex-tool
  :ensure t
  :config
  (setq regex-tool-backend "perl")
  )

(provide 'cfg-perl-regexps)
;;; cfg-perl-regexps.el ends here
