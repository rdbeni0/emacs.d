;;; at-long-lines.el --- Abbrevs with long lines -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Abbrev defs with very long lines.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMACS-LISP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-abbrev emacs-lisp-mode-abbrev-table "qqlinl" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
(define-abbrev emacs-lisp-mode-abbrev-table "qqlinh" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;;;; HEADER1\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CPERL-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-abbrev cperl-mode-abbrev-table "qqlinl" "###############################################################################\n")
(define-abbrev cperl-mode-abbrev-table "qqlinh" "###############################################################################\n#### HEADER1\n###############################################################################\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PYTHON-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-abbrev python-mode-abbrev-table "qqlinl" "###############################################################################\n")
(define-abbrev python-mode-abbrev-table "qqlinh" "###############################################################################\n#### HEADER1\n###############################################################################\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SH-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-abbrev sh-mode-abbrev-table "qqlinl" "###############################################################################\n")
(define-abbrev sh-mode-abbrev-table "qqlinh" "###############################################################################\n#### HEADER1\n###############################################################################\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PHP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-abbrev php-mode-abbrev-table "qqlinl" "###############################################################################\n")
;;(define-abbrev php-mode-abbrev-table "qqlinh" "###############################################################################\n#### HEADER1\n###############################################################################\n")



(provide 'at-long-lines)
;;; at-long-lines.el ends here
