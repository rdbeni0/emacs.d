;;; cfg-abbrevs-defs.el --- abbrevs definitions -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Abbrevs definitions (public).
;;
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; START -> TABLE OF CONTENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;; emacs-lisp-mode
;; cperl-mode
;; python-mode
;; sh-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq org-mode-abbrev-table nil)
(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("qscss" "#+BEGIN_SRC css\n\n#+END_SRC\n" nil 0)
    ("qsel" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC\n" nil 0)
    ("qsjava" "#+BEGIN_SRC java\n\n#+END_SRC\n" nil 0)
    ("qsphp" "#+BEGIN_SRC php\n\n#+END_SRC\n" nil 0)
    ("qspl" "#+BEGIN_SRC perl\n\n#+END_SRC\n" nil 0)
    ("qspy" "#+BEGIN_SRC python\n\n#+END_SRC\n" nil 0)
    ("qssh" "#+BEGIN_SRC shell\n\n#+END_SRC\n" nil 0)
    ("qssql" "#+BEGIN_SRC sql\n\n#+END_SRC\n" nil 0)
    ("qslua" "#+BEGIN_SRC lua\n\n#+END_SRC\n" nil 0)
    ("qqex" "#+BEGIN_EXAMPLE\n\n\#+END_EXAMPLE\n" nil 0)
    ("qqs" "#+BEGIN_SRC\n\n#+END_SRC\n" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMACS-LISP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq emacs-lisp-mode-abbrev-table nil)
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("qqatt" "(define-abbrev-table \'XXX-YYY-mode-abbrev-table\n\  '(\n    (\"te\" \"test\" nil 0)\n    ))" nil 0) ;; define new at
    ("qqata" "    (\"qq\" \"#n\" nil 0)" nil 0) ;; add element to the at
    ("qqll" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" nil 0)
    ("qqlh" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;;;; HEADER1\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CPERL-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq cperl-mode-abbrev-table nil)
(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env perl" nil 0)
    ("qqll" "###############################################################################\n" nil 0)
    ("qqlh" "###############################################################################\n#### HEADER1\n###############################################################################\n" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PYTHON-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq python-mode-abbrev-table nil)
(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env python" nil 0)
    ("qqll" "###############################################################################\n" nil 0)
    ("qqlh" "###############################################################################\n#### HEADER1\n###############################################################################\n" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SH-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq sh-mode-abbrev-table nil)
(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env bash" nil 0)
    ("qqll" "###############################################################################\n" nil 0)
    ("qqlh" "###############################################################################\n#### HEADER1\n###############################################################################\n" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END -> quietly-read-abbrev-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file should be used for private abbrevs.
;; If the user has a file of abbrevs, read it (unless -batch):

(when (and (not noninteractive)
	   (file-exists-p abbrev-file-name)
	   (file-readable-p abbrev-file-name))
  (quietly-read-abbrev-file abbrev-file-name))

(provide 'cfg-abbrevs-defs)
;;; cfg-abbrevs-defs.el ends here
