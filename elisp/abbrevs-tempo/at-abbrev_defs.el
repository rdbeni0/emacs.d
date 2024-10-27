;;; at-abbrev_defs.el --- Abbrev defs (public) -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Abbrev definitions (public).
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TOC - TABLE OF CONTENT
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
    ("qqatt" "(define-abbrev-table 'XXX-mode-abbrev-table\n\  '(\n    (\"te\" \"test\" nil 0)\n    ))" nil 0) ;; define new at
    ("qqata" "    (\"qq\" \"#\\n\" nil 0)" nil 0) ;; add element to the at
    ("qqatd" "(define-abbrev XXX-mode-abbrev-table \"qq\" \"\\n\")" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CPERL-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq cperl-mode-abbrev-table nil)
(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env perl\n# -*- coding: utf-8 -*-" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PYTHON-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq python-mode-abbrev-table nil)
(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env python\n# -*- coding: utf-8 -*-" nil 0)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SH-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq sh-mode-abbrev-table nil)
(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env bash\n# -*- coding: utf-8 -*-" nil 0)
    ))

(provide 'at-abbrev_defs)
;;; at-abbrev_defs.el ends here
