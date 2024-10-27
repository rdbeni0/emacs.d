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
    ("qscss" "#+BEGIN_SRC css\n\n#+END_SRC\n" nil 0 :system t)
    ("qsel" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC\n" nil 0 :system t)
    ("qsjava" "#+BEGIN_SRC java\n\n#+END_SRC\n" nil 0 :system t)
    ("qsphp" "#+BEGIN_SRC php\n\n#+END_SRC\n" nil 0 :system t)
    ("qspl" "#+BEGIN_SRC perl\n\n#+END_SRC\n" nil 0 :system t)
    ("qspy" "#+BEGIN_SRC python\n\n#+END_SRC\n" nil 0 :system t)
    ("qssh" "#+BEGIN_SRC shell\n\n#+END_SRC\n" nil 0 :system t)
    ("qssql" "#+BEGIN_SRC sql\n\n#+END_SRC\n" nil 0 :system t)
    ("qslua" "#+BEGIN_SRC lua\n\n#+END_SRC\n" nil 0 :system t)
    ("qqex" "#+BEGIN_EXAMPLE\n\n\#+END_EXAMPLE\n" nil 0 :system t)
    ("qqsr" "#+BEGIN_SRC\n\n#+END_SRC\n" nil 0 :system t)
    ("qqprop" ":PROPERTIES:\n:CUSTOM_ID: XXX\n:END:\n" nil 0 :system t)
    ("qqqu" "#+BEGIN_QUOTE\n\n#+END_QUOTE\n" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMACS-LISP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq emacs-lisp-mode-abbrev-table nil)
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("qqatt" "(define-abbrev-table 'XXX-mode-abbrev-table\n\  '(\n    (\"te\" \"test\" nil 0 :system t)\n    ))" nil 0 :system t) ;; define new at
    ("qqata" "    (\"qq\" \"#\\n\" nil 0 :system t)" nil 0 :system t) ;; add element to the at
    ("qqatd" "(define-abbrev XXX-mode-abbrev-table \"qq\" \"\\n\")" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CPERL-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq cperl-mode-abbrev-table nil)
(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env perl\n# -*- coding: utf-8 -*-\n\nuse strict;\nuse warnings;\n" nil 0 :system t)
    ("qqsubh" "sub help {\n    print \"Usage: ...\\n\";\n    exit;\n}" nil 0 :system t)
    ("qqif" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n}" nil 0 :system t)
    ("qqife" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n} else {\n    # code here\n}" nil 0 :system t)
    ("qqife1" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n} elsif (CO) {\n    # code here\n} else {\n    # code here\n}" nil 0 :system t)
    ("qqife2" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n} elsif (CO) {\n    # code here\n} elsif (CO) {\n    # code here\n} else {\n    # code here\n}" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PYTHON-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq python-mode-abbrev-table nil)

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env python\n# -*- coding: utf-8 -*-" nil 0 :system t)
    ("qqdefh" "def help():\n    print('X')\n    exit()\n" nil 0 :system t)
    ("qqif" "#COnditions: == != > >= <= and or not\nif (CO):\n    X" nil 0 :system t)
    ("qqife" "#COnditions: == != > >= <= and or not\nif (CO):\n    X\nelse:\n    X" nil 0 :system t)
    ("qqife1" "#COnditions: == != > >= <= and or not\nif (CO):\n    X\nelif (CO):\n    X\nelse:\n    X" nil 0 :system t)
    ("qqife2" "#COnditions: == != > >= <= and or not\nif (CO):\n    X\nelif (CO):\n    X\nelif (CO):\n    X\nelse:\n    X" nil 0 :system t)
    ("qqimp" "import re, os, sys\n" nil 0 :system t)
    ))

(define-skeleton python-argparse-skeleton
  "Insert a Python argparse example with def main()."
  nil
  "\n#!/usr/bin/env python\n"
  "# -*- coding: utf-8 -*-\n\n"
  "import argparse\n\n"
  "def main():\n"
  "    parser = argparse.ArgumentParser()\n"
  "    parser.add_argument(\"-t\", \"--test\", required=True, help=\"Test param\")\n"
  "    parser.add_argument(\"-a\", \"--age\", required=False, type=int, help=\"Foo\")\n\n"
  "    args = parser.parse_args()\n\n"
  "    print(f\"Hello, {args.test}! You are {args.age} years old.\")\n\n\n"
  "if __name__ == \"__main__\":\n"
  "    main()\n")

(define-abbrev python-mode-abbrev-table "qqdefargp" "#SKELETON > Insert a Python argparse example with def main()." 'python-argparse-skeleton :system t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SH-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq sh-mode-abbrev-table nil)
(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("qqs" "#!/usr/bin/env bash\n# -*- coding: utf-8 -*-" nil 0 :system t)
    ))

(provide 'at-abbrev_defs)
;;; at-abbrev_defs.el ends here
