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
;;;; Shortcuts / namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here you will find general rules and shortcuts (namespaces) for abbrevs.
;; The shortcuts will be based on C or C++ (e.g. `include' as `qqinc' instead of import as `qqimp').
;;
;; prefix:
;;
;; qq    : public common abbrevs (system scope)
;; qs    : public skeletons; larger code templates
;; qz    : private abbrevs (defined in abbrev_defs)
;; qx    : private skeletons (defined in abbrev_defs or somewhere else)
;;
;; shortcut:
;;
;; arr  :  arrays, tables
;; cfg  :  config; app configs
;; cla  :  class
;; dbs  :  databases, sql
;; deb  :  debug info
;; exc  :  exceptions
;; fih  :  file handling; file operations
;; fun  :  function, definitions, submodules
;; ifc  :  if conditions; ...else if... else statements (overall conditionals)
;; inc  :  include, import; loading external libraries
;; lfo  :  loop: for
;; lwh  :  loop: while
;; lin  :  lines; long lines of comments
;; opt  :  options; getopt - parsing program options (CLI args and flags)
;; sta  :  start; everything related to the beginning of the file, e.g. shebang or declarations for editor settings
;; str  :  string; manipulations with strings
;; sys  :  sys, system(); run commands
;; tes  :  tests
;; url  :  url; curl, http/rest requests
;; TODO...
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq org-mode-abbrev-table nil)
(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("qqex" "#+BEGIN_EXAMPLE\n\n\#+END_EXAMPLE\n" nil 0 :system t)
    ("qqs" "#+BEGIN_SRC\n\n#+END_SRC\n" nil 0 :system t)
    ("qqscss" "#+BEGIN_SRC css\n\n#+END_SRC\n" nil 0 :system t)
    ("qqsel" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC\n" nil 0 :system t)
    ("qqsjava" "#+BEGIN_SRC java\n\n#+END_SRC\n" nil 0 :system t)
    ("qqslua" "#+BEGIN_SRC lua\n\n#+END_SRC\n" nil 0 :system t)
    ("qqsphp" "#+BEGIN_SRC php\n\n#+END_SRC\n" nil 0 :system t)
    ("qqspl" "#+BEGIN_SRC perl\n\n#+END_SRC\n" nil 0 :system t)
    ("qqspy" "#+BEGIN_SRC python\n\n#+END_SRC\n" nil 0 :system t)
    ("qqssh" "#+BEGIN_SRC shell\n\n#+END_SRC\n" nil 0 :system t)
    ("qqssql" "#+BEGIN_SRC sql\n\n#+END_SRC\n" nil 0 :system t)
    ("qqprop" ":PROPERTIES:\n:CUSTOM_ID: XXX\n:END:\n" nil 0 :system t)
    ("qqqu" "#+BEGIN_QUOTE\n\n#+END_QUOTE\n" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMACS-LISP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq emacs-lisp-mode-abbrev-table nil)
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("qqarrt" "(define-abbrev-table 'XXX-mode-abbrev-table\n\  '(\n    (\"te\" \"test\" nil 0 :system t)\n    ))" nil 0 :system t) ;; define new at
    ("qqarra" "    (\"qq\" \"#\\n\" nil 0 :system t)" nil 0 :system t) ;; add element to the at
    ("qqarrd" "(define-abbrev XXX-mode-abbrev-table \"qq\" \"\\n\")" nil 0 :system t)
    ("qqsta1" ";;; cfg-XX.el --- XX -*- lexical-binding: t -*-\n;;; Commentary:\n;;\n;;; Code:\n\n(provide 'cfg-XX)\n;;; cfg-XX.el ends here" nil 0 :system t)
    ))

(define-skeleton emacs-lisp-skeleton-use-package
  "Emacs Lisp skeleton for use-package with useful options"
  nil
  "(use-package my-package\n"
  "  :ensure t\n"
  "  :init\n"
  "  ;; Code to run before package is loaded\n"
  "  (setq my-package-variable t)\n"
  "  :config\n"
  "  ;; Code to run after package is loaded\n"
  "  (setq package-var t)\n"
  "  ;; load general.el and keybindings:\n"
  "  (require 'cfg-gen-XX)\n"
  "  :bind (;; Remaps - replace with emacs native:\n"
  "  ([remap old-emacs-defun] . new-defun-name))\n"
  "  :custom\n"
  "  ;; Custom variables for the package\n"
  "  (my-package-setting 'value)\n"
  "  :defer t\n"
  "  ;; Defer loading until needed\n"
  "  :after (evil hydra)\n"
  "  :if\n"
  "  ;; Conditional loading based on a variable\n"
  "  (featurep 'another-feature))\n")

(define-skeleton emacs-lisp-skeleteon-skel
  "Skeleteon for skeleteons"
  nil
  "(define-skeleton my-mode-skeleton-name\n"
  "  \"Description...\"\n"
  "  nil\n"
  "  \"  <elisp code>\\n\"\n"
  ")\n"
  "\n"
  "(define-abbrev my-mode-abbrev-table \"qsXXX\" \";; SKELETON> description of functionality.\\n\" 'my-mode-skeleton-name :system t)\n"
  )

(define-abbrev emacs-lisp-mode-abbrev-table "qsinc1" ";; SKELETON> use-package with examples.\n" 'emacs-lisp-skeleton-use-package :system t)
(define-abbrev emacs-lisp-mode-abbrev-table "qsdef1" ";; SKELETON> skeleton for skeletons.\n" 'emacs-lisp-skeleteon-skel :system t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CPERL-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq cperl-mode-abbrev-table nil)
(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("qqsta" "#!/usr/bin/env perl\n# -*- coding: utf-8 -*-\n\nuse strict;\nuse warnings;\n" nil 0 :system t)
    ("qqfunh" "sub help {\n    print \"Usage: ...\\n\";\n    exit;\n}" nil 0 :system t)
    ("qqifc" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n}" nil 0 :system t)
    ("qqifce" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n} else {\n    # code here\n}" nil 0 :system t)
    ("qqifce1" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n} elsif (CO) {\n    # code here\n} else {\n    # code here\n}" nil 0 :system t)
    ("qqifce2" "# COnditions: == != > >= <= eq ne gt ge lt le\nif (CO) {\n    # code here\n} elsif (CO) {\n    # code here\n} elsif (CO) {\n    # code here\n} else {\n    # code here\n}" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PYTHON-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq python-mode-abbrev-table nil)
(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("qqsta" "#!/usr/bin/env python\n# -*- coding: utf-8 -*-" nil 0 :system t)
    ("qqfunh" "def help():\n    print('X')\n    exit()\n" nil 0 :system t)
    ("qqifc" "#COnditions: == != > >= <= and or not\nif (CO):\n    X" nil 0 :system t)
    ("qqifce" "#COnditions: == != > >= <= and or not\nif (CO):\n    X\nelse:\n    X" nil 0 :system t)
    ("qqifce1" "#COnditions: == != > >= <= and or not\nif (CO):\n    X\nelif (CO):\n    X\nelse:\n    X" nil 0 :system t)
    ("qqifce2" "#COnditions: == != > >= <= and or not\nif (CO):\n    X\nelif (CO):\n    X\nelif (CO):\n    X\nelse:\n    X" nil 0 :system t)
    ("qqinc" "import re, os, sys\n" nil 0 :system t)
    ("qqfihw1" "with open(\'X\', \'a\') as file:\n    file.write(\"\\nNew Line\")" nil 0 :system t)
    ("qqfihr1" "with open(\'X\', \'r\') as file:\n    for line in file:\n        print(line.strip())\n" nil 0 :system t)
    ))

(define-skeleton python-argparse-skeleton
  "Insert a Python argparse example with def main()."
  nil
  "#!/usr/bin/env python\n"
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

(define-skeleton python-file-replace-skeleton
  "Insert Python code to replace text line by line in a file."
  nil
  "#SKELETON> In the case of 'with open (...)' file.close() is automatic (not required).\n"
  "with open('X', 'r') as file:\n"
  "    lines = file.readlines()\n\n"
  "with open('X', 'w') as file:\n"
  "    for line in lines:\n"
  "        new_line = line.replace(\"" (skeleton-read "Old text: ") "\", \"" (skeleton-read "New text: ") "\")\n"
  "        file.write(new_line)\n")

(define-abbrev python-mode-abbrev-table "qsopt1" "#SKELETON> Argparse - example with def main().\n" 'python-argparse-skeleton :system t)
(define-abbrev python-mode-abbrev-table "qsfih1" "#SKELETON> File handling - replace text line by line in a file.\n" 'python-file-replace-skeleton :system t)
;; python-mode skeletons:
(define-abbrev python-mode-abbrev-table "qscla1" "#SKELETON> Insert class (via python-mode).\n" 'python-skeleton-class :system t)
(define-abbrev python-mode-abbrev-table "qsfun1" "#SKELETON> Insert def (via python-mode).\n" 'python-skeleton-def :system t)
(define-abbrev python-mode-abbrev-table "qslfo1" "#SKELETON> Insert for loop (via python-mode).\n" 'python-skeleton-for :system t)
(define-abbrev python-mode-abbrev-table "qslwh1" "#SKELETON> Insert while loop (via python-mode).\n" 'python-skeleton-while :system t)
(define-abbrev python-mode-abbrev-table "qsifc1" "#SKELETON> Insert if (via python-mode).\n" 'python-skeleton-if :system t)
(define-abbrev python-mode-abbrev-table "qsinc1" "#SKELETON> Insert import (via python-mode).\n" 'python-skeleton-import :system t)
(define-abbrev python-mode-abbrev-table "qsexc1" "#SKELETON> Insert try.. catch block (via python-mode).\n" 'python-skeleton-try :system t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SH-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq sh-mode-abbrev-table nil)
(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("qqsta" "#!/usr/bin/env bash\n# -*- coding: utf-8 -*-" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PHP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq php-mode-abbrev-table nil)
(define-abbrev-table 'php-mode-abbrev-table
  '(
    ("qqsta" "#!/usr/bin/env php\n<?php\n" nil 0 :system t)
    ("qqifc" "// COnditions strict: > >= === !== <= <\n// COnditions non strict: == != <=>\nif ($condition) {\n    // code here\n}" nil 0 :system t)
    ("qqifce" "// COnditions strict: > >= === !== <= <\n// COnditions non strict: == != <=>\nif ($condition) {\n    // code here\n} else {\n    // code here\n}" nil 0 :system t)
    ("qqifce1" "// COnditions strict: > >= === !== <= <\n// COnditions non strict: == != <=>\nif ($condition) {\n    // code here\n} elseif ($condition) {\n    // code here\n} else {\n    // code here\n}" nil 0 :system t)
    ("qqifce2" "// COnditions strict: > >= === !== <= <\n// COnditions non strict: == != <=>\nif ($condition) {\n    // code here\n} elseif ($condition) {\n    // code here\n} elseif ($condition) {\n    // code here\n} else {\n    // code here\n}" nil 0 :system t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTMUCH-MESSAGE-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq notmuch-message-mode-abbrev-table nil)
(define-abbrev-table 'notmuch-message-mode-abbrev-table '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LUA-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-abbrev-table 'lua-mode-abbrev-table
  '(
    ("qqsta" "#!/usr/bin/env lua\n-- -*- mode: lua -*-\n" nil 0 :system t)

    ;; if conditions
    ("qqifc" "-- Conditions examples:\n-- > >= == ~= <= <\nif condition then\n  -- code here\nend" nil 0 :system t)

    ;; if ... else
    ("qqifce" "-- Conditions examples:\n-- > >= == ~= <= <\nif condition then\n  -- code here\nelse\n  -- code here\nend" nil 0 :system t)

    ;; if ... elseif ... else (1 elseif)
    ("qqifce1" "-- Conditions examples:\n-- > >= == ~= <= <\nif condition then\n  -- code here\nelseif condition then\n  -- code here\nelse\n  -- code here\nend" nil 0 :system t)

    ;; if ... elseif ... elseif ... else (2 elseif)
    ("qqifce2" "-- Conditions examples:\n-- > >= == ~= <= <\nif condition then\n  -- code here\nelseif condition then\n  -- code here\nelseif condition then\n  -- code here\nelse\n  -- code here\nend" nil 0 :system t)

    ;; FILE HANDLING (qqfih*)

    ;; Write single line to file (overwrite)
    ("qqfihw1" "-- Write single line to file (overwrite)\nlocal file = io.open('X', 'w')\nfile:write('New Line\\n')\nfile:close()" nil 0 :system t)

    ;; Append single line to file
    ("qqfihw2" "-- Append single line to file\nlocal file = io.open('X', 'a')\nfile:write('Appended Line\\n')\nfile:close()" nil 0 :system t)

    ;; Write multiple lines to file
    ("qqfihw3" "-- Write multiple lines to file\nlocal file = io.open('X', 'w')\nfile:write('Line 1\\n')\nfile:write('Line 2\\n')\nfile:close()" nil 0 :system t)

    ;; Write table contents to file
    ("qqfihw4" "-- Write table contents to file\nlocal file = io.open('X', 'w')\nlocal data = { 'one', 'two', 'three' }\nfor _, v in ipairs(data) do\n  file:write(v .. '\\n')\nend\nfile:close()" nil 0 :system t)

    ;; Read all file content at once
    ("qqfihr1" "-- Read all file content at once\nlocal file = io.open('X', 'r')\nlocal content = file:read('*all')\nfile:close()\nprint(content)" nil 0 :system t)

    ;; Read file line by line and print
    ("qqfihr2" "-- Read file line by line and print\nfor line in io.lines('X') do\n  print(line)\nend" nil 0 :system t)

    ;; Read file into table
    ("qqfihr3" "-- Read file into table\nlocal lines = {}\nfor line in io.lines('X') do\n  table.insert(lines, line)\nend\nprint('Total lines:', #lines)" nil 0 :system t)

    ;; Safe file read with error handling
    ("qqfihr4" "-- Safe file read with error handling\nlocal file, err = io.open('X', 'r')\nif not file then\n  print('Error opening file:', err)\n  return\nend\nfor line in file:lines() do\n  print(line)\nend\nfile:close()" nil 0 :system t)

    ;; Check if file exists
    ("qqfihx1" "-- Check if file exists\nlocal f = io.open('X', 'r')\nif f then\n  print('File exists')\n  f:close()\nelse\n  print('File not found')\nend" nil 0 :system t)

    ;; Copy one file to another
    ("qqfihc1" "-- Copy one file to another\nlocal src = io.open('source.txt', 'r')\nlocal dst = io.open('dest.txt', 'w')\nfor line in src:lines() do\n  dst:write(line .. '\\n')\nend\nsrc:close()\ndst:close()" nil 0 :system t)
    ))


(provide 'at-abbrev_defs)
;;; at-abbrev_defs.el ends here
