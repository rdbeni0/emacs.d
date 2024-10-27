;;; cfg-abbrevs-defs.el --- abbrevs definitions -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Abbrevs definitions (public).
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
;;
;;; Code:

(define-abbrev-table 'org-mode-abbrev-table '(
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
					      ))

(provide 'cfg-abbrevs-defs)
;;; cfg-abbrevs-defs.el ends here
