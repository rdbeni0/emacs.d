;;; cfg-abbrevs-tempo.el --- configfuration for abbrevs and tempo -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
;; http://xahlee.info/emacs/emacs/elisp_abbrev_hook.html
;;
;;; Code:

;; This file should be used for private abbrevs:
(setq abbrev-file-name (expand-file-name "data/abbrev_defs" user-emacs-directory))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; If the user has a file of abbrevs, read it (unless -batch):
(when (and (not noninteractive)
	   (file-exists-p abbrev-file-name)
	   (file-readable-p abbrev-file-name))
  (progn
    (require 'at-abbrev_defs) ;; main file with abbrev defs
    (require 'at-long-lines) ;; defs with long lines
    ;; This file should be used for private abbrevs:
    (quietly-read-abbrev-file abbrev-file-name)
    ))

(defun cfg/expand-abbrev ()
  "Try to expand abbrev at point. If no expansion, prompt to select from the current mode's abbrev table."
  (interactive)
  (if (expand-abbrev)
      (message "Abbrev expanded")
    (let* ((abbrev-table-symbol (intern (concat (symbol-name major-mode) "-abbrev-table")))
	   (abbrev-table (and (boundp abbrev-table-symbol) (symbol-value abbrev-table-symbol))))
      (if abbrev-table
	  (let* ((abbrev (completing-read "Select abbrev: " abbrev-table))
		 (expansion (abbrev-expansion abbrev abbrev-table)))
	    (when expansion
	      (insert expansion)))
	(message "No abbrev found.")))))

(provide 'cfg-abbrevs-tempo)
;;; cfg-abbrevs-tempo.el ends here
