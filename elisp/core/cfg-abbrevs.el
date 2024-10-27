;;; cfg-abbrevs.el --- configfuration for abbrevs -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
;;
;;; Code:

(setq abbrev-file-name (expand-file-name "data/abbrev_defs" user-emacs-directory))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; If the user has a file of abbrevs, read it (unless -batch)
(when (and (not noninteractive)
	   (file-exists-p abbrev-file-name)
	   (file-readable-p abbrev-file-name))
  (quietly-read-abbrev-file abbrev-file-name))

(provide 'cfg-abbrevs)
;;; cfg-abbrevs.el ends here
