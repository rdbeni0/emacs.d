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

(provide 'cfg-abbrevs-tempo)
;;; cfg-abbrevs-tempo.el ends here
