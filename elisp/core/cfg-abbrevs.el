;;; cfg-abbrevs.el --- configfuration for abbrevs -*- lexical-binding: t -*-
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

(when (and (not noninteractive)
	   (file-exists-p abbrev-file-name)
	   (file-readable-p abbrev-file-name))
  (require 'cfg-abbrevs-defs))

(provide 'cfg-abbrevs)
;;; cfg-abbrevs.el ends here
