;;; cfg-list-auto-mode-alist.el --- Full list for auto-mode-alist -*- lexical-binding: t -*-
;;; Commentary:

;; Full list with major modes and files:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html
;; https://www.emacswiki.org/emacs/AutoModeAlist

;;; Code:

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("\\.*_conf_file\\'" . conf-mode)
	 ("\\.gcs\\'" . text-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode)
	 )
       auto-mode-alist)
      )

(provide 'cfg-list-auto-mode-alist)
;;; cfg-list-auto-mode-alist.el ends here
