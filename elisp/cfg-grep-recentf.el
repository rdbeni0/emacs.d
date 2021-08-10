;;; cfg-grep-recentf.el --- configfuration for grep and recentf filtering -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "recentf", "grep" and similar filtering.
;; https://www.emacswiki.org/emacs/RecentFiles

;;; Code:

;; grep

(use-package wgrep
  :ensure t
  :config
  ;;
  )

(defun grep-recentf (filepattern pattern)
  (interactive "sFiles regexp: \nsSearch regexp: ")
  (let ((files (if filepattern
                   (cl-remove-if-not (lambda (item) (string-match filepattern item))
                                     recentf-list)
                 recentf-list))
        (limit 50)
        (grep-use-null-device nil))
    (if (> (length files) limit)
        (subseq files 0 limit))

    (let* ((tempfile (make-temp-file "emacs"))
           (orig compilation-finish-functions))
      (add-to-list 'compilation-finish-functions
                   (lambda (buf result)
                     (setq font-lock-keywords-case-fold-search t)
                     (highlight-regexp pattern 'hi-yellow)
                     (delete-file tempfile)
                     (setq compilation-finish-functions orig)))

      (write-region  (mapconcat 'identity files (char-to-string 0))
                     nil tempfile)

      (grep (format "%s %s | xargs -0 grep -n -i \"%s\" " pattern))))
  )

;; recentf

(recentf-mode 1)

;; Save 1000 files for recentf mode:

(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 1000)

;; "By default, Recentf saves the list of recent files on exiting Emacs (specifically, `recentf-save-list` is called on `kill-emacs-hook`). 
;; If Emacs exits abruptly for some reason the recent file list will be lost - therefore you may wish to call `recentf-save-list` periodically, e.g. every 5 minutes:"
;;
;; https://stackoverflow.com/questions/8023670/change-number-of-files-recentf-in-emacs-stores-using-ido-completion-method

(run-at-time nil (* 10 60) 'recentf-save-list)

;; ... but it's not affecting helm! 
;; Currently working on "pure" recentf-mode ("recentf-open-files").
;;

(provide 'cfg-grep-recentf)
;;; cfg-grep-recentf.el ends here
