;;; cfg-grep.el --- configfuration for grep and recentf filtering -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with "recentf", "grep" and similar filtering.
;; https://www.emacswiki.org/emacs/RecentFiles
;;
;;; Code:

(use-package wgrep
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-grep-mode))

(defun cfg/grep-recentf (filepattern pattern)
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

      (grep (format "%s %s | xargs -0 grep -n -i \"%s\" " pattern)))))

;; grep command - add "-R" to follow symlinks:
(setq grep-command "grep --color=auto -nH --null -R -e ")
;; https://stackoverflow.com/questions/28915372/change-the-default-find-grep-command-in-emacs
;; change find command to check also for symlinks - it will be used via 'M-x rgrep':
(setq grep-find-template
      "find <D> <X> \\( -type f -o -type l \\) <F> -exec grep <C> -r -nH -e <R> \\{\\} +")

(defun cfg/ffap ()
  "Standard verion of `ffatp` command (without embark)."
  (interactive)
  (ffap))

(provide 'cfg-grep)
;;; cfg-grep.el ends here
