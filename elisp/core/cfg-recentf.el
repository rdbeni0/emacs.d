;;; cfg-recentf.el --- configfuration for recentf filtering -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with "recentf", "grep" and similar filtering.
;; https://www.emacswiki.org/emacs/RecentFiles
;;
;;; Code:

(use-package recentf
  :config
  (recentf-mode 1)
  ;; Save 10000 files for recentf mode:
  (setq recentf-max-saved-items 10000)
  (setq recentf-max-menu-items 10000)
  ;; "By default, Recentf saves the list of recent files on exiting Emacs (specifically, `recentf-save-list` is called on `kill-emacs-hook`).
  ;; If Emacs exits abruptly for some reason the recent file list will be lost - therefore you may wish to call `recentf-save-list` periodically, e.g. every 5 minutes:"
  ;;
  ;; https://stackoverflow.com/questions/8023670/change-number-of-files-recentf-in-emacs-stores-using-ido-completion-method
  (run-at-time nil (* 10 60) 'recentf-save-list)
  ;; Currently working on "pure" recentf-mode ("recentf-open-files").
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-recentf))

(defun cfg/recentf-jump-open ()
  "Use `completing-read' to \\[find-file] a recent file
   https://www.masteringemacs.org/article/find-files-faster-recent-files-package "
  (interactive)
  (if (find-file (completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'cfg-recentf)
;;; cfg-recentf.el ends here
