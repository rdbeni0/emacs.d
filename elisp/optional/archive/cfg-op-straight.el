;;; cfg-op-php-straight.el --- configfuration for straight.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/radian-software/straight.el
;; https://systemcrafters.net/advanced-package-management/using-straight-el/
;;
;;; Code:

;; In order to use straight.el, you will need to somehow get it loaded into Emacs. 
;; (This is easy for package.el, since package.el is built in to Emacs. straight.el must work a little harder.)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(provide 'cfg-op-straight)
;;; cfg-op-straight.el ends here
