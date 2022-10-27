;;; cfg-list-pure-packages.el --- list of "pure" packages -*- lexical-binding: t -*-
;;; Commentary:

;; "Pure packages" - packages without config.
;; Just make sure that all pkgs are installed and nothing more.
;; For additional config - "use-package" should be used.

;;; Code:

(dolist (pure-packages
	 '(epm
	   s
	   htmlize
	   command-log-mode
	   ;; json-mode
	   ;; elisp-format
	   simpleclip
	   fzf
	   ))
  (unless (package-installed-p pure-packages) (package-install pure-packages)))

(provide 'cfg-list-pure-packages)
;;; cfg-list-pure-packages.el ends here
