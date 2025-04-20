;;; cfg-list-core-packages.el --- declarative list of core packages -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Core packages: packages which are required for core config.
;; Just make sure that all pkgs in the list are installed and nothing more.
;; For additional config - "use-package" should be used.
;;
;;; Code:

(dolist (core-packages
	 '(
	   which-key
	   evil
	   annalist ;; required for "evil-collection"
	   evil-collection
	   evil-org
	   general
	   wgrep
	   ;;
	   ))
  (unless (package-installed-p core-packages) (package-install core-packages)))

(provide 'cfg-list-core-packages)
;;; cfg-list-core-packages.el ends here
