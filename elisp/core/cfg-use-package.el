;;; cfg-use-package.el --- configuration for packages and repositories -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Basic setup for use-package and additional repositories.
;; https://jwiegley.github.io/use-package/
;;
;;; Code:

;; files/packages from "optional" namespace should be loaded manually, via (require '):
(add-to-list 'load-path (expand-file-name "elisp/optional" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/abbrevs-tempo" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/site-elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "data/local" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "elisp/cfg-general" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/cfg-general/cfg-gen-core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/cfg-general/cfg-gen-optional" user-emacs-directory))

;; TODO: add also subdirs
;;
;; found here: https://stackoverflow.com/questions/56799992/how-can-i-register-a-recursive-load-path
;;
;; (let ((default-directory (expand-file-name "site-elisp/" user-emacs-directory))) (normal-top-level-add-subdirs-to-load-path))
;; ^ but it's not working!

(require 'package)
;; https://www.reddit.com/r/emacs/comments/1rdstn/set_packageenableatstartup_to_nil_for_slightly/
(setq package-enable-at-startup nil)

;; The following lines tell emacs where on the internet to look up for new packages (elisp repositories):
;; WARNING! The same settings could be used separately for epm (.epm.el) package - so please also look at .epm.el file
;; WARNING! Do not use marmalade - is an obsolete repository : https://marmalade-repo.org/#download

(setq package-archives '(
                         ("gnu"    . "http://elpa.gnu.org/packages/")
                         ("melpa"  . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ))


;; use-package

;; Unless it is already installed - update packages archive and install the most recent version of use-package:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; defuns

(defun cfg/load-all-el-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded.
  This function is (...) to avoid re-loading a library when both .el and .elc versions are present.
  More info and solutions:
  https://stackoverflow.com/questions/18706250/emacs-require-all-files-in-a-directory
  https://www.emacswiki.org/emacs/LoadingLispFiles"
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(defun cfg/load-all-el-in-directory-alt (dir)
  "`load' all elisp libraries in directory DIR - alternative version."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(provide 'cfg-use-package)
;;; cfg-use-package.el ends here
