;;; cfg-org.el --- configfuration for org mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with `org-mode'.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; old ISSUE - force install GNU ELPA version (and ignore built-in pkg):
;; https://github.com/jwiegley/use-package/issues/955
;; ^^ Remember to put it high in your config file so Emacs doesn't load the built-in org-mode first, otherwise you might get weird stuff like (void-function org-assert-version).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New options since Emacs 29+++

(use-package org
  :pin gnu ;; gnu must be there, other versions are buggy!
  :ensure t
  :config

  ;; Please create correct "lo-org.el" file inside ~/.emacs.d/data/local/lo-org.el (or other emacs dir)
  ;; Please add below variables inside this file:
  ;; (setq org-agenda-files ...

  (if (file-readable-p (expand-file-name "data/local/lo-org.el" user-emacs-directory))
      (require 'lo-org) ; if true, load additional variables for org-mode
					; if false, then message with "WARNING" will appear during initialization of org-mode:
    (message "WARNING! File data/local/lo-org.el inside your emacs.d is not readable (or not exist)! Please create it and add correct org-mode options!"))

  (require 'org-compat)
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-org-mode))

(provide 'cfg-org)
;;; cfg-org.el ends here
