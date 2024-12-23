;;; cfg-op-org.el --- configfuration for org mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Additional packages for org-mode.
;;
;;; Code:

;; org-static-blog
;; https://github.com/bastibe/org-static-blog
(use-package org-static-blog
  :ensure t
  )

;; OPTIONAL: orglink
;; Similar to: https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
;;
;; https://github.com/tarsius/orglink
;; (use-package orglink
;;   :ensure t
;;   :config
;;   (global-orglink-mode 1))

;; org-roam
;; https://github.com/org-roam
;; https://www.reddit.com/r/OrgRoam/comments/lxl353/what_keybinding_remaps_have_you_done_for_org_roam/
;; https://systemcrafters.cc/build-a-second-brain-in-emacs/getting-started-with-org-roam/
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "data/OrgRoamNotes" user-emacs-directory))
  (org-roam-db-location (expand-file-name "data/org-roam.db" user-emacs-directory))
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup))

;; load general.el and keybindings - will be used also for other packages:
(require 'cfg-gen-op-org-mode)

;; Please create correct "lo-op-org.el" file inside ~/.emacs.d/data/local/lo-op-org.el (or other emacs dir)
;; Please add additional configuration for org-static-blog or/and org-roam inside this file.
(if (file-readable-p (expand-file-name "data/local/lo-op-org.el" user-emacs-directory))
    (require 'lo-op-org)  ; if true, load additional variables for org
					; if false, then message with "WARNING" will appear during initialization of op-org:
  (message "WARNING! File data/local/lo-op-org.el inside your emacs.d is not readable (or not exist)! Please create it and add correct op-org options!"))

(provide 'cfg-op-org)
;;; cfg-op-org.el ends here
