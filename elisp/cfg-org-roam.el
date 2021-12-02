;;; cfg-org-roam.el --- configfuration for org mode -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with org-roam (but not org-mode).
;;
;; sources:
;; https://github.com/org-roam
;; https://www.reddit.com/r/OrgRoam/comments/lxl353/what_keybinding_remaps_have_you_done_for_org_roam/
;; https://systemcrafters.cc/build-a-second-brain-in-emacs/getting-started-with-org-roam/
;;
;;; Code:

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/emacs.d/OrgRoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))


(provide 'cfg-org-roam)
;;; cfg-org-roam.el ends here
