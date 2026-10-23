;;; cfg-gen-op-treemacs-mode.el --- keybindings for treemacs-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Central place for treemacs keybindings.
;;
;; NOTE: We do NOT use general.el for the in-house treemacs (treemacs-in-house)
;; bindings below.  general.el does not implement correct support for
;; treemacs-mode (and the old custom treemacs evil state), so keys would not
;; bind reliably.  All in-house treemacs keys are installed with plain
;; `define-key' and `evil-define-key*'.
;;
;; Upstream (legacy) treemacs still uses general.el in the else-branch.
;;
;;; Code:

(require 'treemacs nil t)

;; ---------------------------------------------------------------------------
;; Common part (always): leader SPC 0 / 00
;; ---------------------------------------------------------------------------

(when (fboundp 'general-define-key)
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC"
   "0"  '(:ignore t :which-key "tree-smacs")
   "00" '(treemacs :which-key "treemacs")))

;; ---------------------------------------------------------------------------
;; Part dependent on built-in treemacs-in-house
;; ---------------------------------------------------------------------------
;;
;; No general.el here – see file header.  Bindings that used to live in
;; treemacs.el (mode map, mouse, evil) are installed below.

(defun cfg-gen-op-treemacs--install-inhouse-keys ()
  "Install all in-house treemacs keybindings on `treemacs-mode-map'."
  (when (and (bound-and-true-p treemacs-in-house)
             (boundp 'treemacs-mode-map)
             (keymapp treemacs-mode-map))
    (let ((map treemacs-mode-map))
      ;; Core navigation / actions
      (define-key map (kbd "RET") #'treemacs-RET-action)
      (define-key map (kbd "TAB") #'treemacs-TAB-action)
      (define-key map (kbd "<tab>") #'treemacs-TAB-action)
      (define-key map (kbd "g") #'treemacs-refresh)
      (define-key map (kbd "r") #'treemacs-refresh)
      (define-key map (kbd "q") #'treemacs-quit)
      (define-key map (kbd "n") #'treemacs-next-line)
      (define-key map (kbd "p") #'treemacs-previous-line)
      (define-key map (kbd "j") #'treemacs-next-line)
      (define-key map (kbd "k") #'treemacs-previous-line)
      (define-key map (kbd "h") #'treemacs-COLLAPSE-action)
      (define-key map (kbd "l") #'treemacs-RET-action)
      (define-key map (kbd "R") #'treemacs-rename-file)
      (define-key map (kbd "d") #'treemacs-delete-file)
      (define-key map (kbd "m") #'treemacs-move-file)
      (define-key map (kbd "ya") #'treemacs-copy-absolute-path-at-point)
      (define-key map (kbd "yr") #'treemacs-copy-relative-path-at-point)
      (define-key map (kbd "yn") #'treemacs-copy-filename-at-point)
      (define-key map (kbd "yf") #'treemacs-copy-file)
      (define-key map (kbd "cf") #'treemacs-create-file)
      (define-key map (kbd "cd") #'treemacs-create-dir)
      (define-key map (kbd "ov") #'treemacs-visit-node-vertical-split)
      (define-key map (kbd "oh") #'treemacs-visit-node-horizontal-split)
      (define-key map (kbd "oo") #'treemacs-visit-node-no-split)
      (define-key map (kbd "ox") #'treemacs-visit-node-in-external-application)
      ;; Toggle / local leader-style keys (replacing former general bindings)
      (define-key map (kbd "0") #'treemacs-quit)
      (define-key map (kbd ",0") #'treemacs-quit)
      ;; Mouse: 1st click highlights; click on highlighted node opens.
      (define-key map [down-mouse-1] #'ignore)
      (define-key map [drag-mouse-1] #'ignore)
      (define-key map [mouse-1] #'treemacs-leftclick-action)
      (define-key map [double-mouse-1] #'treemacs-doubleclick-action)
      (define-key map [triple-mouse-1] #'ignore)
      (define-key map [down-mouse-3] #'ignore)
      (define-key map [mouse-3] #'treemacs-rightclick-menu))

    ;; Evil: normal/visual/emacs on the same map (no custom treemacs state)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal treemacs-mode-map
        (kbd "j") #'treemacs-next-line
        (kbd "k") #'treemacs-previous-line
        (kbd "h") #'treemacs-COLLAPSE-action
        (kbd "l") #'treemacs-RET-action
        (kbd "g") #'treemacs-refresh
        (kbd "r") #'treemacs-refresh
        (kbd "q") #'treemacs-quit
        (kbd "R") #'treemacs-rename-file
        (kbd "d") #'treemacs-delete-file
        (kbd "m") #'treemacs-move-file
        (kbd "0") #'treemacs-quit
        (kbd ",0") #'treemacs
        (kbd "RET") #'treemacs-RET-action
        (kbd "TAB") #'treemacs-TAB-action)
      (evil-define-key* 'visual treemacs-mode-map
        (kbd "j") #'treemacs-next-line
        (kbd "k") #'treemacs-previous-line
        (kbd "q") #'treemacs-quit
        (kbd "0") #'treemacs-quit)
      (evil-define-key* 'emacs treemacs-mode-map
        (kbd "j") #'treemacs-next-line
        (kbd "k") #'treemacs-previous-line
        (kbd "q") #'treemacs-quit
        (kbd "0") #'treemacs-quit)
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps)))))

(if (bound-and-true-p treemacs-in-house)
    (progn
      ;; Install now if treemacs is already loaded, otherwise after load.
      (if (featurep 'treemacs)
          (cfg-gen-op-treemacs--install-inhouse-keys)
        (with-eval-after-load 'treemacs
          (cfg-gen-op-treemacs--install-inhouse-keys)))
      ;; Also re-apply on mode entry (covers buffers created before this file).
      (add-hook 'treemacs-mode-hook #'cfg-gen-op-treemacs--install-inhouse-keys))

  ;; ---------------------------------------------------------------------------
  ;; Part dependent on alternative upstream treemacs (legacy)
  ;; ---------------------------------------------------------------------------
  (progn
    (when (fboundp 'general-define-key)
      (general-define-key
       :states '(normal visual emacs)
       :keymaps 'override
       :prefix "SPC"
       "0o"  '(treemacs-switch-workspace :which-key "switch-workspace")
       "0w"  '(:ignore t :which-key "treemacs-workspaces")
       "0ww" '(treemacs-switch-workspace :which-key "switch-workspace")
       "0wa" '(treemacs-create-workspace :which-key "create-workspace")
       "0wd" '(treemacs-remove-workspace :which-key "remove-workspace")
       "0wr" '(treemacs-rename-workspace :which-key "rename-workspace")
       "0wf" '(treemacs-set-fallback-workspace :which-key "set-fallback-workspace")
       "0we" '(treemacs-edit-workspaces :which-key "edit-workspaces")
       "0wn" '(treemacs-next-workspaces :which-key "next-workspace")
       "0p"  '(:ignore t :which-key "treemacs-projects")
       "0pa" '(treemacs-add-project :which-key "add-project")
       "0pA" '(treemacs-add-project-to-workspace :which-key "add-project-to-workspace")
       "0pd" '(treemacs-remove-project-from-workspace :which-key "remove-project")
       "0pr" '(treemacs-rename-project :which-key "rename-project"))

      (general-define-key
       :states '(treemacs normal visual emacs)
       :keymaps 'treemacs-mode-map
       :major-modes 'treemacs-mode
       "0"  '(treemacs :which-key "treemacs"))

      (general-define-key
       :states '(treemacs normal visual emacs)
       :keymaps 'treemacs-mode-map
       :major-modes 'treemacs-mode
       :prefix ","
       "0"  '(treemacs :which-key "treemacs")
       "w"  '(:ignore t :which-key "treemacs-workspaces")
       "o"  '(treemacs-switch-workspace :which-key "switch-workspace")
       "ww" '(treemacs-switch-workspace :which-key "switch-workspace")
       "wa" '(treemacs-create-workspace :which-key "create-workspace")
       "wd" '(treemacs-remove-workspace :which-key "remove-workspace")
       "wr" '(treemacs-rename-workspace :which-key "rename-workspace")
       "wf" '(treemacs-set-fallback-workspace :which-key "set-fallback-workspace")
       "we" '(treemacs-edit-workspaces :which-key "edit-workspaces")
       "wn" '(treemacs-next-workspaces :which-key "next-workspace")
       "p"  '(:ignore t :which-key "treemacs-projects")
       "pa" '(treemacs-add-project :which-key "add-project-to-workspace")
       "pA" '(treemacs-add-project-to-workspace :which-key "add-project")
       "pd" '(treemacs-remove-project-from-workspace :which-key "remove-project")
       "pr" '(treemacs-rename-project :which-key "rename-project")))))

(provide 'cfg-gen-op-treemacs-mode)
;;; cfg-gen-op-treemacs-mode.el ends here
