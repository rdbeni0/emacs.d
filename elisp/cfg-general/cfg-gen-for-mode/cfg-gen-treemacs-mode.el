;; general-treemacs-mode:

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
 "ww"  '(treemacs-switch-workspace :which-key "switch-workspace")
 "wa"  '(treemacs-create-workspace :which-key "create-workspace")
 "wd"  '(treemacs-remove-workspace :which-key "remove-workspace")
 "wr"  '(treemacs-rename-workspace :which-key "rename-workspace")
 "wf"  '(treemacs-set-fallback-workspace :which-key "set-fallback-workspace")
 "we"  '(treemacs-edit-workspaces :which-key "edit-workspaces")
 "wn"  '(treemacs-next-workspaces :which-key "next-workspace")
 "p"  '(:ignore t :which-key "treemacs-projects")
 "pa" '(treemacs-add-project :which-key "add-project-to-workspace")
 "pA" '(treemacs-add-project-to-workspace :which-key "add-project")
 "pd" '(treemacs-remove-project-from-workspace :which-key "remove-project")
 "pr" '(treemacs-rename-project :which-key "rename-project")
 )
