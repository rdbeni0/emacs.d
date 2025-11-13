;;; cfg-gen-op-treemacs-mode.el --- general.el for treemacs-mode in Emacs -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 "0"   '(:ignore t :which-key "tree-smacs") ;; tree-sitter + treemacs, please check cfg-gen-op-tree-sitter-mode
 "00"  '(treemacs :which-key "treemacs")
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

;; treemacs-mode itself:

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
 "pr" '(treemacs-rename-project :which-key "rename-project"))

(provide 'cfg-gen-op-treemacs-mode)
;;; cfg-gen-op-treemacs-mode.el ends here
