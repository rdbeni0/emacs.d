;;; cfg-gen-op-projectile.el --- general.el for webpaste in Emacs -*- lexical-binding: t -*-

(general-define-key
 :prefix "SPC"
 :states '(normal visual emacs)
 :keymaps 'override
 "p" '(:keymap projectile-command-map :wk "projects")
 "px"  '(:ignore t :which-key "exec")
 "p4"  '(:ignore t :which-key "oth-window")
 "p5"  '(:ignore t :which-key "oth-frame")
 "pA"  '(projectile-add-known-project :which-key "add-known-project")
 "fyC" '(cfg/projectile-copy-file-path-with-line-column :which-key "projectile-copy-file-path-with-line-column")
 "fyD" '(cfg/projectile-copy-directory-path :which-key "projectile-copy-directory-path")
 "fyL" '(cfg/projectile-copy-file-path-with-line :which-key "projectile-copy-file-path-with-line")
 "fyY" '(cfg/projectile-copy-file-path :which-key "projectile-copy-file-path"))

;; OBSOLETE AND OPTIONAL CODE:

;; (general-define-key
;;  :states '(normal visual emacs)
;;  :keymaps 'override
;;  :prefix "SPC"
;;  "po"  '(:ignore t :which-key "projectile")
;;  "poT" '(projectile-test-project :which-key "poe-test-project")
;;  "pe"  '(projectile-edit-dir-locals :which-key "poe-edit-dir-locals")
;;  "poa" '(projectile-toggle-between-implementation-and-test :which-key "poe-toggle-between-implementation-and-test")
;;  "poR" '(projectile-replace :which-key "poe-replace")
;;  "pog" '(projectile-find-tag :which-key "poe-find-tag")
;;  "poG" '(projectile-regenerate-tags :which-key "poe-regenerate-tags")
;;  "pot" '(treemacs-projectile :which-key "treemacs-projectile")
;;  "poK" '(projectile-add-known-project :which-key "poe-add-known-project")
;;  "poM" '(projectile-remove-known-project :which-key "poe-remove-known-project")
;;  "poI" '(projectile-invalidate-cache :which-key "poe-invalidate-cache")
;;  "por" '(projectile-recentf :which-key "poe-recentf")
;;  "p;g" '(projectile-grep :which-key "poe-grep")
;;  "p;a" '(projectile-ag :which-key "poe-ag")
;;  "p;r" '(projectile-ripgrep :which-key "poe-ripgrep"))

;; EXPERIMENTAL AND OLD CODE:

;; (define-key evil-normal-state-map (kbd "SPC p") 'projectile-map)
;; (general-def '(normal visual emacs) projectile-mode :definer 'minor-mode "\\" projectile-command-map)

;; (general-define-key
;;  "p" '( :prefix "SPC" :states '(normal visual emacs) :keymap projectile-command-map :package projectile))

(provide 'cfg-gen-op-projectile)
;;; cfg-gen-op-projectile.el ends here
