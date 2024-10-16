;;; cfg-gen-op-magit.el --- general.el for magit in Emacs -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "gb" '(magit-blame :which-key "magit-blame")
 "gc" '(magit-clone :which-key "magit-clone")
 "gi" '(magit-init :which-key "magit-init")
 "gs" '(magit-status :which-key "magit-status")
 "gm" '(magit-dispatch :which-key "magit-dispatch")
 "gL" '(magit-list-repositories :which-key "magit-list-repositories")
 "gS" '(magit-stage-file :which-key "magit-stage-file")
 "gU" '(magit-unstage-file :which-key "magit-unstage-file")
 "gr" '(magit-refresh :which-key "magit-refresh")
 "gd" '(magit-diff :which-key "magit-diff")
 "gf" '(magit-find-file :which-key "magit-find-file"))

;; general-magit-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'magit-mode-map
 :major-modes 'magit-mode
 "e" 'magit-diff)

(provide 'cfg-gen-op-magit)
;;; cfg-gen-op-magit.el ends here
