;;; cfg-gen-op-tabbar.el --- general.el for tabbar in Emacs -*- lexical-binding: t -*-

;; no space + no which-key
(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global
 "<S-next>" 'tabbar-backward
 "<S-prior>" 'tabbar-forward
 "<S-home>" 'tabbar-mode
 ;;   "<header-line> <mouse-1>" '(tabbar-press-home :wk t)
 ;;   "<header-line> <mouse-2>" '(tabbar-press-home :wk t)
 ;;   "<header-line> <mouse-3>" '(tabbar-press-home :wk t)
 "<header-line> <mouse-9>" 'tabbar-forward-group
 "<header-line> <drag-mouse-9>" 'tabbar-forward-group
 "<header-line> <mouse-8>" 'tabbar-backward-group
 "<header-line> <drag-mouse-8>" 'tabbar-backward-group
 )

;; space as leader-key + which-key
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 "<mouse-1>" '(tabbar-mode :which-key "tbb-mode")
 "<mouse-3>" '(tabbar-press-home :which-key "tbb-home")
 "<mouse-2>" '(tabbar-backward-group :which-key "tbb-backward")
 "<home>" 'tabbar-mode
 "<next>" 'tabbar-backward
 "<prior>" 'tabbar-forward
 ;; windows
 "wt <home>" '(tabbar-mode :which-key "tabbar-mode")
 "wt <next>" '(tabbar-backward :which-key "tabbar-backward")
 "wt <prior>" '(tabbar-forward :which-key "tabbar-forward"))

(provide 'cfg-gen-op-tabbar)
;;; cfg-gen-op-tabbar.el ends here
