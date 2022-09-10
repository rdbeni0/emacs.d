;; general-magit-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'magit-mode-map
 :major-modes 'magit-mode
 "e" 'vdiff-magit-dwim
 "E" 'vdiff-magit)
