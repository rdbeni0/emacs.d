;; general-dired-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'dired-mode-map
 :major-modes 'dired-mode
 "^" (lambda () (interactive) (find-alternate-file ".."))
 "<RET>" (lambda () (interactive) (dired-find-alternate-file))
 "q" 'kill-this-buffer)

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'dired-mode-map
 :major-modes 'dired-mode
 :prefix ","
 ","  '(dired-narrow :which-key "dired-narrow")
 "l"  '(cfg/cycle-dired-switches :which-key "cycle-dired-switches")
 "d"  '(:ignore t :which-key "dired-du")
 "dc" '(dired-du-count-sizes :which-key "count-sizes")
 "dr" '(dired-du-recompute-dir-size :which-key "recompute-dir-size")
 "dd" '(dired-du-mode :which-key "dired-du-mode")
 "dh" '(dired-du--toggle-human-readable :which-key "toggle-human-readable"))
