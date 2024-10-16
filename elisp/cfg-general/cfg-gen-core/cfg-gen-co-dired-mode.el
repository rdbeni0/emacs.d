;;; cfg-gen-co-dired-mode.el --- general.el for perl/cperl mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(dired-mode-map wdired-mode-map)
 :major-modes '(dired-mode wdired-mode)
 "q" 'kill-this-buffer)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(dired-mode-map wdired-mode-map)
 :major-modes '(dired-mode wdired-mode)
 :prefix ","
 "l"  '(cfg/cycle-dired-switches :which-key "cycle-dired-switches")
 "m"  '(wdired-change-to-wdired-mode :which-key "wdired-mode")
 "n"  '(wdired-exit :which-key "wdired-exit"))

(provide 'cfg-gen-co-dired-mode)
;;; cfg-gen-co-dired-mode.el ends here
