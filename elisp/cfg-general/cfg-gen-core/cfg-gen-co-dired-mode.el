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
 ";"  '(wdired-change-to-wdired-mode :which-key "wdired-mode")
 "'"  '(wdired-exit :which-key "wdired-exit")
 "C"  '(dired-do-copy :which-key "do-copy")
 "E"  '(dired-do-open :which-key "do-open")
 "="  '(dired-diff :which-key "diff")
 "M"  '(dired-do-chmod :which-key "do-chmod")
 "O"  '(dired-do-chown :which-key "do-chown")
 "T"  '(dired-do-touch :which-key "do-touch")
 "R"  '(dired-do-rename :which-key "do-rename")
 "D"  '(dired-do-delete :which-key "do-delete")
 "S"  '(dired-do-symlink :which-key "do-symlink")
 "Z"  '(dired-do-compress :which-key "do-compress")
 "H"  '(dired-do-hardlink :which-key "do-hardlink")
 "|"  '(dired-do-redisplay :which-key "do-redisplay")
 "+"  '(dired-create-directory :which-key "create-directory")
 "!"  '(dired-do-shell-command :which-key "do-shell-command")
 "m"  '(dired-mark :which-key "mark")
 "x"  '(dired-do-flagged-delete :which-key "do-flagged-delete")
 "u"  '(dired-umark :which-key "umark")
 "U"  '(dired-unmark-all-marks :which-key "unmark-all-marks")
 "t"  '(dired-toggle-marks :which-key "toggle-marks"))

;; dired marks * (rozne opcje do zaznaczania)

(provide 'cfg-gen-co-dired-mode)
;;; cfg-gen-co-dired-mode.el ends here
