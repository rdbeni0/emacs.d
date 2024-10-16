;;; cfg-gen-op-dired-mode.el --- general.el for dired mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(dired-mode-map wdired-mode-map)
 :major-modes'(dired-mode wdired-mode)
 :prefix ","
 ","  '(dired-narrow :which-key "dired-narrow")
 "d"  '(:ignore t :which-key "dired-du")
 "dc" '(dired-du-count-sizes :which-key "count-sizes")
 "dr" '(dired-du-recompute-dir-size :which-key "recompute-dir-size")
 "dd" '(dired-du-mode :which-key "dired-du-mode")
 "dh" '(dired-du--toggle-human-readable :which-key "toggle-human-readable"))

(provide 'cfg-gen-op-dired-mode)
;;; cfg-gen-co-dired-mode.el ends here
