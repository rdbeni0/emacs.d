;;; cfg-gen-co-text-fundamental.el --- general.el for text and fundamental modes -*- lexical-binding: t -*-

;; javascript/json:
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(fundamental-mode-map text-mode-map)
 :major-modes '(fundamental-mode text-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(fundamental-mode-map text-mode-map)
 :major-modes '(fundamental-mode text-mode)
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))

(provide 'cfg-gen-co-text-fundamental)
;;; cfg-gen-co-text-fundamental.el ends here
