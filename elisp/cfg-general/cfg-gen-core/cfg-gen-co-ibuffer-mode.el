;;; cfg-gen-co-ibuffer-mode.el --- general.el for ibuffer-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(ibuffer-mode-map)
 :major-modes '(ibuffer-mode)
 "q"  'kill-buffer-and-window
 "Q"  'kill-this-buffer)

 (provide 'cfg-gen-co-ibuffer-mode)
;;; cfg-gen-co-ibuffer-mode.el ends here
