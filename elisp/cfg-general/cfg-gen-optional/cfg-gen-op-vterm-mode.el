;;; cfg-gen-op-vterm-mode.el --- general for vterm-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps '(vterm-mode-map)
 :major-modes 'term-mode
 "S-<up>" 'multi-vterm-rename-buffer
 "S-<down>" 'multi-vterm
 "S-<left>" 'multi-vterm-prev
 "S-<right>" 'multi-vterm-next
 "C-l" (lambda () (interactive) (vterm-clear) (vterm-clear-scrollback))
 "<tab>" 'vterm-send-tab ;; for zsh
 "<delete>" 'vterm-send-delete
 "<backspace>" 'vterm-send-backspace)

 (provide 'cfg-gen-op-vterm-mode)
;;; cfg-gen-op-vterm-mode.el ends here
