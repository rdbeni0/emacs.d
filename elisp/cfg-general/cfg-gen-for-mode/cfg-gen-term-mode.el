;; general-term-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(term-mode-map term-raw-map)
 :major-modes 'term-mode
 :prefix ","
 "f" '(find-file :which-key "find-file"))

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps '(term-mode-map term-raw-map)
 :major-modes 'term-mode
 "S-<up>" 'rename-buffer
 "S-<down>" 'cfg/multi-term-buffer-rn
 "S-<left>" 'multi-term-prev
 "S-<right>" 'multi-term-next
 "<delete>" 'term-send-del
 "<backspace>" 'term-send-backspace
 "<home>" 'term-send-home
 "<end>" 'term-send-end
 "<up>" 'term-send-up
 "<down>" 'term-send-down
 "<left>" 'term-send-left
 "<right>" 'term-send-right
 "C-c" 'term-interrupt-subjob
 "C-z" 'term-stop-subjob
 "C-." 'term-send-esc
 "C-," 'term-send-eof)
