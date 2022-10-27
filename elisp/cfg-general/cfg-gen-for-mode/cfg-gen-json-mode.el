;; general-json-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'json-mode-map
 :major-modes 'json-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 "=b" '(format-all-buffer :which-key "format-all-buffer")
 "=i" '(json-mode-beautify :which-key "jm-beautify")
 "=p" '(json-pretty-print-buffer :which-key "pretty-print-buffer")
 "=o" '(json-pretty-print-buffer-ordered :which-key "pretty-print-buffer-ordered")
 "=r" '(json-pretty-print-ordered :which-key "pretty-print-ordered")
 "p" '(jsons-print-path :which-key "jsons-print-path")
 "n" '(json-navigator-navigate-after-point :which-key "navigate-after-point")
 "r" '(json-navigator-navigate-region :which-key "navigate-region"))
