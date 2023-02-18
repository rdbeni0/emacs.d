;; general-markdown-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(markdown-mode-map gfm-mode-map)
 :major-modes '(markdown-mode gfm-mode)
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 "=b" '(format-all-buffer :which-key "format-all-buffer")

 "i" '(:ignore t :which-key "insert")
 "il" '(markdown-insert-link :which-key "insert-link")
 "im" '(markdown-insert-image :which-key "insert-image")
 "it" '(markdown-insert-table :which-key "insert-table")
 "ic" '(markdown-insert-code  :which-key "insert-code")

 "e" '(:ignore t :which-key "export")
 "ee" '(markdown-export-and-preview :which-key "export-html-preview")
 "ep" '(markdown-preview :which-key "tmp-preview")

 "b" '(:ignore t :which-key "blocks")
 "bq"  '(markdown-insert-blockquote :which-key "insert-blockquote")
 "bc"  '(markdown-insert-gfm-code-block :which-key "insert-gfm-code-block"))
