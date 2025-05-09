;;; cfg-gen-op-markdown-mode.el --- general.el for markdown-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(markdown-mode-map gfm-mode-map gfm-view-mode-map)
 :major-modes '(markdown-mode gfm-mode gfm-view-mode)
 :prefix ","

 "<down>" '(markdown-move-down :which-key "mv-down")
 "<up>" '(markdown-move-up :which-key "mv-up")
 "<right>" '(markdown-demote :which-key "demote")
 "<left>" '(markdown-promote :which-key "promote")
 
 "a" '(read-only-mode :which-key "toggle-read-only")

 "i" '(:ignore t :which-key "insert")
 "i-" '(markdown-insert-hr :which-key "hr")
 "i_" '(markdown-insert-footnote :which-key "footnote")
 "ib" '(markdown-insert-bold  :which-key "txt-bold")
 "ic" '(markdown-insert-code  :which-key "code")
 "id" '(markdown-insert-strike-through :which-key "strike-through")
 "ig" '(markdown-insert-gfm-checkbox :which-key "gfm-checkbox")
 "ii" '(markdown-insert-italic  :which-key "txt-italic")
 "ik" '(markdown-insert-kbd  :which-key "keyboard")
 "il" '(markdown-insert-link :which-key "link")
 "im" '(markdown-insert-image :which-key "image")
 "ip" '(markdown-insert-pre :which-key "preformatted")
 "is" '(markdown-insert-list-item :which-key "list-item")
 "iu" '(markdown-insert-uri :which-key "uri")
 "iw" '(markdown-insert-wiki-link :which-key "wiki-link")

 "r" '(:ignore t :which-key "regions")
 "rc" '(markdown-check-refs  :which-key "check-refs")
 "rd" '(markdown-kill-thing-at-point :which-key "del-element")
 "rl" '(markdown-cleanup-list-numbers :which-key "cleanup-list-numbers")
 "rp" '(markdown-pre-region :which-key "pre-region")
 "ru" '(markdown-unused-refs  :which-key "unused-refs")
 "rt" '(:ignore t :which-key "toggle")
 "rta" '(read-only-mode :which-key "toggle-read-only")
 "rtf" '(markdown-toggle-fontify-code-blocks-natively  :which-key "toggle-code-blocks-fonts")
 "rtg" '(markdown-toggle-gfm-checkbox  :which-key "toggle-gfm-checkbox")
 "rth" '(markdown-toggle-math  :which-key "toggle-math")
 "rti" '(markdown-toggle-inline-images  :which-key "toggle-images")
 "rtl" '(markdown-toggle-url-hiding  :which-key "toggle-url")
 "rtm" '(markdown-toggle-markup-hiding  :which-key "toggle-markup-hiding")
 "rtw" '(markdown-toggle-wiki-links  :which-key "toggle-wiki-links")

 "t" '(:ignore t :which-key "table")
 "t <down>"  '(markdown-table-move-row-down :which-key "mv-row-down")
 "t <up>"    '(markdown-table-move-row-up :which-key "mv-row-up")
 "t <right>" '(markdown-demote :which-key "mv-col-right")
 "t <left>" '(markdown-promote :which-key "mv-col-left")
 "t%" '(markdown-table-transpose :which-key "transpose")
 "tC" '(markdown-table-delete-column :which-key "delete-col")
 "tR" '(markdown-table-delete-row :which-key "delete-row")
 "tc" '(markdown-table-insert-column :which-key "insert-col")
 "to" '(markdown-table-convert-region :which-key "convert-region")
 "tr" '(markdown-table-insert-row :which-key "insert-row")
 "ts" '(markdown-table-sort-lines :which-key "sort-lines")
 "tt" '(markdown-insert-table :which-key "insert-table")

 "e" '(:ignore t :which-key "export")
 "ec" '(markdown-other-window :which-key "html-create")
 "ee" '(markdown-export :which-key "html-export")
 "eh" '(markdown-export-and-preview :which-key "export-html-preview")
 "el" '(markdown-live-preview-mode :which-key "live-preview-mode")
 "eo" '(markdown-open :which-key "open")
 "ep" '(markdown-preview :which-key "tmp-preview")

 "b" '(:ignore t :which-key "blocks")
 "b'"  '(markdown-edit-code-block :which-key "edit-code-block")
 "bf"  '(markdown-insert-foldable-block :which-key "insert-foldable-block")
 "bm"  '(markdown-mark-block :which-key "mark-block")
 "bq"  '(markdown-insert-blockquote :which-key "insert-blockquote")
 "br"  '(markdown-blockquote-region :which-key "blockquote-region")
 "b{"  '(markdown-backward-block :which-key "backward-block")
 "b}"  '(markdown-forward-block :which-key "forward-block")
 "bc"  '(markdown-insert-gfm-code-block :which-key "insert-gfm-code-block"))

;; gfm-view-mode - no prefix, it will override evil keymaps:
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'gfm-view-mode-map
 :major-modes 'gfm-view-mode
 "q" '(kill-buffer-and-window :which-key "quit")
 "Q" '(kill-buffer :which-key "kill-this-buffer"))

(provide 'cfg-gen-op-markdown-mode)
;;; cfg-gen-op-markdown-mode.el ends here
