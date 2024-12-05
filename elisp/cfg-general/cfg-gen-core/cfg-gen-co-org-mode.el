;;; cfg-gen-co-org-mode.el --- general.el for org-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(org-mode-map org-static-blog-mode-map)
 :major-modes '(org-mode org-static-blog-mode)
 :prefix ","
 "c" '(org-ctrl-c-ctrl-c :which-key "org-ctrl-c-ctrl-c")
 "p" '(org-priority :which-key "priority")
 "g" '(org-goto :which-key "goto")
 "y" '(org-cycle :which-key "cycle")

 "r" '(:ignore t :which-key "regions")

 "t" '(:ignore t :which-key "table")
 "te" '(org-table-export :which-key "export")
 "tt" '(org-table-create :which-key "create")
 "ty" '(org-table-import :which-key "import")
 "ts" '(org-table-sort-lines :which-key "sort-lines")

 "e" '(:ignore t :which-key "export")
 "ed" '(org-md-export-to-markdown :which-key "export-to-md")
 "eh" '(org-html-export-to-html :which-key "export-to-html")

 "i" '(:ignore t :which-key "insert")
 "iH" '(org-insert-heading-after-current :which-key "heading-after-current")
 "ia" '(org-set-tags-command :which-key "tags-command")
 "ib" '(org-insert-structure-template :which-key "structure-template")
 "ic" '(org-attach :which-key "org-attach")
 "id" '(org-insert-drawer :which-key "drawer")
 "ie" '(org-set-effort :which-key "set-effort")
 "if" '(org-footnote-new :which-key "footnote-new")
 "ih" '(org-insert-heading :which-key "heading")
 "ii" '(org-insert-item :which-key "item")
 "il" '(org-insert-link :which-key "link")
 "in" '(org-add-note :which-key "add-note")
 "ip" '(org-set-property :which-key "property")
 "is" '(org-insert-subheading :which-key "subheading")
 "it" '(org-timer :which-key "timer")

 "b"  '(:ignore t :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle"))

(provide 'cfg-gen-co-org-mode)
;;; cfg-gen-co-org-mode.el ends here
