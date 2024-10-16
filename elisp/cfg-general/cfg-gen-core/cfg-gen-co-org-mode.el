;;; cfg-gen-co-org-mode.el --- general.el for org-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(org-mode-map org-static-blog-mode-map)
 :major-modes '(org-mode org-static-blog-mode)
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "c" '(org-ctrl-c-ctrl-c :which-key "org-ctrl-c-ctrl-c")
 "p" '(org-priority :which-key "org-priority")
 "g" '(org-goto :which-key "org-goto")
 "j" '(imenu :which-key "imenu")
 "." '(org-cycle :which-key "cycle")
 "A" '(org-attach :which-key "org-attach")

 "e" '(:ignore t :which-key "export")
 "ed" '(org-md-export-to-markdown :which-key "export-to-md")
 "eh" '(org-html-export-to-html :which-key "export-to-html")

 "i" '(:ignore t :which-key "insert")
 "ib" '(org-insert-structure-template :which-key "org-insert-structure-template")
 "id" '(org-insert-drawer :which-key "org-insert-drawer")
 "ie" '(org-set-effort :which-key "org-set-effort")
 "if" '(org-footnote-new :which-key "org-footnote-new")
 "ih" '(org-insert-heading :which-key "org-insert-heading")
 "iH" '(org-insert-heading-after-current :which-key "org-insert-heading-after-current")
 "ii" '(org-insert-item :which-key "org-insert-item")
 "il" '(org-insert-link :which-key "org-insert-link")
 "in" '(org-add-note :which-key "org-add-note")
 "ip" '(org-set-property :which-key "org-set-property")
 "is" '(org-insert-subheading :which-key "org-insert-subheading")
 "it" '(org-set-tags-command :which-key "org-set-tags-command")

 "b"  '(:ignore t :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle"))

(provide 'cfg-gen-co-org-mode)
;;; cfg-gen-co-org-mode.el ends here
