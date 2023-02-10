;; general-org-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(org-mode-map org-static-blog-mode-map)
 :major-modes '(org-mode org-static-blog-mode)
 :prefix ","
 "p" '(org-priority :which-key "org-priority")
 "j" '( :which-key "imenu")
 "," '(org-ctrl-c-ctrl-c :which-key "org-ctrl-c-ctrl-c")
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

 "l"   '(:ignore t :which-key "org-static-blog")
 "ld"  '(org-static-blog-mode :which-key "osb-mode")
 "lh"  '(org-static-blog-publish :which-key "osb-publish")
 "ll"  '(org-static-blog-publish-file :which-key "osb-publish-file")
 "la"  '(org-static-blog-open-matching-publish-file :which-key "osb-open-matching-pub-file")
 "ln"  '(org-static-blog-open-next-post :which-key "osb-open-next-post")
 "lm"  '(org-static-blog-open-previous-post :which-key "osb-open-prev-post")
 "lo"  '(org-static-blog-create-new-post :which-key "osb-create-new-post")
 "lj"  '(org-static-blog-create-new-draft :which-key "osb-create-new-draft")

 "b" '(:ignore t :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle")

 "o" '(:ignore t :which-key "org-roam")
 "ol"  '(org-roam-buffer-toggle :which-key "buffer-toggle")
 "of"  '(org-roam-node-find :which-key "node-find")
 "oi"  '(org-roam-node-insert :which-key "node-insert"))
