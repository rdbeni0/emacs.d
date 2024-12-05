;;; cfg-gen-op-org-mode.el --- general.el for org-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(org-mode-map org-static-blog-mode-map)
 :major-modes '(org-mode org-static-blog-mode)
 :prefix ","

 "s"   '(:ignore t :which-key "org-static-blog")
 "sd"  '(org-static-blog-mode :which-key "osb-mode")
 "sl"  '(org-static-blog-publish :which-key "osb-publish")
 "sf"  '(org-static-blog-publish-file :which-key "osb-publish-file")
 "sa"  '(org-static-blog-open-matching-publish-file :which-key "osb-open-matching-pub-file")
 "sn"  '(org-static-blog-open-next-post :which-key "osb-open-next-post")
 "sm"  '(org-static-blog-open-previous-post :which-key "osb-open-prev-post")
 "so"  '(org-static-blog-create-new-post :which-key "osb-create-new-post")
 "sj"  '(org-static-blog-create-new-draft :which-key "osb-create-new-draft")

 "o"  '(:ignore t :which-key "org-roam")
 "ol"  '(org-roam-buffer-toggle :which-key "buffer-toggle")
 "of"  '(org-roam-node-find :which-key "node-find")
 "oi"  '(org-roam-node-insert :which-key "node-insert"))

(provide 'cfg-gen-op-org-mode)
;;; cfg-gen-op-org-mode.el ends here
