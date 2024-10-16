;;; cfg-gen-op-webpaste.el --- general.el for webpaste in Emacs -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "aw"   '(:ignore t :which-key "webpaste")
 "awk"  '(webpaste-paste-buffer :which-key "webpaste-paste-buffer")
 "awl"  '(webpaste-paste-region :which-key "webpaste-paste-region"))

(provide 'cfg-gen-op-webpaste)
;;; cfg-gen-op-webpaste.el ends here
