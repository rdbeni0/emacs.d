;;; cfg-gen-co-comment-mark.el --- general.el for comments and marks -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-comment
 :major-modes list-gen-mode-comment
 :prefix ","
 "k"  '(:ignore t :which-key "comments")
 "kl" '(comment-dwim :which-key "comment-dwim")
 "kk" '(mark-defun :which-key "mark-fun")
 "km" '(comment-region :which-key "comment-region")
 "kn" '(uncomment-region :which-key "uncomment-region")
 "kj" '(comment-line :which-key "comment-line")
 "kt" '(comment-kill :which-key "comment-kill"))

(provide 'cfg-gen-co-comment-mark)
;;; cfg-gen-co-comment-mark.el ends here
