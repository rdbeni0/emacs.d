;; comments:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode fish-mode)
 :prefix ","
 "k"  '(:ignore t :which-key "comments")
 "kl" '(comment-dwim :which-key "comment-dwim")
 "kk" '(mark-defun :which-key "mark-fun")
 "km" '(comment-region :which-key "comment-region")
 "kn" '(uncomment-region :which-key "uncomment-region")
 "kj" '(comment-line :which-key "comment-line")
 "kt" '(comment-kill :which-key "comment-kill"))
