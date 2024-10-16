;;; cfg-gen-co-recentf.el --- general.el for recentf -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(recentf-dialog-mode-map)
 :major-modes '(recentf-dialog-mode)
 "/" 'evil-ex-search-forward
 "N" 'evil-ex-search-previous
 "n" 'evil-ex-search-next
)

(provide 'cfg-gen-co-recentf)
;;; cfg-gen-co-recentf.el ends here
