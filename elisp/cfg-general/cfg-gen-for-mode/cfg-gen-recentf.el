;; recentf-dialog-mode:

 (general-define-key
  :states '(normal visual emacs)
  :keymaps '(recentf-dialog-mode-map)
  :major-modes '(recentf-dialog-mode)
  "/" 'evil-ex-search-forward
  "N" 'evil-ex-search-previous
  "n" 'evil-ex-search-next
)
