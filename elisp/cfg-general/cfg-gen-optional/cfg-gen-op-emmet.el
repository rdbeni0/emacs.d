;;; cfg-gen-op-emmet.el --- general.el for emmet -*- lexical-binding: t -*-

;; should be used with combination with web-mode:
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map) ;; only web-mode should be there, emmet-mode is not working
 :major-modes '(web-mode)
 :prefix ","
 "m"  '(emmet-expand-line :which-key "emmet-expand")
 "M"  '(emmet-preview :which-key "emmet-preview"))

(provide 'cfg-gen-op-emmet)
;;; cfg-gen-op-emmet.el ends here
