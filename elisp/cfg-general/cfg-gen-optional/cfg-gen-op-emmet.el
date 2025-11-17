;;; cfg-gen-op-emmet.el --- general.el for emmet -*- lexical-binding: t -*-

;; should be used with combination with web-mode:
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map html-ts-mode-map css-mode-map css-ts-mode-map js-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode html-ts-mode css-mode css-ts-mode js-mode)
 :prefix ","
 "m"  '(emmet-expand-line :which-key "emmet-expand")
 "M"  '(emmet-preview :which-key "emmet-preview"))

(provide 'cfg-gen-op-emmet)
;;; cfg-gen-op-emmet.el ends here
