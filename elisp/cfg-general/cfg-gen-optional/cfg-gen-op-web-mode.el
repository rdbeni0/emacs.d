;;; cfg-gen-op-web-mode.el --- general for web-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode css-mode js-mode)
 :prefix ","
 "o"  '(web-mode-fold-or-unfold :which-key "fold")
 "i"  '(web-mode-snippet-insert :which-key "snippet")
 "w"  '(web-mode-whitespaces-show :which-key "toggle-whitespaces")
 "j"  '(:ignore t :which-key "js")
 "t"  '(:ignore t :which-key "styles")
 "ts" '(css-lookup-symbol :which-key "css-lookup-symbol")
 "=\\" '(web-mode-buffer-indent :which-key "web-mode-buffer-indent"))

(provide 'cfg-gen-op-web-mode)
;;; cfg-gen-op-web-mode.el ends here
