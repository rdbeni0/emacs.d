;; general-web-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode css-mode js-mode)
 :prefix ","
 "j"  '(:ignore t :which-key "js")
 "t"  '(:ignore t :which-key "styles")
 "ts" '(css-lookup-symbol :which-key "css-lookup-symbol"))
