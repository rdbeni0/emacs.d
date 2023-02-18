;; general-web-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode css-mode js-mode)
 :prefix ","
 "a"  '(cfg/ffap :which-key "ffap")
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 "=b" '(format-all-buffer :which-key "format-all-buffer")
 "j"  '(:ignore t :which-key "js")
 "t"  '(:ignore t :which-key "styles")
 "ts" '(css-lookup-symbol :which-key "css-lookup-symbol"))
