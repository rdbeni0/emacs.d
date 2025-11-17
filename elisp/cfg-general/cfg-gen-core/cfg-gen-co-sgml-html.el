;;; cfg-gen-co-sgml-html.el --- general.el for sgml-mode and html and mhtml -*- lexical-binding: t -*-

;; web-mode: sgml
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map html-ts-mode-map sgml-mode-map nxml-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode html-ts-mode sgml-mode nxml-mode)
 :prefix ","
 "g"  '(:ignore t :which-key "sgml")

 "ga" '(sgml-attributes :which-key "attributes")
 "gb" '(sgml-skip-tag-backward :which-key "skip-backward")
 "gf" '(sgml-skip-tag-forward :which-key "skip-forward")
 "gd" '(sgml-delete-tag :which-key "delete-tag")
 "gt" '(sgml-tag :which-key "tag")
 "gn" '(sgml-name-char :which-key "name-char")
 "g8" '(sgml-name-8bit-mode :which-key "name-8bit")
 "g?" '(sgml-tag-help :which-key "help")
 "g/" '(sgml-close-tag :which-key "close-tag")
 "g<" '(sgml-skip-tag-backward :which-key "skip-backward")
 "g>" '(sgml-skip-tag-forward :which-key "skip-forward")
 "gD" '(sgml-delete-tag :which-key "delete-tag")
 "gT" '(sgml-tags-invisible :which-key "tags-invisible")

 "h"  '(:ignore t :which-key "htmlm")

 "hp" '(html-paragraph :which-key "paragraph")
 "hs" '(html-autoview-mode :which-key "autoview")
 "hv" '(browse-url-of-buffer :which-key "browse-url")

 "h1" '(html-headline-1 :which-key "headline-1")
 "h2" '(html-headline-2 :which-key "headline-2")
 "h3" '(html-headline-3 :which-key "headline-3")
 "h4" '(html-headline-4 :which-key "headline-4")
 "h5" '(html-headline-5 :which-key "headline-5")
 "h6" '(html-headline-6 :which-key "headline-6")

 "h#" '(html-id-anchor :which-key "id-anchor")
 "h-" '(html-horizontal-rule :which-key "hrule")
 "hc" '(html-checkboxes :which-key "checkboxes")
 "hf" '(html-href-anchor-file :which-key "href-file")
 "hh" '(html-href-anchor :which-key "href")
 "hi" '(html-image :which-key "image")
 "hj" '(html-line :which-key "line-break")
 "hl" '(html-list-item :which-key "list-item")
 "hn" '(html-name-anchor :which-key "name-anchor")
 "ho" '(html-ordered-list :which-key "olist")
 "hr" '(html-radio-buttons :which-key "radio")
 "hu" '(html-unordered-list :which-key "ulist"))

(provide 'cfg-gen-co-sgml-html)
;;; cfg-gen-co-sgml-html.el ends here
