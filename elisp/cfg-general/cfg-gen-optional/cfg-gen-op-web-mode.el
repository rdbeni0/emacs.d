;;; cfg-gen-op-web-mode.el --- general for web-mode -*- lexical-binding: t -*-

;; web-mode only and formatting
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map)
 :major-modes '(web-mode)
 :prefix ","
 "==" '(web-mode-buffer-indent :which-key "web-mode-buffer-indent")
 "=]" '(cfg/twig-cs-fixer-fix-buffer :which-key "twig-cs-fixer-fix-buffer")
 "=[" '(cfg/twig-cs-fixer-lint-buffer :which-key "twig-cs-fixer-lint-buffer")
 "w"  '(:ignore t :which-key "twig-cs-fixer")
 "w]" '(cfg/twig-cs-fixer-fix-buffer :which-key "twig-cs-fixer-fix-buffer")
 "w[" '(cfg/twig-cs-fixer-lint-buffer :which-key "twig-cs-fixer-lint-buffer"))

;; formatting
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map html-mode-map html-ts-mode-map nxml-mode-map)
 :major-modes '(web-mode html-mode html-ts-mode nxml-mode)
 :prefix ","
 "=0" '(web-mode-buffer-indent :which-key "web-mode-buffer-indent"))

;; web-mode and html
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map html-ts-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode html-ts-mode)
 :prefix ","
 "f"  '(web-mode-fold-or-unfold :which-key "wm-fold")
 "s"  '(web-mode-snippet-insert :which-key "snippet")
 "x"  '(web-mode-mark-and-expand :which-key "mark-and-expand")
 "o"  '(web-mode-comment-or-uncomment :which-key "un/comment")
 "ko"  '(web-mode-comment-or-uncomment :which-key "un/comment")

 "d"  '(:ignore t :which-key "dom")
 "da" '(web-mode-dom-apostrophes-replace :which-key "apostrophes-replace")
 "dd" '(web-mode-dom-errors-show :which-key "errors-show")
 "de" '(web-mode-dom-entities-replace :which-key "entities-replace")
 "dn" '(web-mode-dom-normalize :which-key "normalize")
 "dq" '(web-mode-dom-quotes-replace :which-key "quotes-replace")
 "dt" '(web-mode-dom-traverse :which-key "traverse")
 "dx" '(web-mode-dom-xpath :which-key "xpath")

 "b"  '(:ignore t :which-key "block")
 "bb" '(web-mode-block-beginning :which-key "beginning")
 "bc" '(web-mode-block-close :which-key "close")
 "be" '(web-mode-block-end :which-key "end")
 "bk" '(web-mode-block-kill :which-key "kill")
 "bn" '(web-mode-block-next :which-key "next")
 "bp" '(web-mode-block-previous :which-key "previous")
 "bs" '(web-mode-block-select :which-key "select")

 "e"  '(:ignore t :which-key "html-element")
 "e+" '(web-mode-element-extract :which-key "extract")
 "e-" '(web-mode-element-contract :which-key "contract")
 "e/" '(web-mode-element-close :which-key "close")
 "eI" '(web-mode-element-insert-at-point :which-key "insert-at-point")
 "ea" '(web-mode-element-content-select :which-key "content-select")
 "eb" '(web-mode-element-beginning :which-key "beginning")
 "ec" '(web-mode-element-clone :which-key "clone")
 "ed" '(web-mode-element-child :which-key "child")
 "ee" '(web-mode-element-end :which-key "end")
 "ef" '(web-mode-element-children-fold-or-unfold :which-key "children-fold-or-unfold")
 "ei" '(web-mode-element-insert :which-key "insert")
 "ek" '(web-mode-element-kill :which-key "kill")
 "em" '(web-mode-element-mute-blanks :which-key "mute-blanks")
 "en" '(web-mode-element-next :which-key "next")
 "ep" '(web-mode-element-previous :which-key "previous")
 "er" '(web-mode-element-rename :which-key "rename")
 "es" '(web-mode-element-select :which-key "select")
 "et" '(web-mode-element-transpose :which-key "transpose")
 "eu" '(web-mode-element-parent :which-key "parent")
 "ev" '(web-mode-element-vanish :which-key "vanish")
 "ew" '(web-mode-element-wrap :which-key "wrap")

 "t"  '(:ignore t :which-key "html-tag")
 "ta" '(web-mode-tag-attributes-sort :which-key "attributes-sort")
 "tb" '(web-mode-tag-beginning :which-key "beginning")
 "te" '(web-mode-tag-end :which-key "end")
 "tm" '(web-mode-tag-match :which-key "match")
 "tn" '(web-mode-tag-next :which-key "next")
 "tp" '(web-mode-tag-previous :which-key "previous")
 "ts" '(web-mode-tag-select :which-key "select")

 "a"  '(:ignore t :which-key "html-attr")
 "ab" '(web-mode-attribute-beginning :which-key "beginning")
 "ae" '(web-mode-attribute-end :which-key "end")
 "ai" '(web-mode-attribute-insert :which-key "insert")
 "ak" '(web-mode-attribute-kill :which-key "kill")
 "an" '(web-mode-attribute-next :which-key "next")
 "ap" '(web-mode-attribute-previous :which-key "previous")
 "as" '(web-mode-attribute-select :which-key "select")
 "at" '(web-mode-attribute-transpose :which-key "transpose"))

;; web-mode and html + css
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map html-ts-mode-map css-mode-map css-ts-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode html-ts-mode css-mode css-ts-mode)
 :prefix ","
 "i"  '(web-mode-whitespaces-show :which-key "toggle-whitespaces")
 "c"  '(:ignore t :which-key "css/styles")
 "cs" '(css-lookup-symbol :which-key "css-lookup-symbol")
 "cc" '(css-cycle-color-format :which-key "cycle-color-format"))

;; folding via "evil-mode" currently is not working in "web-mode"
;; no space + which-key + no which-key : normal mode
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map)
 :major-modes '(web-mode)
 "za" '(web-mode-fold-or-unfold :which-key "fold-or-unfold")
 "zo" '(web-mode-fold-or-unfold :which-key "fold-or-unfold")
 "zc" '(web-mode-fold-or-unfold :which-key "fold-or-unfold"))

(provide 'cfg-gen-op-web-mode)
;;; cfg-gen-op-web-mode.el ends here
