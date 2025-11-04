;;; cfg-gen-co-txt-manipulations.el --- general.el for various text manipulations -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-txtman
 :major-modes list-gen-mode-txtman
 :prefix ","
 "."   '(:ignore t :which-key "txt-man")
 ".%"  '(string-rectangle :which-key "string-rectangle")
 ".'"  '(transpose-words :which-key "swap-words")
 ".;"  '(sort-words :which-key "sort-words")
 ".A"  '(align-regexp :which-key "align-regexp")
 ".G"  '(sort-paragraphs :which-key "sort-paragraphs")
 ".J"  '(cfg/join-lines-in-region :which-key "join-lines")
 ".T"  '(cfg/toggle-case-active :which-key "toggle-case-active")
 ".U"  '(cfg/unix2dos :which-key "unix2dos")
 ".a"  '(align :which-key "align")
 ".b"  '(delete-blank-lines :which-key "del-blank-lines")
 ".c"  '(center-region :which-key "center-region")
 ".d"  '(cfg/downcase-region :which-key "downcase")
 ".e"  '(cfg/duplicate-current-line-or-region :which-key "dup-line-or-reg")
 ".g"  '(fill-paragraph :which-key "fill-paragraph")
 ".j"  '(cfg/join-lines-in-region-add-spc :which-key "join-lines-spc")
 ".l"  '(kill-whole-line :which-key "kill-line")
 ".n"  '(transpose-lines :which-key "swap-lines")
 ".p"  '(delete-duplicate-lines :which-key "del-dup-lines")
 ".r"  '(reverse-region :which-key "reverse-lines")
 ".s"  '(sort-lines :which-key "sort-lines")
 ".t"  '(cfg/toggle-case :which-key "toggle-case")
 ".u"  '(cfg/dos2unix :which-key "dos2unix")
 ".w"  '(delete-trailing-whitespace :which-key "delete-tr-whitespace")
 ".y"  '(untabify :which-key "untabify")
 ".i"  '(whitespace-mode :which-key "whitespace-mode")
 ".0"  '(whitespace-newline-mode :which-key "whitespace-newline-mode")
 ".z"  '(cfg/capitalize-region :which-key "capitalize"))

(provide 'cfg-gen-co-txt-manipulations)
;;; cfg-gen-co-txt-manipulations.el ends here
