;;; cfg-gen-co-format.el --- general.el for code formatting -*- lexical-binding: t -*-

;; "shell-script-mode is an alias for ‘sh-mode’ in ‘sh-script.el’."
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-format-core
 :major-modes list-gen-mode-format-core
 :prefix ","
 "="    '(:ignore t :which-key "format")
 "=-"   '(cfg/built-in-format-via-indent :which-key "built-in-format"))

;; perl/cperl
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map)
 :major-modes '(perl-mode cperl-mode)
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "=-" '(cfg/built-in-format-perl :which-key "built-in-format")
 "=p" '(cfg/perltidy-format-buffer :which-key "perltidy-format-buffer")
 "=o" '(cfg/perltidy-format :which-key "perltidy-format")
 "=f" '(cfg/perltidy-format-function :which-key "perltidy-format-function"))

;; js-json-mode:
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'js-json-mode-map
 :major-modes 'js-json-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "=-" '(json-pretty-print-buffer :which-key "built-in-format")
 "=p" '(json-pretty-print-buffer :which-key "pretty-p-buffer")
 "=o" '(json-pretty-print-ordered :which-key "pretty-p-ordered")
 "=r" '(json-pretty-print-buffer-ordered :which-key "pretty-p-buffer-ordered"))

;; nxml-mode:
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'nxml-mode-map
 :major-modes 'nxml-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(cfg/xmllint-format-buffer :which-key "xmllint-format-buffer")
 "=-" '(cfg/built-in-format-nxml :which-key "built-in-format")
 "=p" '(cfg/xmllint-format-buffer :which-key "xmllint-format-buffer"))

(provide 'cfg-gen-co-format)
;;; cfg-gen-co-format.el ends here
