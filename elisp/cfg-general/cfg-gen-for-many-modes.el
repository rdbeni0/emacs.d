;; comments:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map nxml-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode ssh-config-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode nxml-mode)
 :prefix ","
 ","  '(ffap :which-key "act_ffap")
 "k"  '(:ignore t :which-key "comments")
 "kl" '(comment-dwim :which-key "comment-dwim")
 "kk" '(mark-defun :which-key "mark-fun")
 "km" '(comment-region :which-key "comment-region")
 "kn" '(uncomment-region :which-key "uncomment-region")
 "kj" '(comment-line :which-key "comment-line")
 "kt" '(comment-kill :which-key "comment-kill")
 "<f5>"     '(:ignore t :which-key "completions")
 "<f5><f4>" '(company-files :which-key "company-files")
 "<f5><f5>" '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f6>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f7>" '(company-ispell :which-key "company-ispell")
 "<f5><f8>" '(completion-at-point :which-key "completion-at-point-capf"))

;; F-keys for all many modes (also in insert mode)

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map lisp-interaction-mode-map fundamental-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map nxml-mode-map)
 :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode lisp-interaction-mode fundamental-mode python-mode php-mode ssh-config-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode nxml-mode)
 "<f5><f4>" '(company-files :which-key "company-files")
 "<f5><f5>" '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f6>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f7>" '(company-ispell :which-key "company-ispell")
 "<f5><f8>" '(completion-at-point :which-key "completion-at-point-capf"))
