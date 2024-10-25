;;; cfg-gen-for-many-modes.el --- general.el for many modes -*- lexical-binding: t -*-

(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure lists - if you want to add any mode, do it here:
;; https://www.emacswiki.org/emacs/ListModification
;;
;; to add element (major mode) - use "append"
;; to remove element  (major mode) - use "seq-difference"
;; to copy whole list - use "copy-sequence"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common

(setq list-gen-mode '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode ssh-config-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode c++-mode nxml-mode groovy-mode jenkinsfile-mode nix-mode))

(setq list-gen-mode-map '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map c++-mode-map nxml-mode-map groovy-mode-map jenkinsfile-mode-map nix-mode-map))

(delete-dups list-gen-mode) ;; remove duplicates
(delete-dups list-gen-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(setq list-gen-mode-flycheck (seq-difference list-gen-mode '(ssh-config-mode jenkinsfile-mode fish-mode nix-mode)))
(setq list-gen-mode-map-flycheck (seq-difference list-gen-mode-map '(ssh-config-mode-map jenkinsfile-mode-map fish-mode-map nix-mode-map)))

;; format core
(setq list-gen-mode-format-core (seq-difference list-gen-mode '(ssh-config-mode perl-mode cperl-mode js-json-mode nxml-mode)))
(setq list-gen-mode-map-format-core (seq-difference list-gen-mode-map '(ssh-config-mode-map perl-mode-map cperl-mode-map js-json-mode-map nxml-mode-map)))

;; format optional
(setq list-gen-mode-format-optional (seq-difference list-gen-mode '(ssh-config-mode)))
(setq list-gen-mode-map-format-optional (seq-difference list-gen-mode-map '(ssh-config-mode-map)))
(setq list-gen-mode-format-optional (append list-gen-mode '(json-mode markdown-mode)))
(setq list-gen-mode-map-format-optional (append list-gen-mode-map '(json-mode-map markdown-mode-map)))

;; ffap
(setq list-gen-mode-ffap (append list-gen-mode '(conf-mode)))
(setq list-gen-mode-map-ffap (append list-gen-mode-map '(conf-mode-map)))

;; xref
(setq list-gen-mode-xref (copy-sequence list-gen-mode))
(setq list-gen-mode-map-xref (copy-sequence list-gen-mode-map))

;; comment (dwim)
(setq list-gen-mode-comment (append list-gen-mode '(conf-mode)))
(setq list-gen-mode-map-comment (append list-gen-mode-map '(conf-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'cfg-gen-for-many-modes)
;;; cfg-gen-for-many-modes.el ends here
