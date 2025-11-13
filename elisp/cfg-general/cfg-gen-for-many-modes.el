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
;; common prog modes
;; do not use nix-mode below, it will be in conflict with nix-ts-mode
(setq list-gen-mode '(bash-ts-mode c++-mode c-mode cc-mode cperl-mode css-mode css-ts-mode emacs-lisp-mode fish-mode gfm-mode gfm-view-mode groovy-mode html-mode jenkinsfile-mode js-mode js-ts-mode json-mode json-ts-mode jsonc-mode jsonc-ts-mode lisp-interaction-mode lua-mode lua-ts-mode markdown-mode mhtml-mode nix-mode nix-ts-mode nxml-mode perl-mode perl-ts-mode-map php-mode php-ts-mode python-mode python-ts-mode sh-mode ssh-config-mode web-mode))
(setq list-gen-mode-map '(bash-ts-mode-map js-ts-mode-map json-ts-mode-map json-ts-mode-map jsonc-ts-mode-map perl-ts-mode-map python-ts-mode-map js-ts-mode-map c++-mode-map c-mode-map cc-mode-map cperl-mode-map css-mode-map css-ts-mode-map emacs-lisp-mode-map fish-mode-map gfm-mode-map gfm-view-mode-map groovy-mode-map html-mode-map jenkinsfile-mode-map js-mode-map json-mode-map jsonc-mode-map lisp-interaction-mode-map lua-mode-map lua-ts-mode-map markdown-mode-map mhtml-mode-map nix-mode-map nix-ts-mode-map nxml-mode-map perl-mode-map php-mode-map php-ts-mode-map python-mode-map sh-mode-map ssh-config-mode-map web-mode-map))

;; remove duplicates (if any)
(delete-dups list-gen-mode)
(delete-dups list-gen-mode-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-sitter workarounds:
(setq list-gen-mode
      (seq-difference list-gen-mode '(nix-mode)))

(setq list-gen-mode-map
      (seq-difference list-gen-mode-map '(nix-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conf-modes
(setq list-gen-mode-conf-mode '(conf-mode conf-unix-mode conf-windows-mode conf-xdefaults-mode conf-space-mode robots-txt-mode yaml-mode))
(setq list-gen-mode-conf-mode-map '(conf-mode-map conf-unix-mode-map conf-windows-mode-map conf-xdefaults-mode-map conf-space-mode-map robots-txt-mode-map yaml-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(setq list-gen-mode-flycheck (seq-difference list-gen-mode '(ssh-config-mode jenkinsfile-mode fish-mode)))
(setq list-gen-mode-map-flycheck (seq-difference list-gen-mode-map '(ssh-config-mode-map jenkinsfile-mode-map fish-mode-map)))

;; format core
(setq list-gen-mode-format-core (append (seq-difference list-gen-mode
							'(ssh-config-mode perl-mode cperl-mode js-json-mode nxml-mode markdown-mode gfm-mode))
					'(yaml-mode)))
(setq list-gen-mode-map-format-core (append
				     (seq-difference list-gen-mode-map
						     '(ssh-config-mode-map perl-mode-map cperl-mode-map js-json-mode-map nxml-mode-map markdown-mode-map gfm-mode-map))
				     '(yaml-mode-map)))

;; format optional
(setq list-gen-mode-format-optional (append
				     (seq-difference list-gen-mode '(ssh-config-mode))
				     '(json-mode yaml-mode)))
(setq list-gen-mode-map-format-optional (append
					 (seq-difference list-gen-mode-map '(ssh-config-mode-map))
					 '(json-mode-map yaml-mode-map)))

;; goto (ffap and imenu)
(setq list-gen-mode-ffap  (append list-gen-mode list-gen-mode-conf-mode))
(setq list-gen-mode-map-ffap  (append list-gen-mode-map list-gen-mode-conf-mode-map))
(setq list-gen-mode-ffap (append list-gen-mode-ffap '(org-mode text-mode)))
(setq list-gen-mode-map-ffap (append list-gen-mode-map-ffap '(org-mode-map text-mode-map)))

;; xref
(setq list-gen-mode-xref (copy-sequence (seq-difference list-gen-mode '(ssh-config-mode nxml-mode markdown-mode gfm-mode))))
(setq list-gen-mode-map-xref (copy-sequence (seq-difference list-gen-mode-map '(ssh-config-mode-map nxml-mode-map markdown-mode-map gfm-mode-map))))

;; comment (dwim)
(setq list-gen-mode-comment (append list-gen-mode list-gen-mode-conf-mode))
(setq list-gen-mode-map-comment (append list-gen-mode-map list-gen-mode-conf-mode-map))

;; text manipulations
(setq list-gen-mode-txtman (append (append list-gen-mode list-gen-mode-conf-mode) '(org-mode text-mode)))
(setq list-gen-mode-map-txtman (append (append list-gen-mode-map list-gen-mode-conf-mode-map) '(org-mode-map text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cfg-gen-for-many-modes)
;;; cfg-gen-for-many-modes.el ends here
