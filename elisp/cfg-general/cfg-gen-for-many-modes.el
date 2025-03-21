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
(setq list-gen-mode '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode ssh-config-mode fish-mode web-mode mhtml-mode html-mode css-mode js-mode c-mode cc-mode c++-mode nxml-mode groovy-mode jenkinsfile-mode nix-mode lisp-interaction-mode markdown-mode gfm-mode gfm-view-mode json-mode jsonc-mode lua-mode))

(setq list-gen-mode-map '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map ssh-config-mode-map fish-mode-map web-mode-map mhtml-mode-map html-mode-map css-mode-map js-mode-map c-mode-map cc-mode-map c++-mode-map nxml-mode-map groovy-mode-map jenkinsfile-mode-map nix-mode-map lisp-interaction-mode-map markdown-mode-map gfm-mode-map gfm-view-mode-map json-mode-map jsonc-mode-map lua-mode-map))

;; remove duplicates (if any)
(delete-dups list-gen-mode)
(delete-dups list-gen-mode-map)

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
(setq list-gen-mode-ffap (append list-gen-mode-ffap '(org-mode)))
(setq list-gen-mode-map-ffap (append list-gen-mode-map-ffap '(org-mode-map)))

;; xref
(setq list-gen-mode-xref (copy-sequence (seq-difference list-gen-mode '(ssh-config-mode nxml-mode markdown-mode gfm-mode))))
(setq list-gen-mode-map-xref (copy-sequence (seq-difference list-gen-mode-map '(ssh-config-mode-map nxml-mode-map markdown-mode-map gfm-mode-map))))

;; comment (dwim)
(setq list-gen-mode-comment (append list-gen-mode list-gen-mode-conf-mode))
(setq list-gen-mode-map-comment (append list-gen-mode-map list-gen-mode-conf-mode-map))

;; text manipulations
(setq list-gen-mode-txtman (append (append list-gen-mode list-gen-mode-conf-mode) '(org-mode)))
(setq list-gen-mode-map-txtman (append (append list-gen-mode-map list-gen-mode-conf-mode-map) '(org-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cfg-gen-for-many-modes)
;;; cfg-gen-for-many-modes.el ends here
