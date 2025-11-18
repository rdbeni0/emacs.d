;;; cfg-gen-for-many-modes.el --- general.el for many modes -*- lexical-binding: t -*-

(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure lists - if you want to add any mode, do it here:
;; https://www.emacswiki.org/emacs/ListModification
;;
;; to add element (major mode) - use "append"
;; to remove element  (major mode) - use "seq-difference"
;; to copy whole list - use "copy-sequence"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMMON PROG MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode
      '(
  	bash-ts-mode
	c++-mode
        c-mode
        cc-mode
        cperl-mode
        css-mode
        css-ts-mode
        emacs-lisp-mode
        fish-mode
        gfm-mode
        gfm-view-mode
        groovy-mode
        html-mode
        html-ts-mode
        jenkinsfile-mode
        js-mode
        js-ts-mode
        json-mode
        json-ts-mode
        jsonc-mode
        jsonc-ts-mode
        lisp-interaction-mode
        lua-mode
        lua-ts-mode
        markdown-mode
        mhtml-mode
        nix-mode
        nix-ts-mode
        nxml-mode
        perl-mode
        perl-ts-mode
        php-mode
        php-ts-mode
        python-mode
        python-ts-mode
        sh-mode
        ssh-config-mode
        web-mode
        typescript-ts-mode
        js-ts-mode
        ))

(setq list-gen-mode-map
      '(
	bash-ts-mode-map
        c++-mode-map
        c-mode-map
        cc-mode-map
        cperl-mode-map
        css-mode-map
        css-ts-mode-map
        emacs-lisp-mode-map
        fish-mode-map
        gfm-mode-map
        gfm-view-mode-map
        groovy-mode-map
        html-mode-map
        html-ts-mode-map
        jenkinsfile-mode-map
        js-mode-map
        js-ts-mode-map
        json-mode-map
        json-ts-mode-map
        jsonc-mode-map
        jsonc-ts-mode-map
        lisp-interaction-mode-map
        lua-mode-map
        lua-ts-mode-map
        markdown-mode-map
        mhtml-mode-map
 	nix-mode-map
        nix-ts-mode-map
        nxml-mode-map
        perl-mode-map
        perl-ts-mode-map
        php-mode-map
        php-ts-mode-map
        python-mode-map
        python-ts-mode-map
        sh-mode-map
        ssh-config-mode-map
        web-mode-map
        typescript-ts-mode-map
        js-ts-mode-map
        ))

;; remove duplicates (if any)
(delete-dups list-gen-mode)
(delete-dups list-gen-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TREE-SITTER WORKAROUNDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove from list:
(setq list-gen-mode
      (seq-difference list-gen-mode '(perl-mode cperl-mode)))

;; Remove from list:
(setq list-gen-mode-map
      (seq-difference list-gen-mode-map '(perl-mode-map cperl-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONF-MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-conf-mode '(conf-mode conf-unix-mode conf-windows-mode conf-xdefaults-mode conf-space-mode robots-txt-mode yaml-mode yaml-ts-mode conf-colon-mode conf-neon-mode neon-mode text-mode))
(setq list-gen-mode-conf-mode-map '(conf-mode-map conf-unix-mode-map conf-windows-mode-map conf-xdefaults-mode-map conf-space-mode-map robots-txt-mode-map yaml-mode-map yaml-ts-mode-map conf-colon-mode-map conf-neon-mode-map neon-mode-map text-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove from list:
(setq list-gen-mode-flycheck
      (seq-difference list-gen-mode
		      '(ssh-config-mode jenkinsfile-mode fish-mode web-mode)))
(setq list-gen-mode-map-flycheck
      (seq-difference list-gen-mode-map
		      '(ssh-config-mode-map jenkinsfile-mode-map fish-mode-map web-mode-map)))

;; Add to list:
(setq list-gen-mode-flycheck
      (append list-gen-mode-flycheck '(yaml-mode yaml-ts-mode)))

(setq list-gen-mode-map-flycheck
      (append list-gen-mode-map-flycheck '(yaml-mode-map yaml-ts-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORMAT CORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-format-core (append
				 ;; Remove from list:
				 (seq-difference list-gen-mode
						 '(ssh-config-mode perl-mode cperl-mode js-json-mode nxml-mode markdown-mode gfm-mode))
				 ;; Add to list:
				 '(yaml-mode yaml-ts-mode)))

(setq list-gen-mode-map-format-core (append
				     ;; Remove from list:
				     (seq-difference list-gen-mode-map
						     '(ssh-config-mode-map perl-mode-map cperl-mode-map js-json-mode-map nxml-mode-map markdown-mode-map gfm-mode-map))
				     ;; Add to list:
				     '(yaml-mode-map yaml-ts-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORMAT OPTIONAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-format-optional (append
				     ;; Remove from list:
				     (seq-difference list-gen-mode '(ssh-config-mode web-mode))
				     ;; Add to list:
				     '(json-mode yaml-mode yaml-ts-mode)))
(setq list-gen-mode-map-format-optional (append
					 ;; Remove from list:
					 (seq-difference list-gen-mode-map '(ssh-config-mode-map web-mode-map))
					 ;; Add to list:
					 '(json-mode-map yaml-mode-map yaml-ts-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GOTO (FFAP AND IMENU)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-ffap  (append list-gen-mode list-gen-mode-conf-mode))
(setq list-gen-mode-map-ffap  (append list-gen-mode-map list-gen-mode-conf-mode-map))
(setq list-gen-mode-ffap (append list-gen-mode-ffap '(org-mode text-mode)))
(setq list-gen-mode-map-ffap (append list-gen-mode-map-ffap '(org-mode-map text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XREF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-xref
      (copy-sequence
       (seq-difference list-gen-mode '(ssh-config-mode nxml-mode markdown-mode gfm-mode))))
(setq list-gen-mode-map-xref
      (copy-sequence
       (seq-difference list-gen-mode-map '(ssh-config-mode-map nxml-mode-map markdown-mode-map gfm-mode-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMMENTS (DWIM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-comment (append list-gen-mode list-gen-mode-conf-mode))
(setq list-gen-mode-map-comment (append list-gen-mode-map list-gen-mode-conf-mode-map))

;; Remove from list:
(setq list-gen-mode-comment
      (seq-difference list-gen-mode-comment '(text-mode)))

;; Remove from list:
(setq list-gen-mode-map-comment
      (seq-difference list-gen-mode-map-comment '(text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TEXT MANIPULATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-txtman (append (append list-gen-mode list-gen-mode-conf-mode) '(org-mode text-mode)))
(setq list-gen-mode-map-txtman (append (append list-gen-mode-map list-gen-mode-conf-mode-map) '(org-mode-map text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cfg-gen-for-many-modes)
;;; cfg-gen-for-many-modes.el ends here
