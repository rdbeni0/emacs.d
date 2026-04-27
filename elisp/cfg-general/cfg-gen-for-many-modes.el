;;; cfg-gen-for-many-modes.el --- general.el for many modes -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

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

(defvar list-gen-mode
  '(
    bash-ts-mode
    c++-mode
    c-mode
    cc-mode
    cperl-mode
    css-mode
    css-ts-mode
    emacs-lisp-mode
    esql-mode
    fish-mode
    gfm-mode
    gfm-view-mode
    go-ts-mode
    go-mod-ts-mode
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
    latex-mode
    mhtml-mode
    makefile-gmake-mode
    makefile-mode
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
    typescript-ts-mode)
  "Base list of main prog modes for general.el.")

(defvar list-gen-mode-map
  '(
    bash-ts-mode-map
    c++-mode-map
    c-mode-map
    cc-mode-map
    cperl-mode-map
    css-mode-map
    css-ts-mode-map
    emacs-lisp-mode-map
    esql-mode-map
    fish-mode-map
    gfm-mode-map
    gfm-view-mode-map
    go-ts-mode-map
    go-mod-ts-mode-map
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
    latex-mode-map
    mhtml-mode-map
    makefile-gmake-mode-map
    makefile-mode-map
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
    typescript-ts-mode-map)
  "Base list of keymaps corresponding to the modes in `list-gen-mode`.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONF-MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar list-gen-mode-conf-mode
  '(conf-mode conf-unix-mode conf-windows-mode conf-xdefaults-mode
              conf-space-mode robots-txt-mode yaml-mode yaml-ts-mode
              conf-colon-mode conf-neon-mode neon-mode text-mode
              conf-javaprop-mode)
  "Base list of configuration modes.")

(defvar list-gen-mode-map-conf-mode
  '(conf-mode-map conf-unix-mode-map conf-windows-mode-map conf-xdefaults-mode-map
                  conf-space-mode-map robots-txt-mode-map yaml-mode-map yaml-ts-mode-map
                  conf-colon-mode-map conf-neon-mode-map neon-mode-map text-mode-map
                  conf-javaprop-mode-map)
  "Base list of keymaps for configuration modes.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLEANUP AND INITIAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode (delete-dups list-gen-mode))
(setq list-gen-mode-map (delete-dups list-gen-mode-map))

(defvar list-gen-mode-flycheck nil)
(defvar list-gen-mode-map-flycheck nil)

(defvar list-gen-mode-format-core nil)
(defvar list-gen-mode-map-format-core nil)

(defvar list-gen-mode-format-optional nil)
(defvar list-gen-mode-map-format-optional nil)

(defvar list-gen-mode-comment nil)
(defvar list-gen-mode-map-comment nil)

(defvar list-gen-mode-ffap nil)
(defvar list-gen-mode-map-ffap nil)

(defvar list-gen-mode-txtman nil)
(defvar list-gen-mode-map-txtman nil)

(defvar list-gen-mode-xref nil)
(defvar list-gen-mode-map-xref nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TREE-SITTER WORKAROUNDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove from list:
(setq list-gen-mode
      (seq-difference list-gen-mode '(
                                      ;; foo-ts-mode
                                      )))

;; Remove from list:
(setq list-gen-mode-map
      (seq-difference list-gen-mode-map '(
                                          ;; foo-ts-mode-map
                                          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove from list:
(setq list-gen-mode-flycheck
      (seq-difference list-gen-mode
		              '(ssh-config-mode
                        jenkinsfile-mode
                        fish-mode
                        web-mode
                        latex-mode
                        makefile-mode
                        makefile-gmake-mode)))

(setq list-gen-mode-map-flycheck
      (seq-difference list-gen-mode-map
		              '(ssh-config-mode-map
                        jenkinsfile-mode-map
                        fish-mode-map
                        web-mode-map
                        latex-mode-map
                        makefile-mode-map
                        makefile-gmake-mode-map)))

;; Add to list:
(setq list-gen-mode-flycheck
      (append list-gen-mode-flycheck
              '(yaml-mode
                yaml-ts-mode
                org-mode)))

(setq list-gen-mode-map-flycheck
      (append list-gen-mode-map-flycheck
              '(yaml-mode-map
                yaml-ts-mode-map
                org-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORMAT CORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-format-core (append
				                 ;; Remove from list:
				                 (seq-difference list-gen-mode
						                         '(ssh-config-mode
                                                   perl-mode
                                                   cperl-mode
                                                   js-json-mode
                                                   nxml-mode
                                                   markdown-mode
                                                   makefile-mode
                                                   makefile-gmake-mode
                                                   gfm-mode))
				                 ;; Add to list:
				                 '(yaml-mode yaml-ts-mode)))

(setq list-gen-mode-map-format-core (append
				                     ;; Remove from list:
				                     (seq-difference list-gen-mode-map
						                             '(ssh-config-mode-map
                                                       perl-mode-map
                                                       cperl-mode-map
                                                       js-json-mode-map
                                                       nxml-mode-map
                                                       markdown-mode-map
                                                       makefile-mode-map
                                                       makefile-gmake-mode-map
                                                       gfm-mode-map))
				                     ;; Add to list:
				                     '(yaml-mode-map yaml-ts-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORMAT OPTIONAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-format-optional (append
				                     ;; Remove from list:
				                     (seq-difference list-gen-mode
                                                     '(ssh-config-mode
                                                       web-mode
                                                       latex-mode
                                                       makefile-mode
                                                       makefile-gmake-mode
                                                       esql-mode))
				                     ;; Add to list:
				                     '(json-mode yaml-mode yaml-ts-mode)))

(setq list-gen-mode-map-format-optional (append
					                     ;; Remove from list:
					                     (seq-difference list-gen-mode-map
                                                         '(ssh-config-mode-map
                                                           web-mode-map
                                                           latex-mode-map
                                                           makefile-mode-map
                                                           makefile-gmake-mode-map
                                                           esql-mode-map))
					                     ;; Add to list:
					                     '(json-mode-map yaml-mode-map yaml-ts-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GOTO (FFAP AND IMENU)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-ffap  (append list-gen-mode list-gen-mode-conf-mode))
(setq list-gen-mode-map-ffap  (append list-gen-mode-map list-gen-mode-map-conf-mode))
(setq list-gen-mode-ffap (append list-gen-mode-ffap '(org-mode text-mode)))
(setq list-gen-mode-map-ffap (append list-gen-mode-map-ffap '(org-mode-map text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XREF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-xref
      (copy-sequence
       ;; Remove from list:
       (seq-difference list-gen-mode
                       '(ssh-config-mode
                         nxml-mode
                         markdown-mode
                         latex-mode
                         makefile-mode
                         makefile-gmake-mode
                         gfm-mode))))

(setq list-gen-mode-map-xref
      (copy-sequence
       ;; Remove from list:
       (seq-difference list-gen-mode-map
                       '(ssh-config-mode-map
                         nxml-mode-map
                         markdown-mode-map
                         latex-mode-map
                         makefile-mode-map
                         makefile-gmake-mode-map
                         gfm-mode-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMMENTS (DWIM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add to list:
(setq list-gen-mode-comment
      (append list-gen-mode list-gen-mode-conf-mode))

(setq list-gen-mode-map-comment
      (append list-gen-mode-map list-gen-mode-map-conf-mode))

;; Remove from list:
(setq list-gen-mode-comment
      (seq-difference list-gen-mode-comment '(text-mode)))

(setq list-gen-mode-map-comment
      (seq-difference list-gen-mode-map-comment '(text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TEXT MANIPULATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq list-gen-mode-txtman
      (append (append list-gen-mode list-gen-mode-conf-mode) '(org-mode text-mode)))

(setq list-gen-mode-map-txtman
      (append (append list-gen-mode-map list-gen-mode-map-conf-mode) '(org-mode-map text-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cfg-gen-for-many-modes)
;;; cfg-gen-for-many-modes.el ends here
