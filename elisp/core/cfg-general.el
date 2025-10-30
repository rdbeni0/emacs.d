;;; cfg-general.el --- general.el, which-key and all keys -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for general.el
;;
;;; Code:

(use-package general
  :after evil
  :config

  ;; https://github.com/noctuid/general.el/issues/99
  ;; general-override-mode
  ;; :keymaps 'override
  ;; ^override evil keybindings
  (general-override-mode 1)

  ;; https://github.com/noctuid/general.el#automatic-key-unbinding
  ;; "To automatically prevent Key sequence starts with a non-prefix key errors without the need to explicitly unbind non-prefix keys, you can add (general-auto-unbind-keys) to your configuration file. This will advise define-key to unbind any bound subsequence of the KEY."
  (general-auto-unbind-keys)

  ;;;;;;;;;;;;;; Split whole general.el mapping into small pieces:

  (require 'cfg-gen-for-all-modes)
  (require 'cfg-gen-for-all-modes-fkeys)
  (require 'cfg-gen-for-many-modes)

  ;;; Load general.el for all modes (global scope) and for many modes (but not all; local scope)...
					; (cfg/load-all-el-in-directory (expand-file-name "elisp/cfg-general" user-emacs-directory))
					; (cfg/load-all-el-in-directory (expand-file-name "elisp/cfg-general/cfg-gen-core" user-emacs-directory))
  )

(provide 'cfg-general)
;;; cfg-general.el ends here
