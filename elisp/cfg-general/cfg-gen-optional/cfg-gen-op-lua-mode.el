;;; cfg-gen-op-lua-mode.el --- general.el for lua-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package general
  :functions
  (general-define-key))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(lua-mode-map lua-ts-mode-map)
 :major-modes '(lua-mode lua-ts-mode)
 :prefix ","
 )

(provide 'cfg-gen-op-lua-mode)
;;; cfg-gen-op-lua-mode.el ends here
