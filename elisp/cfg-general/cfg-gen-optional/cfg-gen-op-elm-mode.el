;;; cfg-gen-op-elm-mode.el --- general.el for elm-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package general
  :functions
  (general-define-key))

(general-define-key
  :states '(normal visual emacs)
  :keymaps '(elm-mode-map elm-ts-mode-map)
  :major-modes '(elm-mode elm-ts-mode)
  :prefix ","
  "="  '(:ignore t :which-key "format")
  "=n" '(elm-format-buffer :which-key "elm-format-buffer"))

(provide 'cfg-gen-op-elm-mode)
;;; cfg-gen-op-elm-mode.el ends here
