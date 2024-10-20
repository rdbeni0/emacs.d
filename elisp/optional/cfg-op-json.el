;;; cfg-op-json.el --- configfuration for json  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with json.
;;
;;; Code:

(use-package json-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-json-mode))

(use-package json-navigator
  :ensure t
  :after hierarchy
  :config
  ;; TODO: update config
  )

(provide 'cfg-op-json)
;;; cfg-op-json.el ends here
