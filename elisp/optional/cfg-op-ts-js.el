;;; cfg-op-ts-js.el --- configfuration for ssh via tramp and sudo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; load general.el and keybindings:
(require 'cfg-gen-op-ts-js)

;; flycheck: eslint for JS and TS
(flycheck-add-mode 'javascript-eslint 'typescript-mode)
(flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
(flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'js-ts-mode)

;;;; Examples of settings:
;; Config:
;; (setq flycheck-eslint-args '("--config" "/full/path/to/somewhere/.eslintrc.json"))
;; Ignore pattern:
;; (setq flycheck-eslint-args '("--ignore-pattern" "dist/" "--ignore-pattern" "node_modules/"))

(provide 'cfg-op-ts-js)
;;; cfg-op-ts-js.el ends here
