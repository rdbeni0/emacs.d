;;; cfg-gen-co-js-mode.el --- general.el for js-mode and js-json-mode -*- lexical-binding: t -*-

;; javascript/json:
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(js-json-mode-map js-ts-mode-map js-mode-map)
 :major-modes '(js-json-mode js-ts-mode js-mode)
 :prefix ",")

(provide 'cfg-gen-co-js-mode)
;;; cfg-gen-co-js-mode.el ends here
