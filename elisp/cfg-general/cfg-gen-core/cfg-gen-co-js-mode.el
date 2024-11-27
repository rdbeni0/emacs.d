;;; cfg-gen-co-js-mode.el --- general.el for js-mode and js-json-mode -*- lexical-binding: t -*-

;; js-json-mode:
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'js-json-mode-map
 :major-modes 'js-json-mode
 :prefix ",")

(provide 'cfg-gen-co-js-mode)
;;; cfg-gen-co-js-mode.el ends here
