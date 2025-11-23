;;; cfg-gen-op-ts-js.el --- general for TS/JS -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(typescript-ts-mode-map js-ts-mode-map)
 :major-modes '(typescript-ts-mode js-ts-mode)
 :prefix ","
 )

(provide 'cfg-gen-op-ts-js)
;;; cfg-gen-op-ts-js.el ends here
