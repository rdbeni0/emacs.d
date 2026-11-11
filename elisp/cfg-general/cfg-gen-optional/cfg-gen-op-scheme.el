;;; cfg-gen-op-scheme.el --- general.el for various scheme modes -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package general
  :functions
  (general-define-key))

(general-define-key
  :states '(normal visual emacs)
  :keymaps '(scheme-mode-map geiser-mode-map)
  :major-modes '(scheme-mode geiser-mode)
  :prefix ","
  "="  '(:ignore t :which-key "format")
  "==" '(cfg/format-smart :which-key "format-smart")

  "p"   '(:ignore t :which-key "REPL")
  "pp"  '(run-geiser :which-key "run-geiser/repl")
  "ps"  '(geiser-repl-switch :which-key "repl-switch")
  "pq"  '(cfg/geiser-quit-repl :which-key "quit-repl")
  "pr"  '(geiser-restart-repl :which-key "restart-repl")

  "e"   '(:ignore t :which-key "eval")
  "ee"  '(geiser-eval-last-sexp :which-key "eval-last-sexp")
  "ed"  '(geiser-eval-definition :which-key "eval-definition")
  "eb"  '(geiser-eval-buffer :which-key "eval-buffer")
  "er"  '(geiser-eval-region :which-key "eval-region")

  "d"   '(:ignore t :which-key "doc")
  "dd"  '(geiser-doc-symbol-at-point :which-key "doc-symbol-at-point")
  "dm"  '(geiser-doc-module :which-key "doc-module")
  "ds"  '(geiser-doc-look-up-manual :which-key "doc-look-up-manual")

  "g"   '(:ignore t :which-key "goto")
  "gd"  '(geiser-edit-symbol-at-point :which-key "go to definition")
  "gm"  '(geiser-edit-module-at-point :which-key "edit-module")
  "gb"  '(pop-tag-mark :which-key "go back (pop tag mark)"))

(general-define-key
  :states '(normal visual emacs)
  :keymaps '(geiser-repl-mode-map)
  :major-modes '(geiser-repl-mode)
  "q" 'cfg/geiser-quit-repl
  "Q" 'kill-buffer-and-window)

(general-define-key
  :states '(normal visual emacs)
  :keymaps '(geiser-repl-mode-map)
  :major-modes '(geiser-repl-mode)
  :prefix ","

  "q"  '(cfg/geiser-quit-repl :which-key "quit-repl")
  "Q"  '(kill-buffer-and-window :which-key "kill-repl")
  "x"  '(kill-buffer-and-window :which-key "kill-repl")

  "e"   '(:ignore t :which-key "eval")
  "ee"  '(geiser-repl-maybe-send :which-key "repl-maybe-send")
  "em"  '(geiser-repl-set-module :which-key "repl-set-module")

  "p"   '(:ignore t :which-key "REPL")
  "pc"  '(geiser-repl-clear-buffer :which-key "repl-clear-buffer")
  "pr"  '(geiser-restart-repl :which-key "restart-repl")
  "pq"  '(cfg/geiser-quit-repl :which-key "quit-repl")
  "pQ"  '(kill-buffer-and-window :which-key "kill-repl")
  "px"  '(kill-buffer-and-window :which-key "kill-repl")

  "c"   '(:ignore t :which-key "comint")
  "cp"  '(comint-previous-input :which-key "previous-input")
  "cn"  '(comint-next-input :which-key "next-input")
  "cr"  '(comint-history-isearch-backward :which-key "history-isearch-backward")

  "d"   '(:ignore t :which-key "doc")
  "dd"  '(geiser-doc-symbol-at-point :which-key "doc-symbol-at-point")

  "g"   '(:ignore t :which-key "goto")
  "gd"  '(geiser-edit-symbol :which-key "edit-symbol")
  "gm"  '(geiser-edit-module :which-key "edit-module")
  "gb"  '(pop-tag-mark :which-key "go back"))


(general-define-key
  :states '(normal visual emacs)
  :keymaps 'geiser-debug-mode-map
  :major-modes 'geiser-debug-mode
  :prefix ","

  "d"   '(:ignore t :which-key "debugger actions")
  "dq"  '(geiser-debug-quit :which-key "quit debugger")
  "ds"  '(geiser-debug-step :which-key "step into / next frame")

  "g"   '(:ignore t :which-key "goto source")
  "go"  '(geiser-debug-goto-source :which-key "jump to error source line"))

(provide 'cfg-gen-op-scheme)
;;; cfg-gen-op-scheme.el ends here
