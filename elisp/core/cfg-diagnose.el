;;; cfg-diagnose.el --- diagnose Emacs conffiguration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Various tools for diagnosing Emacs config.
;; Inspired by doom Emacs doctor.el
;; For example:
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/sh/doctor.el
;; https://github.com/doomemacs/doomemacs/tree/master/modules/lang/erlang/doctor.el
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/elixir/doctor.el
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/javascript/doctor.el
;;
;;; Code:

(defun cfg/diagnose-exec-find ()
  "Check that all CLI commands exist or not and make raport in dedicated buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*exec-find*")))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    ;; (set-buffer buffer)
    (with-current-buffer "*exec-find*" (insert "executable-find raport:\n\n")
                         ;; (unless (executable-find "foo") (insert "foo : NOT FOUND\n"))
                         (if (executable-find "rg") (insert "rg : FOUND\n") (insert "rg (ripgrep) : NOT FOUND\n"))
                         (if (executable-find "git") (insert "git : FOUND\n") (insert "git : NOT FOUND\n"))
                         (if (executable-find "find") (insert "find : FOUND\n") (insert "find : NOT FOUND\n"))
                         (if (executable-find "fd") (insert "fd : FOUND\n") (insert "fd : NOT FOUND. Project filtering will be corrupted!\n"))
                         (if (executable-find "clang-format") (insert "clang-format : FOUND\n") (insert "clang-format : NOT FOUND : c/c++ formatting will not work!\n"))
                         (if (executable-find "make") (insert "make : FOUND\n") (insert "make : NOT FOUND : makefile will not be used!\n"))
                         (if (executable-find "xmllint") (insert "xmllint : FOUND\n") (insert "xmllint : NOT FOUND : formatting and nxml-mode will not work correctly!\n"))
                         (if (executable-find "perltidy") (insert "perltidy : FOUND\n") (insert "perltidy : NOT FOUND : formatting and cperl-mode will not work correctly!\n"))
                         ;; lsp:
                         (if (executable-find "pyright") (insert "pyright : FOUND\n") (insert "pyright : NOT FOUND : lsp for python will not work!\n"))
                         (if (executable-find "ccls") (insert "ccls : FOUND\n") (insert "ccls : NOT FOUND : lsp for c/c++ will not work!\n"))
                         (if (executable-find "clangd") (insert "clangd : FOUND\n") (insert "clangd : NOT FOUND : lsp for c/c++ will not work!\n"))
                         (if (file-exists-p "~/.local/share/fonts/NFM.ttf") (insert "file/font NFM.ttf : FOUND\n") (insert "font NFM.ttf : NOT FOUND : doom-modeline will not work correctly: https://github.com/seagle0128/doom-modeline ! \n"))

                         )
    (switch-to-buffer buffer)
    ;; (pop-to-buffer buffer)
    ))

(provide 'cfg-diagnose)
;;; cfg-diagnose.el ends here
