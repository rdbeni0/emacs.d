;;; cfg-op-dumbjump.el --- configfuration for dumbjump package-*- lexical-binding: t -*-
;;; Commentary:
;;
;; dumb-jump - Dumb Jump is an Emacs "jump to definition" package with support for 50+ programming languages that favors "just working".
;; https://github.com/jacktasia/dumb-jump
;;
;;; Code:

(use-package dumb-jump
  :ensure t
  :hook ((prog-mode . cfg/dumb-jump-activate))
  :defines
  (xref-show-definitions-function)
  :functions
  (dumb-jump-xref-activate
   xref-show-definitions-completing-read)
  :init (defun cfg/dumb-jump-activate ()
          (interactive)
	      (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
	      (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :config
  ;; https://github.com/jacktasia/dumb-jump#configuration

  ;; https://github.com/BurntSushi/ripgrep
  (setq dumb-jump-prefer-searcher 'rg)

  ;; Following symbolic links
  (setq dumb-jump-rg-search-args "--pcre2 --follow")
  ;; For grep:
  (setq dumb-jump-grep-args "-REn")

  (defun cfg-/dumb-jump-extra-search-paths-function (lang proj-root)
    "Return additional paths extracted env."
    ;;
    ;; Lua:
    ;;
    (when (string= lang "lua")
      (let ((package-path
             (or (getenv "LUA_PATH_5_5")
                 (getenv "LUA_PATH_5_4")
                 (getenv "LUA_PATH_5_3")
                 (getenv "LUA_PATH_5_2")
                 (getenv "LUA_PATH_5_1")
                 (getenv "LUA_PATH")))
            paths)
        (when (and package-path
                   (not (string-empty-p package-path)))
          (dolist (template (split-string package-path ";" t))
            ;; Convert:
            ;;   /path/to/?.lua       -> /path/to
            ;;   /path/to/?/init.lua   -> /path/to
            ;;   ./?.lua              -> PROJ-ROOT
            (let ((dir
                   (cond
                    ((string-match "\\`\\(.*\\)/?/\\?\\.lua\\'" template)
                     (match-string 1 template))
                    ((string-match "\\`\\(.*\\)/?/init\\.lua\\'" template)
                     (match-string 1 template))
                    (t
                     (replace-regexp-in-string
                      "/?\\?.*" ""
                      template)))))
              (when (and dir
                         (not (string-empty-p dir)))
                (setq dir
                      (if (file-name-absolute-p dir)
                          (expand-file-name dir)
                        (expand-file-name dir proj-root)))
                (when (file-directory-p dir)
                  ;; exclude /nix/store
                  (not (string-prefix-p "/nix/store/" dir))
                  ;;
                  (push dir paths))))))
        (delete-dups (nreverse paths)))))

  (setq dumb-jump-extra-search-paths-function #'cfg-/dumb-jump-extra-search-paths-function))

(provide 'cfg-op-dumbjump)
;;; cfg-op-dumbjump.el ends here
