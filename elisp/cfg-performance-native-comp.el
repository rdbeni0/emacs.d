;;; cfg-performance-native-comp.el --- performance options and native compilation -*- lexical-binding: t -*-
;;; Commentary:

;; Basic setup for performance tweaks and options.
;; And also native compilation (emacs ver >= 28.5)

;;; Code:

;; >>>>>>>>>>>>>>>>>> BYTE COMPILATION ...
;; https://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
;; 'Surprisingly, it doesn't add much to my startup time (unless something needs to be compiled).'
;; (byte-recompile-directory (expand-file-name "site-elisp" user-emacs-directory) 0)
;; (byte-recompile-directory (expand-file-name "elisp" user-emacs-directory) 0)
;; ^ BUT warning, it should not be used with native-comp (errors)!

;; for native-comp sometimes old *elc need to be removed:
;; https://www.reddit.com/r/emacs/comments/myej3z/the_nativecompilation_branch_was_just_merged_into/
;; "One snag I hit after trying to recompile native compilation after using it for a while
;; was needing to delete the old .elc files in the source directory"

;; >>>>>>>>>>>>>>>>>> ... or NATIVE COMPILATION
;; https://news.ycombinator.com/item?id=24117853

;; Optimization level for native compilation, a number between -1 and 3.
;; -1 functions are kept in bytecode form and no native compilation is performed.
;;  0 native compilation is performed with no optimizations.
;;  1 light optimizations.
;;  2 max optimization level fully adherent to the language semantic.
;;  3 max optimization level, to be used only when necessary.
;;    Warning: with 3, the compiler is free to perform dangerous optimizations.

(setq comp-speed 2)
(setq native-comp-speed 2)

;; additional options:

(setq native-comp-deferred-compilation t) ; should be t, and default is t
(setq native-comp-async-query-on-exit t) ; no risky

;; aggressive performance experiment:
;; default is 0, but this value could be risky (the best option is default value)
;; (setq native-comp-async-jobs-number 4) ; very risky, but possibility of better performance during startup

;; no warnings when compilation is ongoing:
(setq native-comp-async-report-warnings-errors nil)
(setq comp-async-report-warnings-errors nil)

;; kill unnecessary compilation buffer:
;; please check cfg- file with tempbuf configuration or try to experiment with (kill-buffer ) ...

;;  >>>>>>>>>>>>>>>>>> DEFUNS FOR COMPILATION OF PARTICULAR DIRECTORIES:
;; All below defuns should me executed manually via M-x or via CLI scripts:

(defun cfg/eln-compile-elisp ()
  "Compile all files inside \"~/.emacs.d/elisp\" into eln. WARNING! Could cause errors and hang emacs."
  (interactive)
  (native-compile-async (expand-file-name "elisp/" user-emacs-directory) 2 t))

(defun cfg/eln-compile-site-elisp ()
  "Compile all files inside \"~/.emacs.d/site-elisp\" into eln."
  (interactive)
  (native-compile-async (expand-file-name "site-elisp/" user-emacs-directory) 2 t))

(defun cfg/eln-compile-elisp-general ()
  "Compile all files inside \"~/.emacs.d/elisp/cfg-general\" into eln."
  (interactive)
  (native-compile-async (expand-file-name "elisp/cfg-general" user-emacs-directory) 2 t))

(defun cfg/eln-compile-only-safe ()
  "Compile all \"safe\" files - should not cause any errors."
  (interactive)
  ;; (cfg/eln-compile-elisp) ;; unfortunately this is NOT SAFE - 20210814 - issues with some cfg- files
  ;; (cfg/eln-compile-elisp-general) ;; 20220910: unfortunately as above...
  (cfg/eln-compile-site-elisp)
  (message "Compilation has been started. Please check reults in the dedicated buffer!"))

;; >>>>>>>>>>>>>>>>>> OVERALL PERFORMANCE OPTIONS
;; see Doom Emacs for inspiration: https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

(setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(provide 'cfg-performance-native-comp)
;;; cfg-performance-native-comp.el ends here
