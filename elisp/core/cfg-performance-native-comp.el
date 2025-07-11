;;; cfg-performance-native-comp.el --- performance options and native compilation -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Basic setup for performance tweaks and options.
;; And also native compilation (emacs ver >= 28.5)
;;
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

;; manipulations with font rendering:
;; https://www.reddit.com/r/emacs/comments/14c4l8j/way_to_make_emacs_feel_smoother/
(setq jit-lock-stealth-time 1.25)
(setq jit-lock-stealth-nice 0.5) ;; Seconds between font locking.
(setq jit-lock-chunk-size 4096)
(setq jit-lock-defer-time 0)

;;  >>>>>>>>>>>>>>>>>> DEFUNS FOR COMPILATION OF PARTICULAR DIRECTORIES:
;; All below defuns should me executed manually via M-x or via CLI scripts:

(defun cfg/eln-compile-elisp ()
  "Compile ALL files inside \"~/.emacs.d/elisp\" (and subfolders) into eln.
  WARNING! Could cause errors and hang emacs."
  (interactive)
  (native-compile-async (expand-file-name "elisp/" user-emacs-directory) 2 t)
  )

(defun cfg/eln-compile-site-elisp ()
  "Compile all files inside \"~/.emacs.d/site-elisp\" into eln."
  (interactive)
  (native-compile-async (expand-file-name "elisp/site-elisp/" user-emacs-directory) 2 t))

;; BUG with native compilation - will be fixed in 30++:
;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup

(if (version< emacs-version "30.0")
    ;; if "M-x version" < 30.0:
    (progn
      (defun fixed-native-compile-async-skip-p
          (native-compile-async-skip-p file load selector)
	(let* ((naive-elc-file (file-name-with-extension file "elc"))
               (elc-file       (replace-regexp-in-string
				"\\.el\\.elc$" ".elc" naive-elc-file)))
          (or (gethash elc-file comp--no-native-compile)
              (funcall native-compile-async-skip-p file load selector))))

      (advice-add 'native-compile-async-skip-p
		  :around 'fixed-native-compile-async-skip-p)
      )
  ;; optional else:
  ;; ()
  )

;; >>>>>>>>>>>>>>>>>> OVERALL PERFORMANCE OPTIONS
;; see Doom Emacs for inspiration:
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; Other examples:
;; https://news.ycombinator.com/item?id=39190110
;; https://www.reddit.com/r/emacs/comments/r7qah6/emacs_is_bloat_and_memory_intensive/

;; Set garbage collection threshold to 8GB:
(setq gc-cons-threshold #x200000000)
;;(setq gc-cons-threshold most-positive-fixnum)

(setq read-process-output-max (* 1024 1024))

;; https://akrl.sdf.org/#orgc15a10d
;; When idle for 30sec run the GC no matter what:
;; This action ss to set a timer using run-with-idle-timer.
;; That means that every time Emacs will be idle for 30 secs we'll garbage collect once.
;; The assumption is that the probability that we are going to input a command exactly after 30 secs is rather low.

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar k-gc-timer
  (run-with-idle-timer 30 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))


(provide 'cfg-performance-native-comp)
;;; cfg-performance-native-comp.el ends here
