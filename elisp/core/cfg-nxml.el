;;; cfg-nxml.el --- configfuration for xml  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; XML files (nxml)
;;
;;; Code:

(defun cfg/xml-xsd-validate (xsd-schema)
  "Validate the xml file with schema and xmllint."
  (interactive (list (read-file-name "Select XSD Schema: ")))
  (setq validate-file (buffer-file-name))
  (save-window-excursion
    (let ((buffer (get-buffer-create "*XSD Validator*")))
      (set-buffer buffer)
      (nxml-mode)
      (setq-local rng-validate-mode nil)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (pop-to-buffer buffer)
      (setq xml-valid-cmd (format "xmllint --schema %s --noout %s" xsd-schema validate-file))
      (shell-command xml-valid-cmd "*XSD Validator*")
      (setq buffer-read-only t)
      (bury-buffer)))
  (message "XSD validation finished. Please check result buffer."))

(use-package nxml-mode
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-nxml-mode))

(provide 'cfg-nxml)
;;; cfg-nxml.el ends here
