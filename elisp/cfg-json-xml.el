;;; cfg-json-xml.el --- configfuration for json and xml  -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with json and xml formats

;;; Code:

(use-package json-mode
  :ensure t
  )

(use-package json-navigator
  :ensure t
  :after hierarchy
  :config
  ;; TODO: update config
  )


;; xmllint tools:
;; https://github.com/wbolster/emacs-xml-format
(use-package xml-format
  :ensure t
  :after nxml-mode
  )

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


(provide 'cfg-json-xml)
;;; cfg-json-xml.el ends here
