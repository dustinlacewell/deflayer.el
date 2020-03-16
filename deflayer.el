;;; deflayer.el --- Polymode support for Org-brain -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (dash "0"))
;; Keywords: layers, configuration
;; URL: http://github.com/dustinlacewell/deflayer.el

;;; Commentary:

;; This package lets create configuration override layers

;;; Code:
(require 'dash)

(defvar deflayer--layers (make-hash-table)
  "A hashtable mapping defcustom groups to a list of configuration defaults")

(defun deflayer--get-all-settings (group)
  "Get a list of all variable names in defcustom group GROUP."
  (let* ((config-alist (symbol-plist group))
         (culled-alist (-drop-while (lambda (x) (not (eq x 'custom-group))) config-alist))
         (dirty-settings (car (cdr culled-alist)))
         (filtered-settings (--remove (not (eq (cadr it) 'custom-variable)) dirty-settings)))
    (-map 'car filtered-settings)))

(defun deflayer--get-activation-name (name)
  "Produce a activation function name from NAME."
  (format "deflayer-activate-%s" name))

(defun deflayer-save (group)
  "Save the current values for all variables in defcustom group
GROUP to deflayer--layers hashtable."
  (map-put! deflayer--layers group
           (--map (list it (symbol-value it)) (deflayer--get-all-settings group))))

(defun deflayer-restore (group)
  "Restore all cached defaults for defcustom group GROUP."
  (let ((defaults (map-elt deflayer--layers group)))
    (--each defaults (set (car it) (cadr it)))))

(defmacro deflayer (name group body)
  "Save the current values for all variables in the defcustom
group GROUP and produce a function with name based on NAME which
sets the variable values specified as list of name value pairs in
BODY."
  (deflayer-save group)
  `(defun ,(intern (deflayer--get-activation-name name)) ()
     (interactive)
     (deflayer-restore (quote ,group))
     ,@(--map (list 'setq (car it) (cadr it)) body)))

(provide 'deflayer)

;;; deflayer.el ends here
