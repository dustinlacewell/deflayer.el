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

(defvar deflayer--layers (make-hash-table))

(defun deflayer--get-all-settings (group)
  (let* ((config-alist (symbol-plist group))
         (culled-alist (-drop-while (lambda (x) (not (eq x 'custom-group))) config-alist))
         (dirty-settings (car (cdr culled-alist)))
         (filtered-settings (--remove (not (eq (cadr it) 'custom-variable)) dirty-settings)))
    (-map 'car filtered-settings)))

(defun deflayer--function-name-for-group (group)
 (format "deflayer-activate-%s" (symbol-name name)))

(defun deflayer-save (group)
  (map-put deflayer--layers group
           (--map (list it (symbol-value it)) (deflayer--get-all-settings group))))

(defun deflayer-restore (group)
  (let ((defaults (map-elt deflayer--layers group))))
  (--each deflayer--defaults (set (car it) (cadr it))))

(defmacro deflayer (name group body)
  (deflayer-save group)
  `(defun ,(intern (deflayer--function-name-for-group group)) ()
     (deflayer-restore (quote ,group))
     ,@(--map (list 'setq (car it) (cadr it)) body)))

(provide 'deflayer)

;;; deflayer.el ends here
