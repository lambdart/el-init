;;; Package --- Load
;;; Commentary:
;;; Code:

;;; -*- lexical-binding: t -*-

(require 'cl-seq)

(defun eos-update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun eos-add-subdirs-to-load-path (&rest _)
  "Add sub-directories to `load-path'."
  (interactive)
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)
    (cl-remove-duplicates load-path)))

(defun eos-load-file (file)
  "Load FILE if exists."
  (if (file-exists-p file)
    (load (expand-file-name file) t nil nil)
    (message "file %s not found" file)))

(provide 'eos-load)
;;; eos-load.el ends here
