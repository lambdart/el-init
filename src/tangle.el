;; tangle the source code blocks require org
(require 'org)

(defvar vlm-org-file
  (expand-file-name "vlm.org" user-emacs-directory)
  "The vlm.org file - full path.")

(defvar vlm-tmp-init
  (expand-file-name "vlm.el" user-emacs-directory)
  "Temporary file.")

(defvar vlm-new-init
  (expand-file-name "init.el" user-emacs-directory)
  "Init file.")

(defun vlm-tangle-and-rename ()
  "Tangle and rename `vlm-org-file' file."
  ;; extract elisp source code blocks from vlm.org
  (org-babel-tangle-file vlm-org-file nil "emacs-lisp")
  ;; rename files if temporary file exists
  (when (file-exists-p vlm-tmp-init)
    (rename-file vlm-tmp-init vlm-new-init t)))

;; invoke vlm-tangle-and-rename function
(funcall 'vlm-tangle-and-rename)
