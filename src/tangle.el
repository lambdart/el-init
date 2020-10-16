;; tangle the source code blocks require org
(require 'org)

;; read vlm.org file
(find-file (expand-file-name "vlm.org" user-emacs-directory))

;; tangle it (only emacs-lisp source code)
(org-babel-tangle nil nil 'emacs-lisp)

;; rename: vlm.el to init.el
(rename-file
  (expand-file-name "vlm.el" user-emacs-directory)
  (expand-file-name "init.el" user-emacs-directory)
  ;; OK-IF-ALREADY-EXISTS
  t)
