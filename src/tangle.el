;; we can't tangle without org
(require 'org)

;; read eos org file
(find-file (expand-file-name "vlm.org" user-emacs-directory))

;; tangle it (only emacs-lisp source code)
(org-babel-tangle nil nil 'emacs-lisp)

;; rename: eos to init
(rename-file
  (expand-file-name "vlm.el" user-emacs-directory)
  (expand-file-name "init.el" user-emacs-directory)
  ;; OK-IF-ALREADY-EXISTS
  t)
