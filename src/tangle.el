;; We can't tangle without org!
(require 'org)

;; read eos file
(find-file (expand-file-name "eos.org" user-emacs-directory))

;; tangle it
(org-babel-tangle)

;; copy to init.el
(copy-file
  (expand-file-name "eos.el" user-emacs-directory)
  (expand-file-name "init.el" user-emacs-directory)
  t t t)
