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
  ;; OK-IF-ALREADY-EXISTS, KEEP-TIME,
  ;; PRESERVE-UID-GID, PRESERVE-PERMISSIONS
  t t t t)

;; delete eos.el
(delete-file "eos.el")
