;; This file replaces itself with the actual configuration at first run.

;; We can't tangle without org!
(require 'org)

;; Open the configuration
(find-file (concat user-emacs-directory "init.org"))

;; tangle it
(org-babel-tangle)
