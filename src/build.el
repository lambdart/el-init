;; This file replaces itself with the actual configuration at first run.

;; We can't tangle without org!
(require 'org)

;; Open the configuration
(find-file "eos.org")

;; tangle it
(org-babel-tangle)

;; finally byte-compile it
(byte-compile-file "eos.el")
