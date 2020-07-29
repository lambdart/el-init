;; set emacs-lisp-mode local variables
((emacs-lisp-mode
  ;; compile command
  (compile-command . "make -C ~/.emacs.d all")))

;; set c++-mode local variables
((c++-mode
  ;; compile command
  (compile-command . "make all")
  ;; indent style
  (c-file-style . "user")))
