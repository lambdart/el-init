;;; Package --- Build
;;; Commentary:
;;; Code:

(require 'ob-tangle)
(require 'prog-mode)

(defun eos/build ()
  "If the current buffer is 'init.org' the code-blocks are tangled.
The tangled file will be compiled."
  (interactive)

  ;; avoid running hooks when tangling.
  (let ((prog-mode-hook nil)
         (buffer (current-buffer)))

    ;; switch or open init.org file
    (find-file (expand-file-name "init.org" user-emacs-directory))

    ;; tangle and compile
    (org-babel-tangle)
    (byte-compile-file (concat user-emacs-directory "init.el"))

    ;; switch to the previous buffer
    (switch-to-buffer buffer)))

(provide 'eos-build)
;;; eos-build.el ends here
