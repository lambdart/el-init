;;; Package --- Compile
;;; Commentary:
;;; Code:

(defun eos-compile (dir command)
  "Compile COMMAND at specific DIR.
     Just a `compile` function wrapper."
  (interactive)
  (if (file-exists-p dir)
    (let ((default-directory dir))
      (compile command))))

(provide 'eos-compile)
;;; eos-compile.el ends here
