;;; Package --- Describe
;;; Commentary:
;;; Code:

(defun eos/describe-symbol-at-point (&optional arg)
    "Get help (documentation) for the symbol at point as ARG.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
    (interactive "P")
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol)))
    (when current-prefix-arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

(provide 'eos-describe)
;;; eos-describe.el ends here
