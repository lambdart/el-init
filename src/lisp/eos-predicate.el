;;; Package --- Predicates
;;; Commentary:
;;; Code:

(require 'simple)

(defun eos/big-file-p ()
  "Return t if buffer size is greater then `x` (is to big)."
  (interactive)
  (or (> (buffer-size) (* 4086 64))
    (> (line-number-at-pos (point-max)) 4086)))

(provide 'eos-predicate)
;;; eos-predicate.el ends here
