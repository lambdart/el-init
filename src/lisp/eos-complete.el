;;; Package --- Complete
;;; Commentary:
;;; Code:

(require 'completion)
(require 'minibuffer)

(defun eos/complete-or-indent ()
  "Complete or indent."
  (interactive)
  (if (looking-at "\\_>")
    (when (fboundp 'complete)
      (complete nil)))
  (indent-according-to-mode))

(defun eos/complete-at-point-or-indent ()
  "This smart tab is a `minibuffer' compliant.

It acts as usual in the `minibuffer'.
Else, if mark is active, indents region.
Else if point is at the end of a symbol, expands it.
Else indents the current line."

  (interactive)
  (if (minibufferp)
    (unless (minibuffer-complete)
      (complete-symbol nil))
    (if mark-active
      (indent-region (region-beginning)
        (region-end))
      (if (looking-at "\\_>")
        (complete-symbol nil)
        (indent-according-to-mode)))))

(defun eos/complete-buffer-or-indent ()
  "Company (complete anything (in-buffer)) or indent."
  (interactive)
  (if (looking-at "\\_>")
    (progn
      (when (fboundp 'company-complete)
        (funcall 'company-complete)))
    (indent-according-to-mode)))

(provide 'eos-complete)
;;; eos-complete.el ends here
