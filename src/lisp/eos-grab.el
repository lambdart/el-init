;;; Package --- Grab
;;; Commentary:
;;; Code:

(require 'simple)

(defun eos-grab-text-or-symbol-at-point ()
  "Get the text in region or symbol at point.
If region is active, return the text in that region.  Else if the
point is on a symbol, return that symbol name.  Else return nil."
  (cond ((use-region-p)
          (buffer-substring-no-properties
            (region-beginning) (region-end)))
    ((symbol-at-point)
      (substring-no-properties (thing-at-point 'symbol)))
    (t
      nil)))

(provide 'eos-grab)
;;; eos-grab.el ends here
