;;; Package --- Toggle
;;; Commentary:
;;; Code:

(defun eos/occur-at-point ()
  "Occur with `thing-at-point' function."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol (occur symbol)
      (message "Occur-at-point: No candidate."))))

(defun eos/search-keymaps (key)
  "Search for KEY in all known keymaps.
Keymaps list will be printed on *Messages* buffer."
  (interactive "kPress key: ")
  (mapatoms (lambda (ob)
              (when (and (boundp ob) (keymapp (symbol-value ob)))
                (when (functionp (lookup-key (symbol-value ob) key))
                  (message "%s" ob))))))

(provide 'eos-search)
;;; eos-search.el ends here
