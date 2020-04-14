;;; Package --- Eww
;;; Commentary:
;;; Code:

(require 'eos-grab)

(defun eos/eww/search-words ()
  "Search the web for the text between BEG and END.
           If region is active (and not whitespace), search the web for
           the text in that region.
           Else if the region is not active, and the point is on a symbol,
           search the web for that symbol.
           Else prompt the user for a search string.
           See the `eww-search-prefix' variable for the search engine used."
  (interactive)
  (let ((search-string (eos-grab-text-or-symbol-at-point)))
    (when (and (stringp search-string)
            (string-match-p "\\`[[:blank:]]*\\'" search-string))
      (customize-set-variable search-string nil))
    (if (stringp search-string)
      (eww search-string)
      (call-interactively #'eww))))

(provide 'eos-eww)
;;; eos-eww.el ends here
