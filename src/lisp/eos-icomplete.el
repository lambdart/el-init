;;; Package --- Icomplete
;;; Commentary:
;;; Code:

(require 'minibuffer)
(require 'icomplete)

(defun eos/icomplete/kill-ring-save (&optional arg)
  "Expand and save current icomplete match (ARG) to the kill ring.
With a prefix argument, insert the match to the point in the
current buffer"
  (interactive "*P")
  (when (and (minibufferp)
          (bound-and-true-p icomplete-mode))
    (icomplete-force-complete-and-exit)
    (kill-new (field-string-no-properties))
    (if current-prefix-arg
      (progn
        (select-window (get-mru-window))
        (insert (car kill-ring))))))

(defun eos/icomplete/toggle-completion-styles (&optional arg)
  "Toggle between completion styles.
With pregix ARG use basic completion instead.
These styles are described in `completion-styles-alist'."
  (interactive "*P")
  (when (and (minibufferp)
          (bound-and-true-p icomplete-mode))
    (let* ((completion-styles-original completion-styles)
            (basic  '(basic emacs22))
            (flex   '(helm-flex initials substring partial-completion))
            (prefix '(partial-completion substring initials)))

      ;; maybe toggle (basic or flex)
      (if current-prefix-arg
        (setq-local completion-styles basic)
        (if (not (eq (car completion-styles) 'helm-flex))
          (setq-local completion-styles flex)
          (setq-local completion-styles prefix)))

      ;; show completion style
      (message "completion style: %s "
        (propertize (format "%s" (car completion-styles)) 'face 'highlight)))))

(provide 'eos-icomplete)
;;; eos-icomplete.el ends here
