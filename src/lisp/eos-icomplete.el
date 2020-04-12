;;; Package --- Icomplete
;;; Commentary:
;;; Code:

(require 'recentf)
(require 'minibuffer)
(require 'icomplete)

(defun eos/icomplete/kill-ring-save (&optional arg)
  "Expand and save current icomplete match (ARG) to the kill ring.
With a prefix argument, insert the match to the point in the
current buffer"
  (interactive "*P")
  (when (and (minibufferp)
          (bound-and-true-p icomplete-mode))
    (kill-new (field-string-no-properties))
    (if current-prefix-arg
      (progn
        (select-window (get-mru-window))
        (insert (car kill-ring)
          (abort-recursive-edit))))))

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

(defun eos/icomplete/open-recent-file ()
  "Open `recent-list' item in a new buffer.
The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file
      (completing-read "ic-recentf: " files nil t))))

(defun eos/icomplete/insert-kill-ring ()
  "Insert the selected `kill-ring' item directly at point."
  (interactive)
  (insert
    (completing-read "ic-kill-ring: " kill-ring nil t)))

(defun eos/icomplete/company ()
  "Insert the selected company candidate directly at point."
  (interactive)
  (if (and (boundp 'company-candidates)
        (boundp 'company-common)
        (fboundp 'company-complete))
    (progn
      (unless company-candidates
        (company-complete))
      (unless (= (length company-candidates) 0)
        (let ((candidate (completing-read "ic-company: " company-candidates nil nil)))
          (delete-char (- (length company-common)))
          (insert candidate))))
    nil))

(provide 'eos-icomplete)
;;; eos-icomplete.el ends here
