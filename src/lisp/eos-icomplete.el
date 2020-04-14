;;; Package --- Icomplete
;;; Commentary:
;;; Code:

(require 'simple)
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
            (basic    '(basic emacs22))
            (initials '(initials substring partial-completion))
            (prefix   '(partial-completion substring initials)))

      ;; choose basic, initials or prefix
      (if current-prefix-arg
        (setq-local completion-styles basic)
        (progn
          (if (not (eq (car completion-styles) 'initials))
            (setq-local completion-styles initials)
            (setq-local completion-styles prefix))))

      ;; show which current completion style
      (message "Completion style: %s "
        (format "%s" (car completion-styles))))))

(defun eos/icomplete/recentf-open-file ()
  "Open `recent-list' item in a new buffer.
The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file
      (completing-read "Recentf: " files nil t))))

(defun eos/icomplete/kill-ring ()
  "Insert the selected `kill-ring' item directly at point."
  (interactive)
  (insert
    (completing-read "Kill-ring: " kill-ring nil t)))

(defun eos/icomplete-mark-ring-line-string-at-pos (pos)
  "Return line string at position POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((line (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (remove-text-properties 0 (length line) '(read-only) line)
      (if (string= "" line)
        "<EMPTY LINE>"
        line))))

(defun eos/icomplete/mark-ring ()
  "Browse `mark-ring' interactively."
  (interactive)
  (let* (candidates)
    (setq candidates
      (cl-loop with marks = (if (mark t)
                              (cons (mark-marker) mark-ring)
                              mark-ring)
        for marker in marks
        with max-line-number = (line-number-at-pos (point-max))
        with width = (length (number-to-string max-line-number))
        for m = (format (concat "%" (number-to-string width) "d: %s")
                  (line-number-at-pos marker)
                  (eos/icomplete-mark-ring-line-string-at-pos marker))
        unless (and recip (assoc m recip))
        collect (cons m marker) into recip
        finally return recip))
    (if candidates
      (progn
        (let (candidate)
          (setq candidate (completing-read "Mark-ring: " candidates nil t))
          (goto-char (cdr (assoc candidate candidates))))))
    (message "Mark ring is empty")))

(defun eos/icomplete/company ()
  "Insert the selected company candidate directly at point."
  (interactive)
  (if (and
        (boundp 'company-common)
        (boundp 'company-candidates)
        (fboundp 'company-complete))
    (progn
      (unless company-candidates
        (company-complete))
      (unless (= (length company-candidates) 0)
        (let ((candidate (completing-read "ic-company: " company-candidates nil nil)))
          (delete-char (- (length company-common)))
          (insert candidate))))
    nil))

(defun eos/icomplete/dash-docs-search ()
  "Provide dash-docs candidates to `icomplete."
  (interactive)
  (dash-docs-create-common-connections)
  (dash-docs-create-buffer-connections)

  ;; get candidates
  (let* ((candidates (cl-loop for docset in (dash-docs-maybe-narrow-docsets "")
                       appending (dash-docs-search-docset docset "")))
          (candidate (completing-read "ic-docs-search: " candidates nil nil)))
    ;; parse candidate
    (let* ((i 0)
            (n (catch 'nth-elt
                 (dolist (value candidates)
                   (when (equal candidate (car value))
                     (throw 'nth-elt i))
                   (setq i (+ 1 i)))))
            (search-result (nth n candidates)))
      (pop search-result)

      ;; action: open documentation file
      (dash-docs-browse-url search-result))))

(provide 'eos-icomplete)
;;; eos-icomplete.el ends here
