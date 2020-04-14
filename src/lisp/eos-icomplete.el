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
      (completing-read "ic-recentf: " files nil t))))

(defun eos/icomplete/kill-ring ()
  "Insert the selected `kill-ring' item directly at point."
  (interactive)
  (insert
    (completing-read "ic-kill-ring: " kill-ring nil t)))

(defun eos/icomplete/mark-ring ()
  "Browse `mark-ring' interactively."
  (interactive)

  ;; get marks
  (let* ((marks (delete-dups (copy-sequence mark-ring)))
          (marks (if (equal (mark-marker) (make-marker)) marks
                   (cons (copy-marker (mark-marker)) marks))))
    ;; parse mark candidates
    (let* ((width (length (number-to-string (line-number-at-pos (point-max)))))
            (fmt  (format "%%%dd %%s" width))
            (candidates (mapcar
                          (lambda (mark)
                            (goto-char (marker-position mark))
                            (let ((line-num (line-number-at-pos))
                                   (line-str (buffer-substring
                                               (line-beginning-position) (line-end-position))))
                              (propertize (format fmt line-num line-str) 'point (point))))
                          marks)))
      ;; candidates? if yes, goto action
      (if candidates
        (progn
          (let* ((candidate (completing-read "ic-mark-ring: " candidates nil t))
                  (pos (get-text-property 0 'point candidate)))
            (when pos
              (unless (<= (point-min) pos (point-max))
                (if widen-automatically (widen)
                  (error "Position outside buffer bounds")))
              (goto-char pos))))
        (message "Mark ring is empty")))))

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
