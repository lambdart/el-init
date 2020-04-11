;;; Package --- Edit
;;; Commentary:
;;; Code:

(require 'simple)

(defun eos-edit-move-lines (n)
  "Move N lines, up if N is positive, else down."
  (let* (text-start
          text-end
          (region-start (point))
          (region-end region-start)
          swap-point-mark
          delete-latest-newline)

    ;; STEP 1: identifying the text to cut.
    (when (region-active-p)
      (if (> (point) (mark))
        (setq region-start (mark))
        (exchange-point-and-mark)
        (setq swap-point-mark t
          region-end (point))))

    ;; text-end and region-end
    (end-of-line)

    (if (< (point) (point-max))
      (forward-char 1)
      (setq delete-latest-newline t)
      (insert-char ?\n))
    (setq text-end (point)
      region-end (- region-end text-end))

    ;; text-start and region-start
    (goto-char region-start)
    (beginning-of-line)
    (setq text-start (point)
      region-start (- region-start text-end))

    ;; STEP 2: cut and paste.
    (let ((text (delete-and-extract-region text-start text-end)))
      (forward-line n)
      ;; If the current-column != 0, I have moved the region at the bottom of a
      ;; buffer doesn't have the trailing newline.
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (insert text))

    ;; STEP 3: Restoring.
    (forward-char region-end)

    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))

    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark (+ (point) (- region-start region-end)))
      (if swap-point-mark
        (exchange-point-and-mark)))))

(defun eos/edit-move-lines-up (n)
  "Move N lines up."
  (interactive "p")
  (if (eq n nil)
    (setq n 1))
  (eos-edit-move-lines (- n)))

(defun eos/edit-move-lines-down (n)
  "Move N lines down."
  (interactive "p")
  (if (eq n nil)
    (setq n 1))
  (eos-edit-move-lines n))

(defun eos/edit-move-words-left (n)
  "Move word N times to the left."
  (interactive "p")
  (if (eq n nil)
    (setq n 1))
  (transpose-words (- n)))

(defun eos/edit-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun eos/edit-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (message "Indented selected region."))
      (progn
        (eos/edit-indent-buffer)
        (message "Indented buffer.")))))

(defun eos/edit-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.

If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will be duplicated."

  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
      (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end))
           (i arg))
      (while (> i 0)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))
        (setq i (1- i)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(provide 'eos-edit)
;;; eos-edit.el ends here
