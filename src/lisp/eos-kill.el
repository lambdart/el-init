;;; Package --- Kill
;;; Commentary:
;;; Code:

(require 'simple)

(defun eos-kill-buffer (buffer-name)
  "Kill BUFFER-NAME if exists."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

(defun eos-kill-minibuffer ()
  "Kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun eos/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun eos/kill-line (&optional arg)
  "Do a kill-line but copy rather than kill. This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables. This function works by temporarily making the
buffer read-only."
  (interactive "P")
  (let ((buffer-read-only t)
         (kill-read-only-ok t))
    (kill-line arg))
    (move-beginning-of-line nil))

(provide 'eos-kill)
;;; eos-edit.el ends here
