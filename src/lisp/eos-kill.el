;;; Package --- Kill
;;; Commentary:
;;; Code:

(defun eos-kill-buffer (buffer-name)
  "Kill BUFFER-NAME if exists."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

(defun eos-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun eos-kill-minibuffer ()
  "Kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun eos-kill-minibuffer-first (sub-read &rest args)
  "Kill minibuffer before open another one."
  (let ((active (active-minibuffer-window)))
    (if active
      (progn
        ;; we have to trampoline, since we're IN the minibuffer right now.
        (apply 'run-at-time 0 nil sub-read args)
        (abort-recursive-edit))
      (apply sub-read args))))

;; testing works
;; (advice-add 'read-from-minibuffer :around 'eos-kill-minibuffer-first)

(provide 'eos-kill)
;;; eos-edit.el ends here
