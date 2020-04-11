;;; Package --- Focus
;;; Commentary:
;;; Code:

(require 'minibuffer)

(defun eos/focus-minibuffer ()
  "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer."

  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun eos/focus-minibuffer-or-completions ()
  "Focus the active minibuffer or the \\*Completions\\*.

      If both the minibuffer and the Completions are present, this
      command will first move per invocation to the former, then the
      latter, and then continue to switch between the two.

      The continuous switch is essentially the same as running
      `eos/focus-minibuffer' and `switch-to-completions' in
      succession."
  (interactive)
  (let* ((mini (active-minibuffer-window))
          (completions (get-buffer-window "*Completions*")))
    (cond ((and mini
             (not (minibufferp)))
            (select-window mini nil))
      ((and completions
         (not (eq (selected-window)
                completions)))
        (select-window completions nil)))))

;; testing
(global-set-key (kbd "s-v") 'eos/focus-minibuffer-or-completions)

(provide 'eos-focus)
;;; eos-focus.el ends here
