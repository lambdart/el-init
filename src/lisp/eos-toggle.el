;;; Package --- Toggle
;;; Commentary:
;;; Code:

(defun eos/toggle-debug-on-error ()
  "Toggle `debug-on-error`."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Debug-on-error: %s"
    (if debug-on-error "enabled" "disabled")))

(provide 'eos-toggle)
;;; eos-toggle.el ends here
