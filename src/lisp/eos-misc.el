;;; Package --- Miscellaneous
;;; Commentary:
;;; Code:

(require 'simple)

(defun eos/misc/raise-volume ()
  "Raise volume (factor +5)."
  (interactive)
  (async-shell-command "amixer -D default set Master 5+ unmute"))

(defun eos/misc/reduce-volume ()
  "Reduce volume (factor -5)."
  (interactive)
  (async-shell-command "amixer -D default set Master 5- unmute"))

(defun eos/misc/toggle-audio ()
  "Toggle audio (mute or unmute)."
  (interactive)
  (async-shell-command "amixer -D default set Master"))

(provide 'eos-misc)
;;; eos-misc.el ends here
