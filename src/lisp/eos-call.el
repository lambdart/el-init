;;; Package --- Call
;;; Commentary:
;;; Code:

(defun eos-call-proc (name)
  "Call (execute) a process by NAME."
  (if (executable-find name)
    (start-process name nil name)
    nil))

(defun eos-call-func (func &rest args)
  "Call FUNC with ARGS, if it's bounded."
  (when (fboundp func)
    (funcall func args)))

(provide 'eos-call)
;;; eos-call.el ends here
