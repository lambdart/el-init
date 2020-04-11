;;; Package --- Utilities
;;; Commentary:
;;; Code:

(require 'frame)
(require 'simple)

(defun eos-mkdir (dir)
  "Create DIR in the file system."
  (when (and (not (file-exists-p dir))
          (make-directory dir :parents))))

(defun eos-set-frame-font (font)
  "Set the default font to FONT."
  (cond ((find-font (font-spec :name font))
          (set-frame-font font nil t))))

(defun eos-set-dash-docset (docset)
  "Activate a DOCSET, if available."
  (when (fboundp 'dash-docs-activate-docset)
    (funcall 'dash-docs-activate-docset docset)))

(defun eos-set-company-backends (backends)
  "Set company back ends with BACKENDS."
  (make-local-variable 'company-backends)
  (when (boundp 'company-backends)
    (setq company-backends backends)))

(defun eos/set-flycheck-checker (checker)
  "Set flycheck CHECKER variable."
  (make-local-variable 'flycheck-checker)
  (when (boundp 'flycheck-checker)
    (setq flycheck-checker checker)))

(defun eos/set-frame-opacy (alpha)
  "Set ALPHA opacity in current frame."
  (interactive "nAlpha: ")
  (let ((alpha (or alpha 1.0)))
    (if (executable-find "transset")
      (async-shell-command (format "transset -a %.1f" alpha))
      (error "Transset not found"))))

(provide 'eos-util)
;;; eos-util.el ends here
