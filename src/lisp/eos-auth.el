;;; Package --- Authentication
;;; Commentary:
;;; Code:

(defun eos-auth-lookup-password (host user port)
  "Lookup (format HOST USER PORT) password on auth-source default file."
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
      (let ((secretf (plist-get (car auth) :secret)))
        (if secretf
          (funcall secretf)
          (error "Auth entry for %s@%s:%s has no secret!"
            user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(provide 'eos-auth)
;;; eos-auth.el ends here
