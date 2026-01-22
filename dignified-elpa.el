;;; dignified-elpa.el --- Stripe Connected Accounts Author Onboarding  -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2"))

(require 'url)
(require 'json)
(require 'web-server)

(defconst dignified-elpa-onboard-url "https://dignified-elpa.commandlinesystems.com/author_onboard")
(defconst dignified-elpa-status-url "https://dignified-elpa.commandlinesystems.com/author_status")
(defconst dignified-elpa-token-dir (expand-file-name "~/.local/share/aa/cognito/"))
(defconst dignified-elpa-ws-port 17973)

(defvar dignified-elpa--ws-result nil)

(defun dignified-elpa--read-token ()
  (let ((token-file (expand-file-name "id_token" dignified-elpa-token-dir)))
    (when (file-readable-p token-file)
      (with-temp-buffer
        (insert-file-contents token-file)
        (string-trim (buffer-string))))))

(defun dignified-elpa--onboard-session ()
  (let* ((id-token (dignified-elpa--read-token))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "id_token=" (url-hexify-string id-token))))
    (unless id-token
      (error "No id_token found. Please authenticate first"))
    (with-current-buffer (url-retrieve-synchronously dignified-elpa-onboard-url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun dignified-elpa--status ()
  (let* ((id-token (dignified-elpa--read-token))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "id_token=" (url-hexify-string id-token))))
    (unless id-token
      (error "No id_token found. Please authenticate first"))
    (with-current-buffer (url-retrieve-synchronously dignified-elpa-status-url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun dignified-elpa--ws-handler (request)
  (with-slots (process headers) request
    (let ((params (cadr (assoc "Content" headers))))
      (setq dignified-elpa--ws-result params)
      (ws-response-header process 200 '("Content-Type" . "text/html"))
      (process-send-string process "OK"))))

(defun dignified-elpa-onboard-flow (url _account-id)
  (setq dignified-elpa--ws-result nil)
  (let ((ws (ws-start `((:POST . dignified-elpa--ws-handler)) dignified-elpa-ws-port nil :host "127.0.0.1"))
        (timeout-time (+ (float-time) 420)))
    (unwind-protect
        (progn
          (browse-url url)
          (while (and (null dignified-elpa--ws-result)
                      (< (float-time) timeout-time))
            (accept-process-output nil 0.1))
          (when dignified-elpa--ws-result
            (let ((params (url-parse-query-string dignified-elpa--ws-result)))
              (string= (caar (cdr (assoc "onboarding_complete" params))) "true"))))
      (ws-stop ws))))

;;;###autoload
(defun dignified-elpa-onboard ()
  (interactive)
  (condition-case err
      (let* ((session (dignified-elpa--onboard-session))
             (url (cdr (assoc 'url session)))
             (account-id (cdr (assoc 'account_id session)))
             (error-msg (cdr (assoc 'error session))))
        (if error-msg
            (message "Error: %s" error-msg)
          (message "Opening Stripe onboarding in browser...")
          (if (dignified-elpa-onboard-flow url account-id)
              (message "Author onboarding completed successfully!")
            (message "Onboarding timed out or was incomplete. Please try again."))))
    (error (message "Onboarding failed: %s" (error-message-string err)))))

;;;###autoload
(defun dignified-elpa-status ()
  (interactive)
  (condition-case err
      (let* ((status (dignified-elpa--status))
             (status-type (cdr (assoc 'status status))))
        (with-help-window "*Author Status*"
          (princ "Author Account Status\n")
          (princ "=====================\n\n")
          (if (string= status-type "not_onboarded")
              (princ "Status: Not onboarded\n\nRun M-x dignified-elpa-onboard to begin onboarding.")
            (princ (format "Status: Onboarded\n"))
            (princ (format "Account ID: %s\n" (cdr (assoc 'account_id status))))
            (princ (format "Details Submitted: %s\n" (if (cdr (assoc 'details_submitted status)) "Yes" "No")))
            (princ (format "Charges Enabled: %s\n" (if (cdr (assoc 'charges_enabled status)) "Yes" "No")))
            (princ (format "Payouts Enabled: %s\n" (if (cdr (assoc 'payouts_enabled status)) "Yes" "No")))
            (when (cdr (assoc 'onboarded_at status))
              (princ (format "Onboarded At: %s\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (seconds-to-time (cdr (assoc 'onboarded_at status))))))))))
    (error (message "Failed to retrieve status: %s" (error-message-string err)))))

(provide 'dignified-elpa)
;;; dignified-elpa.el ends here
