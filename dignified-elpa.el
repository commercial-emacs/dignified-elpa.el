;;; dignified-elpa.el --- line for checkout -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2"))

(require 'cl-lib)
(require 'url-auth)
(require 'json)
(require 'web-server)

(eval-when-compile
  (when (< emacs-major-version 29)
    (defun seq-keep (function sequence)
      "Apply FUNCTION to SEQUENCE and return all non-nil results."
      (delq nil (seq-map function sequence)))))

(defvar url-http-end-of-headers)

(defmacro dignified-elpa-let-token-dir (token-dir host &rest body)
  (declare (indent defun))
  `(let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
          (,token-dir (directory-file-name
		       (mapconcat #'file-name-as-directory
				  (list data-home
					"dignified-elpa"
					(symbol-name ,host))
				  ""))))
     (unless (file-directory-p ,token-dir)
       (make-directory ,token-dir t))
     (unless (equal "700" (format "%o" (file-modes ,token-dir)))
       (set-file-modes ,token-dir #o700))
     ,@body))

(defun dignified-elpa-packages ()
  (when-let ((dignified (cl-loop for (name . url) in package-archives when
				 (string= (url-host (url-generic-parse-url url))
					  "dignified-elpa.commandlinesystems.com")
				 return name)))
    (mapcar (lambda (desc)
	      (intern
	       (file-relative-name
		(url-filename
		 (url-generic-parse-url
		  (alist-get :url (package-desc-extras desc))))
		"/")))
	    (cl-mapcan (lambda (descs)
			 (seq-filter
			  (lambda (desc)
			    (equal (package-desc-archive desc) dignified))
			  descs))
		       (mapcar #'cdr package-archive-contents)))))

(defvar dignified-packages (dignified-elpa-packages))

;;;###autoload
(defun dignify (package)
  (interactive
   (list (let ((completion-styles '(substring flex)))
	   (completing-read
	    "Package: " dignified-packages
	    (lambda (pkg) (null (locate-library (concat (symbol-name pkg) ".el"))))
	    :must-match))))
  (unless (string-empty-p (string-trim package))
    (if (member package (mapcar #'symbol-name dignified-packages))
	(dignified-elpa-checkout package)
      (user-error "%s not among dignified packages" package))))

(defconst dignified-elpa-domain "dignified-elpa.commandlinesystems.com")
(defconst dignified-elpa-auth "allaccess.auth.commandlinesystems.com")
(defconst dignified-elpa-client "7li6vomknkgvomeqhj9674nlcb")
(defconst dignified-elpa-ws-port 17973)

(defconst dignified-elpa-checkout-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil dignified-elpa-domain nil
			  "/checkout" nil nil t)))

(defconst dignified-elpa-success-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil dignified-elpa-domain nil
			  "/success.html" nil nil t)))

(defconst dignified-elpa-cancel-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil dignified-elpa-domain nil
			  "/cancel.html" nil nil t)))

(defconst dignified-elpa-purchases-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil dignified-elpa-domain nil
			  "/purchases" nil nil t)))

(defconst dignified-elpa-redirect-url
  (url-recreate-url
   (url-parse-make-urlobj "http" nil nil "127.0.0.1"
			  dignified-elpa-ws-port "/" nil nil t)))

(defconst dignified-elpa-hosted-ui
  (let ((redirect (url-recreate-url
		   (url-parse-make-urlobj "https" nil nil dignified-elpa-domain
					  nil "/code/" nil nil t))))
    (url-recreate-url
     (let ((query-string (url-build-query-string
			  `(("response_type" "code")
			    ("client_id" ,dignified-elpa-client)
			    ("redirect_uri" ,redirect)))))
       (url-parse-make-urlobj "https" nil nil dignified-elpa-auth nil
			      (concat "/login?" query-string) nil nil t)))))

(defsubst dignified-elpa-id-token-alist ()
  (when-let* ((token (dignified-elpa-token "id_token")))
    (json-parse-string
     (base64-decode-string
      (cl-second (split-string (symbol-name token) "\\."))
      'base64url)
     :array-type 'list
     :object-type 'alist)))

(defun dignified-elpa-token (which)
  (dignified-elpa-let-token-dir token-dir 'cognito
    (ignore-errors
      (with-temp-buffer
	(save-excursion (insert-file-contents (expand-file-name which token-dir)))
	(read (current-buffer))))))

(defmacro dignified-elpa-squirrel (token)
  "Persist value of symbol .TOKEN to a file named TOKEN."
  `(dignified-elpa-let-token-dir token-dir 'cognito
     (with-temp-file (concat (file-name-as-directory token-dir) (symbol-name ',token))
       (insert ,(intern (concat "." (symbol-name token))) "\n"))))

(defun dignified-elpa-squirrel-tokens (alist)
  "Return list of tokens changed.
Side effect squirrel changed tokens to disk."
  (let (result)
    (let-alist alist
      (when .access_token
	(dignified-elpa-squirrel access_token)
	(push 'access_token result))
      (when .refresh_token
	(dignified-elpa-squirrel refresh_token)
	(push 'refresh_token result))
      (when .id_token
	(dignified-elpa-squirrel id_token)
	(push 'id_token result))
      (nreverse result))))

(defun dignified-elpa-hosted-ui-flow ()
  "Return t if hosted-ui => elpa_token.php => ws."
  (unwind-protect
      (catch 'success
	(ws-start
	 '(((:POST . ".*") .
	    (lambda (request)
	     (with-slots (process headers) request
	      (dignified-elpa-squirrel-tokens
	       (mapcar (lambda (pair)
			(cons (if (stringp (car pair))
			       (intern (car pair))
			       (car pair))
			 (cdr pair)))
		headers))
	      (ws-response-header process 200 '("Content-type" . "text/plain"))
	      (process-send-string process "Thanks. Identity verified.\n")
	      (process-send-eof process)
	      (throw 'success t)))))
	 dignified-elpa-ws-port)
	(browse-url dignified-elpa-hosted-ui)
	(message "Blocking on browser login... C-g to abort")
	(cl-loop repeat 210 ; 3.5 minutes to complete login
		 do (accept-process-output nil 1)
		 finally return nil))
    (ws-stop-all)))

(defmacro dignified-elpa--align (&rest args)
  (declare (indent defun))
  (let* ((cars (let ((idx 0))
		 (seq-keep (lambda (x) (prog1 (when (zerop (% idx 2)) x)
					 (cl-incf idx)))
			   args)))
	 (cdrs (let ((idx 0))
		 (seq-keep (lambda (x) (prog1 (unless (zerop (% idx 2)) x)
					 (cl-incf idx)))
			   args)))
	 (widest (cl-reduce #'max (mapcar #'length cars))))
    `(mapc
      (lambda (pair)
	(cl-flet ((insert-lhs ()
		    (insert (make-string (1+ (- ,widest (string-width (car pair)))) ?\s)
			    (propertize (concat (car pair) ": ")
					'font-lock-face 'font-lock-function-name-face)))
		  (decorate-rhs (rhs)
		    (if (equal "Notification" (car pair))
			(insert (propertize rhs 'font-lock-face 'font-lock-string-face))
		      (insert rhs))))
	  (if (and (stringp (cdr pair)) (not (zerop (length (cdr pair)))))
	      (progn (insert-lhs)
		     (decorate-rhs (cdr pair))
		     (insert "\n"))
	    (when (functionp (cdr pair))
	      (insert-lhs)
	      (let ((pt (point)))
		(funcall (cdr pair))
		(if (equal pt (point))
		    (progn
		      (beginning-of-line)
		      (kill-line))
		  (insert "\n")))))))
      (cl-map 'list #'cons (quote ,cars) (backquote ,cdrs)))))

(defun dignified-elpa-checkout-flow (url)
  "Return t if checkout.stripe.com => {success,cancel}.html => ws."
  (unwind-protect
      (catch 'success
	(ws-start
	 '(((:POST . ".*") .
	    (lambda (request)
	      (with-slots (process headers) request
		(ws-response-header
		 process 303
		 `("Location" . ,(if (assoc-default "success" headers)
				     dignified-elpa-success-url
				   dignified-elpa-cancel-url)))
		(process-send-eof process)
		(throw 'success (assoc-default "success" headers))))))
	 dignified-elpa-ws-port)
	(browse-url url)
	(message "Blocking on checkout... C-g to abort")
	(cl-loop repeat 210 ; 3.5 minutes to complete checkout
		 do (accept-process-output nil 1)
		 finally return nil))
    (ws-stop-all)))

(defun dignified-elpa-checkout (repo)
  (let-alist (dignified-elpa--auth-get
	      (concat (directory-file-name dignified-elpa-checkout-url) "?"
		      (url-build-query-string `(("name" ,repo)))))
    (if .error
	(user-error (format "dignified-elpa-checkout: %s" .error))
      (dignified-elpa-checkout-flow .url))))

(defun dignified-elpa--auth-get (url)
  (catch 'done
    (dotimes (_i 2)
      (let ((result
	     (cl-destructuring-bind (code . buffer)
		 (dignified-elpa--get url)
	       (unwind-protect
		   (let ((parsed (with-current-buffer buffer
				   (goto-char url-http-end-of-headers)
				   (condition-case nil
				       (json-parse-buffer :false-object nil
							  :null-object nil
							  :array-type 'list
							  :object-type 'alist)
				     (json-end-of-file nil)))))
		     (cl-case (/ code 100)
		       (4 (if (y-or-n-p "Open browser to permission Dignified Elpa? ")
			      (when (dignified-elpa-hosted-ui-flow)
				(cl-destructuring-bind (code* . buffer*)
				    (dignified-elpa--get url)
				  (setq code code*
					buffer (prog1 buffer* (when (buffer-live-p buffer)
								(kill-buffer buffer))))))
			    (user-error "That's too bad")))
		       (5 (error "Server error: %s" (or (alist-get 'error parsed)
							 "Contact commandlinesystems.com"))))
		     (if (= 2 (/ code 100))
			 parsed
		       (error "dignified-elpa--auth-get %s"
			      (with-current-buffer buffer
				(buffer-substring-no-properties
				 (point-min)
				 url-http-end-of-headers)))))
		 (when (buffer-live-p buffer)
		   (kill-buffer buffer))))))
	;; if RESULT were a re-auth (causing squirrel to succeed)
	;; then go round again
	(unless (dignified-elpa-squirrel-tokens result)
	  (throw 'done result))))))

(defun dignified-elpa--get (url)
  "Return cons of response code and buffer response.
Caller must clean it up."
  (let* (url-registered-auth-schemes
	 (access-token (dignified-elpa-token "access_token"))
	 (id-token (dignified-elpa-token "id_token"))
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" .
				       "application/x-www-form-urlencoded")
				      ;; Dummy authorization so that
				      ;; `url-http-handle-authentication'
				      ;; doesn't keep trying.
				      ("Authorization" .
				       "Basic")))
	 (url-request-data (url-build-query-string
			    `(,@(when access-token
				 (list `("access_token" ,access-token)))
			      ,@(when id-token
				 (list `("id_token" ,id-token)))
			      ("redirect_uri" ,dignified-elpa-redirect-url)))))
    (let ((buffer (url-retrieve-synchronously url)))
      (cons (buffer-local-value 'url-http-response-status buffer) buffer))))

(defun dignified-elpa-dashboard ()
  (interactive)
  (with-help-window "*Dignified Elpa*"
    (let ((back-p (dignified-elpa-token "id_token")))
      (let-alist (dignified-elpa--auth-get dignified-elpa-purchases-url)
	(dignified-elpa--align
	 "Notification" ,(concat "Welcome " (when back-p "back ") "to Dignified Elpa!")
	 "User" ,(alist-get 'email (dignified-elpa-id-token-alist))
	 "Purchases" ,(string-join (or .purchases '("None")) ", "))))))

(defalias 'dignified-elpa #'dignified-elpa-dashboard)

(add-hook 'package-refresh-contents-hook
          (lambda (&rest_args) (setq dignified-packages (dignified-elpa-packages))))

(provide 'dignified-elpa)
;;; dignified-elpa.el ends here
