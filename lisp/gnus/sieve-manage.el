;;; sieve-manage.el --- Implementation of the managesieve protocol in elisp

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an elisp API for the managesieve network
;; protocol.
;;
;; It uses the SASL library for authentication, which means it
;; supports DIGEST-MD5, CRAM-MD5, SCRAM-MD5, NTLM, PLAIN and LOGIN
;; methods.  STARTTLS is not well tested, but should be easy to get to
;; work if someone wants.
;;
;; The API should be fairly obvious for anyone familiar with the
;; managesieve protocol, interface functions include:
;;
;; `sieve-manage-open'
;; open connection to managesieve server, returning a buffer to be
;; used by all other API functions.
;;
;; `sieve-manage-opened'
;; check if a server is open or not
;;
;; `sieve-manage-close'
;; close a server connection.
;;
;; `sieve-manage-listscripts'
;; `sieve-manage-deletescript'
;; `sieve-manage-getscript'
;; performs managesieve protocol actions
;;
;; and that's it.  Example of a managesieve session in *scratch*:
;;
;; (with-current-buffer (sieve-manage-open "mail.example.com")
;;   (sieve-manage-authenticate)
;;   (sieve-manage-listscripts))
;;
;; => ((active . "main") "vacation")
;;
;; References:
;;
;; draft-martin-managesieve-02.txt,
;; "A Protocol for Remotely Managing Sieve Scripts",
;; by Tim Martin.
;;
;; Release history:
;;
;; 2001-10-31 Committed to Oort Gnus.
;; 2002-07-27 Added DELETESCRIPT.  Suggested by Ned Ludd.
;; 2002-08-03 Use SASL library.

;;; Code:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(if (locate-library "password-cache")
    (require 'password-cache)
  (require 'password))

(eval-when-compile
  (require 'cl)				; caddr
  (require 'sasl)
  (require 'starttls))
(autoload 'sasl-find-mechanism "sasl")
(autoload 'starttls-open-stream "starttls")
(autoload 'auth-source-search "auth-source")

;; User customizable variables:

(defgroup sieve-manage nil
  "Low-level Managesieve protocol issues."
  :group 'mail
  :prefix "sieve-")

(defcustom sieve-manage-log "*sieve-manage-log*"
  "Name of buffer for managesieve session trace."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-server-eol "\r\n"
  "The EOL string sent from the server."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-client-eol "\r\n"
  "The EOL string we send to the server."
  :type 'string
  :group 'sieve-manage)

(defcustom sieve-manage-streams '(network starttls shell)
  "Priority of streams to consider when opening connection to server."
  :group 'sieve-manage)

(defcustom sieve-manage-stream-alist
  '((network   sieve-manage-network-p          sieve-manage-network-open)
    (shell     sieve-manage-shell-p            sieve-manage-shell-open)
    (starttls  sieve-manage-starttls-p         sieve-manage-starttls-open))
  "Definition of network streams.

\(NAME CHECK OPEN)

NAME names the stream, CHECK is a function returning non-nil if the
server support the stream and OPEN is a function for opening the
stream."
  :group 'sieve-manage)

(defcustom sieve-manage-authenticators '(digest-md5
					 cram-md5
					 scram-md5
					 ntlm
					 plain
					 login)
  "Priority of authenticators to consider when authenticating to server."
  :group 'sieve-manage)

(defcustom sieve-manage-authenticator-alist
  '((cram-md5   sieve-manage-cram-md5-p       sieve-manage-cram-md5-auth)
    (digest-md5 sieve-manage-digest-md5-p     sieve-manage-digest-md5-auth)
    (scram-md5  sieve-manage-scram-md5-p      sieve-manage-scram-md5-auth)
    (ntlm       sieve-manage-ntlm-p           sieve-manage-ntlm-auth)
    (plain      sieve-manage-plain-p          sieve-manage-plain-auth)
    (login      sieve-manage-login-p          sieve-manage-login-auth))
  "Definition of authenticators.

\(NAME CHECK AUTHENTICATE)

NAME names the authenticator.  CHECK is a function returning non-nil if
the server support the authenticator and AUTHENTICATE is a function
for doing the actual authentication."
  :group 'sieve-manage)

(defcustom sieve-manage-default-port 2000
  "Default port number or service name for managesieve protocol."
  :type 'integer
  :group 'sieve-manage)

(defcustom sieve-manage-default-stream 'network
  "Default stream type to use for `sieve-manage'.
Must be a name of a stream in `sieve-manage-stream-alist'."
  :version "24.1"
  :type 'symbol
  :group 'sieve-manage)

;; Internal variables:

(defconst sieve-manage-local-variables '(sieve-manage-server
					 sieve-manage-port
					 sieve-manage-auth
					 sieve-manage-stream
					 sieve-manage-process
					 sieve-manage-client-eol
					 sieve-manage-server-eol
					 sieve-manage-capability))
(defconst sieve-manage-coding-system-for-read 'binary)
(defconst sieve-manage-coding-system-for-write 'binary)
(defvar sieve-manage-stream nil)
(defvar sieve-manage-auth nil)
(defvar sieve-manage-server nil)
(defvar sieve-manage-port nil)
(defvar sieve-manage-state 'closed
  "Managesieve state.
Valid states are `closed', `initial', `nonauth', and `auth'.")
(defvar sieve-manage-process nil)
(defvar sieve-manage-capability nil)

;; Internal utility functions

(defmacro sieve-manage-disable-multibyte ()
  "Enable multibyte in the current buffer."
  (unless (featurep 'xemacs)
    '(set-buffer-multibyte nil)))

(defun sieve-manage-erase (&optional p buffer)
  (let ((buffer (or buffer (current-buffer))))
    (and sieve-manage-log
	 (with-current-buffer (get-buffer-create sieve-manage-log)
	   (sieve-manage-disable-multibyte)
	   (buffer-disable-undo)
	   (goto-char (point-max))
	   (insert-buffer-substring buffer (with-current-buffer buffer
					     (point-min))
				    (or p (with-current-buffer buffer
					    (point-max)))))))
  (delete-region (point-min) (or p (point-max))))

(defun sieve-manage-open-1 (buffer)
  (with-current-buffer buffer
    (sieve-manage-erase)
    (setq sieve-manage-state 'initial
	  sieve-manage-process
	  (condition-case ()
	      (funcall (nth 2 (assq sieve-manage-stream
				    sieve-manage-stream-alist))
		       "sieve" buffer sieve-manage-server sieve-manage-port)
	    ((error quit) nil)))
    (when sieve-manage-process
      (while (and (eq sieve-manage-state 'initial)
		  (memq (process-status sieve-manage-process) '(open run)))
	(message "Waiting for response from %s..." sieve-manage-server)
	(accept-process-output sieve-manage-process 1))
      (message "Waiting for response from %s...done" sieve-manage-server)
      (and (memq (process-status sieve-manage-process) '(open run))
	   sieve-manage-process))))

;; Streams

(defun sieve-manage-network-p (buffer)
  t)

(defun sieve-manage-network-open (name buffer server port)
  (let* ((port (or port sieve-manage-default-port))
	 (coding-system-for-read sieve-manage-coding-system-for-read)
	 (coding-system-for-write sieve-manage-coding-system-for-write)
	 (process (open-network-stream name buffer server port)))
    (when process
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-min))
		  (not (sieve-manage-parse-greeting-1)))
	(accept-process-output process 1)
	(sit-for 1))
      (sieve-manage-erase nil buffer)
      (when (memq (process-status process) '(open run))
	process))))

(defun sieve-manage-starttls-p (buffer)
  (condition-case ()
      (progn
	(require 'starttls)
	(call-process "starttls"))
    (error nil)))

(defun sieve-manage-starttls-open (name buffer server port)
  (let* ((port (or port sieve-manage-default-port))
	 (coding-system-for-read sieve-manage-coding-system-for-read)
	 (coding-system-for-write sieve-manage-coding-system-for-write)
	 (process (starttls-open-stream name buffer server port))
	 done)
    (when process
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-min))
		  (not (sieve-manage-parse-greeting-1)))
	(accept-process-output process 1)
	(sit-for 1))
      (sieve-manage-erase nil buffer)
      (sieve-manage-send "STARTTLS")
      (starttls-negotiate process))
    (when (memq (process-status process) '(open run))
      process)))

;; Authenticators
(defun sieve-sasl-auth (buffer mech)
  "Login to server using the SASL MECH method."
  (message "sieve: Authenticating using %s..." mech)
  (with-current-buffer buffer
    (let* ((auth-info (auth-source-search :host sieve-manage-server
                                          :port "sieve"
                                          :max 1
                                          :create t))
           (user-name (or (plist-get (nth 0 auth-info) :user) ""))
           (user-password (or (plist-get (nth 0 auth-info) :secret) ""))
           (user-password (if (functionp user-password)
                              (funcall user-password)
                            user-password))
           (client (sasl-make-client (sasl-find-mechanism (list mech))
                                     user-name "sieve" sieve-manage-server))
           (sasl-read-passphrase
            ;; We *need* to copy the password, because sasl will modify it
            ;; somehow.
            `(lambda (prompt) ,(copy-sequence user-password)))
           (step (sasl-next-step client nil))
           (tag (sieve-manage-send
                 (concat
                  "AUTHENTICATE \""
                  mech
                  "\""
                  (and (sasl-step-data step)
                       (concat
                        " \""
                        (base64-encode-string
                         (sasl-step-data step)
                         'no-line-break)
                        "\"")))))
           data rsp)
      (catch 'done
        (while t
          (setq rsp nil)
          (goto-char (point-min))
          (while (null (or (progn
                             (setq rsp (sieve-manage-is-string))
                             (if (not (and rsp (looking-at
                                                sieve-manage-server-eol)))
                                 (setq rsp nil)
                               (goto-char (match-end 0))
                               rsp))
                           (setq rsp (sieve-manage-is-okno))))
            (accept-process-output sieve-manage-process 1)
            (goto-char (point-min)))
          (sieve-manage-erase)
          (when (sieve-manage-ok-p rsp)
            (when (and (cadr rsp)
                       (string-match "^SASL \"\\([^\"]+\\)\"" (cadr rsp)))
              (sasl-step-set-data
               step (base64-decode-string (match-string 1 (cadr rsp)))))
            (if (and (setq step (sasl-next-step client step))
                     (setq data (sasl-step-data step)))
                ;; We got data for server but it's finished
                (error "Server not ready for SASL data: %s" data)
              ;; The authentication process is finished.
              (throw 'done t)))
          (unless (stringp rsp)
            (error "Server aborted SASL authentication: %s" (caddr rsp)))
          (sasl-step-set-data step (base64-decode-string rsp))
          (setq step (sasl-next-step client step))
          (sieve-manage-send
           (if (sasl-step-data step)
               (concat "\""
                       (base64-encode-string (sasl-step-data step)
                                             'no-line-break)
                       "\"")
             ""))))
      (message "sieve: Login using %s...done" mech))))

(defun sieve-manage-cram-md5-p (buffer)
  (sieve-manage-capability "SASL" "CRAM-MD5" buffer))

(defun sieve-manage-cram-md5-auth (buffer)
  "Login to managesieve server using the CRAM-MD5 SASL method."
  (sieve-sasl-auth buffer "CRAM-MD5"))

(defun sieve-manage-digest-md5-p (buffer)
  (sieve-manage-capability "SASL" "DIGEST-MD5" buffer))

(defun sieve-manage-digest-md5-auth (buffer)
  "Login to managesieve server using the DIGEST-MD5 SASL method."
  (sieve-sasl-auth buffer "DIGEST-MD5"))

(defun sieve-manage-scram-md5-p (buffer)
  (sieve-manage-capability "SASL" "SCRAM-MD5" buffer))

(defun sieve-manage-scram-md5-auth (buffer)
  "Login to managesieve server using the SCRAM-MD5 SASL method."
  (sieve-sasl-auth buffer "SCRAM-MD5"))

(defun sieve-manage-ntlm-p (buffer)
  (sieve-manage-capability "SASL" "NTLM" buffer))

(defun sieve-manage-ntlm-auth (buffer)
  "Login to managesieve server using the NTLM SASL method."
  (sieve-sasl-auth buffer "NTLM"))

(defun sieve-manage-plain-p (buffer)
  (sieve-manage-capability "SASL" "PLAIN" buffer))

(defun sieve-manage-plain-auth (buffer)
  "Login to managesieve server using the PLAIN SASL method."
  (sieve-sasl-auth buffer "PLAIN"))

(defun sieve-manage-login-p (buffer)
  (sieve-manage-capability "SASL" "LOGIN" buffer))

(defun sieve-manage-login-auth (buffer)
  "Login to managesieve server using the LOGIN SASL method."
  (sieve-sasl-auth buffer "LOGIN"))

;; Managesieve API

(defun sieve-manage-open (server &optional port stream auth buffer)
  "Open a network connection to a managesieve SERVER (string).
Optional argument PORT is port number (integer) on remote server.
Optional argument STREAM is any of `sieve-manage-streams' (a symbol).
Optional argument AUTH indicates authenticator to use, see
`sieve-manage-authenticators' for available authenticators.
If nil, chooses the best stream the server is capable of.
Optional argument BUFFER is buffer (buffer, or string naming buffer)
to work in."
  (or port (setq port sieve-manage-default-port))
  (setq buffer (or buffer (format " *sieve* %s:%s" server port)))
  (with-current-buffer (get-buffer-create buffer)
    (mapc 'make-local-variable sieve-manage-local-variables)
    (sieve-manage-disable-multibyte)
    (buffer-disable-undo)
    (setq sieve-manage-server (or server sieve-manage-server))
    (setq sieve-manage-port port)
    (setq sieve-manage-stream (or stream sieve-manage-stream))
    (message "sieve: Connecting to %s..." sieve-manage-server)
    (if (let ((sieve-manage-stream
	       (or sieve-manage-stream sieve-manage-default-stream)))
	  (sieve-manage-open-1 buffer))
	;; Choose stream.
	(let (stream-changed)
	  (message "sieve: Connecting to %s...done" sieve-manage-server)
	  (when (null sieve-manage-stream)
	    (let ((streams sieve-manage-streams))
	      (while (setq stream (pop streams))
		(if (funcall (nth 1 (assq stream
					  sieve-manage-stream-alist)) buffer)
		    (setq stream-changed
			  (not (eq (or sieve-manage-stream
				       sieve-manage-default-stream)
				   stream))
			  sieve-manage-stream stream
			  streams nil)))
	      (unless sieve-manage-stream
		(error "Couldn't figure out a stream for server"))))
	  (when stream-changed
	    (message "sieve: Reconnecting with stream `%s'..."
		     sieve-manage-stream)
	    (sieve-manage-close buffer)
	    (if (sieve-manage-open-1 buffer)
		(message "sieve: Reconnecting with stream `%s'...done"
			 sieve-manage-stream)
	      (message "sieve: Reconnecting with stream `%s'...failed"
		       sieve-manage-stream))
	    (setq sieve-manage-capability nil))
	  (if (sieve-manage-opened buffer)
	      ;; Choose authenticator
	      (when (and (null sieve-manage-auth)
			 (not (eq sieve-manage-state 'auth)))
		(let ((auths sieve-manage-authenticators))
		  (while (setq auth (pop auths))
		    (if (funcall (nth 1 (assq
					 auth
					 sieve-manage-authenticator-alist))
				 buffer)
			(setq sieve-manage-auth auth
			      auths nil)))
		  (unless sieve-manage-auth
		    (error "Couldn't figure out authenticator for server"))))))
      (message "sieve: Connecting to %s...failed" sieve-manage-server))
    (when (sieve-manage-opened buffer)
      (sieve-manage-erase)
      buffer)))

(defun sieve-manage-authenticate (&optional buffer)
  "Authenticate on server in BUFFER.
Return `sieve-manage-state' value."
  (with-current-buffer (or buffer (current-buffer))
    (if (eq sieve-manage-state 'nonauth)
        (when (funcall (nth 2 (assq sieve-manage-auth
                                    sieve-manage-authenticator-alist))
                       (current-buffer))
          (setq sieve-manage-state 'auth))
      sieve-manage-state)))

(defun sieve-manage-opened (&optional buffer)
  "Return non-nil if connection to managesieve server in BUFFER is open.
If BUFFER is nil then the current buffer is used."
  (and (setq buffer (get-buffer (or buffer (current-buffer))))
       (buffer-live-p buffer)
       (with-current-buffer buffer
	 (and sieve-manage-process
	      (memq (process-status sieve-manage-process) '(open run))))))

(defun sieve-manage-close (&optional buffer)
  "Close connection to managesieve server in BUFFER.
If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (when (sieve-manage-opened)
      (sieve-manage-send "LOGOUT")
      (sit-for 1))
    (when (and sieve-manage-process
	       (memq (process-status sieve-manage-process) '(open run)))
      (delete-process sieve-manage-process))
    (setq sieve-manage-process nil)
    (sieve-manage-erase)
    t))

(defun sieve-manage-capability (&optional name value buffer)
  "Check if capability NAME of server BUFFER match VALUE.
If it does, return the server value of NAME. If not returns nil.
If VALUE is nil, do not check VALUE and return server value.
If NAME is nil, return the full server list of capabilities."
  (with-current-buffer (or buffer (current-buffer))
    (if (null name)
	sieve-manage-capability
      (let ((server-value (cadr (assoc name sieve-manage-capability))))
        (when (or (null value)
                  (and server-value
                       (string-match value server-value)))
          server-value)))))

(defun sieve-manage-listscripts (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send "LISTSCRIPTS")
    (sieve-manage-parse-listscripts)))

(defun sieve-manage-havespace (name size &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "HAVESPACE \"%s\" %s" name size))
    (sieve-manage-parse-okno)))

(defun sieve-manage-putscript (name content &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "PUTSCRIPT \"%s\" {%d+}%s%s" name
                               ;; Here we assume that the coding-system will
                               ;; replace each char with a single byte.
                               ;; This is always the case if `content' is
                               ;; a unibyte string.
			       (length content)
			       sieve-manage-client-eol content))
    (sieve-manage-parse-okno)))

(defun sieve-manage-deletescript (name &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "DELETESCRIPT \"%s\"" name))
    (sieve-manage-parse-okno)))

(defun sieve-manage-getscript (name output-buffer &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "GETSCRIPT \"%s\"" name))
    (let ((script (sieve-manage-parse-string)))
      (sieve-manage-parse-crlf)
      (with-current-buffer output-buffer
	(insert script))
      (sieve-manage-parse-okno))))

(defun sieve-manage-setactive (name &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (sieve-manage-send (format "SETACTIVE \"%s\"" name))
    (sieve-manage-parse-okno)))

;; Protocol parsing routines

(defun sieve-manage-ok-p (rsp)
  (string= (downcase (or (car-safe rsp) "")) "ok"))

(defsubst sieve-manage-forward ()
  (or (eobp) (forward-char)))

(defun sieve-manage-is-okno ()
  (when (looking-at (concat
		     "^\\(OK\\|NO\\)\\( (\\([^)]+\\))\\)?\\( \\(.*\\)\\)?"
		     sieve-manage-server-eol))
    (let ((status (match-string 1))
	  (resp-code (match-string 3))
	  (response (match-string 5)))
      (when response
	(goto-char (match-beginning 5))
	(setq response (sieve-manage-is-string)))
      (list status resp-code response))))

(defun sieve-manage-parse-okno ()
  (let (rsp)
    (while (null rsp)
      (accept-process-output (get-buffer-process (current-buffer)) 1)
      (goto-char (point-min))
      (setq rsp (sieve-manage-is-okno)))
    (sieve-manage-erase)
    rsp))

(defun sieve-manage-parse-capability-1 ()
  "Accept a managesieve greeting."
  (let (str)
    (while (setq str (sieve-manage-is-string))
      (if (eq (char-after) ? )
	  (progn
	    (sieve-manage-forward)
	    (push (list str (sieve-manage-is-string))
		  sieve-manage-capability))
	(push (list str) sieve-manage-capability))
      (forward-line)))
  (when (re-search-forward (concat "^OK.*" sieve-manage-server-eol) nil t)
    (setq sieve-manage-state 'nonauth)))

(defalias 'sieve-manage-parse-greeting-1 'sieve-manage-parse-capability-1)

(defun sieve-manage-is-string ()
  (cond ((looking-at "\"\\([^\"]+\\)\"")
	 (prog1
	     (match-string 1)
	   (goto-char (match-end 0))))
	((looking-at (concat "{\\([0-9]+\\+?\\)}" sieve-manage-server-eol))
	 (let ((pos (match-end 0))
	       (len (string-to-number (match-string 1))))
	   (if (< (point-max) (+ pos len))
	       nil
	     (goto-char (+ pos len))
	     (buffer-substring pos (+ pos len)))))))

(defun sieve-manage-parse-string ()
  (let (rsp)
    (while (null rsp)
      (accept-process-output (get-buffer-process (current-buffer)) 1)
      (goto-char (point-min))
      (setq rsp (sieve-manage-is-string)))
    (sieve-manage-erase (point))
    rsp))

(defun sieve-manage-parse-crlf ()
  (when (looking-at sieve-manage-server-eol)
    (sieve-manage-erase (match-end 0))))

(defun sieve-manage-parse-listscripts ()
  (let (tmp rsp data)
    (while (null rsp)
      (while (null (or (setq rsp (sieve-manage-is-okno))
		       (setq tmp (sieve-manage-is-string))))
	(accept-process-output (get-buffer-process (current-buffer)) 1)
	(goto-char (point-min)))
      (when tmp
	(while (not (looking-at (concat "\\( ACTIVE\\)?"
					sieve-manage-server-eol)))
	  (accept-process-output (get-buffer-process (current-buffer)) 1)
	  (goto-char (point-min)))
	(if (match-string 1)
	    (push (cons 'active tmp) data)
	  (push tmp data))
	(goto-char (match-end 0))
	(setq tmp nil)))
    (sieve-manage-erase)
    (if (sieve-manage-ok-p rsp)
	data
      rsp)))

(defun sieve-manage-send (cmdstr)
  (setq cmdstr (concat cmdstr sieve-manage-client-eol))
  (and sieve-manage-log
       (with-current-buffer (get-buffer-create sieve-manage-log)
	 (sieve-manage-disable-multibyte)
	 (buffer-disable-undo)
	 (goto-char (point-max))
	 (insert cmdstr)))
  (process-send-string sieve-manage-process cmdstr))

(provide 'sieve-manage)

;; sieve-manage.el ends here
