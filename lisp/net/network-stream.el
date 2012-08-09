;;; network-stream.el --- open network processes, possibly with encryption

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

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

;; This library provides the function `open-network-stream', which provides a
;; higher-level interface for opening TCP network processes than the built-in
;; function `make-network-process'.  In addition to plain connections, it
;; supports TLS/SSL and STARTTLS connections.

;; Usage example:

;; (open-network-stream
;;  "*nnimap*" buffer address port
;;  :type 'network
;;  :capability-command "1 CAPABILITY\r\n"
;;  :success " OK "
;;  :starttls-function
;;  (lambda (capabilities)
;;    (if (not (string-match "STARTTLS" capabilities))
;;        nil
;;      "1 STARTTLS\r\n")))

;;; Code:

(require 'tls)
(require 'starttls)
(require 'auth-source)

(autoload 'gnutls-negotiate "gnutls")
(autoload 'open-gnutls-stream "gnutls")

;;;###autoload
(defun open-network-stream (name buffer host service &rest parameters)
  "Open a TCP connection to HOST, optionally with encryption.
Normally, return a network process object; with a non-nil
:return-list parameter, return a list instead (see below).
Input and output work as for subprocesses; `delete-process'
closes it.

NAME is the name for the process.  It is modified if necessary to
 make it unique.
BUFFER is a buffer or buffer name to associate with the process.
 Process output goes at end of that buffer.  BUFFER may be nil,
 meaning that the process is not associated with any buffer.
HOST is the name or IP address of the host to connect to.
SERVICE is the name of the service desired, or an integer specifying
 a port number to connect to.

The remaining PARAMETERS should be a sequence of keywords and
values:

:type specifies the connection type, one of the following:
  nil or `network'
             -- Begin with an ordinary network connection, and if
                the parameters :success and :capability-command
                are also supplied, try to upgrade to an encrypted
                connection via STARTTLS.  Even if that
                fails (e.g. if HOST does not support TLS), retain
                an unencrypted connection.
  `plain'    -- An ordinary, unencrypted network connection.
  `starttls' -- Begin with an ordinary connection, and try
                upgrading via STARTTLS.  If that fails for any
                reason, drop the connection; in that case the
                returned object is a killed process.
  `tls'      -- A TLS connection.
  `ssl'      -- Equivalent to `tls'.
  `shell'    -- A shell connection.

:return-list specifies this function's return value.
  If omitted or nil, return a process object.  A non-nil means to
  return (PROC . PROPS), where PROC is a process object and PROPS
  is a plist of connection properties, with these keywords:
   :greeting -- the greeting returned by HOST (a string), or nil.
   :capabilities -- a string representing HOST's capabilities,
                    or nil if none could be found.
   :type -- the resulting connection type; `plain' (unencrypted)
            or `tls' (TLS-encrypted).

:end-of-command specifies a regexp matching the end of a command.

:end-of-capability specifies a regexp matching the end of the
  response to the command specified for :capability-command.
  It defaults to the regexp specified for :end-of-command.

:success specifies a regexp matching a message indicating a
  successful STARTTLS negotiation.  For instance, the default
  should be \"^3\" for an NNTP connection.

:capability-command specifies a command used to query the HOST
  for its capabilities.  For instance, for IMAP this should be
  \"1 CAPABILITY\\r\\n\".

:starttls-function specifies a function for handling STARTTLS.
  This function should take one parameter, the response to the
  capability command, and should return the command to switch on
  STARTTLS if the server supports STARTTLS, and nil otherwise.

:always-query-capabilities says whether to query the server for
  capabilities, even if we're doing a `plain' network connection.

:client-certificate should either be a list where the first
  element is the certificate key file name, and the second
  element is the certificate file name itself, or `t', which
  means that `auth-source' will be queried for the key and the
  certificate.  This parameter will only be used when doing TLS
  or STARTTLS connections.

:use-starttls-if-possible is a boolean that says to do opportunistic
STARTTLS upgrades even if Emacs doesn't have built-in TLS functionality.

:nowait is a boolean that says the connection should be made
  asynchronously, if possible."
  (unless (featurep 'make-network-process)
    (error "Emacs was compiled without networking support"))
  (let ((type (plist-get parameters :type))
	(return-list (plist-get parameters :return-list)))
    (if (and (not return-list)
	     (or (eq type 'plain)
		 (and (memq type '(nil network))
		      (not (and (plist-get parameters :success)
				(plist-get parameters :capability-command))))))
	;; The simplest case: wrapper around `make-network-process'.
	(make-network-process :name name :buffer buffer
			      :host host :service service
			      :nowait (plist-get parameters :nowait))
      (let ((work-buffer (or buffer
			     (generate-new-buffer " *stream buffer*")))
	    (fun (cond ((and (eq type 'plain)
			     (not (plist-get parameters
					     :always-query-capabilities)))
			'network-stream-open-plain)
		       ((memq type '(nil network starttls plain))
			'network-stream-open-starttls)
		       ((memq type '(tls ssl)) 'network-stream-open-tls)
		       ((eq type 'shell) 'network-stream-open-shell)
		       (t (error "Invalid connection type %s" type))))
	    result)
	(unwind-protect
	    (setq result (funcall fun name work-buffer host service parameters))
	  (unless buffer
	    (and (processp (car result))
		 (set-process-buffer (car result) nil))
	    (kill-buffer work-buffer)))
	(if return-list
	    (list (car result)
		  :greeting     (nth 1 result)
		  :capabilities (nth 2 result)
		  :type         (nth 3 result)
		  :error        (nth 4 result))
	  (car result))))))

(defun network-stream-certificate (host service parameters)
  (let ((spec (plist-get :client-certificate parameters)))
    (cond
     ((listp spec)
      ;; Either nil or a list with a key/certificate pair.
      spec)
     ((eq spec t)
      (let* ((auth-info
	      (car (auth-source-search :max 1
				       :host host
				       :port service)))
	     (key (plist-get auth-info :key))
	     (cert (plist-get auth-info :cert)))
	(and key cert
	     (list key cert)))))))

;;;###autoload
(defalias 'open-protocol-stream 'open-network-stream)

(defun network-stream-open-plain (name buffer host service parameters)
  (let ((start (with-current-buffer buffer (point)))
	(stream (make-network-process :name name :buffer buffer
				      :host host :service service
				      :nowait (plist-get parameters :nowait))))
    (list stream
	  (network-stream-get-response stream start
				       (plist-get parameters :end-of-command))
	  nil
	  'plain)))

(defun network-stream-open-starttls (name buffer host service parameters)
  (let* ((start (with-current-buffer buffer (point)))
	 (require-tls    (eq (plist-get parameters :type) 'starttls))
	 (starttls-function  (plist-get parameters :starttls-function))
	 (success-string     (plist-get parameters :success))
	 (capability-command (plist-get parameters :capability-command))
	 (eoc                (plist-get parameters :end-of-command))
	 (eo-capa            (or (plist-get parameters :end-of-capability)
				 eoc))
	 ;; Return (STREAM GREETING CAPABILITIES RESULTING-TYPE)
	 (stream (make-network-process :name name :buffer buffer
				       :host host :service service))
	 (greeting (network-stream-get-response stream start eoc))
	 (capabilities (network-stream-command stream capability-command
					       eo-capa))
	 (resulting-type 'plain)
	 (builtin-starttls (and (fboundp 'gnutls-available-p)
				(gnutls-available-p)))
	 starttls-available starttls-command error)

    ;; First check whether the server supports STARTTLS at all.
    (when (and capabilities success-string starttls-function)
      (setq starttls-command
	    (funcall starttls-function capabilities)))
    ;; If we have built-in STARTTLS support, try to upgrade the
    ;; connection.
    (when (and starttls-command
	       (setq starttls-available
		     (or builtin-starttls
			 (and (or require-tls
				  (plist-get parameters :use-starttls-if-possible))
			      (starttls-available-p))))
	       (not (eq (plist-get parameters :type) 'plain)))
      ;; If using external STARTTLS, drop this connection and start
      ;; anew with `starttls-open-stream'.
      (unless builtin-starttls
	(delete-process stream)
	(setq start (with-current-buffer buffer (point-max)))
	(let* ((starttls-extra-arguments
		(if require-tls
		    starttls-extra-arguments
		  ;; For opportunistic TLS upgrades, we don't really
		  ;; care about the identity of the peer.
		  (cons "--insecure" starttls-extra-arguments)))
	       (starttls-extra-args starttls-extra-args)
	       (cert (network-stream-certificate host service parameters)))
	  ;; There are client certificates requested, so add them to
	  ;; the command line.
	  (when cert
	    (setq starttls-extra-arguments
		  (nconc (list "--x509keyfile" (expand-file-name (nth 0 cert))
			       "--x509certfile" (expand-file-name (nth 1 cert)))
			 starttls-extra-arguments)
		  starttls-extra-args
		  (nconc (list "--key-file" (expand-file-name (nth 0 cert))
			       "--cert-file" (expand-file-name (nth 1 cert)))
			 starttls-extra-args)))
	  (setq stream (starttls-open-stream name buffer host service)))
	(network-stream-get-response stream start eoc)
	;; Requery capabilities for protocols that require it; i.e.,
	;; EHLO for SMTP.
	(when (plist-get parameters :always-query-capabilities)
	  (network-stream-command stream capability-command eo-capa)))
      (when (string-match success-string
			  (network-stream-command stream starttls-command eoc))
	;; The server said it was OK to begin STARTTLS negotiations.
	(if builtin-starttls
	    (let ((cert (network-stream-certificate host service parameters)))
	      (condition-case nil
		  (gnutls-negotiate :process stream :hostname host
				    :keylist (and cert (list cert)))
		;; If we get a gnutls-specific error (for instance if
		;; the certificate the server gives us is completely
		;; syntactically invalid), then close the connection
		;; and possibly (further down) try to create a
		;; non-encrypted connection.
		(gnutls-error
		 (delete-process stream))))
	  (unless (starttls-negotiate stream)
	    (delete-process stream)))
	(if (memq (process-status stream) '(open run))
	    (setq resulting-type 'tls)
	  ;; We didn't successfully negotiate STARTTLS; if TLS
	  ;; isn't demanded, reopen an unencrypted connection.
	  (unless require-tls
	    (setq stream
		  (make-network-process :name name :buffer buffer
					:host host :service service))
	    (network-stream-get-response stream start eoc)))
	;; Re-get the capabilities, which may have now changed.
	(setq capabilities
	      (network-stream-command stream capability-command eo-capa))))

    ;; If TLS is mandatory, close the connection if it's unencrypted.
    (when (and require-tls
	       ;; ... but Emacs wasn't able to -- either no built-in
	       ;; support, or no gnutls-cli installed.
	       (eq resulting-type 'plain))
      (setq error
	    (if (or (null starttls-command)
		    starttls-available)
		"Server does not support TLS"
	      ;; See `starttls-available-p'.  If this predicate
	      ;; changes to allow running under Windows, the error
	      ;; message below should be amended.
	      (if (memq system-type '(windows-nt ms-dos))
		  (concat "Emacs does not support TLS")
		(concat "Emacs does not support TLS, and no external `"
			(if starttls-use-gnutls
			    starttls-gnutls-program
			  starttls-program)
			"' program was found"))))
      (delete-process stream)
      (setq stream nil))
    ;; Return value:
    (list stream greeting capabilities resulting-type error)))

(defun network-stream-command (stream command eoc)
  (when command
    (let ((start (with-current-buffer (process-buffer stream) (point-max))))
      (process-send-string stream command)
      (network-stream-get-response stream start eoc))))

(defun network-stream-get-response (stream start end-of-command)
  (when end-of-command
    (with-current-buffer (process-buffer stream)
      (save-excursion
	(goto-char start)
	(while (and (memq (process-status stream) '(open run))
		    (not (re-search-forward end-of-command nil t)))
	  (accept-process-output stream 0 50)
	  (goto-char start))
	;; Return the data we got back, or nil if the process died.
	(unless (= start (point))
	  (buffer-substring start (point)))))))

(defun network-stream-open-tls (name buffer host service parameters)
  (with-current-buffer buffer
    (let* ((start (point-max))
	   (use-builtin-gnutls (and (fboundp 'gnutls-available-p)
				    (gnutls-available-p)))
	   (stream
	    (funcall (if use-builtin-gnutls
			 'open-gnutls-stream
		       'open-tls-stream)
		     name buffer host service))
	   (eoc (plist-get parameters :end-of-command)))
      (if (null stream)
	  (list nil nil nil 'plain)
	;; If we're using tls.el, we have to delete the output from
	;; openssl/gnutls-cli.
	(when (and (null use-builtin-gnutls)
		   eoc)
	  (network-stream-get-response stream start eoc)
	  (goto-char (point-min))
	  (when (re-search-forward eoc nil t)
	    (goto-char (match-beginning 0))
	    (delete-region (point-min) (line-beginning-position))))
	(let* ((capability-command (plist-get parameters :capability-command)))
	  (list stream
		(network-stream-get-response stream start eoc)
		(network-stream-command stream capability-command eoc)
		'tls))))))

(defun network-stream-open-shell (name buffer host service parameters)
  (require 'format-spec)
  (let* ((capability-command (plist-get parameters :capability-command))
	 (eoc 		     (plist-get parameters :end-of-command))
	 (start (with-current-buffer buffer (point)))
	 (stream (let ((process-connection-type nil))
		   (start-process name buffer shell-file-name
				  shell-command-switch
				  (format-spec
				   (plist-get parameters :shell-command)
				   (format-spec-make
				    ?s host
				    ?p service))))))
    (list stream
	  (network-stream-get-response stream start eoc)
	  (network-stream-command stream capability-command
				  (or (plist-get parameters :end-of-capability)
				      eoc))
	  'plain)))

(provide 'network-stream)

;;; network-stream.el ends here
