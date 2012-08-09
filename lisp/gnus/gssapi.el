;;; gssapi.el --- GSSAPI/Kerberos 5 interface for Emacs

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;;         Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(require 'format-spec)

(defcustom gssapi-program (list
			   (concat "gsasl %s %p "
				   "--mechanism GSSAPI "
				   "--authentication-id %l")
			   "imtest -m gssapi -u %l -p %p %s")
  "List of strings containing commands for GSSAPI (krb5) authentication.
%s is replaced with server hostname, %p with port to connect to,
and %l with the user name.  The program should accept commands on
stdin and return responses to stdout.  Each entry in the list is
tried until a successful connection is made."
  :version "24.1"
  :group 'network
  :type '(repeat string))

(defun open-gssapi-stream (name buffer server port user)
  (let ((cmds gssapi-program)
	cmd done)
    (with-current-buffer buffer
      (while (and (not done)
		  (setq cmd (pop cmds)))
	(message "Opening GSSAPI connection with `%s'..." cmd)
	(erase-buffer)
	(let* ((coding-system-for-read 'binary)
	       (coding-system-for-write 'binary)
	       (process (start-process
			 name buffer shell-file-name shell-command-switch
			 (format-spec
			  cmd
			  (format-spec-make
			   ?s server
			   ?p (number-to-string port)
			   ?l user))))
	       response)
	  (when process
	    (while (and (memq (process-status process) '(open run))
			(goto-char (point-min))
			;; Athena IMTEST can output SSL verify errors
			(or (while (looking-at "^verify error:num=")
			      (forward-line))
			    t)
			(or (while (looking-at "^TLS connection established")
			      (forward-line))
			    t)
			;; cyrus 1.6.x (13? < x <= 22) queries capabilities
			(or (while (looking-at "^C:")
			      (forward-line))
			    t)
			;; cyrus 1.6 imtest print "S: " before server greeting
			(or (not (looking-at "S: "))
			    (forward-char 3)
			    t)
			;; GNU SASL may print 'Trying ...' first.
			(or (not (looking-at "Trying "))
			    (forward-line)
			    t)
			(not (and (looking-at "\\* \\(OK\\|PREAUTH\\|BYE\\) ")
				  ;; success in imtest 1.6:
				  (re-search-forward
				   (concat "^\\(\\(Authenticat.*\\)\\|\\("
					   "Client authentication "
					   "finished.*\\)\\)")
				   nil t)
				  (setq response (match-string 1)))))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (erase-buffer)
	    (message "GSSAPI connection: %s" (or response "failed"))
	    (if (and response (let ((case-fold-search nil))
				(not (string-match "failed" response))))
		(setq done process)
	      (delete-process process)
	      nil))))
      done)))

(provide 'gssapi)

;;; gssapi.el ends here
