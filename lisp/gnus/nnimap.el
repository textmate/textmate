;;; nnimap.el --- IMAP interface for Gnus

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;         Simon Josefsson <simon@josefsson.org>

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

;; nnimap interfaces Gnus with IMAP servers.

;;; Code:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-and-compile
  (require 'nnheader)
  ;; In Emacs 24, `open-protocol-stream' is an autoloaded alias for
  ;; `make-network-stream'.
  (unless (fboundp 'open-protocol-stream)
    (require 'proto-stream)))

(eval-when-compile
  (require 'cl))

(require 'nnheader)
(require 'gnus-util)
(require 'gnus)
(require 'nnoo)
(require 'netrc)
(require 'utf7)
(require 'tls)
(require 'parse-time)
(require 'nnmail)

(autoload 'auth-source-forget+ "auth-source")
(autoload 'auth-source-search "auth-source")

(nnoo-declare nnimap)

(defvoo nnimap-address nil
  "The address of the IMAP server.")

(defvoo nnimap-user nil
  "Username to use for authentication to the IMAP server.")

(defvoo nnimap-server-port nil
  "The IMAP port used.
If nnimap-stream is `ssl', this will default to `imaps'.  If not,
it will default to `imap'.")

(defvoo nnimap-stream 'undecided
  "How nnimap talks to the IMAP server.
The value should be either `undecided', `ssl' or `tls',
`network', `starttls', `plain', or `shell'.

If the value is `undecided', nnimap tries `ssl' first, then falls
back on `network'.")

(defvoo nnimap-shell-program (if (boundp 'imap-shell-program)
				 (if (listp imap-shell-program)
				     (car imap-shell-program)
				   imap-shell-program)
			       "ssh %s imapd"))

(defvoo nnimap-inbox nil
  "The mail box where incoming mail arrives and should be split out of.
For example, \"INBOX\".")

(defvoo nnimap-split-methods nil
  "How mail is split.
Uses the same syntax as `nnmail-split-methods'.")

(defvoo nnimap-split-fancy nil
  "Uses the same syntax as `nnmail-split-fancy'.")

(defvoo nnimap-unsplittable-articles '(%Deleted %Seen)
  "Articles with the flags in the list will not be considered when splitting.")

(make-obsolete-variable 'nnimap-split-rule "see `nnimap-split-methods'"
			"Emacs 24.1")

(defvoo nnimap-authenticator nil
  "How nnimap authenticate itself to the server.
Possible choices are nil (use default methods) or `anonymous'.")

(defvoo nnimap-expunge t
  "If non-nil, expunge articles after deleting them.
This is always done if the server supports UID EXPUNGE, but it's
not done by default on servers that doesn't support that command.")

(defvoo nnimap-streaming t
  "If non-nil, try to use streaming commands with IMAP servers.
Switching this off will make nnimap slower, but it helps with
some servers.")

(defvoo nnimap-connection-alist nil)

(defvoo nnimap-current-infos nil)

(defvoo nnimap-fetch-partial-articles nil
  "If non-nil, Gnus will fetch partial articles.
If t, nnimap will fetch only the first part.  If a string, it
will fetch all parts that have types that match that string.  A
likely value would be \"text/\" to automatically fetch all
textual parts.")

(defvar nnimap-process nil)

(defvar nnimap-status-string "")

(defvar nnimap-split-download-body-default nil
  "Internal variable with default value for `nnimap-split-download-body'.")

(defvar nnimap-keepalive-timer nil)
(defvar nnimap-process-buffers nil)

(defstruct nnimap
  group process commands capabilities select-result newlinep server
  last-command-time greeting examined stream-type initial-resync)

(defvar nnimap-object nil)

(defvar nnimap-mark-alist
  '((read "\\Seen" %Seen)
    (tick "\\Flagged" %Flagged)
    (reply "\\Answered" %Answered)
    (expire "gnus-expire")
    (dormant "gnus-dormant")
    (score "gnus-score")
    (save "gnus-save")
    (download "gnus-download")
    (forward "gnus-forward")))

(defvar nnimap-quirks
  '(("QRESYNC" "Zimbra" "QRESYNC ")))

(defvar nnimap-inhibit-logging nil)

(defun nnimap-buffer ()
  (nnimap-find-process-buffer nntp-server-buffer))

(defun nnimap-header-parameters ()
  (format "(UID RFC822.SIZE BODYSTRUCTURE %s)"
	  (format
	   (if (nnimap-ver4-p)
	       "BODY.PEEK[HEADER.FIELDS %s]"
	     "RFC822.HEADER.LINES %s")
	   (append '(Subject From Date Message-Id
			     References In-Reply-To Xref)
		   nnmail-extra-headers))))

(deffoo nnimap-retrieve-headers (articles &optional group server fetch-old)
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (when (nnimap-possibly-change-group group server)
      (with-current-buffer (nnimap-buffer)
	(erase-buffer)
	(nnimap-wait-for-response
	 (nnimap-send-command
	  "UID FETCH %s %s"
	  (nnimap-article-ranges (gnus-compress-sequence articles))
	  (nnimap-header-parameters))
	 t)
	(nnimap-transform-headers)
	(nnheader-remove-cr-followed-by-lf))
      (insert-buffer-substring
       (nnimap-find-process-buffer (current-buffer))))
    'headers))

(defun nnimap-transform-headers ()
  (goto-char (point-min))
  (let (article lines size string)
    (block nil
      (while (not (eobp))
	(while (not (looking-at "\\* [0-9]+ FETCH"))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (when (eobp)
	    (return)))
	(goto-char (match-end 0))
	;; Unfold quoted {number} strings.
	(while (re-search-forward
		"[^]][ (]{\\([0-9]+\\)}\r?\n"
		(save-excursion
		  ;; Start of the header section.
		  (or (re-search-forward "] {[0-9]+}\r?\n" nil t)
		      ;; Start of the next FETCH.
		      (re-search-forward "\\* [0-9]+ FETCH" nil t)
		      (point-max)))
		t)
	  (setq size (string-to-number (match-string 1)))
	  (delete-region (+ (match-beginning 0) 2) (point))
	  (setq string (buffer-substring (point) (+ (point) size)))
	  (delete-region (point) (+ (point) size))
	  (insert (format "%S" (mm-subst-char-in-string ?\n ?\s string))))
	(beginning-of-line)
	(setq article
	      (and (re-search-forward "UID \\([0-9]+\\)" (line-end-position)
				      t)
		   (match-string 1)))
	(setq lines nil)
	(setq size
	      (and (re-search-forward "RFC822.SIZE \\([0-9]+\\)"
				      (line-end-position)
				      t)
		   (match-string 1)))
	(beginning-of-line)
	(when (search-forward "BODYSTRUCTURE" (line-end-position) t)
	  (let ((structure (ignore-errors
			     (read (current-buffer)))))
	    (while (and (consp structure)
			(not (atom (car structure))))
	      (setq structure (car structure)))
	    (setq lines (if (and
			     (stringp (car structure))
			     (equal (upcase (nth 0 structure)) "MESSAGE")
			     (equal (upcase (nth 1 structure)) "RFC822"))
			    (nth 9 structure)
			  (nth 7 structure)))))
	(delete-region (line-beginning-position) (line-end-position))
	(insert (format "211 %s Article retrieved." article))
	(forward-line 1)
	(when size
	  (insert (format "Chars: %s\n" size)))
	(when lines
	  (insert (format "Lines: %s\n" lines)))
	(unless (re-search-forward "^\r$" nil t)
	  (goto-char (point-max)))
	(delete-region (line-beginning-position) (line-end-position))
	(insert ".")
	(forward-line 1)))))

(defun nnimap-unfold-quoted-lines ()
  ;; Unfold quoted {number} strings.
  (let (size string)
    (while (re-search-forward " {\\([0-9]+\\)}\r?\n" nil t)
      (setq size (string-to-number (match-string 1)))
      (delete-region (1+ (match-beginning 0)) (point))
      (setq string (buffer-substring (point) (+ (point) size)))
      (delete-region (point) (+ (point) size))
      (insert (format "%S" string)))))

(defun nnimap-get-length ()
  (and (re-search-forward "{\\([0-9]+\\)}" (line-end-position) t)
       (string-to-number (match-string 1))))

(defun nnimap-article-ranges (ranges)
  (let (result)
    (cond
     ((numberp ranges)
      (number-to-string ranges))
     ((numberp (cdr ranges))
      (format "%d:%d" (car ranges) (cdr ranges)))
     (t
      (dolist (elem ranges)
	(push
	 (if (consp elem)
	     (format "%d:%d" (car elem) (cdr elem))
	   (number-to-string elem))
	 result))
      (mapconcat #'identity (nreverse result) ",")))))

(deffoo nnimap-open-server (server &optional defs no-reconnect)
  (if (nnimap-server-opened server)
      t
    (unless (assq 'nnimap-address defs)
      (setq defs (append defs (list (list 'nnimap-address server)))))
    (nnoo-change-server 'nnimap server defs)
    (if no-reconnect
	(nnimap-find-connection nntp-server-buffer)
      (or (nnimap-find-connection nntp-server-buffer)
	  (nnimap-open-connection nntp-server-buffer)))))

(defun nnimap-make-process-buffer (buffer)
  (with-current-buffer
      (generate-new-buffer (format " *nnimap %s %s %s*"
				   nnimap-address nnimap-server-port
				   (gnus-buffer-exists-p buffer)))
    (mm-disable-multibyte)
    (buffer-disable-undo)
    (gnus-add-buffer)
    (set (make-local-variable 'after-change-functions) nil)
    (set (make-local-variable 'nnimap-object)
	 (make-nnimap :server (nnoo-current-server 'nnimap)
		      :initial-resync 0))
    (push (list buffer (current-buffer)) nnimap-connection-alist)
    (push (current-buffer) nnimap-process-buffers)
    (current-buffer)))

(defun nnimap-credentials (address ports user)
  (let* ((auth-source-creation-prompts
          '((user  . "IMAP user at %h: ")
            (secret . "IMAP password for %u@%h: ")))
         (found (nth 0 (auth-source-search :max 1
                                           :host address
                                           :port ports
                                           :user user
                                           :require '(:user :secret)
                                           :create t))))
    (if found
        (list (plist-get found :user)
	      (let ((secret (plist-get found :secret)))
		(if (functionp secret)
		    (funcall secret)
		  secret))
	      (plist-get found :save-function))
      nil)))

(defun nnimap-keepalive ()
  (let ((now (current-time)))
    (dolist (buffer nnimap-process-buffers)
      (when (buffer-name buffer)
	(with-current-buffer buffer
	  (when (and nnimap-object
		     (nnimap-last-command-time nnimap-object)
		     (> (gnus-float-time
			 (time-subtract
			  now
			  (nnimap-last-command-time nnimap-object)))
			;; More than five minutes since the last command.
			(* 5 60)))
	    (nnimap-send-command "NOOP")))))))

(defun nnimap-open-connection (buffer)
  ;; Be backwards-compatible -- the earlier value of nnimap-stream was
  ;; `ssl' when nnimap-server-port was nil.  Sort of.
  (when (and nnimap-server-port
	     (eq nnimap-stream 'undecided))
    (setq nnimap-stream 'ssl))
  (let ((stream
	 (if (eq nnimap-stream 'undecided)
	     (loop for type in '(ssl network)
		   for stream = (let ((nnimap-stream type))
				  (nnimap-open-connection-1 buffer))
		   while (eq stream 'no-connect)
		   finally (return stream))
	   (nnimap-open-connection-1 buffer))))
    (if (eq stream 'no-connect)
	nil
      stream)))

(defun nnimap-map-port (port)
  (if (equal port "imaps")
      "993"
    port))

(defun nnimap-open-connection-1 (buffer)
  (unless nnimap-keepalive-timer
    (setq nnimap-keepalive-timer (run-at-time (* 60 15) (* 60 15)
					      'nnimap-keepalive)))
  (with-current-buffer (nnimap-make-process-buffer buffer)
    (let* ((coding-system-for-read 'binary)
	   (coding-system-for-write 'binary)
	   (ports
	    (cond
	     ((memq nnimap-stream '(network plain starttls))
	      (nnheader-message 7 "Opening connection to %s..."
				nnimap-address)
	      '("imap" "143"))
	     ((eq nnimap-stream 'shell)
	      (nnheader-message 7 "Opening connection to %s via shell..."
				nnimap-address)
	      '("imap"))
	     ((memq nnimap-stream '(ssl tls))
	      (nnheader-message 7 "Opening connection to %s via tls..."
				nnimap-address)
	      '("imaps" "imap" "993" "143"))
	     (t
	      (error "Unknown stream type: %s" nnimap-stream))))
           login-result credentials)
      (when nnimap-server-port
	(push nnimap-server-port ports))
      (let* ((stream-list
	      (open-protocol-stream
	       "*nnimap*" (current-buffer) nnimap-address
	       (nnimap-map-port (car ports))
	       :type nnimap-stream
	       :return-list t
	       :shell-command nnimap-shell-program
	       :capability-command "1 CAPABILITY\r\n"
	       :end-of-command "\r\n"
	       :success " OK "
	       :starttls-function
	       (lambda (capabilities)
		 (when (gnus-string-match-p "STARTTLS" capabilities)
		   "1 STARTTLS\r\n"))))
	     (stream (car stream-list))
	     (props (cdr stream-list))
	     (greeting (plist-get props :greeting))
	     (capabilities (plist-get props :capabilities))
	     (stream-type (plist-get props :type)))
	(when (and stream (not (memq (process-status stream) '(open run))))
	  (setq stream nil))

        (when (and (fboundp 'set-network-process-option) ;; Not in XEmacs.
                   (fboundp 'process-type) ;; Emacs 22 doesn't provide it.
                   (eq (process-type stream) 'network))
          ;; Use TCP-keepalive so that connections that pass through a NAT
          ;; router don't hang when left idle.
          (set-network-process-option stream :keepalive t))

	(setf (nnimap-process nnimap-object) stream)
	(setf (nnimap-stream-type nnimap-object) stream-type)
	(if (not stream)
	    (progn
	      (nnheader-report 'nnimap "Unable to contact %s:%s via %s"
			       nnimap-address (car ports) nnimap-stream)
	      'no-connect)
	  (gnus-set-process-query-on-exit-flag stream nil)
	  (if (not (gnus-string-match-p "[*.] \\(OK\\|PREAUTH\\)" greeting))
	      (nnheader-report 'nnimap "%s" greeting)
	    ;; Store the greeting (for debugging purposes).
	    (setf (nnimap-greeting nnimap-object) greeting)
	    (setf (nnimap-capabilities nnimap-object)
		  (mapcar #'upcase
			  (split-string capabilities)))
	    (unless (gnus-string-match-p "[*.] PREAUTH" greeting)
	      (if (not (setq credentials
			     (if (eq nnimap-authenticator 'anonymous)
				 (list "anonymous"
				       (message-make-address))
                               ;; Look for the credentials based on
                               ;; the virtual server name and the address
                               (nnimap-credentials
				(gnus-delete-duplicates
				 (list
				  nnimap-address
				  (nnoo-current-server 'nnimap)))
                                ports
                                nnimap-user))))
		  (setq nnimap-object nil)
		(let ((nnimap-inhibit-logging t))
		  (setq login-result
			(nnimap-login (car credentials) (cadr credentials))))
		(if (car login-result)
		    (progn
		      ;; Save the credentials if a save function exists
		      ;; (such a function will only be passed if a new
		      ;; token was created).
		      (when (functionp (nth 2 credentials))
			(funcall (nth 2 credentials)))
		      ;; See if CAPABILITY is set as part of login
		      ;; response.
		      (dolist (response (cddr login-result))
			(when (string= "CAPABILITY" (upcase (car response)))
			  (setf (nnimap-capabilities nnimap-object)
				(mapcar #'upcase (cdr response))))))
		  ;; If the login failed, then forget the credentials
		  ;; that are now possibly cached.
		  (dolist (host (list (nnoo-current-server 'nnimap)
				      nnimap-address))
		    (dolist (port ports)
                      (auth-source-forget+ :host host :port port)))
		  (delete-process (nnimap-process nnimap-object))
		  (setq nnimap-object nil))))
	    (when nnimap-object
	      (when (nnimap-capability "QRESYNC")
		(nnimap-command "ENABLE QRESYNC"))
	      (nnimap-process nnimap-object))))))))

(autoload 'rfc2104-hash "rfc2104")

(defun nnimap-login (user password)
  (cond
   ;; Prefer plain LOGIN if it's enabled (since it requires fewer
   ;; round trips than CRAM-MD5, and it's less likely to be buggy),
   ;; and we're using an encrypted connection.
   ((and (not (nnimap-capability "LOGINDISABLED"))
	 (eq (nnimap-stream-type nnimap-object) 'tls))
    (nnimap-command "LOGIN %S %S" user password))
   ((nnimap-capability "AUTH=CRAM-MD5")
    (erase-buffer)
    (let ((sequence (nnimap-send-command "AUTHENTICATE CRAM-MD5"))
	  (challenge (nnimap-wait-for-line "^\\+\\(.*\\)\n")))
      (process-send-string
       (get-buffer-process (current-buffer))
       (concat
	(base64-encode-string
	 (concat user " "
		 (rfc2104-hash 'md5 64 16 password
			       (base64-decode-string challenge))))
	"\r\n"))
      (nnimap-wait-for-response sequence)))
   ((not (nnimap-capability "LOGINDISABLED"))
    (nnimap-command "LOGIN %S %S" user password))
   ((nnimap-capability "AUTH=PLAIN")
    (nnimap-command
     "AUTHENTICATE PLAIN %s"
     (base64-encode-string
      (format "\000%s\000%s"
	      (nnimap-quote-specials user)
	      (nnimap-quote-specials password)))))))

(defun nnimap-quote-specials (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[\\\"]" nil t)
      (forward-char -1)
      (insert "\\")
      (forward-char 1))
    (buffer-string)))

(defun nnimap-find-parameter (parameter elems)
  (let (result)
    (dolist (elem elems)
      (cond
       ((equal (car elem) parameter)
	(setq result (cdr elem)))
       ((and (equal (car elem) "OK")
	     (consp (cadr elem))
	     (equal (caadr elem) parameter))
	(setq result (cdr (cadr elem))))))
    result))

(deffoo nnimap-close-server (&optional server)
  (when (nnoo-change-server 'nnimap server nil)
    (ignore-errors
      (delete-process (get-buffer-process (nnimap-buffer))))
    (nnoo-close-server 'nnimap server)
    t))

(deffoo nnimap-request-close ()
  t)

(deffoo nnimap-server-opened (&optional server)
  (and (nnoo-current-server-p 'nnimap server)
       nntp-server-buffer
       (gnus-buffer-live-p nntp-server-buffer)
       (nnimap-find-connection nntp-server-buffer)))

(deffoo nnimap-status-message (&optional server)
  nnimap-status-string)

(deffoo nnimap-request-article (article &optional group server to-buffer)
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (with-current-buffer nntp-server-buffer
    (let ((result (nnimap-possibly-change-group group server))
	  parts structure)
      (when (stringp article)
	(setq article (nnimap-find-article-by-message-id group article)))
      (when (and result
		 article)
	(erase-buffer)
	(with-current-buffer (nnimap-buffer)
	  (erase-buffer)
	  (when nnimap-fetch-partial-articles
	    (nnimap-command "UID FETCH %d (BODYSTRUCTURE)" article)
	    (goto-char (point-min))
	    (when (re-search-forward "FETCH.*BODYSTRUCTURE" nil t)
	      (setq structure (ignore-errors
				(let ((start (point)))
				  (forward-sexp 1)
				  (downcase-region start (point))
				  (goto-char start)
				  (read (current-buffer))))
		    parts (nnimap-find-wanted-parts structure))))
	  (when (if parts
		    (nnimap-get-partial-article article parts structure)
		  (nnimap-get-whole-article article))
	    (let ((buffer (current-buffer)))
	      (with-current-buffer (or to-buffer nntp-server-buffer)
		(nnheader-insert-buffer-substring buffer)
		(nnheader-ms-strip-cr)))
	    (cons group article)))))))

(deffoo nnimap-request-head (article &optional group server to-buffer)
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (when (nnimap-possibly-change-group group server)
    (with-current-buffer (nnimap-buffer)
      (when (stringp article)
	(setq article (nnimap-find-article-by-message-id group article)))
      (if (null article)
	  nil
	(nnimap-get-whole-article
	 article (format "UID FETCH %%d %s"
			 (nnimap-header-parameters)))
	(let ((buffer (current-buffer)))
	  (with-current-buffer (or to-buffer nntp-server-buffer)
	    (erase-buffer)
	    (insert-buffer-substring buffer)
	    (nnheader-ms-strip-cr)
	    (cons group article)))))))

(defun nnimap-get-whole-article (article &optional command)
  (let ((result
	 (nnimap-command
	  (or command
	      (if (nnimap-ver4-p)
		  "UID FETCH %d BODY.PEEK[]"
		"UID FETCH %d RFC822.PEEK"))
	  article)))
    ;; Check that we really got an article.
    (goto-char (point-min))
    (unless (re-search-forward "\\* [0-9]+ FETCH" nil t)
      (setq result nil))
    (when result
      ;; Remove any data that may have arrived before the FETCH data.
      (beginning-of-line)
      (unless (bobp)
	(delete-region (point-min) (point)))
      (let ((bytes (nnimap-get-length)))
	(delete-region (line-beginning-position)
		       (progn (forward-line 1) (point)))
	(goto-char (+ (point) bytes))
	(delete-region (point) (point-max)))
      t)))

(defun nnimap-capability (capability)
  (member capability (nnimap-capabilities nnimap-object)))

(defun nnimap-ver4-p ()
  (nnimap-capability "IMAP4REV1"))

(defun nnimap-get-partial-article (article parts structure)
  (let ((result
	 (nnimap-command
	  "UID FETCH %d (%s %s)"
	  article
	  (if (nnimap-ver4-p)
	      "BODY.PEEK[HEADER]"
	    "RFC822.HEADER")
	  (if (nnimap-ver4-p)
	      (mapconcat (lambda (part)
			   (format "BODY.PEEK[%s]" part))
			 parts " ")
	    (mapconcat (lambda (part)
			 (format "RFC822.PEEK[%s]" part))
		       parts " ")))))
    (when result
      (nnimap-convert-partial-article structure))))

(defun nnimap-convert-partial-article (structure)
  ;; First just skip past the headers.
  (goto-char (point-min))
  (let ((bytes (nnimap-get-length))
	id parts)
    ;; Delete "FETCH" line.
    (delete-region (line-beginning-position)
		   (progn (forward-line 1) (point)))
    (goto-char (+ (point) bytes))
    ;; Collect all the body parts.
    (while (looking-at ".*BODY\\[\\([.0-9]+\\)\\]")
      (setq id (match-string 1)
	    bytes (or (nnimap-get-length) 0))
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))
      (push (list id (buffer-substring (point) (+ (point) bytes)))
	    parts)
      (delete-region (point) (+ (point) bytes)))
    ;; Delete trailing junk.
    (delete-region (point) (point-max))
    ;; Now insert all the parts again where they fit in the structure.
    (nnimap-insert-partial-structure structure parts)
    t))

(defun nnimap-insert-partial-structure (structure parts &optional subp)
  (let (type boundary)
    (let ((bstruc structure))
      (while (consp (car bstruc))
	(pop bstruc))
      (setq type (car bstruc))
      (setq bstruc (car (cdr bstruc)))
      (let ((has-boundary (member "boundary" bstruc)))
        (when has-boundary
          (setq boundary (cadr has-boundary)))))
    (when subp
      (insert (format "Content-type: multipart/%s; boundary=%S\n\n"
		      (downcase type) boundary)))
    (while (not (stringp (car structure)))
      (insert "\n--" boundary "\n")
      (if (consp (caar structure))
	  (nnimap-insert-partial-structure (pop structure) parts t)
	(let ((bit (pop structure)))
	  (insert (format "Content-type: %s/%s"
			  (downcase (nth 0 bit))
			  (downcase (nth 1 bit))))
	  (if (member-ignore-case "CHARSET" (nth 2 bit))
	      (insert (format
		       "; charset=%S\n"
		       (cadr (member-ignore-case "CHARSET" (nth 2 bit)))))
	    (insert "\n"))
	  (insert (format "Content-transfer-encoding: %s\n"
			  (nth 5 bit)))
	  (insert "\n")
	  (when (assoc (nth 9 bit) parts)
	    (insert (cadr (assoc (nth 9 bit) parts)))))))
    (insert "\n--" boundary "--\n")))

(defun nnimap-find-wanted-parts (structure)
  (message-flatten-list (nnimap-find-wanted-parts-1 structure "")))

(defun nnimap-find-wanted-parts-1 (structure prefix)
  (let ((num 1)
	parts)
    (while (consp (car structure))
      (let ((sub (pop structure)))
	(if (consp (car sub))
	    (push (nnimap-find-wanted-parts-1
		   sub (if (string= prefix "")
			   (number-to-string num)
			 (format "%s.%s" prefix num)))
		  parts)
	  (let ((type (format "%s/%s" (nth 0 sub) (nth 1 sub)))
		(id (if (string= prefix "")
			(number-to-string num)
		      (format "%s.%s" prefix num))))
	    (setcar (nthcdr 9 sub) id)
	    (when (if (eq nnimap-fetch-partial-articles t)
		      (equal id "1")
		    (string-match nnimap-fetch-partial-articles type))
	      (push id parts))))
	(incf num)))
    (nreverse parts)))

(defun nnimap-decode-gnus-group (group)
  (decode-coding-string group 'utf-8))

(deffoo nnimap-request-group (group &optional server dont-check info)
  (setq group (nnimap-decode-gnus-group group))
  (let ((result (nnimap-possibly-change-group
		 ;; Don't SELECT the group if we're going to select it
		 ;; later, anyway.
		 (if (and (not dont-check)
			  (assoc group nnimap-current-infos))
		     nil
		   group)
		 server))
	articles active marks high low)
    (with-current-buffer nntp-server-buffer
      (when result
	(if (and dont-check
		 (setq active (nth 2 (assoc group nnimap-current-infos))))
	    (insert (format "211 %d %d %d %S\n"
			    (- (cdr active) (car active))
			    (car active)
			    (cdr active)
			    group))
	  (with-current-buffer (nnimap-buffer)
	    (erase-buffer)
	    (let ((group-sequence
		   (nnimap-send-command "SELECT %S" (utf7-encode group t)))
		  (flag-sequence
		   (nnimap-send-command "UID FETCH 1:* FLAGS")))
	      (setf (nnimap-group nnimap-object) group)
	      (nnimap-wait-for-response flag-sequence)
	      (setq marks
		    (nnimap-flags-to-marks
		     (nnimap-parse-flags
		      (list (list group-sequence flag-sequence
				  1 group "SELECT")))))
	      (when (and info
			 marks)
		(nnimap-update-infos marks (list info))
		(nnimap-store-info info (gnus-active (gnus-info-group info))))
	      (goto-char (point-max))
	      (let ((uidnext (nth 5 (car marks))))
		(setq high (or (if uidnext
                                   (1- uidnext)
                                 (nth 3 (car marks)))
                               0)
		      low (or (nth 4 (car marks)) uidnext 1)))))
	  (erase-buffer)
	  (insert
	   (format
	    "211 %d %d %d %S\n" (1+ (- high low)) low high group)))
	t))))

(deffoo nnimap-request-create-group (group &optional server args)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group nil server)
    (with-current-buffer (nnimap-buffer)
      (car (nnimap-command "CREATE %S" (utf7-encode group t))))))

(deffoo nnimap-request-delete-group (group &optional force server)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group nil server)
    (with-current-buffer (nnimap-buffer)
      (car (nnimap-command "DELETE %S" (utf7-encode group t))))))

(deffoo nnimap-request-rename-group (group new-name &optional server)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group nil server)
    (with-current-buffer (nnimap-buffer)
      (nnimap-unselect-group)
      (car (nnimap-command "RENAME %S %S"
			   (utf7-encode group t) (utf7-encode new-name t))))))

(defun nnimap-unselect-group ()
  ;; Make sure we don't have this group open read/write by asking
  ;; to examine a mailbox that doesn't exist.  This seems to be
  ;; the only way that allows us to reliably go back to unselected
  ;; state on Courier.
  (nnimap-command "EXAMINE DOES.NOT.EXIST"))

(deffoo nnimap-request-expunge-group (group &optional server)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group group server)
    (with-current-buffer (nnimap-buffer)
      (car (nnimap-command "EXPUNGE")))))

(defun nnimap-get-flags (spec)
  (let ((articles nil)
	elems end)
    (with-current-buffer (nnimap-buffer)
      (erase-buffer)
      (nnimap-wait-for-response (nnimap-send-command
				 "UID FETCH %s FLAGS" spec))
      (setq end (point))
      (subst-char-in-region (point-min) (point-max)
			    ?\\ ?% t)
      (goto-char (point-min))
      (while (search-forward " FETCH " end t)
	(setq elems (read (current-buffer)))
	(push (cons (cadr (memq 'UID elems))
		    (cadr (memq 'FLAGS elems)))
	      articles)))
    (nreverse articles)))

(deffoo nnimap-close-group (group &optional server)
  t)

(deffoo nnimap-request-move-article (article group server accept-form
					     &optional last internal-move-group)
  (setq group (nnimap-decode-gnus-group group))
  (with-temp-buffer
    (mm-disable-multibyte)
    (when (funcall (if internal-move-group
		       'nnimap-request-head
		     'nnimap-request-article)
		   article group server (current-buffer))
      ;; If the move is internal (on the same server), just do it the easy
      ;; way.
      (let ((message-id (message-field-value "message-id")))
	(if internal-move-group
	    (let ((result
		   (with-current-buffer (nnimap-buffer)
		     (nnimap-command "UID COPY %d %S"
				     article
				     (utf7-encode internal-move-group t)))))
	      (when (car result)
		(nnimap-delete-article article)
		(cons internal-move-group
		      (or (nnimap-find-uid-response "COPYUID" (cadr result))
			  (nnimap-find-article-by-message-id
			   internal-move-group message-id)))))
	  ;; Move the article to a different method.
	  (let ((result (eval accept-form)))
	    (when result
	      (nnimap-possibly-change-group group server)
	      (nnimap-delete-article article)
	      result)))))))

(deffoo nnimap-request-expire-articles (articles group &optional server force)
  (setq group (nnimap-decode-gnus-group group))
  (cond
   ((null articles)
    nil)
   ((not (nnimap-possibly-change-group group server))
    articles)
   ((and force
	 (eq nnmail-expiry-target 'delete))
    (unless (nnimap-delete-article (gnus-compress-sequence articles))
      (nnheader-message 7 "Article marked for deletion, but not expunged."))
    nil)
   (t
    (let ((deletable-articles
	   (if (or force
		   (eq nnmail-expiry-wait 'immediate))
	       articles
	     (gnus-sorted-intersection
	      articles
	      (nnimap-find-expired-articles group)))))
      (if (null deletable-articles)
	  articles
	(if (eq nnmail-expiry-target 'delete)
	    (nnimap-delete-article (gnus-compress-sequence deletable-articles))
	  (setq deletable-articles
		(nnimap-process-expiry-targets
		 deletable-articles group server)))
	;; Return the articles we didn't delete.
	(gnus-sorted-complement articles deletable-articles))))))

(defun nnimap-process-expiry-targets (articles group server)
  (let ((deleted-articles nil))
    (cond
     ;; shortcut further processing if we're going to delete the articles
     ((eq nnmail-expiry-target 'delete)
      (setq deleted-articles articles)
      t)
     ;; or just move them to another folder on the same IMAP server
     ((and (not (functionp nnmail-expiry-target))
	   (gnus-server-equal (gnus-group-method nnmail-expiry-target)
			      (gnus-server-to-method
			       (format "nnimap:%s" server))))
      (and (nnimap-possibly-change-group group server)
	   (with-current-buffer (nnimap-buffer)
	     (nnheader-message 7 "Expiring articles from %s: %s" group articles)
	     (nnimap-command
	      "UID COPY %s %S"
	      (nnimap-article-ranges (gnus-compress-sequence articles))
	      (utf7-encode (gnus-group-real-name nnmail-expiry-target) t))
	     (setq deleted-articles articles)))
      t)
     (t
      (dolist (article articles)
	(let ((target nnmail-expiry-target))
	  (with-temp-buffer
	    (mm-disable-multibyte)
	    (when (nnimap-request-article article group server (current-buffer))
	      (when (functionp target)
		(setq target (funcall target group)))
	      (if (and target
		       (not (eq target 'delete)))
		  (if (or (gnus-request-group target t)
			  (gnus-request-create-group target))
		      (progn
			(nnmail-expiry-target-group target group)
			(nnheader-message 7 "Expiring article %s:%d to %s"
					  group article target))
		    (setq target nil))
		(nnheader-message 7 "Expiring article %s:%d" group article))
	      (when target
		(push article deleted-articles))))))))
    ;; Change back to the current group again.
    (nnimap-possibly-change-group group server)
    (setq deleted-articles (nreverse deleted-articles))
    (nnimap-delete-article (gnus-compress-sequence deleted-articles))
    deleted-articles))

(defun nnimap-find-expired-articles (group)
  (let ((cutoff (nnmail-expired-article-p group nil nil)))
    (with-current-buffer (nnimap-buffer)
      (let ((result
	     (nnimap-command
	      "UID SEARCH SENTBEFORE %s"
	      (format-time-string
	       (format "%%d-%s-%%Y"
		       (upcase
			(car (rassoc (nth 4 (decode-time cutoff))
				     parse-time-months))))
	       cutoff))))
	(and (car result)
	     (delete 0 (mapcar #'string-to-number
			       (cdr (assoc "SEARCH" (cdr result))))))))))


(defun nnimap-find-article-by-message-id (group message-id)
  (with-current-buffer (nnimap-buffer)
    (erase-buffer)
    (unless (equal group (nnimap-group nnimap-object))
      (setf (nnimap-group nnimap-object) nil)
      (setf (nnimap-examined nnimap-object) group)
      (nnimap-send-command "EXAMINE %S" (utf7-encode group t)))
    (let ((sequence
	   (nnimap-send-command "UID SEARCH HEADER Message-Id %S" message-id))
	  article result)
      (setq result (nnimap-wait-for-response sequence))
      (when (and result
		 (car (setq result (nnimap-parse-response))))
	;; Select the last instance of the message in the group.
	(and (setq article
		   (car (last (cdr (assoc "SEARCH" (cdr result))))))
	     (string-to-number article))))))

(defun nnimap-delete-article (articles)
  (with-current-buffer (nnimap-buffer)
    (nnimap-command "UID STORE %s +FLAGS.SILENT (\\Deleted)"
		    (nnimap-article-ranges articles))
    (cond
     ((nnimap-capability "UIDPLUS")
      (nnimap-command "UID EXPUNGE %s"
		      (nnimap-article-ranges articles))
      t)
     (nnimap-expunge
      (nnimap-command "EXPUNGE")
      t)
     (t (gnus-message 7 (concat "nnimap: nnimap-expunge is not set and the "
                                "server doesn't support UIDPLUS, so we won't "
                                "delete this article now"))))))

(deffoo nnimap-request-scan (&optional group server)
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (when (and (nnimap-possibly-change-group nil server)
	     nnimap-inbox
	     nnimap-split-methods)
    (nnheader-message 7 "nnimap %s splitting mail..." server)
    (nnimap-split-incoming-mail)
    (nnheader-message 7 "nnimap %s splitting mail...done" server)))

(defun nnimap-marks-to-flags (marks)
  (let (flags flag)
    (dolist (mark marks)
      (when (setq flag (cadr (assq mark nnimap-mark-alist)))
	(push flag flags)))
    flags))

(deffoo nnimap-request-update-group-status (group status &optional server)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group nil server)
    (let ((command (assoc
		    status
		    '((subscribe "SUBSCRIBE")
		      (unsubscribe "UNSUBSCRIBE")))))
      (when command
	(with-current-buffer (nnimap-buffer)
	  (nnimap-command "%s %S" (cadr command) (utf7-encode group t)))))))

(deffoo nnimap-request-set-mark (group actions &optional server)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group group server)
    (let (sequence)
      (with-current-buffer (nnimap-buffer)
	(erase-buffer)
	;; Just send all the STORE commands without waiting for
	;; response.  If they're successful, they're successful.
	(dolist (action actions)
	  (destructuring-bind (range action marks) action
	    (let ((flags (nnimap-marks-to-flags marks)))
	      (when flags
		(setq sequence (nnimap-send-command
				"UID STORE %s %sFLAGS.SILENT (%s)"
				(nnimap-article-ranges range)
				(cond
				 ((eq action 'del) "-")
				 ((eq action 'add) "+")
				 ((eq action 'set) ""))
				(mapconcat #'identity flags " ")))))))
	;; Wait for the last command to complete to avoid later
	;; synchronization problems with the stream.
	(when sequence
	  (nnimap-wait-for-response sequence))))))

(deffoo nnimap-request-accept-article (group &optional server last)
  (setq group (nnimap-decode-gnus-group group))
  (when (nnimap-possibly-change-group nil server)
    (nnmail-check-syntax)
    (let ((message-id (message-field-value "message-id"))
	  sequence message)
      (nnimap-add-cr)
      (setq message (buffer-substring-no-properties (point-min) (point-max)))
      (with-current-buffer (nnimap-buffer)
	(when (setq message (or (nnimap-process-quirk "OK Gimap " 'append message)
				message))
	  ;; If we have this group open read-only, then unselect it
	  ;; before appending to it.
	  (when (equal (nnimap-examined nnimap-object) group)
	    (nnimap-unselect-group))
	  (erase-buffer)
	  (setq sequence (nnimap-send-command
			  "APPEND %S {%d}" (utf7-encode group t)
			  (length message)))
	  (unless nnimap-streaming
	    (nnimap-wait-for-connection "^[+]"))
	  (process-send-string (get-buffer-process (current-buffer)) message)
	  (process-send-string (get-buffer-process (current-buffer))
			       (if (nnimap-newlinep nnimap-object)
				   "\n"
				 "\r\n"))
	  (let ((result (nnimap-get-response sequence)))
	    (if (not (nnimap-ok-p result))
		(progn
		  (nnheader-report 'nnimap "%s" result)
		  nil)
	      (cons group
		    (or (nnimap-find-uid-response "APPENDUID" (car result))
			(nnimap-find-article-by-message-id
			 group message-id))))))))))

(defun nnimap-process-quirk (greeting-match type data)
  (when (and (nnimap-greeting nnimap-object)
	     (string-match greeting-match (nnimap-greeting nnimap-object))
	     (eq type 'append)
	     (string-match "\000" data))
    (let ((choice (gnus-multiple-choice
		   "Message contains NUL characters.  Delete, continue, abort? "
		   '((?d "Delete NUL characters")
		     (?c "Try to APPEND the message as is")
		     (?a "Abort")))))
      (cond
       ((eq choice ?a)
	(nnheader-report 'nnimap "Aborted APPEND due to NUL characters"))
       ((eq choice ?c)
	data)
       (t
	(with-temp-buffer
	  (insert data)
	  (goto-char (point-min))
	  (while (search-forward "\000" nil t)
	    (replace-match "" t t))
	  (buffer-string)))))))

(defun nnimap-ok-p (value)
  (and (consp value)
       (consp (car value))
       (equal (caar value) "OK")))

(defun nnimap-find-uid-response (name list)
  (let ((result (car (last (nnimap-find-response-element name list)))))
    (and result
	 (string-to-number result))))

(defun nnimap-find-response-element (name list)
  (let (result)
    (dolist (elem list)
      (when (and (consp elem)
		 (equal name (car elem)))
	(setq result elem)))
    result))

(deffoo nnimap-request-replace-article (article group buffer)
  (setq group (nnimap-decode-gnus-group group))
  (let (group-art)
    (when (and (nnimap-possibly-change-group group nil)
	       ;; Put the article into the group.
	       (with-current-buffer buffer
		 (setq group-art
		       (nnimap-request-accept-article group nil t))))
      (nnimap-delete-article (list article))
      ;; Return the new article number.
      (cdr group-art))))

(defun nnimap-add-cr ()
  (goto-char (point-min))
  (while (re-search-forward "\r?\n" nil t)
    (replace-match "\r\n" t t)))

(defun nnimap-get-groups ()
  (erase-buffer)
  (let ((sequence (nnimap-send-command "LIST \"\" \"*\""))
	groups)
    (nnimap-wait-for-response sequence)
    (subst-char-in-region (point-min) (point-max)
			  ?\\ ?% t)
    (goto-char (point-min))
    (nnimap-unfold-quoted-lines)
    (goto-char (point-min))
    (while (search-forward "* LIST " nil t)
      (let ((flags (read (current-buffer)))
	    (separator (read (current-buffer)))
	    (group (read (current-buffer))))
	(unless (member '%NoSelect flags)
	  (push (utf7-decode (if (stringp group)
				 group
			       (format "%s" group)) t)
		groups))))
    (nreverse groups)))

(deffoo nnimap-request-list (&optional server)
  (when (nnimap-possibly-change-group nil server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (let ((groups
	     (with-current-buffer (nnimap-buffer)
	       (nnimap-get-groups)))
	    sequences responses)
	(when groups
	  (with-current-buffer (nnimap-buffer)
	    (setf (nnimap-group nnimap-object) nil)
	    (dolist (group groups)
	      (setf (nnimap-examined nnimap-object) group)
	      (push (list (nnimap-send-command "EXAMINE %S"
					       (utf7-encode group t))
			  group)
		    sequences))
	    (nnimap-wait-for-response (caar sequences))
	    (setq responses
		  (nnimap-get-responses (mapcar #'car sequences))))
	  (dolist (response responses)
	    (let* ((sequence (car response))
		   (response (cadr response))
		   (group (cadr (assoc sequence sequences)))
		   (egroup (encode-coding-string group 'utf-8)))
	      (when (and group
			 (equal (caar response) "OK"))
		(let ((uidnext (nnimap-find-parameter "UIDNEXT" response))
		      highest exists)
		  (dolist (elem response)
		    (when (equal (cadr elem) "EXISTS")
		      (setq exists (string-to-number (car elem)))))
		  (when uidnext
		    (setq highest (1- (string-to-number (car uidnext)))))
		  (cond
		   ((null highest)
		    (insert (format "%S 0 1 y\n" egroup)))
		   ((zerop exists)
		    ;; Empty group.
		    (insert (format "%S %d %d y\n" egroup
				    highest (1+ highest))))
		   (t
		    ;; Return the widest possible range.
		    (insert (format "%S %d 1 y\n" egroup
				    (or highest exists)))))))))
	  t)))))

(deffoo nnimap-request-newgroups (date &optional server)
  (when (nnimap-possibly-change-group nil server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (with-current-buffer (nnimap-buffer)
		       (nnimap-get-groups)))
	(unless (assoc group nnimap-current-infos)
	  ;; Insert dummy numbers here -- they don't matter.
	  (insert (format "%S 0 1 y\n" (encode-coding-string group 'utf-8)))))
      t)))

(deffoo nnimap-retrieve-group-data-early (server infos)
  (when (and (nnimap-possibly-change-group nil server)
	     infos)
    (with-current-buffer (nnimap-buffer)
      (erase-buffer)
      (setf (nnimap-group nnimap-object) nil)
      (setf (nnimap-initial-resync nnimap-object) 0)
      (let ((qresyncp (nnimap-capability "QRESYNC"))
	    params groups sequences active uidvalidity modseq group)
	;; Go through the infos and gather the data needed to know
	;; what and how to request the data.
	(dolist (info infos)
	  (setq params (gnus-info-params info)
		group (nnimap-decode-gnus-group
		       (gnus-group-real-name (gnus-info-group info)))
		active (cdr (assq 'active params))
		uidvalidity (cdr (assq 'uidvalidity params))
		modseq (cdr (assq 'modseq params)))
	  (setf (nnimap-examined nnimap-object) group)
	  (if (and qresyncp
		   uidvalidity
		   active
		   modseq)
	      (push
	       (list (nnimap-send-command "EXAMINE %S (%s (%s %s))"
					  (utf7-encode group t)
					  (nnimap-quirk "QRESYNC")
					  uidvalidity modseq)
		     'qresync
		     nil group 'qresync)
	       sequences)
	    (let ((command
		   (if uidvalidity
		       "EXAMINE"
		     ;; If we don't have a UIDVALIDITY, then this is
		     ;; the first time we've seen the group, so we
		     ;; have to do a SELECT (which is slower than an
		     ;; examine), but will tell us whether the group
		     ;; is read-only or not.
		     "SELECT"))
		  start)
	      (if (and active uidvalidity)
		  ;; Fetch the last 100 flags.
		  (setq start (max 1 (- (cdr active) 100)))
		(setf (nnimap-initial-resync nnimap-object)
		      (1+ (nnimap-initial-resync nnimap-object)))
		(setq start 1))
	      (push (list (nnimap-send-command "%s %S" command
					       (utf7-encode group t))
			  (nnimap-send-command "UID FETCH %d:* FLAGS" start)
			  start group command)
		    sequences))))
	sequences))))

(defun nnimap-quirk (command)
  (let ((quirk (assoc command nnimap-quirks)))
    ;; If this server is of a type that matches a quirk, then return
    ;; the "quirked" command instead of the proper one.
    (if (or (null quirk)
	    (not (string-match (nth 1 quirk) (nnimap-greeting nnimap-object))))
	command
      (nth 2 quirk))))

(deffoo nnimap-finish-retrieve-group-infos (server infos sequences)
  (when (and sequences
	     (nnimap-possibly-change-group nil server t)
	     ;; Check that the process is still alive.
	     (get-buffer-process (nnimap-buffer))
	     (memq (process-status (get-buffer-process (nnimap-buffer)))
		   '(open run)))
    (with-current-buffer (nnimap-buffer)
      ;; Wait for the final data to trickle in.
      (when (nnimap-wait-for-response (if (eq (cadar sequences) 'qresync)
					  (caar sequences)
					(cadar sequences))
				      t)
	;; Now we should have most of the data we need, no matter
	;; whether we're QRESYNCING, fetching all the flags from
	;; scratch, or just fetching the last 100 flags per group.
	(nnimap-update-infos (nnimap-flags-to-marks
			      (nnimap-parse-flags
			       (nreverse sequences)))
			     infos)
	;; Finally, just return something resembling an active file in
	;; the nntp buffer, so that the agent can save the info, too.
	(with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (dolist (info infos)
	    (let* ((group (gnus-info-group info))
		   (active (gnus-active group)))
	      (when active
		(insert (format "%S %d %d y\n"
				(decode-coding-string
				 (gnus-group-real-name group) 'utf-8)
				(cdr active)
				(car active)))))))))))

(defun nnimap-update-infos (flags infos)
  (dolist (info infos)
    (let* ((group (nnimap-decode-gnus-group
		   (gnus-group-real-name (gnus-info-group info))))
	   (marks (cdr (assoc group flags))))
      (when marks
	(nnimap-update-info info marks)))))

(defun nnimap-update-info (info marks)
  (destructuring-bind (existing flags high low uidnext start-article
				permanent-flags uidvalidity
				vanished highestmodseq) marks
    (cond
     ;; Ignore groups with no UIDNEXT/marks.  This happens for
     ;; completely empty groups.
     ((and (not existing)
	   (not uidnext))
      (let ((active (cdr (assq 'active (gnus-info-params info)))))
	(when active
	  (gnus-set-active (gnus-info-group info) active))))
     ;; We have a mismatch between the old and new UIDVALIDITY
     ;; identifiers, so we have to re-request the group info (the next
     ;; time).  This virtually never happens.
     ((let ((old-uidvalidity
	     (cdr (assq 'uidvalidity (gnus-info-params info)))))
	(and old-uidvalidity
	     (not (equal old-uidvalidity uidvalidity))
             (or (not start-article)
                 (> start-article 1))))
      (gnus-group-remove-parameter info 'uidvalidity)
      (gnus-group-remove-parameter info 'modseq))
     ;; We have the data needed to update.
     (t
      (let* ((group (gnus-info-group info))
	     (completep (and start-article
			     (= start-article 1)))
	     (active (or (gnus-active group)
			 (cdr (assq 'active (gnus-info-params info))))))
	(when uidnext
	  (setq high (1- uidnext)))
	;; First set the active ranges based on high/low.
	(if (or completep
		(not (gnus-active group)))
	    (gnus-set-active group
			     (cond
			      (active
			       (cons (min (or low (car active))
					  (car active))
				     (max (or high (cdr active))
					  (cdr active))))
			      ((and low high)
			       (cons low high))
			      (uidnext
			       ;; No articles in this group.
			       (cons uidnext (1- uidnext)))
			      (start-article
			       (cons start-article (1- start-article)))
			      (t
			       ;; No articles and no uidnext.
			       nil)))
	  (gnus-set-active group
			   (cons (car active)
				 (or high (1- uidnext)))))
	;; See whether this is a read-only group.
	(unless (eq permanent-flags 'not-scanned)
	  (gnus-group-set-parameter
	   info 'permanent-flags
	   (and (or (memq '%* permanent-flags)
		    (memq '%Seen permanent-flags))
		permanent-flags)))
	;; Update marks and read articles if this isn't a
	;; read-only IMAP group.
	(when (setq permanent-flags
		    (cdr (assq 'permanent-flags (gnus-info-params info))))
	  (if (and highestmodseq
		   (not start-article))
	      ;; We've gotten the data by QRESYNCing.
	      (nnimap-update-qresync-info
	       info existing (nnimap-imap-ranges-to-gnus-ranges vanished) flags)
	    ;; Do normal non-QRESYNC flag updates.
	    ;; Update the list of read articles.
	    (let* ((unread
		    (gnus-compress-sequence
		     (gnus-set-difference
		      (gnus-set-difference
		       existing
		       (cdr (assoc '%Seen flags)))
		      (cdr (assoc '%Flagged flags)))))
		   (read (gnus-range-difference
			  (cons start-article high) unread)))
	      (when (> start-article 1)
		(setq read
		      (gnus-range-nconcat
		       (if (> start-article 1)
			   (gnus-sorted-range-intersection
			    (cons 1 (1- start-article))
			    (gnus-info-read info))
			 (gnus-info-read info))
		       read)))
	      (when (or (not (listp permanent-flags))
			(memq '%Seen permanent-flags))
		(gnus-info-set-read info read))
	      ;; Update the marks.
	      (setq marks (gnus-info-marks info))
	      (dolist (type (cdr nnimap-mark-alist))
		(when (or (not (listp permanent-flags))
			  (memq (car (assoc (caddr type) flags))
				permanent-flags)
			  (memq '%* permanent-flags))
		  (let ((old-marks (assoc (car type) marks))
			(new-marks
			 (gnus-compress-sequence
			  (cdr (or (assoc (caddr type) flags) ; %Flagged
				   (assoc (intern (cadr type) obarray) flags)
				   (assoc (cadr type) flags)))))) ; "\Flagged"
		    (setq marks (delq old-marks marks))
		    (pop old-marks)
		    (when (and old-marks
			       (> start-article 1))
		      (setq old-marks (gnus-range-difference
				       old-marks
				       (cons start-article high)))
		      (setq new-marks (gnus-range-nconcat old-marks new-marks)))
		    (when new-marks
		      (push (cons (car type) new-marks) marks)))))
	      (gnus-info-set-marks info marks t))))
	;; Tell Gnus whether there are any \Recent messages in any of
	;; the groups.
	(let ((recent (cdr (assoc '%Recent flags))))
	  (when (and active
		     recent
		     (> (car (last recent)) (cdr active)))
	    (push (list (cons (gnus-group-real-name group) 0))
		  nnmail-split-history)))
	;; Note the active level for the next run-through.
	(gnus-group-set-parameter info 'active (gnus-active group))
	(gnus-group-set-parameter info 'uidvalidity uidvalidity)
	(gnus-group-set-parameter info 'modseq highestmodseq)
	(nnimap-store-info info (gnus-active group)))))))

(defun nnimap-update-qresync-info (info existing vanished flags)
  ;; Add all the vanished articles to the list of read articles.
  (gnus-info-set-read
   info
   (gnus-add-to-range
    (gnus-add-to-range
     (gnus-range-add (gnus-info-read info)
		     vanished)
     (cdr (assq '%Flagged flags)))
    (cdr (assq '%Seen flags))))
  (let ((marks (gnus-info-marks info)))
    (dolist (type (cdr nnimap-mark-alist))
      (let ((ticks (assoc (car type) marks))
	    (new-marks
	     (cdr (or (assoc (caddr type) flags) ; %Flagged
		      (assoc (intern (cadr type) obarray) flags)
		      (assoc (cadr type) flags))))) ; "\Flagged"
	(setq marks (delq ticks marks))
	(pop ticks)
	;; Add the new marks we got.
	(setq ticks (gnus-add-to-range ticks new-marks))
	;; Remove the marks from messages that don't have them.
	(setq ticks (gnus-remove-from-range
		     ticks
		     (gnus-compress-sequence
		      (gnus-sorted-complement existing new-marks))))
	(when ticks
	  (push (cons (car type) ticks) marks)))
      (gnus-info-set-marks info marks t))))

(defun nnimap-imap-ranges-to-gnus-ranges (irange)
  (if (zerop (length irange))
      nil
    (let ((result nil))
      (dolist (elem (split-string irange ","))
	(push
	 (if (string-match ":" elem)
	     (let ((numbers (split-string elem ":")))
	       (cons (string-to-number (car numbers))
		     (string-to-number (cadr numbers))))
	   (string-to-number elem))
	 result))
      (nreverse result))))

(defun nnimap-store-info (info active)
  (let* ((group (gnus-group-real-name (gnus-info-group info)))
	 (entry (assoc group nnimap-current-infos)))
    (if entry
	(setcdr entry (list info active))
      (push (list group info active) nnimap-current-infos))))

(defun nnimap-flags-to-marks (groups)
  (let (data group totalp uidnext articles start-article mark permanent-flags
	     uidvalidity vanished highestmodseq)
    (dolist (elem groups)
      (setq group (car elem)
	    uidnext (nth 1 elem)
	    start-article (nth 2 elem)
	    permanent-flags (nth 3 elem)
	    uidvalidity (nth 4 elem)
	    vanished (nth 5 elem)
	    highestmodseq (nth 6 elem)
	    articles (nthcdr 7 elem))
      (let ((high (caar articles))
	    marks low existing)
	(dolist (article articles)
	  (setq low (car article))
	  (push (car article) existing)
	  (dolist (flag (cdr article))
	    (setq mark (assoc flag marks))
	    (if (not mark)
		(push (list flag (car article)) marks)
	      (setcdr mark (cons (car article) (cdr mark))))))
	(push (list group existing marks high low uidnext start-article
		    permanent-flags uidvalidity vanished highestmodseq)
	      data)))
    data))

(defun nnimap-parse-flags (sequences)
  (goto-char (point-min))
  ;; Change \Delete etc to %Delete, so that the Emacs Lisp reader can
  ;; read it.
  (subst-char-in-region (point-min) (point-max)
			?\\ ?% t)
  ;; Remove any MODSEQ entries in the buffer, because they may contain
  ;; numbers that are too large for 32-bit Emacsen.
  (while (re-search-forward " MODSEQ ([0-9]+)" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (let (start end articles groups uidnext elems permanent-flags
	      uidvalidity vanished highestmodseq)
    (dolist (elem sequences)
      (destructuring-bind (group-sequence flag-sequence totalp group command)
	  elem
	(setq start (point))
	(when (and
	       ;; The EXAMINE was successful.
	       (search-forward (format "\n%d OK " group-sequence) nil t)
	       (progn
		 (forward-line 1)
		 (setq end (point))
		 (goto-char start)
		 (setq permanent-flags
		       (if (equal command "SELECT")
			   (and (search-forward "PERMANENTFLAGS "
						(or end (point-min)) t)
				(read (current-buffer)))
			 'not-scanned))
		 (goto-char start)
		 (setq uidnext
		       (and (search-forward "UIDNEXT "
					    (or end (point-min)) t)
			    (read (current-buffer))))
		 (goto-char start)
		 (setq uidvalidity
		       (and (re-search-forward "UIDVALIDITY \\([0-9]+\\)"
					       (or end (point-min)) t)
			    ;; Store UIDVALIDITY as a string, as it's
			    ;; too big for 32-bit Emacsen, usually.
			    (match-string 1)))
		 (goto-char start)
		 (setq vanished
		       (and (eq flag-sequence 'qresync)
			    (re-search-forward "^\\* VANISHED .*? \\([0-9:,]+\\)"
					       (or end (point-min)) t)
			    (match-string 1)))
		 (goto-char start)
		 (setq highestmodseq
		       (and (re-search-forward "HIGHESTMODSEQ \\([0-9]+\\)"
					    (or end (point-min)) t)
			    (match-string 1)))
		 (goto-char end)
		 (forward-line -1))
	       ;; The UID FETCH FLAGS was successful.
	       (or (eq flag-sequence 'qresync)
		   (search-forward (format "\n%d OK " flag-sequence) nil t)))
	  (if (eq flag-sequence 'qresync)
	      (progn
		(goto-char start)
		(setq start end))
	    (setq start (point))
	    (goto-char end))
	  (while (re-search-forward "^\\* [0-9]+ FETCH " start t)
	    (let ((p (point)))
	      (setq elems (read (current-buffer)))
	      (push (cons (cadr (memq 'UID elems))
			  (cadr (memq 'FLAGS elems)))
		    articles)))
	  (push (nconc (list group uidnext totalp permanent-flags uidvalidity
			     vanished highestmodseq)
		       articles)
		groups)
	  (if (eq flag-sequence 'qresync)
	      (goto-char end)
	    (setq end (point)))
	  (setq articles nil))))
    groups))

(defun nnimap-find-process-buffer (buffer)
  (cadr (assoc buffer nnimap-connection-alist)))

(deffoo nnimap-request-post (&optional server)
  (setq nnimap-status-string "Read-only server")
  nil)

(declare-function gnus-fetch-headers "gnus-sum"
		  (articles &optional limit force-new dependencies))

(autoload 'nnir-search-thread "nnir")

(deffoo nnimap-request-thread (header &optional group server)
  (when group
    (setq group (nnimap-decode-gnus-group group)))
  (if gnus-refer-thread-use-nnir
      (nnir-search-thread header)
    (when (nnimap-possibly-change-group group server)
      (let* ((cmd (nnimap-make-thread-query header))
             (result (with-current-buffer (nnimap-buffer)
                       (nnimap-command  "UID SEARCH %s" cmd))))
        (when result
          (gnus-fetch-headers
           (and (car result)
		(delete 0 (mapcar #'string-to-number
				  (cdr (assoc "SEARCH" (cdr result))))))
           nil t))))))

(defun nnimap-possibly-change-group (group server &optional no-reconnect)
  (let ((open-result t))
    (when (and server
	       (not (nnimap-server-opened server)))
      (setq open-result (nnimap-open-server server nil no-reconnect)))
    (cond
     ((not open-result)
      nil)
     ((not group)
      t)
     (t
      (with-current-buffer (nnimap-buffer)
	(if (equal group (nnimap-group nnimap-object))
	    t
	  (let ((result (nnimap-command "SELECT %S" (utf7-encode group t))))
	    (when (car result)
	      (setf (nnimap-group nnimap-object) group
		    (nnimap-select-result nnimap-object) result)
	      result))))))))

(defun nnimap-find-connection (buffer)
  "Find the connection delivering to BUFFER."
  (let ((entry (assoc buffer nnimap-connection-alist)))
    (when entry
      (if (and (buffer-name (cadr entry))
	       (get-buffer-process (cadr entry))
	       (memq (process-status (get-buffer-process (cadr entry)))
		     '(open run)))
	  (get-buffer-process (cadr entry))
	(setq nnimap-connection-alist (delq entry nnimap-connection-alist))
	nil))))

(defvar nnimap-sequence 0)

(defun nnimap-send-command (&rest args)
  (setf (nnimap-last-command-time nnimap-object) (current-time))
  (process-send-string
   (get-buffer-process (current-buffer))
   (nnimap-log-command
    (format "%d %s%s\n"
	    (incf nnimap-sequence)
	    (apply #'format args)
	    (if (nnimap-newlinep nnimap-object)
		""
	      "\r"))))
  ;; Some servers apparently can't have many outstanding
  ;; commands, so throttle them.
  (unless nnimap-streaming
    (nnimap-wait-for-response nnimap-sequence))
  nnimap-sequence)

(defvar nnimap-record-commands nil
  "If non-nil, log commands to the \"*imap log*\" buffer.")

(defun nnimap-log-command (command)
  (when nnimap-record-commands
    (with-current-buffer (get-buffer-create "*imap log*")
      (goto-char (point-max))
      (insert (format-time-string "%H:%M:%S")
	      " [" nnimap-address "] "
	      (if nnimap-inhibit-logging
		  "(inhibited)\n"
		command))))
  command)

(defun nnimap-command (&rest args)
  (erase-buffer)
  (let* ((sequence (apply #'nnimap-send-command args))
	 (response (nnimap-get-response sequence)))
    (if (equal (caar response) "OK")
	(cons t response)
      (nnheader-report 'nnimap "%s"
		       (mapconcat (lambda (a)
				    (format "%s" a))
				  (car response) " "))
      nil)))

(defun nnimap-get-response (sequence)
  (nnimap-wait-for-response sequence)
  (nnimap-parse-response))

(defun nnimap-wait-for-connection (&optional regexp)
  (nnimap-wait-for-line (or regexp "^[*.] .*\n") "[*.] \\([A-Z0-9]+\\)"))

(defun nnimap-wait-for-line (regexp &optional response-regexp)
  (let ((process (get-buffer-process (current-buffer))))
    (goto-char (point-min))
    (while (and (memq (process-status process)
		      '(open run))
		(not (re-search-forward regexp nil t)))
      (nnheader-accept-process-output process)
      (goto-char (point-min)))
    (forward-line -1)
    (and (looking-at (or response-regexp regexp))
	 (match-string 1))))

(defun nnimap-wait-for-response (sequence &optional messagep)
  (let ((process (get-buffer-process (current-buffer)))
	openp)
    (condition-case nil
        (progn
	  (goto-char (point-max))
	  (while (and (setq openp (memq (process-status process)
					'(open run)))
		      (progn
			;; Skip past any "*" lines that the server has
			;; output.
			(while (and (not (bobp))
				    (progn
				      (forward-line -1)
				      (looking-at "\\*"))))
			(not (looking-at (format "%d .*\n" sequence)))))
	    (when messagep
	      (nnheader-message-maybe
	       7 "nnimap read %dk from %s%s" (/ (buffer-size) 1000)
	       nnimap-address
	       (if (not (zerop (nnimap-initial-resync nnimap-object)))
		   (format " (initial sync of %d group%s; please wait)"
			   (nnimap-initial-resync nnimap-object)
			   (if (= (nnimap-initial-resync nnimap-object) 1)
			       ""
			     "s"))
		 "")))
	    (nnheader-accept-process-output process)
	    (goto-char (point-max)))
	  (setf (nnimap-initial-resync nnimap-object) 0)
          openp)
      (quit
       (when debug-on-quit
	 (debug "Quit"))
       ;; The user hit C-g while we were waiting: kill the process, in case
       ;; it's a gnutls-cli process that's stuck (tends to happen a lot behind
       ;; NAT routers).
       (delete-process process)
       nil))))

(defun nnimap-parse-response ()
  (let ((lines (split-string (nnimap-last-response-string) "\r\n" t))
	result)
    (dolist (line lines)
      (push (cdr (nnimap-parse-line line)) result))
    ;; Return the OK/error code first, and then all the "continuation
    ;; lines" afterwards.
    (cons (pop result)
	  (nreverse result))))

;; Parse an IMAP response line lightly.  They look like
;; "* OK [UIDVALIDITY 1164213559] UIDs valid", typically, so parse
;; the lines into a list of strings and lists of string.
(defun nnimap-parse-line (line)
  (let (char result)
    (with-temp-buffer
      (mm-disable-multibyte)
      (insert line)
      (goto-char (point-min))
      (while (not (eobp))
	(if (eql (setq char (following-char)) ? )
	    (forward-char 1)
	  (push
	   (cond
	    ((eql char ?\[)
	     (split-string
	      (buffer-substring
	       (1+ (point))
	       (if (search-forward "]" (line-end-position) 'move)
		   (1- (point))
		 (point)))))
	    ((eql char ?\()
	     (split-string
	      (buffer-substring
	       (1+ (point))
	       (if (search-forward ")" (line-end-position) 'move)
		   (1- (point))
		 (point)))))
	    ((eql char ?\")
	     (forward-char 1)
	     (buffer-substring
	      (point)
	      (1- (or (search-forward "\"" (line-end-position) 'move)
		      (point)))))
	    (t
	     (buffer-substring (point) (if (search-forward " " nil t)
					   (1- (point))
					 (goto-char (point-max))))))
	   result)))
      (nreverse result))))

(defun nnimap-last-response-string ()
  (save-excursion
    (forward-line 1)
    (let ((end (point)))
      (forward-line -1)
      (when (not (bobp))
	(forward-line -1)
	(while (and (not (bobp))
		    (eql (following-char) ?*))
	  (forward-line -1))
	(unless (eql (following-char) ?*)
	  (forward-line 1)))
      (buffer-substring (point) end))))

(defun nnimap-get-responses (sequences)
  (let (responses)
    (dolist (sequence sequences)
      (goto-char (point-min))
      (when (re-search-forward (format "^%d " sequence) nil t)
	(push (list sequence (nnimap-parse-response))
	      responses)))
    responses))

(defvar nnimap-incoming-split-list nil)

(defun nnimap-fetch-inbox (articles)
  (erase-buffer)
  (nnimap-wait-for-response
   (nnimap-send-command
    "UID FETCH %s %s"
    (nnimap-article-ranges articles)
    (format "(UID %s%s)"
	    (format
	     (if (nnimap-ver4-p)
		 "BODY.PEEK"
	       "RFC822.PEEK"))
	    (cond
	     (nnimap-split-download-body-default
	      "[]")
	     ((nnimap-ver4-p)
	      "[HEADER]")
	     (t
	      "[1]"))))
   t))

(defun nnimap-split-incoming-mail ()
  (with-current-buffer (nnimap-buffer)
    (let ((nnimap-incoming-split-list nil)
	  (nnmail-split-methods
	   (cond
	    ((eq nnimap-split-methods 'default)
	     nnmail-split-methods)
	    (nnimap-split-methods
	     nnimap-split-methods)
	    (nnimap-split-fancy
	     'nnmail-split-fancy)))
	  (nnmail-split-fancy (or nnimap-split-fancy
				  nnmail-split-fancy))
	  (nnmail-inhibit-default-split-group t)
	  (groups (nnimap-get-groups))
	  new-articles)
      (erase-buffer)
      (nnimap-command "SELECT %S" nnimap-inbox)
      (setf (nnimap-group nnimap-object) nnimap-inbox)
      (setq new-articles (nnimap-new-articles (nnimap-get-flags "1:*")))
      (when new-articles
	(nnimap-fetch-inbox new-articles)
	(nnimap-transform-split-mail)
	(nnheader-ms-strip-cr)
	(nnmail-cache-open)
	(nnmail-split-incoming (current-buffer)
			       #'nnimap-save-mail-spec
			       nil nil
			       #'nnimap-dummy-active-number
			       #'nnimap-save-mail-spec)
	(when nnimap-incoming-split-list
	  (let ((specs (nnimap-make-split-specs nnimap-incoming-split-list))
		sequences junk-articles)
	    ;; Create any groups that doesn't already exist on the
	    ;; server first.
	    (dolist (spec specs)
	      (when (and (not (member (car spec) groups))
			 (not (eq (car spec) 'junk)))
		(nnimap-command "CREATE %S" (utf7-encode (car spec) t))))
	    ;; Then copy over all the messages.
	    (erase-buffer)
	    (dolist (spec specs)
	      (let ((group (car spec))
		    (ranges (cdr spec)))
		(if (eq group 'junk)
		    (setq junk-articles ranges)
		  (push (list (nnimap-send-command
			       "UID COPY %s %S"
			       (nnimap-article-ranges ranges)
			       (utf7-encode group t))
			      ranges)
			sequences))))
	    ;; Wait for the last COPY response...
	    (when sequences
	      (nnimap-wait-for-response (caar sequences))
	      ;; And then mark the successful copy actions as deleted,
	      ;; and possibly expunge them.
	      (nnimap-mark-and-expunge-incoming
	       (nnimap-parse-copied-articles sequences)))
            (nnimap-mark-and-expunge-incoming junk-articles)))))))

(defun nnimap-mark-and-expunge-incoming (range)
  (when range
    (setq range (nnimap-article-ranges range))
    (erase-buffer)
    (let ((sequence
	   (nnimap-send-command
	    "UID STORE %s +FLAGS.SILENT (\\Deleted)" range)))
      (cond
       ;; If the server supports it, we now delete the message we have
       ;; just copied over.
       ((nnimap-capability "UIDPLUS")
	(setq sequence (nnimap-send-command "UID EXPUNGE %s" range)))
       ;; If it doesn't support UID EXPUNGE, then we only expunge if the
       ;; user has configured it.
       (nnimap-expunge
	(setq sequence (nnimap-send-command "EXPUNGE"))))
      (nnimap-wait-for-response sequence))))

(defun nnimap-parse-copied-articles (sequences)
  (let (sequence copied range)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\) OK\\b" nil t)
      (setq sequence (string-to-number (match-string 1)))
      (when (setq range (cadr (assq sequence sequences)))
	(push (gnus-uncompress-range range) copied)))
    (gnus-compress-sequence (sort (apply #'nconc copied) #'<))))

(defun nnimap-new-articles (flags)
  (let (new)
    (dolist (elem flags)
      (unless (gnus-list-memq-of-list nnimap-unsplittable-articles
				      (cdr elem))
	(push (car elem) new)))
    (gnus-compress-sequence (nreverse new))))

(defun nnimap-make-split-specs (list)
  (let ((specs nil)
	entry)
    (dolist (elem list)
      (destructuring-bind (article spec) elem
	(dolist (group (delete nil (mapcar #'car spec)))
	  (unless (setq entry (assoc group specs))
	    (push (setq entry (list group)) specs))
	  (setcdr entry (cons article (cdr entry))))))
    (dolist (entry specs)
      (setcdr entry (gnus-compress-sequence (sort (cdr entry) #'<))))
    specs))

(defun nnimap-transform-split-mail ()
  (goto-char (point-min))
  (let (article bytes)
    (block nil
      (while (not (eobp))
	(while (not (looking-at "\\* [0-9]+ FETCH.+UID \\([0-9]+\\)"))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (when (eobp)
	    (return)))
	(setq article (match-string 1)
	      bytes (nnimap-get-length))
	(delete-region (line-beginning-position) (line-end-position))
	;; Insert MMDF separator, and a way to remember what this
	;; article UID is.
	(insert (format "\^A\^A\^A\^A\n\nX-nnimap-article: %s" article))
	(forward-char (1+ bytes))
	(setq bytes (nnimap-get-length))
	(delete-region (line-beginning-position) (line-end-position))
	;; There's a body; skip past that.
	(when bytes
	  (forward-char (1+ bytes))
	  (delete-region (line-beginning-position) (line-end-position)))))))

(defun nnimap-dummy-active-number (group &optional server)
  1)

(defun nnimap-save-mail-spec (group-art &optional server full-nov)
  (let (article)
    (goto-char (point-min))
    (if (not (re-search-forward "X-nnimap-article: \\([0-9]+\\)" nil t))
	(error "Invalid nnimap mail")
      (setq article (string-to-number (match-string 1))))
    (push (list article
		(if (eq group-art 'junk)
		    (list (cons 'junk 1))
		  group-art))
	  nnimap-incoming-split-list)))

(defun nnimap-make-thread-query (header)
  (let* ((id  (mail-header-id header))
	 (refs (split-string
		(or (mail-header-references header)
		    "")))
	 (value
	  (format
	   "(OR HEADER REFERENCES %S HEADER Message-Id %S)"
	   id id)))
    (dolist (refid refs value)
      (setq value (format
		   "(OR (OR HEADER Message-Id %S HEADER REFERENCES %S) %s)"
		   refid refid value)))))


(provide 'nnimap)

;;; nnimap.el ends here
