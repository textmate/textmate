;;; spam-report.el --- Reporting spam

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: network, spam, mail, gmane, report

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

;;; This module addresses a few aspects of spam reporting under Gnus.  Page
;;; breaks are used for grouping declarations and documentation relating to
;;; each particular aspect.

;;; Code:
(require 'gnus)
(require 'gnus-sum)

(autoload 'mm-url-insert "mm-url")

(defgroup spam-report nil
  "Spam reporting configuration."
  :group 'mail
  :group 'news)

(defcustom spam-report-gmane-regex nil
  "Regexp matching Gmane newsgroups, e.g. \"^nntp\\+.*:gmane\\.\"
If you are using spam.el, consider setting gnus-spam-process-newsgroups
or the gnus-group-spam-exit-processor-report-gmane group/topic parameter
instead."
  :type '(radio (const nil)
		(regexp :value "^nntp\+.*:gmane\."))
  :group 'spam-report)

(defcustom spam-report-gmane-use-article-number t
  "Whether the article number (faster!) or the header should be used.

You must set this to nil if you don't read Gmane groups directly
from news.gmane.org, e.g. when using local newsserver such as
leafnode."
  :type 'boolean
  :group 'spam-report)

(defcustom spam-report-url-ping-function
  'spam-report-url-ping-plain
  "Function to use for url ping spam reporting.
The function must accept the arguments `host' and `report'."
  :type '(choice
	  (const :tag "Connect directly"
		 spam-report-url-ping-plain)
	  (const :tag "Use the external program specified in `mm-url-program'"
		 spam-report-url-ping-mm-url)
	  (const :tag "Store request URLs in `spam-report-requests-file'"
		 spam-report-url-to-file)
	  (function :tag "User defined function" nil))
  :group 'spam-report)

(defcustom spam-report-requests-file
  (nnheader-concat gnus-directory "spam/" "spam-report-requests.url")
  ;; Is there a convention for the extension of such a file?
  ;; Should we use `spam-directory'?
  "File where spam report request are stored."
  :type 'file
  :group 'spam-report)

(defcustom spam-report-resend-to nil
  "Email address that spam articles are resent to when reporting.
If not set, the user will be prompted to enter a value which will be
saved for future use."
  :type 'string
  :group 'spam-report)

(defvar spam-report-url-ping-temp-agent-function nil
  "Internal variable for `spam-report-agentize' and `spam-report-deagentize'.
This variable will store the value of `spam-report-url-ping-function' from
before `spam-report-agentize' was run, so that `spam-report-deagentize' can
undo that change.")

(defun spam-report-resend (articles &optional ham)
  "Report an article as spam by resending via email.
Reports is as ham when HAM is set."
  (dolist (article articles)
    (gnus-message 6
		  "Reporting %s article %d to <%s>..."
		  (if ham "ham" "spam")
		  article spam-report-resend-to)
    (unless spam-report-resend-to
      (customize-set-variable
       spam-report-resend-to
       (read-from-minibuffer "email address to resend SPAM/HAM to? ")))
    ;; This is yanked from the `gnus-summary-resend-message' function.
    ;; It involves rendering the SPAM, which is undesirable, but there does
    ;; not seem to be a nicer way to achieve this.
    ;; select this particular article
    (gnus-summary-select-article nil nil nil article)
    ;; resend it to the destination address
    (with-current-buffer gnus-original-article-buffer
      (message-resend spam-report-resend-to))))

(defun spam-report-resend-ham (articles)
  "Report an article as ham by resending via email."
  (spam-report-resend articles t))

(defconst spam-report-gmane-max-requests 4
  "Number of reports to send before waiting for a response.")

(defvar spam-report-gmane-wait nil
  "When non-nil, wait until we get a server response.
This makes sure we don't DOS the host, if many reports are
submitted at once.  Internal variable.")

(defun spam-report-gmane-ham (&rest articles)
  "Report ARTICLES as ham (unregister) through Gmane."
  (interactive (gnus-summary-work-articles current-prefix-arg))
  (let ((count 0))
    (dolist (article articles)
      (setq count (1+ count))
      (let ((spam-report-gmane-wait
	     (zerop (% count spam-report-gmane-max-requests))))
	(spam-report-gmane-internal t article)))))

(defun spam-report-gmane-spam (&rest articles)
  "Report ARTICLES as spam through Gmane."
  (interactive (gnus-summary-work-articles current-prefix-arg))
  (let ((count 0))
    (dolist (article articles)
      (setq count (1+ count))
      (let ((spam-report-gmane-wait
	     (zerop (% count spam-report-gmane-max-requests))))
	(spam-report-gmane-internal nil article)))))

;; `spam-report-gmane' was an interactive entry point, so we should provide an
;; alias.
(defalias 'spam-report-gmane 'spam-report-gmane-spam)

(defun spam-report-gmane-internal (unspam article)
  "Report ARTICLE as spam or not-spam through Gmane, depending on UNSPAM."
  (when (and gnus-newsgroup-name
	     (or (null spam-report-gmane-regex)
		 (string-match spam-report-gmane-regex gnus-newsgroup-name)))
    (let ((rpt-host (if unspam "unspam.gmane.org" "spam.gmane.org")))
      (gnus-message 6 "Reporting article %d to %s..." article rpt-host)
      (cond
       ;; Special-case nnweb groups -- these have the URL to use in
       ;; the Xref headers.
       ((eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnweb)
	(spam-report-url-ping
	 rpt-host
	 (concat
	  "/"
	  (gnus-replace-in-string
	   (gnus-replace-in-string
	    (gnus-replace-in-string
	     (mail-header-xref (gnus-summary-article-header article))
	     "/raw" ":silent")
	    "^.*article.gmane.org/" "")
	   "/" ":"))))
       (spam-report-gmane-use-article-number
	(spam-report-url-ping
	 rpt-host
	 (format "/%s:%d"
		 (gnus-group-real-name gnus-newsgroup-name)
		 article)))
       (t
	(with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (gnus-request-head article gnus-newsgroup-name)
	  (let ((case-fold-search t)
		field host report url)
	    ;; First check for X-Report-Spam because it's more specific to
	    ;; spam reporting than Archived-At.  OTOH, all new articles on
	    ;; Gmane don't have X-Report-Spam anymore (unless Lars changes his
	    ;; mind :-)).
	    ;;
	    ;; There might be more than one Archived-At header so we need to
	    ;; find (and transform) the one related to Gmane.
	    (setq field (or (gnus-fetch-field "X-Report-Spam")
			    (gnus-fetch-field "X-Report-Unspam")
			    (gnus-fetch-field "Archived-At")))
	    (if (not (stringp field))
		(if (and (setq field (gnus-fetch-field "Xref"))
			 (string-match "[^ ]+ +\\([^ ]+\\)" field))
		    (setq report (concat "/" (match-string 1 field))
			  host rpt-host))
	      (setq host
		    (progn
		      (string-match
		       (concat "http://\\([a-z]+\\.gmane\\.org\\)"
			       "\\(/[^:/]+[:/][0-9]+\\)")
		       field)
		      (match-string 1 field)))
	      (setq report (match-string 2 field)))
	    (when host
	      (when (string-equal "permalink.gmane.org" host)
		(setq host rpt-host)
		(setq report (gnus-replace-in-string
			      report "/\\([0-9]+\\)$" ":\\1")))
	      (setq url (format "http://%s%s" host report)))
	    (if (not (and host report url))
		(gnus-message
		 3 "Could not find a spam report header in article %d..."
		 article)
	      (gnus-message 7 "Reporting article through URL %s..." url)
	      (spam-report-url-ping host report)))))))))

(defun spam-report-url-ping (host report)
  "Ping a host through HTTP, addressing a specific GET resource using
the function specified by `spam-report-url-ping-function'."
  ;; Example:
  ;; host: "spam.gmane.org"
  ;; report: "/gmane.some.group:123456"
  (funcall spam-report-url-ping-function host report))

(defcustom spam-report-user-mail-address
  (and (stringp user-mail-address)
       (gnus-replace-in-string user-mail-address "@" "<at>"))
  "Mail address of this user used for spam reports to Gmane.
This is initialized based on `user-mail-address'."
  :type '(choice string
		 (const :tag "Don't expose address" nil))
  :version "23.1" ;; No Gnus
  :group 'spam-report)

(defvar spam-report-user-agent
  (if spam-report-user-mail-address
      (format "%s (%s) %s" "spam-report.el"
	      spam-report-user-mail-address
	      (gnus-extended-version))
    (format "%s %s" "spam-report.el"
	    (gnus-extended-version))))

(defun spam-report-url-ping-plain (host report)
  "Ping a host through HTTP, addressing a specific GET resource."
  (let ((tcp-connection))
    (with-temp-buffer
      (or (setq tcp-connection
		(open-network-stream
		 "URL ping"
		 (buffer-name)
		 host
		 80))
	  (error "Could not open connection to %s" host))
      (set-marker (process-mark tcp-connection) (point-min))
      (gnus-set-process-query-on-exit-flag tcp-connection nil)
      (process-send-string
       tcp-connection
       (format "GET %s HTTP/1.1\nUser-Agent: %s\nHost: %s\n\n"
	       report spam-report-user-agent host))
      ;; Wait until we get something so we don't DOS the host, if
      ;; `spam-report-gmane-wait' is let-bound to t.
      (when spam-report-gmane-wait
	(gnus-message 7 "Waiting for response from %s..." host)
	(while (and (memq (process-status tcp-connection) '(open run))
		    (zerop (buffer-size)))
	  (accept-process-output tcp-connection 1))
	(gnus-message 7 "Waiting for response from %s... done" host)))))

;;;###autoload
(defun spam-report-process-queue (&optional file keep)
  "Report all queued requests from `spam-report-requests-file'.

If FILE is given, use it instead of `spam-report-requests-file'.
If KEEP is t, leave old requests in the file.  If KEEP is the
symbol `ask', query before flushing the queue file."
  (interactive
   (list (read-file-name
	  "File: "
	  (file-name-directory spam-report-requests-file)
	  spam-report-requests-file
	  nil
	  (file-name-nondirectory spam-report-requests-file))
	 current-prefix-arg))
  (if (eq spam-report-url-ping-function 'spam-report-url-to-file)
      (error (concat "Cannot process requests when "
		     "`spam-report-url-ping-function' is "
		     "`spam-report-url-to-file'."))
    (gnus-message 7 "Processing requests using `%s'."
		  spam-report-url-ping-function))
  (or file (setq file spam-report-requests-file))
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (while (and (not (eobp))
		(re-search-forward
		 "http://\\([^/]+\\)\\(/.*\\) *$" (point-at-eol) t))
      (let ((spam-report-gmane-wait
	     (zerop (% (mm-line-number-at-pos)
		       spam-report-gmane-max-requests))))
	(gnus-message 6 "Reporting %s%s..."
		      (match-string 1) (match-string 2))
	(funcall spam-report-url-ping-function
		 (match-string 1) (match-string 2)))
      (forward-line 1))
    (if (or (eq keep nil)
	    (and (eq keep 'ask)
		 (y-or-n-p
		  (format
		   "Flush requests from `%s'? " (current-buffer)))))
	(progn
	  (gnus-message 7 "Flushing request file `%s'"
			spam-report-requests-file)
	  (erase-buffer)
	  (save-buffer)
	  (kill-buffer (current-buffer)))
      (gnus-message 7 "Keeping requests in `%s'" spam-report-requests-file))))

;;;###autoload
(defun spam-report-url-ping-mm-url (host report)
  "Ping a host through HTTP, addressing a specific GET resource. Use
the external program specified in `mm-url-program' to connect to
server."
  (with-temp-buffer
    (let ((url (format "http://%s%s" host report)))
      (mm-url-insert url t))))

;;;###autoload
(defun spam-report-url-to-file (host report)
  "Collect spam report requests in `spam-report-requests-file'.
Customize `spam-report-url-ping-function' to use this function."
  (let ((url (format "http://%s%s" host report))
	(file spam-report-requests-file))
    (gnus-make-directory (file-name-directory file))
    (gnus-message 9 "Writing URL `%s' to file `%s'" url file)
    (with-temp-buffer
      (insert url)
      (newline)
      (append-to-file (point-min) (point-max) file))))

;;;###autoload
(defun spam-report-agentize ()
  "Add spam-report support to the Agent.
Spam reports will be queued with \\[spam-report-url-to-file] when
the Agent is unplugged, and will be submitted in a batch when the
Agent is plugged."
  (interactive)
  (add-hook 'gnus-agent-plugged-hook 'spam-report-plug-agent)
  (add-hook 'gnus-agent-unplugged-hook 'spam-report-unplug-agent))

;;;###autoload
(defun spam-report-deagentize ()
  "Remove spam-report support from the Agent.
Spam reports will be queued with the method used when
\\[spam-report-agentize] was run."
  (interactive)
  (remove-hook 'gnus-agent-plugged-hook 'spam-report-plug-agent)
  (remove-hook 'gnus-agent-unplugged-hook 'spam-report-unplug-agent))

(defun spam-report-plug-agent ()
  "Adjust spam report settings for plugged state.
Process queued spam reports."
  ;; Process the queue, unless the user only wanted to report to a file
  ;; anyway.
  (unless (equal spam-report-url-ping-temp-agent-function
		 'spam-report-url-to-file)
    (spam-report-process-queue))
  ;; Set the reporting function, if we have memorized something otherwise,
  ;; stick with plain URL reporting.
  (setq spam-report-url-ping-function
	(or spam-report-url-ping-temp-agent-function
	    'spam-report-url-ping-plain)))

(defun spam-report-unplug-agent ()
  "Restore spam report settings for unplugged state."
  ;; save the old value
  (setq spam-report-url-ping-temp-agent-function
	spam-report-url-ping-function)
  ;; store all reports to file
  (setq spam-report-url-ping-function
	'spam-report-url-to-file))

(provide 'spam-report)

;;; spam-report.el ends here.
