;;; emacsbug.el --- command to report Emacs bugs to appropriate mailing list

;; Copyright (C) 1985, 1994, 1997-1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: maint mail
;; Package: emacs

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

;; `M-x report-emacs-bug' starts an email note to the Emacs maintainers
;; describing a problem.  You need to be able to send mail from Emacs
;; to complete the process.  Alternatively, compose the bug report in
;; Emacs then paste it into your normal mail client.

;;; Code:

(require 'sendmail)
(require 'message)

(defgroup emacsbug nil
  "Sending Emacs bug reports."
  :group 'maint
  :group 'mail)

(define-obsolete-variable-alias 'report-emacs-bug-pretest-address
  'report-emacs-bug-address "24.1")

(defcustom report-emacs-bug-address "bug-gnu-emacs@gnu.org"
  "Address of mailing list for GNU Emacs bugs."
  :group 'emacsbug
  :type 'string)

(defcustom report-emacs-bug-no-confirmation nil
  "If non-nil, suppress the confirmations asked for the sake of novice users."
  :group 'emacsbug
  :type 'boolean)

(defcustom report-emacs-bug-no-explanations nil
  "If non-nil, suppress the explanations given for the sake of novice users."
  :group 'emacsbug
  :type 'boolean)

;; User options end here.

(defvar report-emacs-bug-tracker-url "http://debbugs.gnu.org/cgi/"
  "Base URL of the GNU bugtracker.
Used for querying duplicates and linking to existing bugs.")

(defvar report-emacs-bug-orig-text nil
  "The automatically-created initial text of the bug report.")

(defvar report-emacs-bug-send-command nil
  "Name of the command to send the bug report, as a string.")
(make-variable-buffer-local 'report-emacs-bug-send-command)

(defvar report-emacs-bug-send-hook nil
  "Hook run before sending the bug report.")
(make-variable-buffer-local 'report-emacs-bug-send-hook)

(declare-function x-server-vendor "xfns.c" (&optional terminal))
(declare-function x-server-version "xfns.c" (&optional terminal))
(declare-function message-sort-headers "message" ())
(defvar message-strip-special-text-properties)

(defun report-emacs-bug-can-use-osx-open ()
  "Return non-nil if the OS X \"open\" command is available for mailing."
  (and (featurep 'ns)
       (equal (executable-find "open") "/usr/bin/open")
       (memq system-type '(darwin))))

;; FIXME this duplicates much of the logic from browse-url-can-use-xdg-open.
(defun report-emacs-bug-can-use-xdg-email ()
  "Return non-nil if the \"xdg-email\" command can be used.
xdg-email is a desktop utility that calls your preferred mail client.
This requires you to be running either Gnome, KDE, or Xfce4."
  (and (getenv "DISPLAY")
       (executable-find "xdg-email")
       (or (getenv "GNOME_DESKTOP_SESSION_ID")
	   ;; GNOME_DESKTOP_SESSION_ID is deprecated, check on Dbus also.
	   (condition-case nil
	       (eq 0 (call-process
		      "dbus-send" nil nil nil
				  "--dest=org.gnome.SessionManager"
				  "--print-reply"
				  "/org/gnome/SessionManager"
				  "org.gnome.SessionManager.CanShutdown"))
	     (error nil))
	   (equal (getenv "KDE_FULL_SESSION") "true")
	   ;; FIXME? browse-url-can-use-xdg-open also accepts LXDE.
	   ;; Is that no good here, or just overlooked?
	   (condition-case nil
	       (eq 0 (call-process
		      "/bin/sh" nil nil nil
		      "-c"
		      ;; FIXME use string-match rather than grep.
		      "xprop -root _DT_SAVE_MODE|grep xfce4"))
	     (error nil)))))

(defun report-emacs-bug-insert-to-mailer ()
  "Send the message to your preferred mail client.
This requires either the OS X \"open\" command, or the freedesktop
\"xdg-email\" command to be available."
  (interactive)
  (save-excursion
    ;; FIXME? use mail-fetch-field?
    (let* ((to (progn
		 (goto-char (point-min))
		 (forward-line)
		 (and (looking-at "^To: \\(.*\\)")
		      (match-string-no-properties 1))))
	   (subject (progn
		      (forward-line)
		      (and (looking-at "^Subject: \\(.*\\)")
			   (match-string-no-properties 1))))
	   (body (progn
		   (forward-line 2)
		   (if (> (point-max) (point))
		       (buffer-substring-no-properties (point) (point-max))))))
      (if (and to subject body)
	  (if (report-emacs-bug-can-use-osx-open)
	      (start-process "/usr/bin/open" nil "open"
			     (concat "mailto:" to
				     "?subject=" (url-hexify-string subject)
				     "&body=" (url-hexify-string body)))
	    (start-process "xdg-email" nil "xdg-email"
			   "--subject" subject
			   "--body" body
			   (concat "mailto:" to)))
	(error "Subject, To or body not found")))))

;;;###autoload
(defun report-emacs-bug (topic &optional recent-keys)
  "Report a bug in GNU Emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive (reverse (list (recent-keys) (read-string "Bug Subject: "))))
  ;; The syntax `version;' is preferred to `[version]' because the
  ;; latter could be mistakenly stripped by mailing software.
  (if (eq system-type 'ms-dos)
      (setq topic (concat emacs-version "; " topic))
    (when (string-match "^\\(\\([.0-9]+\\)*\\)\\.[0-9]+$" emacs-version)
      (setq topic (concat (match-string 1 emacs-version) "; " topic))))
  (let ((from-buffer (current-buffer))
        ;; Put these properties on semantically-void text.
        ;; report-emacs-bug-hook deletes these regions before sending.
        (prompt-properties '(field emacsbug-prompt
                             intangible but-helpful
                             rear-nonsticky t))
	(can-insert-mail (or (report-emacs-bug-can-use-xdg-email)
			     (report-emacs-bug-can-use-osx-open)))
        user-point message-end-point)
    (setq message-end-point
	  (with-current-buffer (get-buffer-create "*Messages*")
	    (point-max-marker)))
    (compose-mail report-emacs-bug-address topic)
    ;; The rest of this does not execute if the user was asked to
    ;; confirm and said no.
    (when (eq major-mode 'message-mode)
      ;; Message-mode sorts the headers before sending.  We sort now so
      ;; that report-emacs-bug-orig-text remains valid.  (Bug#5178)
      (message-sort-headers)
      ;; Stop message-mode stealing the properties we will add.
      (set (make-local-variable 'message-strip-special-text-properties) nil))
    (rfc822-goto-eoh)
    (forward-line 1)
    ;; Move the mail signature to the proper place.
    (let ((signature (buffer-substring (point) (point-max)))
	  (inhibit-read-only t))
      (delete-region (point) (point-max))
      (insert signature)
      (backward-char (length signature)))
    (unless report-emacs-bug-no-explanations
      ;; Insert warnings for novice users.
      (if (not (equal "bug-gnu-emacs@gnu.org" report-emacs-bug-address))
	  (insert (format "The report will be sent to %s.\n\n"
			  report-emacs-bug-address))
	(insert "This bug report will be sent to the ")
	(insert-button
	 "Bug-GNU-Emacs"
	 'face 'link
	 'help-echo (concat "mouse-2, RET: Follow this link")
	 'action (lambda (button)
		   (browse-url "http://lists.gnu.org/archive/html/bug-gnu-emacs/"))
	 'follow-link t)
	(insert " mailing list\nand the GNU bug tracker at ")
	(insert-button
	 "debbugs.gnu.org"
	 'face 'link
	 'help-echo (concat "mouse-2, RET: Follow this link")
	 'action (lambda (button)
		   (browse-url "http://debbugs.gnu.org/"))
	 'follow-link t)

	(insert ".  Please check that
the From: line contains a valid email address.  After a delay of up
to one day, you should receive an acknowledgement at that address.

Please write in English if possible, as the Emacs maintainers
usually do not have translators for other languages.\n\n")))

    (insert "Please describe exactly what actions triggered the bug, and\n"
	    "the precise symptoms of the bug.  If you can, give a recipe\n"
	    "starting from `emacs -Q':\n\n")
    (add-text-properties (save-excursion
                           (rfc822-goto-eoh)
                           (line-beginning-position 2))
                         (point)
                         prompt-properties)
    (setq user-point (point))
    (insert "\n\n")

    (insert "If Emacs crashed, and you have the Emacs process in the gdb debugger,\n"
	    "please include the output from the following gdb commands:\n"
	    "    `bt full' and `xbacktrace'.\n")

    (let ((debug-file (expand-file-name "DEBUG" data-directory)))
      (if (file-readable-p debug-file)
	  (insert "For information about debugging Emacs, please read the file\n"
		  debug-file ".\n")))
    (add-text-properties (1+ user-point) (point) prompt-properties)

    (insert "\n\nIn " (emacs-version) "\n")
    (if (fboundp 'x-server-vendor)
	(condition-case nil
            ;; This is used not only for X11 but also W32 and others.
	    (insert "Windowing system distributor `" (x-server-vendor)
                    "', version "
		    (mapconcat 'number-to-string (x-server-version) ".") "\n")
	  (error t)))
    (when (and system-configuration-options
	       (not (equal system-configuration-options "")))
      (insert "Configured using:\n `configure "
	      system-configuration-options "'\n\n")
      (fill-region (line-beginning-position -1) (point)))
    (insert "Important settings:\n")
    (mapc
     (lambda (var)
       (insert (format "  value of $%s: %s\n" var (getenv var))))
     '("LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES"
       "LC_MONETARY" "LC_NUMERIC" "LC_TIME" "LANG" "XMODIFIERS"))
    (insert (format "  locale-coding-system: %s\n" locale-coding-system))
    (insert (format "  default enable-multibyte-characters: %s\n"
		    (default-value 'enable-multibyte-characters)))
    (insert "\n")
    (insert (format "Major mode: %s\n"
		    (format-mode-line
                     (buffer-local-value 'mode-name from-buffer)
                     nil nil from-buffer)))
    (insert "\n")
    (insert "Minor modes in effect:\n")
    (dolist (mode minor-mode-list)
      (and (boundp mode) (buffer-local-value mode from-buffer)
	   (insert (format "  %s: %s\n" mode
			   (buffer-local-value mode from-buffer)))))
    (insert "\n")
    (insert "Recent input:\n")
    (let ((before-keys (point)))
      (insert (mapconcat (lambda (key)
			   (if (or (integerp key)
				   (symbolp key)
				   (listp key))
			       (single-key-description key)
			     (prin1-to-string key nil)))
			 (or recent-keys (recent-keys))
			 " "))
      (save-restriction
	(narrow-to-region before-keys (point))
	(goto-char before-keys)
	(while (progn (move-to-column 50) (not (eobp)))
	  (search-forward " " nil t)
	  (insert "\n"))))
    (let ((message-buf (get-buffer "*Messages*")))
      (if message-buf
	  (let (beg-pos
		(end-pos message-end-point))
	    (with-current-buffer message-buf
	      (goto-char end-pos)
	      (forward-line -10)
	      (setq beg-pos (point)))
	    (insert "\n\nRecent messages:\n")
	    (insert-buffer-substring message-buf beg-pos end-pos))))
    ;; After Recent messages, to avoid the messages produced by
    ;; list-load-path-shadows.
    (unless (looking-back "\n")
      (insert "\n"))
    (insert "\n")
    (insert "Load-path shadows:\n")
    (message "Checking for load-path shadows...")
    (let ((shadows (list-load-path-shadows t)))
      (message "Checking for load-path shadows...done")
      (insert (if (zerop (length shadows))
                  "None found.\n"
                shadows)))
    (insert (format "\nFeatures:\n%s\n" features))
    (fill-region (line-beginning-position 0) (point))
    ;; This is so the user has to type something in order to send easily.
    (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-c\C-i" 'report-emacs-bug-info)
    (if can-insert-mail
	(define-key (current-local-map) "\C-cm"
	  'report-emacs-bug-insert-to-mailer))
    (setq report-emacs-bug-send-command (get mail-user-agent 'sendfunc)
	  report-emacs-bug-send-hook (get mail-user-agent 'hookvar))
    (if report-emacs-bug-send-command
	(setq report-emacs-bug-send-command
	      (symbol-name report-emacs-bug-send-command)))
    (unless report-emacs-bug-no-explanations
      (with-output-to-temp-buffer "*Bug Help*"
	(princ "While in the mail buffer:\n\n")
        (if report-emacs-bug-send-command
            (princ (substitute-command-keys
                    (format "  Type \\[%s] to send the bug report.\n"
                            report-emacs-bug-send-command))))
	(princ (substitute-command-keys
		"  Type \\[kill-buffer] RET to cancel (don't send it).\n"))
	(if can-insert-mail
	    (princ (substitute-command-keys
		    "  Type \\[report-emacs-bug-insert-to-mailer] to copy text to your preferred mail program.\n")))
	(terpri)
	(princ (substitute-command-keys
		"  Type \\[report-emacs-bug-info] to visit in Info the Emacs Manual section
    about when and how to write a bug report, and what
    information you should include to help fix the bug.")))
      (shrink-window-if-larger-than-buffer (get-buffer-window "*Bug Help*")))
    ;; Make it less likely people will send empty messages.
    (if report-emacs-bug-send-hook
        (add-hook report-emacs-bug-send-hook 'report-emacs-bug-hook nil t))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (make-local-variable 'report-emacs-bug-orig-text)
    (setq report-emacs-bug-orig-text
          (buffer-substring-no-properties (point-min) (point)))
    (goto-char user-point)))

(defun report-emacs-bug-info ()
  "Go to the Info node on reporting Emacs bugs."
  (interactive)
  (info "(emacs)Bugs"))

;; It's the default mail mode, so it seems OK to use its features.
(autoload 'message-bogus-recipient-p "message")
(defvar message-send-mail-function)

(defun report-emacs-bug-hook ()
  "Do some checking before sending a bug report."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (and (= (- (point) (point-min))
            (length report-emacs-bug-orig-text))
         (string-equal (buffer-substring-no-properties (point-min) (point))
                       report-emacs-bug-orig-text)
         (error "No text entered in bug report"))
    ;; Warning for novice users.
    (unless (or report-emacs-bug-no-confirmation
		(yes-or-no-p
		 "Send this bug report to the Emacs maintainers? "))
      (goto-char (point-min))
      (if (search-forward "To: ")
          (delete-region (point) (line-end-position)))
      (if report-emacs-bug-send-hook
          (kill-local-variable report-emacs-bug-send-hook))
      (with-output-to-temp-buffer "*Bug Help*"
	(princ (substitute-command-keys
                (format "\
You invoked the command M-x report-emacs-bug,
but you decided not to mail the bug report to the Emacs maintainers.

If you want to mail it to someone else instead,
please insert the proper e-mail address after \"To: \",
and send the mail again%s."
                        (if report-emacs-bug-send-command
                            (format " using \\[%s]"
                                    report-emacs-bug-send-command)
                          "")))))
      (error "M-x report-emacs-bug was cancelled, please read *Bug Help* buffer"))
    ;; Query the user for the SMTP method, so that we can skip
    ;; questions about From header validity if the user is going to
    ;; use mailclient, anyway.
    (when (or (and (derived-mode-p 'message-mode)
		   (eq message-send-mail-function 'sendmail-query-once))
	      (and (not (derived-mode-p 'message-mode))
		   (eq send-mail-function 'sendmail-query-once)))
      (sendmail-query-user-about-smtp)
      (when (derived-mode-p 'message-mode)
	(setq message-send-mail-function (message-default-send-mail-function))))
    (or report-emacs-bug-no-confirmation
	;; mailclient.el does not need a valid From
	(if (derived-mode-p 'message-mode)
	    (eq message-send-mail-function 'message-send-mail-with-mailclient)
	  (eq send-mail-function 'mailclient-send-it))
	;; Not narrowing to the headers, but that's OK.
	(let ((from (mail-fetch-field "From")))
	  (and (or (not from)
		   (message-bogus-recipient-p from)
		   ;; This is the default user-mail-address.  On today's
		   ;; systems, it seems more likely to be wrong than right,
		   ;; since most people don't run their own mail server.
		   (string-match (format "\\<%s@%s\\>"
					 (regexp-quote (user-login-name))
					 (regexp-quote (system-name)))
				 from))
	       (not (yes-or-no-p
		     (format "Is `%s' really your email address? " from)))
	       (error "Please edit the From address and try again"))))
    ;; Delete the uninteresting text that was just to help fill out the report.
    (rfc822-goto-eoh)
    (forward-line 1)
    (let ((pos (1- (point))))
      (while (setq pos (text-property-any pos (point-max)
                                          'field 'emacsbug-prompt))
        (delete-region pos (field-end (1+ pos)))))))


;; Querying the bug database

(defvar report-emacs-bug-bug-alist nil)
(make-variable-buffer-local 'report-emacs-bug-bug-alist)
(defvar report-emacs-bug-choice-widget nil)
(make-variable-buffer-local 'report-emacs-bug-choice-widget)

(defun report-emacs-bug-create-existing-bugs-buffer (bugs keywords)
  (switch-to-buffer (get-buffer-create "*Existing Emacs Bugs*"))
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq report-emacs-bug-bug-alist bugs)
    (widget-insert (propertize (concat "Already known bugs ("
				       keywords "):\n\n")
			       'face 'bold))
    (if bugs
	(setq report-emacs-bug-choice-widget
	      (apply 'widget-create 'radio-button-choice
		     :value (caar bugs)
		     (let (items)
		       (dolist (bug bugs)
			 (push (list
				'url-link
				:format (concat "Bug#" (number-to-string (nth 2 bug))
						": " (cadr bug) "\n    %[%v%]\n")
				;; FIXME: Why is only the link of the
				;; active item clickable?
				(car bug))
			       items))
		       (nreverse items))))
      (widget-insert "No bugs matching your keywords found.\n"))
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     ;; TODO: Do something!
			     (message "Reporting new bug!"))
		   "Report new bug")
    (when bugs
      (widget-insert " ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (let ((val (widget-value report-emacs-bug-choice-widget)))
				 ;; TODO: Do something!
				 (message "Appending to bug %s!"
					  (nth 2 (assoc val report-emacs-bug-bug-alist)))))
		     "Append to chosen bug"))
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-buffer))
		   "Quit reporting bug")
    (widget-insert "\n"))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun report-emacs-bug-parse-query-results (status keywords)
  (goto-char (point-min))
  (let (buglist)
    (while (re-search-forward "<a href=\"bugreport\\.cgi\\?bug=\\([[:digit:]]+\\)\">\\([^<]+\\)</a>" nil t)
      (let ((number (match-string 1))
	    (subject (match-string 2)))
	(when (not (string-match "^#" subject))
	  (push (list
		 ;; first the bug URL
		 (concat report-emacs-bug-tracker-url
			 "bugreport.cgi?bug=" number)
		 ;; then the subject and number
		 subject (string-to-number number))
		buglist))))
    (report-emacs-bug-create-existing-bugs-buffer (nreverse buglist) keywords)))

;;;###autoload
(defun report-emacs-bug-query-existing-bugs (keywords)
  "Query for KEYWORDS at `report-emacs-bug-tracker-url', and return the result.
The result is an alist with items of the form (URL SUBJECT NO)."
  (interactive "sBug keywords (comma separated): ")
  (url-retrieve (concat report-emacs-bug-tracker-url
			"pkgreport.cgi?include=subject%3A"
			(replace-regexp-in-string "[[:space:]]+" "+" keywords)
			";package=emacs")
		'report-emacs-bug-parse-query-results (list keywords)))

(provide 'emacsbug)

;;; emacsbug.el ends here
