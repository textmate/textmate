;;; sendmail.el --- mail sending commands for Emacs

;; Copyright (C) 1985-1986, 1992-1996, 1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;; This mode provides mail-sending facilities from within Emacs.  It is
;; documented in the Emacs user's manual.

;;; Code:
(require 'mail-utils)

(require 'rfc2047)

(defgroup sendmail nil
  "Mail sending commands for Emacs."
  :prefix "mail-"
  :group 'mail)

(defcustom mail-setup-with-from t
  "Non-nil means insert `From:' field when setting up the message."
  :type 'boolean
  :group 'sendmail
  :version "22.1")

(defcustom sendmail-program
  (or (executable-find "sendmail")
      (cond
       ((file-exists-p "/usr/sbin/sendmail") "/usr/sbin/sendmail")
       ((file-exists-p "/usr/lib/sendmail") "/usr/lib/sendmail")
       ((file-exists-p "/usr/ucblib/sendmail") "/usr/ucblib/sendmail")
       (t "sendmail")))
  "Program used to send messages."
  :version "24.1"		; add executable-find, remove fakemail
  :group 'mail
  :type 'file)

;;;###autoload
(defcustom mail-from-style 'default
  "Specifies how \"From:\" fields look.

If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>

Otherwise, most addresses look like `angles', but they look like
`parens' if `angles' would need quoting and `parens' would not."
  ;; The value `system-default' is now deprecated.
  :type '(choice (const :tag "simple" nil)
		 (const parens)
		 (const angles)
		 (const default))
  :version "20.3"
  :group 'sendmail)

;;;###autoload
(defcustom mail-specify-envelope-from nil
  "If non-nil, specify the envelope-from address when sending mail.
The value used to specify it is whatever is found in
the variable `mail-envelope-from', with `user-mail-address' as fallback.

On most systems, specifying the envelope-from address is a
privileged operation.  This variable affects sendmail and
smtpmail -- if you use feedmail to send mail, see instead the
variable `feedmail-deduce-envelope-from'."
  :version "21.1"
  :type 'boolean
  :group 'sendmail)

(defcustom mail-envelope-from nil
  "If non-nil, designate the envelope-from address when sending mail.
This only has an effect if `mail-specify-envelope-from' is non-nil.
The value should be either a string, or the symbol `header' (in
which case the contents of the \"From\" header of the message
being sent is used), or nil (in which case the value of
`user-mail-address' is used)."
  :version "21.1"
  :type '(choice (string :tag "From-name")
		 (const :tag "Use From: header from message" header)
		 (const :tag "Use `user-mail-address'" nil))
  :group 'sendmail)

;;;###autoload
(defcustom mail-self-blind nil
  "Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default."
  :type 'boolean
  :group 'sendmail)

;;;###autoload
(defcustom mail-interactive t
  ;; We used to use a default of nil rather than t, but nowadays it is very
  ;; common for sendmail to be misconfigured, so one cannot rely on the
  ;; bounce message to be delivered anywhere, least of all to the
  ;; user's mailbox.
  "Non-nil means when sending a message wait for and display errors.
Otherwise, let mailer send back a message to report errors."
  :type 'boolean
  :version "23.1"			; changed from nil to t
  :group 'sendmail)

(defcustom mail-yank-ignored-headers
  (concat "^"
          (regexp-opt '("via" "mail-from" "origin" "status" "remailed"
                        "received" "message-id" "summary-line" "to" "subject"
                        "in-reply-to" "return-path" "mail-reply-to"
                        ;; Should really be rmail-attribute-header and
                        ;; rmail-keyword-header, but this file does not
                        ;; require rmail (at run time).
                        "x-rmail-attributes" "x-rmail-keywords"
                        "mail-followup-to") "\\(?:")
          ":")
  "Delete these headers from old message when it's inserted in a reply."
  :type 'regexp
  :group 'sendmail
  :version "23.1")

;; Useful to set in site-init.el
;;;###autoload
(defcustom send-mail-function
  ;; Assume smtpmail is the preferred choice if it's already configured.
  (if (and (boundp 'smtpmail-smtp-server)
           smtpmail-smtp-server)
      'smtpmail-send-it 'sendmail-query-once)
  "Function to call to send the current buffer as mail.
The headers should be delimited by a line which is
not a valid RFC822 header or continuation line,
that matches the variable `mail-header-separator'.
This is used by the default mail-sending commands.  See also
`message-send-mail-function' for use with the Message package."
  :type '(radio (function-item sendmail-send-it :tag "Use Sendmail package")
		(function-item sendmail-query-once :tag "Query the user")
		(function-item smtpmail-send-it :tag "Use SMTPmail package")
		(function-item feedmail-send-it :tag "Use Feedmail package")
		(function-item mailclient-send-it :tag "Use Mailclient package")
		function)
  :version "24.1"
  :group 'sendmail)

;;;###autoload
(defcustom mail-header-separator (purecopy "--text follows this line--")
  "Line used to separate headers from text in messages being composed."
  :type 'string
  :group 'sendmail)

;; Set up mail-header-separator for use as a category text property.
(put 'mail-header-separator 'rear-nonsticky '(category))
;; This was a nice idea, for preventing accidental modification of
;; the separator.   But I found it also prevented or obstructed
;; certain deliberate operations, such as copying the separator line
;; up to the top to send myself a copy of an already sent outgoing message
;; and other things.  So I turned it off.  --rms.
;;(put 'mail-header-separator 'read-only t)

;;;###autoload
(defcustom mail-archive-file-name nil
  "Name of file to write all outgoing messages in, or nil for none.
This is normally an mbox file, but for backwards compatibility may also
be a Babyl file."
  :type '(choice file (const nil))
  :group 'sendmail)

;;;###autoload
(defcustom mail-default-reply-to nil
  "Address to insert as default Reply-to field of outgoing messages.
If nil, it will be initialized from the REPLYTO environment variable
when you first send mail."
  :type '(choice (const nil) string)
  :group 'sendmail)

(defcustom mail-alias-file nil
  "If non-nil, the name of a file to use instead of the sendmail default.
This file defines aliases to be expanded by the mailer; this is a different
feature from that of defining aliases in `.mailrc' to be expanded in Emacs.
This variable has no effect unless your system uses sendmail as its mailer.
The default file is defined in sendmail's configuration file, e.g.
`/etc/aliases'."
  :type '(choice (const :tag "Sendmail default" nil) file)
  :group 'sendmail)

;;;###autoload
(defcustom mail-personal-alias-file (purecopy "~/.mailrc")
  "If non-nil, the name of the user's personal mail alias file.
This file typically should be in same format as the `.mailrc' file used by
the `Mail' or `mailx' program.
This file need not actually exist."
  :type '(choice (const nil) file)
  :group 'sendmail)

;;;###autoload
(defcustom mail-setup-hook nil
  "Normal hook, run each time a new outgoing message is initialized."
  :type 'hook
  :options '(fortune-to-signature spook mail-abbrevs-setup)
  :group 'sendmail)

;;;###autoload
(defvar mail-aliases t
  "Alist of mail address aliases,
or t meaning should be initialized from your mail aliases file.
\(The file's name is normally `~/.mailrc', but `mail-personal-alias-file'
can specify a different file name.)
The alias definitions in the file have this form:
    alias ALIAS MEANING")

(defvar mail-alias-modtime nil
  "The modification time of your mail alias file when it was last examined.")

;;;###autoload
(defcustom mail-yank-prefix "> "
  "Prefix insert on lines of yanked message being replied to.
If this is nil, use indentation, as specified by `mail-indentation-spaces'."
  :type '(choice (const nil) string)
  :group 'sendmail)

;;;###autoload
(defcustom mail-indentation-spaces 3
  "Number of spaces to insert at the beginning of each cited line.
Used by `mail-yank-original' via `mail-indent-citation'."
  :type 'integer
  :group 'sendmail)

;; FIXME make it really obsolete.
(defvar mail-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

This is a normal hook, misnamed for historical reasons.
It is semi-obsolete and mail agents should no longer use it.")

;;;###autoload
(defcustom mail-citation-hook nil
  "Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t),
and should leave point and mark around the citation text as modified.
The hook functions can find the header of the cited message
in the variable `mail-citation-header', whether or not this is included
in the cited portion of the message.

If this hook is entirely empty (nil), a default action is taken
instead of no action."
  :type 'hook
  :group 'sendmail)

(defvar mail-citation-header nil
  "While running `mail-citation-hook', this variable holds the message header.
This enables the hook functions to see the whole message header
regardless of what part of it (if any) is included in the cited text.")

;;;###autoload
(defcustom mail-citation-prefix-regexp
  (purecopy "\\([ \t]*\\(\\w\\|[_.]\\)+>+\\|[ \t]*[]>|]\\)+")
  "Regular expression to match a citation prefix plus whitespace.
It should match whatever sort of citation prefixes you want to handle,
with whitespace before and after; it should also match just whitespace.
The default value matches citations like `foo-bar>' plus whitespace."
  :type 'regexp
  :group 'sendmail
  :version "24.1")

(defvar mail-abbrevs-loaded nil)
(defvar mail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\t" 'completion-at-point)
    (define-key map "\C-c?" 'describe-mode)
    (define-key map "\C-c\C-f\C-t" 'mail-to)
    (define-key map "\C-c\C-f\C-b" 'mail-bcc)
    (define-key map "\C-c\C-f\C-f" 'mail-fcc)
    (define-key map "\C-c\C-f\C-c" 'mail-cc)
    (define-key map "\C-c\C-f\C-s" 'mail-subject)
    (define-key map "\C-c\C-f\C-r" 'mail-reply-to)
    (define-key map "\C-c\C-f\C-a" 'mail-mail-reply-to)    ; author
    (define-key map "\C-c\C-f\C-l" 'mail-mail-followup-to) ; list
    (define-key map "\C-c\C-t" 'mail-text)
    (define-key map "\C-c\C-y" 'mail-yank-original)
    (define-key map "\C-c\C-r" 'mail-yank-region)
    (define-key map [remap split-line] 'mail-split-line)
    (define-key map "\C-c\C-q" 'mail-fill-yanked-message)
    (define-key map "\C-c\C-w" 'mail-signature)
    (define-key map "\C-c\C-c" 'mail-send-and-exit)
    (define-key map "\C-c\C-s" 'mail-send)
    (define-key map "\C-c\C-i" 'mail-insert-file)
    ;; FIXME add this? "b" = bury buffer.  It's in the menu-bar.
;;;    (define-key map "\C-c\C-b" 'mail-dont-send)

    (define-key map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))

    (define-key map [menu-bar mail attachment]
      '("Attach File" . mail-add-attachment))

    (define-key map [menu-bar mail fill]
      '("Fill Citation" . mail-fill-yanked-message))

    (define-key map [menu-bar mail yank]
      '(menu-item "Cite Original" mail-yank-original :enable mail-reply-action))

    (define-key map [menu-bar mail signature]
      '("Insert Signature" . mail-signature))

    (define-key map [menu-bar mail mail-sep]
      '("--"))

    (define-key map [menu-bar mail cancel]
      '("Cancel" . mail-dont-send))

    (define-key map [menu-bar mail send-stay]
      '("Send, Keep Editing" . mail-send))

    (define-key map [menu-bar mail send]
      '("Send Message" . mail-send-and-exit))

    (define-key map [menu-bar headers]
      (cons "Headers" (make-sparse-keymap "Move to Header")))

    (define-key map [menu-bar headers text]
      '("Text" . mail-text))

    (define-key map [menu-bar headers expand-aliases]
      '("Expand Aliases" . expand-mail-aliases))

    (define-key map [menu-bar headers mail-reply-to]
      '("Mail-Reply-To" . mail-mail-reply-to))

    (define-key map [menu-bar headers mail-followup-to]
      '("Mail-Followup-To" . mail-mail-followup-to))

    (define-key map [menu-bar headers reply-to]
      '("Reply-To" . mail-reply-to))

    (define-key map [menu-bar headers bcc]
      '("Bcc" . mail-bcc))

    (define-key map [menu-bar headers fcc]
      '("Fcc" . mail-fcc))

    (define-key map [menu-bar headers cc]
      '("Cc" . mail-cc))

    (define-key map [menu-bar headers subject]
      '("Subject" . mail-subject))

    (define-key map [menu-bar headers to]
      '("To" . mail-to))

    map))

(autoload 'build-mail-aliases "mailalias"
  "Read mail aliases from personal aliases file and set `mail-aliases'.
By default, this is the file specified by `mail-personal-alias-file'." t)

;;;###autoload
(defcustom mail-signature t
  "Text inserted at end of mail buffer when a message is initialized.
If t, it means to insert the contents of the file `mail-signature-file'.
If a string, that string is inserted.
 (To make a proper signature, the string should begin with \\n\\n-- \\n,
  which is the standard way to delimit a signature in a message.)
Otherwise, it should be an expression; it is evaluated
and should insert whatever you want to insert."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Use `.signature' file" t)
		 (string :tag "String to insert")
		 (sexp :tag "Expression to evaluate"))
  :group 'sendmail)
(put 'mail-signature 'risky-local-variable t)

;;;###autoload
(defcustom mail-signature-file (purecopy "~/.signature")
  "File containing the text inserted at end of mail buffer."
  :type 'file
  :group 'sendmail)

;;;###autoload
(defcustom mail-default-directory (purecopy "~/")
  "Value of `default-directory' for Mail mode buffers.
This directory is used for auto-save files of Mail mode buffers.

Note that Message mode does not use this variable; it auto-saves
in `message-auto-save-directory'."
  :type '(directory :tag "Directory")
  :group 'sendmail
  :version "22.1")

(defvar mail-reply-action nil)
(defvar mail-send-actions nil
  "A list of actions to be performed upon successful sending of a message.")
(defvar mail-return-action nil)

;;;###autoload
(defcustom mail-default-headers nil
  "A string containing header lines, to be inserted in outgoing messages.
It can contain newlines, and should end in one.  It is inserted
before you edit the message, so you can edit or delete the lines."
  :type '(choice (const nil) string)
  :group 'sendmail)

(defcustom mail-bury-selects-summary t
  "If non-nil, try to show Rmail summary buffer after returning from mail.
The functions \\[mail-send-on-exit] or \\[mail-dont-send] select
the Rmail summary buffer before returning, if it exists and this variable
is non-nil."
  :type 'boolean
  :group 'sendmail)

(defcustom mail-send-nonascii 'mime
  "Specify whether to allow sending non-ASCII characters in mail.
If t, that means do allow it.  nil means don't allow it.
`query' means ask the user each time.
`mime' means add an appropriate MIME header if none already present.
The default is `mime'.
Including non-ASCII characters in a mail message can be problematical
for the recipient, who may not know how to decode them properly."
  :type '(choice (const t) (const nil) (const query) (const mime))
  :group 'sendmail)

(defcustom mail-use-dsn nil
  "Ask MTA for notification of failed, delayed or successful delivery.
Note that only some MTAs (currently only recent versions of Sendmail)
support Delivery Status Notification."
  :group 'sendmail
  :type '(repeat (radio (const :tag "Failure" failure)
			(const :tag "Delay" delay)
			(const :tag "Success" success)))
  :version "22.1")

;; Note: could use /usr/ucb/mail instead of sendmail;
;; options -t, and -v if not interactive.
(defvar mail-mailer-swallows-blank-line nil
  "Set this non-nil if the system's mailer runs the header and body together.
The actual value should be an expression to evaluate that returns
non-nil if the problem will actually occur.
\(As far as we know, this is not an issue on any system still supported
by Emacs.)")

(put 'mail-mailer-swallows-blank-line 'risky-local-variable t) ; gets evalled
(make-obsolete-variable 'mail-mailer-swallows-blank-line
			"no need to set this on any modern system."
                        "24.1" 'set)

(defvar mail-mode-syntax-table
  ;; define-derived-mode will make it inherit from text-mode-syntax-table.
  (let ((st (make-syntax-table)))
    ;; FIXME this is probably very obsolete now ("percent hack").
    ;; sending.texi used to say:
    ;;   Mail mode defines the character `%' as a word separator; this
    ;;   is helpful for using the word commands to edit mail addresses.
    (modify-syntax-entry ?% ". " st)
    st)
  "Syntax table used while in `mail-mode'.")

(defvar mail-font-lock-keywords
  (eval-when-compile
    (let* ((cite-chars "[>|}]")
	   (cite-prefix "[:alpha:]")
	   (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
      (list '("^\\(To\\|Newsgroups\\):" . font-lock-function-name-face)
	    '("^\\(B?CC\\|Reply-to\\|Mail-\\(reply\\|followup\\)-to\\):" . font-lock-keyword-face)
	    '("^\\(Subject:\\)[ \t]*\\(.+\\)?"
	      (1 font-lock-comment-face)
;;	      (2 font-lock-type-face nil t)
	      )
	    ;; Use EVAL to delay in case `mail-header-separator' gets changed.
	    '(eval .
	      (let ((separator (if (zerop (length mail-header-separator))
				   " \\`\\' "
				 (regexp-quote mail-header-separator))))
		(cons (concat "^" separator "$") 'font-lock-warning-face)))
	    ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
	    `(,cite-chars
	      (,(concat "\\=[ \t]*"
			"\\(\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
			"\\(" cite-chars "[ \t]*\\)\\)+\\)"
			"\\(.*\\)")
	       (beginning-of-line) (end-of-line)
	       (1 font-lock-comment-delimiter-face nil t)
	       (5 font-lock-comment-face nil t)))
	    '("^\\(X-[A-Za-z0-9-]+\\|In-reply-to\\):.*\\(\n[ \t]+.*\\)*$"
	      . font-lock-string-face))))
  "Additional expressions to highlight in Mail mode.")


;;;###autoload
(defun sendmail-query-once ()
  "Query for `send-mail-function' and send mail with it.
This also saves the value of `send-mail-function' via Customize."
  ;; If send-mail-function is already setup, we're incorrectly called
  ;; a second time, probably because someone's using an old value
  ;; of send-mail-function.
  (when (eq send-mail-function 'sendmail-query-once)
    (sendmail-query-user-about-smtp))
  (funcall send-mail-function))

(defun sendmail-query-user-about-smtp ()
  (let* ((options `(("mail client" . mailclient-send-it)
		    ,@(when (and sendmail-program
				 (executable-find sendmail-program))
			'(("transport" . sendmail-send-it)))
		    ("smtp" . smtpmail-send-it)))
	 (choice
	  ;; Query the user.
	  (with-temp-buffer
	    (rename-buffer "*Emacs Mail Setup Help*" t)
	    (insert "\
 Emacs is about to send an email message, but it has not been
 configured for sending email.  To tell Emacs how to send email:

 - Type `"
		    (propertize "mail client" 'face 'bold)
		    "' to start your default email client and
   pass it the message text.\n\n")
	    (and sendmail-program
		 (executable-find sendmail-program)
		 (insert "\
 - Type `"
			 (propertize "transport" 'face 'bold)
			 "' to invoke the system's mail transport agent
   (the `"
			 sendmail-program
			 "' program).\n\n"))
	    (insert "\
 - Type `"
		    (propertize "smtp" 'face 'bold)
		    "' to send mail directly to an \"outgoing mail\" server.
   (Emacs may prompt you for SMTP settings).

 Emacs will record your selection and will use it thereafter.
 To change it later, customize the option `send-mail-function'.\n")
	    (goto-char (point-min))
	    (display-buffer (current-buffer))
	    (let ((completion-ignore-case t))
	      (completing-read "Send mail via: "
			       options nil 'require-match)))))
    (customize-save-variable 'send-mail-function
			     (cdr (assoc-string choice options t)))))

(defun sendmail-sync-aliases ()
  (when mail-personal-alias-file
    (let ((modtime (nth 5 (file-attributes mail-personal-alias-file))))
      (or (equal mail-alias-modtime modtime)
	  (setq mail-alias-modtime modtime
		mail-aliases t)))))


;;;###autoload
(define-mail-user-agent 'sendmail-user-agent
  'sendmail-user-agent-compose
  'mail-send-and-exit)

;;;###autoload
(defun sendmail-user-agent-compose (&optional to subject other-headers
				    continue switch-function yank-action
				    send-actions return-action
				    &rest ignored)
  (if switch-function
      (funcall switch-function "*mail*"))
  (let ((cc (cdr (assoc-string "cc" other-headers t)))
	(in-reply-to (cdr (assoc-string "in-reply-to" other-headers t)))
	(body (cdr (assoc-string "body" other-headers t))))
    (or (mail continue to subject in-reply-to cc yank-action
	      send-actions return-action)
	continue
	(error "Message aborted"))
    (save-excursion
      (rfc822-goto-eoh)
      (while other-headers
	(unless (member-ignore-case (car (car other-headers))
				    '("in-reply-to" "cc" "body"))
	  (insert (car (car other-headers)) ": "
		  (cdr (car other-headers))
		  (if use-hard-newlines hard-newline "\n")))
	(setq other-headers (cdr other-headers)))
      (when body
	(forward-line 1)
	(insert body))
      t)))

(defun mail-setup (to subject in-reply-to cc replybuffer
		   actions return-action)
  (or mail-default-reply-to
      (setq mail-default-reply-to (getenv "REPLYTO")))
  (sendmail-sync-aliases)
  (when (eq mail-aliases t)
    (setq mail-aliases nil)
    (and mail-personal-alias-file
	 (file-exists-p mail-personal-alias-file)
	 (build-mail-aliases)))
  ;; Don't leave this around from a previous message.
  (kill-local-variable 'buffer-file-coding-system)
  ;; This doesn't work for enable-multibyte-characters.
  ;; (kill-local-variable 'enable-multibyte-characters)
  (set-buffer-multibyte (default-value 'enable-multibyte-characters))
  (if current-input-method
      (inactivate-input-method))

  ;; Local variables for Mail mode.
  (setq mail-send-actions actions)
  (setq mail-reply-action replybuffer)
  (setq mail-return-action return-action)

  (goto-char (point-min))
  (if mail-setup-with-from
      (mail-insert-from-field))
  (insert "To: ")
  (save-excursion
    (if to
	;; Here removed code to extract names from within <...>
	;; on the assumption that mail-strip-quoted-names
	;; has been called and has done so.
	(let ((fill-prefix "\t")
	      (address-start (point)))
	  (insert to "\n")
	  (fill-region-as-paragraph address-start (point-max))
	  (goto-char (point-max))
	  (unless (bolp)
	    (newline)))
      (newline))
    (if cc
	(let ((fill-prefix "\t")
	      (address-start (progn (insert "CC: ") (point))))
	  (insert cc "\n")
	  (fill-region-as-paragraph address-start (point-max))
	  (goto-char (point-max))
	  (unless (bolp)
	    (newline))))
    (if in-reply-to
	(let ((fill-prefix "\t")
	      (fill-column 78)
	      (address-start (point)))
	  (insert "In-reply-to: " in-reply-to "\n")
	  (fill-region-as-paragraph address-start (point-max))
	  (goto-char (point-max))
	  (unless (bolp)
	    (newline))))
    (insert "Subject: " (or subject "") "\n")
    (if mail-default-headers
	(insert mail-default-headers))
    (if mail-default-reply-to
	(insert "Reply-to: " mail-default-reply-to "\n"))
    (if mail-self-blind
	(insert "BCC: " user-mail-address "\n"))
    (if mail-archive-file-name
	(insert "FCC: " mail-archive-file-name "\n"))
    (put-text-property (point)
		       (progn
			 (insert mail-header-separator "\n")
			 (1- (point)))
		       'category 'mail-header-separator)
    ;; Insert the signature.  But remember the beginning of the message.
    (if to (setq to (point)))
    (if mail-signature (mail-signature t))
    (goto-char (point-max))
    (or (bolp) (newline)))
  (if to (goto-char to))
  (or to subject in-reply-to
      (set-buffer-modified-p nil))
  (run-hooks 'mail-setup-hook))

(defcustom mail-mode-hook nil
  "Hook run by Mail mode.
When composing a mail, this runs immediately after creating, or
switching to, the `*mail*' buffer.  See also `mail-setup-hook'."
  :group 'sendmail
  :type 'hook
  :options '(footnote-mode))

(defvar mail-mode-abbrev-table text-mode-abbrev-table)
(defvar mail-encode-mml)
;;;###autoload
(define-derived-mode mail-mode text-mode "Mail"
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:

\\[mail-send]  mail-send (send the message)
\\[mail-send-and-exit]  mail-send-and-exit (send the message and exit)

Here are commands that move to a header field (and create it if there isn't):
	 \\[mail-to]  move to To:	\\[mail-subject]  move to Subj:
	 \\[mail-bcc]  move to BCC:	\\[mail-cc]  move to CC:
	 \\[mail-fcc]  move to FCC:	\\[mail-reply-to] move to Reply-To:
         \\[mail-mail-reply-to]  move to Mail-Reply-To:
         \\[mail-mail-followup-to] move to Mail-Followup-To:
\\[mail-text]  move to message text.
\\[mail-signature]  mail-signature (insert `mail-signature-file' file).
\\[mail-yank-original]  mail-yank-original (insert current message, in Rmail).
\\[mail-fill-yanked-message]  mail-fill-yanked-message (fill what was yanked).
\\[mail-insert-file] insert a text file into the message.
\\[mail-add-attachment] attach to the message a file as binary attachment.
Turning on Mail mode runs the normal hooks `text-mode-hook' and
`mail-mode-hook' (in that order)."
  (make-local-variable 'mail-reply-action)
  (make-local-variable 'mail-send-actions)
  (make-local-variable 'mail-return-action)
  (make-local-variable 'mail-encode-mml)
  (setq mail-encode-mml nil)
  (setq buffer-offer-save t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mail-font-lock-keywords t t))
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'mail-mode-auto-fill)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'mail-mode-fill-paragraph)
  ;; Allow using comment commands to add/remove quoting (this only does
  ;; anything if mail-yank-prefix is set to a non-nil value).
  (set (make-local-variable 'comment-start) mail-yank-prefix)
  (if mail-yank-prefix
      (set (make-local-variable 'comment-start-skip)
	   (concat "^" (regexp-quote mail-yank-prefix) "[ \t]*")))
  (make-local-variable 'adaptive-fill-regexp)
  ;; Also update the paragraph-separate entry if you change this.
  (setq adaptive-fill-regexp
	(concat "[ \t]*[-[:alnum:]]+>+[ \t]*\\|"
		adaptive-fill-regexp))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (setq adaptive-fill-first-line-regexp
	(concat "[ \t]*[-[:alnum:]]*>+[ \t]*\\|"
		adaptive-fill-first-line-regexp))
  (add-hook 'completion-at-point-functions #'mail-completion-at-point-function
            nil 'local)
  ;; `-- ' precedes the signature.  `-----' appears at the start of the
  ;; lines that delimit forwarded messages.
  ;; Lines containing just >= 3 dashes, perhaps after whitespace,
  ;; are also sometimes used and should be separators.
  (setq paragraph-separate
	(concat (regexp-quote mail-header-separator)
		;; This is based on adaptive-fill-regexp (presumably
		;; the idea is to allow navigation etc of cited paragraphs).
		"$\\|\t*[-–!|#%;>*·•‣⁃◦ ]+$"
		"\\|[ \t]*[-[:alnum:]]*>+[ \t]*$\\|[ \t]*$\\|"
		"--\\( \\|-+\\)$\\|"
		page-delimiter)))


(defun mail-header-end ()
  "Return the buffer location of the end of headers, as a number."
  (save-restriction
    (widen)
    (save-excursion
      (rfc822-goto-eoh)
      (point))))

(defun mail-text-start ()
  "Return the buffer location of the start of text, as a number."
  (save-restriction
    (widen)
    (save-excursion
      (rfc822-goto-eoh)
      (forward-line 1)
      (point))))

(defun mail-sendmail-delimit-header ()
  "Set up whatever header delimiter convention sendmail will use.
Concretely: replace the first blank line in the header with the separator."
  (rfc822-goto-eoh)
  (insert mail-header-separator)
  (point))

(defun mail-sendmail-undelimit-header ()
  "Remove header separator to put the message in correct form for sendmail.
Leave point at the start of the delimiter line."
  (rfc822-goto-eoh)
  (delete-region (point) (progn (end-of-line) (point))))

(defun mail-mode-auto-fill ()
  "Carry out Auto Fill for Mail mode.
If within the headers, this makes the new lines into continuation lines."
  (if (< (point) (mail-header-end))
      (let ((old-line-start (line-beginning-position)))
	(if (do-auto-fill)
	    (save-excursion
	      (beginning-of-line)
	      (while (not (eq (point) old-line-start))
		;; Use insert-before-markers in case we're inserting
		;; before the saved value of point (which is common).
		(insert-before-markers "   ")
		(forward-line -1))
	      t)))
    (do-auto-fill)))

(defun mail-mode-fill-paragraph (arg)
  ;; Do something special only if within the headers.
  (if (< (point) (mail-header-end))
      (let (beg end fieldname)
	(when (prog1 (re-search-backward "^[-a-zA-Z]+:" nil 'yes)
		(setq beg (point)))
	(setq fieldname
		(downcase (buffer-substring beg (1- (match-end 0))))))
	(forward-line 1)
	;; Find continuation lines and get rid of their continuation markers.
	(while (looking-at "[ \t]")
	  (delete-horizontal-space)
	  (forward-line 1))
	(setq end (point-marker))
	(goto-char beg)
	;; If this field contains addresses,
	;; make sure we can fill after each address.
	(if (member fieldname
		    '("to" "cc" "bcc" "from" "reply-to"
		      "mail-reply-to" "mail-followup-to"
		      "resent-to" "resent-cc" "resent-bcc"
		      "resent-from" "resent-reply-to"))
	    (while (search-forward "," end t)
	      (or (looking-at "[ \t]")
		  (insert " "))))
	(fill-region-as-paragraph beg end arg)
	;; Mark all lines except the first as continuations.
	(goto-char beg)
	(forward-line 1)
	(while (< (point) end)
	  (insert "  ")
	  (forward-line 1))
	(move-marker end nil)
	t)))

;; User-level commands for sending.

(defun mail-send-and-exit (&optional arg)
  "Send message like `mail-send', then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-send)
  (mail-bury arg))

(defun mail-dont-send (&optional arg)
  "Don't send the message you have been editing.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-bury arg))

(defun mail-bury (&optional arg)
  "Bury this mail buffer."
  (let ((newbuf (other-buffer (current-buffer)))
	(return-action mail-return-action)
	some-rmail)
    (bury-buffer (current-buffer))
    ;; If there is an Rmail buffer, return to it nicely
    ;; even if this message was not started by an Rmail command.
    (unless return-action
      (dolist (buffer (buffer-list))
	(if (and (eq (buffer-local-value 'major-mode buffer) 'rmail-mode)
		 (null return-action)
		 ;; Don't match message-viewer buffer.
		 (not (string-match "\\` " (buffer-name buffer))))
	    (setq return-action `(rmail-mail-return ,buffer)))))
    (if (and (null arg) return-action)
	(apply (car return-action) (cdr return-action))
      (switch-to-buffer newbuf))))

(defcustom mail-send-hook nil
  "Hook run just before sending a message."
  :type 'hook
  :options '(flyspell-mode-off)
  :group 'sendmail)

;;;###autoload
(defcustom mail-mailing-lists nil
"List of mailing list addresses the user is subscribed to.
The variable is used to trigger insertion of the \"Mail-Followup-To\"
header when sending a message to a mailing list."
  :type '(repeat string)
  :group 'sendmail)

(declare-function mml-to-mime "mml" ())

(defun mail-send ()
  "Send the message in the current buffer.
If `mail-interactive' is non-nil, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  (interactive)
  (if (if buffer-file-name
	  (y-or-n-p "Send buffer contents as mail message? ")
	(or (buffer-modified-p)
	    (y-or-n-p "Message already sent; resend? ")))
      (let ((inhibit-read-only t)
	    (opoint (point))
	    (ml (when mail-mailing-lists
                ;; The surrounding regexp assumes the use of
                ;; `mail-strip-quoted-names' on addresses before matching
                ;; Cannot deal with full RFC 822 freedom, but that is
                ;; unlikely to be problematic.
                (concat "\\(?:[[:space:];,]\\|\\`\\)"
                        (regexp-opt mail-mailing-lists t)
                        "\\(?:[[:space:];,]\\|\\'\\)"))))
	;; If there are mailing lists defined
	(when ml
	  (save-excursion
	    (let* ((to (mail-fetch-field "to" nil t))
		   (cc (mail-fetch-field "cc" nil t))
		   (new-header-values	; To: and Cc:
		    (mail-strip-quoted-names
		     (concat to (when cc (concat ", " cc))))))
	      ;; If message goes to known mailing list ...
	      (when (string-match ml new-header-values)
		;; Add Mail-Followup-To if none yet
		(unless (mail-fetch-field "mail-followup-to")
		  (goto-char (mail-header-end))
		  (insert "Mail-Followup-To: "
			  (let ((l))
			    (mapc
			     ;; remove duplicates
			     (lambda (e)
                               (unless (member e l)
                                 (push e l)))
			     (split-string new-header-values
					   ",[[:space:]]+" t))
			    (mapconcat 'identity l ", "))
			  "\n"))
		;; Add Mail-Reply-To if none yet
		(unless (mail-fetch-field "mail-reply-to")
		  (goto-char (mail-header-end))
		  (insert "Mail-Reply-To: "
			  (or (mail-fetch-field "reply-to")
			      user-mail-address)
			  "\n"))))))
	(unless (memq mail-send-nonascii '(t mime))
	  (goto-char (point-min))
	  (skip-chars-forward "\0-\177")
	  (or (= (point) (point-max))
	      (if (eq mail-send-nonascii 'query)
		  (or (y-or-n-p "Message contains non-ASCII characters; send anyway? ")
		      (error "Aborted"))
		(error "Message contains non-ASCII characters"))))
	;; Complain about any invalid line.
	(goto-char (point-min))
	(re-search-forward (regexp-quote mail-header-separator) (point-max) t)
	(let ((header-end (or (match-beginning 0) (point-max))))
	  (goto-char (point-min))
	  (while (< (point) header-end)
	    (unless (looking-at "[ \t]\\|.*:\\|$")
	      (push-mark opoint)
	      (error "Invalid header line (maybe a continuation line lacks initial whitespace)"))
	    (forward-line 1)))
	(goto-char opoint)
	(when mail-encode-mml
	  (mml-to-mime)
	  (setq mail-encode-mml nil))
	(run-hooks 'mail-send-hook)
	(message "Sending...")
	(funcall send-mail-function)
	;; Now perform actions on successful sending.
	(while mail-send-actions
	  (condition-case nil
	      (apply (car (car mail-send-actions))
		     (cdr (car mail-send-actions)))
	    (error))
	  (setq mail-send-actions (cdr mail-send-actions)))
	(message "Sending...done")
	;; If buffer has no file, mark it as unmodified and delete auto-save.
	(if (not buffer-file-name)
	    (progn
	      (set-buffer-modified-p nil)
	      (delete-auto-save-file-if-necessary t))))))

(defun mail-envelope-from ()
  "Return the envelope mail address to use when sending mail.
This function uses `mail-envelope-from'."
  (if (eq mail-envelope-from 'header)
      (nth 1 (mail-extract-address-components
 	      (mail-fetch-field "From")))
    mail-envelope-from))

;; This does the real work of sending a message via sendmail.
;; It is called via the variable send-mail-function.

;;;###autoload
(defvar sendmail-coding-system nil
  "*Coding system for encoding the outgoing mail.
This has higher priority than the default `buffer-file-coding-system'
and `default-sendmail-coding-system',
but lower priority than the local value of `buffer-file-coding-system'.
See also the function `select-message-coding-system'.")

;;;###autoload
(defvar default-sendmail-coding-system 'iso-latin-1
  "Default coding system for encoding the outgoing mail.
This variable is used only when `sendmail-coding-system' is nil.

This variable is set/changed by the command `set-language-environment'.
User should not set this variable manually,
instead use `sendmail-coding-system' to get a constant encoding
of outgoing mails regardless of the current language environment.
See also the function `select-message-coding-system'.")

(defun mail-insert-from-field ()
  "Insert the \"From:\" field of a mail header.
The style of the field is determined by the variable `mail-from-style'.
This function does not perform RFC2047 encoding."
  (let* ((login user-mail-address)
	 (fullname (user-full-name))
	 (quote-fullname nil))
    (if (string-match "[^\0-\177]" fullname)
	(setq quote-fullname t))
    (cond ((null mail-from-style)
	   (insert "From: " login "\n"))
	  ;; This is deprecated.
	  ((eq mail-from-style 'system-default)
	   nil)
	  ((or (eq mail-from-style 'angles)
	       (and (not (eq mail-from-style 'parens))
		    ;; Use angles if no quoting is needed, or if
		    ;; parens would need quoting too.
		    (or (not (string-match "[^- !#-'*+/-9=?A-Z^-~]" fullname))
			(let ((tmp (concat fullname nil)))
			  (while (string-match "([^()]*)" tmp)
			    (aset tmp (match-beginning 0) ?-)
			    (aset tmp (1- (match-end 0)) ?-))
			  (string-match "[\\()]" tmp)))))
	   (insert "From: " fullname)
	   (let ((fullname-start (+ (point-min) 6))
		 (fullname-end (point-marker)))
	     (goto-char fullname-start)
	     ;; Look for a character that cannot appear unquoted
	     ;; according to RFC 822.
	     (if (or (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
					fullname-end 1)
		     quote-fullname)
		 (progn
		   ;; Quote fullname, escaping specials.
		   (goto-char fullname-start)
		   (insert "\"")
		   (while (re-search-forward "[\"\\]"
					     fullname-end 1)
		     (replace-match "\\\\\\&" t))
		   (insert "\""))))
	   (insert " <" login ">\n"))
	  ;; 'parens or default
	  (t
	   (insert "From: " login " (")
	   (let ((fullname-start (point)))
	     (if quote-fullname
		 (insert "\""))
	     (insert fullname)
	     (if quote-fullname
		 (insert "\""))
	     (let ((fullname-end (point-marker)))
	       (goto-char fullname-start)
	       ;; RFC 822 says \ and nonmatching parentheses
	       ;; must be escaped in comments.
	       ;; Escape every instance of ()\ ...
	       (while (re-search-forward "[()\\]" fullname-end 1)
		 (replace-match "\\\\\\&" t))
	       ;; ... then undo escaping of matching parentheses,
	       ;; including matching nested parentheses.
	       (goto-char fullname-start)
	       (while (re-search-forward
		       "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
		       fullname-end 1)
		 (replace-match "\\1(\\3)" t)
		 (goto-char fullname-start))))
	   (insert ")\n")))))

(defun mail-encode-header (beg end)
  "Encode the mail header between BEG and END according to RFC2047.
Return non-nil if and only if some part of the header is encoded."
  (save-restriction
    (narrow-to-region beg end)
    (let* ((selected (select-message-coding-system))
	   (mm-coding-system-priorities
	    (if (and selected (coding-system-get selected :mime-charset))
		(cons selected mm-coding-system-priorities)
	      mm-coding-system-priorities))
	   (tick (buffer-chars-modified-tick))
	   ;; Many mailers, including Gnus, passes a message of which
	   ;; the header is already encoded, so this is necessary to
	   ;; prevent it from being encoded again.
	   (rfc2047-encode-encoded-words nil))
      (rfc2047-encode-message-header)
      (= tick (buffer-chars-modified-tick)))))

;; Normally you will not need to modify these options unless you are
;; using some non-genuine substitute for sendmail which does not
;; implement each and every option that the original supports.
;; E.g., ssmtp does not support "-odb", so, if your site uses it,
;; you will need to modify `sendmail-error-reporting-non-interactive'
;; in your site-init.el.
(defvar sendmail-error-reporting-interactive
  ;; These mean "report errors to terminal" and "deliver interactively"
  '("-oep" "-odi"))
(defvar sendmail-error-reporting-non-interactive
  ;; These mean "report errors by mail" and "deliver in background".
  '("-oem" "-odb"))

(defun sendmail-send-it ()
  "Send the current mail buffer using the Sendmail package.
This is a suitable value for `send-mail-function'.  It sends using the
external program defined by `sendmail-program'."
  (require 'mail-utils)
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " sendmail errors")
		  0))
	(tembuf (generate-new-buffer " sendmail temp"))
	(multibyte enable-multibyte-characters)
	(case-fold-search nil)
	(selected-coding (select-message-coding-system))
	resend-to-addresses
	delimline
	fcc-was-found
	(mailbuf (current-buffer))
	;; Examine these variables now, so that
	;; local binding in the mail buffer will take effect.
	(envelope-from
	 (and mail-specify-envelope-from
	      (or (mail-envelope-from) user-mail-address))))
    (unwind-protect
	(with-current-buffer tembuf
	  (erase-buffer)
	  (unless multibyte
	    (set-buffer-multibyte nil))
	  (insert-buffer-substring mailbuf)
	  (set-buffer-file-coding-system selected-coding)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (mail-header-end))
	  (delete-region (point) (progn (end-of-line) (point)))
	  (setq delimline (point-marker))
	  (sendmail-sync-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; Ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (goto-char (point-min))
	  ;; Look for Resent- headers.  They require sending
	  ;; the message specially.
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (while (re-search-forward "^Resent-\\(to\\|cc\\|bcc\\):" delimline t)
	      ;; Put a list of such addresses in resend-to-addresses.
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (forward-line 1)
					  (while (looking-at "^[ \t]")
					    (forward-line 1))
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses)))
	      ;; Delete Resent-BCC ourselves
	      (if (save-excursion (beginning-of-line)
				  (looking-at "resent-bcc"))
		  (delete-region (line-beginning-position)
				 (line-beginning-position 2))))
            ;; Apparently this causes a duplicate Sender.
	    ;; ;; If the From is different than current user, insert Sender.
	    ;; (goto-char (point-min))
	    ;; (and (re-search-forward "^From:"  delimline t)
	    ;;      (progn
	    ;;        (require 'mail-utils)
	    ;;        (not (string-equal
	    ;;     	 (mail-strip-quoted-names
	    ;;     	  (save-restriction
	    ;;     	    (narrow-to-region (point-min) delimline)
	    ;;     	    (mail-fetch-field "From")))
	    ;;     	 (user-login-name))))
	    ;;      (progn
	    ;;        (forward-line 1)
	    ;;        (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match "")
	      ;; This one matches a Subject just before the header delimiter.
	      (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
		       (= (match-end 0) delimline))
		  (replace-match "")))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(mail-insert-from-field))
	    ;; Possibly add a MIME header for the current coding system
	    (let (charset where-content-type)
	      (goto-char (point-min))
	      (setq where-content-type
		    (re-search-forward "^Content-type:" delimline t))
	      (goto-char (point-min))
	      (and (eq mail-send-nonascii 'mime)
		   (not (re-search-forward "^MIME-version:" delimline t))
		   (progn (skip-chars-forward "\0-\177")
			  (/= (point) (point-max)))
		   selected-coding
		   (setq charset
			 (coding-system-get selected-coding :mime-charset))
		   (progn
		     (goto-char delimline)
		     (insert "MIME-version: 1.0\n"
			     "Content-type: text/plain; charset="
			     (symbol-name charset)
			     "\nContent-Transfer-Encoding: 8bit\n")
		   ;; The character set we will actually use
		   ;; should override any specified in the message itself.
		     (when where-content-type
		       (goto-char where-content-type)
		       (delete-region (point-at-bol)
				      (progn (forward-line 1) (point)))))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(progn
		  (setq fcc-was-found t)
		  (mail-do-fcc delimline)))
	    (if mail-interactive
		(with-current-buffer errbuf
		  (erase-buffer))))
	  ;; Encode the header according to RFC2047.
	  (mail-encode-header (point-min) delimline)
	  (goto-char (point-min))
	  (if (let ((case-fold-search t))
		(or resend-to-addresses
		    (re-search-forward "^To:\\|^cc:\\|^bcc:"
				       delimline t)))
	      (let* ((default-directory "/")
		     (coding-system-for-write selected-coding)
		     (args
		      (append (list (point-min) (point-max)
				    sendmail-program
				    nil errbuf nil "-oi")
			      (and envelope-from
				   (list "-f" envelope-from))
			      ;; ;; Don't say "from root" if running under su.
			      ;; (and (equal (user-real-login-name) "root")
			      ;;      (list "-f" (user-login-name)))
			      (and mail-alias-file
				   (list (concat "-oA" mail-alias-file)))
			      (if mail-interactive
                                  sendmail-error-reporting-interactive
                                  sendmail-error-reporting-non-interactive)
			      ;; Get the addresses from the message
			      ;; unless this is a resend.
			      ;; We must not do that for a resend
			      ;; because we would find the original addresses.
			      ;; For a resend, include the specific addresses.
			      (or resend-to-addresses
				  '("-t")
				  )
			      (if mail-use-dsn
				  (list "-N" (mapconcat 'symbol-name
							mail-use-dsn ",")))
			      )
		      )
		     (exit-value (apply 'call-process-region args)))
		(cond ((or (null exit-value) (eq 0 exit-value)))
		      ((numberp exit-value)
		       (error "Sending...failed with exit value %d" exit-value))
		      ((stringp exit-value)
		       (error "Sending...terminated by signal: %s" exit-value))
		      (t
		       (error "SENDMAIL-SEND-IT -- fall through: %S" exit-value))))
	    (or fcc-was-found
		(error "No recipients")))
	  (if mail-interactive
	      (with-current-buffer errbuf
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

(autoload 'rmail-output-to-rmail-buffer "rmailout")

(defun mail-do-fcc (header-end)
  "Find and act on any FCC: headers in the current message before HEADER-END.
If a buffer is visiting the FCC file, append to it before
offering to save it, if it was modified initially.  If this is an
Rmail buffer, update Rmail as needed.  If there is no buffer,
just append to the file, in Babyl format if necessary."
  (unless (markerp header-end)
    (error "Value of `header-end' must be a marker"))
  (let (fcc-list
	(mailbuf (current-buffer))
	(time (current-time)))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^FCC:[ \t]*" header-end t)
	  (push (buffer-substring (point)
				  (progn
				    (end-of-line)
				    (skip-chars-backward " \t")
				    (point)))
		fcc-list)
	  (delete-region (match-beginning 0)
			 (progn (forward-line 1) (point)))))
      (with-temp-buffer
	;; This initial newline is not written out if we create a new
	;; file (see below).
	(insert "\nFrom " (user-login-name) " " (current-time-string time) "\n")
	;; Insert the time zone before the year.
	(forward-char -1)
	(forward-word -1)
	(require 'mail-utils)
	(insert (mail-rfc822-time-zone time) " ")
	(goto-char (point-max))
	(insert-buffer-substring mailbuf)
	;; Make sure messages are separated.
	(goto-char (point-max))
	(insert ?\n)
	(goto-char 2)
	;; ``Quote'' "^From " as ">From "
	;;  (note that this isn't really quoting, as there is no requirement
	;;   that "^[>]+From " be quoted in the same transparent way.)
	(let ((case-fold-search nil))
	  (while (search-forward "\nFrom " nil t)
	    (forward-char -5)
	    (insert ?>)))
	(dolist (fcc fcc-list)
	  (let* ((buffer (find-buffer-visiting fcc))
		 (curbuf (current-buffer))
		 dont-write-the-file
		 buffer-matches-file
		 (beg (point-min))	; the initial blank line
		 (end (point-max))
		 ;; After the ^From line.
		 (beg2 (save-excursion (goto-char (point-min))
				       (forward-line 2) (point))))
	    (if buffer
		;; File is present in a buffer => append to that buffer.
		(with-current-buffer buffer
		  (setq buffer-matches-file
			(and (not (buffer-modified-p))
			     (verify-visited-file-modtime buffer)))
		  (let ((msg (bound-and-true-p rmail-current-message))
			(buffer-read-only nil))
		    ;; If MSG is non-nil, buffer is in Rmail mode.
		    (if msg
			(let ((buff (generate-new-buffer " *mail-do-fcc")))
			  (unwind-protect
			      (progn
				(with-current-buffer buff
				  (insert-buffer-substring curbuf (1+ beg) end))
				(rmail-output-to-rmail-buffer buff msg))
			    (kill-buffer buff)))
		      ;; Output file not in Rmail mode => just insert
		      ;; at the end.
		      (save-restriction
			(widen)
			(goto-char (point-max))
			(insert-buffer-substring curbuf beg end)))
		    ;; Offer to save the buffer if it was modified
		    ;; before we started.
		    (unless buffer-matches-file
		      (if (y-or-n-p (format "Save file %s? " fcc))
			  (save-buffer))
		      (setq dont-write-the-file t)))))
	    ;; Append to the file directly, unless we've already taken
	    ;; care of it.
	    (unless dont-write-the-file
	      (if (and (file-exists-p fcc)
		       (mail-file-babyl-p fcc))
		  ;; If the file is a Babyl file, convert the message to
		  ;; Babyl format.  Even though Rmail no longer uses
		  ;; Babyl, this code can remain for the time being, on
		  ;; the off-chance one FCCs to a Babyl file that has
		  ;; not yet been converted to mbox.
		  (let ((coding-system-for-write
			 (or rmail-file-coding-system 'emacs-mule)))
		    (with-temp-buffer
		      (insert "\C-l\n0, unseen,,\n*** EOOH ***\nDate: "
			      (mail-rfc822-date) "\n")
		      (insert-buffer-substring curbuf beg2 end)
		      (insert "\n\C-_")
		      (write-region (point-min) (point-max) fcc t)))
		;; Ensure there is a blank line between messages, but
		;; not at the very start of the file.
		(write-region (if (file-exists-p fcc)
				  (point-min)
				(1+ (point-min)))
			      (point-max) fcc t)))
	    (and buffer (not dont-write-the-file)
		 (with-current-buffer buffer
		   (set-visited-file-modtime)))))))))

(defun mail-sent-via ()
  "Make a Sent-via header line from each To or CC header line."
  (interactive)
  (save-excursion
    ;; put a marker at the end of the header
    (let ((end (copy-marker (mail-header-end)))
	  (case-fold-search t))
      (goto-char (point-min))
      ;; search for the To: lines and make Sent-via: lines from them
      ;; search for the next To: line
      (while (re-search-forward "^\\(to\\|cc\\):" end t)
	;; Grab this line plus all its continuations, sans the `to:'.
	(let ((to-line
	       (buffer-substring (point)
				 (progn
				   (if (re-search-forward "^[^ \t\n]" end t)
				       (backward-char 1)
				     (goto-char end))
				   (point)))))
	  ;; Insert a copy, with altered header field name.
	  (insert-before-markers "Sent-via:" to-line))))))

(make-obsolete 'mail-sent-via "nobody can remember what it is for." "24.1")


(defun mail-to ()
  "Move point to end of To field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "To"))

(defun mail-subject ()
  "Move point to end of Subject field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Subject"))

(defun mail-cc ()
  "Move point to end of CC field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "cc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nCC: "))))

(defun mail-bcc ()
  "Move point to end of BCC field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "bcc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nBCC: "))))

(defun mail-fcc (folder)
  "Add a new FCC field, with file name completion."
  (interactive "FFolder carbon copy: ")
  (expand-abbrev)
  (or (mail-position-on-field "fcc" t)	;Put new field after exiting FCC.
      (mail-position-on-field "to"))
  (insert "\nFCC: " folder))

(defun mail-reply-to ()
  "Move point to end of Reply-To field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Reply-To"))

(defun mail-mail-reply-to ()
  "Move point to end of Mail-Reply-To field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "mail-reply-to" t)
      (progn (mail-position-on-field "to")
           (insert "\nMail-Reply-To: "))))

(defun mail-mail-followup-to ()
  "Move point to end of Mail-Followup-To field, creating it if necessary."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "mail-followup-to" t)
      (progn (mail-position-on-field "to")
           (insert "\nMail-Followup-To: "))))

(defun mail-position-on-field (field &optional soft)
  (let (end
	(case-fold-search t))
    (setq end (mail-header-end))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote field) ":") end t)
	(progn
	  (re-search-forward "^[^ \t]" nil 'move)
	  (beginning-of-line)
	  (skip-chars-backward "\n")
	  t)
      (or soft
	  (progn (goto-char end)
		 (insert field ": \n")
		 (skip-chars-backward "\n")))
      nil)))

(defun mail-text ()
  "Move point to beginning of text field."
  (interactive)
  (expand-abbrev)
  (goto-char (mail-text-start)))

(defun mail-signature (&optional atpoint)
  "Sign letter with signature.
If the variable `mail-signature' is a string, inserts it.
If it is t or nil, inserts the contents of the file `mail-signature-file'.
Otherwise, evals `mail-signature'.
Prefix argument ATPOINT means insert at point rather than the end."
  (interactive "*P")
  ;; Test for an unreadable file here, before we delete trailing
  ;; whitespace, so that we don't modify the buffer needlessly.
  (if (and (memq mail-signature '(t nil))
	   (not (file-readable-p mail-signature-file)))
      (if (called-interactively-p 'interactive)
	  (message "The signature file `%s' could not be read"
		   mail-signature-file))
    (save-excursion
      (unless atpoint
	(goto-char (point-max))
	;; Delete trailing whitespace and blank lines.
	(skip-chars-backward " \t\n")
	(end-of-line)
	(delete-region (point) (point-max)))
      (cond ((stringp mail-signature)
	     (insert mail-signature))
	    ((memq mail-signature '(t nil))
	     (insert "\n\n-- \n")
	     (insert-file-contents (expand-file-name mail-signature-file)))
	    (t
	     ;; FIXME add condition-case error handling?
	     (eval mail-signature))))))

(defun mail-fill-yanked-message (&optional justifyp)
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  (interactive "P")
  (save-excursion
    (goto-char (mail-text-start))
    (fill-individual-paragraphs (point)
				(point-max)
				justifyp
				mail-citation-prefix-regexp)))

(defun mail-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  (mail-yank-clear-headers (region-beginning) (region-end))
  (if (null mail-yank-prefix)
      (indent-rigidly (region-beginning) (region-end)
		      mail-indentation-spaces)
    (save-excursion
      (let ((end (set-marker (make-marker) (region-end))))
	(goto-char (region-beginning))
	(while (< (point) end)
	  (insert mail-yank-prefix)
	  (forward-line 1))))))

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in Rmail).
Puts point after the text and mark before.
Normally, indents each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line.

Just \\[universal-argument] as argument means don't indent, insert no prefix,
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-action
      (let ((start (point))
	    (original mail-reply-action)
	    (omark (mark t)))
	(and (consp original) (eq (car original) 'insert-buffer)
	     (setq original (nth 1 original)))
	(if (consp original)
	    (progn
	      ;; Call yank function, and set the mark if it doesn't.
	      (apply (car original) (cdr original))
	      (if (eq omark (mark t))
		  (push-mark (point))))
	  ;; If the original message is in another window in the same
	  ;; frame, delete that window to save space.
	  (delete-windows-on original t)
	  (with-no-warnings
	    ;; We really want this to set mark.
	    (insert-buffer original)
	    ;; If they yank the original text, the encoding of the
	    ;; original message is a better default than
	    ;; the default buffer-file-coding-system.
	    (and (coding-system-equal
		  (default-value 'buffer-file-coding-system)
		  buffer-file-coding-system)
		 (setq buffer-file-coding-system
		       (coding-system-change-text-conversion
			buffer-file-coding-system
			(coding-system-base
			 (with-current-buffer original
			   buffer-file-coding-system))))))
	  (set-text-properties (point) (mark t) nil))
	(if (consp arg)
	    nil
	  (goto-char start)
	  (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
					   mail-indentation-spaces))
		;; Avoid error in Transient Mark mode
		;; on account of mark's being inactive.
		(mark-even-if-inactive t))
	    (cond (mail-citation-hook
		   ;; Bind mail-citation-header to the inserted
		   ;; message's header.
		   (let ((mail-citation-header
			  (buffer-substring-no-properties
			   start
			   (save-excursion
			     (save-restriction
			       (narrow-to-region start (point-max))
			       (goto-char start)
			       (rfc822-goto-eoh)
			       (point))))))
		     (run-hooks 'mail-citation-hook)))
		  (mail-yank-hooks
		   (run-hooks 'mail-yank-hooks))
		  (t
		   (mail-indent-citation)))))
	;; This is like exchange-point-and-mark, but doesn't activate the mark.
	;; It is cleaner to avoid activation, even though the command
	;; loop would deactivate the mark because we inserted text.
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))
	(if (not (eolp)) (insert ?\n)))))

(defun mail-yank-clear-headers (start end)
  (if (< end start)
      (let (temp)
	(setq temp start start end end temp)))
  (if mail-yank-ignored-headers
      (save-excursion
	(goto-char start)
	(if (search-forward "\n\n" end t)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char start)
	      (while (let ((case-fold-search t))
		       (re-search-forward mail-yank-ignored-headers nil t))
		(beginning-of-line)
		(delete-region (point)
			       (progn (re-search-forward "\n[^ \t]")
				      (forward-char -1)
				      (point)))))))))

(defun mail-yank-region (arg)
  "Insert the selected region from the message being replied to.
Puts point after the text and mark before.
Normally, indents each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line.

Just \\[universal-argument] as argument means don't indent, insert no prefix,
and don't delete any header fields."
  (interactive "P")
  (and (consp mail-reply-action)
       (memq (car mail-reply-action)
	     '(rmail-yank-current-message insert-buffer))
       (with-current-buffer (nth 1 mail-reply-action)
	 (or (mark t)
	     (error "No mark set: %S" (current-buffer))))
       (let ((buffer (nth 1 mail-reply-action))
	     (start (point))
	     ;; Avoid error in Transient Mark mode
	     ;; on account of mark's being inactive.
	     (mark-even-if-inactive t))
	 ;; Insert the citation text.
	 (insert (with-current-buffer buffer
		   (buffer-substring-no-properties (point) (mark))))
	 (push-mark start)
	 ;; Indent or otherwise annotate the citation text.
	 (if (consp arg)
	     nil
	   (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
					    mail-indentation-spaces)))
	     (if mail-citation-hook
		 ;; Bind mail-citation-header to the original message's header.
		 (let ((mail-citation-header
			(with-current-buffer buffer
			  (buffer-substring-no-properties
			   (point-min)
			   (save-excursion
			     (goto-char (point-min))
			     (rfc822-goto-eoh)
			     (point))))))
		   (run-hooks 'mail-citation-hook))
	       (if mail-yank-hooks
		   (run-hooks 'mail-yank-hooks)
		 (mail-indent-citation))))))))

(defun mail-split-line ()
  "Split current line, moving portion beyond point vertically down.
If the current line has `mail-yank-prefix', insert it on the new line."
  (interactive "*")
  (split-line mail-yank-prefix))


(defun mail-insert-file (&optional file)
  "Insert a file at the end of the buffer, with separator lines around it."
  (interactive "fAttach file: ")
  (save-excursion
    (goto-char (point-max))
    (or (bolp) (newline))
    (newline)
    (let ((start (point))
	  middle)
      (insert (format "===File %s===" file))
      (insert-char ?= (max 0 (- 60 (current-column))))
      (newline)
      (setq middle (point))
      (insert "============================================================\n")
      (push-mark)
      (goto-char middle)
      (insert-file-contents file)
      (or (bolp) (newline))
      (goto-char start))))

(define-obsolete-function-alias 'mail-attach-file 'mail-insert-file "24.1")

(declare-function mml-attach-file "mml"
		  (file &optional type description disposition))
(declare-function mm-default-file-encoding "mm-encode" (file))

(defun mail-add-attachment (file)
  "Add FILE as a MIME attachment to the end of the mail message being composed."
  (interactive "fAttach file: ")
  (mml-attach-file file
		   (or (mm-default-file-encoding file)
		       "application/octet-stream") nil)
  (setq mail-encode-mml t))


;; Put these commands last, to reduce chance of lossage from quitting
;; in middle of loading the file.

;;;###autoload
(defun mail (&optional noerase to subject in-reply-to cc replybuffer
		       actions return-action)
  "Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

Optionally, the signature file `mail-signature-file' can be inserted at the
end; see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

The normal hook `mail-setup-hook' is run after the message is
initialized.  It can add more default fields to the message.

The first argument, NOERASE, determines what to do when there is
an existing modified `*mail*' buffer.  If NOERASE is nil, the
existing mail buffer is used, and the user is prompted whether to
keep the old contents or to erase them.  If NOERASE has the value
`new', a new mail buffer will be created instead of using the old
one.  Any other non-nil value means to always select the old
buffer without erasing the contents.

The second through fifth arguments,
 TO, SUBJECT, IN-REPLY-TO and CC, specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer which contains an
 original message being replied to, or else an action
 of the form (FUNCTION . ARGS) which says how to insert the original.
 Or it can be nil, if not replying to anything.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'."
  (interactive "P")
  (if (eq noerase 'new)
      (pop-to-buffer-same-window (generate-new-buffer "*mail*"))
    (and noerase
	 (not (get-buffer "*mail*"))
	 (setq noerase nil))
    (pop-to-buffer-same-window "*mail*"))

  ;; Avoid danger that the auto-save file can't be written.
  (let ((dir (expand-file-name
	      (file-name-as-directory mail-default-directory))))
    (if (file-exists-p dir)
	(setq default-directory dir)))
  ;; Only call auto-save-mode if necessary, to avoid changing auto-save file.
  (if (or (and auto-save-default (not buffer-auto-save-file-name))
          (and (not auto-save-default) buffer-auto-save-file-name))
      (auto-save-mode auto-save-default))
  (mail-mode)
  ;; Disconnect the buffer from its visited file
  ;; (in case the user has actually visited a file *mail*).
  ;; (set-visited-file-name nil)
  (let (initialized)
    (and (not (and noerase
		   (not (eq noerase 'new))))
	 (if buffer-file-name
	     (if (buffer-modified-p)
		 (when (y-or-n-p "Buffer has unsaved changes; reinitialize it and discard them? ")
		   (if (y-or-n-p "Disconnect buffer from visited file? ")
		       (set-visited-file-name nil))
		   t)
	       (when (y-or-n-p "Reinitialize buffer, and disconnect it from the visited file? ")
		 (set-visited-file-name nil)
		 t))
	   ;; A non-file-visiting buffer.
	   (if (buffer-modified-p)
	       (y-or-n-p "Unsent message being composed; erase it? ")
	     t))
	 (let ((inhibit-read-only t))
	   (erase-buffer)
	   (mail-setup to subject in-reply-to cc replybuffer actions
		       return-action)
	   (setq initialized t)))
    (if (and buffer-auto-save-file-name
	     (file-exists-p buffer-auto-save-file-name))
	(message "Auto save file for draft message exists; consider M-x mail-recover"))
    initialized))

(declare-function dired-view-file "dired" ())
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defun mail-recover-1 ()
  "Pop up a list of auto-saved draft messages so you can recover one of them."
  (interactive)
  (let ((file-name (make-auto-save-file-name))
	(ls-lisp-support-shell-wildcards t)
	non-random-len wildcard)
    ;; Remove the random part from the auto-save-file-name, and
    ;; create a wildcard which matches possible candidates.
    ;; Note: this knows that make-auto-save-file-name appends
    ;; "#<RANDOM-STUFF>#" to the buffer name, where RANDOM-STUFF
    ;; is the result of (make-temp-name "").
    (setq non-random-len
	  (- (length file-name) (length (make-temp-name "")) 1))
    (setq wildcard (concat (substring file-name 0 non-random-len) "*"))
    (if (null (file-expand-wildcards wildcard))
	(message "There are no auto-saved drafts to recover")
      ;; Bind dired-trivial-filenames to t because all auto-save file
      ;; names are normally ``trivial'', so Dired will set point after
      ;; all the files, at buffer bottom.  We want it on the first
      ;; file instead.
      ;; Require dired so that dired-trivial-filenames does not get
      ;; unbound on exit from the let.
      (require 'dired)
      (let ((dired-trivial-filenames t))
	(dired-other-window wildcard (concat dired-listing-switches " -t")))
      (rename-buffer "*Auto-saved Drafts*" t)
      (save-excursion
	(goto-char (point-min))
	(or (looking-at " Move to the draft file you want to recover,")
	    (let ((inhibit-read-only t))
	      ;; Each line starts with a space so that Font Lock mode
	      ;; won't highlight the first character.
	      (insert "\
 Move to the draft file you want to recover, then type C-c C-c
 to recover text of message whose composition was interrupted.
 To browse text of a draft, type v on the draft file's line.

 You can also delete some of these files;
 type d on a line to mark that file for deletion.

 List of possible auto-save files for recovery:

"))))
      (use-local-map
       (let ((map (make-sparse-keymap)))
	 (set-keymap-parent map (current-local-map))
	 map))
      (define-key (current-local-map) "v"
	(lambda ()
	  (interactive)
	  (let ((coding-system-for-read 'utf-8-emacs-unix))
	    (dired-view-file))))
      (define-key (current-local-map) "\C-c\C-c"
	(lambda ()
	  (interactive)
	  (let ((fname (dired-get-filename))
		;; Auto-saved files are written in the internal
		;; representation, so they should be read accordingly.
		(coding-system-for-read 'utf-8-emacs-unix))
	    (switch-to-buffer-other-window "*mail*")
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (insert-file-contents fname nil)
	      ;; insert-file-contents will set buffer-file-coding-system
	      ;; to utf-8-emacs, which is probably not what they want to
	      ;; use for sending the message.  But we don't know what
	      ;; was its value before the buffer was killed or Emacs
	      ;; crashed.  We therefore reset buffer-file-coding-system
	      ;; to the default value, so that either the default does
	      ;; TRT, or the user will get prompted for the right
	      ;; encoding when they send the message.
	      (setq buffer-file-coding-system
		    (default-value 'buffer-file-coding-system)))))))))

(declare-function dired-move-to-filename "dired" (&optional raise-error eol))
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function dired-view-file "dired" ())

(defun mail-recover ()
  "Recover interrupted mail composition from auto-save files.

If the mail buffer has a current valid auto-save file,
the command recovers that file.  Otherwise, it displays a
buffer showing the existing auto-saved draft messages;
you can move to one of them and type C-c C-c to recover that one."
  (interactive)
  ;; In case they invoke us from some random buffer...
  (switch-to-buffer "*mail*")
  ;; If *mail* didn't exist, set its directory, so that auto-saved
  ;; drafts will be found.
  (let ((dir (expand-file-name
	      (file-name-as-directory mail-default-directory))))
    (if (file-exists-p dir)
	(setq default-directory dir)))
  (or (eq major-mode 'mail-mode)
      (mail-mode))
  (let ((file-name buffer-auto-save-file-name))
    (cond ((and file-name (file-exists-p file-name))
	   (let ((dispbuf
		  ;; This used to invoke `ls' via call-process, but
		  ;; dired-noselect is more portable to systems where
		  ;; `ls' is not a standard program (it will use
		  ;; ls-lisp instead).
		  (dired-noselect file-name
				  (concat dired-listing-switches " -t"))))
	     (save-selected-window
	       (switch-to-buffer-other-window dispbuf)
	       (goto-char (point-min))
	       (forward-line 2)
	       (dired-move-to-filename)
	       (setq dispbuf (rename-buffer "*Directory*" t)))
	     (if (not (yes-or-no-p
		       (format "Recover mail draft from auto save file %s? "
			       file-name)))
		 (error "mail-recover cancelled")
	       (let ((buffer-read-only nil)
		     (buffer-coding buffer-file-coding-system)
		     ;; Auto-save files are written in internal
		     ;; representation of non-ASCII characters.
		     (coding-system-for-read 'utf-8-emacs-unix))
		 (erase-buffer)
		 (insert-file-contents file-name nil)
		 (setq buffer-file-coding-system buffer-coding)))))
	  (t (mail-recover-1)))))

;;;###autoload
(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another window."
  (interactive "P")
  (switch-to-buffer-other-window "*mail*")
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;;;###autoload
(defun mail-other-frame (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another frame."
  (interactive "P")
  (switch-to-buffer-other-frame "*mail*")
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;; Do not add anything but external entries on this page.

(provide 'sendmail)

;; Local Variables:
;; byte-compile-dynamic: t
;; coding: utf-8
;; End:

;;; sendmail.el ends here
