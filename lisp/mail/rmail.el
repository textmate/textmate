;;; rmail.el --- main code of "RMAIL" mail reader for Emacs

;; Copyright (C) 1985-1988, 1993-1998, 2000-2012
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

;;; Code:

;; Souped up by shane@mit-ajax based on ideas of rlk@athena.mit.edu
;;   New features include attribute and keyword support, message
;;   selection by dispatch table, summary by attributes and keywords,
;;   expunging by dispatch table, sticky options for file commands.

;; Extended by Bob Weiner of Motorola
;;   New features include: rmail and rmail-summary buffers remain
;;   synchronized and key bindings basically operate the same way in both
;;   buffers, summary by topic or by regular expression, rmail-reply-prefix
;;   variable, and a bury rmail buffer (wipe) command.
;;

(require 'mail-utils)
(require 'rfc2047)

(defconst rmail-attribute-header "X-RMAIL-ATTRIBUTES"
  "The header that stores the Rmail attribute data.")

(defconst rmail-keyword-header "X-RMAIL-KEYWORDS"
  "The header that stores the Rmail keyword data.")

;;; Attribute indexes

(defconst rmail-answered-attr-index 0
  "The index for the `answered' attribute.")

(defconst rmail-deleted-attr-index 1
  "The index for the `deleted' attribute.")

(defconst rmail-edited-attr-index 2
  "The index for the `edited' attribute.")

(defconst rmail-filed-attr-index 3
  "The index for the `filed' attribute.")

(defconst rmail-retried-attr-index 4
  "The index for the `retried' attribute.")

(defconst rmail-forwarded-attr-index 5
  "The index for the `forwarded' attribute.")

(defconst rmail-unseen-attr-index 6
  "The index for the `unseen' attribute.")

(defconst rmail-resent-attr-index 7
  "The index for the `resent' attribute.")

(defconst rmail-attr-array
  '[(?A "answered")
    (?D "deleted")
    (?E "edited")
    (?F "filed")
    (?R "retried")
    (?S "forwarded")
    (?U "unseen")
    (?r "resent")]
  "An array that provides a mapping between an attribute index,
its character representation and its display representation.")

(defvar deleted-head)
(defvar font-lock-fontified)
(defvar mail-abbrev-syntax-table)
(defvar mail-abbrevs)
(defvar messages-head)
(defvar total-messages)
(defvar tool-bar-map)
(defvar mail-encode-mml)

(defvar rmail-header-style 'normal
  "The current header display style choice, one of
'normal (selected headers) or 'full (all headers).")

;; rmail-spool-directory and rmail-file-name are defined in paths.el.

(defgroup rmail nil
  "Mail reader for Emacs."
  :group 'mail)

(defgroup rmail-retrieve nil
  "Rmail retrieval options."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-files nil
  "Rmail files."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-headers nil
  "Rmail header options."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-reply nil
  "Rmail reply options."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-summary nil
  "Rmail summary options."
  :prefix "rmail-"
  :prefix "rmail-summary-"
  :group 'rmail)

(defgroup rmail-output nil
  "Output message to a file."
  :prefix "rmail-output-"
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-edit nil
  "Rmail editing."
  :prefix "rmail-edit-"
  :group 'rmail)

(defcustom rmail-movemail-program nil
  "If non-nil, the file name of the `movemail' program."
  :group 'rmail-retrieve
  :type '(choice (const nil) string))

(define-obsolete-variable-alias 'rmail-pop-password
  'rmail-remote-password "22.1")

(defcustom rmail-remote-password nil
  "Password to use when reading mail from a remote server.
This setting is ignored for mailboxes whose URL already contains a password."
  :type '(choice (string :tag "Password")
		 (const :tag "Not Required" nil))
  :group 'rmail-retrieve
  :version "22.1")

(define-obsolete-variable-alias 'rmail-pop-password-required
  'rmail-remote-password-required "22.1")

(defcustom rmail-remote-password-required nil
  "Non-nil if a password is required when reading mail from a remote server."
  :type 'boolean
  :group 'rmail-retrieve
  :version "22.1")

(defcustom rmail-movemail-flags nil
  "List of flags to pass to movemail.
Most commonly used to specify `-g' to enable GSS-API authentication
or `-k' to enable Kerberos authentication."
  :type '(repeat string)
  :group 'rmail-retrieve
  :version "20.3")

(defvar rmail-remote-password-error "invalid usercode or password\\|
unknown user name or bad password\\|Authentication failed\\|MU_ERR_AUTH_FAILURE"
  "Regular expression matching incorrect-password POP or IMAP server error
messages.
If you get an incorrect-password error that this expression does not match,
please report it with \\[report-emacs-bug].")

(defvar rmail-encoded-remote-password nil)

(defcustom rmail-preserve-inbox nil
  "Non-nil means leave incoming mail in the user's inbox--don't delete it."
  :type 'boolean
  :group 'rmail-retrieve)

(defcustom rmail-movemail-search-path nil
  "List of directories to search for movemail (in addition to `exec-path')."
  :group 'rmail-retrieve
  :type '(repeat (directory)))

(declare-function mail-dont-reply-to "mail-utils" (destinations))
(declare-function rmail-update-summary "rmailsum" (&rest ignore))
(declare-function rmail-mime-toggle-hidden "rmailmm" ())

(defun rmail-probe (prog)
  "Determine what flavor of movemail PROG is.
We do this by executing it with `--version' and analyzing its output."
  (with-temp-buffer
    (let ((tbuf (current-buffer)))
      (buffer-disable-undo tbuf)
      (call-process prog nil tbuf nil "--version")
      (if (not (buffer-modified-p tbuf))
	  ;; Should not happen...
	  nil
	(goto-char (point-min))
	(cond
	 ((looking-at ".*movemail: invalid option")
	  'emacs)    ;; Possibly...
	 ((looking-at "movemail (GNU Mailutils .*)")
	  'mailutils)
	 (t
	  ;; FIXME:
	  'emacs))))))

(defun rmail-autodetect ()
  "Determine the file name of the `movemail' program and return its flavor.
If `rmail-movemail-program' is non-nil, use it.
Otherwise, look for `movemail' in the directories in
`rmail-movemail-search-path', those in `exec-path', and `exec-directory'."
  (if rmail-movemail-program
      (rmail-probe rmail-movemail-program)
    (catch 'scan
      (dolist (dir (append rmail-movemail-search-path exec-path
			   (list exec-directory)))
	(when (and dir (file-accessible-directory-p dir))
	  ;; Previously, this didn't have to work on Windows, because
	  ;; rmail-insert-inbox-text before r1.439 fell back to using
	  ;; (expand-file-name "movemail" exec-directory) and just
	  ;; assuming it would work.
	  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-02/msg00087.html
	  (let ((progname (expand-file-name
			   (concat "movemail"
				   (if (memq system-type '(ms-dos windows-nt))
				       ".exe")) dir)))
	    (when (and (not (file-directory-p progname))
		       (file-executable-p progname))
	      (let ((x (rmail-probe progname)))
		(when x
		  (setq rmail-movemail-program progname)
		  (throw 'scan x))))))))))

(defvar rmail-movemail-variant-in-use nil
  "The movemail variant currently in use. Known variants are:

  `emacs'     Means any implementation, compatible with the native Emacs one.
              This is the default;
  `mailutils' Means GNU mailutils implementation, capable of handling full
mail URLs as the source mailbox.")

;;;###autoload
(defun rmail-movemail-variant-p (&rest variants)
  "Return t if the current movemail variant is any of VARIANTS.
Currently known variants are 'emacs and 'mailutils."
  (when (not rmail-movemail-variant-in-use)
    ;; Autodetect
    (setq rmail-movemail-variant-in-use (rmail-autodetect)))
  (not (null (member rmail-movemail-variant-in-use variants))))

;; Call for effect, to set rmail-movemail-program (if not set by the
;; user), and rmail-movemail-variant-in-use. Used by various functions.
;; I'm not sure if M-x rmail is the only entry point to this package.
;; If so, this can be moved there.
(rmail-movemail-variant-p)

;;;###autoload
(defcustom rmail-user-mail-address-regexp nil
  "Regexp matching user mail addresses.
If non-nil, this variable is used to identify the correspondent
when receiving new mail.  If it matches the address of the sender,
the recipient is taken as correspondent of a mail.
If nil \(default value\), your `user-login-name' and `user-mail-address'
are used to exclude yourself as correspondent.

Usually you don't have to set this variable, except if you collect mails
sent by you under different user names.
Then it should be a regexp matching your mail addresses.

Setting this variable has an effect only before reading a mail."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'rmail-retrieve
  :version "21.1")

;;;###autoload
(define-obsolete-variable-alias 'rmail-dont-reply-to-names
  'mail-dont-reply-to-names "24.1")

;; Prior to 24.1, this used to contain "\\`info-".
;;;###autoload
(defvar rmail-default-dont-reply-to-names nil
  "Regexp specifying part of the default value of `mail-dont-reply-to-names'.
This is used when the user does not set `mail-dont-reply-to-names'
explicitly.")
;;;###autoload
(make-obsolete-variable 'rmail-default-dont-reply-to-names
                        'mail-dont-reply-to-names "24.1")

;;;###autoload
(defcustom rmail-ignored-headers
  (purecopy
  (concat "^via:\\|^mail-from:\\|^origin:\\|^references:\\|^sender:"
	  "\\|^status:\\|^received:\\|^x400-originator:\\|^x400-recipients:"
	  "\\|^x400-received:\\|^x400-mts-identifier:\\|^x400-content-type:"
	  "\\|^\\(resent-\\|\\)message-id:\\|^summary-line:\\|^resent-date:"
	  "\\|^nntp-posting-host:\\|^path:\\|^x-char.*:\\|^x-face:\\|^face:"
	  "\\|^x-mailer:\\|^delivered-to:\\|^lines:"
	  "\\|^content-transfer-encoding:\\|^x-coding-system:"
	  "\\|^return-path:\\|^errors-to:\\|^return-receipt-to:"
	  "\\|^precedence:\\|^mime-version:"
	  "\\|^list-owner:\\|^list-help:\\|^list-post:\\|^list-subscribe:"
	  "\\|^list-id:\\|^list-unsubscribe:\\|^list-archive:"
	  "\\|^content-length:\\|^nntp-posting-date:\\|^user-agent"
	  "\\|^importance:\\|^envelope-to:\\|^delivery-date\\|^openpgp:"
	  "\\|^mbox-line:\\|^cancel-lock:"
	  "\\|^DomainKey-Signature:\\|^dkim-signature:"
	  "\\|^resent-face:\\|^resent-x.*:\\|^resent-organization:\\|^resent-openpgp:"
	  "\\|^x-.*:"))
  "Regexp to match header fields that Rmail should normally hide.
\(See also `rmail-nonignored-headers', which overrides this regexp.)
This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Rmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[rmail-toggle-header] twice."
  :type 'regexp
  :group 'rmail-headers)

(defcustom rmail-nonignored-headers "^x-spam-status:"
  "Regexp to match X header fields that Rmail should show.
This regexp overrides `rmail-ignored-headers'; if both this regexp
and that one match a certain header field, Rmail shows the field.
If this is nil, ignore all header fields in `rmail-ignored-headers'.

This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Rmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[rmail-toggle-header] twice."
  :type '(choice (const nil) (regexp))
  :group 'rmail-headers)

;;;###autoload
(defcustom rmail-displayed-headers nil
  "Regexp to match Header fields that Rmail should display.
If nil, display all header fields except those matched by
`rmail-ignored-headers'."
  :type '(choice regexp (const :tag "All"))
  :group 'rmail-headers)

;;;###autoload
(defcustom rmail-retry-ignored-headers (purecopy "^x-authentication-warning:\\|^x-detected-operating-system:\\|^x-spam[-a-z]*:\\|content-type:\\|content-transfer-encoding:\\|mime-version:\\|message-id:")
  "Headers that should be stripped when retrying a failed message."
  :type '(choice regexp (const nil :tag "None"))
  :group 'rmail-headers
  :version "23.2")	   ; added x-detected-operating-system, x-spam

;;;###autoload
(defcustom rmail-highlighted-headers (purecopy "^From:\\|^Subject:")
  "Regexp to match Header fields that Rmail should normally highlight.
A value of nil means don't highlight.  Uses the face `rmail-highlight'."
  :type 'regexp
  :group 'rmail-headers)

(defface rmail-highlight
  '((t (:inherit highlight)))
  "Face to use for highlighting the most important header fields.
The variable `rmail-highlighted-headers' specifies which headers."
  :group 'rmail-headers
  :version "22.1")

;; This was removed in Emacs 23.1 with no notification, an unnecessary
;; incompatible change.
(defcustom rmail-highlight-face 'rmail-highlight
  "Face used by Rmail for highlighting headers."
  ;; Note that nil doesn't actually mean use the default face, it
  ;; means use either bold or highlight. It's not worth fixing this
  ;; now that this is obsolete.
  :type '(choice (const :tag "Default" nil)
		 face)
  :group 'rmail-headers)
(make-obsolete-variable 'rmail-highlight-face
			"customize the face `rmail-highlight' instead."
			"23.2")

(defface rmail-header-name
  '((t (:inherit font-lock-function-name-face)))
  "Face to use for highlighting the header names.
The variable `rmail-font-lock-keywords' specifies which headers
get highlighted."
  :group 'rmail-headers
  :version "23.1")

(defcustom rmail-delete-after-output nil
  "Non-nil means automatically delete a message that is copied to a file."
  :type 'boolean
  :group 'rmail-files)

;;;###autoload
(defcustom rmail-primary-inbox-list nil
  "List of files that are inboxes for your primary mail file `rmail-file-name'.
If this is nil, uses the environment variable MAIL.  If that is
unset, uses a file named by the function `user-login-name' in the
directory `rmail-spool-directory' (whose value depends on the
operating system).  For example, \"/var/mail/USER\"."
  ;; Don't use backquote here, because we don't want to need it at load time.
  ;; (That must be an old comment - it's dumped these days.)
  :type (list 'choice '(const :tag "Default" nil)
	      (list 'repeat ':value (list (or (getenv "MAIL")
					      (concat rmail-spool-directory
						      (user-login-name))))
		    'file))
  :group 'rmail-retrieve
  :group 'rmail-files)

(defcustom rmail-mail-new-frame nil
  "Non-nil means Rmail makes a new frame for composing outgoing mail.
This is handy if you want to preserve the window configuration of
the frame where you have the RMAIL buffer displayed."
  :type 'boolean
  :group 'rmail-reply)

;;;###autoload
(defcustom rmail-secondary-file-directory (purecopy "~/")
  "Directory for additional secondary Rmail files."
  :type 'directory
  :group 'rmail-files)
;;;###autoload
(defcustom rmail-secondary-file-regexp (purecopy "\\.xmail$")
  "Regexp for which files are secondary Rmail files."
  :type 'regexp
  :group 'rmail-files)

(defcustom rmail-confirm-expunge 'y-or-n-p
  "Whether and how to ask for confirmation before expunging deleted messages.
The value, if non-nil is a function to call with a question (string)
as argument, to ask the user that question."
  :type '(choice (const :tag "No confirmation" nil)
		 (const :tag "Confirm with y-or-n-p" y-or-n-p)
		 (const :tag "Confirm with yes-or-no-p" yes-or-no-p))
  :version "21.1"
  :group 'rmail-files)
(put 'rmail-confirm-expunge 'risky-local-variable t)

;;;###autoload
(defvar rmail-mode-hook nil
  "List of functions to call when Rmail is invoked.")

(defvar rmail-get-new-mail-hook nil
  "List of functions to call when Rmail has retrieved new mail.")

;;;###autoload
(defcustom rmail-show-message-hook nil
  "List of functions to call when Rmail displays a message."
  :type 'hook
  :options '(goto-address)
  :group 'rmail)

(defvar rmail-quit-hook nil
  "List of functions to call when quitting out of Rmail.")

(defvar rmail-delete-message-hook nil
  "List of functions to call when Rmail deletes a message.
When the hooks are called, the message has been marked deleted but is
still the current message in the Rmail buffer.")

;; These may be altered by site-init.el to match the format of mmdf files
;;  delimiting used on a given host (delim1 and delim2 from the config
;;  files).

(defvar rmail-mmdf-delim1 "^\001\001\001\001\n"
  "Regexp marking the start of an mmdf message.")
(defvar rmail-mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message.")

;; FIXME Post-mbox, this is now unused.
;; In Emacs-22, this was called:
;;  i) the very first time a message was shown.
;; ii) when toggling the headers to the normal state, every time.
;; It's not clear what it should do now, since there is nothing that
;; records when a message is shown for the first time (unseen is not
;; necessarily the same thing).
;; See http://lists.gnu.org/archive/html/emacs-devel/2009-03/msg00013.html
(defcustom rmail-message-filter nil
  "If non-nil, a filter function for new messages in RMAIL.
Called with region narrowed to the message, including headers,
before obeying `rmail-ignored-headers'."
  :group 'rmail-headers
  :type '(choice (const nil) function))

(make-obsolete-variable 'rmail-message-filter
			"it is not used (try `rmail-show-message-hook')."
			"23.1")

(defcustom rmail-automatic-folder-directives nil
  "List of directives specifying how to automatically file messages.
Whenever Rmail shows a message in the folder that `rmail-file-name'
specifies, it calls `rmail-auto-file' to maybe file the message in
another folder according to this list.  Messages that are already
marked as `filed', or are in different folders, are left alone.

Each element of the list is of the form:

  (FOLDERNAME FIELD REGEXP [ FIELD REGEXP ] ... )

FOLDERNAME is the name of a folder in which to put the message.
If FOLDERNAME is nil then Rmail deletes the message, and moves on to
the next.  If FOLDERNAME is \"/dev/null\", Rmail deletes the message,
but does not move to the next.

FIELD is the name of a header field in the message, such as
\"subject\" or \"from\".  A FIELD of \"to\" includes all text
from both the \"to\" and \"cc\" headers.

REGEXP is a regular expression to match (case-sensitively) against
the preceding specified FIELD.

There may be any number of FIELD/REGEXP pairs.
All pairs must match for a directive to apply to a message.
For a given message, Rmail applies only the first matching directive.

Examples:
  (\"/dev/null\" \"from\" \"@spam.com\") ; delete all mail from spam.com
  (\"RMS\" \"from\" \"rms@\") ; save all mail from RMS.
"
  :group 'rmail
  :version "21.1"
  :type '(repeat (sexp :tag "Directive")))

(defvar rmail-reply-prefix "Re: "
  "String to prepend to Subject line when replying to a message.")

;; Some mailers use "Re(2):" or "Re^2:" or "Re: Re:" or "Re[2]:".
;; This pattern should catch all the common variants.
;; rms: I deleted the change to delete tags in square brackets
;; because they mess up RT tags.
(defvar rmail-reply-regexp "\\`\\(Re\\(([0-9]+)\\|\\[[0-9]+\\]\\|\\^[0-9]+\\)?: *\\)*"
  "Regexp to delete from Subject line before inserting `rmail-reply-prefix'.")

(defcustom rmail-display-summary nil
  "If non-nil, Rmail always displays the summary buffer."
  :group 'rmail-summary
  :type 'boolean)

(defvar rmail-inbox-list nil)
(put 'rmail-inbox-list 'permanent-local t)

(defvar rmail-buffer nil
  "The RMAIL buffer related to the current buffer.
In an RMAIL buffer, this holds the RMAIL buffer itself.
In a summary buffer, this holds the RMAIL buffer it is a summary for.")
(put 'rmail-buffer 'permanent-local t)

(defvar rmail-was-converted nil
  "Non-nil in an Rmail buffer that was just converted from Babyl format.")
(put 'rmail-was-converted 'permanent-local t)

(defvar rmail-seriously-modified nil
  "Non-nil in an Rmail buffer that has been modified in a major way.")
(put 'rmail-seriously-modified 'permanent-local t)

;; Message counters and markers.  Deleted flags.

(defvar rmail-current-message nil
  "Integer specifying the message currently being displayed in this folder.
Counts messages from 1 to `rmail-total-messages'.  A value of 0
means there are no messages in the folder.")
(put 'rmail-current-message 'permanent-local t)

(defvar rmail-total-messages nil
  "Integer specifying the total number of messages in this folder.
Includes deleted messages.")
(put 'rmail-total-messages 'permanent-local t)

(defvar rmail-message-vector nil
  "Vector of markers specifying the start and end of each message.
Element N and N+1 specify the start and end of message N.")
(put 'rmail-message-vector 'permanent-local t)

(defvar rmail-deleted-vector nil
  "A string of length `rmail-total-messages' plus one.
Character N is either a space or \"D\", according to whether
message N is deleted or not.")
(put 'rmail-deleted-vector 'permanent-local t)

(defvar rmail-msgref-vector nil
  "In an Rmail buffer, a vector whose Nth element is a list (N).
When expunging renumbers messages, these lists are modified
by substituting the new message number into the existing list.")
(put 'rmail-msgref-vector 'permanent-local t)

(defvar rmail-overlay-list nil)
(put 'rmail-overlay-list 'permanent-local t)

;; These are used by autoloaded rmail-summary.

(defvar rmail-summary-buffer nil)
(put 'rmail-summary-buffer 'permanent-local t)
(defvar rmail-summary-vector nil
  "In an Rmail buffer, vector of (newline-terminated) strings.
Element N specifies the summary line for message N+1.")
(put 'rmail-summary-vector 'permanent-local t)

;; Rmail buffer swapping variables.

(defvar rmail-buffer-swapped nil
  "If non-nil, `rmail-buffer' is swapped with `rmail-view-buffer'.")
(make-variable-buffer-local 'rmail-buffer-swapped)
(put 'rmail-buffer-swapped 'permanent-local t)

(defvar rmail-view-buffer nil
  "Buffer which holds RMAIL message for MIME displaying.")
(make-variable-buffer-local 'rmail-view-buffer)
(put 'rmail-view-buffer 'permanent-local t)

;; `Sticky' default variables.

;; Last individual label specified to a or k.
(defvar rmail-last-label nil)

;; Last set of values specified to C-M-n, C-M-p, C-M-s or C-M-l.
(defvar rmail-last-multi-labels nil)

(defvar rmail-last-regexp nil)
(put 'rmail-last-regexp 'permanent-local t)

(defcustom rmail-default-file "~/xmail"
  "Default file name for \\[rmail-output]."
  :type 'file
  :group 'rmail-files)
(defcustom rmail-default-body-file "~/mailout"
  "Default file name for \\[rmail-output-body-to-file]."
  :type 'file
  :group 'rmail-files
  :version "20.3")

;; Mule and MIME related variables.

;;;###autoload
(defvar rmail-file-coding-system nil
  "Coding system used in RMAIL file.

This is set to nil by default.")

(defcustom rmail-enable-mime t
  "If non-nil, RMAIL automatically displays decoded MIME messages.
For this to work, the feature specified by `rmail-mime-feature' must
be available."
  :type 'boolean
  :version "23.3"
  :group 'rmail)

(defcustom rmail-enable-mime-composing t
  "If non-nil, use `rmail-insert-mime-forwarded-message-function' to forward."
  :type 'boolean
  :version "24.1"			; nil -> t
  :group 'rmail)

(defvar rmail-show-mime-function nil
  "Function of no argument called to show a decoded MIME message.
This function is called when `rmail-enable-mime' is non-nil.
The package providing MIME support should set this.")

;;;###autoload
(defvar rmail-insert-mime-forwarded-message-function nil
  "Function to insert a message in MIME format so it can be forwarded.
This function is called if `rmail-enable-mime' and
`rmail-enable-mime-composing' are non-nil.
It is called with one argument FORWARD-BUFFER, which is a
buffer containing the message to forward.  The current buffer
is the outgoing mail buffer.")

(defvar rmail-insert-mime-resent-message-function nil
  "Function to insert a message in MIME format so it can be resent.
This function is called by `rmail-resend' if `rmail-enable-mime' is non-nil.
It is called with one argument FORWARD-BUFFER, which is a
buffer containing the message to forward.  The current buffer
is the outgoing mail buffer.")

;; FIXME one might want to pass a LIMIT, as per
;; rmail-search-mime-header-function.
(defvar rmail-search-mime-message-function nil
  "Function to check if a regexp matches a MIME message.
This function is called by `rmail-search-message' if
`rmail-enable-mime' is non-nil.  It is called (with point at the
start of the message) with two arguments MSG and REGEXP, where
MSG is the message number, REGEXP is the regular expression.")

(defvar rmail-search-mime-header-function nil
  "Function to check if a regexp matches a header of MIME message.
This function is called by `rmail-message-regexp-p-1' if
`rmail-enable-mime' is non-nil.  It is called (with point at the
start of the header) with three arguments MSG, REGEXP, and LIMIT,
where MSG is the message number, REGEXP is the regular
expression, LIMIT is the position specifying the end of header.")

(defvar rmail-mime-feature 'rmailmm
  "Feature to require for MIME support in Rmail.
When starting Rmail, if `rmail-enable-mime' is non-nil, this
feature is loaded with `require'.  The default value is `rmailmm'.

The library should set the variable `rmail-show-mime-function'
to an appropriate value, and optionally also set
`rmail-search-mime-message-function',
`rmail-search-mime-header-function',
`rmail-insert-mime-forwarded-message-function', and
`rmail-insert-mime-resent-message-function'.")

;; FIXME this is unused since 23.1.
(defvar rmail-decode-mime-charset t
  "*Non-nil means a message is decoded by MIME's charset specification.
If this variable is nil, or the message has not MIME specification,
the message is decoded as normal way.

If the variable `rmail-enable-mime' is non-nil, this variable is
ignored, and all the decoding work is done by a feature specified by
the variable `rmail-mime-feature'.")

(make-obsolete-variable 'rmail-decode-mime-charset
			"it does nothing." "23.1")

(defvar rmail-mime-charset-pattern
  (concat "^content-type:[ \t]*text/plain;"
	  "\\(?:[ \t\n]*\\(?:format\\|delsp\\)=\"?[-a-z0-9]+\"?;\\)*"
	  "[ \t\n]*charset=\"?\\([^ \t\n\";]+\\)\"?")
  "Regexp to match MIME-charset specification in a header of message.
The first parenthesized expression should match the MIME-charset name.")


(defvar rmail-unix-mail-delimiter
  (let ((time-zone-regexp
	 (concat "\\([A-Z]?[A-Z]?[A-Z][A-Z]\\( DST\\)?"
		 "\\|[-+]?[0-9][0-9][0-9][0-9]"
		 "\\|"
		 "\\) *")))
    (concat
     "From "

     ;; Many things can happen to an RFC 822 mailbox before it is put into
     ;; a `From' line.  The leading phrase can be stripped, e.g.
     ;; `Joe <@w.x:joe@y.z>' -> `<@w.x:joe@y.z>'.  The <> can be stripped, e.g.
     ;; `<@x.y:joe@y.z>' -> `@x.y:joe@y.z'.  Everything starting with a CRLF
     ;; can be removed, e.g.
     ;;		From: joe@y.z (Joe	K
     ;;			User)
     ;; can yield `From joe@y.z (Joe 	K Fri Mar 22 08:11:15 1996', and
     ;;		From: Joe User
     ;;			<joe@y.z>
     ;; can yield `From Joe User Fri Mar 22 08:11:15 1996'.
     ;; The mailbox can be removed or be replaced by white space, e.g.
     ;;		From: "Joe User"{space}{tab}
     ;;			<joe@y.z>
     ;; can yield `From {space}{tab} Fri Mar 22 08:11:15 1996',
     ;; where {space} and {tab} represent the Ascii space and tab characters.
     ;; We want to match the results of any of these manglings.
     ;; The following regexp rejects names whose first characters are
     ;; obviously bogus, but after that anything goes.
     "\\([^\0-\b\n-\r\^?].*\\)? "

     ;; The time the message was sent.
     "\\([^\0-\r \^?]+\\) +"				; day of the week
     "\\([^\0-\r \^?]+\\) +"				; month
     "\\([0-3]?[0-9]\\) +"				; day of month
     "\\([0-2][0-9]:[0-5][0-9]\\(:[0-6][0-9]\\)?\\) *"	; time of day

     ;; Perhaps a time zone, specified by an abbreviation, or by a
     ;; numeric offset.
     time-zone-regexp

     ;; The year.
     " \\([0-9][0-9]+\\) *"

     ;; On some systems the time zone can appear after the year, too.
     time-zone-regexp

     ;; Old uucp cruft.
     "\\(remote from .*\\)?"

     "\n"))
  "Regexp matching the delimiter of messages in UNIX mail format
\(UNIX From lines), minus the initial ^.  Note that if you change
this expression, you must change the code in `rmail-nuke-pinhead-header'
that knows the exact ordering of the \\( \\) subexpressions.")

;; FIXME the rmail-header-name headers ought to be customizable.
;; It seems a bit arbitrary, for example, that all of the Date: line
;; gets highlighted.
(defvar rmail-font-lock-keywords
  ;; These are all matched case-insensitively.
  (eval-when-compile
    (let* ((cite-chars "[>|}]")
	   (cite-prefix "[:alpha:]")
	   (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
      (list '("^\\(From\\|Sender\\|Resent-From\\):"
	      . 'rmail-header-name)
	    '("^\\(Mail-\\)?Reply-To:.*$" . 'rmail-header-name)
	    ;; FIXME Mail-Followup-To should probably be here too.
	    '("^Subject:" . 'rmail-header-name)
	    '("^X-Spam-Status:" . 'rmail-header-name)
	    '("^\\(To\\|Apparently-To\\|Cc\\|Newsgroups\\):"
	      . 'rmail-header-name)
	    ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
	    `(,cite-chars
	      (,(concat "\\=[ \t]*"
			"\\(\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
			"\\(" cite-chars "[ \t]*\\)\\)+\\)"
			"\\(.*\\)")
	       (beginning-of-line) (end-of-line)
	       (1 font-lock-comment-delimiter-face nil t)
	       (5 font-lock-comment-face nil t)))
	    '("^\\(X-[a-z0-9-]+\\|In-reply-to\\|Date\\):.*\\(\n[ \t]+.*\\)*$"
	      . 'rmail-header-name))))
  "Additional expressions to highlight in Rmail mode.")

;; Rmail does not expect horizontal splitting.  (Bug#2282)
(defun rmail-pop-to-buffer (&rest args)
  "Like `pop-to-buffer', but with `split-width-threshold' set to nil."
  (let (split-width-threshold)
    (apply 'pop-to-buffer args)))

;; Perform BODY in the summary buffer
;; in such a way that its cursor is properly updated in its own window.
(defmacro rmail-select-summary (&rest body)
  `(let ((total rmail-total-messages))
     (if (rmail-summary-displayed)
	 (let ((window (selected-window)))
	   (save-excursion
	     (unwind-protect
		 (progn
		   (rmail-pop-to-buffer rmail-summary-buffer)
		   ;; rmail-total-messages is a buffer-local var
		   ;; in the rmail buffer.
		   ;; This way we make it available for the body
		   ;; even tho the rmail buffer is not current.
		   (let ((rmail-total-messages total))
		     ,@body))
	       (select-window window))))
       (with-current-buffer rmail-summary-buffer
	 (let ((rmail-total-messages total))
	   ,@body)))
     (rmail-maybe-display-summary)))

;;;; *** Rmail Mode ***

(defun rmail-require-mime-maybe ()
  "Require `rmail-mime-feature' if that is non-nil.
Signal an error and set `rmail-mime-feature' to nil if the feature
isn't provided."
  (when rmail-enable-mime
    (condition-case err
	(require rmail-mime-feature)
      (error
       (display-warning
	'rmail
	(format "Although MIME support is requested
through `rmail-enable-mime' being non-nil, the required feature
`%s' (the value of `rmail-mime-feature')
is not available in the current session.
So, MIME support is turned off for the moment."
		rmail-mime-feature)
	:warning)
       (setq rmail-enable-mime nil)))))


;;;###autoload
(defun rmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by `rmail-file-name' and edits that
file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with file name as argument; then performs rmail editing on
that file, but does not copy any new mail into the file.
Interactively, if you supply a prefix argument, then you
have a chance to specify a file name with the minibuffer.

If `rmail-display-summary' is non-nil, make a summary for this RMAIL file."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail on RMAIL file: "))))
  (rmail-require-mime-maybe)
  (let* ((file-name (expand-file-name (or file-name-arg rmail-file-name)))
	 ;; Use find-buffer-visiting, not get-file-buffer, for those users
	 ;; who have find-file-visit-truename set to t.
	 (existed (find-buffer-visiting file-name))
	 run-mail-hook mail-buf msg-shown)
    ;; Determine if an existing mail file has been changed behind the
    ;; scene...
    (if (and existed (not (verify-visited-file-modtime existed)))
	;; The mail file has been changed.  Revisit it and reset the
	;; message state variables when in rmail mode.
	(progn
	  (find-file file-name)
	  (when (and (verify-visited-file-modtime existed)
		     (eq major-mode 'rmail-mode))
	    (rmail-swap-buffers-maybe)
	    (rmail-set-message-counters)))
      ;; The mail file is either unchanged or not visited.  Visit it.
      (switch-to-buffer
       (let ((enable-local-variables nil)
	     ;; Force no-conversion by default, since that's what
	     ;; pre-mbox Rmail did with BABYL files (via
	     ;; auto-coding-regexp-alist).
	     (coding-system-for-read
	      (or coding-system-for-read 'no-conversion)))
	 (find-file-noselect file-name))))
    ;; Ensure that the collection and view buffers are in sync and
    ;; ensure that a message is not being edited.
    (if (eq major-mode 'rmail-mode)
	(rmail-swap-buffers-maybe))
    (if (eq major-mode 'rmail-edit-mode)
	(error "Exit Rmail Edit mode before getting new mail"))
    (or (and existed (> (buffer-size) 0))
	(setq run-mail-hook t))
    ;; Ensure that the Rmail file is in mbox format, the buffer is in
    ;; Rmail mode and has been scanned to find all the messages
    ;; (setting the global message variables in the process).
    (rmail-convert-file-maybe)
    (unless (eq major-mode 'rmail-mode)
      (rmail-mode-2))
    (goto-char (point-max))
    (rmail-maybe-set-message-counters)
    (setq mail-buf rmail-buffer)
    ;; Show the first unread message and process summary mode.
    (unwind-protect
	;; Only get new mail when there is not a file name argument.
	(unless file-name-arg
	  (setq msg-shown (rmail-get-new-mail)))
      (progn
	(set-buffer mail-buf)
	(or msg-shown
	    (rmail-show-message (rmail-first-unseen-message)))
	(if rmail-display-summary (rmail-summary))
	(rmail-construct-io-menu)
	(if run-mail-hook
	    (run-hooks 'rmail-mode-hook))))))

(defun rmail-convert-file-maybe ()
  "Determine if the file needs to be converted to mbox format."
  (widen)
  (goto-char (point-min))
  ;; Detect previous Babyl format files.
  (let ((case-fold-search nil))
    (cond ((looking-at "BABYL OPTIONS:")
	   ;; The file is Babyl version 5.  Use unrmail to convert
	   ;; it.
	   (rmail-convert-babyl-to-mbox))
	  ((looking-at "Version: 5\n")
	   ;; Losing babyl file made by old version of Rmail.  Fix the
	   ;; babyl file header and use unrmail to convert to mbox
	   ;; format.
	   (let ((buffer-read-only nil))
	     (insert "BABYL OPTIONS: -*- rmail -*-\n")
	     (rmail-convert-babyl-to-mbox)))
	  ((equal (point-min) (point-max))
	   (message "Empty Rmail file."))
	  ((looking-at "From "))
	  (t (error "Invalid mbox file")))))

(defun rmail-error-bad-format (&optional msgnum)
  "Report that the buffer is not in the mbox file format.
MSGNUM, if present, indicates the malformed message."
  (if msgnum
      (error "Message %d is not a valid RFC2822 message" msgnum)
    (error "Message is not a valid RFC2822 message")))

(defun rmail-convert-babyl-to-mbox ()
  "Convert the mail file from Babyl version 5 to mbox.
This function also reinitializes local variables used by Rmail."
  (let ((old-file (make-temp-file "rmail"))
	(new-file (make-temp-file "rmail")))
    (unwind-protect
	(progn
	  (kill-all-local-variables)
	  (write-region (point-min) (point-max) old-file)
	  (unrmail old-file new-file)
	  (message "Replacing BABYL format with mbox format...")
	  (let ((inhibit-read-only t)
		(coding-system-for-read 'raw-text)
		(buffer-undo-list t))
	    (erase-buffer)
	    (insert-file-contents new-file)
	    ;; Rmail buffers need to be saved with Unix EOLs, or else
	    ;; the format will not be recognized.
	    (set-buffer-file-coding-system 'raw-text-unix)
	    (rmail-mode-1)
	    (rmail-perm-variables)
	    (rmail-variables)
	    (setq rmail-was-converted t)
	    (rmail-dont-modify-format)
	    (goto-char (point-max))
	    (rmail-set-message-counters))
	  (message "Replacing BABYL format with mbox format...done"))
      (delete-file old-file)
      (delete-file new-file))))

(defun rmail-get-coding-system ()
  "Return a suitable coding system to use for the current mail message.
The buffer is expected to be narrowed to just the header of the message."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward rmail-mime-charset-pattern nil t)
	(coding-system-from-name (match-string 1))
      'undecided)))

;;; Set up Rmail mode keymaps

(defvar rmail-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "a"      'rmail-add-label)
    (define-key map "b"      'rmail-bury)
    (define-key map "c"      'rmail-continue)
    (define-key map "d"      'rmail-delete-forward)
    (define-key map "\C-d"   'rmail-delete-backward)
    (define-key map "e"      'rmail-edit-current-message)
    ;; If you change this, change the rmail-resend menu-item's :keys.
    (define-key map "f"      'rmail-forward)
    (define-key map "g"      'rmail-get-new-mail)
    (define-key map "h"      'rmail-summary)
    (define-key map "i"      'rmail-input)
    (define-key map "j"      'rmail-show-message)
    (define-key map "k"      'rmail-kill-label)
    (define-key map "l"      'rmail-summary-by-labels)
    (define-key map "\e\C-h" 'rmail-summary)
    (define-key map "\e\C-l" 'rmail-summary-by-labels)
    (define-key map "\e\C-r" 'rmail-summary-by-recipients)
    (define-key map "\e\C-s" 'rmail-summary-by-regexp)
    (define-key map "\e\C-f" 'rmail-summary-by-senders)
    (define-key map "\e\C-t" 'rmail-summary-by-topic)
    (define-key map "m"      'rmail-mail)
    (define-key map "\em"    'rmail-retry-failure)
    (define-key map "n"      'rmail-next-undeleted-message)
    (define-key map "\en"    'rmail-next-message)
    (define-key map "\e\C-n" 'rmail-next-labeled-message)
    (define-key map "o"      'rmail-output)
    (define-key map "\C-o"   'rmail-output-as-seen)
    (define-key map "p"      'rmail-previous-undeleted-message)
    (define-key map "\ep"    'rmail-previous-message)
    (define-key map "\e\C-p" 'rmail-previous-labeled-message)
    (define-key map "q"      'rmail-quit)
    (define-key map "r"      'rmail-reply)
    ;; I find I can't live without the default M-r command -- rms.
    ;;  (define-key rmail-mode-map "\er"  'rmail-search-backwards)
    (define-key map "s"      'rmail-expunge-and-save)
    (define-key map "\es"    'rmail-search)
    (define-key map "t"      'rmail-toggle-header)
    (define-key map "u"      'rmail-undelete-previous-message)
    (define-key map "v"      'rmail-mime)
    (define-key map "w"      'rmail-output-body-to-file)
    (define-key map "\C-c\C-w"    'rmail-widen)
    (define-key map "x"      'rmail-expunge)
    (define-key map "."      'rmail-beginning-of-message)
    (define-key map "/"      'rmail-end-of-message)
    (define-key map "<"      'rmail-first-message)
    (define-key map ">"      'rmail-last-message)
    (define-key map " "      'scroll-up-command)
    (define-key map "\177"   'scroll-down-command)
    (define-key map "?"      'describe-mode)
    (define-key map "\C-c\C-s\C-d" 'rmail-sort-by-date)
    (define-key map "\C-c\C-s\C-s" 'rmail-sort-by-subject)
    (define-key map "\C-c\C-s\C-a" 'rmail-sort-by-author)
    (define-key map "\C-c\C-s\C-r" 'rmail-sort-by-recipient)
    (define-key map "\C-c\C-s\C-c" 'rmail-sort-by-correspondent)
    (define-key map "\C-c\C-s\C-l" 'rmail-sort-by-lines)
    (define-key map "\C-c\C-s\C-k" 'rmail-sort-by-labels)
    (define-key map "\C-c\C-n" 'rmail-next-same-subject)
    (define-key map "\C-c\C-p" 'rmail-previous-same-subject)


    (define-key map [menu-bar] (make-sparse-keymap))

    (define-key map [menu-bar classify]
      (cons "Classify" (make-sparse-keymap "Classify")))

    (define-key map [menu-bar classify input-menu]
      nil)

    (define-key map [menu-bar classify output-menu]
      nil)

    (define-key map [menu-bar classify output-body]
      '("Output body to file..." . rmail-output-body-to-file))

    (define-key map [menu-bar classify output-inbox]
      '("Output..." . rmail-output))

    (define-key map [menu-bar classify output]
      '("Output as seen..." . rmail-output-as-seen))

    (define-key map [menu-bar classify kill-label]
      '("Kill Label..." . rmail-kill-label))

    (define-key map [menu-bar classify add-label]
      '("Add Label..." . rmail-add-label))

    (define-key map [menu-bar summary]
      (cons "Summary" (make-sparse-keymap "Summary")))

    (define-key map [menu-bar summary senders]
      '("By Senders..." . rmail-summary-by-senders))

    (define-key map [menu-bar summary labels]
      '("By Labels..." . rmail-summary-by-labels))

    (define-key map [menu-bar summary recipients]
      '("By Recipients..." . rmail-summary-by-recipients))

    (define-key map [menu-bar summary topic]
      '("By Topic..." . rmail-summary-by-topic))

    (define-key map [menu-bar summary regexp]
      '("By Regexp..." . rmail-summary-by-regexp))

    (define-key map [menu-bar summary all]
      '("All" . rmail-summary))

    (define-key map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))

    (define-key map [menu-bar mail rmail-get-new-mail]
      '("Get New Mail" . rmail-get-new-mail))

    (define-key map [menu-bar mail lambda]
      '("----"))

    (define-key map [menu-bar mail continue]
      '("Continue" . rmail-continue))

    (define-key map [menu-bar mail resend]
    '(menu-item "Resend..." rmail-resend :keys "C-u f"))

    (define-key map [menu-bar mail forward]
      '("Forward" . rmail-forward))

    (define-key map [menu-bar mail retry]
      '("Retry" . rmail-retry-failure))

    (define-key map [menu-bar mail reply]
      '("Reply" . rmail-reply))

    (define-key map [menu-bar mail mail]
      '("Mail" . rmail-mail))

    (define-key map [menu-bar delete]
      (cons "Delete" (make-sparse-keymap "Delete")))

    (define-key map [menu-bar delete expunge/save]
      '("Expunge/Save" . rmail-expunge-and-save))

    (define-key map [menu-bar delete expunge]
      '("Expunge" . rmail-expunge))

    (define-key map [menu-bar delete undelete]
      '("Undelete" . rmail-undelete-previous-message))

    (define-key map [menu-bar delete delete]
      '("Delete" . rmail-delete-forward))

    (define-key map [menu-bar move]
      (cons "Move" (make-sparse-keymap "Move")))

    (define-key map [menu-bar move search-back]
      '("Search Back..." . rmail-search-backwards))

    (define-key map [menu-bar move search]
      '("Search..." . rmail-search))

    (define-key map [menu-bar move previous]
      '("Previous Nondeleted" . rmail-previous-undeleted-message))

    (define-key map [menu-bar move next]
      '("Next Nondeleted" . rmail-next-undeleted-message))

    (define-key map [menu-bar move last]
      '("Last" . rmail-last-message))

    (define-key map [menu-bar move first]
      '("First" . rmail-first-message))

    (define-key map [menu-bar move previous]
      '("Previous" . rmail-previous-message))

    (define-key map [menu-bar move next]
      '("Next" . rmail-next-message))

   map)
  "Keymap used in Rmail mode.")

;; Rmail toolbar
(defvar rmail-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu 'rmail-get-new-mail "mail/inbox"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-next-undeleted-message "right-arrow"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-previous-undeleted-message "left-arrow"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-search "search"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-input "open"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-mail "mail/compose"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-reply "mail/reply-all"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-forward "mail/forward"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-delete-forward "close"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-output "mail/move"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-output-body-to-file "mail/save"
				   map rmail-mode-map)
    (tool-bar-local-item-from-menu 'rmail-expunge "delete"
				   map rmail-mode-map)
    map))



;; Rmail mode is suitable only for specially formatted data.
(put 'rmail-mode 'mode-class 'special)

(defun rmail-mode-kill-summary ()
  (if rmail-summary-buffer (kill-buffer rmail-summary-buffer)))

(defvar rmail-enable-multibyte)         ; dynamically bound

;;;###autoload
(defun rmail-mode ()
  "Rmail Mode is used by \\<rmail-mode-map>\\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

\\[rmail-beginning-of-message]	Move point to front of this message.
\\[rmail-end-of-message]	Move point to bottom of this message.
\\[scroll-up]	Scroll to next screen of this message.
\\[scroll-down]	Scroll to previous screen of this message.
\\[rmail-next-undeleted-message]	Move to Next non-deleted message.
\\[rmail-previous-undeleted-message]	Move to Previous non-deleted message.
\\[rmail-next-message]	Move to Next message whether deleted or not.
\\[rmail-previous-message]	Move to Previous message whether deleted or not.
\\[rmail-first-message]	Move to the first message in Rmail file.
\\[rmail-last-message]	Move to the last message in Rmail file.
\\[rmail-show-message]	Jump to message specified by numeric position in file.
\\[rmail-search]	Search for string and show message it is found in.
\\[rmail-delete-forward]	Delete this message, move to next nondeleted.
\\[rmail-delete-backward]	Delete this message, move to previous nondeleted.
\\[rmail-undelete-previous-message]	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
\\[rmail-edit-current-message]	Edit the current message.  \\[rmail-cease-edit] to return to Rmail.
\\[rmail-expunge]	Expunge deleted messages.
\\[rmail-expunge-and-save]	Expunge and save the file.
\\[rmail-quit]       Quit Rmail: expunge, save, then switch to another buffer.
\\[save-buffer] Save without expunging.
\\[rmail-get-new-mail]	Move new mail from system spool directory into this file.
\\[rmail-mail]	Mail a message (same as \\[mail-other-window]).
\\[rmail-continue]	Continue composing outgoing message started before.
\\[rmail-reply]	Reply to this message.  Like \\[rmail-mail] but initializes some fields.
\\[rmail-retry-failure]	Send this message again.  Used on a mailer failure message.
\\[rmail-forward]	Forward this message to another user.
\\[rmail-output]	Output (append) this message to another mail file.
\\[rmail-output-as-seen]	Output (append) this message to file as it's displayed.
\\[rmail-output-body-to-file]	Save message body to a file.  Default filename comes from Subject line.
\\[rmail-input]	Input Rmail file.  Run Rmail on that file.
\\[rmail-add-label]	Add label to message.  It will be displayed in the mode line.
\\[rmail-kill-label]	Kill label.  Remove a label from current message.
\\[rmail-next-labeled-message]   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with \\[rmail-add-label].
\\[rmail-previous-labeled-message]   Move to Previous message with specified label
\\[rmail-summary]	Show headers buffer, with a one line summary of each message.
\\[rmail-summary-by-labels]	Summarize only messages with particular label(s).
\\[rmail-summary-by-recipients]   Summarize only messages with particular recipient(s).
\\[rmail-summary-by-regexp]   Summarize only messages with particular regexp(s).
\\[rmail-summary-by-topic]   Summarize only messages with subject line regexp(s).
\\[rmail-toggle-header]	Toggle display of complete header."
  (interactive)
  (let ((finding-rmail-file (not (eq major-mode 'rmail-mode))))
    (rmail-mode-2)
    (when (and finding-rmail-file
	       (null coding-system-for-read)
	       (default-value 'enable-multibyte-characters))
      (let ((rmail-enable-multibyte t))
	(rmail-require-mime-maybe)
	(rmail-convert-file-maybe)
	(goto-char (point-max))
	(set-buffer-multibyte t)))
    (rmail-set-message-counters)
    (rmail-show-message rmail-total-messages)
    (when finding-rmail-file
      (when rmail-display-summary
	(rmail-summary))
      (rmail-construct-io-menu))
    (run-mode-hooks 'rmail-mode-hook)))

(defun rmail-mode-2 ()
  (kill-all-local-variables)
  (rmail-mode-1)
  (rmail-perm-variables)
  (rmail-variables))

(defun rmail-mode-1 ()
  (setq major-mode 'rmail-mode)
  (setq mode-name "RMAIL")
  (setq buffer-read-only t)
  ;; No need to auto save RMAIL files in normal circumstances
  ;; because they contain no info except attribute changes
  ;; and deletion of messages.
  ;; The one exception is when messages are copied into another mbox buffer.
  ;; rmail-output enables auto save when you do that.
  (setq buffer-auto-save-file-name nil)
  (use-local-map rmail-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  ;; Functions to support buffer swapping:
  (add-hook 'write-region-annotate-functions
	    'rmail-write-region-annotate nil t)
  (add-hook 'kill-buffer-hook 'rmail-mode-kill-buffer-hook nil t)
  (add-hook 'change-major-mode-hook 'rmail-change-major-mode-hook nil t))

(defun rmail-generate-viewer-buffer ()
  "Return a reusable buffer suitable for viewing messages.
Create the buffer if necessary."
  ;; We want to reuse any existing view buffer, so as not to create an
  ;; endless number of them.  But we must avoid clashes if we visit
  ;; two different rmail files with the same basename (Bug#4593).
  (if (and (local-variable-p 'rmail-view-buffer)
	   (buffer-live-p rmail-view-buffer))
      rmail-view-buffer
    (let ((newbuf
	   (generate-new-buffer
	    (format " *message-viewer %s*"
		    (file-name-nondirectory
		     (or buffer-file-name (buffer-name)))))))
      (with-current-buffer newbuf
	(add-hook 'kill-buffer-hook 'rmail-view-buffer-kill-buffer-hook nil t))
      newbuf)))

(defun rmail-swap-buffers ()
  "Swap text between current buffer and `rmail-view-buffer'.
This function preserves the current buffer's modified flag, and also
sets the current buffer's `buffer-file-coding-system' to that of
`rmail-view-buffer'."
  (let ((modp-this (buffer-modified-p))
	(modp-that
	 (with-current-buffer rmail-view-buffer (buffer-modified-p)))
	(coding-this buffer-file-coding-system)
	(coding-that
	 (with-current-buffer rmail-view-buffer
	   buffer-file-coding-system)))
    (buffer-swap-text rmail-view-buffer)
    (setq buffer-file-coding-system coding-that)
    (with-current-buffer rmail-view-buffer
      (setq buffer-file-coding-system coding-this)
      (restore-buffer-modified-p modp-that))
    (restore-buffer-modified-p modp-this)))

(defun rmail-buffers-swapped-p ()
  "Return non-nil if the message collection is in `rmail-view-buffer'."
  ;; This is analogous to tar-data-swapped-p in tar-mode.el.
  rmail-buffer-swapped)

(defun rmail-change-major-mode-hook ()
  ;; Bring the actual Rmail messages back into the main buffer.
  (when (rmail-buffers-swapped-p)
    (rmail-swap-buffers)
    (setq rmail-buffer-swapped nil)))

(defun rmail-swap-buffers-maybe ()
  "Determine if the Rmail buffer is showing a message.
If so restore the actual mbox message collection."
  (with-current-buffer rmail-buffer
    (when (rmail-buffers-swapped-p)
      (rmail-swap-buffers)
      (setq rmail-buffer-swapped nil))))

(defun rmail-modify-format ()
  "Warn if important modifications would change Rmail file's format."
  (with-current-buffer rmail-buffer
    (and rmail-was-converted
	 ;; If it's already modified, don't warn again.
	 (not rmail-seriously-modified)
	 (not
	  (yes-or-no-p
	   (message "After this, %s would be saved in mbox format.  Proceed? "
		    (buffer-name))))
	 (error "Aborted"))
    (setq rmail-seriously-modified t)))

(defun rmail-dont-modify-format ()
  (when (and rmail-was-converted (not rmail-seriously-modified))
    (set-buffer-modified-p nil)
    (message "Marking buffer unmodified to avoid rewriting Babyl file as mbox file")))

(defun rmail-mode-kill-buffer-hook ()
  ;; Turn off the hook on the view buffer, so we can kill it, then kill it.
  (if (buffer-live-p rmail-view-buffer)
      (with-current-buffer rmail-view-buffer
	(setq kill-buffer-hook nil)
	(kill-buffer rmail-view-buffer))))

(defun rmail-view-buffer-kill-buffer-hook ()
  (error "Can't kill Rmail view buffer `%s' by itself"
	 (buffer-name (current-buffer))))

;; Set up the permanent locals associated with an Rmail file.
(defun rmail-perm-variables ()
  (make-local-variable 'rmail-last-regexp)
  (make-local-variable 'rmail-deleted-vector)
  (make-local-variable 'rmail-buffer)
  (make-local-variable 'rmail-was-converted)
  (setq rmail-was-converted nil)
  (make-local-variable 'rmail-seriously-modified)
  (setq rmail-seriously-modified nil)
  (setq rmail-buffer (current-buffer))
  (set-buffer-multibyte nil)
  (with-current-buffer (setq rmail-view-buffer (rmail-generate-viewer-buffer))
    (setq buffer-undo-list t)
    ;; Note that this does not erase the buffer.  Should it?
    ;; It depends on how this is called.  If somehow called with the
    ;; rmail buffers swapped, it would erase the message collection.
    (set (make-local-variable 'rmail-overlay-list) nil)
    (set-buffer-multibyte t)
    ;; Force C-x C-s write Unix EOLs.
    (set-buffer-file-coding-system 'undecided-unix))
  (make-local-variable 'rmail-summary-buffer)
  (make-local-variable 'rmail-summary-vector)
  (make-local-variable 'rmail-current-message)
  (make-local-variable 'rmail-total-messages)
  (setq rmail-total-messages 0)
  (make-local-variable 'rmail-message-vector)
  (make-local-variable 'rmail-msgref-vector)
  (make-local-variable 'rmail-inbox-list)
  ;; Provide default set of inboxes for primary mail file ~/RMAIL.
  (and (null rmail-inbox-list)
       (or (equal buffer-file-name (expand-file-name rmail-file-name))
	   (equal buffer-file-truename
		  (abbreviate-file-name (file-truename rmail-file-name))))
       (setq rmail-inbox-list
	     (or rmail-primary-inbox-list
		 (list (or (getenv "MAIL")
			   ;; FIXME expand-file-name?
			   (concat rmail-spool-directory
				   (user-login-name)))))))
  (set (make-local-variable 'tool-bar-map) rmail-tool-bar-map))

;; Set up the non-permanent locals associated with Rmail mode.
(defun rmail-variables ()
  ;; Turn off undo.  We turn it back on in rmail-edit.
  (setq buffer-undo-list t)
  ;; Don't let a local variables list in a message cause confusion.
  (make-local-variable 'local-enable-local-variables)
  (setq local-enable-local-variables nil)
  ;; Don't turn off auto-saving based on the size of the buffer
  ;; because that code does not understand buffer-swapping.
  (make-local-variable 'auto-save-include-big-deletions)
  (setq auto-save-include-big-deletions t)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'rmail-revert)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(rmail-font-lock-keywords
	  t t nil nil
	  (font-lock-maximum-size . nil)
	  (font-lock-fontify-buffer-function . rmail-fontify-buffer-function)
	  (font-lock-unfontify-buffer-function . rmail-unfontify-buffer-function)
	  (font-lock-inhibit-thing-lock . (lazy-lock-mode fast-lock-mode))))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'rmail-mode-kill-summary)
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag t)
  (make-local-variable 'desktop-save-buffer)
  (setq desktop-save-buffer t)
  (setq next-error-move-function 'rmail-next-error-move))

;; Handle M-x revert-buffer done in an rmail-mode buffer.
(defun rmail-revert (arg noconfirm)
  (set-buffer rmail-buffer)
  (let* ((revert-buffer-function (default-value 'revert-buffer-function))
	 (rmail-enable-multibyte enable-multibyte-characters)
	 ;; See similar code in `rmail'.
	 ;; FIXME needs updating?
	 (coding-system-for-read (and rmail-enable-multibyte 'raw-text))
	 (before-revert-hook 'rmail-swap-buffers-maybe))
    ;; Call our caller again, but this time it does the default thing.
    (when (revert-buffer arg noconfirm)
      ;; If the user said "yes", and we changed something,
      ;; reparse the messages.
      (set-buffer rmail-buffer)
      (rmail-mode-2)
      ;; Convert all or part to Babyl file if possible.
      (rmail-convert-file-maybe)
      ;; We have read the file as raw-text, so the buffer is set to
      ;; unibyte.  Make it multibyte if necessary.
      (if (and rmail-enable-multibyte
	       (not enable-multibyte-characters))
	  (set-buffer-multibyte t))
      (goto-char (point-max))
      (rmail-set-message-counters)
      (rmail-show-message rmail-total-messages)
      (run-hooks 'rmail-mode-hook))))

(defun rmail-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-expunge)
  ;; No need to swap buffers: rmail-write-region-annotate takes care of it.
  ;; (rmail-swap-buffers-maybe)
  (save-buffer)
  (if (rmail-summary-exists)
      (rmail-select-summary (set-buffer-modified-p nil))))

(defun rmail-quit ()
  "Quit out of RMAIL.
Hook `rmail-quit-hook' is run after expunging."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-expunge t)
  (save-buffer)
  (when (boundp 'rmail-quit-hook)
    (run-hooks 'rmail-quit-hook))
  ;; Don't switch to the summary buffer even if it was recently visible.
  (when rmail-summary-buffer
    (with-current-buffer rmail-summary-buffer
      (set-buffer-modified-p nil))
    (replace-buffer-in-windows rmail-summary-buffer)
    (bury-buffer rmail-summary-buffer))
  (let ((obuf (current-buffer)))
    (quit-window)
    (replace-buffer-in-windows obuf)))

(defun rmail-bury ()
  "Bury current Rmail buffer and its summary buffer."
  (interactive)
  ;; This let var was called rmail-buffer, but that interfered
  ;; with the buffer-local var used in summary buffers.
  (let ((buffer-to-bury (current-buffer)))
    (if (rmail-summary-exists)
	(let (window)
	  (while (setq window (get-buffer-window rmail-summary-buffer))
	    (quit-window nil window))
	  (bury-buffer rmail-summary-buffer)))
    (quit-window)))

(defun rmail-duplicate-message ()
  "Create a duplicated copy of the current message.
The duplicate copy goes into the Rmail file just after the original."
  ;; If we are in a summary buffer, switch to the Rmail buffer.
  ;; FIXME simpler to swap the contents, not the buffers?
  (set-buffer rmail-buffer)
  (rmail-modify-format)
  (let ((buff (current-buffer))
        (n rmail-current-message)
        (beg (rmail-msgbeg rmail-current-message))
        (end (rmail-msgend rmail-current-message)))
    (if (rmail-buffers-swapped-p) (set-buffer rmail-view-buffer))
      (widen)
      (let ((buffer-read-only nil)
          (string (buffer-substring-no-properties beg end)))
      (goto-char end)
      (insert string))
    (set-buffer buff)
    (rmail-swap-buffers-maybe)
    (goto-char (point-max))
    (rmail-set-message-counters)
    (set-buffer-modified-p t)
    (rmail-show-message-1 n))
  (if (rmail-summary-exists)
      (rmail-select-summary (rmail-update-summary)))
  (message "Message duplicated"))

;;;###autoload
(defun rmail-input (filename)
  "Run Rmail on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  (rmail filename))

;; This used to scan subdirectories recursively, but someone pointed out
;; that if the user wants that, person can put all the files in one dir.
;; And the recursive scan was slow.  So I took it out.
;; rms, Sep 1996.
(defun rmail-find-all-files (start)
  "Return list of file in dir START that match `rmail-secondary-file-regexp'."
  (if (file-accessible-directory-p start)
      ;; Don't sort here.
      (let* ((case-fold-search t)
	     (files (directory-files start t rmail-secondary-file-regexp)))
	;; Sort here instead of in directory-files
	;; because this list is usually much shorter.
	(sort files 'string<))))

(defun rmail-list-to-menu (menu-name l action &optional full-name)
  (let ((menu (make-sparse-keymap menu-name))
	name)
    (mapc
     (lambda (item)
       (let (command)
	 (if (consp item)
	     (setq command
		   (rmail-list-to-menu
		    (car item) (cdr item) action
		    (if full-name
			(concat full-name "/"
				(car item))
		      (car item)))
		   name (car item))
	   (setq name item)
	   (setq command
		 (list 'lambda () '(interactive)
		       (list action
			     (expand-file-name
			      (if full-name
				  (concat full-name "/" item)
				item)
			      rmail-secondary-file-directory)))))
	 (define-key menu (vector (intern name))
	   (cons name command))))
     (reverse l))
    menu))

;; This command is always "disabled" when it appears in a menu.
(put 'rmail-disable-menu 'menu-enable ''nil)

(defun rmail-construct-io-menu ()
  (let ((files (rmail-find-all-files rmail-secondary-file-directory)))
    (if files
	(progn
	  (define-key rmail-mode-map [menu-bar classify input-menu]
	    (cons "Input Rmail File"
		  (rmail-list-to-menu "Input Rmail File"
				      files
				      'rmail-input)))
	  (define-key rmail-mode-map [menu-bar classify output-menu]
	    (cons "Output Rmail File"
		  (rmail-list-to-menu "Output Rmail File"
				      files
				      'rmail-output))))

      (define-key rmail-mode-map [menu-bar classify input-menu]
	'("Input Rmail File" . rmail-disable-menu))
      (define-key rmail-mode-map [menu-bar classify output-menu]
	'("Output Rmail File" . rmail-disable-menu)))))


;;;; *** Rmail input ***

(declare-function rmail-summary-goto-msg "rmailsum" (&optional n nowarn skip-rmail))
(declare-function rmail-summary-mark-undeleted "rmailsum" (n))
(declare-function rmail-summary-mark-deleted "rmailsum" (&optional n undel))
(declare-function rfc822-addresses "rfc822" (header-text))
(declare-function mail-abbrev-make-syntax-table "mailabbrev.el" ())

;; RLK feature not added in this version:
;; argument specifies inbox file or files in various ways.

;; In Babyl, the Mail: header in the preamble overrode rmail-inbox-list.
;; Mbox does not have this feature.
(defun rmail-get-new-mail (&optional file-name)
  "Move any new mail from this Rmail file's inbox files.
The buffer-local variable `rmail-inbox-list' specifies the list
of inbox files.  By default, this is nil, except for your primary
Rmail file `rmail-file-name'.  In this case, when you first visit
the Rmail file it is initialized using either
`rmail-primary-inbox-list', or the \"MAIL\" environment variable,
or the function `user-login-name' and the directory
`rmail-spool-directory' (whose value depends on the operating system).

The command `set-rmail-inbox-list' sets `rmail-inbox-list' to the
value you specify.

You can also specify the file to get new mail from just for one
instance of this command.  In this case, the file of new mail is
not changed or deleted.  Noninteractively, you can pass the inbox
file name as an argument.  Interactively, a prefix argument
causes us to read a file name and use that file as the inbox.

If the variable `rmail-preserve-inbox' is non-nil, new mail will
always be left in inbox files rather than deleted.

Before doing anything, this runs `rmail-before-get-new-mail-hook'.
Just before returning, it runs `rmail-after-get-new-mail-hook',
whether or not there is new mail.

If there is new mail, it runs `rmail-get-new-mail-hook', saves
the updated file, and shows the first unseen message (which might
not be a new one).  It returns non-nil if it got any new messages."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (run-hooks 'rmail-before-get-new-mail-hook)
  ;; If the disk file has been changed from under us,
  ;; revert to it before we get new mail.
  (or (verify-visited-file-modtime (current-buffer))
      (find-file (buffer-file-name)))
  (set-buffer rmail-buffer)
  (rmail-modify-format)
  (rmail-swap-buffers-maybe)
  (rmail-maybe-set-message-counters)
  (widen)
  ;; Get rid of all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (let ((all-files (if file-name (list file-name) rmail-inbox-list))
	(rmail-enable-multibyte (default-value 'enable-multibyte-characters))
	found)
    (unwind-protect
	(progn
	  ;; This loops if any members of the inbox list have the same
	  ;; basename (see "name conflict" below).
	  (while all-files
	    (let ((opoint (point))
		  ;; If buffer has not changed yet, and has not been
		  ;; saved yet, don't replace the old backup file now.
		  (make-backup-files (and make-backup-files
					  (buffer-modified-p)))
		  (buffer-read-only nil)
		  ;; Don't make undo records while getting mail.
		  (buffer-undo-list t)
		  delete-files success files file-last-names)
	      ;; Pull files off all-files onto files as long as there is
	      ;; no name conflict.  A conflict happens when two inbox
	      ;; file names have the same last component.
	      ;; The reason this careful handling is necessary seems
	      ;; to be that rmail-insert-inbox-text uses .newmail-BASENAME.
	      (while (and all-files
			  (not (member (file-name-nondirectory (car all-files))
				file-last-names)))
		(setq files (cons (car all-files) files)
		      file-last-names
		      (cons (file-name-nondirectory (car all-files)) files))
		(setq all-files (cdr all-files)))
	      ;; Put them back in their original order.
	      (setq files (nreverse files))
	      (goto-char (point-max))
	      ;; Make sure we end with a blank line unless there are
	      ;; no messages, as required by mbox format (Bug#9974).
	      (unless (bobp)
		(while (not (looking-back "\n\n"))
		  (insert "\n")))
	      (setq found (or
			   (rmail-get-new-mail-1 file-name files delete-files)
			   found))))
	  ;; Move to the first new message unless we have other unseen
	  ;; messages before it.
	  (if found (rmail-show-message (rmail-first-unseen-message)))
	  (run-hooks 'rmail-after-get-new-mail-hook)
	  found)
      ;; Don't leave the buffer screwed up if we get a disk-full error.
      (rmail-show-message))))

(defvar rmail-use-spam-filter)
(declare-function rmail-get-new-mail-filter-spam "rmail-spam-filter" (nnew))

(defun rmail-get-new-mail-1 (file-name files delete-files)
  "Return t if new messages are detected without error, nil otherwise."
  (save-excursion
    (save-restriction
      (let ((new-messages 0)
	    (spam-filter-p (and (featurep 'rmail-spam-filter)
				rmail-use-spam-filter))
	    (blurb "")
	    result success suffix)
	(narrow-to-region (point) (point))
	;; Read in the contents of the inbox files, renaming them as
	;; necessary, and adding to the list of files to delete
	;; eventually.
	(if file-name
	    (rmail-insert-inbox-text files nil)
	  (setq delete-files (rmail-insert-inbox-text files t)))
	;; Scan the new text and convert each message to
	;; Rmail/mbox format.
	(goto-char (point-min))
	(skip-chars-forward " \n")
	(narrow-to-region (point) (point-max))
	(unwind-protect
	    (setq new-messages (rmail-add-mbox-headers)
		  success t)
	  ;; Try to delete the garbage just inserted.
	  (or success (delete-region (point-min) (point-max)))
	  ;; If we could not convert the file's inboxes, rename the
	  ;; files we tried to read so we won't over and over again.
	  (if (and (not file-name) (not success))
	      (let ((delfiles delete-files)
		    (count 0))
		(while delfiles
		  (while (file-exists-p (format "RMAILOSE.%d" count))
		    (setq count (1+ count)))
		  (rename-file (car delfiles) (format "RMAILOSE.%d" count))
		  (setq delfiles (cdr delfiles))))))
	;; Determine if there are messages.
	(unless (zerop new-messages)
	  ;; There are.  Process them.
	  (goto-char (point-min))
	  (rmail-count-new-messages)
	  (run-hooks 'rmail-get-new-mail-hook)
	  (save-buffer))
	;; Delete the old files, now that the Rmail file is saved.
	(while delete-files
	  (condition-case ()
	      ;; First, try deleting.
	      (condition-case ()
		  (delete-file (car delete-files))
		(file-error
		 ;; If we can't delete it, truncate it.
		 (write-region (point) (point) (car delete-files))))
	    (file-error nil))
	  (setq delete-files (cdr delete-files)))
	(if (zerop new-messages)
	    (when (or file-name rmail-inbox-list)
	      (message "(No new mail has arrived)"))
	  (if spam-filter-p
	      (setq blurb (rmail-get-new-mail-filter-spam new-messages))))
	(if (rmail-summary-exists)
	    (rmail-select-summary (rmail-update-summary)))
	(setq suffix (if (= 1 new-messages) "" "s"))
	(message "%d new message%s read%s" new-messages suffix blurb)
	;; Establish the return value.
	(setq result (> new-messages 0))
	result))))

(defun rmail-parse-url (file)
  "Parse the supplied URL. Return (list MAILBOX-NAME REMOTE PASSWORD GOT-PASSWORD)
WHERE MAILBOX-NAME is the name of the mailbox suitable as argument to the
actual version of `movemail', REMOTE is non-nil if MAILBOX-NAME refers to
a remote mailbox, PASSWORD is the password if it should be
supplied as a separate argument to `movemail' or nil otherwise, GOT-PASSWORD
is non-nil if the user has supplied the password interactively.
"
  (cond
   ((string-match "^\\([^:]+\\)://\\(\\([^:@]+\\)\\(:\\([^@]+\\)\\)?@\\)?.*" file)
      (let (got-password supplied-password
	    (proto (match-string 1 file))
	    (user  (match-string 3 file))
	    (pass  (match-string 5 file))
	    (host  (substring file (or (match-end 2)
				       (+ 3 (match-end 1))))))

	(if (not pass)
	    (when rmail-remote-password-required
	      (setq got-password (not (rmail-have-password)))
	      (setq supplied-password (rmail-get-remote-password
				       (string-equal proto "imap"))))
	  ;; The password is embedded.  Strip it out since movemail
	  ;; does not really like it, in spite of the movemail spec.
	  (setq file (concat proto "://" user "@" host)))

	(if (rmail-movemail-variant-p 'emacs)
	    (if (string-equal proto "pop")
		(list (concat "po:" user ":" host)
		      t
		      (or pass supplied-password)
		      got-password)
	      (error "Emacs movemail does not support %s protocol" proto))
	  (list file
		(or (string-equal proto "pop") (string-equal proto "imap"))
		(or supplied-password pass)
		got-password))))

   ((string-match "^po:\\([^:]+\\)\\(:\\(.*\\)\\)?" file)
    (let (got-password supplied-password
          (proto "pop")
	  (user  (match-string 1 file))
	  (host  (match-string 3 file)))

      (when rmail-remote-password-required
	(setq got-password (not (rmail-have-password)))
	(setq supplied-password (rmail-get-remote-password nil)))

      (list file "pop" supplied-password got-password)))

   (t
    (list file nil nil nil))))

(defun rmail-unrmail-new-mail (from-file)
  "Replace newly read mail in Babyl format with equivalent mbox format.

FROM-FILE is the Babyl file from which the new mail should be read."
  (let ((to-file (make-temp-file "rmail"))
	size)
    (unrmail from-file to-file)
    (let ((inhibit-read-only t)
	  (coding-system-for-read 'raw-text)
	  (buffer-undo-list t))
      (delete-region (point) (point-max))
      (setq size (nth 1 (insert-file-contents to-file)))
      (delete-file to-file)
      size)))

(defun rmail-unrmail-new-mail-maybe (file size)
  "If newly read mail from FILE is in Babyl format, convert it to mbox format.

SIZE is the original size of the newly read mail.
Value is the size of the newly read mail after conversion."
  ;; Detect previous Babyl format files.
  (let ((case-fold-search nil)
	(old-file file)
	new-file)
    (cond ((looking-at "BABYL OPTIONS:")
	   ;; The new mail is in Babyl version 5 format.  Use unrmail
	   ;; to convert it.
	   (setq size (rmail-unrmail-new-mail old-file)))
	  ((looking-at "Version: 5\n")
	   ;; New mail is in Babyl format made by old version of
	   ;; Rmail.  Fix the babyl file header and use unrmail to
	   ;; convert it.
	   (let ((buffer-read-only nil)
		 (write-region-annotate-functions nil)
		 (write-region-post-annotation-function nil)
		 (old-file  (make-temp-file "rmail")))
	     (insert "BABYL OPTIONS: -*- rmail -*-\n")
	     (forward-line -1)
	     (write-region (point) (point-max) old-file)
	     (setq size (rmail-unrmail-new-mail old-file))
	     (delete-file old-file))))
    size))

(defun rmail-insert-inbox-text (files renamep)
  ;; Detect a locked file now, so that we avoid moving mail
  ;; out of the real inbox file.  (That could scare people.)
  (or (memq (file-locked-p buffer-file-name) '(nil t))
      (error "RMAIL file %s is locked"
	     (file-name-nondirectory buffer-file-name)))
  (let (file tofile delete-files movemail popmail got-password password)
    (while files
      ;; Handle remote mailbox names specially; don't expand as filenames
      ;; in case the userid contains a directory separator.
      (setq file (car files))
      (let ((url-data (rmail-parse-url file)))
	(setq file (nth 0 url-data))
	(setq popmail (nth 1 url-data))
	(setq password (nth 2 url-data))
	(setq got-password (nth 3 url-data)))

      (if popmail
	  (setq renamep t)
	(setq file (file-truename
		    (substitute-in-file-name (expand-file-name file)))))
      (setq tofile (expand-file-name
		    ;; Generate name to move to from inbox name,
		    ;; in case of multiple inboxes that need moving.
		    (concat ".newmail-"
			    (file-name-nondirectory
			     (if (memq system-type '(windows-nt cygwin ms-dos))
				 ;; cannot have colons in file name
				 (replace-regexp-in-string ":" "-" file)
			       file)))
		    ;; Use the directory of this rmail file
		    ;; because it's a nuisance to use the homedir
		    ;; if that is on a full disk and this rmail
		    ;; file isn't.
		    (file-name-directory
		     (expand-file-name buffer-file-name))))
      ;; Always use movemail to rename the file,
      ;; since there can be mailboxes in various directories.
      (when (not popmail)
	;; On some systems, /usr/spool/mail/foo is a directory
	;; and the actual inbox is /usr/spool/mail/foo/foo.
	(if (file-directory-p file)
	    (setq file (expand-file-name (user-login-name)
					 file))))
      (cond (popmail
	     (message "Getting mail from the remote server ..."))
	    ((and (file-exists-p tofile)
		  (/= 0 (nth 7 (file-attributes tofile))))
	     (message "Getting mail from %s..." tofile))
	    ((and (file-exists-p file)
		  (/= 0 (nth 7 (file-attributes file))))
	     (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (and (not popmail)
					     (not (file-exists-p file))))
	     nil)
	    (t
	     (with-temp-buffer
	       (let ((errors (current-buffer)))
		 (buffer-disable-undo errors)
		 (let ((args
			(append
			 (list (or rmail-movemail-program "movemail") nil errors nil)
			 (if rmail-preserve-inbox
			     (list "-p")
			   nil)
			 (if (rmail-movemail-variant-p 'mailutils)
			     (append (list "--emacs") rmail-movemail-flags)
			   rmail-movemail-flags)
			 (list file tofile)
			 (if password (list password) nil))))
		   (apply 'call-process args))
		 (if (not (buffer-modified-p errors))
		     ;; No output => movemail won
		     nil
		   (set-buffer errors)
		   (subst-char-in-region (point-min) (point-max)
					 ?\n ?\s)
		   (goto-char (point-max))
		   (skip-chars-backward " \t")
		   (delete-region (point) (point-max))
		   (goto-char (point-min))
		   (if (looking-at "movemail: ")
		       (delete-region (point-min) (match-end 0)))
		   (beep t)
		   ;; If we just read the password, most likely it is
		   ;; wrong.  Otherwise, see if there is a specific
		   ;; reason to think that the problem is a wrong passwd.
		   (if (or got-password
			   (re-search-forward rmail-remote-password-error
					      nil t))
		       (rmail-set-remote-password nil))

		   ;; If using Mailutils, remove initial error code
		   ;; abbreviation
		   (when (rmail-movemail-variant-p 'mailutils)
		     (goto-char (point-min))
		     (when (looking-at "[A-Z][A-Z0-9_]*:")
		       (delete-region (point-min) (match-end 0))))

		   (message "movemail: %s"
			    (buffer-substring (point-min)
					      (point-max)))

		   (sit-for 3)
		   nil)))))

      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (let ((coding-system-for-read 'no-conversion)
		size)
	    (goto-char (point-max))
	    (setq size
		  ;; If new mail is in Babyl format, convert it to mbox.
		  (rmail-unrmail-new-mail-maybe
		   tofile
		   (nth 1 (insert-file-contents tofile))))
	    (goto-char (point-max))
	    ;; Make sure the read-in mbox data properly ends with a
	    ;; blank line unless it is of size 0.
	    (unless (zerop size)
	      (while (not (looking-back "\n\n"))
		(insert "\n")))
	    (if (not (and rmail-preserve-inbox (string= file tofile)))
		(setq delete-files (cons tofile delete-files)))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;; Decode the region specified by FROM and TO by CODING.
;; If CODING is nil or an invalid coding system, decode by `undecided'.
(defun rmail-decode-region (from to coding &optional destination)
  (if (or (not coding) (not (coding-system-p coding)))
      (setq coding 'undecided))
  ;; Use -dos decoding, to remove ^M characters left from base64 or
  ;; rogue qp-encoded text.
  (decode-coding-region
   from to (coding-system-change-eol-conversion coding 1) destination)
  ;; Don't reveal the fact we used -dos decoding, as users generally
  ;; will not expect the RMAIL buffer to use DOS EOL format.
  (cond
   ((null destination)
    (setq buffer-file-coding-system
	  (setq last-coding-system-used
		(coding-system-change-eol-conversion coding 0))))
   ((bufferp destination)
    (with-current-buffer destination
      (setq buffer-file-coding-system
	    (setq last-coding-system-used
		  (coding-system-change-eol-conversion coding 0)))))))

(defun rmail-ensure-blank-line ()
  "Ensure a message ends in a blank line.
Call with point at the end of the message."
  (unless (bolp)
    (insert "\n"))
  (unless (looking-back "\n\n")
    (insert "\n")))

(defun rmail-add-mbox-headers ()
  "Validate the RFC2822 format for the new messages.
Point should be at the first new message.
An error is signaled if the new messages are not RFC2822
compliant.
Unless an Rmail attribute header already exists, add it to the
new messages.  Return the number of new messages."
  (save-excursion
    (save-restriction
      (let ((count 0)
	    (start (point))
	    (value "------U-")
	    (case-fold-search nil)
	    (delim (concat "\n\n" rmail-unix-mail-delimiter))
	    limit stop)
	;; Detect an empty inbox file.
	(unless (= start (point-max))
	  ;; Scan the new messages to establish a count and to ensure that
	  ;; an attribute header is present.
	  (if (looking-at rmail-unix-mail-delimiter)
	      (while (not stop)
		;; Determine if a new attribute header needs to be
		;; added to the message.
		(if (search-forward "\n\n" nil t)
		    (progn
		      (setq count (1+ count))
		      (narrow-to-region start (point))
		      (unless (mail-fetch-field rmail-attribute-header)
			(backward-char 1)
			(insert rmail-attribute-header ": " value "\n"))
		      (widen))
		  (rmail-error-bad-format))
		;; Move to the next message.
		(if (not (re-search-forward delim nil 'move))
		    (setq stop t)
		  (goto-char (match-beginning 0))
		  (forward-char 2))
		(setq start (point)))
	    (rmail-error-bad-format)))
	count))))

(defun rmail-get-header-1 (name)
  "Subroutine of `rmail-get-header'.
Narrow to header, call `mail-fetch-field' to find header NAME."
  (if (search-forward "\n\n" nil t)
      (progn
        (narrow-to-region (point-min) (point))
        (mail-fetch-field name))
    (rmail-error-bad-format)))

(defun rmail-get-header (name &optional msgnum)
  "Return the value of message header NAME, nil if it has none.
MSGNUM specifies the message number to get it from.
If MSGNUM is nil, use the current message."
  (rmail-apply-in-message msgnum 'rmail-get-header-1 name))

(defun rmail-set-header-1 (name value)
  "Subroutine of `rmail-set-header'.
Narrow to header, set header NAME to VALUE, replacing existing if present.
VALUE nil means to remove NAME altogether."
  (if (search-forward "\n\n" nil t)
      (progn
	(forward-char -1)
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(if (re-search-forward (concat "^" (regexp-quote name) ":") nil 'move)
            (if value
                (progn
                  (delete-region (point) (line-end-position))
                  (insert " " value))
              (delete-region (line-beginning-position)
                             (line-beginning-position 2)))
          (if value (insert name ": " value "\n"))))
    (rmail-error-bad-format)))

(defun rmail-set-header (name &optional msgnum value)
  "Set message header NAME to VALUE in message number MSGNUM.
If MSGNUM is nil, use the current message.  NAME and VALUE are strings.
VALUE may also be nil, meaning to remove the header."
  (rmail-apply-in-message msgnum 'rmail-set-header-1 name value)
  (with-current-buffer rmail-buffer
    ;; Ensure header changes get saved.
    ;; (Note replacing a header with an identical copy modifies.)
    (set-buffer-modified-p t)
    ;; However: don't save in mbox format over a Babyl file
    ;; merely because of this.
    (rmail-dont-modify-format)))

;;;; *** Rmail Attributes and Keywords ***

(defun rmail-get-attr-names (&optional msg)
  "Return the message attributes in a comma separated string.
MSG specifies the message number to get it from.
If MSG is nil, use the current message."
  (let ((value (rmail-get-header rmail-attribute-header msg))
	(nmax (length rmail-attr-array))
	result temp)
    (when value
      (if (> (length value) nmax)
          (message "Warning: corrupt attribute header in message")
        (dotimes (index (length value))
          (setq temp (and (not (= ?- (aref value index)))
                          (nth 1 (aref rmail-attr-array index)))
                result
                (cond
                 ((and temp result) (format "%s, %s" result temp))
                 (temp temp)
                 (t result)))))
      result)))

(defun rmail-get-keywords (&optional msg)
  "Return the message keywords in a comma separated string.
MSG, if non-nil, identifies the message number to use.
If nil, that means the current message."
  (rmail-get-header rmail-keyword-header msg))

(defun rmail-get-labels (&optional msg)
  "Return a string with the labels (attributes and keywords) of msg MSG.
It is put in comma-separated form.
MSG, if non-nil, identifies the message number to use.
If nil, that means the current message."
  (or msg (setq msg rmail-current-message))
  (let (attr-names keywords)
    ;; Combine the message attributes and keywords
    ;; into a comma-separated list.
    (setq attr-names (rmail-get-attr-names msg)
	  keywords (rmail-get-keywords msg))
    (if (string= keywords "")
	(setq keywords nil))
    (cond
     ;; FIXME ? old rmail did not have spaces in the comma-separated lists.
     ((and attr-names keywords) (concat " " attr-names "; " keywords))
     (attr-names (concat " " attr-names))
     (keywords (concat " " keywords))
     (t ""))))

(defun rmail-display-labels ()
  "Update the current messages's attributes and keywords in mode line."
  (let ((blurb (rmail-get-labels)))
    (setq mode-line-process
	  (format " %d/%d%s"
		  rmail-current-message rmail-total-messages blurb))))

(defun rmail-get-attr-value (attr state)
  "Return the character value for ATTR.
ATTR is a (numeric) index, an offset into the mbox attribute
header value. STATE is one of nil, t, or a character value."
  (cond
   ((numberp state) state)
   ((not state) ?-)
   (t (nth 0 (aref rmail-attr-array attr)))))

(defun rmail-set-attribute-1 (attr state)
  "Subroutine of `rmail-set-attribute'.
Set Rmail attribute ATTR to STATE in `rmail-attribute-header',
creating the header if necessary.  Returns non-nil if a
significant attribute change was made."
  (let ((limit (search-forward "\n\n" nil t))
        (value (rmail-get-attr-value attr state))
        (inhibit-read-only t)
        altered)
    (goto-char (point-min))
    (if (search-forward (concat rmail-attribute-header ": ") limit t)
        ;; If this message already records attributes, just change the
        ;; value for this one.
        (let ((missing (- (+ (point) attr) (line-end-position))))
          ;; Position point at this attribute, adding attributes if necessary.
          (if (> missing 0)
              (progn
                (end-of-line)
                (insert-char ?- missing)
                (backward-char 1))
            (forward-char attr))
          ;; Change this attribute.
          (when (/= value (char-after))
            (setq altered t)
            (delete-char 1)
            (insert value)))
      ;; Otherwise add a header line to record the attributes and set
      ;; all but this one to no.
      (let ((header-value "--------"))
        (aset header-value attr value)
        (goto-char (if limit (1- limit) (point-max)))
        (setq altered (/= value ?-))
        (insert rmail-attribute-header ": " header-value "\n")))
    altered))

(defun rmail-set-attribute (attr state &optional msgnum)
  "Turn an attribute of a message on or off according to STATE.
STATE is either nil or the character (numeric) value associated
with the state (nil represents off and non-nil represents on).
ATTR is either the index number of the attribute, or a string,
both from `rmail-attr-array'.  MSGNUM is message number to
change; nil means current message."
  (let ((n 0)
        (nmax (length rmail-attr-array)))
    (while (and (stringp attr)
                (< n nmax))
      (if (string-equal attr (cadr (aref rmail-attr-array n)))
          (setq attr n))
      (setq n (1+ n))))
  (if (stringp attr)
      (error "Unknown attribute `%s'" attr))
  ;; Ask for confirmation before setting any attribute except `unseen'
  ;; if it would force a format change.
  (unless (= attr rmail-unseen-attr-index)
    (rmail-modify-format))
  (with-current-buffer rmail-buffer
    (or msgnum (setq msgnum rmail-current-message))
    (when (> msgnum 0)
      ;; The "deleted" attribute is also stored in a special vector so
      ;; update that too.
      (if (= attr rmail-deleted-attr-index)
          (rmail-set-message-deleted-p msgnum state))
      (if (prog1
              (rmail-apply-in-message msgnum 'rmail-set-attribute-1 attr state)
            (if (= msgnum rmail-current-message)
                (rmail-display-labels)))
	  ;; Don't save in mbox format over a Babyl file
	  ;; merely because of a change in `unseen' attribute.
	  (if (= attr rmail-unseen-attr-index)
	      (rmail-dont-modify-format)
	    ;; Otherwise, if we modified the file text via the view buffer,
	    ;; mark the main buffer modified too.
	    (set-buffer-modified-p t))))))

(defun rmail-message-attr-p (msg attrs)
  "Return non-nil if message number MSG has attributes matching regexp ATTRS."
  (let ((value (rmail-get-header rmail-attribute-header msg)))
    (and value (string-match attrs value))))

(defun rmail-message-unseen-p (msgnum)
  "Return non-nil if message number MSGNUM has the unseen attribute."
  (rmail-message-attr-p msgnum "......U"))

;; FIXME rmail-get-labels does some formatting (eg leading space, `;'
;; between attributes and labels), so this might not do what you want.
;; Eg see rmail-sort-by-labels.  rmail-get-labels could have an
;; optional `noformat' argument.
(defun rmail-message-labels-p (msg labels)
  "Return non-nil if message number MSG has labels matching regexp LABELS."
  (string-match labels (rmail-get-labels msg)))

;;;; *** Rmail Message Selection And Support ***

(defun rmail-msgend (n)
  "Return the end position for message number N."
  (marker-position (aref rmail-message-vector (1+ n))))

(defun rmail-msgbeg (n)
  "Return the start position for message number N."
  (marker-position (aref rmail-message-vector n)))

(defun rmail-apply-in-message (msgnum function &rest args)
  "Call FUNCTION on ARGS while narrowed to message MSGNUM.
Point is at the start of the message.
This returns what the call to FUNCTION returns.
If MSGNUM is nil, use the current message."
  (with-current-buffer rmail-buffer
    (or msgnum (setq msgnum rmail-current-message))
    (when (> msgnum 0)
      (let (msgbeg msgend)
	(setq msgbeg (rmail-msgbeg msgnum))
	(setq msgend (rmail-msgend msgnum))
	;; All access to the rmail-buffer's local variables is now finished...
	(save-excursion
	  ;; ... so it is ok to go to a different buffer.
	  (if (rmail-buffers-swapped-p) (set-buffer rmail-view-buffer))
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char msgbeg)
	      (narrow-to-region msgbeg msgend)
	      (apply function args))))))))

;; Unused (save for commented out code in rmailedit.el).
(defun rmail-widen-to-current-msgbeg (function)
  "Call FUNCTION with point at start of internal data of current message.
Assumes that bounds were previously narrowed to display the message in Rmail.
The bounds are widened enough to move point where desired, then narrowed
again afterward.

FUNCTION may not change the visible text of the message, but it may
change the invisible header text."
  (save-excursion
    (unwind-protect
	(progn
	  (narrow-to-region (rmail-msgbeg rmail-current-message)
			    (point-max))
	  (goto-char (point-min))
	  (funcall function))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
      (narrow-to-region (rmail-msgbeg rmail-current-message)
			(rmail-msgend rmail-current-message)))))

;; Manage the message vectors and counters.

(defun rmail-forget-messages ()
  (unwind-protect
      (if (vectorp rmail-message-vector)
	  (let* ((v rmail-message-vector)
		 (n (length v)))
	    (dotimes (i n)
	      (if (aref v i)
		  (move-marker (aref v i)  nil)))))
    (setq rmail-message-vector nil)
    (setq rmail-msgref-vector nil)
    (setq rmail-deleted-vector nil)))

(defun rmail-maybe-set-message-counters ()
  (if (not (and rmail-deleted-vector
		rmail-message-vector
		rmail-current-message
		rmail-total-messages))
      (rmail-set-message-counters)))

(defun rmail-count-new-messages (&optional nomsg)
  "Count the number of new messages.
The buffer should be narrowed to include only the new messages.
Output a helpful message unless NOMSG is non-nil."
  (let* ((case-fold-search nil)
	 (total-messages 0)
	 (messages-head nil)
	 (deleted-head nil))
    (or nomsg (message "Counting new messages..."))
    (goto-char (point-max))
    ;; Put at the end of messages-head
    ;; the entry for message N+1, which marks
    ;; the end of message N.  (N = number of messages).
    (setq messages-head (list (point-marker)))
    (rmail-set-message-counters-counter (point-min))
    (setq rmail-current-message (1+ rmail-total-messages))
    (setq rmail-total-messages
	  (+ rmail-total-messages total-messages))
    (setq rmail-message-vector
	  (vconcat rmail-message-vector (cdr messages-head)))
    (aset rmail-message-vector
	  rmail-current-message (car messages-head))
    (setq rmail-deleted-vector
	  (concat rmail-deleted-vector deleted-head))
    (setq rmail-summary-vector
	  (vconcat rmail-summary-vector (make-vector total-messages nil)))
    (setq rmail-msgref-vector
	  (vconcat rmail-msgref-vector (make-vector total-messages nil)))
    ;; Fill in the new elements of rmail-msgref-vector.
    (let ((i (1+ (- rmail-total-messages total-messages))))
      (while (<= i rmail-total-messages)
	(aset rmail-msgref-vector i (list i))
	(setq i (1+ i))))
    (goto-char (point-min))
    (or nomsg (message "Counting new messages...done (%d)" total-messages))))

(defun rmail-set-message-counters ()
  (rmail-forget-messages)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((point-save (point))
	     (total-messages 0)
	     (messages-after-point)
	     (case-fold-search nil)
	     (messages-head nil)
	     (deleted-head nil))
	;; Determine how many messages follow point.
	(message "Counting messages...")
	(goto-char (point-max))
	;; Put at the end of messages-head
	;; the entry for message N+1, which marks
	;; the end of message N.  (N = number of messages).
	(setq messages-head (list (point-marker)))
	(setq messages-after-point
	      (or (rmail-set-message-counters-counter (min (point) point-save))
		  0))

	(setq rmail-total-messages total-messages)
	(setq rmail-current-message
	      (min total-messages
		   (max 1 (- total-messages messages-after-point))))

	;; Make an element 0 in rmail-message-vector and rmail-deleted-vector
	;; which will never be used.
	(push nil messages-head)
	(push ?0 deleted-head)
	(setq rmail-message-vector (apply 'vector messages-head)
	      rmail-deleted-vector (concat deleted-head))

	(setq rmail-summary-vector (make-vector rmail-total-messages nil)
	      rmail-msgref-vector (make-vector (1+ rmail-total-messages) nil))

	(let ((i 0))
	  (while (<= i rmail-total-messages)
	    (aset rmail-msgref-vector i (list i))
	    (setq i (1+ i))))
	(let ((i 0))
	  (while (<= i rmail-total-messages)
	    (rmail-set-message-deleted-p i (rmail-message-attr-p i ".D"))
	    (setq i (1+ i))))
	(message "Counting messages...done")))))


(defsubst rmail-collect-deleted (message-end)
  "Collect the message deletion flags for each message.
MESSAGE-END is the buffer position corresponding to the end of
the message.  Point is at the beginning of the message."
  ;; NOTE: This piece of code will be executed on a per-message basis.
  ;; In the face of thousands of messages, it has to be as fast as
  ;; possible, hence some brute force constant use is employed in
  ;; addition to inlining.
  (save-excursion
    (setq deleted-head
	  (cons (if (and (search-forward (concat rmail-attribute-header ": ") message-end t)
			 (looking-at "?D"))
		    ?D
		  ?\s) deleted-head))))

(defun rmail-set-message-counters-counter (&optional spot-to-find)
  "Collect the start positions of messages in list `messages-head'.
Return the number of messages after the one containing SPOT-TO-FIND."
  (let ((start (point))
	messages-after-spot)
    (while (search-backward "\n\nFrom " nil t)
      (forward-char 2)
      (when (looking-at rmail-unix-mail-delimiter)
	(if (and (<= (point) spot-to-find)
		 (null messages-after-spot))
	    (setq messages-after-spot total-messages))
	(rmail-collect-deleted start)
	(setq messages-head (cons (point-marker) messages-head)
	      total-messages (1+ total-messages)
	      start (point))
	;; Show progress after every 20 messages or so.
	(if (zerop (% total-messages 20))
	    (message "Counting messages...%d" total-messages))))
    ;; Handle the first message, maybe.
    (goto-char (point-min))
    (unless (not (looking-at rmail-unix-mail-delimiter))
      (if (and (<= (point) spot-to-find)
	       (null messages-after-spot))
	  (setq messages-after-spot total-messages))
      (rmail-collect-deleted start)
      (setq messages-head (cons (point-marker) messages-head)
	    total-messages (1+ total-messages)))
    messages-after-spot))

;; Display a message.

;;;; *** Rmail Message Formatting and Header Manipulation ***

;; This is used outside of rmail.
(defun rmail-msg-is-pruned ()
  "Return nil if the current message is showing full headers."
  (with-current-buffer (if (rmail-buffers-swapped-p) rmail-view-buffer
                         rmail-buffer)
    (eq rmail-header-style 'normal)))

(defun rmail-toggle-header (&optional arg)
  "Toggle between showing full and normal message headers.
With optional integer ARG, show the normal message header if ARG
is greater than zero; otherwise, show it in full."
  (interactive "P")
  (let ((rmail-header-style
	 (if (numberp arg)
	     (if (> arg 0) 'normal 'full)
           (if (rmail-msg-is-pruned) 'full 'normal))))
    (rmail-show-message)))

(defun rmail-beginning-of-message ()
  "Show current message starting from the beginning."
  (interactive)
  (let ((rmail-show-message-hook '((lambda () (goto-char (point-min)))))
	(rmail-header-style (with-current-buffer (if (rmail-buffers-swapped-p)
						     rmail-view-buffer
						   rmail-buffer)
			      rmail-header-style)))
    (rmail-show-message rmail-current-message)))

(defun rmail-end-of-message ()
  "Show bottom of current message."
  (interactive)
  (let ((rmail-show-message-hook '((lambda ()
				     (goto-char (point-max))
				     (recenter (1- (window-height))))))
	(rmail-header-style (with-current-buffer (if (rmail-buffers-swapped-p)
						     rmail-view-buffer
						   rmail-buffer)
			      rmail-header-style)))
    (rmail-show-message rmail-current-message)))

(defun rmail-unknown-mail-followup-to ()
  "Handle a \"Mail-Followup-To\" header field with an unknown mailing list.
Ask the user whether to add that list name to `mail-mailing-lists'."
  ;; FIXME s-r not needed?  Use rmail-get-header?
  ;; We have not narrowed to the headers at ths point?
   (save-restriction
     (let ((mail-followup-to (mail-fetch-field "mail-followup-to" nil t)))
       (when mail-followup-to
	 (let ((addresses
		(split-string
		 (mail-strip-quoted-names mail-followup-to)
		 ",[[:space:]]+" t)))
	   (dolist (addr addresses)
	     (when (and (not (member addr mail-mailing-lists))
			(not
			 ;; taken from rmailsum.el
			 (string-match
			  (or rmail-user-mail-address-regexp
			      (concat "^\\("
				      (regexp-quote (user-login-name))
				      "\\($\\|@\\)\\|"
				      (regexp-quote
				       (or user-mail-address
					   (concat (user-login-name) "@"
						   (or mail-host-address
						       (system-name)))))
				      "\\>\\)"))
			  addr))
			(y-or-n-p
			 (format "Add `%s' to `mail-mailing-lists'? "
				 addr)))
	       (customize-save-variable 'mail-mailing-lists
					(cons addr mail-mailing-lists)))))))))

(defun rmail-widen ()
  "Display the entire mailbox file."
  (interactive)
  (rmail-swap-buffers-maybe)
  (widen))

(defun rmail-no-mail-p ()
  "Return nil if there is mail, else \"No mail.\"."
  (if (zerop rmail-total-messages)
      (save-excursion
	;; Eg we deleted all the messages, so remove the old N/M mark.
	(with-current-buffer rmail-buffer (setq mode-line-process nil))
	(with-current-buffer rmail-view-buffer
	  (erase-buffer)
	  "No mail."))))

(defun rmail-show-message (&optional n no-summary)
  "Show message number N (prefix argument), counting from start of file.
If summary buffer is currently displayed, update current message there also.
N defaults to the current message."
  (interactive "p")
  (or (eq major-mode 'rmail-mode)
      (switch-to-buffer rmail-buffer))
  ;; FIXME: Why do we swap the raw data back in?
  (rmail-swap-buffers-maybe)
  (rmail-maybe-set-message-counters)
  (widen)
  (let ((blurb (rmail-show-message-1 n)))
    (or (zerop rmail-total-messages)
	(progn
	  (when mail-mailing-lists
	    (rmail-unknown-mail-followup-to))
	  (if transient-mark-mode (deactivate-mark))
	  ;; If there is a summary buffer, try to move to this message
	  ;; in that buffer.  But don't complain if this message is
	  ;; not mentioned in the summary.  Don't do this at all if we
	  ;; were called on behalf of cursor motion in the summary
	  ;; buffer.
	  (and (rmail-summary-exists) (not no-summary)
	       (let ((curr-msg rmail-current-message))
		 (rmail-select-summary
		  (rmail-summary-goto-msg curr-msg t t))))
	  (with-current-buffer rmail-buffer
	    (rmail-auto-file))))
    (if blurb
	(message blurb))))

(defun rmail-is-text-p ()
  "Return t if the region contains a text message, nil otherwise."
  (save-excursion
    (let ((text-regexp "\\(text\\|message\\)/")
	  (content-type-header (mail-fetch-field "content-type")))
      ;; The message is text if either there is no content type header
      ;; (a default of "text/plain; charset=US-ASCII" is assumed) or
      ;; the base content type is either text or message.
      (or (not content-type-header)
	  (string-match text-regexp content-type-header)))))

(defcustom rmail-show-message-verbose-min 200000
  "Message size at which to show progress messages for displaying it."
  :type 'integer
  :group 'rmail
  :version "23.1")

(defun rmail-show-message-1 (&optional msg)
  "Show message MSG (default: current message) using `rmail-view-buffer'.
Return text to display in the minibuffer if MSG is out of
range (displaying a reasonable choice as well), nil otherwise.
The current mail message becomes the message displayed."
  (let ((mbox-buf rmail-buffer)
	(view-buf rmail-view-buffer)
	blurb beg end body-start coding-system character-coding
	is-text-message header-style)
    (if (not msg)
	(setq msg rmail-current-message))
    (unless (setq blurb (rmail-no-mail-p))
      (cond ((<= msg 0)
	     (setq msg 1
		   rmail-current-message 1
		   blurb "No previous message"))
	    ((> msg rmail-total-messages)
	     (setq msg rmail-total-messages
		   rmail-current-message rmail-total-messages
		   blurb "No following message"))
	    (t (setq rmail-current-message msg)))
      (with-current-buffer rmail-buffer
	(setq header-style rmail-header-style)
	;; Mark the message as seen, but preserve buffer modified flag.
	(let ((modiff (buffer-modified-p)))
	  (rmail-set-attribute rmail-unseen-attr-index nil)
	  (unless modiff
	    (restore-buffer-modified-p modiff)))
	;; bracket the message in the mail
	;; buffer and determine the coding system the transfer encoding.
	(rmail-swap-buffers-maybe)
	(setq beg (rmail-msgbeg msg)
	      end (rmail-msgend msg))
	(when (> (- end beg) rmail-show-message-verbose-min)
	  (message "Showing message %d" msg))
	(narrow-to-region beg end)
	(goto-char beg)
	(with-current-buffer rmail-view-buffer
	  ;; We give the view buffer a buffer-local value of
	  ;; rmail-header-style based on the binding in effect when
	  ;; this function is called; `rmail-toggle-headers' can
	  ;; inspect this value to determine how to toggle.
	  (set (make-local-variable 'rmail-header-style) header-style))
	(if (and rmail-enable-mime
		 rmail-show-mime-function
		 (re-search-forward "mime-version: 1.0" nil t))
	    (let ((rmail-buffer mbox-buf)
		  (rmail-view-buffer view-buf))
	      (funcall rmail-show-mime-function))
	  (setq body-start (search-forward "\n\n" nil t))
	  (narrow-to-region beg (point))
	  (goto-char beg)
	  (save-excursion
	    (if (re-search-forward "^X-Coding-System: *\\(.*\\)$" nil t)
		(setq coding-system (intern (match-string 1)))
	      (setq coding-system (rmail-get-coding-system))))
	  (setq character-coding (mail-fetch-field "content-transfer-encoding")
		is-text-message (rmail-is-text-p))
	  (if character-coding
	      (setq character-coding (downcase character-coding)))
	  (narrow-to-region beg end)
	  ;; Decode the message body into an empty view buffer using a
	  ;; unibyte temporary buffer where the character decoding takes
	  ;; place.
	  (with-current-buffer rmail-view-buffer
	    (erase-buffer))
	  (if (null character-coding)
	      ;; Do it directly since that is fast.
	      (rmail-decode-region body-start end coding-system view-buf)
	    ;; Can this be done directly, skipping the temp buffer?
	    (with-temp-buffer
	      (set-buffer-multibyte nil)
	      (insert-buffer-substring mbox-buf body-start end)
	      (cond
	       ((string= character-coding "quoted-printable")
		;; See bug#5441.
		(or (mail-unquote-printable-region (point-min) (point-max)
						   nil t 'unibyte)
		    (message "Malformed MIME quoted-printable message")))
	       ((and (string= character-coding "base64") is-text-message)
		(condition-case err
		    (base64-decode-region (point-min) (point-max))
		  (error (message "%s" (cdr err)))))
	       ((eq character-coding 'uuencode)
		(error "uuencoded messages are not supported yet"))
	       (t))
	      (rmail-decode-region (point-min) (point-max)
				   coding-system view-buf)))
	  (with-current-buffer rmail-view-buffer
	    ;; Prepare the separator (blank line) before the body.
	    (goto-char (point-min))
	    (insert "\n")
	    ;; Unquote quoted From lines
	    (while (re-search-forward "^>+From " nil t)
	      (beginning-of-line)
	      (delete-char 1)
	      (forward-line))
	    (goto-char (point-min)))
	  ;; Copy the headers to the front of the message view buffer.
	  (rmail-copy-headers beg end)
	  ;; Decode any RFC2047 encoded message headers.
	  (if rmail-enable-mime
	      (with-current-buffer rmail-view-buffer
		(rfc2047-decode-region
		 (point-min)
		 (progn
		   (search-forward "\n\n" nil 'move)
		   (point))))))
	;; highlight the message, activate any URL like text and add
	;; special highlighting for and quoted material.
	(with-current-buffer rmail-view-buffer
	  (goto-char (point-min))
	  (rmail-highlight-headers)
					;(rmail-activate-urls)
					;(rmail-process-quoted-material)
	  )
	;; Update the mode-line with message status information and swap
	;; the view buffer/mail buffer contents.
	(rmail-display-labels)
	(rmail-swap-buffers)
	(setq rmail-buffer-swapped t)
	(run-hooks 'rmail-show-message-hook)
	(when (> (- end beg) rmail-show-message-verbose-min)
	  (message "Showing message %d...done" msg))))
    blurb))

(defun rmail-copy-headers (beg end &optional ignored-headers)
  "Copy displayed header fields to the message viewer buffer.
BEG and END marks the start and end positions of the message in
the mbox buffer.  If the optional argument IGNORED-HEADERS is
non-nil, ignore all header fields whose names match that regexp.
Otherwise, if `rmail-displayed-headers' is non-nil, copy only
those header fields whose names match that regexp.  Otherwise,
copy all header fields whose names do not match
`rmail-ignored-headers' (unless they also match
`rmail-nonignored-headers').  Moves point in the message viewer
buffer to the end of the headers."
  (let ((header-start-regexp "\n[^ \t]")
	lim)
    (with-current-buffer rmail-buffer
      (when (search-forward "\n\n" nil t)
	(forward-char -1)
	(save-restriction
	  ;; Put point right after the From header line.
	  (narrow-to-region beg (point))
	  (goto-char (point-min))
	  (unless (re-search-forward header-start-regexp nil t)
	    (rmail-error-bad-format))
	  (forward-char -1)
	  (cond
	   ;; Handle the case where all headers should be copied.
	   ((eq rmail-header-style 'full)
	    (prepend-to-buffer rmail-view-buffer beg (point-max))
	    ;; rmail-show-message-1 expects this function to leave point
	    ;; at the end of the headers.

	    (let ((len (- (point-max) beg)))
	      (with-current-buffer rmail-view-buffer
		(goto-char (1+ len)))))

	   ;; Handle the case where the headers matching the displayed
	   ;; headers regexp should be copied.
	   ((and rmail-displayed-headers (null ignored-headers))
	    (while (not (eobp))
	      (save-excursion
		(setq lim (if (re-search-forward header-start-regexp nil t)
			      (1+ (match-beginning 0))
			    (point-max))))
	      (when (looking-at rmail-displayed-headers)
		(append-to-buffer rmail-view-buffer (point) lim))
	      (goto-char lim)))
	   ;; Handle the ignored headers.
	   ((or ignored-headers (setq ignored-headers rmail-ignored-headers))
	    (while (and ignored-headers (not (eobp)))
	      (save-excursion
		(setq lim (if (re-search-forward header-start-regexp nil t)
			      (1+ (match-beginning 0))
			    (point-max))))
	      (if (and (looking-at ignored-headers)
		       (not (looking-at rmail-nonignored-headers)))
		  (goto-char lim)
		(append-to-buffer rmail-view-buffer (point) lim)
		(goto-char lim))))
	   (t (error "No headers selected for display!"))))))))

(defun rmail-redecode-body (coding)
  "Decode the body of the current message using coding system CODING.
This is useful with mail messages that have malformed or missing
charset= headers.

This function assumes that the current message is already decoded
and displayed in the RMAIL buffer, but the coding system used to
decode it was incorrect.  It then decodes the message again,
using the coding system CODING."
  (interactive "zCoding system for re-decoding this message: ")
  (when (not rmail-enable-mime)
    (with-current-buffer rmail-buffer
      (rmail-swap-buffers-maybe)
      (save-restriction
	(widen)
	(let ((msgbeg (rmail-msgbeg rmail-current-message))
	      (msgend (rmail-msgend rmail-current-message))
	      (buffer-read-only nil)
	      body-start x-coding-header old-coding)
	  (narrow-to-region msgbeg msgend)
	  (goto-char (point-min))
	  (unless (setq body-start (search-forward "\n\n" (point-max) 1))
	    (error "No message body"))

	  (save-restriction
	    ;; Narrow to headers
	    (narrow-to-region (point-min) body-start)
	    (setq x-coding-header (goto-char (point-min)))
	    (if (not (re-search-forward "^X-Coding-System: *\\(.*\\)$" nil t))
		(setq old-coding (rmail-get-coding-system))
	      (setq old-coding (intern (match-string 1)))
	      (setq x-coding-header (point)))
	    (check-coding-system old-coding)
	    ;; Make sure the new coding system uses the same EOL
	    ;; conversion, to prevent ^M characters from popping up
	    ;; all over the place.
	    (let ((eol-type (coding-system-eol-type old-coding)))
	      (if (numberp eol-type)
		  (setq coding
			(coding-system-change-eol-conversion coding eol-type))))
	    (when (not (coding-system-equal
			(coding-system-base old-coding)
			(coding-system-base coding)))
	      ;; Rewrite the coding-system header.
	      (goto-char x-coding-header)
	      (if (> (point) (point-min))
		  (delete-region (line-beginning-position) (point))
		(forward-line)
		(insert "\n")
		(forward-line -1))
	      (insert "X-Coding-System: "
		      (symbol-name coding))))
	  (rmail-show-message))))))

(defun rmail-highlight-headers ()
  "Highlight the headers specified by `rmail-highlighted-headers'.
Uses the face specified by `rmail-highlight-face'."
  (if rmail-highlighted-headers
      (save-excursion
	(search-forward "\n\n" nil 'move)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (let ((case-fold-search t)
		(inhibit-read-only t)
		;; When rmail-highlight-face is removed, just
		;; use 'rmail-highlight here.
		(face (or rmail-highlight-face
			  (if (face-differs-from-default-p 'bold)
			      'bold 'highlight)))
		;; List of overlays to reuse.
		(overlays rmail-overlay-list))
	    (goto-char (point-min))
	    (while (re-search-forward rmail-highlighted-headers nil t)
	      (skip-chars-forward " \t")
	      (let ((beg (point))
		    overlay)
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		;; Back up over newline, then trailing spaces or tabs
		(forward-char -1)
		(while (member (preceding-char) '(?  ?\t))
		  (forward-char -1))
		(if overlays
		    ;; Reuse an overlay we already have.
		    (progn
		      (setq overlay (car overlays)
			    overlays (cdr overlays))
		      (overlay-put overlay 'face face)
		      (move-overlay overlay beg (point)))
		  ;; Make a new overlay and add it to
		  ;; rmail-overlay-list.
		  (setq overlay (make-overlay beg (point)))
		  (overlay-put overlay 'face face)
		  (setq rmail-overlay-list
			(cons overlay rmail-overlay-list))))))))))

(defun rmail-auto-file ()
  "Automatically move a message into another sfolder based on criteria.
This moves messages according to `rmail-automatic-folder-directives'.
It only does something in the folder that `rmail-file-name' specifies.
The function `rmail-show-message' calls this whenever it shows a message.
This leaves a message alone if it already has the `filed' attribute."
  (if (or (zerop rmail-total-messages)
	  (rmail-message-attr-p rmail-current-message "...F")
	  (not (string= (buffer-file-name)
			(expand-file-name rmail-file-name))))
      ;; Do nothing if the message has already been filed or if there
      ;; are no messages.
      nil
    ;; Find out some basics (common fields)
    (let ((from (mail-fetch-field "from"))
	  (subj (mail-fetch-field "subject"))
	  (to   (concat (mail-fetch-field "to") "," (mail-fetch-field "cc")))
	  (d rmail-automatic-folder-directives)
	  (directive-loop nil)
	  (folder nil))
      (while d
	(setq folder (car (car d))
	      directive-loop (cdr (car d)))
	(while (and (car directive-loop)
		    (let ((f (cond
			      ((string= (downcase (car directive-loop)) "from")
			       from)
			      ((string= (downcase (car directive-loop)) "to")
			       to)
			      ((string= (downcase (car directive-loop))
					"subject") subj)
			      (t (mail-fetch-field (car directive-loop))))))
		      ;; FIXME - shouldn't this ignore case?
		      (and f (string-match (car (cdr directive-loop)) f))))
	  (setq directive-loop (cdr (cdr directive-loop))))
	;; If there are no directives left, then it was a complete match.
	(if (null directive-loop)
	    (if (null folder)
		(rmail-delete-forward)
	      (if (string= "/dev/null" folder)
		  (rmail-delete-message)
		(rmail-output folder 1)
		(setq d nil))))
	(setq d (cdr d))))))

;; Simple message motion commands.

(defun rmail-next-message (n)
  "Show following message whether deleted or not.
With prefix arg N, moves forward N messages, or backward if N is negative."
  (interactive "p")
  (set-buffer rmail-buffer)
  (rmail-maybe-set-message-counters)
  (rmail-show-message (+ rmail-current-message n)))

(defun rmail-previous-message (n)
  "Show previous message whether deleted or not.
With prefix arg N, moves backward N messages, or forward if N is negative."
  (interactive "p")
  (rmail-next-message (- n)))

(defun rmail-next-undeleted-message (n)
  "Show following non-deleted message.
With prefix arg N, moves forward N non-deleted messages,
or backward if N is negative.

Returns t if a new message is being shown, nil otherwise."
  (interactive "p")
  (set-buffer rmail-buffer)
  (rmail-maybe-set-message-counters)
  (let ((lastwin rmail-current-message)
	(current rmail-current-message))
    (while (and (> n 0) (< current rmail-total-messages))
      (setq current (1+ current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1- n))))
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1+ n))))
    (if (/= lastwin rmail-current-message)
 	(progn (rmail-show-message lastwin)
 	       t)
      (if (< n 0)
	  (message "No previous nondeleted message"))
      (if (> n 0)
	  (message "No following nondeleted message"))
      nil)))

(defun rmail-previous-undeleted-message (n)
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  (interactive "p")
  (rmail-next-undeleted-message (- n)))

(defun rmail-first-message ()
  "Show first message in file."
  (interactive)
  (rmail-maybe-set-message-counters)
  (rmail-show-message 1))

(defun rmail-last-message ()
  "Show last message in file."
  (interactive)
  (rmail-maybe-set-message-counters)
  (rmail-show-message rmail-total-messages))

(defun rmail-next-error-move (msg-pos bad-marker)
  "Move to an error locus (probably grep hit) in an Rmail buffer.
MSG-POS is a marker pointing at the error message in the grep buffer.
BAD-MARKER is a marker that ought to point at where to move to,
but probably is garbage."

  (let* ((message-loc (compilation--message->loc
		       (get-text-property msg-pos 'compilation-message
					  (marker-buffer msg-pos))))
	 (column (car message-loc))
	 (linenum (cadr message-loc))
	 line-text
	 pos
	 msgnum msgbeg msgend
	 header-field
	 line-number-within)

    ;; Look at the whole Rmail file.
    (rmail-swap-buffers-maybe)

    (save-restriction
      (widen)
      (save-excursion
	;; Find the line that the error message points at.
	(goto-char (point-min))
	(forward-line (1- linenum))
	(setq pos (point))

	;; Find the text at the start of the line,
	;; before the first = sign.
	;; This text has a good chance of being also in the
	;; decoded message.
	(save-excursion
	  (skip-chars-forward "^=\n")
	  (setq line-text (buffer-substring pos (point))))

	;; Find which message this position is in,
	;; and the limits of that message.
	(setq msgnum (rmail-what-message pos))
	(setq msgbeg (rmail-msgbeg msgnum))
	(setq msgend (rmail-msgend msgnum))

	;; Find which header this locus is in,
	;; or if it's in the message body,
	;; and the line-based position within that.
	(goto-char msgbeg)
	(let ((header-end msgend))
	  (if (search-forward "\n\n" nil t)
	      (setq header-end (point)))
	  (if (>= pos header-end)
	      (setq line-number-within
		    (count-lines header-end pos))
	    (goto-char pos)
	    (unless (looking-at "^[^ \t]")
	      (re-search-backward "^[^ \t]"))
	    (looking-at "[^:\n]*[:\n]")
	    (setq header-field (match-string 0)
		  line-number-within (count-lines (point) pos))))))

    ;; Display the right message.
    (rmail-show-message msgnum)

    ;; Move to the right position within the displayed message.
    ;; Or at least try.  The decoded message's lines may not
    ;; correspond to the lines in the inbox file.
    (goto-char (point-min))
    (if header-field
	(progn
	  (re-search-forward (concat "^" (regexp-quote header-field)) nil t)
	  (forward-line line-number-within))
      (search-forward "\n\n" nil t)
      (if (re-search-forward (concat "^" (regexp-quote line-text)) nil t)
	  (goto-char (match-beginning 0))))
    (if (eobp)
	;; If the decoded message doesn't have enough lines,
	;; go to the beginning rather than the end.
	(goto-char (point-min))
      ;; Otherwise, go to the right column.
      (if column
	  (forward-char column)))))

(defun rmail-what-message (&optional pos)
  "Return message number POS (or point) is in."
  (let* ((high rmail-total-messages)
         (mid (/ high 2))
         (low 1)
         (where (or pos
		    (with-current-buffer (if (rmail-buffers-swapped-p)
					     rmail-view-buffer
					   (current-buffer))
		      (point)))))
    (while (> (- high low) 1)
      (if (>= where (rmail-msgbeg mid))
          (setq low mid)
          (setq high mid))
      (setq mid (+ low (/ (- high low) 2))))
    (if (>= where (rmail-msgbeg high)) high low)))

;; Searching in Rmail file.

(defun rmail-search-message (msg regexp)
  "Return non-nil, if for message number MSG, regexp REGEXP matches."
  ;; This is adequate because its only caller, rmail-search,
  ;; unswaps the buffers.
  (goto-char (rmail-msgbeg msg))
  (if (and rmail-enable-mime
	   rmail-search-mime-message-function)
      (funcall rmail-search-mime-message-function msg regexp)
    (re-search-forward regexp (rmail-msgend msg) t)))

(defvar rmail-search-last-regexp nil)
(defun rmail-search (regexp &optional n)
  "Show message containing next match for REGEXP (but not the current msg).
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (< (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if rmail-search-last-regexp
			(concat ", default "
				rmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (or n (setq n 1))
  (message "%sRmail search for %s..."
	   (if (< n 0) "Reverse " "")
	   regexp)
  (set-buffer rmail-buffer)
  (let ((orig-message rmail-current-message)
	(msg rmail-current-message)
	(reversep (< n 0))
	(opoint (if (rmail-buffers-swapped-p) (point)))
	found)
    (rmail-swap-buffers-maybe)
    (rmail-maybe-set-message-counters)
    (widen)
    (unwind-protect
	(while (/= n 0)
	  ;; Check messages one by one, advancing message number up or
	  ;; down but searching forward through each message.
	  (if reversep
	      (while (and (null found) (> msg 1))
		(setq msg (1- msg)
		      found (rmail-search-message msg regexp)))
	    (while (and (null found) (< msg rmail-total-messages))
	      (setq msg (1+ msg)
		    found (rmail-search-message msg regexp))))
	  (setq n (+ n (if reversep 1 -1))))
      (if found
	  (progn
	    (rmail-show-message msg)
	    ;; Search forward (if this is a normal search) or backward
	    ;; (if this is a reverse search) through this message to
	    ;; position point.  This search may fail because REGEXP
	    ;; was found in the hidden portion of this message.  In
	    ;; that case, move point to the beginning of visible
	    ;; portion.
	    (if reversep
		(progn
		  (goto-char (point-max))
		  (re-search-backward regexp nil 'move))
	      (goto-char (point-min))
	      (re-search-forward regexp nil t))
	    (message "%sRmail search for %s...done"
		     (if reversep "Reverse " "")
		     regexp))
	(rmail-show-message orig-message)
	(if opoint (goto-char opoint))
	(ding)
	(message "Search failed: %s" regexp)))))

(defun rmail-search-backwards (regexp &optional n)
  "Show message containing previous match for REGEXP.
Prefix argument gives repeat count; negative argument means search
forward (through later messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (>= (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if rmail-search-last-regexp
			(concat ", default "
				rmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (rmail-search regexp (- (or n 1))))

;; Scan for attributes, and compare subjects.

(defun rmail-first-unseen-message ()
  "Return message number of first message which has `unseen' attribute."
  (rmail-maybe-set-message-counters)
  (let ((current 1)
	found)
    (save-restriction
      (widen)
      (while (and (not found) (<= current rmail-total-messages))
	(if (rmail-message-attr-p current "......U")
	    (setq found current))
	(setq current (1+ current))))
    found))

(defun rmail-simplified-subject (&optional msgnum)
  "Return the simplified subject of message MSGNUM (or current message).
Simplifying the subject means stripping leading and trailing whitespace,
and typical reply prefixes such as Re:."
  (let ((subject (or (rmail-get-header "Subject" msgnum) "")))
    (setq subject (rfc2047-decode-string subject))
    (if (string-match "\\`[ \t]+" subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match rmail-reply-regexp subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match "[ \t]+\\'" subject)
	(setq subject (substring subject 0 (match-beginning 0))))
    ;; If Subject is long, mailers will break it into several lines at
    ;; arbitrary places, so normalize whitespace by replacing every
    ;; run of whitespace characters with a single space.
    (setq subject (replace-regexp-in-string "[ \t\n]+" " " subject))
    subject))

(defun rmail-simplified-subject-regexp ()
  "Return a regular expression matching the current simplified subject.
The idea is to match it against simplified subjects of other messages."
  (let ((subject (rmail-simplified-subject)))
    (setq subject (regexp-quote subject))
    ;; Hide commas so it will work ok if parsed as a comma-separated list
    ;; of regexps.
    (setq subject
	  (replace-regexp-in-string "," "\054" subject t t))
    (concat "\\`" subject "\\'")))

(defun rmail-next-same-subject (n)
  "Go to the next mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go backwards instead."
  (interactive "p")
  (let ((subject (rmail-simplified-subject))
	(forward (> n 0))
	(i rmail-current-message)
	found)
    (while (and (/= n 0)
		(if forward
		    (< i rmail-total-messages)
		  (> i 1)))
      (let (done)
	(while (and (not done)
		    (if forward
			(< i rmail-total-messages)
		      (> i 1)))
	  (setq i (if forward (1+ i) (1- i)))
	  (setq done (string-equal subject (rmail-simplified-subject i))))
	(if done (setq found i)))
      (setq n (if forward (1- n) (1+ n))))
    (if found
	(rmail-show-message found)
      (error "No %s message with same subject"
	     (if forward "following" "previous")))))

(defun rmail-previous-same-subject (n)
  "Go to the previous mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go forwards instead."
  (interactive "p")
  (rmail-next-same-subject (- n)))

;;;; *** Rmail Message Deletion Commands ***

(defun rmail-message-deleted-p (n)
  "Return non-nil if message number N is deleted (in `rmail-deleted-vector')."
  (= (aref rmail-deleted-vector n) ?D))

(defun rmail-set-message-deleted-p (n state)
  "Set the deleted state of message number N (in `rmail-deleted-vector').
STATE non-nil means mark as deleted."
  (aset rmail-deleted-vector n (if state ?D ?\s)))

(defun rmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (rmail-set-attribute rmail-deleted-attr-index t)
  (run-hooks 'rmail-delete-message-hook))

(defun rmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
  (set-buffer rmail-buffer)
  (let ((msg rmail-current-message))
    (while (and (> msg 0)
		(not (rmail-message-deleted-p msg)))
      (setq msg (1- msg)))
    (if (= msg 0)
	(error "No previous deleted message")
      (if (/= msg rmail-current-message)
	  (rmail-show-message msg))
      (rmail-set-attribute rmail-deleted-attr-index nil)
      (if (rmail-summary-exists)
	  (with-current-buffer rmail-summary-buffer
	    (rmail-summary-mark-undeleted msg)))
      (rmail-maybe-display-summary))))

(defun rmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward.

Returns t if a new message is displayed after the delete, or nil otherwise."
  (interactive "P")
  (rmail-set-attribute rmail-deleted-attr-index t)
  (run-hooks 'rmail-delete-message-hook)
  (let ((del-msg rmail-current-message))
    (if (rmail-summary-exists)
	(rmail-select-summary
	 (rmail-summary-mark-deleted del-msg)))
    (prog1 (rmail-next-undeleted-message (if backward -1 1))
      (rmail-maybe-display-summary))))

(defun rmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-delete-forward t))

;; Expunging.

;; Compute the message number a given message would have after expunging.
;; The present number of the message is OLDNUM.
;; DELETEDVEC should be rmail-deleted-vector.
;; The value is nil for a message that would be deleted.
(defun rmail-msg-number-after-expunge (deletedvec oldnum)
  (if (or (null oldnum) (= (aref deletedvec oldnum) ?D))
      nil
    (let ((i 0)
	  (newnum 0))
      (while (< i oldnum)
	(if (/= (aref deletedvec i) ?D)
	    (setq newnum (1+ newnum)))
	(setq i (1+ i)))
      newnum)))

(defun rmail-expunge-confirmed ()
  "Return t if expunge is needed and desirable.
If `rmail-confirm-expunge' is non-nil, ask user to confirm."
  (set-buffer rmail-buffer)
  (and (stringp rmail-deleted-vector)
       (string-match "D" rmail-deleted-vector)
       (if rmail-confirm-expunge
	   (funcall rmail-confirm-expunge
		    "Erase deleted messages from Rmail file? ")
	 t)))

(defun rmail-only-expunge (&optional dont-show)
  "Actually erase all deleted messages in the file."
  (interactive)
  (rmail-swap-buffers-maybe)
  (set-buffer rmail-buffer)
  (message "Expunging deleted messages...")
  ;; Discard all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (rmail-maybe-set-message-counters)
  (let* ((omax (- (buffer-size) (point-max)))
	 (omin (- (buffer-size) (point-min)))
	 (opoint (if (and (> rmail-current-message 0)
			  (rmail-message-deleted-p rmail-current-message))
		     0
		   (if rmail-enable-mime
		       (with-current-buffer rmail-view-buffer
			 (- (point)(point-min)))
		     (- (point) (point-min)))))
	 (messages-head (cons (aref rmail-message-vector 0) nil))
	 (messages-tail messages-head)
	 ;; Don't make any undo records for the expunging.
	 (buffer-undo-list t)
	 (win))
    (unwind-protect
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (let ((counter 0)
		(number 1)
		new-summary
		(new-msgref (list (list 0)))
		(buffer-read-only nil)
		(total rmail-total-messages)
		(new-message-number rmail-current-message)
		(messages rmail-message-vector)
		(deleted rmail-deleted-vector)
		(summary rmail-summary-vector))
	    (setq rmail-total-messages nil
		  rmail-current-message nil
		  rmail-message-vector nil
		  rmail-deleted-vector nil
		  rmail-summary-vector nil)

	    (while (<= number total)
	      (if (= (aref deleted number) ?D)
		  (progn
		    (delete-region (aref messages number)
				   (aref messages (1+ number)))
		    (move-marker (aref messages number) nil)
		    (if (> new-message-number counter)
			(setq new-message-number (1- new-message-number))))
		(setq counter (1+ counter))
		(setq messages-tail
		      (setcdr messages-tail
			      (cons (aref messages number) nil)))
		(setq new-summary
		      (cons (if (= counter number) (aref summary (1- number)))
			    new-summary))
		(setq new-msgref
		      (cons (aref rmail-msgref-vector number)
			    new-msgref))
		(setcar (car new-msgref) counter))
	      (if (zerop (% (setq number (1+ number)) 20))
		  (message "Expunging deleted messages...%d" number)))
	    (setq messages-tail
		  (setcdr messages-tail
			  (cons (aref messages number) nil)))
	    (setq rmail-current-message new-message-number
		  rmail-total-messages counter
		  rmail-message-vector (apply 'vector messages-head)
		  rmail-deleted-vector (make-string (1+ counter) ?\s)
		  rmail-summary-vector (vconcat (nreverse new-summary))
		  rmail-msgref-vector (apply 'vector (nreverse new-msgref))
		  win t)))
      (message "Expunging deleted messages...done")
      (if (not win)
	  (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax)))
      (if (not dont-show)
	  (rmail-show-message (min rmail-current-message rmail-total-messages)))
      (if rmail-enable-mime
	  (goto-char (+ (point-min) opoint))
	(goto-char (+ (point) opoint))))))

;; The DONT-SHOW argument is new in 23.  Does not seem very important.
(defun rmail-expunge (&optional dont-show)
  "Erase deleted messages from Rmail file and summary buffer.
This always shows a message (so as not to leave the Rmail buffer
unswapped), and always updates any summary (so that it remains
consistent with the Rmail buffer).  If DONT-SHOW is non-nil, it
does not pop any summary buffer."
  (interactive)
  (when (rmail-expunge-confirmed)
    (rmail-modify-format)
    (let ((was-deleted (rmail-message-deleted-p rmail-current-message))
	  (was-swapped (rmail-buffers-swapped-p)))
      (rmail-only-expunge t)
      ;; We always update the summary buffer, so that the contents
      ;; remain consistent with the rmail buffer.
      ;; The only difference is, in the dont-show case, we use a
      ;; cut-down version of rmail-select-summary that does not pop
      ;; the summary buffer.  It's only used by rmail-quit, which is
      ;; just going to bury any summary immediately after.  If we made
      ;; rmail-quit bury the summary first, dont-show could be removed.
      ;; But the expunge might not be confirmed...
      (if (rmail-summary-exists)
	  (if dont-show
	      (let ((total rmail-total-messages))
		(with-current-buffer rmail-summary-buffer
		  (let ((rmail-total-messages total))
		    (rmail-update-summary))))
	    (rmail-select-summary (rmail-update-summary))))
      ;; We always show a message, because (rmail-only-expunge t)
      ;; leaves the rmail buffer unswapped.
      ;; If we expunged the current message, a new one is current now,
      ;; so show it.  If we weren't showing a message, show it.
      (if (or was-deleted (not was-swapped))
	  (rmail-show-message-1 rmail-current-message)
	;; We can just show the same message that was being shown before.
	(rmail-display-labels)
	(rmail-swap-buffers)
	(setq rmail-buffer-swapped t)))))

;;;; *** Rmail Mailing Commands ***

(defun rmail-yank-current-message (buffer)
  "Yank into the current buffer the current message of Rmail buffer BUFFER.
If BUFFER is swapped with its message viewer buffer, yank out of BUFFER.
If BUFFER is not swapped, yank out of its message viewer buffer."
  (with-current-buffer buffer
    (unless (rmail-buffers-swapped-p)
      (setq buffer rmail-view-buffer)))
  (insert-buffer-substring buffer)
  ;; If they yank the text of BUFFER, the encoding of BUFFER is a
  ;; better default for the reply message than the default value of
  ;; buffer-file-coding-system.
  (and (coding-system-equal (default-value 'buffer-file-coding-system)
			    buffer-file-coding-system)
       (setq buffer-file-coding-system
	     (coding-system-change-text-conversion
	      buffer-file-coding-system (coding-system-base
					 (with-current-buffer buffer
					   buffer-file-coding-system))))))

(defun rmail-start-mail (&optional noerase to subject in-reply-to cc
				   replybuffer sendactions same-window
				   other-headers)
  (let ((switch-function
	 (cond (same-window nil)
	       (rmail-mail-new-frame 'switch-to-buffer-other-frame)
	       (t 'switch-to-buffer-other-window)))
	yank-action)
    (if replybuffer
	;; The function used here must behave like insert-buffer wrt
	;; point and mark (see doc of sc-cite-original).
	(setq yank-action
	      `(rmail-yank-current-message ,replybuffer)))
    (push (cons "cc" cc) other-headers)
    (push (cons "in-reply-to" in-reply-to) other-headers)
    (setq other-headers
	  (mapcar #'(lambda (elt)
		      (cons (car elt) (if (stringp (cdr elt))
					  (rfc2047-decode-string (cdr elt)))))
		  other-headers))
    (if (stringp to) (setq to (rfc2047-decode-string to)))
    (if (stringp in-reply-to)
	(setq in-reply-to (rfc2047-decode-string in-reply-to)))
    (if (stringp cc) (setq cc (rfc2047-decode-string cc)))
    (if (stringp subject) (setq subject (rfc2047-decode-string subject)))
    (prog1
	(compose-mail to subject other-headers noerase
		      switch-function yank-action sendactions
		      (if replybuffer `(rmail-mail-return ,replybuffer)))
      (if (eq switch-function 'switch-to-buffer-other-frame)
	  ;; This is not a standard frame parameter; nothing except
	  ;; sendmail.el looks at it.
	  (modify-frame-parameters (selected-frame)
				   '((mail-dedicated-frame . t)))))))

(defun rmail-mail-return (&optional newbuf)
  "Try to return to Rmail from the mail window.
If optional argument NEWBUF is specified, it is the Rmail buffer
to switch to."
  (cond
   ;; If there is only one visible frame with no special handling,
   ;; consider deleting the mail window to return to Rmail.
   ((or (null (delq (selected-frame) (visible-frame-list)))
	(not (or (window-dedicated-p (frame-selected-window))
		 (and pop-up-frames (one-window-p))
		 (cdr (assq 'mail-dedicated-frame
			    (frame-parameters))))))
    (let (rmail-flag summary-buffer)
      (unless (one-window-p)
	(with-current-buffer
	    (window-buffer (next-window (selected-window) 'not))
	  (setq rmail-flag (eq major-mode 'rmail-mode))
	  (setq summary-buffer
		(and (boundp 'mail-bury-selects-summary)
		     mail-bury-selects-summary
		     (boundp 'rmail-summary-buffer)
		     rmail-summary-buffer
		     (buffer-name rmail-summary-buffer)
		     (not (get-buffer-window rmail-summary-buffer))
		     rmail-summary-buffer))))
      (cond ((null rmail-flag)
	     ;; If the Rmail buffer is not in the next window, switch
	     ;; directly to the Rmail buffer specified by NEWBUF.
	     (if (buffer-live-p newbuf)
		 (switch-to-buffer newbuf)))
	    ;; If the Rmail buffer is in the next window, switch to
	    ;; the summary buffer if `mail-bury-selects-summary' is
	    ;; non-nil.  Otherwise just delete this window.
	    (summary-buffer
	     (switch-to-buffer summary-buffer))
	    (t
	     (delete-window)))))
   ;; If the frame was probably made for this buffer, the user
   ;; probably wants to delete it now.
   ((display-multi-frame-p)
    (delete-frame (selected-frame)))
   ;; The previous frame is where normally they have the Rmail buffer
   ;; displayed.
   (t (other-frame -1))))

(defun rmail-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (rmail-start-mail nil nil nil nil nil rmail-buffer))

;; FIXME should complain if there is nothing to continue.
(defun rmail-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (rmail-start-mail t))

(defun rmail-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (if (zerop rmail-current-message)
      (error "There is no message to reply to"))
  (let (from reply-to cc subject date to message-id references
	     resent-to resent-cc resent-reply-to
	     (msgnum rmail-current-message))
    (rmail-apply-in-message
     rmail-current-message
     (lambda ()
       (search-forward "\n\n" nil 'move)
       (narrow-to-region (point-min) (point))
       (setq from (mail-fetch-field "from")
	     reply-to (or (mail-fetch-field "mail-reply-to" nil t)
			  (mail-fetch-field "reply-to" nil t)
			  from)
	     subject (mail-fetch-field "subject")
	     date (mail-fetch-field "date")
	     message-id (mail-fetch-field "message-id")
	     references (mail-fetch-field "references" nil nil t)
	     resent-reply-to (mail-fetch-field "resent-reply-to" nil t)
	     ;; Bug#512.  It's inappropriate to reply to these addresses.
;;;	     resent-cc (and (not just-sender)
;;;			    (mail-fetch-field "resent-cc" nil t))
;;;	     resent-to (or (mail-fetch-field "resent-to" nil t) "")
;;;	     resent-subject (mail-fetch-field "resent-subject")
;;;	     resent-date (mail-fetch-field "resent-date")
;;;	     resent-message-id (mail-fetch-field "resent-message-id")
	     )
       (unless just-sender
	 (if (mail-fetch-field "mail-followup-to" nil t)
	     ;; If this header field is present, use it instead of the
	     ;; To and CC fields.
	     (setq to (mail-fetch-field "mail-followup-to" nil t))
	   (setq cc (or (mail-fetch-field "cc" nil t) "")
		 to (or (mail-fetch-field "to" nil t) ""))))))
    ;; Merge the resent-to and resent-cc into the to and cc.
    ;; Bug#512.  It's inappropriate to reply to these addresses.
;;;    (if (and resent-to (not (equal resent-to "")))
;;;	(if (not (equal to ""))
;;;	    (setq to (concat to ", " resent-to))
;;;	  (setq to resent-to)))
;;;    (if (and resent-cc (not (equal resent-cc "")))
;;;	(if (not (equal cc ""))
;;;	    (setq cc (concat cc ", " resent-cc))
;;;	  (setq cc resent-cc)))
    ;; Add `Re: ' to subject if not there already.
    (and (stringp subject)
	 (setq subject
	       (concat rmail-reply-prefix
		       (if (let ((case-fold-search t))
			     (string-match rmail-reply-regexp subject))
			   (substring subject (match-end 0))
			 subject))))
    (rmail-start-mail
     nil
     ;; Using mail-strip-quoted-names is undesirable with newer mailers
     ;; since they can handle the names unstripped.
     ;; I don't know whether there are other mailers that still
     ;; need the names to be stripped.
;;;     (mail-strip-quoted-names reply-to)
     ;; Remove unwanted names from reply-to, since Mail-Followup-To
     ;; header causes all the names in it to wind up in reply-to, not
     ;; in cc.  But if what's left is an empty list, use the original.
     (let* ((reply-to-list (mail-dont-reply-to reply-to)))
       (if (string= reply-to-list "") reply-to reply-to-list))
     subject
     (rmail-make-in-reply-to-field from date message-id)
     (if just-sender
	 nil
       ;; `mail-dont-reply-to' doesn't need `mail-strip-quoted-names'.
       (let* ((cc-list (mail-dont-reply-to
			(mail-strip-quoted-names
			 (if (null cc) to (concat to ", " cc))))))
	 (if (string= cc-list "") nil cc-list)))
     rmail-buffer
     (list (list 'rmail-mark-message
		 rmail-buffer
		 (with-current-buffer rmail-buffer
		   (aref rmail-msgref-vector msgnum))
		 rmail-answered-attr-index))
     nil
     (if (or references message-id)
	 (list (cons "References" (if references
				      (concat
				       (mapconcat 'identity references " ")
				       " " message-id)
				    message-id)))))))

(defun rmail-mark-message (buffer msgnum-list attribute)
  "Give BUFFER's message number in MSGNUM-LIST the attribute ATTRIBUTE.
This is use in the send-actions for message buffers.
MSGNUM-LIST is a list of the form (MSGNUM)
which is an element of rmail-msgref-vector."
  (with-current-buffer buffer
    (if (car msgnum-list)
	(rmail-set-attribute attribute t (car msgnum-list)))))

(defun rmail-make-in-reply-to-field (from date message-id)
  (cond ((not from)
         (if message-id
             message-id
             nil))
        (mail-use-rfc822
         (require 'rfc822)
         (let ((tem (car (rfc822-addresses from))))
           (if message-id
               (if (or (not tem)
		       (string-match
			(regexp-quote (if (string-match "@[^@]*\\'" tem)
					  (substring tem 0
						     (match-beginning 0))
					tem))
			message-id))
                   ;; missing From, or Message-ID is sufficiently informative
                   message-id
                   (concat message-id " (" tem ")"))
	     ;; Copy TEM, discarding text properties.
	     (setq tem (copy-sequence tem))
	     (set-text-properties 0 (length tem) nil tem)
	     (setq tem (copy-sequence tem))
	     ;; Use prin1 to fake RFC822 quoting
	     (let ((field (prin1-to-string tem)))
	       (if date
		   (concat field "'s message of " date)
		   field)))))
        ((let* ((foo "[^][\000-\037()<>@,;:\\\" ]+")
                (bar "[^][\000-\037()<>@,;:\\\"]+"))
	   ;; These strings both match all non-ASCII characters.
           (or (string-match (concat "\\`[ \t]*\\(" bar
                                     "\\)\\(<" foo "@" foo ">\\)?[ \t]*\\'")
                             ;; "Unix Loser <Foo@bar.edu>" => "Unix Loser"
                             from)
               (string-match (concat "\\`[ \t]*<" foo "@" foo ">[ \t]*(\\("
                                     bar "\\))[ \t]*\\'")
                             ;; "<Bugs@bar.edu>" (Losing Unix) => "Losing Unix"
                             from)))
         (let ((start (match-beginning 1))
               (end (match-end 1)))
           ;; Trim whitespace which above regexp match allows
           (while (and (< start end)
                       (memq (aref from start) '(?\t ?\s)))
             (setq start (1+ start)))
           (while (and (< start end)
                       (memq (aref from (1- end)) '(?\t ?\s)))
             (setq end (1- end)))
           (let ((field (substring from start end)))
             (if date (setq field (concat "message from " field " on " date)))
             (if message-id
                 ;; "<AA259@bar.edu> (message from Unix Loser on 1-Apr-89)"
                 (concat message-id " (" field ")")
                 field))))
        (t
         ;; If we can't kludge it simply, do it correctly
         (let ((mail-use-rfc822 t))
           (rmail-make-in-reply-to-field from date message-id)))))

(defun rmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
  (if (zerop rmail-current-message)
      (error "No message to forward"))
  (if resend
      (call-interactively 'rmail-resend)
    (let ((forward-buffer rmail-buffer)
	  (msgnum rmail-current-message)
	  (subject (concat "["
			   (let ((from (or (mail-fetch-field "From")
					   (mail-fetch-field ">From"))))
			     (if from
				 (concat (mail-strip-quoted-names from) ": ")
			       ""))
			   (or (mail-fetch-field "Subject") "")
			   "]")))
      (if (rmail-start-mail
	   nil nil subject nil nil rmail-buffer
	   (list (list 'rmail-mark-message
		       forward-buffer
		       (with-current-buffer rmail-buffer
			 (aref rmail-msgref-vector msgnum))
		       rmail-forwarded-attr-index))
	   ;; If only one window, use it for the mail buffer.
	   ;; Otherwise, use another window for the mail buffer
	   ;; so that the Rmail buffer remains visible
	   ;; and sending the mail will get back to it.
	   (and (not rmail-mail-new-frame) (one-window-p t)))
	  ;; The mail buffer is now current.
	  (save-excursion
	    ;; Insert after header separator--before signature if any.
	    (rfc822-goto-eoh)
	    (forward-line 1)
	    (if (and rmail-enable-mime rmail-enable-mime-composing
		     rmail-insert-mime-forwarded-message-function)
		(prog1
		    (funcall rmail-insert-mime-forwarded-message-function
			     forward-buffer)
		  ;; rmail-insert-mime-forwarded-message-function
		  ;; works by inserting MML tags into forward-buffer.
		  ;; The MUA will need to convert it to MIME before
		  ;; sending.  mail-encode-mml tells them to do that.
		  ;; message.el does that automagically.
		  (or (eq mail-user-agent 'message-user-agent)
		      (setq mail-encode-mml t)))
	      (insert "------- Start of forwarded message -------\n")
	      ;; Quote lines with `- ' if they start with `-'.
	      (let ((beg (point)) end)
		(setq end (point-marker))
		(set-marker-insertion-type end t)
		(insert-buffer-substring forward-buffer)
		(goto-char beg)
		(while (re-search-forward "^-" end t)
		  (beginning-of-line)
		  (insert "- ")
		  (forward-line 1))
		(goto-char end)
		(skip-chars-backward "\n")
		(if (< (point) end)
		    (forward-char 1))
		(delete-region (point) end)
		(set-marker end nil))
	      (insert "------- End of forwarded message -------\n"))
	    (push-mark))))))

(defun rmail-resend (address &optional from comment mail-alias-file)
  "Resend current message to ADDRESSES.
ADDRESSES should be a single address, a string consisting of several
addresses separated by commas, or a list of addresses.

Optional FROM is the address to resend the message from, and
defaults from the value of `user-mail-address'.
Optional COMMENT is a string to insert as a comment in the resent message.
Optional ALIAS-FILE is alternate aliases file to be used by sendmail,
typically for purposes of moderating a list."
  (interactive "sResend to: ")
  (require 'sendmail)
  (require 'mailalias)
  (unless (or (eq rmail-view-buffer (current-buffer))
	      (eq rmail-buffer (current-buffer)))
    (error "Not an Rmail buffer"))
  (if (not from) (setq from user-mail-address))
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	(mail-personal-alias-file
	 (or mail-alias-file mail-personal-alias-file))
	(mailbuf rmail-buffer))
    (unwind-protect
	(with-current-buffer tembuf
	  ;;>> Copy message into temp buffer
	  (if (and rmail-enable-mime
		   rmail-insert-mime-resent-message-function)
                  (funcall rmail-insert-mime-resent-message-function mailbuf)
	    (insert-buffer-substring mailbuf))
	  (goto-char (point-min))
	  ;; Delete any Sender field, since that's not specifiable.
	  ; Only delete Sender fields in the actual header.
	  (re-search-forward "^$" nil 'move)
	  ; Using "while" here rather than "if" because some buggy mail
	  ; software may have inserted multiple Sender fields.
	  (while (re-search-backward "^Sender:" nil t)
	    (let (beg)
	      (setq beg (point))
	      (forward-line 1)
	      (while (looking-at "[ \t]")
		(forward-line 1))
	      (delete-region beg (point))))
	  ; Go back to the beginning of the buffer so the Resent- fields
	  ; are inserted there.
	  (goto-char (point-min))
	  ;;>> Insert resent-from:
	  (insert "Resent-From: " from "\n")
	  (insert "Resent-Date: " (mail-rfc822-date) "\n")
	  ;;>> Insert resent-to: and bcc if need be.
	  (let ((before (point)))
	    (if mail-self-blind
		(insert "Resent-Bcc: " (user-login-name) "\n"))
	    (insert "Resent-To: " (if (stringp address)
			       address
			     (mapconcat 'identity address ",\n\t"))
		    "\n")
	    ;; Expand abbrevs in the recipients.
	    (save-excursion
	      (if (featurep 'mailabbrev)
		  (let ((end (point-marker))
			(local-abbrev-table mail-abbrevs)
			(old-syntax-table (syntax-table)))
		    (if (and (not (vectorp mail-abbrevs))
			     (file-exists-p mail-personal-alias-file))
			(build-mail-abbrevs))
		    (unless mail-abbrev-syntax-table
		      (mail-abbrev-make-syntax-table))
		    (set-syntax-table mail-abbrev-syntax-table)
		    (goto-char before)
		    (while (and (< (point) end)
				(progn (forward-word 1)
				       (<= (point) end)))
		      (expand-abbrev))
		    (set-syntax-table old-syntax-table))
		(expand-mail-aliases before (point)))))
	  ;;>> Set up comment, if any.
	  (if (and (sequencep comment) (not (zerop (length comment))))
	      (let ((before (point))
		    after)
		(insert comment)
		(or (eolp) (insert "\n"))
		(setq after (point))
		(goto-char before)
		(while (< (point) after)
		  (insert "Resent-Comment: ")
		  (forward-line 1))))
	  ;; Don't expand aliases in the destination fields
	  ;; of the original message.
	  (let (mail-aliases)
	    (funcall send-mail-function)))
      (kill-buffer tembuf))
    (with-current-buffer rmail-buffer
      (rmail-set-attribute rmail-resent-attr-index t rmail-current-message))))

(defvar mail-unsent-separator
  (concat "^ *---+ +Unsent message follows +---+ *$\\|"
	  "^ *---+ +Returned message +---+ *$\\|"
	  "^ *---+ *Returned mail follows *---+ *$\\|"
	  "^Start of returned message$\\|"
	  "^---+ Below this line is a copy of the message.$\\|"
	  "^ *---+ +Original message +---+ *$\\|"
	  "^ *--+ +begin message +--+ *$\\|"
	  "^ *---+ +Original message follows +---+ *$\\|"
	  "^ *---+ +Your message follows +---+ *$\\|"
	  "^|? *---+ +Message text follows: +---+ *|?$\\|"
	  "^ *---+ +This is a copy of \\w+ message, including all the headers.*---+ *$")
  "A regexp that matches the separator before the text of a failed message.")

(defvar mail-mime-unsent-header "^Content-Type: message/rfc822 *$"
 "A regexp that matches the header of a MIME body part with a failed message.")

;; This is a cut-down version of rmail-clear-headers from Emacs 22.
;; It doesn't have the same functionality, hence the name change.
(defun rmail-delete-headers (regexp)
  "Delete any mail headers matching REGEXP.
The message should be narrowed to just the headers."
  (when regexp
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (beginning-of-line)
      ;; This code from Emacs 22 doesn't seem right, since r-n-h is
      ;; just for display.
;;;      (if (looking-at rmail-nonignored-headers)
;;;	  (forward-line 1)
      (delete-region (point)
		     (save-excursion
		       (if (re-search-forward "\n[^ \t]" nil t)
			   (1- (point))
			 (point-max)))))))

(autoload 'mail-position-on-field "sendmail")

(defun rmail-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message.
If the failed message is a MIME multipart message, it is searched for a
body part with a header which matches the variable `mail-mime-unsent-header'.
Otherwise, the variable `mail-unsent-separator' should match the string that
delimits the returned original message.
The variable `rmail-retry-ignored-headers' is a regular expression
specifying headers which should not be copied into the new message."
  (interactive)
  (require 'mail-utils)
  (let ((rmail-this-buffer (current-buffer))
	(msgnum rmail-current-message)
	bounce-start bounce-end bounce-indent resending
	(content-type (rmail-get-header "Content-Type")))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(if (and content-type
		 (string-match
		  ";[\n\t ]*boundary=\"?\\([-0-9a-z'()+_,./:=? ]+\\)\"?"
		  content-type))
	    ;; Handle a MIME multipart bounce message.
	    (let ((codestring
		   (concat "\n--"
			   (substring content-type (match-beginning 1)
				      (match-end 1)))))
	      (unless (re-search-forward mail-mime-unsent-header nil t)
		(error "Cannot find beginning of header in failed message"))
	      (unless (search-forward "\n\n" nil t)
		(error "Cannot find start of Mime data in failed message"))
	      (setq bounce-start (point))
	      (if (search-forward codestring nil t)
		  (setq bounce-end (match-beginning 0))
		(setq bounce-end (point-max))))
	  ;; Non-MIME bounce.
	  (or (re-search-forward mail-unsent-separator nil t)
	      (error "Cannot parse this as a failure message"))
	  (skip-chars-forward "\n")
	  ;; Support a style of failure message in which the original
	  ;; message is indented, and included within lines saying
	  ;; `Start of returned message' and `End of returned message'.
	  (if (looking-at " +Received:")
	      (progn
		(setq bounce-start (point))
		(skip-chars-forward " ")
		(setq bounce-indent (- (current-column)))
		(goto-char (point-max))
		(re-search-backward "^End of returned message$" nil t)
		(setq bounce-end (point)))
	    ;; One message contained a few random lines before
	    ;; the old message header.  The first line of the
	    ;; message started with two hyphens.  A blank line
	    ;; followed these random lines.  The same line
	    ;; beginning with two hyphens was possibly marking
	    ;; the end of the message.
	    (if (looking-at "^--")
		(let ((boundary (buffer-substring-no-properties
				 (point)
				 (progn (end-of-line) (point)))))
		  (search-forward "\n\n")
		  (skip-chars-forward "\n")
		  (setq bounce-start (point))
		  (goto-char (point-max))
		  (search-backward (concat "\n\n" boundary) bounce-start t)
		  (setq bounce-end (point)))
	      (setq bounce-start (point)
		    bounce-end (point-max)))
	    (unless (search-forward "\n\n" nil t)
	      (error "Cannot find end of header in failed message"))))))
    ;; We have found the message that bounced, within the current message.
    ;; Now start sending new message; default header fields from original.
    ;; Turn off the usual actions for initializing the message body
    ;; because we want to get only the text from the failure message.
    (let (mail-signature mail-setup-hook)
      (if (rmail-start-mail nil nil nil nil nil rmail-this-buffer
			    (list (list 'rmail-mark-message
					rmail-this-buffer
					(aref rmail-msgref-vector msgnum)
					rmail-retried-attr-index)))
	  ;; Insert original text as initial text of new draft message.
	  ;; Bind inhibit-read-only since the header delimiter
	  ;; of the previous message was probably read-only.
	  (let ((inhibit-read-only t)
		eoh)
	    (erase-buffer)
	    (insert-buffer-substring rmail-this-buffer
				     bounce-start bounce-end)
	    (goto-char (point-min))
	    (if bounce-indent
		(indent-rigidly (point-min) (point-max) bounce-indent))
	    (rfc822-goto-eoh)
	    (setq eoh (point))
	    (insert mail-header-separator)
	    (save-restriction
	      (narrow-to-region (point-min) eoh)
	      (rmail-delete-headers rmail-retry-ignored-headers)
	      (rmail-delete-headers "^\\(sender\\|return-path\\|received\\):")
	      (setq resending (mail-fetch-field "resent-to"))
	      (if mail-self-blind
		  (if resending
		      (insert "Resent-Bcc: " (user-login-name) "\n")
		    (insert "BCC: " (user-login-name) "\n"))))
	    (goto-char (point-min))
	    (mail-position-on-field (if resending "Resent-To" "To") t))))))

(defun rmail-summary-exists ()
  "Non-nil if in an RMAIL buffer and an associated summary buffer exists.
In fact, the non-nil value returned is the summary buffer itself."
  (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
       rmail-summary-buffer))

(defun rmail-summary-displayed ()
  "t if in RMAIL buffer and an associated summary buffer is displayed."
  (and rmail-summary-buffer (get-buffer-window rmail-summary-buffer)))

(defcustom rmail-redisplay-summary nil
  "Non-nil means Rmail should show the summary when it changes.
This has an effect only if a summary buffer exists."
  :type 'boolean
  :group 'rmail-summary)

(defcustom rmail-summary-window-size nil
  "Non-nil means specify the height for an Rmail summary window."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'rmail-summary)

;; Put the summary buffer back on the screen, if user wants that.
(defun rmail-maybe-display-summary ()
  (let ((selected (selected-window))
	window)
    ;; If requested, make sure the summary is displayed.
    (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
	 rmail-redisplay-summary
	 (if (get-buffer-window rmail-summary-buffer 0)
	     ;; It's already in some frame; show that one.
	     (let ((frame (window-frame
			   (get-buffer-window rmail-summary-buffer 0))))
	       (make-frame-visible frame)
	       (raise-frame frame))
	   (display-buffer rmail-summary-buffer)))
    ;; If requested, set the height of the summary window.
    (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
	 rmail-summary-window-size
	 (setq window (get-buffer-window rmail-summary-buffer))
	 ;; Don't try to change the size if just one window in frame.
	 (not (eq window (frame-root-window (window-frame window))))
	 (unwind-protect
	     (progn
	       (select-window window)
	       (enlarge-window (- rmail-summary-window-size (window-height))))
	   (select-window selected)))))

;;;; *** Rmail Local Fontification ***

(defun rmail-fontify-buffer-function ()
  ;; This function's symbol is bound to font-lock-fontify-buffer-function.
  (add-hook 'rmail-show-message-hook 'rmail-fontify-message nil t)
  ;; If we're already showing a message, fontify it now.
  (if rmail-current-message (rmail-fontify-message))
  ;; Prevent Font Lock mode from kicking in.
  (setq font-lock-fontified t))

(defun rmail-unfontify-buffer-function ()
  ;; This function's symbol is bound to font-lock-fontify-unbuffer-function.
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t) (inhibit-read-only t)
	before-change-functions after-change-functions
	buffer-file-name buffer-file-truename)
    (save-restriction
      (widen)
      (remove-hook 'rmail-show-message-hook 'rmail-fontify-message t)
      (remove-text-properties (point-min) (point-max) '(rmail-fontified nil))
      (font-lock-default-unfontify-buffer)
      (and (not modified) (buffer-modified-p)
           (restore-buffer-modified-p nil)))))

(defun rmail-fontify-message ()
  ;; Fontify the current message if it is not already fontified.
  (if (text-property-any (point-min) (point-max) 'rmail-fontified nil)
      (let ((modified (buffer-modified-p))
	    (buffer-undo-list t) (inhibit-read-only t)
	    before-change-functions after-change-functions
	    buffer-file-name buffer-file-truename)
	(save-excursion
	  (save-match-data
	    (add-text-properties (point-min) (point-max) '(rmail-fontified t))
	    (font-lock-fontify-region (point-min) (point-max))
	    (and (not modified) (buffer-modified-p)
                 (restore-buffer-modified-p nil)))))))

;;; Speedbar support for RMAIL files.
(eval-when-compile (require 'speedbar))

(defvar rmail-speedbar-match-folder-regexp "^[A-Z0-9]+\\(\\.[A-Z0-9]+\\)?$"
  "*This regex is used to match folder names to be displayed in speedbar.
Enabling this will permit speedbar to display your folders for easy
browsing, and moving of messages.")

(defvar rmail-speedbar-last-user nil
  "The last user to be displayed in the speedbar.")

(defvar rmail-speedbar-key-map nil
  "Keymap used when in rmail display mode.")

(defun rmail-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance rmail."
  (if rmail-speedbar-key-map
      nil
    (setq rmail-speedbar-key-map (speedbar-make-specialized-keymap))

    (define-key rmail-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "r" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "M"
      'rmail-speedbar-move-message-to-folder-on-line)))

;; Mouse-3.
(defvar rmail-speedbar-menu-items
  '(["Read Folder" speedbar-edit-line t]
    ["Move message to folder" rmail-speedbar-move-message-to-folder-on-line
     (save-excursion (beginning-of-line)
		     (looking-at "<M> "))])
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (rmail-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'rmail-install-speedbar-variables))

(defun rmail-speedbar-buttons (buffer)
  "Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder."
  (let ((from nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (if (not (re-search-forward "^Reply-To: " nil t))
	  (if (not (re-search-forward "^From:? " nil t))
	      (setq from t)))
      (if from
	  nil
	(setq from (buffer-substring (point) (line-end-position)))))
    (goto-char (point-min))
    (if (and (looking-at "Reply to:")
	     (equal from rmail-speedbar-last-user))
	nil
      (setq rmail-speedbar-last-user from)
      (erase-buffer)
      (insert "Reply To:\n")
      (if (stringp from)
	  (speedbar-insert-button from 'speedbar-directory-face 'highlight
				  'rmail-speedbar-button 'rmail-reply))
      (insert "Folders:\n")
      (let* ((case-fold-search nil)
	     (df (directory-files (with-current-buffer buffer
                                    default-directory)
				  nil rmail-speedbar-match-folder-regexp)))
	(dolist (file df)
	  (when (file-regular-p file)
	    (speedbar-insert-button "<M>" 'speedbar-button-face 'highlight
				    'rmail-speedbar-move-message file)
	    (speedbar-insert-button file 'speedbar-file-face 'highlight
				    'rmail-speedbar-find-file nil t)))))))

(defun rmail-speedbar-button (text token indent)
  "Execute an rmail command specified by TEXT.
The command used is TOKEN.  INDENT is not used."
  (speedbar-with-attached-buffer
   (funcall token t)))

(defun rmail-speedbar-find-file (text token indent)
  "Load in the rmail file TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Loading in RMAIL file %s..." text)
   (rmail text)))

(defun rmail-speedbar-move-message-to-folder-on-line ()
  "If the current line is a folder, move current message to it."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "<M> " (line-end-position) t)
	(progn
	  (forward-char -2)
	  (speedbar-do-function-pointer)))))

(defun rmail-speedbar-move-message (text token indent)
  "From button TEXT, copy current message to the rmail file specified by TOKEN.
TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Moving message to %s" token)
   ;; expand-file-name is needed due to the unhelpful way in which
   ;; rmail-output expands non-absolute filenames against rmail-default-file.
   ;; What is the point of that, anyway?
   (rmail-output (expand-file-name token))))

;; Functions for setting, getting and encoding the POP password.
;; The password is encoded to prevent it from being easily accessible
;; to "prying eyes."  Obviously, this encoding isn't "real security,"
;; nor is it meant to be.

;;;###autoload
(defun rmail-set-remote-password (password)
  "Set PASSWORD to be used for retrieving mail from a POP or IMAP server."
  (interactive "sPassword: ")
  (if password
      (setq rmail-encoded-remote-password
	    (rmail-encode-string password (emacs-pid)))
    (setq rmail-remote-password nil)
    (setq rmail-encoded-remote-password nil)))

(defun rmail-get-remote-password (imap)
  "Get the password for retrieving mail from a POP or IMAP server.  If none
has been set, then prompt the user for one."
  (when (not rmail-encoded-remote-password)
    (if (not rmail-remote-password)
	(setq rmail-remote-password
	      (read-passwd (if imap
			       "IMAP password: "
			     "POP password: "))))
    (rmail-set-remote-password rmail-remote-password)
    (setq rmail-remote-password nil))
  (rmail-encode-string rmail-encoded-remote-password (emacs-pid)))

(defun rmail-have-password ()
  (or rmail-remote-password rmail-encoded-remote-password))

(defun rmail-encode-string (string mask)
 "Encode STRING with integer MASK, by taking the exclusive OR of the
lowest byte in the mask with the first character of string, the
second-lowest-byte with the second character of the string, etc.,
restarting at the lowest byte of the mask whenever it runs out.
Returns the encoded string.  Calling the function again with an
encoded string (and the same mask) will decode the string."
 (setq mask (abs mask))			; doesn't work if negative
 (let* ((string-vector (string-to-vector string)) (i 0)
	(len (length string-vector)) (curmask mask) charmask)
   (while (< i len)
     (if (= curmask 0)
	 (setq curmask mask))
     (setq charmask (% curmask 256))
     (setq curmask (lsh curmask -8))
     (aset string-vector i (logxor charmask (aref string-vector i)))
     (setq i (1+ i)))
   (concat string-vector)))

;; Should this have a key-binding, or be in a menu?
;; There doesn't really seem to be an appropriate menu.
;; Eg the edit command is not in a menu either.
(defun rmail-epa-decrypt ()
  "Decrypt OpenPGP armors in current message."
  (interactive)

  ;; Save the current buffer here for cleanliness, in case we
  ;; change it in one of the calls to `epa-decrypt-region'.

  (save-excursion
    (let (decrypts)
      (goto-char (point-min))

      ;; In case the encrypted data is inside a mime attachment,
      ;; show it.  This is a kludge; to be clean, it should not
      ;; modify the buffer, but I don't see how to do that.
      (when (search-forward "octet-stream" nil t)
	(beginning-of-line)
	(forward-button 1)
	(if (looking-at "Show")
	    (rmail-mime-toggle-hidden)))

      ;; Now find all armored messages in the buffer
      ;; and decrypt them one by one.
      (goto-char (point-min))
      (while (re-search-forward "-----BEGIN PGP MESSAGE-----$" nil t)
	(let ((coding-system-for-read coding-system-for-read)
	      armor-start armor-end after-end)
	  (setq armor-start (match-beginning 0)
		armor-end (re-search-forward "^-----END PGP MESSAGE-----$"
					     nil t))
	  (unless armor-end
	    (error "Encryption armor beginning has no matching end"))
	  (goto-char armor-start)

	  ;; Because epa--find-coding-system-for-mime-charset not autoloaded.
	  (require 'epa)

	  ;; Use the charset specified in the armor.
	  (unless coding-system-for-read
	    (if (re-search-forward "^Charset: \\(.*\\)" armor-end t)
		(setq coding-system-for-read
		      (epa--find-coding-system-for-mime-charset
		       (intern (downcase (match-string 1)))))))

	  ;; Advance over this armor.
	  (goto-char armor-end)
	  (setq after-end (- (point-max) armor-end))

	  ;; Decrypt it, maybe in place, maybe making new buffer.
	  (epa-decrypt-region
	   armor-start armor-end
	   ;; Call back this function to prepare the output.
	   (lambda ()
	     (let ((inhibit-read-only t))
	       (delete-region armor-start armor-end)
	       (goto-char armor-start)
	       (current-buffer))))

	  (push (list armor-start (- (point-max) after-end))
		decrypts)))

      (when (and decrypts (rmail-buffers-swapped-p))
	(when (y-or-n-p "Replace the original message? ")
	  (setq decrypts (nreverse decrypts))
	  (let ((beg (rmail-msgbeg rmail-current-message))
		(end (rmail-msgend rmail-current-message))
		(from-buffer (current-buffer)))
	    (with-current-buffer rmail-view-buffer
	      (narrow-to-region beg end)
	      (goto-char (point-min))
	      (dolist (d decrypts)
		(if (re-search-forward "-----BEGIN PGP MESSAGE-----$" nil t)
		    (let (armor-start armor-end)
		      (setq armor-start (match-beginning 0)
			    armor-end (re-search-forward "^-----END PGP MESSAGE-----$"
							 nil t))
		      (when armor-end
			(delete-region armor-start armor-end)
			(insert-buffer-substring from-buffer (nth 0 d) (nth 1 d)))))))))))))

;;;;  Desktop support

(defun rmail-restore-desktop-buffer (desktop-buffer-file-name
				     desktop-buffer-name
				     desktop-buffer-misc)
  "Restore an rmail buffer specified in a desktop file."
  (condition-case error
      (progn
	(rmail-input desktop-buffer-file-name)
	(if (eq major-mode 'rmail-mode)
	    (current-buffer)
	  rmail-buffer))
    (file-locked
      (kill-buffer (current-buffer))
      nil)))

(add-to-list 'desktop-buffer-mode-handlers
	     '(rmail-mode . rmail-restore-desktop-buffer))

;; We use this to record the encoding of the current message before
;; saving the message collection.
(defvar rmail-message-encoding nil)

;; Used in `write-region-annotate-functions' to write rmail files.
(defun rmail-write-region-annotate (start end)
  (when (and (null start) rmail-buffer-swapped)
    (unless (buffer-live-p rmail-view-buffer)
      (error "Buffer `%s' with real text of `%s' has disappeared"
	     (buffer-name rmail-view-buffer)
	     (buffer-name (current-buffer))))
    (setq rmail-message-encoding buffer-file-coding-system)
    (set-buffer rmail-view-buffer)
    (widen)
    nil))

;; Used to restore the encoding of the buffer where we show the
;; current message, after we save the message collection.  This is
;; needed because rmail-write-region-annotate switches buffers behind
;; save-file's back, with the side effect that last-coding-system-used
;; is assigned to buffer-file-coding-system of the wrong buffer.
(defun rmail-after-save-hook ()
  (if (or (eq rmail-view-buffer (current-buffer))
	  (eq rmail-buffer (current-buffer)))
      (with-current-buffer
	  (if (rmail-buffers-swapped-p) rmail-buffer rmail-view-buffer)
	(setq buffer-file-coding-system rmail-message-encoding))))
(add-hook 'after-save-hook 'rmail-after-save-hook)


;;; Start of automatically extracted autoloads.

;;;### (autoloads (rmail-edit-current-message) "rmailedit" "rmailedit.el"
;;;;;;  "7d558f958574f6003fa474ce2f3c80a8")
;;; Generated autoloads from rmailedit.el

(autoload 'rmail-edit-current-message "rmailedit" "\
Edit the contents of this message.

\(fn)" t nil)

;;;***

;;;### (autoloads (rmail-next-labeled-message rmail-previous-labeled-message
;;;;;;  rmail-read-label rmail-kill-label rmail-add-label) "rmailkwd"
;;;;;;  "rmailkwd.el" "4ae5660d86d49e524f4a6bcbc6d9a984")
;;; Generated autoloads from rmailkwd.el

(autoload 'rmail-add-label "rmailkwd" "\
Add LABEL to labels associated with current RMAIL message.
Completes (see `rmail-read-label') over known labels when reading.
LABEL may be a symbol or string.  Only one label is allowed.

\(fn LABEL)" t nil)

(autoload 'rmail-kill-label "rmailkwd" "\
Remove LABEL from labels associated with current RMAIL message.
Completes (see `rmail-read-label') over known labels when reading.
LABEL may be a symbol or string.  Only one label is allowed.

\(fn LABEL)" t nil)

(autoload 'rmail-read-label "rmailkwd" "\
Read a label with completion, prompting with PROMPT.
Completions are chosen from `rmail-label-obarray'.  The default
is `rmail-last-label', if that is non-nil.  Updates `rmail-last-label'
according to the choice made, and returns a symbol.

\(fn PROMPT)" nil nil)

(autoload 'rmail-previous-labeled-message "rmailkwd" "\
Show previous message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves backward N messages with these labels.

\(fn N LABELS)" t nil)

(autoload 'rmail-next-labeled-message "rmailkwd" "\
Show next message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves forward N messages with these labels.

\(fn N LABELS)" t nil)

;;;***

;;;### (autoloads (rmail-mime) "rmailmm" "rmailmm.el" "cd7656f82944d0b92b0d093a5f3a4c36")
;;; Generated autoloads from rmailmm.el

(autoload 'rmail-mime "rmailmm" "\
Toggle the display of a MIME message.

The actual behavior depends on the value of `rmail-enable-mime'.

If `rmail-enable-mime' is non-nil (the default), this command toggles
the display of a MIME message between decoded presentation form and
raw data.  With optional prefix argument ARG, it toggles the display only
of the MIME entity at point, if there is one.  The optional argument
STATE forces a particular display state, rather than toggling.
`raw' forces raw mode, any other non-nil value forces decoded mode.

If `rmail-enable-mime' is nil, this creates a temporary \"*RMAIL*\"
buffer holding a decoded copy of the message. Inline content-types are
handled according to `rmail-mime-media-type-handlers-alist'.
By default, this displays text and multipart messages, and offers to
download attachments as specified by `rmail-mime-attachment-dirs-alist'.
The arguments ARG and STATE have no effect in this case.

\(fn &optional ARG STATE)" t nil)

;;;***

;;;### (autoloads (set-rmail-inbox-list) "rmailmsc" "rmailmsc.el"
;;;;;;  "e2212ea15561d60365ffa1f7a5902939")
;;; Generated autoloads from rmailmsc.el

(autoload 'set-rmail-inbox-list "rmailmsc" "\
Set the inbox list of the current RMAIL file to FILE-NAME.
You can specify one file name, or several names separated by commas.
If FILE-NAME is empty, remove any existing inbox list.

This applies only to the current session.

\(fn FILE-NAME)" t nil)

;;;***

;;;### (autoloads (rmail-sort-by-labels rmail-sort-by-lines rmail-sort-by-correspondent
;;;;;;  rmail-sort-by-recipient rmail-sort-by-author rmail-sort-by-subject
;;;;;;  rmail-sort-by-date) "rmailsort" "rmailsort.el" "38da5f17d4ed0dcd2b09c158642cef63")
;;; Generated autoloads from rmailsort.el

(autoload 'rmail-sort-by-date "rmailsort" "\
Sort messages of current Rmail buffer by \"Date\" header.
If prefix argument REVERSE is non-nil, sorts in reverse order.

\(fn REVERSE)" t nil)

(autoload 'rmail-sort-by-subject "rmailsort" "\
Sort messages of current Rmail buffer by \"Subject\" header.
Ignores any \"Re: \" prefix.  If prefix argument REVERSE is
non-nil, sorts in reverse order.

\(fn REVERSE)" t nil)

(autoload 'rmail-sort-by-author "rmailsort" "\
Sort messages of current Rmail buffer by author.
This uses either the \"From\" or \"Sender\" header, downcased.
If prefix argument REVERSE is non-nil, sorts in reverse order.

\(fn REVERSE)" t nil)

(autoload 'rmail-sort-by-recipient "rmailsort" "\
Sort messages of current Rmail buffer by recipient.
This uses either the \"To\" or \"Apparently-To\" header, downcased.
If prefix argument REVERSE is non-nil, sorts in reverse order.

\(fn REVERSE)" t nil)

(autoload 'rmail-sort-by-correspondent "rmailsort" "\
Sort messages of current Rmail buffer by other correspondent.
This uses either the \"From\", \"Sender\", \"To\", or
\"Apparently-To\" header, downcased.  Uses the first header not
excluded by `mail-dont-reply-to-names'.  If prefix argument
REVERSE is non-nil, sorts in reverse order.

\(fn REVERSE)" t nil)

(autoload 'rmail-sort-by-lines "rmailsort" "\
Sort messages of current Rmail buffer by the number of lines.
If prefix argument REVERSE is non-nil, sorts in reverse order.

\(fn REVERSE)" t nil)

(autoload 'rmail-sort-by-labels "rmailsort" "\
Sort messages of current Rmail buffer by labels.
LABELS is a comma-separated list of labels.  The order of these
labels specifies the order of messages: messages with the first
label come first, messages with the second label come second, and
so on.  Messages that have none of these labels come last.
If prefix argument REVERSE is non-nil, sorts in reverse order.

\(fn REVERSE LABELS)" t nil)

;;;***

;;;### (autoloads (rmail-summary-by-senders rmail-summary-by-topic
;;;;;;  rmail-summary-by-regexp rmail-summary-by-recipients rmail-summary-by-labels
;;;;;;  rmail-summary) "rmailsum" "rmailsum.el" "bef21a376bd5bd59792a20dd86e6ec34")
;;; Generated autoloads from rmailsum.el

(autoload 'rmail-summary "rmailsum" "\
Display a summary of all messages, one line per message.

\(fn)" t nil)

(autoload 'rmail-summary-by-labels "rmailsum" "\
Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas.

\(fn LABELS)" t nil)

(autoload 'rmail-summary-by-recipients "rmailsum" "\
Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas.

\(fn RECIPIENTS &optional PRIMARY-ONLY)" t nil)

(autoload 'rmail-summary-by-regexp "rmailsum" "\
Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Emacs will list the message in the summary.

\(fn REGEXP)" t nil)

(autoload 'rmail-summary-by-topic "rmailsum" "\
Display a summary of all messages with the given SUBJECT.
Normally checks just the Subject field of headers; but with prefix
argument WHOLE-MESSAGE is non-nil, looks in the whole message.
SUBJECT is a string of regexps separated by commas.

\(fn SUBJECT &optional WHOLE-MESSAGE)" t nil)

(autoload 'rmail-summary-by-senders "rmailsum" "\
Display a summary of all messages whose \"From\" field matches SENDERS.
SENDERS is a string of regexps separated by commas.

\(fn SENDERS)" t nil)

;;;***

;;;### (autoloads (unforward-rmail-message undigestify-rmail-message)
;;;;;;  "undigest" "undigest.el" "a31a35802a2adbc51be42959c3043dbd")
;;; Generated autoloads from undigest.el

(autoload 'undigestify-rmail-message "undigest" "\
Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages.

\(fn)" t nil)

(autoload 'unforward-rmail-message "undigest" "\
Extract a forwarded message from the containing message.
This puts the forwarded message into a separate rmail message following
the containing message.  This command is only useful when messages are
forwarded with `rmail-enable-mime-composing' set to nil.

\(fn)" t nil)

;;;***

;;; End of automatically extracted autoloads.


(provide 'rmail)

;;; rmail.el ends here
