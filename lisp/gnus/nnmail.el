;;; nnmail.el --- mail support functions for the Gnus mail backends

;; Copyright (C) 1995-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile (require 'cl))

(require 'gnus)				; for macro gnus-kill-buffer, at least
(require 'nnheader)
(require 'message)
(require 'gnus-util)
(require 'mail-source)
(require 'mm-util)
(require 'gnus-int)

(autoload 'gnus-add-buffer "gnus")
(autoload 'gnus-kill-buffer "gnus")

(defgroup nnmail nil
  "Reading mail with Gnus."
  :group 'gnus)

(defgroup nnmail-retrieve nil
  "Retrieving new mail."
  :group 'nnmail)

(defgroup nnmail-prepare nil
  "Preparing (or mangling) new mail after retrieval."
  :group 'nnmail)

(defgroup nnmail-duplicate nil
  "Handling of duplicate mail messages."
  :group 'nnmail)

(defgroup nnmail-split nil
  "Organizing the incoming mail in folders."
  :group 'nnmail)

(defgroup nnmail-files nil
  "Mail files."
  :group 'gnus-files
  :group 'nnmail)

(defgroup nnmail-expire nil
  "Expiring old mail."
  :group 'nnmail)

(defgroup nnmail-procmail nil
  "Interfacing with procmail and other mail agents."
  :group 'nnmail)

(defgroup nnmail-various nil
  "Various mail options."
  :group 'nnmail)

(defcustom nnmail-split-methods '(("mail.misc" ""))
  "*Incoming mail will be split according to this variable.

If you'd like, for instance, one mail group for mail from the
\"4ad-l\" mailing list, one group for junk mail and one for everything
else, you could do something like this:

 (setq nnmail-split-methods
       '((\"mail.4ad\" \"From:.*4ad\")
	 (\"mail.junk\" \"From:.*Lars\\\\|Subject:.*buy\")
	 (\"mail.misc\" \"\")))

As you can see, this variable is a list of lists, where the first
element in each \"rule\" is the name of the group (which, by the way,
does not have to be called anything beginning with \"mail\",
\"yonka.zow\" is a fine, fine name), and the second is a regexp that
nnmail will try to match on the header to find a fit.

The second element can also be a function.  In that case, it will be
called narrowed to the headers with the first element of the rule as
the argument.  It should return a non-nil value if it thinks that the
mail belongs in that group.

The last element should always have \"\" as the regexp.

This variable can also have a function as its value, and it can
also have a fancy split method as its value.  See
`nnmail-split-fancy' for an explanation of that syntax."
  :group 'nnmail-split
  :type '(choice (repeat :tag "Alist" (group (string :tag "Name")
					     (choice regexp function)))
		 (function-item nnmail-split-fancy)
		 (function :tag "Other")))

;; Suggested by Erik Selberg <speed@cs.washington.edu>.
(defcustom nnmail-crosspost t
  "If non-nil, do crossposting if several split methods match the mail.
If nil, the first match found will be used."
  :group 'nnmail-split
  :type 'boolean)

(defcustom nnmail-split-fancy-with-parent-ignore-groups nil
  "Regexp that matches group names to be ignored when applying `nnmail-split-fancy-with-parent'.
This can also be a list of regexps."
  :version "22.1"
  :group 'nnmail-split
  :type '(choice (const :tag "none" nil)
		 (regexp :value ".*")
		 (repeat :value (".*") regexp)))

(defcustom nnmail-cache-ignore-groups nil
  "Regexp that matches group names to be ignored when inserting message ids into the cache (`nnmail-cache-insert').
This can also be a list of regexps."
  :version "22.1"
  :group 'nnmail-split
  :type '(choice (const :tag "none" nil)
		 (regexp :value ".*")
		 (repeat :value (".*") regexp)))

;; Added by gord@enci.ucalgary.ca (Gordon Matzigkeit).
(defcustom nnmail-keep-last-article nil
  "If non-nil, nnmail will never delete/move a group's last article.
It can be marked expirable, so it will be deleted when it is no longer last.

You may need to set this variable if other programs are putting
new mail into folder numbers that Gnus has marked as expired."
  :group 'nnmail-procmail
  :group 'nnmail-various
  :type 'boolean)

(defcustom nnmail-use-long-file-names nil
  "If non-nil the mail backends will use long file and directory names.
If nil, groups like \"mail.misc\" will end up in directories like
\"mail/misc/\"."
  :group 'nnmail-files
  :type 'boolean)

(defcustom nnmail-default-file-modes 384
  "Set the mode bits of all new mail files to this integer."
  :group 'nnmail-files
  :type 'integer)

(defcustom nnmail-expiry-wait 7
  "*Expirable articles that are older than this will be expired.
This variable can either be a number (which will be interpreted as a
number of days) -- this doesn't have to be an integer.  This variable
can also be `immediate' and `never'."
  :group 'nnmail-expire
  :type '(choice (const immediate)
		 (number :tag "days")
		 (const never)))

(defcustom nnmail-expiry-wait-function nil
  "Variable that holds function to specify how old articles should be before they are expired.
The function will be called with the name of the group that the expiry
is to be performed in, and it should return an integer that says how
many days an article can be stored before it is considered \"old\".
It can also return the values `never' and `immediate'.

Eg.:

\(setq nnmail-expiry-wait-function
      (lambda (newsgroup)
	(cond ((string-match \"private\" newsgroup) 31)
	      ((string-match \"junk\" newsgroup) 1)
	      ((string-match \"important\" newsgroup) 'never)
	      (t 7))))"
  :group 'nnmail-expire
  :type '(choice (const :tag "nnmail-expiry-wait" nil)
		 (function :format "%v" nnmail-)))

(defcustom nnmail-expiry-target 'delete
  "*Variable that says where expired messages should end up.
The default value is `delete' (which says to delete the messages),
but it can also be a string or a function.  If it is a string, expired
messages end up in that group.  If it is a function, the function is
called in a buffer narrowed to the message in question.  The function
receives one argument, the name of the group the message comes from.
The return value should be `delete' or a group name (a string)."
  :version "21.1"
  :group 'nnmail-expire
  :type '(choice (const delete)
		 function
		 string))

(defcustom nnmail-fancy-expiry-targets nil
  "Determine expiry target based on articles using fancy techniques.

This is a list of (\"HEADER\" \"REGEXP\" \"TARGET\") entries.  If
`nnmail-expiry-target' is set to the function
`nnmail-fancy-expiry-target' and HEADER of the article matches REGEXP,
the message will be expired to a group determined by invoking
`format-time-string' with TARGET used as the format string and the
time extracted from the articles' Date header (if missing the current
time is used).

In the special cases that HEADER is the symbol `to-from', the regexp
will try to match against both the From and the To header.

Example:

\(setq nnmail-fancy-expiry-targets
      '((to-from \"boss\" \"nnfolder:Work\")
	(\"Subject\" \"IMPORTANT\" \"nnfolder:IMPORTANT.%Y.%b\")
	(\"from\" \".*\" \"nnfolder:Archive-%Y\")))

In this case, articles containing the string \"boss\" in the To or the
From header will be expired to the group \"nnfolder:Work\";
articles containing the string \"IMPORTANT\" in the Subject header will
be expired to the group \"nnfolder:IMPORTANT.YYYY.MMM\"; and
everything else will be expired to \"nnfolder:Archive-YYYY\"."
  :version "22.1"
  :group 'nnmail-expire
  :type '(repeat (list (choice :tag "Match against"
			       (string :tag "Header")
			       (const to-from))
		       regexp
		       (string :tag "Target group format string"))))

(defcustom nnmail-cache-accepted-message-ids nil
  "If non-nil, put Message-IDs of Gcc'd articles into the duplicate cache.
If non-nil, also update the cache when copy or move articles."
  :group 'nnmail
  :type 'boolean)

(make-obsolete-variable 'nnmail-spool-file 'mail-sources
			"Gnus 5.9 (Emacs 22.1)")
;; revision 5.29 / p0-85 / Gnus 5.9
;; Variable removed in No Gnus v0.7

(defcustom nnmail-resplit-incoming nil
  "*If non-nil, re-split incoming procmail sorted mail."
  :group 'nnmail-procmail
  :type 'boolean)

(defcustom nnmail-scan-directory-mail-source-once nil
  "*If non-nil, scan all incoming procmail sorted mails once.
It scans low-level sorted spools even when not required."
  :version "21.1"
  :group 'nnmail-procmail
  :type 'boolean)

(defcustom nnmail-delete-file-function 'delete-file
  "Function called to delete files in some mail backends."
  :group 'nnmail-files
  :type 'function)

(defcustom nnmail-crosspost-link-function
  (if (string-match "windows-nt" (symbol-name system-type))
      'copy-file
    'add-name-to-file)
  "*Function called to create a copy of a file.
This is `add-name-to-file' by default, which means that crossposts
will use hard links.  If your file system doesn't allow hard
links, you could set this variable to `copy-file' instead."
  :group 'nnmail-files
  :type '(radio (function-item add-name-to-file)
		(function-item copy-file)
		(function :tag "Other")))

(defcustom nnmail-read-incoming-hook
  (if (eq system-type 'windows-nt)
      '(nnheader-ms-strip-cr)
    nil)
  "*Hook that will be run after the incoming mail has been transferred.
The incoming mail is moved from the specified spool file (which normally is
something like \"/usr/spool/mail/$user\") to the user's home
directory.  This hook is called after the incoming mail box has been
emptied, and can be used to call any mail box programs you have
running (\"xwatch\", etc.)

Eg.

\(add-hook 'nnmail-read-incoming-hook
	  (lambda ()
	    (call-process \"/local/bin/mailsend\" nil nil nil
			  \"read\"
			  ;; The incoming mail box file.
			  (expand-file-name (user-login-name)
					    rmail-spool-directory))))

If you have xwatch running, this will alert it that mail has been
read.

If you use `display-time', you could use something like this:

\(add-hook 'nnmail-read-incoming-hook
	  (lambda ()
	    ;; Update the displayed time, since that will clear out
	    ;; the flag that says you have mail.
	    (when (eq (process-status \"display-time\") 'run)
	      (display-time-filter display-time-process \"\"))))"
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-prepare-incoming-hook nil
  "Hook called before treating incoming mail.
The hook is run in a buffer with all the new, incoming mail."
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-prepare-incoming-header-hook nil
  "Hook called narrowed to the headers of each message.
This can be used to remove excessive spaces (and stuff like
that) from the headers before splitting and saving the messages."
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-prepare-incoming-message-hook nil
  "Hook called narrowed to each message."
  :group 'nnmail-prepare
  :type 'hook)

(defcustom nnmail-list-identifiers nil
  "Regexp that matches list identifiers to be removed.
This can also be a list of regexps."
  :group 'nnmail-prepare
  :type '(choice (const :tag "none" nil)
		 (regexp :value ".*")
		 (repeat :value (".*") regexp)))

(defcustom nnmail-pre-get-new-mail-hook nil
  "Hook called just before starting to handle new incoming mail."
  :group 'nnmail-retrieve
  :type 'hook)

(defcustom nnmail-post-get-new-mail-hook nil
  "Hook called just after finishing handling new incoming mail."
  :group 'nnmail-retrieve
  :type 'hook)

(defcustom nnmail-split-hook nil
  "Hook called before deciding where to split an article.
The functions in this hook are free to modify the buffer
contents in any way they choose -- the buffer contents are
discarded after running the split process."
  :group 'nnmail-split
  :type 'hook)

(defcustom nnmail-spool-hook nil
  "*A hook called when a new article is spooled."
  :version "22.1"
  :group 'nnmail
  :type 'hook)

(defcustom nnmail-large-newsgroup 50
  "*The number of articles which indicates a large newsgroup or nil.
If the number of articles is greater than the value, verbose
messages will be shown to indicate the current status."
  :group 'nnmail-various
  :type '(choice (const :tag "infinite" nil)
                 (number :tag "count")))

(define-widget 'nnmail-lazy 'default
  "Base widget for recursive datastructures.

This is copy of the `lazy' widget in Emacs 22.1 provided for compatibility."
  :format "%{%t%}: %v"
  :convert-widget 'widget-value-convert-widget
  :value-create (lambda (widget)
                  (let ((value (widget-get widget :value))
                        (type (widget-get widget :type)))
                    (widget-put widget :children
                                (list (widget-create-child-value
                                       widget (widget-convert type) value)))))
  :value-delete 'widget-children-value-delete
  :value-get (lambda (widget)
               (widget-value (car (widget-get widget :children))))
  :value-inline (lambda (widget)
                  (widget-apply (car (widget-get widget :children))
                                :value-inline))
  :default-get (lambda (widget)
                 (widget-default-get
                  (widget-convert (widget-get widget :type))))
  :match (lambda (widget value)
           (widget-apply (widget-convert (widget-get widget :type))
                         :match value))
  :validate (lambda (widget)
              (widget-apply (car (widget-get widget :children)) :validate)))

(define-widget 'nnmail-split-fancy 'nnmail-lazy
  "Widget for customizing splits in the variable of the same name."
  :tag "Split"
  :type '(menu-choice :value (any ".*value.*" "misc")
                      :tag "Type"
                      (string :tag "Destination")
                      (list :tag "Use first match (|)" :value (|)
                            (const :format "" |)
                            (editable-list :inline t nnmail-split-fancy))
                      (list :tag "Use all matches (&)" :value (&)
                            (const :format "" &)
                            (editable-list :inline t nnmail-split-fancy))
                      (list :tag "Function with fixed arguments (:)"
                            :value (:)
                            (const :format "" :value :)
                            function
                            (editable-list :inline t (sexp :tag "Arg"))
                            )
                      (list :tag "Function with split arguments (!)"
                            :value (!)
                            (const :format "" !)
                            function
                            (editable-list :inline t nnmail-split-fancy))
                      (list :tag "Field match"
                            (choice :tag "Field"
                                    regexp symbol)
                            (choice :tag "Match"
                                    regexp
                                    (symbol :value mail))
                            (repeat :inline t
                                    :tag "Restrictions"
                                    (group :inline t
                                           (const :format "" -)
                                           regexp))
                            nnmail-split-fancy)
                      (const :tag "Junk (delete mail)" junk)))

(defcustom nnmail-split-fancy "mail.misc"
  "Incoming mail can be split according to this fancy variable.
To enable this, set `nnmail-split-methods' to `nnmail-split-fancy'.

The format of this variable is SPLIT, where SPLIT can be one of
the following:

GROUP: Mail will be stored in GROUP (a string).

\(FIELD VALUE [- RESTRICT [- RESTRICT [...]]] SPLIT): If the message
  field FIELD (a regexp) contains VALUE (a regexp), store the messages
  as specified by SPLIT.  If RESTRICT (a regexp) matches some string
  after FIELD and before the end of the matched VALUE, return nil,
  otherwise process SPLIT.  Multiple RESTRICTs add up, further
  restricting the possibility of processing SPLIT.

\(| SPLIT...): Process each SPLIT expression until one of them matches.
  A SPLIT expression is said to match if it will cause the mail
  message to be stored in one or more groups.

\(& SPLIT...): Process each SPLIT expression.

\(: FUNCTION optional args): Call FUNCTION with the optional args, in
  the buffer containing the message headers.  The return value FUNCTION
  should be a split, which is then recursively processed.

\(! FUNCTION SPLIT): Call FUNCTION with the result of SPLIT.  The
  return value FUNCTION should be a split, which is then recursively
  processed.

junk: Mail will be deleted.  Use with care!  Do not submerge in water!
  Example:
  (setq nnmail-split-fancy
	'(| (\"Subject\" \"MAKE MONEY FAST\" junk)
	    ...other.rules.omitted...))

FIELD must match a complete field name.  VALUE must match a complete
word according to the `nnmail-split-fancy-syntax-table' syntax table.
You can use \".*\" in the regexps to match partial field names or words.

FIELD and VALUE can also be Lisp symbols, in that case they are expanded
as specified in `nnmail-split-abbrev-alist'.

GROUP can contain \\& and \\N which will substitute from matching
\\(\\) patterns in the previous VALUE.

Example:

\(setq nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
      ;; Messages from the mailer daemon are not crossposted to any of
      ;; the ordinary groups.  Warnings are put in a separate group
      ;; from real errors.
      '(| (\"from\" mail (| (\"subject\" \"warn.*\" \"mail.warning\")
			  \"mail.misc\"))
	  ;; Non-error messages are crossposted to all relevant
	  ;; groups, but we don't crosspost between the group for the
	  ;; (ding) list and the group for other (ding) related mail.
	  (& (| (any \"ding@ifi\\\\.uio\\\\.no\" \"ding.list\")
		(\"subject\" \"ding\" \"ding.misc\"))
	     ;; Other mailing lists...
	     (any \"procmail@informatik\\\\.rwth-aachen\\\\.de\" \"procmail.list\")
	     (any \"SmartList@informatik\\\\.rwth-aachen\\\\.de\" \"SmartList.list\")
	     ;; Both lists below have the same suffix, so prevent
	     ;; cross-posting to mkpkg.list of messages posted only to
	     ;; the bugs- list, but allow cross-posting when the
	     ;; message was really cross-posted.
	     (any \"bugs-mypackage@somewhere\" \"mypkg.bugs\")
	     (any \"mypackage@somewhere\" - \"bugs-mypackage\" \"mypkg.list\")
	     ;;
	     ;; People...
	     (any \"larsi@ifi\\\\.uio\\\\.no\" \"people.Lars Magne Ingebrigtsen\"))
	  ;; Unmatched mail goes to the catch all group.
	  \"misc.misc\"))"
  :group 'nnmail-split
  :type 'nnmail-split-fancy)

(defcustom nnmail-split-abbrev-alist
  '((any . "from\\|to\\|cc\\|sender\\|apparently-to\\|resent-from\\|resent-to\\|resent-cc")
    (mail . "mailer-daemon\\|postmaster\\|uucp")
    (to . "to\\|cc\\|apparently-to\\|resent-to\\|resent-cc")
    (from . "from\\|sender\\|resent-from")
    (nato . "to\\|cc\\|resent-to\\|resent-cc")
    (naany . "from\\|to\\|cc\\|sender\\|resent-from\\|resent-to\\|resent-cc"))
  "*Alist of abbreviations allowed in `nnmail-split-fancy'."
  :group 'nnmail-split
  :type '(repeat (cons :format "%v" symbol regexp)))

(defcustom nnmail-message-id-cache-length 1000
  "*The approximate number of Message-IDs nnmail will keep in its cache.
If this variable is nil, no checking on duplicate messages will be
performed."
  :group 'nnmail-duplicate
  :type '(choice (const :tag "disable" nil)
		 (integer :format "%v")))

(defcustom nnmail-message-id-cache-file
  (nnheader-concat gnus-home-directory ".nnmail-cache")
  "The file name of the nnmail Message-ID cache."
  :group 'nnmail-duplicate
  :group 'nnmail-files
  :type 'file)

(defcustom nnmail-treat-duplicates 'warn
  "*If non-nil, nnmail keep a cache of Message-IDs to discover mail duplicates.
Three values are valid: nil, which means that nnmail is not to keep a
Message-ID cache; `warn', which means that nnmail should insert extra
headers to warn the user about the duplication (this is the default);
and `delete', which means that nnmail will delete duplicated mails.

This variable can also be a function.  It will be called from a buffer
narrowed to the article in question with the Message-ID as a
parameter.  It should return nil, `warn' or `delete'."
  :group 'nnmail-duplicate
  :type '(choice (const :tag "off" nil)
		 (const warn)
		 (const delete)))

(defcustom nnmail-extra-headers '(To Newsgroups)
  "Extra headers to parse.
In addition to the standard headers, these extra headers will be
included in NOV headers (and the like) when backends parse headers."
  :version "21.1"
  :group 'nnmail
  :type '(repeat symbol))

(defcustom nnmail-split-header-length-limit 2048
  "Header lines longer than this limit are excluded from the split function."
  :version "21.1"
  :group 'nnmail
  :type 'integer)

(defcustom nnmail-mail-splitting-charset nil
  "Default charset to be used when splitting incoming mail."
  :version "22.1"
  :group 'nnmail
  :type 'symbol)

(defcustom nnmail-mail-splitting-decodes nil
  "Whether the nnmail splitting functionality should MIME decode headers."
  :version "22.1"
  :group 'nnmail
  :type 'boolean)

(defcustom nnmail-split-fancy-match-partial-words nil
  "Whether to match partial words when fancy splitting.
Normally, regexes given in `nnmail-split-fancy' are implicitly surrounded
by \"\\=\\<...\\>\".  If this variable is true, they are not implicitly\
 surrounded
by anything."
  :version "22.1"
  :group 'nnmail
  :type 'boolean)

(defcustom nnmail-split-lowercase-expanded t
  "Whether to lowercase expanded entries (i.e. \\N) when splitting mails.
This avoids the creation of multiple groups when users send to an address
using different case (i.e. mailing-list@domain vs Mailing-List@Domain)."
  :version "22.1"
  :group 'nnmail
  :type 'boolean)

;;; Internal variables.

(defvar nnmail-article-buffer " *nnmail incoming*"
  "The buffer used for splitting incoming mails.")

(defvar nnmail-split-history nil
  "List of group/article elements that say where the previous split put messages.")

(defvar nnmail-split-fancy-syntax-table
  (let ((table (make-syntax-table)))
    ;; support the %-hack
    (modify-syntax-entry ?\% "." table)
    table)
  "Syntax table used by `nnmail-split-fancy'.")

(defvar nnmail-prepare-save-mail-hook nil
  "Hook called before saving mail.")

(defvar nnmail-split-tracing nil)
(defvar nnmail-split-trace nil)
(defvar nnmail-inhibit-default-split-group nil)



(defun nnmail-request-post (&optional server)
  (mail-send-and-exit nil))

(defvar nnmail-file-coding-system 'raw-text
  "Coding system used in nnmail.")

(defvar nnmail-incoming-coding-system
  mm-text-coding-system
  "Coding system used in reading inbox")

(defvar nnmail-pathname-coding-system
  ;; This causes Emacs 22.2 and 22.3 to issue a useless warning.
  ;;(if (and (featurep 'xemacs) (featurep 'file-coding))
  (if (featurep 'xemacs)
      (if (featurep 'file-coding)
	  ;; Work around a bug in many XEmacs 21.5 betas.
	  ;; Cf. http://thread.gmane.org/gmane.emacs.gnus.general/68134
	  (setq file-name-coding-system (coding-system-aliasee 'file-name))))
  "*Coding system for file name.")

(defun nnmail-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (delete-region (point-min) (point-max))
  (let ((format-alist nil)
	(after-insert-file-functions nil))
    (condition-case ()
	(let ((coding-system-for-read nnmail-file-coding-system)
	      (auto-mode-alist (mm-auto-mode-alist))
	      (file-name-coding-system nnmail-pathname-coding-system))
	  (insert-file-contents file)
	  t)
      (file-error nil))))

(defun nnmail-group-pathname (group dir &optional file)
  "Make file name for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     (setq group (nnheader-replace-duplicate-chars-in-string
		  (nnheader-replace-chars-in-string group ?/ ?_)
		  ?. ?_))
     (setq group (nnheader-translate-file-chars group))
     ;; If this directory exists, we use it directly.
     (file-name-as-directory
      (if (or nnmail-use-long-file-names
	      (file-directory-p (concat dir group)))
	  (expand-file-name group dir)
	;; If not, we translate dots into slashes.
	(expand-file-name
	 (nnheader-replace-chars-in-string group ?. ?/)
	 dir))))
   (or file "")))

(defun nnmail-get-active ()
  "Returns an assoc of group names and active ranges.
nn*-request-list should have been called before calling this function."
  ;; Go through all groups from the active list.
  (with-current-buffer nntp-server-buffer
    (nnmail-parse-active)))

(defun nnmail-parse-active ()
  "Parse the active file in the current buffer and return an alist."
  (goto-char (point-min))
  (unless (re-search-forward "[\\\"]" nil t)
    (goto-char (point-max))
    (while (re-search-backward "[][';?()#]" nil t)
      (insert ?\\)))
  (goto-char (point-min))
  (let ((buffer (current-buffer))
	group-assoc group max min)
    (while (not (eobp))
      (condition-case err
	  (progn
	    (narrow-to-region (point) (point-at-eol))
	    (setq group (read buffer))
	    (unless (stringp group)
	      (setq group (symbol-name group)))
	    (if (and (numberp (setq max (read buffer)))
		     (numberp (setq min (read buffer))))
		(push (list (mm-string-as-unibyte group) (cons min max))
		      group-assoc)))
	(error nil))
      (widen)
      (forward-line 1))
    group-assoc))

(defvar nnmail-active-file-coding-system 'raw-text
  "*Coding system for active file.")

(defun nnmail-save-active (group-assoc file-name)
  "Save GROUP-ASSOC in ACTIVE-FILE."
  (let ((coding-system-for-write nnmail-active-file-coding-system))
    (when file-name
      (with-temp-file file-name
	(mm-disable-multibyte)
	(nnmail-generate-active group-assoc)))))

(defun nnmail-generate-active (alist)
  "Generate an active file from group-alist ALIST."
  (erase-buffer)
  (let (group)
    (while (setq group (pop alist))
      (insert (format "%S %d %d y\n" (intern (car group)) (cdadr group)
		      (caadr group))))
    (goto-char (point-max))
    (while (search-backward "\\." nil t)
      (delete-char 1))))

(defun nnmail-get-split-group (file source)
  "Find out whether this FILE is to be split into GROUP only.
If SOURCE is a directory spec, try to return the group name component."
  (if (eq (car source) 'directory)
      (let ((file (file-name-nondirectory file)))
	(mail-source-bind (directory source)
	  (if (string-match (concat (regexp-quote suffix) "$") file)
	      (substring file 0 (match-beginning 0))
	    nil)))
    nil))

(defun nnmail-process-babyl-mail-format (func artnum-func)
  (let ((case-fold-search t)
	(count 0)
	start message-id content-length do-search end)
    (while (not (eobp))
      (goto-char (point-min))
      (re-search-forward
       "\n0, *unseen,+\n\\(\\*\\*\\* EOOH \\*\\*\\*\n\\)?" nil t)
      (goto-char (match-end 0))
      (delete-region (match-beginning 0) (match-end 0))
      (narrow-to-region
       (setq start (point))
       (progn
	 ;; Skip all the headers in case there are more "From "s...
	 (or (search-forward "\n\n" nil t)
	     (search-forward-regexp "^[^:]*\\( .*\\|\\)$" nil t)
	     (search-forward ""))
	 (point)))
      ;; Unquote the ">From " line, if any.
      (goto-char (point-min))
      (when (looking-at ">From ")
	(replace-match "X-From-Line: ") )
      (run-hooks 'nnmail-prepare-incoming-header-hook)
      (goto-char (point-max))
      ;; Find the Message-ID header.
      (save-excursion
	(if (re-search-backward
	     "^Message-ID[ \t]*:[ \n\t]*\\(<[^>]*>\\)" nil t)
	    (setq message-id (buffer-substring (match-beginning 1)
					       (match-end 1)))
	  ;; There is no Message-ID here, so we create one.
	  (save-excursion
	    (when (re-search-backward "^Message-ID[ \t]*:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  (forward-line -1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id))
		  "\n")))
      ;; Look for a Content-Length header.
      (if (not (save-excursion
		 (and (re-search-backward
		       "^Content-Length:[ \t]*\\([0-9]+\\)" start t)
		      (setq content-length (string-to-number
					    (buffer-substring
					     (match-beginning 1)
					     (match-end 1))))
		      ;; We destroy the header, since none of
		      ;; the backends ever use it, and we do not
		      ;; want to confuse other mailers by having
		      ;; a (possibly) faulty header.
		      (progn (insert "X-") t))))
	  (setq do-search t)
	(widen)
	(if (or (= (+ (point) content-length) (point-max))
		(save-excursion
		  (goto-char (+ (point) content-length))
		  (looking-at "")))
	    (progn
	      (goto-char (+ (point) content-length))
	      (setq do-search nil))
	  (setq do-search t)))
      (widen)
      ;; Go to the beginning of the next article - or to the end
      ;; of the buffer.
      (when do-search
	(if (re-search-forward "^" nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (1- (point-max)))))
      (delete-char 1)			; delete ^_
      (save-excursion
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char (point-min))
	  (nnmail-check-duplication message-id func artnum-func)
	  (incf count)
	  (setq end (point-max))))
      (goto-char end))
    count))

(defsubst nnmail-search-unix-mail-delim ()
  "Put point at the beginning of the next Unix mbox message."
  ;; Algorithm used to find the next article in the
  ;; brain-dead Unix mbox format:
  ;;
  ;; 1) Search for "^From ".
  ;; 2) If we find it, then see whether the previous
  ;;    line is blank and the next line looks like a header.
  ;; Then it's possible that this is a mail delim, and we use it.
  (let ((case-fold-search nil)
	found)
    (while (not found)
      (if (not (re-search-forward "^From " nil t))
	  (setq found 'no)
	(save-excursion
	  (beginning-of-line)
	  (when (and (or (bobp)
			 (save-excursion
			   (forward-line -1)
			   (eq (char-after) ?\n)))
		     (save-excursion
		       (forward-line 1)
		       (while (looking-at ">From \\|From ")
			 (forward-line 1))
		       (looking-at "[^ \n\t:]+[ \n\t]*:")))
	    (setq found 'yes)))))
    (beginning-of-line)
    (eq found 'yes)))

(defun nnmail-search-unix-mail-delim-backward ()
  "Put point at the beginning of the current Unix mbox message."
  ;; Algorithm used to find the next article in the
  ;; brain-dead Unix mbox format:
  ;;
  ;; 1) Search for "^From ".
  ;; 2) If we find it, then see whether the previous
  ;;    line is blank and the next line looks like a header.
  ;; Then it's possible that this is a mail delim, and we use it.
  (let ((case-fold-search nil)
	found)
    (while (not found)
      (if (not (re-search-backward "^From " nil t))
	  (setq found 'no)
	(save-excursion
	  (beginning-of-line)
	  (when (and (or (bobp)
			 (save-excursion
			   (forward-line -1)
			   (eq (char-after) ?\n)))
		     (save-excursion
		       (forward-line 1)
		       (while (looking-at ">From \\|From ")
			 (forward-line 1))
		       (looking-at "[^ \n\t:]+[ \n\t]*:")))
	    (setq found 'yes)))))
    (beginning-of-line)
    (eq found 'yes)))

(defun nnmail-process-unix-mail-format (func artnum-func)
  (let ((case-fold-search t)
	(count 0)
	start message-id content-length end skip head-end)
    (goto-char (point-min))
    (if (not (and (re-search-forward "^From " nil t)
		  (goto-char (match-beginning 0))))
	;; Possibly wrong format?
	(error "Error, unknown mail format! (Possibly corrupted %s `%s'.)"
	       (if (buffer-file-name) "file" "buffer")
	       (or (buffer-file-name) (buffer-name)))
      ;; Carry on until the bitter end.
      (while (not (eobp))
	(setq start (point)
	      end nil)
	;; Find the end of the head.
	(narrow-to-region
	 start
	 (if (search-forward "\n\n" nil t)
	     (1- (point))
	   ;; This will never happen, but just to be on the safe side --
	   ;; if there is no head-body delimiter, we search a bit manually.
	   (while (and (looking-at "From \\|[^ \t]+:")
		       (not (eobp)))
	     (forward-line 1))
	   (point)))
	;; Find the Message-ID header.
	(goto-char (point-min))
	(if (re-search-forward "^Message-ID[ \t]*:[ \n\t]*\\(<[^>]+>\\)" nil t)
	    (setq message-id (match-string 1))
	  (save-excursion
	    (when (re-search-forward "^Message-ID[ \t]*:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  ;; There is no Message-ID here, so we create one.
	  (forward-line 1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
	;; Look for a Content-Length header.
	(goto-char (point-min))
	(if (not (re-search-forward
		  "^Content-Length:[ \t]*\\([0-9]+\\)" nil t))
	    (setq content-length nil)
	  (setq content-length (string-to-number (match-string 1)))
	  ;; We destroy the header, since none of the backends ever
	  ;; use it, and we do not want to confuse other mailers by
	  ;; having a (possibly) faulty header.
	  (beginning-of-line)
	  (insert "X-"))
	(run-hooks 'nnmail-prepare-incoming-header-hook)
	;; Find the end of this article.
	(goto-char (point-max))
	(widen)
	(setq head-end (point))
	;; We try the Content-Length value.  The idea: skip over the header
	;; separator, then check what happens content-length bytes into the
	;; message body.  This should be either the end of the buffer, the
	;; message separator or a blank line followed by the separator.
	;; The blank line should probably be deleted.  If neither of the
	;; three is met, the content-length header is probably invalid.
	(when content-length
	  (forward-line 1)
	  (setq skip (+ (point) content-length))
	  (goto-char skip)
	  (cond ((or (= skip (point-max))
		     (= (1+ skip) (point-max)))
		 (setq end (point-max)))
		((looking-at "From ")
		 (setq end skip))
		((looking-at "[ \t]*\n\\(From \\)")
		 (setq end (match-beginning 1)))
		(t (setq end nil))))
	(if end
	    (goto-char end)
	  ;; No Content-Length, so we find the beginning of the next
	  ;; article or the end of the buffer.
	  (goto-char head-end)
	  (or (nnmail-search-unix-mail-delim)
	      (goto-char (point-max))))
	;; Allow the backend to save the article.
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    (incf count)
	    (nnmail-check-duplication message-id func artnum-func)
	    (setq end (point-max))))
	(goto-char end)))
    count))

(defun nnmail-process-mmdf-mail-format (func artnum-func &optional junk-func)
  (let ((delim "^\^A\^A\^A\^A$")
	(case-fold-search t)
	(count 0)
	start message-id end)
    (goto-char (point-min))
    (if (not (and (re-search-forward delim nil t)
		  (forward-line 1)))
	;; Possibly wrong format?
	(error "Error, unknown mail format! (Possibly corrupted.)")
      ;; Carry on until the bitter end.
      (while (not (eobp))
	(setq start (point))
	;; Find the end of the head.
	(narrow-to-region
	 start
	 (if (search-forward "\n\n" nil t)
	     (1- (point))
	   ;; This will never happen, but just to be on the safe side --
	   ;; if there is no head-body delimiter, we search a bit manually.
	   (while (and (looking-at "From \\|[^ \t]+:")
		       (not (eobp)))
	     (forward-line 1))
	   (point)))
	;; Find the Message-ID header.
	(goto-char (point-min))
	(if (re-search-forward "^Message-ID[ \t]*:[ \n\t]*\\(<[^>]+>\\)" nil t)
	    (setq message-id (match-string 1))
	  ;; There is no Message-ID here, so we create one.
	  (save-excursion
	    (when (re-search-backward "^Message-ID[ \t]*:" nil t)
	      (beginning-of-line)
	      (insert "Original-")))
	  (forward-line 1)
	  (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
	(run-hooks 'nnmail-prepare-incoming-header-hook)
	;; Find the end of this article.
	(goto-char (point-max))
	(widen)
	(if (re-search-forward delim nil t)
	    (beginning-of-line)
	  (goto-char (point-max)))
	;; Allow the backend to save the article.
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    (incf count)
	    (nnmail-check-duplication message-id func artnum-func junk-func)
	    (setq end (point-max))))
	(goto-char end)
	(forward-line 2)))
    count))

(defun nnmail-process-maildir-mail-format (func artnum-func)
  ;; In a maildir, every file contains exactly one mail.
  (let ((case-fold-search t)
	message-id)
    (goto-char (point-min))
    ;; Find the end of the head.
    (narrow-to-region
     (point-min)
     (if (search-forward "\n\n" nil t)
	 (1- (point))
       ;; This will never happen, but just to be on the safe side --
       ;; if there is no head-body delimiter, we search a bit manually.
       (while (and (looking-at "From \\|[^ \t]+:")
		   (not (eobp)))
	 (forward-line 1))
       (point)))
    ;; Find the Message-ID header.
    (goto-char (point-min))
    (if (re-search-forward "^Message-ID:[ \t]*\\(<[^>]+>\\)" nil t)
	(setq message-id (match-string 1))
      ;; There is no Message-ID here, so we create one.
      (save-excursion
	(when (re-search-backward "^Message-ID[ \t]*:" nil t)
	  (beginning-of-line)
	  (insert "Original-")))
      (forward-line 1)
      (insert "Message-ID: " (setq message-id (nnmail-message-id)) "\n"))
    (run-hooks 'nnmail-prepare-incoming-header-hook)
    ;; Allow the backend to save the article.
    (widen)
    (save-excursion
      (goto-char (point-min))
      (nnmail-check-duplication message-id func artnum-func))
    1))

(defvar nnmail-group-names-not-encoded-p nil
  "Non-nil means group names are not encoded.")

(defun nnmail-split-incoming (incoming func &optional exit-func
				       group artnum-func junk-func)
  "Go through the entire INCOMING file and pick out each individual mail.
FUNC will be called with the buffer narrowed to each mail.
INCOMING can also be a buffer object.  In that case, the mail
will be copied over from that buffer."
  (let ( ;; If this is a group-specific split, we bind the split
	;; methods to just this group.
	(nnmail-split-methods (if (and group
				       (not nnmail-resplit-incoming))
				  (list (list group ""))
				nnmail-split-methods))
	(nnmail-group-names-not-encoded-p t))
    ;; Insert the incoming file.
    (with-current-buffer (get-buffer-create nnmail-article-buffer)
      (erase-buffer)
      (if (bufferp incoming)
	  (insert-buffer-substring incoming)
	(let ((coding-system-for-read nnmail-incoming-coding-system))
	  (mm-insert-file-contents incoming)))
      (prog1
	  (if (zerop (buffer-size))
	      0
	    (goto-char (point-min))
	    (save-excursion (run-hooks 'nnmail-prepare-incoming-hook))
	    ;; Handle both babyl, MMDF and unix mail formats, since
	    ;; movemail will use the former when fetching from a
	    ;; mailbox, the latter when fetching from a file.
	    (cond ((or (looking-at "\^L")
		       (looking-at "BABYL OPTIONS:"))
		   (nnmail-process-babyl-mail-format func artnum-func))
		  ((looking-at "\^A\^A\^A\^A")
		   (nnmail-process-mmdf-mail-format
		    func artnum-func junk-func))
		  ((looking-at "Return-Path:")
		   (nnmail-process-maildir-mail-format func artnum-func))
		  (t
		   (nnmail-process-unix-mail-format func artnum-func))))
	(when exit-func
	  (funcall exit-func))
	(kill-buffer (current-buffer))))))

(defun nnmail-article-group (func &optional trace junk-func)
  "Look at the headers and return an alist of groups that match.
FUNC will be called with the group name to determine the article number."
  (let ((methods (or nnmail-split-methods '(("bogus" ""))))
	(obuf (current-buffer))
	group-art method grp)
    (if (and (sequencep methods)
	     (= (length methods) 1)
	     (not nnmail-inhibit-default-split-group))
	;; If there is only just one group to put everything in, we
	;; just return a list with just this one method in.
	(setq group-art
	      (list (cons (caar methods) (funcall func (caar methods)))))
      ;; We do actual comparison.
      ;; Copy the article into the work buffer.
      (with-current-buffer nntp-server-buffer
	(erase-buffer)
	(insert-buffer-substring obuf)
	;; Narrow to headers.
	(narrow-to-region
	 (goto-char (point-min))
	 (if (search-forward "\n\n" nil t)
	     (point)
	   (point-max)))
	(goto-char (point-min))
	;; Decode MIME headers and charsets.
	(when nnmail-mail-splitting-decodes
	  (let ((mail-parse-charset nnmail-mail-splitting-charset))
	    (mail-decode-encoded-word-region (point-min) (point-max))))
	;; Fold continuation lines.
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	;; Nuke pathologically long headers.  Since Gnus applies
	;; pathologically complex regexps to the buffer, lines
	;; that are looong will take longer than the Universe's
	;; existence to process.
	(goto-char (point-min))
	(while (not (eobp))
	  (unless (< (move-to-column nnmail-split-header-length-limit)
		     nnmail-split-header-length-limit)
	    (delete-region (point) (point-at-eol)))
	  (forward-line 1))
	;; Allow washing.
	(goto-char (point-min))
	(run-hooks 'nnmail-split-hook)
	(when (setq nnmail-split-tracing trace)
	  (setq nnmail-split-trace nil))
	(if (or (and (symbolp nnmail-split-methods)
		     (fboundp nnmail-split-methods))
		(not (consp (car-safe nnmail-split-methods)))
		(and (listp nnmail-split-methods)
		     ;; Not a regular split method, so it has to be a
		     ;; fancy one.
		     (not (let ((top-element (car-safe nnmail-split-methods)))
			    (and (= 2 (length top-element))
				 (stringp (nth 0 top-element))
				 (stringp (nth 1 top-element)))))))
	    (let* ((method-function
		    (if (and (symbolp nnmail-split-methods)
			     (fboundp nnmail-split-methods))
			nnmail-split-methods
		      'nnmail-split-fancy))
		   (split
		    (condition-case error-info
			;; `nnmail-split-methods' is a function, so we
			;; just call this function here and use the
			;; result.
			(or (funcall method-function)
			    (and (not nnmail-inhibit-default-split-group)
				 '("bogus")))
		      (error
		       (nnheader-message
			5 "Error in `nnmail-split-methods'; using `bogus' mail group: %S" error-info)
		       (sit-for 1)
		       '("bogus")))))
	      (setq split (mm-delete-duplicates split))
	      ;; The article may be "cross-posted" to `junk'.  What
	      ;; to do?  Just remove the `junk' spec.  Don't really
	      ;; see anything else to do...
	      (when (and (memq 'junk split)
			 junk-func)
		(funcall junk-func 'junk))
	      (setq split (delq 'junk split))
	      (when split
		(setq group-art
		      (mapcar
		       (lambda (group) (cons group (funcall func group)))
		       split))))
	  ;; Go through the split methods to find a match.
	  (while (and methods
		      (or nnmail-crosspost
			  (not group-art)))
	    (goto-char (point-max))
	    (setq method (pop methods)
		  grp (car method))
	    (if (or methods
		    (not (equal "" (nth 1 method))))
		(when (and
		       (ignore-errors
			 (if (stringp (nth 1 method))
			     (let ((expand (string-match "\\\\[0-9&]" grp))
				   (pos (re-search-backward (cadr method)
							    nil t)))
			       (and expand
				    (setq grp (nnmail-expand-newtext grp)))
			       pos)
			   ;; Function to say whether this is a match.
			   (funcall (nth 1 method) grp)))
		       ;; Don't enter the article into the same
		       ;; group twice.
		       (not (assoc grp group-art)))
		  (push (cons grp (funcall func grp))
			group-art))
	      ;; This is the final group, which is used as a
	      ;; catch-all.
	      (when (and (not group-art)
			 (or (equal "" (nth 1 method))
			     (not nnmail-inhibit-default-split-group)))
		(setq group-art
		      (list (cons (car method)
				  (funcall func (car method))))))))
	  ;; Fall back on "bogus" if all else fails.
	  (when (and (not group-art)
		     (not nnmail-inhibit-default-split-group))
	    (setq group-art (list (cons "bogus" (funcall func "bogus"))))))
	;; Produce a trace if non-empty.
	(when (and trace nnmail-split-trace)
	  (let ((restore (current-buffer)))
	    (nnheader-set-temp-buffer "*Split Trace*")
	    (gnus-add-buffer)
	    (dolist (trace (nreverse nnmail-split-trace))
	      (prin1 trace (current-buffer))
	      (insert "\n"))
	    (goto-char (point-min))
	    (gnus-configure-windows 'split-trace)
	    (set-buffer restore)))
	(widen)
	;; See whether the split methods returned `junk'.
	(if (equal group-art '(junk))
	    nil
	  ;; The article may be "cross-posted" to `junk'.  What
	  ;; to do?  Just remove the `junk' spec.  Don't really
	  ;; see anything else to do...
	  (let (elem)
	    (while (setq elem (car (memq 'junk group-art)))
	      (setq group-art (delq elem group-art)))
	    (nreverse group-art)))))))

(defun nnmail-insert-lines ()
  "Insert how many lines there are in the body of the mail.
Return the number of characters in the body."
  (let (lines chars)
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward "\n\n" nil t)
	(goto-char (point-max))
	(insert "\n"))
      (setq chars (- (point-max) (point)))
      (setq lines (count-lines (point) (point-max)))
      (forward-char -1)
      (save-excursion
	(when (re-search-backward "^Lines: " nil t)
	  (delete-region (point) (progn (forward-line 1) (point)))))
      (beginning-of-line)
      (insert (format "Lines: %d\n" (max lines 0)))
      chars)))

(defun nnmail-insert-xref (group-alist)
  "Insert an Xref line based on the (group . article) alist."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (goto-char (point-max))
      (insert "\n"))
    (forward-char -1)
    (when (re-search-backward "^Xref: " nil t)
      (delete-region (match-beginning 0)
		     (progn (forward-line 1) (point))))
    (insert (format "Xref: %s" (system-name)))
    (while group-alist
      (insert (if (mm-multibyte-p)
		  (mm-string-as-multibyte
		   (format " %s:%d" (caar group-alist) (cdar group-alist)))
		(mm-string-as-unibyte
		 (format " %s:%d" (caar group-alist) (cdar group-alist)))))
      (setq group-alist (cdr group-alist)))
    (insert "\n")))

;;; Message washing functions

(defun nnmail-remove-leading-whitespace ()
  "Remove excessive whitespace from all headers."
  (goto-char (point-min))
  (while (re-search-forward "^\\([^ :]+: \\) +" nil t)
    (replace-match "\\1" t)))

(defun nnmail-remove-list-identifiers ()
  "Remove list identifiers from Subject headers."
  (let ((regexp
	 (if (consp nnmail-list-identifiers)
	     (mapconcat 'identity nnmail-list-identifiers " *\\|")
	   nnmail-list-identifiers)))
    (when regexp
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^Subject: +\\(R[Ee]: +\\)*\\(" regexp " *\\)")
	      nil t)
	(delete-region (match-beginning 2) (match-end 0))
	(beginning-of-line))
      (when (re-search-forward "^Subject: +\\(\\(R[Ee]: +\\)+\\)R[Ee]: +"
			       nil t)
	(delete-region (match-beginning 1) (match-end 1))
	(beginning-of-line)))))

(defun nnmail-remove-tabs ()
  "Translate TAB characters into SPACE characters."
  (subst-char-in-region (point-min) (point-max) ?\t ?  t))

(defcustom nnmail-broken-references-mailers
  "^X-Mailer:.*\\(Eudora\\|Pegasus\\)"
  "Header line matching mailer producing bogus References lines.
See `nnmail-ignore-broken-references'."
  :group 'nnmail-prepare
  :version "23.1" ;; No Gnus
  :type 'regexp)

(defun nnmail-ignore-broken-references ()
  "Ignore the References line and use In-Reply-To

Eudora has a broken References line, but an OK In-Reply-To."
  (goto-char (point-min))
  (when (re-search-forward nnmail-broken-references-mailers nil t)
    (goto-char (point-min))
    (when (re-search-forward "^References:" nil t)
      (beginning-of-line)
      (insert "X-Gnus-Broken-Eudora-"))
    (goto-char (point-min))
    (when (re-search-forward "^\\(In-Reply-To:[^\n]+\\)\n[ \t]+" nil t)
      (replace-match "\\1" t))))

(defalias 'nnmail-fix-eudora-headers 'nnmail-ignore-broken-references)
(make-obsolete 'nnmail-fix-eudora-headers 'nnmail-ignore-broken-references "Emacs 23.1")

(custom-add-option 'nnmail-prepare-incoming-header-hook
		   'nnmail-ignore-broken-references)

;;; Utility functions

(declare-function gnus-activate-group "gnus-start"
                  (group &optional scan dont-check method dont-sub-check))

(defun nnmail-do-request-post (accept-func &optional server)
  "Utility function to directly post a message to an nnmail-derived group.
Calls ACCEPT-FUNC (which should be `nnchoke-request-accept-article')
to actually put the message in the right group."
  (let ((success t))
    (dolist (mbx (message-unquote-tokens
		  (message-tokenize-header
		   (message-fetch-field "Newsgroups") ", ")) success)
      (let ((to-newsgroup (gnus-group-prefixed-name mbx gnus-command-method)))
	(or (gnus-active to-newsgroup)
	    (gnus-activate-group to-newsgroup)
	    (if (gnus-y-or-n-p (format "No such group: %s.  Create it? "
				       to-newsgroup))
		(or (and (gnus-request-create-group
			  to-newsgroup gnus-command-method)
			 (gnus-activate-group to-newsgroup nil nil
					      gnus-command-method))
		    (error "Couldn't create group %s" to-newsgroup)))
	    (error "No such group: %s" to-newsgroup))
	(unless (funcall accept-func mbx (nth 1 gnus-command-method))
	  (setq success nil))))))

(defun nnmail-split-fancy ()
  "Fancy splitting method.
See the documentation for the variable `nnmail-split-fancy' for details."
  (with-syntax-table nnmail-split-fancy-syntax-table
    (nnmail-split-it nnmail-split-fancy)))

(defvar nnmail-split-cache nil)
;; Alist of split expressions their equivalent regexps.

(defun nnmail-split-it (split)
  ;; Return a list of groups matching SPLIT.
  (let (cached-pair)
    (cond
     ;; nil split
     ((null split)
      nil)

     ;; A group name.  Do the \& and \N subs into the string.
     ((stringp split)
      (when nnmail-split-tracing
	(push split nnmail-split-trace))
      (list (nnmail-expand-newtext split)))

     ;; Junk the message.
     ((eq split 'junk)
      (when nnmail-split-tracing
	(push "junk" nnmail-split-trace))
      (list 'junk))

     ;; Builtin & operation.
     ((eq (car split) '&)
      (apply 'nconc (mapcar 'nnmail-split-it (cdr split))))

     ;; Builtin | operation.
     ((eq (car split) '|)
      (let (done)
	(while (and (not done) (cdr split))
	  (setq split (cdr split)
		done (nnmail-split-it (car split))))
	done))

     ;; Builtin : operation.
     ((eq (car split) ':)
      (when nnmail-split-tracing
	(push split nnmail-split-trace))
      (nnmail-split-it (save-excursion (eval (cdr split)))))

     ;; Builtin ! operation.
     ((eq (car split) '!)
      (funcall (cadr split) (nnmail-split-it (caddr split))))

     ;; Check the cache for the regexp for this split.
     ((setq cached-pair (assq split nnmail-split-cache))
      (let (split-result
	    (end-point (point-max))
	    (value (nth 1 split)))
	(if (symbolp value)
	    (setq value (cdr (assq value nnmail-split-abbrev-alist))))
	(while (and (goto-char end-point)
		    (re-search-backward (cdr cached-pair) nil t))
	  (when nnmail-split-tracing
	    (push split nnmail-split-trace))
	  (let ((split-rest (cddr split))
		(end (match-end 0))
		;; The searched regexp is \(\(FIELD\).*\)\(VALUE\).
		;; So, start-of-value is the point just before the
		;; beginning of the value, whereas after-header-name
		;; is the point just after the field name.
		(start-of-value (match-end 1))
		(after-header-name (match-end 2)))
	    ;; Start the next search just before the beginning of the
	    ;; VALUE match.
	    (setq end-point (1- start-of-value))
	    ;; Handle - RESTRICTs
	    (while (eq (car split-rest) '-)
	      ;; RESTRICT must start after-header-name and
	      ;; end after start-of-value, so that, for
	      ;; (any "foo" - "x-foo" "foo.list")
	      ;; we do not exclude foo.list just because
	      ;; the header is: ``To: x-foo, foo''
	      (goto-char end)
	      (if (and (re-search-backward (cadr split-rest)
					   after-header-name t)
		       (> (match-end 0) start-of-value))
		  (setq split-rest nil)
		(setq split-rest (cddr split-rest))))
	    (when split-rest
	      (goto-char end)
	      (let ((value (nth 1 split)))
		(if (symbolp value)
		    (setq value (cdr (assq value nnmail-split-abbrev-alist))))
		;; Someone might want to do a \N sub on this match, so get the
		;; correct match positions.
		(re-search-backward value start-of-value))
	      (dolist (sp (nnmail-split-it (car split-rest)))
		(unless (member sp split-result)
		  (push sp split-result))))))
	split-result))

     ;; Not in cache, compute a regexp for the field/value pair.
     (t
      (let ((field (nth 0 split))
	    (value (nth 1 split))
	    (split-rest (cddr split))
	    partial-front
	    partial-rear
	    regexp)
	(if (symbolp value)
	    (setq value (cdr (assq value nnmail-split-abbrev-alist))))
	(if (and (>= (length value) 2)
		 (string= ".*" (substring value 0 2)))
	    (setq value (substring value 2)
		  partial-front ""))
	;; Same trick for the rear of the regexp
	(if (and (>= (length value) 2)
		 (string= ".*" (substring value -2)))
	    (setq value (substring value 0 -2)
		  partial-rear ""))
	;; Invert the match-partial-words behavior if the optional
	;; last element is specified.
	(while (eq (car split-rest) '-)
	  (setq split-rest (cddr split-rest)))
	(when (if (cadr split-rest)
		  (not nnmail-split-fancy-match-partial-words)
		nnmail-split-fancy-match-partial-words)
	  (setq partial-front ""
		partial-rear ""))
	(setq regexp (concat "^\\(\\("
			     (if (symbolp field)
				 (cdr (assq field nnmail-split-abbrev-alist))
			       field)
			     "\\):.*\\)"
			     (or partial-front "\\<")
			     "\\("
			     value
			     "\\)"
			     (or partial-rear "\\>")))
	(push (cons split regexp) nnmail-split-cache)
	;; Now that it's in the cache, just call nnmail-split-it again
	;; on the same split, which will find it immediately in the cache.
	(nnmail-split-it split))))))

(defun nnmail-expand-newtext (newtext)
  (let ((len (length newtext))
	(pos 0)
	c expanded beg N did-expand)
    (while (< pos len)
      (setq beg pos)
      (while (and (< pos len)
		  (not (= (aref newtext pos) ?\\)))
	(setq pos (1+ pos)))
      (unless (= beg pos)
	(push (substring newtext beg pos) expanded))
      (when (< pos len)
	;; We hit a \; expand it.
	(setq did-expand t
	      pos (1+ pos)
	      c (aref newtext pos))
	(if (not (or (= c ?\&)
		     (and (>= c ?1)
			  (<= c ?9))))
	    ;; \ followed by some character we don't expand.
	    (push (char-to-string c) expanded)
	  ;; \& or \N
	  (if (= c ?\&)
	      (setq N 0)
	    (setq N (- c ?0)))
	  (when (match-beginning N)
	    (push (if nnmail-split-lowercase-expanded
		      (downcase (buffer-substring (match-beginning N)
						  (match-end N)))
		    (buffer-substring (match-beginning N) (match-end N)))
		  expanded))))
      (setq pos (1+ pos)))
    (if did-expand
	(apply 'concat (nreverse expanded))
      newtext)))

;; Activate a backend only if it isn't already activated.
;; If FORCE, re-read the active file even if the backend is
;; already activated.
(defun nnmail-activate (backend &optional force)
  (nnheader-init-server-buffer)
  (let (file timestamp file-time)
    (if (or (not (symbol-value (intern (format "%s-group-alist" backend))))
	    force
	    (and (setq file (ignore-errors
			      (symbol-value (intern (format "%s-active-file"
							    backend)))))
		 (setq file-time (nth 5 (file-attributes file)))
		 (or (not
		      (setq timestamp
			    (condition-case ()
				(symbol-value (intern
					       (format "%s-active-timestamp"
						       backend)))
			      (error 'none))))
		     (not (consp timestamp))
		     (equal timestamp '(0 0))
		     (> (nth 0 file-time) (nth 0 timestamp))
		     (and (= (nth 0 file-time) (nth 0 timestamp))
			  (> (nth 1 file-time) (nth 1 timestamp))))))
	(save-excursion
	  (or (eq timestamp 'none)
	      (set (intern (format "%s-active-timestamp" backend))
		   file-time))
	  (funcall (intern (format "%s-request-list" backend)))))
    t))

(defun nnmail-message-id ()
  (concat "<" (message-unique-id) "@totally-fudged-out-message-id>"))

;;;
;;; nnmail duplicate handling
;;;

(defvar nnmail-cache-buffer nil)

(defun nnmail-cache-open ()
  (if (or (not nnmail-treat-duplicates)
	  (and nnmail-cache-buffer
	       (buffer-name nnmail-cache-buffer)))
      ()				; The buffer is open.
    (with-current-buffer
       (setq nnmail-cache-buffer
	     (get-buffer-create " *nnmail message-id cache*"))
      (gnus-add-buffer)
      (when (file-exists-p nnmail-message-id-cache-file)
	(nnheader-insert-file-contents nnmail-message-id-cache-file))
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun nnmail-cache-close ()
  (when (and nnmail-cache-buffer
	     nnmail-treat-duplicates
	     (buffer-name nnmail-cache-buffer)
	     (buffer-modified-p nnmail-cache-buffer))
    (with-current-buffer nnmail-cache-buffer
      ;; Weed out the excess number of Message-IDs.
      (goto-char (point-max))
      (when (search-backward "\n" nil t nnmail-message-id-cache-length)
	(progn
	  (beginning-of-line)
	  (delete-region (point-min) (point))))
      ;; Save the buffer.
      (or (file-exists-p (file-name-directory nnmail-message-id-cache-file))
	  (make-directory (file-name-directory nnmail-message-id-cache-file)
			  t))
      (nnmail-write-region (point-min) (point-max)
			   nnmail-message-id-cache-file nil 'silent)
      (set-buffer-modified-p nil)
      (setq nnmail-cache-buffer nil)
      (gnus-kill-buffer (current-buffer)))))

(defun nnmail-cache-insert (id grp &optional subject sender)
  (when (stringp id)
    ;; this will handle cases like `B r' where the group is nil
    (let ((grp (or grp gnus-newsgroup-name "UNKNOWN")))
      (run-hook-with-args 'nnmail-spool-hook
			  id grp subject sender))
    (when nnmail-treat-duplicates
      ;; Store some information about the group this message is written
      ;; to.  This is passed in as the grp argument -- all locations this
      ;; has been called from have been checked and the group is available.
      ;; The only ambiguous case is nnmail-check-duplication which will only
      ;; pass the first (of possibly >1) group which matches. -Josh
      (unless (gnus-buffer-live-p nnmail-cache-buffer)
	(nnmail-cache-open))
      (with-current-buffer nnmail-cache-buffer
	(goto-char (point-max))
	(if (and grp (not (string= "" grp))
		 (gnus-methods-equal-p gnus-command-method
				       (nnmail-cache-primary-mail-backend)))
	    (let ((regexp (if (consp nnmail-cache-ignore-groups)
			      (mapconcat 'identity nnmail-cache-ignore-groups
					 "\\|")
			    nnmail-cache-ignore-groups)))
	      (unless (and regexp (string-match regexp grp))
		(insert id "\t" grp "\n")))
	  (insert id "\n"))))))

(defun nnmail-cache-primary-mail-backend ()
  (let ((be-list (cons gnus-select-method gnus-secondary-select-methods))
	(be nil)
	(res nil)
        (get-new-mail nil))
    (while (and (null res) be-list)
      (setq be (car be-list))
      (setq be-list (cdr be-list))
      (when (and (gnus-method-option-p be 'respool)
                 (setq get-new-mail
                       (intern (format "%s-get-new-mail" (car be))))
                 (boundp get-new-mail)
		 (symbol-value get-new-mail))
	(setq res be)))
    res))

;; Fetch the group name corresponding to the message id stored in the
;; cache.
(defun nnmail-cache-fetch-group (id)
  (when (and nnmail-treat-duplicates nnmail-cache-buffer)
    (with-current-buffer nnmail-cache-buffer
      (goto-char (point-max))
      (when (search-backward id nil t)
	(beginning-of-line)
	(skip-chars-forward "^\n\r\t")
	(unless (looking-at "[\r\n]")
	  (forward-char 1)
	  (buffer-substring (point) (point-at-eol)))))))

;; Function for nnmail-split-fancy: look up all references in the
;; cache and if a match is found, return that group.
(defun nnmail-split-fancy-with-parent ()
  "Split this message into the same group as its parent.
This function can be used as an entry in `nnmail-split-fancy', for
example like this: (: nnmail-split-fancy-with-parent)
For a message to be split, it looks for the parent message in the
References or In-Reply-To header and then looks in the message id
cache file (given by the variable `nnmail-message-id-cache-file') to
see which group that message was put in.  This group is returned.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (let* ((refstr (or (message-fetch-field "references")
		     (message-fetch-field "in-reply-to")))
	 (references nil)
	 (res nil)
	 (regexp (if (consp nnmail-split-fancy-with-parent-ignore-groups)
		     (mapconcat
		      (lambda (x) (format "\\(%s\\)" x))
		      nnmail-split-fancy-with-parent-ignore-groups
		      "\\|")
		   nnmail-split-fancy-with-parent-ignore-groups)))
    (when refstr
      (setq references (nreverse (gnus-split-references refstr)))
      (unless (gnus-buffer-live-p nnmail-cache-buffer)
	(nnmail-cache-open))
      (dolist (x references)
	(setq res (or (nnmail-cache-fetch-group x) res))
	(when (or (member res '("delayed" "drafts" "queue"))
		  (and regexp res (string-match regexp res)))
	  (setq res nil)))
      res)))

(defun nnmail-cache-id-exists-p (id)
  (when nnmail-treat-duplicates
    (with-current-buffer nnmail-cache-buffer
      (goto-char (point-max))
      (search-backward id nil t))))

(defun nnmail-fetch-field (header)
  (save-excursion
    (save-restriction
      (message-narrow-to-head)
      (message-fetch-field header))))

(defun nnmail-check-duplication (message-id func artnum-func
					    &optional junk-func)
  (run-hooks 'nnmail-prepare-incoming-message-hook)
  ;; If this is a duplicate message, then we do not save it.
  (let* ((duplication (nnmail-cache-id-exists-p message-id))
	 (case-fold-search t)
	 (action (when duplication
		   (cond
		    ((memq nnmail-treat-duplicates '(warn delete))
		     nnmail-treat-duplicates)
		    ((functionp nnmail-treat-duplicates)
		     (funcall nnmail-treat-duplicates message-id))
		    (t
		     nnmail-treat-duplicates))))
	 group-art)
    ;; We insert a line that says what the mail source is.
    (let ((case-fold-search t))
      (goto-char (point-min))
      (re-search-forward "^message-id[ \t]*:" nil t)
      (beginning-of-line)
      (insert (format "X-Gnus-Mail-Source: %s\n" mail-source-string)))

    ;; Let the backend save the article (or not).
    (cond
     ((not duplication)
      (funcall func (setq group-art
			  (nreverse (nnmail-article-group
				     artnum-func nil junk-func))))
      (nnmail-cache-insert message-id (caar group-art)))
     ((eq action 'delete)
      (setq group-art nil))
     ((eq action 'warn)
      ;; We insert a warning.
      (let ((case-fold-search t))
	(goto-char (point-min))
	(re-search-forward "^message-id[ \t]*:" nil t)
	(beginning-of-line)
	(insert
	 "Gnus-Warning: This is a duplicate of message " message-id "\n")
	(funcall func (setq group-art
			    (nreverse (nnmail-article-group artnum-func))))))
     (t
      (funcall func (setq group-art
			  (nreverse (nnmail-article-group artnum-func))))))
    ;; Add the group-art list to the history list.
    (if group-art
	(push group-art nnmail-split-history)
      (delete-region (point-min) (point-max)))))

;;; Get new mail.

(defvar nnmail-fetched-sources nil)

(defun nnmail-get-value (&rest args)
  (let ((sym (intern (apply 'format args))))
    (when (boundp sym)
      (symbol-value sym))))

(defun nnmail-get-new-mail (method exit-func temp
			    &optional group spool-func)
  "Read new incoming mail."
  (nnmail-get-new-mail-1 method exit-func temp group nil spool-func))

(defun nnmail-get-new-mail-1 (method exit-func temp
			      group in-group spool-func)
  (let* ((sources mail-sources)
	 fetching-sources
	 (i 0)
	 (new 0)
	 (total 0)
	 source)
    (when (and (nnmail-get-value "%s-get-new-mail" method)
	       sources)
      (while (setq source (pop sources))
	;; Use group's parameter
	(when (eq (car source) 'group)
	  (let ((mail-sources
		 (list
		  (gnus-group-find-parameter
		   (concat (symbol-name method) ":" group)
		   'mail-source t))))
	    (nnmail-get-new-mail-1 method exit-func temp
				   group group spool-func))
	  (setq source nil))
	;; Hack to only fetch the contents of a single group's spool file.
	(when (and (eq (car source) 'directory)
		   (null nnmail-scan-directory-mail-source-once)
		   group)
	  (mail-source-bind (directory source)
	    (setq source (append source
				 (list
				  :predicate
				  (gnus-byte-compile
				   `(lambda (file)
				      (string-equal
				       ,(concat group suffix)
				       (file-name-nondirectory file)))))))))
	(when nnmail-fetched-sources
	  (if (member source nnmail-fetched-sources)
	      (setq source nil)
	    (push source nnmail-fetched-sources)
	    (push source fetching-sources)))))
    (when fetching-sources
      ;; We first activate all the groups.
      (nnmail-activate method)
      ;; Allow the user to hook.
      (run-hooks 'nnmail-pre-get-new-mail-hook)
      ;; Open the message-id cache.
      (nnmail-cache-open)
      ;; The we go through all the existing mail source specification
      ;; and fetch the mail from each.
      (while (setq source (pop fetching-sources))
	(when (setq new
		    (condition-case cond
			(mail-source-fetch
			 source
			 (gnus-byte-compile
			  `(lambda (file orig-file)
			     (nnmail-split-incoming
			      file ',(intern (format "%s-save-mail" method))
			      ',spool-func
			      (or in-group
				  (if (equal file orig-file)
				      nil
				    (nnmail-get-split-group orig-file
							    ',source)))
			      ',(intern (format "%s-active-number" method))))))
		      ((error quit)
		       (message "Mail source %s failed: %s" source cond)
		       0)))
	  (incf total new)
	  (incf i)))
      ;; If we did indeed read any incoming spools, we save all info.
      (if (zerop total)
	  (when mail-source-plugged
	    (nnheader-message 4 "%s: Reading incoming mail (no new mail)...done"
			      method (car source)))
	(nnmail-save-active
	 (nnmail-get-value "%s-group-alist" method)
	 (nnmail-get-value "%s-active-file" method))
	(when exit-func
	  (funcall exit-func))
	(run-hooks 'nnmail-read-incoming-hook)
	(nnheader-message 4 "%s: Reading incoming mail (%d new)...done" method
			  total))
      ;; Close the message-id cache.
      (nnmail-cache-close)
      ;; Allow the user to hook.
      (run-hooks 'nnmail-post-get-new-mail-hook))))

(defun nnmail-expired-article-p (group time force &optional inhibit)
  "Say whether an article that is TIME old in GROUP should be expired.
If TIME is nil, then return the cutoff time for oldness instead."
  (if force
      (if (null time)
	  (current-time)
	t)
    (let ((days (or (and nnmail-expiry-wait-function
			 (funcall nnmail-expiry-wait-function group))
		    nnmail-expiry-wait)))
      (cond ((or (eq days 'never)
		 (and (not force)
		      inhibit))
	     ;; This isn't an expirable group.
	     nil)
	    ((eq days 'immediate)
	     ;; We expire all articles on sight.
	     (if (null time)
		 (current-time)
	       t))
	    ((equal time '(0 0))
	    ;; This is an ange-ftp group, and we don't have any dates.
	     nil)
	    ((numberp days)
	     (setq days (days-to-time days))
	     ;; Compare the time with the current time.
	     (if (null time)
		 (time-subtract (current-time) days)
	       (ignore-errors (time-less-p days (time-since time)))))))))

(declare-function gnus-group-mark-article-read "gnus-group" (group article))

(defun nnmail-expiry-target-group (target group)
  ;; Do not invoke this from nntp-server-buffer!  At least nnfolder clears
  ;; that buffer if the nnfolder group isn't selected.
  (let (nnmail-cache-accepted-message-ids)
    ;; Don't enter Message-IDs into cache.
    ;; Let users hack it in TARGET function.
    (when (functionp target)
      (setq target (funcall target group)))
    (unless (eq target 'delete)
      (when (or (gnus-request-group target)
		(gnus-request-create-group target))
	(let ((group-art (gnus-request-accept-article target nil nil t)))
	  (when (and (consp group-art)
		     (cdr group-art))
	    (gnus-group-mark-article-read target (cdr group-art))))))))

(defun nnmail-fancy-expiry-target (group)
  "Returns a target expiry group determined by `nnmail-fancy-expiry-targets'."
  (let* (header
	 (case-fold-search nil)
	 (from (or (message-fetch-field "from") ""))
	 (to (or (message-fetch-field "to") ""))
	 (date (message-fetch-field "date"))
	 (target 'delete))
    (setq date (if date
		   (condition-case err
		       (date-to-time date)
		     (error
		      (message "%s" (error-message-string err))
		      (current-time)))
		 (current-time)))
    (dolist (regexp-target-pair (reverse nnmail-fancy-expiry-targets) target)
      (setq header (car regexp-target-pair))
      (cond
       ;; If the header is to-from then match against the
       ;; To or From header
       ((and (equal header 'to-from)
	     (or (string-match (cadr regexp-target-pair) from)
		 (and (string-match (cadr regexp-target-pair) to)
		      (let ((rmail-dont-reply-to-names
			     (message-dont-reply-to-names)))
			(equal (rmail-dont-reply-to from) "")))))
	(setq target (format-time-string (caddr regexp-target-pair) date)))
       ((and (not (equal header 'to-from))
	     (string-match (cadr regexp-target-pair)
			   (or
			    (message-fetch-field header)
			    "")))
	(setq target
	      (format-time-string (caddr regexp-target-pair) date)))))))

(defun nnmail-check-syntax ()
  "Check (and modify) the syntax of the message in the current buffer."
  (save-restriction
    (message-narrow-to-head)
    (let ((case-fold-search t))
      (unless (re-search-forward "^Message-ID[ \t]*:" nil t)
	(insert "Message-ID: " (nnmail-message-id) "\n")))))

(defun nnmail-write-region (start end filename &optional append visit lockname)
  "Do a `write-region', and then set the file modes."
  (let ((coding-system-for-write nnmail-file-coding-system)
	(file-name-coding-system nnmail-pathname-coding-system))
    (write-region start end filename append visit lockname)
    (set-file-modes filename nnmail-default-file-modes)))

;;;
;;; Status functions
;;;

(defun nnmail-replace-status (name value)
  "Make status NAME and VALUE part of the current status line."
  (save-restriction
    (message-narrow-to-head)
    (let ((status (nnmail-decode-status)))
      (setq status (delq (member name status) status))
      (when value
	(push (cons name value) status))
      (message-remove-header "status")
      (goto-char (point-max))
      (insert "Status: " (nnmail-encode-status status) "\n"))))

(defun nnmail-decode-status ()
  "Return a status-value alist from STATUS."
  (goto-char (point-min))
  (when (re-search-forward "^Status: " nil t)
    (let (name value status)
      (save-restriction
	;; Narrow to the status.
	(narrow-to-region
	 (point)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (1- (point))
	   (point-max)))
	;; Go through all elements and add them to the list.
	(goto-char (point-min))
	(while (re-search-forward "[^ \t=]+" nil t)
	  (setq name (match-string 0))
	  (if (not (eq (char-after) ?=))
	      ;; Implied "yes".
	      (setq value "yes")
	    (forward-char 1)
	    (if (not (eq (char-after) ?\"))
		(if (not (looking-at "[^ \t]"))
		    ;; Implied "no".
		    (setq value "no")
		  ;; Unquoted value.
		  (setq value (match-string 0))
		  (goto-char (match-end 0)))
	      ;; Quoted value.
	      (setq value (read (current-buffer)))))
	  (push (cons name value) status)))
      status)))

(defun nnmail-encode-status (status)
  "Return a status string from STATUS."
  (mapconcat
   (lambda (elem)
     (concat
      (car elem) "="
      (if (string-match "[ \t]" (cdr elem))
	  (prin1-to-string (cdr elem))
	(cdr elem))))
   status " "))

(defun nnmail-split-history ()
  "Generate an overview of where the last mail split put articles."
  (interactive)
  (unless nnmail-split-history
    (error "No current split history"))
  (with-output-to-temp-buffer "*nnmail split history*"
    (with-current-buffer standard-output
      (fundamental-mode))		; for Emacs 20.4+
      (dolist (elem nnmail-split-history)
	(princ (mapconcat (lambda (ga)
			    (concat (car ga) ":" (int-to-string (cdr ga))))
			  elem
			  ", "))
	(princ "\n"))))

(defun nnmail-purge-split-history (group)
  "Remove all instances of GROUP from `nnmail-split-history'."
  (let ((history nnmail-split-history))
    (while history
      (setcar history (gnus-remove-if (lambda (e) (string= (car e) group))
				      (car history)))
      (pop history))
    (setq nnmail-split-history (delq nil nnmail-split-history))))

(defun nnmail-new-mail-p (group)
  "Say whether GROUP has new mail."
  (let ((his nnmail-split-history)
	found)
    (while his
      (when (assoc group (pop his))
	(setq found t
	      his nil)))
    found))

(defun nnmail-within-headers-p ()
  "Check to see if point is within the headers of a unix mail message.
Doesn't change point."
  (let ((pos (point)))
    (save-excursion
      (and (nnmail-search-unix-mail-delim-backward)
	   (not (search-forward "\n\n" pos t))))))

(run-hooks 'nnmail-load-hook)

(provide 'nnmail)

;;; nnmail.el ends here
