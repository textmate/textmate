;;; gnus-start.el --- startup functions for Gnus

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

(require 'gnus)
(require 'gnus-win)
(require 'gnus-int)
(require 'gnus-spec)
(require 'gnus-range)
(require 'gnus-util)
(autoload 'message-make-date "message")
(autoload 'gnus-agent-read-servers-validate "gnus-agent")
(autoload 'gnus-agent-save-local "gnus-agent")
(autoload 'gnus-agent-possibly-alter-active "gnus-agent")

(eval-when-compile
  (require 'cl))

(defvar gnus-agent-covered-methods)
(defvar gnus-agent-file-loading-local)
(defvar gnus-agent-file-loading-cache)

(defcustom gnus-startup-file (nnheader-concat gnus-home-directory ".newsrc")
  "Your `.newsrc' file.
`.newsrc-SERVER' will be used instead if that exists."
  :group 'gnus-start
  :type 'file)

(defcustom gnus-backup-startup-file 'never
  "Control use of version numbers for backups of `gnus-startup-file'.
This variable takes the same values as the `version-control'
variable."
  :version "22.1"
  :group 'gnus-start
  :type '(choice (const :tag "Never" never)
		 (const :tag "If existing" nil)
		 (other :tag "Always" t)))

(defcustom gnus-save-startup-file-via-temp-buffer t
  "Whether to write the startup file contents to a buffer then save
the buffer or write directly to the file.  The buffer is faster
because all of the contents are written at once.  The direct write
uses considerably less memory."
  :version "22.1"
  :group 'gnus-start
  :type '(choice (const :tag "Write via buffer" t)
                 (const :tag "Write directly to file" nil)))

(defcustom gnus-init-file (nnheader-concat gnus-home-directory ".gnus")
  "Your Gnus Emacs-Lisp startup file name.
If a file with the `.el' or `.elc' suffixes exists, it will be read instead."
  :group 'gnus-start
  :type 'file)

(defcustom gnus-site-init-file
  (condition-case nil
      (concat (file-name-directory
	       (directory-file-name installation-directory))
	      "site-lisp/gnus-init")
    (error nil))
  "The site-wide Gnus Emacs-Lisp startup file name, or nil if none.
If a file with the `.el' or `.elc' suffixes exists, it will be read instead."
  :group 'gnus-start
  :type '(choice file (const nil)))

(defcustom gnus-use-dribble-file t
  "*Non-nil means that Gnus will use a dribble file to store user updates.
If Emacs should crash without saving the .newsrc files, complete
information can be restored from the dribble file."
  :group 'gnus-dribble-file
  :type 'boolean)

(defcustom gnus-dribble-directory nil
  "*The directory where dribble files will be saved.
If this variable is nil, the directory where the .newsrc files are
saved will be used."
  :group 'gnus-dribble-file
  :type '(choice directory (const nil)))

(defcustom gnus-check-new-newsgroups 'ask-server
  "*Non-nil means that Gnus will run `gnus-find-new-newsgroups' at startup.
This normally finds new newsgroups by comparing the active groups the
servers have already reported with those Gnus already knows, either alive
or killed.

When any of the following are true, `gnus-find-new-newsgroups' will instead
ask the servers (primary, secondary, and archive servers) to list new
groups since the last time it checked:
  1. This variable is `ask-server'.
  2. This variable is a list of select methods (see below).
  3. `gnus-read-active-file' is nil or `some'.
  4. A prefix argument is given to `gnus-find-new-newsgroups' interactively.

Thus, if this variable is `ask-server' or a list of select methods or
`gnus-read-active-file' is nil or `some', then the killed list is no
longer necessary, so you could safely set `gnus-save-killed-list' to nil.

This variable can be a list of select methods which Gnus will query with
the `ask-server' method in addition to the primary, secondary, and archive
servers.

Eg.
  (setq gnus-check-new-newsgroups
	'((nntp \"some.server\") (nntp \"other.server\")))

If this variable is nil, then you have to tell Gnus explicitly to
check for new newsgroups with \\<gnus-group-mode-map>\\[gnus-find-new-newsgroups]."
  :group 'gnus-start
  :type '(choice (const :tag "no" nil)
		 (const :tag "by brute force" t)
		 (const :tag "ask servers" ask-server)
		 (repeat :menu-tag "ask additional servers"
			 :tag "ask additional servers"
			 :value ((nntp ""))
			 (sexp :format "%v"))))

(defcustom gnus-check-bogus-newsgroups nil
  "*Non-nil means that Gnus will check and remove bogus newsgroup at startup.
If this variable is nil, then you have to tell Gnus explicitly to
check for bogus newsgroups with \\<gnus-group-mode-map>\\[gnus-group-check-bogus-groups]."
  :group 'gnus-start-server
  :type 'boolean)

(defcustom gnus-read-active-file 'some
  "*Non-nil means that Gnus will read the entire active file at startup.
If this variable is nil, Gnus will only know about the groups in your
`.newsrc' file.

If this variable is `some', Gnus will try to only read the relevant
parts of the active file from the server.  Not all servers support
this, and it might be quite slow with other servers, but this should
generally be faster than both the t and nil value.

If you set this variable to nil or `some', you probably still want to
be told about new newsgroups that arrive.  To do that, set
`gnus-check-new-newsgroups' to `ask-server'.  This may not work
properly with all servers."
  :group 'gnus-start-server
  :type '(choice (const nil)
		 (const some)
		 (const t)))

(defconst gnus-level-subscribed 5
  "Groups with levels less than or equal to this variable are subscribed.")

(defconst gnus-level-unsubscribed 7
  "Groups with levels less than or equal to this variable are unsubscribed.

Groups with levels less than `gnus-level-subscribed', which
should be less than this variable, are subscribed.  Groups with
levels from `gnus-level-subscribed' (exclusive) upto this
variable (inclusive) are unsubscribed.  See also
`gnus-level-zombie', `gnus-level-killed' and the Info node `(gnus)Group
Levels' for details.")

(defconst gnus-level-zombie 8
  "Groups with this level are zombie groups.")

(defconst gnus-level-killed 9
  "Groups with this level are killed.")

(defcustom gnus-level-default-subscribed 3
  "*New subscribed groups will be subscribed at this level."
  :group 'gnus-group-levels
  :type 'integer)

(defcustom gnus-level-default-unsubscribed 6
  "*New unsubscribed groups will be unsubscribed at this level."
  :group 'gnus-group-levels
  :type 'integer)

(defcustom gnus-activate-level (1+ gnus-level-subscribed)
  "*Groups higher than this level won't be activated on startup.
Setting this variable to something low might save lots of time when
you have many groups that you aren't interested in."
  :group 'gnus-group-levels
  :type 'integer)

(defcustom gnus-activate-foreign-newsgroups 4
  "*If nil, Gnus will not check foreign newsgroups at startup.
If it is non-nil, it should be a number between one and nine.  Foreign
newsgroups that have a level lower or equal to this number will be
activated on startup.  For instance, if you want to active all
subscribed newsgroups, but not the rest, you'd set this variable to
`gnus-level-subscribed'.

If you subscribe to lots of newsgroups from different servers, startup
might take a while.  By setting this variable to nil, you'll save time,
but you won't be told how many unread articles there are in the
groups."
  :group 'gnus-group-levels
  :type '(choice integer
		 (const :tag "none" nil)))

(defcustom gnus-read-newsrc-file t
  "*Non-nil means that Gnus will read the `.newsrc' file.
Gnus always reads its own startup file, which is called
\".newsrc.eld\".  The file called \".newsrc\" is in a format that can
be readily understood by other newsreaders.  If you don't plan on
using other newsreaders, set this variable to nil to save some time on
entry."
  :version "21.1"
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-save-newsrc-file t
  "*Non-nil means that Gnus will save the `.newsrc' file.
Gnus always saves its own startup file, which is called
\".newsrc.eld\".  The file called \".newsrc\" is in a format that can
be readily understood by other newsreaders.  If you don't plan on
using other newsreaders, set this variable to nil to save some time on
exit."
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-save-killed-list t
  "*If non-nil, save the list of killed groups to the startup file.
If you set this variable to nil, you'll save both time (when starting
and quitting) and space (both memory and disk), but it will also mean
that Gnus has no record of which groups are new and which are old, so
the automatic new newsgroups subscription methods become meaningless.

You should always set `gnus-check-new-newsgroups' to `ask-server' or
nil if you set this variable to nil.

This variable can also be a regexp.  In that case, all groups that do
not match this regexp will be removed before saving the list."
  :group 'gnus-newsrc
  :type '(radio (sexp :format "Non-nil\n"
		      :match (lambda (widget value)
			       (and value (not (stringp value))))
		      :value t)
		(const nil)
		regexp))

(defcustom gnus-ignored-newsgroups
  (mapconcat 'identity
	     '("^to\\."			; not "real" groups
	       "^[0-9. \t]+\\( \\|$\\)"	; all digits in name
	       "^[\"][\"#'()]"	; bogus characters
	       )
	     "\\|")
  "*A regexp to match uninteresting newsgroups in the active file.
Any lines in the active file matching this regular expression are
removed from the newsgroup list before anything else is done to it,
thus making them effectively non-existent."
  :group 'gnus-group-new
  :type 'regexp)

(defcustom gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
  "*Function(s) called with a group name when new group is detected.
A few pre-made functions are supplied: `gnus-subscribe-randomly'
inserts new groups at the beginning of the list of groups;
`gnus-subscribe-alphabetically' inserts new groups in strict
alphabetic order; `gnus-subscribe-hierarchically' inserts new groups
in hierarchical newsgroup order; `gnus-subscribe-interactively' asks
for your decision; `gnus-subscribe-killed' kills all new groups;
`gnus-subscribe-zombies' will make all new groups into zombies;
`gnus-subscribe-topics' will enter groups into the topics that
claim them."
  :group 'gnus-group-new
  :type '(radio (function-item gnus-subscribe-randomly)
		(function-item gnus-subscribe-alphabetically)
		(function-item gnus-subscribe-hierarchically)
		(function-item gnus-subscribe-interactively)
		(function-item gnus-subscribe-killed)
		(function-item gnus-subscribe-zombies)
		(function-item gnus-subscribe-topics)
		function
		(repeat function)))

(defcustom gnus-subscribe-newsgroup-hooks nil
  "*Hooks run after you subscribe to a new group.
The hooks will be called with new group's name as argument."
  :version "22.1"
  :group 'gnus-group-new
  :type 'hook)

(defcustom gnus-subscribe-options-newsgroup-method
  'gnus-subscribe-alphabetically
  "*Function(s) called to subscribe newsgroups mentioned on \"options -n\" lines.
If, for instance, you want to subscribe to all newsgroups in the
\"no\" and \"alt\" hierarchies, you'd put the following in your
.newsrc file:

options -n no.all alt.all

Gnus will then subscribe all new newsgroups in these hierarchies
with the subscription method in this variable."
  :group 'gnus-group-new
  :type '(radio (function-item gnus-subscribe-randomly)
		(function-item gnus-subscribe-alphabetically)
		(function-item gnus-subscribe-hierarchically)
		(function-item gnus-subscribe-interactively)
		(function-item gnus-subscribe-killed)
		(function-item gnus-subscribe-zombies)
		(function-item gnus-subscribe-topics)
		function
		(repeat function)))

(defcustom gnus-subscribe-hierarchical-interactive nil
  "*If non-nil, Gnus will offer to subscribe hierarchically.
When a new hierarchy appears, Gnus will ask the user:

'alt.binaries': Do you want to subscribe to this hierarchy? ([d]ys):

If the user pressed `d', Gnus will descend the hierarchy, `y' will
subscribe to all newsgroups in the hierarchy and `s' will skip this
hierarchy in its entirety."
  :group 'gnus-group-new
  :type 'boolean)

(defcustom gnus-auto-subscribed-categories '(mail post-mail)
  "*New groups from methods of these categories will be subscribed automatically.
Note that this variable only deals with new groups.  It has no
effect whatsoever on old groups.  The default is to automatically
subscribe all groups from mail-like backends."
  :version "24.1"
  :group 'gnus-group-new
  :type '(repeat symbol))

(defcustom gnus-auto-subscribed-groups
  "^nnml\\|^nnfolder\\|^nnmbox\\|^nnmh\\|^nnbabyl\\|^nnmaildir\\|^nnimap"
  "*All new groups that match this regexp will be subscribed automatically.
Note that this variable only deals with new groups.  It has no effect
whatsoever on old groups.

New groups that match this regexp will not be handled by
`gnus-subscribe-newsgroup-method'.  Instead, they will
be subscribed using `gnus-subscribe-options-newsgroup-method'."
  :group 'gnus-group-new
  :type 'regexp)

(defcustom gnus-options-subscribe nil
  "*All new groups matching this regexp will be subscribed unconditionally.
Note that this variable deals only with new newsgroups.  This variable
does not affect old newsgroups.

New groups that match this regexp will not be handled by
`gnus-subscribe-newsgroup-method'.  Instead, they will
be subscribed using `gnus-subscribe-options-newsgroup-method'."
  :group 'gnus-group-new
  :type '(choice regexp
		 (const :tag "none" nil)))

(defcustom gnus-options-not-subscribe nil
  "*All new groups matching this regexp will be ignored.
Note that this variable deals only with new newsgroups.  This variable
does not affect old (already subscribed) newsgroups."
  :group 'gnus-group-new
  :type '(choice regexp
		 (const :tag "none" nil)))

(defcustom gnus-modtime-botch nil
  "*Non-nil means .newsrc should be deleted prior to save.
Its use is due to the bogus appearance that .newsrc was modified on
disc."
  :group 'gnus-newsrc
  :type 'boolean)

(defcustom gnus-check-bogus-groups-hook nil
  "A hook run after removing bogus groups."
  :group 'gnus-start-server
  :type 'hook)

(defcustom gnus-startup-hook nil
  "A hook called at startup.
This hook is called after Gnus is connected to the NNTP server."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-before-startup-hook nil
  "A hook called before startup.
This hook is called as the first thing when Gnus is started."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-started-hook nil
  "A hook called as the last thing after startup."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-setup-news-hook nil
  "A hook after reading the .newsrc file, but before generating the buffer."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-get-top-new-news-hook nil
  "A hook run just before Gnus checks for new news globally."
  :version "22.1"
  :group 'gnus-group-new
  :type 'hook)

(defcustom gnus-get-new-news-hook nil
  "A hook run just before Gnus checks for new news."
  :group 'gnus-group-new
  :type 'hook)

(defcustom gnus-after-getting-new-news-hook
  '(gnus-display-time-event-handler)
  "*A hook run after Gnus checks for new news when Gnus is already running."
  :version "24.1"
  :group 'gnus-group-new
  :type 'hook)

(defcustom gnus-read-newsrc-el-hook nil
  "A hook called after reading the newsrc.eld? file."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-save-newsrc-hook nil
  "A hook called before saving any of the newsrc files."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-save-quick-newsrc-hook nil
  "A hook called just before saving the quick newsrc file.
Can be used to turn version control on or off."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-save-standard-newsrc-hook nil
  "A hook called just before saving the standard newsrc file.
Can be used to turn version control on or off."
  :group 'gnus-newsrc
  :type 'hook)

(defcustom gnus-group-mode-hook nil
  "Hook for Gnus group mode."
  :group 'gnus-group-various
  :options '(gnus-topic-mode)
  :type 'hook)

(defcustom gnus-always-read-dribble-file nil
  "Unconditionally read the dribble file."
  :group 'gnus-newsrc
  :type 'boolean)

;;; Internal variables

;; Fixme: deal with old emacs-mule when mm-universal-coding-system is
;; utf-8-emacs.
(defvar gnus-ding-file-coding-system mm-universal-coding-system
  "Coding system for ding file.")

(defvar gnus-newsrc-file-version nil)
(defvar gnus-override-subscribe-method nil)
(defvar gnus-dribble-buffer nil)
(defvar gnus-newsrc-options nil
  "Options line in the .newsrc file.")

(defvar gnus-newsrc-options-n nil
  "List of regexps representing groups to be subscribed/ignored unconditionally.")

(defvar gnus-newsrc-last-checked-date nil
  "Date Gnus last asked server for new newsgroups.")

(defvar gnus-current-startup-file nil
  "Startup file for the current host.")

;; Byte-compiler warning.
(defvar gnus-group-line-format)

;; Suggested by Brian Edmonds <edmonds@cs.ubc.ca>.
(defvar gnus-init-inhibit nil)
(defun gnus-read-init-file (&optional inhibit-next)
  ;; Don't load .gnus if the -q option was used.
  (when init-file-user
    (if gnus-init-inhibit
	(setq gnus-init-inhibit nil)
      (setq gnus-init-inhibit inhibit-next)
      (dolist (file (list gnus-site-init-file gnus-init-file))
	(when (and file
		   (locate-library file))
	  (if (or debug-on-error debug-on-quit)
	      (load file nil t)
	    (condition-case var
		(load file nil t)
	      (error
	       (error "Error in %s: %s" file (cadr var))))))))))

;; For subscribing new newsgroup

(defun gnus-subscribe-hierarchical-interactive (groups)
  (let ((groups (sort groups 'string<))
	prefixes prefix start ans group starts real-group)
    (while groups
      (setq prefixes (list "^"))
      (while (and groups prefixes)
	(while (not (string-match (car prefixes)
				  (gnus-group-real-name (car groups))))
	  (setq prefixes (cdr prefixes)))
	(setq prefix (car prefixes))
	(setq start (1- (length prefix)))
	(if (and (string-match "[^\\.]\\." (gnus-group-real-name (car groups))
			       start)
		 (cdr groups)
		 (setq prefix
		       (concat "^" (substring
				    (gnus-group-real-name (car groups))
				    0 (match-end 0))))
		 (string-match prefix (gnus-group-real-name (cadr groups))))
	    (progn
	      (push prefix prefixes)
	      (message "Descend hierarchy %s? ([y]nsq): "
		       (substring prefix 1 (1- (length prefix))))
	      (while (not (memq (setq ans (read-char-exclusive))
				'(?y ?\n ?\r ?n ?s ?q)))
		(ding)
		(message "Descend hierarchy %s? ([y]nsq): "
			 (substring prefix 1 (1- (length prefix)))))
	      (cond ((= ans ?n)
		     (while (and groups
				 (setq group (car groups)
				       real-group (gnus-group-real-name group))
				 (string-match prefix real-group))
		       (push group gnus-killed-list)
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?s)
		     (while (and groups
				 (setq group (car groups)
				       real-group (gnus-group-real-name group))
				 (string-match prefix real-group))
		       (gnus-sethash group group gnus-killed-hashtb)
		       (gnus-subscribe-alphabetically (car groups))
		       (setq groups (cdr groups)))
		     (setq starts (cdr starts)))
		    ((= ans ?q)
		     (while groups
		       (setq group (car groups))
		       (push group gnus-killed-list)
		       (gnus-sethash group group gnus-killed-hashtb)
		       (setq groups (cdr groups))))
		    (t nil)))
	  (message "Subscribe %s? ([n]yq)" (car groups))
	  (while (not (memq (setq ans (read-char-exclusive))
			    '(?y ?\n ?\r ?q ?n)))
	    (ding)
	    (message "Subscribe %s? ([n]yq)" (car groups)))
	  (setq group (car groups))
	  (cond ((= ans ?y)
		 (gnus-subscribe-alphabetically (car groups))
		 (gnus-sethash group group gnus-killed-hashtb))
		((= ans ?q)
		 (while groups
		   (setq group (car groups))
		   (push group gnus-killed-list)
		   (gnus-sethash group group gnus-killed-hashtb)
		   (setq groups (cdr groups))))
		(t
		 (push group gnus-killed-list)
		 (gnus-sethash group group gnus-killed-hashtb)))
	  (setq groups (cdr groups)))))))

(defun gnus-subscribe-randomly (newsgroup)
  "Subscribe new NEWSGROUP by making it the first newsgroup."
  (gnus-subscribe-newsgroup newsgroup))

(defun gnus-subscribe-alphabetically (newgroup)
  "Subscribe new NEWGROUP and insert it in alphabetical order."
  (let ((groups (cdr gnus-newsrc-alist))
	before)
    (while (and (not before) groups)
      (if (string< newgroup (caar groups))
	  (setq before (caar groups))
	(setq groups (cdr groups))))
    (gnus-subscribe-newsgroup newgroup before)))

(defun gnus-subscribe-hierarchically (newgroup)
  "Subscribe new NEWGROUP and insert it in hierarchical newsgroup order."
  ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
  (with-current-buffer (nnheader-find-file-noselect gnus-current-startup-file)
    (prog1
	(let ((groupkey newgroup) before)
	  (while (and (not before) groupkey)
	    (goto-char (point-min))
	    (let ((groupkey-re
		   (concat "^\\(" (regexp-quote groupkey) ".*\\)[!:]")))
	      (while (and (re-search-forward groupkey-re nil t)
			  (progn
			    (setq before (match-string 1))
			    (string< before newgroup)))))
	    ;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
	    (setq groupkey
		  (when (string-match "^\\(.*\\)\\.[^.]+$" groupkey)
		    (substring groupkey (match-beginning 1) (match-end 1)))))
	  (gnus-subscribe-newsgroup newgroup before))
      (kill-buffer (current-buffer)))))

(defun gnus-subscribe-interactively (group)
  "Subscribe the new GROUP interactively.
It is inserted in hierarchical newsgroup order if subscribed.  If not,
it is killed."
  (if (gnus-y-or-n-p (format "Subscribe new newsgroup %s? " group))
      (gnus-subscribe-hierarchically group)
    (push group gnus-killed-list)))

(defun gnus-subscribe-zombies (group)
  "Make the new GROUP into a zombie group."
  (push group gnus-zombie-list))

(defun gnus-subscribe-killed (group)
  "Make the new GROUP a killed group."
  (push group gnus-killed-list))

(defun gnus-subscribe-newsgroup (newsgroup &optional next)
  "Subscribe new NEWSGROUP.
If NEXT is non-nil, it is inserted before NEXT.  Otherwise it is made
the first newsgroup."
  (save-excursion
    (goto-char (point-min))
    ;; We subscribe the group by changing its level to `subscribed'.
    (gnus-group-change-level
     newsgroup gnus-level-default-subscribed
     gnus-level-killed (gnus-group-entry (or next "dummy.group")))
    (gnus-request-update-group-status newsgroup 'subscribe)
    (gnus-message 5 "Subscribe newsgroup: %s" newsgroup)
    (run-hook-with-args 'gnus-subscribe-newsgroup-hooks newsgroup)
    t))

(defun gnus-read-active-file-p ()
  "Say whether the active file has been read from `gnus-select-method'."
  (memq gnus-select-method gnus-have-read-active-file))

;;; General various misc type functions.

;; Silence byte-compiler.
(defvar gnus-current-headers)
(defvar gnus-thread-indent-array)
(defvar gnus-newsgroup-name)
(defvar gnus-newsgroup-headers)
(defvar gnus-group-list-mode)
(defvar gnus-group-mark-positions)
(defvar gnus-newsgroup-data)
(defvar gnus-newsgroup-unreads)
(defvar nnoo-state-alist)
(defvar gnus-current-select-method)
(defvar mail-sources)
(defvar nnmail-scan-directory-mail-source-once)
(defvar nnmail-split-history)
(defvar nnmail-spool-file)

(defun gnus-close-all-servers ()
  "Close all servers."
  (interactive)
  (dolist (server gnus-opened-servers)
    (gnus-close-server (car server))))

(defun gnus-clear-system ()
  "Clear all variables and buffers."
  ;; Clear Gnus variables.
  (let ((variables (remove 'gnus-format-specs gnus-variable-list)))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  ;; Clear other internal variables.
  (setq gnus-list-of-killed-groups nil
	gnus-have-read-active-file nil
        gnus-agent-covered-methods nil
        gnus-agent-file-loading-local nil
        gnus-agent-file-loading-cache nil
        gnus-server-method-cache nil
	gnus-newsrc-alist nil
	gnus-newsrc-hashtb nil
	gnus-killed-list nil
	gnus-zombie-list nil
	gnus-killed-hashtb nil
	gnus-active-hashtb nil
	gnus-moderated-hashtb nil
	gnus-description-hashtb nil
	gnus-current-headers nil
	gnus-thread-indent-array nil
	gnus-newsgroup-headers nil
	gnus-newsgroup-name nil
	gnus-server-alist nil
	gnus-group-list-mode nil
	gnus-opened-servers nil
	gnus-group-mark-positions nil
	gnus-newsgroup-data nil
	gnus-newsgroup-unreads nil
	nnoo-state-alist nil
	gnus-current-select-method nil
	nnmail-split-history nil
	gnus-extended-servers nil
	gnus-ephemeral-servers nil)
  (gnus-shutdown 'gnus)
  ;; Kill the startup file.
  (and gnus-current-startup-file
       (get-file-buffer gnus-current-startup-file)
       (kill-buffer (get-file-buffer gnus-current-startup-file)))
  ;; Clear the dribble buffer.
  (gnus-dribble-clear)
  ;; Kill global KILL file buffer.
  (when (get-file-buffer (gnus-newsgroup-kill-file nil))
    (kill-buffer (get-file-buffer (gnus-newsgroup-kill-file nil))))
  (gnus-kill-buffer nntp-server-buffer)
  ;; Kill Gnus buffers.
  (dolist (buffer (gnus-buffers))
    (gnus-kill-buffer buffer))
  ;; Remove Gnus frames.
  (gnus-kill-gnus-frames))

(defun gnus-no-server-1 (&optional arg slave)
  "Read network news.
If ARG is a positive number, Gnus will use that as the startup
level.  If ARG is nil, Gnus will be started at level 2
\(`gnus-level-default-subscribed' minus one).  If ARG is non-nil
and not a positive number, Gnus will prompt the user for the name
of an NNTP server to use.  As opposed to \\[gnus], this command
will not connect to the local server."
  (interactive "P")
  (let ((val (or arg (1- gnus-level-default-subscribed))))
    (gnus val t slave)
    (make-local-variable 'gnus-group-use-permanent-levels)
    (setq gnus-group-use-permanent-levels val)))

(defun gnus-1 (&optional arg dont-connect slave)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.  If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")

  (if (gnus-alive-p)
      (progn
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-get-new-news
	 (and (numberp arg)
	      (> arg 0)
	      (max (car gnus-group-list-mode) arg))))

    (gnus-clear-system)
    (gnus-splash)
    (gnus-run-hooks 'gnus-before-startup-hook)
    (nnheader-init-server-buffer)
    (setq gnus-slave slave)
    (gnus-read-init-file)

    ;; Add "native" to gnus-predefined-server-alist just to have a
    ;; name for the native select method.
    (when gnus-select-method
      (add-to-list 'gnus-predefined-server-alist
		   (cons "native" gnus-select-method)))

    (if gnus-agent
	(gnus-agentize))

    (let ((level (and (numberp arg) (> arg 0) arg))
	  did-connect)
      (unwind-protect
	  (progn
	    (unless dont-connect
	      (setq did-connect
		    (gnus-start-news-server (and arg (not level))))))
	(if (and (not dont-connect)
		 (not did-connect))
	    ;; Couldn't connect to the server, so bail out.
	    (gnus-group-quit)
	  (gnus-run-hooks 'gnus-startup-hook)
	  ;; Find the current startup file name.
	  (setq gnus-current-startup-file
		(gnus-make-newsrc-file gnus-startup-file))

	  ;; Read the dribble file.
	  (when (or gnus-slave gnus-use-dribble-file)
	    (gnus-dribble-read-file))

	  ;; Do the actual startup.
	  (gnus-setup-news nil level dont-connect)
	  (gnus-run-hooks 'gnus-setup-news-hook)
	  (when gnus-agent
	    (gnus-request-create-group "queue" '(nndraft "")))
	  (gnus-start-draft-setup)
	  ;; Generate the group buffer.
	  (gnus-group-list-groups level)
	  (gnus-group-first-unread-group)
	  (gnus-configure-windows 'group)
	  (gnus-group-set-mode-line)
	  (gnus-run-hooks 'gnus-started-hook))))))

(defun gnus-start-draft-setup ()
  "Make sure the draft group exists."
  (interactive)
  (gnus-request-create-group "drafts" '(nndraft ""))
  (unless (gnus-group-entry "nndraft:drafts")
    (let ((gnus-level-default-subscribed 1))
      (gnus-subscribe-group "nndraft:drafts" nil '(nndraft "")))
    (setcar (gnus-group-entry "nndraft:drafts") 0))
  (unless (equal (gnus-group-get-parameter "nndraft:drafts" 'gnus-dummy t)
		 '((gnus-draft-mode)))
    (gnus-group-set-parameter
     "nndraft:drafts" 'gnus-dummy '((gnus-draft-mode)))))


;;;
;;; Dribble file
;;;

(defvar gnus-dribble-ignore nil)
(defvar gnus-dribble-eval-file nil)

(defun gnus-dribble-file-name ()
  "Return the dribble file for the current .newsrc."
  (concat
   (if gnus-dribble-directory
       (concat (file-name-as-directory gnus-dribble-directory)
	       (file-name-nondirectory gnus-current-startup-file))
     gnus-current-startup-file)
   "-dribble"))

(defun gnus-dribble-enter (string &optional regexp)
  "Enter STRING into the dribble buffer.
If REGEXP is given, lines that match it will be deleted."
  (when (and (not gnus-dribble-ignore)
	     gnus-dribble-buffer
	     (buffer-name gnus-dribble-buffer))
    (let ((obuf (current-buffer)))
      (set-buffer gnus-dribble-buffer)
      (when regexp
	(goto-char (point-min))
	(let (end)
	  (while (re-search-forward regexp nil t)
	    (unless (bolp) (forward-line 1))
	    (setq end (point))
	    (goto-char (match-beginning 0))
	    (delete-region (point-at-bol) end))))
      (goto-char (point-max))
      (insert string "\n")
      ;; This has been commented by Josh Huber <huber@alum.wpi.edu>
      ;; It causes problems with both XEmacs and Emacs 21, and doesn't
      ;; seem to be of much value. (FIXME: remove this after we make sure
      ;; it's not needed).
      ;; (set-window-point (get-buffer-window (current-buffer)) (point-max))
      (bury-buffer gnus-dribble-buffer)
      (with-current-buffer gnus-group-buffer
	(gnus-group-set-mode-line))
      (set-buffer obuf))))

(defun gnus-dribble-touch ()
  "Touch the dribble buffer."
  (gnus-dribble-enter ""))

(defun gnus-dribble-read-file ()
  "Read the dribble file from disk."
  (let ((dribble-file (gnus-dribble-file-name)))
    (unless (file-exists-p (file-name-directory dribble-file))
      (make-directory (file-name-directory dribble-file) t))
    (with-current-buffer (setq gnus-dribble-buffer
			       (gnus-get-buffer-create
				(file-name-nondirectory dribble-file)))
      (set (make-local-variable 'file-precious-flag) t)
      (setq buffer-save-without-query t)
      (erase-buffer)
      (setq buffer-file-name dribble-file)
      (auto-save-mode t)
      (buffer-disable-undo)
      (bury-buffer (current-buffer))
      (set-buffer-modified-p nil)
      (let ((auto (make-auto-save-file-name))
	    (gnus-dribble-ignore t)
	    (purpose nil)
	    modes)
	(when (or (file-exists-p auto) (file-exists-p dribble-file))
	  ;; Load whichever file is newest -- the auto save file
	  ;; or the "real" file.
	  (if (file-newer-than-file-p auto dribble-file)
	      (nnheader-insert-file-contents auto)
	    (nnheader-insert-file-contents dribble-file))
	  (unless (zerop (buffer-size))
	    (set-buffer-modified-p t))
	  ;; Set the file modes to reflect the .newsrc file modes.
	  (save-buffer)
	  (when (and (file-exists-p gnus-current-startup-file)
		     (file-exists-p dribble-file)
		     (setq modes (file-modes gnus-current-startup-file)))
	    (gnus-set-file-modes dribble-file modes))
	  (goto-char (point-min))
	  (when (search-forward "Gnus was exited on purpose" nil t)
	    (setq purpose t))
	  ;; Possibly eval the file later.
	  (when (or gnus-always-read-dribble-file
		    (gnus-y-or-n-p
		     (if purpose
			 "Gnus exited on purpose without saving; read auto-save file anyway? "
		     "Gnus auto-save file exists.  Do you want to read it? ")))
	    (setq gnus-dribble-eval-file t)))))))

(defun gnus-dribble-eval-file ()
  (when gnus-dribble-eval-file
    (setq gnus-dribble-eval-file nil)
    (save-excursion
      (let ((gnus-dribble-ignore t))
	(set-buffer gnus-dribble-buffer)
	(eval-buffer (current-buffer))))))

(defun gnus-dribble-delete-file ()
  (when (file-exists-p (gnus-dribble-file-name))
    (delete-file (gnus-dribble-file-name)))
  (when gnus-dribble-buffer
    (with-current-buffer gnus-dribble-buffer
      (let ((auto (make-auto-save-file-name)))
	(when (file-exists-p auto)
	  (delete-file auto))
	(erase-buffer)
	(set-buffer-modified-p nil)))))

(defun gnus-dribble-save ()
  (when (and gnus-dribble-buffer
	     (buffer-name gnus-dribble-buffer))
    (with-current-buffer gnus-dribble-buffer
      (save-buffer))))

(defun gnus-dribble-clear ()
  (when (gnus-buffer-exists-p gnus-dribble-buffer)
    (with-current-buffer gnus-dribble-buffer
      (erase-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-saved-size (buffer-size)))))


;;;
;;; Active & Newsrc File Handling
;;;

(defun gnus-setup-news (&optional rawfile level dont-connect)
  "Setup news information.
If RAWFILE is non-nil, the .newsrc file will also be read.
If LEVEL is non-nil, the news will be set up at level LEVEL."
  (require 'nnmail)
  (let ((init (not (and gnus-newsrc-alist gnus-active-hashtb (not rawfile))))
	;; Binding this variable will inhibit multiple fetchings
	;; of the same mail source.
	(nnmail-fetched-sources (list t)))

    (when init
      ;; Clear some variables to re-initialize news information.
      (setq gnus-newsrc-alist nil
	    gnus-active-hashtb nil)
      ;; Read the newsrc file and create `gnus-newsrc-hashtb'.
      (gnus-read-newsrc-file rawfile))

    ;; Make sure the archive server is available to all and sundry.
    (let ((method (or (and (stringp gnus-message-archive-method)
			   (gnus-server-to-method
			    gnus-message-archive-method))
		      gnus-message-archive-method)))
      ;; Check whether the archive method is writable.
      (unless (or (not method)
		  (stringp method)
		  (memq 'respool (assoc (format "%s" (car method))
					gnus-valid-select-methods)))
	(setq method "archive")) ;; The default.
      (when (stringp method)
	(setq method `(nnfolder
		       ,method
		       (nnfolder-directory
			,(nnheader-concat message-directory method))
		       (nnfolder-active-file
			,(nnheader-concat message-directory
					  (concat method "/active")))
		       (nnfolder-get-new-mail nil)
		       (nnfolder-inhibit-expiry t))))
      (if (assoc "archive" gnus-server-alist)
	  (when gnus-update-message-archive-method
	    (if method
		(setcdr (assoc "archive" gnus-server-alist) method)
	      (setq gnus-server-alist (delq (assoc "archive" gnus-server-alist)
					    gnus-server-alist))))
	(when method
	  (push (cons "archive" method) gnus-server-alist))))

    ;; If we don't read the complete active file, we fill in the
    ;; hashtb here.
    (when (or (null gnus-read-active-file)
	      (eq gnus-read-active-file 'some))
      (gnus-update-active-hashtb-from-killed))
    (unless gnus-active-hashtb
      (setq gnus-active-hashtb (gnus-make-hashtable 4096)))
    ;; Initialize the cache.
    (when gnus-use-cache
      (gnus-cache-open))

    ;; Possibly eval the dribble file.
    (and init
	 (or gnus-use-dribble-file gnus-slave)
	 (gnus-dribble-eval-file))

    ;; Slave Gnusii should then clear the dribble buffer.
    (when (and init gnus-slave)
      (gnus-dribble-clear))

    (gnus-update-format-specifications)

    ;; See whether we need to read the description file.
    (when (and (boundp 'gnus-group-line-format)
	       (stringp gnus-group-line-format)
	       (let ((case-fold-search nil))
		 (string-match "%[-,0-9]*D" gnus-group-line-format))
	       (not gnus-description-hashtb)
	       (not dont-connect)
	       gnus-read-active-file)
      (gnus-read-all-descriptions-files))

    ;; Find new newsgroups and treat them.
    (when (and init gnus-check-new-newsgroups (not level)
	       (gnus-check-server gnus-select-method)
	       (not gnus-slave)
	       gnus-plugged)
      (gnus-find-new-newsgroups))

    ;; Check and remove bogus newsgroups.
    (when (and init gnus-check-bogus-newsgroups
	       gnus-read-active-file (not level)
	       (gnus-server-opened gnus-select-method))
      (gnus-check-bogus-newsgroups))

    ;; Read any slave files.
    (gnus-master-read-slave-newsrc)

    ;; Find the number of unread articles in each non-dead group.
    (let ((gnus-read-active-file (and (not level) gnus-read-active-file)))
      (gnus-get-unread-articles level dont-connect))))

(defun gnus-call-subscribe-functions (method group)
  "Call METHOD to subscribe GROUP.
If no function returns `non-nil', call `gnus-subscribe-zombies'."
  (unless (cond
	   ((functionp method)
	    (funcall method group))
	   ((listp method)
	    (catch 'found
	      (dolist (func method)
		(if (funcall func group)
		    (throw 'found t)))
	      nil))
	   (t nil))
    (gnus-subscribe-zombies group)))

(defun gnus-find-new-newsgroups (&optional arg)
  "Search for new newsgroups and add them.
Each new newsgroup will be treated with `gnus-subscribe-newsgroup-method'.
The `-n' option line from .newsrc is respected.

With 1 C-u, use the `ask-server' method to query the server for new
groups.
With 2 C-u's, use most complete method possible to query the server
for new groups, and subscribe the new groups as zombies."
  (interactive "p")
  (let* ((gnus-subscribe-newsgroup-method
	  gnus-subscribe-newsgroup-method)
	 (check (cond
		 ((or (and (= (or arg 1) 4)
			   (not (listp gnus-check-new-newsgroups)))
		      (null gnus-read-active-file)
		      (eq gnus-read-active-file 'some))
		  'ask-server)
		 ((= (or arg 1) 16)
		  (setq gnus-subscribe-newsgroup-method
			'gnus-subscribe-zombies)
		  t)
		 (t gnus-check-new-newsgroups))))
    (if (or (consp check)
            (eq check 'ask-server))
        ;; Ask the server for new groups.
        (gnus-ask-server-for-new-groups)
      ;; Go through the active hashtb and look for new groups.
      (let ((groups 0)
            group new-newsgroups)
        (gnus-message 5 "Looking for new newsgroups...")
        (unless gnus-have-read-active-file
          (gnus-read-active-file))
        (setq gnus-newsrc-last-checked-date (message-make-date))
        (unless gnus-killed-hashtb
          (gnus-make-hashtable-from-killed))
        ;; Go though every newsgroup in `gnus-active-hashtb' and compare
        ;; with `gnus-newsrc-hashtb' and `gnus-killed-hashtb'.
        (mapatoms
         (lambda (sym)
           (if (or (null (setq group (symbol-name sym)))
                   (not (boundp sym))
                   (null (symbol-value sym))
                   (gnus-gethash group gnus-killed-hashtb)
                   (gnus-gethash group gnus-newsrc-hashtb))
               ()
             (let ((do-sub (gnus-matches-options-n group)))
               (cond
                ((eq do-sub 'subscribe)
                 (setq groups (1+ groups))
                 (gnus-sethash group group gnus-killed-hashtb)
                 (gnus-call-subscribe-functions
                  gnus-subscribe-options-newsgroup-method group))
                ((eq do-sub 'ignore)
                 nil)
                (t
                 (setq groups (1+ groups))
                 (gnus-sethash group group gnus-killed-hashtb)
                 (if gnus-subscribe-hierarchical-interactive
                     (push group new-newsgroups)
                   (gnus-call-subscribe-functions
                    gnus-subscribe-newsgroup-method group)))))))
         gnus-active-hashtb)
        (when new-newsgroups
          (gnus-subscribe-hierarchical-interactive new-newsgroups))
        (if (> groups 0)
            (gnus-message 5 "%d new newsgroup%s arrived."
                          groups (if (> groups 1) "s have" " has"))
          (gnus-message 5 "No new newsgroups."))
	groups))))

(defun gnus-matches-options-n (group)
  ;; Returns `subscribe' if the group is to be unconditionally
  ;; subscribed, `ignore' if it is to be ignored, and nil if there is
  ;; no match for the group.

  ;; First we check the two user variables.
  (cond
   ((and gnus-options-subscribe
	 (string-match gnus-options-subscribe group))
    'subscribe)
   ((let ((do-subscribe nil))
      (dolist (category gnus-auto-subscribed-categories)
	(when (gnus-member-of-valid category group)
	  (setq do-subscribe t)))
      do-subscribe)
    'subscribe)
   ((and gnus-auto-subscribed-groups
	 (string-match gnus-auto-subscribed-groups group))
    'subscribe)
   ((and gnus-options-not-subscribe
	 (string-match gnus-options-not-subscribe group))
    'ignore)
   ;; Then we go through the list that was retrieved from the .newsrc
   ;; file.  This list has elements on the form
   ;; `(REGEXP . {ignore,subscribe})'.  The first match found (the list
   ;; is in the reverse order of the options line) is returned.
   (t
    (let ((regs gnus-newsrc-options-n))
      (while (and regs
		  (not (string-match (caar regs) group)))
	(setq regs (cdr regs)))
      (and regs (cdar regs))))))

(defun gnus-ask-server-for-new-groups ()
  (let* ((new-date (message-make-date))
	 (date (or gnus-newsrc-last-checked-date new-date))
	 (methods (cons gnus-select-method
			(nconc
			 (when (gnus-archive-server-wanted-p)
			   (list "archive"))
			 (append
			  (and (consp gnus-check-new-newsgroups)
			       gnus-check-new-newsgroups)
			  gnus-secondary-select-methods))))
	 (groups 0)
	 group new-newsgroups got-new method hashtb
	 gnus-override-subscribe-method)
    (unless gnus-killed-hashtb
      (gnus-make-hashtable-from-killed))
    ;; Go through both primary and secondary select methods and
    ;; request new newsgroups.
    (while (setq method (gnus-server-get-method nil (pop methods)))
      (setq new-newsgroups nil
	    gnus-override-subscribe-method method)
      (when (and (gnus-check-server method)
		 (gnus-request-newgroups date method))
	(save-excursion
	  (setq got-new t
		hashtb (gnus-make-hashtable 100))
	  (set-buffer nntp-server-buffer)
	  ;; Enter all the new groups into a hashtable.
	  (gnus-active-to-gnus-format method hashtb 'ignore))
	;; Now all new groups from `method' are in `hashtb'.
	(mapatoms
	 (lambda (group-sym)
	   (if (or (null (setq group (symbol-name group-sym)))
		   (not (boundp group-sym))
		   (null (symbol-value group-sym))
		   (gnus-gethash group gnus-newsrc-hashtb)
		   (member group gnus-zombie-list)
		   (member group gnus-killed-list))
	       ;; The group is already known.
	       ()
	     ;; Make this group active.
	     (when (symbol-value group-sym)
	       (gnus-set-active group (symbol-value group-sym)))
	     ;; Check whether we want it or not.
	     (let ((do-sub (gnus-matches-options-n group)))
	       (cond
		((eq do-sub 'subscribe)
		 (incf groups)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (gnus-call-subscribe-functions
		  gnus-subscribe-options-newsgroup-method group))
		((eq do-sub 'ignore)
		 nil)
		(t
		 (incf groups)
		 (gnus-sethash group group gnus-killed-hashtb)
		 (if gnus-subscribe-hierarchical-interactive
		     (push group new-newsgroups)
		   (gnus-call-subscribe-functions
		    gnus-subscribe-newsgroup-method group)))))))
	 hashtb))
      (when new-newsgroups
	(gnus-subscribe-hierarchical-interactive new-newsgroups)))
    (if (> groups 0)
	(gnus-message 5 "%d new newsgroup%s arrived"
		      groups (if (> groups 1) "s have" " has"))
      (gnus-message 5 "No new newsgroups"))
    (when got-new
      (setq gnus-newsrc-last-checked-date new-date))
    new-newsgroups))

(defun gnus-subscribe-group (group &optional previous method)
  "Subscribe GROUP and put it after PREVIOUS."
  (gnus-group-change-level
   (if method
       (list t group gnus-level-default-subscribed nil nil method)
     group)
   gnus-level-default-subscribed gnus-level-killed previous t)
  t)

;; `gnus-group-change-level' is the fundamental function for changing
;; subscription levels of newsgroups.  This might mean just changing
;; from level 1 to 2, which is pretty trivial, from 2 to 6 or back
;; again, which subscribes/unsubscribes a group, which is equally
;; trivial.  Changing from 1-7 to 8-9 means that you kill a group, and
;; from 8-9 to 1-7 means that you remove the group from the list of
;; killed (or zombie) groups and add them to the (kinda) subscribed
;; groups.  And last but not least, moving from 8 to 9 and 9 to 8,
;; which is trivial.
;; ENTRY can either be a string (newsgroup name) or a list (if
;; FROMKILLED is t, it's a list on the format (NUM INFO-LIST),
;; otherwise it's a list in the format of the `gnus-newsrc-hashtb'
;; entries.
;; LEVEL is the new level of the group, OLDLEVEL is the old level and
;; PREVIOUS is the group (in hashtb entry format) to insert this group
;; after.
(defun gnus-group-change-level (entry level &optional oldlevel
				      previous fromkilled)
  (let (group info active num)
    ;; Glean what info we can from the arguments
    (if (consp entry)
	(if fromkilled (setq group (nth 1 entry))
	  (setq group (car (nth 2 entry))))
      (setq group entry))
    (when (and (stringp entry)
	       oldlevel
	       (< oldlevel gnus-level-zombie))
      (setq entry (gnus-group-entry entry)))
    (if (and (not oldlevel)
	     (consp entry))
	(setq oldlevel (gnus-info-level (nth 2 entry)))
      (setq oldlevel (or oldlevel gnus-level-killed)))
    (when (stringp previous)
      (setq previous (gnus-group-entry previous)))

    (if (and (>= oldlevel gnus-level-zombie)
	     (gnus-group-entry group))
	;; We are trying to subscribe a group that is already
	;; subscribed.
	()				; Do nothing.

      (unless (gnus-ephemeral-group-p group)
	(gnus-dribble-enter
	 (format "(gnus-group-change-level %S %S %S %S %S)"
		 group level oldlevel (car (nth 2 previous)) fromkilled)))

      ;; Then we remove the newgroup from any old structures, if needed.
      ;; If the group was killed, we remove it from the killed or zombie
      ;; list.  If not, and it is in fact going to be killed, we remove
      ;; it from the newsrc hash table and assoc.
      (cond
       ((>= oldlevel gnus-level-zombie)
	;; oldlevel could be wrong.
	(setq gnus-zombie-list (delete group gnus-zombie-list))
	(setq gnus-killed-list (delete group gnus-killed-list)))
       (t
	(when (and (>= level gnus-level-zombie)
		   entry)
	  (gnus-sethash (car (nth 2 entry)) nil gnus-newsrc-hashtb)
	  (when (nth 3 entry)
	    (setcdr (gnus-group-entry (car (nth 3 entry)))
		    (cdr entry)))
	  (setcdr (cdr entry) (cdddr entry)))))

      ;; Finally we enter (if needed) the list where it is supposed to
      ;; go, and change the subscription level.  If it is to be killed,
      ;; we enter it into the killed or zombie list.
      (cond
       ((>= level gnus-level-zombie)
	;; Remove from the hash table.
	(gnus-sethash group nil gnus-newsrc-hashtb)
	(if (= level gnus-level-zombie)
	    (push group gnus-zombie-list)
	  (if (= oldlevel gnus-level-killed)
	      ;; Remove from active hashtb.
	      (unintern group gnus-active-hashtb)
	    ;; Don't add it into killed-list if it was killed.
	    (push group gnus-killed-list))))
       (t
	;; If the list is to be entered into the newsrc assoc, and
	;; it was killed, we have to create an entry in the newsrc
	;; hashtb format and fix the pointers in the newsrc assoc.
	(if (< oldlevel gnus-level-zombie)
	    ;; It was alive, and it is going to stay alive, so we
	    ;; just change the level and don't change any pointers or
	    ;; hash table entries.
	    (setcar (cdaddr entry) level)
	  (if (listp entry)
	      (setq info (cdr entry)
		    num (car entry))
	    (setq active (gnus-active group))
	    (setq num
		  (if active (- (1+ (cdr active)) (car active)) t))
	    ;; Shorten the select method if possible, if we need to
	    ;; store it at all (native groups).
	    (let ((method (gnus-method-simplify
			   (or gnus-override-subscribe-method
			       (gnus-group-method group)))))
	      (if method
		  (setq info (list group level nil nil method))
		(setq info (list group level nil)))))
	  (unless previous
	    (setq previous
		  (let ((p gnus-newsrc-alist))
		    (while (cddr p)
		      (setq p (cdr p)))
		    p)))
	  (setq entry (cons info (cddr previous)))
	  (if (cdr previous)
	      (progn
		(setcdr (cdr previous) entry)
		(gnus-sethash group (cons num (cdr previous))
			      gnus-newsrc-hashtb))
	    (setcdr previous entry)
	    (gnus-sethash group (cons num previous)
			  gnus-newsrc-hashtb))
	  (when (cdr entry)
	    (setcdr (gnus-group-entry (caadr entry)) entry))
	  (gnus-dribble-enter
	   (format "(gnus-group-set-info '%S)" info)
	   (concat "^(gnus-group-set-info '(\"" (regexp-quote group) "\"")))))
      (when gnus-group-change-level-function
	(funcall gnus-group-change-level-function
		 group level oldlevel previous)))))

(defun gnus-kill-newsgroup (newsgroup)
  "Obsolete function.  Kills a newsgroup."
  (gnus-group-change-level
   (gnus-group-entry newsgroup) gnus-level-killed))

(defun gnus-check-bogus-newsgroups (&optional confirm)
  "Remove bogus newsgroups.
If CONFIRM is non-nil, the user has to confirm the deletion of every
newsgroup."
  (let ((newsrc (cdr gnus-newsrc-alist))
	bogus group entry info)
    (gnus-message 5 "Checking bogus newsgroups...")
    (unless (gnus-read-active-file-p)
      (gnus-read-active-file t))
    (when (gnus-read-active-file-p)
      ;; Find all bogus newsgroup that are subscribed.
      (while newsrc
	(setq info (pop newsrc)
	      group (gnus-info-group info))
	(unless (or (gnus-active group)	; Active
		    (and (gnus-info-method info)
			 (not (gnus-secondary-method-p
			       (gnus-info-method info))))) ; Foreign
	  ;; Found a bogus newsgroup.
	  (push group bogus)))
      (if confirm
	  (map-y-or-n-p
	   (format "Remove bogus group %%s (of %d groups)? " (length bogus))
	   (lambda (group)
	     ;; Remove all bogus subscribed groups by first killing them, and
	     ;; then removing them from the list of killed groups.
	     (when (setq entry (gnus-group-entry group))
	       (gnus-group-change-level entry gnus-level-killed)
	       (setq gnus-killed-list (delete group gnus-killed-list))))
	   bogus '("group" "groups" "remove"))
	(while (setq group (pop bogus))
	  ;; Remove all bogus subscribed groups by first killing them, and
	  ;; then removing them from the list of killed groups.
	  (when (setq entry (gnus-group-entry group))
	    (gnus-group-change-level entry gnus-level-killed)
	    (setq gnus-killed-list (delete group gnus-killed-list)))))
      ;; Then we remove all bogus groups from the list of killed and
      ;; zombie groups.  They are removed without confirmation.
      (let ((dead-lists '(gnus-killed-list gnus-zombie-list))
	    killed)
	(while dead-lists
	  (setq killed (symbol-value (car dead-lists)))
	  (while killed
	    (unless (gnus-active (setq group (pop killed)))
	      ;; The group is bogus.
	      ;; !!!Slow as hell.
	      (set (car dead-lists)
		   (delete group (symbol-value (car dead-lists))))))
	  (setq dead-lists (cdr dead-lists))))
      (gnus-run-hooks 'gnus-check-bogus-groups-hook)
      (gnus-message 5 "Checking bogus newsgroups...done"))))

(defun gnus-check-duplicate-killed-groups ()
  "Remove duplicates from the list of killed groups."
  (interactive)
  (let ((killed gnus-killed-list))
    (while killed
      (gnus-message 9 "%d" (length killed))
      (setcdr killed (delete (car killed) (cdr killed)))
      (setq killed (cdr killed)))))

;; We want to inline a function from gnus-cache, so we cheat here:
(defvar gnus-cache-active-hashtb)
(eval-when-compile
  (defun gnus-cache-possibly-alter-active (group active)
    "Alter the ACTIVE info for GROUP to reflect the articles in the cache."
    (when gnus-cache-active-hashtb
      (let ((cache-active (gnus-gethash group gnus-cache-active-hashtb)))
	(when cache-active
	  (when (< (car cache-active) (car active))
	    (setcar active (car cache-active)))
	  (when (> (cdr cache-active) (cdr active))
	    (setcdr active (cdr cache-active))))))))

(defun gnus-activate-group (group &optional scan dont-check method
				  dont-sub-check)
  "Check whether a group has been activated or not.
If SCAN, request a scan of that group as well.  If METHOD, use
that select method instead of determining the method based on the
group name.  If DONT-CHECK, don't check check whether the group
actually exists.  If DONT-SUB-CHECK or DONT-CHECK, don't let the
backend check whether the group actually exists."
  (let ((method (or method (inline (gnus-find-method-for-group group))))
	active)
    (and (inline (gnus-check-server method))
	 ;; We escape all bugs and quit here to make it possible to
	 ;; continue if a group is so out-there that it reports bugs
	 ;; and stuff.
	 (progn
	   (and scan
		(gnus-check-backend-function 'request-scan (car method))
		(gnus-request-scan group method))
	   t)
	 (if (or debug-on-error debug-on-quit)
	     (inline (gnus-request-group group (or dont-sub-check dont-check)
					 method
					 (gnus-get-info group)))
	   (condition-case nil
	       (inline (gnus-request-group group (or dont-sub-check dont-check)
					   method
					   (gnus-get-info group)))
	     (quit
	      (if debug-on-quit
		  (debug "Quit")
		(message "Quit activating %s" group))
	      nil)))
	 (unless dont-check
	   (setq active (gnus-parse-active))
	   ;; If there are no articles in the group, the GROUP
	   ;; command may have responded with the `(0 . 0)'.  We
	   ;; ignore this if we already have an active entry
	   ;; for the group.
	   (if (and (zerop (or (car active) 0))
		    (zerop (or (cdr active) 0))
		    (gnus-active group))
	       (gnus-active group)

             ;; If a cache is present, we may have to alter the active info.
             (when gnus-use-cache
               (inline (gnus-cache-possibly-alter-active
                        group active)))

             ;; If the agent is enabled, we may have to alter the active info.
             (when gnus-agent
               (gnus-agent-possibly-alter-active group active))

	     (gnus-set-active group active)
	     ;; Return the new active info.
	     active)))))

(defvar gnus-propagate-marks)		; gnus-sum

(defun gnus-get-unread-articles-in-group (info active &optional update)
  (when (and info active)
    ;; Allow the backend to update the info in the group.
    (when (and update
	       (gnus-request-update-info
		info (inline (gnus-find-method-for-group
			      (gnus-info-group info)))))
      (gnus-activate-group (gnus-info-group info) nil t))

    ;; Allow backends to update marks,
    (when gnus-propagate-marks
      (let ((method (inline (gnus-find-method-for-group
			     (gnus-info-group info)))))
	(when (gnus-check-backend-function 'request-marks (car method))
	  (gnus-request-marks info method))))

    (let* ((range (gnus-info-read info))
	   (num 0))

      ;; These checks are present in gnus-activate-group but skipped
      ;; due to setting dont-check in the preceding call.

      ;; If a cache is present, we may have to alter the active info.
      (when (and gnus-use-cache info)
	(inline (gnus-cache-possibly-alter-active
		 (gnus-info-group info) active)))

      ;; If the agent is enabled, we may have to alter the active info.
      (when (and gnus-agent info)
	(gnus-agent-possibly-alter-active (gnus-info-group info) active info))

      ;; Modify the list of read articles according to what articles
      ;; are available; then tally the unread articles and add the
      ;; number to the group hash table entry.
      (cond
       ((zerop (cdr active))
	(setq num 0))
       ((not range)
	(setq num (- (1+ (cdr active)) (car active))))
       ((not (listp (cdr range)))
	;; Fix a single (num . num) range according to the
	;; active hash table.
	;; Fix by Carsten Bormann <cabo@Informatik.Uni-Bremen.DE>.
	(and (< (cdr range) (car active)) (setcdr range (1- (car active))))
	(and (> (cdr range) (cdr active)) (setcdr range (cdr active)))
	;; Compute number of unread articles.
	(setq num (max 0 (- (cdr active) (- (1+ (cdr range)) (car range))))))
       (t
	;; The read list is a list of ranges.  Fix them according to
	;; the active hash table.
	;; First peel off any elements that are below the lower
	;; active limit.
	(while (and (cdr range)
		    (>= (car active)
			(or (and (atom (cadr range)) (cadr range))
			    (caadr range))))
	  (if (numberp (car range))
	      (setcar range
		      (cons (car range)
			    (or (and (numberp (cadr range))
				     (cadr range))
				(cdadr range))))
	    (setcdr (car range)
		    (or (and (numberp (nth 1 range)) (nth 1 range))
			(cdadr range))))
	  (setcdr range (cddr range)))
	;; Adjust the first element to be the same as the lower limit.
	(when (and (not (atom (car range)))
		   (< (cdar range) (car active)))
	  (setcdr (car range) (1- (car active))))
	;; Then we want to peel off any elements that are higher
	;; than the upper active limit.
	(let ((srange range))
	  ;; Go past all valid elements.
	  (while (and (cdr srange)
		      (<= (or (and (atom (cadr srange))
				   (cadr srange))
			      (caadr srange))
			  (cdr active)))
	    (setq srange (cdr srange)))
	  (when (cdr srange)
	    ;; Nuke all remaining invalid elements.
	    (setcdr srange nil))

	  ;; Adjust the final element.
	  (when (and (not (atom (car srange)))
		     (> (cdar srange) (cdr active)))
	    (setcdr (car srange) (cdr active))))
	;; Compute the number of unread articles.
	(while range
	  (setq num (+ num (- (1+ (or (and (atom (car range)) (car range))
				      (cdar range)))
			      (or (and (atom (car range)) (car range))
				  (caar range)))))
	  (setq range (cdr range)))
	(setq num (max 0 (- (cdr active) num)))))
      ;; Set the number of unread articles.
      (when (and info
		 (gnus-group-entry (gnus-info-group info)))
	(setcar (gnus-group-entry (gnus-info-group info)) num))
      num)))

;; Go though `gnus-newsrc-alist' and compare with `gnus-active-hashtb'
;; and compute how many unread articles there are in each group.
(defun gnus-get-unread-articles (&optional level dont-connect)
  (setq gnus-server-method-cache nil)
  (require 'gnus-agent)
  (let* ((newsrc (cdr gnus-newsrc-alist))
	 (alevel (or level gnus-activate-level (1+ gnus-level-subscribed)))
	 (foreign-level
	  (or
	   level
	   (min
	    (cond ((and gnus-activate-foreign-newsgroups
			(not (numberp gnus-activate-foreign-newsgroups)))
		   (1+ gnus-level-subscribed))
		  ((numberp gnus-activate-foreign-newsgroups)
		   gnus-activate-foreign-newsgroups)
		  (t 0))
	    alevel)))
	 (methods-cache nil)
	 (type-cache nil)
	 (gnus-agent-article-local-times 0)
	 (archive-method (gnus-server-to-method "archive"))
	 infos info group active method cmethod
	 method-type method-group-list entry)
    (gnus-message 6 "Checking new news...")

    (while newsrc
      (setq active (gnus-active (setq group (gnus-info-group
					     (setq info (pop newsrc))))))
      ;; First go through all the groups, see what select methods they
      ;; belong to, and then collect them into lists per unique select
      ;; method.
      (if (not (setq method (gnus-info-method info)))
	  (setq method gnus-select-method)
	;; There may be several similar methods.  Possibly extend the
	;; method.
	(if (setq cmethod (assoc method methods-cache))
	    (setq method (cdr cmethod))
	  (setq cmethod (if (stringp method)
			    (gnus-server-to-method method)
			  (inline (gnus-find-method-for-group
				   (gnus-info-group info) info))))
	  (push (cons method cmethod) methods-cache)
	  (setq method cmethod)))
      (setq method-group-list (assoc method type-cache))
      (unless method-group-list
	(setq method-type
	      (cond
	       ((or (gnus-secondary-method-p method)
		    (and (gnus-archive-server-wanted-p)
			 (gnus-methods-equal-p archive-method method)))
		'secondary)
	       ((inline (gnus-server-equal gnus-select-method method))
		'primary)
	       (t
		'foreign)))
	(push (setq method-group-list (list method method-type nil nil))
	      type-cache))
      ;; Only add groups that need updating.
      (if (<= (gnus-info-level info)
	      (if (eq (cadr method-group-list) 'foreign)
		  foreign-level
		alevel))
	  (setcar (nthcdr 2 method-group-list)
		  (cons info (nth 2 method-group-list)))
	;; The group is inactive, so we nix out the number of unread articles.
	;; It leads `(gnus-group-unread group)' to return t.  See also
	;; `gnus-group-prepare-flat'.
	(unless active
	  (when (setq entry (gnus-group-entry group))
	    (setcar entry t)))))

    ;; Sort the methods based so that the primary and secondary
    ;; methods come first.  This is done for legacy reasons to try to
    ;; ensure that side-effect behavior doesn't change from previous
    ;; Gnus versions.
    (setq type-cache
	  (sort (nreverse type-cache)
		(lambda (c1 c2)
		  (< (gnus-method-rank (cadr c1) (car c1))
		     (gnus-method-rank (cadr c2) (car c2))))))
    ;; Go through the list of servers and possibly extend methods that
    ;; aren't equal (and that need extension; i.e., they are async).
    (let ((methods nil))
      (dolist (elem type-cache)
	(destructuring-bind (method method-type infos dummy) elem
	  (let ((gnus-opened-servers methods))
	    (when (and (gnus-similar-server-opened method)
		       (gnus-check-backend-function
			'retrieve-group-data-early (car method)))
	      (setq method (gnus-server-extend-method
			    (gnus-info-group (car infos))
			    method))
	      (setcar elem method))
	    (push (list method 'ok) methods)))))

    ;; If we have primary/secondary select methods, but no groups from
    ;; them, we still want to issue a retrieval request from them.
    (unless dont-connect
      (dolist (method (cons gnus-select-method
			    gnus-secondary-select-methods))
	(when (and (not (assoc method type-cache))
		   (gnus-check-backend-function 'request-list (car method)))
	  (with-current-buffer nntp-server-buffer
	    (gnus-read-active-file-1 method nil)))))

    ;; Clear out all the early methods.
    (dolist (elem type-cache)
      (destructuring-bind (method method-type infos dummy) elem
	(when (and method
		   infos
		   (gnus-check-backend-function
		    'retrieve-group-data-early (car method))
		   (not (gnus-method-denied-p method)))
	  (when (ignore-errors (gnus-get-function method 'open-server))
	    (unless (gnus-server-opened method)
	      (gnus-open-server method))
	    (when (gnus-server-opened method)
	      ;; Just mark this server as "cleared".
	      (gnus-retrieve-group-data-early method nil))))))

    ;; Start early async retrieval of data.
    (let ((done-methods nil)
	  sanity-spec)
      (dolist (elem type-cache)
	(destructuring-bind (method method-type infos dummy) elem
	  (setq sanity-spec (list (car method) (cadr method)))
	  (when (and method infos
		     (not (gnus-method-denied-p method)))
	    ;; If the open-server method doesn't exist, then the method
	    ;; itself doesn't exist, so we ignore it.
	    (if (not (ignore-errors (gnus-get-function method 'open-server)))
		(setq type-cache (delq elem type-cache))
	      (unless (gnus-server-opened method)
		(gnus-open-server method))
	      (when (and
		     ;; This is a sanity check, so that we never
		     ;; attempt to start two async requests to the
		     ;; same server, because that will fail.  This
		     ;; should never happen, since the methods should
		     ;; be unique at this point, but apparently it
		     ;; does happen in the wild with some setups.
		     (not (member sanity-spec done-methods))
		     (gnus-server-opened method)
		     (gnus-check-backend-function
		      'retrieve-group-data-early (car method)))
		(push sanity-spec done-methods)
		(when (gnus-check-backend-function 'request-scan (car method))
		  (gnus-request-scan nil method))
		;; Store the token we get back from -early so that we
		;; can pass it to -finish later.
		(setcar (nthcdr 3 elem)
			(gnus-retrieve-group-data-early method infos))))))))

    ;; Do the rest of the retrieval.
    (dolist (elem type-cache)
      (destructuring-bind (method method-type infos early-data) elem
	(when (and method infos
		   (not (gnus-method-denied-p method)))
	  (let ((updatep (gnus-check-backend-function
			  'request-update-info (car method))))
	    ;; See if any of the groups from this method require updating.
	    (gnus-read-active-for-groups method infos early-data)
	    (dolist (info infos)
	      (inline (gnus-get-unread-articles-in-group
		       info (gnus-active (gnus-info-group info))
		       updatep)))))))
    (gnus-message 6 "Checking new news...done")))

(defun gnus-method-rank (type method)
  (cond
   ;; Get info for virtual groups last.
   ((eq (car method) 'nnvirtual)
    200)
   ((eq type 'primary)
    1)
   ;; Compute the rank of the secondary methods based on where they
   ;; are in the secondary select list.
   ((eq type 'secondary)
    (let ((i 2))
      (block nil
	(dolist (smethod gnus-secondary-select-methods)
	  (when (equal method smethod)
	    (return i))
	  (incf i))
	i)))
   ;; Just say that all foreign groups have the same rank.
   (t
    100)))

(defun gnus-read-active-for-groups (method infos early-data)
  (with-current-buffer nntp-server-buffer
    (cond
     ;; Finish up getting the data from the methods that have -early
     ;; methods.
     ((and
       early-data
       (gnus-check-backend-function 'finish-retrieve-group-infos (car method))
       (or (not (gnus-agent-method-p method))
	   (gnus-online method)))
      (gnus-finish-retrieve-group-infos method infos early-data)
      (gnus-agent-save-active method))
     ;; Most backends have -retrieve-groups.
     ((gnus-check-backend-function 'retrieve-groups (car method))
      (when (gnus-check-backend-function 'request-scan (car method))
	(gnus-request-scan nil method))
      (let (groups)
	(gnus-read-active-file-2
	 (dolist (info infos (nreverse groups))
	   (push (gnus-group-real-name (gnus-info-group info)) groups))
	 method)))
     ;; Virtually all backends have -request-list.
     ((gnus-check-backend-function 'request-list (car method))
      (gnus-read-active-file-1 method nil))
     ;; Except nnvirtual and friends, where we request each group, one
     ;; by one.
     (t
      (dolist (info infos)
	(gnus-activate-group (gnus-info-group info) nil nil method t))))))

;; Create a hash table out of the newsrc alist.  The `car's of the
;; alist elements are used as keys.
(defun gnus-make-hashtable-from-newsrc-alist ()
  (let ((alist gnus-newsrc-alist)
	(ohashtb gnus-newsrc-hashtb)
	prev info method rest methods)
    (setq gnus-newsrc-hashtb (gnus-make-hashtable (length alist)))
    (setq alist
	  (setq prev (setq gnus-newsrc-alist
			   (if (equal (caar gnus-newsrc-alist)
				      "dummy.group")
			       gnus-newsrc-alist
			     (cons (list "dummy.group" 0 nil) alist)))))
    (while alist
      (setq info (car alist))
      ;; Make the same select-methods identical Lisp objects.
      (when (setq method (gnus-info-method info))
	(if (setq rest (member method methods))
	    (gnus-info-set-method info (car rest))
	  (push method methods)))
      ;; Check for duplicates.
      (if (gnus-gethash (car info) gnus-newsrc-hashtb)
	  ;; Remove this entry from the alist.
	  (setcdr prev (cddr prev))
	(gnus-sethash
	 (car info)
	 ;; Preserve number of unread articles in groups.
	 (cons (and ohashtb (car (gnus-gethash (car info) ohashtb)))
	       prev)
	 gnus-newsrc-hashtb)
	(setq prev alist))
      (setq alist (cdr alist)))
    ;; Make the same select-methods in `gnus-server-alist' identical
    ;; as well.
    (while methods
      (setq method (pop methods))
      (when (setq rest (rassoc method gnus-server-alist))
	(setcdr rest method)))))

(defun gnus-make-hashtable-from-killed ()
  "Create a hash table from the killed and zombie lists."
  (let ((lists '(gnus-killed-list gnus-zombie-list))
	list)
    (setq gnus-killed-hashtb
	  (gnus-make-hashtable
	   (+ (length gnus-killed-list) (length gnus-zombie-list))))
    (while lists
      (setq list (symbol-value (pop lists)))
      (while list
	(gnus-sethash (car list) (pop list) gnus-killed-hashtb)))))

(defun gnus-parse-active ()
  "Parse active info in the nntp server buffer."
  (with-current-buffer nntp-server-buffer
    (goto-char (point-min))
    ;; Parse the result we got from `gnus-request-group'.
    (when (looking-at "[0-9]+ [0-9]+ \\([0-9]+\\) [0-9]+")
      (goto-char (match-beginning 1))
      (cons (read (current-buffer))
	    (read (current-buffer))))))

(defun gnus-make-articles-unread (group articles)
  "Mark ARTICLES in GROUP as unread."
  (let* ((info (nth 2 (or (gnus-group-entry group)
			  (gnus-group-entry
			   (gnus-group-real-name group)))))
	 (ranges (gnus-info-read info))
	 news article)
    (while articles
      (when (gnus-member-of-range
	     (setq article (pop articles)) ranges)
	(push article news)))
    (when news
      ;; Enter this list into the group info.
      (gnus-info-set-read
       info (gnus-remove-from-range (gnus-info-read info) (nreverse news)))

      ;; Set the number of unread articles in gnus-newsrc-hashtb.
      (gnus-get-unread-articles-in-group info (gnus-active group))

      ;; Insert the change into the group buffer and the dribble file.
      (gnus-group-update-group group t))))

(defun gnus-make-ascending-articles-unread (group articles)
  "Mark ascending ARTICLES in GROUP as unread."
  (let* ((entry (or (gnus-group-entry group)
                    (gnus-group-entry (gnus-group-real-name group))))
         (info (nth 2 entry))
	 (ranges (gnus-info-read info))
         (r ranges)
	 modified)

    (while articles
      (let ((article (pop articles))) ; get the next article to remove from ranges
        (while (let ((range (car ranges))) ; note the current range
                 (if (atom range)       ; single value range
                     (cond ((not range)
                            ;; the articles extend past the end of the ranges
                            ;; OK - I'm done
                            (setq articles nil))
                           ((< range article)
                            ;; this range precedes the article. Leave the range unmodified.
                            (pop ranges)
                            ranges)
                           ((= range article)
                            ;; this range exactly matches the article; REMOVE THE RANGE.
                            ;; NOTE: When the range being removed is the last range, the list is corrupted by inserting null at its end.
                            (setcar ranges (cadr ranges))
                            (setcdr ranges (cddr ranges))
                            (setq modified (if (car ranges) t 'remove-null))
                            nil))
                   (let ((min (car range))
                         (max (cdr range)))
                     ;; I have a min/max range to consider
                     (cond ((> min max) ; invalid range introduced by splitter
                            (setcar ranges (cadr ranges))
                            (setcdr ranges (cddr ranges))
                            (setq modified (if (car ranges) t 'remove-null))
                            ranges)
                           ((= min max)
                            ;; replace min/max range with a single-value range
                            (setcar ranges min)
                            ranges)
                           ((< max article)
                            ;; this range precedes the article. Leave the range unmodified.
                            (pop ranges)
                            ranges)
                           ((< article min)
                            ;; this article precedes the range.  Return null to move to the
                            ;; next article
                            nil)
                           (t
                            ;; this article splits the range into two parts
                            (setcdr ranges (cons (cons (1+ article) max) (cdr ranges)))
                            (setcdr range (1- article))
                            (setq modified t)
                            ranges))))))))

    (when modified
      (when (eq modified 'remove-null)
        (setq r (delq nil r)))
      ;; Enter this list into the group info.
      (gnus-info-set-read info r)

      ;; Set the number of unread articles in gnus-newsrc-hashtb.
      (gnus-get-unread-articles-in-group info (gnus-active group))

      ;; Insert the change into the group buffer and the dribble file.
      (gnus-group-update-group group t))))

;; Enter all dead groups into the hashtb.
(defun gnus-update-active-hashtb-from-killed ()
  (let ((hashtb (setq gnus-active-hashtb (gnus-make-hashtable 4096)))
	(lists (list gnus-killed-list gnus-zombie-list))
	killed)
    (while lists
      (setq killed (car lists))
      (while killed
	(gnus-sethash (mm-string-as-unibyte (car killed)) nil hashtb)
	(setq killed (cdr killed)))
      (setq lists (cdr lists)))))

(defun gnus-get-killed-groups ()
  "Go through the active hashtb and mark all unknown groups as killed."
  ;; First make sure active file has been read.
  (unless (gnus-read-active-file-p)
    (let ((gnus-read-active-file t))
      (gnus-read-active-file)))
  (unless gnus-killed-hashtb
    (gnus-make-hashtable-from-killed))
  ;; Go through all newsgroups that are known to Gnus - enlarge kill list.
  (mapatoms
   (lambda (sym)
     (let ((groups 0)
	   (group (symbol-name sym)))
       (if (or (null group)
	       (gnus-gethash group gnus-killed-hashtb)
	       (gnus-gethash group gnus-newsrc-hashtb))
	   ()
	 (let ((do-sub (gnus-matches-options-n group)))
	   (if (or (eq do-sub 'subscribe) (eq do-sub 'ignore))
	       ()
	     (setq groups (1+ groups))
	     (push group gnus-killed-list)
	     (gnus-sethash group group gnus-killed-hashtb))))))
   gnus-active-hashtb)
  (gnus-dribble-touch))

;; Get the active file(s) from the backend(s).
(defun gnus-read-active-file (&optional force not-native)
  (gnus-group-set-mode-line)
  (let ((methods
	 (mapcar
	  (lambda (m) (if (stringp m) (gnus-server-get-method nil m) m))
	  (append
	   (if (and (not not-native)
		    (gnus-check-server gnus-select-method))
	       ;; The native server is available.
	       (cons gnus-select-method gnus-secondary-select-methods)
	     ;; The native server is down, so we just do the
	     ;; secondary ones.
	     gnus-secondary-select-methods)
	   ;; Also read from the archive server.
	   (when (gnus-archive-server-wanted-p)
	     (list "archive")))))
	method)
    (setq gnus-have-read-active-file nil)
    (with-current-buffer nntp-server-buffer
      (while (setq method (pop methods))
	;; Only do each method once, in case the methods appear more
	;; than once in this list.
	(when (and (not (member method methods))
		   ;; Check whether the backend exists.
		   (ignore-errors (gnus-get-function method 'open-server)))
	  (if (or debug-on-error debug-on-quit)
	      (gnus-read-active-file-1 method force)
	    (condition-case ()
		(gnus-read-active-file-1 method force)
	      ;; We catch C-g so that we can continue past servers
	      ;; that do not respond.
	      (quit
	       (if debug-on-quit
		   (debug "Quit")
		 (message "Quit reading the active file"))
	       nil))))))))

(defun gnus-read-active-file-1 (method force)
  (let (where mesg)
    (setq where (nth 1 method)
	  mesg (format "Reading active file%s via %s..."
		       (if (and where (not (zerop (length where))))
			   (concat " from " where) "")
		       (car method)))
    (gnus-message 5 "%s" mesg)
    (when (gnus-check-server method)
      ;; Request that the backend scan its incoming messages.
      (when (and (or (and gnus-agent
			  (gnus-online method))
		     (not gnus-agent))
		 (gnus-check-backend-function 'request-scan (car method)))
	(gnus-request-scan nil method))
      (cond
       ((and (eq gnus-read-active-file 'some)
	     (gnus-check-backend-function 'retrieve-groups (car method))
	     (not force))
	(let ((newsrc (cdr gnus-newsrc-alist))
	      (gmethod (gnus-server-get-method nil method))
	      groups info)
	  (while (setq info (pop newsrc))
	    (when (inline
		    (gnus-server-equal
			  (inline
			    (gnus-find-method-for-group
				  (gnus-info-group info) info))
			  gmethod))
	      (push (gnus-group-real-name (gnus-info-group info))
		    groups)))
	  (gnus-read-active-file-2 groups method)))
       ((null method)
	t)
       (t
	(if (not (gnus-request-list method))
	    (unless (equal method gnus-message-archive-method)
	      (gnus-error 1 "Cannot read active file from %s server"
			  (car method)))
	  (gnus-message 5 "%s" mesg)
	  (gnus-active-to-gnus-format method gnus-active-hashtb nil t)
	  ;; We mark this active file as read.
	  (add-to-list 'gnus-have-read-active-file method)
	  (gnus-message 5 "%sdone" mesg)))))))

(defun gnus-read-active-file-2 (groups method)
  "Read an active file for GROUPS in METHOD using `gnus-retrieve-groups'."
  (when groups
    (with-current-buffer nntp-server-buffer
      (gnus-check-server method)
      (let ((list-type (gnus-retrieve-groups groups method)))
	(cond ((not list-type)
	       (gnus-error
		1.2 "Cannot read partial active file from %s server."
		(car method)))
	      ((eq list-type 'active)
	       (gnus-active-to-gnus-format method gnus-active-hashtb nil t))
	      (t
	       (gnus-groups-to-gnus-format method gnus-active-hashtb t)))))))

;; Read an active file and place the results in `gnus-active-hashtb'.
(defun gnus-active-to-gnus-format (&optional method hashtb ignore-errors
					     real-active)
  (unless method
    (setq method gnus-select-method))
  (let ((cur (current-buffer))
	(hashtb (or hashtb
		    (if (and gnus-active-hashtb
			     (not (equal method gnus-select-method)))
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (if (equal method gnus-select-method)
				(gnus-make-hashtable
				 (count-lines (point-min) (point-max)))
			      (gnus-make-hashtable 4096))))))
	group max min)
    ;; Delete unnecessary lines.
    (goto-char (point-min))
    (cond
     ((string= gnus-ignored-newsgroups "")
      (delete-matching-lines "^to\\."))
     (t
      (delete-matching-lines (concat "^to\\.\\|" gnus-ignored-newsgroups))))

    (goto-char (point-min))
    (unless (re-search-forward "[\\\"]" nil t)
      ;; Make the group names readable as a lisp expression even if they
      ;; contain special characters.
      (goto-char (point-max))
      (while (re-search-backward "[][';?()#]" nil t)
	(insert ?\\)))

    ;; Let the Gnus agent save the active file.
    (when (and gnus-agent real-active (gnus-online method))
      (gnus-agent-save-active method))

    ;; If these are groups from a foreign select method, we insert the
    ;; group prefix in front of the group names.
    (when (not (gnus-server-equal
		(gnus-server-get-method nil method)
		(gnus-server-get-method nil gnus-select-method)))
      (let ((prefix (gnus-group-prefixed-name "" method)))
	(goto-char (point-min))
	(while (and (not (eobp))
		    (progn
		      (when (= (following-char) ?\")
			(forward-char 1))
		      (insert prefix)
		      (zerop (forward-line 1)))))))
    ;; Store the active file in a hash table.
    ;; Use a unibyte buffer in order to make `read' read non-ASCII
    ;; group names (which have been encoded) as unibyte strings.
    (mm-with-unibyte-buffer
      (insert-buffer-substring cur)
      (setq cur (current-buffer))
      (goto-char (point-min))
      (while (not (eobp))
	(condition-case ()
	    (progn
	      (narrow-to-region (point) (point-at-eol))
	      ;; group gets set to a symbol interned in the hash table
	      ;; (what a hack!!) - jwz
	      (setq group (let ((obarray hashtb)) (read cur)))
	      ;; ### The extended group name scheme makes
	      ;; the previous optimization strategy sort of pointless...
	      (when (stringp group)
		(setq group (intern group hashtb)))
	      (if (and (numberp (setq max (read cur)))
		       (numberp (setq min (read cur)))
		       (progn
			 (skip-chars-forward " \t")
			 (not
			  (or (eq (char-after) ?=)
			      (eq (char-after) ?x)
			      (eq (char-after) ?j)))))
		  (progn
		    (set group (cons min max))
		    ;; if group is moderated, stick in moderation table
		    (when (eq (char-after) ?m)
		      (unless gnus-moderated-hashtb
			(setq gnus-moderated-hashtb (gnus-make-hashtable)))
		      (gnus-sethash (symbol-name group) t
				    gnus-moderated-hashtb)))
		(set group nil)))
	  (error
	   (and group
		(symbolp group)
		(set group nil))
	   (unless ignore-errors
	     (gnus-message 3 "Warning - invalid active: %s"
			   (buffer-substring
			    (point-at-bol) (point-at-eol))))))
	(widen)
	(forward-line 1)))))

(defun gnus-groups-to-gnus-format (method &optional hashtb real-active)
  ;; Parse a "groups" active file.
  (let ((cur (current-buffer))
	(hashtb (or hashtb
		    (if (and method gnus-active-hashtb)
			gnus-active-hashtb
		      (setq gnus-active-hashtb
			    (gnus-make-hashtable
			     (count-lines (point-min) (point-max)))))))
	(prefix (and method
		     (not (gnus-server-equal
			   (gnus-server-get-method nil method)
			   (gnus-server-get-method nil gnus-select-method)))
		     (gnus-group-prefixed-name "" method))))

    ;; Let the Gnus agent save the active file.
    (if (and gnus-agent
	     real-active
	     (gnus-online method)
	     (gnus-agent-method-p method))
	(progn
	  (gnus-agent-save-active method)
	  (gnus-active-to-gnus-format method hashtb nil real-active))

      (goto-char (point-min))
      ;; We split this into to separate loops, one with the prefix
      ;; and one without to speed the reading up somewhat.
      (if prefix
	  (let (min max opoint group)
	    (while (not (eobp))
	      (condition-case ()
		  (progn
		    (read cur) (read cur)
		    (setq min (read cur)
			  max (read cur)
			  opoint (point))
		    (skip-chars-forward " \t")
		    (insert prefix)
		    (goto-char opoint)
		    (set (let ((obarray hashtb)) (read cur))
			 (cons min max)))
		(error (and group (symbolp group) (set group nil))))
	      (forward-line 1)))
	(let (min max group)
	  (while (not (eobp))
	    (condition-case ()
		(when (eq (char-after) ?2)
		  (read cur) (read cur)
		  (setq min (read cur)
			max (read cur))
		  (set (setq group (let ((obarray hashtb)) (read cur)))
		       (cons min max)))
	      (error (and group (symbolp group) (set group nil))))
	    (forward-line 1)))))))

(defun gnus-read-newsrc-file (&optional force)
  "Read startup file.
If FORCE is non-nil, the .newsrc file is read."
  ;; Reset variables that might be defined in the .newsrc.eld file.
  (let ((variables (remove 'gnus-format-specs gnus-variable-list)))
    (while variables
      (set (car variables) nil)
      (setq variables (cdr variables))))
  (let* ((newsrc-file gnus-current-startup-file)
	 (quick-file (concat newsrc-file ".el")))
    (save-excursion
      ;; We always load the .newsrc.eld file.  If always contains
      ;; much information that can not be gotten from the .newsrc
      ;; file (ticked articles, killed groups, foreign methods, etc.)
      (gnus-read-newsrc-el-file quick-file)

      (when (and gnus-read-newsrc-file
		 (file-exists-p gnus-current-startup-file)
		 (or force
		     (and (file-newer-than-file-p newsrc-file quick-file)
			  (file-newer-than-file-p newsrc-file
						  (concat quick-file "d")))
		     (not gnus-newsrc-alist)))
	;; We read the .newsrc file.  Note that if there if a
	;; .newsrc.eld file exists, it has already been read, and
	;; the `gnus-newsrc-hashtb' has been created.  While reading
	;; the .newsrc file, Gnus will only use the information it
	;; can find there for changing the data already read -
	;; i. e., reading the .newsrc file will not trash the data
	;; already read (except for read articles).
	(save-excursion
	  (gnus-message 5 "Reading %s..." newsrc-file)
	  (set-buffer (nnheader-find-file-noselect newsrc-file))
	  (buffer-disable-undo)
	  (gnus-newsrc-to-gnus-format)
	  (kill-buffer (current-buffer))
	  (gnus-message 5 "Reading %s...done" newsrc-file)))

      ;; Convert old to new.
      (gnus-convert-old-newsrc))))

(defun gnus-convert-old-newsrc ()
  "Convert old newsrc formats into the current format, if needed."
  (let ((fcv (and gnus-newsrc-file-version
		  (gnus-continuum-version gnus-newsrc-file-version)))
	(gcv (gnus-continuum-version)))
    (when fcv
      ;; A newsrc file was loaded.
      (let (prompt-displayed
            (converters
             (sort
              (mapcar (lambda (date-func)
                        (cons (gnus-continuum-version (car date-func))
                              date-func))
                      ;; This is a list of converters that must be run
                      ;; to bring the newsrc file up to the current
                      ;; version.  If you create an incompatibility
                      ;; with older versions, you should create an
                      ;; entry here.  The entry should consist of the
                      ;; current gnus version (hardcoded so that it
                      ;; doesn't change with each release) and the
                      ;; function that must be applied to convert the
                      ;; previous version into the current version.
                      '(("September Gnus v0.1" nil
                         gnus-convert-old-ticks)
                        ("Oort Gnus v0.08"     "legacy-gnus-agent"
                         gnus-agent-convert-to-compressed-agentview)
                        ("Gnus v5.10.7"        "legacy-gnus-agent"
                         gnus-agent-unlist-expire-days)
                        ("Gnus v5.10.7"        "legacy-gnus-agent"
                         gnus-agent-unhook-expire-days)))
              #'car-less-than-car)))
        ;; Skip converters older than the file version
        (while (and converters (>= fcv (caar converters)))
          (pop converters))

        ;; Perform converters to bring older version up to date.
	(when (and converters (< fcv (caar converters)))
	  (while (and converters (< fcv (caar converters))
		      (<= (caar converters) gcv))
            (let* ((converter-spec  (pop converters))
                   (convert-to      (nth 1 converter-spec))
                   (load-from       (nth 2 converter-spec))
                   (func            (nth 3 converter-spec)))
              (when (and load-from
                         (not (fboundp func)))
                (load load-from t))
              (or prompt-displayed
                  (not (gnus-convert-converter-needs-prompt func))
                  (while (let (c
                               (cursor-in-echo-area t)
                               (echo-keystrokes 0))
                           (message "Convert gnus from version '%s' to '%s'? (n/y/?)"
                                    gnus-newsrc-file-version gnus-version)
                           (setq c (read-char-exclusive))

                           (cond ((or (eq c ?n) (eq c ?N))
                                  (error "Can not start gnus without converting"))
                                 ((or (eq c ?y) (eq c ?Y))
                                  (setq prompt-displayed t)
                                  nil)
                                 ((eq c ?\?)
                                  (message "This conversion is irreversible. \
 To be safe, you should backup your files before proceeding.")
                                  (sit-for 5)
                                  t)
                                 (t
                                  (gnus-message 3 "Ignoring unexpected input")
                                  (sit-for 3)
                                  t)))))

              (funcall func convert-to)))
          (gnus-dribble-enter
           (format ";Converted gnus from version '%s' to '%s'."
                   gnus-newsrc-file-version gnus-version)))))))

(defun gnus-convert-mark-converter-prompt (converter no-prompt)
  "Indicate whether CONVERTER requires gnus-convert-old-newsrc to
  display the conversion prompt.  NO-PROMPT may be nil (prompt),
  t (no prompt), or any form that can be called as a function.
  The form should return either t or nil."
  (put converter 'gnus-convert-no-prompt no-prompt))

(defun gnus-convert-converter-needs-prompt (converter)
  (let ((no-prompt (get converter 'gnus-convert-no-prompt)))
    (not (if (memq no-prompt '(t nil))
	     no-prompt
	   (funcall no-prompt)))))

(defun gnus-convert-old-ticks (converting-to)
  (let ((newsrc (cdr gnus-newsrc-alist))
	marks info dormant ticked)
    (while (setq info (pop newsrc))
      (when (setq marks (gnus-info-marks info))
	(setq dormant (cdr (assq 'dormant marks))
	      ticked (cdr (assq 'tick marks)))
	(when (or dormant ticked)
	  (gnus-info-set-read
	   info
	   (gnus-add-to-range
	    (gnus-info-read info)
	    (nconc (gnus-uncompress-range dormant)
		   (gnus-uncompress-range ticked)))))))))

(defun gnus-load (file)
  "Load FILE, but in such a way that read errors can be reported."
  (with-temp-buffer
    (insert-file-contents file)
    (while (not (eobp))
      (condition-case type
	  (let ((form (read (current-buffer))))
	    (eval form))
	(error
	 (unless (eq (car type) 'end-of-file)
	   (let ((errmsg (format "Error in %s line %d" file
				 (count-lines (point-min) (point)))))
	     (ding)
	     (unless (gnus-yes-or-no-p (concat errmsg "; continue? "))
	       (error "%s" errmsg)))))))))

(defun gnus-read-newsrc-el-file (file)
  (let ((ding-file (concat file "d")))
    (when (file-exists-p ding-file)
      ;; We always, always read the .eld file.
      (gnus-message 5 "Reading %s..." ding-file)
      (let (gnus-newsrc-assoc)
	(gnus-load ding-file)
	;; Older versions of `gnus-format-specs' are no longer valid
	;; in Oort Gnus 0.01.
	(let ((version
	       (and gnus-newsrc-file-version
		    (gnus-continuum-version gnus-newsrc-file-version))))
	  (when (or (not version)
		    (< version 5.090009))
	    (setq gnus-format-specs gnus-default-format-specs)))
	(when gnus-newsrc-assoc
	  (setq gnus-newsrc-alist gnus-newsrc-assoc))))
    (dolist (elem gnus-newsrc-alist)
      ;; Protect against broken .newsrc.el files.
      (when (car elem)
	(setcar elem (mm-string-as-unibyte (car elem)))))
    (gnus-make-hashtable-from-newsrc-alist)
    (when (file-newer-than-file-p file ding-file)
      ;; Old format quick file
      (gnus-message 5 "Reading %s..." file)
      ;; The .el file is newer than the .eld file, so we read that one
      ;; as well.
      (gnus-read-old-newsrc-el-file file)))
  (gnus-run-hooks 'gnus-read-newsrc-el-hook))

;; Parse the old-style quick startup file
(defun gnus-read-old-newsrc-el-file (file)
  (let (newsrc killed marked group m info)
    (prog1
	(let ((gnus-killed-assoc nil)
	      gnus-marked-assoc gnus-newsrc-alist gnus-newsrc-assoc)
	  (prog1
	      (ignore-errors
		(load file t t t))
	    (setq newsrc gnus-newsrc-assoc
		  killed gnus-killed-assoc
		  marked gnus-marked-assoc)))
      (setq gnus-newsrc-alist nil)
      (while (setq group (pop newsrc))
	(if (setq info (gnus-get-info (car group)))
	    (progn
	      (gnus-info-set-read info (cddr group))
	      (gnus-info-set-level
	       info (if (nth 1 group) gnus-level-default-subscribed
		      gnus-level-default-unsubscribed))
	      (push info gnus-newsrc-alist))
	  (push (setq info
		      (list (car group)
			    (if (nth 1 group) gnus-level-default-subscribed
			      gnus-level-default-unsubscribed)
			    (cddr group)))
		gnus-newsrc-alist))
	;; Copy marks into info.
	(when (setq m (assoc (car group) marked))
	  (unless (nthcdr 3 info)
	    (nconc info (list nil)))
	  (gnus-info-set-marks
	   info (list (cons 'tick (gnus-compress-sequence
				   (sort (cdr m) '<) t))))))
      (setq newsrc killed)
      (while newsrc
	(setcar newsrc (caar newsrc))
	(setq newsrc (cdr newsrc)))
      (setq gnus-killed-list killed))
    ;; The .el file version of this variable does not begin with
    ;; "options", while the .eld version does, so we just add it if it
    ;; isn't there.
    (when
	gnus-newsrc-options
      (when (not (string-match "^ *options" gnus-newsrc-options))
	(setq gnus-newsrc-options (concat "options " gnus-newsrc-options)))
      (when (not (string-match "\n$" gnus-newsrc-options))
	(setq gnus-newsrc-options (concat gnus-newsrc-options "\n")))
      ;; Finally, if we read some options lines, we parse them.
      (unless (string= gnus-newsrc-options "")
	(gnus-newsrc-parse-options gnus-newsrc-options)))

    (setq gnus-newsrc-alist (nreverse gnus-newsrc-alist))
    (gnus-make-hashtable-from-newsrc-alist)))

(defun gnus-make-newsrc-file (file)
  "Make server dependent file name by catenating FILE and server host name."
  (let* ((file (expand-file-name file nil))
	 (real-file (concat file "-" (nth 1 gnus-select-method))))
    (if (or (file-exists-p real-file)
	    (file-exists-p (concat real-file ".el"))
	    (file-exists-p (concat real-file ".eld")))
	real-file
      file)))

(defun gnus-newsrc-to-gnus-format ()
  (setq gnus-newsrc-options "")
  (setq gnus-newsrc-options-n nil)

  (unless gnus-active-hashtb
    (setq gnus-active-hashtb (gnus-make-hashtable 4096)))
  (let ((buf (current-buffer))
	(already-read (> (length gnus-newsrc-alist) 1))
	group subscribed options-symbol newsrc Options-symbol
	symbol reads num1)
    (goto-char (point-min))
    ;; We intern the symbol `options' in the active hashtb so that we
    ;; can `eq' against it later.
    (set (setq options-symbol (intern "options" gnus-active-hashtb)) nil)
    (set (setq Options-symbol (intern "Options" gnus-active-hashtb)) nil)

    (while (not (eobp))
      ;; We first read the first word on the line by narrowing and
      ;; then reading into `gnus-active-hashtb'.  Most groups will
      ;; already exist in that hashtb, so this will save some string
      ;; space.
      (narrow-to-region
       (point)
       (progn (skip-chars-forward "^ \t!:\n") (point)))
      (goto-char (point-min))
      (setq symbol
	    (and (/= (point-min) (point-max))
		 (let ((obarray gnus-active-hashtb)) (read buf))))
      (widen)
      ;; Now, the symbol we have read is either `options' or a group
      ;; name.  If it is an options line, we just add it to a string.
      (cond
       ((or (eq symbol options-symbol)
	    (eq symbol Options-symbol))
	(setq gnus-newsrc-options
	      ;; This concatting is quite inefficient, but since our
	      ;; thorough studies show that approx 99.37% of all
	      ;; .newsrc files only contain a single options line, we
	      ;; don't give a damn, frankly, my dear.
	      (concat gnus-newsrc-options
		      (buffer-substring
		       (point-at-bol)
		       ;; Options may continue on the next line.
		       (or (and (re-search-forward "^[^ \t]" nil 'move)
				(point-at-bol))
			   (point)))))
	(forward-line -1))
       (symbol
	;; Group names can be just numbers.
	(when (numberp symbol)
	  (setq symbol (intern (int-to-string symbol) gnus-active-hashtb)))
	(unless (boundp symbol)
	  (set symbol nil))
	;; It was a group name.
	(setq subscribed (eq (char-after) ?:)
	      group (symbol-name symbol)
	      reads nil)
	(if (eolp)
	    ;; If the line ends here, this is clearly a buggy line, so
	    ;; we put point a the beginning of line and let the cond
	    ;; below do the error handling.
	    (beginning-of-line)
	  ;; We skip to the beginning of the ranges.
	  (skip-chars-forward "!: \t"))
	;; We are now at the beginning of the list of read articles.
	;; We read them range by range.
	(while
	    (cond
	     ((looking-at "[0-9]+")
	      ;; We narrow and read a number instead of buffer-substring/
	      ;; string-to-number because it's faster.  narrow/widen is
	      ;; faster than save-restriction/narrow, and save-restriction
	      ;; produces a garbage object.
	      (setq num1 (progn
			   (narrow-to-region (match-beginning 0) (match-end 0))
			   (read buf)))
	      (widen)
	      ;; If the next character is a dash, then this is a range.
	      (if (eq (char-after) ?-)
		  (progn
		    ;; We read the upper bound of the range.
		    (forward-char 1)
		    (if (not (looking-at "[0-9]+"))
			;; This is a buggy line, by we pretend that
			;; it's kinda OK.  Perhaps the user should be
			;; dinged?
			(push num1 reads)
		      (push
		       (cons num1
			     (progn
			       (narrow-to-region (match-beginning 0)
						 (match-end 0))
			       (read buf)))
		       reads)
		      (widen)))
		;; It was just a simple number, so we add it to the
		;; list of ranges.
		(push num1 reads))
	      ;; If the next char in ?\n, then we have reached the end
	      ;; of the line and return nil.
	      (not (eq (char-after) ?\n)))
	     ((eq (char-after) ?\n)
	      ;; End of line, so we end.
	      nil)
	     (t
	      ;; Not numbers and not eol, so this might be a buggy
	      ;; line...
	      (unless (eobp)
		;; If it was eob instead of ?\n, we allow it.
		;; The line was buggy.
		(setq group nil)
		(gnus-error 3.1 "Mangled line: %s"
			    (buffer-substring (point-at-bol)
					      (point-at-eol))))
	      nil))
	  ;; Skip past ", ".  Spaces are invalid in these ranges, but
	  ;; we allow them, because it's a common mistake to put a
	  ;; space after the comma.
	  (skip-chars-forward ", "))

	;; We have already read .newsrc.eld, so we gently update the
	;; data in the hash table with the information we have just
	;; read.
	(when group
	  (let ((info (gnus-get-info group))
		level)
	    (if info
		;; There is an entry for this file in the alist.
		(progn
		  (gnus-info-set-read info (nreverse reads))
		  ;; We update the level very gently.  In fact, we
		  ;; only change it if there's been a status change
		  ;; from subscribed to unsubscribed, or vice versa.
		  (setq level (gnus-info-level info))
		  (cond ((and (<= level gnus-level-subscribed)
			      (not subscribed))
			 (setq level (if reads
					 gnus-level-default-unsubscribed
				       (1+ gnus-level-default-unsubscribed))))
			((and (> level gnus-level-subscribed) subscribed)
			 (setq level gnus-level-default-subscribed)))
		  (gnus-info-set-level info level))
	      ;; This is a new group.
	      (setq info (list group
			       (if subscribed
				   gnus-level-default-subscribed
				 (if reads
				     (1+ gnus-level-subscribed)
				   gnus-level-default-unsubscribed))
			       (nreverse reads))))
	    (push info newsrc)))))
      (forward-line 1))

    (setq newsrc (nreverse newsrc))

    (if (not already-read)
	()
      ;; We now have two newsrc lists - `newsrc', which is what we
      ;; have read from .newsrc, and `gnus-newsrc-alist', which is
      ;; what we've read from .newsrc.eld.  We have to merge these
      ;; lists.  We do this by "attaching" any (foreign) groups in the
      ;; gnus-newsrc-alist to the (native) group that precedes them.
      (let ((rc (cdr gnus-newsrc-alist))
	    (prev gnus-newsrc-alist)
	    entry mentry)
	(while rc
	  (or (null (nth 4 (car rc)))	; It's a native group.
	      (assoc (caar rc) newsrc)	; It's already in the alist.
	      (if (setq entry (assoc (caar prev) newsrc))
		  (setcdr (setq mentry (memq entry newsrc))
			  (cons (car rc) (cdr mentry)))
		(push (car rc) newsrc)))
	  (setq prev rc
		rc (cdr rc)))))

    (setq gnus-newsrc-alist newsrc)
    ;; We make the newsrc hashtb.
    (gnus-make-hashtable-from-newsrc-alist)

    ;; Finally, if we read some options lines, we parse them.
    (unless (string= gnus-newsrc-options "")
      (gnus-newsrc-parse-options gnus-newsrc-options))))

;; Parse options lines to find "options -n !all rec.all" and stuff.
;; The return value will be a list on the form
;; ((regexp1 . ignore)
;;  (regexp2 . subscribe)...)
;; When handling new newsgroups, groups that match a `ignore' regexp
;; will be ignored, and groups that match a `subscribe' regexp will be
;; subscribed.  A line like
;; options -n !all rec.all
;; will lead to a list that looks like
;; (("^rec\\..+" . subscribe)
;;  ("^.+" . ignore))
;; So all "rec.*" groups will be subscribed, while all the other
;; groups will be ignored.  Note that "options -n !all rec.all" is very
;; different from "options -n rec.all !all".
(defun gnus-newsrc-parse-options (options)
  (let (out eol)
    (save-excursion
      (gnus-set-work-buffer)
      (insert (regexp-quote options))
      ;; First we treat all continuation lines.
      (goto-char (point-min))
      (while (re-search-forward "\n[ \t]+" nil t)
	(replace-match " " t t))
      ;; Then we transform all "all"s into ".+"s.
      (goto-char (point-min))
      (while (re-search-forward "\\ball\\b" nil t)
	(replace-match ".+" t t))
      (goto-char (point-min))
      ;; We remove all other options than the "-n" ones.
      (while (re-search-forward "[ \t]-[^n][^-]*" nil t)
	(replace-match " ")
	(forward-char -1))
      (goto-char (point-min))

      ;; We are only interested in "options -n" lines - we
      ;; ignore the other option lines.
      (while (re-search-forward "[ \t]-n" nil t)
	(setq eol
	      (or (save-excursion
		    (and (re-search-forward "[ \t]-n" (point-at-eol) t)
			 (- (point) 2)))
		  (point-at-eol)))
	;; Search for all "words"...
	(while (re-search-forward "[^ \t,\n]+" eol t)
	  (if (eq (char-after (match-beginning 0)) ?!)
	      ;; If the word begins with a bang (!), this is a "not"
	      ;; spec.  We put this spec (minus the bang) and the
	      ;; symbol `ignore' into the list.
	      (push (cons (concat
			   "^" (buffer-substring
				(1+ (match-beginning 0))
				(match-end 0))
			   "\\($\\|\\.\\)")
			  'ignore)
		    out)
	    ;; There was no bang, so this is a "yes" spec.
	    (push (cons (concat "^" (match-string 0) "\\($\\|\\.\\)")
			'subscribe)
		  out))))

      (setq gnus-newsrc-options-n out))))

(eval-and-compile
  (defalias 'gnus-long-file-names
    (if (fboundp 'msdos-long-file-names)
      'msdos-long-file-names
      (lambda () t))))

(defun gnus-save-newsrc-file (&optional force)
  "Save .newsrc file."
  ;; Note: We cannot save .newsrc file if all newsgroups are removed
  ;; from the variable gnus-newsrc-alist.
  (when (and (or gnus-newsrc-alist gnus-killed-list)
	     gnus-current-startup-file)
    ;; Save agent range limits for the currently active method.
    (when gnus-agent
      (gnus-agent-save-local force))

    (save-excursion
      (if (and (or gnus-use-dribble-file gnus-slave)
	       (not force)
	       (or (not gnus-dribble-buffer)
		   (not (buffer-name gnus-dribble-buffer))
		   (zerop (with-current-buffer gnus-dribble-buffer
			    (buffer-size)))))
	  (gnus-message 4 "(No changes need to be saved)")
	(gnus-run-hooks 'gnus-save-newsrc-hook)
	(if gnus-slave
	    (gnus-slave-save-newsrc)
	  ;; Save .newsrc.
	  (when gnus-save-newsrc-file
	    (gnus-message 8 "Saving %s..." gnus-current-startup-file)
	    (gnus-gnus-to-newsrc-format)
	    (gnus-message 8 "Saving %s...done" gnus-current-startup-file))

	  ;; Save .newsrc.eld.
	  (set-buffer (gnus-get-buffer-create " *Gnus-newsrc*"))
	  (make-local-variable 'version-control)
	  (setq version-control gnus-backup-startup-file)
	  (setq buffer-file-name
		(concat gnus-current-startup-file ".eld"))
	  (setq default-directory (file-name-directory buffer-file-name))
	  (buffer-disable-undo)
	  (erase-buffer)
          (gnus-message 5 "Saving %s.eld..." gnus-current-startup-file)

          (if gnus-save-startup-file-via-temp-buffer
              (let ((coding-system-for-write gnus-ding-file-coding-system)
                    (standard-output (current-buffer)))
                (gnus-gnus-to-quick-newsrc-format)
                (gnus-run-hooks 'gnus-save-quick-newsrc-hook)
                (save-buffer))
            (let ((coding-system-for-write gnus-ding-file-coding-system)
                  (version-control gnus-backup-startup-file)
                  (startup-file (concat gnus-current-startup-file ".eld"))
                  (working-dir (file-name-directory gnus-current-startup-file))
                  working-file
                  (i -1))
              ;; Generate the name of a non-existent file.
              (while (progn (setq working-file
                                  (format
                                   (if (and (eq system-type 'ms-dos)
                                            (not (gnus-long-file-names)))
                                       "%s#%d.tm#" ; MSDOS limits files to 8+3
				     "%s#tmp#%d")
                                   working-dir (setq i (1+ i))))
                            (file-exists-p working-file)))

              (unwind-protect
                  (progn
                    (gnus-with-output-to-file working-file
		      (gnus-gnus-to-quick-newsrc-format)
		      (gnus-run-hooks 'gnus-save-quick-newsrc-hook))

                    ;; These bindings will mislead the current buffer
                    ;; into thinking that it is visiting the startup
                    ;; file.
                    (let ((buffer-backed-up nil)
                          (buffer-file-name startup-file)
                          (file-precious-flag t)
                          (setmodes (file-modes startup-file)))
                      ;; Backup the current version of the startup file.
                      (backup-buffer)

                      ;; Replace the existing startup file with the temp file.
                      (rename-file working-file startup-file t)
                      (gnus-set-file-modes startup-file setmodes)))
                (condition-case nil
                    (delete-file working-file)
                  (file-error nil)))))

	  (gnus-kill-buffer (current-buffer))
	  (gnus-message
	   5 "Saving %s.eld...done" gnus-current-startup-file))
	(gnus-dribble-delete-file)
	(gnus-group-set-mode-line)))))

(defun gnus-gnus-to-quick-newsrc-format (&optional minimal name &rest specific-variables)
  "Print Gnus variables such as `gnus-newsrc-alist' in Lisp format."
    (princ (format ";; -*- mode:emacs-lisp; coding: %s; -*-\n"
		   gnus-ding-file-coding-system))
    (if name
	(princ (format ";; %s\n" name))
      (princ ";; Gnus startup file.\n"))

    (unless minimal
      (princ "\
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.\n")
      (princ "(setq gnus-newsrc-file-version ")
      (princ (gnus-prin1-to-string gnus-version))
      (princ ")\n"))

    (let* ((print-quoted t)
           (print-readably t)
           (print-escape-multibyte nil)
           (print-escape-nonascii t)
           (print-length nil)
           (print-level nil)
	   (print-circle nil)
           (print-escape-newlines t)
	   (gnus-killed-list
	    (if (and gnus-save-killed-list
		     (stringp gnus-save-killed-list))
		(gnus-strip-killed-list)
	      gnus-killed-list))
	   (variables
	    (or specific-variables
		(if gnus-save-killed-list gnus-variable-list
		  ;; Remove the `gnus-killed-list' from the list of variables
		  ;; to be saved, if required.
		  (delq 'gnus-killed-list (copy-sequence gnus-variable-list)))))
	   ;; Peel off the "dummy" group.
	   (gnus-newsrc-alist (cdr gnus-newsrc-alist))
	   variable)
      ;; Insert the variables into the file.
      (while variables
	(when (and (boundp (setq variable (pop variables)))
		   (symbol-value variable))
	  (princ "\n(setq ")
          (princ (symbol-name variable))
          (princ " '")
	  (prin1 (symbol-value variable))
	  (princ ")\n")))))

(defun gnus-strip-killed-list ()
  "Return the killed list minus the groups that match `gnus-save-killed-list'."
  (let ((list gnus-killed-list)
	olist)
    (while list
      (when (string-match gnus-save-killed-list (car list))
	(push (car list) olist))
      (pop list))
    (nreverse olist)))

(defun gnus-gnus-to-newsrc-format (&optional foreign-ok)
  (interactive (list (gnus-y-or-n-p "write foreign groups too? ")))
  ;; Generate and save the .newsrc file.
  (with-current-buffer (create-file-buffer gnus-current-startup-file)
    (let ((newsrc (cdr gnus-newsrc-alist))
	  (standard-output (current-buffer))
	  info ranges range method)
      (setq buffer-file-name gnus-current-startup-file)
      (setq default-directory (file-name-directory buffer-file-name))
      (buffer-disable-undo)
      (erase-buffer)
      ;; Use a unibyte buffer since group names are unibyte strings;
      ;; in particular, non-ASCII group names are the ones encoded by
      ;; a certain coding system.
      (mm-disable-multibyte)
      ;; Write options.
      (when gnus-newsrc-options
	(insert gnus-newsrc-options))
      ;; Write subscribed and unsubscribed.
      (while (setq info (pop newsrc))
	;; Don't write foreign groups to .newsrc.
	(when (or (null (setq method (gnus-info-method info)))
		  (equal method "native")
		  (inline (gnus-server-equal method gnus-select-method))
                  foreign-ok)
	  (insert (gnus-info-group info)
		  (if (> (gnus-info-level info) gnus-level-subscribed)
		      "!" ":"))
	  (when (setq ranges (gnus-info-read info))
	    (insert " ")
	    (if (not (listp (cdr ranges)))
		(if (= (car ranges) (cdr ranges))
		    (princ (car ranges))
		  (princ (car ranges))
		  (insert "-")
		  (princ (cdr ranges)))
	      (while (setq range (pop ranges))
		(if (or (atom range) (= (car range) (cdr range)))
		    (princ (or (and (atom range) range) (car range)))
		  (princ (car range))
		  (insert "-")
		  (princ (cdr range)))
		(when ranges
		  (insert ",")))))
	  (insert "\n")))
      (make-local-variable 'version-control)
      (setq version-control 'never)
      ;; It has been reported that sometime the modtime on the .newsrc
      ;; file seems to be off.  We really do want to overwrite it, so
      ;; we clear the modtime here before saving.  It's a bit odd,
      ;; though...
      ;; sometimes the modtime clear isn't sufficient.  most brute force:
      ;; delete the silly thing entirely first.  but this fails to provide
      ;; such niceties as .newsrc~ creation.
      (if gnus-modtime-botch
	  (delete-file gnus-startup-file)
	(clear-visited-file-modtime))
      (gnus-run-hooks 'gnus-save-standard-newsrc-hook)
      (let ((coding-system-for-write 'raw-text))
	(save-buffer))
      (kill-buffer (current-buffer)))))


;;;
;;; Slave functions.
;;;

(defvar gnus-slave-mode nil)

(defun gnus-slave-mode ()
  "Minor mode for slave Gnusae."
  ;; FIXME: gnus-slave-mode appears to never be set (i.e. it'll always be nil):
  ;; Remove, or fix and use define-minor-mode.
  (add-minor-mode 'gnus-slave-mode " Slave" (make-sparse-keymap))
  (gnus-run-hooks 'gnus-slave-mode-hook))

(defun gnus-slave-save-newsrc ()
  (with-current-buffer gnus-dribble-buffer
    (let ((slave-name
	   (mm-make-temp-file (concat gnus-current-startup-file "-slave-")))
	  (modes (ignore-errors
		   (file-modes (concat gnus-current-startup-file ".eld")))))
      (let ((coding-system-for-write gnus-ding-file-coding-system))
	(gnus-write-buffer slave-name))
      (when modes
	(gnus-set-file-modes slave-name modes)))))

(defun gnus-master-read-slave-newsrc ()
  (let ((slave-files
	 (directory-files
	  (file-name-directory gnus-current-startup-file)
	  t (concat
	     "^" (regexp-quote
		  (concat
		   (file-name-nondirectory gnus-current-startup-file)
		   "-slave-")))
	  t))
	file)
    (if (not slave-files)
	()				; There are no slave files to read.
      (gnus-message 7 "Reading slave newsrcs...")
      (with-current-buffer (gnus-get-buffer-create " *gnus slave*")
	(setq slave-files
	      (sort (mapcar (lambda (file)
			      (list (nth 5 (file-attributes file)) file))
			    slave-files)
		    (lambda (f1 f2)
		      (or (< (caar f1) (caar f2))
			  (< (nth 1 (car f1)) (nth 1 (car f2)))))))
	(while slave-files
	  (erase-buffer)
	  (setq file (nth 1 (car slave-files)))
	  (nnheader-insert-file-contents file)
	  (when (condition-case ()
		    (progn
		      (eval-buffer (current-buffer))
		      t)
		  (error
		   (gnus-error 3.2 "Possible error in %s" file)
		   nil))
	    (unless gnus-slave		; Slaves shouldn't delete these files.
	      (ignore-errors
		(delete-file file))))
	  (setq slave-files (cdr slave-files))))
      (gnus-dribble-touch)
      (gnus-message 7 "Reading slave newsrcs...done"))))


;;;
;;; Group description.
;;;

(defun gnus-read-all-descriptions-files ()
  (let ((methods (cons gnus-select-method
		       (nconc
			(when (gnus-archive-server-wanted-p)
			  (list "archive"))
			gnus-secondary-select-methods))))
    (while methods
      (gnus-read-descriptions-file (car methods))
      (setq methods (cdr methods)))
    t))

(defun gnus-read-descriptions-file (&optional method)
  (let ((method (or method gnus-select-method))
	group)
    (when (stringp method)
      (setq method (gnus-server-to-method method)))
    ;; We create the hashtable whether we manage to read the desc file
    ;; to avoid trying to re-read after a failed read.
    (unless gnus-description-hashtb
      (setq gnus-description-hashtb
	    (gnus-make-hashtable (length gnus-active-hashtb))))
    ;; Mark this method's desc file as read.
    (gnus-sethash (gnus-group-prefixed-name "" method) "Has read"
		  gnus-description-hashtb)

    (gnus-message 5 "Reading descriptions file via %s..." (car method))
    (cond
     ((null (gnus-get-function method 'request-list-newsgroups t))
      t)
     ((not (gnus-check-server method))
      (gnus-message 1 "Couldn't open server")
      nil)
     ((not (gnus-request-list-newsgroups method))
      (gnus-message 1 "Couldn't read newsgroups descriptions")
      nil)
     (t
      (save-excursion
        ;; FIXME: Shouldn't save-restriction be done after set-buffer?
	(save-restriction
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (when (or (search-forward "\n.\n" nil t)
		    (goto-char (point-max)))
	    (beginning-of-line)
	    (narrow-to-region (point-min) (point)))
	  ;; If these are groups from a foreign select method, we insert the
	  ;; group prefix in front of the group names.
	  (and method (not (inline
			     (gnus-server-equal
			      (gnus-server-get-method nil method)
			      (gnus-server-get-method
			       nil gnus-select-method))))
	       (let ((prefix (gnus-group-prefixed-name "" method)))
		 (goto-char (point-min))
		 (while (and (not (eobp))
			     (progn (insert prefix)
				    (zerop (forward-line 1)))))))
	  (goto-char (point-min))
	  (while (not (eobp))
	    ;; If we get an error, we set group to 0, which is not a
	    ;; symbol...
	    (setq group
		  (condition-case ()
		      (let ((obarray gnus-description-hashtb))
			;; Group is set to a symbol interned in this
			;; hash table.
			(read nntp-server-buffer))
		    (error 0)))
	    (skip-chars-forward " \t")
	    ;; ...  which leads to this line being effectively ignored.
	    (when (symbolp group)
	      (let* ((str (buffer-substring
			   (point) (progn (end-of-line) (point))))
		     (name (symbol-name group))
		     (charset
		      (or (gnus-group-name-charset method name)
			  (gnus-parameter-charset name)
			  gnus-default-charset)))
		;; Fixme: Don't decode in unibyte mode.
		(when (and str charset (featurep 'mule))
		  (setq str (mm-decode-coding-string str charset)))
		(set group str)))
	    (forward-line 1))))
      (gnus-message 5 "Reading descriptions file...done")
      t))))

(defun gnus-group-get-description (group)
  "Get the description of a group by sending XGTITLE to the server."
  (when (gnus-request-group-description group)
    (with-current-buffer nntp-server-buffer
      (goto-char (point-min))
      (when (looking-at "[^ \t]+[ \t]+\\(.*\\)")
	(match-string 1)))))

;;;###autoload
(defun gnus-declare-backend (name &rest abilities)
  "Declare back end NAME with ABILITIES as a Gnus back end."
  (setq gnus-valid-select-methods
	(nconc gnus-valid-select-methods
	       (list (apply 'list name abilities))))
  (gnus-redefine-select-method-widget))

(defun gnus-set-default-directory ()
  "Set the default directory in the current buffer to `gnus-default-directory'.
If this variable is nil, don't do anything."
  (setq default-directory
	(if (and gnus-default-directory
		 (file-exists-p gnus-default-directory))
	    (file-name-as-directory (expand-file-name gnus-default-directory))
	  default-directory)))

(defun gnus-display-time-event-handler ()
  (if (and (fboundp 'display-time-event-handler)
	   (gnus-boundp 'display-time-timer))
      (display-time-event-handler)))

(defun gnus-check-reasonable-setup ()
  ;; Check whether nnml and nnfolder share a directory.
  (let ((display-warn
	 (if (fboundp 'display-warning)
	     'display-warning
	   (lambda (type message)
	     (if noninteractive
		 (message "Warning (%s): %s" type message)
	       (let (window)
		 (with-current-buffer (get-buffer-create "*Warnings*")
		   (goto-char (point-max))
		   (unless (bolp)
		     (insert "\n"))
		   (insert (format "Warning (%s): %s\n" type message))
		   (setq window (display-buffer (current-buffer)))
		   (set-window-start
		    window
		    (prog2
			(forward-line (- 1 (window-height window)))
			(point)
		      (goto-char (point-max))))))))))
	method active actives match)
    (dolist (server gnus-server-alist)
      (setq method (gnus-server-to-method server)
	    active (intern (format "%s-active-file" (car method))))
      (when (and (member (car method) '(nnml nnfolder))
		 (gnus-server-opened method)
		 (boundp active))
	(when (setq match (assoc (symbol-value active) actives))
	  (funcall display-warn 'gnus-server
		   (format "%s and %s share the same active file %s"
			   (car method)
			   (cadr match)
			   (car match))))
	(push (list (symbol-value active) method) actives)))))

(provide 'gnus-start)

;;; gnus-start.el ends here
