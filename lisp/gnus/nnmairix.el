;;; nnmairix.el --- Mairix back end for Gnus, the Emacs newsreader

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: David Engster <dengste@eml.cc>
;; Keywords: mail searching
;; Version: 0.6

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

;; This is a back end for using the mairix search engine with
;; Gnus.  Mairix is a tool for searching words in locally stored
;; mail.  Mairix is very fast which allows using it efficiently for
;; "smart folders", e.g. folders which are associated with search
;; queries.  Of course, you can also use this back end just for
;; calling mairix with some search query.
;;
;; Mairix is written by Richard Curnow.  More information can be found at
;; http://www.rpcurnow.force9.co.uk/mairix/

;; Commentary on the code: nnmairix sits between Gnus and the "real"
;; back end which handles the mail (currently nnml, nnimap and
;; nnmaildir were tested). I know this is all a bit hacky, but so far
;; it works for me.  This is the first back end I've written for Gnus,
;; so I'd appreciate any comments, suggestions, bug reports (and, of
;; course, patches) for improving nnmairix.

;; nnmairix does not use an active file, since I wanted to contain the
;; back end "inside Gnus" as much as possible without the need of an
;; external file.  It stores the query/folder information in the group
;; parameters instead.  This also implies that once you kill a mairix
;; group, it's gone for good.  I don't think that this is really
;; problematic, since I don't see the need in unsubscribing and
;; re-subscribing search groups

;; Every mairix server is "responsible" for one mairix installation,
;; i.e. you can have several mairix servers for different mairix
;; configurations.  Not that I think anyone will actually do this, but
;; I thought it would be a "nice to have feature"...

;; KNOWN BUGS:
;; * Mairix does only support us-ascii characters.

;; TODO/MISSING FEATURES:
;; * Support of more back ends (nnmh, nnfolder, nnmbox...)?
;; * Maybe use an active file instead of group parameters?
;; * Maybe use "-a" when updating groups which are not newly created?

;;; Changelog:
;; 05/30/2008 - version 0.6
;;
;;    * It is now possible to propagate marks from the nnmairix groups
;;      to the original messages (and for maildir also vice versa). See
;;      the docs for details on this feature - it's pretty delicate
;;      and currently needs a patched mairix binary to work smoothly.
;;
;;    * Keep messages in nnmairix groups always read/unread
;;      (bound to 'G b r').
;;
;;    * Recreate back end folder for nnmairix groups in case you
;;      somehow get wrong article counts (bound to 'G b d').
;;
;;    * New group parameter 'allow-fast'. Toggling of parameter bound
;;      to 'G b a'. The default is nil, meaning that the group will
;;      always be updated with a mairix search, even when only entered.
;;
;;    * More/Better use of the registry (if available). Can now also
;;      deal with duplicate messages in different groups.
;;
;; 02/06/2008 - version 0.5
;;
;;    * New function: nnmairix-goto-original-article. Uses the
;;      registry or the mail file path for determining original group.
;;
;;    * Deal with empty Xref header
;;
;;    * Changed summary mode keybindings since the old ones were
;;      already taken
;;
;;   (Thanks to Tassilo Horn and Ted Zlatanov for their help)
;;
;; 01/07/2008 - version 0.4
;;
;;    * New/fixed doc strings and code cleanup.
;;
;; 11/18/2007 - version 0.3
;;
;;    * Fixed bugs when dealing with nnml and native servers
;;
;;    * Make variables customizable
;;
;; 10/10/2007 - version 0.2
;;
;;    * Use nnml-directory/directory server variables for nnml and
;;    nnmaildir back ends as path for search folders. This way it
;;    becomes independent of 'base' setting in .mairixirc (but not for
;;    nnimap).
;;
;;    * As a result: Changed nnmairix-backend-to-server so that user
;;    is asked when more than one nnmairix server exists and we do not
;;    know which one is responsible for current back end.
;;
;;    * Rename files when using nnml back ends so that there are no
;;    holes in article numbers. This should fix all problems regarding
;;    wrong article counts with nnml.
;;
;;    * More commands for creating queries (using widgets or the
;;    minibuffer).
;;
;;    * Fixed bug in nnmairix-create-search-group-from-message
;;
;;    * Changed copyright to FSF
;;
;;      (Thanks to Georg C. F. Greve and Bastien for suggestions and
;;      ideas!)
;;
;; 10/03/2007 - version 0.1 - first release


;;; Code:

(eval-when-compile (require 'cl))       ;For (pop (cdr ogroup)).

(require 'nnoo)
(require 'gnus-group)
(require 'gnus-sum)
(require 'message)
(require 'nnml)
(require 'widget)

(nnoo-declare nnmairix)

;;; === Keymaps

(eval-when-compile
  (when (featurep 'xemacs)
    ;; The `kbd' macro requires that the `read-kbd-macro' macro is available.
    (require 'edmacro)))

;; Group mode
(defun nnmairix-group-mode-hook ()
  "Nnmairix group mode keymap."
  (define-key gnus-group-mode-map
    (kbd "G b") (make-sparse-keymap))
  (define-key gnus-group-mode-map
    (kbd "G b g") 'nnmairix-create-search-group)
  (define-key gnus-group-mode-map
    (kbd "G b c") 'nnmairix-create-server-and-default-group)
  (define-key gnus-group-mode-map
    (kbd "G b q") 'nnmairix-group-change-query-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b t") 'nnmairix-group-toggle-threads-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b u") 'nnmairix-update-database)
  (define-key gnus-group-mode-map
    (kbd "G b s") 'nnmairix-search)
  (define-key gnus-group-mode-map
    (kbd "G b i") 'nnmairix-search-interactive)
  (define-key gnus-group-mode-map
    (kbd "G b m") 'nnmairix-widget-search)
  (define-key gnus-group-mode-map
    (kbd "G b p") 'nnmairix-group-toggle-propmarks-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b r") 'nnmairix-group-toggle-readmarks-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b d") 'nnmairix-group-delete-recreate-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b a") 'nnmairix-group-toggle-allowfast-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b o") 'nnmairix-propagate-marks))

;; Summary mode
(defun nnmairix-summary-mode-hook ()
  "Nnmairix summary mode keymap."
  (define-key gnus-summary-mode-map
    (kbd "G G t") 'nnmairix-search-thread-this-article)
  (define-key gnus-summary-mode-map
    (kbd "G G f") 'nnmairix-search-from-this-article)
  (define-key gnus-summary-mode-map
    (kbd "G G m") 'nnmairix-widget-search-from-this-article)
  (define-key gnus-summary-mode-map
    (kbd "G G g") 'nnmairix-create-search-group-from-message)
  (define-key gnus-summary-mode-map
    (kbd "G G o") 'nnmairix-goto-original-article)
  (define-key gnus-summary-mode-map
    (kbd "G G u") 'nnmairix-remove-tick-mark-original-article))

(add-hook 'gnus-group-mode-hook 'nnmairix-group-mode-hook)
(add-hook 'gnus-summary-mode-hook 'nnmairix-summary-mode-hook)

;; ;;;###autoload
;; (defun nnmairix-initialize (&optional force)
;;   (interactive "P")
;;   (if (not (or (file-readable-p "~/.mairixrc")
;; 	       force))
;;       (message "No file `~/.mairixrc', skipping nnmairix setup")
;;     (add-hook 'gnus-group-mode-hook 'nnmairix-group-mode-hook)
;;     (add-hook 'gnus-summary-mode-hook 'nnmairix-summary-mode-hook)))

;; Customizable stuff

(defgroup nnmairix nil
  "Back end for the Mairix mail search engine."
  :group 'gnus)

(defcustom nnmairix-group-prefix "zz_mairix"
  "Prefix for mairix search groups on back end server.
nnmairix will create these groups automatically on the back end
server for each nnmairix search group.  The name on the back end
server will be this prefix plus a random number.  You can delete
unused nnmairix groups on the back end using
`nnmairix-purge-old-groups'."
  :version "23.1"
  :type 'string
  :group 'nnmairix)

(defcustom nnmairix-mairix-output-buffer "*mairix output*"
  "Buffer used for mairix output."
  :version "23.1"
  :type 'string
  :group 'nnmairix)

(defcustom nnmairix-customize-query-buffer "*mairix query*"
  "Name of the buffer for customizing Mairix queries."
  :version "23.1"
  :type 'string
  :group 'nnmairix)

(defcustom nnmairix-mairix-update-options '("-F" "-Q")
  "Options when calling mairix for updating the database.
The default is '-F' and '-Q' for making updates faster.  You
should call mairix without these options from time to
time (e.g. via cron job)."
  :version "23.1"
  :type '(repeat string)
  :group 'nnmairix)

(defcustom nnmairix-mairix-search-options '("-Q")
  "Options when calling mairix for searching.
The default is '-Q' for making searching faster."
  :version "23.1"
  :type '(repeat string)
  :group 'nnmairix)

(defcustom nnmairix-mairix-synchronous-update nil
  "Set this to t if you want Emacs to wait for mairix updating the database."
  :version "23.1"
  :type 'boolean
  :group 'nnmairix)

(defcustom nnmairix-rename-files-for-nnml t
  "Rename nnml mail files so that they are consecutively numbered.
When using nnml as back end, mairix might produce holes in the
article numbers which will produce wrong article counts by
Gnus.  This option controls whether nnmairix should rename the
files consecutively."
  :version "23.1"
  :type 'boolean
  :group 'nnmairix)

(defcustom nnmairix-widget-fields-list
  '(("from" "f" "From") ("to" "t" "To") ("cc" "c" "Cc")
    ("subject" "s" "Subject")  ("to" "tc" "To or Cc")
    ("from" "a" "Address") (nil "b" "Body") (nil "n" "Attachment")
    ("Message-ID" "m" "Message ID") (nil "s" "Size") (nil "d" "Date"))
  "Fields that should be editable during interactive query customization.

Header, corresponding mairix command and description for editable
fields in interactive query customization.  The header specifies
which header contents should be inserted into the editable field
when creating a Mairix query based on the current message (can be
nil for disabling this)."
  :version "23.1"
  :type '(repeat (list
		  (choice :tag "Field"
			  (const :tag "none" nil)
			  (const :tag "From" "from")
			  (const :tag "To" "to")
			  (const :tag "Cc" "cc")
			  (const :tag "Subject" "subject")
			  (const :tag "Message ID" "Message-ID"))
		  (string :tag "Command")
		  (string :tag "Description")))
  :group 'nnmairix)

(defcustom nnmairix-widget-select-window-function
  (lambda () (select-window (get-largest-window)))
  "Function for selecting the window for customizing the mairix query.
The default chooses the largest window in the current frame."
  :version "23.1"
  :type 'function
  :group 'nnmairix)

(defcustom nnmairix-propagate-marks-upon-close t
  "Flag if marks should be propagated upon closing a group.
The default of this variable is t. If set to 'ask, the
user will be asked if the flags should be propagated when the
group is closed.  If set to nil, the user will have to manually
call 'nnmairix-propagate-marks'."
  :version "23.1"
  :type '(choice (const :tag "always" t)
		 (const :tag "ask" 'ask)
		 (const :tag "never" nil))
  :group 'nnmairix)

(defcustom nnmairix-propagate-marks-to-nnmairix-groups nil
  "Flag if marks from original articles should be seen in nnmairix groups.
The default is nil since it will only work if the articles are in
maildir format and NOT managed by the nnmaildir back end but
e.g. an IMAP server (which stores the marks in the maildir file
name).  You may safely set this to t for testing - the worst that
can happen are wrong marks in nnmairix groups."
  :version "23.1"
  :type 'boolean
  :group 'nnmairix)

(defcustom nnmairix-only-use-registry nil
  "Use only the registry for determining original group(s).
If set to t, nnmairix will only use the registry for determining
the original group(s) of an article (which is also necessary for
propagating marks).  If set to nil, it will also try to determine
the group from an additional mairix search which might be slow
when propagating lots of marks."
  :version "23.1"
  :type 'boolean
  :group 'nnmairix)

(defcustom nnmairix-allowfast-default nil
  "Whether fast entering should be the default for nnmairix groups.
You may set this to t to make entering the group faster, but note that
this might lead to problems, especially when used with marks propagation."
  :version "23.1"
  :type 'boolean
  :group 'nnmairix)

;; ==== Other variables

(defvar nnmairix-widget-other
  '(threads flags)
  "Other editable mairix commands when using customization widgets.
Currently there are 'threads and 'flags.")

(defvar nnmairix-interactive-query-parameters
  '((?f "from" "f" "From") (?t "to" "t" "To") (?c "to" "tc" "To or Cc")
    (?a "from" "a" "Address") (?s "subject" "s" "Subject") (?b nil "b" "Body")
    (?d nil "d" "Date") (?n nil "n" "Attachment"))
  "Things that should be editable during interactive query generation.
Every list element consists of the following entries: Keystroke,
message field (if any), mairix command and description.")

(defvar nnmairix-delete-and-create-on-change '(nnimap nnmaildir nnml)
  "Controls on which back ends groups should be deleted and re-created.
This variable is a list of back ends where the search group
should be completely deleted and re-created when the query or
thread parameter changes.  The default is to this for all
currently supported back ends.  It usually also corrects the
problem of \"holes\" in the article numbers which often lead to a
wrong count of total articles shown by Gnus.")

;;; === Server variables

(defvoo nnmairix-backend  nil
  "Back end where mairix stores its searches.")

(defvoo nnmairix-backend-server nil
  "Name of the server where mairix stores its searches.")

(defvoo nnmairix-mairix-command "mairix"
  "Command to call mairix for this nnmairix server.")

(defvoo nnmairix-hidden-folders nil
  "Set this to t if the back end server uses hidden directories for
its maildir mail folders (e.g. the Dovecot IMAP server or mutt).")

(defvoo nnmairix-default-group nil
  "Default search group. This is the group which is used for all
temporary searches, e.g. nnmairix-search.")

;;; === Internal variables

(defconst nnmairix-group-regexp
  (format "%s-\\(.*\\)-[0-9]+" nnmairix-group-prefix)
  "Regexp for mairix groups on back end.")

(defconst nnmairix-valid-backends '(nnimap nnml nnmaildir)
  "Back ends supported by nnmairix.
Other back ends might or might not work.")

(defvar nnmairix-last-server nil
  "Last chosen server.")

(defvar nnmairix-current-server nil
  "Current server.")

(defvar nnmairix-marks-cache nil
  "Cache for marks which should be set upon closing current group.")

(defvar nnmairix-version-output nil
  "Version string of mairix binary.")

;;; === Gnus back end functions

(nnoo-define-basics nnmairix)

(gnus-declare-backend "nnmairix" 'mail 'address)

(deffoo nnmairix-open-server (server &optional definitions)
  ;; just set server variables
  (setq nnmairix-current-server server)
  (nnoo-change-server 'nnmairix server definitions))

(deffoo nnmairix-request-group (group &optional server fast info)
  ;; Call mairix and request group on back end server
  (when server (nnmairix-open-server server))
  (let* ((qualgroup (if server
			(gnus-group-prefixed-name group (list 'nnmairix server))
		      group))
	 (folder (gnus-group-get-parameter qualgroup 'folder))
	 (allowfast (gnus-group-get-parameter qualgroup 'allow-fast))
	 (query (gnus-group-get-parameter qualgroup 'query t))
	 (threads (gnus-group-get-parameter qualgroup 'threads))
	 (backendmethod (gnus-server-to-method
			 (format "%s:%s" (symbol-name nnmairix-backend)
				 nnmairix-backend-server)))
	 rval mfolder folderpath args)
    (cond
     ((not folder)
      ;; No folder parameter -> error
      (nnheader-report 'nnmairix "Check folder parameter for group %s" group)
      nil)
     ((not query)
      ;; No query -> return empty group
      (with-current-buffer nntp-server-buffer
	(erase-buffer)
	(insert (concat "211 0 1 0 " group))
	t))
     (t
      ;; For maildir++ folders: create a hidden directory (prepend dot)
      (setq mfolder (if (and nnmairix-hidden-folders
			     (not (string-match "^\\." folder)))
			(concat "." folder)
		      folder))
      ;; For nnml and nnmaildir, precede mfolder with directory where mail
      ;; is actually stored so that it's independent of 'base' setting
      ;; in .mairixrc.
      (when (eq nnmairix-backend 'nnml)
	(setq folderpath (cadr (assoc 'nnml-directory backendmethod)))
	;; if nnml-directory is not explicitly set, use global value
	(when (not folderpath)
	  (setq folderpath nnml-directory)))
      (when (eq nnmairix-backend 'nnmaildir)
	(setq folderpath
	      (cadr (assoc 'directory backendmethod))))
      (when folderpath
	(setq mfolder
	      (concat
	       (file-name-as-directory
		(expand-file-name
		 folderpath))
	       mfolder)))
      ;; If (not fast), call Mairix binary
      ;; recreate underlying folder on the back end
      (setq rval
	    (if (and fast allowfast)
		0
	      (nnmairix-call-mairix-binary
	       (split-string nnmairix-mairix-command)
	       mfolder query threads)))
      ;; Check return value
      (cond
       ((zerop rval)			; call was successful
	(nnmairix-call-backend
	 "open-server" nnmairix-backend-server)
	;; If we're dealing with nnml, rename files
	;; consecutively and make new active file for this
	;; group
	(when (eq nnmairix-backend 'nnml)
	  (when nnmairix-rename-files-for-nnml
	    (nnmairix-rename-files-consecutively mfolder))
	  (nnml-generate-nov-databases-directory mfolder nil t))
	(nnmairix-call-backend
	 "request-scan" folder nnmairix-backend-server)
	(if (and fast allowfast)
	    t
	  (nnmairix-request-group-with-article-number-correction
	   folder qualgroup)))
       ((and (= rval 1)
	     (with-current-buffer nnmairix-mairix-output-buffer
	       (goto-char (point-min))
	       (looking-at "^Matched 0 messages")))
	;; No messages found -> return empty group
	(nnheader-message 5 "Mairix: No matches found.")
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(insert (concat "211 0 1 0 " group))
	t)
       ;; Everything else is an error
       (t
	(nnheader-report
	 'nnmairix "Error running mairix. See buffer %s for details"
	 nnmairix-mairix-output-buffer)
	nil))))))


(deffoo nnmairix-request-create-group (group &optional server args)
  (let ((qualgroup (if server (gnus-group-prefixed-name group (list 'nnmairix server))
		     group))
	(exist t)
	(count 0)
	groupname info)
    (when server (nnmairix-open-server server))
    (gnus-group-add-parameter qualgroup '(query . nil))
    (gnus-group-add-parameter qualgroup '(threads . nil))
    (while exist
      (setq count (1+ count))
      (setq groupname (format "%s-%s-%s" nnmairix-group-prefix group
			      (number-to-string count)))
      (setq exist (nnmairix-call-backend
		   "request-group" groupname nnmairix-backend-server)))
    (nnmairix-call-backend
     "request-create-group" groupname nnmairix-backend-server)
    (gnus-group-add-parameter qualgroup '(folder . nil))
    (when nnmairix-allowfast-default
      (gnus-group-add-parameter qualgroup '(allow-fast . t)))
    (gnus-group-set-parameter qualgroup 'folder groupname))
  t)


(deffoo nnmairix-retrieve-headers (articles group &optional server fetch-old)
  (when server (nnmairix-open-server server))
  (let* ((folder (nnmairix-get-backend-folder group server))
	 (corr (nnmairix-get-numcorr group server))
	 (numcorr 0)
	 rval)
    (when (and corr
	       (not (zerop (cadr corr)))
	       (numberp (car articles)))
      (setq numcorr (cadr corr))
      (setq articles
	    (mapcar
	     (lambda (arg) (- arg numcorr))
	     articles)))
    (setq rval
	  (if (eq nnmairix-backend 'nnimap)
	      (let ((gnus-nov-is-evil t))
		(nnmairix-call-backend
		 "retrieve-headers" articles folder nnmairix-backend-server fetch-old))
	    (nnmairix-call-backend
	     "retrieve-headers" articles folder nnmairix-backend-server fetch-old)))
    (nnmairix-replace-group-and-numbers articles folder group numcorr rval)
    rval))

(deffoo nnmairix-request-article (article &optional group server to-buffer)
  (when server (nnmairix-open-server server))
  (let ((folder (nnmairix-get-backend-folder group server))
	(corr (nnmairix-get-numcorr group server)))
    (when (and
	   (numberp article)
	   corr
	   (not (zerop (cadr corr))))
      (setq article (- article (cadr corr))))
    (nnmairix-call-backend
     "request-article" article folder nnmairix-backend-server to-buffer))
  t)

(deffoo nnmairix-request-list (&optional server)
  (when server (nnmairix-open-server server))
  (if (nnmairix-call-backend "request-list" nnmairix-backend-server)
      (let (cpoint cur qualgroup folder)
	(with-current-buffer nntp-server-buffer
	  (goto-char (point-min))
	  (setq cpoint (point))
	  (while (re-search-forward nnmairix-group-regexp (point-max) t)
	    (setq cur (match-string 1)
		  qualgroup (gnus-group-prefixed-name cur
						      (list 'nnmairix server)))
	    (if (and (gnus-group-entry qualgroup)
		     (string= (match-string 0)
			      (gnus-group-get-parameter qualgroup 'folder)))
		(progn
		  (replace-match cur)
		  (delete-region cpoint (point-at-bol))
		  (forward-line)
		  (setq cpoint (point)))
	      (forward-line)))
	  (delete-region cpoint (point-max)))
	t)
    nil))

;; Silence byte-compiler.
(autoload 'gnus-registry-get-id-key "gnus-registry")

(deffoo nnmairix-request-set-mark (group actions &optional server)
  (when server
    (nnmairix-open-server server))
  (let* ((qualgroup (gnus-group-prefixed-name group (list 'nnmairix nnmairix-current-server)))
	 (propmarks (gnus-group-get-parameter qualgroup 'propmarks))
	 (propto (gnus-group-get-parameter qualgroup 'propto t))
	 (corr (nnmairix-get-numcorr group server))
	 (folder (nnmairix-get-backend-folder group server)))
    (save-excursion
      (dolist (cur actions)
	(let ((type (nth 1 cur))
	      (cmdmarks (nth 2 cur))
	      (range (gnus-uncompress-range (nth 0 cur)))
	      mid ogroup number method temp)
	  (when (and corr
		     (not (zerop (cadr corr))))
	    (setq range (mapcar (lambda (arg)
				  (- arg (cadr corr)))
				range)))
	  (when propmarks
	    (nnheader-message 7 "nnmairix: Setting marks...")
	      (dolist (article range)
		;; get article (header) and extract message id
		;; we try to determine as many original articles as possible
		(catch 'problem
		  (nnmairix-call-backend "open-server" nnmairix-backend-server)
		  (unless (gnus-request-head
			   article
			   (gnus-group-prefixed-name
			    folder
			    (list nnmairix-backend nnmairix-backend-server)))
		    (nnheader-message
		     3 "Unable to set mark: couldn't fetch article header for article number %d"
		     article)
		    (throw 'problem nil))
		  (set-buffer nntp-server-buffer)
		  (goto-char (point-min))
		  (let ((case-fold-search t))
		    (re-search-forward "^message-id:.*\\(<.+>\\)" nil t))
		  (setq mid (match-string 1))
		  (unless mid
		    (nnheader-message
		     3 "Unable to set mark: article number %d has no message-id header"
		     article)
		    (throw 'problem nil))
		  ;; get original group. First try registry, then file path
		  (setq ogroup
			(nnmairix-determine-original-group-from-registry mid))
		  (unless (or ogroup
			      nnmairix-only-use-registry)
		    (setq ogroup
			  (nnmairix-determine-original-group-from-path
			   mid nnmairix-current-server)))
		  (unless ogroup
		    (nnheader-message
		     3 "Unable to set mark: couldn't find original group for %s" mid)
		    (throw 'problem nil))
		  ;; store original groups with mid's. We cannot get
		  ;; the article number immediately since this would
		  ;; generate problems with maildir (articles might
		  ;; get moved from /new to /cur and further marks
		  ;; could then not be set)
		  (dolist (cur ogroup)
		    (setq temp (assoc cur
				      nnmairix-marks-cache))
		    (if temp
			(nconc temp (list (list mid type cmdmarks)))
		      (push (list cur (list mid type cmdmarks))
			    nnmairix-marks-cache)))))
	      (nnheader-message 7 "nnmairix: Setting marks... done")))))))

(deffoo nnmairix-close-group (group &optional server)
  (when server
    (nnmairix-open-server server))
  (let* ((qualgroup (gnus-group-prefixed-name group (list 'nnmairix nnmairix-current-server)))
	 (propmarks (gnus-group-get-parameter qualgroup 'propmarks))
	 method)
    (when (and propmarks
	       nnmairix-marks-cache)
      (when (or (eq nnmairix-propagate-marks-upon-close t)
		(and (eq nnmairix-propagate-marks-upon-close 'ask)
		     (y-or-n-p "Propagate marks to original articles? ")))
      (with-current-buffer gnus-group-buffer
	(nnmairix-propagate-marks)
	;; update mairix group
	(gnus-group-jump-to-group qualgroup)
	(gnus-group-get-new-news-this-group))))))

(autoload 'nnimap-request-update-info-internal "nnimap")

(deffoo nnmairix-request-marks (group info &optional server)
;; propagate info from underlying IMAP folder to nnmairix group
;; This is currently experimental and must be explicitly activated
;; with nnmairix-propagate-marks-to-nnmairix-group
  (when server
    (nnmairix-open-server server))
  (let* ((qualgroup (gnus-group-prefixed-name
		    group
		    (list 'nnmairix nnmairix-current-server)))
	 (readmarks (gnus-group-get-parameter qualgroup 'readmarks))
	 (propmarks (gnus-group-get-parameter qualgroup 'propmarks))
	 (folder (nnmairix-get-backend-folder group server))
	 (corr (nnmairix-get-numcorr group server))
	 (docorr (and corr (not (zerop (cadr corr)))))
	 (folderinfo `(,group 1 ((1 . 1))))
	 readrange marks)
      (when (and propmarks
		 nnmairix-propagate-marks-to-nnmairix-groups)
	;; these groups are not subscribed, so we have to ask the back end directly
	(if (eq nnmairix-backend 'nnimap)
	    (nnimap-request-update-info-internal folder folderinfo nnmairix-backend-server)
	  (nnmairix-call-backend "request-update-info" folder folderinfo nnmairix-backend-server))
	;; set range of read articles
	(gnus-info-set-read
	 info
	 (if docorr
	     (nnmairix-map-range
	      `(lambda (x) (+ x ,(cadr corr)))
	      (gnus-info-read folderinfo))
	   (gnus-info-read folderinfo)))
	;; set other marks
	(gnus-info-set-marks
	 info
	 (if docorr
	     (mapcar (lambda (cur)
			 (cons
			  (car cur)
			  (nnmairix-map-range
			   `(lambda (x) (+ x ,(cadr corr)))
			   (list (cadr cur)))))
		     (gnus-info-marks folderinfo))
	   (gnus-info-marks folderinfo))))
      (when (eq readmarks 'unread)
	(gnus-info-set-read info nil))
      (when (eq readmarks 'read)
	(gnus-info-set-read info (gnus-active qualgroup))))
  t)

(nnoo-define-skeleton nnmairix)


;;; === Interactive functions

(defun nnmairix-create-search-group (server group query threads)
  "Create on SERVER nnmairix search group GROUP with QUERY.
If THREADS is t, include whole threads from found messages.  If
called interactively, user will be asked for parameters."
  (interactive
   (list
    (gnus-server-to-method (car (nnmairix-get-server)))
    (read-string "Group name: ")
    (read-string "Query: ")
    (y-or-n-p "Include threads? ")))
  (when (and (stringp query)
	     (string-match "\\s-" query))
    (setq query (split-string query)))
  (when (not (listp query))
    (setq query (list query)))
  (when (and server group query)
    (save-excursion
      (let ((groupname (gnus-group-prefixed-name group server))
	    info)
	(set-buffer gnus-group-buffer)
	(gnus-group-make-group group server)
	(gnus-group-set-parameter groupname 'query  query)
	(gnus-group-set-parameter groupname 'threads threads)
	(nnmairix-update-and-clear-marks groupname)))))

(defun nnmairix-search-interactive ()
  "Create mairix search interactively with the minibuffer."
  (interactive)
  (let ((char-header nnmairix-interactive-query-parameters)
	header finished query achar)
    (while (not finished)
      (while (not achar)
	(message "Query (%s): " (nnmairix-create-message-line-for-search))
	  (setq achar (read-char))
	  (when (not (assoc achar char-header))
	    (setq achar nil)))
      (setq header (read-string
		    (concat "Match " (nth 3 (assoc achar char-header)) " on: ")))
	(push  (concat (nth 2 (assoc achar char-header)) ":" header) query)
	(setq finished (not (y-or-n-p "Add another search query? "))
	      achar nil))
    (nnmairix-search
     (mapconcat 'identity query " ")
     (car (nnmairix-get-server))
     (y-or-n-p "Include whole threads? "))))

(defun nnmairix-create-search-group-from-message ()
  "Interactively create search group with query based on current message."
  (interactive)
  (let ((char-header nnmairix-interactive-query-parameters)
	(server (nnmairix-backend-to-server gnus-current-select-method))
	 query achar header finished group threads cq)
    (when (or (not (gnus-buffer-live-p gnus-article-buffer))
	      (not (gnus-buffer-live-p gnus-summary-buffer)))
      (error "No article or summary buffer"))
    (when (not server)
      (error "No nnmairix server found for back end %s:%s"
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))
    (while (not finished)
      (save-excursion
	(gnus-summary-toggle-header 1)
	(while (not achar)
	  (message "Query (%s): " (nnmairix-create-message-line-for-search))
	  (setq achar (read-char))
	  (when (not (assoc achar char-header))
	    (setq achar nil)))
	(set-buffer gnus-article-buffer)
	(setq header nil)
	(when (setq cq (nth 1 (assoc achar char-header)))
	  (setq header
		(nnmairix-replace-illegal-chars
		 (gnus-fetch-field (nth 1 (assoc achar char-header))))))
	(setq header (read-string
		      (concat "Match " (nth 3 (assoc achar char-header)) " on: ")
		      header))
	(push  (concat (nth 2 (assoc achar char-header)) ":" header) query)
	(setq finished (not (y-or-n-p "Add another search query? "))
	      achar nil)))
    (setq threads (y-or-n-p "Include whole threads? "))
    (setq group (read-string "Group name: "))
    (set-buffer gnus-summary-buffer)
    (message "Creating group %s on server %s with query %s." group
	     (gnus-method-to-server server) (mapconcat 'identity query " "))
    (nnmairix-create-search-group server group query threads)))

(defun nnmairix-create-server-and-default-group ()
  "Interactively create new nnmairix server with default search group.
All necessary information will be queried from the user."
  (interactive)
  (let* ((name (read-string "Name of the mairix server: "))
	(server (gnus-completing-read "Back end server"
				 (nnmairix-get-valid-servers) t))
	(mairix (read-string "Command to call mairix: " "mairix"))
	(defaultgroup (read-string "Default search group: "))
	(backend (symbol-name (car (gnus-server-to-method server))))
	(servername (nth 1 (gnus-server-to-method server)))
	(hidden (and (string-match "^nn\\(imap\\|maildir\\)$" backend)
		     (y-or-n-p
		      "Does the back end server work with maildir++ (i.e. hidden directories)? ")))
	create)

    (apply (intern (format "%s-%s" backend "open-server"))
	   (list servername))

    (when (and hidden
	       (string-match "^\\." defaultgroup))
      (setq defaultgroup (substring defaultgroup 1)))
    ;; Create default search group
    (gnus-group-make-group
     defaultgroup (list 'nnmairix name  (list 'nnmairix-backend (intern backend))
			(list 'nnmairix-backend-server servername)
			(list 'nnmairix-mairix-command mairix)
			(list 'nnmairix-hidden-folders hidden)
			(list 'nnmairix-default-group defaultgroup)))))

(defun nnmairix-group-change-query-this-group (&optional query)
  "Set QUERY for group under cursor."
  (interactive)
  (let* ((group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group))
	 (oldquery (gnus-group-get-parameter group 'query t)))
    (if (eq (car method) 'nnmairix)
	(progn
	  (when (listp oldquery)
	    (setq oldquery (mapconcat 'identity oldquery " ")))
	  (setq query (or query
			  (read-string "New query: " oldquery)))
	  (when (stringp query)
	    (setq query (split-string query)))
	  (when query
	    (gnus-group-set-parameter group 'query query)
	    (nnmairix-update-and-clear-marks group)))
      (error "This is no nnmairix group"))))


(defun nnmairix-group-toggle-threads-this-group (&optional threads)
  "Toggle threads parameter for this group.
If THREADS is a positive number, set threads parameter to t.
If THREADS is a negative number, set it to nil."
  (interactive)
  (let ((group (gnus-group-group-name)))
    (when (nnmairix-group-toggle-parameter
	   group 'threads "Threads" threads)
      (nnmairix-update-and-clear-marks group))))

(defun nnmairix-group-toggle-propmarks-this-group (&optional propmarks)
  "Toggle marks propagation for this group.
If PROPMARKS is a positive number, set parameter to t.
If PROPMARKS is a negative number, set it to nil."
  (interactive)
  (unless (nnmairix-check-mairix-version "maildirpatch")
    (error "You need a mairix binary with maildir patch to use this feature.  See docs for details"))
  (let ((group (gnus-group-group-name)))
    (when (or (not (string= (gnus-group-short-name group)
			    (cadr (assoc 'nnmairix-default-group
					(gnus-find-method-for-group group)))))
	      (y-or-n-p "You should not activate marks propagation for the default \
search group.  Are you sure? "))
      (nnmairix-group-toggle-parameter
       group 'propmarks "Marks propagation" propmarks))))

(defun nnmairix-group-toggle-allowfast-this-group (&optional allowfast)
  "Toggle fast entering for this group.
If ALLOWFAST is a positive number, set parameter to t.
If ALLOWFAST is a negative number, set it to nil."
  (interactive)
  (nnmairix-group-toggle-parameter
   (gnus-group-group-name) 'allow-fast "Fast entering" allowfast))


(defun nnmairix-group-toggle-readmarks-this-group (&optional readmarks)
  "Toggle read/unread marks for this group.
If READMARKS is a positive number, articles will always be read.
If READMARKS is a negative number, articles will always be unread.
If READMARKS is t or zero, marks will stay unchanged."
  (interactive)
  (let*  ((group (gnus-group-group-name))
	  (method (gnus-find-method-for-group group))
	  (readmarks (or readmarks
			 (gnus-group-get-parameter group 'readmarks))))
    (if (eq (car method) 'nnmairix)
	(cond
	 ((or (and (numberp readmarks) (< readmarks 0))
	      (eq readmarks 'read))
	  (gnus-group-set-parameter group 'readmarks 'unread)
	  (nnheader-message 3 "Articles in %s always unread." group))
	 ((or (and (numberp readmarks) (> readmarks 0))
	      (not readmarks))
	      (gnus-group-set-parameter group 'readmarks 'read)
	      (nnheader-message 3 "Articles in %s always read." group))
	 (t
	  (gnus-group-set-parameter group 'readmarks nil)
	  (nnheader-message 3 "Read marks in %s stay unchanged." group)))
      (error "This is no nnmairix group"))))


(defun nnmairix-search (query &optional server threads)
  "Sends QUERY to nnmairix backend SERVER, using default its search group.

Default search group is automatically entered and results are shown.
If THREADS is t, enable threads.
If THREADS is a negative number, disable threads.
Otherwise, leave threads parameter as it is."
  (interactive (list (read-string "Query: ")))
  (when (not server)
    (setq server (car (nnmairix-get-server))))
  (if (not server)
      (error "No opened nnmairix server found")
    (setq server (gnus-server-to-method server)))
  (nnmairix-open-server (nth 1 server))
  (let* ((qualgroup (gnus-group-prefixed-name nnmairix-default-group
					      (list 'nnmairix (nth 1 server)))))
    (set-buffer gnus-group-buffer)
    (when (stringp query)
      (setq query (split-string query)))
    (gnus-group-set-parameter qualgroup 'query query)
    (if (symbolp threads)
	(when (eq threads 't)
	  (gnus-group-set-parameter qualgroup 'threads t))
      (when (< threads 0)
	(gnus-group-set-parameter qualgroup 'threads nil)))
    (nnmairix-update-and-clear-marks qualgroup)
    (unless (equal (gnus-active qualgroup) '(1 . 0))
      (gnus-group-read-group nil t qualgroup))))

(defun nnmairix-search-thread-this-article ()
  "Search thread for the current article.
This is effectively a shortcut for calling `nnmairix-search'
with m:msgid of the current article and enabled threads."
  (interactive)
  (let* ((server
	  (nnmairix-backend-to-server gnus-current-select-method))
	 mid)
    (if server
	(if (gnus-buffer-live-p gnus-article-buffer)
	    (progn
	      (with-current-buffer gnus-article-buffer
		(gnus-summary-toggle-header 1)
		(setq mid (message-fetch-field "Message-ID")))
	      (while (string-match "[<>]" mid)
		(setq mid (replace-match "" t t mid)))
	      (nnmairix-search (concat "m:" mid) server t))
	  (message "No article buffer."))
      (error "No nnmairix server found for back end %s:%s"
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))))

(defun nnmairix-search-from-this-article ()
  "Search messages from sender of the current article.
This is effectively a shortcut for calling `nnmairix-search' with
f:current_from."
  (interactive)
  (let* ((server
	  (nnmairix-backend-to-server gnus-current-select-method))
	 from)
    (if server
	(if (gnus-buffer-live-p gnus-article-buffer)
	    (progn
	      (with-current-buffer gnus-article-buffer
		(gnus-summary-toggle-header 1)
		(setq from (cadr (gnus-extract-address-components
				  (gnus-fetch-field "From"))))
		(nnmairix-search (concat "f:" from) server -1)))
	  (message "No article buffer."))
      (error "No nnmairix server found for back end %s:%s"
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))))


(defun nnmairix-purge-old-groups (&optional dontask server)
  "Delete mairix search groups which are no longer used.

You may want to call this from time to time if you are creating
and deleting lots of nnmairix groups.  If DONTASK is t, do not ask
before deleting a group on the back end.  SERVER specifies nnmairix server."
  (interactive)
  (let ((server (or server
		    (gnus-server-to-method (car (nnmairix-get-server))))))
    (if (nnmairix-open-server (nth 1 server))
	(when (nnmairix-call-backend
	       "request-list" nnmairix-backend-server)
	  (let (cur qualgroup folder)
	    (with-current-buffer nntp-server-buffer
	      (goto-char (point-min))
	      (while (re-search-forward nnmairix-group-regexp (point-max) t)
		(setq cur (match-string 0)
		      qualgroup (gnus-group-prefixed-name
				 (match-string 1) server))
		(when (not (and (gnus-group-entry qualgroup)
				(string= cur
					 (gnus-group-get-parameter
					  qualgroup 'folder))))
		  (when (or dontask
			    (y-or-n-p
			     (concat "Delete group " cur
				     " on server " nnmairix-backend-server "? ")))
		    (nnmairix-call-backend
		     "request-delete-group" cur t nnmairix-backend-server)))))))
      (message "Couldn't open server %s" (nth 1 server)))))


(defun nnmairix-update-database (&optional servers)
  "Call mairix for updating the database for SERVERS.

If SERVERS is nil, do update for all nnmairix servers.  Mairix
will be called asynchronously unless
`nnmairix-mairix-synchronous-update' is t.  Mairix will be called
with `nnmairix-mairix-update-options'."
  (interactive)
  (let ((servers (or servers
		     (nnmairix-get-nnmairix-servers)))
	args cur commandsplit)
    (while servers
      (setq cur (car (pop servers)))
      (nnmairix-open-server
       (nth 1 (gnus-server-to-method cur)))
      (setq commandsplit (split-string nnmairix-mairix-command))
      (nnheader-message 7 "Updating mairix database for %s..." cur)
      (if nnmairix-mairix-synchronous-update
	  (progn
	    (setq args (append (list (car commandsplit) nil
				     (get-buffer nnmairix-mairix-output-buffer)
				     nil)))
	    (if (> (length commandsplit) 1)
		(setq args (append args (cdr commandsplit) nnmairix-mairix-update-options))
	      (setq args (append args nnmairix-mairix-update-options)))
	    (apply 'call-process args)
	    (nnheader-message 7 "Updating mairix database for %s... done" cur))
	(progn
	  (setq args (append (list cur (get-buffer nnmairix-mairix-output-buffer)
				   (car commandsplit))))
	  (if (> (length commandsplit) 1)
	      (setq args (append args (cdr commandsplit) nnmairix-mairix-update-options))
	    (setq args (append args nnmairix-mairix-update-options)))
	  (set-process-sentinel (apply 'start-process args)
				'nnmairix-sentinel-mairix-update-finished))))))

(defun nnmairix-group-delete-recreate-this-group ()
  "Deletes and recreates group on the back end.
You can use this function on nnmairix groups which continuously
show wrong article counts."
  (interactive)
  (let* ((group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group)))
    (unless (eq (car method) 'nnmairix)
      (error "This is not a nnmairix group"))
    (when (y-or-n-p
	   (format "Really recreate group %s on the back end? " group))
      (nnmairix-delete-recreate-group group)
      (gnus-group-get-new-news-this-group))))

(defun nnmairix-propagate-marks (&optional server)
  "Propagate marks from nnmairix group to original articles.
Unless SERVER is explicitly specified, will use the last opened
nnmairix server. Only marks from current session will be set."
  (interactive)
  (if server
      (nnmairix-open-server server)
    (unless (eq (car gnus-current-select-method) 'nnmairix)
      (if nnmairix-current-server
	  (nnmairix-open-server nnmairix-current-server)
	(error "No opened nnmairix server"))))
  (if nnmairix-marks-cache
      (let (number ogroup number-cache method mid-marks temp)
	;; first we get the article numbers
	(catch 'problem
	  (while (setq ogroup (pop nnmairix-marks-cache))
	    (while (setq mid-marks (pop (cdr ogroup)))
	      (setq number
		    (cdr
		     (gnus-request-head (car mid-marks) (car ogroup))))
	      (unless number
		(nnheader-message
		 3 "Unable to set mark: couldn't determine article number for %s in %s"
		 (car mid-marks) (car ogroup))
		(throw 'problem nil))
	      (setq temp (assoc (car ogroup) number-cache))
	      (if temp
		  (catch 'done
		    (dolist (cur (cdr temp))
		      (when (equal (cdr cur) (list (nth 1 mid-marks) (nth 2 mid-marks)))
			(nconc (car cur) (list number))
			(throw 'done nil)))
		    (nconc temp (list (list (list number) (nth 1 mid-marks) (nth 2 mid-marks)))))
		(push (list (car ogroup) (list (list number) (nth 1 mid-marks) (nth 2 mid-marks)))
		      number-cache)))))
	;; now we set the marks
	(with-current-buffer gnus-group-buffer
	  (nnheader-message 5 "nnmairix: Propagating marks...")
	  (dolist (cur number-cache)
	    (setq method (gnus-find-method-for-group (car cur)))
	    (apply (intern (format "%s-%s"
				   (symbol-name (car method))
				   "request-set-mark"))
		   (gnus-group-short-name (car cur))
		   (cdr cur)
		   (list (nth 1 method)))
	    (gnus-group-jump-to-group (car cur))
	    (gnus-group-get-new-news-this-group)))
	(nnheader-message 5 "nnmairix: Propagating marks... done"))
    (nnheader-message 3 "No marks to propagate.")))

(defun nnmairix-update-groups (servername &optional skipdefault updatedb)
  "Update all search groups on SERVERNAME.
If SKIPDEFAULT is t, the default search group will not be
updated.
If UPDATEDB is t, database for SERVERNAME will be updated first."
  (interactive (list (gnus-completing-read "Update groups on server"
				(nnmairix-get-nnmairix-servers))))
  (save-excursion
    (when (string-match ".*:\\(.*\\)" servername)
      (setq servername (match-string 1 servername)))
    (if (not (assoc (format "nnmairix:%s" servername)
		    (nnmairix-get-nnmairix-servers)))
	(nnheader-message 3 "Server %s not opened" servername)
      (when updatedb
	(let ((nnmairix-mairix-synchronous-update t))
	  (nnmairix-update-database
	   (list (list (format "nnmairix:%s" servername))))))
      (let ((groups (nnmairix-get-groups-from-server servername))
	    default)
	(when skipdefault
	  (setq default
		(format "nnmairix+%s:%s"
			servername
			(cadr
			 (assoc 'nnmairix-default-group
				(gnus-server-to-method
				 (format "nnmairix:%s" servername)))))))
	(dolist (cur groups)
	  (unless (and skipdefault
		       (string= (car cur) default))
	    (gnus-group-jump-to-group (car cur))
	    (gnus-group-mark-group 1)))
	(gnus-group-get-new-news-this-group)))))

(defun nnmairix-remove-tick-mark-original-article ()
  "Remove tick mark from original article.
Marks propagation has to be enabled for this to work."
  (interactive)
  (unless (eq (car gnus-current-select-method) 'nnmairix)
    (error "Not in a nnmairix group"))
  (save-excursion
    (let ((mid (mail-header-message-id (gnus-summary-article-header)))
	  groups cur)
      (when mid
	(setq groups (nnmairix-determine-original-group-from-registry mid))
	(unless (or groups
		    nnmairix-only-use-registry)
	  (setq groups
		(nnmairix-determine-original-group-from-path mid nnmairix-current-server)))
	(unless groups
	  (error "Couldn't find original article"))
	(dolist (cur groups)
	  (push `(,cur (,mid del (tick))) nnmairix-marks-cache))
	(nnheader-message 5 "Will remove tick mark for %s upon closing." mid)))))

;;; ==== Helper functions

(defun nnmairix-request-group-with-article-number-correction (folder qualgroup)
  "Request FOLDER on back end for nnmairix QUALGROUP and article number correction."
  (save-excursion
    (nnmairix-call-backend "request-group" folder nnmairix-backend-server)
    (set-buffer nnmairix-mairix-output-buffer)
    (goto-char (point-min))
    (re-search-forward "^Matched.*messages")
    (nnheader-message 7 (match-string 0))
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (let ((status (read (current-buffer)))
	  (total (read (current-buffer)))
	  (low (read (current-buffer)))
	  (high (read (current-buffer)))
	  (corr (gnus-group-get-parameter qualgroup 'numcorr t)))
      (if (= status 211)
	  (progn
	    ;; Article number correction
	    (if (and corr
		     (> (+ (car (cddr corr)) high) 0))
		(progn
		  (when (car corr) ;Group has changed
		    (setq corr
			  (list nil
				(car (cddr corr))
				(+ (car (cddr corr)) high)))
		    (gnus-group-set-parameter
		     qualgroup 'numcorr corr))
		  (setq low (+ low (cadr corr))
			high (+ high (cadr corr))))
	      (when (member nnmairix-backend
			    nnmairix-delete-and-create-on-change)
		(gnus-group-set-parameter
		 qualgroup 'numcorr (list nil 0 high))))
	    (erase-buffer)
	    (insert (format "%d %d %d %d %s" status total low high
			    (gnus-group-real-name qualgroup)))
	    t)
	(progn
	  (nnheader-report
	   'nnmairix "Error calling back end on group %s" folder)
	  nil)))))

(defun nnmairix-call-mairix-binary (command folder searchquery threads)
  "Call mairix binary with COMMAND, using FOLDER and SEARCHQUERY.
If THREADS is non-nil, enable full threads."
  (let ((args (cons (car command) '(nil t nil))))
    (with-current-buffer
       (get-buffer-create nnmairix-mairix-output-buffer)
      (erase-buffer)
      (when (> (length command) 1)
	(setq args (append args (cdr command))))
      (when nnmairix-mairix-search-options
	(setq args (append args nnmairix-mairix-search-options)))
      ;; If we have a patched mairix binary, call it with "-c"
      (when (nnmairix-check-mairix-version "maildirpatch")
	(setq args (append args '("-c"))))
      (when threads
	(setq args (append args '("-t"))))
      (apply 'call-process
	     (append args (list "-o" folder) searchquery)))))

(defun nnmairix-call-mairix-binary-raw (command query)
  "Call mairix binary with COMMAND and QUERY in raw mode."
  (let ((args (cons (car command) '(nil t nil))))
    (with-current-buffer
       (get-buffer-create nnmairix-mairix-output-buffer)
      (erase-buffer)
      (when (> (length command) 1)
        (setq args (append args (cdr command))))
      (setq args (append args '("-r")))
      (apply 'call-process
             (append args query)))))

(defun nnmairix-get-server ()
  "If there exists just one nnmairix server, return its value.
Otherwise, ask user for server."
  (let ((openedserver (nnmairix-get-nnmairix-servers)))
    (when (not openedserver)
      (error "No opened nnmairix server found"))
    (if (> (length openedserver) 1)
	(progn
	  (while
	      (equal '("")
		  (setq nnmairix-last-server
			(list (gnus-completing-read "Server" openedserver t
					       (or nnmairix-last-server
						   "nnmairix:"))))))
	  nnmairix-last-server)
      (car openedserver))))

(defun nnmairix-get-nnmairix-servers (&optional all)
  "Return available nnmairix servers.
If ALL is t, return also the unopened/failed ones."
  (let ((alist gnus-opened-servers)
	server openedserver)
    (while alist
      (setq server (pop alist))
      (when (and server
		 (or all
		     (eq (cadr server) 'ok))
		 (eq (caar server) 'nnmairix)
		 (not (member (car server) gnus-ephemeral-servers)))
	(setq server
	      (concat (symbol-name (caar server)) ":" (nth 1 (car server))))
	(push (list server) openedserver)))
    openedserver))

(defun nnmairix-get-valid-servers ()
  "Return list of valid back end servers for nnmairix groups."
  (let ((alist gnus-opened-servers)
	(mairixservers (nnmairix-get-nnmairix-servers t))
	server mserver openedserver occ cur)
    ;; Get list of all nnmairix backends (i.e. backends which are
    ;; already occupied)
    (dolist (cur mairixservers)
      (push
       (concat
	(symbol-name
	 (cadr (assoc 'nnmairix-backend
		      (gnus-server-to-method (car cur)))))
	 ":"
	 (cadr (assoc 'nnmairix-backend-server
		      (gnus-server-to-method (car cur)))))
	occ))
    (while alist
      (setq server (pop alist))
      (setq mserver (gnus-method-to-server (car server)))
      ;; If this is the native server, convert it to the real server
      ;; name to avoid confusion
      (when (string= mserver "native")
	(setq mserver (format "%s:%s"
			      (caar server)
			      (nth 1 (car server)))))
      (when (and server
		 (eq (cadr server) 'ok)
		 (member (caar server) nnmairix-valid-backends)
		 (not (member (car server) gnus-ephemeral-servers))
		 (not (member (gnus-method-to-server (car server)) occ)))
	(push
	 mserver
	 openedserver)))
    openedserver))

(defun nnmairix-get-groups-from-server (servername)
  "Return all groups for nnmairix server SERVERNAME."
  (let ((searchstring (format "nnmairix\\+%s:" servername))
	groups)
    (dolist (cur gnus-newsrc-alist)
      (when (string-match searchstring
			  (car cur))
	(push (list (car cur)) groups)))
    groups))

(defun nnmairix-call-backend (func &rest args)
  "Call a function FUNC on backend with ARGS."
  (apply (intern (format "%s-%s" (symbol-name nnmairix-backend) func)) args))

(defun nnmairix-get-backend-folder (group &optional server)
  "Return back end GROUP from nnmairix group on SERVER."
  (let* ((qualgroup (if server
			(gnus-group-prefixed-name group (list 'nnmairix server))
		      group))
	 (folder (gnus-group-get-parameter qualgroup 'folder)))
    folder))

(defun nnmairix-get-numcorr (group &optional server)
  "Return values for article number correction nnmairix GROUP on SERVER."
  (let* ((qualgroup (if server
			(gnus-group-prefixed-name group (list 'nnmairix server))
		      group))
	 (corr (gnus-group-get-parameter qualgroup 'numcorr t)))
    corr))


(defun nnmairix-rename-files-consecutively (path)
  "Rename all nnml mail files in PATH so that they have consecutive numbers.
This should correct problems of wrong article counts when using
nnmairix with nnml backends."
  (let* ((files
	 (sort
	  (mapcar 'string-to-number
		  (directory-files path nil "[0-9]+" t))
	  '<))
	 (lastplusone (car files))
	 (path (file-name-as-directory path)))
    (dolist (cur files)
      (when (not (= cur lastplusone))
	(rename-file (concat path
			     (number-to-string cur))
		     (concat path
			     (number-to-string lastplusone)))
	(setq cur lastplusone))
      (setq lastplusone (1+ cur)))))

(defun nnmairix-replace-group-and-numbers (articles backendgroup mairixgroup numc type)
  "Replace folder names in Xref header and correct article numbers.
Do this for all ARTICLES on BACKENDGROUP.  Replace using
MAIRIXGROUP.  NUMC contains values for article number correction.
TYPE is either 'nov or 'headers."
  (nnheader-message 7 "nnmairix: Rewriting headers...")
  (cond
   ((eq type 'nov)
    (let ((buf (get-buffer-create " *nnmairix buffer*"))
	  (corr (not (zerop numc)))
	  (name (buffer-name nntp-server-buffer))
	  header cur xref)
      (with-current-buffer buf
	(erase-buffer)
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(mapc
	 (lambda (article)
	   (when (or (looking-at (number-to-string article))
		     (nnheader-find-nov-line article))
	     (setq cur (nnheader-parse-nov))
	     (when corr
	       (setq article (+ (mail-header-number cur) numc))
	       (mail-header-set-number cur article))
	     (setq xref (mail-header-xref cur))
	     (when (and (stringp xref)
			(string-match (format "[ \t]%s:[0-9]+" backendgroup) xref))
	       (setq xref (replace-match (format " %s:%d" mairixgroup article) t nil xref))
	       (mail-header-set-xref cur xref))
	     (set-buffer buf)
	     (nnheader-insert-nov cur)
	     (set-buffer nntp-server-buffer)
	     (when (not (eobp))
	       (forward-line 1))))
	 articles)
	(kill-buffer nntp-server-buffer)
	(set-buffer buf)
	(rename-buffer name)
	(setq nntp-server-buffer buf))))
   ((and (eq type 'headers)
	 (not (zerop numc)))
    (with-current-buffer nntp-server-buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^[23][0-9]+ \\([0-9]+\\)" nil t)
	  (replace-match (number-to-string
			  (+ (string-to-number (match-string 1)) numc))
			 t t nil 1))))))
  (nnheader-message 7 "nnmairix: Rewriting headers... done"))

(defun nnmairix-backend-to-server (server)
  "Return nnmairix server most probably responsible for back end SERVER.
User will be asked if this cannot be determined.  Result is saved in
parameter 'indexed-servers of corresponding default search
group."
  (let ((allservers (nnmairix-get-nnmairix-servers))
	mairixserver found defaultgroup)
    (if (> (length allservers) 1)
	(progn
	  ;; If there is more than one nnmairix server, we go through them
	  (while (and allservers (not found))
	    (setq mairixserver (gnus-server-to-method (car (pop allservers))))
	    ;; First we look if SERVER is the backend of current nnmairix server
	    (setq found (and (eq (cadr (assoc 'nnmairix-backend mairixserver))
				 (car server))
			     (string= (cadr (assoc 'nnmairix-backend-server mairixserver))
				      (nth 1 server))))
	    ;; If that's not the case, we look at 'indexed-servers
	    ;; variable in default search group
	    (when (not found)
	      (setq defaultgroup (cadr (assoc 'nnmairix-default-group mairixserver)))
	      (setq found (member (gnus-method-to-server server)
				  (gnus-group-get-parameter
				   (gnus-group-prefixed-name defaultgroup
							     mairixserver)
				   'indexed-servers t)))))
	  ;; If still not found, we ask user
	  (when (not found)
	    (setq mairixserver
		  (gnus-server-to-method
		   (gnus-completing-read
		    (format "Cannot determine which nnmairix server indexes %s. Please specify"
			    (gnus-method-to-server server))
		    (nnmairix-get-nnmairix-servers) nil "nnmairix:")))
	    ;; Save result in parameter of default search group so that
	    ;; we don't have to ask again
	    (setq defaultgroup (gnus-group-prefixed-name
				(cadr (assoc 'nnmairix-default-group mairixserver)) mairixserver))
	    (gnus-group-set-parameter
	     defaultgroup
	     'indexed-servers
	     (append (gnus-group-get-parameter defaultgroup 'indexed-servers t)
		     (list (gnus-method-to-server server)))))
	  mairixserver)
      ;; If there is just one (or none) nnmairix server:
      (gnus-server-to-method (caar allservers)))))


(defun nnmairix-delete-recreate-group (group)
  "Delete and recreate folder from GROUP on the back end."
  (when (member nnmairix-backend nnmairix-delete-and-create-on-change)
    (let ((folder (gnus-group-get-parameter group 'folder)))
      (if (string-match nnmairix-group-regexp folder)
	  (progn
	    (nnmairix-call-backend "open-server"
				   nnmairix-backend-server)
	    (nnmairix-call-backend "request-delete-group"
				   folder t nnmairix-backend-server)
	    (nnmairix-call-backend "request-create-group"
				   folder nnmairix-backend-server))
	(error "`nnmairix-delete-recreate-group' called on \
non-mairix group.  Check folder parameter")))))

(defun nnmairix-update-and-clear-marks (group &optional method)
  "Update group and clear all marks from GROUP using METHOD."
  (let ((method (or method
		    (gnus-find-method-for-group group)))
	(corr (gnus-group-get-parameter group 'numcorr t))
	info)
    (unless (or (gnus-group-prefixed-p group)
		(not method))
      (setq group (gnus-group-prefixed-name group method)))
    (if (eq (nth 0 method) 'nnmairix)
	(save-excursion
	  (nnmairix-open-server (nth 1 method))
	  (set-buffer gnus-group-buffer)
	  ;; (gnus-group-set-parameter group 'propmarks nil)
	  (setq info (gnus-get-info group))
	  ;; Clear active and info
	  (gnus-set-active group nil)
	  (gnus-info-clear-data info)
	  ;; Delete and re-create group if needed
	  (nnmairix-delete-recreate-group group)
	  ;; set flag that group has changed for article number correction
	  (when (member nnmairix-backend nnmairix-delete-and-create-on-change)
	    (when corr
	      (setcar corr t)
	      (gnus-group-set-parameter group 'numcorr corr)))
	  (gnus-group-jump-to-group group)
	  (gnus-group-get-new-news-this-group))
      (error "`nnmairix-update-and-clear-marks' called with non-nnmairix group"))))

(defun nnmairix-sentinel-mairix-update-finished (proc status)
  "Sentinel for mairix update process PROC with STATUS."
  (if (equal status "finished\n")
      (nnheader-message 7 "Updating mairix database for %s... done" proc)
    (error "There was an error updating the mairix database for server %s.  \
See %s for details" proc nnmairix-mairix-output-buffer)))

(defun nnmairix-create-message-line-for-search ()
  "Create message line for interactive query in minibuffer."
  (mapconcat
   (function
    (lambda (cur)
      (format "%c=%s" (car cur) (nth 3 cur))))
   nnmairix-interactive-query-parameters ","))

(defun nnmairix-replace-illegal-chars (header)
  "Replace illegal characters in HEADER for mairix query."
  (when header
    (while (string-match "[^-.@/,& [:alnum:]]" header)
      (setq header (replace-match "" t t header)))
    (while (string-match "[-& ]" header)
      (setq header (replace-match "," t t header)))
    header))

(defun nnmairix-group-toggle-parameter (group parameter description &optional par)
  "Toggle on GROUP a certain PARAMETER.
DESCRIPTION will be shown to the user with the activation
status.  If PAR is a positive number, the group parameter will be
set to t and to nil otherwise."
  (let* ((method (gnus-find-method-for-group group))
	 (par (or par
		  (not (gnus-group-get-parameter group parameter)))))
    (if (eq (car method) 'nnmairix)
	(progn
	  (when (numberp par)
	    (setq par (> par 0)))
	  (gnus-group-set-parameter group parameter par)
	  (if par
	      (message "%s activated for group %s" description group)
	    (message "%s deactivated for group %s" description group))
	  t)
      (error "This is no nnmairix group")
      nil)))

;; Search for original article helper functions

(defun nnmairix-goto-original-article (&optional no-registry)
  "Jump to the original group and display article.
The original group of the article is first determined with the
registry (if enabled).  If the registry is not enabled or did not
find the article or the prefix NO-REGISTRY is non-nil, this
function will try to determine the original group form the path
of the mail file.  The path is obtained through another mairix
search in raw mode."
  (interactive "P")
  (when (not (eq (car gnus-current-select-method) 'nnmairix))
    (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
      (if (eq (car method) 'nnmairix)
	  (nnmairix-open-server (nth 1 method))
	(error "Not in a nnmairix group"))))
  (when (not (gnus-buffer-live-p gnus-article-buffer))
    (error "No article buffer available"))
  (let ((server (nth 1 gnus-current-select-method))
	mid rval group allgroups)
    ;; get message id
    (with-current-buffer gnus-article-buffer
      (gnus-summary-toggle-header 1)
      (setq mid (message-fetch-field "Message-ID"))
      ;; first check the registry (if available)
      (unless no-registry
	(setq allgroups (nnmairix-determine-original-group-from-registry mid)))
      (unless (or allgroups
		  nnmairix-only-use-registry)
	;; registry was not available or did not find article
	;; so we search again with mairix in raw mode to get filename
	(setq allgroups
	      (nnmairix-determine-original-group-from-path mid server)))
      (if (> (length allgroups) 1)
	  (setq group
		(gnus-completing-read
		 "Message exists in more than one group. Choose"
		 allgroups t))
	(setq group (car allgroups))))
    (if group
	;; show article in summary buffer
	(nnmairix-show-original-article group mid)
      (nnheader-message 3 "Couldn't find original article"))))

(defun nnmairix-determine-original-group-from-registry (mid)
  "Try to determine original group for message-id MID from the registry."
  (when (gnus-bound-and-true-p 'gnus-registry-enabled)
    (unless (string-match "^<" mid)
      (set mid (concat "<" mid)))
    (unless (string-match ">$" mid)
      (set mid (concat mid ">")))
    (gnus-registry-get-id-key mid 'group)))

(defun nnmairix-determine-original-group-from-path (mid server)
  "Determine original group(s) for message-id MID from the file path.
The file path is obtained through a mairix search for the id on
SERVER."
  (nnmairix-open-server server)
  (while (string-match "[<>]" mid)
    (setq mid (replace-match "" t t mid)))
  ;; mairix somehow does not like '$' in message-id
  (when (string-match "\\$" mid)
    (setq mid (concat mid "=")))
  (while (string-match "\\$" mid)
    (setq mid (replace-match "=," t t mid)))
  (let (allgroups)
    (if (zerop (nnmairix-call-mairix-binary-raw
		(split-string nnmairix-mairix-command)
		(list (concat "m:" mid))))
	(with-current-buffer nnmairix-mairix-output-buffer
	  (goto-char (point-min))
	  (while (re-search-forward "^/.*$" nil t)
	    (push (nnmairix-get-group-from-file-path (match-string 0))
		  allgroups)
	    (forward-line 1)))
      (error "Mairix could not find original article.  See buffer %s for details"
	     nnmairix-mairix-output-buffer))
    allgroups))

(defun nnmairix-get-group-from-file-path (file)
  "Get group by parsing the message location FILE."
  (let (path filename serverbase group maildirflag allgroups)
    (string-match "^\\(.*\\)/\\(.*?\\)$" file)
    (setq path (expand-file-name (match-string 1 file)))
    (setq filename (match-string 2 file))
    ;; when we deal with maildir, remove cur/new/tmp from path
    (setq maildirflag (string-match ".+\\..+\\..+" filename))
    (when maildirflag
      (setq path
	    (replace-regexp-in-string
	     ".*\\(/cur\\|/new\\|/tmp\\)$" "" path t t 1)))
    ;; we first check nnml and nnmaildir servers
    (setq
     group
     (catch 'found
       (dolist (cur gnus-opened-servers)
	 (when (or (and (not maildirflag)
			(eq (caar cur) 'nnml))
		   (and maildirflag
			(eq (caar cur) 'nnmaildir)))
	   ;; get base path from server
	   (if maildirflag
	       (setq serverbase (cadr (assoc 'directory (car cur))))
	     (setq serverbase (cadr (assoc 'nnml-directory (car cur))))
	     (unless serverbase
	       (setq serverbase nnml-directory)))
	   (setq serverbase (file-name-as-directory
			     (expand-file-name serverbase)))
	   (when (string-match (concat serverbase "\\(.*\\)") path)
	     ;; looks good - rest of the path should be the group
	     (setq group (match-string 1 path))
	     (when (string-match "/$" group)
	       (setq group (replace-match "" t t group)))
	     (unless maildirflag
	       ;; for nnml: convert slashes to dots
	       (while (string-match "/" group)
		 (setq group (replace-match "." t t group))))
	     (setq group (gnus-group-prefixed-name group (car cur)))
	     ;; check whether this group actually exists
	     (when (gnus-group-entry group)
	       (throw 'found group)))))))
    (unless group
      ;; we haven't found it yet --> look for nnimap groups. Assume
      ;; last element of the path is the group. This might fail since
      ;; IMAP servers may present groups to the client in arbitrary
      ;; ways...
      (string-match "^.*/\\.?\\(.*\\)$" path)
      (setq group (match-string 1 path))
      ;; convert dots to slashes (nested group)
      (while (string-match "\\." group)
	(setq group (replace-match "/" t t group)))
      (dolist (cur gnus-opened-servers)
	(when (eq (caar cur) 'nnimap)
	  (when (gnus-group-entry
		 (gnus-group-prefixed-name group (car cur)))
	    (push
	     (gnus-group-prefixed-name group (car cur))
	     allgroups))))
      (if (> (length allgroups) 1)
	  (setq group (gnus-completing-read
		       "Group %s exists on more than one IMAP server. Choose"
		       allgroups t))
	(setq group (car allgroups))))
    group))

(defun nnmairix-show-original-article (group mid)
  "Switch to GROUP and display Article with message-id MID."
  (unless (string-match "^<" mid)
    (set mid (concat "<" mid)))
  (unless (string-match ">$" mid)
    (set mid (concat mid ">")))
  (when (string-match "Summary" (buffer-name (current-buffer)))
    (gnus-summary-exit))
  (pop-to-buffer gnus-group-buffer)
  (gnus-group-jump-to-group group)
  (gnus-summary-read-group group 1 t)
  (gnus-summary-refer-article mid)
  (gnus-summary-limit-to-headers (format "message-id: %s" mid))
  (gnus-summary-select-article)
  ;; Force redisplay
  (gnus-summary-show-article)
  (nnheader-message 5 "Switched to group %s." group))

(defun nnmairix-map-range (func range)
  "Map function FUNC on all members of RANGE."
  (cond
   ((numberp range)
    (funcall func range))
   (t
    (mapcar (lambda (cur)
	      (cond
	       ((listp cur)
		(cons
		 (funcall func (car cur))
		 (funcall func (cdr cur))))
	       ((numberp cur)
		(funcall func cur))))
	    range))))

(defun nnmairix-check-mairix-version (version &optional server)
  "Check mairix VERSION on SERVER.
If VERSION is a number: specifies the minimum version.
If VERSION is a string: must be contained in mairix version output."
  (unless server
    (setq server nnmairix-current-server))
  (let ((versionstring (cadr (assoc server nnmairix-version-output))))
    (unless versionstring
      ;; call "mairix -V" to get the version string
      (with-temp-buffer
	(setq versionstring
	      (let* ((commandsplit (split-string nnmairix-mairix-command))
		     (args (append (list (car commandsplit))
				  `(nil t nil) (cdr commandsplit) '("-V"))))
	      (apply 'call-process args)
	      (goto-char (point-min))
	      (re-search-forward "mairix.*")
	      (match-string 0))))
      ;; save version string for current session
      (setq nnmairix-version-output
	    (append nnmairix-version-output
		    (list (list server versionstring)))))
    (cond
     ((stringp version)
      (string-match version versionstring))
     ((numberp version)
      (<= version (string-to-number
		   (progn
		     (string-match "mairix \\([0-9\\.]+\\)" versionstring)
		     (match-string 1 versionstring))))))))

;; ==== Widget stuff

(defvar nnmairix-widgets)
(defvar nnmairix-widgets-values nil)

(defun nnmairix-widget-search-from-this-article ()
  "Create mairix query based on current article using graphical widgets."
  (interactive)
  (nnmairix-widget-search
   (nnmairix-widget-get-values)))

(defun nnmairix-widget-get-values ()
  "Create values for editable fields from current article."
  (if (not (gnus-buffer-live-p gnus-article-buffer))
      (error "No article buffer available")
    (save-excursion
      (gnus-summary-toggle-header 1)
      (set-buffer gnus-article-buffer)
      (mapcar
       (function
	(lambda (field)
	  (list (car (cddr field))
		(if (car field)
		    (nnmairix-replace-illegal-chars
		     (gnus-fetch-field (car field)))
		  nil))))
       nnmairix-widget-fields-list))))


(defun nnmairix-widget-search (&optional mvalues)
  "Create mairix query interactively using graphical widgets.
MVALUES may contain values from current article."
  (interactive)
  ;; Select window for mairix customization
  (funcall nnmairix-widget-select-window-function)
  ;; generate widgets
  (nnmairix-widget-create-query mvalues)
  ;; generate Buttons
  (widget-create 'push-button
		 :notify
		 (if mvalues
		     (lambda (&rest ignore)
		       (nnmairix-widget-send-query nnmairix-widgets
						   t))
		   (lambda (&rest ignore)
		     (nnmairix-widget-send-query nnmairix-widgets
						 nil)))
		 "Send Query")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify
		 (if mvalues
		     (lambda (&rest ignore)
		       (nnmairix-widget-create-group nnmairix-widgets
						     t))
		   (lambda (&rest ignore)
		     (nnmairix-widget-create-group nnmairix-widgets
						   nil)))
		 "Create permanent group")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer nnmairix-customize-query-buffer))
		 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun nnmairix-widget-send-query (widgets &optional withvalues)
  "Send query from WIDGETS to mairix binary.
If WITHVALUES is t, query is based on current article."
  (nnmairix-search
   (nnmairix-widget-make-query-from-widgets widgets)
   (if withvalues
       (gnus-method-to-server
	(nnmairix-backend-to-server gnus-current-select-method))
     (car (nnmairix-get-server)))
   (if (widget-value (cadr (assoc "Threads" widgets)))
       t
     -1))
  (kill-buffer nnmairix-customize-query-buffer))

(defun nnmairix-widget-create-group (widgets &optional withvalues)
  "Create nnmairix group based on current widget values WIDGETS.
If WITHVALUES is t, query is based on current article."
  (let ((group (read-string "Name of the group: ")))
    (when (not (zerop (length group)))
      (nnmairix-create-search-group
       (if withvalues
	   (gnus-method-to-server
	    (nnmairix-backend-to-server gnus-current-select-method))
	 (car (nnmairix-get-server)))
       group
       (nnmairix-widget-make-query-from-widgets widgets)
       (widget-value (cadr (assoc "Threads" widgets))))))
  (kill-buffer nnmairix-customize-query-buffer))


(defun nnmairix-widget-make-query-from-widgets (widgets)
  "Create mairix query from widget values WIDGETS."
  (let (query temp flag)
    ;; first we do the editable fields
    (dolist (cur nnmairix-widget-fields-list)
      ;; See if checkbox is checked
      (when (widget-value
	     (cadr (assoc (concat "c" (car (cddr cur))) widgets)))
	;; create query for the field
	(push
	 (concat
	  (nth 1 cur)
	  ":"
	  (nnmairix-replace-illegal-chars
	   (widget-value
	   (cadr (assoc (concat "e" (car (cddr cur))) widgets)))))
	 query)))
    ;; Flags
    (when (member 'flags nnmairix-widget-other)
      (setq flag
	    (mapconcat
	     (function
	      (lambda (flag)
		(setq temp
		      (widget-value (cadr (assoc (car flag) nnmairix-widgets))))
		(if (string= "yes" temp)
		    (cadr flag)
		  (if (string= "no" temp)
		      (concat "-" (cadr flag))))))
	     '(("seen" "s") ("replied" "r") ("flagged" "f")) ""))
      (when (not (zerop (length flag)))
	(push (concat "F:" flag) query)))
    ;; return query string
    (mapconcat 'identity query " ")))


(defun nnmairix-widget-create-query (&optional values)
  "Create widgets for creating mairix queries.
Fill in VALUES if based on an article."
  (let (allwidgets)
    (when (get-buffer nnmairix-customize-query-buffer)
      (kill-buffer nnmairix-customize-query-buffer))
    (switch-to-buffer nnmairix-customize-query-buffer)
    (kill-all-local-variables)
    (erase-buffer)
    (widget-insert "Specify your query for Mairix (check boxes for activating fields):\n\n")
    (widget-insert "(Whitespaces will be converted to ',' (i.e. AND). Use '/' for OR.)\n\n")
;    (make-local-variable 'nnmairix-widgets)
    (setq nnmairix-widgets (nnmairix-widget-build-editable-fields values))
    (when (member 'flags nnmairix-widget-other)
      (widget-insert "\nFlags:\n      Seen:     ")
      (nnmairix-widget-add "seen"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore"))
      (widget-insert "      Replied:  ")
      (nnmairix-widget-add "replied"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore"))
      (widget-insert "      Ticked:   ")
      (nnmairix-widget-add "flagged"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore")))
    (when (member 'threads nnmairix-widget-other)
      (widget-insert "\n")
      (nnmairix-widget-add "Threads" 'checkbox nil))
      (widget-insert " Show full threads\n\n")))

(defun nnmairix-widget-build-editable-fields (values)
  "Build editable field widgets in `nnmairix-widget-fields-list'.
VALUES may contain values for editable fields from current article."
  ;; how can this be done less ugly?
  (let ((ret))
    (mapc
     (function
      (lambda (field)
	(setq field (car (cddr field)))
	(setq ret
	      (nconc
	       (list
		(list
		 (concat "c" field)
		 (widget-create 'checkbox
				:tag field
				:notify (lambda (widget &rest ignore)
					  (nnmairix-widget-toggle-activate widget))
				nil)))
	       (list
		(list
		 (concat "e" field)
		 (widget-create 'editable-field
				:size 60
				:format (concat " " field ":"
						(make-string (- 11 (length field)) ?\ )
						"%v")
				:value (or (cadr (assoc field values)) ""))))
	       ret))
	(widget-insert "\n")
	;; Deactivate editable field
	(widget-apply (cadr (nth 1 ret)) :deactivate)))
     nnmairix-widget-fields-list)
    ret))

(defun nnmairix-widget-add (name &rest args)
  "Add a widget NAME with optional ARGS."
  (push
   (list name
	 (apply 'widget-create args))
   nnmairix-widgets))

(defun nnmairix-widget-toggle-activate (widget)
  "Toggle activation status of WIDGET depending on corresponding checkbox value."
  (let ((field (widget-get widget :tag)))
    (if (widget-value widget)
	(widget-apply
	 (cadr (assoc (concat "e" field) nnmairix-widgets))
	 :activate)
      (widget-apply
       (cadr (assoc (concat "e" field) nnmairix-widgets))
       :deactivate)))
  (widget-setup))

(provide 'nnmairix)

;;; nnmairix.el ends here
