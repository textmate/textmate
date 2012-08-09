;;; mairix.el --- Mairix interface for Emacs

;; Copyright (C) 2008-2012  Free Software Foundation, Inc.

;; Author: David Engster <dengste@eml.cc>
;; Keywords: mail searching

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

;; This is an interface to the mairix mail search engine.  Mairix is
;; written by Richard Curnow and is licensed under the GPL.  See the
;; home page for details:
;;
;; http://www.rpcurnow.force9.co.uk/mairix/
;;
;; Features of mairix.el:
;;
;; * Query mairix with a search term.
;; * Currently supported Emacs mail programs: RMail, Gnus (mbox only),
;;   and VM.
;; * Generate search queries using graphical widgets.
;; * Generate search queries based on currently displayed mail.
;; * Save regularly used searches in your .emacs customize section.
;; * Major mode for viewing, editing and querying saved searches.
;; * Update mairix database.
;;
;; Please note: There are currently no pre-defined key bindings, since
;; I guess these would depend on the used mail program.  See the docs
;; for an overview of the provided interactive functions.
;;
;; Attention Gnus users: If you use Gnus with maildir or nnml, you
;; should use the native Gnus back end nnmairix.el instead, since it
;; has more features and is better integrated with Gnus.  This
;; interface is essentially a stripped down version of nnmairix.el.
;;
;; Currently, RMail, Gnus (with mbox files), and VM are supported as
;; mail programs, but it is pretty easy to interface it with other
;; ones as well.  Please see the docs and the source for details.
;; In a nutshell: include your favorite mail program in
;; `mairix-mail-program' and write functions for
;; `mairix-display-functions' and `mairix-get-mail-header-functions'.
;; If you have written such functions for your Emacs mail program of
;; choice, please let me know, so that I can eventually include them
;; in future version of mairix.el.

;;; History:

;; 07/28/2008: version 0.2. Added VM interface, written by Ulrich
;; Mueller.

;; 07/14/2008: Initial release

;;; Code:

(require 'widget)
(require 'cus-edit)

(eval-when-compile
  (require 'cl))

;;; Keymappings

;; (currently none - please create them yourself)

;;; Customizable variables

(defgroup mairix nil
  "Mairix interface for Emacs."
  :group 'mail)

(defcustom mairix-file-path "~/"
  "Path where output files produced by Mairix should be stored."
  :type 'directory
  :group 'mairix)

(defcustom mairix-search-file "mairixsearch.mbox"
  "Name of the default file for storing the searches.
Note that this will be prefixed by `mairix-file-path'."
  :type 'string
  :group 'mairix)

(defcustom mairix-command "mairix"
  "Command for calling mairix.
You can add further options here if you want to, but better use
`mairix-update-options' instead."
  :type 'string
  :group 'mairix)

(defcustom mairix-output-buffer "*mairix output*"
  "Name of the buffer for the output of the mairix binary."
  :type 'string
  :group 'mairix)

(defcustom mairix-customize-query-buffer "*mairix query*"
  "Name of the buffer for customizing a search query."
  :type 'string
  :group 'mairix)

(defcustom mairix-saved-searches-buffer "*mairix searches*"
  "Name of the buffer for displaying saved searches."
  :type 'string
  :group 'mairix)

(defcustom mairix-update-options '("-F" "-Q")
  "Options when calling mairix for updating the database.
The default is '-F' and '-Q' for making updates faster.  You
should call mairix without these options from time to
time (e.g. via cron job)."
  :type '(repeat string)
  :group 'mairix)

(defcustom mairix-search-options '("-Q")
  "Options when calling mairix for searching.
The default is '-Q' for making searching faster."
  :type '(repeat string)
  :group 'mairix)

(defcustom mairix-synchronous-update nil
  "Defines if Emacs should wait for the mairix database update."
  :type 'boolean
  :group 'mairix)

(defcustom mairix-saved-searches nil
  "Saved mairix searches.
The entries are: Name of the search, Mairix query string, Name of
the file (nil: use `mairix-search-file' as default), Search whole
threads (nil or t).  Note that the file will be prefixed by
`mairix-file-path'."
  :type '(repeat (list (string :tag "Name")
		       (string :tag "Query")
		       (choice :tag "File"
			       (const :tag "default")
			       file)
		       (boolean :tag "Threads")))
  :group 'mairix)

(defcustom mairix-mail-program 'rmail
  "Mail program used to display search results.
Currently RMail, Gnus (mbox), and VM are supported.  If you use Gnus
with maildir, use nnmairix.el instead."
  :type '(choice (const :tag "RMail" rmail)
		 (const :tag "Gnus mbox" gnus)
		 (const :tag "VM" vm))
  :group 'mairix)

(defcustom mairix-display-functions
  '((rmail mairix-rmail-display)
    (gnus mairix-gnus-ephemeral-nndoc)
    (vm mairix-vm-display))
  "Specifies which function should be called for displaying search results.
This is an alist where each entry consists of a symbol from
`mairix-mail-program' and the corresponding function for
displaying the search results.  The function will be called with
the mailbox file produced by mairix as the single argument."
  :type '(repeat (list (symbol :tag "Mail program")
		       (function)))
  :group 'mairix)

(defcustom mairix-get-mail-header-functions
  '((rmail mairix-rmail-fetch-field)
    (gnus mairix-gnus-fetch-field)
    (vm mairix-vm-fetch-field))
  "Specifies function for obtaining a header field from the current mail.
This is an alist where each entry consists of a symbol from
`mairix-mail-program' and the corresponding function for
obtaining a header field from the current displayed mail.  The
function will be called with the mail header string as single
argument.  You can use nil if you do not have such a function for
your mail program, but then searches based on the current mail
won't work."
  :type '(repeat (list (symbol :tag "Mail program")
		       (choice :tag "Header function"
			       (const :tag "none")
			       function)))
  :group 'mairix)

(defcustom mairix-widget-select-window-function
  (lambda () (select-window (get-largest-window)))
  "Function for selecting the window for customizing the mairix query.
The default chooses the largest window in the current frame."
  :type 'function
  :group 'mairix)

;; Other variables

(defvar mairix-widget-fields-list
  '(("from" "f" "From") ("to" "t" "To") ("cc" "c" "Cc")
    ("subject" "s" "Subject")  ("to" "tc" "To or Cc")
    ("from" "a" "Address") (nil "b" "Body") (nil "n" "Attachment")
    ("Message-ID" "m" "Message ID") (nil "s" "Size") (nil "d" "Date"))
  "Fields that should be editable during interactive query customization.
Header, corresponding mairix command and description for editable
fields in interactive query customization.  The header specifies
which header contents should be inserted into the editable field
when creating a Mairix query based on the current message (can be
nil for disabling this).")

(defvar mairix-widget-other
  '(threads flags)
  "Other editable mairix commands when using customization widgets.
Currently there are 'threads and 'flags.")

;;;; Internal variables

(defvar mairix-last-search nil)
(defvar mairix-searches-changed nil)

;;;; Interface functions for Emacs mail programs

;;; RMail

;; Display function:
(autoload 'rmail "rmail")
(autoload 'rmail-summary-displayed "rmail")
(autoload 'rmail-summary "rmailsum")
(defvar rmail-buffer)

(defun mairix-rmail-display (folder)
  "Display mbox file FOLDER with RMail."
  (let (show-summary)
    ;; If it exists, select existing RMail window
    (when (and (boundp 'rmail-buffer)
	       rmail-buffer)
      (set-buffer rmail-buffer)
      (when (get-buffer-window rmail-buffer)
	(select-window (get-buffer-window rmail-buffer))
	(setq show-summary (rmail-summary-displayed))))
    ;; check if folder is already open and if so, kill it
    (when (get-buffer (file-name-nondirectory folder))
      (set-buffer
       (get-buffer (file-name-nondirectory folder)))
      (set-buffer-modified-p nil)
      (kill-buffer nil))
    (rmail folder)
    ;; Update summary if necessary
    (when show-summary
      (rmail-summary))))

;; Fetching mail header field:
(defun mairix-rmail-fetch-field (field)
  "Get mail header FIELD for current message using RMail."
  (unless (and (boundp 'rmail-buffer)
	       rmail-buffer)
    (error "No RMail buffer available"))
  ;; At this point, we are in rmail mode, so the rmail funcs are loaded.
  (if (fboundp 'rmail-get-header)	; Emacs 23
      (rmail-get-header field)
    (with-current-buffer rmail-buffer
      (save-restriction
	;; Don't warn about this when compiling Emacs 23.
	(with-no-warnings (rmail-narrow-to-non-pruned-header))
	(mail-fetch-field field)))))

;;; Gnus
(eval-when-compile
  (defvar gnus-article-buffer)
  (autoload 'gnus-summary-toggle-header "gnus-sum")
  (autoload 'gnus-buffer-exists-p "gnus-util")
  (autoload 'message-field-value "message")
  (autoload 'gnus-group-read-ephemeral-group "gnus-group")
  (autoload 'gnus-alive-p "gnus-util"))

;; Display function:
(defun mairix-gnus-ephemeral-nndoc (folder)
  "Create ephemeral nndoc group for reading mbox file FOLDER in Gnus."
  (unless (gnus-alive-p)
    (error "Gnus is not running"))
  (gnus-group-read-ephemeral-group
   ;; add randomness to group string to prevent Gnus from using a
   ;; cached version
   (format "mairix.%s" (number-to-string (random 10000)))
   `(nndoc "mairix"
	   (nndoc-address ,folder)
	   (nndoc-article-type mbox))))

;; Fetching mail header field:
(defun mairix-gnus-fetch-field (field)
  "Get mail header FIELD for current message using Gnus."
  (unless (gnus-alive-p)
    (error "Gnus is not running"))
  (unless (gnus-buffer-exists-p gnus-article-buffer)
    (error "No article buffer available"))
  (with-current-buffer gnus-article-buffer
    (gnus-summary-toggle-header 1)
    (message-field-value field)))

;;; VM
;;; written by Ulrich Mueller

(eval-when-compile
  (autoload 'vm-quit "vm-folder")
  (autoload 'vm-visit-folder "vm")
  (autoload 'vm-select-folder-buffer "vm-macro")
  (autoload 'vm-check-for-killed-summary "vm-misc")
  (autoload 'vm-get-header-contents "vm-summary")
  (autoload 'vm-check-for-killed-summary "vm-misc")
  (autoload 'vm-error-if-folder-empty "vm-misc")
  (autoload 'vm-select-marked-or-prefixed-messages "vm-folder"))

;; Display function
(defun mairix-vm-display (folder)
  "Display mbox file FOLDER with VM."
  (require 'vm)
  ;; check if folder is already open and if so, kill it
  (let ((buf (get-file-buffer folder)))
    (when buf
      (set-buffer buf)
      (set-buffer-modified-p nil)
      (condition-case nil
	  (vm-quit t)
	(error nil))
      (kill-buffer buf)))
  (vm-visit-folder folder t))

;; Fetching mail header field
(defun mairix-vm-fetch-field (field)
  "Get mail header FIELD for current message using VM."
  (save-excursion
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (vm-get-header-contents
     (car (vm-select-marked-or-prefixed-messages 1)) field)))

;;;; Main interactive functions

(defun mairix-search (search threads)
  "Call Mairix with SEARCH.
If THREADS is non-nil, also display whole threads of found
messages.  Results will be put into the default search file."
  (interactive
   (list
    (read-string "Query: ")
    (y-or-n-p "Include threads? ")))
  (when (mairix-call-mairix
	 (split-string search)
	 nil
	 threads)
    (mairix-show-folder mairix-search-file)))

(defun mairix-use-saved-search ()
  "Use a saved search for querying Mairix."
  (interactive)
  (let* ((completions
	  (mapcar (lambda (el) (list (car el))) mairix-saved-searches))
	 (search (completing-read "Name of search: " completions))
	 (query (assoc search mairix-saved-searches))
	 (folder (nth 2 query)))
    (when (not folder)
      (setq folder mairix-search-file))
    (when query
      (mairix-call-mairix
       (split-string (nth 1 query))
       folder
       (car (last query)))
      (mairix-show-folder folder))))

(defun mairix-save-search ()
  "Save the last search."
  (interactive)
  (let* ((name (read-string "Name of the search: "))
	 (exist (assoc name mairix-saved-searches)))
    (if (not exist)
	(add-to-list 'mairix-saved-searches
		     (append (list name) mairix-last-search))
      (when
	  (y-or-n-p
	   "There is already a search with this name.  \
Overwrite existing entry? ")
	(setcdr (assoc name mairix-saved-searches) mairix-last-search))))
  (mairix-select-save))

(defun mairix-edit-saved-searches-customize ()
  "Edit the list of saved searches in a customization buffer."
  (interactive)
  (custom-buffer-create (list (list 'mairix-saved-searches 'custom-variable))
			"*Customize Mairix Query*"
				(concat "\n\n" (make-string 65 ?=)
"\nYou can now customize your saved Mairix searches by modifying\n\
the variable mairix-saved-searches. Don't forget to save your\nchanges \
in your .emacs by pressing 'Save for Future Sessions'.\n"
(make-string 65 ?=) "\n")))

(autoload 'mail-strip-quoted-names "mail-utils")
(defun mairix-search-from-this-article (threads)
  "Search messages from sender of the current article.
This is effectively a shortcut for calling `mairix-search' with
f:current_from.  If prefix THREADS is non-nil, include whole
threads."
  (interactive "P")
  (let ((get-mail-header
	 (cadr (assq mairix-mail-program mairix-get-mail-header-functions))))
    (if get-mail-header
	(mairix-search
	 (format "f:%s"
		 (mail-strip-quoted-names
		  (funcall get-mail-header "from")))
	 threads)
      (error "No function for obtaining mail header specified"))))

(defun mairix-search-thread-this-article ()
  "Search thread for the current article.
This is effectively a shortcut for calling `mairix-search'
with m:msgid of the current article and enabled threads."
  (interactive)
  (let ((get-mail-header
	 (cadr (assq mairix-mail-program mairix-get-mail-header-functions)))
	mid)
    (unless get-mail-header
      (error "No function for obtaining mail header specified"))
    (setq mid (funcall get-mail-header "message-id"))
    (while (string-match "[<>]" mid)
      (setq mid (replace-match "" t t mid)))
    ;; mairix somehow does not like '$' in message-id
    (when (string-match "\\$" mid)
      (setq mid (concat mid "=")))
    (while (string-match "\\$" mid)
      (setq mid (replace-match "=," t t mid)))
    (mairix-search
     (format "m:%s" mid) t)))

(defun mairix-widget-search-based-on-article ()
  "Create mairix query based on current article using widgets."
  (interactive)
  (mairix-widget-search
   (mairix-widget-get-values)))

(defun mairix-edit-saved-searches ()
  "Edit current mairix searches."
  (interactive)
  (switch-to-buffer mairix-saved-searches-buffer)
  (erase-buffer)
  (setq mairix-searches-changed nil)
  (mairix-build-search-list)
  (mairix-searches-mode)
  (hl-line-mode))

(defvar mairix-widgets)

(defun mairix-widget-search (&optional mvalues)
  "Create mairix query interactively using graphical widgets.
MVALUES may contain values from current article."
  (interactive)
  ;; Select window for mairix customization
  (funcall mairix-widget-select-window-function)
  ;; generate widgets
  (mairix-widget-create-query mvalues)
  ;; generate Buttons
  (widget-create 'push-button
		 :notify
		 (lambda (&rest ignore)
		   (mairix-widget-send-query mairix-widgets))
		 "Send Query")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify
		 (lambda (&rest ignore)
		   (mairix-widget-save-search mairix-widgets))
		 "Save search")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer mairix-customize-query-buffer))
		 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun mairix-update-database ()
  "Call mairix for updating the database for SERVERS.
Mairix will be called asynchronously unless
`mairix-synchronous-update' is t.  Mairix will be called with
`mairix-update-options'."
  (interactive)
  (let ((commandsplit (split-string mairix-command))
	args)
    (if mairix-synchronous-update
	(progn
	  (setq args (append (list (car commandsplit) nil
				   (get-buffer-create mairix-output-buffer)
				   nil)))
	  (if (> (length commandsplit) 1)
	      (setq args (append args
				 (cdr commandsplit)
				 mairix-update-options))
	    (setq args (append args mairix-update-options)))
	  (apply 'call-process args))
      (progn
	(message "Updating mairix database...")
	(setq args (append (list "mairixupdate" (get-buffer-create mairix-output-buffer)
				 (car commandsplit))))
	(if (> (length commandsplit) 1)
	    (setq args (append args (cdr commandsplit) mairix-update-options))
	  (setq args (append args mairix-update-options)))
	(set-process-sentinel
	 (apply 'start-process args)
	 'mairix-sentinel-mairix-update-finished)))))


;;;; Helper functions

(defun mairix-show-folder (folder)
  "Display mail FOLDER with mail program.
The mail program is given by `mairix-mail-program'."
  (let ((display-function
	 (cadr (assq mairix-mail-program mairix-display-functions))))
    (if display-function
	(funcall display-function
		 (concat
		  (file-name-as-directory
		   (expand-file-name mairix-file-path))
		  folder))
      (error "No mail program set"))))

(defun mairix-call-mairix (query file threads)
  "Call Mairix with QUERY and output FILE.
If FILE is nil, use default.  If THREADS is non-nil, also return
whole threads.  Function returns t if messages were found."
  (let* ((commandsplit (split-string mairix-command))
	 (args (cons (car commandsplit)
		     `(nil ,(get-buffer-create mairix-output-buffer) nil)))
	 rval)
    (with-current-buffer mairix-output-buffer
      (erase-buffer))
    (when (> (length commandsplit) 1)
      (setq args (append args (cdr commandsplit))))
    (when threads
      (setq args (append args '("-t"))))
    (when (stringp query)
      (setq query (split-string query)))
    (setq mairix-last-search (list (mapconcat 'identity query " ")
				     file threads))
    (when (not file)
      (setq file mairix-search-file))
    (setq file
	  (concat
	   (file-name-as-directory
	    (expand-file-name
	     mairix-file-path))
	   file))
    (setq rval
	  (apply 'call-process
		 (append args (list "-o" file) query)))
    (if (zerop rval)
	(with-current-buffer mairix-output-buffer
	  (goto-char (point-min))
	  (re-search-forward "^Matched.*messages")
	  (message (match-string 0)))
      (if (and (= rval 1)
	       (with-current-buffer mairix-output-buffer
		 (goto-char (point-min))
		 (looking-at "^Matched 0 messages")))
	  (message "No messages found")
	(error "Error running Mairix.  See buffer %s for details"
	       mairix-output-buffer)))
    (zerop rval)))

(defun mairix-replace-invalid-chars (header)
  "Replace invalid characters in HEADER for mairix query."
  (when header
    (while (string-match "[^-.@/,^=~& [:alnum:]]" header)
      (setq header (replace-match "" t t header)))
    (while (string-match "[& ]" header)
      (setq header (replace-match "," t t header)))
  header))

(defun mairix-sentinel-mairix-update-finished (proc status)
  "Sentinel for mairix update process PROC with STATUS."
  (if (equal status "finished\n")
      (message "Updating mairix database... done")
    (error "There was an error updating the mairix database.  \
See %s for details" mairix-output-buffer)))


;;;; Widget stuff



(defun mairix-widget-send-query (widgets)
  "Send query from WIDGETS to mairix binary."
  (mairix-search
   (mairix-widget-make-query-from-widgets widgets)
   (if (widget-value (cadr (assoc "Threads" widgets))) t))
  (kill-buffer mairix-customize-query-buffer))

(defun mairix-widget-save-search (widgets)
  "Save search based on WIDGETS for future use."
  (let ((mairix-last-search
	 `( ,(mairix-widget-make-query-from-widgets widgets)
	    nil
	    ,(widget-value (cadr (assoc "Threads" widgets))))))
        (mairix-save-search)
	(kill-buffer mairix-customize-query-buffer)))

(defun mairix-widget-make-query-from-widgets (widgets)
  "Create mairix query from widget values WIDGETS."
  (let (query temp flag)
    ;; first we do the editable fields
    (dolist (cur mairix-widget-fields-list)
      ;; See if checkbox is checked
      (when (widget-value
	     (cadr (assoc (concat "c" (car (cddr cur))) widgets)))
	;; create query for the field
	(push
	 (concat
	  (nth 1 cur)
	  ":"
	  (mairix-replace-invalid-chars
	   (widget-value
	   (cadr (assoc (concat "e" (car (cddr cur))) widgets)))))
	 query)))
    ;; Flags
    (when (member 'flags mairix-widget-other)
      (setq flag
	    (mapconcat
	     (function
	      (lambda (flag)
		(setq temp
		      (widget-value (cadr (assoc (car flag) mairix-widgets))))
		(if (string= "yes" temp)
		    (cadr flag)
		  (if (string= "no" temp)
		      (concat "-" (cadr flag))))))
	     '(("seen" "s") ("replied" "r") ("flagged" "f")) ""))
      (when (not (zerop (length flag)))
	(push (concat "F:" flag) query)))
    ;; return query string
    (mapconcat 'identity query " ")))

(defun mairix-widget-create-query (&optional values)
  "Create widgets for creating mairix queries.
Fill in VALUES if based on an article."
  (let (allwidgets)
    (when (get-buffer mairix-customize-query-buffer)
      (kill-buffer mairix-customize-query-buffer))
    (switch-to-buffer mairix-customize-query-buffer)
    (kill-all-local-variables)
    (erase-buffer)
    (widget-insert
     "Specify your query for Mairix using check boxes for activating fields.\n\n")
    (widget-insert
     (concat "Use ~word        to match messages "
	     (propertize "not" 'face 'italic)
	     " containing the word)\n"
	     "    substring=   to match words containing the substring\n"
	     "    substring=N  to match words containing the substring, allowing\n"
	     "                  up to N errors(missing/extra/different letters)\n"
	     "    ^substring=  to match the substring at the beginning of a word.\n"))
    (widget-insert
     "Whitespace will be converted to ',' (i.e. AND).  Use '/' for OR.\n\n")
    (setq mairix-widgets (mairix-widget-build-editable-fields values))
    (when (member 'flags mairix-widget-other)
      (widget-insert "\nFlags:\n      Seen:     ")
      (mairix-widget-add "seen"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore"))
      (widget-insert "      Replied:  ")
      (mairix-widget-add "replied"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore"))
      (widget-insert "      Ticked:   ")
      (mairix-widget-add "flagged"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore")))
    (when (member 'threads mairix-widget-other)
      (widget-insert "\n")
      (mairix-widget-add "Threads" 'checkbox nil))
      (widget-insert " Show full threads\n\n")))

(defun mairix-widget-build-editable-fields (values)
  "Build editable field widgets in `nnmairix-widget-fields-list'.
VALUES may contain values for editable fields from current article."
  (let ((ret))
    (mapc
     (function
      (lambda (field)
	(setq field (car (cddr field)))
	(setq
	 ret
	 (nconc
	  (list
	   (list
	    (concat "c" field)
	    (widget-create 'checkbox
			   :tag field
			   :notify (lambda (widget &rest ignore)
				     (mairix-widget-toggle-activate widget))
			   nil)))
	  (list
	   (list
	    (concat "e" field)
	    (widget-create 'editable-field
			   :size 60
			   :format (concat " " field ":"
					   (make-string
					    (- 11 (length field)) ?\ )
					   "%v")
			   :value (or (cadr (assoc field values)) ""))))
	  ret))
	(widget-insert "\n")
	;; Deactivate editable field
	(widget-apply (cadr (nth 1 ret)) :deactivate)))
     mairix-widget-fields-list)
    ret))

(defun mairix-widget-add (name &rest args)
  "Add a widget NAME with optional ARGS."
  (push
   (list name
	 (apply 'widget-create args))
   mairix-widgets))

(defun mairix-widget-toggle-activate (widget)
  "Toggle activation status of WIDGET depending on checkbox value."
  (let ((field (widget-get widget :tag)))
    (if (widget-value widget)
	(widget-apply
	 (cadr (assoc (concat "e" field) mairix-widgets))
	 :activate)
      (widget-apply
       (cadr (assoc (concat "e" field) mairix-widgets))
       :deactivate)))
  (widget-setup))


;;;; Major mode for editing/deleting/saving searches

(defvar mairix-searches-mode-map
  (let ((map (make-keymap)))
    (define-key map [(return)] 'mairix-select-search)
    (define-key map [(down)] 'mairix-next-search)
    (define-key map [(up)] 'mairix-previous-search)
    (define-key map [(right)] 'mairix-next-search)
    (define-key map [(left)] 'mairix-previous-search)
    (define-key map "\C-p" 'mairix-previous-search)
    (define-key map "\C-n" 'mairix-next-search)
    (define-key map [(q)] 'mairix-select-quit)
    (define-key map [(e)] 'mairix-select-edit)
    (define-key map [(d)] 'mairix-select-delete)
    (define-key map [(s)] 'mairix-select-save)
    map)
  "'mairix-searches-mode' keymap.")

(defvar mairix-searches-mode-font-lock-keywords)

(defun mairix-searches-mode ()
  "Major mode for editing mairix searches."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mairix-searches-mode)
  (setq mode-name "mairix-searches")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map mairix-searches-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq mairix-searches-mode-font-lock-keywords
	(list (list "^\\([0-9]+\\)"
		    '(1 font-lock-constant-face))
	      (list "^[0-9 ]+\\(Name:\\) \\(.*\\)"
		    '(1 font-lock-keyword-face) '(2 font-lock-string-face))
	      (list "^[ ]+\\(Query:\\) \\(.*\\) , "
		    '(1 font-lock-keyword-face) '(2 font-lock-string-face))
	      (list ", \\(Threads:\\) \\(.*\\)"
		    '(1 font-lock-keyword-face) '(2 font-lock-constant-face))
	      (list "^\\([A-Z].*\\)$"
		    '(1 font-lock-comment-face))
	      (list "^[ ]+\\(Folder:\\) \\(.*\\)"
		    '(1 font-lock-keyword-face) '(2 font-lock-string-face))))
  (setq font-lock-defaults '(mairix-searches-mode-font-lock-keywords)))

(defun mairix-build-search-list ()
  "Display saved searches in current buffer."
  (insert "These are your current saved mairix searches.\n\
You may use the following keys in this buffer: \n\
Return: execute search, e: edit, d: delete, s: save, q: quit\n\
Use cursor keys or C-n,C-p to select next/previous search.\n\n")
  (let ((num 0)
	(beg (point))
	current)
    (while (< num (length mairix-saved-searches))
      (setq current (nth num mairix-saved-searches))
      (setq num (1+ num))
      (mairix-insert-search-line num current)
      (insert "\n"))
    (goto-char beg)))

(defun mairix-insert-search-line (number field)
  "Insert new mairix query with NUMBER and values FIELD in buffer."
  (insert
   (format "%d Name: %s\n   Query: %s , Threads: %s\n   Folder: %s\n"
	   number
	   (car field)
	   (nth 1 field)
	   (if (nth 3 field)
	       "Yes"
	     "No")
	   (if (nth 2 field)
	       (nth 2 field)
	     "Default"))))

(defun mairix-select-search ()
  "Call mairix with currently selected search."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "[0-9]+ Name"))
      (progn
	(ding)
	(message "Put cursor on a line with a search name first"))
    (progn
      (let* ((query (nth
		    (1- (read (current-buffer)))
		    mairix-saved-searches))
	    (folder (nth 2 query)))
	(when (not folder)
	  (setq folder mairix-search-file))
	(mairix-call-mairix
	 (split-string (nth 1 query))
	 folder
	 (car (last query)))
	(mairix-select-quit)
	(mairix-show-folder folder)))))

(defun mairix-next-search ()
  "Jump to next search."
  (interactive)
  (if (search-forward-regexp "^[0-9]+"
			     (point-max)
			     t
			     2)
      (beginning-of-line)
    (ding)))

(defun mairix-previous-search ()
  "Jump to previous search."
  (interactive)
  (if (search-backward-regexp "^[0-9]+"
			     (point-min)
			     t)
      (beginning-of-line)
    (ding)))

(defun mairix-select-quit ()
  "Quit mairix search mode."
  (interactive)
  (when mairix-searches-changed
    (mairix-select-save))
  (kill-buffer nil))

(defun mairix-select-save ()
  "Save current mairix searches."
  (interactive)
  (when (y-or-n-p "Save mairix searches permanently in your .emacs? ")
    (customize-save-variable 'mairix-saved-searches mairix-saved-searches)))

(defun mairix-select-edit ()
  "Edit currently selected mairix search."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "[0-9]+ Name"))
      (error "Put cursor on a line with a search name first")
    (progn
      (let* ((number (1- (read (current-buffer))))
	     (query (nth number mairix-saved-searches))
	     (folder (nth 2 query))
	     newname newquery newfolder threads)
	(backward-char)
	(setq newname (read-string "Name of the search: " (car query)))
	(when (assoc newname (remq (nth number mairix-saved-searches)
				   mairix-saved-searches))
	  (error "This name does already exist"))
	(setq newquery (read-string "Query: " (nth 1 query)))
	(setq threads (y-or-n-p "Include whole threads? "))
	(setq newfolder
	      (read-string "Mail folder (use empty string for default): "
			   folder))
	(when (zerop (length newfolder))
	  (setq newfolder nil))
	;; set new values
	(setcar (nth number mairix-saved-searches) newname)
	(setcdr (nth number mairix-saved-searches)
		(list newquery newfolder threads))
	(setq mairix-searches-changed t)
	(let ((beg (point)))
	  (forward-line 3)
	  (end-of-line)
	  (delete-region beg (point))
	  (mairix-insert-search-line (1+ number)
				     (nth number mairix-saved-searches))
	  (goto-char beg))))))

(defun mairix-select-delete ()
  "Delete currently selected mairix search."
  (interactive)
  (if (not (looking-at "[0-9]+ Name"))
      (error "Put cursor on a line with a search name first")
    (progn
      (let* ((number (1- (read (current-buffer))))
	     (query (nth number mairix-saved-searches))
	     beg)
	(backward-char)
	(when (y-or-n-p (format "Delete search %s ? " (car query)))
	  (setq mairix-saved-searches
		(delq query mairix-saved-searches))
	  (setq mairix-searches-changed t)
	  (setq beg (point))
	  (forward-line 4)
	  (beginning-of-line)
	  (delete-region beg (point))
	  (while (search-forward-regexp "^[0-9]+"
					(point-max)
					t
					1)
	    (replace-match (number-to-string
			    (setq number (1+ number)))))))
      (beginning-of-line))))

(defun mairix-widget-get-values ()
  "Create values for editable fields from current article."
  (let ((get-mail-header
	 (cadr (assq mairix-mail-program mairix-get-mail-header-functions))))
    (if get-mail-header
	(save-excursion
	  (save-restriction
	    (mapcar
	     (function
	      (lambda (field)
		(list (car (cddr field))
		      (if (car field)
			  (mairix-replace-invalid-chars
			   (funcall get-mail-header (car field)))
			nil))))
	     mairix-widget-fields-list)))
      (error "No function for obtaining mail header specified"))))


(provide 'mairix)

;;; mairix.el ends here
