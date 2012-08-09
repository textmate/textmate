;;; org-feed.el --- Add RSS feed items to Org files
;;
;; Copyright (C) 2009-2012 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This module allows to create and change entries in an Org-mode
;;  file triggered by items in an RSS feed.  The basic functionality is
;;  geared toward simply adding new items found in a feed as outline nodes
;;  to an Org file.  Using hooks, arbitrary actions can be triggered for
;;  new or changed items.
;;
;;  Selecting feeds and target locations
;;  ------------------------------------
;;
;;  This module is configured through a single variable, `org-feed-alist'.
;;  Here is an example, using a notes/tasks feed from reQall.com.
;;
;;    (setq org-feed-alist
;;          '(("ReQall"
;;             "http://www.reqall.com/user/feeds/rss/a1b2c3....."
;;             "~/org/feeds.org" "ReQall Entries")
;;
;;  With this setup, the command `M-x org-feed-update-all' will
;;  collect new entries in the feed at the given URL and create
;;  entries as subheadings under the "ReQall Entries" heading in the
;;  file "~/org/feeds.org".  Each feed should normally have its own
;;  heading - however see the `:drawer' parameter.
;;
;;  Besides these standard elements that need to be specified for each
;;  feed, keyword-value pairs can set additional options.  For example,
;;  to de-select transitional entries with a title containing
;;
;;                   "reQall is typing what you said",
;;
;;  you could use the `:filter' argument:
;;
;;    (setq org-feed-alist
;;          '(("ReQall"
;;             "http://www.reqall.com/user/feeds/rss/a1b2c3....."
;;             "~/org/feeds.org" "ReQall Entries"
;;             :filter my-reqall-filter)))
;;
;;    (defun my-reqall-filter (e)
;;       (if (string-match "reQall is typing what you said"
;;                         (plist-get e :title))
;;           nil
;;         e))
;;
;;  See the docstring for `org-feed-alist' for more details.
;;
;;
;;  Keeping track of previously added entries
;;  -----------------------------------------
;;
;;  Since Org allows you to delete, archive, or move outline nodes,
;;  org-feed.el needs to keep track of which feed items have been handled
;;  before, so that they will not be handled again.  For this, org-feed.el
;;  stores information in a special drawer, FEEDSTATUS, under the heading
;;  that received the input of the feed.  You should add FEEDSTATUS
;;  to your list of drawers in the files that receive feed input:
;;
;;       #+DRAWERS: PROPERTIES LOGBOOK FEEDSTATUS
;;
;;  Acknowledgments
;;  ---------------
;;
;;  org-feed.el is based on ideas by Brad Bozarth who implemented a
;;  similar mechanism using shell and awk scripts.

;;; Code:

(require 'org)
(require 'sha1)

(declare-function url-retrieve-synchronously "url" (url))
(declare-function xml-node-children "xml" (node))
(declare-function xml-get-children "xml" (node child-name))
(declare-function xml-get-attribute "xml" (node attribute))
(declare-function xml-get-attribute-or-nil "xml" (node attribute))
(declare-function xml-substitute-special "xml" (string))

(defgroup org-feed  nil
  "Options concerning RSS feeds as inputs for Org files."
  :tag "Org Feed"
  :group 'org)

(defcustom org-feed-alist nil
  "Alist specifying RSS feeds that should create inputs for Org.
Each entry in this list specified an RSS feed tat should be queried
to create inbox items in Org.  Each entry is a list with the following items:

name         a custom name for this feed
URL          the Feed URL
file         the target Org file where entries should be listed
headline     the headline under which entries should be listed

Additional arguments can be given using keyword-value pairs.  Many of these
specify functions that receive one or a list of \"entries\" as their single
argument.  An entry is a property list that describes a feed item.  The
property list has properties for each field in the item, for example `:title'
for the `<title>' field and `:pubDate' for the publication date.  In addition,
it contains the following properties:

`:item-full-text'   the full text in the <item> tag
`:guid-permalink'   t when the guid property is a permalink

Here are the keyword-value pair allows in `org-feed-alist'.

:drawer drawer-name
     The name of the drawer for storing feed information.  The default is
     \"FEEDSTATUS\".  Using different drawers for different feeds allows
     several feeds to target the same inbox heading.

:filter filter-function
     A function to select interesting entries in the feed.  It gets a single
     entry as parameter.  It should return the entry if it is relevant, or
     nil if it is not.

:template template-string
     The default action on new items in the feed is to add them as children
     under the headline for the feed.  The template describes how the entry
     should be formatted.  If not given, it defaults to
     `org-feed-default-template'.

:formatter formatter-function
     Instead of relying on a template, you may specify a function to format
     the outline node to be inserted as a child.  This function gets passed
     a property list describing a single feed item, and it should return a
     string that is a properly formatted Org outline node of level 1.

:new-handler function
     If adding new items as children to the outline is not what you want
     to do with new items, define a handler function that is called with
     a list of all new items in the feed, each one represented as a property
     list.  The handler should do what needs to be done, and org-feed will
     mark all items given to this handler as \"handled\", i.e. they will not
     be passed to this handler again in future readings of the feed.
     When the handler is called, point will be at the feed headline.

:changed-handler function
     This function gets passed a list of all entries that have been
     handled before, but are now still in the feed and have *changed*
     since last handled (as evidenced by a different sha1 hash).
     When the handler is called, point will be at the feed headline.

:parse-feed function
     This function gets passed a buffer, and should return a list
     of entries, each being a property list containing the
     `:guid' and `:item-full-text' keys.  The default is
     `org-feed-parse-rss-feed'; `org-feed-parse-atom-feed' is an
     alternative.

:parse-entry function
     This function gets passed an entry as returned by the parse-feed
     function, and should return the entry with interesting properties added.
     The default is `org-feed-parse-rss-entry'; `org-feed-parse-atom-entry'
     is an alternative."
  :group 'org-feed
  :type '(repeat
	  (list :value ("" "http://" "" "")
	   (string :tag "Name")
	   (string :tag "Feed URL")
	   (file :tag "File for inbox")
	   (string :tag "Headline for inbox")
	   (repeat :inline t
		   (choice
		    (list :inline t :tag "Filter"
			  (const :filter)
			  (symbol :tag "Filter Function"))
		    (list :inline t :tag "Template"
			  (const :template)
			  (string :tag "Template"))
		    (list :inline t :tag "Formatter"
			  (const :formatter)
			  (symbol :tag "Formatter Function"))
		    (list :inline t :tag "New items handler"
			  (const :new-handler)
			  (symbol :tag "Handler Function"))
		    (list :inline t :tag "Changed items"
			  (const :changed-handler)
			  (symbol :tag "Handler Function"))
		    (list :inline t :tag "Parse Feed"
			  (const :parse-feed)
			  (symbol :tag "Parse Feed Function"))
		    (list :inline t :tag "Parse Entry"
			  (const :parse-entry)
			  (symbol :tag "Parse Entry Function"))
		    )))))

(defcustom org-feed-drawer "FEEDSTATUS"
  "The name of the drawer for feed status information.
Each feed may also specify its own drawer name using the `:drawer'
parameter in `org-feed-alist'.
Note that in order to make these drawers behave like drawers, they must
be added to the variable `org-drawers' or configured with a #+DRAWERS
line."
  :group 'org-feed
  :type '(string :tag "Drawer Name"))

(defcustom org-feed-default-template "\n* %h\n  %U\n  %description\n  %a\n"
  "Template for the Org node created from RSS feed items.
This is just the default, each feed can specify its own.
Any fields from the feed item can be interpolated into the template with
%name, for example %title, %description, %pubDate etc.  In addition, the
following special escapes are valid as well:

%h      the title, or the first line of the description
%t      the date as a stamp, either from <pubDate> (if present), or
        the current date.
%T      date and time
%u,%U   like %t,%T, but inactive time stamps
%a      A link, from <guid> if that is a permalink, else from <link>"
  :group 'org-feed
  :type '(string :tag "Template"))

(defcustom org-feed-save-after-adding t
  "Non-nil means save buffer after adding new feed items."
  :group 'org-feed
  :type 'boolean)

(defcustom org-feed-retrieve-method 'url-retrieve-synchronously
  "The method to be used to retrieve a feed URL.
This can be `curl' or `wget' to call these external programs, or it can be
an Emacs Lisp function that will return a buffer containing the content
of the file pointed to by the URL."
  :group 'org-feed
  :type '(choice
	  (const :tag "Internally with url.el" url-retrieve-synchronously)
	  (const :tag "Externally with curl" curl)
	  (const :tag "Externally with wget" wget)
	  (function :tag "Function")))

 (defcustom org-feed-before-adding-hook nil
  "Hook that is run before adding new feed items to a file.
You might want to commit the file in its current state to version control,
for example."
  :group 'org-feed
  :type 'hook)

(defcustom org-feed-after-adding-hook nil
  "Hook that is run after new items have been added to a file.
Depending on `org-feed-save-after-adding', the buffer will already
have been saved."
  :group 'org-feed
  :type 'hook)

(defvar org-feed-buffer "*Org feed*"
  "The buffer used to retrieve a feed.")

;;;###autoload
(defun org-feed-update-all ()
  "Get inbox items from all feeds in `org-feed-alist'."
  (interactive)
  (let ((nfeeds (length org-feed-alist))
	(nnew (apply '+  (mapcar 'org-feed-update org-feed-alist))))
    (message "%s from %d %s"
	     (cond ((= nnew 0) "No new entries")
		   ((= nnew 1) "1 new entry")
		   (t (format "%d new entries" nnew)))
	     nfeeds
	     (if (= nfeeds 1) "feed" "feeds"))))

;;;###autoload
(defun org-feed-update (feed &optional retrieve-only)
  "Get inbox items from FEED.
FEED can be a string with an association in `org-feed-alist', or
it can be a list structured like an entry in `org-feed-alist'."
  (interactive (list (org-completing-read "Feed name: " org-feed-alist)))
  (if (stringp feed) (setq feed (assoc feed org-feed-alist)))
  (unless feed
    (error "No such feed in `org-feed-alist"))
  (catch 'exit
    (let ((name (car feed))
	  (url (nth 1 feed))
	  (file (nth 2 feed))
	  (headline (nth 3 feed))
	  (filter (nth 1 (memq :filter feed)))
	  (formatter (nth 1 (memq :formatter feed)))
	  (new-handler (nth 1 (memq :new-handler feed)))
	  (changed-handler (nth 1 (memq :changed-handler feed)))
	  (template (or (nth 1 (memq :template feed))
			org-feed-default-template))
	  (drawer (or (nth 1 (memq :drawer feed))
		      org-feed-drawer))
	  (parse-feed (or (nth 1 (memq :parse-feed feed))
			  'org-feed-parse-rss-feed))
	  (parse-entry (or (nth 1 (memq :parse-entry feed))
			   'org-feed-parse-rss-entry))
	  feed-buffer inbox-pos new-formatted
	  entries old-status status new changed guid-alist e guid olds)
      (setq feed-buffer (org-feed-get-feed url))
      (unless (and feed-buffer (bufferp (get-buffer feed-buffer)))
	(error "Cannot get feed %s" name))
      (when retrieve-only
	(throw 'exit feed-buffer))
      (setq entries (funcall parse-feed feed-buffer))
      (ignore-errors (kill-buffer feed-buffer))
      (save-excursion
	(save-window-excursion
	  (setq inbox-pos (org-feed-goto-inbox-internal file headline))
	  (setq old-status (org-feed-read-previous-status inbox-pos drawer))
	  ;; Add the "handled" status to the appropriate entries
	  (setq entries (mapcar (lambda (e)
				  (setq e
					(plist-put e :handled
						   (nth 1 (assoc
							   (plist-get e :guid)
							   old-status)))))
				entries))
	  ;; Find out which entries are new and which are changed
	  (dolist (e entries)
	    (if (not (plist-get e :handled))
		(push e new)
	      (setq olds (nth 2 (assoc (plist-get e :guid) old-status)))
	      (if (and olds
		       (not (string= (sha1
				      (plist-get e :item-full-text))
				     olds)))
		  (push e changed))))

	  ;; Parse the relevant entries fully
	  (setq new     (mapcar parse-entry new)
		changed (mapcar parse-entry changed))

	  ;; Run the filter
	  (when filter
	    (setq new     (delq nil (mapcar filter new))
		  changed (delq nil (mapcar filter new))))

	  (when (not (or new changed))
	    (message "No new items in feed %s" name)
	    (throw 'exit 0))

	  ;; Get alist based on guid, to look up entries
	  (setq guid-alist
		(append
		 (mapcar (lambda (e) (list (plist-get e :guid) e)) new)
		 (mapcar (lambda (e) (list (plist-get e :guid) e)) changed)))

	  ;; Construct the new status
	  (setq status
		(mapcar
		 (lambda (e)
		   (setq guid (plist-get e :guid))
		   (list guid
			 ;; things count as handled if we handle them now,
			 ;; or if they were handled previously
			 (if (assoc guid guid-alist) t (plist-get e :handled))
			 ;; A hash, to detect changes
			 (sha1 (plist-get e :item-full-text))))
		 entries))

	  ;; Handle new items in the feed
	  (when new
	    (if new-handler
		(progn
		  (goto-char inbox-pos)
		  (funcall new-handler new))
	      ;; No custom handler, do the default adding
	      ;; Format the new entries into an alist with GUIDs in the car
	      (setq new-formatted
		    (mapcar
		     (lambda (e) (org-feed-format-entry e template formatter))
		     new)))

	    ;; Insert the new items
	    (org-feed-add-items inbox-pos new-formatted))

	  ;; Handle changed items in the feed
	  (when (and changed-handler changed)
	    (goto-char inbox-pos)
	    (funcall changed-handler changed))

	  ;; Write the new status
	  ;; We do this only now, in case something goes wrong above, so
	  ;; that would would end up with a status that does not reflect
	  ;; which items truely have been handled
	  (org-feed-write-status inbox-pos drawer status)

	  ;; Normalize the visibility of the inbox tree
	  (goto-char inbox-pos)
	  (hide-subtree)
	  (show-children)
	  (org-cycle-hide-drawers 'children)

	  ;; Hooks and messages
	  (when org-feed-save-after-adding (save-buffer))
	  (message "Added %d new item%s from feed %s to file %s, heading %s"
		   (length new) (if (> (length new) 1) "s" "")
		   name
		   (file-name-nondirectory file) headline)
	  (run-hooks 'org-feed-after-adding-hook)
	  (length new))))))

;;;###autoload
(defun org-feed-goto-inbox (feed)
  "Go to the inbox that captures the feed named FEED."
  (interactive
   (list (if (= (length org-feed-alist) 1)
	     (car org-feed-alist)
	   (org-completing-read "Feed name: " org-feed-alist))))
  (if (stringp feed) (setq feed (assoc feed org-feed-alist)))
  (unless feed
    (error "No such feed in `org-feed-alist"))
  (org-feed-goto-inbox-internal (nth 2 feed) (nth 3 feed)))

;;;###autoload
(defun org-feed-show-raw-feed (feed)
  "Show the raw feed buffer of a feed."
  (interactive
   (list (if (= (length org-feed-alist) 1)
	     (car org-feed-alist)
	   (org-completing-read "Feed name: " org-feed-alist))))
  (if (stringp feed) (setq feed (assoc feed org-feed-alist)))
  (unless feed
    (error "No such feed in `org-feed-alist"))
  (org-pop-to-buffer-same-window
   (org-feed-update feed 'retrieve-only))
  (goto-char (point-min)))

(defun org-feed-goto-inbox-internal (file heading)
  "Find or create HEADING in FILE.
Switch to that buffer, and return the position of that headline."
  (find-file file)
  (widen)
  (goto-char (point-min))
  (if (re-search-forward
       (concat "^\\*+[ \t]+" heading "[ \t]*\\(:.*?:[ \t]*\\)?$")
       nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))
      (insert "\n\n* " heading "\n\n")
      (org-back-to-heading t))
  (point))

(defun org-feed-read-previous-status (pos drawer)
  "Get the alist of old GUIDs from the entry at POS.
This will find DRAWER and extract the alist."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (if (re-search-forward
	   (concat "^[ \t]*:" drawer ":[ \t]*\n\\([^\000]*?\\)\n[ \t]*:END:")
	   end t)
	  (read (match-string 1))
	nil))))

(defun org-feed-write-status (pos drawer status)
  "Write the feed STATUS to DRAWER in entry at POS."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t)))
	  guid)
      (if (re-search-forward (concat "^[ \t]*:" drawer ":[ \t]*\n")
			     end t)
	  (progn
	    (goto-char (match-end 0))
	    (delete-region (point)
			   (save-excursion
			     (and (re-search-forward "^[ \t]*:END:" nil t)
				  (match-beginning 0)))))
	(outline-next-heading)
	(insert "  :" drawer ":\n  :END:\n")
	(beginning-of-line 0))
      (insert (pp-to-string status)))))

(defun org-feed-add-items (pos entries)
  "Add the formatted items to the headline as POS."
  (let (entry level)
    (save-excursion
      (goto-char pos)
      (unless (looking-at org-complex-heading-regexp)
	(error "Wrong position"))
      (setq level (org-get-valid-level (length (match-string 1)) 1))
      (org-end-of-subtree t t)
      (skip-chars-backward " \t\n")
      (beginning-of-line 2)
      (setq pos (point))
      (while (setq entry (pop entries))
	(org-paste-subtree level entry 'yank))
      (org-mark-ring-push pos))))

(defun org-feed-format-entry (entry template formatter)
  "Format ENTRY so that it can be inserted into an Org file.
ENTRY is a property list.  This function adds a `:formatted-for-org' property
and returns the full property list.
If that property is already present, nothing changes."
  (if formatter
      (funcall formatter entry)
    (let (dlines fmt tmp indent time name
		 v-h v-t v-T v-u v-U v-a)
      (setq dlines (org-split-string (or (plist-get entry :description) "???")
				     "\n")
	    v-h (or (plist-get entry :title) (car dlines) "???")
	    time (or (if (plist-get entry :pubDate)
			 (org-read-date t t (plist-get entry :pubDate)))
		     (current-time))
	    v-t (format-time-string (org-time-stamp-format nil nil) time)
	    v-T (format-time-string (org-time-stamp-format t   nil) time)
	    v-u (format-time-string (org-time-stamp-format nil t)   time)
	    v-U (format-time-string (org-time-stamp-format t   t)   time)
	    v-a (if (setq tmp (or (and (plist-get entry :guid-permalink)
				       (plist-get entry :guid))
				  (plist-get entry :link)))
		    (concat "[[" tmp "]]\n")
		  ""))
      (with-temp-buffer
	(insert template)
	(goto-char (point-min))
	(while (re-search-forward "%\\([a-zA-Z]+\\)" nil t)
	  (setq name (match-string 1))
	  (cond
	   ((member name '("h" "t" "T" "u" "U" "a"))
	    (replace-match (symbol-value (intern (concat "v-" name))) t t))
	   ((setq tmp (plist-get entry (intern (concat ":" name))))
	    (save-excursion
	      (save-match-data
		(beginning-of-line 1)
		(when (looking-at (concat "^\\([ \t]*\\)%" name "[ \t]*$"))
		  (setq tmp (org-feed-make-indented-block
			     tmp (org-get-indentation))))))
	    (replace-match tmp t t))))
	(decode-coding-string
	 (buffer-string) (detect-coding-region (point-min) (point-max) t))))))

(defun org-feed-make-indented-block (s n)
  "Add indentation of N spaces to a multiline string S."
  (if (not (string-match "\n" s))
      s
    (mapconcat 'identity
	       (org-split-string s "\n")
	       (concat "\n" (make-string n ?\ )))))

(defun org-feed-skip-http-headers (buffer)
  "Remove HTTP headers from BUFFER, and return it.
Assumes headers are indeed present!"
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    buffer))

(defun org-feed-get-feed (url)
  "Get the RSS feed file at URL and return the buffer."
  (cond
   ((eq org-feed-retrieve-method 'url-retrieve-synchronously)
    (org-feed-skip-http-headers (url-retrieve-synchronously url)))
   ((eq org-feed-retrieve-method 'curl)
    (ignore-errors (kill-buffer org-feed-buffer))
    (call-process "curl" nil org-feed-buffer nil "--silent" url)
    org-feed-buffer)
   ((eq org-feed-retrieve-method 'wget)
    (ignore-errors (kill-buffer org-feed-buffer))
    (call-process "wget" nil org-feed-buffer nil "-q" "-O" "-" url)
    org-feed-buffer)
   ((functionp org-feed-retrieve-method)
    (funcall org-feed-retrieve-method url))))

(defun org-feed-parse-rss-feed (buffer)
  "Parse BUFFER for RSS feed entries.
Returns a list of entries, with each entry a property list,
containing the properties `:guid' and `:item-full-text'."
  (let ((case-fold-search t)
	entries beg end item guid entry)
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "<item\\>.*?>" nil t)
	(setq beg (point)
	      end (and (re-search-forward "</item>" nil t)
		       (match-beginning 0)))
	(setq item (buffer-substring beg end)
	      guid (if (string-match "<guid\\>.*?>\\(.*?\\)</guid>" item)
		       (org-match-string-no-properties 1 item)))
	(setq entry (list :guid guid :item-full-text item))
	(push entry entries)
	(widen)
	(goto-char end))
      (nreverse entries))))

(defun org-feed-parse-rss-entry (entry)
  "Parse the `:item-full-text' field for xml tags and create new properties."
  (require 'xml)
  (with-temp-buffer
    (insert (plist-get entry :item-full-text))
    (goto-char (point-min))
    (while (re-search-forward "<\\([a-zA-Z]+\\>\\).*?>\\([^\000]*?\\)</\\1>"
			      nil t)
      (setq entry (plist-put entry
			     (intern (concat ":" (match-string 1)))
			     (xml-substitute-special (match-string 2)))))
    (goto-char (point-min))
    (unless (re-search-forward "isPermaLink[ \t]*=[ \t]*\"false\"" nil t)
      (setq entry (plist-put entry :guid-permalink t))))
  entry)

(defun org-feed-parse-atom-feed (buffer)
  "Parse BUFFER for Atom feed entries.
Returns a list of entries, with each entry a property list,
containing the properties `:guid' and `:item-full-text'.

The `:item-full-text' property actually contains the sexp
formatted as a string, not the original XML data."
  (require 'xml)
  (with-current-buffer buffer
    (widen)
    (let ((feed (car (xml-parse-region (point-min) (point-max)))))
      (mapcar
       (lambda (entry)
	 (list
	  :guid (car (xml-node-children (car (xml-get-children entry 'id))))
	  :item-full-text (prin1-to-string entry)))
       (xml-get-children feed 'entry)))))

(defun org-feed-parse-atom-entry (entry)
  "Parse the `:item-full-text' as a sexp and create new properties."
  (let ((xml (car (read-from-string (plist-get entry :item-full-text)))))
    ;; Get first <link href='foo'/>.
    (setq entry (plist-put entry :link
			   (xml-get-attribute
			    (car (xml-get-children xml 'link))
			    'href)))
    ;; Add <title/> as :title.
    (setq entry (plist-put entry :title
			   (xml-substitute-special
			    (car (xml-node-children
				  (car (xml-get-children xml 'title)))))))
    (let* ((content (car (xml-get-children xml 'content)))
	   (type (xml-get-attribute-or-nil content 'type)))
      (when content
	(cond
	 ((string= type "text")
	  ;; We like plain text.
	  (setq entry (plist-put entry :description
				 (xml-substitute-special
				  (car (xml-node-children content))))))
	 ((string= type "html")
	  ;; TODO: convert HTML to Org markup.
	  (setq entry (plist-put entry :description
				 (xml-substitute-special
				  (car (xml-node-children content))))))
	 ((string= type "xhtml")
	  ;; TODO: convert XHTML to Org markup.
	  (setq entry (plist-put entry :description
				 (prin1-to-string
				  (xml-node-children content)))))
	 (t
	  (setq entry (plist-put entry :description
				 (format "Unknown '%s' content." type)))))))
    entry))

(provide 'org-feed)

;;; org-feed.el ends here
