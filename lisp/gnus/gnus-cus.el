;;; gnus-cus.el --- customization commands for Gnus

;; Copyright (C) 1996, 1999-2012 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
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

(require 'wid-edit)
(require 'gnus)
(require 'gnus-agent)
(require 'gnus-score)
(require 'gnus-topic)
(require 'gnus-art)

;;; Widgets:

(defun gnus-custom-mode ()
  "Major mode for editing Gnus customization buffers.

The following commands are available:

\\[widget-forward]		Move to next button or editable field.
\\[widget-backward]		Move to previous button or editable field.
\\[widget-button-click]		Activate button under the mouse pointer.
\\[widget-button-press]		Activate button under point.

Entry to this mode calls the value of `gnus-custom-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'gnus-custom-mode
	mode-name "Gnus Customize")
  (use-local-map widget-keymap)
  ;; Emacs stuff:
  (when (and (facep 'custom-button-face)
	     (facep 'custom-button-pressed-face))
    (set (make-local-variable 'widget-button-face)
	 'custom-button-face)
    (set (make-local-variable 'widget-button-pressed-face)
	 'custom-button-pressed-face)
    (set (make-local-variable 'widget-mouse-face)
	 'custom-button-pressed-face))
  (when (and (boundp 'custom-raised-buttons)
	     (symbol-value 'custom-raised-buttons))
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))
  (gnus-run-mode-hooks 'gnus-custom-mode-hook))

;;; Group Customization:

(defconst gnus-group-parameters
  '((extra-aliases (choice
		    :tag "Extra Aliases"
		    (list
		     :tag "List"
		     (editable-list
		      :inline t
		      (gnus-email-address :tag "Address")))
		    (gnus-email-address :tag "Address")) "\
Store messages posted from or to this address in this group.

You must be using gnus-group-split for this to work.  The VALUE of the
nnmail-split-fancy SPLIT generated for this group will match these
addresses.")

    (split-regexp (regexp :tag "gnus-group-split Regular Expression") "\
Like gnus-group-split Address, but expects a regular expression.")

    (split-exclude (list :tag "gnus-group-split Restricts"
			 (editable-list
			  :inline t (regexp :tag "Restrict"))) "\
Regular expression that cancels gnus-group-split matches.

Each entry is added to the nnmail-split-fancy SPLIT as a separate
RESTRICT clause.")

    (split-spec (choice :tag "gnus-group-split Overrider"
			(sexp :tag "Fancy Split")
			(const :tag "Catch All" catch-all)
			(const :tag "Ignore" nil)) "\
Override all other gnus-group-split fields.

In `Fancy Split', you can enter any nnmail-split-fancy SPLIT.  Note
that the name of this group won't be automatically assumed, you have
to add it to the SPLITs yourself.  This means you can use such splits
to split messages to other groups too.

If you select `Catch All', this group will get postings for any
messages not matched in any other group.  It overrides the variable
gnus-group-split-default-catch-all-group.

Selecting `Ignore' forces no SPLIT to be generated for this group,
disabling all other gnus-group-split fields.")

    (broken-reply-to (const :tag "Broken Reply To" t) "\
Ignore `Reply-To' headers in this group.

That can be useful if you're reading a mailing list group where the
listserv has inserted `Reply-To' headers that point back to the
listserv itself.  This is broken behavior.  So there!")

    (to-group (string :tag "To Group") "\
All posts will be sent to the specified group.")

    (gcc-self (choice :tag  "GCC"
		      :value t
		      (const :tag "To current group" t)
		      (const none)
		      (string :format "%v" :hide-front-space t)) "\
Specify default value for GCC header.

If this symbol is present in the group parameter list and set to t,
new composed messages will be `Gcc''d to the current group.  If it is
present and set to `none', no `Gcc:' header will be generated, if it
is present and a string, this string will be inserted literally as a
`gcc' header (this symbol takes precedence over any default `Gcc'
rules as described later).")

    (expiry-wait (choice :tag  "Expire Wait"
			 :value never
			 (const never)
			 (const immediate)
			 (number :hide-front-space t
				 :format "%v")) "\
When to expire.

Overrides any `nnmail-expiry-wait' and `nnmail-expiry-wait-function'
when expiring expirable messages.  The value can either be a number of
days (not necessarily an integer) or the symbols `never' or
`immediate'.")

    (expiry-target (choice :tag "Expiry Target"
			   :value delete
			   (const delete)
			   (function :format "%v" nnmail-)
			   string) "\
Where expired messages end up.

Overrides `nnmail-expiry-target'.")

    (score-file (file :tag "Score File") "\
Make the specified file into the current score file.
This means that all score commands you issue will end up in this file.")

    (adapt-file (file :tag "Adapt File") "\
Make the specified file into the current adaptive file.
All adaptive score entries will be put into this file.")

    (admin-address (gnus-email-address :tag "Admin Address") "\
Administration address for a mailing list.

When unsubscribing to a mailing list you should never send the
unsubscription notice to the mailing list itself.  Instead, you'd
send messages to the administrative address.  This parameter allows
you to put the admin address somewhere convenient.")

    (display (choice :tag "Display"
		     :value default
		     (const all)
		     (integer)
		     (const default)
		     (sexp  :tag "Other")) "\
Which articles to display on entering the group.

`all'
     Display all articles, both read and unread.

`integer'
     Display the last NUMBER articles in the group.  This is the same as
     entering the group with C-u NUMBER.

`default'
     Display the default visible articles, which normally includes
     unread and ticked articles.

`Other'
     Display the articles that satisfy the S-expression. The S-expression
     should be in an array form.")

    (comment (string :tag  "Comment") "\
An arbitrary comment on the group.")

    (visible (const :tag "Permanently visible" t) "\
Always display this group, even when there are no unread articles in it.")

    (highlight-words
     (choice :tag "Highlight words"
	     :value nil
	     (repeat (list (regexp :tag "Highlight regexp")
			   (number :tag "Group for entire word" 0)
			   (number :tag "Group for displayed part" 0)
			   (symbol :tag "Face"
				   gnus-emphasis-highlight-words))))
     "highlight regexps.
See `gnus-emphasis-alist'.")

    (posting-style
     (choice :tag "Posting style"
	     :value nil
	     (repeat (list
		      (choice :tag "Type"
			      :value nil
			      (const signature)
			      (const signature-file)
			      (const organization)
			      (const address)
			      (const x-face-file)
			      (const name)
			      (const body)
			      (symbol)
			      (string :tag "Header"))
		      (string :format "%v"))))
     "post style.
See `gnus-posting-styles'."))
  "Alist of valid group or topic parameters.

Each entry has the form (NAME TYPE DOC), where NAME is the parameter
itself (a symbol), TYPE is the parameters type (a sexp widget), and
DOC is a documentation string for the parameter.")

(defconst gnus-extra-topic-parameters
  '((subscribe (regexp :tag "Subscribe") "\
If `gnus-subscribe-newsgroup-method' or
`gnus-subscribe-options-newsgroup-method' is set to
`gnus-subscribe-topics', new groups that matches this regexp will
automatically be subscribed to this topic")
    (subscribe-level (integer :tag "Subscribe Level" :value 1) "\
If this topic parameter is set, when new groups are subscribed
automatically under this topic (via the `subscribe' topic parameter)
assign this level to the group, rather than the default level
set in `gnus-level-default-subscribed'"))
  "Alist of topic parameters that are not also group parameters.

Each entry has the form (NAME TYPE DOC), where NAME is the parameter
itself (a symbol), TYPE is the parameters type (a sexp widget), and
DOC is a documentation string for the parameter.")

(defconst gnus-extra-group-parameters
  '((uidvalidity (string :tag "IMAP uidvalidity") "\
Server-assigned value attached to IMAP groups, used to maintain consistency."))
  "Alist of group parameters that are not also topic parameters.

Each entry has the form (NAME TYPE DOC), where NAME is the parameter
itself (a symbol), TYPE is the parameters type (a sexp widget), and
DOC is a documentation string for the parameter.")

(eval-and-compile
  (defconst gnus-agent-parameters
    '((agent-predicate
       (sexp :tag "Selection Predicate" :value false)
       "Predicate used to automatically select articles for downloading."
       gnus-agent-cat-predicate)
      (agent-score
       (choice :tag "Score File" :value nil
               (const file :tag "Use group's score files")
               (repeat (list (string :format "%v" :tag "File name"))))
       "Which score files to use when using score to select articles to fetch.

    `nil'
         All articles will be scored to zero (0).

    `file'
         The group's score files will be used to score the articles.

    `List'
         A list of score file names."
       gnus-agent-cat-score-file)
      (agent-short-article
       (integer :tag "Max Length of Short Article" :value "")
       "The SHORT predicate will evaluate to true when the article is
shorter than this length."  gnus-agent-cat-length-when-short)
      (agent-long-article
       (integer :tag "Min Length of Long Article" :value "")
       "The LONG predicate will evaluate to true when the article is
longer than this length."  gnus-agent-cat-length-when-long)
      (agent-low-score
       (integer :tag "Low Score Limit" :value "")
       "The LOW predicate will evaluate to true when the article scores
lower than this limit."  gnus-agent-cat-low-score)
      (agent-high-score
       (integer :tag "High Score Limit" :value "")
       "The HIGH predicate will evaluate to true when the article scores
higher than this limit."  gnus-agent-cat-high-score)
      (agent-days-until-old
       (integer :tag "Days Until Old" :value "")
       "The OLD predicate will evaluate to true when the fetched article
has been stored locally for at least this many days."
       gnus-agent-cat-days-until-old)
      (agent-enable-expiration
       (radio :tag "Expire in this Group or Topic" :value nil
              (const :format "Enable " ENABLE)
              (const :format "Disable " DISABLE))
       "\nEnable, or disable, agent expiration in this group or topic."
       gnus-agent-cat-enable-expiration)
      (agent-enable-undownloaded-faces
       (boolean :tag "Enable Agent Faces")
       "Have the summary buffer use the agent's undownloaded faces.
These faces, when enabled, act as a warning that an article has not
been fetched into either the agent nor the cache.  This is of most use
to users who use the agent as a cache (i.e. they only operate on
articles that have been downloaded).  Leave disabled to display normal
article faces even when the article hasn't been downloaded."
gnus-agent-cat-enable-undownloaded-faces))
    "Alist of group parameters that are not also topic parameters.

Each entry has the form (NAME TYPE DOC ACCESSOR), where NAME is the
parameter itself (a symbol), TYPE is the parameters type (a sexp
widget), DOC is a documentation string for the parameter, and ACCESSOR
is a function (symbol) that extracts the current value from the
category."))

(defvar gnus-custom-params)
(defvar gnus-custom-method)
(defvar gnus-custom-group)
(defvar gnus-custom-topic)

(defun gnus-group-customize (group &optional topic)
  "Edit the group or topic on the current line."
  (interactive (list (gnus-group-group-name) (gnus-group-topic-name)))
  (let (info
	(types (mapcar (lambda (entry)
			 `(cons :format "%v%h\n"
				:doc ,(nth 2 entry)
				(const :format "" ,(nth 0 entry))
				,(nth 1 entry)))
		       (append (reverse gnus-group-parameters-more)
			       gnus-group-parameters
			       (if group
				   gnus-extra-group-parameters
				 gnus-extra-topic-parameters))))
	(agent (mapcar (lambda (entry)
                         (let ((type (nth 1 entry))
                               vcons)
                           (if (listp type)
                               (setq type (copy-sequence type)))

                           (setq vcons (cdr (memq :value type)))

                           (if (symbolp (car vcons))
                               (condition-case nil
                                   (setcar vcons (symbol-value (car vcons)))
                                 (error)))
                           `(cons :format "%v%h\n"
                                  :doc ,(nth 2 entry)
                                  (const :format "" ,(nth 0 entry))
                                  ,type)))
		       (if gnus-agent
                           gnus-agent-parameters))))
    (unless (or group topic)
      (error "No group on current line"))
    (when (and group topic)
      (error "Both a group an topic on current line"))
    (unless (or topic (setq info (gnus-get-info group)))
      (error "Killed group; can't be edited"))
    ;; Ready.
    (gnus-kill-buffer (gnus-get-buffer-create "*Gnus Customize*"))
    (switch-to-buffer (gnus-get-buffer-create "*Gnus Customize*"))
    (gnus-custom-mode)
    (make-local-variable 'gnus-custom-group)
    (setq gnus-custom-group group)
    (make-local-variable 'gnus-custom-topic)
    (setq gnus-custom-topic topic)
    (buffer-disable-undo)
    (widget-insert "Customize the ")
    (if group
	(widget-create 'info-link
		       :help-echo "Push me to learn more."
		       :tag "group parameters"
		       "(gnus)Group Parameters")
      (widget-create 'info-link
		     :help-echo "Push me to learn more."
		     :tag  "topic parameters"
		     "(gnus)Topic Parameters"))
    (widget-insert " for <")
    (widget-insert (gnus-group-decoded-name (or group topic)))
    (widget-insert "> and press ")
    (widget-create 'push-button
		   :tag "done"
		   :help-echo "Push me when done customizing."
		   :action 'gnus-group-customize-done)
    (widget-insert ".\n\n")
    (make-local-variable 'gnus-custom-params)

    (let ((values (if group
		      (gnus-info-params info)
		    (gnus-topic-parameters topic))))

      ;; The parameters in values may contain duplicates.  This is
      ;; normally OK as assq returns the first. However, right here
      ;; every duplicate ends up being displayed.  So, rather than
      ;; display them, remove them from the list.

      (let ((tmp (setq values (gnus-copy-sequence values)))
	    elem)
	(while (cdr tmp)
	  (while (setq elem (assq (caar tmp) (cdr tmp)))
	    (delq elem tmp))
	  (setq tmp (cdr tmp))))

      (setq gnus-custom-params
            (apply 'widget-create 'group
                   :value values
                   (delq nil
                         (list `(set :inline t
                                     :greedy t
                                     :tag "Parameters"
                                     :format "%t:\n%h%v"
                                     :doc "\
These special parameters are recognized by Gnus.
Check the [ ] for the parameters you want to apply to this group or
to the groups in this topic, then edit the value to suit your taste."
                                     ,@types)
                               (when gnus-agent
                                 `(set :inline t
                                       :greedy t
                                       :tag "Agent Parameters"
                                       :format "%t:\n%h%v"
                                       :doc "\ These agent parameters are
recognized by Gnus.  They control article selection and expiration for
use in the unplugged cache.  Check the [ ] for the parameters you want
to apply to this group or to the groups in this topic, then edit the
value to suit your taste.

For those interested, group parameters override topic parameters while
topic parameters override agent category parameters.  Underlying
category parameters are the customizable variables."  ,@agent))
                               '(repeat :inline t
                                        :tag "Variables"
                                        :format "%t:\n%h%v%i\n\n"
                                        :doc "\
Set variables local to the group you are entering.

If you want to turn threading off in `news.answers', you could put
`(gnus-show-threads nil)' in the group parameters of that group.
`gnus-show-threads' will be made into a local variable in the summary
buffer you enter, and the form nil will be `eval'ed there.

This can also be used as a group-specific hook function, if you'd
like.  If you want to hear a beep when you enter a group, you could
put something like `(dummy-variable (ding))' in the parameters of that
group.  `dummy-variable' will be set to the result of the `(ding)'
form, but who cares?"
                                        (list :format "%v" :value (nil nil)
                                              (symbol :tag "Variable")
                                              (sexp :tag
                                                    "Value")))

                               '(repeat :inline t
                                        :tag "Unknown entries"
                                        sexp))))))
    (when group
      (widget-insert "\n\nYou can also edit the ")
      (widget-create 'info-link
		     :tag "select method"
		     :help-echo "Push me to learn more about select methods."
		     "(gnus)Select Methods")
      (widget-insert " for the group.\n")
      (setq gnus-custom-method
	    (widget-create 'sexp
			   :tag "Method"
			   :value (gnus-info-method info))))
    (use-local-map widget-keymap)
    (widget-setup)
    (buffer-enable-undo)
    (goto-char (point-min))))

(defun gnus-group-customize-done (&rest ignore)
  "Apply changes and bury the buffer."
  (interactive)
  (if gnus-custom-topic
      (gnus-topic-set-parameters gnus-custom-topic
				 (widget-value gnus-custom-params))
    (gnus-group-edit-group-done 'params gnus-custom-group
				(widget-value gnus-custom-params))
    (gnus-group-edit-group-done 'method gnus-custom-group
				(widget-value gnus-custom-method)))
  (bury-buffer))

;;; Score Customization:

(defconst gnus-score-parameters
  '((mark (number :tag "Mark") "\
The value of this entry should be a number.
Any articles with a score lower than this number will be marked as read.")

    (expunge (number :tag "Expunge") "\
The value of this entry should be a number.
Any articles with a score lower than this number will be removed from
the summary buffer.")

    (mark-and-expunge (number :tag "Mark-and-expunge") "\
The value of this entry should be a number.
Any articles with a score lower than this number will be marked as
read and removed from the summary buffer.")

    (thread-mark-and-expunge (number :tag "Thread-mark-and-expunge") "\
The value of this entry should be a number.
All articles that belong to a thread that has a total score below this
number will be marked as read and removed from the summary buffer.
`gnus-thread-score-function' says how to compute the total score
for a thread.")

    (files (repeat :inline t :tag "Files" file) "\
The value of this entry should be any number of file names.
These files are assumed to be score files as well, and will be loaded
the same way this one was.")

    (exclude-files (repeat :inline t :tag "Exclude-files" file) "\
The clue of this entry should be any number of files.
These files will not be loaded, even though they would normally be so,
for some reason or other.")

    (eval (sexp :tag "Eval" :value nil) "\
The value of this entry will be `eval'el.
This element will be ignored when handling global score files.")

    (read-only (boolean :tag "Read-only" :value t) "\
Read-only score files will not be updated or saved.
Global score files should feature this atom.")

    (orphan (number :tag "Orphan") "\
The value of this entry should be a number.
Articles that do not have parents will get this number added to their
scores.  Imagine you follow some high-volume newsgroup, like
`comp.lang.c'.  Most likely you will only follow a few of the threads,
also want to see any new threads.

You can do this with the following two score file entries:

     (orphan -500)
     (mark-and-expunge -100)

When you enter the group the first time, you will only see the new
threads.  You then raise the score of the threads that you find
interesting (with `I T' or `I S'), and ignore (`C y') the rest.
Next time you enter the group, you will see new articles in the
interesting threads, plus any new threads.

I.e.---the orphan score atom is for high-volume groups where there
exist a few interesting threads which can't be found automatically
by ordinary scoring rules.")

    (adapt (choice :tag "Adapt"
		   (const t)
		   (const ignore)
		   (sexp :format "%v"
			 :hide-front-space t)) "\
This entry controls the adaptive scoring.
If it is t, the default adaptive scoring rules will be used.  If it
is `ignore', no adaptive scoring will be performed on this group.  If
it is a list, this list will be used as the adaptive scoring rules.
If it isn't present, or is something other than t or `ignore', the
default adaptive scoring rules will be used.  If you want to use
adaptive scoring on most groups, you'd set `gnus-use-adaptive-scoring'
to t, and insert an `(adapt ignore)' in the groups where you do not
want adaptive scoring.  If you only want adaptive scoring in a few
groups, you'd set `gnus-use-adaptive-scoring' to nil, and insert
`(adapt t)' in the score files of the groups where you want it.")

    (adapt-file (file :tag "Adapt-file") "\
All adaptive score entries will go to the file named by this entry.
It will also be applied when entering the group.  This atom might
be handy if you want to adapt on several groups at once, using the
same adaptive file for a number of groups.")

    (local (repeat :tag "Local"
		   (group :value (nil nil)
			  (symbol :tag "Variable")
			  (sexp :tag "Value"))) "\
The value of this entry should be a list of `(VAR VALUE)' pairs.
Each VAR will be made buffer-local to the current summary buffer,
and set to the value specified.  This is a convenient, if somewhat
strange, way of setting variables in some groups if you don't like
hooks much.")
    (touched (sexp :format "Touched\n") "Internal variable."))
  "Alist of valid symbolic score parameters.

Each entry has the form (NAME TYPE DOC), where NAME is the parameter
itself (a symbol), TYPE is the parameters type (a sexp widget), and DOC is a
documentation string for the parameter.")

(define-widget 'gnus-score-string 'group
  "Edit score entries for string-valued headers."
  :convert-widget 'gnus-score-string-convert)

(defun gnus-score-string-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(string :tag "Match"))
	 (score '(choice :tag "Score"
			 (const :tag "default" nil)
			 (integer :format "%v"
				  :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value s
			;; I should really create a forgiving :match
			;; function for each type below, that only
			;; looked at the first letter.
			(const :tag "Regexp" r)
			(const :tag "Regexp (fixed case)" R)
			(const :tag "Substring" s)
			(const :tag "Substring (fixed case)" S)
			(const :tag "Exact" e)
			(const :tag "Exact (fixed case)" E)
			(const :tag "Word" w)
			(const :tag "Word (fixed case)" W)
			(const :tag "default" nil)))
	 (group `(group ,match ,score ,expire ,type))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header.\n"))
		      "
You can have an arbitrary number of score entries for this header,
each score entry has four elements:

1. The \"match element\".  This should be the string to look for in the
   header.

2. The \"score element\".  This number should be an integer in the
   neginf to posinf interval.  This number is added to the score
   of the article if the match is successful.  If this element is
   not present, the `gnus-score-interactive-default-score' number
   will be used instead.  This is 1000 by default.

3. The \"date element\".  This date says when the last time this score
   entry matched, which provides a mechanism for expiring the
   score entries.  It this element is not present, the score
   entry is permanent.  The date is represented by the number of
   days since December 31, 1 ce.

4. The \"type element\".  This element specifies what function should
   be used to see whether this score entry matches the article.

   There are the regexp, as well as substring types, and exact match,
   and word match types.  If this element is not present, Gnus will
   assume that substring matching should be used.  There is case
   sensitive variants of all match types.")))
    (widget-put widget :args `(,item
			       (repeat :inline t
				       :indent 0
				       :tag ,tag
				       :doc ,doc
				       :format "%t:\n%h%v%i\n\n"
				       (choice :format "%v"
					       :value ("" nil nil s)
					       ,group
					       sexp)))))
  widget)

(define-widget 'gnus-score-integer 'group
  "Edit score entries for integer-valued headers."
  :convert-widget 'gnus-score-integer-convert)

(defun gnus-score-integer-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(integer :tag "Match"))
	 (score '(choice :tag "Score"
			 (const :tag "default" nil)
			 (integer :format "%v"
				  :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value <
			(const <)
			(const >)
			(const =)
			(const >=)
			(const <=)))
	 (group `(group ,match ,score ,expire ,type))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header.")))))
    (widget-put widget :args `(,item
			       (repeat :inline t
				       :indent 0
				       :tag ,tag
				       :doc ,doc
				       :format "%t:\n%h%v%i\n\n"
				       ,group))))
  widget)

(define-widget 'gnus-score-date 'group
  "Edit score entries for date-valued headers."
  :convert-widget 'gnus-score-date-convert)

(defun gnus-score-date-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(string :tag "Match"))
	 (score '(choice :tag "Score"
			 (const :tag "default" nil)
			 (integer :format "%v"
				  :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value regexp
			(const regexp)
			(const before)
			(const at)
			(const after)))
	 (group `(group ,match ,score ,expire ,type))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header."))
		      "
For the Date header we have three kinda silly match types: `before',
`at' and `after'.  I can't really imagine this ever being useful, but,
like, it would feel kinda silly not to provide this function.  Just in
case.  You never know.  Better safe than sorry.  Once burnt, twice
shy.  Don't judge a book by its cover.  Never not have sex on a first
date.  (I have been told that at least one person, and I quote,
\"found this function indispensable\", however.)

A more useful match type is `regexp'.  With it, you can match the date
string using a regular expression.  The date is normalized to ISO8601
compact format first---`YYYYMMDDTHHMMSS'.  If you want to match all
articles that have been posted on April 1st in every year, you could
use `....0401.........' as a match string, for instance.  (Note that
the date is kept in its original time zone, so this will match
articles that were posted when it was April 1st where the article was
posted from.  Time zones are such wholesome fun for the whole family,
eh?")))
    (widget-put widget :args `(,item
			       (repeat :inline t
				       :indent 0
				       :tag ,tag
				       :doc ,doc
				       :format "%t:\n%h%v%i\n\n"
				       ,group))))
  widget)

(define-widget 'gnus-score-extra 'group
  "Edit score entries for extra headers."
  :convert-widget 'gnus-score-extra-convert)

(defun gnus-score-extra-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(string :tag "Match"))
	 (score '(choice :tag "Score"
			 (const :tag "default" nil)
			 (integer :format "%v"
				  :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value s
			;; I should really create a forgiving :match
			;; function for each type below, that only
			;; looked at the first letter.
			(const :tag "Regexp" r)
			(const :tag "Regexp (fixed case)" R)
			(const :tag "Substring" s)
			(const :tag "Substring (fixed case)" S)
			(const :tag "Exact" e)
			(const :tag "Exact (fixed case)" E)
			(const :tag "Word" w)
			(const :tag "Word (fixed case)" W)
			(const :tag "default" nil)))
	 (header (if gnus-extra-headers
		     (let (name)
		       `(choice :tag "Header"
				,@(mapcar (lambda (h)
					    (setq name (symbol-name h))
					    (list 'const :tag name name))
					  gnus-extra-headers)
				(string :tag "Other" :format "%v")))
		   '(string :tag "Header")))
	 (group `(group ,match ,score ,expire ,type ,header))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header.\n")))))
    (widget-put
     widget :args
     `(,item
       (repeat :inline t
	       :indent 0
	       :tag ,tag
	       :doc ,doc
	       :format "%t:\n%h%v%i\n\n"
	       (choice :format "%v"
		       :value ("" nil nil s
			       ,(if gnus-extra-headers
				    (symbol-name (car gnus-extra-headers))
				  ""))
		       ,group
		       sexp)))))
  widget)

(defvar gnus-custom-scores)
(defvar gnus-custom-score-alist)

(defun gnus-score-customize (file)
  "Customize score file FILE.
When called interactively, FILE defaults to the current score file.
This can be changed using the `\\[gnus-score-change-score-file]' command."
  (interactive (list gnus-current-score-file))
  (unless file
    (error "No score file for %s"
           (gnus-group-decoded-name gnus-newsgroup-name)))
  (let ((scores (gnus-score-load file))
	(types (mapcar (lambda (entry)
			 `(group :format "%v%h\n"
				 :doc ,(nth 2 entry)
				 (const :format "" ,(nth 0 entry))
				 ,(nth 1 entry)))
		       gnus-score-parameters)))
    ;; Ready.
    (kill-buffer (gnus-get-buffer-create "*Gnus Customize*"))
    (switch-to-buffer (gnus-get-buffer-create "*Gnus Customize*"))
    (gnus-custom-mode)
    (make-local-variable 'gnus-custom-score-alist)
    (setq gnus-custom-score-alist scores)
    (widget-insert "Customize the ")
    (widget-create 'info-link
		   :help-echo "Push me to learn more."
		   :tag "score entries"
		   "(gnus)Score File Format")
    (widget-insert " for\n\t")
    (widget-insert file)
    (widget-insert "\nand press ")
    (widget-create 'push-button
		   :tag "done"
		   :help-echo "Push me when done customizing."
		   :action 'gnus-score-customize-done)
    (widget-insert ".\n
Check the [ ] for the entries you want to apply to this score file, then
edit the value to suit your taste.  Don't forget to mark the checkbox,
if you do all your changes will be lost.  ")
    (widget-insert "\n\n")
    (make-local-variable 'gnus-custom-scores)
    (setq gnus-custom-scores
	  (widget-create 'group
			 :value scores
			 `(checklist :inline t
				     :greedy t
				     (gnus-score-string :tag "From")
				     (gnus-score-string :tag "Subject")
				     (gnus-score-string :tag "References")
				     (gnus-score-string :tag "Xref")
				     (gnus-score-extra :tag "Extra")
				     (gnus-score-string :tag "Message-ID")
				     (gnus-score-integer :tag "Lines")
				     (gnus-score-integer :tag "Chars")
				     (gnus-score-date :tag "Date")
				     (gnus-score-string :tag "Head"
							:doc "\
Match all headers in the article.

Using one of `Head', `Body', `All' will slow down scoring considerable.
")
				     (gnus-score-string :tag "Body"
							:doc "\
Match the body sans header of the article.

Using one of `Head', `Body', `All' will slow down scoring considerable.
")
				     (gnus-score-string :tag "All"
							:doc "\
Match the entire article, including both headers and body.

Using one of `Head', `Body', `All' will slow down scoring
considerable.
")
				     (gnus-score-string :tag
							"Followup"
							:doc "\
Score all followups to the specified authors.

This entry is somewhat special, in that it will match the `From:'
header, and affect the score of not only the matching articles, but
also all followups to the matching articles.  This allows you
e.g. increase the score of followups to your own articles, or decrease
the score of followups to the articles of some known trouble-maker.
")
				     (gnus-score-string :tag "Thread"
							:doc "\
Add a score entry on all articles that are part of a thread.

This match key works along the same lines as the `Followup' match key.
If you say that you want to score on a (sub-)thread that is started by
an article with a `Message-ID' X, then you add a `thread' match.  This
will add a new `thread' match for each article that has X in its
`References' header.  (These new `thread' matches will use the
`Message-ID's of these matching articles.)  This will ensure that you
can raise/lower the score of an entire thread, even though some
articles in the thread may not have complete `References' headers.
Note that using this may lead to nondeterministic scores of the
articles in the thread.
")
				     ,@types)
			 '(repeat :inline t
				  :tag "Unknown entries"
				  sexp)))
    (use-local-map widget-keymap)
    (widget-setup)))

(defun gnus-score-customize-done (&rest ignore)
  "Reset the score alist with the present value."
  (let ((alist gnus-custom-score-alist)
	(value (widget-value gnus-custom-scores)))
    (setcar alist (car value))
    (setcdr alist (cdr value))
    (gnus-score-set 'touched '(t) alist))
  (bury-buffer))

(defvar category-fields nil)
(defvar gnus-agent-cat-name)
(defvar gnus-agent-cat-score-file)
(defvar gnus-agent-cat-length-when-short)
(defvar gnus-agent-cat-length-when-long)
(defvar gnus-agent-cat-low-score)
(defvar gnus-agent-cat-high-score)
(defvar gnus-agent-cat-enable-expiration)
(defvar gnus-agent-cat-days-until-old)
(defvar gnus-agent-cat-predicate)
(defvar gnus-agent-cat-groups)
(defvar gnus-agent-cat-enable-undownloaded-faces)

(defun gnus-trim-whitespace (s)
  (when (string-match "\\`[ \n\t]+" s)
    (setq s (substring s (match-end 0))))
  (when (string-match "[ \n\t]+\\'" s)
    (setq s (substring s 0 (match-beginning 0))))
  s)

(defmacro gnus-agent-cat-prepare-category-field (parameter)
  (let* ((entry (assq parameter gnus-agent-parameters))
         (field (nth 3 entry)))
    `(let* ((type (copy-sequence
                   (nth 1 (assq ',parameter gnus-agent-parameters))))
            (val (,field info))
            (deflt (if (,field defaults)
                       (concat " [" (gnus-trim-whitespace
                                     (gnus-pp-to-string (,field defaults)))
                               "]")))
            symb)

       (if (eq (car type) 'radio)
           (let* ((rtype (nreverse type))
                  (rt rtype))
             (while (listp (or (cadr rt) 'not-list))
               (setq rt (cdr rt)))

             (setcdr rt (cons '(const :format "Inherit " nil) (cdr rt)))
             (setq type (nreverse rtype))))

       (if deflt
           (let ((tag (cdr (memq :tag type))))
             (when (string-match "\n" deflt)
	       (while (progn (setq deflt (replace-match "\n " t t
							deflt))
			     (string-match "\n" deflt (match-end 0))))
	       (setq deflt (concat "\n" deflt)))

             (setcar tag (concat (car tag) deflt))))

       (widget-insert "\n")

       (setq val (if val
                     (widget-create type :value val)
                   (widget-create type))
             symb (set (make-local-variable ',field) val))

       (widget-put symb :default val)
       (widget-put symb :accessor ',field)
       (push symb category-fields))))

(defun gnus-agent-customize-category (category)
  "Edit the CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist))
        (defaults (list nil '(agent-predicate . false)
                        (cons 'agent-enable-expiration
                              gnus-agent-enable-expiration)
                        '(agent-days-until-old . 7)
                        (cons 'agent-length-when-short
                              gnus-agent-short-article)
                        (cons 'agent-length-when-long gnus-agent-long-article)
                        (cons 'agent-low-score gnus-agent-low-score)
                        (cons 'agent-high-score gnus-agent-high-score))))

    (let ((old (get-buffer "*Gnus Agent Category Customize*")))
      (when old
        (gnus-kill-buffer old)))
    (switch-to-buffer (gnus-get-buffer-create
                       "*Gnus Agent Category Customize*"))

    (let ((inhibit-read-only t))
      (gnus-custom-mode)
      (buffer-disable-undo)

      (let* ((name (gnus-agent-cat-name info)))
        (widget-insert "Customize the Agent Category '")
        (widget-insert (symbol-name name))
        (widget-insert "' and press ")
        (widget-create
         'push-button
         :notify
         (lambda (&rest ignore)
           (let* ((info (assq gnus-agent-cat-name gnus-category-alist))
                  (widgets category-fields))
             (while widgets
               (let* ((widget (pop widgets))
                      (value (condition-case nil (widget-value widget) (error))))
                 (eval `(setf (,(widget-get widget :accessor) ',info)
                              ',value)))))
           (gnus-category-write)
           (gnus-kill-buffer (current-buffer))
           (when (get-buffer gnus-category-buffer)
             (switch-to-buffer (get-buffer gnus-category-buffer))
             (gnus-category-list)))
                       "Done")
        (widget-insert
         "\n    Note: Empty fields default to the customizable global\
 variables.\n\n")

        (set (make-local-variable 'gnus-agent-cat-name)
             name))

      (set (make-local-variable 'category-fields) nil)
      (gnus-agent-cat-prepare-category-field agent-predicate)

      (gnus-agent-cat-prepare-category-field agent-score)
      (gnus-agent-cat-prepare-category-field agent-short-article)
      (gnus-agent-cat-prepare-category-field agent-long-article)
      (gnus-agent-cat-prepare-category-field agent-low-score)
      (gnus-agent-cat-prepare-category-field agent-high-score)

      ;; The group list is NOT handled with
      ;; gnus-agent-cat-prepare-category-field as I don't want the
      ;; group list to appear when customizing a topic.
      (widget-insert "\n")

      (let ((symb
             (set
              (make-local-variable 'gnus-agent-cat-groups)
              (widget-create
               `(choice
                 :format "%[Select Member Groups%]\n%v" :value ignore
                 (const :menu-tag "do not change" :tag "" :value ignore)
                 (checklist :entry-format "%b %v"
                            :menu-tag "display group selectors"
                            :greedy t
                            :value
                            ,(delq nil
                                   (mapcar
                                    (lambda (newsrc)
                                      (car (member
                                            (gnus-info-group newsrc)
                                            (gnus-agent-cat-groups info))))
                                    (cdr gnus-newsrc-alist)))
                            ,@(mapcar (lambda (newsrc)
                                        `(const ,(gnus-info-group newsrc)))
                                      (cdr gnus-newsrc-alist))))))))

      (widget-put symb :default (gnus-agent-cat-groups info))
      (widget-put symb :accessor 'gnus-agent-cat-groups)
      (push symb category-fields))

      (widget-insert "\nExpiration Settings ")

      (gnus-agent-cat-prepare-category-field agent-enable-expiration)
      (gnus-agent-cat-prepare-category-field agent-days-until-old)

      (widget-insert "\nVisual Settings ")

      (gnus-agent-cat-prepare-category-field agent-enable-undownloaded-faces)

      (use-local-map widget-keymap)
      (widget-setup)
      (buffer-enable-undo))))

;;; The End:

(provide 'gnus-cus)

;;; gnus-cus.el ends here
