;;; org-capture.el --- Fast note taking in Org-mode

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

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

;; This file contains an alternative implementation of the same functionality
;; that is also provided by org-remember.el.  The implementation is more
;; streamlined, can produce more target types (e.g. plain list items or
;; table lines).  Also, it does not use a temporary buffer for editing
;; the captured entry - instead it uses an indirect buffer that visits
;; the new entry already in the target buffer (this was an idea by Samuel
;; Wales).  John Wiegley's excellent `remember.el' is not needed for this
;; implementation, even though we borrow heavily from its ideas.

;; This implementation heavily draws on ideas by James TD Smith and
;; Samuel Wales, and, of cause, uses John Wiegley's remember.el as inspiration.

;;; TODO

;; - find a clever way to not always insert an annotation maybe a
;;   predicate function that can check for conditions for %a to be
;;   used.  This could be one of the properties.

;; - Should there be plist members that arrange for properties to be
;;   asked for, like James proposed in his RFC?

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'org-mks)

(declare-function org-datetree-find-date-create "org-datetree"
		  (date &optional keep-restriction))
(declare-function org-table-get-specials "org-table" ())
(declare-function org-table-goto-line "org-table" (N))
(declare-function org-pop-to-buffer-same-window "org-compat"
		  (&optional buffer-or-name norecord label))

(defvar org-remember-default-headline)
(defvar org-remember-templates)
(defvar org-table-hlines)
(defvar dired-buffers)

(defvar org-capture-clock-was-started nil
  "Internal flag, noting if the clock was started.")

(defvar org-capture-last-stored-marker (make-marker)
  "Marker pointing to the entry most recently stored with `org-capture'.")

;; The following variable is scoped dynamically by org-protocol
;; to indicate that the link properties have already been stored
(defvar org-capture-link-is-already-stored nil)

(defgroup org-capture nil
  "Options concerning capturing new entries."
  :tag "Org Capture"
  :group 'org)

(defcustom org-capture-templates nil
  "Templates for the creation of new entries.

Each entry is a list with the following items:

keys         The keys that will select the template, as a string, characters
             only, for example \"a\" for a template to be selected with a
             single key, or \"bt\" for selection with two keys.  When using
             several keys, keys using the same prefix key must be together
             in the list and preceded by a 2-element entry explaining the
             prefix key, for example

                     (\"b\" \"Templates for marking stuff to buy\")

             The \"C\" key is used by default for quick access to the
             customization of the template variable.  But if you want to use
             that key for a template, you can.

description  A short string describing the template, will be shown during
             selection.

type         The type of entry.  Valid types are:
               entry       an Org-mode node, with a headline. Will be
                           filed as the child of the target entry or as
                           a top-level entry.
               item        a plain list item, will be placed in the
                           first plain list at the target
                           location.
               checkitem   a checkbox item.  This differs from the
                           plain list item only is so far as it uses a
                           different default template.
               table-line  a new line in the first table at target location.
               plain       text to be inserted as it is.

target       Specification of where the captured item should be placed.
             In Org-mode files, targets usually define a node.  Entries will
             become children of this node, other types will be added to the
             table or list in the body of this node.

             Most target specifications contain a file name.  If that file
             name is the empty string, it defaults to `org-default-notes-file'.
             A file can also be given as a variable, function, or Emacs Lisp
             form.

             Valid values are:

             (file \"path/to/file\")
                 Text will be placed at the beginning or end of that file

             (id \"id of existing org entry\")
                 File as child of this entry, or in the body of the entry

             (file+headline \"path/to/file\" \"node headline\")
                 Fast configuration if the target heading is unique in the file

             (file+olp \"path/to/file\" \"Level 1 heading\" \"Level 2\" ...)
                 For non-unique headings, the full path is safer

             (file+regexp  \"path/to/file\" \"regexp to find location\")
                 File to the entry matching regexp

             (file+datetree \"path/to/file\")
                 Will create a heading in a date tree for today's date

             (file+datetree+prompt \"path/to/file\")
                 Will create a heading in a date tree, prompts for date

             (file+function \"path/to/file\" function-finding-location)
                 A function to find the right location in the file

             (clock)
                File to the entry that is currently being clocked

             (function function-finding-location)
                Most general way, write your own function to find both
                file and location

template     The template for creating the capture item.  If you leave this
             empty, an appropriate default template will be used.  See below
             for more details.  Instead of a string, this may also be one of

                 (file \"/path/to/template-file\")
                 (function function-returning-the-template)

             in order to get a template from a file, or dynamically
             from a function.

The rest of the entry is a property list of additional options.  Recognized
properties are:

 :prepend            Normally newly captured information will be appended at
                     the target location (last child, last table line,
                     last list item...).  Setting this property will
                     change that.

 :immediate-finish   When set, do not offer to edit the information, just
                     file it away immediately.  This makes sense if the
                     template only needs information that can be added
                     automatically.

 :empty-lines        Set this to the number of lines the should be inserted
                     before and after the new item.  Default 0, only common
                     other value is 1.

 :clock-in           Start the clock in this item.

 :clock-keep         Keep the clock running when filing the captured entry.

 :clock-resume       Start the interrupted clock when finishing the capture.
                     Note that :clock-keep has precedence over :clock-resume.
                     When setting both to `t', the current clock will run and
                     the previous one will not be resumed.

 :unnarrowed         Do not narrow the target buffer, simply show the
                     full buffer.  Default is to narrow it so that you
                     only see the new stuff.

 :table-line-pos     Specification of the location in the table where the
                     new line should be inserted.  It should be a string like
                     \"II-3\", meaning that the new line should become the
                     third line before the second horizontal separator line.

 :kill-buffer        If the target file was not yet visited by a buffer when
                     capture was invoked, kill the buffer again after capture
                     is finalized.

The template defines the text to be inserted.  Often this is an
org-mode entry (so the first line should start with a star) that
will be filed as a child of the target headline.  It can also be
freely formatted text.  Furthermore, the following %-escapes will
be replaced with content and expanded in this order:

  %[pathname] insert the contents of the file given by `pathname'.
  %(sexp)     evaluate elisp `(sexp)' and replace with the result.
  %<...>      the result of format-time-string on the ... format specification.
  %t          time stamp, date only.
  %T          time stamp with date and time.
  %u, %U      like the above, but inactive time stamps.
  %a          annotation, normally the link created with `org-store-link'.
  %i          initial content, copied from the active region.  If %i is
              indented, the entire inserted text will be indented as well.
  %A          like %a, but prompt for the description part.
  %c          current kill ring head.
  %x          content of the X clipboard.
  %k          title of currently clocked task.
  %K          link to currently clocked task.
  %n          user name (taken from `user-full-name').
  %f          file visited by current buffer when org-capture was called.
  %F          full path of the file or directory visited by current buffer.
  %:keyword   specific information for certain link types, see below.
  %^g         prompt for tags, with completion on tags in target file.
  %^G         prompt for tags, with completion on all tags in all agenda files.
  %^t         like %t, but prompt for date.  Similarly %^T, %^u, %^U.
              You may define a prompt like %^{Please specify birthday.
  %^C         interactive selection of which kill or clip to use.
  %^L         like %^C, but insert as link.
  %^{prop}p   prompt the user for a value for property `prop'.
  %^{prompt}  prompt the user for a string and replace this sequence with it.
              A default value and a completion table ca be specified like this:
              %^{prompt|default|completion2|completion3|...}.
  %?          After completing the template, position cursor here.

Apart from these general escapes, you can access information specific to the
link type that is created.  For example, calling `org-capture' in emails
or gnus will record the author and the subject of the message, which you
can access with \"%:from\" and \"%:subject\", respectively.  Here is a
complete list of what is recorded for each link type.

Link type               |  Available information
------------------------+------------------------------------------------------
bbdb                    |  %:type %:name %:company
vm, wl, mh, mew, rmail  |  %:type %:subject %:message-id
                        |  %:from %:fromname %:fromaddress
                        |  %:to   %:toname   %:toaddress
                        |  %:fromto (either \"to NAME\" or \"from NAME\")
                        |  %:date
                        |  %:date-timestamp (as active timestamp)
                        |  %:date-timestamp-inactive (as inactive timestamp)
gnus                    |  %:group, for messages also all email fields
w3, w3m                 |  %:type %:url
info                    |  %:type %:file %:node
calendar                |  %:type %:date"
  :group 'org-capture
  :version "24.1"
  :type
  '(repeat
    (choice :value ("" "" entry (file "~/org/notes.org") "")
     (list :tag "Multikey description"
	   (string :tag "Keys       ")
	   (string :tag "Description"))
     (list :tag "Template entry"
	   (string :tag "Keys           ")
	   (string :tag "Description    ")
	   (choice :tag "Capture Type   " :value entry
		   (const :tag "Org entry" entry)
		   (const :tag "Plain list item" item)
		   (const :tag "Checkbox item" checkitem)
		   (const :tag "Plain text" plain)
		   (const :tag "Table line" table-line))
	   (choice :tag "Target location"
		   (list :tag "File"
			 (const :format "" file)
			 (file :tag "  File"))
		   (list :tag "ID"
			 (const :format "" id)
			 (string :tag "  ID"))
		   (list :tag "File & Headline"
			 (const :format "" file+headline)
			 (file   :tag "  File    ")
			 (string :tag "  Headline"))
		   (list :tag "File & Outline path"
			 (const :format "" file+olp)
			 (file   :tag "  File    ")
			 (repeat :tag "Outline path" :inline t
				 (string :tag "Headline")))
		   (list :tag "File & Regexp"
			 (const :format "" file+regexp)
			 (file   :tag "  File  ")
			 (regexp :tag "  Regexp"))
		   (list :tag "File & Date tree"
			 (const :format "" file+datetree)
			 (file :tag "  File"))
		   (list :tag "File & Date tree, prompt for date"
			 (const :format "" file+datetree+prompt)
			 (file :tag "  File"))
		   (list :tag "File & function"
			 (const :format "" file+function)
			 (file :tag "  File    ")
			 (sexp :tag "  Function"))
		   (list :tag "Current clocking task"
			 (const :format "" clock))
		   (list :tag "Function"
			 (const :format "" function)
			 (sexp :tag "  Function")))
	   (choice :tag "Template"
		   (string)
		   (list :tag "File"
			 (const :format "" file)
			 (file :tag "Template file"))
		   (list :tag "Function"
			 (const :format "" function)
			 (function :tag "Template function")))
	   (plist :inline t
		  ;; Give the most common options as checkboxes
		  :options (((const :format "%v " :prepend) (const t))
			    ((const :format "%v " :immediate-finish) (const t))
			    ((const :format "%v " :empty-lines) (const 1))
			    ((const :format "%v " :clock-in) (const t))
			    ((const :format "%v " :clock-keep) (const t))
			    ((const :format "%v " :clock-resume) (const t))
			    ((const :format "%v " :unnarrowed) (const t))
			    ((const :format "%v " :kill-buffer) (const t))))))))

(defcustom org-capture-before-finalize-hook nil
  "Hook that is run right before a capture process is finalized.
The capture buffer is still current when this hook runs and it is
widened to the entire buffer."
  :group 'org-capture
  :version "24.1"
  :type 'hook)

(defcustom org-capture-after-finalize-hook nil
  "Hook that is run right after a capture process is finalized.
  Suitable for window cleanup"
  :group 'org-capture
  :version "24.1"
  :type 'hook)

;;; The property list for keeping information about the capture process

(defvar org-capture-plist nil
  "Plist for the current capture process, global, to avoid having to pass it.")

(defvar org-capture-current-plist nil
  "Local variable holding the plist in a capture buffer.
This is used to store the plist for use when finishing a capture process
because another such process might have changed the global variable by then.

Each time a new capture buffer has been set up, the global `org-capture-plist'
is copied to this variable, which is local in the indirect buffer.")

(defvar org-capture-clock-keep nil
  "Local variable to store the value of the :clock-keep parameter.
This is needed in case org-capture-finalize is called interactively.")

(defun org-capture-put (&rest stuff)
  "Add properties to the capture property list `org-capture-plist'."
  (while stuff
    (setq org-capture-plist (plist-put org-capture-plist
				       (pop stuff) (pop stuff)))))
(defun org-capture-get (prop &optional local)
  "Get properties from the capture property list `org-capture-plist'.
When LOCAL is set, use the local variable `org-capture-current-plist',
this is necessary after initialization of the capture process,
to avoid conflicts with other active capture processes."
  (plist-get (if local org-capture-current-plist org-capture-plist) prop))

(defun org-capture-member (prop &optional local)
  "Is PROP a property in `org-capture-plist'.
When LOCAL is set, use the local variable `org-capture-current-plist',
this is necessary after initialization of the capture process,
to avoid conflicts with other active capture processes."
  (plist-get (if local org-capture-current-plist org-capture-plist) prop))

;;; The minor mode

(defvar org-capture-mode-map (make-sparse-keymap)
  "Keymap for `org-capture-mode', a minor mode.
Use this map to set additional keybindings for when Org-mode is used
for a capture buffer.")

(defvar org-capture-mode-hook nil
  "Hook for the minor `org-capture-mode'.")

(define-minor-mode org-capture-mode
  "Minor mode for special key bindings in a capture buffer."
  nil " Rem" org-capture-mode-map
  (org-set-local
   'header-line-format
   "Capture buffer.  Finish `C-c C-c', refile `C-c C-w', abort `C-c C-k'.")
  (run-hooks 'org-capture-mode-hook))
(define-key org-capture-mode-map "\C-c\C-c" 'org-capture-finalize)
(define-key org-capture-mode-map "\C-c\C-k" 'org-capture-kill)
(define-key org-capture-mode-map "\C-c\C-w" 'org-capture-refile)

;;; The main commands

;;;###autoload
(defun org-capture (&optional goto keys)
  "Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and then
file the newly captured information.  The text is immediately inserted
at the target location, and an indirect buffer is shown where you can
edit it.  Pressing \\[org-capture-finalize] brings you back to the previous state
of Emacs, so that you can continue your work.

When called interactively with a \\[universal-argument] prefix argument GOTO, don't capture
anything, just go to the file/headline where the selected template
stores its notes.  With a double prefix argument \
\\[universal-argument] \\[universal-argument], go to the last note
stored.

When called with a `C-0' (zero) prefix, insert a template at point.

Lisp programs can set KEYS to a string associated with a template in
`org-capture-templates'.  In this case, interactive selection will be
bypassed."
  (interactive "P")
  (cond
   ((equal goto '(4)) (org-capture-goto-target))
   ((equal goto '(16)) (org-capture-goto-last-stored))
   (t
    ;; FIXME: Are these needed?
    (let* ((orig-buf (current-buffer))
	   (annotation (if (and (boundp 'org-capture-link-is-already-stored)
				org-capture-link-is-already-stored)
			   (plist-get org-store-link-plist :annotation)
			 (ignore-errors (org-store-link nil))))
	   (initial (and (org-region-active-p)
			 (buffer-substring (point) (mark))))
	   (entry (org-capture-select-template keys)))
      (when (stringp initial)
	(remove-text-properties 0 (length initial) '(read-only t) initial))
      (when (stringp annotation)
	(remove-text-properties 0 (length annotation)
				'(read-only t) annotation))
      (cond
       ((equal entry "C")
	(customize-variable 'org-capture-templates))
       ((equal entry "q")
	(error "Abort"))
       (t
	(org-capture-set-plist entry)
	(org-capture-get-template)
	(org-capture-put :original-buffer orig-buf
			 :original-file (or (buffer-file-name orig-buf)
					    (and (featurep 'dired)
						 (car (rassq orig-buf
							     dired-buffers))))
			 :original-file-nondirectory
			 (and (buffer-file-name orig-buf)
			      (file-name-nondirectory
			       (buffer-file-name orig-buf)))
			 :annotation annotation
			 :initial initial)
	(org-capture-put :default-time
			 (or org-overriding-default-time
			     (org-current-time)))
	(org-capture-set-target-location)
	(condition-case error
	    (org-capture-put :template (org-capture-fill-template))
	  ((error quit)
	   (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
	   (error "Capture abort: %s" error)))

	(setq org-capture-clock-keep (org-capture-get :clock-keep))
	(if (equal goto 0)
	    ;;insert at point
	    (org-capture-insert-template-here)
	  (condition-case error
	      (org-capture-place-template)
	    ((error quit)
	     (if (and (buffer-base-buffer (current-buffer))
		      (string-match "\\`CAPTURE-" (buffer-name)))
		 (kill-buffer (current-buffer)))
	     (set-window-configuration (org-capture-get :return-to-wconf))
	     (error "Capture template `%s': %s"
		    (org-capture-get :key)
		    (nth 1 error))))
	  (if (and (eq major-mode 'org-mode)
		   (org-capture-get :clock-in))
	      (condition-case nil
		  (progn
		    (if (org-clock-is-active)
			(org-capture-put :interrupted-clock
					 (copy-marker org-clock-marker)))
		    (org-clock-in)
		    (org-set-local 'org-capture-clock-was-started t))
		(error
		 "Could not start the clock in this capture buffer")))
	  (if (org-capture-get :immediate-finish)
	      (org-capture-finalize nil)))))))))

(defun org-capture-get-template ()
  "Get the template from a file or a function if necessary."
  (let ((txt (org-capture-get :template)) file)
    (cond
     ((and (listp txt) (eq (car txt) 'file))
      (if (file-exists-p
	   (setq file (expand-file-name (nth 1 txt) org-directory)))
	  (setq txt (org-file-contents file))
	(setq txt (format "* Template file %s not found" (nth 1 txt)))))
     ((and (listp txt) (eq (car txt) 'function))
      (if (fboundp (nth 1 txt))
	  (setq txt (funcall (nth 1 txt)))
	(setq txt (format "* Template function %s not found" (nth 1 txt)))))
     ((not txt) (setq txt ""))
     ((stringp txt))
     (t (setq txt "* Invalid capture template")))
    (org-capture-put :template txt)))

(defun org-capture-finalize (&optional stay-with-capture)
  "Finalize the capture process.
With prefix argument STAY-WITH-CAPTURE, jump to the location of the
captured item after finalizing."
  (interactive "P")
  (unless (and org-capture-mode
	       (buffer-base-buffer (current-buffer)))
    (error "This does not seem to be a capture buffer for Org-mode"))

  ;; Did we start the clock in this capture buffer?
  (when (and org-capture-clock-was-started
	     org-clock-marker (marker-buffer org-clock-marker)
	     (equal (marker-buffer org-clock-marker) (buffer-base-buffer))
	     (> org-clock-marker (point-min))
	     (< org-clock-marker (point-max)))
    ;; Looks like the clock we started is still running.  Clock out.
    (when (not org-capture-clock-keep) (let (org-log-note-clock-out) (org-clock-out)))
    (when (and (not org-capture-clock-keep)
	       (org-capture-get :clock-resume 'local)
	       (markerp (org-capture-get :interrupted-clock 'local))
	       (buffer-live-p (marker-buffer
			       (org-capture-get :interrupted-clock 'local))))
      (let ((clock-in-task (org-capture-get :interrupted-clock 'local)))
	(org-with-point-at clock-in-task
	  (org-clock-in)))
      (message "Interrupted clock has been resumed")))

  (let ((beg (point-min))
	(end (point-max))
	(abort-note nil))
    ;; Store the size of the capture buffer
    (org-capture-put :captured-entry-size (- (point-max) (point-min)))
    (widen)
    ;; Store the insertion point in the target buffer
    (org-capture-put :insertion-point (point))

    (if org-note-abort
	(let ((m1 (org-capture-get :begin-marker 'local))
	      (m2 (org-capture-get :end-marker 'local)))
	  (if (and m1 m2 (= m1 beg) (= m2 end))
	      (progn
		(setq m2 (if (cdr (assoc 'heading org-blank-before-new-entry))
			     m2 (1+ m2))
		      m2 (if (< (point-max) m2) (point-max) m2))
		(setq abort-note 'clean)
		(kill-region m1 m2))
	    (setq abort-note 'dirty)))

      ;; Make sure that the empty lines after are correct
      (when (and (> (point-max) end) ; indeed, the buffer was still narrowed
		 (member (org-capture-get :type 'local)
			 '(entry item checkitem plain)))
	(save-excursion
	  (goto-char end)
	  (or (bolp) (newline))
	  (org-capture-empty-lines-after
	   (or (org-capture-get :empty-lines 'local) 0))))
      ;; Postprocessing:  Update Statistics cookies, do the sorting
      (when (eq major-mode 'org-mode)
	(save-excursion
	  (when (ignore-errors (org-back-to-heading))
	    (org-update-parent-todo-statistics)
	    (org-update-checkbox-count)))
	;; FIXME Here we should do the sorting
	;; If we have added a table line, maybe recompute?
	(when (and (eq (org-capture-get :type 'local) 'table-line)
		   (org-at-table-p))
	  (if (org-table-get-stored-formulas)
	      (org-table-recalculate 'all) ;; FIXME: Should we iterate???
	    (org-table-align))))
      ;; Store this place as the last one where we stored something
      ;; Do the marking in the base buffer, so that it makes sense after
      ;; the indirect buffer has been killed.
      (org-capture-bookmark-last-stored-position)

      ;; Run the hook
      (run-hooks 'org-capture-before-finalize-hook))

    ;; Kill the indirect buffer
    (save-buffer)
    (let ((return-wconf (org-capture-get :return-to-wconf 'local))
	  (new-buffer (org-capture-get :new-buffer 'local))
	  (kill-buffer (org-capture-get :kill-buffer 'local))
	  (base-buffer (buffer-base-buffer (current-buffer))))

      ;; Kill the indirect buffer
      (kill-buffer (current-buffer))

      ;; Narrow back the target buffer to its previous state
      (with-current-buffer (org-capture-get :buffer)
        (let ((reg (org-capture-get :initial-target-region))
	      (pos (org-capture-get :initial-target-position))
	      (ipt (org-capture-get :insertion-point))
	      (size (org-capture-get :captured-entry-size)))
	  (when reg
	    (cond ((< ipt (car reg))
		   ;; insertion point is before the narrowed region
		   (narrow-to-region (+ size (car reg)) (+ size (cdr reg))))
		  ((> ipt (cdr reg))
		   ;; insertion point is after the narrowed region
		   (narrow-to-region (car reg) (cdr reg)))
		  (t
		   ;; insertion point is within the narrowed region
		   (narrow-to-region (car reg) (+ size (cdr reg)))))
	    ;; now place back the point at its original position
	    (if (< ipt (car reg))
		(goto-char (+ size pos))
	      (goto-char (if (< ipt pos) (+ size pos) pos))))))

      ;; Kill the target buffer if that is desired
      (when (and base-buffer new-buffer kill-buffer)
	(with-current-buffer base-buffer (save-buffer))
	(kill-buffer base-buffer))

      ;; Restore the window configuration before capture
      (set-window-configuration return-wconf))

    (run-hooks 'org-capture-after-finalize-hook)
    ;; Special cases
    (cond
     (abort-note
      (cond
       ((equal abort-note 'clean)
	(message "Capture process aborted and target buffer cleaned up"))
       ((equal abort-note 'dirty)
	(error "Capture process aborted, but target buffer could not be cleaned up correctly"))))
     (stay-with-capture
      (org-capture-goto-last-stored)))
    ;; Return if we did store something
    (not abort-note)))

(defun org-capture-refile ()
  "Finalize the current capture and then refile the entry.
Refiling is done from the base buffer, because the indirect buffer is then
already gone.  Any prefix argument will be passed to the refile command."
  (interactive)
  (unless (eq (org-capture-get :type 'local) 'entry)
    (error
     "Refiling from a capture buffer makes only sense for `entry'-type templates"))
  (let ((pos (point))
	(base (buffer-base-buffer (current-buffer)))
	(org-refile-for-capture t))
    (org-capture-finalize)
    (save-window-excursion
      (with-current-buffer (or base (current-buffer))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char pos)
	    (call-interactively 'org-refile)))))))

(defun org-capture-kill ()
  "Abort the current capture process."
  (interactive)
  ;; FIXME: This does not do the right thing, we need to remove the new stuff
  ;; By hand it is easy: undo, then kill the buffer
  (let ((org-note-abort t)
	(org-capture-before-finalize-hook nil))
    (org-capture-finalize)))

(defun org-capture-goto-last-stored ()
  "Go to the location where the last capture note was stored."
  (interactive)
  (org-goto-marker-or-bmk org-capture-last-stored-marker
			  "org-capture-last-stored")
  (message "This is the last note stored by a capture process"))

;;; Supporting functions for handling the process

(defun org-capture-put-target-region-and-position ()
  "Store the initial region with `org-capture-put'."
  (org-capture-put
   :initial-target-region
   ;; Check if the buffer is currently narrowed
   (when (/= (buffer-size) (- (point-max) (point-min)))
     (cons (point-min) (point-max))))
  ;; store the current point
  (org-capture-put :initial-target-position (point)))

(defun org-capture-set-target-location (&optional target)
  "Find target buffer and position and store then in the property list."
  (let ((target-entry-p t))
    (setq target (or target (org-capture-get :target)))
    (save-excursion
      (cond
       ((eq (car target) 'file)
	(set-buffer (org-capture-target-buffer (nth 1 target)))
	(org-capture-put-target-region-and-position)
	(widen)
	(setq target-entry-p nil))

       ((eq (car target) 'id)
	(let ((loc (org-id-find (nth 1 target))))
	  (if (not loc)
	      (error "Cannot find target ID \"%s\"" (nth 1 target))
	    (set-buffer (org-capture-target-buffer (car loc)))
	    (widen)
	    (org-capture-put-target-region-and-position)
	    (goto-char (cdr loc)))))

       ((eq (car target) 'file+headline)
	(set-buffer (org-capture-target-buffer (nth 1 target)))
	(org-capture-put-target-region-and-position)
	(widen)
	(let ((hd (nth 2 target)))
	  (goto-char (point-min))
	  (unless (eq major-mode 'org-mode)
	    (error
	     "Target buffer \"%s\" for file+headline should be in Org mode"
	     (current-buffer)))
	  (if (re-search-forward
	       (format org-complex-heading-regexp-format (regexp-quote hd))
	       nil t)
	      (goto-char (point-at-bol))
	    (goto-char (point-max))
	    (or (bolp) (insert "\n"))
	    (insert "* " hd "\n")
	    (beginning-of-line 0))))

       ((eq (car target) 'file+olp)
 	(let ((m (org-find-olp
		  (cons (org-capture-expand-file (nth 1 target))
			(cddr target)))))
	  (set-buffer (marker-buffer m))
	  (org-capture-put-target-region-and-position)
	  (widen)
	  (goto-char m)))

       ((eq (car target) 'file+regexp)
	(set-buffer (org-capture-target-buffer (nth 1 target)))
	(org-capture-put-target-region-and-position)
	(widen)
	(goto-char (point-min))
	(if (re-search-forward (nth 2 target) nil t)
	    (progn
	      (goto-char (if (org-capture-get :prepend)
			     (match-beginning 0) (match-end 0)))
	      (org-capture-put :exact-position (point))
	      (setq target-entry-p (and (eq major-mode 'org-mode) (org-at-heading-p))))
	  (error "No match for target regexp in file %s" (nth 1 target))))

       ((memq (car target) '(file+datetree file+datetree+prompt))
	(require 'org-datetree)
	(set-buffer (org-capture-target-buffer (nth 1 target)))
	(org-capture-put-target-region-and-position)
	(widen)
	;; Make a date tree entry, with the current date (or yesterday,
	;; if we are extending dates for a couple of hours)
	(org-datetree-find-date-create
	 (calendar-gregorian-from-absolute
	  (cond
	   (org-overriding-default-time
	    ;; use the overriding default time
	    (time-to-days org-overriding-default-time))

	   ((eq (car target) 'file+datetree+prompt)
	    ;; prompt for date
	    (let ((prompt-time (org-read-date
				nil t nil "Date for tree entry:"
				(current-time))))
	      (org-capture-put :prompt-time prompt-time
			       :default-time prompt-time)
	      (time-to-days prompt-time)))
	   (t
	    ;; current date, possible corrected for late night workers
	    (org-today))))))

       ((eq (car target) 'file+function)
	(set-buffer (org-capture-target-buffer (nth 1 target)))
	(org-capture-put-target-region-and-position)
	(widen)
	(funcall (nth 2 target))
	(org-capture-put :exact-position (point))
	(setq target-entry-p (and (eq major-mode 'org-mode) (org-at-heading-p))))

       ((eq (car target) 'function)
	(funcall (nth 1 target))
	(org-capture-put :exact-position (point))
	(setq target-entry-p (and (eq major-mode 'org-mode) (org-at-heading-p))))

       ((eq (car target) 'clock)
	(if (and (markerp org-clock-hd-marker)
		 (marker-buffer org-clock-hd-marker))
	    (progn (set-buffer (marker-buffer org-clock-hd-marker))
		   (org-capture-put-target-region-and-position)
		   (widen)
		   (goto-char org-clock-hd-marker))
	  (error "No running clock that could be used as capture target")))

       (t (error "Invalid capture target specification")))

      (org-capture-put :buffer (current-buffer) :pos (point)
		       :target-entry-p target-entry-p))))

(defun org-capture-expand-file (file)
  "Expand functions and symbols for FILE.
When FILE is a function, call it.  When it is a form, evaluate
it.  When it is a variable, retrieve the value.  Return whatever we get."
  (cond
   ((org-string-nw-p file) file)
   ((functionp file) (funcall file))
   ((and (symbolp file) (boundp file)) (symbol-value file))
   ((and file (consp file)) (eval file))
   (t file)))

(defun org-capture-target-buffer (file)
  "Get a buffer for FILE."
  (setq file (org-capture-expand-file file))
  (setq file (or (org-string-nw-p file)
		 org-default-notes-file
		 (error "No notes file specified, and no default available")))
  (or (org-find-base-buffer-visiting file)
      (progn (org-capture-put :new-buffer t)
	     (find-file-noselect (expand-file-name file org-directory)))))

(defun org-capture-steal-local-variables (buffer)
  "Install Org-mode local variables."
  (mapc (lambda (v)
	  (ignore-errors (org-set-local (car v) (cdr v))))
	(buffer-local-variables buffer)))

(defun org-capture-place-template ()
  "Insert the template at the target location, and display the buffer."
  (org-capture-put :return-to-wconf (current-window-configuration))
  (delete-other-windows)
  (org-switch-to-buffer-other-window
   (org-capture-get-indirect-buffer (org-capture-get :buffer) "CAPTURE"))
  (widen)
  (show-all)
  (goto-char (org-capture-get :pos))
  (org-set-local 'org-capture-target-marker
		 (move-marker (make-marker) (point)))
  (org-set-local 'outline-level 'org-outline-level)
  (let* ((template (org-capture-get :template))
	 (type (org-capture-get :type)))
    (case type
      ((nil entry) (org-capture-place-entry))
      (table-line (org-capture-place-table-line))
      (plain (org-capture-place-plain-text))
      (item (org-capture-place-item))
      (checkitem (org-capture-place-item))))
  (org-capture-mode 1)
  (org-set-local 'org-capture-current-plist org-capture-plist))

(defun org-capture-place-entry ()
  "Place the template as a new Org entry."
  (let* ((txt (org-capture-get :template))
	 (reversed (org-capture-get :prepend))
	 (target-entry-p (org-capture-get :target-entry-p))
	 level beg end file)

    (cond
     ((org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
     ((not target-entry-p)
      ;; Insert as top-level entry, either at beginning or at end of file
      (setq level 1)
      (if reversed
	  (progn (goto-char (point-min))
		 (or (org-at-heading-p)
		     (outline-next-heading)))
	(goto-char (point-max))
	(or (bolp) (insert "\n"))))
     (t
      ;; Insert as a child of the current entry
      (and (looking-at "\\*+")
	   (setq level (- (match-end 0) (match-beginning 0))))
      (setq level (org-get-valid-level (or level 1) 1))
      (if reversed
	  (progn
	    (outline-next-heading)
	    (or (bolp) (insert "\n")))
	(org-end-of-subtree t t)
	(or (bolp) (insert "\n")))))
    (org-capture-empty-lines-before)
    (setq beg (point))
    (org-capture-verify-tree txt)
    (org-paste-subtree level txt 'for-yank)
    (org-capture-empty-lines-after 1)
    (org-capture-position-for-last-stored beg)
    (outline-next-heading)
    (setq end (point))
    (org-capture-mark-kill-region beg (1- end))
    (org-capture-narrow beg (1- end))
    (goto-char beg)
    (if (re-search-forward "%\\?" end t) (replace-match ""))))

(defun org-capture-place-item ()
  "Place the template as a new plain list item."
  (let* ((txt (org-capture-get :template))
	 (target-entry-p (org-capture-get :target-entry-p))
	 (ind 0)
	 beg end)
    (if (org-capture-get :exact-position)
	(goto-char (org-capture-get :exact-position))
      (cond
       ((not target-entry-p)
	;; Insert as top-level entry, either at beginning or at end of file
	(setq beg (point-min) end (point-max)))
       (t
	(setq beg (1+ (point-at-eol))
	      end (save-excursion (outline-next-heading) (point)))))
      (if (org-capture-get :prepend)
	  (progn
	    (goto-char beg)
	    (if (org-list-search-forward (org-item-beginning-re) end t)
		(progn
		  (goto-char (match-beginning 0))
		  (setq ind (org-get-indentation)))
	      (goto-char end)
	      (setq ind 0)))
	(goto-char end)
	(if (org-list-search-backward (org-item-beginning-re) beg t)
	    (progn
	      (setq ind (org-get-indentation))
	      (org-end-of-item))
	  (setq ind 0))))
    ;; Remove common indentation
    (setq txt (org-remove-indentation txt))
    ;; Make sure this is indeed an item
    (unless (string-match (concat "\\`" (org-item-re)) txt)
      (setq txt (concat "- "
			(mapconcat 'identity (split-string txt "\n")
				   "\n  "))))
    ;; Set the correct indentation, depending on context
    (setq ind (make-string ind ?\ ))
    (setq txt (concat ind
		      (mapconcat 'identity (split-string txt "\n")
				 (concat "\n" ind))
		      "\n"))
    ;; Insert, with surrounding empty lines
    (org-capture-empty-lines-before)
    (setq beg (point))
    (insert txt)
    (or (bolp) (insert "\n"))
    (org-capture-empty-lines-after 1)
    (org-capture-position-for-last-stored beg)
    (forward-char 1)
    (setq end (point))
    (org-capture-mark-kill-region beg (1- end))
    (org-capture-narrow beg (1- end))
    (if (re-search-forward "%\\?" end t) (replace-match ""))))

(defun org-capture-place-table-line ()
  "Place the template as a table line."
  (require 'org-table)
  (let* ((txt (org-capture-get :template))
	 (target-entry-p (org-capture-get :target-entry-p))
	 (table-line-pos (org-capture-get :table-line-pos))
	 ind beg end)
    (cond
     ((org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
     ((not target-entry-p)
      ;; Table is not necessarily under a heading
      (setq beg (point-min) end (point-max)))
     (t
      ;; WE are at a heading, limit search to the body
      (setq beg (1+ (point-at-eol))
	    end (save-excursion (outline-next-heading) (point)))))
    (if (re-search-forward org-table-dataline-regexp end t)
	(let ((b (org-table-begin)) (e (org-table-end)))
	  (goto-char e)
	  (if (looking-at "[ \t]*#\\+TBLFM:")
	      (forward-line 1))
	  (narrow-to-region b (point)))
      (goto-char end)
      (insert "\n|   |\n|----|\n|    |\n")
      (narrow-to-region (1+ end) (point)))
    ;; We are narrowed to the table, or to an empty line if there was no table

    ;; Check if the template is good
    (if (not (string-match org-table-dataline-regexp txt))
	(setq txt "| %?Bad template |\n"))
    (cond
     ((and table-line-pos
	   (string-match "\\(I+\\)\\([-+][0-9]\\)" table-line-pos))
      ;; we have a complex line specification
      (goto-char (point-min))
      (let ((nh (- (match-end 1) (match-beginning 1)))
	    (delta (string-to-number (match-string 2 table-line-pos)))
	    ll)
	;; The user wants a special position in the table
	(org-table-get-specials)
	(setq ll (ignore-errors (aref org-table-hlines nh)))
	(unless ll (error "Invalid table line specification \"%s\""
			  table-line-pos))
	(setq ll (+ ll delta (if (< delta 0) 0 -1)))
	(org-goto-line ll)
	(org-table-insert-row 'below)
	(beginning-of-line 1)
	(delete-region (point) (1+ (point-at-eol)))
	(setq beg (point))
	(insert txt)
	(setq end (point))))
     ((org-capture-get :prepend)
      (goto-char (point-min))
      (re-search-forward org-table-hline-regexp nil t)
      (beginning-of-line 1)
      (re-search-forward org-table-dataline-regexp nil t)
      (beginning-of-line 1)
      (setq beg (point))
      (org-table-insert-row)
      (beginning-of-line 1)
      (delete-region (point) (1+ (point-at-eol)))
      (insert txt)
      (setq end (point)))
     (t
      (goto-char (point-max))
      (re-search-backward org-table-dataline-regexp nil t)
      (beginning-of-line 1)
      (org-table-insert-row 'below)
      (beginning-of-line 1)
      (delete-region (point) (1+ (point-at-eol)))
      (setq beg (point))
      (insert txt)
      (setq end (point))))
    (goto-char beg)
    (org-capture-position-for-last-stored 'table-line)
    (if (re-search-forward "%\\?" end t) (replace-match ""))
    (org-table-align)))

(defun org-capture-place-plain-text ()
  "Place the template plainly.
If the target locator points at an Org node, place the template into
the text of the entry, before the first child.  If not, place the
template at the beginning or end of the file.
Of course, if exact position has been required, just put it there."
  (let* ((txt (org-capture-get :template))
	 beg end)
    (cond
     ((org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
     ((and (org-capture-get :target-entry-p)
	   (bolp)
	   (looking-at org-outline-regexp))
      ;; we should place the text into this entry
      (if (org-capture-get :prepend)
	  ;; Skip meta data and drawers
	  (org-end-of-meta-data-and-drawers)
	;; go to ent of the entry text, before the next headline
	(outline-next-heading)))
     (t
      ;; beginning or end of file
      (goto-char (if (org-capture-get :prepend) (point-min) (point-max)))))
    (or (bolp) (newline))
    (org-capture-empty-lines-before)
    (setq beg (point))
    (insert txt)
    (org-capture-empty-lines-after 1)
    (org-capture-position-for-last-stored beg)
    (setq end (point))
    (org-capture-mark-kill-region beg (1- end))
    (org-capture-narrow beg (1- end))
    (if (re-search-forward "%\\?" end t) (replace-match ""))))

(defun org-capture-mark-kill-region (beg end)
  "Mark the region that will have to be killed when aborting capture."
  (let ((m1 (move-marker (make-marker) beg))
	(m2 (move-marker (make-marker) end)))
    (org-capture-put :begin-marker m1)
    (org-capture-put :end-marker m2)))

(defun org-capture-position-for-last-stored (where)
  "Memorize the position that should later become the position of last capture."
  (cond
   ((integerp where)
    (org-capture-put :position-for-last-stored
		     (move-marker (make-marker) where
				  (or (buffer-base-buffer (current-buffer))
				      (current-buffer)))))
   ((eq where 'table-line)
    (org-capture-put :position-for-last-stored
		     (list 'table-line
			   (org-table-current-dline))))
   (t (error "This should not happen"))))

(defun org-capture-bookmark-last-stored-position ()
  "Bookmark the last-captured position."
  (let* ((where (org-capture-get :position-for-last-stored 'local))
	 (pos (cond
	       ((markerp where)
		(prog1 (marker-position where)
		  (move-marker where nil)))
	       ((and (listp where) (eq (car where) 'table-line))
		(if (org-at-table-p)
		    (save-excursion
		      (org-table-goto-line (nth 1 where))
		      (point-at-bol))
		  (point))))))
    (with-current-buffer (buffer-base-buffer (current-buffer))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char pos)
	  (bookmark-set "org-capture-last-stored")
	  (move-marker org-capture-last-stored-marker (point)))))))

(defun org-capture-narrow (beg end)
  "Narrow, unless configuration says not to narrow."
  (unless (org-capture-get :unnarrowed)
    (narrow-to-region beg end)
    (goto-char beg)))

(defun org-capture-empty-lines-before (&optional n)
  "Arrange for the correct number of empty lines before the insertion point.
Point will be after the empty lines, so insertion can directly be done."
  (setq n (or n (org-capture-get :empty-lines) 0))
  (let ((pos (point)))
    (org-back-over-empty-lines)
    (delete-region (point) pos)
    (if (> n 0) (newline n))))

(defun org-capture-empty-lines-after (&optional n)
  "Arrange for the correct number of empty lines after the inserted string.
Point will remain at the first line after the inserted text."
  (setq n (or n (org-capture-get :empty-lines) 0))
  (org-back-over-empty-lines)
  (while (looking-at "[ \t]*\n") (replace-match ""))
  (let ((pos (point)))
    (if (> n 0) (newline n))
    (goto-char pos)))

(defvar org-clock-marker) ; Defined in org.el
;;;###autoload
(defun org-capture-insert-template-here ()
  (let* ((template (org-capture-get :template))
	 (type  (org-capture-get :type))
	 beg end pp)
    (or (bolp) (newline))
    (setq beg (point))
    (cond
     ((and (eq type 'entry) (eq major-mode 'org-mode))
      (org-capture-verify-tree (org-capture-get :template))
      (org-paste-subtree nil template t))
     ((and (memq type '(item checkitem))
	   (eq major-mode 'org-mode)
	   (save-excursion (skip-chars-backward " \t\n")
			   (setq pp (point))
			   (org-in-item-p)))
      (goto-char pp)
      (org-insert-item)
      (skip-chars-backward " ")
      (skip-chars-backward "-+*0123456789).")
      (delete-region (point) (point-at-eol))
      (setq beg (point))
      (org-remove-indentation template)
      (insert template)
      (org-capture-empty-lines-after)
      (goto-char beg)
      (org-list-repair)
      (org-end-of-item)
      (setq end (point)))
     (t (insert template)))
    (setq end (point))
    (goto-char beg)
    (if (re-search-forward "%\\?" end t)
	(replace-match ""))))

(defun org-capture-set-plist (entry)
  "Initialize the property list from the template definition."
  (setq org-capture-plist (copy-sequence (nthcdr 5 entry)))
  (org-capture-put :key (car entry) :description (nth 1 entry)
		   :target (nth 3 entry))
  (let ((txt (nth 4 entry)) (type (or (nth 2 entry) 'entry)))
    (when (or (not txt) (and (stringp txt) (not (string-match "\\S-" txt))))
      ;; The template may be empty or omitted for special types.
      ;; Here we insert the default templates for such cases.
      (cond
       ((eq type 'item) (setq txt "- %?"))
       ((eq type 'checkitem) (setq txt "- [ ] %?"))
       ((eq type 'table-line) (setq txt "| %? |"))
       ((member type '(nil entry)) (setq txt "* %?\n  %a"))))
    (org-capture-put :template txt :type type)))

(defun org-capture-goto-target (&optional template-key)
  "Go to the target location of a capture template.
The user is queried for the template."
  (interactive)
  (let* (org-select-template-temp-major-mode
	 (entry (org-capture-select-template template-key)))
    (unless entry
      (error "No capture template selected"))
    (org-capture-set-plist entry)
    (org-capture-set-target-location)
    (org-pop-to-buffer-same-window (org-capture-get :buffer))
    (goto-char (org-capture-get :pos))))

(defun org-capture-get-indirect-buffer (&optional buffer prefix)
  "Make an indirect buffer for a capture process.
Use PREFIX as a prefix for the name of the indirect buffer."
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (setq bname (concat prefix "-" base))
    (while (buffer-live-p (get-buffer bname))
      (setq bname (concat prefix "-" (number-to-string (incf n)) "-" base)))
    (condition-case nil
        (make-indirect-buffer buffer bname 'clone)
      (error
       (let ((buf (make-indirect-buffer buffer bname)))
	 (with-current-buffer buf (org-mode))
	 buf)))))

(defun org-capture-verify-tree (tree)
  "Throw error if TREE is not a valid tree"
  (unless (org-kill-is-subtree-p tree)
    (error "Template is not a valid Org entry or tree")))

;;; The template code

(defun org-capture-select-template (&optional keys)
  "Select a capture template.
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
	 (or org-capture-templates
	     '(("t" "Task" entry (file+headline "" "Tasks")
		"* TODO %?\n  %u\n  %a")))))
    (if keys
	(or (assoc keys org-capture-templates)
	    (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
	       "Select a capture template\n========================="
	       "Template key: "
	       '(("C" "Customize org-capture-templates")
		 ("q" "Abort"))))))

(defun org-capture-fill-template (&optional template initial annotation)
  "Fill a template and return the filled template as a string.
The template may still contain \"%?\" for cursor positioning."
  (setq template (or template (org-capture-get :template)))
  (when (stringp initial)
    (setq initial (org-no-properties initial))
    (remove-text-properties 0 (length initial) '(read-only t) initial))
  (let* ((buffer (org-capture-get :buffer))
	 (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
	 (ct (org-capture-get :default-time))
	 (dct (decode-time ct))
	 (ct1
	  (if (< (nth 2 dct) org-extend-today-until)
	      (encode-time 0 59 23 (1- (nth 3 dct)) (nth 4 dct) (nth 5 dct))
	    ct))
	 (plist-p (if org-store-link-plist t nil))
	 (v-c (and (> (length kill-ring) 0) (current-kill 0)))
	 (v-x (or (org-get-x-clipboard 'PRIMARY)
		  (org-get-x-clipboard 'CLIPBOARD)
		  (org-get-x-clipboard 'SECONDARY)))
	 (v-t (format-time-string (car org-time-stamp-formats) ct))
	 (v-T (format-time-string (cdr org-time-stamp-formats) ct))
	 (v-u (concat "[" (substring v-t 1 -1) "]"))
	 (v-U (concat "[" (substring v-T 1 -1) "]"))
	 ;; `initial' and `annotation' might habe been passed.
	 ;; But if the property list has them, we prefer those values
	 (v-i (or (plist-get org-store-link-plist :initial)
		  initial
		  (org-capture-get :initial)
		  ""))
	 (v-a (or (plist-get org-store-link-plist :annotation)
		  annotation
		  (org-capture-get :annotation)
		  ""))
	 ;; Is the link empty?  Then we do not want it...
	 (v-a (if (equal v-a "[[]]") "" v-a))
	 (clipboards (remove nil (list v-i
				       (org-get-x-clipboard 'PRIMARY)
				       (org-get-x-clipboard 'CLIPBOARD)
				       (org-get-x-clipboard 'SECONDARY)
				       v-c)))
	 (v-A (if (and v-a
		       (string-match
			"\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" v-a))
		  (replace-match "[\\1[%^{Link description}]]" nil nil v-a)
		v-a))
	 (v-n user-full-name)
	 (v-k (if (marker-buffer org-clock-marker)
		  (org-substring-no-properties org-clock-heading)))
	 (v-K (if (marker-buffer org-clock-marker)
		  (org-make-link-string
		   (buffer-file-name (marker-buffer org-clock-marker))
		   org-clock-heading)))
	 (v-f (or (org-capture-get :original-file-nondirectory) ""))
	 (v-F (or (org-capture-get :original-file) ""))
	 v-I
	 (org-startup-folded nil)
	 (org-inhibit-startup t)
	 org-time-was-given org-end-time-was-given x
	 prompt completions char time pos default histvar)

    (setq org-store-link-plist
	  (plist-put org-store-link-plist :annotation v-a)
	  org-store-link-plist
	  (plist-put org-store-link-plist :initial v-i))
    (setq initial v-i)

    (unless template (setq template "") (message "No template") (ding)
	    (sit-for 1))
    (save-window-excursion
      (delete-other-windows)
      (org-pop-to-buffer-same-window (get-buffer-create "*Capture*"))
      (erase-buffer)
      (insert template)
      (goto-char (point-min))
      (org-capture-steal-local-variables buffer)
      (setq buffer-file-name nil)

      ;; %[] Insert contents of a file.
      (goto-char (point-min))
      (while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
	(unless (org-capture-escaped-%)
	  (let ((start (match-beginning 0))
		(end (match-end 0))
		(filename (expand-file-name (match-string 1))))
	    (goto-char start)
	    (delete-region start end)
	    (condition-case error
		(insert-file-contents filename)
	      (error (insert (format "%%![Couldn't insert %s: %s]"
				     filename error)))))))
      ;; %() embedded elisp
      (goto-char (point-min))
      (while (re-search-forward "%\\((.+)\\)" nil t)
	(unless (org-capture-escaped-%)
	  (goto-char (match-beginning 0))
	  (let ((template-start (point)))
	    (forward-char 1)
	    (let ((result (org-eval (read (current-buffer)))))
	      (delete-region template-start (point))
	      (insert result)))))

      ;; The current time
      (goto-char (point-min))
      (while (re-search-forward "%<\\([^>\n]+\\)>" nil t)
	(replace-match (format-time-string (match-string 1)) t t))

      ;; Simple %-escapes
      (goto-char (point-min))
      (while (re-search-forward "%\\([tTuUaiAcxkKInfF]\\)" nil t)
	(unless (org-capture-escaped-%)
	  (when (and initial (equal (match-string 0) "%i"))
	    (save-match-data
	      (let* ((lead (buffer-substring
			    (point-at-bol) (match-beginning 0))))
		(setq v-i (mapconcat 'identity
				     (org-split-string initial "\n")
				     (concat "\n" lead))))))
	  (replace-match
	   (or (eval (intern (concat "v-" (match-string 1)))) "")
	   t t)))

      ;; From the property list
      (when plist-p
	(goto-char (point-min))
	(while (re-search-forward "%\\(:[-a-zA-Z]+\\)" nil t)
	  (unless (org-capture-escaped-%)
	    (and (setq x (or (plist-get org-store-link-plist
					(intern (match-string 1))) ""))
		 (replace-match x t t)))))

      ;; Turn on org-mode in temp buffer, set local variables
      ;; This is to support completion in interactive prompts
      (let ((org-inhibit-startup t)) (org-mode))
      ;; Interactive template entries
      (goto-char (point-min))
      (while (re-search-forward "%^\\({\\([^}]*\\)}\\)?\\([gGtTuUCLp]\\)?"
				nil t)
	(unless (org-capture-escaped-%)
	  (setq char (if (match-end 3) (match-string-no-properties 3))
		prompt (if (match-end 2) (match-string-no-properties 2)))
	  (goto-char (match-beginning 0))
	  (replace-match "")
	  (setq completions nil default nil)
	  (when prompt
	    (setq completions (org-split-string prompt "|")
		  prompt (pop completions)
		  default (car completions)
		  histvar (intern (concat
				   "org-capture-template-prompt-history::"
				   (or prompt "")))
		  completions (mapcar 'list completions)))
	  (unless (boundp histvar) (set histvar nil))
	  (cond
	   ((member char '("G" "g"))
	    (let* ((org-last-tags-completion-table
		    (org-global-tags-completion-table
		     (if (equal char "G")
			 (org-agenda-files)
		       (and file (list file)))))
		   (org-add-colon-after-tag-completion t)
		   (ins (org-icompleting-read
			 (if prompt (concat prompt ": ") "Tags: ")
			 'org-tags-completion-function nil nil nil
			 'org-tags-history)))
	      (setq ins (mapconcat 'identity
				   (org-split-string
				    ins (org-re "[^[:alnum:]_@#%]+"))
				       ":"))
	      (when (string-match "\\S-" ins)
		(or (equal (char-before) ?:) (insert ":"))
		(insert ins)
		(or (equal (char-after) ?:) (insert ":"))
		(and (org-at-heading-p) (org-set-tags nil 'align)))))
	   ((equal char "C")
	    (cond ((= (length clipboards) 1) (insert (car clipboards)))
		  ((> (length clipboards) 1)
		   (insert (read-string "Clipboard/kill value: "
					(car clipboards) '(clipboards . 1)
					(car clipboards))))))
	   ((equal char "L")
	    (cond ((= (length clipboards) 1)
		   (org-insert-link 0 (car clipboards)))
		  ((> (length clipboards) 1)
		   (org-insert-link 0 (read-string "Clipboard/kill value: "
						   (car clipboards)
						   '(clipboards . 1)
						   (car clipboards))))))
	   ((equal char "p")
	    (org-set-property (org-substring-no-properties prompt) nil))
	   (char
	    ;; These are the date/time related ones
	    (setq org-time-was-given (equal (upcase char) char))
	    (setq time (org-read-date (equal (upcase char) char) t nil
				      prompt))
	    (if (equal (upcase char) char) (setq org-time-was-given t))
	    (org-insert-time-stamp time org-time-was-given
				   (member char '("u" "U"))
				   nil nil (list org-end-time-was-given)))
	   (t
	    (let (org-completion-use-ido)
	      (insert (org-completing-read-no-i
		       (concat (if prompt prompt "Enter string")
			       (if default (concat " [" default "]"))
			       ": ")
		       completions nil nil nil histvar default)))))))
      ;; Make sure there are no empty lines before the text, and that
      ;; it ends with a newline character
      (goto-char (point-min))
      (while (looking-at "[ \t]*\n") (replace-match ""))
      (if (re-search-forward "[ \t\n]*\\'" nil t) (replace-match "\n"))
      ;; Return the expanded template and kill the temporary buffer
      (untabify (point-min) (point-max))
      (set-buffer-modified-p nil)
      (prog1 (buffer-string) (kill-buffer (current-buffer))))))

(defun org-capture-escaped-% ()
  "Check if % was escaped - if yes, unescape it now."
  (if (equal (char-before (match-beginning 0)) ?\\)
      (progn
	(delete-region (1- (match-beginning 0)) (match-beginning 0))
	t)
    nil))

;;;###autoload
(defun org-capture-import-remember-templates ()
  "Set org-capture-templates to be similar to `org-remember-templates'."
  (interactive)
  (when (and (yes-or-no-p
	      "Import old remember templates into org-capture-templates? ")
	     (yes-or-no-p
	      "Note that this will remove any templates currently defined in `org-capture-templates'.  Do you still want to go ahead? "))
    (require 'org-remember)
    (setq org-capture-templates
	  (mapcar
	   (lambda (entry)
	     (let ((desc (car entry))
		   (key (char-to-string (nth 1 entry)))
		   (template (nth 2 entry))
		   (file (or (nth 3 entry) org-default-notes-file))
		   (position (or (nth 4 entry) org-remember-default-headline))
		   (type 'entry)
		   (prepend org-reverse-note-order)
		   immediate target)
	       (cond
		((member position '(top bottom))
		 (setq target (list 'file file)
		       prepend (eq position 'top)))
		((eq position 'date-tree)
		 (setq target (list 'file+datetree file)
		       prepend nil))
		(t (setq target (list 'file+headline file position))))

	       (when (string-match "%!" template)
		 (setq template (replace-match "" t t template)
		       immediate t))

	       (append (list key desc type target template)
		       (if prepend '(:prepend t))
		       (if immediate '(:immediate-finish t)))))

	   org-remember-templates))))

(provide 'org-capture)

;;; org-capture.el ends here
