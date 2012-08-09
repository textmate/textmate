;;; todo-mode.el --- major mode for editing TODO list files

;; Copyright (C) 1997, 1999, 2001-2012  Free Software Foundation, Inc.

;; Author: Oliver Seidel <privat@os10000.net>
;; Maintainer: Stephen Berman <stephen.berman@gmx.net>
;; Created: 2 Aug 1997
;; Keywords: calendar, todo

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

;; ---------------------------------------------------------------------------

;;; Commentary:

;;  Mode Description
;;
;;	TODO is a major mode for EMACS which offers functionality to
;;	treat most lines in one buffer as a list of items one has to
;;	do.  There are facilities to add new items, which are
;;	categorized, to edit or even delete items from the buffer.
;;	The buffer contents are currently compatible with the diary,
;;	so that the list of todo-items will show up in the FANCY diary
;;	mode.
;;
;;	Notice: Besides the major mode, this file also exports the
;;	function `todo-show' which will change to the one specific
;;	TODO file that has been specified in the todo-file-do
;;	variable.  If this file does not conform to the TODO mode
;;	conventions, the todo-show function will add the appropriate
;;	header and footer.  I don't anticipate this to cause much
;;	grief, but be warned, in case you attempt to read a plain text
;;	file.
;;
;;  Preface, Quickstart Installation
;;
;;      To get this to work, make Emacs execute the line
;;
;;          (autoload 'todo-mode "todo-mode"
;;                    "Major mode for editing TODO lists." t)
;;          (autoload 'todo-show "todo-mode"
;;                    "Show TODO items." t)
;;          (autoload 'todo-insert-item "todo-mode"
;;                    "Add TODO item." t)
;;
;;      You may now enter new items by typing "M-x todo-insert-item",
;;      or enter your TODO list file by typing "M-x todo-show".
;;
;;      The TODO list file has a special format and some auxiliary
;;      information, which will be added by the todo-show function if
;;      it attempts to visit an un-initialized file.  Hence it is
;;      recommended to use the todo-show function for the first time,
;;      in order to initialize the file, but it is not necessary
;;      afterwards.
;;
;;      As these commands are quite long to type, I would recommend
;;      the addition of two bindings to your to your global keymap.  I
;;      personally have the following in my initialization file:
;;
;;          (global-set-key "\C-ct" 'todo-show)  ; switch to TODO buffer
;;	    (global-set-key "\C-ci" 'todo-insert-item) ; insert new item
;;
;;      Note, however, that this recommendation has prompted some
;;      criticism, since the keys C-c LETTER are reserved for user
;;      functions.  I believe my recommendation is acceptable, since
;;      the Emacs Lisp Manual *Tips* section also details that the
;;      mode itself should not bind any functions to those keys.  The
;;      express aim of the above two bindings is to work outside the
;;      mode, which doesn't need the show function and offers a
;;      different binding for the insert function.  They serve as
;;      shortcuts and are not even needed (since the TODO mode will be
;;      entered by visiting the TODO file, and later by switching to
;;      its buffer).
;;
;;      If you are an advanced user of this package, please consult
;;      the whole source code for autoloads, because there are several
;;      extensions that are not explicitly listed in the above quick
;;      installation.
;;
;;  Pre-Requisites
;;
;;      This package will require the following packages to be
;;      available on the load-path:
;;
;;          time-stamp
;;          easymenu
;;
;;  Operation
;;
;;	You will have the following facilities available:
;;
;;	    M-x todo-show   will enter the todo list screen, here type
;;
;;	    +  to go to next category
;;          -  to go to previous category
;;          d  to file the current entry, including a
;;            			    comment and timestamp
;;          e  to edit the current entry
;;          E  to edit a multi-line entry
;;          f  to file the current entry, including a
;;            			    comment and timestamp
;;          i  to insert a new entry, with prefix, omit category
;;          I  to insert a new entry at current cursor position
;;	    j  jump to category
;;          k  to kill the current entry
;;          l  to lower the current entry's priority
;;          n  for the next entry
;;          p  for the previous entry
;;	    P  print
;;          q  to save the list and exit the buffer
;;          r  to raise the current entry's priority
;;          s  to save the list
;;          S  to save the list of top priorities
;;	    t  show top priority items for each category
;;
;;	When you add a new entry, you are asked for the text and then
;;	for the category.  I for example have categories for things
;;	that I want to do in the office (like mail my mum), that I
;;	want to do in town (like buy cornflakes) and things I want to
;;	do at home (move my suitcases).  The categories can be
;;	selected with the cursor keys and if you type in the name of a
;;	category which didn't exist before, an empty category of the
;;	desired name will be added and filled with the new entry.
;;
;;  Configuration
;;
;;  Variable todo-prefix
;;
;;	I would like to recommend that you use the prefix "*/*" (by
;;	leaving the variable 'todo-prefix' untouched) so that the
;;	diary displays each entry every day.
;;
;;	To understand what I mean, please read the documentation that
;;	goes with the calendar since that will tell you how you can
;;	set up the fancy diary display and use the #include command to
;;	include your todo list file as part of your diary.
;;
;;	If you have the diary package set up to usually display more
;;	than one day's entries at once, consider using
;;
;;	    "&%%(equal (calendar-current-date) date)"
;;
;;	as the value of `todo-prefix'.  Please note that this may slow
;;	down the processing of your diary file some.
;;
;;      Carsten Dominik <dominik@strw.LeidenUniv.nl> suggested that
;;
;;          "&%%(todo-cp)"
;;
;;      might be nicer and to that effect a function has been declared
;;      further down in the code.  You may wish to auto-load this.
;;
;;      Carsten also writes that that *changing* the prefix after the
;;      todo list is already established is not as simple as changing
;;      the variable - the todo files have to be changed by hand.
;;
;;  Variable todo-file-do
;;
;;	This variable is fairly self-explanatory.  You have to store
;;	your TODO list somewhere.  This variable tells the package
;;	where to go and find this file.
;;
;;  Variable todo-file-done
;;
;;	Even when you're done, you may wish to retain the entries.
;;	Given that they're timestamped and you are offered to add a
;;	comment, this can make a useful diary of past events.  It will
;;	even blend in with the EMACS diary package.  So anyway, this
;;	variable holds the name of the file for the filed todo-items.
;;
;;  Variable todo-file-top
;;
;;      File storing the top priorities of your TODO list when
;;      todo-save-top-priorities is non-nil.  Nice to include in your
;;      diary instead of the complete TODO list.
;;
;;  Variable todo-mode-hook
;;
;;	Just like other modes, too, this mode offers to call your
;;	functions before it goes about its business.  This variable
;;	will be inspected for any functions you may wish to have
;;	called once the other TODO mode preparations have been
;;	completed.
;;
;;  Variable todo-insert-threshold
;;
;;     	Another nifty feature is the insertion accuracy.  If you have
;;     	8 items in your TODO list, then you may get asked 4 questions
;;     	by the binary insertion algorithm.  However, you may not
;;     	really have a need for such accurate priorities amongst your
;;     	TODO items.  If you now think about the binary insertion
;;     	halving the size of the window each time, then the threshold
;;     	is the window size at which it will stop.  If you set the
;;     	threshold to zero, the upper and lower bound will coincide at
;;     	the end of the loop and you will insert your item just before
;;     	that point.  If you set the threshold to, e.g. 8, it will stop
;;     	as soon as the window size drops below that amount and will
;;     	insert the item in the approximate center of that window.  I
;;     	got the idea for this feature after reading a very helpful
;;     	e-mail reply from Trey Jackson <trey@cs.berkeley.edu> who
;;     	corrected some of my awful coding and pointed me towards some
;;     	good reading.  Thanks Trey!
;;
;;  Things to do
;;
;;      These originally were my ideas, but now also include all the
;;      suggestions that I included before forgetting them:
;;
;;      o   Fancy fonts for todo/top-priority buffer
;;      o   Remove todo-prefix option in todo-top-priorities
;;      o   Rename category
;;      o   Move entry from one category to another one
;;      o   Entries which both have the generic */* prefix and a
;;          "deadline" entry which are understood by diary, indicating
;;          an event (unless marked by &)
;;      o   The optional COUNT variable of todo-forward-item should be
;;          applied to the other functions performing similar tasks
;;      o   Modularization could be done for repeated elements of
;;          the code, like the completing-read lines of code.
;;	o   license / version function
;;	o   export to diary file
;;	o   todo-report-bug
;;	o   GNATS support
;;	o   elide multiline (as in bbdb, or, to a lesser degree, in
;;          outline mode)
;;	o   rewrite complete package to store data as Lisp objects
;;          and have display modes for display, for diary export,
;;          etc.  (Richard Stallman pointed out this is a bad idea)
;;      o   so base todo-mode.el on generic-mode.el instead
;;
;;  History and Gossip
;;
;;	Many thanks to all the ones who have contributed to the
;;	evolution of this package!  I hope I have listed all of you
;;	somewhere in the documentation or at least in the RCS history!
;;
;;	Enjoy this package and express your gratitude by sending nice
;;	things to my parents' address!
;;
;;	Oliver Seidel
;;	(Lessingstr.  8, 65760 Eschborn, Federal Republic of Germany)

;;; Code:

(require 'time-stamp)


;; User-configurable variables:

(defgroup todo nil
  "Maintain a list of todo items."
  :link '(emacs-commentary-link "todo-mode")
  :version "21.1"
  :group 'calendar)

(defcustom todo-prefix     "*/*"
  "TODO mode prefix for entries.

This is useful in conjunction with `calendar' and `diary' if you use

#include \"~/.todo-do\"

in your diary file to include your todo list file as part of your
diary.  With the default value \"*/*\" the diary displays each entry
every day and it may also be marked on every day of the calendar.
Using \"&%%(equal (calendar-current-date) date)\" instead will only
show and mark todo entries for today, but may slow down processing of
the diary file somewhat."
  :type 'string
  :group 'todo)
(defcustom todo-file-do    (convert-standard-filename "~/.todo-do")
  "TODO mode list file."
  :type 'file
  :group 'todo)
(defcustom todo-file-done  (convert-standard-filename "~/.todo-done")
  "TODO mode archive file."
  :type 'file
  :group 'todo)
(defcustom todo-mode-hook  nil
  "TODO mode hooks."
  :type 'hook
  :group 'todo)
(defcustom todo-edit-mode-hook nil
  "TODO Edit mode hooks."
  :type 'hook
  :group 'todo)
(defcustom todo-insert-threshold 0
  "TODO mode insertion accuracy.

If you have 8 items in your TODO list, then you may get asked 4
questions by the binary insertion algorithm.  However, you may not
really have a need for such accurate priorities amongst your TODO
items.  If you now think about the binary insertion halving the size
of the window each time, then the threshold is the window size at
which it will stop.  If you set the threshold to zero, the upper and
lower bound will coincide at the end of the loop and you will insert
your item just before that point.  If you set the threshold to,
e.g. 8, it will stop as soon as the window size drops below that
amount and will insert the item in the approximate center of that
window."
  :type 'integer
  :group 'todo)
(defvar todo-edit-buffer " *TODO Edit*"
  "TODO Edit buffer name.")
(defcustom todo-file-top (convert-standard-filename "~/.todo-top")
  "TODO mode top priorities file.

Not in TODO format, but diary compatible.
Automatically generated when `todo-save-top-priorities' is non-nil."
  :type 'string
  :group 'todo)

(defcustom todo-print-function 'ps-print-buffer-with-faces
  "Function to print the current buffer."
  :type 'symbol
  :group 'todo)
(defcustom todo-show-priorities 1
  "Default number of priorities to show by \\[todo-top-priorities].
0 means show all entries."
  :type 'integer
  :group 'todo)
(defcustom todo-print-priorities 0
  "Default number of priorities to print by \\[todo-print].
0 means print all entries."
  :type 'integer
  :group 'todo)
(defcustom todo-remove-separator t
  "Non-nil to remove category separators in\
\\[todo-top-priorities] and \\[todo-print]."
  :type 'boolean
  :group 'todo)
(defcustom todo-save-top-priorities-too t
  "Non-nil makes `todo-save' automatically save top-priorities in `todo-file-top'."
  :type 'boolean
  :group 'todo)

;; Thanks for the ISO time stamp format go to Karl Eichwalder <ke@suse.de>
;; My format string for the appt.el package is "%3b %2d, %y, %02I:%02M%p".
;;
(defcustom todo-time-string-format
  "%:y-%02m-%02d %02H:%02M"
  "TODO mode time string format for done entries.
For details see the variable `time-stamp-format'."
  :type 'string
  :group 'todo)

(defcustom todo-entry-prefix-function 'todo-entry-timestamp-initials
  "Function producing text to insert at start of todo entry."
  :type 'symbol
  :group 'todo)
(defcustom todo-initials (or (getenv "INITIALS") (user-login-name))
  "Initials of todo item author."
  :type 'string
  :group 'todo)

(defun todo-entry-timestamp-initials ()
  "Prepend timestamp and your initials to the head of a TODO entry."
  (let ((time-stamp-format todo-time-string-format))
    (concat (time-stamp-string) " " todo-initials ": ")))

;; ---------------------------------------------------------------------------

;; Set up some helpful context ...

(defvar todo-categories nil
  "TODO categories.")

(defvar todo-cats nil
  "Old variable for holding the TODO categories.
Use `todo-categories' instead.")

(defvar todo-previous-line 0
  "Previous line asked about.")

(defvar todo-previous-answer 0
  "Previous answer got.")

(defvar todo-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'todo-forward-category)
    (define-key map "-" 'todo-backward-category)
    (define-key map "d" 'todo-file-item) ;done/delete
    (define-key map "e" 'todo-edit-item)
    (define-key map "E" 'todo-edit-multiline)
    (define-key map "f" 'todo-file-item)
    (define-key map "i" 'todo-insert-item)
    (define-key map "I" 'todo-insert-item-here)
    (define-key map "j" 'todo-jump-to-category)
    (define-key map "k" 'todo-delete-item)
    (define-key map "l" 'todo-lower-item)
    (define-key map "n" 'todo-forward-item)
    (define-key map "p" 'todo-backward-item)
    (define-key map "P" 'todo-print)
    (define-key map "q" 'todo-quit)
    (define-key map "r" 'todo-raise-item)
    (define-key map "s" 'todo-save)
    (define-key map "S" 'todo-save-top-priorities)
    (define-key map "t" 'todo-top-priorities)
    map)
  "TODO mode keymap.")

(defvar todo-category-number 0 "TODO category number.")

(defvar todo-tmp-buffer-name " *todo tmp*")

(defvar todo-category-sep (make-string 75 ?-)
  "Category separator.")

(defvar todo-category-beg " --- "
  "Category start separator to be prepended onto category name.")

(defvar todo-category-end "--- End"
  "Separator after a category.")

(defvar todo-header "-*- mode: todo; "
  "Header of todo files.")

;; ---------------------------------------------------------------------------

(defun todo-category-select ()
  "Make TODO mode display the current category correctly."
  (let ((name (nth todo-category-number todo-categories)))
    (setq mode-line-buffer-identification
;;          (concat "Category: " name))
          (concat "Category: " (format "%18s" name)))
    (widen)
    (goto-char (point-min))
    (search-forward-regexp
     (concat "^"
             (regexp-quote (concat todo-prefix todo-category-beg name))
             "$"))
    (let ((begin (1+ (line-end-position))))
      (search-forward-regexp (concat "^" todo-category-end))
      (narrow-to-region begin (line-beginning-position))
      (goto-char (point-min)))))
(defalias 'todo-cat-slct 'todo-category-select)

(defun todo-forward-category ()
  "Go forward to TODO list of next category."
  (interactive)
  (setq todo-category-number
        (mod (1+ todo-category-number) (length todo-categories)))
  (todo-category-select))
(defalias 'todo-cmd-forw 'todo-forward-category)

(defun todo-backward-category ()
  "Go back to TODO list of previous category."
  (interactive)
  (setq todo-category-number
        (mod (1- todo-category-number) (length todo-categories)))
  (todo-category-select))
(defalias 'todo-cmd-back 'todo-backward-category)

(defun todo-backward-item ()
  "Select previous entry of TODO list."
  (interactive)
  (search-backward-regexp (concat "^" (regexp-quote todo-prefix)) nil t)
  (message ""))
(defalias 'todo-cmd-prev 'todo-backward-item)

(defun todo-forward-item (&optional count)
  "Select COUNT-th next entry of TODO list."
  (interactive "P")
  (if (listp count) (setq count (car count)))
  (end-of-line)
  (search-forward-regexp (concat "^" (regexp-quote todo-prefix))
                         nil 'goto-end count)
  (beginning-of-line)
  (message ""))
(defalias 'todo-cmd-next 'todo-forward-item)

(defun todo-save ()
  "Save the TODO list."
  (interactive)
  (save-excursion
    (save-restriction
      (save-buffer)))
  (if todo-save-top-priorities-too (todo-save-top-priorities)))
(defalias 'todo-cmd-save 'todo-save)

(defun todo-quit ()
  "Done with TODO list for now."
  (interactive)
  (widen)
  (todo-save)
  (message "")
  (bury-buffer))
(defalias 'todo-cmd-done 'todo-quit)

(defun todo-edit-item ()
  "Edit current TODO list entry."
  (interactive)
  (if (< (point-min) (point-max))
      (let ((item (todo-item-string)))
	(if (todo-string-multiline-p item)
	    (todo-edit-multiline)
	  (let ((new (read-from-minibuffer "Edit: " item)))
	    (todo-remove-item)
	    (insert new "\n")
	    (todo-backward-item)
	    (message ""))))
    (error "No TODO list entry to edit")))
(defalias 'todo-cmd-edit 'todo-edit-item)

(defun todo-edit-multiline ()
  "Set up a buffer for editing a multiline TODO list entry."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name todo-edit-buffer)))
    (switch-to-buffer
     (make-indirect-buffer
      (file-name-nondirectory todo-file-do) buffer-name))
    (message "To exit, simply kill this buffer and return to list.")
    (todo-edit-mode)
    (narrow-to-region (todo-item-start) (todo-item-end))))

;;;###autoload
(defun todo-add-category (&optional cat)
  "Add new category CAT to the TODO list."
  (interactive)
  (let ((buf (find-file-noselect todo-file-do t))
	(prompt "Category: "))
    (unless (zerop (buffer-size buf))
      (and (null todo-categories)
	   (null todo-cats)
	   (error "Error in %s: File is non-empty but contains no category"
		  todo-file-do)))
    (unless cat (setq cat (read-from-minibuffer prompt)))
    (with-current-buffer buf
      ;; reject names that could induce bugs and confusion
      (while (and (cond ((string= "" cat)
			 (setq prompt "Enter a non-empty category name: "))
			((string-match "\\`\\s-+\\'" cat)
			 (setq prompt "Enter a category name that is not only white space: "))
			((member cat todo-categories)
			 (setq prompt "Enter a non-existing category name: ")))
		  (setq cat (read-from-minibuffer prompt))))
      ;; initialize a newly created Todo buffer for Todo mode
      (unless (file-exists-p todo-file-do) (todo-mode))
      (setq todo-categories (cons cat todo-categories))
      (widen)
      (goto-char (point-min))
      (if (search-forward "-*- mode: todo; " (+ (point-min) 16) t)
	  (kill-line)
	(insert "-*- mode: todo; \n")
	(forward-char -1))
      (insert (format "todo-categories: %S; -*-" todo-categories))
      (forward-char 1)
      (insert (format "%s%s%s\n%s\n%s %s\n"
		      todo-prefix todo-category-beg cat
		      todo-category-end
		      todo-prefix todo-category-sep))
      (if (called-interactively-p 'interactive)
	  ;; properly display the newly added category
	  (progn (setq todo-category-number 0) (todo-show))
	0))))

;;;###autoload
(defun todo-add-item-non-interactively (new-item category)
  "Insert NEW-ITEM in TODO list as a new entry in CATEGORY."
  (save-excursion
    (todo-show))
  (save-excursion
    (if (string= "" category)
        (setq category (nth todo-category-number todo-categories)))
    (let ((cat-exists (member category todo-categories)))
      (setq todo-category-number
            (if cat-exists
                (- (length todo-categories) (length cat-exists))
              (todo-add-category category))))
    (todo-show)
    (setq todo-previous-line 0)
    (let ((top 1)
	  (bottom (1+ (count-lines (point-min) (point-max)))))
      (while (> (- bottom top) todo-insert-threshold)
	(let* ((current (/ (+ top bottom) 2))
	       (answer (if (< current bottom)
			   (todo-more-important-p current) nil)))
	  (if answer
	      (setq bottom current)
	    (setq top (1+ current)))))
      (setq top (/ (+ top bottom) 2))
      ;; goto-line doesn't have the desired behavior in a narrowed buffer.
      (goto-char (point-min))
      (forward-line (1- top)))
    (insert new-item "\n")
    (todo-backward-item)
    (todo-save)
    (message "")))

;;;###autoload
(defun todo-insert-item (arg)
  "Insert new TODO list entry.
With a prefix argument ARG solicit the category, otherwise use the current
category."
  (interactive "P")
  (save-excursion
    (if (not (derived-mode-p 'todo-mode)) (todo-show))
    (let* ((new-item (concat todo-prefix " "
			     (read-from-minibuffer
			      "New TODO entry: "
			      (if todo-entry-prefix-function
				  (funcall todo-entry-prefix-function)))))
	   (current-category (nth todo-category-number todo-categories))
	   (category (if arg (todo-completing-read) current-category)))
      (todo-add-item-non-interactively new-item category))))

(defalias 'todo-cmd-inst 'todo-insert-item)

(defun todo-insert-item-here ()
  "Insert a new TODO list entry directly above the entry at point.
If point is on an empty line, insert the entry there."
  (interactive)
  (if (not (derived-mode-p 'todo-mode)) (todo-show))
  (let ((new-item (concat todo-prefix " "
			  (read-from-minibuffer
			   "New TODO entry: "
			   (if todo-entry-prefix-function
			       (funcall todo-entry-prefix-function))))))
    (unless (and (bolp) (eolp)) (todo-item-start))
    (insert (concat new-item "\n"))
    (backward-char)
    ;; put point at start of new entry
    (todo-item-start)))

(defun todo-more-important-p (line)
  "Ask whether entry is more important than the one at LINE."
  (unless (equal todo-previous-line line)
    (setq todo-previous-line line)
    (goto-char (point-min))
    (forward-line (1- todo-previous-line))
    (let ((item (todo-item-string-start)))
      (setq todo-previous-answer
            (y-or-n-p (concat "More important than '" item "'? ")))))
  todo-previous-answer)
(defalias 'todo-ask-p 'todo-more-important-p)

(defun todo-delete-item ()
  "Delete current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((todo-entry (todo-item-string-start))
             (todo-answer (y-or-n-p (concat "Permanently remove '"
                                            todo-entry "'? "))))
        (when todo-answer
          (todo-remove-item)
          (todo-backward-item))
        (message ""))
    (error "No TODO list entry to delete")))
(defalias 'todo-cmd-kill 'todo-delete-item)

(defun todo-raise-item ()
  "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point)) 0)
      (let ((item (todo-item-string)))
        (todo-remove-item)
        (todo-backward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No TODO list entry to raise")))
(defalias 'todo-cmd-rais 'todo-raise-item)

(defun todo-lower-item ()
  "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point) (point-max)) 1)
      ;; Assume there is a final newline
      (let ((item (todo-item-string)))
        (todo-remove-item)
        (todo-forward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No TODO list entry to lower")))
(defalias 'todo-cmd-lowr 'todo-lower-item)

(defun todo-file-item (&optional comment)
  "File the current TODO list entry away, annotated with an optional COMMENT."
  (interactive "sComment: ")
  (or (> (count-lines (point-min) (point-max)) 0)
      (error "No TODO list entry to file away"))
  (let ((time-stamp-format todo-time-string-format))
    (when (and comment (> (length comment) 0))
      (goto-char (todo-item-end))
      (insert
       (if (save-excursion (beginning-of-line)
                           (looking-at (regexp-quote todo-prefix)))
           " "
         "\n\t")
       "(" comment ")"))
    (goto-char (todo-item-end))
    (insert " [" (nth todo-category-number todo-categories) "]")
    (todo-item-start)
    (let ((temp-point (point)))
      (if (looking-at (regexp-quote todo-prefix))
	  (replace-match (time-stamp-string))
	;; Standard prefix -> timestamp
	;; Else prefix non-standard item start with timestamp
	(insert (time-stamp-string)))
      (append-to-file temp-point (todo-item-end 'include-sep) todo-file-done)
      (delete-region temp-point (todo-item-end 'include-sep)))
    (todo-backward-item)
    (message "")))

;; ---------------------------------------------------------------------------

;; Utility functions:


;;;###autoload
(defun todo-top-priorities (&optional nof-priorities category-pr-page
                                      interactive)
  "List top priorities for each category.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to `todo-show-priorities'.

If CATEGORY-PR-PAGE is non-nil, a page separator \'^L\' is inserted
between each category.
INTERACTIVE should be non-nil if this function is called interactively."

  (interactive "P\ni\nP")
  (or nof-priorities (setq nof-priorities todo-show-priorities))
  (if (listp nof-priorities)            ;universal argument
      (setq nof-priorities (car nof-priorities)))
  (let ((todo-print-buffer-name todo-tmp-buffer-name)
        ;;(todo-print-category-number 0)
        (todo-category-break (if category-pr-page "" ""))
        (cat-end
         (concat
          (if todo-remove-separator
              (concat todo-category-end "\n"
                      (regexp-quote todo-prefix) " " todo-category-sep "\n")
            (concat todo-category-end "\n"))))
        beg end)
    (save-excursion
      (todo-show)
      (save-restriction
	(save-current-buffer
	  (widen)
	  (copy-to-buffer todo-print-buffer-name (point-min) (point-max))
	  (set-buffer todo-print-buffer-name)
	  (goto-char (point-min))
	  (when (re-search-forward (regexp-quote todo-header) nil t)
	    (beginning-of-line 1)
	    (delete-region (point) (line-end-position)))
	  (while (re-search-forward       ;Find category start
		  (regexp-quote (concat todo-prefix todo-category-beg))
		  nil t)
	    (setq beg (+ (line-end-position) 1)) ;Start of first entry.
	    (re-search-forward cat-end nil t)
	    (setq end (match-beginning 0))
	    (replace-match todo-category-break)
	    (narrow-to-region beg end)    ;In case we have too few entries.
	    (goto-char (point-min))
	    (if (zerop nof-priorities)      ;Traverse entries.
		(goto-char end)            ;All entries
	      (todo-forward-item nof-priorities))
	    (setq beg (point))
	    (delete-region beg end)
	    (widen))
	  (and (looking-at "") (replace-match "")) ;Remove trailing form-feed.
	  (goto-char (point-min))         ;Due to display buffer
	  )))
    (when interactive (display-buffer todo-print-buffer-name))
    (message "Type C-x 1 to remove %s window.  M-C-v to scroll the help."
             todo-print-buffer-name)))

(defun todo-save-top-priorities (&optional nof-priorities)
  "Save top priorities for each category in `todo-file-top'.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to `todo-show-priorities'."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (todo-top-priorities nof-priorities)
        (set-buffer todo-tmp-buffer-name)
        (write-file todo-file-top)
        (kill-this-buffer)))))

;;;###autoload
(defun todo-print (&optional category-pr-page)
  "Print todo summary using `todo-print-function'.
If CATEGORY-PR-PAGE is non-nil, a page separator `^L' is inserted
between each category.

Number of entries for each category is given by `todo-print-priorities'."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
	(todo-top-priorities todo-print-priorities
			     category-pr-page)
	(set-buffer todo-tmp-buffer-name)
	(and (funcall todo-print-function)
	     (kill-this-buffer))
	(message "Todo printing done.")))))

(defun todo-jump-to-category ()
  "Jump to a category.  Default is previous category."
  (interactive)
  (let ((category (todo-completing-read)))
    (if (string= "" category)
        (setq category (nth todo-category-number todo-categories)))
    (setq todo-category-number
          (if (member category todo-categories)
              (- (length todo-categories)
                 (length (member category todo-categories)))
            (todo-add-category category)))
    (todo-show)))

(defun todo-line-string ()
  "Return current line in buffer as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun todo-item-string-start ()
  "Return the start of this TODO list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (todo-item-string)))
    (if (> (length item) 60)
        (setq item (concat (substring item 0 56) "...")))
    item))

(defun todo-item-start ()
  "Go to start of current TODO list item and return point."
  (beginning-of-line)
  (if (not (looking-at (regexp-quote todo-prefix)))
      (search-backward-regexp
       (concat "^" (regexp-quote todo-prefix)) nil t))
  (point))

(defun todo-item-end (&optional include-sep)
  "Return point at end of current TODO list item.
If INCLUDE-SEP is non-nil, return point after the separator."
  (save-excursion
    (end-of-line)
    (if (search-forward-regexp
         (concat "^" (regexp-quote todo-prefix)) nil 'goto-end)
        (goto-char (match-beginning 0)))
    (unless include-sep (skip-chars-backward "\n"))
    (point)))

(defun todo-remove-item ()
  "Delete the current entry from the TODO list."
  (delete-region (todo-item-start) (todo-item-end 'include-sep)))

(defun todo-item-string ()
  "Return current TODO list entry as a string."
  (buffer-substring (todo-item-start) (todo-item-end)))

(defun todo-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todo-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (todo-string-count-lines string) 1))

(defun todo-completing-read ()
  "Return a category name, with completion, for use in Todo mode."
  ;; make a copy of todo-categories in case history-delete-duplicates is
  ;; non-nil, which makes completing-read alter todo-categories
  (let* ((categories (copy-sequence todo-categories))
	 (history (cons 'todo-categories (1+ todo-category-number)))
	 (default (nth todo-category-number todo-categories))
	 (category (completing-read
		    (concat "Category [" default "]: ")
		    todo-categories nil nil nil history default)))
    ;; restore the original value of todo-categories
    (setq todo-categories categories)
    category))

;; ---------------------------------------------------------------------------

(easy-menu-define todo-menu todo-mode-map "Todo Menu"
                  '("Todo"
                    ["Next category"        todo-forward-category t]
                    ["Previous category"    todo-backward-category t]
                    ["Jump to category"     todo-jump-to-category t]
                    ["Show top priority items" todo-top-priorities t]
                    ["Print categories"     todo-print t]
                    "---"
                    ["Edit item"            todo-edit-item t]
                    ["File item"            todo-file-item t]
                    ["Insert new item"      todo-insert-item t]
                    ["Insert item here"     todo-insert-item-here t]
                    ["Kill item"            todo-delete-item t]
                    "---"
                    ["Lower item priority"  todo-lower-item t]
                    ["Raise item priority"  todo-raise-item t]
                    "---"
                    ["Next item"            todo-forward-item t]
                    ["Previous item"        todo-backward-item t]
                    "---"
                    ["Save"                 todo-save t]
                    ["Save Top Priorities"  todo-save-top-priorities t]
                    "---"
                    ["Quit"                 todo-quit t]
                    ))

;; As calendar reads .todo-do before todo-mode is loaded.
;;;###autoload
(define-derived-mode todo-mode nil "TODO"
  "Major mode for editing TODO lists."
  (easy-menu-add todo-menu))

(defvar date)
(defvar entry)

;; t-c should be used from diary code, which requires calendar.
(declare-function calendar-current-date "calendar" (&optional offset))

;; Read about this function in the setup instructions above!
;;;###autoload
(defun todo-cp ()
  "Make a diary entry appear only in the current date's diary."
  (if (equal (calendar-current-date) date)
      entry))

(define-derived-mode todo-edit-mode text-mode "TODO Edit"
  "Major mode for editing items in the TODO list.

\\{todo-edit-mode-map}")

;;;###autoload
(defun todo-show ()
  "Show TODO list."
  (interactive)
  ;; Call todo-initial-setup only if there is neither a Todo file nor
  ;; a corresponding unsaved buffer.
  (if (or (file-exists-p todo-file-do)
	  (let* ((buf (get-buffer (file-name-nondirectory todo-file-do)))
		 (bufname (buffer-file-name buf)))
	    (equal (expand-file-name todo-file-do) bufname)))
      (find-file todo-file-do)
    (todo-initial-setup))
  (if (null todo-categories)
      (if (null todo-cats)
          (error "Error in %s: No categories in list `todo-categories'"
                 todo-file-do)
        (goto-char (point-min))
        (and (search-forward "todo-cats:" nil t)
             (replace-match "todo-categories:"))
        (make-local-variable 'todo-categories)
        (setq todo-categories todo-cats)))
  (beginning-of-line)
  (todo-category-select))

(defun todo-initial-setup ()
  "Set up things to work properly in TODO mode."
  (find-file todo-file-do)
  (erase-buffer)
  (todo-mode)
  (todo-add-category "Todo"))

(provide 'todo-mode)

;;; todo-mode.el ends here
