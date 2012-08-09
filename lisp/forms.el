;;; forms.el --- Forms mode: edit a file as a form to fill in

;; Copyright (C) 1991, 1994-1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Johan Vromans <jvromans@squirrel.nl>

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

;; Visit a file using a form.  See forms-d2.el for examples.
;;
;; === Naming conventions
;;
;; The names of all variables and functions start with 'forms-'.
;; Names which start with 'forms--' are intended for internal use, and
;; should *NOT* be used from the outside.
;;
;; All variables are buffer-local, to enable multiple forms visits
;; simultaneously.
;; Variable `forms--mode-setup' is local to *ALL* buffers, for it
;; controls if forms-mode has been enabled in a buffer.
;;
;; === How it works ===
;;
;; Forms mode means visiting a data file which is supposed to consist
;; of records each containing a number of fields.  The records are
;; separated by a newline, the fields are separated by a user-defined
;; field separator (default: TAB).
;; When shown, a record is transferred to an Emacs buffer and
;; presented using a user-defined form.  One record is shown at a
;; time.
;;
;; Forms mode is a composite mode.  It involves two files, and two
;; buffers.
;; The first file, called the control file, defines the name of the
;; data file and the forms format.  This file buffer will be used to
;; present the forms.
;; The second file holds the actual data.  The buffer of this file
;; will be buried, for it is never accessed directly.
;;
;; Forms mode is invoked using M-x `forms-find-file' control-file.
;; Alternatively `forms-find-file-other-window' can be used.
;;
;; You may also visit the control file, and switch to forms mode by hand
;; with M-x `forms-mode'.
;;
;; Automatic mode switching is supported if you specify
;; "-*- forms -*-" in the first line of the control file.
;;
;; The control file is visited, evaluated using `eval-buffer',
;; and should set at least the following variables:
;;
;;	forms-file				[string]
;;			The name of the data file.
;;
;;	forms-number-of-fields			[integer]
;;			The number of fields in each record.
;;
;;	forms-format-list			[list]
;;			Formatting instructions.
;;
;; `forms-format-list' should be a list, each element containing
;;
;;   - a string, e.g. "hello".  The string is inserted in the forms
;;	"as is".
;;
;;   - an integer, denoting a field number.
;;	The contents of this field are inserted at this point.
;;     Fields are numbered starting with number one.
;;
;;   - a function call, e.g. (insert "text").
;;	This function call is dynamically evaluated and should return a
;;     string.  It should *NOT* have side-effects on the forms being
;;     constructed.  The current fields are available to the function
;;     in the variable `forms-fields', they should *NOT* be modified.
;;
;;   - a lisp symbol, that must evaluate to one of the above.
;;
;; Optional variables which may be set in the control file:
;;
;;	forms-field-sep				[string, default TAB]
;;			The field separator used to separate the
;;			fields in the data file.  It may be a string.
;;
;;	forms-read-only				[bool, default nil]
;;			Non-nil means that the data file is visited
;;			read-only (view mode) as opposed to edit mode.
;;			If no write access to the data file is
;;			possible, view mode is enforced.
;;
;;	forms-check-number-of-fields            [bool, default t]
;;			If non-nil, a warning will be issued whenever
;;			a record is found that does not have the number
;;			of fields specified by `forms-number-of-fields'.
;;
;;	forms-multi-line			[string, default "^K"]
;;			If non-null, the records of the data file may
;;			contain fields that can span multiple lines in
;;			the form.
;;			This variable denotes the separator string
;;			to be used for this purpose.  Upon display, all
;;			occurrences of this string are translated
;;			to newlines.  Upon storage they are translated
;;			back to the separator string.
;;
;;	forms-forms-scroll			[bool, default nil]
;;			Non-nil means: rebind locally the commands that
;;			perform `scroll-up' or `scroll-down' to use
;;			`forms-next-field' resp. `forms-prev-field'.
;;
;;	forms-forms-jump			[bool, default nil]
;;			Non-nil means: rebind locally the commands
;;			`beginning-of-buffer' and `end-of-buffer' to
;;			perform, respectively, `forms-first-record' and
;;			`forms-last-record' instead.
;;
;;	forms-insert-after			[bool, default nil]
;;			Non-nil means: insertions of new records go after
;;			current record, also initial position is at the
;;			last record.  The default is to insert before the
;;			current record and the initial position is at the
;;			first record.
;;
;;	forms-read-file-filter			[symbol, default nil]
;;			If not nil: this should be the name of a
;;			function that is called after the forms data file
;;			has been read.  It can be used to transform
;;			the contents of the file into a format more suitable
;;			for forms-mode processing.
;;
;;	forms-write-file-filter			[symbol, default nil]
;;			If not nil: this should be the name of a
;;			function that is called before the forms data file
;;			is written (saved) to disk.  It can be used to undo
;;			the effects of `forms-read-file-filter', if any.
;;
;;	forms-new-record-filter			[symbol, default nil]
;;			If not nil: this should be the name of a
;;			function that is called when a new
;;			record is created.  It can be used to fill in
;;			the new record with default fields, for example.
;;
;;	forms-modified-record-filter		[symbol, default nil]
;;			If not nil: this should be the name of a
;;			function that is called when a record has
;;			been modified.  It is called after the fields
;;			are parsed.  It can be used to register
;;			modification dates, for example.
;;
;;	forms-use-text-properties		[bool, see text for default]
;;			This variable controls if forms mode should use
;;			text properties to protect the form text from being
;;			modified (using text-property `read-only').
;;			Also, the read-write fields are shown using a
;;			distinct face, if possible.
;;			As of emacs 19.29, the `intangible' text property
;;			is used to prevent moving into read-only fields.
;;			This variable defaults to t if running Emacs 19 or
;;			later with text properties.
;;			The default face to show read-write fields is
;;			copied from face `region'.
;;
;;	forms-ro-face 				[symbol, default 'default]
;;			This is the face that is used to show
;;			read-only text on the screen.  If used, this
;;			variable should be set to a symbol that is a
;;			valid face.
;;			E.g.
;;			  (make-face 'my-face)
;;			  (setq forms-ro-face 'my-face)
;;
;;	forms-rw-face				[symbol, default 'region]
;;			This is the face that is used to show
;;			read-write text on the screen.
;;
;; After evaluating the control file, its buffer is cleared and used
;; for further processing.
;; The data file (as designated by `forms-file') is visited in a buffer
;; `forms--file-buffer' which normally will not be shown.
;; Great malfunctioning may be expected if this file/buffer is modified
;; outside of this package while it is being visited!
;;
;; Normal operation is to transfer one line (record) from the data file,
;; split it into fields (into `forms--the-record-list'), and display it
;; using the specs in `forms-format-list'.
;; A format routine `forms--format' is built upon startup to format
;; the records according to `forms-format-list'.
;;
;; When a form is changed the record is updated as soon as this form
;; is left.  The contents of the form are parsed using information
;; obtained from `forms-format-list', and the fields which are
;; deduced from the form are modified.  Fields not shown on the forms
;; retain their original values.  The newly formed record then
;; replaces the contents of the old record in `forms--file-buffer'.
;; A parse routine `forms--parser' is built upon startup to parse
;; the records.
;;
;; Two exit functions exist: `forms-exit' and `forms-exit-no-save'.
;; `forms-exit' saves the data to the file, if modified.
;; `forms-exit-no-save' does not.  However, if `forms-exit-no-save'
;; is executed and the file buffer has been modified, Emacs will ask
;; questions anyway.
;;
;; Other functions provided by forms mode are:
;;
;;	paging (forward, backward) by record
;;	jumping (first, last, random number)
;;	searching
;;	creating and deleting records
;;	reverting the form (NOT the file buffer)
;;	switching edit <-> view mode v.v.
;;	jumping from field to field
;;
;; As a documented side-effect: jumping to the last record in the
;; file (using forms-last-record) will adjust forms--total-records if
;; needed.
;;
;; The forms buffer can be in one of two modes: edit mode or view
;; mode.  View mode is a read-only mode, whereby you cannot modify the
;; contents of the buffer.
;;
;; Edit mode commands:
;;
;; TAB		 forms-next-field
;; \C-c TAB	 forms-next-field
;; \C-c <	 forms-first-record
;; \C-c >	 forms-last-record
;; \C-c ?	 describe-mode
;; \C-c \C-k	 forms-delete-record
;; \C-c \C-q	 forms-toggle-read-only
;; \C-c \C-o	 forms-insert-record
;; \C-c \C-l	 forms-jump-record
;; \C-c \C-n	 forms-next-record
;; \C-c \C-p	 forms-prev-record
;; \C-c \C-r	 forms-search-backward
;; \C-c \C-s	 forms-search-forward
;; \C-c \C-x	 forms-exit
;;
;; Read-only mode commands:
;;
;; SPC 	 forms-next-record
;; DEL	 forms-prev-record
;; ?	 describe-mode
;; \C-q  forms-toggle-read-only
;; l	 forms-jump-record
;; n	 forms-next-record
;; p	 forms-prev-record
;; r	 forms-search-backward
;; s	 forms-search-forward
;; x	 forms-exit
;;
;; Of course, it is also possible to use the \C-c prefix to obtain the
;; same command keys as in edit mode.
;;
;; The following bindings are available, independent of the mode:
;;
;; [next]	  forms-next-record
;; [prior]	  forms-prev-record
;; [begin]	  forms-first-record
;; [end]	  forms-last-record
;; [S-TAB]	  forms-prev-field
;; [backtab]	  forms-prev-field
;;
;; For convenience, TAB is always bound to `forms-next-field', so you
;; don't need the C-c prefix for this command.
;;
;; As mentioned above (see `forms-forms-scroll' and `forms-forms-jump'),
;; the bindings of standard functions `scroll-up', `scroll-down',
;; `beginning-of-buffer' and `end-of-buffer' can be locally replaced with
;; forms mode functions next/prev record and first/last
;; record.
;;
;; `write-file-functions' is defined to save the actual data file
;; instead of the buffer data, `revert-file-hook' is defined to
;; revert a forms to original.

;;; Code:

(defgroup forms nil
  "Edit a file as a form to fill in."
  :group 'data)

;;; Global variables and constants:

(provide 'forms)			;;; official
(provide 'forms-mode)			;;; for compatibility

(defcustom forms-mode-hook nil
  "Hook run upon entering Forms mode."
  :group 'forms
  :type 'hook)

;;; Mandatory variables - must be set by evaluating the control file.

(defvar forms-file nil
  "Name of the file holding the data.")

(defvar forms-format-list nil
  "List of formatting specifications.")

(defvar forms-number-of-fields nil
  "Number of fields per record.")

;;; Optional variables with default values.

(defcustom forms-check-number-of-fields t
  "If non-nil, warn about records with wrong number of fields."
  :group 'forms
  :type 'boolean)

(defvar forms-field-sep "\t"
  "Field separator character (default TAB).")

(defvar forms-read-only nil
  "Non-nil means: visit the file in view (read-only) mode.
This is set automatically if the file permissions don't let you write it.")

(defvar forms-multi-line "\C-k" "\
If not nil: use this character to separate multi-line fields (default C-k).")

(defcustom forms-forms-scroll nil
  "Non-nil means replace scroll-up/down commands in Forms mode.
The replacement commands performs forms-next/prev-record."
  :group 'forms
  :type 'boolean)

(defcustom forms-forms-jump nil
  "Non-nil means redefine beginning/end-of-buffer in Forms mode.
The replacement commands performs forms-first/last-record."
  :group 'forms
  :type 'boolean)

(defvar forms-read-file-filter nil
  "The name of a function that is called after reading the data file.
This can be used to change the contents of the file to something more
suitable for forms processing.")

(defvar forms-write-file-filter nil
  "The name of a function that is called before writing the data file.
This can be used to undo the effects of `form-read-file-hook'.")

(defvar forms-new-record-filter nil
  "The name of a function that is called when a new record is created.")

(defvar forms-modified-record-filter nil
  "The name of a function that is called when a record has been modified.")

(defvar forms-fields nil
  "List with fields of the current forms.  First field has number 1.
This variable is for use by the filter routines only.
The contents may NOT be modified.")

(defcustom forms-use-text-properties t
  "Non-nil means: use text properties.
Defaults to t if this Emacs is capable of handling text properties."
  :group 'forms
  :type 'boolean)

(defcustom forms-insert-after nil
  "Non-nil means: inserts of new records go after current record.
Also, initial position is at last record."
  :group 'forms
  :type 'boolean)

(defcustom forms-ro-face 'default
  "The face (a symbol) that is used to display read-only text on the screen."
  :group 'forms
  :type 'face)

(defcustom forms-rw-face 'region
  "The face (a symbol) that is used to display read-write text on the screen."
  :group 'forms
  :type 'face)

;;; Internal variables.

(defvar forms--file-buffer nil
  "Buffer which holds the file data")

(defvar forms--total-records 0
  "Total number of records in the data file.")

(defvar forms--current-record 0
  "Number of the record currently on the screen.")

(defvar forms-mode-map nil
   "Keymap for form buffer.")
(defvar forms-mode-ro-map nil
   "Keymap for form buffer in view mode.")
(defvar forms-mode-edit-map nil
   "Keymap for form buffer in edit mode.")

(defvar forms--markers nil
  "Field markers in the screen.")

(defvar forms--dyntexts nil
  "Dynamic texts (resulting from function calls) on the screen.")

(defvar forms--the-record-list nil
   "List of strings of the current record, as parsed from the file.")

(defvar forms--search-regexp nil
  "Last regexp used by forms-search functions.")

(defvar forms--format nil
  "Formatting routine.")

(defvar forms--parser nil
  "Forms parser routine.")

(defvar forms--mode-setup nil
  "To keep track of forms-mode being set-up.")
(make-variable-buffer-local 'forms--mode-setup)

(defvar forms--dynamic-text nil
  "Array that holds dynamic texts to insert between fields.")

(defvar forms--elements nil
  "Array with the order in which the fields are displayed.")

(defvar forms--ro-face nil
  "Face used to represent read-only data on the screen.")

(defvar forms--rw-face nil
  "Face used to represent read-write data on the screen.")

;;;###autoload
(defun forms-mode (&optional primary)
  "Major mode to visit files in a field-structured manner using a form.

Commands:                        Equivalent keys in read-only mode:
 TAB            forms-next-field          TAB
 C-c TAB        forms-next-field
 C-c <          forms-first-record         <
 C-c >          forms-last-record          >
 C-c ?          describe-mode              ?
 C-c C-k        forms-delete-record
 C-c C-q        forms-toggle-read-only     q
 C-c C-o        forms-insert-record
 C-c C-l        forms-jump-record          l
 C-c C-n        forms-next-record          n
 C-c C-p        forms-prev-record          p
 C-c C-r        forms-search-reverse       r
 C-c C-s        forms-search-forward       s
 C-c C-x        forms-exit                 x
"
  (interactive)

  ;; This is not a simple major mode, as usual.  Therefore, forms-mode
  ;; takes an optional argument `primary' which is used for the
  ;; initial set-up.  Normal use would leave `primary' to nil.
  ;; A global buffer-local variable `forms--mode-setup' has the same
  ;; effect but makes it possible to auto-invoke forms-mode using
  ;; `find-file'.
  ;; Note: although it seems logical to have `make-local-variable'
  ;; executed where the variable is first needed, I have deliberately
  ;; placed all calls in this function.

  ;; Primary set-up: evaluate buffer and check if the mandatory
  ;; variables have been set.
  (if (or primary (not forms--mode-setup))
      (progn
	;;(message "forms: setting up...")
	(kill-all-local-variables)

	;; Make mandatory variables.
	(make-local-variable 'forms-file)
	(make-local-variable 'forms-number-of-fields)
	(make-local-variable 'forms-format-list)

	;; Make optional variables.
	(make-local-variable 'forms-field-sep)
        (make-local-variable 'forms-read-only)
        (make-local-variable 'forms-multi-line)
	(make-local-variable 'forms-forms-scroll)
	(make-local-variable 'forms-forms-jump)
	(make-local-variable 'forms-insert-after)
	(make-local-variable 'forms-use-text-properties)

	;; Filter functions.
	(make-local-variable 'forms-read-file-filter)
	(make-local-variable 'forms-write-file-filter)
	(make-local-variable 'forms-new-record-filter)
	(make-local-variable 'forms-modified-record-filter)

	;; Make sure no filters exist.
	(setq forms-read-file-filter nil)
	(setq forms-write-file-filter nil)
	(setq forms-new-record-filter nil)
	(setq forms-modified-record-filter nil)

	;; If running Emacs 19 under X, setup faces to show read-only and
	;; read-write fields.
	(if (fboundp 'make-face)
	    (progn
	      (make-local-variable 'forms-ro-face)
	      (make-local-variable 'forms-rw-face)))

	;; eval the buffer, should set variables
	;;(message "forms: processing control file...")
	;; If enable-local-eval is not set to t the user is asked first.
	(if (or (eq enable-local-eval t)
		(yes-or-no-p
		 (concat "Evaluate lisp code in buffer "
			 (buffer-name) " to display forms? ")))
	    (eval-buffer)
	  (error "`enable-local-eval' inhibits buffer evaluation"))

	;; Check if the mandatory variables make sense.
	(or forms-file
	    (error (concat "Forms control file error: "
			   "`forms-file' has not been set")))

	;; Check forms-field-sep first, since it can be needed to
	;; construct a default format list.
	(or (stringp forms-field-sep)
	    (error (concat "Forms control file error: "
			   "`forms-field-sep' is not a string")))

	(if forms-number-of-fields
	    (or (and (numberp forms-number-of-fields)
		     (> forms-number-of-fields 0))
		(error (concat "Forms control file error: "
			       "`forms-number-of-fields' must be a number > 0")))
	  (or (null forms-format-list)
	      (error (concat "Forms control file error: "
			     "`forms-number-of-fields' has not been set"))))

	(or forms-format-list
	    (forms--intuit-from-file))

	(if forms-multi-line
	    (if (and (stringp forms-multi-line)
		     (eq (length forms-multi-line) 1))
		(if (string= forms-multi-line forms-field-sep)
		    (error (concat "Forms control file error: "
				   "`forms-multi-line' is equal to `forms-field-sep'")))
	      (error (concat "Forms control file error: "
			     "`forms-multi-line' must be nil or a one-character string"))))
	(or (fboundp 'set-text-properties)
	    (setq forms-use-text-properties nil))

	;; Validate and process forms-format-list.
	;;(message "forms: pre-processing format list...")
	(make-local-variable 'forms--elements)
	(forms--process-format-list)

	;; Build the formatter and parser.
	;;(message "forms: building formatter...")
	(make-local-variable 'forms--format)
	(make-local-variable 'forms--markers)
	(make-local-variable 'forms--dyntexts)
	;;(message "forms: building parser...")
	(forms--make-format)
	(make-local-variable 'forms--parser)
	(forms--make-parser)
	;;(message "forms: building parser... done.")

	;; Check if record filters are defined.
	(if (and forms-new-record-filter
		 (not (fboundp forms-new-record-filter)))
	    (error (concat "Forms control file error: "
			   "`forms-new-record-filter' is not a function")))

	(if (and forms-modified-record-filter
		 (not (fboundp forms-modified-record-filter)))
	    (error (concat "Forms control file error: "
			   "`forms-modified-record-filter' is not a function")))

	;; The filters access the contents of the forms using `forms-fields'.
	(make-local-variable 'forms-fields)

	;; Dynamic text support.
	(make-local-variable 'forms--dynamic-text)

	;; Prevent accidental overwrite of the control file and auto-save.
	(set-visited-file-name nil)

	;; Prepare this buffer for further processing.
	(setq buffer-read-only nil)
	(erase-buffer)

	;;(message "forms: setting up... done.")
	))

  ;; initialization done
  (setq forms--mode-setup t)

  ;; Copy desired faces to the actual variables used by the forms formatter.
  (if (fboundp 'make-face)
      (progn
	(make-local-variable 'forms--ro-face)
	(make-local-variable 'forms--rw-face)
	(if forms-read-only
	    (progn
	      (setq forms--ro-face forms-ro-face)
	      (setq forms--rw-face forms-ro-face))
	  (setq forms--ro-face forms-ro-face)
	  (setq forms--rw-face forms-rw-face))))

  ;; Make more local variables.
  (make-local-variable 'forms--file-buffer)
  (make-local-variable 'forms--total-records)
  (make-local-variable 'forms--current-record)
  (make-local-variable 'forms--the-record-list)
  (make-local-variable 'forms--search-regexp)

  ; The keymaps are global, so multiple forms mode buffers can share them.
  ;(make-local-variable 'forms-mode-map)
  ;(make-local-variable 'forms-mode-ro-map)
  ;(make-local-variable 'forms-mode-edit-map)
  (if forms-mode-map			; already defined
      nil
    ;;(message "forms: building keymap...")
    (forms--mode-commands)
    ;;(message "forms: building keymap... done.")
    )

  ;; set the major mode indicator
  (setq major-mode 'forms-mode)
  (setq mode-name "Forms")

  ;; find the data file
  (setq forms--file-buffer (find-file-noselect forms-file))

  ;; Pre-transform.
  (let ((read-file-filter forms-read-file-filter)
	(write-file-filter forms-write-file-filter))
    (if read-file-filter
	(with-current-buffer forms--file-buffer
	  (let ((inhibit-read-only t)
		(file-modified (buffer-modified-p)))
	    (run-hooks 'read-file-filter)
	    (if (not file-modified) (set-buffer-modified-p nil)))
	  (if write-file-filter
	      (add-hook 'write-file-functions write-file-filter nil t)))
      (if write-file-filter
	  (with-current-buffer forms--file-buffer
	    (add-hook 'write-file-functions write-file-filter nil t)))))

  ;; count the number of records, and set see if it may be modified
  (let (ro)
    (setq forms--total-records
	  (with-current-buffer forms--file-buffer
	    (prog1
		(progn
		  ;;(message "forms: counting records...")
		  (bury-buffer (current-buffer))
		  (setq ro buffer-read-only)
		  (count-lines (point-min) (point-max)))
	      ;;(message "forms: counting records... done.")
	      )))
    (if ro
	(setq forms-read-only t)))

  ;;(message "forms: proceeding setup...")

  ;; Since we aren't really implementing a minor mode, we hack the modeline
  ;; directly to get the text " View " into forms-read-only form buffers.  For
  ;; that reason, this variable must be buffer only.
  (make-local-variable 'minor-mode-alist)
  (setq minor-mode-alist (list (list 'forms-read-only " View")))

  ;;(message "forms: proceeding setup (keymaps)...")
  (forms--set-keymaps)
  ;;(message "forms: proceeding setup (commands)...")
  (forms--change-commands)

  ;;(message "forms: proceeding setup (buffer)...")
  (set-buffer-modified-p nil)

  (if (= forms--total-records 0)
      ;;(message "forms: proceeding setup (new file)...")
      (progn
	(insert
	 "GNU Emacs Forms Mode\n\n"
	 (if (file-exists-p forms-file)
	     (concat "No records available in file `" forms-file "'\n\n")
	   (format "Creating new file `%s'\nwith %d field%s per record\n\n"
		   forms-file forms-number-of-fields
		   (if (= 1 forms-number-of-fields) "" "s")))
	 "Use " (substitute-command-keys "\\[forms-insert-record]")
	 " to create new records.\n")
	(setq forms--current-record 1)
	(setq buffer-read-only t)
	(set-buffer-modified-p nil))

    ;; setup the first (or current) record to show
    (if (< forms--current-record 1)
	(setq forms--current-record 1))
    (forms-jump-record forms--current-record)

    (if forms-insert-after
	(forms-last-record)
      (forms-first-record))
    )

  ;; user customizing
  ;;(message "forms: proceeding setup (user hooks)...")
  (run-mode-hooks 'forms-mode-hook 'forms-mode-hooks)
  ;;(message "forms: setting up... done.")

  ;; be helpful
  (forms--help)
)

(defun forms--process-format-list ()
  ;; Validate `forms-format-list' and set some global variables.
  ;; Symbols in the list are evaluated, and consecutive strings are
  ;; concatenated.
  ;; Array `forms--elements' is constructed that contains the order
  ;; of the fields on the display. This array is used by
  ;; `forms--parser-using-text-properties' to extract the fields data
  ;; from the form on the screen.
  ;; Upon completion, `forms-format-list' is guaranteed correct, so
  ;; `forms--make-format' and `forms--make-parser' do not need to perform
  ;; any checks.

  ;; Verify that `forms-format-list' is not nil.
  (or forms-format-list
      (error (concat "Forms control file error: "
		     "`forms-format-list' has not been set")))
  ;; It must be a list.
  (or (listp forms-format-list)
      (error (concat "Forms control file error: "
		     "`forms-format-list' is not a list")))

  ;; Assume every field is painted once.
  ;; `forms--elements' will grow if needed.
  (setq forms--elements (make-vector forms-number-of-fields nil))

  (let ((the-list forms-format-list)	; the list of format elements
	(prev-item nil)
	(field-num 0))			; highest field number

    (setq forms-format-list nil)	; gonna rebuild

    (while the-list

      (let ((el (car-safe the-list))
	    (rem (cdr-safe the-list)))

	;; If it is a symbol, eval it first.
	(if (and (symbolp el)
		 (boundp el))
	    (setq el (eval el)))

	(cond

	 ;; Try string ...
	 ((stringp el)
	  (if (stringp prev-item)	; try to concatenate strings
	      (setq prev-item (concat prev-item el))
	    (if prev-item
		(setq forms-format-list
		      (append forms-format-list (list prev-item) nil)))
	    (setq prev-item el)))

	 ;; Try numeric ...
	 ((numberp el)

	  ;; Validate range.
	  (if (or (<= el 0)
		  (> el forms-number-of-fields))
	      (error (concat "Forms format error: "
			     "field number %d out of range 1..%d")
		     el forms-number-of-fields))

	  ;; Store forms order.
	  (if (>= field-num (length forms--elements))
	      (setq forms--elements (vconcat forms--elements (1- el)))
	    (aset forms--elements field-num (1- el)))
	  (setq field-num (1+ field-num))

	  (if prev-item
	      (setq forms-format-list
		    (append forms-format-list (list prev-item) nil)))
	  (setq prev-item el))

	 ;; Try function ...
	 ((listp el)

	  ;; Validate.
	  (or (fboundp (car-safe el))
	      (error (concat "Forms format error: "
			     "%S is not a function")
		     (car-safe el)))

	  ;; Shift.
	  (if prev-item
	      (setq forms-format-list
		    (append forms-format-list (list prev-item) nil)))
	  (setq prev-item el))

	 ;; else
	 (t
	  (error (concat "Forms format error: "
			 "invalid element %S")
		 el)))

	;; Advance to next element of the list.
	(setq the-list rem)))

    ;; Append last item.
    (if prev-item
	(progn
	  (setq forms-format-list
		(append forms-format-list (list prev-item) nil))
	  ;; Append a newline if the last item is a field.
	  ;; This prevents parsing problems.
	  ;; Also it makes it possible to insert an empty last field.
	  (if (numberp prev-item)
	      (setq forms-format-list
		    (append forms-format-list (list "\n") nil))))))

  (forms--debug 'forms-format-list
		'forms--elements))

;; Special treatment for read-only segments.
;;
;; If text is inserted between two read-only segments, there seems to
;; be no way to give the newly inserted text the RW face.
;; To solve this, read-only segments get the `insert-in-front-hooks'
;; property set with a function that temporarily switches the
;; properties of the first character of the segment to the RW face, so
;; the new text gets the right face. The `post-command-hook' is
;; used to restore the original properties.

(defvar forms--iif-start nil
  "Record start of modification command.")
(defvar forms--iif-properties nil
  "Original properties of the character being overridden.")

(defun forms--iif-hook (_begin _end)
  "`insert-in-front-hooks' function for read-only segments."

  ;; Note start location.  By making it a marker that points one
  ;; character beyond the actual location, it is guaranteed to move
  ;; correctly if text is inserted.
  (or forms--iif-start
      (setq forms--iif-start (copy-marker (1+ (point)))))

  ;; Check if there is special treatment required.
  (if (or (<= forms--iif-start 2)
	  (get-text-property (- forms--iif-start 2)
			     'read-only))
      (progn
	;; Fetch current properties.
	(setq forms--iif-properties
	      (text-properties-at (1- forms--iif-start)))

	;; Replace them.
	(let ((inhibit-read-only t))
	  (set-text-properties
	   (1- forms--iif-start) forms--iif-start
	   (list 'face forms--rw-face 'front-sticky '(face))))

	;; Enable `post-command-hook' to restore the properties.
	(setq post-command-hook
	      (append (list 'forms--iif-post-command-hook) post-command-hook)))

    ;; No action needed.  Clear marker.
    (setq forms--iif-start nil)))

(defun forms--iif-post-command-hook ()
  "`post-command-hook' function for read-only segments."

  ;; Disable `post-command-hook'.
  (setq post-command-hook
	(delq 'forms--iif-hook-post-command-hook post-command-hook))

  ;; Restore properties.
  (if forms--iif-start
      (let ((inhibit-read-only t))
	(set-text-properties
	 (1- forms--iif-start) forms--iif-start
	 forms--iif-properties)))

  ;; Cleanup.
  (setq forms--iif-start nil))

(defvar forms--marker)
(defvar forms--dyntext)

(defun forms--make-format ()
  "Generate `forms--format' using the information in `forms-format-list'."

  ;; The real work is done using a mapcar of `forms--make-format-elt' on
  ;; `forms-format-list'.
  ;; This function sets up the necessary environment, and decides
  ;; which function to mapcar.

  (let ((forms--marker 0)
	(forms--dyntext 0))
    (setq
     forms--format
     (if forms-use-text-properties
	 `(lambda (arg)
	    (let ((inhibit-read-only t))
	      ,@(apply 'append
		       (mapcar 'forms--make-format-elt-using-text-properties
			       forms-format-list))
	      ;; Prevent insertion before the first text.
	      ,@(if (numberp (car forms-format-list))
		    nil
		  '((add-text-properties (point-min) (1+ (point-min))
					 '(front-sticky (read-only intangible)))))
	      ;; Prevent insertion after the last text.
	      (remove-text-properties (1- (point)) (point)
				      '(rear-nonsticky)))
	    (setq forms--iif-start nil))
       `(lambda (arg)
	  ,@(apply 'append
		   (mapcar 'forms--make-format-elt forms-format-list)))))

    ;; We have tallied the number of markers and dynamic texts,
    ;; so we can allocate the arrays now.
    (setq forms--markers (make-vector forms--marker nil))
    (setq forms--dyntexts (make-vector forms--dyntext nil)))
  (forms--debug 'forms--format))

(defun forms--make-format-elt-using-text-properties (el)
  "Helper routine to generate format function."

  ;; The format routine `forms--format' will look like
  ;;
  ;; ;; preamble
  ;; (lambda (arg)
  ;;   (let ((inhibit-read-only t))
  ;;
  ;;     ;; A string, e.g. "text: ".
  ;;     (set-text-properties
  ;;      (point)
  ;;      (progn (insert "text: ") (point))
  ;;      (list 'face forms--ro-face
  ;;		'read-only 1
  ;;		'insert-in-front-hooks 'forms--iif-hook
  ;;		'rear-nonsticky '(read-only face insert-in-front-hooks)))
  ;;
  ;;     ;; A field, e.g. 6.
  ;;     (let ((here (point)))
  ;;       (aset forms--markers 0 (point-marker))
  ;;       (insert (elt arg 5))
  ;;       (or (= (point) here)
  ;; 	  (set-text-properties
  ;; 	   here (point)
  ;; 	   (list 'face forms--rw-face
  ;;		 'front-sticky '(face))))
  ;;
  ;;     ;; Another string, e.g. "\nmore text: ".
  ;;     (set-text-properties
  ;;      (point)
  ;;      (progn (insert "\nmore text: ") (point))
  ;;      (list 'face forms--ro-face
  ;;		'read-only 2
  ;;		'insert-in-front-hooks 'forms--iif-hook
  ;;		'rear-nonsticky '(read-only face insert-in-front-hooks)))
  ;;
  ;;     ;; A function, e.g. (tocol 40).
  ;;     (set-text-properties
  ;;      (point)
  ;;      (progn
  ;;        (insert (aset forms--dyntexts 0 (tocol 40)))
  ;;        (point))
  ;;      (list 'face forms--ro-face
  ;;		'read-only 2
  ;;		'insert-in-front-hooks 'forms--iif-hook
  ;;		'rear-nonsticky '(read-only face insert-in-front-hooks)))
  ;;
  ;;	 ;; Prevent insertion before the first text.
  ;;	 (add-text-properties (point-min) (1+ (point-min))
  ;;			      '(front-sticky (read-only))))))
  ;;	 ;; Prevent insertion after the last text.
  ;;	 (remove-text-properties (1- (point)) (point)
  ;;	 			 '(rear-nonsticky)))
  ;;
  ;;     ;; wrap up
  ;;     (setq forms--iif-start nil)
  ;;     ))

  (cond
   ((stringp el)

    `((set-text-properties
       (point)				; start at point
       (progn				; until after insertion
	 (insert ,el)
	 (point))
       (list 'face forms--ro-face	; read-only appearance
	     'read-only ,@(list (1+ forms--marker))
	     'intangible ,@(list (1+ forms--marker))
	     'insert-in-front-hooks '(forms--iif-hook)
	     'rear-nonsticky '(face read-only insert-in-front-hooks
				    intangible)))))

   ((numberp el)
    `((let ((here (point)))
	(aset forms--markers
	      ,(prog1 forms--marker
		 (setq forms--marker (1+ forms--marker)))
	      (point-marker))
	(insert (elt arg ,(1- el)))
	(or (= (point) here)
	    (set-text-properties
	     here (point)
	     (list 'face forms--rw-face
		   'front-sticky '(face)))))))

   ((listp el)
    `((set-text-properties
       (point)
       (progn
	 (insert (aset forms--dyntexts
		       ,(prog1 forms--dyntext
			  (setq forms--dyntext (1+ forms--dyntext)))
		       ,el))
	 (point))
       (list 'face forms--ro-face
	     'read-only ,@(list (1+ forms--marker))
	     'intangible ,@(list (1+ forms--marker))
	     'insert-in-front-hooks '(forms--iif-hook)
	     'rear-nonsticky '(read-only face insert-in-front-hooks
					 intangible)))))

   ;; end of cond
   ))

(defun forms--make-format-elt (el)
  "Helper routine to generate format function."

  ;; If we're not using text properties, the format routine
  ;; `forms--format' will look like
  ;;
  ;; (lambda (arg)
  ;;   ;; a string, e.g. "text: "
  ;;   (insert "text: ")
  ;;   ;; a field, e.g. 6
  ;;   (aset forms--markers 0 (point-marker))
  ;;   (insert (elt arg 5))
  ;;   ;; another string, e.g. "\nmore text: "
  ;;   (insert "\nmore text: ")
  ;;   ;; a function, e.g. (tocol 40)
  ;;   (insert (aset forms--dyntexts 0 (tocol 40)))
  ;;   ... )

  (cond
   ((stringp el)
    `((insert ,el)))
   ((numberp el)
    (prog1
	`((aset forms--markers ,forms--marker (point-marker))
	  (insert (elt arg ,(1- el))))
      (setq forms--marker (1+ forms--marker))))
   ((listp el)
    (prog1
	`((insert (aset forms--dyntexts ,forms--dyntext ,el)))
      (setq forms--dyntext (1+ forms--dyntext))))))

(defvar forms--field)
(defvar forms--recordv)
(defvar forms--seen-text)

(defun forms--make-parser ()
  "Generate `forms--parser' from the information in `forms-format-list'."

  ;; If we can use text properties, we simply set it to
  ;; `forms--parser-using-text-properties'.
  ;; Otherwise, the function is constructed using a mapcar of
  ;; `forms--make-parser-elt on `forms-format-list'.

  (setq
   forms--parser
   (if forms-use-text-properties
       (function forms--parser-using-text-properties)
     (let ((forms--field nil)
	   (forms--seen-text nil)
	   (forms--dyntext 0))

       ;; Note: we add a nil element to the list passed to `mapcar',
       ;; see `forms--make-parser-elt' for details.
       `(lambda nil
	  (let (here)
	    (goto-char (point-min))
	    ,@(apply 'append
		     (mapcar
		      'forms--make-parser-elt
		      (append forms-format-list (list nil)))))))))

  (forms--debug 'forms--parser))

(defun forms--parser-using-text-properties ()
  "Extract field info from forms when using text properties."

  ;; Using text properties, we can simply jump to the markers, and
  ;; extract the information up to the following read-only segment.

  (let ((i 0)
	here there)
    (while (< i (length forms--markers))
      (goto-char (setq here (aref forms--markers i)))
      (if (get-text-property here 'read-only)
	  (aset forms--recordv (aref forms--elements i) nil)
	(if (setq there
		  (next-single-property-change here 'read-only))
	    (aset forms--recordv (aref forms--elements i)
		  (buffer-substring-no-properties here there))
	  (aset forms--recordv (aref forms--elements i)
		(buffer-substring-no-properties here (point-max)))))
      (setq i (1+ i)))))

(defun forms--make-parser-elt (el)
  "Helper routine to generate forms parser function."

  ;; The parse routine will look like:
  ;;
  ;; (lambda nil
  ;;   (let (here)
  ;;     (goto-char (point-min))
  ;;
  ;;	 ;;  "text: "
  ;;     (if (not (looking-at "text: "))
  ;; 	    (error "Parse error: cannot find \"text: \""))
  ;;     (forward-char 6)	; past "text: "
  ;;
  ;;     ;;  6
  ;;	 ;;  "\nmore text: "
  ;;     (setq here (point))
  ;;     (if (not (search-forward "\nmore text: " nil t nil))
  ;; 	    (error "Parse error: cannot find \"\\nmore text: \""))
  ;;     (aset forms--recordv 5 (buffer-substring-no-properties here (- (point) 12)))
  ;;
  ;;	 ;;  (tocol 40)
  ;;	(let ((forms--dyntext (car-safe forms--dynamic-text)))
  ;;	  (if (not (looking-at (regexp-quote forms--dyntext)))
  ;;	      (error "Parse error: not looking at \"%s\"" forms--dyntext))
  ;;	  (forward-char (length forms--dyntext))
  ;;	  (setq forms--dynamic-text (cdr-safe forms--dynamic-text)))
  ;;     ...
  ;;     ;; final flush (due to terminator sentinel, see below)
  ;;	(aset forms--recordv 7 (buffer-substring-no-properties (point) (point-max)))

  (cond
   ((stringp el)
    (prog1
	(if forms--field
	    `((setq here (point))
	      (if (not (search-forward ,el nil t nil))
		  (error "Parse error: cannot find `%s'" ,el))
	      (aset forms--recordv ,(1- forms--field)
		    (buffer-substring-no-properties here
						    (- (point) ,(length el)))))
	  `((if (not (looking-at ,(regexp-quote el)))
		(error "Parse error: not looking at `%s'" ,el))
	    (forward-char ,(length el))))
      (setq forms--seen-text t)
      (setq forms--field nil)))
   ((numberp el)
    (if forms--field
	(error "Cannot parse adjacent fields %d and %d"
	       forms--field el)
      (setq forms--field el)
      nil))
   ((null el)
    (if forms--field
	`((aset forms--recordv ,(1- forms--field)
		(buffer-substring-no-properties (point) (point-max))))))
   ((listp el)
    (prog1
	(if forms--field
	    `((let ((here (point))
		    (forms--dyntext (aref forms--dyntexts ,forms--dyntext)))
		(if (not (search-forward forms--dyntext nil t nil))
		    (error "Parse error: cannot find `%s'" forms--dyntext))
		(aset forms--recordv ,(1- forms--field)
		      (buffer-substring-no-properties here
						      (- (point) (length forms--dyntext))))))
	  `((let ((forms--dyntext (aref forms--dyntexts ,forms--dyntext)))
	      (if (not (looking-at (regexp-quote forms--dyntext)))
		  (error "Parse error: not looking at `%s'" forms--dyntext))
	      (forward-char (length forms--dyntext)))))
      (setq forms--dyntext (1+ forms--dyntext))
      (setq forms--seen-text t)
      (setq forms--field nil)))
   ))

(defvar read-file-filter) ; bound in forms--intuit-from-file

(defun forms--intuit-from-file ()
  "Get number of fields and a default form using the data file."

  ;; If `forms-number-of-fields' is not set, get it from the data file.
  (if (null forms-number-of-fields)

      ;; Need a file to do this.
      (if (not (file-exists-p forms-file))
	  (error "Need existing file or explicit `forms-number-of-fields'")

	;; Visit the file and extract the first record.
	(setq forms--file-buffer (find-file-noselect forms-file))
	(let ((read-file-filter forms-read-file-filter)
	      (the-record))
	  (setq the-record
		(with-current-buffer forms--file-buffer
		  (let ((inhibit-read-only t))
		    (run-hooks 'read-file-filter))
		  (goto-char (point-min))
		  (forms--get-record)))

	  ;; This may be overkill, but try to avoid interference with
	  ;; the normal processing.
	  (kill-buffer forms--file-buffer)

	  ;; Count the number of fields in `the-record'.
	  (let ((start-pos 0)
		found-pos
		(field-sep-length (length forms-field-sep)))
	    (setq forms-number-of-fields 1)
	    (while (setq found-pos
			 (string-match forms-field-sep the-record start-pos))
	      (progn
		(setq forms-number-of-fields (1+ forms-number-of-fields))
		(setq start-pos (+ field-sep-length found-pos))))))))

  ;; Construct default format list.
  (setq forms-format-list (list "Forms file \"" forms-file "\".\n\n"))
  (let ((i 0))
    (while (<= (setq i (1+ i)) forms-number-of-fields)
      (setq forms-format-list
	    (append forms-format-list
		    (list (format "%4d: " i) i "\n"))))))

(defun forms--set-keymaps ()
  "Set the keymaps used in this mode."

  (use-local-map (if forms-read-only
		     forms-mode-ro-map
		   forms-mode-edit-map)))

(defun forms--mode-commands ()
  "Fill the Forms mode keymaps."

  ;; `forms-mode-map' is always accessible via \C-c prefix.
  (setq forms-mode-map (make-keymap))
  (define-key forms-mode-map "\t" 'forms-next-field)
  (define-key forms-mode-map "\C-k" 'forms-delete-record)
  (define-key forms-mode-map "\C-q" 'forms-toggle-read-only)
  (define-key forms-mode-map "\C-o" 'forms-insert-record)
  (define-key forms-mode-map "\C-l" 'forms-jump-record)
  (define-key forms-mode-map "\C-n" 'forms-next-record)
  (define-key forms-mode-map "\C-p" 'forms-prev-record)
  (define-key forms-mode-map "\C-r" 'forms-search-backward)
  (define-key forms-mode-map "\C-s" 'forms-search-forward)
  (define-key forms-mode-map "\C-x" 'forms-exit)
  (define-key forms-mode-map "<" 'forms-first-record)
  (define-key forms-mode-map ">" 'forms-last-record)
  (define-key forms-mode-map "\C-?" 'forms-prev-record)

  ;; `forms-mode-ro-map' replaces the local map when in read-only mode.
  (setq forms-mode-ro-map (make-keymap))
  (suppress-keymap forms-mode-ro-map)
  (define-key forms-mode-ro-map "\C-c" forms-mode-map)
  (define-key forms-mode-ro-map "q" 'forms-toggle-read-only)
  (define-key forms-mode-ro-map "l" 'forms-jump-record)
  (define-key forms-mode-ro-map "n" 'forms-next-record)
  (define-key forms-mode-ro-map "p" 'forms-prev-record)
  (define-key forms-mode-ro-map "r" 'forms-search-backward)
  (define-key forms-mode-ro-map "s" 'forms-search-forward)
  (define-key forms-mode-ro-map "x" 'forms-exit)
  (define-key forms-mode-ro-map "<" 'forms-first-record)
  (define-key forms-mode-ro-map ">" 'forms-last-record)
  (define-key forms-mode-ro-map "?" 'describe-mode)
  (define-key forms-mode-ro-map " " 'forms-next-record)
  (forms--mode-commands1 forms-mode-ro-map)
  (forms--mode-menu-ro forms-mode-ro-map)

  ;; This is the normal, local map.
  (setq forms-mode-edit-map (make-keymap))
  (define-key forms-mode-edit-map "\C-c" forms-mode-map)
  (forms--mode-commands1 forms-mode-edit-map)
  (forms--mode-menu-edit forms-mode-edit-map)
  )

(defun forms--mode-menu-ro (map)
;;; Menu initialization
;  (define-key map [menu-bar] (make-sparse-keymap))
  (define-key map [menu-bar forms]
    (cons "Forms" (make-sparse-keymap "Forms")))
  (define-key map [menu-bar forms menu-forms-exit]
    '("Exit Forms Mode" . forms-exit))
  (define-key map [menu-bar forms menu-forms-sep1]
    '("----"))
  (define-key map [menu-bar forms menu-forms-save]
    '("Save Data" . forms-save-buffer))
  (define-key map [menu-bar forms menu-forms-print]
    '("Print Data" . forms-print))
  (define-key map [menu-bar forms menu-forms-describe]
    '("Describe Mode" . describe-mode))
  (define-key map [menu-bar forms menu-forms-toggle-ro]
    '("Toggle View/Edit" . forms-toggle-read-only))
  (define-key map [menu-bar forms menu-forms-jump-record]
    '("Jump" . forms-jump-record))
  (define-key map [menu-bar forms menu-forms-search-backward]
    '("Search Backward" . forms-search-backward))
  (define-key map [menu-bar forms menu-forms-search-forward]
    '("Search Forward" . forms-search-forward))
  (define-key map [menu-bar forms menu-forms-delete-record]
    '("Delete" . forms-delete-record))
  (define-key map [menu-bar forms menu-forms-insert-record]
    '("Insert" . forms-insert-record))
  (define-key map [menu-bar forms menu-forms-sep2]
    '("----"))
  (define-key map [menu-bar forms menu-forms-last-record]
    '("Last Record" . forms-last-record))
  (define-key map [menu-bar forms menu-forms-first-record]
    '("First Record" . forms-first-record))
  (define-key map [menu-bar forms menu-forms-prev-record]
    '("Previous Record" . forms-prev-record))
  (define-key map [menu-bar forms menu-forms-next-record]
    '("Next Record" . forms-next-record))
  (define-key map [menu-bar forms menu-forms-sep3]
    '("----"))
  (define-key map [menu-bar forms menu-forms-prev-field]
    '("Previous Field" . forms-prev-field))
  (define-key map [menu-bar forms menu-forms-next-field]
    '("Next Field" . forms-next-field))
  (put 'forms-insert-record 'menu-enable '(not forms-read-only))
  (put 'forms-delete-record 'menu-enable '(not forms-read-only))
)
(defun forms--mode-menu-edit (map)
;;; Menu initialization
;  (define-key map [menu-bar] (make-sparse-keymap))
  (define-key map [menu-bar forms]
    (cons "Forms" (make-sparse-keymap "Forms")))
  (define-key map [menu-bar forms menu-forms-edit--exit]
    '("Exit" . forms-exit))
  (define-key map [menu-bar forms menu-forms-edit-sep1]
    '("----"))
  (define-key map [menu-bar forms menu-forms-edit-save]
    '("Save Data" . forms-save-buffer))
  (define-key map [menu-bar forms menu-forms-edit-print]
    '("Print Data" . forms-print))
  (define-key map [menu-bar forms menu-forms-edit-describe]
    '("Describe Mode" . describe-mode))
  (define-key map [menu-bar forms menu-forms-edit-toggle-ro]
    '("Toggle View/Edit" . forms-toggle-read-only))
  (define-key map [menu-bar forms menu-forms-edit-jump-record]
    '("Jump" . forms-jump-record))
  (define-key map [menu-bar forms menu-forms-edit-search-backward]
    '("Search Backward" . forms-search-backward))
  (define-key map [menu-bar forms menu-forms-edit-search-forward]
    '("Search Forward" . forms-search-forward))
  (define-key map [menu-bar forms menu-forms-edit-delete-record]
    '("Delete" . forms-delete-record))
  (define-key map [menu-bar forms menu-forms-edit-insert-record]
    '("Insert" . forms-insert-record))
  (define-key map [menu-bar forms menu-forms-edit-sep2]
    '("----"))
  (define-key map [menu-bar forms menu-forms-edit-last-record]
    '("Last Record" . forms-last-record))
  (define-key map [menu-bar forms menu-forms-edit-first-record]
    '("First Record" . forms-first-record))
  (define-key map [menu-bar forms menu-forms-edit-prev-record]
    '("Previous Record" . forms-prev-record))
  (define-key map [menu-bar forms menu-forms-edit-next-record]
    '("Next Record" . forms-next-record))
  (define-key map [menu-bar forms menu-forms-edit-sep3]
    '("----"))
  (define-key map [menu-bar forms menu-forms-edit-prev-field]
    '("Previous Field" . forms-prev-field))
  (define-key map [menu-bar forms menu-forms-edit-next-field]
    '("Next Field" . forms-next-field))
  (put 'forms-insert-record 'menu-enable '(not forms-read-only))
  (put 'forms-delete-record 'menu-enable '(not forms-read-only))
)

(defun forms--mode-commands1 (map)
  "Helper routine to define keys."
  (define-key map "\t" 'forms-next-field)
  (define-key map [S-tab] 'forms-prev-field)
  (define-key map [next] 'forms-next-record)
  (define-key map [prior] 'forms-prev-record)
  (define-key map [begin] 'forms-first-record)
  (define-key map [last] 'forms-last-record)
  (define-key map [backtab] 'forms-prev-field)
  )

;;; Changed functions

(defun forms--change-commands ()
  "Localize some commands for Forms mode."

  ;; scroll-down -> forms-prev-record
  ;; scroll-up -> forms-next-record
  (if forms-forms-scroll
      (progn
	(local-set-key [remap scroll-up] 'forms-next-record)
	(local-set-key [remap scroll-down] 'forms-prev-record)
	(local-set-key [remap scroll-up-command] 'forms-next-record)
	(local-set-key [remap scroll-down-command] 'forms-prev-record)))
  ;;
  ;; beginning-of-buffer -> forms-first-record
  ;; end-of-buffer -> forms-end-record
  (if forms-forms-jump
      (progn
	(local-set-key [remap beginning-of-buffer] 'forms-first-record)
	(local-set-key [remap end-of-buffer] 'forms-last-record)))
  ;;
  ;; Save buffer
  (local-set-key "\C-x\C-s" 'forms-save-buffer)
  ;;
  ;; We have our own revert function - use it.
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'forms--revert-buffer)

  t)

(defun forms--help ()
  "Initial help for Forms mode."
  (message "%s" (substitute-command-keys (concat
  "\\[forms-next-record]:next"
  "   \\[forms-prev-record]:prev"
  "   \\[forms-first-record]:first"
  "   \\[forms-last-record]:last"
  "   \\[describe-mode]:help"))))

(defun forms--trans (subj arg rep)
  "Translate in SUBJ all chars ARG into char REP.  ARG and REP should
 be single-char strings."
  (let ((i 0)
	(re (regexp-quote arg))
	(k (string-to-char rep)))
    (while (setq i (string-match re subj i))
      (aset subj i k)
      (setq i (1+ i)))))

(defun forms--exit (&optional save)
  "Internal exit from forms mode function."

  (let ((buf (buffer-name forms--file-buffer)))
    (forms--checkmod)
    (if (and save
	     (buffer-modified-p forms--file-buffer))
	(forms-save-buffer))
    (with-current-buffer forms--file-buffer
      (delete-auto-save-file-if-necessary)
      (kill-buffer (current-buffer)))
    (if (get-buffer buf)	; not killed???
	(if save
	    (error "Problem saving buffer %s" (buffer-name buf)))
      (delete-auto-save-file-if-necessary)
      (kill-buffer (current-buffer)))))

(defun forms--get-record ()
  "Fetch the current record from the file buffer."

  ;; This function is executed in the context of the `forms--file-buffer'.

  (or (bolp)
      (beginning-of-line nil))
  (let ((here (point)))
    (prog2
     (end-of-line)
     (buffer-substring-no-properties here (point))
     (goto-char here))))

(defun forms--show-record (the-record)
  "Format THE-RECORD and display it in the current buffer."

  ;; Split the-record.
  (let (the-result
	(start-pos 0)
	found-pos
	(field-sep-length (length forms-field-sep)))
    (if forms-multi-line
	(forms--trans the-record forms-multi-line "\n"))
    ;; Add an extra separator (makes splitting easy).
    (setq the-record (concat the-record forms-field-sep))
    (while (setq found-pos (string-match forms-field-sep the-record start-pos))
      (let ((ent (substring the-record start-pos found-pos)))
	(setq the-result
	      (append the-result (list ent)))
	(setq start-pos (+ field-sep-length found-pos))))
    (setq forms--the-record-list the-result))

  (setq buffer-read-only nil)
  (if forms-use-text-properties
      (let ((inhibit-read-only t))
	(set-text-properties (point-min) (point-max) nil)))
  (erase-buffer)

  ;; Verify the number of fields, extend forms--the-record-list if needed.
  (if (= (length forms--the-record-list) forms-number-of-fields)
      nil
    (if (null forms-check-number-of-fields)
	nil
      (message "Warning: this record has %d fields instead of %d"
	       (length forms--the-record-list) forms-number-of-fields))
    (if (< (length forms--the-record-list) forms-number-of-fields)
	(setq forms--the-record-list
	      (append forms--the-record-list
		      (make-list
		       (- forms-number-of-fields
			  (length forms--the-record-list))
		       "")))))

  ;; Call the formatter function.
  (setq forms-fields (append (list nil) forms--the-record-list nil))
  (funcall forms--format forms--the-record-list)

  ;; Prepare.
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only forms-read-only)
  (setq mode-line-process
	(concat " " (int-to-string forms--current-record)
		"/" (int-to-string forms--total-records))))

(defun forms--parse-form ()
  "Parse contents of form into list of strings."
  ;; The contents of the form are parsed, and a new list of strings
  ;; is constructed.
  ;; A vector with the strings from the original record is
  ;; constructed, which is updated with the new contents.  Therefore
  ;; fields which were not in the form are not modified.
  ;; Finally, the vector is transformed into a list for further processing.

  (let (forms--recordv)

    ;; Build the vector.
    (setq forms--recordv (vconcat forms--the-record-list))

    ;; Parse the form and update the vector.
    (let ((forms--dynamic-text forms--dynamic-text))
      (funcall forms--parser))

    (if forms-modified-record-filter
	;; As a service to the user, we add a zeroth element so she
	;; can use the same indices as in the forms definition.
	(let ((the-fields (vconcat [nil] forms--recordv)))
	  (setq the-fields (funcall forms-modified-record-filter the-fields))
	  (cdr (append the-fields nil)))

      ;; Transform to a list and return.
      (append forms--recordv nil))))

(defun forms--update ()
  "Update current record with contents of form.
As a side effect: sets `forms--the-record-list'."

  (if forms-read-only
      (error "Buffer is read-only"))

  (let (the-record)
    ;; Build new record.
    (setq forms--the-record-list (forms--parse-form))
    (setq the-record
	  (mapconcat 'identity forms--the-record-list forms-field-sep))

    (if (string-match (regexp-quote forms-field-sep)
		      (mapconcat 'identity forms--the-record-list ""))
	(error "Field separator occurs in record - update refused"))

    ;; Handle multi-line fields, if allowed.
    (if forms-multi-line
	(forms--trans the-record "\n" forms-multi-line))

    ;; A final sanity check before updating.
    (if (string-match "\n" the-record)
	(error "Multi-line fields in this record - update refused"))

    (with-current-buffer forms--file-buffer
      ;; Use delete-region instead of kill-region, to avoid
      ;; adding junk to the kill-ring.
      (delete-region (line-beginning-position) (line-end-position))
      (insert the-record)
      (beginning-of-line))))

(defun forms--checkmod ()
  "Check if this form has been modified, and call forms--update if so."
  (if (buffer-modified-p nil)
      (let ((here (point)))
	(forms--update)
	(set-buffer-modified-p nil)
	(goto-char here))))

;;; Start and exit

;;;###autoload
(defun forms-find-file (fn)
  "Visit a file in Forms mode."
  (interactive "fForms file: ")
  (let ((enable-local-eval t)
	(enable-local-variables t))
    (find-file-read-only fn)
    (or forms--mode-setup (forms-mode t))))

;;;###autoload
(defun forms-find-file-other-window (fn)
  "Visit a file in Forms mode in other window."
  (interactive "fFbrowse file in other window: ")
  (let ((enable-local-eval t)
	(enable-local-variables t))
    (find-file-other-window fn)
    (or forms--mode-setup (forms-mode t))))

(defun forms-exit ()
  "Normal exit from Forms mode.  Modified buffers are saved."
  (interactive)
  (forms--exit t))

(defun forms-exit-no-save ()
  "Exit from Forms mode without saving buffers."
  (interactive)
  (forms--exit nil))

;;; Navigating commands

(defun forms-next-record (arg)
  "Advance to the ARGth following record."
  (interactive "P")
  (forms-jump-record (+ forms--current-record (prefix-numeric-value arg)) t))

(defun forms-prev-record (arg)
  "Advance to the ARGth previous record."
  (interactive "P")
  (forms-jump-record (- forms--current-record (prefix-numeric-value arg)) t))

(defun forms--goto-record (rn &optional current)
  "Goto record number RN.
If CURRENT is provided, it specifies the current record and can be used
to speed up access to RN.  Returns the number of records missing, if any."
  (if current
      (forward-line (- rn current))
    ;; goto-line does not do what we want when the buffer is narrowed.
    (goto-char (point-min))
    (forward-line (1- rn))))

(defun forms-jump-record (arg &optional relative)
  "Jump to a random record."
  (interactive "NRecord number: ")

  ;; Verify that the record number is within range.
  (if (or (> arg forms--total-records)
	  (<= arg 0))
    (error
      ;; Don't give the message if just paging.
      (if (not relative)
	  (message "Record number %d out of range 1..%d"
		   arg forms--total-records)
	"")))

  ;; Flush.
  (forms--checkmod)

  ;; Calculate displacement.
  (let ((cur forms--current-record))

    ;; `forms--show-record' needs it now.
    (setq forms--current-record arg)

    ;; Get the record and show it.
    (forms--show-record
     (with-current-buffer forms--file-buffer
       (beginning-of-line)

       ;; Move, and adjust the amount if needed (shouldn't happen).
       (setq cur (- arg (forms--goto-record arg (if relative cur))))

       (forms--get-record)))

    ;; This shouldn't happen.
    (if (/= forms--current-record cur)
	(progn
	  (setq forms--current-record cur)
	  (error "Stuck at record %d" cur)))))

(defun forms-first-record ()
  "Jump to first record."
  (interactive)
  (forms-jump-record 1))

(defun forms-last-record ()
  "Jump to last record.
As a side effect: re-calculates the number of records in the data file."
  (interactive)
  (let
      ((numrec
	(with-current-buffer forms--file-buffer
	  (count-lines (point-min) (point-max)))))
    (if (= numrec forms--total-records)
	nil
      (setq forms--total-records numrec)
      (message "Warning: number of records changed to %d" forms--total-records)))
  (forms-jump-record forms--total-records))

;;; Other commands

(defun forms-toggle-read-only (arg)
  "Toggles read-only mode of a forms mode buffer.
With an argument, enables read-only mode if the argument is positive.
Otherwise enables edit mode if the visited file is writable."

  (interactive "P")

  (if (if arg
	  ;; Negative arg means switch it off.
	  (<= (prefix-numeric-value arg) 0)
	;; No arg means toggle.
	forms-read-only)

      ;; Enable edit mode, if possible.
      (let ((ro forms-read-only))
	(if (with-current-buffer forms--file-buffer
	      buffer-read-only)
	    (progn
	      (setq forms-read-only t)
	      (message "No write access to `%s'" forms-file))
	  (setq forms-read-only nil))
	(if (equal ro forms-read-only)
	    nil
	  (forms-mode)))

    ;; Enable view mode.
    (if forms-read-only
	nil
      (forms--checkmod)			; sync
      (setq forms-read-only t)
      (forms-mode))))

;; Sample:
;; (defun my-new-record-filter (the-fields)
;;   ;; numbers are relative to 1
;;   (aset the-fields 4 (current-time-string))
;;   (aset the-fields 6 (user-login-name))
;;   the-list)
;; (setq forms-new-record-filter 'my-new-record-filter)

(defun forms-insert-record (arg)
  "Create a new record before the current one.
With ARG: store the record after the current one.
If `forms-new-record-filter' contains the name of a function,
it is called to fill (some of) the fields with default values.
If `forms-insert-after is non-nil, the default behavior is to insert
after the current record."

  (interactive "P")

  (if forms-read-only
      (error ""))

  (let (ln the-list the-record)

    (if (or (and arg forms-insert-after)
	    (and (not arg) (not forms-insert-after)))
	(setq ln forms--current-record)
      (setq ln (1+ forms--current-record)))

    (forms--checkmod)
    (if forms-new-record-filter
	;; As a service to the user, we add a zeroth element so she
	;; can use the same indices as in the forms definition.
	(let ((the-fields (make-vector (1+ forms-number-of-fields) "")))
	  (setq the-fields (funcall forms-new-record-filter the-fields))
	  (setq the-list (cdr (append the-fields nil))))
      (setq the-list (make-list forms-number-of-fields "")))

    (setq the-record
	  (mapconcat
	  'identity
	  the-list
	  forms-field-sep))

    (with-current-buffer forms--file-buffer
      (forms--goto-record ln)
      (open-line 1)
      (insert the-record)
      (beginning-of-line))

    (setq forms--current-record ln))

  (setq forms--total-records (1+ forms--total-records))
  (forms-jump-record forms--current-record))

(defun forms-delete-record (arg)
  "Deletes a record.  With a prefix argument: don't ask."
  (interactive "P")

  (if forms-read-only
      (error ""))

  (forms--checkmod)
  (if (or arg
	  (y-or-n-p "Really delete this record? "))
      (let ((ln forms--current-record))
	(with-current-buffer forms--file-buffer
	  (forms--goto-record ln)
	  ;; Use delete-region instead of kill-region, to avoid
	  ;; adding junk to the kill-ring.
	  (delete-region (progn (beginning-of-line) (point))
			 (progn (beginning-of-line 2) (point))))
	(setq forms--total-records (1- forms--total-records))
	(if (> forms--current-record forms--total-records)
	    (setq forms--current-record forms--total-records))
	(forms-jump-record forms--current-record)))
  (message ""))

(defun forms-search-forward (regexp)
  "Search forward for record containing REGEXP."
  (interactive
   (list (read-string (concat "Search forward for"
				  (if forms--search-regexp
				   (concat " ("
					   forms--search-regexp
					   ")"))
				  ": "))))
  (if (equal "" regexp)
      (setq regexp forms--search-regexp))
  (forms--checkmod)

  (let (the-line the-record here)
    (with-current-buffer forms--file-buffer
      (end-of-line)
      (setq here (point))
      (if (or (re-search-forward regexp nil t)
	      (and (> here (point-min))
		   (progn
		     (goto-char (point-min))
		     (re-search-forward regexp here t))))
	  (progn
	    (setq the-record (forms--get-record))
	    (setq the-line (1+ (count-lines (point-min) (point))))
	    (if (< (point) here)
		(message "Wrapped")))
	(goto-char here)
	(error "Search failed: %s" regexp)))
    (setq forms--current-record the-line)
    (forms--show-record the-record))
  (re-search-forward regexp nil t)
  (setq forms--search-regexp regexp))

(defun forms-search-backward (regexp)
  "Search backward for record containing REGEXP."
  (interactive
   (list (read-string (concat "Search backward for"
				  (if forms--search-regexp
				   (concat " ("
					   forms--search-regexp
					   ")"))
				  ": "))))
  (if (equal "" regexp)
      (setq regexp forms--search-regexp))
  (forms--checkmod)

  (let (the-line the-record here)
    (with-current-buffer forms--file-buffer
      (beginning-of-line)
      (setq here (point))
      (if (or (re-search-backward regexp nil t)
	      (and (< (point) (point-max))
		   (progn
		     (goto-char (point-max))
		     (re-search-backward regexp here t))))
	  (progn
	    (setq the-record (forms--get-record))
	    (setq the-line (1+ (count-lines (point-min) (point))))
	    (if (> (point) here)
		(message "Wrapped")))
	(goto-char here)
	(error "Search failed: %s" regexp)))
    (setq forms--current-record the-line)
    (forms--show-record the-record))
  (re-search-forward regexp nil t)
  (setq forms--search-regexp regexp))

(defun forms-save-buffer (&optional args)
  "Forms mode replacement for save-buffer.
It saves the data buffer instead of the forms buffer.
Calls `forms-write-file-filter' before, and `forms-read-file-filter'
after writing out the data."
  (interactive "p")
  (forms--checkmod)
  (let ((write-file-filter forms-write-file-filter)
	(read-file-filter forms-read-file-filter)
	(cur forms--current-record))
    (with-current-buffer forms--file-buffer
      (let ((inhibit-read-only t))
	;; Write file hooks are run via write-file-functions.
	;; (if write-file-filter
	;;  (save-excursion
	;;   (run-hooks 'write-file-filter)))

	;; If they have a write-file-filter, force the buffer to be
	;; saved even if it doesn't seem to be changed.  First, they
	;; might have changed the write-file-filter; and second, if
	;; save-buffer does nothing, write-file-filter won't get run,
	;; and then read-file-filter will be mightily confused.
	(or (null write-file-filter)
	    (set-buffer-modified-p t))
	(save-buffer args)
	(if read-file-filter
	   (save-excursion
	     (run-hooks 'read-file-filter)))
	(set-buffer-modified-p nil)))
    ;; Make sure we end up with the same record number as we started.
    ;; Since read-file-filter may perform arbitrary transformations on
    ;; the data buffer contents, save-excursion is not enough.
    (forms-jump-record cur))
  t)

(defun forms--revert-buffer (&optional _arg noconfirm)
  "Reverts current form to un-modified."
  (interactive "P")
  (if (or noconfirm
	  (yes-or-no-p "Revert form to unmodified? "))
      (progn
	(set-buffer-modified-p nil)
	(forms-jump-record forms--current-record))))

(defun forms-next-field (arg)
  "Jump to ARG-th next field."
  (interactive "p")

  (let ((i 0)
	(here (point))
	there
	(cnt 0)
	(inhibit-point-motion-hooks t))

    (if (zerop arg)
	(setq cnt 1)
      (setq cnt (+ cnt arg)))

    (if (catch 'done
	  (while (< i (length forms--markers))
	    (if (or (null (setq there (aref forms--markers i)))
		    (<= there here))
		nil
	      (if (<= (setq cnt (1- cnt)) 0)
		  (progn
		    (goto-char there)
		    (throw 'done t))))
	    (setq i (1+ i))))
	nil
      (goto-char (aref forms--markers 0)))))

(defun forms-prev-field (arg)
  "Jump to ARG-th previous field."
  (interactive "p")

  (let ((i (length forms--markers))
	(here (point))
	there
	(cnt 0)
	(inhibit-point-motion-hooks t))

    (if (zerop arg)
	(setq cnt 1)
      (setq cnt (+ cnt arg)))

    (if (catch 'done
	  (while (> i 0)
	    (setq i ( 1- i))
	    (if (or (null (setq there (aref forms--markers i)))
		    (>= there here))
		nil
	      (if (<= (setq cnt (1- cnt)) 0)
		  (progn
		    (goto-char there)
		    (throw 'done t))))))
	nil
      (goto-char (aref forms--markers (1- (length forms--markers)))))))

(defun forms-print ()
  "Send the records to the printer with `print-buffer', one record per page."
  (interactive)
  (let ((inhibit-read-only t)
	(save-record forms--current-record)
	(total-nb-records forms--total-records)
	(nb-record 1)
	(record nil))
    (while (<= nb-record forms--total-records)
      (forms-jump-record nb-record)
      (setq record (buffer-string))
      (with-current-buffer (get-buffer-create "*forms-print*")
	(goto-char (buffer-end 1))
	(insert record)
	(setq buffer-read-only nil)
	(if (< nb-record total-nb-records)
	    (insert "\n\n")))
      (setq nb-record (1+ nb-record)))
    (with-current-buffer "*forms-print*"
      (print-buffer)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (forms-jump-record save-record)))

;;;
;;; Special service
;;;
(defun forms-enumerate (the-fields)
  "Take a quoted list of symbols, and set their values to sequential numbers.
The first symbol gets number 1, the second 2 and so on.
It returns the highest number.

Usage: (setq forms-number-of-fields
             (forms-enumerate
              '(field1 field2 field2 ...)))"

  (let ((the-index 0))
    (while the-fields
      (setq the-index (1+ the-index))
      (let ((el (car-safe the-fields)))
	(setq the-fields (cdr-safe the-fields))
	(set el the-index)))
    the-index))

;;; Debugging

(defvar forms--debug nil
  "*Enables forms-mode debugging if not nil.")

(defun forms--debug (&rest args)
  "Internal debugging routine."
  (if forms--debug
      (let ((ret nil))
	(while args
	  (let ((el (car-safe args)))
	    (setq args (cdr-safe args))
	    (if (stringp el)
		(setq ret (concat ret el))
	      (setq ret (concat ret (prin1-to-string el) " = "))
	      (if (boundp el)
		  (let ((vel (eval el)))
		    (setq ret (concat ret (prin1-to-string vel) "\n")))
		(setq ret (concat ret "<unbound>" "\n")))
	      (if (fboundp el)
		  (setq ret (concat ret (prin1-to-string (symbol-function el))
				    "\n"))))))
	(with-current-buffer (get-buffer-create "*forms-mode debug*")
	  (if (zerop (buffer-size))
	      (emacs-lisp-mode))
	  (goto-char (point-max))
	  (insert ret)))))

;;; forms.el ends here
