;;; abbrev.el --- abbrev mode commands for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 1985-1987, 1992, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: abbrev convenience
;; Package: emacs

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

;; This facility is documented in the Emacs Manual.

;; Todo:

;; - Cleanup name space.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup abbrev-mode nil
  "Word abbreviations mode."
  :link '(custom-manual "(emacs)Abbrevs")
  :group 'abbrev)

(defcustom abbrev-file-name
  (locate-user-emacs-file "abbrev_defs" ".abbrev_defs")
  "Default name of file from which to read abbrevs."
  :initialize 'custom-initialize-delay
  :type 'file)

(defcustom only-global-abbrevs nil
  "Non-nil means user plans to use global abbrevs only.
This makes the commands that normally define mode-specific abbrevs
define global abbrevs instead."
  :type 'boolean
  :group 'abbrev-mode
  :group 'convenience)

(define-minor-mode abbrev-mode
  "Toggle Abbrev mode in the current buffer.
With a prefix argument ARG, enable Abbrev mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Abbrev mode if ARG is omitted or nil.

In Abbrev mode, inserting an abbreviation causes it to expand and
be replaced by its expansion."
  ;; It's defined in C, this stops the d-m-m macro defining it again.
  :variable abbrev-mode)

(put 'abbrev-mode 'safe-local-variable 'booleanp)


(defvar edit-abbrevs-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'abbrev-edit-save-buffer)
    (define-key map "\C-x\C-w" 'abbrev-edit-save-to-file)
    (define-key map "\C-c\C-c" 'edit-abbrevs-redefine)
    map)
  "Keymap used in `edit-abbrevs'.")

(defun kill-all-abbrevs ()
  "Undefine all defined abbrevs."
  (interactive)
  (dolist (tablesym abbrev-table-name-list)
    (clear-abbrev-table (symbol-value tablesym))))

(defun copy-abbrev-table (table)
  "Make a new abbrev-table with the same abbrevs as TABLE.
Does not copy property lists."
  (let ((new-table (make-abbrev-table)))
    (mapatoms
     (lambda (symbol)
       (define-abbrev new-table
	 (symbol-name symbol)
	 (symbol-value symbol)
	 (symbol-function symbol)))
     table)
    new-table))

(defun insert-abbrevs ()
  "Insert after point a description of all defined abbrevs.
Mark is set after the inserted text."
  (interactive)
  (push-mark
   (save-excursion
     (dolist (tablesym abbrev-table-name-list)
       (insert-abbrev-table-description tablesym t))
     (point))))

(defun list-abbrevs (&optional local)
  "Display a list of defined abbrevs.
If LOCAL is non-nil, interactively when invoked with a
prefix arg, display only local, i.e. mode-specific, abbrevs.
Otherwise display all abbrevs."
  (interactive "P")
  (display-buffer (prepare-abbrev-list-buffer local)))

(defun abbrev-table-name (table)
  "Value is the name of abbrev table TABLE."
  (let ((tables abbrev-table-name-list)
	found)
    (while (and (not found) tables)
      (when (eq (symbol-value (car tables)) table)
	(setq found (car tables)))
      (setq tables (cdr tables)))
    found))

(defun prepare-abbrev-list-buffer (&optional local)
  (let ((local-table local-abbrev-table))
    (with-current-buffer (get-buffer-create "*Abbrevs*")
      (erase-buffer)
      (if local
          (insert-abbrev-table-description
           (abbrev-table-name local-table) t)
        (let (empty-tables)
	  (dolist (table abbrev-table-name-list)
	    (if (abbrev-table-empty-p (symbol-value table))
		(push table empty-tables)
	      (insert-abbrev-table-description table t)))
	  (dolist (table (nreverse empty-tables))
	    (insert-abbrev-table-description table t))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (edit-abbrevs-mode)
      (current-buffer))))

(defun edit-abbrevs-mode ()
  "Major mode for editing the list of abbrev definitions.
\\{edit-abbrevs-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'edit-abbrevs-mode)
  (setq mode-name "Edit-Abbrevs")
  (use-local-map edit-abbrevs-map)
  (run-mode-hooks 'edit-abbrevs-mode-hook))

(defun edit-abbrevs ()
  "Alter abbrev definitions by editing a list of them.
Selects a buffer containing a list of abbrev definitions.
You can edit them and type \\<edit-abbrevs-map>\\[edit-abbrevs-redefine] to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted)."
  (interactive)
  (switch-to-buffer (prepare-abbrev-list-buffer)))

(defun edit-abbrevs-redefine ()
  "Redefine abbrevs according to current buffer contents."
  (interactive)
  (save-restriction
    (widen)
    (define-abbrevs t)
    (set-buffer-modified-p nil)))

(defun define-abbrevs (&optional arg)
  "Define abbrevs according to current visible buffer contents.
See documentation of `edit-abbrevs' for info on the format of the
text you must have in the buffer.
With argument, eliminate all abbrev definitions except
the ones defined from the buffer now."
  (interactive "P")
  (if arg (kill-all-abbrevs))
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (re-search-forward "^(" nil t))
      (let* ((buf (current-buffer))
	     (table (read buf))
	     abbrevs name hook exp count sys)
	(forward-line 1)
	(while (progn (forward-line 1)
		      (not (eolp)))
	  (setq name (read buf) count (read buf))
	  (if (equal count '(sys))
	      (setq sys t count (read buf))
	    (setq sys nil))
	  (setq exp (read buf))
	  (skip-chars-backward " \t\n\f")
	  (setq hook (if (not (eolp)) (read buf)))
	  (skip-chars-backward " \t\n\f")
	  (setq abbrevs (cons (list name exp hook count sys) abbrevs)))
	(define-abbrev-table table abbrevs)))))

(defun read-abbrev-file (&optional file quietly)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Optional second argument QUIETLY non-nil means don't display a message."
  (interactive
   (list
    (read-file-name (format "Read abbrev file (default %s): "
			    abbrev-file-name)
		    nil abbrev-file-name t)))
  (load (or file abbrev-file-name) nil quietly)
  (setq abbrevs-changed nil))

(defun quietly-read-abbrev-file (&optional file)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Does not display any message."
					;(interactive "fRead abbrev file: ")
  (read-abbrev-file file t))

(defun write-abbrev-file (&optional file verbose)
  "Write all user-level abbrev definitions to a file of Lisp code.
This does not include system abbrevs; it includes only the abbrev tables
listed in listed in `abbrev-table-name-list'.
The file written can be loaded in another session to define the same abbrevs.
The argument FILE is the file name to write.  If omitted or nil, the file
specified in `abbrev-file-name' is used.
If VERBOSE is non-nil, display a message indicating where abbrevs
have been saved."
  (interactive
   (list
    (read-file-name "Write abbrev file: "
		    (file-name-directory (expand-file-name abbrev-file-name))
		    abbrev-file-name)))
  (or (and file (> (length file) 0))
      (setq file abbrev-file-name))
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (dolist (table
	       ;; We sort the table in order to ease the automatic
	       ;; merging of different versions of the user's abbrevs
	       ;; file.  This is useful, for example, for when the
	       ;; user keeps their home directory in a revision
	       ;; control system, and is therefore keeping multiple
	       ;; slightly-differing copies loosely synchronized.
	       (sort (copy-sequence abbrev-table-name-list)
		     (lambda (s1 s2)
		       (string< (symbol-name s1)
				(symbol-name s2)))))
	(insert-abbrev-table-description table nil))
      (when (unencodable-char-position (point-min) (point-max) 'utf-8)
	(setq coding-system-for-write
	      (if (> emacs-major-version 24)
		  'utf-8-emacs
		;; For compatibility with Emacs 22 (See Bug#8308)
		'emacs-mule)))
      (goto-char (point-min))
      (insert (format ";;-*-coding: %s;-*-\n" coding-system-for-write))
      (write-region nil nil file nil (and (not verbose) 0)))))

(defun abbrev-edit-save-to-file (file)
  "Save all user-level abbrev definitions in current buffer to FILE."
  (interactive
   (list (read-file-name "Save abbrevs to file: "
			 (file-name-directory
			  (expand-file-name abbrev-file-name))
			 abbrev-file-name)))
  (edit-abbrevs-redefine)
  (write-abbrev-file file t))

(defun abbrev-edit-save-buffer ()
  "Save all user-level abbrev definitions in current buffer.
The saved abbrevs are written to the file specified by
`abbrev-file-name'."
  (interactive)
  (abbrev-edit-save-to-file abbrev-file-name))


(defun add-mode-abbrev (arg)
  "Define mode-specific abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
Reads the abbreviation in the minibuffer.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "p")
  (add-abbrev
   (if only-global-abbrevs
       global-abbrev-table
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" arg))

(defun add-global-abbrev (arg)
  "Define global (all modes) abbrev for last word(s) before point.
The prefix argument specifies the number of words before point that form the
expansion; or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
This command uses the minibuffer to read the abbreviation.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "p")
  (add-abbrev global-abbrev-table "Global" arg))

(defun add-abbrev (table type arg)
  (let ((exp (and (>= arg 0)
		  (buffer-substring-no-properties
		   (point)
		   (if (= arg 0) (mark)
		     (save-excursion (forward-word (- arg)) (point))))))
	name)
    (setq name
	  (read-string (format (if exp "%s abbrev for \"%s\": "
				 "Undefine %s abbrev: ")
			       type exp)))
    (set-text-properties 0 (length name) nil name)
    (if (or (null exp)
	    (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp))))

(defun inverse-add-mode-abbrev (n)
  "Define last word before point as a mode-specific abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev
   (if only-global-abbrevs
       global-abbrev-table
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" n))

(defun inverse-add-global-abbrev (n)
  "Define last word before point as a global (mode-independent) abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev global-abbrev-table "Global" n))

(defun inverse-add-abbrev (table type arg)
  (let (name exp start end)
    (save-excursion
      (forward-word (1+ (- arg)))
      (setq end (point))
      (backward-word 1)
      (setq start (point)
	    name (buffer-substring-no-properties start end)))

    (setq exp (read-string (format "%s expansion for \"%s\": " type name)
			   nil nil nil t))
    (when (or (not (abbrev-expansion name table))
	      (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				name (abbrev-expansion name table))))
      (define-abbrev table (downcase name) exp)
      (save-excursion
	(goto-char end)
	(expand-abbrev)))))

(defun abbrev-prefix-mark (&optional arg)
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word.
This way, you can expand an abbrev with a prefix: insert the prefix,
use this command, then insert the abbrev.  This command inserts a
temporary hyphen after the prefix (until the intended abbrev
expansion occurs).
If the prefix is itself an abbrev, this command expands it, unless
ARG is non-nil.  Interactively, ARG is the prefix argument."
  (interactive "P")
  (or arg (expand-abbrev))
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (insert "-"))

(defun expand-region-abbrevs (start end &optional noquery)
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type `y' or `n' for each occurrence.
A prefix argument means don't query; expand all abbrevs."
  (interactive "r\nP")
  (save-excursion
    (goto-char start)
    (let ((lim (- (point-max) end))
	  pnt string)
      (while (and (not (eobp))
		  (progn (forward-word 1)
			 (<= (setq pnt (point)) (- (point-max) lim))))
	(if (abbrev-expansion
	     (setq string
		   (buffer-substring-no-properties
		    (save-excursion (forward-word -1) (point))
		    pnt)))
	    (if (or noquery (y-or-n-p (format "Expand `%s'? " string)))
		(expand-abbrev)))))))

;;; Abbrev properties.

(defun abbrev-table-get (table prop)
  "Get the PROP property of abbrev table TABLE."
  (let ((sym (intern-soft "" table)))
    (if sym (get sym prop))))

(defun abbrev-table-put (table prop val)
  "Set the PROP property of abbrev table TABLE to VAL."
  (let ((sym (intern "" table)))
    (set sym nil)	     ; Make sure it won't be confused for an abbrev.
    (put sym prop val)))

(defalias 'abbrev-get 'get
  "Get the property PROP of abbrev ABBREV

\(fn ABBREV PROP)")

(defalias 'abbrev-put 'put
  "Set the property PROP of abbrev ABREV to value VAL.
See `define-abbrev' for the effect of some special properties.

\(fn ABBREV PROP VAL)")

;;; Code that used to be implemented in src/abbrev.c

(defvar abbrev-table-name-list '(fundamental-mode-abbrev-table
				 global-abbrev-table)
  "List of symbols whose values are abbrev tables.")

(defun make-abbrev-table (&optional props)
  "Create a new, empty abbrev table object.
PROPS is a list of properties."
  ;; The value 59 is an arbitrary prime number.
  (let ((table (make-vector 59 0)))
    ;; Each abbrev-table has a `modiff' counter which can be used to detect
    ;; when an abbreviation was added.  An example of use would be to
    ;; construct :regexp dynamically as the union of all abbrev names, so
    ;; `modiff' can let us detect that an abbrev was added and hence :regexp
    ;; needs to be refreshed.
    ;; The presence of `modiff' entry is also used as a tag indicating this
    ;; vector is really an abbrev-table.
    (abbrev-table-put table :abbrev-table-modiff 0)
    (while (consp props)
      (abbrev-table-put table (pop props) (pop props)))
    table))

(defun abbrev-table-p (object)
  "Return non-nil if OBJECT is an abbrev table."
  (and (vectorp object)
       (numberp (abbrev-table-get object :abbrev-table-modiff))))

(defun abbrev-table-empty-p (object &optional ignore-system)
  "Return nil if there are no abbrev symbols in OBJECT.
If IGNORE-SYSTEM is non-nil, system definitions are ignored."
  (unless (abbrev-table-p object)
    (error "Non abbrev table object"))
  (not (catch 'some
	 (mapatoms (lambda (abbrev)
		     (unless (or (zerop (length (symbol-name abbrev)))
				 (and ignore-system
				      (abbrev-get abbrev :system)))
		       (throw 'some t)))
		   object))))

(defvar global-abbrev-table (make-abbrev-table)
  "The abbrev table whose abbrevs affect all buffers.
Each buffer may also have a local abbrev table.
If it does, the local table overrides the global one
for any particular abbrev defined in both.")

(defvar abbrev-minor-mode-table-alist nil
  "Alist of abbrev tables to use for minor modes.
Each element looks like (VARIABLE . ABBREV-TABLE);
ABBREV-TABLE is active whenever VARIABLE's value is non-nil.
ABBREV-TABLE can also be a list of abbrev tables.")

(defvar fundamental-mode-abbrev-table
  (let ((table (make-abbrev-table)))
    ;; Set local-abbrev-table's default to be fundamental-mode-abbrev-table.
    (setq-default local-abbrev-table table)
    table)
  "The abbrev table of mode-specific abbrevs for Fundamental Mode.")

(defvar abbrevs-changed nil
  "Set non-nil by defining or altering any word abbrevs.
This causes `save-some-buffers' to offer to save the abbrevs.")

(defcustom abbrev-all-caps nil
  "Non-nil means expand multi-word abbrevs all caps if abbrev was so."
  :type 'boolean
  :group 'abbrev-mode)

(defvar abbrev-start-location nil
  "Buffer position for `expand-abbrev' to use as the start of the abbrev.
When nil, use the word before point as the abbrev.
Calling `expand-abbrev' sets this to nil.")

(defvar abbrev-start-location-buffer nil
  "Buffer that `abbrev-start-location' has been set for.
Trying to expand an abbrev in any other buffer clears `abbrev-start-location'.")

(defvar last-abbrev nil
  "The abbrev-symbol of the last abbrev expanded.  See `abbrev-symbol'.")

(defvar last-abbrev-text nil
  "The exact text of the last abbrev expanded.
It is nil if the abbrev has already been unexpanded.")

(defvar last-abbrev-location 0
  "The location of the start of the last abbrev expanded.")

;; (defvar local-abbrev-table fundamental-mode-abbrev-table
;;   "Local (mode-specific) abbrev table of current buffer.")
;; (make-variable-buffer-local 'local-abbrev-table)

(defcustom pre-abbrev-expand-hook nil
  "Function or functions to be called before abbrev expansion is done.
This is the first thing that `expand-abbrev' does, and so this may change
the current abbrev table before abbrev lookup happens."
  :type 'hook
  :group 'abbrev-mode)
(make-obsolete-variable 'pre-abbrev-expand-hook 'abbrev-expand-functions "23.1")

(defun clear-abbrev-table (table)
  "Undefine all abbrevs in abbrev table TABLE, leaving it empty."
  (setq abbrevs-changed t)
  (let* ((sym (intern-soft "" table)))
    (dotimes (i (length table))
      (aset table i 0))
    ;; Preserve the table's properties.
    (assert sym)
    (let ((newsym (intern "" table)))
      (set newsym nil)	     ; Make sure it won't be confused for an abbrev.
      (setplist newsym (symbol-plist sym)))
    (abbrev-table-put table :abbrev-table-modiff
                      (1+ (abbrev-table-get table :abbrev-table-modiff))))
  ;; For backward compatibility, always return nil.
  nil)

(defun define-abbrev (table name expansion &optional hook &rest props)
  "Define an abbrev in TABLE named NAME, to expand to EXPANSION and call HOOK.
NAME must be a string, and should be lower-case.
EXPANSION should usually be a string.
To undefine an abbrev, define it with EXPANSION = nil.
If HOOK is non-nil, it should be a function of no arguments;
it is called after EXPANSION is inserted.
If EXPANSION is not a string (and not nil), the abbrev is a
 special one, which does not expand in the usual way but only
 runs HOOK.

If HOOK is a non-nil symbol with a non-nil `no-self-insert' property,
it can control whether the character that triggered abbrev expansion
is inserted.  If such a HOOK returns non-nil, the character is not
inserted.  If such a HOOK returns nil, then so does `abbrev-insert'
\(and `expand-abbrev'), as if no abbrev expansion had taken place.

PROPS is a property list.  The following properties are special:
- `:count': the value for the abbrev's usage-count, which is incremented each
  time the abbrev is used (the default is zero).
- `:system': if non-nil, says that this is a \"system\" abbreviation
  which should not be saved in the user's abbreviation file.
  Unless `:system' is `force', a system abbreviation will not
  overwrite a non-system abbreviation of the same name.
- `:case-fixed': non-nil means that abbreviations are looked up without
  case-folding, and the expansion is not capitalized/upcased.
- `:enable-function': a function of no argument which returns non-nil if the
  abbrev should be used for a particular call of `expand-abbrev'.

An obsolete but still supported calling form is:

\(define-abbrev TABLE NAME EXPANSION &optional HOOK COUNT SYSTEM)."
  (when (and (consp props) (or (null (car props)) (numberp (car props))))
    ;; Old-style calling convention.
    (setq props (list* :count (car props)
                       (if (cadr props) (list :system (cadr props))))))
  (unless (plist-get props :count)
    (setq props (plist-put props :count 0)))
  (let ((system-flag (plist-get props :system))
        (sym (intern name table)))
    ;; Don't override a prior user-defined abbrev with a system abbrev,
    ;; unless system-flag is `force'.
    (unless (and (not (memq system-flag '(nil force)))
                 (boundp sym) (symbol-value sym)
                 (not (abbrev-get sym :system)))
      (unless (or system-flag
                  (and (boundp sym) (fboundp sym)
                       ;; load-file-name
                       (equal (symbol-value sym) expansion)
                       (equal (symbol-function sym) hook)))
        (setq abbrevs-changed t))
      (set sym expansion)
      (fset sym hook)
      (setplist sym
                ;; Don't store the `force' value of `system-flag' into
                ;; the :system property.
                (if (eq 'force system-flag) (plist-put props :system t) props))
      (abbrev-table-put table :abbrev-table-modiff
                        (1+ (abbrev-table-get table :abbrev-table-modiff))))
    name))

(defun abbrev--check-chars (abbrev global)
  "Check if the characters in ABBREV have word syntax in either the
current (if global is nil) or standard syntax table."
  (with-syntax-table
      (cond ((null global) (standard-syntax-table))
            ;; ((syntax-table-p global) global)
            (t (syntax-table)))
    (when (string-match "\\W" abbrev)
      (let ((badchars ())
            (pos 0))
        (while (string-match "\\W" abbrev pos)
          (pushnew (aref abbrev (match-beginning 0)) badchars)
          (setq pos (1+ pos)))
        (error "Some abbrev characters (%s) are not word constituents %s"
               (apply 'string (nreverse badchars))
               (if global "in the standard syntax" "in this mode"))))))

(defun define-global-abbrev (abbrev expansion)
  "Define ABBREV as a global abbreviation for EXPANSION.
The characters in ABBREV must all be word constituents in the standard
syntax table."
  (interactive "sDefine global abbrev: \nsExpansion for %s: ")
  (abbrev--check-chars abbrev 'global)
  (define-abbrev global-abbrev-table (downcase abbrev) expansion))

(defun define-mode-abbrev (abbrev expansion)
  "Define ABBREV as a mode-specific abbreviation for EXPANSION.
The characters in ABBREV must all be word-constituents in the current mode."
  (interactive "sDefine mode abbrev: \nsExpansion for %s: ")
  (unless local-abbrev-table
    (error "Major mode has no abbrev table"))
  (abbrev--check-chars abbrev nil)
  (define-abbrev local-abbrev-table (downcase abbrev) expansion))

(defun abbrev--active-tables (&optional tables)
  "Return the list of abbrev tables currently active.
TABLES if non-nil overrides the usual rules.  It can hold
either a single abbrev table or a list of abbrev tables."
  ;; We could just remove the `tables' arg and let callers use
  ;; (or table (abbrev--active-tables)) but then they'd have to be careful
  ;; to treat the distinction between a single table and a list of tables.
  (cond
   ((consp tables) tables)
   ((vectorp tables) (list tables))
   (t
    (let ((tables (if (listp local-abbrev-table)
                      (append local-abbrev-table
                              (list global-abbrev-table))
                    (list local-abbrev-table global-abbrev-table))))
      ;; Add the minor-mode abbrev tables.
      (dolist (x abbrev-minor-mode-table-alist)
        (when (and (symbolp (car x)) (boundp (car x)) (symbol-value (car x)))
          (setq tables
                (if (listp (cdr x))
                    (append (cdr x) tables) (cons (cdr x) tables)))))
      tables))))


(defun abbrev-symbol (abbrev &optional table)
  "Return the symbol representing abbrev named ABBREV.
This symbol's name is ABBREV, but it is not the canonical symbol of that name;
it is interned in an abbrev-table rather than the normal obarray.
The value is nil if that abbrev is not defined.
Optional second arg TABLE is abbrev table to look it up in.
The default is to try buffer's mode-specific abbrev table, then global table."
  (let ((tables (abbrev--active-tables table))
        sym)
    (while (and tables (not (symbol-value sym)))
      (let* ((table (pop tables))
             (case-fold (not (abbrev-table-get table :case-fixed))))
        (setq tables (append (abbrev-table-get table :parents) tables))
        ;; In case the table doesn't set :case-fixed but some of the
        ;; abbrevs do, we have to be careful.
        (setq sym
              ;; First try without case-folding.
              (or (intern-soft abbrev table)
                  (when case-fold
                    ;; We didn't find any abbrev, try case-folding.
                    (let ((sym (intern-soft (downcase abbrev) table)))
                      ;; Only use it if it doesn't require :case-fixed.
                      (and sym (not (abbrev-get sym :case-fixed))
                           sym)))))))
    (if (symbol-value sym)
        sym)))


(defun abbrev-expansion (abbrev &optional table)
  "Return the string that ABBREV expands into in the current buffer.
Optionally specify an abbrev table as second arg;
then ABBREV is looked up in that table only."
  (symbol-value (abbrev-symbol abbrev table)))


(defun abbrev--before-point ()
  "Try and find an abbrev before point.  Return it if found, nil otherwise."
  (unless (eq abbrev-start-location-buffer (current-buffer))
    (setq abbrev-start-location nil))

  (let ((tables (abbrev--active-tables))
        (pos (point))
        start end name res)

    (if abbrev-start-location
        (progn
          (setq start abbrev-start-location)
          (setq abbrev-start-location nil)
          ;; Remove the hyphen inserted by `abbrev-prefix-mark'.
          (if (and (< start (point-max))
                   (eq (char-after start) ?-))
              (delete-region start (1+ start)))
          (skip-syntax-backward " ")
          (setq end (point))
          (when (> end start)
            (setq name (buffer-substring start end))
            (goto-char pos)               ; Restore point.
            (list (abbrev-symbol name tables) name start end)))

      (while (and tables (not (car res)))
        (let* ((table (pop tables))
               (enable-fun (abbrev-table-get table :enable-function)))
          (setq tables (append (abbrev-table-get table :parents) tables))
          (setq res
                (and (or (not enable-fun) (funcall enable-fun))
                     (let ((re (abbrev-table-get table :regexp)))
                       (if (null re)
                           ;; We used to default `re' to "\\<\\(\\w+\\)\\W*"
                           ;; but when words-include-escapes is set, that
                           ;; is not right and fixing it is boring.
                           (let ((lim (point)))
                             (backward-word 1)
                             (setq start (point))
                             (forward-word 1)
                             (setq end (min (point) lim)))
                         (when (looking-back re (line-beginning-position))
                           (setq start (match-beginning 1))
                           (setq end   (match-end 1)))))
                     (setq name  (buffer-substring start end))
                     (let ((abbrev (abbrev-symbol name table)))
                       (when abbrev
                         (setq enable-fun (abbrev-get abbrev :enable-function))
                         (and (or (not enable-fun) (funcall enable-fun))
                              ;; This will also look it up in parent tables.
                              ;; This is not on purpose, but it seems harmless.
                              (list abbrev name start end))))))
          ;; Restore point.
          (goto-char pos)))
      res)))

(defun abbrev-insert (abbrev &optional name wordstart wordend)
  "Insert abbrev ABBREV at point.
If non-nil, NAME is the name by which this abbrev was found.
If non-nil, WORDSTART is the place where to insert the abbrev.
If WORDEND is non-nil, the abbrev replaces the previous text between
WORDSTART and WORDEND.
Return ABBREV if the expansion should be considered as having taken place.
The return value can be influenced by a `no-self-insert' property;
see `define-abbrev' for details."
  (unless name (setq name (symbol-name abbrev)))
  (unless wordstart (setq wordstart (point)))
  (unless wordend (setq wordend wordstart))
  ;; Increment use count.
  (abbrev-put abbrev :count (1+ (abbrev-get abbrev :count)))
  (let ((value abbrev))
    ;; If this abbrev has an expansion, delete the abbrev
    ;; and insert the expansion.
    (when (stringp (symbol-value abbrev))
      (goto-char wordstart)
      ;; Insert at beginning so that markers at the end (e.g. point)
      ;; are preserved.
      (insert (symbol-value abbrev))
      (delete-char (- wordend wordstart))
      (let ((case-fold-search nil))
        ;; If the abbrev's name is different from the buffer text (the
        ;; only difference should be capitalization), then we may want
        ;; to adjust the capitalization of the expansion.
        (when (and (not (equal name (symbol-name abbrev)))
                   (string-match "[[:upper:]]" name))
          (if (not (string-match "[[:lower:]]" name))
              ;; Abbrev was all caps.  If expansion is multiple words,
              ;; normally capitalize each word.
              (if (and (not abbrev-all-caps)
                       (save-excursion
                         (> (progn (backward-word 1) (point))
                            (progn (goto-char wordstart)
                                   (forward-word 1) (point)))))
                  (upcase-initials-region wordstart (point))
                (upcase-region wordstart (point)))
            ;; Abbrev included some caps.  Cap first initial of expansion.
            (let ((end (point)))
              ;; Find the initial.
              (goto-char wordstart)
              (skip-syntax-forward "^w" (1- end))
              ;; Change just that.
              (upcase-initials-region (point) (1+ (point)))
              (goto-char end))))))
    ;; Now point is at the end of the expansion and the beginning is
    ;; in last-abbrev-location.
    (when (symbol-function abbrev)
      (let* ((hook (symbol-function abbrev))
             (expanded
              ;; If the abbrev has a hook function, run it.
              (funcall hook)))
        ;; In addition, if the hook function is a symbol with
        ;; a non-nil `no-self-insert' property, let the value it
        ;; returned specify whether we consider that an expansion took
        ;; place.  If it returns nil, no expansion has been done.
        (if (and (symbolp hook)
                 (null expanded)
                 (get hook 'no-self-insert))
            (setq value nil))))
    value))

(defvar abbrev-expand-functions nil
  "Wrapper hook around `expand-abbrev'.
The functions on this special hook are called with one argument:
a function that performs the abbrev expansion.  It should return
the abbrev symbol if expansion took place.")

(defun expand-abbrev ()
  "Expand the abbrev before point, if there is an abbrev there.
Effective when explicitly called even when `abbrev-mode' is nil.
Returns the abbrev symbol, if expansion took place.  (The actual
return value is that of `abbrev-insert'.)"
  (interactive)
  (run-hooks 'pre-abbrev-expand-hook)
  (with-wrapper-hook abbrev-expand-functions ()
    (destructuring-bind (&optional sym name wordstart wordend)
        (abbrev--before-point)
      (when sym
        (let ((startpos (copy-marker (point) t))
              (endmark (copy-marker wordend t)))
          (unless (or ;; executing-kbd-macro
                   noninteractive
                   (window-minibuffer-p (selected-window)))
            ;; Add an undo boundary, in case we are doing this for
            ;; a self-inserting command which has avoided making one so far.
            (undo-boundary))
          ;; Now sym is the abbrev symbol.
          (setq last-abbrev-text name)
          (setq last-abbrev sym)
          (setq last-abbrev-location wordstart)
          ;; If this abbrev has an expansion, delete the abbrev
          ;; and insert the expansion.
          (prog1
              (abbrev-insert sym name wordstart wordend)
            ;; Yuck!!  If expand-abbrev is called with point slightly
            ;; further than the end of the abbrev, move point back to
            ;; where it started.
            (if (and (> startpos endmark)
                     (= (point) endmark)) ;Obey skeletons that move point.
                (goto-char startpos))))))))

(defun unexpand-abbrev ()
  "Undo the expansion of the last abbrev that expanded.
This differs from ordinary undo in that other editing done since then
is not undone."
  (interactive)
  (save-excursion
    (unless (or (< last-abbrev-location (point-min))
                (> last-abbrev-location (point-max)))
      (goto-char last-abbrev-location)
      (when (stringp last-abbrev-text)
        ;; This isn't correct if last-abbrev's hook was used
        ;; to do the expansion.
        (let ((val (symbol-value last-abbrev)))
          (unless (stringp val)
            (error "Value of abbrev-symbol must be a string"))
          ;; Don't inherit properties here; just copy from old contents.
          (insert last-abbrev-text)
          ;; Delete after inserting, to better preserve markers.
          (delete-region (point) (+ (point) (length val)))
          (setq last-abbrev-text nil))))))

(defun abbrev--write (sym)
  "Write the abbrev in a `read'able form.
Only writes the non-system abbrevs.
Presumes that `standard-output' points to `current-buffer'."
  (unless (or (null (symbol-value sym)) (abbrev-get sym :system))
    (insert "    (")
    (prin1 (symbol-name sym))
    (insert " ")
    (prin1 (symbol-value sym))
    (insert " ")
    (prin1 (symbol-function sym))
    (insert " ")
    (prin1 (abbrev-get sym :count))
    (insert ")\n")))

(defun abbrev--describe (sym)
  (when (symbol-value sym)
    (prin1 (symbol-name sym))
    (if (null (abbrev-get sym :system))
        (indent-to 15 1)
      (insert " (sys)")
      (indent-to 20 1))
    (prin1 (abbrev-get sym :count))
    (indent-to 20 1)
    (prin1 (symbol-value sym))
    (when (symbol-function sym)
      (indent-to 45 1)
      (prin1 (symbol-function sym)))
    (terpri)))

(defun insert-abbrev-table-description (name &optional readable)
  "Insert before point a full description of abbrev table named NAME.
NAME is a symbol whose value is an abbrev table.
If optional 2nd arg READABLE is non-nil, a human-readable description
is inserted.  Otherwise the description is an expression,
a call to `define-abbrev-table', which would
define the abbrev table NAME exactly as it is currently defined.

Abbrevs marked as \"system abbrevs\" are omitted."
  (let ((table (symbol-value name))
        (symbols ()))
    (mapatoms (lambda (sym) (if (symbol-value sym) (push sym symbols))) table)
    (setq symbols (sort symbols 'string-lessp))
    (let ((standard-output (current-buffer)))
      (if readable
	  (progn
	    (insert "(")
	    (prin1 name)
	    (insert ")\n\n")
	    (mapc 'abbrev--describe symbols)
	    (insert "\n\n"))
	(insert "(define-abbrev-table '")
	(prin1 name)
	(if (null symbols)
	    (insert " '())\n\n")
	  (insert "\n  '(\n")
	  (mapc 'abbrev--write symbols)
	  (insert "   ))\n\n")))
      nil)))

(put 'define-abbrev-table 'doc-string-elt 3)
(defun define-abbrev-table (tablename definitions
                                      &optional docstring &rest props)
  "Define TABLENAME (a symbol) as an abbrev table name.
Define abbrevs in it according to DEFINITIONS, which is a list of elements
of the form (ABBREVNAME EXPANSION ...) that are passed to `define-abbrev'.
PROPS is a property list to apply to the table.
Properties with special meaning:
- `:parents' contains a list of abbrev tables from which this table inherits
  abbreviations.
- `:case-fixed' non-nil means that abbreviations are looked up without
  case-folding, and the expansion is not capitalized/upcased.
- `:regexp' is a regular expression that specifies how to extract the
  name of the abbrev before point.  The submatch 1 is treated
  as the potential name of an abbrev.  If :regexp is nil, the default
  behavior uses `backward-word' and `forward-word' to extract the name
  of the abbrev, which can therefore only be a single word.
- `:enable-function' can be set to a function of no argument which returns
  non-nil if and only if the abbrevs in this table should be used for this
  instance of `expand-abbrev'."
  ;; We used to manually add the docstring, but we also want to record this
  ;; location as the definition of the variable (in load-history), so we may
  ;; as well just use `defvar'.
  (eval `(defvar ,tablename nil ,@(if (stringp docstring) (list docstring))))
  (let ((table (if (boundp tablename) (symbol-value tablename))))
    (unless table
      (setq table (make-abbrev-table))
      (set tablename table)
      (unless (memq tablename abbrev-table-name-list)
        (push tablename abbrev-table-name-list)))
    ;; We used to just pass them to `make-abbrev-table', but that fails
    ;; if the table was pre-existing as is the case if it was created by
    ;; loading the user's abbrev file.
    (while (consp props)
      (abbrev-table-put table (pop props) (pop props)))
    (dolist (elt definitions)
      (apply 'define-abbrev table elt))))

(defun abbrev-table-menu (table &optional prompt sortfun)
  "Return a menu that shows all abbrevs in TABLE.
Selecting an entry runs `abbrev-insert'.
PROMPT is the prompt to use for the keymap.
SORTFUN is passed to `sort' to change the default ordering."
  (unless sortfun (setq sortfun 'string-lessp))
  (let ((entries ()))
    (mapatoms (lambda (abbrev)
                (when (symbol-value abbrev)
                  (let ((name (symbol-name abbrev)))
                    (push `(,(intern name) menu-item ,name
                            (lambda () (interactive)
                              (abbrev-insert ',abbrev)))
                          entries))))
              table)
    (nconc (make-sparse-keymap prompt)
           (sort entries (lambda (x y)
                (funcall sortfun (nth 2 x) (nth 2 y)))))))

(provide 'abbrev)

;;; abbrev.el ends here
