;;; derived.el --- allow inheritance of major modes
;; (formerly mode-clone.el)

;; Copyright (C) 1993-1994, 1999, 2001-2012  Free Software Foundation, Inc.

;; Author: David Megginson (dmeggins@aix1.uottawa.ca)
;; Maintainer: FSF
;; Keywords: extensions
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

;; GNU Emacs is already, in a sense, object oriented -- each object
;; (buffer) belongs to a class (major mode), and that class defines
;; the relationship between messages (input events) and methods
;; (commands) by means of a keymap.
;;
;; The only thing missing is a good scheme of inheritance.  It is
;; possible to simulate a single level of inheritance with generous
;; use of hooks and a bit of work -- sgml-mode, for example, also runs
;; the hooks for text-mode, and keymaps can inherit from other keymaps
;; -- but generally, each major mode ends up reinventing the wheel.
;; Ideally, someone should redesign all of Emacs's major modes to
;; follow a more conventional object-oriented system: when defining a
;; new major mode, the user should need only to name the existing mode
;; it is most similar to, then list the (few) differences.
;;
;; In the mean time, this package offers most of the advantages of
;; full inheritance with the existing major modes.  The macro
;; `define-derived-mode' allows the user to make a variant of an existing
;; major mode, with its own keymap.  The new mode will inherit the key
;; bindings of its parent, and will, in fact, run its parent first
;; every time it is called.  For example, the commands
;;
;;  (define-derived-mode hypertext-mode text-mode "Hypertext"
;;    "Major mode for hypertext.\n\n\\{hypertext-mode-map}"
;;    (setq case-fold-search nil))
;;
;;  (define-key hypertext-mode-map [down-mouse-3] 'do-hyper-link)
;;
;; will create a function `hypertext-mode' with its own (sparse)
;; keymap `hypertext-mode-map.'  The command M-x hypertext-mode will
;; perform the following actions:
;;
;; - run the command (text-mode) to get its default setup
;; - replace the current keymap with 'hypertext-mode-map,' which will
;;   inherit from 'text-mode-map'.
;; - replace the current syntax table with
;;   'hypertext-mode-syntax-table', which will borrow its defaults
;;   from the current text-mode-syntax-table.
;; - replace the current abbrev table with
;;   'hypertext-mode-abbrev-table', which will borrow its defaults
;;   from the current text-mode-abbrev table
;; - change the mode line to read "Hypertext"
;; - assign the value 'hypertext-mode' to the 'major-mode' variable
;; - run the body of commands provided in the macro -- in this case,
;;   set the local variable `case-fold-search' to nil.
;;
;; The advantages of this system are threefold.  First, text mode is
;; untouched -- if you had added the new keystroke to `text-mode-map,'
;; possibly using hooks, you would have added it to all text buffers
;; -- here, it appears only in hypertext buffers, where it makes
;; sense.  Second, it is possible to build even further, and make
;; a derived mode from a derived mode.  The commands
;;
;;   (define-derived-mode html-mode hypertext-mode "HTML")
;;   [various key definitions]
;;
;; will add a new major mode for HTML with very little fuss.
;;
;; Note also the function `derived-mode-p' which can tell if the current
;; mode derives from another.  In a hypertext-mode, buffer, for example,
;; (derived-mode-p 'text-mode) would return non-nil.  This should always
;; be used in place of (eq major-mode 'text-mode).

;;; Code:

(eval-when-compile (require 'cl))

;;; PRIVATE: defsubst must be defined before they are first used

(defsubst derived-mode-hook-name (mode)
  "Construct a mode-hook name based on a MODE name."
  (intern (concat (symbol-name mode) "-hook")))

(defsubst derived-mode-map-name (mode)
  "Construct a map name based on a MODE name."
  (intern (concat (symbol-name mode) "-map")))

(defsubst derived-mode-syntax-table-name (mode)
  "Construct a syntax-table name based on a MODE name."
  (intern (concat (symbol-name mode) "-syntax-table")))

(defsubst derived-mode-abbrev-table-name (mode)
  "Construct an abbrev-table name based on a MODE name."
  (intern (concat (symbol-name mode) "-abbrev-table")))

;; PUBLIC: define a new major mode which inherits from an existing one.

;;;###autoload
(defmacro define-derived-mode (child parent name &optional docstring &rest body)
  "Create a new mode as a variant of an existing mode.

The arguments to this command are as follow:

CHILD:     the name of the command for the derived mode.
PARENT:    the name of the command for the parent mode (e.g. `text-mode')
           or nil if there is no parent.
NAME:      a string which will appear in the status line (e.g. \"Hypertext\")
DOCSTRING: an optional documentation string--if you do not supply one,
           the function will attempt to invent something useful.
BODY:      forms to execute just before running the
           hooks for the new mode.  Do not use `interactive' here.

BODY can start with a bunch of keyword arguments.  The following keyword
  arguments are currently understood:
:group GROUP
	Declare the customization group that corresponds to this mode.
	The command `customize-mode' uses this.
:syntax-table TABLE
	Use TABLE instead of the default (CHILD-syntax-table).
	A nil value means to simply use the same syntax-table as the parent.
:abbrev-table TABLE
	Use TABLE instead of the default (CHILD-abbrev-table).
	A nil value means to simply use the same abbrev-table as the parent.

Here is how you could define LaTeX-Thesis mode as a variant of LaTeX mode:

  (define-derived-mode LaTeX-thesis-mode LaTeX-mode \"LaTeX-Thesis\")

You could then make new key bindings for `LaTeX-thesis-mode-map'
without changing regular LaTeX mode.  In this example, BODY is empty,
and DOCSTRING is generated by default.

On a more complicated level, the following command uses `sgml-mode' as
the parent, and then sets the variable `case-fold-search' to nil:

  (define-derived-mode article-mode sgml-mode \"Article\"
    \"Major mode for editing technical articles.\"
    (setq case-fold-search nil))

Note that if the documentation string had been left out, it would have
been generated automatically, with a reference to the keymap.

The new mode runs the hook constructed by the function
`derived-mode-hook-name'.

See Info node `(elisp)Derived Modes' for more details."
  (declare (debug (&define name symbolp sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
	   (doc-string 4))

  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))

  (when (eq parent 'fundamental-mode) (setq parent nil))

  (let ((map (derived-mode-map-name child))
	(syntax (derived-mode-syntax-table-name child))
	(abbrev (derived-mode-abbrev-table-name child))
	(declare-abbrev t)
	(declare-syntax t)
	(hook (derived-mode-hook-name child))
	(group nil))

    ;; Process the keyword args.
    (while (keywordp (car body))
      (case (pop body)
	(:group (setq group (pop body)))
	(:abbrev-table (setq abbrev (pop body)) (setq declare-abbrev nil))
	(:syntax-table (setq syntax (pop body)) (setq declare-syntax nil))
	(t (pop body))))

    (setq docstring (derived-mode-make-docstring
		     parent child docstring syntax abbrev))

    `(progn
       (unless (get ',hook 'variable-documentation)
	 (put ',hook 'variable-documentation
	      (purecopy ,(format "Hook run when entering %s mode.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
		       name))))
       (unless (boundp ',map)
	 (put ',map 'definition-name ',child))
       (with-no-warnings (defvar ,map (make-sparse-keymap)))
       (unless (get ',map 'variable-documentation)
	 (put ',map 'variable-documentation
	      (purecopy ,(format "Keymap for `%s'." child))))
       ,(if declare-syntax
	    `(progn
	       (unless (boundp ',syntax)
		 (put ',syntax 'definition-name ',child))
	       (defvar ,syntax (make-syntax-table))
	       (unless (get ',syntax 'variable-documentation)
		 (put ',syntax 'variable-documentation
		      (purecopy ,(format "Syntax table for `%s'." child))))))
       ,(if declare-abbrev
	    `(progn
	       (put ',abbrev 'definition-name ',child)
	       (defvar ,abbrev
		 (progn (define-abbrev-table ',abbrev nil) ,abbrev))
	       (unless (get ',abbrev 'variable-documentation)
		 (put ',abbrev 'variable-documentation
		      (purecopy ,(format "Abbrev table for `%s'." child))))))
       (put ',child 'derived-mode-parent ',parent)
       ,(if group `(put ',child 'custom-mode-group ,group))

       (defun ,child ()
	 ,docstring
	 (interactive)
					; Run the parent.
	 (delay-mode-hooks

	  (,(or parent 'kill-all-local-variables))
					; Identify the child mode.
	  (setq major-mode (quote ,child))
	  (setq mode-name ,name)
					; Identify special modes.
	  ,(when parent
	     `(progn
		(if (get (quote ,parent) 'mode-class)
		    (put (quote ,child) 'mode-class
			 (get (quote ,parent) 'mode-class)))
					; Set up maps and tables.
		(unless (keymap-parent ,map)
                  ;; It would probably be better to set the keymap's parent
                  ;; at the toplevel rather than inside the mode function,
                  ;; but this is not easy for at least the following reasons:
                  ;; - the parent (and its keymap) may not yet be loaded.
                  ;; - the parent's keymap name may be called something else
                  ;;   than <parent>-mode-map.
		  (set-keymap-parent ,map (current-local-map)))
		,(when declare-syntax
		   `(let ((parent (char-table-parent ,syntax)))
		      (unless (and parent
				   (not (eq parent (standard-syntax-table))))
			(set-char-table-parent ,syntax (syntax-table)))))
                ,(when declare-abbrev
                   `(unless (or (abbrev-table-get ,abbrev :parents)
                                ;; This can happen if the major mode defines
                                ;; the abbrev-table to be its parent's.
                                (eq ,abbrev local-abbrev-table))
                      (abbrev-table-put ,abbrev :parents
                                        (list local-abbrev-table))))))
	  (use-local-map ,map)
	  ,(when syntax `(set-syntax-table ,syntax))
	  ,(when abbrev `(setq local-abbrev-table ,abbrev))
					; Splice in the body (if any).
	  ,@body
	  )
	 ;; Run the hooks, if any.
         (run-mode-hooks ',hook)))))

;; PUBLIC: find the ultimate class of a derived mode.

(defun derived-mode-class (mode)
  "Find the class of a major MODE.
A mode's class is the first ancestor which is NOT a derived mode.
Use the `derived-mode-parent' property of the symbol to trace backwards.
Since major-modes might all derive from `fundamental-mode', this function
is not very useful."
  (while (get mode 'derived-mode-parent)
    (setq mode (get mode 'derived-mode-parent)))
  mode)
(make-obsolete 'derived-mode-class 'derived-mode-p "22.1")


;;; PRIVATE

(defun derived-mode-make-docstring (parent child &optional
					   docstring syntax abbrev)
  "Construct a docstring for a new mode if none is provided."

  (let ((map (derived-mode-map-name child))
	(hook (derived-mode-hook-name child)))

    (unless (stringp docstring)
      ;; Use a default docstring.
      (setq docstring
	    (if (null parent)
		(format "Major-mode.
Uses keymap `%s', abbrev table `%s' and syntax-table `%s'." map abbrev syntax)
	      (format "Major mode derived from `%s' by `define-derived-mode'.
It inherits all of the parent's attributes, but has its own keymap,
abbrev table and syntax table:

  `%s', `%s' and `%s'

which more-or-less shadow %s's corresponding tables."
		      parent map abbrev syntax parent))))

    (unless (string-match (regexp-quote (symbol-name hook)) docstring)
      ;; Make sure the docstring mentions the mode's hook.
      (setq docstring
	    (concat docstring
		    (if (null parent)
			"\n\nThis mode "
		      (concat
		       "\n\nIn addition to any hooks its parent mode "
		       (if (string-match (regexp-quote (format "`%s'" parent))
					 docstring) nil
			 (format "`%s' " parent))
		       "might have run,\nthis mode "))
		    (format "runs the hook `%s'" hook)
		    ", as the final step\nduring initialization.")))

    (unless (string-match "\\\\[{[]" docstring)
      ;; And don't forget to put the mode's keymap.
      (setq docstring (concat docstring "\n\n\\{" (symbol-name map) "}")))

    docstring))


;;; OBSOLETE
;; The functions below are only provided for backward compatibility with
;; code byte-compiled with versions of derived.el prior to Emacs-21.

(defsubst derived-mode-setup-function-name (mode)
  "Construct a setup-function name based on a MODE name."
  (intern (concat (symbol-name mode) "-setup")))


;; Utility functions for defining a derived mode.

;;;###autoload
(defun derived-mode-init-mode-variables (mode)
  "Initialize variables for a new MODE.
Right now, if they don't already exist, set up a blank keymap, an
empty syntax table, and an empty abbrev table -- these will be merged
the first time the mode is used."

  (if (boundp (derived-mode-map-name mode))
      t
    (eval `(defvar ,(derived-mode-map-name mode)
	       (make-sparse-keymap)
	       ,(format "Keymap for %s." mode)))
    (put (derived-mode-map-name mode) 'derived-mode-unmerged t))

  (if (boundp (derived-mode-syntax-table-name mode))
      t
    (eval `(defvar ,(derived-mode-syntax-table-name mode)
	     ;; Make a syntax table which doesn't specify anything
	     ;; for any char.  Valid data will be merged in by
	     ;; derived-mode-merge-syntax-tables.
	     (make-char-table 'syntax-table nil)
	     ,(format "Syntax table for %s." mode)))
    (put (derived-mode-syntax-table-name mode) 'derived-mode-unmerged t))

  (if (boundp (derived-mode-abbrev-table-name mode))
      t
    (eval `(defvar ,(derived-mode-abbrev-table-name mode)
	     (progn
	       (define-abbrev-table (derived-mode-abbrev-table-name mode) nil)
	       (make-abbrev-table))
	     ,(format "Abbrev table for %s." mode)))))

;; Utility functions for running a derived mode.

(defun derived-mode-set-keymap (mode)
  "Set the keymap of the new MODE, maybe merging with the parent."
  (let* ((map-name (derived-mode-map-name mode))
	 (new-map (eval map-name))
	 (old-map (current-local-map)))
    (and old-map
	 (get map-name 'derived-mode-unmerged)
	 (derived-mode-merge-keymaps old-map new-map))
    (put map-name 'derived-mode-unmerged nil)
    (use-local-map new-map)))

(defun derived-mode-set-syntax-table (mode)
  "Set the syntax table of the new MODE, maybe merging with the parent."
  (let* ((table-name (derived-mode-syntax-table-name mode))
	 (old-table (syntax-table))
	 (new-table (eval table-name)))
    (if (get table-name 'derived-mode-unmerged)
	(derived-mode-merge-syntax-tables old-table new-table))
    (put table-name 'derived-mode-unmerged nil)
    (set-syntax-table new-table)))

(defun derived-mode-set-abbrev-table (mode)
  "Set the abbrev table for MODE if it exists.
Always merge its parent into it, since the merge is non-destructive."
  (let* ((table-name (derived-mode-abbrev-table-name mode))
	 (old-table local-abbrev-table)
	 (new-table (eval table-name)))
    (derived-mode-merge-abbrev-tables old-table new-table)
    (setq local-abbrev-table new-table)))

(defun derived-mode-run-hooks (mode)
   "Run the mode hook for MODE."
   (let ((hooks-name (derived-mode-hook-name mode)))
     (if (boundp hooks-name)
         (run-hooks hooks-name))))

;; Functions to merge maps and tables.

(defun derived-mode-merge-keymaps (old new)
  "Merge an OLD keymap into a NEW one.
The old keymap is set to be the last cdr of the new one, so that there will
be automatic inheritance."
  ;; ?? Can this just use `set-keymap-parent'?
  (let ((tail new))
    ;; Scan the NEW map for prefix keys.
    (while (consp tail)
      (and (consp (car tail))
	   (let* ((key (vector (car (car tail))))
		  (subnew (lookup-key new key))
		  (subold (lookup-key old key)))
	     ;; If KEY is a prefix key in both OLD and NEW, merge them.
	     (and (keymapp subnew) (keymapp subold)
		  (derived-mode-merge-keymaps subold subnew))))
      (and (vectorp (car tail))
	   ;; Search a vector of ASCII char bindings for prefix keys.
	   (let ((i (1- (length (car tail)))))
	     (while (>= i 0)
	       (let* ((key (vector i))
		      (subnew (lookup-key new key))
		      (subold (lookup-key old key)))
		 ;; If KEY is a prefix key in both OLD and NEW, merge them.
		 (and (keymapp subnew) (keymapp subold)
		      (derived-mode-merge-keymaps subold subnew)))
	       (setq i (1- i)))))
      (setq tail (cdr tail))))
  (setcdr (nthcdr (1- (length new)) new) old))

(defun derived-mode-merge-syntax-tables (old new)
  "Merge an OLD syntax table into a NEW one.
Where the new table already has an entry, nothing is copied from the old one."
  (set-char-table-parent new old))

;; Merge an old abbrev table into a new one.
;; This function requires internal knowledge of how abbrev tables work,
;; presuming that they are obarrays with the abbrev as the symbol, the expansion
;; as the value of the symbol, and the hook as the function definition.
(defun derived-mode-merge-abbrev-tables (old new)
  (if old
      (mapatoms
       (lambda (symbol)
	 (or (intern-soft (symbol-name symbol) new)
	     (define-abbrev new (symbol-name symbol)
	       (symbol-value symbol) (symbol-function symbol))))
       old)))

(provide 'derived)

;;; derived.el ends here
