;;; ses.el -- Simple Emacs Spreadsheet  -*- coding: utf-8 -*-

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@member.fsf.org>
;; Maintainer: Vincent Belaïche  <vincentb1@users.sourceforge.net>
;; Keywords: spreadsheet Dijkstra

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

;;; To-do list:

;; * split (catch 'cycle ...) call back into one or more functions
;; * Use $ or … for truncated fields
;; * Add command to make a range of columns be temporarily invisible.
;; * Allow paste of one cell to a range of cells -- copy formula to each.
;; * Do something about control characters & octal codes in cell print
;;   areas.  Use string-width?
;; * Input validation functions.  How specified?
;; * Faces (colors & styles) in print cells.
;; * Move a column by dragging its letter in the header line.
;; * Left-margin column for row number.
;; * Move a row by dragging its number in the left-margin.

;;; Cycle detection

;; Cycles used to be detected by stationarity of ses--deferred-recalc.  This was
;; working fine in most cases, however failed in some cases of several path
;; racing together.
;;
;; The current algorithm is based on Dijkstra's algorithm.  The cycle length is
;; stored in some cell property. In order not to reset in all cells such
;; property at each update, the cycle length is stored in this property along
;; with some update attempt id that is incremented at each update. The current
;; update id is ses--Dijkstra-attempt-nb. In case there is a cycle the cycle
;; length diverge to infinite so it will exceed ses--Dijkstra-weight-bound at
;; some point of time that allows detection. Otherwise it converges to the
;; longest path length in the update tree.


;;; Code:

(require 'unsafep)
(eval-when-compile (require 'cl))


;;----------------------------------------------------------------------------
;; User-customizable variables
;;----------------------------------------------------------------------------

(defgroup ses nil
  "Simple Emacs Spreadsheet."
  :group  'applications
  :prefix "ses-"
  :version "21.1")

(defcustom ses-initial-size '(1 . 1)
  "Initial size of a new spreadsheet, as a cons (NUMROWS . NUMCOLS)."
  :group 'ses
  :type '(cons (integer :tag "numrows") (integer :tag "numcols")))

(defcustom ses-initial-column-width 7
  "Initial width of columns in a new spreadsheet."
  :group 'ses
  :type '(integer :match (lambda (widget value) (> value 0))))

(defcustom ses-initial-default-printer "%.7g"
  "Initial default printer for a new spreadsheet."
  :group 'ses
  :type  '(choice string
		  (list :tag "Parenthesized string" string)
		  function))

(defcustom ses-after-entry-functions '(forward-char)
  "Things to do after entering a value into a cell.
An abnormal hook that usually runs a cursor-movement function.
Each function is called with ARG=1."
  :group 'ses
  :type 'hook
  :options '(forward-char backward-char next-line previous-line))

(defcustom ses-mode-hook nil
  "Hook functions to be run upon entering SES mode."
  :group 'ses
  :type 'hook)


;;----------------------------------------------------------------------------
;; Global variables and constants
;;----------------------------------------------------------------------------

(defvar ses-read-cell-history nil
  "List of formulas that have been typed in.")

(defvar ses-read-printer-history nil
  "List of printer functions that have been typed in.")

(easy-menu-define ses-header-line-menu nil
  "Context menu when mouse-3 is used on the header-line in an SES buffer."
  '("SES header row"
    ["Set current row" ses-set-header-row t]
    ["Unset row" ses-unset-header-row (> ses--header-row 0)]))

(defconst ses-mode-map
  (let ((keys `("\C-c\M-\C-l" ses-reconstruct-all
		"\C-c\C-l"    ses-recalculate-all
		"\C-c\C-n"    ses-renarrow-buffer
		"\C-c\C-c"    ses-recalculate-cell
		"\C-c\M-\C-s" ses-sort-column
		"\C-c\M-\C-h" ses-set-header-row
		"\C-c\C-t"    ses-truncate-cell
		"\C-c\C-j"    ses-jump
		"\C-c\C-p"    ses-read-default-printer
		"\M-\C-l"     ses-reprint-all
		[?\S-\C-l]    ses-reprint-all
		[header-line down-mouse-3] ,ses-header-line-menu
		[header-line mouse-2] ses-sort-column-click))
	(newmap (make-sparse-keymap)))
    (while keys
      (define-key (1value newmap) (car keys) (cadr keys))
      (setq keys (cddr keys)))
    newmap)
  "Local keymap for Simple Emacs Spreadsheet.")

(easy-menu-define ses-menu ses-mode-map
  "Menu bar menu for SES."
  '("SES"
    ["Insert row" ses-insert-row (ses-in-print-area)]
    ["Delete row" ses-delete-row (ses-in-print-area)]
    ["Insert column" ses-insert-column (ses-in-print-area)]
    ["Delete column" ses-delete-column (ses-in-print-area)]
    ["Set column printer" ses-read-column-printer t]
    ["Set column width" ses-set-column-width t]
    ["Set default printer" ses-read-default-printer t]
    ["Jump to cell" ses-jump t]
    ["Set cell printer" ses-read-cell-printer t]
    ["Recalculate cell" ses-recalculate-cell t]
    ["Truncate cell display" ses-truncate-cell t]
    ["Export values" ses-export-tsv t]
    ["Export formulas" ses-export-tsf t]))

(defconst ses-mode-edit-map
  (let ((keys '("\C-c\C-r"    ses-insert-range
		"\C-c\C-s"    ses-insert-ses-range
		[S-mouse-3]   ses-insert-range-click
		[C-S-mouse-3] ses-insert-ses-range-click
		"\M-\C-i"     lisp-complete-symbol))
	(newmap (make-sparse-keymap)))
    (set-keymap-parent newmap minibuffer-local-map)
    (while keys
      (define-key newmap (pop keys) (pop keys)))
    newmap)
  "Local keymap for SES minibuffer cell-editing.")

;Local keymap for SES print area
(defalias 'ses-mode-print-map
  (let ((keys '([backtab] backward-char
		[tab]     ses-forward-or-insert
		"\C-i"	  ses-forward-or-insert  ; Needed for ses-coverage.el?
		"\M-o"    ses-insert-column
		"\C-o"	  ses-insert-row
		"\C-m"    ses-edit-cell
		"\M-k"    ses-delete-column
		"\M-y"	  ses-yank-pop
		"\C-k"    ses-delete-row
		"\C-j"    ses-append-row-jump-first-column
		"\M-h"    ses-mark-row
		"\M-H"	  ses-mark-column
		"\C-d"	  ses-clear-cell-forward
		"\C-?"	  ses-clear-cell-backward
		"("       ses-read-cell
		"\""      ses-read-cell
		"'"       ses-read-symbol
		"="	  ses-edit-cell
		"c"	  ses-recalculate-cell
		"j"	  ses-jump
		"p"	  ses-read-cell-printer
		"t"	  ses-truncate-cell
		"w"	  ses-set-column-width
		"x"	  ses-export-keymap
		"\M-p"	  ses-read-column-printer))
	(repl '(;;We'll replace these wherever they appear in the keymap
		clipboard-kill-region ses-kill-override
		end-of-line	      ses-end-of-line
		kill-line	      ses-delete-row
		kill-region           ses-kill-override
		open-line	      ses-insert-row))
	(numeric "0123456789.-")
	(newmap (make-keymap)))
    ;;Get rid of printables
    (suppress-keymap newmap t)
    ;;These keys insert themselves as the beginning of a numeric value
    (dotimes (x (length numeric))
      (define-key newmap (substring numeric x (1+ x)) 'ses-read-cell))
    ;;Override these global functions wherever they're bound
    (while repl
      (substitute-key-definition (car repl) (cadr repl) newmap
				 (current-global-map))
      (setq repl (cddr repl)))
    ;;Apparently substitute-key-definition doesn't catch this?
    (define-key newmap [(menu-bar) edit cut] 'ses-kill-override)
    ;;Define our other local keys
    (while keys
      (define-key newmap (car keys) (cadr keys))
      (setq keys (cddr keys)))
    newmap))

;;Helptext for ses-mode wants keymap as variable, not function
(defconst ses-mode-print-map (symbol-function 'ses-mode-print-map))

;;Key map used for 'x' key.
(defalias 'ses-export-keymap
  (let ((map (make-sparse-keymap "SES export")))
    (define-key map "T" (cons " tab-formulas" 'ses-export-tsf))
    (define-key map "t" (cons " tab-values" 'ses-export-tsv))
    map))

(defconst ses-print-data-boundary "\n\014\n"
  "Marker string denoting the boundary between print area and data area.")

(defconst ses-initial-global-parameters
  "\n( ;Global parameters (these are read first)\n 2 ;SES file-format\n 1 ;numrows\n 1 ;numcols\n)\n\n"
  "Initial contents for the three-element list at the bottom of the data area.")

(defconst ses-initial-file-trailer
  ";; Local Variables:\n;; mode: ses\n;; End:\n"
  "Initial contents for the file-trailer area at the bottom of the file.")

(defconst ses-initial-file-contents
  (concat "       \n" ; One blank cell in print area.
	  ses-print-data-boundary
	  "(ses-cell A1 nil nil nil nil)\n" ; One blank cell in data area.
	  "\n" ; End-of-row terminator for the one row in data area.
	  "(ses-column-widths [7])\n"
	  "(ses-column-printers [nil])\n"
	  "(ses-default-printer \"%.7g\")\n"
	  "(ses-header-row 0)\n"
	  ses-initial-global-parameters
	  ses-initial-file-trailer)
  "The initial contents of an empty spreadsheet.")

(defconst ses-box-prop '(:box (:line-width 2 :style released-button))
  "Display properties to create a raised box for cells in the header line.")

(defconst ses-standard-printer-functions
  '(ses-center ses-center-span ses-dashfill ses-dashfill-span
    ses-tildefill-span)
  "List of print functions to be included in initial history of printer
functions.  None of these standard-printer functions is suitable for use as a
column printer or a global-default printer because they invoke the column or
default printer and then modify its output.")


;;----------------------------------------------------------------------------
;; Local variables and constants
;;----------------------------------------------------------------------------

(eval-and-compile
  (defconst ses-localvars
    '(ses--blank-line ses--cells ses--col-printers
      ses--col-widths ses--curcell ses--curcell-overlay
      ses--default-printer
      ses--deferred-narrow ses--deferred-recalc
      ses--deferred-write ses--file-format
      (ses--header-hscroll . -1) ; Flag for "initial recalc needed"
      ses--header-row ses--header-string ses--linewidth
      ses--numcols ses--numrows ses--symbolic-formulas
      ses--data-marker ses--params-marker (ses--Dijkstra-attempt-nb . 0)
      ses--Dijkstra-weight-bound
      ;; Global variables that we override
      mode-line-process next-line-add-newlines transient-mark-mode)
    "Buffer-local variables used by SES.")

(defun ses-set-localvars ()
  "Set buffer-local and initialize some SES variables."
  (dolist (x ses-localvars)
    (cond
     ((symbolp x)
      (set (make-local-variable x) nil))
     ((consp x)
      (set (make-local-variable (car x)) (cdr x)))
     (t (error "Unexpected elements `%S' in list `ses-localvars'" x))))))

(eval-when-compile			; silence compiler
  (ses-set-localvars))

;;; This variable is documented as being permitted in file-locals:
(put 'ses--symbolic-formulas 'safe-local-variable 'consp)

(defconst ses-paramlines-plist
  '(ses--col-widths  -5 ses--col-printers -4 ses--default-printer -3
    ses--header-row  -2 ses--file-format   1 ses--numrows          2
    ses--numcols      3)
  "Offsets from 'Global parameters' line to various parameter lines in the
data area of a spreadsheet.")


;;
;;  "Side-effect variables".  They are set in one function, altered in
;;  another as a side effect, then read back by the first, as a way of
;;  passing back more than one value.  These declarations are just to make
;;  the compiler happy, and to conform to standard Emacs-Lisp practice (I
;;  think the make-local-variable trick above is cleaner).
;;

(defvar ses-relocate-return nil
  "Set by `ses-relocate-formula' and `ses-relocate-range', read by
`ses-relocate-all'.  Set to 'delete if a cell-reference was deleted from a
formula--so the formula needs recalculation.  Set to 'range if the size of a
`ses-range' was changed--so both the formula's value and list of dependents
need to be recalculated.")

(defvar ses-call-printer-return nil
  "Set to t if last cell printer invoked by `ses-call-printer' requested
left-justification of the result.  Set to error-signal if `ses-call-printer'
encountered an error during printing.  Otherwise nil.")

(defvar ses-start-time nil
  "Time when current operation started.  Used by `ses-time-check' to decide
when to emit a progress message.")


;;----------------------------------------------------------------------------
;; Macros
;;----------------------------------------------------------------------------

(defmacro ses-get-cell (row col)
  "Return the cell structure that stores information about cell (ROW,COL)."
  `(aref (aref ses--cells ,row) ,col))

;; We might want to use defstruct here, but cells are explicitly used as
;; arrays in ses-set-cell, so we'd need to fix this first.  --Stef
(defsubst ses-make-cell (&optional symbol formula printer references
				   property-list)
  (vector symbol formula printer references property-list))

(defmacro ses-cell-symbol (row &optional col)
  "From a CELL or a pair (ROW,COL), get the symbol that names the local-variable holding its value.  (0,0) => A1."
  `(aref ,(if col `(ses-get-cell ,row ,col) row) 0))
(put 'ses-cell-symbol 'safe-function t)

(defmacro ses-cell-formula (row &optional col)
  "From a CELL or a pair (ROW,COL), get the function that computes its value."
  `(aref ,(if col `(ses-get-cell ,row ,col) row) 1))

(defmacro ses-cell-printer (row &optional col)
  "From a CELL or a pair (ROW,COL), get the function that prints its value."
  `(aref ,(if col `(ses-get-cell ,row ,col) row) 2))

(defmacro ses-cell-references (row &optional col)
  "From a CELL or a pair (ROW,COL), get the list of symbols for cells whose
functions refer to its value."
  `(aref ,(if col `(ses-get-cell ,row ,col) row) 3))

(defun ses-cell-property-get-fun (property-name cell)
  ;; To speed up property fetching, each time a property is found it is placed
  ;; in the first position.  This way, after the first get, the full property
  ;; list needs to be scanned only when the property does not exist for that
  ;; cell.
  (let* ((plist  (aref cell 4))
	 (ret (plist-member plist property-name)))
    (if ret
	;; Property was found.
	(let ((val (cadr ret)))
	  (if (eq ret plist)
	      ;; Property found is already in the first position, so just return
	      ;; its value.
	      val
	    ;; Property is not in the first position, the following will move it
	    ;; there before returning its value.
	    (let ((next (cddr ret)))
	      (if next
		  (progn
		    (setcdr ret (cdr next))
		    (setcar ret (car next)))
		(setcdr (last plist 1) nil)))
	    (aset cell 4
		  `(,property-name ,val ,@plist))
	    val)))))

(defmacro ses-cell-property-get (property-name row &optional col)
   "Get property named PROPERTY-NAME from a CELL or a pair (ROW,COL).

When COL is omitted, CELL=ROW is a cell object.  When COL is
present ROW and COL are the integer coordinates of the cell of
interest."
   (declare (debug t))
   `(ses-cell-property-get-fun
     ,property-name
     ,(if col `(ses-get-cell ,row ,col) row)))

(defun ses-cell-property-delq-fun (property-name cell)
  (let	((ret (plist-get (aref cell 4) property-name)))
    (if ret
      (setcdr ret (cddr ret)))))

(defun ses-cell-property-set-fun (property-name property-val cell)
  (let*	((plist  (aref cell 4))
	 (ret (plist-member plist property-name)))
    (if ret
	(setcar (cdr ret) property-val)
      (aset cell 4 `(,property-name ,property-val ,@plist)))))

(defmacro ses-cell-property-set (property-name property-value row &optional col)
   "From a CELL or a pair (ROW,COL), set the property value of
the corresponding cell with name PROPERTY-NAME to PROPERTY-VALUE."
   (if property-value
       `(ses-cell-property-set-fun ,property-name ,property-value
				   ,(if col `(ses-get-cell ,row ,col) row))
       `(ses-cell-property-delq-fun ,property-name
				    ,(if col `(ses-get-cell ,row ,col) row))))

(defun ses-cell-property-pop-fun (property-name cell)
  (let* ((plist  (aref cell 4))
	 (ret (plist-member plist property-name)))
    (if ret
	(prog1 (cadr ret)
	  (let ((next (cddr ret)))
	    (if next
		(progn
		  (setcdr ret (cdr next))
		  (setcar ret (car next)))
	      (if (eq plist ret)
		  (aset cell 4 nil)
		(setcdr (last plist 2) nil))))))))


(defmacro ses-cell-property-pop (property-name row &optional col)
   "From a CELL or a pair (ROW,COL), get and remove the property value of
the corresponding cell with name PROPERTY-NAME."
   `(ses-cell-property-pop-fun  ,property-name
				,(if col `(ses-get-cell ,row ,col) row)))

(defun ses-cell-property-get-handle-fun (property-name cell)
  (let*	((plist  (aref cell 4))
	 (ret (plist-member plist property-name)))
    (if ret
	(if (eq ret plist)
	    (cdr ret)
	  (let ((val (cadr ret))
		(next (cddr ret)))
	    (if next
		(progn
		  (setcdr ret (cdr next))
		  (setcar ret (car next)))
	      (setcdr (last plist 2) nil))
	    (setq ret (cons val plist))
	    (aset cell 4 (cons property-name ret))
	    ret))
      (setq ret (cons nil plist))
      (aset cell 4 (cons property-name ret))
      ret)))

(defmacro ses-cell-property-get-handle (property-name row &optional col)
   "From a CELL or a pair (ROW,COL), get a cons cell whose car is
the property value of the corresponding cell property with name
PROPERTY-NAME."
   `(ses-cell-property-get-handle-fun  ,property-name
				,(if col `(ses-get-cell ,row ,col) row)))


(defalias 'ses-cell-property-handle-car 'car)
(defalias 'ses-cell-property-handle-setcar 'setcar)

(defmacro ses-cell-value (row &optional col)
  "From a CELL or a pair (ROW,COL), get the current value for that cell."
  `(symbol-value (ses-cell-symbol ,row ,col)))

(defmacro ses-col-width (col)
  "Return the width for column COL."
  `(aref ses--col-widths ,col))

(defmacro ses-col-printer (col)
  "Return the default printer for column COL."
  `(aref ses--col-printers ,col))

(defmacro ses-sym-rowcol (sym)
  "From a cell-symbol SYM, gets the cons (row . col).  A1 => (0 . 0).
Result is nil if SYM is not a symbol that names a cell."
  `(and (symbolp ,sym) (get ,sym 'ses-cell)))

(defmacro ses-cell (sym value formula printer references)
  "Load a cell SYM from the spreadsheet file.  Does not recompute VALUE from
FORMULA, does not reprint using PRINTER, does not check REFERENCES.  This is a
macro to prevent propagate-on-load viruses.  Safety-checking for FORMULA and
PRINTER are deferred until first use."
  (let ((rowcol (ses-sym-rowcol sym)))
    (ses-formula-record formula)
    (ses-printer-record printer)
    (or (atom formula)
	(eq safe-functions t)
	(setq formula `(ses-safe-formula ,formula)))
    (or (not printer)
	(stringp printer)
	(eq safe-functions t)
	(setq printer `(ses-safe-printer ,printer)))
    (aset (aref ses--cells (car rowcol))
	  (cdr rowcol)
	  (ses-make-cell sym formula printer references)))
  (set sym value)
  sym)

(defmacro ses-column-widths (widths)
  "Load the vector of column widths from the spreadsheet file.  This is a
macro to prevent propagate-on-load viruses."
  (or (and (vectorp widths) (= (length widths) ses--numcols))
      (error "Bad column-width vector"))
  ;;To save time later, we also calculate the total width of each line in the
  ;;print area (excluding the terminating newline)
  (setq ses--col-widths widths
	ses--linewidth  (apply '+ -1 (mapcar '1+ widths))
	ses--blank-line (concat (make-string ses--linewidth ?\s) "\n"))
  t)

(defmacro ses-column-printers (printers)
  "Load the vector of column printers from the spreadsheet file and checks
them for safety.  This is a macro to prevent propagate-on-load viruses."
  (or (and (vectorp printers) (= (length printers) ses--numcols))
      (error "Bad column-printers vector"))
  (dotimes (x ses--numcols)
    (aset printers x (ses-safe-printer (aref printers x))))
  (setq ses--col-printers printers)
  (mapc 'ses-printer-record printers)
  t)

(defmacro ses-default-printer (def)
  "Load the global default printer from the spreadsheet file and checks it
for safety.  This is a macro to prevent propagate-on-load viruses."
  (setq ses--default-printer (ses-safe-printer def))
  (ses-printer-record def)
  t)

(defmacro ses-header-row (row)
  "Load the header row from the spreadsheet file and checks it
for safety.  This is a macro to prevent propagate-on-load viruses."
  (or (and (wholenump row) (or (zerop ses--numrows) (< row ses--numrows)))
      (error "Bad header-row"))
  (setq ses--header-row row)
  t)

(defmacro ses-dorange (curcell &rest body)
  "Execute BODY repeatedly, with the variables `row' and `col' set to each
cell in the range specified by CURCELL.  The range is available in the
variables `minrow', `maxrow', `mincol', and `maxcol'."
  (declare (indent defun) (debug (form body)))
  (let ((cur (make-symbol "cur"))
	(min (make-symbol "min"))
	(max (make-symbol "max"))
	(r   (make-symbol "r"))
	(c   (make-symbol "c")))
    `(let* ((,cur ,curcell)
	    (,min (ses-sym-rowcol (if (consp ,cur) (car ,cur) ,cur)))
	    (,max (ses-sym-rowcol (if (consp ,cur) (cdr ,cur) ,cur))))
       (let ((minrow (car ,min))
	     (maxrow (car ,max))
	     (mincol (cdr ,min))
	     (maxcol (cdr ,max))
	     row col)
	 (if (or (> minrow maxrow) (> mincol maxcol))
	     (error "Empty range"))
	 (dotimes (,r (- maxrow minrow -1))
	   (setq row (+ ,r minrow))
	   (dotimes (,c (- maxcol mincol -1))
	     (setq col (+ ,c mincol))
	     ,@body))))))

;;Support for coverage testing.
(defmacro 1value (form)
  "For code-coverage testing, indicate that FORM is expected to always have
the same value."
  form)
(defmacro noreturn (form)
  "For code-coverage testing, indicate that FORM will always signal an error."
  form)


;;----------------------------------------------------------------------------
;; Utility functions
;;----------------------------------------------------------------------------

(defun ses-vector-insert (array idx new)
  "Create a new vector which is one larger than ARRAY and has NEW inserted
before element IDX."
  (let* ((len    (length array))
	 (result (make-vector (1+ len) new)))
    (dotimes (x len)
      (aset result
	    (if (< x idx) x (1+ x))
	    (aref array x)))
    result))

;;Allow ARRAY to be a symbol for use in buffer-undo-list
(defun ses-vector-delete (array idx count)
  "Create a new vector which is a copy of ARRAY with COUNT objects removed
starting at element IDX.  ARRAY is either a vector or a symbol whose value
is a vector--if a symbol, the new vector is assigned as the symbol's value."
  (let* ((a      (if (arrayp array) array (symbol-value array)))
	 (len    (- (length a) count))
	 (result (make-vector len nil)))
    (dotimes (x len)
      (aset result x (aref a (if (< x idx) x (+ x count)))))
    (if (symbolp array)
	(set array result))
    result))

(defun ses-delete-line (count)
  "Like `kill-line', but no kill ring."
  (let ((pos (point)))
    (forward-line count)
    (delete-region pos (point))))

(defun ses-printer-validate (printer)
  "Signal an error if PRINTER is not a valid SES cell printer."
  (or (not printer)
      (stringp printer)
      (functionp printer)
      (and (stringp (car-safe printer)) (not (cdr printer)))
      (error "Invalid printer function"))
  printer)

(defun ses-printer-record (printer)
  "Add PRINTER to `ses-read-printer-history' if not already there, after first
checking that it is a valid printer function."
  (ses-printer-validate printer)
  ;;To speed things up, we avoid calling prin1 for the very common "nil" case.
  (if printer
      (add-to-list 'ses-read-printer-history (prin1-to-string printer))))

(defun ses-formula-record (formula)
  "If FORMULA is of the form 'symbol, add it to the list of symbolic formulas
for this spreadsheet."
  (when (and (eq (car-safe formula) 'quote)
	     (symbolp (cadr formula)))
    (add-to-list 'ses--symbolic-formulas
		 (list (symbol-name (cadr formula))))))

(defun ses-column-letter (col)
  "Return the alphabetic name of column number COL.
0-25 become A-Z; 26-701 become AA-ZZ, and so on."
  (let ((units (char-to-string (+ ?A (% col 26)))))
    (if (< col 26)
	units
      (concat (ses-column-letter (1- (/ col 26))) units))))

(defun ses-create-cell-symbol (row col)
  "Produce a symbol that names the cell (ROW,COL).  (0,0) => 'A1."
  (intern (concat (ses-column-letter col) (number-to-string (1+ row)))))

(defun ses-create-cell-variable-range (minrow maxrow mincol maxcol)
  "Create buffer-local variables for cells.  This is undoable."
  (push `(apply ses-destroy-cell-variable-range ,minrow ,maxrow ,mincol ,maxcol)
	buffer-undo-list)
  (let (sym xrow xcol)
    (dotimes (row (1+ (- maxrow minrow)))
      (dotimes (col (1+ (- maxcol mincol)))
	(setq xrow (+ row minrow)
	      xcol (+ col mincol)
	      sym  (ses-create-cell-symbol xrow xcol))
	(put sym 'ses-cell (cons xrow xcol))
	(make-local-variable sym)))))

;; We do not delete the ses-cell properties for the cell-variables, in
;; case a formula that refers to this cell is in the kill-ring and is
;; later pasted back in.
(defun ses-destroy-cell-variable-range (minrow maxrow mincol maxcol)
  "Destroy buffer-local variables for cells.  This is undoable."
  (let (sym)
    (dotimes (row (1+ (- maxrow minrow)))
      (dotimes (col (1+ (- maxcol mincol)))
	(setq sym (ses-create-cell-symbol (+ row minrow) (+ col mincol)))
	(if (boundp sym)
	    (push `(apply ses-set-with-undo ,sym ,(symbol-value sym))
		  buffer-undo-list))
	(kill-local-variable sym))))
  (push `(apply ses-create-cell-variable-range ,minrow ,maxrow ,mincol ,maxcol)
	buffer-undo-list))

(defun ses-reset-header-string ()
  "Flag the header string for update.  Upon undo, the header string will be
updated again."
  (push '(apply ses-reset-header-string) buffer-undo-list)
  (setq ses--header-hscroll -1))

;;Split this code off into a function to avoid coverage-testing difficulties
(defun ses-time-check (format arg)
  "If `ses-start-time' is more than a second ago, call `message' with FORMAT
and (eval ARG) and reset `ses-start-time' to the current time."
  (when (> (- (float-time) ses-start-time) 1.0)
    (message format (eval arg))
    (setq ses-start-time (float-time)))
  nil)


;;----------------------------------------------------------------------------
;; The cells
;;----------------------------------------------------------------------------

(defun ses-set-cell (row col field val)
  "Install VAL as the contents for field FIELD (named by a quoted symbol) of
cell (ROW,COL).  This is undoable.  The cell's data will be updated through
`post-command-hook'."
  (let ((cell (ses-get-cell row col))
	(elt  (plist-get '(value t symbol 0 formula 1 printer 2 references 3)
			 field))
	change)
    (or elt (signal 'args-out-of-range nil))
    (setq change (if (eq elt t)
		     (ses-set-with-undo (ses-cell-symbol cell) val)
		   (ses-aset-with-undo cell elt val)))
    (if change
	(add-to-list 'ses--deferred-write (cons row col))))
  nil) ; Make coverage-tester happy.

(defun ses-cell-set-formula (row col formula)
  "Store a new formula for (ROW . COL) and enqueue the cell for
recalculation via `post-command-hook'.  Updates the reference lists for the
cells that this cell refers to.  Does not update cell value or reprint the
cell.  To avoid inconsistencies, this function is not interruptible, which
means Emacs will crash if FORMULA contains a circular list."
  (let* ((cell (ses-get-cell row col))
	 (old  (ses-cell-formula cell)))
    (let ((sym    (ses-cell-symbol cell))
	  (oldref (ses-formula-references old))
	  (newref (ses-formula-references formula))
	  (inhibit-quit t)
	  x xrow xcol)
      (add-to-list 'ses--deferred-recalc sym)
      ;;Delete old references from this cell.  Skip the ones that are also
      ;;in the new list.
      (dolist (ref oldref)
	(unless (memq ref newref)
	  (setq x    (ses-sym-rowcol ref)
		xrow (car x)
		xcol (cdr x))
	  (ses-set-cell xrow xcol 'references
			(delq sym (ses-cell-references xrow xcol)))))
      ;;Add new ones.  Skip ones left over from old list
      (dolist (ref newref)
	(setq x    (ses-sym-rowcol ref)
	      xrow (car x)
	      xcol (cdr x)
	      x    (ses-cell-references xrow xcol))
	(or (memq sym x)
	    (ses-set-cell xrow xcol 'references (cons sym x))))
      (ses-formula-record formula)
      (ses-set-cell row col 'formula formula))))


(defun ses-repair-cell-reference-all ()
  "Repair cell reference and warn if there was some reference corruption."
  (interactive "*")
  (let (errors)
    ;; Step 1, reset  :ses-repair-reference cell property in the whole sheet.
    (dotimes (row ses--numrows)
      (dotimes (col ses--numcols)
	(let ((references  (ses-cell-property-pop :ses-repair-reference
						  row col)))
	(when references
	  (push (list
		 (ses-cell-symbol row col)
		 :corrupt-property
		 references) errors)))))

    ;; Step 2, build new.
    (dotimes (row ses--numrows)
      (dotimes (col ses--numcols)
	(let* ((cell (ses-get-cell row col))
	       (sym (ses-cell-symbol cell))
	       (formula (ses-cell-formula cell))
	       (new-ref (ses-formula-references formula)))
	  (dolist (ref new-ref)
	    (let* ((rowcol (ses-sym-rowcol ref))
		  (h (ses-cell-property-get-handle :ses-repair-reference
						  (car rowcol) (cdr rowcol))))
	      (unless (memq ref (ses-cell-property-handle-car h))
		(ses-cell-property-handle-setcar
		 h
		 (cons sym
		       (ses-cell-property-handle-car h)))))))))

    ;; Step 3, overwrite with check.
    (dotimes (row ses--numrows)
      (dotimes (col ses--numcols)
	(let* ((cell (ses-get-cell row col))
	       (irrelevant (ses-cell-references cell))
	       (new-ref (ses-cell-property-pop  :ses-repair-reference cell))
	       missing)
	  (dolist (ref new-ref)
	    (if (memq ref irrelevant)
		(setq irrelevant (delq ref irrelevant))
	      (push ref missing)))
	  (ses-set-cell row col 'references new-ref)
	  (when (or missing irrelevant)
	    (push `( ,(ses-cell-symbol cell)
		     ,@(and missing (list :missing missing))
		     ,@(and irrelevant  (list :irrelevant irrelevant)))
		  errors)))))
    (if errors
      (warn "----------------------------------------------------------------
Some references were corrupted.

The following is a list where each element ELT is such
that (car ELT) is the reference of cell CELL with corruption,
and (cdr ELT) is a property list where

* property `:corrupt-property' means that
  property `:ses-repair-reference' of cell CELL was initially non
  nil,

* property `:missing' is a list of missing references

* property `:irrelevant' is a list of non needed references

%S" errors)
      (message "No reference corruption found"))))

(defun ses-calculate-cell (row col force)
  "Calculate and print the value for cell (ROW,COL) using the cell's formula
function and print functions, if any.  Result is nil for normal operation, or
the error signal if the formula or print function failed.  The old value is
left unchanged if it was *skip* and the new value is nil.
  Any cells that depend on this cell are queued for update after the end of
processing for the current keystroke, unless the new value is the same as
the old and FORCE is nil."
  (let ((cell (ses-get-cell row col))
	cycle-error formula-error printer-error)
    (let ((oldval  (ses-cell-value   cell))
	  (formula (ses-cell-formula cell))
	  newval
	  this-cell-Dijkstra-attempt-h
	  this-cell-Dijkstra-attempt
	  this-cell-Dijkstra-attempt+1
	  ref-cell-Dijkstra-attempt-h
	  ref-cell-Dijkstra-attempt
	  ref-rowcol)
      (when (eq (car-safe formula) 'ses-safe-formula)
	(setq formula (ses-safe-formula (cadr formula)))
	(ses-set-cell row col 'formula formula))
      (condition-case sig
	  (setq newval (eval formula))
	(error
	 ;; Variable `sig' can't be nil.
	 (nconc sig (list (ses-cell-symbol cell)))
	 (setq formula-error sig
	       newval        '*error*)))
      (if (and (not newval) (eq oldval '*skip*))
	  ;; Don't lose the *skip* --- previous field spans this one.
	  (setq newval '*skip*))
      (catch 'cycle
	(when (or force (not (eq newval oldval)))
	  (add-to-list 'ses--deferred-write (cons row col)) ; In case force=t.
	  (setq this-cell-Dijkstra-attempt-h
		(ses-cell-property-get-handle :ses-Dijkstra-attempt cell);
		this-cell-Dijkstra-attempt
		(ses-cell-property-handle-car this-cell-Dijkstra-attempt-h))
	  (if (null this-cell-Dijkstra-attempt)
	      (ses-cell-property-handle-setcar
	       this-cell-Dijkstra-attempt-h
	       (setq this-cell-Dijkstra-attempt
		     (cons ses--Dijkstra-attempt-nb 0)))
	    (unless (= ses--Dijkstra-attempt-nb
		       (car this-cell-Dijkstra-attempt))
		(setcar this-cell-Dijkstra-attempt ses--Dijkstra-attempt-nb)
		(setcdr this-cell-Dijkstra-attempt 0)))
	  (setq this-cell-Dijkstra-attempt+1
		(1+ (cdr this-cell-Dijkstra-attempt)))
	  (ses-set-cell row col 'value newval)
	  (dolist (ref (ses-cell-references cell))
	    (add-to-list 'ses--deferred-recalc ref)
	    (setq ref-rowcol (ses-sym-rowcol ref)
		  ref-cell-Dijkstra-attempt-h
		  (ses-cell-property-get-handle
		   :ses-Dijkstra-attempt
		   (car ref-rowcol) (cdr ref-rowcol))
		  ref-cell-Dijkstra-attempt
		  (ses-cell-property-handle-car ref-cell-Dijkstra-attempt-h))

	    (if (null ref-cell-Dijkstra-attempt)
	      (ses-cell-property-handle-setcar
	       ref-cell-Dijkstra-attempt-h
	       (setq ref-cell-Dijkstra-attempt
		      (cons ses--Dijkstra-attempt-nb
			    this-cell-Dijkstra-attempt+1)))
	      (if (= (car ref-cell-Dijkstra-attempt) ses--Dijkstra-attempt-nb)
		  (setcdr ref-cell-Dijkstra-attempt
			  (max (cdr ref-cell-Dijkstra-attempt)
			       this-cell-Dijkstra-attempt+1))
		(setcar ref-cell-Dijkstra-attempt ses--Dijkstra-attempt-nb)
		(setcdr ref-cell-Dijkstra-attempt
			this-cell-Dijkstra-attempt+1)))

	    (when (> this-cell-Dijkstra-attempt+1 ses--Dijkstra-weight-bound)
	      ;; Update print of this cell.
	      (throw 'cycle (setq formula-error
				  `(error ,(format "Found cycle on cells %S"
						   (ses-cell-symbol cell)))
				  cycle-error formula-error)))))))
    (setq printer-error (ses-print-cell row col))
    (or
     (and cycle-error
	  (error (error-message-string cycle-error)))
     formula-error printer-error)))

(defun ses-clear-cell (row col)
  "Delete formula and printer for cell (ROW,COL)."
  (ses-set-cell row col 'printer nil)
  (ses-cell-set-formula row col nil))

(defcustom ses-self-reference-early-detection nil
  "True if cycle detection is early for cells that refer to themselves."
  :version "24.1"
  :type 'boolean
  :group 'ses)

(defun ses-update-cells (list &optional force)
  "Recalculate cells in LIST, checking for dependency loops.  Prints
progress messages every second.  Dependent cells are not recalculated
if the cell's value is unchanged and FORCE is nil."
  (let ((ses--deferred-recalc list)
	(nextlist             list)
	(pos		      (point))
	curlist prevlist this-sym this-rowcol formula)
    (with-temp-message " "
      (while ses--deferred-recalc
	;; In each loop, recalculate cells that refer only to other cells that
	;; have already been recalculated or aren't in the recalculation region.
	;; Repeat until all cells have been processed or until the set of cells
	;; being worked on stops changing.
	(if prevlist
	    (message "Recalculating... (%d cells left)"
		     (length ses--deferred-recalc)))
	(setq curlist              ses--deferred-recalc
	      ses--deferred-recalc nil
	      prevlist             nextlist)
	(while curlist
	  ;; this-sym has to be popped from curlist *BEFORE* the check, and not
	  ;; after because of the case of cells referring to themselves.
	  (setq this-sym   (pop curlist)
		this-rowcol (ses-sym-rowcol this-sym)
		formula     (ses-cell-formula (car this-rowcol)
					      (cdr this-rowcol)))
	  (or (catch 'ref
		(dolist (ref (ses-formula-references formula))
		  (if (and ses-self-reference-early-detection (eq ref this-sym))
		      (error "Cycle found: cell %S is self-referring" this-sym)
		    (when (or (memq ref curlist)
			      (memq ref ses--deferred-recalc))
		      ;; This cell refers to another that isn't done yet
		      (add-to-list 'ses--deferred-recalc this-sym)
		      (throw 'ref t)))))
	      ;; ses-update-cells is called from post-command-hook, so
	      ;; inhibit-quit is implicitly bound to t.
	      (when quit-flag
		;; Abort the recalculation.  User will probably undo now.
		(error "Quit"))
	      (ses-calculate-cell (car this-rowcol) (cdr this-rowcol) force)))
	(dolist (ref ses--deferred-recalc)
	  (add-to-list 'nextlist ref)))
      (when ses--deferred-recalc
	;; Just couldn't finish these.
	(dolist (x ses--deferred-recalc)
	  (let ((this-rowcol (ses-sym-rowcol x)))
	    (ses-set-cell (car this-rowcol) (cdr this-rowcol) 'value '*error*)
	    (1value (ses-print-cell (car this-rowcol) (cdr this-rowcol)))))
	(error "Circular references: %s" ses--deferred-recalc))
      (message " "))
    ;; Can't use save-excursion here: if the cell under point is updated,
    ;; save-excursion's marker will move past the cell.
    (goto-char pos)))


;;----------------------------------------------------------------------------
;; The print area
;;----------------------------------------------------------------------------

(defun ses-in-print-area ()
  "Return t if point is in print area of spreadsheet."
  (<= (point) ses--data-marker))

;; We turn off point-motion-hooks and explicitly position the cursor, in case
;; the intangible properties have gotten screwed up (e.g., when ses-goto-print
;; is called during a recursive ses-print-cell).
(defun ses-goto-print (row col)
  "Move point to print area for cell (ROW,COL)."
  (let ((inhibit-point-motion-hooks t)
	(n 0))
    (goto-char (point-min))
    (forward-line row)
    ;; Calculate column position.
    (dotimes (c col)
      (setq n (+ n (ses-col-width c) 1)))
    ;; Move to the position.
    (and (> n (move-to-column n))
	 (eolp)
	 ;; Move point to the bol of next line (for TAB at the last cell).
	 (forward-char))))

(defun ses-set-curcell ()
  "Set `ses--curcell' to the current cell symbol, or a cons (BEG,END) for a
region, or nil if cursor is not at a cell."
  (if (or (not mark-active)
	  deactivate-mark
	  (= (region-beginning) (region-end)))
      ;; Single cell.
      (setq ses--curcell (get-text-property (point) 'intangible))
    ;; Range.
    (let ((bcell (get-text-property (region-beginning) 'intangible))
	  (ecell (get-text-property (1- (region-end))  'intangible)))
      (when (= (region-end) ses--data-marker)
	;; Correct for overflow.
	(setq ecell (get-text-property (- (region-end) 2)  'intangible)))
      (setq ses--curcell (if (and bcell ecell)
			     (cons bcell ecell)
			   nil))))
  nil)

(defun ses-check-curcell (&rest args)
  "Signal an error if `ses--curcell' is inappropriate.
The end marker is appropriate if some argument is 'end.
A range is appropriate if some argument is 'range.
A single cell is appropriate unless some argument is 'needrange."
  (if (eq ses--curcell t)
      ;; curcell recalculation was postponed, but user typed ahead.
      (ses-set-curcell))
  (cond
   ((not ses--curcell)
    (or (memq 'end args)
	(error "Not at cell")))
   ((consp ses--curcell)
    (or (memq 'range args)
	(memq 'needrange args)
	(error "Can't use a range")))
   ((memq 'needrange args)
    (error "Need a range"))))

(defun ses-print-cell (row col)
  "Format and print the value of cell (ROW,COL) to the print area.
Use the cell's printer function.  If the cell's new print form is too wide,
it will spill over into the following cell, but will not run off the end of the
row or overwrite the next non-nil field.  Result is nil for normal operation,
or the error signal if the printer function failed and the cell was formatted
with \"%s\".  If the cell's value is *skip*, nothing is printed because the
preceding cell has spilled over."
  (catch 'ses-print-cell
    (let* ((cell    (ses-get-cell row col))
	   (value   (ses-cell-value cell))
	   (printer (ses-cell-printer cell))
	   (maxcol  (1+ col))
	   text sig startpos x)
      ;; Create the string to print.
      (cond
       ((eq value '*skip*)
	;; Don't print anything.
	(throw 'ses-print-cell nil))
       ((eq value '*error*)
	(setq text (make-string (ses-col-width col) ?#)))
       (t
	;; Deferred safety-check on printer.
	(if (eq (car-safe printer) 'ses-safe-printer)
	    (ses-set-cell row col 'printer
			  (setq printer (ses-safe-printer (cadr printer)))))
	;; Print the value.
	(setq text (ses-call-printer (or printer
					 (ses-col-printer col)
					 ses--default-printer)
				     value))
	(if (consp ses-call-printer-return)
	    ;; Printer returned an error.
	    (setq sig ses-call-printer-return))))
      ;; Adjust print width to match column width.
      (let ((width (ses-col-width col))
	    (len   (string-width text)))
	(cond
	 ((< len width)
	  ;; Fill field to length with spaces.
	  (setq len  (make-string (- width len) ?\s)
		text (if (eq ses-call-printer-return t)
			 (concat text len)
		       (concat len text))))
	 ((> len width)
	  ;; Spill over into following cells, if possible.
	  (let ((maxwidth width))
	    (while (and (> len maxwidth)
			(< maxcol ses--numcols)
			(or (not (setq x (ses-cell-value row maxcol)))
			    (eq x '*skip*)))
	      (unless x
		;; Set this cell to '*skip* so it won't overwrite our spillover.
		(ses-set-cell row maxcol 'value '*skip*))
	      (setq maxwidth (+ maxwidth (ses-col-width maxcol) 1)
		    maxcol   (1+ maxcol)))
	    (if (<= len maxwidth)
		;; Fill to complete width of all the fields spanned.
		(setq text (concat text (make-string (- maxwidth len) ?\s)))
	      ;; Not enough room to end of line or next non-nil field.  Truncate
	      ;; if string or decimal; otherwise fill with error indicator.
	      (setq sig `(error "Too wide" ,text))
	      (cond
	       ((stringp value)
		(setq text (truncate-string-to-width text maxwidth 0 ?\s)))
	       ((and (numberp value)
		     (string-match "\\.[0-9]+" text)
		     (>= 0 (setq width
				 (- len maxwidth
				    (- (match-end 0) (match-beginning 0))))))
		;; Turn 6.6666666666e+49 into 6.66e+49.  Rounding is too hard!
		(setq text (concat (substring text
					      0
					      (- (match-beginning 0) width))
				   (substring text (match-end 0)))))
	       (t
		(setq text (make-string maxwidth ?#)))))))))
      ;; Substitute question marks for tabs and newlines.  Newlines are used as
      ;; row-separators; tabs could confuse the reimport logic.
      (setq text (replace-regexp-in-string "[\t\n]" "?" text))
      (ses-goto-print row col)
      (setq startpos (point))
      ;; Install the printed result.  This is not interruptible.
      (let ((inhibit-read-only t)
	    (inhibit-quit      t))
	(let ((inhibit-point-motion-hooks t))
	  (delete-region (point) (progn
				   (move-to-column (+ (current-column)
						      (string-width text)))
				   (1+ (point)))))
	;; We use concat instead of inserting separate strings in order to
	;; reduce the number of cells in the undo list.
	(setq x (concat text (if (< maxcol ses--numcols) " " "\n")))
	;; We use set-text-properties to prevent a wacky print function from
	;; inserting rogue properties, and to ensure that the keymap property is
	;; inherited (is it a bug that only unpropertized strings actually
	;; inherit from surrounding text?)
	(set-text-properties 0 (length x) nil x)
	(insert-and-inherit x)
	(put-text-property startpos (point) 'intangible
			   (ses-cell-symbol cell))
	(when (and (zerop row) (zerop col))
	  ;; Reconstruct special beginning-of-buffer attributes.
	  (put-text-property (point-min) (point) 'keymap 'ses-mode-print-map)
	  (put-text-property (point-min) (point) 'read-only 'ses)
	  (put-text-property (point-min) (1+ (point-min)) 'front-sticky t)))
      (if (= row (1- ses--header-row))
	  ;; This line is part of the header --- force recalc.
	  (ses-reset-header-string))
      ;; If this cell (or a preceding one on the line) previously spilled over
      ;; and has gotten shorter, redraw following cells on line recursively.
      (when (and (< maxcol ses--numcols)
		 (eq (ses-cell-value row maxcol) '*skip*))
	(ses-set-cell row maxcol 'value nil)
	(ses-print-cell row maxcol))
      ;; Return to start of cell.
      (goto-char startpos)
      sig)))

(defun ses-call-printer (printer &optional value)
  "Invoke PRINTER (a string or parenthesized string or function-symbol or
lambda of one argument) on VALUE.  Result is the printed cell as a string.
The variable `ses-call-printer-return' is set to t if the printer used
parenthesis to request left-justification, or the error-signal if the
printer signaled one (and \"%s\" is used as the default printer), else nil."
  (setq ses-call-printer-return nil)
  (condition-case signal
      (cond
       ((stringp printer)
	(if value
	    (format printer value)
	  ""))
       ((stringp (car-safe printer))
	(setq ses-call-printer-return t)
	(if value
	    (format (car printer) value)
	  ""))
       (t
	(setq value (funcall printer (or value "")))
	(if (stringp value)
	    value
	  (or (stringp (car-safe value))
	      (error "Printer should return \"string\" or (\"string\")"))
	  (setq ses-call-printer-return t)
	  (car value))))
    (error
     (setq ses-call-printer-return signal)
     (prin1-to-string value t))))

(defun ses-adjust-print-width (col change)
  "Insert CHANGE spaces in front of column COL, or at end of line if
COL=NUMCOLS.  Deletes characters if CHANGE < 0.  Caller should bind
`inhibit-quit' to t."
  (let ((inhibit-read-only t)
	(blank  (if (> change 0) (make-string change ?\s)))
	(at-end (= col ses--numcols)))
    (ses-set-with-undo 'ses--linewidth (+ ses--linewidth change))
    ;; ses-set-with-undo always returns t for strings.
    (1value (ses-set-with-undo 'ses--blank-line
			       (concat (make-string ses--linewidth ?\s) "\n")))
    (dotimes (row ses--numrows)
      (ses-goto-print row col)
      (when at-end
	;; Insert new columns before newline.
	(let ((inhibit-point-motion-hooks t))
	  (backward-char 1)))
      (if blank
	  (insert blank)
	(delete-char (- change))))))

(defun ses-print-cell-new-width (row col)
  "Same as `ses-print-cell', except if the cell's value is *skip*,
the preceding nonskipped cell is reprinted.  This function is used
when the width of cell (ROW,COL) has changed."
  (if (not (eq (ses-cell-value row col) '*skip*))
      (ses-print-cell row col)
    ;;Cell was skipped over - reprint previous
    (ses-goto-print row col)
    (backward-char 1)
    (let ((rowcol (ses-sym-rowcol (get-text-property (point) 'intangible))))
      (ses-print-cell (car rowcol) (cdr rowcol)))))


;;----------------------------------------------------------------------------
;; The data area
;;----------------------------------------------------------------------------

(defun ses-narrowed-p () (/= (- (point-max) (point-min)) (buffer-size)))

(defun ses-widen ()
  "Turn off narrowing, to be reenabled at end of command loop."
  (if (ses-narrowed-p)
      (setq ses--deferred-narrow t))
  (widen))

(defun ses-goto-data (def &optional col)
  "Move point to data area for (DEF,COL).  If DEF is a row
number, COL is the column number for a data cell -- otherwise DEF
is one of the symbols ses--col-widths, ses--col-printers,
ses--default-printer, ses--numrows, or ses--numcols."
  (ses-widen)
  (let ((inhibit-point-motion-hooks t)) ; In case intangible attrs are wrong.
    (if col
	;; It's a cell.
	(progn
	  (goto-char ses--data-marker)
	  (forward-line (+ 1 (* def (1+ ses--numcols)) col)))
      ;; Convert def-symbol to offset.
      (setq def (plist-get ses-paramlines-plist def))
      (or def (signal 'args-out-of-range nil))
      (goto-char ses--params-marker)
      (forward-line def))))

(defun ses-set-parameter (def value &optional elem)
  "Set parameter DEF to VALUE (with undo) and write the value to the data area.
See `ses-goto-data' for meaning of DEF.  Newlines in the data are escaped.
If ELEM is specified, it is the array subscript within DEF to be set to VALUE."
  (save-excursion
    ;; We call ses-goto-data early, using the old values of numrows and numcols
    ;; in case one of them is being changed.
    (ses-goto-data def)
    (let ((inhibit-read-only t)
	  (fmt (plist-get '(ses--col-widths      "(ses-column-widths %S)"
			    ses--col-printers    "(ses-column-printers %S)"
			    ses--default-printer "(ses-default-printer %S)"
			    ses--header-row      "(ses-header-row %S)"
			    ses--file-format     " %S ;SES file-format"
			    ses--numrows         " %S ;numrows"
			    ses--numcols         " %S ;numcols")
			  def))
	  oldval)
      (if elem
	  (progn
	    (setq oldval (aref (symbol-value def) elem))
	    (aset (symbol-value def) elem value))
	(setq oldval (symbol-value def))
	(set def value))
      ;; Special undo since it's outside the narrowed buffer.
      (let (buffer-undo-list)
	(delete-region (point) (line-end-position))
	(insert (format fmt (symbol-value def))))
      (push `(apply ses-set-parameter ,def ,oldval ,elem) buffer-undo-list))))


(defun ses-write-cells ()
  "Write cells in `ses--deferred-write' from local variables to data area.
Newlines in the data are escaped."
  (let* ((inhibit-read-only t)
	 (print-escape-newlines t)
	 rowcol row col cell sym formula printer text)
    (setq ses-start-time (float-time))
    (with-temp-message " "
      (save-excursion
	(while ses--deferred-write
	  (ses-time-check "Writing... (%d cells left)"
			  '(length ses--deferred-write))
	  (setq rowcol  (pop ses--deferred-write)
		row     (car rowcol)
		col     (cdr rowcol)
		cell    (ses-get-cell row col)
		sym     (ses-cell-symbol cell)
		formula (ses-cell-formula cell)
		printer (ses-cell-printer cell))
	  (if (eq (car-safe formula) 'ses-safe-formula)
	      (setq formula (cadr formula)))
	  (if (eq (car-safe printer) 'ses-safe-printer)
	      (setq printer (cadr printer)))
	  ;; This is noticeably faster than (format "%S %S %S %S %S")
	  (setq text    (concat "(ses-cell "
				(symbol-name sym)
				" "
				(prin1-to-string (symbol-value sym))
				" "
				(prin1-to-string formula)
				" "
				(prin1-to-string printer)
				" "
				(if (atom (ses-cell-references cell))
				    "nil"
				  (concat "("
					  (mapconcat 'symbol-name
						     (ses-cell-references cell)
						     " ")
					  ")"))
				")"))
	  (ses-goto-data row col)
	  (delete-region (point) (line-end-position))
	  (insert text)))
      (message " "))))


;;----------------------------------------------------------------------------
;; Formula relocation
;;----------------------------------------------------------------------------

(defun ses-formula-references (formula &optional result-so-far)
  "Produce a list of symbols for cells that this FORMULA's value
refers to.  For recursive calls, RESULT-SO-FAR is the list being
constructed, or t to get a wrong-type-argument error when the
first reference is found."
  (if (ses-sym-rowcol formula)
      ;;Entire formula is one symbol
      (add-to-list 'result-so-far formula)
    (if (consp formula)
	(cond
	 ((eq (car formula) 'ses-range)
	  (dolist (cur
		   (cdr (funcall 'macroexpand
				 (list 'ses-range (nth 1 formula)
				       (nth 2 formula)))))
	    (add-to-list 'result-so-far cur)))
	 ((null (eq (car formula) 'quote))
	  ;;Recursive call for subformulas
	  (dolist (cur formula)
	    (setq result-so-far (ses-formula-references cur result-so-far))))
	 (t
	  ;;Ignore other stuff
	  ))
      ;; other type of atom are ignored
      ))
    result-so-far)

(defsubst ses-relocate-symbol (sym rowcol startrow startcol rowincr colincr)
  "Relocate one symbol SYM, which corresponds to ROWCOL (a cons of ROW and
COL).  Cells starting at (STARTROW,STARTCOL) are being shifted
by (ROWINCR,COLINCR)."
  (let ((row (car rowcol))
	(col (cdr rowcol)))
    (if (or (< row startrow) (< col startcol))
	sym
      (setq row (+ row rowincr)
	    col (+ col colincr))
      (if (and (>= row startrow) (>= col startcol)
	       (< row ses--numrows) (< col ses--numcols))
	  ;;Relocate this variable
	  (ses-create-cell-symbol row col)
	;;Delete reference to a deleted cell
	nil))))

(defun ses-relocate-formula (formula startrow startcol rowincr colincr)
  "Produce a copy of FORMULA where all symbols that refer to cells in row
STARTROW or above, and col STARTCOL or above, are altered by adding ROWINCR
and COLINCR.  STARTROW and STARTCOL are 0-based.  Example:
	(ses-relocate-formula '(+ A1 B2 D3) 1 2 1 -1)
	=> (+ A1 B2 C4)
If ROWINCR or COLINCR is negative, references to cells being deleted are
removed.  Example:
	(ses-relocate-formula '(+ A1 B2 D3) 0 1 0 -1)
	=> (+ A1 C3)
Sets `ses-relocate-return' to 'delete if cell-references were removed."
  (let (rowcol result)
    (if (or (atom formula) (eq (car formula) 'quote))
	(if (setq rowcol (ses-sym-rowcol formula))
	    (ses-relocate-symbol formula rowcol
				 startrow startcol rowincr colincr)
	  formula) ; Pass through as-is.
      (dolist (cur formula)
	(setq rowcol (ses-sym-rowcol cur))
	(cond
	 (rowcol
	  (setq cur (ses-relocate-symbol cur rowcol
					 startrow startcol rowincr colincr))
	  (if cur
	      (push cur result)
	    ;; Reference to a deleted cell.  Set a flag in ses-relocate-return.
	    ;; don't change the flag if it's already 'range, since range implies
	    ;; 'delete.
	    (unless ses-relocate-return
	      (setq ses-relocate-return 'delete))))
	 ((eq (car-safe cur) 'ses-range)
	  (setq cur (ses-relocate-range cur startrow startcol rowincr colincr))
	  (if cur
	      (push cur result)))
	 ((or (atom cur) (eq (car cur) 'quote))
	  ;; Constants pass through unchanged.
	  (push cur result))
	 (t
	  ;; Recursively copy and alter subformulas.
	  (push (ses-relocate-formula cur startrow startcol
						   rowincr colincr)
		result))))
      (nreverse result))))

(defun ses-relocate-range (range startrow startcol rowincr colincr)
  "Relocate one RANGE, of the form '(ses-range min max).  Cells starting
at (STARTROW,STARTCOL) are being shifted by (ROWINCR,COLINCR).  Result is the
new range, or nil if the entire range is deleted.  If new rows are being added
just beyond the end of a row range, or new columns just beyond a column range,
the new rows/columns will be added to the range.  Sets `ses-relocate-return'
if the range was altered."
  (let* ((minorig   (cadr range))
	 (minrowcol (ses-sym-rowcol minorig))
	 (min       (ses-relocate-symbol minorig minrowcol
					 startrow startcol
					 rowincr colincr))
	 (maxorig   (nth 2 range))
	 (maxrowcol (ses-sym-rowcol maxorig))
	 (max       (ses-relocate-symbol maxorig maxrowcol
					 startrow startcol
					 rowincr colincr))
	 field)
    (cond
     ((and (not min) (not max))
      (setq range nil)) ; The entire range is deleted.
     ((zerop colincr)
      ;; Inserting or deleting rows.
      (setq field 'car)
      (if (not min)
	  ;; Chopped off beginning of range.
	  (setq min           (ses-create-cell-symbol startrow (cdr minrowcol))
		ses-relocate-return 'range))
      (if (not max)
	  (if (> rowincr 0)
	      ;; Trying to insert a nonexistent row.
	      (setq max (ses-create-cell-symbol (1- ses--numrows)
						(cdr minrowcol)))
	    ;; End of range is being deleted.
	    (setq max (ses-create-cell-symbol (1- startrow) (cdr minrowcol))
		  ses-relocate-return 'range))
	(and (> rowincr 0)
	     (= (car maxrowcol) (1- startrow))
	     (= (cdr minrowcol) (cdr maxrowcol))
	     ;; Insert after ending row of vertical range --- include it.
	     (setq max (ses-create-cell-symbol (+ startrow rowincr -1)
					       (cdr maxrowcol))))))
     (t
      ;; Inserting or deleting columns.
      (setq field 'cdr)
      (if (not min)
	  ;; Chopped off beginning of range.
	  (setq min          (ses-create-cell-symbol (car minrowcol) startcol)
		ses-relocate-return 'range))
      (if (not max)
	  (if (> colincr 0)
	      ;; Trying to insert a nonexistent column.
	      (setq max (ses-create-cell-symbol (car maxrowcol)
						(1- ses--numcols)))
	    ;; End of range is being deleted.
	    (setq max (ses-create-cell-symbol (car maxrowcol) (1- startcol))
		  ses-relocate-return 'range))
	(and (> colincr 0)
	     (= (cdr maxrowcol) (1- startcol))
	     (= (car minrowcol) (car maxrowcol))
	     ;; Insert after ending column of horizontal range --- include it.
	     (setq max (ses-create-cell-symbol (car maxrowcol)
						  (+ startcol colincr -1)))))))
    (when range
      (if (/= (- (funcall field maxrowcol)
		 (funcall field minrowcol))
	      (- (funcall field (ses-sym-rowcol max))
		 (funcall field (ses-sym-rowcol min))))
	  ;; This range has changed size.
	  (setq ses-relocate-return 'range))
      `(ses-range ,min ,max ,@(cdddr range)))))

(defun ses-relocate-all (minrow mincol rowincr colincr)
  "Alter all cell values, symbols, formulas, and reference-lists to relocate
the rectangle (MINROW,MINCOL)..(NUMROWS,NUMCOLS) by adding ROWINCR and COLINCR
to each symbol."
  (let (reform)
    (let (mycell newval)
      (dotimes-with-progress-reporter
	  (row ses--numrows) "Relocating formulas..."
	(dotimes (col ses--numcols)
	  (setq ses-relocate-return nil
		mycell (ses-get-cell row col)
		newval (ses-relocate-formula (ses-cell-formula mycell)
					     minrow mincol rowincr colincr))
	  (ses-set-cell row col 'formula newval)
	  (if (eq ses-relocate-return 'range)
	      ;; This cell contains a (ses-range X Y) where a cell has been
	      ;; inserted or deleted in the middle of the range.
	      (push (cons row col) reform))
	  (if ses-relocate-return
	      ;; This cell referred to a cell that's been deleted or is no
	      ;; longer part of the range.  We can't fix that now because
	      ;; reference lists cells have been partially updated.
	      (add-to-list 'ses--deferred-recalc
			   (ses-create-cell-symbol row col)))
	  (setq newval (ses-relocate-formula (ses-cell-references mycell)
					     minrow mincol rowincr colincr))
	  (ses-set-cell row col 'references newval)
	  (and (>= row minrow) (>= col mincol)
	       (ses-set-cell row col 'symbol
			     (ses-create-cell-symbol row col))))))
    ;; Relocate the cell values.
    (let (oldval myrow mycol xrow xcol)
      (cond
       ((and (<= rowincr 0) (<= colincr 0))
	;; Deletion of rows and/or columns.
	(dotimes-with-progress-reporter
	    (row (- ses--numrows minrow)) "Relocating variables..."
	  (setq myrow  (+ row minrow))
	  (dotimes (col (- ses--numcols mincol))
	    (setq mycol  (+ col mincol)
		  xrow   (- myrow rowincr)
		  xcol   (- mycol colincr))
	    (if (and (< xrow ses--numrows) (< xcol ses--numcols))
		(setq oldval (ses-cell-value xrow xcol))
	      ;; Cell is off the end of the array.
	      (setq oldval (symbol-value (ses-create-cell-symbol xrow xcol))))
	    (ses-set-cell myrow mycol 'value oldval))))
       ((and (wholenump rowincr) (wholenump colincr))
	;; Insertion of rows and/or columns.  Run the loop backwards.
	(let ((disty (1- ses--numrows))
	      (distx (1- ses--numcols))
	      myrow mycol)
	  (dotimes-with-progress-reporter
	      (row (- ses--numrows minrow)) "Relocating variables..."
	    (setq myrow (- disty row))
	    (dotimes (col (- ses--numcols mincol))
	      (setq mycol (- distx col)
		    xrow  (- myrow rowincr)
		    xcol  (- mycol colincr))
	      (if (or (< xrow minrow) (< xcol mincol))
		  ;; Newly-inserted value.
		  (setq oldval nil)
		;; Transfer old value.
		(setq oldval (ses-cell-value xrow xcol)))
	      (ses-set-cell myrow mycol 'value oldval)))
	  t))  ; Make testcover happy by returning non-nil here.
       (t
	(error "ROWINCR and COLINCR must have the same sign"))))
    ;; Reconstruct reference lists for cells that contain ses-ranges that have
    ;; changed size.
    (when reform
      (message "Fixing ses-ranges...")
      (let (row col)
	(setq ses-start-time (float-time))
	(while reform
	  (ses-time-check "Fixing ses-ranges... (%d left)" '(length reform))
	  (setq row    (caar reform)
		col    (cdar reform)
		reform (cdr reform))
	  (ses-cell-set-formula row col (ses-cell-formula row col))))
      (message nil))))


;;----------------------------------------------------------------------------
;; Undo control
;;----------------------------------------------------------------------------

(defun ses-begin-change ()
  "For undo, remember point before we start changing hidden stuff."
  (let ((inhibit-read-only t))
    (insert-and-inherit "X")
    (delete-region (1- (point)) (point))))

(defun ses-set-with-undo (sym newval)
  "Like set, but undoable.  Result is t if value has changed."
  ;; We try to avoid adding redundant entries to the undo list, but this is
  ;; unavoidable for strings because equal ignores text properties and there's
  ;; no easy way to get the whole property list to see if it's different!
  (unless (and (boundp sym)
	       (equal (symbol-value sym) newval)
	       (not (stringp newval)))
    (push (if (boundp sym)
	      `(apply ses-set-with-undo ,sym ,(symbol-value sym))
	    `(apply ses-unset-with-undo ,sym))
	  buffer-undo-list)
    (set sym newval)
    t))

(defun ses-unset-with-undo (sym)
  "Set SYM to be unbound.  This is undoable."
  (when (1value (boundp sym)) ; Always bound, except after a programming error.
    (push `(apply ses-set-with-undo ,sym ,(symbol-value sym)) buffer-undo-list)
    (makunbound sym)))

(defun ses-aset-with-undo (array idx newval)
  "Like `aset', but undoable.
Result is t if element has changed."
  (unless (equal (aref array idx) newval)
    (push `(apply ses-aset-with-undo ,array ,idx
		  ,(aref array idx)) buffer-undo-list)
    (aset array idx newval)
    t))


;;----------------------------------------------------------------------------
;; Startup for major mode
;;----------------------------------------------------------------------------

(defun ses-load ()
  "Parse the current buffer and set up buffer-local variables.
Does not execute cell formulas or print functions."
  (widen)
  ;; Read our global parameters, which should be a 3-element list.
  (goto-char (point-max))
  (search-backward ";; Local Variables:\n" nil t)
  (backward-list 1)
  (setq ses--params-marker (point-marker))
  (let ((params (condition-case nil (read (current-buffer)) (error nil))))
    (or (and (= (safe-length params) 3)
	     (numberp (car params))
	     (numberp (cadr params))
	     (>= (cadr params) 0)
	     (numberp (nth 2 params))
	     (> (nth 2 params) 0))
	(error "Invalid SES file"))
    (setq ses--file-format (car params)
	  ses--numrows     (cadr params)
	  ses--numcols     (nth 2 params))
    (when (= ses--file-format 1)
      (let (buffer-undo-list) ; This is not undoable.
	(ses-goto-data 'ses--header-row)
	(insert "(ses-header-row 0)\n")
	(ses-set-parameter 'ses--file-format 2)
	(message "Upgrading from SES-1 file format")))
    (or (= ses--file-format 2)
	(error "This file needs a newer version of the SES library code"))
    (ses-create-cell-variable-range 0 (1- ses--numrows) 0 (1- ses--numcols))
    ;; Initialize cell array.
    (setq ses--cells (make-vector ses--numrows nil))
    (dotimes (row ses--numrows)
      (aset ses--cells row (make-vector ses--numcols nil))))
  ;; Skip over print area, which we assume is correct.
  (goto-char (point-min))
  (forward-line ses--numrows)
  (or (looking-at ses-print-data-boundary)
      (error "Missing marker between print and data areas"))
  (forward-char 1)
  (setq ses--data-marker (point-marker))
  (forward-char (1- (length ses-print-data-boundary)))
  ;; Initialize printer and symbol lists.
  (mapc 'ses-printer-record ses-standard-printer-functions)
  (setq ses--symbolic-formulas nil)
  ;; Load cell definitions.
  (dotimes (row ses--numrows)
    (dotimes (col ses--numcols)
      (let* ((x      (read (current-buffer)))
	     (rowcol (ses-sym-rowcol (car-safe (cdr-safe x)))))
	(or (and (looking-at "\n")
		 (eq (car-safe x) 'ses-cell)
		 (eq row (car rowcol))
		 (eq col (cdr rowcol)))
	    (error "Cell-def error"))
	(eval x)))
    (or (looking-at "\n\n")
	(error "Missing blank line between rows")))
  ;; Load global parameters.
  (let ((widths      (read (current-buffer)))
	(n1          (char-after (point)))
	(printers    (read (current-buffer)))
	(n2          (char-after (point)))
	(def-printer (read (current-buffer)))
	(n3          (char-after (point)))
	(head-row    (read (current-buffer)))
	(n4          (char-after (point))))
    (or (and (eq (car-safe widths) 'ses-column-widths)
	     (= n1 ?\n)
	     (eq (car-safe printers) 'ses-column-printers)
	     (= n2 ?\n)
	     (eq (car-safe def-printer) 'ses-default-printer)
	     (= n3 ?\n)
	     (eq (car-safe head-row) 'ses-header-row)
	     (= n4 ?\n))
	(error "Invalid SES global parameters"))
    (1value (eval widths))
    (1value (eval def-printer))
    (1value (eval printers))
    (1value (eval head-row)))
  ;; Should be back at global-params.
  (forward-char 1)
  (or (looking-at (replace-regexp-in-string "1" "[0-9]+"
					    ses-initial-global-parameters))
      (error "Problem with column-defs or global-params"))
  ;; Check for overall newline count in definitions area.
  (forward-line 3)
  (let ((start (point)))
    (ses-goto-data 'ses--numrows)
    (or (= (point) start)
	(error "Extraneous newlines someplace?"))))

(defun ses-setup ()
  "Set up for display of only the printed cell values.

Narrows the buffer to show only the print area.  Gives it `read-only' and
`intangible' properties.  Sets up highlighting for current cell."
  (interactive)
  (let ((end (point-min))
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(was-modified (buffer-modified-p))
	pos sym)
    (ses-goto-data 0 0) ; Include marker between print-area and data-area.
    (set-text-properties (point) (point-max) nil) ; Delete garbage props.
    (mapc 'delete-overlay (overlays-in (point-min) (point-max)))
    ;; The print area is read-only (except for our special commands) and uses a
    ;; special keymap.
    (put-text-property (point-min) (1- (point)) 'read-only 'ses)
    (put-text-property (point-min) (1- (point)) 'keymap 'ses-mode-print-map)
    ;; For the beginning of the buffer, we want the read-only and keymap
    ;; attributes to be  inherited from the first character.
    (put-text-property (point-min) (1+ (point-min)) 'front-sticky t)
    ;; Create intangible properties, which also indicate which cell the text
    ;; came from.
    (dotimes-with-progress-reporter (row ses--numrows) "Finding cells..."
      (dotimes (col ses--numcols)
	(setq pos  end
	      sym  (ses-cell-symbol row col))
	;; Include skipped cells following this one.
	(while (and (< col (1- ses--numcols))
		    (eq (ses-cell-value row (1+ col)) '*skip*))
	  (setq end (+ end (ses-col-width col) 1)
		col (1+ col)))
	(setq end (save-excursion
		    (goto-char pos)
		    (move-to-column (+ (current-column) (- end pos)
				       (ses-col-width col)))
		    (if (eolp)
			(+ end (ses-col-width col) 1)
		      (forward-char)
		      (point))))
	(put-text-property pos end 'intangible sym)))
    ;; Adding these properties did not actually alter the text.
    (unless was-modified
      (restore-buffer-modified-p nil)
      (buffer-disable-undo)
      (buffer-enable-undo)))
  ;; Create the underlining overlay.  It's impossible for (point) to be 2,
  ;; because column A must be at least 1 column wide.
  (setq ses--curcell-overlay (make-overlay (1+ (point-min)) (1+ (point-min))))
  (overlay-put ses--curcell-overlay 'face 'underline))

(defun ses-cleanup ()
  "Cleanup when changing a buffer from SES mode to something else.
Delete overlays, remove special text properties."
  (widen)
  (let ((inhibit-read-only t)
	;; When reverting, hide the buffer name, otherwise Emacs will ask the
	;; user "the file is modified, do you really want to make modifications
	;; to this buffer", where the "modifications" refer to the irrelevant
	;; set-text-properties below.
	(buffer-file-name nil)
	(was-modified      (buffer-modified-p)))
    ;; Delete read-only, keymap, and intangible properties.
    (set-text-properties (point-min) (point-max) nil)
    ;; Delete overlay.
    (mapc 'delete-overlay (overlays-in (point-min) (point-max)))
    (unless was-modified
      (restore-buffer-modified-p nil))))

;;;###autoload
(defun ses-mode ()
  "Major mode for Simple Emacs Spreadsheet.
See \"ses-example.ses\" (in `data-directory') for more info.

Key definitions:
\\{ses-mode-map}
These key definitions are active only in the print area (the visible part):
\\{ses-mode-print-map}
These are active only in the minibuffer, when entering or editing a formula:
\\{ses-mode-edit-map}"
  (interactive)
  (unless (and (boundp 'ses--deferred-narrow)
	       (eq ses--deferred-narrow 'ses-mode))
    (kill-all-local-variables)
    (ses-set-localvars)
    (setq major-mode             'ses-mode
	  mode-name              "SES"
	  next-line-add-newlines nil
	  truncate-lines         t
	  ;; SES deliberately puts lots of trailing whitespace in its buffer.
	  show-trailing-whitespace nil
	  ;; Cell ranges do not work reasonably without this.
	  transient-mark-mode    t
	  ;; Not to use tab characters for safe (tabs may do bad for column
	  ;; calculation).
	  indent-tabs-mode	 nil)
    (1value (add-hook 'change-major-mode-hook 'ses-cleanup nil t))
    (1value (add-hook 'before-revert-hook 'ses-cleanup nil t))
    (setq header-line-format   '(:eval (progn
					 (when (/= (window-hscroll)
						   ses--header-hscroll)
					   ;; Reset ses--header-hscroll first,
					   ;; to avoid recursion problems when
					   ;; debugging ses-create-header-string
					   (setq ses--header-hscroll
						 (window-hscroll))
					   (ses-create-header-string))
					 ses--header-string)))
    (let ((was-empty    (zerop (buffer-size)))
	  (was-modified (buffer-modified-p)))
      (save-excursion
	(if was-empty
	    ;; Initialize buffer to contain one cell, for now.
	    (insert ses-initial-file-contents))
	(ses-load)
	(ses-setup))
      (when was-empty
	(unless (equal ses-initial-default-printer
		       (1value ses--default-printer))
	  (1value (ses-read-default-printer ses-initial-default-printer)))
	(unless (= ses-initial-column-width (1value (ses-col-width 0)))
	  (1value (ses-set-column-width 0 ses-initial-column-width)))
	(ses-set-curcell)
	(if (> (car ses-initial-size) (1value ses--numrows))
	    (1value (ses-insert-row (1- (car ses-initial-size)))))
	(if (> (cdr ses-initial-size) (1value ses--numcols))
	    (1value (ses-insert-column (1- (cdr ses-initial-size)))))
	(ses-write-cells)
	(restore-buffer-modified-p was-modified)
	(buffer-disable-undo)
	(buffer-enable-undo)
	(goto-char (point-min))))
    (use-local-map ses-mode-map)
    ;; Set the deferred narrowing flag (we can't narrow until after
    ;; after-find-file completes).  If .ses is on the auto-load alist and the
    ;; file has "mode: ses", our ses-mode function will be called twice!  Use a
    ;; special flag to detect this (will be reset by ses-command-hook).  For
    ;; find-alternate-file, post-command-hook doesn't get run for some reason,
    ;; so use an idle timer to make sure.
    (setq ses--deferred-narrow 'ses-mode)
    (1value (add-hook 'post-command-hook 'ses-command-hook nil t))
    (run-with-idle-timer 0.01 nil 'ses-command-hook)
    (run-mode-hooks 'ses-mode-hook)))

(put 'ses-mode 'mode-class 'special)

(defun ses-command-hook ()
  "Invoked from `post-command-hook'.  If point has moved to a different cell,
moves the underlining overlay.  Performs any recalculations or cell-data
writes that have been deferred.  If buffer-narrowing has been deferred,
narrows the buffer now."
  (condition-case err
      (when (eq major-mode 'ses-mode)  ; Otherwise, not our buffer anymore.
	(when ses--deferred-recalc
	  ;; We reset the deferred list before starting on the recalc --- in
	  ;; case of error, we don't want to retry the recalc after every
	  ;; keystroke!
	  (ses-initialize-Dijkstra-attempt)
	  (let ((old ses--deferred-recalc))
	    (setq ses--deferred-recalc nil)
	    (ses-update-cells old)))
	(when ses--deferred-write
	  ;; We don't reset the deferred list before starting --- the most
	  ;; likely error is keyboard-quit, and we do want to keep trying these
	  ;; writes after a quit.
	  (ses-write-cells)
	  (push '(apply ses-widen) buffer-undo-list))
	(when ses--deferred-narrow
	  ;; We're not allowed to narrow the buffer until after-find-file has
	  ;; read the local variables at the end of the file.  Now it's safe to
	  ;; do the narrowing.
	  (narrow-to-region (point-min) ses--data-marker)
	  (setq ses--deferred-narrow nil))
	;; Update the modeline.
	(let ((oldcell ses--curcell))
	  (ses-set-curcell)
	  (unless (eq ses--curcell oldcell)
	    (cond
	     ((not ses--curcell)
	      (setq mode-line-process nil))
	     ((atom ses--curcell)
	      (setq mode-line-process (list " cell "
					    (symbol-name ses--curcell))))
	     (t
	      (setq mode-line-process (list " range "
					    (symbol-name (car ses--curcell))
					    "-"
					    (symbol-name (cdr ses--curcell))))))
	    (force-mode-line-update)))
	;; Use underline overlay for single-cells only, turn off otherwise.
	(if (listp ses--curcell)
	    (move-overlay ses--curcell-overlay 2 2)
	  (let ((next (next-single-property-change (point) 'intangible)))
	    (move-overlay ses--curcell-overlay (point) (1- next))))
	(when (not (pos-visible-in-window-p))
	  ;; Scrolling will happen later.
	  (run-with-idle-timer 0.01 nil 'ses-command-hook)
	  (setq ses--curcell t)))
    ;; Prevent errors in this post-command-hook from silently erasing the hook!
    (error
     (unless executing-kbd-macro
       (ding))
     (message "%s" (error-message-string err))))
  nil) ; Make coverage-tester happy.

(defun ses-create-header-string ()
  "Set up `ses--header-string' as the buffer's header line.
Based on the current set of columns and `window-hscroll' position."
  (let ((totwidth (- (window-hscroll)))
	result width x)
    ;; Leave room for the left-side fringe and scrollbar.
    (push (propertize " " 'display '((space :align-to 0))) result)
    (dotimes (col ses--numcols)
      (setq width    (ses-col-width col)
	    totwidth (+ totwidth width 1))
      (if (= totwidth 1)
	  ;; Scrolled so intercolumn space is leftmost.
	  (push " " result))
      (when (> totwidth 1)
	(if (> ses--header-row 0)
	    (save-excursion
	      (ses-goto-print (1- ses--header-row) col)
	      (setq x (buffer-substring-no-properties (point)
						      (+ (point) width)))
	      ;; Strip trailing space.
	      (if (string-match "[ \t]+\\'" x)
		  (setq x (substring x 0 (match-beginning 0))))
	      ;; Cut off excess text.
	      (if (>= (length x) totwidth)
		  (setq x (substring x 0 (- totwidth -1)))))
	  (setq x (ses-column-letter col)))
	  (push (propertize x 'face ses-box-prop) result)
	(push (propertize "."
			    'display    `((space :align-to ,(1- totwidth)))
			    'face       ses-box-prop)
	      result)
	;; Allow the following space to be squished to make room for the 3-D box
	;; Coverage test ignores properties, thinks this is always a space!
	(push (1value (propertize " " 'display `((space :align-to ,totwidth))))
	      result)))
    (if (> ses--header-row 0)
	(push (propertize (format "  [row %d]" ses--header-row)
			  'display '((height (- 1))))
	      result))
    (setq ses--header-string (apply 'concat (nreverse result)))))


;;----------------------------------------------------------------------------
;; Redisplay and recalculation
;;----------------------------------------------------------------------------

(defun ses-jump (sym)
  "Move point to cell SYM."
  (interactive "SJump to cell: ")
  (let ((rowcol (ses-sym-rowcol sym)))
    (or rowcol (error "Invalid cell name"))
    (if (eq (symbol-value sym) '*skip*)
	(error "Cell is covered by preceding cell"))
    (ses-goto-print (car rowcol) (cdr rowcol))))

(defun ses-jump-safe (cell)
  "Like `ses-jump', but no error if invalid cell."
  (condition-case nil
      (ses-jump cell)
    (error)))

(defun ses-reprint-all (&optional nonarrow)
  "Recreate the display area.  Calls all printer functions.  Narrows to
print area if NONARROW is nil."
  (interactive "*P")
  (widen)
  (unless nonarrow
    (setq ses--deferred-narrow t))
  (let ((startcell (get-text-property (point) 'intangible))
	(inhibit-read-only t))
    (ses-begin-change)
    (goto-char (point-min))
    (search-forward ses-print-data-boundary)
    (backward-char (length ses-print-data-boundary))
    (delete-region (point-min) (point))
    ;; Insert all blank lines before printing anything, so ses-print-cell can
    ;; find the data area when inserting or deleting *skip* values for cells.
    (dotimes (row ses--numrows)
      (insert-and-inherit ses--blank-line))
    (dotimes-with-progress-reporter (row ses--numrows) "Reprinting..."
      (if (eq (ses-cell-value row 0) '*skip*)
	  ;; Column deletion left a dangling skip.
	  (ses-set-cell row 0 'value nil))
      (dotimes (col ses--numcols)
	(ses-print-cell row col))
      (beginning-of-line 2))
    (ses-jump-safe startcell)))

(defun ses-initialize-Dijkstra-attempt ()
  (setq ses--Dijkstra-attempt-nb (1+ ses--Dijkstra-attempt-nb)
	ses--Dijkstra-weight-bound (* ses--numrows ses--numcols)))

(defun ses-recalculate-cell ()
  "Recalculate and reprint the current cell or range.

For an individual cell, shows the error if the formula or printer
signals one, or otherwise shows the cell's complete value.  For a range, the
cells are recalculated in \"natural\" order, so cells that other cells refer
to are recalculated first."
  (interactive "*")
  (ses-check-curcell 'range)
  (ses-begin-change)
  (ses-initialize-Dijkstra-attempt)
  (let (sig cur-rowcol)
    (setq ses-start-time (float-time))
    (if (atom ses--curcell)
	(when
	  (setq cur-rowcol (ses-sym-rowcol ses--curcell)
		sig (progn
		      (ses-cell-property-set :ses-Dijkstra-attempt
					     (cons ses--Dijkstra-attempt-nb 0)
					     (car cur-rowcol) (cdr cur-rowcol) )
		      (ses-calculate-cell (car cur-rowcol) (cdr cur-rowcol) t)))
	  (nconc sig (list (ses-cell-symbol (car cur-rowcol)
					    (cdr cur-rowcol)))))
      ;; First, recalculate all cells that don't refer to other cells and
      ;; produce a list of cells with references.
      (ses-dorange ses--curcell
	(ses-time-check "Recalculating... %s" '(ses-cell-symbol row col))
	(condition-case nil
	    (progn
	      ;; The t causes an error if the cell has references.  If no
	      ;; references, the t will be the result value.
	      (1value (ses-formula-references (ses-cell-formula row col) t))
	      (ses-cell-property-set :ses-Dijkstra-attempt
				     (cons ses--Dijkstra-attempt-nb 0)
				     row col)
	      (when (setq sig (ses-calculate-cell row col t))
		(nconc sig (list (ses-cell-symbol row col)))))
	  (wrong-type-argument
	   ;; The formula contains a reference.
	   (add-to-list 'ses--deferred-recalc (ses-cell-symbol row col))))))
    ;; Do the update now, so we can force recalculation.
    (let ((x ses--deferred-recalc))
      (setq ses--deferred-recalc nil)
      (condition-case hold
	  (ses-update-cells x t)
	(error (setq sig hold))))
    (cond
     (sig
      (message "%s" (error-message-string sig)))
     ((consp ses--curcell)
      (message " "))
     (t
      (princ (symbol-value ses--curcell))))))

(defun ses-recalculate-all ()
  "Recalculate and reprint all cells."
  (interactive "*")
  (let ((startcell    (get-text-property (point) 'intangible))
	(ses--curcell (cons 'A1 (ses-cell-symbol (1- ses--numrows)
						 (1- ses--numcols)))))
    (ses-recalculate-cell)
    (ses-jump-safe startcell)))

(defun ses-truncate-cell ()
  "Reprint current cell, but without spillover into any following blank cells."
  (interactive "*")
  (ses-check-curcell)
  (let* ((rowcol (ses-sym-rowcol ses--curcell))
	 (row    (car rowcol))
	 (col    (cdr rowcol)))
    (when (and (< col (1- ses--numcols)) ;;Last column can't spill over, anyway
	       (eq (ses-cell-value row (1+ col)) '*skip*))
      ;; This cell has spill-over.  We'll momentarily pretend the following cell
      ;; has a `t' in it.
      (eval `(let ((,(ses-cell-symbol row (1+ col)) t))
	       (ses-print-cell row col)))
      ;; Now remove the *skip*.  ses-print-cell is always nil here.
      (ses-set-cell row (1+ col) 'value nil)
      (1value (ses-print-cell row (1+ col))))))

(defun ses-reconstruct-all ()
  "Reconstruct buffer based on cell data stored in Emacs variables."
  (interactive "*")
  (ses-begin-change)
  ;;Reconstruct reference lists.
  (let (x yrow ycol)
    ;;Delete old reference lists
    (dotimes-with-progress-reporter
	(row ses--numrows) "Deleting references..."
      (dotimes (col ses--numcols)
	(ses-set-cell row col 'references nil)))
    ;;Create new reference lists
    (dotimes-with-progress-reporter
	(row ses--numrows) "Computing references..."
      (dotimes (col ses--numcols)
	(dolist (ref (ses-formula-references (ses-cell-formula row col)))
	  (setq x    (ses-sym-rowcol ref)
		yrow (car x)
		ycol (cdr x))
	  (ses-set-cell yrow ycol 'references
			(cons (ses-cell-symbol row col)
			      (ses-cell-references yrow ycol)))))))
  ;; Delete everything and reconstruct basic data area.
  (ses-widen)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (if (search-backward ";; Local Variables:\n" nil t)
	(delete-region (point-min) (point))
      ;; Buffer is quite screwed up --- can't even save the user-specified
      ;; locals.
      (delete-region (point-min) (point-max))
      (insert ses-initial-file-trailer)
      (goto-char (point-min)))
    ;; Create a blank display area.
    (dotimes (row ses--numrows)
      (insert ses--blank-line))
    (insert ses-print-data-boundary)
    (backward-char (1- (length ses-print-data-boundary)))
    (setq ses--data-marker (point-marker))
    (forward-char (1- (length ses-print-data-boundary)))
    ;; Placeholders for cell data.
    (insert (make-string (* ses--numrows (1+ ses--numcols)) ?\n))
    ;; Placeholders for col-widths, col-printers, default-printer, header-row.
    (insert "\n\n\n\n")
    (insert ses-initial-global-parameters)
    (backward-char (1- (length ses-initial-global-parameters)))
    (setq ses--params-marker (point-marker))
    (forward-char (1- (length ses-initial-global-parameters))))
  (ses-set-parameter 'ses--col-widths ses--col-widths)
  (ses-set-parameter 'ses--col-printers ses--col-printers)
  (ses-set-parameter 'ses--default-printer ses--default-printer)
  (ses-set-parameter 'ses--header-row ses--header-row)
  (ses-set-parameter 'ses--numrows ses--numrows)
  (ses-set-parameter 'ses--numcols ses--numcols)
  ;;Keep our old narrowing
  (ses-setup)
  (ses-recalculate-all)
  (goto-char (point-min)))


;;----------------------------------------------------------------------------
;; Input of cell formulas
;;----------------------------------------------------------------------------

(defun ses-edit-cell (row col newval)
  "Display current cell contents in minibuffer, for editing.  Returns nil if
cell formula was unsafe and user declined confirmation."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (ses-check-curcell)
     (let* ((rowcol  (ses-sym-rowcol ses--curcell))
	    (row     (car rowcol))
	    (col     (cdr rowcol))
	    (formula (ses-cell-formula row col))
	    initial)
       (if (eq (car-safe formula) 'ses-safe-formula)
	   (setq formula (cadr formula)))
       (if (eq (car-safe formula) 'quote)
	   (setq initial (format "'%S" (cadr formula)))
	 (setq initial (prin1-to-string formula)))
       (if (stringp formula)
	   ;; Position cursor inside close-quote.
	   (setq initial (cons initial (length initial))))
       (list row col
	     (read-from-minibuffer (format "Cell %s: " ses--curcell)
				   initial
				   ses-mode-edit-map
				   t ; Convert to Lisp object.
				   'ses-read-cell-history)))))
  (when (ses-warn-unsafe newval 'unsafep)
    (ses-begin-change)
    (ses-cell-set-formula row col newval)
    t))

(defun ses-read-cell (row col newval)
  "Self-insert for initial character of cell function."
  (interactive
   (let* ((initial (this-command-keys))
          (rowcol  (progn (ses-check-curcell) (ses-sym-rowcol ses--curcell)))
          (curval (ses-cell-formula (car rowcol) (cdr rowcol))))
     (barf-if-buffer-read-only)
     (list (car rowcol)
	   (cdr rowcol)
           (read-from-minibuffer
            (format "Cell %s: " ses--curcell)
            (cons (if (equal initial "\"") "\"\""
                    (if (equal initial "(") "()" initial)) 2)
            ses-mode-edit-map
            t                         ; Convert to Lisp object.
            'ses-read-cell-history
            (prin1-to-string (if (eq (car-safe curval) 'ses-safe-formula)
				 (cadr curval)
			       curval))))))
  (when (ses-edit-cell row col newval)
    (ses-command-hook) ; Update cell widths before movement.
    (dolist (x ses-after-entry-functions)
      (funcall x 1))))

(defun ses-read-symbol (row col symb)
  "Self-insert for a symbol as a cell formula.  The set of all symbols that
have been used as formulas in this spreadsheet is available for completions."
  (interactive
   (let ((rowcol (progn (ses-check-curcell) (ses-sym-rowcol ses--curcell)))
	 newval)
     (barf-if-buffer-read-only)
     (setq newval (completing-read (format "Cell %s ': " ses--curcell)
				   ses--symbolic-formulas))
     (list (car rowcol)
	   (cdr rowcol)
	   (if (string= newval "")
	       nil ; Don't create zero-length symbols!
	     (list 'quote (intern newval))))))
  (when (ses-edit-cell row col symb)
    (ses-command-hook) ; Update cell widths before movement.
    (dolist (x ses-after-entry-functions)
      (funcall x 1))))

(defun ses-clear-cell-forward (count)
  "Delete formula and printer for current cell and then move to next cell.
With prefix, deletes several cells."
  (interactive "*p")
  (if (< count 0)
      (1value (ses-clear-cell-backward (- count)))
    (ses-check-curcell)
    (ses-begin-change)
    (dotimes (x count)
      (ses-set-curcell)
      (let ((rowcol (ses-sym-rowcol ses--curcell)))
	(or rowcol (signal 'end-of-buffer nil))
	(ses-clear-cell (car rowcol) (cdr rowcol)))
      (forward-char 1))))

(defun ses-clear-cell-backward (count)
  "Move to previous cell and then delete it.  With prefix, deletes several
cells."
  (interactive "*p")
  (if (< count 0)
      (1value (ses-clear-cell-forward (- count)))
    (ses-check-curcell 'end)
    (ses-begin-change)
    (dotimes (x count)
      (backward-char 1) ; Will signal 'beginning-of-buffer if appropriate.
      (ses-set-curcell)
      (let ((rowcol (ses-sym-rowcol ses--curcell)))
	(ses-clear-cell (car rowcol) (cdr rowcol))))))


;;----------------------------------------------------------------------------
;; Input of cell-printer functions
;;----------------------------------------------------------------------------

(defun ses-read-printer (prompt default)
  "Common code for `ses-read-cell-printer', `ses-read-column-printer', and `ses-read-default-printer'.
PROMPT should end with \": \".  Result is t if operation was canceled."
  (barf-if-buffer-read-only)
  (if (eq default t)
      (setq default "")
    (setq prompt (format "%s [currently %S]: "
			 (substring prompt 0 -2)
			 default)))
  (let ((new (read-from-minibuffer prompt
				   nil ; Initial contents.
				   ses-mode-edit-map
				   t   ; Evaluate the result.
				   'ses-read-printer-history
				   (prin1-to-string default))))
    (if (equal new default)
	;; User changed mind, decided not to change printer.
	(setq new t)
      (ses-printer-validate new)
      (or (not new)
	  (stringp new)
	  (stringp (car-safe new))
	  (ses-warn-unsafe new 'unsafep-function)
	  (setq new t)))
    new))

(defun ses-read-cell-printer (newval)
  "Set the printer function for the current cell or range.

A printer function is either a string (a format control-string with one
%-sequence -- result from format will be right-justified), or a list of one
string (result from format will be left-justified), or a lambda-expression of
one argument, or a symbol that names a function of one argument.  In the
latter two cases, the function's result should be either a string (will be
right-justified) or a list of one string (will be left-justified)."
  (interactive
   (let ((default t)
	 x)
     (ses-check-curcell 'range)
     ;;Default is none if not all cells in range have same printer
     (catch 'ses-read-cell-printer
       (ses-dorange ses--curcell
	 (setq x (ses-cell-printer row col))
	 (if (eq (car-safe x) 'ses-safe-printer)
	     (setq x (cadr x)))
	 (if (eq default t)
	     (setq default x)
	   (unless (equal default x)
	     ;;Range contains differing printer functions
	     (setq default t)
	     (throw 'ses-read-cell-printer t)))))
     (list (ses-read-printer (format "Cell %S printer: " ses--curcell)
			     default))))
  (unless (eq newval t)
    (ses-begin-change)
    (ses-dorange ses--curcell
      (ses-set-cell row col 'printer newval)
      (ses-print-cell row col))))

(defun ses-read-column-printer (col newval)
  "Set the printer function for the current column.
See `ses-read-cell-printer' for input forms."
  (interactive
   (let ((col (cdr (ses-sym-rowcol ses--curcell))))
     (ses-check-curcell)
     (list col (ses-read-printer (format "Column %s printer: "
					 (ses-column-letter col))
				 (ses-col-printer col)))))

  (unless (eq newval t)
    (ses-begin-change)
    (ses-set-parameter 'ses--col-printers newval col)
    (save-excursion
      (dotimes (row ses--numrows)
	(ses-print-cell row col)))))

(defun ses-read-default-printer (newval)
  "Set the default printer function for cells that have no other.
See `ses-read-cell-printer' for input forms."
  (interactive
   (list (ses-read-printer "Default printer: " ses--default-printer)))
  (unless (eq newval t)
    (ses-begin-change)
    (ses-set-parameter 'ses--default-printer newval)
    (ses-reprint-all t)))


;;----------------------------------------------------------------------------
;; Spreadsheet size adjustments
;;----------------------------------------------------------------------------

(defun ses-insert-row (count)
  "Insert a new row before the current one.
With prefix, insert COUNT rows before current one."
  (interactive "*p")
  (ses-check-curcell 'end)
  (or (> count 0) (signal 'args-out-of-range nil))
  (ses-begin-change)
  (let ((inhibit-quit t)
	(inhibit-read-only t)
	(row (or (car (ses-sym-rowcol ses--curcell)) ses--numrows))
	newrow)
    ;;Create a new set of cell-variables
    (ses-create-cell-variable-range ses--numrows (+ ses--numrows count -1)
				    0            (1- ses--numcols))
    (ses-set-parameter 'ses--numrows (+ ses--numrows count))
    ;;Insert each row
    (ses-goto-print row 0)
    (dotimes-with-progress-reporter (x count) "Inserting row..."
      ;;Create a row of empty cells.  The `symbol' fields will be set by
      ;;the call to ses-relocate-all.
      (setq newrow (make-vector ses--numcols nil))
      (dotimes (col ses--numcols)
	(aset newrow col (ses-make-cell)))
      (setq ses--cells (ses-vector-insert ses--cells row newrow))
      (push `(apply ses-vector-delete ses--cells ,row 1) buffer-undo-list)
      (insert ses--blank-line))
    ;;Insert empty lines in cell data area (will be replaced by
    ;;ses-relocate-all)
    (ses-goto-data row 0)
    (insert (make-string (* (1+ ses--numcols) count) ?\n))
    (ses-relocate-all row 0 count 0)
    ;;If any cell printers insert constant text, insert that text
    ;;into the line.
    (let ((cols   (mapconcat #'ses-call-printer ses--col-printers nil))
	  (global (ses-call-printer ses--default-printer)))
      (if (or (> (length cols) 0) (> (length global) 0))
	  (dotimes (x count)
	    (dotimes (col ses--numcols)
	      ;;These cells are always nil, only constant formatting printed
	      (1value (ses-print-cell (+ x row) col))))))
    (when (> ses--header-row row)
      ;;Inserting before header
      (ses-set-parameter 'ses--header-row (+ ses--header-row count))
      (ses-reset-header-string)))
  ;;Reconstruct text attributes
  (ses-setup)
  ;;Prepare for undo
  (push '(apply ses-widen) buffer-undo-list)
  ;;Return to current cell
  (if ses--curcell
      (ses-jump-safe ses--curcell)
    (ses-goto-print (1- ses--numrows) 0)))

(defun ses-delete-row (count)
  "Delete the current row.
With prefix, deletes COUNT rows starting from the current one."
  (interactive "*p")
  (ses-check-curcell)
  (or (> count 0) (signal 'args-out-of-range nil))
  (let ((inhibit-quit t)
	(inhibit-read-only t)
	(row (car (ses-sym-rowcol ses--curcell))))
    (setq count (min count (- ses--numrows row)))
    (ses-begin-change)
    (ses-set-parameter 'ses--numrows (- ses--numrows count))
    ;;Delete lines from print area
    (ses-goto-print row 0)
    (ses-delete-line count)
    ;;Delete lines from cell data area
    (ses-goto-data row 0)
    (ses-delete-line (* count (1+ ses--numcols)))
    ;;Relocate variables and formulas
    (ses-set-with-undo 'ses--cells (ses-vector-delete ses--cells row count))
    (ses-relocate-all row 0 (- count) 0)
    (ses-destroy-cell-variable-range ses--numrows (+ ses--numrows count -1)
				     0            (1- ses--numcols))
    (when (> ses--header-row row)
      (if (<= ses--header-row (+ row count))
	  ;;Deleting the header row
	  (ses-set-parameter 'ses--header-row 0)
	(ses-set-parameter 'ses--header-row (- ses--header-row count)))
      (ses-reset-header-string)))
  ;;Reconstruct attributes
  (ses-setup)
  ;;Prepare for undo
  (push '(apply ses-widen) buffer-undo-list)
  (ses-jump-safe ses--curcell))

(defun ses-insert-column (count &optional col width printer)
  "Insert a new column before COL (default is the current one).
With prefix, insert COUNT columns before current one.
If COL is specified, the new column(s) get the specified WIDTH and PRINTER
\(otherwise they're taken from the current column)."
  (interactive "*p")
  (ses-check-curcell)
  (or (> count 0) (signal 'args-out-of-range nil))
  (or col
      (setq col     (cdr (ses-sym-rowcol ses--curcell))
	    width   (ses-col-width col)
	    printer (ses-col-printer col)))
  (ses-begin-change)
  (let ((inhibit-quit t)
	(inhibit-read-only t)
	(widths   ses--col-widths)
	(printers ses--col-printers)
	has-skip)
    ;;Create a new set of cell-variables
    (ses-create-cell-variable-range 0            (1- ses--numrows)
				    ses--numcols (+ ses--numcols count -1))
    ;;Insert each column.
    (dotimes-with-progress-reporter (x count) "Inserting column..."
      ;;Create a column of empty cells.  The `symbol' fields will be set by
      ;;the call to ses-relocate-all.
      (ses-adjust-print-width col (1+ width))
      (ses-set-parameter 'ses--numcols (1+ ses--numcols))
      (dotimes (row ses--numrows)
	(and (< (1+ col) ses--numcols) (eq (ses-cell-value row col) '*skip*)
	     ;;Inserting in the middle of a spill-over
	     (setq has-skip t))
	(ses-aset-with-undo ses--cells row
			    (ses-vector-insert (aref ses--cells row)
					       col (ses-make-cell)))
	;;Insert empty lines in cell data area (will be replaced by
	;;ses-relocate-all)
	(ses-goto-data row col)
	(insert ?\n))
      ;; Insert column width and printer.
      (setq widths      (ses-vector-insert widths col width)
	    printers    (ses-vector-insert printers col printer)))
    (ses-set-parameter 'ses--col-widths widths)
    (ses-set-parameter 'ses--col-printers printers)
    (ses-reset-header-string)
    (ses-relocate-all 0 col 0 count)
    (if has-skip
	(ses-reprint-all t)
      (when (or (> (length (ses-call-printer printer)) 0)
		(> (length (ses-call-printer ses--default-printer)) 0))
	;; Either column printer or global printer inserts some constant text.
	;; Reprint the new columns to insert that text.
	(dotimes (x ses--numrows)
	  (dotimes (y count)
	    ;; Always nil here --- this is a blank column.
	    (1value (ses-print-cell-new-width x (+ y col))))))
      (ses-setup)))
  (ses-jump-safe ses--curcell))

(defun ses-delete-column (count)
  "Delete the current column.
With prefix, deletes COUNT columns starting from the current one."
  (interactive "*p")
  (ses-check-curcell)
  (or (> count 0) (signal 'args-out-of-range nil))
  (let ((inhibit-quit t)
	(inhibit-read-only t)
	(rowcol  (ses-sym-rowcol ses--curcell))
	(width 0)
	col origrow has-skip)
    (setq origrow (car rowcol)
	  col     (cdr rowcol)
	  count   (min count (- ses--numcols col)))
    (if (= count ses--numcols)
	(error "Can't delete all columns!"))
    ;;Determine width of column(s) being deleted
    (dotimes (x count)
      (setq width (+ width (ses-col-width (+ col x)) 1)))
    (ses-begin-change)
    (ses-set-parameter 'ses--numcols (- ses--numcols count))
    (ses-adjust-print-width col (- width))
    (dotimes-with-progress-reporter (row ses--numrows) "Deleting column..."
      ;;Delete lines from cell data area
      (ses-goto-data row col)
      (ses-delete-line count)
      ;;Delete cells.  Check if deletion area begins or ends with a skip.
      (if (or (eq (ses-cell-value row col) '*skip*)
	      (and (< col ses--numcols)
		   (eq (ses-cell-value row (+ col count)) '*skip*)))
	  (setq has-skip t))
      (ses-aset-with-undo ses--cells row
			  (ses-vector-delete (aref ses--cells row) col count)))
    ;;Update globals
    (ses-set-parameter 'ses--col-widths
		       (ses-vector-delete ses--col-widths col count))
    (ses-set-parameter 'ses--col-printers
		       (ses-vector-delete ses--col-printers col count))
    (ses-reset-header-string)
    ;;Relocate variables and formulas
    (ses-relocate-all 0 col 0 (- count))
    (ses-destroy-cell-variable-range 0            (1- ses--numrows)
				     ses--numcols (+ ses--numcols count -1))
    (if has-skip
	(ses-reprint-all t)
      (ses-setup))
    (if (>= col ses--numcols)
	(setq col (1- col)))
    (ses-goto-print origrow col)))

(defun ses-forward-or-insert (&optional count)
  "Move to next cell in row, or inserts a new cell if already in last one, or
inserts a new row if at bottom of print area.  Repeat COUNT times."
  (interactive "p")
  (ses-check-curcell 'end)
  (setq deactivate-mark t) ; Doesn't combine well with ranges.
  (dotimes (x count)
    (ses-set-curcell)
    (if (not ses--curcell)
	(progn ; At bottom of print area.
	  (barf-if-buffer-read-only)
	  (ses-insert-row 1))
      (let ((col (cdr (ses-sym-rowcol ses--curcell))))
	(when (/= 32
		  (char-before (next-single-property-change (point)
							    'intangible)))
	  ;; We're already in last nonskipped cell on line.  Need to create a
	  ;; new column.
	  (barf-if-buffer-read-only)
	  (ses-insert-column (- count x)
			     ses--numcols
			     (ses-col-width col)
			     (ses-col-printer col)))))
    (forward-char)))

(defun ses-append-row-jump-first-column ()
  "Insert a new row after current one and jump to its first column."
  (interactive "*")
  (ses-check-curcell)
  (ses-begin-change)
  (beginning-of-line 2)
  (ses-set-curcell)
  (ses-insert-row 1))

(defun ses-set-column-width (col newwidth)
  "Set the width of the current column."
  (interactive
   (let ((col (cdr (progn (ses-check-curcell) (ses-sym-rowcol ses--curcell)))))
     (barf-if-buffer-read-only)
     (list col
	   (if current-prefix-arg
	       (prefix-numeric-value current-prefix-arg)
	     (read-from-minibuffer (format "Column %s width [currently %d]: "
					   (ses-column-letter col)
					   (ses-col-width col))
				   nil  ; No initial contents.
				   nil  ; No override keymap.
				   t    ; Convert to Lisp object.
				   nil  ; No history.
				   (number-to-string
				    (ses-col-width col))))))) ; Default value.
  (if (< newwidth 1)
      (error "Invalid column width"))
  (ses-begin-change)
  (ses-reset-header-string)
  (save-excursion
    (let ((inhibit-quit t))
      (ses-adjust-print-width col (- newwidth (ses-col-width col)))
      (ses-set-parameter 'ses--col-widths newwidth col))
    (dotimes (row ses--numrows)
      (ses-print-cell-new-width row col))))


;;----------------------------------------------------------------------------
;; Cut and paste, import and export
;;----------------------------------------------------------------------------

(defadvice copy-region-as-kill (around ses-copy-region-as-kill
				activate preactivate)
  "It doesn't make sense to copy read-only or intangible attributes into the
kill ring.  It probably doesn't make sense to copy keymap properties.
We'll assume copying front-sticky properties doesn't make sense, either.

This advice also includes some SES-specific code because otherwise it's too
hard to override how mouse-1 works."
  (when (> beg end)
    (let ((temp beg))
      (setq beg end
	    end temp)))
  (if (not (and (eq major-mode 'ses-mode)
		(eq (get-text-property beg 'read-only) 'ses)
		(eq (get-text-property (1- end) 'read-only) 'ses)))
      ad-do-it ; Normal copy-region-as-kill.
    (kill-new (ses-copy-region beg end))
    (if transient-mark-mode
	(setq deactivate-mark t))
    nil))

(defun ses-copy-region (beg end)
  "Treat the region as rectangular.  Convert the intangible attributes to
SES attributes recording the contents of the cell as of the time of copying."
  (when (= end ses--data-marker)
    ;;Avoid overflow situation
    (setq end (1- ses--data-marker)))
  (let* ((inhibit-point-motion-hooks t)
	 (x (mapconcat 'ses-copy-region-helper
		       (extract-rectangle beg (1- end)) "\n")))
    (remove-text-properties 0 (length x)
			    '(read-only t
			      intangible t
			      keymap t
			      front-sticky t)
			    x)
    x))

(defun ses-copy-region-helper (line)
  "Converts one line (of a rectangle being extracted from a spreadsheet) to
external form by attaching to each print cell a 'ses attribute that records
the corresponding data cell."
  (or (> (length line) 1)
      (error "Empty range"))
  (let ((inhibit-read-only t)
	(pos 0)
	mycell next sym rowcol)
    (while pos
      (setq sym    (get-text-property pos 'intangible line)
	    next   (next-single-property-change pos 'intangible line)
	    rowcol (ses-sym-rowcol sym)
	    mycell (ses-get-cell (car rowcol) (cdr rowcol)))
      (put-text-property pos (or next (length line))
			 'ses
			 (list (ses-cell-symbol  mycell)
			       (ses-cell-formula mycell)
			       (ses-cell-printer mycell))
			 line)
      (setq pos next)))
  line)

(defun ses-kill-override (beg end)
  "Generic override for any commands that kill text.
We clear the killed cells instead of deleting them."
  (interactive "r")
  (ses-check-curcell 'needrange)
  ;; For some reason, the text-read-only error is not caught by `delete-region',
  ;; so we have to use subterfuge.
  (let ((buffer-read-only t))
    (1value (condition-case x
		(noreturn (funcall (lookup-key (current-global-map)
					       (this-command-keys))
				   beg end))
	      (buffer-read-only nil)))) ; The expected error.
  ;; Because the buffer was marked read-only, the kill command turned itself
  ;; into a copy.  Now we clear the cells or signal the error.  First we check
  ;; whether the buffer really is read-only.
  (barf-if-buffer-read-only)
  (ses-begin-change)
  (ses-dorange ses--curcell
    (ses-clear-cell row col))
  (ses-jump (car ses--curcell)))

(defadvice yank (around ses-yank activate preactivate)
  "In SES mode, the yanked text is inserted as cells.

If the text contains 'ses attributes (meaning it went to the kill-ring from a
SES buffer), the formulas and print functions are restored for the cells.  If
the text contains tabs, this is an insertion of tab-separated formulas.
Otherwise the text is inserted as the formula for the current cell.

When inserting cells, the formulas are usually relocated to keep the same
relative references to neighboring cells.  This is best if the formulas
generally refer to other cells within the yanked text.  You can use the C-u
prefix to specify insertion without relocation, which is best when the
formulas refer to cells outside the yanked text.

When inserting formulas, the text is treated as a string constant if it doesn't
make sense as a sexp or would otherwise be considered a symbol.  Use 'sym to
explicitly insert a symbol, or use the C-u prefix to treat all unmarked words
as symbols."
  (if (not (and (eq major-mode 'ses-mode)
		(eq (get-text-property (point) 'keymap) 'ses-mode-print-map)))
      ad-do-it ; Normal non-SES yank.
    (ses-check-curcell 'end)
    (push-mark (point))
    (let ((text (current-kill (cond
			       ((listp arg)  0)
			       ((eq arg '-)  -1)
			       (t            (1- arg))))))
      (or (ses-yank-cells text arg)
	  (ses-yank-tsf text arg)
	  (ses-yank-one (ses-yank-resize 1 1)
			text
			0
			(if (memq (aref text (1- (length text))) '(?\t ?\n))
			    ;; Just one cell --- delete final tab or newline.
			    (1- (length text)))
			arg)))
    (if (consp arg)
	(exchange-point-and-mark))))

(defun ses-yank-pop (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop',
when the region contains a stretch of reinserted previously-killed text.
We replace it with a different stretch of killed text.
  Unlike standard `yank-pop', this function uses `undo' to delete the
previous insertion."
  (interactive "*p")
  (or (eq last-command 'yank)
      ;;Use noreturn here just to avoid a "poor-coverage" warning in its
      ;;macro definition.
      (noreturn (error "Previous command was not a yank")))
  (undo)
  (ses-set-curcell)
  (yank (1+ (or arg 1)))
  (setq this-command 'yank))

(defun ses-yank-cells (text arg)
  "If the TEXT has a proper set of 'ses attributes, insert the text as
cells, else return nil.  The cells are reprinted--the supplied text is
ignored because the column widths, default printer, etc. at yank time might
be different from those at kill-time.  ARG is a list to indicate that
formulas are to be inserted without relocation."
  (let ((first (get-text-property 0 'ses text))
	(last  (get-text-property (1- (length text)) 'ses text)))
    (when (and first last) ;;Otherwise not proper set of attributes
      (setq first    (ses-sym-rowcol (car first))
	    last     (ses-sym-rowcol (car last)))
      (let* ((needrows (- (car last) (car first) -1))
	     (needcols (- (cdr last) (cdr first) -1))
	     (rowcol   (ses-yank-resize needrows needcols))
	     (rowincr  (- (car rowcol) (car first)))
	     (colincr  (- (cdr rowcol) (cdr first)))
	     (pos      0)
	     myrow mycol x)
	(dotimes-with-progress-reporter (row needrows) "Yanking..."
	  (setq myrow (+ row (car rowcol)))
	  (dotimes (col needcols)
	    (setq mycol (+ col (cdr rowcol))
		  last (get-text-property pos 'ses text)
		  pos  (next-single-property-change pos 'ses text)
		  x    (ses-sym-rowcol (car last)))
	    (if (not last)
		;; Newline --- all remaining cells on row are skipped.
		(setq x   (cons (- myrow rowincr) (+ needcols colincr -1))
		      last (list nil nil nil)
		      pos  (1- pos)))
	    (if (/= (car x) (- myrow rowincr))
		(error "Cell row error"))
	    (if (< (- mycol colincr) (cdr x))
		;; Some columns were skipped.
		(let ((oldcol mycol))
		  (while (< (- mycol colincr) (cdr x))
		    (ses-clear-cell myrow mycol)
		    (setq col   (1+ col)
			  mycol (1+ mycol)))
		  (ses-print-cell myrow (1- oldcol)))) ;; This inserts *skip*.
	    (when (car last) ; Skip this for *skip* cells.
	      (setq x (nth 2 last))
	      (unless (equal x (ses-cell-printer myrow mycol))
		(or (not x)
		    (stringp x)
		    (eq (car-safe x) 'ses-safe-printer)
		    (setq x `(ses-safe-printer ,x)))
		(ses-set-cell myrow mycol 'printer x))
	      (setq x (cadr last))
	      (if (atom arg)
		  (setq x (ses-relocate-formula x 0 0 rowincr colincr)))
	      (or (atom x)
		  (eq (car-safe x) 'ses-safe-formula)
		  (setq x `(ses-safe-formula ,x)))
	      (ses-cell-set-formula myrow mycol x)))
	  (when pos
	    (if (get-text-property pos 'ses text)
		(error "Missing newline between rows"))
	    (setq pos (next-single-property-change pos 'ses text))))
	t))))

(defun ses-yank-one (rowcol text from to arg)
  "Insert the substring [FROM,TO] of TEXT as the formula for cell ROWCOL (a
cons of ROW and COL).  Treat plain symbols as strings unless ARG is a list."
  (let ((val (condition-case nil
		 (read-from-string text from to)
	       (error (cons nil from)))))
    (cond
     ((< (cdr val) (or to (length text)))
      ;; Invalid sexp --- leave it as a string.
      (setq val (substring text from to)))
     ((and (car val) (symbolp (car val)))
      (if (consp arg)
	  (setq val (list 'quote (car val)))  ; Keep symbol.
	(setq val (substring text from to)))) ; Treat symbol as text.
     (t
      (setq val (car val))))
    (let ((row (car rowcol))
	  (col (cdr rowcol)))
      (or (atom val)
	  (setq val `(ses-safe-formula ,val)))
      (ses-cell-set-formula row col val))))

(defun ses-yank-tsf (text arg)
  "If TEXT contains tabs and/or newlines, treat the tabs as
column-separators and the newlines as row-separators and insert the text as
cell formulas--else return nil.  Treat plain symbols as strings unless ARG
is a list.  Ignore a final newline."
  (if (or (not (string-match "[\t\n]" text))
	  (= (match-end 0) (length text)))
      ;;Not TSF format
      nil
    (if (/= (aref text (1- (length text))) ?\n)
	(setq text (concat text "\n")))
    (let ((pos      -1)
	  (spots    (list -1))
	  (cols     0)
	  (needrows 0)
	  needcols rowcol)
      ;;Find all the tabs and newlines
      (while (setq pos (string-match "[\t\n]" text (1+ pos)))
	(push pos spots)
	(setq cols (1+ cols))
	(when (eq (aref text pos) ?\n)
	  (if (not needcols)
	      (setq needcols cols)
	    (or (= needcols cols)
		(error "Inconsistent row lengths")))
	  (setq cols     0
		needrows (1+ needrows))))
      ;;Insert the formulas
      (setq rowcol (ses-yank-resize needrows needcols))
      (dotimes (row needrows)
	(dotimes (col needcols)
	  (ses-yank-one (cons (+ (car rowcol) needrows (- row) -1)
			      (+ (cdr rowcol) needcols (- col) -1))
			text (1+ (cadr spots)) (car spots) arg)
	  (setq spots (cdr spots))))
      (ses-goto-print (+ (car rowcol) needrows -1)
		      (+ (cdr rowcol) needcols -1))
      t)))

(defun ses-yank-resize (needrows needcols)
  "If this yank will require inserting rows and/or columns, ask for
confirmation and then insert them.  Result is (row,col) for top left of yank
spot, or error signal if user requests cancel."
  (ses-begin-change)
  (let ((rowcol (if ses--curcell
		    (ses-sym-rowcol ses--curcell)
		  (cons ses--numrows 0)))
	rowbool colbool)
    (setq needrows (- (+ (car rowcol) needrows) ses--numrows)
	  needcols (- (+ (cdr rowcol) needcols) ses--numcols)
	  rowbool  (> needrows 0)
	  colbool  (> needcols 0))
    (when (or rowbool colbool)
      ;;Need to insert.  Get confirm
      (or (y-or-n-p (format "Yank will insert %s%s%s.  Continue? "
			    (if rowbool (format "%d rows" needrows) "")
			    (if (and rowbool colbool) " and " "")
			    (if colbool (format "%d columns" needcols) "")))
	  (error "Cancelled"))
      (when rowbool
	(let (ses--curcell)
	  (save-excursion
	    (ses-goto-print ses--numrows 0)
	    (ses-insert-row needrows))))
      (when colbool
	  (ses-insert-column needcols
			     ses--numcols
			     (ses-col-width (1- ses--numcols))
			     (ses-col-printer (1- ses--numcols)))))
    rowcol))

(defun ses-export-tsv (beg end)
  "Export values from the current range, with tabs between columns and
newlines between rows.  Result is placed in kill ring."
  (interactive "r")
  (ses-export-tab nil))

(defun ses-export-tsf (beg end)
  "Export formulas from the current range, with tabs between columns and
newlines between rows.  Result is placed in kill ring."
  (interactive "r")
  (ses-export-tab t))

(defun ses-export-tab (want-formulas)
  "Export the current range with tabs between columns and newlines between rows.
Result is placed in kill ring.  The export is values unless WANT-FORMULAS
is non-nil.  Newlines and tabs in the export text are escaped."
  (ses-check-curcell 'needrange)
  (let ((print-escape-newlines t)
	result item)
    (ses-dorange ses--curcell
      (setq item (if want-formulas
		     (ses-cell-formula row col)
		   (ses-cell-value row col)))
      (if (eq (car-safe item) 'ses-safe-formula)
	  ;;Hide our deferred safety-check marker
	  (setq item (cadr item)))
      (if (or (not item) (eq item '*skip*))
	  (setq item ""))
      (when (eq (car-safe item) 'quote)
	(push "'" result)
	(setq item (cadr item)))
      (setq item (prin1-to-string item t))
      (setq item (replace-regexp-in-string "\t" "\\\\t" item))
      (push item result)
      (cond
       ((< col maxcol)
	(push "\t" result))
       ((< row maxrow)
	(push "\n" result))))
    (setq result (apply 'concat (nreverse result)))
    (kill-new result)))


;;----------------------------------------------------------------------------
;; Other user commands
;;----------------------------------------------------------------------------

(defun ses-unset-header-row ()
  "Select the default header row."
  (interactive)
  (ses-set-header-row 0))

(defun ses-set-header-row (row)
  "Set the ROW to display in the header-line.
With a numerical prefix arg, use that row.
With no prefix arg, use the current row.
With a \\[universal-argument] prefix arg, prompt the user.
The top row is row 1.  Selecting row 0 displays the default header row."
  (interactive
   (list (if (numberp current-prefix-arg) current-prefix-arg
	   (let ((currow (1+ (car (ses-sym-rowcol ses--curcell)))))
	     (if current-prefix-arg
		 (read-number "Header row: " currow)
	       currow)))))
  (if (or (< row 0) (> row ses--numrows))
      (error "Invalid header-row"))
  (ses-begin-change)
  (let ((oldval ses--header-row))
    (let (buffer-undo-list)
      (ses-set-parameter 'ses--header-row row))
    (push `(apply ses-set-header-row ,oldval) buffer-undo-list))
  (ses-reset-header-string))

(defun ses-mark-row ()
  "Mark the entirety of current row as a range."
  (interactive)
  (ses-check-curcell 'range)
  (let ((row (car (ses-sym-rowcol (or (car-safe ses--curcell) ses--curcell)))))
    (push-mark (point))
    (ses-goto-print (1+ row) 0)
    (push-mark (point) nil t)
    (ses-goto-print row 0)))

(defun ses-mark-column ()
  "Mark the entirety of current column as a range."
  (interactive)
  (ses-check-curcell 'range)
  (let ((col (cdr (ses-sym-rowcol (or (car-safe ses--curcell) ses--curcell))))
	(row 0))
    (push-mark (point))
    (ses-goto-print (1- ses--numrows) col)
    (forward-char 1)
    (push-mark (point) nil t)
    (while (eq '*skip* (ses-cell-value row col))
      ;;Skip over initial cells in column that can't be selected
      (setq row (1+ row)))
    (ses-goto-print row col)))

(defun ses-end-of-line ()
  "Move point to last cell on line."
  (interactive)
  (ses-check-curcell 'end 'range)
  (when ses--curcell  ; Otherwise we're at the bottom row, which is empty
		      ; anyway.
    (let ((col (1- ses--numcols))
	  row rowcol)
      (if (symbolp ses--curcell)
	  ;; Single cell.
	  (setq row (car (ses-sym-rowcol ses--curcell)))
	;; Range --- use whichever end of the range the point is at.
	(setq rowcol (ses-sym-rowcol (if (< (point) (mark))
				     (car ses--curcell)
				   (cdr ses--curcell))))
	;; If range already includes the last cell in a row, point is actually
	;; in the following row.
	(if (<= (cdr rowcol) (1- col))
	    (setq row (car rowcol))
	  (setq row (1+ (car rowcol)))
	  (if (= row ses--numrows)
	      ;;Already at end - can't go anywhere
	      (setq col 0))))
      (when (< row ses--numrows) ; Otherwise it's a range that includes last cell.
	(while (eq (ses-cell-value row col) '*skip*)
	  ;; Back to beginning of multi-column cell.
	  (setq col (1- col)))
	(ses-goto-print row col)))))

(defun ses-renarrow-buffer ()
  "Narrow the buffer so only the print area is visible.
Use after \\[widen]."
  (interactive)
  (setq ses--deferred-narrow t))

(defun ses-sort-column (sorter &optional reverse)
  "Sort the range by a specified column.
With prefix, sorts in REVERSE order."
  (interactive "*sSort column: \nP")
  (ses-check-curcell 'needrange)
  (let ((min (ses-sym-rowcol (car ses--curcell)))
	(max (ses-sym-rowcol (cdr ses--curcell))))
    (let ((minrow (car min))
	  (mincol (cdr min))
	  (maxrow (car max))
	  (maxcol (cdr max))
	  keys extracts end)
      (setq sorter (cdr (ses-sym-rowcol (intern (concat sorter "1")))))
      (or (and sorter (>= sorter mincol) (<= sorter maxcol))
	  (error "Invalid sort column"))
      ;;Get key columns and sort them
      (dotimes (x (- maxrow minrow -1))
	(ses-goto-print (+ minrow x) sorter)
	(setq end (next-single-property-change (point) 'intangible))
	(push (cons (buffer-substring-no-properties (point) end)
		    (+ minrow x))
	      keys))
      (setq keys (sort keys #'(lambda (x y) (string< (car x) (car y)))))
      ;;Extract the lines in reverse sorted order
      (or reverse
	  (setq keys (nreverse keys)))
      (dolist (x keys)
	(ses-goto-print (cdr x) (1+ maxcol))
	(setq end (point))
	(ses-goto-print (cdr x) mincol)
	(push (ses-copy-region (point) end) extracts))
      (deactivate-mark)
      ;;Paste the lines sequentially
      (dotimes (x (- maxrow minrow -1))
	(ses-goto-print (+ minrow x) mincol)
	(ses-set-curcell)
	(ses-yank-cells (pop extracts) nil)))))

(defun ses-sort-column-click (event reverse)
  "Mouse version of `ses-sort-column'."
  (interactive "*e\nP")
  (setq event (event-end event))
  (select-window (posn-window event))
  (setq event (car (posn-col-row event))) ; Click column.
  (let ((col 0))
    (while (and (< col ses--numcols) (> event (ses-col-width col)))
      (setq event (- event (ses-col-width col) 1)
	    col   (1+ col)))
    (if (>= col ses--numcols)
	(ding)
      (ses-sort-column (ses-column-letter col) reverse))))

(defun ses-insert-range ()
  "Insert into minibuffer the list of cells currently highlighted in the
spreadsheet."
  (interactive "*")
  (let (x)
    (with-current-buffer (window-buffer minibuffer-scroll-window)
      (ses-command-hook)  ; For ses-coverage.
      (ses-check-curcell 'needrange)
      (setq x (cdr (macroexpand `(ses-range ,(car ses--curcell)
					    ,(cdr ses--curcell))))))
    (insert (substring (prin1-to-string (nreverse x)) 1 -1))))

(defun ses-insert-ses-range ()
  "Insert \"(ses-range x y)\" in the minibuffer to represent the currently
highlighted range in the spreadsheet."
  (interactive "*")
  (let (x)
    (with-current-buffer (window-buffer minibuffer-scroll-window)
      (ses-command-hook)  ; For ses-coverage.
      (ses-check-curcell 'needrange)
      (setq x (format "(ses-range %S %S)"
		      (car ses--curcell)
		      (cdr ses--curcell))))
    (insert x)))

(defun ses-insert-range-click (event)
  "Mouse version of `ses-insert-range'."
  (interactive "*e")
  (mouse-set-point event)
  (ses-insert-range))

(defun ses-insert-ses-range-click (event)
  "Mouse version of `ses-insert-ses-range'."
  (interactive "*e")
  (mouse-set-point event)
  (ses-insert-ses-range))


;;----------------------------------------------------------------------------
;; Checking formulas for safety
;;----------------------------------------------------------------------------

(defun ses-safe-printer (printer)
  "Return PRINTER if safe, or the substitute printer `ses-unsafe' otherwise."
  (if (or (stringp printer)
	  (stringp (car-safe printer))
	  (not printer)
	  (ses-warn-unsafe printer 'unsafep-function))
      printer
    'ses-unsafe))

(defun ses-safe-formula (formula)
  "Return FORMULA if safe, or the substitute formula *unsafe* otherwise."
  (if (ses-warn-unsafe formula 'unsafep)
      formula
    `(ses-unsafe ',formula)))

(defun ses-warn-unsafe (formula checker)
  "Apply CHECKER to FORMULA.
If result is non-nil, asks user for confirmation about FORMULA,
which might be unsafe.  Returns t if formula is safe or user allows
execution anyway.  Always returns t if `safe-functions' is t."
  (if (eq safe-functions t)
      t
    (setq checker (funcall checker formula))
    (if (not checker)
	t
      (y-or-n-p (format "Formula %S\nmight be unsafe %S.  Process it? "
			formula checker)))))


;;----------------------------------------------------------------------------
;; Standard formulas
;;----------------------------------------------------------------------------

(defun ses--clean-! (&rest x)
  "Clean by `delq' list X from any occurrence of `nil' or `*skip*'."
  (delq nil (delq '*skip* x)))

(defun ses--clean-_ (x y)
  "Clean list X  by replacing by Y any occurrence of `nil' or `*skip*'.

This will change X by making `setcar' on its cons cells."
  (let ((ret x) ret-elt)
    (while ret
      (setq ret-elt (car ret))
      (when (memq ret-elt '(nil *skip*))
	(setcar ret y))
      (setq ret (cdr ret))))
  x)

(defmacro ses-range (from to &rest rest)
  "Expand to a list of cell-symbols for the range going from
FROM up to TO.  The range automatically expands to include any
new row or column inserted into its middle.  The SES library code
specifically looks for the symbol `ses-range', so don't create an
alias for this macro!

By passing in REST some flags one can configure the way the range
is read and how it is formatted.

In the sequel we assume that cells A1, B1, A2 B2 have respective values
1 2 3 and 4.

Readout direction is specified by a `>v', '`>^', `<v', `<^',
`v>', `v<', `^>', `^<' flag.  For historical reasons, in absence
of such a flag, a default direction of `^<' is assumed.  This
way `(ses-range A1 B2 ^>)' will evaluate to `(1 3 2 4)',
while `(ses-range A1 B2 >^)' will evaluate to (3 4 1 2).

If the range is one row, then `>' can be used as a shorthand to
`>v' or `>^', and `<' to `<v' or `<^'.

If the range is one column, then `v' can be used as a shorthand to
`v>' or `v<', and `^' to `^>' or `v<'.

A `!' flag will remove all cells whose value is nil or `*skip*'.

A `_' flag will replace nil or `*skip*' by the value following
the `_' flag.  If the `_' flag is the last argument, then they are
replaced by integer 0.

A `*', `*1' or `*2' flag will vectorize the range in the sense of
Calc.  See info node `(Calc) Top'.  Flag `*' will output either a
vector or a matrix depending on the number of rows, `*1' will
flatten the result to a one row vector, and `*2' will make a
matrix whatever the number of rows.

Warning: interaction with Calc is experimental and may produce
confusing results if you are not aware of Calc data format.
Use `math-format-value' as a printer for Calc objects."
  (let (result-row
	result
	(prev-row -1)
	(reorient-x nil)
	(reorient-y nil)
	transpose vectorize
	(clean 'list))
    (ses-dorange (cons from to)
      (when (/= prev-row row)
	(push result-row result)
	(setq result-row nil))
      (push (ses-cell-symbol row col) result-row)
      (setq prev-row row))
    (push result-row result)
    (while rest
      (let ((x (pop rest)))
	(case x
	  ((>v) (setq transpose nil reorient-x nil reorient-y nil))
	  ((>^)(setq transpose nil reorient-x nil reorient-y t))
	  ((<^)(setq transpose nil reorient-x t reorient-y t))
	  ((<v)(setq transpose nil reorient-x t reorient-y nil))
	  ((v>)(setq transpose t reorient-x nil reorient-y t))
	  ((^>)(setq transpose t reorient-x nil reorient-y nil))
	  ((^<)(setq transpose t reorient-x t reorient-y nil))
	  ((v<)(setq transpose t reorient-x t reorient-y t))
	  ((* *2 *1) (setq vectorize x))
	  ((!) (setq clean 'ses--clean-!))
	  ((_) (setq clean `(lambda (&rest x) (ses--clean-_  x ,(if rest (pop rest) 0)))))
	  (t
	   (cond
					; shorthands one row
	    ((and (null (cddr result)) (memq x '(> <)))
	     (push (intern (concat (symbol-name x) "v")) rest))
					; shorthands one col
	    ((and (null (cdar result)) (memq x '(v ^)))
	     (push (intern (concat (symbol-name x) ">")) rest))
	    (t (error "Unexpected flag `%S' in ses-range" x)))))))
    (if reorient-y
	(setcdr (last result 2) nil)
      (setq result (cdr (nreverse result))))
    (unless reorient-x
      (setq result (mapcar 'nreverse result)))
    (when transpose
      (let ((ret (mapcar (lambda (x) (list x)) (pop result))) iter)
	(while result
	  (setq iter ret)
	  (dolist (elt (pop result))
	    (setcar iter (cons elt (car iter)))
	    (setq iter (cdr iter))))
	(setq result ret)))

    (flet ((vectorize-*1
	    (clean result)
	    (cons clean (cons (quote 'vec) (apply 'append result))))
	   (vectorize-*2
	    (clean result)
	    (cons clean (cons (quote 'vec) (mapcar (lambda (x)
						     (cons  clean (cons (quote 'vec) x)))
						   result)))))
      (case vectorize
	((nil) (cons clean (apply 'append result)))
	((*1) (vectorize-*1 clean result))
	((*2) (vectorize-*2 clean result))
	((*) (if (cdr result)
	       (vectorize-*2 clean result)
	     (vectorize-*1 clean result)))))))

(defun ses-delete-blanks (&rest args)
  "Return ARGS reversed, with the blank elements (nil and *skip*) removed."
  (let (result)
    (dolist (cur args)
      (unless (memq cur '(nil *skip*))
	(push cur result)))
    result))

(defun ses+ (&rest args)
  "Compute the sum of the arguments, ignoring blanks."
  (apply '+ (apply 'ses-delete-blanks args)))

(defun ses-average (list)
  "Computes the sum of the numbers in LIST, divided by their length.  Blanks
are ignored.  Result is always floating-point, even if all args are integers."
  (setq list (apply 'ses-delete-blanks list))
  (/ (float (apply '+ list)) (length list)))

(defmacro ses-select (fromrange test torange)
  "Select cells in FROMRANGE that are `equal' to TEST.
For each match, return the corresponding cell from TORANGE.
The ranges are macroexpanded but not evaluated so they should be
either (ses-range BEG END) or (list ...).  The TEST is evaluated."
  (setq fromrange (cdr (macroexpand fromrange))
	torange   (cdr (macroexpand torange))
	test      (eval test))
  (or (= (length fromrange) (length torange))
      (error "ses-select: Ranges not same length"))
  (let (result)
    (dolist (x fromrange)
      (if (equal test (symbol-value x))
	  (push (car torange) result))
      (setq torange (cdr torange)))
    (cons 'list result)))

;;All standard formulas are safe
(dolist (x '(ses-cell-value ses-range ses-delete-blanks ses+ ses-average
	     ses-select))
  (put x 'side-effect-free t))


;;----------------------------------------------------------------------------
;; Standard print functions
;;----------------------------------------------------------------------------

;; These functions use the variables 'row' and 'col' that are dynamically bound
;; by ses-print-cell.  We define these variables at compile-time to make the
;; compiler happy.
(defvar row)
(defvar col)

(defun ses-center (value &optional span fill)
  "Print VALUE, centered within column.
FILL is the fill character for centering (default = space).
SPAN indicates how many additional rightward columns to include
in width (default = 0)."
  (let ((printer (or (ses-col-printer col) ses--default-printer))
	(width   (ses-col-width col))
	half)
    (or fill (setq fill ?\s))
    (or span (setq span 0))
    (setq value (ses-call-printer printer value))
    (dotimes (x span)
      (setq width (+ width 1 (ses-col-width (+ col span (- x))))))
    ;; Set column width.
    (setq width (- width (string-width value)))
    (if (<= width 0)
	value ; Too large for field, anyway.
      (setq half (make-string (/ width 2) fill))
      (concat half value half
	      (if (> (% width 2) 0) (char-to-string fill))))))

(defun ses-center-span (value &optional fill)
  "Print VALUE, centered within the span that starts in the current column
and continues until the next nonblank column.
FILL specifies the fill character (default = space)."
  (let ((end (1+ col)))
    (while (and (< end ses--numcols)
		(memq (ses-cell-value row end) '(nil *skip*)))
      (setq end (1+ end)))
    (ses-center value (- end col 1) fill)))

(defun ses-dashfill (value &optional span)
  "Print VALUE centered using dashes.
SPAN indicates how many rightward columns to include in width (default = 0)."
  (ses-center value span ?-))

(defun ses-dashfill-span (value)
  "Print VALUE, centered using dashes within the span that starts in the
current column and continues until the next nonblank column."
  (ses-center-span value ?-))

(defun ses-tildefill-span (value)
  "Print VALUE, centered using tildes within the span that starts in the
current column and continues until the next nonblank column."
  (ses-center-span value ?~))

(defun ses-unsafe (value)
  "Substitute for an unsafe formula or printer."
  (error "Unsafe formula or printer"))

;;All standard printers are safe, including ses-unsafe!
(dolist (x (cons 'ses-unsafe ses-standard-printer-functions))
  (put x 'side-effect-free t))

(defun ses-unload-function ()
  "Unload the Simple Emacs Spreadsheet."
  (dolist (fun '(copy-region-as-kill yank))
    (ad-remove-advice fun 'around (intern (concat "ses-" (symbol-name fun))))
    (ad-update fun))
  ;; continue standard unloading
  nil)

(provide 'ses)

;;; ses.el ends here
