;;; ede/autoconf-edit.el --- Keymap for autoconf

;; Copyright (C) 1998-2000, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project

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
;;
;; Autoconf editing and modification support, and compatibility layer
;; for Emacses w/out autoconf mode built in.

;;; Code:
(require 'autoconf)
(declare-function ede-srecode-setup "ede/srecode")
(declare-function ede-srecode-insert "ede/srecode")

(defun autoconf-new-program (rootdir program testfile)
  "Initialize a new configure.in in ROOTDIR for PROGRAM using TESTFILE.
ROOTDIR is the root directory of a given autoconf controlled project.
PROGRAM is the program to be configured.
TESTFILE is the file used with AC_INIT.
configure the initial configure script using `autoconf-new-automake-string'"
  (interactive "DRoot Dir: \nsProgram: \nsTest File: ")
  (require 'ede/srecode)
  (if (bufferp rootdir)
      (set-buffer rootdir)
    (let ((cf1 (expand-file-name "configure.in" rootdir))
	  (cf2 (expand-file-name "configure.ac" rootdir)))
      (if (and (or (file-exists-p cf1) (file-exists-p cf2))
	       (not (y-or-n-p (format "File %s exists.  Start Over? "
				      (if (file-exists-p cf1)
					  cf1 cf2)
				      ))))
	  (error "Quit"))
      (find-file cf2)))
  ;; Note, we only ask about overwrite if a string/path is specified.
  (erase-buffer)
  (ede-srecode-setup)
  (ede-srecode-insert
   "file:ede-empty"
   "TEST_FILE" testfile
   "PROGRAM" program)
  )

(defvar autoconf-preferred-macro-order
  '("AC_INIT"
    "AM_INIT_AUTOMAKE"
    "AM_CONFIG_HEADER"
    ;; Arg parsing
    "AC_ARG_ENABLE"
    "AC_ARG_WITH"
    ;; Programs
    "AC_PROG_MAKE_SET"
    "AC_PROG_AWK"
    "AC_PROG_CC"
    "AC_PROG_CC_C_O"
    "AC_PROG_CPP"
    "AC_PROG_CXX"
    "AC_PROG_CXXCPP"
    "AC_ISC_POSIX"
    "AC_PROG_F77"
    "AC_PROG_GCC_TRADITIONAL"
    "AC_PROG_INSTALL"
    "AC_PROG_LEX"
    "AC_PROG_LN_S"
    "AC_PROG_RANLIB"
    "AC_PROG_YACC"
    "AC_CHECK_PROG"
    "AC_CHECK_PROGS"
    "AC_PROG_LIBTOOL"
    ;; Libraries
    "AC_CHECK_LIB"
    "AC_PATH_XTRA"
    ;; Headers
    "AC_HEADER_STDC"
    "AC_HEADER_SYS_WAIT"
    "AC_HEADER_TIME"
    "AC_HEADERS"
    ;; Typedefs, structures
    "AC_TYPE_PID_T"
    "AC_TYPE_SIGNAL"
    "AC_TYPE_UID_T"
    "AC_STRUCT_TM"
    ;; Compiler characteristics
    "AC_CHECK_SIZEOF"
    "AC_C_CONST"
    ;; Library functions
    "AC_CHECK_FUNCS"
    "AC_TRY_LINK"
    ;; System Services
    ;; Other
    "AM_PATH_LISPDIR"
    "AM_INIT_GUILE_MODULE"
    ;; AC_OUTPUT is always last
    "AC_OUTPUT"
    )
  "List of macros in the order that they prefer to occur in.
This helps when inserting a macro which doesn't yet exist
by positioning it near other macros which may exist.
From the autoconf manual:
     `AC_INIT(FILE)'
     checks for programs
     checks for libraries
     checks for header files
     checks for typedefs
     checks for structures
     checks for compiler characteristics
     checks for library functions
     checks for system services
     `AC_OUTPUT([FILE...])'")

(defvar autoconf-multiple-macros
  '("AC_ARG_ENABLE"
    "AC_ARG_WITH"
    "AC_CHECK_PROGS"
    "AC_CHECK_LIB"
    "AC_CHECK_SIZEOF"
    "AC_TRY_LINK"
    )
  "Macros which appear multiple times.")

(defvar autoconf-multiple-multiple-macros
  '("AC_HEADERS" "AC_CHECK_FUNCS")
  "Macros which appear multiple times, and perform multiple queries.")

(defun autoconf-in-macro (macro)
  "Non-nil if point is in a macro of type MACRO."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "\\(A[CM]_" macro "\\|" macro "\\)"))))

(defun autoconf-find-last-macro (macro &optional ignore-bol)
  "Move to the last occurrence of MACRO in FILE, and return that point.
The last macro is usually the one in which we would like to insert more
items such as CHECK_HEADERS."
  (let ((op (point)) (atbol (if ignore-bol "" "^")))
    (goto-char (point-max))
    (if (re-search-backward (concat atbol (regexp-quote macro) "\\s-*\\((\\|$\\)") nil t)
	(progn
	  (unless ignore-bol (beginning-of-line))
	  (point))
      (goto-char op)
      nil)))

(defun autoconf-parameter-strip (param)
  "Strip the parameter PARAM  of whitespace and miscellaneous characters."
  ;; force greedy match for \n.
  (when (string-match "\\`\n*\\s-*\\[?\\s-*" param)
    (setq param (substring param (match-end 0))))
  (when (string-match "\\s-*\\]?\\s-*\\'" param)
    (setq param (substring param 0  (match-beginning 0))))
  param)

(defun autoconf-parameters-for-macro (macro &optional ignore-bol ignore-case)
  "Retrieve the parameters to MACRO.
Returns a list of the arguments passed into MACRO as strings."
  (let ((case-fold-search ignore-case))
    (save-excursion
      (when (autoconf-find-last-macro macro ignore-bol)
	(forward-sexp 1)
	(mapcar
	 #'autoconf-parameter-strip
	 (when (looking-at "(")
	   (let* ((start (+ (point) 1))
		  (end (save-excursion
			 (forward-sexp 1)
			 (- (point) 1)))
		  (ans (buffer-substring-no-properties start end)))
	     (split-string ans "," t))))))))

(defun autoconf-position-for-macro (macro)
  "Position the cursor where a new MACRO could be inserted.
This will appear at the BEGINNING of the macro MACRO should appear AFTER.
This is to make it compatible with `autoconf-find-last-macro'.
Assume that MACRO doesn't appear in the buffer yet, so search
the ordering list `autoconf-preferred-macro-order'."
  ;; Search this list backwards.. heh heh heh
  ;; This lets us do a reverse search easily.
  (let ((ml (member macro (reverse autoconf-preferred-macro-order))))
    (if (not ml) (error "Don't know how to position for %s yet" macro))
    (setq ml (cdr ml))
    (goto-char (point-max))
    (while (and ml (not (autoconf-find-last-macro (car ml))))
      (setq ml (cdr ml)))
    (if (not ml) (error "Could not find context for positioning %s" macro))))

(defun autoconf-insert-macro-at-point (macro &optional param)
  "Add MACRO at the current point with PARAM."
  (insert macro)
  (if param
      (progn
	(insert "(" param ")")
	(if (< (current-column) 3) (insert " dnl")))))

(defun autoconf-insert-new-macro (macro &optional param)
  "Add a call to MACRO in the current autoconf file.
Deals with macro order.  See `autoconf-preferred-macro-order' and
`autoconf-multi-macros'.
Optional argument PARAM is the parameter to pass to the macro as one string."
  (cond ((member macro autoconf-multiple-macros)
	 ;; This occurs multiple times
	 (or (autoconf-find-last-macro macro)
	     (autoconf-position-for-macro macro))
	 (forward-sexp 2)
	 (end-of-line)
	 (insert "\n")
	 (autoconf-insert-macro-at-point macro param))
	((member macro autoconf-multiple-multiple-macros)
	 (if (not param)
	     (error "You must have a parameter for %s" macro))
	 (if (not (autoconf-find-last-macro macro))
	     (progn
	       ;; Doesn't exist yet....
	       (autoconf-position-for-macro macro)
	       (forward-sexp 2)
	       (end-of-line)
	       (insert "\n")
	       (autoconf-insert-macro-at-point macro param))
	   ;; Does exist, can we fit onto the current line?
	   (forward-sexp 2)
	   (down-list -1)
	   (if (> (+ (current-column) (length param))  fill-column)
	       (insert " " param)
	     (up-list 1)
	     (end-of-line)
	     (insert "\n")
	     (autoconf-insert-macro-at-point macro param))))
	((autoconf-find-last-macro macro)
	 ;; If it isn't one of the multi's, it's a singleton.
	 ;; If it exists, ignore it.
	 nil)
	(t
	 (autoconf-position-for-macro macro)
	 (forward-sexp 1)
	 (if (looking-at "\\s-*(")
	     (forward-sexp 1))
	 (end-of-line)
	 (insert "\n")
	 (autoconf-insert-macro-at-point macro param))))

(defun autoconf-find-query-for-header (header)
  "Position the cursor where HEADER is queried."
  (interactive "sHeader: ")
  (let ((op (point))
	(found t))
    (goto-char (point-min))
    (condition-case nil
	(while (not
		(progn
		  (re-search-forward
		   (concat "\\b" (regexp-quote header) "\\b"))
		  (save-excursion
		    (beginning-of-line)
		    (looking-at "AC_CHECK_HEADERS")))))
      ;; We depend on the search failing to exit our loop on failure.
      (error (setq found nil)))
    (if (not found) (goto-char op))
    found))

(defun autoconf-add-query-for-header (header)
  "Add in HEADER to be queried for in our autoconf file."
  (interactive "sHeader: ")
  (or (autoconf-find-query-for-header header)
      (autoconf-insert-new-macro "AC_CHECK_HEADERS" header)))


(defun autoconf-find-query-for-func (func)
  "Position the cursor where FUNC is queried."
  (interactive "sFunction: ")
  (let ((op (point))
	(found t))
    (goto-char (point-min))
    (condition-case nil
	(while (not
		(progn
		  (re-search-forward
		   (concat "\\b" (regexp-quote func) "\\b"))
		  (save-excursion
		    (beginning-of-line)
		    (looking-at "AC_CHECK_FUNCS")))))
      ;; We depend on the search failing to exit our loop on failure.
      (error (setq found nil)))
    (if (not found) (goto-char op))
    found))

(defun autoconf-add-query-for-func (func)
  "Add in FUNC to be queried for in our autoconf file."
  (interactive "sFunction: ")
  (or (autoconf-find-query-for-func func)
      (autoconf-insert-new-macro "AC_CHECK_FUNCS" func)))

(defvar autoconf-program-builtin
  '(("AWK" . "AC_PROG_AWK")
    ("CC" . "AC_PROG_CC")
    ("CPP" . "AC_PROG_CPP")
    ("CXX" . "AC_PROG_CXX")
    ("CXXCPP" . "AC_PROG_CXXCPP")
    ("F77" . "AC_PROG_F77")
    ("GCC_TRADITIONAL" . "AC_PROG_GCC_TRADITIONAL")
    ("INSTALL" . "AC_PROG_INSTALL")
    ("LEX" . "AC_PROG_LEX")
    ("LN_S" . "AC_PROG_LN_S")
    ("RANLIB" . "AC_PROG_RANLIB")
    ("YACC" . "AC_PROG_YACC")
    )
  "Association list of PROGRAM variables and their built-in MACRO.")

(defun autoconf-find-query-for-program (prog)
  "Position the cursor where PROG is queried.
PROG is the VARIABLE to use in autoconf to identify the program.
PROG excludes the _PROG suffix.  Thus if PROG were EMACS, then the
variable in configure.in would be EMACS_PROG."
  (let ((op (point))
	(found t)
	(builtin (assoc prog autoconf-program-builtin)))
    (goto-char (point-min))
    (condition-case nil
	(re-search-forward
	 (concat "^"
		 (or (cdr-safe builtin)
		     (concat "AC_CHECK_PROG\\s-*(\\s-*" prog "_PROG"))
		 "\\>"))
      (error (setq found nil)))
    (if (not found) (goto-char op))
    found))

(defun autoconf-add-query-for-program (prog &optional names)
  "Add in PROG to be queried for in our autoconf file.
Optional NAMES is for non-built-in programs, and is the list
of possible names."
  (interactive "sProgram: ")
  (if (autoconf-find-query-for-program prog)
      nil
    (let ((builtin (assoc prog autoconf-program-builtin)))
      (if builtin
	  (autoconf-insert-new-macro (cdr builtin))
	;; Not built in, try the params item
	(autoconf-insert-new-macro "AC_CHECK_PROGS" (concat prog "," names))
	))))

;;; Scrappy little changes
;;
(defvar autoconf-deleted-text nil
  "Set to the last bit of text deleted during an edit.")

(defvar autoconf-inserted-text nil
  "Set to the last bit of text inserted during an edit.")

(defmacro autoconf-edit-cycle (&rest body)
  "Start an edit cycle, unsetting the modified flag if there is no change.
Optional argument BODY is the code to execute which edits the autoconf file."
  `(let ((autoconf-deleted-text nil)
	 (autoconf-inserted-text nil)
	 (mod (buffer-modified-p)))
     ,@body
     (if (and (not mod)
	      (string= autoconf-deleted-text autoconf-inserted-text))
	 (set-buffer-modified-p nil))))

(defun autoconf-delete-parameter (index)
  "Delete the INDEXth parameter from the macro starting on the current line.
Leaves the cursor where a new parameter can be inserted.
INDEX starts at 1."
  (beginning-of-line)
  (down-list 1)
  (re-search-forward ", ?" nil nil (1- index))
  (let ((end (save-excursion
	       (re-search-forward ",\\|)" (point-at-eol))
	       (forward-char -1)
	       (point))))
    (setq autoconf-deleted-text (buffer-substring (point) end))
    (delete-region (point) end)))

(defun autoconf-insert (text)
  "Insert TEXT."
  (setq autoconf-inserted-text text)
  (insert text))

(defun autoconf-set-version (version)
  "Set the version used with automake to VERSION."
  (if (not (stringp version))
      (signal 'wrong-type-argument '(stringp version)))
  (if (not (autoconf-find-last-macro "AM_INIT_AUTOMAKE"))
      (error "Cannot update version")
    ;; Move to correct position.
    (autoconf-edit-cycle
     (autoconf-delete-parameter 2)
     (autoconf-insert version))))

(defun autoconf-set-output (outputlist)
  "Set the files created in AC_OUTPUT to OUTPUTLIST.
OUTPUTLIST is a list of strings representing relative paths
to Makefiles, or other files using Autoconf substitution."
  (if (not (autoconf-find-last-macro "AC_OUTPUT"))
      (error "Cannot update version")
    (autoconf-edit-cycle
     (autoconf-delete-parameter 1)
     (autoconf-insert (mapconcat (lambda (a) a) outputlist " ")))))

(provide 'ede/autoconf-edit)

;;; ede/autoconf-edit.el ends here
