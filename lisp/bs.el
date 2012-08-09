;;; bs.el --- menu for selecting and displaying buffers -*- lexical-binding: t -*-

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.
;; Author: Olaf Sylvester <Olaf.Sylvester@netsurf.de>
;; Maintainer: Olaf Sylvester <Olaf.Sylvester@netsurf.de>
;; Keywords: convenience

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

;; Version: 1.17
;; X-URL: http://www.geekware.de/software/emacs
;;
;; The bs-package contains a main function bs-show for popping up a
;; buffer in a way similar to `list-buffers' and `electric-buffer-list':
;; The new buffer offers a Buffer Selection Menu for manipulating
;; the buffer list and buffers.
;;
;; -----------------------------------------------------------------------
;; | MR Buffer          Size  Mode          File                         |
;; | -- ------          ----  ----          ----                         |
;; |.   bs.el           14690  Emacs-Lisp    /home/sun/sylvester/el/bs.e$|
;; |  % executable.el    9429  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % vc.el          104893  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % test_vc.el        486  Emacs-Lisp    /home/sun/sylvester/el/test$|
;; |  % vc-hooks.el     43605  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; -----------------------------------------------------------------------

;;; Quick Installation and Customization:

;; To display the bs menu, do
;;   M-x bs-show
;; To customize its behavior, do
;;   M-x bs-customize

;;; More Commentary:

;; bs-show will generate a new buffer named *buffer-selection*, which shows
;; all buffers or a subset of them, and has possibilities for deleting,
;; saving and selecting buffers. For more details see docstring of
;; function `bs-mode'. A current configuration describes which buffers appear
;; in *buffer-selection*. See docstring of variable `bs-configurations' for
;; more details.
;;
;; The package bs combines the advantages of the Emacs functions
;; `list-buffers' and `electric-buffer-list'.
;;
;; Additional features for Buffer Selection Menu:
;;  - configurable list of buffers (show only files etc.).
;;  - comfortable way to change displayed subset of all buffers.
;;  - show sorted list of buffers.
;;  - cyclic navigation:
;;     - goes to top of buffer list if you are on last line and press down.
;;     - goes to end of buffer list if you are on first line and press up.
;;  - Offer an alternative buffer list by prefix key C-u.

;;; Cycling through buffers

;; This package offers two functions for buffer cycling. If you want to cycle
;; through buffer list you can use `bs-cycle-next' or `bs-cycle-previous'.
;; Bind these function to a key like
;;   (global-set-key [(f9)]   'bs-cycle-previous)
;;   (global-set-key [(f10)]  'bs-cycle-next)
;;
;; Both functions use a special subset of all buffers for cycling to avoid
;; to go through internal buffers like *Messages*.
;;
;; Cycling through buffers ignores sorting because sorting destroys
;; the logical buffer list. If buffer list is sorted by size you
;; won't be able to cycle to the smallest buffer.

;;; Customization:

;; There is a customization group called `bs' in group `convenience'.
;; Start customization by M-x bs-customize
;;
;; Buffer list
;; -----------
;; You can define your own configurations by extending variable
;; `bs-configurations' (see docstring for details).
;;
;; `bs-default-configuration' contains the name of default configuration.
;; The default value is "files" which means to show only files.
;;
;; If you always want to see all buffers, customize variable
;; `bs-default-configuration' in customization group `bs'.
;;
;; Configure sorting
;; -----------------
;; You can define functions for sorting the buffer list.
;; When selecting buffers, you can step through available sorting
;; methods with key 'S'.
;; To define a new way of sorting, customize variable `bs-sort-functions'.
;;
;; There are four basic functions for sorting:
;;   by buffer name, by mode, by size, or by filename
;;
;; Configure buffer cycling
;; ------------------------
;; When cycling through buffer list the functions for cycling will use
;; the current configuration of bs to calculate the buffer list.
;; If you want to use a different configuration for cycling you have to set
;; the variable `bs-cycle-configuration-name'. You can customize this variable.
;;
;; For example: If you use the configuration called "files-and-scratch" you
;; can cycle through all file buffers and *scratch* although your current
;; configuration perhaps is "files" which ignores buffer *scratch*.

;;; History:

;;; Code:

(eval-when-compile (require 'cl))

;; ----------------------------------------------------------------------
;; Globals for customization
;; ----------------------------------------------------------------------

(defgroup bs nil
  "Buffer Selection: Maintaining buffers by buffer menu."
  :version "21.1"
  :link '(emacs-commentary-link "bs")
  :link '(url-link "http://www.geekware.de/software/emacs")
  :group 'convenience)

(defgroup bs-appearance nil
  "Buffer Selection appearance: Appearance of bs buffer menu."
  :group 'bs)

(defcustom bs-attributes-list
  '((""       1   1 left  bs--get-marked-string)
    ("M"      1   1 left  bs--get-modified-string)
    ("R"      2   2 left  bs--get-readonly-string)
    ("Buffer" bs--get-name-length 10 left  bs--get-name)
    (""       1   1 left  " ")
    ("Size"   8   8 right bs--get-size-string)
    (""       1   1 left  " ")
    ("Mode"   12 12 right bs--get-mode-name)
    (""       2   2 left  "  ")
    ("File"   12 12 left  bs--get-file-name)
    (""       2   2 left  "  "))
  "List specifying the layout of a Buffer Selection Menu buffer.
Each entry specifies a column and is a list of the form of:
\(HEADER MINIMUM-LENGTH MAXIMUM-LENGTH ALIGNMENT FUN-OR-STRING)

HEADER         : String for header for first line or a function
                 which calculates column title.
MINIMUM-LENGTH : Minimum width of column (number or name of function).
                 The function must return a positive integer.
MAXIMUM-LENGTH : Maximum width of column (number or name of function)
                 (currently ignored).
ALIGNMENT      : Alignment of column (`left', `right', `middle').
FUN-OR-STRING  : Name of a function for calculating the value or a
                 string for a constant value.

The function gets as parameter the buffer where we have started
buffer selection and the list of all buffers to show.  The function must
return a string representing the column's value."
  :group 'bs-appearance
  :type '(repeat sexp))

(defun bs--make-header-match-string ()
  "Return a regexp matching the first line of a Buffer Selection Menu buffer."
  (concat "^\\(" (mapconcat #'car bs-attributes-list " *") " *$\\)"))

;; Font-Lock-Settings
(defvar bs-mode-font-lock-keywords
  (list ;; header in font-lock-type-face
   (list (bs--make-header-match-string)
	 '(1 font-lock-type-face append) '(1 'bold append))
   ;; Buffername embedded by *
   (list "^\\(.*\\*.*\\*.*\\)$"
	 1
	 ;; problem in XEmacs with font-lock-constant-face
	 (if (facep 'font-lock-constant-face)
	     'font-lock-constant-face
	   'font-lock-comment-face))
   ;; Dired-Buffers
   '("^..\\(.*Dired .*\\)$" 1 font-lock-function-name-face)
   ;; the star for modified buffers
   '("^.\\(\\*\\) +[^\\*]"     1 font-lock-comment-face))
  "Default font lock expressions for Buffer Selection Menu.")

(defcustom bs-max-window-height 20
  "Maximal window height of Buffer Selection Menu."
  :group 'bs-appearance
  :type 'integer)

(defvar bs-dont-show-regexp nil
  "Regular expression specifying which buffers not to show.
A buffer whose name matches this regular expression will not be
included in the buffer list.")

(defvar bs-must-show-regexp nil
  "Regular expression for specifying buffers which must be shown.
A buffer whose name matches this regular expression will be
included in the buffer list.
Note that this variable is temporary: if the configuration is changed
it is reset to nil.  Use `bs-must-always-show-regexp' to specify buffers
that must always be shown regardless of the configuration.")

(defcustom bs-must-always-show-regexp nil
  "Regular expression for specifying buffers to show always.
A buffer whose name matches this regular expression will
be shown regardless of current configuration of Buffer Selection Menu."
  :group 'bs
  :type '(choice (const :tag "Nothing at all" nil) regexp))

(defvar bs-dont-show-function nil
  "Function for specifying buffers not to show.
The function gets one argument - the buffer to test.  The function must
return a value different from nil to ignore the buffer in
Buffer Selection Menu.")

(defvar bs-must-show-function nil
  "Function for specifying buffers which must be shown.
The function gets one argument - the buffer to test.")

(defvar bs-buffer-sort-function nil
  "Sort function to sort the buffers that appear in Buffer Selection Menu.
The function gets two arguments - the buffers to compare.
It must return non-nil if the first buffer should sort before the second.")

(defcustom bs-maximal-buffer-name-column 45
  "Maximum column width for buffer names.
The column for buffer names has dynamic width.  The width depends on
maximal and minimal length of names of buffers to show.  The maximal
width is bounded by `bs-maximal-buffer-name-column'.
See also `bs-minimal-buffer-name-column'."
  :group 'bs-appearance
  :type 'integer)

(defcustom bs-minimal-buffer-name-column 15
  "Minimum column width for buffer names.
The column for buffer names has dynamic width.  The width depends on
maximal and minimal length of names of buffers to show.  The minimal
width is bounded by `bs-minimal-buffer-name-column'.
See also `bs-maximal-buffer-name-column'."
  :group 'bs-appearance
  :type 'integer)

(defconst bs-header-lines-length 2
  "Number of lines for headers in Buffer Selection Menu.")

(defcustom bs-configurations
  '(("all" nil nil nil nil nil)
    ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
    ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file
     bs-sort-buffer-interns-are-last)
    ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last))
  "List of all configurations you can use in the Buffer Selection Menu.
A configuration describes which buffers appear in Buffer Selection Menu
and also the order of buffers.  A configuration is a list with
six elements.  The first element is a string and describes the configuration.
The following five elements represent the values for Buffer Selection Menu
configuration variables `bs-must-show-regexp', `bs-must-show-function',
`bs-dont-show-regexp', `bs-dont-show-function' and `bs-buffer-sort-function'.
By setting these variables you define a configuration."
  :group 'bs-appearance
  :type '(repeat sexp))

(defcustom bs-default-configuration "files"
  "Name of default configuration used by the Buffer Selection Menu.
\\<bs-mode-map>
Will be changed using key \\[bs-select-next-configuration].
Must be a string used in `bs-configurations' for naming a configuration."
  :group 'bs
  :type 'string)

(defcustom bs-alternative-configuration "all"
  "Name of configuration used when calling `bs-show' with \
\\[universal-argument] as prefix key.
Must be a string used in `bs-configurations' for naming a configuration."
  :group 'bs
  :type  'string)

(defvar bs-current-configuration bs-default-configuration
  "Name of current configuration.
Must be a string used in `bs-configurations' for naming a configuration.")

(defcustom bs-cycle-configuration-name nil
  "Name of configuration used when cycling through the buffer list.
A value of nil means to use current configuration `bs-default-configuration'.
Must be a string used in `bs-configurations' for naming a configuration."
  :group 'bs
  :type '(choice (const :tag "like current configuration" nil)
   string))

(defcustom bs-string-show-always "+"
  "String added in column 1 indicating a buffer will always be shown."
  :group 'bs-appearance
  :type 'string)

(defcustom bs-string-show-never "-"
  "String added in column 1 indicating a buffer will never be shown."
  :group 'bs-appearance
  :type 'string)

(defcustom bs-string-current "."
  "String added in column 1 indicating the current buffer."
  :group 'bs-appearance
  :type 'string)

(defcustom bs-string-current-marked "#"
  "String added in column 1 indicating the current buffer when it is marked."
  :group 'bs-appearance
  :type 'string)

(defcustom bs-string-marked ">"
  "String added in column 1 indicating a marked buffer."
  :group 'bs-appearance
  :type 'string)

(defcustom bs-string-show-normally  " "
  "String added in column 1 indicating an unmarked buffer."
  :group 'bs-appearance
  :type 'string)

(defvar bs--name-entry-length 20
  "Maximum length of all displayed buffer names.
Used internally, only.")

;; ----------------------------------------------------------------------
;; Internal globals
;; ----------------------------------------------------------------------

(defvar bs-buffer-show-mark nil
  "Flag for the current mode for showing this buffer.
A value of nil means buffer will be shown depending on the current
configuration.
A value of `never' means to never show the buffer.
A value of `always' means to show buffer regardless of the configuration.")

(make-variable-buffer-local 'bs-buffer-show-mark)

;; Make face named region (for XEmacs)
(unless (facep 'region)
  (make-face 'region)
  (set-face-background 'region "gray75"))

(defun bs--sort-by-name (b1 b2)
  "Compare buffers B1 and B2 by buffer name."
  (string< (buffer-name b1)
	   (buffer-name b2)))

(defun bs--sort-by-filename (b1 b2)
  "Compare buffers B1 and B2 by file name."
  (string< (or (buffer-file-name b1) "")
	   (or (buffer-file-name b2) "")))

(defun bs--sort-by-mode (b1 b2)
  "Compare buffers B1 and B2 by mode name."
  (save-current-buffer
    (string< (progn (set-buffer b1) (format-mode-line mode-name nil nil b1))
	     (progn (set-buffer b2) (format-mode-line mode-name nil nil b2)))))

(defun bs--sort-by-size (b1 b2)
  "Compare buffers B1 and B2 by buffer size."
  (< (buffer-size b1) (buffer-size b2)))

(defcustom bs-sort-functions
  '(("by name"     bs--sort-by-name     "Buffer" region)
    ("by size"     bs--sort-by-size     "Size"   region)
    ("by mode"     bs--sort-by-mode     "Mode"   region)
    ("by filename" bs--sort-by-filename "File"   region)
    ("by nothing"  nil                  nil      nil))
  "List of all possible sorting aspects for Buffer Selection Menu.
You can add a new entry with a call to `bs-define-sort-function'.
Each element is a list of four elements (NAME FUNCTION REGEXP-FOR-SORTING FACE).
NAME specifies the sort order defined by function FUNCTION.
FUNCTION nil means don't sort the buffer list.  Otherwise the function
must have two parameters - the buffers to compare.
REGEXP-FOR-SORTING is a regular expression which describes the
column title to highlight.
FACE is a face used to fontify the sorted column title.  A value of nil means
don't highlight."
  :group 'bs
  :type '(repeat sexp))

(defun bs-define-sort-function (name fun &optional regexp-for-sorting face)
  "Define a new function for buffer sorting in Buffer Selection Menu.
NAME specifies the sort order defined by function FUN.
A value of nil for FUN means don't sort the buffer list.  Otherwise the
functions must have two parameters - the buffers to compare.
REGEXP-FOR-SORTING is a regular expression which describes the
column title to highlight.
FACE is a face used to fontify the sorted column title.  A value of nil means
don't highlight.
The new sort aspect will be inserted into list `bs-sort-functions'."
  (let ((tupel (assoc name bs-sort-functions)))
    (if tupel
	(setcdr tupel (list fun regexp-for-sorting face))
      (setq bs-sort-functions
	    (cons (list name fun regexp-for-sorting face)
		  bs-sort-functions)))))

(defvar bs--current-sort-function nil
  "Description of the current function for sorting the buffer list.
This is an element of `bs-sort-functions'.")

(defcustom bs-default-sort-name "by nothing"
  "Name of default sort behavior.
Must be \"by nothing\" or a string used in `bs-sort-functions' for
naming a sort behavior.  Default is \"by nothing\" which means no sorting."
  :group 'bs
  :type  'string
  :set (lambda (var-name value)
	 (set var-name value)
	 (setq bs--current-sort-function
	       (assoc value bs-sort-functions))))

(defvar bs--buffer-coming-from nil
  "The buffer in which the user started the current Buffer Selection Menu.")

(defvar bs--show-all nil
  "Flag whether showing all buffers regardless of current configuration.
Non-nil means to show all buffers.  Otherwise show buffers
defined by current configuration `bs-current-configuration'.")

(defvar bs--window-config-coming-from nil
  "Window configuration before starting Buffer Selection Menu.")

(defvar bs--intern-show-never "^ \\|\\*buffer-selection\\*"
  "Regular expression specifying which buffers never to show.
A buffer whose name matches this regular expression will never be
included in the buffer list.")

(defvar bs-current-list nil
  "List of buffers shown in Buffer Selection Menu.
Used internally, only.")

(defvar bs--marked-buffers nil
  "Currently marked buffers in Buffer Selection Menu.")

(defvar bs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " "       'bs-select)
    (define-key map "f"       'bs-select)
    (define-key map "v"       'bs-view)
    (define-key map "!"       'bs-select-in-one-window)
    (define-key map [mouse-2] 'bs-mouse-select)	;; for GNU EMACS
    (define-key map [button2] 'bs-mouse-select) ;; for XEmacs
    (define-key map "F"       'bs-select-other-frame)
    (let ((key ?1))
      (while (<= key ?9)
	(define-key map (char-to-string key) 'digit-argument)
	(setq key (1+ key))))
    (define-key map "-"       'negative-argument)
    (define-key map "\e-"     'negative-argument)
    (define-key map "o"       'bs-select-other-window)
    (define-key map "\C-o"    'bs-tmp-select-other-window)
    ;; for GNU EMACS
    (define-key map [mouse-3] 'bs-mouse-select-other-frame)
    ;; for XEmacs
    (define-key map [button3] 'bs-mouse-select-other-frame)
    (define-key map [up]      'bs-up)
    (define-key map "n"       'bs-down)
    (define-key map "p"       'bs-up)
    (define-key map [down]    'bs-down)
    (define-key map "\C-m"    'bs-select)
    (define-key map "b"       'bs-bury-buffer)
    (define-key map "s"       'bs-save)
    (define-key map "S"       'bs-show-sorted)
    (define-key map "a"       'bs-toggle-show-all)
    (define-key map "d"       'bs-delete)
    (define-key map "\C-d"    'bs-delete-backward)
    (define-key map "k"       'bs-delete)
    (define-key map "g"       'bs-refresh)
    (define-key map "C"       'bs-set-configuration-and-refresh)
    (define-key map "c"       'bs-select-next-configuration)
    (define-key map "q"       'bs-kill)
    ;; (define-key map "z"       'bs-kill)
    (define-key map "\C-c\C-c" 'bs-kill)
    (define-key map "\C-g"    'bs-abort)
    (define-key map "\C-]"    'bs-abort)
    (define-key map "%"       'bs-toggle-readonly)
    (define-key map "~"       'bs-clear-modified)
    (define-key map "M"       'bs-toggle-current-to-show)
    (define-key map "+"       'bs-set-current-buffer-to-show-always)
    ;;(define-key map "-"       'bs-set-current-buffer-to-show-never)
    (define-key map "t"       'bs-visit-tags-table)
    (define-key map "m"       'bs-mark-current)
    (define-key map "u"       'bs-unmark-current)
    (define-key map ">"       'scroll-right)
    (define-key map "<"       'scroll-left)
    (define-key map "?"       'bs-help)
    map)
  "Keymap of `bs-mode'.")

;; ----------------------------------------------------------------------
;; Functions
;; ----------------------------------------------------------------------

(defun bs-buffer-list (&optional list sort-description)
  "Return a list of buffers to be shown.
LIST is a list of buffers to test for appearance in Buffer Selection Menu.
The result list depends on the global variables `bs-dont-show-regexp',
`bs-must-show-regexp', `bs-dont-show-function', `bs-must-show-function'
and `bs-buffer-sort-function'.
If SORT-DESCRIPTION isn't nil the list will be sorted by
a special function.  SORT-DESCRIPTION is an element of `bs-sort-functions'."
  (setq sort-description (or sort-description bs--current-sort-function)
	list (or list (buffer-list)))
  (let ((result nil))
    (dolist (buf list)
      (let* ((buffername (buffer-name buf))
	     (int-show-never (string-match-p bs--intern-show-never buffername))
	     (ext-show-never (and bs-dont-show-regexp
				  (string-match-p bs-dont-show-regexp
						  buffername)))
	     (extern-must-show (or (and bs-must-always-show-regexp
					(string-match-p
					 bs-must-always-show-regexp
					 buffername))
				   (and bs-must-show-regexp
					(string-match-p bs-must-show-regexp
							buffername))))
	     (extern-show-never-from-fun (and bs-dont-show-function
					      (funcall bs-dont-show-function
						       buf)))
	     (extern-must-show-from-fun (and bs-must-show-function
					     (funcall bs-must-show-function
						      buf)))
	     (show-flag (buffer-local-value 'bs-buffer-show-mark buf)))
	(when (or (eq show-flag 'always)
		  (and (or bs--show-all (not (eq show-flag 'never)))
		       (not int-show-never)
		       (or bs--show-all
			   extern-must-show
			   extern-must-show-from-fun
			   (and (not ext-show-never)
				(not extern-show-never-from-fun)))))
	  (setq result (cons buf result)))))
    (setq result (reverse result))
    ;; The current buffer which was the start point of bs should be an element
    ;; of result list, so that we can leave with space and be back in the
    ;; buffer we started bs-show.
    (when (and bs--buffer-coming-from
	       (buffer-live-p bs--buffer-coming-from)
	       (not (memq bs--buffer-coming-from result)))
      (setq result (cons bs--buffer-coming-from result)))
    ;; sorting
    (if (and sort-description
	     (nth 1 sort-description))
	(setq result (sort result (nth 1 sort-description)))
      ;; else standard sorting
      (bs-buffer-sort result))))

(defun bs-buffer-sort (buffer-list)
  "Sort buffers in BUFFER-LIST according to `bs-buffer-sort-function'."
  (if bs-buffer-sort-function
      (sort buffer-list bs-buffer-sort-function)
    buffer-list))

(defun bs--redisplay (&optional keep-line-p sort-description)
  "Redisplay whole Buffer Selection Menu.
If KEEP-LINE-P is non-nil the point will stay on current line.
SORT-DESCRIPTION is an element of `bs-sort-functions'."
  (let ((line (count-lines 1 (point))))
    (bs-show-in-buffer (bs-buffer-list nil sort-description))
    (when keep-line-p
      (goto-char (point-min))
      (forward-line line))
    (beginning-of-line)))

(defun bs--goto-current-buffer ()
  "Goto line which represents the current buffer;
actually the line which begins with character in `bs-string-current' or
`bs-string-current-marked'."
  (let ((regexp (concat "^"
			(regexp-quote bs-string-current)
			"\\|^"
			(regexp-quote bs-string-current-marked)))
	point)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp regexp nil t)
	(setq point (1- (point)))))
    (when point
      (goto-char point))))

(defun bs--current-config-message ()
  "Return a string describing the current `bs-mode' configuration."
  (if bs--show-all
      "Show all buffers."
    (format "Show buffer by configuration %S"
	    bs-current-configuration)))

(defun bs--track-window-changes (frame)
  "Track window changes to refresh the buffer list.
Used from `window-size-change-functions'."
  (let ((win (get-buffer-window "*buffer-selection*" frame)))
    (when win
      (with-selected-window win
	(bs--set-window-height)))))

(defun bs--remove-hooks ()
  "Remove `bs--track-window-changes' and auxiliary hooks."
  (remove-hook 'window-size-change-functions 'bs--track-window-changes)
  ;; Remove itself
  (remove-hook 'kill-buffer-hook 'bs--remove-hooks t)
  (remove-hook 'change-major-mode-hook 'bs--remove-hooks t))

(put 'bs-mode 'mode-class 'special)

(define-derived-mode bs-mode nil "Buffer-Selection-Menu"
  "Major mode for editing a subset of Emacs's buffers.
\\<bs-mode-map>
Aside from two header lines each line describes one buffer.
Move to a line representing the buffer you want to edit and select
buffer by \\[bs-select] or SPC.  Abort buffer list with \\[bs-kill].
There are many key commands similar to `Buffer-menu-mode' for
manipulating the buffer list and buffers.
For faster navigation each digit key is a digit argument.

\\[bs-select] or SPACE -- select current line's buffer and other marked buffers.
\\[bs-toggle-show-all]  -- toggle between all buffers and a special subset.
\\[bs-select-other-window] -- select current line's buffer in other window.
\\[bs-tmp-select-other-window] -- make another window display that buffer and
    remain in Buffer Selection Menu.
\\[bs-mouse-select] -- select current line's buffer and other marked buffers.
\\[bs-save] -- save current line's buffer immediately.
\\[bs-delete] -- kill current line's buffer immediately.
\\[bs-toggle-readonly] -- toggle read-only status of current line's buffer.
\\[bs-clear-modified] -- clear modified-flag on that buffer.
\\[bs-mark-current] -- mark current line's buffer to be displayed.
\\[bs-unmark-current] -- unmark current line's buffer to be displayed.
\\[bs-show-sorted] -- display buffer list sorted by next sort aspect.
\\[bs-set-configuration-and-refresh] -- ask user for a configuration and \
apply selected configuration.
\\[bs-select-next-configuration] -- select and apply next \
available Buffer Selection Menu configuration.
\\[bs-kill] -- leave Buffer Selection Menu without a selection.
\\[bs-toggle-current-to-show] -- toggle status of appearance.
\\[bs-set-current-buffer-to-show-always] -- mark current line's buffer \
to show always.
\\[bs-visit-tags-table] -- call `visit-tags-table' on current line's buffer.
\\[bs-help] -- display this help text."
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-verbose)
  (make-local-variable 'font-lock-global-modes)
  (buffer-disable-undo)
  (setq buffer-read-only t
	truncate-lines t
	show-trailing-whitespace nil
	font-lock-global-modes '(not bs-mode)
	font-lock-defaults '(bs-mode-font-lock-keywords t)
	font-lock-verbose nil)
  (set (make-local-variable 'revert-buffer-function) 'bs-refresh)
  (add-hook 'window-size-change-functions 'bs--track-window-changes)
  (add-hook 'kill-buffer-hook 'bs--remove-hooks nil t)
  (add-hook 'change-major-mode-hook 'bs--remove-hooks nil t))

(defun bs--restore-window-config ()
  "Restore window configuration on the current frame."
  (when bs--window-config-coming-from
    (let ((frame (selected-frame)))
      (unwind-protect
	   (set-window-configuration bs--window-config-coming-from)
	(select-frame frame)))
    (setq bs--window-config-coming-from nil)))

(defun bs-kill ()
  "Let buffer disappear and reset window configuration."
  (interactive)
  (bury-buffer (current-buffer))
  (bs--restore-window-config))

(defun bs-abort ()
  "Ding and leave Buffer Selection Menu without a selection."
  (interactive)
  (ding)
  (bs-kill))

(defun bs-set-configuration-and-refresh ()
  "Ask user for a configuration and apply selected configuration.
Refresh whole Buffer Selection Menu."
  (interactive)
  (call-interactively 'bs-set-configuration)
  (bs--redisplay t))

(defun bs-refresh (&rest _ignored)
  "Refresh whole Buffer Selection Menu.
Arguments are IGNORED (for `revert-buffer')."
  (interactive)
  (bs--redisplay t))

(defun bs--set-window-height ()
  "Change the height of the selected window to suit the current buffer list."
  (unless (one-window-p t)
    (fit-window-to-buffer (selected-window) bs-max-window-height)))

(defun bs--current-buffer ()
  "Return buffer on current line.
Raise an error if not on a buffer line."
  (beginning-of-line)
  (let ((line (+ (- bs-header-lines-length)
		 (count-lines 1 (point)))))
    (when (< line 0)
      (error "You are on a header row"))
    (nth line bs-current-list)))

(defun bs--update-current-line ()
  "Update the entry on current line for Buffer Selection Menu."
  (let ((buffer (bs--current-buffer))
	(inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (bs--insert-one-entry buffer)
    (beginning-of-line)))

(defun bs-view ()
  "View current line's buffer in View mode.
Leave Buffer Selection Menu."
  (interactive)
  (view-buffer (bs--current-buffer)))

(defun bs-select ()
  "Select current line's buffer and other marked buffers.
If there are no marked buffers the window configuration before starting
Buffer Selection Menu will be restored.
If there are marked buffers each marked buffer and the current line's buffer
will be selected in a window.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (switch-to-buffer buffer)
    (when bs--marked-buffers
      ;; Some marked buffers for selection
      (let* ((all (delq buffer bs--marked-buffers))
	     (height (/ (1- (frame-height)) (1+ (length all)))))
	(delete-other-windows)
	(switch-to-buffer buffer)
	(dolist (buf all)
	  (split-window nil height)
	  (other-window 1)
	  (switch-to-buffer buf))
	;; goto window we have started bs.
	(other-window 1)))))

(defun bs-select-other-window ()
  "Select current line's buffer by `switch-to-buffer-other-window'.
The window configuration before starting Buffer Selection Menu will be restored
unless there is no other window.  In this case a new window will be created.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (switch-to-buffer-other-window buffer)))

(defun bs-tmp-select-other-window ()
  "Make the other window select this line's buffer.
The current window remains selected."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (display-buffer buffer t)))

(defun bs-select-other-frame ()
  "Select current line's buffer in new created frame.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (switch-to-buffer-other-frame buffer)))

(defun bs-mouse-select-other-frame (event)
  "Select selected line's buffer in new created frame.
Leave Buffer Selection Menu.
EVENT: a mouse click event."
  (interactive "e")
  (mouse-set-point event)
  (bs-select-other-frame))

(defun bs-mouse-select (event)
  "Select buffer on mouse click EVENT.
Select buffer by `bs-select'."
  (interactive "e")
  (mouse-set-point event)
  (bs-select))

(defun bs-select-in-one-window ()
  "Select current line's buffer in one window and delete other windows.
Leave Buffer Selection Menu."
  (interactive)
  (bs-select)
  (delete-other-windows))

(defun bs-bury-buffer ()
  "Bury buffer on current line."
  (interactive)
  (bury-buffer (bs--current-buffer))
  (bs--redisplay t))

(defun bs-save ()
  "Save buffer on current line."
  (interactive)
  (with-current-buffer (bs--current-buffer)
    (save-buffer))
  (bs--update-current-line))

(defun bs-visit-tags-table ()
  "Visit the tags table in the buffer on this line.
See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (bs--current-buffer))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun bs-toggle-current-to-show ()
  "Toggle status of showing flag for buffer in current line."
  (interactive)
  (let ((res
         (with-current-buffer (bs--current-buffer)
           (setq bs-buffer-show-mark (case bs-buffer-show-mark
                                       ((nil)   'never)
                                       ((never) 'always)
                                       (t       nil))))))
    (bs--update-current-line)
    (bs--set-window-height)
    (bs--show-config-message res)))

(defun bs-set-current-buffer-to-show-always (&optional not-to-show-p)
  "Toggle status of buffer on line to `always shown'.
NOT-TO-SHOW-P: prefix argument.
With no prefix argument the buffer on current line is marked to show
always.  Otherwise it is marked to show never."
  (interactive "P")
  (if not-to-show-p
      (bs-set-current-buffer-to-show-never)
    (bs--set-toggle-to-show (bs--current-buffer) 'always)))

(defun bs-set-current-buffer-to-show-never ()
  "Toggle status of buffer on line to `never shown'."
  (interactive)
  (bs--set-toggle-to-show (bs--current-buffer) 'never))

(defun bs--set-toggle-to-show (buffer what)
  "Set value `bs-buffer-show-mark' of buffer BUFFER to WHAT.
Redisplay current line and display a message describing
the status of buffer on current line."
  (with-current-buffer buffer (setq bs-buffer-show-mark what))
  (bs--update-current-line)
  (bs--set-window-height)
  (bs--show-config-message what))

(defun bs--mark-unmark (count fun)
  "Call FUN on COUNT consecutive buffers of *buffer-selection*."
  (let ((dir (if (> count 0) 1 -1)))
    (dotimes (_i (abs count))
      (let ((buffer (bs--current-buffer)))
	(when buffer (funcall fun buffer))
	(bs--update-current-line)
	(bs-down dir)))))

(defun bs-mark-current (count)
  "Mark buffers.
COUNT is the number of buffers to mark.
Move cursor vertically down COUNT lines."
  (interactive "p")
  (bs--mark-unmark count
		   (lambda (buf)
		     (add-to-list 'bs--marked-buffers buf))))

(defun bs-unmark-current (count)
  "Unmark buffers.
COUNT is the number of buffers to unmark.
Move cursor vertically down COUNT lines."
  (interactive "p")
  (bs--mark-unmark count
		   (lambda (buf)
		     (setq bs--marked-buffers (delq buf bs--marked-buffers)))))

(defun bs--show-config-message (what)
  "Show message indicating the new showing status WHAT.
WHAT is a value of nil, `never', or `always'."
  (bs-message-without-log (cond ((null what)
				 "Buffer will be shown normally.")
				((eq what 'never)
				 "Mark buffer to never be shown.")
				(t "Mark buffer to show always."))))

(defun bs-delete ()
  "Kill buffer on current line."
  (interactive)
  (let ((current (bs--current-buffer))
	(inhibit-read-only t))
    (unless (kill-buffer current)
      (error "Buffer was not deleted"))
    (setq bs-current-list (delq current bs-current-list))
    (beginning-of-line)
    (delete-region (point) (save-excursion
			     (end-of-line)
			     (if (eobp) (point) (1+ (point)))))
    (when (eobp)
      (backward-delete-char 1)
      (beginning-of-line)
      (recenter -1))
    (bs--set-window-height)))

(defun bs-delete-backward ()
  "Like `bs-delete' but go to buffer in front of current."
  (interactive)
  (let ((on-last-line-p (save-excursion (end-of-line) (eobp))))
    (bs-delete)
    (unless on-last-line-p
      (bs-up 1))))

(defun bs-show-sorted ()
  "Show buffer list sorted by next sort aspect."
  (interactive)
  (setq bs--current-sort-function
	(bs-next-config-aux (car bs--current-sort-function)
			    bs-sort-functions))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (bs-message-without-log "Sorted %s" (car bs--current-sort-function)))

(defun bs-apply-sort-faces (&optional sort-description)
  "Set text properties for the sort described by SORT-DESCRIPTION.
SORT-DESCRIPTION is an element of `bs-sort-functions'.
Default is `bs--current-sort-function'."
  (let ((sort-description (or sort-description
			      bs--current-sort-function)))
    (save-excursion
      (goto-char (point-min))
      (when (and (nth 2 sort-description)
		 (search-forward-regexp (nth 2 sort-description) nil t))
	(let ((inhibit-read-only t))
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'face
			     (or (nth 3 sort-description)
				 'region)))))))

(defun bs-toggle-show-all ()
  "Toggle show all buffers / show buffers with current configuration."
  (interactive)
  (setq bs--show-all (not bs--show-all))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (bs-message-without-log "%s" (bs--current-config-message)))

(defun bs-toggle-readonly ()
  "Toggle read-only status for buffer on current line.
Uses function `toggle-read-only'."
  (interactive)
  (with-current-buffer (bs--current-buffer)
    (toggle-read-only))
  (bs--update-current-line))

(defun bs-clear-modified ()
  "Set modified flag for buffer on current line to nil."
  (interactive)
  (with-current-buffer (bs--current-buffer)
    (set-buffer-modified-p nil))
  (bs--update-current-line))

(defun bs--nth-wrapper (count fun &rest args)
  "Call COUNT times function FUN with arguments ARGS."
  (dotimes (_i (or count 1))
    (apply fun args)))

(defun bs-up (arg)
  "Move cursor vertically up ARG lines in Buffer Selection Menu."
  (interactive "p")
  (if (and arg (numberp arg) (< arg 0))
      (bs--nth-wrapper (- arg) 'bs--down)
    (bs--nth-wrapper arg 'bs--up)))

(defun bs--up ()
  "Move cursor vertically up one line.
If on top of buffer list go to last line."
  (if (> (count-lines 1 (point)) bs-header-lines-length)
      (forward-line -1)
    (goto-char (point-max))
    (beginning-of-line)
    (recenter -1)))

(defun bs-down (arg)
  "Move cursor vertically down ARG lines in Buffer Selection Menu."
  (interactive "p")
  (if (and arg (numberp arg) (< arg 0))
      (bs--nth-wrapper (- arg) 'bs--up)
    (bs--nth-wrapper arg 'bs--down)))

(defun bs--down ()
  "Move cursor vertically down one line.
If at end of buffer list go to first line."
  (if (eq (line-end-position) (point-max))
      (progn
	(goto-char (point-min))
	(forward-line bs-header-lines-length))
    (forward-line 1)))

(defun bs-visits-non-file (buffer)
  "Return whether BUFFER visits no file.
A value of t means BUFFER belongs to no file.
A value of nil means BUFFER belongs to a file."
  (not (buffer-file-name buffer)))

(defun bs-sort-buffer-interns-are-last (_b1 b2)
  "Function for sorting internal buffers at the end of all buffers."
  (string-match-p "^\\*" (buffer-name b2)))

;; ----------------------------------------------------------------------
;; Configurations:
;; ----------------------------------------------------------------------

(defun bs-config-clear ()
  "Reset all variables which specify a configuration.
These variables are `bs-dont-show-regexp', `bs-must-show-regexp',
`bs-dont-show-function', `bs-must-show-function' and
`bs-buffer-sort-function'."
  (setq bs-dont-show-regexp nil
	bs-must-show-regexp nil
	bs-dont-show-function nil
	bs-must-show-function nil
	bs-buffer-sort-function nil))

(defun bs-config--only-files ()
  "Define a configuration for showing only buffers visiting a file."
  (bs-config-clear)
  (setq ;; I want to see *-buffers at the end
   bs-buffer-sort-function 'bs-sort-buffer-interns-are-last
   ;; Don't show files who don't belong to a file
   bs-dont-show-function 'bs-visits-non-file))

(defun bs-config--files-and-scratch ()
  "Define a configuration for showing buffer *scratch* and file buffers."
  (bs-config-clear)
  (setq ;; I want to see *-buffers at the end
   bs-buffer-sort-function 'bs-sort-buffer-interns-are-last
   ;; Don't show files who don't belong to a file
   bs-dont-show-function 'bs-visits-non-file
   ;; Show *scratch* buffer.
   bs-must-show-regexp "^\\*scratch\\*$"))

(defun bs-config--all ()
  "Define a configuration for showing all buffers.
Reset all according variables by `bs-config-clear'."
  (bs-config-clear))

(defun bs-config--all-intern-last ()
  "Define a configuration for showing all buffers.
Internal buffers appear at end of all buffers."
  (bs-config-clear)
  ;; I want to see *-buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last))

(defun bs-set-configuration (name)
  "Set configuration to the one saved under string NAME in `bs-configurations'.
When called interactively ask user for a configuration and apply selected
configuration."
  (interactive (list (completing-read "Use configuration: "
				      bs-configurations
				      nil
				      t)))
  (let ((list (assoc name bs-configurations)))
    (if list
	(if (listp list)
	    (setq bs-current-configuration name
		  bs-must-show-regexp     (nth 1 list)
		  bs-must-show-function   (nth 2 list)
		  bs-dont-show-regexp     (nth 3 list)
		  bs-dont-show-function   (nth 4 list)
		  bs-buffer-sort-function (nth 5 list))
	  ;; for backward compatibility
	  (funcall (cdr list)))
      ;; else
      (ding)
      (bs-message-without-log "No bs-configuration named %S." name))))

(defun bs-help ()
  "Help for `bs-show'."
  (interactive)
  (describe-function 'bs-mode))

(defun bs-next-config-aux (start-name list)
  "Get the next assoc after START-NAME in list LIST.
Will return the first if START-NAME is at end."
  (let ((assocs list)
	(length (length list))
	pos)
    (while (and assocs (not pos))
      (when (string= (car (car assocs)) start-name)
	(setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (setq pos (1+ pos))
    (if (eq pos length)
	(car list)
      (nth pos list))))

(defun bs-next-config (name)
  "Return next configuration with respect to configuration with name NAME."
  (bs-next-config-aux name bs-configurations))

(defun bs-select-next-configuration (&optional start-name)
  "Apply next configuration START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let ((config (bs-next-config (or start-name bs-current-configuration))))
    (bs-set-configuration (car config))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log "Selected configuration: %s" (car config))))

(defun bs-show-in-buffer (list)
  "Display buffer list LIST in buffer *buffer-selection*.
Select buffer *buffer-selection* and display buffers according to current
configuration `bs-current-configuration'.  Set window height, fontify buffer
and move point to current buffer."
  (setq bs-current-list list)
  (switch-to-buffer (get-buffer-create "*buffer-selection*"))
  (bs-mode)
  (let* ((inhibit-read-only t)
	 (map-fun (lambda (entry)
		    (length (buffer-name entry))))
	 (max-length-of-names (apply 'max
				     (cons 0 (mapcar map-fun list))))
	 (name-entry-length (min bs-maximal-buffer-name-column
				 (max bs-minimal-buffer-name-column
				      max-length-of-names))))
    (erase-buffer)
    (setq bs--name-entry-length name-entry-length)
    (bs--show-header)
    (dolist (buffer list)
      (bs--insert-one-entry buffer)
      (insert "\n"))
    (delete-char -1)
    (bs--set-window-height)
    (bs--goto-current-buffer)
    (font-lock-fontify-buffer)
    (bs-apply-sort-faces)
    (set-buffer-modified-p nil)))

(defun bs-next-buffer (&optional buffer-list sorting-p)
  "Return next buffer and buffer list for buffer cycling in BUFFER-LIST.
Ignore sorting when SORTING-P is nil.
If BUFFER-LIST is nil the result of `bs-buffer-list' will be used as
buffer list.  The result is a cons of normally the second element of
BUFFER-LIST and the buffer list used for buffer cycling."
  (let* ((bs--current-sort-function (if sorting-p
					bs--current-sort-function))
	 (bs-buffer-list (or buffer-list (bs-buffer-list))))
    (cons (or (car (cdr bs-buffer-list))
	      (car bs-buffer-list)
	      (current-buffer))
	  bs-buffer-list)))

(defun bs-previous-buffer (&optional buffer-list sorting-p)
  "Return previous buffer and buffer list for buffer cycling in BUFFER-LIST.
Ignore sorting when SORTING-P is nil.
If BUFFER-LIST is nil the result of `bs-buffer-list' will be used as
buffer list.  The result is a cons of last element of BUFFER-LIST and the
buffer list used for buffer cycling."
  (let* ((bs--current-sort-function (if sorting-p
					bs--current-sort-function))
	 (bs-buffer-list (or buffer-list (bs-buffer-list))))
    (cons (or (car (last bs-buffer-list))
	      (current-buffer))
	  bs-buffer-list)))

(defun bs-message-without-log (&rest args)
  "Like `message' but don't log it on the message log.
All arguments ARGS are transferred to function `message'."
  (let ((message-log-max nil))
    (apply 'message args)))

(defvar bs--cycle-list nil
  "Current buffer list used for cycling.")

;;;###autoload
(defun bs-cycle-next ()
  "Select next buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
  (interactive)
  (let ((bs--buffer-coming-from (current-buffer))
	(bs-dont-show-regexp   bs-dont-show-regexp)
	(bs-must-show-regexp   bs-must-show-regexp)
	(bs-dont-show-function bs-dont-show-function)
	(bs-must-show-function bs-must-show-function)
	(bs--show-all          nil))
    (bs-set-configuration (or bs-cycle-configuration-name bs-default-configuration))
    (let ((bs-buffer-sort-function nil)
	  (bs--current-sort-function nil))
      (let* ((tupel (bs-next-buffer (if (or (eq last-command
						'bs-cycle-next)
					    (eq last-command
						'bs-cycle-previous))
					bs--cycle-list)))
	     (next (car tupel))
	     (cycle-list (cdr tupel)))
        ;; We don't want the frame iconified if the only window in the frame
        ;; happens to be dedicated.
        (bury-buffer (current-buffer))
	(switch-to-buffer next nil t)
	(setq bs--cycle-list (append (cdr cycle-list)
				     (list (car cycle-list))))
	(bs-message-without-log "Next buffers: %s"
				(or (cdr bs--cycle-list)
				    "this buffer"))))))

;;;###autoload
(defun bs-cycle-previous ()
  "Select previous buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
  (interactive)
  (let ((bs--buffer-coming-from (current-buffer))
	(bs-dont-show-regexp   bs-dont-show-regexp)
	(bs-must-show-regexp   bs-must-show-regexp)
	(bs-dont-show-function bs-dont-show-function)
	(bs-must-show-function bs-must-show-function)
	(bs--show-all          nil))
    (bs-set-configuration (or bs-cycle-configuration-name bs-default-configuration))
    (let ((bs-buffer-sort-function nil)
	  (bs--current-sort-function nil))
      (let* ((tupel (bs-previous-buffer (if (or (eq last-command
						    'bs-cycle-next)
						(eq last-command
						    'bs-cycle-previous))
					    bs--cycle-list)))
	     (prev-buffer (car tupel))
	     (cycle-list (cdr tupel)))
	(switch-to-buffer prev-buffer nil t)
	(setq bs--cycle-list (append (last cycle-list)
				     (reverse (cdr (reverse cycle-list)))))
	(bs-message-without-log "Previous buffers: %s"
				(or (reverse (cdr bs--cycle-list))
				    "this buffer"))))))

(defun bs--get-value (fun &optional args)
  "Apply function FUN with arguments ARGS.
Return result of evaluation.  Will return FUN if FUN is a number
or a string."
  (cond ((numberp fun)
	 fun)
	((stringp fun)
	 fun)
	(t (apply fun args))))

(defun bs--get-marked-string (start-buffer _all-buffers)
  "Return a string which describes whether current buffer is marked.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu.
The result string is one of `bs-string-current', `bs-string-current-marked',
`bs-string-marked', `bs-string-show-normally', `bs-string-show-never', or
`bs-string-show-always'."
  (cond ;; current buffer is the buffer we started buffer selection.
   ((eq (current-buffer) start-buffer)
    (if (memq (current-buffer) bs--marked-buffers)
	bs-string-current-marked	; buffer is marked
      bs-string-current))
   ;; current buffer is marked
   ((memq (current-buffer) bs--marked-buffers)
    bs-string-marked)
   ;; current buffer hasn't a special mark.
   ((null bs-buffer-show-mark)
    bs-string-show-normally)
   ;; current buffer has a mark not to show itself.
   ((eq bs-buffer-show-mark 'never)
    bs-string-show-never)
   ;; otherwise current buffer is marked to show always.
   (t
    bs-string-show-always)))

(defun bs--get-modified-string (_start-buffer _all-buffers)
  "Return a string which describes whether current buffer is modified.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu."
  (if (buffer-modified-p) "*" " "))

(defun bs--get-readonly-string (_start-buffer _all-buffers)
  "Return a string which describes whether current buffer is read only.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu."
  (if buffer-read-only "%" " "))

(defun bs--get-size-string (_start-buffer _all-buffers)
  "Return a string which describes the size of current buffer.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu."
  (int-to-string (buffer-size)))

(defun bs--get-name (_start-buffer _all-buffers)
  "Return name of current buffer for Buffer Selection Menu.
The name of current buffer gets additional text properties
for mouse highlighting.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu."
  (propertize (buffer-name)
              'help-echo "mouse-2: select this buffer, mouse-3: select in other frame"
              'mouse-face 'highlight))

(defun bs--get-mode-name (start-buffer _all-buffers)
  "Return the name of mode of current buffer for Buffer Selection Menu.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu."
  (format-mode-line mode-name nil nil start-buffer))

(defun bs--get-file-name (_start-buffer _all-buffers)
  "Return string for column 'File' in Buffer Selection Menu.
This is the variable `buffer-file-name' of current buffer.
If not visiting a file, `list-buffers-directory' is returned instead.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffers appearing in Buffer Selection Menu."
  (propertize (or buffer-file-name
		  (bound-and-true-p list-buffers-directory)
		  "")
              'mouse-face 'highlight
              'help-echo "mouse-2: select this buffer, mouse-3: select in other frame"))

(defun bs--insert-one-entry (buffer)
  "Generate one entry for buffer BUFFER in Buffer Selection Menu.
It goes over all columns described in `bs-attributes-list'
and evaluates corresponding string.  Inserts string in current buffer;
normally *buffer-selection*."
  (let ((string "")
	(to-much 0)
        (apply-args (append (list bs--buffer-coming-from bs-current-list))))
    (with-current-buffer buffer
      (dolist (column bs-attributes-list)
	(let* ((min (bs--get-value (nth 1 column)))
	       (new-string (bs--format-aux (bs--get-value (nth 4 column) ; fun
							  apply-args)
					   (nth 3 column)                ; align
					   (- min to-much)))
	       (len (length new-string)))
	  (setq string (concat string new-string))
	  (when (> len min)
	    (setq to-much (- len min))))))
    (insert string)))

(defun bs--format-aux (string align len)
  "Pad STRING to length LEN with alignment ALIGN.
ALIGN is one of the symbols `left', `middle', or `right'."
  (let* ((width (length string))
         (len (max len width)))
    (format (format "%%%s%ds" (if (eq align 'right) "" "-") len)
            (if (eq align 'middle)
                (concat (make-string (/ (- len width) 2) ?\s) string)
              string))))

(defun bs--show-header ()
  "Insert header for Buffer Selection Menu in current buffer."
  (insert (bs--create-header-line #'identity)
	  "\n"
	  (bs--create-header-line (lambda (title)
				    (make-string (length title) ?-)))
	  "\n"))

(defun bs--get-name-length ()
  "Return value of `bs--name-entry-length'."
  bs--name-entry-length)

(defun bs--create-header-line (col)
  "Generate a line for the header.
COL is called for each column in `bs-attributes-list' as a
function of one argument, the string heading for the column."
  (mapconcat (lambda (column)
	       (bs--format-aux (funcall col (bs--get-value (car column)))
			       (nth 3 column) ; align
			       (bs--get-value (nth 1 column))))
	     bs-attributes-list
	     ""))

(defun bs--show-with-configuration (name &optional arg)
  "Display buffer list of configuration with name NAME.
Set configuration NAME and determine window for Buffer Selection Menu.
Unless current buffer is buffer *buffer-selection* we have to save
the buffer we started Buffer Selection Menu and the current window
configuration to restore buffer and window configuration after a
selection.  If there is already a window displaying *buffer-selection*
select this window for Buffer Selection Menu.  Otherwise open a new
window.
The optional argument ARG is the prefix argument when calling a function
for buffer selection."
  (bs-set-configuration name)
  (let ((bs--show-all (or bs--show-all arg)))
    (unless (string= "*buffer-selection*" (buffer-name))
      ;; Only when not in buffer *buffer-selection*
      ;; we have to set the buffer we started the command
      (setq bs--buffer-coming-from (current-buffer)))
    (let ((liste (bs-buffer-list))
	  (active-window (get-window-with-predicate
			  (lambda (w)
			    (string= (buffer-name (window-buffer w))
				     "*buffer-selection*"))
			  nil (selected-frame))))
      (if active-window
	  (select-window active-window)
	(bs--restore-window-config)
	(setq bs--window-config-coming-from (current-window-configuration))
	(when (> (window-height (selected-window)) 7)
          ;; Errors would mess with the window configuration (bug#10882).
          (ignore-errors (select-window (split-window-below)))))
      (bs-show-in-buffer liste)
      (bs-message-without-log "%s" (bs--current-config-message)))))

(defun bs--configuration-name-for-prefix-arg (prefix)
  "Convert prefix argument PREFIX to a name of a buffer configuration.
If PREFIX is nil return `bs-default-configuration'.
If PREFIX is an integer return PREFIX element of `bs-configurations'.
Otherwise return `bs-alternative-configuration'."
  (cond ;; usually activation
   ((null prefix)
    bs-default-configuration)
   ;; call with integer as prefix argument
   ((integerp prefix)
    (if (and (< 0 prefix) (<= prefix (length bs-configurations)))
	(car (nth (1- prefix) bs-configurations))
      bs-default-configuration))
   ;; call by prefix argument C-u
   (t bs-alternative-configuration)))

;; ----------------------------------------------------------------------
;; Main function bs-customize and bs-show
;; ----------------------------------------------------------------------

;;;###autoload
(defun bs-customize ()
  "Customization of group bs for Buffer Selection Menu."
  (interactive)
  (customize-group "bs"))

;;;###autoload
(defun bs-show (arg)
  "Make a menu of buffers so you can manipulate buffers or the buffer list.
\\<bs-mode-map>
There are many key commands similar to `Buffer-menu-mode' for
manipulating the buffer list and the buffers themselves.
User can move with [up] or [down], select a buffer
by \\[bs-select] or [SPC]\n
Type \\[bs-kill] to leave Buffer Selection Menu without a selection.
Type \\[bs-help] after invocation to get help on commands available.
With prefix argument ARG show a different buffer list.  Function
`bs--configuration-name-for-prefix-arg' determine accordingly
name of buffer configuration."
  (interactive "P")
  (setq bs--marked-buffers nil)
  (bs--show-with-configuration (bs--configuration-name-for-prefix-arg arg)))

;; ----------------------------------------------------------------------
;; Cleanup
;; ----------------------------------------------------------------------

(defun bs-unload-function ()
  "Unload the Buffer Selection library."
  (let ((bs-buf (get-buffer "*buffer-selection*")))
    (when bs-buf
      (with-current-buffer bs-buf
	(when (eq major-mode 'bs-mode)
	  (bs-kill)
	  (kill-buffer bs-buf)))))
  ;; continue standard unloading
  nil)

;; Now provide feature bs
(provide 'bs)

;;; bs.el ends here
