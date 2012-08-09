;;; dcl-mode.el --- major mode for editing DCL command files

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Odd Gripenstam <gripenstamol@decus.se>
;; Maintainer: Odd Gripenstam <gripenstamol@decus.se>
;; Keywords: DCL editing major-mode languages

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

;; DCL mode is a package for editing DCL command files.  It helps you
;; indent lines, add leading `$' and trailing `-', move around in the
;; code and insert lexical functions.
;;
;; Type `C-h m' when you are editing a .COM file to get more
;; information about this mode.
;;
;; To use templates you will need a version of tempo.el that is at
;; least later than the buggy 1.1.1, which was included with my versions of
;; Emacs.  I used version 1.2.4.
;; The latest tempo.el distribution can be fetched from
;; ftp.lysator.liu.se in the directory /pub/emacs.
;; I recommend setting (setq tempo-interactive t).  This will make
;; tempo prompt you for values to put in the blank spots in the templates.
;;
;; There is limited support for imenu.  The limitation is that you need
;; a version of imenu.el that uses imenu-generic-expression.  I found
;; the version I use in Emacs 19.30.  (It was *so* much easier to hook
;; into that version than the one in 19.27...)
;;
;; Any feedback will be welcomed.  If you write functions for
;; dcl-calc-command-indent-function or dcl-calc-cont-indent-function,
;; please send them to the maintainer.
;;
;;
;; Ideas for improvement:
;; * Better font-lock support.
;; * Change meaning of `left margin' when dcl-tab-always-indent is nil.
;;   Consider the following line (`_' is the cursor):
;;     $ label: _ command
;;   Pressing tab with the cursor at the underline now inserts a tab.
;;   This should be part of the left margin and pressing tab should indent
;;   the line.
;; * Make M-LFD work properly with comments in all cases.  Now it only
;;   works on comment-only lines.  But what is "properly"? New rules for
;;   indenting comments?
;; * Even smarter indentation of continuation lines.
;; * A delete-indentation function (M-^) that joins continued lines,
;;   including lines with end line comments?
;; * Handle DECK/EOD.
;; * `indent list' commands: C-M-q, C-u TAB.  What is a list in DCL? One
;;   complete command line? A block? A subroutine?

;;; Code:

(require 'tempo)

;;; *** Customization *****************************************************


;; First, font lock.  This is a minimal approach, please improve!

(defvar dcl-font-lock-keywords
  '(("\\<\\(if\\|then\\|else\\|endif\\)\\>"
     1 font-lock-keyword-face)
    ("\\<f[$][a-z_]+\\>"
     0 font-lock-builtin-face)
    ("[.]\\(eq\\|not\\|or\\|and\\|lt\\|gt\\|le\\|ge\\|eqs\\|nes\\)[.]"
     0 font-lock-builtin-face))
  "Font lock keyword specification for DCL mode.
Presently this includes some syntax, .OP.erators, and \"f$\" lexicals.")

(defvar dcl-font-lock-defaults
  '(dcl-font-lock-keywords nil)
  "Font lock specification for DCL mode.")


;; Now the rest.

(defgroup dcl nil
  "Major mode for editing DCL command files."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom dcl-basic-offset 4
  "*Number of columns to indent a block in DCL.
A block is the commands between THEN-ELSE-ENDIF and between the commands
dcl-block-begin-regexp and dcl-block-end-regexp.

The meaning of this variable may be changed if
dcl-calc-command-indent-function is set to a function."
  :type 'integer
  :group 'dcl)


(defcustom dcl-continuation-offset 6
  "*Number of columns to indent a continuation line in DCL.
A continuation line is a line that follows a line ending with `-'.

The meaning of this variable may be changed if
dcl-calc-cont-indent-function is set to a function."
  :type 'integer
  :group 'dcl)


(defcustom dcl-margin-offset 8
  "*Indentation for the first command line in DCL.
The first command line in a file or after a SUBROUTINE statement is indented
this much.  Other command lines are indented the same number of columns as
the preceding command line.
A command line is a line that starts with `$'."
  :type 'integer
  :group 'dcl)


(defcustom dcl-margin-label-offset 2
  "*Number of columns to indent a margin label in DCL.
A margin label is a label that doesn't begin or end a block, i.e. it
doesn't match dcl-block-begin-regexp or dcl-block-end-regexp."
  :type 'integer
  :group 'dcl)


(defcustom dcl-comment-line-regexp "^\\$!"
  "*Regexp describing the start of a comment line in DCL.
Comment lines are not indented."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-block-begin-regexp "loop[0-9]*:"
  "*Regexp describing a command that begins an indented block in DCL.
Set to nil to only indent at THEN-ELSE-ENDIF."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-block-end-regexp "endloop[0-9]*:"
  "*Regexp describing a command that ends an indented block in DCL.
Set to nil to only indent at THEN-ELSE-ENDIF."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-calc-command-indent-function nil
  "*Function to calculate indentation for a command line in DCL.
If this variable is non-nil it is called as a function:

\(func INDENT-TYPE CUR-INDENT EXTRA-INDENT LAST-POINT THIS-POINT)

The function must return the number of columns to indent the current line or
nil to get the default indentation.

INDENT-TYPE is a symbol indicating what kind of indentation should be done.
It can have the following values:
  indent      the lines indentation should be increased, e.g. after THEN.
  outdent     the lines indentation should be decreased, e.g a line with ENDIF.
  first-line  indentation for the first line in a buffer or SUBROUTINE.
CUR-INDENT is the indentation of the preceding command line.
EXTRA-INDENT is the default change in indentation for this line
\(a negative number for 'outdent).
LAST-POINT is the buffer position of the first significant word on the
previous line or nil if the current line is the first line.
THIS-POINT is the buffer position of the first significant word on the
current line.

If this variable is nil, the indentation is calculated as
CUR-INDENT + EXTRA-INDENT.

This package includes two functions suitable for this:
  dcl-calc-command-indent-multiple
  dcl-calc-command-indent-hang"
  :type '(choice (const nil) function)
  :group 'dcl)


(defcustom dcl-calc-cont-indent-function 'dcl-calc-cont-indent-relative
  "*Function to calculate indentation for a continuation line.
If this variable is non-nil it is called as a function:

\(func CUR-INDENT EXTRA-INDENT)

The function must return the number of columns to indent the current line or
nil to get the default indentation.

If this variable is nil, the indentation is calculated as
CUR-INDENT + EXTRA-INDENT.

This package includes one function suitable for this:
  dcl-calc-cont-indent-relative"
  :type 'function
  :group 'dcl)


(defcustom dcl-tab-always-indent t
  "*Controls the operation of the TAB key in DCL mode.
If t, pressing TAB always indents the current line.
If nil, pressing TAB indents the current line if point is at the left margin.
Data lines (i.e. lines not part of a command line or continuation line) are
never indented."
  :type 'boolean
  :group 'dcl)


(defcustom dcl-electric-characters t
  "*Non-nil means reindent immediately when a label, ELSE or ENDIF is inserted."
  :type 'boolean
  :group 'dcl)


(defcustom dcl-tempo-comma ", "
  "*Text to insert when a comma is needed in a template, in DCL mode."
  :type 'string
  :group 'dcl)

(defcustom dcl-tempo-left-paren "("
  "*Text to insert when a left parenthesis is needed in a template in DCL."
  :type 'string
  :group 'dcl)


(defcustom dcl-tempo-right-paren ")"
  "*Text to insert when a right parenthesis is needed in a template in DCL."
  :type 'string
  :group 'dcl)

; I couldn't decide what looked best, so I'll let you decide...
; Remember, you can also customize this with imenu-submenu-name-format.
(defcustom dcl-imenu-label-labels "Labels"
  "*Imenu menu title for sub-listing with label names."
  :type 'string
  :group 'dcl)
(defcustom dcl-imenu-label-goto "GOTO"
  "*Imenu menu title for sub-listing with GOTO statements."
  :type 'string
  :group 'dcl)
(defcustom dcl-imenu-label-gosub "GOSUB"
  "*Imenu menu title for sub-listing with GOSUB statements."
  :type 'string
  :group 'dcl)
(defcustom dcl-imenu-label-call "CALL"
  "*Imenu menu title for sub-listing with CALL statements."
  :type 'string
  :group 'dcl)

(defcustom dcl-imenu-generic-expression
  `((nil "^\\$[ \t]*\\([A-Za-z0-9_\$]+\\):[ \t]+SUBROUTINE\\b" 1)
    (,dcl-imenu-label-labels
     "^\\$[ \t]*\\([A-Za-z0-9_\$]+\\):\\([ \t]\\|$\\)" 1)
    (,dcl-imenu-label-goto "\\s-GOTO[ \t]+\\([A-Za-z0-9_\$]+\\)" 1)
    (,dcl-imenu-label-gosub "\\s-GOSUB[ \t]+\\([A-Za-z0-9_\$]+\\)" 1)
    (,dcl-imenu-label-call "\\s-CALL[ \t]+\\([A-Za-z0-9_\$]+\\)" 1))
  "*Default imenu generic expression for DCL.

The default includes SUBROUTINE labels in the main listing and
sub-listings for other labels, CALL, GOTO and GOSUB statements.
See `imenu-generic-expression' for details."
  :type '(repeat (sexp :tag "Imenu Expression"))
  :group 'dcl)


(defcustom dcl-mode-hook nil
  "*Hook called by `dcl-mode'."
  :type 'hook
  :group 'dcl)


;;; *** Global variables ****************************************************


(defvar dcl-mode-syntax-table nil
  "Syntax table used in DCL-buffers.")
(unless dcl-mode-syntax-table
  (setq dcl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?!  "<" dcl-mode-syntax-table) ; comment start
  (modify-syntax-entry ?\n ">" dcl-mode-syntax-table) ; comment end
  (modify-syntax-entry ?< "(>" dcl-mode-syntax-table) ; < and ...
  (modify-syntax-entry ?> ")<" dcl-mode-syntax-table) ; > is a matching pair
  (modify-syntax-entry ?\\ "_" dcl-mode-syntax-table) ; not an escape
)


(defvar dcl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\n"	'dcl-split-line)
    (define-key map "\e\t" 	'tempo-complete-tag)
    (define-key map "\e^"	'dcl-delete-indentation)
    (define-key map "\em"	'dcl-back-to-indentation)
    (define-key map "\ee"        'dcl-forward-command)
    (define-key map "\ea"        'dcl-backward-command)
    (define-key map "\e\C-q" 	'dcl-indent-command)
    (define-key map "\t"         'dcl-tab)
    (define-key map ":"          'dcl-electric-character)
    (define-key map "F"          'dcl-electric-character)
    (define-key map "f"          'dcl-electric-character)
    (define-key map "E"          'dcl-electric-character)
    (define-key map "e"          'dcl-electric-character)
    (define-key map "\C-c\C-o" 	'dcl-set-option)
    (define-key map "\C-c\C-f" 	'tempo-forward-mark)
    (define-key map "\C-c\C-b" 	'tempo-backward-mark)

    (define-key map [menu-bar] 	(make-sparse-keymap))
    (define-key map [menu-bar dcl]
      (cons "DCL" (make-sparse-keymap "DCL")))

    ;; Define these in bottom-up order
    (define-key map [menu-bar dcl tempo-backward-mark]
      '("Previous template mark" . tempo-backward-mark))
    (define-key map [menu-bar dcl tempo-forward-mark]
      '("Next template mark" . tempo-forward-mark))
    (define-key map [menu-bar dcl tempo-complete-tag]
      '("Complete template tag" . tempo-complete-tag))
    (define-key map [menu-bar dcl dcl-separator-tempo]
      '("--"))
    (define-key map [menu-bar dcl dcl-save-all-options]
      '("Save all options" . dcl-save-all-options))
    (define-key map [menu-bar dcl dcl-save-nondefault-options]
      '("Save changed options" . dcl-save-nondefault-options))
    (define-key map [menu-bar dcl dcl-set-option]
      '("Set option" . dcl-set-option))
    (define-key map [menu-bar dcl dcl-separator-option]
      '("--"))
    (define-key map [menu-bar dcl dcl-delete-indentation]
      '("Delete indentation" . dcl-delete-indentation))
    (define-key map [menu-bar dcl dcl-split-line]
      '("Split line" . dcl-split-line))
    (define-key map [menu-bar dcl dcl-indent-command]
      '("Indent command" . dcl-indent-command))
    (define-key map [menu-bar dcl dcl-tab]
      '("Indent line/insert tab" . dcl-tab))
    (define-key map [menu-bar dcl dcl-back-to-indentation]
      '("Back to indentation" . dcl-back-to-indentation))
    (define-key map [menu-bar dcl dcl-forward-command]
      '("End of statement" . dcl-forward-command))
    (define-key map [menu-bar dcl dcl-backward-command]
      '("Beginning of statement" . dcl-backward-command))
    ;; imenu is only supported for versions with imenu-generic-expression
    (if (boundp 'imenu-generic-expression)
        (progn
          (define-key map [menu-bar dcl dcl-separator-movement]
            '("--"))
          (define-key map [menu-bar dcl imenu]
            '("Buffer index menu" . imenu))))
    map)
  "Keymap used in DCL-mode buffers.")

(defcustom dcl-ws-r
  "\\([ \t]*-[ \t]*\\(!.*\\)*\n\\)*[ \t]*"
  "Regular expression describing white space in a DCL command line.
White space is any number of continued lines with only space,tab,endcomment
followed by space or tab."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-label-r
  "[a-zA-Z0-9_\$]*:\\([ \t!]\\|$\\)"
  "Regular expression describing a label.
A label is a name followed by a colon followed by white-space or end-of-line."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-cmd-r
  "^\\$\\(.*-[ \t]*\\(!.*\\)*\n\\)*[^!\"\n]*\\(\".*\\(\"\".*\\)*\"\\)*[^!\"\n]*"
  "Regular expression describing a DCL command line up to a trailing comment.
A line starting with $, optionally followed by continuation lines,
followed by the end of the command line.
A continuation line is any characters followed by `-',
optionally followed by a comment, followed by a newline."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-command-regexp
  "^\\$\\(.*-[ \t]*\\(!.*\\)*\n\\)*.*\\(\".*\\(\"\".*\\)*\"\\)*"
  "Regular expression describing a DCL command line.
A line starting with $, optionally followed by continuation lines,
followed by the end of the command line.
A continuation line is any characters followed by `-',
optionally followed by a comment, followed by a newline."
  :type 'regexp
  :group 'dcl)


(defcustom dcl-electric-reindent-regexps
  (list "endif" "else" dcl-label-r)
  "*Regexps that can trigger an electric reindent.
A list of regexps that will trigger a reindent if the last letter
is defined as dcl-electric-character.

E.g.: if this list contains `endif', the key `f' is defined as
dcl-electric-character and you have just typed the `f' in
`endif', the line will be reindented."
  :type '(repeat regexp)
  :group 'dcl)


(defvar dcl-option-alist
  '((dcl-basic-offset dcl-option-value-basic)
    (dcl-continuation-offset curval)
    (dcl-margin-offset dcl-option-value-margin-offset)
    (dcl-margin-label-offset dcl-option-value-offset)
    (dcl-comment-line-regexp dcl-option-value-comment-line)
    (dcl-block-begin-regexp curval)
    (dcl-block-end-regexp curval)
    (dcl-tab-always-indent toggle)
    (dcl-electric-characters toggle)
    (dcl-electric-reindent-regexps curval)
    (dcl-tempo-comma curval)
    (dcl-tempo-left-paren curval)
    (dcl-tempo-right-paren curval)
    (dcl-calc-command-indent-function curval)
    (dcl-calc-cont-indent-function curval)
    (comment-start curval)
    (comment-start-skip curval)
    )
  "Options and default values for dcl-set-option.

An alist with option variables and functions or keywords to get a
default value for the option.

The keywords are:
curval       the current value
toggle       the opposite of the current value (for t/nil)")


(defvar dcl-option-history
  (mapcar (lambda (option-assoc)
	    (format "%s" (car option-assoc)))
	  dcl-option-alist)
  "The history list for dcl-set-option.
Preloaded with all known option names from dcl-option-alist")


;; Must be defined after dcl-cmd-r
;; This version is more correct but much slower than the one
;; above.  This version won't find GOTOs in comments or text strings.
;(defvar dcl-imenu-generic-expression
;  (`
;   ((nil "^\\$[ \t]*\\([A-Za-z0-9_\$]+\\):[ \t]+SUBROUTINE\\b" 1)
;    ("Labels" "^\\$[ \t]*\\([A-Za-z0-9_\$]+\\):\\([ \t]\\|$\\)" 1)
;    ("GOTO" (, (concat dcl-cmd-r "GOTO[ \t]+\\([A-Za-z0-9_\$]+\\)")) 5)
;    ("GOSUB" (, (concat dcl-cmd-r
;			"GOSUB[ \t]+\\([A-Za-z0-9_\$]+\\)")) 5)
;    ("CALL" (, (concat dcl-cmd-r "CALL[ \t]+\\([A-Za-z0-9_\$]+\\)")) 5)))
;  "*Default imenu generic expression for DCL.

;The default includes SUBROUTINE labels in the main listing and
;sub-listings for other labels, CALL, GOTO and GOSUB statements.
;See `imenu-generic-expression' in a recent (e.g. Emacs 19.30) imenu.el
;for details.")


;;; *** Mode initialization *************************************************


;;;###autoload
(define-derived-mode dcl-mode prog-mode "DCL"
  "Major mode for editing DCL-files.

This mode indents command lines in blocks.  (A block is commands between
THEN-ELSE-ENDIF and between lines matching dcl-block-begin-regexp and
dcl-block-end-regexp.)

Labels are indented to a fixed position unless they begin or end a block.
Whole-line comments (matching dcl-comment-line-regexp) are not indented.
Data lines are not indented.

Key bindings:

\\{dcl-mode-map}
Commands not usually bound to keys:

\\[dcl-save-nondefault-options]\t\tSave changed options
\\[dcl-save-all-options]\t\tSave all options
\\[dcl-save-option]\t\t\tSave any option
\\[dcl-save-mode]\t\t\tSave buffer mode

Variables controlling indentation style and extra features:

 dcl-basic-offset
    Extra indentation within blocks.

 dcl-continuation-offset
    Extra indentation for continued lines.

 dcl-margin-offset
    Indentation for the first command line in a file or SUBROUTINE.

 dcl-margin-label-offset
    Indentation for a label.

 dcl-comment-line-regexp
    Lines matching this regexp will not be indented.

 dcl-block-begin-regexp
 dcl-block-end-regexp
    Regexps that match command lines that begin and end, respectively,
    a block of command lines that will be given extra indentation.
    Command lines between THEN-ELSE-ENDIF are always indented; these variables
    make it possible to define other places to indent.
    Set to nil to disable this feature.

 dcl-calc-command-indent-function
    Can be set to a function that customizes indentation for command lines.
    Two such functions are included in the package:
	dcl-calc-command-indent-multiple
	dcl-calc-command-indent-hang

 dcl-calc-cont-indent-function
    Can be set to a function that customizes indentation for continued lines.
    One such function is included in the package:
	dcl-calc-cont-indent-relative    (set by default)

 dcl-tab-always-indent
    If t, pressing TAB always indents the current line.
    If nil, pressing TAB indents the current line if point is at the left
    margin.

 dcl-electric-characters
    Non-nil causes lines to be indented at once when a label, ELSE or ENDIF is
    typed.

 dcl-electric-reindent-regexps
    Use this variable and function dcl-electric-character to customize
    which words trigger electric indentation.

 dcl-tempo-comma
 dcl-tempo-left-paren
 dcl-tempo-right-paren
    These variables control the look of expanded templates.

 dcl-imenu-generic-expression
    Default value for imenu-generic-expression.  The default includes
    SUBROUTINE labels in the main listing and sub-listings for
    other labels, CALL, GOTO and GOSUB statements.

 dcl-imenu-label-labels
 dcl-imenu-label-goto
 dcl-imenu-label-gosub
 dcl-imenu-label-call
    Change the text that is used as sub-listing labels in imenu.

Loading this package calls the value of the variable
`dcl-mode-load-hook' with no args, if that value is non-nil.
Turning on DCL mode calls the value of the variable `dcl-mode-hook'
with no args, if that value is non-nil.


The following example uses the default values for all variables:

$! This is a comment line that is not indented (it matches
$! dcl-comment-line-regexp)
$! Next follows the first command line.  It is indented dcl-margin-offset.
$       i = 1
$       ! Other comments are indented like command lines.
$       ! A margin label indented dcl-margin-label-offset:
$ label:
$       if i.eq.1
$       then
$           ! Lines between THEN-ELSE and ELSE-ENDIF are
$           ! indented dcl-basic-offset
$           loop1: ! This matches dcl-block-begin-regexp...
$               ! ...so this line is indented dcl-basic-offset
$               text = \"This \" + - ! is a continued line
                       \"lined up with the command line\"
$               type sys$input
Data lines are not indented at all.
$           endloop1: ! This matches dcl-block-end-regexp
$       endif
$


There is some minimal font-lock support (see vars
`dcl-font-lock-defaults' and `dcl-font-lock-keywords')."
  (set (make-local-variable 'indent-line-function) 'dcl-indent-line)
  (set (make-local-variable 'comment-start) "!")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)

  ;; This used to be "^\\$[ \t]*![ \t]*" which looks more correct.
  ;; The drawback was that you couldn't make empty comment lines by pressing
  ;; C-M-j repeatedly - only the first line became a comment line.
  ;; This version has the drawback that the "$" can be anywhere in the line,
  ;; and something inappropriate might be interpreted as a comment.
  (set (make-local-variable 'comment-start-skip) "\\$[ \t]*![ \t]*")

  (if (boundp 'imenu-generic-expression)
      (progn (setq imenu-generic-expression dcl-imenu-generic-expression)
             (setq imenu-case-fold-search t)))
  (setq imenu-create-index-function 'dcl-imenu-create-index-function)

  (make-local-variable 'dcl-comment-line-regexp)
  (make-local-variable 'dcl-block-begin-regexp)
  (make-local-variable 'dcl-block-end-regexp)
  (make-local-variable 'dcl-basic-offset)
  (make-local-variable 'dcl-continuation-offset)
  (make-local-variable 'dcl-margin-label-offset)
  (make-local-variable 'dcl-margin-offset)
  (make-local-variable 'dcl-tab-always-indent)
  (make-local-variable 'dcl-electric-characters)
  (make-local-variable 'dcl-calc-command-indent-function)
  (make-local-variable 'dcl-calc-cont-indent-function)
  (make-local-variable 'dcl-electric-reindent-regexps)

  ;; font lock
  (set (make-local-variable 'font-lock-defaults) dcl-font-lock-defaults)

  (tempo-use-tag-list 'dcl-tempo-tags))


;;; *** Movement commands ***************************************************


;;;-------------------------------------------------------------------------
(defun dcl-beginning-of-statement ()
  "Go to the beginning of the preceding or current command line."
  (interactive)
  (re-search-backward dcl-command-regexp nil t))


;;;-------------------------------------------------------------------------
(defun dcl-end-of-statement ()
  "Go to the end of the next or current command line."
  (interactive)
  (if (or (dcl-end-of-command-p)
	  (dcl-beginning-of-command-p)
	  (not (dcl-command-p)))
      ()
    (dcl-beginning-of-statement))
  (re-search-forward dcl-command-regexp nil t))


;;;-------------------------------------------------------------------------
(defun dcl-beginning-of-command ()
  "Move point to beginning of current command."
  (interactive)
  (let ((type (dcl-get-line-type)))
    (if (and (eq type '$)
	     (bolp))
	() ; already in the correct position
      (dcl-beginning-of-statement))))


;;;-------------------------------------------------------------------------
(defun dcl-end-of-command ()
  "Move point to end of current command or next command if not on a command."
  (interactive)
  (let ((type (dcl-get-line-type)))
    (if (or (eq type '$)
	    (eq type '-))
	(progn
	  (dcl-beginning-of-command)
	  (dcl-end-of-statement))
      (dcl-end-of-statement))))


;;;-------------------------------------------------------------------------
(defun dcl-backward-command (&optional incl-comment-commands)
  "Move backward to a command.
Move point to the preceding command line that is not a comment line,
a command line with only a comment, only contains a `$' or only
contains a label.

Returns point of the found command line or nil if not able to move."
  (interactive)
  (let ((start (point))
	done
	retval)
    ;; Find first non-empty command line
    (while (not done)
      ;; back up one statement and look at the command
      (if (dcl-beginning-of-statement)
	  (cond
	   ((and dcl-block-begin-regexp ; might be nil
		 (looking-at (concat "^\\$" dcl-ws-r
				     dcl-block-begin-regexp)))
	    (setq done t retval (point)))
	   ((and dcl-block-end-regexp	; might be nil
		 (looking-at (concat "^\\$" dcl-ws-r
				     dcl-block-end-regexp)))
	    (setq done t retval (point)))
	   ((looking-at dcl-comment-line-regexp)
	    t)				; comment line, one more loop
	   ((and (not incl-comment-commands)
		 (looking-at "\\$[ \t]*!"))
	    t)				; comment only command, loop...
	   ((looking-at "^\\$[ \t]*$")
	    t)				; empty line, one more loop
	   ((not (looking-at
		  (concat "^\\$" dcl-ws-r dcl-label-r dcl-ws-r "$")))
	    (setq done t)		; not a label-only line, exit the loop
	    (setq retval (point))))
	;; We couldn't go further back, and we haven't found a command yet.
	;; Return to the start position.
	(goto-char start)
	(setq done t)
	(setq retval nil)))
    retval))


;;;-------------------------------------------------------------------------
(defun dcl-forward-command (&optional incl-comment-commands)
  "Move forward to a command.
Move point to the end of the next command line that is not a comment line,
a command line with only a comment, only contains a `$' or only
contains a label.

Returns point of the found command line or nil if not able to move."
  (interactive)
  (let ((start (point))
	done
	retval)
    ;; Find first non-empty command line
    (while (not done)
      ;; go forward one statement and look at the command
      (if (dcl-end-of-statement)
	  (save-excursion
	    (dcl-beginning-of-statement)
	    (cond
	     ((and dcl-block-begin-regexp ; might be nil
		   (looking-at (concat "^\\$" dcl-ws-r
				       dcl-block-begin-regexp)))
	      (setq done t)
	      (setq retval (point)))
	     ((and dcl-block-end-regexp	; might be nil
		   (looking-at (concat "^\\$" dcl-ws-r
				       dcl-block-end-regexp)))
	      (setq done t)
	      (setq retval (point)))
	     ((looking-at dcl-comment-line-regexp)
	      t)			; comment line, one more loop
	     ((and (not incl-comment-commands)
		   (looking-at "\\$[ \t]*!"))
	      t)			; comment only command, loop...
	     ((looking-at "^\\$[ \t]*$")
	      t)			; empty line, one more loop
	     ((not (looking-at
		    (concat "^\\$" dcl-ws-r dcl-label-r dcl-ws-r "$")))
	      (setq done t)		; not a label-only line, exit the loop
	      (setq retval (point)))))
	;; We couldn't go further back, and we haven't found a command yet.
	;; Return to the start position.
	(goto-char start)
	(setq done t)
	(setq retval nil)))
    retval))


;;;-------------------------------------------------------------------------
(defun dcl-back-to-indentation ()
  "Move point to the first non-whitespace character on this line.
Leading $ and labels counts as whitespace in this case.
If this is a comment line then move to the first non-whitespace character
in the comment.

Typing \\[dcl-back-to-indentation] several times in a row will move point to other
`interesting' points closer to the left margin, and then back to the
rightmost point again.

E.g. on the following line, point would go to the positions indicated
by the numbers in order 1-2-3-1-... :

  $ label: command
  3 2      1"
  (interactive)
  (if (eq last-command 'dcl-back-to-indentation)
      (dcl-back-to-indentation-1 (point))
    (dcl-back-to-indentation-1)))
(defun dcl-back-to-indentation-1 (&optional limit)
  "Helper function for dcl-back-to-indentation"

  ;; "Indentation points" that we will travel to
  ;;  $  l:  !  comment
  ;;  4  3   2  1
  ;;
  ;;  $  !  text
  ;;  3  2  1
  ;;
  ;;  $  l:  command  !
  ;;  3  2   1
  ;;
  ;;  text
  ;;  1

  (let* ((default-limit (1+ (line-end-position)))
	 (limit (or limit default-limit))
	 (last-good-point (point))
	 (opoint (point)))
    ;; Move over blanks
    (back-to-indentation)

    ;; If we already were at the outermost indentation point then we
    ;; start searching for the innermost point again.
    (if (= (point) opoint)
	(setq limit default-limit))

    (if (< (point) limit)
	(setq last-good-point (point)))

    (cond
     ;; Special treatment for comment lines.  We are trying to allow
     ;; things like "$ !*" as comment lines.
     ((looking-at dcl-comment-line-regexp)
      (re-search-forward (concat dcl-comment-line-regexp "[ \t]*") limit t)
      (if (< (point) limit)
	  (setq last-good-point (point))))

     ;; Normal command line
     ((looking-at "^\\$[ \t]*")
      ;; Move over leading "$" and blanks
      (re-search-forward "^\\$[ \t]*" limit t)
      (if (< (point) limit)
	  (setq last-good-point (point)))

      ;; Move over a label (if it isn't a block begin/end)
      ;; We must treat block begin/end labels as commands because
      ;; dcl-set-option relies on it.
      (if (and (looking-at dcl-label-r)
	       (not (or (and dcl-block-begin-regexp
			     (looking-at dcl-block-begin-regexp))
			(and dcl-block-end-regexp
			     (looking-at dcl-block-end-regexp)))))
	  (re-search-forward (concat dcl-label-r "[ \t]*") limit t))
      (if (< (point) limit)
	  (setq last-good-point (point)))

      ;; Move over the beginning of a comment
      (if (looking-at "![ \t]*")
	  (re-search-forward "![ \t]*" limit t))
      (if (< (point) limit)
	  (setq last-good-point (point)))))
    (goto-char last-good-point)))


;;; *** Support for indentation *********************************************


(defun dcl-get-line-type ()
  "Determine the type of the current line.
Returns one of the following symbols:
  $          for a complete command line or the beginning of a command line.
  -          for a continuation line
  $!         for a comment line
  data       for a data line
  empty-data for an empty line following a data line
  empty-$    for an empty line following a command line"
  (or
   ;; Check if it's a comment line.
   ;; A comment line starts with $!
   (save-excursion
    (beginning-of-line)
    (if (looking-at dcl-comment-line-regexp)
        '$!))
   ;; Check if it's a command line.
   ;; A command line starts with $
   (save-excursion
     (beginning-of-line)
     (if (looking-at "^\\$")
         '$))
   ;; Check if it's a continuation line
   (save-excursion
     (beginning-of-line)
     ;; If we're at the beginning of the buffer it can't be a continuation
     (if (bobp)
         ()
       (let ((opoint (point)))
         (dcl-beginning-of-statement)
         (re-search-forward dcl-command-regexp opoint t)
         (if (>= (point) opoint)
             '-))))
   ;; Empty lines might be different things
   (save-excursion
     (if (and (bolp) (eolp))
         (if (bobp)
             'empty-$
           (forward-line -1)
           (let ((type (dcl-get-line-type)))
             (cond
              ((or (equal type '$) (equal type '$!) (equal type '-))
               'empty-$)
              ((equal type 'data)
               'empty-data))))))
   ;; Anything else must be a data line
   (progn 'data)
   ))


;;;-------------------------------------------------------------------------
(defun dcl-indentation-point ()
  "Return point of first non-`whitespace' on this line."
  (save-excursion
    (dcl-back-to-indentation)
    (point)))


;;;---------------------------------------------------------------------------
(defun dcl-show-line-type ()
  "Test dcl-get-line-type."
  (interactive)
  (let ((type (dcl-get-line-type)))
    (cond
     ((equal type '$)
      (message "command line"))
     ((equal type '\?)
      (message "?"))
     ((equal type '$!)
      (message "comment line"))
     ((equal type '-)
      (message "continuation line"))
     ((equal type 'data)
      (message "data"))
     ((equal type 'empty-data)
      (message "empty-data"))
     ((equal type 'empty-$)
      (message "empty-$"))
     (t
      (message "hupp"))
     )))


;;; *** Perform indentation *************************************************


;;;---------------------------------------------------------------------------
(defun dcl-calc-command-indent-multiple
  (indent-type cur-indent extra-indent _last-point _this-point)
  "Indent lines to a multiple of dcl-basic-offset.

Set dcl-calc-command-indent-function to this function to customize
indentation of command lines.

Command lines that need to be indented beyond the left margin are
always indented to a column that is a multiple of dcl-basic-offset, as
if tab stops were set at 4, 8, 12, etc.

This supports a formatting style like this (dcl-margin offset = 2,
dcl-basic-offset = 4):

$ if cond
$ then
$   if cond
$   then
$       ! etc
"
  ;; calculate indentation if it's an interesting indent-type,
  ;; otherwise return nil to get the default indentation
  (let ((indent))
    (cond
     ((equal indent-type 'indent)
      (setq indent (- cur-indent (% cur-indent dcl-basic-offset)))
      (setq indent (+ indent extra-indent))))))


;;;---------------------------------------------------------------------------
;; Some people actually writes likes this.  To each his own...
(defun dcl-calc-command-indent-hang
  (indent-type cur-indent extra-indent last-point this-point)
  "Indent lines as default, but indent THEN, ELSE and ENDIF extra.

Set dcl-calc-command-indent-function to this function to customize
indentation of command lines.

This function supports a formatting style like this:

$ if cond
$   then
$     xxx
$   endif
$ xxx

If you use this function you will probably want to add \"then\" to
dcl-electric-reindent-regexps and define the key \"n\" as
dcl-electric-character.
"
  (let ((case-fold-search t))
    (save-excursion
      (cond
       ;; No indentation, this word is `then': +2
       ;; last word was endif: -2
       ((null indent-type)
	(or (progn
	      (goto-char this-point)
	      (if (looking-at "\\bthen\\b")
		  (+ cur-indent extra-indent 2)))
	    (progn
	      (goto-char last-point)
	      (if (looking-at "\\bendif\\b")
		  (- (+ cur-indent extra-indent) 2)))))
       ;; Indentation, last word was `then' or `else': -2
       ((equal indent-type 'indent)
	(goto-char last-point)
	(cond
	 ((looking-at "\\bthen\\b")
	  (- (+ cur-indent extra-indent) 2))
	 ((looking-at "\\belse\\b")
	  (- (+ cur-indent extra-indent) 2))))
       ;; Outdent, this word is `endif' or `else': + 2
       ((equal indent-type 'outdent)
	(goto-char this-point)
	(cond
	 ((looking-at "\\bendif\\b")
	  (+ cur-indent extra-indent 2))
	 ((looking-at "\\belse\\b")
	  (+ cur-indent extra-indent 2))))))))


;;;---------------------------------------------------------------------------
(defun dcl-calc-command-indent ()
  "Calculate how much the current line shall be indented.
The line is known to be a command line.

Find the indentation of the preceding line and analyze its contents to
see if the current lines should be indented.
Analyze the current line to see if it should be `outdented'.

Calculate the indentation of the current line, either with the default
method or by calling dcl-calc-command-indent-function if it is
non-nil.

If the current line should be outdented, calculate its indentation,
either with the default method or by calling
dcl-calc-command-indent-function if it is non-nil.


Rules for default indentation:

If it is the first line in the buffer, indent dcl-margin-offset.

Go to the previous command line with a command on it.
Find out how much it is indented (cur-indent).
Look at the first word on the line to see if the indentation should be
adjusted.  Skip margin-label, continuations and comments while looking for
the first word.  Save this buffer position as `last-point'.
If the first word after a label is SUBROUTINE, set extra-indent to
dcl-margin-offset.

First word  extra-indent
THEN        +dcl-basic-offset
ELSE        +dcl-basic-offset
block-begin +dcl-basic-offset

Then return to the current line and look at the first word to see if the
indentation should be adjusted again.  Save this buffer position as
`this-point'.

First word  extra-indent
ELSE        -dcl-basic-offset
ENDIF       -dcl-basic-offset
block-end   -dcl-basic-offset


If dcl-calc-command-indent-function is nil or returns nil set
cur-indent to cur-indent+extra-indent.

If an extra adjustment is necessary and if
dcl-calc-command-indent-function is nil or returns nil set cur-indent
to cur-indent+extra-indent.

See also documentation for dcl-calc-command-indent-function.
The indent-type classification could probably be expanded upon.
"
  ()
  (save-excursion
    (beginning-of-line)
    (let ((is-block nil)
	  (case-fold-search t)
	  cur-indent
	  (extra-indent 0)
	  indent-type last-point this-point extra-indent2 cur-indent2
	  indent-type2)
      (if (bobp)			; first line in buffer
	  (setq cur-indent 0 extra-indent dcl-margin-offset
		indent-type 'first-line
		this-point (dcl-indentation-point))
        (save-excursion
          (let (done)
	    ;; Find first non-empty command line
            (while (not done)
	      ;; back up one statement and look at the command
              (if (dcl-beginning-of-statement)
                  (cond
                   ((and dcl-block-begin-regexp ; might be nil
                         (looking-at (concat "^\\$" dcl-ws-r
                                             dcl-block-begin-regexp)))
                    (setq done t) (setq is-block t))
                   ((and dcl-block-end-regexp ; might be nil
                         (looking-at (concat "^\\$" dcl-ws-r
                                             dcl-block-end-regexp)))
                    (setq done t) (setq is-block t))
                   ((looking-at dcl-comment-line-regexp)
                    t) ; comment line, one more loop
                   ((looking-at "^\\$[ \t]*$")
                    t) ; empty line, one more loop
                   ((not (looking-at
                          (concat "^\\$" dcl-ws-r dcl-label-r dcl-ws-r "$")))
                    (setq done t))) ; not a label-only line, exit the loop
                ;; We couldn't go further back, so this must have been the
		;; first line.
                (setq cur-indent dcl-margin-offset
		      last-point (dcl-indentation-point))
                (setq done t)))
	    ;; Examine the line to get current indentation and possibly a
	    ;; reason to indent.
            (cond
             (cur-indent)
             ((looking-at (concat "^\\$[ \t]*" dcl-label-r dcl-ws-r
                                  "\\(subroutine\\b\\)"))
              (setq cur-indent dcl-margin-offset
		    last-point (1+ (match-beginning 1))))
             (t
              ;; Find out how much this line is indented.
              ;; Look at comment, continuation character, command but not label
              ;; unless it's a block.
              (if is-block
                  (re-search-forward "^\\$[ \t]*")
                (re-search-forward (concat "^\\$[ \t]*\\(" dcl-label-r
                                           "\\)*[ \t]*")))
              (setq cur-indent (current-column))
              ;; Look for a reason to indent: Find first word on this line
              (re-search-forward dcl-ws-r)
	      (setq last-point (point))
              (cond
               ((looking-at "\\bthen\\b")
                (setq extra-indent dcl-basic-offset indent-type 'indent))
               ((looking-at "\\belse\\b")
                (setq extra-indent dcl-basic-offset indent-type 'indent))
               ((and dcl-block-begin-regexp ; might be nil
                     (looking-at dcl-block-begin-regexp))
                (setq extra-indent dcl-basic-offset indent-type 'indent))
               ))))))
	(setq extra-indent2 0)
        ;; We're back at the beginning of the original line.
        ;; Look for a reason to outdent: Find first word on this line
        (re-search-forward (concat "^\\$" dcl-ws-r))
	(setq this-point (dcl-indentation-point))
        (cond
         ((looking-at "\\belse\\b")
          (setq extra-indent2 (- dcl-basic-offset) indent-type2 'outdent))
         ((looking-at "\\bendif\\b")
          (setq extra-indent2 (- dcl-basic-offset) indent-type2 'outdent))
         ((and dcl-block-end-regexp ; might be nil
               (looking-at dcl-block-end-regexp))
          (setq extra-indent2 (- dcl-basic-offset) indent-type2 'outdent))
         ((looking-at (concat dcl-label-r dcl-ws-r "\\(subroutine\\b\\)"))
          (setq cur-indent2 0 extra-indent2 dcl-margin-offset
		indent-type2 'first-line
		this-point (1+ (match-beginning 1)))))
	;; Calculate indent
	(setq cur-indent
	      (or (and dcl-calc-command-indent-function
		       (funcall dcl-calc-command-indent-function
				indent-type cur-indent extra-indent
				last-point this-point))
		  (+ cur-indent extra-indent)))
	;; Calculate outdent
	(if indent-type2
	    (progn
	      (or cur-indent2 (setq cur-indent2 cur-indent))
	      (setq cur-indent
		    (or (and dcl-calc-command-indent-function
			     (funcall dcl-calc-command-indent-function
				      indent-type2 cur-indent2 extra-indent2
				      last-point this-point))
			(+ cur-indent2 extra-indent2)))))
	cur-indent
        )))


;;;---------------------------------------------------------------------------
(defun dcl-calc-cont-indent-relative (_cur-indent _extra-indent)
  "Indent continuation lines to align with words on previous line.

Indent continuation lines to a position relative to preceding
significant command line elements.

Set `dcl-calc-cont-indent-function' to this function to customize
indentation of continuation lines.

Indented lines will align with either:

* the second word on the command line
  $ set default -
        [-]
* the word after an assignment
  $ a = b + -
        d
* the third word if it's a qualifier
  $ set terminal/width=80 -
                /page=24
* the innermost nonclosed parenthesis
  $ if ((a.eq.b .and. -
         d.eq.c .or. f$function(xxxx, -
                                yyy)))
"
  (let ((case-fold-search t)
	indent)
    (save-excursion
      (dcl-beginning-of-statement)
      (let ((end (save-excursion (forward-line 1) (point))))
	;; Move over blanks and label
	(if (re-search-forward (concat "^\\$[ \t]*\\(" dcl-label-r
				       "\\)*[ \t]*") end t)
	    (progn
	      ;; Move over the first word (might be `@filespec')
	      (if (> (skip-chars-forward "@:[]<>$\\-a-zA-Z0-9_.;" end) 0)
		  (let (was-assignment)
		    (skip-chars-forward " \t" end)
		    ;; skip over assignment if there is one
		    (if (looking-at ":?==?")
			(progn
			  (setq was-assignment t)
			  (skip-chars-forward " \t:=" end)))
		    ;; This could be the position to indent to
		    (setq indent (current-column))

		    ;; Move to the next word unless we have seen an
		    ;; assignment.  If it starts with `/' it's a
		    ;; qualifier and we will indent to that position
		    (if (and (not was-assignment)
			     (> (skip-chars-forward "a-zA-Z0-9_" end) 0))
			(progn
			  (skip-chars-forward " \t" end)
			  (if (= (char-after (point)) ?/)
			      (setq indent (current-column)))))
		    ))))))
    ;; Now check if there are any parenthesis to adjust to.
    ;; If there is, we will indent to the position after the last non-closed
    ;; opening parenthesis.
    (save-excursion
      (beginning-of-line)
      (let* ((start (save-excursion (dcl-beginning-of-statement) (point)))
	     (parse-sexp-ignore-comments t) ; for parse-partial
	     (par-pos (nth 1 (parse-partial-sexp start (point)))))
	(if par-pos		; is nil if no parenthesis was found
	    (setq indent (save-excursion
			   (goto-char par-pos)
			   (1+ (current-column)))))))
    indent))


;;;---------------------------------------------------------------------------
(defun dcl-calc-continuation-indent ()
  "Calculate how much the current line shall be indented.
The line is known to be a continuation line.

Go to the previous command line.
Find out how much it is indented."
;; This was copied without much thought from dcl-calc-command-indent, so
;; it's a bit clumsy.
  ()
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	;; Huh? a continuation line first in the buffer??
        dcl-margin-offset
      (let ((is-block nil)
            (indent))
        (save-excursion
          ;; Find first non-empty command line
          (let ((done))
            (while (not done)
              (if (dcl-beginning-of-statement)
                  (cond
                   ((and dcl-block-begin-regexp
                         (looking-at (concat "^\\$" dcl-ws-r
                                             dcl-block-begin-regexp)))
                    (setq done t) (setq is-block t))
                   ((and dcl-block-end-regexp
                         (looking-at (concat "^\\$" dcl-ws-r
                                             dcl-block-end-regexp)))
                    (setq done t) (setq is-block t))
                   ((looking-at dcl-comment-line-regexp)
                    t)
                   ((looking-at "^\\$[ \t]*$")
                    t)
                   ((not (looking-at
                          (concat "^\\$" dcl-ws-r dcl-label-r dcl-ws-r "$")))
                    (setq done t)))
                ;; This must have been the first line.
                (setq indent dcl-margin-offset)
                (setq done t)))
            (if indent
                ()
              ;; Find out how much this line is indented.
              ;; Look at comment, continuation character, command but not label
              ;; unless it's a block.
              (if is-block
                  (re-search-forward "^\\$[ \t]*")
                (re-search-forward (concat "^\\$[ \t]*\\(" dcl-label-r
                                           "\\)*[ \t]*")))
              (setq indent (current-column))
              )))
          ;; We're back at the beginning of the original line.
	  (or (and dcl-calc-cont-indent-function
		   (funcall dcl-calc-cont-indent-function indent
			    dcl-continuation-offset))
	      (+ indent dcl-continuation-offset))
        ))))


;;;---------------------------------------------------------------------------
(defun dcl-indent-command-line ()
  "Indent a line known to be a command line."
  (let ((indent (dcl-calc-command-indent))
        (pos (- (point-max) (point))))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "^\\$[ \t]*")
      ;; Indent any margin-label if the offset is set
      ;; (Don't look at block labels)
      (if (and dcl-margin-label-offset
               (looking-at dcl-label-r)
               (not (and dcl-block-begin-regexp
                         (looking-at dcl-block-begin-regexp)))
               (not (and dcl-block-end-regexp
                         (looking-at dcl-block-end-regexp))))
          (progn
            (dcl-indent-to dcl-margin-label-offset)
            (re-search-forward dcl-label-r)))
      (dcl-indent-to indent 1)
      )
    ;;
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))
    ))


;;;-------------------------------------------------------------------------
(defun dcl-indent-continuation-line ()
  "Indent a line known to be a continuation line.

Notice that no special treatment is made for labels.  They have to be
on the first part on a command line to be taken into consideration."
  (let ((indent (dcl-calc-continuation-indent)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "^[ \t]*")
      (dcl-indent-to indent))
    (skip-chars-forward " \t")))


;;;---------------------------------------------------------------------------
(defun dcl-delete-chars (chars)
  "Delete all characters in the set CHARS around point."
  (skip-chars-backward chars)
  (delete-region (point) (progn  (skip-chars-forward chars) (point))))


;;;---------------------------------------------------------------------------
(defun dcl-indent-line ()
  "The DCL version of `indent-line-function'.
Adjusts indentation on the current line.  Data lines are not indented."
  (let ((type (dcl-get-line-type)))
    (cond
     ((equal type '$)
      (dcl-indent-command-line))
     ((equal type '\?)
      (message "Unknown line type!"))
     ((equal type '$!))
     ((equal type 'data))
     ((equal type 'empty-data))
     ((equal type '-)
      (dcl-indent-continuation-line))
     ((equal type 'empty-$)
      (insert "$" )
      (dcl-indent-command-line))
     (t
      (message "dcl-indent-line: unknown type"))
     )))


;;;-------------------------------------------------------------------------
(defun dcl-indent-command ()
  "Indents the complete command line that point is on.
This includes continuation lines."
  (interactive "*")
  (let ((type (dcl-get-line-type)))
    (if (or (equal type '$)
	    (equal type '-)
	    (equal type 'empty-$))
	(save-excursion
	  (indent-region (progn (or (looking-at "^\\$")
				    (dcl-beginning-of-statement))
				(point))
			 (progn (dcl-end-of-statement) (point))
			 nil)))))


;;;-------------------------------------------------------------------------
(defun dcl-tab ()
  "Insert tab in data lines or indent code.
If `dcl-tab-always-indent' is t, code lines are always indented.
If nil, indent the current line only if point is at the left margin or in
the lines indentation; otherwise insert a tab."
  (interactive "*")
  (let ((type (dcl-get-line-type))
        (start-point (point)))
    (cond
     ;; Data line : always insert tab
     ((or (equal type 'data) (equal type 'empty-data))
      (tab-to-tab-stop))
     ;; Indent only at start of line
     ((not dcl-tab-always-indent)       ; nil
      (let ((search-end-point
             (save-excursion
               (beginning-of-line)
               (re-search-forward "^\\$?[ \t]*" start-point t))))
        (if (or (bolp)
                (and search-end-point
                     (>= search-end-point start-point)))
            (dcl-indent-line)
          (tab-to-tab-stop))))
      ;; Always indent
      ((eq dcl-tab-always-indent t)     ; t
      (dcl-indent-line))
     )))


;;;-------------------------------------------------------------------------
(defun dcl-electric-character (arg)
  "Inserts a character and indents if necessary.
Insert a character if the user gave a numeric argument or the flag
`dcl-electric-characters' is not set.  If an argument was given,
insert that many characters.

The line is only reindented if the word just typed matches any of the
regexps in `dcl-electric-reindent-regexps'."
  (interactive "*P")
  (if (or arg (not dcl-electric-characters))
      (if arg
          (self-insert-command (prefix-numeric-value arg))
        (self-insert-command 1))
    ;; Insert the character and indent
    (self-insert-command 1)
    (let ((case-fold-search t))
      ;; There must be a better way than (memq t ...).
      ;; (apply 'or ...) didn't work
      (if (memq t (mapcar 'dcl-was-looking-at dcl-electric-reindent-regexps))
          (dcl-indent-line)))))


;;;-------------------------------------------------------------------------
(defun dcl-indent-to (col &optional minimum)
  "Like indent-to, but only indents if indentation would change"
  (interactive)
  (let (cur-indent collapsed indent)
    (save-excursion
      (skip-chars-forward " \t")
      (setq cur-indent (current-column))
      (skip-chars-backward " \t")
      (setq collapsed (current-column)))
    (setq indent (max col (+ collapsed (or minimum 0))))
    (if (/= indent cur-indent)
	(progn
	  (dcl-delete-chars " \t")
	  (indent-to col minimum)))))


;;;-------------------------------------------------------------------------
(defun dcl-split-line ()
  "Break line at point and insert text to keep the syntax valid.

Inserts continuation marks and splits character strings."
  ;; Still don't know what to do with comments at the end of a command line.
  (interactive "*")
  (let (done
	(type (dcl-get-line-type)))
    (cond
     ((or (equal type '$) (equal type '-))
      (let ((info (parse-partial-sexp
		   (save-excursion (dcl-beginning-of-statement) (point))
		   (point))))
	;; handle some special cases
	(cond
	 ((nth 3 info)			; in text constant
	  (insert "\" + -\n\"")
	  (indent-according-to-mode)
	  (setq done t))
	 ((not (nth 4 info))		; not in comment
	  (cond
	   ((and (not (eolp))
		 (= (char-after (point)) ?\")
		 (= (char-after (1- (point))) ?\"))
	    (progn			; a " "" " situation
	      (forward-char -1)
	      (insert "\" + -\n\"")
	      (forward-char 1)
	      (indent-according-to-mode)
	      (setq done t)))
	   ((and (dcl-was-looking-at "[ \t]*-[ \t]*") ; after cont mark
		 (looking-at "[ \t]*\\(!.*\\)?$"))
	    ;; Do default below.  This might considered wrong if we're
	    ;; after a subtraction:  $ x = 3 - <M-LFD>
	    )
	   (t
	    (delete-horizontal-space)
	    (insert " -")
	    (insert "\n") (indent-according-to-mode)
	    (setq done t))))
	 ))))
    ;; use the normal function for other cases
    (if (not done)			; normal M-LFD action
	(indent-new-comment-line))))


;;;-------------------------------------------------------------------------
(defun dcl-delete-indentation (&optional arg)
  "Join this line to previous like delete-indentation.
Also remove the continuation mark if easily detected."
  (interactive "*P")
  (delete-indentation arg)
  (let ((type (dcl-get-line-type)))
    (if (and (member type '($ - empty-$))
	     (not (bobp))
	     (= (char-before) ?-))
	(progn
	  (delete-char -1)
	  (fixup-whitespace)))))


;;; *** Set options *********************************************************


;;;-------------------------------------------------------------------------
(defun dcl-option-value-basic (_option-assoc)
  "Guess a value for basic-offset."
  (save-excursion
    (dcl-beginning-of-command)
    (let* (;; current lines indentation
	   (this-indent (save-excursion
			  (dcl-back-to-indentation)
			  (current-column)))
	   ;; previous lines indentation
	   (prev-indent (save-excursion
			  (if (dcl-backward-command)
			      (progn
				(dcl-back-to-indentation)
				(current-column)))))
	   (next-indent (save-excursion
			  (dcl-end-of-command)
			  (if (dcl-forward-command)
			      (progn
				(dcl-beginning-of-command)
				(dcl-back-to-indentation)
				(current-column)))))
	   (diff (if prev-indent
		     (abs (- this-indent prev-indent)))))
      (cond
       ((and diff
	     (/= diff 0))
	diff)
       ((and next-indent
	     (/= (- this-indent next-indent) 0))
	(abs (- this-indent next-indent)))
       (t
	dcl-basic-offset)))))


;;;-------------------------------------------------------------------------
(defun dcl-option-value-offset (_option-assoc)
  "Guess a value for an offset.
Find the column of the first non-blank character on the line.
Returns the column offset."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^$[ \t]*" nil t)
    (current-column)))


;;;-------------------------------------------------------------------------
(defun dcl-option-value-margin-offset (_option-assoc)
  "Guess a value for margin offset.
Find the column of the first non-blank character on the line, not
counting labels.
Returns a number as a string."
  (save-excursion
    (beginning-of-line)
    (dcl-back-to-indentation)
    (current-column)))


;;;-------------------------------------------------------------------------
(defun dcl-option-value-comment-line (_option-assoc)
  "Guess a value for `dcl-comment-line-regexp'.
Must return a string."
  ;; Should we set comment-start and comment-start-skip as well?
  ;; If someone wants `$!&' as a comment line, C-M-j won't work well if
  ;; they aren't set.
  ;; This must be done after the user has given the real value in
  ;; dcl-set-option.
  (format
   "%S"
   (save-excursion
     (beginning-of-line)
     ;; We could search for "^\\$.*!+[^ \t]*", but, as noted above, we
     ;; can't handle that case very good, so there is no point in
     ;; suggesting it.
     (if (looking-at "^\\$[^!\n]*!")
	 (let ((regexp (buffer-substring (match-beginning 0) (match-end 0))))
	   (concat "^" (regexp-quote regexp)))
       dcl-comment-line-regexp))))


;;;-------------------------------------------------------------------------
(defun dcl-guess-option-value (option)
  "Guess what value the user would like to give the symbol option."
  (let* ((option-assoc (assoc option dcl-option-alist))
	 (option (car option-assoc))
	 (action (car (cdr option-assoc)))
	 (value (cond
		 ((fboundp action)
		  (funcall action option-assoc))
		 ((eq action 'toggle)
		  (not (eval option)))
		 ((eq action 'curval)
		  (cond ((or (stringp (symbol-value option))
			     (numberp (symbol-value option)))
			 (format "%S" (symbol-value option)))
			(t
			 (format "'%S" (symbol-value option))))))))
    ;; format the value as a string if not already done
    (if (stringp value)
	value
      (format "%S" value))))


;;;-------------------------------------------------------------------------
(defun dcl-guess-option ()
  "Guess what option the user wants to set by looking around in the code.
Returns the name of the option variable as a string."
  (let ((case-fold-search t))
    (cond
     ;; Continued line
     ((eq (dcl-get-line-type) '-)
      "dcl-calc-cont-indent-function")
     ;; Comment line
     ((save-excursion
	(beginning-of-line)
	(looking-at "^\\$[ \t]*!"))
      "dcl-comment-line-regexp")
     ;; Margin offset: subroutine statement or first line in buffer
     ;; Test this before label indentation to detect a subroutine
     ((save-excursion
	(beginning-of-line)
	(or (looking-at (concat "^\\$[ \t]*" dcl-label-r dcl-ws-r
				"subroutine"))
	    (save-excursion
	      (not (dcl-backward-command t)))))
      "dcl-margin-offset")
     ;; Margin offset: on command line after subroutine statement
     ((save-excursion
	(beginning-of-line)
	(and (eq (dcl-get-line-type) '$)
	     (dcl-backward-command)
	     (looking-at (concat "^\\$[ \t]*" dcl-label-r dcl-ws-r
				 "subroutine"))))
      "dcl-margin-offset")
     ;; Label indentation
     ((save-excursion
	(beginning-of-line)
	(and (looking-at (concat "^\\$[ \t]*" dcl-label-r))
	     (not (and dcl-block-begin-regexp
		       (looking-at (concat "^\\$[ \t]*"
					   dcl-block-begin-regexp))))
	     (not (and dcl-block-end-regexp
		       (looking-at (concat "^\\$[ \t]*"
					   dcl-block-end-regexp))))))
      "dcl-margin-label-offset")
     ;; Basic offset
     ((and (eq (dcl-get-line-type) '$)	; beginning of command
	   (save-excursion
	     (beginning-of-line)
	     (let* ((this-indent (save-excursion
				   (dcl-back-to-indentation)
				   (current-column)))
		    (prev-indent (save-excursion
				   (if (dcl-backward-command)
				       (progn
					 (dcl-back-to-indentation)
					 (current-column)))))
		    (next-indent (save-excursion
				   (dcl-end-of-command)
				   (if (dcl-forward-command)
				       (progn
					 (dcl-beginning-of-command)
					 (dcl-back-to-indentation)
					 (current-column))))))
	       (or (and prev-indent	; last cmd is indented differently
			(/= (- this-indent prev-indent) 0))
		   (and next-indent
			(/= (- this-indent next-indent) 0))))))
      "dcl-basic-offset")
     ;; No more guesses.
     (t
      ""))))


;;;-------------------------------------------------------------------------
(defun dcl-set-option (option-sym option-value)
  "Set a value for one of the dcl customization variables.
The function tries to guess which variable should be set and to what value.
All variable names are available as completions and in the history list."
  (interactive
   (let* ((option-sym
	   (intern (completing-read
		    "Set DCL option: " ; prompt
		    (mapcar (function  ; alist of valid values
			     (lambda (option-assoc)
			       (cons  (format "%s" (car option-assoc)) nil)))
			    dcl-option-alist)
		    nil                   ; no predicate
		    t                     ; only value from the list OK
		    (dcl-guess-option)    ; initial (default) value
		    'dcl-option-history))) ; history list
	  (option-value
	   (eval-minibuffer
	    (format "Set DCL option %s to: " option-sym)
	    (dcl-guess-option-value option-sym))))
     (list option-sym option-value)))
  ;; Should make a sanity check on the symbol/value pair.
  ;; `set' instead of `setq' because we want option-sym to be evaluated.
  (set option-sym option-value))


;;; *** Save options ********************************************************


;;;-------------------------------------------------------------------------
(defun dcl-save-local-variable (var &optional def-prefix def-suffix)
  "Save a variable in a `Local Variables' list.
Set or update the value of VAR in the current buffers
`Local Variables:' list."
  ;; Look for "Local variables:" line in last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (if (let ((case-fold-search t))
	  (search-forward "Local Variables:" nil t))
	(let ((continue t)
	      prefix prefixlen suffix beg
	      prefix-string suffix-string)
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
	  (skip-chars-forward " \t")
	  (or (eolp)
	      (setq suffix-string (buffer-substring (point)
                                                    (line-end-position))))
	  (goto-char (match-beginning 0))
	  (or (bolp)
	      (setq prefix-string
		    (buffer-substring (point)
				      (progn (beginning-of-line) (point)))))

	  (if prefix-string (setq prefixlen (length prefix-string)
				  prefix (regexp-quote prefix-string)))
	  (if suffix-string (setq suffix (concat (regexp-quote suffix-string)
						 "$")))
	  (while continue
	    ;; Look at next local variable spec.
	    (if selective-display (re-search-forward "[\n\C-m]")
	      (forward-line 1))
	    ;; Skip the prefix, if any.
	    (if prefix
		(if (looking-at prefix)
		    (forward-char prefixlen)
		  (error "Local variables entry is missing the prefix")))
	    ;; Find the variable name; strip whitespace.
	    (skip-chars-forward " \t")
	    (setq beg (point))
	    (skip-chars-forward "^:\n")
	    (if (eolp) (error "Missing colon in local variables entry"))
	    (skip-chars-backward " \t")
	    (let* ((str (buffer-substring beg (point)))
		   (found-var (read str)))
	      ;; Setting variable named "end" means end of list.
	      (if (string-equal (downcase str) "end")
		  (progn
		    ;; Not found.  Insert a new entry before this line
		    (setq continue nil)
		    (beginning-of-line)
		    (insert (concat prefix-string (symbol-name var) ": "
				    (prin1-to-string (eval var)) " "
				    suffix-string "\n")))
		;; Is it the variable we are looking for?
		(if (eq var found-var)
		    (progn
		      ;; Found it: delete the variable value and insert the
		      ;; new value.
		      (setq continue nil)
		      (skip-chars-forward "^:")
		      (forward-char 1)
		      (delete-region (point) (progn (read (current-buffer))
						    (point)))
		      (insert " ")
		      (prin1 (eval var) (current-buffer))
		      (skip-chars-backward "\n")
		      (skip-chars-forward " \t")
		      (or (if suffix (looking-at suffix) (eolp))
			  (error
			   "Local variables entry is terminated incorrectly")))
		  (end-of-line))))))
      ;; Did not find "Local variables:"
      (goto-char (point-max))
      (if (not (bolp))
	  (insert "\n"))
      ;; If def- parameter not set, use comment- if set.  In that case, make
      ;; sure there is a space in a suitable position
      (let ((def-prefix
	      (cond
	       (def-prefix
		 def-prefix)
	       (comment-start
   		(if (or (equal comment-start "")
   			(string-match "[ \t]$" comment-start))
		    comment-start
		  (concat comment-start " ")))))
	    (def-suffix
	      (cond
	       (def-suffix
		 def-suffix)
	       (comment-end
		(if (or (equal comment-end "")
			(string-match "^[ \t]" comment-end))
		    comment-end
		  (concat " " comment-end))))))
	(insert (concat def-prefix "Local variables:" def-suffix "\n"))
	(insert (concat def-prefix (symbol-name var) ": "
			(prin1-to-string (eval var)) def-suffix "\n"))
	(insert (concat def-prefix "end:" def-suffix)))
      )))


;;;-------------------------------------------------------------------------
(defun dcl-save-all-options ()
  "Save all dcl-mode options for this buffer.
Saves or updates all dcl-mode related options in a `Local Variables:'
section at the end of the current buffer."
  (interactive "*")
  (mapcar (lambda (option-assoc)
	    (let* ((option (car option-assoc)))
	      (dcl-save-local-variable option "$! ")))
	  dcl-option-alist))


;;;-------------------------------------------------------------------------
(defun dcl-save-nondefault-options ()
  "Save changed DCL mode options for this buffer.
Saves or updates all DCL mode related options that don't have their
default values in a `Local Variables:' section at the end of the
current buffer.

No entries are removed from the `Local Variables:' section.  This means
that if a variable is given a non-default value in the section and
later is manually reset to its default value, the variable's entry will
still be present in the `Local Variables:' section with its old value."
  (interactive "*")
  (mapcar (lambda (option-assoc)
	    (let* ((option (car option-assoc))
		   (option-name (symbol-name option)))
	      (if (and (string-equal "dcl-"
				     (substring option-name 0 4))
		       (not (equal (default-value option) (eval option))))
		  (dcl-save-local-variable option "$! "))))
	  dcl-option-alist))


;;;-------------------------------------------------------------------------
(defun dcl-save-option (option)
  "Save a DCL mode option for this buffer.
Saves or updates an option in a `Local Variables:'
section at the end of the current buffer."
  (interactive
   (let ((option (intern (completing-read "Option: " obarray))))
     (list option)))
  (dcl-save-local-variable option))


;;;-------------------------------------------------------------------------
(with-no-warnings
  ;; Dynamically bound in `dcl-save-mode'.
  (defvar mode))

(defun dcl-save-mode ()
  "Save the current mode for this buffer.
Save the current mode in a `Local Variables:'
section at the end of the current buffer."
  (interactive)
  (let ((mode (prin1-to-string major-mode)))
    (if (string-match "-mode$" mode)
	(let ((mode (intern (substring mode 0 (match-beginning 0)))))
	  (dcl-save-option 'mode))
      (message "Strange mode: %s" mode))))


;;; *** Templates ***********************************************************
;; tempo seems to be the only suitable package among those included in
;; standard Emacs.  I would have liked something closer to the functionality
;; of LSE templates...

(defvar dcl-tempo-tags nil
  "Tempo tags for DCL mode.")

(tempo-define-template "dcl-f$context"
		       '("f$context" dcl-tempo-left-paren
			 (p "context-type: ") dcl-tempo-comma
			 (p "context-symbol: ") dcl-tempo-comma
			 (p "selection-item: ") dcl-tempo-comma
			 (p "selection-value: ") dcl-tempo-comma
			 (p "value-qualifier: ") dcl-tempo-right-paren)
		       "f$context" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$csid"
		       '("f$csid" dcl-tempo-left-paren
			 (p "context-symbol: ") dcl-tempo-right-paren)
		       "f$csid" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$cvsi"
		       '("f$cvsi" dcl-tempo-left-paren
			 (p "start-bit: ") dcl-tempo-comma
			 (p "number-of-bits: ") dcl-tempo-comma
			 (p "string: ") dcl-tempo-right-paren)
		       "f$cvsi" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$cvtime"
		       '("f$cvtime" dcl-tempo-left-paren
			 (p "[input_time]: ") dcl-tempo-comma
			 (p "[output_time_format]: ") dcl-tempo-comma
			 (p "[output_field]: ") dcl-tempo-right-paren)
		       "f$cvtime" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$cvui"
		       '("f$cvui" dcl-tempo-left-paren
			 (p "start-bit: ") dcl-tempo-comma
			 (p "number-of-bits: ") dcl-tempo-comma
			 (p "string") dcl-tempo-right-paren)
		       "f$cvui" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$device"
		       '("f$device" dcl-tempo-left-paren
			 (p "[search_devnam]: ") dcl-tempo-comma
			 (p "[devclass]: ") dcl-tempo-comma
			 (p "[devtype]: ") dcl-tempo-comma
			 (p "[stream-id]: ") dcl-tempo-right-paren)
		       "f$device" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$directory"
		       '("f$directory" dcl-tempo-left-paren
			 dcl-tempo-right-paren)
		       "f$directory" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$edit"
		       '("f$edit" dcl-tempo-left-paren
			 (p "string: ") dcl-tempo-comma
			 (p "edit-list: ") dcl-tempo-right-paren)
		       "f$edit" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$element"
		       '("f$element" dcl-tempo-left-paren
			 (p "element-number: ") dcl-tempo-comma
			 (p "delimiter: ") dcl-tempo-comma
			 (p "string: ") dcl-tempo-right-paren)
		       "f$element" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$environment"
		       '("f$environment" dcl-tempo-left-paren
			 (p "item: ") dcl-tempo-right-paren)
		       "f$environment" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$extract"
		       '("f$extract" dcl-tempo-left-paren
			 (p "start: ") dcl-tempo-comma
			 (p "length: ") dcl-tempo-comma
			 (p "string: ") dcl-tempo-right-paren)
		       "f$extract" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$fao"
		       '("f$fao" dcl-tempo-left-paren
			 (p "control-string: ") dcl-tempo-comma
			 ("argument[,...]: ") dcl-tempo-right-paren)
		       "f$fao" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$file_attributes"
		       '("f$file_attributes" dcl-tempo-left-paren
			 (p "filespec: ") dcl-tempo-comma
			 (p "item: ") dcl-tempo-right-paren)
		       "f$file_attributes" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$getdvi"
		       '("f$getdvi" dcl-tempo-left-paren
			 (p "device-name: ") dcl-tempo-comma
			 (p "item: ") dcl-tempo-right-paren)
		       "f$getdvi" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$getjpi"
		       '("f$getjpi" dcl-tempo-left-paren
			 (p "pid: ") dcl-tempo-comma
			 (p "item: ") dcl-tempo-right-paren )
		       "f$getjpi" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$getqui"
		       '("f$getqui" dcl-tempo-left-paren
			 (p "function: ") dcl-tempo-comma
			 (p "[item]: ") dcl-tempo-comma
			 (p "[object-id]: ") dcl-tempo-comma
			 (p "[flags]: ") dcl-tempo-right-paren)
		       "f$getqui" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$getsyi"
		       '("f$getsyi" dcl-tempo-left-paren
			 (p "item: ") dcl-tempo-comma
			 (p "[node-name]: ") dcl-tempo-comma
			 (p "[cluster-id]: ") dcl-tempo-right-paren)
		       "f$getsyi" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$identifier"
		       '("f$identifier" dcl-tempo-left-paren
			 (p "identifier: ") dcl-tempo-comma
			 (p "conversion-type: ") dcl-tempo-right-paren)
		       "f$identifier" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$integer"
		       '("f$integer" dcl-tempo-left-paren
			 (p "expression: ") dcl-tempo-right-paren)
		       "f$integer" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$length"
		       '("f$length" dcl-tempo-left-paren
			 (p "string: ") dcl-tempo-right-paren )
		       "f$length" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$locate"
		       '("f$locate" dcl-tempo-left-paren
			 (p "substring: ") dcl-tempo-comma
			 (p "string: ") dcl-tempo-right-paren)
		       "f$locate" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$message"
		       '("f$message" dcl-tempo-left-paren
			 (p "status-code: ") dcl-tempo-right-paren )
		       "f$message" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$mode"
		       '("f$mode" dcl-tempo-left-paren dcl-tempo-right-paren)
		       "f$mode" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$parse"
		       '("f$parse" dcl-tempo-left-paren
			 (p "filespec: ") dcl-tempo-comma
			 (p "[default-spec]: ") dcl-tempo-comma
			 (p "[related-spec]: ") dcl-tempo-comma
			 (p "[field]: ") dcl-tempo-comma
			 (p "[parse-type]: ") dcl-tempo-right-paren)
		       "f$parse" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$pid"
		       '("f$pid" dcl-tempo-left-paren
			 (p "context-symbol: ") dcl-tempo-right-paren)
		       "f$pid" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$privilege"
		       '("f$privilege" dcl-tempo-left-paren
			 (p "priv-states: ") dcl-tempo-right-paren)
		       "f$privilege" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$process"
		       '("f$process()")
		       "f$process" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$search"
		       '("f$search" dcl-tempo-left-paren
			 (p "filespec: ") dcl-tempo-comma
			 (p "[stream-id]: ") dcl-tempo-right-paren)
		       "f$search" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$setprv"
		       '("f$setprv" dcl-tempo-left-paren
			 (p "priv-states: ") dcl-tempo-right-paren)
		       "f$setprv" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$string"
		       '("f$string" dcl-tempo-left-paren
			 (p "expression: ") dcl-tempo-right-paren)
		       "f$string" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$time"
		       '("f$time" dcl-tempo-left-paren dcl-tempo-right-paren)
		       "f$time" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$trnlnm"
		       '("f$trnlnm" dcl-tempo-left-paren
			 (p "logical-name: ") dcl-tempo-comma
			 (p "[table]: ") dcl-tempo-comma
			 (p "[index]: ") dcl-tempo-comma
			 (p "[mode]: ") dcl-tempo-comma
			 (p "[case]: ") dcl-tempo-comma
			 (p "[item]: ") dcl-tempo-right-paren)
		       "f$trnlnm" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$type"
		       '("f$type" dcl-tempo-left-paren
			 (p "symbol-name: ") dcl-tempo-right-paren)
		       "f$type" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$user"
		       '("f$user" dcl-tempo-left-paren dcl-tempo-right-paren)
		       "f$user" "" 'dcl-tempo-tags)

(tempo-define-template "dcl-f$verify"
		       '("f$verify" dcl-tempo-left-paren
			 (p "[procedure-value]: ") dcl-tempo-comma
			 (p "[image-value]: ") dcl-tempo-right-paren)
		       "f$verify" "" 'dcl-tempo-tags)




;;; *** Unsorted stuff  *****************************************************


;;;-------------------------------------------------------------------------
(defun dcl-beginning-of-command-p ()
  "Return t if point is at the beginning of a command.
Otherwise return nil."
  (and (bolp)
       (eq (dcl-get-line-type) '$)))


;;;-------------------------------------------------------------------------
(defun dcl-end-of-command-p ()
  "Check if point is at the end of a command.
Return t if point is at the end of a command, either the end of an
only line or at the end of the last continuation line.
Otherwise return nil."
  ;; Must be at end-of-line on a command line or a continuation line
  (let ((type (dcl-get-line-type)))
    (if (and (eolp)
	     (or (eq type '$)
		 (eq type '-)))
	;; Next line must not be a continuation line
	(save-excursion
	  (forward-line)
	  (not (eq (dcl-get-line-type) '-))))))


;;;-------------------------------------------------------------------------
(defun dcl-command-p ()
  "Check if point is on a command line.
Return t if point is on a command line or a continuation line,
otherwise return nil."
  (let ((type (dcl-get-line-type)))
    (or (eq type '$)
	(eq type '-))))


;;;-------------------------------------------------------------------------
(defun dcl-was-looking-at (regexp)
  (save-excursion
    (let ((start (point))
          (found (re-search-backward regexp 0 t)))
      (if (not found)
          ()
        (equal start (match-end 0))))))

(declare-function imenu-default-create-index-function "imenu" ())

;;;-------------------------------------------------------------------------
(defun dcl-imenu-create-index-function ()
  "Jacket routine to make imenu searches non case sensitive."
  (let ((case-fold-search t))
    (imenu-default-create-index-function)))



;;; *** Epilogue ************************************************************


(provide 'dcl-mode)

(run-hooks 'dcl-mode-load-hook)		; for your customizations

;;; dcl-mode.el ends here
