;;; speedbar --- quick access to files and tags in a frame

;; Copyright (C) 1996-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: file, tags, tools

(defvar speedbar-version "1.0"
  "The current version of speedbar.")
(defvar speedbar-incompatible-version "0.14beta4"
  "This version of speedbar is incompatible with this version.
Due to massive API changes (removing the use of the word PATH)
this version is not backward compatible to 0.14 or earlier.")

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
;;   The speedbar provides a frame in which files, and locations in
;; files are displayed.  These items can be clicked on with mouse-2 in
;; to display that file location.
;;
;;; Customizing and Developing for speedbar
;;
;; Please see the speedbar manual for information.
;;
;;; Notes:
;;
;;    Users of really old emacsen without the need timer functions
;; will not have speedbar updating automatically.  Use "g" to refresh
;; the display after changing directories.  Remember, do not interrupt
;; the stealthy updates or your display may not be completely
;; refreshed.
;;
;;    AUC-TEX users: The imenu tags for AUC-TEX mode don't work very
;; well.  Use the imenu keywords from tex-mode.el for better results.
;;
;; This file requires the library package assoc (association lists)
;;     assoc should be available in all modern versions of Emacs.
;; The custom package is optional (for easy configuration of speedbar)
;;     http://www.dina.kvl.dk/~abraham/custom/
;;     custom is available in all versions of Emacs version 20 or better.
;;
;;; Developing for speedbar
;;
;; Adding a speedbar specialized display mode:
;;
;; Speedbar can be configured to create a special display for certain
;; modes that do not display traditional file/tag data.  Rmail, Info,
;; and the debugger are examples.  These modes can, however, benefit
;; from a speedbar style display in their own way.
;;
;; If your `major-mode' is `foo-mode', the only requirement is to
;; create a function called `foo-speedbar-buttons' which takes one
;; argument, BUFFER.  BUFFER will be the buffer speedbar wants filled.
;; In `foo-speedbar-buttons' there are several functions that make
;; building a speedbar display easy.  See the documentation for
;; `speedbar-with-writable' (needed because the buffer is usually
;; read-only) `speedbar-make-tag-line', `speedbar-insert-button', and
;; `speedbar-insert-generic-list'.  If you use
;; `speedbar-insert-generic-list', also read the doc for
;; `speedbar-tag-hierarchy-method' in case you wish to override it.
;; The macro `speedbar-with-attached-buffer' brings you back to the
;; buffer speedbar is displaying for.
;;
;; For those functions that make buttons, the "function" should be a
;; symbol that is the function to call when clicked on.  The "token"
;; is extra data you can pass along.  The "function" must take three
;; parameters.  They are (TEXT TOKEN INDENT).  TEXT is the text of the
;; button clicked on.  TOKEN is the data passed in when you create the
;; button.  INDENT is an indentation level, or 0.  You can store
;; indentation levels with `speedbar-make-tag-line' which creates a
;; line with an expander (eg.  [+]) and a text button.
;;
;; Some useful functions when writing expand functions, and click
;; functions are `speedbar-change-expand-button-char',
;; `speedbar-delete-subblock', and `speedbar-center-buffer-smartly'.
;; The variable `speedbar-power-click' is set to t in your functions
;; when the user shift-clicks.  This is an indication of anything from
;; refreshing cached data to making a buffer appear in a new frame.
;;
;; If you wish to add to the default speedbar menu for the case of
;; `foo-mode', create a variable `foo-speedbar-menu-items'.  This
;; should be a list compatible with the `easymenu' package.  It will
;; be spliced into the main menu.  (Available with click-mouse-3).  If
;; you wish to have extra key bindings in your special mode, create a
;; variable `foo-speedbar-key-map'.  Instead of using `make-keymap',
;; or `make-sparse-keymap', use the function
;; `speedbar-make-specialized-keymap'.  This lets you inherit all of
;; speedbar's default bindings with low overhead.
;;
;; Adding a speedbar top-level display mode:
;;
;; Unlike the specialized modes, there are no name requirements,
;; however the methods for writing a button display, menu, and keymap
;; are the same.  Once you create these items, you can call the
;; function `speedbar-add-expansion-list'.  It takes one parameter
;; which is a list element of the form (NAME MENU KEYMAP &rest
;; BUTTON-FUNCTIONS).  NAME is a string that will show up in the
;; Displays menu item.  MENU is a symbol containing the menu items to
;; splice in.  KEYMAP is a symbol holding the keymap to use, and
;; BUTTON-FUNCTIONS are the function names to call, in order, to create
;; the display.
;;  Another tweakable variable is `speedbar-stealthy-function-list'
;; which is of the form (NAME &rest FUNCTION ...).  NAME is the string
;; name matching `speedbar-add-expansion-list'.  (It does not need to
;; exist.). This provides additional display info which might be
;; time-consuming to calculate.
;;  Lastly, `speedbar-mode-functions-list' allows you to set special
;; function overrides.

;;; TODO:
;; - Timeout directories we haven't visited in a while.

(require 'assoc)
(require 'easymenu)
(require 'dframe)
(require 'sb-image)

;; customization stuff
(defgroup speedbar nil
  "File and tag browser frame."
  :group 'etags
  :group 'tools
  :group 'convenience
;  :version "20.3"
  )

(defgroup speedbar-faces nil
  "Faces used in speedbar."
  :prefix "speedbar-"
  :group 'speedbar
  :group 'faces)

(defgroup speedbar-vc nil
  "Version control display in speedbar."
  :prefix "speedbar-"
  :group 'speedbar)

;;; Code:

;; Note: `inversion-test' requires parts of the CEDET package that are
;; not included with Emacs.
;;
;; (defun speedbar-require-version (major minor &optional beta)
;;   "Non-nil if this version of SPEEDBAR does not satisfy a specific version.
;; Arguments can be:
;;
;;   (MAJOR MINOR &optional BETA)
;;
;;   Values MAJOR and MINOR must be integers.  BETA can be an integer, or
;; excluded if a released version is required.
;;
;; It is assumed that if the current version is newer than that specified,
;; everything passes.  Exceptions occur when known incompatibilities are
;; introduced."
;;   (inversion-test 'speedbar
;; 		  (concat major "." minor
;; 			  (when beta (concat "beta" beta)))))

(defvar speedbar-initial-expansion-mode-alist
  '(("buffers" speedbar-buffer-easymenu-definition speedbar-buffers-key-map
     speedbar-buffer-buttons)
    ("quick buffers" speedbar-buffer-easymenu-definition speedbar-buffers-key-map
     speedbar-buffer-buttons-temp)
    ;; Files last, means first in the Displays menu
    ("files" speedbar-easymenu-definition-special speedbar-file-key-map
     speedbar-directory-buttons speedbar-default-directory-list)
    )
  "List of named expansion elements for filling the speedbar frame.
These expansion lists are only valid for regular files.  Special modes
still get to override this list on a mode-by-mode basis.  This list of
lists is of the form (NAME MENU KEYMAP FN1 FN2 ...).  NAME is a string
representing the types of things to be displayed.  MENU is an easymenu
structure used when in this mode.  KEYMAP is a local keymap to install
over the regular speedbar keymap.  FN1 ...  are functions that will be
called in order.  These functions will always get the default
directory to use passed in as the first parameter, and a 0 as the
second parameter.  The 0 indicates the uppermost indentation level.
They must assume that the cursor is at the position where they start
inserting buttons.")

(defvar speedbar-initial-expansion-list-name "files"
  "A symbol name representing the expansion list to use.
The expansion list `speedbar-initial-expansion-mode-alist' contains
the names and associated functions to use for buttons in speedbar.")

(defvar speedbar-previously-used-expansion-list-name "files"
  "Save the last expansion list method.
This is used for returning to a previous expansion list method when
the user is done with the current expansion list.")

(defvar speedbar-stealthy-function-list
  '(("files"
     speedbar-update-current-file
     speedbar-check-read-only
     speedbar-check-vc
     speedbar-check-objects)
    )
  "List of functions to periodically call stealthily.
This list is of the form:
 '( (\"NAME\" FUNCTION ...)
    ...)
where NAME is the name of the major display mode these functions are
for, and the remaining elements FUNCTION are functions to call in order.
Each function must return nil if interrupted, or t if completed.
Stealthy functions which have a single operation should always return t.
Functions which take a long time should maintain a state (where they
are in their speedbar related calculations) and permit interruption.
See `speedbar-check-vc' as a good example.")

(defvar speedbar-mode-functions-list
  '(("files" (speedbar-item-info . speedbar-files-item-info)
     (speedbar-line-directory . speedbar-files-line-directory))
    ("buffers" (speedbar-item-info . speedbar-buffers-item-info)
     (speedbar-line-directory . speedbar-buffers-line-directory))
    ("quick buffers" (speedbar-item-info . speedbar-buffers-item-info)
     (speedbar-line-directory . speedbar-buffers-line-directory))
    )
  "List of function tables to use for different major display modes.
It is not necessary to define any functions for a specialized mode.
This just provides a simple way of adding lots of customizations.
Each sublist is of the form:
  (\"NAME\" (FUNCTIONSYMBOL . REPLACEMENTFUNCTION) ...)
Where NAME is the name of the specialized mode.  The rest of the list
is a set of dotted pairs of the form FUNCTIONSYMBOL, which is the name
of a function you would like to replace, and REPLACEMENTFUNCTION,
which is a function you can call instead.  Not all functions can be
replaced this way.  Replaceable functions must provide that
functionality individually.")

(defcustom speedbar-mode-specific-contents-flag t
  "Non-nil means speedbar will show special mode contents.
This permits some modes to create customized contents for the speedbar
frame."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-query-confirmation-method 'all
  "Query control for file operations.
The 'always flag means to always query before file operations.
The 'none-but-delete flag means to not query before any file
operations, except before a file deletion."
  :group 'speedbar
  :type '(radio (const :tag "Always Query before some file operations."
		       all)
		(const :tag "Never Query before file operations, except for deletions."
		       none-but-delete)
;;;;		(const :tag "Never Every Query."
;;;;		       none)
		))

(defvar speedbar-special-mode-expansion-list nil
  "Default function list for creating specialized button lists.
This list is set by modes that wish to have special speedbar displays.
The list is of function names.  Each function is called with one
parameter BUFFER, the originating buffer.  The current buffer is the
speedbar buffer.")

(defvar speedbar-special-mode-key-map nil
  "Default keymap used when identifying a specialized display mode.
This keymap is local to each buffer that wants to define special keybindings
effective when its display is shown.")

(defcustom speedbar-before-visiting-file-hook '(push-mark)
  "Hooks run before speedbar visits a file in the selected frame.
The default buffer is the buffer in the selected window in the attached frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-visiting-file-hook nil
  "Hooks run when speedbar visits a file in the selected frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-before-visiting-tag-hook '(push-mark)
  "Hooks run before speedbar visits a tag in the selected frame.
The default buffer is the buffer in the selected window in the attached frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-visiting-tag-hook '(speedbar-highlight-one-tag-line)
  "Hooks run when speedbar visits a tag in the selected frame."
  :group 'speedbar
  :type 'hook
  :options '(speedbar-highlight-one-tag-line
	     speedbar-recenter-to-top
	     speedbar-recenter
	     ))

(defcustom speedbar-load-hook nil
  "Hooks run when speedbar is loaded."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-reconfigure-keymaps-hook nil
  "Hooks run when the keymaps are regenerated."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-show-unknown-files nil
  "Non-nil show files we can't expand with a ? in the expand button.
A nil value means don't show the file in the list."
  :group 'speedbar
  :type 'boolean)

;;; EVENTUALLY REMOVE THESE

;; When I moved to a repeating timer, I had the horrible misfortune
;; of losing the ability for adaptive speed choice.  This update
;; speed currently causes long delays when it should have been turned off.
(defvar speedbar-update-speed dframe-update-speed)
(make-obsolete-variable 'speedbar-update-speed
			'dframe-update-speed
			"speedbar 1.0pre3 (Emacs 23.1)")

(defvar speedbar-navigating-speed dframe-update-speed)
(make-obsolete-variable 'speedbar-navigating-speed
			'dframe-update-speed
			"speedbar 1.0pre3 (Emacs 23.1)")
;;; END REMOVE THESE

(defcustom speedbar-frame-parameters '((minibuffer . nil)
				       (width . 20)
				       (border-width . 0)
				       (menu-bar-lines . 0)
				       (tool-bar-lines . 0)
				       (unsplittable . t)
				       (left-fringe . 0)
				       )
  "Parameters to use when creating the speedbar frame in Emacs.
Any parameter supported by a frame may be added.  The parameter `height'
will be initialized to the height of the frame speedbar is
attached to and added to this list before the new frame is initialized."
  :group 'speedbar
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

;; These values by Hrvoje Niksic <hniksic@srce.hr>
(defcustom speedbar-frame-plist
  '(minibuffer nil width 20 border-width 0
	       internal-border-width 0 unsplittable t
	       default-toolbar-visible-p nil has-modeline-p nil
	       menubar-visible-p nil
	       default-gutter-visible-p nil
	       )
  "Parameters to use when creating the speedbar frame in XEmacs.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame speedbar
is attached to."
  :group 'speedbar
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value"))))

(defcustom speedbar-use-imenu-flag (fboundp 'imenu)
  "Non-nil means use imenu for file parsing, nil to use etags.
XEmacs prior to 20.4 doesn't support imenu, therefore the default is to
use etags instead.  Etags support is not as robust as imenu support."
  :tag "Use Imenu for tags"
  :group 'speedbar
  :type 'boolean)

(defvar speedbar-dynamic-tags-function-list
  '((speedbar-fetch-dynamic-imenu . speedbar-insert-imenu-list)
    (speedbar-fetch-dynamic-etags . speedbar-insert-etags-list))
  "Set to a list of functions which will return and insert a list of tags.
Each element is of the form ( FETCH . INSERT ) where FETCH
is a function which takes one parameter (the file to tag) and returns a
list of tags.  The tag list can be of any form as long as the
corresponding insert method can handle it.  If it returns t, then an
error occurred, and the next fetch routine is tried.
INSERT is a function which takes an INDENTation level, and a LIST of
tags to insert.  It will then create the speedbar buttons.")

(defcustom speedbar-use-tool-tips-flag (fboundp 'tooltip-mode)
  "Non-nil means to use tool tips if they are available.
When tooltips are not available, mouse-tracking and minibuffer
display is used instead."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-track-mouse-flag (not speedbar-use-tool-tips-flag)
  "Non-nil means to display info about the line under the mouse."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-default-position 'left-right
  "Default position of the speedbar frame.
Possible values are 'left, 'right or 'left-right.
If value is 'left-right, the most suitable location is
determined automatically."
  :group 'speedbar
  :type '(radio (const :tag "Automatic" left-right)
		(const :tag "Left" left)
		(const :tag "Right" right)))

(defcustom speedbar-sort-tags nil
  "If non-nil, sort tags in the speedbar display.  *Obsolete*.
Use `semantic-tag-hierarchy-method' instead."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-tag-hierarchy-method
  '(speedbar-prefix-group-tag-hierarchy
    speedbar-trim-words-tag-hierarchy)
  "List of hooks which speedbar will use to organize tags into groups.
Groups are defined as expandable meta-tags.  Imenu supports
such things in some languages, such as separating variables from
functions.  Each hook takes one argument LST, and may destructively
create a new list of the same form.  LST is a list of elements of the
form:
  (ELT1 ELT2 ... ELTn)
where each ELT is of the form
  (TAG-NAME-STRING . NUMBER-OR-MARKER)
or
  (GROUP-NAME-STRING ELT1 ELT2... ELTn)"
  :group 'speedbar
  :type 'hook
  :options '(speedbar-prefix-group-tag-hierarchy
	     speedbar-trim-words-tag-hierarchy
	     speedbar-simple-group-tag-hierarchy
	     speedbar-sort-tag-hierarchy)
  )

(defcustom speedbar-tag-group-name-minimum-length 4
  "The minimum length of a prefix group name before expanding.
Thus, if the `speedbar-tag-hierarchy-method' includes `prefix-group'
and one such groups common characters is less than this number of
characters, then the group name will be changed to the form of:
  worda to wordb
instead of just
  word
This way we won't get silly looking listings."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-tag-split-minimum-length 20
  "Minimum length before we stop trying to create sub-lists in tags.
This is used by all tag-hierarchy methods that break large lists into
sub-lists."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-tag-regroup-maximum-length 10
  "Maximum length of submenus that are regrouped.
If the regrouping option is used, then if two or more short subgroups
are next to each other, then they are combined until this number of
items is reached."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-directory-button-trim-method 'span
  "Indicates how the directory button will be displayed.
Possible values are:
 'span - span large directories over multiple lines.
 'trim - trim large directories to only show the last few.
 nil   - no trimming."
  :group 'speedbar
  :type '(radio (const :tag "Span large directories over multiple lines."
		       span)
		(const :tag "Trim large directories to only show the last few."
		       trim)
		(const :tag "No trimming." nil)))

(defcustom speedbar-smart-directory-expand-flag t
  "Non-nil means speedbar should use smart expansion.
Smart expansion only affects when speedbar wants to display a
directory for a file in the attached frame.  When smart expansion is
enabled, new directories which are children of a displayed directory
are expanded in the current framework.  If nil, then the current
hierarchy would be replaced with the new directory."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-indentation-width 1
  "When sub-nodes are expanded, the number of spaces used for indentation."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-hide-button-brackets-flag nil
  "Non-nil means speedbar will hide the brackets around the + or -."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-before-popup-hook nil
  "Hooks called before popping up the speedbar frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-after-create-hook '(speedbar-frame-reposition-smartly)
  "Hooks called after popping up the speedbar frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-before-delete-hook nil
  "Hooks called before deleting the speedbar frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-mode-hook nil
  "Hook run after creating a speedbar buffer."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-timer-hook nil
  "Hooks called after running the speedbar timer function."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-verbosity-level 1
  "Verbosity level of the speedbar.
0 means say nothing.
1 means medium level verbosity.
2 and higher are higher levels of verbosity."
  :group 'speedbar
  :type 'integer)

(defvar speedbar-indicator-separator " "
  "String separating file text from indicator characters.")

(defcustom speedbar-vc-do-check t
  "Non-nil check all files in speedbar to see if they have been checked out.
Any file checked out is marked with `speedbar-vc-indicator'."
  :group 'speedbar-vc
  :type 'boolean)

(defvar speedbar-vc-indicator "*"
  "Text used to mark files which are currently checked out.
Other version control systems can be added by examining the function
`speedbar-vc-directory-enable-hook' and `speedbar-vc-in-control-hook'.")

(defcustom speedbar-vc-directory-enable-hook nil
  "Return non-nil if the current directory should be checked for Version Control.
Functions in this hook must accept one parameter which is the directory
being checked."
  :group 'speedbar-vc
  :type 'hook)

(defcustom speedbar-vc-in-control-hook nil
  "Return non-nil if the specified file is under Version Control.
Functions in this hook must accept two parameters.  The DIRECTORY of the
current file, and the FILENAME of the file being checked."
  :group 'speedbar-vc
  :type 'hook)

(defvar speedbar-vc-to-do-point nil
  "Local variable maintaining the current version control check position.")

(defcustom speedbar-obj-do-check t
  "Non-nil check all files in speedbar to see if they have an object file.
Any file checked out is marked with `speedbar-obj-indicator', and the
marking is based on `speedbar-obj-alist'"
  :group 'speedbar-vc
  :type 'boolean)

(defvar speedbar-obj-to-do-point nil
  "Local variable maintaining the current version control check position.")

(defvar speedbar-obj-indicator '("#" . "!")
  "Text used to mark files that have a corresponding hidden object file.
The car is for an up-to-date object.  The cdr is for an out of date object.
The expression `speedbar-obj-alist' defines who gets tagged.")

(defvar speedbar-obj-alist
  '(("\\.\\([cpC]\\|cpp\\|cc\\|cxx\\)$" . ".o")
    ("\\.el$" . ".elc")
    ("\\.java$" . ".class")
    ("\\.f\\(or\\|90\\|77\\)?$" . ".o")
    ("\\.tex$" . ".dvi")
    ("\\.texi$" . ".info"))
  "Alist of file extensions, and their corresponding object file type.")

(defvar speedbar-ro-to-do-point nil
  "Local variable maintaining the current read only check position.")

(defvar speedbar-object-read-only-indicator "%"
  "Indicator to append onto a line if that item is Read Only.")

;; Note: Look for addition place to add indicator lists that
;; use skip-chars instead of a regular expression.
(defvar speedbar-indicator-regex
  (concat (regexp-quote speedbar-indicator-separator)
	  "\\("
	  (regexp-quote speedbar-vc-indicator)
	  "\\|"
	  (regexp-quote (car speedbar-obj-indicator))
	  "\\|"
	  (regexp-quote (cdr speedbar-obj-indicator))
	  "\\|"
	  (regexp-quote speedbar-object-read-only-indicator)
	  "\\)*")
  "Regular expression used when identifying files.
Permits stripping of indicator characters from a line.")

(defcustom speedbar-scanner-reset-hook nil
  "Hook called whenever generic scanners are reset.
Set this to implement your own scanning / rescan safe functions with
state data."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-ignored-modes '(fundamental-mode)
  "List of major modes which speedbar will not switch directories for."
  :group 'speedbar
  :type '(choice (const nil)
		 (repeat :tag "List of modes" (symbol :tag "Major mode"))))

(defun speedbar-extension-list-to-regex (extlist)
  "Takes EXTLIST, a list of extensions and transforms it into regexp.
All the preceding `.' are stripped for an optimized expression starting
with `.' followed by extensions, followed by full-filenames."
  (let ((regex1 nil) (regex2 nil))
    (while extlist
      (if (= (string-to-char (car extlist)) ?.)
	  (setq regex1 (concat regex1 (if regex1 "\\|" "")
			       (substring (car extlist) 1)))
	(setq regex2 (concat regex2 (if regex2 "\\|" "") (car extlist))))
      (setq extlist (cdr extlist)))
    ;; Concatenate all the subexpressions together, making sure all types
    ;; of parts exist during concatenation.
    (concat "\\("
	    (if regex1 (concat "\\(\\.\\(" regex1 "\\)\\)") "")
	    (if (and regex1 regex2) "\\|" "")
	    (if regex2 (concat "\\(" regex2 "\\)") "")
	    "\\)$")))

(defvar speedbar-ignored-directory-regexp nil
  "Regular expression matching directories speedbar will not switch to.
Created from `speedbar-ignored-directory-expressions' with the function
`speedbar-extension-list-to-regex' (a misnamed function in this case.)
Use the function `speedbar-add-ignored-directory-regexp', or customize the
variable `speedbar-ignored-directory-expressions' to modify this variable.")

(define-obsolete-variable-alias 'speedbar-ignored-path-expressions
  'speedbar-ignored-directory-expressions "22.1")

(defcustom speedbar-ignored-directory-expressions
  '("[/\\]logs?[/\\]\\'")
  "List of regular expressions matching directories speedbar will ignore.
They should included directories which are notoriously very large
and take a long time to load in.  Use the function
`speedbar-add-ignored-directory-regexp' to add new items to this list after
speedbar is loaded.  You may place anything you like in this list
before speedbar has been loaded."
  :group 'speedbar
  :type '(repeat (regexp :tag "Directory Regexp"))
  :set (lambda (_sym val)
	 (setq speedbar-ignored-directory-expressions val
	       speedbar-ignored-directory-regexp
	       (speedbar-extension-list-to-regex val))))

(defcustom speedbar-directory-unshown-regexp "^\\(\\..*\\)\\'"
  "Regular expression matching directories not to show in speedbar.
They should include commonly existing directories which are not
useful.  It is no longer necessary to include version-control
directories here; see `vc-directory-exclusion-list'."
  :group 'speedbar
  :type 'string)

(defcustom speedbar-file-unshown-regexp
  (let ((nstr "") (noext completion-ignored-extensions))
    (while noext
      (setq nstr (concat nstr (regexp-quote (car noext)) "\\'"
			 (if (cdr noext) "\\|" ""))
	    noext (cdr noext)))
    ;;               backup      refdir      lockfile
    (concat nstr "\\|#[^#]+#$\\|\\.\\.?\\'\\|\\.#"))
  "Regexp matching files we don't want displayed in a speedbar buffer.
It is generated from the variable `completion-ignored-extensions'."
  :group 'speedbar
  :type 'string)

(defvar speedbar-file-regexp nil
  "Regular expression matching files we know how to expand.
Created from `speedbar-supported-extension-expressions' with the
function `speedbar-extension-list-to-regex'.")

;; this is dangerous to customize, because the defaults will probably
;; change in the future.
(defcustom speedbar-supported-extension-expressions
  (append '(".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?"
	    ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?")
	  (if speedbar-use-imenu-flag
	      '(".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g"
		;; html is not supported by default, but an imenu tags package
		;; is available.  Also, html files are nice to be able to see.
		".s?html"
		".ma?k" "[Mm]akefile\\(\\.in\\)?")))
  "List of regular expressions which will match files supported by tagging.
Do not prefix the `.' char with a double \\ to quote it, as the period
will be stripped by a simplified optimizer when compiled into a
singular expression.  This variable will be turned into
`speedbar-file-regexp' for use with speedbar.  You should use the
function `speedbar-add-supported-extension' to add a new extension at
runtime, or use the configuration dialog to set it in your .emacs file.
If you add an extension to this list, and it does not appear, you may
need to also modify `completion-ignored-extension' which will also help
file completion."
  :group 'speedbar
  :type '(repeat (regexp :tag "Extension Regexp"))
  :set (lambda (_sym val)
	 (set 'speedbar-supported-extension-expressions val)
	 (set 'speedbar-file-regexp (speedbar-extension-list-to-regex val))))

(setq speedbar-file-regexp
      (speedbar-extension-list-to-regex speedbar-supported-extension-expressions))

(defun speedbar-add-supported-extension (extension)
  "Add EXTENSION as a new supported extension for speedbar tagging.
This should start with a `.' if it is not a complete file name, and
the dot should NOT be quoted in with \\.  Other regular expression
matchers are allowed however.  EXTENSION may be a single string or a
list of strings."
  (interactive "sExtension: ")
  (if (not (listp extension)) (setq extension (list extension)))
  (while extension
    (if (member (car extension) speedbar-supported-extension-expressions)
	nil
      (setq speedbar-supported-extension-expressions
	    (cons (car extension) speedbar-supported-extension-expressions)))
    (setq extension (cdr extension)))
  (setq speedbar-file-regexp (speedbar-extension-list-to-regex
			      speedbar-supported-extension-expressions)))

(defun speedbar-add-ignored-directory-regexp (directory-expression)
  "Add DIRECTORY-EXPRESSION as a new ignored directory for speedbar tracking.
This function will modify `speedbar-ignored-directory-regexp' and add
DIRECTORY-EXPRESSION to `speedbar-ignored-directory-expressions'."
  (interactive "sDirectory regex: ")
  (if (not (listp directory-expression))
      (setq directory-expression (list directory-expression)))
  (while directory-expression
    (if (member (car directory-expression) speedbar-ignored-directory-expressions)
	nil
      (setq speedbar-ignored-directory-expressions
	    (cons (car directory-expression) speedbar-ignored-directory-expressions)))
    (setq directory-expression (cdr directory-expression)))
  (setq speedbar-ignored-directory-regexp (speedbar-extension-list-to-regex
				      speedbar-ignored-directory-expressions)))

;; If we don't have custom, then we set it here by hand.
(if (not (fboundp 'custom-declare-variable))
    (setq speedbar-file-regexp (speedbar-extension-list-to-regex
				speedbar-supported-extension-expressions)
	  speedbar-ignored-directory-regexp (speedbar-extension-list-to-regex
					speedbar-ignored-directory-expressions)))

(defcustom speedbar-update-flag dframe-have-timer-flag
  "Non-nil means to automatically update the display.
When this is nil then speedbar will not follow the attached frame's directory.
If you want to change this while speedbar is active, either use
\\[customize] or call \\<speedbar-key-map> `\\[speedbar-toggle-updates]'."
  :group 'speedbar
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
	 (set sym val)
	 (speedbar-toggle-updates))
  :type 'boolean)

(defvar speedbar-update-flag-disable nil
  "Permanently disable changing of the update flag.")

(defvar speedbar-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Turn off paren matching around here.
    (modify-syntax-entry ?\' " " st)
    (modify-syntax-entry ?\" " " st)
    (modify-syntax-entry ?\( " " st)
    (modify-syntax-entry ?\) " " st)
    (modify-syntax-entry ?\{ " " st)
    (modify-syntax-entry ?\} " " st)
    (modify-syntax-entry ?\[ " " st)
    (modify-syntax-entry ?\]  " " st)
    st)
  "Syntax-table used on the speedbar.")
(define-obsolete-variable-alias
  'speedbar-syntax-table 'speedbar-mode-syntax-table "24.1")


(defvar speedbar-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)

    ;; Control.
    (define-key map "t" 'speedbar-toggle-updates)
    (define-key map "g" 'speedbar-refresh)

    ;; Navigation.
    (define-key map "n" 'speedbar-next)
    (define-key map "p" 'speedbar-prev)
    (define-key map "\M-n" 'speedbar-restricted-next)
    (define-key map "\M-p" 'speedbar-restricted-prev)
    (define-key map "\C-\M-n" 'speedbar-forward-list)
    (define-key map "\C-\M-p" 'speedbar-backward-list)
    ;; These commands never seemed useful.
    ;;  (define-key map " " 'speedbar-scroll-up)
    ;;  (define-key map [delete] 'speedbar-scroll-down)

    ;; Short cuts I happen to find useful.
    (define-key map "r"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list
         speedbar-previously-used-expansion-list-name)))
    (define-key map "b"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "quick buffers")))
    (define-key map "f"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "files")))

    (dframe-update-keymap map)
    map)
  "Keymap used in speedbar buffer.")
(define-obsolete-variable-alias 'speedbar-key-map 'speedbar-mode-map "24.1")

(defun speedbar-make-specialized-keymap ()
  "Create a keymap for use with a speedbar major or minor display mode.
This basically creates a sparse keymap, and makes its parent be
`speedbar-mode-map'."
  (let ((k (make-sparse-keymap)))
    (set-keymap-parent k speedbar-mode-map)
    k))

(defvar speedbar-file-key-map
  (let ((map (speedbar-make-specialized-keymap)))

    ;; Basic tree features.
    (define-key map "e" 'speedbar-edit-line)
    (define-key map "\C-m" 'speedbar-edit-line)
    (define-key map "+" 'speedbar-expand-line)
    (define-key map "=" 'speedbar-expand-line)
    (define-key map "-" 'speedbar-contract-line)

    (define-key map "[" 'speedbar-expand-line-descendants)
    (define-key map "]" 'speedbar-contract-line-descendants)

    (define-key map " " 'speedbar-toggle-line-expansion)

    ;; File based commands.
    (define-key map "U" 'speedbar-up-directory)
    (define-key map "I" 'speedbar-item-info)
    (define-key map "B" 'speedbar-item-byte-compile)
    (define-key map "L" 'speedbar-item-load)
    (define-key map "C" 'speedbar-item-copy)
    (define-key map "D" 'speedbar-item-delete)
    (define-key map "O" 'speedbar-item-object-delete)
    (define-key map "R" 'speedbar-item-rename)
    (define-key map "M" 'speedbar-create-directory)
    map)
  "Keymap used in speedbar buffer while files are displayed.")

(defvar speedbar-easymenu-definition-base
  (append
   '("Speedbar"
     ["Update" speedbar-refresh t]
     ["Auto Update" speedbar-toggle-updates
      :active (not speedbar-update-flag-disable)
      :style toggle :selected speedbar-update-flag])
   (if (and (or (fboundp 'defimage)
		(fboundp 'make-image-specifier))
	    (if (fboundp 'display-graphic-p)
		(display-graphic-p)
	      window-system))
       (list
	["Use Images" speedbar-toggle-images
	 :style toggle :selected speedbar-use-images]))
   )
  "Base part of the speedbar menu.")

(defvar speedbar-easymenu-definition-special
  '(["Edit Item On Line" speedbar-edit-line t]
    ["Show All Files" speedbar-toggle-show-all-files
     :style toggle :selected speedbar-show-unknown-files]
    ["Expand File Tags" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Flush Cache & Expand" speedbar-flush-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Expand All Descendants" speedbar-expand-line-descendants
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. ")) ]
    ["Contract File Tags" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
;    ["Sort Tags" speedbar-toggle-sorting
;     :style toggle :selected speedbar-sort-tags]
    "----"
    ["File/Tag Information" speedbar-item-info t]
    ["Load Lisp File" speedbar-item-load
     (save-excursion
       (beginning-of-line)
       (looking-at "[0-9]+: *\\[[+-]\\] .+\\(\\.el\\)\\( \\|$\\)"))]
    ["Byte Compile File" speedbar-item-byte-compile
     (save-excursion
       (beginning-of-line)
       (looking-at "[0-9]+: *\\[[+-]\\] .+\\(\\.el\\)\\( \\|$\\)"))]
    ["Copy File" speedbar-item-copy
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *\\["))]
    ["Rename File" speedbar-item-rename
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]"))]
    ["Create Directory" speedbar-create-directory
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]"))]
    ["Delete File" speedbar-item-delete
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]"))]
    ["Delete Object" speedbar-item-object-delete
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *\\[[+-]\\] [^ \n]+ \\*?[!#]$"))]
    )
  "Additional menu items while in file-mode.")

(defvar speedbar-easymenu-definition-trailer
  (append
   (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
       (list ["Customize..." speedbar-customize t]))
   (list
    ["Close" dframe-close-frame t]
    ["Quit" delete-frame t] ))
  "Menu items appearing at the end of the speedbar menu.")

(defvar speedbar-desired-buffer nil
  "Non-nil when speedbar is showing buttons specific to a special mode.
In this case it is the originating buffer.")
(defvar speedbar-buffer nil
  "The buffer displaying the speedbar.")
(defvar speedbar-frame nil
  "The frame displaying speedbar.")
(defvar speedbar-cached-frame nil
  "The frame that was last created, then removed from the display.")
(defvar speedbar-full-text-cache nil
  "The last open directory is saved in its entirety for ultra-fast switching.")

(defvar speedbar-last-selected-file nil
  "The last file which was selected in speedbar buffer.")

(defvar speedbar-shown-directories nil
  "Maintain list of directories simultaneously open in the current speedbar.")

(defvar speedbar-directory-contents-alist nil
  "An association list of directories and their contents.
Each sublist was returned by `speedbar-file-lists'.  This list is
maintained to speed up the refresh rate when switching between
directories.")

(defvar speedbar-power-click nil
  "Never set this by hand.  Value is t when S-mouse activity occurs.")


;;; Compatibility
;;
(defalias 'speedbar-make-overlay
  (if (featurep 'xemacs) 'make-extent 'make-overlay))

(defalias 'speedbar-overlay-put
  (if (featurep 'xemacs) 'set-extent-property 'overlay-put))

(defalias 'speedbar-delete-overlay
  (if (featurep 'xemacs) 'delete-extent 'delete-overlay))

(defalias 'speedbar-mode-line-update
  (if (featurep 'xemacs) 'redraw-modeline 'force-mode-line-update))

;;; Mode definitions/ user commands
;;

;;;###autoload
(defalias 'speedbar 'speedbar-frame-mode)
;;;###autoload
(defun speedbar-frame-mode (&optional arg)
  "Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
A nil ARG means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted."
  (interactive "P")
  ;; Get the buffer to play with
  (if (not (buffer-live-p speedbar-buffer))
      (save-excursion
	(setq speedbar-buffer (get-buffer-create " SPEEDBAR"))
	(set-buffer speedbar-buffer)
	(speedbar-mode)))
  ;; Do the frame thing
  (dframe-frame-mode arg
		     'speedbar-frame
		     'speedbar-cached-frame
		     'speedbar-buffer
		     "Speedbar"
		     #'speedbar-frame-mode
		     (if (featurep 'xemacs)
			 (append speedbar-frame-plist
				 ;; This is a hack to get speedbar to iconify
				 ;; with the selected frame.
				 (list 'parent (selected-frame)))
		       speedbar-frame-parameters)
		     speedbar-before-delete-hook
		     speedbar-before-popup-hook
		     speedbar-after-create-hook)
  ;; Start up the timer
  (if (not speedbar-frame)
      (speedbar-set-timer nil)
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)
    (speedbar-set-timer dframe-update-speed)
    )
  ;; Frame modifications
  (set (make-local-variable 'dframe-delete-frame-function)
       'speedbar-handle-delete-frame)
  ;; hscroll
  (set (make-local-variable 'automatic-hscrolling) nil) ; Emacs 21
  ;; reset the selection variable
  (setq speedbar-last-selected-file nil))

(defun speedbar-frame-reposition-smartly ()
  "Reposition the speedbar frame to be next to the attached frame."
  (cond ((and (featurep 'xemacs)
	      (or (member 'left speedbar-frame-plist)
		  (member 'top speedbar-frame-plist)))
	 (dframe-reposition-frame
	  speedbar-frame
	  (dframe-attached-frame speedbar-frame)
	  (cons (car (cdr (member 'left speedbar-frame-plist)))
		(car (cdr (member 'top speedbar-frame-plist)))))
	 )
	((and (not (featurep 'xemacs))
	      (or (assoc 'left speedbar-frame-parameters)
		  (assoc 'top speedbar-frame-parameters)))
	 ;; if left/top were specified in the parameters, pass them
	 ;; down to the reposition function
	 (dframe-reposition-frame
	  speedbar-frame
	  (dframe-attached-frame speedbar-frame)
	  (cons (cdr (assoc 'left speedbar-frame-parameters))
		(cdr (assoc 'top  speedbar-frame-parameters))))
	 )
	(t
	 (dframe-reposition-frame speedbar-frame
				  (dframe-attached-frame speedbar-frame)
				  speedbar-default-position))))

(defsubst speedbar-current-frame ()
  "Return the frame to use for speedbar based on current context."
  (dframe-current-frame 'speedbar-frame 'speedbar-mode))

(defun speedbar-handle-delete-frame (e)
  "Handle a delete frame event E.
If the deleted frame is the frame speedbar is attached to,
we need to delete speedbar also."
  (when (and speedbar-frame
	     (eq (car (car (cdr e))) ;; frame to be deleted
		 dframe-attached-frame))
    (delete-frame speedbar-frame)))

;;;###autoload
(defun speedbar-get-focus ()
  "Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame."
  (interactive)
  (speedbar-reset-scanners)
  (dframe-get-focus 'speedbar-frame 'speedbar-frame-mode
		    (lambda () (let ((speedbar-update-flag t))
				 (speedbar-timer-fn)))))

(defsubst speedbar-frame-width ()
  "Return the width of the speedbar frame in characters.
Return nil if it doesn't exist."
  (frame-width speedbar-frame))

(define-derived-mode speedbar-mode fundamental-mode "Speedbar"
  "Major mode for managing a display of directories and tags.
\\<speedbar-key-map>
The first line represents the default directory of the speedbar frame.
Each directory segment is a button which jumps speedbar's default
directory to that directory.  Buttons are activated by clicking `\\[speedbar-click]'.
In some situations using `\\[dframe-power-click]' is a `power click' which will
rescan cached items, or pop up new frames.

Each line starting with <+> represents a directory.  Click on the <+>
to insert the directory listing into the current tree.  Click on the
<-> to retract that list.  Click on the directory name to go to that
directory as the default.

Each line starting with [+] is a file.  If the variable
`speedbar-show-unknown-files' is t, the lines starting with [?] are
files which don't have imenu support, but are not expressly ignored.
Files are completely ignored if they match `speedbar-file-unshown-regexp'
which is generated from `completion-ignored-extensions'.

Files with a `*' character after their name are files checked out of a
version control system.  (Currently only RCS is supported.)  New
version control systems can be added by examining the documentation
for `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p'.

Files with a `#' or `!' character after them are source files that
have an object file associated with them.  The `!' indicates that the
files is out of date.  You can control what source/object associations
exist through the variable `speedbar-obj-alist'.

Click on the [+] to display a list of tags from that file.  Click on
the [-] to retract the list.  Click on the file name to edit the file
in the attached frame.

If you open tags, you might find a node starting with {+}, which is a
category of tags.  Click the {+} to expand the category.  Jump-able
tags start with >.  Click the name of the tag to go to that position
in the selected file.

\\{speedbar-key-map}"
  (save-excursion
    (setq font-lock-keywords nil) ;; no font-locking please
    (setq truncate-lines t)
    (make-local-variable 'frame-title-format)
    (setq frame-title-format (concat "Speedbar " speedbar-version)
	  case-fold-search nil
	  buffer-read-only t)
    (speedbar-set-mode-line-format)
    ;; Add in our dframe hooks.
    (if speedbar-track-mouse-flag
	(setq dframe-track-mouse-function #'speedbar-track-mouse))
    (setq dframe-help-echo-function #'speedbar-item-info
	  dframe-mouse-click-function #'speedbar-click
	  dframe-mouse-position-function #'speedbar-position-cursor-on-line))
  speedbar-buffer)

(defmacro speedbar-message (fmt &rest args)
  "Like `message', but for use in the speedbar frame.
Argument FMT is the format string, and ARGS are the arguments for message."
  `(dframe-message ,fmt ,@args))

(defsubst speedbar-y-or-n-p (prompt &optional deleting)
  "Like `y-or-n-p', but for use in the speedbar frame.
Argument PROMPT is the prompt to use.
Optional argument DELETING means this is a query that will delete something.
The variable `speedbar-query-confirmation-method' can cause this to
return true without a query."
  (or (and (not deleting)
	   (eq speedbar-query-confirmation-method 'none-but-delete))
      (dframe-y-or-n-p prompt)))

(defsubst speedbar-select-attached-frame ()
  "Select the frame attached to this speedbar."
  (dframe-select-attached-frame (speedbar-current-frame)))

;; Backwards compatibility
(defalias 'speedbar-with-attached-buffer 'dframe-with-attached-buffer)
(defalias 'speedbar-maybee-jump-to-attached-frame 'dframe-maybee-jump-to-attached-frame)

(defun speedbar-set-mode-line-format ()
  "Set the format of the mode line based on the current speedbar environment.
This gives visual indications of what is up.  It EXPECTS the speedbar
frame and window to be the currently active frame and window."
  (if (and (frame-live-p (speedbar-current-frame))
	   (or (not (featurep 'xemacs))
	       (with-no-warnings
		 (specifier-instance has-modeline-p)))
	   speedbar-buffer)
      (with-current-buffer speedbar-buffer
	(let* ((w (or (speedbar-frame-width) 20))
	       (p1 "<<")
	       (p5 ">>")
	       (p3 (if speedbar-update-flag "#" "!"))
	       (p35 (capitalize speedbar-initial-expansion-list-name))
	       (blank (- w (length p1) (length p3) (length p5) (length p35)
			 (if line-number-mode 5 1)))
	       (p2 (if (> blank 0)
		       (make-string (/ blank 2) ? )
		     ""))
	       (p4 (if (> blank 0)
		       (make-string (+ (/ blank 2) (% blank 2)) ? )
		     ""))
	       (tf
		(if line-number-mode
		    (list (concat p1 p2 p3 " " p35) '(line-number-mode " %3l")
			  (concat p4 p5))
		  (list (concat p1 p2 p3 p4 p5)))))
	  (if (not (equal mode-line-format tf))
	      (progn
		(setq mode-line-format tf)
		(speedbar-mode-line-update)))))))

(defvar speedbar-previous-menu nil
  "The menu before the last `speedbar-reconfigure-keymaps' was called.")

(defun speedbar-reconfigure-keymaps ()
  "Reconfigure the menu-bar in a speedbar frame.
Different menu items are displayed depending on the current display mode
and the existence of packages."
  (let ((md (append
	     speedbar-easymenu-definition-base
	     (if speedbar-shown-directories
		 ;; file display mode version
		 (speedbar-initial-menu)
	       (save-excursion
		 (dframe-select-attached-frame speedbar-frame)
		  (eval (nth 1 (assoc speedbar-initial-expansion-list-name
				speedbar-initial-expansion-mode-alist)))))
	     ;; Dynamic menu stuff
	     '("-")
	    (list (cons "Displays"
			(let ((displays nil)
			      (alist speedbar-initial-expansion-mode-alist))
			  (while alist
			    (setq displays
				  (cons
				   (vector
				    (capitalize (car (car alist)))
				    (list
				     'speedbar-change-initial-expansion-list
				     (car (car alist)))
				    :style 'radio
				    :selected
				    `(string= ,(car (car alist))
					 speedbar-initial-expansion-list-name)
				    )
				   displays))
			    (setq alist (cdr alist)))
			  displays)))
	    ;; The trailer
	    speedbar-easymenu-definition-trailer))
	(localmap (save-excursion
		    (let ((cf (selected-frame)))
		      (prog2
			  (dframe-select-attached-frame speedbar-frame)
			  (if (local-variable-p
			       'speedbar-special-mode-key-map
			       (current-buffer))
			      speedbar-special-mode-key-map)
			(select-frame cf))))))
    (with-current-buffer speedbar-buffer
      (use-local-map (or localmap
			 (speedbar-initial-keymap)
			 ;; This creates a small keymap we can glom the
			 ;; menu adjustments into.
			 (speedbar-make-specialized-keymap)))
      ;; Delete the old menu if applicable.
      (if speedbar-previous-menu (easy-menu-remove speedbar-previous-menu))
      (setq speedbar-previous-menu md)
      ;; Now add the new menu
      (if (not (featurep 'xemacs))
	  (easy-menu-define speedbar-menu-map (current-local-map)
			    "Speedbar menu" md)
	(easy-menu-add md (current-local-map))
	;; XEmacs-specific:
	(if (fboundp 'set-buffer-menubar)
	    (set-buffer-menubar (list md)))))

    (run-hooks 'speedbar-reconfigure-keymaps-hook)))


;;; User Input stuff
;;
(defun speedbar-customize ()
  "Customize speedbar using the Custom package."
  (interactive)
  (let ((sf (selected-frame)))
    (dframe-select-attached-frame speedbar-frame)
    (customize-group 'speedbar)
    (select-frame sf))
  (dframe-maybee-jump-to-attached-frame))

(defun speedbar-track-mouse (event)
  "For motion EVENT, display info about the current line."
  (if (not speedbar-track-mouse-flag)
      nil
    (save-excursion
      (save-window-excursion
	(condition-case nil
	    (progn
	      (mouse-set-point event)
	      (if (eq major-mode 'speedbar-mode)
		  ;; XEmacs may let us get in here in other mode buffers.
		  (speedbar-item-info)))
	  (error (speedbar-message nil)))))))

(defun speedbar-show-info-under-mouse ()
  "Call the info function for the line under the mouse."
  (let ((pos (mouse-position)))	; we ignore event until I use it later.
    (if (equal (car pos) speedbar-frame)
	(save-excursion
	  (save-window-excursion
	    (apply 'set-mouse-position pos)
	    (speedbar-item-info))))))

(defun speedbar-next (arg)
  "Move to the next ARGth line in a speedbar buffer."
  (interactive "p")
  (forward-line (or arg 1))
  (speedbar-item-info)
  (speedbar-position-cursor-on-line))

(defun speedbar-prev (arg)
  "Move to the previous ARGth line in a speedbar buffer."
  (interactive "p")
  (speedbar-next (if arg (- arg) -1)))

(defun speedbar-restricted-move (arg)
  "Move to the next ARGth line in a speedbar buffer at the same depth.
This means that movement is restricted to a subnode, and that siblings
of intermediate nodes are skipped."
  (if (not (numberp arg)) (signal 'wrong-type-argument (list 'numberp arg)))
  ;; First find the extent for which we are allowed to move.
  (let ((depth (save-excursion (beginning-of-line)
			       (if (looking-at "[0-9]+:")
				   (string-to-number (match-string 0))
				 0)))
	(crement (if (< arg 0) 1 -1))	; decrement or increment
	(lastmatch (point)))
    (while (/= arg 0)
      (forward-line (- crement))
      (let ((subdepth (save-excursion (beginning-of-line)
				      (if (looking-at "[0-9]+:")
					  (string-to-number (match-string 0))
					0))))
	(cond ((or (< subdepth depth)
		   (progn (end-of-line) (eobp))
		   (progn (beginning-of-line) (bobp)))
	       ;; We have reached the end of this block.
	       (goto-char lastmatch)
	       (setq arg 0)
	       (error "End of sub-list"))
	      ((= subdepth depth)
	       (setq lastmatch (point)
		     arg (+ arg crement))))))
    (speedbar-position-cursor-on-line)))

(defun speedbar-restricted-next (arg)
  "Move to the next ARGth line in a speedbar buffer at the same depth.
This means that movement is restricted to a subnode, and that siblings
of intermediate nodes are skipped."
  (interactive "p")
  (speedbar-restricted-move (or arg 1))
  (speedbar-item-info))

(defun speedbar-restricted-prev (arg)
  "Move to the previous ARGth line in a speedbar buffer at the same depth.
This means that movement is restricted to a subnode, and that siblings
of intermediate nodes are skipped."
  (interactive "p")
  (speedbar-restricted-move (if arg (- arg) -1))
  (speedbar-item-info))

(defun speedbar-navigate-list (arg)
  "Move across ARG groups of similarly typed items in speedbar.
Stop on the first line of the next type of item, or on the last or first item
if we reach a buffer boundary."
  (interactive "p")
  (beginning-of-line)
  (if (looking-at "[0-9]+: *[[<{][-+?][]>}] ")
      (let ((str (regexp-quote (match-string 0))))
	(while (looking-at str)
	  (speedbar-restricted-move arg)
	  (beginning-of-line))))
  (speedbar-position-cursor-on-line))

(defun speedbar-forward-list ()
  "Move forward over the current list.
A LIST in speedbar is a group of similarly typed items, such as directories,
files, or the directory button."
  (interactive)
  (speedbar-navigate-list 1)
  (speedbar-item-info))

(defun speedbar-backward-list ()
  "Move backward over the current list.
A LIST in speedbar is a group of similarly typed items, such as directories,
files, or the directory button."
  (interactive)
  (speedbar-navigate-list -1)
  (speedbar-item-info))

(defun speedbar-scroll-up (&optional arg)
  "Page down one screen-full of the speedbar, or ARG lines."
  (interactive "P")
  (scroll-up arg)
  (speedbar-position-cursor-on-line))

(defun speedbar-scroll-down (&optional arg)
  "Page up one screen-full of the speedbar, or ARG lines."
  (interactive "P")
  (scroll-down arg)
  (speedbar-position-cursor-on-line))

(defun speedbar-up-directory ()
  "Keyboard accelerator for moving the default directory up one.
Assumes that the current buffer is the speedbar buffer."
  (interactive)
  (setq default-directory (expand-file-name (concat default-directory "../")))
  (speedbar-update-contents))

;;; Speedbar file activity (aka creeping featurism)
;;
(defun speedbar-refresh (&optional arg)
  "Refresh the current speedbar display, disposing of any cached data.
Argument ARG represents to force a refresh past any caches that may exist."
  (interactive "P")
  (let ((dl speedbar-shown-directories)
	(dframe-power-click arg)
	deactivate-mark)
    ;; We need to hack something so this works in detached frames.
    (while dl
      (adelete 'speedbar-directory-contents-alist (car dl))
      (setq dl (cdr dl)))
    (if (<= 1 speedbar-verbosity-level)
	(speedbar-message "Refreshing speedbar..."))
    (speedbar-update-contents)
    (speedbar-stealthy-updates)
    ;; Reset the timer in case it got really hosed for some reason...
    (speedbar-set-timer dframe-update-speed)
    (if (<= 1 speedbar-verbosity-level)
	(speedbar-message "Refreshing speedbar...done"))))

(defun speedbar-item-load ()
  "Load the item under the cursor or mouse if it is a Lisp file."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (and (file-exists-p f) (string-match "\\.el\\'" f))
	(if (and (file-exists-p (concat f "c"))
		 (speedbar-y-or-n-p (format "Load %sc? " f)))
	    ;; If the compiled version exists, load that instead...
	    (load-file (concat f "c"))
	  (load-file f))
      (error "Not a loadable file"))))

(defun speedbar-item-byte-compile ()
  "Byte compile the item under the cursor or mouse if it is a Lisp file."
  (interactive)
  (let ((f (speedbar-line-file))
	(sf (selected-frame)))
    (if (and (file-exists-p f) (string-match "\\.el\\'" f))
	(progn
	  (dframe-select-attached-frame speedbar-frame)
	  (byte-compile-file f nil)
	  (select-frame sf)
	  (speedbar-reset-scanners)))
    ))

(defun speedbar-mouse-item-info (event)
  "Provide information about what the user clicked on.
This should be bound to a mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (speedbar-item-info))

(defun speedbar-generic-item-info ()
  "Attempt to derive, and then display information about this line item.
File style information is displayed with `speedbar-item-info'."
  (save-excursion
    (beginning-of-line)
    ;; Skip invisible number info.
    (if (looking-at "\\([0-9]+\\):") (goto-char (match-end 0)))
    ;; Skip items in "folder" type text characters.
    (if (looking-at "\\s-*[[<({].[]>)}] ") (goto-char (match-end 0)))
    ;; Get the text
    (speedbar-message "Text: %s" (buffer-substring-no-properties
				  (point) (line-end-position)))))

(defun speedbar-item-info ()
  "Display info in the minibuffer about the button the mouse is over.
This function can be replaced in `speedbar-mode-functions-list' as
`speedbar-item-info'."
  (interactive)
  (let (message-log-max)
    (funcall (or (speedbar-fetch-replacement-function 'speedbar-item-info)
		 'speedbar-generic-item-info))))

(defun speedbar-item-info-file-helper (&optional filename)
  "Display info about a file that is on the current line.
Return nil if not applicable.  If FILENAME, then use that
instead of reading it from the speedbar buffer."
  (let* ((item (or filename (speedbar-line-file)))
	 (attr (if item (file-attributes item) nil)))
    (if (and item attr) (speedbar-message "%s %-6d %s" (nth 8 attr)
					  (nth 7 attr) item)
      nil)))

(defun speedbar-item-info-tag-helper ()
  "Display info about a tag that is on the current line.
Return nil if not applicable."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward " [-+=]?> \\([^\n]+\\)" (line-end-position) t)
       (let* ((tag (match-string 1))
	      (attr (speedbar-line-token))
	      (item nil)
	      (semantic-tagged (if (fboundp 'semantic-tag-p)
				   (semantic-tag-p attr))))
	  (if semantic-tagged
	    (with-no-warnings
	      (save-excursion
		(when (and (semantic-tag-overlay attr)
			   (semantic-tag-buffer attr))
		  (set-buffer (semantic-tag-buffer attr)))
		(speedbar-message
		 (funcall semantic-sb-info-format-tag-function attr)
		 )))
	    (looking-at "\\([0-9]+\\):")
	    (setq item (file-name-nondirectory (speedbar-line-directory)))
	    (speedbar-message "Tag: %s  in %s" tag item)))
      (if (re-search-forward "{[+-]} \\([^\n]+\\)$" (line-end-position) t)
	  (speedbar-message "Group of tags \"%s\"" (match-string 1))
	(if (re-search-forward " [+-]?[()|@] \\([^\n]+\\)$" nil t)
	    (let* ((detailtext (match-string 1))
		   (detail (or (speedbar-line-token) detailtext))
		   (parent (save-excursion
			     (beginning-of-line)
			     (let ((dep (if (looking-at "[0-9]+:")
					    (1- (string-to-number (match-string 0)))
					  0)))
			       (re-search-backward (concat "^"
			       (int-to-string dep)
			       ":")
						   nil t))
			     (if (looking-at "[0-9]+: +[-+=>]> \\([^\n]+\\)$")
				 (speedbar-line-token)
			       nil))))
	      (if (featurep 'semantic)
		  (with-no-warnings
		    (if (semantic-tag-p detail)
			(speedbar-message
			 (funcall semantic-sb-info-format-tag-function detail parent))
		      (if parent
			  (speedbar-message "Detail: %s of tag %s" detail
					    (if (semantic-tag-p parent)
						(semantic-format-tag-name parent nil t)
					      parent))
			(speedbar-message "Detail: %s" detail))))
		;; Not using `semantic':
		(if parent
		    (speedbar-message "Detail: %s of tag %s" detail parent)
		  (speedbar-message "Detail: %s" detail))))
	  nil)))))

(defun speedbar-files-item-info ()
  "Display info in the minibuffer about the button the mouse is over."
  (if (not speedbar-shown-directories)
      (speedbar-generic-item-info)
    (or (speedbar-item-info-file-helper)
	(speedbar-item-info-tag-helper)
	(speedbar-generic-item-info))))

(defun speedbar-item-copy ()
  "Copy the item under the cursor.
Files can be copied to new names or places."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (not f)	(error "Not a file"))
    (if (file-directory-p f)
	(error "Cannot copy directory")
      (let* ((rt (read-file-name (format "Copy %s to: "
					 (file-name-nondirectory f))
				 (file-name-directory f)))
	     (refresh (member (expand-file-name (file-name-directory rt))
			      speedbar-shown-directories)))
	;; Create the right file name part
	(if (file-directory-p rt)
	    (setq rt
		  (concat (expand-file-name rt)
			  (if (string-match "[/\\]$" rt) "" "/")
			  (file-name-nondirectory f))))
	(if (or (not (file-exists-p rt))
		(speedbar-y-or-n-p (format "Overwrite %s with %s? " rt f)
				   t))
	    (progn
	      (copy-file f rt t t)
	      ;; refresh display if the new place is currently displayed.
	      (if refresh
		  (progn
		    (speedbar-refresh)
		    (if (not (speedbar-goto-this-file rt))
			(speedbar-goto-this-file f))))
	      ))))))

(defun speedbar-item-rename ()
  "Rename the item under the cursor or mouse.
Files can be renamed to new names or moved to new directories."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if f
	(let* ((rt (read-file-name (format "Rename %s to: "
					   (file-name-nondirectory f))
				   (file-name-directory f)))
	       (refresh (member (expand-file-name (file-name-directory rt))
				speedbar-shown-directories)))
	  ;; Create the right file name part
	  (if (file-directory-p rt)
	      (setq rt
		    (concat (expand-file-name rt)
			    (if (string-match "[/\\]\\'" rt) "" "/")
			    (file-name-nondirectory f))))
	  (if (or (not (file-exists-p rt))
		  (speedbar-y-or-n-p (format "Overwrite %s with %s? " rt f)
				     t))
	      (progn
		(rename-file f rt t)
		;; refresh display if the new place is currently displayed.
		(if refresh
		    (progn
		      (speedbar-refresh)
		      (speedbar-goto-this-file rt)
		      )))))
      (error "Not a file"))))

(defun speedbar-create-directory ()
  "Create a directory in speedbar."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if f
	(let* ((basedir (file-name-directory f))
	       (nd (read-directory-name "Create directory: "
				   basedir)))
	  ;; Make the directory
	  (make-directory nd t)
	  (speedbar-refresh)
	  (speedbar-goto-this-file nd)
	  )
      (error "Not a file"))))

(defun speedbar-item-delete ()
  "Delete the item under the cursor.  Files are removed from disk."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (not f) (error "Not a file"))
    (if (speedbar-y-or-n-p (format "Delete %s? " f) t)
	(progn
	  (if (file-directory-p f)
	      (delete-directory f t t)
	    (delete-file f t))
	  (speedbar-message "Okie dokie.")
	  (let ((p (point)))
	    (speedbar-refresh)
	    (goto-char p))
	  ))
    ))

(defun speedbar-item-object-delete ()
  "Delete the object associated from the item under the cursor.
The file is removed from disk.  The object is determined from the
variable `speedbar-obj-alist'."
  (interactive)
  (let* ((f (speedbar-line-file))
	 (obj nil)
	 (oa speedbar-obj-alist))
    (if (not f) (error "Not a file"))
    (while (and oa (not (string-match (car (car oa)) f)))
      (setq oa (cdr oa)))
    (setq obj (concat (file-name-sans-extension f) (cdr (car oa))))
    (if (and oa (file-exists-p obj)
	     (speedbar-y-or-n-p (format "Delete %s? " obj) t))
	(progn
	  (delete-file obj)
	  (speedbar-reset-scanners)))))

(defun speedbar-enable-update ()
  "Enable automatic updating in speedbar via timers."
  (interactive)
  (setq speedbar-update-flag t)
  (speedbar-set-mode-line-format)
  (speedbar-set-timer dframe-update-speed))

(defun speedbar-disable-update ()
  "Disable automatic updating and stop consuming resources."
  (interactive)
  (setq speedbar-update-flag nil)
  (speedbar-set-mode-line-format)
  (speedbar-set-timer nil))

(defun speedbar-toggle-updates ()
  "Toggle automatic update for the speedbar frame."
  (interactive)
  (if speedbar-update-flag
      (speedbar-disable-update)
    (speedbar-enable-update)))

(defun speedbar-toggle-images ()
  "Toggle use of images in the speedbar frame."
  (interactive)
  (setq speedbar-use-images (not speedbar-use-images))
  (speedbar-refresh))

(defun speedbar-toggle-sorting ()
  "Toggle tag sorting."
  (interactive)
  (setq speedbar-sort-tags (not speedbar-sort-tags)))

(defun speedbar-toggle-show-all-files ()
  "Toggle display of files speedbar can not tag."
  (interactive)
  (setq speedbar-show-unknown-files (not speedbar-show-unknown-files))
  (speedbar-refresh))

(defmacro speedbar-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (list 'let '((inhibit-read-only t))
	(cons 'progn forms)))
(put 'speedbar-with-writable 'lisp-indent-function 0)

(defun speedbar-insert-button (text face mouse function
				    &optional token prevline)
  "Insert TEXT as the next logical speedbar button.
FACE is the face to put on the button, MOUSE is the highlight face to use.
When the user clicks on TEXT, FUNCTION is called with the TOKEN parameter.
This function assumes that the current buffer is the speedbar buffer.
If PREVLINE, then put this button on the previous line.

This is a convenience function for special modes that create their own
specialized speedbar displays."
  (goto-char (point-max))
  (let ((start (point)))
    (if (/= (current-column) 0) (insert "\n"))
    (put-text-property start (point) 'invisible nil))
  (if prevline (progn (delete-char -1)
		      (insert " ") ;back up if desired...
		      (put-text-property (1- (point)) (point) 'invisible nil)))
  (let ((start (point)))
    (insert text)
    (speedbar-make-button start (point) face mouse function token))
  (let ((start (point)))
    (insert "\n")
    (add-text-properties
     start (point) '(face nil invisible nil mouse-face nil))))

(defun speedbar-insert-separator (text)
  "Insert a separation label of TEXT.
Separators are not active, have no labels, depth, or actions."
  (if speedbar-use-images
      (let ((start (point)))
	(insert "//")
	(speedbar-insert-image-button-maybe start 2)))
  (let ((start (point)))
    (insert text "\n")
    (speedbar-make-button start (point)
			  'speedbar-separator-face
			  nil nil nil)))

(defun speedbar-make-button (start end face mouse function &optional token)
  "Create a button from START to END, with FACE as the display face.
MOUSE is the mouse face.  When this button is clicked on FUNCTION
will be run with the TOKEN parameter (any Lisp object).  If FACE
is t use the text properties of the string that is passed as an
argument."
  (unless (eq face t)
    (put-text-property start end 'face face))
  (add-text-properties
   start end `(mouse-face ,mouse invisible nil
               speedbar-text ,(buffer-substring-no-properties start end)))
  (if speedbar-use-tool-tips-flag
      (put-text-property start end 'help-echo #'dframe-help-echo))
  (if function (put-text-property start end 'speedbar-function function))
  (if token (put-text-property start end 'speedbar-token token))
  ;; So far the only text we have is less that 3 chars.
  (if (<= (- end start) 3)
      (speedbar-insert-image-button-maybe start (- end start)))
  )

;;; Initial Expansion list management
;;
(defun speedbar-initial-expansion-list ()
  "Return the current default expansion list.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-initial-expansion-mode-alist'."
  ;; cdr1 - name, cdr2 - menu
  (cdr (cdr (cdr (assoc speedbar-initial-expansion-list-name
			speedbar-initial-expansion-mode-alist)))))

(defun speedbar-initial-menu ()
  "Return the current default menu data.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-initial-expansion-mode-alist'."
  (symbol-value
   (car (cdr (assoc speedbar-initial-expansion-list-name
		    speedbar-initial-expansion-mode-alist)))))

(defun speedbar-initial-keymap ()
  "Return the current default menu data.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-initial-expansion-mode-alist'."
  (symbol-value
   (car (cdr (cdr (assoc speedbar-initial-expansion-list-name
			 speedbar-initial-expansion-mode-alist))))))

(defun speedbar-initial-stealthy-functions ()
  "Return a list of functions to call stealthily.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-stealthy-function-list'."
  (cdr (assoc speedbar-initial-expansion-list-name
	      speedbar-stealthy-function-list)))

(defun speedbar-add-expansion-list (new-list)
  "Add NEW-LIST to the list of expansion lists."
  (add-to-list 'speedbar-initial-expansion-mode-alist new-list))

(defun speedbar-change-initial-expansion-list (new-default)
  "Change speedbar's default expansion list to NEW-DEFAULT."
  (interactive
   (list
    (completing-read (format "Speedbar Mode (default %s): "
			     speedbar-previously-used-expansion-list-name)
		     speedbar-initial-expansion-mode-alist
		     nil t "" nil
		     speedbar-previously-used-expansion-list-name)))
  (setq speedbar-previously-used-expansion-list-name
	speedbar-initial-expansion-list-name
	speedbar-initial-expansion-list-name new-default)
  (if (and (speedbar-current-frame) (frame-live-p (speedbar-current-frame)))
      (progn
	(speedbar-refresh)
	(speedbar-reconfigure-keymaps))))

(defun speedbar-fetch-replacement-function (function)
  "Return a current mode-specific replacement for function, or nil.
Scans `speedbar-mode-functions-list' first for the current mode, then
for FUNCTION."
  (cdr (assoc function
	      (cdr (assoc speedbar-initial-expansion-list-name
			  speedbar-mode-functions-list)))))

(defun speedbar-add-mode-functions-list (new-list)
  "Add NEW-LIST to the list of mode functions.
See `speedbar-mode-functions-list' for details."
  (add-to-list 'speedbar-mode-functions-list new-list))


;;; Special speedbar display management
;;
(defun speedbar-maybe-add-localized-support (buffer)
  "Quick check function called on BUFFERs by the speedbar timer function.
Maintains the value of local variables which control speedbar's use
of the special mode functions."
  (or speedbar-special-mode-expansion-list
      (speedbar-add-localized-speedbar-support buffer)))

(defun speedbar-add-localized-speedbar-support (buffer)
  "Add localized speedbar support to BUFFER's mode if it is available."
  (interactive "bBuffer: ")
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (not (buffer-live-p buffer))
      nil
    (with-current-buffer buffer
      (save-match-data
	(let ((ms (symbol-name major-mode)) v)
	  (if (not (string-match "-mode$" ms))
	      nil ;; do nothing to broken mode
	    (setq ms (substring ms 0 (match-beginning 0)))
	    (setq v (intern-soft (concat ms "-speedbar-buttons")))
	    (make-local-variable 'speedbar-special-mode-expansion-list)
	    (if (not v)
		(setq speedbar-special-mode-expansion-list t)
	      ;; If it is autoloaded, we need to load it now so that
	      ;; we have access to the variable -speedbar-menu-items.
	      ;; Is this XEmacs safe?
	      (let ((sf (symbol-function v)))
		(if (and (listp sf) (eq (car sf) 'autoload))
		    (load-library (car (cdr sf)))))
	      (setq speedbar-special-mode-expansion-list (list v))
	      (setq v (intern-soft (concat ms "-speedbar-key-map")))
	      (if (not v)
		  nil ;; don't add special keymap
		(make-local-variable 'speedbar-special-mode-key-map)
		(setq speedbar-special-mode-key-map
		      (symbol-value v)))
	      (setq v (intern-soft (concat ms "-speedbar-menu-items")))
	      (if (not v)
		  nil ;; don't add special menus
		(make-local-variable 'speedbar-easymenu-definition-special)
		(setq speedbar-easymenu-definition-special
		      (symbol-value v)))
	      )))))))

(defun speedbar-remove-localized-speedbar-support (buffer)
  "Remove any traces that BUFFER supports speedbar in a specialized way."
  (with-current-buffer buffer
    (kill-local-variable 'speedbar-special-mode-expansion-list)
    (kill-local-variable 'speedbar-special-mode-key-map)
    (kill-local-variable 'speedbar-easymenu-definition-special)))

;;; File button management
;;
(defun speedbar-file-lists (directory)
  "Create file lists for DIRECTORY.
The car is the list of directories, the cdr is list of files not
matching ignored headers.  Cache any directory files found in
`speedbar-directory-contents-alist' and use that cache before scanning
the file-system."
  (setq directory (expand-file-name directory))
  ;; If in powerclick mode, then the directory we are getting
  ;; should be rescanned.
  (if dframe-power-click
      (adelete 'speedbar-directory-contents-alist directory))
  ;; find the directory, either in the cache, or build it.
  (or (cdr-safe (assoc directory speedbar-directory-contents-alist))
      (let ((default-directory directory)
	    (dir (directory-files directory nil))
	    (dirs nil)
	    (files nil))
	(while dir
	  (if (not
	       (or (string-match speedbar-file-unshown-regexp (car dir))
		   (member (car dir) vc-directory-exclusion-list)
		   (string-match speedbar-directory-unshown-regexp (car dir))))
	      (if (file-directory-p (car dir))
		  (setq dirs (cons (car dir) dirs))
		(setq files (cons (car dir) files))))
	  (setq dir (cdr dir)))
	(let ((nl (cons (nreverse dirs) (list (nreverse files)))))
	  (aput 'speedbar-directory-contents-alist directory nl)
	  nl))
      ))

(defun speedbar-directory-buttons (directory _index)
  "Insert a single button group at point for DIRECTORY.
Each directory part is a different button.  If part of the directory
matches the user directory ~, then it is replaced with a ~.
INDEX is not used, but is required by the caller."
  (let* ((tilde (expand-file-name "~/"))
	 (dd (expand-file-name directory))
	 (junk (string-match (regexp-quote tilde) dd))
	 (displayme (if junk
			(concat "~/" (substring dd (match-end 0)))
		      dd))
	 (p (point)))
    (if (string-match "^~[/\\]?\\'" displayme) (setq displayme tilde))
    (insert displayme)
    (save-excursion
      (goto-char p)
      (while (re-search-forward "\\([^/\\]+\\)[/\\]" nil t)
	(speedbar-make-button (match-beginning 1) (match-end 1)
			      'speedbar-directory-face
			      'speedbar-highlight-face
			      'speedbar-directory-buttons-follow
			      (if (and (= (match-beginning 1) p)
				       (not (char-equal (char-after (+ p 1)) ?:)))
				  (expand-file-name "~/")  ;the tilde
				(buffer-substring-no-properties
				 p (match-end 0)))))
      ;; Nuke the beginning of the directory if it's too long...
      (cond ((eq speedbar-directory-button-trim-method 'span)
	     (beginning-of-line)
	     (let ((ww (or (speedbar-frame-width) 20)))
	       (move-to-column ww nil)
	       (while (>= (current-column) ww)
		 (re-search-backward "[/\\]" nil t)
		 (if (<= (current-column) 2)
		     (progn
		       (re-search-forward "[/\\]" nil t)
		       (if (< (current-column) 4)
			   (re-search-forward "[/\\]" nil t))
		       (forward-char -1)))
		 (if (looking-at "[/\\]?$")
		     (beginning-of-line)
		   (insert "/...\n ")
		   (move-to-column ww nil)))))
	    ((eq speedbar-directory-button-trim-method 'trim)
	     (end-of-line)
	     (let ((ww (or (speedbar-frame-width) 20))
		   (tl (current-column)))
	       (if (< ww tl)
		   (progn
		     (move-to-column (- tl ww))
		     (if (re-search-backward "[/\\]" nil t)
			 (progn
			   (delete-region (point-min) (point))
			   (insert "$")
			   )))))))
      )
    (if (string-match "\\`[/\\][^/\\]+[/\\]\\'" displayme)
	(progn
	  (insert "  ")
	  (let ((p (point)))
	    (insert "<root>")
	    (speedbar-make-button p (point)
				  'speedbar-directory-face
				  'speedbar-highlight-face
				  'speedbar-directory-buttons-follow
				  "/"))))
    (end-of-line)
    (insert-char ?\n 1 nil)))

(defun speedbar-make-tag-line (exp-button-type
			       exp-button-char exp-button-function
			       exp-button-data
			       tag-button tag-button-function tag-button-data
			       tag-button-face depth)
  "Create a tag line with EXP-BUTTON-TYPE for the small expansion button.
This is the button that expands or contracts a node (if applicable),
and EXP-BUTTON-CHAR the character in it (+, -, ?, etc).  EXP-BUTTON-FUNCTION
is the function to call if it's clicked on.  Button types are
'bracket, 'angle, 'curly, 'expandtag, 'statictag, t, or nil.
EXP-BUTTON-DATA is extra data attached to the text forming the expansion
button.

Next, TAG-BUTTON is the text of the tag.  TAG-BUTTON-FUNCTION is the
function to call if clicked on, and TAG-BUTTON-DATA is the data to
attach to the text field (such a tag positioning, etc).
TAG-BUTTON-FACE is a face used for this type of tag.

Lastly, DEPTH shows the depth of expansion.

This function assumes that the cursor is in the speedbar window at the
position to insert a new item, and that the new item will end with a CR."
  (let ((start (point))
	(end (progn
	       (insert (int-to-string depth) ":")
	       (point)))
	(depthspacesize (* depth speedbar-indentation-width)))
    (put-text-property start end 'invisible t)
    (insert-char ?  depthspacesize nil)
    (put-text-property (- (point) depthspacesize) (point) 'invisible nil)
    (let* ((exp-button (cond ((eq exp-button-type 'bracket) "[%c]")
			     ((eq exp-button-type 'angle) "<%c>")
			     ((eq exp-button-type 'curly) "{%c}")
			     ((eq exp-button-type 'expandtag) " %c>")
			     ((eq exp-button-type 'statictag) " =>")
			     (t ">")))
	   (buttxt (format exp-button exp-button-char))
	   (start (point))
	   (end (progn (insert buttxt) (point)))
	   (bf (if (and exp-button-type (not (eq exp-button-type 'statictag)))
		   'speedbar-button-face nil))
	   (mf (if exp-button-function 'speedbar-highlight-face nil))
	   )
      (speedbar-make-button start end bf mf exp-button-function exp-button-data)
      (if speedbar-hide-button-brackets-flag
	  (progn
	    (put-text-property start (1+ start) 'invisible t)
	    (put-text-property end (1- end) 'invisible t)))
      )
    (insert-char ?  1 nil)
    (put-text-property (1- (point)) (point) 'invisible nil)
    (let ((start (point))
	  (end (progn (insert tag-button) (point))))
      (insert-char ?\n 1 nil)
      (put-text-property (1- (point)) (point) 'invisible nil)
      (speedbar-make-button start end tag-button-face
			    (if tag-button-function 'speedbar-highlight-face nil)
			    tag-button-function tag-button-data))
    ))

(defun speedbar-change-expand-button-char (char)
  "Change the expansion button character to CHAR for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward ":\\s-*.\\([-+?]\\)" (line-end-position) t)
	(speedbar-with-writable
	  (goto-char (match-end 1))
	  (insert-char char 1 t)
	  (forward-char -1)
	  (delete-char -1)
	  ;;(put-text-property (point) (1- (point)) 'invisible nil)
	  ;; make sure we fix the image on the text here.
	  (speedbar-insert-image-button-maybe (- (point) 1) 3)))))


;;; Build button lists
;;
(defun speedbar-insert-files-at-point (files level)
  "Insert list of FILES starting at point, and indenting all files to LEVEL.
Tag expandable items with a +, otherwise a ?.  Don't highlight ? as we
don't know how to manage them.  The input parameter FILES is a cons
cell of the form ( 'DIRLIST . 'FILELIST )."
  ;; Start inserting all the directories
  (let ((dirs (car files)))
    (while dirs
      (speedbar-make-tag-line 'angle ?+ 'speedbar-dired (car dirs)
			      (car dirs) 'speedbar-dir-follow nil
			      'speedbar-directory-face level)
      (setq dirs (cdr dirs))))
  (let ((lst (car (cdr files)))
	(case-fold-search t))
    (while lst
      (let* ((known (string-match speedbar-file-regexp (car lst)))
	     (expchar (if known ?+ ??))
	     (fn (if known 'speedbar-tag-file nil)))
	(if (or speedbar-show-unknown-files (/= expchar ??))
	    (speedbar-make-tag-line 'bracket expchar fn (car lst)
				    (car lst) 'speedbar-find-file nil
				    'speedbar-file-face level)))
      (setq lst (cdr lst)))))

(defun speedbar-default-directory-list (directory index)
  "Insert files for DIRECTORY with level INDEX at point."
  (speedbar-insert-files-at-point
   (speedbar-file-lists directory) index)
  (speedbar-reset-scanners)
  (if (= index 0)
      ;; If the shown files variable has extra directories, then
      ;; it is our responsibility to redraw them all
      ;; Luckily, the nature of inserting items into this list means
      ;; that by reversing it, we can easily go in the right order
      (let ((sf (cdr (reverse speedbar-shown-directories))))
	(setq speedbar-shown-directories
	      (list (expand-file-name default-directory)))
	;; Expand them all as we find them.
	(while sf
	  (if (speedbar-goto-this-file (car sf))
	      (progn
		(beginning-of-line)
		(if (looking-at "[0-9]+:[ ]*<")
		    (progn
		      (goto-char (match-end 0))
		      (speedbar-do-function-pointer)))))
	  (setq sf (cdr sf)))
	)))
;;; Generic List support
;;
;;  Generic lists are hierarchies of tags which we may need to permute
;;  in order to make it look nice.
;;
;;  A generic list is of the form:
;;  ( ("name" . marker-or-number)              <-- one tag at this level
;;    ("name" ("name" . mon) ("name" . mon) )  <-- one group of tags
;;    ("name" mon ("name" . mon) )             <-- group w/ a position and tags
(defun speedbar-generic-list-group-p (sublst)
  "Non-nil if SUBLST is a group.
Groups may optionally contain a position."
  (and (stringp (car-safe sublst))
       (or (and (listp (cdr-safe sublst))
		(or (speedbar-generic-list-tag-p (car-safe (cdr-safe sublst)))
		    (speedbar-generic-list-group-p (car-safe (cdr-safe sublst))
						   )))
	   (and (number-or-marker-p (car-safe (cdr-safe sublst)))
		(listp (cdr-safe (cdr-safe sublst)))
		(speedbar-generic-list-tag-p
		 (car-safe (cdr-safe (cdr-safe sublst)))))
	   )))

(defun speedbar-generic-list-positioned-group-p (sublst)
  "Non-nil if SUBLST is a group with a position."
  (and (stringp (car-safe sublst))
       (number-or-marker-p (car-safe (cdr-safe sublst)))
       (listp (cdr-safe (cdr-safe sublst)))
       (let ((rest (car-safe (cdr-safe (cdr-safe sublst)))))
	 (or (speedbar-generic-list-tag-p rest)
	     (speedbar-generic-list-group-p rest)
	     (speedbar-generic-list-positioned-group-p rest)
	     ))))

(defun speedbar-generic-list-tag-p (sublst)
  "Non-nil if SUBLST is a tag."
  (and (stringp (car-safe sublst))
       (or (and (number-or-marker-p (cdr-safe sublst))
		(not (cdr-safe (cdr-safe sublst))))
	   ;; For semantic/bovine items, this is needed
	   (symbolp (car-safe (cdr-safe sublst))))
       ))

(defun speedbar-sort-tag-hierarchy (lst)
  "Sort all elements of tag hierarchy LST."
  (sort (copy-alist lst)
	(lambda (a b) (string< (car a) (car b)))))

(defun speedbar-try-completion (string alist)
  "A wrapper for `try-completion'.
Passes STRING and ALIST to `try-completion' if ALIST
passes some tests."
  (if (and (consp alist)
	   (listp (car alist)) (stringp (car (car alist))))
      (try-completion string alist)
    nil))

(defun speedbar-prefix-group-tag-hierarchy (lst)
  "Prefix group names for tag hierarchy LST."
  (let ((newlst nil)
	(sublst nil)
	(work-list nil)
	(junk-list nil)
	(short-group-list nil)
	(short-start-name nil)
	(short-end-name nil)
	(num-shorts-grouped 0)
	(bins (make-vector 256 nil))
	(diff-idx 0))
    (if (<= (length lst) speedbar-tag-regroup-maximum-length)
	;; Do nothing.  Too short to bother with.
	lst
      ;; Break out sub-lists
      (while lst
	(if (speedbar-generic-list-group-p (car-safe lst))
	    (setq newlst (cons (car lst) newlst))
	  (setq sublst (cons (car lst) sublst)))
	(setq lst (cdr lst)))
      ;; Reverse newlst because it was made backwards.
      ;; Sublist doesn't need reversing because the act
      ;; of binning things will reverse it for us.
      (setq newlst (nreverse newlst)
	    sublst sublst)
      ;; Now, first find out how long our list is.  Never let a
      ;; list get-shorter than our minimum.
      (if (<= (length sublst) speedbar-tag-split-minimum-length)
	  (setq work-list sublst)
	(setq diff-idx (length (speedbar-try-completion "" sublst)))
	;; Sort the whole list into bins.
	(while sublst
	  (let ((e (car sublst))
		(s (car (car sublst))))
	    (cond ((<= (length s) diff-idx)
		   ;; 0 storage bin for shorty.
		   (aset bins 0 (cons e (aref bins 0))))
		  (t
		   ;; stuff into a bin based on ascii value at diff
		   (aset bins (aref s diff-idx)
			 (cons e (aref bins (aref s diff-idx)))))))
	  (setq sublst (cdr sublst)))
	;; Go through all our bins  Stick singles into our
	;; junk-list, everything else as sublsts in work-list.
	;; If two neighboring lists are both small, make a grouped
	;; group combining those two sub-lists.
	(setq diff-idx 0)
	(while (> 256 diff-idx)
	  ;; The bins contents are currently in forward order.
	  (let ((l (aref bins diff-idx)))
	    (if l
		(let ((tmp (cons (speedbar-try-completion "" l) l)))
		  (if (or (> (length l) speedbar-tag-regroup-maximum-length)
			  (> (+ (length l) (length short-group-list))
			     speedbar-tag-split-minimum-length))
		      (progn
			;; We have reached a longer list, so we
			;; must finish off a grouped group.
			(cond
			 ((and short-group-list
			       (= (length short-group-list)
				  num-shorts-grouped))
			  ;; All singles?  Junk list
			  (setq junk-list (append (nreverse short-group-list)
						  junk-list)))
			 ((= num-shorts-grouped 1)
			  ;; Only one short group?  Just stick it in
			  ;; there by itself.  Make a group, and find
			  ;; a subexpression
			  (let ((subexpression (speedbar-try-completion
						"" short-group-list)))
			    (if (< (length subexpression)
				   speedbar-tag-group-name-minimum-length)
				(setq subexpression
				      (concat short-start-name
					      " ("
					      (substring
					       (car (car short-group-list))
					       (length short-start-name))
					      ")")))
			    (setq work-list
				  (cons (cons subexpression
					      short-group-list)
					work-list ))))
			 (short-group-list
			  ;; Multiple groups to be named in a special
			  ;; way by displaying the range over which we
			  ;; have grouped them.
			  (setq work-list
				(cons (cons (concat short-start-name
						    " to "
						    short-end-name)
					    short-group-list)
				      work-list))))
		     ;; Reset short group list information every time.
			(setq short-group-list nil
			      short-start-name nil
			      short-end-name nil
			      num-shorts-grouped 0)))
		  ;; Ok, now that we cleaned up the short-group-list,
		  ;; we can deal with this new list, to decide if it
		  ;; should go on one of these sub-lists or not.
		  (if (< (length l) speedbar-tag-regroup-maximum-length)
		      (setq short-group-list (append l short-group-list)
			    num-shorts-grouped (1+ num-shorts-grouped)
			    short-end-name (car tmp)
			    short-start-name (if short-start-name
						 short-start-name
					       (car tmp)))
		    (setq work-list (cons tmp work-list))))))
	  (setq diff-idx (1+ diff-idx))))
      ;; Did we run out of things?  Drop our new list onto the end.
      (cond
       ((and short-group-list (= (length short-group-list) num-shorts-grouped))
	;; All singles?  Junk list
	(setq junk-list (append short-group-list junk-list)))
       ((= num-shorts-grouped 1)
	;; Only one short group?  Just stick it in
	;; there by itself.
	(setq work-list
	      (cons (cons (speedbar-try-completion "" short-group-list)
			  short-group-list)
		    work-list)))
       (short-group-list
	;; Multiple groups to be named in a special
	;; way by displaying the range over which we
	;; have grouped them.
	(setq work-list
	      (cons (cons (concat short-start-name " to " short-end-name)
			  short-group-list)
		    work-list))))
      ;; Reverse the work list nreversed when consing.
      (setq work-list (nreverse work-list))
      ;; Now, stick our new list onto the end of
      (if work-list
	  (if junk-list
	      (append newlst work-list junk-list)
	    (append newlst work-list))
	(append  newlst junk-list)))))

(defun speedbar-trim-words-tag-hierarchy (lst)
  "Trim all words in a tag hierarchy.
Base trimming information on word separators, and group names.
Argument LST is the list of tags to trim."
  (let ((newlst nil)
	(sublst nil)
	(trim-prefix nil)
	(trim-chars 0)
	(trimlst nil))
    (while lst
      (if (speedbar-generic-list-group-p (car-safe lst))
	  (setq newlst (cons (car lst) newlst))
	(setq sublst (cons (car lst) sublst)))
      (setq lst (cdr lst)))
    ;; Get the prefix to trim by.  Make sure that we don't trim
    ;; off silly pieces, only complete understandable words.
    (setq trim-prefix (speedbar-try-completion "" sublst)
	  newlst (nreverse newlst))
    (if (or (= (length sublst) 1)
	    (not trim-prefix)
	    (not (string-match "\\(\\w+\\W+\\)+" trim-prefix)))
	(append newlst (nreverse sublst))
      (setq trim-prefix (substring trim-prefix (match-beginning 0)
				   (match-end 0)))
      (setq trim-chars (length trim-prefix))
      (while sublst
	(setq trimlst (cons
		       (cons (substring (car (car sublst)) trim-chars)
			     (cdr (car sublst)))
		       trimlst)
	      sublst (cdr sublst)))
      ;; Put the lists together
      (append newlst trimlst))))

(defun speedbar-simple-group-tag-hierarchy (lst)
  "Create a simple 'Tags' group with orphaned tags.
Argument LST is the list of tags to sort into groups."
  (let ((newlst nil)
	(sublst nil))
    (while lst
      (if (speedbar-generic-list-group-p (car-safe lst))
	  (setq newlst (cons (car lst) newlst))
	(setq sublst (cons (car lst) sublst)))
      (setq lst (cdr lst)))
    (if (not newlst)
	(nreverse sublst)
      (setq newlst (cons (cons "Tags" (nreverse sublst)) newlst))
      (nreverse newlst))))

(defun speedbar-create-tag-hierarchy (lst)
  "Adjust the tag hierarchy in LST, and return it.
This uses `speedbar-tag-hierarchy-method' to determine how to adjust
the list."
  (let* ((f (save-excursion
	      (forward-line -1)
	      (or (speedbar-line-file)
		  (speedbar-line-directory))))
	 (methods (if (get-file-buffer f)
		      (with-current-buffer (get-file-buffer f)
                        speedbar-tag-hierarchy-method)
		    speedbar-tag-hierarchy-method))
	 (lst (if (fboundp 'copy-tree)
		  (copy-tree lst)
		lst)))
    (while methods
      (setq lst (funcall (car methods) lst)
	    methods (cdr methods)))
    lst))

(defvar speedbar-generic-list-group-expand-button-type 'curly
  "The type of button created for groups of tags.
Good values for this are `curly' and `expandtag'.
Make buffer local for your mode.")

(defvar speedbar-generic-list-tag-button-type nil
  "The type of button created for tags in generic lists.
Good values for this are nil and `statictag'.
Make buffer local for your mode.")

(defun speedbar-insert-generic-list (level lst expand-fun find-fun)
  "At LEVEL, insert a generic multi-level alist LST.
Associations with lists get {+} tags (to expand into more nodes) and
those with positions just get a > as the indicator.  {+} buttons will
have the function EXPAND-FUN and the token is the cdr list.  The token
name will have the function FIND-FUN and not token."
  ;; Remove imenu rescan button
  (if (string= (car (car lst)) "*Rescan*")
      (setq lst (cdr lst)))
  ;; Get, and set up variables that define how we treat these tags.
   (let ((f (save-excursion (forward-line -1)
			    (or (speedbar-line-file)
				(speedbar-line-directory))))
	 expand-button tag-button)
     (save-excursion
       (if (get-file-buffer f)
	   (set-buffer (get-file-buffer f)))
       (setq expand-button speedbar-generic-list-group-expand-button-type
	     tag-button speedbar-generic-list-tag-button-type))
     ;; Adjust the list.
     (setq lst (speedbar-create-tag-hierarchy lst))
     ;; insert the parts
     (while lst
       (cond ((null (car-safe lst)) nil)	;this would be a separator
	    ((speedbar-generic-list-tag-p (car lst))
	     (speedbar-make-tag-line tag-button
				     nil nil nil ;no expand button data
				     (car (car lst)) ;button name
				     find-fun ;function
				     (cdr (car lst)) ;token is position
				     'speedbar-tag-face
				     (1+ level)))
	    ((speedbar-generic-list-positioned-group-p (car lst))
	     (speedbar-make-tag-line expand-button
				     ?+ expand-fun (cdr (cdr (car lst)))
				     (car (car lst)) ;button name
				     find-fun ;function
				     (car (cdr (car lst))) ;token is posn
				     'speedbar-tag-face
				     (1+ level)))
	    ((speedbar-generic-list-group-p (car lst))
	     (speedbar-make-tag-line expand-button
				     ?+ expand-fun (cdr (car lst))
				     (car (car lst)) ;button name
				     nil nil 'speedbar-tag-face
				     (1+ level)))
	    (t (speedbar-message "speedbar-insert-generic-list: malformed list!")
	       ))
      (setq lst (cdr lst)))))

(defun speedbar-insert-imenu-list (indent lst)
  "At level INDENT, insert the imenu generated LST."
  (speedbar-insert-generic-list indent lst
				'speedbar-tag-expand
				'speedbar-tag-find))

(defun speedbar-insert-etags-list (indent lst)
  "At level INDENT, insert the etags generated LST."
  (speedbar-insert-generic-list indent lst
				'speedbar-tag-expand
				'speedbar-tag-find))

;;; Timed functions
;;
(defun speedbar-update-contents ()
  "Generically update the contents of the speedbar buffer."
  (interactive)
  ;; Set the current special buffer
  (setq speedbar-desired-buffer nil)

  ;; Check for special modes
  (speedbar-maybe-add-localized-support (current-buffer))

  ;; Choose the correct method of doodling.
  (if (and speedbar-mode-specific-contents-flag
	   (consp speedbar-special-mode-expansion-list)
	   (local-variable-p
	    'speedbar-special-mode-expansion-list
	    (current-buffer)))
      ;;(eq (get major-mode 'mode-class 'special)))
      (speedbar-update-special-contents)
    (speedbar-update-directory-contents)))

(defun speedbar-update-localized-contents ()
  "Update the contents of the speedbar buffer for the current situation."
  ;; Due to the historical growth of speedbar, we need to do something
  ;; special for "files" mode.  Too bad.
  (let ((name speedbar-initial-expansion-list-name)
	(funclst (speedbar-initial-expansion-list))
	)
    (if (string= name "files")
	;; Do all the files type work.  It still goes through the
	;; expansion list stuff. :(
	(if (or (member (expand-file-name default-directory)
			speedbar-shown-directories)
		(and speedbar-ignored-directory-regexp
		     (string-match
		      speedbar-ignored-directory-regexp
		      (expand-file-name default-directory))))
	    nil
	  (if (<= 1 speedbar-verbosity-level)
	      (speedbar-message "Updating speedbar to: %s..."
				default-directory))
	  (speedbar-update-directory-contents)
	  (if (<= 1 speedbar-verbosity-level)
	      (progn
		(speedbar-message "Updating speedbar to: %s...done"
				  default-directory)
		(speedbar-message nil))))
      ;; Else, we can do a short cut.  No text cache.
      (let ((cbd (expand-file-name default-directory)))
	(set-buffer speedbar-buffer)
	(speedbar-with-writable
	  (let* ((window (get-buffer-window speedbar-buffer 0))
		 (p (window-point window))
		 (start (window-start window)))
	    (erase-buffer)
	    (dolist (func funclst)
	      (setq default-directory cbd)
	      (funcall func cbd 0))
	    (speedbar-reconfigure-keymaps)
	    (set-window-point window p)
	    (set-window-start window start)))))))

(defun speedbar-update-directory-contents ()
  "Update the contents of the speedbar buffer based on the current directory."
    (let ((cbd (expand-file-name default-directory))
	  cbd-parent
	  (funclst (speedbar-initial-expansion-list))
	  (cache speedbar-full-text-cache)
	  ;; disable stealth during update
	  (speedbar-stealthy-function-list nil)
	  (use-cache nil)
	  (expand-local nil)
	  ;; Because there is a bug I can't find just yet
	  (inhibit-quit nil))
      (set-buffer speedbar-buffer)
      ;; If we are updating contents to where we are, then this is
      ;; really a request to update existing contents, so we must be
      ;; careful with our text cache!
      (if (member cbd speedbar-shown-directories)
	  (progn
	    (setq cache nil)
	    ;; If the current directory is not the last element in the dir
	    ;; list, then we ALSO need to zap the list of expanded directories
	    (if (/= (length (member cbd speedbar-shown-directories)) 1)
		(setq speedbar-shown-directories (list cbd))))

	;; Build cbd-parent, and see if THAT is in the current shown
	;; directories.  First, go through pains to get the parent directory
	(if (and speedbar-smart-directory-expand-flag
		 (save-match-data
		   (setq cbd-parent cbd)
		   (if (string-match "[/\\]$" cbd-parent)
		       (setq cbd-parent (substring cbd-parent 0
						   (match-beginning 0))))
		   (setq cbd-parent (file-name-directory cbd-parent)))
		 (member cbd-parent speedbar-shown-directories))
	    (setq expand-local t)

	  ;; If this directory is NOT in the current list of available
	  ;; directories, then use the cache, and set the cache to our new
	  ;; value.  Make sure to unhighlight the current file, or if we
	  ;; come back to this directory, it might be a different file
	  ;; and then we get a mess!
	  (if (> (point-max) 1)
	      (progn
		(speedbar-clear-current-file)
		(setq speedbar-full-text-cache
		      (cons speedbar-shown-directories (buffer-string)))))

	  ;; Check if our new directory is in the list of directories
	  ;; shown in the text-cache
	  (if (member cbd (car cache))
	      (setq speedbar-shown-directories (car cache)
		    use-cache t)
	    ;; default the shown directories to this list...
	    (setq speedbar-shown-directories (list cbd)))
	  ))
      (if (not expand-local) (setq speedbar-last-selected-file nil))
      (speedbar-with-writable
	(if (and expand-local
		 ;; Find this directory as a speedbar node.
		 (speedbar-directory-line cbd))
	    ;; Open it.
	    (speedbar-expand-line)
	  (let* ((window (get-buffer-window speedbar-buffer 0))
		 (p (window-point window))
		 (start (window-start window)))
	    (erase-buffer)
	    (cond (use-cache
		   (setq default-directory
			 (nth (1- (length speedbar-shown-directories))
			      speedbar-shown-directories))
		   (insert (cdr cache)))
		  (t
		   (dolist (func funclst)
		     (setq default-directory cbd)
	    (funcall func cbd 0))))
	    (set-window-point window p)
	    (set-window-start window start)))))
  (speedbar-reconfigure-keymaps))

(defun speedbar-update-special-contents ()
  "Use the mode-specific variable to fill in the speedbar buffer.
This should only be used by modes classified as special."
  (let ((funclst speedbar-special-mode-expansion-list)
	(specialbuff (current-buffer)))
    (setq speedbar-desired-buffer specialbuff)
    (with-current-buffer speedbar-buffer
      ;; If we are leaving a directory, cache it.
      (if (not speedbar-shown-directories)
	  ;; Do nothing
	  nil
	;; Clean up directory maintenance stuff
	(speedbar-clear-current-file)
	(setq speedbar-full-text-cache
	      (cons speedbar-shown-directories (buffer-string))
	      speedbar-shown-directories nil))
      ;; Now fill in the buffer with our newly found specialized list.
      (speedbar-with-writable
	  (dolist (func funclst)
	    ;; We do not erase the buffer because these functions may
	    ;; decide NOT to update themselves.
	    (funcall func specialbuff)))))
  (speedbar-reconfigure-keymaps))

(defun speedbar-set-timer (timeout)
  "Set up the speedbar timer with TIMEOUT.
Uses `dframe-set-timer'.
Also resets scanner functions."
  (dframe-set-timer timeout 'speedbar-timer-fn 'speedbar-update-flag)
  ;; Apply a revert hook that will reset the scanners.  We attach to revert
  ;; because most reverts occur during VC state change, and this lets our
  ;; VC scanner fix itself.
  (if timeout
      (add-hook 'after-revert-hook 'speedbar-reset-scanners)
    (remove-hook 'after-revert-hook 'speedbar-reset-scanners))
  ;; change this if it changed for some reason
  (speedbar-set-mode-line-format))

(defun speedbar-timer-fn ()
  "Run whenever Emacs is idle to update the speedbar item."
  (if (or (not (speedbar-current-frame))
	  (not (frame-live-p (speedbar-current-frame))))
      (speedbar-set-timer nil)
    ;; Save all the match data so that we don't mess up executing fns
    (save-match-data
      ;; Only do stuff if the frame is visible, not an icon, and if
      ;; it is currently flagged to do something.
      (if (and speedbar-update-flag
	       (speedbar-current-frame)
	       (frame-visible-p (speedbar-current-frame))
	       (not (eq (frame-visible-p (speedbar-current-frame)) 'icon)))
	  (let ((af (selected-frame)))
	      (dframe-select-attached-frame speedbar-frame)
	      ;; make sure we at least choose a window to
	      ;; get a good directory from
	      (if (window-minibuffer-p (selected-window))
		  nil
		;; Check for special modes
		(speedbar-maybe-add-localized-support (current-buffer))
		;; Update for special mode all the time!
		(if (and speedbar-mode-specific-contents-flag
			 (consp speedbar-special-mode-expansion-list)
			 (local-variable-p
			  'speedbar-special-mode-expansion-list
			  (current-buffer)))
		    ;;(eq (get major-mode 'mode-class 'special)))
		    (progn
		      (if (<= 2 speedbar-verbosity-level)
			  (speedbar-message
			   "Updating speedbar to special mode: %s..."
			   major-mode))
		      (speedbar-update-special-contents)
		      (if (<= 2 speedbar-verbosity-level)
			  (progn
			    (speedbar-message
			     "Updating speedbar to special mode: %s...done"
			     major-mode)
			    (speedbar-message nil))))

 		  ;; Update all the contents if directories change!
 		  (unless (and (or (member major-mode speedbar-ignored-modes)
				   (eq af (speedbar-current-frame))
				   (not (buffer-file-name)))
			       ;; Always update for GUD.
			       (not (string-equal "GUD"
				     speedbar-initial-expansion-list-name)))
		    (speedbar-update-localized-contents)))
		(select-frame af))
	    ;; Now run stealthy updates of time-consuming items
	    (speedbar-stealthy-updates)))))
  (run-hooks 'speedbar-timer-hook))


;;; Stealthy activities
;;
(defvar speedbar-stealthy-update-recurse nil
  "Recursion avoidance variable for stealthy update.")

(defun speedbar-stealthy-updates ()
  "For a given speedbar, run all items in the stealthy function list.
Each item returns t if it completes successfully, or nil if
interrupted by the user."
  (if (not speedbar-stealthy-update-recurse)
      (let ((l (speedbar-initial-stealthy-functions))
	    (speedbar-stealthy-update-recurse t))
	(unwind-protect
	    (speedbar-with-writable
	      (while (and l (funcall (car l)))
		;;(sit-for 0)
		(setq l (cdr l))))
	  ;;(speedbar-message "Exit with %S" (car l))
	  ))))

(defun speedbar-reset-scanners ()
  "Reset any variables used by functions in the stealthy list as state.
If new functions are added, their state needs to be updated here."
  (setq speedbar-vc-to-do-point t
	speedbar-obj-to-do-point t
	speedbar-ro-to-do-point t)
  (run-hooks 'speedbar-scanner-reset-hook)
  )

(defun speedbar-find-selected-file (file)
  "Go to the line where FILE is."

  (set-buffer speedbar-buffer)

  (goto-char (point-min))
  (let ((m nil))
    (while (and (setq m (re-search-forward
			 (concat " \\(" (regexp-quote (file-name-nondirectory file))
				 "\\)\\(" speedbar-indicator-regex "\\)?\n")
			 nil t))
		(not (string= file
			      (concat
			       (speedbar-line-directory
				(save-excursion
				  (goto-char (match-beginning 0))
				  (beginning-of-line)
				  (save-match-data
				    (looking-at "[0-9]+:")
				    (string-to-number (match-string 0)))))
			       (match-string 1))))))
    (if m
	(progn
	  (goto-char (match-beginning 1))
	  (match-string 1)))))

(defun speedbar-clear-current-file ()
  "Locate the file thought to be current, and remove its highlighting."
  (save-excursion
    ;;(set-buffer speedbar-buffer)
    (if speedbar-last-selected-file
	(speedbar-with-writable
	  (if (speedbar-find-selected-file speedbar-last-selected-file)
	      (put-text-property (match-beginning 1)
				 (match-end 1)
				 'face
				 'speedbar-file-face))))))

(defun speedbar-update-current-file ()
  "Find the current file, and update our visuals to indicate its name.
This is specific to file names.  If the file name doesn't show up, but
it should be in the list, then the directory cache needs to be updated."
  (let* ((lastf (selected-frame))
	 (newcfd (save-excursion
		   (dframe-select-attached-frame speedbar-frame)
		   (let ((rf (if (buffer-file-name)
				 (buffer-file-name)
			       nil)))
		     (select-frame lastf)
		     rf)))
	 (newcf (if newcfd newcfd))
	 (lastb (current-buffer))
	 (sucf-recursive (boundp 'sucf-recursive))
	 (case-fold-search t))
    (if (and newcf
	     ;; check here, that way we won't refresh to newcf until
	     ;; its been written, thus saving ourselves some time
	     (file-exists-p newcf)
	     (not (string= newcf speedbar-last-selected-file)))
	(progn
	  ;; It is important to select the frame, otherwise the window
	  ;; we want the cursor to move in will not be updated by the
	  ;; search-forward command.
	  (select-frame (speedbar-current-frame))
	  ;; Remove the old file...
	  (speedbar-clear-current-file)
	  ;; now highlight the new one.
	  ;; (set-buffer speedbar-buffer)
	  (speedbar-with-writable
	    (if (speedbar-find-selected-file newcf)
		;; put the property on it
		(put-text-property (match-beginning 1)
				   (match-end 1)
				   'face
				   'speedbar-selected-face)
	      ;; Oops, it's not in the list.  Should it be?
	      (if (and (string-match speedbar-file-regexp newcf)
		       (string= (file-name-directory newcfd)
				(expand-file-name default-directory)))
		  ;; yes, it is (we will ignore unknowns for now...)
		  (progn
		    (speedbar-refresh)
		    (if (speedbar-find-selected-file newcf)
			;; put the property on it
			(put-text-property (match-beginning 1)
					   (match-end 1)
					   'face
					   'speedbar-selected-face)))
		;; if it's not in there now, whatever...
		))
	    (setq speedbar-last-selected-file newcf))
	  (if (not sucf-recursive)
	      (progn

                ;;Sat Dec 15 2001 12:40 AM (burton@openprivacy.org): this
                ;;doesn't need to be in.  We don't want to recenter when we are
                ;;updating files.

                ;;(speedbar-center-buffer-smartly)

		(speedbar-position-cursor-on-line)
		))
	  (set-buffer lastb)
	  (select-frame lastf)
	  )))
  ;; return that we are done with this activity.
  t)

(defun speedbar-add-indicator (indicator-string &optional replace-this)
  "Add INDICATOR-STRING to the end of this speedbar line.
If INDICATOR-STRING is space, and REPLACE-THIS is a character,
then the existing indicator is removed.  If there is already an
indicator, then do not add a space."
  (beginning-of-line)
  ;; The nature of the beast: Assume we are in "the right place"
  (end-of-line)
  (skip-chars-backward (concat " " speedbar-vc-indicator
			       speedbar-object-read-only-indicator
			       (car speedbar-obj-indicator)
			       (cdr speedbar-obj-indicator)))
  (if (and (not (looking-at speedbar-indicator-regex))
	   (not (string= indicator-string " ")))
      (insert speedbar-indicator-separator))
  (speedbar-with-writable
    (save-excursion
      (if (and replace-this
	       (re-search-forward replace-this (line-end-position) t))
	  (delete-region (match-beginning 0) (match-end 0))))
    (end-of-line)
    (if (not (string= " " indicator-string))
	(let ((start (point)))
	  (insert indicator-string)
	  (speedbar-insert-image-button-maybe start (length indicator-string))
	  ))))

(defun speedbar-check-read-only ()
  "Scan all the files in a directory, and for each see if it is read only."
  ;; Check for to-do to be reset.  If reset but no RCS is available
  ;; then set to nil (do nothing) otherwise, start at the beginning
  (save-excursion
    (if speedbar-buffer (set-buffer speedbar-buffer))
    (if (eq speedbar-ro-to-do-point t)
	(setq speedbar-ro-to-do-point 0))
    (if (numberp speedbar-ro-to-do-point)
	(progn
	  (goto-char speedbar-ro-to-do-point)
	  (while (and (not (input-pending-p))
		      (re-search-forward "^\\([0-9]+\\):\\s-*[[<][+-\?][]>] "
					 nil t))
	    (setq speedbar-ro-to-do-point (point))
	    (let ((f (speedbar-line-file)))
	      (if f
		  (if (not (file-writable-p f))
		      (speedbar-add-indicator
		       speedbar-object-read-only-indicator
		       (regexp-quote speedbar-object-read-only-indicator))
		    (speedbar-add-indicator
		     " " (regexp-quote
			  speedbar-object-read-only-indicator))))))
	  (if (input-pending-p)
	      ;; return that we are incomplete
	      nil
	    ;; we are done, set to-do to nil
	    (setq speedbar-ro-to-do-point nil)
	    ;; and return t
	    t))
      t)))

(defun speedbar-check-vc ()
  "Scan all files in a directory, and for each see if it's checked out.
See `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p' for how
to add more types of version control systems."
  ;; Check for to-do to be reset.  If reset but no RCS is available
  ;; then set to nil (do nothing) otherwise, start at the beginning
  (save-excursion
    (if speedbar-buffer (set-buffer speedbar-buffer))
    (if (and speedbar-vc-do-check (eq speedbar-vc-to-do-point t)
	     (speedbar-vc-check-dir-p default-directory)
	     (not (or (and (featurep 'ange-ftp)
			   (string-match
			    (car (symbol-value
				  (if (featurep 'xemacs)
				      'ange-ftp-directory-format
				    'ange-ftp-name-format)))
			    (expand-file-name default-directory)))
		      ;; efs support: Bob Weiner
		      (and (featurep 'efs)
			   (string-match
			    (let ((reg (symbol-value 'efs-directory-regexp)))
			      (if (stringp reg)
				  reg
				(car reg)))
			    (expand-file-name default-directory))))))
	(setq speedbar-vc-to-do-point 0))
    (if (numberp speedbar-vc-to-do-point)
	(progn
	  (goto-char speedbar-vc-to-do-point)
	  (while (and (not (input-pending-p))
		      (re-search-forward "^\\([0-9]+\\):\\s-*\\[[+-?]\\] "
					 nil t))
	    (setq speedbar-vc-to-do-point (point))
	    (if (speedbar-check-vc-this-line (match-string 1))
		(speedbar-add-indicator speedbar-vc-indicator
					(regexp-quote speedbar-vc-indicator))
	      (speedbar-add-indicator " "
				      (regexp-quote speedbar-vc-indicator))))
	  (if (input-pending-p)
	      ;; return that we are incomplete
	      nil
	    ;; we are done, set to-do to nil
	    (setq speedbar-vc-to-do-point nil)
	    ;; and return t
	    t))
      t)))

(defun speedbar-check-vc-this-line (depth)
  "Return t if the file on this line is checked out of a version control system.
Parameter DEPTH is a string with the current depth of indentation of
the file being checked."
  (let* ((d (string-to-number depth))
	 (f (speedbar-line-directory d))
	 (fn (buffer-substring-no-properties
	      ;; Skip-chars: thanks ptype@dra.hmg.gb
	      (point) (progn
			(skip-chars-forward "^ " (line-end-position))
			(point))))
	 (fulln (concat f fn)))
    (if (<= 2 speedbar-verbosity-level)
	(speedbar-message "Speedbar vc check...%s" fulln))
    (and (file-writable-p fulln)
	 (speedbar-this-file-in-vc f fn))))

(defun speedbar-vc-check-dir-p (directory)
  "Return t if we should bother checking DIRECTORY for version control files.
This can be overloaded to add new types of version control systems."
  (or
   (catch t (dolist (vcd vc-directory-exclusion-list)
	      (if (file-exists-p (concat directory vcd)) (throw t t))) nil)
   ;; User extension
   (run-hook-with-args-until-success 'speedbar-vc-directory-enable-hook
                                     directory)
   ))

(defun speedbar-this-file-in-vc (directory name)
  "Check to see if the file in DIRECTORY with NAME is in a version control system.
Automatically recognizes all VCs supported by VC mode.  You can
optimize this function by overriding it and only doing those checks
that will occur on your system."
  (or
   (vc-backend (concat directory "/" name))
   ;; User extension
   (run-hook-with-args 'speedbar-vc-in-control-hook directory name)
   ))

;; Objet File scanning
(defun speedbar-check-objects ()
  "Scan all files in a directory, and for each see if there is an object.
See `speedbar-check-obj-this-line' and `speedbar-obj-alist' for how
to add more object types."
  ;; Check for to-do to be reset.  If reset but no RCS is available
  ;; then set to nil (do nothing) otherwise, start at the beginning
  (save-excursion
    (if speedbar-buffer (set-buffer speedbar-buffer))
    (if (and speedbar-obj-do-check (eq speedbar-obj-to-do-point t))
	(setq speedbar-obj-to-do-point 0))
    (if (numberp speedbar-obj-to-do-point)
	(progn
	  (goto-char speedbar-obj-to-do-point)
	  (while (and (not (input-pending-p))
		      (re-search-forward "^\\([0-9]+\\):\\s-*\\[[+-]\\] "
					 nil t))
	    (setq speedbar-obj-to-do-point (point))
	    (let ((ind (speedbar-check-obj-this-line (match-string 1))))
	      (if (not ind) (setq ind " "))
	      (speedbar-add-indicator ind (concat
					   (car speedbar-obj-indicator)
					   "\\|"
					   (cdr speedbar-obj-indicator)))))
	  (if (input-pending-p)
	      ;; return that we are incomplete
	      nil
	    ;; we are done, set to-do to nil
	    (setq speedbar-obj-to-do-point nil)
	    ;; and return t
	    t))
      t)))

(defun speedbar-check-obj-this-line (depth)
  "Return t if the file on this line has an associated object.
Parameter DEPTH is a string with the current depth of indentation of
the file being checked."
  (let* ((d (string-to-number depth))
	 (f (speedbar-line-directory d))
	 (fn (buffer-substring-no-properties
	      ;; Skip-chars: thanks ptype@dra.hmg.gb
	      (point) (progn
			(skip-chars-forward "^ " (line-end-position))
			(point))))
	 (fulln (concat f fn)))
    (if (<= 2 speedbar-verbosity-level)
	(speedbar-message "Speedbar obj check...%s" fulln))
    (let ((oa speedbar-obj-alist))
      (while (and oa (not (string-match (car (car oa)) fulln)))
	(setq oa (cdr oa)))
      (if (not (and oa (file-exists-p (concat (file-name-sans-extension fulln)
					      (cdr (car oa))))))
	  nil
	;; Find out if the object is out of date or not.
	(let ((date1 (nth 5 (file-attributes fulln)))
	      (date2 (nth 5 (file-attributes (concat
					      (file-name-sans-extension fulln)
                                              (cdr (car oa)))))))
	  (if (or (< (car date1) (car date2))
		  (and (= (car date1) (car date2))
		       (< (nth 1 date1) (nth 1 date2))))
	      (car speedbar-obj-indicator)
	    (cdr speedbar-obj-indicator)))))))

;;; Clicking Activity
;;
(defun speedbar-position-cursor-on-line ()
  "Position the cursor on a line."
  (let ((oldpos (point)))
    (beginning-of-line)
    (if (looking-at "[0-9]+:\\s-*..?.? ")
	(goto-char (1- (match-end 0)))
      (goto-char oldpos))))

(defun speedbar-click (e)
  "Activate any speedbar buttons where the mouse is clicked.
This must be bound to a mouse event.  A button is any location of text
with a mouse face that has a text property called `speedbar-function'.
Argument E is the click event."
  ;; Backward compatibility let statement.
  (let ((speedbar-power-click dframe-power-click))
    (speedbar-do-function-pointer))
  (dframe-quick-mouse e))

(defun speedbar-do-function-pointer ()
  "Look under the cursor and examine the text properties.
From this extract the file/tag name, token, indentation level and call
a function if appropriate."
  (let* ((speedbar-frame (speedbar-current-frame))
	 (fn (get-text-property (point) 'speedbar-function))
	 (tok (get-text-property (point) 'speedbar-token))
	 ;; The 1-,+ is safe because scanning starts AFTER the point
	 ;; specified.  This lets the search include the character the
	 ;; cursor is on.
	 (tp (previous-single-property-change
	      (1+ (point)) 'speedbar-function))
	 (np (next-single-property-change
	      (point) 'speedbar-function))
	 (txt (buffer-substring-no-properties (or tp (point-min))
					      (or np (point-max))))
	 (dent (save-excursion (beginning-of-line)
			       (string-to-number
				(if (looking-at "[0-9]+")
				    (buffer-substring-no-properties
				    (match-beginning 0) (match-end 0))
				  "0")))))
    ;;(speedbar-message "%S:%S:%S:%s" fn tok txt dent)
    (and fn (funcall fn txt tok dent)))
  (speedbar-position-cursor-on-line))

;;; Reading info from the speedbar buffer
;;
(defun speedbar-line-text (&optional p)
  "Retrieve the text after prefix junk for the current line.
Optional argument P is where to start the search from."
  (save-excursion
    (if p (goto-char p))
    (beginning-of-line)
    (if (looking-at (concat
		     "\\([0-9]+\\): *[[<{]?[-+?= ][]>}@()|] \\([^ \n]+\\)"))
	(get-text-property (match-beginning 2) 'speedbar-text)
      nil)))

(defun speedbar-line-token (&optional p)
  "Retrieve the token information after the prefix junk for the current line.
Optional argument P is where to start the search from."
  (save-excursion
    (if p (goto-char p))
    (beginning-of-line)
    (if (looking-at (concat
		     "\\([0-9]+\\): *[[<{]?[-+?= ][]>}@()|] \\([^ \n]+\\)\\("
		     speedbar-indicator-regex "\\)?"))
	(progn
	  (goto-char (match-beginning 2))
	  (get-text-property (point) 'speedbar-token))
      nil)))

(defun speedbar-line-file (&optional p)
  "Retrieve the file or whatever from the line at point P.
The return value is a string representing the file.  If it is a
directory, then it is the directory name."
  (save-match-data
    (save-restriction
      (widen)
      (let ((f (speedbar-line-text p)))
	(if f
	    (let* ((depth (string-to-number (match-string 1)))
		   (directory (speedbar-line-directory depth)))
	      (if (file-exists-p (concat directory f))
		  (concat directory f)
		nil))
	  nil)))))

(defun speedbar-goto-this-file (file)
  "If FILE is displayed, go to this line and return t.
Otherwise do not move and return nil."
  (let ((directory (substring (file-name-directory (expand-file-name file))
			 (length (expand-file-name default-directory))))
	(dest (point)))
    (save-match-data
      (goto-char (point-min))
      ;; scan all the directories
      (while (and directory (not (eq directory t)))
	(if (string-match "^[/\\]?\\([^/\\]+\\)" directory)
	    (let ((pp (match-string 1 directory)))
	      (if (save-match-data
		    (re-search-forward (concat "> " (regexp-quote pp) "$")
				       nil t))
		  (setq directory (substring directory (match-end 1)))
		(setq directory nil)))
	  (setq directory t)))
      ;; find the file part
      (if (or (not directory) (string= (file-name-nondirectory file) ""))
	  ;; only had a dir part
	  (if directory
	      (progn
		(speedbar-position-cursor-on-line)
		t)
	    (goto-char dest) nil)
	;; find the file part
	(let ((nd (file-name-nondirectory file)))
	  (if (re-search-forward
	       (concat "] \\(" (regexp-quote nd)
		       "\\)\\(" speedbar-indicator-regex "\\)$")
	       nil t)
	      (progn
		(speedbar-position-cursor-on-line)
		t)
	    (goto-char dest)
	    nil))))))

(defun speedbar-line-directory (&optional depth)
  "Retrieve the directory name associated with the current line.
This may require traversing backwards from DEPTH and combining the default
directory with these items.  This function is replaceable in
`speedbar-mode-functions-list' as `speedbar-line-directory'."
  (save-restriction
    (widen)
    (let ((rf (speedbar-fetch-replacement-function 'speedbar-line-directory)))
      (if rf (funcall rf depth) default-directory))))

(defun speedbar-files-line-directory (&optional depth)
  "Retrieve the directory associated with the current line.
This may require traversing backwards from DEPTH and combining the default
directory with these items."
  (save-excursion
    (save-match-data
      (if (not depth)
	  (progn
	    (beginning-of-line)
	    (looking-at "^\\([0-9]+\\):")
	    (setq depth (string-to-number (match-string 1)))))
      (let ((directory nil))
	(setq depth (1- depth))
	(while (/= depth -1)
	  (if (not (re-search-backward (format "^%d:" depth) nil t))
	      (error "Error building filename of tag")
	    (cond ((looking-at "[0-9]+:\\s-*<->\\s-+\\([^\n]+\\)")
		   (setq directory (concat (speedbar-line-text)
				      "/"
				      directory)))
		  ((looking-at "[0-9]+:\\s-*[-]\\s-+\\([^\n]+\\)")
		   ;; This is the start of our directory.
		   (setq directory (speedbar-line-text)))))
	  (setq depth (1- depth)))
	(if (and directory
		 (string-match (concat speedbar-indicator-regex "$")
			       directory))
	    (setq directory (substring directory 0 (match-beginning 0))))
	(concat default-directory directory)))))

(defun speedbar-directory-line (directory)
  "Position the cursor on the line specified by DIRECTORY."
  (save-match-data
    (if (string-match "[/\\]$" directory)
	(setq directory (substring directory 0 (match-beginning 0))))
    (let ((nomatch t) (depth 0)
	  (fname (file-name-nondirectory directory))
	  (pname (file-name-directory directory)))
      (if (not (member pname speedbar-shown-directories))
	  (error "Internal Error: File %s not shown in speedbar" directory))
      (goto-char (point-min))
      (while (and nomatch
		  (re-search-forward
		   (concat "[]>] \\(" (regexp-quote fname)
			   "\\)\\(" speedbar-indicator-regex "\\)?$")
		   nil t))
	(beginning-of-line)
	(looking-at "\\([0-9]+\\):")
	(setq depth (string-to-number (match-string 0))
	      nomatch (not (string= pname (speedbar-line-directory depth))))
	(end-of-line))
      (beginning-of-line)
      (not nomatch))))

(defun speedbar-edit-line ()
  "Edit whatever tag or file is on the current speedbar line."
  (interactive)
  (or (save-excursion
	(beginning-of-line)
	;; If this fails, then it is a non-standard click, and as such,
	;; perfectly allowed.
	(if (re-search-forward "[]>?}] [^ ]"
			       (line-end-position)
			       t)
	    (progn
	      (forward-char -1)
	      (speedbar-do-function-pointer))
	  nil))
      (speedbar-do-function-pointer)))

(defun speedbar-expand-line (&optional arg)
  "Expand the line under the cursor.
With universal argument ARG, flush cached data."
  (interactive "P")
  (beginning-of-line)
  (let* ((dframe-power-click arg)
	 (speedbar-power-click arg))
    (condition-case nil
	(progn
	  (re-search-forward ":\\s-*.\\+. "
			     (line-end-position))
	  (forward-char -2)
	  (speedbar-do-function-pointer))
      (error (speedbar-position-cursor-on-line)))))

(defun speedbar-flush-expand-line ()
  "Expand the line under the cursor and flush any cached information."
  (interactive)
  (speedbar-expand-line 1))

(defun speedbar-contract-line ()
  "Contract the line under the cursor."
  (interactive)
  (beginning-of-line)
  (condition-case nil
      (progn
	(re-search-forward ":\\s-*.-. "
			   (line-end-position))
	(forward-char -2)
	(speedbar-do-function-pointer))
    (error (speedbar-position-cursor-on-line))))

(defun speedbar-toggle-line-expansion ()
  "Contract or expand the line under the cursor."
  (interactive)
  (beginning-of-line)
  (condition-case nil
      (progn
	(re-search-forward ":\\s-*.[-+]. "
			   (line-end-position))
	(forward-char -2)
	(speedbar-do-function-pointer))
    (error (speedbar-position-cursor-on-line))))

(defun speedbar-expand-line-descendants (&optional arg)
  "Expand the line under the cursor and all descendants.
Optional argument ARG indicates that any cache should be flushed."
  (interactive "P")
  (speedbar-expand-line arg)
  ;; Now, inside the area expanded here, expand all subnodes of
  ;; the same descendant type.
  (save-excursion
    (speedbar-next 1) ;; Move into the list.
    (let ((err nil))
      (while (not err)
	(condition-case nil
	    (progn
	      (speedbar-expand-line-descendants arg)
	      (speedbar-restricted-next 1))
	  (error (setq err t))))))
  )

(defun speedbar-contract-line-descendants ()
  "Expand the line under the cursor and all descendants."
  (interactive)
  (speedbar-contract-line)
  ;; Don't need to do anything else since all descendants are
  ;; hidden by default anyway.  Yay!  It's easy.
  )

(defun speedbar-find-file (text _token indent)
  "Speedbar click handler for filenames.
TEXT, the file will be displayed in the attached frame.
TOKEN is unused, but required by the click handler.  INDENT is the
current indentation level."
  (let ((cdd (speedbar-line-directory indent)))
    ;; Run before visiting file hook here.
    (let ((f (selected-frame)))
      (dframe-select-attached-frame speedbar-frame)
      (run-hooks 'speedbar-before-visiting-file-hook)
      (select-frame f))
    (speedbar-find-file-in-frame (concat cdd text))
    (speedbar-stealthy-updates)
    (run-hooks 'speedbar-visiting-file-hook)
    ;; Reset the timer with a new timeout when clicking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer dframe-update-speed))
  (dframe-maybee-jump-to-attached-frame))

(defun speedbar-dir-follow (text _token indent)
  "Speedbar click handler for directory names.
Clicking a directory will cause the speedbar to list files in
the subdirectory TEXT.  TOKEN is an unused requirement.  The
subdirectory chosen will be at INDENT level."
  (setq default-directory
	(concat (expand-file-name (concat (speedbar-line-directory indent) text))
		"/"))
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.  Turn off smart expand just
  ;; in case.
  (let ((speedbar-smart-directory-expand-flag nil))
    (speedbar-update-contents))
  (speedbar-set-timer speedbar-navigating-speed)
  (setq speedbar-last-selected-file nil)
  (speedbar-stealthy-updates))

(defun speedbar-delete-subblock (indent)
  "Delete text from point to indentation level INDENT or greater.
Handles end-of-sublist smartly."
  (speedbar-with-writable
    (save-excursion
      (end-of-line) (forward-char 1)
      (let ((start (point)))
	(while (and (looking-at "^\\([0-9]+\\):")
		    (> (string-to-number (match-string 1)) indent)
		    (not (eobp)))
	  (forward-line 1)
	  (beginning-of-line))
	(delete-region start (point))))))

(defun speedbar-dired (text token indent)
  "Speedbar click handler for directory expand button.
Clicking this button expands or contracts a directory.  TEXT is the
button clicked which has either a + or -.  TOKEN is the directory to be
expanded.  INDENT is the current indentation level."
  (cond ((string-match "+" text)	;we have to expand this dir
	 (setq speedbar-shown-directories
	       (cons (expand-file-name
		      (concat (speedbar-line-directory indent) token "/"))
		     speedbar-shown-directories))
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-reset-scanners)
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   (speedbar-with-writable
	     (speedbar-default-directory-list
	      (concat (speedbar-line-directory indent) token "/")
	      (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-reset-scanners)
	 (let ((oldl speedbar-shown-directories)
	       (newl nil)
	       (td (expand-file-name
		    (concat (speedbar-line-directory indent) token))))
	   (while oldl
	     (if (not (string-match (concat "^" (regexp-quote td)) (car oldl)))
		 (setq newl (cons (car oldl) newl)))
	     (setq oldl (cdr oldl)))
	   (setq speedbar-shown-directories (nreverse newl)))
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent)
	 )
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly)
  (save-excursion (speedbar-stealthy-updates)))

(defun speedbar-directory-buttons-follow (_text token _indent)
  "Speedbar click handler for default directory buttons.
TEXT is the button clicked on.  TOKEN is the directory to follow.
INDENT is the current indentation level and is unused."
  (if (string-match "^[A-z]:$" token)
      (setq default-directory (concat token "/"))
    (setq default-directory token))
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.
  (speedbar-update-contents)
  (speedbar-set-timer speedbar-navigating-speed))

(defun speedbar-tag-file (text token indent)
  "The cursor is on a selected line.  Expand the tags in the specified file.
The parameter TEXT and TOKEN are required, where TEXT is the button
clicked, and TOKEN is the file to expand.  INDENT is the current
indentation level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (let* ((fn (expand-file-name (concat (speedbar-line-directory indent)
					      token)))
		(lst (speedbar-fetch-dynamic-tags fn)))
	   ;; if no list, then remove expando button
	   (if (not lst)
	       (speedbar-change-expand-button-char ??)
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (funcall (car lst) indent (cdr lst)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun speedbar-tag-find (_text token indent)
  "For the tag TEXT in a file TOKEN, go to that position.
INDENT is the current indentation level."
  (let ((file (speedbar-line-directory indent)))
    (let ((f (selected-frame)))
      (dframe-select-attached-frame speedbar-frame)
      (run-hooks 'speedbar-before-visiting-tag-hook)
      (select-frame f))
    (speedbar-find-file-in-frame file)
    (save-excursion (speedbar-stealthy-updates))
    ;; Reset the timer with a new timeout when clicking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer dframe-update-speed)
    (goto-char token)
    (run-hooks 'speedbar-visiting-tag-hook)
    (dframe-maybee-jump-to-attached-frame)
    ))

(defun speedbar-tag-expand (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button string.
TOKEN will be the list, and INDENT is the current indentation level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (speedbar-insert-generic-list indent token 'speedbar-tag-expand
					   'speedbar-tag-find))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

;;; Loading files into the attached frame.
;;
(defcustom speedbar-select-frame-method 'attached
  "Specify how to select a frame for displaying a file.
A value of 'attached means to use the attached frame (the frame
that speedbar was started from.)  A number such as 1 or -1 means to
pass that number to `other-frame' while selecting a frame from speedbar."
  :group 'speedbar
  :type 'sexp)

(defun speedbar-find-file-in-frame (file)
  "This will load FILE into the speedbar attached frame.
If the file is being displayed in a different frame already, then raise that
frame instead."
  (let* ((buff (find-file-noselect file))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if dframe-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(if (numberp speedbar-select-frame-method)
	    (other-frame speedbar-select-frame-method)
	  (dframe-select-attached-frame speedbar-frame))
	(switch-to-buffer buff))))
 )

;;; Centering Utility
;;
(defun speedbar-center-buffer-smartly ()
  "Recenter a speedbar buffer so the current indentation level is all visible.
This assumes that the cursor is on a file, or tag of a file which the user is
interested in."

  (save-selected-window

    (select-window (get-buffer-window speedbar-buffer t))

    (set-buffer speedbar-buffer)

    (if (<= (count-lines (point-min) (point-max))
	    (1- (window-height (selected-window))))
	;; whole buffer fits
	(let ((cp (point)))

	  (goto-char (point-min))
	  (recenter 0)
	  (goto-char cp))
      ;; too big
      (let (depth start end exp p)
	(save-excursion
	  (beginning-of-line)
	  (setq depth (if (looking-at "[0-9]+")
			  (string-to-number (buffer-substring-no-properties
					  (match-beginning 0) (match-end 0)))
			0))
	  (setq exp (format "^%d:" depth)))
	(save-excursion
	  (end-of-line)
	  (if (re-search-backward exp nil t)
	      (setq start (point))
	    (setq start (point-min)))
	  (save-excursion		;Not sure about this part.
	    (end-of-line)
	    (setq p (point))
	    (while (and (not (re-search-forward exp nil t))
			(>= depth 0))
	      (setq depth (1- depth))
	      (setq exp (format "^%d:" depth)))
	    (if (/= (point) p)
		(setq end (point))
	      (setq end (point-max)))))
	;; Now work out the details of centering
	(let ((nl (count-lines start end))
              (wl (1- (window-height (selected-window))))
	      (cp (point)))
	  (if (> nl wl)
	      ;; We can't fit it all, so just center on cursor
	      (progn (goto-char start)
		     (recenter 1))
	    ;; we can fit everything on the screen, but...
	    (if (and (pos-visible-in-window-p start (selected-window))
		     (pos-visible-in-window-p end (selected-window)))
		;; we are all set!
		nil
	      ;; we need to do something...
	      (goto-char start)
	      (let ((newcent (/ (- (window-height (selected-window)) nl) 2))
		    (lte (count-lines start (point-max))))
		(if (and (< (+ newcent lte) (window-height (selected-window)))
			 (> (- (window-height (selected-window)) lte 1)
			    newcent))
		    (setq newcent (- (window-height (selected-window))
				     lte 1)))
		(recenter newcent))))
          (goto-char cp))))))

;;; Tag Management -- List of expanders:
;;
(defun speedbar-fetch-dynamic-tags (file)
  "Return a list of tags generated dynamically from FILE.
This uses the entries in `speedbar-dynamic-tags-function-list'
to find the proper tags.  It is up to each of those individual
functions to do caching and flushing if appropriate."
  (save-excursion
    ;; If a file is in memory, switch to that buffer.  This allows
    ;; us to use the local variable.  If the file is on disk, we
    ;; can try a few of the defaults that can get tags without
    ;; opening the file.
    (if (get-file-buffer file)
	(set-buffer (get-file-buffer file)))
    ;; If there is a buffer-local value of
    ;; speedbar-dynamic-tags-function-list, it will now be available.
    (let ((dtf speedbar-dynamic-tags-function-list)
	  (ret t))
      (while (and (eq ret t) dtf)
	(setq ret
	      (if (fboundp (car (car dtf)))
		  (funcall (car (car dtf)) file)
		t))
	(if (eq ret t)
	    (setq dtf (cdr dtf))))
      (if (eq ret t)
	  ;; No valid tag list, return nil
	  nil
	;; We have some tags.  Return the list with the insert fn
	;; prepended
	(cons (cdr (car dtf)) ret)))))

;;; Tag Management -- Imenu
;;
(if (not speedbar-use-imenu-flag)

    nil

(eval-when-compile (condition-case nil (require 'imenu) (error nil)))

(defun speedbar-fetch-dynamic-imenu (file)
  "Load FILE into a buffer, and generate tags using Imenu.
Returns the tag list, or t for an error."
  ;; Load this AND compile it in
  (require 'imenu)
  (set-buffer (find-file-noselect file))
  (if dframe-power-click (setq imenu--index-alist nil))
  (condition-case nil
      (let ((index-alist (imenu--make-index-alist t)))
	(if speedbar-sort-tags
	    (sort (copy-alist index-alist)
		  (lambda (a b) (string< (car a) (car b))))
	  index-alist))
    (error t)))
)

;;; Tag Management -- etags  (old XEmacs compatibility part)
;;
(defvar speedbar-fetch-etags-parse-list
  '(;; Note that java has the same parse-group as c
    ("\\.\\([cChH]\\|c\\+\\+\\|cpp\\|cc\\|hh\\|java\\|cxx\\|hxx\\)\\'" .
     speedbar-parse-c-or-c++tag)
    ("^\\.emacs$\\|.\\(el\\|l\\|lsp\\)\\'" .
     "def[^i]+\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*\C-?")
;    ("\\.\\([fF]\\|for\\|FOR\\|77\\|90\\)\\'" .
;      speedbar-parse-fortran77-tag)
    ("\\.tex\\'" . speedbar-parse-tex-string)
    ("\\.p\\'" .
     "\\(\\(FUNCTION\\|function\\|PROCEDURE\\|procedure\\)\\s-+\\([a-zA-Z0-9_.:]+\\)\\)\\s-*(?^?")
    )
  "Associations of file extensions and expressions for extracting tags.
To add a new file type, you would want to add a new association to the
list, where the car is the file match, and the cdr is the way to
extract an element from the tags output.  If the output is complex,
use a function symbol instead of regexp.  The function should expect
to be at the beginning of a line in the etags buffer.

This variable is ignored if `speedbar-use-imenu-flag' is non-nil.")

(defcustom speedbar-fetch-etags-command "etags"
  "Command used to create an etags file.
This variable is ignored if `speedbar-use-imenu-flag' is t."
  :group 'speedbar
  :type 'string)

(defcustom speedbar-fetch-etags-arguments '("-D" "-I" "-o" "-")
  "List of arguments to use with `speedbar-fetch-etags-command'.
This creates an etags output buffer.  Use `speedbar-toggle-etags' to
modify this list conveniently.
This variable is ignored if `speedbar-use-imenu-flag' is t."
  :group 'speedbar
  :type '(choice (const nil)
		 (repeat :tag "List of arguments" string)))

(defun speedbar-toggle-etags (flag)
  "Toggle FLAG in `speedbar-fetch-etags-arguments'.
FLAG then becomes a member of etags command line arguments.  If flag
is \"sort\", then toggle the value of `speedbar-sort-tags'.  If its
value is \"show\" then toggle the value of
`speedbar-show-unknown-files'.

  This function is a convenience function for XEmacs menu created by
Farzin Guilak <farzin@protocol.com>."
  (interactive)
  (cond
   ((equal flag "sort")
    (setq speedbar-sort-tags (not speedbar-sort-tags)))
   ((equal flag "show")
    (setq speedbar-show-unknown-files (not speedbar-show-unknown-files)))
   ((or (equal flag "-C")
	(equal flag "-S")
	(equal flag "-D"))
    (if (member flag speedbar-fetch-etags-arguments)
	(setq speedbar-fetch-etags-arguments
	      (delete flag speedbar-fetch-etags-arguments))
      (add-to-list 'speedbar-fetch-etags-arguments flag)))
   (t nil)))

(defun speedbar-fetch-dynamic-etags (file)
  "For FILE, run etags and create a list of symbols extracted.
Each symbol will be associated with its line position in FILE."
  (let ((newlist nil))
    (unwind-protect
	(save-excursion
	  (if (get-buffer "*etags tmp*")
	      (kill-buffer "*etags tmp*"))	;kill to clean it up
	  (if (<= 1 speedbar-verbosity-level)
	      (speedbar-message "Fetching etags..."))
	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (apply 'call-process speedbar-fetch-etags-command nil
		 (current-buffer) nil
		 (append speedbar-fetch-etags-arguments (list file)))
	  (goto-char (point-min))
	  (if (<= 1 speedbar-verbosity-level)
	      (speedbar-message "Fetching etags..."))
	  (let ((expr
		 (let ((exprlst speedbar-fetch-etags-parse-list)
		       (ans nil))
		   (while (and (not ans) exprlst)
		     (if (string-match (car (car exprlst)) file)
			 (setq ans (car exprlst)))
		     (setq exprlst (cdr exprlst)))
		   (cdr ans))))
	    (if expr
		(let (tnl)
		  (set-buffer (get-buffer-create "*etags tmp*"))
		  (while (not (save-excursion (end-of-line) (eobp)))
		    (save-excursion
		      (setq tnl (speedbar-extract-one-symbol expr)))
		    (if tnl (setq newlist (cons tnl newlist)))
		    (forward-line 1)))
	      (speedbar-message
	       "Sorry, no support for a file of that extension"))))
      )
    (if speedbar-sort-tags
	(sort newlist (lambda (a b) (string< (car a) (car b))))
      (reverse newlist))))

;; This bit donated by Farzin Guilak <farzin@protocol.com> but I'm not
;; sure it's needed with the different sorting method.
;;
;(defun speedbar-clean-etags()
;  "Removes spaces before the ^? character, and removes `#define',
;return types, etc. preceding tags.  This ensures that the sort operation
;works on the tags, not the return types."
;  (save-excursion
;    (goto-char (point-min))
;    (while
;	(re-search-forward "(?[ \t](?\C-?" nil t)
;      (replace-match "\C-?" nil nil))
;    (goto-char (point-min))
;    (while
;	(re-search-forward "\\(.*[ \t]+\\)\\([^ \t\n]+.*\C-?\\)" nil t)
;      (delete-region (match-beginning 1) (match-end 1)))))

(defun speedbar-extract-one-symbol (expr)
  "At point, return nil, or one alist in the form (SYMBOL . POSITION).
The line should contain output from etags.  Parse the output using the
regular expression EXPR."
  (let* ((sym (if (stringp expr)
		  (if (save-excursion
			(re-search-forward expr (line-end-position) t))
		      (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
		(funcall expr)))
	 (pos (let ((j (re-search-forward "[\C-?\C-a]\\([0-9]+\\),\\([0-9]+\\)"
					  (line-end-position) t)))
		(if (and j sym)
		    (1+ (string-to-number (buffer-substring-no-properties
					(match-beginning 2)
					(match-end 2))))
		  0))))
    (if (/= pos 0)
	(cons sym pos)
      nil)))

(defun speedbar-parse-c-or-c++tag ()
  "Parse a C or C++ tag, which tends to be a little complex."
  (save-excursion
    (let ((bound (line-end-position)))
      (cond ((re-search-forward "\C-?\\([^\C-a]+\\)\C-a" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    ((re-search-forward "\\<\\([^ \t]+\\)\\s-+new(" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    ((re-search-forward "\\<\\([^ \t(]+\\)\\s-*(\C-?" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    (t nil))
      )))

(defun speedbar-parse-tex-string ()
  "Parse a Tex string.  Only find data which is relevant."
  (save-excursion
    (let ((bound (line-end-position)))
      (cond ((re-search-forward "\\(\\(sub\\)*section\\|chapter\\|cite\\)\\s-*{[^\C-?}]*}?" bound t)
	     (buffer-substring-no-properties (match-beginning 0)
					     (match-end 0)))
	    (t nil)))))


;;; BUFFER DISPLAY mode.
;;
(defvar speedbar-buffers-key-map nil
  "Keymap used when in the buffers display mode.")

(if speedbar-buffers-key-map
    nil
  (setq speedbar-buffers-key-map (speedbar-make-specialized-keymap))

  ;; Basic tree features
  (define-key speedbar-buffers-key-map "e" 'speedbar-edit-line)
  (define-key speedbar-buffers-key-map "\C-m" 'speedbar-edit-line)
  (define-key speedbar-buffers-key-map "+" 'speedbar-expand-line)
  (define-key speedbar-buffers-key-map "=" 'speedbar-expand-line)
  (define-key speedbar-buffers-key-map "-" 'speedbar-contract-line)
  (define-key speedbar-buffers-key-map " " 'speedbar-toggle-line-expansion)

  ;; Buffer specific keybindings
  (define-key speedbar-buffers-key-map "k" 'speedbar-buffer-kill-buffer)
  (define-key speedbar-buffers-key-map "r" 'speedbar-buffer-revert-buffer)

  )

(defvar speedbar-buffer-easymenu-definition
  '(["Jump to buffer" speedbar-edit-line t]
    ["Expand File Tags" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Flush Cache & Expand" speedbar-flush-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract File Tags" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    "----"
    ["Kill Buffer" speedbar-buffer-kill-buffer
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.[-+?]. "))]
    ["Revert Buffer" speedbar-buffer-revert-buffer
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.[-+?]. "))]
    )
  "Menu item elements shown when displaying a buffer list.")

(defun speedbar-buffer-buttons (_directory _zero)
  "Create speedbar buttons based on the buffers currently loaded.
DIRECTORY is the directory of the currently active buffer, and ZERO is 0."
  (speedbar-buffer-buttons-engine nil))

(defun speedbar-buffer-buttons-temp (_directory _zero)
  "Create speedbar buttons based on the buffers currently loaded.
DIRECTORY is the directory of the currently active buffer, and ZERO is 0."
  (speedbar-buffer-buttons-engine t))

(defun speedbar-buffer-buttons-engine (temp)
  "Create speedbar buffer buttons.
If TEMP is non-nil, then clicking on a buffer restores the previous display."
  (speedbar-insert-separator "Active Buffers:")
  (let ((bl (buffer-list))
	(case-fold-search t))
    (while bl
      (if (string-match "^[ *]" (buffer-name (car bl)))
	  nil
	(let* ((known (string-match speedbar-file-regexp
				    (buffer-name (car bl))))
	       (expchar (if known ?+ ??))
	       (fn (if known 'speedbar-tag-file nil))
	       (fname (with-current-buffer (car bl)
                        (buffer-file-name))))
	  (speedbar-make-tag-line 'bracket expchar fn
				  (if fname (file-name-nondirectory fname))
				  (buffer-name (car bl))
				  'speedbar-buffer-click temp
				  'speedbar-file-face 0)
	  (speedbar-buffers-tail-notes (car bl))))
      (setq bl (cdr bl)))
    (setq bl (buffer-list))
    (speedbar-insert-separator "Scratch Buffers:")
    (while bl
      (if (not (string-match "^\\*" (buffer-name (car bl))))
	  nil
	(if (eq (car bl) speedbar-buffer)
	    nil
	  (speedbar-make-tag-line 'bracket ?? nil nil
				  (buffer-name (car bl))
				  'speedbar-buffer-click temp
				  'speedbar-file-face 0)
	  (speedbar-buffers-tail-notes (car bl))))
      (setq bl (cdr bl)))
    (setq bl (buffer-list))
    ;;(speedbar-insert-separator "Hidden Buffers:")
    ;;(while bl
    ;;  (if (not (string-match "^ " (buffer-name (car bl))))
    ;;	  nil
    ;;	(if (eq (car bl) speedbar-buffer)
    ;;	    nil
    ;;	  (speedbar-make-tag-line 'bracket ?? nil nil
    ;;				  (buffer-name (car bl))
    ;;				  'speedbar-buffer-click temp
    ;;				  'speedbar-file-face 0)
    ;;	  (speedbar-buffers-tail-notes (car bl))))
    ;;  (setq bl (cdr bl)))
    ))

(defun speedbar-buffers-tail-notes (buffer)
  "Add a note to the end of the last tag line.
Argument BUFFER is the buffer being tested."
  (when (with-current-buffer buffer buffer-read-only)
    (speedbar-insert-button "%" nil nil nil nil t)))

(defun speedbar-buffers-item-info ()
  "Display information about the current buffer on the current line."
  (or (speedbar-item-info-tag-helper)
      (let* ((item (speedbar-line-text))
	     (buffer (if item (get-buffer item) nil)))
	(and buffer
	     (speedbar-message "%s%s %S %d %s"
			       (if (buffer-modified-p buffer) "* " "")
			       item
			       (with-current-buffer buffer major-mode)
			       (with-current-buffer buffer (buffer-size))
			       (or (buffer-file-name buffer) "<No file>"))))))

(defun speedbar-buffers-line-directory (&optional _depth)
  "Fetch the directory of the file (buffer) specified on the current line.
Optional argument DEPTH specifies the current depth of the back search."
  (save-excursion
    (end-of-line)
    (let ((start (point)))
      ;; Buffers are always at level 0
      (if (not (re-search-backward "^0:" nil t))
	  nil
	(let* ((bn (speedbar-line-text))
	       (buffer (if bn (get-buffer bn))))
	  (if buffer
	      (if (eq start (line-end-position))
		  (or (with-current-buffer buffer default-directory)
		      "")
		(buffer-file-name buffer))))))))

(defun speedbar-buffer-click (text token _indent)
  "When the users clicks on a buffer-button in speedbar.
TEXT is the buffer's name, TOKEN and INDENT are unused."
  (if dframe-power-click
      (let ((pop-up-frames t)) (select-window (display-buffer text)))
    (dframe-select-attached-frame speedbar-frame)
    (switch-to-buffer text)
    (if token (speedbar-change-initial-expansion-list
	       speedbar-previously-used-expansion-list-name))))

(defun speedbar-buffer-kill-buffer ()
  "Kill the buffer the cursor is on in the speedbar buffer."
  (interactive)
  (or (save-excursion
	(let ((text (speedbar-line-text)))
	  (if (and (get-buffer text)
		   (speedbar-y-or-n-p (format "Kill buffer %s? " text)))
	      (kill-buffer text))
	  (speedbar-refresh)))))

(defun speedbar-buffer-revert-buffer ()
  "Revert the buffer the cursor is on in the speedbar buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; If this fails, then it is a non-standard click, and as such,
    ;; perfectly allowed
    (if (re-search-forward "[]>?}] [^ ]" (line-end-position) t)
	(let ((text (progn
		      (forward-char -1)
		      (buffer-substring (point) (line-end-position)))))
	  (if (get-buffer text)
	      (progn
		(set-buffer text)
		(revert-buffer t)))))))


;;; Useful hook values and such.
;;
(defvar speedbar-highlight-one-tag-line nil
  "Overlay used for highlighting the most recently jumped to tag line.")

(defun speedbar-highlight-one-tag-line ()
  "Highlight the current line, unhighlighting a previously jumped to line."
  (speedbar-unhighlight-one-tag-line)
  (setq speedbar-highlight-one-tag-line
	(speedbar-make-overlay (line-beginning-position)
			       (1+ (line-end-position))))
  (speedbar-overlay-put speedbar-highlight-one-tag-line 'face
			'speedbar-highlight-face)
  (add-hook 'pre-command-hook 'speedbar-unhighlight-one-tag-line))

(defun speedbar-unhighlight-one-tag-line ()
  "Unhighlight the currently highlighted line."
  (when (and speedbar-highlight-one-tag-line
	     (not (eq this-command 'handle-switch-frame)))
    (speedbar-delete-overlay speedbar-highlight-one-tag-line)
    (setq speedbar-highlight-one-tag-line nil)
    (remove-hook 'pre-command-hook 'speedbar-unhighlight-one-tag-line)))

(defun speedbar-recenter-to-top ()
  "Recenter the current buffer so point is on the top of the window."
  (recenter 1))

(defun speedbar-recenter ()
  "Recenter the current buffer so point is in the center of the window."
  (recenter (/ (window-height (selected-window)) 2)))


;;; Color loading section.
;;
(defface speedbar-button-face '((((class color) (background light))
				 (:foreground "green4"))
				(((class color) (background dark))
				 (:foreground "green3")))
  "Face used for +/- buttons."
  :group 'speedbar-faces)

(defface speedbar-file-face '((((class color) (background light))
			       (:foreground "cyan4"))
			      (((class color) (background dark))
			       (:foreground "cyan"))
			      (t (:bold t)))
  "Face used for file names."
  :group 'speedbar-faces)

(defface speedbar-directory-face '((((class color) (background light))
				    (:foreground "blue4"))
				   (((class color) (background dark))
				    (:foreground "light blue")))
  "Face used for directory names."
  :group 'speedbar-faces)
(defface speedbar-tag-face '((((class color) (background light))
			      (:foreground "brown"))
			     (((class color) (background dark))
			      (:foreground "yellow")))
  "Face used for displaying tags."
  :group 'speedbar-faces)

(defface speedbar-selected-face '((((class color) (background light))
				    (:foreground "red" :underline t))
				  (((class color) (background dark))
				   (:foreground "red" :underline t))
				  (t (:underline t)))
  "Face used to underline the file in the active window."
  :group 'speedbar-faces)

(defface speedbar-highlight-face '((((class color) (background light))
				    (:background "green"))
				   (((class color) (background dark))
				    (:background "sea green"))
				   (((class grayscale monochrome)
				     (background light))
				    (:background "black"))
				   (((class grayscale monochrome)
				     (background dark))
				    (:background "white")))
  "Face used for highlighting buttons with the mouse."
  :group 'speedbar-faces)

(defface speedbar-separator-face '((((class color) (background light))
				    (:background "blue"
				     :foreground "white"
				     :overline "gray"))
				   (((class color) (background dark))
				    (:background "blue"
				     :foreground "white"
				     :overline "gray"))
				   (((class grayscale monochrome)
				     (background light))
				    (:background "black"
				     :foreground "white"
				     :overline "white"))
				   (((class grayscale monochrome)
				     (background dark))
				    (:background "white"
				     :foreground "black"
				     :overline "black")))
  "Face used for separator labels in a display."
  :group 'speedbar-faces)

;; some edebug hooks
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec speedbar-with-writable def-body)))

;; Fix a font lock problem for some versions of Emacs
(and (boundp 'font-lock-global-modes)
     font-lock-global-modes
     (if (eq font-lock-global-modes t)
	 (setq font-lock-global-modes '(not speedbar-mode))
       (if (eq (car font-lock-global-modes) 'not)
	   (add-to-list 'font-lock-global-modes 'speedbar-mode t)
	 (setq font-lock-global-modes (delq 'speedbar-mode
					    font-lock-global-modes)))))

;;; Obsolete variables and functions

(define-obsolete-variable-alias
  'speedbar-ignored-path-regexp 'speedbar-ignored-directory-regexp "22.1")

(define-obsolete-function-alias 'speedbar-add-ignored-path-regexp
  'speedbar-add-ignored-directory-regexp "22.1")

(define-obsolete-function-alias 'speedbar-line-path
  'speedbar-line-directory "22.1")

(define-obsolete-function-alias 'speedbar-buffers-line-path
  'speedbar-buffers-line-directory "22.1")

(define-obsolete-function-alias 'speedbar-path-line
  'speedbar-directory-line "22.1")

(define-obsolete-function-alias 'speedbar-buffers-line-path
  'speedbar-buffers-line-directory "22.1")

(provide 'speedbar)

;; run load-time hooks
(run-hooks 'speedbar-load-hook)

;;; speedbar ends here
