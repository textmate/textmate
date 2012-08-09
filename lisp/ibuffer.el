;;; ibuffer.el --- operate on buffers like dired

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: John Paul Wallington <jpw@gnu.org>
;; Created: 8 Sep 2000
;; Keywords: buffer, convenience

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

;; ibuffer.el is an advanced replacement for the `buffer-menu' which
;; is normally distributed with Emacs.  Its interface is intended to
;; be analogous to that of Dired.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ibuf-macs)
  (require 'dired))

(require 'font-core)

;; These come from ibuf-ext.el, which can not be require'd at compile time
;; because it has a recursive dependency on ibuffer.el
(defvar ibuffer-auto-mode)
(defvar ibuffer-cached-filter-formats)
(defvar ibuffer-compiled-filter-formats)
(defvar ibuffer-filter-format-alist)
(defvar ibuffer-filter-group-kill-ring)
(defvar ibuffer-filter-groups)
(defvar ibuffer-filtering-qualifiers)
(defvar ibuffer-header-line-format)
(defvar ibuffer-hidden-filter-groups)
(defvar ibuffer-inline-columns)
(defvar ibuffer-show-empty-filter-groups)
(defvar ibuffer-tmp-hide-regexps)
(defvar ibuffer-tmp-show-regexps)

(declare-function ibuffer-mark-on-buffer "ibuf-ext"
		  (func &optional ibuffer-mark-on-buffer-mark group))
(declare-function ibuffer-generate-filter-groups "ibuf-ext"
		  (bmarklist &optional noempty nodefault))
(declare-function ibuffer-format-filter-group-data "ibuf-ext" (filter))

(defgroup ibuffer nil
  "An advanced replacement for `buffer-menu'.

Ibuffer allows you to operate on buffers in a manner much like Dired.
Operations include sorting, marking by regular expression, and
the ability to filter the displayed buffers by various criteria."
  :version "22.1"
  :group 'convenience)

(defcustom ibuffer-formats '((mark modified read-only " " (name 18 18 :left :elide)
				   " " (size 9 -1 :right)
				   " " (mode 16 16 :left :elide) " " filename-and-process)
			     (mark " " (name 16 -1) " " filename))
  "A list of ways to display buffer lines.

With Ibuffer, you are not limited to displaying just certain
attributes of a buffer such as size, name, and mode in a particular
order.  Through this variable, you can completely customize and
control the appearance of an Ibuffer buffer.  See also
`define-ibuffer-column', which allows you to define your own columns
for display.

This variable has the form
 ((COLUMN COLUMN ...) (COLUMN COLUMN ...) ...)
Each element in `ibuffer-formats' should be a list containing COLUMN
specifiers.  A COLUMN can be any of the following:

  SYMBOL - A symbol naming the column.  Predefined columns are:
       mark modified read-only name size mode process filename
   When you define your own columns using `define-ibuffer-column', just
   use their name like the predefined columns here.  This entry can
   also be a function of two arguments, which should return a string.
   The first argument is the buffer object, and the second is the mark
   on that buffer.
 or
  \"STRING\" - A literal string to display.
 or
  (SYMBOL MIN-SIZE MAX-SIZE &optional ALIGN ELIDE) - SYMBOL is a
   symbol naming the column, and MIN-SIZE and MAX-SIZE are integers (or
   functions of no arguments returning an integer) which constrict the
   size of a column.  If MAX-SIZE is -1, there is no upper bound.  The
   default values are 0 and -1, respectively.  If MIN-SIZE is negative,
   use the end of the string.  The optional element ALIGN describes the
   alignment of the column; it can be :left, :center or :right.  The
   optional element ELIDE describes whether or not to elide the column
   if it is too long; valid values are :elide and nil.  The default is
   nil (don't elide).

Some example of valid entries in `ibuffer-formats', with
description (also, feel free to try them out, and experiment with your
own!):

 (mark \" \" name)
  This format just displays the current mark (if any) and the name of
  the buffer, separated by a space.
 (mark modified read-only \" \" (name 16 16 :left) \" \" (size 6 -1 :right))
  This format displays the current mark (if any), its modification and
  read-only status, as well as the name of the buffer and its size.  In
  this format, the name is restricted to 16 characters (longer names
  will be truncated, and shorter names will be padded with spaces), and
  the name is also aligned to the left.  The size of the buffer will
  be padded with spaces up to a minimum of six characters, but there is
  no upper limit on its size.  The size will also be aligned to the
  right.

Thus, if you wanted to use these two formats, add

 (setq ibuffer-formats '((mark \" \" name)
		         (mark modified read-only
			  (name 16 16 :left) (size 6 -1 :right))))

to your ~/.emacs file.

Using \\[ibuffer-switch-format], you can rotate the display between
the specified formats in the list."
  :type '(repeat sexp)
  :group 'ibuffer)

(defcustom ibuffer-always-compile-formats (featurep 'bytecomp)
  "If non-nil, then use the byte-compiler to optimize `ibuffer-formats'.
This will increase the redisplay speed, at the cost of loading the
elisp byte-compiler."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-fontification-alist
  `((10 buffer-read-only font-lock-constant-face)
    (15 (and buffer-file-name
	     (string-match ibuffer-compressed-file-name-regexp
			   buffer-file-name))
	font-lock-doc-face)
    (20 (string-match "^*" (buffer-name)) font-lock-keyword-face)
    (25 (and (string-match "^ " (buffer-name))
	     (null buffer-file-name))
	italic)
    (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
    (35 (eq major-mode 'dired-mode) font-lock-function-name-face))
  "An alist describing how to fontify buffers.
Each element should be of the form (PRIORITY FORM FACE), where
PRIORITY is an integer, FORM is an arbitrary form to evaluate in the
buffer, and FACE is the face to use for fontification.  If the FORM
evaluates to non-nil, then FACE will be put on the buffer name.  The
element with the highest PRIORITY takes precedence.

If you change this variable, you must kill the Ibuffer buffer and
recreate it for the change to take effect."
  :type '(repeat
	  (list (integer :tag "Priority")
		(sexp :tag "Test Form")
		face))
  :group 'ibuffer)

(defcustom ibuffer-use-other-window nil
  "If non-nil, display Ibuffer in another window by default."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-default-shrink-to-minimum-size nil
  "If non-nil, minimize the size of the Ibuffer window by default."
  :type 'boolean
  :group 'ibuffer)
(defvar ibuffer-shrink-to-minimum-size nil)

(defcustom ibuffer-display-summary t
  "If non-nil, summarize Ibuffer columns."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-truncate-lines t
  "If non-nil, do not display continuation lines."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-case-fold-search case-fold-search
  "If non-nil, ignore case when searching."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-default-sorting-mode 'recency
  "The criteria by which to sort the buffers.

Note that this variable is local to each Ibuffer buffer.  Thus, you
can have multiple Ibuffer buffers open, each with a different sorted
view of the buffers."
  :type '(choice (const :tag "Last view time" :value recency)
		 (const :tag "Lexicographic" :value alphabetic)
		 (const :tag "Buffer size" :value size)
		 (const :tag "File name" :value filename/process)
		 (const :tag "Major mode" :value major-mode))
  :group 'ibuffer)
(defvar ibuffer-sorting-mode nil)
(defvar ibuffer-last-sorting-mode nil)

(defcustom ibuffer-default-sorting-reversep nil
  "If non-nil, reverse the default sorting order."
  :type 'boolean
  :group 'ibuffer)
(defvar ibuffer-sorting-reversep nil)

(defcustom ibuffer-elide-long-columns nil
  "If non-nil, then elide column entries which exceed their max length."
  :type 'boolean
  :group 'ibuffer)
(make-obsolete-variable 'ibuffer-elide-long-columns
                        "use the :elide argument of `ibuffer-formats'."
                        "22.1")

(defcustom ibuffer-eliding-string "..."
  "The string to use for eliding long columns."
  :type 'string
  :group 'ibuffer)

(defcustom ibuffer-maybe-show-predicates `(,(lambda (buf)
					      (and (string-match "^ " (buffer-name buf))
						   (null buffer-file-name))))
  "A list of predicates for buffers to display conditionally.

A predicate can be a regexp or a function.
If a regexp, then it will be matched against the buffer's name.
If a function, it will be called with the buffer as an argument, and
should return non-nil if this buffer should be shown.

Viewing of buffers hidden because of these predicates may be customized
via `ibuffer-default-display-maybe-show-predicates' and is toggled by
giving a non-nil prefix argument to `ibuffer-update'.
Note that this specialized filtering occurs before real filtering."
  :type '(repeat (choice regexp function))
  :group 'ibuffer)

(defcustom ibuffer-default-display-maybe-show-predicates nil
  "Non-nil means show buffers that match `ibuffer-maybe-show-predicates'."
  :type 'boolean
  :group 'ibuffer)

(defvar ibuffer-display-maybe-show-predicates nil)

(defvar ibuffer-current-format nil)

(defcustom ibuffer-movement-cycle t
  "If non-nil, then forward and backwards movement commands cycle."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-modified-char ?*
  "The character to display for modified buffers."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-read-only-char ?%
  "The character to display for read-only buffers."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-marked-char ?>
  "The character to display for marked buffers."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-deletion-char ?D
  "The character to display for buffers marked for deletion."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-expert nil
  "If non-nil, don't ask for confirmation of \"dangerous\" operations."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-view-ibuffer nil
  "If non-nil, display the current Ibuffer buffer itself.
Note that this has a drawback - the data about the current Ibuffer
buffer will most likely be inaccurate.  This includes modification
state, size, etc."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-always-show-last-buffer nil
  "If non-nil, always display the previous buffer.
This variable takes precedence over filtering, and even
`ibuffer-never-show-predicates'."
  :type '(choice (const :tag "Always" :value t)
		 (const :tag "Never" :value nil)
		 (const :tag "Always except minibuffer" :value :nomini))
  :group 'ibuffer)

(defcustom ibuffer-jump-offer-only-visible-buffers nil
  "If non-nil, only offer buffers visible in the Ibuffer buffer
in completion lists of the `ibuffer-jump-to-buffer' command."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-use-header-line (boundp 'header-line-format)
  "If non-nil, display a header line containing current filters."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-default-directory nil
  "The default directory to use for a new Ibuffer buffer.
If nil, inherit the directory of the buffer in which `ibuffer' was
called.  Otherwise, this variable should be a string naming a
directory, like `default-directory'."
  :type '(choice (const :tag "Inherit" :value nil)
		 string)
  :group 'ibuffer)

(defcustom ibuffer-help-buffer-modes
  '(help-mode apropos-mode Info-mode Info-edit-mode)
  "List of \"Help\" major modes."
  :type '(repeat function)
  :group 'ibuffer)

(defcustom ibuffer-compressed-file-name-regexp
  "\\.\\(arj\\|bgz\\|bz2\\|gz\\|lzh\\|taz\\|tgz\\|xz\\|zip\\|z\\)$"
  "Regexp to match compressed file names."
  :version "24.1"                       ; added xz
  :type 'regexp
  :group 'ibuffer)

(define-obsolete-variable-alias 'ibuffer-hooks 'ibuffer-hook "22.1")

(defcustom ibuffer-hook nil
  "Hook run when `ibuffer' is called."
  :type 'hook
  :group 'ibuffer)

(define-obsolete-variable-alias 'ibuffer-mode-hooks 'ibuffer-mode-hook "22.1")

(defcustom ibuffer-mode-hook nil
  "Hook run upon entry into `ibuffer-mode'."
  :type 'hook
  :group 'ibuffer)

(defcustom ibuffer-load-hook nil
  "Hook run when Ibuffer is loaded."
  :type 'hook
  :group 'ibuffer)

(defcustom ibuffer-marked-face 'warning
  "Face used for displaying marked buffers."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-deletion-face 'error
  "Face used for displaying buffers marked for deletion."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-title-face 'font-lock-type-face
  "Face used for the title string."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-filter-group-name-face 'bold
  "Face used for displaying filtering group names."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-directory-abbrev-alist nil
  "An alist of file name abbreviations like `directory-abbrev-alist'."
  :type '(repeat (cons :format "%v"
		       :value ("" . "")
		       (regexp :tag "From")
		       (regexp :tag "To")))
  :group 'ibuffer)

(defvar ibuffer-mode-groups-popup
  (let ((groups-map (make-sparse-keymap "Filter Groups")))
    ;; Filter groups

    (define-key-after groups-map [filters-to-filter-group]
      '(menu-item "Create filter group from current filters..."
        ibuffer-filters-to-filter-group
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers)))
    (define-key-after groups-map [forward-filter-group]
      '(menu-item "Move point to the next filter group"
        ibuffer-forward-filter-group))
    (define-key-after groups-map [backward-filter-group]
      '(menu-item "Move point to the previous filter group"
        ibuffer-backward-filter-group))
    (define-key-after groups-map [jump-to-filter-group]
      '(menu-item "Move point to a specific filter group..."
        ibuffer-jump-to-filter-group))
    (define-key-after groups-map [kill-filter-group]
      '(menu-item "Kill filter group named..."
        ibuffer-kill-filter-group
        :enable (and (featurep 'ibuf-ext) ibuffer-filter-groups)))
    (define-key-after groups-map [yank-filter-group]
      '(menu-item "Yank last killed filter group before..."
        ibuffer-yank-filter-group
        :enable (and (featurep 'ibuf-ext) ibuffer-filter-group-kill-ring)))
    (define-key-after groups-map [pop-filter-group]
      '(menu-item "Remove top filter group"
        ibuffer-pop-filter-group
        :enable (and (featurep 'ibuf-ext) ibuffer-filter-groups)))
    (define-key-after groups-map [clear-filter-groups]
      '(menu-item "Remove all filter groups"
        ibuffer-clear-filter-groups
        :enable (and (featurep 'ibuf-ext) ibuffer-filter-groups)))
    (define-key-after groups-map [pop-filter-group]
      '(menu-item "Decompose filter group..."
        ibuffer-pop-filter-group
        :help "\"Unmake\" a filter group"
        :enable (and (featurep 'ibuf-ext) ibuffer-filter-groups)))
    (define-key-after groups-map [save-filter-groups]
      '(menu-item "Save current filter groups permanently..."
        ibuffer-save-filter-groups
        :enable (and (featurep 'ibuf-ext) ibuffer-filter-groups)
        :help "Use a mnemonic name to store current filter groups"))
    (define-key-after groups-map [switch-to-saved-filter-groups]
      '(menu-item "Restore permanently saved filters..."
        ibuffer-switch-to-saved-filter-groups
        :enable (and (featurep 'ibuf-ext) ibuffer-saved-filter-groups)
        :help "Replace current filters with a saved stack"))
    (define-key-after groups-map [delete-saved-filter-groups]
      '(menu-item "Delete permanently saved filter groups..."
        ibuffer-delete-saved-filter-groups
        :enable (and (featurep 'ibuf-ext) ibuffer-saved-filter-groups)))
    (define-key-after groups-map [set-filter-groups-by-mode]
      '(menu-item "Set current filter groups to filter by mode"
        ibuffer-set-filter-groups-by-mode))

    groups-map))

(defvar ibuffer-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "0") 'digit-argument)
    (define-key map (kbd "1") 'digit-argument)
    (define-key map (kbd "2") 'digit-argument)
    (define-key map (kbd "3") 'digit-argument)
    (define-key map (kbd "4") 'digit-argument)
    (define-key map (kbd "5") 'digit-argument)
    (define-key map (kbd "6") 'digit-argument)
    (define-key map (kbd "7") 'digit-argument)
    (define-key map (kbd "8") 'digit-argument)
    (define-key map (kbd "9") 'digit-argument)

    (define-key map (kbd "m") 'ibuffer-mark-forward)
    (define-key map (kbd "t") 'ibuffer-toggle-marks)
    (define-key map (kbd "u") 'ibuffer-unmark-forward)
    (define-key map (kbd "=") 'ibuffer-diff-with-file)
    (define-key map (kbd "j") 'ibuffer-jump-to-buffer)
    (define-key map (kbd "M-g") 'ibuffer-jump-to-buffer)
    (define-key map (kbd "M-s a C-s") 'ibuffer-do-isearch)
    (define-key map (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp)
    (define-key map (kbd "DEL") 'ibuffer-unmark-backward)
    (define-key map (kbd "M-DEL") 'ibuffer-unmark-all)
    (define-key map (kbd "* *") 'ibuffer-unmark-all)
    (define-key map (kbd "* M") 'ibuffer-mark-by-mode)
    (define-key map (kbd "* m") 'ibuffer-mark-modified-buffers)
    (define-key map (kbd "* u") 'ibuffer-mark-unsaved-buffers)
    (define-key map (kbd "* s") 'ibuffer-mark-special-buffers)
    (define-key map (kbd "* r") 'ibuffer-mark-read-only-buffers)
    (define-key map (kbd "* /") 'ibuffer-mark-dired-buffers)
    (define-key map (kbd "* e") 'ibuffer-mark-dissociated-buffers)
    (define-key map (kbd "* h") 'ibuffer-mark-help-buffers)
    (define-key map (kbd "* z") 'ibuffer-mark-compressed-file-buffers)
    (define-key map (kbd ".") 'ibuffer-mark-old-buffers)

    (define-key map (kbd "d") 'ibuffer-mark-for-delete)
    (define-key map (kbd "C-d") 'ibuffer-mark-for-delete-backwards)
    (define-key map (kbd "k") 'ibuffer-mark-for-delete)
    (define-key map (kbd "x") 'ibuffer-do-kill-on-deletion-marks)

    ;; immediate operations
    (define-key map (kbd "n") 'ibuffer-forward-line)
    (define-key map (kbd "SPC") 'forward-line)
    (define-key map (kbd "p") 'ibuffer-backward-line)
    (define-key map (kbd "M-}") 'ibuffer-forward-next-marked)
    (define-key map (kbd "M-{") 'ibuffer-backwards-next-marked)
    (define-key map (kbd "l") 'ibuffer-redisplay)
    (define-key map (kbd "g") 'ibuffer-update)
    (define-key map "`" 'ibuffer-switch-format)
    (define-key map "-" 'ibuffer-add-to-tmp-hide)
    (define-key map "+" 'ibuffer-add-to-tmp-show)
    (define-key map "b" 'ibuffer-bury-buffer)
    (define-key map (kbd ",") 'ibuffer-toggle-sorting-mode)
    (define-key map (kbd "s i") 'ibuffer-invert-sorting)
    (define-key map (kbd "s a") 'ibuffer-do-sort-by-alphabetic)
    (define-key map (kbd "s v") 'ibuffer-do-sort-by-recency)
    (define-key map (kbd "s s") 'ibuffer-do-sort-by-size)
    (define-key map (kbd "s f") 'ibuffer-do-sort-by-filename/process)
    (define-key map (kbd "s m") 'ibuffer-do-sort-by-major-mode)

    (define-key map (kbd "/ m") 'ibuffer-filter-by-mode)
    (define-key map (kbd "/ M") 'ibuffer-filter-by-used-mode)
    (define-key map (kbd "/ n") 'ibuffer-filter-by-name)
    (define-key map (kbd "/ c") 'ibuffer-filter-by-content)
    (define-key map (kbd "/ e") 'ibuffer-filter-by-predicate)
    (define-key map (kbd "/ f") 'ibuffer-filter-by-filename)
    (define-key map (kbd "/ >") 'ibuffer-filter-by-size-gt)
    (define-key map (kbd "/ <") 'ibuffer-filter-by-size-lt)
    (define-key map (kbd "/ r") 'ibuffer-switch-to-saved-filters)
    (define-key map (kbd "/ a") 'ibuffer-add-saved-filters)
    (define-key map (kbd "/ x") 'ibuffer-delete-saved-filters)
    (define-key map (kbd "/ d") 'ibuffer-decompose-filter)
    (define-key map (kbd "/ s") 'ibuffer-save-filters)
    (define-key map (kbd "/ p") 'ibuffer-pop-filter)
    (define-key map (kbd "/ !") 'ibuffer-negate-filter)
    (define-key map (kbd "/ t") 'ibuffer-exchange-filters)
    (define-key map (kbd "/ TAB") 'ibuffer-exchange-filters)
    (define-key map (kbd "/ o") 'ibuffer-or-filter)
    (define-key map (kbd "/ g") 'ibuffer-filters-to-filter-group)
    (define-key map (kbd "/ P") 'ibuffer-pop-filter-group)
    (define-key map (kbd "/ D") 'ibuffer-decompose-filter-group)
    (define-key map (kbd "/ /") 'ibuffer-filter-disable)

    (define-key map (kbd "M-n") 'ibuffer-forward-filter-group)
    (define-key map "\t" 'ibuffer-forward-filter-group)
    (define-key map (kbd "M-p") 'ibuffer-backward-filter-group)
    (define-key map [backtab] 'ibuffer-backward-filter-group)
    (define-key map (kbd "M-j") 'ibuffer-jump-to-filter-group)
    (define-key map (kbd "C-k") 'ibuffer-kill-line)
    (define-key map (kbd "C-y") 'ibuffer-yank)
    (define-key map (kbd "/ S") 'ibuffer-save-filter-groups)
    (define-key map (kbd "/ R") 'ibuffer-switch-to-saved-filter-groups)
    (define-key map (kbd "/ X") 'ibuffer-delete-saved-filter-groups)
    (define-key map (kbd "/ \\") 'ibuffer-clear-filter-groups)

    (define-key map (kbd "q") 'ibuffer-quit)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "?") 'describe-mode)

    (define-key map (kbd "% n") 'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "% m") 'ibuffer-mark-by-mode-regexp)
    (define-key map (kbd "% f") 'ibuffer-mark-by-file-name-regexp)

    (define-key map (kbd "C-t") 'ibuffer-visit-tags-table)

    (define-key map (kbd "|") 'ibuffer-do-shell-command-pipe)
    (define-key map (kbd "!") 'ibuffer-do-shell-command-file)
    (define-key map (kbd "~") 'ibuffer-do-toggle-modified)
    ;; marked operations
    (define-key map (kbd "A") 'ibuffer-do-view)
    (define-key map (kbd "D") 'ibuffer-do-delete)
    (define-key map (kbd "E") 'ibuffer-do-eval)
    (define-key map (kbd "F") 'ibuffer-do-shell-command-file)
    (define-key map (kbd "I") 'ibuffer-do-query-replace-regexp)
    (define-key map (kbd "H") 'ibuffer-do-view-other-frame)
    (define-key map (kbd "N") 'ibuffer-do-shell-command-pipe-replace)
    (define-key map (kbd "M") 'ibuffer-do-toggle-modified)
    (define-key map (kbd "O") 'ibuffer-do-occur)
    (define-key map (kbd "P") 'ibuffer-do-print)
    (define-key map (kbd "Q") 'ibuffer-do-query-replace)
    (define-key map (kbd "R") 'ibuffer-do-rename-uniquely)
    (define-key map (kbd "S") 'ibuffer-do-save)
    (define-key map (kbd "T") 'ibuffer-do-toggle-read-only)
    (define-key map (kbd "U") 'ibuffer-do-replace-regexp)
    (define-key map (kbd "V") 'ibuffer-do-revert)
    (define-key map (kbd "W") 'ibuffer-do-view-and-eval)
    (define-key map (kbd "X") 'ibuffer-do-shell-command-pipe)

    (define-key map (kbd "k") 'ibuffer-do-kill-lines)
    (define-key map (kbd "w") 'ibuffer-copy-filename-as-kill)

    (define-key map (kbd "RET") 'ibuffer-visit-buffer)
    (define-key map (kbd "e") 'ibuffer-visit-buffer)
    (define-key map (kbd "f") 'ibuffer-visit-buffer)
    (define-key map (kbd "C-x C-f") 'ibuffer-find-file)
    (define-key map (kbd "o") 'ibuffer-visit-buffer-other-window)
    (define-key map (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect)
    (define-key map (kbd "M-o") 'ibuffer-visit-buffer-1-window)
    (define-key map (kbd "v") 'ibuffer-do-view)
    (define-key map (kbd "C-x v") 'ibuffer-do-view-horizontally)
    (define-key map (kbd "C-c C-a") 'ibuffer-auto-mode)
    (define-key map (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window)
    (define-key map (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame)

    (define-key map [menu-bar view]
      (cons "View" (make-sparse-keymap "View")))

    (define-key-after map [menu-bar view visit-buffer]
      '(menu-item "View this buffer" ibuffer-visit-buffer))
    (define-key-after map [menu-bar view visit-buffer-other-window]
      '(menu-item "View (other window)" ibuffer-visit-buffer-other-window))
    (define-key-after map [menu-bar view visit-buffer-other-frame]
      '(menu-item "View (other frame)" ibuffer-visit-buffer-other-frame))
    (define-key-after map [menu-bar view ibuffer-update]
      '(menu-item "Update" ibuffer-update
        :help "Regenerate the list of buffers"))
    (define-key-after map [menu-bar view switch-format]
      '(menu-item "Switch display format" ibuffer-switch-format
        :help "Toggle between available values of `ibuffer-formats'"))

    (define-key-after map [menu-bar view dashes]
      '("--"))

    (define-key-after map [menu-bar view sort]
      (cons "Sort" (make-sparse-keymap "Sort")))

    (define-key-after map [menu-bar view sort do-sort-by-major-mode]
      '(menu-item "Sort by major mode" ibuffer-do-sort-by-major-mode))
    (define-key-after map [menu-bar view sort do-sort-by-size]
      '(menu-item "Sort by buffer size" ibuffer-do-sort-by-size))
    (define-key-after map [menu-bar view sort do-sort-by-alphabetic]
      '(menu-item "Sort lexicographically" ibuffer-do-sort-by-alphabetic
        :help "Sort by the alphabetic order of buffer name"))
    (define-key-after map [menu-bar view sort do-sort-by-recency]
      '(menu-item "Sort by view time" ibuffer-do-sort-by-recency
        :help "Sort by the last time the buffer was displayed"))
    (define-key-after map [menu-bar view sort dashes]
      '("--"))
    (define-key-after map [menu-bar view sort invert-sorting]
      '(menu-item "Reverse sorting order" ibuffer-invert-sorting))
    (define-key-after map [menu-bar view sort toggle-sorting-mode]
      '(menu-item "Switch sorting mode" ibuffer-toggle-sorting-mode
        :help "Switch between the various sorting criteria"))

    (define-key-after map [menu-bar view filter]
      (cons "Filter" (make-sparse-keymap "Filter")))

    (define-key-after map [menu-bar view filter filter-disable]
      '(menu-item "Disable all filtering" ibuffer-filter-disable
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers)))
    (define-key-after map [menu-bar view filter filter-by-mode]
      '(menu-item "Add filter by major mode..." ibuffer-filter-by-mode))
    (define-key-after map [menu-bar view filter filter-by-mode]
      '(menu-item "Add filter by major mode in use..."
        ibuffer-filter-by-used-mode))
    (define-key-after map [menu-bar view filter filter-by-name]
      '(menu-item "Add filter by buffer name..." ibuffer-filter-by-name))
    (define-key-after map [menu-bar view filter filter-by-filename]
      '(menu-item "Add filter by filename..." ibuffer-filter-by-filename))
    (define-key-after map [menu-bar view filter filter-by-size-lt]
      '(menu-item "Add filter by size less than..." ibuffer-filter-by-size-lt))
    (define-key-after map [menu-bar view filter filter-by-size-gt]
      '(menu-item "Add filter by size greater than..."
        ibuffer-filter-by-size-gt))
    (define-key-after map [menu-bar view filter filter-by-content]
      '(menu-item "Add filter by content (regexp)..."
        ibuffer-filter-by-content))
    (define-key-after map [menu-bar view filter filter-by-predicate]
      '(menu-item "Add filter by Lisp predicate..."
        ibuffer-filter-by-predicate))
    (define-key-after map [menu-bar view filter pop-filter]
      '(menu-item "Remove top filter" ibuffer-pop-filter
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers)))
    (define-key-after map [menu-bar view filter or-filter]
      '(menu-item "OR top two filters" ibuffer-or-filter
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers
                     (cdr ibuffer-filtering-qualifiers))
        :help
        "Create a new filter which is the logical OR of the top two filters"))
    (define-key-after map [menu-bar view filter negate-filter]
      '(menu-item "Negate top filter" ibuffer-negate-filter
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers)))
    (define-key-after map [menu-bar view filter decompose-filter]
      '(menu-item "Decompose top filter" ibuffer-decompose-filter
        :enable (and (featurep 'ibuf-ext)
                     (memq (car ibuffer-filtering-qualifiers) '(or saved not)))
        :help "Break down a complex filter like OR or NOT"))
    (define-key-after map [menu-bar view filter exchange-filters]
      '(menu-item "Swap top two filters" ibuffer-exchange-filters
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers
                     (cdr ibuffer-filtering-qualifiers))))
    (define-key-after map [menu-bar view filter save-filters]
      '(menu-item "Save current filters permanently..." ibuffer-save-filters
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers)
        :help "Use a mnemonic name to store current filter stack"))
    (define-key-after map [menu-bar view filter switch-to-saved-filters]
      '(menu-item "Restore permanently saved filters..."
        ibuffer-switch-to-saved-filters
        :enable (and (featurep 'ibuf-ext) ibuffer-saved-filters)
        :help "Replace current filters with a saved stack"))
    (define-key-after map [menu-bar view filter add-saved-filters]
      '(menu-item "Add to permanently saved filters..."
        ibuffer-add-saved-filters
        :enable (and (featurep 'ibuf-ext) ibuffer-filtering-qualifiers)
        :help "Include already saved stack with current filters"))
    (define-key-after map [menu-bar view filter delete-saved-filters]
      '(menu-item "Delete permanently saved filters..."
        ibuffer-delete-saved-filters
        :enable (and (featurep 'ibuf-ext) ibuffer-saved-filters)))

    (define-key-after map [menu-bar view filter-groups]
      (cons "Filter Groups" ibuffer-mode-groups-popup))

    (define-key-after map [menu-bar view dashes2]
      '("--"))
    (define-key-after map [menu-bar view diff-with-file]
      '(menu-item "Diff with file" ibuffer-diff-with-file
        :help "View the differences between this buffer and its file"))
    (define-key-after map [menu-bar view auto-mode]
      '(menu-item "Toggle Auto Mode" ibuffer-auto-mode
        :help "Attempt to automatically update the Ibuffer buffer"))
    (define-key-after map [menu-bar view customize]
      '(menu-item "Customize Ibuffer" ibuffer-customize
        :help "Use Custom to customize Ibuffer"))

    (define-key-after map [menu-bar mark]
      (cons "Mark" (make-sparse-keymap "Mark")))

    (define-key-after map [menu-bar mark toggle-marks]
      '(menu-item "Toggle marks" ibuffer-toggle-marks
        :help "Unmark marked buffers, and mark unmarked buffers"))
    (define-key-after map [menu-bar mark mark-forward]
      '(menu-item "Mark" ibuffer-mark-forward
        :help "Mark the buffer at point"))
    (define-key-after map [menu-bar mark unmark-forward]
      '(menu-item "Unmark" ibuffer-unmark-forward
        :help "Unmark the buffer at point"))
    (define-key-after map [menu-bar mark mark-by-mode]
      '(menu-item "Mark by mode..." ibuffer-mark-by-mode
        :help "Mark all buffers in a particular major mode"))
    (define-key-after map [menu-bar mark mark-modified-buffers]
      '(menu-item "Mark modified buffers" ibuffer-mark-modified-buffers
        :help "Mark all buffers which have been modified"))
    (define-key-after map [menu-bar mark mark-unsaved-buffers]
      '(menu-item "Mark unsaved buffers" ibuffer-mark-unsaved-buffers
        :help "Mark all buffers which have a file and are modified"))
    (define-key-after map [menu-bar mark mark-read-only-buffers]
      '(menu-item "Mark read-only buffers" ibuffer-mark-read-only-buffers
        :help "Mark all buffers which are read-only"))
    (define-key-after map [menu-bar mark mark-special-buffers]
      '(menu-item "Mark special buffers" ibuffer-mark-special-buffers
        :help "Mark all buffers whose name begins with a *"))
    (define-key-after map [menu-bar mark mark-dired-buffers]
      '(menu-item "Mark dired buffers" ibuffer-mark-dired-buffers
        :help "Mark buffers in dired-mode"))
    (define-key-after map [menu-bar mark mark-dissociated-buffers]
      '(menu-item "Mark dissociated buffers" ibuffer-mark-dissociated-buffers
        :help "Mark buffers with a non-existent associated file"))
    (define-key-after map [menu-bar mark mark-help-buffers]
      '(menu-item "Mark help buffers" ibuffer-mark-help-buffers
        :help "Mark buffers in help-mode"))
    (define-key-after map [menu-bar mark mark-compressed-file-buffers]
      '(menu-item "Mark compressed file buffers"
        ibuffer-mark-compressed-file-buffers
        :help "Mark buffers which have a file that is compressed"))
    (define-key-after map [menu-bar mark mark-old-buffers]
      '(menu-item "Mark old buffers" ibuffer-mark-old-buffers
        :help "Mark buffers which have not been viewed recently"))
    (define-key-after map [menu-bar mark unmark-all]
      '(menu-item "Unmark All" ibuffer-unmark-all))

    (define-key-after map [menu-bar mark dashes]
      '("--"))

    (define-key-after map [menu-bar mark mark-by-name-regexp]
      '(menu-item "Mark by buffer name (regexp)..." ibuffer-mark-by-name-regexp
        :help "Mark buffers whose name matches a regexp"))
    (define-key-after map [menu-bar mark mark-by-mode-regexp]
      '(menu-item "Mark by major mode (regexp)..." ibuffer-mark-by-mode-regexp
        :help "Mark buffers whose major mode name matches a regexp"))
    (define-key-after map [menu-bar mark mark-by-file-name-regexp]
      '(menu-item "Mark by file name (regexp)..."
        ibuffer-mark-by-file-name-regexp
        :help "Mark buffers whose file name matches a regexp"))

    map))

(defvar ibuffer-mode-operate-map
  (let ((operate-map (make-sparse-keymap "Operate")))
    (define-key-after operate-map [do-view]
      '(menu-item "View" ibuffer-do-view))
    (define-key-after operate-map [do-view-other-frame]
      '(menu-item "View (separate frame)" ibuffer-do-view-other-frame))
    (define-key-after operate-map [do-save]
      '(menu-item "Save" ibuffer-do-save))
    (define-key-after operate-map [do-replace-regexp]
      '(menu-item "Replace (regexp)..." ibuffer-do-replace-regexp
        :help "Replace text inside marked buffers"))
    (define-key-after operate-map [do-query-replace]
      '(menu-item "Query Replace..." ibuffer-do-query-replace
        :help "Replace text in marked buffers, asking each time"))
    (define-key-after operate-map [do-query-replace-regexp]
      '(menu-item "Query Replace (regexp)..." ibuffer-do-query-replace-regexp
        :help "Replace text in marked buffers by regexp, asking each time"))
    (define-key-after operate-map [do-print]
      '(menu-item "Print" ibuffer-do-print))
    (define-key-after operate-map [do-toggle-modified]
      '(menu-item "Toggle modification flag" ibuffer-do-toggle-modified))
    (define-key-after operate-map [do-revert]
      '(menu-item "Revert" ibuffer-do-revert
        :help "Revert marked buffers to their associated file"))
    (define-key-after operate-map [do-rename-uniquely]
      '(menu-item "Rename Uniquely" ibuffer-do-rename-uniquely
        :help "Rename marked buffers to a new, unique name"))
    (define-key-after operate-map [do-delete]
      '(menu-item "Kill" ibuffer-do-delete))
    (define-key-after operate-map [do-occur]
      '(menu-item "List lines matching..." ibuffer-do-occur
        :help "View all lines in marked buffers matching a regexp"))
    (define-key-after operate-map [do-shell-command-pipe]
      '(menu-item "Pipe to shell command..." ibuffer-do-shell-command-pipe
        :help "For each marked buffer, send its contents to a shell command"))
    (define-key-after operate-map [do-shell-command-pipe-replace]
      '(menu-item "Pipe to shell command (replace)..." ibuffer-do-shell-command-pipe-replace
        :help "For each marked buffer, replace its contents with output of shell command"))
    (define-key-after operate-map [do-shell-command-file]
      '(menu-item "Shell command on buffer's file..." ibuffer-do-shell-command-file
        :help "For each marked buffer, run a shell command with its file as argument"))
    (define-key-after operate-map [do-eval]
      '(menu-item "Eval..." ibuffer-do-eval
        :help "Evaluate a Lisp form in each marked buffer"))
    (define-key-after operate-map [do-view-and-eval]
      '(menu-item "Eval (viewing buffer)..." ibuffer-do-view-and-eval
        :help "Evaluate a Lisp form in each marked buffer while viewing it"))

    operate-map))

(define-key ibuffer-mode-groups-popup [kill-filter-group]
  '(menu-item "Kill filter group"
	      ibuffer-kill-line
	      :enable (and (featurep 'ibuf-ext)
			   ibuffer-filter-groups)))
(define-key ibuffer-mode-groups-popup [yank-filter-group]
  '(menu-item "Yank last killed filter group"
	      ibuffer-yank
	      :enable (and (featurep 'ibuf-ext)
			   ibuffer-filter-group-kill-ring)))

(defvar ibuffer-name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-mouse-toggle-mark)
    (define-key map [(mouse-2)] 'ibuffer-mouse-visit-buffer)
    (define-key map [down-mouse-3] 'ibuffer-mouse-popup-menu)
    map))

(defvar ibuffer-filename/process-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-do-sort-by-filename/process)
    map))

(defvar ibuffer-mode-name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-2)] 'ibuffer-mouse-filter-by-mode)
    (define-key map (kbd "RET") 'ibuffer-interactive-filter-by-mode)
    map))

(defvar ibuffer-name-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-do-sort-by-alphabetic)
    map))

(defvar ibuffer-size-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-do-sort-by-size)
    map))

(defvar ibuffer-mode-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-do-sort-by-major-mode)
    map))

(defvar ibuffer-mode-filter-group-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-mouse-toggle-mark)
    (define-key map [(mouse-2)] 'ibuffer-mouse-toggle-filter-group)
    (define-key map (kbd "RET") 'ibuffer-toggle-filter-group)
    (define-key map [down-mouse-3] 'ibuffer-mouse-popup-menu)
    map))

(defvar ibuffer-restore-window-config-on-quit nil
  "If non-nil, restore previous window configuration upon exiting `ibuffer'.")

(defvar ibuffer-prev-window-config nil
  "Window configuration before starting Ibuffer.")

(defvar ibuffer-did-modification nil)

(defvar ibuffer-compiled-formats nil)
(defvar ibuffer-cached-formats nil)
(defvar ibuffer-cached-eliding-string nil)
(defvar ibuffer-cached-elide-long-columns 0)

(defvar ibuffer-sorting-functions-alist nil
  "An alist of functions which describe how to sort buffers.

Note: You most likely do not want to modify this variable directly;
use `define-ibuffer-sorter' instead.

The alist elements are constructed like (NAME DESCRIPTION FUNCTION)
Where NAME is a symbol describing the sorting method, DESCRIPTION is a
short string which will be displayed in the minibuffer and menu, and
FUNCTION is a function of two arguments, which will be the buffers to
compare.")

;;; Utility functions
(defun ibuffer-columnize-and-insert-list (list &optional pad-width)
  "Insert LIST into the current buffer in as many columns as possible.
The maximum number of columns is determined by the current window
width and the longest string in LIST."
  (unless pad-width
    (setq pad-width 3))
  (let ((width (window-width))
	(max (+ (apply #'max (mapcar #'length list))
		pad-width)))
    (let ((columns (/ width max)))
      (when (zerop columns)
	(setq columns 1))
      (while list
	(dotimes (i (1- columns))
	  (insert (concat (car list) (make-string (- max (length (car list)))
						  ?\s)))
	  (setq list (cdr list)))
	(when (not (null list))
	  (insert (pop list)))
	(insert "\n")))))

(defsubst ibuffer-current-mark ()
  (cadr (get-text-property (line-beginning-position)
			   'ibuffer-properties)))

(defun ibuffer-mouse-toggle-mark (event)
  "Toggle the marked status of the buffer chosen with the mouse."
  (interactive "e")
  (unwind-protect
      (let ((pt (save-excursion
		  (mouse-set-point event)
		  (point))))
	(ibuffer-aif (get-text-property (point) 'ibuffer-filter-group-name)
	    (ibuffer-toggle-marks it)
	  (goto-char pt)
	  (let ((mark (ibuffer-current-mark)))
	    (setq buffer-read-only nil)
	    (if (eq mark ibuffer-marked-char)
		(ibuffer-set-mark ?\s)
	      (ibuffer-set-mark ibuffer-marked-char)))))
    (setq buffer-read-only t)))

(defun ibuffer-find-file (file &optional wildcards)
  "Like `find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
			      (if (buffer-live-p buf)
				  (with-current-buffer buf
				    default-directory)
				default-directory))))
     (list (read-file-name "Find file: " default-directory)
	   t)))
  (find-file file wildcards))

(defun ibuffer-mouse-visit-buffer (event)
  "Visit the buffer chosen with the mouse."
  (interactive "e")
  (switch-to-buffer
   (save-excursion
     (mouse-set-point event)
     (ibuffer-current-buffer t))))

(defun ibuffer-mouse-popup-menu (event)
  "Display a menu of operations."
  (interactive "e")
  (let ((eventpt (posn-point (event-end event)))
	(origpt (point)))
    (unwind-protect
	(if (get-text-property eventpt 'ibuffer-filter-group-name)
	    (progn
	      (goto-char eventpt)
	      (popup-menu ibuffer-mode-groups-popup))
	  (let ((inhibit-read-only t))
	    (ibuffer-save-marks
	      ;; hm.  we could probably do this in a better fashion
	      (ibuffer-unmark-all ?\r)
	      (save-excursion
		(goto-char eventpt)
		(ibuffer-set-mark ibuffer-marked-char))
	      (save-excursion
		(popup-menu ibuffer-mode-operate-map)))))
      (setq buffer-read-only t)
      (if (= eventpt (point))
	  (goto-char origpt)))))

(defun ibuffer-skip-properties (props direction)
  (while (and (not (eobp))
	      (let ((hit nil))
		(dolist (prop props hit)
		  (when (get-text-property (point) prop)
		    (setq hit t)))))
    (forward-line direction)
    (beginning-of-line)))

(defun ibuffer-customize ()
  "Begin customizing Ibuffer interactively."
  (interactive)
  (customize-group 'ibuffer))

(defun ibuffer-backward-line (&optional arg skip-group-names)
  "Move backwards ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (or arg (setq arg 1))
  (beginning-of-line)
  (while (> arg 0)
    (forward-line -1)
    (when (and ibuffer-movement-cycle
	       (or (get-text-property (point) 'ibuffer-title)
		   (and skip-group-names
			(get-text-property (point)
					   'ibuffer-filter-group-name))))
      (goto-char (point-max))
      (beginning-of-line))
    (ibuffer-skip-properties (append '(ibuffer-summary)
				     (when skip-group-names
				       '(ibuffer-filter-group-name)))
			     -1)
    ;; Handle the special case of no buffers.
    (when (get-text-property (point) 'ibuffer-title)
      (forward-line 1)
      (setq arg 1))
    (decf arg)))

(defun ibuffer-forward-line (&optional arg skip-group-names)
  "Move forward ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (or arg (setq arg 1))
  (beginning-of-line)
  (when (and ibuffer-movement-cycle
	     (or (eobp)
		 (get-text-property (point) 'ibuffer-summary)))
    (goto-char (point-min)))
  (when (or (get-text-property (point) 'ibuffer-title)
	    (and skip-group-names
		 (get-text-property (point) 'ibuffer-filter-group-name)))
    (when (> arg 0)
      (decf arg))
    (ibuffer-skip-properties (append '(ibuffer-title)
				     (when skip-group-names
				       '(ibuffer-filter-group-name)))
			     1))
  (if (< arg 0)
      (ibuffer-backward-line (- arg))
    (while (> arg 0)
      (forward-line 1)
      (when (and ibuffer-movement-cycle
		 (or (eobp)
		     (get-text-property (point) 'ibuffer-summary)))
	(goto-char (point-min)))
      (decf arg)
      (ibuffer-skip-properties (append '(ibuffer-title)
				       (when skip-group-names
					 '(ibuffer-filter-group-name)))
			       1))))

(defun ibuffer-visit-buffer (&optional single)
  "Visit the buffer on this line.
If optional argument SINGLE is non-nil, then also ensure there is only
one window."
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (switch-to-buffer buf)
    (when single
      (delete-other-windows))))

(defun ibuffer-visit-buffer-other-window (&optional noselect)
  "Visit the buffer on this line in another window."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer (current-buffer))
    (if noselect
	(let ((curwin (selected-window)))
	  (pop-to-buffer buf)
	  (select-window curwin))
      (switch-to-buffer-other-window buf))))

(defun ibuffer-visit-buffer-other-window-noselect ()
  "Visit the buffer on this line in another window, but don't select it."
  (interactive)
  (ibuffer-visit-buffer-other-window t))

(defun ibuffer-visit-buffer-other-frame ()
  "Visit the buffer on this line in another frame."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer (current-buffer))
    (switch-to-buffer-other-frame buf)))

(defun ibuffer-visit-buffer-1-window ()
  "Visit the buffer on this line, and delete other windows."
  (interactive)
  (ibuffer-visit-buffer t))

(defun ibuffer-bury-buffer ()
  "Bury the buffer on this line."
  (interactive)
  (let ((buf (ibuffer-current-buffer t))
	(line (+ 1 (count-lines 1 (point)))))
    (bury-buffer buf)
    (ibuffer-update nil t)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ibuffer-visit-tags-table ()
  "Visit the tags table in the buffer on this line.  See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (ibuffer-current-buffer t))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun ibuffer-do-view (&optional other-frame)
  "View marked buffers, or the buffer on the current line.
If optional argument OTHER-FRAME is non-nil, then display each
marked buffer in a new frame.  Otherwise, display each buffer as
a new window in the current frame, splitting vertically."
  (interactive)
  (ibuffer-do-view-1 (if other-frame 'other-frame 'vertically)))

(defun ibuffer-do-view-horizontally (&optional other-frame)
  "As `ibuffer-do-view', but split windows horizontally."
  (interactive)
  (ibuffer-do-view-1 (if other-frame 'other-frame 'horizontally)))

(defun ibuffer-do-view-1 (type)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (when (null marked-bufs)
      (setq marked-bufs (list (ibuffer-current-buffer t))))
    (unless (and (eq type 'other-frame)
		 (not ibuffer-expert)
		 (> (length marked-bufs) 3)
		 (not (y-or-n-p (format "Really create a new frame for %s buffers? "
					(length marked-bufs)))))
      (set-buffer-modified-p nil)
      (delete-other-windows)
      (switch-to-buffer (pop marked-bufs))
      (let ((height (/ (1- (if (eq type 'horizontally) (frame-width)
			     (frame-height)))
		       (1+ (length marked-bufs)))))
	(mapcar (if (eq type 'other-frame)
		    (lambda (buf)
		      (let ((curframe (selected-frame)))
			(select-frame (make-frame))
			(switch-to-buffer buf)
			(select-frame curframe)))
		  (lambda (buf)
		    (split-window nil height (eq type 'horizontally))
		    (other-window 1)
		    (switch-to-buffer buf)))
		marked-bufs)))))

(defun ibuffer-do-view-other-frame ()
  "View each of the marked buffers in a separate frame."
  (interactive)
  (ibuffer-do-view t))

(defsubst ibuffer-map-marked-lines (func)
  (prog1 (ibuffer-map-on-mark ibuffer-marked-char func)
    (ibuffer-redisplay t)))

(defun ibuffer-shrink-to-fit (&optional owin)
  ;; Make sure that redisplay is performed, otherwise there can be a
  ;; bad interaction with code in the window-scroll-functions hook
  (redisplay t)
  (fit-window-to-buffer nil (when owin (/ (frame-height)
					  (length (window-list (selected-frame)))))))

(defun ibuffer-confirm-operation-on (operation names)
  "Display a buffer asking whether to perform OPERATION on NAMES."
  (or ibuffer-expert
      (if (= (length names) 1)
	  (y-or-n-p (format "Really %s buffer %s? " operation (car names)))
	(let ((buf (get-buffer-create "*Ibuffer confirmation*")))
	  (with-current-buffer buf
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (ibuffer-columnize-and-insert-list names)
	    (goto-char (point-min))
	    (setq buffer-read-only t))
	  (let ((lastwin (car (last (window-list nil 'nomini)))))
	    ;; Now attempt to display the buffer...
	    (save-window-excursion
	      (select-window lastwin)
	      ;; The window might be too small to split; in that case,
	      ;; try a few times to increase its size before giving up.
	      (let ((attempts 0)
		    (trying t))
		(while trying
		  (condition-case err
		      (progn
			(split-window)
			(setq trying nil))
		    (error
		     ;; Handle a failure
		     (if (or (> (incf attempts) 4)
			     (and (stringp (cadr err))
				  ;; This definitely falls in the
				  ;; ghetto hack category...
				  (not (string-match "too small" (cadr err)))))
			 (signal (car err) (cdr err))
		       (enlarge-window 3))))))
	      (select-window (next-window))
	      (switch-to-buffer buf)
	      (unwind-protect
		  (progn
		    (fit-window-to-buffer)
		    (y-or-n-p (format "Really %s %d buffers? "
				      operation (length names))))
		(kill-buffer buf))))))))

(defsubst ibuffer-map-lines-nomodify (function)
  "As `ibuffer-map-lines', but don't set the modification flag."
  (ibuffer-map-lines function t))

(defun ibuffer-buffer-names-with-mark (mark)
  (let ((ibuffer-buffer-names-with-mark-result nil))
    (ibuffer-map-lines-nomodify
     (lambda (buf mk)
       (when (char-equal mark mk)
	 (push (buffer-name buf)
	       ibuffer-buffer-names-with-mark-result))))
    ibuffer-buffer-names-with-mark-result))

(defsubst ibuffer-marked-buffer-names ()
  (ibuffer-buffer-names-with-mark ibuffer-marked-char))

(defsubst ibuffer-deletion-marked-buffer-names ()
  (ibuffer-buffer-names-with-mark ibuffer-deletion-char))

(defun ibuffer-count-marked-lines (&optional all)
  (if all
      (ibuffer-map-lines-nomodify
       (lambda (_buf mark)
	 (not (char-equal mark ?\s))))
    (ibuffer-map-lines-nomodify
     (lambda (_buf mark)
       (char-equal mark ibuffer-marked-char)))))

(defsubst ibuffer-count-deletion-lines ()
  (ibuffer-map-lines-nomodify
   (lambda (_buf mark)
     (char-equal mark ibuffer-deletion-char))))

(defsubst ibuffer-map-deletion-lines (func)
  (ibuffer-map-on-mark ibuffer-deletion-char func))

(defsubst ibuffer-assert-ibuffer-mode ()
  (assert (derived-mode-p 'ibuffer-mode)))

(defun ibuffer-buffer-file-name ()
  (or buffer-file-name
      (let ((dirname (or (and (boundp 'dired-directory)
			      (if (stringp dired-directory)
				  dired-directory
				(car dired-directory)))
			 (bound-and-true-p list-buffers-directory))))
	(and dirname (expand-file-name dirname)))))

(define-ibuffer-op ibuffer-do-save ()
  "Save marked buffers as with `save-buffer'."
  (:complex t
   :opstring "saved"
   :modifier-p :maybe)
  (when (buffer-modified-p buf)
    (if (not (with-current-buffer buf
	       buffer-file-name))
	;; handle the case where we're prompted
	;; for a file name
	(save-window-excursion
	  (switch-to-buffer buf)
	  (save-buffer))
      (with-current-buffer buf
	(save-buffer))))
  t)

(define-ibuffer-op ibuffer-do-toggle-modified ()
  "Toggle modification flag of marked buffers."
  (:opstring "(un)marked as modified"
   :modifier-p t)
  (set-buffer-modified-p (not (buffer-modified-p))))

(define-ibuffer-op ibuffer-do-toggle-read-only (&optional arg)
  "Toggle read only status in marked buffers.
With optional ARG, make read-only only if ARG is positive."
  (:opstring "toggled read only status in"
   :interactive "P"
   :modifier-p t)
  (toggle-read-only arg))

(define-ibuffer-op ibuffer-do-delete ()
  "Kill marked buffers as with `kill-this-buffer'."
  (:opstring "killed"
   :active-opstring "kill"
   :dangerous t
   :complex t
   :modifier-p t)
  (if (kill-buffer buf)
      'kill
    nil))

(define-ibuffer-op ibuffer-do-kill-on-deletion-marks ()
  "Kill buffers marked for deletion as with `kill-this-buffer'."
  (:opstring "killed"
   :active-opstring "kill"
   :dangerous t
   :complex t
   :mark :deletion
   :modifier-p t)
  (if (kill-buffer buf)
      'kill
    nil))

(defun ibuffer-unmark-all (mark)
  "Unmark all buffers with mark MARK."
  (interactive "cRemove marks (RET means all):")
  (if (= (ibuffer-count-marked-lines t) 0)
      (message "No buffers marked; use 'm' to mark a buffer")
    (cond
     ((char-equal mark ibuffer-marked-char)
      (ibuffer-map-marked-lines
       (lambda (_buf _mark)
	 (ibuffer-set-mark-1 ?\s)
	 t)))
     ((char-equal mark ibuffer-deletion-char)
      (ibuffer-map-deletion-lines
       (lambda (_buf _mark)
	 (ibuffer-set-mark-1 ?\s)
	 t)))
     (t
      (ibuffer-map-lines
       (lambda (_buf mark)
	 (when (not (char-equal mark ?\s))
	   (ibuffer-set-mark-1 ?\s))
	 t)))))
  (ibuffer-redisplay t))

(defun ibuffer-toggle-marks (&optional group)
  "Toggle which buffers are marked.
In other words, unmarked buffers become marked, and marked buffers
become unmarked.
If point is on a group name, then this function operates on that
group."
  (interactive)
  (ibuffer-aif (get-text-property (point) 'ibuffer-filter-group-name)
      (setq group it))
  (let ((count
	 (ibuffer-map-lines
	  (lambda (_buf mark)
	    (cond ((eq mark ibuffer-marked-char)
		   (ibuffer-set-mark-1 ?\s)
		   nil)
		  ((eq mark ?\s)
		   (ibuffer-set-mark-1 ibuffer-marked-char)
		   t)
		  (t
		   nil)))
	  nil group)))
    (message "%s buffers marked" count))
  (ibuffer-redisplay t))

(defun ibuffer-mark-forward (arg)
  "Mark the buffer on this line, and move forward ARG lines.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (ibuffer-mark-interactive arg ibuffer-marked-char 1))

(defun ibuffer-unmark-forward (arg)
  "Unmark the buffer on this line, and move forward ARG lines.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (ibuffer-mark-interactive arg ?\s 1))

(defun ibuffer-unmark-backward (arg)
  "Unmark the buffer on this line, and move backward ARG lines.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (ibuffer-mark-interactive arg ?\s -1))

(defun ibuffer-mark-interactive (arg mark movement)
  (ibuffer-assert-ibuffer-mode)
  (or arg (setq arg 1))
  (ibuffer-forward-line 0)
  (ibuffer-aif (get-text-property (point) 'ibuffer-filter-group-name)
      (progn
	(require 'ibuf-ext)
	(ibuffer-mark-on-buffer #'identity mark it))
    (ibuffer-forward-line 0 t)
    (let ((inhibit-read-only t))
      (while (> arg 0)
	(ibuffer-set-mark mark)
	(ibuffer-forward-line movement t)
	(setq arg (1- arg))))))

(defun ibuffer-set-mark (mark)
  (ibuffer-assert-ibuffer-mode)
  (let ((inhibit-read-only t))
    (ibuffer-set-mark-1 mark)
    (setq ibuffer-did-modification t)
    (ibuffer-redisplay-current)
    (beginning-of-line)))

(defun ibuffer-set-mark-1 (mark)
  (let ((beg (line-beginning-position))
	(end (line-end-position)))
    (put-text-property beg end 'ibuffer-properties
		       (list (ibuffer-current-buffer)
			     mark))))

(defun ibuffer-mark-for-delete (arg)
  "Mark the buffers on ARG lines forward for deletion.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (ibuffer-mark-interactive arg ibuffer-deletion-char 1))

(defun ibuffer-mark-for-delete-backwards (arg)
  "Mark the buffers on ARG lines backward for deletion.
If point is on a group name, this function operates on that group."
  (interactive "P")
  (ibuffer-mark-interactive arg ibuffer-deletion-char -1))

(defun ibuffer-current-buffer (&optional must-be-live)
  (let ((buf (car (get-text-property (line-beginning-position)
				     'ibuffer-properties))))
    (when must-be-live
      (if (bufferp buf)
	  (unless (buffer-live-p buf)
	    (error "Buffer %s has been killed; %s" buf (substitute-command-keys "use `\\[ibuffer-update]' to update")))
	(error "No buffer on this line")))
    buf))

(defun ibuffer-active-formats-name ()
  (if (boundp 'ibuffer-filter-format-alist)
      (let ((ret nil))
	(dolist (filter ibuffer-filtering-qualifiers ret)
	  (let ((val (assq (car filter) ibuffer-filter-format-alist)))
	    (when val
	      (setq ret (car filter)))))
	(if ret
	    ret
	  :ibuffer-formats))
    :ibuffer-formats))

(defun ibuffer-current-formats (uncompiledp)
  (let* ((name (ibuffer-active-formats-name)))
    (ibuffer-check-formats)
    (if (eq name :ibuffer-formats)
	(if uncompiledp
	    ibuffer-formats
	  ibuffer-compiled-formats)
      (cadr (assq name
		  (if uncompiledp
		      ibuffer-filter-format-alist
		    ibuffer-compiled-filter-formats))))))

(defun ibuffer-current-format (&optional uncompiledp)
  (or ibuffer-current-format
      (setq ibuffer-current-format 0))
  (nth ibuffer-current-format (ibuffer-current-formats uncompiledp)))

(defun ibuffer-expand-format-entry (form)
  (if (or (consp form)
	  (symbolp form))
      (let ((sym (intern (concat "ibuffer-make-column-"
				 (symbol-name (if (consp form)
						  (car form)
						form))))))
	(unless (or (fboundp sym)
		    (assq sym ibuffer-inline-columns))
	  (error "Unknown column %s in ibuffer-formats" form))
	(let (min max align elide)
	  (if (consp form)
	      (setq min (or (nth 1 form) 0)
		    max (or (nth 2 form) -1)
		    align (or (nth 3 form) :left)
		    elide (or (nth 4 form) nil))
	    (setq min 0
		  max -1
		  align :left
		  elide nil))
	  (list sym min max align elide)))
    form))

(defun ibuffer-compile-make-eliding-form (strvar elide from-end-p)
  (let ((ellipsis (propertize ibuffer-eliding-string 'font-lock-face 'bold)))
    (if (or elide (with-no-warnings ibuffer-elide-long-columns))
	`(if (> strlen 5)
	     ,(if from-end-p
		  `(concat ,ellipsis
			   (substring ,strvar
				      (length ibuffer-eliding-string)))
		`(concat
		  (substring ,strvar 0 (- strlen ,(length ellipsis)))
		  ,ellipsis))
	   ,strvar)
      strvar)))

(defun ibuffer-compile-make-substring-form (strvar maxvar from-end-p)
  (if from-end-p
      `(substring str
		  (- strlen ,maxvar))
    `(substring ,strvar 0 ,maxvar)))

(defun ibuffer-compile-make-format-form (strvar widthform alignment)
  (let* ((left `(make-string tmp2 ?\s))
	 (right `(make-string (- tmp1 tmp2) ?\s)))
    `(progn
       (setq tmp1 ,widthform
	     tmp2 (/ tmp1 2))
       ,(case alignment
	  (:right `(concat ,left ,right ,strvar))
	  (:center `(concat ,left ,strvar ,right))
	  (:left `(concat ,strvar ,left ,right))
	  (t (error "Invalid alignment %s" alignment))))))

(defun ibuffer-compile-format (format)
  (let ((result nil)
	;; We use these variables to keep track of which variables
	;; inside the generated function we need to bind, since
	;; binding variables in Emacs takes time.
	str-used tmp1-used tmp2-used global-strlen-used)
    (dolist (form format)
      (push
       ;; Generate a form based on a particular format entry, like
       ;; " ", mark, or (mode 16 16 :right).
       (if (stringp form)
	   ;; It's a string; all we need to do is insert it.
	   `(insert ,form)
	 (let* ((form (ibuffer-expand-format-entry form))
		(sym (nth 0 form))
		(min (nth 1 form))
		(max (nth 2 form))
		(align (nth 3 form))
		(elide (nth 4 form)))
	   (let* ((from-end-p (when (minusp min)
				(setq min (- min))
				t))
		  (letbindings nil)
		  (outforms nil)
		  minform
		  maxform
		  min-used max-used strlen-used)
	     (when (or (not (integerp min)) (>= min 0))
	       ;; This is a complex case; they want it limited to a
	       ;; minimum size.
	       (setq min-used t)
	       (setq str-used t strlen-used t global-strlen-used t
		     tmp1-used t tmp2-used t)
	       ;; Generate code to limit the string to a minimum size.
	       (setq minform `(progn
				(setq str
				      ,(ibuffer-compile-make-format-form
					'str
					`(- ,(if (integerp min)
						 min
					       'min)
					    strlen)
					align)))))
	     (when (or (not (integerp max)) (> max 0))
	       (setq str-used t max-used t)
	       ;; Generate code to limit the string to a maximum size.
	       (setq maxform `(progn
				(setq str
				      ,(ibuffer-compile-make-substring-form
					'str
					(if (integerp max)
					    max
					  'max)
					from-end-p))
				(setq strlen (length str))
				(setq str
				      ,(ibuffer-compile-make-eliding-form
                                        'str elide from-end-p)))))
	     ;; Now, put these forms together with the rest of the code.
	     (let ((callform
		    ;; Is this an "inline" column?  This means we have
		    ;; to get the code from the
		    ;; `ibuffer-inline-columns' alist and insert it
		    ;; into our generated code.  Otherwise, we just
		    ;; generate a call to the column function.
		    (ibuffer-aif (assq sym ibuffer-inline-columns)
			(nth 1 it)
		      `(,sym buffer mark)))
		   ;; You're not expected to understand this.  Hell, I
		   ;; don't even understand it, and I wrote it five
		   ;; minutes ago.
		   (insertgenfn
                    (ibuffer-aif (get sym 'ibuffer-column-summarizer)
                        ;; I really, really wish Emacs Lisp had closures.
                        (lambda (arg sym)
                          `(insert
                            (let ((ret ,arg))
                              (put ',sym 'ibuffer-column-summary
                                   (cons ret (get ',sym
                                                  'ibuffer-column-summary)))
                              ret)))
                      (lambda (arg sym)
                        `(insert ,arg))))
		   (mincompform `(< strlen ,(if (integerp min)
						min
					      'min)))
		   (maxcompform `(> strlen ,(if (integerp max)
						max
					      'max))))
	       (if (or min-used max-used)
		   ;; The complex case, where we have to limit the
		   ;; form to a maximum or minimum size.
		   (progn
		     (when (and min-used (not (integerp min)))
		       (push `(min ,min) letbindings))
		     (when (and max-used (not (integerp max)))
		       (push `(max ,max) letbindings))
		     (push
		      (if (and min-used max-used)
			  `(if ,mincompform
			       ,minform
			     (if ,maxcompform
				 ,maxform))
			(if min-used
			    `(when ,mincompform
			       ,minform)
			  `(when ,maxcompform
			     ,maxform)))
		      outforms)
		     (push (append
			    `(setq str ,callform)
			    (when strlen-used
			      `(strlen (length str))))
			   outforms)
		     (setq outforms
			   (append outforms
                                   (list (funcall insertgenfn 'str sym)))))
		 ;; The simple case; just insert the string.
		 (push (funcall insertgenfn callform sym) outforms))
	       ;; Finally, return a `let' form which binds the
	       ;; variables in `letbindings', and contains all the
	       ;; code in `outforms'.
	       `(let ,letbindings
		  ,@outforms)))))
       result))
    (setq result
	  ;; We don't want to unconditionally load the byte-compiler.
	  (funcall (if (or ibuffer-always-compile-formats
			   (featurep 'bytecomp))
		       #'byte-compile
		     #'identity)
		   ;; Here, we actually create a lambda form which
		   ;; inserts all the generated forms for each entry
		   ;; in the format string.
		   (nconc (list 'lambda '(buffer mark))
			  `((let ,(append (when str-used
					    '(str))
					  (when global-strlen-used
					    '(strlen))
					  (when tmp1-used
					    '(tmp1))
					  (when tmp2-used
					    '(tmp2)))
			      ,@(nreverse result))))))))

(defun ibuffer-recompile-formats ()
  "Recompile `ibuffer-formats'."
  (interactive)
  (setq ibuffer-compiled-formats
	(mapcar #'ibuffer-compile-format ibuffer-formats))
  (when (boundp 'ibuffer-filter-format-alist)
    (setq ibuffer-compiled-filter-formats
	  (mapcar (lambda (entry)
		    (cons (car entry)
			  (mapcar (lambda (formats)
				    (mapcar #'ibuffer-compile-format formats))
				  (cdr entry))))
		  ibuffer-filter-format-alist))))

(defun ibuffer-clear-summary-columns (format)
  (dolist (form format)
    (ibuffer-awhen (and (consp form)
			(get (car form) 'ibuffer-column-summarizer))
      (put (car form) 'ibuffer-column-summary nil))))

(defun ibuffer-check-formats ()
  (when (null ibuffer-formats)
    (error "No formats!"))
  (let ((ext-loaded (featurep 'ibuf-ext)))
    (when (or (null ibuffer-compiled-formats)
	      (null ibuffer-cached-formats)
	      (not (eq ibuffer-cached-formats ibuffer-formats))
	      (null ibuffer-cached-eliding-string)
	      (not (equal ibuffer-cached-eliding-string ibuffer-eliding-string))
	      (eql 0 ibuffer-cached-elide-long-columns)
	      (not (eql ibuffer-cached-elide-long-columns
			(with-no-warnings ibuffer-elide-long-columns)))
	      (and ext-loaded
		   (not (eq ibuffer-cached-filter-formats
			    ibuffer-filter-format-alist))
		   (and ibuffer-filter-format-alist
			(null ibuffer-compiled-filter-formats))))
      (message "Formats have changed, recompiling...")
      (ibuffer-recompile-formats)
      (setq ibuffer-cached-formats ibuffer-formats
	    ibuffer-cached-eliding-string ibuffer-eliding-string
	    ibuffer-cached-elide-long-columns (with-no-warnings ibuffer-elide-long-columns))
      (when ext-loaded
	(setq ibuffer-cached-filter-formats ibuffer-filter-format-alist))
      (message "Formats have changed, recompiling...done"))))

(defvar ibuffer-inline-columns nil)

(define-ibuffer-column mark (:name " " :inline t)
  (string mark))

(define-ibuffer-column read-only (:name "R" :inline t)
  (if buffer-read-only
      (string ibuffer-read-only-char)
    " "))

(define-ibuffer-column modified (:name "M" :inline t)
  (if (buffer-modified-p)
      (string ibuffer-modified-char)
    " "))

(define-ibuffer-column name
  (:inline t
   :header-mouse-map ibuffer-name-header-map
   :props
   ('mouse-face 'highlight 'keymap ibuffer-name-map
		'ibuffer-name-column t
		'help-echo '(if tooltip-mode
				"mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
			      "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
   :summarizer
   (lambda (strings)
     (let ((bufs (length strings)))
       (cond ((zerop bufs) "No buffers")
	     ((= 1 bufs) "1 buffer")
	     (t (format "%s buffers" bufs))))))
  (propertize (buffer-name) 'font-lock-face (ibuffer-buffer-name-face buffer mark)))

(define-ibuffer-column size
  (:inline t
   :header-mouse-map ibuffer-size-header-map
   :summarizer
   (lambda (column-strings)
     (let ((total 0))
       (dolist (string column-strings)
	 (setq total
	       ;; like, ewww ...
	       (+ (float (string-to-number string))
		  total)))
       (format "%.0f" total))))
  (format "%s" (buffer-size)))

(define-ibuffer-column mode
  (:inline t
   :header-mouse-map ibuffer-mode-header-map
   :props
   ('mouse-face 'highlight
		'keymap ibuffer-mode-name-map
		'help-echo "mouse-2: filter by this mode"))
  (format-mode-line mode-name nil nil (current-buffer)))

(define-ibuffer-column process
  (:summarizer
   (lambda (strings)
     (let ((total (length (delete "" strings))))
       (cond ((zerop total) "No processes")
	     ((= 1 total) "1 process")
	     (t (format "%d processes" total))))))
  (ibuffer-aif (get-buffer-process buffer)
      (format "(%s %s)" it (process-status it))
    ""))

(define-ibuffer-column filename
  (:summarizer
   (lambda (strings)
     (let ((total (length (delete "" strings))))
       (cond ((zerop total) "No files")
	     ((= 1 total) "1 file")
	     (t (format "%d files" total))))))
  (let ((directory-abbrev-alist ibuffer-directory-abbrev-alist))
    (abbreviate-file-name
     (or (ibuffer-buffer-file-name) ""))))

(define-ibuffer-column filename-and-process
  (:name "Filename/Process"
   :header-mouse-map ibuffer-filename/process-header-map
   :summarizer
   (lambda (strings)
     (setq strings (delete "" strings))
     (let ((procs 0)
	   (files 0))
       (dolist (string strings)
	 (if (string-match "\\(\?:\\`(\[\[:ascii:\]\]\+)\\)" string)
	     (progn (setq procs (1+ procs))
		    (if (< (match-end 0) (length string))
			(setq files (1+ files))))
	   (setq files (1+ files))))
       (concat (cond ((zerop files) "No files")
		     ((= 1 files) "1 file")
		     (t (format "%d files" files)))
	       ", "
	       (cond ((zerop procs) "no processes")
		     ((= 1 procs) "1 process")
		     (t (format "%d processes" procs)))))))
  (let ((proc (get-buffer-process buffer))
	(filename (ibuffer-make-column-filename buffer mark)))
    (if proc
	(concat (propertize (format "(%s %s)" proc (process-status proc))
			    'font-lock-face 'italic)
		(if (> (length filename) 0)
		    (format " %s" filename)
		  ""))
      filename)))

(defun ibuffer-format-column (str width alignment)
  (let ((left (make-string (/ width 2) ?\s))
	(right (make-string (- width (/ width 2)) ?\s)))
    (case alignment
      (:right (concat left right str))
      (:center (concat left str right))
      (t (concat str left right)))))

(defun ibuffer-buffer-name-face (buf mark)
  (cond ((char-equal mark ibuffer-marked-char)
	 ibuffer-marked-face)
	((char-equal mark ibuffer-deletion-char)
	 ibuffer-deletion-face)
	(t
	 (let ((level -1)
	       result)
	   (dolist (e ibuffer-fontification-alist result)
	     (when (and (> (car e) level)
			(with-current-buffer buf
			  (eval (nth 1 e))))
	       (setq level (car e)
		     result (nth 2 e))))))))

(defun ibuffer-insert-buffer-line (buffer mark format)
  "Insert a line describing BUFFER and MARK using FORMAT."
  (ibuffer-assert-ibuffer-mode)
  (let ((beg (point)))
    (funcall format buffer mark)
    (put-text-property beg (point) 'ibuffer-properties (list buffer mark)))
  (insert "\n"))

;; This function knows a bit too much of the internals.  It would be
;; nice if it was all abstracted away.
(defun ibuffer-redisplay-current ()
  (ibuffer-assert-ibuffer-mode)
  (when (eobp)
    (forward-line -1))
  (beginning-of-line)
  (let ((curformat (mapcar #'ibuffer-expand-format-entry
			   (ibuffer-current-format t))))
    (ibuffer-clear-summary-columns curformat)
    (let ((buf (ibuffer-current-buffer)))
      (when buf
	(let ((mark (ibuffer-current-mark)))
	  (save-excursion
	    (delete-region (point) (1+ (line-end-position)))
	    (ibuffer-insert-buffer-line
	     buf mark
	     (ibuffer-current-format)))
	  (when ibuffer-shrink-to-minimum-size
	    (ibuffer-shrink-to-fit)))))))

(defun ibuffer-map-on-mark (mark func)
  (ibuffer-map-lines
   (lambda (buf mk)
     (if (char-equal mark mk)
	 (funcall func buf mark)
       nil))))

(defun ibuffer-map-lines (function &optional nomodify group)
  "Call FUNCTION for each buffer.
Set the ibuffer modification flag unless NOMODIFY is non-nil.

If optional argument GROUP is non-nil, then only call FUNCTION on
buffers in filtering group GROUP.

FUNCTION is called with two arguments:
the buffer object itself and the current mark symbol."
  (ibuffer-assert-ibuffer-mode)
  (ibuffer-forward-line 0)
  (let* ((orig-target-line (1+ (count-lines (save-excursion
					      (goto-char (point-min))
					      (ibuffer-forward-line 0)
					      (point))
					    (point))))
	 (target-line-offset orig-target-line)
	 (ibuffer-map-lines-total 0)
	 (ibuffer-map-lines-count 0))
    (unwind-protect
	(progn
	  (setq buffer-read-only nil)
	  (goto-char (point-min))
	  (ibuffer-forward-line 0 t)
	  (while (and (not (eobp))
		      (not (get-text-property (point) 'ibuffer-summary))
		      (progn
			(ibuffer-forward-line 0 t)
			(and (not (eobp))
			     (not (get-text-property (point) 'ibuffer-summary)))))
	    (let ((result
		   (if (buffer-live-p (ibuffer-current-buffer))
		       (when (or (null group)
				 (ibuffer-aif (get-text-property (point) 'ibuffer-filter-group)
				     (equal group it)))
			 (save-excursion
			   (funcall function
				    (ibuffer-current-buffer)
				    (ibuffer-current-mark))))
		     ;; Kill the line if the buffer is dead
		     'kill)))
	      ;; A given mapping function should return:
	      ;; `nil' if it chose not to affect the buffer
	      ;; `kill' means the remove line from the buffer list
	      ;; `t' otherwise
	      (incf ibuffer-map-lines-total)
	      (cond ((null result)
		     (forward-line 1))
		    ((eq result 'kill)
		     (delete-region (line-beginning-position)
				    (1+ (line-end-position)))
		     (incf ibuffer-map-lines-count)
		     (when (< ibuffer-map-lines-total
			      orig-target-line)
		       (decf target-line-offset)))
		    (t
		     (incf ibuffer-map-lines-count)
		     (forward-line 1)))))
	  ibuffer-map-lines-count)
      (progn
	(setq buffer-read-only t)
	(unless nomodify
	  (set-buffer-modified-p nil))
	(goto-char (point-min))
	(ibuffer-forward-line 0)
	(ibuffer-forward-line (1- target-line-offset))))))

(defun ibuffer-get-marked-buffers ()
  "Return a list of buffer objects currently marked."
  (delq nil
	(mapcar (lambda (e)
		  (when (eq (cdr e) ibuffer-marked-char)
		    (car e)))
		(ibuffer-current-state-list))))

(defun ibuffer-current-state-list (&optional pos)
  "Return a list like (BUF . MARK) of all buffers in an ibuffer.
If POS is non-nil, return a list like (BUF MARK POINT), where POINT is
the value of point at the beginning of the line for that buffer."
  (let ((ibuffer-current-state-list-tmp '()))
    ;; ah, if only we had closures.  I bet this will mysteriously
    ;; break later.  Don't blame me.
    (if pos
	(ibuffer-map-lines-nomodify
	 (lambda (buf mark)
	   (when (buffer-live-p buf)
	     (push (list buf mark (point)) ibuffer-current-state-list-tmp))))
      (ibuffer-map-lines-nomodify
       (lambda (buf mark)
	 (when (buffer-live-p buf)
	   (push (cons buf mark) ibuffer-current-state-list-tmp)))))
    (nreverse ibuffer-current-state-list-tmp)))

(defun ibuffer-current-buffers-with-marks (curbufs)
  "Return a list like (BUF . MARK) of all open buffers."
  (let ((bufs (ibuffer-current-state-list)))
    (mapcar (lambda (buf) (let ((e (assq buf bufs)))
			    (if e
				e
			      (cons buf ?\s))))
	    curbufs)))

(defun ibuffer-buf-matches-predicates (buf predicates)
  (let ((hit nil)
	(name (buffer-name buf)))
    (dolist (pred predicates)
      (when (if (stringp pred)
		(string-match pred name)
	      (funcall pred buf))
	(setq hit t)))
    hit))

(defun ibuffer-filter-buffers (ibuffer-buf last bmarklist all)
  (let ((ext-loaded (featurep 'ibuf-ext)))
    (delq nil
	  (mapcar
	   ;; element should be like (BUFFER . MARK)
	   (lambda (e)
	     (let* ((buf (car e)))
	       (when
		   ;; This takes precedence over anything else
		   (or (and ibuffer-always-show-last-buffer
			    (eq last buf))
		       (funcall (if ext-loaded
				    #'ibuffer-ext-visible-p
				  #'ibuffer-visible-p)
				buf all ibuffer-buf))
		 e)))
	   bmarklist))))

(defun ibuffer-visible-p (buf all &optional ibuffer-buf)
  (and (or all
	   (not
	    (ibuffer-buf-matches-predicates buf ibuffer-maybe-show-predicates)))
       (or ibuffer-view-ibuffer
	   (and ibuffer-buf
		(not (eq ibuffer-buf buf))))))

;; This function is a special case; it's not defined by
;; `define-ibuffer-sorter'.
(defun ibuffer-do-sort-by-recency ()
  "Sort the buffers by last view time."
  (interactive)
  (setq ibuffer-sorting-mode 'recency)
  (when (eq ibuffer-last-sorting-mode 'recency)
    (setq ibuffer-sorting-reversep (not ibuffer-sorting-reversep)))
  (ibuffer-update nil t)
  (setq ibuffer-last-sorting-mode 'recency))

(defun ibuffer-update-format ()
  (when (null ibuffer-current-format)
    (setq ibuffer-current-format 0))
  (when (null ibuffer-formats)
    (error "Ibuffer error: no formats!")))

(defun ibuffer-switch-format ()
  "Switch the current display format."
  (interactive)
  (ibuffer-assert-ibuffer-mode)
  (unless (consp ibuffer-formats)
    (error "Ibuffer error: No formats!"))
  (setq ibuffer-current-format
	(if (>= ibuffer-current-format (1- (length (ibuffer-current-formats nil))))
	    0
	  (1+ ibuffer-current-format)))
  (ibuffer-update-format)
  (ibuffer-redisplay t))

(defun ibuffer-update-title-and-summary (format)
  (ibuffer-assert-ibuffer-mode)
  ;; Don't do funky font-lock stuff here
  (let ((after-change-functions nil))
    (if (get-text-property (point-min) 'ibuffer-title)
	(delete-region (point-min)
		       (next-single-property-change
			(point-min) 'ibuffer-title)))
    (goto-char (point-min))
    (add-text-properties
     (point)
     (progn
       (let ((opos (point)))
	 ;; Insert the title names.
	 (dolist (element format)
	   (insert
	    (if (stringp element)
		element
	      (let ((sym (car element))
		    (min (cadr element))
		    ;; (max (caddr element))
		    (align (cadddr element)))
		;; Ignore a negative min when we're inserting the title
		(when (minusp min)
		  (setq min (- min)))
		(let* ((name (or (get sym 'ibuffer-column-name)
				 (error "Unknown column %s in ibuffer-formats" sym)))
		       (len (length name))
		       (hmap (get sym 'header-mouse-map))
		       (strname (if (< len min)
				    (ibuffer-format-column name
							   (- min len)
							   align)
				  name)))
		  (when hmap
		    (setq
		     strname
		     (propertize strname 'mouse-face 'highlight 'keymap hmap)))
		  strname)))))
	 (add-text-properties opos (point) `(ibuffer-title-header t))
	 (insert "\n")
	 ;; Add the underlines
	 (let ((str (save-excursion
		      (forward-line -1)
		      (beginning-of-line)
		      (buffer-substring (point) (line-end-position)))))
	   (apply #'insert (mapcar
			    (lambda (c)
			      (if (not (or (char-equal c ?\s)
					   (char-equal c ?\n)))
				  ?-
				?\s))
			    str)))
	 (insert "\n"))
       (point))
     `(ibuffer-title t font-lock-face ,ibuffer-title-face))
    ;; Now, insert the summary columns.
    (goto-char (point-max))
    (if (get-text-property (1- (point-max)) 'ibuffer-summary)
	(delete-region (previous-single-property-change
			(point-max) 'ibuffer-summary)
		       (point-max)))
    (if ibuffer-display-summary
	(add-text-properties
	 (point)
	 (progn
	   (insert "\n")
	   (dolist (element format)
	     (insert
	      (if (stringp element)
		  (make-string (length element) ?\s)
		(let ((sym (car element)))
		  (let ((min (cadr element))
			;; (max (caddr element))
			(align (cadddr element)))
		    ;; Ignore a negative min when we're inserting the title
		    (when (minusp min)
		      (setq min (- min)))
		    (let* ((summary (if (get sym 'ibuffer-column-summarizer)
					(funcall (get sym 'ibuffer-column-summarizer)
						 (get sym 'ibuffer-column-summary))
				      (make-string (length (get sym 'ibuffer-column-name))
						   ?\s)))
			   (len (length summary)))
		      (if (< len min)
			  (ibuffer-format-column summary
						 (- min len)
						 align)
			summary)))))))
	   (point))
	 `(ibuffer-summary t)))))


(defun ibuffer-redisplay (&optional silent)
  "Redisplay the current list of buffers.
This does not show new buffers; use `ibuffer-update' for that.

If optional arg SILENT is non-nil, do not display progress messages."
  (interactive)
  (ibuffer-forward-line 0)
  (unless silent
    (message "Redisplaying current buffer list..."))
  (let ((blist (ibuffer-current-state-list)))
    (when (and (null blist)
	       (featurep 'ibuf-ext)
	       (or ibuffer-filtering-qualifiers ibuffer-hidden-filter-groups))
      (message "No buffers! (note: filtering in effect)"))
    (ibuffer-redisplay-engine blist t)
    (unless silent
      (message "Redisplaying current buffer list...done"))
    (ibuffer-forward-line 0)))

(defun ibuffer-update (arg &optional silent)
  "Regenerate the list of all buffers.

Prefix arg non-nil means to toggle whether buffers that match
`ibuffer-maybe-show-predicates' should be displayed.

If optional arg SILENT is non-nil, do not display progress messages."
  (interactive "P")
  (if arg
      (setq ibuffer-display-maybe-show-predicates
	    (not ibuffer-display-maybe-show-predicates)))
  (ibuffer-forward-line 0)
  (let* ((bufs (buffer-list))
	 (blist (ibuffer-filter-buffers
		 (current-buffer)
		 (if (and
		      (cadr bufs)
		      (eq ibuffer-always-show-last-buffer
			  :nomini)
		      (minibufferp (cadr bufs)))
		     (caddr bufs)
		   (cadr bufs))
		 (ibuffer-current-buffers-with-marks bufs)
		 ibuffer-display-maybe-show-predicates)))
    (and (null blist)
	 (featurep 'ibuf-ext)
	 ibuffer-filtering-qualifiers
	 (message "No buffers! (note: filtering in effect)"))
    (unless silent
      (message "Updating buffer list..."))
    (ibuffer-redisplay-engine blist arg)
    (unless silent
      (message "Updating buffer list...done")))
  (if (eq ibuffer-shrink-to-minimum-size 'onewindow)
      (ibuffer-shrink-to-fit t)
    (when ibuffer-shrink-to-minimum-size
      (ibuffer-shrink-to-fit)))
  (ibuffer-forward-line 0)
  ;; I tried to update this automatically from the mode-line-process format,
  ;; but changing nil-ness of header-line-format while computing
  ;; mode-line-format is asking a bit too much it seems.  --Stef
  (setq header-line-format
        (and ibuffer-use-header-line
             ibuffer-filtering-qualifiers
             ibuffer-header-line-format)))

(defun ibuffer-sort-bufferlist (bmarklist)
  (unless ibuffer-sorting-functions-alist
    ;; make sure the sorting functions are loaded
    (require 'ibuf-ext))
  (let* ((sortdat (assq ibuffer-sorting-mode
			ibuffer-sorting-functions-alist))
	 (func (caddr sortdat)))
    (let ((result
	   ;; actually sort the buffers
	   (if (and sortdat func)
	       (sort bmarklist func)
	     bmarklist)))
      ;; perhaps reverse the sorted buffer list
      (if ibuffer-sorting-reversep
	  (nreverse result)
	result))))

(defun ibuffer-insert-filter-group (name display-name filter-string format bmarklist)
  (add-text-properties
   (point)
   (progn
     (insert "[ " display-name " ]")
     (point))
   `(ibuffer-filter-group-name
     ,name
     font-lock-face ,ibuffer-filter-group-name-face
     keymap ,ibuffer-mode-filter-group-map
     mouse-face highlight
     help-echo ,(let ((echo '(if tooltip-mode
				 "mouse-1: toggle marks in this group\nmouse-2: hide/show this filtering group"
			       "mouse-1: toggle marks  mouse-2: hide/show")))
		  (if (> (length filter-string) 0)
		      `(concat ,filter-string
			       (if tooltip-mode "\n" " ")
			       ,echo)
		    echo))))
  (insert "\n")
  (when bmarklist
    (put-text-property
     (point)
     (progn
       (dolist (entry bmarklist)
	 (ibuffer-insert-buffer-line (car entry) (cdr entry) format))
       (point))
     'ibuffer-filter-group
     name)))

(defun ibuffer-redisplay-engine (bmarklist &optional _ignore)
  (ibuffer-assert-ibuffer-mode)
  (let* ((--ibuffer-insert-buffers-and-marks-format
	  (ibuffer-current-format))
	 (--ibuffer-expanded-format (mapcar #'ibuffer-expand-format-entry
					    (ibuffer-current-format t)))
	 (orig (count-lines (point-min) (point)))
	 ;; Inhibit font-lock caching tricks, since we're modifying the
	 ;; entire buffer at once
	 (after-change-functions nil)
	 (ext-loaded (featurep 'ibuf-ext))
	 (bgroups (if ext-loaded
		      (ibuffer-generate-filter-groups bmarklist)
		    (list (cons "Default" bmarklist)))))
    (ibuffer-clear-summary-columns --ibuffer-expanded-format)
    (unwind-protect
	(progn
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (ibuffer-update-format)
	  (dolist (group (nreverse bgroups))
	    (let* ((name (car group))
		   (disabled (and ext-loaded
				  (member name ibuffer-hidden-filter-groups)))
		   (bmarklist (cdr group)))
	      (unless (and (null bmarklist)
			   (not disabled)
			   ext-loaded
			   (null ibuffer-show-empty-filter-groups))
		(ibuffer-insert-filter-group
		 name
		 (if disabled (concat name " ...") name)
		 (if ext-loaded
		     (ibuffer-format-filter-group-data name)
		   "")
		 --ibuffer-insert-buffers-and-marks-format
		 (if disabled
		     nil
		   (ibuffer-sort-bufferlist bmarklist))))))
	  (ibuffer-update-title-and-summary --ibuffer-expanded-format))
      (setq buffer-read-only t)
      (set-buffer-modified-p ibuffer-did-modification)
      (setq ibuffer-did-modification nil)
      (goto-char (point-min))
      (forward-line orig))))

(defun ibuffer-quit ()
  "Quit this `ibuffer' session.
Try to restore the previous window configuration if
`ibuffer-restore-window-config-on-quit' is non-nil."
  (interactive)
  (if ibuffer-restore-window-config-on-quit
      (progn
	(bury-buffer)
	(unless (= (count-windows) 1)
	  (set-window-configuration ibuffer-prev-window-config)))
    (bury-buffer)))

;;;###autoload
(defun ibuffer-list-buffers (&optional files-only)
  "Display a list of buffers, in another window.
If optional argument FILES-ONLY is non-nil, then add a filter for
buffers which are visiting a file."
  (interactive "P")
  (ibuffer t nil (when files-only
		   '((filename . ".*"))) t))

;;;###autoload
(defun ibuffer-other-window (&optional files-only)
  "Like `ibuffer', but displayed in another window by default.
If optional argument FILES-ONLY is non-nil, then add a filter for
buffers which are visiting a file."
  (interactive "P")
  (ibuffer t nil (when files-only
		   '((filename . ".*")))))

;;;###autoload
(defun ibuffer (&optional other-window-p name qualifiers noselect
			  shrink filter-groups formats)
  "Begin using Ibuffer to edit a list of buffers.
Type 'h' after entering ibuffer for more information.

All arguments are optional.
OTHER-WINDOW-P says to use another window.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").
QUALIFIERS is an initial set of filtering qualifiers to use;
  see `ibuffer-filtering-qualifiers'.
NOSELECT means don't select the Ibuffer buffer.
SHRINK means shrink the buffer to minimal size.  The special
  value `onewindow' means always use another window.
FILTER-GROUPS is an initial set of filtering groups to use;
  see `ibuffer-filter-groups'.
FORMATS is the value to use for `ibuffer-formats'.
  If specified, then the variable `ibuffer-formats' will have
  that value locally in this buffer."
  (interactive "P")
  (when ibuffer-use-other-window
    (setq other-window-p t))
  (setq ibuffer-prev-window-config (current-window-configuration))
  (let ((buf (get-buffer-create (or name "*Ibuffer*"))))
    (if other-window-p
	(funcall (if noselect (lambda (buf) (display-buffer buf t)) #'pop-to-buffer) buf)
      (funcall (if noselect #'display-buffer #'switch-to-buffer) buf))
    (with-current-buffer buf
      (save-selected-window
	;; We switch to the buffer's window in order to be able
	;; to modify the value of point
	(select-window (get-buffer-window buf 0))
	(or (eq major-mode 'ibuffer-mode)
	    (ibuffer-mode))
	(setq ibuffer-restore-window-config-on-quit other-window-p)
	(when shrink
	  (setq ibuffer-shrink-to-minimum-size shrink))
	(when qualifiers
	  (require 'ibuf-ext)
	  (setq ibuffer-filtering-qualifiers qualifiers))
	(when filter-groups
	  (require 'ibuf-ext)
	  (setq ibuffer-filter-groups filter-groups))
	(when formats
	  (set (make-local-variable 'ibuffer-formats) formats))
	(ibuffer-update nil)
	;; Skip the group name by default.
	(ibuffer-forward-line 0 t)
	(unwind-protect
	    (progn
	      (setq buffer-read-only nil)
	      (run-hooks 'ibuffer-hook))
	  (setq buffer-read-only t))
	(unless ibuffer-expert
	  (message "Commands: m, u, t, RET, g, k, S, D, Q; q to quit; h for help"))))))

(put 'ibuffer-mode 'mode-class 'special)
(defun ibuffer-mode ()
  "A major mode for viewing a list of buffers.
In Ibuffer, you can conveniently perform many operations on the
currently open buffers, in addition to filtering your view to a
particular subset of them, and sorting by various criteria.

Operations on marked buffers:
\\<ibuffer-mode-map>
  '\\[ibuffer-do-save]' - Save the marked buffers
  '\\[ibuffer-do-view]' - View the marked buffers in this frame.
  '\\[ibuffer-do-view-other-frame]' - View the marked buffers in another frame.
  '\\[ibuffer-do-revert]' - Revert the marked buffers.
  '\\[ibuffer-do-toggle-read-only]' - Toggle read-only state of marked buffers.
  '\\[ibuffer-do-delete]' - Kill the marked buffers.
  '\\[ibuffer-do-isearch]' - Do incremental search in the marked buffers.
  '\\[ibuffer-do-isearch-regexp]' - Isearch for regexp in the marked buffers.
  '\\[ibuffer-do-replace-regexp]' - Replace by regexp in each of the marked
          buffers.
  '\\[ibuffer-do-query-replace]' - Query replace in each of the marked buffers.
  '\\[ibuffer-do-query-replace-regexp]' - As above, with a regular expression.
  '\\[ibuffer-do-print]' - Print the marked buffers.
  '\\[ibuffer-do-occur]' - List lines in all marked buffers which match
          a given regexp (like the function `occur').
  '\\[ibuffer-do-shell-command-pipe]' - Pipe the contents of the marked
          buffers to a shell command.
  '\\[ibuffer-do-shell-command-pipe-replace]' - Replace the contents of the marked
          buffers with the output of a shell command.
  '\\[ibuffer-do-shell-command-file]' - Run a shell command with the
          buffer's file as an argument.
  '\\[ibuffer-do-eval]' - Evaluate a form in each of the marked buffers.  This
          is a very flexible command.  For example, if you want to make all
          of the marked buffers read only, try using (toggle-read-only 1) as
          the input form.
  '\\[ibuffer-do-view-and-eval]' - As above, but view each buffer while the form
          is evaluated.
  '\\[ibuffer-do-kill-lines]' - Remove the marked lines from the *Ibuffer* buffer,
          but don't kill the associated buffer.
  '\\[ibuffer-do-kill-on-deletion-marks]' - Kill all buffers marked for deletion.

Marking commands:

  '\\[ibuffer-mark-forward]' - Mark the buffer at point.
  '\\[ibuffer-toggle-marks]' - Unmark all currently marked buffers, and mark
          all unmarked buffers.
  '\\[ibuffer-unmark-forward]' - Unmark the buffer at point.
  '\\[ibuffer-unmark-backward]' - Unmark the buffer at point, and move to the
          previous line.
  '\\[ibuffer-unmark-all]' - Unmark all marked buffers.
  '\\[ibuffer-mark-by-mode]' - Mark buffers by major mode.
  '\\[ibuffer-mark-unsaved-buffers]' - Mark all \"unsaved\" buffers.
          This means that the buffer is modified, and has an associated file.
  '\\[ibuffer-mark-modified-buffers]' - Mark all modified buffers,
          regardless of whether or not they have an associated file.
  '\\[ibuffer-mark-special-buffers]' - Mark all buffers whose name begins and
          ends with '*'.
  '\\[ibuffer-mark-dissociated-buffers]' - Mark all buffers which have
          an associated file, but that file doesn't currently exist.
  '\\[ibuffer-mark-read-only-buffers]' - Mark all read-only buffers.
  '\\[ibuffer-mark-dired-buffers]' - Mark buffers in `dired' mode.
  '\\[ibuffer-mark-help-buffers]' - Mark buffers in `help-mode', `apropos-mode', etc.
  '\\[ibuffer-mark-old-buffers]' - Mark buffers older than `ibuffer-old-time'.
  '\\[ibuffer-mark-for-delete]' - Mark the buffer at point for deletion.
  '\\[ibuffer-mark-by-name-regexp]' - Mark buffers by their name, using a regexp.
  '\\[ibuffer-mark-by-mode-regexp]' - Mark buffers by their major mode, using a regexp.
  '\\[ibuffer-mark-by-file-name-regexp]' - Mark buffers by their filename, using a regexp.

Filtering commands:

  '\\[ibuffer-filter-by-mode]' - Add a filter by major mode.
  '\\[ibuffer-filter-by-used-mode]' - Add a filter by major mode now in use.
  '\\[ibuffer-filter-by-name]' - Add a filter by buffer name.
  '\\[ibuffer-filter-by-content]' - Add a filter by buffer content.
  '\\[ibuffer-filter-by-filename]' - Add a filter by filename.
  '\\[ibuffer-filter-by-size-gt]' - Add a filter by buffer size.
  '\\[ibuffer-filter-by-size-lt]' - Add a filter by buffer size.
  '\\[ibuffer-filter-by-predicate]' - Add a filter by an arbitrary Lisp predicate.
  '\\[ibuffer-save-filters]' - Save the current filters with a name.
  '\\[ibuffer-switch-to-saved-filters]' - Switch to previously saved filters.
  '\\[ibuffer-add-saved-filters]' - Add saved filters to current filters.
  '\\[ibuffer-or-filter]' - Replace the top two filters with their logical OR.
  '\\[ibuffer-pop-filter]' - Remove the top filter.
  '\\[ibuffer-negate-filter]' - Invert the logical sense of the top filter.
  '\\[ibuffer-decompose-filter]' - Break down the topmost filter.
  '\\[ibuffer-filter-disable]' - Remove all filtering currently in effect.

Filter group commands:

  '\\[ibuffer-filters-to-filter-group]' - Create filter group from filters.
  '\\[ibuffer-pop-filter-group]' - Remove top filter group.
  '\\[ibuffer-forward-filter-group]' - Move to the next filter group.
  '\\[ibuffer-backward-filter-group]' - Move to the previous filter group.
  '\\[ibuffer-clear-filter-groups]' - Remove all active filter groups.
  '\\[ibuffer-save-filter-groups]' - Save the current groups with a name.
  '\\[ibuffer-switch-to-saved-filter-groups]' - Restore previously saved groups.
  '\\[ibuffer-delete-saved-filter-groups]' - Delete previously saved groups.

Sorting commands:

  '\\[ibuffer-toggle-sorting-mode]' - Rotate between the various sorting modes.
  '\\[ibuffer-invert-sorting]' - Reverse the current sorting order.
  '\\[ibuffer-do-sort-by-alphabetic]' - Sort the buffers lexicographically.
  '\\[ibuffer-do-sort-by-filename/process]' - Sort the buffers by the file name.
  '\\[ibuffer-do-sort-by-recency]' - Sort the buffers by last viewing time.
  '\\[ibuffer-do-sort-by-size]' - Sort the buffers by size.
  '\\[ibuffer-do-sort-by-major-mode]' - Sort the buffers by major mode.

Other commands:

  '\\[ibuffer-update]' - Regenerate the list of all buffers.
          Prefix arg means to toggle whether buffers that match
          `ibuffer-maybe-show-predicates' should be displayed.

  '\\[ibuffer-switch-format]' - Change the current display format.
  '\\[forward-line]' - Move point to the next line.
  '\\[previous-line]' - Move point to the previous line.
  '\\[ibuffer-quit]' - Bury the Ibuffer buffer.
  '\\[describe-mode]' - This help.
  '\\[ibuffer-diff-with-file]' - View the differences between this buffer
          and its associated file.
  '\\[ibuffer-visit-buffer]' - View the buffer on this line.
  '\\[ibuffer-visit-buffer-other-window]' - As above, but in another window.
  '\\[ibuffer-visit-buffer-other-window-noselect]' - As both above, but don't select
          the new window.
  '\\[ibuffer-bury-buffer]' - Bury (not kill!) the buffer on this line.

** Information on Filtering:

 You can filter your ibuffer view via different criteria.  Each Ibuffer
buffer has its own stack of active filters.  For example, suppose you
are working on an Emacs Lisp project.  You can create an Ibuffer
buffer displays buffers in just `emacs-lisp' modes via
'\\[ibuffer-filter-by-mode] emacs-lisp-mode RET'.  In this case, there
is just one entry on the filtering stack.

You can also combine filters.  The various filtering commands push a
new filter onto the stack, and the filters combine to show just
buffers which satisfy ALL criteria on the stack.  For example, suppose
you only want to see buffers in `emacs-lisp' mode, whose names begin
with \"gnus\".  You can accomplish this via:
'\\[ibuffer-filter-by-mode] emacs-lisp-mode RET
\\[ibuffer-filter-by-name] ^gnus RET'.

Additionally, you can OR the top two filters together with
'\\[ibuffer-or-filters]'.  To see all buffers in either
`emacs-lisp-mode' or `lisp-interaction-mode', type:

'\\[ibuffer-filter-by-mode] emacs-lisp-mode RET \\[ibuffer-filter-by-mode] lisp-interaction-mode RET \\[ibuffer-or-filters]'.

Filters can also be saved and restored using mnemonic names: see the
functions `ibuffer-save-filters' and `ibuffer-switch-to-saved-filters'.

To remove the top filter on the stack, use '\\[ibuffer-pop-filter]', and
to disable all filtering currently in effect, use
'\\[ibuffer-filter-disable]'.

** Filter Groups:

Once one has mastered filters, the next logical step up is \"filter
groups\".  A filter group is basically a named group of buffers which
match a filter, which are displayed together in an Ibuffer buffer.  To
create a filter group, simply use the regular functions to create a
filter, and then type '\\[ibuffer-filters-to-filter-group]'.

A quick example will make things clearer.  Suppose that one wants to
group all of one's Emacs Lisp buffers together.  To do this, type

'\\[ibuffer-filter-by-mode] emacs-lisp-mode RET \\[ibuffer-filters-to-filter-group] RET emacs lisp buffers RET'

You may, of course, name the group whatever you want; it doesn't have
to be \"emacs lisp buffers\".  Filter groups may be composed of any
arbitrary combination of filters.

Just like filters themselves, filter groups act as a stack.  Buffers
will not be displayed multiple times if they would be included in
multiple filter groups; instead, the first filter group is used.  The
filter groups are displayed in this order of precedence.

You may rearrange filter groups by using the regular
'\\[ibuffer-kill-line]' and '\\[ibuffer-yank]' pair.  Yanked groups
will be inserted before the group at point."
  (kill-all-local-variables)
  (use-local-map ibuffer-mode-map)
  (setq major-mode 'ibuffer-mode)
  (setq mode-name "Ibuffer")
  ;; Include state info next to the mode name.
  (set (make-local-variable 'mode-line-process)
        '(" by "
          (ibuffer-sorting-mode (:eval (symbol-name ibuffer-sorting-mode))
                                "view time")
          (ibuffer-sorting-reversep " [rev]")
          (ibuffer-auto-mode " (Auto)")
          ;; Only list the filters if they're not already in the header-line.
          (header-line-format
           ""
           (:eval (if (functionp 'ibuffer-format-qualifier)
                      (mapconcat 'ibuffer-format-qualifier
                                 ibuffer-filtering-qualifiers ""))))))
  ;; `ibuffer-update' puts this on header-line-format when needed.
  (setq ibuffer-header-line-format
        ;; Display the part that won't be in the mode-line.
        (list* "" mode-name
               (mapcar (lambda (elem)
                         (if (eq (car-safe elem) 'header-line-format)
                             (nth 2 elem) elem))
                       mode-line-process)))

  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq truncate-lines ibuffer-truncate-lines)
  ;; This makes things less ugly for Emacs 21 users with a non-nil
  ;; `show-trailing-whitespace'.
  (setq show-trailing-whitespace nil)
  ;; disable `show-paren-mode' buffer-locally
  (if (bound-and-true-p show-paren-mode)
      (set (make-local-variable 'show-paren-mode) nil))
  (set (make-local-variable 'revert-buffer-function)
       #'ibuffer-update)
  (set (make-local-variable 'ibuffer-sorting-mode)
       ibuffer-default-sorting-mode)
  (set (make-local-variable 'ibuffer-sorting-reversep)
       ibuffer-default-sorting-reversep)
  (set (make-local-variable 'ibuffer-shrink-to-minimum-size)
       ibuffer-default-shrink-to-minimum-size)
  (set (make-local-variable 'ibuffer-display-maybe-show-predicates)
       ibuffer-default-display-maybe-show-predicates)
  (set (make-local-variable 'ibuffer-filtering-qualifiers) nil)
  (set (make-local-variable 'ibuffer-filter-groups) nil)
  (set (make-local-variable 'ibuffer-filter-group-kill-ring) nil)
  (set (make-local-variable 'ibuffer-hidden-filter-groups) nil)
  (set (make-local-variable 'ibuffer-compiled-formats) nil)
  (set (make-local-variable 'ibuffer-cached-formats) nil)
  (set (make-local-variable 'ibuffer-cached-eliding-string) nil)
  (set (make-local-variable 'ibuffer-cached-elide-long-columns) nil)
  (set (make-local-variable 'ibuffer-current-format) nil)
  (set (make-local-variable 'ibuffer-restore-window-config-on-quit) nil)
  (set (make-local-variable 'ibuffer-did-modification) nil)
  (set (make-local-variable 'ibuffer-tmp-hide-regexps) nil)
  (set (make-local-variable 'ibuffer-tmp-show-regexps) nil)
  (define-key ibuffer-mode-map [menu-bar edit] 'undefined)
  (define-key ibuffer-mode-map [menu-bar operate] (cons "Operate" ibuffer-mode-operate-map))
  (ibuffer-update-format)
  (when ibuffer-default-directory
    (setq default-directory ibuffer-default-directory))
  (add-hook 'change-major-mode-hook 'font-lock-defontify nil t)
  (run-mode-hooks 'ibuffer-mode-hook))


;;; Start of automatically extracted autoloads.

;;;### (autoloads (ibuffer-do-occur ibuffer-mark-dired-buffers ibuffer-mark-read-only-buffers
;;;;;;  ibuffer-mark-special-buffers ibuffer-mark-old-buffers ibuffer-mark-compressed-file-buffers
;;;;;;  ibuffer-mark-help-buffers ibuffer-mark-dissociated-buffers
;;;;;;  ibuffer-mark-unsaved-buffers ibuffer-mark-modified-buffers
;;;;;;  ibuffer-mark-by-mode ibuffer-mark-by-file-name-regexp ibuffer-mark-by-mode-regexp
;;;;;;  ibuffer-mark-by-name-regexp ibuffer-copy-filename-as-kill
;;;;;;  ibuffer-diff-with-file ibuffer-jump-to-buffer ibuffer-do-kill-lines
;;;;;;  ibuffer-backwards-next-marked ibuffer-forward-next-marked
;;;;;;  ibuffer-add-to-tmp-show ibuffer-add-to-tmp-hide ibuffer-bs-show
;;;;;;  ibuffer-invert-sorting ibuffer-toggle-sorting-mode ibuffer-switch-to-saved-filters
;;;;;;  ibuffer-add-saved-filters ibuffer-delete-saved-filters ibuffer-save-filters
;;;;;;  ibuffer-or-filter ibuffer-negate-filter ibuffer-exchange-filters
;;;;;;  ibuffer-decompose-filter ibuffer-pop-filter ibuffer-filter-disable
;;;;;;  ibuffer-switch-to-saved-filter-groups ibuffer-delete-saved-filter-groups
;;;;;;  ibuffer-save-filter-groups ibuffer-yank-filter-group ibuffer-yank
;;;;;;  ibuffer-kill-line ibuffer-kill-filter-group ibuffer-jump-to-filter-group
;;;;;;  ibuffer-clear-filter-groups ibuffer-decompose-filter-group
;;;;;;  ibuffer-pop-filter-group ibuffer-set-filter-groups-by-mode
;;;;;;  ibuffer-filters-to-filter-group ibuffer-included-in-filters-p
;;;;;;  ibuffer-backward-filter-group ibuffer-forward-filter-group
;;;;;;  ibuffer-toggle-filter-group ibuffer-mouse-toggle-filter-group
;;;;;;  ibuffer-interactive-filter-by-mode ibuffer-mouse-filter-by-mode
;;;;;;  ibuffer-auto-mode) "ibuf-ext" "ibuf-ext.el" "1400db1bc3d4a3010cbc4807a6725072")
;;; Generated autoloads from ibuf-ext.el

(autoload 'ibuffer-auto-mode "ibuf-ext" "\
Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode).
With a prefix argument ARG, enable Ibuffer Auto mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

\(fn &optional ARG)" t nil)

(autoload 'ibuffer-mouse-filter-by-mode "ibuf-ext" "\
Enable or disable filtering by the major mode chosen via mouse.

\(fn EVENT)" t nil)

(autoload 'ibuffer-interactive-filter-by-mode "ibuf-ext" "\
Enable or disable filtering by the major mode at point.

\(fn EVENT-OR-POINT)" t nil)

(autoload 'ibuffer-mouse-toggle-filter-group "ibuf-ext" "\
Toggle the display status of the filter group chosen with the mouse.

\(fn EVENT)" t nil)

(autoload 'ibuffer-toggle-filter-group "ibuf-ext" "\
Toggle the display status of the filter group on this line.

\(fn)" t nil)

(autoload 'ibuffer-forward-filter-group "ibuf-ext" "\
Move point forwards by COUNT filtering groups.

\(fn &optional COUNT)" t nil)

(autoload 'ibuffer-backward-filter-group "ibuf-ext" "\
Move point backwards by COUNT filtering groups.

\(fn &optional COUNT)" t nil)
 (autoload 'ibuffer-do-shell-command-pipe "ibuf-ext")
 (autoload 'ibuffer-do-shell-command-pipe-replace "ibuf-ext")
 (autoload 'ibuffer-do-shell-command-file "ibuf-ext")
 (autoload 'ibuffer-do-eval "ibuf-ext")
 (autoload 'ibuffer-do-view-and-eval "ibuf-ext")
 (autoload 'ibuffer-do-rename-uniquely "ibuf-ext")
 (autoload 'ibuffer-do-revert "ibuf-ext")
 (autoload 'ibuffer-do-isearch "ibuf-ext")
 (autoload 'ibuffer-do-isearch-regexp "ibuf-ext")
 (autoload 'ibuffer-do-replace-regexp "ibuf-ext")
 (autoload 'ibuffer-do-query-replace "ibuf-ext")
 (autoload 'ibuffer-do-query-replace-regexp "ibuf-ext")
 (autoload 'ibuffer-do-print "ibuf-ext")

(autoload 'ibuffer-included-in-filters-p "ibuf-ext" "\


\(fn BUF FILTERS)" nil nil)

(autoload 'ibuffer-filters-to-filter-group "ibuf-ext" "\
Make the current filters into a filtering group.

\(fn NAME)" t nil)

(autoload 'ibuffer-set-filter-groups-by-mode "ibuf-ext" "\
Set the current filter groups to filter by mode.

\(fn)" t nil)

(autoload 'ibuffer-pop-filter-group "ibuf-ext" "\
Remove the first filter group.

\(fn)" t nil)

(autoload 'ibuffer-decompose-filter-group "ibuf-ext" "\
Decompose the filter group GROUP into active filters.

\(fn GROUP)" t nil)

(autoload 'ibuffer-clear-filter-groups "ibuf-ext" "\
Remove all filter groups.

\(fn)" t nil)

(autoload 'ibuffer-jump-to-filter-group "ibuf-ext" "\
Move point to the filter group whose name is NAME.

\(fn NAME)" t nil)

(autoload 'ibuffer-kill-filter-group "ibuf-ext" "\
Kill the filter group named NAME.
The group will be added to `ibuffer-filter-group-kill-ring'.

\(fn NAME)" t nil)

(autoload 'ibuffer-kill-line "ibuf-ext" "\
Kill the filter group at point.
See also `ibuffer-kill-filter-group'.

\(fn &optional ARG INTERACTIVE-P)" t nil)

(autoload 'ibuffer-yank "ibuf-ext" "\
Yank the last killed filter group before group at point.

\(fn)" t nil)

(autoload 'ibuffer-yank-filter-group "ibuf-ext" "\
Yank the last killed filter group before group named NAME.

\(fn NAME)" t nil)

(autoload 'ibuffer-save-filter-groups "ibuf-ext" "\
Save all active filter groups GROUPS as NAME.
They are added to `ibuffer-saved-filter-groups'.  Interactively,
prompt for NAME, and use the current filters.

\(fn NAME GROUPS)" t nil)

(autoload 'ibuffer-delete-saved-filter-groups "ibuf-ext" "\
Delete saved filter groups with NAME.
They are removed from `ibuffer-saved-filter-groups'.

\(fn NAME)" t nil)

(autoload 'ibuffer-switch-to-saved-filter-groups "ibuf-ext" "\
Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used.

\(fn NAME)" t nil)

(autoload 'ibuffer-filter-disable "ibuf-ext" "\
Disable all filters currently in effect in this buffer.
With optional arg DELETE-FILTER-GROUPS non-nil, delete all filter
group definitions by setting `ibuffer-filter-groups' to nil.

\(fn &optional DELETE-FILTER-GROUPS)" t nil)

(autoload 'ibuffer-pop-filter "ibuf-ext" "\
Remove the top filter in this buffer.

\(fn)" t nil)

(autoload 'ibuffer-decompose-filter "ibuf-ext" "\
Separate the top compound filter (OR, NOT, or SAVED) in this buffer.

This means that the topmost filter on the filtering stack, which must
be a complex filter like (OR [name: foo] [mode: bar-mode]), will be
turned into two separate filters [name: foo] and [mode: bar-mode].

\(fn)" t nil)

(autoload 'ibuffer-exchange-filters "ibuf-ext" "\
Exchange the top two filters on the stack in this buffer.

\(fn)" t nil)

(autoload 'ibuffer-negate-filter "ibuf-ext" "\
Negate the sense of the top filter in the current buffer.

\(fn)" t nil)

(autoload 'ibuffer-or-filter "ibuf-ext" "\
Replace the top two filters in this buffer with their logical OR.
If optional argument REVERSE is non-nil, instead break the top OR
filter into parts.

\(fn &optional REVERSE)" t nil)

(autoload 'ibuffer-save-filters "ibuf-ext" "\
Save FILTERS in this buffer with name NAME in `ibuffer-saved-filters'.
Interactively, prompt for NAME, and use the current filters.

\(fn NAME FILTERS)" t nil)

(autoload 'ibuffer-delete-saved-filters "ibuf-ext" "\
Delete saved filters with NAME from `ibuffer-saved-filters'.

\(fn NAME)" t nil)

(autoload 'ibuffer-add-saved-filters "ibuf-ext" "\
Add saved filters from `ibuffer-saved-filters' to this buffer's filters.

\(fn NAME)" t nil)

(autoload 'ibuffer-switch-to-saved-filters "ibuf-ext" "\
Set this buffer's filters to filters with NAME from `ibuffer-saved-filters'.

\(fn NAME)" t nil)
 (autoload 'ibuffer-filter-by-mode "ibuf-ext")
 (autoload 'ibuffer-filter-by-used-mode "ibuf-ext")
 (autoload 'ibuffer-filter-by-name "ibuf-ext")
 (autoload 'ibuffer-filter-by-filename "ibuf-ext")
 (autoload 'ibuffer-filter-by-size-gt  "ibuf-ext")
 (autoload 'ibuffer-filter-by-size-lt  "ibuf-ext")
 (autoload 'ibuffer-filter-by-content "ibuf-ext")
 (autoload 'ibuffer-filter-by-predicate "ibuf-ext")

(autoload 'ibuffer-toggle-sorting-mode "ibuf-ext" "\
Toggle the current sorting mode.
Default sorting modes are:
 Recency - the last time the buffer was viewed
 Name - the name of the buffer
 Major Mode - the name of the major mode of the buffer
 Size - the size of the buffer

\(fn)" t nil)

(autoload 'ibuffer-invert-sorting "ibuf-ext" "\
Toggle whether or not sorting is in reverse order.

\(fn)" t nil)
 (autoload 'ibuffer-do-sort-by-major-mode "ibuf-ext")
 (autoload 'ibuffer-do-sort-by-mode-name "ibuf-ext")
 (autoload 'ibuffer-do-sort-by-alphabetic "ibuf-ext")
 (autoload 'ibuffer-do-sort-by-size "ibuf-ext")
 (autoload 'ibuffer-do-sort-by-filename/process "ibuf-ext")

(autoload 'ibuffer-bs-show "ibuf-ext" "\
Emulate `bs-show' from the bs.el package.

\(fn)" t nil)

(autoload 'ibuffer-add-to-tmp-hide "ibuf-ext" "\
Add REGEXP to `ibuffer-tmp-hide-regexps'.
This means that buffers whose name matches REGEXP will not be shown
for this Ibuffer session.

\(fn REGEXP)" t nil)

(autoload 'ibuffer-add-to-tmp-show "ibuf-ext" "\
Add REGEXP to `ibuffer-tmp-show-regexps'.
This means that buffers whose name matches REGEXP will always be shown
for this Ibuffer session.

\(fn REGEXP)" t nil)

(autoload 'ibuffer-forward-next-marked "ibuf-ext" "\
Move forward by COUNT marked buffers (default 1).

If MARK is non-nil, it should be a character denoting the type of mark
to move by.  The default is `ibuffer-marked-char'.

If DIRECTION is non-nil, it should be an integer; negative integers
mean move backwards, non-negative integers mean move forwards.

\(fn &optional COUNT MARK DIRECTION)" t nil)

(autoload 'ibuffer-backwards-next-marked "ibuf-ext" "\
Move backwards by COUNT marked buffers (default 1).

If MARK is non-nil, it should be a character denoting the type of mark
to move by.  The default is `ibuffer-marked-char'.

\(fn &optional COUNT MARK)" t nil)

(autoload 'ibuffer-do-kill-lines "ibuf-ext" "\
Hide all of the currently marked lines.

\(fn)" t nil)

(autoload 'ibuffer-jump-to-buffer "ibuf-ext" "\
Move point to the buffer whose name is NAME.

If called interactively, prompt for a buffer name and go to the
corresponding line in the Ibuffer buffer.  If said buffer is in a
hidden group filter, open it.

If `ibuffer-jump-offer-only-visible-buffers' is non-nil, only offer
visible buffers in the completion list.  Calling the command with
a prefix argument reverses the meaning of that variable.

\(fn NAME)" t nil)

(autoload 'ibuffer-diff-with-file "ibuf-ext" "\
View the differences between marked buffers and their associated files.
If no buffers are marked, use buffer at point.
This requires the external program \"diff\" to be in your `exec-path'.

\(fn)" t nil)

(autoload 'ibuffer-copy-filename-as-kill "ibuf-ext" "\
Copy filenames of marked buffers into the kill ring.

The names are separated by a space.
If a buffer has no filename, it is ignored.

With no prefix arg, use the filename sans its directory of each marked file.
With a zero prefix arg, use the complete filename of each marked file.
With \\[universal-argument], use the filename of each marked file relative
to `ibuffer-default-directory' if non-nil, otherwise `default-directory'.

You can then feed the file name(s) to other commands with \\[yank].

\(fn &optional ARG)" t nil)

(autoload 'ibuffer-mark-by-name-regexp "ibuf-ext" "\
Mark all buffers whose name matches REGEXP.

\(fn REGEXP)" t nil)

(autoload 'ibuffer-mark-by-mode-regexp "ibuf-ext" "\
Mark all buffers whose major mode matches REGEXP.

\(fn REGEXP)" t nil)

(autoload 'ibuffer-mark-by-file-name-regexp "ibuf-ext" "\
Mark all buffers whose file name matches REGEXP.

\(fn REGEXP)" t nil)

(autoload 'ibuffer-mark-by-mode "ibuf-ext" "\
Mark all buffers whose major mode equals MODE.

\(fn MODE)" t nil)

(autoload 'ibuffer-mark-modified-buffers "ibuf-ext" "\
Mark all modified buffers.

\(fn)" t nil)

(autoload 'ibuffer-mark-unsaved-buffers "ibuf-ext" "\
Mark all modified buffers that have an associated file.

\(fn)" t nil)

(autoload 'ibuffer-mark-dissociated-buffers "ibuf-ext" "\
Mark all buffers whose associated file does not exist.

\(fn)" t nil)

(autoload 'ibuffer-mark-help-buffers "ibuf-ext" "\
Mark buffers like *Help*, *Apropos*, *Info*.

\(fn)" t nil)

(autoload 'ibuffer-mark-compressed-file-buffers "ibuf-ext" "\
Mark buffers whose associated file is compressed.

\(fn)" t nil)

(autoload 'ibuffer-mark-old-buffers "ibuf-ext" "\
Mark buffers which have not been viewed in `ibuffer-old-time' hours.

\(fn)" t nil)

(autoload 'ibuffer-mark-special-buffers "ibuf-ext" "\
Mark all buffers whose name begins and ends with '*'.

\(fn)" t nil)

(autoload 'ibuffer-mark-read-only-buffers "ibuf-ext" "\
Mark all read-only buffers.

\(fn)" t nil)

(autoload 'ibuffer-mark-dired-buffers "ibuf-ext" "\
Mark all `dired' buffers.

\(fn)" t nil)

(autoload 'ibuffer-do-occur "ibuf-ext" "\
View lines which match REGEXP in all marked buffers.
Optional argument NLINES says how many lines of context to display: it
defaults to one.

\(fn REGEXP &optional NLINES)" t nil)

;;;***

;;; End of automatically extracted autoloads.


(provide 'ibuffer)

(run-hooks 'ibuffer-load-hook)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; ibuffer.el ends here
