;;; ediff-init.el --- Macros, variables, and defsubsts used by Ediff

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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

;;; Code:

;; Start compiler pacifier
(defvar ediff-metajob-name)
(defvar ediff-meta-buffer)
(defvar ediff-grab-mouse)
(defvar ediff-mouse-pixel-position)
(defvar ediff-mouse-pixel-threshold)
(defvar ediff-whitespace)
(defvar ediff-multiframe)
(defvar ediff-use-toolbar-p)
(defvar mswindowsx-bitmap-file-path)
;; end pacifier

(defvar ediff-force-faces nil
  "If t, Ediff will think that it is running on a display that supports faces.
This is provided as a temporary relief for users of face-capable displays
that Ediff doesn't know about.")

;; Are we running as a window application or on a TTY?
(defsubst ediff-device-type ()
  (if (featurep 'xemacs)
      (device-type (selected-device))
    window-system))

;; in XEmacs: device-type is tty on tty and stream in batch.
(defun ediff-window-display-p ()
  (and (ediff-device-type) (not (memq (ediff-device-type) '(tty pc stream)))))

;; test if supports faces
(defun ediff-has-face-support-p ()
  (cond ((ediff-window-display-p))
	(ediff-force-faces)
	((ediff-color-display-p))
	((featurep 'emacs) (memq (ediff-device-type) '(pc)))
	((featurep 'xemacs) (memq (ediff-device-type) '(tty pc)))
	))

;; toolbar support for emacs hasn't been implemented in ediff
(defun ediff-has-toolbar-support-p ()
  (if (featurep 'xemacs)
      (if (featurep 'toolbar) (console-on-window-system-p))))


(defun ediff-has-gutter-support-p ()
  (if (featurep 'xemacs)
      (if (featurep 'gutter) (console-on-window-system-p))))

(defun ediff-use-toolbar-p ()
  (and (ediff-has-toolbar-support-p)	;Can it do it ?
       (boundp 'ediff-use-toolbar-p)
       ediff-use-toolbar-p))		;Does the user want it ?

;; Defines VAR as an advertised local variable.
;; Performs a defvar, then executes `make-variable-buffer-local' on
;; the variable.  Also sets the `permanent-local' property,
;; so that `kill-all-local-variables' (called by major-mode setting
;; commands) won't destroy Ediff control variables.
;;
;; Plagiarized from `emerge-defvar-local' for XEmacs.
(defmacro ediff-defvar-local (var value doc)
  "Defines VAR as a local variable."
  (declare (indent defun))
  `(progn
     (defvar ,var ,value ,doc)
     (make-variable-buffer-local ',var)
     (put ',var 'permanent-local t)))



;; Variables that control each Ediff session---local to the control buffer.

;; Mode variables
;; The buffer in which the A variant is stored.
(ediff-defvar-local ediff-buffer-A nil "")
;; The buffer in which the B variant is stored.
(ediff-defvar-local ediff-buffer-B nil "")
;; The buffer in which the C variant is stored or where the merge buffer lives.
(ediff-defvar-local ediff-buffer-C nil "")
;; Ancestor buffer
(ediff-defvar-local ediff-ancestor-buffer nil "")
;; The Ediff control buffer
(ediff-defvar-local ediff-control-buffer nil "")

(ediff-defvar-local ediff-temp-indirect-buffer nil
  "If t, the buffer is a temporary indirect buffer.
It needs to be killed when we quit the session.")


;; Association between buff-type and ediff-buffer-*
(defconst ediff-buffer-alist
  '((?A . ediff-buffer-A)
    (?B . ediff-buffer-B)
    (?C . ediff-buffer-C)))

;;; Macros
(defmacro ediff-odd-p (arg)
  `(eq (logand ,arg 1) 1))

(defmacro ediff-buffer-live-p (buf)
  `(and ,buf (get-buffer ,buf) (buffer-name (get-buffer ,buf))))

(defmacro ediff-get-buffer (arg)
  `(cond ((eq ,arg 'A) ediff-buffer-A)
	 ((eq ,arg 'B) ediff-buffer-B)
	 ((eq ,arg 'C) ediff-buffer-C)
	 ((eq ,arg 'Ancestor) ediff-ancestor-buffer)
	 ))

(defmacro ediff-get-value-according-to-buffer-type (buf-type list)
  `(cond ((eq ,buf-type 'A) (nth 0 ,list))
	 ((eq ,buf-type 'B) (nth 1 ,list))
	 ((eq ,buf-type 'C) (nth 2 ,list))
	 ))

(defmacro ediff-char-to-buftype (arg)
  `(cond ((memq ,arg '(?a ?A)) 'A)
	 ((memq ,arg '(?b ?B)) 'B)
	 ((memq ,arg '(?c ?C)) 'C)
	 ))


;; A-list is supposed to be of the form (A . symb) (B . symb)...)
;; where the first part of any association is a buffer type and the second is
;; an appropriate symbol.  Given buffer-type, this function returns the
;; symbol.  This is used to avoid using `intern'
(defsubst ediff-get-symbol-from-alist (buf-type alist)
  (cdr (assoc buf-type alist)))

(defconst ediff-difference-vector-alist
  '((A . ediff-difference-vector-A)
    (B . ediff-difference-vector-B)
    (C . ediff-difference-vector-C)
    (Ancestor . ediff-difference-vector-Ancestor)))

(defmacro ediff-get-difference (n buf-type)
  `(aref
    (symbol-value
     (ediff-get-symbol-from-alist
      ,buf-type ediff-difference-vector-alist))
    ,n))

;; Tell if it has been previously determined that the region has
;; no diffs other than the white space and newlines
;; The argument, N, is the diff region number used by Ediff to index the
;; diff vector.  It is 1 less than the number seen by the user.
;; Returns:
;;		t  if the diffs are whitespace in all buffers
;;		'A (in 3-buf comparison only) if there are only whitespace
;;		   diffs in bufs B and C
;;		'B (in 3-buf comparison only) if there are only whitespace
;;		   diffs in bufs A and C
;;		'C (in 3-buf comparison only) if there are only whitespace
;;		   diffs in bufs A and B
;;
;; A Difference Vector has the form:
;; [diff diff diff ...]
;; where each diff has the form:
;; [overlay fine-diff-vector no-fine-diffs-flag state-of-difference]
;; fine-diff-vector is a vector [fine-diff fine-diff fine-diff ...]
;; no-fine-diffs-flag says if there are fine differences.
;; state-of-difference is A, B, C, or nil, indicating which buffer is
;; 	different from the other two (used only in 3-way jobs).
(defmacro ediff-no-fine-diffs-p (n)
  `(aref (ediff-get-difference ,n 'A) 2))

(defmacro ediff-get-diff-overlay-from-diff-record (diff-rec)
  `(aref ,diff-rec 0))

(defmacro ediff-get-diff-overlay (n buf-type)
  `(ediff-get-diff-overlay-from-diff-record
    (ediff-get-difference ,n ,buf-type)))

(defmacro ediff-get-fine-diff-vector-from-diff-record (diff-rec)
  `(aref ,diff-rec 1))

(defmacro ediff-set-fine-diff-vector (n buf-type fine-vec)
  `(aset (ediff-get-difference ,n ,buf-type) 1 ,fine-vec))

(defmacro ediff-get-state-of-diff (n buf-type)
  `(if (ediff-buffer-live-p ediff-buffer-C)
       (aref (ediff-get-difference ,n ,buf-type) 3)))
(defmacro ediff-set-state-of-diff (n buf-type val)
  `(aset (ediff-get-difference ,n ,buf-type) 3 ,val))

(defmacro ediff-get-state-of-merge (n)
  `(if ediff-state-of-merge
       (aref (aref ediff-state-of-merge ,n) 0)))
(defmacro ediff-set-state-of-merge (n val)
  `(if ediff-state-of-merge
       (aset (aref ediff-state-of-merge ,n) 0 ,val)))

(defmacro ediff-get-state-of-ancestor (n)
  `(if ediff-state-of-merge
       (aref (aref ediff-state-of-merge ,n) 1)))

;; if flag is t, puts a mark on diff region saying that
;; the differences are in white space only.  If flag is nil,
;; the region is marked as essential (i.e., differences are
;; not just in the white space and newlines.)
(defmacro ediff-mark-diff-as-space-only (n flag)
  `(aset (ediff-get-difference ,n 'A) 2 ,flag))

(defmacro ediff-get-fine-diff-vector (n buf-type)
  `(ediff-get-fine-diff-vector-from-diff-record
    (ediff-get-difference ,n ,buf-type)))

;; Macro to switch to BUFFER, evaluate BODY, returns to original buffer.
;; Doesn't save the point and mark.
;; This is `with-current-buffer' with the added test for live buffers."
(defmacro ediff-with-current-buffer (buffer &rest body)
  "Evaluates BODY in BUFFER."
  (declare (indent 1) (debug (form body)))
  `(if (ediff-buffer-live-p ,buffer)
       (save-current-buffer
	 (set-buffer ,buffer)
	 ,@body)
     (or (eq this-command 'ediff-quit)
	 (error ediff-KILLED-VITAL-BUFFER))
     ))


(defsubst ediff-multiframe-setup-p ()
  (and (ediff-window-display-p) ediff-multiframe))

(defmacro ediff-narrow-control-frame-p ()
  `(and (ediff-multiframe-setup-p)
	(equal ediff-help-message ediff-brief-message-string)))

(defmacro ediff-3way-comparison-job ()
  `(memq
    ediff-job-name
    '(ediff-files3 ediff-buffers3)))
(ediff-defvar-local ediff-3way-comparison-job nil "")

(defmacro ediff-merge-job ()
  `(memq
    ediff-job-name
    '(ediff-merge-files
      ediff-merge-buffers
      ediff-merge-files-with-ancestor
      ediff-merge-buffers-with-ancestor
      ediff-merge-revisions
      ediff-merge-revisions-with-ancestor)))
(ediff-defvar-local ediff-merge-job nil "")

(defmacro ediff-patch-job ()
  `(eq ediff-job-name 'epatch))

(defmacro ediff-merge-with-ancestor-job ()
  `(memq
    ediff-job-name
    '(ediff-merge-files-with-ancestor
      ediff-merge-buffers-with-ancestor
      ediff-merge-revisions-with-ancestor)))
(ediff-defvar-local ediff-merge-with-ancestor-job nil "")

(defmacro ediff-3way-job ()
  `(or ediff-3way-comparison-job ediff-merge-job))
(ediff-defvar-local ediff-3way-job nil "")

;; A diff3 job is like a 3way job, but ediff-merge doesn't require the use
;; of diff3.
(defmacro ediff-diff3-job ()
  `(or ediff-3way-comparison-job
       ediff-merge-with-ancestor-job))
(ediff-defvar-local ediff-diff3-job nil "")

(defmacro ediff-windows-job ()
  `(memq ediff-job-name '(ediff-windows-wordwise ediff-windows-linewise)))
(ediff-defvar-local ediff-windows-job nil "")

(defmacro ediff-word-mode-job ()
  `(memq ediff-job-name '(ediff-windows-wordwise ediff-regions-wordwise)))
(ediff-defvar-local ediff-word-mode-job nil "")

(defmacro ediff-narrow-job ()
  `(memq ediff-job-name '(ediff-windows-wordwise
			  ediff-regions-wordwise
			  ediff-windows-linewise
			  ediff-regions-linewise)))
(ediff-defvar-local ediff-narrow-job nil "")

;; Note: ediff-merge-directory-revisions-with-ancestor is not treated as an
;; ancestor metajob, since it behaves differently.
(defsubst ediff-ancestor-metajob (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-merge-directories-with-ancestor
	  ediff-merge-filegroups-with-ancestor)))
(defsubst ediff-revision-metajob (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-directory-revisions
	  ediff-merge-directory-revisions
	  ediff-merge-directory-revisions-with-ancestor)))
(defsubst ediff-patch-metajob (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-multifile-patch)))
;; metajob involving only one group of files, such as multipatch or directory
;; revision
(defsubst ediff-one-filegroup-metajob (&optional metajob)
  (or (ediff-revision-metajob metajob)
      (ediff-patch-metajob metajob)
      ;; add more here
      ))
;; jobs suitable for the operation of collecting diffs into a multifile patch
(defsubst ediff-collect-diffs-metajob (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-directories
	  ediff-merge-directories
	  ediff-merge-directories-with-ancestor
	  ediff-directory-revisions
	  ediff-merge-directory-revisions
	  ediff-merge-directory-revisions-with-ancestor
	  ;; add more here
	  )))
(defsubst ediff-merge-metajob (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-merge-directories
	  ediff-merge-directories-with-ancestor
	  ediff-merge-directory-revisions
	  ediff-merge-directory-revisions-with-ancestor
	  ediff-merge-filegroups-with-ancestor
	  ;; add more here
	  )))

(defsubst ediff-metajob3 (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-merge-directories-with-ancestor
	  ediff-merge-filegroups-with-ancestor
	  ediff-directories3
	  ediff-filegroups3)))
(defsubst ediff-comparison-metajob3 (&optional metajob)
  (memq (or metajob ediff-metajob-name)
	'(ediff-directories3 ediff-filegroups3)))

;; with no argument, checks if we are in ediff-control-buffer
;; with argument, checks if we are in ediff-meta-buffer
(defun ediff-in-control-buffer-p (&optional meta-buf-p)
  (and (boundp 'ediff-control-buffer)
       (eq (if meta-buf-p ediff-meta-buffer ediff-control-buffer)
	   (current-buffer))))

(defsubst ediff-barf-if-not-control-buffer (&optional meta-buf-p)
  (or (ediff-in-control-buffer-p meta-buf-p)
      (error "%S: This command runs in Ediff Control Buffer only!"
	     this-command)))

(defgroup ediff-highlighting nil
  "Highlighting of difference regions in Ediff."
  :prefix "ediff-"
  :group 'ediff)

(defgroup ediff-merge nil
  "Merging utilities."
  :prefix "ediff-"
  :group 'ediff)

(defgroup ediff-hook nil
  "Hooks run by Ediff."
  :prefix "ediff-"
  :group 'ediff)

;; Hook variables

(defcustom ediff-before-setup-hook nil
  "Hooks to run before Ediff begins to set up windows and buffers.
This hook can be used to save the previous window config, which can be restored
on ediff-quit or ediff-suspend."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-before-setup-windows-hook nil
  "Hooks to run before Ediff sets its window configuration.
This hook is run every time when Ediff arranges its windows.
This happens each time Ediff detects that the windows were messed up by the
user."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-after-setup-windows-hook nil
  "Hooks to run after Ediff sets its window configuration.
This can be used to set up control window or icon in a desired place."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-before-setup-control-frame-hook nil
  "Hooks run before setting up the frame to display Ediff Control Panel.
Can be used to change control frame parameters to position it where it
is desirable."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-after-setup-control-frame-hook nil
  "Hooks run after setting up the frame to display Ediff Control Panel.
Can be used to move the frame where it is desired."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-startup-hook nil
  "Hooks to run in the control buffer after Ediff has been set up and is ready for the job."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-select-hook nil
  "Hooks to run after a difference has been selected."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-unselect-hook nil
  "Hooks to run after a difference has been unselected."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-prepare-buffer-hook  nil
  "Hooks run after buffers A, B, and C are set up.
For each buffer, the hooks are run with that buffer made current."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-load-hook nil
  "Hook run after Ediff is loaded.  Can be used to change defaults."
  :type 'hook
  :group 'ediff-hook)

(defcustom ediff-mode-hook nil
  "Hook run just after ediff-mode is set up in the control buffer.
This is done before any windows or frames are created.  One can use it to
set local variables that determine how the display looks like."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-keymap-setup-hook nil
  "Hook run just after the default bindings in Ediff keymap are set up."
  :type 'hook
  :group 'ediff-hook)

(defcustom ediff-display-help-hook nil
  "Hooks run after preparing the help message."
  :type 'hook
  :group 'ediff-hook)

(defcustom ediff-suspend-hook nil
  "Hooks to run in the Ediff control buffer when Ediff is suspended."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-quit-hook nil
  "Hooks to run in the Ediff control buffer after finishing Ediff."
  :type 'hook
  :group 'ediff-hook)
(defcustom ediff-cleanup-hook nil
  "Hooks to run on exiting Ediff but before killing the control and variant buffers."
  :type 'hook
  :group 'ediff-hook)

;; Error messages
(defconst ediff-KILLED-VITAL-BUFFER
  "You have killed a vital Ediff buffer---you must leave Ediff now!")
(defconst ediff-NO-DIFFERENCES
  "Sorry, comparison of identical variants is not what I am made for...")
(defconst ediff-BAD-DIFF-NUMBER
  ;; %S stands for this-command, %d - diff number, %d - max diff
  "%S: Bad diff region number, %d.  Valid numbers are 1 to %d")
(defconst ediff-BAD-INFO (format "
*** The Info file for Ediff, a part of the standard distribution
*** of %sEmacs, does not seem to be properly installed.
***
*** Please contact your system administrator. "
				 (if (featurep 'xemacs) "X" "")))

;; Selective browsing

(ediff-defvar-local ediff-skip-diff-region-function 'ediff-show-all-diffs
  "Function that determines the next/previous diff region to show.
Should return t for regions to be ignored and nil otherwise.
This function gets a region number as an argument.  The region number
is the one used internally by Ediff.  It is 1 less than the number seen
by the user.")

(ediff-defvar-local ediff-hide-regexp-matches-function
  'ediff-hide-regexp-matches
  "Function to use in determining which regions to hide.
See the documentation string of `ediff-hide-regexp-matches' for details.")
(ediff-defvar-local ediff-focus-on-regexp-matches-function
  'ediff-focus-on-regexp-matches
  "Function to use in determining which regions to focus on.
See the documentation string of `ediff-focus-on-regexp-matches' for details.")

;; Regexp that determines buf A regions to focus on when skipping to diff
(ediff-defvar-local ediff-regexp-focus-A "" "")
;; Regexp that determines buf B regions to focus on when skipping to diff
(ediff-defvar-local ediff-regexp-focus-B "" "")
;; Regexp that determines buf C regions to focus on when skipping to diff
(ediff-defvar-local ediff-regexp-focus-C "" "")
;; connective that determines whether to focus regions that match both or
;; one of the regexps
(ediff-defvar-local ediff-focus-regexp-connective 'and "")

;; Regexp that determines buf A regions to ignore when skipping to diff
(ediff-defvar-local ediff-regexp-hide-A "" "")
;; Regexp that determines buf B regions to ignore when skipping to diff
(ediff-defvar-local ediff-regexp-hide-B "" "")
;; Regexp that determines buf C regions to ignore when skipping to diff
(ediff-defvar-local ediff-regexp-hide-C "" "")
;; connective that determines whether to hide regions that match both or
;; one of the regexps
(ediff-defvar-local ediff-hide-regexp-connective 'and "")


;;; Copying difference regions between buffers.

;; A list of killed diffs.
;; A diff is saved here if it is replaced by a diff
;; from another buffer.  This alist has the form:
;; \((num (buff-object . diff) (buff-object . diff) (buff-object . diff)) ...),
;; where some buffer-objects may be missing.
(ediff-defvar-local ediff-killed-diffs-alist nil "")

;; Syntax table to use in ediff-forward-word-function
;; This is chosen by a heuristic. The important thing is for all buffers to
;; have the same syntax table. Which is not too important.
(ediff-defvar-local ediff-syntax-table nil "")


;; Highlighting
(defcustom ediff-before-flag-bol (if (featurep 'xemacs) (make-glyph "->>") "->>")
  "Flag placed before a highlighted block of differences, if block starts at beginning of a line."
  :type 'string
  :tag  "Region before-flag at beginning of line"
  :group 'ediff)

(defcustom ediff-after-flag-eol  (if (featurep 'xemacs) (make-glyph "<<-") "<<-")
  "Flag placed after a highlighted block of differences, if block ends at end of a line."
  :type 'string
  :tag  "Region after-flag at end of line"
  :group 'ediff)

(defcustom ediff-before-flag-mol (if (featurep 'xemacs) (make-glyph "->>") "->>")
  "Flag placed before a highlighted block of differences, if block starts in mid-line."
  :type 'string
  :tag  "Region before-flag in the middle of line"
  :group 'ediff)
(defcustom ediff-after-flag-mol  (if (featurep 'xemacs) (make-glyph "<<-") "<<-")
  "Flag placed after a highlighted block of differences, if block ends in mid-line."
  :type 'string
  :tag  "Region after-flag in the middle of line"
  :group 'ediff)


(defcustom ediff-use-faces t
  "If t, differences are highlighted using faces, if device supports faces.
If nil, differences are highlighted using ASCII flags, ediff-before-flag
and ediff-after-flag.  On a non-window system, differences are always
highlighted using ASCII flags."
  :type 'boolean
  :group 'ediff-highlighting)
(ediff-defvar-local ediff-use-faces t "")

;; this indicates that diff regions are word-size, so fine diffs are
;; permanently nixed; used in ediff-windows-wordwise and ediff-regions-wordwise
(ediff-defvar-local ediff-word-mode nil "")
;; Name of the job (ediff-files, ediff-windows, etc.)
(ediff-defvar-local ediff-job-name nil "")

;; Narrowing and ediff-region/windows support
;; This is a list (overlay-A overlay-B overlay-C)
;; If set, Ediff compares only those parts of buffers A/B/C that lie within
;; the bounds of these overlays.
(ediff-defvar-local ediff-narrow-bounds nil "")

;; List (overlay-A overlay-B overlay-C), where each overlay spans the
;; entire corresponding buffer.
(ediff-defvar-local ediff-wide-bounds nil "")

;; Current visibility boundaries in buffers A, B, and C.
;; This is also a list of overlays.  When the user toggles narrow/widen,
;; this list changes from ediff-wide-bounds to ediff-narrow-bounds.
;; and back.
(ediff-defvar-local ediff-visible-bounds nil "")

(ediff-defvar-local ediff-start-narrowed t
  "Non-nil means start narrowed, if doing ediff-windows-* or ediff-regions-*")
(ediff-defvar-local ediff-quit-widened t
  "*Non-nil means: when finished, Ediff widens buffers A/B.
Actually, Ediff restores the scope of visibility that existed at startup.")

(defcustom ediff-keep-variants t
  "nil means prompt to remove unmodified buffers A/B/C at session end.
Supplying a prefix argument to the quit command `q' temporarily reverses the
meaning of this variable."
  :type 'boolean
  :group 'ediff)

(defcustom ediff-highlight-all-diffs t
  "If nil, only the selected differences are highlighted.
Otherwise, all difference regions are highlighted, but the selected region is
shown in brighter colors."
  :type 'boolean
  :group 'ediff-highlighting)
(ediff-defvar-local ediff-highlight-all-diffs t "")


;; The suffix of the control buffer name.
(ediff-defvar-local ediff-control-buffer-suffix nil "")
;; Same as ediff-control-buffer-suffix, but without <,>.
;; It's a number rather than string.
(ediff-defvar-local ediff-control-buffer-number nil "")


;; The original values of ediff-protected-variables for buffer A
(ediff-defvar-local ediff-buffer-values-orig-A nil "")
;; The original values of ediff-protected-variables for buffer B
(ediff-defvar-local ediff-buffer-values-orig-B nil "")
;; The original values of ediff-protected-variables for buffer C
(ediff-defvar-local ediff-buffer-values-orig-C nil "")
;; The original values of ediff-protected-variables for buffer Ancestor
(ediff-defvar-local ediff-buffer-values-orig-Ancestor nil "")

;; association between buff-type and ediff-buffer-values-orig-*
(defconst ediff-buffer-values-orig-alist
  '((A . ediff-buffer-values-orig-A)
    (B . ediff-buffer-values-orig-B)
    (C . ediff-buffer-values-orig-C)
    (Ancestor . ediff-buffer-values-orig-Ancestor)))

;; Buffer-local variables to be saved then restored during Ediff sessions
(defconst ediff-protected-variables '(
				      ;;buffer-read-only
				      mode-line-format))

;; Vector of differences between the variants.  Each difference is
;; represented by a vector of two overlays plus a vector of fine diffs,
;; plus a no-fine-diffs flag.  The first overlay spans the
;; difference region in the A buffer and the second overlays the diff in
;; the B buffer.  If a difference section is empty, the corresponding
;; overlay's endpoints coincide.
;;
;; The precise form of a Difference Vector for one buffer is:
;; [diff diff diff ...]
;; where each diff has the form:
;; [diff-overlay fine-diff-vector no-fine-diffs-flag state-of-diff]
;; fine-diff-vector is a vector [fine-diff-overlay fine-diff-overlay ...]
;; no-fine-diffs-flag says if there are fine differences.
;; state-of-difference is A, B, C, or nil, indicating which buffer is
;;	different from the other two (used only in 3-way jobs.
(ediff-defvar-local ediff-difference-vector-A nil "")
(ediff-defvar-local ediff-difference-vector-B nil "")
(ediff-defvar-local ediff-difference-vector-C nil "")
(ediff-defvar-local ediff-difference-vector-Ancestor nil "")
;; A-list of diff vector types associated with buffer types
(defconst ediff-difference-vector-alist
  '((A . ediff-difference-vector-A)
    (B . ediff-difference-vector-B)
    (C . ediff-difference-vector-C)
    (Ancestor . ediff-difference-vector-Ancestor)))

;; [ status status status ...]
;; Each status: [state-of-merge state-of-ancestor]
;; state-of-merge is default-A, default-B, prefer-A, or prefer-B.  It
;; indicates the way a diff region was created in buffer C.
;; state-of-ancestor says if the corresponding region in ancestor buffer is
;; empty.
(ediff-defvar-local ediff-state-of-merge nil "")

;; The difference that is currently selected.
(ediff-defvar-local ediff-current-difference -1 "")
;; Number of differences found.
(ediff-defvar-local ediff-number-of-differences nil "")

;; Buffer containing the output of diff, which is used by Ediff to step
;; through files.
(ediff-defvar-local ediff-diff-buffer nil "")
;; Like ediff-diff-buffer, but contains context diff.  It is not used by
;; Ediff, but it is saved in a file, if user requests so.
(ediff-defvar-local ediff-custom-diff-buffer nil "")
;; Buffer used for diff-style fine differences between regions.
(ediff-defvar-local ediff-fine-diff-buffer nil "")
;; Temporary buffer used for computing fine differences.
(defconst ediff-tmp-buffer " *ediff-tmp*" "")
;; Buffer used for messages
(defconst ediff-msg-buffer " *ediff-message*" "")
;; Buffer containing the output of diff when diff returns errors.
(ediff-defvar-local ediff-error-buffer nil "")
;; Buffer to display debug info
(ediff-defvar-local ediff-debug-buffer "*ediff-debug*" "")

;; List of ediff control panels associated with each buffer A/B/C/Ancestor.
;; Not used any more, but may be needed in the future.
(ediff-defvar-local ediff-this-buffer-ediff-sessions  nil "")

;; to be deleted in due time
;; List of difference overlays disturbed by working with the current diff.
(defvar ediff-disturbed-overlays nil "")

;; Priority of non-selected overlays.
(defvar ediff-shadow-overlay-priority  100 "")

(defcustom ediff-version-control-package 'vc
  "Version control package used.
Currently, Ediff supports vc.el, rcs.el, pcl-cvs.el, and generic-sc.el.  The
standard Emacs interface to RCS, CVS, SCCS, etc., is vc.el.  However, some
people find the other two packages more convenient.  Set this variable to the
appropriate symbol: `rcs', `pcl-cvs', or `generic-sc' if you so desire."
  :type 'symbol
  :group 'ediff)

(defcustom ediff-coding-system-for-read 'raw-text
  "The coding system for read to use when running the diff program as a subprocess.
In most cases, the default will do.  However, under certain circumstances in
MS-Windows you might need to use something like 'raw-text-dos here.
So, if the output that your diff program sends to Emacs contains extra ^M's,
you might need to experiment here, if the default or 'raw-text-dos doesn't
work."
  :type 'symbol
  :group 'ediff)

(defcustom ediff-coding-system-for-write (if (featurep 'xemacs)
					     'escape-quoted
					   'emacs-internal)
  "The coding system for write to use when writing out difference regions
to temp files in buffer jobs and when Ediff needs to find fine differences."
  :type 'symbol
  :group 'ediff)


(defalias 'ediff-read-event
  (if (featurep 'xemacs) 'next-command-event 'read-event))

(defalias 'ediff-overlayp
  (if (featurep 'xemacs) 'extentp 'overlayp))

(defalias 'ediff-make-overlay
  (if (featurep 'xemacs) 'make-extent 'make-overlay))

(defalias 'ediff-delete-overlay
  (if (featurep 'xemacs) 'delete-extent 'delete-overlay))

;; Assumes that emacs-major-version and emacs-minor-version are defined.
(defun ediff-check-version (op major minor &optional type-of-emacs)
  "Check the current version against MAJOR and MINOR version numbers.
The comparison uses operator OP, which may be any of: =, >, >=, <, <=.
TYPE-OF-EMACS is either 'xemacs or 'emacs."
  (and (cond ((eq type-of-emacs 'xemacs) (featurep 'xemacs))
	     ((eq type-of-emacs 'emacs) (featurep 'emacs))
	     (t))
       (cond ((eq op '=) (and (= emacs-minor-version minor)
			      (= emacs-major-version major)))
	     ((memq op '(> >= < <=))
	      (and (or (funcall op emacs-major-version major)
		       (= emacs-major-version major))
		   (if (= emacs-major-version major)
		       (funcall op emacs-minor-version minor)
		     t)))
	     (t
	      (error "%S: Invalid op in ediff-check-version" op)))))

;; ediff-check-version seems to be totally unused anyway.
(make-obsolete 'ediff-check-version 'version< "23.1")

(defun ediff-color-display-p ()
  (condition-case nil
      (if (featurep 'xemacs)
	  (eq (device-class (selected-device)) 'color) ; xemacs form
	(display-color-p)) ; emacs form
    (error nil)))


;; A var local to each control panel buffer.  Indicates highlighting style
;; in effect for this buffer: `face', `ascii',
;; `off' -- turned off \(on a dumb terminal only\).
(ediff-defvar-local ediff-highlighting-style
  (if (and (ediff-has-face-support-p) ediff-use-faces) 'face 'ascii)
  "")


(if (ediff-window-display-p)
    (if (featurep 'xemacs)
	(progn
	  (defalias 'ediff-display-pixel-width 'device-pixel-width)
	  (defalias 'ediff-display-pixel-height 'device-pixel-height))
      (defalias 'ediff-display-pixel-width
	(if (fboundp 'display-pixel-width)
	    'display-pixel-width
	  'x-display-pixel-width))
      (defalias 'ediff-display-pixel-height
	(if (fboundp 'display-pixel-height)
	    'display-pixel-height
	  'x-display-pixel-height))))

;; A-list of current-diff-overlay symbols associated with buf types
(defconst ediff-current-diff-overlay-alist
  '((A . ediff-current-diff-overlay-A)
    (B . ediff-current-diff-overlay-B)
    (C . ediff-current-diff-overlay-C)
    (Ancestor . ediff-current-diff-overlay-Ancestor)))

;; A-list of current-diff-face-* symbols associated with buf types
(defconst ediff-current-diff-face-alist
  '((A . ediff-current-diff-A)
    (B . ediff-current-diff-B)
    (C . ediff-current-diff-C)
    (Ancestor . ediff-current-diff-Ancestor)))


(defun ediff-set-overlay-face (extent face)
  (ediff-overlay-put extent 'face face)
  (ediff-overlay-put extent 'help-echo 'ediff-region-help-echo))

(defun ediff-region-help-echo (extent-or-window &optional overlay point)
  (unless overlay
    (setq overlay extent-or-window))
  (let ((is-current (ediff-overlay-get overlay 'ediff))
	(face (ediff-overlay-get overlay 'face))
	(diff-num (ediff-overlay-get overlay 'ediff-diff-num))
	face-help)

    ;; This happens only for refinement overlays
    (if (stringp face)
	(setq face (intern face)))
    (setq face-help (and face (get face 'ediff-help-echo)))

    (cond ((and is-current diff-num)	; current diff region
	   (format "Difference region %S -- current" (1+ diff-num)))
	  (face-help)			; refinement of current diff region
	  (diff-num			; non-current
	   (format "Difference region %S -- non-current" (1+ diff-num)))
	  (t ""))			; none
    ))


(defun ediff-set-face-pixmap (face pixmap)
  "Set face pixmap on a monochrome display."
  (if (and (ediff-window-display-p) (not (ediff-color-display-p)))
      (condition-case nil
	  (set-face-background-pixmap face pixmap)
	(error
	 (message "Pixmap not found for %S: %s" (face-name face) pixmap)
	 (sit-for 1)))))

(defun ediff-hide-face (face)
  (if (and (ediff-has-face-support-p)
	   (boundp 'add-to-list)
	   (boundp 'facemenu-unlisted-faces))
      (add-to-list 'facemenu-unlisted-faces face)))



(defface ediff-current-diff-A
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "firebrick" :background "pale green"))
	(((class color))
	 (:foreground "blue3" :background "yellow3"))
	(t		     (:inverse-video t)))
    '((((type tty))    (:foreground "blue3" :background "yellow3"))
      (((class color)) (:foreground "firebrick" :background "pale green"))
      (t	     	     (:inverse-video t))))
  "Face for highlighting the selected difference in buffer A."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-current-diff-face-A 'ediff-current-diff-A
  "Face for highlighting the selected difference in buffer A.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-current-diff-A'
this variable represents.")
(ediff-hide-face ediff-current-diff-face-A)
;; Until custom.el for XEmacs starts supporting :inverse-video we do this.
;; This means that some user customization may be trashed.
(and (featurep 'xemacs)
     (ediff-has-face-support-p)
     (not (ediff-color-display-p))
     (copy-face 'modeline ediff-current-diff-face-A))



(defface ediff-current-diff-B
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "DarkOrchid" :background "Yellow"))
	(((class color))
	 (:foreground "magenta3" :background "yellow3"
		      :weight bold))
	(t		     (:inverse-video t)))
    '((((type tty))    (:foreground "magenta3" :background "yellow3"
				    :weight bold))
      (((class color)) (:foreground "DarkOrchid" :background "Yellow"))
      (t	     	     (:inverse-video t))))
  "Face for highlighting the selected difference in buffer B."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-current-diff-face-B 'ediff-current-diff-B
  "Face for highlighting the selected difference in buffer B.
 this variable.  Instead, use the customization
widget to customize the actual face `ediff-current-diff-B'
this variable represents.")
(ediff-hide-face ediff-current-diff-face-B)
;; Until custom.el for XEmacs starts supporting :inverse-video we do this.
;; This means that some user customization may be trashed.
(and (featurep 'xemacs)
     (ediff-has-face-support-p)
     (not (ediff-color-display-p))
     (copy-face 'modeline ediff-current-diff-face-B))


(defface ediff-current-diff-C
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "Navy" :background "Pink"))
	(((class color))
	 (:foreground "cyan3" :background "yellow3" :weight bold))
	(t		     (:inverse-video t)))
    '((((type tty))    (:foreground "cyan3" :background "yellow3" :weight bold))
      (((class color)) (:foreground "Navy" :background "Pink"))
      (t	     	     (:inverse-video t))))
  "Face for highlighting the selected difference in buffer C."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-current-diff-face-C 'ediff-current-diff-C
  "Face for highlighting the selected difference in buffer C.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-current-diff-C'
this variable represents.")
(ediff-hide-face ediff-current-diff-face-C)
;; Until custom.el for XEmacs starts supporting :inverse-video we do this.
;; This means that some user customization may be trashed.
(and (featurep 'xemacs)
     (ediff-has-face-support-p)
     (not (ediff-color-display-p))
     (copy-face 'modeline ediff-current-diff-face-C))


(defface ediff-current-diff-Ancestor
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "Black" :background "VioletRed"))
	(((class color))
	 (:foreground "black" :background "magenta3"))
	(t (:inverse-video t)))
    '((((type tty))    (:foreground "black" :background "magenta3"))
      (((class color)) (:foreground "Black" :background "VioletRed"))
      (t (:inverse-video t))))
  "Face for highlighting the selected difference in buffer Ancestor."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-current-diff-face-Ancestor 'ediff-current-diff-Ancestor
  "Face for highlighting the selected difference in buffer Ancestor.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-current-diff-Ancestor'
this variable represents.")
(ediff-hide-face ediff-current-diff-face-Ancestor)
;; Until custom.el for XEmacs starts supporting :inverse-video we do this.
;; This means that some user customization may be trashed.
(and (featurep 'xemacs)
     (ediff-has-face-support-p)
     (not (ediff-color-display-p))
     (copy-face 'modeline ediff-current-diff-face-Ancestor))


(defface ediff-fine-diff-A
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "Navy" :background "sky blue"))
	(((class color))
	 (:foreground "white" :background "sky blue" :weight bold))
	(t (:underline t :stipple "gray3")))
    '((((type tty))    (:foreground "white" :background "sky blue" :weight bold))
      (((class color)) (:foreground "Navy" :background "sky blue"))
      (t (:underline t :stipple "gray3"))))
  "Face for highlighting the refinement of the selected diff in buffer A."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-fine-diff-face-A 'ediff-fine-diff-A
  "Face for highlighting the fine differences in buffer A.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-fine-diff-A'
this variable represents.")
(ediff-hide-face ediff-fine-diff-face-A)

(defface ediff-fine-diff-B
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "Black" :background "cyan"))
	(((class color))
	 (:foreground "magenta3" :background "cyan3"))
	(t		     (:underline t :stipple "gray3")))
    '((((type tty))    (:foreground "magenta3" :background "cyan3"))
      (((class color)) (:foreground "Black" :background "cyan"))
      (t	     	     (:underline t :stipple "gray3"))))
  "Face for highlighting the refinement of the selected diff in buffer B."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-fine-diff-face-B 'ediff-fine-diff-B
  "Face for highlighting the fine differences in buffer B.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-fine-diff-B'
this variable represents.")
(ediff-hide-face ediff-fine-diff-face-B)

(defface ediff-fine-diff-C
  (if (featurep 'emacs)
      '((((type pc))
	 (:foreground "white" :background "Turquoise"))
	(((class color) (min-colors 16))
	 (:foreground "Black" :background "Turquoise"))
	(((class color))
	 (:foreground "yellow3" :background "Turquoise"
		      :weight bold))
	(t (:underline t :stipple "gray3")))
    '((((type tty))    (:foreground "yellow3" :background "Turquoise"
				    :weight bold))
      (((type pc))     (:foreground "white" :background "Turquoise"))
      (((class color)) (:foreground "Black" :background "Turquoise"))
      (t (:underline t :stipple "gray3"))))
  "Face for highlighting the refinement of the selected diff in buffer C."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-fine-diff-face-C 'ediff-fine-diff-C
  "Face for highlighting the fine differences in buffer C.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-fine-diff-C'
this variable represents.")
(ediff-hide-face ediff-fine-diff-face-C)

(defface ediff-fine-diff-Ancestor
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "Black" :background "Green"))
	(((class color))
	 (:foreground "red3" :background "green"))
	(t		     (:underline t :stipple "gray3")))
    '((((type tty))    (:foreground "red3" :background "green"))
      (((class color)) (:foreground "Black" :background "Green"))
      (t	     	     (:underline t :stipple "gray3"))))
  "Face for highlighting the refinement of the selected diff in the ancestor buffer.
At present, this face is not used and no fine differences are computed for the
ancestor buffer."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-fine-diff-face-Ancestor 'ediff-fine-diff-Ancestor
  "Face for highlighting the fine differences in buffer Ancestor.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-fine-diff-Ancestor'
this variable represents.")
(ediff-hide-face ediff-fine-diff-face-Ancestor)

;; Some installs don't have stipple or Stipple. So, try them in turn.
(defvar stipple-pixmap
  (cond ((not (ediff-has-face-support-p)) nil)
	((and (boundp 'x-bitmap-file-path)
	      (locate-library "stipple" t x-bitmap-file-path)) "stipple")
	((and (boundp 'mswindowsx-bitmap-file-path)
	      (locate-library "stipple" t mswindowsx-bitmap-file-path)) "stipple")
	(t "Stipple")))

(defface ediff-even-diff-A
  (if (featurep 'emacs)
      `((((type pc))
	 (:foreground "green3" :background "light grey"))
	(((class color) (min-colors 16))
	 (:foreground "Black" :background "light grey"))
	(((class color))
	 (:foreground "red3" :background "light grey"
		      :weight bold))
	(t		     (:italic t :stipple ,stipple-pixmap)))
    `((((type tty))    (:foreground "red3" :background "light grey"
				    :weight bold))
      (((type pc))     (:foreground "green3" :background "light grey"))
      (((class color)) (:foreground "Black" :background "light grey"))
      (t	     	     (:italic t :stipple ,stipple-pixmap))))
  "Face for highlighting even-numbered non-current differences in buffer A."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-even-diff-face-A 'ediff-even-diff-A
  "Face for highlighting even-numbered non-current differences in buffer A.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-even-diff-A'
this variable represents.")
(ediff-hide-face ediff-even-diff-face-A)

(defface ediff-even-diff-B
  (if (featurep 'emacs)
      `((((class color) (min-colors 16))
	 (:foreground "White" :background "Grey"))
	(((class color))
	 (:foreground "blue3" :background "Grey" :weight bold))
	(t		     (:italic t :stipple ,stipple-pixmap)))
    `((((type tty))    (:foreground "blue3" :background "Grey" :weight bold))
      (((class color)) (:foreground "White" :background "Grey"))
      (t	     	     (:italic t :stipple ,stipple-pixmap))))
  "Face for highlighting even-numbered non-current differences in buffer B."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-even-diff-face-B 'ediff-even-diff-B
  "Face for highlighting even-numbered non-current differences in buffer B.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-even-diff-B'
this variable represents.")
(ediff-hide-face ediff-even-diff-face-B)

(defface ediff-even-diff-C
  (if (featurep 'emacs)
      `((((type pc))
	 (:foreground "yellow3" :background "light grey"))
	(((class color) (min-colors 16))
	 (:foreground "Black" :background "light grey"))
	(((class color))
	 (:foreground "yellow3" :background "light grey"
		      :weight bold))
	(t		     (:italic t :stipple ,stipple-pixmap)))
    `((((type tty))    (:foreground "yellow3" :background "light grey"
				    :weight bold))
      (((type pc))     (:foreground "yellow3" :background "light grey"))
      (((class color)) (:foreground "Black" :background "light grey"))
      (t	     	     (:italic t :stipple ,stipple-pixmap))))
  "Face for highlighting even-numbered non-current differences in buffer C."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-even-diff-face-C 'ediff-even-diff-C
  "Face for highlighting even-numbered non-current differences in buffer C.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-even-diff-C'
this variable represents.")
(ediff-hide-face ediff-even-diff-face-C)

(defface ediff-even-diff-Ancestor
  (if (featurep 'emacs)
      `((((type pc))
	 (:foreground "cyan3" :background "light grey"))
	(((class color) (min-colors 16))
	 (:foreground "White" :background "Grey"))
	(((class color))
	 (:foreground "cyan3" :background "light grey"
		      :weight bold))
	(t (:italic t :stipple ,stipple-pixmap)))
    `((((type tty))    (:foreground "cyan3" :background "light grey"
				    :weight bold))
      (((type pc))     (:foreground "cyan3" :background "light grey"))
      (((class color)) (:foreground "White" :background "Grey"))
      (t (:italic t :stipple ,stipple-pixmap))))
  "Face for highlighting even-numbered non-current differences in the ancestor buffer."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-even-diff-face-Ancestor 'ediff-even-diff-Ancestor
  "Face for highlighting even-numbered non-current differences in buffer Ancestor.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-even-diff-Ancestor'
this variable represents.")
(ediff-hide-face ediff-even-diff-face-Ancestor)

;; Association between buffer types and even-diff-face symbols
(defconst ediff-even-diff-face-alist
  '((A . ediff-even-diff-A)
    (B . ediff-even-diff-B)
    (C . ediff-even-diff-C)
    (Ancestor . ediff-even-diff-Ancestor)))

(defface ediff-odd-diff-A
  (if (featurep 'emacs)
      '((((type pc))
	 (:foreground "green3" :background "gray40"))
	(((class color) (min-colors 16))
	 (:foreground "White" :background "Grey"))
	(((class color))
	 (:foreground "red3" :background "black" :weight bold))
	(t		     (:italic t :stipple "gray1")))
    '((((type tty))    (:foreground "red3" :background "black" :weight bold))
      (((type pc))     (:foreground "green3" :background "gray40"))
      (((class color)) (:foreground "White" :background "Grey"))
      (t	     	     (:italic t :stipple "gray1"))))
  "Face for highlighting odd-numbered non-current differences in buffer A."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-odd-diff-face-A 'ediff-odd-diff-A
  "Face for highlighting odd-numbered non-current differences in buffer A.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-odd-diff-A'
this variable represents.")
(ediff-hide-face ediff-odd-diff-face-A)


(defface ediff-odd-diff-B
  (if (featurep 'emacs)
      '((((type pc))
	 (:foreground "White" :background "gray40"))
	(((class color) (min-colors 16))
	 (:foreground "Black" :background "light grey"))
	(((class color))
	 (:foreground "cyan3" :background "black" :weight bold))
	(t		     (:italic t :stipple "gray1")))
    '((((type tty))    (:foreground "cyan3" :background "black" :weight bold))
      (((type pc))     (:foreground "White" :background "gray40"))
      (((class color)) (:foreground "Black" :background "light grey"))
      (t	     	     (:italic t :stipple "gray1"))))
  "Face for highlighting odd-numbered non-current differences in buffer B."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-odd-diff-face-B 'ediff-odd-diff-B
  "Face for highlighting odd-numbered non-current differences in buffer B.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-odd-diff-B'
this variable represents.")
(ediff-hide-face ediff-odd-diff-face-B)

(defface ediff-odd-diff-C
  (if (featurep 'emacs)
      '((((type pc))
	 (:foreground "yellow3" :background "gray40"))
	(((class color) (min-colors 16))
	 (:foreground "White" :background "Grey"))
	(((class color))
	 (:foreground "yellow3" :background "black" :weight bold))
	(t		     (:italic t :stipple "gray1")))
    '((((type tty))    (:foreground "yellow3" :background "black" :weight bold))
      (((type pc))     (:foreground "yellow3" :background "gray40"))
      (((class color)) (:foreground "White" :background "Grey"))
      (t	     	     (:italic t :stipple "gray1"))))
  "Face for highlighting odd-numbered non-current differences in buffer C."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-odd-diff-face-C 'ediff-odd-diff-C
  "Face for highlighting odd-numbered non-current differences in buffer C.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-odd-diff-C'
this variable represents.")
(ediff-hide-face ediff-odd-diff-face-C)

(defface ediff-odd-diff-Ancestor
  (if (featurep 'emacs)
      '((((class color) (min-colors 16))
	 (:foreground "cyan3" :background "gray40"))
	(((class color))
	 (:foreground "green3" :background "black" :weight bold))
	(t		     (:italic t :stipple "gray1")))
    '((((type tty))    (:foreground "green3" :background "black" :weight bold))
      (((class color)) (:foreground "cyan3" :background "gray40"))
      (t	     	     (:italic t :stipple "gray1"))))
  "Face for highlighting odd-numbered non-current differences in the ancestor buffer."
  :group 'ediff-highlighting)
;; An internal variable.  Ediff takes the face from here.  When unhighlighting,
;; this variable is set to nil, then again to the appropriate face.
(defvar ediff-odd-diff-face-Ancestor 'ediff-odd-diff-Ancestor
  "Face for highlighting odd-numbered non-current differences in buffer Ancestor.
DO NOT CHANGE this variable.  Instead, use the customization
widget to customize the actual face object `ediff-odd-diff-Ancestor'
this variable represents.")
(ediff-hide-face ediff-odd-diff-face-Ancestor)

;; Association between buffer types and odd-diff-face symbols
(defconst ediff-odd-diff-face-alist
  '((A . ediff-odd-diff-A)
    (B . ediff-odd-diff-B)
    (C . ediff-odd-diff-C)
    (Ancestor . ediff-odd-diff-Ancestor)))

;; A-list of fine-diff face symbols associated with buffer types
(defconst ediff-fine-diff-face-alist
  '((A . ediff-fine-diff-A)
    (B . ediff-fine-diff-B)
    (C . ediff-fine-diff-C)
    (Ancestor . ediff-fine-diff-Ancestor)))

;; Help echo
(put ediff-fine-diff-face-A 'ediff-help-echo
     "A `refinement' of the current difference region")
(put ediff-fine-diff-face-B 'ediff-help-echo
     "A `refinement' of the current difference region")
(put ediff-fine-diff-face-C 'ediff-help-echo
     "A `refinement' of the current difference region")
(put ediff-fine-diff-face-Ancestor 'ediff-help-echo
     "A `refinement' of the current difference region")

(add-hook 'ediff-quit-hook 'ediff-cleanup-mess)
(add-hook 'ediff-suspend-hook 'ediff-default-suspend-function)


;;; Overlays

(ediff-defvar-local ediff-current-diff-overlay-A nil
  "Overlay for the current difference region in buffer A.")
(ediff-defvar-local ediff-current-diff-overlay-B nil
  "Overlay for the current difference region in buffer B.")
(ediff-defvar-local ediff-current-diff-overlay-C nil
  "Overlay for the current difference region in buffer C.")
(ediff-defvar-local ediff-current-diff-overlay-Ancestor nil
  "Overlay for the current difference region in the ancestor buffer.")

;; Compute priority of a current ediff overlay.
(defun ediff-highest-priority (start end buffer)
  (let ((pos (max 1 (1- start)))
	ovr-list)
    (if (featurep 'xemacs)
	(1+ ediff-shadow-overlay-priority)
      (ediff-with-current-buffer buffer
	(while (< pos (min (point-max) (1+ end)))
	  (setq ovr-list (append (overlays-at pos) ovr-list))
	  (setq pos (next-overlay-change pos)))
	(+ 1 ediff-shadow-overlay-priority
	   (apply 'max
		  (cons
		   1
		   (mapcar
		    (lambda (ovr)
		      (if (and ovr
			       ;; exclude ediff overlays from priority
			       ;; calculation, or else priority will keep
			       ;; increasing
			       (null (ediff-overlay-get ovr 'ediff))
			       (null (ediff-overlay-get ovr 'ediff-diff-num)))
			  ;; use the overlay priority or 0
			  (or (ediff-overlay-get ovr 'priority) 0)
			0))
		    ovr-list))))))))


(defvar ediff-toggle-read-only-function 'toggle-read-only
  "Function to be used to toggle read-only status of the buffer.
If nil, Ediff tries using the command bound to C-x C-q.")

(defcustom ediff-make-buffers-readonly-at-startup nil
  "Make all variant buffers read-only when Ediff starts up.
This property can be toggled interactively."
  :type 'boolean
  :group 'ediff)


;;; Misc

;; if nil, this silences some messages
(defvar ediff-verbose-p t)

(defcustom ediff-autostore-merges  'group-jobs-only
  "Save the results of merge jobs automatically.
With value nil, don't save automatically.  With value t, always
save.  Anything else means save automatically only if the merge
job is part of a group of jobs, such as `ediff-merge-directory'
or `ediff-merge-directory-revisions'."
  :type '(choice (const nil) (const t) (const group-jobs-only))
  :group 'ediff-merge)
(make-variable-buffer-local 'ediff-autostore-merges)

;; file where the result of the merge is to be saved.  used internally
(ediff-defvar-local ediff-merge-store-file nil "")

(defcustom ediff-merge-filename-prefix "merge_"
  "Prefix to be attached to saved merge buffers."
  :type 'string
  :group 'ediff-merge)

(defcustom ediff-no-emacs-help-in-control-buffer nil
  "Non-nil means C-h should not invoke Emacs help in control buffer.
Instead, C-h would jump to previous difference."
  :type 'boolean
  :group 'ediff)

;; This is the same as temporary-file-directory from Emacs 20.3.
;; Copied over here because XEmacs doesn't have this variable.
(defcustom ediff-temp-file-prefix
  (file-name-as-directory
   (cond ((boundp 'temporary-file-directory) temporary-file-directory)
	 ((fboundp 'temp-directory) (temp-directory))
	 (t "/tmp/")))
;;;  (file-name-as-directory
;;;   (cond ((memq system-type '(ms-dos windows-nt))
;;;	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
;;;	 (t
;;;	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
  "Prefix to put on Ediff temporary file names.
Do not start with `~/' or `~USERNAME/'."
  :type 'string
  :group 'ediff)

(defcustom ediff-temp-file-mode 384	; u=rw only
  "Mode for Ediff temporary files."
  :type 'integer
  :group 'ediff)

;; Metacharacters that have to be protected from the shell when executing
;; a diff/diff3 command.
(defcustom ediff-metachars "[ \t\n!\"#$&'()*;<=>?[\\^`{|~]"
  "Regexp that matches characters that must be quoted with `\\' in shell command line.
This default should work without changes."
  :type 'string
  :group 'ediff)

;; needed to simulate frame-char-width in XEmacs.
(defvar ediff-H-glyph (if (featurep 'xemacs) (make-glyph "H")))


;; Temporary file used for refining difference regions in buffer A.
(ediff-defvar-local ediff-temp-file-A nil "")
;; Temporary file used for refining difference regions in buffer B.
(ediff-defvar-local ediff-temp-file-B nil "")
;; Temporary file used for refining difference regions in buffer C.
(ediff-defvar-local ediff-temp-file-C nil "")


(defun ediff-file-remote-p (file-name)
  (file-remote-p file-name))

;; File for which we can get attributes, such as size or date
(defun ediff-listable-file (file-name)
  (let ((handler (find-file-name-handler file-name 'file-local-copy)))
    (or (null handler) (eq handler 'dired-handler-fn))))


(defsubst ediff-frame-unsplittable-p (frame)
  (cdr (assq 'unsplittable (frame-parameters frame))))

(defsubst ediff-get-next-window (wind prev-wind)
  (cond ((window-live-p wind) wind)
	(prev-wind (next-window wind))
	(t (selected-window))
	))


(defsubst ediff-kill-buffer-carefully (buf)
  "Kill buffer BUF if it exists."
  (if (ediff-buffer-live-p buf)
      (kill-buffer (get-buffer buf))))

(defsubst ediff-background-face (buf-type dif-num)
  ;; The value of dif-num is always 1- the one that user sees.
  ;; This is why even face is used when dif-num is odd.
  (ediff-get-symbol-from-alist
   buf-type (if (ediff-odd-p dif-num)
		ediff-even-diff-face-alist
	      ediff-odd-diff-face-alist)
   ))


;; activate faces on diff regions in buffer
(defun ediff-paint-background-regions-in-one-buffer (buf-type unhighlight)
  (let ((diff-vector
	 (eval (ediff-get-symbol-from-alist
		buf-type ediff-difference-vector-alist)))
	overl diff-num)
    (mapcar (lambda (rec)
	      (setq overl (ediff-get-diff-overlay-from-diff-record rec)
		    diff-num (ediff-overlay-get overl 'ediff-diff-num))
	      (if (ediff-overlay-buffer overl)
		  ;; only if overlay is alive
		  (ediff-set-overlay-face
		   overl
		   (if (not unhighlight)
		       (ediff-background-face buf-type diff-num))))
	      )
	    diff-vector)))


;; activate faces on diff regions in all buffers
(defun ediff-paint-background-regions (&optional unhighlight)
  (ediff-paint-background-regions-in-one-buffer
   'A unhighlight)
  (ediff-paint-background-regions-in-one-buffer
   'B unhighlight)
  (ediff-paint-background-regions-in-one-buffer
   'C unhighlight)
  (ediff-paint-background-regions-in-one-buffer
   'Ancestor unhighlight))


;; arg is a record for a given diff in a difference vector
;; this record is itself a vector
(defsubst ediff-clear-fine-diff-vector (diff-record)
  (if diff-record
      (mapc 'ediff-delete-overlay
	    (ediff-get-fine-diff-vector-from-diff-record diff-record))))

(defsubst ediff-clear-fine-differences-in-one-buffer (n buf-type)
  (ediff-clear-fine-diff-vector (ediff-get-difference n buf-type))
  (ediff-set-fine-diff-vector n buf-type nil))

(defsubst ediff-clear-fine-differences (n)
  (ediff-clear-fine-differences-in-one-buffer n 'A)
  (ediff-clear-fine-differences-in-one-buffer n 'B)
  (if ediff-3way-job
      (ediff-clear-fine-differences-in-one-buffer n 'C)))


(defsubst ediff-mouse-event-p (event)
  (if (featurep 'xemacs)
      (button-event-p event)
    (string-match "mouse" (format "%S" (event-basic-type event)))))


(defsubst ediff-key-press-event-p (event)
  (if (featurep 'xemacs)
      (key-press-event-p event)
    (or (char-or-string-p event) (symbolp event))))

(defun ediff-event-point (event)
  (cond ((ediff-mouse-event-p event)
	 (if (featurep 'xemacs)
	     (event-point event)
	   (posn-point (event-start event))))
	((ediff-key-press-event-p event)
	 (point))
	(t (error "Error"))))

(defun ediff-event-buffer (event)
  (cond ((ediff-mouse-event-p event)
	 (if (featurep 'xemacs)
	     (event-buffer event)
	   (window-buffer (posn-window (event-start event)))))
	((ediff-key-press-event-p event)
	 (current-buffer))
	(t (error "Error"))))

(defun ediff-event-key (event-or-key)
  (if (featurep 'xemacs)
      ;;(if (eventp event-or-key) (event-key event-or-key) event-or-key)
      (if (eventp event-or-key) (event-to-character event-or-key t t) event-or-key)
    event-or-key))

(defun ediff-last-command-char ()
  (ediff-event-key last-command-event))


(defsubst ediff-frame-iconified-p (frame)
  (and (ediff-window-display-p) (frame-live-p frame)
       (if (featurep 'xemacs)
	   (frame-iconified-p frame)
	 (eq (frame-visible-p frame) 'icon))))

(defsubst ediff-window-visible-p (wind)
  ;; under TTY, window-live-p also means window is visible
  (and (window-live-p wind)
       (or (not (ediff-window-display-p))
	   (frame-visible-p (window-frame wind)))))


(defsubst ediff-frame-char-width (frame)
  (if (featurep 'xemacs)
      (/ (frame-pixel-width frame) (frame-width frame))
    (frame-char-width frame)))

(defun ediff-reset-mouse (&optional frame do-not-grab-mouse)
  (or frame (setq frame (selected-frame)))
  (if (ediff-window-display-p)
      (let ((frame-or-wind frame))
	(if (featurep 'xemacs)
	    (setq frame-or-wind (frame-selected-window frame)))
	(or do-not-grab-mouse
	    ;; don't set mouse if the user said to never do this
	    (not ediff-grab-mouse)
	    ;; Don't grab on quit, if the user doesn't want to.
	    ;; If ediff-grab-mouse = t, then mouse won't be grabbed for
	    ;; sessions that are not part of a group (this is done in
	    ;; ediff-recenter).  The condition below affects only terminating
	    ;; sessions in session groups (in which case mouse is warped into
	    ;; a meta buffer).
	    (and (eq ediff-grab-mouse 'maybe)
		 (memq this-command '(ediff-quit ediff-update-diffs)))
	    (set-mouse-position frame-or-wind 1 0))
	)))

(defsubst ediff-spy-after-mouse ()
  (setq ediff-mouse-pixel-position (mouse-pixel-position)))

;; It is not easy to find out when the user grabs the mouse, since emacs and
;; xemacs behave differently when mouse is not in any frame.  Also, this is
;; sensitive to when the user grabbed mouse.  Not used for now.
(defun ediff-user-grabbed-mouse ()
  (if ediff-mouse-pixel-position
      (cond ((not (eq (car ediff-mouse-pixel-position)
		      (car (mouse-pixel-position)))))
	    ((and (car (cdr ediff-mouse-pixel-position))
		  (car (cdr (mouse-pixel-position)))
		  (cdr (cdr ediff-mouse-pixel-position))
		  (cdr (cdr (mouse-pixel-position))))
	     (not (and (< (abs (- (car (cdr ediff-mouse-pixel-position))
				  (car (cdr (mouse-pixel-position)))))
			  ediff-mouse-pixel-threshold)
		       (< (abs (- (cdr (cdr ediff-mouse-pixel-position))
				  (cdr (cdr (mouse-pixel-position)))))
			  ediff-mouse-pixel-threshold))))
	    (t nil))))

(defsubst ediff-frame-char-height (frame)
  (if (featurep 'xemacs)
      (glyph-height ediff-H-glyph (frame-selected-window frame))
    (frame-char-height frame)))

;; Some overlay functions

(defsubst ediff-overlay-start (overl)
  (if (ediff-overlayp overl)
      (if (featurep 'xemacs)
	  (extent-start-position overl)
	(overlay-start overl))))

(defsubst ediff-overlay-end  (overl)
  (if (ediff-overlayp overl)
      (if (featurep 'xemacs)
	  (extent-end-position overl)
	(overlay-end overl))))

(defsubst ediff-empty-overlay-p (overl)
  (= (ediff-overlay-start overl) (ediff-overlay-end overl)))

;; like overlay-buffer in Emacs.  In XEmacs, returns nil if the extent is
;; dead.  Otherwise, works like extent-buffer
(defun ediff-overlay-buffer (overl)
  (if (featurep 'xemacs)
      (and (extent-live-p overl) (extent-object overl))
    (overlay-buffer overl)))

;; like overlay-get in Emacs.  In XEmacs, returns nil if the extent is
;; dead.  Otherwise, like extent-property
(defun ediff-overlay-get (overl property)
  (if (featurep 'xemacs)
      (and (extent-live-p overl) (extent-property overl property))
    (overlay-get overl property)))


;; These two functions are here because XEmacs refuses to
;; handle overlays whose buffers were deleted.
(defun ediff-move-overlay (overlay beg end &optional buffer)
  "Calls `move-overlay' in Emacs and `set-extent-endpoints' in Lemacs.
Checks if overlay's buffer exists before actually doing the move."
  (let ((buf (and overlay (ediff-overlay-buffer overlay))))
    (if (ediff-buffer-live-p buf)
	(if (featurep 'xemacs)
	    (set-extent-endpoints overlay beg end)
	  (move-overlay overlay beg end buffer))
      ;; buffer's dead
      (if overlay
	  (ediff-delete-overlay overlay)))))

(defun ediff-overlay-put (overlay prop value)
  "Calls `overlay-put' or `set-extent-property' depending on Emacs version.
Checks if overlay's buffer exists."
  (if (ediff-buffer-live-p (ediff-overlay-buffer overlay))
      (if (featurep 'xemacs)
	  (set-extent-property overlay prop value)
	(overlay-put overlay prop value))
    (ediff-delete-overlay overlay)))

;; temporarily uses DIR to abbreviate file name
;; if DIR is nil, use default-directory
(defun ediff-abbreviate-file-name (file &optional dir)
  (cond ((stringp dir)
	 (let ((directory-abbrev-alist (list (cons dir ""))))
	   (abbreviate-file-name file)))
	(t
	 (if (featurep 'xemacs)
	     ;; XEmacs requires addl argument
	     (abbreviate-file-name file t)
	   (abbreviate-file-name file)))))

;; Takes a directory and returns the parent directory.
;; does nothing to `/'.  If the ARG is a regular file,
;; strip the file AND the last dir.
(defun ediff-strip-last-dir (dir)
  (if (not (stringp dir)) (setq dir default-directory))
  (setq dir (expand-file-name dir))
  (or (file-directory-p dir) (setq dir (file-name-directory dir)))
  (let* ((pos (1- (length dir)))
	 (last-char (aref dir pos)))
    (if (and (> pos 0) (= last-char ?/))
	(setq dir (substring dir 0 pos)))
    (ediff-abbreviate-file-name (file-name-directory dir))))

(defun ediff-truncate-string-left (str newlen)
  ;; leave space for ... on the left
  (let ((len (length str))
	substr)
    (if (<= len newlen)
	str
      (setq newlen (max 0 (- newlen 3)))
      (setq substr (substring str (max 0 (- len 1 newlen))))
      (concat "..." substr))))

(defsubst ediff-nonempty-string-p (string)
  (and (stringp string) (not (string= string ""))))

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
	  (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
	(setq i (1- i))
	(if (eq (aref newstr i) fromchar)
	    (aset newstr i tochar)))
      newstr)))

(defun ediff-abbrev-jobname (jobname)
  (cond ((eq jobname 'ediff-directories)
	 "Compare two directories")
	((eq jobname 'ediff-files)
	 "Compare two files")
	((eq jobname 'ediff-buffers)
	 "Compare two buffers")
	((eq jobname 'ediff-directories3)
	 "Compare three directories")
	((eq jobname 'ediff-files3)
	 "Compare three files")
	((eq jobname 'ediff-buffers3)
	 "Compare three buffers")
	((eq jobname 'ediff-revision)
	 "Compare file with a version")
	((eq jobname 'ediff-directory-revisions)
	 "Compare dir files with versions")
	((eq jobname 'ediff-merge-directory-revisions)
	 "Merge dir files with versions")
	((eq jobname 'ediff-merge-directory-revisions-with-ancestor)
	 "Merge dir versions via ancestors")
	(t
	 (capitalize
	  (subst-char-in-string ?- ?\s (substring (symbol-name jobname) 6))))
	))


;; If ediff modified mode line, strip the modification
(defsubst ediff-strip-mode-line-format ()
  (and (consp mode-line-format)
       (member (car mode-line-format)
	       '(" A: " " B: " " C: " " Ancestor: "))
       (setq mode-line-format (nth 2 mode-line-format))))

;; Verify that we have a difference selected.
(defsubst ediff-valid-difference-p (&optional n)
  (or n (setq n ediff-current-difference))
  (and (>= n 0) (< n ediff-number-of-differences)))

(defsubst ediff-show-all-diffs (n)
  "Don't skip difference regions."
  nil)

(defsubst ediff-message-if-verbose (string &rest args)
  (if ediff-verbose-p
      (apply 'message string args)))

(defun ediff-file-attributes (filename attr-number)
  (if (ediff-listable-file filename)
      (nth attr-number (file-attributes filename))
    -1)
  )

(defsubst ediff-file-size (filename)
  (ediff-file-attributes filename 7))
(defsubst ediff-file-modtime (filename)
  (ediff-file-attributes filename 5))


(defun ediff-convert-standard-filename (fname)
  (if (fboundp 'convert-standard-filename)
      (convert-standard-filename fname)
    fname))

(if (featurep 'emacs)
    (defalias 'ediff-with-syntax-table 'with-syntax-table)
  (if (fboundp 'with-syntax-table)
      (defalias 'ediff-with-syntax-table 'with-syntax-table)
    ;; stolen from subr.el in emacs 21
    (defmacro ediff-with-syntax-table (table &rest body)
      (let ((old-table (make-symbol "table"))
	    (old-buffer (make-symbol "buffer")))
	`(let ((,old-table (syntax-table))
	       (,old-buffer (current-buffer)))
	   (unwind-protect
	       (progn
		 (set-syntax-table (copy-syntax-table ,table))
		 ,@body)
	     (save-current-buffer
	       (set-buffer ,old-buffer)
	       (set-syntax-table ,old-table))))))))


(provide 'ediff-init)



;; Local Variables:
;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;; End:

;;; ediff-init.el ends here
