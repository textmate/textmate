;;; whitespace.el --- warn about and clean bogus whitespaces in the file

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Rajesh Vaidheeswarran <rv@gnu.org>
;; Keywords: convenience
;; Obsolete-since: 23.1

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

;; URL: http://www.dsmit.com/lisp/
;;
;; The whitespace library is intended to find and help fix five different types
;; of whitespace problems that commonly exist in source code.
;;
;; 1. Leading space (empty lines at the top of a file).
;; 2. Trailing space (empty lines at the end of a file).
;; 3. Indentation space (8 or more spaces at beginning of line, that should be
;;		      replaced with TABS).
;; 4. Spaces followed by a TAB.  (Almost always, we never want that).
;; 5. Spaces or TABS at the end of a line.
;;
;; Whitespace errors are reported in a buffer, and on the modeline.
;;
;; Modeline will show a W:<x>!<y> to denote a particular type of whitespace,
;; where `x' and `y' can be one (or more) of:
;;
;; e - End-of-Line whitespace.
;; i - Indentation whitespace.
;; l - Leading whitespace.
;; s - Space followed by Tab.
;; t - Trailing whitespace.
;;
;; If any of the whitespace checks is turned off, the modeline will display a
;; !<y>.
;;
;;     (since (3) is the most controversial one, here is the rationale: Most
;;     terminal drivers and printer drivers have TAB configured or even
;;     hardcoded to be 8 spaces.  (Some of them allow configuration, but almost
;;     always they default to 8.)
;;
;;     Changing `tab-width' to other than 8 and editing will cause your code to
;;     look different from within Emacs, and say, if you cat it or more it, or
;;     even print it.
;;
;;     Almost all the popular programming modes let you define an offset (like
;;     c-basic-offset or perl-indent-level) to configure the offset, so you
;;     should never have to set your `tab-width' to be other than 8 in all
;;     these modes.  In fact, with an indent level of say, 4, 2 TABS will cause
;;     Emacs to replace your 8 spaces with one \t (try it).  If vi users in
;;     your office complain, tell them to use vim, which distinguishes between
;;     tabstop and shiftwidth (vi equivalent of our offsets), and also ask them
;;     to set smarttab.)
;;
;; All the above have caused (and will cause) unwanted codeline integration and
;; merge problems.
;;
;; whitespace.el will complain if it detects whitespaces on opening a file, and
;; warn you on closing a file also (in case you had inserted any
;; whitespaces during the process of your editing).
;;
;; Exported functions:
;;
;; `whitespace-buffer' - To check the current buffer for whitespace problems.
;; `whitespace-cleanup' - To cleanup all whitespaces in the current buffer.
;; `whitespace-region' - To check between point and mark for whitespace
;;                       problems.
;; `whitespace-cleanup-region' - To cleanup all whitespaces between point
;;                               and mark in the current buffer.

;;; Code:

(defvar whitespace-version "3.5" "Version of the whitespace library.")

(defvar whitespace-all-buffer-files nil
  "An associated list of buffers and files checked for whitespace cleanliness.

This is to enable periodic checking of whitespace cleanliness in the files
visited by the buffers.")

(defvar whitespace-rescan-timer nil
  "Timer object used to rescan the files in buffers that have been modified.")

;; Tell Emacs about this new kind of minor mode
(defvar whitespace-mode nil
  "Non-nil when Whitespace mode (a minor mode) is enabled.")
(make-variable-buffer-local 'whitespace-mode)

(defvar whitespace-mode-line nil
  "String to display in the mode line for Whitespace mode.")
(make-variable-buffer-local 'whitespace-mode-line)

(defvar whitespace-check-buffer-leading nil
  "Test leading whitespace for file in current buffer if t.")
(make-variable-buffer-local 'whitespace-check-buffer-leading)
;;;###autoload(put 'whitespace-check-buffer-leading 'safe-local-variable 'booleanp)

(defvar whitespace-check-buffer-trailing nil
  "Test trailing whitespace for file in current buffer if t.")
(make-variable-buffer-local 'whitespace-check-buffer-trailing)
;;;###autoload(put 'whitespace-check-buffer-trailing 'safe-local-variable 'booleanp)

(defvar whitespace-check-buffer-indent nil
  "Test indentation whitespace for file in current buffer if t.")
(make-variable-buffer-local 'whitespace-check-buffer-indent)
;;;###autoload(put 'whitespace-check-buffer-indent 'safe-local-variable 'booleanp)

(defvar whitespace-check-buffer-spacetab nil
  "Test Space-followed-by-TABS whitespace for file in current buffer if t.")
(make-variable-buffer-local 'whitespace-check-buffer-spacetab)
;;;###autoload(put 'whitespace-check-buffer-spacetab 'safe-local-variable 'booleanp)

(defvar whitespace-check-buffer-ateol nil
  "Test end-of-line whitespace for file in current buffer if t.")
(make-variable-buffer-local 'whitespace-check-buffer-ateol)
;;;###autoload(put 'whitespace-check-buffer-ateol 'safe-local-variable 'booleanp)

(defvar whitespace-highlighted-space nil
  "The variable to store the extent to highlight.")
(make-variable-buffer-local 'whitespace-highlighted-space)

(defalias 'whitespace-make-overlay
  (if (featurep 'xemacs) 'make-extent 'make-overlay))
(defalias 'whitespace-overlay-put
  (if (featurep 'xemacs) 'set-extent-property 'overlay-put))
(defalias 'whitespace-delete-overlay
  (if (featurep 'xemacs) 'delete-extent 'delete-overlay))
(defalias 'whitespace-overlay-start
  (if (featurep 'xemacs) 'extent-start 'overlay-start))
(defalias 'whitespace-overlay-end
  (if (featurep 'xemacs) 'extent-end 'overlay-end))
(defalias 'whitespace-mode-line-update
  (if (featurep 'xemacs) 'redraw-modeline 'force-mode-line-update))

(defgroup whitespace nil
  "Check for and fix five different types of whitespaces in source code."
  :version "21.1"
  :link '(emacs-commentary-link "whitespace.el")
  ;; Since XEmacs doesn't have a 'convenience group, use the next best group
  ;; which is 'editing?
  :group (if (featurep 'xemacs) 'editing 'convenience))

(defcustom whitespace-check-leading-whitespace t
  "Flag to check leading whitespace.  This is the global for the system.
It can be overridden by setting a buffer local variable
`whitespace-check-buffer-leading'."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-check-trailing-whitespace t
  "Flag to check trailing whitespace.  This is the global for the system.
It can be overridden by setting a buffer local variable
`whitespace-check-buffer-trailing'."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-check-spacetab-whitespace t
  "Flag to check space followed by a TAB.  This is the global for the system.
It can be overridden by setting a buffer local variable
`whitespace-check-buffer-spacetab'."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-spacetab-regexp "[ ]+\t"
  "Regexp to match one or more spaces followed by a TAB."
  :type 'regexp
  :group 'whitespace)

(defcustom whitespace-check-indent-whitespace indent-tabs-mode
  "Flag to check indentation whitespace.  This is the global for the system.
It can be overridden by setting a buffer local variable
`whitespace-check-buffer-indent'."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-indent-regexp "^\t*\\(        \\)+"
  "Regexp to match multiples of eight spaces near line beginnings.
The default value ignores leading TABs."
  :type 'regexp
  :group 'whitespace)

(defcustom whitespace-check-ateol-whitespace t
  "Flag to check end-of-line whitespace.  This is the global for the system.
It can be overridden by setting a buffer local variable
`whitespace-check-buffer-ateol'."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-ateol-regexp "[ \t]+$"
  "Regexp to match one or more TABs or spaces at line ends."
  :type 'regexp
  :group 'whitespace)

(defcustom whitespace-errbuf "*Whitespace Errors*"
  "The name of the buffer where whitespace related messages will be logged."
  :type 'string
  :group 'whitespace)

(defcustom whitespace-clean-msg "clean."
  "If non-nil, this message will be displayed after a whitespace check
determines a file to be clean."
  :type 'string
  :group 'whitespace)

(defcustom whitespace-abort-on-error nil
  "While writing a file, abort if the file is unclean.
If `whitespace-auto-cleanup' is set, that takes precedence over
this variable."
  :type  'boolean
  :group 'whitespace)

(defcustom whitespace-auto-cleanup nil
  "Cleanup a buffer automatically on finding it whitespace unclean."
  :type  'boolean
  :group 'whitespace)

(defcustom whitespace-silent nil
  "All whitespace errors will be shown only in the modeline when t.

Note that setting this may cause all whitespaces introduced in a file to go
unnoticed when the buffer is killed, unless the user visits the `*Whitespace
Errors*' buffer before opening (or closing) another file."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-modes '(ada-mode asm-mode autoconf-mode awk-mode
				       c-mode c++-mode cc-mode
				       change-log-mode cperl-mode
				       electric-nroff-mode emacs-lisp-mode
				       f90-mode fortran-mode html-mode
				       html3-mode java-mode jde-mode
				       ksh-mode latex-mode LaTeX-mode
				       lisp-mode m4-mode makefile-mode
				       modula-2-mode nroff-mode objc-mode
				       pascal-mode perl-mode prolog-mode
				       python-mode scheme-mode sgml-mode
				       sh-mode shell-script-mode simula-mode
				       tcl-mode tex-mode texinfo-mode
				       vrml-mode xml-mode)

  "Major modes in which we turn on whitespace checking.

These are mostly programming and documentation modes.  But you may add other
modes that you want whitespaces checked in by adding something like the
following to your `.emacs':

\(setq whitespace-modes (cons 'my-mode (cons 'my-other-mode
					    whitespace-modes))\)

Or, alternately, you can use the Emacs `customize' command to set this."
  :type '(repeat symbol)
  :group 'whitespace)

(defcustom whitespace-rescan-timer-time 600
  "Period in seconds to rescan modified buffers for whitespace creep.

This is the period after which the timer will fire causing
`whitespace-rescan-files-in-buffers' to check for whitespace creep in
modified buffers.

To disable timer scans, set this to zero."
  :type 'integer
  :group 'whitespace)

(defcustom whitespace-display-in-modeline t
  "Display whitespace errors on the modeline."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-display-spaces-in-color t
  "Display the bogus whitespaces by coloring them with the face
`whitespace-highlight'."
  :type 'boolean
  :group 'whitespace)

(defgroup whitespace-faces nil
  "Faces used in whitespace."
  :prefix "whitespace-"
  :group 'whitespace
  :group 'faces)

(defface whitespace-highlight '((((class color) (background light))
				 (:background "green1"))
				(((class color) (background dark))
				 (:background "sea green"))
				(((class grayscale mono)
				  (background light))
				 (:background "black"))
				(((class grayscale mono)
				  (background dark))
				 (:background "white")))
  "Face used for highlighting the bogus whitespaces that exist in the buffer."
  :group 'whitespace-faces)
(define-obsolete-face-alias 'whitespace-highlight-face
  'whitespace-highlight "22.1")

(if (not (assoc 'whitespace-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(whitespace-mode whitespace-mode-line)
				 minor-mode-alist)))

(set-default 'whitespace-check-buffer-leading
	     whitespace-check-leading-whitespace)
(set-default 'whitespace-check-buffer-trailing
	     whitespace-check-trailing-whitespace)
(set-default 'whitespace-check-buffer-indent
	     whitespace-check-indent-whitespace)
(set-default 'whitespace-check-buffer-spacetab
	     whitespace-check-spacetab-whitespace)
(set-default 'whitespace-check-buffer-ateol
	     whitespace-check-ateol-whitespace)

(defun whitespace-check-whitespace-mode (&optional arg)
  "Test and set the whitespace-mode in qualifying buffers."
  (if (null whitespace-mode)
      (setq whitespace-mode
	    (if (or arg (member major-mode whitespace-modes))
		t
	      nil))))

;;;###autoload
(defun whitespace-toggle-leading-check ()
  "Toggle the check for leading space in the local buffer."
  (interactive)
  (let ((current-val whitespace-check-buffer-leading))
    (setq whitespace-check-buffer-leading (not current-val))
    (message "Will%s check for leading space in buffer."
	     (if whitespace-check-buffer-leading "" " not"))
    (if whitespace-check-buffer-leading (whitespace-buffer-leading))))

;;;###autoload
(defun whitespace-toggle-trailing-check ()
  "Toggle the check for trailing space in the local buffer."
  (interactive)
  (let ((current-val whitespace-check-buffer-trailing))
    (setq whitespace-check-buffer-trailing (not current-val))
    (message "Will%s check for trailing space in buffer."
	     (if whitespace-check-buffer-trailing "" " not"))
    (if whitespace-check-buffer-trailing (whitespace-buffer-trailing))))

;;;###autoload
(defun whitespace-toggle-indent-check ()
  "Toggle the check for indentation space in the local buffer."
  (interactive)
  (let ((current-val whitespace-check-buffer-indent))
    (setq whitespace-check-buffer-indent (not current-val))
    (message "Will%s check for indentation space in buffer."
	     (if whitespace-check-buffer-indent "" " not"))
    (if whitespace-check-buffer-indent
	(whitespace-buffer-search whitespace-indent-regexp))))

;;;###autoload
(defun whitespace-toggle-spacetab-check ()
  "Toggle the check for space-followed-by-TABs in the local buffer."
  (interactive)
  (let ((current-val whitespace-check-buffer-spacetab))
    (setq whitespace-check-buffer-spacetab (not current-val))
    (message "Will%s check for space-followed-by-TABs in buffer."
	     (if whitespace-check-buffer-spacetab "" " not"))
    (if whitespace-check-buffer-spacetab
	(whitespace-buffer-search whitespace-spacetab-regexp))))


;;;###autoload
(defun whitespace-toggle-ateol-check ()
  "Toggle the check for end-of-line space in the local buffer."
  (interactive)
  (let ((current-val whitespace-check-buffer-ateol))
    (setq whitespace-check-buffer-ateol (not current-val))
    (message "Will%s check for end-of-line space in buffer."
	     (if whitespace-check-buffer-ateol "" " not"))
    (if whitespace-check-buffer-ateol
	(whitespace-buffer-search whitespace-ateol-regexp))))


;;;###autoload
(defun whitespace-buffer (&optional quiet)
  "Find five different types of white spaces in buffer.
These are:
1. Leading space \(empty lines at the top of a file\).
2. Trailing space \(empty lines at the end of a file\).
3. Indentation space \(8 or more spaces, that should be replaced with TABS\).
4. Spaces followed by a TAB. \(Almost always, we never want that\).
5. Spaces or TABS at the end of a line.

Check for whitespace only if this buffer really contains a non-empty file
and:
1. the major mode is one of the whitespace-modes, or
2. `whitespace-buffer' was explicitly called with a prefix argument."
  (interactive)
  (let ((whitespace-error nil))
    (whitespace-check-whitespace-mode current-prefix-arg)
    (if (and buffer-file-name (> (buffer-size) 0) whitespace-mode)
	(progn
	  (whitespace-check-buffer-list (buffer-name) buffer-file-name)
	  (whitespace-tickle-timer)
	  (overlay-recenter (point-max))
	  (remove-overlays nil nil 'face 'whitespace-highlight)
	  (if whitespace-auto-cleanup
	      (if buffer-read-only
		  (if (not quiet)
		      (message "Can't cleanup: %s is read-only" (buffer-name)))
		(whitespace-cleanup-internal))
	    (let ((whitespace-leading (if whitespace-check-buffer-leading
					  (whitespace-buffer-leading)
					nil))
		  (whitespace-trailing (if whitespace-check-buffer-trailing
					   (whitespace-buffer-trailing)
					 nil))
		  (whitespace-indent (if whitespace-check-buffer-indent
					 (whitespace-buffer-search
					  whitespace-indent-regexp)
				       nil))
		  (whitespace-spacetab (if whitespace-check-buffer-spacetab
					   (whitespace-buffer-search
					    whitespace-spacetab-regexp)
					 nil))
		  (whitespace-ateol (if whitespace-check-buffer-ateol
					(whitespace-buffer-search
					 whitespace-ateol-regexp)
				      nil))
		  (whitespace-errmsg nil)
		  (whitespace-filename buffer-file-name)
		  (whitespace-this-modeline ""))

	      ;; Now let's complain if we found any of the above.
	      (setq whitespace-error (or whitespace-leading whitespace-indent
					 whitespace-spacetab whitespace-ateol
					 whitespace-trailing))

	      (if whitespace-error
		  (progn
		    (setq whitespace-errmsg
			  (concat whitespace-filename " contains:\n"
				  (if whitespace-leading
				      "Leading whitespace\n")
				  (if whitespace-indent
				      (concat "Indentation whitespace"
					      whitespace-indent "\n"))
				  (if whitespace-spacetab
				      (concat "Space followed by Tab"
					      whitespace-spacetab "\n"))
				  (if whitespace-ateol
				      (concat "End-of-line whitespace"
					      whitespace-ateol "\n"))
				  (if whitespace-trailing
				      "Trailing whitespace\n")
				  "\ntype `M-x whitespace-cleanup' to "
				  "cleanup the file."))
		    (setq whitespace-this-modeline
			  (concat (if whitespace-ateol "e")
				  (if whitespace-indent "i")
				  (if whitespace-leading "l")
				  (if whitespace-spacetab "s")
				  (if whitespace-trailing "t")))))
	      (whitespace-update-modeline whitespace-this-modeline)
	      (if (get-buffer whitespace-errbuf)
		  (kill-buffer whitespace-errbuf))
	      (with-current-buffer (get-buffer-create whitespace-errbuf)
		(if whitespace-errmsg
		    (progn
		      (insert whitespace-errmsg)
		      (if (not (or quiet whitespace-silent))
			  (display-buffer (current-buffer) t))
		      (if (not quiet)
			  (message "Whitespaces: [%s%s] in %s"
				   whitespace-this-modeline
				   (let ((whitespace-unchecked
					  (whitespace-unchecked-whitespaces)))
				     (if whitespace-unchecked
					 (concat "!" whitespace-unchecked)
				       ""))
				   whitespace-filename)))
		  (if (and (not quiet) (not (equal whitespace-clean-msg "")))
		      (message "%s %s" whitespace-filename
			       whitespace-clean-msg))))))))
    whitespace-error))

;;;###autoload
(defun whitespace-region (s e)
  "Check the region for whitespace errors."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region s e)
      (whitespace-buffer))))

;;;###autoload
(defun whitespace-cleanup ()
  "Cleanup the five different kinds of whitespace problems.
It normally applies to the whole buffer, but in Transient Mark mode
when the mark is active it applies to the region.
See `whitespace-buffer' docstring for a summary of the problems."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (whitespace-cleanup-region (region-beginning) (region-end))
    (whitespace-cleanup-internal)))

(defun whitespace-cleanup-internal (&optional region-only)
  ;; If this buffer really contains a file, then run, else quit.
  (whitespace-check-whitespace-mode current-prefix-arg)
  (if (and buffer-file-name whitespace-mode)
      (let ((whitespace-any nil)
	    (whitespace-tabwidth 8)
	    (whitespace-tabwidth-saved tab-width))

	;; since all printable TABS should be 8, irrespective of how
	;; they are displayed.
	(setq tab-width whitespace-tabwidth)

	(if (and whitespace-check-buffer-leading
		 (whitespace-buffer-leading))
	    (progn
	      (whitespace-buffer-leading-cleanup)
	      (setq whitespace-any t)))

	(if (and whitespace-check-buffer-trailing
		 (whitespace-buffer-trailing))
	    (progn
	      (whitespace-buffer-trailing-cleanup)
	      (setq whitespace-any t)))

	(if (and whitespace-check-buffer-indent
		 (whitespace-buffer-search whitespace-indent-regexp))
	    (progn
	      (whitespace-indent-cleanup)
	      (setq whitespace-any t)))

	(if (and whitespace-check-buffer-spacetab
		 (whitespace-buffer-search whitespace-spacetab-regexp))
	    (progn
	      (whitespace-buffer-cleanup whitespace-spacetab-regexp "\t")
	      (setq whitespace-any t)))

	(if (and whitespace-check-buffer-ateol
		 (whitespace-buffer-search whitespace-ateol-regexp))
	    (progn
	      (whitespace-buffer-cleanup whitespace-ateol-regexp "")
	      (setq whitespace-any t)))

	;; Call this recursively till everything is taken care of
	(if whitespace-any
	    (whitespace-cleanup-internal region-only)
	  ;; if we are done, talk to the user
	  (progn
	    (unless whitespace-silent
	      (if region-only
		  (message "The region is now clean")
		(message "%s is now clean" buffer-file-name)))
	    (whitespace-update-modeline)))
	(setq tab-width whitespace-tabwidth-saved))))

;;;###autoload
(defun whitespace-cleanup-region (s e)
  "Whitespace cleanup on the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region s e)
      (whitespace-cleanup-internal t))
    (whitespace-buffer t)))

(defun whitespace-buffer-leading ()
  "Return t if the current buffer has leading newline characters.
If highlighting is enabled, highlight these characters."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "\n")
    (unless (bobp)
      (whitespace-highlight-the-space (point-min) (point))
      t)))

(defun whitespace-buffer-leading-cleanup ()
  "Remove any leading newline characters from current buffer."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "\n")
    (delete-region (point-min) (point))))

(defun whitespace-buffer-trailing ()
  "Return t if the current buffer has extra trailing newline characters.
If highlighting is enabled, highlight these characters."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (forward-line)
    (unless (eobp)
      (whitespace-highlight-the-space (point) (point-max))
      t)))

(defun whitespace-buffer-trailing-cleanup ()
  "Remove extra trailing newline characters from current buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (unless (eobp)
      (forward-line)
      (delete-region (point) (point-max)))))

(defun whitespace-buffer-search (regexp)
  "Search for any given whitespace REGEXP."
  (with-local-quit
    (let (whitespace-retval)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (whitespace-highlight-the-space (match-beginning 0) (match-end 0))
	  (push (match-beginning 0) whitespace-retval)))
      (when whitespace-retval
	(format " %s" (nreverse whitespace-retval))))))

(defun whitespace-buffer-cleanup (regexp newregexp)
  "Search for any given whitespace REGEXP and replace it with the NEWREGEXP."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newregexp))))

(defun whitespace-indent-cleanup ()
  "Search for 8/more spaces at the start of a line and replace it with tabs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward whitespace-indent-regexp nil t)
      (let ((column (current-column))
	    (indent-tabs-mode t))
	(delete-region (match-beginning 0) (point))
	(indent-to column)))))

(defun whitespace-unchecked-whitespaces ()
  "Return the list of whitespaces whose testing has been suppressed."
  (let ((unchecked-spaces
	 (concat (if (not whitespace-check-buffer-ateol) "e")
		 (if (not whitespace-check-buffer-indent) "i")
		 (if (not whitespace-check-buffer-leading) "l")
		 (if (not whitespace-check-buffer-spacetab) "s")
		 (if (not whitespace-check-buffer-trailing) "t"))))
    (if (not (equal unchecked-spaces ""))
	unchecked-spaces
      nil)))

(defun whitespace-update-modeline (&optional whitespace-err)
  "Update modeline with whitespace errors.
Also with whitespaces whose testing has been turned off."
  (if whitespace-display-in-modeline
      (progn
	(setq whitespace-mode-line nil)
	;; Whitespace errors
	(if (and whitespace-err (not (equal whitespace-err "")))
	    (setq whitespace-mode-line whitespace-err))
	;; Whitespace suppressed errors
	(let ((whitespace-unchecked (whitespace-unchecked-whitespaces)))
	  (if whitespace-unchecked
	      (setq whitespace-mode-line
		    (concat whitespace-mode-line "!" whitespace-unchecked))))
	;; Add the whitespace modeline prefix
	(setq whitespace-mode-line (if whitespace-mode-line
				       (concat " W:" whitespace-mode-line)
				     nil))
	(whitespace-mode-line-update))))

(defun whitespace-highlight-the-space (b e)
  "Highlight the current line, unhighlighting a previously jumped to line."
  (if whitespace-display-spaces-in-color
      (let ((ol (whitespace-make-overlay b e)))
	(whitespace-overlay-put ol 'face 'whitespace-highlight))))

(defun whitespace-unhighlight-the-space()
  "Unhighlight the currently highlight line."
  (if (and whitespace-display-spaces-in-color whitespace-highlighted-space)
      (progn
	(mapc 'whitespace-delete-overlay whitespace-highlighted-space)
	(setq whitespace-highlighted-space nil))))

(defun whitespace-check-buffer-list (buf-name buf-file)
  "Add a buffer and its file to the whitespace monitor list.

The buffer named BUF-NAME and its associated file BUF-FILE are now monitored
periodically for whitespace."
  (if (and whitespace-mode (not (member (list buf-file buf-name)
					whitespace-all-buffer-files)))
      (add-to-list 'whitespace-all-buffer-files (list buf-file buf-name))))

(defun whitespace-tickle-timer ()
  "Tickle timer to periodically to scan qualifying files for whitespace creep.

If timer is not set, then set it to scan the files in
`whitespace-all-buffer-files' periodically (defined by
`whitespace-rescan-timer-time') for whitespace creep."
  (if (and whitespace-rescan-timer-time
	   (/= whitespace-rescan-timer-time 0)
	   (not whitespace-rescan-timer))
      (setq whitespace-rescan-timer
	    (add-timeout whitespace-rescan-timer-time
			 'whitespace-rescan-files-in-buffers nil
			 whitespace-rescan-timer-time))))

(defun whitespace-rescan-files-in-buffers (&optional arg)
  "Check monitored files for whitespace creep since last scan."
  (let ((whitespace-all-my-files whitespace-all-buffer-files)
	buffile bufname thiselt buf)
    (if (not whitespace-all-my-files)
	(progn
	  (disable-timeout whitespace-rescan-timer)
	  (setq whitespace-rescan-timer nil))
      (while whitespace-all-my-files
	(setq thiselt (car whitespace-all-my-files))
	(setq whitespace-all-my-files (cdr whitespace-all-my-files))
	(setq buffile (car thiselt))
	(setq bufname (cadr thiselt))
	(setq buf (get-buffer bufname))
	(if (buffer-live-p buf)
	    (with-current-buffer bufname
	      ;;(message "buffer %s live" bufname)
	      (if whitespace-mode
		  (progn
		    ;;(message "checking for whitespace in %s" bufname)
		    (if whitespace-auto-cleanup
			(progn
			  ;;(message "cleaning up whitespace in %s" bufname)
			  (whitespace-cleanup-internal))
		      (progn
			;;(message "whitespace-buffer %s." (buffer-name))
			(whitespace-buffer t))))
		;;(message "Removing %s from refresh list" bufname)
		(whitespace-refresh-rescan-list buffile bufname)))
	  ;;(message "Removing %s from refresh list" bufname)
	  (whitespace-refresh-rescan-list buffile bufname))))))

(defun whitespace-refresh-rescan-list (buffile bufname)
  "Refresh the list of files to be rescanned for whitespace creep."
  (if whitespace-all-buffer-files
      (setq whitespace-all-buffer-files
	    (delete (list buffile bufname) whitespace-all-buffer-files))
    (when whitespace-rescan-timer
      (disable-timeout whitespace-rescan-timer)
      (setq whitespace-rescan-timer nil))))

;;;###autoload
(defalias 'global-whitespace-mode 'whitespace-global-mode)

;;;###autoload
(define-minor-mode whitespace-global-mode
  "Toggle using Whitespace mode in new buffers.
With ARG, turn the mode on if ARG is positive, otherwise turn it off.

When this mode is active, `whitespace-buffer' is added to
`find-file-hook' and `kill-buffer-hook'."
  :global t
  :group 'whitespace
  (if whitespace-global-mode
      (progn
	(add-hook 'find-file-hook 'whitespace-buffer)
	(add-hook 'write-file-functions 'whitespace-write-file-hook nil t)
	(add-hook 'kill-buffer-hook 'whitespace-buffer))
    (remove-hook 'find-file-hook 'whitespace-buffer)
    (remove-hook 'write-file-functions 'whitespace-write-file-hook t)
    (remove-hook 'kill-buffer-hook 'whitespace-buffer)))

;;;###autoload
(defun whitespace-write-file-hook ()
  "Hook function to be called on the buffer when whitespace check is enabled.
This is meant to be added buffer-locally to `write-file-functions'."
  (let ((werr nil))
    (if whitespace-auto-cleanup
	(whitespace-cleanup-internal)
      (setq werr (whitespace-buffer)))
    (if (and whitespace-abort-on-error werr)
	(error "Abort write due to whitespaces in %s"
		       buffer-file-name)))
  nil)

(defun whitespace-unload-function ()
  "Unload the whitespace library."
  (if (unintern "whitespace-unload-hook" obarray)
      ;; if whitespace-unload-hook is defined, let's get rid of it
      ;; and recursively call `unload-feature'
      (progn (unload-feature 'whitespace) t)
    ;; this only happens in the recursive call
    (whitespace-global-mode -1)
    (save-current-buffer
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(remove-hook 'write-file-functions 'whitespace-write-file-hook t)))
    ;; continue standard unloading
    nil))

(defun whitespace-unload-hook ()
  (remove-hook 'find-file-hook 'whitespace-buffer)
  (remove-hook 'write-file-functions 'whitespace-write-file-hook t)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer))

(add-hook 'whitespace-unload-hook 'whitespace-unload-hook)

(provide 'whitespace)

;;; whitespace.el ends here
