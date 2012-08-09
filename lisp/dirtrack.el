;;; dirtrack.el --- Directory Tracking by watching the prompt

;; Copyright (C) 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Peter Breton <pbreton@cs.umb.edu>
;; Created: Sun Nov 17 1996
;; Keywords: processes

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

;; Shell directory tracking by watching the prompt.
;;
;; This is yet another attempt at a directory-tracking package for
;; Emacs shell-mode.  However, this package makes one strong assumption:
;; that you can customize your shell's prompt to contain the
;; current working directory.  Most shells do support this, including
;; almost every type of Bourne and C shell on Unix, the native shells on
;; Windows95 (COMMAND.COM) and Windows NT (CMD.EXE), and most 3rd party
;; Windows shells.  If you cannot do this, or do not wish to, this package
;; will be useless to you.
;;
;; Installation:
;;
;; 1) Set your shell's prompt to contain the current working directory.
;; You may need to consult your shell's documentation to find out how to
;; do this.
;;
;; Note that directory tracking is done by matching regular expressions,
;; therefore it is *VERY IMPORTANT* for your prompt to be easily
;; distinguishable from other output.  If your prompt regexp is too general,
;; you will see error messages from the dirtrack filter as it attempts to cd
;; to non-existent directories.
;;
;; 2) Set the variable `dirtrack-list' to an appropriate value.  This
;; should be a list of two elements: the first is a regular expression
;; which matches your prompt up to and including the pathname part.
;; The second is a number which tells which regular expression group to
;; match to extract only the pathname.  If you use a multi-line prompt,
;; add 't' as a third element.  Note that some of the functions in
;; 'comint.el' assume a single-line prompt (eg, comint-bol).
;;
;; Determining this information may take some experimentation.  Using
;; `dirtrack-debug-mode' may help; it causes the directory-tracking
;; filter to log messages to the buffer `dirtrack-debug-buffer'.
;;
;; 3) Activate `dirtrack-mode'.  You may wish to turn ordinary shell
;; tracking off by calling `shell-dirtrack-mode'.
;;
;; Examples:
;;
;; 1) On Windows NT, my prompt is set to emacs$S$P$G.
;; 'dirtrack-list' is set to (list "^emacs \\([a-zA-Z]:.*\\)>" 1)
;;
;; 2) On Solaris running bash, my prompt is set like this:
;;    PS1="\w\012emacs@\h(\!) [\t]% "
;;    'dirtrack-list' is set to (list "^\\([/~].*\\)\nemacs@[^%]+% *" 1 t)
;;
;; I'd appreciate other examples from people who use this package.
;;
;; Here's one from Stephen Eglen:
;;
;;   Running under tcsh:
;;   (setq-default dirtrack-list '("^%E \\([^ ]+\\)" 1))
;;
;;   It might be worth mentioning in your file that emacs sources start up
;;   files of the form: ~/.emacs_<SHELL> where <SHELL> is the name of the
;;   shell.  So for example, I have the following in ~/.emacs_tcsh:
;;
;;   set prompt = "%%E %~ %h% "
;;
;;   This produces a prompt of the form:
;;   %E /var/spool 10%
;;
;;   This saves me from having to use the %E prefix in other non-emacs
;;   shells.
;;
;; A final note:
;;
;;   I run LOTS of shell buffers through Emacs, sometimes as different users
;;   (eg, when logged in as myself, I'll run a root shell in the same Emacs).
;;   If you do this, and the shell prompt contains a ~, Emacs will interpret
;;   this relative to the user which owns the Emacs process, not the user
;;   who owns the shell buffer.  This may cause dirtrack to behave strangely
;;   (typically it reports that it is unable to cd to a directory
;;   with a ~ in it).
;;
;;   The same behavior can occur if you use dirtrack with remote filesystems
;;   (using telnet, rlogin, etc) as Emacs will be checking the local
;;   filesystem, not the remote one.  This problem is not specific to dirtrack,
;;   but also affects file completion, etc.

;;; Code:

(eval-when-compile
  (require 'comint)
  (require 'shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup dirtrack nil
  "Directory tracking by watching the prompt."
  :prefix "dirtrack-"
  :group 'shell)

(defcustom dirtrack-list (list "^emacs \\([a-zA-Z]:.*\\)>" 1)
  "List for directory tracking.
First item is a regexp that describes where to find the path in a prompt.
Second is a number, the regexp group to match."
  :group 'dirtrack
  :type  '(sexp (regexp  :tag "Prompt Expression")
		(integer :tag "Regexp Group"))
  :version "24.1")

(make-variable-buffer-local 'dirtrack-list)

(defcustom dirtrack-debug nil
  "If non-nil, the function `dirtrack' will report debugging info."
  :group 'dirtrack
  :type  'boolean)

(defcustom dirtrack-debug-buffer "*Directory Tracking Log*"
  "Buffer in which to write directory tracking debug information."
  :group 'dirtrack
  :type  'string)

(defcustom dirtrack-directory-function
  (if (memq system-type '(ms-dos windows-nt cygwin))
      'dirtrack-windows-directory-function
    'file-name-as-directory)
  "Function to apply to the prompt directory for comparison purposes."
  :group 'dirtrack
  :type  'function)

(defcustom dirtrack-canonicalize-function
  (if (memq system-type '(ms-dos windows-nt cygwin))
      'downcase 'identity)
  "Function to apply to the default directory for comparison purposes."
  :group 'dirtrack
  :type  'function)

(defcustom dirtrack-directory-change-hook nil
  "Hook that is called when a directory change is made."
  :group 'dirtrack
  :type 'hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dirtrack-windows-directory-function (dir)
  "Return a canonical directory for comparison purposes.
Such a directory is all lowercase, has forward-slashes as delimiters,
and ends with a forward slash."
  (file-name-as-directory (downcase (subst-char-in-string ?\\ ?/ dir))))

(defun dirtrack-cygwin-directory-function (dir)
  "Return a canonical directory taken from a Cygwin path for comparison purposes."
  (if (string-match "/cygdrive/\\([A-Z]\\)\\(.*\\)" dir)
      (concat (match-string 1 dir) ":" (match-string 2 dir))
    dir))


;;;###autoload
(define-minor-mode dirtrack-mode
  "Toggle directory tracking in shell buffers (Dirtrack mode).
With a prefix argument ARG, enable Dirtrack mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This method requires that your shell prompt contain the current
working directory at all times, and that you set the variable
`dirtrack-list' to match the prompt.

This is an alternative to `shell-dirtrack-mode', which works by
tracking `cd' and similar commands which change the shell working
directory."
  nil nil nil
  (if dirtrack-mode
      (add-hook 'comint-preoutput-filter-functions 'dirtrack nil t)
    (remove-hook 'comint-preoutput-filter-functions 'dirtrack t)))

(define-obsolete-function-alias 'dirtrack-toggle 'dirtrack-mode "23.1")
(define-obsolete-variable-alias 'dirtrackp 'dirtrack-mode "23.1")


(define-minor-mode dirtrack-debug-mode
  "Toggle Dirtrack debugging.
With a prefix argument ARG, enable Dirtrack debugging if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil nil nil
  (if dirtrack-debug-mode
      (display-buffer (get-buffer-create dirtrack-debug-buffer))))

(define-obsolete-function-alias 'dirtrack-debug-toggle 'dirtrack-debug-mode
  "23.1")
(define-obsolete-variable-alias 'dirtrack-debug 'dirtrack-debug-mode "23.1")


(defun dirtrack-debug-message (msg1 msg2)
  "Insert strings at the end of `dirtrack-debug-buffer'."
  (when dirtrack-debug-mode
    (with-current-buffer (get-buffer-create dirtrack-debug-buffer)
      (goto-char (point-max))
      (insert msg1 msg2 "\n"))))

;;;###autoload
(defun dirtrack (input)
  "Determine the current directory from the process output for a prompt.
This filter function is used by `dirtrack-mode'.  It looks for
the prompt specified by `dirtrack-list', and calls
`shell-process-cd' if the directory seems to have changed away
from `default-directory'."
  (when (and dirtrack-mode
	     (not (eq (point) (point-min)))) ; there must be output
    (save-excursion ; What's this for? -- cyd
      (if (not (string-match (nth 0 dirtrack-list) input))
	  ;; No match
	  (dirtrack-debug-message
	   "Input failed to match `dirtrack-list': " input)
	(let ((prompt-path (match-string (nth 1 dirtrack-list) input))
	      temp)
	  (cond
	   ;; Don't do anything for empty string
	   ((string-equal prompt-path "")
	    (dirtrack-debug-message "Prompt match gives empty string: " input))
	   ;; If the prompt contains an absolute file name, call
	   ;; `shell-process-cd' if the directory has changed.
	   ((file-name-absolute-p prompt-path)
	    ;; Transform prompts into canonical forms
	    (let ((orig-prompt-path (funcall dirtrack-directory-function
					     prompt-path))
		  (current-dir      (funcall dirtrack-canonicalize-function
					     default-directory)))
	      (setq prompt-path (shell-prefixed-directory-name orig-prompt-path))
	      ;; Compare them
	      (if (or (string-equal current-dir prompt-path)
		      (string-equal (expand-file-name current-dir)
				    (expand-file-name prompt-path)))
		  (dirtrack-debug-message "Not changing directory: " current-dir)
		;; It's possible that Emacs thinks the directory
		;; doesn't exist (e.g. rlogin buffers)
		(if (file-accessible-directory-p prompt-path)
		    ;; `shell-process-cd' adds the prefix, so we need
		    ;; to give it the original (un-prefixed) path.
		    (progn
		      (shell-process-cd orig-prompt-path)
		      (run-hooks 'dirtrack-directory-change-hook)
		      (dirtrack-debug-message "Changing directory to "
					      prompt-path))
		  (dirtrack-debug-message "Not changing to non-existent directory: "
					  prompt-path)))))
	   ;; If the file name is non-absolute, try and see if it
	   ;; seems to be up or down from where we were.
	   ((string-match "\\`\\(.*\\)\\(?:/.*\\)?\n\\(.*/\\)\\1\\(?:/.*\\)?\\'"
			  (setq temp
				(concat prompt-path "\n" default-directory)))
	    (shell-process-cd (concat (match-string 2 temp)
				      prompt-path))
	    (run-hooks 'dirtrack-directory-change-hook)))))))
  input)

(provide 'dirtrack)

;;; dirtrack.el ends here
