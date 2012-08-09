;;; comint.el --- general command interpreter in a window stuff -*- lexical-binding: t -*-

;; Copyright (C) 1988, 1990, 1992-2012  Free Software Foundation, Inc.

;; Author: Olin Shivers <shivers@cs.cmu.edu>
;;	Simon Marshall <simon@gnu.org>
;; Maintainer: FSF
;; Keywords: processes
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

;; This file defines a general command-interpreter-in-a-buffer package
;; (comint mode).  The idea is that you can build specific process-in-a-buffer
;; modes on top of comint mode -- e.g., Lisp, shell, scheme, T, soar, ....
;; This way, all these specific packages share a common base functionality,
;; and a common set of bindings, which makes them easier to use (and
;; saves code, implementation time, etc., etc.).

;; Several packages are already defined using comint mode:
;; - shell.el defines a shell-in-a-buffer mode.
;; - cmulisp.el defines a simple lisp-in-a-buffer mode.
;;
;; - The file cmuscheme.el defines a scheme-in-a-buffer mode.
;; - The file tea.el tunes scheme and inferior-scheme modes for T.
;; - The file soar.el tunes Lisp and inferior-lisp modes for Soar.
;; - cmutex.el defines TeX and LaTeX modes that invoke TeX, LaTeX, BibTeX,
;;   previewers, and printers from within Emacs.
;; - background.el allows csh-like job control inside Emacs.
;; It is pretty easy to make new derived modes for other processes.

;; For documentation on the functionality provided by Comint mode, and
;; the hooks available for customizing it, see the comments below.
;; For further information on the standard derived modes (shell,
;; inferior-lisp, inferior-scheme, ...), see the relevant source files.

;; For hints on converting existing process modes (e.g., tex-mode,
;; background, dbx, gdb, kermit, prolog, telnet) to use comint-mode
;; instead of shell-mode, see the notes at the end of this file.


;; Brief Command Documentation:
;;============================================================================
;; Comint Mode Commands: (common to all derived modes, like shell & cmulisp
;; mode)
;;
;; M-p	   comint-previous-input	   Cycle backwards in input history
;; M-n	   comint-next-input		   Cycle forwards
;; M-r     comint-history-isearch-backward-regexp  Isearch input regexp backward
;; M-C-l   comint-show-output		   Show last batch of process output
;; RET	   comint-send-input
;; C-d	   comint-delchar-or-maybe-eof     Delete char unless at end of buff
;; C-c C-a comint-bol-or-process-mark      First time, move point to bol;
;;					    second time, move to process-mark.
;; C-c C-u comint-kill-input		    ^u
;; C-c C-w backward-kill-word		    ^w
;; C-c C-c comint-interrupt-subjob	    ^c
;; C-c C-z comint-stop-subjob		    ^z
;; C-c C-\ comint-quit-subjob		    ^\
;; C-c C-o comint-delete-output		    Delete last batch of process output
;; C-c C-r comint-show-output		    Show last batch of process output
;; C-c C-l comint-dynamic-list-input-ring  List input history
;;
;; Not bound by default in comint-mode (some are in shell mode)
;; comint-run				Run a program under comint-mode
;; send-invisible			Read a line w/o echo, and send to proc
;; comint-dynamic-complete-filename	Complete filename at point.
;; comint-dynamic-list-filename-completions List completions in help buffer.
;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;					replace with expanded/completed name.
;; comint-replace-by-expanded-history	Expand history at point;
;;					replace with expanded name.
;; comint-magic-space                  Expand history and add (a) space(s).
;; comint-kill-subjob			No mercy.
;; comint-show-maximum-output          Show as much output as possible.
;; comint-continue-subjob		Send CONT signal to buffer's process
;;					group.  Useful if you accidentally
;;					suspend your process (with C-c C-z).
;; comint-get-next-from-history        Fetch successive input history lines
;; comint-accumulate		       Combine lines to send them together
;;					as input.
;; comint-goto-process-mark	       Move point to where process-mark is.
;; comint-set-process-mark	       Set process-mark to point.

;; comint-mode-hook is the Comint mode hook.  Basically for your keybindings.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ring)
(require 'ansi-color)

;; Buffer Local Variables:
;;============================================================================
;; Comint mode buffer local variables:
;;  comint-prompt-regexp		string	comint-bol uses to match prompt
;;  comint-delimiter-argument-list	list	For delimiters and arguments
;;  comint-last-input-start		marker	Handy if inferior always echoes
;;  comint-last-input-end		marker	For comint-delete-output command
;;  comint-input-ring-size		integer	For the input history
;;  comint-input-ring			ring	mechanism
;;  comint-input-ring-index		number	...
;;  comint-save-input-ring-index	number	...
;;  comint-input-autoexpand		symbol	...
;;  comint-input-ignoredups		boolean	...
;;  comint-dynamic-complete-functions	hook   For the completion mechanism
;;  comint-completion-fignore		list	...
;;  comint-file-name-chars		string	...
;;  comint-file-name-quote-list		list	...
;;  comint-get-old-input		function Hooks for specific
;;  comint-input-filter-functions	hook	process-in-a-buffer
;;  comint-output-filter-functions	hook	function modes.
;;  comint-preoutput-filter-functions   hook
;;  comint-input-filter			function ...
;;  comint-input-sender			function ...
;;  comint-eol-on-send			boolean	...
;;  comint-process-echoes		boolean	...
;;  comint-scroll-to-bottom-on-input	symbol	For scroll behavior
;;  comint-move-point-for-output	symbol	...
;;  comint-scroll-show-maximum-output	boolean	...
;;  comint-accum-marker			maker	  For comint-accumulate
;;
;; Comint mode non-buffer local variables:
;;  comint-completion-addsuffix		boolean/cons	For file name
;;  comint-completion-autolist		boolean		completion behavior
;;  comint-completion-recexact		boolean		...

(defgroup comint nil
  "General command interpreter in a window stuff."
  :group 'processes)

(defgroup comint-completion nil
  "Completion facilities in comint."
  :group 'comint)

(defgroup comint-source nil
  "Source finding facilities in comint."
  :prefix "comint-"
  :group 'comint)

(defvar comint-prompt-regexp "^"
  "Regexp to recognize prompts in the inferior process.
Defaults to \"^\", the null string at BOL.

This variable is only used if the variable
`comint-use-prompt-regexp' is non-nil.

Good choices:
  Canonical Lisp: \"^[^> \\n]*>+:? *\" (Lucid, franz, kcl, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
  franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
  kcl: \"^>+ *\"
  shell: \"^[^#$%>\\n]*[#$%>] *\"
  T: \"^>+ *\"

This is a good thing to set in mode hooks.")

(defcustom comint-prompt-read-only nil
  "If non-nil, the comint prompt is read only.
The read only region includes the newline before the prompt.
This does not affect existing prompts.
Certain derived modes may override this option.

If you set this option to t, then the safe way to temporarily
override the read-only-ness of comint prompts is to call
`comint-kill-whole-line' or `comint-kill-region' with no
narrowing in effect.  This way you will be certain that none of
the remaining prompts will be accidentally messed up.  You may
wish to put something like the following in your `.emacs' file:

\(add-hook 'comint-mode-hook
	  (lambda ()
	    (define-key comint-mode-map \"\\C-w\" 'comint-kill-region)
	    (define-key comint-mode-map [C-S-backspace]
	      'comint-kill-whole-line)))

If you sometimes use comint-mode on text-only terminals or with `emacs -nw',
you might wish to use another binding for `comint-kill-whole-line'."
  :type 'boolean
  :group 'comint
  :version "22.1")

(defvar comint-delimiter-argument-list ()
  "List of characters to recognize as separate arguments in input.
Strings comprising a character in this list will separate the arguments
surrounding them, and also be regarded as arguments in their own right (unlike
whitespace).  See `comint-arguments'.
Defaults to the empty list.

For shells, a good value is (?\\| ?& ?< ?> ?\\( ?\\) ?;).

This is a good thing to set in mode hooks.")

(defcustom comint-input-autoexpand nil
  "If non-nil, expand input command history references on completion.
This mirrors the optional behavior of tcsh (its autoexpand and histlist).

If the value is `input', then the expansion is seen on input.
If the value is `history', then the expansion is only when inserting
into the buffer's input ring.  See also `comint-magic-space' and
`comint-dynamic-complete'.

This variable is buffer-local."
  :type '(choice (const :tag "off" nil)
		 (const input)
		 (const history)
		 (other :tag "on" t))
  :group 'comint)

(defface comint-highlight-input '((t (:weight bold)))
  "Face to use to highlight user input."
  :group 'comint)

(defface comint-highlight-prompt
  '((t :inherit minibuffer-prompt))
  "Face to use to highlight prompts."
  :group 'comint)

(defcustom comint-input-ignoredups nil
  "If non-nil, don't add input matching the last on the input ring.
This mirrors the optional behavior of bash.

This variable is buffer-local."
  :type 'boolean
  :group 'comint)

(defcustom comint-input-ring-file-name nil
  "If non-nil, name of the file to read/write input history.
See also `comint-read-input-ring' and `comint-write-input-ring'.
`comint-mode' makes this a buffer-local variable.  You probably want
to set this in a mode hook, rather than customize the default value."
  :type '(choice (const :tag "nil" nil)
		 file)
  :group 'comint)

(defcustom comint-scroll-to-bottom-on-input nil
  "Controls whether input to interpreter causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.

The default is nil.

See `comint-preinput-scroll-to-bottom'.  This variable is buffer-local."
  :type '(choice (const :tag "off" nil)
		 (const t)
		 (const all)
		 (const this))
  :group 'comint)

(defcustom comint-move-point-for-output nil
  "Controls whether interpreter output moves point to the end of the output.
If nil, then output never moves point to the output.
 (If the output occurs at point, it is inserted before point.)
If t or `all', move point in all windows showing the buffer.
If `this', move point only the selected window.
If `others', move point only in other windows, not in the selected window.

The default is nil.

See the variable `comint-scroll-show-maximum-output' and the function
`comint-postoutput-scroll-to-bottom'.
This variable is buffer-local in all Comint buffers."
  :type '(choice (const :tag "off" nil)
		 (const t)
		 (const all)
		 (const this)
		 (const others))
  :group 'comint)

(defvaralias 'comint-scroll-to-bottom-on-output 'comint-move-point-for-output)

(defcustom comint-scroll-show-maximum-output t
  "Controls how to scroll due to interpreter output.
This variable applies when point is at the end of the buffer
\(either because it was originally there, or because
`comint-move-point-for-output' said to move it there)
and output from the subprocess is inserted.

Non-nil means scroll so that the window is full of text
and point is on the last line.  A value of nil
means don't do anything special--scroll normally.

See also the variable `comint-move-point-for-output' and the function
`comint-postoutput-scroll-to-bottom'.
This variable is buffer-local in all Comint buffers."
  :type 'boolean
  :group 'comint)

(defcustom comint-buffer-maximum-size 1024
  "The maximum size in lines for Comint buffers.
Comint buffers are truncated from the top to be no greater than this number, if
the function `comint-truncate-buffer' is on `comint-output-filter-functions'."
  :type 'integer
  :group 'comint)

(defcustom comint-input-ring-size 500
  "Size of the input history ring in `comint-mode'."
  :type 'integer
  :group 'comint
  :version "23.2")

(defvar comint-input-ring-separator "\n"
  "Separator between commands in the history file.")

(defvar comint-input-history-ignore "^#"
  "Regexp for history entries that should be ignored when Comint initializes.")

(defcustom comint-process-echoes nil
  "If non-nil, assume that the subprocess echoes any input.
If so, delete one copy of the input so that only one copy eventually
appears in the buffer.

This variable is buffer-local."
  :type 'boolean
  :group 'comint)

;; AIX puts the name of the person being su'd to in front of the prompt.
;; kinit prints a prompt like `Password for devnull@GNU.ORG: '.
;; ksu prints a prompt like `Kerberos password for devnull/root@GNU.ORG: '.
;; ssh-add prints a prompt like `Enter passphrase: '.
;; plink prints a prompt like `Passphrase for key "root@GNU.ORG": '.
;; Ubuntu's sudo prompts like `[sudo] password for user:'
;; Some implementations of passwd use "Password (again)" as the 2nd prompt.
;; Something called "perforce" uses "Enter password:".
;; See M-x comint-testsuite--test-comint-password-prompt-regexp.
(defcustom comint-password-prompt-regexp
  (concat
   "\\(^ *\\|"
   (regexp-opt
    '("Enter" "enter" "Enter same" "enter same" "Enter the" "enter the"
      "Old" "old" "New" "new" "'s" "login"
      "Kerberos" "CVS" "UNIX" " SMB" "LDAP" "[sudo]" "Repeat" "Bad") t)
   " +\\)"
   (regexp-opt
    '("password" "Password" "passphrase" "Passphrase"
      "pass phrase" "Pass phrase" "Response"))
   "\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\
\\(?: for [^:]+\\)?:\\s *\\'")
  "Regexp matching prompts for passwords in the inferior process.
This is used by `comint-watch-for-password-prompt'."
  :version "24.1"
  :type 'regexp
  :group 'comint)

;; Here are the per-interpreter hooks.
(defvar comint-get-old-input (function comint-get-old-input-default)
  "Function that returns old text in Comint mode.
This function is called when return is typed while the point is in old
text.  It returns the text to be submitted as process input.  The
default is `comint-get-old-input-default', which either grabs the
current input field or grabs the current line and strips off leading
text matching `comint-prompt-regexp', depending on the value of
`comint-use-prompt-regexp'.")

(defvar comint-dynamic-complete-functions
  '(comint-c-a-p-replace-by-expanded-history comint-filename-completion)
  "List of functions called to perform completion.
Works like `completion-at-point-functions'.
See also `comint-dynamic-complete'.

This is a good thing to set in mode hooks.")

(defvar comint-input-filter
  (function (lambda (str) (not (string-match "\\`\\s *\\'" str))))
  "Predicate for filtering additions to input history.
Takes one argument, the input.  If non-nil, the input may be saved on the input
history list.  Default is to save anything that isn't all whitespace.")

(defvar comint-input-filter-functions '()
  "Abnormal hook run before input is sent to the process.
These functions get one argument, a string containing the text to send.")

;;;###autoload
(defvar comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom comint-watch-for-password-prompt)
  "Functions to call after output is inserted into the buffer.
One possible function is `comint-postoutput-scroll-to-bottom'.
These functions get one argument, a string containing the text as originally
inserted.  Note that this might not be the same as the buffer contents between
`comint-last-output-start' and the buffer's `process-mark', if other filter
functions have already modified the buffer.

See also `comint-preoutput-filter-functions'.

You can use `add-hook' to add functions to this list
either globally or locally.")

(defvar comint-input-sender-no-newline nil
  "Non-nil directs the `comint-input-sender' function not to send a newline.")

(defvar comint-input-sender (function comint-simple-send)
  "Function to actually send to PROCESS the STRING submitted by user.
Usually this is just `comint-simple-send', but if your mode needs to
massage the input string, put a different function here.
`comint-simple-send' just sends the string plus a newline.
\(If `comint-input-sender-no-newline' is non-nil, it omits the newline.)
This is called from the user command `comint-send-input'.")

(defcustom comint-eol-on-send t
  "Non-nil means go to the end of the line before sending input.
See `comint-send-input'."
  :type 'boolean
  :group 'comint)

(define-obsolete-variable-alias 'comint-use-prompt-regexp-instead-of-fields
  'comint-use-prompt-regexp "22.1")

;; Note: If it is decided to purge comint-prompt-regexp from the source
;; entirely, searching for uses of this variable will help to identify
;; places that need attention.
(defcustom comint-use-prompt-regexp nil
  "If non-nil, use `comint-prompt-regexp' to recognize prompts.
If nil, then program output and user-input are given different `field'
properties, which Emacs commands can use to distinguish them (in
particular, common movement commands such as `beginning-of-line'
respect field boundaries in a natural way)."
  :type 'boolean
  :group 'comint)

(defcustom comint-mode-hook nil
  "Hook run upon entry to `comint-mode'.
This is run before the process is cranked up."
  :type 'hook
  :group 'comint)

(defcustom comint-exec-hook '()
  "Hook run each time a process is exec'd by `comint-exec'.
This is called after the process is cranked up.  It is useful for things that
must be done each time a process is executed in a Comint mode buffer (e.g.,
`(process-kill-without-query)').  In contrast, the `comint-mode-hook' is only
executed once when the buffer is created."
  :type 'hook
  :group 'comint)

(defvar comint-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keys:
    (define-key map "\ep" 	  'comint-previous-input)
    (define-key map "\en" 	  'comint-next-input)
    (define-key map [C-up] 	  'comint-previous-input)
    (define-key map [C-down] 	  'comint-next-input)
    (define-key map "\er" 	  'comint-history-isearch-backward-regexp)
    (define-key map [?\C-c ?\M-r] 'comint-previous-matching-input-from-input)
    (define-key map [?\C-c ?\M-s] 'comint-next-matching-input-from-input)
    (define-key map "\e\C-l" 	  'comint-show-output)
    (define-key map "\C-m" 	  'comint-send-input)
    (define-key map "\C-d" 	  'comint-delchar-or-maybe-eof)
    ;; The following two are standardly aliased to C-d,
    ;; but they should never do EOF, just delete.
    (define-key map [delete] 	  'delete-char)
    (define-key map [kp-delete]	  'delete-char)
    (define-key map "\C-c " 	  'comint-accumulate)
    (define-key map "\C-c\C-x" 	  'comint-get-next-from-history)
    (define-key map "\C-c\C-a" 	  'comint-bol-or-process-mark)
    (define-key map "\C-c\C-u" 	  'comint-kill-input)
    (define-key map "\C-c\C-w" 	  'backward-kill-word)
    (define-key map "\C-c\C-c" 	  'comint-interrupt-subjob)
    (define-key map "\C-c\C-z" 	  'comint-stop-subjob)
    (define-key map "\C-c\C-\\"   'comint-quit-subjob)
    (define-key map "\C-c\C-m" 	  'comint-copy-old-input)
    (define-key map "\C-c\C-o" 	  'comint-delete-output)
    (define-key map "\C-c\C-r" 	  'comint-show-output)
    (define-key map "\C-c\C-e" 	  'comint-show-maximum-output)
    (define-key map "\C-c\C-l" 	  'comint-dynamic-list-input-ring)
    (define-key map "\C-c\C-n" 	  'comint-next-prompt)
    (define-key map "\C-c\C-p" 	  'comint-previous-prompt)
    (define-key map "\C-c\C-d" 	  'comint-send-eof)
    (define-key map "\C-c\C-s" 	  'comint-write-output)
    (define-key map "\C-c." 	  'comint-insert-previous-argument)
    ;; Mouse Buttons:
    (define-key map [mouse-2]     'comint-insert-input)
    ;; Menu bars:
    ;; completion:
    (define-key map [menu-bar completion]
      (cons "Complete" (make-sparse-keymap "Complete")))
    (define-key map [menu-bar completion complete-expand]
      '("Expand File Name" . comint-replace-by-expanded-filename))
    (define-key map [menu-bar completion complete-listing]
      '("File Completion Listing" . comint-dynamic-list-filename-completions))
    (define-key map [menu-bar completion complete-file]
      '("Complete File Name" . comint-dynamic-complete-filename))
    (define-key map [menu-bar completion complete]
      '("Complete at Point" . completion-at-point))
    ;; Input history:
    (define-key map [menu-bar inout]
      (cons "In/Out" (make-sparse-keymap "In/Out")))
    (define-key map [menu-bar inout delete-output]
      '("Delete Current Output Group" . comint-delete-output))
    (define-key map [menu-bar inout append-output-to-file]
      '("Append Current Output Group to File" . comint-append-output-to-file))
    (define-key map [menu-bar inout write-output]
      '("Write Current Output Group to File" . comint-write-output))
    (define-key map [menu-bar inout next-prompt]
      '("Forward Output Group" . comint-next-prompt))
    (define-key map [menu-bar inout previous-prompt]
      '("Backward Output Group" . comint-previous-prompt))
    (define-key map [menu-bar inout show-maximum-output]
      '("Show Maximum Output" . comint-show-maximum-output))
    (define-key map [menu-bar inout show-output]
      '("Show Current Output Group" . comint-show-output))
    (define-key map [menu-bar inout kill-input]
      '("Kill Current Input" . comint-kill-input))
    (define-key map [menu-bar inout copy-input]
      '("Copy Old Input" . comint-copy-old-input))
    (define-key map [menu-bar inout history-isearch-backward-regexp]
      '("Isearch Input Regexp Backward..." . comint-history-isearch-backward-regexp))
    (define-key map [menu-bar inout history-isearch-backward]
      '("Isearch Input String Backward..." . comint-history-isearch-backward))
    (define-key map [menu-bar inout forward-matching-history]
      '("Forward Matching Input..." . comint-forward-matching-input))
    (define-key map [menu-bar inout backward-matching-history]
      '("Backward Matching Input..." . comint-backward-matching-input))
    (define-key map [menu-bar inout next-matching-history]
      '("Next Matching Input..." . comint-next-matching-input))
    (define-key map [menu-bar inout previous-matching-history]
      '("Previous Matching Input..." . comint-previous-matching-input))
    (define-key map [menu-bar inout next-matching-history-from-input]
      '("Next Matching Current Input" . comint-next-matching-input-from-input))
    (define-key map [menu-bar inout previous-matching-history-from-input]
      '("Previous Matching Current Input" . comint-previous-matching-input-from-input))
    (define-key map [menu-bar inout next-history]
      '("Next Input" . comint-next-input))
    (define-key map [menu-bar inout previous-history]
      '("Previous Input" . comint-previous-input))
    (define-key map [menu-bar inout list-history]
      '("List Input History" . comint-dynamic-list-input-ring))
    (define-key map [menu-bar inout expand-history]
      '("Expand History Before Point" . comint-replace-by-expanded-history))
    ;; Signals
    (let ((signals-map (make-sparse-keymap "Signals")))
      (define-key map [menu-bar signals] (cons "Signals" signals-map))
      (define-key signals-map [eof]   '("EOF"   . comint-send-eof))
      (define-key signals-map [kill]  '("KILL"  . comint-kill-subjob))
      (define-key signals-map [quit]  '("QUIT"  . comint-quit-subjob))
      (define-key signals-map [cont]  '("CONT"  . comint-continue-subjob))
      (define-key signals-map [stop]  '("STOP"  . comint-stop-subjob))
      (define-key signals-map [break] '("BREAK" . comint-interrupt-subjob)))
    ;; Put them in the menu bar:
    (setq menu-bar-final-items (append '(completion inout signals)
				       menu-bar-final-items))
    map))

;; Fixme: Is this still relevant?
(defvar comint-ptyp t
  "Non-nil if communications via pty; false if by pipe.  Buffer local.
This is to work around a bug in Emacs process signaling.")

(defvar comint-input-ring nil)
(defvar comint-last-input-start nil)
(defvar comint-last-input-end nil)
(defvar comint-last-output-start nil)
(defvar comint-input-ring-index nil
  "Index of last matched history element.")
(defvar comint-matching-input-from-input-string ""
  "Input previously used to match input history.")
(defvar comint-save-input-ring-index nil
  "Last input ring index which you copied.
This is to support the command \\[comint-get-next-from-history].")

(defvar comint-accum-marker nil
  "Non-nil if you are accumulating input lines to send as input together.
The command \\[comint-accumulate] sets this.")

(defvar comint-stored-incomplete-input nil
  "Stored input for history cycling.")

(put 'comint-replace-by-expanded-history 'menu-enable 'comint-input-autoexpand)
(put 'comint-input-ring 'permanent-local t)
(put 'comint-input-ring-index 'permanent-local t)
(put 'comint-save-input-ring-index 'permanent-local t)
(put 'comint-input-autoexpand 'permanent-local t)
(put 'comint-input-filter-functions 'permanent-local t)
(put 'comint-output-filter-functions 'permanent-local t)
(put 'comint-preoutput-filter-functions 'permanent-local t)
(put 'comint-scroll-to-bottom-on-input 'permanent-local t)
(put 'comint-move-point-for-output 'permanent-local t)
(put 'comint-scroll-show-maximum-output 'permanent-local t)
(put 'comint-ptyp 'permanent-local t)

(put 'comint-mode 'mode-class 'special)

(define-derived-mode comint-mode fundamental-mode "Comint"
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
Setting variable `comint-eol-on-send' means jump to the end of the line
before submitting new input.

This mode is customized to create major modes such as Inferior Lisp
mode, Shell mode, etc.  This can be done by setting the hooks
`comint-input-filter-functions', `comint-input-filter', `comint-input-sender'
and `comint-get-old-input' to appropriate functions, and the variable
`comint-prompt-regexp' to the appropriate regular expression.

The mode maintains an input history of size `comint-input-ring-size'.
You can access this with the commands \\[comint-next-input],
\\[comint-previous-input], and \\[comint-dynamic-list-input-ring].
Input ring history expansion can be achieved with the commands
\\[comint-replace-by-expanded-history] or \\[comint-magic-space].
Input ring expansion is controlled by the variable `comint-input-autoexpand',
and addition is controlled by the variable `comint-input-ignoredups'.

Commands with no default key bindings include `send-invisible',
`comint-dynamic-complete', `comint-dynamic-list-filename-completions', and
`comint-magic-space'.

Input to, and output from, the subprocess can cause the window to scroll to
the end of the buffer.  See variables `comint-output-filter-functions',
`comint-preoutput-filter-functions', `comint-scroll-to-bottom-on-input',
and `comint-move-point-for-output'.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{comint-mode-map}

Entry to this mode runs the hooks on `comint-mode-hook'."
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'window-point-insertion-type) t)
  (set (make-local-variable 'comint-last-input-start) (point-min-marker))
  (set (make-local-variable 'comint-last-input-end) (point-min-marker))
  (set (make-local-variable 'comint-last-output-start) (make-marker))
  (make-local-variable 'comint-last-prompt-overlay)
  (make-local-variable 'comint-prompt-regexp)        ; Don't set; default
  (make-local-variable 'comint-input-ring-size)      ; ...to global val.
  (make-local-variable 'comint-input-ring)
  (make-local-variable 'comint-input-ring-file-name)
  (or (and (boundp 'comint-input-ring) comint-input-ring)
      (setq comint-input-ring (make-ring comint-input-ring-size)))
  (make-local-variable 'comint-input-ring-index)
  (make-local-variable 'comint-save-input-ring-index)
  (or (and (boundp 'comint-input-ring-index) comint-input-ring-index)
      (setq comint-input-ring-index nil))
  (or (and (boundp 'comint-save-input-ring-index) comint-save-input-ring-index)
      (setq comint-save-input-ring-index nil))
  (make-local-variable 'comint-matching-input-from-input-string)
  (make-local-variable 'comint-input-autoexpand)
  (make-local-variable 'comint-input-ignoredups)
  (make-local-variable 'comint-delimiter-argument-list)
  (make-local-variable 'comint-completion-fignore)
  (make-local-variable 'comint-get-old-input)
  (make-local-variable 'comint-input-filter)
  (make-local-variable 'comint-input-sender)
  (make-local-variable 'comint-eol-on-send)
  (make-local-variable 'comint-scroll-to-bottom-on-input)
  (make-local-variable 'comint-move-point-for-output)
  (make-local-variable 'comint-scroll-show-maximum-output)
  (make-local-variable 'comint-stored-incomplete-input)
  ;; Following disabled because it seems to break the case when
  ;; comint-scroll-show-maximum-output is nil, and no-one can remember
  ;; what the original problem was.  If there are problems with point
  ;; not going to the end, consider re-enabling this.
  ;; http://lists.gnu.org/archive/html/emacs-devel/2007-08/msg00827.html
  ;;
  ;; This makes it really work to keep point at the bottom.
  ;; (make-local-variable 'scroll-conservatively)
  ;; (setq scroll-conservatively 10000)
  (add-hook 'pre-command-hook 'comint-preinput-scroll-to-bottom t t)
  (make-local-variable 'comint-ptyp)
  (make-local-variable 'comint-process-echoes)
  (make-local-variable 'comint-file-name-chars)
  (make-local-variable 'comint-file-name-quote-list)
  ;; dir tracking on remote files
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) ""))
  (make-local-variable 'comint-accum-marker)
  (setq comint-accum-marker (make-marker))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(nil t))
  (add-hook 'change-major-mode-hook 'font-lock-defontify nil t)
  (add-hook 'isearch-mode-hook 'comint-history-isearch-setup nil t)
  (add-hook 'completion-at-point-functions 'comint-completion-at-point nil t)
  ;; This behavior is not useful in comint buffers, and is annoying
  (set (make-local-variable 'next-line-add-newlines) nil))

(defun comint-check-proc (buffer)
  "Return non-nil if there is a living process associated w/buffer BUFFER.
Living means the status is `open', `run', or `stop'.
BUFFER can be either a buffer or the name of one."
  (let ((proc (get-buffer-process buffer)))
    (and proc (memq (process-status proc) '(open run stop)))))

;;;###autoload
(defun make-comint-in-buffer (name buffer program &optional startfile &rest switches)
  "Make a Comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
If there is a running process in BUFFER, it is not restarted.

PROGRAM should be one of the following:
- a string, denoting an executable program to create via
  `start-file-process'
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via `open-network-stream'
- nil, denoting a newly-allocated pty.

Optional fourth arg STARTFILE is the name of a file, whose
contents are sent to the process as its initial input.

If PROGRAM is a string, any more args are arguments to PROGRAM.

Return the (possibly newly created) process buffer."
  (or (fboundp 'start-file-process)
      (error "Multi-processing is not supported for this system"))
  (setq buffer (get-buffer-create (or buffer (concat "*" name "*"))))
  ;; If no process, or nuked process, crank up a new one and put buffer in
  ;; comint mode.  Otherwise, leave buffer and existing process alone.
  (unless (comint-check-proc buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
	(comint-mode))) ; Install local vars, mode, keymap, ...
    (comint-exec buffer name program startfile switches))
  buffer)

;;;###autoload
(defun make-comint (name program &optional startfile &rest switches)
  "Make a Comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-file-process', or a cons pair of the form (HOST . SERVICE) denoting
a TCP connection to be opened via `open-network-stream'.  If there is already
a running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file, whose contents are sent to the
process as its initial input.

If PROGRAM is a string, any more args are arguments to PROGRAM.

Returns the (possibly newly created) process buffer."
  (apply #'make-comint-in-buffer name nil program startfile switches))

;;;###autoload
(defun comint-run (program)
  "Run PROGRAM in a Comint buffer and switch to it.
The buffer name is made by surrounding the file name of PROGRAM with `*'s.
The file name is used to make a symbol name, such as `comint-sh-hook', and any
hooks on this symbol are run in the buffer.
See `make-comint' and `comint-exec'."
  (interactive "sRun program: ")
  (let ((name (file-name-nondirectory program)))
    (switch-to-buffer (make-comint name program))
    (run-hooks (intern-soft (concat "comint-" name "-hook")))))

(defun comint-exec (buffer name command startfile switches)
  "Start up a process named NAME in buffer BUFFER for Comint modes.
Runs the given COMMAND with SWITCHES, and initial input from STARTFILE.

COMMAND should be one of the following:
- a string, denoting an executable program to create via
  `start-file-process'
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via `open-network-stream'
- nil, denoting a newly-allocated pty.

This function blasts any old process running in the buffer, and
does not set the buffer mode.  You can use this to cheaply run a
series of processes in the same Comint buffer.  The hook
`comint-exec-hook' is run after each exec."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc
	   (if (consp command)
	       (open-network-stream name buffer (car command) (cdr command))
	     (comint-exec-1 name buffer command switches))))
      (set-process-filter proc 'comint-output-filter)
      (make-local-variable 'comint-ptyp)
      (setq comint-ptyp process-connection-type) ; t if pty, nil if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      ;; Feed it the startfile.
      (cond (startfile
	     ;;This is guaranteed to wait long enough
	     ;;but has bad results if the comint does not prompt at all
	     ;;	     (while (= size (buffer-size))
	     ;;	       (sleep-for 1))
	     ;;I hope 1 second is enough!
	     (sleep-for 1)
	     (goto-char (point-max))
	     (insert-file-contents startfile)
	     (setq startfile (buffer-substring (point) (point-max)))
	     (delete-region (point) (point-max))
	     (comint-send-string proc startfile)))
      (run-hooks 'comint-exec-hook)
      buffer)))

;; This auxiliary function cranks up the process for comint-exec in
;; the appropriate environment.

(defun comint-exec-1 (name buffer command switches)
  (let ((process-environment
	 (nconc
	  ;; If using termcap, we specify `emacs' as the terminal type
	  ;; because that lets us specify a width.
	  ;; If using terminfo, we specify `dumb' because that is
	  ;; a defined terminal type.  `emacs' is not a defined terminal type
	  ;; and there is no way for us to define it here.
	  ;; Some programs that use terminfo get very confused
	  ;; if TERM is not a valid terminal type.
	  ;; ;; There is similar code in compile.el.
	  (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
	      (list "TERM=dumb" "TERMCAP="
		    (format "COLUMNS=%d" (window-width)))
	    (list "TERM=emacs"
		  (format "TERMCAP=emacs:co#%d:tc=unknown:" (window-width))))
	  (unless (getenv "EMACS")
	    (list "EMACS=t"))
	  (list (format "INSIDE_EMACS=%s,comint" emacs-version))
	  process-environment))
	(default-directory
	  (if (file-accessible-directory-p default-directory)
	      default-directory
	    "/"))
	proc decoding encoding changed)
    (let ((exec-path (if (and command (file-name-directory command))
			 ;; If the command has slashes, make sure we
			 ;; first look relative to the current directory.
			 (cons default-directory exec-path) exec-path)))
      (setq proc (apply 'start-file-process name buffer command switches)))
    ;; Some file name handler cannot start a process, fe ange-ftp.
    (unless (processp proc) (error "No process started"))
    (let ((coding-systems (process-coding-system proc)))
      (setq decoding (car coding-systems)
	    encoding (cdr coding-systems)))
    ;; Even if start-file-process left the coding system for encoding data
    ;; sent from the process undecided, we had better use the same one
    ;; as what we use for decoding.  But, we should suppress EOL
    ;; conversion.
    (if (and decoding (not encoding))
	(setq encoding (coding-system-change-eol-conversion decoding 'unix)
	      changed t))
    (if changed
	(set-process-coding-system proc decoding encoding))
    proc))

(defun comint-insert-input (event)
  "In a Comint buffer, set the current input to the previous input at point.
If there is no previous input at point, run the command specified
by the global keymap (usually `mouse-yank-at-click')."
  (interactive "e")
  ;; Don't set the mouse here, since it may otherwise change the behavior
  ;; of the command on which we fallback if there's no field at point.
  ;; (mouse-set-point event)
  (let ((pos (posn-point (event-end event)))
	field input)
    (with-selected-window (posn-window (event-end event))
      ;; If pos is at the very end of a field, the mouse-click was
      ;; probably outside (to the right) of the field.
      (and (< pos (field-end pos))
	   (< (field-end pos) (point-max))
           (progn (setq field (field-at-pos pos))
		  (setq input (field-string-no-properties pos)))))
    (if (or (null input) (null comint-accum-marker) field)
	;; Fall back to the global definition if (i) the selected
	;; buffer is not a comint buffer (which can happen if a
	;; non-comint window was selected and we clicked in a comint
	;; window), or (ii) there is no input at POS.
	(let* ((keys (this-command-keys))
	       (last-key (and (vectorp keys) (aref keys (1- (length keys)))))
	       (fun (and last-key (lookup-key global-map (vector last-key)))))
	  (and fun (not (eq fun 'comint-insert-input))
	       (call-interactively fun)))
      (with-selected-window (posn-window (event-end event))
        ;; Otherwise, insert the previous input.
        (goto-char (point-max))
        ;; First delete any old unsent input at the end
        (delete-region
         (or (marker-position comint-accum-marker)
             (process-mark (get-buffer-process (current-buffer))))
         (point))
        ;; Insert the input at point
        (insert input)))))

;; Input history processing in a buffer
;; ===========================================================================
;; Useful input history functions, courtesy of the Ergo group.

;; Eleven commands:
;; comint-dynamic-list-input-ring	List history in help buffer.
;; comint-previous-input		Previous input...
;; comint-previous-matching-input	...matching a string.
;; comint-previous-matching-input-from-input ... matching the current input.
;; comint-next-input			Next input...
;; comint-next-matching-input		...matching a string.
;; comint-next-matching-input-from-input     ... matching the current input.
;; comint-backward-matching-input      Backwards input...
;; comint-forward-matching-input       ...matching a string.
;; comint-replace-by-expanded-history	Expand history at point;
;;					replace with expanded history.
;; comint-magic-space			Expand history and insert space.
;;
;; Three functions:
;; comint-read-input-ring              Read into comint-input-ring...
;; comint-write-input-ring             Write to comint-input-ring-file-name.
;; comint-replace-by-expanded-history-before-point Workhorse function.

(defun comint-read-input-ring (&optional silent)
  "Set the buffer's `comint-input-ring' from a history file.
The name of the file is given by the variable `comint-input-ring-file-name'.
The history ring is of size `comint-input-ring-size', regardless of file size.
If `comint-input-ring-file-name' is nil this function does nothing.

If the optional argument SILENT is non-nil, we say nothing about a
failure to read the history file.

This function is useful for major mode commands and mode hooks.

The commands stored in the history file are separated by the
`comint-input-ring-separator', and entries that match
`comint-input-history-ignore' are ignored.  The most recent command
comes last.

See also `comint-input-ignoredups' and `comint-write-input-ring'."
  (cond ((or (null comint-input-ring-file-name)
	     (equal comint-input-ring-file-name ""))
	 nil)
	((not (file-readable-p comint-input-ring-file-name))
	 (or silent
	     (message "Cannot read history file %s"
		      comint-input-ring-file-name)))
	(t
	 (let* ((file comint-input-ring-file-name)
		(count 0)
		;; Some users set HISTSIZE or `comint-input-ring-size'
		;; to huge numbers.  Don't allocate a huge ring right
		;; away; there might not be that much history.
		(ring-size (min 1500 comint-input-ring-size))
		(ring (make-ring ring-size)))
	   (with-temp-buffer
             (insert-file-contents file)
             ;; Save restriction in case file is already visited...
             ;; Watch for those date stamps in history files!
             (goto-char (point-max))
             (let (start end history)
               (while (and (< count comint-input-ring-size)
                           (re-search-backward comint-input-ring-separator
                                               nil t)
                           (setq end (match-beginning 0)))
                 (setq start
                       (if (re-search-backward comint-input-ring-separator
                                               nil t)
                           (match-end 0)
                         (point-min)))
                 (setq history (buffer-substring start end))
                 (goto-char start)
                 (when (and (not (string-match comint-input-history-ignore
					       history))
			    (or (null comint-input-ignoredups)
				(ring-empty-p ring)
				(not (string-equal (ring-ref ring 0)
						   history))))
		   (when (= count ring-size)
		     (ring-extend ring (min (- comint-input-ring-size ring-size)
					    ring-size))
		     (setq ring-size (ring-size ring)))
		   (ring-insert-at-beginning ring history)
		   (setq count (1+ count))))))
	   (setq comint-input-ring ring
		 comint-input-ring-index nil)))))

(defun comint-write-input-ring ()
  "Writes the buffer's `comint-input-ring' to a history file.
The name of the file is given by the variable `comint-input-ring-file-name'.
The original contents of the file are lost if `comint-input-ring' is not empty.
If `comint-input-ring-file-name' is nil this function does nothing.

Useful within process sentinels.

See also `comint-read-input-ring'."
  (cond ((or (null comint-input-ring-file-name)
	     (equal comint-input-ring-file-name "")
	     (null comint-input-ring) (ring-empty-p comint-input-ring))
	 nil)
	((not (file-writable-p comint-input-ring-file-name))
	 (message "Cannot write history file %s" comint-input-ring-file-name))
	(t
	 (let* ((history-buf (get-buffer-create " *Temp Input History*"))
		(ring comint-input-ring)
		(file comint-input-ring-file-name)
		(index (ring-length ring)))
	   ;; Write it all out into a buffer first.  Much faster, but messier,
	   ;; than writing it one line at a time.
	   (with-current-buffer history-buf
	     (erase-buffer)
	     (while (> index 0)
	       (setq index (1- index))
	       (insert (ring-ref ring index) comint-input-ring-separator))
	     (write-region (buffer-string) nil file nil 'no-message)
	     (kill-buffer nil))))))


(defvar comint-dynamic-list-input-ring-window-conf)

(defun comint-dynamic-list-input-ring-select ()
  "Choose the input history entry that point is in or next to."
  (interactive)
  (let ((buffer completion-reference-buffer)
        beg end completion)
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No history entry here"))
    (setq beg (previous-single-property-change beg 'mouse-face))
    (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
    (setq completion (buffer-substring beg end))
    (set-window-configuration comint-dynamic-list-input-ring-window-conf)
    (choose-completion-string completion buffer)))

(defun comint-dynamic-list-input-ring ()
  "Display a list of recent inputs entered into the current buffer."
  (interactive)
  (if (or (not (ring-p comint-input-ring))
	  (ring-empty-p comint-input-ring))
      (message "No history")
    (let ((history nil)
	  (history-buffer " *Input History*")
	  (conf (current-window-configuration)))
      ;; We have to build up a list ourselves from the ring vector.
      (dotimes (index (ring-length comint-input-ring))
	(push (ring-ref comint-input-ring index) history))
      ;; Show them most-recent-first.
      (setq history (nreverse history))
      ;; Change "completion" to "history reference"
      ;; to make the display accurate.
      (with-output-to-temp-buffer history-buffer
	(display-completion-list history)
	(set-buffer history-buffer)
	(let ((keymap (make-sparse-keymap)))
	  (set-keymap-parent keymap (current-local-map))
	  (define-key keymap "\C-m" 'comint-dynamic-list-input-ring-select)
	  (use-local-map keymap))
	(forward-line 3)
	(while (search-backward "completion" nil 'move)
	  (replace-match "history reference")))
      (sit-for 0)
      (message "Hit space to flush")
      (setq comint-dynamic-list-input-ring-window-conf conf)
      (let ((ch (read-event)))
	(if (eq ch ?\s)
	    (set-window-configuration conf)
	  (setq unread-command-events (list ch)))))))


(defun comint-regexp-arg (prompt)
  "Return list of regexp and prefix arg using PROMPT."
  (let* (;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer prompt nil nil nil
				       'minibuffer-history-search-history)))
    ;; If the user didn't enter anything, nothing is added to m-h-s-h.
    ;; Use the previous search regexp, if there is one.
    (list (if (string-equal regexp "")
              (or (car minibuffer-history-search-history)
                  regexp)
	    regexp)
	  (prefix-numeric-value current-prefix-arg))))

(defun comint-search-arg (arg)
  ;; First make sure there is a ring and that we are after the process mark
  (cond ((not (comint-after-pmark-p))
	 (error "Not at command line"))
	((or (null comint-input-ring)
	     (ring-empty-p comint-input-ring))
	 (error "Empty input ring"))
	((zerop arg)
	 ;; arg of zero resets search from beginning, and uses arg of 1
	 (setq comint-input-ring-index nil)
	 1)
	(t
	 arg)))

(defun comint-restore-input ()
  "Restore unfinished input."
  (interactive)
  (when comint-input-ring-index
    (comint-delete-input)
    (when (> (length comint-stored-incomplete-input) 0)
      (insert comint-stored-incomplete-input)
      (message "Input restored"))
    (setq comint-input-ring-index nil)))

(defun comint-search-start (arg)
  "Index to start a directional search, starting at `comint-input-ring-index'."
  (if comint-input-ring-index
      ;; If a search is running, offset by 1 in direction of arg
      (mod (+ comint-input-ring-index (if (> arg 0) 1 -1))
	   (ring-length comint-input-ring))
    ;; For a new search, start from beginning or end, as appropriate
    (if (>= arg 0)
	0				       ; First elt for forward search
      (1- (ring-length comint-input-ring)))))  ; Last elt for backward search

(defun comint-previous-input-string (arg)
  "Return the string ARG places along the input ring.
Moves relative to `comint-input-ring-index'."
  (ring-ref comint-input-ring (if comint-input-ring-index
				  (mod (+ arg comint-input-ring-index)
				       (ring-length comint-input-ring))
				arg)))

(defun comint-previous-input (arg)
  "Cycle backwards through input history, saving input."
  (interactive "*p")
  (if (and comint-input-ring-index
	   (or		       ;; leaving the "end" of the ring
	    (and (< arg 0)		; going down
		 (eq comint-input-ring-index 0))
	    (and (> arg 0)		; going up
		 (eq comint-input-ring-index
		     (1- (ring-length comint-input-ring)))))
	   comint-stored-incomplete-input)
      (comint-restore-input)
    (comint-previous-matching-input "." arg)))

(defun comint-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (comint-previous-input (- arg)))

(defun comint-previous-matching-input-string (regexp arg)
  "Return the string matching REGEXP ARG places along the input ring.
Moves relative to `comint-input-ring-index'."
  (let* ((pos (comint-previous-matching-input-string-position regexp arg)))
    (if pos (ring-ref comint-input-ring pos))))

(defun comint-previous-matching-input-string-position (regexp arg &optional start)
  "Return the index matching REGEXP ARG places along the input ring.
Moves relative to START, or `comint-input-ring-index'."
  (if (or (not (ring-p comint-input-ring))
	  (ring-empty-p comint-input-ring))
      (error "No history"))
  (let* ((len (ring-length comint-input-ring))
	 (motion (if (> arg 0) 1 -1))
	 (n (mod (- (or start (comint-search-start arg)) motion) len))
	 (tried-each-ring-item nil)
	 (prev nil))
    ;; Do the whole search as many times as the argument says.
    (while (and (/= arg 0) (not tried-each-ring-item))
      ;; Step once.
      (setq prev n
	    n (mod (+ n motion) len))
      ;; If we haven't reached a match, step some more.
      (while (and (< n len) (not tried-each-ring-item)
		  (not (string-match regexp (ring-ref comint-input-ring n))))
	(setq n (mod (+ n motion) len)
	      ;; If we have gone all the way around in this search.
	      tried-each-ring-item (= n prev)))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    ;; Now that we know which ring element to use, if we found it, return that.
    (if (string-match regexp (ring-ref comint-input-ring n))
	n)))

(defun comint-delete-input ()
  "Delete all input between accumulation or process mark and point."
  (delete-region
   ;; Can't use kill-region as it sets this-command
   (or  (marker-position comint-accum-marker)
	(process-mark (get-buffer-process (current-buffer))))
   (point-max)))

(defun comint-previous-matching-input (regexp n)
  "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (comint-regexp-arg "Previous input matching (regexp): "))
  (setq n (comint-search-arg n))
  (let ((pos (comint-previous-matching-input-string-position regexp n)))
    ;; Has a match been found?
    (if (null pos)
	(error "Not found")
      ;; If leaving the edit line, save partial input
      (if (null comint-input-ring-index)	;not yet on ring
	  (setq comint-stored-incomplete-input
		(funcall comint-get-old-input)))
      (setq comint-input-ring-index pos)
      (message "History item: %d" (1+ pos))
      (comint-delete-input)
      (insert (ring-ref comint-input-ring pos)))))

(defun comint-next-matching-input (regexp n)
  "Search forwards through input history for match for REGEXP.
\(Later history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (comint-regexp-arg "Next input matching (regexp): "))
  (comint-previous-matching-input regexp (- n)))

(defun comint-previous-matching-input-from-input (n)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (if (not (memq last-command '(comint-previous-matching-input-from-input
				comint-next-matching-input-from-input)))
      ;; Starting a new search
      (setq comint-matching-input-from-input-string
	    (buffer-substring
	     (or (marker-position comint-accum-marker)
		 (process-mark (get-buffer-process (current-buffer))))
	     (point))
	    comint-input-ring-index nil))
  (comint-previous-matching-input
   (concat "^" (regexp-quote comint-matching-input-from-input-string))
   n))

(defun comint-next-matching-input-from-input (n)
  "Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match."
  (interactive "p")
  (comint-previous-matching-input-from-input (- n)))


(defun comint-replace-by-expanded-history (&optional silent start)
  "Expand input command history references before point.
Expansion is dependent on the value of `comint-input-autoexpand'.

This function depends on the buffer's idea of the input history, which may not
match the command interpreter's idea, assuming it has one.

Assumes history syntax is like typical Un*x shells'.  However, since Emacs
cannot know the interpreter's idea of input line numbers, assuming it has one,
it cannot expand absolute input line number references.

If the optional argument SILENT is non-nil, never complain
even if history reference seems erroneous.

If the optional argument START is non-nil, that specifies the
start of the text to scan for history references, rather
than the logical beginning of line.

See `comint-magic-space' and `comint-replace-by-expanded-history-before-point'.

Returns t if successful."
  (interactive)
  (let ((f (comint-c-a-p-replace-by-expanded-history silent start)))
    (if f (funcall f))))

(defun comint-c-a-p-replace-by-expanded-history (&optional silent start)
  "Expand input command history at point.
For use on `completion-at-point-functions'."
  (if (and comint-input-autoexpand
	   (if comint-use-prompt-regexp
	       ;; Use comint-prompt-regexp
	       (save-excursion
		 (beginning-of-line)
		 (looking-at (concat comint-prompt-regexp "!\\|\\^")))
	     ;; Use input fields.  User input that hasn't been entered
	     ;; yet, at the end of the buffer, has a nil `field' property.
	     (and (null (get-char-property (point) 'field))
		  (string-match "!\\|^\\^" (field-string))))
           (catch 'dry-run
             (comint-replace-by-expanded-history-before-point
              silent start 'dry-run)))
      (lambda ()
        ;; Looks like there might be history references in the command.
        (let ((previous-modified-tick (buffer-modified-tick)))
          (comint-replace-by-expanded-history-before-point silent start)
          (/= previous-modified-tick (buffer-modified-tick))))))


(defun comint-replace-by-expanded-history-before-point
  (silent &optional start dry-run)
  "Expand directory stack reference before point.
See `comint-replace-by-expanded-history'.  Returns t if successful.

If the optional argument START is non-nil, that specifies the
start of the text to scan for history references, rather
than the logical beginning of line.

If DRY-RUN is non-nil, throw to DRY-RUN before performing any
actual side-effect."
  (save-excursion
    (let ((toend (- (line-end-position) (point)))
	  (start (or start (comint-line-beginning-position))))
      (goto-char start)
      (while (progn
	       (skip-chars-forward "^!^" (- (line-end-position) toend))
	       (< (point) (- (line-end-position) toend)))
	;; This seems a bit complex.  We look for references such as !!, !-num,
	;; !foo, !?foo, !{bar}, !?{bar}, ^oh, ^my^, ^god^it, ^never^ends^.
	;; If that wasn't enough, the plings can be suffixed with argument
	;; range specifiers.
	;; Argument ranges are complex too, so we hive off the input line,
	;; referenced with plings, with the range string to `comint-args'.
	(setq comint-input-ring-index nil)
	(cond ((or (= (preceding-char) ?\\)
		   (comint-within-quotes start (point)))
	       ;; The history is quoted, or we're in quotes.
	       (goto-char (1+ (point))))
	      ((looking-at "![0-9]+\\($\\|[^-]\\)")
	       ;; We cannot know the interpreter's idea of input line numbers.
               (if dry-run (throw dry-run 'message))
	       (goto-char (match-end 0))
	       (message "Absolute reference cannot be expanded"))
	      ((looking-at "!-\\([0-9]+\\)\\(:?[0-9^$*-]+\\)?")
	       ;; Just a number of args from `number' lines backward.
               (if dry-run (throw dry-run 'history))
	       (let ((number (1- (string-to-number
				  (buffer-substring (match-beginning 1)
						    (match-end 1))))))
		 (if (<= number (ring-length comint-input-ring))
		     (progn
		       (replace-match
			(comint-args (comint-previous-input-string number)
				     (match-beginning 2) (match-end 2))
			t t)
		       (setq comint-input-ring-index number)
		       (message "History item: %d" (1+ number)))
		   (goto-char (match-end 0))
		   (message "Relative reference exceeds input history size"))))
	      ((or (looking-at "!!?:?\\([0-9^$*-]+\\)") (looking-at "!!"))
	       ;; Just a number of args from the previous input line.
               (if dry-run (throw dry-run 'expand))
	       (replace-match (comint-args (comint-previous-input-string 0)
					   (match-beginning 1) (match-end 1))
			      t t)
	       (message "History item: previous"))
	      ((looking-at
		"!\\??\\({\\(.+\\)}\\|\\(\\sw+\\)\\)\\(:?[0-9^$*-]+\\)?")
	       ;; Most recent input starting with or containing (possibly
	       ;; protected) string, maybe just a number of args.  Phew.
               (if dry-run (throw dry-run 'expand))
	       (let* ((mb1 (match-beginning 1)) (me1 (match-end 1))
		      (mb2 (match-beginning 2)) (me2 (match-end 2))
		      (exp (buffer-substring (or mb2 mb1) (or me2 me1)))
		      (pref (if (save-match-data (looking-at "!\\?")) "" "^"))
		      (pos (save-match-data
			     (comint-previous-matching-input-string-position
			      (concat pref (regexp-quote exp)) 1))))
		 (if (null pos)
		     (progn
		       (goto-char (match-end 0))
		       (or silent
			   (progn (message "Not found")
				  (ding))))
		   (setq comint-input-ring-index pos)
		   (replace-match
		    (comint-args (ring-ref comint-input-ring pos)
				 (match-beginning 4) (match-end 4))
		    t t)
		   (message "History item: %d" (1+ pos)))))
	      ((looking-at "\\^\\([^^]+\\)\\^?\\([^^]*\\)\\^?")
	       ;; Quick substitution on the previous input line.
               (if dry-run (throw dry-run 'expand))
	       (let ((old (buffer-substring (match-beginning 1) (match-end 1)))
		     (new (buffer-substring (match-beginning 2) (match-end 2)))
		     (pos nil))
		 (replace-match (comint-previous-input-string 0) t t)
		 (setq pos (point))
		 (goto-char (match-beginning 0))
		 (if (not (search-forward old pos t))
		     (or silent
			 (error "Not found"))
		   (replace-match new t t)
		   (message "History item: substituted"))))
	      (t
	       (forward-char 1)))))
    nil))


(defun comint-magic-space (arg)
  "Expand input history references before point and insert ARG spaces.
A useful command to bind to SPC.  See `comint-replace-by-expanded-history'."
  (interactive "p")
  (comint-replace-by-expanded-history)
  (self-insert-command arg))

;; Isearch in comint input history

(defcustom comint-history-isearch nil
  "Non-nil to Isearch in input history only, not in comint buffer output.
If t, usual Isearch keys like `C-r' and `C-M-r' in comint mode search
in the input history.
If `dwim', Isearch keys search in the input history only when initial
point position is at the comint command line.  When starting Isearch
from other parts of the comint buffer, they search in the comint buffer.
If nil, Isearch operates on the whole comint buffer."
  :type '(choice (const :tag "Don't search in input history" nil)
		 (const :tag "When point is on command line initially, search history" dwim)
		 (const :tag "Always search in input history" t))
  :group 'comint
  :version "23.2")

(defun comint-history-isearch-backward ()
  "Search for a string backward in input history using Isearch."
  (interactive)
  (let ((comint-history-isearch t))
    (isearch-backward)))

(defun comint-history-isearch-backward-regexp ()
  "Search for a regular expression backward in input history using Isearch."
  (interactive)
  (let ((comint-history-isearch t))
    (isearch-backward-regexp)))

(defvar comint-history-isearch-message-overlay nil)
(make-variable-buffer-local 'comint-history-isearch-message-overlay)

(defun comint-history-isearch-setup ()
  "Set up a comint for using Isearch to search the input history.
Intended to be added to `isearch-mode-hook' in `comint-mode'."
  (when (or (eq comint-history-isearch t)
	    (and (eq comint-history-isearch 'dwim)
		 ;; Point is at command line.
		 (comint-after-pmark-p)))
    (setq isearch-message-prefix-add "history ")
    (set (make-local-variable 'isearch-search-fun-function)
	 'comint-history-isearch-search)
    (set (make-local-variable 'isearch-message-function)
	 'comint-history-isearch-message)
    (set (make-local-variable 'isearch-wrap-function)
	 'comint-history-isearch-wrap)
    (set (make-local-variable 'isearch-push-state-function)
	 'comint-history-isearch-push-state)
    (add-hook 'isearch-mode-end-hook 'comint-history-isearch-end nil t)))

(defun comint-history-isearch-end ()
  "Clean up the comint after terminating Isearch in comint."
  (if comint-history-isearch-message-overlay
      (delete-overlay comint-history-isearch-message-overlay))
  (setq isearch-message-prefix-add nil)
  (setq isearch-search-fun-function nil)
  (setq isearch-message-function nil)
  (setq isearch-wrap-function nil)
  (setq isearch-push-state-function nil)
  (remove-hook 'isearch-mode-end-hook 'comint-history-isearch-end t))

(defun comint-goto-input (pos)
  "Put input history item of the absolute history position POS."
  ;; If leaving the edit line, save partial unfinished input.
  (if (null comint-input-ring-index)
      (setq comint-stored-incomplete-input
	    (funcall comint-get-old-input)))
  (setq comint-input-ring-index pos)
  (comint-delete-input)
  (if (and pos (not (ring-empty-p comint-input-ring)))
      (insert (ring-ref comint-input-ring pos))
    ;; Restore partial unfinished input.
    (when (> (length comint-stored-incomplete-input) 0)
      (insert comint-stored-incomplete-input))))

(defun comint-history-isearch-search ()
  "Return the proper search function, for Isearch in input history."
  (cond
   (isearch-word
    (if isearch-forward 'word-search-forward 'word-search-backward))
   (t
    (lambda (string bound noerror)
      (let ((search-fun
	     ;; Use standard functions to search within comint text
             (cond
              (isearch-regexp
               (if isearch-forward 're-search-forward 're-search-backward))
              (t
               (if isearch-forward 'search-forward 'search-backward))))
	    found)
	;; Avoid lazy-highlighting matches in the comint prompt and in the
	;; output when searching forward.  Lazy-highlight calls this lambda
	;; with the bound arg, so skip the prompt and the output.
	(if (and bound isearch-forward (not (comint-after-pmark-p)))
	    (goto-char (process-mark (get-buffer-process (current-buffer)))))
        (or
	 ;; 1. First try searching in the initial comint text
	 (funcall search-fun string
		  (if isearch-forward bound (comint-line-beginning-position))
		  noerror)
	 ;; 2. If the above search fails, start putting next/prev history
	 ;; elements in the comint successively, and search the string
	 ;; in them.  Do this only when bound is nil (i.e. not while
	 ;; lazy-highlighting search strings in the current comint text).
	 (unless bound
	   (condition-case nil
	       (progn
		 (while (not found)
		   (cond (isearch-forward
			  ;; Signal an error here explicitly, because
			  ;; `comint-next-input' doesn't signal an error.
			  (when (null comint-input-ring-index)
			    (error "End of history; no next item"))
			  (comint-next-input 1)
			  (goto-char (comint-line-beginning-position)))
			 (t
			  ;; Signal an error here explicitly, because
			  ;; `comint-previous-input' doesn't signal an error.
			  (when (eq comint-input-ring-index
				    (1- (ring-length comint-input-ring)))
			    (error "Beginning of history; no preceding item"))
			  (comint-previous-input 1)
			  (goto-char (point-max))))
		   (setq isearch-barrier (point) isearch-opoint (point))
		   ;; After putting the next/prev history element, search
		   ;; the string in them again, until comint-next-input
		   ;; or comint-previous-input raises an error at the
		   ;; beginning/end of history.
		   (setq found (funcall search-fun string
					(unless isearch-forward
					  ;; For backward search, don't search
					  ;; in the comint prompt
					  (comint-line-beginning-position))
					noerror)))
		 ;; Return point of the new search result
		 (point))
	     ;; Return nil on the error "no next/preceding item"
	     (error nil)))))))))

(defun comint-history-isearch-message (&optional c-q-hack ellipsis)
  "Display the input history search prompt.
If there are no search errors, this function displays an overlay with
the Isearch prompt which replaces the original comint prompt.
Otherwise, it displays the standard Isearch message returned from
`isearch-message'."
  (if (not (and isearch-success (not isearch-error)))
      ;; Use standard function `isearch-message' when not in comint prompt,
      ;; or search fails, or has an error (like incomplete regexp).
      ;; This function displays isearch message in the echo area,
      ;; so it's possible to see what is wrong in the search string.
      (isearch-message c-q-hack ellipsis)
    ;; Otherwise, put the overlay with the standard isearch prompt over
    ;; the initial comint prompt.
    (if (overlayp comint-history-isearch-message-overlay)
	(move-overlay comint-history-isearch-message-overlay
		      (save-excursion (forward-line 0) (point))
                      (comint-line-beginning-position))
      (setq comint-history-isearch-message-overlay
	    (make-overlay (save-excursion (forward-line 0) (point))
                          (comint-line-beginning-position)))
      (overlay-put comint-history-isearch-message-overlay 'evaporate t))
    (overlay-put comint-history-isearch-message-overlay
		 'display (isearch-message-prefix c-q-hack ellipsis))
    ;; And clear any previous isearch message.
    (message "")))

(defun comint-history-isearch-wrap ()
  "Wrap the input history search when search fails.
Move point to the first history element for a forward search,
or to the last history element for a backward search."
  (unless isearch-word
    ;; When `comint-history-isearch-search' fails on reaching the
    ;; beginning/end of the history, wrap the search to the first/last
    ;; input history element.
    (if isearch-forward
	(comint-goto-input (1- (ring-length comint-input-ring)))
      (comint-goto-input nil))
    (setq isearch-success t))
  (goto-char (if isearch-forward (comint-line-beginning-position) (point-max))))

(defun comint-history-isearch-push-state ()
  "Save a function restoring the state of input history search.
Save `comint-input-ring-index' to the additional state parameter
in the search status stack."
  `(lambda (cmd)
     (comint-history-isearch-pop-state cmd ,comint-input-ring-index)))

(defun comint-history-isearch-pop-state (_cmd hist-pos)
  "Restore the input history search state.
Go to the history element by the absolute history position HIST-POS."
  (comint-goto-input hist-pos))


(defun comint-within-quotes (beg end)
  "Return t if the number of quotes between BEG and END is odd.
Quotes are single and double."
  (let ((countsq (comint-how-many-region "\\(^\\|[^\\\\]\\)\'" beg end))
	(countdq (comint-how-many-region "\\(^\\|[^\\\\]\\)\"" beg end)))
    (or (= (mod countsq 2) 1) (= (mod countdq 2) 1))))

(defun comint-how-many-region (regexp beg end)
  "Return number of matches for REGEXP from BEG to END."
  (let ((count 0))
    (save-excursion
      (save-match-data
	(goto-char beg)
	(while (re-search-forward regexp end t)
	  (setq count (1+ count)))))
    count))

(defun comint-args (string begin end)
  ;; From STRING, return the args depending on the range specified in the text
  ;; from BEGIN to END.  If BEGIN is nil, assume all args.  Ignore leading `:'.
  ;; Range can be x-y, x-, -y, where x/y can be [0-9], *, ^, $.
  (save-match-data
    (if (null begin)
	(comint-arguments string 0 nil)
      (let* ((range (buffer-substring
		     (if (eq (char-after begin) ?:) (1+ begin) begin) end))
	     (nth (cond ((string-match "^[*^]" range) 1)
			((string-match "^-" range) 0)
			((string-equal range "$") nil)
			(t (string-to-number range))))
	     (mth (cond ((string-match "[-*$]$" range) nil)
			((string-match "-" range)
			 (string-to-number (substring range (match-end 0))))
			(t nth))))
	(comint-arguments string nth mth)))))

(defun comint-delim-arg (arg)
  "Return a list of arguments from ARG.
Break it up at the delimiters in `comint-delimiter-argument-list'.
Returned list is backwards.

Characters with non-nil values of the text property `literal' are
assumed to have literal values (e.g., backslash-escaped
characters), and are not considered to be delimiters."
  (if (null comint-delimiter-argument-list)
      (list arg)
    (let ((args nil)
	  (pos 0)
	  (len (length arg)))
      (while (< pos len)
	(let ((char (aref arg pos))
	      (start pos))
	  (if (and (memq char comint-delimiter-argument-list)
		   ;; Ignore backslash-escaped characters.
		   (not (get-text-property pos 'literal arg)))
	      (while (and (< pos len) (eq (aref arg pos) char))
		(setq pos (1+ pos)))
	    (while (and (< pos len)
			(not (and (memq (aref arg pos)
					comint-delimiter-argument-list)
				  (not (get-text-property
					pos 'literal arg)))))
	      (setq pos (1+ pos))))
	  (setq args (cons (substring arg start pos) args))))
      args)))

(defun comint-arguments (string nth mth)
  "Return from STRING the NTH to MTH arguments.
NTH and/or MTH can be nil, which means the last argument.
Returned arguments are separated by single spaces.
We assume whitespace separates arguments, except within quotes
and except for a space or tab that immediately follows a backslash.
Also, a run of one or more of a single character
in `comint-delimiter-argument-list' is a separate argument.
Argument 0 is the command name."
  ;; The first line handles ordinary characters and backslash-sequences
  ;; (except with w32 msdos-like shells, where backslashes are valid).
  ;; The second matches "-quoted strings.
  ;; The third matches '-quoted strings.
  ;; The fourth matches `-quoted strings.
  ;; This seems to fit the syntax of BASH 2.0.
  (let* ((backslash-escape (not (and (fboundp 'w32-shell-dos-semantics)
				     (w32-shell-dos-semantics))))
	 (first (if backslash-escape
		    "[^ \n\t\"'`\\]\\|\\(\\\\.\\)\\|"
		  "[^ \n\t\"'`]+\\|"))
	 (argpart (concat first
			  "\\(\"\\([^\"\\]\\|\\\\.\\)*\"\\|\
'[^']*'\\|\
`[^`]*`\\)"))
	 (quote-subexpr (if backslash-escape 2 1))
	 (args ()) (pos 0)
	 (count 0)
	 beg str quotes)
    ;; Build a list of all the args until we have as many as we want.
    (while (and (or (null mth) (<= count mth))
		(string-match argpart string pos))
      ;; Apply the `literal' text property to backslash-escaped
      ;; characters, so that `comint-delim-arg' won't break them up.
      (and backslash-escape
	   (match-beginning 1)
	   (put-text-property (match-beginning 1) (match-end 1)
			      'literal t string))
      (if (and beg (= pos (match-beginning 0)))
	  ;; It's contiguous, part of the same arg.
	  (setq pos (match-end 0)
		quotes (or quotes (match-beginning quote-subexpr)))
	;; It's a new separate arg.
	(if beg
	    ;; Put the previous arg, if there was one, onto ARGS.
	    (setq str (substring string beg pos)
		  args (if quotes (cons str args)
			 (nconc (comint-delim-arg str) args))))
	(setq count (length args))
	(setq quotes (match-beginning quote-subexpr))
	(setq beg (match-beginning 0))
	(setq pos (match-end 0))))
    (if beg
	(setq str (substring string beg pos)
	      args (if quotes (cons str args)
		     (nconc (comint-delim-arg str) args))))
    (setq count (length args))
    (let ((n (or nth (1- count)))
	  (m (if mth (1- (- count mth)) 0)))
      (mapconcat
       (function (lambda (a) a)) (nthcdr n (nreverse (nthcdr m args))) " "))))

;;
;; Input processing stuff
;;
(defun comint-add-to-input-history (cmd)
  "Add CMD to the input history.
Ignore duplicates if `comint-input-ignoredups' is non-nil."
  (when (and (funcall comint-input-filter cmd)
	     (or (null comint-input-ignoredups)
		 (not (ring-p comint-input-ring))
		 (ring-empty-p comint-input-ring)
		 (not (string-equal (ring-ref comint-input-ring 0) cmd))))
    ;; If `comint-input-ring' is full, maybe grow it.
    (let ((size (ring-size comint-input-ring)))
      (and (= size (ring-length comint-input-ring))
	   (< size comint-input-ring-size)
	   (ring-extend comint-input-ring
			(min size (- comint-input-ring-size size)))))
    (ring-insert comint-input-ring cmd)))

(defun comint-send-input (&optional no-newline artificial)
  "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input.

If variable `comint-eol-on-send' is non-nil, then point is moved to the
end of line before sending the input.

After the input has been sent, if `comint-process-echoes' is non-nil,
then `comint-send-input' waits to see if the process outputs a string
matching the input, and if so, deletes that part of the output.
If ARTIFICIAL is non-nil, it inhibits such deletion.
Callers sending input not from the user should use ARTIFICIAL = t.

The values of `comint-get-old-input', `comint-input-filter-functions', and
`comint-input-filter' are chosen according to the command interpreter running
in the buffer.  E.g.,

If the interpreter is the csh,
    `comint-get-old-input' is the default:
	If `comint-use-prompt-regexp' is nil, then
	either return the current input field, if point is on an input
	field, or the current line, if point is on an output field.
	If `comint-use-prompt-regexp' is non-nil, then
	return the current line with any initial string matching the
	regexp `comint-prompt-regexp' removed.
    `comint-input-filter-functions' monitors input for \"cd\", \"pushd\", and
	\"popd\" commands.  When it sees one, it cd's the buffer.
    `comint-input-filter' is the default: returns t if the input isn't all white
	space.

If the Comint is Lucid Common Lisp,
    `comint-get-old-input' snarfs the sexp ending at point.
    `comint-input-filter-functions' does nothing.
    `comint-input-filter' returns nil if the input matches input-filter-regexp,
	which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send (end-of-line))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        ;; Just whatever's already there.
                        intxt
                      ;; Expand and leave it visible in buffer.
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
             (history (if (not (eq comint-input-autoexpand 'history))
                          input
                        ;; This is messy 'cos ultimately the original
                        ;; functions used do insertion, rather than return
                        ;; strings.  We have to expand, then insert back.
                        (comint-replace-by-expanded-history t pmark)
                        (let ((copy (buffer-substring pmark (point)))
                              (start (point)))
                          (insert input)
                          (delete-region pmark start)
                          copy))))

        (unless no-newline
          (insert ?\n))

        (comint-add-to-input-history history)

        (run-hook-with-args 'comint-input-filter-functions
                            (if no-newline input
                              (concat input "\n")))

        (let ((beg (marker-position pmark))
              (end (if no-newline (point) (1- (point))))
              (inhibit-modification-hooks t))
          (when (> end beg)
            (add-text-properties beg end
                                 '(front-sticky t
                                   font-lock-face comint-highlight-input))
            (unless comint-use-prompt-regexp
              ;; Give old user input a field property of `input', to
              ;; distinguish it from both process output and unsent
              ;; input.  The terminating newline is put into a special
              ;; `boundary' field to make cursor movement between input
              ;; and output fields smoother.
              (add-text-properties
               beg end
               '(mouse-face highlight
                 help-echo "mouse-2: insert after prompt as new input"))))
          (unless (or no-newline comint-use-prompt-regexp)
            ;; Cover the terminating newline
            (add-text-properties end (1+ end)
                                 '(rear-nonsticky t
                                   field boundary
                                   inhibit-line-move-field-capture t))))

        (comint-snapshot-last-prompt)

        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)
        ;; Update the markers before we send the input
        ;; in case we get output amidst sending the input.
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        ;; clear the "accumulation" marker
        (set-marker comint-accum-marker nil)
        (let ((comint-input-sender-no-newline no-newline))
          (funcall comint-input-sender proc input))

        ;; Optionally delete echoed input (after checking it).
        (when (and comint-process-echoes (not artificial))
          (let ((echo-len (- comint-last-input-end
                             comint-last-input-start)))
            ;; Wait for all input to be echoed:
            (while (and (accept-process-output proc)
                        (> (+ comint-last-input-end echo-len)
                           (point-max))
                        (zerop
                         (compare-buffer-substrings
                          nil comint-last-input-start
                          (- (point-max) echo-len)
                          ;; Above difference is equivalent to
                          ;; (+ comint-last-input-start
                          ;;    (- (point-max) comint-last-input-end))
                          nil comint-last-input-end (point-max)))))
            (if (and
                 (<= (+ comint-last-input-end echo-len)
                     (point-max))
                 (zerop
                  (compare-buffer-substrings
                   nil comint-last-input-start comint-last-input-end
                   nil comint-last-input-end
                   (+ comint-last-input-end echo-len))))
                ;; Certain parts of the text to be deleted may have
                ;; been mistaken for prompts.  We have to prevent
                ;; problems when `comint-prompt-read-only' is non-nil.
                (let ((inhibit-read-only t))
                  (delete-region comint-last-input-end
                                 (+ comint-last-input-end echo-len))
                  (when comint-prompt-read-only
                    (save-excursion
                      (goto-char comint-last-input-end)
                      (comint-update-fence)))))))

        ;; This used to call comint-output-filter-functions,
        ;; but that scrolled the buffer in undesirable ways.
        (run-hook-with-args 'comint-output-filter-functions "")))))

(defvar comint-preoutput-filter-functions nil
  "List of functions to call before inserting Comint output into the buffer.
Each function gets one argument, a string containing the text received
from the subprocess.  It should return the string to insert, perhaps
the same string that was received, or perhaps a modified or transformed
string.

The functions on the list are called sequentially, and each one is
given the string returned by the previous one.  The string returned by
the last function is the text that is actually inserted in the
redirection buffer.

You can use `add-hook' to add functions to this list
either globally or locally.")

(defvar comint-inhibit-carriage-motion nil
  "If nil, Comint will interpret `carriage control' characters in output.
See `comint-carriage-motion' for details.")

;; When non-nil, this is an overlay over the last recognized prompt in
;; the buffer; it is used when highlighting the prompt.
(defvar comint-last-prompt-overlay nil)

(defun comint-snapshot-last-prompt ()
  "`snapshot' any current `comint-last-prompt-overlay'.
Freeze its attributes in place, even when more input comes along
and moves the prompt overlay."
  (when comint-last-prompt-overlay
    (let ((inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (add-text-properties (overlay-start comint-last-prompt-overlay)
			   (overlay-end comint-last-prompt-overlay)
			   (overlay-properties comint-last-prompt-overlay)))))

(defun comint-carriage-motion (start end)
  "Interpret carriage control characters in the region from START to END.
Translate carriage return/linefeed sequences to linefeeds.
Make single carriage returns delete to the beginning of the line.
Make backspaces delete the previous character."
  (save-excursion
    ;; We used to check the existence of \b and \r at first to avoid
    ;; calling save-match-data and save-restriction.  But, such a
    ;; check is not necessary now because we don't use regexp search
    ;; nor save-restriction.  Note that the buffer is already widen,
    ;; and calling narrow-to-region and widen are not that heavy.
    (goto-char start)
    (let* ((inhibit-field-text-motion t)
	   (inhibit-read-only t)
	   (lbeg (line-beginning-position))
	   delete-end ch)
      ;; If the preceding text is marked as "must-overwrite", record
      ;; it in delete-end.
      (when (and (> start (point-min))
		 (get-text-property (1- start) 'comint-must-overwrite))
	(setq delete-end (point-marker))
	(remove-text-properties lbeg start '(comint-must-overwrite nil)))
      (narrow-to-region lbeg end)
      ;; Handle BS, LF, and CR specially.
      (while (and (skip-chars-forward "^\b\n\r") (not (eobp)))
	(setq ch (following-char))
	(cond ((= ch ?\b)		; CH = BS
	       (delete-char 1)
	       (if (> (point) lbeg)
		   (delete-char -1)))
	      ((= ch ?\n)
	       (when delete-end		; CH = LF
		 (if (< delete-end (point))
		     (delete-region lbeg delete-end))
		 (set-marker delete-end nil)
		 (setq delete-end nil))
	       (forward-char 1)
	       (setq lbeg (point)))
	      (t			; CH = CR
	       (delete-char 1)
	       (if delete-end
		   (when (< delete-end (point))
		     (delete-region lbeg delete-end)
		     (move-marker delete-end (point)))
		 (setq delete-end (point-marker))))))
      (when delete-end
	(if (< delete-end (point))
	    ;; As there's a text after the last CR, make the current
	    ;; line contain only that text.
	    (delete-region lbeg delete-end)
	  ;; Remember that the process output ends by CR, and thus we
	  ;; must overwrite the contents of the current line next
	  ;; time.
	  (put-text-property lbeg delete-end 'comint-must-overwrite t))
	(set-marker delete-end nil))
      (widen))))

;; The purpose of using this filter for comint processes
;; is to keep comint-last-input-end from moving forward
;; when output is inserted.
(defun comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
	;; Run preoutput filters
	(let ((functions comint-preoutput-filter-functions))
	  (while (and functions string)
	    (if (eq (car functions) t)
		(let ((functions
                       (default-value 'comint-preoutput-filter-functions)))
		  (while (and functions string)
		    (setq string (funcall (car functions) string))
		    (setq functions (cdr functions))))
	      (setq string (funcall (car functions) string)))
	    (setq functions (cdr functions))))

	;; Insert STRING
	(let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
	      (saved-point (copy-marker (point) t)))

	  ;; We temporarily remove any buffer narrowing, in case the
	  ;; process mark is outside of the restriction
	  (save-restriction
	    (widen)

	    (goto-char (process-mark process))
	    (set-marker comint-last-output-start (point))

	    ;; insert-before-markers is a bad thing. XXX
	    ;; Luckily we don't have to use it any more, we use
	    ;; window-point-insertion-type instead.
	    (insert string)

	    ;; Advance process-mark
	    (set-marker (process-mark process) (point))

	    (unless comint-inhibit-carriage-motion
	      ;; Interpret any carriage motion characters (newline, backspace)
	      (comint-carriage-motion comint-last-output-start (point)))

	    ;; Run these hooks with point where the user had it.
	    (goto-char saved-point)
	    (run-hook-with-args 'comint-output-filter-functions string)
	    (set-marker saved-point (point))

	    (goto-char (process-mark process)) ; in case a filter moved it

	    (unless comint-use-prompt-regexp
              (let ((inhibit-read-only t)
		    (inhibit-modification-hooks t))
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
				       (field inhibit-line-move-field-capture)
				       rear-nonsticky t
				       field output
				       inhibit-line-move-field-capture t))))

	    ;; Highlight the prompt, where we define `prompt' to mean
	    ;; the most recent output that doesn't end with a newline.
	    (let ((prompt-start (save-excursion (forward-line 0) (point)))
		  (inhibit-read-only t)
		  (inhibit-modification-hooks t))
	      (when comint-prompt-read-only
		(or (= (point-min) prompt-start)
		    (get-text-property (1- prompt-start) 'read-only)
		    (put-text-property
		     (1- prompt-start) prompt-start 'read-only 'fence))
		(add-text-properties
		 prompt-start (point)
		 '(read-only t rear-nonsticky t front-sticky (read-only))))
	      (unless (and (bolp) (null comint-last-prompt-overlay))
		;; Need to create or move the prompt overlay (in the case
		;; where there is no prompt ((bolp) == t), we still do
		;; this if there's already an existing overlay).
		(if comint-last-prompt-overlay
		    ;; Just move an existing overlay
		    (move-overlay comint-last-prompt-overlay
				  prompt-start (point))
		  ;; Need to create the overlay
		  (setq comint-last-prompt-overlay
			(make-overlay prompt-start (point)))
		  (overlay-put comint-last-prompt-overlay
			       'font-lock-face 'comint-highlight-prompt))))

	    (goto-char saved-point)))))))

(defun comint-preinput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Movement occurs if point in the selected window is not after the process mark,
and `this-command' is an insertion command.  Insertion commands recognized
are `self-insert-command', `comint-magic-space', `yank', and `hilit-yank'.
Depends on the value of `comint-scroll-to-bottom-on-input'.

This function should be a pre-command hook."
  (if (and comint-scroll-to-bottom-on-input
	   (memq this-command '(self-insert-command comint-magic-space yank
				hilit-yank)))
      (let* ((selected (selected-window))
	     (current (current-buffer))
	     (process (get-buffer-process current))
	     (scroll comint-scroll-to-bottom-on-input))
	(if (and process (< (point) (process-mark process)))
	    (if (eq scroll 'this)
		(goto-char (point-max))
	      (walk-windows
               (lambda (window)
                 (if (and (eq (window-buffer window) current)
                          (or (eq scroll t) (eq scroll 'all)))
                     (progn
                       (select-window window)
                       (goto-char (point-max))
                       (select-window selected))))
	       nil t))))))

(defun comint-postoutput-scroll-to-bottom (_string)
  "Go to the end of buffer in some or all windows showing it.
Does not scroll if the current line is the last line in the buffer.
Depends on the value of `comint-move-point-for-output' and
`comint-scroll-show-maximum-output'.

This function should be in the list `comint-output-filter-functions'."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (process (get-buffer-process current))
	 (scroll comint-move-point-for-output))
    (unwind-protect
	(if process
	    (walk-windows
             (lambda (window)
               (when (eq (window-buffer window) current)
                 (select-window window)
                 (if (and (< (point) (process-mark process))
                          (or (eq scroll t) (eq scroll 'all)
                              ;; Maybe user wants point to jump to end.
                              (and (eq scroll 'this) (eq selected window))
                              (and (eq scroll 'others) (not (eq selected window)))
                              ;; If point was at the end, keep it at end.
                              (and (marker-position comint-last-output-start)
                                   (>= (point) comint-last-output-start))))
                     (goto-char (process-mark process)))
                 ;; Optionally scroll so that the text
                 ;; ends at the bottom of the window.
                 (if (and comint-scroll-show-maximum-output
                          (= (point) (point-max)))
                     (save-excursion
                       (goto-char (point-max))
                       (recenter (- -1 scroll-margin))))
                 (select-window selected)))
	     nil t))
      (set-buffer current))))

(defun comint-truncate-buffer (&optional _string)
  "Truncate the buffer to `comint-buffer-maximum-size'.
This function could be on `comint-output-filter-functions' or bound to a key."
  (interactive)
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line (- comint-buffer-maximum-size))
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point)))))

(defun comint-strip-ctrl-m (&optional _string)
  "Strip trailing `^M' characters from the current output group.
This function could be on `comint-output-filter-functions' or bound to a key."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (condition-case nil
	  (goto-char
	   (if (called-interactively-p 'interactive)
	       comint-last-input-end comint-last-output-start))
	(error nil))
      (while (re-search-forward "\r+$" pmark t)
	(replace-match "" t t)))))
(defalias 'shell-strip-ctrl-m 'comint-strip-ctrl-m)

(defun comint-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (interactive)
  (goto-char (point-max))
  (recenter (- -1 scroll-margin)))

(defun comint-get-old-input-default ()
  "Default for `comint-get-old-input'.
If `comint-use-prompt-regexp' is nil, then either
return the current input field, if point is on an input field, or the
current line, if point is on an output field.
If `comint-use-prompt-regexp' is non-nil, then return
the current line with any initial string matching the regexp
`comint-prompt-regexp' removed."
  (let (bof)
    (if (and (not comint-use-prompt-regexp)
             ;; Make sure we're in an input rather than output field.
             (null (get-char-property (setq bof (field-beginning)) 'field)))
	(field-string-no-properties bof)
      (comint-bol)
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun comint-copy-old-input ()
  "Insert after prompt old input at point as new input to be edited.
Calls `comint-get-old-input' to get old input."
  (interactive)
  (let ((input (funcall comint-get-old-input))
	(process (get-buffer-process (current-buffer))))
    (if (not process)
	(error "Current buffer has no process")
      (goto-char (process-mark process))
      (insert input))))

(defun comint-skip-prompt ()
  "Skip past the text matching regexp `comint-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (if (and (looking-at comint-prompt-regexp)
	   (<= (match-end 0) (line-end-position)))
      (goto-char (match-end 0))))

(defun comint-after-pmark-p ()
  "Return t if point is after the process output marker."
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (<= (marker-position pmark) (point))))

(defun comint-simple-send (proc string)
  "Default function for sending to PROC input STRING.
This just sends STRING plus a newline.  To override this,
set the hook `comint-input-sender'."
  (let ((send-string
         (if comint-input-sender-no-newline
             string
           ;; Sending as two separate strings does not work
           ;; on Windows, so concat the \n before sending.
           (concat string "\n"))))
    (comint-send-string proc send-string))
  (if (and comint-input-sender-no-newline
	   (not (string-equal string "")))
      (process-send-eof)))

(defun comint-line-beginning-position ()
  "Return the buffer position of the beginning of the line, after any prompt.
If `comint-use-prompt-regexp' is non-nil, then the prompt skip is done by
skipping text matching the regular expression `comint-prompt-regexp',
a buffer local variable."
  (if comint-use-prompt-regexp
      ;; Use comint-prompt-regexp
      (save-excursion
	(beginning-of-line)
	(comint-skip-prompt)
	(point))
    ;; Use input fields.  Note that, unlike the behavior of
    ;; `line-beginning-position' inside a field, this function will
    ;; return the position of the end of a prompt, even if the point is
    ;; already inside the prompt.  In order to do this, it assumes that
    ;; if there are two fields on a line, then the first one is the
    ;; prompt, and the second one is an input field, and is front-sticky
    ;; (as input fields should be).
    (constrain-to-field (line-beginning-position) (line-end-position))))

(defun comint-bol (&optional arg)
  "Go to the beginning of line, then skip past the prompt, if any.
If prefix argument is given (\\[universal-argument]) the prompt is not skipped.
If `comint-use-prompt-regexp' is non-nil, then the prompt skip is done
by skipping text matching the regular expression `comint-prompt-regexp',
a buffer local variable."
  (interactive "P")
  (if arg
      ;; Unlike `beginning-of-line', forward-line ignores field boundaries
      (forward-line 0)
    (goto-char (comint-line-beginning-position))))

;; For compatibility.
(defun comint-read-noecho (prompt &optional _ignore)
  (read-passwd prompt))

;; These three functions are for entering text you don't want echoed or
;; saved -- typically passwords to ftp, telnet, or somesuch.
;; Just enter m-x send-invisible and type in your line.

(defun send-invisible (&optional prompt)
  "Read a string without echoing.
Then send it to the process running in the current buffer.
The string is sent using `comint-input-sender'.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]; `clear-this-command-keys' can fix that."
  (interactive "P")			; Defeat snooping via C-x ESC ESC
  (let ((proc (get-buffer-process (current-buffer)))
	(prefix
	 (if (eq (window-buffer (selected-window)) (current-buffer))
	     ""
	   (format "(In buffer %s) "
		   (current-buffer)))))
    (if proc
	(let ((str (read-passwd (concat prefix
					(or prompt "Non-echoed text: ")))))
	  (if (stringp str)
	      (progn
		(comint-snapshot-last-prompt)
		(funcall comint-input-sender proc str))
	    (message "Warning: text will be echoed")))
      (error "Buffer %s has no process" (current-buffer)))))

(defun comint-watch-for-password-prompt (string)
  "Prompt in the minibuffer for password and send without echoing.
This function uses `send-invisible' to read and send a password to the buffer's
process if STRING contains a password prompt defined by
`comint-password-prompt-regexp'.

This function could be in the list `comint-output-filter-functions'."
  (when (string-match comint-password-prompt-regexp string)
    (when (string-match "^[ \n\r\t\v\f\b\a]+" string)
      (setq string (replace-match "" t t string)))
    (send-invisible string)))

;; Low-level process communication

(defun comint-send-string (process string)
  "Like `process-send-string', but also does extra bookkeeping for Comint mode."
  (if process
      (with-current-buffer (if (processp process)
			       (process-buffer process)
			     (get-buffer process))
	(comint-snapshot-last-prompt))
    (comint-snapshot-last-prompt))
  (process-send-string process string))

(defun comint-send-region (process start end)
  "Like `process-send-region', but also does extra bookkeeping for Comint mode."
  (if process
      (with-current-buffer (if (processp process)
			       (process-buffer process)
			     (get-buffer process))
	(comint-snapshot-last-prompt))
    (comint-snapshot-last-prompt))
  (process-send-region process start end))


;; Random input hackage

(defun comint-delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(replacement nil)
	(inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
			  (forward-line 0)
			  (point-marker))))
	(delete-region comint-last-input-end pmark)
	(goto-char (process-mark proc))
	(setq replacement (concat "*** output flushed ***\n"
				  (buffer-substring pmark (point))))
	(delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

(defun comint-write-output (filename &optional append mustbenew)
  "Write output from interpreter since last input to FILENAME.
Any prompt at the end of the output is not written.

If the optional argument APPEND (the prefix argument when interactive)
is non-nil, the output is appended to the file instead.

If the optional argument MUSTBENEW is non-nil, check for an existing
file with the same name.  If MUSTBENEW is `excl', that means to get an
error if the file already exists; never overwrite.  If MUSTBENEW is
neither nil nor `excl', that means ask for confirmation before
overwriting, but do go ahead and overwrite the file if the user
confirms.  When interactive, MUSTBENEW is nil when appending, and t
otherwise."
  (interactive
   (list (read-file-name
	  (if current-prefix-arg
	      "Append output to file: "
	    "Write output to file: "))
	 current-prefix-arg
	 (not current-prefix-arg)))
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line 0)
    (write-region comint-last-input-end (point) filename
		  append nil nil mustbenew)))

;; This function exists for the benefit of the menu; from the keyboard,
;; users can just use `comint-write-output' with a prefix arg.
(defun comint-append-output-to-file (filename)
  "Append output from interpreter since last input to FILENAME.
Any prompt at the end of the output is not written."
  (interactive "fAppend output to file: ")
  (comint-write-output filename t))

(defun comint-show-output ()
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run."
  (interactive)
  (push-mark)
  (let ((pos (or (marker-position comint-last-input-end) (point-max))))
    (cond (comint-use-prompt-regexp
	   (goto-char pos)
	   (beginning-of-line 0)
	   (set-window-start (selected-window) (point))
	   (comint-skip-prompt))
	  (t
	   (goto-char (field-beginning pos))
	   (set-window-start (selected-window) (point))))))


(defun comint-interrupt-subjob ()
  "Interrupt the current subjob.
This command also kills the pending input
between the process mark and point."
  (interactive)
  (comint-skip-input)
  (interrupt-process nil comint-ptyp)
  ;; (process-send-string nil "\n")
  )

(defun comint-kill-subjob ()
  "Send kill signal to the current subjob.
This command also kills the pending input
between the process mark and point."
  (interactive)
  (comint-skip-input)
  (kill-process nil comint-ptyp))

(defun comint-quit-subjob ()
  "Send quit signal to the current subjob.
This command also kills the pending input
between the process mark and point."
  (interactive)
  (comint-skip-input)
  (quit-process nil comint-ptyp))

(defun comint-stop-subjob ()
  "Stop the current subjob.
This command also kills the pending input
between the process mark and point.

WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer.  If you accidentally do
this, use \\[comint-continue-subjob] to resume the process.  (This
is not a problem with most shells, since they ignore this signal.)"
  (interactive)
  (comint-skip-input)
  (stop-process nil comint-ptyp))

(defun comint-continue-subjob ()
  "Send CONT signal to process buffer's process group.
Useful if you accidentally suspend the top-level process."
  (interactive)
  (continue-process nil comint-ptyp))

(defun comint-skip-input ()
  "Skip all pending input, from last stuff output by interpreter to point.
This means mark it as if it had been sent as input, without sending it."
  (let ((comint-input-sender 'ignore)
	(comint-input-filter-functions nil))
    (comint-send-input t t))
  (end-of-line)
  (let ((pos (point))
	(marker (process-mark (get-buffer-process (current-buffer)))))
    (insert "  " (key-description (this-command-keys)))
    (if (= marker pos)
	(set-marker marker (point)))))

(defun comint-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (> (point) (marker-position pmark))
	(kill-region pmark (point)))))

(defun comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward or send an EOF to subprocess.
Sends an EOF only if point is at the end of the buffer and there is no input."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
	(comint-send-eof)
      (delete-char arg))))

(defun comint-send-eof ()
  "Send an EOF to the current buffer's process."
  (interactive)
  (comint-send-input t t)
  (process-send-eof))


(defun comint-backward-matching-input (regexp n)
  "Search backward through buffer for input fields that match REGEXP.
If `comint-use-prompt-regexp' is non-nil, then input fields are identified
by lines that match `comint-prompt-regexp'.

With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (comint-regexp-arg "Backward input matching (regexp): "))
  (if comint-use-prompt-regexp
      ;; Use comint-prompt-regexp
      (let* ((re (concat comint-prompt-regexp ".*" regexp))
	     (pos (save-excursion (end-of-line (if (> n 0) 0 1))
				  (if (re-search-backward re nil t n)
				      (point)))))
	(if (null pos)
	    (progn (message "Not found")
		   (ding))
	  (goto-char pos)
	  (comint-bol nil)))
    ;; Use input fields
    (let* ((dir (if (< n 0) -1 1))
	   (pos
	    (save-excursion
	      (while (/= n 0)
		(unless (re-search-backward regexp nil t dir)
		  (error "Not found"))
		(unless (get-char-property (point) 'field)
		  (setq n (- n dir))))
	      (field-beginning))))
      (goto-char pos))))


(defun comint-forward-matching-input (regexp n)
  "Search forward through buffer for input fields that match REGEXP.
If `comint-use-prompt-regexp' is non-nil, then input fields are identified
by lines that match `comint-prompt-regexp'.

With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (comint-regexp-arg "Forward input matching (regexp): "))
  (comint-backward-matching-input regexp (- n)))


(defun comint-next-prompt (n)
  "Move to end of Nth next prompt in the buffer.
If `comint-use-prompt-regexp' is nil, then this means the beginning of
the Nth next `input' field, otherwise, it means the Nth occurrence of
text matching `comint-prompt-regexp'."
  (interactive "p")
  (if comint-use-prompt-regexp
      ;; Use comint-prompt-regexp
      (let ((paragraph-start comint-prompt-regexp))
	(end-of-line (if (> n 0) 1 0))
	(forward-paragraph n)
	(comint-skip-prompt))
    ;; Use input fields
    (let ((pos (point))
	  (input-pos nil)
	  prev-pos)
      (while (/= n 0)
	(setq prev-pos pos)
	(setq pos
	      (if (> n 0)
		  (next-single-char-property-change pos 'field)
		(previous-single-char-property-change pos 'field)))
	(cond ((= pos prev-pos)
	       ;; Ran off the end of the buffer.
	       (when (> n 0)
		 ;; There's always an input field at the end of the
		 ;; buffer, but it has a `field' property of nil.
		 (setq input-pos (point-max)))
	       ;; stop iterating
	       (setq n 0))
	      ((null (get-char-property pos 'field))
	       (setq n (if (< n 0) (1+ n) (1- n)))
	       (setq input-pos pos))))
      (when input-pos
	(goto-char input-pos)))))


(defun comint-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer.
If `comint-use-prompt-regexp' is nil, then this means the beginning of
the Nth previous `input' field, otherwise, it means the Nth occurrence of
text matching `comint-prompt-regexp'."
  (interactive "p")
  (comint-next-prompt (- n)))

;; State used by `comint-insert-previous-argument' when cycling.
(defvar comint-insert-previous-argument-last-start-pos nil)
(make-variable-buffer-local 'comint-insert-previous-argument-last-start-pos)
(defvar comint-insert-previous-argument-last-index nil)
(make-variable-buffer-local 'comint-insert-previous-argument-last-index)

;; Needs fixing:
;;  make comint-arguments understand negative indices as bash does
(defun comint-insert-previous-argument (index)
  "Insert the INDEXth argument from the previous Comint command-line at point.
Spaces are added at beginning and/or end of the inserted string if
necessary to ensure that it's separated from adjacent arguments.
Interactively, if no prefix argument is given, the last argument is inserted.
Repeated interactive invocations will cycle through the same argument
from progressively earlier commands (using the value of INDEX specified
with the first command).
This command is like `M-.' in bash."
  (interactive "P")
  (unless (null index)
    (setq index (prefix-numeric-value index)))
  (cond ((eq last-command this-command)
	 ;; Delete last input inserted by this command.
	 (delete-region comint-insert-previous-argument-last-start-pos (point))
	 (setq index comint-insert-previous-argument-last-index))
	(t
	 ;; This is a non-repeat invocation, so initialize state.
	 (setq comint-input-ring-index nil)
	 (setq comint-insert-previous-argument-last-index index)
	 (when (null comint-insert-previous-argument-last-start-pos)
	   ;; First usage; initialize to a marker
	   (setq comint-insert-previous-argument-last-start-pos
		 (make-marker)))))
  ;; Make sure we're not in the prompt, and add a beginning space if necessary.
  (if (<= (point) (comint-line-beginning-position))
      (comint-bol)
    (just-one-space))
  ;; Remember the beginning of what we insert, so we can delete it if
  ;; the command is repeated.
  (set-marker comint-insert-previous-argument-last-start-pos (point))
  ;; Insert the argument.
  (let ((input-string (comint-previous-input-string 0)))
    (when (string-match "[ \t\n]*&" input-string)
      ;; strip terminating '&'
      (setq input-string (substring input-string 0 (match-beginning 0))))
    (insert (comint-arguments input-string index index)))
  ;; Make next invocation return arg from previous input
  (setq comint-input-ring-index (1+ (or comint-input-ring-index 0)))
  ;; Add a terminating space if necessary.
  (unless (eolp)
    (just-one-space)))


;; Support editing with `comint-prompt-read-only' set to t.

(defun comint-update-fence ()
  "Update read-only status of newline before point.
The `fence' read-only property is used to indicate that a newline
is read-only for no other reason than to \"fence off\" a
following front-sticky read-only region.  This is used to
implement comint read-only prompts.  If the text after a newline
changes, the read-only status of that newline may need updating.
That is what this function does.

This function does nothing if point is not at the beginning of a
line, or is at the beginning of the accessible portion of the buffer.
Otherwise, if the character after point has a front-sticky
read-only property, then the preceding newline is given a
read-only property of `fence', unless it already is read-only.
If the character after point does not have a front-sticky
read-only property, any read-only property of `fence' on the
preceding newline is removed."
  (let* ((pt (point)) (lst (get-text-property pt 'front-sticky))
	 (inhibit-modification-hooks t))
    (and (bolp)
	 (not (bobp))
	 (if (and (get-text-property pt 'read-only)
		  (if (listp lst) (memq 'read-only lst) t))
	     (unless (get-text-property (1- pt) 'read-only)
	       (put-text-property (1- pt) pt 'read-only 'fence))
	   (when (eq (get-text-property (1- pt) 'read-only) 'fence)
	     (remove-list-of-text-properties (1- pt) pt '(read-only)))))))

(defun comint-kill-whole-line (&optional count)
  "Kill current line, ignoring read-only and field properties.
With prefix arg COUNT, kill that many lines starting from the current line.
If COUNT is negative, kill backward.  Also kill the preceding newline,
instead of the trailing one.  \(This is meant to make \\[repeat] work well
with negative arguments.)
If COUNT is zero, kill current line but exclude the trailing newline.
The read-only status of newlines is updated with `comint-update-fence',
if necessary."
  (interactive "p")
  (let ((inhibit-read-only t) (inhibit-field-text-motion t))
    (kill-whole-line count)
    (when (>= count 0) (comint-update-fence))))

(defun comint-kill-region (beg end &optional yank-handler)
  "Like `kill-region', but ignores read-only properties, if safe.
This command assumes that the buffer contains read-only
\"prompts\" which are regions with front-sticky read-only
properties at the beginning of a line, with the preceding newline
being read-only to protect the prompt.  This is true of the
comint prompts if `comint-prompt-read-only' is non-nil.  This
command will not delete the region if this would create mutilated
or out of place prompts.  That is, if any part of a prompt is
deleted, the entire prompt must be deleted and all remaining
prompts should stay at the beginning of a line.  If this is not
the case, this command just calls `kill-region' with all
read-only properties intact.  The read-only status of newlines is
updated using `comint-update-fence', if necessary."
  (interactive "r")
  (save-excursion
    (let* ((true-beg (min beg end))
	   (true-end (max beg end))
	   (beg-bolp (progn (goto-char true-beg) (bolp)))
	   (beg-lst (get-text-property true-beg 'front-sticky))
	   (beg-bad (and (get-text-property true-beg 'read-only)
			 (if (listp beg-lst) (memq 'read-only beg-lst) t)))
	   (end-bolp (progn (goto-char true-end) (bolp)))
	   (end-lst (get-text-property true-end 'front-sticky))
	   (end-bad (and (get-text-property true-end 'read-only)
			 (if (listp end-lst) (memq 'read-only end-lst) t))))
      (if (or (and (not beg-bolp) (or beg-bad end-bad))
	      (and (not end-bolp) end-bad))
	  (kill-region beg end yank-handler)
	(let ((inhibit-read-only t))
	  (kill-region beg end yank-handler)
	  (comint-update-fence))))))
(set-advertised-calling-convention 'comint-kill-region '(beg end) "23.3")


;; Support for source-file processing commands.
;;============================================================================
;; Many command-interpreters (e.g., Lisp, Scheme, Soar) have
;; commands that process files of source text (e.g. loading or compiling
;; files).  So the corresponding process-in-a-buffer modes have commands
;; for doing this (e.g., lisp-load-file).  The functions below are useful
;; for defining these commands.
;;
;; Alas, these guys don't do exactly the right thing for Lisp, Scheme
;; and Soar, in that they don't know anything about file extensions.
;; So the compile/load interface gets the wrong default occasionally.
;; The load-file/compile-file default mechanism could be smarter -- it
;; doesn't know about the relationship between filename extensions and
;; whether the file is source or executable.  If you compile foo.lisp
;; with compile-file, then the next load-file should use foo.bin for
;; the default, not foo.lisp.  This is tricky to do right, particularly
;; because the extension for executable files varies so much (.o, .bin,
;; .lbin, .mo, .vo, .ao, ...).


;; COMINT-SOURCE-DEFAULT -- determines defaults for source-file processing
;; commands.
;;
;; COMINT-CHECK-SOURCE -- if FNAME is in a modified buffer, asks you if you
;; want to save the buffer before issuing any process requests to the command
;; interpreter.
;;
;; COMINT-GET-SOURCE -- used by the source-file processing commands to prompt
;; for the file to process.

(defun comint-source-default (previous-dir/file source-modes)
  "Compute the defaults for `load-file' and `compile-file' commands.

PREVIOUS-DIR/FILE is a pair (DIRECTORY . FILENAME) from the last
source-file processing command, or nil if there hasn't been one yet.
SOURCE-MODES is a list used to determine what buffers contain source
files: if the major mode of the buffer is in SOURCE-MODES, it's source.
Typically, (lisp-mode) or (scheme-mode).

If the command is given while the cursor is inside a string, *and*
the string is an existing filename, *and* the filename is not a directory,
then the string is taken as default.  This allows you to just position
your cursor over a string that's a filename and have it taken as default.

If the command is given in a file buffer whose major mode is in
SOURCE-MODES, then the filename is the default file, and the
file's directory is the default directory.

If the buffer isn't a source file buffer (e.g., it's the process buffer),
then the default directory & file are what was used in the last source-file
processing command (i.e., PREVIOUS-DIR/FILE).  If this is the first time
the command has been run (PREVIOUS-DIR/FILE is nil), the default directory
is the cwd, with no default file.  (\"no default file\" = nil)

SOURCE-MODES is typically going to be something like (tea-mode)
for T programs, (lisp-mode) for Lisp programs, (soar-mode lisp-mode)
for Soar programs, etc.

The function returns a pair: (default-directory . default-file)."
  (cond ((and buffer-file-name (memq major-mode source-modes))
	 (cons (file-name-directory    buffer-file-name)
	       (file-name-nondirectory buffer-file-name)))
	(previous-dir/file)
	(t
	 (cons default-directory nil))))


(defun comint-check-source (fname)
  "Check whether to save buffers visiting file FNAME.
Prior to loading or compiling (or otherwise processing) a file (in the CMU
process-in-a-buffer modes), this function can be called on the filename.
If the file is loaded into a buffer, and the buffer is modified, the user
is queried to see if he wants to save the buffer before proceeding with
the load or compile."
  (let ((buff (get-file-buffer fname)))
    (if (and buff
	     (buffer-modified-p buff)
	     (y-or-n-p (format "Save buffer %s first? " (buffer-name buff))))
	;; save BUFF.
	(let ((old-buffer (current-buffer)))
	  (set-buffer buff)
	  (save-buffer)
	  (set-buffer old-buffer)))))

(defun comint-extract-string ()
  "Return string around point, or nil."
  (let ((syntax (syntax-ppss)))
    (when (nth 3 syntax)
      (condition-case ()
	  (buffer-substring-no-properties (1+ (nth 8 syntax))
					  (progn (goto-char (nth 8 syntax))
						 (forward-sexp)
						 (1- (point))))
	(error nil)))))

(defun comint-get-source (prompt prev-dir/file source-modes mustmatch-p)
  "Prompt for filenames in commands that process source files,
e.g. loading or compiling a file.
Provides a default, if there is one, and returns the result filename.

See `comint-source-default' for more on determining defaults.

PROMPT is the prompt string.  PREV-DIR/FILE is the (DIRECTORY . FILE) pair
from the last source processing command.  SOURCE-MODES is a list of major
modes used to determine what file buffers contain source files.  (These
two arguments are used for determining defaults.)  If MUSTMATCH-P is true,
then the filename reader will only accept a file that exists.

A typical use:
 (interactive (comint-get-source \"Compile file: \" prev-lisp-dir/file
                                 '(lisp-mode) t))"
  (let* ((def (comint-source-default prev-dir/file source-modes))
	 (stringfile (comint-extract-string))
	 (sfile-p (and stringfile
		       (condition-case ()
			   (file-exists-p stringfile)
			 (error nil))
		       (not (file-directory-p stringfile))))
	 (defdir  (if sfile-p (file-name-directory stringfile)
                    (car def)))
	 (deffile (if sfile-p (file-name-nondirectory stringfile)
                    (cdr def)))
	 (ans (read-file-name (if deffile (format "%s(default %s) "
						  prompt    deffile)
                                prompt)
			      defdir
			      (concat defdir deffile)
			      mustmatch-p)))
    (list (expand-file-name (substitute-in-file-name ans)))))

;; I am somewhat divided on this string-default feature. It seems
;; to violate the principle-of-least-astonishment, in that it makes
;; the default harder to predict, so you actually have to look and see
;; what the default really is before choosing it. This can trip you up.
;; On the other hand, it can be useful, I guess. I would appreciate feedback
;; on this.
;;     -Olin


;; Simple process query facility.
;; ===========================================================================
;; This function is for commands that want to send a query to the process
;; and show the response to the user. For example, a command to get the
;; arglist for a Common Lisp function might send a "(arglist 'foo)" query
;; to an inferior Common Lisp process.
;;
;; This simple facility just sends strings to the inferior process and pops
;; up a window for the process buffer so you can see what the process
;; responds with.  We don't do anything fancy like try to intercept what the
;; process responds with and put it in a pop-up window or on the message
;; line. We just display the buffer. Low tech. Simple. Works good.

(defun comint-proc-query (proc str)
  "Send to the inferior process PROC the string STR.
Pop-up but do not select a window for the inferior process so that
its response can be seen."
  (let* ((proc-buf (process-buffer proc))
	 (proc-mark (process-mark proc)))
    (display-buffer proc-buf)
    (set-buffer proc-buf) ; but it's not the selected *window*
    (let ((proc-win (get-buffer-window proc-buf 0))
	  (proc-pt (marker-position proc-mark)))
      (comint-send-string proc str) ; send the query
      (accept-process-output proc)  ; wait for some output
      ;; Try to position the proc window so you can see the answer.
      ;; This is bogus code. If you delete the (sit-for 0), it breaks.
      ;; I don't know why. Wizards invited to improve it.
      (unless (pos-visible-in-window-p proc-pt proc-win)
	(let ((opoint (window-point proc-win)))
	  (set-window-point proc-win proc-mark)
	  (sit-for 0)
	  (if (not (pos-visible-in-window-p opoint proc-win))
	      (push-mark opoint)
	    (set-window-point proc-win opoint)))))))


;; Filename/command/history completion in a buffer
;; ===========================================================================
;; Useful completion functions, courtesy of the Ergo group.

;; Six commands:
;; comint-dynamic-complete		Complete or expand command, filename,
;;                                     history at point.
;; comint-dynamic-complete-filename	Complete filename at point.
;; comint-dynamic-list-filename-completions List completions in help buffer.
;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;					replace with expanded/completed name.

;; These are not installed in the comint-mode keymap.  But they are
;; available for people who want them.  Shell-mode installs them:
;; (define-key shell-mode-map "\t" 'comint-dynamic-complete)
;; (define-key shell-mode-map "\M-?"
;;             'comint-dynamic-list-filename-completions)))
;;
;; Commands like this are fine things to put in load hooks if you
;; want them present in specific modes.

(defcustom comint-completion-autolist nil
  "If non-nil, automatically list possibilities on partial completion.
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'comint-completion)

(defcustom comint-completion-addsuffix t
  "If non-nil, add ` ' to file names.
It can either be a string FILESUFFIX or a cons (DIRSUFFIX . FILESUFFIX)
where DIRSUFFIX is ignored and FILESUFFIX is a string added on unambiguous
or exact completion.
This mirrors the optional behavior of tcsh."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Add SPC" t)
                 (string :tag "File suffix")
		 (cons :tag "Obsolete suffix pair"
		       (string :tag "Ignored")
		       (string :tag "File suffix")))
  :group 'comint-completion)

(defcustom comint-completion-recexact nil
  "If non-nil, use shortest completion if characters cannot be added.
This mirrors the optional behavior of tcsh.

A non-nil value is useful if `comint-completion-autolist' is non-nil too."
  :type 'boolean
  :group 'comint-completion)

(defcustom comint-completion-fignore nil
  "List of suffixes to be disregarded during file completion.
This mirrors the optional behavior of bash and tcsh.

Note that this applies to `comint-dynamic-complete-filename' only."
  :type '(repeat (string :tag "Suffix"))
  :group 'comint-completion)

;;;###autoload
(defvar comint-file-name-prefix (purecopy "")
  "Prefix prepended to absolute file names taken from process input.
This is used by Comint's and shell's completion functions, and by shell's
directory tracking functions.")

(defvar comint-file-name-chars
  (if (memq system-type '(ms-dos windows-nt cygwin))
      "~/A-Za-z0-9_^$!#%&{}@`'.,:()-"
    "[]~/A-Za-z0-9+@:_.$#%,={}-")
  "String of characters valid in a file name.
Note that all non-ASCII characters are considered valid in a file name
regardless of what this variable says.

This is a good thing to set in mode hooks.")

(defvar comint-file-name-quote-list nil
  "List of characters to quote with `\\' when in a file name.

This is a good thing to set in mode hooks.")


(defun comint-directory (directory)
  "Return expanded DIRECTORY, with `comint-file-name-prefix' if absolute."
  (expand-file-name (if (file-name-absolute-p directory)
			(concat comint-file-name-prefix directory)
		      directory)))


(defun comint-word (word-chars)
  "Return the word of WORD-CHARS at point, or nil if none is found.
Word constituents are considered to be those in WORD-CHARS, which is like the
inside of a \"[...]\" (see `skip-chars-forward'), plus all non-ASCII characters."
  (save-excursion
    (let ((here (point))
	  giveup)
      (while (not giveup)
	(let ((startpoint (point)))
	  (skip-chars-backward (concat "\\\\" word-chars))
	  ;; Fixme: This isn't consistent with Bash, at least -- not
	  ;; all non-ASCII chars should be word constituents.
	  (if (and (> (- (point) 2) (point-min))
		   (= (char-after (- (point) 2)) ?\\))
	      (forward-char -2))
	  (if (and (> (- (point) 1) (point-min))
		   (>= (char-after (- (point) 1)) 128))
	      (forward-char -1))
	  (if (= (point) startpoint)
	      (setq giveup t))))
      ;; Set match-data to match the entire string.
      (when (< (point) here)
	(set-match-data (list (point) here))
	(match-string 0)))))

(defun comint-substitute-in-file-name (filename)
  "Return FILENAME with environment variables substituted.
Supports additional environment variable syntax of the command
interpreter (e.g., the percent notation of cmd.exe on Windows)."
  (let ((name (substitute-in-file-name filename)))
    (if (memq system-type '(ms-dos windows-nt))
	(let (env-var-name
	      env-var-val)
	  (save-match-data
	    (while (string-match "%\\([^\\\\/]*\\)%" name)
	      (setq env-var-name (match-string 1 name))
	      (setq env-var-val (or (getenv env-var-name) ""))
	      (setq name (replace-match env-var-val t t name))))))
    name))

(defun comint--match-partial-filename ()
  "Return the filename at point as-is, or nil if none is found.
See `comint-word'."
  (comint-word comint-file-name-chars))

(defun comint--unquote&expand-filename (filename)
  ;; FIXME: The code below does unquote-then-expand which means that "\\$HOME"
  ;; gets expanded to the same as "$HOME"
  (comint-substitute-in-file-name
   (comint-unquote-filename filename)))

(defun comint-match-partial-filename ()
  "Return the unquoted&expanded filename at point, or nil if none is found.
Environment variables are substituted.  See `comint-word'."
  (let ((filename (comint--match-partial-filename)))
    (and filename (comint--unquote&expand-filename filename))))

(defun comint-quote-filename (filename)
  "Return FILENAME with magic characters quoted.
Magic characters are those in `comint-file-name-quote-list'."
  (if (null comint-file-name-quote-list)
      filename
    (let ((regexp
	   (format "[%s]"
                   (mapconcat 'char-to-string comint-file-name-quote-list ""))))
      (save-match-data
	(let ((i 0))
	  (while (string-match regexp filename i)
	    (setq filename (replace-match "\\\\\\&" nil nil filename))
	    (setq i (1+ (match-end 0)))))
	filename))))

(defun comint-unquote-filename (filename)
  "Return FILENAME with quoted characters unquoted."
  (if (null comint-file-name-quote-list)
      filename
    (save-match-data
      (replace-regexp-in-string "\\\\\\(.\\)" "\\1" filename t))))

(defun comint-completion-at-point ()
  (run-hook-with-args-until-success 'comint-dynamic-complete-functions))

(define-obsolete-function-alias
  'comint-dynamic-complete
  'completion-at-point "24.1")

(defun comint-dynamic-complete-filename ()
  "Dynamically complete the filename at point.
Completes if after a filename.
This function is similar to `comint-replace-by-expanded-filename', except that
it won't change parts of the filename already entered in the buffer; it just
adds completion characters to the end of the filename.  A completions listing
may be shown in a separate buffer if completion is ambiguous.

Completion is dependent on the value of `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore', and the timing of
completions listing is dependent on the value of `comint-completion-autolist'.

Returns t if successful."
  (interactive)
  (when (comint--match-partial-filename)
    (unless (window-minibuffer-p (selected-window))
      (message "Completing file name..."))
    (let ((data (comint--complete-file-name-data)))
      (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)))))

(defun comint-filename-completion ()
  "Return completion data for filename at point, if any."
  (when (comint--match-partial-filename)
    (comint--complete-file-name-data)))

;; FIXME: comint--common-suffix, comint--common-quoted-suffix, and
;; comint--table-subvert don't fully solve the problem, since
;; selecting a file from *Completions* won't quote it, among several
;; other problems.

(defun comint--common-suffix (s1 s2)
  (assert (not (or (string-match "\n" s1) (string-match "\n" s2))))
  ;; Since S2 is expected to be the "unquoted/expanded" version of S1,
  ;; there shouldn't be any case difference, even if the completion is
  ;; case-insensitive.
  (let ((case-fold-search nil))
    (string-match ".*?\\(.*\\)\n.*\\1\\'" (concat s1 "\n" s2))
    (- (match-end 1) (match-beginning 1))))

(defun comint--common-quoted-suffix (s1 s2)
  ;; FIXME: Copied in pcomplete.el.
  "Find the common suffix between S1 and S2 where S1 is the expanded S2.
S1 is expected to be the unquoted and expanded version of S2.
Returns (PS1 . PS2), i.e. the shortest prefixes of S1 and S2, such that
S1 = (concat PS1 SS1) and S2 = (concat PS2 SS2) and
SS1 = (unquote SS2)."
  (let* ((cs (comint--common-suffix s1 s2))
         (ss1 (substring s1 (- (length s1) cs)))
         (qss1 (comint-quote-filename ss1))
         qc s2b)
    (if (and (not (equal ss1 qss1))
             (setq qc (comint-quote-filename (substring ss1 0 1)))
	     (setq s2b (- (length s2) cs (length qc) -1))
	     (>= s2b 0)			;bug#11158.
             (eq t (compare-strings s2 s2b (- (length s2) cs -1)
                                    qc nil nil)))
        ;; The difference found is just that one char is quoted in S2
        ;; but not in S1, keep looking before this difference.
        (comint--common-quoted-suffix
         (substring s1 0 (- (length s1) cs))
         (substring s2 0 s2b))
      (cons (substring s1 0 (- (length s1) cs))
            (substring s2 0 (- (length s2) cs))))))

(defun comint--table-subvert (table s1 s2 &optional quote-fun unquote-fun)
  "Completion table that replaces the prefix S1 with S2 in STRING.
The result is a completion table which completes strings of the
form (concat S1 S) in the same way as TABLE completes strings of
the form (concat S2 S)."
  (lambda (string pred action)
    (let* ((str (if (eq t (compare-strings string 0 (length s1) s1 nil nil
                                           completion-ignore-case))
                    (let ((rest (substring string (length s1))))
                      (concat s2 (if unquote-fun
                                     (funcall unquote-fun rest) rest)))))
           (res (if str (complete-with-action action table str pred))))
      (when res
        (cond
         ((and (eq (car-safe action) 'boundaries))
          (let ((beg (or (and (eq (car-safe res) 'boundaries) (cadr res)) 0)))
            (list* 'boundaries
                   (max (length s1)
                        ;; FIXME: Adjust because of quoting/unquoting.
                        (+ beg (- (length s1) (length s2))))
                   (and (eq (car-safe res) 'boundaries) (cddr res)))))
         ((stringp res)
          (if (eq t (compare-strings res 0 (length s2) s2 nil nil
                                     completion-ignore-case))
              (let ((rest (substring res (length s2))))
                (concat s1 (if quote-fun (funcall quote-fun rest) rest)))))
         ((eq action t)
          (let ((bounds (completion-boundaries str table pred "")))
            (if (>= (car bounds) (length s2))
                (if quote-fun (mapcar quote-fun res) res)
              (let ((re (concat "\\`"
                                (regexp-quote (substring s2 (car bounds))))))
                (delq nil
                      (mapcar (lambda (c)
                                (if (string-match re c)
                                    (let ((str (substring c (match-end 0))))
                                      (if quote-fun
                                          (funcall quote-fun str) str))))
                              res))))))
         ;; E.g. action=nil and it's the only completion.
         (res))))))

(defun comint-completion-file-name-table (string pred action)
  (if (not (file-name-absolute-p string))
      (completion-file-name-table string pred action)
    (cond
     ((memq action '(t lambda))
      (completion-file-name-table
       (concat comint-file-name-prefix string) pred action))
     ((null action)
      (let ((res (completion-file-name-table
                  (concat comint-file-name-prefix string) pred action)))
        (if (and (stringp res)
                 (string-match
                  (concat "\\`" (regexp-quote comint-file-name-prefix))
                  res))
            (substring res (match-end 0))
          res)))
     (t (completion-file-name-table string pred action)))))

(defun comint--complete-file-name-data ()
  "Return the completion data for file name at point."
  (let* ((filesuffix (cond ((not comint-completion-addsuffix) "")
			   ((stringp comint-completion-addsuffix)
                            comint-completion-addsuffix)
			   ((not (consp comint-completion-addsuffix)) " ")
			   (t (cdr comint-completion-addsuffix))))
	 (filename (comint--match-partial-filename))
	 (filename-beg (if filename (match-beginning 0) (point)))
	 (filename-end (if filename (match-end 0) (point)))
         (unquoted (if filename (comint--unquote&expand-filename filename) ""))
         (table
          (let ((prefixes (comint--common-quoted-suffix
                           unquoted filename)))
            (comint--table-subvert
             #'comint-completion-file-name-table
             (cdr prefixes) (car prefixes)
             #'comint-quote-filename #'comint-unquote-filename))))
    (nconc
     (list
      filename-beg filename-end
      (lambda (string pred action)
        (let ((completion-ignore-case read-file-name-completion-ignore-case)
              (completion-ignored-extensions comint-completion-fignore))
          (complete-with-action action table string pred))))
     (unless (zerop (length filesuffix))
       (list :exit-function
             (lambda (_s finished)
               (when (memq finished '(sole finished))
                 (if (looking-at (regexp-quote filesuffix))
                     (goto-char (match-end 0))
                   (insert filesuffix)))))))))

(defun comint-dynamic-complete-as-filename ()
  "Dynamically complete at point as a filename.
See `comint-dynamic-complete-filename'.  Returns t if successful."
  (let ((data (comint--complete-file-name-data)))
    (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data))))
(make-obsolete 'comint-dynamic-complete-as-filename
               'comint-filename-completion "24.1")

(defun comint-replace-by-expanded-filename ()
  "Dynamically expand and complete the filename at point.
Replace the filename with an expanded, canonicalized and
completed replacement, i.e. substituting environment
variables (e.g. $HOME), `~'s, `..', and `.', and making the
filename absolute.  For expansion see `expand-file-name' and
`substitute-in-file-name'.  For completion see
`comint-dynamic-complete-filename'."
  (interactive)
  (let ((filename (comint-match-partial-filename)))
    (when filename
      (replace-match (expand-file-name filename) t t)
      (comint-dynamic-complete-filename))))


(defun comint-dynamic-simple-complete (stub candidates)
  "Dynamically complete STUB from CANDIDATES list.
This function inserts completion characters at point by
completing STUB from the strings in CANDIDATES.  If completion is
ambiguous, possibly show a completions listing in a separate
buffer.

Return nil if no completion was inserted.
Return `sole' if completed with the only completion match.
Return `shortest' if completed with the shortest match.
Return `partial' if completed as far as possible.
Return `listed' if a completion listing was shown.

See also `comint-dynamic-complete-filename'."
  (let* ((completion-ignore-case (memq system-type '(ms-dos windows-nt cygwin)))
	 (minibuffer-p (window-minibuffer-p (selected-window)))
	 (suffix (cond ((not comint-completion-addsuffix) "")
		       ((not (consp comint-completion-addsuffix)) " ")
		       (t (cdr comint-completion-addsuffix))))
	 (completions (all-completions stub candidates)))
    (cond ((null completions)
	   (if minibuffer-p
	       (minibuffer-message "No completions of %s" stub)
	     (message "No completions of %s" stub))
	   nil)
	  ((= 1 (length completions))	; Gotcha!
	   (let ((completion (car completions)))
	     (if (string-equal completion stub)
		 (unless minibuffer-p
		   (message "Sole completion"))
	       (insert (substring completion (length stub)))
	       (unless minibuffer-p
		 (message "Completed")))
	     (insert suffix)
	     'sole))
	  (t				; There's no unique completion.
	   (let ((completion (try-completion stub candidates)))
	     ;; Insert the longest substring.
	     (insert (substring completion (length stub)))
	     (cond ((and comint-completion-recexact comint-completion-addsuffix
			 (string-equal stub completion)
			 (member completion completions))
		    ;; It's not unique, but user wants shortest match.
		    (insert suffix)
		    (unless minibuffer-p
		      (message "Completed shortest"))
		    'shortest)
		   ((or comint-completion-autolist
			(string-equal stub completion))
		    ;; It's not unique, list possible completions.
		    (comint-dynamic-list-completions completions stub)
		    'listed)
		   (t
		    (unless minibuffer-p
		      (message "Partially completed"))
		    'partial)))))))
(make-obsolete 'comint-dynamic-simple-complete 'completion-in-region "24.1")


(defun comint-dynamic-list-filename-completions ()
  "Display a list of possible completions for the filename at point."
  (interactive)
  (let* ((data (comint--complete-file-name-data))
         (minibuffer-completion-table (nth 2 data))
         (minibuffer-completion-predicate nil)
         (ol (make-overlay (nth 0 data) (nth 1 data) nil nil t)))
    (overlay-put ol 'field 'completion)
    (unwind-protect
        (call-interactively 'minibuffer-completion-help)
      (delete-overlay ol))))


;; This is bound locally in a *Completions* buffer to the list of
;; completions displayed, and is used to detect the case where the same
;; command is repeatedly used without the set of completions changing.
(defvar comint-displayed-dynamic-completions nil)

(defvar comint-dynamic-list-completions-config nil)

(defun comint-dynamic-list-completions (completions &optional common-substring)
  "Display a list of sorted COMPLETIONS.
The meaning of COMMON-SUBSTRING is the same as in `display-completion-list'.
Typing SPC flushes the completions buffer."
  (let ((window (get-buffer-window "*Completions*" 0)))
    (setq completions (sort completions 'string-lessp))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window))
	     ;; The above tests are not sufficient to detect the case where we
	     ;; should scroll, because the top-level interactive command may
	     ;; not have displayed a completions window the last time it was
	     ;; invoked, and there may be such a window left over from a
	     ;; previous completion command with a different set of
	     ;; completions.  To detect that case, we also test that the set
	     ;; of displayed completions is in fact the same as the previously
	     ;; displayed set.
	     (equal completions
		    (buffer-local-value 'comint-displayed-dynamic-completions
					(window-buffer window))))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))

      ;; Display a completion list for the first time.
      (setq comint-dynamic-list-completions-config
	    (current-window-configuration))
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list completions common-substring))
      (if (window-minibuffer-p (selected-window))
	  (minibuffer-message "Type space to flush; repeat completion command to scroll")
	(message "Type space to flush; repeat completion command to scroll")))

    ;; Read the next key, to process SPC.
    (let (key first)
      (if (with-current-buffer (get-buffer "*Completions*")
	    (set (make-local-variable 'comint-displayed-dynamic-completions)
		 completions)
	    (setq key (read-key-sequence nil)
		  first (aref key 0))
	    (and (consp first) (consp (event-start first))
		 (eq (window-buffer (posn-window (event-start first)))
		     (get-buffer "*Completions*"))
		 (eq (key-binding key) 'mouse-choose-completion)))
	  ;; If the user does mouse-choose-completion with the mouse,
	  ;; execute the command, then delete the completion window.
	  (progn
	    (choose-completion first)
	    (set-window-configuration comint-dynamic-list-completions-config))
	(if (eq first ?\s)
	    (set-window-configuration comint-dynamic-list-completions-config)
	  (setq unread-command-events (listify-key-sequence key)))))))

(defun comint-get-next-from-history ()
  "After fetching a line from input history, this fetches the following line.
In other words, this recalls the input line after the line you recalled last.
You can use this to repeat a sequence of input lines."
  (interactive)
  (if comint-save-input-ring-index
      (progn
	(setq comint-input-ring-index (1+ comint-save-input-ring-index))
	(comint-next-input 1))
    (message "No previous history command")))

(defun comint-accumulate ()
  "Accumulate a line to send as input along with more lines.
This inserts a newline so that you can enter more text
to be sent along with this line.  Use \\[comint-send-input]
to send all the accumulated input, at once.
The entire accumulated text becomes one item in the input history
when you send it."
  (interactive)
  (insert "\n")
  (set-marker comint-accum-marker (point))
  (if comint-input-ring-index
      (setq comint-save-input-ring-index
	    (- comint-input-ring-index 1))))

(defun comint-goto-process-mark ()
  "Move point to the process mark.
The process mark separates output, and input already sent,
from input that has not yet been sent."
  (interactive)
  (let ((proc (or (get-buffer-process (current-buffer))
		  (error "Current buffer has no process"))))
    (goto-char (process-mark proc))
    (when (called-interactively-p 'interactive)
      (message "Point is now at the process mark"))))

(defun comint-bol-or-process-mark ()
  "Move point to beginning of line (after prompt) or to the process mark.
The first time you use this command, it moves to the beginning of the line
\(but after the prompt, if any).  If you repeat it again immediately,
it moves point to the process mark.

The process mark separates the process output, along with input already sent,
from input that has not yet been sent.  Ordinarily, the process mark
is at the beginning of the current input line; but if you have
used \\[comint-accumulate] to send multiple lines at once,
the process mark is at the beginning of the accumulated input."
  (interactive)
  (if (not (eq last-command 'comint-bol-or-process-mark))
      (comint-bol nil)
    (comint-goto-process-mark)))

(defun comint-set-process-mark ()
  "Set the process mark at point."
  (interactive)
  (let ((proc (or (get-buffer-process (current-buffer))
		  (error "Current buffer has no process"))))
    (set-marker (process-mark proc) (point))
    (message "Process mark set")))


;; Author:  Peter Breton <pbreton@cs.umb.edu>

;; This little add-on for comint is intended to make it easy to get
;; output from currently active comint buffers into another buffer,
;; or buffers, and then go back to using the comint shell.
;;
;; My particular use is SQL interpreters; I want to be able to execute a
;; query using the process associated with a comint-buffer, and save that
;; somewhere else.  Because the process might have state (for example, it
;; could be in an uncommitted transaction), just running starting a new
;; process and having it execute the query and then finish, would not
;; work.  I'm sure there are other uses as well, although in many cases
;; starting a new process is the simpler, and thus preferable, approach.
;;
;; The basic implementation is as follows: comint-redirect changes the
;; preoutput filter functions (`comint-preoutput-filter-functions') to use
;; its own filter.  The filter puts the output into the designated buffer,
;; or buffers, until it sees a regexp that tells it to stop (by default,
;; this is the prompt for the interpreter, `comint-prompt-regexp'). When it
;; sees the stop regexp, it restores the old filter functions, and runs
;; `comint-redirect-hook'.
;;
;; Each comint buffer may only use one redirection at a time, but any number
;; of different comint buffers may be simultaneously redirected.
;;
;; NOTE: It is EXTREMELY important that `comint-prompt-regexp' be set to the
;; correct prompt for your interpreter, or that you supply a regexp that says
;; when the redirection is finished.  Otherwise, redirection will continue
;; indefinitely.  The code now does a sanity check to ensure that it can find
;; a prompt in the comint buffer; however, it is still important to ensure that
;; this prompt is set correctly.
;;
;; XXX: This doesn't work so well unless `comint-prompt-regexp' is set;
;; perhaps it should prompt for a terminating string (with an
;; appropriate magic default by examining what we think is the prompt)?
;;
;; Fixme: look for appropriate fields, rather than regexp, if
;; `comint-use-prompt-regexp' is true.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom comint-redirect-verbose nil
  "If non-nil, print messages each time the redirection filter is invoked.
Also print a message when redirection is completed."
  :group 'comint
  :type 'boolean)

;; Directly analogous to comint-preoutput-filter-functions
(defvar comint-redirect-filter-functions nil
  "List of functions to call before inserting redirected process output.
Each function gets one argument, a string containing the text received
from the subprocess.  It should return the string to insert, perhaps
the same string that was received, or perhaps a modified or transformed
string.

The functions on the list are called sequentially, and each one is given
the string returned by the previous one.  The string returned by the
last function is the text that is actually inserted in the redirection buffer.

You can use `add-hook' to add functions to this list
either globally or locally.")

;; Internal variables

(defvar comint-redirect-output-buffer nil
  "The buffer or list of buffers to put output into.")

(defvar comint-redirect-finished-regexp nil
  "Regular expression that determines when to stop redirection in Comint.
When the redirection filter function is given output that matches this regexp,
the output is inserted as usual, and redirection is completed.")

(defvar comint-redirect-insert-matching-regexp nil
  "If non-nil, the text that ends a redirection is included in it.
More precisely, the text that matches `comint-redirect-finished-regexp'
and therefore terminates an output redirection is inserted in the
redirection target buffer, along with the preceding output.")

(defvar comint-redirect-echo-input nil
  "Non-nil means echo input in the process buffer even during redirection.")

(defvar comint-redirect-completed nil
  "Non-nil if redirection has completed in the current buffer.")

(defvar comint-redirect-original-mode-line-process nil
  "Original mode line for redirected process.")

(defvar comint-redirect-perform-sanity-check t
  "If non-nil, check that redirection is likely to complete successfully.
More precisely, before starting a redirection, verify that the
regular expression `comint-redirect-finished-regexp' that controls
when to terminate it actually matches some text already in the process
buffer.  The idea is that this regular expression should match a prompt
string, and that there ought to be at least one copy of your prompt string
in the process buffer already.")

(defvar comint-redirect-original-filter-function nil
  "The process filter that was in place when redirection is started.
When redirection is completed, the process filter is restored to
this value.")

(defvar comint-redirect-subvert-readonly nil
  "Non-nil means `comint-redirect' can insert into read-only buffers.
This works by binding `inhibit-read-only' around the insertion.
This is useful, for instance, for insertion into Help mode buffers.
You probably want to set it locally to the output buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comint-redirect-setup (output-buffer
			      comint-buffer
			      finished-regexp
			      &optional echo-input)
  "Set up for output redirection.
This function sets local variables that are used by `comint-redirect-filter'
to perform redirection.

Output from COMINT-BUFFER is redirected to OUTPUT-BUFFER, until something
in the output matches FINISHED-REGEXP.

If optional argument ECHO-INPUT is non-nil, output is echoed to the
original Comint buffer.

This function is called by `comint-redirect-send-command-to-process',
and does not normally need to be invoked by the end user or programmer."
  (with-current-buffer comint-buffer

    (make-local-variable 'comint-redirect-original-mode-line-process)
    (setq comint-redirect-original-mode-line-process mode-line-process)

    (make-local-variable 'comint-redirect-output-buffer)
    (setq comint-redirect-output-buffer output-buffer)

    (make-local-variable 'comint-redirect-finished-regexp)
    (setq comint-redirect-finished-regexp finished-regexp)

    (make-local-variable 'comint-redirect-echo-input)
    (setq comint-redirect-echo-input echo-input)

    (make-local-variable 'comint-redirect-completed)
    (setq comint-redirect-completed nil)

    (setq mode-line-process
	  (if mode-line-process
	      (list (concat (elt mode-line-process 0) " Redirection"))
	    (list ":%s Redirection")))))

(defun comint-redirect-cleanup ()
  "End a Comint redirection.  See `comint-redirect-send-command'."
  (interactive)
  ;; Restore the process filter
  (set-process-filter (get-buffer-process (current-buffer))
		      comint-redirect-original-filter-function)
  ;; Restore the mode line
  (setq mode-line-process comint-redirect-original-mode-line-process)
  ;; Set the completed flag
  (setq comint-redirect-completed t))

;; Because the cleanup happens as a callback, it's not easy to guarantee
;; that it really occurs.
(defalias 'comint-redirect-remove-redirection 'comint-redirect-cleanup)

(defun comint-redirect-filter (process input-string)
  "Filter function which redirects output from PROCESS to a buffer or buffers.
The variable `comint-redirect-output-buffer' says which buffer(s) to
place output in.

INPUT-STRING is the input from the Comint process.

This function runs as a process filter, and does not need to be invoked by the
end user."
  (and process
       (with-current-buffer (process-buffer process)
	 (comint-redirect-preoutput-filter input-string)
	 ;; If we have to echo output, give it to the original filter function
	 (and comint-redirect-echo-input
	      comint-redirect-original-filter-function
	      (funcall comint-redirect-original-filter-function
		       process input-string)))))


(defun comint-redirect-preoutput-filter (input-string)
  "Comint filter function which redirects Comint output to a buffer or buffers.
The variable `comint-redirect-output-buffer' says which buffer(s) to
place output in.

INPUT-STRING is the input from the Comint process.

This function does not need to be invoked by the end user."
  (let ((output-buffer-list
	 (if (listp comint-redirect-output-buffer)
             comint-redirect-output-buffer
	   (list comint-redirect-output-buffer)))
	(filtered-input-string input-string))

    ;; If there are any filter functions, give them a chance to modify
    ;; the string.
    (let ((functions comint-redirect-filter-functions))
      (while (and functions filtered-input-string)
	(if (eq (car functions) t)
	    ;; If a local value says "use the default value too",
	    ;; do that.
	    (let ((functions
                   (default-value 'comint-redirect-filter-functions)))
	      (while (and functions filtered-input-string)
		(setq filtered-input-string
		      (funcall (car functions) filtered-input-string))
		(setq functions (cdr functions))))
	  (setq filtered-input-string
		(funcall (car functions) filtered-input-string)))
	(setq functions (cdr functions))))

    ;; Clobber `comint-redirect-finished-regexp'
    (or comint-redirect-insert-matching-regexp
	(and (string-match comint-redirect-finished-regexp filtered-input-string)
	     (setq filtered-input-string
		   (replace-match "" nil nil filtered-input-string))))

    ;; Send output to all registered buffers
    (save-excursion
      (dolist (buf output-buffer-list)
	;; Set this buffer to the output buffer
	(set-buffer (get-buffer-create buf))
	;; Go to the end of the buffer
	(goto-char (point-max))
	;; Insert the output
	(let ((inhibit-read-only comint-redirect-subvert-readonly))
	  (insert filtered-input-string))))

    ;; Message
    (and comint-redirect-verbose
	 (message "Redirected output to buffer(s) %s"
		  (mapconcat 'identity output-buffer-list " ")))

    ;; If we see the prompt, tidy up
    ;; We'll look for the prompt in the original string, so nobody can
    ;; clobber it
    (and (string-match comint-redirect-finished-regexp input-string)
	 (progn
	   (and comint-redirect-verbose
		(message "Redirection completed"))
	   (comint-redirect-cleanup)
	   (run-hooks 'comint-redirect-hook)))
    ;; Echo input?
    (if comint-redirect-echo-input
	filtered-input-string
      "")))

;;;###autoload
(defun comint-redirect-send-command (command output-buffer echo &optional no-display)
  "Send COMMAND to process in current buffer, with output to OUTPUT-BUFFER.
With prefix arg ECHO, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer."
  (interactive "sCommand: \nBOutput Buffer: \nP")
  (let ((process (get-buffer-process (current-buffer))))
    (if process
	(comint-redirect-send-command-to-process
	 command output-buffer (current-buffer) echo no-display)
      (error "No process for current buffer"))))

;;;###autoload
(defun comint-redirect-send-command-to-process
  (command output-buffer process echo &optional no-display)
  "Send COMMAND to PROCESS, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer."
  (interactive "sCommand: \nBOutput Buffer: \nbProcess Buffer: \nP")
  (let* (;; The process buffer
	 (process-buffer (if (processp process)
			     (process-buffer process)
			   process))
	 (proc (get-buffer-process process-buffer)))
    ;; Change to the process buffer
    (with-current-buffer process-buffer

      ;; Make sure there's a prompt in the current process buffer
      (and comint-redirect-perform-sanity-check
	   (save-excursion
	     (goto-char (point-max))
	     (or (re-search-backward comint-prompt-regexp nil t)
		 (error "No prompt found or `comint-prompt-regexp' not set properly"))))

      ;; Set up for redirection
      (comint-redirect-setup
       output-buffer
       (current-buffer)                 ; Comint Buffer
       comint-prompt-regexp             ; Finished Regexp
       echo)                            ; Echo input

      ;; Set the filter
      (setq comint-redirect-original-filter-function ; Save the old filter
	    (process-filter proc))
      (set-process-filter proc 'comint-redirect-filter)

      ;; Send the command
      (process-send-string (current-buffer) (concat command "\n"))

      ;; Show the output
      (or no-display
	  (display-buffer
	   (get-buffer-create
	    (if (listp output-buffer)
		(car output-buffer)
	      output-buffer)))))))

;;;###autoload
(defun comint-redirect-results-list (command regexp regexp-group)
  "Send COMMAND to current process.
Return a list of expressions in the output which match REGEXP.
REGEXP-GROUP is the regular expression group in REGEXP to use."
  (comint-redirect-results-list-from-process
   (get-buffer-process (current-buffer))
   command regexp regexp-group))

;;;###autoload
(defun comint-redirect-results-list-from-process (process command regexp regexp-group)
  "Send COMMAND to PROCESS.
Return a list of expressions in the output which match REGEXP.
REGEXP-GROUP is the regular expression group in REGEXP to use."
  (let ((output-buffer " *Comint Redirect Work Buffer*")
	results)
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process command
					       output-buffer process nil t)
      ;; Wait for the process to complete
      (set-buffer (process-buffer process))
      (while (null comint-redirect-completed)
	(accept-process-output nil 1))
      ;; Collect the output
      (set-buffer output-buffer)
      (goto-char (point-min))
      ;; Skip past the command, if it was echoed
      (and (looking-at command)
	   (forward-line))
      (while (re-search-forward regexp nil t)
	(push (buffer-substring-no-properties
               (match-beginning regexp-group)
               (match-end regexp-group))
              results))
      results)))

(dolist (x '("^Not at command line$"
             "^Empty input ring$"
             "^No history$"
             "^Not found$"			; Too common?
             "^Current buffer has no process$"))
  (add-to-list 'debug-ignored-errors x))


;; Converting process modes to use comint mode
;; ===========================================================================
;; The code in the Emacs 19 distribution has all been modified to use comint
;; where needed.  However, there are `third-party' packages out there that
;; still use the old shell mode.  Here's a guide to conversion.
;;
;; Renaming variables
;; Most of the work is renaming variables and functions. These are the common
;; ones:
;; Local variables:
;;	last-input-start	comint-last-input-start
;;	last-input-end		comint-last-input-end
;;	shell-prompt-pattern	comint-prompt-regexp
;;     shell-set-directory-error-hook <no equivalent>
;; Miscellaneous:
;;	shell-set-directory	<unnecessary>
;;	shell-mode-map		comint-mode-map
;; Commands:
;;	shell-send-input	comint-send-input
;;	shell-send-eof		comint-delchar-or-maybe-eof
;;	kill-shell-input	comint-kill-input
;;	interrupt-shell-subjob	comint-interrupt-subjob
;;	stop-shell-subjob	comint-stop-subjob
;;	quit-shell-subjob	comint-quit-subjob
;;	kill-shell-subjob	comint-kill-subjob
;;	kill-output-from-shell	comint-delete-output
;;	show-output-from-shell	comint-show-output
;;	copy-last-shell-input	Use comint-previous-input/comint-next-input
;;
;; SHELL-SET-DIRECTORY is gone, its functionality taken over by
;; SHELL-DIRECTORY-TRACKER, the shell mode's comint-input-filter-functions.
;; Comint mode does not provide functionality equivalent to
;; shell-set-directory-error-hook; it is gone.
;;
;; comint-last-input-start is provided for modes which want to munge
;; the buffer after input is sent, perhaps because the inferior
;; insists on echoing the input.  The LAST-INPUT-START variable in
;; the old shell package was used to implement a history mechanism,
;; but you should think twice before using comint-last-input-start
;; for this; the input history ring often does the job better.
;;
;; If you are implementing some process-in-a-buffer mode, called foo-mode, do
;; *not* create the comint-mode local variables in your foo-mode function.
;; This is not modular.  Instead, call comint-mode, and let *it* create the
;; necessary comint-specific local variables. Then create the
;; foo-mode-specific local variables in foo-mode.  Set the buffer's keymap to
;; be foo-mode-map, and its mode to be foo-mode.  Set the comint-mode hooks
;; (comint-{prompt-regexp, input-filter, input-filter-functions,
;; get-old-input) that need to be different from the defaults.  Call
;; foo-mode-hook, and you're done. Don't run the comint-mode hook yourself;
;; comint-mode will take care of it. The following example, from shell.el,
;; is typical:
;;
;; (defvar shell-mode-map '())
;; (cond ((not shell-mode-map)
;;        (setq shell-mode-map (copy-keymap comint-mode-map))
;;        (define-key shell-mode-map "\C-c\C-f" 'shell-forward-command)
;;        (define-key shell-mode-map "\C-c\C-b" 'shell-backward-command)
;;        (define-key shell-mode-map "\t" 'comint-dynamic-complete)
;;        (define-key shell-mode-map "\M-?"
;;          'comint-dynamic-list-filename-completions)))
;;
;; (defun shell-mode ()
;;   (interactive)
;;   (comint-mode)
;;   (setq comint-prompt-regexp shell-prompt-pattern)
;;   (setq major-mode 'shell-mode)
;;   (setq mode-name "Shell")
;;   (use-local-map shell-mode-map)
;;   (make-local-variable 'shell-directory-stack)
;;   (setq shell-directory-stack nil)
;;   (add-hook 'comint-input-filter-functions 'shell-directory-tracker)
;;   (run-mode-hooks 'shell-mode-hook))
;;
;;
;; Completion for comint-mode users
;;
;; For modes that use comint-mode, comint-dynamic-complete-functions is the
;; hook to add completion functions to.  Functions on this list should return
;; the completion data according to the documentation of
;; `completion-at-point-functions'


(provide 'comint)

;;; comint.el ends here
