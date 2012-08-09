;;; esh-mode.el --- user interface

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Basically, Eshell is used just like shell mode (<M-x shell>).  The
;; keystrokes for navigating the buffer, and accessing the command
;; history, are identical.  Unlike shell mode, however, Eshell mode's
;; governing process is Emacs itself.  With shell mode, an inferior
;; shell process is executed that communicates with Emacs via comint
;; -- a mode for handling sub-process interaction.  Eshell mode, on
;; the other hand, is a truly native Emacs shell.  No subprocess are
;; invoked except the ones requested by the user at the prompt.
;;
;; After entering a command, use <RET> to invoke it ([Command
;; invocation]) .  If there is a command on disk, it will be executed
;; as in a normal shell.  If there is no command by that name on disk,
;; but a Lisp function with that name is defined, the Lisp function
;; will be called, using the arguments passed on the command line.
;;
;; Some of the other features of the command interaction mode are:
;;
;; @ <M-RET> can be used to accumulate further commands while a
;;   command is currently running.  Since all input is passed to the
;;   subprocess being executed, there is no automatic input queueing
;;   as there is with other shells.
;;
;; @ <C-c C-t> can be used to truncate the buffer if it grows too
;;   large.
;;
;; @ <C-c C-r> will move point to the beginning of the output of the
;;   last command.  With a prefix argument, it will narrow to view
;;   only that output.
;;
;; @ <C-c C-o> will delete the output from the last command.
;;
;; @ <C-c C-f> will move forward a complete shell argument.
;;
;; @ <C-c C-b> will move backward a complete shell argument.

;;; Code:

(provide 'esh-mode)

(eval-when-compile (require 'esh-util))
(require 'esh-module)
(require 'esh-cmd)
(require 'esh-io)
(require 'esh-var)

(defgroup eshell-mode nil
  "This module contains code for handling input from the user."
  :tag "User interface"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-mode-unload-hook nil
  "A hook that gets run when `eshell-mode' is unloaded."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-mode-hook nil
  "A hook that gets run when `eshell-mode' is entered."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-first-time-mode-hook nil
  "A hook that gets run the first time `eshell-mode' is entered.
That is to say, the first time during an Emacs session."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-exit-hook nil
  "A hook that is run whenever `eshell' is exited.
This hook is only run if exiting actually kills the buffer."
  :version "24.1"                       ; removed eshell-query-kill-processes
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-kill-on-exit t
  "If non-nil, kill the Eshell buffer on the `exit' command.
Otherwise, the buffer will simply be buried."
  :type 'boolean
  :group 'eshell-mode)

(defcustom eshell-input-filter-functions nil
  "Functions to call before input is processed.
The input is contained in the region from `eshell-last-input-start' to
`eshell-last-input-end'."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-send-direct-to-subprocesses nil
  "If t, send any input immediately to a subprocess."
  :type 'boolean
  :group 'eshell-mode)

(defcustom eshell-expand-input-functions nil
  "Functions to call before input is parsed.
Each function is passed two arguments, which bounds the region of the
current input text."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-scroll-to-bottom-on-input nil
  "Controls whether input to interpreter causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing
buffer.  If `this', scroll only the selected window.

See `eshell-preinput-scroll-to-bottom'."
  :type '(radio (const :tag "Do not scroll Eshell windows" nil)
		(const :tag "Scroll all windows showing the buffer" all)
		(const :tag "Scroll only the selected window" this))
  :group 'eshell-mode)

(defcustom eshell-scroll-to-bottom-on-output nil
  "Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing
buffer.  If `this', scroll only the selected window.  If `others',
scroll only those that are not the selected window.

See variable `eshell-scroll-show-maximum-output' and function
`eshell-postoutput-scroll-to-bottom'."
  :type '(radio (const :tag "Do not scroll Eshell windows" nil)
		(const :tag "Scroll all windows showing the buffer" all)
		(const :tag "Scroll only the selected window" this)
		(const :tag "Scroll all windows other than selected" this))
  :group 'eshell-mode)

(defcustom eshell-scroll-show-maximum-output t
  "Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `eshell-scroll-to-bottom-on-output' and function
`eshell-postoutput-scroll-to-bottom'."
  :type 'boolean
  :group 'eshell-mode)

(defcustom eshell-buffer-maximum-lines 1024
  "The maximum size in lines for eshell buffers.
Eshell buffers are truncated from the top to be no greater than this
number, if the function `eshell-truncate-buffer' is on
`eshell-output-filter-functions'."
  :type 'integer
  :group 'eshell-mode)

(defcustom eshell-output-filter-functions
  '(eshell-postoutput-scroll-to-bottom
    eshell-handle-control-codes
    eshell-handle-ansi-color
    eshell-watch-for-password-prompt)
  "Functions to call before output is displayed.
These functions are only called for output that is displayed
interactively, and not for output which is redirected."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-preoutput-filter-functions nil
  "Functions to call before output is inserted into the buffer.
These functions get one argument, a string containing the text to be
inserted.  They return the string as it should be inserted."
  :type 'hook
  :group 'eshell-mode)

(defcustom eshell-password-prompt-regexp
  "[Pp]ass\\(word\\|phrase\\).*:\\s *\\'"
  "Regexp matching prompts for passwords in the inferior process.
This is used by `eshell-watch-for-password-prompt'."
  :type 'regexp
  :group 'eshell-mode)

(defcustom eshell-skip-prompt-function nil
  "A function called from beginning of line to skip the prompt."
  :type '(choice (const nil) function)
  :group 'eshell-mode)

(defcustom eshell-status-in-modeline t
  "If non-nil, let the user know a command is running in the modeline."
  :type 'boolean
  :group 'eshell-mode)

(defvar eshell-first-time-p t
  "A variable which is non-nil the first time Eshell is loaded.")

;; Internal Variables:

;; these are only set to `nil' initially for the sake of the
;; byte-compiler, when compiling other files which `require' this one
(defvar eshell-mode nil)
(defvar eshell-mode-map nil)
(defvar eshell-command-running-string "--")
(defvar eshell-command-map nil)
(defvar eshell-command-prefix nil)
(defvar eshell-last-input-start nil)
(defvar eshell-last-input-end nil)
(defvar eshell-last-output-start nil)
(defvar eshell-last-output-block-begin nil)
(defvar eshell-last-output-end nil)

(defvar eshell-currently-handling-window nil)
(defvar eshell-mode-syntax-table nil)
(defvar eshell-mode-abbrev-table nil)

(define-abbrev-table 'eshell-mode-abbrev-table ())

(if (not eshell-mode-syntax-table)
    (let ((i 0))
      (setq eshell-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
	(modify-syntax-entry i "_   " eshell-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   " eshell-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " eshell-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   " eshell-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " eshell-mode-syntax-table)
      (modify-syntax-entry ?\t "    " eshell-mode-syntax-table)
      (modify-syntax-entry ?\f "    " eshell-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " eshell-mode-syntax-table)
      ;; Give CR the same syntax as newline, for selective-display.
      (modify-syntax-entry ?\^m ">   " eshell-mode-syntax-table)
;;;   (modify-syntax-entry ?\; "<   " eshell-mode-syntax-table)
      (modify-syntax-entry ?` "'   " eshell-mode-syntax-table)
      (modify-syntax-entry ?' "'   " eshell-mode-syntax-table)
      (modify-syntax-entry ?, "'   " eshell-mode-syntax-table)
      ;; Used to be singlequote; changed for flonums.
      (modify-syntax-entry ?. "_   " eshell-mode-syntax-table)
      (modify-syntax-entry ?- "_   " eshell-mode-syntax-table)
      (modify-syntax-entry ?| ".   " eshell-mode-syntax-table)
      (modify-syntax-entry ?# "'   " eshell-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " eshell-mode-syntax-table)
      (modify-syntax-entry ?\\ "/   " eshell-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " eshell-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " eshell-mode-syntax-table)
      (modify-syntax-entry ?\{ "(}  " eshell-mode-syntax-table)
      (modify-syntax-entry ?\} "){  " eshell-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " eshell-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " eshell-mode-syntax-table)
      ;; All non-word multibyte characters should be `symbol'.
      (if (featurep 'xemacs)
	  (map-char-table
	   (function
	    (lambda (key val)
	      (and (characterp key)
		   (>= (char-int key) 256)
		   (/= (char-syntax key) ?w)
		   (modify-syntax-entry key "_   "
					eshell-mode-syntax-table))))
	   (standard-syntax-table))
	(map-char-table
	 (function
	  (lambda (key val)
	    (and (if (consp key)
		     (and (>= (car key) 128)
			  (/= (char-syntax (car key)) ?w))
		   (and (>= key 256)
			(/= (char-syntax key) ?w)))
		 (modify-syntax-entry key "_   "
				      eshell-mode-syntax-table))))
	 (standard-syntax-table)))))

;;; User Functions:

(defun eshell-kill-buffer-function ()
  "Function added to `kill-buffer-hook' in Eshell buffers.
This runs the function `eshell-kill-processes-on-exit',
and the hook `eshell-exit-hook'."
  ;; It's fine to run this unconditionally since it can be customized
  ;; via the `eshell-kill-processes-on-exit' variable.
  (and (fboundp 'eshell-query-kill-processes)
       (not (memq 'eshell-query-kill-processes eshell-exit-hook))
       (eshell-query-kill-processes))
  (run-hooks 'eshell-exit-hook))

;;;###autoload
(defun eshell-mode ()
  "Emacs shell interactive mode.

\\{eshell-mode-map}"
  (kill-all-local-variables)

  (setq major-mode 'eshell-mode)
  (setq mode-name "EShell")
  (set (make-local-variable 'eshell-mode) t)

  (make-local-variable 'eshell-mode-map)
  (setq eshell-mode-map (make-sparse-keymap))
  (use-local-map eshell-mode-map)

  (when eshell-status-in-modeline
    (make-local-variable 'eshell-command-running-string)
    (let ((fmt (copy-sequence mode-line-format)))
      (make-local-variable 'mode-line-format)
      (setq mode-line-format fmt))
    (let ((modeline (memq 'mode-line-modified mode-line-format)))
      (if modeline
	  (setcar modeline 'eshell-command-running-string))))

  (define-key eshell-mode-map [return] 'eshell-send-input)
  (define-key eshell-mode-map [(control ?m)] 'eshell-send-input)
  (define-key eshell-mode-map [(control ?j)] 'eshell-send-input)
  (define-key eshell-mode-map [(meta return)] 'eshell-queue-input)
  (define-key eshell-mode-map [(meta control ?m)] 'eshell-queue-input)
  (define-key eshell-mode-map [(meta control ?l)] 'eshell-show-output)
  (define-key eshell-mode-map [(control ?a)] 'eshell-bol)

  (set (make-local-variable 'eshell-command-prefix)
       (make-symbol "eshell-command-prefix"))
  (fset eshell-command-prefix (make-sparse-keymap))
  (set (make-local-variable 'eshell-command-map)
       (symbol-function eshell-command-prefix))
  (define-key eshell-mode-map [(control ?c)] eshell-command-prefix)

  ;; without this, find-tag complains about read-only text being
  ;; modified
  (if (eq (key-binding [(meta ?.)]) 'find-tag)
      (define-key eshell-mode-map [(meta ?.)] 'eshell-find-tag))
  (define-key eshell-command-map [(meta ?o)] 'eshell-mark-output)
  (define-key eshell-command-map [(meta ?d)] 'eshell-toggle-direct-send)

  (define-key eshell-command-map [(control ?a)] 'eshell-bol)
  (define-key eshell-command-map [(control ?b)] 'eshell-backward-argument)
  (define-key eshell-command-map [(control ?e)] 'eshell-show-maximum-output)
  (define-key eshell-command-map [(control ?f)] 'eshell-forward-argument)
  (define-key eshell-command-map [return]       'eshell-copy-old-input)
  (define-key eshell-command-map [(control ?m)] 'eshell-copy-old-input)
  (define-key eshell-command-map [(control ?o)] 'eshell-kill-output)
  (define-key eshell-command-map [(control ?r)] 'eshell-show-output)
  (define-key eshell-command-map [(control ?t)] 'eshell-truncate-buffer)
  (define-key eshell-command-map [(control ?u)] 'eshell-kill-input)
  (define-key eshell-command-map [(control ?w)] 'backward-kill-word)
  (define-key eshell-command-map [(control ?y)] 'eshell-repeat-argument)

  (setq local-abbrev-table eshell-mode-abbrev-table)
  (set-syntax-table eshell-mode-syntax-table)

  (set (make-local-variable 'dired-directory) default-directory)
  (set (make-local-variable 'list-buffers-directory)
       (expand-file-name default-directory))

  ;; always set the tab width to 8 in Eshell buffers, since external
  ;; commands which do their own formatting almost always expect this
  (set (make-local-variable 'tab-width) 8)

  ;; don't ever use auto-fill in Eshell buffers
  (setq auto-fill-function nil)

  ;; always display everything from a return value
  (if (boundp 'print-length)
      (set (make-local-variable 'print-length) nil))
  (if (boundp 'print-level)
      (set (make-local-variable 'print-level) nil))

  ;; set require-final-newline to nil; otherwise, all redirected
  ;; output will end with a newline, whether or not the source
  ;; indicated it!
  (set (make-local-variable 'require-final-newline) nil)

  (set (make-local-variable 'max-lisp-eval-depth)
       (max 3000 max-lisp-eval-depth))
  (set (make-local-variable 'max-specpdl-size)
       (max 6000 max-lisp-eval-depth))

  (set (make-local-variable 'eshell-last-input-start) (point-marker))
  (set (make-local-variable 'eshell-last-input-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-start) (point-marker))
  (set (make-local-variable 'eshell-last-output-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-block-begin) (point))

  (let ((modules-list (copy-sequence eshell-modules-list)))
    (make-local-variable 'eshell-modules-list)
    (setq eshell-modules-list modules-list))

  ;; load extension modules into memory.  This will cause any global
  ;; variables they define to be visible, since some of the core
  ;; modules sometimes take advantage of their functionality if used.
  (dolist (module eshell-modules-list)
    (let ((module-fullname (symbol-name module))
	  module-shortname)
      (if (string-match "^eshell-\\(.*\\)" module-fullname)
	  (setq module-shortname
		(concat "em-" (match-string 1 module-fullname))))
      (unless module-shortname
	(error "Invalid Eshell module name: %s" module-fullname))
      (unless (featurep (intern module-shortname))
	(load module-shortname))))

  (unless (file-exists-p eshell-directory-name)
    (eshell-make-private-directory eshell-directory-name t))

  ;; Load core Eshell modules, then extension modules, for this session.
  (dolist (module (append (eshell-subgroups 'eshell) eshell-modules-list))
    (let ((load-hook (intern-soft (format "%s-load-hook" module)))
          (initfunc (intern-soft (format "%s-initialize" module))))
      (when (and load-hook (boundp load-hook))
        (if (memq initfunc (symbol-value load-hook)) (setq initfunc nil))
        (run-hooks load-hook))
      ;; So we don't need the -initialize functions on the hooks (b#5375).
      (and initfunc (fboundp initfunc) (funcall initfunc))))

  (if eshell-send-direct-to-subprocesses
      (add-hook 'pre-command-hook 'eshell-intercept-commands t t))

  (if eshell-scroll-to-bottom-on-input
      (add-hook 'pre-command-hook 'eshell-preinput-scroll-to-bottom t t))

  (when eshell-scroll-show-maximum-output
    (set (make-local-variable 'scroll-conservatively) 1000))

  (when eshell-status-in-modeline
    (add-hook 'eshell-pre-command-hook 'eshell-command-started nil t)
    (add-hook 'eshell-post-command-hook 'eshell-command-finished nil t))

  (add-hook 'kill-buffer-hook 'eshell-kill-buffer-function t t)

  (if eshell-first-time-p
      (run-hooks 'eshell-first-time-mode-hook))
  (run-mode-hooks 'eshell-mode-hook)
  (run-hooks 'eshell-post-command-hook))

(put 'eshell-mode 'mode-class 'special)

(defun eshell-command-started ()
  "Indicate in the modeline that a command has started."
  (setq eshell-command-running-string "**")
  (force-mode-line-update))

(defun eshell-command-finished ()
  "Indicate in the modeline that a command has finished."
  (setq eshell-command-running-string "--")
  (force-mode-line-update))

;;; Internal Functions:

(defun eshell-toggle-direct-send ()
  (interactive)
  (if eshell-send-direct-to-subprocesses
      (progn
	(setq eshell-send-direct-to-subprocesses nil)
	(remove-hook 'pre-command-hook 'eshell-intercept-commands t)
	(message "Sending subprocess input on RET"))
    (setq eshell-send-direct-to-subprocesses t)
    (add-hook 'pre-command-hook 'eshell-intercept-commands t t)
    (message "Sending subprocess input directly")))

(defun eshell-self-insert-command (N)
  (interactive "i")
  (process-send-string
   (eshell-interactive-process)
   (char-to-string (if (symbolp last-command-event)
		       (get last-command-event 'ascii-character)
		     last-command-event))))

(defun eshell-intercept-commands ()
  (when (and (eshell-interactive-process)
	     (not (and (integerp last-input-event)
		       (memq last-input-event '(?\C-x ?\C-c)))))
    (let ((possible-events (where-is-internal this-command))
	  (name (symbol-name this-command))
	  (intercept t))
      ;; Assume that any multikey combination which does NOT target an
      ;; Eshell command, is a combo the user wants invoked rather than
      ;; sent to the underlying subprocess.
      (unless (and (> (length name) 7)
		   (equal (substring name 0 7) "eshell-"))
	(while possible-events
	  (if (> (length (car possible-events)) 1)
	      (setq intercept nil possible-events nil)
	    (setq possible-events (cdr possible-events)))))
      (if intercept
	  (setq this-command 'eshell-self-insert-command)))))

(declare-function find-tag-interactive "etags" (prompt &optional no-default))

(defun eshell-find-tag (&optional tagname next-p regexp-p)
  "A special version of `find-tag' that ignores read-onlyness."
  (interactive)
  (require 'etags)
  (let ((inhibit-read-only t)
	(no-default (eobp))
	(find-tag-default-function 'ignore))
    (setq tagname (car (find-tag-interactive "Find tag: " no-default)))
    (find-tag tagname next-p regexp-p)))

(defun eshell-move-argument (limit func property arg)
  "Move forward ARG arguments."
  (catch 'eshell-incomplete
    (eshell-parse-arguments (save-excursion (eshell-bol) (point))
			    (line-end-position)))
  (let ((pos (save-excursion
	       (funcall func 1)
	       (while (and (> arg 0) (/= (point) limit))
		 (if (get-text-property (point) property)
		     (setq arg (1- arg)))
		 (if (> arg 0)
		     (funcall func 1)))
	       (point))))
    (goto-char pos)
    (if (and (eq func 'forward-char)
	     (= (1+ pos) limit))
	(forward-char 1))))

(defun eshell-forward-argument (&optional arg)
  "Move forward ARG arguments."
  (interactive "p")
  (eshell-move-argument (point-max) 'forward-char 'arg-end arg))

(defun eshell-backward-argument (&optional arg)
  "Move backward ARG arguments."
  (interactive "p")
  (eshell-move-argument (point-min) 'backward-char 'arg-begin arg))

(defun eshell-repeat-argument (&optional arg)
  (interactive "p")
  (let ((begin (save-excursion
		 (eshell-backward-argument arg)
		 (point))))
    (kill-ring-save begin (point))
    (yank)))

(defun eshell-bol ()
  "Goes to the beginning of line, then skips past the prompt, if any."
  (interactive)
  (beginning-of-line)
  (and eshell-skip-prompt-function
       (funcall eshell-skip-prompt-function)))

(defsubst eshell-push-command-mark ()
  "Push a mark at the end of the last input text."
  (push-mark (1- eshell-last-input-end) t))

(custom-add-option 'eshell-pre-command-hook 'eshell-push-command-mark)

(defsubst eshell-goto-input-start ()
  "Goto the start of the last command input.
Putting this function on `eshell-pre-command-hook' will mimic Plan 9's
9term behavior."
  (goto-char eshell-last-input-start))

(custom-add-option 'eshell-pre-command-hook 'eshell-push-command-mark)

(defsubst eshell-interactive-print (string)
  "Print STRING to the eshell display buffer."
  (eshell-output-filter nil string))

(defsubst eshell-begin-on-new-line ()
  "This function outputs a newline if not at beginning of line."
  (save-excursion
    (goto-char eshell-last-output-end)
    (or (bolp)
	(eshell-interactive-print "\n"))))

(defsubst eshell-reset (&optional no-hooks)
  "Output a prompt on a new line, aborting any current input.
If NO-HOOKS is non-nil, then `eshell-post-command-hook' won't be run."
  (goto-char (point-max))
  (setq eshell-last-input-start (point-marker)
	eshell-last-input-end (point-marker)
	eshell-last-output-start (point-marker)
	eshell-last-output-block-begin (point)
	eshell-last-output-end (point-marker))
  (eshell-begin-on-new-line)
  (unless no-hooks
    (run-hooks 'eshell-post-command-hook)
    (goto-char (point-max))))

(defun eshell-parse-command-input (beg end &optional args)
  "Parse the command input from BEG to END.
The difference is that `eshell-parse-command' expects a complete
command string (and will error if it doesn't get one), whereas this
function will inform the caller whether more input is required.

If nil is returned, more input is necessary (probably because a
multi-line input string wasn't terminated properly).  Otherwise, it
will return the parsed command."
  (let (delim command)
    (if (setq delim
	      (catch 'eshell-incomplete
		(ignore
		 (setq command (eshell-parse-command (cons beg end)
						     args t)))))
	(ignore
	 (message "Expecting completion of delimiter %c ..."
		  (if (listp delim)
		      (car delim)
		    delim)))
      command)))

(defun eshell-update-markers (pmark)
  "Update the input and output markers relative to point and PMARK."
  (set-marker eshell-last-input-start pmark)
  (set-marker eshell-last-input-end (point))
  (set-marker eshell-last-output-end (point)))

(defun eshell-queue-input (&optional use-region)
  "Queue the current input text for execution by Eshell.
Particularly, don't send the text to the current process, even if it's
waiting for input."
  (interactive "P")
  (eshell-send-input use-region t))

(defun eshell-send-input (&optional use-region queue-p no-newline)
  "Send the input received to Eshell for parsing and processing.
After `eshell-last-output-end', sends all text from that marker to
point as input.  Before that marker, calls `eshell-get-old-input' to
retrieve old input, copies it to the end of the buffer, and sends it.

If USE-REGION is non-nil, the current region (between point and mark)
will be used as input.

If QUEUE-P is non-nil, input will be queued until the next prompt,
rather than sent to the currently active process.  If no process, the
input is processed immediately.

If NO-NEWLINE is non-nil, the input is sent without an implied final
newline."
  (interactive "P")
  ;; Note that the input string does not include its terminal newline.
  (let ((proc-running-p (and (eshell-interactive-process)
			     (not queue-p)))
	(inhibit-point-motion-hooks t)
	after-change-functions)
    (unless (and proc-running-p
		 (not (eq (process-status
			   (eshell-interactive-process)) 'run)))
      (if (or proc-running-p
	      (>= (point) eshell-last-output-end))
	  (goto-char (point-max))
	(let ((copy (eshell-get-old-input use-region)))
	  (goto-char eshell-last-output-end)
	  (insert-and-inherit copy)))
      (unless (or no-newline
		  (and eshell-send-direct-to-subprocesses
		       proc-running-p))
	(insert-before-markers-and-inherit ?\n))
      (if proc-running-p
	  (progn
	    (eshell-update-markers eshell-last-output-end)
	    (if (or eshell-send-direct-to-subprocesses
		    (= eshell-last-input-start eshell-last-input-end))
		(unless no-newline
		  (process-send-string (eshell-interactive-process) "\n"))
	      (process-send-region (eshell-interactive-process)
				   eshell-last-input-start
				   eshell-last-input-end)))
	(if (= eshell-last-output-end (point))
	    (run-hooks 'eshell-post-command-hook)
	  (let (input)
	    (eshell-condition-case err
		(progn
		  (setq input (buffer-substring-no-properties
			       eshell-last-output-end (1- (point))))
		  (run-hook-with-args 'eshell-expand-input-functions
				      eshell-last-output-end (1- (point)))
		  (let ((cmd (eshell-parse-command-input
			      eshell-last-output-end (1- (point)))))
		    (when cmd
		      (eshell-update-markers eshell-last-output-end)
		      (setq input (buffer-substring-no-properties
				   eshell-last-input-start
				   (1- eshell-last-input-end)))
		      (run-hooks 'eshell-input-filter-functions)
		      (and (catch 'eshell-terminal
			     (ignore
			      (if (eshell-invoke-directly cmd input)
				  (eval cmd)
				(eshell-eval-command cmd input))))
			   (eshell-life-is-too-much)))))
	      (quit
	       (eshell-reset t)
	       (run-hooks 'eshell-post-command-hook)
	       (signal 'quit nil))
	      (error
	       (eshell-reset t)
	       (eshell-interactive-print
		(concat (error-message-string err) "\n"))
	       (run-hooks 'eshell-post-command-hook)
	       (insert-and-inherit input)))))))))

(defsubst eshell-kill-new ()
  "Add the last input text to the kill ring."
  (kill-ring-save eshell-last-input-start eshell-last-input-end))

(custom-add-option 'eshell-input-filter-functions 'eshell-kill-new)

(defun eshell-output-filter (process string)
  "Send the output from PROCESS (STRING) to the interactive display.
This is done after all necessary filtering has been done."
  (let ((oprocbuf (if process (process-buffer process)
		    (current-buffer)))
	(inhibit-point-motion-hooks t)
	after-change-functions)
    (let ((functions eshell-preoutput-filter-functions))
      (while (and functions string)
	(setq string (funcall (car functions) string))
	(setq functions (cdr functions))))
    (if (and string oprocbuf (buffer-name oprocbuf))
	(let (opoint obeg oend)
	  (with-current-buffer oprocbuf
	    (setq opoint (point))
	    (setq obeg (point-min))
	    (setq oend (point-max))
	    (let ((buffer-read-only nil)
		  (nchars (length string))
		  (ostart nil))
	      (widen)
	      (goto-char eshell-last-output-end)
	      (setq ostart (point))
	      (if (<= (point) opoint)
		  (setq opoint (+ opoint nchars)))
	      (if (< (point) obeg)
		  (setq obeg (+ obeg nchars)))
	      (if (<= (point) oend)
		  (setq oend (+ oend nchars)))
	      (insert-before-markers string)
	      (if (= (window-start (selected-window)) (point))
		  (set-window-start (selected-window)
				    (- (point) nchars)))
	      (if (= (point) eshell-last-input-end)
		  (set-marker eshell-last-input-end
			      (- eshell-last-input-end nchars)))
	      (set-marker eshell-last-output-start ostart)
	      (set-marker eshell-last-output-end (point))
	      (force-mode-line-update))
	    (narrow-to-region obeg oend)
	    (goto-char opoint)
	    (eshell-run-output-filters))))))

(defun eshell-run-output-filters ()
  "Run the `eshell-output-filter-functions' on the current output."
  (save-current-buffer
    (run-hooks 'eshell-output-filter-functions))
  (setq eshell-last-output-block-begin
	(marker-position eshell-last-output-end)))

;;; jww (1999-10-23): this needs testing
(defun eshell-preinput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Movement occurs if point in the selected window is not after the
process mark, and `this-command' is an insertion command.  Insertion
commands recognized are `self-insert-command', `yank', and
`hilit-yank'.  Depends on the value of
`eshell-scroll-to-bottom-on-input'.

This function should be a pre-command hook."
  (if (memq this-command '(self-insert-command yank hilit-yank))
      (let* ((selected (selected-window))
	     (current (current-buffer))
	     (scroll eshell-scroll-to-bottom-on-input))
	(if (< (point) eshell-last-output-end)
	    (if (eq scroll 'this)
		(goto-char (point-max))
	      (walk-windows
	       (function
		(lambda (window)
		  (when (and (eq (window-buffer window) current)
			     (or (eq scroll t) (eq scroll 'all)))
		    (select-window window)
		    (goto-char (point-max))
		    (select-window selected))))
	       nil t))))))

;;; jww (1999-10-23): this needs testing
(defun eshell-postoutput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Does not scroll if the current line is the last line in the buffer.
Depends on the value of `eshell-scroll-to-bottom-on-output' and
`eshell-scroll-show-maximum-output'.

This function should be in the list `eshell-output-filter-functions'."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (scroll eshell-scroll-to-bottom-on-output))
    (unwind-protect
	(walk-windows
	 (function
	  (lambda (window)
	    (if (eq (window-buffer window) current)
		(progn
		  (select-window window)
		  (if (and (< (point) eshell-last-output-end)
			   (or (eq scroll t) (eq scroll 'all)
			       ;; Maybe user wants point to jump to end.
			       (and (eq scroll 'this)
				    (eq selected window))
			       (and (eq scroll 'others)
				    (not (eq selected window)))
			       ;; If point was at the end, keep it at end.
			       (>= (point) eshell-last-output-start)))
		      (goto-char eshell-last-output-end))
		  ;; Optionally scroll so that the text
		  ;; ends at the bottom of the window.
		  (if (and eshell-scroll-show-maximum-output
			   (>= (point) eshell-last-output-end))
		      (save-excursion
			(goto-char (point-max))
			(recenter -1)))
		  (select-window selected)))))
	 nil t)
      (set-buffer current))))

(defun eshell-beginning-of-input ()
  "Return the location of the start of the previous input."
  eshell-last-input-start)

(defun eshell-beginning-of-output ()
  "Return the location of the end of the previous output block."
  eshell-last-input-end)

(defun eshell-end-of-output ()
  "Return the location of the end of the previous output block."
  (if (eshell-using-module 'eshell-prompt)
      eshell-last-output-start
    eshell-last-output-end))

(defun eshell-kill-output ()
  "Kill all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (save-excursion
    (goto-char (eshell-beginning-of-output))
    (insert "*** output flushed ***\n")
    (delete-region (point) (eshell-end-of-output))))

(defun eshell-show-output (&optional arg)
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run.
With a prefix argument, narrows region to last command output."
  (interactive "P")
  (goto-char (eshell-beginning-of-output))
  (set-window-start (selected-window)
		    (save-excursion
		      (goto-char (eshell-beginning-of-input))
		      (line-beginning-position)))
  (if arg
      (narrow-to-region (eshell-beginning-of-output)
			(eshell-end-of-output)))
  (eshell-end-of-output))

(defun eshell-mark-output (&optional arg)
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run.
With a prefix argument, narrows region to last command output."
  (interactive "P")
  (push-mark (eshell-show-output arg)))

(defun eshell-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (if (> (point) eshell-last-output-end)
      (kill-region eshell-last-output-end (point))
    (let ((here (point)))
      (eshell-bol)
      (kill-region (point) here))))

(defun eshell-show-maximum-output (&optional interactive)
  "Put the end of the buffer at the bottom of the window.
When run interactively, widen the buffer first."
  (interactive "p")
  (if interactive
      (widen))
  (goto-char (point-max))
  (recenter -1))

(defun eshell-get-old-input (&optional use-current-region)
  "Return the command input on the current line."
  (if use-current-region
      (buffer-substring (min (point) (mark))
			(max (point) (mark)))
    (save-excursion
      (beginning-of-line)
      (and eshell-skip-prompt-function
	   (funcall eshell-skip-prompt-function))
      (let ((beg (point)))
	(end-of-line)
	(buffer-substring beg (point))))))

(defun eshell-copy-old-input ()
  "Insert after prompt old input at point as new input to be edited."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (goto-char eshell-last-output-end)
    (insert-and-inherit input)))

(defun eshell/exit ()
  "Leave or kill the Eshell buffer, depending on `eshell-kill-on-exit'."
  (throw 'eshell-terminal t))

(defun eshell-life-is-too-much ()
  "Kill the current buffer (or bury it).  Good-bye Eshell."
  (interactive)
  (if (not eshell-kill-on-exit)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun eshell-truncate-buffer ()
  "Truncate the buffer to `eshell-buffer-maximum-lines'.
This function could be on `eshell-output-filter-functions' or bound to
a key."
  (interactive)
  (save-excursion
    (goto-char eshell-last-output-end)
    (let ((lines (count-lines 1 (point)))
	  (inhibit-read-only t))
      (forward-line (- eshell-buffer-maximum-lines))
      (beginning-of-line)
      (let ((pos (point)))
	(if (bobp)
	    (if (called-interactively-p 'interactive)
		(message "Buffer too short to truncate"))
	  (delete-region (point-min) (point))
	  (if (called-interactively-p 'interactive)
	      (message "Truncated buffer from %d to %d lines (%.1fk freed)"
		       lines eshell-buffer-maximum-lines
		       (/ pos 1024.0))))))))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-truncate-buffer)

(defun eshell-send-invisible (str)
  "Read a string without echoing.
Then send it to the process running in the current buffer."
  (interactive "P")                     ; Defeat snooping via C-x ESC ESC
  (let ((str (read-passwd
	      (format "%s Password: "
		      (process-name (eshell-interactive-process))))))
    (if (stringp str)
	(process-send-string (eshell-interactive-process)
			     (concat str "\n"))
      (message "Warning: text will be echoed"))))

(defun eshell-watch-for-password-prompt ()
  "Prompt in the minibuffer for password and send without echoing.
This function uses `eshell-send-invisible' to read and send a password to the
buffer's process if STRING contains a password prompt defined by
`eshell-password-prompt-regexp'.

This function could be in the list `eshell-output-filter-functions'."
  (when (eshell-interactive-process)
    (save-excursion
      (goto-char eshell-last-output-block-begin)
      (beginning-of-line)
      (if (re-search-forward eshell-password-prompt-regexp
			     eshell-last-output-end t)
	  (eshell-send-invisible nil)))))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-watch-for-password-prompt)

(defun eshell-handle-control-codes ()
  "Act properly when certain control codes are seen."
  (save-excursion
    (let ((orig (point)))
      (goto-char eshell-last-output-block-begin)
      (unless (eolp)
	(beginning-of-line))
      (while (< (point) eshell-last-output-end)
	(let ((char (char-after)))
	  (cond
	   ((eq char ?\r)
	    (if (< (1+ (point)) eshell-last-output-end)
		(if (memq (char-after (1+ (point)))
			  '(?\n ?\r))
		    (delete-char 1)
		  (let ((end (1+ (point))))
		    (beginning-of-line)
		    (delete-region (point) end)))
	      (add-text-properties (point) (1+ (point))
				   '(invisible t))
	      (forward-char)))
	   ((eq char ?\a)
	    (delete-char 1)
	    (beep))
	   ((eq char ?\C-h)
	    (delete-backward-char 1)
	    (delete-char 1))
	   (t
	    (forward-char))))))))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-handle-control-codes)

(autoload 'ansi-color-apply-on-region "ansi-color")

(defun eshell-handle-ansi-color ()
  "Handle ANSI color codes."
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-handle-ansi-color)

;;; esh-mode.el ends here
