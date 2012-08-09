;;; cmuscheme.el --- Scheme process in a buffer. Adapted from tea.el

;; Copyright (C) 1988, 1994, 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Olin Shivers <olin.shivers@cs.cmu.edu>
;; Maintainer: FSF
;; Keywords: processes, lisp

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

;;    This is a customization of comint-mode (see comint.el)
;;
;; Written by Olin Shivers (olin.shivers@cs.cmu.edu). With bits and pieces
;; lifted from scheme.el, shell.el, clisp.el, newclisp.el, cobol.el, et al..
;; 8/88
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;
;; The changelog is at the end of this file.
;;
;; NOTE: MIT Cscheme, when invoked with the -emacs flag, has a special user
;; interface that communicates process state back to the superior emacs by
;; outputting special control sequences. The gnumacs package, xscheme.el, has
;; lots and lots of special purpose code to read these control sequences, and
;; so is very tightly integrated with the cscheme process. The cscheme
;; interrupt handler and debugger read single character commands in cbreak
;; mode; when this happens, xscheme.el switches to special keymaps that bind
;; the single letter command keys to emacs functions that directly send the
;; character to the scheme process.  Cmuscheme mode does *not* provide this
;; functionality. If you are a cscheme user, you may prefer to use the
;; xscheme.el/cscheme -emacs interaction.
;;
;; Here's a summary of the pros and cons, as I see them.
;; xscheme: Tightly integrated with inferior cscheme process!  A few commands
;;	     not in cmuscheme. But. Integration is a bit of a hack.  Input
;;	     history only keeps the immediately prior input. Bizarre
;;	     keybindings.
;;
;; cmuscheme: Not tightly integrated with inferior cscheme process.  But.
;;            Carefully integrated functionality with the entire suite of
;;            comint-derived CMU process modes. Keybindings reminiscent of
;;            Zwei and Hemlock. Good input history. A few commands not in
;;            xscheme.
;;
;; It's a tradeoff. Pay your money; take your choice. If you use a Scheme
;; that isn't Cscheme, of course, there isn't a choice. Xscheme.el is *very*
;; Cscheme-specific; you must use cmuscheme.el.  Interested parties are
;; invited to port xscheme functionality on top of comint mode...

;;; CHANGE LOG
;; ===========================================================================
;; 8/88 Olin
;; Created.
;;
;; 2/15/89 Olin
;; Removed -emacs flag from process invocation. It's only useful for
;; cscheme, and makes cscheme assume it's running under xscheme.el,
;; which messes things up royally. A bug.
;;
;; 5/22/90 Olin
;; - Upgraded to use comint-send-string and comint-send-region.
;; - run-scheme now offers to let you edit the command line if
;;   you invoke it with a prefix-arg. M-x scheme is redundant, and
;;   has been removed.
;; - Explicit references to process "scheme" have been replaced with
;;   (scheme-proc). This allows better handling of multiple process bufs.
;; - Added scheme-send-last-sexp, bound to C-x C-e. A gnu convention.
;; - Have not added process query facility a la cmulisp.el's lisp-show-arglist
;;   and friends, but interested hackers might find a useful application
;;   of this facility.
;;
;; 3/12/90 Olin
;; - scheme-load-file and scheme-compile-file no longer switch-to-scheme.
;;   Tale suggested this.

;;; Code:

(require 'scheme)
(require 'comint)


(defgroup cmuscheme nil
  "Run a scheme process in a buffer."
  :group 'scheme)

;;; INFERIOR SCHEME MODE STUFF
;;;============================================================================

(defcustom inferior-scheme-mode-hook nil
  "Hook for customizing inferior-scheme mode."
  :type 'hook
  :group 'cmuscheme)

(defvar inferior-scheme-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" 'scheme-send-definition) ;gnu convention
    (define-key m "\C-x\C-e" 'scheme-send-last-sexp)
    (define-key m "\C-c\C-l" 'scheme-load-file)
    (define-key m "\C-c\C-k" 'scheme-compile-file)
    (scheme-mode-commands m)
    m))

;; Install the process communication commands in the scheme-mode keymap.
(define-key scheme-mode-map "\M-\C-x" 'scheme-send-definition);gnu convention
(define-key scheme-mode-map "\C-x\C-e" 'scheme-send-last-sexp);gnu convention
(define-key scheme-mode-map "\C-c\C-e" 'scheme-send-definition)
(define-key scheme-mode-map "\C-c\M-e" 'scheme-send-definition-and-go)
(define-key scheme-mode-map "\C-c\C-r" 'scheme-send-region)
(define-key scheme-mode-map "\C-c\M-r" 'scheme-send-region-and-go)
(define-key scheme-mode-map "\C-c\M-c" 'scheme-compile-definition)
(define-key scheme-mode-map "\C-c\C-c" 'scheme-compile-definition-and-go)
(define-key scheme-mode-map "\C-c\C-t" 'scheme-trace-procedure)
(define-key scheme-mode-map "\C-c\C-x" 'scheme-expand-current-form)
(define-key scheme-mode-map "\C-c\C-z" 'switch-to-scheme)
(define-key scheme-mode-map "\C-c\C-l" 'scheme-load-file)
(define-key scheme-mode-map "\C-c\C-k" 'scheme-compile-file) ;k for "kompile"

(let ((map (lookup-key scheme-mode-map [menu-bar scheme])))
  (define-key map [separator-eval] '("--"))
  (define-key map [compile-file]
    '("Compile Scheme File" . scheme-compile-file))
  (define-key map [load-file]
    '("Load Scheme File" . scheme-load-file))
  (define-key map [switch]
    '("Switch to Scheme" . switch-to-scheme))
  (define-key map [com-def-go]
    '("Compile Definition & Go" . scheme-compile-definition-and-go))
  (define-key map [com-def]
    '("Compile Definition" . scheme-compile-definition))
  (define-key map [exp-form]
    '("Expand current form" . scheme-expand-current-form))
  (define-key map [trace-proc]
    '("Trace procedure" . scheme-trace-procedure))
  (define-key map [send-def-go]
    '("Evaluate Last Definition & Go" . scheme-send-definition-and-go))
  (define-key map [send-def]
    '("Evaluate Last Definition" . scheme-send-definition))
  (define-key map [send-region-go]
    '("Evaluate Region & Go" . scheme-send-region-and-go))
  (define-key map [send-region]
    '("Evaluate Region" . scheme-send-region))
  (define-key map [send-sexp]
    '("Evaluate Last S-expression" . scheme-send-last-sexp))
  )

(defvar scheme-buffer)

(define-derived-mode inferior-scheme-mode comint-mode "Inferior Scheme"
  "Major mode for interacting with an inferior Scheme process.

The following commands are available:
\\{inferior-scheme-mode-map}

A Scheme process can be fired up with M-x run-scheme.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-scheme-mode-hook (in that order).

You can send text to the inferior Scheme process from other buffers containing
Scheme source.
    switch-to-scheme switches the current buffer to the Scheme process buffer.
    scheme-send-definition sends the current definition to the Scheme process.
    scheme-compile-definition compiles the current definition.
    scheme-send-region sends the current region to the Scheme process.
    scheme-compile-region compiles the current region.

    scheme-send-definition-and-go, scheme-compile-definition-and-go,
        scheme-send-region-and-go, and scheme-compile-region-and-go
        switch to the Scheme process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable scheme-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  ;; Customize in inferior-scheme-mode-hook
  (setq comint-prompt-regexp "^[^>\n]*>+ *") ; OK for cscheme, oaklisp, T,...
  (scheme-mode-variables)
  (setq mode-line-process '(":%s"))
  (setq comint-input-filter (function scheme-input-filter))
  (setq comint-get-old-input (function scheme-get-old-input)))

(defcustom inferior-scheme-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'cmuscheme)

(defun scheme-input-filter (str)
  "Don't save anything matching `inferior-scheme-filter-regexp'."
  (not (string-match inferior-scheme-filter-regexp str)))

(defun scheme-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;;;###autoload
(defun run-scheme (cmd)
  "Run an inferior Scheme process, input and output via buffer `*scheme*'.
If there is a process already running in `*scheme*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `scheme-program-name').
If the file `~/.emacs_SCHEMENAME' or `~/.emacs.d/init_SCHEMENAME.scm' exists,
it is given as initial input.
Note that this may lose due to a timing error if the Scheme processor
discards input when it starts up.
Runs the hook `inferior-scheme-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Scheme: " scheme-program-name)
			 scheme-program-name)))
  (if (not (comint-check-proc "*scheme*"))
      (let ((cmdlist (split-string-and-unquote cmd)))
	(set-buffer (apply 'make-comint "scheme" (car cmdlist)
			   (scheme-start-file (car cmdlist)) (cdr cmdlist)))
	(inferior-scheme-mode)))
  (setq scheme-program-name cmd)
  (setq scheme-buffer "*scheme*")
  (pop-to-buffer-same-window "*scheme*"))

(defun scheme-start-file (prog)
  "Return the name of the start file corresponding to PROG.
Search in the directories \"~\" and \"~/.emacs.d\", in this
order.  Return nil if no start file found."
  (let* ((progname (file-name-nondirectory prog))
	 (start-file (concat "~/.emacs_" progname))
	 (alt-start-file (concat user-emacs-directory "init_" progname ".scm")))
    (if (file-exists-p start-file)
        start-file
      (and (file-exists-p alt-start-file) alt-start-file))))

(defun scheme-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (comint-send-region (scheme-proc) start end)
  (comint-send-string (scheme-proc) "\n"))

(defun scheme-send-definition ()
  "Send the current definition to the inferior Scheme process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (scheme-send-region (point) end))))

(defun scheme-send-last-sexp ()
  "Send the previous sexp to the inferior Scheme process."
  (interactive)
  (scheme-send-region (save-excursion (backward-sexp) (point)) (point)))

(defcustom scheme-compile-exp-command "(compile '%s)"
  "Template for issuing commands to compile arbitrary Scheme expressions."
  :type 'string
  :group 'cmuscheme)

(defun scheme-compile-region (start end)
  "Compile the current region in the inferior Scheme process.
\(A BEGIN is wrapped around the region: (BEGIN <region>))"
  (interactive "r")
  (comint-send-string (scheme-proc) (format scheme-compile-exp-command
					    (format "(begin %s)"
						    (buffer-substring start end))))
  (comint-send-string (scheme-proc) "\n"))

(defun scheme-compile-definition ()
  "Compile the current definition in the inferior Scheme process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (scheme-compile-region (point) end))))

(defcustom scheme-trace-command "(trace %s)"
  "Template for issuing commands to trace a Scheme procedure.
Some Scheme implementations might require more elaborate commands here.
For PLT-Scheme, e.g., one should use

   (setq scheme-trace-command \"(begin (require (lib \\\"trace.ss\\\")) (trace %s))\")

For Scheme 48 and Scsh use \",trace %s\"."
  :type 'string
  :group 'cmuscheme)

(defcustom scheme-untrace-command "(untrace %s)"
  "Template for switching off tracing of a Scheme procedure.
Scheme 48 and Scsh users should set this variable to \",untrace %s\"."

  :type 'string
  :group 'cmuscheme)

(defun scheme-trace-procedure (proc &optional untrace)
  "Trace procedure PROC in the inferior Scheme process.
With a prefix argument switch off tracing of procedure PROC."
  (interactive
   (list (let ((current (symbol-at-point))
               (action (if current-prefix-arg "Untrace" "Trace")))
           (if current
               (read-string (format "%s procedure [%s]: " action current) nil nil (symbol-name current))
             (read-string (format "%s procedure: " action))))
         current-prefix-arg))
  (when (= (length proc) 0)
    (error "Invalid procedure name"))
  (comint-send-string (scheme-proc)
                      (format
                       (if untrace scheme-untrace-command scheme-trace-command)
                       proc))
  (comint-send-string (scheme-proc) "\n"))

(defcustom scheme-macro-expand-command "(expand %s)"
  "Template for macro-expanding a Scheme form.
For Scheme 48 and Scsh use \",expand %s\"."
  :type 'string
  :group 'cmuscheme)

(defun scheme-expand-current-form ()
  "Macro-expand the form at point in the inferior Scheme process."
  (interactive)
  (let ((current-form (scheme-form-at-point)))
    (if current-form
        (progn
          (comint-send-string (scheme-proc)
                              (format
                               scheme-macro-expand-command
                               current-form))
          (comint-send-string (scheme-proc) "\n"))
      (error "Not at a form"))))

(defun scheme-form-at-point ()
  (let ((next-sexp (thing-at-point 'sexp)))
    (if (and next-sexp (string-equal (substring next-sexp 0 1) "("))
        next-sexp
      (save-excursion
        (backward-up-list)
        (scheme-form-at-point)))))

(defun switch-to-scheme (eob-p)
  "Switch to the scheme process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and scheme-buffer (get-buffer scheme-buffer))
          (scheme-interactively-start-process))
      (pop-to-buffer-same-window scheme-buffer)
    (error "No current process buffer.  See variable `scheme-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun scheme-send-region-and-go (start end)
  "Send the current region to the inferior Scheme process.
Then switch to the process buffer."
  (interactive "r")
  (scheme-send-region start end)
  (switch-to-scheme t))

(defun scheme-send-definition-and-go ()
  "Send the current definition to the inferior Scheme.
Then switch to the process buffer."
  (interactive)
  (scheme-send-definition)
  (switch-to-scheme t))

(defun scheme-compile-definition-and-go ()
  "Compile the current definition in the inferior Scheme.
Then switch to the process buffer."
  (interactive)
  (scheme-compile-definition)
  (switch-to-scheme t))

(defun scheme-compile-region-and-go (start end)
  "Compile the current region in the inferior Scheme.
Then switch to the process buffer."
  (interactive "r")
  (scheme-compile-region start end)
  (switch-to-scheme t))

(defcustom scheme-source-modes '(scheme-mode)
  "Used to determine if a buffer contains Scheme source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a scheme source file by `scheme-load-file' and `scheme-compile-file'.
Used by these commands to determine defaults."
  :type '(repeat function)
  :group 'cmuscheme)

(defvar scheme-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `scheme-load-file' or
`scheme-compile-file' command.  Used for determining the default
in the next one.")

(defun scheme-load-file (file-name)
  "Load a Scheme file FILE-NAME into the inferior Scheme process."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; t because `load'
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-proc) (concat "(load \""
					    file-name
					    "\"\)\n")))

(defun scheme-compile-file (file-name)
  "Compile a Scheme file FILE-NAME in the inferior Scheme process."
  (interactive (comint-get-source "Compile Scheme file: "
				  scheme-prev-l/c-dir/file
				  scheme-source-modes
				  nil)) ; nil because COMPILE doesn't
                                        ; need an exact name.
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-proc) (concat "(compile-file \""
					    file-name
					    "\"\)\n")))


(defvar scheme-buffer nil "*The current scheme process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
Cmuscheme.el supports, in a fairly simple fashion, running multiple Scheme
processes.  To run multiple Scheme processes, you start the first up with
\\[run-scheme].  It will be in a buffer named *scheme*.  Rename this buffer
with \\[rename-buffer].  You may now start up a new process with another
\\[run-scheme].  It will be in a new buffer, named *scheme*.  You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Scheme processes --
like `scheme-send-definition' or `scheme-compile-region' -- have to choose a
process to send to, when you have more than one Scheme process around.  This
is determined by the global variable `scheme-buffer'.  Suppose you
have three inferior Schemes running:
    Buffer	Process
    foo		scheme
    bar		scheme<2>
    *scheme*    scheme<3>
If you do a \\[scheme-send-definition-and-go] command on some Scheme source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *scheme*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `scheme-buffer'.
This process selection is performed by function `scheme-proc'.

Whenever \\[run-scheme] fires up a new process, it resets `scheme-buffer'
to be the new process's buffer.  If you only run one process, this will
do the right thing.  If you run multiple processes, you can change
`scheme-buffer' to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible.  If you find yourself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Scheme processes.  The approach taken here is
for a minimal, simple implementation.  Feel free to extend it.")

(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary.
See variable `scheme-buffer'."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (scheme-interactively-start-process))
  (or (scheme-get-process)
      (error "No current process.  See variable `scheme-buffer'")))

(defun scheme-get-process ()
  "Return the current Scheme process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'inferior-scheme-mode)
                          (current-buffer)
                        scheme-buffer)))

(defun scheme-interactively-start-process (&optional _cmd)
  "Start an inferior Scheme process.  Return the process started.
Since this command is run implicitly, always ask the user for the
command to run."
  (save-window-excursion
    (run-scheme (read-string "Run Scheme: " scheme-program-name))))

;;; Do the user's customization...

(defcustom cmuscheme-load-hook nil
  "This hook is run when cmuscheme is loaded in.
This is a good place to put keybindings."
  :type 'hook
  :group 'cmuscheme)

(run-hooks 'cmuscheme-load-hook)

(provide 'cmuscheme)

;;; cmuscheme.el ends here
