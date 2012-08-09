;;; eshell.el --- the Emacs command shell

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 2.4.2
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

;;;_* What does Eshell offer you?
;;
;; Despite the sheer fact that running an Emacs shell can be fun, here
;; are a few of the unique features offered by Eshell:
;;
;; @ Integration with the Emacs Lisp programming environment
;;
;; @ A high degree of configurability
;;
;; @ The ability to have the same shell on every system Emacs has been
;;   ported to. Since Eshell imposes no external requirements, and
;;   relies upon only the Lisp functions exposed by Emacs, it is quite
;;   operating system independent. Several of the common UNIX
;;   commands, such as ls, mv, rm, ln, etc., have been implemented in
;;   Lisp in order to provide a more consistent work environment.
;;
;; For those who might be using an older version of Eshell, version
;; 2.1 represents an entirely new, module-based architecture. It
;; supports most of the features offered by modern shells. Here is a
;; brief list of some of its more visible features:
;;
;; @ Command argument completion (tcsh, zsh)
;; @ Input history management (bash)
;; @ Intelligent output scrolling
;; @ Pseudo-devices (such as "/dev/clip" for copying to the clipboard)
;; @ Extended globbing (zsh)
;; @ Argument and globbing predication (zsh)
;; @ I/O redirection to buffers, files, symbols, processes, etc.
;; @ Many niceties otherwise seen only in 4DOS
;; @ Alias functions, both Lisp and Eshell-syntax
;; @ Piping, sequenced commands, background jobs, etc...
;;
;;;_* How to begin
;;
;; To start using Eshell, simply type `M-x eshell'.
;;
;;;_* Philosophy
;;
;; A shell is a layer which metaphorically surrounds the kernel, or
;; heart of an operating system.  This kernel can be seen as an engine
;; of pure functionality, waiting to serve, while the user programs
;; take advantage of that functionality to accomplish their purpose.
;;
;; The shell's role is to make that functionality accessible to the
;; user in an unformed state.  Very roughly, it associates kernel
;; functionality with textual commands, allowing the user to interact
;; with the operating system via linguistic constructs.  Process
;; invocation is perhaps the most significant form this takes, using
;; the kernel's `fork' and `exec' functions.
;;
;; Other programs also interact with the functionality of the kernel,
;; but these user applications typically offer a specific range of
;; functionality, and thus are not classed as "shells" proper.
;; (What they lose in quiddity, they gain in rigidity).
;;
;; Emacs is also a user application, but it does make the
;; functionality of the kernel accessible through an interpreted
;; language -- namely, Lisp.  For that reason, there is little
;; preventing Emacs from serving the same role as a modern shell.  It
;; too can manipulate the kernel in an unpredetermined way to cause
;; system changes.  All it's missing is the shell-ish linguistic
;; model.
;;
;; Enter Eshell.  Eshell translates "shell-like" syntax into Lisp
;; in order to exercise the kernel in the same manner as typical
;; system shells.  There is a fundamental difference here, however,
;; although it may seem subtle at first...
;;
;; Shells like csh and Bourne shell were written several decades ago,
;; in different times, under more restrictive circumstances.  This
;; confined perspective shows itself in the paradigm used by nearly
;; all command-line shells since.  They are linear in conception, byte
;; stream-based, sequential, and confined to movement within a single
;; host machine.
;;
;; Emacs, on the other hand, is more than just a limited translator
;; that can invoke subprocesses and redirect file handles.  It also
;; manages character buffers, windowing frames, network connections,
;; registers, bookmarks, processes, etc.  In other words, it's a very
;; multi-dimensional environment, within which eshell emulates a highly
;; linear methodology.
;;
;; Taking a moment, let's look at how this could affect the future of
;; a shell allowed to develop in such a wider field of play:
;;
;; @ There is no reason why directory movement should be linear, and
;;   confined to a single file-system.  Emacs, through w3 and ange-ftp,
;;   has access to the entire Web.  Why not allow a user to cd to
;;   multiple directories simultaneously, for example?  It might make
;;   some tasks easier, such as diff'ing files separated by very long
;;   pathnames.
;;
;; @ Data sources are available from anywhere Emacs can derive
;;   information from: not just from files or the output of other
;;   processes.
;;
;; @ Multiple shell invocations all share the same environment -- even
;;   the same process list!  It would be possible to have "process
;;   views", so that one buffer is watching standard output, another
;;   standard error, and another the result of standard output grep'd
;;   through a regular expression...
;;
;; @ It is not necessary to "leave" the shell, losing all input and
;;   output history, environment variables, directory stack, etc.
;;   Emacs could save the contents of your eshell environment, and
;;   restore all of it (or at least as much as possible) each time you
;;   restart.  This could occur automatically, without requiring
;;   complex initialization scripts.
;;
;; @ Typos occur all of the time; many of them are repeats of common
;;   errors, such as 'dri' for `dir'.  Since executing non-existent
;;   programs is rarely the intention of the user, eshell could prompt
;;   for the replacement string, and then record that in a database of
;;   known misspellings. (Note: The typo at the beginning of this
;;   paragraph wasn't discovered until two months after I wrote the
;;   text; it was not intentional).
;;
;; @ Emacs's register and bookmarking facilities can be used for
;;   remembering where you've been, and what you've seen -- to varying
;;   levels of persistence.  They could perhaps even be tied to
;;   specific "moments" during eshell execution, which would include
;;   the environment at that time, as well as other variables.
;;   Although this would require functionality orthogonal to Emacs's
;;   own bookmarking facilities, the interface used could be made to
;;   operate very similarly.
;;
;; This presents a brief idea of what the fuller dimensionality of an
;; Emacs shell could offer.  It's not just the language of a shell
;; that determines how it's used, but also the Weltanschauung
;; underlying its design -- and which is felt behind even the smallest
;; feature.  I would hope the freedom provided by using Emacs as a
;; parent environment will invite rich ideas from others.  It
;; certainly feels as though all I've done so far is to tie down the
;; horse, so to speak, so that he will run at a man's pace.
;;
;;;_* Influences
;;
;; The author of Eshell has been a long-time user of the following
;; shells, all of which contributed to Eshell's design:
;;
;; @ rc
;; @ bash
;; @ zsh
;; @ sh
;; @ 4nt
;; @ csh

;;;_* Speeding up load time
;;
;; If you find that Eshell loads too slowly, there is something you
;; can do to speed it up.
;;
;; Create a file, named /tmp/elc, containing this filelist:
;;
;;   esh-util.elc
;;   eshell.elc
;;   esh-module.elc
;;   esh-var.elc
;;   esh-proc.elc
;;   esh-arg.elc
;;   esh-io.elc
;;   esh-ext.elc
;;   esh-cmd.elc
;;   esh-mode.elc
;;   esh-opt.elc
;;   em-alias.elc
;;   em-banner.elc
;;   em-basic.elc
;;   em-cmpl.elc
;;   em-dirs.elc
;;   em-pred.elc
;;   em-glob.elc
;;   em-hist.elc
;;   em-ls.elc
;;   em-prompt.elc
;;   em-rebind.elc
;;   em-script.elc
;;   em-smart.elc
;;   em-term.elc
;;   em-unix.elc
;;   em-xtra.elc
;;
;; The order is very important.  Remove from the filelist any features
;; you don't use.  These all begin with "em-".  If you don't use
;; Eshell's key rebinding module, you can remove "em-rebind.elc" from
;; the filelist.  The modules you are currently using are listed in
;; `eshell-modules-list'.
;;
;; Now, concatenating all of the above mentioned .elc files, in that
;; order, to another file.  Here is how to do this on UNIX:
;;
;;   cat `cat /tmp/elc` > tmp.elc ; mv tmp.elc eshell.elc
;;
;; Now your eshell.elc file contains all of the .elc files that make
;; up Eshell, in the right load order.  When you next load Eshell, it
;; will only have to read in this one file, which will greatly speed
;; things up.

(eval-when-compile
  (require 'cl)
  (require 'esh-util))
(require 'esh-util)
(require 'esh-mode)

(defgroup eshell nil
  "A command shell implemented entirely in Emacs Lisp.
It invokes no external processes beyond those requested by the
user, and is intended to be a functional replacement for command
shells such as bash, zsh, rc, 4dos."
  :tag "The Emacs shell"
  :link '(info-link "(eshell)Top")
  :version "21.1"
  :group 'applications)

;; This is hack to force make-autoload to put the whole definition
;; into the autoload file (see esh-module.el).
(defalias 'eshell-defgroup 'defgroup)

;;;_* User Options
;;
;; The following user options modify the behavior of Eshell overall.
(defvar eshell-buffer-name)

(defsubst eshell-add-to-window-buffer-names ()
  "Add `eshell-buffer-name' to `same-window-buffer-names'."
  (add-to-list 'same-window-buffer-names eshell-buffer-name))

(defsubst eshell-remove-from-window-buffer-names ()
  "Remove `eshell-buffer-name' from `same-window-buffer-names'."
  (setq same-window-buffer-names
	(delete eshell-buffer-name same-window-buffer-names)))

(defcustom eshell-load-hook nil
  "A hook run once Eshell has been loaded."
  :type 'hook
  :group 'eshell)

(defcustom eshell-unload-hook
  '(eshell-remove-from-window-buffer-names
    eshell-unload-all-modules)
  "A hook run when Eshell is unloaded from memory."
  :type 'hook
  :group 'eshell)

(defcustom eshell-buffer-name "*eshell*"
  "The basename used for Eshell buffers."
  :set (lambda (symbol value)
	 ;; remove the old value of `eshell-buffer-name', if present
	 (if (boundp 'eshell-buffer-name)
	     (eshell-remove-from-window-buffer-names))
	 (set symbol value)
	 ;; add the new value
	 (eshell-add-to-window-buffer-names)
	 value)
  :type 'string
  :group 'eshell)

(defcustom eshell-directory-name
  (locate-user-emacs-file "eshell/" ".eshell/")
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)

;;;_* Running Eshell
;;
;; There are only three commands used to invoke Eshell.  The first two
;; are intended for interactive use, while the third is meant for
;; programmers.  They are:

;;;###autoload
(defun eshell (&optional arg)
  "Create an interactive Eshell buffer.
The buffer used for Eshell sessions is determined by the value of
`eshell-buffer-name'.  If there is already an Eshell session active in
that buffer, Emacs will simply switch to it.  Otherwise, a new session
will begin.  A numeric prefix arg (as in `C-u 42 M-x eshell RET')
switches to the session with that number, creating it if necessary.  A
nonnumeric prefix arg means to create a new session.  Returns the
buffer selected (or created)."
  (interactive "P")
  (assert eshell-buffer-name)
  (let ((buf (cond ((numberp arg)
		    (get-buffer-create (format "%s<%d>"
					       eshell-buffer-name
					       arg)))
		   (arg
		    (generate-new-buffer eshell-buffer-name))
		   (t
		    (get-buffer-create eshell-buffer-name)))))
    ;; Simply calling `pop-to-buffer' will not mimic the way that
    ;; shell-mode buffers appear, since they always reuse the same
    ;; window that that command was invoked from.  To achieve this,
    ;; it's necessary to add `eshell-buffer-name' to the variable
    ;; `same-window-buffer-names', which is done when Eshell is loaded
    (assert (and buf (buffer-live-p buf)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'eshell-mode)
      (eshell-mode))
    buf))

(defun eshell-return-exits-minibuffer ()
  (define-key eshell-mode-map [(control ?g)] 'abort-recursive-edit)
  (define-key eshell-mode-map [return] 'exit-minibuffer)
  (define-key eshell-mode-map [(control ?m)] 'exit-minibuffer)
  (define-key eshell-mode-map [(control ?j)] 'exit-minibuffer)
  (define-key eshell-mode-map [(meta return)] 'exit-minibuffer)
  (define-key eshell-mode-map [(meta control ?m)] 'exit-minibuffer))

(defvar eshell-non-interactive-p nil
  "A variable which is non-nil when Eshell is not running interactively.
Modules should use this variable so that they don't clutter
non-interactive sessions, such as when using `eshell-command'.")

;;;###autoload
(defun eshell-command (&optional command arg)
  "Execute the Eshell command string COMMAND.
With prefix ARG, insert output into the current buffer at point."
  (interactive)
  (require 'esh-cmd)
  (unless arg
    (setq arg current-prefix-arg))
  (let ((eshell-non-interactive-p t))
    ;; Enable `eshell-mode' only in this minibuffer.
    (minibuffer-with-setup-hook #'(lambda ()
                                    (eshell-mode)
                                    (eshell-return-exits-minibuffer))
      (unless command
        (setq command (read-from-minibuffer "Emacs shell command: "))
        (eshell-add-input-to-history command))))
  (unless command
    (error "No command specified!"))
  ;; redirection into the current buffer is achieved by adding an
  ;; output redirection to the end of the command, of the form
  ;; 'COMMAND >>> #<buffer BUFFER>'.  This will not interfere with
  ;; other redirections, since multiple redirections merely cause the
  ;; output to be copied to multiple target locations
  (if arg
      (setq command
	    (concat command
		    (format " >>> #<buffer %s>"
			    (buffer-name (current-buffer))))))
  (save-excursion
    (let ((buf (set-buffer (generate-new-buffer " *eshell cmd*")))
	  (eshell-non-interactive-p t))
      (eshell-mode)
      (let* ((proc (eshell-eval-command
		    (list 'eshell-commands
			  (eshell-parse-command command))))
	     intr
	     (bufname (if (and proc (listp proc))
			  "*EShell Async Command Output*"
			(setq intr t)
			"*EShell Command Output*")))
	(if (buffer-live-p (get-buffer bufname))
	    (kill-buffer bufname))
	(rename-buffer bufname)
	;; things get a little coarse here, since the desire is to
	;; make the output as attractive as possible, with no
	;; extraneous newlines
	(when intr
	  (if (eshell-interactive-process)
	      (eshell-wait-for-process (eshell-interactive-process)))
	  (assert (not (eshell-interactive-process)))
	  (goto-char (point-max))
	  (while (and (bolp) (not (bobp)))
	    (delete-char -1)))
	(assert (and buf (buffer-live-p buf)))
	(unless arg
	  (let ((len (if (not intr) 2
		       (count-lines (point-min) (point-max)))))
	    (cond
	     ((= len 0)
	      (message "(There was no command output)")
	      (kill-buffer buf))
	     ((= len 1)
	      (message "%s" (buffer-string))
	      (kill-buffer buf))
	     (t
	      (save-selected-window
		(select-window (display-buffer buf))
		(goto-char (point-min))
		;; cause the output buffer to take up as little screen
		;; real-estate as possible, if temp buffer resizing is
		;; enabled
		(and intr temp-buffer-resize-mode
		     (resize-temp-buffer-window)))))))))))

;;;###autoload
(defun eshell-command-result (command &optional status-var)
  "Execute the given Eshell COMMAND, and return the result.
The result might be any Lisp object.
If STATUS-VAR is a symbol, it will be set to the exit status of the
command.  This is the only way to determine whether the value returned
corresponding to a successful execution."
  ;; a null command produces a null, successful result
  (if (not command)
      (ignore
       (if (and status-var (symbolp status-var))
	   (set status-var 0)))
    (with-temp-buffer
      (let ((eshell-non-interactive-p t))
	(eshell-mode)
	(let ((result (eshell-do-eval
		       (list 'eshell-commands
			     (list 'eshell-command-to-value
				   (eshell-parse-command command))) t)))
	  (assert (eq (car result) 'quote))
	  (if (and status-var (symbolp status-var))
	      (set status-var eshell-last-command-status))
	  (cadr result))))))

;;;_* Reporting bugs
;;
;; If you do encounter a bug, on any system, please report
;; it -- in addition to any particular oddities in your configuration
;; -- so that the problem may be corrected for the benefit of others.

;;;###autoload
(define-obsolete-function-alias 'eshell-report-bug 'report-emacs-bug "23.1")

;;; Code:

(defun eshell-unload-all-modules ()
  "Unload all modules that were loaded by Eshell, if possible.
If the user has require'd in any of the modules, or customized a
variable with a :require tag (such as `eshell-prefer-to-shell'), it
will be impossible to unload Eshell completely without restarting
Emacs."
  ;; if the user set `eshell-prefer-to-shell' to t, but never loaded
  ;; Eshell, then `eshell-subgroups' will be unbound
  (when (fboundp 'eshell-subgroups)
    (dolist (module (eshell-subgroups 'eshell))
      ;; this really only unloads as many modules as possible,
      ;; since other `require' references (such as by customizing
      ;; `eshell-prefer-to-shell' to a non-nil value) might make it
      ;; impossible to unload Eshell completely
      (if (featurep module)
	  (ignore-errors
	    (message "Unloading %s..." (symbol-name module))
	    (unload-feature module)
	    (message "Unloading %s...done" (symbol-name module)))))
    (message "Unloading eshell...done")))

(run-hooks 'eshell-load-hook)

(provide 'eshell)

;;; eshell.el ends here
