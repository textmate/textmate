;;; gdb-mi.el --- User Interface for running GDB

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Nick Roberts <nickrob@gnu.org>
;; Maintainer: FSF
;; Keywords: unix, tools

;; This file is part of GNU Emacs.

;; Homepage: http://www.emacswiki.org/emacs/GDB-MI

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

;;; Credits:

;; This file was written by Nick Roberts following the general design
;; used in gdb-ui.el for Emacs 22.1 - 23.1.  It was further developed
;; by Dmitry Dzhus <dima@sphinx.net.ru> as part of the Google Summer
;; of Code 2009 Project "Emacs GDB/MI migration".

;;; Commentary:

;; This mode acts as a graphical user interface to GDB.  You can interact with
;; GDB through the GUD buffer in the usual way, but there are also further
;; buffers which control the execution and describe the state of your program.
;; It separates the input/output of your program from that of GDB and displays
;; expressions and their current values in their own buffers.  It also uses
;; features of Emacs 21 such as the fringe/display margin for breakpoints, and
;; the toolbar (see the GDB Graphical Interface section in the Emacs info
;; manual).

;; M-x gdb will start the debugger.

;; This file uses GDB/MI as the primary interface to GDB.  It runs gdb with
;; GDB/MI (-interp=mi) and access CLI using "-interpreter-exec console
;; cli-command".  This code replaces gdb-ui.el and uses MI tokens instead
;; of queues.  Eventually MI should be asynchronous.

;; Windows Platforms:

;; If you are using Emacs and GDB on Windows you will need to flush the buffer
;; explicitly in your program if you want timely display of I/O in Emacs.
;; Alternatively you can make the output stream unbuffered, for example, by
;; using a macro:

;;           #ifdef UNBUFFERED
;;	     setvbuf (stdout, (char *) NULL, _IONBF, 0);
;;	     #endif

;; and compiling with -DUNBUFFERED while debugging.

;; If you are using Cygwin GDB and find that the source is not being displayed
;; in Emacs when you step through it, possible solutions are to:

;;   1) Use Cygwin X Windows and Cygwin Emacs.
;;        (Since 22.1 Emacs builds under Cygwin.)
;;   2) Use MinGW GDB instead.
;;   3) Use cygwin-mount.el

;;; Mac OSX:

;; GDB in Emacs on Mac OSX works best with FSF GDB as Apple have made
;; some changes to the version that they include as part of Mac OSX.
;; This requires GDB version 7.0 or later (estimated release date Aug 2009)
;; as earlier versions do not compile on Mac OSX.

;;; Known Bugs:

;; 1) Stack buffer doesn't parse MI output if you stop in a routine without
;;    line information, e.g., a routine in libc (just a TODO item).

;; TODO:
;; 2) Watch windows to work with threads.
;; 3) Use treebuffer.el instead of the speedbar for watch-expressions?
;; 4) Mark breakpoint locations on scroll-bar of source buffer?

;;; Code:

(require 'gud)
(require 'json)
(require 'bindat)
(eval-when-compile (require 'cl))

(declare-function speedbar-change-initial-expansion-list
                  "speedbar" (new-default))
(declare-function speedbar-timer-fn "speedbar" ())
(declare-function speedbar-line-text "speedbar" (&optional p))
(declare-function speedbar-change-expand-button-char "speedbar" (char))
(declare-function speedbar-delete-subblock "speedbar" (indent))
(declare-function speedbar-center-buffer-smartly "speedbar" ())

(defvar tool-bar-map)
(defvar speedbar-initial-expansion-list-name)
(defvar speedbar-frame)

(defvar	gdb-memory-address "main")
(defvar	gdb-memory-last-address nil
  "Last successfully accessed memory address.")
(defvar	gdb-memory-next-page nil
  "Address of next memory page for program memory buffer.")
(defvar	gdb-memory-prev-page nil
  "Address of previous memory page for program memory buffer.")

(defvar gdb-thread-number nil
  "Main current thread.

Invalidation triggers use this variable to query GDB for
information on the specified thread by wrapping GDB/MI commands
in `gdb-current-context-command'.

This variable may be updated implicitly by GDB via `gdb-stopped'
or explicitly by `gdb-select-thread'.

Only `gdb-setq-thread-number' should be used to change this
value.")

(defvar gdb-frame-number nil
  "Selected frame level for main current thread.

Updated according to the following rules:

When a thread is selected or current thread stops, set to \"0\".

When current thread goes running (and possibly exits eventually),
set to nil.

May be manually changed by user with `gdb-select-frame'.")

(defvar gdb-frame-address nil "Identity of frame for watch expression.")

;; Used to show overlay arrow in source buffer. All set in
;; gdb-get-main-selected-frame. Disassembly buffer should not use
;; these but rely on buffer-local thread information instead.
(defvar gdb-selected-frame nil
  "Name of selected function for main current thread.")
(defvar gdb-selected-file nil
  "Name of selected file for main current thread.")
(defvar gdb-selected-line nil
  "Number of selected line for main current thread.")

(defvar gdb-threads-list nil
  "Associative list of threads provided by \"-thread-info\" MI command.

Keys are thread numbers (in strings) and values are structures as
returned from -thread-info by `gdb-json-partial-output'. Updated in
`gdb-thread-list-handler-custom'.")

(defvar gdb-running-threads-count nil
  "Number of currently running threads.

If nil, no information is available.

Updated in `gdb-thread-list-handler-custom'.")

(defvar gdb-stopped-threads-count nil
  "Number of currently stopped threads.

See also `gdb-running-threads-count'.")

(defvar gdb-breakpoints-list nil
  "Associative list of breakpoints provided by \"-break-list\" MI command.

Keys are breakpoint numbers (in string) and values are structures
as returned from \"-break-list\" by `gdb-json-partial-output'
\(\"body\" field is used). Updated in
`gdb-breakpoints-list-handler-custom'.")

(defvar gdb-current-language nil)
(defvar gdb-var-list nil
  "List of variables in watch window.
Each element has the form
  (VARNUM EXPRESSION NUMCHILD TYPE VALUE STATUS HAS_MORE FP)
where STATUS is nil (`unchanged'), `changed' or `out-of-scope', FP the frame
address for root variables.")
(defvar gdb-main-file nil "Source file from which program execution begins.")

;; Overlay arrow markers
(defvar gdb-stack-position nil)
(defvar gdb-thread-position nil)
(defvar gdb-disassembly-position nil)

(defvar gdb-location-alist nil
  "Alist of breakpoint numbers and full filenames.  Only used for files that
Emacs can't find.")
(defvar gdb-active-process nil
  "GUD tooltips display variable values when t, and macro definitions otherwise.")
(defvar gdb-error "Non-nil when GDB is reporting an error.")
(defvar gdb-macro-info nil
  "Non-nil if GDB knows that the inferior includes preprocessor macro info.")
(defvar gdb-register-names nil "List of register names.")
(defvar gdb-changed-registers nil
  "List of changed register numbers (strings).")
(defvar gdb-buffer-fringe-width nil)
(defvar gdb-last-command nil)
(defvar gdb-prompt-name nil)
(defvar gdb-token-number 0)
(defvar gdb-handler-alist '())
(defvar gdb-handler-number nil)
(defvar gdb-source-file-list nil
  "List of source files for the current executable.")
(defvar gdb-first-done-or-error t)
(defvar gdb-source-window nil)
(defvar gdb-inferior-status nil)
(defvar gdb-continuation nil)
(defvar gdb-supports-non-stop nil)
(defvar gdb-filter-output nil
  "Message to be shown in GUD console.

This variable is updated in `gdb-done-or-error' and returned by
`gud-gdbmi-marker-filter'.")

(defvar gdb-non-stop nil
  "Indicates whether current GDB session is using non-stop mode.

It is initialized to `gdb-non-stop-setting' at the beginning of
every GDB session.")

(defvar gdb-buffer-type nil
  "One of the symbols bound in `gdb-buffer-rules'.")
(make-variable-buffer-local 'gdb-buffer-type)

(defvar gdb-output-sink 'nil
  "The disposition of the output of the current gdb command.
Possible values are these symbols:

    `user' -- gdb output should be copied to the GUD buffer
              for the user to see.

    `emacs' -- output should be collected in the partial-output-buffer
	       for subsequent processing by a command.  This is the
	       disposition of output generated by commands that
	       gdb mode sends to gdb on its own behalf.")

;; Pending triggers prevent congestion: Emacs won't send two similar
;; consecutive requests.

(defvar gdb-pending-triggers '()
  "A list of trigger functions which have not yet been handled.

Elements are either function names or pairs (buffer . function)")

(defmacro gdb-add-pending (item)
  `(push ,item gdb-pending-triggers))
(defmacro gdb-pending-p (item)
  `(member ,item gdb-pending-triggers))
(defmacro gdb-delete-pending (item)
  `(setq gdb-pending-triggers
         (delete ,item gdb-pending-triggers)))

(defmacro gdb-wait-for-pending (&rest body)
  "Wait until `gdb-pending-triggers' is empty and evaluate FORM.

This function checks `gdb-pending-triggers' value every
`gdb-wait-for-pending' seconds."
  (run-with-timer
   0.5 nil
   `(lambda ()
      (if (not gdb-pending-triggers)
          (progn ,@body)
        (gdb-wait-for-pending ,@body)))))

;; Publish-subscribe

(defmacro gdb-add-subscriber (publisher subscriber)
  "Register new PUBLISHER's SUBSCRIBER.

SUBSCRIBER must be a pair, where cdr is a function of one
argument (see `gdb-emit-signal')."
  `(add-to-list ',publisher ,subscriber t))

(defmacro gdb-delete-subscriber (publisher subscriber)
  "Unregister SUBSCRIBER from PUBLISHER."
  `(setq ,publisher (delete ,subscriber
                            ,publisher)))

(defun gdb-get-subscribers (publisher)
  publisher)

(defun gdb-emit-signal (publisher &optional signal)
  "Call cdr for each subscriber of PUBLISHER with SIGNAL as argument."
  (dolist (subscriber (gdb-get-subscribers publisher))
    (funcall (cdr subscriber) signal)))

(defvar gdb-buf-publisher '()
  "Used to invalidate GDB buffers by emitting a signal in
`gdb-update'.

Must be a list of pairs with cars being buffers and cdr's being
valid signal handlers.")

(defgroup gdb nil
  "GDB graphical interface"
  :group 'tools
  :link '(info-link "(emacs)GDB Graphical Interface")
  :version "23.2")

(defgroup gdb-non-stop nil
  "GDB non-stop debugging settings"
  :group 'gdb
  :version "23.2")

(defgroup gdb-buffers nil
  "GDB buffers"
  :group 'gdb
  :version "23.2")

(defcustom gdb-debug-log-max 128
  "Maximum size of `gdb-debug-log'.  If nil, size is unlimited."
  :group 'gdb
  :type '(choice (integer :tag "Number of elements")
          (const   :tag "Unlimited" nil))
  :version "22.1")

(defcustom gdb-non-stop-setting t
  "When in non-stop mode, stopped threads can be examined while
other threads continue to execute.

GDB session needs to be restarted for this setting to take
effect."
  :type 'boolean
  :group 'gdb-non-stop
  :version "23.2")

;; TODO Some commands can't be called with --all (give a notice about
;; it in setting doc)
(defcustom gdb-gud-control-all-threads t
  "When enabled, GUD execution commands affect all threads when
in non-stop mode. Otherwise, only current thread is affected."
  :type 'boolean
  :group 'gdb-non-stop
  :version "23.2")

(defcustom gdb-switch-reasons t
  "List of stop reasons which cause Emacs to switch to the thread
which caused the stop. When t, switch to stopped thread no matter
what the reason was. When nil, never switch to stopped thread
automatically.

This setting is used in non-stop mode only. In all-stop mode,
Emacs always switches to the thread which caused the stop."
  ;; exited, exited-normally and exited-signaled are not
  ;; thread-specific stop reasons and therefore are not included in
  ;; this list
  :type '(choice
          (const :tag "All reasons" t)
          (set :tag "Selection of reasons..."
               (const :tag "A breakpoint was reached." "breakpoint-hit")
               (const :tag "A watchpoint was triggered." "watchpoint-trigger")
               (const :tag "A read watchpoint was triggered."
                      "read-watchpoint-trigger")
               (const :tag "An access watchpoint was triggered."
                      "access-watchpoint-trigger")
               (const :tag "Function finished execution." "function-finished")
               (const :tag "Location reached." "location-reached")
               (const :tag "Watchpoint has gone out of scope"
                      "watchpoint-scope")
               (const :tag "End of stepping range reached."
                      "end-stepping-range")
               (const :tag "Signal received (like interruption)."
                      "signal-received"))
          (const :tag "None" nil))
  :group 'gdb-non-stop
  :version "23.2"
  :link '(info-link "(gdb)GDB/MI Async Records"))

(defcustom gdb-stopped-functions nil
  "List of functions called whenever GDB stops.

Each function takes one argument, a parsed MI response, which
contains fields of corresponding MI *stopped async record:

    ((stopped-threads . \"all\")
     (thread-id . \"1\")
     (frame (line . \"38\")
            (fullname . \"/home/sphinx/projects/gsoc/server.c\")
            (file . \"server.c\")
            (args ((value . \"0x804b038\")
                   (name . \"arg\")))
            (func . \"hello\")
            (addr . \"0x0804869e\"))
     (reason . \"end-stepping-range\"))

Note that \"reason\" is only present in non-stop debugging mode.

`bindat-get-field' may be used to access the fields of response.

Each function is called after the new current thread was selected
and GDB buffers were updated in `gdb-stopped'."
  :type '(repeat function)
  :group 'gdb
  :version "23.2"
  :link '(info-link "(gdb)GDB/MI Async Records"))

(defcustom gdb-switch-when-another-stopped t
  "When nil, Emacs won't switch to stopped thread if some other
stopped thread is already selected."
  :type 'boolean
  :group 'gdb-non-stop
  :version "23.2")

(defcustom gdb-stack-buffer-locations t
  "Show file information or library names in stack buffers."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-stack-buffer-addresses nil
  "Show frame addresses in stack buffers."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-verbose-names t
  "Show long thread names in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-arguments t
  "Show function arguments in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-locations t
  "Show file information or library names in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-addresses nil
  "Show addresses for thread frames in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-show-threads-by-default nil
  "Show threads list buffer instead of breakpoints list by
default."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defvar gdb-debug-log nil
  "List of commands sent to and replies received from GDB.
Most recent commands are listed first.  This list stores only the last
`gdb-debug-log-max' values.  This variable is used to debug GDB-MI.")

;;;###autoload
(defcustom gdb-enable-debug nil
  "Non-nil means record the process input and output in `gdb-debug-log'."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-cpp-define-alist-program "gcc -E -dM -"
  "Shell command for generating a list of defined macros in a source file.
This list is used to display the #define directive associated
with an identifier as a tooltip.  It works in a debug session with
GDB, when `gud-tooltip-mode' is t.

Set `gdb-cpp-define-alist-flags' for any include paths or
predefined macros."
  :type 'string
  :group 'gdb
  :version "22.1")

(defcustom gdb-cpp-define-alist-flags ""
  "Preprocessor flags for `gdb-cpp-define-alist-program'."
  :type 'string
  :group 'gdb
  :version "22.1")

(defcustom gdb-create-source-file-list t
  "Non-nil means create a list of files from which the executable was built.
 Set this to nil if the GUD buffer displays \"initializing...\" in the mode
 line for a long time when starting, possibly because your executable was
 built from a large number of files.  This allows quicker initialization
 but means that these files are not automatically enabled for debugging,
 e.g., you won't be able to click in the fringe to set a breakpoint until
 execution has already stopped there."
  :type 'boolean
  :group 'gdb
  :version "23.1")

(defcustom gdb-show-main nil
  "Non-nil means display source file containing the main routine at startup.
Also display the main routine in the disassembly buffer if present."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defun gdb-force-mode-line-update (status)
  (let ((buffer gud-comint-buffer))
    (if (and buffer (buffer-name buffer))
	(with-current-buffer buffer
	  (setq mode-line-process
		(format ":%s [%s]"
			(process-status (get-buffer-process buffer)) status))
	  ;; Force mode line redisplay soon.
	  (force-mode-line-update)))))

(defun gdb-enable-debug (arg)
  "Toggle logging of transaction between Emacs and Gdb.
The log is stored in `gdb-debug-log' as an alist with elements
whose cons is send, send-item or recv and whose cdr is the string
being transferred.  This list may grow up to a size of
`gdb-debug-log-max' after which the oldest element (at the end of
the list) is deleted every time a new one is added (at the front)."
  (interactive "P")
  (setq gdb-enable-debug
	(if (null arg)
	    (not gdb-enable-debug)
	  (> (prefix-numeric-value arg) 0)))
  (message (format "Logging of transaction %sabled"
		   (if gdb-enable-debug "en" "dis"))))

;; These two are used for menu and toolbar
(defun gdb-control-all-threads ()
  "Switch to non-stop/A mode."
  (interactive)
  (setq gdb-gud-control-all-threads t)
  ;; Actually forcing the tool-bar to update.
  (force-mode-line-update)
  (message "Now in non-stop/A mode."))

(defun gdb-control-current-thread ()
  "Switch to non-stop/T mode."
  (interactive)
  (setq gdb-gud-control-all-threads nil)
  ;; Actually forcing the tool-bar to update.
  (force-mode-line-update)
  (message "Now in non-stop/T mode."))

(defun gdb-find-watch-expression ()
  (let* ((var (nth (- (line-number-at-pos (point)) 2) gdb-var-list))
	 (varnum (car var)) expr)
    (string-match "\\(var[0-9]+\\)\\.\\(.*\\)" varnum)
    (let ((var1 (assoc (match-string 1 varnum) gdb-var-list)) var2 varnumlet
	  (component-list (split-string (match-string 2 varnum) "\\." t)))
      (setq expr (nth 1 var1))
      (setq varnumlet (car var1))
      (dolist (component component-list)
	(setq var2 (assoc varnumlet gdb-var-list))
	(setq expr (concat expr
			   (if (string-match ".*\\[[0-9]+\\]$" (nth 3 var2))
			       (concat "[" component "]")
			     (concat "." component))))
	(setq varnumlet (concat varnumlet "." component)))
      expr)))

;; noall is used for commands which don't take --all, but only
;; --thread.
(defun gdb-gud-context-command (command &optional noall)
  "When `gdb-non-stop' is t, add --thread option to COMMAND if
`gdb-gud-control-all-threads' is nil and --all option otherwise.
If NOALL is t, always add --thread option no matter what
`gdb-gud-control-all-threads' value is.

When `gdb-non-stop' is nil, return COMMAND unchanged."
  (if gdb-non-stop
      (if (and gdb-gud-control-all-threads
               (not noall)
	       gdb-supports-non-stop)
          (concat command " --all ")
        (gdb-current-context-command command))
    command))

(defmacro gdb-gud-context-call (cmd1 &optional cmd2 noall noarg)
  "`gud-call' wrapper which adds --thread/--all options between
CMD1 and CMD2. NOALL is the same as in `gdb-gud-context-command'.

NOARG must be t when this macro is used outside `gud-def'"
  `(gud-call
    (concat (gdb-gud-context-command ,cmd1 ,noall) " " ,cmd2)
    ,(when (not noarg) 'arg)))

(defun gdb--check-interpreter (proc string)
  (unless (zerop (length string))
    (let ((filter (process-get proc 'gud-normal-filter)))
      (set-process-filter proc filter)
      (unless (memq (aref string 0) '(?^ ?~ ?@ ?& ?* ?=))
        ;; Apparently we're not running with -i=mi.
        (let ((msg "Error: you did not specify -i=mi on GDB's command line!"))
          (message msg)
          (setq string (concat (propertize msg 'font-lock-face 'error)
                               "\n" string)))
        ;; Use the old gud-gbd filter, not because it works, but because it
        ;; will properly display GDB's answers rather than hanging waiting for
        ;; answers that aren't coming.
        (set (make-local-variable 'gud-marker-filter) #'gud-gdb-marker-filter))
      (funcall filter proc string))))

(defvar gdb-control-level 0)

;;;###autoload
(defun gdb (command-line)
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

COMMAND-LINE is the shell command for starting the gdb session.
It should be a string consisting of the name of the gdb
executable followed by command-line options.  The command-line
options should include \"-i=mi\" to use gdb's MI text interface.
Note that the old \"--annotate\" option is no longer supported.

If `gdb-many-windows' is nil (the default value) then gdb just
pops up the GUD buffer unless `gdb-show-main' is t.  In this case
it starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the inferior.

If `gdb-many-windows' is t, regardless of the value of
`gdb-show-main', the layout below will appear.  Keybindings are
shown in some of the buffers.

Watch expressions appear in the speedbar/slowbar.

The following commands help control operation :

`gdb-many-windows'    - Toggle the number of windows gdb uses.
`gdb-restore-windows' - To restore the window layout.

See Info node `(emacs)GDB Graphical Interface' for a more
detailed description of this mode.


+----------------------------------------------------------------------+
|                               GDB Toolbar                            |
+-----------------------------------+----------------------------------+
| GUD buffer (I/O of GDB)           | Locals buffer                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Source buffer                     | I/O buffer (of debugged program) |
|                                   | (comint-mode)                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET      gdb-select-frame         | SPC    gdb-toggle-breakpoint     |
|                                   | RET    gdb-goto-breakpoint       |
|                                   | D      gdb-delete-breakpoint     |
+-----------------------------------+----------------------------------+"
  ;;
  (interactive (list (gud-query-cmdline 'gdb)))

  (when (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
    (gdb-restore-windows)
    (error
     "Multiple debugging requires restarting in text command mode"))
  ;;
  (gud-common-init command-line nil 'gud-gdbmi-marker-filter)

  ;; Setup a temporary process filter to warn when GDB was not started
  ;; with -i=mi.
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (process-put proc 'gud-normal-filter (process-filter proc))
    (set-process-filter proc #'gdb--check-interpreter))

  (set (make-local-variable 'gud-minor-mode) 'gdbmi)
  (set (make-local-variable 'gdb-control-level) 0)
  (setq comint-input-sender 'gdb-send)
  (when (ring-empty-p comint-input-ring) ; cf shell-mode
    (let ((hfile (expand-file-name (or (getenv "GDBHISTFILE")
				       (if (eq system-type 'ms-dos)
					   "_gdb_history"
					 ".gdb_history"))))
	  ;; gdb defaults to 256, but we'll default to comint-input-ring-size.
	  (hsize (getenv "HISTSIZE")))
      (dolist (file (append '("~/.gdbinit")
			    (unless (string-equal (expand-file-name ".")
                                                  (expand-file-name "~"))
			      '(".gdbinit"))))
	(if (file-readable-p (setq file (expand-file-name file)))
	    (with-temp-buffer
	      (insert-file-contents file)
	      ;; TODO? check for "set history save\\(  *on\\)?" and do
	      ;; not use history otherwise?
	      (while (re-search-forward
		      "^ *set history \\(filename\\|size\\)  *\\(.*\\)" nil t)
		(cond ((string-equal (match-string 1) "filename")
		       (setq hfile (expand-file-name
				    (match-string 2)
				    (file-name-directory file))))
		      ((string-equal (match-string 1) "size")
		       (setq hsize (match-string 2))))))))
      (and (stringp hsize)
	   (integerp (setq hsize (string-to-number hsize)))
	   (> hsize 0)
	   (set (make-local-variable 'comint-input-ring-size) hsize))
      (if (stringp hfile)
	  (set (make-local-variable 'comint-input-ring-file-name) hfile))
      (comint-read-input-ring t)))
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t"
	   "Set temporary breakpoint at current line.")
  (gud-def gud-jump
	   (progn (gud-call "tbreak %f:%l") (gud-call "jump %f:%l"))
	   "\C-j" "Set execution address to current line.")

  (gud-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "print* %e" nil
	   "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-step   (gdb-gud-context-call "-exec-step" "%p" t)
           "\C-s"
	   "Step one source line with display.")
  (gud-def gud-stepi  (gdb-gud-context-call "-exec-step-instruction" "%p" t)
           "\C-i"
	   "Step one instruction with display.")
  (gud-def gud-next   (gdb-gud-context-call "-exec-next" "%p" t)
           "\C-n"
	   "Step one line (skip functions).")
  (gud-def gud-nexti  (gdb-gud-context-call "-exec-next-instruction" "%p" t)
           nil
	   "Step one instruction (skip functions).")
  (gud-def gud-cont   (gdb-gud-context-call "-exec-continue")
           "\C-r"
	   "Continue with display.")
  (gud-def gud-finish (gdb-gud-context-call "-exec-finish" nil t)
           "\C-f"
	   "Finish executing current function.")
  (gud-def gud-run    "-exec-run"
           nil
           "Run the program.")

  (gud-def gud-break (if (not (string-match "Disassembly" mode-name))
			 (gud-call "break %f:%l" arg)
		       (save-excursion
			 (beginning-of-line)
			 (forward-char 2)
			 (gud-call "break *%a" arg)))
	   "\C-b" "Set breakpoint at current line or address.")

  (gud-def gud-remove (if (not (string-match "Disassembly" mode-name))
			  (gud-call "clear %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "clear *%a" arg)))
	   "\C-d" "Remove breakpoint at current line or address.")

  ;; -exec-until doesn't support --all yet
  (gud-def gud-until  (if (not (string-match "Disassembly" mode-name))
			  (gud-call "-exec-until %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "-exec-until *%a" arg)))
	   "\C-u" "Continue to current line or address.")
  ;; TODO Why arg here?
  (gud-def
   gud-go (gud-call (if gdb-active-process
                        (gdb-gud-context-command "-exec-continue")
                      "-exec-run") arg)
   nil "Start or continue execution.")

  ;; For debugging Emacs only.
  (gud-def gud-pp
	   (gud-call
	    (concat
	     "pp " (if (eq (buffer-local-value
			    'major-mode (window-buffer)) 'speedbar-mode)
		       (gdb-find-watch-expression) "%e")) arg)
	   nil   "Print the Emacs s-expression.")

  (define-key gud-minor-mode-map [left-margin mouse-1]
    'gdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-fringe mouse-1]
    'gdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-margin C-mouse-1]
    'gdb-mouse-toggle-breakpoint-margin)
  (define-key gud-minor-mode-map [left-fringe C-mouse-1]
    'gdb-mouse-toggle-breakpoint-fringe)

  (define-key gud-minor-mode-map [left-margin drag-mouse-1]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe drag-mouse-1]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-margin mouse-3]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe mouse-3]
    'gdb-mouse-until)

  (define-key gud-minor-mode-map [left-margin C-drag-mouse-1]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-drag-mouse-1]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-mouse-3]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-margin C-mouse-3]
    'gdb-mouse-jump)

  (set (make-local-variable 'gud-gdb-completion-function)
       'gud-gdbmi-completions)

  (add-hook 'completion-at-point-functions #'gud-gdb-completion-at-point
            nil 'local)
  (local-set-key "\C-i" 'completion-at-point)

  (local-set-key [remap comint-delchar-or-maybe-eof] 'gdb-delchar-or-quit)

  (setq gdb-first-prompt t)
  (setq gud-running nil)

  (gdb-update)

  (run-hooks 'gdb-mode-hook))

(defun gdb-init-1 ()
  ;; (re-)initialize
  (setq gdb-selected-frame nil
	gdb-frame-number nil
        gdb-thread-number nil
	gdb-var-list nil
	gdb-pending-triggers nil
	gdb-output-sink 'user
	gdb-location-alist nil
	gdb-source-file-list nil
	gdb-last-command nil
	gdb-token-number 0
	gdb-handler-alist '()
	gdb-handler-number nil
	gdb-prompt-name nil
	gdb-first-done-or-error t
	gdb-buffer-fringe-width (car (window-fringes))
	gdb-debug-log nil
	gdb-source-window nil
	gdb-inferior-status nil
	gdb-continuation nil
        gdb-buf-publisher '()
        gdb-threads-list '()
        gdb-breakpoints-list '()
        gdb-register-names '()
        gdb-non-stop gdb-non-stop-setting)
  ;;
  (setq gdb-buffer-type 'gdbmi)
  ;;
  (gdb-force-mode-line-update
   (propertize "initializing..." 'face font-lock-variable-name-face))

  (gdb-get-buffer-create 'gdb-inferior-io)
  (gdb-clear-inferior-io)
  (gdb-inferior-io--init-proc (get-process "gdb-inferior"))

  (if (eq window-system 'w32)
      (gdb-input "-gdb-set new-console off" 'ignore))
  (gdb-input "-gdb-set height 0" 'ignore)

  (when gdb-non-stop
    (gdb-input "-gdb-set non-stop 1" 'gdb-non-stop-handler))

  (gdb-input "-enable-pretty-printing" 'ignore)

  ;; find source file and compilation directory here
  (if gdb-create-source-file-list
      ;; Needs GDB 6.2 onwards.
      (gdb-input "-file-list-exec-source-files" 'gdb-get-source-file-list))
  ;; Needs GDB 6.0 onwards.
  (gdb-input "-file-list-exec-source-file" 'gdb-get-source-file)
  (gdb-input "-gdb-show prompt" 'gdb-get-prompt))

(defun gdb-non-stop-handler ()
  (goto-char (point-min))
  (if (re-search-forward "No symbol" nil t)
      (progn
	(message
         "This version of GDB doesn't support non-stop mode.  Turning it off.")
	(setq gdb-non-stop nil)
	(setq gdb-supports-non-stop nil))
    (setq gdb-supports-non-stop t)
    (gdb-input "-gdb-set target-async 1" 'ignore)
    (gdb-input "-list-target-features" 'gdb-check-target-async)))

(defun gdb-check-target-async ()
  (goto-char (point-min))
  (unless (re-search-forward "async" nil t)
    (message
     "Target doesn't support non-stop mode.  Turning it off.")
    (setq gdb-non-stop nil)
    (gdb-input "-gdb-set non-stop 0" 'ignore)))

(defun gdb-delchar-or-quit (arg)
  "Delete ARG characters or send a quit command to GDB.
Send a quit only if point is at the end of the buffer, there is
no input, and GDB is waiting for input."
  (interactive "p")
  (unless (and (eq (current-buffer) gud-comint-buffer)
	       (eq gud-minor-mode 'gdbmi))
    (error "Not in a GDB-MI buffer"))
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (if (and (eobp) proc (process-live-p proc)
	     (not gud-running)
	     (= (point) (marker-position (process-mark proc))))
	;; Sending an EOF does not work with GDB-MI; submit an
	;; explicit quit command.
	(progn
	  (insert "quit")
	  (comint-send-input t t))
      (delete-char arg))))

(defvar gdb-define-alist nil "Alist of #define directives for GUD tooltips.")

(defun gdb-create-define-alist ()
  "Create an alist of #define directives for GUD tooltips."
  (let* ((file (buffer-file-name))
	 (output
	  (with-output-to-string
	    (with-current-buffer standard-output
 	      (and file
		   (file-exists-p file)
 		   ;; call-process doesn't work with remote file names.
		   (not (file-remote-p default-directory))
 		   (call-process shell-file-name file
				 (list t nil) nil "-c"
				 (concat gdb-cpp-define-alist-program " "
					 gdb-cpp-define-alist-flags))))))
         (define-list (split-string output "\n" t))
         (name))
    (setq gdb-define-alist nil)
    (dolist (define define-list)
      (setq name (nth 1 (split-string define "[( ]")))
      (push (cons name define) gdb-define-alist))))

(declare-function tooltip-show "tooltip" (text &optional use-echo-area))

(defun gdb-tooltip-print (expr)
  (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (if (re-search-forward ".*value=\\(\".*\"\\)" nil t)
        (tooltip-show
         (concat expr " = " (read (match-string 1)))
         (or gud-tooltip-echo-area
             (not (display-graphic-p)))))))

;; If expr is a macro for a function don't print because of possible dangerous
;; side-effects. Also printing a function within a tooltip generates an
;; unexpected starting annotation (phase error).
(defun gdb-tooltip-print-1 (expr)
  (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (if (search-forward "expands to: " nil t)
	(unless (looking-at "\\S-+.*(.*).*")
	  (gdb-input (concat "-data-evaluate-expression " expr)
		     `(lambda () (gdb-tooltip-print ,expr)))))))

(defun gdb-init-buffer ()
  (set (make-local-variable 'gud-minor-mode) 'gdbmi)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (when gud-tooltip-mode
    (make-local-variable 'gdb-define-alist)
    (gdb-create-define-alist)
    (add-hook 'after-save-hook 'gdb-create-define-alist nil t)))

(defmacro gdb-if-arrow (arrow-position &rest body)
  `(if ,arrow-position
       (let ((buffer (marker-buffer ,arrow-position)) (line))
         (if (equal buffer (window-buffer (posn-window end)))
             (with-current-buffer buffer
               (when (or (equal start end)
                         (equal (posn-point start)
                                (marker-position ,arrow-position)))
                 ,@body))))))

(defun gdb-mouse-until (event)
  "Continue running until a source line past the current line.
The destination source line can be selected either by clicking
with mouse-3 on the fringe/margin or dragging the arrow
with mouse-1 (default bindings)."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (gdb-if-arrow gud-overlay-arrow-position
		  (setq line (line-number-at-pos (posn-point end)))
		  (gud-call (concat "until " (number-to-string line))))
    (gdb-if-arrow gdb-disassembly-position
		  (save-excursion
		    (goto-char (point-min))
		    (forward-line (1- (line-number-at-pos (posn-point end))))
		    (forward-char 2)
		    (gud-call (concat "until *%a"))))))

(defun gdb-mouse-jump (event)
  "Set execution address/line.
The destination source line can be selected either by clicking with C-mouse-3
on the fringe/margin or dragging the arrow with C-mouse-1 (default bindings).
Unlike `gdb-mouse-until' the destination address can be before the current
line, and no execution takes place."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (gdb-if-arrow gud-overlay-arrow-position
		  (setq line (line-number-at-pos (posn-point end)))
		  (progn
		    (gud-call (concat "tbreak " (number-to-string line)))
		    (gud-call (concat "jump " (number-to-string line)))))
    (gdb-if-arrow gdb-disassembly-position
		  (save-excursion
		    (goto-char (point-min))
		    (forward-line (1- (line-number-at-pos (posn-point end))))
		    (forward-char 2)
		    (progn
		      (gud-call (concat "tbreak *%a"))
		      (gud-call (concat "jump *%a")))))))

(defcustom gdb-show-changed-values t
  "If non-nil change the face of out of scope variables and changed values.
Out of scope variables are suppressed with `shadow' face.
Changed values are highlighted with the face `font-lock-warning-face'."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-max-children 40
  "Maximum number of children before expansion requires confirmation."
  :type 'integer
  :group 'gdb
  :version "22.1")

(defcustom gdb-delete-out-of-scope t
  "If non-nil delete watch expressions automatically when they go out of scope."
  :type 'boolean
  :group 'gdb
  :version "22.2")

(defcustom gdb-speedbar-auto-raise nil
  "If non-nil raise speedbar every time display of watch expressions is\
 updated."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-use-colon-colon-notation nil
  "If non-nil use FUN::VAR format to display variables in the speedbar."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defun gdb-speedbar-auto-raise (arg)
  "Toggle automatic raising of the speedbar for watch expressions.
With prefix argument ARG, automatically raise speedbar if ARG is
positive, otherwise don't automatically raise it."
  (interactive "P")
  (setq gdb-speedbar-auto-raise
	(if (null arg)
	    (not gdb-speedbar-auto-raise)
	  (> (prefix-numeric-value arg) 0)))
  (message (format "Auto raising %sabled"
		   (if gdb-speedbar-auto-raise "en" "dis"))))

(define-key gud-minor-mode-map "\C-c\C-w" 'gud-watch)
(define-key global-map (concat gud-key-prefix "\C-w") 'gud-watch)

(declare-function tooltip-identifier-from-point "tooltip" (point))

(defun gud-watch (&optional arg event)
  "Watch expression at point.
With arg, enter name of variable to be watched in the minibuffer."
  (interactive (list current-prefix-arg last-input-event))
  (let ((minor-mode (buffer-local-value 'gud-minor-mode gud-comint-buffer)))
    (if (eq minor-mode 'gdbmi)
	(progn
	  (if event (posn-set-point (event-end event)))
	  (require 'tooltip)
	  (save-selected-window
	    (let ((expr
		   (if arg
		       (completing-read "Name of variable: "
					'gud-gdb-complete-command)
		     (if (and transient-mark-mode mark-active)
			 (buffer-substring (region-beginning) (region-end))
		       (concat (if (derived-mode-p 'gdb-registers-mode) "$")
			       (tooltip-identifier-from-point (point)))))))
	      (set-text-properties 0 (length expr) nil expr)
	      (gdb-input (concat "-var-create - * "  expr "")
			 `(lambda () (gdb-var-create-handler ,expr))))))
      (message "gud-watch is a no-op in this mode."))))

(defun gdb-var-create-handler (expr)
  (let* ((result (gdb-json-partial-output)))
    (if (not (bindat-get-field result 'msg))
        (let ((var
	       (list (bindat-get-field result 'name)
		     (if (and (string-equal gdb-current-language "c")
			      gdb-use-colon-colon-notation gdb-selected-frame)
			 (setq expr (concat gdb-selected-frame "::" expr))
		       expr)
		     (bindat-get-field result 'numchild)
		     (bindat-get-field result 'type)
		     (bindat-get-field result 'value)
		     nil
		     (bindat-get-field result 'has_more)
                     gdb-frame-address)))
	  (push var gdb-var-list)
	  (speedbar 1)
	  (unless (string-equal
		   speedbar-initial-expansion-list-name "GUD")
	    (speedbar-change-initial-expansion-list "GUD")))
      (message-box "No symbol \"%s\" in current context." expr))))

(defun gdb-speedbar-update ()
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame)
	     (not (gdb-pending-p 'gdb-speedbar-timer)))
    ;; Dummy command to update speedbar even when idle.
    (gdb-input "-environment-pwd" 'gdb-speedbar-timer-fn)
    ;; Keep gdb-pending-triggers non-nil till end.
    (gdb-add-pending 'gdb-speedbar-timer)))

(defun gdb-speedbar-timer-fn ()
  (if gdb-speedbar-auto-raise
      (raise-frame speedbar-frame))
  (gdb-delete-pending 'gdb-speedbar-timer)
  (speedbar-timer-fn))

(defun gdb-var-evaluate-expression-handler (varnum changed)
  (goto-char (point-min))
  (re-search-forward ".*value=\\(\".*\"\\)" nil t)
  (let ((var (assoc varnum gdb-var-list)))
    (when var
      (if changed (setcar (nthcdr 5 var) 'changed))
      (setcar (nthcdr 4 var) (read (match-string 1)))))
  (gdb-speedbar-update))

                                        ; Uses "-var-list-children --all-values".  Needs GDB 6.1 onwards.
(defun gdb-var-list-children (varnum)
  (gdb-input (concat "-var-update " varnum) 'ignore)
  (gdb-input (concat "-var-list-children --all-values " varnum)
	     `(lambda () (gdb-var-list-children-handler ,varnum))))

(defun gdb-var-list-children-handler (varnum)
  (let* ((var-list nil)
	 (output (bindat-get-field (gdb-json-partial-output "child")))
	 (children (bindat-get-field output 'children)))
    (catch 'child-already-watched
      (dolist (var gdb-var-list)
	(if (string-equal varnum (car var))
	    (progn
	      ;; With dynamic varobjs numchild may have increased.
	      (setcar (nthcdr 2 var) (bindat-get-field output 'numchild))
	      (push var var-list)
	      (dolist (child children)
		(let ((varchild (list (bindat-get-field child 'name)
				      (bindat-get-field child 'exp)
				      (bindat-get-field child 'numchild)
				      (bindat-get-field child 'type)
				      (bindat-get-field child 'value)
				      nil
				      (bindat-get-field child 'has_more))))
		  (if (assoc (car varchild) gdb-var-list)
		      (throw 'child-already-watched nil))
		  (push varchild var-list))))
	  (push var var-list)))
      (setq gdb-var-list (nreverse var-list))))
  (gdb-speedbar-update))

(defun gdb-var-set-format (format)
  "Set the output format for a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var)))
    (gdb-input (concat "-var-set-format " varnum " " format) 'ignore)
    (gdb-var-update)))

(defun gdb-var-delete-1 (var varnum)
  (gdb-input (concat "-var-delete " varnum) 'ignore)
  (setq gdb-var-list (delq var gdb-var-list))
  (dolist (varchild gdb-var-list)
    (if (string-match (concat (car var) "\\.") (car varchild))
	(setq gdb-var-list (delq varchild gdb-var-list)))))

(defun gdb-var-delete ()
  "Delete watch expression at point from the speedbar."
  (interactive)
  (let ((text (speedbar-line-text)))
    (string-match "\\(\\S-+\\)" text)
    (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
           (varnum (car var)))
      (if (string-match "\\." (car var))
          (message-box "Can only delete a root expression")
        (gdb-var-delete-1 var varnum)))))

(defun gdb-var-delete-children (varnum)
  "Delete children of variable object at point from the speedbar."
  (gdb-input (concat "-var-delete -c " varnum) 'ignore))

(defun gdb-edit-value (_text _token _indent)
  "Assign a value to a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var)) (value))
    (setq value (read-string "New value: "))
    (gdb-input (concat "-var-assign " varnum " " value)
	       `(lambda () (gdb-edit-value-handler ,value)))))

(defconst gdb-error-regexp "\\^error,msg=\\(\".+\"\\)")

(defun gdb-edit-value-handler (value)
  (goto-char (point-min))
  (if (re-search-forward gdb-error-regexp nil t)
      (message-box "Invalid number or expression (%s)" value)))

                                        ; Uses "-var-update --all-values".  Needs GDB 6.4 onwards.
(defun gdb-var-update ()
  (if (not (gdb-pending-p 'gdb-var-update))
      (gdb-input "-var-update --all-values *" 'gdb-var-update-handler))
  (gdb-add-pending 'gdb-var-update))

(defun gdb-var-update-handler ()
  (let ((changelist (bindat-get-field (gdb-json-partial-output) 'changelist)))
    (dolist (var gdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (let ((temp-var-list gdb-var-list))
      (dolist (change changelist)
	(let* ((varnum (bindat-get-field change 'name))
	       (var (assoc varnum gdb-var-list))
	       (new-num (bindat-get-field change 'new_num_children)))
	  (when var
	    (let ((scope (bindat-get-field change 'in_scope))
		  (has-more (bindat-get-field change 'has_more)))
	      (cond ((string-equal scope "false")
		     (if gdb-delete-out-of-scope
			 (gdb-var-delete-1 var varnum)
		       (setcar (nthcdr 5 var) 'out-of-scope)))
		    ((string-equal scope "true")
		     (setcar (nthcdr 6 var) has-more)
		     (when (and (or (not has-more)
				    (string-equal has-more "0"))
				(not new-num)
				(string-equal (nth 2 var) "0"))
		       (setcar (nthcdr 4 var)
			       (bindat-get-field change 'value))
		       (setcar (nthcdr 5 var) 'changed)))
		    ((string-equal scope "invalid")
		     (gdb-var-delete-1 var varnum)))))
	  (let ((var-list nil) var1
		(children (bindat-get-field change 'new_children)))
	    (when new-num
              (setq var1 (pop temp-var-list))
              (while var1
                (if (string-equal varnum (car var1))
                    (let ((new (string-to-number new-num))
                          (previous (string-to-number (nth 2 var1))))
                      (setcar (nthcdr 2 var1) new-num)
                      (push var1 var-list)
                      (cond
                       ((> new previous)
                        ;; Add new children to list.
                        (dotimes (dummy previous)
                          (push (pop temp-var-list) var-list))
                        (dolist (child children)
                          (let ((varchild
                                 (list (bindat-get-field child 'name)
                                       (bindat-get-field child 'exp)
                                       (bindat-get-field child 'numchild)
                                       (bindat-get-field child 'type)
                                       (bindat-get-field child 'value)
                                       'changed
                                       (bindat-get-field child 'has_more))))
                            (push varchild var-list))))
                       ;; Remove deleted children from list.
                       ((< new previous)
                        (dotimes (dummy new)
                          (push (pop temp-var-list) var-list))
                        (dotimes (dummy (- previous new))
                          (pop temp-var-list)))))
                  (push var1 var-list))
                (setq var1 (pop temp-var-list)))
              (setq gdb-var-list (nreverse var-list))))))))
  (setq gdb-pending-triggers
	(delq 'gdb-var-update gdb-pending-triggers))
  (gdb-speedbar-update))

(defun gdb-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node.
INDENT is the current indentation depth."
  (cond ((string-match "+" text)        ;expand this node
	 (let* ((var (assoc token gdb-var-list))
		(expr (nth 1 var)) (children (nth 2 var)))
	   (if (or (<= (string-to-number children) gdb-max-children)
		   (y-or-n-p
		    (format "%s has %s children. Continue? " expr children)))
	       (gdb-var-list-children token))))
	((string-match "-" text)	;contract this node
	 (dolist (var gdb-var-list)
	   (if (string-match (concat token "\\.") (car var))
	       (setq gdb-var-list (delq var gdb-var-list))))
	 (gdb-var-delete-children token)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun gdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))


;;
;; gdb buffers.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual gdb interaction buffer is given the type `gdbmi' and
;; is constructed specially.
;;
;; Others are constructed by gdb-get-buffer-create and
;; named according to the rules set forth in the gdb-buffer-rules

(defvar gdb-buffer-rules '())

(defun gdb-rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun gdb-rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))
(defun gdb-rules-update-trigger (rules-entry)
  (nth 3 rules-entry))

(defun gdb-update-buffer-name ()
  "Rename current buffer according to name-maker associated with
it in `gdb-buffer-rules'."
  (let ((f (gdb-rules-name-maker (assoc gdb-buffer-type
                                        gdb-buffer-rules))))
    (when f (rename-buffer (funcall f)))))

(defun gdb-current-buffer-rules ()
  "Get `gdb-buffer-rules' entry for current buffer type."
  (assoc gdb-buffer-type gdb-buffer-rules))

(defun gdb-current-buffer-thread ()
  "Get thread object of current buffer from `gdb-threads-list'.

When current buffer is not bound to any thread, return main
thread."
  (cdr (assoc gdb-thread-number gdb-threads-list)))

(defun gdb-current-buffer-frame ()
  "Get current stack frame object for thread of current buffer."
  (bindat-get-field (gdb-current-buffer-thread) 'frame))

(defun gdb-buffer-type (buffer)
  "Get value of `gdb-buffer-type' for BUFFER."
  (with-current-buffer buffer
    gdb-buffer-type))

(defun gdb-buffer-shows-main-thread-p ()
  "Return t if current GDB buffer shows main selected thread and
is not bound to it."
  (current-buffer)
  (not (local-variable-p 'gdb-thread-number)))

(defun gdb-get-buffer (buffer-type &optional thread)
  "Get a specific GDB buffer.

In that buffer, `gdb-buffer-type' must be equal to BUFFER-TYPE
and `gdb-thread-number' (if provided) must be equal to THREAD."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (and (eq gdb-buffer-type buffer-type)
                   (or (not thread)
                       (equal gdb-thread-number thread)))
          (throw 'found buffer))))))

(defun gdb-get-buffer-create (buffer-type &optional thread)
  "Create a new GDB buffer of the type specified by BUFFER-TYPE.
The buffer-type should be one of the cars in `gdb-buffer-rules'.

If THREAD is non-nil, it is assigned to `gdb-thread-number'
buffer-local variable of the new buffer.

Buffer mode and name are selected according to buffer type.

If buffer has trigger associated with it in `gdb-buffer-rules',
this trigger is subscribed to `gdb-buf-publisher' and called with
'update argument."
  (or (gdb-get-buffer buffer-type thread)
      (let ((rules (assoc buffer-type gdb-buffer-rules))
            (new (generate-new-buffer "limbo")))
	(with-current-buffer new
	  (let ((mode (gdb-rules-buffer-mode rules))
                (trigger (gdb-rules-update-trigger rules)))
	    (when mode (funcall mode))
	    (setq gdb-buffer-type buffer-type)
            (when thread
              (set (make-local-variable 'gdb-thread-number) thread))
	    (set (make-local-variable 'gud-minor-mode)
		 (buffer-local-value 'gud-minor-mode gud-comint-buffer))
	    (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
            (rename-buffer (funcall (gdb-rules-name-maker rules)))
	    (when trigger
              (gdb-add-subscriber gdb-buf-publisher
                                  (cons (current-buffer)
                                        (gdb-bind-function-to-buffer
                                         trigger (current-buffer))))
              (funcall trigger 'start))
            (current-buffer))))))

(defun gdb-bind-function-to-buffer (expr buffer)
  "Return a function which will evaluate EXPR in BUFFER."
  `(lambda (&rest args)
     (with-current-buffer ,buffer
       (apply ',expr args))))

;; Used to define all gdb-frame-*-buffer functions except
;; `gdb-frame-io-buffer'
(defmacro def-gdb-frame-for-buffer (name buffer &optional doc)
  "Define a function NAME which shows gdb BUFFER in a separate frame.

DOC is an optional documentation string."
  `(defun ,name (&optional thread)
     ,(when doc doc)
     (interactive)
     (let ((special-display-regexps (append special-display-regexps '(".*")))
           (special-display-frame-alist gdb-frame-parameters))
       (display-buffer (gdb-get-buffer-create ,buffer thread)))))

(defmacro def-gdb-display-buffer (name buffer &optional doc)
  "Define a function NAME which shows gdb BUFFER.

DOC is an optional documentation string."
  `(defun ,name (&optional thread)
     ,(when doc doc)
     (interactive)
     (gdb-display-buffer
      (gdb-get-buffer-create ,buffer thread) t)))

;; Used to display windows with thread-bound buffers
(defmacro def-gdb-preempt-display-buffer (name buffer &optional doc
					       split-horizontal)
  `(defun ,name (&optional thread)
     ,(when doc doc)
     (message thread)
     (gdb-preempt-existing-or-display-buffer
      (gdb-get-buffer-create ,buffer thread)
      ,split-horizontal)))

;; This assoc maps buffer type symbols to rules.  Each rule is a list of
;; at least one and possible more functions.  The functions have these
;; roles in defining a buffer type:
;;
;;     NAME - Return a name for this  buffer type.
;;
;; The remaining function(s) are optional:
;;
;;     MODE - called in a new buffer with no arguments, should establish
;;	      the proper mode for the buffer.
;;

(defun gdb-set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type gdb-buffer-rules)))
    (if binding
	(setcdr binding rules)
      (push (cons buffer-type rules)
	    gdb-buffer-rules))))

(defun gdb-parent-mode ()
  "Generic mode to derive all other GDB buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  ;; Delete buffer from gdb-buf-publisher when it's killed
  ;; (if it has an associated update trigger)
  (add-hook
   'kill-buffer-hook
   (function
    (lambda ()
      (let ((trigger (gdb-rules-update-trigger
                      (gdb-current-buffer-rules))))
        (when trigger
          (gdb-delete-subscriber
           gdb-buf-publisher
           ;; This should match gdb-add-subscriber done in
           ;; gdb-get-buffer-create
           (cons (current-buffer)
                 (gdb-bind-function-to-buffer trigger (current-buffer))))))))
   nil t))

;; Partial-output buffer : This accumulates output from a command executed on
;; behalf of emacs (rather than the user).
;;
(gdb-set-buffer-rules 'gdb-partial-output-buffer
		      'gdb-partial-output-name)

(defun gdb-partial-output-name ()
  (concat " *partial-output-"
	  (gdb-get-target-string)
	  "*"))


(gdb-set-buffer-rules 'gdb-inferior-io
		      'gdb-inferior-io-name
		      'gdb-inferior-io-mode)

(defun gdb-inferior-io-name ()
  (concat "*input/output of "
	  (gdb-get-target-string)
	  "*"))

(defun gdb-display-io-buffer ()
  "Display IO of debugged program in a separate window."
  (interactive)
  (gdb-display-buffer
   (gdb-get-buffer-create 'gdb-inferior-io) t))

(defun gdb-inferior-io--init-proc (proc)
  ;; Set up inferior I/O.  Needs GDB 6.4 onwards.
  (set-process-filter proc 'gdb-inferior-filter)
  (set-process-sentinel proc 'gdb-inferior-io-sentinel)
  (gdb-input
   (concat "-inferior-tty-set "
	   ;; The process can run on a remote host.
	   (or (process-get proc 'remote-tty)
	       (process-tty-name proc)))
   'ignore))

(defun gdb-inferior-io-sentinel (proc str)
  (when (eq (process-status proc) 'failed)
    ;; When the debugged process exits, Emacs gets an EIO error on
    ;; read from the pty, and stops listening to it.  If the gdb
    ;; process is still running, remove the pty, make a new one, and
    ;; pass it to gdb.
    (let ((gdb-proc (get-buffer-process gud-comint-buffer))
	  (io-buffer (process-buffer proc)))
      (when (and gdb-proc (process-live-p gdb-proc)
		 (buffer-live-p io-buffer))
	;; `comint-exec' deletes the original process as a side effect.
	(comint-exec io-buffer "gdb-inferior" nil nil nil)
	(gdb-inferior-io--init-proc (get-buffer-process io-buffer))))))

(defconst gdb-frame-parameters
  '((height . 14) (width . 80)
    (unsplittable . t)
    (tool-bar-lines . nil)
    (menu-bar-lines . nil)
    (minibuffer . nil)))

(defun gdb-frame-io-buffer ()
  "Display IO of debugged program in a new frame."
  (interactive)
  (let ((special-display-regexps (append special-display-regexps '(".*")))
	(special-display-frame-alist gdb-frame-parameters))
    (display-buffer (gdb-get-buffer-create 'gdb-inferior-io))))

(defvar gdb-inferior-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gdb-io-interrupt)
    (define-key map "\C-c\C-z" 'gdb-io-stop)
    (define-key map "\C-c\C-\\" 'gdb-io-quit)
    (define-key map "\C-c\C-d" 'gdb-io-eof)
    (define-key map "\C-d" 'gdb-io-eof)
    map))

;; We want to use comint because it has various nifty and familiar features.
(define-derived-mode gdb-inferior-io-mode comint-mode "Inferior I/O"
  "Major mode for gdb inferior-io."
  :syntax-table nil :abbrev-table nil
  (make-comint-in-buffer "gdb-inferior" (current-buffer) nil))

(defun gdb-inferior-filter (proc string)
  (unless (string-equal string "")
    (gdb-display-buffer (gdb-get-buffer-create 'gdb-inferior-io) t))
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (comint-output-filter proc string)))

(defun gdb-io-interrupt ()
  "Interrupt the program being debugged."
  (interactive)
  (interrupt-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-io-quit ()
  "Send quit signal to the program being debugged."
  (interactive)
  (quit-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-io-stop ()
  "Stop the program being debugged."
  (interactive)
  (stop-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-io-eof ()
  "Send end-of-file to the program being debugged."
  (interactive)
  (process-send-eof
   (get-buffer-process gud-comint-buffer)))

(defun gdb-clear-inferior-io ()
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (erase-buffer)))


(defconst breakpoint-xpm-data
  "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint icon.")

(defconst breakpoint-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

(defconst breakpoint-disabled-pbm-data
  "P1
10 10\",
0 0 1 0 1 0 1 0 0 0
0 1 0 1 0 1 0 1 0 0
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
0 0 1 0 1 0 1 0 1 0
0 0 0 1 0 1 0 1 0 0"
  "PBM data used for disabled breakpoint icon.")

(defvar breakpoint-enabled-icon nil
  "Icon for enabled breakpoint in display margin.")

(defvar breakpoint-disabled-icon nil
  "Icon for disabled breakpoint in display margin.")

(declare-function define-fringe-bitmap "fringe.c"
		  (bitmap bits &optional height width align))

(and (display-images-p)
     ;; Bitmap for breakpoint in fringe
     (define-fringe-bitmap 'breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
     ;; Bitmap for gud-overlay-arrow in fringe
     (define-fringe-bitmap 'hollow-right-triangle
       "\xe0\x90\x88\x84\x84\x88\x90\xe0"))

(defface breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'gdb)

(defface breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    ;; Ensure that on low-color displays that we end up something visible.
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'gdb)


(defvar gdb-control-commands-regexp
  (concat
   "^\\("
   "commands\\|if\\|while\\|define\\|document\\|python\\|"
   "while-stepping\\|stepping\\|ws\\|actions"
   "\\)\\([[:blank:]]+.*\\)?$")
  "Regexp matching GDB commands that enter a recursive reading loop.
As long as GDB is in the recursive reading loop, it does not expect
commands to be prefixed by \"-interpreter-exec console\".")

(defun gdb-send (proc string)
  "A comint send filter for gdb."
  (with-current-buffer gud-comint-buffer
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max) '(face))))
  ;; mimic <RET> key to repeat previous command in GDB
  (if (not (string= "" string))
      (setq gdb-last-command string)
    (if gdb-last-command (setq string gdb-last-command)))
  (if (or (string-match "^-" string)
	  (> gdb-control-level 0))
      ;; Either MI command or we are feeding GDB's recursive reading loop.
      (progn
	(setq gdb-first-done-or-error t)
	(process-send-string proc (concat string "\n"))
	(if (and (string-match "^end$" string)
		 (> gdb-control-level 0))
	    (setq gdb-control-level (1- gdb-control-level))))
    ;; CLI command
    (if (string-match "\\\\$" string)
	(setq gdb-continuation (concat gdb-continuation string "\n"))
      (setq gdb-first-done-or-error t)
      (let ((to-send (concat "-interpreter-exec console "
                             (gdb-mi-quote string)
                             "\n")))
        (if gdb-enable-debug
            (push (cons 'mi-send to-send) gdb-debug-log))
        (process-send-string proc to-send))
      (if (and (string-match "^end$" string)
	       (> gdb-control-level 0))
	  (setq gdb-control-level (1- gdb-control-level)))
      (setq gdb-continuation nil)))
  (if (string-match gdb-control-commands-regexp string)
      (setq gdb-control-level (1+ gdb-control-level))))

(defun gdb-mi-quote (string)
  "Return STRING quoted properly as an MI argument.
The string is enclosed in double quotes.
All embedded quotes, newlines, and backslashes are preceded with a backslash."
  (setq string (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\&" string))
  (setq string (replace-regexp-in-string "\n" "\\n" string t t))
  (concat "\"" string "\""))

(defun gdb-input (command handler-function)
  "Send COMMAND to GDB via the MI interface.
Run the function HANDLER-FUNCTION, with no arguments, once the command is
complete."
  (if gdb-enable-debug (push (list 'send-item command handler-function)
			     gdb-debug-log))
  (setq gdb-token-number (1+ gdb-token-number))
  (setq command (concat (number-to-string gdb-token-number) command))
  (push (cons gdb-token-number handler-function) gdb-handler-alist)
  (process-send-string (get-buffer-process gud-comint-buffer)
		       (concat command "\n")))

;; NOFRAME is used for gud execution control commands
(defun gdb-current-context-command (command)
  "Add --thread to gdb COMMAND when needed."
  (if (and gdb-thread-number
	   gdb-supports-non-stop)
      (concat command " --thread " gdb-thread-number)
    command))

(defun gdb-current-context-buffer-name (name)
  "Add thread information and asterisks to string NAME.

If `gdb-thread-number' is nil, just wrap NAME in asterisks."
  (concat "*" name
          (if (local-variable-p 'gdb-thread-number)
              (format " (bound to thread %s)" gdb-thread-number)
            "")
          "*"))

(defun gdb-current-context-mode-name (mode)
  "Add thread information to MODE which is to be used as
`mode-name'."
  (concat mode
          (if gdb-thread-number
              (format " [thread %s]" gdb-thread-number)
            "")))


(defcustom gud-gdb-command-name "gdb -i=mi"
  "Default command to execute an executable under the GDB debugger."
  :type 'string
  :group 'gdb)

(defun gdb-resync()
  (setq gud-running nil)
  (setq gdb-output-sink 'user)
  (setq gdb-pending-triggers nil))

(defun gdb-update (&optional no-proc)
  "Update buffers showing status of debug session.
If NO-PROC is non-nil, do not try to contact the GDB process."
  (when gdb-first-prompt
    (gdb-force-mode-line-update
     (propertize "initializing..." 'face font-lock-variable-name-face))
    (gdb-init-1)
    (setq gdb-first-prompt nil))

  (unless no-proc
    (gdb-get-main-selected-frame))

  ;; We may need to update gdb-threads-list so we can use
  (gdb-get-buffer-create 'gdb-threads-buffer)
  ;; gdb-break-list is maintained in breakpoints handler
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)

  (unless no-proc
    (gdb-emit-signal gdb-buf-publisher 'update))

  (gdb-get-changed-registers)
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    (dolist (var gdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (gdb-var-update)))

;; gdb-setq-thread-number and gdb-update-gud-running are decoupled
;; because we may need to update current gud-running value without
;; changing current thread (see gdb-running)
(defun gdb-setq-thread-number (number)
  "Only this function must be used to change `gdb-thread-number'
value to NUMBER, because `gud-running' and `gdb-frame-number'
need to be updated appropriately when current thread changes."
  ;; GDB 6.8 and earlier always output thread-id="0" when stopping.
  (unless (string-equal number "0") (setq gdb-thread-number number))
  (setq gdb-frame-number "0")
  (gdb-update-gud-running))

(defun gdb-update-gud-running ()
  "Set `gud-running' according to the state of current thread.

`gdb-frame-number' is set to 0 if current thread is now stopped.

Note that when `gdb-gud-control-all-threads' is t, `gud-running'
cannot be reliably used to determine whether or not execution
control buttons should be shown in menu or toolbar. Use
`gdb-running-threads-count' and `gdb-stopped-threads-count'
instead.

For all-stop mode, thread information is unavailable while target
is running."
  (let ((old-value gud-running))
    (setq gud-running
          (string= (bindat-get-field (gdb-current-buffer-thread) 'state)
                   "running"))
    ;; Set frame number to "0" when _current_ threads stops
    (when (and (gdb-current-buffer-thread)
               (not (eq gud-running old-value)))
      (setq gdb-frame-number "0"))))

(defun gdb-show-run-p ()
  "Return t if \"Run/continue\" should be shown on the toolbar."
  (or (not gdb-active-process)
      (and (or
            (not gdb-gud-control-all-threads)
            (not gdb-non-stop))
           (not gud-running))
      (and gdb-gud-control-all-threads
           (> gdb-stopped-threads-count 0))))

(defun gdb-show-stop-p ()
  "Return t if \"Stop\" should be shown on the toolbar."
  (or (and (or
            (not gdb-gud-control-all-threads)
            (not gdb-non-stop))
           gud-running)
      (and gdb-gud-control-all-threads
           (> gdb-running-threads-count 0))))

;; GUD displays the selected GDB frame.  This might might not be the current
;; GDB frame (after up, down etc).  If no GDB frame is visible but the last
;; visited breakpoint is, use that window.
(defun gdb-display-source-buffer (buffer)
  (let* ((last-window (if gud-last-last-frame
                          (get-buffer-window
                           (gud-find-file (car gud-last-last-frame)))))
	 (source-window (or last-window
			    (if (and gdb-source-window
				     (window-live-p gdb-source-window))
				gdb-source-window))))
    (when source-window
      (setq gdb-source-window source-window)
      (set-window-buffer source-window buffer))
    source-window))

(defun gdb-car< (a b)
  (< (car a) (car b)))

(defvar gdbmi-record-list
  '((gdb-gdb . "(gdb) \n")
    (gdb-done . "\\([0-9]*\\)\\^done,?\\(.*?\\)\n")
    (gdb-starting . "\\([0-9]*\\)\\^running\n")
    (gdb-error . "\\([0-9]*\\)\\^error,\\(.*?\\)\n")
    (gdb-console . "~\\(\".*?\"\\)\n")
    (gdb-internals . "&\\(\".*?\"\\)\n")
    (gdb-stopped . "\\*stopped,?\\(.*?\\)\n")
    (gdb-running . "\\*running,\\(.*?\n\\)")
    (gdb-thread-created . "=thread-created,\\(.*?\n\\)")
    (gdb-thread-selected . "=thread-selected,\\(.*?\\)\n")
    (gdb-thread-exited . "=thread-exited,\\(.*?\n\\)")
    (gdb-ignored-notification . "=[-[:alpha:]]+,?\\(.*?\\)\n")
    (gdb-shell . "\\(\\(?:^.+\n\\)+\\)")))

(defun gud-gdbmi-marker-filter (string)
  "Filter GDB/MI output."

  ;; Record transactions if logging is enabled.
  (when gdb-enable-debug
    (push (cons 'recv string) gdb-debug-log)
    (if (and gdb-debug-log-max
	     (> (length gdb-debug-log) gdb-debug-log-max))
	(setcdr (nthcdr (1- gdb-debug-log-max) gdb-debug-log) nil)))

  ;; Recall the left over gud-marker-acc from last time
  (setq gud-marker-acc (concat gud-marker-acc string))

  ;; Start accumulating output for the GUD buffer
  (setq gdb-filter-output "")
  (let (output-record-list)

    ;; Process all the complete markers in this chunk.
    (dolist (gdbmi-record gdbmi-record-list)
      (while (string-match (cdr gdbmi-record) gud-marker-acc)
	(push (list (match-beginning 0)
		    (car gdbmi-record)
		    (match-string 1 gud-marker-acc)
		    (match-string 2 gud-marker-acc)
		    (match-end 0))
	      output-record-list)
	(setq gud-marker-acc
	      (concat (substring gud-marker-acc 0 (match-beginning 0))
		      ;; Pad with spaces to preserve position.
		      (make-string (length (match-string 0 gud-marker-acc)) 32)
		      (substring gud-marker-acc (match-end 0))))))

    (setq output-record-list (sort output-record-list 'gdb-car<))

    (dolist (output-record output-record-list)
      (let ((record-type (cadr output-record))
	    (arg1 (nth 2 output-record))
	    (arg2 (nth 3 output-record)))
	(cond ((eq record-type 'gdb-error)
	       (gdb-done-or-error arg2 arg1 'error))
	      ((eq record-type 'gdb-done)
	       (gdb-done-or-error arg2 arg1 'done))
	      ;; Suppress "No registers."  GDB 6.8 and earlier
	      ;; duplicates MI error message on internal stream.
	      ;; Don't print to GUD buffer.
	      ((not (and (eq record-type 'gdb-internals)
			 (string-equal (read arg1) "No registers.\n")))
	       (funcall record-type arg1)))))

    (setq gdb-output-sink 'user)
    ;; Remove padding.
    (string-match "^ *" gud-marker-acc)
    (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))

    gdb-filter-output))

(defun gdb-gdb (_output-field))

(defun gdb-shell (output-field)
  (let ((gdb-output-sink gdb-output-sink))
    (setq gdb-filter-output
          (concat output-field gdb-filter-output))))

(defun gdb-ignored-notification (_output-field))

;; gdb-invalidate-threads is defined to accept 'update-threads signal
(defun gdb-thread-created (_output-field))
(defun gdb-thread-exited (output-field)
  "Handle =thread-exited async record: unset `gdb-thread-number'
 if current thread exited and update threads list."
  (let* ((thread-id (bindat-get-field (gdb-json-string output-field) 'id)))
    (if (string= gdb-thread-number thread-id)
        (gdb-setq-thread-number nil))
    ;; When we continue current thread and it quickly exits,
    ;; gdb-pending-triggers left after gdb-running disallow us to
    ;; properly call -thread-info without --thread option. Thus we
    ;; need to use gdb-wait-for-pending.
    (gdb-wait-for-pending
     (gdb-emit-signal gdb-buf-publisher 'update-threads))))

(defun gdb-thread-selected (output-field)
  "Handler for =thread-selected MI output record.

Sets `gdb-thread-number' to new id."
  (let* ((result (gdb-json-string output-field))
         (thread-id (bindat-get-field result 'id)))
    (gdb-setq-thread-number thread-id)
    ;; Typing `thread N` in GUD buffer makes GDB emit `^done` followed
    ;; by `=thread-selected` notification. `^done` causes `gdb-update`
    ;; as usually. Things happen to fast and second call (from
    ;; gdb-thread-selected handler) gets cut off by our beloved
    ;; gdb-pending-triggers.
    ;; Solution is `gdb-wait-for-pending` macro: it guarantees that its
    ;; body will get executed when `gdb-pending-triggers` is empty.
    (gdb-wait-for-pending
     (gdb-update))))

(defun gdb-running (output-field)
  (let* ((thread-id
          (bindat-get-field (gdb-json-string output-field) 'thread-id)))
    ;; We reset gdb-frame-number to nil if current thread has gone
    ;; running. This can't be done in gdb-thread-list-handler-custom
    ;; because we need correct gdb-frame-number by the time
    ;; -thread-info command is sent.
    (when (or (string-equal thread-id "all")
              (string-equal thread-id gdb-thread-number))
      (setq gdb-frame-number nil)))
  (setq gdb-inferior-status "running")
  (gdb-force-mode-line-update
   (propertize gdb-inferior-status 'face font-lock-type-face))
  (when (not gdb-non-stop)
    (setq gud-running t))
  (setq gdb-active-process t)
  (gdb-emit-signal gdb-buf-publisher 'update-threads))

(defun gdb-starting (_output-field)
  ;; CLI commands don't emit ^running at the moment so use gdb-running too.
  (setq gdb-inferior-status "running")
  (gdb-force-mode-line-update
   (propertize gdb-inferior-status 'face font-lock-type-face))
  (setq gdb-active-process t)
  (setq gud-running t)
  ;; GDB doesn't seem to respond to -thread-info before first stop or
  ;; thread exit (even in non-stop mode), so this is useless.
  ;; Behavior may change in the future.
  (gdb-emit-signal gdb-buf-publisher 'update-threads))

;; -break-insert -t didn't give a reason before gdb 6.9

(defun gdb-stopped (output-field)
  "Given the contents of *stopped MI async record, select new
current thread and update GDB buffers."
  ;; Reason is available with target-async only
  (let* ((result (gdb-json-string output-field))
         (reason (bindat-get-field result 'reason))
         (thread-id (bindat-get-field result 'thread-id)))

    ;; -data-list-register-names needs to be issued for any stopped
    ;; thread
    (when (not gdb-register-names)
      (gdb-input (concat "-data-list-register-names"
			 (if gdb-supports-non-stop
			     (concat " --thread " thread-id)))
		 'gdb-register-names-handler))

;;; Don't set gud-last-frame here as it's currently done in gdb-frame-handler
;;; because synchronous GDB doesn't give these fields with CLI.
;;;     (when file
;;;       (setq
;;;        ;; Extract the frame position from the marker.
;;;        gud-last-frame (cons file
;;; 			    (string-to-number
;;; 			     (match-string 6 gud-marker-acc)))))

    (setq gdb-inferior-status (or reason "unknown"))
    (gdb-force-mode-line-update
     (propertize gdb-inferior-status 'face font-lock-warning-face))
    (if (string-equal reason "exited-normally")
	(setq gdb-active-process nil))

    ;; Select new current thread.

    ;; Don't switch if we have no reasons selected
    (when gdb-switch-reasons
      ;; Switch from another stopped thread only if we have
      ;; gdb-switch-when-another-stopped:
      (when (or gdb-switch-when-another-stopped
                (not (string= "stopped"
                              (bindat-get-field (gdb-current-buffer-thread) 'state))))
        ;; Switch if current reason has been selected or we have no
        ;; reasons
        (if (or (eq gdb-switch-reasons t)
                (member reason gdb-switch-reasons))
            (when (not (string-equal gdb-thread-number thread-id))
              (message (concat "Switched to thread " thread-id))
              (gdb-setq-thread-number thread-id))
          (message (format "Thread %s stopped" thread-id)))))

    ;; Print "(gdb)" to GUD console
    (when gdb-first-done-or-error
      (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))

    ;; In non-stop, we update information as soon as another thread gets
    ;; stopped
    (when (or gdb-first-done-or-error
              gdb-non-stop)
      ;; In all-stop this updates gud-running properly as well.
      (gdb-update)
      (setq gdb-first-done-or-error nil))
    (run-hook-with-args 'gdb-stopped-functions result)))

;; Remove the trimmings from log stream containing debugging messages
;; being produced by GDB's internals, use warning face and send to GUD
;; buffer.
(defun gdb-internals (output-field)
  (setq gdb-filter-output
	(gdb-concat-output
	 gdb-filter-output
	 (let ((error-message
		(read output-field)))
	   (put-text-property
	    0 (length error-message)
	    'face font-lock-warning-face
	    error-message)
	   error-message))))

;; Remove the trimmings from the console stream and send to GUD buffer
;; (frontend MI commands should not print to this stream)
(defun gdb-console (output-field)
  (setq gdb-filter-output
	(gdb-concat-output gdb-filter-output (read output-field))))

(defun gdb-done-or-error (output-field token-number type)
  (if (string-equal token-number "")
      ;; Output from command entered by user
      (progn
	(setq gdb-output-sink 'user)
	(setq token-number nil)
	;; MI error - send to minibuffer
	(when (eq type 'error)
          ;; Skip "msg=" from `output-field'
          (message (read (substring output-field 4)))
          ;; Don't send to the console twice.  (If it is a console error
          ;; it is also in the console stream.)
          (setq output-field nil)))
    ;; Output from command from frontend.
    (setq gdb-output-sink 'emacs))

  (gdb-clear-partial-output)

  ;; The process may already be dead (e.g. C-d at the gdb prompt).
  (let* ((proc (get-buffer-process gud-comint-buffer))
	 (no-proc (or (null proc)
		      (memq (process-status proc) '(exit signal)))))

    (when gdb-first-done-or-error
      (unless (or token-number gud-running no-proc)
	(setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))
      (gdb-update no-proc)
      (setq gdb-first-done-or-error nil))

    (setq gdb-filter-output
	  (gdb-concat-output gdb-filter-output output-field))

    (when token-number
      (with-current-buffer
	  (gdb-get-buffer-create 'gdb-partial-output-buffer)
	(funcall
	 (cdr (assoc (string-to-number token-number) gdb-handler-alist))))
      (setq gdb-handler-alist
	    (assq-delete-all token-number gdb-handler-alist)))))

(defun gdb-concat-output (so-far new)
  (cond
   ((eq gdb-output-sink 'user) (concat so-far new))
   ((eq gdb-output-sink 'emacs)
    (gdb-append-to-partial-output new)
    so-far)))

(defun gdb-append-to-partial-output (string)
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (goto-char (point-max))
    (insert string)))

(defun gdb-clear-partial-output ()
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (erase-buffer)))

(defun gdb-jsonify-buffer (&optional fix-key fix-list)
  "Prepare GDB/MI output in current buffer for parsing with `json-read'.

Field names are wrapped in double quotes and equal signs are
replaced with semicolons.

If FIX-KEY is non-nil, strip all \"FIX-KEY=\" occurrences from
partial output. This is used to get rid of useless keys in lists
in MI messages, e.g.: [key=.., key=..]. -stack-list-frames and
-break-info are examples of MI commands which issue such
responses.

If FIX-LIST is non-nil, \"FIX-LIST={..}\" is replaced with
\"FIX-LIST=[..]\" prior to parsing. This is used to fix broken
-break-info output when it contains breakpoint script field
incompatible with GDB/MI output syntax."
  (save-excursion
    (goto-char (point-min))
    (when fix-key
      (save-excursion
        (while (re-search-forward (concat "[\\[,]\\(" fix-key "=\\)") nil t)
          (replace-match "" nil nil nil 1))))
    (when fix-list
      (save-excursion
        ;; Find positions of braces which enclose broken list
        (while (re-search-forward (concat fix-list "={\"") nil t)
          (let ((p1 (goto-char (- (point) 2)))
                (p2 (progn (forward-sexp)
                           (1- (point)))))
            ;; Replace braces with brackets
            (save-excursion
              (goto-char p1)
              (delete-char 1)
              (insert "[")
              (goto-char p2)
              (delete-char 1)
              (insert "]"))))))
    (goto-char (point-min))
    (insert "{")
    (while (re-search-forward
	    "\\([[:alnum:]-_]+\\)=\\({\\|\\[\\|\"\"\\|\".*?[^\\]\"\\)" nil t)
      (replace-match "\"\\1\":\\2" nil nil))
    (goto-char (point-max))
    (insert "}")))

(defun gdb-json-read-buffer (&optional fix-key fix-list)
  "Prepare and parse GDB/MI output in current buffer with `json-read'.

FIX-KEY and FIX-LIST work as in `gdb-jsonify-buffer'."
  (gdb-jsonify-buffer fix-key fix-list)
  (save-excursion
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))

(defun gdb-json-string (string &optional fix-key fix-list)
  "Prepare and parse STRING containing GDB/MI output with `json-read'.

FIX-KEY and FIX-LIST work as in `gdb-jsonify-buffer'."
  (with-temp-buffer
    (insert string)
    (gdb-json-read-buffer fix-key fix-list)))

(defun gdb-json-partial-output (&optional fix-key fix-list)
  "Prepare and parse gdb-partial-output-buffer with `json-read'.

FIX-KEY and FIX-KEY work as in `gdb-jsonify-buffer'."
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (gdb-json-read-buffer fix-key fix-list)))

(defun gdb-line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defmacro gdb-mark-line (line variable)
  "Set VARIABLE marker to point at beginning of LINE.

If current window has no fringes, inverse colors on LINE.

Return position where LINE begins."
  `(save-excursion
     (let* ((posns (gdb-line-posns ,line))
            (start-posn (car posns))
            (end-posn (cdr posns)))
       (set-marker ,variable (copy-marker start-posn))
       (when (not (> (car (window-fringes)) 0))
         (put-text-property start-posn end-posn
                            'font-lock-face '(:inverse-video t)))
       start-posn)))

(defun gdb-pad-string (string padding)
  (format (concat "%" (number-to-string padding) "s") string))

;; gdb-table struct is a way to programmatically construct simple
;; tables. It help to reliably align columns of data in GDB buffers
;; and provides
(defstruct
  gdb-table
  (column-sizes nil)
  (rows nil)
  (row-properties nil)
  (right-align nil))

(defun gdb-mapcar* (function &rest seqs)
  "Apply FUNCTION to each element of SEQS, and make a list of the results.
If there are several SEQS, FUNCTION is called with that many
arguments, and mapping stops as soon as the shortest list runs
out."
  (let ((shortest (apply #'min (mapcar #'length seqs))))
    (mapcar (lambda (i)
              (apply function
                     (mapcar
                      (lambda (seq)
                        (nth i seq))
                      seqs)))
            (number-sequence 0 (1- shortest)))))

(defun gdb-table-add-row (table row &optional properties)
  "Add ROW of string to TABLE and recalculate column sizes.

When non-nil, PROPERTIES will be added to the whole row when
calling `gdb-table-string'."
  (let ((rows (gdb-table-rows table))
        (row-properties (gdb-table-row-properties table))
        (column-sizes (gdb-table-column-sizes table))
        (right-align (gdb-table-right-align table)))
    (when (not column-sizes)
      (setf (gdb-table-column-sizes table)
            (make-list (length row) 0)))
    (setf (gdb-table-rows table)
          (append rows (list row)))
    (setf (gdb-table-row-properties table)
          (append row-properties (list properties)))
    (setf (gdb-table-column-sizes table)
          (gdb-mapcar* (lambda (x s)
                         (let ((new-x
                                (max (abs x) (string-width (or s "")))))
                           (if right-align new-x (- new-x))))
                       (gdb-table-column-sizes table)
                       row))
    ;; Avoid trailing whitespace at eol
    (if (not (gdb-table-right-align table))
        (setcar (last (gdb-table-column-sizes table)) 0))))

(defun gdb-table-string (table &optional sep)
  "Return TABLE as a string with columns separated with SEP."
  (let ((column-sizes (gdb-table-column-sizes table)))
    (mapconcat
     'identity
     (gdb-mapcar*
      (lambda (row properties)
        (apply 'propertize
               (mapconcat 'identity
                          (gdb-mapcar* (lambda (s x) (gdb-pad-string s x))
                                       row column-sizes)
                          sep)
               properties))
      (gdb-table-rows table)
      (gdb-table-row-properties table))
     "\n")))

;; bindat-get-field goes deep, gdb-get-many-fields goes wide
(defun gdb-get-many-fields (struct &rest fields)
  "Return a list of FIELDS values from STRUCT."
  (let ((values))
    (dolist (field fields values)
      (setq values (append values (list (bindat-get-field struct field)))))))

(defmacro def-gdb-auto-update-trigger (trigger-name gdb-command
                                                    handler-name
                                                    &optional signal-list)
  "Define a trigger TRIGGER-NAME which sends GDB-COMMAND and sets
HANDLER-NAME as its handler. HANDLER-NAME is bound to current
buffer with `gdb-bind-function-to-buffer'.

If SIGNAL-LIST is non-nil, GDB-COMMAND is sent only when the
defined trigger is called with an argument from SIGNAL-LIST. It's
not recommended to define triggers with empty SIGNAL-LIST.
Normally triggers should respond at least to 'update signal.

Normally the trigger defined by this command must be called from
the buffer where HANDLER-NAME must work. This should be done so
that buffer-local thread number may be used in GDB-COMMAND (by
calling `gdb-current-context-command').
`gdb-bind-function-to-buffer' is used to achieve this, see
`gdb-get-buffer-create'.

Triggers defined by this command are meant to be used as a
trigger argument when describing buffer types with
`gdb-set-buffer-rules'."
  `(defun ,trigger-name (&optional signal)
     (when
         (or (not ,signal-list)
             (memq signal ,signal-list))
       (when (not (gdb-pending-p
                   (cons (current-buffer) ',trigger-name)))
         (gdb-input ,gdb-command
		    (gdb-bind-function-to-buffer ',handler-name (current-buffer)))
         (gdb-add-pending (cons (current-buffer) ',trigger-name))))))

;; Used by disassembly buffer only, the rest use
;; def-gdb-trigger-and-handler
(defmacro def-gdb-auto-update-handler (handler-name trigger-name custom-defun
                                                    &optional nopreserve)
  "Define a handler HANDLER-NAME for TRIGGER-NAME with CUSTOM-DEFUN.

Handlers are normally called from the buffers they put output in.

Delete ((current-buffer) . TRIGGER-NAME) from
`gdb-pending-triggers', erase current buffer and evaluate
CUSTOM-DEFUN. Then `gdb-update-buffer-name' is called.

If NOPRESERVE is non-nil, window point is not restored after CUSTOM-DEFUN."
  `(defun ,handler-name ()
     (gdb-delete-pending (cons (current-buffer) ',trigger-name))
     (let* ((buffer-read-only nil)
            (window (get-buffer-window (current-buffer) 0))
            (start (window-start window))
            (p (window-point window)))
       (erase-buffer)
       (,custom-defun)
       (gdb-update-buffer-name)
       ,(when (not nopreserve)
          '(set-window-start window start)
          '(set-window-point window p)))))

(defmacro def-gdb-trigger-and-handler (trigger-name gdb-command
                                                    handler-name custom-defun
                                                    &optional signal-list)
  "Define trigger and handler.

TRIGGER-NAME trigger is defined to send GDB-COMMAND. See
`def-gdb-auto-update-trigger'.

HANDLER-NAME handler uses customization of CUSTOM-DEFUN. See
`def-gdb-auto-update-handler'."
  `(progn
     (def-gdb-auto-update-trigger ,trigger-name
       ,gdb-command
       ,handler-name ,signal-list)
     (def-gdb-auto-update-handler ,handler-name
       ,trigger-name ,custom-defun)))



;; Breakpoint buffer : This displays the output of `-break-list'.
(def-gdb-trigger-and-handler
  gdb-invalidate-breakpoints "-break-list"
  gdb-breakpoints-list-handler gdb-breakpoints-list-handler-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-breakpoints-buffer
 'gdb-breakpoints-buffer-name
 'gdb-breakpoints-mode
 'gdb-invalidate-breakpoints)

(defun gdb-breakpoints-list-handler-custom ()
  (let ((breakpoints-list (bindat-get-field
                           (gdb-json-partial-output "bkpt" "script")
                           'BreakpointTable 'body))
        (table (make-gdb-table)))
    (setq gdb-breakpoints-list nil)
    (gdb-table-add-row table '("Num" "Type" "Disp" "Enb" "Addr" "Hits" "What"))
    (dolist (breakpoint breakpoints-list)
      (add-to-list 'gdb-breakpoints-list
                   (cons (bindat-get-field breakpoint 'number)
                         breakpoint))
      (let ((at (bindat-get-field breakpoint 'at))
            (pending (bindat-get-field breakpoint 'pending))
            (func (bindat-get-field breakpoint 'func))
	    (type (bindat-get-field breakpoint 'type)))
        (gdb-table-add-row table
                           (list
                            (bindat-get-field breakpoint 'number)
                            (or type "")
                            (or (bindat-get-field breakpoint 'disp) "")
                            (let ((flag (bindat-get-field breakpoint 'enabled)))
                              (if (string-equal flag "y")
                                  (propertize "y" 'font-lock-face  font-lock-warning-face)
                                (propertize "n" 'font-lock-face  font-lock-comment-face)))
                            (bindat-get-field breakpoint 'addr)
                            (or (bindat-get-field breakpoint 'times) "")
                            (if (and type (string-match ".*watchpoint" type))
                                (bindat-get-field breakpoint 'what)
                              (or pending at
                                  (concat "in "
                                          (propertize (or func "unknown")
                                                      'font-lock-face font-lock-function-name-face)
                                          (gdb-frame-location breakpoint)))))
                           ;; Add clickable properties only for breakpoints with file:line
                           ;; information
                           (append (list 'gdb-breakpoint breakpoint)
                                   (when func '(help-echo "mouse-2, RET: visit breakpoint"
                                                mouse-face highlight))))))
    (insert (gdb-table-string table " "))
    (gdb-place-breakpoints)))

;; Put breakpoint icons in relevant margins (even those set in the GUD buffer).
(defun gdb-place-breakpoints ()
  ;; Remove all breakpoint-icons in source buffers but not assembler buffer.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (and (eq gud-minor-mode 'gdbmi)
               (not (string-match "\\` ?\\*.+\\*\\'" (buffer-name))))
          (gdb-remove-breakpoint-icons (point-min) (point-max)))))
  (dolist (breakpoint gdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint)) ; gdb-breakpoints-list is
                                        ; an associative list
           (line (bindat-get-field breakpoint 'line)))
      (when line
        (let ((file (bindat-get-field breakpoint 'fullname))
              (flag (bindat-get-field breakpoint 'enabled))
              (bptno (bindat-get-field breakpoint 'number)))
          (unless (file-exists-p file)
            (setq file (cdr (assoc bptno gdb-location-alist))))
          (if (and file
                   (not (string-equal file "File not found")))
              (with-current-buffer
                  (find-file-noselect file 'nowarn)
                (gdb-init-buffer)
                ;; Only want one breakpoint icon at each location.
                (gdb-put-breakpoint-icon (string-equal flag "y") bptno
                                         (string-to-number line)))
            (gdb-input (concat "list " file ":1") 'ignore)
            (gdb-input "-file-list-exec-source-file"
		       `(lambda () (gdb-get-location
				    ,bptno ,line ,flag)))))))))

(defvar gdb-source-file-regexp "fullname=\"\\(.*?\\)\"")

(defun gdb-get-location (bptno line flag)
  "Find the directory containing the relevant source file.
Put in buffer and place breakpoint icon."
  (goto-char (point-min))
  (catch 'file-not-found
    (if (re-search-forward gdb-source-file-regexp nil t)
	(delete (cons bptno "File not found") gdb-location-alist)
      (push (cons bptno (match-string 1)) gdb-location-alist)
      (gdb-resync)
      (unless (assoc bptno gdb-location-alist)
	(push (cons bptno "File not found") gdb-location-alist)
	(message-box "Cannot find source file for breakpoint location.
Add directory to search path for source files using the GDB command, dir."))
      (throw 'file-not-found nil))
    (with-current-buffer (find-file-noselect (match-string 1))
      (gdb-init-buffer)
      ;; only want one breakpoint icon at each location
      (gdb-put-breakpoint-icon (eq flag ?y) bptno (string-to-number line)))))

(add-hook 'find-file-hook 'gdb-find-file-hook)

(defun gdb-find-file-hook ()
  "Set up buffer for debugging if file is part of the source code
of the current session."
  (if (and (buffer-name gud-comint-buffer)
	   ;; in case gud or gdb-ui is just loaded
	   gud-comint-buffer
	   (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
	       'gdbmi))
      (if (member buffer-file-name gdb-source-file-list)
	  (with-current-buffer (find-buffer-visiting buffer-file-name)
	    (gdb-init-buffer)))))

(declare-function gud-remove "gdb-mi" t t) ; gud-def
(declare-function gud-break  "gdb-mi" t t) ; gud-def
(declare-function fringe-bitmaps-at-pos "fringe.c" (&optional pos window))

(defun gdb-mouse-set-clear-breakpoint (event)
  "Set/clear breakpoint in left fringe/margin at mouse click.
If not in a source or disassembly buffer just set point."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (or (buffer-file-name) (derived-mode-p 'gdb-disassembly-mode))
	  (if (numberp (posn-point posn))
	      (save-excursion
		(goto-char (posn-point posn))
		(if (or (posn-object posn)
			(eq (car (fringe-bitmaps-at-pos (posn-point posn)))
			    'breakpoint))
		    (gud-remove nil)
		  (gud-break nil)))))
      (posn-set-point posn))))

(defun gdb-mouse-toggle-breakpoint-margin (event)
  "Enable/disable breakpoint in left margin with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (if (numberp (posn-point posn))
	(with-selected-window (posn-window posn)
	  (save-excursion
	    (goto-char (posn-point posn))
	    (if	(posn-object posn)
		(gud-basic-call
		 (let ((bptno (get-text-property
			       0 'gdb-bptno (car (posn-string posn)))))
		   (concat
		    (if (get-text-property
			 0 'gdb-enabled (car (posn-string posn)))
			"-break-disable "
		      "-break-enable ")
		    bptno)))))))))

(defun gdb-mouse-toggle-breakpoint-fringe (event)
  "Enable/disable breakpoint in left fringe with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((posn (event-end event))
	 (pos (posn-point posn))
	 obj)
    (when (numberp pos)
      (with-selected-window (posn-window posn)
	(with-current-buffer (window-buffer (selected-window))
	  (goto-char pos)
	  (dolist (overlay (overlays-in pos pos))
	    (when (overlay-get overlay 'put-break)
	      (setq obj (overlay-get overlay 'before-string))))
	  (when (stringp obj)
	    (gud-basic-call
	     (concat
	      (if (get-text-property 0 'gdb-enabled obj)
		  "-break-disable "
		"-break-enable ")
              (get-text-property 0 'gdb-bptno obj)))))))))

(defun gdb-breakpoints-buffer-name ()
  (concat "*breakpoints of " (gdb-get-target-string) "*"))

(def-gdb-display-buffer
  gdb-display-breakpoints-buffer
  'gdb-breakpoints-buffer
  "Display status of user-settable breakpoints.")

(def-gdb-frame-for-buffer
  gdb-frame-breakpoints-buffer
  'gdb-breakpoints-buffer
  "Display status of user-settable breakpoints in a new frame.")

(defvar gdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap))
	(menu (make-sparse-keymap "Breakpoints")))
    (define-key menu [quit] '("Quit"   . gdb-delete-frame-or-window))
    (define-key menu [goto] '("Goto"   . gdb-goto-breakpoint))
    (define-key menu [delete] '("Delete" . gdb-delete-breakpoint))
    (define-key menu [toggle] '("Toggle" . gdb-toggle-breakpoint))
    (suppress-keymap map)
    (define-key map [menu-bar breakpoints] (cons "Breakpoints" menu))
    (define-key map " " 'gdb-toggle-breakpoint)
    (define-key map "D" 'gdb-delete-breakpoint)
    ;; Don't bind "q" to kill-this-buffer as we need it for breakpoint icons.
    (define-key map "q" 'gdb-delete-frame-or-window)
    (define-key map "\r" 'gdb-goto-breakpoint)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (gdb-set-window-buffer
                            (gdb-get-buffer-create 'gdb-threads-buffer) t)))
    (define-key map [mouse-2] 'gdb-goto-breakpoint)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun gdb-delete-frame-or-window ()
  "Delete frame if there is only one window.  Otherwise delete the window."
  (interactive)
  (if (one-window-p) (delete-frame)
    (delete-window)))

;;from make-mode-line-mouse-map
(defun gdb-make-header-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the header line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    (define-key map (vector 'header-line 'down-mouse-1) 'ignore)
    map))

(defmacro gdb-propertize-header (name buffer help-echo mouse-face face)
  `(propertize ,name
	       'help-echo ,help-echo
	       'mouse-face ',mouse-face
	       'face ',face
	       'local-map
	       (gdb-make-header-line-mouse-map
		'mouse-1
		(lambda (event) (interactive "e")
		  (save-selected-window
		    (select-window (posn-window (event-start event)))
                    (gdb-set-window-buffer
                     (gdb-get-buffer-create ',buffer) t) )))))


;; uses "-thread-info". Needs GDB 7.0 onwards.
;;; Threads view

(defun gdb-threads-buffer-name ()
  (concat "*threads of " (gdb-get-target-string) "*"))

(def-gdb-display-buffer
  gdb-display-threads-buffer
  'gdb-threads-buffer
  "Display GDB threads.")

(def-gdb-frame-for-buffer
  gdb-frame-threads-buffer
  'gdb-threads-buffer
  "Display GDB threads in a new frame.")

(def-gdb-trigger-and-handler
  gdb-invalidate-threads (gdb-current-context-command "-thread-info")
  gdb-thread-list-handler gdb-thread-list-handler-custom
  '(start update update-threads))

(gdb-set-buffer-rules
 'gdb-threads-buffer
 'gdb-threads-buffer-name
 'gdb-threads-mode
 'gdb-invalidate-threads)

(defvar gdb-threads-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face))
    (" \\(stopped\\)"  (1 font-lock-warning-face))
    (" \\(running\\)"  (1 font-lock-string-face))
    ("\\(\\(\\sw\\|[_.]\\)+\\)="  (1 font-lock-variable-name-face)))
  "Font lock keywords used in `gdb-threads-mode'.")

(defvar gdb-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'gdb-select-thread)
    (define-key map "f" 'gdb-display-stack-for-thread)
    (define-key map "F" 'gdb-frame-stack-for-thread)
    (define-key map "l" 'gdb-display-locals-for-thread)
    (define-key map "L" 'gdb-frame-locals-for-thread)
    (define-key map "r" 'gdb-display-registers-for-thread)
    (define-key map "R" 'gdb-frame-registers-for-thread)
    (define-key map "d" 'gdb-display-disassembly-for-thread)
    (define-key map "D" 'gdb-frame-disassembly-for-thread)
    (define-key map "i" 'gdb-interrupt-thread)
    (define-key map "c" 'gdb-continue-thread)
    (define-key map "s" 'gdb-step-thread)
    (define-key map "\t"
      (lambda ()
        (interactive)
        (gdb-set-window-buffer
         (gdb-get-buffer-create 'gdb-breakpoints-buffer) t)))
    (define-key map [mouse-2] 'gdb-select-thread)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar gdb-threads-header
  (list
   (gdb-propertize-header
    "Breakpoints" gdb-breakpoints-buffer
    "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (gdb-propertize-header "Threads" gdb-threads-buffer
			  nil nil mode-line)))

(define-derived-mode gdb-threads-mode gdb-parent-mode "Threads"
  "Major mode for GDB threads."
  (setq gdb-thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdb-thread-position)
  (setq header-line-format gdb-threads-header)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-threads-font-lock-keywords))
  'gdb-invalidate-threads)

(defun gdb-thread-list-handler-custom ()
  (let ((threads-list (bindat-get-field (gdb-json-partial-output) 'threads))
        (table (make-gdb-table))
        (marked-line nil))
    (setq gdb-threads-list nil)
    (setq gdb-running-threads-count 0)
    (setq gdb-stopped-threads-count 0)
    (set-marker gdb-thread-position nil)

    (dolist (thread (reverse threads-list))
      (let ((running (equal (bindat-get-field thread 'state) "running")))
        (add-to-list 'gdb-threads-list
                     (cons (bindat-get-field thread 'id)
                           thread))
        (if running
            (incf gdb-running-threads-count)
          (incf gdb-stopped-threads-count))

        (gdb-table-add-row table
                           (list
                            (bindat-get-field thread 'id)
                            (concat
                             (if gdb-thread-buffer-verbose-names
                                 (concat (bindat-get-field thread 'target-id) " ") "")
                             (bindat-get-field thread 'state)
                             ;; Include frame information for stopped threads
                             (if (not running)
                                 (concat
                                  " in " (bindat-get-field thread 'frame 'func)
                                  (if gdb-thread-buffer-arguments
                                      (concat
                                       " ("
                                       (let ((args (bindat-get-field thread 'frame 'args)))
                                         (mapconcat
                                          (lambda (arg)
                                            (apply #'format "%s=%s"
                                                   (gdb-get-many-fields arg 'name 'value)))
                                          args ","))
                                       ")")
                                    "")
                                  (if gdb-thread-buffer-locations
                                      (gdb-frame-location (bindat-get-field thread 'frame)) "")
                                  (if gdb-thread-buffer-addresses
                                      (concat " at " (bindat-get-field thread 'frame 'addr)) ""))
                               "")))
                           (list
                            'gdb-thread thread
                            'mouse-face 'highlight
                            'help-echo "mouse-2, RET: select thread")))
      (when (string-equal gdb-thread-number
                          (bindat-get-field thread 'id))
        (setq marked-line (length gdb-threads-list))))
    (insert (gdb-table-string table " "))
    (when marked-line
      (gdb-mark-line marked-line gdb-thread-position)))
  ;; We update gud-running here because we need to make sure that
  ;; gdb-threads-list is up-to-date
  (gdb-update-gud-running)
  (gdb-emit-signal gdb-buf-publisher 'update-disassembly))

(defmacro def-gdb-thread-buffer-command (name custom-defun &optional doc)
  "Define a NAME command which will act upon thread on the current line.

CUSTOM-DEFUN may use locally bound `thread' variable, which will
be the value of 'gdb-thread property of the current line. If
'gdb-thread is nil, error is signaled."
  `(defun ,name (&optional event)
     ,(when doc doc)
     (interactive (list last-input-event))
     (if event (posn-set-point (event-end event)))
     (save-excursion
       (beginning-of-line)
       (let ((thread (get-text-property (point) 'gdb-thread)))
         (if thread
             ,custom-defun
           (error "Not recognized as thread line"))))))

(defmacro def-gdb-thread-buffer-simple-command (name buffer-command
                                                     &optional doc)
  "Define a NAME which will call BUFFER-COMMAND with id of thread
on the current line."
  `(def-gdb-thread-buffer-command ,name
     (,buffer-command (bindat-get-field thread 'id))
     ,doc))

(def-gdb-thread-buffer-command gdb-select-thread
  (let ((new-id (bindat-get-field thread 'id)))
    (gdb-setq-thread-number new-id)
    (gdb-input (concat "-thread-select " new-id) 'ignore)
    (gdb-update))
  "Select the thread at current line of threads buffer.")

(def-gdb-thread-buffer-simple-command
  gdb-display-stack-for-thread
  gdb-preemptively-display-stack-buffer
  "Display stack buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-display-locals-for-thread
  gdb-preemptively-display-locals-buffer
  "Display locals buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-display-registers-for-thread
  gdb-preemptively-display-registers-buffer
  "Display registers buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-display-disassembly-for-thread
  gdb-preemptively-display-disassembly-buffer
  "Display disassembly buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-stack-for-thread
  gdb-frame-stack-buffer
  "Display a new frame with stack buffer for the thread at
current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-locals-for-thread
  gdb-frame-locals-buffer
  "Display a new frame with locals buffer for the thread at
current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-registers-for-thread
  gdb-frame-registers-buffer
  "Display a new frame with registers buffer for the thread at
current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-disassembly-for-thread
  gdb-frame-disassembly-buffer
  "Display a new frame with disassembly buffer for the thread at
current line.")

(defmacro def-gdb-thread-buffer-gud-command (name gud-command &optional doc)
  "Define a NAME which will execute GUD-COMMAND with
`gdb-thread-number' locally bound to id of thread on the current
line."
  `(def-gdb-thread-buffer-command ,name
     (if gdb-non-stop
         (let ((gdb-thread-number (bindat-get-field thread 'id))
               (gdb-gud-control-all-threads nil))
           (call-interactively #',gud-command))
       (error "Available in non-stop mode only, customize `gdb-non-stop-setting'"))
     ,doc))

(def-gdb-thread-buffer-gud-command
  gdb-interrupt-thread
  gud-stop-subjob
  "Interrupt thread at current line.")

(def-gdb-thread-buffer-gud-command
  gdb-continue-thread
  gud-cont
  "Continue thread at current line.")

(def-gdb-thread-buffer-gud-command
  gdb-step-thread
  gud-step
  "Step thread at current line.")


;;; Memory view

(defcustom gdb-memory-rows 8
  "Number of data rows in memory window."
  :type 'integer
  :group 'gud
  :version "23.2")

(defcustom gdb-memory-columns 4
  "Number of data columns in memory window."
  :type 'integer
  :group 'gud
  :version "23.2")

(defcustom gdb-memory-format "x"
  "Display format of data items in memory window."
  :type '(choice (const :tag "Hexadecimal" "x")
          (const :tag "Signed decimal" "d")
          (const :tag "Unsigned decimal" "u")
          (const :tag "Octal" "o")
          (const :tag "Binary" "t"))
  :group 'gud
  :version "22.1")

(defcustom gdb-memory-unit 4
  "Unit size of data items in memory window."
  :type '(choice (const :tag "Byte" 1)
          (const :tag "Halfword" 2)
          (const :tag "Word" 4)
          (const :tag "Giant word" 8))
  :group 'gud
  :version "23.2")

(def-gdb-trigger-and-handler
  gdb-invalidate-memory
  (format "-data-read-memory %s %s %d %d %d"
          gdb-memory-address
          gdb-memory-format
          gdb-memory-unit
          gdb-memory-rows
          gdb-memory-columns)
  gdb-read-memory-handler
  gdb-read-memory-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-memory-buffer
 'gdb-memory-buffer-name
 'gdb-memory-mode
 'gdb-invalidate-memory)

(defun gdb-memory-column-width (size format)
  "Return length of string with memory unit of SIZE in FORMAT.

SIZE is in bytes, as in `gdb-memory-unit'. FORMAT is a string as
in `gdb-memory-format'."
  (let ((format-base (cdr (assoc format
                                 '(("x" . 16)
                                   ("d" . 10) ("u" . 10)
                                   ("o" . 8)
                                   ("t" . 2))))))
    (if format-base
        (let ((res (ceiling (log (expt 2.0 (* size 8)) format-base))))
          (cond ((string-equal format "x")
                 (+ 2 res)) ; hexadecimal numbers have 0x in front
                ((or (string-equal format "d")
                     (string-equal format "o"))
                 (1+ res))
                (t res)))
      (error "Unknown format"))))

(defun gdb-read-memory-custom ()
  (let* ((res (gdb-json-partial-output))
         (err-msg (bindat-get-field res 'msg)))
    (if (not err-msg)
        (let ((memory (bindat-get-field res 'memory)))
          (setq gdb-memory-address (bindat-get-field res 'addr))
          (setq gdb-memory-next-page (bindat-get-field res 'next-page))
          (setq gdb-memory-prev-page (bindat-get-field res 'prev-page))
          (setq gdb-memory-last-address gdb-memory-address)
          (dolist (row memory)
            (insert (concat (bindat-get-field row 'addr) ":"))
            (dolist (column (bindat-get-field row 'data))
              (insert (gdb-pad-string column
                                      (+ 2 (gdb-memory-column-width
                                            gdb-memory-unit
                                            gdb-memory-format)))))
            (newline)))
      ;; Show last page instead of empty buffer when out of bounds
      (progn
        (let ((gdb-memory-address gdb-memory-last-address))
          (gdb-invalidate-memory 'update)
          (error err-msg))))))

(defvar gdb-memory-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "n" 'gdb-memory-show-next-page)
    (define-key map "p" 'gdb-memory-show-previous-page)
    (define-key map "a" 'gdb-memory-set-address)
    (define-key map "t" 'gdb-memory-format-binary)
    (define-key map "o" 'gdb-memory-format-octal)
    (define-key map "u" 'gdb-memory-format-unsigned)
    (define-key map "d" 'gdb-memory-format-signed)
    (define-key map "x" 'gdb-memory-format-hexadecimal)
    (define-key map "b" 'gdb-memory-unit-byte)
    (define-key map "h" 'gdb-memory-unit-halfword)
    (define-key map "w" 'gdb-memory-unit-word)
    (define-key map "g" 'gdb-memory-unit-giant)
    (define-key map "R" 'gdb-memory-set-rows)
    (define-key map "C" 'gdb-memory-set-columns)
    map))

(defun gdb-memory-set-address-event (event)
  "Handle a click on address field in memory buffer header."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (gdb-memory-set-address)))

;; Non-event version for use within keymap
(defun gdb-memory-set-address ()
  "Set the start memory address."
  (interactive)
  (let ((arg (read-from-minibuffer "Memory address: ")))
    (setq gdb-memory-address arg))
  (gdb-invalidate-memory 'update))

(defmacro def-gdb-set-positive-number (name variable echo-string &optional doc)
  "Define a function NAME which reads new VAR value from minibuffer."
  `(defun ,name (event)
     ,(when doc doc)
     (interactive "e")
     (save-selected-window
       (select-window (posn-window (event-start event)))
       (let* ((arg (read-from-minibuffer ,echo-string))
              (count (string-to-number arg)))
         (if (<= count 0)
             (error "Positive number only")
           (customize-set-variable ',variable count)
           (gdb-invalidate-memory 'update))))))

(def-gdb-set-positive-number
  gdb-memory-set-rows
  gdb-memory-rows
  "Rows: "
  "Set the number of data rows in memory window.")

(def-gdb-set-positive-number
  gdb-memory-set-columns
  gdb-memory-columns
  "Columns: "
  "Set the number of data columns in memory window.")

(defmacro def-gdb-memory-format (name format doc)
  "Define a function NAME to switch memory buffer to use FORMAT.

DOC is an optional documentation string."
  `(defun ,name () ,(when doc doc)
     (interactive)
     (customize-set-variable 'gdb-memory-format ,format)
     (gdb-invalidate-memory 'update)))

(def-gdb-memory-format
  gdb-memory-format-binary "t"
  "Set the display format to binary.")

(def-gdb-memory-format
  gdb-memory-format-octal "o"
  "Set the display format to octal.")

(def-gdb-memory-format
  gdb-memory-format-unsigned "u"
  "Set the display format to unsigned decimal.")

(def-gdb-memory-format
  gdb-memory-format-signed "d"
  "Set the display format to decimal.")

(def-gdb-memory-format
  gdb-memory-format-hexadecimal "x"
  "Set the display format to hexadecimal.")

(defvar gdb-memory-format-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-3] 'gdb-memory-format-menu-1)
    map)
  "Keymap to select format in the header line.")

(defvar gdb-memory-format-menu
  (let ((map (make-sparse-keymap "Format")))

    (define-key map [binary]
      '(menu-item "Binary" gdb-memory-format-binary
        :button (:radio . (equal gdb-memory-format "t"))))
    (define-key map [octal]
      '(menu-item "Octal" gdb-memory-format-octal
        :button (:radio . (equal gdb-memory-format "o"))))
    (define-key map [unsigned]
      '(menu-item "Unsigned Decimal" gdb-memory-format-unsigned
        :button (:radio . (equal gdb-memory-format "u"))))
    (define-key map [signed]
      '(menu-item "Signed Decimal" gdb-memory-format-signed
        :button (:radio . (equal gdb-memory-format "d"))))
    (define-key map [hexadecimal]
      '(menu-item "Hexadecimal" gdb-memory-format-hexadecimal
        :button (:radio . (equal gdb-memory-format "x"))))
    map)
  "Menu of display formats in the header line.")

(defun gdb-memory-format-menu (event)
  (interactive "@e")
  (x-popup-menu event gdb-memory-format-menu))

(defun gdb-memory-format-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (gdb-memory-format-menu event))
	   (binding (and selection (lookup-key gdb-memory-format-menu
					       (vector (car selection))))))
      (if binding (call-interactively binding)))))

(defmacro def-gdb-memory-unit (name unit-size doc)
  "Define a function NAME to switch memory unit size to UNIT-SIZE.

DOC is an optional documentation string."
  `(defun ,name () ,(when doc doc)
     (interactive)
     (customize-set-variable 'gdb-memory-unit ,unit-size)
     (gdb-invalidate-memory 'update)))

(def-gdb-memory-unit gdb-memory-unit-giant 8
  "Set the unit size to giant words (eight bytes).")

(def-gdb-memory-unit gdb-memory-unit-word 4
  "Set the unit size to words (four bytes).")

(def-gdb-memory-unit gdb-memory-unit-halfword 2
  "Set the unit size to halfwords (two bytes).")

(def-gdb-memory-unit gdb-memory-unit-byte 1
  "Set the unit size to bytes.")

(defmacro def-gdb-memory-show-page (name address-var &optional doc)
  "Define a function NAME which show new address in memory buffer.

The defined function switches Memory buffer to show address
stored in ADDRESS-VAR variable.

DOC is an optional documentation string."
  `(defun ,name
     ,(when doc doc)
     (interactive)
     (let ((gdb-memory-address ,address-var))
       (gdb-invalidate-memory))))

(def-gdb-memory-show-page gdb-memory-show-previous-page
  gdb-memory-prev-page)

(def-gdb-memory-show-page gdb-memory-show-next-page
  gdb-memory-next-page)

(defvar gdb-memory-unit-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-3] 'gdb-memory-unit-menu-1)
    map)
  "Keymap to select units in the header line.")

(defvar gdb-memory-unit-menu
  (let ((map (make-sparse-keymap "Unit")))
    (define-key map [giantwords]
      '(menu-item "Giant words" gdb-memory-unit-giant
        :button (:radio . (equal gdb-memory-unit 8))))
    (define-key map [words]
      '(menu-item "Words" gdb-memory-unit-word
        :button (:radio . (equal gdb-memory-unit 4))))
    (define-key map [halfwords]
      '(menu-item "Halfwords" gdb-memory-unit-halfword
        :button (:radio . (equal gdb-memory-unit 2))))
    (define-key map [bytes]
      '(menu-item "Bytes" gdb-memory-unit-byte
        :button (:radio . (equal gdb-memory-unit 1))))
    map)
  "Menu of units in the header line.")

(defun gdb-memory-unit-menu (event)
  (interactive "@e")
  (x-popup-menu event gdb-memory-unit-menu))

(defun gdb-memory-unit-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (gdb-memory-unit-menu event))
	   (binding (and selection (lookup-key gdb-memory-unit-menu
					       (vector (car selection))))))
      (if binding (call-interactively binding)))))

(defvar gdb-memory-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-memory-mode'.")

(defvar gdb-memory-header
  '(:eval
    (concat
     "Start address["
     (propertize "-"
                 'face font-lock-warning-face
                 'help-echo "mouse-1: decrement address"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-show-previous-page))
     "|"
     (propertize "+"
                 'face font-lock-warning-face
                 'help-echo "mouse-1: increment address"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-show-next-page))
     "]: "
     (propertize gdb-memory-address
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set start address"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-set-address-event))
     "  Rows: "
     (propertize (number-to-string gdb-memory-rows)
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set number of columns"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-set-rows))
     "  Columns: "
     (propertize (number-to-string gdb-memory-columns)
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set number of columns"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-set-columns))
     "  Display Format: "
     (propertize gdb-memory-format
                 'face font-lock-warning-face
                 'help-echo "mouse-3: select display format"
                 'mouse-face 'mode-line-highlight
                 'local-map gdb-memory-format-map)
     "  Unit Size: "
     (propertize (number-to-string gdb-memory-unit)
                 'face font-lock-warning-face
                 'help-echo "mouse-3: select unit size"
                 'mouse-face 'mode-line-highlight
                 'local-map gdb-memory-unit-map)))
  "Header line used in `gdb-memory-mode'.")

(define-derived-mode gdb-memory-mode gdb-parent-mode "Memory"
  "Major mode for examining memory."
  (setq header-line-format gdb-memory-header)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-memory-font-lock-keywords))
  'gdb-invalidate-memory)

(defun gdb-memory-buffer-name ()
  (concat "*memory of " (gdb-get-target-string) "*"))

(def-gdb-display-buffer
  gdb-display-memory-buffer
  'gdb-memory-buffer
  "Display memory contents.")

(defun gdb-frame-memory-buffer ()
  "Display memory contents in a new frame."
  (interactive)
  (let* ((special-display-regexps (append special-display-regexps '(".*")))
	 (special-display-frame-alist
	  `((left-fringe . 0)
            (right-fringe . 0)
            (width . 83)
            ,@gdb-frame-parameters)))
    (display-buffer (gdb-get-buffer-create 'gdb-memory-buffer))))


;;; Disassembly view

(defun gdb-disassembly-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "disassembly of " (gdb-get-target-string))))

(def-gdb-display-buffer
  gdb-display-disassembly-buffer
  'gdb-disassembly-buffer
  "Display disassembly for current stack frame.")

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-disassembly-buffer
  'gdb-disassembly-buffer)

(def-gdb-frame-for-buffer
  gdb-frame-disassembly-buffer
  'gdb-disassembly-buffer
  "Display disassembly in a new frame.")

(def-gdb-auto-update-trigger gdb-invalidate-disassembly
  (let* ((frame (gdb-current-buffer-frame))
         (file (bindat-get-field frame 'fullname))
         (line (bindat-get-field frame 'line)))
    (if file
      (format "-data-disassemble -f %s -l %s -n -1 -- 0" file line)
    ;; If we're unable to get a file name / line for $PC, simply
    ;; follow $PC, disassembling the next 10 (x ~15 (on IA) ==
    ;; 150 bytes) instructions.
    "-data-disassemble -s $pc -e \"$pc + 150\" -- 0"))
  gdb-disassembly-handler
  ;; We update disassembly only after we have actual frame information
  ;; about all threads, so no there's `update' signal in this list
  '(start update-disassembly))

(def-gdb-auto-update-handler
  gdb-disassembly-handler
  gdb-invalidate-disassembly
  gdb-disassembly-handler-custom
  t)

(gdb-set-buffer-rules
 'gdb-disassembly-buffer
 'gdb-disassembly-buffer-name
 'gdb-disassembly-mode
 'gdb-invalidate-disassembly)

(defvar gdb-disassembly-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face))
    ;; 0xNNNNNNNN <__function.name+n>: opcode
    ("^0x[0-9a-f]+ \\(<\\(\\(\\sw\\|[_.]\\)+\\)\\+[0-9]+>\\)?:[ \t]+\\(\\sw+\\)"
     (4 font-lock-keyword-face))
    ;; %register(at least i386)
    ("%\\sw+" . font-lock-variable-name-face)
    ("^\\(Dump of assembler code for function\\) \\(.+\\):"
     (1 font-lock-comment-face)
     (2 font-lock-function-name-face))
    ("^\\(End of assembler dump\\.\\)" . font-lock-comment-face))
  "Font lock keywords used in `gdb-disassembly-mode'.")

(defvar gdb-disassembly-mode-map
  ;; TODO
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    map))

(define-derived-mode gdb-disassembly-mode gdb-parent-mode "Disassembly"
  "Major mode for GDB disassembly information."
  ;; TODO Rename overlay variable for disassembly mode
  (add-to-list 'overlay-arrow-variable-list 'gdb-disassembly-position)
  (setq fringes-outside-margins t)
  (set (make-local-variable 'gdb-disassembly-position) (make-marker))
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-disassembly-font-lock-keywords))
  'gdb-invalidate-disassembly)

(defun gdb-disassembly-handler-custom ()
  (let* ((instructions (bindat-get-field (gdb-json-partial-output) 'asm_insns))
         (address (bindat-get-field (gdb-current-buffer-frame) 'addr))
         (table (make-gdb-table))
         (marked-line nil))
    (dolist (instr instructions)
      (gdb-table-add-row table
                         (list
                          (bindat-get-field instr 'address)
                          (let
                              ((func-name (bindat-get-field instr 'func-name))
                               (offset (bindat-get-field instr 'offset)))
                            (if func-name
                                (format "<%s+%s>:" func-name offset)
                              ""))
                          (bindat-get-field instr 'inst)))
      (when (string-equal (bindat-get-field instr 'address)
                          address)
        (progn
          (setq marked-line (length (gdb-table-rows table)))
          (setq fringe-indicator-alist
                (if (string-equal gdb-frame-number "0")
                    nil
                  '((overlay-arrow . hollow-right-triangle)))))))
    (insert (gdb-table-string table " "))
    (gdb-disassembly-place-breakpoints)
    ;; Mark current position with overlay arrow and scroll window to
    ;; that point
    (when marked-line
      (let ((window (get-buffer-window (current-buffer) 0)))
        (set-window-point window (gdb-mark-line marked-line
                                                gdb-disassembly-position))))
    (setq mode-name
          (gdb-current-context-mode-name
           (concat "Disassembly: "
                   (bindat-get-field (gdb-current-buffer-frame) 'func))))))

(defun gdb-disassembly-place-breakpoints ()
  (gdb-remove-breakpoint-icons (point-min) (point-max))
  (dolist (breakpoint gdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint))
           (bptno (bindat-get-field breakpoint 'number))
           (flag (bindat-get-field breakpoint 'enabled))
           (address (bindat-get-field breakpoint 'addr)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "^" address) nil t)
            (gdb-put-breakpoint-icon (string-equal flag "y") bptno))))))


(defvar gdb-breakpoints-header
  (list
   (gdb-propertize-header "Breakpoints" gdb-breakpoints-buffer
			  nil nil mode-line)
   " "
   (gdb-propertize-header "Threads" gdb-threads-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)))

;;; Breakpoints view
(define-derived-mode gdb-breakpoints-mode gdb-parent-mode "Breakpoints"
  "Major mode for gdb breakpoints."
  (setq header-line-format gdb-breakpoints-header)
  'gdb-invalidate-breakpoints)

(defun gdb-toggle-breakpoint ()
  "Enable/disable breakpoint at current line of breakpoints buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdb-breakpoint)))
      (if breakpoint
          (gud-basic-call
           (concat (if (equal "y" (bindat-get-field breakpoint 'enabled))
                       "-break-disable "
                     "-break-enable ")
                   (bindat-get-field breakpoint 'number)))
        (error "Not recognized as break/watchpoint line")))))

(defun gdb-delete-breakpoint ()
  "Delete the breakpoint at current line of breakpoints buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdb-breakpoint)))
      (if breakpoint
          (gud-basic-call (concat "-break-delete "
                                  (bindat-get-field breakpoint 'number)))
        (error "Not recognized as break/watchpoint line")))))

(defun gdb-goto-breakpoint (&optional event)
  "Go to the location of breakpoint at current line of
breakpoints buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  ;; Hack to stop gdb-goto-breakpoint displaying in GUD buffer.
  (let ((window (get-buffer-window gud-comint-buffer)))
    (if window (save-selected-window  (select-window window))))
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdb-breakpoint)))
      (if breakpoint
          (let ((bptno (bindat-get-field breakpoint 'number))
                (file  (bindat-get-field breakpoint 'fullname))
                (line  (bindat-get-field breakpoint 'line)))
            (save-selected-window
              (let* ((buffer (find-file-noselect
                              (if (file-exists-p file) file
                                (cdr (assoc bptno gdb-location-alist)))))
                     (window (or (gdb-display-source-buffer buffer)
                                 (display-buffer buffer))))
                (setq gdb-source-window window)
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (forward-line (1- (string-to-number line)))
                  (set-window-point window (point))))))
        (error "Not recognized as break/watchpoint line")))))


;; Frames buffer.  This displays a perpetually correct backtrack trace.
;;
(def-gdb-trigger-and-handler
  gdb-invalidate-frames (gdb-current-context-command "-stack-list-frames")
  gdb-stack-list-frames-handler gdb-stack-list-frames-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-stack-buffer
 'gdb-stack-buffer-name
 'gdb-frames-mode
 'gdb-invalidate-frames)

(defun gdb-frame-location (frame)
  "Return \" of file:line\" or \" of library\" for structure FRAME.

FRAME must have either \"file\" and \"line\" members or \"from\"
member."
  (let ((file (bindat-get-field frame 'file))
        (line (bindat-get-field frame 'line))
        (from (bindat-get-field frame 'from)))
    (let ((res (or (and file line (concat file ":" line))
                   from)))
      (if res (concat " of " res) ""))))

(defun gdb-stack-list-frames-custom ()
  (let ((stack (bindat-get-field (gdb-json-partial-output "frame") 'stack))
        (table (make-gdb-table)))
    (set-marker gdb-stack-position nil)
    (dolist (frame stack)
      (gdb-table-add-row table
                         (list
                          (bindat-get-field frame 'level)
                          "in"
                          (concat
                           (bindat-get-field frame 'func)
                           (if gdb-stack-buffer-locations
                               (gdb-frame-location frame) "")
                           (if gdb-stack-buffer-addresses
                               (concat " at " (bindat-get-field frame 'addr)) "")))
                         `(mouse-face highlight
                                      help-echo "mouse-2, RET: Select frame"
                                      gdb-frame ,frame)))
    (insert (gdb-table-string table " ")))
  (when (and gdb-frame-number
             (gdb-buffer-shows-main-thread-p))
    (gdb-mark-line (1+ (string-to-number gdb-frame-number))
                   gdb-stack-position))
  (setq mode-name
        (gdb-current-context-mode-name "Frames")))

(defun gdb-stack-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "stack frames of " (gdb-get-target-string))))

(def-gdb-display-buffer
  gdb-display-stack-buffer
  'gdb-stack-buffer
  "Display backtrace of current stack.")

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-stack-buffer
  'gdb-stack-buffer nil t)

(def-gdb-frame-for-buffer
  gdb-frame-stack-buffer
  'gdb-stack-buffer
  "Display backtrace of current stack in a new frame.")

(defvar gdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\r" 'gdb-select-frame)
    (define-key map [mouse-2] 'gdb-select-frame)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar gdb-frames-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(define-derived-mode gdb-frames-mode gdb-parent-mode "Frames"
  "Major mode for gdb call stack."
  (setq gdb-stack-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdb-stack-position)
  (setq truncate-lines t)  ;; Make it easier to see overlay arrow.
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-frames-font-lock-keywords))
  'gdb-invalidate-frames)

(defun gdb-select-frame (&optional event)
  "Select the frame and display the relevant source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let ((frame (get-text-property (point) 'gdb-frame)))
    (if frame
        (if (gdb-buffer-shows-main-thread-p)
            (let ((new-level (bindat-get-field frame 'level)))
              (setq gdb-frame-number new-level)
              (gdb-input (concat "-stack-select-frame " new-level)
			 'ignore)
              (gdb-update))
          (error "Could not select frame for non-current thread"))
      (error "Not recognized as frame line"))))


;; Locals buffer.
;; uses "-stack-list-locals --simple-values". Needs GDB 6.1 onwards.
(def-gdb-trigger-and-handler
  gdb-invalidate-locals
  (concat (gdb-current-context-command "-stack-list-locals")
          " --simple-values")
  gdb-locals-handler gdb-locals-handler-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-locals-buffer
 'gdb-locals-buffer-name
 'gdb-locals-mode
 'gdb-invalidate-locals)

(defvar gdb-locals-watch-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gud-watch)
    (define-key map [mouse-2] 'gud-watch)
    map)
  "Keymap to create watch expression of a complex data type local variable.")

(defvar gdb-edit-locals-map-1
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-edit-locals-value)
    (define-key map [mouse-2] 'gdb-edit-locals-value)
    map)
  "Keymap to edit value of a simple data type local variable.")

(defun gdb-edit-locals-value (&optional event)
  "Assign a value to a variable displayed in the locals buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (bindat-get-field
                 (get-text-property (point) 'gdb-local-variable) 'name))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-gdb-set variable " var " = " value)))))

;; Don't display values of arrays or structures.
;; These can be expanded using gud-watch.
(defun gdb-locals-handler-custom ()
  (let ((locals-list (bindat-get-field (gdb-json-partial-output) 'locals))
        (table (make-gdb-table)))
    (dolist (local locals-list)
      (let ((name (bindat-get-field local 'name))
            (value (bindat-get-field local 'value))
            (type (bindat-get-field local 'type)))
        (if (or (not value)
                (string-match "\\0x" value))
            (add-text-properties 0 (length name)
                                 `(mouse-face highlight
                                              help-echo "mouse-2: create watch expression"
                                              local-map ,gdb-locals-watch-map)
                                 name)
          (add-text-properties 0 (length value)
                               `(mouse-face highlight
                                            help-echo "mouse-2: edit value"
                                            local-map ,gdb-edit-locals-map-1)
                               value))
        (gdb-table-add-row
         table
         (list
          (propertize type 'font-lock-face font-lock-type-face)
          (propertize name 'font-lock-face font-lock-variable-name-face)
          value)
         `(gdb-local-variable ,local))))
    (insert (gdb-table-string table " "))
    (setq mode-name
          (gdb-current-context-mode-name
           (concat "Locals: "
                   (bindat-get-field (gdb-current-buffer-frame) 'func))))))

(defvar gdb-locals-header
  (list
   (gdb-propertize-header "Locals" gdb-locals-buffer
			  nil nil mode-line)
   " "
   (gdb-propertize-header "Registers" gdb-registers-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)))

(defvar gdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (gdb-set-window-buffer
                            (gdb-get-buffer-create
                             'gdb-registers-buffer
                             gdb-thread-number) t)))
    map))

(define-derived-mode gdb-locals-mode gdb-parent-mode "Locals"
  "Major mode for gdb locals."
  (setq header-line-format gdb-locals-header)
  'gdb-invalidate-locals)

(defun gdb-locals-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "locals of " (gdb-get-target-string))))

(def-gdb-display-buffer
  gdb-display-locals-buffer
  'gdb-locals-buffer
  "Display local variables of current stack and their values.")

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-locals-buffer
  'gdb-locals-buffer nil t)

(def-gdb-frame-for-buffer
  gdb-frame-locals-buffer
  'gdb-locals-buffer
  "Display local variables of current stack and their values in a new frame.")


;; Registers buffer.

(def-gdb-trigger-and-handler
  gdb-invalidate-registers
  (concat (gdb-current-context-command "-data-list-register-values") " x")
  gdb-registers-handler
  gdb-registers-handler-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-registers-buffer
 'gdb-registers-buffer-name
 'gdb-registers-mode
 'gdb-invalidate-registers)

(defun gdb-registers-handler-custom ()
  (when gdb-register-names
    (let ((register-values
           (bindat-get-field (gdb-json-partial-output) 'register-values))
          (table (make-gdb-table)))
      (dolist (register register-values)
        (let* ((register-number (bindat-get-field register 'number))
               (value (bindat-get-field register 'value))
               (register-name (nth (string-to-number register-number)
                                   gdb-register-names)))
          (gdb-table-add-row
           table
           (list
            (propertize register-name
                        'font-lock-face font-lock-variable-name-face)
            (if (member register-number gdb-changed-registers)
                (propertize value 'font-lock-face font-lock-warning-face)
              value))
           `(mouse-face highlight
                        help-echo "mouse-2: edit value"
                        gdb-register-name ,register-name))))
      (insert (gdb-table-string table " ")))
    (setq mode-name
          (gdb-current-context-mode-name "Registers"))))

(defun gdb-edit-register-value (&optional event)
  "Assign a value to a register displayed in the registers buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (bindat-get-field
                 (get-text-property (point) 'gdb-register-name)))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-gdb-set variable $" var " = " value)))))

(defvar gdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-edit-register-value)
    (define-key map [mouse-2] 'gdb-edit-register-value)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (gdb-set-window-buffer
                            (gdb-get-buffer-create
                             'gdb-locals-buffer
                             gdb-thread-number) t)))
    map))

(defvar gdb-registers-header
  (list
   (gdb-propertize-header "Locals" gdb-locals-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)
   " "
   (gdb-propertize-header "Registers" gdb-registers-buffer
			  nil nil mode-line)))

(define-derived-mode gdb-registers-mode gdb-parent-mode "Registers"
  "Major mode for gdb registers."
  (setq header-line-format gdb-registers-header)
  'gdb-invalidate-registers)

(defun gdb-registers-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "registers of " (gdb-get-target-string))))

(def-gdb-display-buffer
  gdb-display-registers-buffer
  'gdb-registers-buffer
  "Display integer register contents.")

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-registers-buffer
  'gdb-registers-buffer nil t)

(def-gdb-frame-for-buffer
  gdb-frame-registers-buffer
  'gdb-registers-buffer
  "Display integer register contents in a new frame.")

;; Needs GDB 6.4 onwards (used to fail with no stack).
(defun gdb-get-changed-registers ()
  (when (and (gdb-get-buffer 'gdb-registers-buffer)
	     (not (gdb-pending-p 'gdb-get-changed-registers)))
    (gdb-input "-data-list-changed-registers"
	       'gdb-changed-registers-handler)
    (gdb-add-pending 'gdb-get-changed-registers)))

(defun gdb-changed-registers-handler ()
  (gdb-delete-pending 'gdb-get-changed-registers)
  (setq gdb-changed-registers nil)
  (dolist (register-number
           (bindat-get-field (gdb-json-partial-output) 'changed-registers))
    (push register-number gdb-changed-registers)))

(defun gdb-register-names-handler ()
  ;; Don't use gdb-pending-triggers because this handler is called
  ;; only once (in gdb-init-1)
  (setq gdb-register-names nil)
  (dolist (register-name
           (bindat-get-field (gdb-json-partial-output) 'register-names))
    (push register-name gdb-register-names))
  (setq gdb-register-names (reverse gdb-register-names)))


(defun gdb-get-source-file-list ()
  "Create list of source files for current GDB session.
If buffers already exist for any of these files, gud-minor-mode
is set in them."
  (goto-char (point-min))
  (while (re-search-forward gdb-source-file-regexp nil t)
    (push (match-string 1) gdb-source-file-list))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (member buffer-file-name gdb-source-file-list)
	(gdb-init-buffer)))))

(defun gdb-get-main-selected-frame ()
  "Trigger for `gdb-frame-handler' which uses main current
thread. Called from `gdb-update'."
  (if (not (gdb-pending-p 'gdb-get-main-selected-frame))
      (progn
	(gdb-input (gdb-current-context-command "-stack-info-frame")
		   'gdb-frame-handler)
	(gdb-add-pending 'gdb-get-main-selected-frame))))

(defun gdb-frame-handler ()
  "Sets `gdb-selected-frame' and `gdb-selected-file' to show
overlay arrow in source buffer."
  (gdb-delete-pending 'gdb-get-main-selected-frame)
  (let ((frame (bindat-get-field (gdb-json-partial-output) 'frame)))
    (when frame
      (setq gdb-selected-frame (bindat-get-field frame 'func))
      (setq gdb-selected-file (bindat-get-field frame 'fullname))
      (setq gdb-frame-number (bindat-get-field frame 'level))
      (setq gdb-frame-address (bindat-get-field frame 'addr))
      (let ((line (bindat-get-field frame 'line)))
        (setq gdb-selected-line (and line (string-to-number line)))
        (when (and gdb-selected-file gdb-selected-line)
          (setq gud-last-frame (cons gdb-selected-file gdb-selected-line))
          (gud-display-frame)))
      (if gud-overlay-arrow-position
          (let ((buffer (marker-buffer gud-overlay-arrow-position))
                (position (marker-position gud-overlay-arrow-position)))
            (when buffer
              (with-current-buffer buffer
                (setq fringe-indicator-alist
                      (if (string-equal gdb-frame-number "0")
                          nil
                        '((overlay-arrow . hollow-right-triangle))))
                (setq gud-overlay-arrow-position (make-marker))
                (set-marker gud-overlay-arrow-position position))))))))

(defvar gdb-prompt-name-regexp "value=\"\\(.*?\\)\"")

(defun gdb-get-prompt ()
  "Find prompt for GDB session."
  (goto-char (point-min))
  (setq gdb-prompt-name nil)
  (re-search-forward gdb-prompt-name-regexp nil t)
  (setq gdb-prompt-name (match-string 1))
  ;; Insert first prompt.
  (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))

;;;; Window management
(defun gdb-display-buffer (buf dedicated &optional frame)
  "Show buffer BUF.

If BUF is already displayed in some window, show it, deiconifying
the frame if necessary. Otherwise, find least recently used
window and show BUF there, if the window is not used for GDB
already, in which case that window is split first."
  (let ((answer (get-buffer-window buf (or frame 0))))
    (if answer
	(display-buffer buf nil (or frame 0)) ;Deiconify frame if necessary.
      (let ((window (get-lru-window)))
	(if (eq (buffer-local-value 'gud-minor-mode (window-buffer window))
                'gdbmi)
	    (let ((largest (get-largest-window)))
	      (setq answer (split-window largest))
	      (set-window-buffer answer buf)
	      (set-window-dedicated-p answer dedicated)
	      answer)
	  (set-window-buffer window buf)
	  window)))))

(defun gdb-preempt-existing-or-display-buffer (buf &optional split-horizontal)
  "Find window displaying a buffer with the same
`gdb-buffer-type' as BUF and show BUF there. If no such window
exists, just call `gdb-display-buffer' for BUF. If the window
found is already dedicated, split window according to
SPLIT-HORIZONTAL and show BUF in the new window."
  (if buf
      (when (not (get-buffer-window buf))
        (let* ((buf-type (gdb-buffer-type buf))
               (existing-window
                (get-window-with-predicate
                 #'(lambda (w)
                     (and (eq buf-type
                              (gdb-buffer-type (window-buffer w)))
                          (not (window-dedicated-p w)))))))
          (if existing-window
              (set-window-buffer existing-window buf)
            (let ((dedicated-window
                   (get-window-with-predicate
                    #'(lambda (w)
                        (eq buf-type
                            (gdb-buffer-type (window-buffer w)))))))
              (if dedicated-window
                  (set-window-buffer
                   (split-window dedicated-window nil split-horizontal) buf)
                (gdb-display-buffer buf t))))))
    (error "Null buffer")))

;;; Shared keymap initialization:

(let ((menu (make-sparse-keymap "GDB-Windows")))
  (define-key gud-menu-map [displays]
    `(menu-item "GDB-Windows" ,menu
		:visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb] '("Gdb" . gdb-display-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-display-threads-buffer))
  (define-key menu [memory] '("Memory" . gdb-display-memory-buffer))
  (define-key menu [disassembly]
    '("Disassembly" . gdb-display-disassembly-buffer))
  (define-key menu [registers] '("Registers" . gdb-display-registers-buffer))
  (define-key menu [inferior]
    '("IO" . gdb-display-io-buffer))
  (define-key menu [locals] '("Locals" . gdb-display-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-display-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . gdb-display-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "GDB-Frames")))
  (define-key gud-menu-map [frames]
    `(menu-item "GDB-Frames" ,menu
		:visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb] '("Gdb" . gdb-frame-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-frame-threads-buffer))
  (define-key menu [memory] '("Memory" . gdb-frame-memory-buffer))
  (define-key menu [disassembly]
    '("Disassembly" . gdb-frame-disassembly-buffer))
  (define-key menu [registers] '("Registers" . gdb-frame-registers-buffer))
  (define-key menu [inferior]
    '("IO" . gdb-frame-io-buffer))
  (define-key menu [locals] '("Locals" . gdb-frame-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-frame-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . gdb-frame-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "GDB-MI")))
  (define-key menu [gdb-customize]
    '(menu-item "Customize" (lambda () (interactive) (customize-group 'gdb))
      :help "Customize Gdb Graphical Mode options."))
  (define-key menu [gdb-many-windows]
    '(menu-item "Display Other Windows" gdb-many-windows
      :help "Toggle display of locals, stack and breakpoint information"
      :button (:toggle . gdb-many-windows)))
  (define-key menu [gdb-restore-windows]
    '(menu-item "Restore Window Layout" gdb-restore-windows
      :help "Restore standard layout for debug session."))
  (define-key menu [sep1]
    '(menu-item "--"))
  (define-key menu [all-threads]
    '(menu-item "GUD controls all threads"
      (lambda ()
        (interactive)
        (setq gdb-gud-control-all-threads t))
      :help "GUD start/stop commands apply to all threads"
      :button (:radio . gdb-gud-control-all-threads)))
  (define-key menu [current-thread]
    '(menu-item "GUD controls current thread"
      (lambda ()
        (interactive)
        (setq gdb-gud-control-all-threads nil))
      :help "GUD start/stop commands apply to current thread only"
      :button (:radio . (not gdb-gud-control-all-threads))))
  (define-key menu [sep2]
    '(menu-item "--"))
  (define-key menu [gdb-customize-reasons]
    '(menu-item "Customize switching..."
      (lambda ()
        (interactive)
        (customize-option 'gdb-switch-reasons))))
  (define-key menu [gdb-switch-when-another-stopped]
    (menu-bar-make-toggle gdb-toggle-switch-when-another-stopped
                          gdb-switch-when-another-stopped
                          "Automatically switch to stopped thread"
                          "GDB thread switching %s"
                          "Switch to stopped thread"))
  (define-key gud-menu-map [mi]
    `(menu-item "GDB-MI" ,menu :visible (eq gud-minor-mode 'gdbmi))))

;; TODO Fit these into tool-bar-local-item-from-menu call in gud.el.
;; GDB-MI menu will need to be moved to gud.el. We can't use
;; tool-bar-local-item-from-menu here because it appends new buttons
;; to toolbar from right to left while we want our A/T throttle to
;; show up right before Run button.
(define-key-after gud-tool-bar-map [all-threads]
  '(menu-item "Switch to non-stop/A mode" gdb-control-all-threads
    :image (find-image '((:type xpm :file "gud/thread.xpm")))
    :visible (and (eq gud-minor-mode 'gdbmi)
                  gdb-non-stop
                  (not gdb-gud-control-all-threads)))
  'run)

(define-key-after gud-tool-bar-map [current-thread]
  '(menu-item "Switch to non-stop/T mode" gdb-control-current-thread
    :image (find-image '((:type xpm :file "gud/all.xpm")))
    :visible (and (eq gud-minor-mode 'gdbmi)
                  gdb-non-stop
                  gdb-gud-control-all-threads))
  'all-threads)

(defun gdb-frame-gdb-buffer ()
  "Display GUD buffer in a new frame."
  (interactive)
  (display-buffer-other-frame gud-comint-buffer))

(defun gdb-display-gdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (pop-to-buffer gud-comint-buffer nil 0))

(defun gdb-set-window-buffer (name &optional ignore-dedicated window)
  "Set buffer of selected window to NAME and dedicate window.

When IGNORE-DEDICATED is non-nil, buffer is set even if selected
window is dedicated."
  (unless window (setq window (selected-window)))
  (when ignore-dedicated
    (set-window-dedicated-p window nil))
  (set-window-buffer window (get-buffer name))
  (set-window-dedicated-p window t))

(defun gdb-setup-windows ()
  "Layout the window pattern for `gdb-many-windows'."
  (gdb-display-locals-buffer)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (delete-other-windows)
  ;; Don't dedicate.
  (switch-to-buffer gud-comint-buffer)
  (let ((win0 (selected-window))
        (win1 (split-window nil ( / ( * (window-height) 3) 4)))
        (win2 (split-window nil ( / (window-height) 3)))
        (win3 (split-window-right)))
    (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
    (select-window win2)
    (set-window-buffer
     win2
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win4 (split-window-right)))
      (gdb-set-window-buffer
       (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
    (select-window win1)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (let ((win5 (split-window-right)))
      (gdb-set-window-buffer (if gdb-show-threads-by-default
                                 (gdb-threads-buffer-name)
                               (gdb-breakpoints-buffer-name))
                             nil win5))
    (select-window win0)))

(defcustom gdb-many-windows nil
  "If nil just pop up the GUD buffer unless `gdb-show-main' is t.
In this case it starts with two windows: one displaying the GUD
buffer and the other with the source file with the main routine
of the debugged program.  Non-nil means display the layout shown for
`gdb'."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defun gdb-many-windows (arg)
  "Toggle the number of windows in the basic arrangement.
With arg, display additional buffers iff arg is positive."
  (interactive "P")
  (setq gdb-many-windows
        (if (null arg)
            (not gdb-many-windows)
          (> (prefix-numeric-value arg) 0)))
  (message (format "Display of other windows %sabled"
                   (if gdb-many-windows "en" "dis")))
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer))
      (condition-case nil
          (gdb-restore-windows)
        (error nil))))

(defun gdb-restore-windows ()
  "Restore the basic arrangement of windows used by gdb.
This arrangement depends on the value of `gdb-many-windows'."
  (interactive)
  (switch-to-buffer gud-comint-buffer) ;Select the right window and frame.
  (delete-other-windows)
  (if gdb-many-windows
      (gdb-setup-windows)
    (when (or gud-last-last-frame gdb-show-main)
      (let ((win (split-window)))
        (set-window-buffer
         win
         (if gud-last-last-frame
             (gud-find-file (car gud-last-last-frame))
           (gud-find-file gdb-main-file)))
        (setq gdb-source-window win)))))

;; Called from `gud-sentinel' in gud.el:
(defun gdb-reset ()
  "Exit a debugging session cleanly.
Kills the gdb buffers, and resets variables and the source buffers."
  ;; The gdb-inferior buffer has a pty hooked up to the main gdb
  ;; process.  This pty must be deleted explicitly.
  (let ((pty (get-process "gdb-inferior")))
    (if pty (delete-process pty)))
  ;; Find gdb-mi buffers and kill them.
  (dolist (buffer (buffer-list))
    (unless (eq buffer gud-comint-buffer)
      (with-current-buffer buffer
        (if (eq gud-minor-mode 'gdbmi)
            (if (string-match "\\` ?\\*.+\\*\\'" (buffer-name))
                (kill-buffer nil)
              (gdb-remove-breakpoint-icons (point-min) (point-max) t)
              (setq gud-minor-mode nil)
              (kill-local-variable 'tool-bar-map)
              (kill-local-variable 'gdb-define-alist))))))
  (setq gdb-disassembly-position nil)
  (setq overlay-arrow-variable-list
        (delq 'gdb-disassembly-position overlay-arrow-variable-list))
  (setq fringe-indicator-alist '((overlay-arrow . right-triangle)))
  (setq gdb-stack-position nil)
  (setq overlay-arrow-variable-list
        (delq 'gdb-stack-position overlay-arrow-variable-list))
  (setq gdb-thread-position nil)
  (setq overlay-arrow-variable-list
        (delq 'gdb-thread-position overlay-arrow-variable-list))
  (if (boundp 'speedbar-frame) (speedbar-timer-fn))
  (setq gud-running nil)
  (setq gdb-active-process nil)
  (remove-hook 'after-save-hook 'gdb-create-define-alist t))

(defun gdb-get-source-file ()
  "Find the source file where the program starts and display it with related
buffers, if required."
  (goto-char (point-min))
  (if (re-search-forward gdb-source-file-regexp nil t)
      (setq gdb-main-file (match-string 1)))
  (if gdb-many-windows
      (gdb-setup-windows)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (if (and gdb-show-main gdb-main-file)
        (let ((pop-up-windows t))
          (display-buffer (gud-find-file gdb-main-file)))))
  (gdb-force-mode-line-update
   (propertize "ready" 'face font-lock-variable-name-face)))

;;from put-image
(defun gdb-put-string (putstring pos &optional dprop &rest sprops)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' string that has a `display' property whose value is
PUTSTRING."
  (let ((string (make-string 1 ?x))
        (buffer (current-buffer)))
    (setq putstring (copy-sequence putstring))
    (let ((overlay (make-overlay pos pos buffer))
          (prop (or dprop
                    (list (list 'margin 'left-margin) putstring))))
      (put-text-property 0 1 'display prop string)
      (if sprops
          (add-text-properties 0 1 sprops string))
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string string))))

;;from remove-images
(defun gdb-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdb-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (delete-overlay overlay))))

(defun gdb-put-breakpoint-icon (enabled bptno &optional line)
  (let* ((posns (gdb-line-posns (or line (line-number-at-pos))))
         (start (- (car posns) 1))
         (end (+ (cdr posns) 1))
         (putstring (if enabled "B" "b"))
         (source-window (get-buffer-window (current-buffer) 0)))
    (add-text-properties
     0 1 '(help-echo "mouse-1: clear bkpt, mouse-3: enable/disable bkpt")
     putstring)
    (if enabled
        (add-text-properties
         0 1 `(gdb-bptno ,bptno gdb-enabled t) putstring)
      (add-text-properties
       0 1 `(gdb-bptno ,bptno gdb-enabled nil) putstring))
    (gdb-remove-breakpoint-icons start end)
    (if (display-images-p)
        (if (>= (or left-fringe-width
                    (if source-window (car (window-fringes source-window)))
                    gdb-buffer-fringe-width) 8)
            (gdb-put-string
             nil (1+ start)
             `(left-fringe breakpoint
                           ,(if enabled
                                'breakpoint-enabled
                              'breakpoint-disabled))
             'gdb-bptno bptno
             'gdb-enabled enabled)
          (when (< left-margin-width 2)
            (save-current-buffer
              (setq left-margin-width 2)
              (if source-window
                  (set-window-margins
                   source-window
                   left-margin-width right-margin-width))))
          (put-image
           (if enabled
               (or breakpoint-enabled-icon
                   (setq breakpoint-enabled-icon
                         (find-image `((:type xpm :data
                                        ,breakpoint-xpm-data
                                        :ascent 100 :pointer hand)
                                       (:type pbm :data
                                        ,breakpoint-enabled-pbm-data
                                        :ascent 100 :pointer hand)))))
             (or breakpoint-disabled-icon
                 (setq breakpoint-disabled-icon
                       (find-image `((:type xpm :data
                                      ,breakpoint-xpm-data
                                      :conversion disabled
                                      :ascent 100 :pointer hand)
                                     (:type pbm :data
                                      ,breakpoint-disabled-pbm-data
                                      :ascent 100 :pointer hand))))))
           (+ start 1)
           putstring
           'left-margin))
      (when (< left-margin-width 2)
        (save-current-buffer
          (setq left-margin-width 2)
          (let ((window (get-buffer-window (current-buffer) 0)))
            (if window
                (set-window-margins
                 window left-margin-width right-margin-width)))))
      (gdb-put-string
       (propertize putstring
                   'face (if enabled
                             'breakpoint-enabled 'breakpoint-disabled))
       (1+ start)))))

(defun gdb-remove-breakpoint-icons (start end &optional remove-margin)
  (gdb-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (if window
          (set-window-margins
           window left-margin-width right-margin-width)))))


;;; Functions for inline completion.

(defvar gud-gdb-fetch-lines-in-progress)
(defvar gud-gdb-fetch-lines-string)
(defvar gud-gdb-fetch-lines-break)
(defvar gud-gdb-fetched-lines)

(defun gud-gdbmi-completions (context command)
  "Completion table for GDB/MI commands.
COMMAND is the prefix for which we seek completion.
CONTEXT is the text before COMMAND on the line."
  (let ((gud-gdb-fetch-lines-in-progress t)
	(gud-gdb-fetch-lines-string nil)
	(gud-gdb-fetch-lines-break (length context))
	(gud-gdb-fetched-lines nil)
	;; This filter dumps output lines to `gud-gdb-fetched-lines'.
	(gud-marker-filter #'gud-gdbmi-fetch-lines-filter)
	complete-list)
    (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
      (gdb-input (concat "complete " context command)
		 (lambda () (setq gud-gdb-fetch-lines-in-progress nil)))
      (while gud-gdb-fetch-lines-in-progress
	(accept-process-output (get-buffer-process gud-comint-buffer))))
    (gud-gdb-completions-1 gud-gdb-fetched-lines)))

(defun gud-gdbmi-fetch-lines-filter (string)
  "Custom filter function for `gud-gdbmi-completions'."
  (setq string (concat gud-gdb-fetch-lines-string
		       (gud-gdbmi-marker-filter string)))
  (while (string-match "\n" string)
    (push (substring string gud-gdb-fetch-lines-break (match-beginning 0))
	  gud-gdb-fetched-lines)
    (setq string (substring string (match-end 0))))
  "")

(provide 'gdb-mi)

;;; gdb-mi.el ends here
