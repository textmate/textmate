;;; server.el --- Lisp code for GNU Emacs running as server process -*- lexical-binding: t -*-

;; Copyright (C) 1986-1987, 1992, 1994-2012  Free Software Foundation, Inc.

;; Author: William Sommerfeld <wesommer@athena.mit.edu>
;; Maintainer: FSF
;; Keywords: processes

;; Changes by peck@sun.com and by rms.
;; Overhaul by Karoly Lorentey <lorentey@elte.hu> for multi-tty support.

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

;; This Lisp code is run in Emacs when it is to operate as
;; a server for other processes.

;; Load this library and do M-x server-edit to enable Emacs as a server.
;; Emacs opens up a socket for communication with clients.  If there are no
;; client buffers to edit, server-edit acts like (switch-to-buffer
;; (other-buffer))

;; When some other program runs "the editor" to edit a file,
;; "the editor" can be the Emacs client program ../lib-src/emacsclient.
;; This program transmits the file names to Emacs through
;; the server subprocess, and Emacs visits them and lets you edit them.

;; Note that any number of clients may dispatch files to Emacs to be edited.

;; When you finish editing a Server buffer, again call server-edit
;; to mark that buffer as done for the client and switch to the next
;; Server buffer.  When all the buffers for a client have been edited
;; and exited with server-edit, the client "editor" will return
;; to the program that invoked it.

;; Your editing commands and Emacs's display output go to and from
;; the terminal in the usual way.  Thus, server operation is possible
;; only when Emacs can talk to the terminal at the time you invoke
;; the client.  This is possible in four cases:

;; 1. On a window system, where Emacs runs in one window and the
;; program that wants to use "the editor" runs in another.

;; 2. On a multi-terminal system, where Emacs runs on one terminal and the
;; program that wants to use "the editor" runs on another.

;; 3. When the program that wants to use "the editor" is running
;; as a subprocess of Emacs.

;; 4. On a system with job control, when Emacs is suspended, the program
;; that wants to use "the editor" will stop and display
;; "Waiting for Emacs...".  It can then be suspended, and Emacs can be
;; brought into the foreground for editing.  When done editing, Emacs is
;; suspended again, and the client program is brought into the foreground.

;; The buffer local variable "server-buffer-clients" lists
;; the clients who are waiting for this buffer to be edited.
;; The global variable "server-clients" lists all the waiting clients,
;; and which files are yet to be edited for each.

;; Todo:

;; - handle command-line-args-left.
;; - move most of the args processing and decision making from emacsclient.c
;;   to here.
;; - fix up handling of the client's environment (place it in the terminal?).

;;; Code:

(eval-when-compile (require 'cl))

(defgroup server nil
  "Emacs running as a server process."
  :group 'external)

(defcustom server-use-tcp nil
  "If non-nil, use TCP sockets instead of local sockets."
  :set #'(lambda (sym val)
           (unless (featurep 'make-network-process '(:family local))
             (setq val t)
             (unless load-in-progress
               (message "Local sockets unsupported, using TCP sockets")))
           (when val (random t))
           (set-default sym val))
  :group 'server
  :type 'boolean
  :version "22.1")

(defcustom server-host nil
  "The name or IP address to use as host address of the server process.
If set, the server accepts remote connections; otherwise it is local."
  :group 'server
  :type '(choice
          (string :tag "Name or IP address")
          (const :tag "Local" nil))
  :version "22.1")
;;;###autoload
(put 'server-host 'risky-local-variable t)

(defcustom server-port nil
  "The port number that the server process should listen on.
This variable only takes effect when the Emacs server is using
TCP instead of local sockets.  A nil value means to use a random
port number."
  :group 'server
  :type '(choice
          (string :tag "Port number")
          (const :tag "Random" nil))
  :version "24.1")
;;;###autoload
(put 'server-port 'risky-local-variable t)

(defcustom server-auth-dir (locate-user-emacs-file "server/")
  "Directory for server authentication files.
We only use this if `server-use-tcp' is non-nil.
Otherwise we use `server-socket-dir'.

NOTE: On FAT32 filesystems, directories are not secure;
files can be read and modified by any user or process.
It is strongly suggested to set `server-auth-dir' to a
directory residing in a NTFS partition instead."
  :group 'server
  :type 'directory
  :version "22.1")
;;;###autoload
(put 'server-auth-dir 'risky-local-variable t)

(defcustom server-raise-frame t
  "If non-nil, raise frame when switching to a buffer."
  :group 'server
  :type 'boolean
  :version "22.1")

(defcustom server-visit-hook nil
  "Hook run when visiting a file for the Emacs server."
  :group 'server
  :type 'hook)

(defcustom server-switch-hook nil
  "Hook run when switching to a buffer for the Emacs server."
  :group 'server
  :type 'hook)

(defcustom server-done-hook nil
  "Hook run when done editing a buffer for the Emacs server."
  :group 'server
  :type 'hook)

(defvar server-process nil
  "The current server process.")

(defvar server-clients nil
  "List of current server clients.
Each element is a process.")

(defvar server-buffer-clients nil
  "List of client processes requesting editing of current buffer.")
(make-variable-buffer-local 'server-buffer-clients)
;; Changing major modes should not erase this local.
(put 'server-buffer-clients 'permanent-local t)

(defcustom server-window nil
  "Specification of the window to use for selecting Emacs server buffers.
If nil, use the selected window.
If it is a function, it should take one argument (a buffer) and
display and select it.  A common value is `pop-to-buffer'.
If it is a window, use that.
If it is a frame, use the frame's selected window.

It is not meaningful to set this to a specific frame or window with Custom.
Only programs can do so."
  :group 'server
  :version "22.1"
  :type '(choice (const :tag "Use selected window"
			:match (lambda (widget value)
				 (not (functionp value)))
			nil)
		 (function-item :tag "Display in new frame" switch-to-buffer-other-frame)
		 (function-item :tag "Use pop-to-buffer" pop-to-buffer)
		 (function :tag "Other function")))

(defcustom server-temp-file-regexp "^/tmp/Re\\|/draft$"
  "Regexp matching names of temporary files.
These are deleted and reused after each edit by the programs that
invoke the Emacs server."
  :group 'server
  :type 'regexp)

(defcustom server-kill-new-buffers t
  "Whether to kill buffers when done with them.
If non-nil, kill a buffer unless it already existed before editing
it with the Emacs server.  If nil, kill only buffers as specified by
`server-temp-file-regexp'.
Please note that only buffers that still have a client are killed,
i.e. buffers visited with \"emacsclient --no-wait\" are never killed
in this way."
  :group 'server
  :type 'boolean
  :version "21.1")

(or (assq 'server-buffer-clients minor-mode-alist)
    (push '(server-buffer-clients " Server") minor-mode-alist))

(defvar server-existing-buffer nil
  "Non-nil means the buffer existed before the server was asked to visit it.
This means that the server should not kill the buffer when you say you
are done with it in the server.")
(make-variable-buffer-local 'server-existing-buffer)

(defcustom server-name "server"
  "The name of the Emacs server, if this Emacs process creates one.
The command `server-start' makes use of this.  It should not be
changed while a server is running."
  :group 'server
  :type 'string
  :version "23.1")

;; We do not use `temporary-file-directory' here, because emacsclient
;; does not read the init file.
(defvar server-socket-dir
  (and (featurep 'make-network-process '(:family local))
       (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
  "The directory in which to place the server socket.
If local sockets are not supported, this is nil.")

(defun server-clients-with (property value)
  "Return a list of clients with PROPERTY set to VALUE."
  (let (result)
    (dolist (proc server-clients)
      (when (equal value (process-get proc property))
	(push proc result)))
    result))

(defun server-add-client (proc)
  "Create a client for process PROC, if it doesn't already have one.
New clients have no properties."
  (add-to-list 'server-clients proc))

(defmacro server-with-environment (env vars &rest body)
  "Evaluate BODY with environment variables VARS set to those in ENV.
The environment variables are then restored to their previous values.

VARS should be a list of strings.
ENV should be in the same format as `process-environment'."
  (declare (indent 2))
  (let ((var (make-symbol "var"))
	(value (make-symbol "value")))
    `(let ((process-environment process-environment))
       (dolist (,var ,vars)
         (let ((,value (getenv-internal ,var ,env)))
           (push (if (stringp ,value)
                     (concat ,var "=" ,value)
                   ,var)
                 process-environment)))
       (progn ,@body))))

(defun server-delete-client (proc &optional noframe)
  "Delete PROC, including its buffers, terminals and frames.
If NOFRAME is non-nil, let the frames live.
Updates `server-clients'."
  (server-log (concat "server-delete-client" (if noframe " noframe")) proc)
  ;; Force a new lookup of client (prevents infinite recursion).
  (when (memq proc server-clients)
    (let ((buffers (process-get proc 'buffers)))

      ;; Kill the client's buffers.
      (dolist (buf buffers)
	(when (buffer-live-p buf)
	  (with-current-buffer buf
	    ;; Kill the buffer if necessary.
	    (when (and (equal server-buffer-clients
			      (list proc))
		       (or (and server-kill-new-buffers
				(not server-existing-buffer))
			   (server-temp-file-p))
		       (not (buffer-modified-p)))
	      (let (flag)
		(unwind-protect
		    (progn (setq server-buffer-clients nil)
			   (kill-buffer (current-buffer))
			   (setq flag t))
		  (unless flag
		    ;; Restore clients if user pressed C-g in `kill-buffer'.
		    (setq server-buffer-clients (list proc)))))))))

      ;; Delete the client's frames.
      (unless noframe
	(dolist (frame (frame-list))
	  (when (and (frame-live-p frame)
		     (equal proc (frame-parameter frame 'client)))
	    ;; Prevent `server-handle-delete-frame' from calling us
	    ;; recursively.
	    (set-frame-parameter frame 'client nil)
	    (delete-frame frame))))

      (setq server-clients (delq proc server-clients))

      ;; Delete the client's tty, except on Windows (both GUI and console),
      ;; where there's only one terminal and does not make sense to delete it.
      (unless (eq system-type 'windows-nt)
	(let ((terminal (process-get proc 'terminal)))
	  ;; Only delete the terminal if it is non-nil.
	  (when (and terminal (eq (terminal-live-p terminal) t))
	    (delete-terminal terminal))))

      ;; Delete the client's process.
      (if (eq (process-status proc) 'open)
	  (delete-process proc))

      (server-log "Deleted" proc))))

(defvar server-log-time-function 'current-time-string
  "Function to generate timestamps for `server-buffer'.")

(defconst server-buffer " *server*"
  "Buffer used internally by Emacs's server.
One use is to log the I/O for debugging purposes (see `server-log'),
the other is to provide a current buffer in which the process filter can
safely let-bind buffer-local variables like `default-directory'.")

(defvar server-log nil
  "If non-nil, log the server's inputs and outputs in the `server-buffer'.")

(defun server-log (string &optional client)
  "If `server-log' is non-nil, log STRING to `server-buffer'.
If CLIENT is non-nil, add a description of it to the logged message."
  (when server-log
    (with-current-buffer (get-buffer-create server-buffer)
      (goto-char (point-max))
      (insert (funcall server-log-time-function)
	      (cond
	       ((null client) " ")
	       ((listp client) (format " %s: " (car client)))
	       (t (format " %s: " client)))
	      string)
      (or (bolp) (newline)))))

(defun server-sentinel (proc msg)
  "The process sentinel for Emacs server connections."
  ;; If this is a new client process, set the query-on-exit flag to nil
  ;; for this process (it isn't inherited from the server process).
  (when (and (eq (process-status proc) 'open)
	     (process-query-on-exit-flag proc))
    (set-process-query-on-exit-flag proc nil))
  ;; Delete the associated connection file, if applicable.
  ;; Although there's no 100% guarantee that the file is owned by the
  ;; running Emacs instance, server-start uses server-running-p to check
  ;; for possible servers before doing anything, so it *should* be ours.
  (and (process-contact proc :server)
       (eq (process-status proc) 'closed)
       (ignore-errors
	 (delete-file (process-get proc :server-file))))
  (server-log (format "Status changed to %s: %s" (process-status proc) msg) proc)
  (server-delete-client proc))

(defun server--on-display-p (frame display)
  (and (equal (frame-parameter frame 'display) display)
       ;; Note: TTY frames still get a `display' parameter set to the value of
       ;; $DISPLAY.  This is useful when running from that tty frame
       ;; sub-processes that want to connect to the X server, but that means we
       ;; have to be careful here not to be tricked into thinking those frames
       ;; are on `display'.
       (not (eq (framep frame) t))))

(defun server-select-display (display)
  ;; If the current frame is on `display' we're all set.
  ;; Similarly if we are unable to open frames on other displays, there's
  ;; nothing more we can do.
  (unless (or (not (fboundp 'make-frame-on-display))
              (server--on-display-p (selected-frame) display))
    ;; Otherwise, look for an existing frame there and select it.
    (dolist (frame (frame-list))
      (when (server--on-display-p frame display)
	(select-frame frame)))
    ;; If there's no frame on that display yet, create and select one.
    (unless (server--on-display-p (selected-frame) display)
      (let* ((buffer (generate-new-buffer " *server-dummy*"))
             (frame (make-frame-on-display
                     display
                     ;; Make it display (and remember) some dummy buffer, so
                     ;; we can detect later if the frame is in use or not.
                     `((server-dummy-buffer . ,buffer)
                       ;; This frame may be deleted later (see
                       ;; server-unselect-display) so we want it to be as
                       ;; unobtrusive as possible.
                       (visibility . nil)))))
        (select-frame frame)
        (set-window-buffer (selected-window) buffer)
        frame))))

(defun server-unselect-display (frame)
  (when (frame-live-p frame)
    ;; If the temporary frame is in use (displays something real), make it
    ;; visible.  If not (which can happen if the user's customizations call
    ;; pop-to-buffer etc.), delete it to avoid preserving the connection after
    ;; the last real frame is deleted.

    ;; Rewritten to avoid inadvertently killing the current buffer after
    ;; `delete-frame' removed FRAME (Bug#10729).
    (let ((buffer (frame-parameter frame 'server-dummy-buffer)))
      (if (and (one-window-p 'nomini frame)
	       (eq (window-buffer (frame-first-window frame)) buffer))
	  ;; The temp frame still only shows one buffer, and that is the
	  ;; internal temp buffer.
	  (delete-frame frame)
	(set-frame-parameter frame 'visibility t)
	(set-frame-parameter frame 'server-dummy-buffer nil))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))))

(defun server-handle-delete-frame (frame)
  "Delete the client connection when the emacsclient frame is deleted.
\(To be used from `delete-frame-functions'.)"
  (let ((proc (frame-parameter frame 'client)))
    (when (and (frame-live-p frame)
	       proc
	       ;; See if this is the last frame for this client.
	       (>= 1 (let ((frame-num 0))
		       (dolist (f (frame-list))
			 (when (eq proc (frame-parameter f 'client))
			   (setq frame-num (1+ frame-num))))
		       frame-num)))
      (server-log (format "server-handle-delete-frame, frame %s" frame) proc)
      (server-delete-client proc 'noframe)))) ; Let delete-frame delete the frame later.

(defun server-handle-suspend-tty (terminal)
  "Notify the client process that its tty device is suspended."
  (dolist (proc (server-clients-with 'terminal terminal))
    (server-log (format "server-handle-suspend-tty, terminal %s" terminal)
                proc)
    (condition-case nil
	(server-send-string proc "-suspend \n")
      (file-error                       ;The pipe/socket was closed.
       (ignore-errors (server-delete-client proc))))))

(defun server-unquote-arg (arg)
  "Remove &-quotation from ARG.
See `server-quote-arg' and `server-process-filter'."
  (replace-regexp-in-string
   "&." (lambda (s)
	  (case (aref s 1)
	    (?& "&")
	    (?- "-")
	    (?n "\n")
	    (t " ")))
   arg t t))

(defun server-quote-arg (arg)
  "In ARG, insert a & before each &, each space, each newline, and -.
Change spaces to underscores, too, so that the return value never
contains a space.

See `server-unquote-arg' and `server-process-filter'."
  (replace-regexp-in-string
   "[-&\n ]" (lambda (s)
	       (case (aref s 0)
		 (?& "&&")
		 (?- "&-")
		 (?\n "&n")
		 (?\s "&_")))
   arg t t))

(defun server-send-string (proc string)
  "A wrapper around `process-send-string' for logging."
  (server-log (concat "Sent " string) proc)
  (process-send-string proc string))

(defun server-ensure-safe-dir (dir)
  "Make sure DIR is a directory with no race-condition issues.
Creates the directory if necessary and makes sure:
- there's no symlink involved
- it's owned by us
- it's not readable/writable by anybody else."
  (setq dir (directory-file-name dir))
  (let ((attrs (file-attributes dir 'integer)))
    (unless attrs
      (letf (((default-file-modes) ?\700)) (make-directory dir t))
      (setq attrs (file-attributes dir 'integer)))

    ;; Check that it's safe for use.
    (let* ((uid (nth 2 attrs))
	   (w32 (eq system-type 'windows-nt))
	   (safe (catch :safe
		   (unless (eq t (car attrs))   ; is a dir?
		     (throw :safe nil))
		   (when (and w32 (zerop uid))  ; on FAT32?
		     (display-warning
		      'server
		      (format "Using `%s' to store Emacs-server authentication files.
Directories on FAT32 filesystems are NOT secure against tampering.
See variable `server-auth-dir' for details."
			      (file-name-as-directory dir))
		      :warning)
		     (throw :safe t))
		   (unless (or (= uid (user-uid)) ; is the dir ours?
			       (and w32
				    ;; Files created on Windows by
				    ;; Administrator (RID=500) have
				    ;; the Administrators (RID=544)
				    ;; group recorded as the owner.
				    (= uid 544) (= (user-uid) 500)))
		     (throw :safe nil))
		   (when w32                    ; on NTFS?
		     (throw :safe t))
		   (unless (zerop (logand ?\077 (file-modes dir)))
		     (throw :safe nil))
		   t)))
      (unless safe
	(error "The directory `%s' is unsafe" dir)))))

;;;###autoload
(defun server-start (&optional leave-dead inhibit-prompt)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs
job.  To use the server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Optional argument LEAVE-DEAD (interactively, a prefix arg) means just
kill any existing server communications subprocess.

If a server is already running, restart it.  If clients are
running, ask the user for confirmation first, unless optional
argument INHIBIT-PROMPT is non-nil.

To force-start a server, do \\[server-force-delete] and then
\\[server-start]."
  (interactive "P")
  (when (or (not server-clients)
	    ;; Ask the user before deleting existing clients---except
	    ;; when we can't get user input, which may happen when
	    ;; doing emacsclient --eval "(kill-emacs)" in daemon mode.
	    (cond
	     ((and (daemonp)
		   (null (cdr (frame-list)))
		   (eq (selected-frame) terminal-frame))
	      leave-dead)
	     (inhibit-prompt t)
	     (t (yes-or-no-p
		 "The current server still has clients; delete them? "))))
    (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
	   (server-file (expand-file-name server-name server-dir)))
      (when server-process
	;; kill it dead!
	(ignore-errors (delete-process server-process)))
      ;; Delete the socket files made by previous server invocations.
      (if (not (eq t (server-running-p server-name)))
	  ;; Remove any leftover socket or authentication file
	  (ignore-errors
	    (let (delete-by-moving-to-trash)
	      (delete-file server-file)))
	(setq server-mode nil) ;; already set by the minor mode code
	(display-warning
	 'server
	 (concat "Unable to start the Emacs server.\n"
		 (format "There is an existing Emacs server, named %S.\n"
			 server-name)
		 "To start the server in this Emacs process, stop the existing
server or call `M-x server-force-delete' to forcibly disconnect it.")
	 :warning)
	(setq leave-dead t))
      ;; If this Emacs already had a server, clear out associated status.
      (while server-clients
	(server-delete-client (car server-clients)))
      ;; Now any previous server is properly stopped.
      (if leave-dead
	  (progn
	    (unless (eq t leave-dead) (server-log (message "Server stopped")))
	    (setq server-process nil))
	;; Make sure there is a safe directory in which to place the socket.
	(server-ensure-safe-dir server-dir)
	(when server-process
	  (server-log (message "Restarting server")))
	(letf (((default-file-modes) ?\700))
	  (add-hook 'suspend-tty-functions 'server-handle-suspend-tty)
	  (add-hook 'delete-frame-functions 'server-handle-delete-frame)
	  (add-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
	  (add-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)
	  (add-hook 'kill-emacs-hook 'server-force-stop) ;Cleanup upon exit.
	  (setq server-process
		(apply #'make-network-process
		       :name server-name
		       :server t
		       :noquery t
		       :sentinel 'server-sentinel
		       :filter 'server-process-filter
		       ;; We must receive file names without being decoded.
		       ;; Those are decoded by server-process-filter according
		       ;; to file-name-coding-system.  Also don't get
		       ;; confused by CRs since we don't quote them.
		       :coding 'raw-text-unix
		       ;; The other args depend on the kind of socket used.
		       (if server-use-tcp
			   (list :family 'ipv4  ;; We're not ready for IPv6 yet
				 :service (or server-port t)
				 :host (or server-host 'local)
				 :plist '(:authenticated nil))
			 (list :family 'local
			       :service server-file
			       :plist '(:authenticated t)))))
	  (unless server-process (error "Could not start server process"))
	  (process-put server-process :server-file server-file)
	  (when server-use-tcp
	    (let ((auth-key
		   (loop
		    ;; The auth key is a 64-byte string of random chars in the
		    ;; range `!'..`~'.
		    repeat 64
		    collect (+ 33 (random 94)) into auth
		    finally return (concat auth))))
	      (process-put server-process :auth-key auth-key)
	      (with-temp-file server-file
		(set-buffer-multibyte nil)
		(setq buffer-file-coding-system 'no-conversion)
		(insert (format-network-address
			 (process-contact server-process :local))
			" " (number-to-string (emacs-pid)) ; Kept for compatibility
			"\n" auth-key)))))))))

(defun server-force-stop ()
  "Kill all connections to the current server.
This function is meant to be called from `kill-emacs-hook'."
  (server-start t t))

;;;###autoload
(defun server-force-delete (&optional name)
  "Unconditionally delete connection file for server NAME.
If server is running, it is first stopped.
NAME defaults to `server-name'.  With argument, ask for NAME."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Server name: " nil nil server-name))))
  (when server-mode (with-temp-message nil (server-mode -1)))
  (let ((file (expand-file-name (or name server-name)
				(if server-use-tcp
				    server-auth-dir
				  server-socket-dir))))
    (condition-case nil
	(let (delete-by-moving-to-trash)
	  (delete-file file)
	  (message "Connection file %S deleted" file))
      (file-error
       (message "No connection file %S" file)))))

(defun server-running-p (&optional name)
  "Test whether server NAME is running.

Return values:
  nil              the server is definitely not running.
  t                the server seems to be running.
  something else   we cannot determine whether it's running without using
                   commands which may have to wait for a long time."
  (unless name (setq name server-name))
  (condition-case nil
      (if server-use-tcp
	  (with-temp-buffer
	    (insert-file-contents-literally (expand-file-name name server-auth-dir))
	    (or (and (looking-at "127\\.0\\.0\\.1:[0-9]+ \\([0-9]+\\)")
		     (assq 'comm
			   (process-attributes
			    (string-to-number (match-string 1))))
		     t)
		:other))
	(delete-process
	 (make-network-process
	  :name "server-client-test" :family 'local :server nil :noquery t
	  :service (expand-file-name name server-socket-dir)))
	t)
    (file-error nil)))

;;;###autoload
(define-minor-mode server-mode
  "Toggle Server mode.
With a prefix argument ARG, enable Server mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Server mode if ARG is omitted or nil.

Server mode runs a process that accepts commands from the
`emacsclient' program.  See Info node `Emacs server' and
`server-start' for details."
  :global t
  :group 'server
  :version "22.1"
  ;; Fixme: Should this check for an existing server socket and do
  ;; nothing if there is one (for multiple Emacs sessions)?
  (server-start (not server-mode)))

(defun server-eval-and-print (expr proc)
  "Eval EXPR and send the result back to client PROC."
  ;; While we're running asynchronously (from a process filter), it is likely
  ;; that the emacsclient command was run in response to a user
  ;; action, so the user probably knows that Emacs is processing this
  ;; emacsclient request, so if we get a C-g it's likely that the user
  ;; intended it to interrupt us rather than interrupt whatever Emacs
  ;; was doing before it started handling the process filter.
  ;; Hence `with-local-quit' (bug#6585).
  (let ((v (with-local-quit (eval (car (read-from-string expr))))))
    (when proc
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (pp v)
          (let ((text (buffer-substring-no-properties
                       (point-min) (point-max))))
            (server-reply-print (server-quote-arg text) proc)))))))

(defconst server-msg-size 1024
  "Maximum size of a message sent to a client.")

(defun server-reply-print (qtext proc)
  "Send a `-print QTEXT' command to client PROC.
QTEXT must be already quoted.
This handles splitting the command if it would be bigger than
`server-msg-size'."
  (let ((prefix "-print ")
	part)
    (while (> (+ (length qtext) (length prefix) 1) server-msg-size)
      ;; We have to split the string
      (setq part (substring qtext 0 (- server-msg-size (length prefix) 1)))
      ;; Don't split in the middle of a quote sequence
      (if (string-match "\\(^\\|[^&]\\)\\(&&\\)+$" part)
	  ;; There is an uneven number of & at the end
	  (setq part (substring part 0 -1)))
      (setq qtext (substring qtext (length part)))
      (server-send-string proc (concat prefix part "\n"))
      (setq prefix "-print-nonl "))
    (server-send-string proc (concat prefix qtext "\n"))))

(defun server-create-tty-frame (tty type proc)
  (unless tty
    (error "Invalid terminal device"))
  (unless type
    (error "Invalid terminal type"))
  (add-to-list 'frame-inherited-parameters 'client)
  (let ((frame
         (server-with-environment (process-get proc 'env)
				  '("LANG" "LC_CTYPE" "LC_ALL"
				    ;; For tgetent(3); list according to ncurses(3).
				    "BAUDRATE" "COLUMNS" "ESCDELAY" "HOME" "LINES"
				    "NCURSES_ASSUMED_COLORS" "NCURSES_NO_PADDING"
				    "NCURSES_NO_SETBUF" "TERM" "TERMCAP" "TERMINFO"
				    "TERMINFO_DIRS" "TERMPATH"
				    ;; rxvt wants these
				    "COLORFGBG" "COLORTERM")
				  (make-frame `((window-system . nil)
						(tty . ,tty)
						(tty-type . ,type)
						;; Ignore nowait here; we always need to
						;; clean up opened ttys when the client dies.
						(client . ,proc)
						;; This is a leftover from an earlier
						;; attempt at making it possible for process
						;; run in the server process to use the
						;; environment of the client process.
						;; It has no effect now and to make it work
						;; we'd need to decide how to make
						;; process-environment interact with client
						;; envvars, and then to change the
						;; C functions `child_setup' and
						;; `getenv_internal' accordingly.
						(environment . ,(process-get proc 'env)))))))

    ;; ttys don't use the `display' parameter, but callproc.c does to set
    ;; the DISPLAY environment on subprocesses.
    (set-frame-parameter frame 'display
                         (getenv-internal "DISPLAY" (process-get proc 'env)))
    (select-frame frame)
    (process-put proc 'frame frame)
    (process-put proc 'terminal (frame-terminal frame))

    ;; Display *scratch* by default.
    (switch-to-buffer (get-buffer-create "*scratch*") 'norecord)

    frame))

(defun server-create-window-system-frame (display nowait proc parent-id
						  &optional parameters)
  (add-to-list 'frame-inherited-parameters 'client)
  (if (not (fboundp 'make-frame-on-display))
      (progn
        ;; This emacs does not support X.
        (server-log "Window system unsupported" proc)
        (server-send-string proc "-window-system-unsupported \n")
        nil)
    ;; Flag frame as client-created, but use a dummy client.
    ;; This will prevent the frame from being deleted when
    ;; emacsclient quits while also preventing
    ;; `server-save-buffers-kill-terminal' from unexpectedly
    ;; killing emacs on that frame.
    (let* ((params `((client . ,(if nowait 'nowait proc))
                     ;; This is a leftover, see above.
                     (environment . ,(process-get proc 'env))
                     ,@parameters))
	   (display (or display
			(frame-parameter nil 'display)
			(getenv "DISPLAY")
			(error "Please specify display")))
	   frame)
      (if parent-id
	  (push (cons 'parent-id (string-to-number parent-id)) params))
      (setq frame (make-frame-on-display display params))
      (server-log (format "%s created" frame) proc)
      (select-frame frame)
      (process-put proc 'frame frame)
      (process-put proc 'terminal (frame-terminal frame))

      ;; Display *scratch* by default.
      (switch-to-buffer (get-buffer-create "*scratch*") 'norecord)
      frame)))

(defun server-goto-toplevel (proc)
  (condition-case nil
      ;; If we're running isearch, we must abort it to allow Emacs to
      ;; display the buffer and switch to it.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p isearch-mode)
            (isearch-cancel))))
    ;; Signaled by isearch-cancel.
    (quit (message nil)))
  (when (> (recursion-depth) 0)
    ;; We're inside a minibuffer already, so if the emacs-client is trying
    ;; to open a frame on a new display, we might end up with an unusable
    ;; frame because input from that display will be blocked (until exiting
    ;; the minibuffer).  Better exit this minibuffer right away.
    ;; Similarly with recursive-edits such as the splash screen.
    (run-with-timer 0 nil (lambda () (server-execute-continuation proc)))
    (top-level)))

;; We use various special properties on process objects:
;; - `env' stores the info about the environment of the emacsclient process.
;; - `continuation' is a no-arg function that we need to execute.  It contains
;;   commands we wanted to execute in some earlier invocation of the process
;;   filter but that we somehow were unable to process at that time
;;   (e.g. because we first need to throw to the toplevel).

(defun server-execute-continuation (proc)
  (let ((continuation (process-get proc 'continuation)))
    (process-put proc 'continuation nil)
    (if continuation (ignore-errors (funcall continuation)))))

(defun* server-process-filter (proc string)
  "Process a request from the server to edit some files.
PROC is the server process.  STRING consists of a sequence of
commands prefixed by a dash.  Some commands have arguments;
these are &-quoted and need to be decoded by `server-unquote-arg'.
The filter parses and executes these commands.

To illustrate the protocol, here is an example command that
emacsclient sends to create a new X frame (note that the whole
sequence is sent on a single line):

	-env HOME=/home/lorentey
	-env DISPLAY=:0.0
	... lots of other -env commands
	-display :0.0
	-window-system

The following commands are accepted by the server:

`-auth AUTH-STRING'
  Authenticate the client using the secret authentication string
  AUTH-STRING.

`-env NAME=VALUE'
  An environment variable on the client side.

`-dir DIRNAME'
  The current working directory of the client process.

`-current-frame'
  Forbid the creation of new frames.

`-frame-parameters ALIST'
  Set the parameters of the created frame.

`-nowait'
  Request that the next frame created should not be
  associated with this client.

`-display DISPLAY'
  Set the display name to open X frames on.

`-position LINE[:COLUMN]'
  Go to the given line and column number
  in the next file opened.

`-file FILENAME'
  Load the given file in the current frame.

`-eval EXPR'
  Evaluate EXPR as a Lisp expression and return the
  result in -print commands.

`-window-system'
  Open a new X frame.

`-tty DEVICENAME TYPE'
  Open a new tty frame at the client.

`-suspend'
  Suspend this tty frame.  The client sends this string in
  response to SIGTSTP and SIGTTOU.  The server must cease all I/O
  on this tty until it gets a -resume command.

`-resume'
  Resume this tty frame.  The client sends this string when it
  gets the SIGCONT signal and it is the foreground process on its
  controlling tty.

`-ignore COMMENT'
  Do nothing, but put the comment in the server log.
  Useful for debugging.


The following commands are accepted by the client:

`-emacs-pid PID'
  Describes the process id of the Emacs process;
  used to forward window change signals to it.

`-window-system-unsupported'
  Signals that the server does not support creating X frames;
  the client must try again with a tty frame.

`-print STRING'
  Print STRING on stdout.  Used to send values
  returned by -eval.

`-print-nonl STRING'
  Print STRING on stdout.  Used to continue a
  preceding -print command that would be too big to send
  in a single message.

`-error DESCRIPTION'
  Signal an error and delete process PROC.

`-suspend'
  Suspend this terminal, i.e., stop the client process.
  Sent when the user presses C-z."
  (server-log (concat "Received " string) proc)
  ;; First things first: let's check the authentication
  (unless (process-get proc :authenticated)
    (if (and (string-match "-auth \\([!-~]+\\)\n?" string)
	     (equal (match-string 1 string) (process-get proc :auth-key)))
	(progn
	  (setq string (substring string (match-end 0)))
	  (process-put proc :authenticated t)
	  (server-log "Authentication successful" proc))
      (server-log "Authentication failed" proc)
      (server-send-string
       proc (concat "-error " (server-quote-arg "Authentication failed")))
      ;; Before calling `delete-process', give emacsclient time to
      ;; receive the error string and shut down on its own.
      (sit-for 1)
      (delete-process proc)
      ;; We return immediately
      (return-from server-process-filter)))
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (condition-case err
      (progn
	(server-add-client proc)
	;; Send our pid
	(server-send-string proc (concat "-emacs-pid "
					 (number-to-string (emacs-pid)) "\n"))
	(if (not (string-match "\n" string))
            ;; Save for later any partial line that remains.
            (when (> (length string) 0)
              (process-put proc 'previous-string string))

          ;; In earlier versions of server.el (where we used an `emacsserver'
          ;; process), there could be multiple lines.  Nowadays this is not
          ;; supported any more.
          (assert (eq (match-end 0) (length string)))
	  (let ((request (substring string 0 (match-beginning 0)))
		(coding-system (and (default-value 'enable-multibyte-characters)
				    (or file-name-coding-system
					default-file-name-coding-system)))
		nowait     ; t if emacsclient does not want to wait for us.
		frame      ; Frame opened for the client (if any).
		display    ; Open frame on this display.
		parent-id  ; Window ID for XEmbed
		dontkill   ; t if client should not be killed.
		commands
		dir
		use-current-frame
		frame-parameters  ;parameters for newly created frame
		tty-name   ; nil, `window-system', or the tty name.
		tty-type   ; string.
		files
		filepos
		args-left)
	    ;; Remove this line from STRING.
	    (setq string (substring string (match-end 0)))
	    (setq args-left
		  (mapcar 'server-unquote-arg (split-string request " " t)))
	    (while args-left
              (pcase (pop args-left)
                ;; -version CLIENT-VERSION: obsolete at birth.
                (`"-version" (pop args-left))

                ;; -nowait:  Emacsclient won't wait for a result.
                (`"-nowait" (setq nowait t))

                ;; -current-frame:  Don't create frames.
                (`"-current-frame" (setq use-current-frame t))

                ;; -frame-parameters: Set frame parameters
                (`"-frame-parameters"
                 (let ((alist (pop args-left)))
                   (if coding-system
                       (setq alist (decode-coding-string alist coding-system)))
                   (setq frame-parameters (car (read-from-string alist)))))

                ;; -display DISPLAY:
                ;; Open X frames on the given display instead of the default.
                (`"-display"
                 (setq display (pop args-left))
                 (if (zerop (length display)) (setq display nil)))

                ;; -parent-id ID:
                ;; Open X frame within window ID, via XEmbed.
                (`"-parent-id"
                 (setq parent-id (pop args-left))
                 (if (zerop (length parent-id)) (setq parent-id nil)))

                ;; -window-system:  Open a new X frame.
                (`"-window-system"
                 (setq dontkill t)
                 (setq tty-name 'window-system))

                ;; -resume:  Resume a suspended tty frame.
                (`"-resume"
                 (let ((terminal (process-get proc 'terminal)))
                   (setq dontkill t)
                   (push (lambda ()
                           (when (eq (terminal-live-p terminal) t)
                             (resume-tty terminal)))
                         commands)))

                ;; -suspend:  Suspend the client's frame.  (In case we
                ;; get out of sync, and a C-z sends a SIGTSTP to
                ;; emacsclient.)
                (`"-suspend"
                 (let ((terminal (process-get proc 'terminal)))
                   (setq dontkill t)
                   (push (lambda ()
                           (when (eq (terminal-live-p terminal) t)
                             (suspend-tty terminal)))
                         commands)))

                ;; -ignore COMMENT:  Noop; useful for debugging emacsclient.
                ;; (The given comment appears in the server log.)
                (`"-ignore"
                 (setq dontkill t)
                 (pop args-left))

                ;; -tty DEVICE-NAME TYPE:  Open a new tty frame at the client.
                (`"-tty"
                 (setq tty-name (pop args-left)
                       tty-type (pop args-left)
                       dontkill (or dontkill
                                    (not use-current-frame)))
                 ;; On Windows, emacsclient always asks for a tty frame.
                 ;; If running a GUI server, force the frame type to GUI.
                 (when (eq window-system 'w32)
                   (push "-window-system" args-left)))

                ;; -position LINE[:COLUMN]:  Set point to the given
                ;;  position in the next file.
                (`"-position"
                 (if (not (string-match "\\+\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?"
                                        (car args-left)))
                     (error "Invalid -position command in client args"))
                 (let ((arg (pop args-left)))
                   (setq filepos
                         (cons (string-to-number (match-string 1 arg))
                               (string-to-number (or (match-string 2 arg)
                                                     ""))))))

                ;; -file FILENAME:  Load the given file.
                (`"-file"
                 (let ((file (pop args-left)))
                   (if coding-system
                       (setq file (decode-coding-string file coding-system)))
                   (setq file (expand-file-name file dir))
                   (push (cons file filepos) files)
                   (server-log (format "New file: %s %s"
                                       file (or filepos "")) proc))
                 (setq filepos nil))

                ;; -eval EXPR:  Evaluate a Lisp expression.
                (`"-eval"
                 (if use-current-frame
                     (setq use-current-frame 'always))
                 (let ((expr (pop args-left)))
                   (if coding-system
                       (setq expr (decode-coding-string expr coding-system)))
                   (push (lambda () (server-eval-and-print expr proc))
                         commands)
                   (setq filepos nil)))

                ;; -env NAME=VALUE:  An environment variable.
                (`"-env"
                 (let ((var (pop args-left)))
                   ;; XXX Variables should be encoded as in getenv/setenv.
                   (process-put proc 'env
                                (cons var (process-get proc 'env)))))

                ;; -dir DIRNAME:  The cwd of the emacsclient process.
                (`"-dir"
                 (setq dir (pop args-left))
                 (if coding-system
                     (setq dir (decode-coding-string dir coding-system)))
                 (setq dir (command-line-normalize-file-name dir)))

                ;; Unknown command.
                (arg (error "Unknown command: %s" arg))))

	    ;; If both -no-wait and -tty are given with file or sexp
	    ;; arguments, use an existing frame.
	    (and nowait
		 (not (eq tty-name 'window-system))
		 (or files commands)
		 (setq use-current-frame t))

	    (setq frame
		  (cond
		   ((and use-current-frame
			 (or (eq use-current-frame 'always)
			     ;; We can't use the Emacs daemon's
			     ;; terminal frame.
			     (not (and (daemonp)
				       (null (cdr (frame-list)))
				       (eq (selected-frame)
					   terminal-frame)))))
		    (setq tty-name nil tty-type nil)
		    (if display (server-select-display display)))
		   ((eq tty-name 'window-system)
		    (server-create-window-system-frame display nowait proc
						       parent-id
						       frame-parameters))
		   ;; When resuming on a tty, tty-name is nil.
		   (tty-name
		    (server-create-tty-frame tty-name tty-type proc))))

            (process-put
             proc 'continuation
             (lambda ()
               (with-current-buffer (get-buffer-create server-buffer)
                 ;; Use the same cwd as the emacsclient, if possible, so
                 ;; relative file names work correctly, even in `eval'.
                 (let ((default-directory
                         (if (and dir (file-directory-p dir))
                             dir default-directory)))
                   (server-execute proc files nowait commands
                                   dontkill frame tty-name)))))

            (when (or frame files)
              (server-goto-toplevel proc))

            (server-execute-continuation proc))))
    ;; condition-case
    (error (server-return-error proc err))))

(defun server-execute (proc files nowait commands dontkill frame tty-name)
  ;; This is run from timers and process-filters, i.e. "asynchronously".
  ;; But w.r.t the user, this is not really asynchronous since the timer
  ;; is run after 0s and the process-filter is run in response to the
  ;; user running `emacsclient'.  So it is OK to override the
  ;; inhibit-quit flag, which is good since `commands' (as well as
  ;; find-file-noselect via the major-mode) can run arbitrary code,
  ;; including code that needs to wait.
  (with-local-quit
    (condition-case err
        (let* ((buffers
                (when files
                  (server-visit-files files proc nowait))))

          (mapc 'funcall (nreverse commands))

          ;; Delete the client if necessary.
          (cond
           (nowait
            ;; Client requested nowait; return immediately.
            (server-log "Close nowait client" proc)
            (server-delete-client proc))
           ((and (not dontkill) (null buffers))
            ;; This client is empty; get rid of it immediately.
            (server-log "Close empty client" proc)
            (server-delete-client proc)))
          (cond
           ((or isearch-mode (minibufferp))
            nil)
           ((and frame (null buffers))
            (message "%s" (substitute-command-keys
                           "When done with this frame, type \\[delete-frame]")))
           ((not (null buffers))
            (server-switch-buffer (car buffers) nil (cdr (car files)))
            (run-hooks 'server-switch-hook)
            (unless nowait
              (message "%s" (substitute-command-keys
                             "When done with a buffer, type \\[server-edit]")))))
          (when (and frame (null tty-name))
            (server-unselect-display frame)))
      ((quit error)
       (when (eq (car err) 'quit)
         (message "Quit emacsclient request"))
       (server-return-error proc err)))))

(defun server-return-error (proc err)
  (ignore-errors
    (server-send-string
     proc (concat "-error " (server-quote-arg
                             (error-message-string err))))
    (server-log (error-message-string err) proc)
    ;; Before calling `delete-process', give emacsclient time to
    ;; receive the error string and shut down on its own.
    (sit-for 5)
    (delete-process proc)))

(defun server-goto-line-column (line-col)
  "Move point to the position indicated in LINE-COL.
LINE-COL should be a pair (LINE . COL)."
  (when line-col
    (goto-char (point-min))
    (forward-line (1- (car line-col)))
    (let ((column-number (cdr line-col)))
      (when (> column-number 0)
        (move-to-column (1- column-number))))))

(defun server-visit-files (files proc &optional nowait)
  "Find FILES and return a list of buffers created.
FILES is an alist whose elements are (FILENAME . FILEPOS)
where FILEPOS can be nil or a pair (LINENUMBER . COLUMNNUMBER).
PROC is the client that requested this operation.
NOWAIT non-nil means this client is not waiting for the results,
so don't mark these buffers specially, just visit them normally."
  ;; Bind last-nonmenu-event to force use of keyboard, not mouse, for queries.
  (let ((last-nonmenu-event t) client-record)
    ;; Restore the current buffer afterward, but not using save-excursion,
    ;; because we don't want to save point in this buffer
    ;; if it happens to be one of those specified by the server.
    (save-current-buffer
      (dolist (file files)
	;; If there is an existing buffer modified or the file is
	;; modified, revert it.  If there is an existing buffer with
	;; deleted file, offer to write it.
	(let* ((minibuffer-auto-raise (or server-raise-frame
					  minibuffer-auto-raise))
	       (filen (car file))
	       (obuf (get-file-buffer filen)))
	  (add-to-history 'file-name-history filen)
	  (if (null obuf)
	      (progn
		(run-hooks 'pre-command-hook)
		(set-buffer (find-file-noselect filen)))
            (set-buffer obuf)
	    ;; separately for each file, in sync with post-command hooks,
	    ;; with the new buffer current:
	    (run-hooks 'pre-command-hook)
            (cond ((file-exists-p filen)
                   (when (not (verify-visited-file-modtime obuf))
                     (revert-buffer t nil)))
                  (t
                   (when (y-or-n-p
                          (concat "File no longer exists: " filen
                                  ", write buffer to file? "))
                     (write-file filen))))
            (unless server-buffer-clients
              (setq server-existing-buffer t)))
          (server-goto-line-column (cdr file))
          (run-hooks 'server-visit-hook)
	  ;; hooks may be specific to current buffer:
	  (run-hooks 'post-command-hook))
	(unless nowait
	  ;; When the buffer is killed, inform the clients.
	  (add-hook 'kill-buffer-hook 'server-kill-buffer nil t)
	  (push proc server-buffer-clients))
	(push (current-buffer) client-record)))
    (unless nowait
      (process-put proc 'buffers
                   (nconc (process-get proc 'buffers) client-record)))
    client-record))

(defvar server-kill-buffer-running nil
  "Non-nil while `server-kill-buffer' or `server-buffer-done' is running.")

(defun server-buffer-done (buffer &optional for-killing)
  "Mark BUFFER as \"done\" for its client(s).
This buries the buffer, then returns a list of the form (NEXT-BUFFER KILLED).
NEXT-BUFFER is another server buffer, as a suggestion for what to select next,
or nil.  KILLED is t if we killed BUFFER (typically, because it was visiting
a temp file).
FOR-KILLING if non-nil indicates that we are called from `kill-buffer'."
  (let ((next-buffer nil)
	(killed nil))
    (dolist (proc server-clients)
      (let ((buffers (process-get proc 'buffers)))
	(or next-buffer
	    (setq next-buffer (nth 1 (memq buffer buffers))))
	(when buffers			; Ignore bufferless clients.
	  (setq buffers (delq buffer buffers))
	  ;; Delete all dead buffers from PROC.
	  (dolist (b buffers)
	    (and (bufferp b)
		 (not (buffer-live-p b))
		 (setq buffers (delq b buffers))))
	  (process-put proc 'buffers buffers)
	  ;; If client now has no pending buffers,
	  ;; tell it that it is done, and forget it entirely.
	  (unless buffers
	    (server-log "Close" proc)
	    (if for-killing
		;; `server-delete-client' might delete the client's
		;; frames, which might change the current buffer.  We
		;; don't want that (bug#640).
		(save-current-buffer
		  (server-delete-client proc))
	      (server-delete-client proc))))))
    (when (and (bufferp buffer) (buffer-name buffer))
      ;; We may or may not kill this buffer;
      ;; if we do, do not call server-buffer-done recursively
      ;; from kill-buffer-hook.
      (let ((server-kill-buffer-running t))
	(with-current-buffer buffer
	  (setq server-buffer-clients nil)
	  (run-hooks 'server-done-hook))
	;; Notice whether server-done-hook killed the buffer.
	(if (null (buffer-name buffer))
	    (setq killed t)
	  ;; Don't bother killing or burying the buffer
	  ;; when we are called from kill-buffer.
	  (unless for-killing
	    (when (and (not killed)
		       server-kill-new-buffers
		       (with-current-buffer buffer
			 (not server-existing-buffer)))
	      (setq killed t)
	      (bury-buffer buffer)
	      ;; Prevent kill-buffer from prompting (Bug#3696).
	      (with-current-buffer buffer
		(set-buffer-modified-p nil))
	      (kill-buffer buffer))
	    (unless killed
	      (if (server-temp-file-p buffer)
		  (progn
		    (with-current-buffer buffer
		      (set-buffer-modified-p nil))
		    (kill-buffer buffer)
		    (setq killed t))
		(bury-buffer buffer)))))))
    (list next-buffer killed)))

(defun server-temp-file-p (&optional buffer)
  "Return non-nil if BUFFER contains a file considered temporary.
These are files whose names suggest they are repeatedly
reused to pass information to another program.

The variable `server-temp-file-regexp' controls which filenames
are considered temporary."
  (and (buffer-file-name buffer)
       (string-match-p server-temp-file-regexp (buffer-file-name buffer))))

(defun server-done ()
  "Offer to save current buffer, mark it as \"done\" for clients.
This kills or buries the buffer, then returns a list
of the form (NEXT-BUFFER KILLED).  NEXT-BUFFER is another server buffer,
as a suggestion for what to select next, or nil.
KILLED is t if we killed BUFFER, which happens if it was created
specifically for the clients and did not exist before their request for it."
  (when server-buffer-clients
    (if (server-temp-file-p)
	;; For a temp file, save, and do make a non-numeric backup
	;; (unless make-backup-files is nil).
	(let ((version-control nil)
	      (buffer-backed-up nil))
	  (save-buffer))
      (when (and (buffer-modified-p)
		 buffer-file-name
		 (y-or-n-p (concat "Save file " buffer-file-name "? ")))
	(save-buffer)))
    (server-buffer-done (current-buffer))))

;; Ask before killing a server buffer.
;; It was suggested to release its client instead,
;; but I think that is dangerous--the client would proceed
;; using whatever is on disk in that file. -- rms.
(defun server-kill-buffer-query-function ()
  "Ask before killing a server buffer."
  (or (not server-buffer-clients)
      (let ((res t))
	(dolist (proc server-buffer-clients)
          (when (and (memq proc server-clients)
                     (eq (process-status proc) 'open))
            (setq res nil)))
         res)
      (yes-or-no-p (format "Buffer `%s' still has clients; kill it? "
			   (buffer-name (current-buffer))))))

(defun server-kill-emacs-query-function ()
  "Ask before exiting Emacs if it has live clients."
  (or (not server-clients)
      (let (live-client)
	(dolist (proc server-clients)
	  (when (memq t (mapcar 'buffer-live-p (process-get
						proc 'buffers)))
	    (setq live-client t)))
        live-client)
      (yes-or-no-p "This Emacs session has clients; exit anyway? ")))

(defun server-kill-buffer ()
  "Remove the current buffer from its clients' buffer list.
Designed to be added to `kill-buffer-hook'."
  ;; Prevent infinite recursion if user has made server-done-hook
  ;; call kill-buffer.
  (or server-kill-buffer-running
      (and server-buffer-clients
	   (let ((server-kill-buffer-running t))
	     (when server-process
	       (server-buffer-done (current-buffer) t))))))

(defun server-edit (&optional arg)
  "Switch to next server editing buffer; say \"Done\" for current buffer.
If a server buffer is current, it is marked \"done\" and optionally saved.
The buffer is also killed if it did not exist before the clients asked for it.
When all of a client's buffers are marked as \"done\", the client is notified.

Temporary files such as MH <draft> files are always saved and backed up,
no questions asked.  (The variable `make-backup-files', if nil, still
inhibits a backup; you can set it locally in a particular buffer to
prevent a backup for it.)  The variable `server-temp-file-regexp' controls
which filenames are considered temporary.

If invoked with a prefix argument, or if there is no server process running,
starts server process and that is all.  Invoked by \\[server-edit]."
  (interactive "P")
  (cond
   ((or arg
	(not server-process)
	(memq (process-status server-process) '(signal exit)))
    (server-mode 1))
   (server-clients (apply 'server-switch-buffer (server-done)))
   (t (message "No server editing buffers exist"))))

(defun server-switch-buffer (&optional next-buffer killed-one filepos)
  "Switch to another buffer, preferably one that has a client.
Arg NEXT-BUFFER is a suggestion; if it is a live buffer, use it.

KILLED-ONE is t in a recursive call if we have already killed one
temp-file server buffer.  This means we should avoid the final
\"switch to some other buffer\" since we've already effectively
done that.

FILEPOS specifies a new buffer position for NEXT-BUFFER, if we
visit NEXT-BUFFER in an existing window.  If non-nil, it should
be a cons cell (LINENUMBER . COLUMNNUMBER)."
  (if (null next-buffer)
      (progn
	(let ((rest server-clients))
	  (while (and rest (not next-buffer))
	    (let ((proc (car rest)))
	      ;; Only look at frameless clients, or those in the selected
	      ;; frame.
	      (when (or (not (process-get proc 'frame))
			(eq (process-get proc 'frame) (selected-frame)))
		(setq next-buffer (car (process-get proc 'buffers))))
	      (setq rest (cdr rest)))))
	(and next-buffer (server-switch-buffer next-buffer killed-one))
	(unless (or next-buffer killed-one (window-dedicated-p (selected-window)))
	  ;; (switch-to-buffer (other-buffer))
	  (message "No server buffers remain to edit")))
    (if (not (buffer-live-p next-buffer))
	;; If NEXT-BUFFER is a dead buffer, remove the server records for it
	;; and try the next surviving server buffer.
	(apply 'server-switch-buffer (server-buffer-done next-buffer))
      ;; OK, we know next-buffer is live, let's display and select it.
      (if (functionp server-window)
	  (funcall server-window next-buffer)
	(let ((win (get-buffer-window next-buffer 0)))
	  (if (and win (not server-window))
	      ;; The buffer is already displayed: just reuse the
	      ;; window.  If FILEPOS is non-nil, use it to replace the
	      ;; window's own value of point.
              (progn
                (select-window win)
                (set-buffer next-buffer)
		(when filepos
		  (server-goto-line-column filepos)))
	    ;; Otherwise, let's find an appropriate window.
	    (cond ((window-live-p server-window)
		   (select-window server-window))
		  ((framep server-window)
		   (unless (frame-live-p server-window)
		     (setq server-window (make-frame)))
		   (select-window (frame-selected-window server-window))))
	    (when (window-minibuffer-p (selected-window))
	      (select-window (next-window nil 'nomini 0)))
	    ;; Move to a non-dedicated window, if we have one.
	    (when (window-dedicated-p (selected-window))
	      (select-window
	       (get-window-with-predicate
		(lambda (w)
		  (and (not (window-dedicated-p w))
		       (equal (frame-terminal (window-frame w))
			      (frame-terminal (selected-frame)))))
		'nomini 'visible (selected-window))))
	    (condition-case nil
		(switch-to-buffer next-buffer)
	      ;; After all the above, we might still have ended up with
	      ;; a minibuffer/dedicated-window (if there's no other).
	      (error (pop-to-buffer next-buffer)))))))
    (when server-raise-frame
      (select-frame-set-input-focus (window-frame (selected-window))))))

;;;###autoload
(defun server-save-buffers-kill-terminal (arg)
  ;; Called from save-buffers-kill-terminal in files.el.
  "Offer to save each buffer, then kill the current client.
With ARG non-nil, silently save all file-visiting buffers, then kill.

If emacsclient was started with a list of filenames to edit, then
only these files will be asked to be saved."
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (cond ((eq proc 'nowait)
	   ;; Nowait frames have no client buffer list.
	   (if (cdr (frame-list))
	       (progn (save-some-buffers arg)
		      (delete-frame))
	     ;; If we're the last frame standing, kill Emacs.
	     (save-buffers-kill-emacs arg)))
	  ((processp proc)
	   (let ((buffers (process-get proc 'buffers)))
	     ;; If client is bufferless, emulate a normal Emacs exit
	     ;; and offer to save all buffers.  Otherwise, offer to
	     ;; save only the buffers belonging to the client.
	     (save-some-buffers
	      arg (if buffers
		      (lambda () (memq (current-buffer) buffers))
		    t))
	     (server-delete-client proc)))
	  (t (error "Invalid client frame")))))

(define-key ctl-x-map "#" 'server-edit)

(defun server-unload-function ()
  "Unload the server library."
  (server-mode -1)
  (substitute-key-definition 'server-edit nil ctl-x-map)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (remove-hook 'kill-buffer-hook 'server-kill-buffer t)))
  ;; continue standard unloading
  nil)

(defun server-eval-at (server form)
  "Contact the Emacs server named SERVER and evaluate FORM there.
Returns the result of the evaluation, or signals an error if it
cannot contact the specified server.  For example:
  \(server-eval-at \"server\" '(emacs-pid))
returns the process ID of the Emacs instance running \"server\"."
  (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
	 (server-file (expand-file-name server server-dir))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 address port secret process)
    (unless (file-exists-p server-file)
      (error "No such server: %s" server))
    (with-temp-buffer
      (when server-use-tcp
	(let ((coding-system-for-read 'no-conversion))
	  (insert-file-contents server-file)
	  (unless (looking-at "\\([0-9.]+\\):\\([0-9]+\\)")
	    (error "Invalid auth file"))
	  (setq address (match-string 1)
		port (string-to-number (match-string 2)))
	  (forward-line 1)
	  (setq secret (buffer-substring (point) (line-end-position)))
	  (erase-buffer)))
      (unless (setq process (make-network-process
			     :name "eval-at"
			     :buffer (current-buffer)
			     :host address
			     :service (if server-use-tcp port server-file)
			     :family (if server-use-tcp 'ipv4 'local)
			     :noquery t))
	       (error "Unable to contact the server"))
      (if server-use-tcp
	  (process-send-string process (concat "-auth " secret "\n")))
      (process-send-string process
			   (concat "-eval "
				   (server-quote-arg (format "%S" form))
				   "\n"))
      (while (memq (process-status process) '(open run))
	(accept-process-output process 0 10))
      (goto-char (point-min))
      ;; If the result is nil, there's nothing in the buffer.  If the
      ;; result is non-nil, it's after "-print ".
      (let ((answer ""))
	(while (re-search-forward "\n-print\\(-nonl\\)? " nil t)
	  (setq answer
		(concat answer
			(buffer-substring (point)
					  (progn (skip-chars-forward "^\n")
						 (point))))))
	(if (not (equal answer ""))
	    (read (decode-coding-string (server-unquote-arg answer)
					'emacs-internal)))))))


(provide 'server)

;;; server.el ends here
