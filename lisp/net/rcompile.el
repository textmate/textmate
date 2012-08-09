;;; rcompile.el --- run a compilation on a remote machine

;; Copyright (C) 1993-1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Alon Albert <alon@milcse.rtsg.mot.com>
;; Maintainer: FSF
;; Created: 1993 Oct 6
;; Keywords: tools, processes

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

;; This package is for running a remote compilation and using emacs to parse
;; the error messages. It works by rsh'ing the compilation to a remote host
;; and parsing the output. If the file visited at the time remote-compile was
;; called was loaded remotely (ange-ftp), the host and user name are obtained
;; by the calling ange-ftp-ftp-name on the current directory. In this case the
;; next-error command will also ange-ftp the files over. This is achieved
;; automatically because the compilation-parse-errors function uses
;; default-directory to build its file names. If however the file visited was
;; loaded locally, remote-compile prompts for a host and user and assumes the
;; files mounted locally (otherwise, how was the visited file loaded).

;; See the user defined variables section for more info.

;; I was contemplating redefining "compile" to "remote-compile" automatically
;; if the file visited was ange-ftp'ed but decided against it for now. If you
;; feel this is a good idea, let me know and I'll consider it again.

;; Installation:

;; To use rcompile, you also need to give yourself permission to connect to
;; the remote host.  You do this by putting lines like:

;; monopoly alon
;; vme33
;;
;; in a file named .rhosts in the home directory (of the remote machine).
;; Be careful what you put in this file. A line like:
;;
;; +
;;
;; Will allow anyone access to your account without a password. I suggest you
;; read the rhosts(5) manual page before you edit this file (if you are not
;; familiar with it already)

;;; Code:

(provide 'rcompile)
(require 'compile)
;;; The following should not be needed.
;;; (eval-when-compile (require 'ange-ftp))

;;;; user defined variables

(defgroup remote-compile nil
  "Run a compilation on a remote machine."
  :group 'processes
  :group 'tools)


(defcustom remote-compile-host nil
  "Host for remote compilations."
  :type '(choice string (const nil))
  :group 'remote-compile)

(defcustom remote-compile-user nil
  "User for remote compilations.
nil means use the value returned by \\[user-login-name]."
  :type '(choice string (const nil))
  :group 'remote-compile)

(defcustom remote-compile-run-before nil
  "Command to run before compilation.
This can be used for setting up environment variables,
since rsh does not invoke the shell as a login shell and files like .login
\(tcsh\) and .bash_profile \(bash\) are not run.
nil means run no commands."
  :type '(choice string (const nil))
  :group 'remote-compile)

(defcustom remote-compile-prompt-for-host nil
  "Non-nil means prompt for host if not available from filename."
  :type 'boolean
  :group 'remote-compile)

(defcustom remote-compile-prompt-for-user nil
  "Non-nil means prompt for user if not available from filename."
  :type 'boolean
  :group 'remote-compile)

;;;; internal variables

;; History of remote compile hosts and users
(defvar remote-compile-host-history nil)
(defvar remote-compile-user-history nil)


;;;; entry point

;; We use the Tramp internal function`tramp-make-tramp-file-name'.
;; Better would be, if there are functions to provide user, host and
;; localname of a remote filename, independent of Tramp's implementation.
;; The function calls are wrapped by `funcall' in order to pacify the byte
;; compiler.  ange-ftp check removed, because it is handled also by Tramp.
;;;###autoload
(defun remote-compile (host user command)
  "Compile the current buffer's directory on HOST.  Log in as USER.
See \\[compile]."
  (interactive
   (let (host user command prompt l l-host l-user)
     (setq prompt (if (stringp remote-compile-host)
                      (format "Compile on host (default %s): "
                              remote-compile-host)
                    "Compile on host: ")
           host (if (or remote-compile-prompt-for-host
                        (null remote-compile-host))
                    (read-from-minibuffer prompt
                                          "" nil nil
                                          'remote-compile-host-history)
                  remote-compile-host)
           user (if remote-compile-prompt-for-user
                    (read-from-minibuffer (format
                                           "Compile by user (default %s): "
                                           (or remote-compile-user
                                               (user-login-name)))
                                          "" nil nil
                                          'remote-compile-user-history)
                  remote-compile-user))
     (setq command (read-from-minibuffer "Compile command: "
                                         compile-command nil nil
                                         '(compile-history . 1)))
     (list (if (string= host "") remote-compile-host host)
           (if (string= user "") remote-compile-user user)
           command)))
  (setq compile-command command)
  (cond (user
         (setq remote-compile-user user))
        ((null remote-compile-user)
         (setq remote-compile-user (user-login-name))))
  (let* (localname ;; Pacify byte-compiler.
         (compile-command
          (format "%s %s -l %s \"(%scd %s; %s)\""
		  remote-shell-program
                  host
                  remote-compile-user
                  (if remote-compile-run-before
                      (concat remote-compile-run-before "; ")
                    "")
                  ""
                  compile-command)))
    (setq remote-compile-host host)
    (save-some-buffers nil nil)
    (compilation-start compile-command)
    ;; Set comint-file-name-prefix in the compilation buffer so
    ;; compilation-parse-errors will find referenced files by Tramp.
    (with-current-buffer compilation-last-buffer
      (when (fboundp 'tramp-make-tramp-file-name)
	(set (make-local-variable 'comint-file-name-prefix)
	     (tramp-make-tramp-file-name
	      nil ;; method.
	      remote-compile-user
	      remote-compile-host
	      ""))))))

;;; rcompile.el ends here
