;;; paths.el --- define pathnames for use by various Emacs commands -*- no-byte-compile: t -*-

;; Copyright (C) 1986, 1988, 1994, 1999-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
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

;; These are default settings for names of certain files and directories
;; that Emacs needs to refer to from time to time.

;; If these settings are not right, override them with `setq'
;; in site-init.el.  Do not change this file.

;;; Code:

;; Docstrings in this file should, where reasonable, follow the
;; conventions described in make-docfile, so that they get put in the
;; DOC file rather than in memory.

(defun prune-directory-list (dirs &optional keep reject)
  "\
Return a copy of DIRS with all non-existent directories removed.
The optional argument KEEP is a list of directories to retain even if
they don't exist, and REJECT is a list of directories to remove from
DIRS, even if they exist; REJECT takes precedence over KEEP.

Note that membership in REJECT and KEEP is checked using simple string
comparison."
  (apply #'nconc
	 (mapcar (lambda (dir)
		   (and (not (member dir reject))
			(or (member dir keep) (file-directory-p dir))
			(list dir)))
		    dirs)))

(defvar Info-default-directory-list
  (let* ((config-dir
	  (file-name-as-directory configure-info-directory))
	 (config
	  (list config-dir))
	 (unpruned-prefixes
	  ;; Directory trees that may not exist at installation time, and
	  ;; so shouldn't be pruned based on existence.
	  '("/usr/local/"))
	 (prefixes
	  ;; Directory trees in which to look for info subdirectories
	  (prune-directory-list '("/usr/local/" "/usr/" "/opt/" "/")
				unpruned-prefixes))
	 (suffixes
	  ;; Subdirectories in each directory tree that may contain info
	  ;; directories.
	  '("share/" "" "gnu/" "gnu/lib/" "gnu/lib/emacs/"
	    "emacs/" "lib/" "lib/emacs/"))
	 (standard-info-dirs
	  (apply #'nconc
		 (mapcar (lambda (pfx)
			   (let ((dirs
				  (mapcar (lambda (sfx)
					    (concat pfx sfx "info/"))
					  suffixes)))
			     (if (member pfx unpruned-prefixes)
				 dirs
			       (prune-directory-list dirs config))))
			 prefixes))))
    ;; If $(prefix)/share/info is not one of the standard info
    ;; directories, they are probably installing an experimental
    ;; version of Emacs, so make sure that experimental version's Info
    ;; files override the ones in standard directories.
    (if (member config-dir standard-info-dirs)
	(nconc standard-info-dirs config)
      (cons config-dir standard-info-dirs)))
  "Default list of directories to search for Info documentation files.
They are searched in the order they are given in the list.
Therefore, the directory of Info files that come with Emacs
normally should come last (so that local files override standard ones),
unless Emacs is installed into a non-standard directory.  In the latter
case, the directory of Info files that come with Emacs should be
first in this list.

Once Info is started, the list of directories to search
comes from the variable `Info-directory-list'.
This variable `Info-default-directory-list' is used as the default
for initializing `Info-directory-list' when Info is started, unless
the environment variable INFOPATH is set.")

(defvar news-directory
  (purecopy (if (file-exists-p "/usr/spool/news/")
      "/usr/spool/news/"
    "/var/spool/news/"))
  "The root directory below which all news files are stored.")
(defvaralias 'news-path 'news-directory)

(defvar news-inews-program
  (purecopy
  (cond ((file-exists-p "/usr/bin/inews") "/usr/bin/inews")
	((file-exists-p "/usr/local/inews") "/usr/local/inews")
	((file-exists-p "/usr/local/bin/inews") "/usr/local/bin/inews")
	((file-exists-p "/usr/contrib/lib/news/inews") "/usr/contrib/lib/news/inews")
	((file-exists-p "/usr/lib/news/inews") "/usr/lib/news/inews")
	(t "inews")))
  "Program to post news.")

;; set this to your local server
(defvar gnus-default-nntp-server (purecopy "") "\
The name of the host running an NNTP server.
The null string means use the local host as the server site.")

(defvar gnus-nntp-service (purecopy "nntp") "\
NNTP service name, usually \"nntp\" or 119.
Go to a local news spool if its value is nil, in which case `gnus-nntp-server'
should be set to `(system-name)'.")

(defvar gnus-local-organization nil "\
*The name of your organization, as a string.
The `ORGANIZATION' environment variable is used instead if defined.")

(defcustom rmail-file-name (purecopy "~/RMAIL") "\
Name of user's primary mail file."
  :type 'string
  :group 'rmail
  :version "21.1")

(defvar rmail-spool-directory
  (purecopy
  (cond ((file-exists-p "/var/mail")
	 ;; SVR4 and recent BSD are said to use this.
	 ;; Rather than trying to know precisely which systems use it,
	 ;; let's assume this dir is never used for anything else.
	 "/var/mail/")
	;; Many GNU/Linux systems use this name.
	((file-exists-p "/var/spool/mail")
	 "/var/spool/mail/")
	((memq system-type '(hpux usg-unix-v irix))
	 "/usr/mail/")
	(t "/usr/spool/mail/")))
  "Name of directory used by system mailer for delivering new mail.
Its name should end with a slash.")

(defcustom remote-shell-program
  (purecopy
  (cond
   ;; Some systems use rsh for the remote shell; others use that name for the
   ;; restricted shell and use remsh for the remote shell.  Let's try to guess
   ;; based on what we actually find out there.  The restricted shell is
   ;; almost certainly in /bin or /usr/bin, so it's probably safe to assume
   ;; that an rsh found elsewhere is the remote shell program.  The converse
   ;; is not true: /usr/bin/rsh could be either one, so check that last.
   ((file-exists-p "/usr/ucb/remsh") "/usr/ucb/remsh")
   ((file-exists-p "/usr/bsd/remsh") "/usr/bsd/remsh")
   ((file-exists-p "/bin/remsh") "/bin/remsh")
   ((file-exists-p "/usr/bin/remsh") "/usr/bin/remsh")
   ((file-exists-p "/usr/local/bin/remsh") "/usr/local/bin/remsh")
   ((file-exists-p "/usr/ucb/rsh") "/usr/ucb/rsh")
   ((file-exists-p "/usr/bsd/rsh") "/usr/bsd/rsh")
   ((file-exists-p "/usr/local/bin/rsh") "/usr/local/bin/rsh")
   ((file-exists-p "/usr/bin/rcmd") "/usr/bin/rcmd")
   ((file-exists-p "/bin/rcmd") "/bin/rcmd")
   ((file-exists-p "/bin/rsh") "/bin/rsh")
   ((file-exists-p "/usr/bin/rsh") "/usr/bin/rsh")
   (t "rsh")))
  "File name for remote-shell program (often rsh or remsh)."
  :group 'environment
  :type 'file)

(defvar term-file-prefix (purecopy "term/") "\
If non-nil, Emacs startup does (load (concat term-file-prefix (getenv \"TERM\")))
You may set this variable to nil in your `.emacs' file if you do not wish
the terminal-initialization file to be loaded.")

;;; paths.el ends here
