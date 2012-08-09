;;; resume.el --- process command line args from within a suspended Emacs job

;; Copyright (C) 1992, 2001-2012 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@bucsf.bu.edu>
;; Adapted-By: ESR
;; Keywords: processes
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

;; The purpose of this library is to handle command line arguments
;; when you resume an existing Emacs job.

;; In order to use it, you must put this code in your .emacs file.

;; (add-hook 'suspend-hook 'resume-suspend-hook)
;; (add-hook 'suspend-resume-hook 'resume-process-args)

;; You can't get the benefit of this library by using the `emacs' command,
;; since that always starts a new Emacs job.  Instead you must use a
;; command called `edit' which knows how to resume an existing Emacs job
;; if you have one, or start a new Emacs job if you don't have one.

;; To define the `edit' command, run the script etc/emacs.csh (if you use CSH),
;; or etc/emacs.bash if you use BASH.  You would normally do this in your
;; login script.

;; Stephan Gildea suggested bug fix (gildea@bbn.com).
;; Ideas from Michael DeCorte and other people.

;;; Code:

(defvar resume-emacs-args-file (expand-file-name "~/.emacs_args")
  "*This file is where arguments are placed for a suspended Emacs job.")

(defvar resume-emacs-args-buffer " *Command Line Args*"
  "Buffer that is used by `resume-process-args'.")

(defun resume-process-args ()
  "Handler for command line args given when Emacs is resumed."
  (let ((start-buffer (current-buffer))
	(args-buffer (get-buffer-create resume-emacs-args-buffer))
	length args
	(command-line-default-directory default-directory))
    (unwind-protect
	(progn
	  (set-buffer args-buffer)
	  (erase-buffer)
	  ;; get the contents of resume-emacs-args-file
	  (condition-case ()
	      (let ((result (insert-file-contents resume-emacs-args-file)))
		(setq length (car (cdr result))))
	    ;; the file doesn't exist, ergo no arguments
	    (file-error
	      (erase-buffer)
	      (setq length 0)))
	  (if (<= length 0)
	      (setq args nil)
	    ;; get the arguments from the buffer
	    (goto-char (point-min))
	    (while (not (eobp))
	      (skip-chars-forward " \t\n")
	      (let ((begin (point)))
		(skip-chars-forward "^ \t\n")
		(setq args (cons (buffer-substring begin (point)) args)))
	      (skip-chars-forward " \t\n"))
	    ;; arguments are now in reverse order
	    (setq args (nreverse args))
	    ;; make sure they're not read again
	    (erase-buffer))
	  (resume-write-buffer-to-file (current-buffer) resume-emacs-args-file)
	  ;; if nothing was in buffer, args will be null
	  (or (null args)
	      (setq command-line-default-directory
		    (file-name-as-directory (car args))
		    args (cdr args)))
	  ;; actually process the arguments
	  (command-line-1 args))
      ;; If the command line args don't result in a find-file, the
      ;; buffer will be left in args-buffer.  So we change back to the
      ;; original buffer.  The reason I don't just use
      ;; (let ((default-directory foo))
      ;;    (command-line-1 args))
      ;; in the context of the original buffer is because let does not
      ;; work properly with buffer-local variables.
      (if (eq (current-buffer) args-buffer)
	  (set-buffer start-buffer)))))

;;;###autoload
(defun resume-suspend-hook ()
  "Clear out the file used for transmitting args when Emacs resumes."
  (with-current-buffer (get-buffer-create resume-emacs-args-buffer)
    (erase-buffer)
    (resume-write-buffer-to-file (current-buffer) resume-emacs-args-file)))

(defun resume-write-buffer-to-file (buffer file)
  "Writes the contents of BUFFER into FILE, if permissions allow."
  (if (not (file-writable-p file))
      (error "No permission to write file %s" file))
  (with-current-buffer buffer
    (clear-visited-file-modtime)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) file nil 'quiet))
    (set-buffer-modified-p nil)))

(provide 'resume)

;;; resume.el ends here
