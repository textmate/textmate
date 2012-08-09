;;; diff.el --- run `diff'

;; Copyright (C) 1992, 1994, 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Frank Bresz
;; (according to authors.el)
;; Maintainer: FSF
;; Keywords: unix, vc, tools

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

;; This package helps you explore differences between files, using the
;; UNIX command diff(1).  The commands are `diff' and `diff-backup'.
;; You can specify options with `diff-switches'.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup diff nil
  "Comparing files with `diff'."
  :group 'tools)

;;;###autoload
(defcustom diff-switches (purecopy "-c")
  "A string or list of strings specifying switches to be passed to diff."
  :type '(choice string (repeat string))
  :group 'diff)

;;;###autoload
(defcustom diff-command (purecopy "diff")
  "The command to use to run diff."
  :type 'string
  :group 'diff)

;; prompt if prefix arg present
(defun diff-switches ()
  (if current-prefix-arg
      (read-string "Diff switches: "
		   (if (stringp diff-switches)
		       diff-switches
		     (mapconcat 'identity diff-switches " ")))))

(defun diff-sentinel (code &optional old-temp-file new-temp-file)
  "Code run when the diff process exits.
CODE is the exit code of the process.  It should be 0 only if no diffs
were found.
If optional args OLD-TEMP-FILE and/or NEW-TEMP-FILE are non-nil,
delete the temporary files so named."
  (if old-temp-file (delete-file old-temp-file))
  (if new-temp-file (delete-file new-temp-file))
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format "\nDiff finished%s.  %s\n"
		      (cond ((equal 0 code) " (no differences)")
			    ((equal 2 code) " (diff error)")
			    (t ""))
		      (current-time-string))))))

;;;###autoload
(defun diff (old new &optional switches no-async)
  "Find and display the differences between OLD and NEW files.
When called interactively, read NEW, then OLD, using the
minibuffer.  The default for NEW is the current buffer's file
name, and the default for OLD is a backup file for NEW, if one
exists.  If NO-ASYNC is non-nil, call diff synchronously.

When called interactively with a prefix argument, prompt
interactively for diff switches.  Otherwise, the switches
specified in `diff-switches' are passed to the diff command."
  (interactive
   (let* ((newf (if (and buffer-file-name (file-exists-p buffer-file-name))
		    (read-file-name
		     (concat "Diff new file (default "
			     (file-name-nondirectory buffer-file-name) "): ")
		     nil buffer-file-name t)
		  (read-file-name "Diff new file: " nil nil t)))
          (oldf (file-newest-backup newf)))
     (setq oldf (if (and oldf (file-exists-p oldf))
		    (read-file-name
		     (concat "Diff original file (default "
			     (file-name-nondirectory oldf) "): ")
		     (file-name-directory oldf) oldf t)
		  (read-file-name "Diff original file: "
				  (file-name-directory newf) nil t)))
     (list oldf newf (diff-switches))))
  (display-buffer
   (diff-no-select old new switches no-async)))

(defun diff-file-local-copy (file-or-buf)
  (if (bufferp file-or-buf)
      (with-current-buffer file-or-buf
        (let ((tempfile (make-temp-file "buffer-content-")))
          (write-region nil nil tempfile nil 'nomessage)
          tempfile))
    (file-local-copy file-or-buf)))

(defun diff-no-select (old new &optional switches no-async buf)
  ;; Noninteractive helper for creating and reverting diff buffers
  (unless (bufferp new) (setq new (expand-file-name new)))
  (unless (bufferp old) (setq old (expand-file-name old)))
  (or switches (setq switches diff-switches)) ; If not specified, use default.
  (unless (listp switches) (setq switches (list switches)))
  (or buf (setq buf (get-buffer-create "*Diff*")))
  (let* ((old-alt (diff-file-local-copy old))
	 (new-alt (diff-file-local-copy new))
	 (command
	  (mapconcat 'identity
		     `(,diff-command
		       ;; Use explicitly specified switches
		       ,@switches
                       ,@(mapcar #'shell-quote-argument
                                 (nconc
                                  (when (or old-alt new-alt)
                                    (list "-L" (if (stringp old)
                                                   old (prin1-to-string old))
                                          "-L" (if (stringp new)
                                                   new (prin1-to-string new))))
                                  (list (or old-alt old)
                                        (or new-alt new)))))
		     " "))
	 (thisdir default-directory))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (buffer-disable-undo (current-buffer))
      (let ((inhibit-read-only t))
	(erase-buffer))
      (buffer-enable-undo (current-buffer))
      (diff-mode)
      (set (make-local-variable 'revert-buffer-function)
           (lexical-let ((old old) (new new)
                         (switches switches)
                         (no-async no-async))
             (lambda (ignore-auto noconfirm)
               (diff-no-select old new switches no-async (current-buffer)))))
      (setq default-directory thisdir)
      (let ((inhibit-read-only t))
	(insert command "\n"))
      (if (and (not no-async) (fboundp 'start-process))
	  (let ((proc (start-process "Diff" buf shell-file-name
                                     shell-command-switch command)))
	    (set-process-filter proc 'diff-process-filter)
            (lexical-let ((old-alt old-alt) (new-alt new-alt))
              (set-process-sentinel
               proc (lambda (proc msg)
                      (with-current-buffer (process-buffer proc)
                        (diff-sentinel (process-exit-status proc)
                                       old-alt new-alt))))))
	;; Async processes aren't available.
	(let ((inhibit-read-only t))
	  (diff-sentinel
	   (call-process shell-file-name nil buf nil
			 shell-command-switch command)
           old-alt new-alt))))
    buf))

(defun diff-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(let ((inhibit-read-only t))
	  (insert string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

;;;###autoload
(defun diff-backup (file &optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for diff switches."
  (interactive (list (read-file-name "Diff (file with backup): ")
		     (diff-switches)))
  (let (bak ori)
    (if (backup-file-name-p file)
	(setq bak file
	      ori (file-name-sans-versions file))
      (setq bak (or (diff-latest-backup-file file)
		    (error "No backup found for %s" file))
	    ori file))
    (diff bak ori switches)))

(defun diff-latest-backup-file (fn)	; actually belongs into files.el
  "Return the latest existing backup of FILE, or nil."
  (let ((handler (find-file-name-handler fn 'diff-latest-backup-file)))
    (if handler
	(funcall handler 'diff-latest-backup-file fn)
      (file-newest-backup fn))))

;;;###autoload
(defun diff-buffer-with-file (&optional buffer)
  "View the differences between BUFFER and its associated file.
This requires the external program `diff' to be in your `exec-path'."
  (interactive "bBuffer: ")
  (with-current-buffer (get-buffer (or buffer (current-buffer)))
    (diff buffer-file-name (current-buffer) nil 'noasync)))

(provide 'diff)

;;; diff.el ends here
