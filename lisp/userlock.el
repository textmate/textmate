;;; userlock.el --- handle file access contention between multiple users

;; Copyright (C) 1985-1986, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; This file is autoloaded to handle certain conditions
;; detected by the file-locking code within Emacs.
;; The two entry points are `ask-user-about-lock' and
;; `ask-user-about-supersession-threat'.

;;; Code:

(put 'file-locked 'error-conditions '(file-locked file-error error))
(put 'file-locked 'error-message "File is locked")

;;;###autoload
(defun ask-user-about-lock (file opponent)
  "Ask user what to do when he wants to edit FILE but it is locked by OPPONENT.
This function has a choice of three things to do:
  do (signal 'file-locked (list FILE OPPONENT))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can redefine this function to choose among those three alternatives
in any way you like."
  (discard-input)
  (save-window-excursion
    (let (answer short-opponent short-file)
      (setq short-file
	    (if (> (length file) 22)
		(concat "..." (substring file (- (length file) 22)))
	      file))
      (setq short-opponent
	    (if (> (length opponent) 25)
		(save-match-data
		  (string-match " (pid [0-9]+)" opponent)
		  (concat (substring opponent 0 13) "..."
			  (match-string 0 opponent)))
	      opponent))
      (while (null answer)
	(message "%s locked by %s: (s, q, p, ?)? "
		 short-file short-opponent)
	(let ((tem (let ((inhibit-quit t)
			 (cursor-in-echo-area t))
		     (prog1 (downcase (read-char))
		            (setq quit-flag nil)))))
	  (if (= tem help-char)
	      (ask-user-about-lock-help)
	    (setq answer (assoc tem '((?s . t)
				      (?q . yield)
				      (?\C-g . yield)
				      (?p . nil)
				      (?? . help))))
	    (cond ((null answer)
		   (beep)
		   (message "Please type q, s, or p; or ? for help")
		   (sit-for 3))
		  ((eq (cdr answer) 'help)
		   (ask-user-about-lock-help)
		   (setq answer nil))
		  ((eq (cdr answer) 'yield)
		   (signal 'file-locked (list file opponent)))))))
      (cdr answer))))

(defun ask-user-about-lock-help ()
  (with-output-to-temp-buffer "*Help*"
    (princ "It has been detected that you want to modify a file that someone else has
already started modifying in Emacs.

You can <s>teal the file; the other user becomes the
  intruder if (s)he ever unmodifies the file and then changes it again.
You can <p>roceed; you edit at your own (and the other user's) risk.
You can <q>uit; don't modify this file.")
    (with-current-buffer standard-output
      (help-mode))))

(put
 'file-supersession 'error-conditions '(file-supersession file-error error))

;;;###autoload
(defun ask-user-about-supersession-threat (fn)
  "Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called."
  (discard-input)
  (save-window-excursion
    (let (answer)
      (while (null answer)
	(message "%s changed on disk; really edit the buffer? (y, n, r or C-h) "
		 (file-name-nondirectory fn))
	(let ((tem (downcase (let ((cursor-in-echo-area t))
			       (read-char-exclusive)))))
	  (setq answer
		(if (= tem help-char)
		    'help
		  (cdr (assoc tem '((?n . yield)
				    (?\C-g . yield)
				    (?y . proceed)
				    (?r . revert)
				    (?? . help))))))
	  (cond ((null answer)
		 (beep)
		 (message "Please type y, n or r; or ? for help")
		 (sit-for 3))
		((eq answer 'help)
		 (ask-user-about-supersession-help)
		 (setq answer nil))
		((eq answer 'revert)
		 (revert-buffer nil (not (buffer-modified-p)))
					; ask confirmation if buffer modified
		 (signal 'file-supersession
			 (list "File reverted" fn)))
		((eq answer 'yield)
		 (signal 'file-supersession
			 (list "File changed on disk" fn))))))
      (message
        "File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))

(defun ask-user-about-supersession-help ()
  (with-output-to-temp-buffer "*Help*"
    (princ "You want to modify a buffer whose disk file has changed
since you last read it in or saved it with this buffer.

If you say `y' to go ahead and modify this buffer,
you risk ruining the work of whoever rewrote the file.
If you say `r' to revert, the contents of the buffer are refreshed
from the file on disk.
If you say `n', the change you started to make will be aborted.

Usually, you should type `n' and then `M-x revert-buffer',
to get the latest version of the file, then make the change again.")
    (with-current-buffer standard-output
      (help-mode))))

;;; userlock.el ends here
