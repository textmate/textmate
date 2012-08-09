;;; spam-stat.el --- detecting spam based on statistics

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Keywords: network
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SpamStat

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

;; This implements spam analysis according to Paul Graham in "A Plan
;; for Spam".  The basis for all this is a statistical distribution of
;; words for your spam and non-spam mails.  We need this information
;; in a hash-table so that the analysis can use the information when
;; looking at your mails.  Therefore, before you begin, you need tons
;; of mails (Graham uses 4000 non-spam and 4000 spam mails for his
;; experiments).
;;
;; The main interface to using spam-stat, are the following functions:
;;
;; `spam-stat-buffer-is-spam' -- called in a buffer, that buffer is
;; considered to be a new spam mail; use this for new mail that has
;; not been processed before
;;
;; `spam-stat-buffer-is-non-spam' -- called in a buffer, that buffer
;; is considered to be a new non-spam mail; use this for new mail that
;; has not been processed before
;;
;; `spam-stat-buffer-change-to-spam' -- called in a buffer, that
;; buffer is no longer considered to be normal mail but spam; use this
;; to change the status of a mail that has already been processed as
;; non-spam
;;
;; `spam-stat-buffer-change-to-non-spam' -- called in a buffer, that
;; buffer is no longer considered to be spam but normal mail; use this
;; to change the status of a mail that has already been processed as
;; spam
;;
;; `spam-stat-save' -- save the hash table to the file; the filename
;; used is stored in the variable `spam-stat-file'
;;
;; `spam-stat-load' -- load the hash table from a file; the filename
;; used is stored in the variable `spam-stat-file'
;;
;; `spam-stat-score-word' -- return the spam score for a word
;;
;; `spam-stat-score-buffer' -- return the spam score for a buffer
;;
;; `spam-stat-split-fancy' -- for fancy mail splitting; add
;; the rule (: spam-stat-split-fancy) to `nnmail-split-fancy'
;;
;; This requires the following in your ~/.gnus file:
;;
;; (require 'spam-stat)
;; (spam-stat-load)

;;; Testing:

;; Typical test will involve calls to the following functions:
;;
;; Reset: (spam-stat-reset)
;; Learn spam: (spam-stat-process-spam-directory "~/Mail/mail/spam")
;; Learn non-spam: (spam-stat-process-non-spam-directory "~/Mail/mail/misc")
;; Save table: (spam-stat-save)
;; File size: (nth 7 (file-attributes spam-stat-file))
;; Number of words: (hash-table-count spam-stat)
;; Test spam: (spam-stat-test-directory "~/Mail/mail/spam")
;; Test non-spam: (spam-stat-test-directory "~/Mail/mail/misc")
;; Reduce table size: (spam-stat-reduce-size)
;; Save table: (spam-stat-save)
;; File size: (nth 7 (file-attributes spam-stat-file))
;; Number of words: (hash-table-count spam-stat)
;; Test spam: (spam-stat-test-directory "~/Mail/mail/spam")
;; Test non-spam: (spam-stat-test-directory "~/Mail/mail/misc")

;;; Dictionary Creation:

;; Typically, you will filter away mailing lists etc. using specific
;; rules in `nnmail-split-fancy'.  Somewhere among these rules, you
;; will filter spam.  Here is how you would create your dictionary:

;; Reset: (spam-stat-reset)
;; Learn spam: (spam-stat-process-spam-directory "~/Mail/mail/spam")
;; Learn non-spam: (spam-stat-process-non-spam-directory "~/Mail/mail/misc")
;; Repeat for any other non-spam group you need...
;; Reduce table size: (spam-stat-reduce-size)
;; Save table: (spam-stat-save)

;;; Todo:

;; Speed it up.  Integrate with Gnus such that it uses spam and expiry
;; marks to call the appropriate functions when leaving the summary
;; buffer and saves the hash table when leaving Gnus.  More testing:
;; More mails, disabling SpamAssassin, double checking algorithm, find
;; improved algorithm.

;;; Thanks:

;; Ted Zlatanov <tzz@lifelogs.com>
;; Jesper Harder <harder@myrealbox.com>
;; Dan Schmidt <dfan@dfan.org>
;; Lasse Rasinen <lrasinen@iki.fi>
;; Milan Zamazal <pdm@zamazal.org>



;;; Code:
(require 'mail-parse)

(defvar gnus-original-article-buffer)

(defgroup spam-stat nil
  "Statistical spam detection for Emacs.
Use the functions to build a dictionary of words and their statistical
distribution in spam and non-spam mails.  Then use a function to determine
whether a buffer contains spam or not."
  :version "22.1"
  :group 'gnus)

(defcustom spam-stat-file "~/.spam-stat.el"
  "File used to save and load the dictionary.
See `spam-stat-to-hash-table' for the format of the file."
  :type 'file
  :group 'spam-stat)

(defcustom spam-stat-unknown-word-score 0.2
  "The score to use for unknown words.
Also used for words that don't appear often enough."
  :type 'number
  :group 'spam-stat)

(defcustom spam-stat-max-word-length 15
  "Only words shorter than this will be considered."
  :type 'integer
  :group 'spam-stat)

(defcustom spam-stat-max-buffer-length 10240
  "Only the beginning of buffers will be analyzed.
This variable says how many characters this will be."
  :type 'integer
  :group 'spam-stat)

(defcustom spam-stat-split-fancy-spam-group "mail.spam"
  "Name of the group where spam should be stored.
If `spam-stat-split-fancy' is used in fancy splitting rules.  Has
no effect when spam-stat is invoked through spam.el."
  :type 'string
  :group 'spam-stat)

(defcustom spam-stat-split-fancy-spam-threshold 0.9
  "Spam score threshold in spam-stat-split-fancy."
  :type 'number
  :group 'spam-stat)

(defcustom spam-stat-washing-hook nil
  "Hook applied to each message before analysis."
  :type 'hook
  :group 'spam-stat)

(defcustom spam-stat-score-buffer-user-functions nil
  "List of additional scoring functions.
Called  one by one on the buffer.

If all of these functions return non-nil answers, these numerical
answers are added to the computed spam stat score on the buffer.  If
you defun such functions, make sure they don't return the buffer in a
narrowed state or such: use, for example, `save-excursion'.  Each of
your functions is also passed the initial spam-stat score which might
aid in your scoring.

Also be careful when defining such functions.  If they take a long
time, they will slow down your mail splitting.  Thus, if the buffer is
large, don't forget to use smaller regions, by wrapping your work in,
say, `with-spam-stat-max-buffer-size'."
  :type '(repeat sexp)
  :group 'spam-stat)

(defcustom spam-stat-process-directory-age 90
  "Max. age of files to be processed in directory, in days.
When using `spam-stat-process-spam-directory' or
`spam-stat-process-non-spam-directory', only files that have
been touched in this many days will be considered.  Without
this filter, re-training spam-stat with several thousand messages
will start to take a very long time."
  :type 'number
  :group 'spam-stat)

(defvar spam-stat-last-saved-at nil
  "Time stamp of last change of spam-stat-file on this run")

(defvar spam-stat-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?! "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?+ "w" table)
    table)
  "Syntax table used when processing mails for statistical analysis.
The important part is which characters are word constituents.")

(defvar spam-stat-dirty nil
  "Whether the spam-stat database needs saving.")

(defvar spam-stat-buffer nil
  "Buffer to use for scoring while splitting.
This is set by hooking into Gnus.")

(defvar spam-stat-buffer-name " *spam stat buffer*"
  "Name of the `spam-stat-buffer'.")

(defvar spam-stat-coding-system
  (if (mm-coding-system-p 'emacs-mule) 'emacs-mule 'raw-text)
  "Coding system used for `spam-stat-file'.")

;; Hooking into Gnus

(defun spam-stat-store-current-buffer ()
  "Store a copy of the current buffer in `spam-stat-buffer'."
  (let ((buf (current-buffer)))
    (with-current-buffer (get-buffer-create spam-stat-buffer-name)
      (erase-buffer)
      (insert-buffer-substring buf)
      (setq spam-stat-buffer (current-buffer)))))

(defun spam-stat-store-gnus-article-buffer ()
  "Store a copy of the current article in `spam-stat-buffer'.
This uses `gnus-article-buffer'."
  (with-current-buffer gnus-original-article-buffer
    (spam-stat-store-current-buffer)))

;; Data -- not using defstruct in order to save space and time

(defvar spam-stat (make-hash-table :test 'equal)
  "Hash table used to store the statistics.
Use `spam-stat-load' to load the file.
Every word is used as a key in this table.  The value is a vector.
Use `spam-stat-ngood', `spam-stat-nbad', `spam-stat-good',
`spam-stat-bad', and `spam-stat-score' to access this vector.")

(defvar spam-stat-ngood 0
  "The number of good mails in the dictionary.")

(defvar spam-stat-nbad 0
  "The number of bad mails in the dictionary.")

(defvar spam-stat-error-holder nil
  "A holder for condition-case errors while scoring buffers.")

(defsubst spam-stat-good (entry)
  "Return the number of times this word belongs to good mails."
  (aref entry 0))

(defsubst spam-stat-bad (entry)
  "Return the number of times this word belongs to bad mails."
  (aref entry 1))

(defsubst spam-stat-score (entry)
  "Set the score of this word."
  (if entry
      (aref entry 2)
    spam-stat-unknown-word-score))

(defsubst spam-stat-set-good (entry value)
  "Set the number of times this word belongs to good mails."
  (aset entry 0 value))

(defsubst spam-stat-set-bad (entry value)
  "Set the number of times this word belongs to bad mails."
  (aset entry 1 value))

(defsubst spam-stat-set-score (entry value)
  "Set the score of this word."
  (aset entry 2 value))

(defsubst spam-stat-make-entry (good bad)
  "Return a vector with the given properties."
  (let ((entry (vector good bad nil)))
    (spam-stat-set-score entry (spam-stat-compute-score entry))
    entry))

;; Computing

(defun spam-stat-compute-score (entry)
  "Compute the score of this word.  1.0 means spam."
   ;; promote all numbers to floats for the divisions
   (let* ((g (* 2.0 (spam-stat-good entry)))
	  (b (float (spam-stat-bad entry))))
     (cond ((< (+ g b) 5)
	    .2)
	   ((= 0 spam-stat-ngood)
	    .99)
	   ((= 0 spam-stat-nbad)
	    .01)
	   (t
	    (max .01
		 (min .99 (/ (/ b spam-stat-nbad)
			     (+ (/ g spam-stat-ngood)
				(/ b spam-stat-nbad)))))))))

;; Parsing

(defmacro with-spam-stat-max-buffer-size (&rest body)
  "Narrow the buffer down to the first 4k characters, then evaluate BODY."
  `(save-restriction
     (when (> (- (point-max)
		 (point-min))
	      spam-stat-max-buffer-length)
       (narrow-to-region (point-min)
			 (+ (point-min) spam-stat-max-buffer-length)))
     ,@body))

(defun spam-stat-buffer-words ()
  "Return a hash table of words and number of occurrences in the buffer."
  (run-hooks 'spam-stat-washing-hook)
  (with-spam-stat-max-buffer-size
   (with-syntax-table spam-stat-syntax-table
     (goto-char (point-min))
     (let ((result (make-hash-table :test 'equal))
	   word count)
       (while (re-search-forward "\\w+" nil t)
	 (setq word (match-string-no-properties 0)
	       count (1+ (gethash word result 0)))
	 (when (< (length word) spam-stat-max-word-length)
	   (puthash word count result)))
       result))))

(defun spam-stat-buffer-is-spam ()
  "Consider current buffer to be a new spam mail."
  (setq spam-stat-nbad (1+ spam-stat-nbad))
  (maphash
   (lambda (word count)
     (let ((entry (gethash word spam-stat)))
       (if entry
	   (spam-stat-set-bad entry (+ count (spam-stat-bad entry)))
	 (setq entry (spam-stat-make-entry 0 count)))
       (spam-stat-set-score entry (spam-stat-compute-score entry))
       (puthash word entry spam-stat)))
   (spam-stat-buffer-words))
  (setq spam-stat-dirty t))

(defun spam-stat-buffer-is-non-spam ()
  "Consider current buffer to be a new non-spam mail."
  (setq spam-stat-ngood (1+ spam-stat-ngood))
  (maphash
   (lambda (word count)
     (let ((entry (gethash word spam-stat)))
       (if entry
	   (spam-stat-set-good entry (+ count (spam-stat-good entry)))
	 (setq entry (spam-stat-make-entry count 0)))
       (spam-stat-set-score entry (spam-stat-compute-score entry))
       (puthash word entry spam-stat)))
   (spam-stat-buffer-words))
  (setq spam-stat-dirty t))

(autoload 'gnus-message "gnus-util")

(defun spam-stat-buffer-change-to-spam ()
  "Consider current buffer no longer normal mail but spam."
  (setq spam-stat-nbad (1+ spam-stat-nbad)
	spam-stat-ngood (1- spam-stat-ngood))
  (maphash
   (lambda (word count)
     (let ((entry (gethash word spam-stat)))
       (if (not entry)
	   (gnus-message 8 "This buffer has unknown words in it")
	 (spam-stat-set-good entry (- (spam-stat-good entry) count))
	 (spam-stat-set-bad entry (+ (spam-stat-bad entry) count))
	 (spam-stat-set-score entry (spam-stat-compute-score entry))
	 (puthash word entry spam-stat))))
   (spam-stat-buffer-words))
  (setq spam-stat-dirty t))

(defun spam-stat-buffer-change-to-non-spam ()
  "Consider current buffer no longer spam but normal mail."
  (setq spam-stat-nbad (1- spam-stat-nbad)
	spam-stat-ngood (1+ spam-stat-ngood))
  (maphash
   (lambda (word count)
     (let ((entry (gethash word spam-stat)))
       (if (not entry)
	   (gnus-message 8 "This buffer has unknown words in it")
	 (spam-stat-set-good entry (+ (spam-stat-good entry) count))
	 (spam-stat-set-bad entry (- (spam-stat-bad entry) count))
	 (spam-stat-set-score entry (spam-stat-compute-score entry))
	 (puthash word entry spam-stat))))
   (spam-stat-buffer-words))
  (setq spam-stat-dirty t))

;; Saving and Loading

(defun spam-stat-save (&optional force)
  "Save the `spam-stat' hash table as lisp file.
With a prefix argument save unconditionally."
  (interactive "P")
  (when (or force spam-stat-dirty)
    (let ((coding-system-for-write spam-stat-coding-system))
      (with-temp-file spam-stat-file
	(let ((standard-output (current-buffer))
	      (font-lock-maximum-size 0))
	  (insert (format ";-*- coding: %s; -*-\n" spam-stat-coding-system))
	  (insert (format "(setq spam-stat-ngood %d spam-stat-nbad %d
spam-stat (spam-stat-to-hash-table '(" spam-stat-ngood spam-stat-nbad))
	  (maphash (lambda (word entry)
		     (prin1 (list word
				  (spam-stat-good entry)
				  (spam-stat-bad entry))))
		   spam-stat)
	  (insert ")))"))))
    (message "Saved %s."  spam-stat-file)
    (setq spam-stat-dirty nil
          spam-stat-last-saved-at (nth 5 (file-attributes spam-stat-file)))))

(defun spam-stat-load ()
  "Read the `spam-stat' hash table from disk."
  ;; TODO: maybe we should warn the user if spam-stat-dirty is t?
  (let ((coding-system-for-read spam-stat-coding-system))
    (cond (spam-stat-dirty (message "Spam stat not loaded: spam-stat-dirty t"))
          ((or (not (boundp 'spam-stat-last-saved-at))
               (null spam-stat-last-saved-at)
               (not (equal spam-stat-last-saved-at
                           (nth 5 (file-attributes spam-stat-file)))))
           (progn
             (load-file spam-stat-file)
             (setq spam-stat-dirty nil
                   spam-stat-last-saved-at
                   (nth 5 (file-attributes spam-stat-file)))))
          (t (message "Spam stat file not loaded: no change in disk.")))))

(defun spam-stat-to-hash-table (entries)
  "Turn list ENTRIES into a hash table and store as `spam-stat'.
Every element in ENTRIES has the form \(WORD GOOD BAD) where WORD is
the word string, NGOOD is the number of good mails it has appeared in,
NBAD is the number of bad mails it has appeared in, GOOD is the number
of times it appeared in good mails, and BAD is the number of times it
has appeared in bad mails."
  (let ((table (make-hash-table :size (length entries)
				:test 'equal)))
    (mapc (lambda (l)
	    (puthash (car l)
		     (spam-stat-make-entry (nth 1 l) (nth 2 l))
		     table))
	  entries)
    table))

(defun spam-stat-reset ()
  "Reset `spam-stat' to an empty hash-table.
This deletes all the statistics."
  (interactive)
  (setq spam-stat (make-hash-table :test 'equal)
	spam-stat-ngood 0
	spam-stat-nbad 0)
  (setq spam-stat-dirty t))

;; Scoring buffers

(defvar spam-stat-score-data nil
  "Raw data used in the last run of `spam-stat-score-buffer'.")

(defsubst spam-stat-score-word (word)
  "Return score for WORD.
The default score for unknown words is stored in
`spam-stat-unknown-word-score'."
  (spam-stat-score (gethash word spam-stat)))

(defun spam-stat-buffer-words-with-scores ()
  "Process current buffer, return the 15 most conspicuous words.
These are the words whose spam-stat differs the most from 0.5.
The list returned contains elements of the form \(WORD SCORE DIFF),
where DIFF is the difference between SCORE and 0.5."
  (let (result word score)
    (maphash (lambda (word ignore)
	       (setq score (spam-stat-score-word word)
		     result (cons (list word score (abs (- score 0.5)))
				  result)))
	     (spam-stat-buffer-words))
    (setq result (sort result (lambda (a b) (< (nth 2 b) (nth 2 a)))))
    (setcdr (nthcdr 14 result) nil)
    result))

(defun spam-stat-score-buffer ()
  "Return a score describing the spam-probability for this buffer.
Add user supplied modifications if supplied."
  (interactive) ; helps in debugging.
  (setq spam-stat-score-data (spam-stat-buffer-words-with-scores))
  (let* ((probs (mapcar 'cadr spam-stat-score-data))
	 (prod (apply #'* probs))
	 (score0
	  (/ prod (+ prod (apply #'* (mapcar #'(lambda (x) (- 1 x))
					     probs)))))
	 (score1s
	  (condition-case
	      spam-stat-error-holder
	      (spam-stat-score-buffer-user score0)
	    (error nil)))
	 (ans
	  (if score1s (+ score0 score1s) score0)))
    (when (interactive-p)
      (message "%S" ans))
    ans))

(defun spam-stat-score-buffer-user (&rest args)
  (let* ((scores
	  (mapcar
	   (lambda (fn)
	     (apply fn args))
	   spam-stat-score-buffer-user-functions)))
    (if (memq nil scores) nil
      (apply #'+ scores))))

(defun spam-stat-split-fancy ()
  "Return the name of the spam group if the current mail is spam.
Use this function on `nnmail-split-fancy'.  If you are interested in
the raw data used for the last run of `spam-stat-score-buffer',
check the variable `spam-stat-score-data'."
  (condition-case spam-stat-error-holder
      (progn
	(set-buffer spam-stat-buffer)
	(goto-char (point-min))
	(when (> (spam-stat-score-buffer) spam-stat-split-fancy-spam-threshold)
	  (when (boundp 'nnmail-split-trace)
	    (mapc (lambda (entry)
		    (push entry nnmail-split-trace))
		  spam-stat-score-data))
	  spam-stat-split-fancy-spam-group))
    (error (message "Error in spam-stat-split-fancy: %S" spam-stat-error-holder)
	   nil)))

;; Testing

(defun spam-stat-strip-xref ()
  "Strip the Xref header."
  (save-restriction
    (mail-narrow-to-head)
    (when (re-search-forward "^Xref:.*\n" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(autoload 'time-to-number-of-days "time-date")

(defun spam-stat-process-directory (dir func)
  "Process all the regular files in directory DIR using function FUNC."
  (let* ((files (directory-files dir t "^[^.]"))
	 (max (/ (length files) 100.0))
	 (count 0))
    (with-temp-buffer
      (dolist (f files)
	(when (and (file-readable-p f)
		   (file-regular-p f)
                   (> (nth 7 (file-attributes f)) 0)
		   (< (time-to-number-of-days (time-since (nth 5 (file-attributes f))))
		      spam-stat-process-directory-age))
	  (setq count (1+ count))
	  (message "Reading %s: %.2f%%" dir (/ count max))
	  (insert-file-contents-literally f)
	  (spam-stat-strip-xref)
	  (funcall func)
	  (erase-buffer))))))

(defun spam-stat-process-spam-directory (dir)
  "Process all the regular files in directory DIR as spam."
  (interactive "D")
  (spam-stat-process-directory dir 'spam-stat-buffer-is-spam))

(defun spam-stat-process-non-spam-directory (dir)
  "Process all the regular files in directory DIR as non-spam."
  (interactive "D")
  (spam-stat-process-directory dir 'spam-stat-buffer-is-non-spam))

(defun spam-stat-count ()
  "Return size of `spam-stat'."
  (interactive)
  (hash-table-count spam-stat))

(defun spam-stat-test-directory (dir &optional verbose)
  "Test all the regular files in directory DIR for spam.
If the result is 1.0, then all files are considered spam.
If the result is 0.0, non of the files is considered spam.
You can use this to determine error rates.

If VERBOSE is non-nil display names of files detected as spam or
non-spam in a temporary buffer.  If it is the symbol `ham',
display non-spam files; otherwise display spam files."
  (interactive "DDirectory: ")
  (let* ((files (directory-files dir t "^[^.]"))
	 display-files
	 buffer-score
	 (total (length files))
	 (score 0.0); float
	 (max (/ total 100.0)); float
	 (count 0))
    (with-temp-buffer
      (dolist (f files)
	(when (and (file-readable-p f)
		   (file-regular-p f)
                   (> (nth 7 (file-attributes f)) 0))
	  (setq count (1+ count))
	  (message "Reading %.2f%%, score %.2f"
	  	   (/ count max) (/ score count))
	  (insert-file-contents-literally f)
	  (setq buffer-score (spam-stat-score-buffer))
	  (when (> buffer-score 0.9)
	    (setq score (1+ score)))
	  (when verbose
	    (if (> buffer-score 0.9)
		(unless (eq verbose 'ham) (push f display-files))
	      (when (eq verbose 'ham) (push f display-files))))
	  (erase-buffer))))
    (when display-files
      (with-output-to-temp-buffer "*spam-stat results*"
	(dolist (file display-files)
	  (princ file)
	  (terpri))))
    (message "Final score: %d / %d = %f" score total (/ score total))))

;; Shrinking the dictionary

(defun spam-stat-reduce-size (&optional count)
  "Reduce the size of `spam-stat'.
This removes all words that occur less than COUNT from the dictionary.
COUNT defaults to 5"
  (interactive)
  (setq count (or count 5))
  (maphash (lambda (key entry)
	     (when (< (+ (spam-stat-good entry)
			 (spam-stat-bad entry))
		      count)
	       (remhash key spam-stat)))
	   spam-stat)
  (setq spam-stat-dirty t))

(defun spam-stat-install-hooks-function ()
  "Install the spam-stat function hooks."
  (interactive)
  (add-hook 'nnmail-prepare-incoming-message-hook
	    'spam-stat-store-current-buffer)
  (add-hook 'gnus-select-article-hook
	    'spam-stat-store-gnus-article-buffer))

(defun spam-stat-unload-hook ()
  "Uninstall the spam-stat function hooks."
  (interactive)
  (remove-hook 'nnmail-prepare-incoming-message-hook
	       'spam-stat-store-current-buffer)
  (remove-hook 'gnus-select-article-hook
	       'spam-stat-store-gnus-article-buffer))

(add-hook 'spam-stat-unload-hook 'spam-stat-unload-hook)

(provide 'spam-stat)

;;; spam-stat.el ends here
