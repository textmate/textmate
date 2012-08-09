;;; url-history.el --- Global history tracking for URL package

;; Copyright (C) 1996-1999, 2004-2012  Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
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

;;; Code:

;; This can get a recursive require.
;;(require 'url)
(require 'url-parse)
(autoload 'url-do-setup "url")

(defgroup url-history nil
  "History variables in the URL package."
  :prefix "url-history"
  :group 'url)

(defcustom url-history-track nil
  "Controls whether to keep a list of all the URLs being visited.
If non-nil, the URL package will keep track of all the URLs visited.
If set to t, then the list is saved to disk at the end of each Emacs
session."
  :set #'(lambda (var val)
	   (set-default var val)
	   (and (bound-and-true-p url-setup-done)
		(url-history-setup-save-timer)))
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (const :tag "within session" 'session))
  :group 'url-history)

(defcustom url-history-file nil
  "The global history file for the URL package.
This file contains a list of all the URLs you have visited.  This file
is parsed at startup and used to provide URL completion."
  :type '(choice (const :tag "Default" :value nil) file)
  :group 'url-history)

(defcustom url-history-save-interval 3600
  "The number of seconds between automatic saves of the history list.
Default is 1 hour.  Note that if you change this variable outside of
the `customize' interface after `url-do-setup' has been run, you need
to run the `url-history-setup-save-timer' function manually."
  :set #'(lambda (var val)
	   (set-default var val)
	   (if (bound-and-true-p url-setup-done)
	       (url-history-setup-save-timer)))
  :type 'integer
  :group 'url-history)

(defvar url-history-timer nil)

(defvar url-history-changed-since-last-save nil
  "Whether the history list has changed since the last save operation.")

(defvar url-history-hash-table (make-hash-table :size 31 :test 'equal)
  "Hash table for global history completion.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun url-history-setup-save-timer ()
  "Reset the history list timer."
  (interactive)
  (condition-case nil
      (cancel-timer url-history-timer)
    (error nil))
  (setq url-history-timer nil)
  (if (and (eq url-history-track t) url-history-save-interval)
      (setq url-history-timer (run-at-time url-history-save-interval
					   url-history-save-interval
					   'url-history-save-history))))

(defun url-history-parse-history (&optional fname)
  "Parse a history file stored in FNAME."
  ;; Parse out the mosaic global history file for completions, etc.
  (or fname (setq fname (expand-file-name url-history-file)))
  (cond
   ((not (file-exists-p fname))
    ;; It's completely normal for this file not to exist, so don't complain.
    ;; (message "%s does not exist." fname)
    )
   ((not (file-readable-p fname))
    (message "%s is unreadable." fname))
   (t
    (condition-case nil
	(load fname nil t)
      (error (message "Could not load %s" fname))))))

(defun url-history-update-url (url time)
  (setq url-history-changed-since-last-save t)
  (puthash (if (vectorp url) (url-recreate-url url) url) time
           url-history-hash-table))

(autoload 'url-make-private-file "url-util")

(defun url-history-save-history (&optional fname)
  "Write the global history file into `url-history-file'.
The type of data written is determined by what is in the file to begin
with.  If the type of storage cannot be determined, then prompt the
user for what type to save as."
  (interactive)
  (when url-history-changed-since-last-save
    (or fname (setq fname (expand-file-name url-history-file)))
    (if (condition-case nil
            (progn
              (url-make-private-file fname)
              nil)
          (error t))
        (message "Error accessing history file `%s'" fname)
      (let ((make-backup-files nil)
            (version-control nil)
            (require-final-newline t)
            (count 0))
        (with-temp-buffer
          (maphash (lambda (key value)
                     (while (string-match "[\r\n]+" key)
                       (setq key (concat (substring key 0 (match-beginning 0))
                                         (substring key (match-end 0) nil))))
                     (setq count (1+ count))
                     (insert "(puthash \"" key "\""
                             (if (not (stringp value)) " '" "")
                             (prin1-to-string value)
                             " url-history-hash-table)\n"))
                   url-history-hash-table)
          ;; We used to add this in the file, but it just makes the code
          ;; more complex with no benefit.  Worse: it makes it harder to
          ;; preserve preexisting history when loading the history file.
	  ;; (goto-char (point-min))
	  ;; (insert (format
	  ;;          "(setq url-history-hash-table (make-hash-table :size %d :test 'equal))\n"
	  ;;          (/ count 4)))
	  ;; (goto-char (point-max))
	  (insert "\n")
	  (write-file fname)))
      (setq url-history-changed-since-last-save nil))))

(defun url-have-visited-url (url)
  (url-do-setup)
  (gethash url url-history-hash-table nil))

(defun url-completion-function (string predicate function)
  ;; Completion function to complete urls from the history.
  ;; This is obsolete since we can now pass the hash-table directly as a
  ;; completion table.
  (url-do-setup)
  (cond
   ((eq function nil)
    (let ((list nil))
      (maphash (lambda (key val) (push key list))
               url-history-hash-table)
      ;; Not sure why we bother reversing the list.  --Stef
      (try-completion string (nreverse list) predicate)))
   ((eq function t)
    (let ((stub (concat "\\`" (regexp-quote string)))
	  (retval nil))
      (maphash
       (lambda (url time)
         (if (string-match stub url) (push url retval)))
       url-history-hash-table)
      retval))
   ((eq function 'lambda)
    (and (gethash string url-history-hash-table) t))
   (t
    (error "url-completion-function very confused"))))

(provide 'url-history)

;;; url-history.el ends here
