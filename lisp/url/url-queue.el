;;; url-queue.el --- Fetching web pages in parallel

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: comm

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

;; The point of this package is to allow fetching web pages in
;; parallel -- but control the level of parallelism to avoid DoS-ing
;; web servers and Emacs.

;;; Code:

(eval-when-compile (require 'cl))
(require 'browse-url)
(require 'url-parse)

(defcustom url-queue-parallel-processes 6
  "The number of concurrent processes."
  :version "24.1"
  :type 'integer
  :group 'url)

(defcustom url-queue-timeout 5
  "How long to let a job live once it's started (in seconds)."
  :version "24.1"
  :type 'integer
  :group 'url)

;;; Internal variables.

(defvar url-queue nil)

(defstruct url-queue
  url callback cbargs silentp
  buffer start-time pre-triggered
  inhibit-cookiesp)

;;;###autoload
(defun url-queue-retrieve (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
This is like `url-retrieve' (which see for details of the arguments),
but with limits on the degree of parallelism.  The variable
`url-queue-parallel-processes' sets the number of concurrent processes.
The variable `url-queue-timeout' sets a timeout."
  (setq url-queue
	(append url-queue
		(list (make-url-queue :url url
				      :callback callback
				      :cbargs cbargs
				      :silentp silent
				      :inhibit-cookiesp inhibit-cookies))))
  (url-queue-setup-runners))

;; To ensure asynch behaviour, we start the required number of queue
;; runners from `run-with-idle-timer'.  So we're basically going
;; through the queue in two ways: 1) synchronously when a program
;; calls `url-queue-retrieve' (which will then start the required
;; number of queue runners), and 2) at the exit of each job, which
;; will then not start any further threads, but just reuse the
;; previous "slot".

(defun url-queue-setup-runners ()
  (let ((running 0)
	waiting)
    (dolist (entry url-queue)
      (cond
       ((or (url-queue-start-time entry)
	    (url-queue-pre-triggered entry))
	(incf running))
       ((not waiting)
	(setq waiting entry))))
    (when (and waiting
	       (< running url-queue-parallel-processes))
      (setf (url-queue-pre-triggered waiting) t)
      (run-with-idle-timer 0.01 nil 'url-queue-run-queue))))

(defun url-queue-run-queue ()
  (url-queue-prune-old-entries)
  (let ((running 0)
	waiting)
    (dolist (entry url-queue)
      (cond
       ((url-queue-start-time entry)
	(incf running))
       ((not waiting)
	(setq waiting entry))))
    (when (and waiting
	       (< running url-queue-parallel-processes))
      (setf (url-queue-start-time waiting) (float-time))
      (url-queue-start-retrieve waiting))))

(defun url-queue-callback-function (status job)
  (setq url-queue (delq job url-queue))
  (when (and (eq (car status) :error)
	     (eq (cadr (cadr status)) 'connection-failed))
    ;; If we get a connection error, then flush all other jobs from
    ;; the host from the queue.  This particularly makes sense if the
    ;; error really is a DNS resolver issue, which happens
    ;; synchronously and totally halts Emacs.
    (url-queue-remove-jobs-from-host
     (plist-get (nthcdr 3 (cadr status)) :host)))
  (url-queue-run-queue)
  (apply (url-queue-callback job) (cons status (url-queue-cbargs job))))

(defun url-queue-remove-jobs-from-host (host)
  (let ((jobs nil))
    (dolist (job url-queue)
      (when (equal (url-host (url-generic-parse-url (url-queue-url job)))
		   host)
	(push job jobs)))
    (dolist (job jobs)
      (url-queue-kill-job job)
      (setq url-queue (delq job url-queue)))))

(defun url-queue-start-retrieve (job)
  (setf (url-queue-buffer job)
	(ignore-errors
	  (url-retrieve (url-queue-url job)
			#'url-queue-callback-function (list job)
			(url-queue-silentp job)
			(url-queue-inhibit-cookiesp job)))))

(defun url-queue-prune-old-entries ()
  (let (dead-jobs)
    (dolist (job url-queue)
      ;; Kill jobs that have lasted longer than the timeout.
      (when (and (url-queue-start-time job)
		 (> (- (float-time) (url-queue-start-time job))
		    url-queue-timeout))
	(push job dead-jobs)))
    (dolist (job dead-jobs)
      (url-queue-kill-job job)
      (setq url-queue (delq job url-queue)))))

(defun url-queue-kill-job (job)
  (when (bufferp (url-queue-buffer job))
    (let (process)
      (while (setq process (get-buffer-process (url-queue-buffer job)))
	(set-process-sentinel process 'ignore)
	(ignore-errors
	  (delete-process process)))))
  ;; Call the callback with an error message to ensure that the caller
  ;; is notified that the job has failed.
  (with-current-buffer
      (if (and (bufferp (url-queue-buffer job))
	       (buffer-live-p (url-queue-buffer job)))
	  ;; Use the (partially filled) process buffer it it exists.
	  (url-queue-buffer job)
	;; If not, just create a new buffer, which will probably be
	;; killed again by the caller.
	(generate-new-buffer " *temp*"))
    (apply (url-queue-callback job)
	   (cons (list :error (list 'error 'url-queue-timeout
				    "Queue timeout exceeded"))
		 (url-queue-cbargs job)))))

(provide 'url-queue)

;;; url-queue.el ends here
