;;; tq.el --- utility to maintain a transaction queue

;; Copyright (C) 1985-1987, 1992, 2001-2012 Free Software Foundation, Inc.

;; Author: Scott Draves <spot@cs.cmu.edu>
;; Maintainer: FSF
;; Adapted-By: ESR
;; Keywords: extensions

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

;; This file manages receiving a stream asynchronously, parsing it
;; into transactions, and then calling the associated handler function
;; upon the completion of each transaction.

;; Our basic structure is the queue/process/buffer triple.  Each entry
;; of the queue part is a list of question, regexp, closure, and
;; function that is consed to the last element.

;; A transaction queue may be created by calling `tq-create'.

;; A request may be added to the queue by calling `tq-enqueue'.  If
;; the `delay-question' argument is non-nil, we will wait to send the
;; question to the process until it has finished sending other input.
;; Otherwise, once a request is enqueued, we send the given question
;; immediately to the process.

;; We then buffer bytes from the process until we see the regexp that
;; was provided in the call to `tq-enqueue'.  Then we call the
;; provided function with the closure and the collected bytes.  If we
;; have indicated that the question from the next transaction was not
;; sent immediately, send it at this point, awaiting the response.

;;; Code:

;;; Accessors

;; This part looks like (queue . (process . buffer))
(defun tq-queue               (tq) (car tq))
(defun tq-process             (tq) (car (cdr tq)))
(defun tq-buffer              (tq) (cdr (cdr tq)))

;; The structure of `queue' is as follows
;; ((question regexp closure . fn)
;;  <other queue entries>)
;; question: string to send to the process
(defun tq-queue-head-question (tq) (car (car (tq-queue tq))))
;; regexp: regular expression that matches the end of a response from
;; the process
(defun tq-queue-head-regexp   (tq) (car (cdr (car (tq-queue tq)))))
;; closure: additional data to pass to the function
(defun tq-queue-head-closure  (tq) (car (cdr (cdr (car (tq-queue tq))))))
;; fn: function to call upon receiving a complete response from the
;; process
(defun tq-queue-head-fn       (tq) (cdr (cdr (cdr (car (tq-queue tq))))))

;; Determine whether queue is empty
(defun tq-queue-empty         (tq) (not (tq-queue tq)))

;;; Core functionality

;;;###autoload
(defun tq-create (process)
  "Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine."
  (let ((tq (cons nil (cons process
			    (generate-new-buffer
			     (concat " tq-temp-"
				     (process-name process)))))))
    (buffer-disable-undo (tq-buffer tq))
    (set-process-filter process
			`(lambda (proc string)
			   (tq-filter ',tq string)))
    tq))

(defun tq-queue-add (tq question re closure fn)
  (setcar tq (nconc (tq-queue tq)
		    (cons (cons question (cons re (cons closure fn))) nil)))
  'ok)

(defun tq-queue-pop (tq)
  (setcar tq (cdr (car tq)))
  (let ((question (tq-queue-head-question tq)))
    (condition-case nil
	(process-send-string (tq-process tq) question)
      (error nil)))
  (null (car tq)))

(defun tq-enqueue (tq question regexp closure fn &optional delay-question)
  "Add a transaction to transaction queue TQ.
This sends the string QUESTION to the process that TQ communicates with.

When the corresponding answer comes back, we call FN with two
arguments: CLOSURE, which may contain additional data that FN
needs, and the answer to the question.

REGEXP is a regular expression to match the entire answer;
that's how we tell where the answer ends.

If DELAY-QUESTION is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes."
  (let ((sendp (or (not delay-question)
		   (not (tq-queue tq)))))
    (tq-queue-add tq (unless sendp question) regexp closure fn)
    (when sendp
      (process-send-string (tq-process tq) question))))

(defun tq-close (tq)
  "Shut down transaction queue TQ, terminating the process."
  (delete-process (tq-process tq))
  (kill-buffer (tq-buffer tq)))

(defun tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert string)
	(tq-process-buffer tq)))))

(defun tq-process-buffer (tq)
  "Check TQ's buffer for the regexp at the head of the queue."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (set-buffer buffer)
      (if (= 0 (buffer-size)) ()
	(if (tq-queue-empty tq)
	    (let ((buf (generate-new-buffer "*spurious*")))
	      (copy-to-buffer buf (point-min) (point-max))
	      (delete-region (point-min) (point))
	      (pop-to-buffer buf nil)
	      (error "Spurious communication from process %s, see buffer %s"
		     (process-name (tq-process tq))
		     (buffer-name buf)))
	  (goto-char (point-min))
	  (if (re-search-forward (tq-queue-head-regexp tq) nil t)
	      (let ((answer (buffer-substring (point-min) (point))))
		(delete-region (point-min) (point))
		(unwind-protect
		    (condition-case nil
			(funcall (tq-queue-head-fn tq)
				 (tq-queue-head-closure tq)
				 answer)
		      (error nil))
		  (tq-queue-pop tq))
		(tq-process-buffer tq))))))))

(provide 'tq)

;;; tq.el ends here
