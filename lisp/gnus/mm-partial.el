;;; mm-partial.el --- showing message/partial

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: message partial

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

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus-sum)
(require 'mm-util)
(require 'mm-decode)

(defun mm-partial-find-parts (id &optional art)
  (let ((headers (with-current-buffer gnus-summary-buffer
		   gnus-newsgroup-headers))
	phandles header)
    (while (setq header (pop headers))
      (unless (eq (aref header 0) art)
	(mm-with-unibyte-buffer
	  (gnus-request-article-this-buffer (aref header 0)
					    gnus-newsgroup-name)
	  (when (search-forward id nil t)
	    (let ((nhandles (mm-dissect-buffer
			     nil gnus-article-loose-mime)) nid)
	      (if (consp (car nhandles))
		  (mm-destroy-parts nhandles)
		(setq nid (cdr (assq 'id
				     (cdr (mm-handle-type nhandles)))))
		(if (not (equal id nid))
		    (mm-destroy-parts nhandles)
		  (push nhandles phandles))))))))
    phandles))

;;;###autoload
(defun mm-inline-partial (handle &optional no-display)
  "Show the partial part of HANDLE.
This function replaces the buffer of HANDLE with a buffer contains
the entire message.
If NO-DISPLAY is nil, display it. Otherwise, do nothing after replacing."
  (let ((id (cdr (assq 'id (cdr (mm-handle-type handle)))))
	phandles
	(b (point)) (n 1) total
	phandle nn ntotal
	gnus-displaying-mime handles buffer)
    (unless (mm-handle-cache handle)
      (unless id
	(error "Can not find message/partial id"))
      (setq phandles
	    (sort (cons handle
			(mm-partial-find-parts
			 id
			 (with-current-buffer gnus-summary-buffer
			   (gnus-summary-article-number))))
		  #'(lambda (a b)
		      (let ((anumber (string-to-number
				      (cdr (assq 'number
						 (cdr (mm-handle-type a))))))
			    (bnumber (string-to-number
				      (cdr (assq 'number
						 (cdr (mm-handle-type b)))))))
			(< anumber bnumber)))))
      (setq gnus-article-mime-handles
	    (mm-merge-handles gnus-article-mime-handles phandles))
      (with-current-buffer (generate-new-buffer " *mm*")
	(while (setq phandle (pop phandles))
	  (setq nn (string-to-number
		    (cdr (assq 'number
			       (cdr (mm-handle-type phandle))))))
	  (setq ntotal (string-to-number
			(cdr (assq 'total
				   (cdr (mm-handle-type phandle))))))
	  (if ntotal
	      (if total
		  (unless (eq total ntotal)
		  (error "The numbers of total are different"))
		(setq total ntotal)))
	  (unless (< nn n)
	    (unless (eq nn n)
	      (error "Missing part %d" n))
	    (mm-insert-part phandle)
	    (goto-char (point-max))
	    (when (not (eq 0 (skip-chars-backward "\r\n")))
	      ;; remove tail blank spaces except one
	      (if (looking-at "\r?\n")
		  (goto-char (match-end 0)))
	      (delete-region (point) (point-max)))
	    (setq n (+ n 1))))
	(unless total
	  (error "Don't known the total number of"))
	(if (<= n total)
	    (error "Missing part %d" n))
	(kill-buffer (mm-handle-buffer handle))
	(goto-char (point-min))
	(let ((point (if (search-forward "\n\n" nil t)
			 (1- (point))
		       (point-max))))
	  (goto-char (point-min))
	  (unless (re-search-forward "^mime-version:" point t)
	    (insert "MIME-Version: 1.0\n")))
	(setcar handle (current-buffer))
	(mm-handle-set-cache handle t)))
    (unless no-display
      (save-excursion
	(save-restriction
	  (narrow-to-region b b)
	  (mm-insert-part handle)
	  (let (gnus-article-mime-handles)
	    (run-hooks 'gnus-article-decode-hook)
	    (gnus-article-prepare-display)
	    (setq handles gnus-article-mime-handles))
	  (when handles
	    ;; It is in article buffer.
	    (setq gnus-article-mime-handles
		  (mm-merge-handles gnus-article-mime-handles handles)))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(condition-case nil
		    ;; This is only valid on XEmacs.
		    (mapcar (lambda (prop)
			    (remove-specifier
			     (face-property 'default prop) (current-buffer)))
			    '(background background-pixmap foreground))
		  (error nil))
		(delete-region ,(point-min-marker) ,(point-max-marker))))))))))

(provide 'mm-partial)

;;; mm-partial.el ends here
