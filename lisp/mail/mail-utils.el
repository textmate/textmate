;;; mail-utils.el --- utility functions used both by rmail and rnews

;; Copyright (C) 1985, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail, news

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

;; Utility functions for mail and netnews handling.  These handle fine
;; points of header parsing.

;;; Code:

;;;###autoload
(defcustom mail-use-rfc822 nil
  "If non-nil, use a full, hairy RFC822 parser on mail addresses.
Otherwise, (the default) use a smaller, somewhat faster, and
often correct parser."
  :type 'boolean
  :group 'mail)

;;;###autoload
(defcustom mail-dont-reply-to-names nil
  "Regexp specifying addresses to prune from a reply message.
If this is nil, it is set the first time you compose a reply, to
a value which excludes your own email address.

Matching addresses are excluded from the CC field in replies, and
also the To field, unless this would leave an empty To field."
  :type '(choice regexp (const :tag "Your Name" nil))
  :group 'mail)

;; Returns t if file FILE is an Rmail file.
;;;###autoload
(defun mail-file-babyl-p (file)
  "Return non-nil if FILE is a Babyl file."
  (with-temp-buffer
    (insert-file-contents file nil 0 100)
    (looking-at "BABYL OPTIONS:")))

(defun mail-string-delete (string start end)
  "Returns a string containing all of STRING except the part
from START (inclusive) to END (exclusive)."
  (if (null end) (substring string 0 start)
    (concat (substring string 0 start)
	    (substring string end nil))))

;;;###autoload
(defun mail-quote-printable (string &optional wrapper)
  "Convert a string to the \"quoted printable\" Q encoding if necessary.
If the string contains only ASCII characters and no troublesome ones,
we return it unconverted.

If the optional argument WRAPPER is non-nil,
we add the wrapper characters =?ISO-8859-1?Q?....?=."
  (let ((i 0) (result ""))
    (save-match-data
      (while (or (string-match "[?=\"]" string i)
		 (string-match "[^\000-\177]" string i))
	(setq result
	      (concat result (substring string i (match-beginning 0))
		      (upcase (format "=%02x"
				      (aref string (match-beginning 0))))))
	(setq i (match-end 0)))
      (if wrapper
	  (concat "=?ISO-8859-1?Q?"
		  result (substring string i)
		  "?=")
	(concat result (substring string i))))))

;;;###autoload
(defun mail-quote-printable-region (beg end &optional wrapper)
  "Convert the region to the \"quoted printable\" Q encoding.
If the optional argument WRAPPER is non-nil,
we add the wrapper characters =?ISO-8859-1?Q?....?=."
  (interactive "r\nP")
  (save-match-data
    (save-excursion
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(while (re-search-forward "[?=\"\200-\377]" nil t)
	  (replace-match (upcase (format "=%02x" (preceding-char)))
			 t t))
	(when wrapper
	  (goto-char beg)
	  (insert "=?ISO-8859-1?Q?")
	  (goto-char end)
	  (insert "?="))))))

(defun mail-unquote-printable-hexdigit (char)
  (setq char (upcase char))
  (if (>= char ?A)
      (+ (- char ?A) 10)
    (- char ?0)))

;;;###autoload
(defun mail-unquote-printable (string &optional wrapper)
  "Undo the \"quoted printable\" encoding.
If the optional argument WRAPPER is non-nil,
we expect to find and remove the wrapper characters =?ISO-8859-1?Q?....?=."
  (save-match-data
    (and wrapper
	 (string-match "\\`=\\?ISO-8859-1\\?Q\\?\\([^?]*\\)\\?" string)
	 (setq string (match-string 1 string)))
    (let ((i 0) strings)
      (while (string-match "=\\(..\\|\n\\)" string i)
	(setq strings (cons (substring string i (match-beginning 0)) strings))
	(unless (= (aref string (match-beginning 1)) ?\n)
	  (setq strings
		(cons (make-string 1
				   (+ (* 16 (mail-unquote-printable-hexdigit
					     (aref string (match-beginning 1))))
				      (mail-unquote-printable-hexdigit
				       (aref string (1+ (match-beginning 1))))))
		      strings)))
	(setq i (match-end 0)))
      (apply 'concat (nreverse (cons (substring string i) strings))))))

;; FIXME Gnus for some reason has `quoted-printable-decode-region' in qp.el.
;;;###autoload
(defun mail-unquote-printable-region (beg end &optional wrapper noerror
					  unibyte)
  "Undo the \"quoted printable\" encoding in buffer from BEG to END.
If the optional argument WRAPPER is non-nil,
we expect to find and remove the wrapper characters =?ISO-8859-1?Q?....?=.
On encountering malformed quoted-printable text, exits with an error,
unless NOERROR is non-nil, in which case it continues, and returns nil
when finished.  Returns non-nil on successful completion.
If UNIBYTE is non-nil, insert converted characters as unibyte.
That is useful if you are going to character code decoding afterward,
as Rmail does."
  ;; FIXME: `unibyte' should always be non-nil, and the iso-latin-1
  ;; specific handling should be removed (or moved elsewhere and generalized).
  (interactive "r\nP")
  (let (failed)
    (save-match-data
      (save-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (when (and wrapper
		     (looking-at "\\`=\\?ISO-8859-1\\?Q\\?\\([^?]*\\)\\?"))
	    (delete-region (match-end 1) end)
	    (delete-region (point) (match-beginning 1)))
	  (while (re-search-forward "=\\(\\([0-9A-F][0-9A-F]\\)\\|[=\n]\\|..\\)" nil t)
	    (goto-char (match-end 0))
	    (cond ((= (char-after (match-beginning 1)) ?\n)
		   (replace-match ""))
		  ((= (char-after (match-beginning 1)) ?=)
		   (replace-match "="))
		  ((match-beginning 2)
		   (let ((char (+ (* 16 (mail-unquote-printable-hexdigit
					 (char-after (match-beginning 2))))
				  (mail-unquote-printable-hexdigit
				   (char-after (1+ (match-beginning 2)))))))
		     (if unibyte
			 (progn
			   (replace-match "")
			   ;; insert-byte will insert this as a
			   ;; corresponding eight-bit character.
			   (insert-byte char 1))
		       (replace-match (make-string 1 char) t t))))
		  (noerror
		   (setq failed t))
		  (t
		   (error "Malformed MIME quoted-printable message"))))
	  (not failed))))))

(eval-when-compile (require 'rfc822))

(defun mail-strip-quoted-names (address)
  "Delete comments and quoted strings in an address list ADDRESS.
Also delete leading/trailing whitespace and replace FOO <BAR> with just BAR.
Return a modified address list."
  (if (null address)
      nil
    (if mail-use-rfc822
	(progn (require 'rfc822)
	       (mapconcat 'identity (rfc822-addresses address) ", "))
      (let (pos)

        ;; Strip comments.
        (while (setq pos (string-match
                          "[ \t]*(\\([^()\\]\\|\\\\.\\|\\\\\n\\)*)"
                          address))
          (setq address (replace-match "" nil nil address 0)))

        ;; strip surrounding whitespace
        (string-match "\\`[ \t\n]*" address)
        (setq address (substring address
                                 (match-end 0)
                                 (string-match "[ \t\n]*\\'" address
                                               (match-end 0))))

        ;; strip `quoted' names (This is supposed to hack `"Foo Bar" <bar@host>')
        (setq pos 0)
        (while (setq pos (string-match
                          "\\([ \t]?\\)\\([ \t]*\"\\([^\"\\]\\|\\\\.\\|\\\\\n\\)*\"[ \t\n]*\\)"
			  address pos))
          ;; If the next thing is "@", we have "foo bar"@host.  Leave it.
          (if (and (> (length address) (match-end 0))
                   (= (aref address (match-end 0)) ?@))
              (setq pos (match-end 0))
            ;; Otherwise discard the "..." part.
            (setq address (replace-match "" nil nil address 2))))
        ;; If this address contains <...>, replace it with just
        ;; the part between the <...>.
        (while (setq pos (string-match "\\(,\\s-*\\|\\`\\)\\([^,]*<\\([^>,:]*\\)>[^,]*\\)\\(\\s-*,\\|\\'\\)"
                                       address))
          (setq address (replace-match (match-string 3 address)
                                       nil 'literal address 2)))
        address))))

(defun mail-dont-reply-to (destinations)
  "Prune addresses from DESTINATIONS, a list of recipient addresses.
Remove all addresses matching `mail-dont-reply-to-names' from the
comma-separated list, and return the pruned list."
  ;; FIXME this (setting a user option the first time a command is used)
  ;; is somewhat strange.  Normally one would never set the option,
  ;; but instead fall back to the default so long as it was nil.
  ;; Or just set the default directly in the defcustom.
  (if (null mail-dont-reply-to-names)
      (setq mail-dont-reply-to-names
	    (concat
	     ;; `rmail-default-dont-reply-to-names' is obsolete.
	     (if (bound-and-true-p rmail-default-dont-reply-to-names)
		 (concat rmail-default-dont-reply-to-names "\\|")
	       "")
	     (if (and user-mail-address
		      (not (equal user-mail-address user-login-name)))
		 ;; Anchor the login name and email address so that we
		 ;; don't match substrings: if the login name is
		 ;; "foo", we shouldn't match "barfoo@baz.com".
		 (concat "\\`"
			 (regexp-quote user-mail-address)
			 "\\'\\|")
	       "")
	     (concat "\\`" (regexp-quote user-login-name) "@"))))
  ;; Split up DESTINATIONS and match each element separately.
  (let ((start-pos 0) (cur-pos 0)
	(case-fold-search t))
    (while start-pos
      (setq cur-pos (string-match "[,\"]" destinations cur-pos))
      (if (and cur-pos (equal (match-string 0 destinations) "\""))
	  ;; Search for matching quote.
	  (let ((next-pos (string-match "\"" destinations (1+ cur-pos))))
	    (if next-pos
		(setq cur-pos (1+ next-pos))
	      ;; If the open-quote has no close-quote,
	      ;; delete the open-quote to get something well-defined.
	      ;; This case is not valid, but it can happen if things
	      ;; are weird elsewhere.
	      (setq destinations (concat (substring destinations 0 cur-pos)
				    (substring destinations (1+ cur-pos))))
	      (setq cur-pos start-pos)))
	(let* ((address (substring destinations start-pos cur-pos))
	       (naked-address (mail-strip-quoted-names address)))
	  (if (string-match mail-dont-reply-to-names naked-address)
	      (setq destinations (concat (substring destinations 0 start-pos)
				    (and cur-pos (substring destinations
							    (1+ cur-pos))))
		    cur-pos start-pos)
	    (setq cur-pos (and cur-pos (1+ cur-pos))
		  start-pos cur-pos))))))
  ;; get rid of any trailing commas
  (let ((pos (string-match "[ ,\t\n]*\\'" destinations)))
    (if pos
        (setq destinations (substring destinations 0 pos))))
  ;; remove leading spaces. they bother me.
  (if (string-match "\\(\\s \\|,\\)*" destinations)
      (substring destinations (match-end 0))
    destinations))

;; Legacy name
(define-obsolete-function-alias 'rmail-dont-reply-to 'mail-dont-reply-to "24.1")


;;;###autoload
(defun mail-fetch-field (field-name &optional last all list)
  "Return the value of the header field whose type is FIELD-NAME.
If second arg LAST is non-nil, use the last field of type FIELD-NAME.
If third arg ALL is non-nil, concatenate all such fields with commas between.
If 4th arg LIST is non-nil, return a list of all such fields.
The buffer should be narrowed to just the header, else false
matches may be returned from the message body."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (name (concat "^" (regexp-quote field-name) "[ \t]*:[ \t]*")))
      (if (or all list)
	  (let ((value (if all "")))
	    (while (re-search-forward name nil t)
	      (let ((opoint (point)))
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		;; Back up over newline, then trailing spaces or tabs
		(forward-char -1)
		(skip-chars-backward " \t" opoint)
		(if list
		    (setq value (cons (buffer-substring-no-properties
				       opoint (point))
				      value))
		  (setq value (concat value
				      (if (string= value "") "" ", ")
				      (buffer-substring-no-properties
				       opoint (point)))))))
	    (if list
		value
	      (and (not (string= value "")) value)))
	(if (re-search-forward name nil t)
	    (progn
	      (if last (while (re-search-forward name nil t)))
	      (let ((opoint (point)))
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		;; Back up over newline, then trailing spaces or tabs
		(forward-char -1)
		(skip-chars-backward " \t" opoint)
		(buffer-substring-no-properties opoint (point)))))))))

;; Parse a list of tokens separated by commas.
;; It runs from point to the end of the visible part of the buffer.
;; Whitespace before or after tokens is ignored,
;; but whitespace within tokens is kept.
(defun mail-parse-comma-list ()
  (let (accumulated
	beg)
    (skip-chars-forward " \t\n")
    (while (not (eobp))
      (setq beg (point))
      (skip-chars-forward "^,")
      (skip-chars-backward " \t\n")
      (setq accumulated
	    (cons (buffer-substring-no-properties beg (point))
		  accumulated))
      (skip-chars-forward "^,")
      (skip-chars-forward ", \t\n"))
    accumulated))

(defun mail-comma-list-regexp (labels)
  (let (pos)
    (setq pos (or (string-match "[^ \t]" labels) 0))
    ;; Remove leading and trailing whitespace.
    (setq labels (substring labels pos (string-match "[ \t]*$" labels pos)))
    ;; Change each comma to \|, and flush surrounding whitespace.
    (while (setq pos (string-match "[ \t]*,[ \t]*" labels))
      (setq labels
	    (concat (substring labels 0 pos)
		    "\\|"
		    (substring labels (match-end 0))))))
  labels)

(defun mail-rfc822-time-zone (time)
  (let* ((sec (or (car (current-time-zone time)) 0))
	 (absmin (/ (abs sec) 60)))
    (format "%c%02d%02d" (if (< sec 0) ?- ?+) (/ absmin 60) (% absmin 60))))

(defun mail-rfc822-date ()
  (let* ((time (current-time))
	 (s (current-time-string time)))
    (string-match "[^ ]+ +\\([^ ]+\\) +\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)" s)
    (concat (substring s (match-beginning 2) (match-end 2)) " "
	    (substring s (match-beginning 1) (match-end 1)) " "
	    (substring s (match-beginning 4) (match-end 4)) " "
	    (substring s (match-beginning 3) (match-end 3)) " "
	    (mail-rfc822-time-zone time))))

(defun mail-mbox-from ()
  "Return an mbox \"From \" line for the current message.
The buffer should be narrowed to just the header."
  (let* ((from (mail-strip-quoted-names (or (mail-fetch-field "from")
					    (mail-fetch-field "really-from")
					    (mail-fetch-field "sender")
					    (mail-fetch-field "return-path")
					    "unknown")))
	 (date (mail-fetch-field "date"))
	 ;; A From: header can contain multiple addresses, a "From "
	 ;; line must contain only one.  (Bug#7760)
	 ;; See eg RFC 5322, 3.6.2. Originator Fields.
	 (end (string-match "[ \t]*[,\n]" from)))
    (format "From %s %s\n" (if end
			       (substring from 0 end)
			     from)
	    (or (and date
		     (ignore-errors
		      (current-time-string (date-to-time date))))
		(current-time-string)))))

(provide 'mail-utils)

;;; mail-utils.el ends here
