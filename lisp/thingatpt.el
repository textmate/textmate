;;; thingatpt.el --- get the `thing' at point

;; Copyright (C) 1991-1998, 2000-2012  Free Software Foundation, Inc.

;; Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;; Maintainer: FSF
;; Keywords: extensions, matching, mouse
;; Created: Thu Mar 28 13:48:23 1991

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

;; This file provides routines for getting the "thing" at the location of
;; point, whatever that "thing" happens to be.  The "thing" is defined by
;; its beginning and end positions in the buffer.
;;
;; The function bounds-of-thing-at-point finds the beginning and end
;; positions by moving first forward to the end of the "thing", and then
;; backwards to the beginning.  By default, it uses the corresponding
;; forward-"thing" operator (eg. forward-word, forward-line).
;;
;; Special cases are allowed for using properties associated with the named
;; "thing":
;;
;;   forward-op		Function to call to skip forward over a "thing" (or
;;                      with a negative argument, backward).
;;
;;   beginning-op	Function to call to skip to the beginning of a "thing".
;;   end-op		Function to call to skip to the end of a "thing".
;;
;; Reliance on existing operators means that many `things' can be accessed
;; without further code:  eg.
;;     (thing-at-point 'line)
;;     (thing-at-point 'page)

;;; Code:

(provide 'thingatpt)

;; Basic movement

;;;###autoload
(defun forward-thing (thing &optional n)
  "Move forward to the end of the Nth next THING.
THING should be a symbol specifying a type of syntactic entity.
Possibilities include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `word', `sentence', `whitespace',
`line', and `page'."
  (let ((forward-op (or (get thing 'forward-op)
			(intern-soft (format "forward-%s" thing)))))
    (if (functionp forward-op)
	(funcall forward-op (or n 1))
      (error "Can't determine how to move over a %s" thing))))

;; General routines

;;;###autoload
(defun bounds-of-thing-at-point (thing)
  "Determine the start and end buffer locations for the THING at point.
THING should be a symbol specifying a type of syntactic entity.
Possibilities include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `word', `sentence', `whitespace',
`line', and `page'.

See the file `thingatpt.el' for documentation on how to define a
valid THING.

Return a cons cell (START . END) giving the start and end
positions of the thing found."
  (if (get thing 'bounds-of-thing-at-point)
      (funcall (get thing 'bounds-of-thing-at-point))
    (let ((orig (point)))
      (condition-case nil
	  (save-excursion
	    ;; Try moving forward, then back.
            (funcall ;; First move to end.
             (or (get thing 'end-op)
                 (lambda () (forward-thing thing 1))))
            (funcall ;; Then move to beg.
             (or (get thing 'beginning-op)
                 (lambda () (forward-thing thing -1))))
	    (let ((beg (point)))
	      (if (<= beg orig)
		  ;; If that brings us all the way back to ORIG,
		  ;; it worked.  But END may not be the real end.
		  ;; So find the real end that corresponds to BEG.
                  ;; FIXME: in which cases can `real-end' differ from `end'?
		  (let ((real-end
			 (progn
			   (funcall
			    (or (get thing 'end-op)
                                (lambda () (forward-thing thing 1))))
			   (point))))
		    (when (and (<= orig real-end) (< beg real-end))
                      (cons beg real-end)))
		(goto-char orig)
		;; Try a second time, moving backward first and then forward,
		;; so that we can find a thing that ends at ORIG.
                (funcall ;; First, move to beg.
                 (or (get thing 'beginning-op)
                     (lambda () (forward-thing thing -1))))
                (funcall ;; Then move to end.
                 (or (get thing 'end-op)
                     (lambda () (forward-thing thing 1))))
		(let ((end (point))
                      (real-beg
		       (progn
			 (funcall
			  (or (get thing 'beginning-op)
                              (lambda () (forward-thing thing -1))))
			 (point))))
		  (if (and (<= real-beg orig) (<= orig end) (< real-beg end))
		      (cons real-beg end))))))
	(error nil)))))

;;;###autoload
(defun thing-at-point (thing)
  "Return the THING at point.
THING should be a symbol specifying a type of syntactic entity.
Possibilities include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `word', `sentence', `whitespace',
`line', and `page'.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (if (get thing 'thing-at-point)
      (funcall (get thing 'thing-at-point))
    (let ((bounds (bounds-of-thing-at-point thing)))
      (if bounds
	  (buffer-substring (car bounds) (cdr bounds))))))

;; Go to beginning/end

(defun beginning-of-thing (thing)
  "Move point to the beginning of THING.
The bounds of THING are determined by `bounds-of-thing-at-point'."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (car bounds))))

(defun end-of-thing (thing)
  "Move point to the end of THING.
The bounds of THING are determined by `bounds-of-thing-at-point'."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (cdr bounds))))

;;  Special cases

;;  Lines

;; bolp will be false when you click on the last line in the buffer
;; and it has no final newline.

(put 'line 'beginning-op
     (lambda () (if (bolp) (forward-line -1) (beginning-of-line))))

;;  Sexps

(defun in-string-p ()
  "Return non-nil if point is in a string.
\[This is an internal function.]"
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) orig)))))

(defun end-of-sexp ()
  "Move point to the end of the current sexp.
\[This is an internal function.]"
  (let ((char-syntax (char-syntax (char-after))))
    (if (or (eq char-syntax ?\))
	    (and (eq char-syntax ?\") (in-string-p)))
	(forward-char 1)
      (forward-sexp 1))))

(put 'sexp 'end-op 'end-of-sexp)

(defun beginning-of-sexp ()
  "Move point to the beginning of the current sexp.
\[This is an internal function.]"
  (let ((char-syntax (char-syntax (char-before))))
    (if (or (eq char-syntax ?\()
	    (and (eq char-syntax ?\") (in-string-p)))
	(forward-char -1)
      (forward-sexp -1))))

(put 'sexp 'beginning-op 'beginning-of-sexp)

;;  Lists

(put 'list 'bounds-of-thing-at-point 'thing-at-point-bounds-of-list-at-point)

(defun thing-at-point-bounds-of-list-at-point ()
  "Return the bounds of the list at point.
\[Internal function used by `bounds-of-thing-at-point'.]"
  (save-excursion
    (let ((opoint (point))
	  (beg (condition-case nil
		   (progn (up-list -1)
			  (point))
		 (error nil))))
      (condition-case nil
	  (if beg
	      (progn (forward-sexp)
		     (cons beg (point)))
	    ;; Are we are at the beginning of a top-level sexp?
	    (forward-sexp)
	    (let ((end (point)))
	      (backward-sexp)
	      (if (>= opoint (point))
		  (cons opoint end))))
	(error nil)))))

;; Defuns

(put 'defun 'beginning-op 'beginning-of-defun)
(put 'defun 'end-op       'end-of-defun)
(put 'defun 'forward-op   'end-of-defun)

;;  Filenames and URLs  www.com/foo%32bar

(defvar thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,:"
  "Characters allowable in filenames.")

(put 'filename 'end-op
     (lambda ()
       (re-search-forward (concat "\\=[" thing-at-point-file-name-chars "]*")
			  nil t)))
(put 'filename 'beginning-op
     (lambda ()
       (if (re-search-backward (concat "[^" thing-at-point-file-name-chars "]")
			       nil t)
	   (forward-char)
	 (goto-char (point-min)))))

(defvar thing-at-point-url-path-regexp
  "[^]\t\n \"'<>[^`{}]*[^]\t\n \"'<>[^`{}.,;]+"
  "A regular expression probably matching the host and filename or e-mail part of a URL.")

(defvar thing-at-point-short-url-regexp
  (concat "[-A-Za-z0-9]+\\.[-A-Za-z0-9.]+" thing-at-point-url-path-regexp)
  "A regular expression probably matching a URL without an access scheme.
Hostname matching is stricter in this case than for
``thing-at-point-url-regexp''.")

(defvar thing-at-point-uri-schemes
  ;; Officials from http://www.iana.org/assignments/uri-schemes.html
  '("ftp://" "http://" "gopher://" "mailto:" "news:" "nntp:"
    "telnet://" "wais://" "file:/" "prospero:" "z39.50s:" "z39.50r:"
    "cid:" "mid:" "vemmi:" "service:" "imap:" "nfs:" "acap:" "rtsp:"
    "tip:" "pop:" "data:" "dav:" "opaquelocktoken:" "sip:" "tel:" "fax:"
    "modem:" "ldap:" "https://" "soap.beep:" "soap.beeps:" "urn:" "go:"
    "afs:" "tn3270:" "mailserver:"
    "crid:" "dict:" "dns:" "dtn:" "h323:" "im:" "info:" "ipp:"
    "iris.beep:" "mtqp:" "mupdate:" "pres:" "sips:" "snmp:" "tag:"
    "tftp:" "xmlrpc.beep:" "xmlrpc.beeps:" "xmpp:"
  ;; Compatibility
    "snews:" "irc:" "mms://" "mmsh://")
  "Uniform Resource Identifier (URI) Schemes.")

(defvar thing-at-point-url-regexp
  (concat "\\<\\(" (mapconcat 'identity thing-at-point-uri-schemes "\\|") "\\)"
          thing-at-point-url-path-regexp)
  "A regular expression probably matching a complete URL.")

(defvar thing-at-point-markedup-url-regexp
  "<URL:[^>]+>"
  "A regular expression matching a URL marked up per RFC1738.
This may contain whitespace (including newlines) .")

(put 'url 'bounds-of-thing-at-point 'thing-at-point-bounds-of-url-at-point)
(defun thing-at-point-bounds-of-url-at-point ()
  (let ((strip (thing-at-point-looking-at
			 thing-at-point-markedup-url-regexp))) ;; (url "") short
    (if (or strip
	    (thing-at-point-looking-at thing-at-point-url-regexp)
	    ;; Access scheme omitted?
	    ;; (setq short (thing-at-point-looking-at
	    ;;     	 thing-at-point-short-url-regexp))
            )
	(let ((beginning (match-beginning 0))
	      (end (match-end 0)))
	  (when strip
            (setq beginning (+ beginning 5))
            (setq end (- end 1)))
	  (cons beginning end)))))

(put 'url 'thing-at-point 'thing-at-point-url-at-point)
(defun thing-at-point-url-at-point ()
  "Return the URL around or before point.

Search backwards for the start of a URL ending at or after point.  If
no URL found, return nil.  The access scheme will be prepended if
absent: \"mailto:\" if the string contains \"@\", \"ftp://\" if it
starts with \"ftp\" and not \"ftp:/\", or \"http://\" by default."

  (let ((url "") short strip)
    (if (or (setq strip (thing-at-point-looking-at
			 thing-at-point-markedup-url-regexp))
	    (thing-at-point-looking-at thing-at-point-url-regexp)
	    ;; Access scheme omitted?
	    (setq short (thing-at-point-looking-at
			 thing-at-point-short-url-regexp)))
	(progn
	  (setq url (buffer-substring-no-properties (match-beginning 0)
						    (match-end 0)))
	  (and strip (setq url (substring url 5 -1))) ; Drop "<URL:" & ">"
	  ;; strip whitespace
	  (while (string-match "[ \t\n\r]+" url)
	    (setq url (replace-match "" t t url)))
	  (and short (setq url (concat (cond ((string-match "^[a-zA-Z]+:" url)
					       ;; already has a URL scheme.
					       "")
					     ((string-match "@" url)
                                              "mailto:")
					     ;; e.g. ftp.swiss... or ftp-swiss...
                                             ((string-match "^ftp" url)
                                              "ftp://")
                                             (t "http://"))
                                       url)))
	  (if (string-equal "" url)
	      nil
	    url)))))

;; The normal thingatpt mechanism doesn't work for complex regexps.
;; This should work for almost any regexp wherever we are in the
;; match.  To do a perfect job for any arbitrary regexp would mean
;; testing every position before point.  Regexp searches won't find
;; matches that straddle the start position so we search forwards once
;; and then back repeatedly and then back up a char at a time.

(defun thing-at-point-looking-at (regexp)
  "Return non-nil if point is in or just after a match for REGEXP.
Set the match data from the earliest such match ending at or after
point."
  (save-excursion
    (let ((old-point (point)) match)
      (and (looking-at regexp)
	   (>= (match-end 0) old-point)
	   (setq match (point)))
      ;; Search back repeatedly from end of next match.
      ;; This may fail if next match ends before this match does.
      (re-search-forward regexp nil 'limit)
      (while (and (re-search-backward regexp nil t)
		  (or (> (match-beginning 0) old-point)
		      (and (looking-at regexp)	; Extend match-end past search start
			   (>= (match-end 0) old-point)
			   (setq match (point))))))
      (if (not match) nil
	(goto-char match)
	;; Back up a char at a time in case search skipped
	;; intermediate match straddling search start pos.
	(while (and (not (bobp))
		    (progn (backward-char 1) (looking-at regexp))
		    (>= (match-end 0) old-point)
		    (setq match (point))))
	(goto-char match)
	(looking-at regexp)))))

(put 'url 'end-op
     (lambda ()
       (let ((bounds (thing-at-point-bounds-of-url-at-point)))
         (if bounds
             (goto-char (cdr bounds))
           (error "No URL here")))))
(put 'url 'beginning-op
     (lambda ()
       (let ((bounds (thing-at-point-bounds-of-url-at-point)))
         (if bounds
             (goto-char (car bounds))
           (error "No URL here")))))

;;   Email addresses
(defvar thing-at-point-email-regexp
  "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
  "A regular expression probably matching an email address.
This does not match the real name portion, only the address, optionally
with angle brackets.")

;; Haven't set 'forward-op on 'email nor defined 'forward-email' because
;; not sure they're actually needed, and URL seems to skip them too.
;; Note that (end-of-thing 'email) and (beginning-of-thing 'email)
;; work automagically, though.

(put 'email 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at thing-at-point-email-regexp)))
         (if thing
             (let ((beginning (match-beginning 0))
                   (end (match-end 0)))
               (cons beginning end))))))

(put 'email 'thing-at-point
     (lambda ()
       (let ((boundary-pair (bounds-of-thing-at-point 'email)))
         (if boundary-pair
             (buffer-substring-no-properties
              (car boundary-pair) (cdr boundary-pair))))))

;;  Whitespace

(defun forward-whitespace (arg)
  "Move point to the end of the next sequence of whitespace chars.
Each such sequence may be a single newline, or a sequence of
consecutive space and/or tab characters.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
  (interactive "p")
  (if (natnump arg)
      (re-search-forward "[ \t]+\\|\n" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "[ \t]+\\|\n" nil 'move)
	  (or (eq (char-after (match-beginning 0)) ?\n)
	      (skip-chars-backward " \t")))
      (setq arg (1+ arg)))))

;;  Buffer

(put 'buffer 'end-op (lambda () (goto-char (point-max))))
(put 'buffer 'beginning-op (lambda () (goto-char (point-min))))

;;  Symbols

(defun forward-symbol (arg)
  "Move point to the next position that is the end of a symbol.
A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
  (interactive "p")
  (if (natnump arg)
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil 'move)
	  (skip-syntax-backward "w_"))
      (setq arg (1+ arg)))))

;;  Syntax blocks

(defun forward-same-syntax (&optional arg)
  "Move point past all characters with the same syntax class.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
  (interactive "p")
  (while (< arg 0)
    (skip-syntax-backward
     (char-to-string (char-syntax (char-before))))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (skip-syntax-forward (char-to-string (char-syntax (char-after))))
    (setq arg (1- arg))))

;;  Aliases

(defun word-at-point ()
  "Return the word at point.  See `thing-at-point'."
  (thing-at-point 'word))

(defun sentence-at-point ()
  "Return the sentence at point.  See `thing-at-point'."
  (thing-at-point 'sentence))

(defun read-from-whole-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used."
  (let* ((read-data (read-from-string str))
	 (more-left
	  (condition-case nil
	      ;; The call to `ignore' suppresses a compiler warning.
	      (progn (ignore (read-from-string (substring str (cdr read-data))))
		     t)
	    (end-of-file nil))))
    (if more-left
	(error "Can't read whole string")
      (car read-data))))

(defun form-at-point (&optional thing pred)
  (let ((sexp (condition-case nil
		  (read-from-whole-string (thing-at-point (or thing 'sexp)))
		(error nil))))
    (if (or (not pred) (funcall pred sexp)) sexp)))

;;;###autoload
(defun sexp-at-point ()
  "Return the sexp at point, or nil if none is found."
  (form-at-point 'sexp))
;;;###autoload
(defun symbol-at-point ()
  "Return the symbol at point, or nil if none is found."
  (let ((thing (thing-at-point 'symbol)))
    (if thing (intern thing))))
;;;###autoload
(defun number-at-point ()
  "Return the number at point, or nil if none is found."
  (form-at-point 'sexp 'numberp))
;;;###autoload
(defun list-at-point ()
  "Return the Lisp list at point, or nil if none is found."
  (form-at-point 'list 'listp))

;;; thingatpt.el ends here
