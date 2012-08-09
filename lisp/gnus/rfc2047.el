;;; rfc2047.el --- functions for encoding and decoding rfc2047 messages

;; Copyright (C) 1998-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;; RFC 2047 is "MIME (Multipurpose Internet Mail Extensions) Part
;; Three:  Message Header Extensions for Non-ASCII Text".

;;; Code:

(eval-when-compile
  (require 'cl))
(defvar message-posting-charset)

(require 'mm-util)
(require 'ietf-drums)
;; Fixme: Avoid this (used for mail-parse-charset) mm dependence on gnus.
(require 'mail-prsvr)
(require 'rfc2045) ;; rfc2045-encode-string
(autoload 'mm-body-7-or-8 "mm-bodies")

(defvar rfc2047-header-encoding-alist
  '(("Newsgroups" . nil)
    ("Followup-To" . nil)
    ("Message-ID" . nil)
    ("\\(Resent-\\)?\\(From\\|Cc\\|To\\|Bcc\\|\\(In-\\)?Reply-To\\|Sender\
\\|Mail-Followup-To\\|Mail-Copies-To\\|Approved\\)" . address-mime)
    (t . mime))
  "*Header/encoding method alist.
The list is traversed sequentially.  The keys can either be
header regexps or t.

The values can be:

1) nil, in which case no encoding is done;
2) `mime', in which case the header will be encoded according to RFC2047;
3) `address-mime', like `mime', but takes account of the rules for address
   fields (where quoted strings and comments must be treated separately);
4) a charset, in which case it will be encoded as that charset;
5) `default', in which case the field will be encoded as the rest
   of the article.")

(defvar rfc2047-charset-encoding-alist
  '((us-ascii . nil)
    (iso-8859-1 . Q)
    (iso-8859-2 . Q)
    (iso-8859-3 . Q)
    (iso-8859-4 . Q)
    (iso-8859-5 . B)
    (koi8-r . B)
    (iso-8859-7 . B)
    (iso-8859-8 . B)
    (iso-8859-9 . Q)
    (iso-8859-14 . Q)
    (iso-8859-15 . Q)
    (iso-2022-jp . B)
    (iso-2022-kr . B)
    (gb2312 . B)
    (gbk . B)
    (gb18030 . B)
    (big5 . B)
    (cn-big5 . B)
    (cn-gb . B)
    (cn-gb-2312 . B)
    (euc-kr . B)
    (iso-2022-jp-2 . B)
    (iso-2022-int-1 . B)
    (viscii . Q))
  "Alist of MIME charsets to RFC2047 encodings.
Valid encodings are nil, `Q' and `B'.  These indicate binary (no) encoding,
quoted-printable and base64 respectively.")

(defvar rfc2047-encode-function-alist
  '((Q . rfc2047-q-encode-string)
    (B . rfc2047-b-encode-string)
    (nil . identity))
  "Alist of RFC2047 encodings to encoding functions.")

(defvar rfc2047-encode-encoded-words t
  "Whether encoded words should be encoded again.")

(defvar rfc2047-allow-irregular-q-encoded-words t
  "*Whether to decode irregular Q-encoded words.")

(eval-and-compile ;; Necessary to hard code them in `rfc2047-decode-region'.
  (defconst rfc2047-encoded-word-regexp
    "=\\?\\([^][\000-\040()<>@,\;:*\\\"/?.=]+\\)\\(?:\\*[^?]+\\)?\\?\
\\(B\\?[+/0-9A-Za-z]*=*\
\\|Q\\?[ ->@-~]*\
\\)\\?="
    "Regexp that matches encoded word."
    ;; The patterns for the B encoding and the Q encoding, i.e. the ones
    ;; beginning with "B" and "Q" respectively, are restricted into only
    ;; the characters that those encodings may generally use.
    )
  (defconst rfc2047-encoded-word-regexp-loose
    "=\\?\\([^][\000-\040()<>@,\;:*\\\"/?.=]+\\)\\(?:\\*[^?]+\\)?\\?\
\\(B\\?[+/0-9A-Za-z]*=*\
\\|Q\\?\\(?:\\?+[ -<>@-~]\\)?\\(?:[ ->@-~]+\\?+[ -<>@-~]\\)*[ ->@-~]*\\?*\
\\)\\?="
    "Regexp that matches encoded word allowing loose Q encoding."
    ;; The pattern for the Q encoding, i.e. the one beginning with "Q",
    ;; is similar to:
    ;; "Q\\?\\(\\?+[^\n=?]\\)?\\([^\n?]+\\?+[^\n=?]\\)*[^\n?]*\\?*"
    ;;      <--------1-------><----------2,3----------><--4--><-5->
    ;; They mean:
    ;; 1. After "Q?", allow "?"s that follow a character other than "=".
    ;; 2. Allow "=" after "Q?"; it isn't regarded as the terminator.
    ;; 3. In the middle of an encoded word, allow "?"s that follow a
    ;;    character other than "=".
    ;; 4. Allow any characters other than "?" in the middle of an
    ;;    encoded word.
    ;; 5. At the end, allow "?"s.
    ))

;;;
;;; Functions for encoding RFC2047 messages
;;;

(defun rfc2047-qp-or-base64 ()
  "Return the type with which to encode the buffer.
This is either `base64' or `quoted-printable'."
  (save-excursion
    (let ((limit (min (point-max) (+ 2000 (point-min))))
	  (n8bit 0))
      (goto-char (point-min))
      (skip-chars-forward "\x20-\x7f\r\n\t" limit)
      (while (< (point) limit)
	(incf n8bit)
	(forward-char 1)
	(skip-chars-forward "\x20-\x7f\r\n\t" limit))
      (if (or (< (* 6 n8bit) (- limit (point-min)))
	      ;; Don't base64, say, a short line with a single
	      ;; non-ASCII char when splitting parts by charset.
	      (= n8bit 1))
	  'quoted-printable
	'base64))))

(defun rfc2047-narrow-to-field ()
  "Narrow the buffer to the header on the current line."
  (beginning-of-line)
  (narrow-to-region
   (point)
   (progn
     (forward-line 1)
     (if (re-search-forward "^[^ \n\t]" nil t)
	 (point-at-bol)
       (point-max))))
  (goto-char (point-min)))

(defun rfc2047-field-value ()
  "Return the value of the field at point."
  (save-excursion
    (save-restriction
      (rfc2047-narrow-to-field)
      (re-search-forward ":[ \t\n]*" nil t)
      (buffer-substring-no-properties (point) (point-max)))))

(defun rfc2047-quote-special-characters-in-quoted-strings (&optional
							   encodable-regexp)
  "Quote special characters with `\\'s in quoted strings.
Quoting will not be done in a quoted string if it contains characters
matching ENCODABLE-REGEXP or it is within parentheses."
  (goto-char (point-min))
  (let ((tspecials (concat "[" ietf-drums-tspecials "]"))
	(start (point))
	beg end)
    (with-syntax-table (standard-syntax-table)
      (while (not (eobp))
	(if (ignore-errors
	      (forward-list 1)
	      (eq (char-before) ?\)))
	    (forward-list -1)
	  (goto-char (point-max)))
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (while (search-forward "\"" nil t)
	    (setq beg (match-beginning 0))
	    (unless (eq (char-before beg) ?\\)
	      (goto-char beg)
	      (setq beg (1+ beg))
	      (condition-case nil
		  (progn
		    (forward-sexp)
		    (setq end (1- (point)))
		    (goto-char beg)
		    (if (and encodable-regexp
			     (re-search-forward encodable-regexp end t))
			(goto-char (1+ end))
		      (save-restriction
			(narrow-to-region beg end)
			(while (re-search-forward tspecials nil 'move)
			  (if (eq (char-before) ?\\)
			      (if (looking-at tspecials) ;; Already quoted.
				  (forward-char)
				(insert "\\"))
			    (goto-char (match-beginning 0))
			    (insert "\\")
			    (forward-char))))
		      (forward-char)))
		(error
		 (goto-char beg)))))
	  (goto-char (point-max)))
	(forward-list 1)
	(setq start (point))))))

(defvar rfc2047-encoding-type 'address-mime
  "The type of encoding done by `rfc2047-encode-region'.
This should be dynamically bound around calls to
`rfc2047-encode-region' to either `mime' or `address-mime'.  See
`rfc2047-header-encoding-alist', for definitions.")

(defun rfc2047-encode-message-header ()
  "Encode the message header according to `rfc2047-header-encoding-alist'.
Should be called narrowed to the head of the message."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (let (alist elem method)
      (while (not (eobp))
	(save-restriction
	  (rfc2047-narrow-to-field)
	  (setq method nil
		alist rfc2047-header-encoding-alist)
	  (while (setq elem (pop alist))
	    (when (or (and (stringp (car elem))
			   (looking-at (car elem)))
		      (eq (car elem) t))
	      (setq alist nil
		    method (cdr elem))))
	  (if (not (rfc2047-encodable-p))
	      (prog2
		  (when (eq method 'address-mime)
		    (rfc2047-quote-special-characters-in-quoted-strings))
		  (if (and (eq (mm-body-7-or-8) '8bit)
			   (mm-multibyte-p)
			   (mm-coding-system-p
			    (car message-posting-charset)))
		      ;; 8 bit must be decoded.
		      (mm-encode-coding-region
		       (point-min) (point-max)
		       (mm-charset-to-coding-system
			(car message-posting-charset))))
		;; No encoding necessary, but folding is nice
		(when nil
		  (rfc2047-fold-region
		   (save-excursion
		     (goto-char (point-min))
		     (skip-chars-forward "^:")
		     (when (looking-at ": ")
		       (forward-char 2))
		     (point))
		   (point-max))))
	    ;; We found something that may perhaps be encoded.
	    (re-search-forward "^[^:]+: *" nil t)
	    (cond
	     ((eq method 'address-mime)
	      (rfc2047-encode-region (point) (point-max)))
	     ((eq method 'mime)
	      (let ((rfc2047-encoding-type 'mime))
		(rfc2047-encode-region (point) (point-max))))
	     ((eq method 'default)
	      (if (and (featurep 'mule)
		       (if (boundp 'enable-multibyte-characters)
			   (default-value 'enable-multibyte-characters))
		       mail-parse-charset)
		  (mm-encode-coding-region (point) (point-max)
					   mail-parse-charset)))
	     ;; We get this when CC'ing messages to newsgroups with
	     ;; 8-bit names.  The group name mail copy just got
	     ;; unconditionally encoded.  Previously, it would ask
	     ;; whether to encode, which was quite confusing for the
	     ;; user.  If the new behavior is wrong, tell me.  I have
	     ;; left the old code commented out below.
	     ;; -- Per Abrahamsen <abraham@dina.kvl.dk> Date: 2001-10-07.
	     ;; Modified by Dave Love, with the commented-out code changed
	     ;; in accordance with changes elsewhere.
	     ((null method)
	      (rfc2047-encode-region (point) (point-max)))
;;; 	     ((null method)
;;; 	      (if (or (message-options-get
;;; 		       'rfc2047-encode-message-header-encode-any)
;;; 		      (message-options-set
;;; 		       'rfc2047-encode-message-header-encode-any
;;; 		       (y-or-n-p
;;; 			"Some texts are not encoded. Encode anyway?")))
;;; 		  (rfc2047-encode-region (point-min) (point-max))
;;; 		(error "Cannot send unencoded text")))
	     ((mm-coding-system-p method)
	      (if (or (and (featurep 'mule)
			   (if (boundp 'enable-multibyte-characters)
			       (default-value 'enable-multibyte-characters)))
		      (featurep 'file-coding))
		  (mm-encode-coding-region (point) (point-max) method)))
	     ;; Hm.
	     (t)))
	  (goto-char (point-max)))))))

;; Fixme: This, and the require below may not be the Right Thing, but
;; should be safe just before release.  -- fx 2001-02-08

(defun rfc2047-encodable-p ()
  "Return non-nil if any characters in current buffer need encoding in headers.
The buffer may be narrowed."
  (require 'message)			; for message-posting-charset
  (let ((charsets
	 (mm-find-mime-charset-region (point-min) (point-max))))
    (goto-char (point-min))
    (or (and rfc2047-encode-encoded-words
	     (prog1
		 (re-search-forward rfc2047-encoded-word-regexp nil t)
	       (goto-char (point-min))))
	(and charsets
	     (not (equal charsets (list (car message-posting-charset))))))))

;; Use this syntax table when parsing into regions that may need
;; encoding.  Double quotes are string delimiters, backslash is
;; character quoting, and all other RFC 2822 special characters are
;; treated as punctuation so we can use forward-sexp/forward-word to
;; skip to the end of regions appropriately.  Nb. ietf-drums does
;; things differently.
(defconst rfc2047-syntax-table
  ;; (make-char-table 'syntax-table '(2)) only works in Emacs.
  (let ((table (make-syntax-table)))
    ;; The following is done to work for setting all elements of the table;
    ;; it appears to be the cleanest way.
    ;; Play safe and don't assume the form of the word syntax entry --
    ;; copy it from ?a.
    (if (featurep 'xemacs)
	(put-char-table t (get-char-table ?a (standard-syntax-table)) table)
      (set-char-table-range table t (aref (standard-syntax-table) ?a)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\( "(" table)
    (modify-syntax-entry ?\) ")" table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?@ "." table)
    table))

(defun rfc2047-encode-region (b e &optional dont-fold)
  "Encode words in region B to E that need encoding.
By default, the region is treated as containing RFC2822 addresses.
Dynamically bind `rfc2047-encoding-type' to change that."
  (save-restriction
    (narrow-to-region b e)
    (let ((encodable-regexp (if rfc2047-encode-encoded-words
				"[^\000-\177]+\\|=\\?"
			      "[^\000-\177]+"))
	  start				; start of current token
	  end begin csyntax
	  ;; Whether there's an encoded word before the current token,
	  ;; either immediately or separated by space.
	  last-encoded
	  (orig-text (buffer-substring-no-properties b e)))
      (if (eq 'mime rfc2047-encoding-type)
	  ;; Simple case.  Continuous words in which all those contain
	  ;; non-ASCII characters are encoded collectively.  Encoding
	  ;; ASCII words, including `Re:' used in Subject headers, is
	  ;; avoided for interoperability with non-MIME clients and
	  ;; for making it easy to find keywords.
	  (progn
	    (goto-char (point-min))
	    (while (progn (skip-chars-forward " \t\n")
			  (not (eobp)))
	      (setq start (point))
	      (while (and (looking-at "[ \t\n]*\\([^ \t\n]+\\)")
			  (progn
			    (setq end (match-end 0))
			    (re-search-forward encodable-regexp end t)))
		(goto-char end))
	      (if (> (point) start)
		  (rfc2047-encode start (point))
		(goto-char end))))
	;; `address-mime' case -- take care of quoted words, comments.
	(rfc2047-quote-special-characters-in-quoted-strings encodable-regexp)
	(with-syntax-table rfc2047-syntax-table
	  (goto-char (point-min))
	  (condition-case err		; in case of unbalanced quotes
	      ;; Look for rfc2822-style: sequences of atoms, quoted
	      ;; strings, specials, whitespace.  (Specials mustn't be
	      ;; encoded.)
	      (while (not (eobp))
		;; Skip whitespace.
		(skip-chars-forward " \t\n")
		(setq start (point))
		(cond
		 ((not (char-after)))	; eob
		 ;; else token start
		 ((eq ?\" (setq csyntax (char-syntax (char-after))))
		  ;; Quoted word.
		  (forward-sexp)
		  (setq end (point))
		  ;; Does it need encoding?
		  (goto-char start)
		  (if (re-search-forward encodable-regexp end 'move)
		      ;; It needs encoding.  Strip the quotes first,
		      ;; since encoded words can't occur in quotes.
		      (progn
			(goto-char end)
			(delete-char -1)
			(goto-char start)
			(delete-char 1)
			(when last-encoded
			  ;; There was a preceding quoted word.  We need
			  ;; to include any separating whitespace in this
			  ;; word to avoid it getting lost.
			  (skip-chars-backward " \t")
			  ;; A space is needed between the encoded words.
			  (insert ? )
			  (setq start (point)
				end (1+ end)))
			;; Adjust the end position for the deleted quotes.
			(rfc2047-encode start (- end 2))
			(setq last-encoded t)) ; record that it was encoded
		    (setq last-encoded  nil)))
		 ((eq ?. csyntax)
		  ;; Skip other delimiters, but record that they've
		  ;; potentially separated quoted words.
		  (forward-char)
		  (setq last-encoded nil))
		 ((eq ?\) csyntax)
		  (error "Unbalanced parentheses"))
		 ((eq ?\( csyntax)
		  ;; Look for the end of parentheses.
		  (forward-list)
		  ;; Encode text as an unstructured field.
		  (let ((rfc2047-encoding-type 'mime))
		    (rfc2047-encode-region (1+ start) (1- (point))))
		  (skip-chars-forward ")"))
		 (t		    ; normal token/whitespace sequence
		  ;; Find the end.
		  ;; Skip one ASCII word, or encode continuous words
		  ;; in which all those contain non-ASCII characters.
		  (setq end nil)
		  (while (not (or end (eobp)))
		    (when (looking-at "[\000-\177]+")
		      (setq begin (point)
			    end (match-end 0))
		      (when (progn
			      (while (and (or (re-search-forward
					       "[ \t\n]\\|\\Sw" end 'move)
					      (setq end nil))
					  (eq ?\\ (char-syntax (char-before))))
				;; Skip backslash-quoted characters.
				(forward-char))
			      end)
			(setq end (match-beginning 0))
			(if rfc2047-encode-encoded-words
			    (progn
			      (goto-char begin)
			      (when (search-forward "=?" end 'move)
				(goto-char (match-beginning 0))
				(setq end nil)))
			  (goto-char end))))
		    ;; Where the value nil of `end' means there may be
		    ;; text to have to be encoded following the point.
		    ;; Otherwise, the point reached to the end of ASCII
		    ;; words separated by whitespace or a special char.
		    (unless end
		      (when (looking-at encodable-regexp)
			(goto-char (setq begin (match-end 0)))
			(while (and (looking-at "[ \t\n]+\\([^ \t\n]+\\)")
				    (setq end (match-end 0))
				    (progn
				      (while (re-search-forward
					      encodable-regexp end t))
				      (< begin (point)))
				    (goto-char begin)
				    (or (not (re-search-forward "\\Sw" end t))
					(progn
					  (goto-char (match-beginning 0))
					  nil)))
			  (goto-char end))
			(when (looking-at "[^ \t\n]+")
			  (setq end (match-end 0))
			  (if (re-search-forward "\\Sw+" end t)
			      ;; There are special characters better
			      ;; to be encoded so that MTAs may parse
			      ;; them safely.
			      (cond ((= end (point)))
				    ((looking-at (concat "\\sw*\\("
							 encodable-regexp
							 "\\)"))
				     (setq end nil))
				    (t
				     (goto-char (1- (match-end 0)))
				     (unless (= (point) (match-beginning 0))
				       ;; Separate encodable text and
				       ;; delimiter.
				       (insert " "))))
			    (goto-char end)
			    (skip-chars-forward " \t\n")
			    (if (and (looking-at "[^ \t\n]+")
				     (string-match encodable-regexp
						   (match-string 0)))
				(setq end nil)
			      (goto-char end)))))))
		  (skip-chars-backward " \t\n")
		  (setq end (point))
		  (goto-char start)
		  (if (re-search-forward encodable-regexp end 'move)
		      (progn
			(unless (memq (char-before start) '(nil ?\t ? ))
			  (if (progn
				(goto-char start)
				(skip-chars-backward "^ \t\n")
				(and (looking-at "\\Sw+")
				     (= (match-end 0) start)))
			      ;; Also encode bogus delimiters.
			      (setq start (point))
			    ;; Separate encodable text and delimiter.
			    (goto-char start)
			    (insert " ")
			    (setq start (1+ start)
				  end (1+ end))))
			(rfc2047-encode start end)
			(setq last-encoded t))
		    (setq last-encoded nil)))))
	    (error
	     (if (or debug-on-quit debug-on-error)
		 (signal (car err) (cdr err))
	       (error "Invalid data for rfc2047 encoding: %s"
		      (mm-replace-in-string orig-text "[ \t\n]+" " "))))))))
    (unless dont-fold
      (rfc2047-fold-region b (point)))
    (goto-char (point-max))))

(defun rfc2047-encode-string (string &optional dont-fold)
  "Encode words in STRING.
By default, the string is treated as containing addresses (see
`rfc2047-encoding-type')."
  (mm-with-multibyte-buffer
    (insert string)
    (rfc2047-encode-region (point-min) (point-max) dont-fold)
    (buffer-string)))

;; From RFC 2047:
;; 2. Syntax of encoded-words
;;    [...]
;;    While there is no limit to the length of a multiple-line header
;;    field, each line of a header field that contains one or more
;;    'encoded-word's is limited to 76 characters.
;;
;; In `rfc2047-encode-parameter' it is bound to nil, so don't defconst it.
(defvar rfc2047-encode-max-chars 76
  "Maximum characters of each header line that contain encoded-words.
According to RFC 2047, it is 76.  If it is nil, encoded-words
will not be folded.  Too small value may cause an error.  You
should not change this value.")

(defun rfc2047-encode-1 (column string cs encoder start crest tail
				&optional eword)
  "Subroutine used by `rfc2047-encode'."
  (cond ((string-equal string "")
	 (or eword ""))
	((not rfc2047-encode-max-chars)
	 (concat start
		 (funcall encoder (if cs
				      (mm-encode-coding-string string cs)
				    string))
		 "?="))
	((>= column rfc2047-encode-max-chars)
	 (when eword
	   (cond ((string-match "\n[ \t]+\\'" eword)
		  ;; Remove a superfluous empty line.
		  (setq eword (substring eword 0 (match-beginning 0))))
		 ((string-match "(+\\'" eword)
		  ;; Break the line before the open parenthesis.
		  (setq crest (concat crest (match-string 0 eword))
			eword (substring eword 0 (match-beginning 0))))))
	 (rfc2047-encode-1 (length crest) string cs encoder start " " tail
			   (concat eword "\n" crest)))
	(t
	 (let ((index 0)
	       (limit (1- (length string)))
	       (prev "")
	       next len)
	   (while (and prev
		       (<= index limit))
	     (setq next (concat start
				(funcall encoder
					 (if cs
					     (mm-encode-coding-string
					      (substring string 0 (1+ index))
					      cs)
					   (substring string 0 (1+ index))))
				"?=")
		   len (+ column (length next)))
	     (if (> len rfc2047-encode-max-chars)
		 (setq next prev
		       prev nil)
	       (if (or (< index limit)
		       (<= (+ len (or (string-match "\n" tail)
				      (length tail)))
			   rfc2047-encode-max-chars))
		   (setq prev next
			 index (1+ index))
		 (if (string-match "\\`)+" tail)
		     ;; Break the line after the close parenthesis.
		     (setq tail (concat (substring tail 0 (match-end 0))
					"\n "
					(substring tail (match-end 0)))
			   prev next
			   index (1+ index))
		   (setq next prev
			 prev nil)))))
	   (if (> index limit)
	       (concat eword next tail)
	     (if (= 0 index)
		 (if (and eword
			  (string-match "(+\\'" eword))
		     (setq crest (concat crest (match-string 0 eword))
			   eword (substring eword 0 (match-beginning 0)))
		   (setq eword (concat eword next)))
	       (setq crest " "
		     eword (concat eword next)))
	     (when (string-match "\n[ \t]+\\'" eword)
	       ;; Remove a superfluous empty line.
	       (setq eword (substring eword 0 (match-beginning 0))))
	     (rfc2047-encode-1 (length crest) (substring string index)
			       cs encoder start " " tail
			       (concat eword "\n" crest)))))))

(defun rfc2047-encode (b e)
  "Encode the word(s) in the region B to E.
Point moves to the end of the region."
  (let ((mime-charset (or (mm-find-mime-charset-region b e) (list 'us-ascii)))
	cs encoding tail crest eword)
    ;; Use utf-8 as a last resort if determining charset of text fails.
    (if (memq nil mime-charset)
	(setq mime-charset (list 'utf-8)))
    (cond ((> (length mime-charset) 1)
	   (error "Can't rfc2047-encode `%s'"
		  (buffer-substring-no-properties b e)))
	  ((= (length mime-charset) 1)
	   (setq mime-charset (car mime-charset)
		 cs (mm-charset-to-coding-system mime-charset))
	   (unless (and (mm-multibyte-p)
			(mm-coding-system-p cs))
	     (setq cs nil))
	   (save-restriction
	     (narrow-to-region b e)
	     (setq encoding
		   (or (cdr (assq mime-charset
				  rfc2047-charset-encoding-alist))
		       ;; For the charsets that don't have a preferred
		       ;; encoding, choose the one that's shorter.
		       (if (eq (rfc2047-qp-or-base64) 'base64)
			   'B
			 'Q)))
	     (widen)
	     (goto-char e)
	     (skip-chars-forward "^ \t\n")
	     ;; `tail' may contain a close parenthesis.
	     (setq tail (buffer-substring-no-properties e (point)))
	     (goto-char b)
	     (setq b (point-marker)
		   e (set-marker (make-marker) e))
	     (rfc2047-fold-region (point-at-bol) b)
	     (goto-char b)
	     (skip-chars-backward "^ \t\n")
	     (unless (= 0 (skip-chars-backward " \t"))
	       ;; `crest' may contain whitespace and an open parenthesis.
	       (setq crest (buffer-substring-no-properties (point) b)))
	     (setq eword (rfc2047-encode-1
			  (- b (point-at-bol))
			  (mm-replace-in-string
			   (buffer-substring-no-properties b e)
			   "\n\\([ \t]?\\)" "\\1")
			  cs
			  (or (cdr (assq encoding
					 rfc2047-encode-function-alist))
			      'identity)
			  (concat "=?" (downcase (symbol-name mime-charset))
				  "?" (upcase (symbol-name encoding)) "?")
			  (or crest " ")
			  tail))
	     (delete-region (if (eq (aref eword 0) ?\n)
				(if (bolp)
				    ;; The line was folded before encoding.
				    (1- (point))
				  (point))
			      (goto-char b))
			    (+ e (length tail)))
	     ;; `eword' contains `crest' and `tail'.
	     (insert eword)
	     (set-marker b nil)
	     (set-marker e nil)
	     (unless (or (/= 0 (length tail))
			 (eobp)
			 (looking-at "[ \t\n)]"))
	       (insert " "))))
	  (t
	   (goto-char e)))))

(defun rfc2047-fold-field ()
  "Fold the current header field."
  (save-excursion
    (save-restriction
      (rfc2047-narrow-to-field)
      (rfc2047-fold-region (point-min) (point-max)))))

(defun rfc2047-fold-region (b e)
  "Fold long lines in region B to E."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((break nil)
	  (qword-break nil)
	  (first t)
	  (bol (save-restriction
		 (widen)
		 (point-at-bol))))
      (while (not (eobp))
	(when (and (or break qword-break)
		   (> (- (point) bol) 76))
	  (goto-char (or break qword-break))
	  (setq break nil
		qword-break nil)
	  (skip-chars-backward " \t")
	  (if (looking-at "[ \t]")
	      (insert ?\n)
	    (insert "\n "))
	  (setq bol (1- (point)))
	  ;; Don't break before the first non-LWSP characters.
	  (skip-chars-forward " \t")
	  (unless (eobp)
	    (forward-char 1)))
	(cond
	 ((eq (char-after) ?\n)
	  (forward-char 1)
	  (setq bol (point)
		break nil
		qword-break nil)
	  (skip-chars-forward " \t")
	  (unless (or (eobp) (eq (char-after) ?\n))
	    (forward-char 1)))
	 ((eq (char-after) ?\r)
	  (forward-char 1))
	 ((memq (char-after) '(?  ?\t))
	  (skip-chars-forward " \t")
	  (unless first ;; Don't break just after the header name.
	    (setq break (point))))
	 ((not break)
	  (if (not (looking-at "=\\?[^=]"))
	      (if (eq (char-after) ?=)
		  (forward-char 1)
		(skip-chars-forward "^ \t\n\r="))
	    ;; Don't break at the start of the field.
	    (unless (= (point) b)
	      (setq qword-break (point)))
	    (skip-chars-forward "^ \t\n\r")))
	 (t
	  (skip-chars-forward "^ \t\n\r")))
	(setq first nil))
      (when (and (or break qword-break)
		 (> (- (point) bol) 76))
	(goto-char (or break qword-break))
	(setq break nil
	      qword-break nil)
	(if (or (> 0 (skip-chars-backward " \t"))
		(looking-at "[ \t]"))
	    (insert ?\n)
	  (insert "\n "))
	(setq bol (1- (point)))
	;; Don't break before the first non-LWSP characters.
	(skip-chars-forward " \t")
	(unless (eobp)
	  (forward-char 1))))))

(defun rfc2047-unfold-field ()
  "Fold the current line."
  (save-excursion
    (save-restriction
      (rfc2047-narrow-to-field)
      (rfc2047-unfold-region (point-min) (point-max)))))

(defun rfc2047-unfold-region (b e)
  "Unfold lines in region B to E."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((bol (save-restriction
		 (widen)
		 (point-at-bol)))
	  (eol (point-at-eol)))
      (forward-line 1)
      (while (not (eobp))
	(if (and (looking-at "[ \t]")
		 (< (- (point-at-eol) bol) 76))
	    (delete-region eol (progn
				 (goto-char eol)
				 (skip-chars-forward "\r\n")
				 (point)))
	  (setq bol (point-at-bol)))
	(setq eol (point-at-eol))
	(forward-line 1)))))

(defun rfc2047-b-encode-string (string)
  "Base64-encode the header contained in STRING."
  (base64-encode-string string t))

(autoload 'quoted-printable-encode-region "qp")

(defun rfc2047-q-encode-string (string)
  "Quoted-printable-encode the header in STRING."
  (mm-with-unibyte-buffer
    (insert string)
    (quoted-printable-encode-region
     (point-min) (point-max) nil
     ;; = (\075), _ (\137), ? (\077) are used in the encoded word.
     ;; Avoid using 8bit characters.
     ;; This list excludes `especials' (see the RFC2047 syntax),
     ;; meaning that some characters in non-structured fields will
     ;; get encoded when they con't need to be.  The following is
     ;; what it used to be.
     ;;;  ;; Equivalent to "^\000-\007\011\013\015-\037\200-\377=_?"
     ;;;  "\010\012\014\040-\074\076\100-\136\140-\177")
     "-\b\n\f !#-'*+0-9A-Z\\^`-~\d")
    (subst-char-in-region (point-min) (point-max) ?  ?_)
    (buffer-string)))

(defun rfc2047-encode-parameter (param value)
  "Return and PARAM=VALUE string encoded in the RFC2047-like style.
This is a substitution for the `rfc2231-encode-string' function, that
is the standard but many mailers don't support it."
  (let ((rfc2047-encoding-type 'mime)
	(rfc2047-encode-max-chars nil))
    (rfc2045-encode-string param (rfc2047-encode-string value t))))

;;;
;;; Functions for decoding RFC2047 messages
;;;

(defvar rfc2047-quote-decoded-words-containing-tspecials nil
  "If non-nil, quote decoded words containing special characters.")

(defvar rfc2047-allow-incomplete-encoded-text t
  "*Non-nil means allow incomplete encoded-text in successive encoded-words.
Dividing of encoded-text in the place other than character boundaries
violates RFC2047 section 5, while we have a capability to decode it.
If it is non-nil, the decoder will decode B- or Q-encoding in each
encoded-word, concatenate them, and decode it by charset.  Otherwise,
the decoder will fully decode each encoded-word before concatenating
them.")

(defun rfc2047-strip-backslashes-in-quoted-strings ()
  "Strip backslashes in quoted strings.  `\\\"' remains."
  (goto-char (point-min))
  (let (beg)
    (with-syntax-table (standard-syntax-table)
      (while (search-forward "\"" nil t)
	(unless (eq (char-before) ?\\)
	  (setq beg (match-end 0))
	  (goto-char (match-beginning 0))
	  (condition-case nil
	      (progn
		(forward-sexp)
		(save-restriction
		  (narrow-to-region beg (1- (point)))
		  (goto-char beg)
		  (while (search-forward "\\" nil 'move)
		    (unless (memq (char-after) '(?\"))
		      (delete-char -1))
		    (forward-char)))
		(forward-char))
	    (error
	     (goto-char beg))))))))

(defun rfc2047-charset-to-coding-system (charset &optional allow-override)
  "Return coding-system corresponding to MIME CHARSET.
If your Emacs implementation can't decode CHARSET, return nil.

If allow-override is given, use `mm-charset-override-alist' to
map undesired charset names to their replacement.  This should
only be used for decoding, not for encoding."
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (when (or (not charset)
	    (eq 'gnus-all mail-parse-ignored-charsets)
	    (memq 'gnus-all mail-parse-ignored-charsets)
	    (memq charset mail-parse-ignored-charsets))
    (setq charset mail-parse-charset))
  (let ((cs (mm-charset-to-coding-system charset nil allow-override)))
    (cond ((eq cs 'ascii)
	   (setq cs (or (mm-charset-to-coding-system mail-parse-charset)
			'raw-text)))
	  ((mm-coding-system-p cs))
	  ((and charset
		(listp mail-parse-ignored-charsets)
		(memq 'gnus-unknown mail-parse-ignored-charsets))
	   (setq cs (mm-charset-to-coding-system mail-parse-charset))))
    (if (eq cs 'ascii)
	'raw-text
      cs)))

(autoload 'quoted-printable-decode-string "qp")

(defun rfc2047-decode-encoded-words (words)
  "Decode successive encoded-words in WORDS and return a decoded string.
Each element of WORDS looks like (CHARSET ENCODING ENCODED-TEXT
ENCODED-WORD)."
  (let (word charset cs encoding text rest)
    (while words
      (setq word (pop words))
      (if (and (setq cs (rfc2047-charset-to-coding-system
			 (setq charset (car word)) t))
	       (condition-case code
		   (cond ((char-equal ?B (nth 1 word))
			  (setq text (base64-decode-string
				      (rfc2047-pad-base64 (nth 2 word)))))
			 ((char-equal ?Q (nth 1 word))
			  (setq text (quoted-printable-decode-string
				      (mm-subst-char-in-string
				       ?_ ?  (nth 2 word) t)))))
		 (error
		  (message "%s" (error-message-string code))
		  nil)))
	  (if (and rfc2047-allow-incomplete-encoded-text
		   (eq cs (caar rest)))
	      ;; Concatenate text of which the charset is the same.
	      (setcdr (car rest) (concat (cdar rest) text))
	    (push (cons cs text) rest))
	;; Don't decode encoded-word.
	(push (cons nil (nth 3 word)) rest)))
    (while rest
      (setq words (concat
		   (or (and (setq cs (caar rest))
			    (condition-case code
				(mm-decode-coding-string (cdar rest) cs)
			      (error
			       (message "%s" (error-message-string code))
			       nil)))
		       (concat (when (cdr rest) " ")
			       (cdar rest)
			       (when (and words
					  (not (eq (string-to-char words) ? )))
				 " ")))
		   words)
	    rest (cdr rest)))
    words))

;; Fixme: This should decode in place, not cons intermediate strings.
;; Also check whether it needs to worry about delimiting fields like
;; encoding.

;; In fact it's reported that (invalid) encoding of mailboxes in
;; addr-specs is in use, so delimiting fields might help.  Probably
;; not decoding a word which isn't properly delimited is good enough
;; and worthwhile (is it more correct or not?), e.g. something like
;; `=?iso-8859-1?q?foo?=@'.

(defun rfc2047-decode-region (start end &optional address-mime)
  "Decode MIME-encoded words in region between START and END.
If ADDRESS-MIME is non-nil, strip backslashes which precede characters
other than `\"' and `\\' in quoted strings."
  (interactive "r")
  (let ((case-fold-search t)
	(eword-regexp
	 (if rfc2047-allow-irregular-q-encoded-words
	     (eval-when-compile
	       (concat "[\n\t ]*\\(" rfc2047-encoded-word-regexp-loose "\\)"))
	   (eval-when-compile
	     (concat "[\n\t ]*\\(" rfc2047-encoded-word-regexp "\\)"))))
	b e match words)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(when address-mime
	  (rfc2047-strip-backslashes-in-quoted-strings))
	(goto-char (setq b start))
	;; Look for the encoded-words.
	(while (setq match (re-search-forward eword-regexp nil t))
	  (setq e (match-beginning 1)
		end (match-end 0)
		words nil)
	  (while match
	    (push (list (match-string 2) ;; charset
			(char-after (match-beginning 3)) ;; encoding
			(substring (match-string 3) 2) ;; encoded-text
			(match-string 1)) ;; encoded-word
		  words)
	    ;; Look for the subsequent encoded-words.
	    (when (setq match (looking-at eword-regexp))
	      (goto-char (setq end (match-end 0)))))
	  ;; Replace the encoded-words with the decoded one.
	  (delete-region e end)
	  (insert (rfc2047-decode-encoded-words (nreverse words)))
	  (save-restriction
	    (narrow-to-region e (point))
	    (goto-char e)
	    ;; Remove newlines between decoded words, though such
	    ;; things essentially must not be there.
	    (while (re-search-forward "[\n\r]+" nil t)
	      (replace-match " "))
	    (setq end (point-max))
	    ;; Quote decoded words if there are special characters
	    ;; which might violate RFC2822.
	    (when (and rfc2047-quote-decoded-words-containing-tspecials
		       (let ((regexp (car (rassq
					   'address-mime
					   rfc2047-header-encoding-alist))))
			 (when regexp
			   (save-restriction
			     (widen)
			     (and
			      ;; Don't quote words if already quoted.
			      (not (and (eq (char-before e) ?\")
					(eq (char-after end) ?\")))
			      (progn
				(beginning-of-line)
				(while (and (memq (char-after) '(?  ?\t))
					    (zerop (forward-line -1))))
				(looking-at regexp)))))))
	      (let (quoted)
		(goto-char e)
		(skip-chars-forward " \t")
		(setq start (point))
		(setq quoted (eq (char-after) ?\"))
		(goto-char (point-max))
		(skip-chars-backward " \t" start)
		(if (setq quoted (and quoted
				      (> (point) (1+ start))
				      (eq (char-before) ?\")))
		    (progn
		      (backward-char)
		      (setq start (1+ start)
			    end (point-marker)))
		  (setq end (point-marker)))
		(goto-char start)
		(while (search-forward "\"" end t)
		  (when (prog2
			    (backward-char)
			    (zerop (% (skip-chars-backward "\\\\") 2))
			  (goto-char (match-beginning 0)))
		    (insert "\\"))
		  (forward-char))
		(when (and (not quoted)
			   (progn
			     (goto-char start)
			     (re-search-forward
			      (concat "[" ietf-drums-tspecials "]")
			      end t)))
		  (goto-char start)
		  (insert "\"")
		  (goto-char end)
		  (insert "\""))
		(set-marker end nil)))
	    (goto-char (point-max)))
	  (when (and (mm-multibyte-p)
		     mail-parse-charset
		     (not (eq mail-parse-charset 'us-ascii))
		     (not (eq mail-parse-charset 'gnus-decoded)))
	    (mm-decode-coding-region b e mail-parse-charset))
	  (setq b (point)))
	(when (and (mm-multibyte-p)
		   mail-parse-charset
		   (not (eq mail-parse-charset 'us-ascii))
		   (not (eq mail-parse-charset 'gnus-decoded)))
	  (mm-decode-coding-region b (point-max) mail-parse-charset))))))

(defun rfc2047-decode-address-region (start end)
  "Decode MIME-encoded words in region between START and END.
Backslashes which precede characters other than `\"' and `\\' in quoted
strings are stripped."
  (rfc2047-decode-region start end t))

(defun rfc2047-decode-string (string &optional address-mime)
  "Decode MIME-encoded STRING and return the result.
If ADDRESS-MIME is non-nil, strip backslashes which precede characters
other than `\"' and `\\' in quoted strings."
  ;; (let ((m (mm-multibyte-p)))
    (if (string-match "=\\?" string)
	(with-temp-buffer
          ;; We used to only call mm-enable-multibyte if `m' is non-nil,
          ;; but this can't be the right criterion.  Don't just revert this
          ;; change if it encounters a bug.  Please help me fix it
          ;; right instead.  --Stef
          ;; The string returned should always be multibyte in a multibyte
	  ;; session, i.e. the buffer should be multibyte before
	  ;; `buffer-string' is called.
          (mm-enable-multibyte)
	  (insert string)
	  (inline
	    (rfc2047-decode-region (point-min) (point-max) address-mime))
	  (buffer-string))
      (when address-mime
	(setq string
	      (with-temp-buffer
		(when (mm-multibyte-string-p string)
		  (mm-enable-multibyte))
		(insert string)
		(rfc2047-strip-backslashes-in-quoted-strings)
		(buffer-string))))
      ;; Fixme: As above, `m' here is inappropriate.
      (if (and ;; m
	       mail-parse-charset
	       (not (eq mail-parse-charset 'us-ascii))
	       (not (eq mail-parse-charset 'gnus-decoded)))
	  ;; `decode-coding-string' in Emacs offers a third optional
	  ;; arg NOCOPY to avoid consing a new string if the decoding
	  ;; is "trivial".  Unfortunately it currently doesn't
	  ;; consider anything else than a `nil' coding system
	  ;; trivial.
	  ;; `rfc2047-decode-string' is called multiple times for each
	  ;; article during summary buffer generation, and we really
	  ;; want to avoid unnecessary consing.  So we bypass
	  ;; `decode-coding-string' if the string is purely ASCII.
	  (if (and (fboundp 'detect-coding-string)
		   ;; string is purely ASCII
		   (eq (detect-coding-string string t) 'undecided))
              string
            (mm-decode-coding-string string mail-parse-charset))
        (mm-string-to-multibyte string)))) ;; )

(defun rfc2047-decode-address-string (string)
  "Decode MIME-encoded STRING and return the result.
Backslashes which precede characters other than `\"' and `\\' in quoted
strings are stripped."
  (rfc2047-decode-string string t))

(defun rfc2047-pad-base64 (string)
  "Pad STRING to quartets."
  ;; Be more liberal to accept buggy base64 strings. If
  ;; base64-decode-string accepts buggy strings, this function could
  ;; be aliased to identity.
  (if (= 0 (mod (length string) 4))
      string
    (when (string-match "=+$" string)
      (setq string (substring string 0 (match-beginning 0))))
    (case (mod (length string) 4)
      (0 string)
      (1 string) ;; Error, don't pad it.
      (2 (concat string "=="))
      (3 (concat string "=")))))

(provide 'rfc2047)

;;; rfc2047.el ends here
