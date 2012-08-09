;;; footnote.el --- footnote support for message mode  -*- coding: utf-8;-*-

;; Copyright (C) 1997, 2000-2012 Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@xemacs.org>
;; Keywords: mail, news
;; Version: 0.19

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

;; This file provides footnote[1] support for message-mode in emacsen.
;; footnote-mode is implemented as a minor mode.

;; [1] Footnotes look something like this.  Along with some decorative
;; stuff.

;; TODO:
;; Reasonable Undo support.
;; more language styles.

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar filladapt-token-table))

(defgroup footnote nil
  "Support for footnotes in mail and news messages."
  :version "21.1"
  :group 'message)

(defcustom footnote-mode-line-string " FN"
  "String to display in modes section of the mode-line."
  :group 'footnote)

(defcustom footnote-mode-hook nil
  "Hook functions run when footnote-mode is activated."
  :type 'hook
  :group 'footnote)

(defcustom footnote-narrow-to-footnotes-when-editing nil
  "If non-nil, narrow to footnote text body while editing a footnote."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-prompt-before-deletion t
  "If non-nil, prompt before deleting a footnote.
There is currently no way to undo deletions."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-spaced-footnotes t
  "If non-nil, insert an empty line between footnotes.
Customizing this variable has no effect on buffers already
displaying footnotes."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-use-message-mode t ; Nowhere used.
  "If non-nil, assume Footnoting will be done in `message-mode'."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-body-tag-spacing 2
  "Number of spaces separating a footnote body tag and its text.
Customizing this variable has no effect on buffers already
displaying footnotes."
  :type 'integer
  :group 'footnote)

(defcustom footnote-prefix [(control ?c) ?!]
  "Prefix key to use for Footnote command in Footnote minor mode.
The value of this variable is checked as part of loading Footnote mode.
After that, changing the prefix key requires manipulating keymaps."
  ;; FIXME: the type should be a key-sequence, but it seems Custom
  ;; doesn't support that yet.
  ;; :type  'string
  )

;;; Interface variables that probably shouldn't be changed

(defcustom footnote-section-tag "Footnotes: "
  "Tag inserted at beginning of footnote section.
If you set this to the empty string, no tag is inserted and the
value of `footnote-section-tag-regexp' is ignored.  Customizing
this variable has no effect on buffers already displaying
footnotes."
  :type 'string
  :group 'footnote)

(defcustom footnote-section-tag-regexp "Footnotes\\(\\[.\\]\\)?: "
  "Regexp which indicates the start of a footnote section.
This variable is disregarded when `footnote-section-tag' is the
empty string.  Customizing this variable has no effect on buffers
already displaying footnotes."
  :type 'regexp
  :group 'footnote)

;; The following three should be consumed by footnote styles.
(defcustom footnote-start-tag "["
  "String used to denote start of numbered footnote.
Should not be set to the empty string.  Customizing this variable
has no effect on buffers already displaying footnotes."
  :type 'string
  :group 'footnote)

(defcustom footnote-end-tag "]"
  "String used to denote end of numbered footnote.
Should not be set to the empty string.  Customizing this variable
has no effect on buffers already displaying footnotes."
  :type 'string
  :group 'footnote)

(defvar footnote-signature-separator (if (boundp 'message-signature-separator)
					 message-signature-separator
				       "^-- $")
  "*String used to recognize .signatures.")

;;; Private variables

(defvar footnote-style-number nil
  "Footnote style represented as an index into footnote-style-alist.")
(make-variable-buffer-local 'footnote-style-number)

(defvar footnote-text-marker-alist nil
  "List of markers pointing to text of footnotes in message buffer.")
(make-variable-buffer-local 'footnote-text-marker-alist)

(defvar footnote-pointer-marker-alist nil
  "List of markers pointing to footnote pointers in message buffer.")
(make-variable-buffer-local 'footnote-pointer-marker-alist)

(defvar footnote-mouse-highlight 'highlight
  "Text property name to enable mouse over highlight.")

;;; Default styles
;;; NUMERIC
(defconst footnote-numeric-regexp "[0-9]+"
  "Regexp for digits.")

(defun Footnote-numeric (n)
  "Numeric footnote style.
Use Arabic numerals for footnoting."
  (int-to-string n))

;;; ENGLISH UPPER
(defconst footnote-english-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Upper case English alphabet.")

(defconst footnote-english-upper-regexp "[A-Z]+"
  "Regexp for upper case English alphabet.")

(defun Footnote-english-upper (n)
  "Upper case English footnoting.
Wrapping around the alphabet implies successive repetitions of letters."
  (let* ((ltr (mod (1- n) (length footnote-english-upper)))
	 (rep (/ (1- n) (length footnote-english-upper)))
	 (chr (char-to-string (aref footnote-english-upper ltr)))
	 rc)
    (while (>= rep 0)
      (setq rc (concat rc chr))
      (setq rep (1- rep)))
    rc))

;;; ENGLISH LOWER
(defconst footnote-english-lower "abcdefghijklmnopqrstuvwxyz"
  "Lower case English alphabet.")

(defconst footnote-english-lower-regexp "[a-z]+"
  "Regexp of lower case English alphabet.")

(defun Footnote-english-lower (n)
  "Lower case English footnoting.
Wrapping around the alphabet implies successive repetitions of letters."
  (let* ((ltr (mod (1- n) (length footnote-english-lower)))
	 (rep (/ (1- n) (length footnote-english-lower)))
	 (chr (char-to-string (aref footnote-english-lower ltr)))
	 rc)
    (while (>= rep 0)
      (setq rc (concat rc chr))
      (setq rep (1- rep)))
    rc))

;;; ROMAN LOWER
(defconst footnote-roman-lower-list
  '((1 . "i") (5 . "v") (10 . "x")
    (50 . "l") (100 . "c") (500 . "d") (1000 . "m"))
  "List of roman numerals with their values.")

(defconst footnote-roman-lower-regexp "[ivxlcdm]+"
  "Regexp of roman numerals.")

(defun Footnote-roman-lower (n)
  "Generic Roman number footnoting."
  (Footnote-roman-common n footnote-roman-lower-list))

;;; ROMAN UPPER
(defconst footnote-roman-upper-list
  '((1 . "I") (5 . "V") (10 . "X")
    (50 . "L") (100 . "C") (500 . "D") (1000 . "M"))
  "List of roman numerals with their values.")

(defconst footnote-roman-upper-regexp "[IVXLCDM]+"
  "Regexp of roman numerals.  Not complete")

(defun Footnote-roman-upper (n)
  "Generic Roman number footnoting."
  (Footnote-roman-common n footnote-roman-upper-list))

(defun Footnote-roman-common (n footnote-roman-list)
  "Lower case Roman footnoting."
  (let* ((our-list footnote-roman-list)
	 (rom-lngth (length our-list))
	 (rom-high 0)
	 (rom-low 0)
	 (rom-div -1)
	 (count-high 0)
	 (count-low 0))
    ;; find surrounding numbers
    (while (and (<= count-high (1- rom-lngth))
		(>= n (car (nth count-high our-list))))
      ;; (message "Checking %d" (car (nth count-high our-list)))
      (setq count-high (1+ count-high)))
    (setq rom-high count-high)
    (setq rom-low (1- count-high))
    ;; find the appropriate divisor (if it exists)
    (while (and (= rom-div -1)
		(< count-low rom-high))
      (when (or (> n (- (car (nth rom-high our-list))
			(/ (car (nth count-low our-list))
			   2)))
		(= n (- (car (nth rom-high our-list))
			(car (nth count-low our-list)))))
	(setq rom-div count-low))
      ;; (message "Checking %d and %d in div loop" rom-high count-low)
      (setq count-low (1+ count-low)))
    ;;(message "We now have high: %d, low: %d, div: %d, n: %d"
    ;;	       rom-high rom-low (if rom-div rom-div -1) n)
    (let ((rom-low-pair (nth rom-low our-list))
	  (rom-high-pair (nth rom-high our-list))
	  (rom-div-pair (if (not (= rom-div -1)) (nth rom-div our-list) nil)))
      ;; (message "pairs are: rom-low: %S, rom-high: %S, rom-div: %S"
      ;;	  rom-low-pair rom-high-pair rom-div-pair)
      (cond
       ((< n 0) (error "Footnote-roman-common called with n < 0"))
       ((= n 0) "")
       ((= n (car rom-low-pair)) (cdr rom-low-pair))
       ((= n (car rom-high-pair)) (cdr rom-high-pair))
       ((= (car rom-low-pair) (car rom-high-pair))
	(concat (cdr rom-low-pair)
		(Footnote-roman-common
		 (- n (car rom-low-pair))
		 footnote-roman-list)))
       ((>= rom-div 0) (concat (cdr rom-div-pair) (cdr rom-high-pair)
			       (Footnote-roman-common
				(- n (- (car rom-high-pair)
					(car rom-div-pair)))
				footnote-roman-list)))
       (t (concat (cdr rom-low-pair)
		  (Footnote-roman-common
		   (- n (car rom-low-pair))
		   footnote-roman-list)))))))

;; Latin-1

(defconst footnote-latin-string "¹²³ºª§¶"
  "String of Latin-1 footnoting characters.")

;; Note not [...]+, because this style cycles.
(defconst footnote-latin-regexp (concat "[" footnote-latin-string "]")
  "Regexp for Latin-1 footnoting characters.")

(defun Footnote-latin (n)
  "Latin-1 footnote style.
Use a range of Latin-1 non-ASCII characters for footnoting."
  (string (aref footnote-latin-string
		(mod (1- n) (length footnote-latin-string)))))

;; Unicode

(defconst footnote-unicode-string "⁰¹²³⁴⁵⁶⁷⁸⁹"
  "String of Unicode footnoting characters.")

(defconst footnote-unicode-regexp (concat "[" footnote-unicode-string "]+")
  "Regexp for Unicode footnoting characters.")

(defun Footnote-unicode (n)
  "Unicode footnote style.
Use Unicode characters for footnoting."
  (let (modulus result done)
    (while (not done)
      (setq modulus (mod n 10)
            n (truncate n 10))
      (and (zerop n) (setq done t))
      (push (aref footnote-unicode-string modulus) result))
    (apply #'string result)))

;;; list of all footnote styles
(defvar footnote-style-alist
  `((numeric Footnote-numeric ,footnote-numeric-regexp)
    (english-lower Footnote-english-lower ,footnote-english-lower-regexp)
    (english-upper Footnote-english-upper ,footnote-english-upper-regexp)
    (roman-lower Footnote-roman-lower ,footnote-roman-lower-regexp)
    (roman-upper Footnote-roman-upper ,footnote-roman-upper-regexp)
    (latin Footnote-latin ,footnote-latin-regexp)
    (unicode Footnote-unicode ,footnote-unicode-regexp))
  "Styles of footnote tags available.
By default only boring Arabic numbers, English letters and Roman Numerals
are available.
See footnote-han.el, footnote-greek.el and footnote-hebrew.el for more
exciting styles.")

(defcustom footnote-style 'numeric
  "Default style used for footnoting.
numeric == 1, 2, 3, ...
english-lower == a, b, c, ...
english-upper == A, B, C, ...
roman-lower == i, ii, iii, iv, v, ...
roman-upper == I, II, III, IV, V, ...
latin == ¹ ² ³ º ª § ¶
unicode == ¹, ², ³, ...
See also variables `footnote-start-tag' and `footnote-end-tag'.

Note: some characters in the unicode style may not show up
properly if the default font does not contain those characters.

Customizing this variable has no effect on buffers already
displaying footnotes.  To change the style of footnotes in such a
buffer use the command `Footnote-set-style'."
  :type (cons 'choice (mapcar (lambda (x) (list 'const (car x)))
			      footnote-style-alist))
  :group 'footnote)

;;; Style utilities & functions
(defun Footnote-style-p (style)
  "Return non-nil if style is a valid style known to `footnote-mode'."
  (assq style footnote-style-alist))

(defun Footnote-index-to-string (index)
  "Convert a binary index into a string to display as a footnote.
Conversion is done based upon the current selected style."
  (let ((alist (if (Footnote-style-p footnote-style)
		   (assq footnote-style footnote-style-alist)
		 (nth 0 footnote-style-alist))))
    (funcall (nth 1 alist) index)))

(defun Footnote-current-regexp ()
  "Return the regexp of the index of the current style."
  (concat (nth 2 (or (assq footnote-style footnote-style-alist)
		     (nth 0 footnote-style-alist)))
	  "*"))

(defun Footnote-refresh-footnotes (&optional index-regexp)
  "Redraw all footnotes.
You must call this or arrange to have this called after changing footnote
styles."
  (unless index-regexp
    (setq index-regexp (Footnote-current-regexp)))
  (save-excursion
    ;; Take care of the pointers first
    (let ((i 0) locn alist)
      (while (setq alist (nth i footnote-pointer-marker-alist))
	(setq locn (cdr alist))
	(while locn
	  (goto-char (car locn))
	  ;; Try to handle the case where `footnote-start-tag' and
	  ;; `footnote-end-tag' are the same string.
	  (when (looking-back (concat
			       (regexp-quote footnote-start-tag)
			       "\\(" index-regexp "+\\)"
			       (regexp-quote footnote-end-tag))
			      (line-beginning-position))
	    (replace-match
	     (propertize
	      (concat
	       footnote-start-tag
	       (Footnote-index-to-string (1+ i))
	       footnote-end-tag)
	      'footnote-number (1+ i) footnote-mouse-highlight t)
	     nil "\\1"))
	  (setq locn (cdr locn)))
	(setq i (1+ i))))

    ;; Now take care of the text section
    (let ((i 0) alist)
      (while (setq alist (nth i footnote-text-marker-alist))
	(goto-char (cdr alist))
	(when (looking-at (concat
			   (regexp-quote footnote-start-tag)
			   "\\(" index-regexp "+\\)"
			   (regexp-quote footnote-end-tag)))
	  (replace-match
	   (propertize
	    (concat
	     footnote-start-tag
	     (Footnote-index-to-string (1+ i))
	     footnote-end-tag)
	    'footnote-number (1+ i))
	   nil "\\1"))
	(setq i (1+ i))))))

(defun Footnote-assoc-index (key alist)
  "Give index of key in alist."
  (let ((i 0) (max (length alist)) rc)
    (while (and (null rc)
		(< i max))
      (when (eq key (car (nth i alist)))
	(setq rc i))
      (setq i (1+ i)))
    rc))

(defun Footnote-cycle-style ()
  "Select next defined footnote style."
  (interactive)
  (let ((old (Footnote-assoc-index footnote-style footnote-style-alist))
	(max (length footnote-style-alist))
	idx)
    (setq idx (1+ old))
    (when (>= idx max)
      (setq idx 0))
    (setq footnote-style (car (nth idx footnote-style-alist)))
    (Footnote-refresh-footnotes (nth 2 (nth old footnote-style-alist)))))

(defun Footnote-set-style (&optional style)
  "Select a specific style."
  (interactive
   (list (intern (completing-read
		  "Footnote Style: "
		  obarray #'Footnote-style-p 'require-match))))
  (let ((old (Footnote-assoc-index footnote-style footnote-style-alist)))
    (setq footnote-style style)
    (Footnote-refresh-footnotes (nth 2 (nth old footnote-style-alist)))))

;; Internal functions
(defun Footnote-insert-numbered-footnote (arg &optional mousable)
  "Insert numbered footnote at (point)."
  (let ((string (concat footnote-start-tag
			(Footnote-index-to-string arg)
			footnote-end-tag)))
    (insert-before-markers
     (if mousable
	 (propertize
	  string 'footnote-number arg footnote-mouse-highlight t)
       (propertize string 'footnote-number arg)))))

(defun Footnote-renumber (from to pointer-alist text-alist)
  "Renumber a single footnote."
  (let* ((posn-list (cdr pointer-alist)))
    (setcar pointer-alist to)
    (setcar text-alist to)
    (while posn-list
      (goto-char (car posn-list))
      (when (looking-back (concat (regexp-quote footnote-start-tag)
				  (Footnote-current-regexp)
				  (regexp-quote footnote-end-tag))
			  (line-beginning-position))
	(replace-match
	 (propertize
	  (concat footnote-start-tag
		  (Footnote-index-to-string to)
		  footnote-end-tag)
	  'footnote-number to footnote-mouse-highlight t)))
      (setq posn-list (cdr posn-list)))
    (goto-char (cdr text-alist))
    (when (looking-at (concat (regexp-quote footnote-start-tag)
			      (Footnote-current-regexp)
			      (regexp-quote footnote-end-tag)))
      (replace-match
       (propertize
	(concat footnote-start-tag
		(Footnote-index-to-string to)
		footnote-end-tag)
	'footnote-number to)))))

;; Not needed?
(defun Footnote-narrow-to-footnotes ()
  "Restrict text in buffer to show only text of footnotes."
  (interactive)	; testing
  (goto-char (point-max))
  (when (re-search-backward footnote-signature-separator nil t)
    (let ((end (point)))
      (cond
       ((and (not (string-equal footnote-section-tag ""))
	     (re-search-backward
	      (concat "^" footnote-section-tag-regexp) nil t))
	(narrow-to-region (point) end))
       (footnote-text-marker-alist
	(narrow-to-region (cdar footnote-text-marker-alist) end))))))

(defun Footnote-goto-char-point-max ()
  "Move to end of buffer or prior to start of .signature."
  (goto-char (point-max))
  (or (re-search-backward footnote-signature-separator nil t)
      (point)))

(defun Footnote-insert-text-marker (arg locn)
  "Insert a marker pointing to footnote ARG, at buffer location LOCN."
  (let ((marker (make-marker)))
    (unless (assq arg footnote-text-marker-alist)
      (set-marker marker locn)
      (setq footnote-text-marker-alist
	    (cons (cons arg marker) footnote-text-marker-alist))
      (setq footnote-text-marker-alist
	    (Footnote-sort footnote-text-marker-alist)))))

(defun Footnote-insert-pointer-marker (arg locn)
  "Insert a marker pointing to footnote ARG, at buffer location LOCN."
  (let ((marker (make-marker))
	alist)
    (set-marker marker locn)
    (if (setq alist (assq arg footnote-pointer-marker-alist))
	(setf alist
	      (cons marker (cdr alist)))
      (setq footnote-pointer-marker-alist
	    (cons (cons arg (list marker)) footnote-pointer-marker-alist))
      (setq footnote-pointer-marker-alist
	    (Footnote-sort footnote-pointer-marker-alist)))))

(defun Footnote-insert-footnote (arg)
  "Insert a footnote numbered ARG, at (point)."
  (push-mark)
  (Footnote-insert-pointer-marker arg (point))
  (Footnote-insert-numbered-footnote arg t)
  (Footnote-goto-char-point-max)
  (if (cond
       ((not (string-equal footnote-section-tag ""))
	(re-search-backward (concat "^" footnote-section-tag-regexp) nil t))
       (footnote-text-marker-alist
	(goto-char (cdar footnote-text-marker-alist))))
      (save-restriction
	(when footnote-narrow-to-footnotes-when-editing
	  (Footnote-narrow-to-footnotes))
	(Footnote-goto-footnote (1- arg)) ; evil, FIXME (less evil now)
	;; (message "Inserting footnote %d" arg)
	(unless
	    (or (eq arg 1)
		(when (re-search-forward
		       (if footnote-spaced-footnotes
			   "\n\n"
			 (concat "\n"
				 (regexp-quote footnote-start-tag)
				 (Footnote-current-regexp)
				 (regexp-quote footnote-end-tag)))
		       nil t)
		  (unless (beginning-of-line) t))
		(Footnote-goto-char-point-max)
		(cond
		 ((not (string-equal footnote-section-tag ""))
		  (re-search-backward
		   (concat "^" footnote-section-tag-regexp) nil t))
		 (footnote-text-marker-alist
		  (goto-char (cdar footnote-text-marker-alist)))))))
    (unless (looking-at "^$")
      (insert "\n"))
    (when (eobp)
      (insert "\n"))
    (unless (string-equal footnote-section-tag "")
      (insert footnote-section-tag "\n")))
  (let ((old-point (point)))
    (Footnote-insert-numbered-footnote arg nil)
    (Footnote-insert-text-marker arg old-point)))

(defun Footnote-sort (list)
  (sort list (lambda (e1 e2)
	       (< (car e1) (car e2)))))

(defun Footnote-text-under-cursor ()
  "Return the number of footnote if in footnote text.
Return nil if the cursor is not positioned over the text of
a footnote."
  (when (and (let ((old-point (point)))
	       (save-excursion
		 (save-restriction
		   (Footnote-narrow-to-footnotes)
		   (and (>= old-point (point-min))
			(<= old-point (point-max))))))
	     footnote-text-marker-alist
             (>= (point) (cdar footnote-text-marker-alist)))
    (let ((i 1)
	  alist-txt rc)
      (while (and (setq alist-txt (nth i footnote-text-marker-alist))
		  (null rc))
	(when (< (point) (cdr alist-txt))
	  (setq rc (car (nth (1- i) footnote-text-marker-alist))))
	(setq i (1+ i)))
      (when (and (null rc)
		 (null alist-txt))
	(setq rc (car (nth (1- i) footnote-text-marker-alist))))
      rc)))

(defun Footnote-under-cursor ()
  "Return the number of the footnote underneath the cursor.
Return nil if the cursor is not over a footnote."
  (or (get-text-property (point) 'footnote-number)
      (Footnote-text-under-cursor)))

;;; User functions

(defun Footnote-make-hole ()
  (save-excursion
    (let ((i 0)
	  (notes (length footnote-pointer-marker-alist))
	  alist-ptr alist-txt rc)
      (while (< i notes)
	(setq alist-ptr (nth i footnote-pointer-marker-alist))
	(setq alist-txt (nth i footnote-text-marker-alist))
	(when (< (point) (- (cadr alist-ptr) 3))
	  (unless rc
	    (setq rc (car alist-ptr)))
	  (save-excursion
	    (message "Renumbering from %s to %s"
		     (Footnote-index-to-string (car alist-ptr))
		     (Footnote-index-to-string
		      (1+ (car alist-ptr))))
	    (Footnote-renumber (car alist-ptr)
			       (1+ (car alist-ptr))
			       alist-ptr
			       alist-txt)))
	(setq i (1+ i)))
      rc)))

(defun Footnote-add-footnote (&optional arg)
  "Add a numbered footnote.
The number the footnote receives is dependent upon the relative location
of any other previously existing footnotes.
If the variable `footnote-narrow-to-footnotes-when-editing' is set,
the buffer is narrowed to the footnote body.  The restriction is removed
by using `Footnote-back-to-message'."
  (interactive "*P")
  (let (num)
    (if footnote-text-marker-alist
	(if (< (point) (cadar (last footnote-pointer-marker-alist)))
	    (setq num (Footnote-make-hole))
	  (setq num (1+ (caar (last footnote-text-marker-alist)))))
      (setq num 1))
    (message "Adding footnote %d" num)
    (Footnote-insert-footnote num)
    (insert-before-markers (make-string footnote-body-tag-spacing ? ))
    (let ((opoint (point)))
      (save-excursion
	(insert-before-markers
	 (if footnote-spaced-footnotes
	     "\n\n"
	   "\n"))
	(when footnote-narrow-to-footnotes-when-editing
	  (Footnote-narrow-to-footnotes)))
      ;; Emacs/XEmacs bug?  save-excursion doesn't restore point when using
      ;; insert-before-markers.
      (goto-char opoint))))

(defun Footnote-delete-footnote (&optional arg)
  "Delete a numbered footnote.
With no parameter, delete the footnote under (point).  With ARG specified,
delete the footnote with that number."
  (interactive "*P")
  (unless arg
    (setq arg (Footnote-under-cursor)))
  (when (and arg
	     (or (not footnote-prompt-before-deletion)
		 (y-or-n-p (format "Really delete footnote %d?" arg))))
    (let (alist-ptr alist-txt locn)
      (setq alist-ptr (assq arg footnote-pointer-marker-alist))
      (setq alist-txt (assq arg footnote-text-marker-alist))
      (unless (and alist-ptr alist-txt)
	(error "Can't delete footnote %d" arg))
      (setq locn (cdr alist-ptr))
      (while (car locn)
	(save-excursion
	  (goto-char (car locn))
	  (when (looking-back (concat (regexp-quote footnote-start-tag)
				      (Footnote-current-regexp)
				      (regexp-quote footnote-end-tag))
			      (line-beginning-position))
	    (delete-region (match-beginning 0) (match-end 0))))
	(setq locn (cdr locn)))
      (save-excursion
	(goto-char (cdr alist-txt))
	(delete-region
	 (point)
	 (if footnote-spaced-footnotes
	     (search-forward "\n\n" nil t)
	   (save-restriction
	     (end-of-line)
	     (next-single-char-property-change
	      (point) 'footnote-number nil (Footnote-goto-char-point-max))))))
      (setq footnote-pointer-marker-alist
	    (delq alist-ptr footnote-pointer-marker-alist))
      (setq footnote-text-marker-alist
	    (delq alist-txt footnote-text-marker-alist))
      (Footnote-renumber-footnotes)
      (when (and (null footnote-text-marker-alist)
		 (null footnote-pointer-marker-alist))
	(save-excursion
	  (if (not (string-equal footnote-section-tag ""))
	      (let* ((end (Footnote-goto-char-point-max))
		     (start (1- (re-search-backward
				 (concat "^" footnote-section-tag-regexp)
				 nil t))))
		(forward-line -1)
		(when (looking-at "\n")
		  (kill-line))
		(delete-region start (if (< end (point-max))
					 end
				       (point-max))))
	    (Footnote-goto-char-point-max)
	    (when (looking-back "\n\n")
	      (kill-line -1))))))))

(defun Footnote-renumber-footnotes (&optional arg)
  "Renumber footnotes, starting from 1."
  (interactive "*P")
  (save-excursion
    (let ((i 0)
	  (notes (length footnote-pointer-marker-alist))
	  alist-ptr alist-txt)
      (while (< i notes)
	(setq alist-ptr (nth i footnote-pointer-marker-alist))
	(setq alist-txt (nth i footnote-text-marker-alist))
	(unless (= (1+ i) (car alist-ptr))
	  (Footnote-renumber (car alist-ptr) (1+ i) alist-ptr alist-txt))
	(setq i (1+ i))))))

(defun Footnote-goto-footnote (&optional arg)
  "Jump to the text of a footnote.
With no parameter, jump to the text of the footnote under (point).  With ARG
specified, jump to the text of that footnote."
  (interactive "P")
  (unless arg
    (setq arg (Footnote-under-cursor)))
  (let ((footnote (assq arg footnote-text-marker-alist)))
    (cond
     (footnote
      (goto-char (cdr footnote)))
     ((eq arg 0)
      (goto-char (point-max))
      (cond
       ((not (string-equal footnote-section-tag ""))
	(re-search-backward (concat "^" footnote-section-tag-regexp))
	(forward-line 1))
       (footnote-text-marker-alist
	(goto-char (cdar footnote-text-marker-alist)))))
     (t
      (error "I don't see a footnote here")))))

(defun Footnote-back-to-message (&optional arg)
  "Move cursor back to footnote referent.
If the cursor is not over the text of a footnote, point is not changed.
If the buffer was narrowed due to `footnote-narrow-to-footnotes-when-editing'
being set it is automatically widened."
  (interactive "P")
  (let ((note (Footnote-text-under-cursor)))
    (when note
      (when footnote-narrow-to-footnotes-when-editing
	(widen))
      (goto-char (cadr (assq note footnote-pointer-marker-alist))))))

(defvar footnote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'Footnote-add-footnote)
    (define-key map "b" 'Footnote-back-to-message)
    (define-key map "c" 'Footnote-cycle-style)
    (define-key map "d" 'Footnote-delete-footnote)
    (define-key map "g" 'Footnote-goto-footnote)
    (define-key map "r" 'Footnote-renumber-footnotes)
    (define-key map "s" 'Footnote-set-style)
    map))

(defvar footnote-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map footnote-prefix footnote-mode-map)
    map)
  "Keymap used for binding footnote minor mode.")

;;;###autoload
(define-minor-mode footnote-mode
  "Toggle Footnote mode.
With a prefix argument ARG, enable Footnote mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Footnode mode is a buffer-local minor mode.  If enabled, it
provides footnote support for `message-mode'.  To get started,
play around with the following keys:
\\{footnote-minor-mode-map}"
  :lighter    footnote-mode-line-string
  :keymap     footnote-minor-mode-map
  ;; (filladapt-mode t)
  (when footnote-mode
    ;; (Footnote-setup-keybindings)
    (make-local-variable 'footnote-style)
    (make-local-variable 'footnote-body-tag-spacing)
    (make-local-variable 'footnote-spaced-footnotes)
    (make-local-variable 'footnote-section-tag)
    (make-local-variable 'footnote-section-tag-regexp)
    (make-local-variable 'footnote-start-tag)
    (make-local-variable 'footnote-end-tag)

    (when (boundp 'filladapt-token-table)
      ;; add tokens to filladapt to match footnotes
      ;; 1] xxxxxxxxxxx x x x or [1] x x x x x x x
      ;;    xxx x xx xxx xxxx	     x x x xxxxxxxxxx
      (let ((bullet-regexp (concat (regexp-quote footnote-start-tag)
				   "?[0-9a-zA-Z]+"
				   (regexp-quote footnote-end-tag)
				   "[ \t]")))
	(unless (assoc bullet-regexp filladapt-token-table)
	  (setq filladapt-token-table
		(append filladapt-token-table
			(list (list bullet-regexp 'bullet)))))))))

(provide 'footnote)

;;; footnote.el ends here
