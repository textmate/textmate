;;; json.el --- JavaScript Object Notation parser / generator

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Version: 1.3
;; Keywords: convenience

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

;; This is a library for parsing and generating JSON (JavaScript Object
;; Notation).

;; Learn all about JSON here: <URL:http://json.org/>.

;; The user-serviceable entry points for the parser are the functions
;; `json-read' and `json-read-from-string'. The encoder has a single
;; entry point, `json-encode'.

;; Since there are several natural representations of key-value pair
;; mappings in elisp (alist, plist, hash-table), `json-read' allows you
;; to specify which you'd prefer (see `json-object-type' and
;; `json-array-type').

;; Similarly, since `false' and `null' are distinct in JSON, you can
;; distinguish them by binding `json-false' and `json-null' as desired.

;;; History:

;; 2006-03-11 - Initial version.
;; 2006-03-13 - Added JSON generation in addition to parsing. Various
;;              other cleanups, bugfixes, and improvements.
;; 2006-12-29 - XEmacs support, from Aidan Kehoe <kehoea@parhasard.net>.
;; 2008-02-21 - Installed in GNU Emacs.
;; 2011-10-17 - Patch `json-alist-p' and `json-plist-p' to avoid recursion -tzz

;;; Code:

(eval-when-compile (require 'cl))

;; Compatibility code

(defalias 'json-encode-char0 'encode-char)
(defalias 'json-decode-char0 'decode-char)


;; Parameters

(defvar json-object-type 'alist
  "Type to convert JSON objects to.
Must be one of `alist', `plist', or `hash-table'.  Consider let-binding
this around your call to `json-read' instead of `setq'ing it.")

(defvar json-array-type 'vector
  "Type to convert JSON arrays to.
Must be one of `vector' or `list'.  Consider let-binding this around
your call to `json-read' instead of `setq'ing it.")

(defvar json-key-type nil
  "Type to convert JSON keys to.
Must be one of `string', `symbol', `keyword', or nil.

If nil, `json-read' will guess the type based on the value of
`json-object-type':

    If `json-object-type' is:   nil will be interpreted as:
      `hash-table'                `string'
      `alist'                     `symbol'
      `plist'                     `keyword'

Note that values other than `string' might behave strangely for
Sufficiently Weird keys.  Consider let-binding this around your call to
`json-read' instead of `setq'ing it.")

(defvar json-false :json-false
  "Value to use when reading JSON `false'.
If this has the same value as `json-null', you might not be able to tell
the difference between `false' and `null'.  Consider let-binding this
around your call to `json-read' instead of `setq'ing it.")

(defvar json-null nil
  "Value to use when reading JSON `null'.
If this has the same value as `json-false', you might not be able to
tell the difference between `false' and `null'.  Consider let-binding
this around your call to `json-read' instead of `setq'ing it.")



;;; Utilities

(defun json-join (strings separator)
  "Join STRINGS with SEPARATOR."
  (mapconcat 'identity strings separator))

(defun json-alist-p (list)
  "Non-null if and only if LIST is an alist."
  (while (consp list)
    (setq list (if (consp (car list))
                   (cdr list)
                 'not-alist)))
  (null list))

(defun json-plist-p (list)
  "Non-null if and only if LIST is a plist."
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))

;; Reader utilities

(defsubst json-advance (&optional n)
  "Skip past the following N characters."
  (forward-char n))

(defsubst json-peek ()
  "Return the character at point."
  (let ((char (char-after (point))))
    (or char :json-eof)))

(defsubst json-pop ()
  "Advance past the character at point, returning it."
  (let ((char (json-peek)))
    (if (eq char :json-eof)
        (signal 'end-of-file nil)
      (json-advance)
      char)))

(defun json-skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "\t\r\n\f\b "))



;; Error conditions

(put 'json-error 'error-message "Unknown JSON error")
(put 'json-error 'error-conditions '(json-error error))

(put 'json-readtable-error 'error-message "JSON readtable error")
(put 'json-readtable-error 'error-conditions
     '(json-readtable-error json-error error))

(put 'json-unknown-keyword 'error-message "Unrecognized keyword")
(put 'json-unknown-keyword 'error-conditions
     '(json-unknown-keyword json-error error))

(put 'json-number-format 'error-message "Invalid number format")
(put 'json-number-format 'error-conditions
     '(json-number-format json-error error))

(put 'json-string-escape 'error-message "Bad Unicode escape")
(put 'json-string-escape 'error-conditions
     '(json-string-escape json-error error))

(put 'json-string-format 'error-message "Bad string format")
(put 'json-string-format 'error-conditions
     '(json-string-format json-error error))

(put 'json-object-format 'error-message "Bad JSON object")
(put 'json-object-format 'error-conditions
     '(json-object-format json-error error))



;;; Keywords

(defvar json-keywords '("true" "false" "null")
  "List of JSON keywords.")

;; Keyword parsing

(defun json-read-keyword (keyword)
  "Read a JSON keyword at point.
KEYWORD is the keyword expected."
  (unless (member keyword json-keywords)
    (signal 'json-unknown-keyword (list keyword)))
  (mapc (lambda (char)
          (unless (char-equal char (json-peek))
            (signal 'json-unknown-keyword
                    (list (save-excursion
                            (backward-word 1)
                            (thing-at-point 'word)))))
          (json-advance))
        keyword)
  (unless (looking-at "\\(\\s-\\|[],}]\\|$\\)")
    (signal 'json-unknown-keyword
            (list (save-excursion
                    (backward-word 1)
                    (thing-at-point 'word)))))
  (cond ((string-equal keyword "true") t)
        ((string-equal keyword "false") json-false)
        ((string-equal keyword "null") json-null)))

;; Keyword encoding

(defun json-encode-keyword (keyword)
  "Encode KEYWORD as a JSON value."
  (cond ((eq keyword t)          "true")
        ((eq keyword json-false) "false")
        ((eq keyword json-null)  "null")))

;;; Numbers

;; Number parsing

(defun json-read-number (&optional sign)
 "Read the JSON number following point.
The optional SIGN argument is for internal use.

N.B.: Only numbers which can fit in Emacs Lisp's native number
representation will be parsed correctly."
 ;; If SIGN is non-nil, the number is explicitly signed.
 (let ((number-regexp
        "\\([0-9]+\\)?\\(\\.[0-9]+\\)?\\([Ee][+-]?[0-9]+\\)?"))
   (cond ((and (null sign) (char-equal (json-peek) ?-))
          (json-advance)
          (- (json-read-number t)))
         ((and (null sign) (char-equal (json-peek) ?+))
          (json-advance)
          (json-read-number t))
         ((and (looking-at number-regexp)
               (or (match-beginning 1)
                   (match-beginning 2)))
          (goto-char (match-end 0))
          (string-to-number (match-string 0)))
         (t (signal 'json-number-format (list (point)))))))

;; Number encoding

(defun json-encode-number (number)
  "Return a JSON representation of NUMBER."
  (format "%s" number))

;;; Strings

(defvar json-special-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    (?/ . ?/)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which are escaped in JSON, with their elisp counterparts.")

;; String parsing

(defun json-read-escaped-char ()
  "Read the JSON string escaped character at point."
  ;; Skip over the '\'
  (json-advance)
  (let* ((char (json-pop))
         (special (assq char json-special-chars)))
    (cond
     (special (cdr special))
     ((not (eq char ?u)) char)
     ((looking-at "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]")
      (let ((hex (match-string 0)))
        (json-advance 4)
        (json-decode-char0 'ucs (string-to-number hex 16))))
     (t
      (signal 'json-string-escape (list (point)))))))

(defun json-read-string ()
  "Read the JSON string at point."
  (unless (char-equal (json-peek) ?\")
    (signal 'json-string-format (list "doesn't start with '\"'!")))
  ;; Skip over the '"'
  (json-advance)
  (let ((characters '())
        (char (json-peek)))
    (while (not (char-equal char ?\"))
      (push (if (char-equal char ?\\)
                (json-read-escaped-char)
              (json-pop))
            characters)
      (setq char (json-peek)))
    ;; Skip over the '"'
    (json-advance)
    (if characters
        (apply 'string (nreverse characters))
      "")))

;; String encoding

(defun json-encode-char (char)
  "Encode CHAR as a JSON string."
  (setq char (json-encode-char0 char 'ucs))
  (let ((control-char (car (rassoc char json-special-chars))))
    (cond
     ;; Special JSON character (\n, \r, etc.)
     (control-char
      (format "\\%c" control-char))
     ;; ASCIIish printable character
     ((and (> char 31) (< char 161))
      (format "%c" char))
     ;; Fallback: UCS code point in \uNNNN form
     (t
      (format "\\u%04x" char)))))

(defun json-encode-string (string)
  "Return a JSON representation of STRING."
  (format "\"%s\"" (mapconcat 'json-encode-char string "")))

;;; JSON Objects

(defun json-new-object ()
  "Create a new Elisp object corresponding to a JSON object.
Please see the documentation of `json-object-type'."
  (cond ((eq json-object-type 'hash-table)
         (make-hash-table :test 'equal))
        (t
         (list))))

(defun json-add-to-object (object key value)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object, which you should save, e.g.:
    (setq obj (json-add-to-object obj \"foo\" \"bar\"))
Please see the documentation of `json-object-type' and `json-key-type'."
  (let ((json-key-type
         (if (eq json-key-type nil)
             (cdr (assq json-object-type '((hash-table . string)
                                           (alist . symbol)
                                           (plist . keyword))))
           json-key-type)))
    (setq key
          (cond ((eq json-key-type 'string)
                 key)
                ((eq json-key-type 'symbol)
                 (intern key))
                ((eq json-key-type 'keyword)
                 (intern (concat ":" key)))))
    (cond ((eq json-object-type 'hash-table)
           (puthash key value object)
           object)
          ((eq json-object-type 'alist)
           (cons (cons key value) object))
          ((eq json-object-type 'plist)
           (cons key (cons value object))))))

;; JSON object parsing

(defun json-read-object ()
  "Read the JSON object at point."
  ;; Skip over the "{"
  (json-advance)
  (json-skip-whitespace)
  ;; read key/value pairs until "}"
  (let ((elements (json-new-object))
        key value)
    (while (not (char-equal (json-peek) ?}))
      (json-skip-whitespace)
      (setq key (json-read-string))
      (json-skip-whitespace)
      (if (char-equal (json-peek) ?:)
          (json-advance)
        (signal 'json-object-format (list ":" (json-peek))))
      (setq value (json-read))
      (setq elements (json-add-to-object elements key value))
      (json-skip-whitespace)
      (unless (char-equal (json-peek) ?})
        (if (char-equal (json-peek) ?,)
            (json-advance)
          (signal 'json-object-format (list "," (json-peek))))))
    ;; Skip over the "}"
    (json-advance)
    elements))

;; Hash table encoding

(defun json-encode-hash-table (hash-table)
  "Return a JSON representation of HASH-TABLE."
  (format "{%s}"
          (json-join
           (let (r)
             (maphash
              (lambda (k v)
                (push (format "%s:%s"
                              (json-encode k)
                              (json-encode v))
                      r))
              hash-table)
             r)
           ", ")))

;; List encoding (including alists and plists)

(defun json-encode-alist (alist)
  "Return a JSON representation of ALIST."
  (format "{%s}"
          (json-join (mapcar (lambda (cons)
                               (format "%s:%s"
                                       (json-encode (car cons))
                                       (json-encode (cdr cons))))
                             alist)
                     ", ")))

(defun json-encode-plist (plist)
  "Return a JSON representation of PLIST."
  (let (result)
    (while plist
      (push (concat (json-encode (car plist))
                    ":"
                    (json-encode (cadr plist)))
            result)
      (setq plist (cddr plist)))
    (concat "{" (json-join (nreverse result) ", ") "}")))

(defun json-encode-list (list)
  "Return a JSON representation of LIST.
Tries to DWIM: simple lists become JSON arrays, while alists and plists
become JSON objects."
  (cond ((null list)         "null")
        ((json-alist-p list) (json-encode-alist list))
        ((json-plist-p list) (json-encode-plist list))
        ((listp list)        (json-encode-array list))
        (t
         (signal 'json-error (list list)))))

;;; Arrays

;; Array parsing

(defun json-read-array ()
  "Read the JSON array at point."
  ;; Skip over the "["
  (json-advance)
  (json-skip-whitespace)
  ;; read values until "]"
  (let (elements)
    (while (not (char-equal (json-peek) ?\]))
      (push (json-read) elements)
      (json-skip-whitespace)
      (unless (char-equal (json-peek) ?\])
        (if (char-equal (json-peek) ?,)
            (json-advance)
          (signal 'json-error (list 'bleah)))))
    ;; Skip over the "]"
    (json-advance)
    (apply json-array-type (nreverse elements))))

;; Array encoding

(defun json-encode-array (array)
  "Return a JSON representation of ARRAY."
  (concat "[" (mapconcat 'json-encode array ", ") "]"))



;;; JSON reader.

(defvar json-readtable
  (let ((table
         '((?t json-read-keyword "true")
           (?f json-read-keyword "false")
           (?n json-read-keyword "null")
           (?{ json-read-object)
           (?\[ json-read-array)
           (?\" json-read-string))))
    (mapc (lambda (char)
            (push (list char 'json-read-number) table))
          '(?- ?+ ?. ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table)
  "Readtable for JSON reader.")

(defun json-read ()
  "Parse and return the JSON object following point.
Advances point just past JSON object."
  (json-skip-whitespace)
  (let ((char (json-peek)))
    (if (not (eq char :json-eof))
        (let ((record (cdr (assq char json-readtable))))
          (if (functionp (car record))
              (apply (car record) (cdr record))
            (signal 'json-readtable-error record)))
      (signal 'end-of-file nil))))

;; Syntactic sugar for the reader

(defun json-read-from-string (string)
  "Read the JSON object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (json-read)))

(defun json-read-file (file)
  "Read the first JSON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-read)))



;;; JSON encoder

(defun json-encode (object)
  "Return a JSON representation of OBJECT as a string."
  (cond ((memq object (list t json-null json-false))
         (json-encode-keyword object))
        ((stringp object)      (json-encode-string object))
        ((keywordp object)     (json-encode-string
                                (substring (symbol-name object) 1)))
        ((symbolp object)      (json-encode-string
                                (symbol-name object)))
        ((numberp object)      (json-encode-number object))
        ((arrayp object)       (json-encode-array object))
        ((hash-table-p object) (json-encode-hash-table object))
        ((listp object)        (json-encode-list object))
        (t                     (signal 'json-error (list object)))))

(provide 'json)

;;; json.el ends here
