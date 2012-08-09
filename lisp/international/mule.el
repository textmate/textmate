;;; mule.el --- basic commands for multilingual environment

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, multilingual, character set, coding system

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

(defconst mule-version "6.0 (HANACHIRUSATO)" "\
Version number and name of this version of MULE (multilingual environment).")

(defconst mule-version-date "2003.9.1" "\
Distribution date of this version of MULE (multilingual environment).")


;;; CHARSET

;; Backward compatibility code for handling emacs-mule charsets.
(defvar private-char-area-1-min #xF0000)
(defvar private-char-area-1-max #xFFFFE)
(defvar private-char-area-2-min #x100000)
(defvar private-char-area-2-max #x10FFFE)

;; Table of emacs-mule charsets indexed by their emacs-mule ID.
(defvar emacs-mule-charset-table (make-vector 256 nil))
(aset emacs-mule-charset-table 0 'ascii)

;; Convert the argument of old-style call of define-charset to a
;; property list used by the new-style.
;; INFO-VECTOR is a vector of the format:
;;   [DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
;;    SHORT-NAME LONG-NAME DESCRIPTION]

(defun convert-define-charset-argument (emacs-mule-id info-vector)
  (let* ((dim (aref info-vector 0))
	 (chars (aref info-vector 1))
	 (total (if (= dim 1) chars (* chars chars)))
	 (code-space (if (= dim 1) (if (= chars 96) [32 127] [33 126])
		       (if (= chars 96) [32 127 32 127] [33 126 33 126])))
	 code-offset)
    (if (integerp emacs-mule-id)
	(or (= emacs-mule-id 0)
	    (and (>= emacs-mule-id 129) (< emacs-mule-id 256))
	    (error "Invalid CHARSET-ID: %d" emacs-mule-id))
      (let (from-id to-id)
	(if (= dim 1) (setq from-id 160 to-id 224)
	  (setq from-id 224 to-id 255))
	(while (and (< from-id to-id)
		    (not (aref emacs-mule-charset-table from-id)))
	  (setq from-id (1+ from-id)))
	(if (= from-id to-id)
	    (error "No more room for the new Emacs-mule charset"))
	(setq emacs-mule-id from-id)))
    (if (> (- private-char-area-1-max private-char-area-1-min) total)
	(setq code-offset private-char-area-1-min
	      private-char-area-1-min (+ private-char-area-1-min total))
      (if (> (- private-char-area-2-max private-char-area-2-min) total)
	  (setq code-offset private-char-area-2-min
		private-char-area-2-min (+ private-char-area-2-min total))
	(error "No more space for a new charset")))
    (list :dimension dim
	  :code-space code-space
	  :iso-final-char (aref info-vector 4)
	  :code-offset code-offset
	  :emacs-mule-id emacs-mule-id)))

(defun define-charset (name docstring &rest props)
  "Define NAME (symbol) as a charset with DOCSTRING.
The remaining arguments must come in pairs ATTRIBUTE VALUE.  ATTRIBUTE
may be any symbol.  The following have special meanings, and one of
`:code-offset', `:map', `:subset', `:superset' must be specified.

`:short-name'

VALUE must be a short string to identify the charset.  If omitted,
NAME is used.

`:long-name'

VALUE must be a string longer than `:short-name' to identify the
charset.  If omitted, the value of the `:short-name' attribute is used.

`:dimension'

VALUE must be an integer 0, 1, 2, or 3, specifying the dimension of
code-points of the charsets.  If omitted, it is calculated from the
value of the `:code-space' attribute.

`:code-space'

VALUE must be a vector of length at most 8 specifying the byte code
range of each dimension in this format:
	[ MIN-1 MAX-1 MIN-2 MAX-2 ... ]
where MIN-N is the minimum byte value of Nth dimension of code-point,
MAX-N is the maximum byte value of that.

`:min-code'

VALUE must be an integer specifying the minimum code point of the
charset.  If omitted, it is calculated from `:code-space'.  VALUE may
be a cons (HIGH . LOW), where HIGH is the most significant 16 bits of
the code point and LOW is the least significant 16 bits.

`:max-code'

VALUE must be an integer specifying the maximum code point of the
charset.  If omitted, it is calculated from `:code-space'.  VALUE may
be a cons (HIGH . LOW), where HIGH is the most significant 16 bits of
the code point and LOW is the least significant 16 bits.

`:iso-final-char'

VALUE must be a character in the range 32 to 127 (inclusive)
specifying the final char of the charset for ISO-2022 encoding.  If
omitted, the charset can't be encoded by ISO-2022 based
coding-systems.

`:iso-revision-number'

VALUE must be an integer in the range 0..63, specifying the revision
number of the charset for ISO-2022 encoding.

`:emacs-mule-id'

VALUE must be an integer of 0, 129..255.  If omitted, the charset
can't be encoded by coding-systems of type `emacs-mule'.

`:ascii-compatible-p'

VALUE must be nil or t (default nil).  If VALUE is t, the charset is
compatible with ASCII, i.e. the first 128 code points map to ASCII.

`:supplementary-p'

VALUE must be nil or t.  If the VALUE is t, the charset is
supplementary, which means it is used only as a parent or a
subset of some other charset, or it is provided just for backward
compatibility.

`:invalid-code'

VALUE must be a nonnegative integer that can be used as an invalid
code point of the charset.  If the minimum code is 0 and the maximum
code is greater than Emacs's maximum integer value, `:invalid-code'
should not be omitted.

`:code-offset'

VALUE must be an integer added to the index number of a character to
get the corresponding character code.

`:map'

VALUE must be vector or string.

If it is a vector, the format is [ CODE-1 CHAR-1 CODE-2 CHAR-2 ... ],
where CODE-n is a code-point of the charset, and CHAR-n is the
corresponding character code.

If it is a string, it is a name of file that contains the above
information.   Each line of the file must be this format:
	0xXXX 0xYYY
where XXX is a hexadecimal representation of CODE-n and YYY is a
hexadecimal representation of CHAR-n.  A line starting with `#' is a
comment line.

`:subset'

VALUE must be a list:
	( PARENT MIN-CODE MAX-CODE OFFSET )
PARENT is a parent charset.  MIN-CODE and MAX-CODE specify the range
of characters inherited from the parent.  OFFSET is an integer value
to add to a code point of the parent charset to get the corresponding
code point of this charset.

`:superset'

VALUE must be a list of parent charsets.  The charset inherits
characters from them.  Each element of the list may be a cons (PARENT
. OFFSET), where PARENT is a parent charset, and OFFSET is an offset
value to add to a code point of PARENT to get the corresponding code
point of this charset.

`:unify-map'

VALUE must be vector or string.

If it is a vector, the format is [ CODE-1 CHAR-1 CODE-2 CHAR-2 ... ],
where CODE-n is a code-point of the charset, and CHAR-n is the
corresponding Unicode character code.

If it is a string, it is a name of file that contains the above
information.  The file format is the same as what described for `:map'
attribute."
  (when (vectorp (car props))
    ;; Old style code:
    ;;   (define-charset CHARSET-ID CHARSET-SYMBOL INFO-VECTOR)
    ;; Convert the argument to make it fit with the current style.
    (let ((vec (car props)))
      (setq props (convert-define-charset-argument name vec)
	    name docstring
	    docstring (aref vec 8))))
  (let ((attrs (mapcar 'list '(:dimension
			       :code-space
			       :min-code
			       :max-code
			       :iso-final-char
			       :iso-revision-number
			       :emacs-mule-id
			       :ascii-compatible-p
			       :supplementary-p
			       :invalid-code
			       :code-offset
			       :map
			       :subset
			       :superset
			       :unify-map
			       :plist))))

    ;; If :dimension is omitted, get the dimension from :code-space.
    (let ((dimension (plist-get props :dimension)))
      (or dimension
	  (let ((code-space (plist-get props :code-space)))
	    (setq dimension (if code-space (/ (length code-space) 2) 4))
	    (setq props (plist-put props :dimension dimension)))))

    (let ((code-space (plist-get props :code-space)))
      (or code-space
	  (let ((dimension (plist-get props :dimension)))
	    (setq code-space (make-vector 8 0))
	    (dotimes (i dimension)
	      (aset code-space (1+ (* i 2)) #xFF))
	    (setq props (plist-put props :code-space code-space)))))

    ;; If :emacs-mule-id is specified, update emacs-mule-charset-table.
    (let ((emacs-mule-id (plist-get props :emacs-mule-id)))
      (if (integerp emacs-mule-id)
	  (aset emacs-mule-charset-table emacs-mule-id name)))

    (dolist (slot attrs)
      (setcdr slot (purecopy (plist-get props (car slot)))))

    ;; Make sure that the value of :code-space is a vector of 8
    ;; elements.
    (let* ((slot (assq :code-space attrs))
	   (val (cdr slot))
	   (len (length val)))
      (if (< len 8)
	  (setcdr slot
		  (vconcat val (make-vector (- 8 len) 0)))))

    ;; Add :name and :docstring properties to PROPS.
    (setq props
	  (cons :name (cons name (cons :docstring (cons (purecopy docstring) props)))))
    (or (plist-get props :short-name)
	(plist-put props :short-name (symbol-name name)))
    (or (plist-get props :long-name)
	(plist-put props :long-name (plist-get props :short-name)))
    (plist-put props :base name)
    ;; We can probably get a worthwhile amount in purespace.
    (setq props
	  (mapcar (lambda (elt)
		    (if (stringp elt)
			(purecopy elt)
		      elt))
		  props))
    (setcdr (assq :plist attrs) props)

    (apply 'define-charset-internal name (mapcar 'cdr attrs))))


(defun load-with-code-conversion (fullname file &optional noerror nomessage)
  "Execute a file of Lisp code named FILE whose absolute name is FULLNAME.
The file contents are decoded before evaluation if necessary.
If optional third arg NOERROR is non-nil,
 report no error if FILE doesn't exist.
Print messages at start and end of loading unless
 optional fourth arg NOMESSAGE is non-nil.
Return t if file exists."
  (if (null (file-readable-p fullname))
      (and (null noerror)
	   (signal 'file-error (list "Cannot open load file" file)))
    ;; Read file with code conversion, and then eval.
    (let* ((buffer
            ;; We can't use `generate-new-buffer' because files.el
            ;; is not yet loaded.
            (get-buffer-create (generate-new-buffer-name " *load*")))
	   (load-in-progress t)
	   (source (save-match-data (string-match "\\.el\\'" fullname))))
      (unless nomessage
	(if source
	    (message "Loading %s (source)..." file)
	  (message "Loading %s..." file)))
      (when purify-flag
	(push (purecopy file) preloaded-file-list))
      (unwind-protect
	  (let ((load-file-name fullname)
		(set-auto-coding-for-load t)
		(inhibit-file-name-operation nil))
	    (with-current-buffer buffer
              ;; So that we don't get completely screwed if the
              ;; file is encoded in some complicated character set,
              ;; read it with real decoding, as a multibyte buffer.
              (set-buffer-multibyte t)
	      ;; Don't let deactivate-mark remain set.
	      (let (deactivate-mark)
		(insert-file-contents fullname))
	      ;; If the loaded file was inserted with no-conversion or
	      ;; raw-text coding system, make the buffer unibyte.
	      ;; Otherwise, eval-buffer might try to interpret random
	      ;; binary junk as multibyte characters.
	      (if (and enable-multibyte-characters
		       (or (eq (coding-system-type last-coding-system-used)
			       'raw-text)))
		  (set-buffer-multibyte nil))
	      ;; Make `kill-buffer' quiet.
	      (set-buffer-modified-p nil))
	    ;; Have the original buffer current while we eval.
	    (eval-buffer buffer nil
			 ;; This is compatible with what `load' does.
			 (if purify-flag file fullname)
			 nil t))
	(let (kill-buffer-hook kill-buffer-query-functions)
	  (kill-buffer buffer)))
      (do-after-load-evaluation fullname)

      (unless (or nomessage noninteractive)
	(if source
	    (message "Loading %s (source)...done" file)
	  (message "Loading %s...done" file)))
      t)))

(defun charset-info (charset)
  "Return a vector of information of CHARSET.
This function is provided for backward compatibility.

The elements of the vector are:
	CHARSET-ID, BYTES, DIMENSION, CHARS, WIDTH, DIRECTION,
	LEADING-CODE-BASE, LEADING-CODE-EXT,
	ISO-FINAL-CHAR, ISO-GRAPHIC-PLANE,
	REVERSE-CHARSET, SHORT-NAME, LONG-NAME,	DESCRIPTION,
	PLIST.
where
CHARSET-ID is always 0.
BYTES is always 0.
DIMENSION is the number of bytes of a code-point of the charset:
  1, 2, 3, or 4.
CHARS is the number of characters in a dimension:
  94, 96, 128, or 256.
WIDTH is always 0.
DIRECTION is always 0.
LEADING-CODE-BASE is always 0.
LEADING-CODE-EXT is always 0.
ISO-FINAL-CHAR (character) is the final character of the
  corresponding ISO 2022 charset.  If the charset is not assigned
  any final character, the value is -1.
ISO-GRAPHIC-PLANE is always 0.
REVERSE-CHARSET is always -1.
SHORT-NAME (string) is the short name to refer to the charset.
LONG-NAME (string) is the long name to refer to the charset
DESCRIPTION (string) is the description string of the charset.
PLIST (property list) may contain any type of information a user
  want to put and get by functions `put-charset-property' and
  `get-charset-property' respectively."
  (vector 0
	  0
	  (charset-dimension charset)
	  (charset-chars charset)
	  0
	  0
	  0
	  0
	  (charset-iso-final-char charset)
	  0
	  -1
	  (get-charset-property charset :short-name)
	  (get-charset-property charset :short-name)
	  (charset-description charset)
	  (charset-plist charset)))

;; It is better not to use backquote in this file,
;; because that makes a bootstrapping problem
;; if you need to recompile all the Lisp files using interpreted code.

(defun charset-id (charset)
  "Always return 0.  This is provided for backward compatibility."
  0)
(make-obsolete 'charset-id "do not use it." "23.1")

(defmacro charset-bytes (charset)
  "Always return 0.  This is provided for backward compatibility."
  0)
(make-obsolete 'charset-bytes "do not use it." "23.1")

(defun get-charset-property (charset propname)
  "Return the value of CHARSET's PROPNAME property.
This is the last value stored with
 (put-charset-property CHARSET PROPNAME VALUE)."
  (plist-get (charset-plist charset) propname))

(defun put-charset-property (charset propname value)
  "Set CHARSETS's PROPNAME property to value VALUE.
It can be retrieved with `(get-charset-property CHARSET PROPNAME)'."
  (set-charset-plist charset
		     (plist-put (charset-plist charset) propname
				(if (stringp value)
				    (purecopy value)
				  value))))

(defun charset-description (charset)
  "Return description string of CHARSET."
  (plist-get (charset-plist charset) :docstring))

(defun charset-dimension (charset)
  "Return dimension of CHARSET."
  (plist-get (charset-plist charset) :dimension))

(defun charset-chars (charset &optional dimension)
  "Return number of characters contained in DIMENSION of CHARSET.
DIMENSION defaults to the first dimension."
  (unless dimension (setq dimension 1))
  (let ((code-space (plist-get (charset-plist charset) :code-space)))
    (1+ (- (aref code-space (1- (* 2 dimension)))
	   (aref code-space (- (* 2 dimension) 2))))))

(defun charset-iso-final-char (charset)
  "Return ISO-2022 final character of CHARSET.
Return -1 if charset isn't an ISO 2022 one."
  (or (plist-get (charset-plist charset) :iso-final-char)
      -1))

(defmacro charset-short-name (charset)
  "Return short name of CHARSET."
  (plist-get (charset-plist charset) :short-name))

(defmacro charset-long-name (charset)
  "Return long name of CHARSET."
  (plist-get (charset-plist charset) :long-name))

(defun charset-list ()
  "Return list of all charsets ever defined."
  charset-list)
(make-obsolete 'charset-list "use variable `charset-list'." "23.1")


;;; CHARACTER
(define-obsolete-function-alias 'char-valid-p 'characterp "23.1")

(defun generic-char-p (char)
  "Always return nil.  This is provided for backward compatibility."
  nil)
(make-obsolete 'generic-char-p "generic characters no longer exist." "23.1")

(defun make-char-internal (charset-id &optional code1 code2)
  (let ((charset (aref emacs-mule-charset-table charset-id)))
    (or charset
	(error "Invalid Emacs-mule charset ID: %d" charset-id))
    (make-char charset code1 code2)))

;; Save the ASCII case table in case we need it later.  Some locales
;; (such as Turkish) modify the case behavior of ASCII characters,
;; which can interfere with networking code that uses ASCII strings.

(defvar ascii-case-table
  ;; Code copied from copy-case-table to avoid requiring case-table.el
  (let ((tbl (copy-sequence (standard-case-table)))
	(up  (char-table-extra-slot (standard-case-table) 0)))
    (if up (set-char-table-extra-slot tbl 0 (copy-sequence up)))
    (set-char-table-extra-slot tbl 1 nil)
    (set-char-table-extra-slot tbl 2 nil)
    tbl)
  "Case table for the ASCII character set.")

;; Coding system stuff

;; Coding system is a symbol that has been defined by the function
;; `define-coding-system'.

(defconst coding-system-iso-2022-flags
  '(long-form
    ascii-at-eol
    ascii-at-cntl
    7-bit
    locking-shift
    single-shift
    designation
    revision
    direction
    init-at-bol
    designate-at-bol
    safe
    latin-extra
    composition
    euc-tw-shift
    use-roman
    use-oldjis)
  "List of symbols that control ISO-2022 encoder/decoder.

The value of the `:flags' attribute in the argument of the function
`define-coding-system' must be one of them.

If `long-form' is specified, use a long designation sequence on
encoding for the charsets `japanese-jisx0208-1978', `chinese-gb2312',
and `japanese-jisx0208'.  The long designation sequence doesn't
conform to ISO 2022, but is used by such coding systems as
`compound-text'.

If `ascii-at-eol' is specified, designate ASCII to g0 at end of line
on encoding.

If `ascii-at-cntl' is specified, designate ASCII to g0 before control
codes and SPC on encoding.

If `7-bit' is specified, use 7-bit code only on encoding.

If `locking-shift' is specified, decode locking-shift code correctly
on decoding, and use locking-shift to invoke a graphic element on
encoding.

If `single-shift' is specified, decode single-shift code correctly on
decoding, and use single-shift to invoke a graphic element on encoding.

If `designation' is specified, decode designation code correctly on
decoding, and use designation to designate a charset to a graphic
element on encoding.

If `revision' is specified, produce an escape sequence to specify
revision number of a charset on encoding.  Such an escape sequence is
always correctly decoded on decoding.

If `direction' is specified, decode ISO6429's code for specifying
direction correctly, and produce the code on encoding.

If `init-at-bol' is specified, on encoding, it is assumed that
invocation and designation statuses are reset at each beginning of
line even if `ascii-at-eol' is not specified; thus no codes for
resetting them are produced.

If `safe' is specified, on encoding, characters not supported by a
coding are replaced with `?'.

If `latin-extra' is specified, the code-detection routine assumes that a
code specified in `latin-extra-code-table' (which see) is valid.

If `composition' is specified, an escape sequence to specify
composition sequence is correctly decoded on decoding, and is produced
on encoding.

If `euc-tw-shift' is specified, the EUC-TW specific shifting code is
correctly decoded on decoding, and is produced on encoding.

If `use-roman' is specified, JIS0201-1976-Roman is designated instead
of ASCII.

If `use-oldjis' is specified, JIS0208-1976 is designated instead of
JIS0208-1983.")

(defun define-coding-system (name docstring &rest props)
  "Define NAME (a symbol) as a coding system with DOCSTRING and attributes.
The remaining arguments must come in pairs ATTRIBUTE VALUE.  ATTRIBUTE
may be any symbol.

The following attributes have special meanings.  Those labeled as
\"(required)\" should not be omitted.

`:mnemonic' (required)

VALUE is a character to display on mode line for the coding system.

`:coding-type' (required)

VALUE must be one of `charset', `utf-8', `utf-16', `iso-2022',
`emacs-mule', `shift-jis', `ccl', `raw-text', `undecided'.

`:eol-type'

VALUE is the EOL (end-of-line) format of the coding system.  It must be
one of `unix', `dos', `mac'.  The symbol `unix' means Unix-like EOL
\(i.e. single LF), `dos' means DOS-like EOL \(i.e. sequence of CR LF),
and `mac' means Mac-like EOL \(i.e. single CR).  If omitted, Emacs
detects the EOL format automatically when decoding.

`:charset-list'

VALUE must be a list of charsets supported by the coding system.  On
encoding by the coding system, if a character belongs to multiple
charsets in the list, a charset that comes earlier in the list is
selected.  If `:coding-type' is `iso-2022', VALUE may be `iso-2022',
which indicates that the coding system supports all ISO-2022 based
charsets.  If `:coding-type' is `emacs-mule', VALUE may be
`emacs-mule', which indicates that the coding system supports all
charsets that have the `:emacs-mule-id' property.

`:ascii-compatible-p'

If VALUE is non-nil, the coding system decodes all 7-bit bytes into
the corresponding ASCII characters, and encodes all ASCII characters
back to the corresponding 7-bit bytes.  VALUE defaults to nil.

`:decode-translation-table'

VALUE must be a translation table to use on decoding.

`:encode-translation-table'

VALUE must be a translation table to use on encoding.

`:post-read-conversion'

VALUE must be a function to call after some text is inserted and
decoded by the coding system itself and before any functions in
`after-insert-functions' are called.  This function is passed one
argument; the number of characters in the text to convert, with
point at the start of the text.  The function should leave point
the same, and return the new character count.

`:pre-write-conversion'

VALUE must be a function to call after all functions in
`write-region-annotate-functions' and `buffer-file-format' are
called, and before the text is encoded by the coding system
itself.  This function should convert the whole text in the
current buffer.  For backward compatibility, this function is
passed two arguments which can be ignored.

`:default-char'

VALUE must be a character.  On encoding, a character not supported by
the coding system is replaced with VALUE.

`:for-unibyte'

VALUE non-nil means that visiting a file with the coding system
results in a unibyte buffer.

`:mime-charset'

VALUE must be a symbol whose name is that of a MIME charset converted
to lower case.

`:mime-text-unsuitable'

VALUE non-nil means the `:mime-charset' property names a charset which
is unsuitable for the top-level media type \"text\".

`:flags'

VALUE must be a list of symbols that control the ISO-2022 converter.
Each must be a member of the list `coding-system-iso-2022-flags'
\(which see).  This attribute has a meaning only when `:coding-type'
is `iso-2022'.

`:designation'

VALUE must be a vector [G0-USAGE G1-USAGE G2-USAGE G3-USAGE].
GN-USAGE specifies the usage of graphic register GN as follows.

If it is nil, no charset can be designated to GN.

If it is a charset, the charset is initially designated to GN, and
never used by the other charsets.

If it is a list, the elements must be charsets, nil, 94, or 96.  GN
can be used by all the listed charsets.  If the list contains 94, any
iso-2022 charset whose code-space ranges are 94 long can be designated
to GN.  If the list contains 96, any charsets whose whose ranges are
96 long can be designated to GN.  If the first element is a charset,
that charset is initially designated to GN.

This attribute has a meaning only when `:coding-type' is `iso-2022'.

`:bom'

This attributes specifies whether the coding system uses a `byte order
mark'.  VALUE must be nil, t, or cons of coding systems whose
`:coding-type' is `utf-16' or `utf-8'.

If the value is nil, on decoding, don't treat the first two-byte as
BOM, and on encoding, don't produce BOM bytes.

If the value is t, on decoding, skip the first two-byte as BOM, and on
encoding, produce BOM bytes according to the value of `:endian'.

If the value is cons, on decoding, check the first two-byte.  If they
are 0xFE 0xFF, use the car part coding system of the value.  If they
are 0xFF 0xFE, use the cdr part coding system of the value.
Otherwise, treat them as bytes for a normal character.  On encoding,
produce BOM bytes according to the value of `:endian'.

This attribute has a meaning only when `:coding-type' is `utf-16' or
`utf-8'.

`:endian'

VALUE must be `big' or `little' specifying big-endian and
little-endian respectively.  The default value is `big'.

This attribute has a meaning only when `:coding-type' is `utf-16'.

`:ccl-decoder'

VALUE is a symbol representing the registered CCL program used for
decoding.  This attribute has a meaning only when `:coding-type' is
`ccl'.

`:ccl-encoder'

VALUE is a symbol representing the registered CCL program used for
encoding.  This attribute has a meaning only when `:coding-type' is
`ccl'."
  (let* ((common-attrs (mapcar 'list
			       '(:mnemonic
				 :coding-type
				 :charset-list
				 :ascii-compatible-p
				 :decode-translation-table
				 :encode-translation-table
				 :post-read-conversion
				 :pre-write-conversion
				 :default-char
				 :for-unibyte
				 :plist
				 :eol-type)))
	 (coding-type (plist-get props :coding-type))
	 (spec-attrs (mapcar 'list
			     (cond ((eq coding-type 'iso-2022)
				    '(:initial
				      :reg-usage
				      :request
				      :flags))
				   ((eq coding-type 'utf-8)
				    '(:bom))
				   ((eq coding-type 'utf-16)
				    '(:bom
				      :endian))
				   ((eq coding-type 'ccl)
				    '(:ccl-decoder
				      :ccl-encoder
				      :valids))))))

    (dolist (slot common-attrs)
      (setcdr slot (plist-get props (car slot))))

    (dolist (slot spec-attrs)
      (setcdr slot (plist-get props (car slot))))

    (if (eq coding-type 'iso-2022)
	(let ((designation (plist-get props :designation))
	      (flags (plist-get props :flags))
	      (initial (make-vector 4 nil))
	      (reg-usage (cons 4 4))
	      request elt)
	  (dotimes (i 4)
	    (setq elt (aref designation i))
	    (cond ((charsetp elt)
		   (aset initial i elt)
		   (setq request (cons (cons elt i) request)))
		  ((consp elt)
		   (aset initial i (car elt))
		   (if (charsetp (car elt))
		       (setq request (cons (cons (car elt) i) request)))
		   (dolist (e (cdr elt))
		     (cond ((charsetp e)
			    (setq request (cons (cons e i) request)))
			   ((eq e 94)
			    (setcar reg-usage i))
			   ((eq e 96)
			    (setcdr reg-usage i))
			   ((eq e t)
			    (setcar reg-usage i)
			    (setcdr reg-usage i)))))))
	  (setcdr (assq :initial spec-attrs) initial)
	  (setcdr (assq :reg-usage spec-attrs) reg-usage)
	  (setcdr (assq :request spec-attrs) request)

	  ;; Change :flags value from a list to a bit-mask.
	  (let ((bits 0)
		(i 0))
	    (dolist (elt coding-system-iso-2022-flags)
	      (if (memq elt flags)
		  (setq bits (logior bits (lsh 1 i))))
	      (setq i (1+ i)))
	    (setcdr (assq :flags spec-attrs) bits))))

    ;; Add :name and :docstring properties to PROPS.
    (setq props
	  (cons :name (cons name (cons :docstring (cons (purecopy docstring)
							props)))))
    (setcdr (assq :plist common-attrs) props)
    (apply 'define-coding-system-internal
	   name (mapcar 'cdr (append common-attrs spec-attrs)))))

(defun coding-system-doc-string (coding-system)
  "Return the documentation string for CODING-SYSTEM."
  (plist-get (coding-system-plist coding-system) :docstring))

(defun coding-system-mnemonic (coding-system)
  "Return the mnemonic character of CODING-SYSTEM.
The mnemonic character of a coding system is used in mode line to
indicate the coding system.  If CODING-SYSTEM is nil, return ?=."
  (plist-get (coding-system-plist coding-system) :mnemonic))

(defun coding-system-type (coding-system)
  "Return the coding type of CODING-SYSTEM.
A coding type is a symbol indicating the encoding method of CODING-SYSTEM.
See the function `define-coding-system' for more detail."
  (plist-get (coding-system-plist coding-system) :coding-type))

(defun coding-system-charset-list (coding-system)
  "Return list of charsets supported by CODING-SYSTEM.
If CODING-SYSTEM supports all ISO-2022 charsets, return `iso-2022'.
If CODING-SYSTEM supports all emacs-mule charsets, return `emacs-mule'."
  (plist-get (coding-system-plist coding-system) :charset-list))

(defun coding-system-category (coding-system)
  "Return a category symbol of CODING-SYSTEM."
  (plist-get (coding-system-plist coding-system) :category))

(defun coding-system-get (coding-system prop)
  "Extract a value from CODING-SYSTEM's property list for property PROP.
For compatibility with Emacs 20/21, this accepts old-style symbols
like `mime-charset' as well as the current style like `:mime-charset'."
  (or (plist-get (coding-system-plist coding-system) prop)
      (if (not (keywordp prop))
	  ;; For backward compatibility.
	  (if (eq prop 'ascii-incompatible)
	      (not (plist-get (coding-system-plist coding-system)
			      :ascii-compatible-p))
	    (plist-get (coding-system-plist coding-system)
		       (intern (concat ":" (symbol-name prop))))))))

(defun coding-system-eol-type-mnemonic (coding-system)
  "Return the string indicating end-of-line format of CODING-SYSTEM."
  (let* ((eol-type (coding-system-eol-type coding-system))
	 (val (cond ((eq eol-type 0) eol-mnemonic-unix)
		    ((eq eol-type 1) eol-mnemonic-dos)
		    ((eq eol-type 2) eol-mnemonic-mac)
		    (t eol-mnemonic-undecided))))
    (if (stringp val)
	val
      (char-to-string val))))

(defun coding-system-lessp (x y)
  (cond ((eq x 'no-conversion) t)
	((eq y 'no-conversion) nil)
	((eq x 'emacs-mule) t)
	((eq y 'emacs-mule) nil)
	((eq x 'undecided) t)
	((eq y 'undecided) nil)
	(t (let ((c1 (coding-system-mnemonic x))
		 (c2 (coding-system-mnemonic y)))
	     (or (< (downcase c1) (downcase c2))
		 (and (not (> (downcase c1) (downcase c2)))
		      (< c1 c2)))))))

(defun coding-system-equal (coding-system-1 coding-system-2)
  "Return t if and only if CODING-SYSTEM-1 and CODING-SYSTEM-2 are identical.
Two coding systems are identical if both symbols are equal
or one is an alias of the other."
  (or (eq coding-system-1 coding-system-2)
      (and (equal (coding-system-plist coding-system-1)
		  (coding-system-plist coding-system-2))
	   (let ((eol-type-1 (coding-system-eol-type coding-system-1))
		 (eol-type-2 (coding-system-eol-type coding-system-2)))
	     (or (eq eol-type-1 eol-type-2)
		 (and (vectorp eol-type-1) (vectorp eol-type-2)))))))

(defun add-to-coding-system-list (coding-system)
  "Add CODING-SYSTEM to `coding-system-list' while keeping it sorted."
  (if (or (null coding-system-list)
	  (coding-system-lessp coding-system (car coding-system-list)))
      (setq coding-system-list (cons coding-system coding-system-list))
    (let ((len (length coding-system-list))
	  mid (tem coding-system-list))
      (while (> len 1)
	(setq mid (nthcdr (/ len 2) tem))
	(if (coding-system-lessp (car mid) coding-system)
	    (setq tem mid
		  len (- len (/ len 2)))
	  (setq len (/ len 2))))
      (setcdr tem (cons coding-system (cdr tem))))))

(defun coding-system-list (&optional base-only)
  "Return a list of all existing non-subsidiary coding systems.
If optional arg BASE-ONLY is non-nil, only base coding systems are
listed.  The value doesn't include subsidiary coding systems which are
made from bases and aliases automatically for various end-of-line
formats (e.g. iso-latin-1-unix, koi8-r-dos)."
  (let ((codings nil))
    (dolist (coding coding-system-list)
      (if (eq (coding-system-base coding) coding)
	  (if base-only
	      (setq codings (cons coding codings))
	    (dolist (alias (coding-system-aliases coding))
	      (setq codings (cons alias codings))))))
    codings))

(defconst char-coding-system-table nil
  "It exists just for backward compatibility, and the value is always nil.")
(make-obsolete-variable 'char-coding-system-table nil "23.1")

(defun transform-make-coding-system-args (name type &optional doc-string props)
  "For internal use only.
Transform XEmacs style args for `make-coding-system' to Emacs style.
Value is a list of transformed arguments."
  (let ((mnemonic (string-to-char (or (plist-get props 'mnemonic) "?")))
	(eol-type (plist-get props 'eol-type))
	properties tmp)
    (cond
     ((eq eol-type 'lf) (setq eol-type 'unix))
     ((eq eol-type 'crlf) (setq eol-type 'dos))
     ((eq eol-type 'cr) (setq eol-type 'mac)))
    (if (setq tmp (plist-get props 'post-read-conversion))
	(setq properties (plist-put properties 'post-read-conversion tmp)))
    (if (setq tmp (plist-get props 'pre-write-conversion))
	(setq properties (plist-put properties 'pre-write-conversion tmp)))
    (cond
     ((eq type 'shift-jis)
      `(,name 1 ,mnemonic ,doc-string () ,properties ,eol-type))
     ((eq type 'iso2022) ; This is not perfect.
      (if (plist-get props 'escape-quoted)
	  (error "escape-quoted is not supported: %S"
		 `(,name ,type ,doc-string ,props)))
      (let ((g0 (plist-get props 'charset-g0))
      	    (g1 (plist-get props 'charset-g1))
      	    (g2 (plist-get props 'charset-g2))
      	    (g3 (plist-get props 'charset-g3))
      	    (use-roman
             (and
	      (eq (cadr (assoc 'latin-jisx0201
			       (plist-get props 'input-charset-conversion)))
		  'ascii)
	      (eq (cadr (assoc 'ascii
			       (plist-get props 'output-charset-conversion)))
		  'latin-jisx0201)))
            (use-oldjis
             (and
	      (eq (cadr (assoc 'japanese-jisx0208-1978
			       (plist-get props 'input-charset-conversion)))
		  'japanese-jisx0208)
	      (eq (cadr (assoc 'japanese-jisx0208
			       (plist-get props 'output-charset-conversion)))
		  'japanese-jisx0208-1978))))
	(if (charsetp g0)
	    (if (plist-get props 'force-g0-on-output)
		(setq g0 `(nil ,g0))
	      (setq g0 `(,g0 t))))
	(if (charsetp g1)
	    (if (plist-get props 'force-g1-on-output)
		(setq g1 `(nil ,g1))
	      (setq g1 `(,g1 t))))
	(if (charsetp g2)
	    (if (plist-get props 'force-g2-on-output)
		(setq g2 `(nil ,g2))
	      (setq g2 `(,g2 t))))
	(if (charsetp g3)
	    (if (plist-get props 'force-g3-on-output)
		(setq g3 `(nil ,g3))
	      (setq g3 `(,g3 t))))
	`(,name 2 ,mnemonic ,doc-string
	  (,g0 ,g1 ,g2 ,g3
	   ,(plist-get props 'short)
	   ,(not (plist-get props 'no-ascii-eol))
	   ,(not (plist-get props 'no-ascii-cntl))
	   ,(plist-get props 'seven)
	   t
	   ,(not (plist-get props 'lock-shift))
	   ,use-roman
	   ,use-oldjis
	   ,(plist-get props 'no-iso6429)
	   nil nil nil nil)
	,properties ,eol-type)))
     ((eq type 'big5)
      `(,name 3 ,mnemonic ,doc-string () ,properties ,eol-type))
     ((eq type 'ccl)
      `(,name 4 ,mnemonic ,doc-string
	      (,(plist-get props 'decode) . ,(plist-get props 'encode))
	      ,properties ,eol-type))
     (t
      (error "unsupported XEmacs style make-coding-style arguments: %S"
	     `(,name ,type ,doc-string ,props))))))

(defun make-coding-system (coding-system type mnemonic doc-string
					 &optional
					 flags
					 properties
					 eol-type)
  "Define a new coding system CODING-SYSTEM (symbol).
This function is provided for backward compatibility."
  ;; For compatibility with XEmacs, we check the type of TYPE.  If it
  ;; is a symbol, perhaps, this function is called with XEmacs-style
  ;; arguments.  Here, try to transform that kind of arguments to
  ;; Emacs style.
  (if (symbolp type)
      (let ((args (transform-make-coding-system-args coding-system type
						     mnemonic doc-string)))
	(setq coding-system (car args)
	      type (nth 1 args)
	      mnemonic (nth 2 args)
	      doc-string (nth 3 args)
	      flags (nth 4 args)
	      properties (nth 5 args)
	      eol-type (nth 6 args))))

  (setq type
	(cond ((eq type 0) 'emacs-mule)
	      ((eq type 1) 'shift-jis)
	      ((eq type 2) 'iso2022)
	      ((eq type 3) 'big5)
	      ((eq type 4) 'ccl)
	      ((eq type 5) 'raw-text)
	      (t
	       (error "Invalid coding system type: %s" type))))

  (setq properties
	(let ((plist nil) key)
	  (dolist (elt properties)
	    (setq key (car elt))
	    (cond ((eq key 'post-read-conversion)
		   (setq key :post-read-conversion))
		  ((eq key 'pre-write-conversion)
		   (setq key :pre-write-conversion))
		  ((eq key 'translation-table-for-decode)
		   (setq key :decode-translation-table))
		  ((eq key 'translation-table-for-encode)
		   (setq key :encode-translation-table))
		  ((eq key 'safe-charsets)
		   (setq key :charset-list))
		  ((eq key 'mime-charset)
		   (setq key :mime-charset))
		  ((eq key 'valid-codes)
		   (setq key :valids)))
	    (setq plist (plist-put plist key (cdr elt))))
	  plist))
  (setq properties (plist-put properties :mnemonic mnemonic))
  (plist-put properties :coding-type type)
  (cond ((eq eol-type 0) (setq eol-type 'unix))
	((eq eol-type 1) (setq eol-type 'dos))
	((eq eol-type 2) (setq eol-type 'mac))
	((vectorp eol-type) (setq eol-type nil)))
  (plist-put properties :eol-type eol-type)

  (cond
   ((eq type 'iso2022)
    (plist-put properties :flags
	       (list (and (or (consp (nth 0 flags))
			      (consp (nth 1 flags))
			      (consp (nth 2 flags))
			      (consp (nth 3 flags))) 'designation)
		     (or (nth 4 flags) 'long-form)
		     (and (nth 5 flags) 'ascii-at-eol)
		     (and (nth 6 flags) 'ascii-at-cntl)
		     (and (nth 7 flags) '7-bit)
		     (and (nth 8 flags) 'locking-shift)
		     (and (nth 9 flags) 'single-shift)
		     (and (nth 10 flags) 'use-roman)
		     (and (nth 11 flags) 'use-oldjis)
		     (or (nth 12 flags) 'direction)
		     (and (nth 13 flags) 'init-at-bol)
		     (and (nth 14 flags) 'designate-at-bol)
		     (and (nth 15 flags) 'safe)
		     (and (nth 16 flags) 'latin-extra)))
    (plist-put properties :designation
	       (let ((vec (make-vector 4 nil)))
		 (dotimes (i 4)
		   (let ((spec (nth i flags)))
		     (if (eq spec t)
			 (aset vec i '(94 96))
		     (if (consp spec)
			 (progn
			   (if (memq t spec)
			       (setq spec (append (delq t spec) '(94 96))))
			   (aset vec i spec))))))
		 vec)))

   ((eq type 'ccl)
    (plist-put properties :ccl-decoder (car flags))
    (plist-put properties :ccl-encoder (cdr flags))))

  (apply 'define-coding-system coding-system doc-string properties))

(make-obsolete 'make-coding-system 'define-coding-system "23.1")

(defun merge-coding-systems (first second)
  "Fill in any unspecified aspects of coding system FIRST from SECOND.
Return the resulting coding system."
  (let ((base (coding-system-base second))
	(eol (coding-system-eol-type second)))
    ;; If FIRST doesn't specify text conversion, merge with that of SECOND.
    (if (eq (coding-system-base first) 'undecided)
	(setq first (coding-system-change-text-conversion first base)))
    ;; If FIRST doesn't specify eol conversion, merge with that of SECOND.
    (if (and (vectorp (coding-system-eol-type first))
	     (numberp eol) (>= eol 0) (<= eol 2))
	(setq first (coding-system-change-eol-conversion
		     first eol)))
    first))

(defun autoload-coding-system (symbol form)
  "Define SYMBOL as a coding-system that is defined on demand.

FORM is a form to evaluate to define the coding-system."
  (put symbol 'coding-system-define-form form)
  (setq coding-system-alist (cons (list (symbol-name symbol))
				  coding-system-alist))
  (dolist (elt '("-unix" "-dos" "-mac"))
    (let ((name (concat (symbol-name symbol) elt)))
      (put (intern name) 'coding-system-define-form form)
      (setq coding-system-alist (cons (list name) coding-system-alist)))))

;; This variable is set in these three cases:
;;   (1) A file is read by a coding system specified explicitly.
;;       after-insert-file-set-coding sets the car of this value to
;;       coding-system-for-read, and sets the cdr to nil.
;;   (2) A buffer is saved.
;;       After writing, basic-save-buffer-1 sets the car of this value
;;       to last-coding-system-used.
;;   (3) set-buffer-file-coding-system is called.
;;       The cdr of this value is set to the specified coding system.
;; This variable is used for decoding in revert-buffer and encoding in
;; select-safe-coding-system.
(defvar buffer-file-coding-system-explicit nil
  "The file coding system explicitly specified for the current buffer.
The value is a cons of coding systems for reading (decoding) and
writing (encoding).
Internal use only.")
(make-variable-buffer-local 'buffer-file-coding-system-explicit)
(put 'buffer-file-coding-system-explicit 'permanent-local t)

(defun read-buffer-file-coding-system ()
  (let* ((bcss (find-coding-systems-region (point-min) (point-max)))
         (css-table
          (unless (equal bcss '(undecided))
            (append '("dos" "unix" "mac")
                    (delq nil (mapcar (lambda (cs)
                                        (if (memq (coding-system-base cs) bcss)
                                            (symbol-name cs)))
                                      coding-system-list)))))
         (combined-table
          (if css-table
              (completion-table-in-turn css-table coding-system-alist)
            coding-system-alist))
         (auto-cs
          (unless find-file-literally
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (funcall set-auto-coding-function
                         (or buffer-file-name "") (buffer-size))))))
         (preferred
          (let ((bfcs (default-value 'buffer-file-coding-system)))
            (cons (and (or (equal bcss '(undecided))
                           (memq (coding-system-base bfcs) bcss))
                       bfcs)
                  (mapcar (lambda (cs)
                            (and (coding-system-p cs)
                                 (coding-system-get cs :mime-charset)
                                 (or (equal bcss '(undecided))
                                     (memq (coding-system-base cs) bcss))
                                 cs))
                          (coding-system-priority-list)))))
         (default
           (let ((current (coding-system-base buffer-file-coding-system)))
             ;; Generally use as a default the first preferred coding-system
             ;; different from the current coding-system, except for
             ;; the case of auto-cs since choosing anything else is asking
             ;; for trouble (would lead to using a different coding
             ;; system than specified in the coding tag).
             (or auto-cs
                 (car (delq nil
                            (mapcar (lambda (cs)
                                      (if (eq current (coding-system-base cs))
                                          nil
                                        cs))
                                    preferred))))))
         (completion-ignore-case t)
         (completion-pcm--delim-wild-regex ; Let "u8" complete to "utf-8".
          (concat completion-pcm--delim-wild-regex
                  "\\|\\([[:alpha:]]\\)[[:digit:]]"))
         (cs (completing-read
              (format "Coding system for saving file (default %s): " default)
              combined-table
              nil t nil 'coding-system-history
              (if default (symbol-name default)))))
    (unless (zerop (length cs)) (intern cs))))

(defun set-buffer-file-coding-system (coding-system &optional force nomodify)
  "Set the file coding-system of the current buffer to CODING-SYSTEM.
This means that when you save the buffer, it will be converted
according to CODING-SYSTEM.  For a list of possible values of
CODING-SYSTEM, use \\[list-coding-systems].

If CODING-SYSTEM leaves the text conversion unspecified, or if it leaves
the end-of-line conversion unspecified, FORCE controls what to do.
If FORCE is nil, get the unspecified aspect (or aspects) from the buffer's
previous `buffer-file-coding-system' value (if it is specified there).
Otherwise, leave it unspecified.

This marks the buffer modified so that the succeeding \\[save-buffer]
surely saves the buffer with CODING-SYSTEM.  From a program, if you
don't want to mark the buffer modified, specify t for NOMODIFY.
If you know exactly what coding system you want to use,
just set the variable `buffer-file-coding-system' directly."
  (interactive
   (list (read-buffer-file-coding-system)
         current-prefix-arg))
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (setq coding-system
	    (merge-coding-systems coding-system buffer-file-coding-system)))
  (when (called-interactively-p 'interactive)
    ;; Check whether save would succeed, and jump to the offending char(s)
    ;; if not.
    (let ((css (find-coding-systems-region (point-min) (point-max))))
      (unless (or (eq (car css) 'undecided)
                  (memq (coding-system-base coding-system) css))
        (setq coding-system (select-safe-coding-system-interactively
                             (point-min) (point-max) css
                             (list coding-system))))))
  (setq buffer-file-coding-system coding-system)
  (if buffer-file-coding-system-explicit
      (setcdr buffer-file-coding-system-explicit coding-system)
    (setq buffer-file-coding-system-explicit (cons nil coding-system)))
  (unless nomodify
    (set-buffer-modified-p t))
  (force-mode-line-update))

(defun revert-buffer-with-coding-system (coding-system &optional force)
  "Visit the current buffer's file again using coding system CODING-SYSTEM.
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].

If CODING-SYSTEM leaves the text conversion unspecified, or if it leaves
the end-of-line conversion unspecified, FORCE controls what to do.
If FORCE is nil, get the unspecified aspect (or aspects) from the buffer's
previous `buffer-file-coding-system' value (if it is specified there).
Otherwise, determine it from the file contents as usual for visiting a file."
  (interactive "zCoding system for visited file (default nil): \nP")
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (setq coding-system
	    (merge-coding-systems coding-system buffer-file-coding-system)))
  (let ((coding-system-for-read coding-system))
    (revert-buffer)))

(defun set-file-name-coding-system (coding-system)
  "Set coding system for decoding and encoding file names to CODING-SYSTEM.
It actually just set the variable `file-name-coding-system' (which see)
to CODING-SYSTEM."
  (interactive "zCoding system for file names (default nil): ")
  (check-coding-system coding-system)
  (if (and coding-system
	   (not (coding-system-get coding-system :ascii-compatible-p))
	   (not (coding-system-get coding-system :suitable-for-file-name)))
      (error "%s is not suitable for file names" coding-system))
  (setq file-name-coding-system coding-system))

(defvar default-terminal-coding-system nil
  "Default value for the terminal coding system.
This is normally set according to the selected language environment.
See also the command `set-terminal-coding-system'.")

(defun set-terminal-coding-system (coding-system &optional terminal)
  "Set coding system of terminal output to CODING-SYSTEM.
All text output to TERMINAL will be encoded
with the specified coding system.

For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].
The default is determined by the selected language environment
or by the previous use of this command.

TERMINAL may be a terminal object, a frame, or nil for the
selected frame's terminal.  The setting has no effect on
graphical terminals."
  (interactive
   (list (let ((default (if (and (not (terminal-coding-system))
				 default-terminal-coding-system)
			    default-terminal-coding-system)))
	   (read-coding-system
	    (format "Coding system for terminal display (default %s): "
		    default)
	    default))))
  (if (and (not coding-system)
	   (not (terminal-coding-system)))
      (setq coding-system default-terminal-coding-system))
  (if coding-system
      (setq default-terminal-coding-system coding-system))
  (set-terminal-coding-system-internal coding-system terminal)
  (redraw-frame (selected-frame)))

(defvar default-keyboard-coding-system nil
  "Default value of the keyboard coding system.
This is normally set according to the selected language environment.
See also the command `set-keyboard-coding-system'.")

(defun set-keyboard-coding-system (coding-system &optional terminal)
  "Set coding system for keyboard input on TERMINAL to CODING-SYSTEM.

For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].
The default is determined by the selected language environment
or by the previous use of this command.

If CODING-SYSTEM is nil or the coding-type of CODING-SYSTEM is
`raw-text', the decoding of keyboard input is disabled.

TERMINAL may be a terminal object, a frame, or nil for the
selected frame's terminal.  The setting has no effect on
graphical terminals."
  (interactive
   (list (let* ((coding (keyboard-coding-system nil))
		(default (if (eq (coding-system-type coding) 'raw-text)
			     default-keyboard-coding-system)))
	   (read-coding-system
	    (format "Coding system for keyboard input (default %s): "
		    default)
	    default))))
  (let ((coding-type (coding-system-type coding-system))
	(saved-meta-mode
	 (terminal-parameter terminal 'keyboard-coding-saved-meta-mode)))
    (if (not (eq coding-type 'raw-text))
	(let (accept-8-bit)
	  (if (not (or (coding-system-get coding-system :suitable-for-keyboard)
		       (coding-system-get coding-system :ascii-compatible-p)))
	      (error "Unsuitable coding system for keyboard: %s" coding-system))
	  (cond ((memq coding-type '(charset utf-8 shift-jis big5 ccl))
		 (setq accept-8-bit t))
		((eq coding-type 'iso-2022)
		 (let ((flags (coding-system-get coding-system :flags)))
		   (or (memq '7-bit flags)
		       (setq accept-8-bit t))))
		(t
		 (error "Unsupported coding system for keyboard: %s"
			coding-system)))
	  (when accept-8-bit
	    (or saved-meta-mode
		(set-terminal-parameter terminal
					'keyboard-coding-saved-meta-mode
					(cons (nth 2 (current-input-mode))
					      nil)))
	    (set-input-meta-mode 8))
	  ;; Avoid end-of-line conversion.
	  (setq coding-system
		(coding-system-change-eol-conversion coding-system 'unix)))

      (when saved-meta-mode
	(set-input-meta-mode (car saved-meta-mode))
	(set-terminal-parameter terminal
				'keyboard-coding-saved-meta-mode
				nil))))
  (set-keyboard-coding-system-internal coding-system terminal)
  (setq keyboard-coding-system coding-system))

(defcustom keyboard-coding-system nil
  "Specify coding system for keyboard input.
If you set this on a terminal which can't distinguish Meta keys from
8-bit characters, you will have to use ESC to type Meta characters.
See Info node `Terminal Coding' and Info node `Unibyte Mode'.

On non-windowing terminals, this is set from the locale by default.

Setting this variable directly does not take effect;
use either \\[customize] or \\[set-keyboard-coding-system]."
  :type '(coding-system :tag "Coding system")
  :link '(info-link "(emacs)Terminal Coding")
  :link '(info-link "(emacs)Unibyte Mode")
  :set (lambda (symbol value)
	 ;; Don't load encoded-kb unnecessarily.
	 (if (or value (boundp 'encoded-kbd-setup-display))
	     (set-keyboard-coding-system value)
	   (set-default 'keyboard-coding-system nil))) ; must initialize
  :version "22.1"
  :group 'keyboard
  :group 'mule)

(defun set-buffer-process-coding-system (decoding encoding)
  "Set coding systems for the process associated with the current buffer.
DECODING is the coding system to be used to decode input from the process,
ENCODING is the coding system to be used to encode output to the process.

For a list of possible coding systems, use \\[list-coding-systems]."
  (interactive
   "zCoding-system for output from the process: \nzCoding-system for input to the process: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "No process")
      (check-coding-system decoding)
      (check-coding-system encoding)
      (set-process-coding-system proc decoding encoding)))
  (force-mode-line-update))

(defalias 'set-clipboard-coding-system 'set-selection-coding-system)

(defun set-selection-coding-system (coding-system)
  "Make CODING-SYSTEM used for communicating with other X clients.
When sending or receiving text via cut_buffer, selection, and clipboard,
the text is encoded or decoded by CODING-SYSTEM."
  (interactive "zCoding system for X selection: ")
  (check-coding-system coding-system)
  (setq selection-coding-system coding-system))

;; Coding system lastly specified by the command
;; set-next-selection-coding-system.
(defvar last-next-selection-coding-system nil)

(defun set-next-selection-coding-system (coding-system)
  "Use CODING-SYSTEM for next communication with other window system clients.
This setting is effective for the next communication only."
  (interactive
   (list (read-coding-system
	  (if last-next-selection-coding-system
	      (format "Coding system for the next selection (default %S): "
		      last-next-selection-coding-system)
	    "Coding system for the next selection: ")
	  last-next-selection-coding-system)))
  (if coding-system
      (setq last-next-selection-coding-system coding-system)
    (setq coding-system last-next-selection-coding-system))
  (check-coding-system coding-system)

  (setq next-selection-coding-system coding-system))

(defun set-coding-priority (arg)
  "Set priority of coding categories according to ARG.
ARG is a list of coding categories ordered by priority.

This function is provided for backward compatibility."
  (apply 'set-coding-system-priority
	 (mapcar #'(lambda (x) (symbol-value x)) arg)))
(make-obsolete 'set-coding-priority 'set-coding-system-priority "23.1")

;;; X selections

(defvar ctext-non-standard-encodings-alist
  (mapcar 'purecopy
  '(("big5-0" big5 2 big5)
    ("ISO8859-14" iso-8859-14 1 latin-iso8859-14)
    ("ISO8859-15" iso-8859-15 1 latin-iso8859-15)
    ("gbk-0" gbk 2 chinese-gbk)
    ("koi8-r" koi8-r 1 koi8-r)
    ("microsoft-cp1251" windows-1251 1 windows-1251)))
  "Alist of non-standard encoding names vs the corresponding usages in CTEXT.

It controls how extended segments of a compound text are handled
by the coding system `compound-text-with-extensions'.

Each element has the form (ENCODING-NAME CODING-SYSTEM N-OCTET CHARSET).

ENCODING-NAME is an encoding name of an \"extended segment\".

CODING-SYSTEM is the coding-system to encode (or decode) the
characters into (or from) the extended segment.

N-OCTET is the number of octets (bytes) that encodes a character
in the segment.  It can be 0 (meaning the number of octets per
character is variable), 1, 2, 3, or 4.

CHARSET is a character set containing characters that are encoded
in the segment.  It can be a list of character sets.

On decoding CTEXT, all encoding names listed here are recognized.

On encoding CTEXT, encoding names in the variable
`ctext-non-standard-encodings' (which see) and in the information
listed for the current language environment under the key
`ctext-non-standard-encodings' are used.")

(defvar ctext-non-standard-encodings nil
  "List of non-standard encoding names used in extended segments of CTEXT.
Each element must be one of the names listed in the variable
`ctext-non-standard-encodings-alist' (which see).")

(defvar ctext-non-standard-encodings-regexp
  (purecopy
  (string-to-multibyte
   (concat
    ;; For non-standard encodings.
    "\\(\e%/[0-4][\200-\377][\200-\377]\\([^\002]+\\)\002\\)"
    "\\|"
    ;; For UTF-8 encoding.
    "\\(\e%G[^\e]*\e%@\\)"))))

;; Functions to support "Non-Standard Character Set Encodings" defined
;; by the COMPOUND-TEXT spec.  They also support "The UTF-8 encoding"
;; described in the section 7 of the documentation of COMPOUND-TEXT
;; distributed with XFree86.

(defun ctext-post-read-conversion (len)
  "Decode LEN characters encoded as Compound Text with Extended Segments."
  ;; We don't need the following because it is expected that this
  ;; function is mainly used for decoding X selection which is not
  ;; that big data.
  ;;(buffer-disable-undo) ; minimize consing due to insertions and deletions
  (save-match-data
    (save-restriction
      (narrow-to-region (point) (+ (point) len))
      (let ((case-fold-search nil)
	    last-coding-system-used
	    pos bytes)
	(decode-coding-region (point-min) (point-max) 'ctext)
	(while (re-search-forward ctext-non-standard-encodings-regexp
				  nil 'move)
	  (setq pos (match-beginning 0))
	  (if (match-beginning 1)
	      ;; ESC % / [0-4] M L --ENCODING-NAME-- \002 --BYTES--
	      (let* ((M (multibyte-char-to-unibyte (char-after (+ pos 4))))
		     (L (multibyte-char-to-unibyte (char-after (+ pos 5))))
		     (encoding (match-string 2))
		     (encoding-info (assoc-string
				     encoding
				     ctext-non-standard-encodings-alist t))
		     (coding (if encoding-info
				 (nth 1 encoding-info)
			       (setq encoding (intern (downcase encoding)))
			       (and (coding-system-p encoding)
				    encoding))))
		(setq bytes (- (+ (* (- M 128) 128) (- L 128))
			       (- (point) (+ pos 6))))
		(when coding
		  (delete-region pos (point))
		  (forward-char bytes)
		  (decode-coding-region (- (point) bytes) (point) coding)))
	    ;; ESC % G --UTF-8-BYTES-- ESC % @
	    (delete-char -3)
	    (delete-region pos (+ pos 3))
	    (decode-coding-region pos (point) 'utf-8))))
      (goto-char (point-min))
      (- (point-max) (point)))))

(defvar ctext-standard-encodings
  '(ascii latin-jisx0201 katakana-jisx0201
	  latin-iso8859-1 latin-iso8859-2 latin-iso8859-3 latin-iso8859-4
	  greek-iso8859-7 arabic-iso8859-6 hebrew-iso8859-8 cyrillic-iso8859-5
	  latin-iso8859-9
	  chinese-gb2312 japanese-jisx0208 korean-ksc5601)
  "List of approved standard encodings (i.e. charsets) of X's Compound Text.
Coding-system `compound-text-with-extensions' encodes a character
belonging to any of those charsets using the normal ISO2022
designation sequence unless the current language environment or
the variable `ctext-non-standard-encodings' decide to use an extended
segment of CTEXT for that character.  See also the documentation
of `ctext-non-standard-encodings-alist'.")

;; Return an alist of CHARSET vs CTEXT-USAGE-INFO generated from
;; `ctext-non-standard-encodings' and a list specified by the key
;; `ctext-non-standard-encodings' for the current language
;; environment.  CTEXT-USAGE-INFO is one of the element of
;; `ctext-non-standard-encodings-alist' or nil.  In the former case, a
;; character in CHARSET is encoded using extended segment.  In the
;; latter case, a character in CHARSET is encoded using normal ISO2022
;; designation sequence.  If a character is not in any of CHARSETs, it
;; is encoded using UTF-8 encoding extension.

(defun ctext-non-standard-encodings-table ()
  (let* ((table (append ctext-non-standard-encodings
			(copy-sequence
			 (get-language-info current-language-environment
					    'ctext-non-standard-encodings))))
	 (tail table)
	 elt)
    (while tail
      (setq elt (car tail))
      (let* ((slot (assoc elt ctext-non-standard-encodings-alist))
	     (charset (nth 3 slot)))
	(if (charsetp charset)
	    (setcar tail
		    (cons (plist-get (charset-plist charset) :base) slot))
	  (setcar tail (cons (car charset) slot))
	  (dolist (cs (cdr charset))
	    (setcdr tail
		    (cons (cons (plist-get (charset-plist (car cs)) :base) slot)
			  (cdr tail)))
	    (setq tail (cdr tail))))
	(setq tail (cdr tail))))
    table))

(defun ctext-pre-write-conversion (from to)
  "Encode characters between FROM and TO as Compound Text w/Extended Segments.

If FROM is a string, generate a new temp buffer, insert the text,
and convert it in the temporary buffer.  Otherwise, convert
in-place."
  (save-match-data
    ;; Setup a working buffer if necessary.
    (when (stringp from)
      (set-buffer (generate-new-buffer " *temp"))
      (set-buffer-multibyte (multibyte-string-p from))
      (insert from)
      (setq from (point-min) to (point-max)))
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (let ((encoding-table (ctext-non-standard-encodings-table))
	    (charset-list (sort-charsets
			   (copy-sequence ctext-standard-encodings)))
	    (end-pos (make-marker))
	    last-coding-system-used
	    last-pos charset encoding-info)
	(dolist (elt encoding-table)
	  (push (car elt) charset-list))
	(setq end-pos (point-marker))
	(while (re-search-forward "[^\0-\177]+" nil t)
	  ;; Found a sequence of non-ASCII characters.
	  (set-marker end-pos (match-end 0))
	  (goto-char (match-beginning 0))
	  (setq last-pos (point)
		charset (char-charset (following-char) charset-list))
	  (forward-char 1)
	  (while (and (< (point) end-pos)
		      (eq charset (char-charset (following-char) charset-list)))
	    (forward-char 1))
	  (if charset
	      (if (setq encoding-info (cdr (assq charset encoding-table)))
		  ;; Encode this range using an extended segment.
		  (let ((encoding-name (car encoding-info))
			(coding-system (nth 1 encoding-info))
			(noctets (nth 2 encoding-info))
			len)
		    (encode-coding-region last-pos (point) coding-system)
		    (setq len (+ (length encoding-name) 1
				 (- (point) last-pos)))
		    ;; According to the spec of CTEXT, it is not
		    ;; necessary to produce this extra designation
		    ;; sequence, but some buggy application
		    ;; (e.g. crxvt-gb) requires it.
		    (insert "\e(B")
		    (save-excursion
		      (goto-char last-pos)
		      (insert (format "\e%%/%d" noctets))
		      (insert-byte (+ (/ len 128) 128) 1)
		      (insert-byte (+ (% len 128) 128) 1)
		      (insert encoding-name)
		      (insert 2)))
		;; Encode this range as characters in CHARSET.
		(put-text-property last-pos (point) 'charset charset))
	    ;; Encode this range using UTF-8 encoding extension.
	    (encode-coding-region last-pos (point) 'mule-utf-8)
	    (save-excursion
	      (goto-char last-pos)
	      (insert "\e%G"))
	    (insert "\e%@")))
	(goto-char (point-min)))))
  ;; Must return nil, as build_annotations_2 expects that.
  nil)

;;; FILE I/O

;; TODO many elements of this list are also in inhibit-local-variables-regexps.
(defcustom auto-coding-alist
  ;; .exe and .EXE are added to support archive-mode looking at DOS
  ;; self-extracting exe archives.
  (mapcar (lambda (arg) (cons (purecopy (car arg)) (cdr arg)))
	  '(("\\.\\(\
arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|7z\\|\
ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|7Z\\)\\'"
     . no-conversion-multibyte)
    ("\\.\\(exe\\|EXE\\)\\'" . no-conversion)
    ("\\.\\(sx[dmicw]\\|odt\\|tar\\|t[bg]z\\)\\'" . no-conversion)
    ("\\.\\(gz\\|Z\\|bz\\|bz2\\|xz\\|gpg\\)\\'" . no-conversion)
    ("\\.\\(jpe?g\\|png\\|gif\\|tiff?\\|p[bpgn]m\\)\\'" . no-conversion)
    ("\\.pdf\\'" . no-conversion)
    ("/#[^/]+#\\'" . emacs-mule)))
  "Alist of filename patterns vs corresponding coding systems.
Each element looks like (REGEXP . CODING-SYSTEM).
A file whose name matches REGEXP is decoded by CODING-SYSTEM on reading.

The settings in this alist take priority over `coding:' tags
in the file (see the function `set-auto-coding')
and the contents of `file-coding-system-alist'."
  :version "24.1"                       ; added xz
  :group 'files
  :group 'mule
  :type '(repeat (cons (regexp :tag "File name regexp")
		       (symbol :tag "Coding system"))))

(defcustom auto-coding-regexp-alist
  (mapcar (lambda (arg) (cons (purecopy (car arg)) (cdr arg)))
  '(("\\`BABYL OPTIONS:[ \t]*-\\*-[ \t]*rmail[ \t]*-\\*-" . no-conversion)
    ("\\`\xFE\xFF" . utf-16be-with-signature)
    ("\\`\xFF\xFE" . utf-16le-with-signature)
    ("\\`\xEF\xBB\xBF" . utf-8-with-signature)
    ("\\`;ELC\024\0\0\0" . emacs-mule)))	; Emacs 20-compiled
  "Alist of patterns vs corresponding coding systems.
Each element looks like (REGEXP . CODING-SYSTEM).
A file whose first bytes match REGEXP is decoded by CODING-SYSTEM on reading.

The settings in this alist take priority over `coding:' tags
in the file (see the function `set-auto-coding')
and the contents of `file-coding-system-alist'."
  :group 'files
  :group 'mule
  :type '(repeat (cons (regexp :tag "Regexp")
		       (symbol :tag "Coding system"))))

(defun auto-coding-regexp-alist-lookup (from to)
  "Lookup `auto-coding-regexp-alist' for the contents of the current buffer.
The value is a coding system is specified for the region FROM and TO,
or nil."
  (save-excursion
    (goto-char from)
    (let ((alist auto-coding-regexp-alist)
	  coding-system)
      (while (and alist (not coding-system))
	(let ((regexp (car (car alist))))
	  (if enable-multibyte-characters
	      (setq regexp (string-to-multibyte regexp)))
	  (if (re-search-forward regexp to t)
	      (setq coding-system (cdr (car alist)))
	    (setq alist (cdr alist)))))
      coding-system)))

;; See the bottom of this file for built-in auto coding functions.
(defcustom auto-coding-functions '(sgml-xml-auto-coding-function
				   sgml-html-meta-auto-coding-function)
  "A list of functions which attempt to determine a coding system.

Each function in this list should be written to operate on the
current buffer, but should not modify it in any way.  The buffer
will contain undecoded text of parts of the file.  Each function
should take one argument, SIZE, which says how many characters
\(starting from point) it should look at.

If one of these functions succeeds in determining a coding
system, it should return that coding system.  Otherwise, it
should return nil.

If a file has a `coding:' tag, that takes precedence over these
functions, so they won't be called at all."
  :group 'files
  :group 'mule
  :type '(repeat function))

(defvar set-auto-coding-for-load nil
  "Non-nil means respect a \"unibyte: t\" entry in file local variables.
Emacs binds this variable to t when loading or byte-compiling Emacs Lisp
files.")

(defun auto-coding-alist-lookup (filename)
  "Return the coding system specified by `auto-coding-alist' for FILENAME."
  (let ((alist auto-coding-alist)
	(case-fold-search (memq system-type '(windows-nt ms-dos cygwin)))
	coding-system)
    (while (and alist (not coding-system))
      (if (string-match (car (car alist)) filename)
	  (setq coding-system (cdr (car alist)))
	(setq alist (cdr alist))))
    coding-system))

(put 'enable-character-translation 'permanent-local t)
(put 'enable-character-translation 'safe-local-variable	'booleanp)

(defun find-auto-coding (filename size)
  "Find a coding system for a file FILENAME of which SIZE bytes follow point.
These bytes should include at least the first 1k of the file
and the last 3k of the file, but the middle may be omitted.

The function checks FILENAME against the variable `auto-coding-alist'.
If FILENAME doesn't match any entries in the variable, it checks the
contents of the current buffer following point against
`auto-coding-regexp-alist'.  If no match is found, it checks for a
`coding:' tag in the first one or two lines following point.  If no
`coding:' tag is found, it checks any local variables list in the last
3K bytes out of the SIZE bytes.  Finally, if none of these methods
succeed, it checks to see if any function in `auto-coding-functions'
gives a match.

If a coding system is specified, the return value is a cons
\(CODING . SOURCE), where CODING is the specified coding system and
SOURCE is a symbol `auto-coding-alist', `auto-coding-regexp-alist',
`:coding', or `auto-coding-functions' indicating by what CODING is
specified.  Note that the validity of CODING is not checked;
it's the caller's responsibility to check it.

If nothing is specified, the return value is nil."
  (or (let ((coding-system (auto-coding-alist-lookup filename)))
	(if coding-system
	    (cons coding-system 'auto-coding-alist)))
      ;; Try using `auto-coding-regexp-alist'.
      (let ((coding-system (auto-coding-regexp-alist-lookup (point)
							    (+ (point) size))))
	(if coding-system
	    (cons coding-system 'auto-coding-regexp-alist)))
      (let* ((case-fold-search t)
	     (head-start (point))
	     (head-end (+ head-start (min size 1024)))
	     (tail-start (+ head-start (max (- size 3072) 0)))
	     (tail-end (+ head-start size))
	     coding-system head-found tail-found pos char-trans)
	;; Try a short cut by searching for the string "coding:"
	;; and for "unibyte:" at the head and tail of SIZE bytes.
	(setq head-found (or (search-forward "coding:" head-end t)
			     (search-forward "unibyte:" head-end t)
			     (search-forward "enable-character-translation:"
					     head-end t)))
	(if (and head-found (> head-found tail-start))
	    ;; Head and tail are overlapped.
	    (setq tail-found head-found)
	  (goto-char tail-start)
	  (setq tail-found (or (search-forward "coding:" tail-end t)
			       (search-forward "unibyte:" tail-end t)
			       (search-forward "enable-character-translation:"
					       tail-end t))))

	;; At first check the head.
	(when head-found
	  (goto-char head-start)
	  (setq head-end (set-auto-mode-1))
	  (setq head-start (point))
	  (when (and head-end (< head-found head-end))
	    (goto-char head-start)
	    (when (and set-auto-coding-for-load
		       (re-search-forward
			"\\(.*;\\)?[ \t]*unibyte:[ \t]*\\([^ ;]+\\)"
			head-end t))
	      (setq coding-system 'raw-text))
	    (when (and (not coding-system)
		       (re-search-forward
			"\\(.*;\\)?[ \t]*coding:[ \t]*\\([^ ;]+\\)"
			head-end t))
	      (setq coding-system (intern (match-string 2))))
	    (when (re-search-forward
		   "\\(.*;\\)?[ \t]*enable-character-translation:[ \t]*\\([^ ;]+\\)"
		   head-end t)
	      (setq char-trans (match-string 2)))))

	;; If no coding: tag in the head, check the tail.
	;; Here we must pay attention to the case that the end-of-line
	;; is just "\r" and we can't use "^" nor "$" in regexp.
	(when (and tail-found (or (not coding-system) (not char-trans)))
	  (goto-char tail-start)
	  (re-search-forward "[\r\n]\^L" tail-end t)
	  (if (re-search-forward
	       "[\r\n]\\([^[\r\n]*\\)[ \t]*Local Variables:[ \t]*\\([^\r\n]*\\)[\r\n]"
	       tail-end t)
	      ;; The prefix is what comes before "local variables:" in its
	      ;; line.  The suffix is what comes after "local variables:"
	      ;; in its line.
	      (let* ((prefix (regexp-quote (match-string 1)))
		     (suffix (regexp-quote (match-string 2)))
		     (re-coding
		      (concat
		       "[\r\n]" prefix
		       ;; N.B. without the \n below, the regexp can
		       ;; eat newlines.
		       "[ \t]*coding[ \t]*:[ \t]*\\([^ \t\r\n]+\\)[ \t]*"
		       suffix "[\r\n]"))
		     (re-unibyte
		      (concat
		       "[\r\n]" prefix
		       "[ \t]*unibyte[ \t]*:[ \t]*\\([^ \t\r\n]+\\)[ \t]*"
		       suffix "[\r\n]"))
		     (re-char-trans
		      (concat
		       "[\r\n]" prefix
		       "[ \t]*enable-character-translation[ \t]*:[ \t]*\\([^ \t\r\n]+\\)[ \t]*"
		       suffix "[\r\n]"))
		     (re-end
		      (concat "[\r\n]" prefix "[ \t]*End *:[ \t]*" suffix
			      "[\r\n]?"))
		     (pos (1- (point))))
		(forward-char -1)	; skip back \r or \n.
		(re-search-forward re-end tail-end 'move)
		(setq tail-end (point))
		(goto-char pos)
		(when (and set-auto-coding-for-load
			   (re-search-forward re-unibyte tail-end t))
		  (setq coding-system 'raw-text))
		(when (and (not coding-system)
			   (re-search-forward re-coding tail-end t))
		  (setq coding-system (intern (match-string 1))))
		(when (and (not char-trans)
			   (re-search-forward re-char-trans tail-end t))
		  (setq char-trans (match-string 1))))))
	(if coding-system
	    ;; If the coding-system name ends with "!", remove it and
	    ;; set char-trans to "nil".
	    (let ((name (symbol-name coding-system)))
	      (if (= (aref name (1- (length name))) ?!)
		  (setq coding-system (intern (substring name 0 -1))
			char-trans "nil"))))
	(when (and char-trans
		   (not (setq char-trans (intern char-trans))))
	  (make-local-variable 'enable-character-translation)
	  (setq enable-character-translation nil))
	(if coding-system
	    (cons coding-system :coding)))
      ;; Finally, try all the `auto-coding-functions'.
      (let ((funcs auto-coding-functions)
	    (coding-system nil))
	(while (and funcs (not coding-system))
	  (setq coding-system (condition-case e
				  (save-excursion
				    (goto-char (point-min))
				    (funcall (pop funcs) size))
				(error nil))))
	(if coding-system
	    (cons coding-system 'auto-coding-functions)))))

(defun set-auto-coding (filename size)
  "Return coding system for a file FILENAME of which SIZE bytes follow point.
See `find-auto-coding' for how the coding system is found.
Return nil if an invalid coding system is found.

The variable `set-auto-coding-function' (which see) is set to this
function by default."
  (let ((found (find-auto-coding filename size)))
    (if (and found (coding-system-p (car found)))
	(car found))))

(setq set-auto-coding-function 'set-auto-coding)

(defun after-insert-file-set-coding (inserted &optional visit)
  "Set `buffer-file-coding-system' of current buffer after text is inserted.
INSERTED is the number of characters that were inserted, as figured
in the situation before this function.  Return the number of characters
inserted, as figured in the situation after.  The two numbers can be
different if the buffer has become unibyte.
The optional second arg VISIT non-nil means that we are visiting a file."
  (if (and visit
	   coding-system-for-read
	   (not (eq coding-system-for-read 'auto-save-coding)))
      (setq buffer-file-coding-system-explicit
	    (cons coding-system-for-read nil)))
  (if last-coding-system-used
      (let ((coding-system
	     (find-new-buffer-file-coding-system last-coding-system-used)))
	(if coding-system
	    (setq buffer-file-coding-system coding-system))))
  inserted)

;; The coding-spec and eol-type of coding-system returned is decided
;; independently in the following order.
;;	1. That of buffer-file-coding-system locally bound.
;;	2. That of CODING.

(defun find-new-buffer-file-coding-system (coding)
  "Return a coding system for a buffer when a file of CODING is inserted.
The local variable `buffer-file-coding-system' of the current buffer
is set to the returned value.
Return nil if there's no need to set `buffer-file-coding-system'."
  (let (local-coding local-eol
	found-coding found-eol
	new-coding new-eol)
    (if (null coding)
	;; Nothing found about coding.
	nil

      ;; Get information of `buffer-file-coding-system' in LOCAL-EOL
      ;; and LOCAL-CODING.
      (setq local-eol (coding-system-eol-type buffer-file-coding-system))
      (if (null (numberp local-eol))
	  ;; But eol-type is not yet set.
	  (setq local-eol nil))
      (if (and buffer-file-coding-system
	       (not (eq (coding-system-type buffer-file-coding-system)
			'undecided)))
	  (setq local-coding (coding-system-base buffer-file-coding-system)))

      (if (and (local-variable-p 'buffer-file-coding-system)
	       local-eol local-coding)
	  ;; The current buffer has already set full coding-system, we
	  ;; had better not change it.
	  nil

	(setq found-eol (coding-system-eol-type coding))
	(if (null (numberp found-eol))
  	    ;; But eol-type is not found.
	    ;; If EOL conversions are inhibited, force unix eol-type.
	    (setq found-eol (if inhibit-eol-conversion 0)))
	(setq found-coding (coding-system-base coding))

	(if (and (not found-eol) (eq found-coding 'undecided))
	    ;; No valid coding information found.
	    nil

	  ;; Some coding information (eol or text) found.

	  ;; The local setting takes precedence over the found one.
	  (setq new-coding (if (local-variable-p 'buffer-file-coding-system)
			       (or local-coding found-coding)
			     (or found-coding local-coding)))
	  (setq new-eol (if (local-variable-p 'buffer-file-coding-system)
			    (or local-eol found-eol)
			  (or found-eol local-eol)))

	  (let ((eol-type (coding-system-eol-type new-coding)))
	    (if (and (numberp new-eol) (vectorp eol-type))
		(aref eol-type new-eol)
	      new-coding)))))))

(defun modify-coding-system-alist (target-type regexp coding-system)
  "Modify one of look up tables for finding a coding system on I/O operation.
There are three of such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.

TARGET-TYPE specifies which of them to modify.
If it is `file', it affects `file-coding-system-alist' (which see).
If it is `process', it affects `process-coding-system-alist' (which see).
If it is `network', it affects `network-coding-system-alist' (which see).

REGEXP is a regular expression matching a target of I/O operation.
The target is a file name if TARGET-TYPE is `file', a program name if
TARGET-TYPE is `process', or a network service name or a port number
to connect to if TARGET-TYPE is `network'.

CODING-SYSTEM is a coding system to perform code conversion on the I/O
operation, or a cons cell (DECODING . ENCODING) specifying the coding
systems for decoding and encoding respectively, or a function symbol
which, when called, returns such a cons cell."
  (or (memq target-type '(file process network))
      (error "Invalid target type: %s" target-type))
  (or (stringp regexp)
      (and (eq target-type 'network) (integerp regexp))
      (error "Invalid regular expression: %s" regexp))
  (if (symbolp coding-system)
      (if (not (fboundp coding-system))
	  (progn
	    (check-coding-system coding-system)
	    (setq coding-system (cons coding-system coding-system))))
    (check-coding-system (car coding-system))
    (check-coding-system (cdr coding-system)))
  (cond ((eq target-type 'file)
	 (let ((slot (assoc regexp file-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq file-coding-system-alist
		   (cons (cons regexp coding-system)
			 file-coding-system-alist)))))
	((eq target-type 'process)
	 (let ((slot (assoc regexp process-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq process-coding-system-alist
		   (cons (cons regexp coding-system)
			 process-coding-system-alist)))))
	(t
	 (let ((slot (assoc regexp network-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq network-coding-system-alist
		   (cons (cons regexp coding-system)
			 network-coding-system-alist)))))))

(defun decode-coding-inserted-region (from to filename
					   &optional visit beg end replace)
  "Decode the region between FROM and TO as if it is read from file FILENAME.
The idea is that the text between FROM and TO was just inserted somehow.
Optional arguments VISIT, BEG, END, and REPLACE are the same as those
of the function `insert-file-contents'.
Part of the job of this function is setting `buffer-undo-list' appropriately."
  (save-excursion
    (save-restriction
      (let ((coding coding-system-for-read)
	    undo-list-saved)
	(if visit
	    ;; Temporarily turn off undo recording, if we're decoding the
	    ;; text of a visited file.
	    (setq buffer-undo-list t)
	  ;; Otherwise, if we can recognize the undo elt for the insertion,
	  ;; remove it and get ready to replace it later.
	  ;; In the mean time, turn off undo recording.
	  (let ((last (car-safe buffer-undo-list)))
	    (if (and (consp last) (eql (car last) from) (eql (cdr last) to))
		(setq undo-list-saved (cdr buffer-undo-list)
		      buffer-undo-list t))))
	(narrow-to-region from to)
	(goto-char (point-min))
	(or coding
	    (setq coding (funcall set-auto-coding-function
				  filename (- (point-max) (point-min)))))
	(or coding
	    (setq coding (car (find-operation-coding-system
			       'insert-file-contents
			       (cons filename (current-buffer))
			       visit beg end replace))))
	(if (coding-system-p coding)
	    (or enable-multibyte-characters
		(setq coding
		      (coding-system-change-text-conversion coding 'raw-text)))
	  (setq coding nil))
	(if coding
	    (decode-coding-region (point-min) (point-max) coding)
	  (setq last-coding-system-used coding))
	;; If we're decoding the text of a visited file,
	;; the undo list should start out empty.
	(if visit
	    (setq buffer-undo-list nil)
	  ;; If we decided to replace the undo entry for the insertion,
	  ;; do so now.
	  (if undo-list-saved
	      (setq buffer-undo-list
		    (cons (cons from (point-max)) undo-list-saved))))))))

(defun recode-region (start end new-coding coding)
  "Re-decode the region (previously decoded by CODING) by NEW-CODING."
  (interactive
   (list (region-beginning) (region-end)
	 (read-coding-system "Text was really in: ")
	 (let ((coding (or buffer-file-coding-system last-coding-system-used)))
	   (read-coding-system
	    (concat "But was interpreted as"
		    (if coding (format " (default %S): " coding) ": "))
	    coding))))
  (or (and new-coding coding)
      (error "Coding system not specified"))
  ;; Check it before we encode the region.
  (check-coding-system new-coding)
  (save-restriction
    (narrow-to-region start end)
    (encode-coding-region (point-min) (point-max) coding)
    (decode-coding-region (point-min) (point-max) new-coding))
  (if (region-active-p)
      (deactivate-mark)))

(defun make-translation-table (&rest args)
  "Make a translation table from arguments.
A translation table is a char table intended for character
translation in CCL programs.

Each argument is a list of elements of the form (FROM . TO), where FROM
is a character to be translated to TO.

The arguments and forms in each argument are processed in the given
order, and if a previous form already translates TO to some other
character, say TO-ALT, FROM is also translated to TO-ALT."
  (let ((table (make-char-table 'translation-table))
	revlist)
    (dolist (elts args)
      (dolist (elt elts)
	(let ((from (car elt))
	      (to (cdr elt))
	      to-alt rev-from rev-to)
	  ;; If we have already translated TO to TO-ALT, FROM should
	  ;; also be translated to TO-ALT.
	  (if (setq to-alt (aref table to))
	      (setq to to-alt))
	  (aset table from to)
	  ;; If we have already translated some chars to FROM, they
	  ;; should also be translated to TO.
	  (when (setq rev-from (assq from revlist))
	    (dolist (elt (cdr rev-from))
	      (aset table elt to))
	    (setq revlist (delq rev-from revlist)
		  rev-from (cdr rev-from)))
	  ;; Now update REVLIST.
	  (setq rev-to (assq to revlist))
	  (if rev-to
	      (setcdr rev-to (cons from (cdr rev-to)))
	    (setq rev-to (list to from)
		  revlist (cons rev-to revlist)))
	  (if rev-from
	      (setcdr rev-to (append rev-from (cdr rev-to)))))))
    ;; Return TABLE just created.
    (set-char-table-extra-slot table 1 1)
    table))

(defun make-translation-table-from-vector (vec)
  "Make translation table from decoding vector VEC.
VEC is an array of 256 elements to map unibyte codes to multibyte
characters.  Elements may be nil for undefined code points."
  (let ((table (make-char-table 'translation-table))
	(rev-table (make-char-table 'translation-table))
	ch)
    (dotimes (i 256)
      (setq ch (aref vec i))
      (when ch
	(aset table i ch)
	(if (>= ch 256)
	    (aset rev-table ch i))))
    (set-char-table-extra-slot table 0 rev-table)
    (set-char-table-extra-slot table 1 1)
    (set-char-table-extra-slot rev-table 1 1)
    table))

(defun make-translation-table-from-alist (alist)
  "Make translation table from N<->M mapping in ALIST.
ALIST is an alist, each element has the form (FROM . TO).
FROM and TO are a character or a vector of characters.
If FROM is a character, that character is translated to TO.
If FROM is a vector of characters, that sequence is translated to TO.
The first extra-slot of the value is a translation table for reverse mapping."
  (let ((tables (vector (make-char-table 'translation-table)
			(make-char-table 'translation-table)))
	table max-lookup from to idx val)
    (dotimes (i 2)
      (setq table (aref tables i))
      (setq max-lookup 1)
      (dolist (elt alist)
	(if (= i 0)
	    (setq from (car elt) to (cdr elt))
	  (setq from (cdr elt) to (car elt)))
	(if (characterp from)
	    (setq idx from)
	  (setq idx (aref from 0)
		max-lookup (max max-lookup (length from))))
	(setq val (aref table idx))
	(if val
	    (progn
	      (or (consp val)
		  (setq val (list (cons (vector idx) val))))
	      (if (characterp from)
		  (setq from (vector from)))
	      (setq val (nconc val (list (cons from to)))))
	  (if (characterp from)
	      (setq val to)
	    (setq val (list (cons from to)))))
	(aset table idx val))
      (set-char-table-extra-slot table 1 max-lookup))
    (set-char-table-extra-slot (aref tables 0) 0 (aref tables 1))
    (aref tables 0)))

(defun define-translation-table (symbol &rest args)
  "Define SYMBOL as the name of translation table made by ARGS.
This sets up information so that the table can be used for
translations in a CCL program.

If the first element of ARGS is a char-table whose purpose is
`translation-table', just define SYMBOL to name it.  (Note that this
function does not bind SYMBOL.)

Any other ARGS should be suitable as arguments of the function
`make-translation-table' (which see).

This function sets properties `translation-table' and
`translation-table-id' of SYMBOL to the created table itself and the
identification number of the table respectively.  It also registers
the table in `translation-table-vector'."
  (let ((table (if (and (char-table-p (car args))
			(eq (char-table-subtype (car args))
			    'translation-table))
		   (car args)
		 (apply 'make-translation-table args)))
	(len (length translation-table-vector))
	(id 0)
	(done nil))
    (put symbol 'translation-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-table-vector
		(vconcat translation-table-vector (make-vector len nil))))
      (let ((slot (aref translation-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-table-id id)
    id))

(defun translate-region (start end table)
  "From START to END, translate characters according to TABLE.
TABLE is a string or a char-table.
If TABLE is a string, the Nth character in it is the mapping
for the character with code N.
If TABLE is a char-table, the element for character N is the mapping
for the character with code N.
It returns the number of characters changed."
  (interactive
   (list (region-beginning)
	 (region-end)
	 (let (table l)
	   (dotimes (i (length translation-table-vector))
	     (if (consp (aref translation-table-vector i))
		 (push (list (symbol-name
			      (car (aref translation-table-vector i)))) l)))
	   (if (not l)
	       (error "No translation table defined"))
	   (while (not table)
	     (setq table (completing-read "Translation table: " l nil t)))
	   (intern table))))
  (if (symbolp table)
      (let ((val (get table 'translation-table)))
	(or (char-table-p val)
	    (error "Invalid translation table name: %s" table))
	(setq table val)))
  (translate-region-internal start end table))

(defmacro with-category-table (table &rest body)
  "Execute BODY like `progn' with TABLE the current category table.
The category table of the current buffer is saved, BODY is evaluated,
then the saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (declare (indent 1) (debug t))
  (let ((old-table (make-symbol "old-table"))
	(old-buffer (make-symbol "old-buffer")))
    `(let ((,old-table (category-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-category-table ,table)
	     ,@body)
	 (with-current-buffer ,old-buffer
	   (set-category-table ,old-table))))))

(defun define-translation-hash-table (symbol table)
  "Define SYMBOL as the name of the hash translation TABLE for use in CCL.

Analogous to `define-translation-table', but updates
`translation-hash-table-vector' and the table is for use in the CCL
`lookup-integer' and `lookup-character' functions."
  (unless (and (symbolp symbol)
	       (hash-table-p table))
    (error "Bad args to define-translation-hash-table"))
  (let ((len (length translation-hash-table-vector))
	(id 0)
	done)
    (put symbol 'translation-hash-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-hash-table-vector
		(vconcat translation-hash-table-vector [nil])))
      (let ((slot (aref translation-hash-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-hash-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-hash-table-id id)
    id))

;;; Initialize some variables.

(put 'use-default-ascent 'char-table-extra-slots 0)
(setq use-default-ascent (make-char-table 'use-default-ascent))
(put 'ignore-relative-composition 'char-table-extra-slots 0)
(setq ignore-relative-composition
      (make-char-table 'ignore-relative-composition))

(make-obsolete 'set-char-table-default
	       "generic characters no longer exist." "23.1")

;;; Built-in auto-coding-functions:

(defun sgml-xml-auto-coding-function (size)
  "Determine whether the buffer is XML, and if so, its encoding.
This function is intended to be added to `auto-coding-functions'."
  (setq size (+ (point) size))
  (when (re-search-forward "\\`[[:space:]\n]*<\\?xml" size t)
    (let ((end (save-excursion
		 ;; This is a hack.
		 (re-search-forward "[\"']\\s-*\\?>" size t))))
      (when end
	(if (re-search-forward "encoding=[\"']\\(.+?\\)[\"']" end t)
	    (let* ((match (match-string 1))
		   (sym (intern (downcase match))))
	      (if (coding-system-p sym)
		  sym
		(message "Warning: unknown coding system \"%s\"" match)
		nil))
          ;; Files without an encoding tag should be UTF-8. But users
          ;; may be naive about encodings, and have saved the file from
          ;; another editor that does not help them get the encoding right.
          ;; Detect the encoding and warn the user if it is detected as
          ;; something other than UTF-8.
	  (let ((detected
                 (with-coding-priority '(utf-8)
                   (coding-system-base
                    (detect-coding-region (point-min) size t)))))
            ;; Pure ASCII always comes back as undecided.
            (if (memq detected '(utf-8 undecided))
                'utf-8
              (warn "File contents detected as %s.
  Consider adding an encoding attribute to the xml declaration,
  or saving as utf-8, as mandated by the xml specification." detected)
              detected)))))))

(defun sgml-html-meta-auto-coding-function (size)
  "If the buffer has an HTML meta tag, use it to determine encoding.
This function is intended to be added to `auto-coding-functions'."
  (let ((case-fold-search t))
    (setq size (min (+ (point) size)
		    (save-excursion
		      ;; Limit the search by the end of the HTML header.
		      (or (search-forward "</head>" (+ (point) size) t)
			  ;; In case of no header, search only 10 lines.
			  (forward-line 10))
		      (point))))
    ;; Make sure that the buffer really contains an HTML document, by
    ;; checking that it starts with a doctype or a <HTML> start tag
    ;; (allowing for whitespace at bob).  Note: 'DOCTYPE NETSCAPE' is
    ;; useful for Mozilla bookmark files.
    (when (and (re-search-forward "\\`[[:space:]\n]*\\(<!doctype[[:space:]\n]+\\(html\\|netscape\\)\\|<html\\)" size t)
	       (re-search-forward "<meta\\s-+\\(http-equiv=[\"']?content-type[\"']?\\s-+content=[\"']text/\\sw+;\\s-*\\)?charset=[\"']?\\(.+?\\)[\"'\\s-/>]" size t))
      (let* ((match (match-string 2))
	     (sym (intern (downcase match))))
	(if (coding-system-p sym)
	    sym
	  (message "Warning: unknown coding system \"%s\"" match)
	  nil)))))

(defun xml-find-file-coding-system (args)
  "Determine the coding system of an XML file without a declaration.
Strictly speaking, the file should be utf-8, but mistakes are
made, and there are genuine cases where XML fragments are saved,
with the encoding properly specified in a master document, or
added by processing software."
  (if (eq (car args) 'insert-file-contents)
      (let ((detected
             (with-coding-priority '(utf-8)
               (coding-system-base
                (detect-coding-region (point-min) (point-max) t)))))
        ;; Pure ASCII always comes back as undecided.
        (cond
         ((memq detected '(utf-8 undecided))
          'utf-8)
         ((eq detected 'utf-16le-with-signature) 'utf-16le-with-signature)
         ((eq detected 'utf-16be-with-signature) 'utf-16be-with-signature)
         (t
          (warn "File contents detected as %s.
  Consider adding an xml declaration with the encoding specified,
  or saving as utf-8, as mandated by the xml specification." detected)
          detected)))
    ;; Don't interfere with the user's wishes for saving the buffer.
    ;; We did what we could when the buffer was created to ensure the
    ;; correct encoding was used, or the user was warned, so any
    ;; non-conformity here is deliberate on the part of the user.
    'undecided))

;;;
(provide 'mule)

;;; mule.el ends here
