;;; bindat.el --- binary data structure packing and unpacking.

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Assignment name: struct.el
;; Keywords: comm data processes

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

;;  Packing and unpacking of (binary) data structures.
;;
;;  The data formats used in binary files and network protocols are
;;  often structed data which can be described by a C-style structure
;;  such as the one shown below.  Using the bindat package, decoding
;;  and encoding binary data formats like these is made simple using a
;;  structure specification which closely resembles the C style
;;  structure declarations.
;;
;;  Encoded (binary) data is stored in a unibyte string or vector,
;;  while the decoded data is stored in an alist with (FIELD . VALUE)
;;  pairs.

;; Example:

;;  Consider the following C structures:
;;
;;  struct header {
;;	unsigned long	dest_ip;
;;	unsigned long	src_ip;
;;	unsigned short	dest_port;
;;	unsigned short	src_port;
;;  };
;;
;;  struct data {
;;	unsigned char	type;
;;	unsigned char	opcode;
;;	unsigned long	length;  /* In little endian order */
;;	unsigned char	id[8];   /* nul-terminated string  */
;;	unsigned char	data[/* (length + 3) & ~3 */];
;;  };
;;
;;  struct packet {
;;	struct header	header;
;;	unsigned char	items;
;;	unsigned char   filler[3];
;;	struct data	item[/* items */];
;;  };
;;
;;  The corresponding Lisp bindat specification looks like this:
;;
;;  (setq header-bindat-spec
;;    '((dest-ip   ip)
;;	(src-ip    ip)
;;	(dest-port u16)
;;	(src-port  u16)))
;;
;;  (setq data-bindat-spec
;;    '((type      u8)
;;	(opcode	   u8)
;;	(length	   u16r)  ;; little endian order
;;	(id	   strz 8)
;;	(data	   vec (length))
;;	(align     4)))
;;
;;  (setq packet-bindat-spec
;;    '((header    struct header-bindat-spec)
;;	(items     u8)
;;	(fill      3)
;;	(item	   repeat (items)
;;		   (struct data-bindat-spec))))
;;
;;
;;  A binary data representation may look like
;;   [ 192 168 1 100 192 168 1 101 01 28 21 32 2 0 0 0
;;     2 3 5 0 ?A ?B ?C ?D ?E ?F 0 0 1 2 3 4 5 0 0 0
;;     1 4 7 0 ?B ?C ?D ?E ?F ?G 0 0 6 7 8 9 10 11 12 0 ]
;;
;;  The corresponding decoded structure looks like
;;
;;      ((header
;;        (dest-ip   . [192 168 1 100])
;;        (src-ip    . [192 168 1 101])
;;        (dest-port . 284)
;;        (src-port  . 5408))
;;       (items . 2)
;;       (item ((data . [1 2 3 4 5])
;;      	(id . "ABCDEF")
;;      	(length . 5)
;;      	(opcode . 3)
;;      	(type . 2))
;;             ((data . [6 7 8 9 10 11 12])
;;      	(id . "BCDEFG")
;;      	(length . 7)
;;      	(opcode . 4)
;;      	(type . 1))))
;;
;;  To access a specific value in this structure, use the function
;;  bindat-get-field with the structure as first arg followed by a list
;;  of field names and array indexes, e.g. using the data above,
;;    (bindat-get-field decoded-structure 'item 1 'id)
;;  returns "BCDEFG".

;; Binary Data Structure Specification Format
;; ------------------------------------------

;; We recommend using names that end in `-bindat-spec'; such names
;; are recognized automatically as "risky" variables.

;; The data specification is formatted as follows:

;; SPEC    ::= ( ITEM... )

;; ITEM    ::= ( [FIELD] TYPE )
;;          |  ( [FIELD] eval FORM )    -- eval FORM for side-effect only
;;          |  ( [FIELD] fill LEN )     -- skip LEN bytes
;;          |  ( [FIELD] align LEN )    -- skip to next multiple of LEN bytes
;;          |  ( [FIELD] struct SPEC_NAME )
;;          |  ( [FIELD] union TAG_VAL (TAG SPEC)... [(t SPEC)] )
;;          |  ( [FIELD] repeat COUNT ITEM... )

;;          -- In (eval EXPR), the value of the last field is available in
;;             the dynamically bound variable `last'.

;; TYPE    ::= ( eval EXPR )		-- interpret result as TYPE
;;	    |  u8   | byte		-- length 1
;;          |  u16  | word | short      -- length 2, network byte order
;;          |  u24                      -- 3-byte value
;;          |  u32  | dword | long      -- length 4, network byte order
;;          |  u16r | u24r | u32r       -- little endian byte order.
;;	    |  str LEN                  -- LEN byte string
;;          |  strz LEN                 -- LEN byte (zero-terminated) string
;;          |  vec LEN [TYPE]           -- vector of LEN items of TYPE (default: u8)
;;          |  ip                       -- 4 byte vector
;;          |  bits LEN                 -- List with bits set in LEN bytes.
;;
;;          -- Note: 32 bit values may be limited by emacs' INTEGER
;;             implementation limits.
;;
;;          -- Example: `bits 2' will unpack 0x28 0x1c to (2 3 4 11 13)
;;                                       and 0x1c 0x28 to (3 5 10 11 12).

;; FIELD   ::= ( eval EXPR )		-- use result as NAME
;;          |  NAME

;; LEN     ::= ARG
;;          |  <omitted> | nil		-- LEN = 1


;; TAG_VAL ::= ARG

;; TAG     ::= LISP_CONSTANT
;;          |  ( eval EXPR )		-- return non-nil if tag match;
;;					   current TAG_VAL in `tag'.

;; ARG     ::= ( eval EXPR )		-- interpret result as ARG
;;          |  INTEGER_CONSTANT
;;          |  DEREF

;; DEREF   ::= ( [NAME | INTEGER]... )	-- Field NAME or Array index relative
;;                                         to current structure spec.
;;                                      -- see bindat-get-field

;; A `union' specification
;;    ([FIELD] union TAG_VAL (TAG SPEC) ... [(t SPEC)])
;; is interpreted by evalling TAG_VAL and then comparing that to
;; each TAG using equal; if a match is found, the corresponding SPEC
;; is used.
;; If TAG is a form (eval EXPR), EXPR is evalled with `tag' bound to the
;; value of TAG_VAL; the corresponding SPEC is used if the result is non-nil.
;; Finally, if TAG is t, the corresponding SPEC is used unconditionally.
;;
;; An `eval' specification
;;  ([FIELD] eval FORM)
;; is interpreted by evalling FORM for its side effects only.
;; If FIELD is specified, the value is bound to that field.
;; The FORM may access and update `bindat-raw' and `bindat-idx' (see `bindat-unpack').

;;; Code:

;; Helper functions for structure unpacking.
;; Relies on dynamic binding of BINDAT-RAW and BINDAT-IDX

(defvar bindat-raw)
(defvar bindat-idx)

(defun bindat--unpack-u8 ()
  (prog1
    (aref bindat-raw bindat-idx)
    (setq bindat-idx (1+ bindat-idx))))

(defun bindat--unpack-u16 ()
  (logior (lsh (bindat--unpack-u8) 8) (bindat--unpack-u8)))

(defun bindat--unpack-u24 ()
  (logior (lsh (bindat--unpack-u16) 8) (bindat--unpack-u8)))

(defun bindat--unpack-u32 ()
  (logior (lsh (bindat--unpack-u16) 16) (bindat--unpack-u16)))

(defun bindat--unpack-u16r ()
  (logior (bindat--unpack-u8) (lsh (bindat--unpack-u8) 8)))

(defun bindat--unpack-u24r ()
  (logior (bindat--unpack-u16r) (lsh (bindat--unpack-u8) 16)))

(defun bindat--unpack-u32r ()
  (logior (bindat--unpack-u16r) (lsh (bindat--unpack-u16r) 16)))

(defun bindat--unpack-item (type len &optional vectype)
  (if (eq type 'ip)
      (setq type 'vec len 4))
  (cond
   ((memq type '(u8 byte))
    (bindat--unpack-u8))
   ((memq type '(u16 word short))
    (bindat--unpack-u16))
   ((eq type 'u24)
    (bindat--unpack-u24))
   ((memq type '(u32 dword long))
    (bindat--unpack-u32))
   ((eq type 'u16r)
    (bindat--unpack-u16r))
   ((eq type 'u24r)
    (bindat--unpack-u24r))
   ((eq type 'u32r)
    (bindat--unpack-u32r))
   ((eq type 'bits)
    (let ((bits nil) (bnum (1- (* 8 len))) j m)
      (while (>= bnum 0)
	(if (= (setq m (bindat--unpack-u8)) 0)
	    (setq bnum (- bnum 8))
	  (setq j 128)
	  (while (> j 0)
	    (if (/= 0 (logand m j))
		(setq bits (cons bnum bits)))
	    (setq bnum (1- bnum)
		  j (lsh j -1)))))
      bits))
   ((eq type 'str)
    (let ((s (substring bindat-raw bindat-idx (+ bindat-idx len))))
      (setq bindat-idx (+ bindat-idx len))
      (if (stringp s) s
	(string-make-unibyte (concat s)))))
   ((eq type 'strz)
    (let ((i 0) s)
      (while (and (< i len) (/= (aref bindat-raw (+ bindat-idx i)) 0))
	(setq i (1+ i)))
      (setq s (substring bindat-raw bindat-idx (+ bindat-idx i)))
      (setq bindat-idx (+ bindat-idx len))
      (if (stringp s) s
	(string-make-unibyte (concat s)))))
   ((eq type 'vec)
    (let ((v (make-vector len 0)) (i 0) (vlen 1))
      (if (consp vectype)
	  (setq vlen (nth 1 vectype)
		vectype (nth 2 vectype))
	(setq type (or vectype 'u8)
	      vectype nil))
      (while (< i len)
	(aset v i (bindat--unpack-item type vlen vectype))
	(setq i (1+ i)))
      v))
   (t nil)))

(defun bindat--unpack-group (spec)
  (let (struct last)
    (while spec
      (let* ((item (car spec))
	     (field (car item))
	     (type (nth 1 item))
	     (len (nth 2 item))
	     (vectype (and (eq type 'vec) (nth 3 item)))
	     (tail 3)
	     data)
	(setq spec (cdr spec))
	(if (and (consp field) (eq (car field) 'eval))
	    (setq field (eval (car (cdr field)))))
	(if (and type (consp type) (eq (car type) 'eval))
	    (setq type (eval (car (cdr type)))))
	(if (and len (consp len) (eq (car len) 'eval))
	    (setq len (eval (car (cdr len)))))
	(if (memq field '(eval fill align struct union))
	    (setq tail 2
		  len type
		  type field
		  field nil))
	(if (and (consp len) (not (eq type 'eval)))
	    (setq len (apply 'bindat-get-field struct len)))
	(if (not len)
	    (setq len 1))
	(cond
	 ((eq type 'eval)
	  (if field
	      (setq data (eval len))
	    (eval len)))
	 ((eq type 'fill)
	  (setq bindat-idx (+ bindat-idx len)))
	 ((eq type 'align)
	  (while (/= (% bindat-idx len) 0)
	    (setq bindat-idx (1+ bindat-idx))))
	 ((eq type 'struct)
	  (setq data (bindat--unpack-group (eval len))))
	 ((eq type 'repeat)
	  (let ((index 0) (count len))
	    (while (< index count)
	      (setq data (cons (bindat--unpack-group (nthcdr tail item)) data))
	      (setq index (1+ index)))
	    (setq data (nreverse data))))
	 ((eq type 'union)
	  (let ((tag len) (cases (nthcdr tail item)) case cc)
	    (while cases
	      (setq case (car cases)
		    cases (cdr cases)
		    cc (car case))
	      (if (or (equal cc tag) (equal cc t)
		      (and (consp cc) (eval cc)))
		  (setq data (bindat--unpack-group (cdr case))
			cases nil)))))
	 (t
	  (setq data (bindat--unpack-item type len vectype)
		last data)))
	(if data
	    (if field
		(setq struct (cons (cons field data) struct))
	      (setq struct (append data struct))))))
    struct))

(defun bindat-unpack (spec bindat-raw &optional bindat-idx)
  "Return structured data according to SPEC for binary data in BINDAT-RAW.
BINDAT-RAW is a unibyte string or vector.
Optional third arg BINDAT-IDX specifies the starting offset in BINDAT-RAW."
  (when (multibyte-string-p bindat-raw)
    (error "String is multibyte"))
  (unless bindat-idx (setq bindat-idx 0))
  (bindat--unpack-group spec))

(defun bindat-get-field (struct &rest field)
  "In structured data STRUCT, return value of field named FIELD.
If multiple field names are specified, use the field names to
lookup nested sub-structures in STRUCT, corresponding to the
C-language syntax STRUCT.FIELD1.FIELD2.FIELD3...
An integer value in the field list is taken as an array index,
e.g. corresponding to STRUCT.FIELD1[INDEX2].FIELD3..."
  (while (and struct field)
    (setq struct (if (integerp (car field))
		     (nth (car field) struct)
		   (let ((val (assq (car field) struct)))
		     (if (consp val) (cdr val)))))
    (setq field (cdr field)))
  struct)


;; Calculate bindat-raw length of structured data

(defvar bindat--fixed-length-alist
  '((u8 . 1) (byte . 1)
    (u16 . 2) (u16r . 2) (word . 2) (short . 2)
    (u24 . 3) (u24r . 3)
    (u32 . 4) (u32r . 4) (dword . 4) (long . 4)
    (ip . 4)))

(defun bindat--length-group (struct spec)
  (let (last)
    (while spec
      (let* ((item (car spec))
	     (field (car item))
	     (type (nth 1 item))
	     (len (nth 2 item))
	     (vectype (and (eq type 'vec) (nth 3 item)))
	     (tail 3))
	(setq spec (cdr spec))
	(if (and (consp field) (eq (car field) 'eval))
	    (setq field (eval (car (cdr field)))))
	(if (and type (consp type) (eq (car type) 'eval))
	    (setq type (eval (car (cdr type)))))
	(if (and len (consp len) (eq (car len) 'eval))
	    (setq len (eval (car (cdr len)))))
	(if (memq field '(eval fill align struct union))
	    (setq tail 2
		  len type
		  type field
		  field nil))
	(if (and (consp len) (not (eq type 'eval)))
	    (setq len (apply 'bindat-get-field struct len)))
	(if (not len)
	    (setq len 1))
	(while (eq type 'vec)
	  (let ((vlen 1))
	    (if (consp vectype)
		(setq len (* len (nth 1 vectype))
		      type (nth 2 vectype))
	      (setq type (or vectype 'u8)
		    vectype nil))))
	(cond
	 ((eq type 'eval)
	  (if field
	      (setq struct (cons (cons field (eval len)) struct))
	    (eval len)))
	 ((eq type 'fill)
	  (setq bindat-idx (+ bindat-idx len)))
	 ((eq type 'align)
	  (while (/= (% bindat-idx len) 0)
	    (setq bindat-idx (1+ bindat-idx))))
	 ((eq type 'struct)
	  (bindat--length-group
	   (if field (bindat-get-field struct field) struct) (eval len)))
	 ((eq type 'repeat)
	  (let ((index 0) (count len))
	    (while (< index count)
	      (bindat--length-group
               (nth index (bindat-get-field struct field))
               (nthcdr tail item))
	      (setq index (1+ index)))))
	 ((eq type 'union)
	  (let ((tag len) (cases (nthcdr tail item)) case cc)
	    (while cases
	      (setq case (car cases)
		    cases (cdr cases)
		    cc (car case))
	      (if (or (equal cc tag) (equal cc t)
		      (and (consp cc) (eval cc)))
		  (progn
		    (bindat--length-group struct (cdr case))
		    (setq cases nil))))))
	 (t
	  (if (setq type (assq type bindat--fixed-length-alist))
	      (setq len (* len (cdr type))))
	  (if field
	      (setq last (bindat-get-field struct field)))
	  (setq bindat-idx (+ bindat-idx len))))))))

(defun bindat-length (spec struct)
  "Calculate bindat-raw length for STRUCT according to bindat SPEC."
  (let ((bindat-idx 0))
    (bindat--length-group struct spec)
    bindat-idx))


;; Pack structured data into bindat-raw

(defun bindat--pack-u8 (v)
  (aset bindat-raw bindat-idx (logand v 255))
  (setq bindat-idx (1+ bindat-idx)))

(defun bindat--pack-u16 (v)
  (aset bindat-raw bindat-idx (logand (lsh v -8) 255))
  (aset bindat-raw (1+ bindat-idx) (logand v 255))
  (setq bindat-idx (+ bindat-idx 2)))

(defun bindat--pack-u24 (v)
  (bindat--pack-u8 (lsh v -16))
  (bindat--pack-u16 v))

(defun bindat--pack-u32 (v)
  (bindat--pack-u16 (lsh v -16))
  (bindat--pack-u16 v))

(defun bindat--pack-u16r (v)
  (aset bindat-raw (1+ bindat-idx) (logand (lsh v -8) 255))
  (aset bindat-raw bindat-idx (logand v 255))
  (setq bindat-idx (+ bindat-idx 2)))

(defun bindat--pack-u24r (v)
  (bindat--pack-u16r v)
  (bindat--pack-u8 (lsh v -16)))

(defun bindat--pack-u32r (v)
  (bindat--pack-u16r v)
  (bindat--pack-u16r (lsh v -16)))

(defun bindat--pack-item (v type len &optional vectype)
  (if (eq type 'ip)
      (setq type 'vec len 4))
  (cond
   ((null v)
    (setq bindat-idx (+ bindat-idx len)))
   ((memq type '(u8 byte))
    (bindat--pack-u8 v))
   ((memq type '(u16 word short))
    (bindat--pack-u16 v))
   ((eq type 'u24)
    (bindat--pack-u24 v))
   ((memq type '(u32 dword long))
    (bindat--pack-u32 v))
   ((eq type 'u16r)
    (bindat--pack-u16r v))
   ((eq type 'u24r)
    (bindat--pack-u24r v))
   ((eq type 'u32r)
    (bindat--pack-u32r v))
   ((eq type 'bits)
    (let ((bnum (1- (* 8 len))) j m)
      (while (>= bnum 0)
	(setq m 0)
	(if (null v)
	    (setq bnum (- bnum 8))
	  (setq j 128)
	  (while (> j 0)
	    (if (memq bnum v)
		(setq m (logior m j)))
	    (setq bnum (1- bnum)
		  j (lsh j -1))))
	(bindat--pack-u8 m))))
   ((memq type '(str strz))
    (let ((l (length v)) (i 0))
      (if (> l len) (setq l len))
      (while (< i l)
	(aset bindat-raw (+ bindat-idx i) (aref v i))
	(setq i (1+ i)))
      (setq bindat-idx (+ bindat-idx len))))
   ((eq type 'vec)
    (let ((l (length v)) (i 0) (vlen 1))
      (if (consp vectype)
	  (setq vlen (nth 1 vectype)
		vectype (nth 2 vectype))
	(setq type (or vectype 'u8)
	      vectype nil))
      (if (> l len) (setq l len))
      (while (< i l)
	(bindat--pack-item (aref v i) type vlen vectype)
	(setq i (1+ i)))))
   (t
    (setq bindat-idx (+ bindat-idx len)))))

(defun bindat--pack-group (struct spec)
  (let (last)
    (while spec
      (let* ((item (car spec))
	     (field (car item))
	     (type (nth 1 item))
	     (len (nth 2 item))
	     (vectype (and (eq type 'vec) (nth 3 item)))
	     (tail 3))
	(setq spec (cdr spec))
	(if (and (consp field) (eq (car field) 'eval))
	    (setq field (eval (car (cdr field)))))
	(if (and type (consp type) (eq (car type) 'eval))
	    (setq type (eval (car (cdr type)))))
	(if (and len (consp len) (eq (car len) 'eval))
	    (setq len (eval (car (cdr len)))))
	(if (memq field '(eval fill align struct union))
	    (setq tail 2
		  len type
		  type field
		  field nil))
	(if (and (consp len) (not (eq type 'eval)))
	    (setq len (apply 'bindat-get-field struct len)))
	(if (not len)
	    (setq len 1))
	(cond
	 ((eq type 'eval)
	  (if field
	      (setq struct (cons (cons field (eval len)) struct))
	    (eval len)))
	 ((eq type 'fill)
	  (setq bindat-idx (+ bindat-idx len)))
	 ((eq type 'align)
	  (while (/= (% bindat-idx len) 0)
	    (setq bindat-idx (1+ bindat-idx))))
	 ((eq type 'struct)
	  (bindat--pack-group
	   (if field (bindat-get-field struct field) struct) (eval len)))
	 ((eq type 'repeat)
	  (let ((index 0) (count len))
	    (while (< index count)
	      (bindat--pack-group
               (nth index (bindat-get-field struct field))
               (nthcdr tail item))
	      (setq index (1+ index)))))
	 ((eq type 'union)
	  (let ((tag len) (cases (nthcdr tail item)) case cc)
	    (while cases
	      (setq case (car cases)
		    cases (cdr cases)
		    cc (car case))
	      (if (or (equal cc tag) (equal cc t)
		      (and (consp cc) (eval cc)))
		  (progn
		    (bindat--pack-group struct (cdr case))
		    (setq cases nil))))))
	 (t
	  (setq last (bindat-get-field struct field))
	  (bindat--pack-item last type len vectype)
	  ))))))

(defun bindat-pack (spec struct &optional bindat-raw bindat-idx)
  "Return binary data packed according to SPEC for structured data STRUCT.
Optional third arg BINDAT-RAW is a pre-allocated unibyte string or vector to
pack into.
Optional fourth arg BINDAT-IDX is the starting offset into BINDAT-RAW."
  (when (multibyte-string-p bindat-raw)
    (error "Pre-allocated string is multibyte"))
  (let ((no-return bindat-raw))
    (unless bindat-idx (setq bindat-idx 0))
    (unless bindat-raw
      (setq bindat-raw (make-string (+ bindat-idx (bindat-length spec struct)) 0)))
    (bindat--pack-group struct spec)
    (if no-return nil bindat-raw)))


;; Misc. format conversions

(defun bindat-format-vector (vect fmt sep &optional len)
  "Format vector VECT using element format FMT and separator SEP.
Result is a string with each element of VECT formatted using FMT and
separated by the string SEP.  If optional fourth arg LEN is given, use
only that many elements from VECT."
  (unless len
    (setq len (length vect)))
  (let ((i len) (fmt2 (concat sep fmt)) (s nil))
    (while (> i 0)
      (setq i (1- i)
	    s (cons (format (if (= i 0) fmt fmt2) (aref vect i)) s)))
    (apply 'concat s)))

(defun bindat-vector-to-dec (vect &optional sep)
  "Format vector VECT in decimal format separated by dots.
If optional second arg SEP is a string, use that as separator."
  (bindat-format-vector vect "%d" (if (stringp sep) sep ".")))

(defun bindat-vector-to-hex (vect &optional sep)
  "Format vector VECT in hex format separated by dots.
If optional second arg SEP is a string, use that as separator."
  (bindat-format-vector vect "%02x" (if (stringp sep) sep ":")))

(defun bindat-ip-to-string (ip)
  "Format vector IP as an ip address in dotted notation.
The port (if any) is omitted.  IP can be a string, as well."
  (if (vectorp ip)
      (format-network-address ip t)
    (format "%d.%d.%d.%d"
            (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3))))

(provide 'bindat)

;;; bindat.el ends here
