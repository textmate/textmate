;;; pgg-parse.el --- OpenPGP packet parsing

;; Copyright (C) 1999, 2002-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/10/28
;; Keywords: PGP, OpenPGP, GnuPG
;; Package: pgg
;; Obsolete-since: 24.1

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

;;    This module is based on

;;	[OpenPGP] RFC 2440: "OpenPGP Message Format"
;;	    by John W. Noerenberg, II <jwn2@qualcomm.com>,
;;          Jon Callas <jon@pgp.com>, Lutz Donnerhacke <lutz@iks-jena.de>,
;;          Hal Finney <hal@pgp.com> and Rodney Thayer <rodney@unitran.com>
;;	    (1998/11)

;;; Code:

(eval-when-compile
  ;; For Emacs <22.2 and XEmacs.
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r)))
  (require 'cl))

(defgroup pgg-parse ()
  "OpenPGP packet parsing."
  :group 'pgg)

(defcustom pgg-parse-public-key-algorithm-alist
  '((1 . RSA) (2 . RSA-E) (3 . RSA-S) (16 . ELG-E) (17 . DSA) (20 . ELG))
  "Alist of the assigned number to the public key algorithm."
  :group 'pgg-parse
  :type '(repeat
	  (cons (sexp :tag "Number") (sexp :tag "Type"))))

(defcustom pgg-parse-symmetric-key-algorithm-alist
  '((1 . IDEA) (2 . 3DES) (4 . CAST5) (5 . SAFER-SK128))
  "Alist of the assigned number to the symmetric key algorithm."
  :group 'pgg-parse
  :type '(repeat
	  (cons (sexp :tag "Number") (sexp :tag "Type"))))

(defcustom pgg-parse-hash-algorithm-alist
  '((1 . MD5) (2 . SHA1) (3 . RIPEMD160) (5 . MD2) (8 . SHA256) (9 . SHA384)
    (10 . SHA512))
  "Alist of the assigned number to the cryptographic hash algorithm."
  :group 'pgg-parse
  :type '(repeat
	  (cons (sexp :tag "Number") (sexp :tag "Type"))))

(defcustom pgg-parse-compression-algorithm-alist
  '((0 . nil); Uncompressed
    (1 . ZIP)
    (2 . ZLIB))
  "Alist of the assigned number to the compression algorithm."
  :group 'pgg-parse
  :type '(repeat
	  (cons (sexp :tag "Number") (sexp :tag "Type"))))

(defcustom pgg-parse-signature-type-alist
  '((0 . "Signature of a binary document")
    (1 . "Signature of a canonical text document")
    (2 . "Standalone signature")
    (16 . "Generic certification of a User ID and Public Key packet")
    (17 . "Persona certification of a User ID and Public Key packet")
    (18 . "Casual certification of a User ID and Public Key packet")
    (19 . "Positive certification of a User ID and Public Key packet")
    (24 . "Subkey Binding Signature")
    (31 . "Signature directly on a key")
    (32 . "Key revocation signature")
    (40 . "Subkey revocation signature")
    (48 . "Certification revocation signature")
    (64 . "Timestamp signature."))
  "Alist of the assigned number to the signature type."
  :group 'pgg-parse
  :type '(repeat
	  (cons (sexp :tag "Number") (sexp :tag "Type"))))

(defcustom pgg-ignore-packet-checksum t; XXX
  "If non-nil checksum of each ascii armored packet will be ignored."
  :group 'pgg-parse
  :type 'boolean)

(defvar pgg-armor-header-lines
  '("^-----BEGIN PGP MESSAGE\\(, PART [0-9]+\\(/[0-9]+\\)?\\)?-----\r?$"
    "^-----BEGIN PGP PUBLIC KEY BLOCK-----\r?$"
    "^-----BEGIN PGP PRIVATE KEY BLOCK-----\r?$"
    "^-----BEGIN PGP SIGNATURE-----\r?$")
  "Armor headers.")

(eval-and-compile
  (defalias 'pgg-char-int (if (fboundp 'char-int)
			      'char-int
			    'identity)))

(defmacro pgg-format-key-identifier (string)
  `(mapconcat (lambda (c) (format "%02X" (pgg-char-int c)))
	      ,string "")
  ;; `(upcase (apply #'format "%02x%02x%02x%02x%02x%02x%02x%02x"
  ;;                 (string-to-number-list ,string)))
  )

(defmacro pgg-parse-time-field (bytes)
  `(list (logior (lsh (car ,bytes) 8)
		 (nth 1 ,bytes))
	 (logior (lsh (nth 2 ,bytes) 8)
		 (nth 3 ,bytes))
	 0))

(defmacro pgg-byte-after (&optional pos)
  `(pgg-char-int (char-after ,(or pos `(point)))))

(defmacro pgg-read-byte ()
  `(pgg-char-int (char-after (prog1 (point) (forward-char)))))

(defmacro pgg-read-bytes-string (nbytes)
  `(buffer-substring
    (point) (prog1 (+ ,nbytes (point))
	      (forward-char ,nbytes))))

(defmacro pgg-read-bytes (nbytes)
  `(mapcar #'pgg-char-int (pgg-read-bytes-string ,nbytes))
  ;; `(string-to-number-list (pgg-read-bytes-string ,nbytes))
  )

(defmacro pgg-read-body-string (ptag)
  `(if (nth 1 ,ptag)
       (pgg-read-bytes-string (nth 1 ,ptag))
     (pgg-read-bytes-string (- (point-max) (point)))))

(defmacro pgg-read-body (ptag)
  `(mapcar #'pgg-char-int (pgg-read-body-string ,ptag))
  ;; `(string-to-number-list (pgg-read-body-string ,ptag))
  )

(defalias 'pgg-skip-bytes 'forward-char)

(defmacro pgg-skip-header (ptag)
  `(pgg-skip-bytes (nth 2 ,ptag)))

(defmacro pgg-skip-body (ptag)
  `(pgg-skip-bytes (nth 1 ,ptag)))

(defmacro pgg-set-alist (alist key value)
  `(setq ,alist (nconc ,alist (list (cons ,key ,value)))))

(when (fboundp 'define-ccl-program)

  (define-ccl-program pgg-parse-crc24
    '(1
      ((loop
	(read r0) (r1 ^= r0) (r2 ^= 0)
	(r5 = 0)
	(loop
	 (r1 <<= 1)
	 (r1 += ((r2 >> 15) & 1))
	 (r2 <<= 1)
	 (if (r1 & 256)
	     ((r1 ^= 390) (r2 ^= 19707)))
	 (if (r5 < 7)
	     ((r5 += 1)
	      (repeat))))
	(repeat)))))

  (defvar pgg-parse-crc24)

  (defun pgg-parse-crc24-string (string)
    (let ((h (vector nil 183 1230 nil nil nil nil nil nil)))
      (ccl-execute-on-string pgg-parse-crc24 h string)
      (format "%c%c%c"
	      (logand (aref h 1) 255)
	      (logand (lsh (aref h 2) -8) 255)
	      (logand (aref h 2) 255)))))

(defmacro pgg-parse-length-type (c)
  `(cond
    ((< ,c 192) (cons ,c 1))
    ((< ,c 224)
     (cons (+ (lsh (- ,c 192) 8)
	      (pgg-byte-after (+ 2 (point)))
	      192)
	   2))
    ((= ,c 255)
     (cons (cons (logior (lsh (pgg-byte-after (+ 2 (point))) 8)
			 (pgg-byte-after (+ 3 (point))))
		 (logior (lsh (pgg-byte-after (+ 4 (point))) 8)
			 (pgg-byte-after (+ 5 (point)))))
	   5))
    (t;partial body length
     '(0 . 0))))

(defun pgg-parse-packet-header ()
  (let ((ptag (pgg-byte-after))
	length-type content-tag packet-bytes header-bytes)
    (if (zerop (logand 64 ptag));Old format
	(progn
	  (setq length-type (logand ptag 3)
		length-type (if (= 3 length-type) 0 (lsh 1 length-type))
		content-tag (logand 15 (lsh ptag -2))
		packet-bytes 0
		header-bytes (1+ length-type))
	  (dotimes (i length-type)
	    (setq packet-bytes
		  (logior (lsh packet-bytes 8)
			  (pgg-byte-after (+ 1 i (point)))))))
      (setq content-tag (logand 63 ptag)
	    length-type (pgg-parse-length-type
			 (pgg-byte-after (1+ (point))))
	    packet-bytes (car length-type)
	    header-bytes (1+ (cdr length-type))))
    (list content-tag packet-bytes header-bytes)))

(defun pgg-parse-packet (ptag)
  (case (car ptag)
    (1 ;Public-Key Encrypted Session Key Packet
     (pgg-parse-public-key-encrypted-session-key-packet ptag))
    (2 ;Signature Packet
     (pgg-parse-signature-packet ptag))
    (3 ;Symmetric-Key Encrypted Session Key Packet
     (pgg-parse-symmetric-key-encrypted-session-key-packet ptag))
    ;; 4        -- One-Pass Signature Packet
    ;; 5        -- Secret Key Packet
    (6 ;Public Key Packet
     (pgg-parse-public-key-packet ptag))
    ;; 7        -- Secret Subkey Packet
    ;; 8        -- Compressed Data Packet
    (9 ;Symmetrically Encrypted Data Packet
     (pgg-read-body-string ptag))
    (10 ;Marker Packet
     (pgg-read-body-string ptag))
    (11 ;Literal Data Packet
     (pgg-read-body-string ptag))
    ;; 12       -- Trust Packet
    (13 ;User ID Packet
     (pgg-read-body-string ptag))
    ;; 14       -- Public Subkey Packet
    ;; 60 .. 63 -- Private or Experimental Values
    ))

(defun pgg-parse-packets (&optional header-parser body-parser)
  (let ((header-parser
	 (or header-parser
	     (function pgg-parse-packet-header)))
	(body-parser
	 (or body-parser
	     (function pgg-parse-packet)))
	result ptag)
    (while (> (point-max) (1+ (point)))
      (setq ptag (funcall header-parser))
      (pgg-skip-header ptag)
      (push (cons (car ptag)
		  (save-excursion
		    (funcall body-parser ptag)))
	    result)
      (if (zerop (nth 1 ptag))
	  (goto-char (point-max))
	(forward-char (nth 1 ptag))))
    result))

(defun pgg-parse-signature-subpacket-header ()
  (let ((length-type (pgg-parse-length-type (pgg-byte-after))))
    (list (pgg-byte-after (+ (cdr length-type) (point)))
	  (1- (car length-type))
	  (1+ (cdr length-type)))))

(defun pgg-parse-signature-subpacket (ptag)
  (case (car ptag)
    (2 ;signature creation time
     (cons 'creation-time
	   (let ((bytes (pgg-read-bytes 4)))
	     (pgg-parse-time-field bytes))))
    (3 ;signature expiration time
     (cons 'signature-expiry
	   (let ((bytes (pgg-read-bytes 4)))
	     (pgg-parse-time-field bytes))))
    (4 ;exportable certification
     (cons 'exportability (pgg-read-byte)))
    (5 ;trust signature
     (cons 'trust-level (pgg-read-byte)))
    (6 ;regular expression
     (cons 'regular-expression
	   (pgg-read-body-string ptag)))
    (7 ;revocable
     (cons 'revocability (pgg-read-byte)))
    (9 ;key expiration time
     (cons 'key-expiry
	   (let ((bytes (pgg-read-bytes 4)))
	     (pgg-parse-time-field bytes))))
    ;; 10 = placeholder for backward compatibility
    (11 ;preferred symmetric algorithms
     (cons 'preferred-symmetric-key-algorithm
	   (cdr (assq (pgg-read-byte)
		      pgg-parse-symmetric-key-algorithm-alist))))
    (12 ;revocation key
     )
    (16 ;issuer key ID
     (cons 'key-identifier
	   (pgg-format-key-identifier (pgg-read-body-string ptag))))
    (20 ;notation data
     (pgg-skip-bytes 4)
     (cons 'notation
	   (let ((name-bytes (pgg-read-bytes 2))
		 (value-bytes (pgg-read-bytes 2)))
	     (cons (pgg-read-bytes-string
		    (logior (lsh (car name-bytes) 8)
			    (nth 1 name-bytes)))
		   (pgg-read-bytes-string
		    (logior (lsh (car value-bytes) 8)
			    (nth 1 value-bytes)))))))
    (21 ;preferred hash algorithms
     (cons 'preferred-hash-algorithm
	   (cdr (assq (pgg-read-byte)
		      pgg-parse-hash-algorithm-alist))))
    (22 ;preferred compression algorithms
     (cons 'preferred-compression-algorithm
	   (cdr (assq (pgg-read-byte)
		      pgg-parse-compression-algorithm-alist))))
    (23 ;key server preferences
     (cons 'key-server-preferences
	   (pgg-read-body ptag)))
    (24 ;preferred key server
     (cons 'preferred-key-server
	   (pgg-read-body-string ptag)))
    ;; 25 = primary user id
    (26 ;policy URL
     (cons 'policy-url (pgg-read-body-string ptag)))
    ;; 27 = key flags
    ;; 28 = signer's user id
    ;; 29 = reason for revocation
    ;; 100 to 110 = internal or user-defined
    ))

(defun pgg-parse-signature-packet (ptag)
  (let* ((signature-version (pgg-byte-after))
	 (result (list (cons 'version signature-version)))
	 hashed-material field n)
    (cond
     ((= signature-version 3)
      (pgg-skip-bytes 2)
      (setq hashed-material (pgg-read-bytes 5))
      (pgg-set-alist result
		     'signature-type
		     (cdr (assq (pop hashed-material)
				pgg-parse-signature-type-alist)))
      (pgg-set-alist result
		     'creation-time
		     (pgg-parse-time-field hashed-material))
      (pgg-set-alist result
		     'key-identifier
		     (pgg-format-key-identifier
		      (pgg-read-bytes-string 8)))
      (pgg-set-alist result
		     'public-key-algorithm (pgg-read-byte))
      (pgg-set-alist result
		     'hash-algorithm (pgg-read-byte)))
     ((= signature-version 4)
      (pgg-skip-bytes 1)
      (pgg-set-alist result
		     'signature-type
		     (cdr (assq (pgg-read-byte)
				pgg-parse-signature-type-alist)))
      (pgg-set-alist result
		     'public-key-algorithm
		     (pgg-read-byte))
      (pgg-set-alist result
		     'hash-algorithm (pgg-read-byte))
      (when (>= 10000 (setq n (pgg-read-bytes 2)
			    n (logior (lsh (car n) 8)
				      (nth 1 n))))
	(save-restriction
	  (narrow-to-region (point)(+ n (point)))
	  (nconc result
		 (mapcar (function cdr) ;remove packet types
			 (pgg-parse-packets
			  #'pgg-parse-signature-subpacket-header
			  #'pgg-parse-signature-subpacket)))
	  (goto-char (point-max))))
      (when (>= 10000 (setq n (pgg-read-bytes 2)
			    n (logior (lsh (car n) 8)
				      (nth 1 n))))
	(save-restriction
	  (narrow-to-region (point)(+ n (point)))
	  (nconc result
		 (mapcar (function cdr) ;remove packet types
			 (pgg-parse-packets
			  #'pgg-parse-signature-subpacket-header
			  #'pgg-parse-signature-subpacket)))))))

    (setcdr (setq field (assq 'public-key-algorithm
			      result))
	    (cdr (assq (cdr field)
		       pgg-parse-public-key-algorithm-alist)))
    (setcdr (setq field (assq 'hash-algorithm
			      result))
	    (cdr (assq (cdr field)
		       pgg-parse-hash-algorithm-alist)))
    result))

(defun pgg-parse-public-key-encrypted-session-key-packet (ptag)
  (let (result)
    (pgg-set-alist result
		   'version (pgg-read-byte))
    (pgg-set-alist result
		   'key-identifier
		   (pgg-format-key-identifier
		    (pgg-read-bytes-string 8)))
    (pgg-set-alist result
		   'public-key-algorithm
		   (cdr (assq (pgg-read-byte)
			      pgg-parse-public-key-algorithm-alist)))
    result))

(defun pgg-parse-symmetric-key-encrypted-session-key-packet (ptag)
  (let (result)
    (pgg-set-alist result
		   'version
		   (pgg-read-byte))
    (pgg-set-alist result
		   'symmetric-key-algorithm
		   (cdr (assq (pgg-read-byte)
			      pgg-parse-symmetric-key-algorithm-alist)))
    result))

(defun pgg-parse-public-key-packet (ptag)
  (let* ((key-version (pgg-read-byte))
	 (result (list (cons 'version key-version)))
	 field)
    (cond
     ((= 3 key-version)
      (pgg-set-alist result
		     'creation-time
		     (let ((bytes (pgg-read-bytes 4)))
		       (pgg-parse-time-field bytes)))
      (pgg-set-alist result
		     'key-expiry (pgg-read-bytes 2))
      (pgg-set-alist result
		     'public-key-algorithm (pgg-read-byte)))
     ((= 4 key-version)
      (pgg-set-alist result
		     'creation-time
		     (let ((bytes (pgg-read-bytes 4)))
		       (pgg-parse-time-field bytes)))
      (pgg-set-alist result
		     'public-key-algorithm (pgg-read-byte))))

    (setcdr (setq field (assq 'public-key-algorithm
			      result))
	    (cdr (assq (cdr field)
		       pgg-parse-public-key-algorithm-alist)))
    result))

;; p-d-p only calls this if it is defined, but the compiler does not
;; recognize that.
(declare-function pgg-parse-crc24-string "pgg-parse" (string))

(defun pgg-decode-packets ()
  (if (re-search-forward "^=\\([A-Za-z0-9+/]\\{4\\}\\)$" nil t)
      (let ((p (match-beginning 0))
	    (checksum (match-string 1)))
	(delete-region p (point-max))
	(if (ignore-errors (base64-decode-region (point-min) p))
	    (or (not (fboundp 'pgg-parse-crc24-string))
		pgg-ignore-packet-checksum
		(string-equal (base64-encode-string (pgg-parse-crc24-string
						     (buffer-string)))
			      checksum)
		(progn
		  (message "PGP packet checksum does not match")
		  nil))
	  (message "PGP packet contain invalid base64")
	  nil))
    (message "PGP packet checksum not found")
    nil))

(defun pgg-decode-armor-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (re-search-forward "^-+BEGIN PGP" nil t)
    (delete-region (point-min)
		   (and (search-forward "\n\n")
			(match-end 0)))
    (when (pgg-decode-packets)
      (goto-char (point-min))
      (pgg-parse-packets))))

(defun pgg-parse-armor (string)
  (with-temp-buffer
    (buffer-disable-undo)
    (unless (featurep 'xemacs)
      (set-buffer-multibyte nil))
    (insert string)
    (pgg-decode-armor-region (point-min)(point))))

(eval-and-compile
  (defalias 'pgg-string-as-unibyte (if (fboundp 'string-as-unibyte)
				       'string-as-unibyte
				     'identity)))

(defun pgg-parse-armor-region (start end)
  (pgg-parse-armor (pgg-string-as-unibyte (buffer-substring start end))))

(provide 'pgg-parse)

;;; pgg-parse.el ends here
