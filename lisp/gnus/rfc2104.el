;;; rfc2104.el --- RFC2104 Hashed Message Authentication Codes

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: mail

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

;; This is a high performance implementation of RFC2104.
;;
;; Example:
;;
;; (require 'md5)
;; (rfc2104-hash 'md5 64 16 "Jefe" "what do ya want for nothing?")
;; "750c783e6ab0b503eaa86e310a5db738"
;;
;; (require 'sha1)
;; (rfc2104-hash 'sha1 64 20 "Jefe" "what do ya want for nothing?")
;; "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
;;
;; 64 is block length of hash function (64 for MD5 and SHA), 16 is
;; resulting hash length (16 for MD5, 20 for SHA).
;;
;; Tested with Emacs 20.2 and XEmacs 20.3.
;;
;; Test case reference: RFC 2202.

;;; History:

;; 1998-08-16  initial release posted to gnu.emacs.sources
;; 1998-08-17  use append instead of char-list-to-string
;; 1998-08-26  don't require hexl
;; 1998-09-25  renamed from hmac.el to rfc2104.el, also renamed functions
;; 1999-10-23  included in pgnus
;; 2000-08-15  `rfc2104-hexstring-to-bitstring'
;; 2000-05-12  added sha-1 example, added test case reference
;; 2003-11-13  change rfc2104-hexstring-to-bitstring to ...-byte-list
;; 2008-04-25  rewrite rfc2104-hash for speed

;;; Code:

(eval-when-compile (require 'cl))

;; Magic character for inner HMAC round. 0x36 == 54 == '6'
(defconst rfc2104-ipad ?\x36)

;; Magic character for outer HMAC round. 0x5C == 92 == '\'
(defconst rfc2104-opad ?\x5C)

(defconst rfc2104-nybbles
  (let ((v (make-vector
            ;; Find upper bound to save some space.
            (1+ (max ?0 ?9 ?a ?f ?A ?F))
            ;; Use non-numeric default to catch bogus hex strings.
            nil))
        (ls '((?0 . 0)	 (?a . 10)   (?A . 10)
              (?1 . 1)	 (?b . 11)   (?B . 11)
              (?2 . 2)	 (?c . 12)   (?C . 12)
              (?3 . 3)	 (?d . 13)   (?D . 13)
              (?4 . 4)	 (?e . 14)   (?E . 14)
              (?5 . 5)	 (?f . 15)   (?F . 15)
              (?6 . 6)
              (?7 . 7)
              (?8 . 8)
              (?9 . 9))))
    (while ls
      (aset v (caar ls) (cdar ls))
      (setq ls (cdr ls)))
    v))

(eval-when-compile
  (defmacro rfc2104-string-make-unibyte (string)
    "Return the unibyte equivalent of STRING.
In XEmacs return just STRING."
    (if (featurep 'xemacs)
	string
      `(string-make-unibyte ,string))))

(defun rfc2104-hash (hash block-length hash-length key text)
  (let* (;; if key is longer than B, reset it to HASH(key)
	 (key (if (> (length key) block-length)
		  (funcall hash key) key))
         (len (length key))
	 (ipad (make-string    block-length              rfc2104-ipad))
	 (opad (make-string (+ block-length hash-length) rfc2104-opad))
         c partial)
    ;; Prefix *pad with key, appropriately XORed.
    (do ((i 0 (1+ i)))
        ((= len i))
      (setq c (aref key i))
      (aset ipad i (logxor rfc2104-ipad c))
      (aset opad i (logxor rfc2104-opad c)))
    ;; Perform inner hash.
    (setq partial (rfc2104-string-make-unibyte
		   (funcall hash (concat ipad text))))
    ;; Pack latter part of opad.
    (do ((r 0 (+ 2 r))
         (w block-length (1+ w)))
        ((= (* 2 hash-length) r))
      (aset opad w
            (+ (* 16 (aref rfc2104-nybbles (aref partial     r)))
               (      aref rfc2104-nybbles (aref partial (1+ r))))))
    ;; Perform outer hash.
    (rfc2104-string-make-unibyte (funcall hash opad))))

(provide 'rfc2104)

;;; rfc2104.el ends here
