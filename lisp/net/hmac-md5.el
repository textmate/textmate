;;; hmac-md5.el --- Compute HMAC-MD5.

;; Copyright (C) 1999, 2001, 2007-2012  Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: HMAC, RFC2104, HMAC-MD5, MD5, KEYED-MD5, CRAM-MD5

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

;; Test cases from RFC 2202, "Test Cases for HMAC-MD5 and HMAC-SHA-1".
;;
;; (encode-hex-string (hmac-md5 "Hi There" (make-string 16 ?\x0b)))
;;  => "9294727a3638bb1c13f48ef8158bfc9d"
;;
;; (encode-hex-string (hmac-md5 "what do ya want for nothing?" "Jefe"))
;;  => "750c783e6ab0b503eaa86e310a5db738"
;;
;; (encode-hex-string (hmac-md5 (make-string 50 ?\xdd) (make-string 16 ?\xaa)))
;;  => "56be34521d144c88dbb8c733f0e8b3f6"
;;
;; (encode-hex-string
;;  (hmac-md5
;;   (make-string 50 ?\xcd)
;;   (decode-hex-string "0102030405060708090a0b0c0d0e0f10111213141516171819")))
;;  => "697eaf0aca3a3aea3a75164746ffaa79"
;;
;; (encode-hex-string
;;  (hmac-md5 "Test With Truncation" (make-string 16 ?\x0c)))
;;  => "56461ef2342edc00f9bab995690efd4c"
;;
;; (encode-hex-string
;;  (hmac-md5-96 "Test With Truncation" (make-string 16 ?\x0c)))
;;  => "56461ef2342edc00f9bab995"
;;
;; (encode-hex-string
;;  (hmac-md5
;;   "Test Using Larger Than Block-Size Key - Hash Key First"
;;   (make-string 80 ?\xaa)))
;;  => "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"
;;
;; (encode-hex-string
;;  (hmac-md5
;;   "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
;;   (make-string 80 ?\xaa)))
;;  => "6f630fad67cda0ee1fb1f562db3aa53e"

;;; Code:

(eval-when-compile (require 'hmac-def))
(require 'hex-util)			; (decode-hex-string STRING)
(require 'md5)				; expects (md5 STRING)

(defun md5-binary (string)
  "Return the MD5 of STRING in binary form."
  (if (condition-case nil
	  ;; `md5' of v21 takes 4th arg CODING (and 5th arg NOERROR).
	  (md5 "" nil nil 'binary)	; => "d41d8cd98f00b204e9800998ecf8427e"
	(wrong-number-of-arguments nil))
      (decode-hex-string (md5 string nil nil 'binary))
    (decode-hex-string (md5 string))))

(define-hmac-function hmac-md5 md5-binary 64 16) ; => (hmac-md5 TEXT KEY)
(define-hmac-function hmac-md5-96 md5-binary 64 16 96)

(provide 'hmac-md5)

;;; hmac-md5.el ends here
