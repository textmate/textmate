;;; hmac-def.el --- A macro for defining HMAC functions.

;; Copyright (C) 1999, 2001, 2007-2012  Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: HMAC, RFC2104

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

;; This program is implemented from RFC2104,
;; "HMAC: Keyed-Hashing for Message Authentication".

;;; Code:

(defmacro define-hmac-function (name H B L &optional bit)
  "Define a function NAME(TEXT KEY) which computes HMAC with function H.

HMAC function is H(KEY XOR opad, H(KEY XOR ipad, TEXT)):

H is a cryptographic hash function, such as SHA1 and MD5, which takes
a string and return a digest of it (in binary form).
B is a byte-length of a block size of H. (B=64 for both SHA1 and MD5.)
L is a byte-length of hash outputs. (L=16 for MD5, L=20 for SHA1.)
If BIT is non-nil, truncate output to specified bits."
  `(defun ,name (text key)
     ,(concat "Compute "
	      (upcase (symbol-name name))
	      " over TEXT with KEY.")
     (let ((key-xor-ipad (make-string ,B ?\x36))
	   (key-xor-opad (make-string ,B ?\x5C))
	   (len (length key))
	   (pos 0))
       (unwind-protect
	   (progn
	     ;; if `key' is longer than the block size, apply hash function
	     ;; to `key' and use the result as a real `key'.
	     (if (> len ,B)
		 (setq key (,H key)
		       len ,L))
	     (while (< pos len)
	       (aset key-xor-ipad pos (logxor (aref key pos) ?\x36))
	       (aset key-xor-opad pos (logxor (aref key pos) ?\x5C))
	       (setq pos (1+ pos)))
	     (setq key-xor-ipad (unwind-protect
				    (concat key-xor-ipad text)
				  (fillarray key-xor-ipad 0))
		   key-xor-ipad (unwind-protect
				    (,H key-xor-ipad)
				  (fillarray key-xor-ipad 0))
		   key-xor-opad (unwind-protect
				    (concat key-xor-opad key-xor-ipad)
				  (fillarray key-xor-opad 0))
		   key-xor-opad (unwind-protect
				    (,H key-xor-opad)
				  (fillarray key-xor-opad 0)))
	     ;; now `key-xor-opad' contains
	     ;; H(KEY XOR opad, H(KEY XOR ipad, TEXT)).
	     ,(if (and bit (< (/ bit 8) L))
		  `(substring key-xor-opad 0 ,(/ bit 8))
		;; return a copy of `key-xor-opad'.
		`(concat key-xor-opad)))
	 ;; cleanup.
	 (fillarray key-xor-ipad 0)
	 (fillarray key-xor-opad 0)))))

(provide 'hmac-def)

;;; hmac-def.el ends here
