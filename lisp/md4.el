;;; md4.el --- MD4 Message Digest Algorithm.

;; Copyright (C) 2001, 2004, 2007-2012  Free Software Foundation, Inc.

;; Author: Taro Kawagishi <tarok@transpulse.org>
;; Keywords: MD4
;; Version: 1.00
;; Created: February 2001

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

;;; Code:

;;;
;;; MD4 hash calculation

(defvar md4-buffer (make-vector 4 '(0 . 0))
  "Work buffer of four 32-bit integers.")

(defun md4 (in n)
  "Return the MD4 hash for a string IN of length N bytes.
The returned hash is 16 bytes long.  N is required to handle
strings containing the character 0."
  (let (m
	(b (cons 0 (* n 8)))
	(i 0)
	(buf (make-string 128 0)) c4)
    ;; initial values
    (aset md4-buffer 0 '(26437 . 8961))		;0x67452301
    (aset md4-buffer 1 '(61389 . 43913))	;0xefcdab89
    (aset md4-buffer 2 '(39098 . 56574))	;0x98badcfe
    (aset md4-buffer 3 '(4146 . 21622))		;0x10325476

    ;; process the string in 64 bits chunks
    (while (> n 64)
      (setq m (md4-copy64 (substring in 0 64)))
      (md4-64 m)
      (setq in (substring in 64))
      (setq n (- n 64)))

    ;; process the rest of the string (length is now n <= 64)
    (setq i 0)
    (while (< i n)
      (aset buf i (aref in i))
      (setq i (1+ i)))
    (aset buf n 128)			;0x80
    (if (<= n 55)
	(progn
	  (setq c4 (md4-pack-int32 b))
	  (aset buf 56 (aref c4 0))
	  (aset buf 57 (aref c4 1))
	  (aset buf 58 (aref c4 2))
	  (aset buf 59 (aref c4 3))
	  (setq m (md4-copy64 buf))
	  (md4-64 m))
      ;; else
      (setq c4 (md4-pack-int32 b))
      (aset buf 120 (aref c4 0))
      (aset buf 121 (aref c4 1))
      (aset buf 122 (aref c4 2))
      (aset buf 123 (aref c4 3))
      (setq m (md4-copy64 buf))
      (md4-64 m)
      (setq m (md4-copy64 (substring buf 64)))
      (md4-64 m)))

    (concat (md4-pack-int32 (aref md4-buffer 0))
	    (md4-pack-int32 (aref md4-buffer 1))
	    (md4-pack-int32 (aref md4-buffer 2))
	    (md4-pack-int32 (aref md4-buffer 3))))

(defsubst md4-F (x y z) (logior (logand x y) (logand (lognot x) z)))
(defsubst md4-G (x y z) (logior (logand x y) (logand x z) (logand y z)))
(defsubst md4-H (x y z) (logxor x y z))

(defmacro md4-make-step (name func)
  `(defun ,name (a b c d xk s ac)
     (let*
         ((h1 (+ (car a) (,func (car b) (car c) (car d)) (car xk) (car ac)))
          (l1 (+ (cdr a) (,func (cdr b) (cdr c) (cdr d)) (cdr xk) (cdr ac)))
          (h2 (logand 65535 (+ h1 (lsh l1 -16))))
          (l2 (logand 65535 l1))
	  ;; cyclic shift of 32 bits integer
          (h3 (logand 65535 (if (> s 15)
                                (+ (lsh h2 (- s 32)) (lsh l2 (- s 16)))
                              (+ (lsh h2 s) (lsh l2 (- s 16))))))
          (l3 (logand 65535 (if (> s 15)
                                (+ (lsh l2 (- s 32)) (lsh h2 (- s 16)))
                              (+ (lsh l2 s) (lsh h2 (- s 16)))))))
       (cons h3 l3))))

(md4-make-step md4-round1 md4-F)
(md4-make-step md4-round2 md4-G)
(md4-make-step md4-round3 md4-H)

(defsubst md4-add (x y)
  "Return 32-bit sum of 32-bit integers X and Y."
  (let ((h (+ (car x) (car y)))
	(l (+ (cdr x) (cdr y))))
    (cons (logand 65535 (+ h (lsh l -16))) (logand 65535 l))))

(defsubst md4-and (x y)
  (cons (logand (car x) (car y)) (logand (cdr x) (cdr y))))

(defun md4-64 (m)
  "Calculate MD4 hash of M.
M is a 64-bytes chunk, represented as 16 pairs of 32-bit integers.
The resulting MD4 value is placed in `md4-buffer'."
  (let ((a (aref md4-buffer 0))
	(b (aref md4-buffer 1))
	(c (aref md4-buffer 2))
	(d (aref md4-buffer 3)))
    (setq a (md4-round1 a b c d (aref m  0)   3 '(0 . 0))
	  d (md4-round1 d a b c (aref m  1)   7 '(0 . 0))
	  c (md4-round1 c d a b (aref m  2)  11 '(0 . 0))
	  b (md4-round1 b c d a (aref m  3)  19 '(0 . 0))
	  a (md4-round1 a b c d (aref m  4)   3 '(0 . 0))
	  d (md4-round1 d a b c (aref m  5)   7 '(0 . 0))
	  c (md4-round1 c d a b (aref m  6)  11 '(0 . 0))
	  b (md4-round1 b c d a (aref m  7)  19 '(0 . 0))
	  a (md4-round1 a b c d (aref m  8)   3 '(0 . 0))
	  d (md4-round1 d a b c (aref m  9)   7 '(0 . 0))
	  c (md4-round1 c d a b (aref m 10)  11 '(0 . 0))
	  b (md4-round1 b c d a (aref m 11)  19 '(0 . 0))
	  a (md4-round1 a b c d (aref m 12)   3 '(0 . 0))
	  d (md4-round1 d a b c (aref m 13)   7 '(0 . 0))
	  c (md4-round1 c d a b (aref m 14)  11 '(0 . 0))
	  b (md4-round1 b c d a (aref m 15)  19 '(0 . 0))

	  a (md4-round2 a b c d (aref m  0)   3 '(23170 . 31129)) ;0x5A827999
	  d (md4-round2 d a b c (aref m  4)   5 '(23170 . 31129))
	  c (md4-round2 c d a b (aref m  8)   9 '(23170 . 31129))
	  b (md4-round2 b c d a (aref m 12)  13 '(23170 . 31129))
	  a (md4-round2 a b c d (aref m  1)   3 '(23170 . 31129))
	  d (md4-round2 d a b c (aref m  5)   5 '(23170 . 31129))
	  c (md4-round2 c d a b (aref m  9)   9 '(23170 . 31129))
	  b (md4-round2 b c d a (aref m 13)  13 '(23170 . 31129))
	  a (md4-round2 a b c d (aref m  2)   3 '(23170 . 31129))
	  d (md4-round2 d a b c (aref m  6)   5 '(23170 . 31129))
	  c (md4-round2 c d a b (aref m 10)   9 '(23170 . 31129))
	  b (md4-round2 b c d a (aref m 14)  13 '(23170 . 31129))
	  a (md4-round2 a b c d (aref m  3)   3 '(23170 . 31129))
	  d (md4-round2 d a b c (aref m  7)   5 '(23170 . 31129))
	  c (md4-round2 c d a b (aref m 11)   9 '(23170 . 31129))
	  b (md4-round2 b c d a (aref m 15)  13 '(23170 . 31129))

	  a (md4-round3 a b c d (aref m  0)   3 '(28377 . 60321)) ;0x6ED9EBA1
	  d (md4-round3 d a b c (aref m  8)   9 '(28377 . 60321))
	  c (md4-round3 c d a b (aref m  4)  11 '(28377 . 60321))
	  b (md4-round3 b c d a (aref m 12)  15 '(28377 . 60321))
	  a (md4-round3 a b c d (aref m  2)   3 '(28377 . 60321))
	  d (md4-round3 d a b c (aref m 10)   9 '(28377 . 60321))
	  c (md4-round3 c d a b (aref m  6)  11 '(28377 . 60321))
	  b (md4-round3 b c d a (aref m 14)  15 '(28377 . 60321))
	  a (md4-round3 a b c d (aref m  1)   3 '(28377 . 60321))
	  d (md4-round3 d a b c (aref m  9)   9 '(28377 . 60321))
	  c (md4-round3 c d a b (aref m  5)  11 '(28377 . 60321))
	  b (md4-round3 b c d a (aref m 13)  15 '(28377 . 60321))
	  a (md4-round3 a b c d (aref m  3)   3 '(28377 . 60321))
	  d (md4-round3 d a b c (aref m 11)   9 '(28377 . 60321))
	  c (md4-round3 c d a b (aref m  7)  11 '(28377 . 60321))
	  b (md4-round3 b c d a (aref m 15)  15 '(28377 . 60321)))

    (aset md4-buffer 0 (md4-add a (aref md4-buffer 0)))
    (aset md4-buffer 1 (md4-add b (aref md4-buffer 1)))
    (aset md4-buffer 2 (md4-add c (aref md4-buffer 2)))
    (aset md4-buffer 3 (md4-add d (aref md4-buffer 3)))
    ))

(defun md4-copy64 (seq)
  "Unpack a 64 bytes string into 16 pairs of 32 bits integers."
  (let ((int32s (make-vector 16 0)) (i 0) j)
    (while (< i 16)
      (setq j (* i 4))
      (aset int32s i (cons (+ (aref seq (+ j 2)) (lsh (aref seq (+ j 3)) 8))
			   (+ (aref seq j) (lsh (aref seq (1+ j)) 8))))
      (setq i (1+ i)))
    int32s))

;;;
;;; sub functions

(defun md4-pack-int16 (int16)
  "Pack 16 bits integer in 2 bytes string as little endian."
  (let ((str (make-string 2 0)))
    (aset str 0 (logand int16 255))
    (aset str 1 (lsh int16 -8))
    str))

(defun md4-pack-int32 (int32)
  "Pack 32 bits integer in a 4 bytes string as little endian.
A 32 bits integer is represented as a pair of two 16 bits
integers (cons high low)."
  (let ((str (make-string 4 0))
	(h (car int32)) (l (cdr int32)))
    (aset str 0 (logand l 255))
    (aset str 1 (lsh l -8))
    (aset str 2 (logand h 255))
    (aset str 3 (lsh h -8))
    str))

(defun md4-unpack-int16 (str)
  (if (eq 2 (length str))
      (+ (lsh (aref str 1) 8) (aref str 0))
    (error "%s is not 2 bytes long" str)))

(defun md4-unpack-int32 (str)
  (if (eq 4 (length str))
      (cons (+ (lsh (aref str 3) 8) (aref str 2))
	    (+ (lsh (aref str 1) 8) (aref str 0)))
    (error "%s is not 4 bytes long" str)))

(provide 'md4)

;;; md4.el ends here
