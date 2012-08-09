;;; hex-util.el --- Functions to encode/decode hexadecimal string.

;; Copyright (C) 1999, 2001-2012 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: data

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

(eval-when-compile
  (defmacro hex-char-to-num (chr)
    `(let ((chr ,chr))
       (cond
        ((and (<= ?a chr)(<= chr ?f)) (+ (- chr ?a) 10))
        ((and (<= ?A chr)(<= chr ?F)) (+ (- chr ?A) 10))
        ((and (<= ?0 chr)(<= chr ?9)) (- chr ?0))
        (t (error "Invalid hexadecimal digit `%c'" chr)))))
  (defmacro num-to-hex-char (num)
    `(aref "0123456789abcdef" ,num)))

(defun decode-hex-string (string)
  "Decode hexadecimal STRING to octet string."
  (let* ((len (length string))
	 (dst (make-string (/ len 2) 0))
	 (idx 0)(pos 0))
    (while (< pos len)
      ;; logior and lsh are not byte-coded.
      ;; (aset dst idx (logior (lsh (hex-char-to-num (aref string pos)) 4)
      ;; 			    (hex-char-to-num (aref string (1+ pos)))))
      (aset dst idx (+ (* (hex-char-to-num (aref string pos)) 16)
		       (hex-char-to-num (aref string (1+ pos)))))
      (setq idx (1+ idx)
	    pos (+ 2 pos)))
    dst))

(defun encode-hex-string (string)
  "Encode octet STRING to hexadecimal string."
  (let* ((len (length string))
	 (dst (make-string (* len 2) 0))
	 (idx 0)(pos 0))
    (while (< pos len)
      ;; logand and lsh are not byte-coded.
      ;; (aset dst idx (num-to-hex-char (logand (lsh (aref string pos) -4) 15)))
      (aset dst idx (num-to-hex-char (/ (aref string pos) 16)))
      (setq idx (1+ idx))
      ;; (aset dst idx (num-to-hex-char (logand (aref string pos) 15)))
      (aset dst idx (num-to-hex-char (% (aref string pos) 16)))
      (setq idx (1+ idx)
	    pos (1+ pos)))
    dst))

(provide 'hex-util)

;;; hex-util.el ends here
