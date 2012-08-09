;;; cookie1.el --- retrieve random phrases from fortune cookie files

;; Copyright (C) 1993, 2001-2012 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
;; Keywords: games, extensions
;; Created: Mon Mar 22 17:06:26 1993

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

;; Support for random cookie fetches from phrase files, used for such
;; critical applications as emulating Zippy the Pinhead and confounding
;; the NSA Trunk Trawler.
;;
;; The two entry points are `cookie' and `cookie-insert'.  The helper
;; function `shuffle-vector' may be of interest to programmers.
;;
;; The code expects phrase files to be in one of two formats:
;;
;; * ITS-style LINS format (strings terminated by ASCII 0 characters,
;; leading whitespace ignored).
;;
;; * UNIX fortune file format (quotes terminated by %% on a line by itself).
;;
;; Everything up to the first delimiter is treated as a comment.  Other
;; formats could be supported by adding alternates to the regexp
;; `cookie-delimiter'.
;;
;; strfile(1) is the program used to compile the files for fortune(6).
;; In order to achieve total compatibility with strfile(1), cookie files
;; should start with two consecutive delimiters (and no comment).
;;
;; This code derives from Steve Strassmann's 1987 spook.el package, but
;; has been generalized so that it supports multiple simultaneous
;; cookie databases and fortune files.  It is intended to be called
;; from other packages such as yow.el and spook.el.

;;; Code:

; Randomize the seed in the random number generator.
(random t)

(defconst cookie-delimiter "\n%%\n\\|\n%\n\\|\0"
  "Delimiter used to separate cookie file entries.")

(defvar cookie-cache (make-vector 511 0)
  "Cache of cookie files that have already been snarfed.")

;;;###autoload
(defun cookie (phrase-file startmsg endmsg)
  "Return a random phrase from PHRASE-FILE.
When the phrase file is read in, display STARTMSG at the beginning
of load, ENDMSG at the end."
  (let ((cookie-vector (cookie-snarf phrase-file startmsg endmsg)))
    (shuffle-vector cookie-vector)
    (aref cookie-vector 0)))

;;;###autoload
(defun cookie-insert (phrase-file &optional count startmsg endmsg)
  "Insert random phrases from PHRASE-FILE; COUNT of them.
When the phrase file is read in, display STARTMSG at the beginning
of load, ENDMSG at the end."
  (let ((cookie-vector (cookie-snarf phrase-file startmsg endmsg)))
    (shuffle-vector cookie-vector)
    (let ((start (point)))
      (insert ?\n)
      (cookie1 (min (- (length cookie-vector) 1) (or count 1)) cookie-vector)
      (insert ?\n)
      (fill-region-as-paragraph start (point) nil))))

(defun cookie1 (arg cookie-vec)
  "Inserts a cookie phrase ARG times."
  (cond ((zerop arg) t)
	(t (insert (aref cookie-vec arg))
	   (insert " ")
	   (cookie1 (1- arg) cookie-vec))))

;;;###autoload
(defun cookie-snarf (phrase-file startmsg endmsg)
  "Reads in the PHRASE-FILE, returns it as a vector of strings.
Emit STARTMSG and ENDMSG before and after.  Caches the result; second
and subsequent calls on the same file won't go to disk."
  (let ((sym (intern-soft phrase-file cookie-cache)))
    (and sym (not (equal (symbol-function sym)
			 (nth 5 (file-attributes phrase-file))))
	 (yes-or-no-p (concat phrase-file
			      " has changed.  Read new contents? "))
	 (setq sym nil))
    (if sym
	(symbol-value sym)
      (setq sym (intern phrase-file cookie-cache))
      (message "%s" startmsg)
      (save-excursion
	(let ((buf (generate-new-buffer "*cookie*"))
	      (result nil))
	  (set-buffer buf)
	  (fset sym (nth 5 (file-attributes phrase-file)))
	  (insert-file-contents (expand-file-name phrase-file))
	  (re-search-forward cookie-delimiter)
	  (while (progn (skip-chars-forward " \t\n\r\f") (not (eobp)))
	    (let ((beg (point)))
	      (re-search-forward cookie-delimiter)
	      (setq result (cons (buffer-substring beg (match-beginning 0))
				 result))))
	  (kill-buffer buf)
	  (message "%s" endmsg)
	  (set sym (apply 'vector result)))))))

(defun read-cookie (prompt phrase-file startmsg endmsg &optional require-match)
  "Prompt with PROMPT and read with completion among cookies in PHRASE-FILE.
STARTMSG and ENDMSG are passed along to `cookie-snarf'.
Optional fifth arg REQUIRE-MATCH non-nil forces a matching cookie."
  ;; Make sure the cookies are in the cache.
  (or (intern-soft phrase-file cookie-cache)
      (cookie-snarf phrase-file startmsg endmsg))
  (completing-read prompt
		   (let ((sym (intern phrase-file cookie-cache)))
		     ;; We cache the alist form of the cookie in a property.
		     (or (get sym 'completion-alist)
			 (let* ((alist nil)
				(vec (cookie-snarf phrase-file
						   startmsg endmsg))
				(i (length vec)))
			   (while (>= (setq i (1- i)) 0)
			     (setq alist (cons (list (aref vec i)) alist)))
			   (put sym 'completion-alist alist))))
		   nil require-match nil nil))

; Thanks to Ian G Batten <BattenIG@CS.BHAM.AC.UK>
; [of the University of Birmingham Computer Science Department]
; for the iterative version of this shuffle.
;
;;;###autoload
(defun shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)."
  (let ((i 0)
	j
	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)

(provide 'cookie1)

;;; cookie1.el ends here
