;;; morse.el --- convert text to morse code and back             -*- coding: utf-8 -*-

;; Copyright (C) 1995, 2001-2012 Free Software Foundation, Inc.

;; Author: Rick Farnbach <rick_farnbach@MENTORG.COM>
;; Keywords: games

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

;; Converts text to Morse code and back with M-x morse-region and
;; M-x unmorse-region (though Morse code is no longer official :-().

;; Converts text to NATO phonetic alphabet and back with M-x
;; nato-region and M-x denato-region.

;;; Code:

(defvar morse-code '(("a" . ".-")
		     ("b" . "-...")
		     ("c" . "-.-.")
		     ("d" . "-..")
		     ("e" . ".")
		     ("f" . "..-.")
		     ("g" . "--.")
		     ("h" . "....")
		     ("i" . "..")
		     ("j" . ".---")
		     ("k" . "-.-")
		     ("l" . ".-..")
		     ("m" . "--")
		     ("n" . "-.")
		     ("o" . "---")
		     ("p" . ".--.")
		     ("q" . "--.-")
		     ("r" . ".-.")
		     ("s" . "...")
		     ("t" . "-")
		     ("u" . "..-")
		     ("v" . "...-")
		     ("w" . ".--")
		     ("x" . "-..-")
		     ("y" . "-.--")
		     ("z" . "--..")
		     ;; Punctuation
		     ("=" . "-...-")
		     ("?" . "..--..")
		     ("/" . "-..-.")
		     ("," . "--..--")
		     ("." . ".-.-.-")
		     (":" . "---...")
		     ("'" . ".----.")
		     ("-" . "-....-")
		     ("(" . "-.--.-")
		     (")" . "-.--.-")
		     ;; Numbers
		     ("0" . "-----")
		     ("1" . ".----")
		     ("2" . "..---")
		     ("3" . "...--")
		     ("4" . "....-")
		     ("5" . ".....")
		     ("6" . "-....")
		     ("7" . "--...")
		     ("8" . "---..")
		     ("9" . "----.")
		     ;; Non-ASCII
		     ("Ä" . ".-.-")
		     ("Æ" . ".-.-")
		     ("Á" . ".--.-")
		     ("Å" . ".--.-")
		     ;; ligature character?? ("Ch" . "----")
		     ("ß" . ".../...")
		     ("É" . "..-..")
		     ("Ñ" . "--.--")
		     ("Ö" . "---.")
		     ("Ø" . "---.")
		     ("Ü" . "..--")
		     ;; Recently standardized
		     ("@" . ".--.-."))
  "Morse code character set.")

(defvar nato-alphabet '(("a" . "Alfa")
			("b" . "Bravo")
			("c" . "Charlie")
			("d" . "Delta")
			("e" . "Echo")
			("f" . "Foxtrot")
			("g" . "Golf")
			("h" . "Hotel")
			("i" . "India")
			("j" . "Juliett")
			("k" . "Kilo")
			("l" . "Lima")
			("m" . "Mike")
			("n" . "November")
			("o" . "Oscar")
			("p" . "Papa")
			("q" . "Quebec")
			("r" . "Romeo")
			("s" . "Sierra")
			("t" . "Tango")
			("u" . "Uniform")
			("v" . "Victor")
			("w" . "Whiskey")
			("x" . "Xray")
			("y" . "Yankee")
			("z" . "Zulu")
			;; Numbers
			("0" . "Zero")
			("1" . "One")
			("2" . "Two")
			("3" . "Three")
			("4" . "Four")
			("5" . "Five")
			("6" . "Six")
			("7" . "Seven")
			("8" . "Eight")
			("9" . "Niner")
			;; Punctuation is not part of standard
			("=" . "Equals")
			("?" . "Query")
			("/" . "Slash")
			("," . "Comma")
			("." . "Stop")
			(":" . "Colon")
			("'" . "Apostrophe")
			("-" . "Dash")
			("(" . "Open")
			(")" . "Close")
			("@" . "At"))
  "NATO phonetic alphabet.
See ''International Code of Signals'' (INTERCO), United States
Edition, 1969 Edition (Revised 2003) available from National
Geospatial-Intelligence Agency at http://www.nga.mil/")

;;;###autoload
(defun morse-region (beg end)
  "Convert all text in a given region to morse code."
  (interactive "*r")
  (if (integerp end)
      (setq end (copy-marker end)))
  (save-excursion
    (let ((sep "")
	  str morse)
      (goto-char beg)
      (while (< (point) end)
	(setq str (downcase (buffer-substring (point) (1+ (point)))))
	(cond ((looking-at "\\s-+")
	       (goto-char (match-end 0))
	       (setq sep ""))
	      ((setq morse (assoc str morse-code))
	       (delete-char 1)
	       (insert sep (cdr morse))
	       (setq sep "/"))
	      (t
	       (forward-char 1)
	       (setq sep "")))))))

;;;###autoload
(defun unmorse-region (beg end)
  "Convert morse coded text in region to ordinary ASCII text."
  (interactive "*r")
  (if (integerp end)
      (setq end (copy-marker end)))
  (save-excursion
    (let (str paren morse)
      (goto-char beg)
      (while (< (point) end)
	(if (null (looking-at "[-.]+"))
	    (forward-char 1)
	  (setq str (buffer-substring (match-beginning 0) (match-end 0)))
	  (if (null (setq morse (rassoc str morse-code)))
	      (goto-char (match-end 0))
	    (replace-match
		  (if (string-equal "(" (car morse))
		      (if (setq paren (null paren)) "(" ")")
		    (car morse)) t)
	    (if (looking-at "/")
		(delete-char 1))))))))

;;;###autoload
(defun nato-region (beg end)
  "Convert all text in a given region to NATO phonetic alphabet."
  ;; Copied from morse-region. -- ashawley 2009-02-10
  (interactive "*r")
  (if (integerp end)
      (setq end (copy-marker end)))
  (save-excursion
    (let ((sep "")
	  str nato)
      (goto-char beg)
      (while (< (point) end)
	(setq str (downcase (buffer-substring (point) (1+ (point)))))
	(cond ((looking-at "\\s-+")
	       (goto-char (match-end 0))
	       (setq sep ""))
	      ((setq nato (assoc str nato-alphabet))
	       (delete-char 1)
	       (insert sep (cdr nato))
	       (setq sep "-"))
	      (t
	       (forward-char 1)
	       (setq sep "")))))))

;;;###autoload
(defun denato-region (beg end)
  "Convert NATO phonetic alphabet in region to ordinary ASCII text."
  ;; Copied from unmorse-region. -- ashawley 2009-02-10
  (interactive "*r")
  (if (integerp end)
      (setq end (copy-marker end)))
  (save-excursion
    (let (str paren nato)
      (goto-char beg)
      (while (< (point) end)
	(if (null (looking-at "[a-z]+"))
	    (forward-char 1)
	  (setq str (buffer-substring (match-beginning 0) (match-end 0)))
	  (if (null (setq nato (rassoc (capitalize str) nato-alphabet)))
	      (goto-char (match-end 0))
	    (replace-match
		  (if (string-equal "(" (car nato))
		      (if (setq paren (null paren)) "(" ")")
		    (car nato)) t)
	    (if (looking-at "-")
		(delete-char 1))))))))

(provide 'morse)

;;; morse.el ends here
