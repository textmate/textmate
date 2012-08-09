;;; lao.el --- Quail package for inputting Lao characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Lao

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

(require 'quail)
(require 'lao-util)

(defun quail-lao-update-translation (control-flag)
  (if (integerp control-flag)
      ;; Non-composable character typed.
      (setq quail-current-str
	    (buffer-substring (overlay-start quail-overlay)
			      (overlay-end quail-overlay))
	    unread-command-events
	    (string-to-list
	     (substring quail-current-key control-flag)))
    (setq quail-current-str
	  (compose-string (quail-lookup-map-and-concat quail-current-key))))
  control-flag)

(defvar lao-key-alist
  '(("!" . "1")
    ("\"" . "=")
    ("#" . "3")
    ("$" . "4")
    ("&" . "5")
    ("%" . "(1l(B")
    ("'" . "(1'(B")
    ("(" . "7")
    (")" . "8")
    ("*" . "6")
    ("+" . ["(1mh(B"])
    ("," . "(1A(B")
    ("-" . "(1*(B")
    ("." . "(1c(B")
    ("/" . "(1=(B")
    ("0" . "(1"(B")
    ("1" . "(1B(B")
    ("2" . "(1?(B")
    ("3" . "(1b(B")
    ("4" . "(16(B")
    ("5" . "(1X(B")
    ("6" . "(1Y(B")
    ("7" . "(1$(B")
    ("8" . "(15(B")
    ("9" . "(1((B")
    (":" . "%")
    (";" . "(1G(B")
    ("<" . "(1}(B")
    ("=" . "(1m(B")
    (">" . "$")
    ("?" . ")")
    ("@" . "2")
    ("A" . ["(1Qi(B"])
    ("B" . ["(1Vi(B"])
    ("C" . "(1O(B")
    ("D" . ".")
    ("E" . ["(1Si(B"])
    ("F" . ",")
    ("G" . ":")
    ("H" . "(1j(B")
    ("I" . "(1N(B")
    ("J" . "(1k(B")
    ("K" . "!")
    ("L" . "?")
    ("M" . "(1f(B")
    ("N" . ["(1Wi(B"])
    ("O" . "(1|(B")
    ("P" . "(1](B")
    ("Q" . ["(1[i(B"])
    ("R" . "_")
    ("S" . ";")
    ("T" . "+")
    ("U" . ["(1Ui(B"])
    ("V" . "x")
    ("W" . "0")
    ("X" . "(")
    ("Y" . ["(1Ti(B"])
    ("Z" . "\"")
    ("[" . "(1:(B")
    ("]" . "(1E(B")
    ("^" . "(1\(B")
    ("_" . "9")
    ("`" . "(1'(B")
    ("a" . "(1Q(B")
    ("b" . "(1V(B")
    ("c" . "(1a(B")
    ("d" . "(1!(B")
    ("e" . "(1S(B")
    ("f" . "(14(B")
    ("g" . "(1`(B")
    ("h" . "(1i(B")
    ("i" . "(1C(B")
    ("j" . "(1h(B")
    ("k" . "(1R(B")
    ("l" . "(1J(B")
    ("m" . "(17(B")
    ("n" . "(1W(B")
    ("o" . "(19(B")
    ("p" . "(1-(B")
    ("q" . "(1[(B")
    ("r" . "(1>(B")
    ("s" . "(1K(B")
    ("t" . "(1P(B")
    ("u" . "(1U(B")
    ("v" . "(1M(B")
    ("w" . "(1d(B")
    ("x" . "(1;(B")
    ("y" . "(1T(B")
    ("z" . "(1<(B")
    ("{" . "-")
    ("|" . ["(1K\(B"])
    ("}" . "/")
    ("~" . "(1l(B")
    ("\\0" . "(1p(B")
    ("\\1" . "(1q(B")
    ("\\2" . "(1r(B")
    ("\\3" . "(1s(B")
    ("\\4" . "(1t(B")
    ("\\5" . "(1u(B")
    ("\\6" . "(1v(B")
    ("\\7" . "(1w(B")
    ("\\8" . "(1x(B")
    ("\\9" . "(1y(B")
    )
  "Alist of key sequences vs the corresponding Lao string to input.
This variable is for the input method \"lao\".
If you change the value of this variable while quail/lao is already loaded,
you need to re-load it to properly re-initialize related alists.")

;; Temporary variable to initialize lao-consonant-key-alist, etc.
(defconst lao-key-alist-vector
  (let ((tail lao-key-alist)
	consonant-key-alist semivowel-key-alist vowel-key-alist 
	voweltone-key-alist tone-key-alist other-key-alist
	elt phonetic-type)
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (if (stringp (cdr elt))
	  (setq phonetic-type (get-char-code-property (aref (cdr elt) 0)
						      'phonetic-type))
	(setq phonetic-type (get-char-code-property (aref (aref (cdr elt) 0) 0)
						    'phonetic-type))
	(aset (cdr elt) 0 (compose-string (aref (cdr elt) 0))))
      (cond ((eq phonetic-type 'consonant)
	     (setq consonant-key-alist (cons elt consonant-key-alist)))
	    ((memq phonetic-type '(vowel-upper vowel-lower))
	     (if (stringp (cdr elt))
		 (setq vowel-key-alist (cons elt vowel-key-alist))
	       (setq voweltone-key-alist (cons elt voweltone-key-alist))))
	    ((eq  phonetic-type 'tone)
	     (setq tone-key-alist (cons elt tone-key-alist)))
	    ((eq phonetic-type 'semivowel-lower)
	     (setq semivowel-key-alist (cons elt semivowel-key-alist)))
	    (t
	     (setq other-key-alist (cons elt other-key-alist)))))
    (vector consonant-key-alist semivowel-key-alist vowel-key-alist 
	    voweltone-key-alist tone-key-alist other-key-alist)))

(defconst lao-consonant-key-alist (aref lao-key-alist-vector 0))
(defconst lao-semivowel-key-alist (aref lao-key-alist-vector 1))
(defconst lao-vowel-key-alist (aref lao-key-alist-vector 2))
(defconst lao-voweltone-key-alist (aref lao-key-alist-vector 3))
(defconst lao-tone-key-alist (aref lao-key-alist-vector 4))
(defconst lao-other-key-alist (aref lao-key-alist-vector 5))

;; Done with it.
(makunbound 'lao-key-alist-vector)

(quail-define-package
 "lao" "Lao" "(1E(B" t
 "Lao input method simulating Lao keyboard layout based on Thai TIS620"
 nil t t t t nil nil nil 'quail-lao-update-translation nil t)

(quail-install-map
 (quail-map-from-table
  '((base-state (lao-consonant-key-alist . svt-state)
		lao-vowel-key-alist
		lao-voweltone-key-alist
		lao-tone-key-alist
		lao-other-key-alist)
    (svt-state (lao-semivowel-key-alist . v-state)
	       (lao-vowel-key-alist . t-state)
	       lao-voweltone-key-alist
	       lao-tone-key-alist)
    (v-state (lao-vowel-key-alist . t-state))
    (t-state lao-tone-key-alist))))

;;; lao.el ends here
