;;; iso-transl.el --- keyboard input definitions for ISO 8859-1  -*- coding: iso-8859-1 -*-

;; Copyright (C) 1987, 1993-1999, 2001-2012  Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n

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

;; Loading this package defines three ways of entering the non-ASCII
;; printable characters with codes above 127: the prefix C-x 8, or the
;; Alt key, or a dead accent key.  For example, you can enter uppercase
;; A-umlaut as `C-x 8 " A' or `Alt-" A' (if you have an Alt key) or
;; `umlaut A' (if you have an umlaut/diaeresis key).

;; C-x 8 is set up to autoload this package,
;; but Alt keys and dead accent keys are only defined
;; once you have loaded the package.  It is nontrivial
;; to make all of the Alt keys autoload, and it is not clear
;; that the dead accent keys SHOULD autoload this package.

;;; Code:

;;; Provide some binding for startup:
;;;###autoload (or key-translation-map (setq key-translation-map (make-sparse-keymap)))
;;;###autoload (define-key key-translation-map "\C-x8" 'iso-transl-ctl-x-8-map)
;;;###autoload (autoload 'iso-transl-ctl-x-8-map "iso-transl" "Keymap for C-x 8 prefix." t 'keymap)

(defvar iso-transl-dead-key-alist
  '((?\' . mute-acute)
    (?\` . mute-grave)
    (?\" . mute-diaeresis)
    (?^ . mute-asciicircum)
    (?\~ . mute-asciitilde)
    (?\' . dead-acute)
    (?\` . dead-grave)
    (?\" . dead-diaeresis)
    (?^ . dead-asciicircum)
    (?\~ . dead-asciitilde)
    (?^ . dead-circum)
    (?^ . dead-circumflex)
    (?\~ . dead-tilde)
    ;; Someone reports that these keys don't work if shifted.
    ;; This might fix it--no word yet.
    (?\' . S-dead-acute)
    (?\` . S-dead-grave)
    (?\" . S-dead-diaeresis)
    (?^ . S-dead-asciicircum)
    (?\~ . S-dead-asciitilde)
    (?^ . S-dead-circum)
    (?^ . S-dead-circumflex)
    (?\~ . S-dead-tilde))
  "Mapping of ASCII characters to their corresponding dead-key symbols.")

;; The two-character mnemonics are intended to be available in all languages.
;; The ones beginning with `*' have one-character synonyms, but a
;; language-specific table might override the short form for its own use.

(defvar iso-transl-char-map
  '(("* "   . [?�])
    (" "    . [?�])
    ("*!"   . [?�])
    ("!"    . [?�])
    ("\"\"" . [?�])
    ("\"A"  . [?�])
    ("\"E"  . [?�])
    ("\"I"  . [?�])
    ("\"O"  . [?�])
    ("\"U"  . [?�])
    ("\"a"  . [?�])
    ("\"e"  . [?�])
    ("\"i"  . [?�])
    ("\"o"  . [?�])
    ("\"s"  . [?�])
    ("\"u"  . [?�])
    ("\"y"  . [?�])
    ("''"   . [?�])
    ("'A"   . [?�])
    ("'E"   . [?�])
    ("'I"   . [?�])
    ("'O"   . [?�])
    ("'U"   . [?�])
    ("'Y"   . [?�])
    ("'a"   . [?�])
    ("'e"   . [?�])
    ("'i"   . [?�])
    ("'o"   . [?�])
    ("'u"   . [?�])
    ("'y"   . [?�])
    ("*$"   . [?�])
    ("$"    . [?�])
    ("*+"   . [?�])
    ("+"    . [?�])
    (",,"   . [?�])
    (",C"   . [?�])
    (",c"   . [?�])
    ("*-"   . [?�])
    ("-"    . [?�])
    ("*."   . [?�])
    ("."    . [?�])
    ("//"   . [?�])
    ("/A"   . [?�])
    ("/E"   . [?�])
    ("/O"   . [?�])
    ("/a"   . [?�])
    ("/e"   . [?�])
    ("/o"   . [?�])
    ("1/2"  . [?�])
    ("1/4"  . [?�])
    ("3/4"  . [?�])
    ("*<"   . [?�])
    ("<"    . [?�])
    ("*="   . [?�])
    ("="    . [?�])
    ("*>"   . [?�])
    (">"    . [?�])
    ("*?"   . [?�])
    ("?"    . [?�])
    ("*C"   . [?�])
    ("C"    . [?�])
    ("*L"   . [?�])
    ("L"    . [?�])
    ("*P"   . [?�])
    ("P"    . [?�])
    ("*R"   . [?�])
    ("R"    . [?�])
    ("*S"   . [?�])
    ("S"    . [?�])
    ("*Y"   . [?�])
    ("Y"    . [?�])
    ("^1"   . [?�])
    ("^2"   . [?�])
    ("^3"   . [?�])
    ("^A"   . [?�])
    ("^E"   . [?�])
    ("^I"   . [?�])
    ("^O"   . [?�])
    ("^U"   . [?�])
    ("^a"   . [?�])
    ("^e"   . [?�])
    ("^i"   . [?�])
    ("^o"   . [?�])
    ("^u"   . [?�])
    ("_a"   . [?�])
    ("_o"   . [?�])
    ("`A"   . [?�])
    ("`E"   . [?�])
    ("`I"   . [?�])
    ("`O"   . [?�])
    ("`U"   . [?�])
    ("`a"   . [?�])
    ("`e"   . [?�])
    ("`i"   . [?�])
    ("`o"   . [?�])
    ("`u"   . [?�])
    ("*c"   . [?�])
    ("c"    . [?�])
    ("*o"   . [?�])
    ("o"    . [?�])
    ("*u"   . [?�])
    ("u"    . [?�])
    ("*m"   . [?�])
    ("m"    . [?�])
    ("*x"   . [?�])
    ("x"    . [?�])
    ("*|"   . [?�])
    ("|"    . [?�])
    ("~A"   . [?�])
    ("~D"   . [?�])
    ("~N"   . [?�])
    ("~O"   . [?�])
    ("~T"   . [?�])
    ("~a"   . [?�])
    ("~d"   . [?�])
    ("~n"   . [?�])
    ("~o"   . [?�])
    ("~t"   . [?�])
    ("~~"   . [?�])
    ("' "   . "'")
    ("` "   . "`")
    ("\" "  . "\"")
    ("^ "   . "^")
    ("~ "   . "~"))
  "Alist of character translations for entering ISO characters.
Each element has the form (STRING . VECTOR).
The sequence STRING of ASCII chars translates into the
sequence VECTOR.  (VECTOR is normally one character long.)")

;; Language-specific translation lists.
(defvar iso-transl-language-alist
  '(("Esperanto"
     ("C"  . [?�])
     ("G"  . [?�])
     ("H"  . [?�])
     ("J"  . [?�])
     ("S"  . [?�])
     ("U"  . [?�])
     ("c"  . [?�])
     ("g"  . [?�])
     ("h"  . [?�])
     ("j"  . [?�])
     ("s"  . [?�])
     ("u"  . [?�]))
    ("French"
     ("C"  . [?�])
     ("c"  . [?�]))
    ("German"
     ("A"  . [?�])
     ("O"  . [?�])
     ("U"  . [?�])
     ("a"  . [?�])
     ("o"  . [?�])
     ("s"  . [?�])
     ("u"  . [?�]))
    ("Portuguese"
     ("C"  . [?�])
     ("c"  . [?�]))
    ("Spanish"
     ("!"  . [?�])
     ("?"  . [?�])
     ("N"  . [?�])
     ("n"  . [?�]))))

(defvar iso-transl-ctl-x-8-map nil
  "Keymap for C-x 8 prefix.")
(or iso-transl-ctl-x-8-map
    (fset 'iso-transl-ctl-x-8-map
	  (setq iso-transl-ctl-x-8-map (make-sparse-keymap))))
(or key-translation-map
    (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\C-x8" iso-transl-ctl-x-8-map)

;; For each entry in the alist, we'll make up to three ways to generate
;; the character in question: the prefix `C-x 8'; the ALT modifier on
;; the first key of the sequence; and (if applicable) replacing the first
;; key of the sequence with the corresponding dead key.  For example, a
;; character associated with the string "~n" can be input with `C-x 8 ~ n'
;; or `Alt-~ n' or `mute-asciitilde n'.
(defun iso-transl-define-keys (alist)
  (while alist
    (let ((translated-vec (cdr (car alist))))
      (define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec)
      (let ((inchar (aref (car (car alist)) 0))
	    (vec (vconcat (car (car alist))))
	    (tail iso-transl-dead-key-alist))
	(aset vec 0 (logior (aref vec 0) ?\A-\^@))
	(define-key key-translation-map vec translated-vec)
	(define-key isearch-mode-map (vector (aref vec 0)) nil)
	(while tail
	  (if (eq (car (car tail)) inchar)
	      (let ((deadvec (copy-sequence vec))
		    (deadkey (cdr (car tail))))
		(aset deadvec 0 deadkey)
		(define-key isearch-mode-map (vector deadkey) nil)
		(define-key key-translation-map deadvec translated-vec)))
	  (setq tail (cdr tail)))))
    (setq alist (cdr alist))))

(defun iso-transl-set-language (lang)
  (interactive (list (let ((completion-ignore-case t))
		       (completing-read "Set which language? "
					iso-transl-language-alist nil t))))
  (iso-transl-define-keys (cdr (assoc lang iso-transl-language-alist))))


;; The standard mapping comes automatically.  You can partially overlay it
;; with a language-specific mapping by using `M-x iso-transl-set-language'.
(iso-transl-define-keys iso-transl-char-map)

(define-key isearch-mode-map "\C-x" nil)
(define-key isearch-mode-map [?\C-x t] 'isearch-other-control-char)
(define-key isearch-mode-map "\C-x8" nil)


(provide 'iso-transl)

;;; iso-transl.el ends here
