;;; tibet-util.el --- utilities for Tibetan   -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Author: Toru TOMABECHI <Toru.Tomabechi@orient.unil.ch>
;; Keywords: multilingual, Tibetan
;; Created: Feb. 17. 1997

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

;;; History:
;; 1997.03.13 Modification in treatment of text properties;
;;            Support for some special signs and punctuation.
;; 1999.10.25 Modification for a new composition way by K.Handa.

;;; Commentary:

;;; Code:

(defconst tibetan-obsolete-glyphs
  `(("$(7!=(B" . "$(7!=(B")			; 2 col <-> 1 col
    ("$(7!?(B" . "$(7!?(B")
    ("$(7!@(B" . "$(7!@(B")
    ("$(7!A(B" . "$(7!A(B")
    ("$(7"`(B" . "$(7"`(B")
    ("$(7!;(B" . "$(7!;(B")
    ("$(7!D(B" . "$(7!D(B")
    ;; Yes these are dirty. But ...
    ("$(7!>(B $(7!>(B" . ,(compose-string "$(7!>(B $(7!>(B" 0 3 [?$(7!>(B (Br . Bl) ?  (Br . Bl) ?$(7!>(B]))
    ("$(7!4!5!5(B" . ,(compose-string
		  "$(7#R#S#S#S(B" 0 4
		  [?$(7#R(B (Br . Bl) ?$(7#S(B (Br . Bl) ?$(7#S(B (Br . Bl) ?$(7#S(B]))
    ("$(7!4!5(B" . ,(compose-string "$(7#R#S#S(B" 0 3 [?$(7#R(B (Br . Bl) ?$(7#S(B (Br . Bl) ?$(7#S(B]))
    ("$(7!6(B" . ,(compose-string "$(7#R#S!I(B" 0 3 [?$(7#R(B (Br . Bl) ?$(7#S(B (br . tr) ?$(7!I(B]))
    ("$(7!4(B"   . ,(compose-string "$(7#R#S(B" 0 2 [?$(7#R(B (Br . Bl) ?$(7#S(B]))))

;;;###autoload
(defun tibetan-char-p (ch)
  "Check if char CH is Tibetan character.
Returns non-nil if CH is Tibetan. Otherwise, returns nil."
  (memq (char-charset ch) '(tibetan tibetan-1-column)))

;;; Functions for Tibetan <-> Tibetan-transcription.

;;;###autoload
(defun tibetan-tibetan-to-transcription (str)
  "Transcribe Tibetan string STR and return the corresponding Roman string."
  (let (;; Accumulate transcriptions here in reverse order.
	(trans nil)
	(len (length str))
	(i 0)
	ch this-trans)
    (while (< i len)
      (let ((idx (string-match tibetan-precomposition-rule-regexp str i)))
	(if (eq idx i)
	    ;; Ith character and the followings matches precomposable
	    ;; Tibetan sequence.
	    (setq i (match-end 0)
		  this-trans
		  (car (rassoc
			(cdr (assoc (match-string 0 str)
				    tibetan-precomposition-rule-alist))
			tibetan-precomposed-transcription-alist)))
	  (setq ch (substring str i (1+ i))
		i (1+ i)
		this-trans
		(car (or (rassoc ch tibetan-consonant-transcription-alist)
			 (rassoc ch tibetan-vowel-transcription-alist)
			 (rassoc ch tibetan-subjoined-transcription-alist)))))
	(setq trans (cons this-trans trans))))
    (apply 'concat (nreverse trans))))

;;;###autoload
(defun tibetan-transcription-to-tibetan (str)
  "Convert Tibetan Roman string STR to Tibetan character string.
The returned string has no composition information."
  (let (;; Case is significant.
	(case-fold-search nil)
	(idx 0)
	;; Accumulate Tibetan strings here in reverse order.
	(t-str-list nil)
	i subtrans)
    (while (setq i (string-match tibetan-regexp str idx))
      (if (< idx i)
	  ;; STR contains a pattern that doesn't match Tibetan
	  ;; transcription.  Include the pattern as is.
	  (setq t-str-list (cons (substring str idx i) t-str-list)))
      (setq subtrans (match-string 0 str)
	    idx (match-end 0))
      (let ((t-char (cdr (assoc subtrans
				tibetan-precomposed-transcription-alist))))
	(if t-char
	    ;; SUBTRANS corresponds to a transcription for
	    ;; precomposable Tibetan sequence.
	    (setq t-char (car (rassoc t-char
				      tibetan-precomposition-rule-alist)))
	  (setq t-char
		(cdr
		 (or (assoc subtrans tibetan-consonant-transcription-alist)
		     (assoc subtrans tibetan-vowel-transcription-alist)
		     (assoc subtrans tibetan-modifier-transcription-alist)
		     (assoc subtrans tibetan-subjoined-transcription-alist)))))
	(setq t-str-list (cons t-char t-str-list))))
    (if (< idx (length str))
	(setq t-str-list (cons (substring str idx) t-str-list)))
    (apply 'concat (nreverse t-str-list))))

;;;
;;; Functions for composing/decomposing Tibetan sequence.
;;;
;;; A Tibetan syllable is typically structured as follows:
;;;
;;;      [Prefix] C [C+] V [M] [Suffix [Post suffix]]
;;;
;;; where C's are all vertically stacked, V appears below or above
;;; consonant cluster and M is always put above the C[C+]V combination.
;;; (Sanskrit visarga, though it is a vowel modifier, is considered
;;;  to be a punctuation.)
;;;
;;; Here are examples of the words "bsgrubs" and "hfauM"
;;;
;;;            $(7"7"G###C"U"7"G(B            $(7"H"R"U"_(B
;;;
;;;                             M
;;;             b s b s         h
;;;               g             fa
;;;               r             u
;;;               u
;;;
;;; Consonants `'' ($(7"A(B), `w' ($(7">(B), `y' ($(7"B(B), `r' ($(7"C(B) take special
;;; forms when they are used as subjoined consonant.  Consonant `r'
;;; takes another special form when used as superjoined in such a case
;;; as "rka", while it does not change its form when conjoined with
;;; subjoined `'', `w' or `y' as in "rwa", "rya".

;; Append a proper composition rule and glyph to COMPONENTS to compose
;; CHAR with a composition that has COMPONENTS.

(defun tibetan-add-components (components char)
  (let ((last (last components))
	(stack-upper '(tc . bc))
	(stack-under '(bc . tc))
	rule comp-vowel tmp)
    ;; Special treatment for 'a chung.
    ;; If 'a follows a consonant, turn it into the subjoined form.
    ;; * Disabled by Tomabechi 2000/06/09 *
    ;; Because in Unicode, $(7"A(B may follow directly a consonant without
    ;; any intervening vowel, as in $(7"9"""Q"A!;(B=$(7"9(B $(7""(B $(7"A(B not $(7"9(B $(7""(B $(7"Q(B $(7"A(B
    ;;(if (and (= char ?$(7"A(B)
    ;;	     (aref (char-category-set (car last)) ?0))
    ;;	(setq char ?$(7"R(B)) ;; modified for new font by Tomabechi 1999/12/10

    ;; Composite vowel signs are decomposed before being added
    ;; Added by Tomabechi 2000/06/08
    (if (memq char '(?$(7"T(B ?$(7"V(B ?$(7"W(B ?$(7"X(B ?$(7"Y(B ?$(7"Z(B ?$(7"b(B))
	(setq comp-vowel
	      (copy-sequence
	       (cddr (assoc (char-to-string char)
			    tibetan-composite-vowel-alist)))
	      char
	      (cadr (assoc (char-to-string char)
			   tibetan-composite-vowel-alist))))
    (cond
     ;; Compose upper vowel sign vertically over.
     ((aref (char-category-set char) ?2)
      (setq rule stack-upper))

     ;; Compose lower vowel sign vertically under.
     ((aref (char-category-set char) ?3)
      (if (or (eq char ?$(7"Q(B) ;; `$(7"Q(B' and `$,1FP(B' should not visible when composed.
	      (eq char #xF70))
	  (setq rule nil)
	(setq rule stack-under)))
     ;; Transform ra-mgo (superscribed r) if followed by a subjoined
     ;; consonant other than w, ', y, r.
     ((and (= (car last) ?$(7"C(B)
	   (not (memq char '(?$(7#>(B ?$(7"R(B ?$(7#B(B ?$(7#C(B))))
      (setcar last ?$(7!"(B) ;; modified for newfont by Tomabechi 1999/12/10
      (setq rule stack-under))
     ;; Transform initial base consonant if followed by a subjoined
     ;; consonant but 'a.
     (t
      (let ((laststr (char-to-string (car last))))
	(if (and (/= char ?$(7"R(B) ;; modified for new font by Tomabechi
		 (string-match "[$(7"!(B-$(7"="?"@"D(B-$(7"J"K(B]" laststr))
	    (setcar last (string-to-char
			  (cdr (assoc (char-to-string (car last))
				      tibetan-base-to-subjoined-alist)))))
	(setq rule stack-under))))

    (if rule
	(setcdr last (list rule char)))
    ;; Added by Tomabechi 2000/06/08
    (if comp-vowel
	(nconc last comp-vowel))
    ))

;;;###autoload
(defun tibetan-compose-string (str)
  "Compose Tibetan string STR."
  (let ((idx 0))
    ;; `$(7"A(B' is included in the pattern for subjoined consonants
    ;; because we treat it specially in tibetan-add-components.
    ;; (This feature is removed by Tomabechi 2000/06/08)
    (while (setq idx (string-match tibetan-composable-pattern str idx))
      (let ((from idx)
	    (to (match-end 0))
	    components)
	(if (eq (string-match tibetan-precomposition-rule-regexp str idx) idx)
	    (setq idx (match-end 0)
		  components
		  (list (string-to-char
			 (cdr
			  (assoc (match-string 0 str)
				 tibetan-precomposition-rule-alist)))))
	  (setq components (list (aref str idx))
		idx (1+ idx)))
	(while (< idx to)
	  (tibetan-add-components components (aref str idx))
	  (setq idx (1+ idx)))
	(compose-string str from to components))))
  str)

;;;###autoload
(defun tibetan-compose-region (beg end)
  "Compose Tibetan text the region BEG and END."
  (interactive "r")
  (let (str result chars)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	;; `$(7"A(B' is included in the pattern for subjoined consonants
	;; because we treat it specially in tibetan-add-components.
	;; (This feature is removed by Tomabechi 2000/06/08)
	(while (re-search-forward tibetan-composable-pattern nil t)
	  (let ((from (match-beginning 0))
		(to (match-end 0))
		components)
	    (goto-char from)
	    (if (looking-at tibetan-precomposition-rule-regexp)
		(progn
		  (setq components
			(list (string-to-char
			       (cdr
				(assoc (match-string 0)
				       tibetan-precomposition-rule-alist)))))
		  (goto-char (match-end 0)))
	      (setq components (list (char-after from)))
	      (forward-char 1))
	    (while (< (point) to)
	      (tibetan-add-components components (following-char))
	      (forward-char 1))
	    (compose-region from to components)))))))

(defvar tibetan-decompose-precomposition-alist
  (mapcar (function (lambda (x) (cons (string-to-char (cdr x)) (car x))))
	  tibetan-precomposition-rule-alist))

;;;###autoload
(defun tibetan-decompose-region (from to)
  "Decompose Tibetan text in the region FROM and TO.
This is different from decompose-region because precomposed Tibetan characters
are decomposed into normal Tibetan character sequences."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (decompose-region from to)
    (goto-char from)
    (while (not (eobp))
      (let* ((char (following-char))
	     (slot (assq char tibetan-decompose-precomposition-alist)))
	(if slot
	    (progn
	      (delete-char 1)
	      (insert (cdr slot)))
	  (forward-char 1))))))


;;;###autoload
(defun tibetan-decompose-string (str)
  "Decompose Tibetan string STR.
This is different from decompose-string because precomposed Tibetan characters
are decomposed into normal Tibetan character sequences."
  (let ((new "")
	(len (length str))
	(idx 0)
	char slot)
    (while (< idx len)
      (setq char (aref str idx)
	    slot (assq (aref str idx) tibetan-decompose-precomposition-alist)
	    new (concat new (if slot (cdr slot) (char-to-string char)))
	    idx (1+ idx)))
    new))

;;;
;;; This variable is used to avoid repeated decomposition.
;;;
(setq-default tibetan-decomposed nil)

;;;###autoload
(defun tibetan-decompose-buffer ()
  "Decomposes Tibetan characters in the buffer into their components.
See also the documentation of the function `tibetan-decompose-region'."
  (interactive)
  (make-local-variable 'tibetan-decomposed)
  (cond ((not tibetan-decomposed)
	 (tibetan-decompose-region (point-min) (point-max))
	 (setq tibetan-decomposed t))))

;;;###autoload
(defun tibetan-compose-buffer ()
  "Composes Tibetan character components in the buffer.
See also docstring of the function tibetan-compose-region."
  (interactive)
  (make-local-variable 'tibetan-decomposed)
  (tibetan-compose-region (point-min) (point-max))
  (setq tibetan-decomposed nil))

;;;###autoload
(defun tibetan-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (let ((buffer-modified-p (buffer-modified-p)))
	(narrow-to-region (point) (+ (point) len))
	(tibetan-compose-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(make-local-variable 'tibetan-decomposed)
	(setq tibetan-decomposed nil)
	(- (point-max) (point-min))))))


;;;###autoload
(defun tibetan-pre-write-conversion (from to)
  (setq tibetan-decomposed-temp tibetan-decomposed)
  (let ((old-buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring old-buf from to))
    (if (not tibetan-decomposed-temp)
	(tibetan-decompose-region (point-min) (point-max)))
    ;; Should return nil as annotations.
    nil))


;;;
;;; Unicode-related definitions.
;;;

(defvar tibetan-canonicalize-for-unicode-alist
  '(("$(7"Q(B" . "")	;; remove vowel a
    ("$(7"T(B" . "$(7"R"S(B") ;; decompose vowels whose use is ``discouraged'' in Unicode 3.0
    ("$(7"V(B" . "$(7"R"U(B")
    ("$(7"W(B" . "$(7#C"a(B")
    ("$(7"X(B" . "$(7#C"R"a(B")
    ("$(7"Y(B" . "$(7#D"a(B")
    ("$(7"Z(B" . "$(7#D"R"a(B")
    ("$(7"b(B" . "$(7"R"a(B"))
  "Rules for canonicalizing Tibetan vowels for Unicode.")

(defvar tibetan-canonicalize-for-unicode-regexp
  "[$(7"Q"T"V"W"X"Y"Z"b(B]"
  "Regexp for Tibetan vowels to be canonicalized in Unicode.")

(defun tibetan-canonicalize-for-unicode-region (from to)
  (save-restriction
    (narrow-to-region from to)
    (goto-char from)
    (while (re-search-forward tibetan-canonicalize-for-unicode-regexp nil t)
      (let (
	    ;;(from (match-beginning 0))
	    ;;(to (match-end 0))
	    (canonical-form
	     (cdr (assoc (match-string 0)
			 tibetan-canonicalize-for-unicode-alist))))
	;;(goto-char from)
	;;(delete-region from to)
	;;(insert canonical-form)
	(replace-match canonical-form)
	))))

(defvar tibetan-strict-unicode t
  "*Flag to control Tibetan canonicalizing for Unicode.

If non-nil, the vowel a is removed and composite vowels are decomposed
before writing buffer in Unicode.  See also
`tibetan-canonicalize-for-unicode-regexp' and
`tibetan-canonicalize-for-unicode-alist'.")

;;;###autoload
(defun tibetan-pre-write-canonicalize-for-unicode (from to)
  (let ((old-buf (current-buffer))
	(strict-unicode tibetan-strict-unicode))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring old-buf from to))
    (if strict-unicode
	(tibetan-canonicalize-for-unicode-region (point-min) (point-max)))
    ;; Should return nil as annotations.
    nil))

(provide 'tibet-util)

;;; tibet-util.el ends here
