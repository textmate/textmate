;;; ethio-util.el --- utilities for Ethiopic	-*- coding: utf-8-emacs; -*-

;; Copyright (C) 1997-1998, 2002-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2005, 2006
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number: H15PRO110

;; Keywords: mule, multilingual, Ethiopic

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

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>

;;; Commentary:

;;; Code:

(require 'robin)

;; Information for exiting Ethiopic environment.
(defvar exit-ethiopic-environment-data nil)

;;;###autoload
(defun setup-ethiopic-environment-internal ()
  (let ((key-bindings '((" " . ethio-insert-space)
			([?\S- ] . ethio-insert-ethio-space)
			;; ([?\C-'] . ethio-gemination)
			([f3] . ethio-fidel-to-sera-buffer)
			([S-f3] . ethio-fidel-to-sera-region)
			([C-f3] . ethio-fidel-to-sera-marker)
			([f4] . ethio-sera-to-fidel-buffer)
			([S-f4] . ethio-sera-to-fidel-region)
			([C-f4] . ethio-sera-to-fidel-marker)
			;; ([S-f5] . ethio-toggle-punctuation)
			([S-f6] . ethio-modify-vowel)
			([S-f7] . ethio-replace-space)
			;; ([S-f8] . ethio-input-special-character) ; deprecated
			([C-f9] . ethio-toggle-space)
			([S-f9] . ethio-replace-space) ; as requested
			))
	kb)
    (while key-bindings
      (setq kb (car (car key-bindings)))
      (setq exit-ethiopic-environment-data
	    (cons (cons kb (global-key-binding kb))
		  exit-ethiopic-environment-data))
      (global-set-key kb (cdr (car key-bindings)))
      (setq key-bindings (cdr key-bindings))))

  (add-hook 'find-file-hook 'ethio-find-file)
  (add-hook 'write-file-functions 'ethio-write-file)
  (add-hook 'after-save-hook 'ethio-find-file))

(defun exit-ethiopic-environment ()
  "Exit Ethiopic language environment."
  (while exit-ethiopic-environment-data
    (global-set-key (car (car exit-ethiopic-environment-data))
		    (cdr (car exit-ethiopic-environment-data)))
    (setq exit-ethiopic-environment-data
	  (cdr exit-ethiopic-environment-data)))

  (remove-hook 'find-file-hook 'ethio-find-file)
  (remove-hook 'write-file-functions 'ethio-write-file)
  (remove-hook 'after-save-hook 'ethio-find-file))

;;
;; ETHIOPIC UTILITY FUNCTIONS
;;

;; If the filename ends in ".sera", editing is done in fidel
;; but file I/O is done in SERA.
;;
;; If the filename ends in ".java", editing is done in fidel
;; but file I/O is done in the \uXXXX style, where XXXX is
;; the Unicode codepoint for the Ethiopic character.
;;
;; If the filename ends in ".tex", editing is done in fidel
;; but file I/O is done in EthioTeX format.

;;
;; users' preference
;;

(defvar ethio-primary-language 'tigrigna
  "*Symbol that defines the primary language in SERA --> FIDEL conversion.
The value should be one of: `tigrigna', `amharic' or `english'.")

(defvar ethio-secondary-language 'english
  "*Symbol that defines the secondary language in SERA --> FIDEL conversion.
The value should be one of: `tigrigna', `amharic' or `english'.")

(defvar ethio-use-colon-for-colon nil
  "*Non-nil means associate ASCII colon with Ethiopic colon.
If nil, associate ASCII colon with Ethiopic word separator, i.e., two
vertically stacked dots.  All SERA <--> FIDEL converters refer this
variable.")

(defvar ethio-use-three-dot-question nil
  "*Non-nil means associate ASCII question mark with Ethiopic old style question mark (three vertically stacked dots).
If nil, associate ASCII question mark with Ethiopic stylized question
mark.  All SERA <--> FIDEL converters refer this variable.")

(defvar ethio-quote-vowel-always nil
  "*Non-nil means always put an apostrophe before an isolated vowel (except at word initial) in FIDEL --> SERA conversion.
If nil, put an apostrophe only between a 6th-form consonant and an
isolated vowel.")

(defvar ethio-W-sixth-always nil
  "*Non-nil means convert the Wu-form of a 12-form consonant to \"W'\" instead of \"Wu\" in FIDEL --> SERA conversion.")

(defvar ethio-numeric-reduction 0
  "*Degree of reduction in converting Ethiopic digits into Arabic digits.
Should be 0, 1 or 2.
For example, ({10}{9}{100}{80}{7}) is converted into:
    `10`9`100`80`7  if `ethio-numeric-reduction' is 0,
    `109100807	    if `ethio-numeric-reduction' is 1,
    `10900807	    if `ethio-numeric-reduction' is 2.")

(defvar ethio-java-save-lowercase nil
  "*Non-nil means save Ethiopic characters in lowercase hex numbers to Java files.
If nil, use uppercases.")

(defun ethio-prefer-amharic-p ()
  (or (eq ethio-primary-language 'amharic)
      (and (not (eq ethio-primary-language 'tigrigna))
	   (eq ethio-secondary-language 'amharic))))

(defun ethio-prefer-amharic (arg)
  (if arg
      (progn
	(robin-modify-package "ethiopic-sera" "'a" ?አ)
	(robin-modify-package "ethiopic-sera" "a" "አ")
	(robin-modify-package "ethiopic-sera" "'A" ?ኣ)
	(robin-modify-package "ethiopic-sera" "A" "ኣ"))
    (robin-modify-package "ethiopic-sera" "'A" ?አ)
    (robin-modify-package "ethiopic-sera" "A" "አ")
    (robin-modify-package "ethiopic-sera" "'a" ?ኣ)
    (robin-modify-package "ethiopic-sera" "a" "ኣ")))

(defun ethio-use-colon-for-colon (arg)
  (if arg
      (progn
	(robin-modify-package "ethiopic-sera" ":" ?፥)
	(robin-modify-package "ethiopic-sera" "`:" ?፡))
    (robin-modify-package "ethiopic-sera" " : " ?፡)
    (robin-modify-package "ethiopic-sera" ":" "፡")
    (robin-modify-package "ethiopic-sera" "-:" ?፥)))

(defun ethio-use-three-dot-question (arg)
  (if arg
      (progn
	(robin-modify-package "ethiopic-sera" "?" ?፧)
	(robin-modify-package "ethiopic-sera" "`?" ??))
    (robin-modify-package "ethiopic-sera" "?" ??)
    (robin-modify-package "ethiopic-sera" "`?" ?፧)))

(defun ethio-adjust-robin ()
  (ethio-prefer-amharic (ethio-prefer-amharic-p))
  (ethio-use-colon-for-colon ethio-use-colon-for-colon)
  (ethio-use-three-dot-question ethio-use-three-dot-question))

(add-hook 'robin-activate-hook 'ethio-adjust-robin)

;;
;; SERA to FIDEL
;;

;;;###autoload
(defun ethio-sera-to-fidel-buffer (&optional secondary force)
  "Convert the current buffer from SERA to FIDEL.

The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 1st optional argument SECONDARY is non-nil, assume the
buffer begins with the secondary language; otherwise with the
primary language.

If the 2nd optional argument FORCE is non-nil, perform conversion
even if the buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon' and `ethio-use-three-dot-question'."

  (interactive "P")
  (ethio-sera-to-fidel-region (point-min) (point-max) secondary force))

;; To avoid byte-compiler warnings.  It should never be set globally.
(defvar ethio-sera-being-called-by-w3)
;; This variable will be bound by some third-party package.
(defvar sera-being-called-by-w3)

;;;###autoload
(defun ethio-sera-to-fidel-region (begin end &optional secondary force)
  "Convert the characters in region from SERA to FIDEL.

The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 3rd argument SECONDARY is given and non-nil, assume the
region begins with the secondary language; otherwise with the
primary language.

If the 4th argument FORCE is given and non-nil, perform
conversion even if the buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon' and `ethio-use-three-dot-question'."

  (interactive "r\nP")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))

  (let ((ethio-primary-language ethio-primary-language)
	(ethio-secondary-language ethio-secondary-language)
	;; The above two variables may be changed temporarily by tilde
	;; escapes during conversion.  We bind them to the variables
	;; of the same names so that the original values are restored
	;; when this function exits.
	(buffer-read-only nil)
	(lang (if secondary ethio-secondary-language ethio-primary-language))
	ret)

    (ethio-use-colon-for-colon ethio-use-colon-for-colon)
    (ethio-use-three-dot-question ethio-use-three-dot-question)

    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (not (eobp))
	(setq ret
	      (cond
	       ((eq lang 'amharic)
		(ethio-prefer-amharic t)
		(ethio-sera-to-fidel-region-ethio 'amharic))
	       ((eq lang 'tigrigna)
		(ethio-prefer-amharic nil)
		(ethio-sera-to-fidel-region-ethio 'tigrigna))
	       (t
		(ethio-sera-to-fidel-region-noethio))))
	(setq lang
	      (if (eq ret 'toggle)
		  (if (eq lang ethio-primary-language)
		      ethio-secondary-language
		    ethio-primary-language)
		ret)))))

  ;; Restore user's preference.
  (ethio-adjust-robin))

(defun ethio-sera-to-fidel-region-noethio ()
  "Return next language as symbol: amharic, tigrigna, toggle or nil."
  (let (lflag)
    (cond

     ;; No more "\", i.e. nothing to do.
     ((not (search-forward "\\" nil 0))
      nil)

     ;; Hereafter point is put after a "\".
     ;; First delete that "\", then check the following chars.

     ;; A language flag.
     ((progn (delete-char -1) (setq lflag (ethio-process-language-flag)))
      lflag)

     ;; "\\" : leave the second "\" and continue in the same language.
     ((= (following-char) ?\\)
      (forward-char 1)
      nil)

     ;; "\ " : delete the following " " and toggle the language.
     ((= (following-char) 32)
      (delete-char 1)
      'toggle)

     ;; A  "\" but not a special sequence: simply toggle the language.
     (t
      'toggle))))

(defun ethio-sera-to-fidel-region-ethio (lang)
  "Return next language as symbol: amharic, tigrigna, toggle or nil."
  (save-restriction
    (narrow-to-region
     (point)
     (if (re-search-forward "\\(`[1-9][0-9]*\\)\\|[\\<&]" nil t)
	 (match-beginning 0)
       (point-max)))
    (robin-convert-region (point-min) (point-max) "ethiopic-sera")
    (goto-char (point-max)))

  (let (lflag)
    (cond
     ((= (following-char) ?`)
      (delete-char 1)
      (ethio-process-digits)
      lang)

     ((looking-at "[<&]")
      (if (or (and (boundp 'ethio-sera-being-called-by-w3)
		   ethio-sera-being-called-by-w3)
	      (and (boundp 'sera-being-called-by-w3)
		   sera-being-called-by-w3))
	  (search-forward (if (= (following-char) ?<) ">" ";") nil 0)
	(forward-char 1))
      lang)

     ((eobp)
      nil)

     ;; Now we must be looking at a "\".
     ;; First delete that "\", then check the following chars.

     ((progn (delete-char 1) (= (following-char) 32))
      (delete-char 1)
      'toggle)

     ((looking-at "[,.;:'`?\\]+")
      (goto-char (match-end 0))
      lang)

     ((/= (following-char) ?~)
      'toggle)

     ;; Now we must be looking at a "~".

     ((setq lflag (ethio-process-language-flag))
      lflag)

     ;; Delete the following "~" and check the following chars.

     ((progn (delete-char 1) (looking-at "! ?"))
      (replace-match "")
      (if (re-search-forward "\\\\~! ?" nil 0)
	  (replace-match ""))
      lang)

     ((looking-at "-: ?")
      (replace-match "")
      (ethio-use-colon-for-colon t)
      lang)

     ((looking-at "`: ?")
      (replace-match "")
      (ethio-use-colon-for-colon nil)
      lang)

     ((looking-at "`| ?")
      (replace-match "")
      (ethio-use-three-dot-question t)
      lang)

     ((looking-at "\\? ?")
      (replace-match "")
      (ethio-use-three-dot-question nil)
      lang)

     ;; Unknown tilde escape.  Recover the deleted chars.
     (t
      (insert "\\~")
      lang))))

(defun ethio-process-language-flag nil
  "Process a language flag of the form \"~lang\" or \"~lang1~lang2\".

If looking at \"~lang1~lang2\", set `ethio-primary-language' and
`ethio-secondary-language' based on \"lang1\" and \"lang2\".
Then delete the language flag \"~lang1~lang2\" from the buffer.
Return value is the new primary language.

If looking at \"~lang\", delete that language flag \"~lang\" from
the buffer and return that language.  In this case
`ethio-primary-language' and `ethio-secondary-language' are left
unchanged.

If an unsupported language flag is found, just return nil without
changing anything."

  (let (lang1 lang2)
    (cond

     ;; ~lang1~lang2
     ((and (looking-at
	    "~\\([a-z][a-z][a-z]?\\)~\\([a-z][a-z][a-z]?\\)[ \t\n\\]")
	   (setq lang1 (ethio-flag-to-language (match-string 1)))
	   (setq lang2 (ethio-flag-to-language (match-string 2))))
      (setq ethio-primary-language lang1
	    ethio-secondary-language lang2)
      (delete-region (point) (match-end 2))
      (if (= (following-char) 32)
	  (delete-char 1))
      ethio-primary-language)

     ;; ~lang
     ((and (looking-at "~\\([a-z][a-z][a-z]?\\)[ \t\n\\]")
	   (setq lang1 (ethio-flag-to-language (match-string 1))))
      (delete-region (point) (match-end 1))
      (if (= (following-char) 32)
	  (delete-char 1))
      lang1)

     ;; otherwise
     (t
      nil))))

(defun ethio-flag-to-language (flag)
  (cond
   ((or (string= flag "en") (string= flag "eng")) 'english)
   ((or (string= flag "ti") (string= flag "tir")) 'tigrigna)
   ((or (string= flag "am") (string= flag "amh")) 'amharic)
   (t nil)))

(defun ethio-process-digits nil
  "Convert Arabic digits to Ethiopic digits."
  (let (ch z)
    (while (and (>= (setq ch (following-char)) ?1)
		(<= ch ?9))
      (delete-char 1)

      ;; count up following zeros
      (setq z 0)
      (while (= (following-char) ?0)
	(delete-char 1)
	(setq z (1+ z)))

      (cond

       ;; first digit is 10, 20, ..., or 90
       ((= (mod z 2) 1)
	(insert (aref [?፲ ?፳ ?፴ ?፵ ?፶ ?፷ ?፸ ?፹ ?፺] (- ch ?1)))
	(setq z (1- z)))

       ;; first digit is 2, 3, ..., or 9
       ((/= ch ?1)
	(insert (aref [?፪ ?፫ ?፬ ?፭ ?፮ ?፯ ?፰ ?፱] (- ch ?2))))

       ;; single 1
       ((= z 0)
	(insert "፩")))

      ;; 100
      (if (= (mod z 4) 2)
	  (insert "፻"))

      ;; 10000
      (insert-char ?፼ (/ z 4)))))

;;;###autoload
(defun ethio-sera-to-fidel-marker (&optional force)
  "Convert the regions surrounded by \"<sera>\" and \"</sera>\" from SERA to FIDEL.
Assume that each region begins with `ethio-primary-language'.
The markers \"<sera>\" and \"</sera>\" themselves are not deleted."
  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<sera>" nil t)
      (ethio-sera-to-fidel-region
       (point)
       (if (search-forward "</sera>" nil t)
	   (match-beginning 0)
	 (point-max))
       nil
       'force))))

;;
;; FIDEL to SERA
;;

(defun ethio-language-to-flag (lang)
  (cond
   ((eq lang 'english) "eng")
   ((eq lang 'tigrigna) "tir")
   ((eq lang 'amharic) "amh")
   (t "")))

;;;###autoload
(defun ethio-fidel-to-sera-buffer (&optional secondary force)
  "Replace all the FIDEL characters in the current buffer to the SERA format.
The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 1st optional argument SECONDARY is non-nil, try to convert the
region so that it begins with the secondary language; otherwise with the
primary language.

If the 2nd optional argument FORCE is non-nil, convert even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon', `ethio-use-three-dot-question',
`ethio-quote-vowel-always' and `ethio-numeric-reduction'."

  (interactive "P")
  (ethio-fidel-to-sera-region (point-min) (point-max) secondary force))

;;;###autoload
(defun ethio-fidel-to-sera-region (begin end &optional secondary force)
  "Replace all the FIDEL characters in the region to the SERA format.

The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 3rd argument SECONDARY is given and non-nil, convert
the region so that it begins with the secondary language; otherwise with
the primary language.

If the 4th argument FORCE is given and non-nil, convert even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon', `ethio-use-three-dot-question',
`ethio-quote-vowel-always' and `ethio-numeric-reduction'."

  (interactive "r\nP")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))

  (save-restriction
    (narrow-to-region begin end)

    (let ((buffer-read-only nil)
	  (mode (if secondary
		    ethio-secondary-language
		  ethio-primary-language))
	  (flag (if (ethio-prefer-amharic-p) "\\~amh " "\\~tir "))
	  p ch)

      (goto-char (point-min))
      (ethio-adjust-robin)
      (unless (eq mode 'english)
	(setq mode 'ethiopic))
      (if (and (eq mode 'english) (looking-at "\\ce"))
	  (setq mode 'ethiopic))
      (if (and (eq mode 'ethiopic) (looking-at "\\Ce"))
	  (setq mode 'english))
      (insert (if (eq mode 'english) "\\~eng " flag))

      (while (not (eobp))

	(if (eq mode 'english)
	    (progn
	      (if (re-search-forward "\\(\\ce\\|\\\\\\)" nil 0)
		  (forward-char -1))
	      (cond
	       ((eq (following-char) ?\\)
		(insert "\\")
		(forward-char 1))
	       ((looking-at "\\ce")
		(insert flag)
		(setq mode 'ethiopic))))

	  ;; If we reach here, mode is ethiopic.
	  (setq p (point))
	  (if (re-search-forward "[a-z,.;:'`?\\<&]" nil 0)
	      (forward-char -1))
	  (save-restriction
	    (narrow-to-region p (point))
	    (robin-invert-region (point-min) (point-max) "ethiopic-sera")

	    ;; ethio-quote-vowel-always
	    (goto-char (point-min))
	    (while (re-search-forward "'[eauio]" nil t)
	      (save-excursion
		(forward-char -2)
		(setq ch (preceding-char))
		(if (or (and (>= ch ?a) (<= ch ?z))
			(and (>= ch ?A) (<= ch ?Z)))
		    (if (and (not ethio-quote-vowel-always)
			     (memq ch '(?e ?a ?u ?i ?o ?E ?A ?I)))
			(delete-char 1))
		  (delete-char 1))))

	    ;; ethio-W-sixth-always
	    (unless ethio-W-sixth-always
	      (goto-char (point-min))
	      (while (search-forward "W'" nil t)
		(delete-char -1)
		(insert "u")))

	    ;; ethio-numeric-reduction
	    (when (> ethio-numeric-reduction 0)
	      (goto-char (point-min))
	      (while (re-search-forward "\\([0-9]\\)`\\([0-9]\\)" nil t)
		(replace-match "\\1\\2")
		(forward-char -1)))
	    (when (= ethio-numeric-reduction 2)
	      (goto-char (point-min))
	      (while (re-search-forward "\\([0-9]\\)1\\(0+\\)" nil t)
		(replace-match "\\1\\2")))

	    (goto-char (point-max)))

	  (cond
	   ((looking-at "[a-z]")
	    (insert"\\~eng ")
	    (setq mode 'english))
	   ((looking-at "[,.;:'`\\]+")
	    (insert "\\")
	    (goto-char (1+ (match-end 0))))
	   ((= (following-char) ??)
	    (if ethio-use-three-dot-question
		(insert "\\"))
	    (forward-char 1))
	   ((looking-at "[<&]")
	    (if (or (and (boundp 'ethio-sera-being-called-by-w3)
			 ethio-sera-being-called-by-w3)
		    (and (boundp 'sera-being-called-by-w3)
			 sera-being-called-by-w3))
		(search-forward (if (= (following-char) ?<) ">" "&") nil 0)
	      (forward-char 1)))))))))

;;;###autoload
(defun ethio-fidel-to-sera-marker (&optional force)
  "Convert the regions surrounded by \"<sera>\" and \"</sera>\" from FIDEL to SERA.
The markers \"<sera>\" and \"</sera>\" themselves are not deleted."

  (interactive "P")
  (if (and buffer-read-only
	   (not force)
	   (not (y-or-n-p "Buffer is read-only.  Force to convert? ")))
      (error ""))
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<sera>" nil t)
      (ethio-fidel-to-sera-region
       (point)
       (if (search-forward "</sera>" nil t)
	   (match-beginning 0)
	 (point-max))
       nil
       'force))))

;;
;; vowel modification
;;

;;;###autoload
(defun ethio-modify-vowel nil
  "Modify the vowel of the FIDEL that is under the cursor."
  (interactive)
  (ethio-adjust-robin)
  (let ((consonant (ethio-get-consonant (following-char)))
	vowel)
    (if (null consonant)
	(error "")			; not an Ethiopic char
      (setq vowel (read-char "Modify vowel to: "))
      (delete-char 1)
      (if (and (string= consonant "'") (= vowel ?W))
	  (insert ?ኧ)
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert consonant vowel)
	  (robin-convert-region (point-min) (point-max) "ethiopic-sera"))))))

(defun ethio-get-consonant (ch)
  "Return the consonant part of CH's SERA spelling in ethiopic-sera."
  (let ((sera (get-char-code-property ch 'ethiopic-sera)))
    (cond
     ((null sera) nil)
     ((= ch ?ኧ) "'")			; Only this has two vowel letters.
     (t (with-temp-buffer
	  (insert sera)
	  (if (memq (preceding-char) '(?e ?u ?i ?a ?o ?E ?I ?A ?'))
	      (delete-char -1))
	  (buffer-substring (point-min) (point-max)))))))

;;
;; space replacement
;;

;;;###autoload
(defun ethio-replace-space (ch begin end)
  "Replace ASCII spaces with Ethiopic word separators in the region.

In the specified region, replace word separators surrounded by two
Ethiopic characters, depending on the first argument CH, which should
be 1, 2, or 3.

If CH = 1, word separator will be replaced with an ASCII space.
If CH = 2, with two ASCII spaces.
If CH = 3, with the Ethiopic colon-like word separator.

The 2nd and 3rd arguments BEGIN and END specify the region."

  (interactive "*cReplace spaces to: 1 (sg col), 2 (dbl col), 3 (Ethiopic)\nr")
  (if (not (memq ch '(?1 ?2 ?3)))
      (error ""))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)

      (cond
       ((= ch ?1)
	;; an Ethiopic word separator --> an ASCII space
	(goto-char (point-min))
	(while (search-forward "፡" nil t)
	  (replace-match " "))

	;; two ASCII spaces between Ethiopic characters --> an ASCII space
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\)  \\(\\ce\\)" nil t)
	  (replace-match "\\1 \\2")
	  (forward-char -1)))

       ((= ch ?2)
	;; An Ethiopic word separator --> two ASCII spaces
	(goto-char (point-min))
	(while (search-forward "፡" nil t)
	  (replace-match "  "))

	;; An ASCII space between Ethiopic characters --> two ASCII spaces
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\) \\(\\ce\\)" nil t)
	  (replace-match "\\1  \\2")
	  (forward-char -1)))

       (t
	;; One or two ASCII spaces between Ethiopic characters
	;;   --> An Ethiopic word separator
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\)  ?\\(\\ce\\)" nil t)
	  (replace-match "\\1፡\\2")
	  (forward-char -1))

	;; Three or more ASCII spaces between Ethiopic characters
	;;   --> An Ethiopic word separator + (N - 2) ASCII spaces
	(goto-char (point-min))
	(while (re-search-forward "\\(\\ce\\)  \\( +\\ce\\)" nil t)
	  (replace-match "\\1፡\\2")
	  (forward-char -1)))))))

;;
;; special icons
;;

;; This function is deprecated.
;;;###autoload
(defun ethio-input-special-character (arg)
  "This function is deprecated."
  (interactive "*cInput number: 1.����  2.����  3.����  4.����  5.����")
  (cond
   ((= arg ?1)
    (insert "����"))
   ((= arg ?2)
    (insert "����"))
   ((= arg ?3)
    (insert "����"))
   ((= arg ?4)
    (insert "����"))
   ((= arg ?5)
    (insert "����"))
   (t
    (error ""))))

;;
;; TeX support
;;

;;;###autoload
(defun ethio-fidel-to-tex-buffer nil
  "Convert each fidel characters in the current buffer into a fidel-tex command."
  (interactive)
  (let ((buffer-read-only nil)
	comp ch)

    ;; Special treatment for geminated characters.
    ;; Geminated characters la", etc. change into \geminateG{\laG}, etc.
    (goto-char (point-min))
    (while (re-search-forward "፟\\|����" nil t)
      (setq comp (find-composition (match-beginning 0)))
      (if (null comp)
	  (replace-match "\\\\geminateG{}" t)
	(decompose-region (car comp) (cadr comp))
	(delete-char -1)
	(forward-char -1)
	(insert "\\geminateG{")
	(forward-char 1)
	(insert "}")))

    ;; Special Ethiopic punctuation.
    (goto-char (point-min))
    (while (re-search-forward "\\ce[»\\.\\?]\\|«\\ce" nil t)
      (cond
       ((= (setq ch (preceding-char)) ?\»)
	(delete-char -1)
	(insert "\\rquoteG"))
       ((= ch ?.)
	(delete-char -1)
	(insert "\\dotG"))
       ((= ch ??)
	(delete-char -1)
	(insert "\\qmarkG"))
       (t
	(forward-char -1)
	(delete-char -1)
	(insert "\\lquoteG")
	(forward-char 1))))

    ;; Ethiopic characters to TeX macros
    (robin-invert-region (point-min) (point-max) "ethiopic-tex")

    (goto-char (point-min))
    (set-buffer-modified-p nil)))

;;;###autoload
(defun ethio-tex-to-fidel-buffer nil
  "Convert fidel-tex commands in the current buffer into fidel chars."
  (interactive)
  (let ((buffer-read-only nil)
	(p) (ch))

    ;; TeX macros to Ethiopic characters
    (robin-convert-region (point-min) (point-max) "ethiopic-tex")

    ;; compose geminated characters
    (goto-char (point-min))
    (while (re-search-forward "\\\\geminateG{\\(\\ce?\\)}" nil t)
      (replace-match "\\1፟"))

    ;; remove redundant braces, if any
    (goto-char (point-min))
    (while (re-search-forward "{\\(\\ce\\)}" nil t)
      (replace-match "\\1"))

    (goto-char (point-min))
    (set-buffer-modified-p nil)))

;;
;; Java support
;;

;;;###autoload
(defun ethio-fidel-to-java-buffer nil
  "Convert Ethiopic characters into the Java escape sequences.

Each escape sequence is of the form \\uXXXX, where XXXX is the
character's codepoint (in hex) in Unicode.

If `ethio-java-save-lowercase' is non-nil, use [0-9a-f].
Otherwise, [0-9A-F]."
  (let ((ucode))

    (goto-char (point-min))
    (while (re-search-forward "[ሀ-፼]" nil t)
      (setq ucode (preceding-char))
      (delete-char -1)
      (insert
       (format (if ethio-java-save-lowercase "\\u%4x" "\\u%4X")
	       ucode)))))

;;;###autoload
(defun ethio-java-to-fidel-buffer nil
  "Convert the Java escape sequences into corresponding Ethiopic characters."
  (let ((case-fold-search t)
	(ucode))
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\)" nil t)
      (setq ucode (read (concat "#x" (match-string 1))))
      (when (and (>= ucode #x1200) (<= ucode #x137f))
	(replace-match (char-to-string ucode))))))

;;
;; file I/O hooks
;;

;;;###autoload
(defun ethio-find-file nil
  "Transliterate file content into Ethiopic depending on filename suffix."
  (cond

   ((string-match "\\.sera$" (buffer-file-name))
    (save-excursion
      (ethio-sera-to-fidel-buffer nil 'force)
      (set-buffer-modified-p nil)))

   ((string-match "\\.html$" (buffer-file-name))
    (let ((ethio-sera-being-called-by-w3 t))
      (save-excursion
	(ethio-sera-to-fidel-marker 'force)
	(goto-char (point-min))
	(while (re-search-forward "&[lr]aquo;" nil t)
	  (if (= (char-after (1+ (match-beginning 0))) ?l)
	      (replace-match "«")
	    (replace-match "»")))
	(set-buffer-modified-p nil))))

   ((string-match "\\.tex$" (buffer-file-name))
    (save-excursion
      (ethio-tex-to-fidel-buffer)
      (set-buffer-modified-p nil)))

   ((string-match "\\.java$" (buffer-file-name))
    (save-excursion
      (ethio-java-to-fidel-buffer)
      (set-buffer-modified-p nil)))

   (t
    nil)))

;;;###autoload
(defun ethio-write-file nil
  "Transliterate Ethiopic characters in ASCII depending on the file extension."
  (cond

   ((string-match "\\.sera$" (buffer-file-name))
    (save-excursion
      (ethio-fidel-to-sera-buffer nil 'force)
      (goto-char (point-min))
      (ethio-record-user-preference)
      (set-buffer-modified-p nil)))

   ((string-match "\\.html$" (buffer-file-name))
    (save-excursion
      (let ((ethio-sera-being-called-by-w3 t))
	(ethio-fidel-to-sera-marker 'force)
	(goto-char (point-min))
	(while (re-search-forward "[«»]" nil t)
	  (replace-match (if (= (preceding-char) ?«) "&laquo;" "&raquo;")))
	(goto-char (point-min))
	(if (search-forward "<sera>" nil t)
	    (ethio-record-user-preference))
	(set-buffer-modified-p nil))))

   ((string-match "\\.tex$" (buffer-file-name))
    (save-excursion
      (ethio-fidel-to-tex-buffer)
      (set-buffer-modified-p nil)))

   ((string-match "\\.java$" (buffer-file-name))
    (save-excursion
      (ethio-fidel-to-java-buffer)
      (set-buffer-modified-p nil)))

   (t
    nil)))

(defun ethio-record-user-preference nil
  (insert (if ethio-use-colon-for-colon "\\~-: " "\\~`: ")
	  (if ethio-use-three-dot-question "\\~`| " "\\~? ")))

;;
;; Ethiopic word separator vs. ASCII space
;;

(defvar ethio-prefer-ascii-space t)
(make-variable-buffer-local 'ethio-prefer-ascii-space)

(defun ethio-toggle-space nil
  "Toggle ASCII space and Ethiopic separator for keyboard input."
  (interactive)
  (setq ethio-prefer-ascii-space
	(not ethio-prefer-ascii-space)))

(defun ethio-insert-space (arg)
  "Insert ASCII spaces or Ethiopic word separators depending on context.

If the current word separator (indicated in mode-line) is the ASCII space,
insert an ASCII space.  With ARG, insert that many ASCII spaces.

If the current word separator is the colon-like Ethiopic word
separator and the point is preceded by `an Ethiopic punctuation mark
followed by zero or more ASCII spaces', then insert also an ASCII
space.  With ARG, insert that many ASCII spaces.

Otherwise, insert a colon-like Ethiopic word separator.  With ARG, insert that
many Ethiopic word separators."

  (interactive "*p")
  (cond
   (ethio-prefer-ascii-space
    (insert-char 32 arg))
   ((save-excursion
      (skip-chars-backward " ")
      (memq (preceding-char)
	    '(?፡ ?። ?፣ ?፤ ?፥ ?፦ ?፧ ?፨ ?���� ?���� ?���� ?���� ?����)))
    (insert-char 32 arg))
   (t
    (insert-char ?፡ arg))))

;;;###autoload
(defun ethio-insert-ethio-space (arg)
  "Insert the Ethiopic word delimiter (the colon-like character).
With ARG, insert that many delimiters."
  (interactive "*p")
  (insert-char ?፡ arg))

;;
;; Gemination
;;

;;;###autoload
(defun ethio-composition-function (pos to font-object string)
  (setq pos (1- pos))
  (let ((pattern "\\ce\\(፟\\|����\\)"))
    (if string
	(if (and (>= pos 0)
		 (eq (string-match pattern string pos) pos))
	    (prog1 (match-end 0)
	      (compose-string string pos (match-end 0))))
      (if (>= pos (point-min))
	  (progn
	    (goto-char pos)
	    (if (looking-at pattern)
		(prog1 (match-end 0)
		  (compose-region pos (match-end 0)))))))))

;; This function is not used any more.
(defun ethio-gemination nil
  "Compose the character before the point with the Ethiopic gemination mark.
If the character is already composed, decompose it and remove the gemination
mark."
  (interactive "*")
  (let ((ch (preceding-char)))
    (cond
     ((and (= ch ?����) (find-composition (1- (point))))
      (decompose-region (- (point) 2) (point)))
     ((and (>= ch #x1200) (<= ch #x137f))
      (insert "����")
      (compose-region (- (point) 2) (point)))
     (t
      (error "")))))

;;;
;;; Robin packages
;;;

(robin-define-package "ethiopic-sera"
 "SERA transliteration system for Ethiopic."

 ("he" ?ሀ)
 ("hu" ?ሁ)
 ("hi" ?ሂ)
 ("ha" ?ሃ)
 ("hE" ?ሄ) ("hee" "ሄ")
 ("h" ?ህ)
 ("ho" ?ሆ)

 ("le" ?ለ) ("Le" "ለ")
 ("lu" ?ሉ) ("Lu" "ሉ")
 ("li" ?ሊ) ("Li" "ሊ")
 ("la" ?ላ) ("La" "ላ")
 ("lE" ?ሌ) ("LE" "ሌ") ("lee" "ሌ") ("Lee" "ሌ")
 ("l" ?ል) ("L" "ል")
 ("lo" ?ሎ) ("Lo" "ሎ")
 ("lWa" ?ሏ) ("LWa" "ሏ") ("lW" "ሏ") ("LW" "ሏ")

 ("He" ?ሐ)
 ("Hu" ?ሑ)
 ("Hi" ?ሒ)
 ("Ha" ?ሓ)
 ("HE" ?ሔ) ("Hee" "ሔ")
 ("H" ?ሕ)
 ("Ho" ?ሖ)
 ("HWa" ?ሗ) ("HW" "ሗ")

 ("me" ?መ) ("Me" "መ")
 ("mu" ?ሙ) ("Mu" "ሙ")
 ("mi" ?ሚ) ("Mi" "ሚ")
 ("ma" ?ማ) ("Ma" "ማ")
 ("mE" ?ሜ) ("ME" "ሜ") ("mee" "ሜ") ("Mee" "ሜ")
 ("m" ?ም) ("M" "ም")
 ("mo" ?ሞ) ("Mo" "ሞ")
 ("mWa" ?ሟ) ("MWa" "ሟ") ("mW" "ሟ") ("MW" "ሟ")

 ("`se" ?ሠ) ("sse" "ሠ") ("s2e" "ሠ")
 ("`su" ?ሡ) ("ssu" "ሡ") ("s2u" "ሡ")
 ("`si" ?ሢ) ("ssi" "ሢ") ("s2i" "ሢ")
 ("`sa" ?ሣ) ("ssa" "ሣ") ("s2a" "ሣ")
 ("`sE" ?ሤ) ("ssE" "ሤ") ("s2E" "ሤ")
   ("`see" "ሤ") ("ssee" "ሤ") ("s2ee" "ሤ")
 ("`s" ?ሥ) ("ss" "ሥ") ("s2" "ሥ")
 ("`so" ?ሦ) ("sso" "ሦ") ("s2o" "ሦ")
 ("`sWa" ?ሧ) ("ssWa" "ሧ") ("s2Wa" "ሧ")
   ("`sW" "ሧ") ("ssW" "ሧ") ("s2W" "ሧ")

 ("re" ?ረ) ("Re" "ረ")
 ("ru" ?ሩ) ("Ru" "ሩ")
 ("ri" ?ሪ) ("Ri" "ሪ")
 ("ra" ?ራ) ("Ra" "ራ")
 ("rE" ?ሬ) ("RE" "ሬ") ("ree" "ሬ") ("Ree" "ሬ")
 ("r" ?ር) ("R" "ር")
 ("ro" ?ሮ) ("Ro" "ሮ")
 ("rWa" ?ሯ) ("RWa" "ሯ") ("rW" "ሯ") ("RW" "ሯ")

 ("se" ?ሰ)
 ("su" ?ሱ)
 ("si" ?ሲ)
 ("sa" ?ሳ)
 ("sE" ?ሴ) ("see" "ሴ")
 ("s" ?ስ)
 ("so" ?ሶ)
 ("sWa" ?ሷ) ("sW" "ሷ")

 ("xe" ?ሸ)
 ("xu" ?ሹ)
 ("xi" ?ሺ)
 ("xa" ?ሻ)
 ("xE" ?ሼ) ("xee" "ሼ")
 ("x" ?ሽ)
 ("xo" ?ሾ)
 ("xWa" ?ሿ) ("xW" "ሿ")

 ("qe" ?ቀ)
 ("qu" ?ቁ)
 ("qi" ?ቂ)
 ("qa" ?ቃ)
 ("qE" ?ቄ) ("qee" "ቄ")
 ("q" ?ቅ)
 ("qo" ?ቆ)
 ("qWe" ?ቈ)
 ("qWi" ?ቊ)
 ("qWa" ?ቋ) ("qW" "ቋ")
 ("qWE" ?ቌ) ("qWee" "ቌ")
 ("qW'" ?ቍ) ("qWu" "ቍ")

 ("Qe" ?ቐ)
 ("Qu" ?ቑ)
 ("Qi" ?ቒ)
 ("Qa" ?ቓ)
 ("QE" ?ቔ) ("Qee" "ቔ")
 ("Q" ?ቕ)
 ("Qo" ?ቖ)
 ("QWe" ?ቘ)
 ("QWi" ?ቚ)
 ("QWa" ?ቛ) ("QW" "ቛ")
 ("QWE" ?ቜ) ("QWee" "ቜ")
 ("QW'" ?ቝ) ("QWu" "ቝ")

 ("be" ?በ) ("Be" "በ")
 ("bu" ?ቡ) ("Bu" "ቡ")
 ("bi" ?ቢ) ("Bi" "ቢ")
 ("ba" ?ባ) ("Ba" "ባ")
 ("bE" ?ቤ) ("BE" "ቤ") ("bee" "ቤ") ("Bee" "ቤ")
 ("b" ?ብ) ("B" "ብ")
 ("bo" ?ቦ) ("Bo" "ቦ")
 ("bWa" ?ቧ) ("BWa" "ቧ") ("bW" "ቧ") ("BW" "ቧ")

 ("ve" ?ቨ) ("Ve" "ቨ")
 ("vu" ?ቩ) ("Vu" "ቩ")
 ("vi" ?ቪ) ("Vi" "ቪ")
 ("va" ?ቫ) ("Va" "ቫ")
 ("vE" ?ቬ) ("VE" "ቬ") ("vee" "ቬ") ("Vee" "ቬ")
 ("v" ?ቭ) ("V" "ቭ")
 ("vo" ?ቮ) ("Vo" "ቮ")
 ("vWa" ?ቯ) ("VWa" "ቯ") ("vW" "ቯ") ("VW" "ቯ")

 ("te" ?ተ)
 ("tu" ?ቱ)
 ("ti" ?ቲ)
 ("ta" ?ታ)
 ("tE" ?ቴ) ("tee" "ቴ")
 ("t" ?ት)
 ("to" ?ቶ)
 ("tWa" ?ቷ) ("tW" "ቷ")

 ("ce" ?ቸ)
 ("cu" ?ቹ)
 ("ci" ?ቺ)
 ("ca" ?ቻ)
 ("cE" ?ቼ) ("cee" "ቼ")
 ("c" ?ች)
 ("co" ?ቾ)
 ("cWa" ?ቿ) ("cW" "ቿ")

 ("`he" ?ኀ) ("hhe" "ኀ") ("h2e" "ኀ")
 ("`hu" ?ኁ) ("hhu" "ኁ") ("h2u" "ኁ")
 ("`hi" ?ኂ) ("hhi" "ኂ") ("h2i" "ኂ")
 ("`ha" ?ኃ) ("hha" "ኃ") ("h2a" "ኃ")
 ("`hE" ?ኄ) ("hhE" "ኄ") ("h2E" "ኄ")
   ("`hee" "ኄ") ("hhee" "ኄ") ("h2ee" "ኄ")
 ("`h" ?ኅ) ("hh" "ኅ") ("h2" "ኅ")
 ("`ho" ?ኆ) ("hho" "ኆ") ("h2o" "ኆ")
 ("`hWe" ?ኈ) ("hhWe" "ኈ") ("h2We" "ኈ") ("hWe" "ኈ")
 ("`hWi" ?ኊ) ("hhWi" "ኊ") ("h2Wi" "ኊ") ("hWi" "ኊ")
 ("`hWa" ?ኋ) ("hhWa" "ኋ") ("h2Wa" "ኋ") ("hWa" "ኋ")
   ("`hW" "ኋ") ("hhW" "ኋ") ("h2W" "ኋ")
 ("`hWE" ?ኌ) ("hhWE" "ኌ") ("h2WE" "ኌ") ("hWE" "ኌ")
   ("`hWee" "ኌ") ("hhWee" "ኌ") ("h2Wee" "ኌ") ("hWee" "ኌ")
 ("`hW'" ?ኍ) ("hhW'" "ኍ") ("h2W'" "ኍ") ("hW'" "ኍ")
   ("`hWu" "ኍ") ("hhWu" "ኍ") ("h2Wu" "ኍ") ("hWu" "ኍ")

 ("ne" ?ነ)
 ("nu" ?ኑ)
 ("ni" ?ኒ)
 ("na" ?ና)
 ("nE" ?ኔ) ("nee" "ኔ")
 ("n" ?ን)
 ("no" ?ኖ)
 ("nWa" ?ኗ) ("nW" "ኗ")

 ("Ne" ?ኘ)
 ("Nu" ?ኙ)
 ("Ni" ?ኚ)
 ("Na" ?ኛ)
 ("NE" ?ኜ) ("Nee" "ኜ")
 ("N" ?ኝ)
 ("No" ?ኞ)
 ("NWa" ?ኟ) ("NW" "ኟ")

 ("'A" ?አ) ("A" "አ")
 ("'u" ?ኡ) ("u" "ኡ") ("'U" "ኡ") ("U" "ኡ")
 ("'i" ?ኢ) ("i" "ኢ")
 ("'a" ?ኣ) ("a" "ኣ")
 ("'E" ?ኤ) ("E" "ኤ")
 ("'I" ?እ) ("I" "እ") ("'e" "እ") ("e" "እ")
 ("'o" ?ኦ) ("o" "ኦ") ("'O" "ኦ") ("O" "ኦ")
 ("'ea" ?ኧ) ("ea" "ኧ")

 ("ke" ?ከ)
 ("ku" ?ኩ)
 ("ki" ?ኪ)
 ("ka" ?ካ)
 ("kE" ?ኬ) ("kee" "ኬ")
 ("k" ?ክ)
 ("ko" ?ኮ)
 ("kWe" ?ኰ)
 ("kWi" ?ኲ)
 ("kWa" ?ኳ) ("kW" "ኳ")
 ("kWE" ?ኴ) ("kWee" "ኴ")
 ("kW'" ?ኵ) ("kWu" "ኵ")

 ("Ke" ?ኸ)
 ("Ku" ?ኹ)
 ("Ki" ?ኺ)
 ("Ka" ?ኻ)
 ("KE" ?ኼ) ("Kee" "ኼ")
 ("K" ?ኽ)
 ("Ko" ?ኾ)
 ("KWe" ?ዀ)
 ("KWi" ?ዂ)
 ("KWa" ?ዃ) ("KW" "ዃ")
 ("KWE" ?ዄ) ("KWee" "ዄ")
 ("KW'" ?ዅ) ("KWu" "ዅ")

 ("we" ?ወ)
 ("wu" ?ዉ)
 ("wi" ?ዊ)
 ("wa" ?ዋ)
 ("wE" ?ዌ) ("wee" "ዌ")
 ("w" ?ው)
 ("wo" ?ዎ)

 ("`e" ?ዐ) ("ae" "ዐ") ("aaa" "ዐ") ("e2" "ዐ")
 ("`u" ?ዑ) ("uu" "ዑ") ("u2" "ዑ") ("`U" "ዑ") ("UU" "ዑ") ("U2" "ዑ")
 ("`i" ?ዒ) ("ii" "ዒ") ("i2" "ዒ")
 ("`a" ?ዓ) ("aa" "ዓ") ("a2" "ዓ") ("`A" "ዓ") ("AA" "ዓ") ("A2" "ዓ")
 ("`E" ?ዔ) ("EE" "ዔ") ("E2" "ዔ")
 ("`I" ?ዕ) ("II" "ዕ") ("I2" "ዕ") ("ee" "ዕ")
 ("`o" ?ዖ) ("oo" "ዖ") ("o2" "ዖ") ("`O" "ዖ") ("OO" "ዖ") ("O2" "ዖ")

 ("ze" ?ዘ)
 ("zu" ?ዙ)
 ("zi" ?ዚ)
 ("za" ?ዛ)
 ("zE" ?ዜ) ("zee" "ዜ")
 ("z" ?ዝ)
 ("zo" ?ዞ)
 ("zWa" ?ዟ) ("zW" "ዟ")

 ("Ze" ?ዠ)
 ("Zu" ?ዡ)
 ("Zi" ?ዢ)
 ("Za" ?ዣ)
 ("ZE" ?ዤ) ("Zee" "ዤ")
 ("Z" ?ዥ)
 ("Zo" ?ዦ)
 ("ZWa" ?ዧ) ("ZW" "ዧ")

 ("ye" ?የ) ("Ye" "የ")
 ("yu" ?ዩ) ("Yu" "ዩ")
 ("yi" ?ዪ) ("Yi" "ዪ")
 ("ya" ?ያ) ("Ya" "ያ")
 ("yE" ?ዬ) ("YE" "ዬ") ("yee" "ዬ") ("Yee" "ዬ")
 ("y" ?ይ) ("Y" "ይ")
 ("yo" ?ዮ) ("Yo" "ዮ")

 ("de" ?ደ)
 ("du" ?ዱ)
 ("di" ?ዲ)
 ("da" ?ዳ)
 ("dE" ?ዴ) ("dee" "ዴ")
 ("d" ?ድ)
 ("do" ?ዶ)
 ("dWa" ?ዷ) ("dW" "ዷ")

 ("De" ?ዸ)
 ("Du" ?ዹ)
 ("Di" ?ዺ)
 ("Da" ?ዻ)
 ("DE" ?ዼ) ("Dee" "ዼ")
 ("D" ?ዽ)
 ("Do" ?ዾ)
 ("DWa" ?ዿ) ("DW" "ዿ")

 ("je" ?ጀ) ("Je" "ጀ")
 ("ju" ?ጁ) ("Ju" "ጁ")
 ("ji" ?ጂ) ("Ji" "ጂ")
 ("ja" ?ጃ) ("Ja" "ጃ")
 ("jE" ?ጄ) ("JE" "ጄ") ("jee" "ጄ") ("Jee" "ጄ")
 ("j" ?ጅ) ("J" "ጅ")
 ("jo" ?ጆ) ("Jo" "ጆ")
 ("jWa" ?ጇ) ("jW" "ጇ") ("JWa" "ጇ") ("JW" "ጇ")

 ("ge" ?ገ)
 ("gu" ?ጉ)
 ("gi" ?ጊ)
 ("ga" ?ጋ)
 ("gE" ?ጌ) ("gee" "ጌ")
 ("g" ?ግ)
 ("go" ?ጎ)
 ("gWe" ?ጐ)
 ("gWi" ?ጒ)
 ("gWa" ?ጓ) ("gW" "ጓ")
 ("gWE" ?ጔ) ("gWee" "ጔ")
 ("gW'" ?ጕ) ("gWu" "ጕ")

 ("Ge" ?ጘ)
 ("Gu" ?ጙ)
 ("Gi" ?ጚ)
 ("Ga" ?ጛ)
 ("GE" ?ጜ) ("Gee" "ጜ")
 ("G" ?ጝ)
 ("Go" ?ጞ)

 ("Te" ?ጠ)
 ("Tu" ?ጡ)
 ("Ti" ?ጢ)
 ("Ta" ?ጣ)
 ("TE" ?ጤ) ("Tee" "ጤ")
 ("T" ?ጥ)
 ("To" ?ጦ)
 ("TWa" ?ጧ) ("TW" "ጧ")

 ("Ce" ?ጨ)
 ("Cu" ?ጩ)
 ("Ci" ?ጪ)
 ("Ca" ?ጫ)
 ("CE" ?ጬ) ("Cee" "ጬ")
 ("C" ?ጭ)
 ("Co" ?ጮ)
 ("CWa" ?ጯ) ("CW" "ጯ")

 ("Pe" ?ጰ)
 ("Pu" ?ጱ)
 ("Pi" ?ጲ)
 ("Pa" ?ጳ)
 ("PE" ?ጴ) ("Pee" "ጴ")
 ("P" ?ጵ)
 ("Po" ?ጶ)
 ("PWa" ?ጷ) ("PW" "ጷ")

 ("Se" ?ጸ)
 ("Su" ?ጹ)
 ("Si" ?ጺ)
 ("Sa" ?ጻ)
 ("SE" ?ጼ) ("See" "ጼ")
 ("S" ?ጽ)
 ("So" ?ጾ)
 ("SWa" ?ጿ) ("`SWa" "ጿ") ("SSWa" "ጿ") ("S2Wa" "ጿ")
   ("SW" "ጿ") ("`SW" "ጿ") ("SSW" "ጿ") ("S2W" "ጿ")

 ("`Se" ?ፀ) ("SSe" "ፀ") ("S2e" "ፀ")
 ("`Su" ?ፁ) ("SSu" "ፁ") ("S2u" "ፁ")
 ("`Si" ?ፂ) ("SSi" "ፂ") ("S2i" "ፂ")
 ("`Sa" ?ፃ) ("SSa" "ፃ") ("S2a" "ፃ")
 ("`SE" ?ፄ) ("SSE" "ፄ") ("S2E" "ፄ")
   ("`See" "ፄ") ("SSee" "ፄ") ("S2ee" "ፄ")
 ("`S" ?ፅ) ("SS" "ፅ") ("S2" "ፅ")
 ("`So" ?ፆ) ("SSo" "ፆ") ("S2o" "ፆ")

 ("fe" ?ፈ) ("Fe" "ፈ")
 ("fu" ?ፉ) ("Fu" "ፉ")
 ("fi" ?ፊ) ("Fi" "ፊ")
 ("fa" ?ፋ) ("Fa" "ፋ")
 ("fE" ?ፌ) ("FE" "ፌ") ("fee" "ፌ") ("Fee" "ፌ")
 ("f" ?ፍ) ("F" "ፍ")
 ("fo" ?ፎ) ("Fo" "ፎ")
 ("fWa" ?ፏ) ("FWa" "ፏ") ("fW" "ፏ") ("FW" "ፏ")

 ("pe" ?ፐ)
 ("pu" ?ፑ)
 ("pi" ?ፒ)
 ("pa" ?ፓ)
 ("pE" ?ፔ) ("pee" "ፔ")
 ("p" ?ፕ)
 ("po" ?ፖ)
 ("pWa" ?ፗ) ("pW" "ፗ")

 ("rYa" ?ፘ) ("RYa" "ፘ") ("rY" "ፘ") ("RY" "ፘ")
 ("mYa" ?ፙ) ("MYa" "ፙ") ("mY" "ፙ") ("MY" "ፙ")
 ("fYa" ?ፚ) ("FYa" "ፚ") ("fY" "ፚ") ("FY" "ፚ")

 (" : " ?፡) (":" "፡") ("`:" "፡")
 ("::" ?።) ("." "።")
 ("," ?፣)
 (";" ?፤)
 ("-:" ?፥)
 (":-" ?፦)
 ("`?" ?፧) ("??" "፧")
 (":|:" ?፨) ("**" "፨")

 ;; Explicit syllable delimiter
 ("'" "")

 ;; Quick ASCII input
 ("''" "'")
 (":::" ":")
 (".." ".")
 (",," ",")
 (";;" ";")

 ("`1" ?፩)
 ("`2" ?፪)
 ("`3" ?፫)
 ("`4" ?፬)
 ("`5" ?፭)
 ("`6" ?፮)
 ("`7" ?፯)
 ("`8" ?፰)
 ("`9" ?፱)
 ("`10" ?፲)
 ("`20" ?፳)
 ("`30" ?፴)
 ("`40" ?፵)
 ("`50" ?፶)
 ("`60" ?፷)
 ("`70" ?፸)
 ("`80" ?፹)
 ("`90" ?፺)
 ("`100" ?፻)
 ("`10000" ?፼)

 ("`200" "፪፻")
 ("`300" "፫፻")
 ("`400" "፬፻")
 ("`500" "፭፻")
 ("`600" "፮፻")
 ("`700" "፯፻")
 ("`800" "፰፻")
 ("`900" "፱፻")
 ("`1000" "፲፻")
 ("`2000" "፳፻")
 ("`3000" "፴፻")
 ("`4000" "፵፻")
 ("`5000" "፶፻")
 ("`6000" "፷፻")
 ("`7000" "፸፻")
 ("`8000" "፹፻")
 ("`9000" "፺፻")
 ("`20000" "፪፼")
 ("`30000" "፫፼")
 ("`40000" "፬፼")
 ("`50000" "፭፼")
 ("`60000" "፮፼")
 ("`70000" "፯፼")
 ("`80000" "፰፼")
 ("`90000" "፱፼")
 ("`100000" "፲፼")
 ("`200000" "፳፼")
 ("`300000" "፴፼")
 ("`400000" "፵፼")
 ("`500000" "፶፼")
 ("`600000" "፷፼")
 ("`700000" "፸፼")
 ("`800000" "፹፼")
 ("`900000" "፺፼")
 ("`1000000" "፻፼")
 )

(register-input-method
 "ethiopic-sera" "Ethiopic"
 'robin-use-package "et" "An input method for Ethiopic.")

(robin-define-package "ethiopic-tex"
 "TeX transliteration system for Ethiopic."

 ("\\heG" ?ሀ)				; U+1200 ..
 ("\\huG" ?ሁ)
 ("\\hiG" ?ሂ)
 ("\\haG" ?ሃ)
 ("\\hEG" ?ሄ)
 ("\\hG" ?ህ)
 ("\\hoG" ?ሆ)
 ;; reserved
 ("\\leG" ?ለ)
 ("\\luG" ?ሉ)
 ("\\liG" ?ሊ)
 ("\\laG" ?ላ)
 ("\\lEG" ?ሌ)
 ("\\lG" ?ል)
 ("\\loG" ?ሎ)
 ("\\lWaG" ?ሏ)

 ("\\HeG" ?ሐ)				; U+1210 ..
 ("\\HuG" ?ሑ)
 ("\\HiG" ?ሒ)
 ("\\HaG" ?ሓ)
 ("\\HEG" ?ሔ)
 ("\\HG" ?ሕ)
 ("\\HoG" ?ሖ)
 ("\\HWaG" ?ሗ)
 ("\\meG" ?መ)
 ("\\muG" ?ሙ)
 ("\\miG" ?ሚ)
 ("\\maG" ?ማ)
 ("\\mEG" ?ሜ)
 ("\\mG" ?ም)
 ("\\moG" ?ሞ)
 ("\\mWaG" ?ሟ)

 ("\\sseG" ?ሠ)				; U+1220 ..
 ("\\ssuG" ?ሡ)
 ("\\ssiG" ?ሢ)
 ("\\ssaG" ?ሣ)
 ("\\ssEG" ?ሤ)
 ("\\ssG" ?ሥ)
 ("\\ssoG" ?ሦ)
 ("\\ssWaG" ?ሧ)
 ("\\reG" ?ረ)
 ("\\ruG" ?ሩ)
 ("\\riG" ?ሪ)
 ("\\raG" ?ራ)
 ("\\rEG" ?ሬ)
 ("\\rG" ?ር)
 ("\\roG" ?ሮ)
 ("\\rWaG" ?ሯ)

 ("\\seG" ?ሰ)				; U+1230 ..
 ("\\suG" ?ሱ)
 ("\\siG" ?ሲ)
 ("\\saG" ?ሳ)
 ("\\sEG" ?ሴ)
 ("\\sG" ?ስ)
 ("\\soG" ?ሶ)
 ("\\sWaG" ?ሷ)
 ("\\xeG" ?ሸ)
 ("\\xuG" ?ሹ)
 ("\\xiG" ?ሺ)
 ("\\xaG" ?ሻ)
 ("\\xEG" ?ሼ)
 ("\\xG" ?ሽ)
 ("\\xoG" ?ሾ)
 ("\\xWaG" ?ሿ)

 ("\\qeG" ?ቀ)				; U+1240 ..
 ("\\quG" ?ቁ)
 ("\\qiG" ?ቂ)
 ("\\qaG" ?ቃ)
 ("\\qEG" ?ቄ)
 ("\\qG" ?ቅ)
 ("\\qoG" ?ቆ)
 ;; reserved
 ("\\qWeG" ?ቈ)
 ;; reserved
 ("\\qWiG" ?ቊ)
 ("\\qWaG" ?ቋ)
 ("\\qWEG" ?ቌ)
 ("\\qWG" ?ቍ)
 ;; reserved
 ;; reserved

 ("\\QeG" ?ቐ)				; U+1250 ..
 ("\\QuG" ?ቑ)
 ("\\QiG" ?ቒ)
 ("\\QaG" ?ቓ)
 ("\\QEG" ?ቔ)
 ("\\QG" ?ቕ)
 ("\\QoG" ?ቖ)
 ;; reserved
 ("\\QWeG" ?ቘ)
 ;; reserved
 ("\\QWiG" ?ቚ)
 ("\\QWaG" ?ቛ)
 ("\\QWEG" ?ቜ)
 ("\\QWG" ?ቝ)
 ;; reserved
 ;; reserved

 ("\\beG" ?በ)				; U+1260 ..
 ("\\buG" ?ቡ)
 ("\\biG" ?ቢ)
 ("\\baG" ?ባ)
 ("\\bEG" ?ቤ)
 ("\\bG" ?ብ)
 ("\\boG" ?ቦ)
 ("\\bWaG" ?ቧ)
 ("\\veG" ?ቨ)
 ("\\vuG" ?ቩ)
 ("\\viG" ?ቪ)
 ("\\vaG" ?ቫ)
 ("\\vEG" ?ቬ)
 ("\\vG" ?ቭ)
 ("\\voG" ?ቮ)
 ("\\vWaG" ?ቯ)

 ("\\teG" ?ተ)				; U+1270 ..
 ("\\tuG" ?ቱ)
 ("\\tiG" ?ቲ)
 ("\\taG" ?ታ)
 ("\\tEG" ?ቴ)
 ("\\tG" ?ት)
 ("\\toG" ?ቶ)
 ("\\tWaG" ?ቷ)
 ("\\ceG" ?ቸ)
 ("\\cuG" ?ቹ)
 ("\\ciG" ?ቺ)
 ("\\caG" ?ቻ)
 ("\\cEG" ?ቼ)
 ("\\cG" ?ች)
 ("\\coG" ?ቾ)
 ("\\cWaG" ?ቿ)

 ("\\hheG" ?ኀ)				; U+1280 ..
 ("\\hhuG" ?ኁ)
 ("\\hhiG" ?ኂ)
 ("\\hhaG" ?ኃ)
 ("\\hhEG" ?ኄ)
 ("\\hhG" ?ኅ)
 ("\\hhoG" ?ኆ)
 ;; reserved
 ("\\hWeG" ?ኈ)
 ;; reserved
 ("\\hWiG" ?ኊ)
 ("\\hWaG" ?ኋ)
 ("\\hWEG" ?ኌ)
 ("\\hWG" ?ኍ)
 ;; reserved
 ;; reserved

 ("\\neG" ?ነ)				; U+1290 ..
 ("\\nuG" ?ኑ)
 ("\\niG" ?ኒ)
 ("\\naG" ?ና)
 ("\\nEG" ?ኔ)
 ("\\nG" ?ን)
 ("\\noG" ?ኖ)
 ("\\nWaG" ?ኗ)
 ("\\NeG" ?ኘ)
 ("\\NuG" ?ኙ)
 ("\\NiG" ?ኚ)
 ("\\NaG" ?ኛ)
 ("\\NEG" ?ኜ)
 ("\\NG" ?ኝ)
 ("\\NoG" ?ኞ)
 ("\\NWaG" ?ኟ)

 ("\\eG" ?አ)				; U+12A0 ..
 ("\\uG" ?ኡ)
 ("\\iG" ?ኢ)
 ("\\AG" ?ኣ)
 ("\\EG" ?ኤ)
 ("\\IG" ?እ)
 ("\\oG" ?ኦ)
 ("\\eaG" ?ኧ)
 ("\\keG" ?ከ)
 ("\\kuG" ?ኩ)
 ("\\kiG" ?ኪ)
 ("\\kaG" ?ካ)
 ("\\kEG" ?ኬ)
 ("\\kG" ?ክ)
 ("\\koG" ?ኮ)
 ;; reserved

 ("\\kWeG" ?ኰ)				; U+12B0 ..
 ;; reserved
 ("\\kWiG" ?ኲ)
 ("\\kWaG" ?ኳ)
 ("\\kWEG" ?ኴ)
 ("\\kWG" ?ኵ)
 ;; reserved
 ;; reserved
 ("\\KeG" ?ኸ)
 ("\\KuG" ?ኹ)
 ("\\KiG" ?ኺ)
 ("\\KaG" ?ኻ)
 ("\\KEG" ?ኼ)
 ("\\KG" ?ኽ)
 ("\\KoG" ?ኾ)
 ;; reserved

 ("\\KWeG" ?ዀ)				; U+12C0 ..
 ;; reserved
 ("\\KWiG" ?ዂ)
 ("\\KWaG" ?ዃ)
 ("\\KWEG" ?ዄ)
 ("\\KWG" ?ዅ)
 ;; reserved
 ;; reserved
 ("\\weG" ?ወ)
 ("\\wuG" ?ዉ)
 ("\\wiG" ?ዊ)
 ("\\waG" ?ዋ)
 ("\\wEG" ?ዌ)
 ("\\wG" ?ው)
 ("\\woG" ?ዎ)
 ;; reserved

 ("\\eeG" ?ዐ)				; U+12D0 ..
 ("\\uuG" ?ዑ)
 ("\\iiG" ?ዒ)
 ("\\aaG" ?ዓ)
 ("\\EEG" ?ዔ)
 ("\\IIG" ?ዕ)
 ("\\ooG" ?ዖ)
 ;; reserved
 ("\\zeG" ?ዘ)
 ("\\zuG" ?ዙ)
 ("\\ziG" ?ዚ)
 ("\\zaG" ?ዛ)
 ("\\zEG" ?ዜ)
 ("\\zG" ?ዝ)
 ("\\zoG" ?ዞ)
 ("\\zWaG" ?ዟ)

 ("\\ZeG" ?ዠ)				; U+12E0 ..
 ("\\ZuG" ?ዡ)
 ("\\ZiG" ?ዢ)
 ("\\ZaG" ?ዣ)
 ("\\ZEG" ?ዤ)
 ("\\ZG" ?ዥ)
 ("\\ZoG" ?ዦ)
 ("\\ZWaG" ?ዧ)
 ("\\yeG" ?የ)
 ("\\yuG" ?ዩ)
 ("\\yiG" ?ዪ)
 ("\\yaG" ?ያ)
 ("\\yEG" ?ዬ)
 ("\\yG" ?ይ)
 ("\\yoG" ?ዮ)
 ;; reserved

 ("\\deG" ?ደ)				; U+12F0 ..
 ("\\duG" ?ዱ)
 ("\\diG" ?ዲ)
 ("\\daG" ?ዳ)
 ("\\dEG" ?ዴ)
 ("\\dG" ?ድ)
 ("\\doG" ?ዶ)
 ("\\dWaG" ?ዷ)
 ("\\DeG" ?ዸ)
 ("\\DuG" ?ዹ)
 ("\\DiG" ?ዺ)
 ("\\DaG" ?ዻ)
 ("\\DEG" ?ዼ)
 ("\\DG" ?ዽ)
 ("\\DoG" ?ዾ)
 ("\\DWaG" ?ዿ)

 ("\\jeG" ?ጀ)				; U+1300 ..
 ("\\juG" ?ጁ)
 ("\\jiG" ?ጂ)
 ("\\jaG" ?ጃ)
 ("\\jEG" ?ጄ)
 ("\\jG" ?ጅ)
 ("\\joG" ?ጆ)
 ("\\jWaG" ?ጇ)
 ("\\geG" ?ገ)
 ("\\guG" ?ጉ)
 ("\\giG" ?ጊ)
 ("\\gaG" ?ጋ)
 ("\\gEG" ?ጌ)
 ("\\gG" ?ግ)
 ("\\goG" ?ጎ)
 ;; reserved

 ("\\gWeG" ?ጐ)				; U+1310 ..
 ;; reserved
 ("\\gWiG" ?ጒ)
 ("\\gWaG" ?ጓ)
 ("\\gWEG" ?ጔ)
 ("\\gWG" ?ጕ)
 ;; reserved
 ;; reserved
 ("\\GeG" ?ጘ)
 ("\\GuG" ?ጙ)
 ("\\GiG" ?ጚ)
 ("\\GaG" ?ጛ)
 ("\\GEG" ?ጜ)
 ("\\GG" ?ጝ)
 ("\\GoG" ?ጞ)
 ;; reserved

 ("\\TeG" ?ጠ)				; U+1320 ..
 ("\\TuG" ?ጡ)
 ("\\TiG" ?ጢ)
 ("\\TaG" ?ጣ)
 ("\\TEG" ?ጤ)
 ("\\TG" ?ጥ)
 ("\\ToG" ?ጦ)
 ("\\TWaG" ?ጧ)
 ("\\CeG" ?ጨ)
 ("\\CuG" ?ጩ)
 ("\\CiG" ?ጪ)
 ("\\CaG" ?ጫ)
 ("\\CEG" ?ጬ)
 ("\\CG" ?ጭ)
 ("\\CoG" ?ጮ)
 ("\\CWaG" ?ጯ)

 ("\\PeG" ?ጰ)				; U+1330 ..
 ("\\PuG" ?ጱ)
 ("\\PiG" ?ጲ)
 ("\\PaG" ?ጳ)
 ("\\PEG" ?ጴ)
 ("\\PG" ?ጵ)
 ("\\PoG" ?ጶ)
 ("\\PWaG" ?ጷ)
 ("\\SeG" ?ጸ)
 ("\\SuG" ?ጹ)
 ("\\SiG" ?ጺ)
 ("\\SaG" ?ጻ)
 ("\\SEG" ?ጼ)
 ("\\SG" ?ጽ)
 ("\\SoG" ?ጾ)
 ("\\SWaG" ?ጿ)

 ("\\SSeG" ?ፀ)				; U+1340 ..
 ("\\SSuG" ?ፁ)
 ("\\SSiG" ?ፂ)
 ("\\SSaG" ?ፃ)
 ("\\SSEG" ?ፄ)
 ("\\SSG" ?ፅ)
 ("\\SSoG" ?ፆ)
 ;; reserved
 ("\\feG" ?ፈ)
 ("\\fuG" ?ፉ)
 ("\\fiG" ?ፊ)
 ("\\faG" ?ፋ)
 ("\\fEG" ?ፌ)
 ("\\fG" ?ፍ)
 ("\\foG" ?ፎ)
 ("\\fWaG" ?ፏ)

 ("\\peG" ?ፐ)				; U+1350 ..
 ("\\puG" ?ፑ)
 ("\\piG" ?ፒ)
 ("\\paG" ?ፓ)
 ("\\pEG" ?ፔ)
 ("\\pG" ?ፕ)
 ("\\poG" ?ፖ)
 ("\\pWaG" ?ፗ)
 ("\\mYaG" ?ፘ)
 ("\\rYaG" ?ፙ)
 ("\\fYaG" ?ፚ)
 ;; reserved
 ;; reserved
 ;; reserved
 ;; reserved
 ;; reserved

 ;; reserved				; U+1360 ..
 ("\\spaceG" ?፡)
 ("\\periodG" ?።)
 ("\\commaG" ?፣)
 ("\\semicolonG" ?፤)
 ("\\colonG" ?፥)
 ("\\precolonG" ?፦)
 ("\\oldqmarkG" ?፧)
 ("\\pbreakG" ?፨)
 ("\\andG" ?፩)
 ("\\huletG" ?፪)
 ("\\sostG" ?፫)
 ("\\aratG" ?፬)
 ("\\amstG" ?፭)
 ("\\sadstG" ?፮)
 ("\\sabatG" ?፯)

 ("\\smntG" ?፰)			; U+1370 ..
 ("\\zeteNG" ?፱)
 ("\\asrG" ?፲)
 ("\\heyaG" ?፳)
 ("\\selasaG" ?፴)
 ("\\arbaG" ?፵)
 ("\\hemsaG" ?፶)
 ("\\slsaG" ?፷)
 ("\\sebaG" ?፸)
 ("\\semanyaG" ?፹)
 ("\\zeTanaG" ?፺)
 ("\\metoG" ?፻)
 ("\\asrxiG" ?፼)
 ;; reserved
 ;; reserved
 ;; reserved

 ;;
 ;; private extension
 ;;

 ("\\yWaG" ?����)				; U+1A00EF (was U+12EF)

 ("\\GWaG" ?����)				; U+1A011F (was U+131F)

 ("\\qqeG" ?����)				; U+1A0180 .. (was U+1380 ..)
 ("\\qquG" ?����)
 ("\\qqiG" ?����)
 ("\\qqaG" ?����)
 ("\\qqEG" ?����)
 ("\\qqG" ?����)
 ("\\qqoG" ?����)
 ;; unused
 ("\\MWeG" ?����)
 ("\\bWeG" ?����)
 ("\\GWeG" ?����)
 ("\\fWeG" ?����)
 ("\\pWeG" ?����)
 ;; unused
 ;; unused
 ;; unused

 ("\\kkeG" ?����)				; U+1A0190 .. (was U+1390 ..)
 ("\\kkuG" ?����)
 ("\\kkiG" ?����)
 ("\\kkaG" ?����)
 ("\\kkEG" ?����)
 ("\\kkG" ?����)
 ("\\kkoG" ?����)
 ;; unused
 ("\\mWiG" ?����)
 ("\\bWiG" ?����)
 ("\\GWiG" ?����)
 ("\\fWiG" ?����)
 ("\\pWiG" ?����)
 ;; unused
 ;; unused
 ;; unused

 ("\\XeG" ?����)				; U+1A01A0 .. (was U+13A0 ..)
 ("\\XuG" ?����)
 ("\\XiG" ?����)
 ("\\XaG" ?����)
 ("\\XEG" ?����)
 ("\\XG" ?����)
 ("\\XoG" ?����)
 ;; unused
 ("\\mWEG" ?����)
 ("\\bWEG" ?����)
 ("\\GWEG" ?����)
 ("\\fWEG" ?����)
 ("\\pWEG" ?����)
 ;; unused
 ;; unused
 ;; unused

 ("\\ggeG" ?����)				; U+1A01B0 .. (was U+13B0 ..)
 ("\\gguG" ?����)
 ("\\ggiG" ?����)
 ("\\ggaG" ?����)
 ("\\ggEG" ?����)
 ("\\ggG" ?����)
 ("\\ggoG" ?����)
 ;; unused
 ("\\mWG" ?����)
 ("\\bWG" ?����)
 ("\\GWG" ?����)
 ("\\fWG" ?����)
 ("\\pWG" ?����)
 ;; unused
 ;; unused
 ;; unused

 ("\\ornamentG" ?����)			; U+1A01C0 .. (was U+FDF0 ..)
 ("\\flandG" ?����)
 ("\\iflandG" ?����)
 ("\\africaG" ?����)
 ("\\iafricaG" ?����)
 ("\\wWeG" ?����)
 ("\\wWiG" ?����)
 ("\\wWaG" ?����)
 ("\\wWEG" ?����)
 ("\\wWG" ?����)
 ;; Gemination (����) is handled in a special way.
 ("\\slaqG" ?����)

 ;; Assign reverse conversion to Fidel chars.
 ;; Then override forward conversion with ASCII chars.
 ;; ASCII chars should not have reverse conversions.
 ("\\dotG" ?����) ("\\dotG" ".")
 ("\\lquoteG" ?����) ("\\lquoteG" "«")
 ("\\rquoteG" ?����) ("\\rquoteG" "»")
 ("\\qmarkG" ?����) ("\\qmarkG" "?")

 ;;
 ;; New characters in Unicode 4.1.
 ;;
 ;; In forward conversion, these characters override the old private
 ;; extensions above.  The old private extensions still keep their
 ;; reverse conversion.
 ;;

 ("\\ornamentG" ?፠)
 ("\\yWaG" ?ዯ)
 ("\\GWaG" ?ጟ)
 ("\\MWeG" ?ᎀ)
 ("\\mWiG" ?ᎁ)
 ("\\mWEG" ?ᎂ)
 ("\\mWG" ?ᎃ)
 ("\\bWeG" ?ᎄ)
 ("\\bWiG" ?ᎅ)
 ("\\bWEG" ?ᎆ)
 ("\\bWG" ?ᎇ)
 ("\\fWeG" ?ᎈ)
 ("\\fWiG" ?ᎉ)
 ("\\fWEG" ?ᎊ)
 ("\\fWG" ?ᎋ)
 ("\\pWeG" ?ᎌ)
 ("\\pWiG" ?ᎍ)
 ("\\pWEG" ?ᎎ)
 ("\\pWG" ?ᎏ)
 ("\\GWeG" ?ⶓ)
 ("\\GWiG" ?ⶔ)
 ("\\GWEG" ?ⶕ)
 ("\\GWG" ?ⶖ)
 ("\\qqeG" ?ⷀ)
 ("\\qquG" ?ⷁ)
 ("\\qqiG" ?ⷂ)
 ("\\qqaG" ?ⷃ)
 ("\\qqEG" ?ⷄ)
 ("\\qqG" ?ⷅ)
 ("\\qqoG" ?ⷆ)
 ("\\kkeG" ?ⷈ)
 ("\\kkuG" ?ⷉ)
 ("\\kkiG" ?ⷊ)
 ("\\kkaG" ?ⷋ)
 ("\\kkEG" ?ⷌ)
 ("\\kkG" ?ⷍ)
 ("\\kkoG" ?ⷎ)
 ("\\XeG" ?ⷐ)
 ("\\XuG" ?ⷑ)
 ("\\XiG" ?ⷒ)
 ("\\XaG" ?ⷓ)
 ("\\XEG" ?ⷔ)
 ("\\XG" ?ⷕ)
 ("\\XoG" ?ⷖ)
 ("\\ggeG" ?ⷘ)
 ("\\gguG" ?ⷙ)
 ("\\ggiG" ?ⷚ)
 ("\\ggaG" ?ⷛ)
 ("\\ggEG" ?ⷜ)
 ("\\ggG" ?ⷝ)
 ("\\ggoG" ?ⷞ)
 )

;; The ethiopic-tex package is not used for keyboard input, therefore
;; not registered with the register-input-method function.

(provide 'ethio-util)

;;; ethio-util.el ends here

;;; ethio-util.el ends here
