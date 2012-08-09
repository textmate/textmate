;;; titdic-cnv.el --- convert cxterm dictionary (TIT format) to Quail package -*- coding:iso-2022-7bit; -*-

;; Copyright (C) 1997-1998, 2000-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: Quail, TIT, cxterm

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

;; Convert cxterm dictionary (of TIT format) to quail-package.
;;
;; Usage (within Emacs):
;;	M-x titdic-convert<CR>CXTERM-DICTIONARY-NAME<CR>
;; Usage (from shell):
;;	% emacs -batch -l titdic-cnv -f batch-titdic-convert\
;;		[-dir DIR] [DIR | FILE] ...
;;
;; When you run titdic-convert within Emacs, you have a chance to
;; modify arguments of `quail-define-package' before saving the
;; converted file.  For instance, you are likely to modify TITLE,
;; DOCSTRING, and KEY-BINDINGS.

;; Cxterm dictionary file (*.tit) is a line-oriented text (English,
;; Chinese, Japanese, and Korean) file.  The whole file contains of
;; two parts, the definition part (`header' here after) followed by
;; the dictionary part (`body' here after).  All lines begin with
;; leading '#' are ignored.
;;
;; Each line in the header part has two fields, KEY and VALUE.  These
;; fields are separated by one or more white characters.
;;
;; Each line in the body part has two fields, KEYSEQ and TRANSLATIONS.
;; These fields are separated by one or more white characters.
;;
;; See the manual page of `tit2cit' of cxterm distribution for more
;; detail.
;;
;; Near the end of this file, we also have a few other tools to convert
;; miscellaneous dictionaries.

;;; Code:

(require 'quail)

;; List of values of key "ENCODE:" and the corresponding Emacs
;; coding-system and language environment name.
(defvar tit-encode-list
  '(("GB" euc-china "Chinese-GB")
    ("BIG5" cn-big5 "Chinese-BIG5")
    ("JIS" euc-japan "Japanese")
    ("KS" euc-kr "Korean")))

;; Alist of input method names and the corresponding title and extra
;; docstring.  For each of input method generated from TIT dictionary,
;; a docstring is automatically generated from the comments in the
;; dictionary.  The extra docstring in this alist is to add more
;; information.
;; The command describe-input-method shows the automatically generated
;; docstring, then an extra docstring while replacing the form \<VAR>
;; by the value of variable VAR.  For instance, the form
;; \<quail-translation-docstring> is replaced by a description about
;; how to select a translation from a list of candidates.

(defvar quail-cxterm-package-ext-info
  '(("chinese-4corner" "$(0(?-F(B")
    ("chinese-array30" "$(0#R#O(B")
    ("chinese-ccdospy" "$AKuF4(B"
     "Pinyin base input method for Chinese charset GB2312 \(`chinese-gb2312').

Pinyin is the standard Roman transliteration method for Chinese.
For the detail of Pinyin system, see the documentation of the input
method `chinese-py'.

This input method works almost the same way as `chinese-py'.  The
difference is that you type a single key for these Pinyin spelling.
    Pinyin:  zh  en  eng ang ch  an  ao  ai  ong sh  ing  yu($A(9(B)
    keyseq:   a   f   g   h   i   j   k   l   s   u   y   v
For example:
    Chinese:  $A0!(B    $A9{(B    $AVP(B    $AND(B    $A9b(B    $ASq(B    $AH+(B
    Pinyin:   a    guo   zhong  wen  guang  yu   quan
    Keyseq:   a1   guo4   as1   wf4  guh1  yu..6 qvj6

\\<quail-translation-docstring>

For double-width GB2312 characters corresponding to ASCII, use the
input method `chinese-qj'.")

    ("chinese-ecdict" "$(05CKH(B"
"In this input method, you enter a Chinese (Big5) character or word
by typing the corresponding English word.  For example, if you type
\"computer\", \"$(0IZH+(B\" is input.

\\<quail-translation-docstring>")

    ("chinese-etzy" "$(06/0D(B"
"Zhuyin base input method for Chinese Big5 characters (`chinese-big5-1',
`chinese-big5-2').

Zhuyin is a kind of phonetic symbol.  One to three Zhuyin symbols
compose one Chinese character.

In this input method, you enter a Chinese character by first typing
keys corresponding to Zhuyin symbols (see the above table) followed by
SPC, 1, 2, 3, or 4 specifying a tone (SPC:$(0?v(N(B, 1:$(0M=Vy(B, 2:$(0Dm(N(B, 3: $(0&9Vy(B,
4:$(0(+Vy(B).

\\<quail-translation-docstring>")

    ("chinese-punct-b5" "$(0O:(BB"
     "Input method for Chinese punctuation and symbols of Big5
\(`chinese-big5-1' and `chinese-big5-2').")

    ("chinese-punct" "$A1j(BG"
     "Input method for Chinese punctuation and symbols of GB2312
\(`chinese-gb2312').")

    ("chinese-py-b5" "$(03<(BB"
     "Pinyin base input method for Chinese Big5 characters
\(`chinese-big5-1', `chinese-big5-2').

This input method works almost the same way as `chinese-py' (which
see).

This input method supports only Han characters.  The more convenient
method is `chinese-py-punct-b5', which is the combination of this
method and `chinese-punct-b5' and which supports both Han characters
and punctuation/symbols.

For double-width Big5 characters corresponding to ASCII, use the input
method `chinese-qj-b5'.

The input method `chinese-py' and `chinese-tonepy' are also Pinyin
based, but for the character set GB2312 (`chinese-gb2312').")

    ("chinese-qj-b5" "$(0)A(BB")

    ("chinese-qj" "$AH+(BG")

    ("chinese-sw" "$AJWN2(B"
"Radical base input method for Chinese charset GB2312 (`chinese-gb2312').

In this input method, you enter a Chinese character by typing two
keys.  The first key corresponds to the first ($AJW(B) radical, the second
key corresponds to the last ($AN2(B) radical.  The correspondence of keys
and radicals is as below:

 first radical:
 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
 $APD(B $AZ"(B $AJ,(B $AX<(B $A;p(B $A?Z(B $A^P(B $Ac_(B $AZ%(B $A\3(B $AXi(B $AD>(B $Alj(B $Ab;(B $ATB(B $Afy(B $AJ/(B $AMu(B $A0K(B $AX/(B $AHU(B $AeA(B $Aak(B $AVq(B $AR;(B $AHK(B
 last radical:
 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
 $ASV(B $AI=(B $AMA(B $A56(B $AZb(B $A?Z(B $ARB(B $Aqb(B $A4s(B $A6!(B $A[L(B $Ala(B $AJ.(B $A4u(B $AXg(B $ACE(B $A=q(B $AX-(B $AE.(B $ARR(B $A`m(B $AP!(B $A3'(B $A3f(B $A_.(B $A27(B

\\<quail-translation-docstring>")

    ("chinese-tonepy" "$A5wF4(B"
     "Pinyin base input method for Chinese charset GB2312 (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
For the details of Pinyin system, see the documentation of the input
method `chinese-py'.

This input method works almost the same way as `chinese-py'.  The
difference is that you must type 1..5 after each Pinyin spelling to
specify a tone (1:$ARuF=(B, 2:$AQtF=(B, 3:$AIOIy(B, 4$AOBIy(B, 5:$AGaIy(B).

\\<quail-translation-docstring>

For instance, to input $ADc(B, you type \"n i 3 3\", the first \"n i\" is
a Pinyin, the next \"3\" specifies tone, and the last \"3\" selects
the third character from the candidate list.

For double-width GB2312 characters corresponding to ASCII, use the
input method `chinese-qj'.")

    ("chinese-zozy" "$(0I\0D(B"
"Zhuyin base input method for Chinese Big5 characters (`chinese-big5-1',
`chinese-big5-2').

Zhuyin is a kind of a phonetic symbol.  One to three Zhuyin symbols
compose a Chinese character.

In this input method, you enter a Chinese character by first typing
keys corresponding to Zhuyin symbols (see the above table) followed by
SPC, 6, 3, 4, or 7 specifying a tone (SPC:$(0?v(N(B, 6:$(0Dm(N(B, 3:$(0&9Vy(B, 4:$(0(+Vy(B,
7:$(0M=Vy(B).

\\<quail-translation-docstring>")))

;; Return a value of the key in the current line.
(defsubst tit-read-key-value ()
  (if (looking-at "[^ \t\r\n]+")
      (car (read-from-string (concat "\"" (match-string 0) "\"")))))

;; Return an appropriate quail-package filename from FILENAME (TIT
;; dictionary filename).  For instance, ".../ZOZY.tit" -> "ZOZY.el".
(defun tit-make-quail-package-file-name (filename &optional dirname)
  (expand-file-name
   (concat (file-name-nondirectory (substring filename 0 -4)) ".el")
   dirname))

;; This value is nil if we are processing phrase dictionary.
(defvar tit-dictionary t)
(defvar tit-encode nil)
(defvar tit-default-encode "GB")

;; Generate elements of KEY-BINDINGS arg for `quail-define-package' so
;; that each characters in KEYS invokes FUNCTION-SYMBOL.
(defun tit-generate-key-bindings (keys function-symbol)
  (let ((len (length keys))
	(i 0)
	(first t)
	key)
    (while (< i len)
      (or first (princ "\n   "))
      (setq key (aref keys i))
      (if (if (< key ?\ )
	      (eq (lookup-key quail-translation-keymap
			      (char-to-string key))
		  'quail-execute-non-quail-command)
	    (<= key 127))
	  (progn
	    (princ (cons (cond ((< key ?\ ) (format "\"\\C-%c\"" (+ key ?@)))
			       ((< key 127) (format "\"%c\"" key))
			       (t "\"\\C-?\""))
			 function-symbol))
	    (setq first nil)))
      (setq i (1+ i)))))

;; Analyze header part of TIT dictionary and generate an appropriate
;; `quail-define-package' function call.
(defun tit-process-header (filename)
  (message "Processing header part...")
  (goto-char (point-min))

  ;; At first, generate header part of the Quail package while
  ;; collecting information from the original header.
  (let ((package (concat
		  "chinese-"
		  (substring (downcase (file-name-nondirectory filename))
			     0 -4)))
	;; TIT keywords and the corresponding default values.
	(tit-multichoice t)
	(tit-prompt "")
	(tit-comments nil)
	(tit-backspace "\010\177")
	(tit-deleteall "\015\025")
	(tit-moveright ".>")
	(tit-moveleft ",<")
	(tit-keyprompt nil))

    (princ ";; Quail package `")
    (princ package)
    (princ (format "' -*- coding:%s -*-\n" coding-system-for-write))
    (princ ";;   Generated by the command `titdic-convert'\n;;\tDate: ")
    (princ (current-time-string))
    (princ "\n;;\tOriginal TIT dictionary file: ")
    (princ (file-name-nondirectory filename))
    (princ "\n\n;;; Comment:\n\n")
    (princ ";; Byte-compile this file again after any modification.\n\n")
    (princ ";;; Start of the header of original TIT dictionary.\n\n")

    (while (not (eobp))
      (let ((ch (following-char))
	    (pos (point)))
	(cond ((= ch ?C)		; COMMENT
	       (cond ((looking-at "COMMENT")
		      (let ((pos (match-end 0)))
			(end-of-line)
			(setq tit-comments
			      (cons (buffer-substring-no-properties pos (point))
				    tit-comments))))))
	      ((= ch ?M)		; MULTICHOICE, MOVERIGHT, MOVELEFT
	       (cond ((looking-at "MULTICHOICE:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-multichoice (looking-at "YES")))
		     ((looking-at "MOVERIGHT:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-moveright (tit-read-key-value)))
		     ((looking-at "MOVELEFT:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-moveleft (tit-read-key-value)))))
	      ((= ch ?P)		; PROMPT
	       (cond ((looking-at "PROMPT:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-prompt (tit-read-key-value))
		      ;; Some TIT dictionaries that are encoded by
		      ;; euc-china contains invalid character at the tail.
		      (let* ((last (aref tit-prompt (1- (length tit-prompt))))
			     (split (split-char last)))
			(if (or (eq (nth 1 split) 32)
				(eq (nth 2 split) 32))
			    (setq tit-prompt (substring tit-prompt 0 -1)))))))
	      ((= ch ?B)		; BACKSPACE, BEGINDICTIONARY,
					; BEGINPHRASE
	       (cond ((looking-at "BACKSPACE:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-backspace (tit-read-key-value)))
		     ((looking-at "BEGINDICTIONARY")
		      (setq tit-dictionary t))
		     ((looking-at "BEGINPHRASE")
		      (setq tit-dictionary nil))))
	      ((= ch ?K)		; KEYPROMPT
	       (cond ((looking-at "KEYPROMPT(\\(.*\\)):[ \t]*")
		      (let ((key-char (match-string 1)))
			(goto-char (match-end 0))
			(if (string-match "\\\\[0-9]+" key-char)
			    (setq key-char
				  (car (read-from-string (format "\"%s\""
								 key-char)))))
			(setq tit-keyprompt
			      (cons (cons key-char (tit-read-key-value))
				    tit-keyprompt)))))))
	(end-of-line)
	(princ ";; ")
	(princ (buffer-substring-no-properties pos (point)))
	(princ "\n")
	(forward-line 1)))

    (princ "\n;;; End of the header of original TIT dictionary.\n\n")
    (princ ";;; Code:\n\n(require 'quail)\n\n")

    (princ "(quail-define-package ")
    ;; Args NAME, LANGUAGE, TITLE
    (let ((title (nth 1 (assoc package quail-cxterm-package-ext-info))))
      (princ "\"")
      (princ package)
      (princ "\" \"")
      (princ (nth 2 (assoc tit-encode tit-encode-list)))
      (princ "\" \"")
      (princ (or title
		 (if (string-match "[:$A!K$(0!(!J(B]+\\([^:$A!K$(0!(!K(B]+\\)" tit-prompt)
		     (substring tit-prompt (match-beginning 1) (match-end 1))
		   tit-prompt)))
      (princ "\"\n"))

    ;; Arg GUIDANCE
    (if tit-keyprompt
	(progn
	  (princ " '(")
	  (while tit-keyprompt
	    (princ "   ")
	    (princ (format "(%d . \"%s\")\n"
			   (string-to-char (car (car tit-keyprompt)))
			   (cdr (car tit-keyprompt))))
	    (setq tit-keyprompt (cdr tit-keyprompt)))
	  (princ ")"))
      (princ " t\n"))

    ;; Arg DOCSTRING
    (let ((doc (concat tit-prompt "\n"))
	  (comments (if tit-comments
			(mapconcat 'identity (nreverse tit-comments) "\n")))
	  (doc-ext (nth 2 (assoc package quail-cxterm-package-ext-info))))
      (if comments
	  (setq doc (concat doc "\n" comments "\n")))
      (if doc-ext
	  (setq doc (concat doc "\n" doc-ext "\n")))
      (prin1 doc)
      (terpri))

    ;; Arg KEY-BINDINGS
    (princ " '(")
    (tit-generate-key-bindings tit-backspace 'quail-delete-last-char)
    (princ "\n   ")
    (tit-generate-key-bindings tit-deleteall 'quail-abort-translation)
    (princ "\n   ")
    (tit-generate-key-bindings tit-moveright 'quail-next-translation)
    (princ "\n   ")
    (tit-generate-key-bindings tit-moveleft 'quail-prev-translation)
    (princ ")\n")

    ;; Args FORGET-TRANSLATION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT.
    ;; The remaining args are all nil.
    (princ " nil")
    (princ (if tit-multichoice " nil" " t"))
    (princ (if tit-keyprompt " t t)\n\n" " nil nil)\n\n"))))

(defsubst tit-flush-translations (key translations)
  (if (string-match "\\\\[0-9][0-9][0-9]" key)
      (let ((newkey (concat (substring key 0 (match-beginning 0))
			    (car (read-from-string
				  (concat "\"" (match-string 0 key) "\"")))))
	    (idx (match-end 0)))
	(while (string-match "\\\\[0-9][0-9][0-9]" key idx)
	  (setq newkey (concat
			newkey
			(substring key idx (match-beginning 0))
			(car (read-from-string
			      (concat "\"" (match-string 0 key) "\"")))))
	  (setq idx (match-end 0)))
	(setq key (concat newkey (substring key idx)))))
  (prin1 (list key (if tit-dictionary translations
		     (vconcat (nreverse translations)))))
  (princ "\n"))

;; Convert body part of TIT dictionary into `quail-define-rules'
;; function call.
(defun tit-process-body ()
  (message "Formatting translation rules...")
  (let* ((template (list nil nil))
	 (second (cdr template))
	 (prev-key "")
	 ch key translations pos)
    (princ "(quail-define-rules\n")
    (while (null (eobp))
      (setq ch (following-char))
      (if (or (= ch ?#) (= ch ?\n))
	  (forward-line 1)
	(setq pos (point))
	(skip-chars-forward "^ \t\n")
	(setq key (buffer-substring-no-properties pos (point)))
	(skip-chars-forward " \t")
	(setq ch (following-char))
	(if (or (= ch ?#) (= ch ?\n))
	    ;; This entry contains no translations.  Let's ignore it.
	    (forward-line 1)
	  (or (string= key prev-key)
	      (progn
		(if translations
		    (tit-flush-translations prev-key translations))
		(setq translations nil
		      prev-key key)))
	  (if tit-dictionary
	      (progn
		(setq pos (point))
		(skip-chars-forward "^ \t#\n")
		(setq translations
		      (if translations
			  (concat translations
				  (buffer-substring-no-properties pos (point)))
			(buffer-substring-no-properties pos (point)))))
	    (while (not (eolp))
	      (setq pos (point))
	      (skip-chars-forward "^ \t\n")
	      (setq translations (cons (buffer-substring-no-properties
					pos (point))
				       translations))
	      (skip-chars-forward " \t")
	      (setq ch (following-char))
	      (if (= ch ?#) (end-of-line))))
	  (forward-line 1))))

    (if translations
	(tit-flush-translations prev-key translations))
    (princ ")\n")))

;;;###autoload
(defun titdic-convert (filename &optional dirname)
  "Convert a TIT dictionary of FILENAME into a Quail package.
Optional argument DIRNAME if specified is the directory name under which
the generated Quail package is saved."
  (interactive "FTIT dictionary file: ")
  (let ((coding-system-for-write nil))
    (with-temp-file  (tit-make-quail-package-file-name filename dirname)
      (let ((standard-output (current-buffer)))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  ;; Here we must use `raw-text' instead of `no-conversion' to
	  ;; enable auto-decoding of eol format (CRLF->LF).
	  (let ((coding-system-for-read 'raw-text))
	    (insert-file-contents (expand-file-name filename)))

	  ;; Decode the buffer contents from the encoding specified by a
	  ;; value of the key "ENCODE:".
	  (if (not (search-forward "\nBEGIN" nil t))
	      (error "TIT dictionary doesn't have body part"))
	  (let ((limit (point))
		coding-system slot)
	    (goto-char (point-min))
	    (if (re-search-forward "^ENCODE:[ \t]*" limit t)
		(progn
		  (goto-char (match-end 0))
		  (setq tit-encode (tit-read-key-value)))
	      (setq tit-encode tit-default-encode))
	    (setq slot (assoc tit-encode tit-encode-list))
	    (if (not slot)
		(error "Invalid ENCODE: value in TIT dictionary"))
	    (setq coding-system (nth 1 slot))
	    (message "Decoding with coding system %s..." coding-system)
	    (goto-char (point-min))
	    (decode-coding-region (point-min) (point-max) coding-system)
	    ;; Explicitly set eol format to `unix'.
	    (setq coding-system-for-write
		  (coding-system-change-eol-conversion coding-system 'unix))
	    (remove-text-properties (point-min) (point-max) '(charset nil)))

	  (set-buffer-multibyte t)
	  ;; Set point the starting position of the body part.
	  (goto-char (point-min))
	  (if (not (search-forward "\nBEGIN" nil t))
	      (error "TIT dictionary can't be decoded correctly"))

	  ;; Process the header part.
	  (forward-line 1)
	  (narrow-to-region (point-min) (point))
	  (tit-process-header filename)
	  (widen)

	  ;; Process the body part
	  (tit-process-body))))))

;;;###autoload
(defun batch-titdic-convert (&optional force)
  "Run `titdic-convert' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
For example, invoke \"emacs -batch -f batch-titdic-convert XXX.tit\" to
 generate Quail package file \"xxx.el\" from TIT dictionary file \"XXX.tit\".
To get complete usage, invoke \"emacs -batch -f batch-titdic-convert -h\"."
  (defvar command-line-args-left)	; Avoid compiler warning.
  (if (not noninteractive)
      (error "`batch-titdic-convert' should be used only with -batch"))
  (if (string= (car command-line-args-left) "-h")
      (progn
	(message "To convert XXX.tit and YYY.tit into xxx.el and yyy.el:")
	(message "  %% emacs -batch -l titdic-cnv -f batch-titdic-convert XXX.tit YYY.tit")
	(message "To convert XXX.tit into DIR/xxx.el:")
	(message "  %% emacs -batch -l titdic-cnv -f batch-titdic-convert -dir DIR XXX.tit"))
    (let (targetdir filename files file)
      (if (string= (car command-line-args-left) "-dir")
	  (progn
	    (setq command-line-args-left (cdr command-line-args-left))
	    (setq targetdir (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left))))
      (while command-line-args-left
	(setq filename (expand-file-name (car command-line-args-left)))
	(if (file-directory-p filename)
	    (progn
	      (message "Converting all tit files in the directory %s" filename)
	      (setq files (directory-files filename t "\\.tit$")))
	  (setq files (list filename)))
	(while files
	  (setq file (expand-file-name (car files)))
	  (when (or force
		    (file-newer-than-file-p
		     file (tit-make-quail-package-file-name file targetdir)))
	    (message "Converting %s to quail-package..." file)
	    (titdic-convert file targetdir))
	  (setq files (cdr files)))
	(setq command-line-args-left (cdr command-line-args-left)))
      (message "Byte-compile the created files by:")
      (message "  %% emacs -batch -f batch-byte-compile XXX.el")))
  (kill-emacs 0))


;;; Converter of miscellaneous dictionaries other than TIT format.

;; Alist of input method names and the corresponding information.
;; Each element has this form:
;;   (INPUT-METHOD-NAME		;; Name of the input method.
;;    INPUT-METHOD-TITLE	;; Title string of the input method
;;    DICFILE			;; Name of the source dictionary file.
;;    CODING			;; Coding system of the dictionary file.
;;    QUAILFILE			;; Name of the Quail package file.
;;    CONVERTER			;; Function to generate the Quail package.
;;    COPYRIGHT-NOTICE		;; Copyright notice of the source dictionary.
;;    )

(defvar quail-misc-package-ext-info
  '(("chinese-b5-tsangchi" "$(06A(BB"
     "cangjie-table.b5" big5 "tsang-b5.el"
     tsang-b5-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-b5-quick" "$(0X|(BB"
     "cangjie-table.b5" big5 "quick-b5.el"
     quick-b5-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-cns-tsangchi" "$(GT?(BC"
     "cangjie-table.cns" iso-2022-cn-ext "tsang-cns.el"
     tsang-cns-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-cns-quick" "$(Gv|(BC"
     "cangjie-table.cns" iso-2022-cn-ext "quick-cns.el"
     quick-cns-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-py" "$AF4(BG"
     "pinyin.map" cn-gb-2312 "PY.el"
     py-converter
     "\
;; \"pinyin.map\" is included in a free package called CCE.  It is
;; available at:
;;	http://ftp.debian.org/debian/dists/potato/main
;;		/source/utils/cce_0.36.orig.tar.gz
;; This package contains the following copyright notice.
;;
;;
;;             Copyright (C) 1999, Rui He, herui@cs.duke.edu
;;
;;
;;                  CCE(Console Chinese Environment) 0.32
;;
;; CCE is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version.
;;
;; CCE is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; CCE; see the file COPYING.  If not, write to the Free Software Foundation,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.")

    ("chinese-ziranma" "$AWTH;(B"
     "ziranma.cin" cn-gb-2312 "ZIRANMA.el"
     ziranma-converter
     "\
;; \"ziranma.cin\" is included in a free package called CCE.  It is
;; available at:
;;	http://ftp.debian.org/debian/dists/potato/main
;;		/source/utils/cce_0.36.orig.tar.gz
;; This package contains the following copyright notice.
;;
;;
;;             Copyright (C) 1999, Rui He, herui@cs.duke.edu
;;
;;
;;                  CCE(Console Chinese Environment) 0.32
;;
;; CCE is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version.
;;
;; CCE is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; CCE; see the file COPYING.  If not, write to the Free Software Foundation,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.")

    ("chinese-ctlau" "$AAuTA(B"
     "CTLau.html" cn-gb-2312 "CTLau.el"
     ctlau-gb-converter
     "\
;; \"CTLau.html\" is available at:
;;
;;   http://umunhum.stanford.edu/~lee/chicomp/CTLau.html
;;
;; It contains the following copyright notice:
;;
;; # Copyright (C) 1988-2001  Fung Fung Lee (lee@umunhum.stanford.edu)
;; #
;; # This program is free software; you can redistribute it and/or
;; # modify it under the terms of the GNU General Public License
;; # as published by the Free Software Foundation; either version 2
;; # of the License, or any later version.
;; #
;; # This program is distributed in the hope that it will be useful,
;; # but WITHOUT ANY WARRANTY; without even the implied warranty of
;; # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; # GNU General Public License for more details.
;; #
;; # You should have received a copy of the GNU General Public License
;; # along with this program; if not, write to the Free Software Foundation,
;; # Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.")

    ("chinese-ctlaub" "$(0N,Gn(B"
     "CTLau-b5.html" big5 "CTLau-b5.el"
     ctlau-b5-converter
     "\
;; \"CTLau-b5.html\" is available at:
;;
;;   http://umunhum.stanford.edu/~lee/chicomp/CTLau-b5.html
;;
;; It contains the following copyright notice:
;;
;; # Copyright (C) 1988-2001  Fung Fung Lee (lee@umunhum.stanford.edu)
;; #
;; # This program is free software; you can redistribute it and/or
;; # modify it under the terms of the GNU General Public License
;; # as published by the Free Software Foundation; either version 2
;; # of the License, or any later version.
;; #
;; # This program is distributed in the hope that it will be useful,
;; # but WITHOUT ANY WARRANTY; without even the implied warranty of
;; # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; # GNU General Public License for more details.
;; #
;; # You should have received a copy of the GNU General Public License
;; # along with this program; if not, write to the Free Software Foundation,
;; # Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.")
    ))

;; Generate a code of a Quail package in the current buffer from Tsang
;; dictionary in the buffer DICBUF.  The input method name of the
;; Quail package is NAME, and the title string is TITLE.

;; TSANG-P is non-nil, generate $(06AQo(B input method.  Otherwise
;; generate $(0X|/y(B (simple version of $(06AQo(B).  If BIG5-P is non-nil, the
;; input method is for inputting Big5 characters.  Otherwise the input
;; method is for inputting CNS characters.

(defun tsang-quick-converter (dicbuf name title tsang-p big5-p)
  (let ((fulltitle (if tsang-p (if big5-p "$(06AQo(B" "$(GT?on(B")
		     (if big5-p "$(0X|/y(B" "$(Gv|Mx(B")))
	dic)
    (goto-char (point-max))
    (if big5-p
	(insert (format "\"$(0&d'GTT&,!J(B%s$(0!K(BBIG5

	$(0KHM$(B%s$(0TT&,WoOu(B

   [Q $(0'D(B] [W $(0(q(B] [E $(0'V(B] [R $(0&H(B] [T $(0'>(B] [Y $(0&4(B] [U $(0&U(B] [I $(0'B(B] [O $(0&*(B] [P $(0'A(B]

    [A $(0'K(B] [S $(0&T(B] [D $(0'N(B] [F $(0'W(B] [G $(0&I(B] [H $(0*M(B] [J $(0&3(B] [L $(0&d(B]

      [Z  ] [X $(0[E(B] [C $(01[(B] [V $(0&M(B] [B $(0'M(B] [N $(0&_(B] [M $(0&"(B]

\\\\<quail-translation-docstring>\"\n"
			fulltitle fulltitle))
      (insert (format "\"$(GDcEFrSD+!J(B%s$(G!K(BCNS

	$(GiGk#(B%s$(GrSD+uomu(B

   [Q $(GEC(B] [W $(GFp(B] [E $(GEU(B] [R $(GDG(B] [T $(GE=(B] [Y $(GD3(B] [U $(GDT(B] [I $(GEA(B] [O $(GD)(B] [P $(GE@(B]

    [A $(GEJ(B] [S $(GDS(B] [D $(GEM(B] [F $(GEV(B] [G $(GDH(B] [H $(GHL(B] [J $(GD2(B] [L $(GDc(B]

      [Z  ] [X $(GyE(B] [C $(GOZ(B] [V $(GDL(B] [B $(GEL(B] [N $(GD^(B] [M $(GD!(B]

\\\\<quail-translation-docstring>\"\n"
		      fulltitle fulltitle)))
    (insert "  '((\".\" . quail-next-translation-block)
   (\",\" . quail-prev-translation-block))
  nil nil)\n\n")
    (insert "(quail-define-rules\n")
    (with-current-buffer dicbuf
      ;; Handle double CR line ends, which result when checking out of
      ;; CVS on MS-Windows.
      (goto-char (point-min))
      (search-forward "A440")
      (beginning-of-line)
      (let ((table (make-hash-table :test 'equal))
	    val)
	(while (not (eobp))
	  (forward-char 5)
	  (let ((trans (char-to-string (following-char)))
		key slot)
	    (re-search-forward "\\([A-Z]+\\)\r*$" nil t)
	    (setq key (downcase
		       (if (or tsang-p
			       (<= (- (match-end 1) (match-beginning 1)) 1))
			   (match-string 1)
			 (string (char-after (match-beginning 1))
				 (char-after (1- (match-end 1)))))))
	    (setq val (gethash key table))
	    (if val (setq trans (concat val trans)))
	    (puthash key trans table)
	    (forward-line 1)))
	(maphash #'(lambda (key val) (setq dic (cons (cons key val) dic)))
		 table)))
    (setq dic (sort dic (function (lambda (x y) (string< (car x ) (car y))))))
    (dolist (elt dic)
      (insert (format "(%S\t%S)\n" (car elt) (cdr elt))))
    (let ((punctuation '((";" "$(0!'!2!"!#!.!/(B" "$(G!'!2!"!#!.!/(B")
			 (":" "$(0!(!+!3!%!$!&!0!1(B" "$(G!(!+!3!%!$!&!0!1(B")
			 ("'" "$(0!e!d(B" "$(G!e!d(B")
			 ("\"" "$(0!g!f!h!i!q(B" "$(G!g!f!h!i!q(B")
			 ("\\" "$(0"`"b#M(B" "$(G"`"b#M(B")
			 ("|" "$(0!6!8!:"^(B" "$(G!6!8!:"^(B")
			 ("/" "$(0"_"a#L(B" "$(G"_"a#L(B")
			 ("?" "$(0!)!4(B" "$(G!)!4(B")
			 ("<" "$(0!R"6"A!T"H(B" "$(G!R"6"A!T"H(B")
			 (">" "$(0!S"7"B!U(B" "$(G!S"7"B!U(B")
			 ("[" "$(0!F!J!b!H!L!V!Z!X!\(B" "$(G!F!J!b!H!L!V!Z!X!\(B")
			 ("]" "$(0!G!K!c!I!M!W![!Y!](B" "$(G!G!K!c!I!M!W![!Y!](B")
			 ("{" "$(0!B!`!D(B " "$(G!B!`!D(B ")
			 ("}" "$(0!C!a!E(B" "$(G!C!a!E(B")
			 ("`" "$(0!j!k(B" "$(G!j!k(B")
			 ("~" "$(0"D"+",!<!=(B" "$(G"D"+",!<!=(B")
			 ("!" "$(0!*!5(B" "$(G!*!5(B")
			 ("@" "$(0"i"n(B" "$(G"i"n(B")
			 ("#" "$(0!l"-(B" "$(G!l"-(B")
			 ("$" "$(0"c"l(B" "$(G"c"l(B")
			 ("%" "$(0"h"m(B" "$(G"h"m(B")
			 ("&" "$(0!m".(B" "$(G!m".(B")
			 ("*" "$(0!n"/!o!w!x(B" "$(G!n"/!o!w!x(B")
			 ("(" "$(0!>!^!@(B" "$(G!>!^!@(B")
			 (")" "$(0!?!_!A(B" "$(G!?!_!A(B")
			 ("-" "$(0!7!9"#"$"1"@(B" "$(G!7!9"#"$"1"@(B")
			 ("_" "$(0"%"&(B" "$(G"%"&(B")
			 ("=" "$(0"8"C(B" "$(G"8"C(B")
			 ("+" "$(0"0"?(B" "$(G"0"?(B"))))
    (dolist (elt punctuation)
      (insert (format "(%S %S)\n" (concat "z" (car elt))
		      (if big5-p (nth 1 elt) (nth 2 elt))))))
    (insert ")\n")))

(defun tsang-b5-converter (dicbuf name title)
  (tsang-quick-converter dicbuf name title t t))

(defun quick-b5-converter (dicbuf name title)
  (tsang-quick-converter dicbuf name title nil t))

(defun tsang-cns-converter (dicbuf name title)
  (tsang-quick-converter dicbuf name title t nil))

(defun quick-cns-converter (dicbuf name title)
  (tsang-quick-converter dicbuf name title nil nil))

;; Generate a code of a Quail package in the current buffer from
;; Pinyin dictionary in the buffer DICBUF.  The input method name of
;; the Quail package is NAME, and the title string is TITLE.

(defun py-converter (dicbuf name title)
  (goto-char (point-max))
  (insert (format "%S\n" "$A::WVJdHk!KF4Rt!K(B

	$AF4Rt7=08(B

 $AP!P4S"NDWVD84z1m!8F4Rt!97{:E#,(B \"u(yu) $ATrSC(B u: $A1mJ>!C(B

Pinyin base input method for Chinese charset GB2312 (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
Pinyin uses a sequence of Latin alphabetic characters for each Chinese
character.  The sequence is made by the combination of the initials
\(the beginning sounds) and finals (the ending sounds).

  initials: b p m f d t n l z c s zh ch sh r j q x g k h
  finals: a o e i er ai ei oa ou an en ang eng ong i ia iao ie iu ian in
          iang ing iong u ua uo uai ui uan un uan ueng yu yue yuan yun

  (Note: In the correct Pinyin writing, the sequence \"yu\" in the last
   four finals should be written by the character u-umlaut `$A(9(B'.)

With this input method, you enter a Chinese character by first
entering its pinyin spelling.

\\<quail-translation-docstring>

For instance, to input $ADc(B, you type \"n i C-n 3\".  The first \"n i\"
is a Pinyin, \"C-n\" selects the next group of candidates (each group
contains at most 10 characters), \"3\" select the third character in
that group.

This input method supports only Han characters.  The related input
method `chinese-py-punct' is the combination of this method and
`chinese-punct'; it supports both Han characters and punctuation
characters.

For double-width GB2312 characters corresponding to ASCII, use the
input method `chinese-qj'.

The correct Pinyin system specifies tones by diacritical marks, but
this input method doesn't use them, which results in easy (you don't
have to know the exact tones), but verbose (many characters are assigned
to the same key sequence) input.  You may also want to try the input
method `chinese-tonepy' with which you must specify tones by digits
\(1..5)."))
  (insert "  '((\"\C-?\" . quail-delete-last-char)
   (\".\" . quail-next-translation)
   (\">\" . quail-next-translation)
   (\",\" . quail-prev-translation)
   (\"<\" . quail-prev-translation))
  nil nil nil nil)\n\n")
  (insert "(quail-define-rules\n")
  (let ((pos (point)))
    (insert-buffer-substring-no-properties dicbuf)
    (goto-char pos)
    (re-search-forward "^[a-z]")
    (beginning-of-line)
    (delete-region pos (point))
    (while (not (eobp))
      (insert "(\"")
      (skip-chars-forward "a-z")
      (insert "\" \"")
      (delete-char 1)
      (end-of-line)
      (while (= (preceding-char) ?\r)
	(delete-char -1))
      (insert "\")")
      (forward-line 1)))
  (insert ")\n"))

;; Generate a code of a Quail package in the current buffer from
;; Ziranma dictionary in the buffer DICBUF.  The input method name of
;; the Quail package is NAME, and the title string is TITLE.

(defun ziranma-converter (dicbuf name title)
  (let (dic)
    (with-current-buffer dicbuf
      (goto-char (point-min))
      (search-forward "\n%keyname end")
      (forward-line 1)
      (let ((table (make-hash-table :test 'equal))
	    elt pos key trans val)
	(while (not (eobp))
	  (setq pos (point))
	  (skip-chars-forward "^ \t")
	  (setq key (buffer-substring-no-properties pos (point)))
	  (skip-chars-forward " \t")
	  (setq pos (point))
	  (skip-chars-forward "^\r\n")
	  (setq trans (vector (buffer-substring-no-properties pos (point))))
	  (setq val (gethash key table))
	  (if val (setq trans (vconcat val trans)))
	  (puthash key trans table)
	  (forward-line 1))
	(maphash #'(lambda (key trans)
		     (let ((len (length trans))
			   i)
		       (if (and (= len 1) (= (length (aref trans 0)) 1))
			   (setq trans (aref trans 0))
			 (setq i 0)
			 (while (and (< i len)
				     (= (length (aref trans i)) 1))
			   (setq i (1+ i)))
			 (if (= i len)
			     (setq trans (mapconcat 'identity trans "")))))
		     (setq dic (cons (cons key trans) dic)))
		 table)))
    (setq dic (sort dic (function (lambda (x y) (string< (car x) (car y))))))
    (goto-char (point-max))
    (insert (format "%S\n" "$A::WVJdHk!K!>WTH;!?!K(B

                            $A<|EL6TUU1m(B:
 $A)3)%)%)W)%)%)W)%)%)W)%)%)W)%)%)W)%)%)W)%)%)W)%)%)W)%)%)W)%)%)7(B
 $A)'#Q(B  $A)'#W(B  $A)'#E(B  $A)'#R(B  $A)'#T(B  $A)'#Y(B  $A)'#U(Bsh$A)'#I(Bch$A)'#O(B  $A)'#P(B  $A)'(B
 $A)'(B  iu$A)'(B  ua$A)'(B   e$A)'(B uan$A)'(B  ue$A)'(B uai$A)'(B   u$A)'(B   i$A)'(B   o$A)'(B  un$A)'(B
 $A)'(B    $A)'(B  ia$A)'(B    $A)'(B van$A)'(B  ve$A)'(B ing$A)'(B    $A)'(B    $A)'(B  uo$A)'(B  vn$A)'(B
 $A);)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)?(B
   $A)'#A(B  $A)'#S(B  $A)'#D(B  $A)'#F(B  $A)'#G(B  $A)'#H(B  $A)'#J(B  $A)'#K(B  $A)'#L(B  $A)'(B
   $A)'(B   a$A)'(Biong$A)'(Buang$A)'(B  en$A)'(B eng$A)'(B ang$A)'(B  an$A)'(B  ao$A)'(B  ai$A)'(B
   $A)'(B    $A)'(B ong$A)'(Biang$A)'(B    $A)'(B  ng$A)'(B    $A)'(B    $A)'(B    $A)'(B    $A)'(B
   $A);)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)_)W)%)%)7(B
     $A)'#Z(B  $A)'#X(B  $A)'#C(B  $A)'#V(Bzh$A)'#B(B  $A)'#N(B  $A)'#M(B  $A)'#,(B  $A)'#.(B  $A)'(B $A#/(B $A)'(B
     $A)'(B  ei$A)'(B  ie$A)'(B iao$A)'(B  ui$A)'(B  ou$A)'(B  in$A)'(B ian$A)'G0R3)':sR3)'7{:E)'(B
     $A)'(B    $A)'(B    $A)'(B    $A)'(B   v$A)'(B    $A)'(B    $A)'(B    $A)'(B    $A)'(B    $A)'(B    $A)'(B
     $A);)%)%)_)%)%)_)%)%)_)%)%)_)%)%)_)%)%)_)%)%)_)%)%)_)%)%)_)%)%)?(B


Pinyin base input method for Chinese GB2312 characters (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
For the details of Pinyin system, see the documentation of the input
method `chinese-py'.

Unlike the standard spelling of Pinyin, in this input method all
initials and finals are assigned to single keys (see the above table).
For instance, the initial \"ch\" is assigned to the key `i', the final
\"iu\" is assigned to the key `q', and tones 1, 2, 3, 4, and $AGaIy(B are
assigned to the keys `q', `w', `e', `r', `t' respectively.

\\<quail-translation-docstring>

To input one-letter words, you type 4 keys, the first two for the
Pinyin of the letter, next one for tone, and the last one is always a
quote (').  For instance, \"vsq'\" input $AVP(B.  Exceptions are these
letters.  You can input them just by typing a single key.

	Character: $A04(B $A2;(B $A4N(B $A5D(B $A6~(B $A7"(B $A8v(B $A:M(B $A3v(B $A<0(B $A?I(B $AAK(B $AC;(B
	Key:	   a  b  c  d  e  f  g  h  i  j  k  l  m
	Character: $ADc(B $AE7(B $AF,(B $AF_(B $AHK(B $AH}(B $AK{(B $AJG(B $AWE(B $ANR(B $AP!(B $AR;(B $ATZ(B
	Key:	   n  o  p  q  r  s  t  u  v  w  x  y  z

To input two-letter words, you have two ways.  One way is to type 4
keys, two for the first Pinyin, two for the second Pinyin.  For
instance, \"vsgo\" inputs $AVP9z(B.  Another way is to type 3 keys: 2
initials of two letters, and quote (').  For instance, \"vg'\" also
inputs $AVP9z(B.

To input three-letter words, you type 4 keys: initials of three
letters, and the last is quote (').  For instance, \"bjy'2\" inputs $A11(B
$A>)Q<(B (the last `2' is to select one of the candidates).

To input words of more than three letters, you type 4 keys, initials
of the first three letters and the last letter.  For instance,
\"bjdt\" inputs $A11>)5gJSL((B.

To input symbols and punctuation, type `/' followed by one of `a' to
`z', then select one of the candidates."))
    (insert "  '((\"\C-?\" . quail-delete-last-char)
   (\".\" . quail-next-translation)
   (\"[\" . quail-next-translation)
   (\",\" . quail-prev-translation)
   (\"]\" . quail-prev-translation))
  nil nil nil nil)\n\n")
    (insert "(quail-define-rules\n")
    (dolist (elt dic)
      (insert (format "(%S %S)\n" (car elt) (cdr elt))))
    (insert ")\n")))

;; Generate the code for a Quail package in the current buffer from a
;; CTLau or CTLau-b5 dictionary in the buffer DICBUF.  The input
;; method name of the Quail package is NAME, and the title string is
;; TITLE.  DESCRIPTION is the string shown by describe-input-method.

(defun ctlau-converter (dicbuf name title description)
  (goto-char (point-max))
  (insert (format "%S\n" description))
  (insert "  '((\"\C-?\" . quail-delete-last-char)
   (\".\" . quail-next-translation)
   (\">\" . quail-next-translation)
   (\",\" . quail-prev-translation)
   (\"<\" . quail-prev-translation))
  nil nil nil nil)\n\n")
  (insert "(quail-define-rules\n")
  (let (dicbuf-start dicbuf-end key-start key (pos (point)))
    ;; Find the dictionary, which starts below a horizontal rule and
    ;; ends at the second to last line in the HTML file.
    (with-current-buffer dicbuf
      (goto-char (point-min))
      (re-search-forward "^#<hr>")
      (forward-line 1)
      (setq dicbuf-start (point))
      (goto-char (point-max))
      (re-search-backward "^<hr>")
      (setq dicbuf-end (point)))
    (insert-buffer-substring-no-properties dicbuf dicbuf-start dicbuf-end)
    ;; CTLau-b5.html contains characters (0xa1 0xbc) which show up as
    ;; hollow boxes when the original characters in CTLau.html from
    ;; which the file is converted have no Big5 equivalent.  Go
    ;; through and delete them.
    (goto-char pos)
    (while (search-forward "$(0!{(B" nil t)
      (delete-char -1))
    ;; Uppercase keys in dictionary need to be downcased.  Backslashes
    ;; at the beginning of keys need to be turned into double
    ;; backslashes.
    (goto-char pos)
    (while (not (eobp))
      (insert "(\"")
      (if (char-equal (following-char) ?\\)
	  (insert "\\"))
      (setq key-start (point))
      (skip-chars-forward "\\\\A-Z")
      (downcase-region key-start (point))
      (insert "\" \"")
      (delete-char 1)
      (end-of-line)
      (while (= (preceding-char) ?\r)
	(delete-char -1))
      (insert "\")")
      (forward-line 1)))
  (insert ")\n"))

(defun ctlau-gb-converter (dicbuf name title)
  (ctlau-converter dicbuf name title
"$A::WVJdHk!KAuN}OiJ=TARt!K(B

 $AAuN}OiJ=TASoW"Rt7=08(B
 Sidney Lau's Cantonese transcription scheme as described in his book
 \"Elementary Cantonese\", The Government Printer, Hong Kong, 1972.
 This file was prepared by Fung Fung Lee ($A@n7c7e(B).
 Originally converted from CTCPS3.tit
 Last modified: June 2, 1993.

 Some infrequent GB characters are accessed by typing \\, followed by
 the Cantonese romanization of the respective radical ($A2?JW(B)."))

(defun ctlau-b5-converter (dicbuf name title)
  (ctlau-converter dicbuf name title
"$(0KH)tTT&,!(N,Tg>A*#Gn5x!((B

 $(0N,Tg>A*#GnM$0D5x'J7{(B
 Sidney Lau's Cantonese transcription scheme as described in his book
 \"Elementary Cantonese\", The Government Printer, Hong Kong, 1972.
 This file was prepared by Fung Fung Lee ($(0,XFS76(B).
 Originally converted from CTCPS3.tit
 Last modified: June 2, 1993.

 Some infrequent characters are accessed by typing \\, followed by
 the Cantonese romanization of the respective radical ($(0?f5}(B)."))

(declare-function dos-8+3-filename "dos-fns.el" (filename))

(defun miscdic-convert (filename &optional dirname)
  "Convert a dictionary file FILENAME into a Quail package.
Optional argument DIRNAME if specified is the directory name under which
the generated Quail package is saved."
  (interactive "FInput method dictionary file: ")
  (or (file-readable-p filename)
      (error "%s does not exist" filename))
  (let ((tail quail-misc-package-ext-info)
	coding-system-for-write
	slot
	name title dicfile coding quailfile converter copyright
	dicbuf)
    (while tail
      (setq slot (car tail)
	    dicfile (nth 2 slot)
	    quailfile (nth 4 slot))
      (when (and (or (string-match dicfile filename)
		     ;; MS-DOS filesystem truncates file names to 8+3
		     ;; limits, so "cangjie-table.cns" becomes
		     ;; "cangjie-.cns", and the above string-match
		     ;; fails.  Give DOS users a chance...
		     (and (fboundp 'msdos-long-file-names)
			  (not (msdos-long-file-names))
			  (string-match (dos-8+3-filename dicfile) filename)))
		 (if (file-newer-than-file-p
		      filename (expand-file-name quailfile dirname))
		     t
		   (message "%s is up to date" quailfile)
		   nil))
	(setq name (car slot)
	      title (nth 1 slot)
	      coding (nth 3 slot)
	      converter (nth 5 slot)
	      copyright (nth 6 slot))
	(message "Converting %s to %s..." dicfile quailfile)
	;; Explicitly set eol format to `unix'.
	(setq coding-system-for-write
	      (coding-system-change-eol-conversion coding 'unix))
	(with-temp-file (expand-file-name quailfile dirname)
	  (insert (format ";; Quail package `%s' -*- coding:%s -*-\n"
                          name coding))
	  (insert ";;   Generated by the command `miscdic-convert'\n")
	  (insert ";;   Date: " (current-time-string) "\n")
	  (insert ";;   Source dictionary file: " dicfile "\n")
	  (insert ";;   Copyright notice of the source file\n")
	  (insert ";;------------------------------------------------------\n")
	  (insert copyright "\n")
	  (insert ";;------------------------------------------------------\n")
	  (insert "\n")
	  (insert ";;; Code:\n\n")
	  (insert "(require 'quail)\n")
	  (insert "(quail-define-package \"" name "\" \""
		  (if (eq coding 'big5) "Chinese-BIG5"
		    (if (eq coding 'iso-2022-cn-ext) "Chinese-CNS"
		      "Chinese-GB"))
		  "\" \"" title "\" t\n")
	  (let* ((coding-system-for-read
		  (coding-system-change-eol-conversion coding 'unix))
		 (dicbuf (find-file-noselect filename)))
	    (funcall converter dicbuf name title)
	    (kill-buffer dicbuf)))
	(message "Converting %s to %s...done" dicfile quailfile))
      (setq tail (cdr tail)))))

(defun batch-miscdic-convert ()
  "Run `miscdic-convert' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
If there's an argument \"-dir\", the next argument specifies a directory
to store generated Quail packages."
  (defvar command-line-args-left)	; Avoid compiler warning.
  (if (not noninteractive)
      (error "`batch-miscdic-convert' should be used only with -batch"))
  (let ((dir default-directory)
	filename)
    (while command-line-args-left
      (if (string= (car command-line-args-left) "-dir")
	  (progn
	    (setq command-line-args-left (cdr command-line-args-left))
	    (setq dir (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left))))
      (setq filename (car command-line-args-left)
	    command-line-args-left (cdr command-line-args-left))
      (if (file-directory-p filename)
	  (dolist (file (directory-files filename t nil t))
	    (or (file-directory-p file)
		(miscdic-convert file dir)))
	(miscdic-convert filename dir))))
  (kill-emacs 0))

;;; titdic-cnv.el ends here
