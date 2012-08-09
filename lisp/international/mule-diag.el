;;; mule-diag.el --- show diagnosis of multilingual environment (Mule)

;; Copyright (C) 1997-1998, 2000-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, charset, coding system, fontset, diagnosis, i18n

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

;; Make sure the help-xref button type is defined.
(require 'help-mode)

;;; General utility function

(defun print-list (&rest args)
  "Print all arguments with single space separator in one line."
  (princ (mapconcat (lambda (arg) (prin1-to-string arg t)) args " "))
  (princ "\n"))

;;; CHARSET

(define-button-type 'sort-listed-character-sets
  'help-echo (purecopy "mouse-2, RET: sort on this column")
  'face 'bold
  'action #'(lambda (button)
	      (sort-listed-character-sets (button-get button 'sort-key))))

(define-button-type 'list-charset-chars
  :supertype 'help-xref
  'help-function #'list-charset-chars
  'help-echo "mouse-2, RET: show table of characters for this character set")

;;;###autoload
(defun list-character-sets (arg)
  "Display a list of all character sets.

The D column contains the dimension of this character set.  The CH
column contains the number of characters in a block of this character
set.  The FINAL-BYTE column contains an ISO-2022 <final-byte> to use
in the designation escape sequence for this character set in
ISO-2022-based coding systems.

With prefix ARG, the output format gets more cryptic,
but still shows the full information."
  (interactive "P")
  (help-setup-xref (list #'list-character-sets arg)
		   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer "*Character Set List*"
    (with-current-buffer standard-output
      (if arg
	  (list-character-sets-2)
	;; Insert header.
	(insert "Supplementary character sets are shown below.\n")
	(insert
	 (substitute-command-keys
	  (concat "Use "
		  (if (display-mouse-p) "\\[help-follow-mouse] or ")
		  "\\[help-follow]:\n")))
	(insert "  on a column title to sort by that title,")
	(indent-to 48)
	(insert "+----DIMENSION\n")
	(insert "  on a charset name to list characters.")
	(indent-to 48)
	(insert "| +--CHARS\n")
	(let ((columns '(("CHARSET-NAME" . name) "\t\t\t\t\t"
			 ("D CH  FINAL-BYTE" . iso-spec)))
	      pos)
	  (while columns
	    (if (stringp (car columns))
		(insert (car columns))
	      (insert-text-button (car (car columns))
				  :type 'sort-listed-character-sets
				  'sort-key (cdr (car columns)))
	      (goto-char (point-max)))
	    (setq columns (cdr columns)))
	  (insert "\n"))
	(insert "------------\t\t\t\t\t- --- ----------\n")

	;; Insert body sorted by charset IDs.
	(list-character-sets-1 'name)))))

(defun sort-listed-character-sets (sort-key)
  (if sort-key
      (save-excursion
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (search-forward "\n-")
	  (forward-line 1)
	  (delete-region (point) (point-max))
	  (list-character-sets-1 sort-key)))))

(defun list-character-sets-1 (sort-key)
  "Insert a list of character sets sorted by SORT-KEY.
SORT-KEY should be `name' or `iso-spec' (default `name')."
  (or sort-key
      (setq sort-key 'name))
  (let ((tail charset-list)
	charset-info-list supplementary-list charset sort-func)
    (dolist (charset charset-list)
      ;; Generate a list that contains all information to display.
      (let ((elt (list charset
		       (charset-dimension charset)
		       (charset-chars charset)
		       (charset-iso-final-char charset))))
	(if (plist-get (charset-plist charset) :supplementary-p)
	    (push elt supplementary-list)
	  (push elt charset-info-list))))

    ;; Determine a predicate for `sort' by SORT-KEY.
    (setq sort-func
	  (cond ((eq sort-key 'name)
		 (lambda (x y) (string< (car x) (car y))))

		((eq sort-key 'iso-spec)
		 ;; Sort by DIMENSION CHARS FINAL-CHAR
		 (function
		  (lambda (x y)
		    (or (< (nth 1 x) (nth 1 y))
			(and (= (nth 1 x) (nth 1 y))
			     (or (< (nth 2 x) (nth 2 y))
				 (and (= (nth 2 x) (nth 2 y))
				      (< (nth 3 x) (nth 3 y)))))))))
		(t
		 (error "Invalid charset sort key: %s" sort-key))))

    (setq charset-info-list (sort charset-info-list sort-func))
    (setq supplementary-list (sort supplementary-list sort-func))

    ;; Insert information of character sets.
    (dolist (elt (append charset-info-list (list t) supplementary-list))
      (if (eq elt t)
	  (progn
	    (insert "\n-------------- ")
	    (insert-text-button "Supplementary Character Sets"
				'type 'help-info
				'help-args '("(emacs)Charsets"))
	    (insert " --------------
Character sets for defining other charsets, or for backward compatibility
"))
	(insert-text-button (symbol-name (car elt)) ; NAME
			    :type 'list-charset-chars
			    'help-args (list (car elt)))
	(goto-char (point-max))
	(insert "\t")
	(indent-to 48)
	(insert (format "%d %3d "
			(nth 1 elt) (nth 2 elt)) ; DIMENSION and CHARS
		(if (< (nth 3 elt) 0)
		    "none"
		  (nth 3 elt))))	; FINAL-CHAR
      (insert "\n"))))


;; List all character sets in a form that a program can easily parse.

(defun list-character-sets-2 ()
  (insert "#########################
## LIST OF CHARSETS
## Each line corresponds to one charset.
## The following attributes are listed in this order
## separated by a colon `:' in one line.
##	CHARSET-SYMBOL-NAME,
##	DIMENSION (1-4)
##	CHARS (number of characters in first dimension of charset)
##	ISO-FINAL-CHAR (character code of ISO-2022's final character)
##		-1 means that no final character is assigned.
##	DESCRIPTION (describing string of the charset)
")
  (dolist (charset charset-list)
    (princ (format "%s:%d:%d:%d:%s\n"
		   charset
		   (charset-dimension charset)
		   (charset-chars charset)
;;;		   (char-width (make-char charset))
;;; 		   (charset-direction charset)
		   (charset-iso-final-char charset)
;;;		   (charset-iso-graphic-plane charset)
		   (charset-description charset)))))

(defvar non-iso-charset-alist nil
  "Obsolete.")
(make-obsolete-variable 'non-iso-charset-alist "no longer relevant." "23.1")

(defun decode-codepage-char (codepage code)
  "Decode a character that has code CODE in CODEPAGE.
Return a decoded character string.  Each CODEPAGE corresponds to a
coding system cpCODEPAGE."
  (decode-char (intern (format "cp%d" codepage)) code))
(make-obsolete 'decode-codepage-char 'decode-char "23.1")

;; A variable to hold charset input history.
(defvar charset-history nil)


;;;###autoload
(defun read-charset (prompt &optional default-value initial-input)
  "Read a character set from the minibuffer, prompting with string PROMPT.
It must be an Emacs character set listed in the variable `charset-list'.

Optional arguments are DEFAULT-VALUE and INITIAL-INPUT.
DEFAULT-VALUE, if non-nil, is the default value.
INITIAL-INPUT, if non-nil, is a string inserted in the minibuffer initially.
See the documentation of the function `completing-read' for the detailed
meanings of these arguments."
  (let* ((table (mapcar (lambda (x) (list (symbol-name x))) charset-list))
	 (charset (completing-read prompt table
				   nil t initial-input 'charset-history
				   default-value)))
    (if (> (length charset) 0)
	(intern charset))))

;; List characters of the range MIN and MAX of CHARSET.  If dimension
;; of CHARSET is two (i.e. 2-byte charset), ROW is the first byte
;; (block index) of the characters, and MIN and MAX are the second
;; bytes of the characters.  If the dimension is one, ROW should be 0.

(defun list-block-of-chars (charset row min max)
  (let (i ch)
    (insert-char ?- (+ 7 (* 4 16)))
    (insert "\n     ")
    (setq i 0)
    (while (< i 16)
      (insert (format "%4X" i))
      (setq i (1+ i)))
    (setq i (* (/ min 16) 16))
    (while (<= i max)
      (if (= (% i 16) 0)
	  (insert (format "\n%6Xx" (/ (+ (* row 256) i) 16))))
      (setq ch (if (< i min)
		   32
		 (or (decode-char charset (+ (* row 256) i))
		     32)))		; gap in mapping
      ;; Don't insert control codes, non-Unicode characters.
      (if (or (< ch 32) (= ch 127))
	  (setq ch (single-key-description ch))
	(if (and (>= ch 128) (< ch 160))
	    (setq ch (format "%02Xh" ch))
	  (if (> ch #x10FFFF)
	      (setq ch 32))))
      (insert "\t" ch)
      (setq i (1+ i))))
  (insert "\n"))

;;;###autoload
(defun list-charset-chars (charset)
  "Display a list of characters in character set CHARSET."
  (interactive (list (read-charset "Character set: ")))
  (or (charsetp charset)
      (error "Invalid character set: %s" charset))
  (with-output-to-temp-buffer "*Character List*"
    (with-current-buffer standard-output
      (if (coding-system-p charset)
	  ;; Useful to be able to do C-u C-x = to find file code, for
	  ;; instance:
	  (set-buffer-file-coding-system charset))
      (setq mode-line-format (copy-sequence mode-line-format))
      (let ((slot (memq 'mode-line-buffer-identification mode-line-format)))
	(if slot
	    (setcdr slot
		    (cons (format " (%s)" charset)
			  (cdr slot)))))
      (setq tab-width 4)
      (set-buffer-multibyte t)
      (let ((dim (charset-dimension charset))
	    (chars (charset-chars charset))
	    ;; 	(plane (charset-iso-graphic-plane charset))
	    (plane 1)
	    (range (plist-get (charset-plist charset) :code-space))
	    min max min2 max2)
	(if (> dim 2)
	    (error "Can only list 1- and 2-dimensional charsets"))
	(insert (format "Characters in the coded character set %s.\n" charset))
	(narrow-to-region (point) (point))
	(setq min (aref range 0)
	      max (aref range 1))
	(if (= dim 1)
	    (list-block-of-chars charset 0 min max)
	  (setq min2 (aref range 2)
		max2 (aref range 3))
	  (let ((i min2))
	    (while (<= i max2)
	      (list-block-of-chars charset i min max)
	      (setq i (1+ i)))))
	(put-text-property (point-min) (point-max) 'charset charset)
	(widen)))))


;;;###autoload
(defun describe-character-set (charset)
  "Display information about built-in character set CHARSET."
  (interactive (list (read-charset "Charset: ")))
  (or (charsetp charset)
      (error "Invalid charset: %S" charset))
  (help-setup-xref (list #'describe-character-set charset)
		   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer (help-buffer)
    (with-current-buffer standard-output
      (insert "Character set: " (symbol-name charset))
      (let ((name (get-charset-property charset :name)))
	(if (not (eq name charset))
	    (insert " (alias of " (symbol-name name) ?\))))
      (insert "\n\n" (charset-description charset) "\n\n")
      (insert "Number of contained characters: ")
      (dotimes (i (charset-dimension charset))
	(unless (= i 0)
	  (insert ?x))
	(insert (format "%d" (charset-chars charset (1+ i)))))
      (insert ?\n)
      (let ((char (charset-iso-final-char charset)))
	(when (> char 0)
	  (insert "Final char of ISO2022 designation sequence: ")
	  (insert (format "`%c'\n" char))))
      (let (aliases)
	(dolist (c charset-list)
	  (if (and (not (eq c charset))
		   (eq charset (get-charset-property c :name)))
	      (push c aliases)))
	(if aliases
	    (insert "Aliases: " (mapconcat #'symbol-name aliases ", ") ?\n)))

      (dolist (elt `((:ascii-compatible-p "ASCII compatible." nil)
		     (:map "Map file: " identity)
		     (:unify-map "Unification map file: " identity)
		     (:invalid-code
		      nil
		      ,(lambda (c)
			 (format "Invalid character: %c (code %d)" c c)))
		     (:emacs-mule-id "Id in emacs-mule coding system: "
				     number-to-string)
		     (:parents "Parents: "
			       (lambda (parents)
				 (mapconcat ,(lambda (elt)
					       (format "%s" elt))
					    parents
					    ", ")))
		     (:code-space "Code space: " ,(lambda (c)
						    (format "%s" c)))
		     (:code-offset "Code offset: " number-to-string)
		     (:iso-revision-number "ISO revision number: "
					   number-to-string)
		     (:supplementary-p
		      "Used only as a parent of some other charset." nil)))
	(let ((val (get-charset-property charset (car elt))))
	  (when val
	    (if (cadr elt) (insert (cadr elt)))
	    (if (nth 2 elt)
		(let ((print-length 10) (print-level 2))
		  (princ (funcall (nth 2 elt) val) (current-buffer))))
	    (insert ?\n)))))))

;;; CODING-SYSTEM

(defvar graphic-register)		; dynamic bondage

;; Print information about designation of each graphic register in
;; DESIGNATIONS in human readable format.  See the documentation of
;; `define-coding-system' for the meaning of DESIGNATIONS
;; (`:designation' property).
(defun print-designation (designations)
  (let (charset)
    (dotimes (graphic-register 4)
      (setq charset (aref designations graphic-register))
      (princ (format
	      "  G%d -- %s\n"
	      graphic-register
	      (cond ((null charset)
		     "never used")
		    ((eq charset t)
		     "no initial designation, and used by any charsets")
		    ((symbolp charset)
		     (format "%s:%s"
			     charset (charset-description charset)))
		    ((listp charset)
		     (if (charsetp (car charset))
			 (format "%s:%s, and also used by the following:"
				 (car charset)
				 (charset-description (car charset)))
		       "no initial designation, and used by the following:"))
		    (t
		     "invalid designation information"))))
      (when (listp charset)
	(setq charset (cdr charset))
	(while charset
	  (cond ((eq (car charset) t)
		 (princ "\tany other charsets\n"))
		((charsetp (car charset))
		 (princ (format "\t%s:%s\n"
				(car charset)
				(charset-description (car charset)))))
		(t
		 "invalid designation information"))
	  (setq charset (cdr charset)))))))

;;;###autoload
(defun describe-coding-system (coding-system)
  "Display information about CODING-SYSTEM."
  (interactive "zDescribe coding system (default current choices): ")
  (if (null coding-system)
      (describe-current-coding-system)
    (help-setup-xref (list #'describe-coding-system coding-system)
		     (called-interactively-p 'interactive))
    (with-output-to-temp-buffer (help-buffer)
      (print-coding-system-briefly coding-system 'doc-string)
      (let ((type (coding-system-type coding-system))
	    ;; Fixme: use this
	    (extra-spec (coding-system-plist coding-system)))
	(princ "Type: ")
	(princ type)
	(cond ((eq type 'undecided)
	       (princ " (do automatic conversion)"))
	      ((eq type 'utf-8)
	       (princ " (UTF-8: Emacs internal multibyte form)"))
	      ((eq type 'utf-16)
	       ;; (princ " (UTF-16)")
	       )
	      ((eq type 'shift-jis)
	       (princ " (Shift-JIS, MS-KANJI)"))
	      ((eq type 'iso-2022)
	       (princ " (variant of ISO-2022)\n")
	       (princ "Initial designations:\n")
	       (print-designation (coding-system-get coding-system
						     :designation))

	       (when (coding-system-get coding-system :flags)
		 (princ "Other specifications: \n  ")
		 (apply #'print-list
			(coding-system-get coding-system :flags))))
	      ((eq type 'charset)
	       (princ " (charset)"))
	      ((eq type 'ccl)
	       (princ " (do conversion by CCL program)"))
	      ((eq type 'raw-text)
	       (princ " (text with random binary characters)"))
	      ((eq type 'emacs-mule)
	       (princ " (Emacs 21 internal encoding)"))
	      ((eq type 'big5))
	      (t (princ ": invalid coding-system.")))
	(princ "\nEOL type: ")
	(let ((eol-type (coding-system-eol-type coding-system)))
	  (cond ((vectorp eol-type)
		 (princ "Automatic selection from:\n\t")
		 (princ eol-type)
		 (princ "\n"))
		((or (null eol-type) (eq eol-type 0)) (princ "LF\n"))
		((eq eol-type 1) (princ "CRLF\n"))
		((eq eol-type 2) (princ "CR\n"))
		(t (princ "invalid\n")))))
      (let ((postread (coding-system-get coding-system :post-read-conversion)))
	(when postread
	  (princ "After decoding text normally,")
	  (princ " perform post-conversion using the function: ")
	  (princ "\n  ")
	  (princ postread)
	  (princ "\n")))
      (let ((prewrite (coding-system-get coding-system :pre-write-conversion)))
	(when prewrite
	  (princ "Before encoding text normally,")
	  (princ " perform pre-conversion using the function: ")
	  (princ "\n  ")
	  (princ prewrite)
	  (princ "\n")))
      (with-current-buffer standard-output
	(let ((charsets (coding-system-charset-list coding-system)))
	  (when (and (not (eq (coding-system-base coding-system) 'raw-text))
		     charsets)
	    (cond
	     ((eq charsets 'iso-2022)
	      (insert "This coding system can encode all ISO 2022 charsets."))
	     ((eq charsets 'emacs-mule)
	      (insert "This coding system can encode all emacs-mule charsets\
."""))
	     (t
	      (insert "This coding system encodes the following charsets:\n ")
	      (while charsets
		(insert " " (symbol-name (car charsets)))
		(search-backward (symbol-name (car charsets)))
		(help-xref-button 0 'help-character-set (car charsets))
		(goto-char (point-max))
		(setq charsets (cdr charsets)))))))))))

;;;###autoload
(defun describe-current-coding-system-briefly ()
  "Display coding systems currently used in a brief format in echo area.

The format is \"F[..],K[..],T[..],P>[..],P<[..], default F[..],P<[..],P<[..]\",
where mnemonics of the following coding systems come in this order
in place of `..':
  `buffer-file-coding-system' (of the current buffer)
  eol-type of `buffer-file-coding-system' (of the current buffer)
  Value returned by `keyboard-coding-system'
  eol-type of `keyboard-coding-system'
  Value returned by `terminal-coding-system'.
  eol-type of `terminal-coding-system'
  `process-coding-system' for read (of the current buffer, if any)
  eol-type of `process-coding-system' for read (of the current buffer, if any)
  `process-coding-system' for write (of the current buffer, if any)
  eol-type of `process-coding-system' for write (of the current buffer, if any)
  default `buffer-file-coding-system'
  eol-type of default `buffer-file-coding-system'
  `default-process-coding-system' for read
  eol-type of `default-process-coding-system' for read
  `default-process-coding-system' for write
  eol-type of `default-process-coding-system'"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (process-coding-systems (if proc (process-coding-system proc))))
    (message
     "F[%c%s],K[%c%s],T[%c%s],P>[%c%s],P<[%c%s], default F[%c%s],P>[%c%s],P<[%c%s]"
     (coding-system-mnemonic buffer-file-coding-system)
     (coding-system-eol-type-mnemonic buffer-file-coding-system)
     (coding-system-mnemonic (keyboard-coding-system))
     (coding-system-eol-type-mnemonic (keyboard-coding-system))
     (coding-system-mnemonic (terminal-coding-system))
     (coding-system-eol-type-mnemonic (terminal-coding-system))
     (coding-system-mnemonic (car process-coding-systems))
     (coding-system-eol-type-mnemonic (car process-coding-systems))
     (coding-system-mnemonic (cdr process-coding-systems))
     (coding-system-eol-type-mnemonic (cdr process-coding-systems))
     (coding-system-mnemonic (default-value 'buffer-file-coding-system))
     (coding-system-eol-type-mnemonic
      (default-value 'buffer-file-coding-system))
     (coding-system-mnemonic (car default-process-coding-system))
     (coding-system-eol-type-mnemonic (car default-process-coding-system))
     (coding-system-mnemonic (cdr default-process-coding-system))
     (coding-system-eol-type-mnemonic (cdr default-process-coding-system))
     )))

(defun print-coding-system-briefly (coding-system &optional doc-string)
  "Print symbol name and mnemonic letter of CODING-SYSTEM with `princ'.
If DOC-STRING is non-nil, print also the docstring of CODING-SYSTEM.
If DOC-STRING is `tightly', don't print an empty line before the
docstring, and print only the first line of the docstring."
  (if (not coding-system)
      (princ "nil\n")
    (princ (format "%c -- %s"
		   (coding-system-mnemonic coding-system)
		   coding-system))
    (let ((aliases (coding-system-aliases coding-system)))
      (cond ((eq coding-system (car aliases))
	     (if (cdr aliases)
		 (princ (format " %S" (cons 'alias: (cdr aliases))))))
	    ((memq coding-system aliases)
	     (princ (format " (alias of %s)" (car aliases))))
	    (t
	     (let ((eol-type (coding-system-eol-type coding-system))
		   (base-eol-type (coding-system-eol-type (car aliases))))
	       (if (and (integerp eol-type)
			(vectorp base-eol-type)
			(not (eq coding-system (aref base-eol-type eol-type))))
		   (princ (format " (alias of %s)"
				  (aref base-eol-type eol-type))))))))
    (princ "\n")
    (or (eq doc-string 'tightly)
	(princ "\n"))
    (if doc-string
	(let ((doc (or (coding-system-doc-string coding-system) "")))
	  (when (eq doc-string 'tightly)
	    (if (string-match "\n" doc)
		(setq doc (substring doc 0 (match-beginning 0))))
	    (setq doc (concat "  " doc)))
	  (princ (format "%s\n" doc))))))

;;;###autoload
(defun describe-current-coding-system ()
  "Display coding systems currently used, in detail."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let* ((proc (get-buffer-process (current-buffer)))
	   (process-coding-systems (if proc (process-coding-system proc))))
      (princ "Coding system for saving this buffer:\n  ")
      (if (local-variable-p 'buffer-file-coding-system)
	  (print-coding-system-briefly buffer-file-coding-system)
	(princ "Not set locally, use the default.\n"))
      (princ "Default coding system (for new files):\n  ")
      (print-coding-system-briefly (default-value 'buffer-file-coding-system))
      (princ "Coding system for keyboard input:\n  ")
      (print-coding-system-briefly (keyboard-coding-system))
      (princ "Coding system for terminal output:\n  ")
      (print-coding-system-briefly (terminal-coding-system))
      (when (boundp 'selection-coding-system)
          (princ "Coding system for inter-client cut and paste:\n  ")
          (print-coding-system-briefly selection-coding-system))
      (when (get-buffer-process (current-buffer))
	(princ "Coding systems for process I/O:\n")
	(princ "  encoding input to the process: ")
	(print-coding-system-briefly (cdr process-coding-systems))
	(princ "  decoding output from the process: ")
	(print-coding-system-briefly (car process-coding-systems)))
      (princ "Defaults for subprocess I/O:\n")
      (princ "  decoding: ")
      (print-coding-system-briefly (car default-process-coding-system))
      (princ "  encoding: ")
      (print-coding-system-briefly (cdr default-process-coding-system)))

    (with-current-buffer standard-output

      (princ "
Priority order for recognizing coding systems when reading files:\n")
      (let ((i 1))
	(dolist (elt (coding-system-priority-list))
	  (princ (format "  %d. %s " i elt))
	  (let ((aliases (coding-system-aliases elt)))
	    (if (eq elt (car aliases))
		(if (cdr aliases)
		    (princ (cons 'alias: (cdr aliases))))
	      (princ (list 'alias 'of (car aliases))))
	    (terpri)
	    (setq i (1+ i)))))

      (princ "\n  Other coding systems cannot be distinguished automatically
  from these, and therefore cannot be recognized automatically
  with the present coding system priorities.\n\n")

      ;; Fixme: should this be replaced or junked?
      (if nil
      (let ((categories '(coding-category-iso-7 coding-category-iso-7-else))
	    coding-system codings)
	(while categories
	  (setq coding-system (symbol-value (car categories)))
	  (mapc
	   (lambda (x)
	     (if (and (not (eq x coding-system))
		       (let ((flags (coding-system-get :flags)))
			 (not (or (memq 'use-roman flags)
				  (memq 'use-oldjis flags)))))
		 (setq codings (cons x codings))))
	   (get (car categories) 'coding-systems))
	  (if codings
	      (let ((max-col (window-width))
		    pos)
		(princ (format "\
  The following are decoded correctly but recognized as %s:\n   "
			       coding-system))
		(while codings
		  (setq pos (point))
		  (insert (format " %s" (car codings)))
		  (when (> (current-column) max-col)
		    (goto-char pos)
		    (insert "\n   ")
		    (goto-char (point-max)))
		  (setq codings (cdr codings)))
		(insert "\n\n")))
	  (setq categories (cdr categories)))))

      (princ "Particular coding systems specified for certain file names:\n")
      (terpri)
      (princ "  OPERATION\tTARGET PATTERN\t\tCODING SYSTEM(s)\n")
      (princ "  ---------\t--------------\t\t----------------\n")
      (let ((func (lambda (operation alist)
		    (princ "  ")
		    (princ operation)
		    (if (not alist)
			(princ "\tnothing specified\n")
		      (while alist
			(indent-to 16)
			(prin1 (car (car alist)))
			(if (>= (current-column) 40)
			    (newline))
			(indent-to 40)
			(princ (cdr (car alist)))
			(princ "\n")
			(setq alist (cdr alist)))))))
	(funcall func "File I/O" file-coding-system-alist)
	(funcall func "Process I/O" process-coding-system-alist)
	(funcall func "Network I/O" network-coding-system-alist))
      (help-mode))))

(defun print-coding-system (coding-system)
  "Print detailed information on CODING-SYSTEM."
  (let ((type (coding-system-type coding-system))
	(eol-type (coding-system-eol-type coding-system))
	(flags (coding-system-get coding-system :flags))
	(aliases (coding-system-aliases coding-system)))
    (if (not (eq (car aliases) coding-system))
	(princ (format "%s (alias of %s)\n" coding-system (car aliases)))
      (princ coding-system)
      (dolist (alias (cdr aliases))
	(princ ",")
	(princ alias))
      (princ (format ":%s:%c:%d:"
		     type
		     (coding-system-mnemonic coding-system)
		     (if (integerp eol-type) eol-type 3)))
      (cond ((eq type 'iso2022)
	     (let ((idx 0)
		   charset)
	       (while (< idx 4)
		 (setq charset (aref flags idx))
		 (cond ((null charset)
			(princ -1))
		       ((eq charset t)
			(princ -2))
		       ((charsetp charset)
			(princ charset))
		       ((listp charset)
			(princ "(")
			(princ (car charset))
			(setq charset (cdr charset))
			(while charset
			  (princ ",")
			  (princ (car charset))
			  (setq charset (cdr charset)))
			(princ ")")))
		 (princ ",")
		 (setq idx (1+ idx)))
	       (while (< idx 12)
		 (princ (if (aref flags idx) 1 0))
		 (princ ",")
		 (setq idx (1+ idx)))
	       (princ (if (aref flags idx) 1 0))))
	    ((eq type 'ccl)
	     (let (i len)
	       (if (symbolp (car flags))
		   (princ (format " %s" (car flags)))
		 (setq i 0 len (length (car flags)))
		 (while (< i len)
		   (princ (format " %x" (aref (car flags) i)))
		   (setq i (1+ i))))
	       (princ ",")
	       (if (symbolp (cdr flags))
		   (princ (format "%s" (cdr flags)))
		 (setq i 0 len (length (cdr flags)))
		 (while (< i len)
		   (princ (format " %x" (aref (cdr flags) i)))
		   (setq i (1+ i))))))
	    (t (princ 0)))
      (princ ":")
      (princ (coding-system-doc-string coding-system))
      (princ "\n"))))

;;;###autoload
(defun list-coding-systems (&optional arg)
  "Display a list of all coding systems.
This shows the mnemonic letter, name, and description of each coding system.

With prefix ARG, the output format gets more cryptic,
but still contains full information about each coding system."
  (interactive "P")
  (with-output-to-temp-buffer "*Help*"
    (list-coding-systems-1 arg)))

(defun list-coding-systems-1 (arg)
  (if (null arg)
      (princ "\
###############################################
# List of coding systems in the following format:
# MNEMONIC-LETTER -- CODING-SYSTEM-NAME
#   DOC-STRING
")
    (princ "\
#########################
## LIST OF CODING SYSTEMS
## Each line corresponds to one coding system
## Format of a line is:
##   NAME[,ALIAS...]:TYPE:MNEMONIC:EOL:FLAGS:POST-READ-CONVERSION
##	:PRE-WRITE-CONVERSION:DOC-STRING,
## where
##  NAME = coding system name
##  ALIAS = alias of the coding system
##  TYPE = nil (no conversion), t (undecided or automatic detection),
##         0 (EMACS-MULE), 1 (SJIS), 2 (ISO2022), 3 (BIG5), or 4 (CCL)
##  EOL = 0 (LF), 1 (CRLF), 2 (CR), or 3 (Automatic detection)
##  FLAGS =
##    if TYPE = 2 then
##      comma (`,') separated data of the following:
##        G0, G1, G2, G3, SHORT-FORM, ASCII-EOL, ASCII-CNTL, SEVEN,
##        LOCKING-SHIFT, SINGLE-SHIFT, USE-ROMAN, USE-OLDJIS, NO-ISO6429
##    else if TYPE = 4 then
##      comma (`,') separated CCL programs for read and write
##    else
##      0
##  POST-READ-CONVERSION, PRE-WRITE-CONVERSION = function name to be called
##
"))
  (dolist (coding-system (sort-coding-systems (coding-system-list 'base-only)))
    (if (null arg)
	(print-coding-system-briefly coding-system 'tightly)
      (print-coding-system coding-system))))

;; Fixme: delete?
;;;###autoload
(defun list-coding-categories ()
  "Display a list of all coding categories."
  (with-output-to-temp-buffer "*Help*"
    (princ "\
############################
## LIST OF CODING CATEGORIES (ordered by priority)
## CATEGORY:CODING-SYSTEM
##
")
    (let ((l coding-category-list))
      (while l
	(princ (format "%s:%s\n" (car l) (symbol-value (car l))))
	(setq l (cdr l))))))

;;; FONT

(declare-function font-info "font.c" (name &optional frame))

(defun describe-font-internal (font-info &optional ignored)
  "Print information about a font in FONT-INFO.
The IGNORED argument is ignored."
  (print-list "name (opened by):" (aref font-info 0))
  (print-list "       full name:" (aref font-info 1))
  (print-list "            size:" (format "%2d" (aref font-info 2)))
  (print-list "          height:" (format "%2d" (aref font-info 3)))
  (print-list " baseline-offset:" (format "%2d" (aref font-info 4)))
  (print-list "relative-compose:" (format "%2d" (aref font-info 5))))

;;;###autoload
(defun describe-font (fontname)
  "Display information about a font whose name is FONTNAME.
The font must be already used by Emacs."
  (interactive "sFont name (default current choice for ASCII chars): ")
  (or (and window-system (fboundp 'fontset-list))
      (error "No fonts being used"))
  (let (font-info)
    (if (or (not fontname) (= (length fontname) 0))
	(setq fontname (face-attribute 'default :font)))
    (setq font-info (font-info fontname))
    (if (null font-info)
	(if (fontp fontname 'font-object)
	    ;; The font should be surely used.  So, there's some
	    ;; problem about getting information about it.  It is
	    ;; better to print the fontname to show which font has
	    ;; this problem.
	    (message "No information about \"%s\"" (font-xlfd-name fontname))
	  (message "No matching font found"))
      (with-output-to-temp-buffer "*Help*"
	(describe-font-internal font-info)))))

(defun print-fontset-element (val)
  ;; VAL has this format:
  ;;  ((REQUESTED-FONT-NAME OPENED-FONT-NAME ...) ...)
  ;; CHAR RANGE is already inserted.  Get character codes from
  ;; the current line.
  (beginning-of-line)
  (let ((from (following-char))
	(to (if (looking-at "[^.]*[.]* ")
		(char-after (match-end 0)))))
    (if (re-search-forward "[ \t]*$" nil t)
	(delete-region (match-beginning 0) (match-end 0)))

    ;; For non-ASCII characters, insert also CODE RANGE.
    (if (or (>= from 128) (and to (>= to 128)))
	(if to
	    (insert (format " (#x%02X .. #x%02X)" from to))
	  (insert (format " (#x%02X)" from))))

    ;; Insert a requested font name.
    (dolist (elt val)
      (if (not elt)
	  (insert "\n    -- inhibit fallback fonts --")
	(let ((requested (car elt)))
	  (if (stringp requested)
	      (insert "\n    " requested)
	    (let (family registry weight slant width adstyle)
	      (if (and (fboundp 'fontp) (fontp requested))
		  (setq family (font-get requested :family)
			registry (font-get requested :registry)
			weight (font-get requested :weight)
			slant (font-get requested :slant)
			width (font-get requested :width)
			adstyle (font-get requested :adstyle))
		(setq family (aref requested 0)
		      registry (aref requested 5)
		      weight (aref requested 1)
		      slant (aref requested 2)
		      width (aref requested 3)
		      adstyle (aref requested 4)))
	      (if (not family)
		  (setq family "*-*")
		(if (symbolp family)
		    (setq family (symbol-name family)))
		(or (string-match "-" family)
		    (setq family (concat "*-" family))))
	      (if (not registry)
		  (setq registry "*-*")
		(if (symbolp registry)
		    (setq registry (symbol-name registry)))
		(or (string-match "-" registry)
		    (= (aref registry (1- (length registry))) ?*)
		    (setq registry (concat registry "*"))))
	      (insert (format"\n    -%s-%s-%s-%s-%s-*-*-*-*-*-*-%s"
			     family (or weight "*") (or slant "*") (or width "*")
			     (or adstyle "*") registry)))))

	;; Insert opened font names (if any).
	(if (and (boundp 'print-opened) (symbol-value 'print-opened))
	    (dolist (opened (cdr elt))
	      (insert "\n\t[" opened "]")))))))

(declare-function query-fontset "fontset.c" (pattern &optional regexpp))
(declare-function fontset-info "fontset.c" (fontset &optional frame))

(defun print-fontset (fontset &optional print-opened)
  "Print information about FONTSET.
FONTSET nil means the fontset of the selected frame, t means the
default fontset.
If optional arg PRINT-OPENED is non-nil, also print names of all opened
fonts for FONTSET.  This function actually inserts the information in
the current buffer."
  (if (eq fontset t)
      (setq fontset (query-fontset "fontset-default"))
    (if (eq fontset nil)
	(setq fontset (face-attribute 'default :fontset))))
  (beginning-of-line)
  (narrow-to-region (point) (point))
  (insert "Fontset: " fontset "\n")
  (insert (propertize "CHAR RANGE" 'face 'underline)
	   " (" (propertize "CODE RANGE" 'face 'underline) ")\n")
  (insert "    " (propertize "FONT NAME" 'face 'underline)
	  " (" (propertize "REQUESTED" 'face 'underline)
	  " and [" (propertize "OPENED" 'face 'underline) "])")
  (let* ((info (fontset-info fontset))
	 (default-info (char-table-extra-slot info 0))
	 start1 end1 start2 end2)
    (describe-vector info 'print-fontset-element)
    (when (char-table-range info nil)
      ;; The default of FONTSET is described.
      (setq start1 (re-search-backward "^default"))
      (delete-region (point) (line-end-position))
      (insert "\n  ---<fallback to the default of the specified fontset>---")
      (put-text-property (line-beginning-position) (point) 'face 'highlight)
      (goto-char (point-max))
      (setq end1 (setq start2 (point))))
    (when default-info
      (insert "\n  ---<fallback to the default fontset>---")
      (put-text-property (line-beginning-position) (point) 'face 'highlight)
      (describe-vector default-info 'print-fontset-element)
      (when (char-table-range default-info nil)
	;; The default of the default fontset is described.
	(setq end2 (re-search-backward "^default"))
	(delete-region (point) (line-end-position))
	(insert "\n  ---<fallback to the default of the default fontset>---")
	(put-text-property (line-beginning-position) (point) 'face 'highlight)))
      (if (and start1 end2)
	  ;; Reorder the printed information to match with the font
	  ;; searching strategy; i.e. FONTSET, the default fontset,
	  ;; default of FONTSET, default of the default fontset.
	  (transpose-regions start1 end1 start2 end2))
      (goto-char (point-max)))
  (widen))

(defvar fontset-alias-alist)
(declare-function fontset-list "fontset.c" ())

;;;###autoload
(defun describe-fontset (fontset)
  "Display information about FONTSET.
This shows which font is used for which character(s)."
  (interactive
   (if (not (and window-system (fboundp 'fontset-list)))
       (error "No fontsets being used")
     (let ((fontset-list (nconc
			  (fontset-list)
			  (mapcar 'cdr fontset-alias-alist)))
	   (completion-ignore-case t))
       (list (completing-read
	      "Fontset (default used by the current frame): "
	      fontset-list nil t)))))
  (if (= (length fontset) 0)
      (setq fontset (face-attribute 'default :fontset))
    (setq fontset (query-fontset fontset)))
  (help-setup-xref (list #'describe-fontset fontset)
		   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer (help-buffer)
    (with-current-buffer standard-output
      (print-fontset fontset t))))

(declare-function fontset-plain-name "fontset" (fontset))

;;;###autoload
(defun list-fontsets (arg)
  "Display a list of all fontsets.
This shows the name, size, and style of each fontset.
With prefix arg, also list the fonts contained in each fontset;
see the function `describe-fontset' for the format of the list."
  (interactive "P")
  (if (not (and window-system (fboundp 'fontset-list)))
      (error "No fontsets being used")
    (help-setup-xref (list #'list-fontsets arg)
		     (called-interactively-p 'interactive))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	;; This code is duplicated near the end of mule-diag.
	(let ((fontsets
	       (sort (fontset-list)
		     (lambda (x y)
		       (string< (fontset-plain-name x)
				(fontset-plain-name y))))))
	  (while fontsets
	    (if arg
		(print-fontset (car fontsets) nil)
	      (insert "Fontset: " (car fontsets) "\n"))
	    (setq fontsets (cdr fontsets))))))))

;;;###autoload
(defun list-input-methods ()
  "Display information about all input methods."
  (interactive)
  (help-setup-xref '(list-input-methods)
		   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer (help-buffer)
    (list-input-methods-1)
    (with-current-buffer standard-output
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
		"^  \\([^ ]+\\) (`.*' in mode line)$" nil t)
	  (help-xref-button 1 'help-input-method (match-string 1)))))))

(defun list-input-methods-1 ()
  (if (not input-method-alist)
      (princ "
No input method is available, perhaps because you have not
installed LEIM (Libraries of Emacs Input Methods).")
    (princ "LANGUAGE\n  NAME (`TITLE' in mode line)\n")
    (princ "    SHORT-DESCRIPTION\n------------------------------\n")
    (setq input-method-alist
	  (sort input-method-alist
		(lambda (x y) (string< (nth 1 x) (nth 1 y)))))

    (let (language)
      (dolist (elt input-method-alist)
	(when (not (equal language (nth 1 elt)))
	  (setq language (nth 1 elt))
	  (princ language)
	  (terpri))
	(princ (format "  %s (`%s' in mode line)\n    %s\n"
		       (car elt)
		       (let ((title (nth 3 elt)))
			 (if (and (consp title) (stringp (car title)))
			     (car title)
			   title))
		       ;; If the doc is multi-line, indent all
		       ;; non-blank lines. (Bug#8066)
		       (replace-regexp-in-string "\n\\(.\\)" "\n    \\1"
						 (or (nth 4 elt) ""))))))))

;;; DIAGNOSIS

;; Insert a header of a section with SECTION-NUMBER and TITLE.
(defun insert-section (section-number title)
  (insert "########################################\n"
	  "# Section " (format "%d" section-number) ".  " title "\n"
	  "########################################\n\n"))

;;;###autoload
(defun mule-diag ()
  "Display diagnosis of the multilingual environment (Mule).

This shows various information related to the current multilingual
environment, including lists of input methods, coding systems,
character sets, and fontsets (if Emacs is running under a window
system which uses fontsets)."
  (interactive)
  (with-output-to-temp-buffer "*Mule-Diagnosis*"
    (with-current-buffer standard-output
      (insert "###############################################\n"
	      "### Current Status of Multilingual Features ###\n"
	      "###############################################\n\n"
	      "CONTENTS: Section 1.  General Information\n"
	      "          Section 2.  Display\n"
	      "          Section 3.  Input methods\n"
	      "          Section 4.  Coding systems\n"
	      "          Section 5.  Character sets\n")
      (if (and window-system (fboundp 'fontset-list))
	  (insert "          Section 6.  Fontsets\n"))
      (insert "\n")

      (insert-section 1 "General Information")
      (insert "Version of this emacs:\n  " (emacs-version) "\n\n")
      (insert "Configuration options:\n  " system-configuration-options "\n\n")
      (insert "Multibyte characters awareness:\n"
	      (format "  default: %S\n" (default-value
					  'enable-multibyte-characters))
	      (format "  current-buffer: %S\n\n" enable-multibyte-characters))
      (insert "Current language environment: " current-language-environment
	      "\n\n")

      (insert-section 2 "Display")
      (if window-system
	  (insert (format "Window-system: %s, version %s"
			  window-system window-system-version))
	(insert "Terminal: " (getenv "TERM")))
      (insert "\n\n")

      (if window-system
	  (let ((font (cdr (assq 'font (frame-parameters)))))
	    (insert "The font and fontset of the selected frame are:\n"
		    "     font: " font "\n"
		    "  fontset: " (face-attribute 'default :fontset) "\n"))
	(insert "Coding system of the terminal: "
		(symbol-name (terminal-coding-system))))
      (insert "\n\n")

      (insert-section 3 "Input methods")
      (list-input-methods-1)
      (insert "\n")
      (if default-input-method
	  (insert (format "Default input method: %s\n" default-input-method))
	(insert "No default input method is specified\n"))

      (insert-section 4 "Coding systems")
      (list-coding-systems-1 t)
      (insert "\n")

      (insert-section 5 "Character sets")
      (list-character-sets-2)
      (insert "\n")

      (when (and window-system (fboundp 'fontset-list))
	;; This code duplicates most of list-fontsets.
	(insert-section 6 "Fontsets")
	(insert "Fontset-Name\t\t\t\t\t\t  WDxHT Style\n")
	(insert "------------\t\t\t\t\t\t  ----- -----\n")
	(dolist (fontset (fontset-list))
	  (print-fontset fontset t)
	  (insert "\n")))
      (help-print-return-message))))

;;;###autoload
(defun font-show-log (&optional limit)
  "Show log of font listing and opening.
Prefix arg LIMIT says how many fonts to show for each listing.
The default is 20.  If LIMIT is negative, do not limit the listing."
  (interactive "P")
  (setq limit (if limit (prefix-numeric-value limit) 20))
  (if (eq font-log t)
      (message "Font logging is currently suppressed")
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (dolist (elt (reverse font-log))
	(insert (format "%s: %s\n" (car elt) (cadr elt)))
	(setq elt (nth 2 elt))
	(if (or (vectorp elt) (listp elt))
	    (let ((i 0))
	      (catch 'tag
		(mapc #'(lambda (x)
			  (setq i (1+ i))
			  (when (= i limit)
			    (insert "  ...\n")
			    (throw 'tag nil))
			  (insert (format "  %s\n" x)))
		      elt)))
	  (insert (format "  %s\n" elt)))))))


(provide 'mule-diag)

;;; mule-diag.el ends here
