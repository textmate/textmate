;;; ja-dic-cnv.el --- convert a Japanese dictionary (SKK-JISYO.L) to Emacs Lisp

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: i18n, mule, multilingual, Japanese

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

;; SKK is a Japanese input method running on Mule created by Masahiko
;; Sato <masahiko@sato.riec.tohoku.ac.jp>.  Here we provide utilities
;; to handle a dictionary distributed with SKK so that a different
;; input method (e.g. quail-japanese) can utilize the dictionary.

;; The format of SKK dictionary is quite simple.  Each line has the
;; form "KANASTRING /CONV1/CONV2/.../" which means KANASTRING (仮名文
;; 字列) can be converted to one of CONVi.  CONVi is a Kanji (漢字)
;; and Kana (仮名) mixed string.
;;
;; KANASTRING may have a trailing ASCII letter for Okurigana (送り仮名)
;; information.  For instance, the trailing letter `k' means that one
;; of the following Okurigana is allowed: かきくけこ.  So, in that
;; case, the string "KANASTRINGく" can be converted to one of "CONV1く",
;; CONV2く, ...

;;; Code:

;; Name of a file to generate from SKK dictionary.
(defvar ja-dic-filename "ja-dic.el")

(defun skkdic-convert-okuri-ari (skkbuf buf)
  (message "Processing OKURI-ARI entries ...")
  (goto-char (point-min))
  (with-current-buffer buf
    (insert ";; Setting okuri-ari entries.\n"
	    "(skkdic-set-okuri-ari\n"))
  (while (not (eobp))
    (if (/= (following-char) ?>)
	(let ((from (point))
	      (to (line-end-position)))
	  (with-current-buffer buf
	    (insert-buffer-substring skkbuf from to)
	    (beginning-of-line)
	    (insert "\"")
	    (search-forward " ")
	    (delete-char 1)		; delete the first '/'
	    (let ((p (point)))
	      (end-of-line)
	      (delete-char -1)		; delete the last '/'
	      (subst-char-in-region p (point) ?/ ? 'noundo))
	    (insert "\"\n"))))

    (forward-line 1))
  (with-current-buffer buf
    (insert ")\n\n")))

(defconst skkdic-postfix-list '(skkdic-postfix-list))

(defconst skkdic-postfix-data
  '(("いき" "行")
    ("がかり" "係")
    ("がく" "学")
    ("がわ" "川")
    ("しゃ" "社")
    ("しゅう" "集")
    ("しょう" "賞" "城")
    ("じょう" "城")
    ("せん" "線")
    ("だけ" "岳")
    ("ちゃく" "着")
    ("てん" "店")
    ("とうげ" "峠")
    ("どおり" "通り")
    ("やま" "山")
    ("ばし" "橋")
    ("はつ" "発")
    ("もく" "目")
    ("ゆき" "行")))

(defun skkdic-convert-postfix (skkbuf buf)
  (message "Processing POSTFIX entries ...")
  (goto-char (point-min))
  (with-current-buffer buf
    (insert ";; Setting postfix entries.\n"
	    "(skkdic-set-postfix\n"))

  ;; Initialize SKKDIC-POSTFIX-LIST by predefined data
  ;; SKKDIC-POSTFIX-DATA.
  (with-current-buffer buf
    (let ((l skkdic-postfix-data)
	  kana candidates entry)
      (while l
	(setq kana (car (car l)) candidates (cdr (car l)))
	(insert "\"" kana)
	(while candidates
	  (insert " " (car candidates))
	  (setq entry (lookup-nested-alist (car candidates)
					   skkdic-postfix-list nil nil t))
	  (if (consp (car entry))
	      (setcar entry (cons kana (car entry)))
	    (set-nested-alist (car candidates) (list kana)
			      skkdic-postfix-list))
	  (setq candidates (cdr candidates)))
	(insert "\"\n")
	(setq l (cdr l)))))

  ;; Search postfix entries.
  (while (re-search-forward "^[#<>?]\\(\\(\\cH\\|ー\\)+\\) " nil t)
    (let ((kana (match-string 1))
	  str candidates)
      (while (looking-at "/[#0-9 ]*\\([^/\n]*\\)/")
	(setq str (match-string 1))
	(if (not (member str candidates))
	    (setq candidates (cons str candidates)))
	(goto-char (match-end 1)))
      (with-current-buffer buf
	(insert "\"" kana)
	(while candidates
	  (insert " " (car candidates))
	  (let ((entry (lookup-nested-alist (car candidates)
					    skkdic-postfix-list nil nil t)))
	    (if (consp (car entry))
		(if (not (member kana (car entry)))
		    (setcar entry (cons kana (car entry))))
	      (set-nested-alist (car candidates) (list kana)
				skkdic-postfix-list)))
	  (setq candidates (cdr candidates)))
	(insert "\"\n"))))
  (with-current-buffer buf
    (insert ")\n\n")))

(defconst skkdic-prefix-list '(skkdic-prefix-list))

(defun skkdic-convert-prefix (skkbuf buf)
  (message "Processing PREFIX entries ...")
  (goto-char (point-min))
  (with-current-buffer buf
    (insert ";; Setting prefix entries.\n"
	    "(skkdic-set-prefix\n"))
  (save-excursion
    (while (re-search-forward "^\\(\\(\\cH\\|ー\\)+\\)[<>?] " nil t)
      (let ((kana (match-string 1))
	    str candidates)
	(while (looking-at "/\\([^/\n]+\\)/")
	  (setq str (match-string 1))
	  (if (not (member str candidates))
	      (setq candidates (cons str candidates)))
	  (goto-char (match-end 1)))
	(with-current-buffer buf
	  (insert "\"" kana)
	  (while candidates
	    (insert " " (car candidates))
	    (set-nested-alist (car candidates) kana skkdic-prefix-list)
	    (setq candidates (cdr candidates)))
	  (insert "\"\n")))))
  (with-current-buffer buf
    (insert ")\n\n")))

;; FROM and TO point the head and tail of "/J../J../.../".
(defun skkdic-get-candidate-list (from to)
  (let (candidates)
    (goto-char from)
    (while (re-search-forward "/[^/ \n]+" to t)
      (setq candidates (cons (buffer-substring (1+ (match-beginning 0))
					       (match-end 0))
			     candidates)))
    candidates))

;; Return entry for STR from nested alist ALIST.
(defsubst skkdic-get-entry (str alist)
  (car (lookup-nested-alist str alist nil nil t)))


(defconst skkdic-word-list '(skkdic-word-list))

;; Return t if substring of STR (between FROM and TO) can be broken up
;; to chunks all of which can be derived from another entry in SKK
;; dictionary.  SKKBUF is the buffer where the original SKK dictionary
;; is visited, KANA is the current entry for STR.  FIRST is t only if
;; this is called at top level.

(defun skkdic-breakup-string (skkbuf kana str from to &optional first)
  (let ((len (- to from)))
    (or (and (>= len 2)
	     (let ((min-idx (+ from 2))
		   (idx (if first (1- to ) to))
		   (found nil))
	       (while (and (not found) (>= idx min-idx))
		 (let ((kana2-list (skkdic-get-entry
				    (substring str from idx)
				    skkdic-word-list)))
		   (if (or (and (consp kana2-list)
				(let ((kana-len (length kana))
				      kana2)
				  (catch 'skkdic-tag
				    (while kana2-list
				      (setq kana2 (car kana2-list))
				      (if (string-match kana2 kana)
					  (throw 'skkdic-tag t))
				      (setq kana2-list (cdr kana2-list)))))
				(or (= idx to)
				    (skkdic-breakup-string skkbuf kana str
							   idx to)))
			   (and (stringp kana2-list)
				(string-match kana2-list kana)))
		       (setq found t)
		     (setq idx (1- idx)))))
	       found))
	(and first
	     (> len 2)
	     (let ((kana2 (skkdic-get-entry
			   (substring str from (1+ from))
			   skkdic-prefix-list)))
	       (and (stringp kana2)
		    (eq (string-match kana2 kana) 0)))
	     (skkdic-breakup-string skkbuf kana str (1+ from) to))
	(and (not first)
	     (>= len 1)
	     (let ((kana2-list (skkdic-get-entry
				(substring str from to)
				skkdic-postfix-list)))
	       (and (consp kana2-list)
		    (let (kana2)
		      (catch 'skkdic-tag
			(while kana2-list
			  (setq kana2 (car kana2-list))
			  (if (string= kana2
				       (substring kana (- (length kana2))))
			      (throw 'skkdic-tag t))
			  (setq kana2-list (cdr kana2-list)))))))))))

;; Return list of candidates which excludes some from CANDIDATES.
;; Excluded candidates can be derived from another entry.

(defun skkdic-reduced-candidates (skkbuf kana candidates)
  (let (elt l)
    (while candidates
      (setq elt (car candidates))
      (if (or (= (length elt) 1)
	      (and (string-match "^\\cj" elt)
		   (not (skkdic-breakup-string skkbuf kana elt 0 (length elt)
					       'first))))
	  (setq l (cons elt l)))
      (setq candidates (cdr candidates)))
    (nreverse l)))

(defvar skkdic-okuri-nasi-entries (list nil))
(defvar skkdic-okuri-nasi-entries-count 0)

(defun skkdic-collect-okuri-nasi ()
  (message "Collecting OKURI-NASI entries ...")
  (save-excursion
    (let ((prev-ratio 0)
	  ratio)
      (while (re-search-forward "^\\(\\(\\cH\\|ー\\)+\\) \\(/\\cj.*\\)/$"
				nil t)
	(let ((kana (match-string 1))
	      (candidates (skkdic-get-candidate-list (match-beginning 3)
						     (match-end 3))))
	  (setq skkdic-okuri-nasi-entries
		(cons (cons kana candidates) skkdic-okuri-nasi-entries)
		skkdic-okuri-nasi-entries-count
		(1+ skkdic-okuri-nasi-entries-count))
	  (setq ratio (floor (/ (* (point) 100.0) (point-max))))
	  (if (/= ratio prev-ratio)
	      (progn
		(message "collected %2d%% %s ..." ratio kana)
		(setq prev-ratio ratio)))
	  (while candidates
	    (let ((entry (lookup-nested-alist (car candidates)
					      skkdic-word-list nil nil t)))
	      (if (consp (car entry))
		  (setcar entry (cons kana (car entry)))
		(set-nested-alist (car candidates) (list kana)
				  skkdic-word-list)))
	    (setq candidates (cdr candidates))))))))

(defun skkdic-convert-okuri-nasi (skkbuf buf)
  (message "Processing OKURI-NASI entries ...")
  (with-current-buffer buf
    (insert ";; Setting okuri-nasi entries.\n"
	    "(skkdic-set-okuri-nasi\n")
    (let ((l (nreverse skkdic-okuri-nasi-entries))
	  (count 0)
	  (prev-ratio 0)
	  ratio)
      (while l
	(let ((kana (car (car l)))
	      (candidates (cdr (car l))))
	  (setq ratio (/ (* count 1000) skkdic-okuri-nasi-entries-count)
		count (1+ count))
	  (if (/= prev-ratio (/ ratio 10))
	      (progn
		(message "processed %2d%% %s ..." (/ ratio 10) kana)
		(setq prev-ratio (/ ratio 10))))
	  (if (setq candidates
		    (skkdic-reduced-candidates skkbuf kana candidates))
	      (progn
		(insert "\"" kana)
		(while candidates
		  (insert " " (car candidates))
		  (setq candidates (cdr candidates)))
		(insert "\"\n"))))
	(setq l (cdr l))))
    (insert ")\n\n")))

(defun skkdic-convert (filename &optional dirname)
  "Generate Emacs Lisp file form Japanese dictionary file FILENAME.
The format of the dictionary file should be the same as SKK dictionaries.
Optional argument DIRNAME if specified is the directory name under which
the generated Emacs Lisp is saved.
The name of generated file is specified by the variable `ja-dic-filename'."
  (interactive "FSKK dictionary file: ")
  (message "Reading file \"%s\" ..." filename)
  (let* ((coding-system-for-read 'euc-japan)
	 (skkbuf(find-file-noselect (expand-file-name filename)))
	 (buf (get-buffer-create "*skkdic-work*")))
    ;; Setup and generate the header part of working buffer.
    (with-current-buffer buf
      (erase-buffer)
      (buffer-disable-undo)
      (insert ";;; ja-dic.el --- dictionary for Japanese input method"
	      " -*-coding: euc-japan; -*-\n"
	      ";;\tGenerated by the command `skkdic-convert'\n"
	      ";;\tDate: " (current-time-string) "\n"
	      ";;\tOriginal SKK dictionary file: "
	      (file-relative-name (expand-file-name filename) dirname)
	      "\n\n"
	      ";; This file is part of GNU Emacs.\n\n"
	      ";;; Commentary:\n\n"
	      ";; Do byte-compile this file again after any modification.\n\n"
	      ";;; Start of the header of the original SKK dictionary.\n\n")
      (set-buffer skkbuf)
      (widen)
      (goto-char 1)
      (let (pos)
	(search-forward ";; okuri-ari")
	(forward-line 1)
	(setq pos (point))
	(set-buffer buf)
	(insert-buffer-substring skkbuf 1 pos))
      (insert "\n"
	      ";;; Code:\n\n(eval-when-compile (require 'ja-dic-cnv))\n\n")

      ;; Generate the body part of working buffer.
      (set-buffer skkbuf)
      (let ((from (point))
	    to)
	;; Convert okuri-ari entries.
	(search-forward ";; okuri-nasi")
	(beginning-of-line)
	(setq to (point))
	(narrow-to-region from to)
	(skkdic-convert-okuri-ari skkbuf buf)
	(widen)

	;; Convert okuri-nasi postfix entries.
	(goto-char to)
	(forward-line 1)
	(setq from (point))
	(re-search-forward "^\\cH")
	(setq to (match-beginning 0))
	(narrow-to-region from to)
	(skkdic-convert-postfix skkbuf buf)
	(widen)

	;; Convert okuri-nasi prefix entries.
	(goto-char to)
	(skkdic-convert-prefix skkbuf buf)

	;;
	(skkdic-collect-okuri-nasi)

	;; Convert okuri-nasi general entries.
	(skkdic-convert-okuri-nasi skkbuf buf)

	;; Postfix
	(with-current-buffer buf
	  (goto-char (point-max))
	  (insert ";;\n(provide 'ja-dic)\n\n;;; ja-dic.el ends here\n")))

      ;; Save the working buffer.
      (set-buffer buf)
      (set-visited-file-name (expand-file-name ja-dic-filename dirname) t)
      (set-buffer-file-coding-system 'euc-japan)
      (save-buffer 0))
    (kill-buffer skkbuf)
    (switch-to-buffer buf)))

(defun batch-skkdic-convert ()
  "Run `skkdic-convert' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
For example, invoke:
  % emacs -batch -l ja-dic-cnv -f batch-skkdic-convert SKK-JISYO.L
to generate  \"ja-dic.el\" from SKK dictionary file \"SKK-JISYO.L\".
To get complete usage, invoke:
 % emacs -batch -l ja-dic-cnv -f batch-skkdic-convert -h"
  (defvar command-line-args-left)	; Avoid compiler warning.
  (if (not noninteractive)
      (error "`batch-skkdic-convert' should be used only with -batch"))
  (if (string= (car command-line-args-left) "-h")
      (progn
	(message "To convert SKK-JISYO.L into skkdic.el:")
	(message "  %% emacs -batch -l ja-dic-cnv -f batch-skkdic-convert SKK-JISYO.L")
	(message "To convert SKK-JISYO.L into DIR/ja-dic.el:")
	(message "  %% emacs -batch -l ja-dic-cnv -f batch-skkdic-convert -dir DIR SKK-JISYO.L"))
    (let (targetdir filename)
      (if (string= (car command-line-args-left) "-dir")
	  (progn
	    (setq command-line-args-left (cdr command-line-args-left))
	    (setq targetdir (expand-file-name (car command-line-args-left)))
	    (setq command-line-args-left (cdr command-line-args-left))))
      (setq filename (expand-file-name (car command-line-args-left)))
      (message "Converting %s to %s ..." filename ja-dic-filename)
      (message "It takes around 10 minutes even on Sun SS20.")
      (skkdic-convert filename targetdir)
      (message "Do byte-compile the created file by:")
      (message "  %% emacs -batch -f batch-byte-compile %s" ja-dic-filename)
      ))
  (kill-emacs 0))


;; The following macros are expanded at byte-compiling time so that
;; compiled code can be loaded quickly.

(defun skkdic-get-kana-compact-codes (kana)
  (let* ((len (length kana))
	 (vec (make-vector len 0))
	 (i 0)
	 ch)
    (while (< i len)
      (setq ch (aref kana i))
      (aset vec i
	    (if (< ch 128)		; CH is an ASCII letter for OKURIGANA,
		(- ch)			;  represented by a negative code.
	      (if (= ch ?ー)		; `ー' is represented by 0.
		  0
		(- (logand (encode-char ch 'japanese-jisx0208) #xFF) 32))))
      (setq i (1+ i)))
    vec))

(defun skkdic-extract-conversion-data (entry)
  (string-match "^\\cj+[a-z]* " entry)
  (let ((kana (substring entry (match-beginning 0) (1- (match-end 0))))
	(i (match-end 0))
	candidates)
    (while (string-match "[^ ]+" entry i)
      (setq candidates (cons (match-string 0 entry) candidates))
      (setq i (match-end 0)))
    (cons (skkdic-get-kana-compact-codes kana) candidates)))

(defmacro skkdic-set-okuri-ari (&rest entries)
  `(defconst skkdic-okuri-ari
     ',(let ((l entries)
	     (map '(skkdic-okuri-ari))
	     entry)
	 (while l
	   (setq entry (skkdic-extract-conversion-data (car l)))
	   (set-nested-alist (car entry) (cdr entry) map)
	   (setq l (cdr l)))
	 map)))

(defmacro skkdic-set-postfix (&rest entries)
  `(defconst skkdic-postfix
     ',(let ((l entries)
	     (map '(nil))
	     (longest 1)
	     len entry)
	 (while l
	   (setq entry (skkdic-extract-conversion-data (car l)))
	   (setq len (length (car entry)))
	   (if (> len longest)
	       (setq longest len))
	   (let ((entry2 (lookup-nested-alist (car entry) map nil nil t)))
	     (if (consp (car entry2))
		 (let ((conversions (cdr entry)))
		   (while conversions
		     (if (not (member (car conversions) (car entry2)))
			 (setcar entry2 (cons (car conversions) (car entry2))))
		     (setq conversions (cdr conversions))))
	       (set-nested-alist (car entry) (cdr entry) map)))
	   (setq l (cdr l)))
	 (setcar map longest)
	 map)))

(defmacro skkdic-set-prefix (&rest entries)
  `(defconst skkdic-prefix
     ',(let ((l entries)
	     (map '(nil))
	     (longest 1)
	     len entry)
	 (while l
	   (setq entry (skkdic-extract-conversion-data (car l)))
	   (setq len (length (car entry)))
	   (if (> len longest)
	       (setq longest len))
	   (let ((entry2 (lookup-nested-alist (car entry) map len nil t)))
	     (if (consp (car entry2))
		 (let ((conversions (cdr entry)))
		   (while conversions
		     (if (not (member (car conversions) (car entry2)))
			 (setcar entry2 (cons (car conversions) (car entry2))))
		     (setq conversions (cdr conversions))))
	       (set-nested-alist (car entry) (cdr entry) map len)))
	   (setq l (cdr l)))
	 (setcar map longest)
	 map)))

(defmacro skkdic-set-okuri-nasi (&rest entries)
  `(defconst skkdic-okuri-nasi
     ',(let ((l entries)
	     (map '(skdic-okuri-nasi))
	     (count 0)
	     entry)
	 (while l
	   (setq count (1+ count))
	   (if (= (% count 10000) 0)
	       (message "%d entries" count))
	   (setq entry (skkdic-extract-conversion-data (car l)))
	   (set-nested-alist (car entry) (cdr entry) map)
	   (setq l (cdr l)))
	 map)))

(provide 'ja-dic-cnv)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; ja-dic-cnv.el ends here
