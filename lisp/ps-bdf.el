;;; ps-bdf.el --- BDF font file handler for ps-print

;; Copyright (C) 1998-1999, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Author: Kenichi Handa <handa@m17n.org>
;; (according to ack.texi)
;; Keywords: wp, BDF, font, PostScript
;; Package: ps-print

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

;; Functions for getting bitmap information from X's BDF font file are
;; provided.

;;; Code:

(require 'ps-mule)

;;;###autoload
(defcustom bdf-directory-list
  (if (memq system-type '(ms-dos windows-nt))
      (list (expand-file-name "fonts/bdf" installation-directory))
    '("/usr/local/share/emacs/fonts/bdf"))
  "List of directories to search for `BDF' font files.
The default value is '(\"/usr/local/share/emacs/fonts/bdf\")."
  :type '(repeat :tag "BDF font directory list"
		 (directory :tag "BDF font directory"))
  :group 'ps-print-miscellany)

;; MS-DOS and MS-Windows users like to move the binary around after
;; it's built, but the value above is computed at load-up time.
(and (memq system-type '(ms-dos windows-nt))
     (setq bdf-directory-list
	   (list (expand-file-name "fonts/bdf" installation-directory))))

(defun bdf-expand-file-name (bdfname)
  "Return an absolute path name of a `BDF' font file BDFNAME.
It searches directories listed in the variable `bdf-directory-list'
for BDFNAME."
  (if (file-name-absolute-p bdfname)
      (and (file-readable-p bdfname)
	   bdfname)
    (catch 'tag
      (dolist (dir bdf-directory-list)
	(let ((absolute-path (expand-file-name bdfname dir)))
	  (if (file-readable-p absolute-path)
	    (throw 'tag absolute-path)))))))

(defsubst bdf-file-mod-time (filename)
  "Return modification time of FILENAME.
The value is a list of two integers, the first integer has high-order
16 bits, the second has low 16 bits."
  (nth 5 (file-attributes filename)))

(defun bdf-file-newer-than-time (filename mod-time)
  "Return non-nil if and only if FILENAME is newer than MOD-TIME.
MOD-TIME is a modification time as a list of two integers, the first
integer has high-order 16 bits, the second has low 16 bits."
  (let* ((new-mod-time (bdf-file-mod-time filename))
	 (new-time (car new-mod-time))
	 (time (car mod-time)))
    (or (> new-time time)
	(and (= new-time time)
	     (> (nth 1 new-mod-time) (nth 1 mod-time))))))

(defun bdf-find-file (bdfname)
  "Return a buffer visiting a bdf file BDFNAME.
BDFNAME must be an absolute file name.
If BDFNAME doesn't exist, return nil."
  (and (file-readable-p bdfname)
       (let ((buf (generate-new-buffer " *bdf-work*"))
	     (coding-system-for-read 'no-conversion))
	 (with-current-buffer buf
	   (insert-file-contents bdfname)
	   buf))))

(defvar bdf-cache-file (if (eq system-type 'ms-dos)
			   ;; convert-standard-filename doesn't
			   ;; guarantee that the .el extension will be
			   ;; preserved.
			   "~/_bdfcache.el"
			 (convert-standard-filename "~/.bdfcache.el"))
  "Name of cache file which contains information of `BDF' font files.")

(defvar bdf-cache nil
  "Cached information of `BDF' font files.  It is a list of FONT-INFO.
FONT-INFO is a list of the following format:
    (ABSOLUTE-FILE-NAME MOD-TIME SIZE FONT-BOUNDING-BOX
     RELATIVE-COMPOSE BASELINE-OFFSET CODE-RANGE MAXLEN OFFSET-VECTOR)
See the documentation of the function `bdf-read-font-info' for more detail.")

(defun bdf-read-cache ()
  "Return a cached information about `BDF' font files from a cache file.
The variable `bdf-cache-file' holds the cache file name.
If the cache file is not readable, this return nil."
  (setq bdf-cache nil)
  (condition-case nil
      (and (file-readable-p bdf-cache-file)
	   (progn
	     (load-file bdf-cache-file)
	     (if (listp bdf-cache)
		 bdf-cache
	       (setq bdf-cache nil))))
    (error nil)))

(defun bdf-write-cache ()
  "Write out cached information of `BDF' font file to a file.
The variable `bdf-cache-file' holds the cache file name.
The file is written if and only if the file already exists and writable."
  (and bdf-cache
       (file-exists-p bdf-cache-file)
       (file-writable-p bdf-cache-file)
       (write-region (format "(setq bdf-cache '%S)\n" bdf-cache)
		     nil bdf-cache-file)))

(defun bdf-set-cache (font-info)
  "Cache FONT-INFO as information about one `BDF' font file.
FONT-INFO is a list of the following format:
    (ABSOLUTE-FILE-NAME MOD-TIME SIZE FONT-BOUNDING-BOX
     RELATIVE-COMPOSE BASELINE-OFFSET CODE-RANGE MAXLEN OFFSET-VECTOR)
See the documentation of the function `bdf-read-font-info' for more detail."
  (let ((slot (assoc (car font-info) bdf-cache)))
    (if slot
	(setcdr slot (cdr font-info))
      (setq bdf-cache (cons font-info bdf-cache)))))

(defun bdf-initialize ()
  "Initialize `bdf' library."
  (and (bdf-read-cache)
       (add-hook 'kill-emacs-hook 'bdf-write-cache)))

(defun bdf-compact-code (code code-range)
  (if (or (< code (aref code-range 4))
	  (> code (aref code-range 5)))
      (setq code (aref code-range 6)))
  (+ (* (- (lsh code -8) (aref code-range 0))
	(1+ (- (aref code-range 3) (aref code-range 2))))
     (- (logand code 255) (aref code-range 2))))

(defun bdf-expand-code (code code-range)
  (let ((code0-range (1+ (- (aref code-range 3) (aref code-range 2)))))
    (+ (* (+ (/ code code0-range) (aref code-range 0)) 256)
       (+ (% code code0-range) (aref code-range 2)))))

(defun bdf-search-and-read (match limit)
  (goto-char (point-min))
  (and (search-forward match limit t)
       (progn
	 (goto-char (match-end 0))
	 (read (current-buffer)))))

(defun bdf-read-font-info (bdfname)
  "Read `BDF' font file BDFNAME and return information (FONT-INFO) of the file.
BDFNAME must be an absolute file name.
FONT-INFO is a list of the following format:
    (BDFFILE MOD-TIME FONT-BOUNDING-BOX
     RELATIVE-COMPOSE BASELINE-OFFSET CODE-RANGE MAXLEN OFFSET-VECTOR)

MOD-TIME is last modification time as a list of two integers, the
first integer has high-order 16 bits, the second has low 16 bits.

SIZE is a size of the font on 72 dpi device.  This value is got
from SIZE record of the font.

FONT-BOUNDING-BOX is the font bounding box as a list of four integers,
BBX-WIDTH, BBX-HEIGHT, BBX-XOFF, and BBX-YOFF.

RELATIVE-COMPOSE is an integer value of the font's property
`_MULE_RELATIVE_COMPOSE'.  If the font doesn't have this property, the
value is 0.

BASELINE-OFFSET is an integer value of the font's property
`_MULE_BASELINE_OFFSET'.  If the font doesn't have this property, the
value is 0.

CODE-RANGE is a vector of minimum 1st byte, maximum 1st byte, minimum
2nd byte, maximum 2nd byte, minimum code, maximum code, and default
code.  For 1-byte fonts, the first two elements are 0.

MAXLEN is a maximum bytes of one glyph information in the font file.

OFFSET-VECTOR is a vector of a file position which starts bitmap data
of the glyph in the font file.

Nth element of OFFSET-VECTOR is a file position for the glyph of code
CODE, where N and CODE are in the following relation:
    (bdf-compact-code CODE) => N, (bdf-expand-code N) => CODE"
  (let* ((buf (bdf-find-file bdfname))
	 (maxlen 0)
	 (relative-compose 'false)
	 (baseline-offset 0)
	 size
	 dpi
	 font-bounding-box
	 default-char
	 code-range
	 offset-vector)
    (if buf
	(message "Reading %s..." bdfname)
      (error "BDF file %s doesn't exist" bdfname))
    (unwind-protect
	(with-current-buffer buf
	  (goto-char (point-min))
	  (search-forward "\nFONTBOUNDINGBOX")
	  (setq font-bounding-box
		(vector (read (current-buffer)) (read (current-buffer))
			(read (current-buffer)) (read (current-buffer))))
	  ;; The following kludgy code is to avoid bugs of fonts
	  ;; jiskan16.bdf and jiskan24.bdf distributed with X.
	  ;; They contain wrong FONTBOUNDINGBOX.
	  (and (> (aref font-bounding-box 3) 0)
	       (string-match "jiskan\\(16\\|24\\)" bdfname)
	       (aset font-bounding-box 3
		     (- (aref font-bounding-box 3))))

	  (goto-char (point-min))
	  (search-forward "\nFONT ")
	  (if (looking-at "-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-\\([0-9]+\\)")
	      (setq size (string-to-number (match-string 1)))
	    (search-forward "\nSIZE ")
	    (setq size (read (current-buffer)))
	    ;; The following kludgy code is to avoid bugs of several
	    ;; fonts which have wrong SIZE record.
	    (and (string-match "jiskan" bdfname)
		 (<= size (/ (aref font-bounding-box 1) 3))
		 (setq size (aref font-bounding-box 1)))
	    (setq dpi (read (current-buffer)))
	    (if (and (> dpi 0) (/= dpi 72))
		(setq size (/ (* size dpi) 72))))

	  (setq default-char (bdf-search-and-read "\nDEFAULT_CHAR" nil))

	  (search-forward "\nSTARTCHAR")
	  (forward-line -1)
	  (let ((limit (point)))
	    (setq relative-compose
		  (or (bdf-search-and-read "\n_MULE_RELATIVE_COMPOSE" limit)
		      'false)
		  baseline-offset
		  (or (bdf-search-and-read "\n_MULE_BASELINE_OFFSET" limit)
		      0)))

	  (let ((min-code0 256) (min-code1 256) (min-code 65536)
		(max-code0 0) (max-code1 0) (max-code 0)
		glyph glyph-list code0 code1 code offset)

	    (while (search-forward "\nSTARTCHAR" nil t)
	      (setq offset (line-beginning-position))
	      (search-forward "\nENCODING")
	      (setq code (read (current-buffer)))
	      (if (< code 0)
		  (search-forward "ENDCHAR")
		(setq code0 (lsh code -8)
		      code1 (logand code 255)
		      min-code (min min-code code)
		      max-code (max max-code code)
		      min-code0 (min min-code0 code0)
		      max-code0 (max max-code0 code0)
		      min-code1 (min min-code1 code1)
		      max-code1 (max max-code1 code1))
		(search-forward "ENDCHAR")
		(setq maxlen (max maxlen (- (point) offset))
		      glyph-list (cons (cons code offset) glyph-list))))

	    (setq code-range
		  (vector min-code0 max-code0 min-code1 max-code1
			  min-code max-code (or default-char min-code))
		  offset-vector
		  (make-vector (1+ (bdf-compact-code max-code code-range))
			       nil))

	    (while glyph-list
	      (setq glyph (car glyph-list)
		    glyph-list (cdr glyph-list))
	      (aset offset-vector
		    (bdf-compact-code (car glyph) code-range)
		    (cdr glyph)))))

    (kill-buffer buf))
  (message "Reading %s...done" bdfname)
  (list bdfname (bdf-file-mod-time bdfname)
	size font-bounding-box relative-compose baseline-offset
	code-range maxlen offset-vector)))

(defsubst bdf-info-absolute-path (font-info)     (nth 0 font-info))
(defsubst bdf-info-mod-time (font-info)          (nth 1 font-info))
(defsubst bdf-info-size (font-info)              (nth 2 font-info))
(defsubst bdf-info-font-bounding-box (font-info) (nth 3 font-info))
(defsubst bdf-info-relative-compose (font-info)  (nth 4 font-info))
(defsubst bdf-info-baseline-offset (font-info)   (nth 5 font-info))
(defsubst bdf-info-code-range (font-info)        (nth 6 font-info))
(defsubst bdf-info-maxlen (font-info)            (nth 7 font-info))
(defsubst bdf-info-offset-vector (font-info)     (nth 8 font-info))

(defun bdf-get-font-info (bdfname)
  "Return information about `BDF' font file BDFNAME.
BDFNAME must be an absolute file name.
The value FONT-INFO is a list of the following format:
    (BDFNAME MOD-TIME SIZE FONT-BOUNDING-BOX
     RELATIVE-COMPOSE BASELINE-OFFSET CODE-RANGE MAXLEN OFFSET-VECTOR)
See the documentation of the function `bdf-read-font-info' for more detail."
  (or bdf-cache
      (bdf-read-cache))
  (let ((font-info (assoc bdfname bdf-cache)))
    (if (or (not font-info)
	    (not (file-readable-p bdfname))
	    (bdf-file-newer-than-time bdfname (bdf-info-mod-time font-info)))
	(progn
	  (setq font-info (bdf-read-font-info bdfname))
	  (bdf-set-cache font-info)))
    font-info))

(defun bdf-read-bitmap (bdfname offset maxlen relative-compose)
  "Read `BDF' font file BDFNAME to get bitmap data at file position OFFSET.
BDFNAME is an absolute path name of the font file.
MAXLEN specifies how many bytes we should read at least.
The value is a list of DWIDTH, BBX, and BITMAP-STRING.
DWIDTH is a pixel width of a glyph.
BBX is a bounding box of the glyph.
BITMAP-STRING is a string representing bits by hexadecimal digits."
  (let* ((coding-system-for-read 'no-conversion)
	 (bbx (bdf-info-font-bounding-box (bdf-get-font-info bdfname)))
	 (dwidth (elt bbx 0))
	 (bitmap-string "")
	 height yoff)
    (condition-case nil
	(with-temp-buffer
	  (insert-file-contents bdfname nil offset (+ offset maxlen))
	  (goto-char (point-min))
	  (search-forward "\nDWIDTH")
	  (setq dwidth (read (current-buffer)))
	  (if (= dwidth 0)
	      (setq dwidth 0.1))
	  (goto-char (point-min))
	  (search-forward "\nBBX")
	  (setq bbx (vector (read (current-buffer)) (read (current-buffer))
			    (read (current-buffer)) (read (current-buffer)))
		height (aref bbx 1)
		yoff (aref bbx 3))
	  (search-forward "\nBITMAP")
	  (forward-line 1)
	  (delete-region (point-min) (point))
	  (and (looking-at "\\(0+\n\\)+")
	       (progn
		 (setq height (- height (count-lines (point) (match-end 0))))
		 (delete-region (point) (match-end 0))))
	  (or (looking-at "ENDCHAR")
	      (progn
		(search-forward "ENDCHAR" nil 'move)
		(forward-line -1)
		(while (looking-at "0+$")
		  (setq yoff (1+ yoff)
			height (1- height))
		  (forward-line -1))
		(forward-line 1)))
	  (aset bbx 1 height)
	  (aset bbx 3 yoff)
	  (delete-region (point) (point-max))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (end-of-line)
	    (delete-char 1))
	  (setq bitmap-string (buffer-string)))
      (error nil))
    (vector dwidth (aref bbx 0) (aref bbx 1) (aref bbx 2) (aref bbx 3)
	    (concat "<" bitmap-string ">")
	    (or relative-compose 'false))))

(defun bdf-get-bitmap (bdfname code)
  "Return bitmap information of glyph of CODE in `BDF' font file BDFNAME.
CODE is an encoding number of glyph in the file.
The value is a list (DWIDTH BBX BITMAP-STRING).
DWIDTH is a pixel width of a glyph.
BBX is a bounding box of the glyph.
BITMAP-STRING is a string representing bits by hexadecimal digits."
  (let* ((info (bdf-get-font-info bdfname))
	 (maxlen (bdf-info-maxlen info))
	 (code-range (bdf-info-code-range info))
	 (offset-vector (bdf-info-offset-vector info)))
    (bdf-read-bitmap bdfname
		     (aref offset-vector (bdf-compact-code code code-range))
		     maxlen (bdf-info-relative-compose info))))

;;; Interface to ps-mule.el

;; Called from ps-mule-init-external-library.
(defun bdf-generate-prologue ()
  (or bdf-cache
      (bdf-initialize))
  (ps-mule-generate-bitmap-prologue))

;; Called from ps-mule-check-font.
(defun bdf-check-font (font-spec)
  (let ((font-name-list (ps-mule-font-spec-name font-spec)))
    (ps-mule-font-spec-set-name
     font-spec
     (if (stringp font-name-list)
	 (bdf-expand-file-name font-name-list)
       (catch 'tag
	 (dolist (font-name font-name-list)
	   (setq font-name (bdf-expand-file-name font-name))
	   (if font-name
	       (throw 'tag font-name))))))))

;; Called from ps-mule-generate-font.
(defun bdf-generate-font (font-spec)
  (let ((info (bdf-get-font-info (ps-mule-font-spec-name font-spec))))
    (ps-mule-font-spec-set-extra
     font-spec (bdf-info-absolute-path info))
    (ps-mule-generate-bitmap-font font-spec
				  (bdf-info-size info)
				  (bdf-info-relative-compose info)
				  (bdf-info-baseline-offset info)
				  (bdf-info-font-bounding-box info))))

;; Called from ps-mule-generate-glyph.
(defun bdf-generate-glyph (font-spec char)
  (let ((font-name (ps-mule-font-spec-extra font-spec))
	(code (ps-mule-encode-char char font-spec)))
    (ps-mule-generate-bitmap-glyph font-spec char code
				   (bdf-get-bitmap font-name code))))

(provide 'ps-bdf)

;;; ps-bdf.el ends here
