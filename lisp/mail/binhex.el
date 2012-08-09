;;; binhex.el --- decode BinHex-encoded text

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: binhex news

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

;; BinHex is a binary-to-text encoding scheme similar to uuencode.
;; The command `binhex-decode-region' decodes BinHex-encoded text, via
;; the external program "hexbin" if that is available, or an Emacs
;; Lisp implementation if not.

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  (defalias 'binhex-char-int
    (if (fboundp 'char-int)
	'char-int
      'identity)))

(defgroup binhex nil
  "Decoding of BinHex (binary-to-hexadecimal) data."
  :group 'mail
  :group 'news)

(defcustom binhex-decoder-program "hexbin"
  "*Non-nil value should be a string that names a binhex decoder.
The program should expect to read binhex data on its standard
input and write the converted data to its standard output."
  :type 'string
  :group 'binhex)

(defcustom binhex-decoder-switches '("-d")
  "*List of command line flags passed to the command `binhex-decoder-program'."
  :group 'binhex
  :type '(repeat string))

(defcustom binhex-use-external
  (executable-find binhex-decoder-program)
  "*Use external binhex program."
  :version "22.1"
  :group 'binhex
  :type 'boolean)

(defconst binhex-alphabet-decoding-alist
  '(( ?\! . 0) ( ?\" . 1) ( ?\# . 2) ( ?\$ . 3) ( ?\% . 4) ( ?\& . 5)
    ( ?\' . 6) ( ?\( . 7) ( ?\) . 8) ( ?\* . 9) ( ?\+ . 10) ( ?\, . 11)
    ( ?\- . 12) ( ?0 . 13) ( ?1 . 14) ( ?2 . 15) ( ?3 . 16) ( ?4 . 17)
    ( ?5 . 18) ( ?6 . 19) ( ?8 . 20) ( ?9 . 21) ( ?@ . 22) ( ?A . 23)
    ( ?B . 24) ( ?C . 25) ( ?D . 26) ( ?E . 27) ( ?F . 28) ( ?G . 29)
    ( ?H . 30) ( ?I . 31) ( ?J . 32) ( ?K . 33) ( ?L . 34) ( ?M . 35)
    ( ?N . 36) ( ?P . 37) ( ?Q . 38) ( ?R . 39) ( ?S . 40) ( ?T . 41)
    ( ?U . 42) ( ?V . 43) ( ?X . 44) ( ?Y . 45) ( ?Z . 46) ( ?\[ . 47)
    ( ?\` . 48) ( ?a . 49) ( ?b . 50) ( ?c . 51) ( ?d . 52) ( ?e . 53)
    ( ?f . 54) ( ?h . 55) ( ?i . 56) ( ?j . 57) ( ?k . 58) ( ?l . 59)
    ( ?m . 60) ( ?p . 61) ( ?q . 62) ( ?r . 63)))

(defun binhex-char-map (char)
  (cdr (assq char binhex-alphabet-decoding-alist)))

;;;###autoload
(defconst binhex-begin-line
  "^:...............................................................$"
  "Regular expression matching the start of a BinHex encoded region.")
(defconst binhex-body-line
  "^[^:]...............................................................$")
(defconst binhex-end-line ":$")		; unused

(defvar binhex-temporary-file-directory
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	("/tmp/")))

(eval-and-compile
  (defalias 'binhex-insert-char
    (if (featurep 'xemacs)
	'insert-char
      (lambda (char &optional count ignored buffer)
	"Insert COUNT copies of CHARACTER into BUFFER."
	(if (or (null buffer) (eq buffer (current-buffer)))
	    (insert-char char count)
	  (with-current-buffer buffer
	    (insert-char char count)))))))

(defvar binhex-crc-table
  [0  4129  8258  12387  16516  20645  24774  28903
      33032  37161  41290  45419  49548  53677  57806  61935
      4657  528  12915  8786  21173  17044  29431  25302
      37689  33560  45947  41818  54205  50076  62463  58334
      9314  13379  1056  5121  25830  29895  17572  21637
      42346  46411  34088  38153  58862  62927  50604  54669
      13907  9842  5649  1584  30423  26358  22165  18100
      46939  42874  38681  34616  63455  59390  55197  51132
      18628  22757  26758  30887  2112  6241  10242  14371
      51660  55789  59790  63919  35144  39273  43274  47403
      23285  19156  31415  27286  6769  2640  14899  10770
      56317  52188  64447  60318  39801  35672  47931  43802
      27814  31879  19684  23749  11298  15363  3168  7233
      60846  64911  52716  56781  44330  48395  36200  40265
      32407  28342  24277  20212  15891  11826  7761  3696
      65439  61374  57309  53244  48923  44858  40793  36728
      37256  33193  45514  41451  53516  49453  61774  57711
      4224  161  12482  8419  20484  16421  28742  24679
      33721  37784  41979  46042  49981  54044  58239  62302
      689  4752  8947  13010  16949  21012  25207  29270
      46570  42443  38312  34185  62830  58703  54572  50445
      13538  9411  5280  1153  29798  25671  21540  17413
      42971  47098  34713  38840  59231  63358  50973  55100
      9939  14066  1681  5808  26199  30326  17941  22068
      55628  51565  63758  59695  39368  35305  47498  43435
      22596  18533  30726  26663  6336  2273  14466  10403
      52093  56156  60223  64286  35833  39896  43963  48026
      19061  23124  27191  31254  2801  6864  10931  14994
      64814  60687  56684  52557  48554  44427  40424  36297
      31782  27655  23652  19525  15522  11395  7392  3265
      61215  65342  53085  57212  44955  49082  36825  40952
      28183  32310  20053  24180  11923  16050  3793  7920])

(defun binhex-update-crc (crc char &optional count)
  (if (null count) (setq count 1))
  (while (> count 0)
    (setq crc (logxor (logand (lsh crc 8) 65280)
		      (aref binhex-crc-table
			    (logxor (logand (lsh crc -8) 255)
				    char)))
	  count (1- count)))
  crc)

(defun binhex-verify-crc (buffer start end)
  (with-current-buffer buffer
    (let ((pos start) (crc 0) (last (- end 2)))
      (while (< pos last)
	(setq crc (binhex-update-crc crc (char-after pos))
	      pos (1+ pos)))
      (if (= crc (binhex-string-big-endian (buffer-substring last end)))
	  nil
	(error "CRC error")))))

(defun binhex-string-big-endian (string)
  (let ((ret 0) (i 0) (len (length string)))
    (while (< i len)
      (setq ret (+ (lsh ret 8) (binhex-char-int (aref string i)))
	    i (1+ i)))
    ret))

(defun binhex-string-little-endian (string)
  (let ((ret 0) (i 0) (shift 0) (len (length string)))
    (while (< i len)
      (setq ret (+ ret (lsh (binhex-char-int (aref string i)) shift))
	    i (1+ i)
	    shift (+ shift 8)))
    ret))

(defun binhex-header (buffer)
  (with-current-buffer buffer
    (let ((pos (point-min)) len)
      (vector
       (prog1
	   (setq len (binhex-char-int (char-after pos)))
	 (setq pos (1+ pos)))
       (buffer-substring pos (setq pos (+ pos len)))
       (prog1
	   (setq len (binhex-char-int (char-after pos)))
	 (setq pos (1+ pos)))
       (buffer-substring pos (setq pos (+ pos 4)))
       (buffer-substring pos (setq pos (+ pos 4)))
       (binhex-string-big-endian
	(buffer-substring pos (setq pos (+ pos 2))))
       (binhex-string-big-endian
	(buffer-substring pos (setq pos (+ pos 4))))
       (binhex-string-big-endian
	(buffer-substring pos (setq pos (+ pos 4))))))))

(defvar binhex-last-char)
(defvar binhex-repeat)

(defun binhex-push-char (char &optional count ignored buffer)
  (cond
   (binhex-repeat
    (if (eq char 0)
	(binhex-insert-char (setq binhex-last-char 144) 1
			    ignored buffer)
      (binhex-insert-char binhex-last-char (- char 1)
			  ignored buffer)
      (setq binhex-last-char nil))
    (setq binhex-repeat nil))
   ((= char 144)
    (setq binhex-repeat t))
   (t
    (binhex-insert-char (setq binhex-last-char char) 1 ignored buffer))))

;;;###autoload
(defun binhex-decode-region-internal (start end &optional header-only)
  "Binhex decode region between START and END without using an external program.
If HEADER-ONLY is non-nil only decode header and return filename."
  (interactive "r")
  (let ((work-buffer nil)
	(counter 0)
	(bits 0) (tmp t)
	(lim 0) inputpos
	(non-data-chars " \t\n\r:")
	file-name-length data-fork-start
	header
	binhex-last-char binhex-repeat)
    (unwind-protect
	(save-excursion
	  (goto-char start)
	  (when (re-search-forward binhex-begin-line end t)
            (setq work-buffer (generate-new-buffer " *binhex-work*"))
	    (unless (featurep 'xemacs)
	      (with-current-buffer work-buffer (set-buffer-multibyte nil)))
	    (beginning-of-line)
	    (setq bits 0 counter 0)
	    (while tmp
	      (skip-chars-forward non-data-chars end)
	      (setq inputpos (point))
	      (end-of-line)
	      (setq lim (point))
	      (while (and (< inputpos lim)
			  (setq tmp (binhex-char-map (char-after inputpos))))
		(setq bits (+ bits tmp)
		      counter (1+ counter)
		      inputpos (1+ inputpos))
		(cond ((= counter 4)
		       (binhex-push-char (lsh bits -16) 1 nil work-buffer)
		       (binhex-push-char (logand (lsh bits -8) 255) 1 nil
					 work-buffer)
		       (binhex-push-char (logand bits 255) 1 nil
					 work-buffer)
		       (setq bits 0 counter 0))
		      (t (setq bits (lsh bits 6)))))
	      (if (null file-name-length)
		  (with-current-buffer work-buffer
		    (setq file-name-length (char-after (point-min))
			  data-fork-start (+ (point-min)
					     file-name-length 22))))
	      (when (and (null header)
			 (with-current-buffer work-buffer
			   (>= (buffer-size) data-fork-start)))
		(binhex-verify-crc work-buffer
				   (point-min) data-fork-start)
		(setq header (binhex-header work-buffer))
		(when header-only (setq tmp nil counter 0)))
	      (setq tmp (and tmp (not (eq inputpos end)))))
	    (cond
	     ((= counter 3)
	      (binhex-push-char (logand (lsh bits -16) 255) 1 nil
				work-buffer)
	      (binhex-push-char (logand (lsh bits -8) 255) 1 nil
				work-buffer))
	     ((= counter 2)
	      (binhex-push-char (logand (lsh bits -10) 255) 1 nil
				work-buffer))))
	  (if header-only nil
	    (binhex-verify-crc work-buffer
			       data-fork-start
			       (+ data-fork-start (aref header 6) 2))
	    (or (markerp end) (setq end (set-marker (make-marker) end)))
	    (goto-char start)
	    (insert-buffer-substring work-buffer
				     data-fork-start (+ data-fork-start
							(aref header 6)))
	    (delete-region (point) end)))
      (and work-buffer (kill-buffer work-buffer)))
    (if header (aref header 1))))

;;;###autoload
(defun binhex-decode-region-external (start end)
  "Binhex decode region between START and END using external decoder."
  (interactive "r")
  (let ((cbuf (current-buffer)) firstline work-buffer status
	(file-name (expand-file-name
		    (concat (binhex-decode-region-internal start end t)
			    ".data")
		    binhex-temporary-file-directory)))
    (save-excursion
      (goto-char start)
      (when (re-search-forward binhex-begin-line nil t)
	(let ((cdir default-directory) default-process-coding-system)
	  (unwind-protect
	      (progn
		(set-buffer (setq work-buffer
				  (generate-new-buffer " *binhex-work*")))
		(buffer-disable-undo work-buffer)
		(insert-buffer-substring cbuf firstline end)
		(cd binhex-temporary-file-directory)
		(apply 'call-process-region
		       (point-min)
		       (point-max)
		       binhex-decoder-program
		       nil
		       nil
		       nil
		       binhex-decoder-switches))
	    (cd cdir) (set-buffer cbuf)))
	(if (and file-name (file-exists-p file-name))
	    (progn
	      (goto-char start)
	      (delete-region start end)
	      (let (format-alist)
		(insert-file-contents-literally file-name)))
	  (error "Can not binhex")))
      (and work-buffer (kill-buffer work-buffer))
      (ignore-errors
	(if file-name (delete-file file-name))))))

;;;###autoload
(defun binhex-decode-region (start end)
  "Binhex decode region between START and END."
  (interactive "r")
  (if binhex-use-external
      (binhex-decode-region-external start end)
    (binhex-decode-region-internal start end)))

(provide 'binhex)

;;; binhex.el ends here
