;;; yenc.el --- elisp native yenc decoder

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Keywords: yenc news

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

;; Functions for decoding yenc encoded messages.
;;
;; Limitations:
;;
;; * Does not handle multipart messages.
;; * No support for external decoders.
;; * Doesn't check the crc32 checksum (if present).

;;; Code:

(eval-when-compile (require 'cl))

(defconst yenc-begin-line
  "^=ybegin.*$")

(defconst yenc-decoding-vector
  [214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230
       231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247
       248 249 250 251 252 253 254 255 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
       16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38
       39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61
       62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84
       85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105
       106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
       123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
       140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
       157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173
       174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190
       191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
       208 209 210 211 212 213])

(defun yenc-first-part-p ()
  "Say whether the buffer contains the first part of a yEnc file."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^=ybegin part=1 " nil t)))

(defun yenc-last-part-p ()
  "Say whether the buffer contains the last part of a yEnc file."
  (save-excursion
    (goto-char (point-min))
    (let (total-size end-size)
      (when (re-search-forward "^=ybegin.*size=\\([0-9]+\\)" nil t)
	(setq total-size (match-string 1)))
      (when (re-search-forward "^=ypart.*end=\\([0-9]+\\)" nil t)
	(setq end-size (match-string 1)))
      (and total-size
	   end-size
	   (string= total-size end-size)))))

;;;###autoload
(defun yenc-decode-region (start end)
  "Yenc decode region between START and END using an internal decoder."
  (interactive "r")
  (let (work-buffer)
    (unwind-protect
	(save-excursion
	  (goto-char start)
	  (when (re-search-forward yenc-begin-line end t)
	    (let ((first (match-end 0))
		  (header-alist (yenc-parse-line (match-string 0)))
		  bytes last footer-alist char)
	      (when (re-search-forward "^=ypart.*$" end t)
		(setq first (match-end 0)))
	      (when (re-search-forward "^=yend.*$" end t)
		(setq last (match-beginning 0))
		(setq footer-alist (yenc-parse-line (match-string 0)))
		(setq work-buffer (generate-new-buffer " *yenc-work*"))
		(unless (featurep 'xemacs)
		  (with-current-buffer work-buffer (set-buffer-multibyte nil)))
		(while (< first last)
		  (setq char (char-after first))
		  (cond ((or (eq char ?\r)
			     (eq char ?\n)))
			((eq char ?=)
			 (setq char (char-after (incf first)))
			 (with-current-buffer work-buffer
			   (insert-char (mod (- char 106) 256) 1)))
			(t
			 (with-current-buffer work-buffer
			   ;;(insert-char (mod (- char 42) 256) 1)
			   (insert-char (aref yenc-decoding-vector char) 1))))
		  (incf first))
		(setq bytes (buffer-size work-buffer))
		(unless (and (= (cdr (assq 'size header-alist)) bytes)
			     (= (cdr (assq 'size footer-alist)) bytes))
		  (message "Warning: Size mismatch while decoding."))
		(goto-char start)
		(delete-region start end)
		(insert-buffer-substring work-buffer))))
	  (and work-buffer (kill-buffer work-buffer))))))

;;;###autoload
(defun yenc-extract-filename ()
  "Extract file name from an yenc header."
  (save-excursion
    (when (re-search-forward yenc-begin-line nil t)
      (cdr (assoc 'name (yenc-parse-line (match-string 0)))))))

(defun yenc-parse-line (str)
  "Extract file name and size from STR."
  (let (result name)
    (when (string-match "^=y.*size=\\([0-9]+\\)" str)
      (push (cons 'size (string-to-number (match-string 1 str))) result))
    (when (string-match "^=y.*name=\\(.*\\)$" str)
      (setq name (match-string 1 str))
      ;; Remove trailing white space
      (when (string-match " +$" name)
	(setq name (substring name 0 (match-beginning 0))))
      (push (cons 'name name) result))
    result))

(provide 'yenc)

;;; yenc.el ends here
