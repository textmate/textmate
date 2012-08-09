;;; swedish.el --- miscellaneous functions for dealing with Swedish

;; Copyright (C) 1988, 2001-2012  Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n
;; Obsolete-since: 22.1

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

;; Fixme: Is this actually used?  if so, it should be in language,
;; possibly as a feature property of Swedish, probably defining a
;; `swascii' coding system.

;;; Code:

;; Written by Howard Gayle.  See case-table.el for details.

;; See iso-swed.el for a description of the character set.

(defvar mail-send-hook)
(defvar news-group-hook-alist)
(defvar news-inews-hook)

(defvar swedish-re
  "[ \t\n]\\(och\\|att\\|en\\|{r\\|\\[R\\|p}\\|P\\]\\|som\\|det\\|av\\|den\\|f|r\\|F\\\\R\\)[ \t\n.,?!:;'\")}]"
  "Regular expression for common Swedish words.")

(defvar swascii-to-8859-trans
  (let ((string (make-string 256 ? ))
	(i 0))
    (while (< i 256)
      (aset string i i)
      (setq i (1+ i)))
    (aset string ?\[ 196)
    (aset string ?\] 197)
    (aset string ?\\ 214)
    (aset string ?^ 220)
    (aset string ?\{ 228)
    (aset string ?\} 229)
    (aset string ?\` 233)
    (aset string ?\| 246)
    (aset string ?~ 252)
    string)
  "Trans table from SWASCII to 8859.")

; $ is not converted because it almost always means US
; dollars, not general currency sign.  @ is not converted
; because it is more likely to be an at sign in a mail address
; than an E with acute accent.

(defun swascii-to-8859-buffer ()
  "Convert characters in buffer from Swedish/Finnish-ascii to ISO 8859/1.
Works even on read-only buffers.  `$' and `@' are not converted."
  (interactive)
  (let  ((buffer-read-only nil))
    (translate-region (point-min) (point-max) swascii-to-8859-trans)))

(defun swascii-to-8859-buffer-maybe ()
  "Call swascii-to-8859-buffer if the buffer looks like Swedish-ascii.
Leaves point just after the word that looks Swedish."
  (interactive)
  (let ((case-fold-search t))
    (if (re-search-forward swedish-re nil t)
	(swascii-to-8859-buffer))))

(setq rmail-show-message-hook 'swascii-to-8859-buffer-maybe)

(setq news-group-hook-alist
      (append '(("^swnet." . swascii-to-8859-buffer-maybe))
	      (bound-and-true-p news-group-hook-alist)))

(defvar 8859-to-swascii-trans
  (let ((string (make-string 256 ? ))
	(i 0))
    (while (< i 256)
      (aset string i i)
      (setq i (1+ i)))
    (aset string 164 ?$)
    (aset string 196 ?\[)
    (aset string 197 ?\])
    (aset string 201 ?@)
    (aset string 214 ?\\)
    (aset string 220 ?^)
    (aset string 228 ?\{)
    (aset string 229 ?\})
    (aset string 233 ?\`)
    (aset string 246 ?\|)
    (aset string 252 ?~)
    string)
  "8859 to SWASCII trans table.")

(defun 8859-to-swascii-buffer ()
   "Convert characters in buffer from ISO 8859/1 to Swedish/Finnish-ascii."
   (interactive "*")
   (translate-region (point-min) (point-max) 8859-to-swascii-trans))

(setq mail-send-hook  '8859-to-swascii-buffer)
(setq news-inews-hook '8859-to-swascii-buffer)

;; It's not clear what purpose is served by a separate
;; Swedish mode that differs from Text mode only in having
;; a separate abbrev table.  Nothing says that the abbrevs you
;; define in Text mode have to be English!

;(defvar swedish-mode-abbrev-table nil
;   "Abbrev table used while in swedish mode.")
;(define-abbrev-table 'swedish-mode-abbrev-table ())

;(defun swedish-mode ()
;   "Major mode for editing Swedish text intended for humans to
;read.  Special commands:\\{text-mode-map}
;Turning on swedish-mode calls the value of the variable
;text-mode-hook, if that value is non-nil."
;   (interactive)
;   (kill-all-local-variables)
;   (use-local-map text-mode-map)
;   (setq mode-name "Swedish")
;   (setq major-mode 'swedish-mode)
;   (setq local-abbrev-table swedish-mode-abbrev-table)
;   (set-syntax-table text-mode-syntax-table)
;   (run-mode-hooks 'text-mode-hook))

;(defun indented-swedish-mode ()
;   "Major mode for editing indented Swedish text intended for
;humans to read.\\{indented-text-mode-map}
;Turning on indented-swedish-mode calls the value of the
;variable text-mode-hook, if that value is non-nil."
;   (interactive)
;   (kill-all-local-variables)
;   (use-local-map text-mode-map)
;   (define-abbrev-table 'swedish-mode-abbrev-table ())
;   (setq local-abbrev-table swedish-mode-abbrev-table)
;   (set-syntax-table text-mode-syntax-table)
;   (make-local-variable 'indent-line-function)
;   (setq indent-line-function 'indent-relative-maybe)
;   (use-local-map indented-text-mode-map)
;   (setq mode-name "Indented Swedish")
;   (setq major-mode 'indented-swedish-mode)
;   (run-mode-hooks 'text-mode-hook))

(provide 'swedish)

;;; swedish.el ends here
