;;; sb-image --- Image management for speedbar

;; Copyright (C) 1999-2003, 2005-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: file, tags, tools

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
;;
;; Supporting Image display for Emacs 20 and less, Emacs 21, and XEmacs,
;; is a challenging task, which doesn't take kindly to being byte compiled.
;; When sharing speedbar.elc between these three applications, the Image
;; support can get lost.
;;
;; By splitting out that hard part into this file, and avoiding byte
;; compilation, one copy speedbar can support all these platforms together.
;;
;; This file requires the `image' package if it is available.

(require 'ezimage)

;;; Code:
(defcustom speedbar-use-images ezimage-use-images
  "Non-nil if speedbar should display icons."
  :group 'speedbar
  :version "21.1"
  :type 'boolean)

(defalias 'defimage-speedbar 'defezimage)

(defvar speedbar-expand-image-button-alist
  '(("<+>" . ezimage-directory-plus)
    ("<->" . ezimage-directory-minus)
    ("< >" . ezimage-directory)
    ("[+]" . ezimage-page-plus)
    ("[-]" . ezimage-page-minus)
    ("[?]" . ezimage-page)
    ("[ ]" . ezimage-page)
    ("{+}" . ezimage-box-plus)
    ("{-}" . ezimage-box-minus)
    ("<M>" . ezimage-mail)
    ("<d>" . ezimage-document-tag)
    ("<i>" . ezimage-info-tag)
    (" =>" . ezimage-tag)
    (" +>" . ezimage-tag-gt)
    (" ->" . ezimage-tag-v)
    (">"   . ezimage-tag)
    ("@"   . ezimage-tag-type)
    ("  @" . ezimage-tag-type)
    ("*"   . ezimage-checkout)
    ("#"   . ezimage-object)
    ("!"   . ezimage-object-out-of-date)
    ("//"  . ezimage-label)
    ("%"   . ezimage-lock)
    )
  "List of text and image associations.")

(defun speedbar-insert-image-button-maybe (start length)
  "Insert an image button based on text starting at START for LENGTH chars.
If buttontext is unknown, just insert that text.
If we have an image associated with it, use that image."
  (when speedbar-use-images
    (let ((ezimage-expand-image-button-alist
	   speedbar-expand-image-button-alist))
      (ezimage-insert-image-button-maybe start length))))

(defun speedbar-image-dump ()
  "Dump out the current state of the Speedbar image alist.
See `speedbar-expand-image-button-alist' for details."
  (interactive)
  (with-output-to-temp-buffer "*Speedbar Images*"
    (with-current-buffer "*Speedbar Images*"
      (goto-char (point-max))
      (insert "Speedbar image cache.\n\n")
      (let ((start (point)) (end nil))
	(insert "Image\tText\tImage Name")
	(setq end (point))
	(insert "\n")
	(put-text-property start end 'face 'underline))
      (let ((ia speedbar-expand-image-button-alist))
	(while ia
	  (let ((start (point)))
	    (insert (car (car ia)))
	    (insert "\t")
	    (speedbar-insert-image-button-maybe start
						(length (car (car ia))))
	    (insert (car (car ia)) "\t" (format "%s" (cdr (car ia))) "\n"))
	  (setq ia (cdr ia)))))))

(provide 'sb-image)

;;; sb-image.el ends here
