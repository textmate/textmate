;;; ezimage --- Generalized Image management

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

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
;; A few routines for placing an image over text that will work for any
;; Emacs implementation without error.  When images are not supported, then
;; they are just not displayed.
;;
;; The idea is that gui buffers (trees, buttons, etc) will have text
;; representations of the GUI elements.  These routines will replace the text
;; with an image when images are available.
;;
;; This file requires the `image' package if it is available.

(condition-case nil			; for older XEmacs
    (require 'image)
  (error nil))

;;; Code:
(defcustom ezimage-use-images (if (featurep 'xemacs)
				  (and (fboundp 'make-image-specifier)
				       window-system)
				(and (display-images-p)
				     (image-type-available-p 'xpm)))
  "Non-nil means ezimage should display icons."
  :group 'ezimage
  :version "21.1"
  :type 'boolean)

;;; Create our own version of defimage
(eval-and-compile

(if (featurep 'emacs)
    (progn
      (defmacro defezimage (variable imagespec docstring)
	"Define VARIABLE as an image if `defimage' is not available.
IMAGESPEC is the image data, and DOCSTRING is documentation for the image."
	`(progn
	   (defimage ,variable ,imagespec ,docstring)
	   (put (quote ,variable) 'ezimage t)))

;; This hack is for the ezimage install which has an icons directory for
;; the default icons to be used.
;; (add-to-list 'load-path
;; 	     (concat (file-name-directory
;; 		      (locate-library "ezimage.el"))
;; 		     "icons"))

       )

  ;; XEmacs.
  (if (not (fboundp 'make-glyph))

      (defmacro defezimage (variable _imagespec docstring)
	"Don't bother loading up an image...
Argument VARIABLE is the variable to define.
Argument IMAGESPEC is the list defining the image to create.
Argument DOCSTRING is the documentation for VARIABLE."
	`(defvar ,variable nil ,docstring))

    (defun ezimage-find-image-on-load-path (image)
      "Find the image file IMAGE on the load path."
      (let ((l (cons
		;; In XEmacs, try the data directory first (for an
		;; install in XEmacs proper.)   Search the load
		;; path next (for user installs)
		(locate-data-directory "ezimage")
		load-path))
	    (r nil))
	(while (and l (not r))
	  (if (file-exists-p (concat (car l) "/" image))
	      (setq r (concat (car l) "/" image))
	    (if (file-exists-p (concat (car l) "/icons/" image))
		(setq r (concat (car l) "/icons/" image))
	      ))
	  (setq l (cdr l)))
	r))

    (defun ezimage-convert-emacs21-imagespec-to-xemacs (spec)
      "Convert the Emacs21 image SPEC into an XEmacs image spec.
The Emacs 21 spec is what I first learned, and is easy to convert."
      (let* ((sl (car spec))
	     (itype (nth 1 sl))
	     (ifile (nth 3 sl)))
	(vector itype ':file (ezimage-find-image-on-load-path ifile))))

    (defmacro defezimage (variable imagespec docstring)
      "Define VARIABLE as an image if `defimage' is not available.
IMAGESPEC is the image data, and DOCSTRING is documentation for the image."
      `(progn
	 (defvar ,variable
	   ;; The Emacs21 version of defimage looks just like the XEmacs image
	   ;; specifier, except that it needs a :type keyword.  If we line
	   ;; stuff up right, we can use this cheat to support XEmacs specifiers.
	   (condition-case nil
	       (make-glyph
		(make-image-specifier
		 (ezimage-convert-emacs21-imagespec-to-xemacs (quote ,imagespec)))
		'buffer)
	     (error nil))
	   ,docstring)
	 (put ',variable 'ezimage t)))

    )))

(defezimage ezimage-directory
  ((:type xpm :file "ezimage/dir.xpm" :ascent center))
  "Image used for empty directories.")

(defezimage ezimage-directory-plus
  ((:type xpm :file "ezimage/dir-plus.xpm" :ascent center))
  "Image used for closed directories with stuff in them.")

(defezimage ezimage-directory-minus
  ((:type xpm :file "ezimage/dir-minus.xpm" :ascent center))
  "Image used for open directories with stuff in them.")

(defezimage ezimage-page-plus
  ((:type xpm :file "ezimage/page-plus.xpm" :ascent center))
  "Image used for closed files with stuff in them.")

(defezimage ezimage-page-minus
  ((:type xpm :file "ezimage/page-minus.xpm" :ascent center))
  "Image used for open files with stuff in them.")

(defezimage ezimage-page
  ((:type xpm :file "ezimage/page.xpm" :ascent center))
  "Image used for files with nothing interesting in it.")

(defezimage ezimage-tag
  ((:type xpm :file "ezimage/tag.xpm" :ascent center))
  "Image used for tags.")

(defezimage ezimage-tag-plus
  ((:type xpm :file "ezimage/tag-plus.xpm" :ascent center))
  "Image used for closed tag groups.")

(defezimage ezimage-tag-minus
  ((:type xpm :file "ezimage/tag-minus.xpm" :ascent center))
  "Image used for open tags.")

(defezimage ezimage-tag-gt
  ((:type xpm :file "ezimage/tag-gt.xpm" :ascent center))
  "Image used for closed tags (with twist arrow).")

(defezimage ezimage-tag-v
  ((:type xpm :file "ezimage/tag-v.xpm" :ascent center))
  "Image used for open tags (with twist arrow).")

(defezimage ezimage-tag-type
  ((:type xpm :file "ezimage/tag-type.xpm" :ascent center))
  "Image used for tags that represent a data type.")

(defezimage ezimage-box-plus
  ((:type xpm :file "ezimage/box-plus.xpm" :ascent center))
  "Image of a closed box.")

(defezimage ezimage-box-minus
  ((:type xpm :file "ezimage/box-minus.xpm" :ascent center))
  "Image of an open box.")

(defezimage ezimage-mail
  ((:type xpm :file "ezimage/mail.xpm" :ascent center))
  "Image of an envelope.")

(defezimage ezimage-checkout
  ((:type xpm :file "ezimage/checkmark.xpm" :ascent center))
  "Image representing a checkmark.  For files checked out of a VC.")

(defezimage ezimage-object
  ((:type xpm :file "ezimage/bits.xpm" :ascent center))
  "Image representing bits (an object file.)")

(defezimage ezimage-object-out-of-date
  ((:type xpm :file "ezimage/bitsbang.xpm" :ascent center))
  "Image representing bits with a ! in it.  (An out of data object file.)")

(defezimage ezimage-label
  ((:type xpm :file "ezimage/label.xpm" :ascent center))
  "Image used for label prefix.")

(defezimage ezimage-lock
  ((:type xpm :file "ezimage/lock.xpm" :ascent center))
  "Image of a lock.  Used for Read Only, or private.")

(defezimage ezimage-unlock
  ((:type xpm :file "ezimage/unlock.xpm" :ascent center))
  "Image of an unlocked lock.")

(defezimage ezimage-key
  ((:type xpm :file "ezimage/key.xpm" :ascent center))
  "Image of a key.")

(defezimage ezimage-document-tag
  ((:type xpm :file "ezimage/doc.xpm" :ascent center))
  "Image used to indicate documentation available.")

(defezimage ezimage-document-plus
  ((:type xpm :file "ezimage/doc-plus.xpm" :ascent center))
  "Image used to indicate closed documentation.")

(defezimage ezimage-document-minus
  ((:type xpm :file "ezimage/doc-minus.xpm" :ascent center))
  "Image used to indicate open documentation.")

(defezimage ezimage-info-tag
  ((:type xpm :file "ezimage/info.xpm" :ascent center))
  "Image used to indicate more information available.")

(defvar ezimage-expand-image-button-alist
  '(
    ;; here are some standard representations
    ("<+>" . ezimage-directory-plus)
    ("<->" . ezimage-directory-minus)
    ("< >" . ezimage-directory)
    ("[+]" . ezimage-page-plus)
    ("[-]" . ezimage-page-minus)
    ("[?]" . ezimage-page)
    ("[ ]" . ezimage-page)
    ("{+}" . ezimage-box-plus)
    ("{-}" . ezimage-box-minus)
    ;; Some vaguely representative entries
    ("*" . ezimage-checkout)
    ("#" . ezimage-object)
    ("!" . ezimage-object-out-of-date)
    ("%" . ezimage-lock)
    )
  "List of text and image associations.")

(defun ezimage-insert-image-button-maybe (start length &optional string)
  "Insert an image button based on text starting at START for LENGTH chars.
If buttontext is unknown, just insert that text.
If we have an image associated with it, use that image.
Optional argument STRING is a string upon which to add text properties."
  (when ezimage-use-images
    (let* ((bt (buffer-substring start (+ length start)))
	   (a (assoc bt ezimage-expand-image-button-alist)))
      ;; Regular images (created with `insert-image' are intangible
      ;; which (I suppose) make them more compatible with XEmacs 21.
      ;; Unfortunately, there is a giant pile of code dependent on the
      ;; underlying text.  This means if we leave it tangible, then I
      ;; don't have to change said giant piles of code.
      (if (and a (symbol-value (cdr a)))
	  (ezimage-insert-over-text (symbol-value (cdr a))
				    start
				    (+ start (length bt))))))
  string)

(defun ezimage-image-over-string (string &optional alist)
  "Insert over the text in STRING an image found in ALIST.
Return STRING with properties applied."
  (if ezimage-use-images
      (let ((a (assoc string alist)))
	(if (and a (symbol-value (cdr a)))
	    (ezimage-insert-over-text (symbol-value (cdr a))
				      0 (length string)
				      string)
	  string))
    string))

(defun ezimage-insert-over-text (image start end &optional string)
  "Place IMAGE over the text between START and END.
Assumes the image is part of a GUI and can be clicked on.
Optional argument STRING is a string upon which to add text properties."
  (when ezimage-use-images
    (add-text-properties start end
			 (if (featurep 'xemacs)
			     (list 'end-glyph image
				   'rear-nonsticky (list 'display)
				   'invisible t
				   'detachable t)
			   (list 'display image
				 'rear-nonsticky (list 'display)))
			 string))
  string)

(defun ezimage-image-association-dump ()
  "Dump out the current state of the Ezimage image alist.
See `ezimage-expand-image-button-alist' for details."
  (interactive)
  (with-output-to-temp-buffer "*Ezimage Images*"
    (with-current-buffer "*Ezimage Images*"
      (goto-char (point-max))
      (insert "Ezimage image cache.\n\n")
      (let ((start (point)) (end nil))
	(insert "Image\tText\tImage Name")
	(setq end (point))
	(insert "\n")
	(put-text-property start end 'face 'underline))
      (let ((ia ezimage-expand-image-button-alist))
	(while ia
	  (let ((start (point)))
	    (insert (car (car ia)))
	    (insert "\t")
	    (ezimage-insert-image-button-maybe start
						(length (car (car ia))))
	    (insert (car (car ia)) "\t" (format "%s" (cdr (car ia))) "\n"))
	  (setq ia (cdr ia)))))))

(defun ezimage-image-dump ()
  "Dump out the current state of the Ezimage image alist.
See `ezimage-expand-image-button-alist' for details."
  (interactive)
  (with-output-to-temp-buffer "*Ezimage Images*"
    (with-current-buffer "*Ezimage Images*"
      (goto-char (point-max))
      (insert "Ezimage image cache.\n\n")
      (let ((start (point)) (end nil))
	(insert "Image\tImage Name")
	(setq end (point))
	(insert "\n")
	(put-text-property start end 'face 'underline))
      (let ((ia (ezimage-all-images)))
	(while ia
	  (let ((start (point)))
	    (insert "cm")
	    (ezimage-insert-over-text (symbol-value (car ia)) start (point))
	    (insert "\t" (format "%s" (car ia)) "\n"))
	  (setq ia (cdr ia)))))))

(defun ezimage-all-images ()
  "Return a list of all variables containing ez images."
  (let ((ans nil))
    (mapatoms (lambda (sym)
		(if (get sym 'ezimage) (setq ans (cons sym ans)))))
    (setq ans (sort ans (lambda (a b)
			  (string< (symbol-name a) (symbol-name b)))))
    ans))

(provide 'ezimage)

;;; sb-image.el ends here
