;;; image-file.el --- support for visiting image files
;;
;; Copyright (C) 2000-2012 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: multimedia

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

;; Defines a file-name-handler hook that transforms visited (or
;; inserted) image files so that they are displayed by Emacs as
;; images.  This is done by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;;; Code:

(require 'image)


;;;###autoload
(defcustom image-file-name-extensions
  (purecopy '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg"))
  "A list of image-file filename extensions.
Filenames having one of these extensions are considered image files,
in addition to those matching `image-file-name-regexps'.

See `auto-image-file-mode'; if `auto-image-file-mode' is enabled,
setting this variable directly does not take effect unless
`auto-image-file-mode' is re-enabled; this happens automatically when
the variable is set using \\[customize]."
  :type '(repeat string)
  :set (lambda (sym val)
	 (set-default sym val)
	 (when auto-image-file-mode
	   ;; Re-initialize the image-file handler
	   (auto-image-file-mode t)))
  :initialize 'custom-initialize-default
  :group 'image)

;;;###autoload
(defcustom image-file-name-regexps nil
  "List of regexps matching image-file filenames.
Filenames matching one of these regexps are considered image files,
in addition to those with an extension in `image-file-name-extensions'.

See function `auto-image-file-mode'; if `auto-image-file-mode' is
enabled, setting this variable directly does not take effect unless
`auto-image-file-mode' is re-enabled; this happens automatically when
the variable is set using \\[customize]."
  :type '(repeat regexp)
  :set (lambda (sym val)
	 (set-default sym val)
	 (when auto-image-file-mode
	   ;; Re-initialize the image-file handler
	   (auto-image-file-mode t)))
  :initialize 'custom-initialize-default
  :group 'image)


;;;###autoload
(defun image-file-name-regexp ()
  "Return a regular expression matching image-file filenames."
  (let ((exts-regexp
	 (and image-file-name-extensions
	      (concat "\\."
		      (regexp-opt (nconc (mapcar #'upcase
						 image-file-name-extensions)
					 image-file-name-extensions)
				  t)
		      "\\'"))))
    (if image-file-name-regexps
	(mapconcat 'identity
		   (if exts-regexp
		       (cons exts-regexp image-file-name-regexps)
		     image-file-name-regexps)
		   "\\|")
      exts-regexp)))


;;;###autoload
(defun insert-image-file (file &optional visit beg end replace)
  "Insert the image file FILE into the current buffer.
Optional arguments VISIT, BEG, END, and REPLACE are interpreted as for
the command `insert-file-contents'."
  (let ((rval
	 (image-file-call-underlying #'insert-file-contents-literally
				     'insert-file-contents
				     file visit beg end replace)))
    ;; Turn the image data into a real image, but only if the whole file
    ;; was inserted
    (when (and (or (null beg) (zerop beg)) (null end))
      (let* ((ibeg (point))
	     (iend (+ (point) (cadr rval)))
	     (visitingp (and visit (= ibeg (point-min)) (= iend (point-max))))
	     (data
	      (string-make-unibyte
	       (buffer-substring-no-properties ibeg iend)))
	     (image
	      (create-image data nil t))
	     (props
	      `(display ,image
			yank-handler
			(image-file-yank-handler nil t)
			intangible ,image
			rear-nonsticky (display intangible)
			;; This a cheap attempt to make the whole buffer
			;; read-only when we're visiting the file (as
			;; opposed to just inserting it).
			,@(and visitingp
			       '(read-only t front-sticky (read-only))))))
	(add-text-properties ibeg iend props)
	(when visitingp
	  ;; Inhibit the cursor when the buffer contains only an image,
	  ;; because cursors look very strange on top of images.
	  (setq cursor-type nil)
	  ;; This just makes the arrow displayed in the right fringe
	  ;; area look correct when the image is wider than the window.
	  (setq truncate-lines t))))
    rval))

;; We use a yank-handler to make yanked images unique, so that
;; yanking two copies of the same image next to each other are
;; recognized as two different images.
(defun image-file-yank-handler (string)
  "Yank handler for inserting an image into a buffer."
  (let ((len (length string))
	(image (get-text-property 0 'display string)))
    (remove-text-properties 0 len yank-excluded-properties string)
    (if (consp image)
	(add-text-properties 0
			     (or (next-single-property-change 0 'image-counter string)
				 (length string))
			     `(display
			       ,(cons (car image) (cdr image))
			       yank-handler
			       ,(cons 'image-file-yank-handler '(nil t)))
			     string))
    (insert string)))

(put 'image-file-handler 'safe-magic t)
(defun image-file-handler (operation &rest args)
  "Filename handler for inserting image files.
OPERATION is the operation to perform, on ARGS.
See `file-name-handler-alist' for details."
  (if (and (eq operation 'insert-file-contents)
	   auto-image-file-mode)
      (apply #'insert-image-file args)
    ;; We don't handle OPERATION, use another handler or the default
    (apply #'image-file-call-underlying operation operation args)))

(defun image-file-call-underlying (function operation &rest args)
  "Call FUNCTION with `image-file-handler' and OPERATION inhibited.
Optional argument ARGS are the arguments to call FUNCTION with."
  (let ((inhibit-file-name-handlers
	 (cons 'image-file-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply function args)))


;;;###autoload
(define-minor-mode auto-image-file-mode
  "Toggle visiting of image files as images (Auto Image File mode).
With a prefix argument ARG, enable Auto Image File mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

An image file is one whose name has an extension in
`image-file-name-extensions', or matches a regexp in
`image-file-name-regexps'."
  :global t
  :group 'image
  ;; Remove existing handler
  (let ((existing-entry
	 (rassq 'image-file-handler file-name-handler-alist)))
    (when existing-entry
      (setq file-name-handler-alist
	    (delq existing-entry file-name-handler-alist))))
  ;; Add new handler, if enabled
  (when auto-image-file-mode
    (push (cons (image-file-name-regexp) 'image-file-handler)
	  file-name-handler-alist)))


(provide 'image-file)

;;; image-file.el ends here
