;;; gnus-fun.el --- various frivolous extension functions to Gnus

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile
  (require 'cl))

(require 'mm-util)
(require 'gnus-ems)
(require 'gnus-util)
(require 'gnus)

(defvar gnus-face-properties-alist)

(defcustom gnus-x-face-directory (expand-file-name "x-faces" gnus-directory)
  "*Directory where X-Face PBM files are stored."
  :version "22.1"
  :group 'gnus-fun
  :type 'directory)

(defcustom gnus-convert-pbm-to-x-face-command "pbmtoxbm %s | compface"
  "Command for converting a PBM to an X-Face."
  :version "22.1"
  :group 'gnus-fun
  :type 'string)

(defcustom gnus-convert-image-to-x-face-command
  "convert -scale 48x48! %s xbm:- | xbm2xface.pl"
  "Command for converting an image to an X-Face.
The command must take a image filename (use \"%s\") as input.
The output must be the X-Face header data on stdout."
  :version "22.1"
  :group 'gnus-fun
  :type '(choice (const :tag "giftopnm, netpbm (GIF input only)"
			"giftopnm %s | ppmnorm | pnmscale -width 48 -height 48 | ppmtopgm | pgmtopbm | pbmtoxbm | compface")
		 (const :tag "convert"
			"convert -scale 48x48! %s xbm:- | xbm2xface.pl")
		 (string)))

(defcustom gnus-convert-image-to-face-command
  "convert -scale 48x48! %s -colors %d png:-"
  "Command for converting an image to a Face.

The command must take an image filename (first format argument
\"%s\") and the number of colors (second format argument: \"%d\")
as input.  The output must be the Face header data on stdout in
PNG format."
  :version "22.1"
  :group 'gnus-fun
  :type '(choice (const :tag "djpeg, netpbm (JPG input only)"
			"djpeg %s | ppmnorm | pnmscale -width 48 -height 48 | ppmquant %d | pnmtopng")
		 (const :tag "convert"
			"convert -scale 48x48! %s -colors %d png:-")
		 (string)))

(defun gnus-shell-command-to-string (command)
  "Like `shell-command-to-string' except not mingling ERROR."
  (with-output-to-string
    (call-process shell-file-name nil (list standard-output nil)
		  nil shell-command-switch command)))

(defun gnus-shell-command-on-region (start end command)
  "A simplified `shell-command-on-region'.
Output to the current buffer, replace text, and don't mingle error."
  (call-process-region start end shell-file-name t
		       (list (current-buffer) nil)
		       nil shell-command-switch command))

;;;###autoload
(defun gnus-random-x-face ()
  "Return X-Face header data chosen randomly from `gnus-x-face-directory'."
  (interactive)
  (when (file-exists-p gnus-x-face-directory)
    (let* ((files (directory-files gnus-x-face-directory t "\\.pbm$"))
	   (file (nth (random (length files)) files)))
      (when file
	(gnus-shell-command-to-string
	 (format gnus-convert-pbm-to-x-face-command
		 (shell-quote-argument file)))))))

(autoload 'message-goto-eoh "message" nil t)

;;;###autoload
(defun gnus-insert-random-x-face-header ()
  "Insert a random X-Face header from `gnus-x-face-directory'."
  (interactive)
  (let ((data (gnus-random-x-face)))
    (save-excursion
      (message-goto-eoh)
      (if data
	  (insert "X-Face: " data)
	(message
	 "No face returned by `gnus-random-x-face'.  Does %s/*.pbm exist?"
	 gnus-x-face-directory)))))

;;;###autoload
(defun gnus-x-face-from-file (file)
  "Insert an X-Face header based on an image file.

Depending on `gnus-convert-image-to-x-face-command' it may accept
different input formats."
  (interactive "fImage file name: ")
  (when (file-exists-p file)
    (gnus-shell-command-to-string
     (format gnus-convert-image-to-x-face-command
	     (shell-quote-argument (expand-file-name file))))))

;;;###autoload
(defun gnus-face-from-file (file)
  "Return a Face header based on an image file.

Depending on `gnus-convert-image-to-face-command' it may accept
different input formats."
  (interactive "fImage file name: ")
  (when (file-exists-p file)
    (let ((done nil)
	  (attempt "")
	  (quant 16))
      (while (and (not done)
		  (> quant 1))
	(setq attempt
	      (let ((coding-system-for-read 'binary))
		(gnus-shell-command-to-string
		 (format gnus-convert-image-to-face-command
			 (shell-quote-argument (expand-file-name file))
			 quant))))
	(if (> (length attempt) 726)
	    (progn
	      (setq quant (- quant (if (< quant 10) 1 2)))
	      (gnus-message 9 "Length %d; trying quant %d"
			    (length attempt) quant))
	  (setq done t)))
      (if done
	  (mm-with-unibyte-buffer
	    (insert attempt)
	    (gnus-face-encode))
	nil))))

(defun gnus-face-encode ()
  (let ((step 72))
    (base64-encode-region (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (> (- (point-max) (point))
	      step)
      (forward-char step)
      (insert "\n ")
      (setq step 76))
    (buffer-string)))

;;;###autoload
(defun gnus-convert-face-to-png (face)
  "Convert FACE (which is base64-encoded) to a PNG.
The PNG is returned as a string."
  (mm-with-unibyte-buffer
    (insert face)
    (ignore-errors
      (base64-decode-region (point-min) (point-max)))
    (buffer-string)))

;;;###autoload
(defun gnus-convert-png-to-face (file)
  "Convert FILE to a Face.
FILE should be a PNG file that's 48x48 and smaller than or equal to
726 bytes."
  (mm-with-unibyte-buffer
    (insert-file-contents file)
    (when (> (buffer-size) 726)
      (error "The file is %d bytes long, which is too long"
	     (buffer-size)))
    (gnus-face-encode)))

(defface gnus-x-face '((t (:foreground "black" :background "white")))
  "Face to show X-Face.
The colors from this face are used as the foreground and background
colors of the displayed X-Faces."
  :group 'gnus-article-headers)

(declare-function article-narrow-to-head   "gnus-art" ())
(declare-function gnus-article-goto-header "gnus-art" (header))
(declare-function gnus-add-image           "gnus-art" (category image))
(declare-function gnus-add-wash-type       "gnus-art" (type))

(defun gnus-display-x-face-in-from (data)
  "Display the X-Face DATA in the From header."
  (require 'gnus-art)
  (let (pbm)
    (when (or (gnus-image-type-available-p 'xface)
	      (and (gnus-image-type-available-p 'pbm)
		   (setq pbm (uncompface data))))
      (save-excursion
	(save-restriction
	  (article-narrow-to-head)
	  (gnus-article-goto-header "from")
	  (when (bobp)
	    (insert "From: [no `from' set]\n")
	    (forward-char -17))
	  (gnus-add-image
	   'xface
	   (gnus-put-image
	    (if (gnus-image-type-available-p 'xface)
		(apply 'gnus-create-image (concat "X-Face: " data) 'xface t
		       (cdr (assq 'xface gnus-face-properties-alist)))
	      (apply 'gnus-create-image pbm 'pbm t
		     (cdr (assq 'pbm gnus-face-properties-alist))))
	    nil 'xface))
	  (gnus-add-wash-type 'xface))))))

(defun gnus-grab-cam-x-face ()
  "Grab a picture off the camera and make it into an X-Face."
  (interactive)
  (shell-command "xawtv-remote snap ppm")
  (let ((file nil))
    (while (null (setq file (directory-files "/tftpboot/sparky/tmp"
					     t "snap.*ppm")))
      (sleep-for 1))
    (setq file (car file))
    (with-temp-buffer
      (shell-command
       (format "pnmcut -left 110 -top 30 -width 144 -height 144 '%s' | ppmnorm 2>/dev/null | pnmscale -width 48 | ppmtopgm | pgmtopbm -threshold -value 0.92 | pbmtoxbm | compface"
	       file)
       (current-buffer))
      ;;(sleep-for 3)
      (delete-file file)
      (buffer-string))))

(defun gnus-grab-cam-face ()
  "Grab a picture off the camera and make it into an X-Face."
  (interactive)
  (shell-command "xawtv-remote snap ppm")
  (let ((file nil)
	result)
    (while (null (setq file (directory-files "/tftpboot/sparky/tmp"
					     t "snap.*ppm")))
      (sleep-for 1))
    (setq file (car file))
    (shell-command
     (format "pnmcut -left 110 -top 30 -width 144 -height 144 '%s' | pnmscale -width 48 -height 48 | ppmtopgm > /tmp/gnus.face.ppm"
	     file))
    (let ((gnus-convert-image-to-face-command
	   (format "cat '%%s' | ppmquant %%d | ppmchange %s | pnmtopng"
		   (gnus-fun-ppm-change-string))))
      (setq result (gnus-face-from-file "/tmp/gnus.face.ppm")))
    (delete-file file)
    ;;(delete-file "/tmp/gnus.face.ppm")
    result))

(defun gnus-fun-ppm-change-string ()
  (let* ((possibilities '("%02x0000" "00%02x00" "0000%02x"
			  "%02x%02x00" "00%02x%02x" "%02x00%02x"))
	 (format (concat "'#%02x%02x%02x' '#"
			 (nth (random 6) possibilities)
			 "'"))
	 (values nil))
  (dotimes (i 255)
    (push (format format i i i i i i)
	  values))
  (mapconcat 'identity values " ")))

(provide 'gnus-fun)

;;; gnus-fun.el ends here
