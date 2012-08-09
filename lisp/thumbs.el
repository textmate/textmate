;;; thumbs.el --- Thumbnails previewer for images files

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Jean-Philippe Theberge <jphiltheberge@videotron.ca>
;; Maintainer: FSF
;; Keywords: Multimedia

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

;; This package create two new modes: thumbs-mode and thumbs-view-image-mode.
;; It is used for basic browsing and viewing of images from within Emacs.
;; Minimal image manipulation functions are also available via external
;; programs.  If you want to do more complex tasks like categorize and tag
;; your images, use image-dired.el
;;
;; The 'convert' program from 'ImageMagick'
;; [URL:http://www.imagemagick.org/] is required.
;;
;; Thanks: Alex Schroeder <alex@gnu.org> for maintaining the package at some
;;         time.  The peoples at #emacs@freenode.net for numerous help.  RMS
;;         for emacs and the GNU project.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CHANGELOG
;;
;; This is version 2.0
;;
;; USAGE
;;
;; Type M-x thumbs RET DIR RET to view the directory DIR in Thumbs mode.
;; That should be a directory containing image files.
;; from dired, C-t m enter in thumbs-mode with all marked files
;;             C-t a enter in thumbs-mode with all files in current-directory
;; In thumbs-mode, pressing <return> on a image will bring you in image view
;; mode for that image.  C-h m will give you a list of available keybinding.

;;; History:
;;

;;; Code:

(require 'dired)

;; CUSTOMIZATIONS

(defgroup thumbs nil
  "Thumbnails previewer."
  :version "22.1"
  :group 'multimedia)

(defcustom thumbs-thumbsdir (locate-user-emacs-file "thumbs")
  "Directory to store thumbnails."
  :type 'directory
  :group 'thumbs)

(defcustom thumbs-geometry "100x100"
  "Size of thumbnails."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-per-line 4
  "Number of thumbnails per line to show in directory."
  :type 'integer
  :group 'thumbs)

(defcustom thumbs-max-image-number 16
 "Maximum number of images initially displayed in thumbs buffer."
  :type 'integer
  :group 'thumbs)

(defcustom thumbs-thumbsdir-max-size 50000000
  "Maximum size for thumbnails directory.
When it reaches that size (in bytes), a warning is sent."
  :type 'integer
  :group 'thumbs)

;; Unfortunately Windows XP has a program called CONVERT.EXE in
;; C:/WINDOWS/SYSTEM32/ for partitioning NTFS systems.  So Emacs
;; can find the one in your ImageMagick directory, you need to
;; customize this value to the absolute filename.
(defcustom thumbs-conversion-program
  (if (eq system-type 'windows-nt)
      "convert.exe"
    (or (executable-find "convert")
	"/usr/X11R6/bin/convert"))
  "Name of conversion program for thumbnails generation.
It must be 'convert'."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-setroot-command
  "xloadimage -onroot -fullscreen *"
  "Command to set the root window."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-relief 5
  "Size of button-like border around thumbnails."
  :type 'integer
  :group 'thumbs)

(defcustom thumbs-margin 2
  "Size of the margin around thumbnails.
This is where you see the cursor."
  :type 'integer
  :group 'thumbs)

(defcustom thumbs-thumbsdir-auto-clean t
  "If set, delete older file in the thumbnails directory.
Deletion is done at load time when the directory size is bigger
than `thumbs-thumbsdir-max-size'."
  :type 'boolean
  :group 'thumbs)

(defcustom thumbs-image-resizing-step 10
  "Step by which to resize image as a percentage."
  :type 'integer
  :group 'thumbs)

(defcustom thumbs-temp-dir temporary-file-directory
  "Temporary directory to use.
Defaults to `temporary-file-directory'.  Leaving it to
this value can let another user see some of your images."
  :type 'directory
  :group 'thumbs)

(defcustom thumbs-temp-prefix "emacsthumbs"
  "Prefix to add to temp files."
  :type 'string
  :group 'thumbs)

;; Initialize some variable, for later use.
(defvar thumbs-current-tmp-filename nil
  "Temporary filename of current image.")
(make-variable-buffer-local 'thumbs-current-tmp-filename)

(defvar thumbs-current-image-filename nil
  "Filename of current image.")
(make-variable-buffer-local 'thumbs-current-image-filename)

(defvar thumbs-extra-images 1
  "Counter for showing extra images in thumbs buffer.")
(make-variable-buffer-local 'thumbs-extra-images)
(put 'thumbs-extra-images 'permanent-local t)

(defvar thumbs-current-image-size nil
  "Size of current image.")

(defvar thumbs-image-num nil
  "Number of current image.")
(make-variable-buffer-local 'thumbs-image-num)

(defvar thumbs-buffer nil
  "Name of buffer containing thumbnails associated with image.")
(make-variable-buffer-local 'thumbs-buffer)

(defvar thumbs-current-dir nil
  "Current directory.")

(defvar thumbs-marked-list nil
  "List of marked files.")
(make-variable-buffer-local 'thumbs-marked-list)
(put 'thumbs-marked-list 'permanent-local t)

(defalias 'thumbs-gensym
    (if (fboundp 'gensym)
        'gensym
      ;; Copied from cl-macs.el
      (defvar thumbs-gensym-counter 0)
      (lambda (&optional prefix)
	"Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
	(let ((pfix (if (stringp prefix) prefix "G"))
	      (num (if (integerp prefix) prefix
		     (prog1 thumbs-gensym-counter
		       (setq thumbs-gensym-counter
			     (1+ thumbs-gensym-counter))))))
	  (make-symbol (format "%s%d" pfix num))))))

(defsubst thumbs-temp-dir ()
  (file-name-as-directory (expand-file-name thumbs-temp-dir)))

(defun thumbs-temp-file ()
  "Return a unique temporary filename for an image."
  (format "%s%s-%s.jpg"
          (thumbs-temp-dir)
          thumbs-temp-prefix
          (thumbs-gensym "T")))

(defun thumbs-thumbsdir ()
  "Return the current thumbnails directory (from `thumbs-thumbsdir').
Create the thumbnails directory if it does not exist."
  (let ((thumbs-thumbsdir (file-name-as-directory
                           (expand-file-name thumbs-thumbsdir))))
    (unless (file-directory-p thumbs-thumbsdir)
      (make-directory thumbs-thumbsdir t)
      (message "Creating thumbnails directory"))
    thumbs-thumbsdir))

(defun thumbs-cleanup-thumbsdir ()
  "Clean the thumbnails directory.
If the total size of all files in `thumbs-thumbsdir' is bigger than
`thumbs-thumbsdir-max-size', files are deleted until the max size is
reached."
  (let* ((files-list
	  (sort
	   (mapcar
	    (lambda (f)
	      (let ((fattribs-list (file-attributes f)))
		`(,(nth 4 fattribs-list) ,(nth 7 fattribs-list) ,f)))
	    (directory-files (thumbs-thumbsdir) t (image-file-name-regexp)))
	   (lambda (l1 l2) (time-less-p (car l1) (car l2)))))
	 (dirsize (apply '+ (mapcar (lambda (x) (cadr x)) files-list))))
    (while (> dirsize thumbs-thumbsdir-max-size)
      (progn
	(message "Deleting file %s" (cadr (cdar files-list))))
      (delete-file (cadr (cdar files-list)))
      (setq dirsize (- dirsize (car (cdar files-list))))
      (setq files-list (cdr files-list)))))

;; Check the thumbnail directory size and clean it if necessary.
(when thumbs-thumbsdir-auto-clean
  (thumbs-cleanup-thumbsdir))

(defun thumbs-call-convert (filein fileout action
				   &optional arg output-format action-prefix)
  "Call the convert program.
FILEIN is the input file,
FILEOUT is the output file,
ACTION is the command to send to convert.
Optional arguments are:
ARG any arguments to the ACTION command,
OUTPUT-FORMAT is the file format to output (default is jpeg),
ACTION-PREFIX is the symbol to place before the ACTION command
              (defaults to '-' but can sometimes be '+')."
  (call-process thumbs-conversion-program nil nil nil
		(or action-prefix "-")
		action
		(or arg "")
		filein
		(format "%s:%s"	(or output-format "jpeg") fileout)))

(defun thumbs-new-image-size (s increment)
  "New image (a cons of width x height)."
  (let ((d (* increment thumbs-image-resizing-step)))
    (cons
     (round (+ (car s) (/ (* d (car s)) 100)))
     (round (+ (cdr s) (/ (* d (cdr s)) 100))))))

(defun thumbs-resize-image-1 (&optional increment size)
  "Resize image in current buffer.
If SIZE is specified use it.  Otherwise make the image larger or
smaller according to whether INCREMENT is 1 or -1."
  (let* ((buffer-read-only nil)
	 (old thumbs-current-tmp-filename)
	 (x (or size
		(thumbs-new-image-size thumbs-current-image-size increment)))
	 (tmp (thumbs-temp-file)))
    (erase-buffer)
    (thumbs-call-convert (or old thumbs-current-image-filename)
			 tmp "sample"
			 (concat (number-to-string (car x)) "x"
				 (number-to-string (cdr x))))
    (save-excursion
      (thumbs-insert-image tmp 'jpeg 0))
    (setq thumbs-current-tmp-filename tmp)))

(defun thumbs-resize-image (width height)
  "Resize image interactively to specified WIDTH and HEIGHT."
  (interactive "nWidth: \nnHeight: ")
  (thumbs-resize-image-1 nil (cons width height)))

(defun thumbs-shrink-image ()
  "Resize image (smaller)."
  (interactive)
  (thumbs-resize-image-1 -1))

(defun thumbs-enlarge-image ()
  "Resize image (bigger)."
  (interactive)
  (thumbs-resize-image-1 1))

(defun thumbs-thumbname (img)
  "Return a thumbnail name for the image IMG."
  (convert-standard-filename
   (let ((filename (expand-file-name img)))
     (format "%s%08x-%s.jpg"
             (thumbs-thumbsdir)
             (sxhash filename)
             (subst-char-in-string
              ?\s ?\_
              (apply
               'concat
               (split-string filename "/")))))))

(defun thumbs-make-thumb (img)
  "Create the thumbnail for IMG."
  (let ((fn (expand-file-name img))
        (tn (thumbs-thumbname img)))
    (if (or (not (file-exists-p tn))
	    ;;  This is not the right fix, but I don't understand
	    ;;  the external program or why it produces a geometry
	    ;;  unequal to the one requested -- rms.
;;;	    (not (equal (thumbs-file-size tn) thumbs-geometry))
	    )
	(thumbs-call-convert fn tn "sample" thumbs-geometry))
    tn))

(defun thumbs-image-type (img)
  "Return image type from filename IMG."
  (cond ((string-match ".*\\.jpe?g\\'" img) 'jpeg)
	((string-match ".*\\.xpm\\'" img) 'xpm)
	((string-match ".*\\.xbm\\'" img) 'xbm)
	((string-match ".*\\.pbm\\'" img) 'pbm)
	((string-match ".*\\.gif\\'" img) 'gif)
	((string-match ".*\\.bmp\\'" img) 'bmp)
	((string-match ".*\\.png\\'" img) 'png)
	((string-match ".*\\.tiff?\\'" img) 'tiff)))

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun thumbs-file-size (img)
  (let ((i (image-size
	    (find-image `((:type ,(thumbs-image-type img) :file ,img))) t)))
    (concat (number-to-string (round (car i))) "x"
	    (number-to-string (round (cdr i))))))

;;;###autoload
(defun thumbs-find-thumb (img)
  "Display the thumbnail for IMG."
  (interactive "f")
  (find-file (thumbs-make-thumb img)))

(defun thumbs-insert-image (img type relief &optional marked)
  "Insert image IMG at point.
TYPE and RELIEF will be used in constructing the image; see `image'
in the emacs-lisp manual for further documentation.
If MARKED is non-nil, the image is marked."
  (let ((i `(image :type ,type
		   :file ,img
		   :relief ,relief
		   :conversion ,(if marked 'disabled)
		   :margin ,thumbs-margin)))
    (insert-image i)
    (set (make-local-variable 'thumbs-current-image-size)
         (image-size i t))))

(defun thumbs-insert-thumb (img &optional marked)
  "Insert the thumbnail for IMG at point.
If MARKED is non-nil, the image is marked."
  (thumbs-insert-image
   (thumbs-make-thumb img) 'jpeg thumbs-relief marked)
  (add-text-properties (1- (point)) (point)
		     `(thumb-image-file ,img
		       help-echo ,(file-name-nondirectory img)
		       rear-nonsticky help-echo)))

(defun thumbs-do-thumbs-insertion (list)
  "Insert all thumbnails into thumbs buffer."
  (let* ((i 0)
	(length (length list))
	(diff (- length (* thumbs-max-image-number thumbs-extra-images))))
    (nbutlast list diff)
    (dolist (img list)
      (thumbs-insert-thumb img
			   (member img thumbs-marked-list))
      (when (= 0 (mod (setq i (1+ i)) thumbs-per-line))
	(newline)))
    (unless (bobp) (newline))
    (if (> diff 0) (message "Type + to display more images."))))

(defun thumbs-show-thumbs-list (list &optional dir same-window)
  (unless (and (display-images-p)
               (image-type-available-p 'jpeg))
    (error "Required image type is not supported in this Emacs session"))
  (funcall (if same-window 'switch-to-buffer 'pop-to-buffer)
	   (if dir (concat "*Thumbs: " dir) "*THUMB-View*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (thumbs-mode)
    (setq thumbs-buffer (current-buffer))
    (if dir (setq default-directory dir))
    (thumbs-do-thumbs-insertion list)
    (goto-char (point-min))
    (set (make-local-variable 'thumbs-current-dir) default-directory)))

;;;###autoload
(defun thumbs-show-from-dir (dir &optional reg same-window)
  "Make a preview buffer for all images in DIR.
Optional argument REG to select file matching a regexp,
and SAME-WINDOW to show thumbs in the same window."
  (interactive "DDir: ")
  (thumbs-show-thumbs-list
   (directory-files dir t (or reg (image-file-name-regexp)))
   dir same-window))

;;;###autoload
(defun thumbs-dired-show-marked ()
  "In dired, make a thumbs buffer with marked files."
  (interactive)
  (thumbs-show-thumbs-list (dired-get-marked-files) nil t))

;;;###autoload
(defun thumbs-dired-show ()
  "In dired, make a thumbs buffer with all files in current directory."
  (interactive)
  (thumbs-show-from-dir default-directory nil t))

;;;###autoload
(defalias 'thumbs 'thumbs-show-from-dir)

(defun thumbs-find-image (img &optional num otherwin)
  (let ((buffer (current-buffer)))
    (funcall
     (if otherwin 'switch-to-buffer-other-window 'switch-to-buffer)
     "*Image*")
    (thumbs-view-image-mode)
    (setq mode-name
	  (concat "image-view-mode: " (file-name-nondirectory img)
		  " - " (number-to-string num)))
    (setq thumbs-buffer buffer)
    (let ((inhibit-read-only t))
      (setq thumbs-current-image-filename img
	    thumbs-current-tmp-filename nil
	    thumbs-image-num (or num 0))
      (delete-region (point-min)(point-max))
      (save-excursion
	(thumbs-insert-image img (thumbs-image-type img) 0)))))

(defun thumbs-find-image-at-point (&optional img otherwin)
  "Display image IMG for thumbnail at point.
Use another window if OTHERWIN is t."
  (interactive)
  (let* ((i (or img (thumbs-current-image))))
    (thumbs-find-image i (point) otherwin)))

(defun thumbs-find-image-at-point-other-window ()
  "Display image for thumbnail at point in the preview buffer.
Open another window."
  (interactive)
  (thumbs-find-image-at-point nil t))

(defun thumbs-mouse-find-image (event)
  "Display image for thumbnail at mouse click EVENT."
  (interactive "e")
  (mouse-set-point event)
  (thumbs-find-image-at-point))

(defun thumbs-call-setroot-command (img)
  "Call the setroot program for IMG."
  (run-hooks 'thumbs-before-setroot-hook)
  (shell-command (replace-regexp-in-string
		  "\\*"
		  (shell-quote-argument (expand-file-name img))
		  thumbs-setroot-command nil t))
  (run-hooks 'thumbs-after-setroot-hook))

(defun thumbs-set-image-at-point-to-root-window ()
  "Set the image at point as the desktop wallpaper."
  (interactive)
  (thumbs-call-setroot-command
   (thumbs-current-image)))

(defun thumbs-set-root ()
  "Set the current image as root."
  (interactive)
  (thumbs-call-setroot-command
   (or thumbs-current-tmp-filename
       thumbs-current-image-filename)))

(defun thumbs-file-alist ()
  "Make an alist of elements (POS . FILENAME) for all images in thumb buffer."
  (with-current-buffer thumbs-buffer
    (save-excursion
      (let (list)
	(goto-char (point-min))
	(while (not (eobp))
	  (unless (eolp)
	    (if (thumbs-current-image)
		(push (cons (point-marker)
			    (thumbs-current-image))
		    list)))
	  (forward-char 1))
	(nreverse list)))))

(defun thumbs-file-list ()
  "Make a list of file names for all images in thumb buffer."
  (save-excursion
    (let (list)
      (goto-char (point-min))
      (while (not (eobp))
	(if (thumbs-current-image)
	    (push (thumbs-current-image) list))
	(forward-char 1))
      (nreverse list))))

(defun thumbs-delete-images ()
  "Delete the image at point (and its thumbnail) (or marked files if any)."
  (interactive)
  (let ((files (or thumbs-marked-list (list (thumbs-current-image)))))
    (if (yes-or-no-p (format "Really delete %d files? " (length files)))
	(let ((thumbs-file-list (thumbs-file-alist))
	      (inhibit-read-only t))
	  (dolist (x files)
	    (let (failure)
	      (condition-case ()
		  (progn
		    (delete-file x)
		    (delete-file (thumbs-thumbname x)))
		(file-error (setq failure t)))
	      (unless failure
		(when (rassoc x thumbs-file-list)
		  (goto-char (car (rassoc x thumbs-file-list)))
		  (delete-region (point) (1+ (point))))
		(setq thumbs-marked-list
		      (delq x thumbs-marked-list)))))))))

(defun thumbs-rename-images (newfile)
  "Rename the image at point (and its thumbnail) (or marked files if any)."
  (interactive "FRename to file or directory: ")
  (let ((files (or thumbs-marked-list (list (thumbs-current-image))))
	failures)
    (if (and (not (file-directory-p newfile))
	     thumbs-marked-list)
	(if (file-exists-p newfile)
	    (error "Renaming marked files to file name `%s'" newfile)
	  (make-directory newfile t)))
    (if (yes-or-no-p (format "Really rename %d files? " (length files)))
	(let ((thumbs-file-list (thumbs-file-alist))
	      (inhibit-read-only t))
	  (dolist (file files)
	    (let (failure)
	      (condition-case ()
		  (if (file-directory-p newfile)
		      (rename-file file
				   (expand-file-name
				    (file-name-nondirectory file)
				    newfile))
		    (rename-file file newfile))
		(file-error (setq failure t)
			    (push file failures)))
	      (unless failure
		(when (rassoc file thumbs-file-list)
		  (goto-char (car (rassoc file thumbs-file-list)))
		  (delete-region (point) (1+ (point))))
		(setq thumbs-marked-list
		      (delq file thumbs-marked-list)))))))
    (if failures
	(display-warning 'file-error
			 (format "Rename failures for %s into %s"
				 failures newfile)
			 :error))))

(defun thumbs-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (quit-window t (selected-window)))

(defun thumbs-show-image-num (num)
  "Show the image with number NUM."
  (let ((image-buffer (get-buffer-create "*Image*")))
    (let ((img (cdr (nth (1- num) (thumbs-file-alist)))))
      (with-current-buffer image-buffer
	(setq mode-name
	      (concat "image-view-mode: " (file-name-nondirectory img)
		      " - " (number-to-string num)))
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (thumbs-insert-image img (thumbs-image-type img) 0)
	  (goto-char (point-min))))
      (setq thumbs-image-num num
	    thumbs-current-image-filename img))))

(defun thumbs-previous-image ()
  "Show the previous image."
  (interactive)
  (let* ((i (- thumbs-image-num 1))
	 (number (length (thumbs-file-alist))))
    (if (= i 0) (setq i (1- number)))
    (thumbs-show-image-num i)))

(defun thumbs-next-image ()
  "Show the next image."
  (interactive)
  (let* ((i (1+ thumbs-image-num))
	 (number (length (thumbs-file-alist))))
    (if (= i number) (setq i 1))
    (thumbs-show-image-num i)))

(defun thumbs-display-thumbs-buffer ()
  "Display the associated thumbs buffer."
  (interactive)
  (display-buffer thumbs-buffer))

(defun thumbs-redraw-buffer ()
  "Redraw the current thumbs buffer."
  (let ((p (point))
	(inhibit-read-only t)
	(files (thumbs-file-list)))
    (erase-buffer)
    (thumbs-do-thumbs-insertion files)
    (goto-char p)))

(defun thumbs-mark ()
  "Mark the image at point."
  (interactive)
  (let ((elt (thumbs-current-image)))
    (unless elt
      (error "No image here"))
    (push elt thumbs-marked-list)
    (let ((inhibit-read-only t))
      (delete-char 1)
      (thumbs-insert-thumb elt t)))
  (when (eolp) (forward-char)))

(defun thumbs-unmark ()
  "Unmark the image at point."
  (interactive)
  (let ((elt (thumbs-current-image)))
    (unless elt
      (error "No image here"))
    (setq thumbs-marked-list (delete elt thumbs-marked-list))
    (let ((inhibit-read-only t))
      (delete-char 1)
      (thumbs-insert-thumb elt nil)))
  (when (eolp) (forward-char)))

;; cleaning of old temp files
(mapc 'delete-file
      (directory-files (thumbs-temp-dir) t thumbs-temp-prefix))

;; Image modification routines

(defun thumbs-modify-image (action &optional arg)
  "Call convert to do ACTION on image with argument ARG.
ACTION and ARG should be a valid convert command."
  (interactive "sAction: \nsValue: ")
  (let* ((buffer-read-only nil)
	 (old thumbs-current-tmp-filename)
	 (tmp (thumbs-temp-file)))
    (erase-buffer)
    (thumbs-call-convert (or old thumbs-current-image-filename)
			 tmp
			 action
			 (or arg ""))
    (save-excursion
      (thumbs-insert-image tmp 'jpeg 0))
    (setq thumbs-current-tmp-filename tmp)))

(defun thumbs-emboss-image (emboss)
  "Emboss the image with value EMBOSS."
  (interactive "nEmboss value: ")
  (if (or (< emboss 3) (> emboss 31) (zerop (% emboss 2)))
      (error "Arg must be an odd number between 3 and 31"))
  (thumbs-modify-image "emboss" (number-to-string emboss)))

(defun thumbs-monochrome-image ()
  "Turn the image to monochrome."
  (interactive)
  (thumbs-modify-image "monochrome"))

(defun thumbs-negate-image ()
  "Negate the image."
  (interactive)
  (thumbs-modify-image "negate"))

(defun thumbs-rotate-left ()
  "Rotate the image 90 degrees counter-clockwise."
  (interactive)
  (thumbs-modify-image "rotate" "270"))

(defun thumbs-rotate-right ()
  "Rotate the image 90 degrees clockwise."
  (interactive)
  (thumbs-modify-image "rotate" "90"))

(defun thumbs-current-image ()
  "Return the name of the image file name at point."
  (get-text-property (point) 'thumb-image-file))

(defun thumbs-forward-char ()
  "Move forward one image."
  (interactive)
  (forward-char)
  (while (and (not (eobp)) (not (thumbs-current-image)))
    (forward-char))
  (thumbs-show-name))

(defun thumbs-backward-char ()
  "Move backward one image."
  (interactive)
  (forward-char -1)
  (while (and (not (bobp)) (not (thumbs-current-image)))
    (forward-char -1))
  (thumbs-show-name))

(defun thumbs-backward-line ()
  "Move up one line."
  (interactive)
  (forward-line -1)
  (thumbs-show-name))

(defun thumbs-forward-line ()
  "Move down one line."
  (interactive)
  (forward-line 1)
  (thumbs-show-name))

(defun thumbs-show-more-images (&optional arg)
  "Show more than `thumbs-max-image-number' images, if present."
  (interactive "P")
  (or arg (setq arg 1))
  (setq thumbs-extra-images (+ thumbs-extra-images arg))
  (thumbs-dired-show))

(defun thumbs-show-name ()
  "Show the name of the current file."
  (interactive)
  (let ((f (thumbs-current-image)))
    (and f (message "%s [%s]" f (thumbs-file-size f)))))

(defun thumbs-save-current-image ()
  "Save the current image."
  (interactive)
  (let ((f (or thumbs-current-tmp-filename
	       thumbs-current-image-filename))
	(sa (read-from-minibuffer "Save image file as: "
				  thumbs-current-image-filename)))
    (copy-file f sa)))

(defun thumbs-dired ()
  "Use `dired' on the current thumbs directory."
  (interactive)
  (dired thumbs-current-dir))

;; thumbs-mode

(defvar thumbs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'thumbs-find-image-at-point)
    (define-key map [mouse-2] 'thumbs-mouse-find-image)
    (define-key map [(meta return)] 'thumbs-find-image-at-point-other-window)
    (define-key map [(control return)] 'thumbs-set-image-at-point-to-root-window)
    (define-key map [delete] 'thumbs-delete-images)
    (define-key map [right] 'thumbs-forward-char)
    (define-key map [left] 'thumbs-backward-char)
    (define-key map [up] 'thumbs-backward-line)
    (define-key map [down] 'thumbs-forward-line)
    (define-key map "+" 'thumbs-show-more-images)
    (define-key map "d" 'thumbs-dired)
    (define-key map "m" 'thumbs-mark)
    (define-key map "u" 'thumbs-unmark)
    (define-key map "R" 'thumbs-rename-images)
    (define-key map "x" 'thumbs-delete-images)
    (define-key map "s" 'thumbs-show-name)
    (define-key map "q" 'thumbs-kill-buffer)
    map)
  "Keymap for `thumbs-mode'.")

(put 'thumbs-mode 'mode-class 'special)
(define-derived-mode thumbs-mode
  fundamental-mode "thumbs"
  "Preview images in a thumbnails buffer"
  (setq buffer-read-only t))

(defvar thumbs-view-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [prior] 'thumbs-previous-image)
    (define-key map [next] 'thumbs-next-image)
    (define-key map "^" 'thumbs-display-thumbs-buffer)
    (define-key map "-" 'thumbs-shrink-image)
    (define-key map "+" 'thumbs-enlarge-image)
    (define-key map "<" 'thumbs-rotate-left)
    (define-key map ">" 'thumbs-rotate-right)
    (define-key map "e" 'thumbs-emboss-image)
    (define-key map "r" 'thumbs-resize-image)
    (define-key map "s" 'thumbs-save-current-image)
    (define-key map "q" 'thumbs-kill-buffer)
    (define-key map "w" 'thumbs-set-root)
    map)
  "Keymap for `thumbs-view-image-mode'.")

;; thumbs-view-image-mode
(put 'thumbs-view-image-mode 'mode-class 'special)
(define-derived-mode thumbs-view-image-mode
  fundamental-mode "image-view-mode"
  (setq buffer-read-only t))

;;;###autoload
(defun thumbs-dired-setroot ()
  "In dired, call the setroot program on the image at point."
  (interactive)
  (thumbs-call-setroot-command (dired-get-filename)))

;; Modif to dired mode map
(define-key dired-mode-map "\C-ta" 'thumbs-dired-show)
(define-key dired-mode-map "\C-tm" 'thumbs-dired-show-marked)
(define-key dired-mode-map "\C-tw" 'thumbs-dired-setroot)

(provide 'thumbs)

;;; thumbs.el ends here
