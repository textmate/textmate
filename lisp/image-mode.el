;;; image-mode.el --- support for visiting image files
;;
;; Copyright (C) 2005-2012 Free Software Foundation, Inc.
;;
;; Author: Richard Stallman <rms@gnu.org>
;; Keywords: multimedia
;; Package: emacs

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

;; Defines a major mode for visiting image files
;; that allows conversion between viewing the text of the file
;; and viewing the file as an image.  Viewing the image
;; works by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;;; Code:

(require 'image)
(eval-when-compile (require 'cl))

;;; Image mode window-info management.

(defvar image-mode-winprops-alist t)
(make-variable-buffer-local 'image-mode-winprops-alist)

(defvar image-mode-new-window-functions nil
  "Special hook run when image data is requested in a new window.
It is called with one argument, the initial WINPROPS.")

(defun image-mode-winprops (&optional window cleanup)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST)."
  (cond ((null window)
	 (setq window (selected-window)))
	((not (windowp window))
	 (error "Not a window: %s" window)))
  (when cleanup
    (setq image-mode-winprops-alist
  	  (delq nil (mapcar (lambda (winprop)
  			      (if (window-live-p (car-safe winprop))
  				  winprop))
  			    image-mode-winprops-alist))))
  (let ((winprops (assq window image-mode-winprops-alist)))
    ;; For new windows, set defaults from the latest.
    (unless winprops
      (setq winprops (cons window
                           (copy-alist (cdar image-mode-winprops-alist))))
      (run-hook-with-args 'image-mode-new-window-functions winprops))
    ;; Move window to front.
    (setq image-mode-winprops-alist
          (cons winprops (delq winprops image-mode-winprops-alist)))
    winprops))

(defun image-mode-window-get (prop &optional winprops)
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (cdr (assq prop (cdr winprops))))

(defsetf image-mode-window-get (prop &optional winprops) (val)
  `(image-mode-window-put ,prop ,val ,winprops))

(defun image-mode-window-put (prop val &optional winprops)
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (setcdr winprops (cons (cons prop val)
                         (delq (assq prop (cdr winprops)) (cdr winprops)))))

(defun image-set-window-vscroll (vscroll)
  (setf (image-mode-window-get 'vscroll) vscroll)
  (set-window-vscroll (selected-window) vscroll))

(defun image-set-window-hscroll (ncol)
  (setf (image-mode-window-get 'hscroll) ncol)
  (set-window-hscroll (selected-window) ncol))

(defun image-mode-reapply-winprops ()
  ;; When set-window-buffer, set hscroll and vscroll to what they were
  ;; last time the image was displayed in this window.
  (when (and (image-get-display-property)
	     (listp image-mode-winprops-alist))
    (let* ((winprops (image-mode-winprops nil t))
           (hscroll (image-mode-window-get 'hscroll winprops))
           (vscroll (image-mode-window-get 'vscroll winprops)))
      (if hscroll (set-window-hscroll (selected-window) hscroll))
      (if vscroll (set-window-vscroll (selected-window) vscroll)))))

(defun image-mode-setup-winprops ()
  ;; Record current scroll settings.
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil))
  (add-hook 'window-configuration-change-hook
 	    'image-mode-reapply-winprops nil t))

;;; Image scrolling functions

(defun image-get-display-property ()
  (get-char-property (point-min) 'display
                     ;; There might be different images for different displays.
                     (if (eq (window-buffer) (current-buffer))
                         (selected-window))))

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun image-display-size (spec &optional pixels frame)
  "Wrapper around `image-size', handling slice display properties.
Like `image-size', the return value is (WIDTH . HEIGHT).
WIDTH and HEIGHT are in canonical character units if PIXELS is
nil, and in pixel units if PIXELS is non-nil.

If SPEC is an image display property, this function is equivalent
to `image-size'.  If SPEC is a list of properties containing
`image' and `slice' properties, return the display size taking
the slice property into account.  If the list contains `image'
but not `slice', return the `image-size' of the specified image."
  (if (eq (car spec) 'image)
      (image-size spec pixels frame)
    (let ((image (assoc 'image spec))
	  (slice (assoc 'slice spec)))
      (cond ((and image slice)
	     (if pixels
		 (cons (nth 3 slice) (nth 4 slice))
	       (cons (/ (float (nth 3 slice)) (frame-char-width frame))
		     (/ (float (nth 4 slice)) (frame-char-height frame)))))
	    (image
	     (image-size image pixels frame))
	    (t
	     (error "Invalid image specification: %s" spec))))))

(defun image-forward-hscroll (&optional n)
  "Scroll image in current window to the left by N character widths.
Stop if the right edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-set-window-hscroll (max 0 (+ (window-hscroll) n))))
	(t
	 (let* ((image (image-get-display-property))
		(edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges)))
		(img-width (ceiling (car (image-display-size image)))))
	   (image-set-window-hscroll (min (max 0 (- img-width win-width))
					  (+ n (window-hscroll))))))))

(defun image-backward-hscroll (&optional n)
  "Scroll image in current window to the right by N character widths.
Stop if the left edge of the image is reached."
  (interactive "p")
  (image-forward-hscroll (- n)))

(defun image-next-line (n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-set-window-vscroll (max 0 (+ (window-vscroll) n))))
	(t
	 (let* ((image (image-get-display-property))
		(edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges)))
		(img-height (ceiling (cdr (image-display-size image)))))
	   (image-set-window-vscroll (min (max 0 (- img-height win-height))
					  (+ n (window-vscroll))))))))

(defun image-previous-line (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached."
  (interactive "p")
  (image-next-line (- n)))

(defun image-scroll-up (&optional n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached.
If ARG is omitted or nil, scroll upward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (max 0 (- win-height next-screen-context-lines)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (min 0 (- next-screen-context-lines win-height)))))
	(t (image-next-line (prefix-numeric-value n)))))

(defun image-scroll-down (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached.
If ARG is omitted or nil, scroll downward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (min 0 (- next-screen-context-lines win-height)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (max 0 (- win-height next-screen-context-lines)))))
	(t (image-next-line (- (prefix-numeric-value n))))))

(defun image-bol (arg)
  "Scroll horizontally to the left edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-next-line (- arg 1)))
  (image-set-window-hscroll 0))

(defun image-eol (arg)
  "Scroll horizontally to the right edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-next-line (- arg 1)))
  (let* ((image (image-get-display-property))
	 (edges (window-inside-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))))

(defun image-bob ()
  "Scroll to the top-left corner of the image in the current window."
  (interactive)
  (image-set-window-hscroll 0)
  (image-set-window-vscroll 0))

(defun image-eob ()
  "Scroll to the bottom-right corner of the image in the current window."
  (interactive)
  (let* ((image (image-get-display-property))
	 (edges (window-inside-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-display-size image))))
	 (win-height (- (nth 3 edges) (nth 1 edges)))
	 (img-height (ceiling (cdr (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))
    (image-set-window-vscroll (max 0 (- img-height win-height)))))

;; Adjust frame and image size.

(defun image-mode-fit-frame ()
  "Toggle whether to fit the frame to the current image.
This function assumes the current frame has only one window."
  ;; FIXME: This does not take into account decorations like mode-line,
  ;; minibuffer, header-line, ...
  (interactive)
  (let* ((saved (frame-parameter nil 'image-mode-saved-size))
         (display (image-get-display-property))
         (size (image-display-size display)))
    (if (and saved
             (eq (caar saved) (frame-width))
             (eq (cdar saved) (frame-height)))
        (progn ;; Toggle back to previous non-fitted size.
          (set-frame-parameter nil 'image-mode-saved-size nil)
          (setq size (cdr saved)))
      ;; Round up size, and save current size so we can toggle back to it.
      (setcar size (ceiling (car size)))
      (setcdr size (ceiling (cdr size)))
      (set-frame-parameter nil 'image-mode-saved-size
                           (cons size (cons (frame-width) (frame-height)))))
    (set-frame-width  (selected-frame) (car size))
    (set-frame-height (selected-frame) (cdr size))))

;;; Image Mode setup

(defvar image-type nil
  "The image type for the current Image mode buffer.")
(make-variable-buffer-local 'image-type)

(defvar image-mode-previous-major-mode nil
  "Internal variable to keep the previous non-image major mode.")

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "\C-c\C-c" 'image-toggle-display)
    (define-key map (kbd "SPC")       'image-scroll-up)
    (define-key map (kbd "DEL")       'image-scroll-down)
    (define-key map (kbd "RET")       'image-toggle-animation)
    (define-key map [remap forward-char] 'image-forward-hscroll)
    (define-key map [remap backward-char] 'image-backward-hscroll)
    (define-key map [remap right-char] 'image-forward-hscroll)
    (define-key map [remap left-char] 'image-backward-hscroll)
    (define-key map [remap previous-line] 'image-previous-line)
    (define-key map [remap next-line] 'image-next-line)
    (define-key map [remap scroll-up] 'image-scroll-up)
    (define-key map [remap scroll-down] 'image-scroll-down)
    (define-key map [remap scroll-up-command] 'image-scroll-up)
    (define-key map [remap scroll-down-command] 'image-scroll-down)
    (define-key map [remap move-beginning-of-line] 'image-bol)
    (define-key map [remap move-end-of-line] 'image-eol)
    (define-key map [remap beginning-of-buffer] 'image-bob)
    (define-key map [remap end-of-buffer] 'image-eob)
    map)
  "Mode keymap for `image-mode'.")

(defvar image-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    map)
  "Mode keymap for `image-minor-mode'.")

(defvar bookmark-make-record-function)

(put 'image-mode 'mode-class 'special)

;;;###autoload
(defun image-mode ()
  "Major mode for image files.
You can use \\<image-mode-map>\\[image-toggle-display]
to toggle between display as an image and display as text."
  (interactive)
  (condition-case err
      (progn
	(unless (display-images-p)
	  (error "Display does not support images"))

	(kill-all-local-variables)
	(setq major-mode 'image-mode)

	(if (not (image-get-display-property))
	    (progn
	      (image-toggle-display-image)
	      ;; If attempt to display the image fails.
	      (if (not (image-get-display-property))
		  (error "Invalid image")))
	  ;; Set next vars when image is already displayed but local
	  ;; variables were cleared by kill-all-local-variables
	  (setq cursor-type nil truncate-lines t
		image-type (plist-get (cdr (image-get-display-property)) :type)))

	(setq mode-name (if image-type (format "Image[%s]" image-type) "Image"))
	(use-local-map image-mode-map)

	;; Use our own bookmarking function for images.
	(set (make-local-variable 'bookmark-make-record-function)
	     'image-bookmark-make-record)

	;; Keep track of [vh]scroll when switching buffers
	(image-mode-setup-winprops)

	(add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
	(add-hook 'after-revert-hook 'image-after-revert-hook nil t)
	(run-mode-hooks 'image-mode-hook)
	(let ((image (image-get-display-property))
	      (msg1 (substitute-command-keys
		     "Type \\[image-toggle-display] to view the image as ")))
	  (cond
	   ((null image)
	    (message "%s" (concat msg1 "an image.")))
	   ((image-animated-p image)
	    (message "%s"
		     (concat msg1 "text, or "
			     (substitute-command-keys
			      "\\[image-toggle-animation] to animate."))))
	   (t
	    (message "%s" (concat msg1 "text."))))))

    (error
     (image-mode-as-text)
     (funcall
      (if (called-interactively-p 'any) 'error 'message)
      "Cannot display image: %s" (cdr err)))))

;;;###autoload
(define-minor-mode image-minor-mode
  "Toggle Image minor mode in this buffer.
With a prefix argument ARG, enable Image minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Image minor mode provides the key \\<image-mode-map>\\[image-toggle-display],
to switch back to `image-mode' and display an image file as the
actual image."
  nil (:eval (if image-type (format " Image[%s]" image-type) " Image"))
  image-minor-mode-map
  :group 'image
  :version "22.1"
  (if image-minor-mode
      (add-hook 'change-major-mode-hook (lambda () (image-minor-mode -1)) nil t)))

;;;###autoload
(defun image-mode-as-text ()
  "Set a non-image mode as major mode in combination with image minor mode.
A non-image major mode found from `auto-mode-alist' or Fundamental mode
displays an image file as text.  `image-minor-mode' provides the key
\\<image-mode-map>\\[image-toggle-display] to switch back to `image-mode'
to display an image file as the actual image.

You can use `image-mode-as-text' in `auto-mode-alist' when you want
to display an image file as text initially.

See commands `image-mode' and `image-minor-mode' for more information
on these modes."
  (interactive)
  ;; image-mode-as-text = normal-mode + image-minor-mode
  (let ((previous-image-type image-type)) ; preserve `image-type'
    (if image-mode-previous-major-mode
	;; Restore previous major mode that was already found by this
	;; function and cached in `image-mode-previous-major-mode'
	(funcall image-mode-previous-major-mode)
      (let ((auto-mode-alist
	     (delq nil (mapcar
			(lambda (elt)
			  (unless (memq (or (car-safe (cdr elt)) (cdr elt))
					'(image-mode image-mode-maybe image-mode-as-text))
			    elt))
			auto-mode-alist)))
	    (magic-fallback-mode-alist
	     (delq nil (mapcar
			(lambda (elt)
			  (unless (memq (or (car-safe (cdr elt)) (cdr elt))
					'(image-mode image-mode-maybe image-mode-as-text))
			    elt))
			magic-fallback-mode-alist))))
	(normal-mode)
	(set (make-local-variable 'image-mode-previous-major-mode) major-mode)))
    ;; Restore `image-type' after `kill-all-local-variables' in `normal-mode'.
    (setq image-type previous-image-type)
    ;; Enable image minor mode with `C-c C-c'.
    (image-minor-mode 1)
    ;; Show the image file as text.
    (image-toggle-display-text)
    (message "%s" (concat
		   (substitute-command-keys
		    "Type \\[image-toggle-display] to view the image as ")
		   (if (image-get-display-property)
		       "text" "an image") "."))))

(define-obsolete-function-alias 'image-mode-maybe 'image-mode "23.2")

(defun image-toggle-display-text ()
  "Show the image file as text.
Remove text properties that display the image."
  (let ((inhibit-read-only t)
	(buffer-undo-list t)
	(modified (buffer-modified-p)))
    (remove-list-of-text-properties (point-min) (point-max)
				    '(display read-nonsticky ;; intangible
					      read-only front-sticky))
    (set-buffer-modified-p modified)
    (if (called-interactively-p 'any)
	(message "Repeat this command to go back to displaying the image"))))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)
(declare-function image-flush "image.c" (spec &optional frame))

(defun image-toggle-display-image ()
  "Show the image of the image file.
Turn the image data into a real image, but only if the whole file
was inserted."
  (unless (derived-mode-p 'image-mode)
    (error "The buffer is not in Image mode"))
  (let* ((filename (buffer-file-name))
	 (data-p (not (and filename
			   (file-readable-p filename)
			   (not (file-remote-p filename))
			   (not (buffer-modified-p))
			   (not (and (boundp 'archive-superior-buffer)
				     archive-superior-buffer))
			   (not (and (boundp 'tar-superior-buffer)
				     tar-superior-buffer)))))
	 (file-or-data (if data-p
			   (string-make-unibyte
			    (buffer-substring-no-properties (point-min) (point-max)))
			 filename))
	 (type (image-type file-or-data nil data-p))
	 (image (create-image file-or-data type data-p))
	 (inhibit-read-only t)
	 (buffer-undo-list t)
	 (modified (buffer-modified-p))
	 props)

    ;; Discard any stale image data before looking it up again.
    (image-flush image)
    (setq image (append image (image-transform-properties image)))
    (setq props
	  `(display ,image
		    ;; intangible ,image
		    rear-nonsticky (display) ;; intangible
		    read-only t front-sticky (read-only)))

    (let ((buffer-file-truename nil)) ; avoid changing dir mtime by lock_file
      (add-text-properties (point-min) (point-max) props)
      (restore-buffer-modified-p modified))
    ;; Inhibit the cursor when the buffer contains only an image,
    ;; because cursors look very strange on top of images.
    (setq cursor-type nil)
    ;; This just makes the arrow displayed in the right fringe
    ;; area look correct when the image is wider than the window.
    (setq truncate-lines t)
    ;; Disable adding a newline at the end of the image file when it
    ;; is written with, e.g., C-x C-w.
    (if (coding-system-equal (coding-system-base buffer-file-coding-system)
			     'no-conversion)
	(set (make-local-variable 'find-file-literally) t))
    ;; Allow navigation of large images
    (set (make-local-variable 'auto-hscroll-mode) nil)
    (setq image-type type)
    (if (eq major-mode 'image-mode)
	(setq mode-name (format "Image[%s]" type)))
    (if (called-interactively-p 'any)
	(message "Repeat this command to go back to displaying the file as text"))))

(defun image-toggle-display ()
  "Toggle between image and text display.
If the current buffer is displaying an image file as an image,
call `image-mode-as-text' to switch to text.  Otherwise, display
the image by calling `image-mode'."
  (interactive)
  (if (image-get-display-property)
      (image-mode-as-text)
    (image-mode)))

(defun image-after-revert-hook ()
  (when (image-get-display-property)
    (image-toggle-display-text)
    ;; Update image display.
    (mapc (lambda (window) (redraw-frame (window-frame window)))
          (get-buffer-window-list (current-buffer) 'nomini 'visible))
    (image-toggle-display-image)))


;;; Animated images

(defcustom image-animate-loop nil
  "Non-nil means animated images loop forever, rather than playing once."
  :type 'boolean
  :version "24.1"
  :group 'image)

(defun image-toggle-animation ()
  "Start or stop animating the current image.
If `image-animate-loop' is non-nil, animation loops forever.
Otherwise it plays once, then stops."
  (interactive)
  (let ((image (image-get-display-property))
	animation)
    (cond
     ((null image)
      (error "No image is present"))
     ((null (setq animation (image-animated-p image)))
      (message "No image animation."))
     (t
      (let ((timer (image-animate-timer image)))
	(if timer
	    (cancel-timer timer)
	  (let ((index (plist-get (cdr image) :index)))
	    ;; If we're at the end, restart.
	    (and index
		 (>= index (1- (car animation)))
		 (setq index nil))
	    (image-animate image index
			   (if image-animate-loop t)))))))))


;;; Support for bookmark.el
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))

(defun image-bookmark-make-record ()
  `(,@(bookmark-make-record-default nil 'no-context 0)
      (image-type . ,image-type)
      (handler    . image-bookmark-jump)))

;;;###autoload
(defun image-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `bookmark-make-record-function', which see.
  (prog1 (bookmark-default-handler bmk)
    (when (not (string= image-type (bookmark-prop-get bmk 'image-type)))
      (image-toggle-display))))


;; Not yet implemented.
;;; (defvar image-transform-minor-mode-map
;;;   (let ((map (make-sparse-keymap)))
;;;     ;; (define-key map  [(control ?+)] 'image-scale-in)
;;;     ;; (define-key map  [(control ?-)] 'image-scale-out)
;;;     ;; (define-key map  [(control ?=)] 'image-scale-none)
;;;     ;; (define-key map "c f h" 'image-scale-fit-height)
;;;     ;; (define-key map "c ]" 'image-rotate-right)
;;;     map)
;;;   "Minor mode keymap `image-transform-mode'.")
;;;
;;; (define-minor-mode image-transform-mode
;;;   "Minor mode for scaling and rotating images.
;;; With a prefix argument ARG, enable the mode if ARG is positive,
;;; and disable it otherwise.  If called from Lisp, enable the mode
;;; if ARG is omitted or nil.  This minor mode requires Emacs to have
;;; been compiled with ImageMagick support."
;;;   nil "image-transform" image-transform-minor-mode-map)


;; FIXME this doesn't seem mature yet. Document in manual when it is.
(defvar image-transform-resize nil
  "The image resize operation.
Its value should be one of the following:
 - nil, meaning no resizing.
 - `fit-height', meaning to fit the image to the window height.
 - `fit-width', meaning to fit the image to the window width.
 - A number, which is a scale factor (the default size is 100).")

(defvar image-transform-rotation 0.0
  "Rotation angle for the image in the current Image mode buffer.")

(defun image-transform-properties (spec)
  "Return rescaling/rotation properties for image SPEC.
These properties are determined by the Image mode variables
`image-transform-resize' and `image-transform-rotation'.  The
return value is suitable for appending to an image spec.

Rescaling and rotation properties only take effect if Emacs is
compiled with ImageMagick support."
  (when (or image-transform-resize
	    (not (equal image-transform-rotation 0.0)))
    ;; Note: `image-size' looks up and thus caches the untransformed
    ;; image.  There's no easy way to prevent that.
    (let* ((size (image-size spec t))
	   (height
	    (cond
	     ((numberp image-transform-resize)
	      (unless (= image-transform-resize 100)
		(* image-transform-resize (cdr size))))
	     ((eq image-transform-resize 'fit-height)
	      (- (nth 3 (window-inside-pixel-edges))
		 (nth 1 (window-inside-pixel-edges))))))
	   (width (if (eq image-transform-resize 'fit-width)
		      (- (nth 2 (window-inside-pixel-edges))
			 (nth 0 (window-inside-pixel-edges))))))
      ;;TODO fit-to-* should consider the rotation angle
      `(,@(if height (list :height height))
	,@(if width (list :width width))
	,@(if (not (equal 0.0 image-transform-rotation))
	      (list :rotation image-transform-rotation))))))

;; FIXME 2 works, but eg 1.9 or 0.5 don't?
(defun image-transform-set-scale (scale)
  "Prompt for a number, and resize the current image by that amount.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive "nScale: ")
  (setq image-transform-resize scale)
  (image-toggle-display-image))

(defun image-transform-fit-to-height ()
  "Fit the current image to the height of the current window.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive)
  (setq image-transform-resize 'fit-height)
  (image-toggle-display-image))

(defun image-transform-fit-to-width ()
  "Fit the current image to the width of the current window.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive)
  (setq image-transform-resize 'fit-width)
  (image-toggle-display-image))

(defun image-transform-set-rotation (rotation)
  "Prompt for an angle ROTATION, and rotate the image by that amount.
ROTATION should be in degrees.  This command has no effect unless
Emacs is compiled with ImageMagick support."
  (interactive "nRotation angle (in degrees): ")
  ;;TODO 0 90 180 270 degrees are the only reasonable angles here
  ;;otherwise combining with rescaling will get very awkward
  (setq image-transform-rotation (float rotation))
  (image-toggle-display-image))

(provide 'image-mode)

;;; image-mode.el ends here
