;;; eudc-bob.el --- Binary Objects Support for EUDC

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;; Maintainer: Pavel Janík <Pavel@Janik.cz>
;; Keywords: comm
;; Package: eudc

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

;;; Usage:
;;    See the corresponding info file

;;; Code:

(require 'eudc)

(defvar eudc-bob-generic-keymap nil
  "Keymap for multimedia objects.")

(defvar eudc-bob-image-keymap nil
  "Keymap for inline images.")

(defvar eudc-bob-sound-keymap nil
  "Keymap for inline sounds.")

(defvar eudc-bob-url-keymap nil
  "Keymap for inline urls.")

(defvar eudc-bob-mail-keymap nil
  "Keymap for inline e-mail addresses.")

(defvar eudc-bob-generic-menu
  '("EUDC Binary Object Menu"
    ["---" nil nil]
    ["Pipe to external program" eudc-bob-pipe-object-to-external-program t]
    ["Save object" eudc-bob-save-object t]))

(defvar eudc-bob-image-menu
  `("EUDC Image Menu"
    ["---" nil nil]
    ["Toggle inline display" eudc-bob-toggle-inline-display
     (eudc-bob-can-display-inline-images)]
    ,@(cdr (cdr eudc-bob-generic-menu))))

(defvar eudc-bob-sound-menu
  `("EUDC Sound Menu"
    ["---" nil nil]
    ["Play sound" eudc-bob-play-sound-at-point
     (fboundp 'play-sound)]
    ,@(cdr (cdr eudc-bob-generic-menu))))

(defun eudc-jump-to-event (event)
  "Jump to the window and point where EVENT occurred."
  (if (fboundp 'event-closest-point)
      (goto-char (event-closest-point event))
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))))

(defun eudc-bob-get-overlay-prop (prop)
  "Get property PROP from one of the overlays around."
  (let ((overlays (append (overlays-at (1- (point)))
			  (overlays-at (point))))
	overlay value
	(notfound t))
    (while (and notfound
		(setq overlay (car overlays)))
      (if (setq value (overlay-get overlay prop))
	  (setq notfound nil))
      (setq overlays (cdr overlays)))
    value))

(defun eudc-bob-can-display-inline-images ()
  "Return non-nil if we can display images inline."
  (if (fboundp 'console-type)
      (and (memq (console-type) '(x mswindows))
	   (fboundp 'make-glyph))
    (and (fboundp 'display-graphic-p)
	 (display-graphic-p))))

(defun eudc-bob-make-button (label keymap &optional menu plist)
  "Create a button with LABEL.
Attach KEYMAP, MENU and properties from PLIST to a new overlay covering
LABEL."
  (let (overlay
	(p (point))
	prop val)
    (insert label)
    (put-text-property p (point) 'face 'bold)
    (setq overlay (make-overlay p (point)))
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'keymap keymap)
    (overlay-put overlay 'local-map keymap)
    (overlay-put overlay 'menu menu)
    (while plist
      (setq prop (car plist)
	    plist (cdr plist)
	    val (car plist)
	    plist (cdr plist))
      (overlay-put overlay prop val))))

(defun eudc-bob-display-jpeg (data inline)
  "Display the JPEG DATA at point.
If INLINE is non-nil, try to inline the image otherwise simply
display a button."
  (cond ((fboundp 'make-glyph)
	 (let ((glyph (if (eudc-bob-can-display-inline-images)
			  (make-glyph (list (vector 'jpeg :data data)
					    [string :data "[JPEG Picture]"])))))
	   (eudc-bob-make-button "[JPEG Picture]"
				 eudc-bob-image-keymap
				 eudc-bob-image-menu
				 (list 'glyph glyph
				       'end-glyph (if inline glyph)
				       'duplicable t
				       'invisible inline
				       'start-open t
				       'end-open t
				       'object-data data))))
	((fboundp 'create-image)
	 (let* ((image (create-image data nil t))
		(props (list 'object-data data 'eudc-image image)))
	   (when (and inline (image-type-available-p 'jpeg))
	     (setq props (nconc (list 'display image) props)))
	   (eudc-bob-make-button "[Picture]"
				 eudc-bob-image-keymap
				 eudc-bob-image-menu
				 props)))))

(defun eudc-bob-toggle-inline-display ()
  "Toggle inline display of an image."
  (interactive)
  (when (eudc-bob-can-display-inline-images)
    (cond ((featurep 'xemacs)
	   (let ((overlays (append (overlays-at (1- (point)))
				   (overlays-at (point))))
		 overlay glyph)
	     (setq overlay (car overlays))
	     (while (and overlay
			 (not (setq glyph (overlay-get overlay 'glyph))))
	       (setq overlays (cdr overlays))
	       (setq overlay (car overlays)))
	     (if overlay
		 (if (overlay-get overlay 'end-glyph)
		     (progn
		       (overlay-put overlay 'end-glyph nil)
		       (overlay-put overlay 'invisible nil))
		   (overlay-put overlay 'end-glyph glyph)
		   (overlay-put overlay 'invisible t)))))
	  (t
	   (let* ((overlays (append (overlays-at (1- (point)))
				    (overlays-at (point))))
		  image)

	     ;; Search overlay with an image.
	     (while (and overlays (null image))
	       (let ((prop (overlay-get (car overlays) 'eudc-image)))
		 (if (eq 'image (car-safe prop))
		     (setq image prop)
		   (setq overlays (cdr overlays)))))

	     ;; Toggle that overlay's image display.
	     (when overlays
	       (let ((overlay (car overlays)))
		 (overlay-put overlay 'display
			      (if (overlay-get overlay 'display)
				  nil image)))))))))

(defun eudc-bob-display-audio (data)
  "Display a button for audio DATA."
  (eudc-bob-make-button "[Audio Sound]"
			eudc-bob-sound-keymap
			eudc-bob-sound-menu
			(list 'duplicable t
			      'start-open t
			      'end-open t
			      'object-data data)))

(defun eudc-bob-display-generic-binary (data)
  "Display a button for unidentified binary DATA."
  (eudc-bob-make-button "[Binary Data]"
			eudc-bob-generic-keymap
			eudc-bob-generic-menu
			(list 'duplicable t
			      'start-open t
			      'end-open t
			      'object-data data)))

(defun eudc-bob-play-sound-at-point ()
  "Play the sound data contained in the button at point."
  (interactive)
  (let (sound)
    (if (null (setq sound (eudc-bob-get-overlay-prop 'object-data)))
	(error "No sound data available here")
      (unless (fboundp 'play-sound)
	(error "Playing sounds not supported on this system"))
      (play-sound (list 'sound :data sound)))))

(defun eudc-bob-play-sound-at-mouse (event)
  "Play the sound data contained in the button where EVENT occurred."
  (interactive "e")
  (save-excursion
    (eudc-jump-to-event event)
    (eudc-bob-play-sound-at-point)))

(defun eudc-bob-save-object ()
  "Save the object data of the button at point."
  (interactive)
  (let ((data (eudc-bob-get-overlay-prop 'object-data))
	(buffer (generate-new-buffer "*eudc-tmp*")))
    (save-excursion
      (if (fboundp 'set-buffer-file-coding-system)
	  (set-buffer-file-coding-system 'binary))
      (set-buffer buffer)
      (set-buffer-multibyte nil)
      (insert data)
      (save-buffer))
    (kill-buffer buffer)))

(defun eudc-bob-pipe-object-to-external-program ()
  "Pipe the object data of the button at point to an external program."
  (interactive)
  (let ((data (eudc-bob-get-overlay-prop 'object-data))
	(buffer (generate-new-buffer "*eudc-tmp*"))
	program
	viewer)
    (condition-case nil
	(save-excursion
	  (if (fboundp 'set-buffer-file-coding-system)
	      (set-buffer-file-coding-system 'binary))
	  (set-buffer buffer)
	  (insert data)
	  (setq program (completing-read "Viewer: " eudc-external-viewers))
	  (if (setq viewer (assoc program eudc-external-viewers))
	      (call-process-region (point-min) (point-max)
				   (car (cdr viewer))
				   (cdr (cdr viewer)))
	    (call-process-region (point-min) (point-max) program)))
      (error
       (kill-buffer buffer)))))

(defun eudc-bob-menu ()
  "Retrieve the menu attached to a binary object."
  (eudc-bob-get-overlay-prop 'menu))

(defun eudc-bob-popup-menu (event)
  "Pop-up a menu of EUDC multimedia commands."
  (interactive "@e")
  (run-hooks 'activate-menubar-hook)
  (eudc-jump-to-event event)
  (if (featurep 'xemacs)
      (progn
	(run-hooks 'activate-popup-menu-hook)
	(popup-menu (eudc-bob-menu)))
    (let ((result (x-popup-menu t (eudc-bob-menu)))
	  command)
      (if result
	  (progn
	    (setq command (lookup-key (eudc-bob-menu)
				      (apply 'vector result)))
	    (command-execute command))))))

(setq eudc-bob-generic-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map "s" 'eudc-bob-save-object)
	(define-key map "!" 'eudc-bob-pipe-object-to-external-program)
	(define-key map (if (featurep 'xemacs)
			    [button3]
			  [down-mouse-3]) 'eudc-bob-popup-menu)
	map))

(setq eudc-bob-image-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map "t" 'eudc-bob-toggle-inline-display)
	map))

(setq eudc-bob-sound-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map [return] 'eudc-bob-play-sound-at-point)
	(define-key map (if (featurep 'xemacs)
			    [button2]
			  [down-mouse-2]) 'eudc-bob-play-sound-at-mouse)
	map))

(setq eudc-bob-url-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map [return] 'browse-url-at-point)
	(define-key map (if (featurep 'xemacs)
			    [button2]
			  [down-mouse-2]) 'browse-url-at-mouse)
	map))

(setq eudc-bob-mail-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map [return] 'goto-address-at-point)
	(define-key map (if (featurep 'xemacs)
			    [button2]
			  [down-mouse-2]) 'goto-address-at-mouse)
	map))

(set-keymap-parent eudc-bob-image-keymap eudc-bob-generic-keymap)
(set-keymap-parent eudc-bob-sound-keymap eudc-bob-generic-keymap)

;; If the first arguments can be nil here, then these 3 can be
;; defconsts once more.
(when (not (featurep 'xemacs))
  (easy-menu-define eudc-bob-generic-menu
    eudc-bob-generic-keymap
    ""
    eudc-bob-generic-menu)
  (easy-menu-define eudc-bob-image-menu
    eudc-bob-image-keymap
    ""
    eudc-bob-image-menu)
  (easy-menu-define eudc-bob-sound-menu
    eudc-bob-sound-keymap
    ""
    eudc-bob-sound-menu))

;;;###autoload
(defun eudc-display-generic-binary (data)
  "Display a button for unidentified binary DATA."
  (eudc-bob-display-generic-binary data))

;;;###autoload
(defun eudc-display-url (url)
  "Display URL and make it clickable."
  (require 'browse-url)
  (eudc-bob-make-button url eudc-bob-url-keymap))

;;;###autoload
(defun eudc-display-mail (mail)
  "Display e-mail address and make it clickable."
  (require 'goto-addr)
  (eudc-bob-make-button mail eudc-bob-mail-keymap))

;;;###autoload
(defun eudc-display-sound (data)
  "Display a button to play the sound DATA."
  (eudc-bob-display-audio data))

;;;###autoload
(defun eudc-display-jpeg-inline (data)
  "Display the JPEG DATA inline at point if possible."
  (eudc-bob-display-jpeg data (eudc-bob-can-display-inline-images)))

;;;###autoload
(defun eudc-display-jpeg-as-button (data)
  "Display a button for the JPEG DATA."
  (eudc-bob-display-jpeg data nil))

;;; eudc-bob.el ends here
