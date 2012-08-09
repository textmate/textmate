;;; gs.el --- interface to Ghostscript

;; Copyright (C) 1998, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; This code is experimental.  Don't use it.

;;; Code:

(defvar gs-program "gs"
  "The name of the Ghostscript interpreter.")


(defvar gs-device "x11"
  "The Ghostscript device to use to produce images.")


(defvar gs-options
  '("-q"
    ;"-dNOPAUSE"
    "-dSAFER"
    "-dBATCH"
    "-sDEVICE=<device>"
    "<file>")
  "List of command line arguments to pass to Ghostscript.
Arguments may contain place-holders `<file>' for the name of the
input file, and `<device>' for the device to use.")
(put 'gs-options 'risky-local-variable t)

(defun gs-options (device file)
  "Return a list of command line options with place-holders replaced.
DEVICE is the value to substitute for the place-holder `<device>',
FILE is the value to substitute for the place-holder `<file>'."
  (mapcar #'(lambda (option)
	      (setq option (replace-regexp-in-string "<device>" device option)
		    option (replace-regexp-in-string "<file>" file option)))
	  gs-options))

;; The GHOSTVIEW property (taken from gv 3.5.8).
;;
;; Type:
;;
;; STRING
;;
;; Parameters:
;;
;; BPIXMAP ORIENT LLX LLY URX URY XDPI YDPI [LEFT BOTTOM TOP RIGHT]
;;
;; Scanf format: "%d %d %d %d %d %d %f %f %d %d %d %d"
;;
;; Explanation of parameters:
;;
;; BPIXMAP: pixmap id of the backing pixmap for the window.  If no
;; pixmap is to be used, this parameter should be zero.  This
;; parameter must be zero when drawing on a pixmap.
;;
;; ORIENT: orientation of the page.  The number represents clockwise
;; rotation of the paper in degrees.  Permitted values are 0, 90, 180,
;; 270.
;;
;; LLX, LLY, URX, URY: Bounding box of the drawable.  The bounding box
;; is specified in PostScript points in default user coordinates.
;;
;; XDPI, YDPI: Resolution of window.  (This can be derived from the
;; other parameters, but not without roundoff error.  These values are
;; included to avoid this error.)
;;
;; LEFT, BOTTOM, TOP, RIGHT: (optional) Margins around the window.
;; The margins extend the imageable area beyond the boundaries of the
;; window.  This is primarily used for popup zoom windows.  I have
;; encountered several instances of PostScript programs that position
;; themselves with respect to the imageable area.  The margins are
;; specified in PostScript points.  If omitted, the margins are
;; assumed to be 0.

(declare-function x-display-mm-width "xfns.c" (&optional terminal))
(declare-function x-display-pixel-width "xfns.c" (&optional terminal))

(defun gs-width-in-pt (frame pixel-width)
  "Return, on FRAME, pixel width PIXEL-WIDTH translated to pt."
  (let ((mm (* (float pixel-width)
	       (/ (float (x-display-mm-width frame))
		  (float (x-display-pixel-width frame))))))
    (/ (* 25.4 mm) 72.0)))

(declare-function x-display-mm-height "xfns.c" (&optional terminal))
(declare-function x-display-pixel-height "xfns.c" (&optional terminal))

(defun gs-height-in-pt (frame pixel-height)
  "Return, on FRAME, pixel height PIXEL-HEIGHT translated to pt."
  (let ((mm (* (float pixel-height)
	       (/ (float (x-display-mm-height frame))
		  (float (x-display-pixel-height frame))))))
    (/ (* 25.4 mm) 72.0)))

(declare-function x-change-window-property "xfns.c"
		  (prop value &optional frame type format outer-p))

(defun gs-set-ghostview-window-prop (frame spec img-width img-height)
  "Set the `GHOSTVIEW' window property of FRAME.
SPEC is a GS image specification.  IMG-WIDTH is the width of the
requested image, and IMG-HEIGHT is the height of the requested
image in pixels."
  (let* ((box (plist-get (cdr spec) :bounding-box))
	 (llx (elt box 0))
	 (lly (elt box 1))
	 (urx (elt box 2))
	 (ury (elt box 3))
	 (rotation (or (plist-get (cdr spec) :rotate) 0))
	 ;; The pixel width IMG-WIDTH of the pixmap gives the
	 ;; dots, URX - LLX give the inch.
	 (in-width (/ (- urx llx) 72.0))
	 (in-height (/ (- ury lly) 72.0))
	 (xdpi (/ img-width in-width))
	 (ydpi (/ img-height in-height)))
    (x-change-window-property "GHOSTVIEW"
			      (format "0 %d %d %d %d %d %g %g"
				      rotation llx lly urx ury xdpi ydpi)
			      frame)))

(declare-function x-display-grayscale-p "xfns.c" (&optional terminal))

(defun gs-set-ghostview-colors-window-prop (frame pixel-colors)
  "Set the `GHOSTVIEW_COLORS' environment variable depending on FRAME."
  (let ((mode (cond ((x-display-color-p frame) "Color")
		    ((x-display-grayscale-p frame) "Grayscale")
		    (t "Monochrome"))))
    (x-change-window-property "GHOSTVIEW_COLORS"
			      (format "%s %s" mode pixel-colors)
			      frame)))

(declare-function x-window-property "xfns.c"
		  (prop &optional frame type source delete-p vector-ret-p))

;;;###autoload
(defun gs-load-image (frame spec img-width img-height window-and-pixmap-id
			    pixel-colors)
  "Load a PS image for display on FRAME.
SPEC is an image specification, IMG-HEIGHT and IMG-WIDTH are width
and height of the image in pixels.  WINDOW-AND-PIXMAP-ID is a string of
the form \"WINDOW-ID PIXMAP-ID\".  Value is non-nil if successful."
  (unwind-protect
      (let ((file (plist-get (cdr spec) :file))
	    gs
 	    (timeout 40))
 	;; Wait while property gets freed from a previous ghostscript process
 	;; sit-for returns nil as soon as input starts being
	;; available, so if we want to give GhostScript a reasonable
 	;; chance of starting up, we better use sleep-for.  We let
 	;; sleep-for wait only half the time because if input is
 	;; available, it is more likely that we don't care that much
 	;; about garbled redisplay and are in a hurry.
 	(while (and
 		;; Wait while the property is not yet available
 		(not (zerop (length (x-window-property "GHOSTVIEW"
 						       frame))))
 		;; The following was an alternative condition: wait
 		;; while there is still a process running.  The idea
 		;; was to avoid contention between processes.  Turned
 		;; out even more sluggish.
 		;; (get-buffer-process "*GS*")
 		(not (zerop timeout)))
 	  (unless (sit-for 0.1 t)
 	    (sleep-for 0.05))
 	  (setq timeout (1- timeout)))

 	;; No use waiting longer.  We might want to try killing off
 	;; stuck processes, but there is no point in doing so: either
 	;; they are stuck for good, in which case the user would
 	;; probably be responsible for that, and killing them off will
 	;; make debugging harder, or they are not.  In that case, they
 	;; will cause incomplete displays.  But the same will happen
 	;; if they are killed, anyway. The whole is rather
 	;; disconcerting, and fast scrolling through a dozen images
 	;; will make Emacs freeze for a while.  The alternatives are a)
 	;; proper implementation not waiting at all but creating
 	;; appropriate queues, or b) permanently bad display due to
 	;; bad cached images.  So remember that this
 	;; is just a hack and if people don't like the behavior, they
 	;; will most likely like the easy alternatives even less.
 	;; And at least the image cache will make the delay apparent
 	;; just once.
	(gs-set-ghostview-window-prop frame spec img-width img-height)
	(gs-set-ghostview-colors-window-prop frame pixel-colors)
	(setenv "GHOSTVIEW" window-and-pixmap-id)
	(setq gs (apply 'start-process "gs" "*GS*" gs-program
			(gs-options gs-device file)))
	(set-process-query-on-exit-flag gs nil)
	gs)
    nil))


;(defun gs-put-tiger ()
;  (let* ((ps-file "/usr/local/share/ghostscript/5.10/examples/tiger.ps")
;	  (spec `(image :type postscript
;			:pt-width 200 :pt-height 200
;			:bounding-box (22 171 567 738)
;			:file ,ps-file)))
;    (put-text-property 1 2 'display spec)))
;

(provide 'gs)

;;; gs.el ends here
