;;; pc-win.el --- setup support for `PC windows' (whatever that is)

;; Copyright (C) 1994, 1996-1997, 1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
;; Maintainer: FSF

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

;; This file is preloaded into Emacs by loadup.el.  The functions in
;; this file are then called during startup from startup.el.  This
;; means that just loading this file should not have any side effects
;; besides defining functions and variables, and in particular should
;; NOT initialize any window systems.

;; The main entry points to this file's features are msdos-handle-args,
;; msdos-create-frame-with-faces, msdos-initialize-window-system,
;; terminal-init-internal.  The last one is not supposed to be called,
;; so it just errors out.

;;; Code:

(if (not (fboundp 'msdos-remember-default-colors))
    (error "%s: Loading pc-win.el but not compiled for MS-DOS"
	   (invocation-name)))

(load "term/internal" nil t)

(declare-function msdos-remember-default-colors "msdos.c")
(declare-function w16-set-clipboard-data "w16select.c")
(declare-function w16-get-clipboard-data "w16select.c")
(declare-function msdos-setup-keyboard "internal" (frame))

;;; This was copied from etc/rgb.txt, except that some values were changed
;;; a bit to make them consistent with DOS console colors, and the RGB
;;; values were scaled up to 16 bits, as `tty-define-color' requires.
;;;
;;; The mapping between the 16 standard EGA/VGA colors and X color names
;;; was done by running a Unix version of Emacs inside an X client and a
;;; DJGPP-compiled Emacs on the same PC.  The names of X colors used to
;;; define the pixel values are shown as comments to each color below.
;;;
;;; If you want to change the RGB values, keep in mind that various pieces
;;; of Emacs think that a color whose RGB values add up to less than 0.6 of
;;; the values for WHITE (i.e. less than 117963) are ``dark'', otherwise the
;;; color is ``light''; see `frame-set-background-mode' in lisp/faces.el for
;;; an example.
(defvar msdos-color-values
  '(("black"          0     0     0     0)
    ("blue"           1     0     0 52480) ; MediumBlue
    ("green"          2  8704 35584  8704) ; ForestGreen
    ("cyan"           3     0 52736 53504) ; DarkTurquoise
    ("red"            4 45568  8704  8704) ; FireBrick
    ("magenta"        5 35584     0 35584) ; DarkMagenta
    ("brown"          6 40960 20992 11520) ; Sienna
    ("lightgray"      7 48640 48640 48640) ; Gray
    ("darkgray"       8 26112 26112 26112) ; Gray40
    ("lightblue"      9     0     0 65535) ; Blue
    ("lightgreen"    10     0 65535     0) ; Green
    ("lightcyan"     11     0 65535 65535) ; Cyan
    ("lightred"      12 65535     0     0) ; Red
    ("lightmagenta"  13 65535     0 65535) ; Magenta
    ("yellow"        14 65535 65535     0) ; Yellow
    ("white"         15 65535 65535 65535))
  "A list of MS-DOS console colors, their indices and 16-bit RGB values.")

;; ---------------------------------------------------------------------------
;; We want to delay setting frame parameters until the faces are setup
(defvar default-frame-alist nil)
;(modify-frame-parameters terminal-frame default-frame-alist)

(defun msdos-face-setup ()
  "Initial setup of faces for the MS-DOS display."
  (set-face-foreground 'bold "yellow")
  (set-face-foreground 'italic "red")
  (set-face-foreground 'bold-italic "lightred")
  (set-face-foreground 'underline "white")

  (make-face 'msdos-menu-active-face)
  (make-face 'msdos-menu-passive-face)
  (make-face 'msdos-menu-select-face)
  (set-face-foreground 'msdos-menu-active-face "white")
  (set-face-foreground 'msdos-menu-passive-face "lightgray")
  (set-face-background 'msdos-menu-active-face "blue")
  (set-face-background 'msdos-menu-passive-face "blue")
  (set-face-background 'msdos-menu-select-face "red"))

(defun msdos-handle-reverse-video (frame parameters)
  "Handle the reverse-video frame parameter on MS-DOS frames."
  (when (cdr (or (assq 'reverse parameters)
		 (assq 'reverse default-frame-alist)))
      (let* ((params (frame-parameters frame))
	     (fg (cdr (assq 'foreground-color params)))
	     (bg (cdr (assq 'background-color params))))
	(if (equal fg (cdr (assq 'mouse-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'mouse-color bg))))
	(if (equal fg (cdr (assq 'cursor-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'cursor-color bg)))))))

;; This must run after all the default colors are inserted into
;; tty-color-alist, since msdos-handle-reverse-video needs to know the
;; actual frame colors.
(defun msdos-setup-initial-frame ()
  (modify-frame-parameters terminal-frame default-frame-alist)
  ;; This remembers the screen colors after applying default-frame-alist,
  ;; so that all subsequent frames could begin with those colors.
  (msdos-remember-default-colors terminal-frame)
  (modify-frame-parameters terminal-frame initial-frame-alist)
  (msdos-handle-reverse-video terminal-frame
			      (frame-parameters terminal-frame))

  (frame-set-background-mode terminal-frame)
  (face-set-after-frame-default terminal-frame))

;; We create frames as if we were a terminal, but without invoking the
;; terminal-initialization function.  Also, our handling of reverse
;; video is slightly different.
(defun msdos-create-frame-with-faces (&optional parameters)
  "Create a frame on MS-DOS display.
Optional frame parameters PARAMETERS specify the frame parameters.
Parameters not specified by PARAMETERS are taken from
`default-frame-alist'.  If either PARAMETERS or `default-frame-alist'
contains a `reverse' parameter, handle that.  Value is the new frame
created."
  (let ((frame (make-terminal-frame parameters))
	success)
    (unwind-protect
	(with-selected-frame frame
	  (msdos-handle-reverse-video frame (frame-parameters frame))
          (unless (terminal-parameter frame 'terminal-initted)
            (set-terminal-parameter frame 'terminal-initted t))
	  (frame-set-background-mode frame)
	  (face-set-after-frame-default frame)
	  (setq success t))
      (unless success (delete-frame frame)))
    frame))

;; ---------------------------------------------------------------------------
;; More or less useful imitations of certain X-functions.  A lot of the
;; values returned are questionable, but usually only the form of the
;; returned value matters.  Also, by the way, recall that `ignore' is
;; a useful function for returning 'nil regardless of argument.

;; Note: Any re-definition in this file of a function that is defined
;; in C on other platforms, should either have no doc-string, or one
;; that is identical to the C version, but with the arglist signature
;; at the end.  Otherwise help-split-fundoc gets confused on other
;; platforms.  (Bug#10783)

;; From src/xfns.c
(defun x-list-fonts (pattern &optional face frame maximum width)
  (if (or (null width) (and (numberp width) (= width 1)))
      (list "ms-dos")
    (list "no-such-font")))
(defun x-display-pixel-width (&optional frame) (frame-width frame))
(defun x-display-pixel-height (&optional frame) (frame-height frame))
(defun x-display-planes (&optional frame) 4) ;bg switched to 16 colors as well
(defun x-display-color-cells (&optional frame) 16)
(defun x-server-max-request-size (&optional frame) 1000000) ; ???
(defun x-server-vendor (&optional frame) t "GNU")
(defun x-server-version (&optional frame) '(1 0 0))
(defun x-display-screens (&optional frame) 1)
(defun x-display-mm-height (&optional frame) 245) ; Guess the size of my
(defun x-display-mm-width (&optional frame) 322)  ; monitor, EZ...
(defun x-display-backing-store (&optional frame) 'not-useful)
(defun x-display-visual-class (&optional frame) 'static-color)
(fset 'x-display-save-under 'ignore)
(fset 'x-get-resource 'ignore)

;; From lisp/term/x-win.el
(defvar x-display-name "pc"
  "The name of the window display on which Emacs was started.
On X, the display name of individual X frames is recorded in the
`display' frame parameter.")
(defvar x-colors (mapcar 'car msdos-color-values)
  "List of basic colors available on color displays.
For X, the list comes from the `rgb.txt' file,v 10.41 94/02/20.
For Nextstep, this is a list of non-PANTONE colors returned by
the operating system.")

;; From lisp/term/w32-win.el
;
;;;; Selections
;
;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-selection-value.
(defvar x-last-selected-text nil)

(defcustom x-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection.

Note that MS-Windows does not support selection types other than the
clipboard.  (The primary selection that is set by Emacs is not
accessible to other programs on MS-Windows.)

This variable is not used by the Nextstep port."
  :type 'boolean
  :group 'killing)

(defun x-select-text (text)
  "Select TEXT, a string, according to the window system.

On X, if `x-select-enable-clipboard' is non-nil, copy TEXT to the
clipboard.  If `x-select-enable-primary' is non-nil, put TEXT in
the primary selection.

On MS-Windows, make TEXT the current selection.  If
`x-select-enable-clipboard' is non-nil, copy the text to the
clipboard as well.

On Nextstep, put TEXT in the pasteboard (`x-select-enable-clipboard'
is not used)."
  (if x-select-enable-clipboard
      (w16-set-clipboard-data text))
  (setq x-last-selected-text text))

;;; Return the value of the current selection.
;;; Consult the selection.  Treat empty strings as if they were unset.
(defun x-get-selection-value ()
  (if x-select-enable-clipboard
      (let (text)
	;; Don't die if x-get-selection signals an error.
	(condition-case c
	    (setq text (w16-get-clipboard-data))
	  (error (message "w16-get-clipboard-data:%s" c)))
	(if (string= text "") (setq text nil))
	(cond
	 ((not text) nil)
	 ((eq text x-last-selected-text) nil)
	 ((string= text x-last-selected-text)
	  ;; Record the newer string, so subsequent calls can use the 'eq' test.
	  (setq x-last-selected-text text)
	  nil)
	 (t
	  (setq x-last-selected-text text))))))

;; x-selection-owner-p is used in simple.el.
(defun x-selection-owner-p (&optional selection terminal)
  "Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.

\(fn &optional SELECTION TERMINAL)"
    (if x-select-enable-clipboard
      (let (text)
	;; Don't die if w16-get-clipboard-data signals an error.
	(ignore-errors
	  (setq text (w16-get-clipboard-data)))
	;; We consider ourselves the owner of the selection if it does
	;; not exist, or exists and compares equal with the last text
	;; we've put into the Windows clipboard.
	(cond
	 ((not text) t)
	 ((or (eq text x-last-selected-text)
	      (string= text x-last-selected-text))
	  text)
	 (t nil)))))

;; x-own-selection-internal and x-disown-selection-internal are used
;; in select.el:x-set-selection.
(defun x-own-selection-internal (selection value &optional frame)
  "Assert an X selection of the type SELECTION with and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.

FRAME should be a frame that should own the selection.  If omitted or
nil, it defaults to the selected frame.

On Nextstep, FRAME is unused.

\(fn SELECTION VALUE &optional FRAME)"
  (ignore-errors
    (x-select-text value))
  value)

(defun x-disown-selection-internal (selection &optional time-object terminal)
  "If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.

Sets the last-change time for the selection to TIME-OBJECT (by default
the time of the last event).

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, the TIME-OBJECT and TERMINAL arguments are unused.
On MS-DOS, all this does is return non-nil if we own the selection.

\(fn SELECTION &optional TIME-OBJECT TERMINAL)"
  (if (x-selection-owner-p selection)
      t))

;; x-get-selection-internal is used in select.el
(defun x-get-selection-internal (selection-symbol target-type &optional time-stamp terminal)
  "Return text selected from some X window.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'.

TIME-STAMP is the time to use in the XConvertSelection call for foreign
selections.  If omitted, defaults to the time for the last event.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TIME-STAMP and TERMINAL are unused.

\(fn SELECTION-SYMBOL TARGET-TYPE &optional TIME-STAMP TERMINAL)"
  (x-get-selection-value))

;; From src/fontset.c:
(fset 'query-fontset 'ignore)

;; From lisp/term/x-win.el: make iconify-or-deiconify-frame a no-op.
(fset 'iconify-or-deiconify-frame 'ignore)

;; From lisp/frame.el
(fset 'set-default-font 'ignore)
(fset 'set-mouse-color 'ignore)		; We cannot, I think.
(fset 'set-cursor-color 'ignore)	; Hardware determined by char under.
(fset 'set-border-color 'ignore)	; Not useful.

(defvar msdos-last-help-message nil
  "The last help message received via `show-help-function'.
This is used by `msdos-show-help'.")

(defvar msdos-previous-message nil
  "The content of the echo area before help echo was displayed.")

(defun msdos-show-help (help)
  "Function installed as `show-help-function' on MS-DOS frames."
  (when (and (not (window-minibuffer-p)) ;Don't overwrite minibuffer contents.
             (not cursor-in-echo-area)) ;Don't overwrite a prompt.
    (cond
     ((stringp help)
      (setq help (replace-regexp-in-string "\n" ", " help))
      (unless (or msdos-previous-message
		  (string-equal help (current-message))
		  (and (stringp msdos-last-help-message)
		       (string-equal msdos-last-help-message
				     (current-message))))
        (setq msdos-previous-message (current-message)))
      (setq msdos-last-help-message help)
      (let ((message-truncate-lines nil)
            (message-log-max nil))
        (message "%s" help)))
     ((stringp msdos-previous-message)
      (let ((message-log-max nil))
        (message "%s" msdos-previous-message)
        (setq msdos-previous-message nil)))
     (t
      (message nil)))))


;; Initialization.
;; ---------------------------------------------------------------------------
;; This function is run, by faces.el:tty-create-frame-with-faces, only
;; for the initial frame (on each terminal, but we have only one).
;; This works by setting the `terminal-initted' terminal parameter to
;; this function, the first time `tty-create-frame-with-faces' is
;; called on that terminal.  `tty-create-frame-with-faces' is called
;; directly from startup.el and also by `make-frame' through
;; `frame-creation-function-alist'.  `make-frame' will call this
;; function if `msdos-create-frame-with-faces' (see below) is not
;; found in `frame-creation-function-alist', which means something is
;; _very_ wrong, because "internal" terminal emulator should not be
;; turned on if our window-system is not `pc'.  Therefore, the only
;; Right Thing for us to do here is scream bloody murder.
(defun terminal-init-internal ()
  "Terminal initialization function for the MS-DOS \"internal\" terminal.
Errors out because it is not supposed to be called, ever."
  (error "terminal-init-internal called for window-system `%s'"
	 (window-system)))

(defun msdos-initialize-window-system ()
  "Initialization function for the `pc' \"window system\"."
  (or (eq (window-system) 'pc)
      (error
       "`msdos-initialize-window-system' called, but window-system is `%s'"
       (window-system)))
  ;; First, the keyboard.
  (msdos-setup-keyboard terminal-frame)	; see internal.el
  ;; Next, register the default colors.
  (let* ((colors msdos-color-values)
	 (color (car colors)))
    (tty-color-clear)
    (while colors
      (tty-color-define (car color) (cadr color) (cddr color))
      (setq colors (cdr colors) color (car colors))))
  ;; Modifying color mappings means realized faces don't
  ;; use the right colors, so clear them.
  (clear-face-cache)
  ;; Now set up some additional faces.
  (msdos-face-setup)
  ;; Set up the initial frame.
  (msdos-setup-initial-frame)
  ;; Help echo is displayed in the echo area.
  (setq show-help-function 'msdos-show-help)
  ;; We want to delay the codepage-related setup until after user's
  ;; .emacs is processed, because people might define their
  ;; `dos-codepage-setup-hook' there.
  (add-hook 'after-init-hook 'dos-codepage-setup)
  ;; In multibyte mode, we want unibyte buffers to be displayed
  ;; using the terminal coding system, so that they display
  ;; correctly on the DOS terminal; in unibyte mode we want to see
  ;; all 8-bit characters verbatim.  In both cases, we want the
  ;; entire range of 8-bit characters to arrive at our display code
  ;; verbatim.
  (standard-display-8bit 127 255)
  ;; We are fast enough to make this optimization unnecessary.
  (setq split-window-keep-point t)
  ;; Arrange for the kill and yank functions to set and check the
  ;; clipboard.
  (setq interprogram-cut-function 'x-select-text)
  (setq interprogram-paste-function 'x-get-selection-value)
  (menu-bar-enable-clipboard)
  (run-hooks 'terminal-init-msdos-hook))

;; frame-creation-function-alist is examined by frame.el:make-frame.
(add-to-list 'frame-creation-function-alist
	     '(pc . msdos-create-frame-with-faces))
;; window-system-initialization-alist is examined by startup.el:command-line.
(add-to-list 'window-system-initialization-alist
	     '(pc . msdos-initialize-window-system))
;; We don't need anything beyond tty-handle-args for handling
;; command-line argument; see startup.el.
(add-to-list 'handle-args-function-alist '(pc . tty-handle-args))

;; ---------------------------------------------------------------------------

(provide 'pc-win)

;;; pc-win.el ends here
