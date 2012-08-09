;;; artist.el --- draw ascii graphics with your mouse

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author:       Tomas Abrahamsson <tab@lysator.liu.se>
;; Maintainer:   Tomas Abrahamsson <tab@lysator.liu.se>
;; Keywords:     mouse
;; Version:	 1.2.6
;; Release-date: 6-Aug-2004
;; Location:     http://www.lysator.liu.se/~tab/artist/

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file on 19/3/2008, and the maintainer agreed that when a bug is filed in
;; the Emacs bug reporting system against this file, a copy of the bug
;; report be sent to the maintainer's email address.

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

;; What is artist?
;; ---------------
;;
;; Artist is an Emacs lisp package that allows you to draw lines,
;; rectangles and ellipses by using your mouse and/or keyboard.  The
;; shapes are made up with the ascii characters |, -, / and \.
;;
;; Features are:
;;
;; * Intersecting: When a `|' intersects with a `-', a `+' is
;;   drawn, like this:    |        \ /
;;                      --+--       X
;;                        |        / \
;;
;; * Rubber-banding: When drawing lines you can interactively see the
;;   result while holding the mouse button down and moving the mouse.  If
;;   your machine is not fast enough (a 386 is a bit to slow, but a
;;   pentium is well enough), you can turn this feature off.  You will
;;   then see 1's and 2's which mark the 1st and 2nd endpoint of the line
;;   you are drawing.
;;
;; * Drawing operations: The following drawing operations are implemented:
;;
;;     lines                    straight-lines
;;     rectangles               squares
;;     poly-lines               straight poly-lines
;;     ellipses                 circles
;;     text (see-thru)          text (overwrite)
;;     spray-can                setting size for spraying
;;     vaporize line            vaporize lines
;;     erase characters         erase rectangles
;;
;;   Straight lines are lines that go horizontally, vertically or
;;   diagonally.  Plain lines go in any direction.  The operations in
;;   the right column are accessed by holding down the shift key while
;;   drawing.
;;
;;   It is possible to vaporize (erase) entire lines and connected lines
;;   (rectangles for example) as long as the lines being vaporized are
;;   straight and connected at their endpoints.  Vaporizing is inspired
;;   by the drawrect package by Jari Aalto <jari.aalto@poboxes.com>.
;;
;; * Flood-filling: You can fill any area with a certain character by
;;   flood-filling.
;;
;; * Cut copy and paste: You can cut, copy and paste rectangular
;;   regions.  Artist also interfaces with the rect package (this can be
;;   turned off if it causes you any trouble) so anything you cut in
;;   artist can be yanked with C-x r y and vice versa.
;;
;; * Drawing with keys: Everything you can do with the mouse, you can
;;   also do without the mouse.
;;
;; * Arrows: After having drawn a (straight) line or a (straight)
;;   poly-line, you can set arrows on the line-ends by typing < or >.
;;
;; * Aspect-ratio: You can set the variable artist-aspect-ratio to
;;   reflect the height-width ratio for the font you are using.  Squares
;;   and circles are then drawn square/round.  Note, that once your
;;   ascii-file is shown with font with a different height-width ratio,
;;   the squares won't be square and the circles won't be round.
;;
;; * Picture mode compatibility: Artist is picture mode compatible (this
;;   can be turned off).
;;
;; See the documentation for the function artist-mode for a detailed
;; description on how to use artist.
;;
;;
;; What about adding my own drawing modes?
;; ---------------------------------------
;;
;; See the short guide at the end of this file.
;; If you add a new drawing mode, send it to me, and I would gladly
;; include in the next release!

;;; Installation:

;; To use artist, put this in your .emacs:
;;
;;    (autoload 'artist-mode "artist" "Enter artist-mode" t)


;;; Requirements:

;; Artist requires Emacs 19.28 or higher.
;;
;; Artist requires the `rect' package (which comes with Emacs) to be
;; loadable, unless the variable `artist-interface-with-rect' is set
;; to nil.
;;
;; Artist also requires the Picture mode (which also comes with Emacs)
;; to be loadable, unless the variable `artist-picture-compatibility'
;; is set to nil.

;;; Known bugs:

;; The shifted operations are not available when drawing with the mouse
;; in Emacs 19.29 and 19.30.
;;
;; It is not possible to change between shifted and unshifted operation
;; while drawing with the mouse. (See the comment in the function
;; artist-shift-has-changed for further details.)


;;; ChangeLog:

;; 1.2.6	6-Aug-2004
;; New:		Coerced with the artist.el that's in Emacs-21.3.
;;              (minor editorial changes)
;;
;; 1.2.5	4-Aug-2004
;; New:		Added tool selection via the mouse-wheel
;;		Function provided by Andreas Leue <al@sphenon.de>
;;
;; 1.2.4	25-Oct-2001
;; Bugfix:	Some operations (the edit menu) got hidden
;; Bugfix:      The first arrow for poly-lines was always pointing
;;              to the right
;; Changed:	Updated with changes made for Emacs 21.1
;;
;; 1.2.3	20-Nov-2000
;; Bugfix:	Autoload cookie corrected
;;
;; 1.2.2	19-Nov-2000
;; Changed:	More documentation fixes.
;; Bugfix:	The arrow characters (`artist-arrows'), which
;;              got wrong in 1.1, are now corrected.
;;
;; 1.2.1	15-Nov-2000
;; New:		Documentation fixes.
;; Bugfix:	Sets next-line-add-newlines to t while in artist-mode.
;;		Drawing with keys was confusing without this fix, if
;;		next-line-add-newlines was set to nil.
;;		Thanks to Tatsuo Furukawa <tatsuo@kobe.hp.com> for this.
;;
;; 1.2		22-Oct-2000
;; New:		Updated to work with Emacs 21
;;
;; 1.1		15-Aug-2000
;; Bugfix:	Cursor follows mouse pointer more closely.
;; New:		Works with Emacs 20.x
;; New:		Variables are customizable
;;
;; 1.1-beta1    21-Apr-1998
;; New:		Spray-can (Utterly useless, I believe, but it was fun
;;		to implement :-) after an idea by Karl-Johan Karlsson
;;		<kj@lysator.liu.se>.
;; New:		Freehand drawing (with pen).
;; New:		Vaporizing lines.
;; New:		Text-rendering using figlet.
;; New:		Picture mode compatibility.
;; Changed:	All Artist keys now uses the prefix C-c C-a not to conflict
;;		with Picture mode.
;; Bugfix:	No longer leaves traces of lines when rubberbanding
;;              if the buffer auto-scrolls.
;; Bugfix:	Infinite loop sometimes when rubberbanding was turned
;;		off.
;;
;; 1.0          01-Mar-1998
;; First official release.

;;; Code:

;; Variables

(defconst artist-version "1.2.6")
(defconst artist-maintainer-address "tab@lysator.liu.se")

(defvar x-pointer-crosshair)

;; User options
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup artist nil
  "Customization of the Artist mode."
  :group 'mouse)

(defgroup artist-text nil
  "Customization of the text rendering."
  :group 'artist)

(defcustom artist-rubber-banding t
  "Interactively do rubber-banding when non-nil."
  :group 'artist
  :type 'boolean)

(defcustom artist-first-char ?1
  "Character to set at first point when not rubber-banding."
  :group 'artist
  :type 'character)

(defcustom artist-second-char ?2
  "Character to set at second point when not rubber-banding."
  :group 'artist
  :type 'character)

(defcustom artist-interface-with-rect t
  "Whether to interface with the rect package or not.

Interfacing to the rect package means that the Copy and Paste operations
will use the rectangle buffer when accessing the copied area.  This means
that you can insert a rectangle which is copied using the artist package
and vice versa.

If this causes any problem for you (for example, if the implementation of
the rectangle package changes), you can set this variable to nil, and the
artist package will use its own copy buffer."
  :group 'artist
  :type 'boolean)

(defvar artist-arrows [ ?> nil ?v ?L ?< nil ?^ nil ]
  ;; This is a defvar, not a defcustom, since the custom
  ;; package shows vectors of characters as a vector of integers,
  ;; which is confusing
  "A vector of characters to use as arrows.

The vector is 8 elements long and contains a character for each
direction, or nil if there is no suitable character to use for arrow
in that direction.

The directions are as follows:

			  5  6  7
			   \\ | /
			 4 - * - 0
			   / | \\
			  3  2  1")

(defcustom artist-aspect-ratio 1
  "Defines the character height-to-width aspect ratio.
This is used when drawing squares and circles."
  :group 'artist
  :type 'number)

(defcustom artist-trim-line-endings t
  "Whether or not to remove white-space at end of lines.

If non-nil, line-endings are trimmed (that is, extraneous white-space
at the end of the line is removed) when the shape is drawn."
  :group 'artist
  :type 'boolean)


(defcustom artist-flood-fill-right-border 'window-width
  "Right edge definition, used when flood-filling.

When flood-filling, if the area is not closed off to the right, then
flood-filling will fill no more to the right than specified by this
variable.  This limit is called the fill-border."
  :group 'artist
  :type '(choice (const :tag "limited to window" window-width)
		 (const :tag "limited to value of `fill-column'" fill-column)))

(defcustom artist-flood-fill-show-incrementally t
  "Whether or not to incrementally update display when flood-filling.

If non-nil, incrementally update display when flood-filling.
If set to non-nil, this currently implies discarding any input events
during the flood-fill."
  :group 'artist
  :type 'boolean)


(defcustom artist-ellipse-right-char ?\)
  "Character to use at the rightmost position when drawing narrow ellipses.

In this figure, it is the right parenthesis (the ``)'' character):
             -----
            (     )
             -----"
  :group 'artist
  :type 'character)


(defcustom artist-ellipse-left-char ?\(
  "Character to use at the leftmost position when drawing narrow ellipses.

In this figure, it is the left parenthesis (the ``('' character):
             -----
            (     )
             -----"
  :group 'artist
  :type 'character)

(defcustom artist-picture-compatibility t
  "Whether or not picture mode compatibility is on."
  :group 'artist
  :type 'boolean)




(defcustom artist-vaporize-fuzziness 1
  "How to vaporize lines that are cut off.

Accept this many characters cutting off a line and still treat
it as one line.
Example:
 If `artist-vaporize-fuzziness' is 2, then those will be recognized as
 lines from A to B (provided you start vaporizing them at the ``*''):
                         /
            A----*------/-----------B
                      \\/
            A----*----/\\------------B
                     /  \\

 but this one won't, since it is cut off by more than 2 characters:
                      \\/ /
            A----*----/\\/----------B
                     / /\\
 (in fact, only the left part [between the A and the leftmost ``/''
 crossing the line] will be vaporized)."
  :group 'artist
  :type 'integer)


(defvar artist-pointer-shape (if (eq window-system 'x) x-pointer-crosshair nil)
  "*If in X Windows, use this pointer shape while drawing with the mouse.")


(defcustom artist-text-renderer-function 'artist-figlet
  "Function for doing text rendering."
  :group 'artist-text
  :type 'symbol)
(defvaralias 'artist-text-renderer 'artist-text-renderer-function)


(defcustom artist-figlet-program "figlet"
  "Program to run for `figlet'."
  :group 'artist-text
  :type 'string)


(defcustom artist-figlet-default-font "standard"
  "Default font for `figlet'."
  :group 'artist-text
  :type 'string)


(defcustom artist-figlet-list-fonts-command
  ;; list files ending with *.flf in any directory printed by the
  ;; ``figlet -I2'' command. I think this will not produce more than
  ;; one directory, but it never hurts to be on the safe side...
  "for dir in `figlet -I2`; do cd $dir; ls *.flf; done"
  "Command to run to get list of available fonts."
  :group 'artist-text
  :type 'string)


(defcustom artist-spray-interval 0.2
  "Number of seconds between repeated spraying."
  :group 'artist
  :type 'number)


(defcustom artist-spray-radius 4
  "Size of the area for spraying."
  :group 'artist
  :type 'integer)


(defvar artist-spray-chars '(?\s ?. ?- ?+ ?m ?% ?* ?#)
  ;; This is a defvar, not a defcustom, since the custom
  ;; package shows lists of characters as a lists of integers,
  ;; which is confusing
  "*Characters (``color'') to use when spraying.
They should be ordered from the ``lightest'' to the ``heaviest''
since spraying replaces a light character with the next heavier one.")


(defvar artist-spray-new-char ?.
  "*Initial character to use when spraying.
This character is used if spraying upon a character that is not in
`artist-spray-chars'.  The character defined by this variable should
be in `artist-spray-chars', or spraying will behave strangely.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; End of user options


;; Internal variables
;;
(defvar artist-mode nil
  "Non-nil to enable `artist-mode' and nil to disable.")
(make-variable-buffer-local 'artist-mode)

(defvar artist-mode-name " Artist"
  "Name of Artist mode beginning with a space (appears in the mode-line).")

(defvar artist-curr-go 'pen-line
  "Current selected graphics operation.")
(make-variable-buffer-local 'artist-curr-go)

(defvar artist-line-char-set nil
  "Boolean to tell whether user has set some char to use when drawing lines.")
(make-variable-buffer-local 'artist-line-char-set)

(defvar artist-line-char nil
  "Char to use when drawing lines.")
(make-variable-buffer-local 'artist-line-char)

(defvar artist-fill-char-set nil
  "Boolean to tell whether user has set some char to use when filling.")
(make-variable-buffer-local 'artist-fill-char-set)

(defvar artist-fill-char nil
  "Char to use when filling.")
(make-variable-buffer-local 'artist-fill-char)

(defvar artist-erase-char ?\s
  "Char to use when erasing.")
(make-variable-buffer-local 'artist-erase-char)

(defvar artist-default-fill-char ?.
  "Char to use when a fill-char is required but none is set.")
(make-variable-buffer-local 'artist-default-fill-char)

; This variable is not buffer local
(defvar artist-copy-buffer nil
  "Copy buffer.")

(defvar artist-draw-region-min-y 0
  "Line-number for top-most visited line for draw operation.")
(make-variable-buffer-local 'artist-draw-region-min-y)

(defvar artist-draw-region-max-y 0
  "Line-number for bottom-most visited line for draw operation.")
(make-variable-buffer-local 'artist-draw-region-max-y)

(defvar artist-borderless-shapes nil
  "When non-nil, draw shapes without border.
The fill char is used instead, if it is set.")
(make-variable-buffer-local 'artist-borderless-shapes)

(defvar artist-prev-next-op-alist nil
  "Assoc list for looking up next and/or previous draw operation.
The structure is as follows:  (OP . (PREV-OP . NEXT-OP))
where the elements are as follows:
* OP is an atom: the KEY-SYMBOL in the `artist-mt' structure
* PREV-OP and NEXT-OP are strings: the KEYWORD in the `artist-mt' structure

This variable is initialized by the `artist-make-prev-next-op-alist' function.")

(eval-when-compile
  ;; Make rect available at compile-time
  (require 'rect)			; for interfacing with rect
  (require 'reporter)			; the bug-reporting tool
  (require 'picture))			; picture mode compatibility

(if artist-interface-with-rect
    (require 'rect))

(require 'reporter)

(if artist-picture-compatibility
    (require 'picture))

;; Variables that are made local in artist-mode-init
(defvar artist-key-is-drawing nil)
(defvar artist-key-endpoint1 nil)
(defvar artist-key-poly-point-list nil)
(defvar artist-key-shape nil)
(defvar artist-key-draw-how nil)
(defvar artist-popup-menu-table nil)
(defvar artist-key-compl-table nil)
(defvar artist-rb-save-data nil)
(defvar artist-arrow-point-1 nil)
(defvar artist-arrow-point-2 nil)

(defvar artist-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [spray-chars]
      '(menu-item "Characters for Spray" artist-select-spray-chars
		  :help "Choose characters for sprayed by the spray-can"))
    (define-key map [borders]
      '(menu-item "Draw Shape Borders" artist-toggle-borderless-shapes
		  :help "Toggle whether shapes are drawn with borders"
		  :button (:toggle . (not artist-borderless-shapes))))
    (define-key map [trimming]
      '(menu-item "Trim Line Endings" artist-toggle-trim-line-endings
		  :help "Toggle trimming of line-endings"
		  :button (:toggle . artist-trim-line-endings)))
    (define-key map [rubber-band]
      '(menu-item "Rubber-banding" artist-toggle-rubber-banding
		  :help "Toggle rubber-banding"
		  :button (:toggle . artist-rubber-banding)))
    (define-key map [set-erase]
      '(menu-item "Character to Erase..." artist-select-erase-char
		  :help "Choose a specific character to erase"))
    (define-key map [set-line]
      '(menu-item "Character for Line..." artist-select-line-char
		  :help "Choose the character to insert when drawing lines"))
    (define-key map [set-fill]
      '(menu-item "Character for Fill..." artist-select-fill-char
		  :help "Choose the character to insert when filling in shapes"))
    (define-key map [artist-separator] '(menu-item "--"))
    (dolist (op '(("Vaporize" artist-select-op-vaporize-lines vaporize-lines)
		  ("Erase" artist-select-op-erase-rectangle erase-rect)
		  ("Spray-can" artist-select-op-spray-set-size spray-get-size)
		  ("Text" artist-select-op-text-overwrite text-ovwrt)
		  ("Ellipse" artist-select-op-circle circle)
		  ("Poly-line" artist-select-op-straight-poly-line spolyline)
		  ("Square" artist-select-op-square square)
		  ("Rectangle" artist-select-op-rectangle rectangle)
    		  ("Line" artist-select-op-straight-line s-line)
    		  ("Pen" artist-select-op-pen-line pen-line)))
      (define-key map (vector (nth 2 op))
    	`(menu-item ,(nth 0 op)
    		    ,(nth 1 op)
    		    :help ,(format "Draw using the %s style" (nth 0 op))
    		    :button (:radio . (eq artist-curr-go ',(nth 2 op))))))
    map))

(defvar artist-mode-map
  (let ((map (make-sparse-keymap)))
    (setq artist-mode-map (make-sparse-keymap))
    (define-key map [down-mouse-1] 'artist-down-mouse-1)
    (define-key map [S-down-mouse-1] 'artist-down-mouse-1)
    (define-key map [down-mouse-2] 'artist-mouse-choose-operation)
    (define-key map [S-down-mouse-2] 'artist-mouse-choose-operation)
    (define-key map [down-mouse-3] 'artist-down-mouse-3)
    (define-key map [S-down-mouse-3] 'artist-down-mouse-3)
    (define-key map [C-mouse-4] 'artist-select-prev-op-in-list)
    (define-key map [C-mouse-5] 'artist-select-next-op-in-list)
    (define-key map "\r" 'artist-key-set-point) ; return
    (define-key map [up] 'artist-previous-line)
    (define-key map "\C-p" 'artist-previous-line)
    (define-key map [down] 'artist-next-line)
    (define-key map "\C-n" 'artist-next-line)
    (define-key map [left] 'artist-backward-char)
    (define-key map "\C-b" 'artist-backward-char)
    (define-key map [right] 'artist-forward-char)
    (define-key map "\C-f" 'artist-forward-char)
    (define-key map "<" 'artist-toggle-first-arrow)
    (define-key map ">" 'artist-toggle-second-arrow)
    (define-key map "\C-c\C-a\C-e" 'artist-select-erase-char)
    (define-key map "\C-c\C-a\C-f" 'artist-select-fill-char)
    (define-key map "\C-c\C-a\C-l" 'artist-select-line-char)
    (define-key map "\C-c\C-a\C-o" 'artist-select-operation)
    (define-key map "\C-c\C-a\C-r" 'artist-toggle-rubber-banding)
    (define-key map "\C-c\C-a\C-t" 'artist-toggle-trim-line-endings)
    (define-key map "\C-c\C-a\C-s" 'artist-toggle-borderless-shapes)
    (define-key map "\C-c\C-c"     'artist-mode-off)
    (define-key map "\C-c\C-al"    'artist-select-op-line)
    (define-key map "\C-c\C-aL"    'artist-select-op-straight-line)
    (define-key map "\C-c\C-ar"    'artist-select-op-rectangle)
    (define-key map "\C-c\C-aR"    'artist-select-op-square)
    (define-key map "\C-c\C-as"    'artist-select-op-square)
    (define-key map "\C-c\C-ap"    'artist-select-op-poly-line)
    (define-key map "\C-c\C-aP"    'artist-select-op-straight-poly-line)
    (define-key map "\C-c\C-ae"    'artist-select-op-ellipse)
    (define-key map "\C-c\C-ac"    'artist-select-op-circle)
    (define-key map "\C-c\C-at"    'artist-select-op-text-see-thru)
    (define-key map "\C-c\C-aT"    'artist-select-op-text-overwrite)
    (define-key map "\C-c\C-aS"    'artist-select-op-spray-can)
    (define-key map "\C-c\C-az"    'artist-select-op-spray-set-size)
    (define-key map "\C-c\C-a\C-d" 'artist-select-op-erase-char)
    (define-key map "\C-c\C-aE"    'artist-select-op-erase-rectangle)
    (define-key map "\C-c\C-av"    'artist-select-op-vaporize-line)
    (define-key map "\C-c\C-aV"    'artist-select-op-vaporize-lines)
    (define-key map "\C-c\C-a\C-k" 'artist-select-op-cut-rectangle)
    (define-key map "\C-c\C-a\M-w" 'artist-select-op-copy-rectangle)
    (define-key map "\C-c\C-a\C-y" 'artist-select-op-paste)
    (define-key map "\C-c\C-af"    'artist-select-op-flood-fill)
    (define-key map "\C-c\C-a\C-b" 'artist-submit-bug-report)
    (define-key map [menu-bar artist] (cons "Artist" artist-menu-map))
    map)
  "Keymap for `artist-minor-mode'.")

(defvar artist-replacement-table (make-vector 256 0)
  "Replacement table for `artist-replace-char'.")


;;;
;;; The table of graphic operations
;;;
(defvar artist-mt
  ;; Implementation note: Maybe this should be done using a structure
  ;; in the cl package?
  ;;
  '(
    (menu
     ("Drawing"
      ((function-call
	( "Undo"		do-undo		undo))

       (separator )
       (graphics-operation
	("Pen" (("Pen" pen-char "pen-c"
		 artist-no-arrows nil
		 nil nil nil
		 artist-do-continously
		 artist-pen
		 (nil))
		("Pen Line" pen-line "pen-l"
		 artist-arrows artist-pen-set-arrow-points
		 artist-pen-reset-last-xy nil nil
		 artist-do-continously
		 artist-pen-line
		 (nil)))))

       (graphics-operation
	("Line" (("line" line "line"
		  artist-arrows artist-set-arrow-points-for-2points
		  nil nil nil
		  2
		  artist-draw-line
		  (artist-undraw-line
		   artist-nil nil))
		 ("straight line" s-line "sline"
		  artist-arrows artist-set-arrow-points-for-2points
		  nil nil nil
		  2
		  artist-draw-sline
		  (artist-undraw-sline
		   artist-nil nil)))))

       (graphics-operation
	("Rectangle" (("rectangle" rect "rect"
		       artist-no-arrows nil
		       nil nil nil
		       2
		       artist-draw-rect
		       (artist-undraw-rect
			artist-t-if-fill-char-set artist-fill-rect))
		      ("square" square "square"
		       artist-no-arrows nil
		       nil nil nil
		       2
		       artist-draw-square
		       (artist-undraw-square
			artist-t-if-fill-char-set artist-fill-square)))))

       (graphics-operation
	("Poly-line" (("poly-line" polyline "poly"
		       artist-arrows artist-set-arrow-points-for-poly
		       nil nil nil
		       artist-do-poly
		       artist-draw-line
		       (artist-undraw-line
			artist-nil nil))
		      ("straight poly-line" spolyline "s-poly"
		       artist-arrows artist-set-arrow-points-for-poly
		       nil nil nil
		       artist-do-poly
		       artist-draw-sline
		       (artist-undraw-sline
			artist-nil nil)))))

       (graphics-operation
	("Ellipse" (("ellipse" ellipse "ellipse"
		     artist-no-arrows nil
		     nil nil nil
		     2
		     artist-draw-ellipse
		     (artist-undraw-ellipse
		      artist-t-if-fill-char-set artist-fill-ellipse))
		    ("circle" circle "circle"
		     artist-no-arrows nil
		     nil nil nil
		     2
		     artist-draw-circle
		     (artist-undraw-circle
		      artist-t-if-fill-char-set artist-fill-circle)))))

       (graphics-operation
	("Text" (("text see-thru" text-thru "text-thru"
		   artist-no-arrows nil
		   nil nil nil
		   1
		   artist-text-see-thru
		   nil)
		  ("text overwrite" text-ovwrt "text-ovwrt"
		   artist-no-arrows nil
		   nil nil nil
		   1
		   artist-text-overwrite
		   nil))))

       (graphics-operation
	("Spray-can" (("spray-can" spray-can "spray-can"
		       artist-no-arrows nil
		       nil nil nil
		       artist-do-continously
		       artist-spray
		       (artist-spray-get-interval))
		      ("spray set size" spray-get-size "spray-size"
		       artist-no-arrows nil
		       nil artist-spray-clear-circle artist-spray-set-radius
		       2
		       artist-draw-circle
		       (artist-undraw-circle
			artist-nil nil)))))

       (graphics-operation
	("Erase" (("erase char" erase-char "erase-c"
		   artist-no-arrows nil
		   nil nil nil
		   artist-do-continously
		   artist-erase-char
		   (nil))
		  ("erase rectangle" erase-rect "erase-r"
		   artist-no-arrows nil
		   nil nil nil
		   2
		   artist-draw-rect
		   (artist-undraw-rect
		    artist-t artist-erase-rect)))))

       (graphics-operation
	("Vaporize" (("vaporize line" vaporize-line "vaporize-1"
		   artist-no-arrows nil
		   nil nil nil
		   1
		   artist-vaporize-line
		   nil)
		  ("vaporize lines" vaporize-lines "vaporize-n"
		   artist-no-arrows nil
		   nil nil nil
		   1
		   artist-vaporize-lines
		   nil)))))))

    (menu
     ("Edit"
      ((graphics-operation
	("Cut" (("cut rectangle" cut-r "cut-r"
		 artist-no-arrows nil
		 nil nil nil
		 2
		 artist-draw-rect
		 (artist-undraw-rect
		  artist-t artist-cut-rect))
		("cut square" cut-s "cut-s"
		 artist-no-arrows nil
		 nil nil nil
		 2
		 artist-draw-square
		 (artist-undraw-square
		  artist-t artist-cut-square)))))

       (graphics-operation
	("Copy" (("copy rectangle" copy-r "copy-r"
		  artist-no-arrows nil
		  nil nil nil
		  2
		  artist-draw-rect
		  (artist-undraw-rect
		   artist-t artist-copy-rect))
		 ("copy square" copy-s "copy-s"
		  artist-no-arrows nil
		  nil nil nil
		  2
		  artist-draw-square
		  (artist-undraw-square
		   artist-t artist-copy-square)))))

       (graphics-operation
	("Paste" (("paste" paste "paste"
		   artist-no-arrows nil
		   nil nil nil
		   1
		   artist-paste
		   nil)
		  ("paste" paste "paste"
		   artist-no-arrows nil
		   nil nil nil
		   1
		   artist-paste
		   nil))))

       (graphics-operation
	("Flood-fill" (("flood-fill" flood-fill "flood"
			artist-no-arrows nil
			nil nil nil
			1
			artist-flood-fill
			nil)
		       ("flood-fill" flood-fill "flood"
			artist-no-arrows nil
			nil nil nil
			1
			artist-flood-fill
			nil)))))))

    (menu
     ("Settings"
      ((function-call
	("Set Fill"	set-fill	artist-select-fill-char))

       (function-call
	("Set Line"	set-line	artist-select-line-char))

       (function-call
	("Set Erase"	set-erase	artist-select-erase-char))

       (function-call
	("Rubber-banding" rubber-band	artist-toggle-rubber-banding))

       (function-call
	("Trimming"	trimming	artist-toggle-trim-line-endings))

       (function-call
	("Borders"	borders		artist-toggle-borderless-shapes))

       (function-call
	("Spray-chars"	spray-chars	artist-select-spray-chars)))))

  ) ;; end of list

  "Master Table for `artist-mode'.
This table is primarily a table over the different graphics operations
available in Artist mode, but it also holds layout information for the
popup menu.

The master table is a list of table elements.  The elements of this table
have the layout

  (TAG INFO-PART)

There are three kinds of TAG:

  `menu'                -- a sub-menu
  `separator'           -- produce a separator in the popup menu
  `function-call'       -- call a function
  `graphics-operation'  -- a graphics operation

The layout of the INFO-PART for `menu' is

  (TITLE ((TAG-1 INFO-PART-1) (TAG-2 INFO-PART-2) ...))

TITLE is the title of the submenu; this is followed by a list of
menu items, each on the general form (TAG INFO-PART).


The layout of the INFO-PART for `separator' is empty and not used.


This is the layout of the INFO-PART for `function-call':

  (KEYWORD SYMBOL FN)

KEYWORD is a string naming the operation, and appears in the popup menu.
SYMBOL is the symbol for the operations.
FN is the function performing the operation.  This function
  is called with no arguments.  Its return value is ignored.


The layout of the INFO-PART for `graphics-operation' is

  (TITLE (UNSHIFTED SHIFTED))

TITLE is the title that appears in the popup menu.  UNSHIFTED
and SHIFTED specify for unshifted and shifted operation.  Both
have the form

  (KEYWORD KEY-SYMBOL MODE-LINE ARROW-PRED ARROW-SET-FN
   INIT-FN PREP-FILL-FN EXIT-FN DRAW-HOW DRAW-FN EXTRA-DRAW-INFO)

KEYWORD is a string specifying the name of the shape to draw.
  This is used when selecting drawing operation.
KEY-SYMBOL is the key which is used when looking up members
  through the functions `artist-go-get-MEMBER-from-symbol'
  and `artist-fc-get-MEMBER-from-symbol'.
MODE-LINE is a string that appears in the mode-line when drawing
  the shape.
ARROW-PRED is a function that is called to find out if the shape
  can have arrows.  The function is called with no arguments and
  must return nil or t.
ARROW-SET-FN is a function that is called to set arrow end-points.
  Arguments and return values for this function are described below.
INIT-FN is, if non-nil, a function that is called when the first
  point of the shape is set.  Arguments and return values for
  this function are described below.
PREP-FILL-FN is, if non-nil, a function that is called after
  the last point is set, but before the filling is done.
  Arguments and return values for this function are described below.
EXIT-FN is, if non-nil, a function that is called after filling
  is done.  Arguments and return values for this function are
  described below.
DRAW-HOW defines the kind of shape.  The kinds of shapes are:
  `artist-do-continously'  -- Do drawing operation continuously,
                              as long as the mouse button is held down.
  `artist-do-poly'         -- Do drawing operation many times.
  1                        -- Do drawing operation only once.
  2                        -- The drawing operation requires two points.
DRAW-FN is the function to call for drawing.  Arguments and
  return values for this function are described below.
EXTRA-DRAW-INFO the layout of this depends on the value of DRAW-HOW:
  If DRAW-HOW is `artist-do-continously':

    (INTERVAL-FN)

    INTERVAL-FN is, if non-nil, a function to call for getting
      an interval between repeated calls to the DRAW-FN.
      This function is called with no arguments and must
      return a number, the interval in seconds.
      If nil, calls to DRAW-FN are done only when the mouse
      or cursor is moved.

  If DRAW-HOW is either `artist-do-poly' or 2:

    (UNDRAW-FN FILL-PRED FILL-FN)

    UNDRAW-FN is a function to call for undrawing the shape.
      Arguments and return values for this function are
      described below.
    FILL-PRED is a function that is called to find out if the shape
      can have arrows.  The function must take no arguments and
      return nil or t.
    FILL-FN  is a function to call for filling the shape.
      Arguments and return values for this function are
      described below.

  If DRAW-HOW is 1:

    ()

Note! All symbols and keywords (both in the `function-call' INFO-PART
      as well as in the `graphics-operation' INFO-PART) must be unique.

The following table describe function arguments and return value
for different functions and DRAW-HOWs.

If DRAW-HOW is either `artist-do-continously' or 1:

  INIT-FN       X Y ==> ignored
  PREP-FILL-FN  X Y ==> ignored
  EXIT-FN       X Y ==> ignored
  ARROW-SET-FN  X Y ==> ignored
  DRAW-FN       X Y ==> ignored

If DRAW-HOW is 2:

  INIT-FN       X1 Y1  ==> ignored
  PREP-FILL-FN  X1 Y1 X2 Y2 ==> ignored
  EXIT-FN       X1 Y1 X2 Y2 ==> ignored
  ARROW-SET-FN  X1 Y1 X2 Y2 ==> ignored
  DRAW-FN       X1 Y1 X2 Y2 ==> (ENDPOINT-1 ENDPOINT-2 SHAPE)
  UNDRAW-FN     (ENDPOINT-1 ENDPOINT-2 SHAPE) ==> ignored
  FILL-FN	 (ENDPOINT-1 ENDPOINT-2 SHAPE) X1 Y1 X2 Y2 ==> ignored

  ENDPOINT-1 and ENDPOINT-2 are endpoints which are created with
  `artist-make-endpoint'
  SHAPE is an opaque structure, created by the DRAW-FN and intended
  to be used only by the UNDRAW-FN.

If DRAW-HOW is `artist-do-poly':

  INIT-FN       X1 Y1
  PREP-FILL-FN  POINT-LIST
  ARROW-SET-FN  POINT-LIST
  EXIT-FN       POINT-LIST
  DRAW-FN       X-LAST Y-LAST X-NEW Y-NEW ==> (ENDPOINT-1 ENDPOINT-2 SHAPE)
  UNDRAW-FN     (ENDPOINT-1 ENDPOINT-2 SHAPE)
  FILL-FN       POINT-LIST

  ENDPOINT-1 and ENDPOINT-2 are endpoints which are created with
  `artist-make-endpoint'.
  SHAPE is an opaque structure, created by the DRAW-FN and intended
  to be used only by the UNDRAW-FN.
  POINT-LIST is a list of vectors [X Y].")


;;
;; Accessors for the master table
;;

(defun artist-mt-get-tag (element)
  "Retrieve the tag component from the master table ELEMENT."
  (elt element 0))

(defun artist-mt-get-info-part (element)
  "Retrieve the info part component from the master table ELEMENT."
  (elt element 1))

;; For the 'graphics-operation info-parts
;;
(defsubst artist-go-get-desc (info-part)
  "Retrieve the description component from a graphics operation INFO-PART."
  (elt info-part 0))

(defsubst artist-go-get-unshifted (info-part)
  "Retrieve the unshifted info from a graphics operation INFO-PART."
  (elt (elt info-part 1) 0))

(defsubst artist-go-get-shifted (info-part)
  "Retrieve the shifted info from a graphics operation INFO-PART."
  (elt (elt info-part 1) 1))

(defsubst artist-go-get-keyword (info-variant-part)
  "Retrieve the keyword component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 0))

(defsubst artist-go-get-symbol (info-variant-part)
  "Retrieve the symbol component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 1))

(defsubst artist-go-get-mode-line (info-variant-part)
  "Retrieve the mode line component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 2))

(defsubst artist-go-get-arrow-pred (info-variant-part)
  "Retrieve the arrow predicate component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 3))

(defsubst artist-go-get-arrow-set-fn (info-variant-part)
  "Retrieve the arrow set component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 4))

(defsubst artist-go-get-init-fn (info-variant-part)
  "Retrieve the init function component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 5))

(defsubst artist-go-get-prep-fill-fn (info-variant-part)
  "Retrieve the fill preparation function component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 6))

(defsubst artist-go-get-exit-fn (info-variant-part)
  "Retrieve the exit component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 7))

(defsubst artist-go-get-draw-how (info-variant-part)
  "Retrieve the draw how component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 8))

(defsubst artist-go-get-draw-fn (info-variant-part)
  "Retrieve the draw function component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part."
  (elt info-variant-part 9))

(defsubst artist-go-get-undraw-fn (info-variant-part)
  "Retrieve the undraw function component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part.
This interval function component is available only if the `draw-how'
component is other than `artist-do-continously' or 1."
  (elt (elt info-variant-part 10) 0))

(defsubst artist-go-get-interval-fn (info-variant-part)
  "Retrieve the interval function component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part.
This interval function component is available only if the `draw-how'
component is `artist-do-continously'."
  (elt (elt info-variant-part 10) 0))

(defsubst artist-go-get-fill-pred (info-variant-part)
  "Retrieve the fill predicate component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part.
This interval function component is available only if the `draw-how'
component is other than `artist-do-continously' or 1."
  (elt (elt info-variant-part 10) 1))

(defsubst artist-go-get-fill-fn (info-variant-part)
  "Retrieve the fill function component from an INFO-VARIANT-PART.
An INFO-VARIANT-PART is the shifted or unshifted info from a info-part.
This interval function component is available only if the `draw-how'
component is other than `artist-do-continously' or 1."
  (elt (elt info-variant-part 10) 2))

;; For the 'function-call info-parts
;;
(defsubst artist-fc-get-keyword (info-part)
  "Retrieve the keyword component from a graphics operation INFO-PART."
  (elt info-part 0))

(defsubst artist-fc-get-symbol (info-part)
  "Retrieve the symbol component from a graphics operation INFO-PART."
  (elt info-part 1))

(defsubst artist-fc-get-fn (info-part)
  "Retrieve the function component from a graphics operation INFO-PART."
  (elt info-part 2))

;; For the 'menu info-parts
;;
(defsubst artist-mn-get-title (info-part)
  "Retrieve the title component from a graphics operation INFO-PART."
  (elt info-part 0))

(defsubst artist-mn-get-items (info-part)
  "Retrieve the items component from a graphics operation INFO-PART."
  (elt info-part 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mouse wheel cyclic operation selection

(defun artist-get-last-non-nil-op (op-list &optional last-non-nil)
  "Find the last non-nil draw operation in OP-LIST.
Optional LAST-NON-NIL will be returned if OP-LIST is nil."
  (if op-list
      (artist-get-last-non-nil-op (cdr op-list)
				  (or (car (car op-list)) last-non-nil))
    last-non-nil))

(defun artist-get-first-non-nil-op (op-list)
  "Find the first non-nil draw operation in OP-LIST."
  (or (car (car op-list)) (artist-get-first-non-nil-op (cdr op-list))))

(defun artist-is-in-op-list-p (op op-list)
  "Check whether OP is in OP-LIST."
  (and op-list
       (or (and (car (car op-list)) (string= op (car (car op-list))))
	   (artist-is-in-op-list-p op (cdr op-list)))))

(defun artist-make-prev-next-op-alist (op-list
				       &optional
				       last-non-nil-arg first-non-nil-arg
				       prev-entry prev-op-arg)
  "Build an assoc-list of OP-LIST.
The arguments LAST-NON-NIL-ARG, FIRST-NON-NIL-ARG, PREV-ENTRY and
PREV-OP-ARG are used when invoked recursively during the build-up."
  (let* ((last-non-nil  (or last-non-nil-arg
			    (artist-get-last-non-nil-op
			     artist-key-compl-table)))
         (first-non-nil (or first-non-nil-arg
			    (artist-get-first-non-nil-op
			     artist-key-compl-table)))
         (prev-op       (or prev-op-arg last-non-nil))
         (op            (car (car op-list)))
         (opsym         (artist-mt-get-symbol-from-keyword op))
         (entry         (cons opsym (cons prev-op nil))))
    (if (or (and op-list (not op))
	    (artist-is-in-op-list-p op (cdr op-list)))
        (artist-make-prev-next-op-alist (cdr op-list)
					last-non-nil first-non-nil
					prev-entry prev-op)
      (if prev-entry (setcdr (cdr prev-entry) op))
      (if op-list
          (cons entry (artist-make-prev-next-op-alist
		       (cdr op-list)
		       last-non-nil first-non-nil
		       entry op))
        (progn (setcdr (cdr prev-entry) first-non-nil) nil)))))

(defun artist-select-next-op-in-list ()
  "Cyclically select next drawing mode operation."
  (interactive)
  (let ((next-op (cdr (cdr (assoc artist-curr-go artist-prev-next-op-alist)))))
    (artist-select-operation next-op)
    (message "%s" next-op)))

(defun artist-select-prev-op-in-list ()
  "Cyclically select previous drawing mode operation."
  (interactive)
  (let ((prev-op (car (cdr (assoc artist-curr-go artist-prev-next-op-alist)))))
    (artist-select-operation prev-op)
    (message "%s" prev-op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---------------------------------
;;; The artist-mode
;;; ---------------------------------

;;;###autoload
(defun artist-mode (&optional state)
  "Toggle Artist mode.
With argument STATE, turn Artist mode on if STATE is positive.
Artist lets you draw lines, squares, rectangles and poly-lines,
ellipses and circles with your mouse and/or keyboard.

How to quit Artist mode

 Type \\[artist-mode-off] to quit artist-mode.


How to submit a bug report

 Type \\[artist-submit-bug-report] to submit a bug report.


Drawing with the mouse:

 mouse-2
 shift mouse-2	Pops up a menu where you can select what to draw with
		mouse-1, and where you can do some settings (described
		below).

 mouse-1
 shift mouse-1	Draws lines, rectangles or poly-lines, erases, cuts, copies
		or pastes:

		Operation	Not shifted		  Shifted
		--------------------------------------------------------------
                Pen             fill-char at point        line from last point
                                                          to new point
		--------------------------------------------------------------
		Line		Line in any direction	  Straight line
		--------------------------------------------------------------
		Rectangle	Rectangle		  Square
		--------------------------------------------------------------
		Poly-line	Poly-line in any dir	  Straight poly-lines
		--------------------------------------------------------------
		Ellipses	Ellipses		  Circles
		--------------------------------------------------------------
		Text		Text (see thru)		  Text (overwrite)
		--------------------------------------------------------------
		Spray-can	Spray-can		  Set size for spray
		--------------------------------------------------------------
		Erase		Erase character		  Erase rectangle
		--------------------------------------------------------------
		Vaporize	Erase single line	  Erase connected
							  lines
		--------------------------------------------------------------
		Cut		Cut rectangle		  Cut square
		--------------------------------------------------------------
		Copy		Copy rectangle		  Copy square
		--------------------------------------------------------------
		Paste		Paste			  Paste
		--------------------------------------------------------------
		Flood-fill	Flood-fill		  Flood-fill
		--------------------------------------------------------------

		* Straight lines can only go horizontally, vertically
		  or diagonally.

		* Poly-lines are drawn while holding mouse-1 down.  When you
		  release the button, the point is set.  If you want a segment
		  to be straight, hold down shift before pressing the
		  mouse-1 button.  Click mouse-2 or mouse-3 to stop drawing
		  poly-lines.

		* See thru for text means that text already in the buffer
		  will be visible through blanks in the text rendered, while
		  overwrite means the opposite.

		* Vaporizing connected lines only vaporizes lines whose
		  _endpoints_ are connected.  See also the variable
		  `artist-vaporize-fuzziness'.

		* Cut copies, then clears the rectangle/square.

		* When drawing lines or poly-lines, you can set arrows.
		  See below under ``Arrows'' for more info.

		* The mode line shows the currently selected drawing operation.
		  In addition, if it has an asterisk (*) at the end, you
		  are currently drawing something.

		* Be patient when flood-filling -- large areas take quite
		  some time to fill.


 mouse-3	Erases character under pointer
 shift mouse-3	Erases rectangle


Settings

 Set fill	Sets the character used when filling rectangles/squares

 Set line	Sets the character used when drawing lines

 Erase char	Sets the character used when erasing

 Rubber-banding	Toggles rubber-banding

 Trimming	Toggles trimming of line-endings (that is: when the shape
		is drawn, extraneous white-space at end of lines is removed)

 Borders        Toggles the drawing of line borders around filled shapes


Drawing with keys

 \\[artist-key-set-point]		Does one of the following:
		For lines/rectangles/squares: sets the first/second endpoint
		For poly-lines: sets a point (use C-u \\[artist-key-set-point] to set last point)
		When erase characters: toggles erasing
		When cutting/copying: Sets first/last endpoint of rect/square
		When pasting: Pastes

 \\[artist-select-operation]	Selects what to draw

 Move around with \\[artist-next-line], \\[artist-previous-line], \\[artist-forward-char] and \\[artist-backward-char].

 \\[artist-select-fill-char]	Sets the character to use when filling
 \\[artist-select-line-char]	Sets the character to use when drawing
 \\[artist-select-erase-char]	Sets the character to use when erasing
 \\[artist-toggle-rubber-banding]	Toggles rubber-banding
 \\[artist-toggle-trim-line-endings]	Toggles trimming of line-endings
 \\[artist-toggle-borderless-shapes]	Toggles borders on drawn shapes


Arrows

 \\[artist-toggle-first-arrow]		Sets/unsets an arrow at the beginning
		of the line/poly-line

 \\[artist-toggle-second-arrow]		Sets/unsets an arrow at the end
		of the line/poly-line


Selecting operation

 There are some keys for quickly selecting drawing operations:

 \\[artist-select-op-line]	Selects drawing lines
 \\[artist-select-op-straight-line]	Selects drawing straight lines
 \\[artist-select-op-rectangle]	Selects drawing rectangles
 \\[artist-select-op-square]	Selects drawing squares
 \\[artist-select-op-poly-line]	Selects drawing poly-lines
 \\[artist-select-op-straight-poly-line]	Selects drawing straight poly-lines
 \\[artist-select-op-ellipse]	Selects drawing ellipses
 \\[artist-select-op-circle]	Selects drawing circles
 \\[artist-select-op-text-see-thru]	Selects rendering text (see thru)
 \\[artist-select-op-text-overwrite]	Selects rendering text (overwrite)
 \\[artist-select-op-spray-can]	Spray with spray-can
 \\[artist-select-op-spray-set-size]	Set size for the spray-can
 \\[artist-select-op-erase-char]	Selects erasing characters
 \\[artist-select-op-erase-rectangle]	Selects erasing rectangles
 \\[artist-select-op-vaporize-line]	Selects vaporizing single lines
 \\[artist-select-op-vaporize-lines]	Selects vaporizing connected lines
 \\[artist-select-op-cut-rectangle]	Selects cutting rectangles
 \\[artist-select-op-copy-rectangle]	Selects copying rectangles
 \\[artist-select-op-paste]	Selects pasting
 \\[artist-select-op-flood-fill]	Selects flood-filling


Variables

 This is a brief overview of the different variables.  For more info,
 see the documentation for the variables (type \\[describe-variable] <variable> RET).

 artist-rubber-banding		Interactively do rubber-banding or not
 artist-first-char		What to set at first/second point...
 artist-second-char		...when not rubber-banding
 artist-interface-with-rect	If cut/copy/paste should interface with rect
 artist-arrows			The arrows to use when drawing arrows
 artist-aspect-ratio		Character height-to-width for squares
 artist-trim-line-endings	Trimming of line endings
 artist-flood-fill-right-border	Right border when flood-filling
 artist-flood-fill-show-incrementally	Update display while filling
 artist-pointer-shape		Pointer shape to use while drawing
 artist-ellipse-left-char	Character to use for narrow ellipses
 artist-ellipse-right-char	Character to use for narrow ellipses
 artist-borderless-shapes       If shapes should have borders
 artist-picture-compatibility   Whether or not to be picture mode compatible
 artist-vaporize-fuzziness      Tolerance when recognizing lines
 artist-spray-interval          Seconds between repeated sprayings
 artist-spray-radius            Size of the spray-area
 artist-spray-chars             The spray-``color''
 artist-spray-new-chars         Initial spray-``color''

Hooks

 When entering artist-mode, the hook `artist-mode-init-hook' is called.
 When quitting artist-mode, the hook `artist-mode-exit-hook' is called.


Keymap summary

\\{artist-mode-map}"
  (interactive)
  (if (setq artist-mode
	    (if (null state) (not artist-mode)
	      (> (prefix-numeric-value state) 0)))
      (artist-mode-init)
    (artist-mode-exit)))

;; insert our minor mode string
(or (assq 'artist-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(artist-mode artist-mode-name)
		minor-mode-alist)))

;; insert our minor mode keymap
(or (assq 'artist-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'artist-mode artist-mode-map)
		minor-mode-map-alist)))


;; Init and exit
(defun artist-mode-init ()
  "Init Artist mode.  This will call the hook `artist-mode-init-hook'."
  ;; Set up a conversion table for mapping tabs and new-lines to spaces.
  ;; the last case, 0, is for the last position in buffer/region, where
  ;; the `following-char' function returns 0.
  (let ((i 0))
    (while (< i 256)
      (aset artist-replacement-table i i)
      (setq i (1+ i))))
  (aset artist-replacement-table ?\n ?\s)
  (aset artist-replacement-table ?\t ?\s)
  (aset artist-replacement-table 0 ?\s)
  ;; More setup
  (make-local-variable 'artist-key-is-drawing)
  (make-local-variable 'artist-key-endpoint1)
  (make-local-variable 'artist-key-poly-point-list)
  (make-local-variable 'artist-key-shape)
  (make-local-variable 'artist-key-draw-how)
  (make-local-variable 'artist-popup-menu-table)
  (make-local-variable 'artist-key-compl-table)
  (make-local-variable 'artist-prev-next-op-alist)
  (make-local-variable 'artist-rb-save-data)
  (make-local-variable 'artist-arrow-point-1)
  (make-local-variable 'artist-arrow-point-2)
  (setq artist-key-is-drawing nil)
  (setq artist-key-endpoint1 nil)
  (setq artist-key-poly-point-list nil)
  (setq artist-key-shape nil)
  (setq artist-popup-menu-table (artist-compute-popup-menu-table artist-mt))
  (setq artist-key-compl-table (artist-compute-key-compl-table artist-mt))
  (setq artist-prev-next-op-alist
	(artist-make-prev-next-op-alist artist-key-compl-table))
  (setq artist-rb-save-data (make-vector 7 0))
  (setq artist-arrow-point-1 nil)
  (setq artist-arrow-point-2 nil)
  (make-local-variable 'next-line-add-newlines)
  (setq next-line-add-newlines t)
  (setq artist-key-draw-how
	(artist-go-get-draw-how-from-symbol artist-curr-go))
  (if (and artist-picture-compatibility (not (eq major-mode 'picture-mode)))
      (progn
	(picture-mode)
	(message "")))
  (run-hooks 'artist-mode-init-hook)
  (artist-mode-line-show-curr-operation artist-key-is-drawing))

(defun artist-mode-exit ()
  "Exit Artist mode.  This will call the hook `artist-mode-exit-hook'."
  (if (and artist-picture-compatibility (eq major-mode 'picture-mode))
      (picture-mode-exit))
  (kill-local-variable 'next-line-add-newlines)
  (run-hooks 'artist-mode-exit-hook))

(defun artist-mode-off ()
  "Turn Artist mode off."
  (interactive)
  (artist-mode -1)
  (force-mode-line-update))

;;
;; General routines
;;

(defun artist-update-display ()
  "Repaint the display."
  (sit-for 0))

(defun artist-mode-line-show-curr-operation (is-drawing)
  "Show current operation in mode-line.  If IS-DRAWING, show that."
  (let ((mtext (concat artist-mode-name "/"
		       (artist-go-get-mode-line-from-symbol artist-curr-go)
		       (if is-drawing "/*" ""))))
    (setcdr (assq 'artist-mode minor-mode-alist) (list mtext)))
  (force-mode-line-update))


(defun artist-t-if-fill-char-set ()
  "Return the value of the variable `artist-fill-char-set'."
  artist-fill-char-set)

(defun artist-t ()
  "Always return t."
  t)

(defun artist-nil ()
  "Always return nil."
  nil)

(defun artist-arrows ()
  "Say yes to arrows!"
  t)

(defun artist-no-arrows ()
  "Say no to arrows!"
  nil)

;;
;; Auxiliary init-routines
;;

;
; Computing the table for the x-popup-menu from the master table
;

(defun artist-compute-popup-menu-table (menu-table)
  "Create a menu from MENU-TABLE data.
The returned value is suitable for the `x-popup-menu' function."
  (cons "Artist menu"
	(artist-compute-popup-menu-table-sub menu-table)))

(defun artist-compute-popup-menu-table-sub (menu-table)
  "Compute operation table suitable for `x-popup-menu' from MENU-TABLE."
  (mapcar
   (lambda (element)
     (let ((element-tag (artist-mt-get-tag element)))
       (cond ((eq element-tag 'graphics-operation)
	      (let* ((info-part (artist-mt-get-info-part element))
		     (descr     (artist-go-get-desc info-part))
		     (unshifted (artist-go-get-unshifted info-part))
		     (symbol    (artist-go-get-symbol unshifted)))
		(list descr symbol)))

	     ((eq element-tag 'function-call)
	      (let* ((info-part (artist-mt-get-info-part element))
		     (keyword   (artist-fc-get-keyword info-part))
		     (symbol    (artist-fc-get-symbol info-part)))
		(list keyword symbol)))

	     ((eq element-tag 'separator)
	      '("" ""))

	     ((eq element-tag 'menu)
	      (let* ((info-part (artist-mt-get-info-part element))
		     (title     (artist-mn-get-title info-part))
		     (items     (artist-mn-get-items info-part)))
		(cons title (artist-compute-popup-menu-table-sub items))))

	     (t
	      (error "Internal error: unknown element-tag: \"%s\""
		     element-tag)))))
   menu-table))

;
; Computing the completion table from the master table
;

(defun artist-compute-key-compl-table (menu-table)
  "Compute completion table from MENU-TABLE, suitable for `completing-read'."
  (apply
   'nconc
   (remq nil
    (mapcar
     (lambda (element)
       (let ((element-tag (artist-mt-get-tag element)))
	 (cond ((eq element-tag 'graphics-operation)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (unshifted     (artist-go-get-unshifted info-part))
		       (shifted       (artist-go-get-shifted info-part))
		       (unshifted-kwd (artist-go-get-keyword unshifted))
		       (shifted-kwd   (artist-go-get-keyword shifted)))
		  (list (list unshifted-kwd) (list shifted-kwd))))
	       ((eq element-tag 'menu)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (items         (artist-mn-get-items info-part)))
		  (artist-compute-key-compl-table items)))
	       (t
		nil))))
     menu-table))))


;
; Retrieving  a symbol (graphics operation or function-call) from a keyword
;

(defun artist-mt-get-symbol-from-keyword (kwd)
  "Search master table for keyword KWD and return its symbol."
  (artist-mt-get-symbol-from-keyword-sub artist-mt kwd))

(defun artist-mt-get-symbol-from-keyword-sub (table kwd)
  "Search TABLE for keyword KWD and return its symbol."
  (catch 'found
    (mapc
     (lambda (element)
       (let ((element-tag (artist-mt-get-tag element)))
	 (cond ((eq element-tag 'graphics-operation)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (unshifted     (artist-go-get-unshifted info-part))
		       (shifted       (artist-go-get-shifted info-part))
		       (unshifted-kwd (artist-go-get-keyword unshifted))
		       (shifted-kwd   (artist-go-get-keyword shifted))
		       (unshifted-sym (artist-go-get-symbol unshifted))
		       (shifted-sym   (artist-go-get-symbol shifted)))
		  (if (string-equal kwd unshifted-kwd)
		      (throw 'found unshifted-sym))
		  (if (string-equal kwd shifted-kwd)
		      (throw 'found shifted-sym))))

	       ((eq element-tag 'function-call)
		(let* ((info-part (artist-mt-get-info-part element))
		       (keyword   (artist-fc-get-keyword info-part))
		       (symbol    (artist-fc-get-symbol info-part)))
		  (if (string-equal kwd keyword)
		      (throw 'found symbol))))
	       ((eq element-tag 'menu)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (items         (artist-mn-get-items info-part))
		       (answer        (artist-mt-get-symbol-from-keyword-sub
				       items kwd)))
		  (if answer (throw 'found answer))))
	       (t
		nil))))
     table)
    nil))


;
; Retrieving info from a graphics operation symbol
;

(defun artist-go-retrieve-from-symbol (symbol retrieve-fn)
  "Search the master table for a graphics operation SYMBOL.
Calls RETRIEVE-FN to retrieve information from that symbol's
info-variant-part."
  (artist-go-retrieve-from-symbol-sub artist-mt symbol retrieve-fn))

(defun artist-go-retrieve-from-symbol-sub (table symbol retrieve-fn)
  "Search the TABLE for a graphics operation SYMBOL.
Calls RETRIEVE-FN to retrieve information from that symbol's
info-variant-part."
  (catch 'found
    (mapc
     (lambda (element)
       (let ((element-tag (artist-mt-get-tag element)))
	 (cond ((eq element-tag 'graphics-operation)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (unshifted     (artist-go-get-unshifted info-part))
		       (shifted       (artist-go-get-shifted info-part))
		       (unshifted-sym (artist-go-get-symbol unshifted))
		       (shifted-sym   (artist-go-get-symbol shifted))
		       (variant-part  (cond
				       ((eq unshifted-sym symbol) unshifted)
				       ((eq shifted-sym symbol) shifted)
				       (t nil))))
		  (if variant-part	; if found do:
		      (throw 'found (funcall retrieve-fn variant-part)))))

	       ((eq element-tag 'menu)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (items         (artist-mn-get-items info-part))
		       (answer        (artist-go-retrieve-from-symbol-sub
				       items symbol retrieve-fn)))
		  (if answer (throw 'found answer)))))))

     table)
    nil))

(defun artist-go-get-keyword-from-symbol (symbol)
  "Search the master table, get keyword from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-keyword))

(defun artist-go-get-mode-line-from-symbol (symbol)
  "Search the master table, get mode-line from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-mode-line))

(defun artist-go-get-arrow-pred-from-symbol (symbol)
  "Search the master table, get arrow-pred from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-arrow-pred))

(defun artist-go-get-arrow-set-fn-from-symbol (symbol)
  "Search the master table, get arrow-set-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-arrow-set-fn))

(defun artist-go-get-init-fn-from-symbol (symbol)
  "Search the master table, get init-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-init-fn))

(defun artist-go-get-prep-fill-fn-from-symbol (symbol)
  "Search the master table, get prep-fill-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-prep-fill-fn))

(defun artist-go-get-exit-fn-from-symbol (symbol)
  "Search the master table, get exit-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-exit-fn))

(defun artist-go-get-draw-fn-from-symbol (symbol)
  "Search the master table, get draw-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-draw-fn))

(defun artist-go-get-draw-how-from-symbol (symbol)
  "Search the master table, get draw-how from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-draw-how))

(defun artist-go-get-undraw-fn-from-symbol (symbol)
  "Search the master table, get undraw-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-undraw-fn))

(defun artist-go-get-interval-fn-from-symbol (symbol)
  "Search the master table, get interval-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-interval-fn))

(defun artist-go-get-fill-pred-from-symbol (symbol)
  "Search the master table, get fill-pred from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-fill-pred))

(defun artist-go-get-fill-fn-from-symbol (symbol)
  "Search the master table, get fill-fn from a graphics operation SYMBOL."
  (artist-go-retrieve-from-symbol symbol 'artist-go-get-fill-fn))

(defun artist-go-get-symbol-shift (symbol is-shifted)
  "Search for (shifted or unshifted) graphics operation SYMBOL.
If IS-SHIFTED is non-nil, return the shifted symbol,
otherwise the unshifted symbol."
  (artist-go-get-symbol-shift-sub artist-mt symbol is-shifted))

(defun artist-go-get-symbol-shift-sub (table symbol is-shifted)
  "Search TABLE for (shifted or unshifted) graphics SYMBOL.
If IS-SHIFTED is non-nil, return the shifted symbol,
otherwise the unshifted symbol."
  (catch 'found
    (mapc
     (lambda (element)
       (let ((element-tag (artist-mt-get-tag element)))
	 (cond ((eq element-tag 'graphics-operation)
		(let* ((info-part       (artist-mt-get-info-part element))
		       (unshift-variant (artist-go-get-unshifted info-part))
		       (shift-variant   (artist-go-get-shifted info-part))
		       (unshift-sym     (artist-go-get-symbol unshift-variant))
		       (shift-sym       (artist-go-get-symbol shift-variant)))
		  (if (or (eq symbol unshift-sym) (eq symbol shift-sym))
		      (throw 'found (if is-shifted shift-sym unshift-sym)))))

	       ((eq element-tag 'menu)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (items         (artist-mn-get-items info-part))
		       (answer        (artist-go-get-symbol-shift-sub
				       items symbol is-shifted)))
		  (if answer (throw 'found answer)))))))

     table)
    nil))

;
; Retrieving info from a function-call symbol
;

(defun artist-fc-retrieve-from-symbol (symbol retrieve-fn)
  "Search the master table for a function call SYMBOL.
Calls RETRIEVE-FN to retrieve information from that symbol's
info-variant-part."
  (artist-fc-retrieve-from-symbol-sub artist-mt symbol retrieve-fn))

(defun artist-fc-retrieve-from-symbol-sub (table symbol retrieve-fn)
  "Search TABLE for a function-call SYMBOL.
Calls RETRIEVE-FN to retrieve information from that symbol's
info-variant-part."
  (catch 'found
    (mapc
     (lambda (element)
       (let ((element-tag (artist-mt-get-tag element)))
	 (cond ((eq element-tag 'function-call)
		(let* ((info-part (artist-mt-get-info-part element))
		       (fc-symbol (artist-fc-get-symbol info-part)))
		  (if (eq fc-symbol symbol)
		      (throw 'found (funcall retrieve-fn info-part)))))

	       ((eq element-tag 'menu)
		(let* ((info-part     (artist-mt-get-info-part element))
		       (items         (artist-mn-get-items info-part))
		       (answer        (artist-fc-retrieve-from-symbol-sub
				       items symbol retrieve-fn)))
		  (if answer (throw 'found answer)))))))

     table)
    nil))

(defun artist-fc-get-fn-from-symbol (symbol)
  "Search the master table to get function from a function call SYMBOL."
  (artist-fc-retrieve-from-symbol symbol 'artist-fc-get-fn))


;;
;; Utilities
;;

;; Macro that won't funcall the function if it is nil.
;;
(defmacro artist-funcall (fn &rest args)
  "Call function FN with ARGS, if FN is not nil."
  (list 'if fn (cons 'funcall (cons fn args))))

(defun artist-uniq (l)
  "Remove consecutive duplicates in list L.  Comparison is done with `equal'."
  (cond ((null l) nil)
	((null (cdr l)) l)		; only one element in list
	((equal (car l) (car (cdr l))) (artist-uniq (cdr l))) ; first 2 equal
	(t (cons (car l) (artist-uniq (cdr l)))))) ; first 2 are different

(defun artist-string-split (str r)
  "Split string STR at occurrences of regexp R, returning a list of strings."
  (let ((res nil)
	(start 0)
	(match-pos 0))
    (while (setq match-pos (string-match r str start))
      (setq res (cons (copy-sequence (substring str start match-pos)) res))
      (setq start (match-end 0)))
    (if (null res)
	(list str)
      (if (< (match-end 0) (- (length str) 1))
	  (setq res (cons (substring str (match-end 0) (length str)) res)))
      (reverse res))))

(defun artist-string-to-file (str file-name)
  "Write string STR to file FILE-NAME."
  (write-region str 'end-is-ignored file-name nil 'no-message))

(defun artist-file-to-string (file-name)
  "Read from file FILE-NAME into a string."
  (save-excursion
    (let ((tmp-buffer (get-buffer-create (concat "*artist-" file-name "*"))))
      (set-buffer tmp-buffer)
      (goto-char (point-min))
      (insert-file-contents file-name nil nil nil t)
      (let ((str (copy-sequence (buffer-substring (point-min)
						  (point-max)))))
	(kill-buffer tmp-buffer)
	str))))

(defun artist-clear-buffer (buf)
  "Clear contents of buffer BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (delete-char (- (point-max) (point-min)) nil)))


(defun artist-system (program stdin &optional program-args)
  "Run PROGRAM synchronously with the contents of string STDIN to stdin.
Optional args PROGRAM-ARGS are arguments to PROGRAM.
Return a list (RETURN-CODE STDOUT STDERR)."
  (save-excursion
    (let* ((tmp-stdin-file-name (if stdin
				    (make-temp-file "artist-stdin.")
				  nil))
	   (tmp-stdout-buffer (get-buffer-create
			       (concat "*artist-" program "*")))
	   (tmp-stderr-file-name (make-temp-file "artist-stdout."))
	   (binary-process-input nil)	; for msdos
	   (binary-process-output nil))

      ;; Prepare stdin
      (if stdin (artist-string-to-file stdin tmp-stdin-file-name))

      ;; Clear the buffer
      (artist-clear-buffer tmp-stdout-buffer)

      ;; Start the program
      (unwind-protect
	  (let ((res (if program-args
			 (apply 'call-process
				program
				tmp-stdin-file-name
				(list tmp-stdout-buffer
				      tmp-stderr-file-name)
				nil
				(if (stringp program-args)
				    (list program-args)
				  program-args))
		       (apply 'call-process
			      program
			      tmp-stdin-file-name
			      (list tmp-stdout-buffer
				    tmp-stderr-file-name)
			      nil))))

	    ;; the return value
	    (list res
		  (with-current-buffer tmp-stdout-buffer
                    (buffer-substring (point-min) (point-max)))
		  (artist-file-to-string tmp-stderr-file-name)))

	;; Unwind: remove temporary files and buffers
	(if (and stdin (file-exists-p tmp-stdin-file-name))
	    (delete-file tmp-stdin-file-name))
	(if (file-exists-p tmp-stderr-file-name)
	    (delete-file tmp-stderr-file-name))
	(if (memq tmp-stdout-buffer (buffer-list))
	    (kill-buffer tmp-stdout-buffer))))))

;; Routines that deal with the buffer
;;
;; artist-current-line			get line number (top of buffer is 0)
;;
;; artist-move-to-xy			move to (x,y) (0,0) is beg-of-buffer
;;
;; artist-get-char-at-xy		get char in at (x,y)
;;
;; artist-replace-char			overwrite (replace) char at point
;; artist-replace-chars			overwrite (replace) chars at point
;;

(defsubst artist-current-column ()
  "Return point's current column."
  (current-column))

(defsubst artist-current-line ()
  "Return point's current line, buffer-relative.  Top of buffer is 0."
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defsubst artist-move-to-xy (x y)
  "Move to column X, at row Y from the top of buffer.  Top line is 0."
  ;;
  ;; Q: Why do we do forward-line twice?
  ;; A: The documentation for forward-line says
  ;;
  ;;        "... Returns the count of lines left to move. ... With
  ;;        positive N, a non-empty line at the end counts as one
  ;;        line successfully moved (for the return value)."
  ;;
  ;;    This means that if we are trying to move forward past the end
  ;;    of the buffer, and that last line happened to be longer than
  ;;    the current column, then we end up at the end of that last
  ;;    line, and forward-line returns one less than we actually
  ;;    wanted to move.
  ;;
  ;;	Example: In the figure below, the `X' is the very last
  ;;		 character in the buffer ("a non-empty line at the
  ;;             end"). Suppose point is at P. Then (forward-line 1)
  ;;             returns 0 and puts point after the `X'.
  ;;
  ;;			--------top of buffer--------
  ;;
  ;;			     P      X
  ;;			-------bottom of buffer------
  ;;
  ;;    But, if we are at the end of buffer when trying to move
  ;;    forward, then forward-line will return the (for us) correct
  ;;    value, which is good, because we will come to the end of the
  ;;    buffer by the first forward-line. The second forward-line
  ;;    will then get us where we really wanted to go.
  ;;
  ;;    If we are not moving past the end of the buffer, then the
  ;;    second forward-line will return 0.
  ;;
  ;; Q: What happens if we are moving upwards?
  ;; A: That will work good. insert-char won't insert a negative
  ;;    number of chars, and forward-line will fail silently if we are
  ;;    moving past the beginning of the buffer.
  ;;
  (forward-line (- y (artist-current-line)))
  (insert-char ?\n (forward-line (- y (artist-current-line))))
  (move-to-column (max x 0) t)
  (let ((curr-y (artist-current-line)))
    (setq artist-draw-region-min-y (min curr-y artist-draw-region-min-y))
    (setq artist-draw-region-max-y (max curr-y artist-draw-region-max-y))))

(defsubst artist-get-char-at-xy (x y)
  "Return the character found at column X, row Y.
Also updates the variables `artist-draw-min-y' and `artist-draw-max-y'."
  (artist-move-to-xy x y)
  (let ((curr-y (artist-current-line)))
    (setq artist-draw-region-min-y (min curr-y artist-draw-region-min-y))
    (setq artist-draw-region-max-y (max curr-y artist-draw-region-max-y)))
  (following-char))


(defsubst artist-get-replacement-char (c)
  "Retrieve a replacement for character C from `artist-replacement-table'.
The replacement is used to convert tabs and new-lines to spaces."
  ;; Characters may be outside the range of the `artist-replacement-table',
  ;; for example if they are Unicode code points >= 256.
  ;; Check so we don't attempt to access the array out of its bounds,
  ;; assuming no such character needs to be replaced.
  (if (< c (length artist-replacement-table))
      (aref artist-replacement-table c)
    c))

(defun artist-get-char-at-xy-conv (x y)
  "Retrieve the character at X, Y, converting tabs and new-lines to spaces."
  (save-excursion
    (artist-get-replacement-char (artist-get-char-at-xy x y))))


(defun artist-replace-char (new-char)
  "Replace the character at point with NEW-CHAR."
  (let ((overwrite-mode 'overwrite-mode-textual)
	(fill-column 32765)		; Large :-)
	(blink-matching-paren nil))
    (setq last-command-event (artist-get-replacement-char new-char))
    (self-insert-command 1)))

(defun artist-replace-chars (new-char count)
  "Replace characters at point with NEW-CHAR.  COUNT chars are replaced."
  ;; Check that the variable exists first. The doc says it was added in 19.23.
  (if (and (and (boundp 'emacs-major-version) (= emacs-major-version 20))
	   (and (boundp 'emacs-minor-version) (<= emacs-minor-version 3)))
      ;; This is a bug workaround for Emacs 20, versions up to 20.3:
      ;; The self-insert-command doesn't care about the overwrite-mode,
      ;; so the insertion is done in the same way as in picture mode.
      ;; This seems to be a little bit slower.
      (let* ((replaced-c (artist-get-replacement-char new-char))
	     (replaced-s (make-string count replaced-c)))
	(artist-move-to-xy (+ (artist-current-column) count)
			   (artist-current-line))
	(delete-char (- count))
	(insert replaced-s))
    ;; In emacs-19, the self-insert-command works better
    (let ((overwrite-mode 'overwrite-mode-textual)
	  (fill-column 32765)		; Large :-)
	  (blink-matching-paren nil))
      (setq last-command-event (artist-get-replacement-char new-char))
      (self-insert-command count))))

(defsubst artist-replace-string (string &optional see-thru)
  "Replace contents at point with STRING.
With optional argument SEE-THRU set to non-nil, text in the buffer
``shines thru'' blanks in the STRING."
  (let ((char-list (append string nil))	; convert the string to a list
	(overwrite-mode 'overwrite-mode-textual)
	(fill-column 32765)		; Large :-)
	(blink-matching-paren nil))
    (while char-list
      (let ((c (car char-list)))
	(if (and see-thru (= (artist-get-replacement-char c) ?\s))
	    (artist-move-to-xy (1+ (artist-current-column))
			       (artist-current-line))
	  (artist-replace-char c)))
      (setq char-list (cdr char-list)))))

;;
;; Routines for setting and unsetting points
;; Used when not rubber-banding
;;
(defun artist-no-rb-unset-point1 ()
  "Unsets point 1 when not rubber-banding."
  (let ((x-now (artist-current-column))
	(y-now (artist-current-line))
	(x (aref artist-rb-save-data 0))
	(y (aref artist-rb-save-data 1)))
    (artist-move-to-xy x y)
    (artist-replace-char (aref artist-rb-save-data 2))
    (artist-move-to-xy x-now y-now)))

(defun artist-no-rb-set-point1 (x y)
  "Set point 1 at X, Y when not rubber-banding."
  (let ((x-now (artist-current-column))
	(y-now (artist-current-line)))
    (aset artist-rb-save-data 0 x)
    (aset artist-rb-save-data 1 y)
    (aset artist-rb-save-data 2 (artist-get-char-at-xy x y))
    (artist-move-to-xy x y)
    (artist-replace-char artist-first-char)
    (artist-move-to-xy x-now y-now)
    (aset artist-rb-save-data 6 0)))

(defun artist-no-rb-unset-point2 ()
  "This function unsets point 2 when not rubber-banding."
  (if (= (aref artist-rb-save-data 6) 1)
      (let ((x-now (artist-current-column))
	    (y-now (artist-current-line))
	    (x (aref artist-rb-save-data 3))
	    (y (aref artist-rb-save-data 4)))
	(artist-move-to-xy x y)
	(artist-replace-char (aref artist-rb-save-data 5))
	(artist-move-to-xy x-now y-now))))

(defun artist-no-rb-set-point2 (x y)
  "Set point 2 at X, Y when not rubber-banding."
  (let ((x-now (artist-current-column))
	(y-now (artist-current-line)))
    (aset artist-rb-save-data 3 x)
    (aset artist-rb-save-data 4 y)
    (aset artist-rb-save-data 5 (artist-get-char-at-xy x y))
    (artist-move-to-xy x y)
    (artist-replace-char artist-second-char)
    (artist-move-to-xy x-now y-now)
    (aset artist-rb-save-data 6 1)))

(defun artist-no-rb-unset-points ()
  "This function unsets point 1 and 2 when not rubber-banding."
  (artist-no-rb-unset-point1)
  (artist-no-rb-unset-point2))


;; artist-intersection-char
;;
;; Note: If changing this, see the notes for artist-unintersection-char
;;       and artist-vaporize-lines
;;
(defun artist-intersection-char (new-c old-c)
  "Calculates intersection character when drawing a NEW-C on top of an OLD-C.
Return character according to this scheme:

		OLD-C	NEW-C		return
		 -	 |		   +
		 |	 -		   +
		 +	 |		   +
		 +	 -		   +
		 \\	 /		   X
		 /	 \\		   X
		 X	 /		   X
		 X	 \\		   X
		other combinations	   NEW-C"

  (cond ((and (= old-c ?- )  (= new-c ?| ))  ?+ )
	((and (= old-c ?| )  (= new-c ?- ))  ?+ )
	((and (= old-c ?+ )  (= new-c ?- ))  ?+ )
	((and (= old-c ?+ )  (= new-c ?| ))  ?+ )
	((and (= old-c ?\\ ) (= new-c ?/ ))  ?X )
	((and (= old-c ?/ )  (= new-c ?\\ )) ?X )
	((and (= old-c ?X )  (= new-c ?/ ))  ?X )
	((and (= old-c ?X )  (= new-c ?\\ )) ?X )
	(t new-c)))

;; artist-unintersection-char
;;
;; Note: If changing this, see the note for artist-vaporize-lines
;;
(defun artist-unintersection-char (line-c buffer-c)
  "Restore character to before intersection when removing LINE-C from BUFFER-C.
Return character according to this scheme:

		LINE-C	BUFFER-C	return
		 -	 +		   |
		 |	 +		   -
		 \\	 X		   /
		 /	 X		   \\
		other combinations	   `artist-erase-char'."

  (cond ((and (= line-c ?- )  (= buffer-c ?+ ))  ?|  )
	((and (= line-c ?| )  (= buffer-c ?+ ))  ?-  )
	((and (= line-c ?\\ ) (= buffer-c ?X ))  ?/  )
	((and (= line-c ?/ )  (= buffer-c ?X ))  ?\\ )
	((= line-c buffer-c) artist-erase-char)
	(t buffer-c)))


;; Computing the line-char to use
;; for use with borderless shapes
;;
(defsubst artist-compute-line-char ()
  "Compute which character to use for lines, if any.
Return value is either nil for the default characters that make up lines, or
a character chosen depending on the variables `artist-borderless-shapes',
`artist-fill-char-set', `artist-fill-char' and
`artist-line-char-set' and `artist-line-char'."
  (if (and artist-borderless-shapes artist-fill-char-set)
      artist-fill-char
    (if artist-line-char-set
	artist-line-char
      nil)))


;; Things for drawing horizontal, vertical and diagonal (straight) lines.
;;
;; A line here is a vector:
;; [ start-x start-y length direction saved-char-1 saved-char-2 ... ]
;; directions start with 0 at the x-axis and counts anti clockwise.
;;
(defvar artist-direction-info
  ;;   x  y char
  [ [  1  0 ?- ]			; direction 0
    [  1  1 ?\\ ]			; direction 1
    [  0  1 ?| ]			; direction 2
    [ -1  1 ?/ ]			; direction 3
    [ -1  0 ?- ]			; direction 4
    [ -1 -1 ?\\ ]			; direction 5
    [  0 -1 ?| ]			; direction 6
    [  1 -1 ?/ ] ]			; direction 7
  "Table used for stepping x and y coordinates in a specific direction.
This table is also used for determining which char to use for that direction.")

(defsubst artist-direction-step-x (direction)
  "Return the x-step for DIRECTION from the `artist-direction-info' table."
  (aref (aref artist-direction-info direction) 0))

(defsubst artist-direction-step-y (direction)
  "Return the y-step for DIRECTION from the `artist-direction-info' table."
  (aref (aref artist-direction-info direction) 1))

(defun artist-direction-char (direction)
  "Return the character for DIRECTION from the `artist-direction-info' table."
  (aref (aref artist-direction-info direction) 2))

;; artist-find-direction
;;
;;
;;
(defun artist-find-direction (x1 y1 x2 y2)
  "Find the direction from point X1,Y1 to X2,Y2.
Returns a DIRECTION, a number 0--7, coded as follows:

			 5  6  7
			  \\ | /
			4 - * - 0
			  / | \\
			 3  2  1"
  (let ((delta-x (- x2 x1))
	(delta-y (- y2 y1)))
    (cond ((>= delta-x (* 2 (abs delta-y))) 0)
	  ((>= delta-y (* 2 (abs delta-x))) 2)
 	  ((>= (- delta-x) (* 2 (abs delta-y))) 4)
  	  ((>= (- delta-y) (* 2 (abs delta-x))) 6)
   	  ((and (>= delta-x 0) (>= delta-y 0)) 1)
    	  ((and (<= delta-x 0) (>= delta-y 0)) 3)
     	  ((and (<= delta-x 0) (<= delta-y 0)) 5)
      	  ((and (>= delta-x 0) (<= delta-y 0)) 7))))

(defun artist-straight-calculate-length (direction x1 y1 x2 y2)
  "Calculate length for a straight line in DIRECTION from X1,Y1 to X2,Y2."
  (cond ((or (= direction 7)
	     (= direction 0)
	     (= direction 1)) (1+ (- x2 x1)))
	((or (= direction 3)
	     (= direction 4)
	     (= direction 5)) (1+ (- x1 x2)))
	(t (1+ (abs (- y2 y1))))))

(defun artist-sline (x1 y1 x2 y2)
  "Create a straight line from X1,Y1 to X2,Y2."
  (let* ((direction (artist-find-direction x1 y1 x2 y2))
	 (length (artist-straight-calculate-length direction x1 y1 x2 y2))
	 (line (make-vector (+ length 4) x1)))
    ;; not needed:
    ;; (aset line 0 x1)
    ;; because we set all elements to x1
    (aset line 1 y1)
    (aset line 2 length)
    (aset line 3 direction)
    line))

(defun artist-save-chars-under-sline (line)
  "Save characters under a LINE."
  (let ((x (aref line 0))
       	(y (aref line 1))
  	(length (+ (aref line 2) 4))
   	(direction (aref line 3))
    	(i 4))
    (while (< i length)
      (aset line i (artist-get-char-at-xy x y))
      (setq x (+ x (artist-direction-step-x direction)))
      (setq y (+ y (artist-direction-step-y direction)))
      (setq i (1+ i))))
    line)



;; Things for drawing lines in all directions.
;; The line drawing engine is the eight-point algorithm.
;;
;; A line is here a list of (x y saved-char new-char)s.
;;
(defvar artist-octant-info
  ;; Initial	Step in		Step in
  ;; coeffs	x and y		x and y
  ;; for	if q >= 0	if g < 0
  ;; dfdx,dfdy
  [ [  2  1	 1  0		 1  1 ]		; 1st octant
    [  1  2	 1  1		 0  1 ]		; 2nd octant
    [ -1  2	 0  1		-1  1 ]		; 3rd octant
    [ -2  1	-1  1		-1  0 ]		; 4th octant
    [ -2 -1	-1  0		-1 -1 ]		; 5th octant
    [ -1 -2	-1 -1		 0 -1 ]		; 6th octant
    [  1 -2	 0 -1		 1 -1 ]		; 7th octant
    [  2 -1	 1 -1		 1  0 ] ]	; 8th octant
  "Table used by line drawing algorithm (eight point).")

;; Primitives for the artist-octant-info.
;; Decrease octant by 1 since elt counts from 0 and octant counts from 1.
;;
(defsubst artist-get-dfdx-init-coeff (octant)
  "Retrieve dfdx component for OCTANT."
  (aref (aref artist-octant-info (- octant 1)) 0))

(defsubst artist-get-dfdy-init-coeff (octant)
  "Retrieve dfdy component for OCTANT."
  (aref (aref artist-octant-info (- octant 1)) 1))

(defsubst artist-get-x-step-q>=0 (octant)
  "Retrieve x-step component for OCTANT when q >= 0."
  (aref (aref artist-octant-info (- octant 1)) 2))

(defsubst artist-get-y-step-q>=0 (octant)
  "Retrieve y-step component for OCTANT when q >= 0."
  (aref (aref artist-octant-info (- octant 1)) 3))

(defsubst artist-get-x-step-q<0 (octant)
  "Retrieve x-step component for OCTANT for q < 0."
  (aref (aref artist-octant-info (- octant 1)) 4))

(defsubst artist-get-y-step-q<0 (octant)
  "Retrieve y-step component for OCTANT for q < 0."
  (aref (aref artist-octant-info (- octant 1)) 5))


;; Find octant from x1 y1 x2 y2 coordinates.
;;
(defun artist-find-octant (x1 y1 x2 y2)
  "Find octant for a line from X1,Y1 to X2,Y2.
Octant are numbered 1--8, anti-clockwise as:

		 \\3|2/
		 4\\|/1
		---+---
		 5/|\\8
		 /6|7\\"

  (if (<= x1 x2)				; quadrant 1 or 4
      (if (<= y1 y2)				; quadrant 1, octant 1 or 2
	  (if (>= (- x2 x1) (- y2 y1))
	      1
	    2)
	(if (>= (- x2 x1) (- (- y2 y1)))	; quadrant 4, octant 7 or 8
	    8
	  7))
    (if (<= y1 y2)				; quadrant 2 or 3
	(if (>= (- (- x2 x1)) (- y2 y1))	; quadrant 2, octant 3 or 4
	    4
	  3)
      (if (>= (- (- x2 x1)) (- (- y2 y1)))	; quadrant 3, octant 5 or 6
	  5
	6))))

;; Some inline functions for creating, setting and reading
;; members of a coordinate
;;

(defsubst artist-new-coord (x y &optional new-char)
  "Create a new coordinate at X,Y for use in a line.
Optional argument NEW-CHAR can be used for setting the new-char component
in the coord."
  (let ((coord (make-vector 4 x)))
    (aset coord 1 y)
    (aset coord 3 new-char)
    coord))

(defsubst artist-coord-get-x (coord)
  "Retrieve the x component of a COORD."
  (aref coord 0))

(defsubst artist-coord-get-y (coord)
  "Retrieve the y component of a COORD."
  (aref coord 1))

(defsubst artist-coord-set-x (coord new-x)
  "Set the x component of a COORD to NEW-X."
  (aset coord 0 new-x)
  coord)

(defsubst artist-coord-set-y (coord new-y)
  "Set the y component of a COORD to NEW-Y."
  (aset coord 1 new-y)
  coord)

(defsubst artist-coord-get-saved-char (coord)
  "Retrieve the saved char component of a COORD."
  (aref coord 2))

(defsubst artist-coord-get-new-char (coord)
  "Retrieve the new char component of a COORD."
  (aref coord 3))

(defsubst artist-coord-add-saved-char (coord saved-char)
  "Set the saved char component of a COORD to SAVED-CHAR."
  (aset coord 2 saved-char)
  coord)

(defsubst artist-coord-add-new-char (coord new-char)
  "Set the new char component of a COORD to NEW-CHAR."
  (aset coord 3 new-char)
  coord)

(defsubst artist-coord-set-new-char (coord new-char)
  "Set the new char component of a COORD to NEW-CHAR."
  (aset coord 3 new-char)
  coord)


;; Pretend we are plotting a pixel. Instead we just list it
;;
(defmacro artist-put-pixel (point-list x y)
  "In POINT-LIST, store a ``pixel'' at coord X,Y."
  (list 'setq point-list
	(list 'append point-list (list 'list (list 'artist-new-coord x y)))))

;; Calculate list of points using eight point algorithm
;; return a list of coords
;;
(defun artist-eight-point (x1 y1 x2 y2)
  "Run the eight-point algorithm to get a list of coords from X1,Y1 to X2,Y2."
  (let* ((point-list nil)
	 (octant (artist-find-octant x1 y1 x2 y2))
	 (dfdx-coeff (artist-get-dfdx-init-coeff octant))
	 (dfdy-coeff (artist-get-dfdy-init-coeff octant))
	 (x-step-q>=0 (artist-get-x-step-q>=0 octant))
	 (y-step-q>=0 (artist-get-y-step-q>=0 octant))
	 (x-step-q<0 (artist-get-x-step-q<0 octant))
	 (y-step-q<0 (artist-get-y-step-q<0 octant))
	 (dfdx (- (- y2 y1)))
	 (dfdy (- x2 x1))
	 (x x1)
	 (y y1)
	 (f 0)
	 (q (+ (* 2 f)
	       (* dfdx-coeff dfdx)
	       (* dfdy-coeff dfdy))))
    (artist-put-pixel point-list x y)
    (while (or (not (eq x x2)) (not (eq y y2)))
      (if (>= q 0)
	  (progn
	    (setq x (+ x x-step-q>=0))
	    (setq y (+ y y-step-q>=0))
	    (setq f (+ f (* x-step-q>=0 dfdx) (* y-step-q>=0 dfdy))))
	(progn
	  (setq x (+ x x-step-q<0))
	  (setq y (+ y y-step-q<0))
	  (setq f (+ f (* x-step-q<0 dfdx) (* y-step-q<0 dfdy)))))
      (setq q (+ (* 2 f) (* dfdx-coeff dfdx) (* dfdy-coeff dfdy)))
      (artist-put-pixel point-list x y))
    point-list))

;; artist-save-chars-under-point-list
;; Remembers the chars that were there before we did draw the line.
;; Returns point-list.
;;
(defun artist-save-chars-under-point-list (point-list)
  "Save characters originally under POINT-LIST."
  (mapcar
   (lambda (coord)
     (artist-coord-add-saved-char
      coord
      (artist-get-char-at-xy (artist-coord-get-x coord)
			     (artist-coord-get-y coord))))
   point-list))

;; artist-calculate-new-char, artist-calculate-new-chars
;; Calculates which char to insert depending on direction of point-list.
;;
;; Depending on new-coord's position relative to last-coord one of the
;; following chars are returned: \ | / - o, as indicated by this:
;;
;;			\ | /
;;			- o -
;;			/ | \
;;
;; artist-calculate-new-char  works on one coordinate, returns char.
;; artist-calculate-new-chars works on a point-list, returns point-list.
;;
(defun artist-calculate-new-char (last-coord new-coord)
  "Return a line-char to use when moving from LAST-COORD to NEW-COORD."
  (let ((last-x (artist-coord-get-x last-coord))
	(last-y (artist-coord-get-y last-coord))
	(new-x (artist-coord-get-x new-coord))
	(new-y (artist-coord-get-y new-coord)))
    (cond ((> new-x last-x) (cond ((< new-y last-y) ?/ )
				  ((> new-y last-y) ?\\ )
				  (t ?- )))
	  ((< new-x last-x) (cond ((< new-y last-y) ?\\ )
				  ((> new-y last-y) ?/ )
				  (t ?- )))
	  ((eq new-y last-y) ?o)
	  (t ?| ))))

(defun artist-calculate-new-chars (point-list)
  "Return a list of coords with line-chars calculated.  Input: POINT-LIST."
  (if (null (cdr point-list))
      (list (artist-coord-add-new-char (car point-list) ?o ))
    (let ((last-coord (car point-list)))
      (cons (artist-coord-add-new-char
	     (car point-list)
	     (artist-calculate-new-char (car (cdr point-list))
					(car point-list)))
	    (mapcar
	     (lambda (this-coord)
	       (prog1
		   (artist-coord-add-new-char
		    this-coord
		    (artist-calculate-new-char last-coord this-coord))
		 (setq last-coord this-coord)))
	     (cdr point-list))))))

;; artist-modify-new-chars
;; Replaces some characters with some other characters.
;;
;; artist-modify-new-chars works on a point-list, returns point-list.
;;
(defun artist-modify-new-chars (point-list)
  "Replace intersecting characters in POINT-LIST.
This function returns a point-list."
  (mapcar
   (lambda (coord)
     (let* ((new-c (artist-coord-get-new-char coord))
	    (saved-c (artist-coord-get-saved-char coord))
	    (modified-c (artist-intersection-char new-c saved-c)))
       (artist-coord-set-new-char coord modified-c)))
   point-list))


;;
;; functions for accessing endpoints and elements in object requiring
;; 2 endpoints
;;

(defun artist-make-endpoint (x y)
  "Create an endpoint at X, Y."
  (let ((new-endpoint (make-vector 2 x)))
    (aset new-endpoint 1 y)
    new-endpoint))

(defun artist-endpoint-get-x (endpoint)
  "Retrieve the x component of an ENDPOINT."
  (aref endpoint 0))

(defun artist-endpoint-get-y (endpoint)
  "Retrieve the y component of an ENDPOINT."
  (aref endpoint 1))

(defun artist-make-2point-object (endpoint1 endpoint2 shapeinfo)
  "Create a 2-point object of ENDPOINT1, ENDPOINT2 and SHAPEINFO."
  (list endpoint1 endpoint2 shapeinfo))

(defun artist-2point-get-endpoint1 (obj)
  "Retrieve the first endpoint of a 2-point object OBJ."
  (elt obj 0))

(defun artist-2point-get-endpoint2 (obj)
  "Retrieve the second endpoint of a 2-point object OBJ."
  (elt obj 1))

(defun artist-2point-get-shapeinfo (obj)
  "Retrieve the shapeinfo component of a 2-point object OBJ."
  (elt obj 2))


;;
;; Drawing and undrawing lines (any direction)
;;

(defun artist-draw-line (x1 y1 x2 y2)
  "Draw a line from X1, Y1 to X2, Y2.

Output is a line, which is a list (END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a list of vectors [X Y SAVED-CHAR NEW-CHAR]."
  (let ((endpoint1 (artist-make-endpoint x1 y1))
	(endpoint2 (artist-make-endpoint x2 y2)))
    (artist-make-2point-object
     endpoint1
     endpoint2
     (mapcar
      (lambda (coord)
	(artist-move-to-xy (artist-coord-get-x coord)
			   (artist-coord-get-y coord))
	(if artist-line-char-set
	    (artist-replace-char artist-line-char)
	  (artist-replace-char (artist-coord-get-new-char coord)))
	coord)
      (artist-modify-new-chars
       (artist-calculate-new-chars
	(artist-save-chars-under-point-list
	 (artist-eight-point x1 y1 x2 y2))))))))

(defun artist-undraw-line (line)
  "Undraw LINE."
  (mapcar
   (lambda (coord)
     (artist-move-to-xy (artist-coord-get-x coord)
			(artist-coord-get-y coord))
     (artist-replace-char (artist-coord-get-saved-char coord))
     coord)
   (artist-2point-get-shapeinfo line)))

;;
;; Drawing and undrawing straight lines
;;

(defun artist-draw-sline (x1 y1 x2 y2)
  "Draw a straight line from X1, Y1 to X2, Y2.
Straight lines are vertical, horizontal or diagonal lines.
They are faster to draw and most often they are what you need
when drawing a simple image.

Output is a straight line, which is a list on the form
\(END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a vector [START-X START-Y LENGTH-OF-LINE DIRECTION
                        ORIGINAL-CHAR-1 ORIGINAL-CHAR-2 ... ]."
  (let* ((line (artist-save-chars-under-sline (artist-sline x1 y1 x2 y2)))
	 (x (aref line 0))
	 (y (aref line 1))
	 (length (+ (aref line 2) 4))
	 (direction (aref line 3))
	 (line-char (artist-direction-char direction))
	 (i 4)
	 (endpoint1 (artist-make-endpoint x y))
	 (endpoint2 nil))
    (while (< i length)
      (artist-move-to-xy x y)
      (if artist-line-char-set
	  (artist-replace-char artist-line-char)
	(artist-replace-char (artist-intersection-char
			      line-char
			      (aref line i))))
      (if (not (< (1+ i) length))
	  ;; This is the last element. Set the second endpoint
	  (setq endpoint2 (artist-make-endpoint x y)))
      (setq x (+ x (artist-direction-step-x direction)))
      (setq y (+ y (artist-direction-step-y direction)))
      (setq i (1+ i)))
    (artist-make-2point-object endpoint1 endpoint2 line)))


(defun artist-undraw-sline (line)
  "Undraw a straight line LINE."
  (if line
      (let* ((shape-info (artist-2point-get-shapeinfo line))
	     (x (aref shape-info 0))
	     (y (aref shape-info 1))
	     (length (+ (aref shape-info 2) 4))
	     (direction (aref shape-info 3))
	     (i 4))
	(while (< i length)
	  (artist-move-to-xy x y)
	  (artist-replace-char (aref shape-info i))
	  (setq x (+ x (artist-direction-step-x direction)))
	  (setq y (+ y (artist-direction-step-y direction)))
	  (setq i (1+ i))))))


;;
;; Drawing and undrawing rectangles and squares
;;

(defun artist-draw-rect (x1 y1 x2 y2)
  "Draw a rectangle with corners at X1, Y1 and X2, Y2.

Output is a rectangle, which is a list on the form
\(END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a list of four straight lines."
  (let* ((artist-line-char (artist-compute-line-char))
         (artist-line-char-set artist-line-char)
         (line1 (artist-draw-sline x1 y1 x2 y1))
	 (line2 (artist-draw-sline x2 y1 x2 y2))
	 (line3 (artist-draw-sline x2 y2 x1 y2))
	 (line4 (artist-draw-sline x1 y2 x1 y1))
	 (endpoint1 (artist-make-endpoint x1 y1))
	 (endpoint2 (artist-make-endpoint x2 y2)))
    (artist-make-2point-object endpoint1
			       endpoint2
			       (list line1 line2 line3 line4))))

(defun artist-undraw-rect (rectangle)
  "Undraw RECTANGLE."
  (if rectangle
      (let ((shape-info (artist-2point-get-shapeinfo rectangle)))
	(artist-undraw-sline (elt shape-info 3))
	(artist-undraw-sline (elt shape-info 2))
	(artist-undraw-sline (elt shape-info 1))
	(artist-undraw-sline (elt shape-info 0)))))


(defun artist-rect-corners-squarify (x1 y1 x2 y2)
  "Compute square corners from rectangle corners at X1, Y1 and X2, Y2.
The square's first corner will be X1, Y1.  The position of the second
corner depends on which of X2 and Y2 is most far away from X1, Y1."
  (let* ((delta-x      (- x2 x1))
	 (delta-y      (- y2 y1))
	 (delta-x-sign (if (< delta-x 0) -1 1))
	 (delta-y-sign (if (< delta-y 0) -1 1))
	 (new-x2)	; set below
	 (new-y2))	; set below

    ;; Check which of x2 and y2 is most distant
    ;; take care to the aspect ratio
    (if (> (abs delta-x) (abs delta-y))

	;; *** x2 more distant than y2 (with care taken to aspect ratio)
	(progn
	  (setq new-x2 x2)
	  (setq new-y2 (+ y1 (round (/ (* (abs delta-x) delta-y-sign)
				       artist-aspect-ratio)))))

      ;; *** y2 more distant than x2  (with care taken to aspect ratio)
      (progn
	(setq new-x2 (round (+ x1 (* (* (abs delta-y) delta-x-sign)
				     artist-aspect-ratio))))
	(setq new-y2 y2)))

    ;; Return this
    (list x1 y1 new-x2 new-y2)))


(defun artist-draw-square (x1 y1 x2 y2)
  "Draw a square with corners at X1, Y1 and X2, Y2.

Output is a square, which is a list on the form
\(END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a list of four straight lines."
  (let* ((artist-line-char (artist-compute-line-char))
         (artist-line-char-set artist-line-char)
         (square-corners (artist-rect-corners-squarify x1 y1 x2 y2))
	 (new-x1 (elt square-corners 0))
	 (new-y1 (elt square-corners 1))
	 (new-x2 (elt square-corners 2))
	 (new-y2 (elt square-corners 3))
	 (endpoint1 (artist-make-endpoint new-x1 new-y1))
	 (endpoint2 (artist-make-endpoint new-x2 new-y2))
	 (line1 (artist-draw-sline new-x1 new-y1 new-x2 new-y1))
	 (line2 (artist-draw-sline new-x2 new-y1 new-x2 new-y2))
	 (line3 (artist-draw-sline new-x2 new-y2 new-x1 new-y2))
	 (line4 (artist-draw-sline new-x1 new-y2 new-x1 new-y1)))
    (artist-make-2point-object endpoint1
			       endpoint2
			       (list line1 line2 line3 line4))))

(defun artist-undraw-square (square)
  "Undraw SQUARE."
  (if square
      (let ((shape-info (artist-2point-get-shapeinfo square)))
	(artist-undraw-sline (elt shape-info 3))
	(artist-undraw-sline (elt shape-info 2))
	(artist-undraw-sline (elt shape-info 1))
	(artist-undraw-sline (elt shape-info 0)))))

;;
;; Filling rectangles and squares
;;

(defun artist-fill-rect (rect x1 y1 x2 y2)
  "Fill rectangle RECT from X1,Y1 to X2,Y2."
  (let ((x (1+ (min x1 x2)))
	(y (1+ (min y1 y2)))
	(x-max (max x1 x2))
	(y-max (max y1 y2)))
    (let ((w (- x-max x)))
      (while (< y y-max)
	(artist-move-to-xy x y)
	(artist-replace-chars artist-fill-char w)
	(setq y (1+ y))))))

(defun artist-fill-square (square x1 y1 x2 y2)
  "Fill a SQUARE from X1,Y1 to X2,Y2."
  (let* ((square-corners (artist-rect-corners-squarify x1 y1 x2 y2))
	 (new-x1 (elt square-corners 0))
	 (new-y1 (elt square-corners 1))
	 (new-x2 (elt square-corners 2))
	 (new-y2 (elt square-corners 3))
	 (x (1+ (min new-x1 new-x2)))
	 (y (1+ (min new-y1 new-y2)))
	 (x-max (max new-x1 new-x2))
	 (y-max (max new-y1 new-y2))
	 (w (- x-max x)))
    (while (< y y-max)
      (artist-move-to-xy x y)
      (artist-replace-chars artist-fill-char w)
      (setq y (1+ y)))))


;;
;; Pen drawing
;;

(defun artist-pen (x1 y1)
  "Draw a character at X1, Y1.
The character is replaced with the character in `artist-fill-char'."
  (artist-move-to-xy x1 y1)
  (artist-replace-char (if artist-line-char-set
			   artist-line-char
			 (if artist-fill-char-set
			     artist-fill-char
			   artist-default-fill-char))))


(defun artist-pen-line (x1 y1)
  "Draw a line from last pen position to X1, Y1.
The character is replaced with the character in `artist-fill-char'.
This will store all points in `artist-key-poly-point-list' in reversed
order (I assume it is faster to cons to the beginning of the list than
to append to the end of the list, when doing free-hand drawing)."
  (let ((artist-line-char (if artist-line-char-set
			      artist-line-char
			    (if artist-fill-char-set
				artist-fill-char
			      artist-default-fill-char))))

    ;; Draw line from last point to this
    (let ((x-last (car (car artist-key-poly-point-list)))
	  (y-last (cdr (car artist-key-poly-point-list))))
      (artist-move-to-xy x-last y-last)
      (artist-replace-char artist-line-char)
      (artist-draw-line x-last y-last x1 y1))

    ;; Update the point-list
    (setq artist-key-poly-point-list
	  (cons (cons x1 y1) artist-key-poly-point-list))))

(defun artist-pen-reset-last-xy (x1 y1)
  "Reset the last x and y points to X1, Y1 when doing pen-drawing."
  (artist-clear-arrow-points)
  (setq artist-key-poly-point-list (list (cons x1 y1))))


(defun artist-pen-set-arrow-points (x1 y1)
  "Set arrow points for pen drawing using X1, Y1.
Also, the `artist-key-poly-point-list' is reversed."

  (setq artist-key-poly-point-list
	(artist-uniq artist-key-poly-point-list))

  (if (>= (length artist-key-poly-point-list) 2)

      ;; Only set arrow-points if the point-list has two or more entries
      (let ((xn   (car (car artist-key-poly-point-list)))
	    (yn   (cdr (car artist-key-poly-point-list)))
	    (xn-1 (car (car (cdr artist-key-poly-point-list))))
	    (yn-1 (cdr (car (cdr artist-key-poly-point-list))))
	    (dirn))			; direction for point n
	(setq artist-key-poly-point-list (reverse artist-key-poly-point-list))
	(let ((x0 (car (car artist-key-poly-point-list)))
	      (y0 (cdr (car artist-key-poly-point-list)))
	      (x1 (car (car (cdr artist-key-poly-point-list))))
	      (y1 (cdr (car (cdr artist-key-poly-point-list))))
	      (dir0))			; direction for point 0
	  (setq dir0 (artist-find-direction x1 y1 x0 y0))
	  (setq dirn (artist-find-direction xn-1 yn-1 xn yn))
	  (setq artist-arrow-point-1 (artist-make-arrow-point x0 y0 dir0))
	  (setq artist-arrow-point-2 (artist-make-arrow-point xn yn dirn))))))


;;
;; Text rendering
;;
(defun artist-figlet-run (text font extra-args)
  "Run figlet rendering TEXT using FONT.
EXTRA-ARGS for figlet, for the command line, may be specified."
  (let* ((figlet-args (cond ((and font extra-args)
			     (cons (concat "-f" font)
				   (artist-string-split extra-args "[ \t]+")))
			    (font (concat "-f" font))
			    (extra-args
			     (artist-string-split extra-args "[ \t]+"))
			    (t nil)))
	 (figlet-output (artist-system artist-figlet-program text figlet-args))
	 (exit-code (elt figlet-output 0))
	 (stdout    (elt figlet-output 1))
	 (stderr    (elt figlet-output 2)))
    (if (not (= exit-code 0))
	(error "Failed to render font: %s (%d)" stderr exit-code))
    stdout))

(defun artist-figlet-get-font-list ()
  "Read fonts in with the shell command.
Returns a list of strings."
  (let* ((cmd-interpreter "/bin/sh")
	 (ls-cmd          artist-figlet-list-fonts-command)
	 (result          (artist-system cmd-interpreter ls-cmd nil))
	 (exit-code       (elt result 0))
	 (stdout          (elt result 1))
	 (stderr          (elt result 2)))
    (if (not (= exit-code 0))
	(error "Failed to read available fonts: %s (%d)" stderr exit-code))
    (artist-string-split stdout ".flf\n")))

(defun artist-figlet-choose-font ()
  "Read any extra arguments for figlet."
  (interactive)
  (let* ((avail-fonts  (artist-figlet-get-font-list))
	 (font (completing-read (concat "Select font (default "
					artist-figlet-default-font
					"): ")
				(mapcar
				 (lambda (font) (cons font font))
				 avail-fonts))))
    (if (string= font "") artist-figlet-default-font font)))

(defun artist-figlet-get-extra-args ()
  "Read any extra arguments for figlet."
  (let ((extra-args (read-string "Extra args to figlet: ")))
    (if (string= extra-args "")
	nil
      extra-args)))

(defun artist-figlet (text)
  "Render TEXT using figlet."
  (let* ((figlet-font (artist-figlet-choose-font))
	 (figlet-extra-args (artist-figlet-get-extra-args)))
    (artist-figlet-run text figlet-font figlet-extra-args)))


(defun artist-text-insert-common (x y text see-thru)
  "At position X, Y, insert text TEXT.
If SEE-THRU is non-nil, then blanks in TEXT do not replace text
in the buffer."
  (let* ((string-list (artist-string-split text "\n"))
	 (i 0)
	 (len (length string-list)))
    (while (< i len)
      (artist-move-to-xy x (+ y i))
      (artist-replace-string (car string-list) see-thru)
      (setq string-list (cdr string-list))
      (setq i (1+ i)))))

(defun artist-text-insert-see-thru (x y text)
  "At position X, Y, insert text TEXT.
Let text already in buffer shine thru the TEXT inserted."
  (artist-text-insert-common x y text t))

(defun artist-text-insert-overwrite (x y text)
  "At position X, Y, insert text TEXT.
Let blanks in TEXT overwrite any text already in the buffer."
  (artist-text-insert-common x y text nil))

(defun artist-text-see-thru (x y)
  "Prompt for text to render, render it at X,Y.
This is done by calling the function specified by
`artist-text-renderer-function', which must return a list of strings,
to be inserted in the buffer.

Text already in the buffer ``shines thru'' blanks in the rendered text."
  (let* ((input-text (read-string "Type text to render: "))
	 (rendered-text (artist-funcall artist-text-renderer-function input-text)))
    (artist-text-insert-see-thru x y rendered-text)))


(defun artist-text-overwrite (x y)
  "Prompt for text to render, render it at X,Y.
This is done by calling the function specified by
`artist-text-renderer-function', which must return a list of strings,
to be inserted in the buffer.

Blanks in the rendered text overwrite any text in the buffer."
  (let* ((input-text (read-string "Type text to render: "))
	 (rendered-text (artist-funcall artist-text-renderer-function input-text)))
    (artist-text-insert-overwrite x y rendered-text)))

;;
;; Spraying
;;

(defun artist-spray-get-interval ()
  "Retrieve the interval for repeated spray."
  artist-spray-interval)

(defun artist-spray-random-points (n radius)
  "Generate N random points within a radius of RADIUS.
Returns a list of points.  Each point is on the form (X1 . Y1)."
  (let ((points))
    (while (> n 0)
      (let* ((angle (* (random 359) (/ float-pi 180)))
	     (dist  (random radius))
	     (point (cons (round (* dist (cos angle)))
			  (round (* dist (sin angle))))))
	(setq points (cons point points)))
      (setq n (- n 1)))
    points))

(defun artist-spray (x1 y1)
  "Spray at X1, Y1."
  (let* ((num-points (* artist-spray-radius artist-spray-radius))
	 (spray-points (artist-spray-random-points num-points
						   artist-spray-radius)))
    (while spray-points
      ;; Replace one spray point
      (let* ((point  (car spray-points))
	     (x      (+ x1 (car point)))
	     (y      (+ y1 (cdr point)))
	     (buf-c  (artist-get-char-at-xy-conv x y))
	     (this-c (memq buf-c artist-spray-chars))
	     (next-c (cond ((null this-c) artist-spray-new-char)
			   ((null (cdr this-c)) (car this-c))
			   (t (car (cdr this-c))))))
	(artist-move-to-xy x y)
	(artist-replace-char next-c))

      ;; Step to next spray point
      (setq spray-points (cdr spray-points)))))

(defun artist-spray-clear-circle (circle x1 y1 x2 y2)
  "Clear circle CIRCLE at X1, Y1 through X2, Y2."
  (artist-undraw-circle circle))

(defun artist-spray-set-radius (circle x1 y1 x2 y2)
  "Set spray radius from CIRCLE at X1, Y1 through X2, Y2."
  (let ((dx (- x2 x1))
	(dy (- y2 y1)))
    (setq artist-spray-radius (round (sqrt (+ (* dx dx) (* dy dy)))))
    (if (= 0 artist-spray-radius)
	(setq artist-spray-radius 1))))

;;
;; Erasing
;;

(defun artist-erase-char (x1 y1)
  "Erase a character at X1, Y1.
The character is replaced with the character in `artist-erase-char'."
  (artist-move-to-xy x1 y1)
  (artist-replace-char artist-erase-char))

(defun artist-erase-rect (rect x1 y1 x2 y2)
  "Erase rectangle RECT from X1, Y1, X2, Y2."
  (let ((artist-line-char-set t)
	(artist-fill-char-set t)
	(artist-line-char artist-erase-char)
	(artist-fill-char artist-erase-char))
    (artist-draw-rect x1 y1 x2 y2)
    (artist-fill-rect rect x1 y1 x2 y2)))


;;
;; Vaporizing (erasing) line and lines
;;


(defun artist-vap-find-endpoint (x1 y1 step-x step-y accept-set reject-set)
  "Find one endpoint for line through X1, Y1.
The endpoint is searched for in the direction defined by STEP-X, STEP-Y,
accepting characters in the list ACCEPT-SET, stopping immediately
when finding characters in the list REJECT-SET.  Fuzziness, that is
the number of consecutive characters not in ACCEPT-SET to allow as
part of the line, is determined by the variable `artist-vaporize-fuzziness'.
An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (let ((x x1)
	(y y1)
	(x-last x1)
	(y-last y1)
	(done nil))
    (while (not done)
      (let ((c (artist-get-char-at-xy-conv x y)))
	(cond ((memq c reject-set)
	       (setq done t))

	      ;; We found a character we are accepting as part of the line.
	      ;; Update position
	      ((memq c accept-set)
	       (setq x-last x
		     y-last y
		     x (+ x step-x)
		     y (+ y step-y))
	       (if (or (< x 0) (< y 0))	;stop at the edge
		   (setq done t)))

	      ;; We found a character we are not accepting as part of
	      ;; the line Search `artist-vaporize-fuzziness'
	      ;; characters away from this position in the same
	      ;; direction to see if there are any characters in the
	      ;; accept-set. If not, we have found the endpoint.
	      (t
	       (let ((fuzziness artist-vaporize-fuzziness)
		     (x-tmp x)
		     (y-tmp y))

		 ;; while we have more fuzziness left and we have not
		 ;; found a character accepted as a line, move
		 ;; forward!
		 (while (and (> fuzziness 0) (not (memq c accept-set)))
		   (setq x-tmp (+ x-tmp step-x))
		   (setq y-tmp (+ y-tmp step-y))
		   (setq c (artist-get-char-at-xy-conv x-tmp y-tmp))
		   (setq fuzziness (- fuzziness 1)))
		 (if (memq c accept-set)

		     ;; The line continues on the other side of the
		     ;; not-accepted character.
		     (setq x x-tmp
			   y y-tmp)

		   ;; Else: We couldn't find any line on the other side.
		   ;; That means we are done searching for the endpoint.
		   (setq done t)))))))
    (cons x-last y-last)))


(defun artist-vap-find-endpoints-horiz (x y)
  "Find endpoints for a horizontal line through X, Y.
An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (list (artist-vap-find-endpoint x y  1 0 '(?- ?+) '(?\s))
	(artist-vap-find-endpoint x y -1 0 '(?- ?+) '(?\s))))

(defun artist-vap-find-endpoints-vert (x y)
  "Find endpoints for a vertical line through X, Y.
An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (list (artist-vap-find-endpoint x y 0  1 '(?| ?+) '(?\s))
	(artist-vap-find-endpoint x y 0 -1 '(?| ?+) '(?\s))))

(defun artist-vap-find-endpoints-swne (x y)
  "Find endpoints for a diagonal line (made by /'s) through X, Y.
An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (list (artist-vap-find-endpoint x y  1 -1 '(?/ ?X) '(?\s))
	(artist-vap-find-endpoint x y -1  1 '(?/ ?X) '(?\s))))

(defun artist-vap-find-endpoints-nwse (x y)
  "Find endpoints for a diagonal line (made by \\'s) through X, Y.
An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (list (artist-vap-find-endpoint x y  1  1 '(?\\ ?X) '(?\s))
	(artist-vap-find-endpoint x y -1 -1 '(?\\ ?X) '(?\s))))


(defun artist-vap-find-endpoints (x y)
  "Given a point X1, Y1, return a list of endpoints of lines through X, Y.
An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (if artist-line-char-set
      nil
    (let ((c (artist-get-char-at-xy-conv x y)))
      (cond ((eq c ?-)  (artist-vap-find-endpoints-horiz x y))
	    ((eq c ?|)  (artist-vap-find-endpoints-vert x y))
	    ((eq c ?/)  (artist-vap-find-endpoints-swne x y))
	    ((eq c ?\\) (artist-vap-find-endpoints-nwse x y))
	    ((eq c ?+)  (append (artist-vap-find-endpoints-horiz x y)
				(artist-vap-find-endpoints-vert x y)))
	    ((eq c ?X)  (append (artist-vap-find-endpoints-swne x y)
				(artist-vap-find-endpoints-nwse x y)))

	    ;; We don't know how to find directions when we are on
	    ;; another character
	    (t nil)))))


(defun artist-vap-group-in-pairs (l)
  "Group elements in list L in pairs."
  (cond ((null l) nil)
	((null (cdr l)) l)		; unevent number of elements in list
	(t (append (list (list (car l) (car (cdr l))))
		   (artist-vap-group-in-pairs (cdr (cdr l)))))))

(defun artist-vaporize-by-endpoints (endpoint1 endpoint2)
  "Given ENDPOINT1 and ENDPOINT2, vaporize the line between them.
An endpoint is a pair (X . Y)."
  (let* ((x1 (car endpoint1))
	 (y1 (cdr endpoint1))
	 (x2 (car endpoint2))
	 (y2 (cdr endpoint2))
	 (dir (artist-find-direction x1 y1 x2 y2))
	 (x-step (aref [1 1 0 -1 -1 -1 0 1] dir))
	 (y-step (aref [0 1 1 1 0 -1 -1 -1] dir))
	 (line-c (aref [?- ?\\ ?| ?/ ?- ?\\ ?| ?/] dir))
	 (line-len (elt (list (abs (- x2 x1))
			      (abs (- x2 x1))
			      (abs (- y2 y1))
			      (abs (- y2 y1))
			      (abs (- x1 x2))
			      (abs (- x1 x2))
			      (abs (- y1 y2))
			      (abs (- y1 y2)))
			dir))
	 (x x1)
	 (y y1))
    (while (>= line-len 0)
      (let* ((buffer-c (artist-get-char-at-xy-conv x y))
	     (new-c (artist-unintersection-char line-c buffer-c)))
	(artist-move-to-xy x y)
	(artist-replace-char new-c))
      (setq x (+ x x-step)
	    y (+ y y-step)
	    line-len (- line-len 1)))))


(defun artist-vaporize-line (x1 y1)
  "Vaporize (erase) the straight line through X1, Y1.
Do this by replacing the characters that forms the line with
`artist-erase-char'.  Output is a list of endpoints for lines through
X1, Y1.  An endpoint is a cons pair, (ENDPOINT-X . ENDPOINT-Y)."
  (let ((endpoints (artist-vap-find-endpoints x1 y1)))
    (mapc
     (lambda (endpoints)
       (let ((ep1 (car endpoints))
	     (ep2 (car (cdr endpoints))))
	 (artist-vaporize-by-endpoints ep1 ep2)))
     (artist-vap-group-in-pairs endpoints))
    endpoints))


;; Implementation note: This depends on artist-vaporize-line doing
;; unintersections of intersecting lines.
;;
;; Example:
;;   Suppose the buffer looks like this and that we start vaporizing
;;   lines at (3,0) (at the ``*'').
;;
;;          0123456
;;         0+--*--+
;;         1|     |
;;         2|     |
;;         3+-----+
;;
;;   We will then push (0,0) and (6,0) on the stack, and vaporize the
;;   topmost horizontal line:
;;
;;          0123456
;;         0|     |
;;         1|     |
;;         2|     |
;;         3+-----+
;;
;;   We will then pop (0,0) and remove the left-most vertical line while
;;   pushing the lower left corner (0,3) on the stack, and so on until
;;   the entire rectangle is vaporized.
;;
;;   Now, What if the `+' in the upper left and upper right corners,
;;   had not been changed to `|' but to spaces instead? We would
;;   have failed when popping (0,0) and vaporizing that line because
;;   we wouldn't find any line at (0,0):
;;
;;          0123456
;;         0
;;         1|     |
;;         2|     |
;;         3+-----+
;;
;;   That's why we depend on artist-vaporize-line doing unintersecting
;;   of crossing lines. There are alternative ways to handle this
;;   if it becomes too much a trouble.
;;
(defun artist-vaporize-lines (x1 y1)
  "Vaporize lines reachable from point X1, Y1."
  (let ((ep-stack nil))
    (mapc
     (lambda (ep) (push ep ep-stack))
     (artist-vap-find-endpoints x1 y1))
    (while (not (null ep-stack))
      (let* ((vaporize-point (pop ep-stack))
	     (new-endpoints (artist-vaporize-line (car vaporize-point)
						  (cdr vaporize-point))))
	(mapc
	 (lambda (endpoint) (push endpoint ep-stack))
	 new-endpoints)))))


;;
;; Circles and ellipses
;;
(defun artist-ellipse-generate-quadrant (x-radius y-radius)
  "Create a point-list for first quadrant.
Points go from (X-RADIUS, 0) to (0, Y-RADIUS).
Quadrant is generated around origin."
  (let* ((rx2 (* x-radius x-radius))
	 (ry2 (* y-radius y-radius))
	 (2rx2 (* 2 rx2))
	 (2ry2 (* 2 ry2))
	 (p)
	 (x 0)
	 (y y-radius)
	 (px 0)
	 (py (* 2rx2 y))
	 (point-list nil))
    (artist-put-pixel point-list x y)
    (setq p (round (+ ry2 (- (* rx2 y-radius)) (* 0.25 rx2))))
    (while (< px py)
      (setq x (1+ x)
	    px (+ px 2ry2))
      (if (< p 0)
	  (setq p (+ p ry2 px))
	(setq y (- y 1)
	      py (- py 2rx2)
	      p (+ p ry2 px (- py))))
      (artist-put-pixel point-list x y))
    (setq p (round (+ (* ry2 (+ x 0.5) (+ x 0.5))
		      (* rx2 (- y 1) (- y 1))
		      (- (* rx2 ry2)))))
    (while (> y 0)
      (setq y (- y 1)
	    py (- py 2rx2))
      (if (> p 0)
	  (setq p (+ p rx2 (- py)))
	(setq x (1+ x)
	      px (+ px 2ry2)
	      p (+ p rx2 (- py) px)))
      (artist-put-pixel point-list x y))
    point-list))

(defsubst artist-new-fill-item (x y width)
  "Create a new item at X, Y, with WIDTH.
This is for use in fill-info in ellipses and circles."
  (let ((new-item (make-vector 3 x)))
    (aset new-item 1 y)
    (aset new-item 2 width)
    new-item))

(defsubst artist-fill-item-get-x (fill-item)
  "Retrieve the x component of a FILL-ITEM."
  (aref fill-item 0))

(defsubst artist-fill-item-set-x (fill-item new-x)
  "Set the x component of a FILL-ITEM to NEW-X."
  (aset fill-item 0 new-x)
  fill-item)

(defsubst artist-fill-item-get-y (fill-item)
  "Retrieve the y component of a FILL-ITEM."
  (aref fill-item 1))

(defsubst artist-fill-item-set-y (fill-item new-y)
  "Set the y component of a FILL-ITEM to NEW-Y."
  (aset fill-item 1 new-y)
  fill-item)

(defsubst artist-fill-item-get-width (fill-item)
  "Retrieve the width component of a FILL-ITEM."
  (aref fill-item 2))

(defsubst artist-fill-item-set-width (fill-item new-width)
  "Set the width component of a FILL-ITEM to NEW-WIDTH."
  (aset fill-item 2 new-width)
  fill-item)


(defun artist-ellipse-point-list-add-center (x-center y-center point-list)
  "Add offsets X-CENTER and Y-CENTER to coordinates in POINT-LIST."
  (mapcar
   (lambda (p)
     (artist-coord-set-x p (+ x-center (artist-coord-get-x p)))
     (artist-coord-set-y p (+ y-center (artist-coord-get-y p))))
   point-list))


(defun artist-ellipse-fill-info-add-center (x-center y-center fill-info)
  "Add offsets X-CENTER and Y-CENTER to fill-items in FILL-INFO."
  (mapcar
   (lambda (p)
     (artist-fill-item-set-x p (+ x-center (artist-fill-item-get-x p)))
     (artist-fill-item-set-y p (+ y-center (artist-fill-item-get-y p))))
   fill-info))

(defun artist-ellipse-remove-0-fills (fill-info)
  "Remove fill-infos from FILL-INFO that fills a zero-width field."
  (cond ((null fill-info)
	 nil)
	((= 0 (artist-fill-item-get-width (car fill-info)))
	 (artist-ellipse-remove-0-fills (cdr fill-info)))
	(t
	 (append (list (car fill-info))
		 (artist-ellipse-remove-0-fills (cdr fill-info))))))


(defun artist-ellipse-compute-fill-info (point-list)
  "Compute fill info for ellipse around 0,0 from POINT-LIST.
The POINT-LIST is expected to cover the first quadrant."
  (let ((first-half nil)
	(both-halves nil)
	(last-y nil))

    ;; Create first half (the lower one (since y grows downwards)) from
    ;; the first quadrant.
    (mapc
     (lambda (coord)
       (let* ((x         (artist-coord-get-x coord))
	      (y         (artist-coord-get-y coord))
	      (width     (max (- (* 2 x) 1) 0))
	      (left-edge (- x width)))
	 (if (or (null last-y) (not (= y last-y)))
	     ;; This was either the first time,
	     ;; or it was the first time on a new line
	     (setq first-half
		   (append first-half
			   ;; Fill info item starts at left-edge on line y
			   (list (artist-new-fill-item left-edge y width)))))
	 (setq last-y y)))
     point-list)

    ;; Create the other half by mirroring the first half.
    (setq both-halves
	  (append first-half
		  (mapc
		   (lambda (i)
		     (artist-new-fill-item (artist-fill-item-get-x i)
					   (- (artist-fill-item-get-y i))
					   (artist-fill-item-get-width i)))
		   ;; The cdr below is so we don't include fill-info for
		   ;;; the middle line twice
		   (cdr (reverse first-half)))))
    (artist-ellipse-remove-0-fills both-halves)))


(defun artist-ellipse-mirror-quadrant (point-list)
  "Mirror a POINT-LIST describing first quadrant to create a complete ellipse."
  (let ((right-half nil)
	(left-half nil))

    ;; First, if last char in that quadrant is `/', then replace it with `)'
    ;; This way we avoids things
    ;;                      ---------                        ---------
    ;;                     /         \                      /         \
    ;; that look like:    \           /  instead we get:   (           )
    ;;                     \         /                      \         /
    ;;                      ---------                        ---------
    (let ((last-coord (car (last point-list))))
      (if (= (artist-coord-get-new-char last-coord) ?/)
	  (artist-coord-set-new-char last-coord artist-ellipse-right-char)))

    ;; Create the other part of the right half by mirroring the first part
    (setq right-half
	  (append
	   point-list
	   (mapcar
	    (lambda (coord)
	      (let ((c (artist-coord-get-new-char coord)))
		(artist-new-coord (artist-coord-get-x coord)
				  (- (artist-coord-get-y coord))
				  (cond ((= c ?/) ?\\)
					((= c ?\\) ?/)
					(t c)))))
	    ;; The cdr below is so we don't draw the middle right char twice
	    (cdr (reverse point-list)))))

    ;; Create the left half by mirroring the right half.
    (setq left-half
	  (mapcar
	   (lambda (coord)
	     (let ((c (artist-coord-get-new-char coord)))
	       (artist-new-coord (- (artist-coord-get-x coord))
				 (artist-coord-get-y coord)
				 (cond ((= c ?/)  ?\\)
				       ((= c ?\\) ?/)
				       ((= c artist-ellipse-right-char)
					artist-ellipse-left-char)
				       (t c)))))
	   ;; The cdr and butlast below is so we don't draw the middle top
	   ;; and middle bottom char twice.
	   (butlast (cdr (reverse right-half)))))
    (append right-half left-half)))


(defun artist-draw-ellipse-general (x1 y1 x-radius y-radius)
  "Draw an ellipse with center at X1, Y1 and X-RADIUS and Y-RADIUS.

Output is an ellipse, which is a list (END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a two-element vector on the form [POINT-LIST FILL-INFO].

POINT-LIST is a list of vectors on the form [X Y SAVED-CHAR NEW-CHAR].
FILL-INFO is a list of vectors on the form [X Y ELLIPSE-WIDTH-ON-THIS-LINE].

Ellipses with zero Y-RADIUS are not drawn correctly."
  (let* ((point-list   (artist-ellipse-generate-quadrant x-radius y-radius))
	 (fill-info    (artist-ellipse-compute-fill-info point-list))
	 (shape-info   (make-vector 2 0)))

    (setq point-list (artist-calculate-new-chars point-list))
    (setq point-list (artist-ellipse-mirror-quadrant point-list))
    (setq point-list (artist-ellipse-point-list-add-center x1 y1 point-list))
    (setq fill-info (artist-ellipse-fill-info-add-center x1 y1 fill-info))

    ;; Draw the ellipse
    (setq point-list
	  (mapcar
	   (lambda (coord)
	     (artist-move-to-xy (artist-coord-get-x coord)
				(artist-coord-get-y coord))
	     (if artist-line-char-set
		 (artist-replace-char artist-line-char)
	       (artist-replace-char (artist-coord-get-new-char coord)))
	     coord)
	   (artist-modify-new-chars
	     (artist-save-chars-under-point-list point-list))))

    (aset shape-info 0 point-list)
    (aset shape-info 1 fill-info)
    (artist-make-2point-object (artist-make-endpoint x1 y1)
			       (artist-make-endpoint x-radius y-radius)
			       shape-info)))

(defun artist-draw-ellipse-with-0-height (x1 y1 x-radius y-radius)
  "Draw an ellipse with center at X1, Y1 and X-RADIUS and Y-RADIUS.

Output is an ellipse, which is a list (END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a two-element vector on the form [POINT-LIST FILL-INFO].

POINT-LIST is a list of vectors on the form [X Y SAVED-CHAR NEW-CHAR].
FILL-INFO is a list of vectors on the form [X Y ELLIPSE-WIDTH-ON-THIS-LINE].

The Y-RADIUS must be 0, but the X-RADIUS must not be 0."
  (let ((point-list nil)
	(width      (max (- (abs (* 2 x-radius)) 1)))
	(left-edge  (1+ (- x1 (abs x-radius))))
	(line-char  (if artist-line-char-set artist-line-char ?-))
	(i          0)
	(point-list nil)
	(fill-info  nil)
	(shape-info (make-vector 2 0)))
    (while (< i width)
      (let* ((line-x (+ left-edge i))
	     (line-y y1)
	     (new-coord (artist-new-coord line-x line-y)))
	(artist-coord-add-saved-char new-coord
				     (artist-get-char-at-xy line-x line-y))
	(artist-move-to-xy line-x line-y)
	(artist-replace-char line-char)
	(setq point-list (append point-list (list new-coord)))
	(setq i (1+ i))))
    (aset shape-info 0 point-list)
    (aset shape-info 1 fill-info)
    (artist-make-2point-object (artist-make-endpoint x1 y1)
			       (artist-make-endpoint x-radius y-radius)
			       shape-info)))

(defun artist-draw-ellipse (x1 y1 x2 y2)
  "Draw an ellipse with center at X1, Y1 and point X2,Y2.

Output is an ellipse, which is a list (END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a two-element vector on the form [POINT-LIST FILL-INFO].

POINT-LIST is a list of vectors on the form [X Y SAVED-CHAR NEW-CHAR].
FILL-INFO is a list of vectors on the form [X Y ELLIPSE-WIDTH-ON-THIS-LINE]."
  (let* ((artist-line-char (artist-compute-line-char))
         (artist-line-char-set artist-line-char)
         (width (abs (- x2 x1)))
	 (height (abs (- y2 y1)))
	 ;;
	 ;; When we draw our ellipse, we want it to go through the cursor
	 ;; position, but since x1,y1, x2,y2 marks the corners of one
	 ;; of the quadrants, we have to enlarge the ellipse a bit.
	 ;; Ok, so then why by sqrt(2)?
	 ;; It comes from the equation for the ellipse (where a is the
	 ;; x-radius and b is the y-radius):
	 ;;	f(x,y) = x^2 / a^2 + y^2 / b^2 - 1 = 0
	 ;; and the fact that we want the enlarged ellipse to have the
	 ;; same proportions as the smaller square, therefore we have:
	 ;;     a/b = x/y
	 ;; Solving this yields a-in-larger-ellipse = a-in-smaller * sqrt(2)
	 (x-radius (round (* width (sqrt 2))))
	 (y-radius (round (* height (sqrt 2))))
	 (x x1)
	 (y y1))
    (if (and (= y1 y2) (not (= x1 x2)))
	(artist-draw-ellipse-with-0-height x y x-radius y-radius)
      (artist-draw-ellipse-general x y x-radius y-radius))))


(defun artist-undraw-ellipse (ellipse)
  "Undraw ELLIPSE."
  (if ellipse
      (let ((point-list (aref (artist-2point-get-shapeinfo ellipse) 0)))
	(mapcar
	 (lambda (coord)
	   (artist-move-to-xy (artist-coord-get-x coord)
			      (artist-coord-get-y coord))
	   (artist-replace-char (artist-coord-get-saved-char coord))
	   coord)
	 point-list))))


(defun artist-draw-circle (x1 y1 x2 y2)
  "Draw a circle with center at X1, Y1 and point X2,Y2.

Output is an ellipse, which is a list (END-POINT-1 END-POINT-2 SHAPE-INFO).

END-POINT-1 and END-POINT-2 are two-element vectors on the form [X Y].
SHAPE-INFO is a two-element vector on the form [POINT-LIST FILL-INFO].

POINT-LIST is a list of vectors on the form [X Y SAVED-CHAR NEW-CHAR].
FILL-INFO is a list of vectors on the form [X Y ELLIPSE-WIDTH-ON-THIS-LINE]."
  (let* ((artist-line-char (artist-compute-line-char))
         (artist-line-char-set artist-line-char)
         (width (abs (- x2 x1)))
	 (height (abs (- y2 y1)))
	 ;; When drawing our circle, we want it to through the cursor
	 ;; just as when drawing the ellipse, but we have to take
	 ;; care for the aspect-ratio.
	 ;; The equation for the ellipse  (where a is the x-radius and
	 ;; b is the y-radius):
	 ;;	f(x,y) = x^2 / a^2 + y^2 / b^2 - 1 = 0
	 ;; together with the relationship
	 ;;     a = aspect-ratio * b
	 ;; gives
	 ;; a = sqrt( x^2 + (aspect-ratio * y)^2 ) and
	 ;; b = a / aspect-ratio
	 (x-radius (round (sqrt (+ (* width width)
				   (* (* artist-aspect-ratio height)
				      (* artist-aspect-ratio height))))))
	 (y-radius (round (/ x-radius artist-aspect-ratio))))
    (artist-draw-ellipse-general x1 y1 x-radius y-radius)))

(defalias 'artist-undraw-circle 'artist-undraw-ellipse)


;
; Filling ellipses
;
(defun artist-fill-ellipse (ellipse x y x-radius y-radius)
  "Fill an ELLIPSE centered at X,Y with radius X-RADIUS and Y-RADIUS."
  (let ((fill-info (aref (artist-2point-get-shapeinfo ellipse) 1)))
    (mapcar
     (lambda (fill-item)
       (artist-move-to-xy (artist-fill-item-get-x fill-item)
			  (artist-fill-item-get-y fill-item))
       (artist-replace-chars artist-fill-char
			    (artist-fill-item-get-width fill-item))
       fill-item)
     fill-info)))

(defalias 'artist-fill-circle 'artist-fill-ellipse)


;;
;; Cutting, copying and pasting rectangles and squares
;; (filling functions)
;;

(defun artist-cut-rect (rect x1 y1 x2 y2)
  "Copy rectangle RECT drawn from X1, Y1 to X2, Y2, then clear it."
  (artist-undraw-rect rect)
  (artist-copy-generic x1 y1 x2 y2)
  (artist-erase-rect rect x1 y1 x2 y2))

(defun artist-cut-square (square x1 y1 x2 y2)
  "Copy a SQUARE drawn from X1, Y1 to X2, Y2 (made square), then clears it."
  (artist-undraw-square square)
  (let* ((square-corners (artist-rect-corners-squarify x1 y1 x2 y2))
	 (new-x1 (elt square-corners 0))
	 (new-y1 (elt square-corners 1))
	 (new-x2 (elt square-corners 2))
	 (new-y2 (elt square-corners 3)))
    (artist-copy-generic new-x1 new-y1 new-x2 new-y2)
    (artist-erase-rect square new-x1 new-y1 new-x2 new-y2)))


(defun artist-get-buffer-contents-at-xy (x y width)
  "Retrieve contents from the buffer at X, Y.  WIDTH characters are returned."
  (artist-move-to-xy x y)
  (let ((here (point))
	(there (save-excursion (artist-move-to-xy (+ x width) y) (point))))
    (untabify here there)
    (setq there (save-excursion (artist-move-to-xy (+ x width) y) (point)))
    (buffer-substring here there)))


(defun artist-copy-generic (x1 y1 x2 y2)
  "Copy a rectangular area with corners at X1, Y1 and X2, Y2.
Output is a copy buffer, a list of strings, representing the
original contents of that area in the buffer."
  (let* ((x    	(min x1 x2))
	 (y    	(min y1 y2))
	 (x-max (max x1 x2))
	 (y-max (max y1 y2))
	 (w     (+ (- x-max x) 1))
	 (l     nil))
    (while (<= y y-max)
      (setq l (cons (artist-get-buffer-contents-at-xy x y w) l))
      (setq y (1+ y)))
    (if artist-interface-with-rect
	(setq killed-rectangle (reverse l))
      (setq artist-copy-buffer (reverse l)))))


(defun artist-copy-rect (rect x1 y1 x2 y2)
  "Copy rectangle RECT drawn from X1, Y1 to X2, Y2."
  (artist-undraw-rect rect)
  (artist-copy-generic x1 y1 x2 y2))

(defun artist-copy-square (square x1 y1 x2 y2)
  "Copy a SQUARE drawn from X1, Y1 to X2, Y2 (but made square)."
  (artist-undraw-square square)
  (let* ((square-corners (artist-rect-corners-squarify x1 y1 x2 y2))
	 (new-x1 (elt square-corners 0))
	 (new-y1 (elt square-corners 1))
	 (new-x2 (elt square-corners 2))
	 (new-y2 (elt square-corners 3)))
    (artist-copy-generic new-x1 new-y1 new-x2 new-y2)))

(defun artist-paste (x y)
  "Paste the contents of the copy-buffer at X,Y."
  (let ((copy-buf (if artist-interface-with-rect
		      killed-rectangle
		    artist-copy-buffer)))
    (if (not (null copy-buf))
	(while (not (null copy-buf))
	  (artist-move-to-xy x y)
	  (artist-replace-string (car copy-buf))
	  (setq copy-buf (cdr copy-buf))
	  (setq y (1+ y)))
      (message "Nothing to paste"))))


;;
;; Flood filling
;;
(defun artist-ff-too-far-right (x)
  "Determine if the position X is too far to the right."
  (cond ((numberp artist-flood-fill-right-border)
	 (> x artist-flood-fill-right-border))
	((eq artist-flood-fill-right-border 'window-width)
	 (> x (- (window-width) 2)))
	((eq artist-flood-fill-right-border 'fill-column)
	 (> x fill-column))
	(t (error "Invalid value for `artist-flood-fill-right-border'"))))

(defun artist-ff-get-rightmost-from-xy (x y)
  "Find the rightmost position in this run, starting at X, Y."
  (save-excursion
    (let ((char-at-xy (artist-get-char-at-xy-conv x y))
	  (last-x x))
      (setq x (1+ x))
      (while (and (not (artist-ff-too-far-right x))
		  (= char-at-xy (artist-get-char-at-xy-conv x y)))
	(setq last-x x)
	(setq x (1+ x)))
      last-x)))

(defun artist-ff-is-topmost-line (x y)
  "Determine whether the position X,Y is on the topmost line or not."
  (= y 0))

(defun artist-ff-is-bottommost-line (x y)
  "Determine whether the position X,Y is on the bottommost line or not."
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (let ((last-line (artist-current-line)))
      (if (= (point) (point-max))

	  ;; Last line is empty, don't paint on it, report previous line
	  ;; as last line
	  (>= y (- last-line 1))
        (>= y last-line)))))

(defun artist-flood-fill (x1 y1)
  "Flood-fill starting at X1, Y1.  Fill with the char in `artist-fill-char'."
  (let ((stack nil)
	(input-queue nil)
	;; We are flood-filling the area that has this character.
	(c     (artist-get-char-at-xy-conv x1 y1))
        (artist-fill-char (if artist-fill-char-set
                              artist-fill-char
                            artist-default-fill-char)))

    ;; Fill only if the fill-char is not the same as the character whose
    ;; area we are about to fill, or, in other words, don't fill if we
    ;; needn't.
    (if (not (= c artist-fill-char))
	(push (artist-new-coord x1 y1) stack))

    (while (not (null stack))
      (let* ((coord (pop stack))
	     (x (artist-coord-get-x coord))
	     (y (artist-coord-get-y coord))

	     ;; Here we keep track of the leftmost and rightmost position
	     ;; for this run
	     (x-leftmost 0)
	     (x-rightmost 0)
	     (last-x 0)

	     ;; Remember if line above and below are accessible
	     ;; Lines below the last one, and prior to the first-one
	     ;; are not accessible.
	     (lines-above nil)
	     (lines-below nil)

	     ;; Remember char for position on line above and below, so we
	     ;; can find the rightmost positions on the runs.
	     (last-c-above -1)
	     (last-c-below -1))

	(setq x-rightmost (artist-ff-get-rightmost-from-xy x y))
	(setq lines-above (not (artist-ff-is-topmost-line x y)))
	(setq lines-below (not (artist-ff-is-bottommost-line x y)))
	(setq last-x x-rightmost)
	(setq x x-rightmost)

	;; Search line above, push rightmost positions of runs for that line
	(while (and (>= x 0) (= c (artist-get-char-at-xy-conv x y)))
	  (if lines-above
	      (let ((c-above (artist-get-char-at-xy-conv x (- y 1))))
		(if (and (= c-above c) (/= c-above last-c-above))
		    (push (artist-new-coord x (- y 1)) stack))
		(setq last-c-above c-above)))
	  (setq last-x x)
	  (setq x (- x 1)))

	;; Remember the left-most position on this run
	(setq x-leftmost last-x)

	;; Search line below, push rightmost positions of runs for that line
	(setq x x-rightmost)
	(while (>= x x-leftmost)
	  (if lines-below
	      (let ((c-below (artist-get-char-at-xy-conv x (1+ y))))
		(if (and (= c-below c) (/= c-below last-c-below))
		    (push (artist-new-coord x (1+ y)) stack))
		(setq last-c-below c-below)))
	  (setq x (- x 1)))

	(artist-move-to-xy x-leftmost y)
	(artist-replace-chars artist-fill-char (1+ (- x-rightmost x-leftmost)))

	;; If we are to show incrementally, we have to remove any pending
	;; input from the input queue, because processing of pending input
	;; always has priority over display updates (although this input
	;; won't be processed until we are done). Later on we will queue
	;; the input on the input queue again.
	(if artist-flood-fill-show-incrementally
	    (progn
	      (if (input-pending-p)
		  (discard-input))
	      (artist-update-display)))))))

;;
;; Accessors to arrow-points
;;

(defun artist-make-arrow-point (x y direction &optional state)
  "Create an arrow point at X, Y for a line in direction DIRECTION.
Optional argument STATE can be used to set state (default is nil)."
  (save-excursion
    (let* ((arrow-point  (make-vector 4 0))
	   (arrow-marker (make-marker)))
      (artist-move-to-xy x y)
      (set-marker arrow-marker (point))
      (aset arrow-point 0 arrow-marker)
      (aset arrow-point 1 (artist-get-char-at-xy x y))
      (aset arrow-point 2 direction)
      (aset arrow-point 3 state)
      arrow-point)))

(defsubst artist-arrow-point-get-marker (arrow-point)
  "Retrieve the marker component of an ARROW-POINT."
  (aref arrow-point 0))

(defsubst artist-arrow-point-get-orig-char (arrow-point)
  "Retrieve the orig char component of an ARROW-POINT."
  (aref arrow-point 1))

(defsubst artist-arrow-point-get-direction (arrow-point)
  "Retrieve the direction component of an ARROW-POINT."
  (aref arrow-point 2))

(defsubst artist-arrow-point-get-state (arrow-point)
  "Retrieve the state component of an ARROW-POINT."
  (aref arrow-point 3))

(defsubst artist-arrow-point-set-state (arrow-point new-state)
  "Set the state component of an ARROW-POINT to NEW-STATE."
  (aset arrow-point 3 new-state))


(defun artist-clear-arrow-points ()
  "Clear current endpoints."
  (setq artist-arrow-point-1 nil)
  (setq artist-arrow-point-2 nil))

(defun artist-set-arrow-points-for-poly (point-list)
  "Generic function for setting arrow-points for poly-shapes from POINT-LIST."
  (let* ((ep1   (elt point-list 0))
	 (ep2   (elt point-list 1))
	 (x1    (artist-endpoint-get-x ep1))
	 (y1    (artist-endpoint-get-y ep1))
	 (x2    (artist-endpoint-get-x ep2))
	 (y2    (artist-endpoint-get-y ep2))
	 (dir1  (artist-find-direction x2 y2 x1 y1))
	 (epn   (car (last point-list)))
	 (epn-1 (car (last point-list 2)))
	 (xn    (artist-endpoint-get-x epn))
	 (yn    (artist-endpoint-get-y epn))
	 (xn-1  (artist-endpoint-get-x epn-1))
	 (yn-1  (artist-endpoint-get-y epn-1))
	 (dirn  (artist-find-direction xn-1 yn-1 xn yn)))
    (setq artist-arrow-point-1 (artist-make-arrow-point x1 y1 dir1))
    (setq artist-arrow-point-2 (artist-make-arrow-point xn yn dirn))))


(defun artist-set-arrow-points-for-2points (shape x1 y1 x2 y2)
  "Generic function for setting arrow-points for 2-point shapes.
The 2-point shape SHAPE is drawn from X1, Y1 to X2, Y2."
  (let* ((endpoint1 (artist-2point-get-endpoint1 shape))
	 (endpoint2 (artist-2point-get-endpoint2 shape))
	 (x1        (artist-endpoint-get-x endpoint1))
	 (y1        (artist-endpoint-get-y endpoint1))
	 (x2        (artist-endpoint-get-x endpoint2))
	 (y2        (artist-endpoint-get-y endpoint2)))
    (setq artist-arrow-point-1
	  (artist-make-arrow-point x1 y1
				   (artist-find-direction x2 y2 x1 y1)))
    (setq artist-arrow-point-2
	  (artist-make-arrow-point x2 y2
				   (artist-find-direction x1 y1 x2 y2)))))


;;
;; Common routine for drawing/undrawing shapes based
;; on the draw-how
;;

(defun artist-key-undraw-continously (x y)
  "Undraw current continuous shape with point at X, Y."
  ;; No undraw-info for continuous shapes
  nil)

(defun artist-key-undraw-poly (x y)
  "Undraw current poly shape with point at X, Y."
  (let ((undraw-fn (artist-go-get-undraw-fn-from-symbol artist-curr-go))
	(x1        (artist-endpoint-get-x artist-key-endpoint1))
	(y1        (artist-endpoint-get-y artist-key-endpoint1)))
    (artist-funcall undraw-fn artist-key-shape)))

(defun artist-key-undraw-1point (x y)
  "Undraw current 1-point shape at X, Y."
  ;; No undraw-info for 1-point shapes
  nil)

(defun artist-key-undraw-2points (x y)
  "Undraw current 2-point shape at X, Y."
  (let ((undraw-fn (artist-go-get-undraw-fn-from-symbol artist-curr-go))
	(x1        (artist-endpoint-get-x artist-key-endpoint1))
	(y1        (artist-endpoint-get-y artist-key-endpoint1)))
    (artist-funcall undraw-fn artist-key-shape)))

(defun artist-key-undraw-common ()
  "Common routine undrawing current shape."
 (let ((draw-how (artist-go-get-draw-how-from-symbol artist-curr-go))
       (col      (artist-current-column))
       (row      (artist-current-line)))

    ;; Depending on what we are currently drawing, call other routines
    ;; that knows how to do the job
    ;;
    (cond ((eq draw-how 'artist-do-continously)
	   (artist-key-undraw-continously col row))
	  ((eq draw-how 'artist-do-poly)
	   (artist-key-undraw-poly col row))
	  ((and (numberp draw-how) (= draw-how 1))
	   (artist-key-undraw-1point col row))
	  ((and (numberp draw-how) (= draw-how 2))
	   (artist-key-undraw-2points col row))
	  (t (message "Undrawing \"%s\"s is not yet implemented" draw-how)))

    ;; Now restore the old position
    ;;
    (artist-move-to-xy col row)))



;; Implementation note: This really should honor the interval-fn entry
;; in the master table, `artist-mt', which would mean leaving a timer
;; that calls `draw-fn' every now and then. That timer would then have
;; to be canceled and reinstalled whenever the user moves the cursor.
;; This could be done, but what if the user suddenly switches to another
;; drawing mode, or even kills the buffer! In the mouse case, it is much
;; simpler: when at the end of `artist-mouse-draw-continously', the
;; user has released the button, so the timer will always be canceled
;; at that point.
(defun artist-key-draw-continously (x y)
  "Draw current continuous shape at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go)))
    (setq artist-key-shape (artist-funcall draw-fn x y))))

(defun artist-key-draw-poly (x y)
  "Draw current poly-point shape with nth point at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(x1        (artist-endpoint-get-x artist-key-endpoint1))
	(y1        (artist-endpoint-get-y artist-key-endpoint1)))
    (setq artist-key-shape (artist-funcall draw-fn x1 y1 x y))))

(defun artist-key-draw-1point (x y)
  "Draw current 1-point shape at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go)))
    (setq artist-key-shape (artist-funcall draw-fn x y))))


(defun artist-key-draw-2points (x y)
  "Draw current 2-point shape at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(x1        (artist-endpoint-get-x artist-key-endpoint1))
	(y1        (artist-endpoint-get-y artist-key-endpoint1)))
    (setq artist-key-shape (artist-funcall draw-fn x1 y1 x y))))

(defun artist-key-draw-common ()
  "Common routine for drawing current shape."
  (let ((draw-how (artist-go-get-draw-how-from-symbol artist-curr-go))
	(col      (artist-current-column))
	(row      (artist-current-line)))

    ;; Depending on what we are currently drawing, call other routines
    ;; that knows how to do the job
    ;;
    (cond ((eq draw-how 'artist-do-continously)
	   (artist-key-draw-continously col row))
	  ((eq draw-how 'artist-do-poly)
	   (artist-key-draw-poly col row))
	  ((and (numberp draw-how) (= draw-how 1))
	   (artist-key-draw-1point col row))
	  ((and (numberp draw-how) (= draw-how 2))
	   (artist-key-draw-2points col row))
	  (t (message "Drawing \"%s\"s is not yet implemented" draw-how)))

    ;; Now restore the old position
    ;;
    (artist-move-to-xy col row)))



;;
;; Functions related to trimming line-endings
;; The region between the topmost and bottommost visited line is
;; called a draw-region.
;;

(defun artist-draw-region-reset ()
  "Reset the current draw-region."
  (setq artist-draw-region-max-y 0)
  (setq artist-draw-region-min-y 1000000))

(defun artist-draw-region-trim-line-endings (min-y max-y)
  "Trim lines in current draw-region from MIN-Y to MAX-Y.
Trimming here means removing white space at end of a line."
  ;; Safety check: switch min-y and max-y if max-y is smaller
  (if (< max-y min-y)
      (let ((tmp min-y))
	(setq min-y max-y)
	(setq max-y tmp)))
  (save-excursion
    (let ((curr-y min-y))
      (while (<= curr-y max-y)
	(artist-move-to-xy 0 curr-y)
	(end-of-line)
	(delete-horizontal-space)
	(setq curr-y (1+ curr-y))))))

;;
;; Drawing shapes by using keys
;;

(defun artist-key-do-continously-continously (x y)
  "Update current continuous shape at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go)))
    (artist-funcall draw-fn x y)))


(defun artist-key-do-continously-poly (x y)
  "Update current poly-point shape with nth point at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(undraw-fn (artist-go-get-undraw-fn-from-symbol artist-curr-go))
	(x1        (artist-endpoint-get-x artist-key-endpoint1))
	(y1        (artist-endpoint-get-y artist-key-endpoint1))
	(x2        x)
	(y2        y))
    ;; If not rubber-banding, then move the 2
    ;; Otherwise re-draw the shape to the new position
    ;;
    (if (not artist-rubber-banding)
	(progn
	  (artist-no-rb-unset-point2)
	  (artist-no-rb-set-point2 x y))
      (progn
	(artist-funcall undraw-fn artist-key-shape)
	(setq artist-key-shape (artist-funcall draw-fn x1 y1 x2 y2))))))


(defun artist-key-do-continously-1point (x y)
  "Update current 1-point shape at X,Y."
  ;; Nothing to do continuously for operations
  ;; where we have only one input point
  nil)

(defun artist-key-do-continously-2points (x y)
  "Update current 2-point shape with 2nd point at X,Y."
  (let ((draw-fn   (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(undraw-fn (artist-go-get-undraw-fn-from-symbol artist-curr-go))
	(x1        (artist-endpoint-get-x artist-key-endpoint1))
	(y1        (artist-endpoint-get-y artist-key-endpoint1))
	(x2        x)
	(y2        y))
    ;; If not rubber-banding, then move the 2
    ;; Otherwise re-draw the shape to the new position
    ;;
    (if (not artist-rubber-banding)
	(progn
	  (artist-no-rb-unset-point2)
	  (artist-no-rb-set-point2 x y))
      (progn
	(artist-funcall undraw-fn artist-key-shape)
	(setq artist-key-shape (artist-funcall draw-fn x1 y1 x2 y2))))))


(defun artist-key-do-continously-common ()
  "Common routine for updating current shape."
  (let ((draw-how (artist-go-get-draw-how-from-symbol artist-curr-go))
	(col      (artist-current-column))
	(row      (artist-current-line)))

    ;; Depending on what we are currently drawing, call other routines
    ;; that knows how to do the job
    ;;
    (cond ((eq draw-how 'artist-do-continously)
	   (artist-key-do-continously-continously col row))
	  ((eq draw-how 'artist-do-poly)
	   (artist-key-do-continously-poly col row))
	  ((and (numberp draw-how) (= draw-how 1))
	   (artist-key-do-continously-1point col row))
	  ((and (numberp draw-how) (= draw-how 2))
	   (artist-key-do-continously-2points col row))
	  (t (message "Drawing \"%s\"s is not yet implemented" draw-how)))

    ;; Now restore the old position
    ;;
    (artist-move-to-xy col row)))


(defun artist-key-set-point-continously (x y)
  "Set point for current continuous shape at X,Y."
  ;; Maybe set arrow-points for continuous shapes
  (let ((arrow-pred   (artist-go-get-arrow-pred-from-symbol artist-curr-go))
	(arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol artist-curr-go))
	(init-fn      (artist-go-get-init-fn-from-symbol artist-curr-go))
	(prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol artist-curr-go))
	(exit-fn      (artist-go-get-exit-fn-from-symbol artist-curr-go)))

    (if (not artist-key-is-drawing)
	;; *** We are about to begin drawing
	(progn
	  (artist-funcall init-fn x y))

      ;; *** We are about to stop drawing
      (progn

	(artist-funcall prep-fill-fn x y)
	(if (artist-funcall arrow-pred)
	    (artist-funcall arrow-set-fn x y)
	  (artist-clear-arrow-points))
	(artist-funcall exit-fn x y))))

  ;; Toggle the is-drawing flag
  (setq artist-key-is-drawing (not artist-key-is-drawing)))



(defun artist-key-set-point-poly (x y &optional this-is-last-point)
  "Set point for current poly-point shape at X,Y.
If optional argument THIS-IS-LAST-POINT is non-nil, this point is the last."
  (let ((draw-fn      (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(init-fn      (artist-go-get-init-fn-from-symbol artist-curr-go))
	(prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol artist-curr-go))
	(exit-fn      (artist-go-get-exit-fn-from-symbol artist-curr-go))
	(fill-pred    (artist-go-get-fill-pred-from-symbol artist-curr-go))
	(fill-fn      (artist-go-get-fill-fn-from-symbol artist-curr-go))
	(arrow-pred   (artist-go-get-arrow-pred-from-symbol artist-curr-go))
	(arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol artist-curr-go)))

    (if (not artist-key-is-drawing)

	;; *** We were not drawing ==> set first point
	(progn

	  (artist-funcall init-fn x y)

	  ;; If not rubber-banding, set first point.
	  ;; Otherwise, draw the shape from x,y to x,y
	  (if (not artist-rubber-banding)
	      (artist-no-rb-set-point1 x y)
	    (setq artist-key-shape (artist-funcall draw-fn x y x y)))

	  ;; Set first endpoint
	  (setq artist-key-endpoint1 (artist-make-endpoint x y))

	  ;; Set point-list to contain start point
	  (setq artist-key-poly-point-list (list (artist-make-endpoint x y)))

	  ;; Since we are not ready, set the arrow-points to nil
	  (artist-clear-arrow-points)

	  ;; Change state to drawing
	  (setq artist-key-is-drawing t)

	  ;; Feedback
	  (message "%s" (substitute-command-keys
		    (concat "First point set. "
			    "Set next with \\[artist-key-set-point], "
			    "set last with C-u \\[artist-key-set-point]"))))


      ;; *** We were drawing ==> we are about to set nth point
      ;;     (last point if the argument this-is-last-point is non-nil)
      ;;
      (let ((x1 (artist-endpoint-get-x artist-key-endpoint1))
	    (y1 (artist-endpoint-get-y artist-key-endpoint1))
	    (x2 x)
	    (y2 y))

	;; If not rubber-banding, undraw the 1's and 2's, then
	;; draw the shape (if we were rubber-banding, then the
	;; shape is already drawn in artist-key-do-continously-2points.)
	;;
	(if (not artist-rubber-banding)
	    (progn
	      (artist-no-rb-unset-points)
	      (setq artist-key-shape (artist-funcall draw-fn x1 y1 x2 y2))))

	;; Set x2 and y2 from shape's second point
	;; (which might be different from the mouse's second point,
	;; if, for example, we are drawing a straight line)
	;;
	(if (not (null artist-key-shape))
	    (let ((endpoint2 (artist-2point-get-endpoint2 artist-key-shape)))
	      (setq x2 (artist-endpoint-get-x endpoint2))
	      (setq y2 (artist-endpoint-get-y endpoint2))))

	;; Add the endpoint to the list of poly-points
	(setq artist-key-poly-point-list
	      (append artist-key-poly-point-list
		      (list (artist-make-endpoint x2 y2))))

	;; Now do handle the case when this is the last point,
	;; and the case when this point isn't the last
	;;
	(if (not this-is-last-point)
	    ;; ** This is not the last point
	    (progn
	      ;; Start drawing a new 2-point-shape from last endpoint.

	      ;; First set the start-point
	      (setq x1 x2)
	      (setq y1 y2)
	      (setq artist-key-endpoint1 (artist-make-endpoint x1 y1))

	      ;; If we are not rubber-banding, then place the '1
	      ;; Otherwise, draw the shape from x1,y1 to x1,y1
	      (if (not artist-rubber-banding)
		  (artist-no-rb-set-point1 x1 y1)
		(setq artist-key-shape (artist-funcall draw-fn x1 y1 x1 y1)))

	      ;; Feedback
	      (message "Point set"))

	  ;; ** This is the last point
	  (progn

	    (artist-funcall prep-fill-fn artist-key-poly-point-list)

	    ;; Maybe fill
	    (if (artist-funcall fill-pred)
		(artist-funcall fill-fn artist-key-shape
				artist-key-poly-point-list))

	    ;; Set the arrow-points
	    (if (artist-funcall arrow-pred)
		(artist-funcall arrow-set-fn artist-key-poly-point-list)
	      (artist-clear-arrow-points))

	    (artist-funcall exit-fn artist-key-poly-point-list)

	    ;; Change state to not drawing
	    (setq artist-key-shape nil)
	    (setq artist-key-endpoint1 nil)
	    (setq artist-key-is-drawing nil)))))))


(defun artist-key-set-point-1point (x y)
  "Set point for current 1-point shape at X,Y."
  (let ((draw-fn      (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(init-fn      (artist-go-get-init-fn-from-symbol artist-curr-go))
	(prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol artist-curr-go))
	(exit-fn      (artist-go-get-exit-fn-from-symbol artist-curr-go))
	(draw-fn      (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(arrow-pred   (artist-go-get-arrow-pred-from-symbol artist-curr-go))
	(arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol artist-curr-go)))
    (artist-funcall init-fn x y)
    (artist-funcall draw-fn x y)
    (artist-funcall prep-fill-fn x y)
    (if (artist-funcall arrow-pred)
	(artist-funcall arrow-set-fn x y)
      (artist-clear-arrow-points))
    (artist-funcall exit-fn x y))
  (setq artist-key-shape nil)
  (setq artist-key-is-drawing nil))


(defun artist-key-set-point-2points (x y)
  "Set first or second point in current 2-point shape at X,Y."
  (let ((draw-fn      (artist-go-get-draw-fn-from-symbol artist-curr-go))
	(init-fn      (artist-go-get-init-fn-from-symbol artist-curr-go))
	(prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol artist-curr-go))
	(exit-fn      (artist-go-get-exit-fn-from-symbol artist-curr-go))
	(fill-pred    (artist-go-get-fill-pred-from-symbol artist-curr-go))
	(fill-fn      (artist-go-get-fill-fn-from-symbol artist-curr-go))
	(arrow-pred   (artist-go-get-arrow-pred-from-symbol artist-curr-go))
	(arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol artist-curr-go)))
    (if (not artist-key-is-drawing)

	;; *** We were not drawing ==> set first point
	(progn

	  (artist-funcall init-fn x y)

	  ;; If not rubber-banding, set first point.
	  ;; Otherwise, draw the shape from x,y to x,y
	  (if (not artist-rubber-banding)
	      (artist-no-rb-set-point1 x y)
	    (setq artist-key-shape (artist-funcall draw-fn x y x y)))

	  ;; Set first endpoint
	  (setq artist-key-endpoint1 (artist-make-endpoint x y))

	  ;; Since we are not ready, clear the arrow-points
	  (artist-clear-arrow-points)

	  ;; Change state to drawing
	  (setq artist-key-is-drawing t))

      ;; *** We were drawing ==> we are about to set 2nd point
      ;;     and end the drawing operation

      (let ((x1 (artist-endpoint-get-x artist-key-endpoint1))
	    (y1 (artist-endpoint-get-y artist-key-endpoint1))
	    (x2 x)
	    (y2 y))

	;; If not rubber-banding, undraw the 1's and 2's, then
	;; draw the shape (if we were rubber-banding, then the
	;; shape is already drawn in artist-key-do-continously-2points.)
	;;
	(if (not artist-rubber-banding)
	    (progn
	      (artist-no-rb-unset-points)
	      (setq artist-key-shape (artist-funcall draw-fn x1 y1 x2 y2))))

	(artist-funcall prep-fill-fn artist-key-shape x1 y1 x2 y2)

	;; Maybe fill
	;;
	(if (artist-funcall fill-pred)
	    (artist-funcall fill-fn artist-key-shape x1 y1 x2 y2))

	;; Maybe set the arrow-points
	;;
	(if (artist-funcall arrow-pred)
	    (artist-funcall arrow-set-fn artist-key-shape x1 y1 x2 y2)
	  (artist-clear-arrow-points))

	(artist-funcall exit-fn artist-key-shape x1 y1 x2 y2)

	;; Change state to not drawing
	(setq artist-key-is-drawing nil)))))


(defun artist-key-set-point-common (arg)
  "Common routine for setting point in current shape.
With non-nil ARG, set the last point."
  (let ((draw-how    (artist-go-get-draw-how-from-symbol artist-curr-go))
	(col         (artist-current-column))
	(row         (artist-current-line))
	(was-drawing artist-key-is-drawing))

    ;; First, if we are about to draw, then reset the draw-region
    (if (not artist-key-is-drawing)
	(artist-draw-region-reset))

    ;; Depending on what we are currently drawing, call other routines
    ;; that knows how to do the job
    ;;
    (cond ((eq draw-how 'artist-do-continously)
	   (artist-key-set-point-continously col row)
	   ;; Do this now, otherwise nothing will happen until we move.
	   (artist-key-do-continously-continously col row))
	  ((eq draw-how 'artist-do-poly)
	   (artist-key-set-point-poly col row arg))
	  ((and (numberp draw-how) (= draw-how 1))
	   (artist-key-set-point-1point col row))
	  ((and (numberp draw-how) (= draw-how 2))
	   (artist-key-set-point-2points col row))
	  (t (message "Drawing \"%s\"s is not yet implemented" draw-how)))

    ;; Maybe trim line endings
    (if (and artist-trim-line-endings
	     was-drawing
	     (not artist-key-is-drawing))
	(artist-draw-region-trim-line-endings artist-draw-region-min-y
					      artist-draw-region-max-y))

    ;; Now restore the old position
    ;;
    (artist-move-to-xy col row)
    (artist-mode-line-show-curr-operation artist-key-is-drawing)))

;;
;; Key navigation
;;

(defun artist-previous-line (&optional n)
  "Move cursor up N lines (default is 1), updating current shape.
If N is negative, move cursor down."
  (interactive "p")
  (let ((col (artist-current-column)))
    (forward-line (- n))
    (move-to-column col t))
  (when artist-key-is-drawing
    (artist-key-do-continously-common)))


(defun artist-next-line (&optional n)
  "Move cursor down N lines (default is 1), updating current shape.
If N is negative, move cursor up."
  (interactive "p")
  (let ((col (artist-current-column)))
    (forward-line n)
    (move-to-column col t))
  (when artist-key-is-drawing
    (artist-key-do-continously-common)))

(defun artist-backward-char (&optional n)
  "Move cursor backward N chars (default is 1), updating current shape.
If N is negative, move forward."
  (interactive "p")
  (if (> n 0)
      (artist-forward-char (- n))
    (artist-forward-char n)))

(defun artist-forward-char (&optional n)
  "Move cursor forward N chars (default is 1), updating current shape.
If N is negative, move backward."
  (interactive "p")
  (let* ((step-x   (if (>= n 0) 1 -1))
	 (distance (abs n))
	 (curr-col (artist-current-column))
	 (new-col  (max 0 (+ curr-col (* distance step-x)))))
    (if (not artist-key-is-drawing)
	  (move-to-column new-col t)
      (move-to-column new-col t)
      (artist-key-do-continously-common))))


(defun artist-key-set-point (&optional arg)
  "Set a point for the current shape.  With optional ARG, set the last point."
  (interactive "P")
  (artist-key-set-point-common arg))


(defun artist-select-fill-char (c)
  "Set current fill character to be C."
  (interactive "cType fill char (type RET to turn off): ")
  (cond ((eq c ?\r) (setq artist-fill-char-set nil)
		    (message "Fill canceled"))
	(t	    (setq artist-fill-char-set t)
		    (setq artist-fill-char c)
		    (message "Fill set to \"%c\"" c))))


(defun artist-select-line-char (c)
  "Set current line character to be C."
  (interactive "cType line char (type RET to turn off): ")
  (cond ((eq c ?\r) (setq artist-line-char-set nil)
		    (message "Normal lines"))
	(t	    (setq artist-line-char-set t)
		    (setq artist-line-char c)
		    (message "Line drawn with \"%c\"" c)))
  (if artist-key-is-drawing
      (artist-key-do-continously-common)))


(defun artist-select-erase-char (c)
  "Set current erase character to be C."
  (interactive "cType char to use when erasing (type RET for normal): ")
  (cond ((eq c ?\r) (setq artist-erase-char ?\s)
		    (message "Normal erasing"))
	(t	    (setq artist-erase-char c)
		    (message "Erasing with \"%c\"" c)))
  (if artist-key-is-drawing
      (artist-key-do-continously-common)))

(defun artist-charlist-to-string (char-list)
  "Convert a list of characters, CHAR-LIST, to a string."
  (concat char-list))

(defun artist-string-to-charlist (str)
  "Convert a string, STR, to list of characters."
  (append str nil))

(defun artist-select-spray-chars (chars initial-char)
  "Set current spray characters to be CHARS, starting with INITIAL-CHAR."
  ;; This huge unreadable `interactive'-clause does the following
  ;; 1. Asks for a string of spray-characters
  ;; 2. Asks for the initial character (default is the first),
  ;;    and loops if the answer is not a char within the string in 1.
  (interactive
   (let* ((str  (read-string "Select spray-can characters, lightest first: "
			     (artist-charlist-to-string artist-spray-chars)))
	  (char-list (artist-string-to-charlist str))
	  (initial (let* ((err-msg "")
			  (ok nil)
			  (first-char-as-str (char-to-string (car char-list)))
			  (first-s) (first-c))
		     (while (not ok)
		       (setq first-s
			     (read-string
			      (format (concat "%sSelect initial-character, "
					      "one of \"%s\" (%s): ")
				      err-msg str first-char-as-str)))
		       (if (equal first-s "")
			   (setq first-s first-char-as-str))
		       (setq first-c (car (artist-string-to-charlist first-s)))
		       (setq ok (not (null (member first-c char-list))))
		       (if (not ok)
			   (setq err-msg (format
					  "Not in spray-chars: \"%s\". "
					  (char-to-string first-c)))))
		     first-c)))
     (list char-list initial)))
  (setq artist-spray-chars chars)
  (setq artist-spray-new-char initial-char)
  (message "Spray-chars set to \"%s\", initial: \"%s\""
	   (artist-charlist-to-string chars) (char-to-string initial-char)))


(defun artist-select-operation (op-str)
  "Select drawing operation OP-STR."
  (interactive (list (completing-read "Select operation: "
				      artist-key-compl-table)))
  (let* ((op-symbol (artist-mt-get-symbol-from-keyword op-str))
	 (draw-how  (if op-symbol
			(artist-go-get-draw-how-from-symbol op-symbol)
		      nil)))
    ;; First check that the string was valid
    (if (null op-symbol)
	(error "Unknown drawing method: %s" op-str))

    ;; Second, check that we are not about to switch to a different
    ;; kind of shape (do that only if we are drawing with keys;
    ;; otherwise this function cannot get called).
    (if (and artist-key-is-drawing
	     (not (equal artist-key-draw-how draw-how)))
	(error "Cannot switch to a different kind of shape while drawing"))

    ;; If we were drawing, undraw the shape
    (if (and artist-key-is-drawing
	     artist-rubber-banding)
	(artist-key-undraw-common))

    ;; Set the current operation and draw-how
    (setq artist-curr-go op-symbol)
    (setq artist-key-draw-how draw-how)

    ;; If we were drawing, redraw the shape (but don't if shape
    ;; is drawn by setting only one point)
    (if (and artist-key-is-drawing
	     artist-rubber-banding
	     (not (eq artist-key-draw-how 1)))
	(artist-key-draw-common)))

  ;; Feedback
  (artist-mode-line-show-curr-operation artist-key-is-drawing))


(defun artist-toggle-rubber-banding (&optional state)
  "Toggle rubber-banding.
If optional argument STATE is positive, turn rubber-banding on."
  (interactive)
  (if artist-key-is-drawing
      (error "Cannot toggle rubber-banding while drawing"))
  (if (setq artist-rubber-banding
	    (if (null state) (not artist-rubber-banding)
	      (> (prefix-numeric-value state) 0)))
      (message "Rubber-banding is now on")
    (message "Rubber-banding is now off")))


(defun artist-toggle-trim-line-endings (&optional state)
  "Toggle trimming of line-endings.
If optional argument STATE is positive, turn trimming on."
  (interactive)
  (if (setq artist-trim-line-endings
	    (if (null state) (not artist-trim-line-endings)
	      (> (prefix-numeric-value state) 0)))
      (message "Trimming is now on")
    (message "Trimming is now off")))


(defun artist-toggle-borderless-shapes (&optional state)
  "Toggle borders of shapes.
If optional argument STATE is positive, turn borders on."
  (interactive)
  (if (setq artist-borderless-shapes
	    (if (null state) (not artist-borderless-shapes)
	      (> (prefix-numeric-value state) 0)))
      (message "Borders are now off")
    (message "Borders are now on")))


(defun artist-toggle-first-arrow ()
  "Toggle first arrow for shape, if possible."
  (interactive)
  (save-excursion
    (if (not (null artist-arrow-point-1))
	(let* ((arrow-point  artist-arrow-point-1)
	       (arrow-state  (artist-arrow-point-get-state arrow-point))
	       (arrow-marker (artist-arrow-point-get-marker arrow-point))
	       (direction    (artist-arrow-point-get-direction arrow-point))
	       (orig-char    (artist-arrow-point-get-orig-char arrow-point))
	       (arrow-char   (aref artist-arrows direction))
	       (new-state    (not arrow-state)))

	  (goto-char (marker-position arrow-marker))

	  (if new-state
	      (if arrow-char
		  (artist-replace-char arrow-char))
	    (artist-replace-char orig-char))

	  (artist-arrow-point-set-state artist-arrow-point-1 new-state)))))

(defun artist-toggle-second-arrow ()
  "Toggle second arrow for shape, if possible."
  (interactive)
  (save-excursion
    (if (not (null artist-arrow-point-2))
	(let* ((arrow-point  artist-arrow-point-2)
	       (arrow-state  (artist-arrow-point-get-state arrow-point))
	       (arrow-marker (artist-arrow-point-get-marker arrow-point))
	       (direction    (artist-arrow-point-get-direction arrow-point))
	       (orig-char    (artist-arrow-point-get-orig-char arrow-point))
	       (arrow-char   (aref artist-arrows direction))
	       (new-state    (not arrow-state)))

	  (goto-char (marker-position arrow-marker))

	  (if new-state
	      (if arrow-char
		  (artist-replace-char arrow-char))
	    (artist-replace-char orig-char))

	  (artist-arrow-point-set-state artist-arrow-point-2 new-state)))))

(defun artist-select-op-pen-line ()
  "Select drawing pen lines."
  (interactive)
  (artist-select-operation "Pen Line"))

(defun artist-select-op-line ()
  "Select drawing lines."
  (interactive)
  (artist-select-operation "line"))

(defun artist-select-op-straight-line ()
  "Select drawing straight lines."
  (interactive)
  (artist-select-operation "straight line"))

(defun artist-select-op-rectangle ()
  "Select drawing rectangles."
  (interactive)
  (artist-select-operation "rectangle"))

(defun artist-select-op-square ()
  "Select drawing squares."
  (interactive)
  (artist-select-operation "square"))

(defun artist-select-op-poly-line ()
  "Select drawing poly-lines."
  (interactive)
  (artist-select-operation "poly-line"))

(defun artist-select-op-straight-poly-line ()
  "Select drawing straight poly-lines."
  (interactive)
  (artist-select-operation "straight poly-line"))

(defun artist-select-op-ellipse ()
  "Select drawing ellipses."
  (interactive)
  (artist-select-operation "ellipse"))

(defun artist-select-op-circle ()
  "Select drawing circles."
  (interactive)
  (artist-select-operation "circle"))

(defun artist-select-op-text-see-thru ()
  "Select rendering text (see thru)."
  (interactive)
  (artist-select-operation "text see-thru"))

(defun artist-select-op-text-overwrite ()
  "Select rendering text (overwrite)."
  (interactive)
  (artist-select-operation "text overwrite"))

(defun artist-select-op-spray-can ()
  "Select spraying."
  (interactive)
  (artist-select-operation "spray-can"))

(defun artist-select-op-spray-set-size ()
  "Select setting size for spraying."
  (interactive)
  (artist-select-operation "spray set size"))

(defun artist-select-op-erase-char ()
  "Select erasing characters."
  (interactive)
  (artist-select-operation "erase char"))

(defun artist-select-op-erase-rectangle ()
  "Select erasing rectangles."
  (interactive)
  (artist-select-operation "erase rectangle"))

(defun artist-select-op-vaporize-line ()
  "Select vaporizing single lines."
  (interactive)
  (artist-select-operation "vaporize line"))

(defun artist-select-op-vaporize-lines ()
  "Select vaporizing connected lines."
  (interactive)
  (artist-select-operation "vaporize lines"))

(defun artist-select-op-cut-rectangle ()
  "Select cutting rectangles."
  (interactive)
  (artist-select-operation "cut rectangle"))

(defun artist-select-op-cut-square ()
  "Select cutting squares."
  (interactive)
  (artist-select-operation "cut square"))

(defun artist-select-op-copy-rectangle ()
  "Select copying rectangles."
  (interactive)
  (artist-select-operation "copy rectangle"))

(defun artist-select-op-copy-square ()
  "Select copying squares."
  (interactive)
  (artist-select-operation "cut square"))

(defun artist-select-op-paste ()
  "Select pasting."
  (interactive)
  (artist-select-operation "paste"))

(defun artist-select-op-flood-fill ()
  "Select flood-filling."
  (interactive)
  (artist-select-operation "flood-fill"))


;; Drawing lines by using mouse
;; Mouse button actions
;;

(defun artist-update-pointer-shape ()
  "Perform the update of the X Windows pointer shape."
  (set-mouse-color nil))

(defvar x-pointer-shape)

(defun artist-set-pointer-shape (new-pointer-shape)
  "Set the shape of the X Windows pointer to NEW-POINTER-SHAPE."
  (setq x-pointer-shape new-pointer-shape)
  (artist-update-pointer-shape))

(defsubst artist-event-is-shifted (ev)
  "Check whether the shift-key is pressed in event EV."
  (memq 'shift (event-modifiers ev)))

(defun artist-do-nothing ()
  "Function that does nothing."
  (interactive))

(defun artist-compute-up-event-key (ev)
  "Compute the corresponding up key sequence for event EV."
 (let* ((basic (event-basic-type ev))
	(unshifted basic)
	(shifted (make-symbol (concat "S-" (symbol-name basic)))))
   (if (artist-event-is-shifted ev)
       (make-vector 1 shifted)
     (make-vector 1 unshifted))))

(defun artist-down-mouse-1 (ev)
  "Perform drawing action for event EV."
  (interactive "@e")
  (let* ((real (artist-go-get-symbol-shift
		artist-curr-go (artist-event-is-shifted ev)))
	 (draw-how (artist-go-get-draw-how-from-symbol real))
	 ;; Remember original values for draw-region-min-y and max-y
	 ;; in case we are interrupting a key-draw operation.
	 (orig-draw-region-min-y artist-draw-region-min-y)
	 (orig-draw-region-max-y artist-draw-region-max-y)
	 (orig-pointer-shape (if (eq window-system 'x) x-pointer-shape nil))
	 (echoq-keystrokes 10000)	; a lot of seconds
	 ;; Remember original binding for the button-up event to this
	 ;; button-down event.
	 (key (artist-compute-up-event-key ev))
	 (orig-button-up-binding (lookup-key (current-global-map) key)))

    (unwind-protect
	(progn
	  (if (eq window-system 'x)
	      (artist-set-pointer-shape artist-pointer-shape))

	  ;; Redefine the button-up binding temporarily (the original
	  ;; binding is restored in the unwind-forms below). This is to
	  ;; avoid the phenomenon outlined in this scenario:
	  ;;
	  ;; 1. A routine which reads something from the mini-buffer (such
	  ;;    as the text renderer) is called from below.
	  ;; 2. Meanwhile, the users releases the mouse button.
	  ;; 3. As a (funny :-) coincidence, the binding for the
	  ;;    button-up event is often mouse-set-point, so Emacs
	  ;;    sets the point to where the button was released, which is
	  ;;    in the buffer where the user wants to place the text.
	  ;; 4. The user types C-x o (or uses the mouse once again)
	  ;;    until he reaches the mini-buffer which is still prompting
	  ;;    for some text to render.
	  ;;
	  ;; To do this foolproof, all local and minor-mode maps should
	  ;; be searched and temporarily changed as well, since they
	  ;; too might have some binding for the button-up event,
	  ;; but I hope dealing with the global map will suffice.
	  (define-key (current-global-map) key 'artist-do-nothing)

	  (artist-draw-region-reset)

	  (artist-mode-line-show-curr-operation t)

	  (cond ((eq draw-how 'artist-do-continously)
		 (artist-mouse-draw-continously ev))
		((eq draw-how 'artist-do-poly)
		 (artist-mouse-draw-poly ev))
		((and (numberp draw-how) (= draw-how 1))
		 (artist-mouse-draw-1point ev))
		((and (numberp draw-how) (= draw-how 2))
		 (artist-mouse-draw-2points ev))
		(t (message "Drawing \"%s\"s is not yet implemented"
			    draw-how)))

	  (if artist-trim-line-endings
	      (artist-draw-region-trim-line-endings artist-draw-region-min-y
						    artist-draw-region-max-y))
	  (setq artist-draw-region-min-y orig-draw-region-min-y)
	  (setq artist-draw-region-max-y orig-draw-region-max-y))

      ; This is protected
      (if (eq window-system 'x)
	  (artist-set-pointer-shape orig-pointer-shape))

      (if orig-button-up-binding
	  (define-key (current-global-map) key orig-button-up-binding))

      (artist-mode-line-show-curr-operation artist-key-is-drawing))))


(defun artist-mouse-choose-operation (ev op)
  "Choose operation for event EV and operation OP."
  (interactive
   (progn
     (select-window (posn-window (event-start last-input-event)))
     (list last-input-event
	   (if (display-popup-menus-p)
	       (x-popup-menu last-nonmenu-event artist-popup-menu-table)
	     'no-popup-menus))))

  (if (eq op 'no-popup-menus)
      ;; No popup menus. Call `tmm-prompt' instead, but with the
      ;; up-mouse-button, if any, temporarily disabled, otherwise
      ;; it'll interfere.
      (let* ((key (artist-compute-up-event-key ev))
	     (orig-button-up-binding (lookup-key (current-global-map) key)))
	(unwind-protect
	    (define-key (current-global-map) key 'artist-do-nothing)
	    (setq op (tmm-prompt artist-popup-menu-table))
	  (if orig-button-up-binding
	      (define-key (current-global-map) key orig-button-up-binding)))))

  (let ((draw-fn (artist-go-get-draw-fn-from-symbol (car op)))
	(set-fn (artist-fc-get-fn-from-symbol (car op))))
    (cond

     ;; *** It was a draw-function
     ((not (listp draw-fn))
      (let* ((unshifted    (artist-go-get-symbol-shift (car op) nil))
	     (shifted      (artist-go-get-symbol-shift (car op) t))
	     (shift-state  (artist-event-is-shifted ev))
	     (selected-op  (if shift-state shifted unshifted))
	     (keyword      (artist-go-get-keyword-from-symbol selected-op)))
	(artist-select-operation keyword)))

     ;; *** It was a set/unset function
     ((not (listp set-fn))
      (call-interactively set-fn)))))


(defun artist-down-mouse-3 (ev)
  "Erase character or rectangle, depending on event EV."
  (interactive "@e")
  (let ((artist-curr-go 'erase-char))
    (artist-down-mouse-1 ev))
  ;; Restore mode-line
  (artist-mode-line-show-curr-operation artist-key-is-drawing))


;;
;; Mouse routines
;;

(defsubst artist-shift-has-changed (shift-state ev)
  "From the last SHIFT-STATE and EV, determine if the shift-state has changed."
  ;; This one simply doesn't work.
  ;;
  ;; There seems to be no way to tell whether the user has pressed shift
  ;; while dragging the cursor around when we are in a track-mouse
  ;; form. Calling (event-modifiers ev) yields nil :-( Neither is the
  ;; (event-basic-type ev) of any help (it is simply `mouse-movement').
  ;;
  ;; So this doesn't work:
  ;; (cond ((and shift-state (not (artist-event-is-shifted ev))) t)
  ;;       ((and (not shift-state) (artist-event-is-shifted ev)) t)
  ;;       (t nil))
  nil)

(defun artist-coord-win-to-buf (coord)
  "Convert a window-relative coordinate COORD to a buffer-relative coordinate."
  (let ((window-x       (car coord))
	(window-y       (cdr coord))
	(window-start-x (window-hscroll))
	(window-start-y (save-excursion (goto-char (window-start))
					(artist-current-line))))
      (cons (+ window-x window-start-x)
	    (+ window-y window-start-y))))


(defun artist-mouse-draw-continously (ev)
  "Generic function for shapes that require 1 point as input.
Operation is done continuously while the mouse button is hold down.
The event, EV, is the mouse event."
  (let* ((unshifted    (artist-go-get-symbol-shift artist-curr-go nil))
	 (shifted      (artist-go-get-symbol-shift artist-curr-go t))
	 (shift-state  (artist-event-is-shifted ev))
	 (op           (if shift-state shifted unshifted))
	 (draw-how     (artist-go-get-draw-how-from-symbol op))
	 (init-fn      (artist-go-get-init-fn-from-symbol op))
	 (prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol op))
	 (exit-fn      (artist-go-get-exit-fn-from-symbol op))
	 (draw-fn      (artist-go-get-draw-fn-from-symbol op))
	 (interval-fn  (artist-go-get-interval-fn-from-symbol op))
	 (interval     (artist-funcall interval-fn))
	 (arrow-pred   (artist-go-get-arrow-pred-from-symbol op))
	 (arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol op))
	 (ev-start     (event-start ev))
	 (initial-win  (posn-window ev-start))
	 (ev-start-pos (artist-coord-win-to-buf (posn-col-row ev-start)))
	 (x1           (car ev-start-pos))
	 (y1           (cdr ev-start-pos))
	 (shape)
	 (timer))
    (select-window (posn-window ev-start))
    (artist-funcall init-fn x1 y1)
    (if (not artist-rubber-banding)
	(artist-no-rb-set-point1 x1 y1))
    (track-mouse
      (while (or (mouse-movement-p ev)
		 (member 'down (event-modifiers ev)))
	(setq ev-start-pos (artist-coord-win-to-buf
			    (posn-col-row (event-start ev))))
	(setq x1 (car ev-start-pos))
	(setq y1 (cdr ev-start-pos))

	;; Cancel previous timer
	(if timer
	    (cancel-timer timer))

	(if (not (eq initial-win (posn-window (event-start ev))))
	    ;; If we moved outside the window, do nothing
	    nil

	  ;; Still in same window:
	  ;;
	  ;; Check if user presses or releases shift key
	  (if (artist-shift-has-changed shift-state ev)

	      ;; First check that the draw-how is the same as we
	      ;; already have. Otherwise, ignore the changed shift-state.
	      (if (not (eq draw-how
			   (artist-go-get-draw-how-from-symbol
			    (if (not shift-state) shifted unshifted))))
		  (message "Cannot switch to shifted operation")

		;; progn is "implicit" since this is the else-part
		(setq shift-state (not shift-state))
		(setq op          (if shift-state shifted unshifted))
		(setq draw-how    (artist-go-get-draw-how-from-symbol op))
		(setq draw-fn     (artist-go-get-draw-fn-from-symbol op))))

	  ;; Draw the new shape
	  (setq shape (artist-funcall draw-fn x1 y1))
	  (artist-move-to-xy x1 y1)

	  ;; Start the timer to call `draw-fn' repeatedly every
	  ;; `interval' second
	  (if (and interval draw-fn)
	      (setq timer (run-at-time interval interval draw-fn x1 y1))))

	;; Read next event
	(setq ev (read-event))))

    ;; Cancel any timers
    (if timer
	(cancel-timer timer))

    (artist-funcall prep-fill-fn x1 y1)

    (if (artist-funcall arrow-pred)
	(artist-funcall arrow-set-fn x1 y1)
      (artist-clear-arrow-points))

    (artist-funcall exit-fn x1 y1)
    (artist-move-to-xy x1 y1)))



(defun artist-mouse-draw-poly (ev)
  "Generic function for shapes requiring several points as input.
The event, EV, is the mouse event."
  (interactive "@e")
  (message "Mouse-1: set new point, mouse-2: set last point")
  (let* ((unshifted    (artist-go-get-symbol-shift artist-curr-go nil))
	 (shifted      (artist-go-get-symbol-shift artist-curr-go t))
	 (shift-state  (artist-event-is-shifted ev))
	 (op           (if shift-state shifted unshifted))
	 (draw-how     (artist-go-get-draw-how-from-symbol op))
	 (init-fn      (artist-go-get-init-fn-from-symbol op))
	 (prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol op))
	 (exit-fn      (artist-go-get-exit-fn-from-symbol op))
	 (draw-fn      (artist-go-get-draw-fn-from-symbol op))
	 (undraw-fn    (artist-go-get-undraw-fn-from-symbol op))
	 (fill-pred    (artist-go-get-fill-pred-from-symbol op))
	 (fill-fn      (artist-go-get-fill-fn-from-symbol op))
	 (arrow-pred   (artist-go-get-arrow-pred-from-symbol op))
	 (arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol op))
	 (ev-start     (event-start ev))
	 (initial-win  (posn-window ev-start))
	 (ev-start-pos (artist-coord-win-to-buf (posn-col-row ev-start)))
	 (x1-last      (car ev-start-pos))
	 (y1-last      (cdr ev-start-pos))
	 (x2           x1-last)
	 (y2           y1-last)
	 (is-down      t)
	 (shape        nil)
	 (point-list   nil)
	 (done         nil))
    (select-window (posn-window ev-start))
    (artist-funcall init-fn x1-last y1-last)
    (if (not artist-rubber-banding)
	(artist-no-rb-set-point1 x1-last y1-last))
    (track-mouse
      (while (not done)
	;; decide what to do
	(cond

	 ;; *** Mouse button is released.
	 ((and is-down
	       (or (member 'click (event-modifiers ev))
		   (member 'drag (event-modifiers ev))))
	  ;; First, if not rubber-banding, draw the line.
	  ;;
	  (if (not artist-rubber-banding)
	      (progn
		(artist-no-rb-unset-points)
		(setq shape (artist-funcall draw-fn x1-last y1-last x2 y2))))

	  ;; Set the second point to the shape's second point
	  ;; (which might be different from the mouse's second point,
	  ;; if, for example, we are drawing a straight line)
	  ;;
	  (if (not (null shape))
	      (let ((endpoint2 (artist-2point-get-endpoint2 shape)))
		(setq x1-last (artist-endpoint-get-x endpoint2))
		(setq y1-last (artist-endpoint-get-y endpoint2))))
	  (setq point-list (cons (artist-make-endpoint x1-last y1-last)
				 point-list))
	  (setq shape nil)
	  (setq is-down nil))

	 ;; *** Mouse button 2 or 3 down
	 ((and (member 'down (event-modifiers ev))
	       (or (equal (event-basic-type ev) 'mouse-2)
		   (equal (event-basic-type ev) 'mouse-3)))
	  ;; Ignore
	  nil)

	 ;; *** Mouse button 2 or 3 released
	 ((and (or (member 'click (event-modifiers ev))
		   (member 'drag (event-modifiers ev)))
	       (or (equal (event-basic-type ev) 'mouse-2)
		   (equal (event-basic-type ev) 'mouse-3)))

	  ;; This means the end of our poly-line drawing-session.
	  ;;
	  (setq done t))

	 ;; *** Mouse button 1 went down
	 ((and (not is-down)
	       (member 'down (event-modifiers ev))
	       (equal (event-basic-type ev) 'mouse-1))
	  ;; Check whether the (possibly new, that depends on if shift
	  ;; has been pressed or released) symbol has the same draw-how
	  ;; information as the previous had. If it hasn't, we can't
	  ;; proceed.
	  ;;
	  (if (not (eq draw-how
		       (artist-go-get-draw-how-from-symbol
			(if (not shift-state) shifted unshifted))))
	      (message "Cannot switch operation")
	    (progn
	      ;; Decide operation
	      ;;
	      (setq unshifted
		    (artist-go-get-symbol-shift artist-curr-go nil)
		    shifted
		    (artist-go-get-symbol-shift artist-curr-go t)
		    shift-state  (artist-event-is-shifted ev)
		    op           (if shift-state shifted unshifted)
		    draw-how     (artist-go-get-draw-how-from-symbol op)
		    draw-fn      (artist-go-get-draw-fn-from-symbol op)
		    undraw-fn    (artist-go-get-undraw-fn-from-symbol op)
		    fill-pred    (artist-go-get-fill-pred-from-symbol op)
		    fill-fn      (artist-go-get-fill-fn-from-symbol op))

	      ;; Draw shape from last place to this place

	      ;; set x2 and y2
	      ;;
	      (setq ev-start-pos (artist-coord-win-to-buf
				  (posn-col-row (event-start ev))))
	      (setq x2 (car ev-start-pos))
	      (setq y2 (cdr ev-start-pos))

	      ;; Draw the new shape (if not rubber-banding, place both marks)
	      ;;
	      (if artist-rubber-banding
		  (setq shape (artist-funcall draw-fn x1-last y1-last x2 y2))
		(progn
		  (artist-no-rb-set-point1 x1-last y1-last)
		  (artist-no-rb-set-point2 x2 y2)))

	      ;; Show new operation in mode-line
	      (let ((artist-curr-go op))
		(artist-mode-line-show-curr-operation t))))

	  (setq is-down t))


	 ;; *** Mouse moved, button is down and we are still in orig window
	 ((and (mouse-movement-p ev)
	       is-down
	       (eq initial-win (posn-window (event-start ev))))
	  ;; Draw shape from last place to this place
	  ;;
	  ;; set x2 and y2
	  (setq ev-start-pos (artist-coord-win-to-buf
			      (posn-col-row (event-start ev))))
	  (setq x2 (car ev-start-pos))
	  (setq y2 (cdr ev-start-pos))

	  ;; First undraw last shape
	  ;; (unset last point if not rubberbanding)
	  ;;
	  (artist-funcall undraw-fn shape)

	  ;; Draw the new shape (if not rubberbanding, set 2nd mark)
	  ;;
	  (if artist-rubber-banding
	      (setq shape (artist-funcall draw-fn x1-last y1-last x2 y2))
	    (progn
	      (artist-no-rb-unset-point2)
	      (artist-no-rb-set-point2 x2 y2)))
	  ;; Move cursor
	  (artist-move-to-xy x2 y2))

	 ;; *** Mouse moved, button is down but we are NOT in orig window
	 ((and (mouse-movement-p ev)
	       is-down
	       (not (eq initial-win (posn-window (event-start ev)))))
	  ;; Ignore
	  nil)


	 ;; *** Moving mouse while mouse button is not down
	 ((and (mouse-movement-p ev) (not is-down))
	  ;; don't do anything.
	  nil)


	 ;; *** Mouse button 1 went down, first time
	 ((and is-down
	       (member 'down (event-modifiers ev))
	       (equal (event-basic-type ev) 'mouse-1))
	  ;; don't do anything
	  nil)


	 ;; *** Another event
	 (t
	  ;; End drawing
	  ;;
	  (setq done t)))

	;; Read next event (only if we should not stop)
	(if (not done)
	    (setq ev (read-event)))))

    ;; Reverse point-list (last points are cond'ed first)
    (setq point-list (reverse point-list))

    (artist-funcall prep-fill-fn point-list)

    ;; Maybe fill
    (if (artist-funcall fill-pred)
	(artist-funcall fill-fn point-list))

    ;; Maybe set arrow points
    (if (and point-list (artist-funcall arrow-pred))
	(artist-funcall arrow-set-fn point-list)
      (artist-clear-arrow-points))

    (artist-funcall exit-fn point-list)
    (artist-move-to-xy x2 y2)))


(defun artist-mouse-draw-1point (ev)
  "Generic function for shapes requiring only 1 point as input.
Operation is done once.  The event, EV, is the mouse event."
  (interactive "@e")
  (let* ((unshifted    (artist-go-get-symbol-shift artist-curr-go nil))
	 (shifted      (artist-go-get-symbol-shift artist-curr-go t))
	 (shift-state  (artist-event-is-shifted ev))
	 (op           (if shift-state shifted unshifted))
	 (draw-how     (artist-go-get-draw-how-from-symbol op))
	 (init-fn      (artist-go-get-init-fn-from-symbol op))
	 (prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol op))
	 (exit-fn      (artist-go-get-exit-fn-from-symbol op))
	 (draw-fn      (artist-go-get-draw-fn-from-symbol op))
	 (arrow-pred   (artist-go-get-arrow-pred-from-symbol op))
	 (arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol op))
	 (ev-start     (event-start ev))
	 (ev-start-pos (artist-coord-win-to-buf (posn-col-row ev-start)))
	 (x1           (car ev-start-pos))
	 (y1           (cdr  ev-start-pos)))
    (select-window (posn-window ev-start))
    (artist-funcall init-fn x1 y1)
    (artist-funcall draw-fn x1 y1)
    (artist-funcall prep-fill-fn x1 y1)
    (if (artist-funcall arrow-pred)
	(artist-funcall arrow-set-fn x1 y1)
      (artist-clear-arrow-points))
    (artist-funcall exit-fn x1 y1)
    (artist-move-to-xy x1 y1)))


(defun artist-mouse-draw-2points (ev)
  "Generic function for shapes requiring 2 points as input.
The event, EV, is the mouse event."
  (interactive "@e")
  (let* ((unshifted    (artist-go-get-symbol-shift artist-curr-go nil))
	 (shifted      (artist-go-get-symbol-shift artist-curr-go t))
	 (shift-state  (artist-event-is-shifted ev))
	 (op           (if shift-state shifted unshifted))
	 (draw-how     (artist-go-get-draw-how-from-symbol op))
	 (init-fn      (artist-go-get-init-fn-from-symbol op))
	 (prep-fill-fn (artist-go-get-prep-fill-fn-from-symbol op))
	 (exit-fn      (artist-go-get-exit-fn-from-symbol op))
	 (draw-fn      (artist-go-get-draw-fn-from-symbol op))
	 (undraw-fn    (artist-go-get-undraw-fn-from-symbol op))
	 (fill-pred    (artist-go-get-fill-pred-from-symbol op))
	 (fill-fn      (artist-go-get-fill-fn-from-symbol op))
	 (arrow-pred   (artist-go-get-arrow-pred-from-symbol op))
	 (arrow-set-fn (artist-go-get-arrow-set-fn-from-symbol op))
	 (ev-start     (event-start ev))
	 (initial-win  (posn-window ev-start))
	 (ev-start-pos (artist-coord-win-to-buf (posn-col-row ev-start)))
	 (x1           (car ev-start-pos))
	 (y1           (cdr ev-start-pos))
	 (x2)
	 (y2)
	 (shape))
    (select-window (posn-window ev-start))
    (artist-funcall init-fn x1 y1)
    (if (not artist-rubber-banding)
	(artist-no-rb-set-point1 x1 y1))
    (track-mouse
      (while (or (mouse-movement-p ev)
		 (member 'down (event-modifiers ev)))
	(setq ev-start-pos (artist-coord-win-to-buf
			    (posn-col-row (event-start ev))))
	(setq x2 (car ev-start-pos))
	(setq y2 (cdr ev-start-pos))

	(if (not (eq initial-win (posn-window (event-start ev))))
	    ;; If we moved outside the window, do nothing
	    nil

	  ;; Still in same window:
	  ;;
	  ;; First undraw last shape (unset last point if not rubberbanding)
	  (if artist-rubber-banding
	      (artist-funcall undraw-fn shape)
	    (artist-no-rb-unset-point2))

	  ;; Check if user presses or releases shift key
	  (if (artist-shift-has-changed shift-state ev)

	      ;; First check that the draw-how is the same as we
	      ;; already have. Otherwise, ignore the changed shift-state.
	      (if (not (eq draw-how
			   (artist-go-get-draw-how-from-symbol
			    (if (not shift-state) shifted unshifted))))
		  (message "Cannot switch to shifted operation")

		(message "Switching")
		;; progn is "implicit" since this is the else-part
		(setq shift-state (not shift-state))
		(setq op          (if shift-state shifted unshifted))
		(setq draw-how    (artist-go-get-draw-how-from-symbol op))
		(setq draw-fn     (artist-go-get-draw-fn-from-symbol op))
		(setq undraw-fn   (artist-go-get-undraw-fn-from-symbol op))
		(setq fill-pred   (artist-go-get-fill-pred-from-symbol op))
		(setq fill-fn     (artist-go-get-fill-fn-from-symbol op))))

	  ;; Draw the new shape
	  (if artist-rubber-banding
	      (setq shape (artist-funcall draw-fn x1 y1 x2 y2))
	    (artist-no-rb-set-point2 x2 y2))
	  ;; Move cursor
	  (artist-move-to-xy x2 y2))


	;; Read next event
	(setq ev (read-event))))

    ;; If we are not rubber-banding (that is, we were moving around the `2')
    ;; draw the shape
    (if (not artist-rubber-banding)
	(progn
	  (artist-no-rb-unset-points)
	  (setq shape (artist-funcall draw-fn x1 y1 x2 y2))))

    (artist-funcall prep-fill-fn shape x1 y1 x2 y2)

    ;; Maybe fill
    (if (artist-funcall fill-pred)
	(artist-funcall fill-fn shape x1 y1 x2 y2))

    ;; Maybe set arrow-points
    (if (artist-funcall arrow-pred)
	(artist-funcall arrow-set-fn shape x1 y1 x2 y2)
      (artist-clear-arrow-points))

    (artist-funcall exit-fn shape x1 y1 x2 y2)
    (artist-move-to-xy x2 y2)))


;;
;; Bug-report-submitting
;;
(defun artist-submit-bug-report ()
  "Submit via mail a bug report on Artist."
  (interactive)
  (require 'reporter)
  (if (y-or-n-p "Do you want to submit a bug report on Artist? ")
      (let ((to   artist-maintainer-address)
	    (vars '(window-system
		    window-system-version
		    ;;
		    artist-rubber-banding
		    artist-interface-with-rect
		    artist-aspect-ratio
		    ;; Now the internal ones
		    artist-curr-go
		    artist-key-poly-point-list
		    artist-key-shape
		    artist-key-draw-how
		    artist-arrow-point-1
		    artist-arrow-point-2)))
	;; Remove those variables from vars that are not bound
	(mapc
	 (function
	  (lambda (x)
	    (if (not (and (boundp x) (symbol-value x)))
		(setq vars (delq x vars))))) vars)
	(reporter-submit-bug-report
	 artist-maintainer-address
	 (concat "artist.el " artist-version)
	 vars
	 nil nil
	 (concat "Hello Tomas,\n\n"
		 "I have a nice bug report on Artist for you! Here it is:")))))


;;
;; Now provide this minor mode
;;

(provide 'artist)


;;; About adding drawing modes
;;; --------------------------

;; If you are going to add a new drawing mode, read the following
;; sketchy outlines to get started a bit easier.
;;
;; 1.   If your new drawing mode falls into one of the following
;;      categories, goto point 2, otherwise goto point 3.
;;
;;         - Modes where the shapes are drawn continuously, as long as
;;           the mouse button is held down (continuous modes).
;;           Example: the erase-char mode, the pen and pen-line modes.
;;
;;         - Modes where the shape is made up of from 2 points to an
;;           arbitrary number of points (poly-point modes).
;;           Example: the poly-line mode
;;
;;         - Modes where the shape is made up of 2 points (2-point
;;           modes).
;;           Example: lines, rectangles
;;
;;         - Modes where the shape is made up of 1 point (1-point
;;           modes). This mode differs from the continuous modes in
;;           that the shape is drawn only once when the mouse button
;;           is pressed.
;;           Examples: paste, a flood-fill, vaporize modes
;;
;;
;; 2. To make it easier and more flexible to program new drawing
;;    modes, you might choose to specify
;;      init-fn:      a function to be called at the very beginning
;;                    of the drawing phase,
;;      prep-fill-fn: a function to be called before filling,
;;      arrow-set-fn: a function for setting arrows, to be called
;;                    after filling, and
;;      exit-fn:      a function to be called at the very end of
;;                    the drawing phase.
;;    For each of the cases below, the arguments given to the init-fn,
;;    prep-fill-fn, arrow-set-fn and exit-fn are stated.
;;
;;    If your mode matches the continuous mode or the 1-point mode:
;;
;;      a. Create a draw-function that draws your shape. Your function
;;         must take x and y as arguments. The return value is not
;;         used.
;;
;;      b. Add your mode to the master table, `artist-mt'.
;;
;;	init-fn:      x y
;;	prep-fill-fn: x y
;;	arrow-set-fn: x y
;;	exit-fn:      x y
;;
;;    If your mode matches the 2-point mode:
;;
;;      a. Create one draw-function that draws your shape and one
;;         undraw-function that undraws it.
;;
;;         The draw-function must take x1, y1, x2 and y2 as
;;         arguments. It must return a list with three elements:
;;           Endpoint1: a vector [x1 y1]
;;           Endpoint2: a vector [x2 y2]
;;           Shapeinfo: all info necessary for your undraw-function to
;;                      be able to undraw the shape
;;         Use the artist-endpoint-* accessors to create and inspect
;;         the endpoints.
;;
;;         If applicable, you must be able to draw your shape without
;;         borders if the `artist-borderless-shapes' is non-nil.
;;         See `artist-draw-rect' for an example.
;;
;;         The undraw-function must take one argument: the list created
;;         by your draw-function. The return value is not used.
;;
;;      b. If you want to provide a fill-function, then create a
;;         function that takes 5 arguments: the list created by your
;;         draw-function, x1, y1, x2 and y2. The return value is not
;;         used.
;;
;;      c. Add your mode to the master table, `artist-mt'.
;;
;;	init-fn:      x1 y1
;;	prep-fill-fn: shape x1 y1 x2 y2
;;	arrow-set-fn: shape x1 y1 x2 y2
;;	exit-fn:      shape x1 y1 x2 y2
;;
;;    If your mode matches the poly-point mode:
;;
;;      a. Create one draw-function that draws your shape and one
;;         undraw-function that undraws it. The draw- and
;;         undraw-functions are used to draw/undraw a segment of
;;         your poly-point mode between 2 points. The draw- and
;;         undraw-functions are then really 2-point mode functions.
;;         They must take the same arguments and return the same
;;         values as those of the 2-point mode.
;;
;;         If applicable, you must be able to draw your shape without
;;         borders if the `artist-borderless-shapes' is non-nil.
;;         See `artist-draw-rect' for an example.
;;
;;      b. If you want to provide a fill-function, then create a
;;         function that takes 1 argument: a list of points where each
;;         point is a vector, [x, y].
;;
;;      c. Add your mode to the master table, `artist-mt'.
;;
;;	init-fn:      x1 y1
;;	prep-fill-fn: point-list
;;	arrow-set-fn: point-list
;;	exit-fn:      point-list
;;
;;    The arrow-set-fn must set the variables `artist-arrow-point-1'
;;    and `artist-arrow-point-2'. If your mode does not take arrows,
;;    you must set the variables to nil. Use the accessors
;;    artist-arrow-point-* to create and inspect arrow-points.
;;
;;
;; 3. If your mode doesn't match any of the categories, you are facing
;;    a bit more work, and I cannot be as detailed as above. Here is a
;;    brief outline of what you have to do:
;;
;;      a. Decide on a name for your type of mode. Let's assume that
;;         you decided on `xxx'. Then you should use the draw-how
;;         symbol artist-do-xxx.
;;
;;      b. Create a function artist-mouse-draw-xxx for drawing with
;;         mouse. It should be called from `artist-down-mouse-1'.
;;
;;         The all coordinates must be converted from window-relative
;;         to buffer relative before saved or handed over to
;;         any other function. Converting is done with
;;         the function `artist-coord-win-to-buf'.
;;
;;         It must take care to the `artist-rubber-banding' variable
;;         and perform rubber-banding accordingly. Use the
;;         artist-no-rb-* functions if not rubber-banding.
;;
;;         If applicable, you must be able to draw your shape without
;;         borders if the `artist-borderless-shapes' is non-nil.
;;         See `artist-draw-rect' for an example.
;;
;;         You must call the init-fn, the prep-fill-fn, arrow-set-fn
;;         and the exit-fn at the appropriate points.
;;
;;         When artist-mouse-draw-xxx ends, the shape for your mode
;;         must be completely drawn.
;;
;;      c. Create functions for drawing with keys:
;;
;;         - artist-key-set-point-xxx for setting a point in the
;;           mode, to be called from `artist-key-set-point-common'.
;;
;;         - artist-key-do-continuously-xxx to be called from
;;           `artist-key-do-continuously-common' whenever the user
;;           moves around.
;;
;;         As for the artist-mouse-draw-xxx, these two functions must
;;         take care to do rubber-banding, borderless shapes and to
;;         set arrows.
;;
;;         These functions should set the variable `artist-key-shape'
;;         to the shape drawn.
;;
;;      d. Create artist-key-draw-xxx and artist-key-undraw-xxx for
;;         drawing and undrawing. These are needed when the user
;;         switches operation to draw another shape of the same type
;;         of drawing mode.
;;
;;         You should provide these functions. You might think that
;;         only you is using your type of mode, so no one will be able
;;         to switch to another operation of the same type of mode,
;;         but someone else might base a new drawing mode upon your
;;         work.
;;
;;         You must call the init-fn, the prep-fill-fn, arrow-set-fn
;;         and the exit-fn at the appropriate points.
;;
;;      e. Add your new mode to the master table, `artist-mt'.
;;
;;
;; Happy hacking! Please let me hear if you add any drawing modes!
;; Don't hesitate to ask me any questions.


;;; artist.el ends here
