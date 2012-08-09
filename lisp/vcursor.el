;;; vcursor.el --- manipulate an alternative ("virtual") cursor

;; Copyright (C) 1994, 1996, 1998, 2001-2012 Free Software Foundation, Inc.

;; Author:   Peter Stephenson <pws@ibmth.df.unipi.it>
;; Maintainer: FSF
;; Keywords: virtual cursor, convenience

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

;; Latest changes
;; ==============
;;
;; - *IMPORTANT* vcursor-key-bindings is now nil by default, to avoid
;;   side-effects when the package is loaded.  This means no keys are
;;   bound by default.  Use customize to change it to t to restore
;;   the old behavior.  (If you do it by hand in .emacs, it
;;   must come before vcursor is loaded.)
;; - You can alter the main variables and the vcursor face via
;;   M-x customize: go to the Editing group and find Vcursor.
;; - vcursor-auto-disable can now be 'copy (actually any value not nil
;;   or t), which means that copying from the vcursor will be turned
;;   off after any operation not involving the vcursor, but the
;;   vcursor itself will be left alone.
;; - works on dumb terminals with Emacs 19.29 and later
;; - new keymap vcursor-map for binding to a prefix key
;; - vcursor-compare-windows substantially improved
;; - vcursor-execute-{key,command} much better about using the
;;   right keymaps and arranging for the correct windows to be used
;; - vcursor-window-funcall can call functions interactively
;; - vcursor-interpret-input for special effects
;;
;; Introduction
;; ============
;;
;; Virtual cursor commands.  I got this idea from the old BBC micro.
;; You need Emacs 19 or 20 and a window system for the best effects.
;; For character terminals, at least Emacs 19.29 is required
;; (special behavior for the overlay property
;; "before-string" must be implemented).  Search for "dumb terminals"
;; for more information.
;;
;; This is much easier to use than the instructions are to read.
;; First, you need to let vcursor define some keys: setting
;; vcursor-key-bindings to t before loading, or by customize, will
;; define various keys with the prefix C-S.  You'll have to read
;; further if you don't want this.  Then I suggest you simply load it
;; and play around with holding down Ctrl and Shift and pressing up,
;; down, left, right, tab, return, and see what happens.  (Find a
;; scratch buffer before using C-S-tab: that toggles copying.)
;;
;; Most of the functions described in this documentation are in
;; parentheses so that if you have the package loaded you can type C-h
;; f on top of them for help.
;;
;; Using the cursor keys with both control and shift held down moves
;; around a virtual cursor, which is initially at point.  When active,
;; it appears with an underline through it to distinguish it from the
;; normal cursor.  You can then use one of the other commands to copy
;; characters from the location of the virtual cursor to point.  This
;; is very useful, for example, when copying some previous text while
;; making changes to it at the same time, since you never have to move
;; the "real" cursor away from where you are inserting.
;;
;; The remaining default key bindings are based around the PC-type
;; cluster found above the cursor keys on a lot of keyboards, the
;; function keys which my limited knowledge of X terminals expects to
;; find at the top.  Some functions are duplicated in more obvious
;; places for the X version.
;;
;; All the keybindings require you to hold down control and shift at
;; once.  I assumed this combination wouldn't be heavily bound by most
;; people and that it would be easy to type with the left hand.
;; Inevitably it will clash with some other packages, but I can't help
;; that: an intuitive binding is a prerequisite here.  See below for
;; other alternatives (search for "Oemacs").  There is also a keymap
;; which you can bind to a prefix key, which may give some more
;; intuitive alternatives in some cases, see `The vcursor keymap' below.
;;
;; Holding down control and shift and pressing insert (vcursor-copy)
;; copies one character from wherever the virtual cursor is to point;
;; point and the virtual cursor advance in the separate and equal
;; station to which... (etc.).  M-C-S-return (vcursor-copy-line)
;; copies to the end of the line instead of just one character,
;; C-S-delete or C-S-remove (vcursor-copy-word) copies a word.
;;
;; A more general way of copying is to use C-S-tab, which is a toggle.
;; In the "on" state, moving the virtual cursor will copy the
;; moved-over text to the normal cursor position (including when going
;; backwards, though each piece of text moved over is copied forwards:
;; compare the behavior of C-S-up and C-S-left).
;;
;; However, that's just a small part of the magic.  If the virtual
;; cursor goes off the display, it will be redisplayed in some other
;; window.  (See the function (vcursor-find-window) for details of how
;; this window is chosen.)  This gives you fingertip control over two
;; windows at once.
;;
;; C-S-return (vcursor-disable) disables the virtual cursor, removing
;; it so that it starts from point whenever you move it again --- note
;; that simply moving the cursor and virtual cursor on top of one
;; another does not have this effect.
;;
;; If you give C-S-return a positive prefix arg, it will also delete the
;; window (unless it's the current one).  Whenever the virtual cursor
;; goes off-screen in its own window, point in that window is moved as
;; well to restore it to view.  (It's easier that way, that's why.
;; However, point doesn't move unless the view in the window does, so
;; it's not tied to the virtual cursor location.)
;;
;; You can also use C-S-return with a negative prefix argument which
;; forces the vcursor to appear at point.  This is particularly useful if
;; you actually want to edit in another window but would like to
;; remember the current cursor location for examining or copying from
;; that buffer.  (I just hit C-S-right C-S-left, but I'm a hopeless
;; lowbrow.)
;;
;; There is also C-S-f6 (vcursor-other-window) which behaves like
;; C-x o on the virtual rather than the real cursor, except that it
;; will create another window if necessary.
;;
;; The keys C-S-prior (vcursor-scroll-down) and C-S-next
;; (vcursor-scroll-up) (i.e., PageUp and PageDown) will scroll the
;; virtual cursor window, appropriately chosen.  They will always
;; create a new window or take over an old one if necessary.
;; Likewise, M-C-S-left and M-C-S-right move you to the
;; beginning or end of a line, C-S-home and C-S-end the
;; beginning or end of a buffer (these are also on M-C-S-up and
;; M-C-S-down for those of us stuck with DEC keyboards).
;;
;; C-S-f7 (vcursor-goto) will take you to the vcursor position
;; (swapping windows if it seems sensible) and (unless you give it a
;; prefix argument) delete the virtual cursor, so this is useful for
;; you to take over editing at the virtual cursor position.  It is not
;; an error if the virtual cursor is not active; it simply leaves you
;; at point, because that is where the virtual cursor would start
;; from.
;;
;; In a similar vein, M-C-S-tab (hope your left hand's flexible;
;; C-S-select on DEC keyboards) (vcursor-swap-point) will take you to
;; the virtual cursor position but simultaneously put the virtual
;; cursor at the old cursor position.  It is also supposed to ensure
;; that both are visible.
;;
;; C-S-f8 (C-S-find on DEC keyboards) (vcursor-isearch-forward)
;; allows you to do an isearch in another window.  It works a bit like
;; vcursor-scroll-*; it moves into another window, calls isearch
;; there, and sets the virtual cursor position to the point found.  In
;; other words, it works just like isearch but with the virtual cursor
;; instead of the real one (that's why it's called a "virtual
;; cursor").  While you are isearching, you are editing in the virtual
;; cursor window, but when you have finished you return to where you
;; started.  Note that once you are in isearch all the keys are normal
;; --- use C-s, not C-S-f8, to search for the next occurrence.
;;
;; If you set the variable vcursor-auto-disable, then any command
;; which does not involve moving or copying from the virtual cursor
;; causes the virtual cursor to be disabled.  If you set it to non-nil
;; but not t, then the vcursor itself will remain active, but copying
;; will be turned off, so that the next time the vcursor is moved no
;; text is copied over.  Experience shows that this setting is
;; particularly useful.  If you don't intend to use this, you can
;; comment out the `add-hook' line at the bottom of this file.  (This
;; feature partially emulates the way the "copy" key on the BBC micro
;; worked; actually, the copy cursor was homed when you hit return.
;; This was in keeping with the line-by-line way of entering BASIC,
;; but is less appropriate here.)
;;
;; vcursor-compare-windows is now a reliable adaption of
;; compare-windows, which compares between point in the current buffer
;; and the vcursor location in the other one.  It is an error if
;; vcursor is not set, however it will be brought up in another window
;; if it is not currently visible.  The prefix argument acts just like
;; compare-windows, ignoring whitespace if set.  (In versions before
;; 1.6, this simply called compare-windows, which was much less likely
;; to pick the two windows you wanted.)
;;
;; There is a way of moving the virtual cursor using ordinary
;; commands: C-S-f9 (vcursor-execute-key) reads a key string,
;; moves to the virtual cursor position, executes the command bound to
;; the string, then returns to the original point.  Thus C-S-f9 M-m
;; moves the virtual cursor back to the first non-whitespace character
;; on its line.  As the command is called interactively all the usual
;; ways of passing information to the command called, such as by a
;; prefix argument, are available.  This has many uses not necessarily
;; related to moving the vcursor itself; it can do essentially
;; everything that the \C-x 4 series of commands can do and a lot
;; more.  Note, however, that a new window is not used if the vcursor
;; is visible in the current one:  this can lead to some strange effects,
;; but it is preferable to making a new window every time the vcursor
;; is moved in this may.
;;
;; C-S-f10 (C-S-x) (vcursor-execute-command) behaves the same way but
;; you enter the name of the command.  To do anything really
;; complicated, you are better off using M-C-S-tab
;; (vcursor-swap-point), doing whatever it is, then calling M-C-S-tab
;; again.
;;
;; If you want to add your own moving or copying functions you should
;; be able to do this fairly easily with (vcursor-relative-move) and
;; (vcursor-copy) together with (vcursor-get-char-count).  If you want to
;; do something in a different window, use (vcursor-window-funcall).
;;
;; Key bindings
;; ============
;;
;; There is an alternative set of key bindings which will be used
;; automatically for a PC if Oemacs is detected.  This set uses separate
;; control, shift and meta keys with function keys 1 to 10.  In
;; particular, movement keys are concentrated on f5 to f8 with (in
;; increasing order of distance traveled) C-, M- and S- as prefixes.
;; See the actual bindings below (search for C-f1).  This is because the
;; C-S- prefix is represented by weird key sequences and the set is
;; incomplete; if you don't mind that, some hints are given in comments
;; below.
;;
;; You can specify the usual or the Oemacs bindings by setting the
;; variable vcursor-key-bindings to `xterm' or `oemacs'.  You can also set
;; it to nil, in which case vcursor will not make any key bindings
;; and you can define your own.  The default is t, which makes vcursor
;; guess (it will use xterm unless it thinks Oemacs is running).  The
;; oemacs set will work on an X terminal with function keys, but the
;; xterm set will not work under Oemacs.
;;
;; Usage on dumb terminals
;; =======================
;;
;; If Emacs has set the variable window-system to nil, vcursor will
;; assume that overlays cannot be displayed in a different face,
;; and will instead use a string (the variable vcursor-string, by
;; default "**>") to show its position.  This was first implemented
;; in Emacs 19.29.  Unlike the old-fashioned overlay arrow (as used
;; by debuggers), this appears between existing text, which can
;; make it hard to read if you're not used to it.  (This seemed the
;; better option here.)  This means moving the vcursor up and down is
;; a very efficient way of locating it!
;;
;; Everything else should function as expected, but there is no way to
;; get an easy key binding for the vcursor keys on a generic terminal.
;; Consequently a special keymap is defined for you to use traditional
;; methods: the keymap, however, is available on any terminal type.
;;
;; The vcursor keymap
;; ==================
;;
;; In addition to any other bindings, vcursor-map contains key definitions
;; for handling the vcursor.  You should assign this to a prefix key
;; in the usual way, e.g.
;;      (global-set-key [f14] vcursor-map)
;; and also as usual \C-h in this map will list the key definitions, which
;; are designed to be easy to remember.
;;
;; A special feature is provided by (vcursor-use-vcursor-map), bound
;; to t in that keymap.  With this in effect, the main keymap
;; is overridden by the vcursor map, so keys like \C-p and so on
;; move the vcursor instead.  Remember how to turn it off (type t),
;; or you are in serious trouble!  Note that the cursor keys are not
;; bound by default in this keymap and will continue to move the
;; ordinary cursor.
;;
;; Interpreted input
;; =================
;;
;; Just occasionally, you may want to pretend the strings copied from
;; the vcursor position are to be interpreted as if you had typed them
;; from the keyboard.  Normally, they will just insert themselves anyway,
;; but in some modes (Info and calc for example) typing ordinary characters
;; does something else.  To get this effect, set
;; vcursor-interpret-input to t.  This is normally not a good idea as
;; interpreting input is very much slower than copying text.
;;
;; Un-features
;; ===========
;;
;;  - The vcursor will not move to point-max, since otherwise it would
;;    disappear.  However, no error is flagged as point-max is a valid
;;    point in the buffer.  Thus cursor right or down at the second
;;    last point in the file does not flag an error, which is inconsistent,
;;    and if copying is on the last character (typically newline) will
;;    be repeatedly copied.  (I've tried making it flag an error
;;    instead and that's worse since often the vcursor is sent to
;;    point in some other window, which may be point-max.)
;;  - The vcursor widens when over a tab character or right at the
;;    end of the line.  You're welcome to consider this a feature;
;;    it's just a part of how overlays work.
;;  - The vcursor obscures the real cursor.  Creative use of overlays
;;    could cure this.
;;  - The vcursor does not remember its own previous positions.  If
;;    you cycle it back into a window it was in before, it will be at
;;    point in that window.  Often, that is where a previous recenter
;;    left point, not where the vcursor was before.
;;    (Note, however, that the vcursor does remember where it *is*,
;;    even if it's off-screen.  This can also lead to surprises, but I
;;    don't think it's a bug.)
;;  - vcursor-window-funcall could perhaps be smarter about restoring
;;    the previous window state on failure.
;;  - The logic in vcursor-find-window is rather complicated and
;;    therefore bug-prone, though in practice it seems to work OK.
;;
;; Possible enhancements:
;; It would be easy to implement vcursor-push (save vcursor position
;; as mark and deactivate) and vcursor-pop (deactivate vcursor and
;; move to last pushed position) functions.

;;; Code:

(eval-when-compile (require 'compare-w))

(defgroup vcursor nil
  "Manipulate an alternative (\"virtual\") cursor."
  :prefix "vcursor-"
  :group 'convenience)

(defface vcursor
  '((((class color)) (:foreground "blue" :background "cyan" :underline t))
    (t (:inverse-video t :underline t)))
  "Face for the virtual cursor."
  :group 'vcursor)

(defcustom vcursor-auto-disable nil
  "If non-nil, disable the virtual cursor after use.
Any non-vcursor command will force `vcursor-disable' to be called.
If non-nil but not t, just make sure copying is toggled off, but don't
disable the vcursor."
  :type '(choice (const t) (const nil) (const copy))
  :group 'vcursor)

(defcustom vcursor-modifiers (list 'control 'shift)
  "A list of modifiers that are used to define vcursor key bindings."
  :type '(repeat symbol)
  :group 'vcursor)

;; Needed for defcustom, must be up here
(defun vcursor-cs-binding (base &optional meta)
  (vector (let ((key (append vcursor-modifiers (list (intern base)))))
	    (if meta
		(cons 'meta key)
	      key))))

(defun vcursor-bind-keys (var value)
  "Alter the value of the variable VAR to VALUE, binding keys as required.
VAR is usually `vcursor-key-bindings'.  Normally this function is called
on loading vcursor and from the customize package."
  (set var value)
  (cond
   ((not value));; don't set any key bindings
   ((or (eq value 'oemacs)
	(and (eq value t) (fboundp 'oemacs-version)))
    (global-set-key [C-f1] 'vcursor-toggle-copy)
    (global-set-key [C-f2] 'vcursor-copy)
    (global-set-key [C-f3] 'vcursor-copy-word)
    (global-set-key [C-f4] 'vcursor-copy-line)

    (global-set-key [S-f1] 'vcursor-disable)
    (global-set-key [S-f2] 'vcursor-other-window)
    (global-set-key [S-f3] 'vcursor-goto)
    (global-set-key [S-f4] 'vcursor-swap-point)

    (global-set-key [C-f5] 'vcursor-backward-char)
    (global-set-key [C-f6] 'vcursor-previous-line)
    (global-set-key [C-f7] 'vcursor-next-line)
    (global-set-key [C-f8] 'vcursor-forward-char)

    (global-set-key [M-f5] 'vcursor-beginning-of-line)
    (global-set-key [M-f6] 'vcursor-backward-word)
    (global-set-key [M-f6] 'vcursor-forward-word)
    (global-set-key [M-f8] 'vcursor-end-of-line)

    (global-set-key [S-f5] 'vcursor-beginning-of-buffer)
    (global-set-key [S-f6] 'vcursor-scroll-down)
    (global-set-key [S-f7] 'vcursor-scroll-up)
    (global-set-key [S-f8] 'vcursor-end-of-buffer)

    (global-set-key [C-f9] 'vcursor-isearch-forward)

    (global-set-key [S-f9] 'vcursor-execute-key)
    (global-set-key [S-f10] 'vcursor-execute-command)

;;; Partial dictionary of Oemacs key sequences for you to roll your own,
;;; e.g C-S-up: (global-set-key "\M-[\C-f\M-\C-m" 'vcursor-previous-line)
;;;    Sequence:         Sends:
;;; "\M-[\C-f\M-\C-m"   C-S-up
;;; "\M-[\C-f\M-\C-q"   C-S-down
;;; "\M-[\C-fs"         C-S-left
;;; "\M-[\C-ft"         C-S-right
;;;
;;; "\M-[\C-fw"         C-S-home
;;; "\M-[\C-b\C-o"      S-tab
;;; "\M-[\C-f\M-\C-r"   C-S-insert
;;; "\M-[\C-fu"         C-S-end
;;; "\M-[\C-f\M-\C-s"   C-S-delete
;;; "\M-[\C-f\M-\C-d"   C-S-prior
;;; "\M-[\C-fv"         C-S-next
;;;
;;; "\M-[\C-f^"         C-S-f1
;;; "\M-[\C-f_"         C-S-f2
;;; "\M-[\C-f`"         C-S-f3
;;; "\M-[\C-fa"         C-S-f4
;;; "\M-[\C-fb"         C-S-f5
;;; "\M-[\C-fc"         C-S-f6
;;; "\M-[\C-fd"         C-S-f7
;;; "\M-[\C-fe"         C-S-f8
;;; "\M-[\C-ff"         C-S-f9
;;; "\M-[\C-fg"         C-S-f10
    )
   (t
    (global-set-key (vcursor-cs-binding "up") 'vcursor-previous-line)
    (global-set-key (vcursor-cs-binding "down") 'vcursor-next-line)
    (global-set-key (vcursor-cs-binding "left") 'vcursor-backward-char)
    (global-set-key (vcursor-cs-binding "right") 'vcursor-forward-char)

    (global-set-key (vcursor-cs-binding "return") 'vcursor-disable)
    (global-set-key (vcursor-cs-binding "insert")  'vcursor-copy)
    (global-set-key (vcursor-cs-binding "delete") 'vcursor-copy-word)
    (global-set-key (vcursor-cs-binding "remove") 'vcursor-copy-word)
    (global-set-key (vcursor-cs-binding "tab") 'vcursor-toggle-copy)
    (global-set-key (vcursor-cs-binding "backtab") 'vcursor-toggle-copy)
    (global-set-key (vcursor-cs-binding "home") 'vcursor-beginning-of-buffer)
    (global-set-key (vcursor-cs-binding "up" t) 'vcursor-beginning-of-buffer)
    (global-set-key (vcursor-cs-binding "end") 'vcursor-end-of-buffer)
    (global-set-key (vcursor-cs-binding "down" t) 'vcursor-end-of-buffer)
    (global-set-key (vcursor-cs-binding "prior") 'vcursor-scroll-down)
    (global-set-key (vcursor-cs-binding "next") 'vcursor-scroll-up)

    (global-set-key (vcursor-cs-binding "f6") 'vcursor-other-window)
    (global-set-key (vcursor-cs-binding "f7") 'vcursor-goto)

    (global-set-key (vcursor-cs-binding "select")
		    'vcursor-swap-point) ; DEC keyboards
    (global-set-key (vcursor-cs-binding "tab" t) 'vcursor-swap-point)

    (global-set-key (vcursor-cs-binding "find")
		    'vcursor-isearch-forward) ; DEC keyboards
    (global-set-key (vcursor-cs-binding "f8") 'vcursor-isearch-forward)

    (global-set-key (vcursor-cs-binding "left" t) 'vcursor-beginning-of-line)
    (global-set-key (vcursor-cs-binding "right" t) 'vcursor-end-of-line)

    (global-set-key (vcursor-cs-binding "prior" t) 'vcursor-backward-word)
    (global-set-key (vcursor-cs-binding "next" t) 'vcursor-forward-word)

    (global-set-key (vcursor-cs-binding "return" t) 'vcursor-copy-line)

    (global-set-key (vcursor-cs-binding "f9") 'vcursor-execute-key)
    (global-set-key (vcursor-cs-binding "f10") 'vcursor-execute-command)
    )))

(defcustom vcursor-key-bindings nil
  "How to bind keys when vcursor is loaded.
If t, guess; if `xterm', use bindings suitable for an X terminal; if
`oemacs', use bindings which work on a PC with Oemacs.  If nil, don't
define any key bindings.

Default is nil."
  :type '(choice (const t) (const nil) (const xterm) (const oemacs))
  :group 'vcursor
  :set 'vcursor-bind-keys
  :version "20.3")

(defcustom vcursor-interpret-input nil
  "If non-nil, input from the vcursor is treated as interactive input.
This will cause text insertion to be much slower.  Note that no special
interpretation of strings is done: \"\C-x\" is a string of four
characters.  The default is simply to copy strings."
  :type 'boolean
  :group 'vcursor
  :version "20.3")

(defcustom vcursor-string "**>"
  "String used to show the vcursor position on dumb terminals."
  :type 'string
  :group 'vcursor
  :version "20.3")

(defvar vcursor-overlay nil
  "Overlay for the virtual cursor.
It is nil if that is not enabled.")

(defvar vcursor-window nil
  "Last window to have displayed the virtual cursor.
See the function `vcursor-find-window' for how this is used.")

(defvar vcursor-last-command nil
  "Non-nil if last command was a vcursor command.
The commands `vcursor-copy', `vcursor-relative-move' and the ones for
scrolling set this.  It is used by the `vcursor-auto-disable' code.")
;; could do some memq-ing with last-command instead, but this will
;; automatically handle any new commands using the primitives.

(defcustom vcursor-copy-flag nil
  "Non-nil means moving vcursor should copy characters moved over to point."
  :type 'boolean
  :group 'vcursor)

(defvar vcursor-temp-goal-column nil
  "Keeps track of temporary goal columns for the virtual cursor.")

(defvar vcursor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'vcursor-use-vcursor-map)

    (define-key map "\C-p" 'vcursor-previous-line)
    (define-key map "\C-n" 'vcursor-next-line)
    (define-key map "\C-b" 'vcursor-backward-char)
    (define-key map "\C-f" 'vcursor-forward-char)

    (define-key map "\r" 'vcursor-disable)
    (define-key map " " 'vcursor-copy)
    (define-key map "\C-y" 'vcursor-copy-word)
    (define-key map "\C-i" 'vcursor-toggle-copy)
    (define-key map "<" 'vcursor-beginning-of-buffer)
    (define-key map ">" 'vcursor-end-of-buffer)
    (define-key map "\M-v" 'vcursor-scroll-down)
    (define-key map "\C-v" 'vcursor-scroll-up)
    (define-key map "o" 'vcursor-other-window)
    (define-key map "g" 'vcursor-goto)
    (define-key map "x" 'vcursor-swap-point)
    (define-key map "\C-s" 'vcursor-isearch-forward)
    (define-key map "\C-r" 'vcursor-isearch-backward)
    (define-key map "\C-a" 'vcursor-beginning-of-line)
    (define-key map "\C-e" 'vcursor-end-of-line)
    (define-key map "\M-w" 'vcursor-forward-word)
    (define-key map "\M-b" 'vcursor-backward-word)
    (define-key map "\M-l" 'vcursor-copy-line)
    (define-key map "c" 'vcursor-compare-windows)
    (define-key map "k" 'vcursor-execute-key)
    (define-key map "\M-x" 'vcursor-execute-command)
    map)
  "Keymap for vcursor command.")
;; This seems unused, but it was done as part of define-prefix-command,
;; so let's keep it for now.
(fset 'vcursor-map vcursor-map)

;; If vcursor-key-bindings is already set on loading, bind the keys now.
;; This hybrid way of doing it retains compatibility while allowing
;; customize to work smoothly.
(if vcursor-key-bindings
    (vcursor-bind-keys 'vcursor-key-bindings vcursor-key-bindings))

(defun vcursor-locate ()
  "Go to the starting point of the virtual cursor.
If that's disabled, don't go anywhere but don't complain."
  ;; This is where we go off-mass-shell.  Assume there is a
  ;; save-excursion to get us back to the pole, er, point.

  (and (overlayp vcursor-overlay)
       (overlay-buffer vcursor-overlay)
       (set-buffer (overlay-buffer vcursor-overlay))
       (goto-char (overlay-start vcursor-overlay)))
  )

(defun vcursor-find-window (&optional not-this new-win this-frame)
  "Return a suitable window for displaying the virtual cursor.
This is the first window in cyclic order where the vcursor is visible.

With optional NOT-THIS non-nil never return the current window.

With NEW-WIN non-nil, display the virtual cursor buffer in another
window if the virtual cursor is not currently visible \(note, however,
that this function never changes `window-point'\).

With THIS-FRAME non-nil, don't search other frames for a new window
\(though if the vcursor is already off-frame then its current window is
always considered, and the value of `pop-up-frames' is always respected\).

Returns nil if the virtual cursor is not visible anywhere suitable.
Set `vcursor-window' to the returned value as a side effect."

  ;; The order of priorities (respecting NOT-THIS) is (1)
  ;; vcursor-window if the virtual cursor is visible there (2) any
  ;; window displaying the virtual cursor (3) vcursor-window provided
  ;; it is still displaying the buffer containing the virtual cursor and
  ;; is not selected (4) any unselected window displaying the vcursor
  ;; buffer (5) with NEW-WIN, a window selected by display-buffer (so
  ;; the variables pop-up-windows and pop-up-frames are significant)
  ;; (6) nil.

  (let ((thiswin (selected-window)) winok winbuf)
    (save-excursion
      (vcursor-locate)
      (or (and (window-live-p vcursor-window)
	       (eq (current-buffer) (window-buffer vcursor-window))
	       (not (and not-this (eq thiswin vcursor-window))))
	  (setq vcursor-window nil))
      (or (and vcursor-window		; choice 1
	       (pos-visible-in-window-p (point) vcursor-window))
	  (progn
	    (walk-windows
	     (function
	      (lambda (win)
		(and (not winok)
		     (eq (current-buffer) (window-buffer win))
		     (not (and not-this (eq thiswin win)))
		     (cond
		      ((pos-visible-in-window-p (point) win) (setq winok win))
		      ((eq thiswin win))
		      ((not winbuf) (setq winbuf win))))))
	     nil (not this-frame))
	    (setq vcursor-window
		  (cond
		   (winok)		; choice 2
		   ((and vcursor-window	; choice 3
			 (not (eq thiswin vcursor-window))) vcursor-window)
		   (winbuf)		; choice 4
		   (new-win (display-buffer (current-buffer) t)) ; choice 5
		   (t nil)))))))	; default (choice 6)
  vcursor-window
  )

(defun vcursor-toggle-copy (&optional arg nomsg)
  "Toggle copying to point when the vcursor is moved.
With a prefix ARG, turn on if non-negative, off if negative.
Display a message unless optional NOMSG is non-nil."
  (interactive "P")
  (setq vcursor-copy-flag
	(cond ((not arg) (not vcursor-copy-flag))
	      ((< (prefix-numeric-value arg) 0) nil)
	      (t))
	vcursor-last-command t)
  (or nomsg (message "Copying from the vcursor is now %s."
		     (if vcursor-copy-flag "on" "off")))
  )

(defun vcursor-move (pt &optional leave-b leave-w)
  "Move the virtual cursor to the character to the right of PT.
PT is an absolute location in the current buffer.  With optional
LEAVE-B, PT is in the same buffer the vcursor is currently in.

If the new virtual cursor location would not be visible, display it in
another window.  With LEAVE-W, use the current `vcursor-window'."
  ;; this works even if we're on-mass-shell, but usually we won't be.

  (save-excursion
    (and leave-b (vcursor-check t)
	 (set-buffer (overlay-buffer vcursor-overlay)))
    (if (eq pt (point-max))
	  (setq pt (1- pt)))
    (if (vcursor-check t)
	(move-overlay vcursor-overlay pt (+ pt 1) (current-buffer))
      (setq vcursor-overlay (make-overlay pt (+ pt 1)))
      (or window-system
	  (display-color-p)
	  (overlay-put vcursor-overlay 'before-string vcursor-string))
      (overlay-put vcursor-overlay 'face 'vcursor))
    (or leave-w (vcursor-find-window nil t))
    ;; vcursor-window now contains the right buffer
    (or (pos-visible-in-window-p pt vcursor-window)
	(set-window-point vcursor-window pt)))
  )

(defun vcursor-insert (text)
  "Insert TEXT, respecting `vcursor-interpret-input'."
  (if vcursor-interpret-input
      (setq unread-command-events
	    (append (listify-key-sequence text) unread-command-events))
    (insert text))
  )

(defun vcursor-relative-move (func &rest args)
  "Call FUNC with arbitrary ARGS ... to move the virtual cursor.

This is called by most of the virtual-cursor motion commands."
  (let (text opoint)
    (save-excursion
      (vcursor-locate)
      (setq opoint (point))
      (apply func args)
      (and (eq opoint (point-max)) (eq opoint (point))
	   (signal 'end-of-buffer nil))
      (vcursor-move (point))
      (if vcursor-copy-flag (setq text (buffer-substring opoint (point)))))
    (if text (vcursor-insert text)))
  (setq vcursor-last-command t)
  )

(defun vcursor-goto (&optional arg)
  "Move the real cursor to the virtual cursor position.
If the virtual cursor is (or was recently) visible in another window,
switch to that first.  Without a prefix ARG, disable the virtual
cursor as well."

  (interactive "P")
  (and (vcursor-find-window) (select-window vcursor-window))
  (let ((buf (and vcursor-overlay (overlay-buffer vcursor-overlay))))
    (and buf (not (eq (current-buffer) buf)) (switch-to-buffer buf)))
  (vcursor-locate)
  (or arg (vcursor-disable))
  )

(defun vcursor-swap-point ()
  "Swap the location of point and that of the virtual cursor.

The virtual cursor window becomes the selected window and the old
window becomes the virtual cursor window.  If the virtual cursor would
not be visible otherwise, display it in another window."

  (interactive)
  (let ((buf (current-buffer)) (here (point)) (win (selected-window)))
    (vcursor-goto) ; will disable the vcursor
    (with-current-buffer buf
      (setq vcursor-window win)
      (vcursor-move here)))
)

(defun vcursor-scroll-up (&optional n)
  "Scroll up the vcursor window ARG lines or near full screen if none.
The vcursor will always appear in an unselected window."

  (interactive "P")
  (vcursor-window-funcall 'scroll-up n)
)

(defun vcursor-scroll-down (&optional n)
  "Scroll down the vcursor window ARG lines or near full screen if none.
The vcursor will always appear in an unselected window."

  (interactive "P")
  (vcursor-window-funcall 'scroll-down n)
  )

(defun vcursor-isearch-forward (&optional rep norecurs)
  "Perform forward incremental search in the virtual cursor window.
The virtual cursor is moved to the resulting point; the ordinary
cursor stays where it was."

  (interactive "P")
  (vcursor-window-funcall 'isearch-forward rep norecurs)
  )

(defun vcursor-isearch-backward (&optional rep norecurs)
  "Perform backward incremental search in the virtual cursor window.
The virtual cursor is moved to the resulting point; the ordinary
cursor stays where it was."

  (interactive "P")
  (vcursor-window-funcall 'isearch-backward rep norecurs)
  )

(defun vcursor-window-funcall (func &rest args)
  "Call FUNC with ARGS ... in a virtual cursor window.
A window other than the currently-selected one will always be used.
The virtual cursor is moved to the value of point when the function
returns.

If FUNC is a list, call the car of the list interactively, ignoring
ARGS.  In this case, a new window will not be created if the vcursor
is visible in the current one."
;; that's to avoid messing up compatibility with old versions
;; by introducing a new argument, which would have to come before ARGS.

  (vcursor-find-window (not (and (listp func) (vcursor-check t))) t)
  (save-excursion
    (let ((sw (selected-window)) text)
      ;; We can't use save-window-excursion because that would restore
      ;; the original display in the window we may want to alter.
      (unwind-protect
	  (let ((here (point)))
	    (select-window vcursor-window)
	    (vcursor-locate)
	    (if (listp func)
		(call-interactively (car func))
	      (apply func args))
	    (setq vcursor-window (selected-window))
	    (and vcursor-copy-flag
		 (eq (current-buffer) (overlay-buffer vcursor-overlay))
		 (setq text (buffer-substring here (point))))
	    ;; vcursor-window and the current buffer are definitely
	    ;; right, so make sure vcursor-move doesn't pick others.
	    (vcursor-move (point) nil t))
	(select-window sw))
      (if text (vcursor-insert text))))
  (setq vcursor-last-command t)
  )

(defun vcursor-get-char-count (func &rest args)
  "Apply FUNC to ARGS ... and return the number of characters moved.
Point is temporarily set to the virtual cursor position before FUNC
is called.

This is called by most of the virtual-cursor copying commands to find
out how much to copy."

  (vcursor-check)
  (with-current-buffer (overlay-buffer vcursor-overlay)
    (let ((start (goto-char (overlay-start vcursor-overlay))))
      (- (progn (apply func args) (point)) start)))
  )

;; Make sure the virtual cursor is active.  Unless arg is non-nil,
;; report an error if it is not.
(defun vcursor-check (&optional arg)
  (cond
   ((and (overlayp vcursor-overlay) (overlay-start vcursor-overlay))
    t)
   (arg nil)
   (t (error "The virtual cursor is not active now")))
  )

(define-minor-mode vcursor-use-vcursor-map
  "Toggle the state of the vcursor key map.
With a prefix argument ARG, enable it if ARG is positive, and disable
it otherwise.  If called from Lisp, enable it if ARG is omitted or nil.
When on, the keys defined in it are mapped directly on top of the main
keymap, allowing you to move the vcursor with ordinary motion keys.
An indication \"!VC\" appears in the mode list.  The effect is
local to the current buffer.
Disabling the vcursor automatically turns this off."
  :keymap vcursor-map
  :lighter " !VC")

(defun vcursor-disable (&optional arg)
  "Disable the virtual cursor.
Next time you use it, it will start from point.

With a positive prefix ARG, the first window in cyclic order
displaying the virtual cursor (or which was recently displaying the
virtual cursor) will be deleted unless it's the selected window.

With a negative prefix argument, enable the virtual cursor: make it
active at the same point as the real cursor.

Copying mode is always turned off: the next use of the vcursor will
not copy text until you turn it on again."

  (interactive "P")
  (if (overlayp vcursor-overlay)
      (progn
	(delete-overlay vcursor-overlay)
	(setq vcursor-overlay nil)))
  (cond
   ((not (vcursor-find-window t)))
   ((or (not arg) (< (prefix-numeric-value arg) 0)))
   ((delete-window vcursor-window)))
  (cond
   ((and arg (< (prefix-numeric-value arg) 0))
    (vcursor-move (point))
    (setq vcursor-window (selected-window)))
   (vcursor-use-vcursor-map (vcursor-use-vcursor-map 0)))
  (setq vcursor-copy-flag nil)
  )

(defun vcursor-other-window (n &optional all-frames)
  "Activate the virtual cursor in another window.
This is the next window cyclically after one currently showing the
virtual cursor, or else after the current selected window.  If there
is no other window, the current window is split.

Arguments N and optional ALL-FRAMES are the same as with `other-window'.
ALL-FRAMES is also used to decide whether to split the window."

  (interactive "p")
  (if (if (fboundp 'oemacs-version)
	  (one-window-p nil)
	(one-window-p nil all-frames))
      (display-buffer (current-buffer) t))
  (save-excursion
    (save-window-excursion
      ;; We don't use fancy vcursor-find-window trickery, since we're
      ;; quite happy to have the vcursor cycle back into the current
      ;; window.
      (let ((win (vcursor-find-window nil nil (not all-frames))))
	(if win (select-window win))
	;; else start from here
	(other-window n all-frames)
	(vcursor-disable -1))))
  )

;; vcursor-compare-windows is copied from compare-w.el with only
;; minor modifications; these are too bound up with the function
;; to make it really useful to call compare-windows itself.
(defun vcursor-compare-windows (&optional ignore-whitespace)
  "Compare text in current window with text in window with vcursor.
Compares the text starting at point in the current window and at the
vcursor position in the other window, moving over text in each one as
far as they match.

A prefix argument, if any, means ignore changes in whitespace.
The variable `compare-windows-whitespace' controls how whitespace is skipped.
If `compare-ignore-case' is non-nil, changes in case are also ignored."
  (interactive "P")
  ;; (vcursor-window-funcall 'compare-windows arg)
  (require 'compare-w)
  (let* (p1 p2 maxp1 maxp2 b1 b2 w2
	    success
	    (opoint1 (point))
	    opoint2
	    (skip-whitespace (if ignore-whitespace
				 compare-windows-whitespace)))
    (setq p1 (point) b1 (current-buffer))
    (setq w2 (vcursor-find-window t t))
    (if (or (eq w2 (selected-window)) (not w2))
	(error "No other window with vcursor"))
    (save-excursion
      (vcursor-locate)
      (setq p2 (point) b2 (current-buffer)))
    (setq opoint2 p2)
    (setq maxp1 (point-max))
    (with-current-buffer b2
      (setq maxp2 (point-max)))

    (setq success t)
    (while success
      (setq success nil)
      ;; if interrupted, show how far we've gotten
      (goto-char p1)
      (vcursor-move p2 t)

      ;; If both buffers have whitespace next to point,
      ;; optionally skip over it.

      (and skip-whitespace
	   (save-excursion
	     (let (p1a p2a result1 result2)
	       (setq result1
		     (if (stringp skip-whitespace)
			 (compare-windows-skip-whitespace opoint1)
		       (funcall skip-whitespace opoint1)))
	       (setq p1a (point))
	       (set-buffer b2)
	       (goto-char p2)
	       (setq result2
		     (if (stringp skip-whitespace)
			 (compare-windows-skip-whitespace opoint2)
		       (funcall skip-whitespace opoint2)))
	       (setq p2a (point))
	       (if (or (stringp skip-whitespace)
		       (and result1 result2 (eq result1 result2)))
		   (setq p1 p1a
			 p2 p2a)))))

      ;; Try advancing comparing 1000 chars at a time.
      ;; When that fails, go 500 chars at a time, and so on.
      (let ((size 1000)
	    success-1
	    (case-fold-search compare-ignore-case))
	(while (> size 0)
	  (setq success-1 t)
	  ;; Try comparing SIZE chars at a time, repeatedly, till that fails.
	  (while success-1
	    (setq size (min size (- maxp1 p1) (- maxp2 p2)))
	    (setq success-1
		  (and (> size 0)
		       (= 0 (compare-buffer-substrings b2 p2 (+ size p2)
						       b1 p1 (+ size p1)))))
	    (if success-1
		(setq p1 (+ p1 size) p2 (+ p2 size)
		      success t)))
	  ;; If SIZE chars don't match, try fewer.
	  (setq size (/ size 2)))))

    (goto-char p1)
    (vcursor-move p2 t)
    (if (= (point) opoint1)
	(ding)))
)

(defun vcursor-next-line (arg)
  "Move the virtual cursor forward ARG lines."
  ;; This is next-line rewritten for the vcursor.  Maybe it would
  ;; be easier simply to rewrite line-move.
  (interactive "p")
  (let (temporary-goal-column opoint text)
    (save-excursion
      (vcursor-locate)
      (setq temporary-goal-column
	    (if (or (eq last-command 'vcursor-next-line)
		    (eq last-command 'vcursor-previous-line))
		(progn
		  (setq last-command 'next-line) ; trick line-move
		  vcursor-temp-goal-column)
	      (if (and track-eol (eolp)
		       (or (not (bolp)) (eq last-command 'end-of-line)))
		  9999
		(current-column)))
	    opoint (point))
      (line-move arg)
      (and (eq opoint (point-max)) (eq opoint (point))
	   (signal 'end-of-buffer nil))
      (if vcursor-copy-flag (setq text (buffer-substring opoint (point))))
      (vcursor-move (point))
      (setq vcursor-temp-goal-column temporary-goal-column
	    vcursor-last-command t))
    (if text (vcursor-insert text)))
  )

(defun vcursor-previous-line (arg)
  "Move the virtual cursor back ARG lines."
  (interactive "p")
  (vcursor-next-line (- arg))
  )

(defun vcursor-forward-char (arg)
  "Move the virtual cursor forward ARG characters."
  (interactive "p")
  (vcursor-relative-move 'forward-char arg)
  )

(defun vcursor-backward-char (arg)
  "Move the virtual cursor backward ARG characters."
  (interactive "p")
  (vcursor-relative-move 'backward-char arg)
  )

(defun vcursor-forward-word (arg)
  "Move the virtual cursor forward ARG words."
  (interactive "p")
  (vcursor-relative-move 'forward-word arg)
  )

(defun vcursor-backward-word (arg)
  "Move the virtual cursor backward ARG words."
  (interactive "p")
  (vcursor-relative-move 'backward-word arg)
  )

(defun vcursor-beginning-of-line (arg)
  "Move the virtual cursor to beginning of its current line.
ARG is as for `beginning-of-line'."
  (interactive "P")
  (vcursor-relative-move 'beginning-of-line
			 (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-end-of-line (arg)
  "Move the virtual cursor to end of its current line.
ARG is as for `end-of-line'."
  (interactive "P")
  (vcursor-relative-move 'end-of-line
			 (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-beginning-of-buffer (&optional arg)
  "Move the virtual cursor to the beginning of its buffer.
ARG is as for `beginning-of-buffer'."
  (interactive "P")
  (vcursor-relative-move
   (lambda (arg)
     (goto-char (if arg (/ (* arg (- (point-max) (point-min))) 10)
		  (point-min))))
   (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-end-of-buffer (&optional arg)
  "Move the virtual cursor to the end of its buffer.
ARG is as for `end-of-buffer'.

Actually, the vcursor is moved to the second from last character or it
would be invisible."
  (interactive "P")
  (vcursor-relative-move
   (lambda (arg)
     (goto-char (if arg (- (point-max)
			   (/ (* arg (- (point-max) (point-min))) 10))
		  (point-max))))
   (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-execute-command (cmd)
  "Execute COMMAND for the virtual cursor.
COMMAND is called interactively.  Not all commands (in fact, only a
small subset) are useful."
  (interactive "CCommand: ")
  (vcursor-window-funcall (list cmd))
  )

(defun vcursor-execute-key ()
  "Read a key sequence and execute the bound command for the virtual cursor.
The key sequence is read at the vcursor location.  The command found
is called interactively, so prefix argument etc. are usable."
  (interactive)
  (let (cmd)
    (save-excursion
      ;; We'd like to avoid the display changing when we locate
      ;; to the vcursor position and read a key sequence.
      (vcursor-find-window (not (vcursor-check t)) t)
      (save-window-excursion
	(select-window vcursor-window)
	(vcursor-locate)
	(setq cmd (key-binding (read-key-sequence "Key sequence: ")))))
    (vcursor-window-funcall (list cmd)))
  )

(defun vcursor-copy (arg)
  "Copy ARG characters from the virtual cursor position to point."
  (interactive "p")
  (vcursor-check)
  (vcursor-insert
   (with-current-buffer (overlay-buffer vcursor-overlay)
     (let* ((ostart (overlay-start vcursor-overlay))
	    (end (+ ostart arg)))
       (prog1
	   (buffer-substring ostart end)
	 (vcursor-move end)))))
  (setq vcursor-last-command t)
)

(defun vcursor-copy-word (arg)
  "Copy ARG words from the virtual cursor position to point."
  (interactive "p")
  (vcursor-copy (vcursor-get-char-count 'forward-word arg))
  )

(defun vcursor-copy-line (arg)
  "Copy up to ARGth line after virtual cursor position.
With no argument, copy to the end of the current line.

Behavior with regard to newlines is similar (but not identical) to
`kill-line'; the main difference is that whitespace at the end of the
line is treated like ordinary characters."

  (interactive "P")
  (let* ((num (prefix-numeric-value arg))
	 (count (vcursor-get-char-count 'end-of-line num)))
    (vcursor-copy (if (or (= count 0) arg) (1+ count) count)))
  )

(define-obsolete-function-alias
  'vcursor-toggle-vcursor-map 'vcursor-use-vcursor-map "23.1")

(defun vcursor-post-command ()
  (and vcursor-auto-disable (not vcursor-last-command)
       vcursor-overlay
       (if (eq vcursor-auto-disable t)
	   (vcursor-disable)
	 (vcursor-toggle-copy -1 t)))
  (setq vcursor-last-command nil)
  )

(add-hook 'post-command-hook 'vcursor-post-command)

(provide 'vcursor)

;;; vcursor.el ends here
