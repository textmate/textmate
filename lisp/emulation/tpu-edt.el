;;; tpu-edt.el --- Emacs emulating TPU emulating EDT

;; Copyright (C) 1993-1995, 2000-2012 Free Software Foundation, Inc.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Version: 4.5
;; Keywords: emulations

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

;; TPU-edt is based on tpu.el by Jeff Kowalski and Bob Covey.

;;; Commentary:

;; %% TPU-edt -- Emacs emulating TPU emulating EDT

;; %% Contents

;;  % Introduction
;;  % Differences Between TPU-edt and DEC TPU/edt
;;  % Starting TPU-edt
;;  % Customizing TPU-edt using the Emacs Initialization File
;;  % Regular Expressions in TPU-edt


;; %% Introduction

;;    TPU-edt emulates the popular DEC VMS editor EDT (actually, it emulates
;;    DEC TPU's EDT emulation, hence the name TPU-edt).  TPU-edt features the
;;    following TPU/edt functionality:

;;     .  EDT keypad
;;     .  On-line help
;;     .  Repeat counts
;;     .  Scroll margins
;;     .  Learn sequences
;;     .  Free cursor mode
;;     .  Rectangular cut and paste
;;     .  Multiple windows and buffers
;;     .  TPU line-mode REPLACE command
;;     .  Wild card search and substitution
;;     .  Configurable through an initialization file
;;     .  History recall of search strings, file names, and commands

;;    Please note that TPU-edt does NOT emulate TPU.  It emulates TPU's EDT
;;    emulation.  Very few TPU line-mode commands are supported.

;;    TPU-edt, like its VMS cousin, works on VT-series terminals with DEC
;;    style keyboards.  VT terminal emulators, including xterm with the
;;    appropriate key translations, work just fine too.

;;    TPU-edt works with X-windows.  This is accomplished through a TPU-edt
;;    X key map.  The tpu-mapper command creates this map and stores it in a
;;    file.  See the tpu-mapper command help for more information, or just
;;    run it and follow the directions.


;; %% Differences Between TPU-edt and DEC TPU/edt

;;    In some cases, Emacs doesn't support text highlighting, so selected
;;    regions are not shown in inverse video.  Emacs uses the concept of "the
;;    mark".  The mark is set at one end of a selected region; the cursor is
;;    at the other.  In cases where the selected region cannot be shown in
;;    inverse video an at sign (@) appears in the mode line when mark is set.
;;    The native Emacs command ^X^X (Control-X twice) exchanges the cursor
;;    with the mark; this provides a handy way to find the location of the
;;    mark.

;;    In TPU the cursor can be either bound or free.  Bound means the cursor
;;    cannot wander outside the text of the file being edited.  Free means
;;    the arrow keys can move the cursor past the ends of lines.  Free is the
;;    default mode in TPU; bound is the only mode in EDT.  Bound is the only
;;    mode in the base version of TPU-edt; optional extensions add an
;;    approximation of free mode, see the commentary in tpu-extras.el for
;;    details.

;;    Like TPU, Emacs uses multiple buffers.  Some buffers are used to hold
;;    files you are editing; other "internal" buffers are used for Emacs's own
;;    purposes (like showing you help).  Here are some commands for dealing
;;    with buffers.

;;       Gold-B   moves to next buffer, including internal buffers
;;       Gold-N   moves to next buffer containing a file
;;       Gold-M   brings up a buffer menu (like TPU "show buffers")

;;    Emacs is very fond of throwing up new windows.  Dealing with all these
;;    windows can be a little confusing at first, so here are a few commands
;;    to that may help:

;;       Gold-Next_Scr  moves to the next window on the screen
;;       Gold-Prev_Scr  moves to the previous window on the screen
;;       Gold-TAB       also moves to the next window on the screen

;;       Control-x 1    deletes all but the current window
;;       Control-x 0    deletes the current window

;;    Note that the buffers associated with deleted windows still exist!

;;    Like TPU, TPU-edt has a "command" function, invoked with Gold-KP7 or
;;    Do.  Most of the commands available are Emacs commands.  Some TPU
;;    commands are available, they are: replace, exit, quit, include, and
;;    Get (unfortunately, "get" is an internal Emacs function, so we are
;;    stuck with "Get" - to make life easier, Get is available as Gold-g).

;;    TPU-edt supports the recall of commands, file names, and search
;;    strings.  The history of strings recalled differs slightly from
;;    TPU/edt, but it is still very convenient.

;;    Help is available!  The traditional help keys (Help and PF2) display
;;    a small help file showing the default keypad layout, control key
;;    functions, and Gold key functions.  Pressing any key inside of help
;;    splits the screen and prints a description of the function of the
;;    pressed key.  Gold-PF2 invokes the native Emacs help, with its
;;    zillions of options.

;;    Thanks to Emacs, TPU-edt has some extensions that may make your life
;;    easier, or at least more interesting.  For example, Gold-r toggles
;;    TPU-edt rectangular mode.  In rectangular mode, Remove and Insert work
;;    on rectangles.  Likewise, Gold-* toggles TPU-edt regular expression
;;    mode.  In regular expression mode Find, Find Next, and the line-mode
;;    replace command work with regular expressions.  [A regular expression
;;    is a pattern that denotes a set of strings; like VMS wildcards.]

;;    Emacs also gives TPU-edt the undo and occur functions.  Undo does
;;    what it says; it undoes the last change.  Multiple undos in a row
;;    undo multiple changes.  For your convenience, undo is available on
;;    Gold-u.  Occur shows all the lines containing a specific string in
;;    another window.  Moving to that window, and typing ^C^C (Control-C
;;    twice) on a particular line moves you back to the original window
;;    at that line.  Occur is on Gold-o.

;;    Finally, as you edit, remember that all the power of Emacs is at
;;    your disposal.  It really is a fantastic tool.  You may even want to
;;    take some time and read the Emacs tutorial; perhaps not to learn the
;;    native Emacs key bindings, but to get a feel for all the things
;;    Emacs can do for you.  The Emacs tutorial is available from the
;;    Emacs help function: "Gold-PF2 t"


;; %% Starting TPU-edt

;;    All you have to do to start TPU-edt, is turn it on.  This can be
;;    done from the command line when running Emacs.

;;        prompt> emacs -f tpu-edt

;;    If you've already started Emacs, turn on TPU-edt using the tpu-edt
;;    command.  First press `M-x' (that's usually `ESC' followed by `x')
;;    and type `tpu-edt' followed by a carriage return.

;;    If you like TPU-edt and want to use it all the time, you can start
;;    TPU-edt using the Emacs initialization file, .emacs.  Simply create
;;    a .emacs file in your home directory containing the line:

;;        (tpu-edt)

;;    That's all you need to do to start TPU-edt.


;; %% Customizing TPU-edt using the Emacs Initialization File

;;    The following is a sample Emacs initialization file.  It shows how to
;;    invoke TPU-edt, and how to customize it.

;;    ; .emacs - a sample Emacs initialization file

;;    ; Turn on TPU-edt
;;    (tpu-edt)

;;    ; Set scroll margins 10% (top) and 15% (bottom).
;;    (tpu-set-scroll-margins "10%" "15%")

;;    ; Load the vtxxx terminal control functions.
;;    (load "vt-control" t)

;;    ; TPU-edt treats words like EDT; here's how to add word separators.
;;    ; Note that backslash (\) and double quote (") are quoted with '\'.
;;    (tpu-add-word-separators "]\\[-_,.\"=+()'/*#:!&;$")

;;    ; Emacs is happy to save files without a final newline; other Unix
;;    ; programs hate that!  Here we make sure that files end with newlines.
;;    (setq require-final-newline t)

;;    ; Emacs uses Control-s and Control-q.  Problems can occur when using
;;    ; Emacs on terminals that use these codes for flow control (Xon/Xoff
;;    ; flow control).  These lines disable Emacs's use of these characters.
;;    (global-unset-key "\C-s")
;;    (global-unset-key "\C-q")

;;    ; The Emacs universal-argument function is very useful.
;;    ; This line maps universal-argument to Gold-PF1.
;;    (define-key tpu-gold-map [kp_f1] 'universal-argument)      ; Gold-PF1

;;    ; Make KP7 move by paragraphs, instead of pages.
;;    (define-key tpu-global-map [kf_7] 'tpu-paragraph)          ; KP7

;;    ; Repeat the preceding mappings for X-windows.
;;    (cond
;;     (window-system
;;      (define-key tpu-global-map [kp_7] 'tpu-paragraph)        ; KP7
;;      (define-key tpu-gold-map [kp_f1] 'universal-argument)))  ; GOLD-PF1

;;    ; Display the TPU-edt version.
;;    (tpu-version)


;; %% Regular Expressions in TPU-edt

;;    Gold-* toggles TPU-edt regular expression mode.  In regular expression
;;    mode, find, find next, replace, and substitute accept Emacs regular
;;    expressions.  A complete list of Emacs regular expressions can be found
;;    using the Emacs "info" command (it's somewhat like the VMS help
;;    command).  Try the following sequence of commands:

;;        DO info             <enter info mode>
;;        m emacs             <select the "emacs" topic>
;;        m regexs            <select the "regular expression" topic>

;;    Type "q" to quit out of info mode.

;;    There is a problem in regular expression mode when searching for empty
;;    strings, like beginning-of-line (^) and end-of-line ($).  When searching
;;    for these strings, find-next may find the current string, instead of the
;;    next one.  This can cause global replace and substitute commands to loop
;;    forever in the same location.  For this reason, commands like

;;        replace "^" "> "       <add "> " to beginning of line>
;;        replace "$" "00711"    <add "00711" to end of line>

;;    may not work properly.

;;    Commands like those above are very useful for adding text to the
;;    beginning or end of lines.  They might work on a line-by-line basis, but
;;    go into an infinite loop if the "all" response is specified.  If the
;;    goal is to add a string to the beginning or end of a particular set of
;;    lines TPU-edt provides functions to do this.

;;        Gold-^  Add a string at BOL in region or buffer
;;        Gold-$  Add a string at EOL in region or buffer

;;    There is also a TPU-edt interface to the native Emacs string replacement
;;    commands.  Gold-/ invokes this command.  It accepts regular expressions
;;    if TPU-edt is in regular expression mode.  Given a repeat count, it will
;;    perform the replacement without prompting for confirmation.

;;    This command replaces empty strings correctly, however, it has its
;;    drawbacks.  As a native Emacs command, it has a different interface
;;    than the emulated TPU commands.  Also, it works only in the forward
;;    direction, regardless of the current TPU-edt direction.

;;; Todo/Bugs:

;; We shouldn't use vt100 ESC sequences since it is uselessly fighting
;; against function-key-map.  Better use real key names.

;;; Code:

;; we use picture-mode functions
(require 'picture)

(defgroup tpu nil
  "Emacs emulating TPU emulating EDT."
  :prefix "tpu-"
  :group 'emulations)


;;;
;;;  Version Information
;;;
(defconst tpu-version "4.5" "TPU-edt version number.")


;;;
;;;  User Configurable Variables
;;;
(defcustom tpu-have-ispell t
  "Non-nil means `tpu-spell-check' uses `ispell-region' for spell checking.
Otherwise, use `spell-region'."
  :type 'boolean
  :group 'tpu)
(make-obsolete-variable 'tpu-have-ispell "the `spell' package is obsolete."
                        "23.1")

(defcustom tpu-kill-buffers-silently nil
  "If non-nil, TPU-edt kills modified buffers without asking."
  :type 'boolean
  :group 'tpu)

(defcustom tpu-percent-scroll 75
  "Percentage of the screen to scroll for next/previous screen commands."
  :type 'integer
  :group 'tpu)

(defcustom tpu-pan-columns 16
  "Number of columns the tpu-pan functions scroll left or right."
  :type 'integer
  :group 'tpu)


;;;
;;;  Global Keymaps
;;;

(defvar tpu-gold-map
  (let ((map (make-keymap)))
    ;; Previously we used escape sequences here.  We now instead presume
    ;; that term/*.el does its job to map the escape sequence to the right
    ;; key-symbol.

    (define-key map [up]    'tpu-move-to-beginning)	; up-arrow
    (define-key map [down]  'tpu-move-to-end)		; down-arrow
    (define-key map [right] 'end-of-line)		; right-arrow
    (define-key map [left]  'beginning-of-line)		; left-arrow

    ;; (define-key map [find]   nil)			; Find
    ;; (define-key map [insert] nil)			; Insert Here
    (define-key map [delete] 'tpu-store-text)		; Remove
    (define-key map [select] 'tpu-unselect)		; Select
    (define-key map [prior]  'tpu-previous-window)	; Prev Screen
    (define-key map [next]   'tpu-next-window)		; Next Screen

    ;; (define-key map [f1] nil)			; F1
    ;; (define-key map [f2] nil)			; F2
    ;; (define-key map [f3] nil)			; F3
    ;; (define-key map [f4] nil)			; F4
    ;; (define-key map [f5] nil)			; F5
    ;; (define-key map [f6] nil)			; F6
    ;; (define-key map [f7] nil)			; F7
    ;; (define-key map [f8] nil)			; F8
    ;; (define-key map [f9] nil)			; F9
    ;; (define-key map [f10] nil)			; F10
    ;; (define-key map [f11] nil)			; F11
    ;; (define-key map [f12] nil)			; F12
    ;; (define-key map [f13] nil)			; F13
    ;; (define-key map [f14] nil)			; F14
    (define-key map [help] 'describe-bindings)		; HELP
    ;; (define-key map [menu] nil)			; DO
    (define-key map [f17] 'tpu-drop-breadcrumb)		; F17
    ;; (define-key map [f18] nil)			; F18
    ;; (define-key map [f19] nil)			; F19
    ;; (define-key map [f20] nil)			; F20

    (define-key map [kp-f1] 'keyboard-quit)		; PF1
    (define-key map [kp-f2] 'help-for-help)		; PF2
    (define-key map [kp-f3] 'tpu-search)		; PF3
    (define-key map [kp-f4] 'tpu-undelete-lines)	; PF4
    (define-key map [kp-0] 'open-line)			; KP0
    (define-key map [kp-1] 'tpu-change-case)		; KP1
    (define-key map [kp-2] 'tpu-delete-to-eol)		; KP2
    (define-key map [kp-3] 'tpu-special-insert)		; KP3
    (define-key map [kp-4] 'tpu-move-to-end)		; KP4
    (define-key map [kp-5] 'tpu-move-to-beginning)	; KP5
    (define-key map [kp-6] 'tpu-paste)			; KP6
    (define-key map [kp-7] 'execute-extended-command)	; KP7
    (define-key map [kp-8] 'tpu-fill)			; KP8
    (define-key map [kp-9] 'tpu-replace)		; KP9
    (define-key map [kp-subtract] 'tpu-undelete-words)	; KP-
    (define-key map [kp-separator] 'tpu-undelete-char)	; KP,
    (define-key map [kp-decimal] 'tpu-unselect)		; KP.
    (define-key map [kp-enter] 'tpu-substitute)		; KPenter

    ;;
    (define-key map "\C-A" 'tpu-toggle-overwrite-mode)	; ^A
    ;; (define-key map "\C-B" nil)			; ^B
    ;; (define-key map "\C-C" nil)			; ^C
    ;; (define-key map "\C-D" nil)			; ^D
    ;; (define-key map "\C-E" nil)			; ^E
    (define-key map "\C-F" 'set-visited-file-name)	; ^F
    (define-key map "\C-g" 'keyboard-quit)		; safety first
    (define-key map "\C-h" 'delete-other-windows)	; BS
    (define-key map "\C-i" 'other-window)		; TAB
    ;; (define-key map "\C-J" nil)			; ^J
    (define-key map "\C-K" 'tpu-define-macro-key)	; ^K
    (define-key map "\C-l" 'downcase-region)		; ^L
    ;; (define-key map "\C-M" nil)			; ^M
    ;; (define-key map "\C-N" nil)			; ^N
    ;; (define-key map "\C-O" nil)			; ^O
    ;; (define-key map "\C-P" nil)			; ^P
    ;; (define-key map "\C-Q" nil)			; ^Q
    ;; (define-key map "\C-R" nil)			; ^R
    ;; (define-key map "\C-S" nil)			; ^S
    (define-key map "\C-T" 'tpu-toggle-control-keys)	; ^T
    (define-key map "\C-u" 'upcase-region)		; ^U
    ;; (define-key map "\C-V" nil)			; ^V
    (define-key map "\C-w" 'tpu-write-current-buffers)	; ^W
    ;; (define-key map "\C-X" nil)			; ^X
    ;; (define-key map "\C-Y" nil)			; ^Y
    ;; (define-key map "\C-Z" nil)			; ^Z
    (define-key map " " 'undo)				; SPC
    ;; (define-key map "!" nil)				; !
    ;; (define-key map "#" nil)				; #
    (define-key map "$" 'tpu-add-at-eol)		; $
    (define-key map "%" 'tpu-goto-percent)		; %
    ;; (define-key map "&" nil)				; &
    ;; (define-key map "(" nil)				; (
    ;; (define-key map ")" nil)				; )
    (define-key map "*" 'tpu-toggle-regexp)		; *
    ;; (define-key map "+" nil)				; +
    (define-key map "," 'tpu-goto-breadcrumb)		; ,
    (define-key map "-" 'negative-argument)		; -
    (define-key map "." 'tpu-drop-breadcrumb)		; .
    (define-key map "/" 'tpu-emacs-replace)		; /
    (define-key map "0" 'digit-argument)		; 0
    (define-key map "1" 'digit-argument)		; 1
    (define-key map "2" 'digit-argument)		; 2
    (define-key map "3" 'digit-argument)		; 3
    (define-key map "4" 'digit-argument)		; 4
    (define-key map "5" 'digit-argument)		; 5
    (define-key map "6" 'digit-argument)		; 6
    (define-key map "7" 'digit-argument)		; 7
    (define-key map "8" 'digit-argument)		; 8
    (define-key map "9" 'digit-argument)		; 9
    ;; (define-key map ":" nil)				; :
    (define-key map ";" 'tpu-trim-line-ends)		; ;
    ;; (define-key map "<" nil)				; <
    ;; (define-key map "=" nil)				; =
    ;; (define-key map ">" nil)				; >
    (define-key map "?" 'tpu-spell-check)		; ?
    ;; (define-key map "A" 'tpu-toggle-newline-and-indent) ; A
    ;; (define-key map "B" 'tpu-next-buffer)		; B
    ;; (define-key map "C" 'repeat-complex-command)	; C
    ;; (define-key map "D" 'shell-command)		; D
    ;; (define-key map "E" 'tpu-exit)			; E
    ;; (define-key map "F" 'tpu-cursor-free-mode)	; F
    ;; (define-key map "G" 'tpu-get)			; G
    ;; (define-key map "H" nil)				; H
    ;; (define-key map "I" 'tpu-include)		; I
    ;; (define-key map "K" 'tpu-kill-buffer)		; K
    (define-key map "L" 'tpu-what-line)			; L
    ;; (define-key map "M" 'buffer-menu)		; M
    ;; (define-key map "N" 'tpu-next-file-buffer)	; N
    ;; (define-key map "O" 'occur)			; O
    (define-key map "P" 'lpr-buffer)			; P
    ;; (define-key map "Q" 'tpu-quit)			; Q
    ;; (define-key map "R" 'tpu-toggle-rectangle)	; R
    ;; (define-key map "S" 'replace)			; S
    ;; (define-key map "T" 'tpu-line-to-top-of-window)	; T
    ;; (define-key map "U" 'undo)			; U
    ;; (define-key map "V" 'tpu-version)		; V
    ;; (define-key map "W" 'save-buffer)		; W
    ;; (define-key map "X" 'tpu-save-all-buffers-kill-emacs) ; X
    ;; (define-key map "Y" 'copy-region-as-kill)	; Y
    ;; (define-key map "Z" 'suspend-emacs)		; Z
    (define-key map "[" 'blink-matching-open)		; [
    ;; (define-key map "\\" nil)			; \
    (define-key map "]" 'blink-matching-open)		; ]
    (define-key map "^" 'tpu-add-at-bol)		; ^
    (define-key map "_" 'split-window-below)	        ; -
    (define-key map "`" 'what-line)			; `
    (define-key map "a" 'tpu-toggle-newline-and-indent)	; a
    (define-key map "b" 'tpu-next-buffer)		; b
    (define-key map "c" 'repeat-complex-command)	; c
    (define-key map "d" 'shell-command)			; d
    (define-key map "e" 'tpu-exit)			; e
    (define-key map "f" 'tpu-cursor-free-mode)		; f
    (define-key map "g" 'tpu-get)			; g
    ;; (define-key map "h" nil)				; h
    (define-key map "i" 'tpu-include)			; i
    (define-key map "k" 'tpu-kill-buffer)		; k
    (define-key map "l" 'goto-line)			; l
    (define-key map "m" 'buffer-menu)			; m
    (define-key map "n" 'tpu-next-file-buffer)		; n
    (define-key map "o" 'occur)				; o
    (define-key map "p" 'lpr-region)			; p
    (define-key map "q" 'tpu-quit)			; q
    (define-key map "r" 'tpu-toggle-rectangle)		; r
    (define-key map "s" 'replace)			; s
    (define-key map "t" 'tpu-line-to-top-of-window)	; t
    (define-key map "u" 'undo)				; u
    (define-key map "v" 'tpu-version)			; v
    (define-key map "w" 'save-buffer)			; w
    (define-key map "x" 'tpu-save-all-buffers-kill-emacs) ; x
    (define-key map "y" 'copy-region-as-kill)		; y
    (define-key map "z" 'suspend-emacs)			; z
    ;; (define-key map "{" nil)				; {
    (define-key map "|" 'split-window-right)	        ; |
    ;; (define-key map "}" nil)				; }
    (define-key map "~" 'exchange-point-and-mark)	; ~
    (define-key map "\177" 'delete-window)		; <X]
    map)
  "Maps the function keys on the VT100 keyboard preceded by PF1.
GOLD is the ASCII 7-bit escape sequence <ESC>OP.")
(define-obsolete-variable-alias 'GOLD-map 'tpu-gold-map "23.1")

(defvar tpu-global-map
  (let ((map (make-sparse-keymap)))

    ;; Previously defined in CSI-map.  We now presume that term/*.el does
    ;; its job to map the escape sequence to the right key-symbol.
    (define-key map [find]   'tpu-search)		; Find
    (define-key map [insert] 'tpu-paste)		; Insert Here
    (define-key map [delete] 'tpu-cut)			; Remove
    (define-key map [select] 'tpu-select)		; Select
    (define-key map [prior]  'tpu-scroll-window-down)	; Prev Screen
    (define-key map [next]   'tpu-scroll-window-up)	; Next Screen

    ;; (define-key map [f1] nil)			; F1
    ;; (define-key map [f2] nil)			; F2
    ;; (define-key map [f3] nil)			; F3
    ;; (define-key map [f4] nil)			; F4
    ;; (define-key map [f5] nil)			; F5
    ;; (define-key map [f6] nil)			; F6
    ;; (define-key map [f7] nil)			; F7
    ;; (define-key map [f8] nil)			; F8
    ;; (define-key map [f9] nil)			; F9
    (define-key map [f10] 'tpu-exit)			; F10
    (define-key map [f11] 'tpu-insert-escape)		; F11 (ESC)
    (define-key map [f12] 'tpu-next-beginning-of-line)	; F12 (BS)
    (define-key map [f13] 'tpu-delete-previous-word)	; F13 (LF)
    (define-key map [f14] 'tpu-toggle-overwrite-mode)	; F14
    (define-key map [help] 'tpu-help)			; HELP
    (define-key map [menu] 'execute-extended-command)	; DO
    (define-key map [f17] 'tpu-goto-breadcrumb)		; F17
    ;; (define-key map [f18] nil)			; F18
    ;; (define-key map [f19] nil)			; F19
    ;; (define-key map [f20] nil)			; F20


    ;; Previously defined in SS3-map.  We now presume that term/*.el does
    ;; its job to map the escape sequence to the right key-symbol.
    (define-key map [kp-f1] tpu-gold-map)		; GOLD map
    ;;
    (define-key map [up]    'tpu-previous-line)		; up
    (define-key map [down]  'tpu-next-line)		; down
    (define-key map [right] 'tpu-forward-char)		; right
    (define-key map [left]  'tpu-backward-char)		; left

    (define-key map [kp-f2] 'tpu-help)			; PF2
    (define-key map [kp-f3] 'tpu-search-again)		; PF3
    (define-key map [kp-f4] 'tpu-delete-current-line)	; PF4
    (define-key map [kp-0] 'tpu-line)			; KP0
    (define-key map [kp-1] 'tpu-word)			; KP1
    (define-key map [kp-2] 'tpu-end-of-line)		; KP2
    (define-key map [kp-3] 'tpu-char)			; KP3
    (define-key map [kp-4] 'tpu-advance-direction)	; KP4
    (define-key map [kp-5] 'tpu-backup-direction)	; KP5
    (define-key map [kp-6] 'tpu-cut)			; KP6
    (define-key map [kp-7] 'tpu-page)			; KP7
    (define-key map [kp-8] 'tpu-scroll-window)		; KP8
    (define-key map [kp-9] 'tpu-append-region)		; KP9
    (define-key map [kp-subtract] 'tpu-delete-current-word) ; KP-
    (define-key map [kp-separator] 'tpu-delete-current-char) ; KP,
    (define-key map [kp-decimal] 'tpu-select)		; KP.
    (define-key map [kp-enter] 'newline)		; KPenter

    map)
  "TPU-edt global keymap.")


;;;
;;;  Global Variables
;;;
(defvar tpu-last-replaced-text ""
  "Last text deleted by a TPU-edt replace command.")
(defvar tpu-last-deleted-region ""
  "Last text deleted by a TPU-edt remove command.")
(defvar tpu-last-deleted-lines ""
  "Last text deleted by a TPU-edt line-delete command.")
(defvar tpu-last-deleted-words ""
  "Last text deleted by a TPU-edt word-delete command.")
(defvar tpu-last-deleted-char ""
  "Last character deleted by a TPU-edt character-delete command.")

(defvar tpu-searching-forward t
  "If non-nil, TPU-edt is searching in the forward direction.")
(defvar tpu-search-last-string ""
  "Last text searched for by the TPU-edt search commands.")
(defvar tpu-search-overlay (make-overlay 1 1)
  "Search highlight overlay.")
(overlay-put tpu-search-overlay 'face 'bold)

(defvar tpu-replace-overlay (make-overlay 1 1)
  "Replace highlight overlay.")
(overlay-put tpu-replace-overlay 'face 'highlight)

(defvar tpu-regexp-p nil
  "If non-nil, TPU-edt uses regexp search and replace routines.")
(defvar tpu-rectangular-p nil
  "If non-nil, TPU-edt removes and inserts rectangles.")
(defvar tpu-advance t
  "True when TPU-edt is operating in the forward direction.")
(defvar tpu-reverse nil
  "True when TPU-edt is operating in the backward direction.")
(defvar tpu-control-keys nil
  "If non-nil, control keys are set to perform TPU functions.")
(defvar tpu-xkeys-file nil
  "File containing TPU-edt X key map.")

(defvar tpu-rectangle-string nil
  "Mode line string to identify rectangular mode.")
(defvar tpu-direction-string nil
  "Mode line string to identify current direction.")

(defvar tpu-add-at-bol-hist nil
  "History variable for tpu-edt-add-at-bol function.")
(defvar tpu-add-at-eol-hist nil
  "History variable for tpu-edt-add-at-eol function.")
(defvar tpu-regexp-prompt-hist  nil
  "History variable for search and replace functions.")


;;;
;;;  Buffer Local Variables
;;;
(defvar tpu-newline-and-indent-p nil
  "If non-nil, Return produces a newline and indents.")
(make-variable-buffer-local 'tpu-newline-and-indent-p)

(defvar tpu-newline-and-indent-string nil
  "Mode line string to identify AutoIndent mode.")
(make-variable-buffer-local 'tpu-newline-and-indent-string)

(defvar tpu-saved-delete-func nil
  "Saved value of the delete key.")
(make-variable-buffer-local 'tpu-saved-delete-func)

(defvar tpu-buffer-local-map nil
  "TPU-edt buffer local key map.")
(make-variable-buffer-local 'tpu-buffer-local-map)


;;;
;;;  Mode Line - Modify the mode line to show the following
;;;
;;;     o  Mark state.
;;;     o  Direction of motion.
;;;     o  Active rectangle mode.
;;;     o  Active auto indent mode.
;;;
(defvar tpu-original-mm-alist minor-mode-alist)

(defvar tpu-mark-flag "")
(make-variable-buffer-local 'tpu-mark-flag)

(defun tpu-set-mode-line (for-tpu)
  "Set ``minor-mode-alist'' for TPU-edt, or reset it to default Emacs."
  (let ((entries '((tpu-newline-and-indent-p tpu-newline-and-indent-string)
                   (tpu-rectangular-p tpu-rectangle-string)
                   (tpu-direction-string tpu-direction-string)
                   (tpu-mark-flag tpu-mark-flag))))
    (dolist (entry entries)
      (if for-tpu
          (add-to-list 'minor-mode-alist entry)
        (setq minor-mode-alist (remove entry minor-mode-alist))))))

(defun tpu-update-mode-line nil
  "Make sure mode-line in the current buffer reflects all changes."
  (setq tpu-mark-flag (if transient-mark-mode "" (if (tpu-mark) " @" "  ")))
  (force-mode-line-update))

(cond ((featurep 'xemacs)
       (add-hook 'zmacs-deactivate-region-hook 'tpu-update-mode-line)
       (add-hook 'zmacs-activate-region-hook 'tpu-update-mode-line))
      (t
       (add-hook 'activate-mark-hook 'tpu-update-mode-line)
       (add-hook 'deactivate-mark-hook 'tpu-update-mode-line)))


;;;
;;;  Match Markers -
;;;
;;;     Set in:  Search
;;;
;;;     Used in: Replace, Substitute, Store-Text, Cut/Remove,
;;;              Append, and Change-Case
;;;
(defvar tpu-match-beginning-mark (make-marker))
(defvar tpu-match-end-mark (make-marker))

(defun tpu-set-match nil
  "Set markers at match beginning and end."
  ;; Add one to beginning mark so it stays with the first character of
  ;;   the string even if characters are added just before the string.
  (setq tpu-match-beginning-mark (copy-marker (match-beginning 0) t))
  (setq tpu-match-end-mark (copy-marker (match-end 0))))

(defun tpu-unset-match nil
  "Unset match beginning and end markers."
  (set-marker tpu-match-beginning-mark nil)
  (set-marker tpu-match-end-mark nil))

(defun tpu-match-beginning nil
  "Return the location of the last match beginning."
  (marker-position tpu-match-beginning-mark))

(defun tpu-match-end nil
  "Return the location of the last match end."
  (marker-position tpu-match-end-mark))

(defun tpu-check-match nil
  "Return t if point is between tpu-match markers.
Otherwise sets the tpu-match markers to nil and returns nil."
  ;; make sure 1- marker is in this buffer
  ;;           2- point is at or after beginning marker
  ;;           3- point is before ending marker, or in the case of
  ;;              zero length regions (like bol, or eol) that the
  ;;              beginning, end, and point are equal.
  (cond ((and
	  (equal (marker-buffer tpu-match-beginning-mark) (current-buffer))
	  (>= (point) (marker-position tpu-match-beginning-mark))
	  (or
	   (< (point) (marker-position tpu-match-end-mark))
	   (and (= (marker-position tpu-match-beginning-mark)
		   (marker-position tpu-match-end-mark))
		(= (marker-position tpu-match-end-mark) (point))))) t)
	(t
	 (tpu-unset-match) nil)))

(defun tpu-show-match-markers nil
  "Show the values of the match markers."
  (interactive)
  (if (markerp tpu-match-beginning-mark)
      (message "(%s, %s) in %s -- current %s in %s"
               (marker-position tpu-match-beginning-mark)
               (marker-position tpu-match-end-mark)
               (marker-buffer tpu-match-end-mark)
               (point) (current-buffer))))


;;;
;;;  Utilities
;;;

(defun tpu-mark nil
  "TPU-edt version of the mark function.
Return the appropriate value of the mark for the current
version of Emacs."
  (cond ((featurep 'xemacs) (mark (not zmacs-regions)))
	(t (and mark-active (mark (not transient-mark-mode))))))

(defun tpu-set-mark (pos)
  "TPU-edt version of the `set-mark' function.
Sets the mark at POS and activates the region according to the
current version of Emacs."
  (set-mark pos)
  (when (featurep 'xemacs) (when pos (zmacs-activate-region))))

(defun tpu-string-prompt (prompt history-symbol)
  "Read a string with PROMPT."
  (read-from-minibuffer prompt nil nil nil history-symbol))

(defvar tpu-last-answer nil "Most recent response to tpu-y-or-n-p.")

(defun tpu-y-or-n-p (prompt &optional not-yes)
  "Prompt for a y or n answer with positive default.
Optional second argument NOT-YES changes default to negative.
Like Emacs `y-or-n-p', but also accepts space as y and DEL as n."
  (message "%s[%s]" prompt (if not-yes "n" "y"))
  (let ((doit t))
    (while doit
      (setq doit nil)
      (let ((ans (read-char)))
	(cond ((or (= ans ?y) (= ans ?Y) (= ans ?\ ))
	       (setq tpu-last-answer t))
	      ((or (= ans ?n) (= ans ?N) (= ans ?\C-?))
	       (setq tpu-last-answer nil))
	      ((= ans ?\r) (setq tpu-last-answer (not not-yes)))
	      (t
	       (setq doit t) (beep)
	       (message "Please answer y or n.  %s[%s]"
			prompt (if not-yes "n" "y")))))))
  tpu-last-answer)

(defun tpu-local-set-key (key func)
  "Replace a key in the TPU-edt local key map.
Create the key map if necessary."
  (cond ((not (keymapp tpu-buffer-local-map))
	 (setq tpu-buffer-local-map (if (current-local-map)
					(copy-keymap (current-local-map))
				      (make-sparse-keymap)))
	 (use-local-map tpu-buffer-local-map)))
  (local-set-key key func))

(defun tpu-current-line ()
  "Return the vertical position of point in the selected window.
Top line is 0.  Counts each text line only once, even if it wraps."
  (or
   (cdr (nth 6 (posn-at-point)))
   (if (eq (window-start) (point)) 0
     (1- (count-screen-lines (window-start) (point) 'count-final-newline)))))


;;;
;;;  Breadcrumbs
;;;
(defvar tpu-breadcrumb-plist nil
  "The set of user-defined markers (breadcrumbs), as a plist.")

(defun tpu-drop-breadcrumb (num)
  "Drops a breadcrumb that can be returned to later with goto-breadcrumb."
  (interactive "p")
  (put tpu-breadcrumb-plist num (list (current-buffer) (point)))
  (message "Mark %d set." num))

(defun tpu-goto-breadcrumb (num)
  "Return to a breadcrumb set with drop-breadcrumb."
  (interactive "p")
  (cond ((get tpu-breadcrumb-plist num)
	 (switch-to-buffer (car (get tpu-breadcrumb-plist num)))
	 (goto-char (cadr (get tpu-breadcrumb-plist num)))
	 (message "mark %d found." num))
	(t
	 (message "mark %d not found." num))))


;;;
;;;  Miscellaneous
;;;
(defun tpu-change-case (num)
  "Change the case of the character under the cursor or region.
Accepts a prefix argument of the number of characters to invert."
  (interactive "p")
  (cond ((tpu-mark)
	 (let ((beg (region-beginning)) (end (region-end)))
	   (while (> end beg)
	     (funcall (if (= (downcase (char-after beg)) (char-after beg))
			  'upcase-region 'downcase-region)
		      beg (1+ beg))
	     (setq beg (1+ beg)))
	   (tpu-unselect t)))
	((tpu-check-match)
	 (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
	   (while (> end beg)
	     (funcall (if (= (downcase (char-after beg)) (char-after beg))
			  'upcase-region 'downcase-region)
		      beg (1+ beg))
	     (setq beg (1+ beg)))
	   (tpu-unset-match)))
	(t
	 (while (> num 0)
	   (funcall (if (= (downcase (following-char)) (following-char))
			'upcase-region 'downcase-region)
		    (point) (1+ (point)))
	   (forward-char (if tpu-reverse -1 1))
	   (setq num (1- num))))))

(defun tpu-fill (num)
  "Fill paragraph or marked region.
With argument, fill and justify."
  (interactive "P")
  (cond ((tpu-mark)
	 (fill-region (point) (tpu-mark) num)
	 (tpu-unselect t))
	(t
	 (fill-paragraph num))))

(defun tpu-version nil
  "Print the TPU-edt version number."
  (interactive)
  (message
   "TPU-edt version %s by Rob Riepel (riepel@networking.stanford.edu)"
   tpu-version))

(defun tpu-reset-screen-size (height width)
  "Set the screen size."
  (interactive "nnew screen height: \nnnew screen width: ")
  (set-frame-height (selected-frame) height)
  (set-frame-width (selected-frame) width))

(defun tpu-toggle-newline-and-indent nil
  "Toggle between 'newline and indent' and 'simple newline'."
  (interactive)
  (cond (tpu-newline-and-indent-p
         (setq tpu-newline-and-indent-string "")
         (setq tpu-newline-and-indent-p nil)
         (tpu-local-set-key "\C-m" 'newline))
        (t
         (setq tpu-newline-and-indent-string " AutoIndent")
         (setq tpu-newline-and-indent-p t)
         (tpu-local-set-key "\C-m" 'newline-and-indent)))
  (tpu-update-mode-line)
  (and (called-interactively-p 'interactive)
       (message "Carriage return inserts a newline%s"
		(if tpu-newline-and-indent-p " and indents." "."))))

(defun tpu-spell-check nil
  "Check the spelling of the region, or of the entire buffer,
if no region is selected."
  (interactive)
  (let ((m (tpu-mark)))
    (apply (if tpu-have-ispell 'ispell-region
             'spell-region)
           (if m
               (if (> m (point)) (list (point) m)
                 (list m (point)))
             (list (point-min) (point-max))))
    (if m (tpu-unselect t))))

(defun tpu-toggle-overwrite-mode nil
  "Switch in and out of overwrite mode."
  (interactive)
  (cond (overwrite-mode
	 (tpu-local-set-key "\177" tpu-saved-delete-func)
	 (overwrite-mode 0))
	(t
	 (setq tpu-saved-delete-func (local-key-binding "\177"))
	 (tpu-local-set-key "\177" 'picture-backward-clear-column)
	 (overwrite-mode 1))))

(defun tpu-special-insert (num)
  "Insert a character or control code according to its ASCII decimal value."
  (interactive "P")
  (if overwrite-mode (delete-char 1))
  (insert (or num 0)))

(defun tpu-quoted-insert (num)
  "Read next input character and insert it.
This is useful for inserting control characters."
  (interactive "*p")
  (let ((char (read-char)) )
    (if overwrite-mode (delete-char num))
    (insert-char char num)))


;;;
;;;  TPU line-mode commands
;;;
(defun tpu-include (file)
  "TPU-like include file."
  (interactive "fInclude file: ")
  (insert-file-contents file)
  (message ""))

(defun tpu-get (file)
  "TPU-like get file."
  (interactive "FFile to get: ")
  (find-file file find-file-wildcards))

(defun tpu-what-line nil
  "Tell what line the point is on,
and the total number of lines in the buffer."
  (interactive)
  (if (eobp)
      (message "You are at the End of Buffer.  The last line is %d."
	       (count-lines 1 (point-max)))
    (let* ((cur (count-lines 1 (1+ (point))))
	   (max (count-lines 1 (point-max)))
	   (pct (/ (* 100 (+ cur (/ max 200))) max)))
      (message "You are on line %d out of %d (%d%%)." cur max pct))))

(defun tpu-exit nil
  "Exit the way TPU does, save current buffer and ask about others."
  (interactive)
  (if (not (eq (recursion-depth) 0))
      (exit-recursive-edit)
    (progn (save-buffer) (save-buffers-kill-emacs))))

(defun tpu-quit nil
  "Quit the way TPU does, ask to make sure changes should be abandoned."
  (interactive)
  (let ((list (buffer-list))
	(working t))
    (while (and list working)
      (let ((buffer (car list)))
	(if (and (buffer-file-name buffer) (buffer-modified-p buffer))
            (if (tpu-y-or-n-p
		 "Modifications will not be saved, continue quitting? ")
		(kill-emacs t) (setq working nil)))
	(setq list (cdr list))))
    (if working (kill-emacs t))))


;;;
;;;  Command and Function Aliases
;;;
;;;###autoload
(define-minor-mode tpu-edt-mode
  "Toggle TPU/edt emulation on or off.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :global t
  (if tpu-edt-mode (tpu-edt-on) (tpu-edt-off)))

(defalias 'TPU-EDT-MODE 'tpu-edt-mode)

;;;###autoload
(defalias 'tpu-edt 'tpu-edt-on)
(defalias 'TPU-EDT 'tpu-edt-on)

;; Note:  The following functions have no `tpu-' prefix.  This is unavoidable.
;;        The real TPU/edt editor has interactive commands with these names,
;;        so tpu-edt.el users expect things like M-x exit RET and M-x help RET
;;        to work.  Therefore it really is necessary to define these functions,
;;        even in cases where they redefine existing Emacs functions.

(defalias 'exit 'tpu-exit)
(defalias 'EXIT 'tpu-exit)

(defalias 'Get 'tpu-get)
(defalias 'GET 'tpu-get)

(defalias 'include 'tpu-include)
(defalias 'INCLUDE 'tpu-include)

(defalias 'quit 'tpu-quit)
(defalias 'QUIT 'tpu-quit)

(defalias 'spell 'tpu-spell-check)
(defalias 'SPELL 'tpu-spell-check)

(defalias 'what\ line 'tpu-what-line)
(defalias 'WHAT\ LINE 'tpu-what-line)

(defalias 'replace 'tpu-lm-replace)
(defalias 'REPLACE 'tpu-lm-replace)

(defalias 'help 'tpu-help)
(defalias 'HELP 'tpu-help)

(defalias 'set\ cursor\ free 'tpu-set-cursor-free)
(defalias 'SET\ CURSOR\ FREE 'tpu-set-cursor-free)

(defalias 'set\ cursor\ bound 'tpu-set-cursor-bound)
(defalias 'SET\ CURSOR\ BOUND 'tpu-set-cursor-bound)

(defalias 'set\ scroll\ margins 'tpu-set-scroll-margins)
(defalias 'SET\ SCROLL\ MARGINS 'tpu-set-scroll-margins)

;; Real TPU error messages end in periods.
;; Define this to avoid openly flouting Emacs coding standards.
(defalias 'tpu-error 'error)


;;;
;;;  Help
;;;
(defvar tpu-help-keypad-map "\f
          _______________________    _______________________________
         | HELP  |      Do       |  |       |       |       |       |
         |KeyDefs|               |  |       |       |       |       |
         |_______|_______________|  |_______|_______|_______|_______|
          _______________________    _______________________________
         | Find  |Insert |Remove |  | Gold  | HELP  |FndNxt | Del L |
         |       |       |Sto Tex|  |  key  |E-Help | Find  |Undel L|
         |_______|_______|_______|  |_______|_______|_______|_______|
         |Select |Pre Scr|Nex Scr|  | Page  | Sect  |Append | Del W |
         | Reset |Pre Win|Nex Win|  |  Do   | Fill  |Replace|Undel W|
         |_______|_______|_______|  |_______|_______|_______|_______|
                 |Move up|          |Forward|Reverse|Remove | Del C |
                 |  Top  |          |Bottom |  Top  |Insert |Undel C|
          _______|_______|_______   |_______|_______|_______|_______|
         |Mov Lef|Mov Dow|Mov Rig|  | Word  |  EOL  | Char  |       |
         |StaOfLi|Bottom |EndOfLi|  |ChngCas|Del EOL|SpecIns| Enter |
         |_______|_______|_______|  |_______|_______|_______|       |
                                    |     Line      |Select | Subs  |
                                    |   Open Line   | Reset |       |
                                    |_______________|_______|_______|
")

(defvar tpu-help-text "
\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\f

      Control Characters

      ^A  toggle insert and overwrite
      ^B  recall
      ^E  end of line

      ^G  Cancel current operation
      ^H  beginning of line
      ^J  delete previous word

      ^K  learn
      ^L  insert page break
      ^R  remember (during learn), re-center

      ^U  delete to beginning of line
      ^V  quote
      ^W  refresh

      ^Z  exit
    ^X^X  exchange point and mark - useful for checking region boundaries

\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\f
       Gold-<key> Functions

       B     Next Buffer - display the next buffer (all buffers)
       C     Recall - edit and possibly repeat previous commands
       E     Exit - save current buffer and ask about others
       G     Get - load a file into a new edit buffer

       I     Include - include a file in this buffer
       K     Kill Buffer - abandon edits and delete buffer
       M     Buffer Menu - display a list of all buffers
       N     Next File Buffer - display next buffer containing a file

       O     Occur - show following lines containing REGEXP
       Q     Quit - exit without saving anything
       R     Toggle rectangular mode for remove and insert
       S     Search and substitute - line mode REPLACE command

      ^T     Toggle control key bindings between TPU and Emacs
       U     Undo - undo the last edit
       W     Write - save current buffer
       X     Exit - save all modified buffers and exit

\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\f

  More extensive documentation on TPU-edt can be found in the `Commentary'
  section of tpu-edt.el.  This section can be accessed through the standard
  Emacs help facility using the `p' option.  Once you exit TPU-edt Help, one
  of the following key sequences is sure to get you there.

    ^h p        if you're not yet using TPU-edt
    Gold-PF2 p  if you're using TPU-edt

  Alternatively, fire up Emacs help from the command prompt, with

    M-x help-for-help <CR> p <CR>

  Where `M-x' might be any of `Gold-KP7', 'Do', or 'ESC-x'.

  When you successfully invoke this part of the Emacs help facility, you
  will see a buffer named `*Finder*' listing a number of topics.  Look for
  tpu-edt under `emulations'.

\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\f

   *** No more help, use P to view previous screen")

(defvar tpu-help-enter (format "%s" "\eOM"))    ; tpu-help enter key symbol
(defvar tpu-help-return (format "%s" "\r"))     ; tpu-help enter key symbol
(defvar tpu-help-N "N")                         ; tpu-help "N" symbol
(defvar tpu-help-n "n")                         ; tpu-help "n" symbol
(defvar tpu-help-P "P")                         ; tpu-help "P" symbol
(defvar tpu-help-p "p")                         ; tpu-help "p" symbol

(defun tpu-help nil
  "Display TPU-edt help."
  (interactive)
  ;; Save current window configuration
  (save-window-excursion
    ;; Create and fill help buffer if necessary
    (if (not (get-buffer "*TPU-edt Help*"))
	(progn (generate-new-buffer "*TPU-edt Help*")
	       (switch-to-buffer "*TPU-edt Help*")
	       (insert tpu-help-keypad-map)
	       (insert tpu-help-text)
	       (setq buffer-read-only t)))

    ;; Display the help buffer
    (switch-to-buffer "*TPU-edt Help*")
    (delete-other-windows)
    (tpu-move-to-beginning)
    (forward-line 1)
    (tpu-line-to-top-of-window)

    ;; Prompt for keys to describe, based on screen state (split/not split)
    (let ((key nil) (fkey nil) (split nil))
      (while (not (equal tpu-help-return fkey))
	(if split
	    (setq key
		  (read-key-sequence
		   "Press the key you want help on (RET=exit, ENTER=redisplay, N=next, P=prev): "))
	  (setq key
		(read-key-sequence
		 "Press the key you want help on (RET to exit, N next screen, P prev screen): ")))

	;; Process the read key
	;;
	;;    ENTER   -  Display just the help window
	;;    N or n  -  Next help or describe-key screen
	;;    P or p  -  Previous help or describe-key screen
	;;    RETURN  -  Exit from TPU-help
	;;    default -  describe the key
	;;
	(setq fkey (format "%s" key))
	(cond ((equal tpu-help-enter fkey)
	       (setq split nil)
	       (delete-other-windows))
	      ((or (equal tpu-help-N fkey) (equal tpu-help-n fkey))
	       (cond (split
	              (condition-case nil
	        	  (scroll-other-window 8)
	        	(error nil)))
	             (t
	              (forward-page)
	              (forward-line 1)
	              (tpu-line-to-top-of-window))))
	      ((or (equal tpu-help-P fkey) (equal tpu-help-p fkey))
	       (cond (split
	              (condition-case nil
	        	  (scroll-other-window -8)
	        	(error nil)))
	             (t
	              (forward-line -1)
	              (backward-page)
	              (forward-line 1)
	              (tpu-line-to-top-of-window))))
	      ((not (equal tpu-help-return fkey))
	       (setq split t)
	       (describe-key key)
	       ;; If the key is undefined, leave the
	       ;;   message in the mini-buffer for 3 seconds
	       (if (not (key-binding key)) (sit-for 3))))))))


;;;
;;;  Auto-insert
;;;
(defun tpu-insert-escape nil
  "Insert an escape character, and so becomes the escape-key alias."
  (interactive)
  (insert "\e"))

(defun tpu-insert-formfeed nil
  "Insert a formfeed character."
  (interactive)
  (insert "\C-L"))


;;;
;;;  Define key
;;;
(defvar tpu-saved-control-r nil "Saved value of Control-r.")

(defun tpu-end-define-macro-key (key)
  "End the current macro definition."
  (interactive "kPress the key you want to use to do what was just learned: ")
  (end-kbd-macro nil)
  (global-set-key key last-kbd-macro)
  (global-set-key "\C-r" tpu-saved-control-r))

(defun tpu-define-macro-key nil
  "Bind a set of keystrokes to a single key, or key combination."
  (interactive)
  (setq tpu-saved-control-r (global-key-binding "\C-r"))
  (global-set-key "\C-r" 'tpu-end-define-macro-key)
  (start-kbd-macro nil))


;;;
;;;  Buffers and Windows
;;;
(defun tpu-kill-buffer nil
  "Kill the current buffer.
If `tpu-kill-buffers-silently' is non-nil,
kill modified buffers without asking."
  (interactive)
  (if tpu-kill-buffers-silently (set-buffer-modified-p nil))
  (kill-buffer (current-buffer)))

(defun tpu-save-all-buffers-kill-emacs nil
  "Save all buffers and exit Emacs."
  (interactive)
  (let ((delete-old-versions t))
    (save-buffers-kill-emacs t)))

(defun tpu-write-current-buffers nil
  "Save all modified buffers without exiting."
  (interactive)
  (save-some-buffers t))

(defun tpu-next-buffer nil
  "Go to next buffer in ring."
  (interactive)
  (switch-to-buffer (car (reverse (buffer-list)))))

(defun tpu-next-file-buffer nil
  "Go to next buffer in ring that is visiting a file or directory."
  (interactive)
  (let ((list (tpu-make-file-buffer-list (buffer-list))))
    (setq list (delq (current-buffer) list))
    (if (not list) (tpu-error "No other buffers."))
    (switch-to-buffer (car (reverse list)))))

(defun tpu-make-file-buffer-list (buffer-list)
  "Return names from BUFFER-LIST excluding those beginning with a space or star."
  (delq nil (mapcar (lambda (b)
                      (if (or (= (aref (buffer-name b) 0) ?\s)
                              (= (aref (buffer-name b) 0) ?*)) nil b))
                    buffer-list)))

(defun tpu-next-window nil
  "Move to the next window."
  (interactive)
  (if (one-window-p) (message "There is only one window on screen.")
    (other-window 1)))

(defun tpu-previous-window nil
  "Move to the previous window."
  (interactive)
  (if (one-window-p) (message "There is only one window on screen.")
    (select-window (previous-window))))


;;;
;;;  Search
;;;
(defun tpu-toggle-regexp nil
  "Switch in and out of regular expression search and replace mode."
  (interactive)
  (setq tpu-regexp-p (not tpu-regexp-p))
  (tpu-set-search)
  (and (called-interactively-p 'interactive)
       (message "Regular expression search and substitute %sabled."
		(if tpu-regexp-p "en" "dis"))))

(defun tpu-regexp-prompt (prompt)
  "Read a string, adding 'RE' to the prompt if tpu-regexp-p is set."
  (let ((re-prompt (concat (if tpu-regexp-p "RE ") prompt)))
    (read-from-minibuffer re-prompt nil nil nil 'tpu-regexp-prompt-hist)))

(defun tpu-search-highlight nil
  (if (tpu-check-match)
      (move-overlay tpu-search-overlay
                    (tpu-match-beginning) (tpu-match-end) (current-buffer))
    (unless (equal (overlay-start tpu-search-overlay)
		   (overlay-end tpu-search-overlay))
      (move-overlay tpu-search-overlay 1 1 (current-buffer)))))

(defun tpu-search nil
  "Search for a string or regular expression.
The search is performed in the current direction."
  (interactive)
  (tpu-set-search)
  (tpu-search-internal ""))

(defun tpu-search-forward nil
  "Search for a string or regular expression.
The search is begins in the forward direction."
  (interactive)
  (setq tpu-searching-forward t)
  (tpu-set-search t)
  (tpu-search-internal ""))

(defun tpu-search-reverse nil
  "Search for a string or regular expression.
The search is begins in the reverse direction."
  (interactive)
  (setq tpu-searching-forward nil)
  (tpu-set-search t)
  (tpu-search-internal ""))

(defun tpu-search-again nil
  "Search for the same string or regular expression as last time.
The search is performed in the current direction."
  (interactive)
  (tpu-search-internal tpu-search-last-string))

;;  tpu-set-search defines the search functions used by the TPU-edt internal
;;  search function.  It should be called whenever the direction changes, or
;;  the regular expression mode is turned on or off.  It can also be called
;;  to ensure that the next search will be in the current direction.  It is
;;  called from:

;;       tpu-advance                   tpu-backup
;;       tpu-toggle-regexp             tpu-toggle-search-direction (t)
;;       tpu-search                    tpu-lm-replace
;;       tpu-search-forward (t)        tpu-search-reverse (t)
;;       tpu-search-forward-exit (t)   tpu-search-backward-exit (t)

(declare-function tpu-emacs-search "tpu-edt")
(declare-function tpu-emacs-rev-search "tpu-edt")

(defun tpu-set-search (&optional arg)
  "Set the search functions and set the search direction to the current direction.
If an argument is specified, don't set the search direction."
  (if (not arg) (setq tpu-searching-forward tpu-advance))
  (cond (tpu-searching-forward
	 (cond (tpu-regexp-p
		(fset 'tpu-emacs-search 're-search-forward)
		(fset 'tpu-emacs-rev-search 're-search-backward))
	       (t
		(fset 'tpu-emacs-search 'search-forward)
		(fset 'tpu-emacs-rev-search 'search-backward))))
	(t
	 (cond (tpu-regexp-p
		(fset 'tpu-emacs-search 're-search-backward)
		(fset 'tpu-emacs-rev-search 're-search-forward))
	       (t
		(fset 'tpu-emacs-search 'search-backward)
		(fset 'tpu-emacs-rev-search 'search-forward))))))

(defun tpu-search-internal (pat &optional quiet)
  "Search for a string or regular expression."
  (setq tpu-search-last-string
	(if (not (string= "" pat)) pat (tpu-regexp-prompt "Search: ")))

  (tpu-unset-match)
  (tpu-adjust-search)

  (let ((case-fold-search
	 (and case-fold-search (tpu-check-search-case tpu-search-last-string))))

    (cond ((tpu-emacs-search tpu-search-last-string nil t)
	   (tpu-set-match) (goto-char (tpu-match-beginning)))

	  (t
	   (tpu-adjust-search t)
	   (let ((found nil) (pos nil))
	     (save-excursion
	       (let ((tpu-searching-forward (not tpu-searching-forward)))
		 (tpu-adjust-search)
		 (setq found (tpu-emacs-rev-search tpu-search-last-string nil t))
		 (setq pos (match-beginning 0))))

	     (cond
	      (found
	       (cond ((tpu-y-or-n-p
		       (format "Found in %s direction.  Go there? "
			       (if tpu-searching-forward "reverse" "forward")))
		      (goto-char pos) (tpu-set-match)
		      (tpu-toggle-search-direction))))

	      (t
	       (if (not quiet)
		   (message
		    "%sSearch failed: \"%s\""
		    (if tpu-regexp-p "RE " "") tpu-search-last-string)))))))))

(defalias 'tpu-search-internal-core (symbol-function 'tpu-search-internal))

(defun tpu-check-search-case (string)
  "Return t if string contains upper case."
  ;; if using regexp, eliminate upper case forms (\B \W \S.)
  (if tpu-regexp-p
      (let ((pat (copy-sequence string)) (case-fold-search nil) (pos 0))
	(while (setq pos (string-match "\\\\\\\\" pat)) (aset pat (+ 1 pos) ?.))
	(while (setq pos (string-match "\\\\B" pat)) (aset pat (+ 1 pos) ?.))
	(while (setq pos (string-match "\\\\W" pat)) (aset pat (+ 1 pos) ?.))
	(while (setq pos (string-match "\\\\S." pat))
	  (aset pat (+ 1 pos) ?.) (aset pat (+ 2 pos) ?.))
	(string-equal pat (downcase pat)))
    (string-equal string (downcase string))))

(defun tpu-adjust-search (&optional arg)
  "For forward searches, move forward a character before searching,
and backward a character after a failed search.  Arg means end of search."
  (if tpu-searching-forward
      (cond (arg (if (not (bobp)) (forward-char -1)))
	    (t (if (not (eobp)) (forward-char 1))))))

(defun tpu-toggle-search-direction nil
  "Toggle the TPU-edt search direction.
Used for reversing a search in progress."
  (interactive)
  (setq tpu-searching-forward (not tpu-searching-forward))
  (tpu-set-search t)
  (and (called-interactively-p 'interactive)
       (message "Searching %sward."
		(if tpu-searching-forward "for" "back"))))

(defun tpu-search-forward-exit nil
  "Set search direction forward and exit minibuffer."
  (interactive)
  (setq tpu-searching-forward t)
  (tpu-set-search t)
  (exit-minibuffer))

(defun tpu-search-backward-exit nil
  "Set search direction backward and exit minibuffer."
  (interactive)
  (setq tpu-searching-forward nil)
  (tpu-set-search t)
  (exit-minibuffer))


;;;
;;;  Select / Unselect
;;;
(defun tpu-select (&optional quiet)
  "Set the mark to define one end of a region."
  (interactive "P")
  (cond ((tpu-mark)
	 (tpu-unselect quiet))
	(t
	 (tpu-set-mark (point))
	 (tpu-update-mode-line)
	 (if (not quiet) (message "Move the text cursor to select text.")))))

(defun tpu-unselect (&optional quiet)
  "Remove the mark to unselect the current region."
  (interactive "P")
  (deactivate-mark)
  (setq mark-ring nil)
  (tpu-set-mark nil)
  (tpu-update-mode-line)
  (if (not quiet) (message "Selection canceled.")))


;;;
;;;  Delete / Cut
;;;
(defun tpu-toggle-rectangle nil
  "Toggle rectangular mode for remove and insert."
  (interactive)
  (setq tpu-rectangular-p (not tpu-rectangular-p))
  (setq tpu-rectangle-string (if tpu-rectangular-p " Rect" ""))
  (tpu-update-mode-line)
  (and (called-interactively-p 'interactive)
       (message "Rectangular cut and paste %sabled."
		(if tpu-rectangular-p "en" "dis"))))

(defun tpu-arrange-rectangle nil
  "Adjust point and mark to upper left and lower right corners of a rectangle."
  (let ((mc (current-column))
	(pc (progn (exchange-point-and-mark) (current-column))))

    (cond ((> (point) (tpu-mark))                  ; point on lower line
	   (cond ((> pc mc)                        ; point @  lower-right
		  (exchange-point-and-mark))       ; point -> upper-left

		 (t	                           ; point @  lower-left
		  (move-to-column mc t)            ; point -> lower-right
		  (exchange-point-and-mark)        ; point -> upper-right
		  (move-to-column pc t))))         ; point -> upper-left

	  (t                                       ; point on upper line
	   (cond ((> pc mc)                        ; point @  upper-right
		  (move-to-column mc t)            ; point -> upper-left
		  (exchange-point-and-mark)        ; point -> lower-left
		  (move-to-column pc t)            ; point -> lower-right
		  (exchange-point-and-mark)))))))  ; point -> upper-left

(defun tpu-cut-text nil
  "Delete the selected region.
The text is saved for the tpu-paste command."
  (interactive)
  (cond ((tpu-mark)
	 (cond (tpu-rectangular-p
		(tpu-arrange-rectangle)
		(picture-clear-rectangle (point) (tpu-mark) (not overwrite-mode))
		(tpu-unselect t))
	       (t
		(setq tpu-last-deleted-region
		      (buffer-substring (tpu-mark) (point)))
		(delete-region (tpu-mark) (point))
		(tpu-unselect t))))
	((tpu-check-match)
	 (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
	   (setq tpu-last-deleted-region (buffer-substring beg end))
	   (delete-region beg end)
	   (tpu-unset-match)))
	(t
	 (tpu-error "No selection active."))))

(defun tpu-store-text nil
  "Copy the selected region to the cut buffer without deleting it.
The text is saved for the tpu-paste command."
  (interactive)
  (cond ((tpu-mark)
	 (cond (tpu-rectangular-p
		(save-excursion
		  (tpu-arrange-rectangle)
		  (setq picture-killed-rectangle
			(extract-rectangle (point) (tpu-mark))))
		(tpu-unselect t))
	       (t
		(setq tpu-last-deleted-region
		      (buffer-substring (tpu-mark) (point)))
		(tpu-unselect t))))
	((tpu-check-match)
	 (setq tpu-last-deleted-region
	       (buffer-substring (tpu-match-beginning) (tpu-match-end)))
	 (tpu-unset-match))
	(t
	 (tpu-error "No selection active."))))

(defun tpu-cut (arg)
  "Copy selected region to the cut buffer.
In the absence of an argument, delete the selected region too."
  (interactive "P")
  (if arg (tpu-store-text) (tpu-cut-text)))

(defun tpu-append-region (arg)
  "Append selected region to the tpu-cut buffer.
In the absence of an argument, delete the selected region too."
  (interactive "P")
  (cond ((tpu-mark)
	 (let ((beg (region-beginning)) (end (region-end)))
	   (setq tpu-last-deleted-region
		 (concat tpu-last-deleted-region
			 (buffer-substring beg end)))
	   (if (not arg) (delete-region beg end))
	   (tpu-unselect t)))
	((tpu-check-match)
	 (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
	   (setq tpu-last-deleted-region
		 (concat tpu-last-deleted-region
			 (buffer-substring beg end)))
	   (if (not arg) (delete-region beg end))
	   (tpu-unset-match)))
	(t
	 (tpu-error "No selection active."))))

(defun tpu-delete-current-line (num)
  "Delete one or specified number of lines after point.
This includes the newline character at the end of each line.
They are saved for the TPU-edt undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-line num)
    (if (not (eq (preceding-char) ?\n))
        (insert "\n"))
    (setq tpu-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun tpu-delete-to-eol (num)
  "Delete text up to end of line.
With argument, delete up to the Nth line-end past point.
They are saved for the TPU-edt undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-char 1)
    (end-of-line num)
    (setq tpu-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun tpu-delete-to-bol (num)
  "Delete text back to beginning of line.
With argument, delete up to the Nth line-end past point.
They are saved for the TPU-edt undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (tpu-next-beginning-of-line num)
    (setq tpu-last-deleted-lines
          (buffer-substring (point) beg))
    (delete-region (point) beg)))

(defun tpu-delete-current-word (num)
  "Delete one or specified number of words after point.
They are saved for the TPU-edt undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (tpu-forward-to-word num)
    (setq tpu-last-deleted-words
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun tpu-delete-previous-word (num)
  "Delete one or specified number of words before point.
They are saved for the TPU-edt undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (tpu-backward-to-word num)
    (setq tpu-last-deleted-words
          (buffer-substring (point) beg))
    (delete-region beg (point))))

(defun tpu-delete-current-char (num)
  "Delete one or specified number of characters after point.
The last character deleted is saved for the TPU-edt undelete-char command."
  (interactive "p")
  (while (and (> num 0) (not (eobp)))
    (setq tpu-last-deleted-char (char-after (point)))
    (cond (overwrite-mode
	   (picture-clear-column 1)
	   (forward-char 1))
	  (t
	   (delete-char 1)))
    (setq num (1- num))))


;;;
;;;  Undelete / Paste
;;;
(defun tpu-paste (num)
  "Insert the last region or rectangle of killed text.
With argument reinserts the text that many times."
  (interactive "p")
  (while (> num 0)
    (cond (tpu-rectangular-p
	   (let ((beg (point)))
	     (save-excursion
	       (picture-yank-rectangle (not overwrite-mode))
	       (message ""))
	     (goto-char beg)))
	  (t
	   (insert tpu-last-deleted-region)))
    (setq num (1- num))))

(defun tpu-undelete-lines (num)
  "Insert lines deleted by last TPU-edt line-deletion command.
With argument reinserts lines that many times."
  (interactive "p")
  (let ((beg (point)))
    (while (> num 0)
      (insert tpu-last-deleted-lines)
      (setq num (1- num)))
    (goto-char beg)))

(defun tpu-undelete-words (num)
  "Insert words deleted by last TPU-edt word-deletion command.
With argument reinserts words that many times."
  (interactive "p")
  (let ((beg (point)))
    (while (> num 0)
      (insert tpu-last-deleted-words)
      (setq num (1- num)))
    (goto-char beg)))

(defun tpu-undelete-char (num)
  "Insert character deleted by last TPU-edt character-deletion command.
With argument reinserts the character that many times."
  (interactive "p")
  (while (> num 0)
    (if overwrite-mode (prog1 (forward-char -1) (delete-char 1)))
    (insert tpu-last-deleted-char)
    (forward-char -1)
    (setq num (1- num))))


;;;
;;;  Replace and Substitute
;;;
(defun tpu-replace nil
  "Replace the selected region with the contents of the cut buffer."
  (interactive)
  (cond ((tpu-mark)
	 (let ((beg (region-beginning)) (end (region-end)))
	   (setq tpu-last-replaced-text (buffer-substring beg end))
	   (delete-region beg end)
	   (insert tpu-last-deleted-region)
	   (tpu-unselect t)))
	((tpu-check-match)
	 (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
	   (setq tpu-last-replaced-text (buffer-substring beg end))
	   (replace-match tpu-last-deleted-region
			  (not case-replace) (not tpu-regexp-p))
	   (tpu-unset-match)))
	(t
	 (tpu-error "No selection active."))))

(defun tpu-substitute (num)
  "Replace the selected region with the contents of the cut buffer,
and repeat most recent search.  A numeric argument serves as a repeat count.
A negative argument means replace all occurrences of the search string."
  (interactive "p")
  (cond ((or (tpu-mark) (tpu-check-match))
	 (while (and (not (= num 0)) (or (tpu-mark) (tpu-check-match)))
	   (let ((beg (point)))
	     (tpu-replace)
	     (if tpu-searching-forward (forward-char -1) (goto-char beg))
	     (if (= num 1) (tpu-search-internal tpu-search-last-string)
	       (tpu-search-internal-core tpu-search-last-string)))
	   (setq num (1- num))))
	(t
	 (tpu-error "No selection active."))))

(defun tpu-lm-replace (from to)
  "Interactively search for OLD-string and substitute NEW-string."
  (interactive (list (tpu-regexp-prompt "Old String: ")
		     (tpu-regexp-prompt "New String: ")))

  (let ((doit t) (strings 0))

    ;; Can't replace null strings
    (if (string= "" from) (tpu-error "No string to replace."))

    ;; Find the first occurrence
    (tpu-set-search)
    (tpu-search-internal from t)

    ;; Loop on replace question - yes, no, all, last, or quit.
    (while doit
      (if (not (tpu-check-match)) (setq doit nil)
	(progn
	  (move-overlay tpu-replace-overlay
			(tpu-match-beginning) (tpu-match-end) (current-buffer))
	  (message "Replace? Type Yes, No, All, Last, or Quit: ")
	  (let ((ans (read-char)))

	    (cond ((or (= ans ?y) (= ans ?Y) (= ans ?\r) (= ans ?\ ))
		   (let ((beg (point)))
		     (replace-match to (not case-replace) (not tpu-regexp-p))
		     (setq strings (1+ strings))
		     (if tpu-searching-forward (forward-char -1) (goto-char beg)))
		   (tpu-search-internal from t))

		  ((or (= ans ?n) (= ans ?N) (= ans ?\C-?))
		   (tpu-search-internal from t))

		  ((or (= ans ?a) (= ans ?A))
		   (save-excursion
		     (let ((beg (point)))
		       (replace-match to (not case-replace) (not tpu-regexp-p))
		       (setq strings (1+ strings))
		       (if tpu-searching-forward (forward-char -1) (goto-char beg)))
		     (tpu-search-internal-core from t)
		     (while (tpu-check-match)
		       (let ((beg (point)))
			 (replace-match to (not case-replace) (not tpu-regexp-p))
			 (setq strings (1+ strings))
			 (if tpu-searching-forward (forward-char -1) (goto-char beg)))
		       (tpu-search-internal-core from t)))
		   (setq doit nil))

		  ((or (= ans ?l) (= ans ?L))
		   (let ((beg (point)))
		     (replace-match to (not case-replace) (not tpu-regexp-p))
		     (setq strings (1+ strings))
		     (if tpu-searching-forward (forward-char -1) (goto-char beg)))
		   (setq doit nil))

		  ((or (= ans ?q) (= ans ?Q))
		   (tpu-unset-match)
		   (setq doit nil)))))))

    (move-overlay tpu-replace-overlay 1 1 (current-buffer))
    (message "Replaced %s occurrence%s." strings (if (not (= 1 strings)) "s" ""))))

(defun tpu-emacs-replace (&optional dont-ask)
  "A TPU-edt interface to the Emacs replace functions.
If TPU-edt is currently in regular expression mode, the Emacs regular
expression replace functions are used.  If an argument is supplied,
replacements are performed without asking.  Only works in forward direction."
  (interactive "P")
  (cond (dont-ask
	 (setq current-prefix-arg nil)
	 (call-interactively
	  (if tpu-regexp-p 'replace-regexp 'replace-string)))
	(t
	 (call-interactively
	  (if tpu-regexp-p 'query-replace-regexp 'query-replace)))))

(defun tpu-add-at-bol (text)
  "Add text to the beginning of each line in a region,
or each line in the entire buffer if no region is selected."
  (interactive
   (list (tpu-string-prompt "String to add: " 'tpu-add-at-bol-hist)))
  (if (string= "" text) (tpu-error "No string specified."))
  (cond ((tpu-mark)
	 (save-excursion
	   (if (> (point) (tpu-mark)) (exchange-point-and-mark))
	   (while (and (< (point) (tpu-mark)) (re-search-forward "^" (tpu-mark) t))
	     (if (< (point) (tpu-mark)) (replace-match text))))
	 (tpu-unselect t))
	(t
	 (save-excursion
	   (goto-char (point-min))
	   (while (and (re-search-forward "^" nil t) (not (eobp)))
	     (replace-match text))))))

(defun tpu-add-at-eol (text)
  "Add text to the end of each line in a region,
or each line of the entire buffer if no region is selected."
  (interactive
   (list (tpu-string-prompt "String to add: " 'tpu-add-at-eol-hist)))
  (if (string= "" text) (tpu-error "No string specified."))
  (cond ((tpu-mark)
	 (save-excursion
	   (if (> (point) (tpu-mark)) (exchange-point-and-mark))
	   (while (< (point) (tpu-mark))
	     (end-of-line)
	     (if (<= (point) (tpu-mark)) (insert text))
	     (forward-line)))
	 (tpu-unselect t))
	(t
	 (save-excursion
	   (goto-char (point-min))
	   (while (not (eobp))
	     (end-of-line) (insert text) (forward-line))))))

(defun tpu-trim-line-ends nil
  "Remove trailing whitespace from every line in the buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t][ \t]*$" nil t)
	(delete-region (match-beginning 0) (match-end 0))))))


;;;
;;;  Movement by character
;;;
(defun tpu-char (num)
  "Move to the next character in the current direction.
A repeat count means move that many characters."
  (interactive "p")
  (if tpu-advance (tpu-forward-char num) (tpu-backward-char num)))

(defun tpu-forward-char (num)
  "Move right ARG characters (left if ARG is negative)."
  (interactive "p")
  (forward-char num))

(defun tpu-backward-char (num)
  "Move left ARG characters (right if ARG is negative)."
  (interactive "p")
  (backward-char num))


;;;
;;;  Movement by word
;;;
(defvar tpu-word-separator-list '()
  "List of additional word separators.")
(defvar tpu-skip-chars "^ \t"
  "Characters to skip when moving by word.
Additional word separators are added to this string.")

(defun tpu-word (num)
  "Move to the beginning of the next word in the current direction.
A repeat count means move that many words."
  (interactive "p")
  (if tpu-advance (tpu-forward-to-word num) (tpu-backward-to-word num)))

(defun tpu-forward-to-word (num)
  "Move forward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (while (and (> num 0) (not (eobp)))
    (let* ((beg (point))
	   (end (prog2 (end-of-line) (point) (goto-char beg))))
      (cond ((eolp)
	     (forward-char 1))
	    ((memq (char-after (point)) tpu-word-separator-list)
	     (forward-char 1)
	     (skip-chars-forward " \t" end))
	    (t
	     (skip-chars-forward tpu-skip-chars end)
	     (skip-chars-forward " \t" end))))
    (setq num (1- num))))

(defun tpu-backward-to-word (num)
  "Move backward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (while (and (> num 0) (not (bobp)))
    (let* ((beg (point))
	   (end (prog2 (beginning-of-line) (point) (goto-char beg))))
      (cond ((bolp)
	     ( forward-char -1))
	    ((memq (char-after (1- (point)))  tpu-word-separator-list)
	     (forward-char -1))
	    (t
	     (skip-chars-backward " \t" end)
	     (skip-chars-backward tpu-skip-chars end)
	     (if (and (not (bolp)) (= ?  (char-syntax (char-after (point)))))
		 (forward-char -1)))))
    (setq num (1- num))))

(defun tpu-add-word-separators (separators)
  "Add new word separators for TPU-edt word commands."
  (interactive "sSeparators: ")
  (let* ((n 0) (length (length separators)))
    (while (< n length)
      (let ((char (aref separators n))
	    (ss (substring separators n (1+ n))))
	(cond ((not (memq char tpu-word-separator-list))
	       (setq tpu-word-separator-list
		     (append ss tpu-word-separator-list))
	       (cond ((= char ?-)
		      (setq tpu-skip-chars (concat tpu-skip-chars "\\-")))
		     ((= char ?\\)
		      (setq tpu-skip-chars (concat tpu-skip-chars "\\\\")))
		     ((= char ?^)
		      (setq tpu-skip-chars (concat tpu-skip-chars "\\^")))
		     (t
		      (setq tpu-skip-chars (concat tpu-skip-chars ss))))))
	(setq n (1+ n))))))

(defun tpu-reset-word-separators nil
  "Reset word separators to default value."
  (interactive)
  (setq tpu-word-separator-list nil)
  (setq tpu-skip-chars "^ \t"))

(defun tpu-set-word-separators (separators)
  "Set new word separators for TPU-edt word commands."
  (interactive "sSeparators: ")
  (tpu-reset-word-separators)
  (tpu-add-word-separators separators))


;;;
;;;  Movement by line
;;;
(defun tpu-next-line (num)
  "Move to next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (line-move num)
  (setq this-command 'next-line))

(defun tpu-previous-line (num)
  "Move to previous line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (line-move (- num))
  (setq this-command 'previous-line))

(defun tpu-next-beginning-of-line (num)
  "Move to beginning of line; if at beginning, move to beginning of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (backward-char 1)
  (forward-visible-line (- 1 num)))

(defun tpu-end-of-line (num)
  "Move to the next end of line in the current direction.
A repeat count means move that many lines."
  (interactive "p")
  (if tpu-advance (tpu-next-end-of-line num) (tpu-previous-end-of-line num)))

(defun tpu-next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (forward-char 1)
  (end-of-line num))

(defun tpu-previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (end-of-line (- 1 num)))

(defun tpu-current-end-of-line nil
  "Move point to end of current line."
  (interactive)
  (let ((beg (point)))
    (end-of-line)
    (if (= beg (point)) (message "You are already at the end of a line."))))

(defun tpu-line (num)
  "Move to the beginning of the next line in the current direction.
A repeat count means move that many lines."
  (interactive "p")
  (if tpu-advance (tpu-forward-line num) (tpu-backward-line num)))

(defun tpu-forward-line (num)
  "Move to beginning of next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (forward-line num))

(defun tpu-backward-line (num)
  "Move to beginning of previous line.
Prefix argument serves as repeat count."
  (interactive "p")
  (or (bolp) (>= 0 num) (setq num (- num 1)))
  (forward-line (- num)))


;;;
;;;  Movement by paragraph
;;;
(defun tpu-paragraph (num)
  "Move to the next paragraph in the current direction.
A repeat count means move that many paragraphs."
  (interactive "p")
  (if tpu-advance
      (tpu-next-paragraph num) (tpu-previous-paragraph num)))

(defun tpu-next-paragraph (num)
  "Move to beginning of the next paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (beginning-of-line)
  (while (and (not (eobp)) (> num 0))
    (if (re-search-forward "^[ \t]*$" nil t)
	(if (re-search-forward "[^ \t\n]" nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (point-max))))
    (setq num (1- num)))
  (beginning-of-line))


(defun tpu-previous-paragraph (num)
  "Move to beginning of previous paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (end-of-line)
  (while (and (not (bobp)) (> num 0))
    (if (not (and (re-search-backward "^[ \t]*$" nil t)
		  (re-search-backward "[^ \t\n]" nil t)
		  (re-search-backward "^[ \t]*$" nil t)
		  (progn (re-search-forward "[^ \t\n]" nil t)
			 (goto-char (match-beginning 0)))))
	(goto-char (point-min)))
    (setq num (1- num)))
  (beginning-of-line))


;;;
;;;  Movement by page
;;;
(defun tpu-page (num)
  "Move to the next page in the current direction.
A repeat count means move that many pages."
  (interactive "p")
  (if tpu-advance (forward-page num) (backward-page num))
  (if (eobp) (recenter -1)))


;;;
;;;  Scrolling and movement within the buffer
;;;
(defun tpu-scroll-window (num)
  "Scroll the display to the next section in the current direction.
A repeat count means scroll that many sections."
  (interactive "p")
  (if tpu-advance (tpu-scroll-window-up num) (tpu-scroll-window-down num)))

(defun tpu-scroll-window-down (num)
  "Scroll the display down to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (tpu-current-line))
	 (height (1- (window-height)))
	 (lines (* num (/ (* height tpu-percent-scroll) 100))))
    (line-move (- lines))
    (if (> lines beg) (recenter 0))))

(defun tpu-scroll-window-up (num)
  "Scroll the display up to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (tpu-current-line))
	 (height (1- (window-height)))
	 (lines (* num (/ (* height tpu-percent-scroll) 100))))
    (line-move lines)
    (if (>= (+ lines beg) height) (recenter -1))))

(defun tpu-pan-right (num)
  "Pan right tpu-pan-columns (16 by default).
Accepts a prefix argument for the number of tpu-pan-columns to scroll."
  (interactive "p")
  (scroll-left (* tpu-pan-columns num)))

(defun tpu-pan-left (num)
  "Pan left tpu-pan-columns (16 by default).
Accepts a prefix argument for the number of tpu-pan-columns to scroll."
  (interactive "p")
  (scroll-right (* tpu-pan-columns num)))

(defun tpu-move-to-beginning nil
  "Move cursor to the beginning of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-min)))

(defun tpu-move-to-end nil
  "Move cursor to the end of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(defun tpu-goto-percent (perc)
  "Move point to ARG percentage of the buffer."
  (interactive "NGoto-percentage: ")
  (if (or (> perc 100) (< perc 0))
      (tpu-error "Percentage %d out of range 0 < percent < 100." perc)
    (goto-char (/ (* (point-max) perc) 100))))

(defun tpu-beginning-of-window nil
  "Move cursor to top of window."
  (interactive)
  (move-to-window-line 0))

(defun tpu-end-of-window nil
  "Move cursor to bottom of window."
  (interactive)
  (move-to-window-line -1))

(defun tpu-line-to-bottom-of-window nil
  "Move the current line to the bottom of the window."
  (interactive)
  (recenter -1))

(defun tpu-line-to-top-of-window nil
  "Move the current line to the top of the window."
  (interactive)
  (recenter 0))


;;;
;;;  Direction
;;;
(defun tpu-advance-direction nil
  "Set TPU Advance mode so keypad commands move forward."
  (interactive)
  (setq tpu-direction-string " Advance")
  (setq tpu-advance t)
  (setq tpu-reverse nil)
  (tpu-set-search)
  (tpu-update-mode-line))

(defun tpu-backup-direction nil
  "Set TPU Backup mode so keypad commands move backward."
  (interactive)
  (setq tpu-direction-string " Reverse")
  (setq tpu-advance nil)
  (setq tpu-reverse t)
  (tpu-set-search)
  (tpu-update-mode-line))

(defun tpu-toggle-direction nil
  "Change the current TPU direction."
  (interactive)
  (if tpu-advance (tpu-backup-direction) (tpu-advance-direction)))


;;;
;;;  Minibuffer map additions to make KP_enter = RET
;;;
;; Standard Emacs settings under xterm in function-key-map map
;; "\eOM" to [kp-enter] and [kp-enter] to RET, but since the output of the map
;; is not fed back into the map, the key stays as kp-enter :-(.
(define-key minibuffer-local-map [kp-enter] 'exit-minibuffer)
;; These are not necessary because they are inherited.
;; (define-key minibuffer-local-ns-map [kp-enter] 'exit-minibuffer)
;; (define-key minibuffer-local-completion-map [kp-enter] 'exit-minibuffer)
(define-key minibuffer-local-must-match-map [kp-enter] 'minibuffer-complete-and-exit)


;;;
;;;  Minibuffer map additions to set search direction
;;;
(define-key minibuffer-local-map [kp-4] 'tpu-search-forward-exit)  ;KP4
(define-key minibuffer-local-map [kp-5] 'tpu-search-backward-exit) ;KP5


;;;
;;;  Functions to set, reset, and toggle the control key bindings
;;;

(defvar tpu-control-keys-map
  (let ((map (make-sparse-keymap)))
  (define-key map "\C-\\" 'quoted-insert)                ; ^\
  (define-key map "\C-a" 'tpu-toggle-overwrite-mode)     ; ^A
  (define-key map "\C-b" 'repeat-complex-command)        ; ^B
  (define-key map "\C-e" 'tpu-current-end-of-line)       ; ^E
  (define-key map "\C-h" 'tpu-next-beginning-of-line)    ; ^H (BS)
  (define-key map "\C-j" 'tpu-delete-previous-word)      ; ^J (LF)
  (define-key map "\C-k" 'tpu-define-macro-key)          ; ^K
  (define-key map "\C-l" 'tpu-insert-formfeed)           ; ^L (FF)
  (define-key map "\C-r" 'recenter)                      ; ^R
  (define-key map "\C-u" 'tpu-delete-to-bol)             ; ^U
  (define-key map "\C-v" 'tpu-quoted-insert)             ; ^V
  (define-key map "\C-w" 'redraw-display)                ; ^W
  (define-key map "\C-z" 'tpu-exit)                      ; ^Z
  map))

(defun tpu-set-control-keys ()
  "Set control keys to TPU style functions."
  (tpu-reset-control-keys 'tpu))

(defun tpu-reset-control-keys (tpu-style)
  "Set control keys to TPU or Emacs style functions."
  (let ((parent (keymap-parent tpu-global-map)))
    (if tpu-style
        (if (eq parent tpu-control-keys-map)
            nil                         ;All done already.
          ;; Insert tpu-control-keys-map in the global map.
          (set-keymap-parent tpu-control-keys-map parent)
          (set-keymap-parent tpu-global-map tpu-control-keys-map))
      (if (not (eq parent tpu-control-keys-map))
          nil                         ;All done already.
        ;; Remove tpu-control-keys-map from the global map.
        (set-keymap-parent tpu-global-map (keymap-parent parent))
        (set-keymap-parent tpu-control-keys-map nil)))
    (setq tpu-control-keys tpu-style)))

(defun tpu-toggle-control-keys nil
  "Toggle control key bindings between TPU-edt and Emacs."
  (interactive)
  (tpu-reset-control-keys (not tpu-control-keys))
  (and (called-interactively-p 'interactive)
       (message "Control keys function with %s bindings."
		(if tpu-control-keys "TPU-edt" "Emacs"))))


;;;
;;;  Emacs version 19 minibuffer history support
;;;
(defun tpu-next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element n)
  (goto-char (point-max)))

(defun tpu-previous-history-element (n)
  "Insert the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (previous-history-element n)
  (goto-char (point-max)))

(defun tpu-arrow-history nil
  "Modify minibuffer maps to use arrows for history recall."
  (interactive)
  (dolist (cur (where-is-internal 'tpu-previous-line))
    (define-key read-expression-map cur 'tpu-previous-history-element)
    (define-key minibuffer-local-map cur 'tpu-previous-history-element)
    ;; These are inherited anyway.  --Stef
    ;; (define-key minibuffer-local-ns-map cur 'tpu-previous-history-element)
    ;; (define-key minibuffer-local-completion-map cur 'tpu-previous-history-element)
    ;; (define-key minibuffer-local-must-match-map cur 'tpu-previous-history-element)
    )

  (dolist (cur (where-is-internal 'tpu-next-line))
    (define-key read-expression-map cur 'tpu-next-history-element)
    (define-key minibuffer-local-map cur 'tpu-next-history-element)
    ;; These are inherited anyway.  --Stef
    ;; (define-key minibuffer-local-ns-map cur 'tpu-next-history-element)
    ;; (define-key minibuffer-local-completion-map cur 'tpu-next-history-element)
    ;; (define-key minibuffer-local-must-match-map cur 'tpu-next-history-element)
    ))


;;;
;;;  Emacs version 19 X-windows key definition support
;;;
(defun tpu-load-xkeys (file)
  "Load the TPU-edt X-windows key definitions FILE.
If FILE is nil, try to load a default file.  The default file names are
`~/.tpu-lucid-keys' for XEmacs, and `~/.tpu-keys' for Emacs."
  (interactive "fX key definition file: ")
  (cond (file
	 (setq file (expand-file-name file)))
	(tpu-xkeys-file
	 (setq file (expand-file-name tpu-xkeys-file)))
	((featurep 'xemacs)
	 (setq file (convert-standard-filename
		     (expand-file-name "~/.tpu-lucid-keys"))))
	(t
	 (setq file (convert-standard-filename
		     (expand-file-name "~/.tpu-keys")))
	 (and (not (file-exists-p file))
	      (file-exists-p
	       (convert-standard-filename
		(expand-file-name "~/.tpu-gnu-keys")))
	      (tpu-copy-keyfile
	       (convert-standard-filename
		(expand-file-name "~/.tpu-gnu-keys")) file))))
  (cond ((file-readable-p file)
	 (load-file file))
	(t
         ;; This used to force the user to build `file'.  With the
         ;; new code, such a file may not be necessary.  In case it
         ;; is, issue a message giving a hint as to how to build it.
         (message "%s not found: use M-x tpu-mapper to create it"
                  (abbreviate-file-name file)))))

(defun tpu-copy-keyfile (oldname newname)
  "Copy the TPU-edt X key definitions file to the new default name."
  (interactive "fOld name: \nFNew name: ")
  (if (not (get-buffer "*TPU-Notice*")) (generate-new-buffer "*TPU-Notice*"))
  (set-buffer "*TPU-Notice*")
  (erase-buffer)
  (insert "
  NOTICE --

  The default name of the TPU-edt key definition file has changed
  from `~/.tpu-gnu-keys' to `~/.tpu-keys'.  With your permission,
  your key definitions will be copied to the new file.  If you'll
  never use older versions of Emacs, you can remove the old file.
  If the copy fails, you'll be asked if you want to create a new
  key definitions file.  Do you want to copy your key definition
  file now?
  ")
  (save-window-excursion
    (switch-to-buffer-other-window "*TPU-Notice*")
    (shrink-window-if-larger-than-buffer)
    (goto-char (point-min))
    (beep)
    (and (tpu-y-or-n-p "Copy key definitions to the new file now? ")
	 (condition-case conditions
             (copy-file oldname newname)
	   (error (message "Sorry, couldn't copy - %s." (cdr conditions)))))
    (kill-buffer "*TPU-Notice*")))

(defvar tpu-edt-old-global-values nil)

;;;
;;;  Start and Stop TPU-edt
;;;
;;;###autoload
(defun tpu-edt-on ()
  "Turn on TPU/edt emulation."
  (interactive)
  ;; To clean things up (and avoid cycles in the global map).
  (tpu-edt-off)
  ;; First, activate tpu-global-map, while protecting the original keymap.
  (set-keymap-parent tpu-global-map global-map)
  (setq global-map tpu-global-map)
  (use-global-map global-map)
  ;; Then do the normal TPU setup.
  (transient-mark-mode t)
  (add-hook 'post-command-hook 'tpu-search-highlight)
  (tpu-set-mode-line t)
  (tpu-advance-direction)
  ;; set page delimiter, display line truncation, and scrolling like TPU
  (dolist (varval '((page-delimiter . "\f")
                    (truncate-lines . t)
                    (scroll-step . 1)))
    (push (cons (car varval) (default-value (car varval)))
          tpu-edt-old-global-values)
    (set-default (car varval) (cdr varval)))
  (tpu-set-control-keys)
  (and window-system (tpu-load-xkeys nil))
  (tpu-arrow-history)
  ;; Then protect tpu-global-map from user modifications.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map global-map)
    (setq global-map map)
    (use-global-map map))
  (setq tpu-edt-mode t))

(defun tpu-edt-off ()
  "Turn off TPU/edt emulation.  Note that the keypad is left on."
  (interactive)
  (tpu-reset-control-keys nil)
  (remove-hook 'post-command-hook 'tpu-search-highlight)
  (tpu-set-mode-line nil)
  (while tpu-edt-old-global-values
    (let ((varval (pop tpu-edt-old-global-values)))
      (set-default (car varval) (cdr varval))))
  ;; Remove tpu-global-map from the global map.
  (let ((map global-map))
    (while map
      (let ((parent (keymap-parent map)))
        (if (eq tpu-global-map parent)
            (set-keymap-parent map (keymap-parent parent))
          (setq map parent)))))
  ;; Only has an effect if the advice in tpu-extras has been activated.
  (condition-case nil
      (with-no-warnings (ad-disable-regexp "\\`tpu-"))
    (error nil))
  (setq tpu-edt-mode nil))


;;;### (autoloads (tpu-set-cursor-bound tpu-set-cursor-free tpu-set-scroll-margins
;;;;;;  tpu-cursor-free-mode) "tpu-extras" "tpu-extras.el" "76f06905db4c5bfb3b86491a51512a0e")
;;; Generated autoloads from tpu-extras.el

(autoload 'tpu-cursor-free-mode "tpu-extras" "\
Minor mode to allow the cursor to move freely about the screen.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

\(fn &optional ARG)" t nil)

(autoload 'tpu-set-scroll-margins "tpu-extras" "\
Set scroll margins.

\(fn TOP BOTTOM)" t nil)

(autoload 'tpu-set-cursor-free "tpu-extras" "\
Allow the cursor to move freely about the screen.

\(fn)" t nil)

(autoload 'tpu-set-cursor-bound "tpu-extras" "\
Constrain the cursor to the flow of the text.

\(fn)" t nil)

;;;***

(provide 'tpu-edt)

;;; tpu-edt.el ends here
