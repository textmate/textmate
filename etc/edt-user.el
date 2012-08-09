;;; edt-user.el --- Sample user customizations for Emacs EDT emulation

;; Copyright (C) 1986, 1992-1993, 2000-2012  Free Software Foundation, Inc.

;; Author: Kevin Gallagher <kgallagh@@spd.dsccc.com>
;; Maintainer: Kevin Gallagher <kgallagh@@spd.dsccc.com>
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

;;; Commentary:

;; This is an example of the `edt-user.el' file that you can use
;; to customize the Emacs EDT emulation mode.  Copy this file to
;; somewhere in your `load-path', and edit it as desired.
;; See Info node `edt' for more details.

;; ====================================================================


;;; Code:

;;;;
;;;; Setup user custom EDT key bindings.
;;;;

(defun edt-setup-user-bindings ()
  "Assigns user custom EDT Emulation keyboard bindings."

  ;; PF1 (GOLD), PF2, PF3, PF4
  ;;
  ;; This file MUST contain a binding of PF1 to edt-user-gold-map.  So
  ;; DON'T CHANGE OR DELETE THE REGULAR KEY BINDING OF PF1 BELOW!
  ;; (However, you may change the GOLD-PF1 binding, if you wish.)
  (edt-bind-function-key "PF1" 'edt-user-gold-map 'edt-mark-section-wisely)
  (edt-bind-function-key "PF2" 'query-replace 'other-window)
  (edt-bind-function-key "PF4" 'edt-delete-entire-line 'edt-undelete-line)

  ;; EDT Keypad Keys
  (edt-bind-function-key "KP1" 'edt-word-forward 'edt-change-case)
  (edt-bind-function-key "KP3" 'edt-word-backward 'edt-copy)
  (edt-bind-function-key "KP6" 'edt-cut-or-copy 'yank)
  (edt-bind-function-key "KP8" 'edt-scroll-window 'fill-paragraph)
  (edt-bind-function-key "KP9" 'open-line 'edt-eliminate-all-tabs)
  (edt-bind-function-key "KPP"
			 'edt-toggle-select 'edt-line-to-middle-of-window)
  (edt-bind-function-key "KPE" 'edt-change-direction 'overwrite-mode)

  ;; GOLD bindings for regular keys.
  (edt-bind-gold-key "a" 'edt-append)
  (edt-bind-gold-key "A" 'edt-append)
  (edt-bind-gold-key "h" 'edt-electric-user-keypad-help)
  (edt-bind-gold-key "H" 'edt-electric-user-keypad-help)

  ;; Control bindings for regular keys.
  ;;; Leave binding of C-c as original prefix key.
  (edt-bind-key "\C-j" 'edt-duplicate-word)
  (edt-bind-key "\C-k" 'edt-define-key)
  (edt-bind-gold-key  "\C-k" 'edt-restore-key)
  (edt-bind-key "\C-l" 'edt-learn)
  ;;; Leave binding of C-m to newline.
  (edt-bind-key "\C-n" 'edt-set-screen-width-80)
  (edt-bind-key "\C-o" 'open-line)
  (edt-bind-key "\C-p" 'fill-paragraph)
  ;;; Leave binding of C-r to isearch-backward.
  ;;; Leave binding of C-s to isearch-forward.
  (edt-bind-key "\C-t" 'edt-display-the-time)
  (edt-bind-key "\C-v" 'redraw-display)
  (edt-bind-key "\C-w" 'edt-set-screen-width-132)
  ;;; Leave binding of C-x as original prefix key.
)

;;;
;;; LK-201 KEYBOARD USER EDT KEYPAD HELP
;;;

(defun edt-user-keypad-help ()
  "
                                USER EDT Keypad Active

                                +----------+----------+----------+----------+
   F7: Copy Rectangle           |Prev Line |Next Line |Bkwd Char |Frwd Char |
   F8: Cut Rect Overstrike      |   (UP)   |  (DOWN)  |  (LEFT)  | (RIGHT)  |
 G-F8: Paste Rect Overstrike    |Window Top|Window Bot|Bkwd Sent |Frwd Sent |
   F9: Cut Rect Insert          +----------+----------+----------+----------+
 G-F9: Paste Rect Insert
  F10: Cut Rectangle
G-F10: Paste Rectangle
  F11: ESC                      +----------+----------+----------+----------+
  F12: Beginning of Line        |   GOLD   |Query Repl|  FNDNXT  |Del Ent L |
G-F12: Delete Other Windows     |   (PF1)  |   (PF2)  |   (PF3)  |   (PF4)  |
  F13: Delete to Begin of Word  |Mark Wisel|Other Wind|   FIND   |  UND L   |
 HELP: Keypad Help              +----------+----------+----------+----------+
G-HELP: Emacs Help              |   PAGE   |Scroll Win|Open Line |  DEL W   |
   DO: Execute extended command |    (7)   |    (8)   |    (9)   |   (-)    |
  C-a: Beginning of Line        |Ex Ext Cmd|Fill Parag|Elim Tabs |  UND W   |
  C-b: Backward Character       +----------+----------+----------+----------+
  C-d: Delete Character         |  ADVANCE |  BACKUP  | CUT/COPY |  DEL C   |
  C-e: End of Line              |    (4)   |    (5)   |    (6)   |   (,)    |
  C-f: Forward Character        |   BOTTOM |    TOP   |   Yank   |  UND C   |
  C-g: Keyboard Quit            +----------+----------+----------+----------+
G-C-g: Keyboard Quit            | Fwd Word |    EOL   | Bwd Word |  Change  |
  C-h: Electric Emacs Help      |    (1)   |    (2)   |    (3)   | Direction|
G-C-h: Emacs Help               | CHNGCASE |  DEL EOL |   COPY   |          |
  C-i: Indent for Tab           +---------------------+----------+  (ENTER) |
  C-j: Duplicate Word           |         LINE        |SELECT/RES|          |
  C-k: Define Key               |         (0)         |    (.)   |  Toggle  |
G-C-k: Restore Key              |      Open Line      |Center Lin|Insrt/Over|
  C-l: Learn                    +---------------------+----------+----------+
  C-n: Set Screen Width 80
  C-o: Open Line                       +----------+----------+----------+
  C-p: Fill Paragraph                  |  FNDNXT  |   Yank   |    CUT   |
  C-q: Quoted Insert                   |  (FIND)) | (INSERT) | (REMOVE) |
  C-r: Isearch Backward                |   FIND   |          |   COPY   |
  C-s: Isearch Forward                 +----------+----------+----------+
  C-t: Display the Time                |SELECT/RES|SECT BACKW|SECT FORWA|
  C-u: Universal Argument              | (SELECT) |(PREVIOUS)|  (NEXT)  |
  C-v: Redraw Display                  |          |          |          |
  C-w: Set Screen Width 132            +----------+----------+----------+
  C-z: Suspend Emacs
G-C-\\: Split Window

  G-a: Append to Kill Buffer
  G-b: Buffer Menu
  G-c: Compile
  G-d: Delete Window
  G-e: Exit
  G-f: Find File
  G-g: Find File Other Window
  G-h: Keypad Help
  G-i: Insert File
  G-k: Toggle Capitalization Word
  G-l: Lowercase Word or Region
  G-m: Save Some Buffers
  G-n: Next Error
  G-o: Switch Windows
  G-q: Quit
  G-r: Revert File
  G-s: Save Buffer
  G-u: Uppercase Word or Region
  G-v: Find File Other Window
  G-w: Write file
  G-y: EDT Emulation OFF
  G-z: Switch to Default EDT Key Bindings
  G-2: Split Window
  G-%: Go to Percentage
  G- : Undo  (GOLD Spacebar)
  G-=: Go to Line
  G-`: What line
  G-/: Query-Replace"

  (interactive)
  (describe-function 'edt-user-keypad-help))

;;; edt-user.el ends here
