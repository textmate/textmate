;;; edt-pc.el --- enhanced EDT keypad mode emulation for PC 101 keyboards

;; Copyright (C) 1986, 1994-1995, 2001-2012 Free Software Foundation, Inc.

;; Author: Kevin Gallagher <Kevin.Gallagher@boeing.com>
;; Maintainer: Kevin Gallagher <Kevin.Gallagher@boeing.com>
;; Keywords: emulations
;; Package: edt

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

;; See Info node `edt' for more details.

;; ====================================================================

;;;;
;;;; KEY TRANSLATIONS
;;;;

;; Associate EDT keynames with Emacs terminal function vector names.
;;
;; To emulate the DEC LK-201 keypad keys on the PC 101 keyboard,
;; NumLock must be ON.
;;
;; The PC keypad keys are mapped to the corresponding DEC LK-201
;; keypad keys according to the corresponding physical position on
;; the keyboard.  Thus, the physical position of the PC keypad key
;; determines its function, not the PC keycap name.
;;
;; There are two LK-201 keypad keys needing special handling: PF1 and
;; the keypad comma key.
;;
;; PF1:
;;  Most PC software does not see a press of the NumLock key.  A TSR
;;  program distributed with MS-Kermit to support its VT-100 emulation
;;  solves this problem.  The TSR, called GOLD, causes a press of the
;;  keypad NumLock key to look as if the PC F1 key were pressed.  So
;;  the PC F1 key is mapped here to behave as the PF1 (GOLD) key.
;;  Then with GOLD loaded, the NumLock key will behave as the GOLD key.
;;
;;  By the way, with GOLD loaded, you can still toggle numlock on/off.
;;  GOLD binds this to Shift-NumLock.
;;
;; Keypad Comma:
;;  There is no physical PC keypad key to correspond to the LK-201
;;  keypad comma key.  So, the EDT Emulation is configured below to
;;  ignore attempts to bind functions to the keypad comma key.
;;
;; Finally, F2 through F12 are also available for making key bindings
;; in the EDT Emulation on the PC.  F1 is reserved for the GOLD key,
;; so don't attempt to bind anything to it.  Also, F13, F14, HELP, DO,
;; and F17 through F20 do not exist on the PC, so the EDT emulation is
;; configured below to ignore attempts to bind functions to those keys.
;;

;;; Code:

(defconst *EDT-keys*
  '(("KP0" . [kp-0]) ("KP1" . [kp-1]) ("KP2" . [kp-2]) ("KP3" . [kp-3])
    ("KP4" . [kp-4]) ("KP5" . [kp-5]) ("KP6" . [kp-6]) ("KP7" . [kp-7])
    ("KP8" . [kp-8]) ("KP9" . [kp-9]) ("KP," . "" )
    ("KP-" . [kp-add]) ("KPP" . [kp-decimal]) ("KPE" . [kp-enter])
    ("PF1" . [f1]) ("PF2" . [kp-divide]) ("PF3" . [kp-multiply])
    ("PF4" . [kp-subtract])
    ("UP" . [up]) ("DOWN" . [down]) ("RIGHT" . [right]) ("LEFT" . [left])
    ("FIND" . [insert]) ("INSERT" . [home]) ("REMOVE" . [prior])
    ("SELECT" . [delete]) ("PREVIOUS" . [end]) ("NEXT" . [next])
    ("F1" . "" ) ("F2" . [f2]) ("F3" . [f3]) ("F4" . [f4]) ("F5" . [f5])
    ("F6" . [f6]) ("F7" . [f7]) ("F8" . [f8]) ("F9" . [f9]) ("F10" . [f10])
    ("F11" . [f11]) ("F12" . [f12]) ("F13" . "" ) ("F14" . "" )
    ("HELP" . "" ) ("DO" . "" ) ("F17" . "" ) ("F18" . "" )
    ("F19" . "" ) ("F20" . "" )))

;;; edt-pc.el ends here
