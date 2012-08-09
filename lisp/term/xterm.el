;;; xterm.el --- define function key sequences and standard colors for xterm

;; Copyright (C) 1995, 2001-2012 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

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

;;; Code:

(defgroup xterm nil
  "XTerm support."
  :version "24.1"
  :group 'emacs)

(defcustom xterm-extra-capabilities 'check
  "Whether Xterm supports some additional, more modern, features.
If nil, just assume that it does not.
If `check', try to check if it does.
If a list, assume that the listed features are supported, without checking.

The relevant features are:
  modifyOtherKeys  -- if supported, more key bindings work (e.g, \"\\C-,\")
  reportBackground -- if supported, Xterm reports its background color
"
  :version "24.1"
  :group 'xterm
  :type '(choice (const :tag "No" nil)
                 (const :tag "Check" check)
                 ;; NOTE: If you add entries here, make sure to update
                 ;; `tocheck-capabilities' in `terminal-init-xterm' as well.
                 (set (const :tag "modifyOtherKeys support" modifyOtherKeys)
                      (const :tag "report background" reportBackground))))

(defvar xterm-function-map
  (let ((map (make-sparse-keymap)))

    ;; xterm from X.org 6.8.2 uses these key definitions.
    (define-key map "\eOP" [f1])
    (define-key map "\eOQ" [f2])
    (define-key map "\eOR" [f3])
    (define-key map "\eOS" [f4])
    (define-key map "\e[15~" [f5])
    (define-key map "\e[17~" [f6])
    (define-key map "\e[18~" [f7])
    (define-key map "\e[19~" [f8])
    (define-key map "\e[20~" [f9])
    (define-key map "\e[21~" [f10])
    (define-key map "\e[23~" [f11])
    (define-key map "\e[24~" [f12])

    (define-key map "\eO2P" [S-f1])
    (define-key map "\eO2Q" [S-f2])
    (define-key map "\eO2R" [S-f3])
    (define-key map "\eO2S" [S-f4])
    (define-key map "\e[1;2P" [S-f1])
    (define-key map "\e[1;2Q" [S-f2])
    (define-key map "\e[1;2R" [S-f3])
    (define-key map "\e[1;2S" [S-f4])
    (define-key map "\e[15;2~" [S-f5])
    (define-key map "\e[17;2~" [S-f6])
    (define-key map "\e[18;2~" [S-f7])
    (define-key map "\e[19;2~" [S-f8])
    (define-key map "\e[20;2~" [S-f9])
    (define-key map "\e[21;2~" [S-f10])
    (define-key map "\e[23;2~" [S-f11])
    (define-key map "\e[24;2~" [S-f12])

    (define-key map "\eO5P" [C-f1])
    (define-key map "\eO5Q" [C-f2])
    (define-key map "\eO5R" [C-f3])
    (define-key map "\eO5S" [C-f4])
    (define-key map "\e[15;5~" [C-f5])
    (define-key map "\e[17;5~" [C-f6])
    (define-key map "\e[18;5~" [C-f7])
    (define-key map "\e[19;5~" [C-f8])
    (define-key map "\e[20;5~" [C-f9])
    (define-key map "\e[21;5~" [C-f10])
    (define-key map "\e[23;5~" [C-f11])
    (define-key map "\e[24;5~" [C-f12])

    (define-key map "\eO6P" [C-S-f1])
    (define-key map "\eO6Q" [C-S-f2])
    (define-key map "\eO6R" [C-S-f3])
    (define-key map "\eO6S" [C-S-f4])
    (define-key map "\e[15;6~" [C-S-f5])
    (define-key map "\e[17;6~" [C-S-f6])
    (define-key map "\e[18;6~" [C-S-f7])
    (define-key map "\e[19;6~" [C-S-f8])
    (define-key map "\e[20;6~" [C-S-f9])
    (define-key map "\e[21;6~" [C-S-f10])
    (define-key map "\e[23;6~" [C-S-f11])
    (define-key map "\e[24;6~" [C-S-f12])

    (define-key map "\eO3P" [M-f1])
    (define-key map "\eO3Q" [M-f2])
    (define-key map "\eO3R" [M-f3])
    (define-key map "\eO3S" [M-f4])
    (define-key map "\e[15;3~" [M-f5])
    (define-key map "\e[17;3~" [M-f6])
    (define-key map "\e[18;3~" [M-f7])
    (define-key map "\e[19;3~" [M-f8])
    (define-key map "\e[20;3~" [M-f9])
    (define-key map "\e[21;3~" [M-f10])
    (define-key map "\e[23;3~" [M-f11])
    (define-key map "\e[24;3~" [M-f12])

    (define-key map "\eO4P" [M-S-f1])
    (define-key map "\eO4Q" [M-S-f2])
    (define-key map "\eO4R" [M-S-f3])
    (define-key map "\eO4S" [M-S-f4])
    (define-key map "\e[15;4~" [M-S-f5])
    (define-key map "\e[17;4~" [M-S-f6])
    (define-key map "\e[18;4~" [M-S-f7])
    (define-key map "\e[19;4~" [M-S-f8])
    (define-key map "\e[20;4~" [M-S-f9])
    (define-key map "\e[21;4~" [M-S-f10])
    (define-key map "\e[23;4~" [M-S-f11])
    (define-key map "\e[24;4~" [M-S-f12])

    (define-key map "\eOA" [up])
    (define-key map "\eOB" [down])
    (define-key map "\eOC" [right])
    (define-key map "\eOD" [left])
    (define-key map "\eOF" [end])
    (define-key map "\eOH" [home])

    (define-key map "\e[1;2A" [S-up])
    (define-key map "\e[1;2B" [S-down])
    (define-key map "\e[1;2C" [S-right])
    (define-key map "\e[1;2D" [S-left])
    (define-key map "\e[1;2F" [S-end])
    (define-key map "\e[1;2H" [S-home])

    (define-key map "\e[1;4A" [M-S-up])
    (define-key map "\e[1;4B" [M-S-down])
    (define-key map "\e[1;4C" [M-S-right])
    (define-key map "\e[1;4D" [M-S-left])
    (define-key map "\e[1;4F" [M-S-end])
    (define-key map "\e[1;4H" [M-S-home])

    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[1;5F" [C-end])
    (define-key map "\e[1;5H" [C-home])

    (define-key map "\e[1;6A" [C-S-up])
    (define-key map "\e[1;6B" [C-S-down])
    (define-key map "\e[1;6C" [C-S-right])
    (define-key map "\e[1;6D" [C-S-left])
    (define-key map "\e[1;6F" [C-S-end])
    (define-key map "\e[1;6H" [C-S-home])

    (define-key map "\e[1;7A" [C-M-up])
    (define-key map "\e[1;7B" [C-M-down])
    (define-key map "\e[1;7C" [C-M-right])
    (define-key map "\e[1;7D" [C-M-left])
    (define-key map "\e[1;7F" [C-M-end])
    (define-key map "\e[1;7H" [C-M-home])

    (define-key map "\e[1;8A" [C-M-S-up])
    (define-key map "\e[1;8B" [C-M-S-down])
    (define-key map "\e[1;8C" [C-M-S-right])
    (define-key map "\e[1;8D" [C-M-S-left])
    (define-key map "\e[1;8F" [C-M-S-end])
    (define-key map "\e[1;8H" [C-M-S-home])

    (define-key map "\e[1;3A" [M-up])
    (define-key map "\e[1;3B" [M-down])
    (define-key map "\e[1;3C" [M-right])
    (define-key map "\e[1;3D" [M-left])
    (define-key map "\e[1;3F" [M-end])
    (define-key map "\e[1;3H" [M-home])

    (define-key map "\e[2~" [insert])
    (define-key map "\e[3~" [delete])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])

    (define-key map "\e[2;2~" [S-insert])
    (define-key map "\e[3;2~" [S-delete])
    (define-key map "\e[5;2~" [S-prior])
    (define-key map "\e[6;2~" [S-next])

    (define-key map "\e[2;4~" [M-S-insert])
    (define-key map "\e[3;4~" [M-S-delete])
    (define-key map "\e[5;4~" [M-S-prior])
    (define-key map "\e[6;4~" [M-S-next])

    (define-key map "\e[2;5~" [C-insert])
    (define-key map "\e[3;5~" [C-delete])
    (define-key map "\e[5;5~" [C-prior])
    (define-key map "\e[6;5~" [C-next])

    (define-key map "\e[2;6~" [C-S-insert])
    (define-key map "\e[3;6~" [C-S-delete])
    (define-key map "\e[5;6~" [C-S-prior])
    (define-key map "\e[6;6~" [C-S-next])

    (define-key map "\e[2;7~" [C-M-insert])
    (define-key map "\e[3;7~" [C-M-delete])
    (define-key map "\e[5;7~" [C-M-prior])
    (define-key map "\e[6;7~" [C-M-next])

    (define-key map "\e[2;8~" [C-M-S-insert])
    (define-key map "\e[3;8~" [C-M-S-delete])
    (define-key map "\e[5;8~" [C-M-S-prior])
    (define-key map "\e[6;8~" [C-M-S-next])

    (define-key map "\e[2;3~" [M-insert])
    (define-key map "\e[3;3~" [M-delete])
    (define-key map "\e[5;3~" [M-prior])
    (define-key map "\e[6;3~" [M-next])

    (define-key map "\e[4~" [select])
    (define-key map "\e[29~" [print])

    (define-key map "\eOj" [kp-multiply])
    (define-key map "\eOk" [kp-add])
    (define-key map "\eOl" [kp-separator])
    (define-key map "\eOm" [kp-subtract])
    (define-key map "\eOo" [kp-divide])
    (define-key map "\eOp" [kp-0])
    (define-key map "\eOq" [kp-1])
    (define-key map "\eOr" [kp-2])
    (define-key map "\eOs" [kp-3])
    (define-key map "\eOt" [kp-4])
    (define-key map "\eOu" [kp-5])
    (define-key map "\eOv" [kp-6])
    (define-key map "\eOw" [kp-7])
    (define-key map "\eOx" [kp-8])
    (define-key map "\eOy" [kp-9])

    ;; These keys are available in xterm starting from version 216
    ;; if the modifyOtherKeys resource is set to 1.

    (define-key map "\e[27;5;9~"   [C-tab])
    (define-key map "\e[27;5;13~"  [C-return])
    (define-key map "\e[27;5;39~"  [?\C-\'])
    (define-key map "\e[27;5;44~"  [?\C-,])
    (define-key map "\e[27;5;45~"  [?\C--])
    (define-key map "\e[27;5;46~"  [?\C-.])
    (define-key map "\e[27;5;47~"  [?\C-/])
    (define-key map "\e[27;5;48~"  [?\C-0])
    (define-key map "\e[27;5;49~"  [?\C-1])
    ;; Not all C-DIGIT keys have a distinct binding.
    (define-key map "\e[27;5;57~"  [?\C-9])
    (define-key map "\e[27;5;59~"  [?\C-\;])
    (define-key map "\e[27;5;61~"  [?\C-=])
    (define-key map "\e[27;5;92~"  [?\C-\\])

    (define-key map "\e[27;6;33~"  [?\C-!])
    (define-key map "\e[27;6;34~"  [?\C-\"])
    (define-key map "\e[27;6;35~"  [?\C-#])
    (define-key map "\e[27;6;36~"  [?\C-$])
    (define-key map "\e[27;6;37~"  [?\C-%])
    (define-key map "\e[27;6;38~"  [?\C-&])
    (define-key map "\e[27;6;40~"  [?\C-(])
    (define-key map "\e[27;6;41~"  [?\C-)])
    (define-key map "\e[27;6;42~"  [?\C-*])
    (define-key map "\e[27;6;43~"  [?\C-+])
    (define-key map "\e[27;6;58~"  [?\C-:])
    (define-key map "\e[27;6;60~"  [?\C-<])
    (define-key map "\e[27;6;62~"  [?\C->])
    (define-key map "\e[27;6;63~"  [(control ??)])

    ;; These are the strings emitted for various C-M- combinations
    ;; for keyboards that the Meta and Alt modifiers are on the same
    ;; key (usually labeled "Alt").
    (define-key map "\e[27;13;9~"  [C-M-tab])
    (define-key map "\e[27;13;13~" [C-M-return])

    (define-key map "\e[27;13;39~" [?\C-\M-\'])
    (define-key map "\e[27;13;44~" [?\C-\M-,])
    (define-key map "\e[27;13;45~" [?\C-\M--])
    (define-key map "\e[27;13;46~" [?\C-\M-.])
    (define-key map "\e[27;13;47~" [?\C-\M-/])
    (define-key map "\e[27;13;48~" [?\C-\M-0])
    (define-key map "\e[27;13;49~" [?\C-\M-1])
    (define-key map "\e[27;13;50~" [?\C-\M-2])
    (define-key map "\e[27;13;51~" [?\C-\M-3])
    (define-key map "\e[27;13;52~" [?\C-\M-4])
    (define-key map "\e[27;13;53~" [?\C-\M-5])
    (define-key map "\e[27;13;54~" [?\C-\M-6])
    (define-key map "\e[27;13;55~" [?\C-\M-7])
    (define-key map "\e[27;13;56~" [?\C-\M-8])
    (define-key map "\e[27;13;57~" [?\C-\M-9])
    (define-key map "\e[27;13;59~" [?\C-\M-\;])
    (define-key map "\e[27;13;61~" [?\C-\M-=])
    (define-key map "\e[27;13;92~" [?\C-\M-\\])

    (define-key map "\e[27;14;33~"  [?\C-\M-!])
    (define-key map "\e[27;14;34~"  [?\C-\M-\"])
    (define-key map "\e[27;14;35~"  [?\C-\M-#])
    (define-key map "\e[27;14;36~"  [?\C-\M-$])
    (define-key map "\e[27;14;37~"  [?\C-\M-%])
    (define-key map "\e[27;14;38~"  [?\C-\M-&])
    (define-key map "\e[27;14;40~"  [?\C-\M-\(])
    (define-key map "\e[27;14;41~"  [?\C-\M-\)])
    (define-key map "\e[27;14;42~"  [?\C-\M-*])
    (define-key map "\e[27;14;43~"  [?\C-\M-+])
    (define-key map "\e[27;14;58~"  [?\C-\M-:])
    (define-key map "\e[27;14;60~"  [?\C-\M-<])
    (define-key map "\e[27;14;62~"  [?\C-\M->])
    (define-key map "\e[27;14;63~"  [(control meta ??)])

    (define-key map "\e[27;7;9~"  [C-M-tab])
    (define-key map "\e[27;7;13~" [C-M-return])

    (define-key map "\e[27;7;32~" [?\C-\M-\s])
    (define-key map "\e[27;7;39~" [?\C-\M-\'])
    (define-key map "\e[27;7;44~" [?\C-\M-,])
    (define-key map "\e[27;7;45~" [?\C-\M--])
    (define-key map "\e[27;7;46~" [?\C-\M-.])
    (define-key map "\e[27;7;47~" [?\C-\M-/])
    (define-key map "\e[27;7;48~" [?\C-\M-0])
    (define-key map "\e[27;7;49~" [?\C-\M-1])
    (define-key map "\e[27;7;50~" [?\C-\M-2])
    (define-key map "\e[27;7;51~" [?\C-\M-3])
    (define-key map "\e[27;7;52~" [?\C-\M-4])
    (define-key map "\e[27;7;53~" [?\C-\M-5])
    (define-key map "\e[27;7;54~" [?\C-\M-6])
    (define-key map "\e[27;7;55~" [?\C-\M-7])
    (define-key map "\e[27;7;56~" [?\C-\M-8])
    (define-key map "\e[27;7;57~" [?\C-\M-9])
    (define-key map "\e[27;7;59~" [?\C-\M-\;])
    (define-key map "\e[27;7;61~" [?\C-\M-=])
    (define-key map "\e[27;7;92~" [?\C-\M-\\])

    (define-key map "\e[27;8;33~"  [?\C-\M-!])
    (define-key map "\e[27;8;34~"  [?\C-\M-\"])
    (define-key map "\e[27;8;35~"  [?\C-\M-#])
    (define-key map "\e[27;8;36~"  [?\C-\M-$])
    (define-key map "\e[27;8;37~"  [?\C-\M-%])
    (define-key map "\e[27;8;38~"  [?\C-\M-&])
    (define-key map "\e[27;8;40~"  [?\C-\M-\(])
    (define-key map "\e[27;8;41~"  [?\C-\M-\)])
    (define-key map "\e[27;8;42~"  [?\C-\M-*])
    (define-key map "\e[27;8;43~"  [?\C-\M-+])
    (define-key map "\e[27;8;58~"  [?\C-\M-:])
    (define-key map "\e[27;8;60~"  [?\C-\M-<])
    (define-key map "\e[27;8;62~"  [?\C-\M->])
    (define-key map "\e[27;8;63~"  [(control meta ??)])

    (define-key map "\e[27;2;9~"   [S-tab])
    (define-key map "\e[27;2;13~"  [S-return])

    (define-key map "\e[27;6;9~"   [C-S-tab])
    (define-key map "\e[27;6;13~"  [C-S-return])

    ;; Other versions of xterm might emit these.
    (define-key map "\e[A" [up])
    (define-key map "\e[B" [down])
    (define-key map "\e[C" [right])
    (define-key map "\e[D" [left])
    (define-key map "\e[1~" [home])

    (define-key map "\eO2A" [S-up])
    (define-key map "\eO2B" [S-down])
    (define-key map "\eO2C" [S-right])
    (define-key map "\eO2D" [S-left])
    (define-key map "\eO2F" [S-end])
    (define-key map "\eO2H" [S-home])

    (define-key map "\eO5A" [C-up])
    (define-key map "\eO5B" [C-down])
    (define-key map "\eO5C" [C-right])
    (define-key map "\eO5D" [C-left])
    (define-key map "\eO5F" [C-end])
    (define-key map "\eO5H" [C-home])

    (define-key map "\e[11~" [f1])
    (define-key map "\e[12~" [f2])
    (define-key map "\e[13~" [f3])
    (define-key map "\e[14~" [f4])
    map)
  "Function key map overrides for xterm.")

(defvar xterm-alternatives-map
  (let ((map (make-sparse-keymap)))
    ;; The terminal initialization C code file might have initialized
    ;; function keys F13->F60 from the termcap/terminfo information.
    ;; On a PC-style keyboard these keys correspond to
    ;; MODIFIER-FUNCTION_KEY, where modifier is S-, C, A-, C-S-.  The code
    ;; here substitutes the corresponding definitions in function-key-map.
    ;; The mapping from escape sequences to Fn is done in input-decode-map
    ;; whereas this here mapping is done in local-function-key-map so that
    ;; bindings to f45 still work, in case your keyboard really has an f45
    ;; key rather than C-S-f9.
    (define-key map [f13] [S-f1])
    (define-key map [f14] [S-f2])
    (define-key map [f15] [S-f3])
    (define-key map [f16] [S-f4])
    (define-key map [f17] [S-f5])
    (define-key map [f18] [S-f6])
    (define-key map [f19] [S-f7])
    (define-key map [f20] [S-f8])
    (define-key map [f21] [S-f9])
    (define-key map [f22] [S-f10])
    (define-key map [f23] [S-f11])
    (define-key map [f24] [S-f12])

    (define-key map [f25] [C-f1])
    (define-key map [f26] [C-f2])
    (define-key map [f27] [C-f3])
    (define-key map [f28] [C-f4])
    (define-key map [f29] [C-f5])
    (define-key map [f30] [C-f6])
    (define-key map [f31] [C-f7])
    (define-key map [f32] [C-f8])
    (define-key map [f33] [C-f9])
    (define-key map [f34] [C-f10])
    (define-key map [f35] [C-f11])
    (define-key map [f36] [C-f12])

    (define-key map [f37] [C-S-f1])
    (define-key map [f38] [C-S-f2])
    (define-key map [f39] [C-S-f3])
    (define-key map [f40] [C-S-f4])
    (define-key map [f41] [C-S-f5])
    (define-key map [f42] [C-S-f6])
    (define-key map [f43] [C-S-f7])
    (define-key map [f44] [C-S-f8])
    (define-key map [f45] [C-S-f9])
    (define-key map [f46] [C-S-f10])
    (define-key map [f47] [C-S-f11])
    (define-key map [f48] [C-S-f12])

    (define-key map [f49] [M-f1])
    (define-key map [f50] [M-f2])
    (define-key map [f51] [M-f3])
    (define-key map [f52] [M-f4])
    (define-key map [f53] [M-f5])
    (define-key map [f54] [M-f6])
    (define-key map [f55] [M-f7])
    (define-key map [f56] [M-f8])
    (define-key map [f57] [M-f9])
    (define-key map [f58] [M-f10])
    (define-key map [f59] [M-f11])
    (define-key map [f60] [M-f12])

    map)
  "Keymap of possible alternative meanings for some keys.")

;; List of terminals for which modify-other-keys has been turned on.
(defvar xterm-modify-other-keys-terminal-list nil)

(defun terminal-init-xterm ()
  "Terminal initialization function for xterm."
  ;; rxvt terminals sometimes set the TERM variable to "xterm", but
  ;; rxvt's keybindings are incompatible with xterm's. It is
  ;; better in that case to use rxvt's initialization function.
  (if (and (getenv "COLORTERM" (selected-frame))
	   (string-match "\\`rxvt" (getenv "COLORTERM" (selected-frame))))
      (tty-run-terminal-initialization (selected-frame) "rxvt")

    (let ((map (copy-keymap xterm-alternatives-map)))
      (set-keymap-parent map (keymap-parent local-function-key-map))
      (set-keymap-parent local-function-key-map map))

    (let ((map (copy-keymap xterm-function-map)))

      ;; Use inheritance to let the main keymap override those defaults.
      ;; This way we don't override terminfo-derived settings or settings
      ;; made in the .emacs file.
      (set-keymap-parent map (keymap-parent input-decode-map))
      (set-keymap-parent input-decode-map map)))

  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces)

  ;; Try to turn on the modifyOtherKeys feature on modern xterms.
  ;; When it is turned on many more key bindings work: things like
  ;; C-. C-, etc.
  ;; To do that we need to find out if the current terminal supports
  ;; modifyOtherKeys.  At this time only xterm does.
  (when xterm-extra-capabilities
    (let ((coding-system-for-read 'binary)
          (chr nil)
          (str "")
          (recompute-faces nil)
          ;; If `xterm-extra-capabilities' is 'check, we don't know
          ;; the capabilities.  We need to check for those defined
          ;; as `xterm-extra-capabilities' set options.  Otherwise,
          ;; we don't need to check for any capabilities because
          ;; they are given by setting `xterm-extra-capabilities' to
          ;; a list (which could be empty).
          (tocheck-capabilities (if (eq 'check xterm-extra-capabilities)
                                    '(modifyOtherKeys reportBackground)))
          ;; The given capabilities are either the contents of
          ;; `xterm-extra-capabilities', if it's a list, or an empty list.
          (given-capabilities (if (consp xterm-extra-capabilities)
                                  xterm-extra-capabilities))
          version)
      ;; 1. Set `version'

      ;; Pending input can be mistakenly returned by the calls to
      ;; read-event below.  Discard it.
      (discard-input)
      ;; Try to find out the type of terminal by sending a "Secondary
      ;; Device Attributes (DA)" query.
      (send-string-to-terminal "\e[>0c")

      ;; The reply should be: \e [ > NUMBER1 ; NUMBER2 ; NUMBER3 c
      ;; If the timeout is completely removed for read-event, this
      ;; might hang for terminals that pretend to be xterm, but don't
      ;; respond to this escape sequence.  RMS' opinion was to remove
      ;; it completely.  That might be right, but let's first try to
      ;; see if by using a longer timeout we get rid of most issues.
      (when (and (equal (read-event nil nil 2) ?\e)
                 (equal (read-event nil nil 2) ?\[))
        (while (not (equal (setq chr (read-event nil nil 2)) ?c))
          (setq str (concat str (string chr))))
        (if (string-match ">0;\\([0-9]+\\);0" str)
            (setq version (string-to-number (match-string 1 str)))))
      ;; 2. If reportBackground is known to be supported, or the
      ;; version is 242 or higher, assume the xterm supports
      ;; reporting the background color (TODO: maybe earlier
      ;; versions do too...)
      (when (or (memq 'reportBackground given-capabilities)
                (and (memq 'reportBackground tocheck-capabilities)
                     version
                     (>= version 242)))
        (send-string-to-terminal "\e]11;?\e\\")
        (when (and (equal (read-event nil nil 2) ?\e)
                   (equal (read-event nil nil 2) ?\]))
          (setq str "")
          (while (not (equal (setq chr (read-event nil nil 2)) ?\\))
            (setq str (concat str (string chr))))
          (if (string-match
               "11;rgb:\\([a-f0-9]+\\)/\\([a-f0-9]+\\)/\\([a-f0-9]+\\)" str)
              (setq recompute-faces
                    (xterm-maybe-set-dark-background-mode
                     (string-to-number (match-string 1 str) 16)
                     (string-to-number (match-string 2 str) 16)
                     (string-to-number (match-string 3 str) 16))))))

      ;; 3. If modifyOtherKeys is known to be supported or the
      ;; version is 216 (the version when modifyOtherKeys was
      ;; introduced) or higher, initialize the modifyOtherKeys support.
      (if (or (memq 'modifyOtherKeys given-capabilities)
              (and (memq 'modifyOtherKeys tocheck-capabilities)
                   version
                   (>= version 216)))
          (terminal-init-xterm-modify-other-keys))

      ;; Recompute faces here in case the background mode was
      ;; set to dark.  We used to call
      ;; `tty-set-up-initial-frame-faces' only once, but that
      ;; caused the light background faces to be computed
      ;; incorrectly.  See:
      ;; http://permalink.gmane.org/gmane.emacs.devel/119627
      (when recompute-faces
        (tty-set-up-initial-frame-faces))))

    (run-hooks 'terminal-init-xterm-hook))

(defun terminal-init-xterm-modify-other-keys ()
  "Terminal initialization for xterm's modifyOtherKeys support."
  ;; Make sure that the modifyOtherKeys state is restored when
  ;; suspending, resuming and exiting.
  (add-hook 'suspend-hook 'xterm-turn-off-modify-other-keys)
  (add-hook 'suspend-resume-hook 'xterm-turn-on-modify-other-keys)
  (add-hook 'kill-emacs-hook 'xterm-remove-modify-other-keys)
  (add-hook 'delete-terminal-functions 'xterm-remove-modify-other-keys)
  ;; Add the selected frame to the list of frames that
  ;; need to deal with modify-other-keys.
  (push (frame-terminal (selected-frame))
      xterm-modify-other-keys-terminal-list)
  (xterm-turn-on-modify-other-keys))

;; Set up colors, for those versions of xterm that support it.
(defvar xterm-standard-colors
  ;; The names in the comments taken from XTerm-col.ad in the xterm
  ;; distribution, see ftp://dickey.his.com/xterm/.  RGB values are
  ;; from rgb.txt.
  '(("black"          0 (  0   0   0))	; black
    ("red"            1 (205   0   0))	; red3
    ("green"          2 (  0 205   0))	; green3
    ("yellow"         3 (205 205   0))	; yellow3
    ("blue"           4 (  0   0 238))	; blue2
    ("magenta"        5 (205   0 205))	; magenta3
    ("cyan"           6 (  0 205 205))	; cyan3
    ("white"          7 (229 229 229))	; gray90
    ("brightblack"    8 (127 127 127))	; gray50
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (92   92 255))	; rgb:5c/5c/ff
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard xterm/aixterm colors, their numbers, and RGB values.")

(defun xterm-rgb-convert-to-16bit (prim)
  "Convert an 8-bit primary color value PRIM to a corresponding 16-bit value."
  (logior prim (lsh prim 8)))

(defun xterm-register-default-colors ()
  "Register the default set of colors for xterm or compatible emulator.

This function registers the number of colors returned by `display-color-cells'
for the currently selected frame.  The first 16 colors are taken from
`xterm-standard-colors', which see, while the rest are computed assuming
either the 88- or 256-color standard color scheme supported by latest
versions of xterm."
  (let* ((ncolors (display-color-cells (selected-frame)))
	 (colors xterm-standard-colors)
	 (color (car colors)))
    (if (> ncolors 0)
	;; Clear the 8 default tty colors registered by startup.el
	(tty-color-clear))
    ;; Only register as many colors as are supported by the display.
    (while (and (> ncolors 0) colors)
      (tty-color-define (car color) (cadr color)
			(mapcar 'xterm-rgb-convert-to-16bit
				(car (cddr color))))
      (setq colors (cdr colors)
	    color (car colors)
	    ncolors (1- ncolors)))
    ;; We've exhausted the colors from `xterm-standard-colors'.  If there
    ;; are more colors to support, compute them now.
    (when (> ncolors 0)
      (cond
       ((= ncolors 240)	; 256-color xterm
	;; 216 non-gray colors first
	(let ((r 0) (g 0) (b 0))
	  (while (> ncolors 24)
	    ;; This and other formulas taken from 256colres.pl and
	    ;; 88colres.pl in the xterm distribution.
	    (tty-color-define (format "color-%d" (- 256 ncolors))
			      (- 256 ncolors)
			      (mapcar 'xterm-rgb-convert-to-16bit
				      (list (if (zerop r) 0 (+ (* r 40) 55))
					    (if (zerop g) 0 (+ (* g 40) 55))
					    (if (zerop b) 0 (+ (* b 40) 55)))))

	    (setq b (1+ b))
	    (if (> b 5)
		(setq g (1+ g)
		      b 0))
	    (if (> g 5)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 24 gray colors
	(while (> ncolors 0)
	  (setq color (xterm-rgb-convert-to-16bit (+ 8 (* (- 24 ncolors) 10))))
	  (tty-color-define (format "color-%d" (- 256 ncolors))
			    (- 256 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       ((= ncolors 72)  ; 88-color xterm
	;; 64 non-gray colors
	(let ((levels '(0 139 205 255))
	      (r 0) (g 0) (b 0))
	  (while (> ncolors 8)
	    (tty-color-define (format "color-%d" (- 88 ncolors))
			      (- 88 ncolors)
			      (mapcar 'xterm-rgb-convert-to-16bit
				      (list (nth r levels)
					    (nth g levels)
					    (nth b levels))))
	    (setq b (1+ b))
	    (if (> b 3)
		(setq g (1+ g)
		      b 0))
	    (if (> g 3)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 8 gray colors
	(while (> ncolors 0)
	  (setq color (xterm-rgb-convert-to-16bit
		       (floor
			(if (= ncolors 8)
			    46.36363636
			  (+ (* (- 8 ncolors) 23.18181818) 69.54545454)))))
	  (tty-color-define (format "color-%d" (- 88 ncolors))
			    (- 88 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       (t (error "Unsupported number of xterm colors (%d)" (+ 16 ncolors)))))
    ;; Modifying color mappings means realized faces don't use the
    ;; right colors, so clear them.
    (clear-face-cache)))

(defun xterm-turn-on-modify-other-keys ()
  "Turn the modifyOtherKeys feature of xterm back on."
  (let ((terminal (frame-terminal (selected-frame))))
    (when (and (terminal-live-p terminal)
	       (memq terminal xterm-modify-other-keys-terminal-list))
      (send-string-to-terminal "\e[>4;1m" terminal))))

(defun xterm-turn-off-modify-other-keys (&optional frame)
  "Temporarily turn off the modifyOtherKeys feature of xterm."
  (let ((terminal (when frame (frame-terminal frame))))
    (when (and (terminal-live-p terminal)
               (memq terminal xterm-modify-other-keys-terminal-list))
      (send-string-to-terminal "\e[>4m" terminal))))

(defun xterm-remove-modify-other-keys (&optional terminal)
  "Turn off the modifyOtherKeys feature of xterm for good."
  (setq terminal (or terminal (frame-terminal (selected-frame))))
  (when (and (terminal-live-p terminal)
	     (memq terminal xterm-modify-other-keys-terminal-list))
    (setq xterm-modify-other-keys-terminal-list
	  (delq terminal xterm-modify-other-keys-terminal-list))
    (send-string-to-terminal "\e[>4m" terminal)))

(defun xterm-maybe-set-dark-background-mode (redc greenc bluec)
  ;; Use the heuristic in `frame-set-background-mode' to decide if a
  ;; frame is dark.
  (when (< (+ redc greenc bluec) (* .6 (+ 65535 65535 65535)))
    (set-terminal-parameter nil 'background-mode 'dark)
    t))

;;; xterm.el ends here
