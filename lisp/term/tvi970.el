;;; tvi970.el --- terminal support for the Televideo 970

;; Copyright (C) 1992, 2001-2012 Free Software Foundation, Inc.

;; Author: Jim Blandy <jimb@occs.cs.oberlin.edu>
;; Keywords: terminals
;; Created: January 1992

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

;; Uses the Emacs 19 terminal initialization features --- won't work with 18.

;;; Code:

(eval-when-compile (require 'cl))

(defvar tvi970-terminal-map
  (let ((map (make-sparse-keymap)))

    ;; Miscellaneous keys
    (dolist (key-binding
             '(;; These are set up by termcap or terminfo
               ;; ("\eOP"	[kp-f1])
               ;; ("\eOQ"	[kp-f2])
               ;; ("\eOR"	[kp-f3])
               ;; ("\eOS"	[kp-f4])

               ;; These might bre set by terminfo.
               ("\e[H"	[home])
               ("\e[Z"	[backtab])
               ("\e[i"	[print])
               ("\e[@"	[insert])
               ("\e[L"	[insertline])
               ("\e[M"	[deleteline])
               ("\e[U"	[next]) ;; actually the `page' key

               ;; These won't be set up by either
               ("\eOm"	[kp-subtract])
               ("\eOl"	[kp-separator])
               ("\eOn"	[kp-decimal])
               ("\eOM"	[kp-enter])

               ;; These won't be set up by either either
               ("\e[K"	[key_eol])	;; Not an X keysym
               ("\e[J"	[key_eos])	;; Not an X keysym
               ("\e[2J"	[key_clear])	;; Not an X keysym
               ("\e[P"	[key_dc])	;; Not an X keysym
               ("\e[g"	[S-tab])	;; Not an X keysym
               ("\e[2N"	[clearentry])	;; Not an X keysym
               ("\e[2K"	[S-clearentry])	;; Not an X keysym
               ("\e[E"	[?\C-j])	;; Not an X keysym
               ("\e[g"	[S-backtab])	;; Not an X keysym
               ("\e[?1i"	[key_sprint]) ;; Not an X keysym
               ("\e[4h"	[key_sic])            ;; Not an X keysym
               ("\e[4l"	[S-delete])           ;; Not an X keysym
               ("\e[Q"	[S-insertline])       ;; Not an X keysym
               ("\e[1Q"	[key_sdl])            ;; Not an X keysym
               ("\e[19l"	[key_seol])   ;; Not an X keysym
               ("\e[19h"	[S-erasepage]) ;; Not an X keysym
               ("\e[V"	[S-page])              ;; Not an X keysym
               ("\eS"	[send])                ;; Not an X keysym
               ("\e5"	[S-send])              ;; Not an X keysym
               ))
      (define-key map (car key-binding) (nth 1 key-binding)))
             

    ;; The numeric keypad keys.
    (dotimes (i 10)
      (define-key map (format "\eO%c" (+ i ?p))
        (vector (intern (format "kp-%d" i)))))
    ;; The numbered function keys.
    (dotimes (i 16)
      (define-key map (format "\e?%c" (+ i ?a))
        (vector (intern (format "f%d" (1+ i)))))
      (define-key map (format "\e?%c" (+ i ?A))
        (vector (intern (format "S-f%d" (1+ i))))))
    map))

(defun terminal-init-tvi970 ()
  "Terminal initialization function for tvi970."
  ;; Use inheritance to let the main keymap override these defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (let ((m (copy-keymap tvi970-terminal-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m))
  (tvi970-set-keypad-mode 1))


;; Should keypad numbers send ordinary digits or distinct escape sequences?
(define-minor-mode tvi970-set-keypad-mode
  "Toggle alternate keypad mode on TVI 970 keypad.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

In ``alternate keypad mode'', the keys send distinct escape
sequences, meaning that they can have their own bindings,
independent of the normal number keys.

When disabled, the terminal enters ``numeric keypad mode'', in
which the keypad's keys act as ordinary digits."
  :variable (terminal-parameter nil 'tvi970-keypad-numeric)
  (send-string-to-terminal
   (if (terminal-parameter nil 'tvi970-keypad-numeric) "\e=" "\e>")))

;;; tvi970.el ends here
