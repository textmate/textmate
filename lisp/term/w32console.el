;;; w32console.el -- Setup w32 console keys and colors.

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

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

;; W32 uses different color indexes than standard:

(defvar w32-tty-standard-colors
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
"A list of VGA console colors, their indices and 16-bit RGB values.")

(declare-function x-setup-function-keys "term/common-win" (frame))
(declare-function get-screen-color "w32console.c" ())

(defun terminal-init-w32console ()
  "Terminal initialization function for w32 console."
  ;; Share function key initialization with w32 gui frames
  (x-setup-function-keys (selected-frame))
  (let* ((colors w32-tty-standard-colors)
         (color (car colors)))
    (tty-color-clear)
    (while colors
      (tty-color-define (car color) (cadr color) (cddr color))
      (setq colors (cdr colors)
            color (car colors))))
  (clear-face-cache)
  ;; Figure out what are the colors of the console window, and set up
  ;; the background-mode correspondingly.
  (let* ((screen-color (get-screen-color))
	 (bg (cadr screen-color))
	 (descr (tty-color-by-index bg))
	 r g b bg-mode)
    (setq r (nth 2 descr)
	  g (nth 3 descr)
	  b (nth 4 descr))
    (if (< (+ r g b) (* .6 (+ 65535 65535 65535)))
	(setq bg-mode 'dark)
      (setq bg-mode 'light))
    (set-terminal-parameter nil 'background-mode bg-mode))
  (tty-set-up-initial-frame-faces)
  (run-hooks 'terminal-init-w32-hook))

;;; w32console.el ends here
