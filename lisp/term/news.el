;;; news.el --- keypad and function key bindings for the Sony NEWS keyboard -*- no-byte-compile: t -*-

;; Copyright (C) 1989, 1993, 2001-2012 Free Software Foundation, Inc.

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

;; Uses the Emacs 19 terminal initialization features --- won't work with 18.

;;; Code:

(defun terminal-init-news ()
  "Terminal initialization function for news."
  ;; The terminal initialization should already have set up some keys
  (let ((news-fkey-prefix (lookup-key input-decode-map "\eO")))
    (if (not (keymapp news-fkey-prefix))
	(error "What?  Your news termcap/terminfo has no keycaps in it"))

    ;; Termcap or terminfo will set these
    ;; (define-key news-fkey-prefix "P" [f1])
    ;; (define-key news-fkey-prefix "Q" [f2])
    ;; (define-key news-fkey-prefix "R" [f3])
    ;; (define-key news-fkey-prefix "S" [f4])
    ;; (define-key news-fkey-prefix "T" [f5])
    ;; (define-key news-fkey-prefix "U" [f6])
    ;; (define-key news-fkey-prefix "V" [f7])
    ;; (define-key news-fkey-prefix "W" [f8])
    ;; (define-key news-fkey-prefix "X" [f9])
    ;; (define-key news-fkey-prefix "Y" [f10])

    ;; Terminfo will set these
    (define-key news-fkey-prefix "a" [execute])
    (define-key news-fkey-prefix "b" [select])
    (define-key news-fkey-prefix "c" [cancel])
    (define-key news-fkey-prefix "M" [kp-enter])
    (define-key news-fkey-prefix "q" [kp-1])
    (define-key news-fkey-prefix "s" [kp-3])
    (define-key news-fkey-prefix "u" [kp-5])
    (define-key news-fkey-prefix "w" [kp-7])
    (define-key news-fkey-prefix "y" [kp-9])

    ;; These aren't in either termcap or terminfo's repertoire
    (define-key news-fkey-prefix "m" [kp-subtract])
    (define-key news-fkey-prefix "k" [kp-add])
    (define-key news-fkey-prefix "l" [kp-separator])
    (define-key news-fkey-prefix "n" [kp-decimal])
    (define-key news-fkey-prefix "p" [kp-0])
    (define-key news-fkey-prefix "r" [kp-2])
    (define-key news-fkey-prefix "t" [kp-4])
    (define-key news-fkey-prefix "v" [kp-6])
    (define-key news-fkey-prefix "x" [kp-8])
    ))

;;; news.el ends here
