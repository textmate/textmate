;;; life.el --- John Horton Conway's `Life' game for GNU Emacs

;; Copyright (C) 1988, 2001-2012  Free Software Foundation, Inc.

;; Author: Kyle Jones <kyleuunet.uu.net>
;; Maintainer: FSF
;; Keywords: games

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

;; A demonstrator for John Horton Conway's "Life" cellular automaton
;; in Emacs Lisp.  Picks a random one of a set of interesting Life
;; patterns and evolves it according to the familiar rules.

;;; Code:

(defvar life-patterns
  [("@@@" " @@" "@@@")
   ("@@@ @@@" "@@  @@ " "@@@ @@@")
   ("@@@ @@@" "@@   @@" "@@@ @@@")
   ("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
   ("@@@@@@@@@@")
   ("   @@@@@@@@@@       "
    "     @@@@@@@@@@     "
    "       @@@@@@@@@@   "
    "@@@@@@@@@@          "
    "@@@@@@@@@@          ")
   ("@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@")
   ("@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @")
   ("@@               " " @@              " "  @@             "
    "   @@            " "    @@           " "     @@          "
    "      @@         " "       @@        " "        @@       "
    "         @@      " "          @@     " "           @@    "
    "            @@   " "             @@  " "              @@ "
    "               @@")
   ("@@@@@@@@@" "@   @   @" "@ @@@@@ @" "@ @   @ @" "@@@   @@@"
    "@ @   @ @" "@ @@@@@ @" "@   @   @" "@@@@@@@@@")
   ("                        @           "
    "                      @ @           "
    "            @@      @@            @@"
    "           @   @    @@            @@"
    "@@        @     @   @@              "
    "@@        @   @ @@    @ @           "
    "          @     @       @           "
    "           @   @                    "
    "            @@                      ")
   ("      @ "
    "    @ @@"
    "    @ @ "
    "    @   "
    "  @     "
    "@ @     ")
   ("@@@ @"
    "@    "
    "   @@"
    " @@ @"
    "@ @ @")
   ("@@@@@@@@ @@@@@   @@@      @@@@@@@ @@@@@")]
  "Vector of rectangles containing some Life startup patterns.")

;; Macros are used macros for manifest constants instead of variables
;; because the compiler will convert them to constants, which should
;; eval faster than symbols.
;;
;; Don't change any of the life-* macro constants unless you thoroughly
;; understand the `life-grim-reaper' function.

(defmacro life-life-char () ?@)
(defmacro life-death-char () (1+ (life-life-char)))
(defmacro life-birth-char () 3)
(defmacro life-void-char () ?\ )

(defmacro life-life-string () (char-to-string (life-life-char)))
(defmacro life-death-string () (char-to-string (life-death-char)))
(defmacro life-birth-string () (char-to-string (life-birth-char)))
(defmacro life-void-string () (char-to-string (life-void-char)))
(defmacro life-not-void-regexp () (concat "[^" (life-void-string) "\n]"))

(defmacro life-increment (variable) (list 'setq variable (list '1+ variable)))


;; list of numbers that tell how many characters to move to get to
;; each of a cell's eight neighbors.
(defvar life-neighbor-deltas nil)

;; window display always starts here.  Easier to deal with than
;; (scroll-up) and (scroll-down) when trying to center the display.
(defvar life-window-start nil)

;; For mode line
(defvar life-current-generation nil)
;; Sadly, mode-line-format won't display numbers.
(defvar life-generation-string nil)

(defvar life-initialized nil
  "Non-nil if `life' has been run at least once.")

;;;###autoload
(defun life (&optional sleeptime)
  "Run Conway's Life simulation.
The starting pattern is randomly selected.  Prefix arg (optional first
arg non-nil from a program) is the number of seconds to sleep between
generations (this defaults to 1)."
  (interactive "p")
  (or life-initialized
      (random t))
  (setq life-initialized t)
  (or sleeptime (setq sleeptime 1))
  (life-setup)
  (catch 'life-exit
    (while t
      (let ((inhibit-quit t))
	(life-display-generation sleeptime)
	(life-grim-reaper)
	(life-expand-plane-if-needed)
	(life-increment-generation)))))

(defalias 'life-mode 'life)
(put 'life-mode 'mode-class 'special)

(defun life-setup ()
  (let (n)
    (switch-to-buffer (get-buffer-create "*Life*") t)
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search nil
	  mode-name "Life"
	  major-mode 'life-mode
	  truncate-lines t
          show-trailing-whitespace nil
	  life-current-generation 0
	  life-generation-string "0"
	  mode-line-buffer-identification '("Life: generation "
					    life-generation-string)
	  fill-column (1- (window-width))
	  life-window-start 1)
    (buffer-disable-undo (current-buffer))
    ;; stuff in the random pattern
    (life-insert-random-pattern)
    ;; make sure (life-life-char) is used throughout
    (goto-char (point-min))
    (while (re-search-forward (life-not-void-regexp) nil t)
      (replace-match (life-life-string) t t))
    ;; center the pattern horizontally
    (goto-char (point-min))
    (setq n (/ (- fill-column (line-end-position)) 2))
    (while (not (eobp))
      (indent-to n)
      (forward-line))
    ;; center the pattern vertically
    (setq n (/ (- (1- (window-height))
		  (count-lines (point-min) (point-max)))
	       2))
    (goto-char (point-min))
    (newline n)
    (goto-char (point-max))
    (newline n)
    ;; pad lines out to fill-column
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (indent-to fill-column)
      (move-to-column fill-column)
      (delete-region (point) (progn (end-of-line) (point)))
      (forward-line))
    ;; expand tabs to spaces
    (untabify (point-min) (point-max))
    ;; before starting be sure the automaton has room to grow
    (life-expand-plane-if-needed)
    ;; compute initial neighbor deltas
    (life-compute-neighbor-deltas)))

(defun life-compute-neighbor-deltas ()
  (setq life-neighbor-deltas
	(list -1 (- fill-column)
	      (- (1+ fill-column)) (- (+ 2 fill-column))
	      1 fill-column (1+ fill-column)
	      (+ 2 fill-column))))

(defun life-insert-random-pattern ()
  (insert-rectangle
   (elt life-patterns (random (length life-patterns))))
  (insert ?\n))

(defun life-increment-generation ()
  (life-increment life-current-generation)
  (setq life-generation-string (int-to-string life-current-generation)))

(defun life-grim-reaper ()
  ;; Clear the match information.  Later we check to see if it
  ;; is still clear, if so then all the cells have died.
  (set-match-data nil)
  (goto-char (point-min))
  ;; For speed declare all local variable outside the loop.
  (let (point char pivot living-neighbors list)
    (while (search-forward (life-life-string) nil t)
      (setq list life-neighbor-deltas
	    living-neighbors 0
	    pivot (1- (point)))
      (while list
	(setq point (+ pivot (car list))
	      char (char-after point))
	(cond ((eq char (life-void-char))
	       (subst-char-in-region point (1+ point)
				     (life-void-char) 1 t))
	      ((< char 3)
	       (subst-char-in-region point (1+ point) char (1+ char) t))
	      ((< char 9)
	       (subst-char-in-region point (1+ point) char 9 t))
	      ((>= char (life-life-char))
	       (life-increment living-neighbors)))
	(setq list (cdr list)))
      (if (memq living-neighbors '(2 3))
	  ()
	(subst-char-in-region pivot (1+ pivot)
			    (life-life-char) (life-death-char) t))))
  (if (null (match-beginning 0))
      (life-extinct-quit))
  (subst-char-in-region 1 (point-max) 9 (life-void-char) t)
  (subst-char-in-region 1 (point-max) 1 (life-void-char) t)
  (subst-char-in-region 1 (point-max) 2 (life-void-char) t)
  (subst-char-in-region 1 (point-max) (life-birth-char) (life-life-char) t)
  (subst-char-in-region 1 (point-max) (life-death-char) (life-void-char) t))

(defun life-expand-plane-if-needed ()
  (catch 'done
    (goto-char (point-min))
    (while (not (eobp))
      ;; check for life at beginning or end of line.  If found at
      ;; either end, expand at both ends,
      (cond ((or (eq (following-char) (life-life-char))
		 (eq (progn (end-of-line) (preceding-char)) (life-life-char)))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (insert (life-void-char))
	       (end-of-line)
	       (insert (life-void-char))
	       (forward-char))
	   (setq fill-column (+ 2 fill-column))
	   (scroll-left 1)
	   (life-compute-neighbor-deltas)
	   (throw 'done t)))
      (forward-line)))
  (goto-char (point-min))
  ;; check for life within the first two lines of the buffer.
  ;; If present insert two lifeless lines at the beginning..
  (cond ((search-forward (life-life-string)
			 (+ (point) fill-column fill-column 2) t)
	 (goto-char (point-min))
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (setq life-window-start (+ life-window-start fill-column 1))))
  (goto-char (point-max))
  ;; check for life within the last two lines of the buffer.
  ;; If present insert two lifeless lines at the end.
  (cond ((search-backward (life-life-string)
			  (- (point) fill-column fill-column 2) t)
	 (goto-char (point-max))
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (setq life-window-start (+ life-window-start fill-column 1)))))

(defun life-display-generation (sleeptime)
  (goto-char life-window-start)
  (recenter 0)

  ;; Redisplay; if the user has hit a key, exit the loop.
  (or (and (sit-for sleeptime) (< 0 sleeptime))
      (not (input-pending-p))
      (throw 'life-exit nil)))

(defun life-extinct-quit ()
  (life-display-generation 0)
  (signal 'life-extinct nil))

(put 'life-extinct 'error-conditions '(life-extinct quit))
(put 'life-extinct 'error-message "All life has perished")

(provide 'life)

;;; life.el ends here
