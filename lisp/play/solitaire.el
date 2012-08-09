;;; solitaire.el --- game of solitaire in Emacs Lisp

;; Copyright (C) 1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Jan Schormann <Jan.Schormann@rechen-gilde.de>
;; Created: Fri afternoon, Jun  3,  1994
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

;; This mode is for playing a well-known game of solitaire
;; in which you jump pegs across other pegs.

;; The game itself is somehow self-explanatory.  Read the help text to
;; solitaire, and try it.

;;; Code:

(defgroup solitaire nil
  "Game of Solitaire."
  :prefix "solitaire-"
  :group 'games)

(defcustom solitaire-mode-hook nil
  "Hook to run upon entry to Solitaire."
  :type 'hook
  :group 'solitaire)

(defvar solitaire-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)

    (define-key map "\C-f" 'solitaire-right)
    (define-key map "\C-b" 'solitaire-left)
    (define-key map "\C-p" 'solitaire-up)
    (define-key map "\C-n" 'solitaire-down)
    (define-key map "\r" 'solitaire-move)
    (define-key map [remap undo] 'solitaire-undo)
    (define-key map " " 'solitaire-do-check)

    (define-key map [right] 'solitaire-right)
    (define-key map [left] 'solitaire-left)
    (define-key map [up] 'solitaire-up)
    (define-key map [down] 'solitaire-down)

    (define-key map [S-right] 'solitaire-move-right)
    (define-key map [S-left]  'solitaire-move-left)
    (define-key map [S-up]    'solitaire-move-up)
    (define-key map [S-down]  'solitaire-move-down)

    (define-key map [kp-6] 'solitaire-right)
    (define-key map [kp-4] 'solitaire-left)
    (define-key map [kp-8] 'solitaire-up)
    (define-key map [kp-2] 'solitaire-down)
    (define-key map [kp-5] 'solitaire-center-point)

    (define-key map [S-kp-6] 'solitaire-move-right)
    (define-key map [S-kp-4] 'solitaire-move-left)
    (define-key map [S-kp-8] 'solitaire-move-up)
    (define-key map [S-kp-2] 'solitaire-move-down)

    (define-key map [kp-enter] 'solitaire-move)
    (define-key map [kp-0] 'solitaire-undo)

    ;; spoil it with s ;)
    (define-key map [?s] 'solitaire-solve)

    ;;  (define-key map [kp-0] 'solitaire-hint) - Not yet provided ;)
    map)
  "Keymap for playing Solitaire.")

;; Solitaire mode is suitable only for specially formatted data.
(put 'solitaire-mode 'mode-class 'special)

(define-derived-mode solitaire-mode special-mode "Solitaire"
  "Major mode for playing Solitaire.
To learn how to play Solitaire, see the documentation for function
`solitaire'.
\\<solitaire-mode-map>
The usual mnemonic keys move the cursor around the board; in addition,
\\[solitaire-move] is a prefix character for actually moving a stone on the board."
  (setq truncate-lines t)
  (setq show-trailing-whitespace nil))

(defvar solitaire-stones 0
  "Counter for the stones that are still there.")

(defvar solitaire-center nil
  "Center of the board.")

(defvar solitaire-start nil
  "Upper left corner of the board.")

(defvar solitaire-start-x nil)
(defvar solitaire-start-y nil)

(defvar solitaire-end nil
  "Lower right corner of the board.")

(defvar solitaire-end-x nil)
(defvar solitaire-end-y nil)

(defcustom solitaire-auto-eval t
  "Non-nil means check for possible moves after each major change.
This takes a while, so switch this on if you like to be informed when
the game is over, or off, if you are working on a slow machine."
  :type 'boolean
  :group 'solitaire)

(defconst solitaire-valid-directions
  '(solitaire-left solitaire-right solitaire-up solitaire-down))

;;;###autoload
(defun solitaire (_arg)
  "Play Solitaire.

To play Solitaire, type \\[solitaire].
\\<solitaire-mode-map>
Move around the board using the cursor keys.
Move stones using \\[solitaire-move] followed by a direction key.
Undo moves using \\[solitaire-undo].
Check for possible moves using \\[solitaire-do-check].
\(The variable `solitaire-auto-eval' controls whether to automatically
check after each move or undo.)

What is Solitaire?

I don't know who invented this game, but it seems to be rather old and
its origin seems to be northern Africa.  Here's how to play:
Initially, the board will look similar to this:

	Le Solitaire
	============

		o   o   o

		o   o   o

	o   o   o   o   o   o   o

	o   o   o   .   o   o   o

	o   o   o   o   o   o   o

		o   o   o

		o   o   o

Let's call the o's stones and the .'s holes.  One stone fits into one
hole.  As you can see, all holes but one are occupied by stones.  The
aim of the game is to get rid of all but one stone, leaving that last
one in the middle of the board if you're cool.

A stone can be moved if there is another stone next to it, and a hole
after that one.  Thus there must be three fields in a row, either
horizontally or vertically, up, down, left or right, which look like
this:  o  o  .

Then the first stone is moved to the hole, jumping over the second,
which therefore is taken away.  The above thus `evaluates' to:  .  .  o

That's all.  Here's the board after two moves:

		o   o   o

		.   o   o

	o   o   .   o   o   o   o

	o   .   o   o   o   o   o

	o   o   o   o   o   o   o

		o   o   o

		o   o   o

Pick your favorite shortcuts:

\\{solitaire-mode-map}"

  (interactive "P")
  (switch-to-buffer "*Solitaire*")
  (let ((inhibit-read-only t))
    (solitaire-mode)
    (setq buffer-read-only t)
    (setq solitaire-stones 32)
    (solitaire-insert-board)
    (solitaire-build-modeline)
    (goto-char (point-max))
    (setq solitaire-center (search-backward "."))
    (setq buffer-undo-list (list (point)))))

(defun solitaire-build-modeline ()
  (setq mode-line-format
	(list "" "---" 'mode-line-buffer-identification
	      (if (< 1 solitaire-stones)
		  (format "--> There are %d stones left <--" solitaire-stones)
		"------")
	      'global-mode-string "   %[(" 'mode-name 'minor-mode-alist "%n"
	      ")%]-%-"))
  (force-mode-line-update))

(defun solitaire-insert-board ()
  (let* ((buffer-read-only nil)
	 (w (window-width))
	 (h (window-height))
	 (hsep (cond ((> w 26) "   ")
		     ((> w 20) " ")
		     (t "")))
	 (vsep (cond ((> h 17) "\n\n")
		     (t "\n")))
	 (indent (make-string (/ (- w 7 (* 6 (length hsep))) 2) ?\s)))
    (erase-buffer)
    (insert (make-string (/ (- h 7 (if (> h 12) 3 0)
			       (* 6 (1- (length vsep)))) 2) ?\n))
    (when (or (string= vsep "\n\n") (> h 12))
      (insert (format "%sLe Solitaire\n" indent))
      (insert (format "%s============\n\n" indent)))
    (insert indent)
    (setq solitaire-start (point))
    (setq solitaire-start-x (current-column))
    (setq solitaire-start-y (solitaire-current-line))
    (insert (format " %s %so%so%so%s" hsep hsep hsep hsep vsep))
    (insert (format "%s %s %so%so%so%s" indent hsep hsep hsep hsep vsep))
    (insert (format "%so%so%so%so%so%so%so%s" indent hsep hsep hsep hsep hsep hsep vsep))
    (insert (format "%so%so%so%s" indent hsep hsep hsep))
    (setq solitaire-center (point))
    (insert (format ".%so%so%so%s" hsep hsep hsep vsep))
    (insert (format "%so%so%so%so%so%so%so%s" indent hsep hsep hsep hsep hsep hsep vsep))
    (insert (format "%s %s %so%so%so%s" indent hsep hsep hsep hsep vsep))
    (insert (format "%s %s %so%so%so%s %s " indent hsep hsep hsep hsep hsep hsep))
    (setq solitaire-end (point))
    (setq solitaire-end-x (current-column))
    (setq solitaire-end-y (solitaire-current-line))))

(defun solitaire-right ()
  (interactive)
  (let ((start (point)))
    (forward-char)
    (while (= ?\s (following-char))
      (forward-char))
    (when (or (= 0 (following-char))
	      (= ?\s (following-char))
	      (= ?\n (following-char)))
      (goto-char start))))

(defun solitaire-left ()
  (interactive)
  (let ((start (point)))
    (backward-char)
    (while (= ?\s (following-char))
      (backward-char))
    (when (or (= 0 (preceding-char))
	      (= ?\s (following-char))
	      (= ?\n (following-char)))
      (goto-char start))))

(defun solitaire-up ()
  (interactive)
  (let ((start (point))
	(c (current-column)))
    (forward-line -1)
    (move-to-column c)
    (while (and (= ?\n (following-char))
		(forward-line -1)
		(move-to-column c)
		(not (bolp))))
    (when (or (= 0 (preceding-char))
	      (= ?\s (following-char))
	      (= ?\= (following-char))
	      (= ?\n (following-char)))
      (goto-char start))))

(defun solitaire-down ()
  (interactive)
  (let ((start (point))
	(c (current-column)))
    (forward-line 1)
    (move-to-column c)
    (while (and (= ?\n (following-char))
		(forward-line 1)
		(move-to-column c)
		(not (eolp))))
    (when (or (= 0 (following-char))
	      (= ?\s (following-char))
	      (= ?\n (following-char)))
      (goto-char start))))

(defun solitaire-center-point ()
  (interactive)
  (goto-char solitaire-center))

(defun solitaire-move-right () (interactive) (solitaire-move '[right]))
(defun solitaire-move-left () (interactive) (solitaire-move '[left]))
(defun solitaire-move-up () (interactive) (solitaire-move '[up]))
(defun solitaire-move-down () (interactive) (solitaire-move '[down]))

(defun solitaire-possible-move (movesymbol)
  "Check if a move is possible from current point in the specified direction.
MOVESYMBOL specifies the direction.
Returns either a string, indicating cause of contraindication, or a
list containing three numbers: starting field, skipped field (from
which a stone will be taken away) and target."

  (save-excursion
    (if (memq movesymbol solitaire-valid-directions)
	(let ((start (point))
	      (skip (progn (funcall movesymbol) (point)))
	      (target (progn (funcall movesymbol) (point))))
	  (if (= skip target)
	      "Off Board!"
	    (if (or (/= ?o (char-after start))
		    (/= ?o (char-after skip))
		    (/= ?. (char-after target)))
		"Wrong move!"
	      (list start skip target))))
      "Not a valid direction")))

(defun solitaire-move (dir)
  "Pseudo-prefix command to move a stone in Solitaire."
  (interactive "kMove where? ")
  (let* ((class (solitaire-possible-move (lookup-key solitaire-mode-map dir)))
	 (buffer-read-only nil))
    (if (stringp class)
	(error class)
      (let ((start (car class))
	    (skip (car (cdr class)))
	    (target (car (cdr (cdr class)))))
	(goto-char start)
	(delete-char 1)
	(insert ?.)
	(goto-char skip)
	(delete-char 1)
	(insert ?.)
	(goto-char target)
	(delete-char 1)
	(insert ?o)
	(goto-char target)
	(setq solitaire-stones (1- solitaire-stones))
	(solitaire-build-modeline)
	(if solitaire-auto-eval (solitaire-do-check))))))

(defun solitaire-undo (arg)
  "Undo a move in Solitaire."
  (interactive "P")
  (let ((buffer-read-only nil))
    (undo arg))
  (save-excursion
    (setq solitaire-stones
	  (let ((count 0))
	    (goto-char solitaire-end)
	    (while (search-backward "o" solitaire-start 'done)
	      (and (>= (current-column) solitaire-start-x)
		   (<= (current-column) solitaire-end-x)
		   (>= (solitaire-current-line) solitaire-start-y)
		   (<= (solitaire-current-line) solitaire-end-y)
		   (setq count (1+ count))))
	    count)))
  (solitaire-build-modeline)
  (when solitaire-auto-eval (solitaire-do-check)))

(defun solitaire-check ()
  (save-excursion
    (if (= 1 solitaire-stones)
	0
      (goto-char solitaire-end)
      (let ((count 0))
	(while (search-backward "o" solitaire-start 'done)
	  (and (>= (current-column) solitaire-start-x)
	       (<= (current-column) solitaire-end-x)
	       (>= (solitaire-current-line) solitaire-start-y)
	       (<= (solitaire-current-line) solitaire-end-y)
	       (mapc
		(lambda (movesymbol)
		  (when (listp (solitaire-possible-move movesymbol))
		    (setq count (1+ count))))
		solitaire-valid-directions)))
	count))))

(defun solitaire-do-check (&optional _arg)
  "Check for any possible moves in Solitaire."
  (interactive "P")
  (let ((moves (solitaire-check)))
    (cond ((= 1 solitaire-stones)
	   (message "Yeah! You made it! Only the King is left!"))
	  ((zerop moves)
	   (message "Sorry, no more possible moves."))
	  ((= 1 moves)
	   (message "There is one possible move."))
	  (t (message "There are %d possible moves." moves)))))

(defun solitaire-current-line ()
  "Return the vertical position of point.
Seen in info on text lines."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)
     -1))

;; And here's the spoiler:)
(defun solitaire-solve ()
  "Spoil Solitaire by solving the game for you - nearly ...
... stops with five stones left ;)"
  (interactive)
  (when (< solitaire-stones 32)
    (error "Cannot solve game in progress"))
  (let ((allmoves [up up S-down up left left S-right up up left S-down
		      up up right right S-left down down down S-up up
		      S-down down down down S-up left left down
		      S-right left left up up S-down right right right
		      S-left left S-right right right right S-left
		      right down down S-up down down left left S-right
		      up up up S-down down S-up up up up S-down up
		      right right S-left down right right down S-up
		      left left left S-right right S-left down down
		      left S-right S-up S-left S-left S-down S-right
		      up S-right left left])
	;; down down S-up left S-right
	;; right S-left
	(solitaire-auto-eval nil))
    (solitaire-center-point)
    (mapc (lambda (op)
	    (when (memq op '(S-left S-right S-up S-down))
	      (sit-for 0.2))
	    (execute-kbd-macro (vector op))
	    (when (memq op '(S-left S-right S-up S-down))
	      (sit-for 0.4)))
	  allmoves))
  (solitaire-do-check))

(provide 'solitaire)

;;; solitaire.el ends here
