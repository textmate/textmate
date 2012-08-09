;;; animate.el --- make text dance

;; Copyright (C) 2001-2012 Free Software Foundation, Inc.

;; Maintainer: Richard Stallman <rms@gnu.org>
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

;; (animate-string STRING VPOS &optional HPOS)
;; makes the string STRING appear starting at VPOS, HPOS
;; by having each letter swoop into place from random starting position.

;; animate-birthday-present was the first application of this program.

;;; Code:

;;; STRING is the string to be displayed,
;;; and DEST-X, DEST-Y say where on the screen
;;; it should end up.

;;; This function returns a list describing
;;; all the characters and the paths they should take.
;;; Each element has the form
;;;  (CHAR START-Y START-X DEST-Y DEST-X).

;;; The start position of each character is chosen randomly.
;;; The destination is chosen to put it in the right place
;;; in the string when the whole string finally reaches its
;;; specified position.

(defun animate-initialize (string vpos hpos)
  (let ((characters nil))
    (dotimes (i (length string))
      (setq characters
	    (cons (list (aref string i)
			;; Random starting positions.
			(random (window-height))
			(random (1- (window-width)))
			;; All the chars should end up
			;; on the specified line.
			vpos
			;; The Ith character in the string
			;; needs to end up I positions later.
			(+ hpos i))
		  characters)))
    characters))

;;; Display the characters in CHARACTERS,
;;; each one FRACTION of the way from its start to its destination.
;;; If FRACTION is 0, the characters appear in their starting positions.
;;; If FRACTION is 1, the characters appear in their destinations.

(defun animate-step (characters fraction)
  (let ((remains (- 1 fraction)))
    (dolist (item characters)
      (let ((vpos (+ (* remains (nth 1 item))
		     (* fraction (nth 3 item))))
	    (hpos (+ (* remains (nth 2 item))
		     (* fraction (nth 4 item)))))
	(animate-place-char (car item) vpos hpos)))))

;;; Place the character CHAR at position VPOS, HPOS in the current buffer.
(defun animate-place-char (char vpos hpos)
  (goto-char (window-start))
  (let (abbrev-mode)
    (dotimes (i vpos)
      (end-of-line)
      (if (= (forward-line 1) 1)
	  (insert "\n"))))
  (beginning-of-line)
  (move-to-column (floor hpos) t)
  (unless (eolp) (delete-char 1))
  (insert-char char 1))

(defvar animate-n-steps 10
"*Number of steps `animate-string' will place a char before its last position.")

(defvar animation-buffer-name nil
  "*String naming the default buffer for animations.
When nil animations displayed in the buffer named *Animation*.")

;;;###autoload
(defun animate-string (string vpos &optional hpos)
  "Display STRING animations starting at position VPOS, HPOS.
The characters start at randomly chosen places,
and all slide in parallel to their final positions,
passing through `animate-n-steps' positions before the final ones.
If HPOS is nil (or omitted), center the string horizontally
in the current window."
  (let ((characters
	 (animate-initialize string vpos
			     (or hpos
				 ;; HPOS unspecified, so compute
				 ;; it so as to center the string.
				 (max 0 (/ (- (window-width) (length string)) 2)))))
	(show-trailing-whitespace nil)
	;; Make sure indentation does not use tabs.
	;; They would confuse things.
	(indent-tabs-mode nil))
    (dotimes (i animate-n-steps)
      ;; Bind buffer-undo-list so it will be unchanged when we are done.
      ;; (We're going to undo all our changes anyway.)
      (let (buffer-undo-list
	    list-to-undo)
	;; Display the characters at the Ith position.
	;; This inserts them in the buffer.
	(animate-step characters (/ i 1.0 animate-n-steps))
	;; Make sure buffer is displayed starting at the beginning.
	(set-window-start nil 1)
	;; Display it, and wait just a little while.
	(sit-for .05)
	;; Now undo the changes we made in the buffer.
	(setq list-to-undo buffer-undo-list)
	(while list-to-undo
	  (let ((undo-in-progress t))
	    (setq list-to-undo (primitive-undo 1 list-to-undo))))))
    ;; Insert the characters in their final positions.
    (animate-step characters 1)
    ;; Put the cursor at the end of the text on the line.
    (end-of-line)
    ;; Redisplay so they appear on the screen there.
    (sit-for 0)
    ;; This is so that the undo command, used afterwards,
    ;; will undo the "animate" calls one by one.
    (undo-boundary)))

;;;###autoload
(defun animate-sequence (list-of-strings space)
  "Display animation strings from LIST-OF-STRING with buffer *Animation*.
Strings will be separated from each other by SPACE lines.
 When the variable `animation-buffer-name' is non-nil display
animation in the buffer named by variable's value, creating the
buffer if one does not exist."
  (let ((vpos (/ (- (window-height)
		    1 ;; For the mode-line
		    (* (1- (length list-of-strings)) space)
		    (length list-of-strings))
		 2)))
    (switch-to-buffer (get-buffer-create
                       (or animation-buffer-name
                           "*Animation*")))
    (erase-buffer)
    (sit-for 0)
    (while list-of-strings
      (animate-string (car list-of-strings) vpos)
      (setq vpos (+ vpos space 1))
      (setq list-of-strings (cdr list-of-strings)))))

;;;###autoload
(defun animate-birthday-present (&optional name)
  "Return a birthday present in the buffer *Birthday-Present*.
When optional arg NAME is non-nil or called-interactively, prompt for
NAME of birthday present receiver and return a birthday present in
the buffer *Birthday-Present-for-Name*."
  (interactive (list (read-string "Birthday present for: "
				  nil nil)))
  ;; Make a suitable buffer to display the birthday present in.
  (switch-to-buffer (get-buffer-create
		     (if name
			 (concat "*A-Present-for-" (capitalize name) "*")
		       "*Birthday-Present*")))
  (erase-buffer)
  ;; Display the empty buffer.
  (sit-for 0)

  (if name
      (animate-string "Happy Birthday," 6)
      (animate-string "Happy Birthday" 6))
  (when name (animate-string (format "%s" (capitalize name)) 7))
  (sit-for 1)

  (animate-string "You are my sunshine," 10 30)
  (sit-for .5)
  (animate-string "My only sunshine." 11 30)
  (sit-for .5)
  (animate-string "I'm awful sad that" 12 30)
  (sit-for .5)
  (animate-string "You've moved away." 13 30)
  (sit-for .5)
  (animate-string "Let's talk together" 15 30)
  (sit-for .5)
  (animate-string "And love more deeply." 16 30)
  (sit-for .5)
  (animate-string "Please bring back" 17 30)
  (animate-string "my sunshine" 18 34)
  (animate-string "to stay!" 19 34))

(random t)

(provide 'animate)

;;; animate.el ends here
