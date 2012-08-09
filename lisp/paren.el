;;; paren.el --- highlight matching paren

;; Copyright (C) 1993, 1996, 2001-2012  Free Software Foundation, Inc.

;; Author: rms@gnu.org
;; Maintainer: FSF
;; Keywords: languages, faces

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

;; Put this into your ~/.emacs:

;;  (show-paren-mode t)

;; It will display highlighting on whatever paren matches the one
;; before or after point.

;;; Code:

(defgroup paren-showing nil
  "Showing (un)matching of parens and expressions."
  :prefix "show-paren-"
  :group 'paren-matching)

;; This is the overlay used to highlight the matching paren.
(defvar show-paren-overlay nil)
;; This is the overlay used to highlight the closeparen right before point.
(defvar show-paren-overlay-1 nil)

(defcustom show-paren-style 'parenthesis
  "Style used when showing a matching paren.
Valid styles are `parenthesis' (meaning show the matching paren),
`expression' (meaning show the entire expression enclosed by the paren) and
`mixed' (meaning show the matching paren if it is visible, and the expression
otherwise)."
  :type '(choice (const parenthesis) (const expression) (const mixed))
  :group 'paren-showing)

(defcustom show-paren-delay 0.125
  "Time in seconds to delay before showing a matching paren."
  :type '(number :tag "seconds")
  :group 'paren-showing)

(defcustom show-paren-priority 1000
  "Priority of paren highlighting overlays."
  :type 'integer
  :group 'paren-showing
  :version "21.1")

(defcustom show-paren-ring-bell-on-mismatch nil
  "If non-nil, beep if mismatched paren is detected."
  :type 'boolean
  :group 'paren-showing
  :version "20.3")

(defgroup paren-showing-faces nil
  "Group for faces of Show Paren mode."
  :group 'paren-showing
  :group 'faces
  :version "22.1")

(defface show-paren-match
  '((((class color) (background light))
     :background "turquoise")		; looks OK on tty (becomes cyan)
    (((class color) (background dark))
     :background "steelblue3")		; looks OK on tty (becomes blue)
    (((background dark))
     :background "grey50")
    (t
     :background "gray"))
  "Show Paren mode face used for a matching paren."
  :group 'paren-showing-faces)
(define-obsolete-face-alias 'show-paren-match-face 'show-paren-match "22.1")

(defface show-paren-mismatch
  '((((class color)) (:foreground "white" :background "purple"))
    (t (:inverse-video t)))
  "Show Paren mode face used for a mismatching paren."
  :group 'paren-showing-faces)
(define-obsolete-face-alias 'show-paren-mismatch-face
  'show-paren-mismatch "22.1")

(defvar show-paren-highlight-openparen t
  "*Non-nil turns on openparen highlighting when matching forward.")

(defvar show-paren-idle-timer nil)

;;;###autoload
(define-minor-mode show-paren-mode
  "Toggle visualization of matching parens (Show Paren mode).
With a prefix argument ARG, enable Show Paren mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Show Paren mode is a global minor mode.  When enabled, any
matching parenthesis is highlighted in `show-paren-style' after
`show-paren-delay' seconds of Emacs idle time."
  :global t :group 'paren-showing
    ;; Enable or disable the mechanism.
    ;; First get rid of the old idle timer.
    (if show-paren-idle-timer
	(cancel-timer show-paren-idle-timer))
    (setq show-paren-idle-timer nil)
    ;; If show-paren-mode is enabled in some buffer now,
    ;; set up a new timer.
    (when (memq t (mapcar (lambda (buffer)
			    (with-current-buffer buffer
			      show-paren-mode))
			  (buffer-list)))
      (setq show-paren-idle-timer (run-with-idle-timer
				   show-paren-delay t
				   'show-paren-function)))
    (unless show-paren-mode
      (and show-paren-overlay
	   (eq (overlay-buffer show-paren-overlay) (current-buffer))
	   (delete-overlay show-paren-overlay))
      (and show-paren-overlay-1
	   (eq (overlay-buffer show-paren-overlay-1) (current-buffer))
	   (delete-overlay show-paren-overlay-1))))

;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun show-paren-function ()
  (if show-paren-mode
      (let* ((oldpos (point))
	     (dir (cond ((eq (syntax-class (syntax-after (1- (point)))) 5) -1)
			((eq (syntax-class (syntax-after (point)))      4) 1)))
	     (unescaped
	      (when dir
		;; Verify an even number of quoting characters precede the paren.
		;; Follow the same logic as in `blink-matching-open'.
		(= (if (= dir -1) 1 0)
		   (logand 1 (- (point)
				(save-excursion
				  (if (= dir -1) (forward-char -1))
				  (skip-syntax-backward "/\\")
				  (point)))))))
	     pos mismatch face)
	;;
	;; Find the other end of the sexp.
	(when unescaped
	  (save-excursion
	    (save-restriction
	      ;; Determine the range within which to look for a match.
	      (when blink-matching-paren-distance
		(narrow-to-region
		 (max (point-min) (- (point) blink-matching-paren-distance))
		 (min (point-max) (+ (point) blink-matching-paren-distance))))
	      ;; Scan across one sexp within that range.
	      ;; Errors or nil mean there is a mismatch.
	      (condition-case ()
		  (setq pos (scan-sexps (point) dir))
		(error (setq pos t mismatch t)))
	      ;; Move back the other way and verify we get back to the
	      ;; starting point.  If not, these two parens don't really match.
	      ;; Maybe the one at point is escaped and doesn't really count.
	      (when (integerp pos)
		(unless (condition-case ()
			    (eq (point) (scan-sexps pos (- dir)))
			  (error nil))
		  (setq pos nil)))
	      ;; If found a "matching" paren, see if it is the right
	      ;; kind of paren to match the one we started at.
	      (when (integerp pos)
		(let ((beg (min pos oldpos)) (end (max pos oldpos)))
		  (unless (eq (syntax-class (syntax-after beg)) 8)
		    (setq mismatch
			  (not (or (eq (char-before end)
				       ;; This can give nil.
				       (cdr (syntax-after beg)))
				   (eq (char-after beg)
				       ;; This can give nil.
				       (cdr (syntax-after (1- end))))
                                   ;; The cdr might hold a new paren-class
                                   ;; info rather than a matching-char info,
                                   ;; in which case the two CDRs should match.
                                   (eq (cdr (syntax-after (1- end)))
                                       (cdr (syntax-after beg))))))))))))
	;;
	;; Highlight the other end of the sexp, or unhighlight if none.
	(if (not pos)
	    (progn
	      ;; If not at a paren that has a match,
	      ;; turn off any previous paren highlighting.
	      (and show-paren-overlay (overlay-buffer show-paren-overlay)
		   (delete-overlay show-paren-overlay))
	      (and show-paren-overlay-1 (overlay-buffer show-paren-overlay-1)
		   (delete-overlay show-paren-overlay-1)))
	  ;;
	  ;; Use the correct face.
	  (if mismatch
	      (progn
		(if show-paren-ring-bell-on-mismatch
		    (beep))
		(setq face 'show-paren-mismatch))
	    (setq face 'show-paren-match))
	  ;;
	  ;; If matching backwards, highlight the closeparen
	  ;; before point as well as its matching open.
	  ;; If matching forward, and the openparen is unbalanced,
	  ;; highlight the paren at point to indicate misbalance.
	  ;; Otherwise, turn off any such highlighting.
	  (if (and (not show-paren-highlight-openparen) (= dir 1) (integerp pos))
	      (when (and show-paren-overlay-1
			 (overlay-buffer show-paren-overlay-1))
		(delete-overlay show-paren-overlay-1))
	    (let ((from (if (= dir 1)
			    (point)
			  (- (point) 1)))
		  (to (if (= dir 1)
			  (+ (point) 1)
			(point))))
	      (if show-paren-overlay-1
		  (move-overlay show-paren-overlay-1 from to (current-buffer))
		(setq show-paren-overlay-1 (make-overlay from to nil t)))
	      ;; Always set the overlay face, since it varies.
	      (overlay-put show-paren-overlay-1 'priority show-paren-priority)
	      (overlay-put show-paren-overlay-1 'face face)))
	  ;;
	  ;; Turn on highlighting for the matching paren, if found.
	  ;; If it's an unmatched paren, turn off any such highlighting.
	  (unless (integerp pos)
	    (delete-overlay show-paren-overlay))
	  (let ((to (if (or (eq show-paren-style 'expression)
			    (and (eq show-paren-style 'mixed)
				 (not (pos-visible-in-window-p pos))))
			(point)
		      pos))
		(from (if (or (eq show-paren-style 'expression)
			      (and (eq show-paren-style 'mixed)
				   (not (pos-visible-in-window-p pos))))
			  pos
			(save-excursion
			  (goto-char pos)
			  (- (point) dir)))))
	    (if show-paren-overlay
		(move-overlay show-paren-overlay from to (current-buffer))
	      (setq show-paren-overlay (make-overlay from to nil t))))
	  ;;
	  ;; Always set the overlay face, since it varies.
	  (overlay-put show-paren-overlay 'priority show-paren-priority)
	  (overlay-put show-paren-overlay 'face face)))
    ;; show-paren-mode is nil in this buffer.
    (and show-paren-overlay
	 (delete-overlay show-paren-overlay))
    (and show-paren-overlay-1
	 (delete-overlay show-paren-overlay-1))))

(provide 'paren)

;;; paren.el ends here
