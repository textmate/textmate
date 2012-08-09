;;; cua-rect.el --- CUA unified rectangle support

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Keywords: keyboard emulations convenience CUA
;; Package: cua-base

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

;;; Acknowledgements

;; The rectangle handling and display code borrows from the standard
;; GNU emacs rect.el package and the rect-mark.el package by Rick
;; Sladkey <jrs@world.std.com>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cua-base))

;;; Rectangle support

(require 'rect)

;; If non-nil, restrict current region to this rectangle.
;; Value is a vector [top bot left right corner ins virt select].
;; CORNER specifies currently active corner 0=t/l 1=t/r 2=b/l 3=b/r.
;; INS specifies whether to insert on left(nil) or right(t) side.
;; If VIRT is non-nil, virtual straight edges are enabled.
;; If SELECT is a regexp, only lines starting with that regexp are affected.")
(defvar cua--rectangle nil)
(make-variable-buffer-local 'cua--rectangle)

;; Most recent rectangle geometry.  Note: car is buffer.
(defvar cua--last-rectangle nil)

;; Rectangle restored by undo.
(defvar cua--restored-rectangle nil)

;; Last rectangle copied/killed; nil if last kill was not a rectangle.
(defvar cua--last-killed-rectangle nil)

;; List of overlays used to display current rectangle.
(defvar cua--rectangle-overlays nil)
(make-variable-buffer-local 'cua--rectangle-overlays)
(put 'cua--rectangle-overlays 'permanent-local t)

(defvar cua--overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'cua-rotate-rectangle)))

(defvar cua--virtual-edges-debug nil)

;; Undo rectangle commands.

(defvar cua--rect-undo-set-point nil)

(defun cua--rectangle-undo-boundary ()
  (when (listp buffer-undo-list)
    (let ((s (cua--rect-start-position))
	  (e (cua--rect-end-position)))
      (undo-boundary)
      (push (list 'apply 0 s e
		  'cua--rect-undo-handler
		  (copy-sequence cua--rectangle) t s e)
	  buffer-undo-list))))

(defun cua--rect-undo-handler (rect on s e)
  (if (setq on (not on))
      (setq cua--rect-undo-set-point s)
    (setq cua--restored-rectangle (copy-sequence rect))
    (setq cua--buffer-and-point-before-command nil))
  (push (list 'apply 0 s (if on e s)
	      'cua--rect-undo-handler rect on s e)
	buffer-undo-list))

;;; Rectangle geometry

(defun cua--rectangle-top (&optional val)
  ;; Top of CUA rectangle (buffer position on first line).
  (if (not val)
      (aref cua--rectangle 0)
    (setq val (line-beginning-position))
    (if (<= val (aref cua--rectangle 1))
        (aset cua--rectangle 0 val)
      (aset cua--rectangle 1 val)
      (cua--rectangle-corner 2))))

(defun cua--rectangle-bot (&optional val)
  ;; Bot of CUA rectangle (buffer position on last line).
  (if (not val)
      (aref cua--rectangle 1)
    (setq val (line-end-position))
    (if (>= val (aref cua--rectangle 0))
        (aset cua--rectangle 1 val)
      (aset cua--rectangle 0 val)
      (cua--rectangle-corner 2))))

(defun cua--rectangle-left (&optional val)
  ;; Left column of CUA rectangle.
  (if (integerp val)
      (if (<= val (aref cua--rectangle 3))
          (aset cua--rectangle 2 val)
        (aset cua--rectangle 3 val)
        (cua--rectangle-corner (if (cua--rectangle-right-side) -1 1)))
    (aref cua--rectangle 2)))

(defun cua--rectangle-right (&optional val)
  ;; Right column of CUA rectangle.
  (if (integerp val)
      (if (>= val (aref cua--rectangle 2))
          (aset cua--rectangle 3 val)
        (aset cua--rectangle 2 val)
        (cua--rectangle-corner (if (cua--rectangle-right-side) -1 1)))
    (aref cua--rectangle 3)))

(defun cua--rectangle-corner (&optional advance)
  ;; Currently active corner of rectangle.
  (let ((c (aref cua--rectangle 4)))
    (if (not (integerp advance))
        c
      (aset cua--rectangle 4
            (if (= advance 0)
                (- 3 c) ; opposite corner
              (mod (+ c 4 advance) 4)))
      (aset cua--rectangle 5 0))))

(defun cua--rectangle-right-side (&optional topbot)
  ;; t if point is on right side of rectangle.
  (if (and topbot (= (cua--rectangle-left) (cua--rectangle-right)))
      (< (cua--rectangle-corner) 2)
    (= (mod (cua--rectangle-corner) 2) 1)))

(defun cua--rectangle-column ()
  (if (cua--rectangle-right-side)
      (cua--rectangle-right)
    (cua--rectangle-left)))

(defun cua--rectangle-insert-col (&optional col)
  ;; Currently active corner of rectangle.
  (if (integerp col)
      (aset cua--rectangle 5 col)
    (if (cua--rectangle-right-side t)
        (if (= (aref cua--rectangle 5) 0)
            (1+ (cua--rectangle-right))
          (aref cua--rectangle 5))
      (cua--rectangle-left))))

(defun cua--rectangle-virtual-edges (&optional set val)
  ;; Current setting of rectangle virtual-edges
  (if set
      (aset cua--rectangle 6 val))
  (and ;(not buffer-read-only)
       (aref cua--rectangle 6)))

(defun cua--rectangle-restriction (&optional val bounded negated)
  ;; Current rectangle restriction
  (if val
      (aset cua--rectangle 7
            (and (stringp val)
             (> (length val) 0)
             (list val bounded negated)))
    (aref cua--rectangle 7)))

(defun cua--rectangle-assert ()
  (message "%S (%d)" cua--rectangle (point))
  (if (< (cua--rectangle-right) (cua--rectangle-left))
      (message "rectangle right < left"))
  (if (< (cua--rectangle-bot) (cua--rectangle-top))
      (message "rectangle bot < top")))

(defun cua--rectangle-get-corners ()
  ;; Calculate the rectangular region represented by point and mark,
  ;; putting start in the upper left corner and end in the
  ;; bottom right corner.
  (let ((top (point)) (bot (mark)) r l corner)
    (save-excursion
      (goto-char top)
      (setq l (current-column))
      (goto-char bot)
      (setq r (current-column))
      (if (<= top bot)
          (setq corner (if (<= l r) 0 1))
        (setq top (prog1 bot (setq bot top)))
        (setq corner (if (<= l r) 2 3)))
      (if (<= l r)
          (if (< l r)
              (setq r (1- r)))
        (setq l (prog1 r (setq r l)))
        (goto-char top)
        (move-to-column l)
        (setq top (point))
        (goto-char bot)
        (move-to-column r)
        (setq bot (point))))
    (vector top bot l r corner 0 cua-virtual-rectangle-edges nil)))

(defun cua--rectangle-set-corners ()
  ;; Set mark and point in opposite corners of current rectangle.
  (let (pp pc mp mc (c (cua--rectangle-corner)))
    (cond
     ((= c 0)  ; top/left -> bot/right
      (setq pp (cua--rectangle-top) pc (cua--rectangle-left)
            mp (cua--rectangle-bot) mc (cua--rectangle-right)))
     ((= c 1)  ; top/right -> bot/left
      (setq pp (cua--rectangle-top) pc (cua--rectangle-right)
            mp (cua--rectangle-bot) mc (cua--rectangle-left)))
     ((= c 2)  ; bot/left -> top/right
      (setq pp (cua--rectangle-bot) pc (cua--rectangle-left)
            mp (cua--rectangle-top) mc (cua--rectangle-right)))
     ((= c 3)  ; bot/right -> top/left
      (setq pp (cua--rectangle-bot) pc (cua--rectangle-right)
            mp (cua--rectangle-top) mc (cua--rectangle-left))))
    (goto-char mp)
    (move-to-column mc)
    (set-mark (point))
    (goto-char pp)
    ;; Move cursor inside rectangle, except if char at right edge is a tab.
    (if (and (if (cua--rectangle-right-side)
		 (and (= (move-to-column pc) (- pc tab-width))
		      (not (eolp)))
	       (> (move-to-column pc) pc))
	     (not (bolp)))
	(backward-char 1))
    ))

(defun cua--rect-start-position ()
  ;; Return point of top left corner
  (save-excursion
    (goto-char (cua--rectangle-top))
    (and (> (move-to-column (cua--rectangle-left))
	    (cua--rectangle-left))
	 (not (bolp))
	 (backward-char 1))
    (point)))

(defun cua--rect-end-position ()
  ;; Return point of bottom right cornet
  (save-excursion
    (goto-char (cua--rectangle-bot))
    (and (= (move-to-column (cua--rectangle-right))
	    (- (cua--rectangle-right) tab-width))
	 (not (eolp))
	 (not (bolp))
	 (backward-char 1))
    (point)))

;;; Rectangle resizing

(defun cua--forward-line (n)
  ;; Move forward/backward one line.  Returns t if movement.
  (let ((pt (point)))
    (and (= (forward-line n) 0)
	 ;; Deal with end of buffer
	 (or (not (eobp))
	     (goto-char pt)))))

(defun cua--rectangle-resized ()
  ;; Refresh state after resizing rectangle
  (setq cua--buffer-and-point-before-command nil)
  (cua--rectangle-insert-col 0)
  (cua--rectangle-set-corners)
  (cua--keep-active))

(defun cua-resize-rectangle-right (n)
  "Resize rectangle to the right."
  (interactive "p")
  (let ((resized (> n 0)))
    (while (> n 0)
      (setq n (1- n))
      (cond
       ((cua--rectangle-right-side)
        (cua--rectangle-right (1+ (cua--rectangle-right)))
        (move-to-column (cua--rectangle-right)))
       (t
        (cua--rectangle-left (1+ (cua--rectangle-left)))
        (move-to-column (cua--rectangle-right)))))
    (if resized
        (cua--rectangle-resized))))

(defun cua-resize-rectangle-left (n)
  "Resize rectangle to the left."
  (interactive "p")
  (let (resized)
    (while (> n 0)
      (setq n (1- n))
      (if (or (= (cua--rectangle-right) 0)
              (and (not (cua--rectangle-right-side)) (= (cua--rectangle-left) 0)))
          (setq n 0)
        (cond
         ((cua--rectangle-right-side)
          (cua--rectangle-right (1- (cua--rectangle-right)))
          (move-to-column (cua--rectangle-right)))
         (t
          (cua--rectangle-left (1- (cua--rectangle-left)))
          (move-to-column (cua--rectangle-right))))
        (setq resized t)))
    (if resized
        (cua--rectangle-resized))))

(defun cua-resize-rectangle-down (n)
  "Resize rectangle downwards."
  (interactive "p")
  (let (resized)
    (while (> n 0)
      (setq n (1- n))
      (cond
       ((>= (cua--rectangle-corner) 2)
        (goto-char (cua--rectangle-bot))
        (when (cua--forward-line 1)
          (move-to-column (cua--rectangle-column))
          (cua--rectangle-bot t)
          (setq resized t)))
       (t
        (goto-char (cua--rectangle-top))
        (when (cua--forward-line 1)
          (move-to-column (cua--rectangle-column))
          (cua--rectangle-top t)
          (setq resized t)))))
    (if resized
        (cua--rectangle-resized))))

(defun cua-resize-rectangle-up (n)
  "Resize rectangle upwards."
  (interactive "p")
  (let (resized)
    (while (> n 0)
      (setq n (1- n))
      (cond
       ((>= (cua--rectangle-corner) 2)
        (goto-char (cua--rectangle-bot))
        (when (cua--forward-line -1)
          (move-to-column (cua--rectangle-column))
          (cua--rectangle-bot t)
          (setq resized t)))
       (t
        (goto-char (cua--rectangle-top))
        (when (cua--forward-line -1)
          (move-to-column (cua--rectangle-column))
          (cua--rectangle-top t)
          (setq resized t)))))
    (if resized
        (cua--rectangle-resized))))

(defun cua-resize-rectangle-eol ()
  "Resize rectangle to end of line."
  (interactive)
  (unless (eolp)
    (end-of-line)
    (if (> (current-column) (cua--rectangle-right))
        (cua--rectangle-right (current-column)))
    (if (not (cua--rectangle-right-side))
        (cua--rectangle-corner 1))
    (cua--rectangle-resized)))

(defun cua-resize-rectangle-bol ()
  "Resize rectangle to beginning of line."
  (interactive)
  (unless (bolp)
    (beginning-of-line)
    (cua--rectangle-left (current-column))
    (if (cua--rectangle-right-side)
        (cua--rectangle-corner -1))
    (cua--rectangle-resized)))

(defun cua-resize-rectangle-bot ()
  "Resize rectangle to bottom of buffer."
  (interactive)
  (goto-char (point-max))
  (move-to-column (cua--rectangle-column))
  (cua--rectangle-bot t)
  (cua--rectangle-resized))

(defun cua-resize-rectangle-top ()
  "Resize rectangle to top of buffer."
  (interactive)
  (goto-char (point-min))
  (move-to-column (cua--rectangle-column))
  (cua--rectangle-top t)
  (cua--rectangle-resized))

(defun cua-resize-rectangle-page-up ()
  "Resize rectangle upwards by one scroll page."
  (interactive)
  (scroll-down)
  (move-to-column (cua--rectangle-column))
  (if (>= (cua--rectangle-corner) 2)
      (cua--rectangle-bot t)
    (cua--rectangle-top t))
  (cua--rectangle-resized))

(defun cua-resize-rectangle-page-down ()
  "Resize rectangle downwards by one scroll page."
  (interactive)
  (scroll-up)
  (move-to-column (cua--rectangle-column))
  (if (>= (cua--rectangle-corner) 2)
      (cua--rectangle-bot t)
    (cua--rectangle-top t))
  (cua--rectangle-resized))

;;; Mouse support

;; This is pretty simplistic, but it does the job...

(defun cua-mouse-resize-rectangle (event)
  "Set rectangle corner at mouse click position."
  (interactive "e")
  (mouse-set-point event)
  ;; FIX ME -- need to calculate virtual column.
  (if (cua--rectangle-virtual-edges)
      (move-to-column (car (posn-col-row (event-end event))) t))
  (if (cua--rectangle-right-side)
      (cua--rectangle-right (current-column))
    (cua--rectangle-left (current-column)))
  (if (>= (cua--rectangle-corner) 2)
      (cua--rectangle-bot t)
    (cua--rectangle-top t))
  (cua--rectangle-resized))

(defvar cua--mouse-last-pos nil)

(defun cua-mouse-set-rectangle-mark (event)
  "Start rectangle at mouse click position."
  (interactive "e")
  (when cua--rectangle
    (cua--deactivate-rectangle)
    (cua--deactivate t))
  (setq cua--last-rectangle nil)
  (mouse-set-point event)
  ;; FIX ME -- need to calculate virtual column.
  (cua-set-rectangle-mark)
  (setq cua--buffer-and-point-before-command nil)
  (setq cua--mouse-last-pos nil))

(defun cua-mouse-save-then-kill-rectangle (event arg)
  "Expand rectangle to mouse click position and copy rectangle.
If command is repeated at same position, delete the rectangle."
  (interactive "e\nP")
  (if (and (eq this-command last-command)
           (eq (point) (car-safe cua--mouse-last-pos))
           (eq cua--last-killed-rectangle (cdr-safe cua--mouse-last-pos)))
      (progn
        (unless buffer-read-only
          (cua--delete-rectangle))
        (cua--deactivate))
    (cua-mouse-resize-rectangle event)
    (let ((cua-keep-region-after-copy t))
      (cua-copy-rectangle arg)
      (setq cua--mouse-last-pos (cons (point) cua--last-killed-rectangle)))))

(defun cua--mouse-ignore (event)
  (interactive "e")
  (setq this-command last-command))

(defun cua--rectangle-move (dir)
  (let ((moved t)
        (top (cua--rectangle-top))
        (bot (cua--rectangle-bot))
        (l (cua--rectangle-left))
        (r (cua--rectangle-right)))
    (cond
     ((eq dir 'up)
      (goto-char top)
      (when (cua--forward-line -1)
        (cua--rectangle-top t)
        (goto-char bot)
        (forward-line -1)
        (cua--rectangle-bot t)))
     ((eq dir 'down)
      (goto-char bot)
      (when (cua--forward-line 1)
        (cua--rectangle-bot t)
        (goto-char top)
        (cua--forward-line 1)
        (cua--rectangle-top t)))
     ((eq dir 'left)
      (when (> l 0)
        (cua--rectangle-left (1- l))
        (cua--rectangle-right (1- r))))
     ((eq dir 'right)
      (cua--rectangle-right (1+ r))
      (cua--rectangle-left (1+ l)))
     (t
      (setq moved nil)))
    (when moved
      (setq cua--buffer-and-point-before-command nil)
      (cua--rectangle-set-corners)
      (cua--keep-active))))


;;; Operations on current rectangle

(defun cua--tabify-start (start end)
  ;; Return position where auto-tabify should start (or nil if not required).
  (save-excursion
    (save-restriction
      (widen)
      (and (not buffer-read-only)
	   cua-auto-tabify-rectangles
	   (if (or (not (integerp cua-auto-tabify-rectangles))
		   (= (point-min) (point-max))
		   (progn
		     (goto-char (max (point-min)
				     (- start cua-auto-tabify-rectangles)))
		     (search-forward "\t" (min (point-max)
					       (+ end cua-auto-tabify-rectangles)) t)))
	       start)))))

(defun cua--rectangle-operation (keep-clear visible undo pad tabify &optional fct post-fct)
  ;; Call FCT for each line of region with 4 parameters:
  ;; Region start, end, left-col, right-col
  ;; Point is at start when FCT is called
  ;; Call fct with (s,e) = whole lines if VISIBLE non-nil.
  ;; Only call fct for visible lines if VISIBLE==t.
  ;; Set undo boundary if UNDO is non-nil.
  ;; Rectangle is padded if PAD = t or numeric and (cua--rectangle-virtual-edges)
  ;; Perform auto-tabify after operation if TABIFY is non-nil.
  ;; Mark is kept if keep-clear is 'keep and cleared if keep-clear is 'clear.
  (let* ((inhibit-field-text-motion t)
	 (start (cua--rectangle-top))
         (end   (cua--rectangle-bot))
         (l (cua--rectangle-left))
         (r (1+ (cua--rectangle-right)))
         (m (make-marker))
         (tabpad (and (integerp pad) (= pad 2)))
         (sel (cua--rectangle-restriction))
	 (tabify-start (and tabify (cua--tabify-start start end))))
    (if undo
        (cua--rectangle-undo-boundary))
    (if (integerp pad)
        (setq pad (cua--rectangle-virtual-edges)))
    (save-excursion
      (save-restriction
        (widen)
        (when (> (cua--rectangle-corner) 1)
          (goto-char end)
          (and (bolp) (not (eolp)) (not (eobp))
               (setq end (1+ end))))
        (when (eq visible t)
          (setq start (max (window-start) start))
          (setq end   (min (window-end) end)))
        (goto-char end)
        (setq end (line-end-position))
	(if (and visible (bolp) (not (eobp)))
	    (setq end (1+ end)))
        (goto-char start)
        (setq start (line-beginning-position))
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (move-to-column r pad)
          (and (not pad) (not visible) (> (current-column) r)
               (backward-char 1))
          (if (and tabpad (not pad) (looking-at "\t"))
              (forward-char 1))
          (set-marker m (point))
          (move-to-column l pad)
          (if (and fct (or visible (and (>= (current-column) l) (<= (current-column) r))))
              (let ((v t) (p (point)))
                (when sel
                  (if (car (cdr sel))
                      (setq v (looking-at (car sel)))
                    (setq v (re-search-forward (car sel) m t))
                    (goto-char p))
                  (if (car (cdr (cdr sel)))
                      (setq v (null v))))
                (if visible
		    (funcall fct p m l r v)
                  (if v
                      (funcall fct p m l r)))))
          (set-marker m nil)
          (forward-line 1))
        (if (not visible)
            (cua--rectangle-bot t))
        (if post-fct
            (funcall post-fct l r))
	(when tabify-start
	  (tabify tabify-start (point)))))
    (cond
     ((eq keep-clear 'keep)
      (cua--keep-active))
     ((eq keep-clear 'clear)
      (cua--deactivate))
     ((eq keep-clear 'corners)
      (cua--rectangle-set-corners)
      (cua--keep-active)))
    (setq cua--buffer-and-point-before-command nil)))

(put 'cua--rectangle-operation 'lisp-indent-function 4)

(defun cua--delete-rectangle ()
  (let ((lines 0))
    (if (not (cua--rectangle-virtual-edges))
	(cua--rectangle-operation nil nil t 2 t
	  (lambda (s e l r v)
            (setq lines (1+ lines))
            (if (and (> e s) (<= e (point-max)))
                (delete-region s e))))
      (cua--rectangle-operation nil 1 t nil t
	(lambda (s e l r v)
	   (setq lines (1+ lines))
	   (when (and (> e s) (<= e (point-max)))
	     (delete-region s e)))))
    lines))

(defun cua--extract-rectangle ()
  (let (rect)
    (if (not (cua--rectangle-virtual-edges))
	(cua--rectangle-operation nil nil nil nil nil ; do not tabify
	  (lambda (s e l r)
	     (setq rect (cons (cua--filter-buffer-noprops s e) rect))))
      (cua--rectangle-operation nil 1 nil nil nil ; do not tabify
	(lambda (s e l r v)
	   (let ((copy t) (bs 0) (as 0) row)
	     (if (= s e) (setq e (1+ e)))
	     (goto-char s)
	     (move-to-column l)
	     (if (= (point) (line-end-position))
		 (setq bs (- r l)
		       copy nil)
	       (skip-chars-forward "\s\t" e)
	       (setq bs (- (min r (current-column)) l)
		     s (point))
	       (move-to-column r)
	       (skip-chars-backward "\s\t" s)
	       (setq as (- r (max (current-column) l))
		     e (point)))
       	     (setq row (if (and copy (> e s))
			   (cua--filter-buffer-noprops s e)
			 ""))
    	     (when (> bs 0)
    	       (setq row (concat (make-string bs ?\s) row)))
    	     (when (> as 0)
    	       (setq row (concat row (make-string as ?\s))))
    	     (setq rect (cons row rect))))))
    (nreverse rect)))

(defun cua--insert-rectangle (rect &optional below paste-column line-count)
  ;; Insert rectangle as insert-rectangle, but don't set mark and exit with
  ;; point at either next to top right or below bottom left corner
  ;; Notice: In overwrite mode, the rectangle is inserted as separate text lines.
  (if (eq below 'auto)
      (setq below (and (bolp)
                       (or (eolp) (eobp) (= (1+ (point)) (point-max))))))
  (unless paste-column
    (setq paste-column (current-column)))
  (let ((lines rect)
        (first t)
	(tabify-start (cua--tabify-start (point) (point)))
	last-column
        p)
    (while (or lines below)
      (or first
          (if overwrite-mode
              (insert ?\n)
            (forward-line 1)
            (or (bolp) (insert ?\n))))
      (unless overwrite-mode
	(move-to-column paste-column t))
      (if (not lines)
          (setq below nil)
        (insert-for-yank (car lines))
	(unless last-column
	  (setq last-column (current-column)))
        (setq lines (cdr lines))
        (and first (not below)
             (setq p (point))))
      (setq first nil)
      (if (and line-count (= (setq line-count (1- line-count)) 0))
	  (setq lines nil)))
    (when (and line-count last-column (not overwrite-mode))
      (while (> line-count 0)
	(forward-line 1)
	(or (bolp) (insert ?\n))
	(move-to-column paste-column t)
        (insert-char ?\s (- last-column paste-column -1))
	(setq line-count (1- line-count))))
    (when (and tabify-start
	       (not overwrite-mode))
      (tabify tabify-start (point)))
    (and p (not overwrite-mode)
         (goto-char p))))

(defun cua--copy-rectangle-as-kill (&optional ring)
  (if cua--register
      (set-register cua--register (cua--extract-rectangle))
    (setq killed-rectangle (cua--extract-rectangle))
    (setq cua--last-killed-rectangle (cons (and kill-ring (car kill-ring)) killed-rectangle))
    (if ring
        (kill-new (mapconcat
                   (function (lambda (row) (concat row "\n")))
                   killed-rectangle "")))))

(defun cua--activate-rectangle ()
  ;; Turn on rectangular marking mode by disabling transient mark mode
  ;; and manually handling highlighting from a post command hook.
  ;; Be careful if we are already marking a rectangle.
  (setq cua--rectangle
        (if (and cua--last-rectangle
                 (eq (car cua--last-rectangle) (current-buffer))
                 (eq (car (cdr cua--last-rectangle)) (point)))
            (cdr (cdr cua--last-rectangle))
          (cua--rectangle-get-corners))
        cua--status-string (if (cua--rectangle-virtual-edges) " [R]" "")
        cua--last-rectangle nil))

;; (defvar cua-save-point nil)

(defun cua--deactivate-rectangle ()
  ;; This is used to clean up after `cua--activate-rectangle'.
  (mapc (function delete-overlay) cua--rectangle-overlays)
  (setq cua--last-rectangle (cons (current-buffer)
                                  (cons (point) ;; cua-save-point
                                        cua--rectangle))
        cua--rectangle nil
        cua--rectangle-overlays nil
        cua--status-string nil
        cua--mouse-last-pos nil))

(defun cua--highlight-rectangle ()
  ;; This function is used to highlight the rectangular region.
  ;; We do this by putting an overlay on each line within the rectangle.
  ;; Each overlay extends across all the columns of the rectangle.
  ;; We try to reuse overlays where possible because this is more efficient
  ;; and results in less flicker.
  ;; If cua--rectangle-virtual-edges is nil and the buffer contains tabs or short lines,
  ;; the highlighted region may not be perfectly rectangular.
  (let ((deactivate-mark deactivate-mark)
        (old cua--rectangle-overlays)
        (new nil)
        (left (cua--rectangle-left))
        (right (1+ (cua--rectangle-right))))
    (when (/= left right)
      (sit-for 0)  ; make window top/bottom reliable
      (cua--rectangle-operation nil t nil nil nil ; do not tabify
        (lambda (s e l r v)
           (let ((rface (if v 'cua-rectangle 'cua-rectangle-noselect))
                 overlay bs ms as)
	     (when (cua--rectangle-virtual-edges)
	       (let ((lb (line-beginning-position))
		     (le (line-end-position))
		     cl cl0 pl cr cr0 pr)
		 (goto-char s)
		 (setq cl (move-to-column l)
		       pl (point))
		 (setq cr (move-to-column r)
		       pr (point))
		 (if (= lb pl)
		     (setq cl0 0)
		   (goto-char (1- pl))
		   (setq cl0 (current-column)))
		 (if (= lb le)
		     (setq cr0 0)
		   (goto-char (1- pr))
		   (setq cr0 (current-column)))
		 (unless (and (= cl l) (= cr r))
		   (when (/= cl l)
		     (setq bs (propertize
			       (make-string
				(- l cl0 (if (and (= le pl) (/= le lb)) 1 0))
				(if cua--virtual-edges-debug ?. ?\s))
			       'face (or (get-text-property (1- s) 'face) 'default)))
		     (if (/= pl le)
			 (setq s (1- s))))
		   (cond
		    ((= cr r)
		     (if (and (/= pr le)
			      (/= cr0 (1- cr))
			      (or bs (/= cr0 (- cr tab-width)))
			      (/= (mod cr tab-width) 0))
			 (setq e (1- e))))
		    ((= cr cl)
		     (setq ms (propertize
			       (make-string
				(- r l)
				(if cua--virtual-edges-debug ?, ?\s))
			       'face rface))
		     (if (cua--rectangle-right-side)
			 (put-text-property (1- (length ms)) (length ms) 'cursor 2 ms)
		       (put-text-property 0 1 'cursor 2 ms))
		     (setq bs (concat bs ms))
		     (setq rface nil))
 		    (t
		     (setq as (propertize
			       (make-string
				(- r cr0 (if (= le pr) 1 0))
				(if cua--virtual-edges-debug ?~ ?\s))
			       'face rface))
		     (if (cua--rectangle-right-side)
			 (put-text-property (1- (length as)) (length as) 'cursor 2 as)
		       (put-text-property 0 1 'cursor 2 as))
		     (if (/= pr le)
			 (setq e (1- e))))))))
	     ;; Trim old leading overlays.
             (while (and old
                         (setq overlay (car old))
                         (< (overlay-start overlay) s)
                         (/= (overlay-end overlay) e))
               (delete-overlay overlay)
               (setq old (cdr old)))
             ;; Reuse an overlay if possible, otherwise create one.
             (if (and old
                      (setq overlay (car old))
                      (or (= (overlay-start overlay) s)
                          (= (overlay-end overlay) e)))
                 (progn
                   (move-overlay overlay s e)
                   (setq old (cdr old)))
               (setq overlay (make-overlay s e)))
 	     (overlay-put overlay 'before-string bs)
	     (overlay-put overlay 'after-string as)
	     (overlay-put overlay 'face rface)
	     (overlay-put overlay 'keymap cua--overlay-keymap)
	     (overlay-put overlay 'window (selected-window))
	     (setq new (cons overlay new))))))
    ;; Trim old trailing overlays.
    (mapc (function delete-overlay) old)
    (setq cua--rectangle-overlays (nreverse new))))

(defun cua--indent-rectangle (&optional ch to-col clear)
  ;; Indent current rectangle.
  (let ((col (cua--rectangle-insert-col))
        (pad (cua--rectangle-virtual-edges))
        indent)
    (cua--rectangle-operation (if clear 'clear 'corners) nil t pad nil
      (lambda (s e l r)
         (move-to-column col pad)
         (if (and (eolp)
                  (< (current-column) col))
             (move-to-column col t))
	 (cond
	  (to-col (indent-to to-col))
	  ((and ch (not (eq ch ?\t))) (insert ch))
	  (t (tab-to-tab-stop)))
         (if (cua--rectangle-right-side t)
             (cua--rectangle-insert-col (current-column))
           (setq indent (- (current-column) l))))
      (lambda (l r)
         (when (and indent (> indent 0))
           (aset cua--rectangle 2 (+ l indent))
           (aset cua--rectangle 3 (+ r indent -1)))))))

;;
;; rectangle functions / actions
;;

(defvar cua--rectangle-initialized nil)

(defun cua-set-rectangle-mark (&optional reopen)
  "Set mark and start in CUA rectangle mode.
With prefix argument, activate previous rectangle if possible."
  (interactive "P")
  (unless cua--rectangle-initialized
    (cua--init-rectangles))
  (when (not cua--rectangle)
    (if (and reopen
             cua--last-rectangle
             (eq (car cua--last-rectangle) (current-buffer)))
        (goto-char (car (cdr cua--last-rectangle)))
      (if (not mark-active)
          (push-mark nil nil t)))
    (cua--activate-rectangle)
    (cua--rectangle-set-corners)
    (setq mark-active t
          cua--explicit-region-start t)
    (if cua-enable-rectangle-auto-help
        (cua-help-for-rectangle t))))

(defun cua-clear-rectangle-mark ()
  "Cancel current rectangle."
  (interactive)
  (when cua--rectangle
    (setq mark-active nil
          cua--explicit-region-start nil)
    (cua--deactivate-rectangle)))

(defun cua-toggle-rectangle-mark ()
  (interactive)
  (if cua--rectangle
      (cua--deactivate-rectangle)
    (unless cua--rectangle-initialized
      (cua--init-rectangles))
    (cua--activate-rectangle))
  (if cua--rectangle
      (if cua-enable-rectangle-auto-help
          (cua-help-for-rectangle t))
    (if cua-enable-region-auto-help
        (cua-help-for-region t))))

(defun cua-restrict-regexp-rectangle (arg)
  "Restrict rectangle to lines (not) matching REGEXP.
With prefix argument, the toggle restriction."
  (interactive "P")
  (let ((r (cua--rectangle-restriction)) regexp)
    (if (and r (null (car (cdr r))))
      (if arg
          (cua--rectangle-restriction (car r) nil (not (car (cdr (cdr r)))))
        (cua--rectangle-restriction "" nil nil))
      (cua--rectangle-restriction
       (read-from-minibuffer "Restrict rectangle (regexp): "
                             nil nil nil nil) nil arg))))

(defun cua-restrict-prefix-rectangle (arg)
  "Restrict rectangle to lines (not) starting with CHAR.
With prefix argument, the toggle restriction."
  (interactive "P")
  (let ((r (cua--rectangle-restriction)) regexp)
    (if (and r (car (cdr r)))
      (if arg
          (cua--rectangle-restriction (car r) t (not (car (cdr (cdr r)))))
        (cua--rectangle-restriction "" nil nil))
      (cua--rectangle-restriction
       (format "[%c]"
               (read-char "Restrictive rectangle (char): ")) t arg))))

(defun cua-move-rectangle-up ()
  (interactive)
  (cua--rectangle-move 'up))

(defun cua-move-rectangle-down ()
  (interactive)
  (cua--rectangle-move 'down))

(defun cua-move-rectangle-left ()
  (interactive)
  (cua--rectangle-move 'left))

(defun cua-move-rectangle-right ()
  (interactive)
  (cua--rectangle-move 'right))

(defun cua-copy-rectangle (arg)
  (interactive "P")
  (setq arg (cua--prefix-arg arg))
  (cua--copy-rectangle-as-kill arg)
  (if cua-keep-region-after-copy
      (cua--keep-active)
    (cua--deactivate)))

(defun cua-cut-rectangle (arg)
  (interactive "P")
  (if buffer-read-only
      (cua-copy-rectangle arg)
    (setq arg (cua--prefix-arg arg))
    (goto-char (min (mark) (point)))
    (cua--copy-rectangle-as-kill arg)
    (cua--delete-rectangle))
  (cua--deactivate))

(defun cua-delete-rectangle ()
  (interactive)
  (goto-char (min (point) (mark)))
  (if cua-delete-copy-to-register-0
      (set-register ?0 (cua--extract-rectangle)))
  (cua--delete-rectangle)
  (cua--deactivate))

(defun cua-rotate-rectangle ()
  (interactive)
  (cua--rectangle-corner (if (= (cua--rectangle-left) (cua--rectangle-right)) 0 1))
  (cua--rectangle-set-corners)
  (if (cua--rectangle-virtual-edges)
      (setq cua--buffer-and-point-before-command nil)))

(defun cua-toggle-rectangle-virtual-edges ()
  (interactive)
  (cua--rectangle-virtual-edges t (not (cua--rectangle-virtual-edges)))
  (cua--rectangle-set-corners)
  (setq cua--status-string (and (cua--rectangle-virtual-edges) " [R]"))
  (cua--keep-active))

(defun cua-do-rectangle-padding ()
  (interactive)
  (if buffer-read-only
      (message "Cannot do padding in read-only buffer")
    (cua--rectangle-operation nil nil t t t)
    (cua--rectangle-set-corners))
  (cua--keep-active))

(defun cua-open-rectangle ()
  "Blank out CUA rectangle, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle."
  (interactive)
  (cua--rectangle-operation 'corners nil t 1 nil
   (lambda (s e l r)
      (skip-chars-forward " \t")
      (let ((ws (- (current-column) l))
            (p (point)))
        (skip-chars-backward " \t")
        (delete-region (point) p)
        (indent-to (+ r ws))))))

(defun cua-close-rectangle (arg)
  "Delete all whitespace starting at left edge of CUA rectangle.
On each line in the rectangle, all continuous whitespace starting
at that column is deleted.
With prefix arg, also delete whitespace to the left of that column."
  (interactive "P")
  (cua--rectangle-operation 'clear nil t 1 nil
   (lambda (s e l r)
      (when arg
        (skip-syntax-backward " " (line-beginning-position))
        (setq s (point)))
      (skip-syntax-forward " " (line-end-position))
      (delete-region s (point)))))

(defun cua-blank-rectangle ()
  "Blank out CUA rectangle.
The text previously in the rectangle is overwritten by the blanks."
  (interactive)
  (cua--rectangle-operation 'keep nil nil 1 nil
   (lambda (s e l r)
      (goto-char e)
      (skip-syntax-forward " " (line-end-position))
      (setq e (point))
      (let ((column (current-column)))
        (goto-char s)
        (skip-syntax-backward " " (line-beginning-position))
        (delete-region (point) e)
        (indent-to column)))))

(defun cua-align-rectangle ()
  "Align rectangle lines to left column."
  (interactive)
  (let (x)
    (cua--rectangle-operation 'clear nil t t nil
     (lambda (s e l r)
        (let ((b (line-beginning-position)))
          (skip-syntax-backward "^ " b)
          (skip-syntax-backward " " b)
          (setq s (point)))
        (skip-syntax-forward " " (line-end-position))
        (delete-region s (point))
        (indent-to l))
     (lambda (l r)
        (move-to-column l)
        ;; (setq cua-save-point (point))
        ))))

(declare-function cua--cut-rectangle-to-global-mark  "cua-gmrk" (as-text))
(declare-function cua--copy-rectangle-to-global-mark "cua-gmrk" (as-text))

(defun cua-copy-rectangle-as-text (&optional arg delete)
  "Copy rectangle, but store as normal text."
  (interactive "P")
  (if cua--global-mark-active
      (if delete
          (cua--cut-rectangle-to-global-mark t)
        (cua--copy-rectangle-to-global-mark t))
    (let* ((rect (cua--extract-rectangle))
           (text (mapconcat
                  (function (lambda (row) (concat row "\n")))
                  rect "")))
      (setq arg (cua--prefix-arg arg))
      (if cua--register
          (set-register cua--register text)
        (kill-new text)))
    (if delete
        (cua--delete-rectangle))
    (cua--deactivate)))

(defun cua-cut-rectangle-as-text (arg)
  "Kill rectangle, but store as normal text."
  (interactive "P")
  (cua-copy-rectangle-as-text arg (not buffer-read-only)))

(defun cua-string-rectangle (string)
  "Replace CUA rectangle contents with STRING on each line.
The length of STRING need not be the same as the rectangle width."
  (interactive "sString rectangle: ")
  (cua--rectangle-operation 'keep nil t t nil
     (lambda (s e l r)
        (delete-region s e)
        (skip-chars-forward " \t")
        (let ((ws (- (current-column) l)))
          (delete-region s (point))
          (insert string)
          (indent-to (+ (current-column) ws))))
     (unless (cua--rectangle-restriction)
       (lambda (l r)
          (cua--rectangle-right (max l (+ l (length string) -1)))))))

(defun cua-fill-char-rectangle (character)
  "Replace CUA rectangle contents with CHARACTER."
  (interactive "cFill rectangle with character: ")
  (cua--rectangle-operation 'clear nil t 1 nil
   (lambda (s e l r)
      (delete-region s e)
      (move-to-column l t)
      (insert-char character (- r l)))))

(defun cua-replace-in-rectangle (regexp newtext)
  "Replace REGEXP with NEWTEXT in each line of CUA rectangle."
  (interactive "sReplace regexp: \nsNew text: ")
  (if buffer-read-only
      (message "Cannot replace in read-only buffer")
    (cua--rectangle-operation 'keep nil t 1 nil
     (lambda (s e l r)
        (if (re-search-forward regexp e t)
            (replace-match newtext nil nil))))))

(defun cua-incr-rectangle (increment)
  "Increment each line of CUA rectangle by prefix amount."
  (interactive "p")
  (cua--rectangle-operation 'keep nil t 1 nil
     (lambda (s e l r)
        (cond
         ((re-search-forward "0x\\([0-9a-fA-F]+\\)" e t)
          (let* ((txt (cua--filter-buffer-noprops (match-beginning 1) (match-end 1)))
                 (n (string-to-number txt 16))
                 (fmt (format "0x%%0%dx" (length txt))))
            (replace-match (format fmt (+ n increment)))))
         ((re-search-forward "\\( *-?[0-9]+\\)" e t)
          (let* ((txt (cua--filter-buffer-noprops (match-beginning 1) (match-end 1)))
                 (prefix (if (= (aref txt 0) ?0) "0" ""))
                 (n (string-to-number txt 10))
                 (fmt (format "%%%s%dd" prefix (length txt))))
            (replace-match (format fmt (+ n increment)))))
         (t nil)))))

(defvar cua--rectangle-seq-format "%d"
  "Last format used by `cua-sequence-rectangle'.")

(defun cua-sequence-rectangle (first incr format)
  "Resequence each line of CUA rectangle starting from FIRST.
The numbers are formatted according to the FORMAT string."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (string-to-number
            (read-string "Start value: (0) " nil nil "0")))
         (string-to-number
          (read-string "Increment: (1) " nil nil "1"))
         (read-string (concat "Format: (" cua--rectangle-seq-format ") "))))
  (if (= (length format) 0)
      (setq format cua--rectangle-seq-format)
    (setq cua--rectangle-seq-format format))
  (cua--rectangle-operation 'clear nil t 1 nil
     (lambda (s e l r)
         (delete-region s e)
         (insert (format format first))
         (setq first (+ first incr)))))

(defmacro cua--convert-rectangle-as (command tabify)
  `(cua--rectangle-operation 'clear nil nil nil ,tabify
    (lambda (s e l r)
       (,command s e))))

(defun cua-upcase-rectangle ()
  "Convert the rectangle to upper case."
  (interactive)
  (cua--convert-rectangle-as upcase-region nil))

(defun cua-downcase-rectangle ()
  "Convert the rectangle to lower case."
  (interactive)
  (cua--convert-rectangle-as downcase-region nil))

(defun cua-upcase-initials-rectangle ()
  "Convert the rectangle initials to upper case."
  (interactive)
  (cua--convert-rectangle-as upcase-initials-region nil))

(defun cua-capitalize-rectangle ()
  "Convert the rectangle to proper case."
  (interactive)
  (cua--convert-rectangle-as capitalize-region nil))


;;; Replace/rearrange text in current rectangle

(defun cua--rectangle-aux-replace (width adjust keep replace pad format-fct &optional setup-fct)
  ;; Process text inserted by calling SETUP-FCT or current rectangle if nil.
  ;; Then call FORMAT-FCT on text (if non-nil); takes two args: start and end.
  ;; Fill to WIDTH characters if > 0 or fill to current width if == 0.
  ;; Don't fill if WIDTH < 0.
  ;; Replace current rectangle by filled text if REPLACE is non-nil
  (let ((auxbuf (get-buffer-create "*CUA temp*"))
        (w (if (> width 1) width
	     (- (cua--rectangle-right) (cua--rectangle-left) -1)))
        (r (or setup-fct (cua--extract-rectangle)))
        y z (tr 0))
    (with-current-buffer auxbuf
      (erase-buffer)
      (if setup-fct
          (funcall setup-fct)
        (cua--insert-rectangle r))
      (if format-fct
          (let ((fill-column w))
            (funcall format-fct (point-min) (point-max))))
      (when replace
        (goto-char (point-min))
        (while (not (eobp))
          (setq z (cons (filter-buffer-substring (point) (line-end-position)) z))
          (forward-line 1))))
    (if (not cua--debug)
	(kill-buffer auxbuf))
    (when replace
      (setq z (reverse z))
      (if cua--debug
	  (print z auxbuf))
      (cua--rectangle-operation nil nil t pad nil
        (lambda (s e l r)
           (let (cc)
             (goto-char e)
             (skip-chars-forward " \t")
             (setq cc (current-column))
	     (if cua--debug
		 (print (list cc s e) auxbuf))
             (delete-region s (point))
             (if (not z)
                 (setq y 0)
	       (move-to-column l t)
	       (insert (car z))
	       (when (> (current-column) (+ l w))
		 (setq y (point))
		 (move-to-column (+ l w) t)
		 (delete-region (point) y)
		 (setq tr (1+ tr)))
	       (setq z (cdr z)))
	     (if cua--debug
		 (print (list (current-column) cc) auxbuf))
	     (just-one-space 0)
             (indent-to cc))))
      (if (> tr 0)
	  (message "Warning:  Truncated %d row%s" tr (if (> tr 1) "s" "")))
      (if adjust
          (cua--rectangle-right (+ (cua--rectangle-left) w -1)))
      (if keep
          (cua--rectangle-resized)))))

(put 'cua--rectangle-aux-replace 'lisp-indent-function 4)

(defun cua--left-fill-rectangle (start end)
  (beginning-of-line)
  (while (< (point) (point-max))
    (delete-horizontal-space nil)
    (forward-line 1))
  (fill-region-as-paragraph (point-min) (point-max) 'left nil)
  (untabify (point-min) (point-max)))

(defun cua-text-fill-rectangle (width text)
  "Replace rectangle with filled TEXT read from minibuffer.
A numeric prefix argument is used a new width for the filled rectangle."
  (interactive (list
                (prefix-numeric-value current-prefix-arg)
                (read-from-minibuffer "Enter text: "
                                      nil nil nil nil)))
  (cua--rectangle-aux-replace width t t t 1
    'cua--left-fill-rectangle
    (lambda () (insert text))))

(defun cua-refill-rectangle (width)
  "Fill contents of current rectangle.
A numeric prefix argument is used as new width for the filled rectangle."
  (interactive "P")
  (cua--rectangle-aux-replace
      (if width (prefix-numeric-value width) 0)
      t t t 1 'cua--left-fill-rectangle))

(defun cua-shell-command-on-rectangle (replace command)
  "Run shell command on rectangle like `shell-command-on-region'.
With prefix arg, replace rectangle with output from command."
  (interactive (list
                current-prefix-arg
                (read-from-minibuffer "Shell command on rectangle: "
                                      nil nil nil
                                      'shell-command-history)))
  (cua--rectangle-aux-replace -1 t t replace 1
    (lambda (s e)
       (shell-command-on-region s e command
                                replace replace nil))))

(defun cua-reverse-rectangle ()
  "Reverse the lines of the rectangle."
  (interactive)
  (cua--rectangle-aux-replace 0 t t t t 'reverse-region))

(defun cua-scroll-rectangle-up ()
  "Remove the first line of the rectangle and scroll remaining lines up."
  (interactive)
  (cua--rectangle-aux-replace 0 t t t t
    (lambda (s e)
       (if (= (forward-line 1) 0)
           (delete-region s (point))))))

(defun cua-scroll-rectangle-down ()
  "Insert a blank line at the first line of the rectangle.
The remaining lines are scrolled down, losing the last line."
  (interactive)
  (cua--rectangle-aux-replace 0 t t t t
    (lambda (s e)
       (goto-char s)
       (insert "\n"))))


;;; Insert/delete text to left or right of rectangle

(defun cua-insert-char-rectangle (&optional ch)
  (interactive)
  (if buffer-read-only
      (ding)
    (cua--indent-rectangle (or ch (aref (this-single-command-keys) 0)))
    (cua--keep-active))
  t)

(defun cua-indent-rectangle (column)
  "Indent rectangle to next tab stop.
With prefix arg, indent to that column."
  (interactive "P")
  (if (null column)
      (cua-insert-char-rectangle ?\t)
    (cua--indent-rectangle nil (prefix-numeric-value column))))

(defun cua-delete-char-rectangle ()
  "Delete char to left or right of rectangle."
  (interactive)
  (let ((col (cua--rectangle-insert-col))
        (pad (cua--rectangle-virtual-edges))
        indent)
    (cua--rectangle-operation 'corners nil t pad nil
     (lambda (s e l r)
        (move-to-column
         (if (cua--rectangle-right-side t)
             (max (1+ r) col) l)
         pad)
        (if (bolp)
            nil
          (delete-char -1)
          (if (cua--rectangle-right-side t)
              (cua--rectangle-insert-col (current-column))
            (setq indent (- l (current-column))))))
     (lambda (l r)
        (when (and indent (> indent 0))
          (aset cua--rectangle 2 (- l indent))
          (aset cua--rectangle 3 (- r indent 1)))))))

(defun cua-help-for-rectangle (&optional help)
  (interactive)
  (let ((M (cond ((eq cua--rectangle-modifier-key 'hyper) " H-")
		 ((eq cua--rectangle-modifier-key 'super) " s-")
		 ((eq cua--rectangle-modifier-key 'alt) " A-")
		 (t " M-"))))
    (message
     (concat (if help "C-?:help" "")
             M "p:pad" M "o:open" M "c:close" M "b:blank"
             M "s:string" M "f:fill" M "i:incr" M "n:seq"))))


;;; CUA-like cut & paste for rectangles

(defun cua--cancel-rectangle ()
  ;; Cancel rectangle
  (if cua--rectangle
      (cua--deactivate-rectangle))
  (setq cua--last-rectangle nil))

(defun cua--rectangle-post-command ()
  (if cua--restored-rectangle
      (progn
	(setq cua--rectangle cua--restored-rectangle
	      cua--restored-rectangle nil
	      mark-active t
	      deactivate-mark nil)
	(cua--rectangle-set-corners))
    (when (and cua--rectangle cua--buffer-and-point-before-command
               (equal (car cua--buffer-and-point-before-command) (current-buffer))
               (not (= (cdr cua--buffer-and-point-before-command) (point))))
      (if (cua--rectangle-right-side)
          (cua--rectangle-right (current-column))
        (cua--rectangle-left (current-column)))
      (if (>= (cua--rectangle-corner) 2)
          (cua--rectangle-bot t)
        (cua--rectangle-top t))))
  (if cua--rectangle
      (if (and mark-active
               (not deactivate-mark))
          (cua--highlight-rectangle)
        (cua--deactivate-rectangle))
    (when cua--rectangle-overlays
      ;; clean-up after revert-buffer
      (mapc (function delete-overlay) cua--rectangle-overlays)
      (setq cua--rectangle-overlays nil)
      (setq deactivate-mark t)))
  (when cua--rect-undo-set-point
    (goto-char cua--rect-undo-set-point)
    (setq cua--rect-undo-set-point nil)))

;;; Initialization

(defun cua--rect-M/H-key (key cmd)
  (cua--M/H-key cua--rectangle-keymap key cmd))

(defun cua--init-rectangles ()
  (define-key cua--rectangle-keymap cua-rectangle-mark-key 'cua-clear-rectangle-mark)
  (define-key cua--region-keymap    cua-rectangle-mark-key 'cua-toggle-rectangle-mark)
  (unless (eq cua--rectangle-modifier-key 'meta)
    (cua--rect-M/H-key ?\s			       'cua-clear-rectangle-mark)
    (cua--M/H-key cua--region-keymap ?\s	       'cua-toggle-rectangle-mark))

  (define-key cua--rectangle-keymap [remap copy-region-as-kill] 'cua-copy-rectangle)
  (define-key cua--rectangle-keymap [remap kill-ring-save]      'cua-copy-rectangle)
  (define-key cua--rectangle-keymap [remap kill-region]         'cua-cut-rectangle)
  (define-key cua--rectangle-keymap [remap delete-char]         'cua-delete-rectangle)
  (define-key cua--rectangle-keymap [remap delete-forward-char] 'cua-delete-rectangle)
  (define-key cua--rectangle-keymap [remap set-mark-command]    'cua-toggle-rectangle-mark)

  (define-key cua--rectangle-keymap [remap forward-char]        'cua-resize-rectangle-right)
  (define-key cua--rectangle-keymap [remap backward-char]       'cua-resize-rectangle-left)
  (define-key cua--rectangle-keymap [remap next-line]           'cua-resize-rectangle-down)
  (define-key cua--rectangle-keymap [remap previous-line]       'cua-resize-rectangle-up)
  (define-key cua--rectangle-keymap [remap end-of-line]         'cua-resize-rectangle-eol)
  (define-key cua--rectangle-keymap [remap beginning-of-line]   'cua-resize-rectangle-bol)
  (define-key cua--rectangle-keymap [remap end-of-buffer]       'cua-resize-rectangle-bot)
  (define-key cua--rectangle-keymap [remap beginning-of-buffer] 'cua-resize-rectangle-top)
  (define-key cua--rectangle-keymap [remap scroll-down]         'cua-resize-rectangle-page-up)
  (define-key cua--rectangle-keymap [remap scroll-up]           'cua-resize-rectangle-page-down)
  (define-key cua--rectangle-keymap [remap scroll-down-command] 'cua-resize-rectangle-page-up)
  (define-key cua--rectangle-keymap [remap scroll-up-command]   'cua-resize-rectangle-page-down)

  (define-key cua--rectangle-keymap [remap delete-backward-char] 'cua-delete-char-rectangle)
  (define-key cua--rectangle-keymap [remap backward-delete-char] 'cua-delete-char-rectangle)
  (define-key cua--rectangle-keymap [remap backward-delete-char-untabify] 'cua-delete-char-rectangle)
  (define-key cua--rectangle-keymap [remap self-insert-command]	 'cua-insert-char-rectangle)
  (define-key cua--rectangle-keymap [remap self-insert-iso]	 'cua-insert-char-rectangle)

  ;; Catch self-inserting characters which are "stolen" by other modes
  (define-key cua--rectangle-keymap [t]
    '(menu-item "sic" cua-insert-char-rectangle :filter cua--self-insert-char-p))

  (define-key cua--rectangle-keymap "\r"     'cua-rotate-rectangle)
  (define-key cua--rectangle-keymap "\t"     'cua-indent-rectangle)

  (define-key cua--rectangle-keymap [(control ??)] 'cua-help-for-rectangle)

  (define-key cua--rectangle-keymap [mouse-1]	   'cua-mouse-set-rectangle-mark)
  (define-key cua--rectangle-keymap [down-mouse-1] 'cua--mouse-ignore)
  (define-key cua--rectangle-keymap [drag-mouse-1] 'cua--mouse-ignore)
  (define-key cua--rectangle-keymap [mouse-3]	   'cua-mouse-save-then-kill-rectangle)
  (define-key cua--rectangle-keymap [down-mouse-3] 'cua--mouse-ignore)
  (define-key cua--rectangle-keymap [drag-mouse-3] 'cua--mouse-ignore)

  (cua--rect-M/H-key 'up    'cua-move-rectangle-up)
  (cua--rect-M/H-key 'down  'cua-move-rectangle-down)
  (cua--rect-M/H-key 'left  'cua-move-rectangle-left)
  (cua--rect-M/H-key 'right 'cua-move-rectangle-right)

  (cua--rect-M/H-key '(control up)   'cua-scroll-rectangle-up)
  (cua--rect-M/H-key '(control down) 'cua-scroll-rectangle-down)

  (cua--rect-M/H-key ?a	'cua-align-rectangle)
  (cua--rect-M/H-key ?b	'cua-blank-rectangle)
  (cua--rect-M/H-key ?c	'cua-close-rectangle)
  (cua--rect-M/H-key ?f	'cua-fill-char-rectangle)
  (cua--rect-M/H-key ?i	'cua-incr-rectangle)
  (cua--rect-M/H-key ?k	'cua-cut-rectangle-as-text)
  (cua--rect-M/H-key ?l	'cua-downcase-rectangle)
  (cua--rect-M/H-key ?m	'cua-copy-rectangle-as-text)
  (cua--rect-M/H-key ?n	'cua-sequence-rectangle)
  (cua--rect-M/H-key ?o	'cua-open-rectangle)
  (cua--rect-M/H-key ?p	'cua-toggle-rectangle-virtual-edges)
  (cua--rect-M/H-key ?P	'cua-do-rectangle-padding)
  (cua--rect-M/H-key ?q	'cua-refill-rectangle)
  (cua--rect-M/H-key ?r	'cua-replace-in-rectangle)
  (cua--rect-M/H-key ?R	'cua-reverse-rectangle)
  (cua--rect-M/H-key ?s	'cua-string-rectangle)
  (cua--rect-M/H-key ?t	'cua-text-fill-rectangle)
  (cua--rect-M/H-key ?u	'cua-upcase-rectangle)
  (cua--rect-M/H-key ?|	'cua-shell-command-on-rectangle)
  (cua--rect-M/H-key ?'	'cua-restrict-prefix-rectangle)
  (cua--rect-M/H-key ?/	'cua-restrict-regexp-rectangle)

  (setq cua--rectangle-initialized t))

(provide 'cua-rect)

;;; cua-rect.el ends here
