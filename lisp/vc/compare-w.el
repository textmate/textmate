;;; compare-w.el --- compare text between windows for Emacs

;; Copyright (C) 1986, 1989, 1993, 1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: convenience files vc

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

;; This package provides one entry point, compare-windows.  It compares
;; text starting from point in two adjacent windows, advancing point
;; until it finds a difference.  Option variables permit you to ignore
;; whitespace differences, or case differences, or both.

;;; Code:

(defgroup compare-windows nil
  "Compare text between windows."
  :prefix "compare-"
  :group 'tools)

(defcustom compare-windows-whitespace "\\(\\s-\\|\n\\)+"
  "Regexp or function that defines whitespace sequences for `compare-windows'.
That command optionally ignores changes in whitespace.

The value of `compare-windows-whitespace' is normally a regexp, but it
can also be a function.  The function's job is to categorize any
whitespace around (including before) point; it should also advance
past any whitespace.  The function is called in each window, with
point at the current scanning point.  It gets one argument, the point
where \\[compare-windows] was originally called; it should not look at
any text before that point.

If the function returns the same value for both windows, then the
whitespace is considered to match, and is skipped."
  :type '(choice regexp function)
  :group 'compare-windows)

(defcustom compare-ignore-whitespace nil
  "Non-nil means `compare-windows' ignores whitespace."
  :type 'boolean
  :group 'compare-windows
  :version "22.1")

(defcustom compare-ignore-case nil
  "Non-nil means `compare-windows' ignores case differences."
  :type 'boolean
  :group 'compare-windows)

(defcustom compare-windows-sync 'compare-windows-sync-default-function
  "Function or regexp that is used to synchronize points in two
windows if before calling `compare-windows' points are located
on mismatched positions.

The value of `compare-windows-sync' can be a function.  The
function's job is to advance points in both windows to the next
matching text.  If the value of `compare-windows-sync' is a
regexp, then points in both windows are advanced to the next
occurrence of this regexp.

The current default value is the general function
`compare-windows-sync-default-function' that is able to
synchronize points by using quadratic algorithm to find the first
matching 32-character string in two windows.

The other useful values of this variable could be such functions
as `forward-word', `forward-sentence', `forward-paragraph', or a
regexp containing some field separator or a newline, depending on
the nature of the difference units separator.  The variable can
be made buffer-local.

If the value of this variable is `nil' (option \"No sync\"), then
no synchronization is performed, and the function `ding' is called
to beep or flash the screen when points are mismatched."
  :type '(choice function regexp (const :tag "No sync" nil))
  :group 'compare-windows
  :version "22.1")

(defcustom compare-windows-sync-string-size 32
  "Size of string from one window that is searched in second window.

Small number makes difference regions more fine-grained, but it
may fail by finding the wrong match.  The bigger number makes
difference regions more coarse-grained.

The default value 32 is good for the most cases."
  :type 'integer
  :group 'compare-windows
  :version "22.1")

(defcustom compare-windows-recenter nil
  "List of two values, each of which is used as argument of
function `recenter' called in each of two windows to place
matching points side-by-side.

The value `(-1 0)' is useful if windows are split vertically,
and the value `((4) (4))' for horizontally split windows."
  :type '(list sexp sexp)
  :group 'compare-windows
  :version "22.1")

(defcustom compare-windows-highlight t
  "Non-nil means compare-windows highlights the differences.
The value t removes highlighting immediately after invoking a command
other than `compare-windows'.
The value `persistent' leaves all highlighted differences.  You can clear
out all highlighting later with the command `compare-windows-dehighlight'."
  :type '(choice (const :tag "No highlighting" nil)
		 (const :tag "Persistent highlighting" persistent)
		 (other :tag "Highlight until next command" t))
  :group 'compare-windows
  :version "22.1")

(defface compare-windows
  '((t :inherit lazy-highlight))
  "Face for highlighting of compare-windows difference regions."
  :group 'compare-windows
  :version "22.1")

(defvar compare-windows-overlay1 nil)
(defvar compare-windows-overlay2 nil)
(defvar compare-windows-overlays1 nil)
(defvar compare-windows-overlays2 nil)
(defvar compare-windows-sync-point nil)

;;;###autoload
(defun compare-windows (ignore-whitespace)
  "Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match.

This command pushes the mark in each window
at the prior location of point in that window.
If both windows display the same buffer,
the mark is pushed twice in that buffer:
first in the other window, then in the selected window.

A prefix arg means reverse the value of variable
`compare-ignore-whitespace'.  If `compare-ignore-whitespace' is
nil, then a prefix arg means ignore changes in whitespace.  If
`compare-ignore-whitespace' is non-nil, then a prefix arg means
don't ignore changes in whitespace.  The variable
`compare-windows-whitespace' controls how whitespace is skipped.
If `compare-ignore-case' is non-nil, changes in case are also
ignored.

If `compare-windows-sync' is non-nil, then successive calls of
this command work in interlaced mode:
on first call it advances points to the next difference,
on second call it synchronizes points by skipping the difference,
on third call it again advances points to the next difference and so on."
  (interactive "P")
  (if compare-ignore-whitespace
      (setq ignore-whitespace (not ignore-whitespace)))
  (let* (p1 p2 maxp1 maxp2 b1 b2 w2
	    (progress 1)
	    (opoint1 (point))
	    opoint2
	    skip-func-1
	    skip-func-2
	    (sync-func (if (stringp compare-windows-sync)
                           'compare-windows-sync-regexp
                         compare-windows-sync)))
    (setq p1 (point) b1 (current-buffer))
    (setq w2 (next-window (selected-window)))
    (if (eq w2 (selected-window))
	(setq w2 (next-window (selected-window) nil 'visible)))
    (if (eq w2 (selected-window))
	(error "No other window"))
    (setq p2 (window-point w2)
	  b2 (window-buffer w2))
    (setq opoint2 p2)
    (setq maxp1 (point-max))

    (setq skip-func-1 (if ignore-whitespace
			  (if (stringp compare-windows-whitespace)
			      (lambda (pos)
				(compare-windows-skip-whitespace pos)
				t)
			    compare-windows-whitespace)))

    (with-current-buffer b2
      (setq skip-func-2 (if ignore-whitespace
			    (if (stringp compare-windows-whitespace)
			      (lambda (pos)
				(compare-windows-skip-whitespace pos)
				t)
			      compare-windows-whitespace)))
      (push-mark p2 t)
      (setq maxp2 (point-max)))
    (push-mark)

    (while (> progress 0)
      ;; If both windows have whitespace next to point,
      ;; optionally skip over it.
      (and skip-func-1
	   (save-excursion
	     (let (p1a p2a w1 w2 result1 result2)
	       (setq result1 (funcall skip-func-1 opoint1))
	       (setq p1a (point))
	       (set-buffer b2)
	       (goto-char p2)
	       (setq result2 (funcall skip-func-2 opoint2))
	       (setq p2a (point))
	       (if (and result1 result2 (eq result1 result2))
		   (setq p1 p1a
			 p2 p2a)))))

      (let ((size (min (- maxp1 p1) (- maxp2 p2)))
	    (case-fold-search compare-ignore-case))
	(setq progress (compare-buffer-substrings b2 p2 (+ size p2)
						  b1 p1 (+ size p1)))
	(setq progress (if (zerop progress) size (1- (abs progress))))
	(setq p1 (+ p1 progress) p2 (+ p2 progress)))
      ;; Advance point now rather than later, in case we're interrupted.
      (goto-char p1)
      (set-window-point w2 p2)
      (when compare-windows-recenter
        (recenter (car compare-windows-recenter))
        (with-selected-window w2 (recenter (cadr compare-windows-recenter)))))

    (if (= (point) opoint1)
	(if (not sync-func)
            (ding)
          ;; If points are not advanced (i.e. already on mismatch position),
          ;; then synchronize points between both windows
          (save-excursion
            (setq compare-windows-sync-point nil)
            (funcall sync-func)
            (setq p1 (point))
            (set-buffer b2)
            (goto-char p2)
            (funcall sync-func)
            (setq p2 (point)))
          (goto-char p1)
          (set-window-point w2 p2)
          (when compare-windows-recenter
            (recenter (car compare-windows-recenter))
            (with-selected-window w2 (recenter (cadr compare-windows-recenter))))
          ;; If points are still not synchronized, then ding
          (when (and (= p1 opoint1) (= p2 opoint2))
            ;; Display error message when current points in two windows
            ;; are unmatched and next matching points can't be found.
            (compare-windows-dehighlight)
            (ding)
            (message "No more matching points"))))))

;; Move forward over whatever might be called whitespace.
;; compare-windows-whitespace is a regexp that matches whitespace.
;; Match it at various starting points before the original point
;; and find the latest point at which a match ends.
;; Don't try starting points before START, though.
;; Value is non-nil if whitespace is found.
;; If there is whitespace before point, but none after,
;; then return t, but don't advance point.
(defun compare-windows-skip-whitespace (start)
  (let ((end (point))
	(beg (point))
	(opoint (point)))
    (while (or (and (looking-at compare-windows-whitespace)
		    (<= end (match-end 0))
		    ;; This match goes past END, so advance END.
		    (progn (setq end (match-end 0))
			   (> (point) start)))
	       (and (/= (point) start)
		    ;; Consider at least the char before point,
		    ;; unless it is also before START.
		    (= (point) opoint)))
      ;; keep going back until whitespace
      ;; doesn't extend to or past end
      (forward-char -1))
    (setq beg (point))
    (goto-char end)
    (or (/= beg opoint)
	(/= end opoint))))

;; Move forward to the next synchronization regexp.
(defun compare-windows-sync-regexp ()
  (if (stringp compare-windows-sync)
      (re-search-forward compare-windows-sync nil t)))

;; Function works in two passes: one call on each window.
;; On the first call both matching points are computed,
;; and one of them is stored in compare-windows-sync-point
;; to be used when this function is called on second window.
(defun compare-windows-sync-default-function ()
  (if (not compare-windows-sync-point)
      (let* ((w1 (selected-window))
             (w2 (next-window w1))
             (b2 (window-buffer w2))
             (point-max2 (with-current-buffer b2 (point-max)))
             (op2 (window-point w2))
             (op1 (point))
             (region-size compare-windows-sync-string-size)
             (string-size compare-windows-sync-string-size)
             in-bounds-p s1 p2 p12s p12)
        (while (and
                ;; until matching points are found
                (not p12s)
                ;; until size exceeds the maximum points of both buffers
                ;; (bounds below take care to not overdo in each of them)
                (or (setq in-bounds-p (< region-size (max (- (point-max) op1)
                                                          (- point-max2 op2))))
                    ;; until string size becomes smaller than 4
                    (> string-size 4)))
          (if in-bounds-p
              ;; make the next search in the double-sized region;
              ;; on first iteration it is 2*compare-windows-sync-string-size,
              ;; on last iterations it exceeds both buffers maximum points
              (setq region-size (* region-size 2))
            ;; if region size exceeds the maximum points of both buffers,
            ;; then start to halve the string size until 4;
            ;; this helps to find differences near the end of buffers
            (setq string-size (/ string-size 2)))
          (let ((p1 op1)
                (bound1 (- (min (+ op1 region-size) (point-max)) string-size))
                (bound2 (min (+ op2 region-size) point-max2)))
            (while (< p1 bound1)
              (setq s1 (buffer-substring-no-properties p1 (+ p1 string-size)))
              (setq p2 (with-current-buffer b2
                         (goto-char op2)
                         (let ((case-fold-search compare-ignore-case))
                           (search-forward s1 bound2 t))))
              (when p2
                (setq p2 (- p2 string-size))
                (setq p12s (cons (list (+ p1 p2) p1 p2) p12s)))
              (setq p1 (1+ p1)))))
        (when p12s
          ;; use closest matching points (i.e. points with minimal sum)
          (setq p12 (cdr (assq (apply 'min (mapcar 'car p12s)) p12s)))
          (goto-char (car p12))
          (compare-windows-highlight op1 (car p12) (current-buffer) w1
                                     op2 (cadr p12) b2 w2))
        (setq compare-windows-sync-point (or (cadr p12) t)))
    ;; else set point in the second window to the pre-calculated value
    (if (numberp compare-windows-sync-point)
        (goto-char compare-windows-sync-point))
    (setq compare-windows-sync-point nil)))

;; Highlight differences
(defun compare-windows-highlight (beg1 end1 b1 w1 beg2 end2 b2 w2)
  (when compare-windows-highlight
    (if compare-windows-overlay1
        (move-overlay compare-windows-overlay1 beg1 end1 b1)
      (setq compare-windows-overlay1 (make-overlay beg1 end1 b1))
      (overlay-put compare-windows-overlay1 'face 'compare-windows)
      (overlay-put compare-windows-overlay1 'priority 1000))
    (overlay-put compare-windows-overlay1 'window w1)
    (if compare-windows-overlay2
        (move-overlay compare-windows-overlay2 beg2 end2 b2)
      (setq compare-windows-overlay2 (make-overlay beg2 end2 b2))
      (overlay-put compare-windows-overlay2 'face 'compare-windows)
      (overlay-put compare-windows-overlay2 'priority 1000))
    (overlay-put compare-windows-overlay2 'window w2)
    (if (not (eq compare-windows-highlight 'persistent))
	;; Remove highlighting before next command is executed
	(add-hook 'pre-command-hook 'compare-windows-dehighlight)
      (when compare-windows-overlay1
	(push (copy-overlay compare-windows-overlay1) compare-windows-overlays1)
	(delete-overlay compare-windows-overlay1))
      (when compare-windows-overlay2
	(push (copy-overlay compare-windows-overlay2) compare-windows-overlays2)
	(delete-overlay compare-windows-overlay2)))))

(defun compare-windows-dehighlight ()
  "Remove highlighting created by `compare-windows-highlight'."
  (interactive)
  (remove-hook 'pre-command-hook 'compare-windows-dehighlight)
  (mapc 'delete-overlay compare-windows-overlays1)
  (mapc 'delete-overlay compare-windows-overlays2)
  (and compare-windows-overlay1 (delete-overlay compare-windows-overlay1))
  (and compare-windows-overlay2 (delete-overlay compare-windows-overlay2)))

(provide 'compare-w)

;;; compare-w.el ends here
