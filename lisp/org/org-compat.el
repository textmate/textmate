;;; org-compat.el --- Compatibility code for Org-mode

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code needed for compatibility with XEmacs and older
;; versions of GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org-macs)

(declare-function find-library-name "find-func"  (library))
(declare-function w32-focus-frame "term/w32-win" (frame))

;; The following constant is for backward compatibility.  We do not use
;; it in org-mode, because the Byte compiler evaluates (featurep 'xemacs)
;; at compilation time and can therefore optimize code better.
(defconst org-xemacs-p (featurep 'xemacs))
(defconst org-format-transports-properties-p
  (let ((x "a"))
    (add-text-properties 0 1 '(test t) x)
    (get-text-property 0 'test (format "%s" x)))
  "Does format transport text properties?")

(defun org-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports it,
just inherit the face.  If INHERITS is set and the Emacs version does
not support it, copy the face specification from the inheritance face.
If INHERITS is not given and SPECS is, use SPECS to define the face.
XEmacs and Emacs 21 do not know about the `min-colors' attribute.
For them we convert a (min-colors 8) entry to a `tty' entry and move it
to the top of the list.  The `min-colors' attribute will be removed from
any other entries, and any resulting duplicates will be removed entirely."
  (when (and inherits (facep inherits) (not specs))
    (setq specs (or specs
		    (get inherits 'saved-face)
		    (get inherits 'face-defface-spec))))
  (cond
   ((and inherits (facep inherits)
	 (not (featurep 'xemacs))
	 (>= emacs-major-version 22)
	 ;; do not inherit outline faces before Emacs 23
	 (or (>= emacs-major-version 23)
	     (not (string-match "\\`outline-[0-9]+"
				(symbol-name inherits)))))
    (list (list t :inherit inherits)))
   ((or (featurep 'xemacs) (< emacs-major-version 22))
    ;; These do not understand the `min-colors' attribute.
    (let (r e a)
      (while (setq e (pop specs))
	(cond
	 ((memq (car e) '(t default)) (push e r))
	 ((setq a (member '(min-colors 8) (car e)))
	  (nconc r (list (cons (cons '(type tty) (delq (car a) (car e)))
			       (cdr e)))))
	 ((setq a (assq 'min-colors (car e)))
	  (setq e (cons (delq a (car e)) (cdr e)))
	  (or (assoc (car e) r) (push e r)))
	 (t (or (assoc (car e) r) (push e r)))))
      (nreverse r)))
   (t specs)))
(put 'org-compatible-face 'lisp-indent-function 1)

(defun org-version-check (version feature level)
  (let* ((v1 (mapcar 'string-to-number (split-string version "[.]")))
	 (v2 (mapcar 'string-to-number (split-string emacs-version "[.]")))
	 (rmaj (or (nth 0 v1) 99))
	 (rmin (or (nth 1 v1) 99))
	 (rbld (or (nth 2 v1) 99))
	 (maj (or (nth 0 v2) 0))
	 (min (or (nth 1 v2) 0))
	 (bld (or (nth 2 v2) 0)))
    (if (or (< maj rmaj)
	    (and (= maj rmaj)
		 (< min rmin))
	    (and (= maj rmaj)
		 (= min rmin)
		 (< bld rbld)))
	(if (eq level :predicate)
	    ;; just return if we have the version
	    nil
	  (let ((msg (format "Emacs %s or greater is recommended for %s"
			     version feature)))
	    (display-warning 'org msg level)
	    t))
      t)))

;;;; Emacs/XEmacs compatibility

;; Keys
(defconst org-xemacs-key-equivalents
  '(([mouse-1] . [button1])
    ([mouse-2] . [button2])
    ([mouse-3] . [button3])
    ([C-mouse-4] . [(control mouse-4)])
    ([C-mouse-5] . [(control mouse-5)]))
  "Translation alist for a couple of keys.")

;; Overlay compatibility functions
(defun org-detach-overlay (ovl)
  (if (featurep 'xemacs) (detach-extent ovl) (delete-overlay ovl)))
(defun org-overlay-display (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if (featurep 'xemacs)
      (let ((gl (make-glyph text)))
	(and face (set-glyph-face gl face))
	(set-extent-property ovl 'invisible t)
	(set-extent-property ovl 'end-glyph gl))
    (overlay-put ovl 'display text)
    (if face (overlay-put ovl 'face face))
    (if evap (overlay-put ovl 'evaporate t))))
(defun org-overlay-before-string (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if (featurep 'xemacs)
      (let ((gl (make-glyph text)))
	(and face (set-glyph-face gl face))
	(set-extent-property ovl 'begin-glyph gl))
    (if face (org-add-props text nil 'face face))
    (overlay-put ovl 'before-string text)
    (if evap (overlay-put ovl 'evaporate t))))
(defun org-find-overlays (prop &optional pos delete)
  "Find all overlays specifying PROP at POS or point.
If DELETE is non-nil, delete all those overlays."
  (let ((overlays (overlays-at (or pos (point))))
	ov found)
    (while (setq ov (pop overlays))
      (if (overlay-get ov prop)
          (if delete (delete-overlay ov) (push ov found))))
    found))

(defun org-get-x-clipboard (value)
  "Get the value of the x clipboard, compatible with XEmacs, and GNU Emacs 21."
  (if (eq window-system 'x)
      (let ((x (org-get-x-clipboard-compat value)))
	(if x (org-no-properties x)))))

(defsubst org-decompose-region (beg end)
  "Decompose from BEG to END."
  (if (featurep 'xemacs)
      (let ((modified-p (buffer-modified-p))
	    (buffer-read-only nil))
	(remove-text-properties beg end '(composition nil))
	(set-buffer-modified-p modified-p))
    (decompose-region beg end)))

;; Miscellaneous functions

(defun org-add-hook (hook function &optional append local)
  "Add-hook, compatible with both Emacsen."
  (if (and local (featurep 'xemacs))
      (add-local-hook hook function append)
    (add-hook hook function append local)))

(defun org-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)
(put 'org-add-props 'lisp-indent-function 2)

(defun org-fit-window-to-buffer (&optional window max-height min-height
					   shrink-only)
  "Fit WINDOW to the buffer, but only if it is not a side-by-side window.
WINDOW defaults to the selected window.  MAX-HEIGHT and MIN-HEIGHT are
passed through to `fit-window-to-buffer'.  If SHRINK-ONLY is set, call
`shrink-window-if-larger-than-buffer' instead, the height limit is
ignored in this case."
  (cond ((if (fboundp 'window-full-width-p)
	     (not (window-full-width-p window))
	   (> (frame-width) (window-width window)))
	 ;; do nothing if another window would suffer
	 )
	((and (fboundp 'fit-window-to-buffer) (not shrink-only))
	 (fit-window-to-buffer window max-height min-height))
	((fboundp 'shrink-window-if-larger-than-buffer)
	 (shrink-window-if-larger-than-buffer window)))
  (or window (selected-window)))

(defun org-number-sequence (from &optional to inc)
  "Call `number-sequence or emulate it."
  (if (fboundp 'number-sequence)
      (number-sequence from to inc)
    (if (or (not to) (= from to))
	(list from)
      (or inc (setq inc 1))
      (when (zerop inc) (error "The increment can not be zero"))
      (let (seq (n 0) (next from))
	(if (> inc 0)
	    (while (<= next to)
	      (setq seq (cons next seq)
		    n (1+ n)
		    next (+ from (* n inc))))
	  (while (>= next to)
	    (setq seq (cons next seq)
		  n (1+ n)
		  next (+ from (* n inc)))))
	(nreverse seq)))))

;; Region compatibility

(defvar org-ignore-region nil
  "To temporarily disable the active region.")

(defun org-region-active-p ()
  "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
  (if org-ignore-region
      nil
    (if (featurep 'xemacs)
	(and zmacs-regions (region-active-p))
      (if (fboundp 'use-region-p)
	  (use-region-p)
	(and transient-mark-mode mark-active))))) ; Emacs 22 and before

(defun org-cursor-to-region-beginning ()
  (when (and (org-region-active-p)
	     (> (point) (region-beginning)))
    (exchange-point-and-mark)))

;; Emacs 22 misses `activate-mark'
(if (fboundp 'activate-mark)
    (defalias 'org-activate-mark 'activate-mark)
  (defun org-activate-mark ()
    (when (mark t)
      (setq mark-active t)
      (when (and (boundp 'transient-mark-mode)
		 (not transient-mark-mode))
	(setq transient-mark-mode 'lambda))
      (when (boundp 'zmacs-regions)
	(setq zmacs-regions t)))))


;; Invisibility compatibility

(defun org-remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (fboundp 'remove-from-invisibility-spec)
      (remove-from-invisibility-spec arg)
    (if (consp buffer-invisibility-spec)
	(setq buffer-invisibility-spec
	      (delete arg buffer-invisibility-spec)))))

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (if (consp buffer-invisibility-spec)
      (member arg buffer-invisibility-spec)
    nil))

(defmacro org-xemacs-without-invisibility (&rest body)
  "Turn off extents with invisibility while executing BODY."
  `(let ((ext-inv (extent-list nil (point-at-bol) (point-at-eol)
			       'all-extents-closed-open 'invisible))
	 ext-inv-specs)
     (dolist (ext ext-inv)
       (when (extent-property ext 'invisible)
	 (add-to-list 'ext-inv-specs (list ext (extent-property
						ext 'invisible)))
	 (set-extent-property ext 'invisible nil)))
     ,@body
     (dolist (ext-inv-spec ext-inv-specs)
       (set-extent-property (car ext-inv-spec) 'invisible
			    (cadr ext-inv-spec)))))
(def-edebug-spec org-xemacs-without-invisibility (body))

(defun org-indent-to-column (column &optional minimum buffer)
  "Work around a bug with extents with invisibility in XEmacs."
  (if (featurep 'xemacs)
      (org-xemacs-without-invisibility (indent-to-column column minimum buffer))
    (indent-to-column column minimum)))

(defun org-indent-line-to (column)
  "Work around a bug with extents with invisibility in XEmacs."
  (if (featurep 'xemacs)
      (org-xemacs-without-invisibility (indent-line-to column))
    (indent-line-to column)))

(defun org-move-to-column (column &optional force buffer)
  (if (featurep 'xemacs)
      (org-xemacs-without-invisibility (move-to-column column force buffer))
    (move-to-column column force)))

(defun org-get-x-clipboard-compat (value)
  "Get the clipboard value on XEmacs or Emacs 21."
  (cond ((featurep 'xemacs)
	 (org-no-warnings (get-selection-no-error value)))
	((fboundp 'x-get-selection)
	 (condition-case nil
	     (or (x-get-selection value 'UTF8_STRING)
		 (x-get-selection value 'COMPOUND_TEXT)
		 (x-get-selection value 'STRING)
		 (x-get-selection value 'TEXT))
	   (error nil)))))

(defun org-propertize (string &rest properties)
  (if (featurep 'xemacs)
      (progn
	(add-text-properties 0 (length string) properties string)
	string)
    (apply 'propertize string properties)))

(defun org-substring-no-properties (string &optional from to)
  (if (featurep 'xemacs)
      (org-no-properties (substring string (or from 0) to))
    (substring-no-properties string from to)))

(defun org-find-library-name (library)
  (if (fboundp 'find-library-name)
      (file-name-directory (find-library-name library))
    ; XEmacs does not have `find-library-name'
    (flet ((find-library-name-helper (filename ignored-codesys)
				     filename)
	   (find-library-name (library)
	    (find-library library nil 'find-library-name-helper)))
      (file-name-directory (find-library-name library)))))

(defun org-count-lines (s)
  "How many lines in string S?"
  (let ((start 0) (n 1))
    (while (string-match "\n" s start)
      (setq start (match-end 0) n (1+ n)))
    (if (and (> (length s) 0) (= (aref s (1- (length s))) ?\n))
	(setq n (1- n)))
    n))

(defun org-kill-new (string &rest args)
  (remove-text-properties 0 (length string) '(line-prefix t wrap-prefix t)
			  string)
  (apply 'kill-new string args))

(defun org-select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible."
  (cond ((featurep 'xemacs)
	 (if (fboundp 'select-frame-set-input-focus)
	     (select-frame-set-input-focus frame)
	   (raise-frame frame)
	   (select-frame frame)
	   (focus-frame frame)))
	;; `select-frame-set-input-focus' defined in Emacs 21 will not
	;; set the input focus.
	((>= emacs-major-version 22)
	 (select-frame-set-input-focus frame))
	(t
	 (raise-frame frame)
	 (select-frame frame)
	 (cond ((memq window-system '(x ns mac))
		(x-focus-frame frame))
	       ((eq window-system 'w32)
		(w32-focus-frame frame)))
	 (when focus-follows-mouse
	   (set-mouse-position frame (1- (frame-width frame)) 0)))))

(defun org-float-time (&optional time)
  "Convert time value TIME to a floating point number.
TIME defaults to the current time."
  (if (featurep 'xemacs)
      (time-to-seconds (or time (current-time)))
    (float-time time)))

(if (fboundp 'string-match-p)
    (defalias 'org-string-match-p 'string-match-p)
  (defun org-string-match-p (regexp string &optional start)
    (save-match-data
      (funcall 'string-match regexp string start))))

(if (fboundp 'looking-at-p)
    (defalias 'org-looking-at-p 'looking-at-p)
  (defun org-looking-at-p (&rest args)
    (save-match-data
      (apply 'looking-at args))))

; XEmacs does not have `looking-back'.
(if (fboundp 'looking-back)
    (defalias 'org-looking-back 'looking-back)
  (defun org-looking-back (regexp &optional limit greedy)
    "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as
possible, stopping when a single additional previous character
cannot be part of a match for REGEXP.  When the match is
extended, its starting position is allowed to occur before
LIMIT."
    (let ((start (point))
	  (pos
	   (save-excursion
	     (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
		  (point)))))
      (if (and greedy pos)
	  (save-restriction
	    (narrow-to-region (point-min) start)
	    (while (and (> pos (point-min))
			(save-excursion
			  (goto-char pos)
			  (backward-char 1)
			  (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
	      (setq pos (1- pos)))
	    (save-excursion
	      (goto-char pos)
	      (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
      (not (null pos)))))

(defun org-floor* (x &optional y)
  "Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient."
  (let ((q (floor x y)))
    (list q (- x (if y (* y q) q)))))

;; `pop-to-buffer-same-window' has been introduced with Emacs 24.1.
(defun org-pop-to-buffer-same-window
  (&optional buffer-or-name norecord label)
  "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
  (if (fboundp 'pop-to-buffer-same-window)
      (funcall
       'pop-to-buffer-same-window buffer-or-name norecord)
    (funcall 'switch-to-buffer buffer-or-name norecord)))

(provide 'org-compat)

;;; org-compat.el ends here
