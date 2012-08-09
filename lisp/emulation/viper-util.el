;;; viper-util.el --- Utilities used by viper.el

;; Copyright (C) 1994-1997, 1999-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: viper

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

(provide 'viper-util)


;; Compiler pacifier
(defvar viper-overriding-map)
(defvar pm-color-alist)
(defvar viper-minibuffer-current-face)
(defvar viper-minibuffer-insert-face)
(defvar viper-minibuffer-vi-face)
(defvar viper-minibuffer-emacs-face)
(defvar viper-replace-overlay-face)
(defvar viper-fast-keyseq-timeout)
(defvar ex-unix-type-shell)
(defvar ex-unix-type-shell-options)
(defvar viper-ex-tmp-buf-name)
(defvar viper-syntax-preference)
(defvar viper-saved-mark)

(require 'ring)

(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest  r))))

;; end pacifier

(require 'viper-init)



(defalias 'viper-overlay-p
  (if (featurep 'xemacs) 'extentp 'overlayp))
(defalias 'viper-make-overlay
  (if (featurep 'xemacs) 'make-extent 'make-overlay))
(defalias 'viper-overlay-live-p
  (if (featurep 'xemacs) 'extent-live-p 'overlayp))
(defalias 'viper-move-overlay
  (if (featurep 'xemacs) 'set-extent-endpoints 'move-overlay))
(defalias 'viper-overlay-start
  (if (featurep 'xemacs) 'extent-start-position 'overlay-start))
(defalias 'viper-overlay-end
  (if (featurep 'xemacs) 'extent-end-position 'overlay-end))
(defalias 'viper-overlay-get
  (if (featurep 'xemacs) 'extent-property 'overlay-get))
(defalias 'viper-overlay-put
  (if (featurep 'xemacs) 'set-extent-property 'overlay-put))
(defalias 'viper-read-event
  (if (featurep 'xemacs) 'next-command-event 'read-event))
(defalias 'viper-characterp
  (if (featurep 'xemacs) 'characterp 'integerp))
(defalias 'viper-int-to-char
  (if (featurep 'xemacs) 'int-to-char 'identity))
(defalias 'viper-get-face
  (if (featurep 'xemacs) 'get-face 'facep))
(defalias 'viper-color-defined-p
  (if (featurep 'xemacs) 'valid-color-name-p 'x-color-defined-p))
(defalias 'viper-iconify
  (if (featurep 'xemacs) 'iconify-frame 'iconify-or-deiconify-frame))


;; CHAR is supposed to be a char or an integer (positive or negative)
;; LIST is a list of chars, nil, and negative numbers
;; Check if CHAR is a member by trying to convert in characters, if necessary.
;; Introduced for compatibility with XEmacs, where integers are not the same as
;; chars.
(defun viper-memq-char (char list)
  (cond ((and (integerp char) (>= char 0))
	 (memq (viper-int-to-char char) list))
	((memq char list))))

;; Check if char-or-int and char are the same as characters
(defun viper-char-equal (char-or-int char)
  (cond ((and (integerp char-or-int) (>= char-or-int 0))
	 (= (viper-int-to-char char-or-int) char))
	((eq char-or-int char))))

;; Like =, but accommodates null and also is t for eq-objects
(defun viper= (char char1)
  (cond ((eq char char1) t)
	((and (viper-characterp char) (viper-characterp char1))
	 (= char char1))
	(t nil)))

(defsubst viper-color-display-p ()
  (if (featurep 'xemacs) (eq (device-class (selected-device)) 'color)
    (x-display-color-p)))

(defun viper-get-cursor-color (&optional frame)
  (if (featurep 'xemacs)
      (color-instance-name
       (frame-property (or frame (selected-frame)) 'cursor-color))
    (cdr (assoc 'cursor-color (frame-parameters)))))

(defmacro viper-frame-value (variable)
  "Return the value of VARIABLE local to the current frame, if there is one.
Otherwise return the normal value."
  `(if (featurep 'xemacs)
       ,variable
     ;; Frame-local variables are obsolete from Emacs 22.2 onwards,
     ;; so we do it by hand instead.
     ;; Buffer-local values take precedence over frame-local ones.
     (if (local-variable-p ',variable)
	 ,variable
       ;; Distinguish between no frame parameter and a frame parameter
       ;; with a value of nil.
       (let ((fp (assoc ',variable (frame-parameters))))
	 (if fp (cdr fp)
	   ,variable)))))

;; OS/2
(cond ((eq (viper-device-type) 'pm)
       (fset 'viper-color-defined-p
	     (lambda (color) (assoc color pm-color-alist)))))


;; cursor colors
(defun viper-change-cursor-color (new-color &optional frame)
  (if (and (viper-window-display-p)  (viper-color-display-p)
	   (stringp new-color) (viper-color-defined-p new-color)
	   (not (string= new-color (viper-get-cursor-color))))
      (if (featurep 'xemacs)
          (set-frame-property
           (or frame (selected-frame))
           'cursor-color (make-color-instance new-color))
        (modify-frame-parameters
         (or frame (selected-frame))
         (list (cons 'cursor-color new-color))))))

;; Note that the colors this function uses might not be those
;; associated with FRAME, if there are frame-local values.
;; This was equally true before the advent of viper-frame-value.
;; Now it could be changed by passing frame to v-f-v.
(defun viper-set-cursor-color-according-to-state (&optional frame)
  (cond ((eq viper-current-state 'replace-state)
	 (viper-change-cursor-color
	  (viper-frame-value viper-replace-overlay-cursor-color)
	  frame))
	((and (eq viper-current-state 'emacs-state)
	      (viper-frame-value viper-emacs-state-cursor-color))
	 (viper-change-cursor-color
	  (viper-frame-value viper-emacs-state-cursor-color)
	  frame))
	((eq viper-current-state 'insert-state)
	 (viper-change-cursor-color
	  (viper-frame-value viper-insert-state-cursor-color)
	  frame))
	(t
	 (viper-change-cursor-color
	  (viper-frame-value viper-vi-state-cursor-color)
	  frame))))

;; By default, saves current frame cursor color before changing viper state
(defun viper-save-cursor-color (before-which-mode)
  (if (and (viper-window-display-p) (viper-color-display-p))
      (let ((color (viper-get-cursor-color)))
	(if (and (stringp color) (viper-color-defined-p color)
		 ;; there is something fishy in that the color is not saved if
		 ;; it is the same as frames default cursor color. need to be
		 ;; checked.
		 (not (string= color
			       (viper-frame-value
				viper-replace-overlay-cursor-color))))
	    (modify-frame-parameters
	     (selected-frame)
	     (list
	      (cons
	       (cond ((eq before-which-mode 'before-replace-mode)
		      'viper-saved-cursor-color-in-replace-mode)
		     ((eq before-which-mode 'before-emacs-mode)
		      'viper-saved-cursor-color-in-emacs-mode)
		     (t
		      'viper-saved-cursor-color-in-insert-mode))
	       color)))))))


(defun viper-get-saved-cursor-color-in-replace-mode ()
  (or
   (funcall
    (if (featurep 'emacs) 'frame-parameter 'frame-property)
    (selected-frame)
    'viper-saved-cursor-color-in-replace-mode)
   (or (and (eq viper-current-state 'emacs-mode)
	    (viper-frame-value viper-emacs-state-cursor-color))
       (viper-frame-value viper-vi-state-cursor-color))))

(defun viper-get-saved-cursor-color-in-insert-mode ()
  (or
   (funcall
    (if (featurep 'emacs) 'frame-parameter 'frame-property)
    (selected-frame)
    'viper-saved-cursor-color-in-insert-mode)
   (or (and (eq viper-current-state 'emacs-mode)
	    (viper-frame-value viper-emacs-state-cursor-color))
       (viper-frame-value viper-vi-state-cursor-color))))

(defun viper-get-saved-cursor-color-in-emacs-mode ()
  (or
   (funcall
    (if (featurep 'emacs) 'frame-parameter 'frame-property)
    (selected-frame)
    'viper-saved-cursor-color-in-emacs-mode)
   (viper-frame-value viper-vi-state-cursor-color)))

;; restore cursor color from replace overlay
(defun viper-restore-cursor-color(after-which-mode)
  (if (viper-overlay-p viper-replace-overlay)
      (viper-change-cursor-color
       (cond ((eq after-which-mode 'after-replace-mode)
	      (viper-get-saved-cursor-color-in-replace-mode))
	     ((eq after-which-mode 'after-emacs-mode)
	      (viper-get-saved-cursor-color-in-emacs-mode))
	     (t (viper-get-saved-cursor-color-in-insert-mode)))
       )))


;; Check the current version against the major and minor version numbers
;; using op: cur-vers op major.minor If emacs-major-version or
;; emacs-minor-version are not defined, we assume that the current version
;; is hopelessly outdated.  We assume that emacs-major-version and
;; emacs-minor-version are defined.  Otherwise, for Emacs/XEmacs 19, if the
;; current minor version is < 10 (xemacs) or < 23 (emacs) the return value
;; will be nil (when op is =, >, or >=) and t (when op is <, <=), which may be
;; incorrect.  However, this gives correct result in our cases, since we are
;; testing for sufficiently high Emacs versions.
(defun viper-check-version (op major minor &optional type-of-emacs)
  (if (and (boundp 'emacs-major-version) (boundp 'emacs-minor-version))
      (and (cond ((eq type-of-emacs 'xemacs) (featurep 'xemacs))
		 ((eq type-of-emacs 'emacs) (featurep 'emacs))
		 (t t))
	   (cond ((eq op '=) (and (= emacs-minor-version minor)
				  (= emacs-major-version major)))
		 ((memq op '(> >= < <=))
		  (and (or (funcall op emacs-major-version major)
			   (= emacs-major-version major))
		       (if (= emacs-major-version major)
			   (funcall op emacs-minor-version minor)
			 t)))
		 (t
		  (error "%S: Invalid op in viper-check-version" op))))
    (cond ((memq op '(= > >=)) nil)
	  ((memq op '(< <=)) t))))


(defun viper-get-visible-buffer-window (wind)
  (if (featurep 'xemacs)
      (get-buffer-window wind t)
    (get-buffer-window wind 'visible)))


;; Return line position.
;; If pos is 'start then returns position of line start.
;; If pos is 'end, returns line end.  If pos is 'mid, returns line center.
;; Pos = 'indent returns beginning of indentation.
;; Otherwise, returns point.  Current point is not moved in any case."
(defun viper-line-pos (pos)
  (let ((cur-pos (point))
        (result))
    (cond
     ((equal pos 'start)
      (beginning-of-line))
     ((equal pos 'end)
      (end-of-line))
     ((equal pos 'mid)
      (goto-char (+ (viper-line-pos 'start) (viper-line-pos 'end) 2)))
     ((equal pos 'indent)
      (back-to-indentation))
     (t   nil))
    (setq result (point))
    (goto-char cur-pos)
    result))

;; Emacs used to count each multibyte character as several positions in the buffer,
;; so we had to use Emacs's chars-in-region to count characters. Since 20.3,
;; Emacs counts multibyte characters as 1 position.  XEmacs has always been
;; counting each char as just one pos. So, now we can simply subtract beg from
;; end to determine the number of characters in a region.
(defun viper-chars-in-region (beg end &optional preserve-sign)
  ;;(let ((count (abs (if (fboundp 'chars-in-region)
  ;;    		(chars-in-region beg end)
  ;;    	      (- end beg)))))
  (let ((count (abs (- end beg))))
    (if (and (< end beg) preserve-sign)
	(- count)
      count)))

;; Test if POS is between BEG and END
(defsubst viper-pos-within-region (pos beg end)
  (and (>= pos (min beg end)) (>= (max beg end) pos)))


;; Like move-marker but creates a virgin marker if arg isn't already a marker.
;; The first argument must eval to a variable name.
;; Arguments: (var-name position &optional buffer).
;;
;; This is useful for moving markers that are supposed to be local.
;; For this, VAR-NAME should be made buffer-local with nil as a default.
;; Then, each time this var is used in `viper-move-marker-locally' in a new
;; buffer, a new marker will be created.
(defun viper-move-marker-locally (var pos &optional buffer)
  (if (markerp (eval var))
      ()
    (set var (make-marker)))
  (move-marker (eval var) pos buffer))


;; Print CONDITIONS as a message.
(defun viper-message-conditions (conditions)
  (let ((case (car conditions)) (msg (cdr conditions)))
    (if (null msg)
	(message "%s" case)
      (message "%s: %s" case (mapconcat 'prin1-to-string msg " ")))
    (beep 1)))



;;; List/alist utilities

;; Convert LIST to an alist
(defun viper-list-to-alist (lst)
  (let ((alist))
    (while lst
      (setq alist (cons (list (car lst)) alist))
      (setq lst (cdr lst)))
    alist))

;; Convert ALIST to a list.
(defun viper-alist-to-list (alst)
  (let ((lst))
    (while alst
      (setq lst (cons (car (car alst)) lst))
      (setq alst (cdr alst)))
    lst))

;; Filter ALIST using REGEXP.  Return alist whose elements match the regexp.
(defun viper-filter-alist (regexp alst)
  (interactive "s x")
  (let ((outalst) (inalst alst))
    (while (car inalst)
      (if (string-match regexp (car (car inalst)))
	  (setq outalst (cons (car inalst) outalst)))
      (setq inalst (cdr inalst)))
    outalst))

;; Filter LIST using REGEXP.  Return list whose elements match the regexp.
(defun viper-filter-list (regexp lst)
  (interactive "s x")
  (let ((outlst) (inlst lst))
    (while (car inlst)
      (if (string-match regexp (car inlst))
	  (setq outlst (cons (car inlst) outlst)))
      (setq inlst (cdr inlst)))
    outlst))


;; Append LIS2 to LIS1, both alists, by side-effect and returns LIS1
;; LIS2 is modified by filtering it: deleting its members of the form
;; \(car elt\) such that (car elt') is in LIS1.
(defun viper-append-filter-alist (lis1 lis2)
  (let ((temp lis1)
	elt)
    ;;filter-append the second list
    (while temp
      ;; delete all occurrences
      (while (setq elt (assoc (car (car temp)) lis2))
	(setq lis2 (delq elt lis2)))
      (setq temp (cdr temp)))

    (append lis1 lis2)))



(declare-function viper-forward-Word "viper-cmd" (arg))

;;; Support for :e, :r, :w file globbing

;; Glob the file spec.
;; This function is designed to work under Unix.
(defun viper-glob-unix-files (filespec)
  (let ((gshell
	 (cond (ex-unix-type-shell shell-file-name)
	       (t "sh"))) ; probably Unix anyway
	(gshell-options
	 ;; using cond in anticipation of further additions
	 (cond (ex-unix-type-shell-options)
	       ))
	(command (cond (viper-ms-style-os-p (format "\"ls -1 -d %s\"" filespec))
		       (t (format "ls -1 -d %s" filespec))))
	status)
    (with-current-buffer (get-buffer-create viper-ex-tmp-buf-name)
      (erase-buffer)
      (setq status
	    (if gshell-options
		(call-process gshell nil t nil
			      gshell-options
			      "-c"
			      command)
	      (call-process gshell nil t nil
			    "-c"
			    command)))
      (goto-char (point-min))
      ;; Issue an error, if no match.
      (unless (eq 0 status)
	(save-excursion
	  (skip-chars-forward " \t\n\j")
	  (if (looking-at "ls:")
	      (viper-forward-Word 1))
	  (error "%s: %s"
		 (if (stringp  gshell)
		     gshell
		   "shell")
		 (buffer-substring (point) (viper-line-pos 'end)))
	  ))
      (goto-char (point-min))
      (viper-get-filenames-from-buffer 'one-per-line))
    ))


;; Interpret the stuff in the buffer as a list of file names
;; return a list of file names listed in the buffer beginning at point
;; If optional arg is supplied, assume each filename is listed on a separate
;; line
(defun viper-get-filenames-from-buffer (&optional one-per-line)
  (let ((skip-chars (if one-per-line "\t\n" " \t\n"))
	 result fname delim)
    (skip-chars-forward skip-chars)
    (while (not (eobp))
      (if (cond ((looking-at "\"")
		 (setq delim ?\")
		 (re-search-forward "[^\"]+" nil t)) ; noerror
		((looking-at "'")
		 (setq delim ?')
		 (re-search-forward "[^']+" nil t)) ; noerror
		(t
		 (re-search-forward
		  (concat "[^" skip-chars "]+") nil t))) ;noerror
	  (setq fname
		(buffer-substring (match-beginning 0) (match-end 0))))
      (if delim
	  (forward-char 1))
      (skip-chars-forward " \t\n")
      (setq result (cons fname result)))
    result))

;; convert MS-DOS wildcards to regexp
(defun viper-wildcard-to-regexp (wcard)
  (with-current-buffer (get-buffer-create viper-ex-tmp-buf-name)
    (erase-buffer)
    (insert wcard)
    (goto-char (point-min))
    (while (not (eobp))
      (skip-chars-forward "^*?.\\\\")
      (cond ((eq (char-after (point)) ?*) (insert ".")(forward-char 1))
	    ((eq (char-after (point)) ?.) (insert "\\")(forward-char 1))
	    ((eq (char-after (point)) ?\\) (insert "\\")(forward-char 1))
	    ((eq (char-after (point)) ??) (delete-char 1)(insert ".")))
      )
    (buffer-string)
    ))


;; glob windows files
;; LIST is expected to be in reverse order
(defun viper-glob-mswindows-files (filespec)
  (let ((case-fold-search t)
	tmp tmp2)
    (with-current-buffer (get-buffer-create viper-ex-tmp-buf-name)
      (erase-buffer)
      (insert filespec)
      (goto-char (point-min))
      (setq tmp (viper-get-filenames-from-buffer))
      (while tmp
	(setq tmp2 (cons (directory-files
			  ;; the directory part
			  (or (file-name-directory (car tmp))
			      "")
			  t  ; return full names
			  ;; the regexp part: globs the file names
			  (concat "^"
				  (viper-wildcard-to-regexp
				   (file-name-nondirectory (car tmp)))
				  "$"))
			 tmp2))
	(setq tmp (cdr tmp)))
      (reverse (apply 'append tmp2)))))


;;; Insertion ring

;; Rotate RING's index.  DIRection can be positive or negative.
(defun viper-ring-rotate1 (ring dir)
  (if (and (ring-p ring) (> (ring-length ring) 0))
      (progn
	(setcar ring (cond ((> dir 0)
			    (ring-plus1 (car ring) (ring-length ring)))
			   ((< dir 0)
			    (ring-minus1 (car ring) (ring-length ring)))
			   ;; don't rotate if dir = 0
			   (t (car ring))))
	(viper-current-ring-item ring)
	)))

(defun viper-special-ring-rotate1 (ring dir)
  (if (memq viper-intermediate-command
	    '(repeating-display-destructive-command
	      repeating-insertion-from-ring))
      (viper-ring-rotate1 ring dir)
    ;; don't rotate otherwise
    (viper-ring-rotate1 ring 0)))

;; current ring item; if N is given, then so many items back from the
;; current
(defun viper-current-ring-item (ring &optional n)
  (setq n (or n 0))
  (if (and (ring-p ring) (> (ring-length ring) 0))
      (aref (cdr (cdr ring)) (mod (- (car ring) 1 n) (ring-length ring)))))

;; Push item onto ring.  The second argument is a ring-variable, not value.
(defun viper-push-onto-ring (item ring-var)
  (or (ring-p (eval ring-var))
      (set ring-var (make-ring (eval (intern (format "%S-size" ring-var))))))
  (or (null item) ; don't push nil
      (and (stringp item) (string= item "")) ; or empty strings
      (equal item (viper-current-ring-item (eval ring-var))) ; or old stuff
      ;; Since viper-set-destructive-command checks if we are inside
      ;; viper-repeat, we don't check whether this-command-keys is a `.'.  The
      ;; cmd viper-repeat makes a call to the current function only if `.' is
      ;; executing a command from the command history.  It doesn't call the
      ;; push-onto-ring function if `.' is simply repeating the last
      ;; destructive command.  We only check for ESC (which happens when we do
      ;; insert with a prefix argument, or if this-command-keys doesn't give
      ;; anything meaningful (in that case we don't know what to show to the
      ;; user).
      (and (eq ring-var 'viper-command-ring)
	   (string-match "\\([0-9]*\e\\|^[ \t]*$\\|escape\\)"
			 (viper-array-to-string (this-command-keys))))
      (viper-ring-insert (eval ring-var) item))
  )


;; removing elts from ring seems to break it
(defun viper-cleanup-ring (ring)
  (or (< (ring-length ring) 2)
      (null (viper-current-ring-item ring))
      ;; last and previous equal
      (if (equal (viper-current-ring-item ring)
		 (viper-current-ring-item ring 1))
	  (viper-ring-pop ring))))

;; ring-remove seems to be buggy, so we concocted this for our purposes.
(defun viper-ring-pop (ring)
  (let* ((ln (ring-length ring))
	 (vec (cdr (cdr ring)))
	 (veclen (length vec))
	 (hd (car ring))
	 (idx (max 0 (ring-minus1 hd ln)))
	 (top-elt (aref vec idx)))

	;; shift elements
	(while (< (1+ idx) veclen)
	  (aset vec idx (aref vec (1+ idx)))
	  (setq idx (1+ idx)))
	(aset vec idx nil)

	(setq hd (max 0 (ring-minus1 hd ln)))
	(if (= hd (1- ln)) (setq hd 0))
	(setcar ring hd) ; move head
	(setcar (cdr ring) (max 0 (1- ln))) ; adjust length
	top-elt
	))

(defun viper-ring-insert (ring item)
  (let* ((ln (ring-length ring))
	 (vec (cdr (cdr ring)))
	 (veclen (length vec))
	 (hd (car ring))
	 (vecpos-after-hd (if (= hd 0) ln hd))
	 (idx ln))

    (if (= ln veclen)
	(progn
	  (aset vec hd item) ; hd is always 1+ the actual head index in vec
	  (setcar ring (ring-plus1 hd ln)))
      (setcar (cdr ring) (1+ ln))
      (setcar ring (ring-plus1 vecpos-after-hd (1+ ln)))
      (while (and (>= idx vecpos-after-hd) (> ln 0))
	(aset vec idx (aref vec (1- idx)))
	(setq idx (1- idx)))
      (aset vec vecpos-after-hd item))
    item))


;;; String utilities

;; If STRING is longer than MAX-LEN, truncate it and print ...... instead
;; PRE-STRING is a string to prepend to the abbrev string.
;; POST-STRING is a string to append to the abbrev string.
;; ABBREV_SIGN is a string to be inserted before POST-STRING
;; if the orig string was truncated.
(defun viper-abbreviate-string (string max-len
				     pre-string post-string abbrev-sign)
  (let (truncated-str)
    (setq truncated-str
	  (if (stringp string)
	      (substring string 0 (min max-len (length string)))))
    (cond ((null truncated-str) "")
	  ((> (length string) max-len)
	   (format "%s%s%s%s"
		   pre-string truncated-str abbrev-sign post-string))
	  (t (format "%s%s%s" pre-string truncated-str post-string)))))

;; tells if we are over a whitespace-only line
(defsubst viper-over-whitespace-line ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))


;;; Saving settings in custom file

;; Save the current setting of VAR in CUSTOM-FILE.
;; If given, MESSAGE is a message to be displayed after that.
;; This message is erased after 2 secs, if erase-msg is non-nil.
;; Arguments: var message custom-file &optional erase-message
(defun viper-save-setting (var message custom-file &optional erase-msg)
  (let* ((var-name (symbol-name var))
	 (var-val (if (boundp var) (eval var)))
	 (regexp (format "^[^;]*%s[ \t\n]*[a-zA-Z---_']*[ \t\n)]" var-name))
	 (buf (find-file-noselect (substitute-in-file-name custom-file)))
	)
    (message "%s" (or message ""))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
	  (let ((reg-end (1- (match-end 0))))
	    (search-backward var-name)
	    (delete-region (match-beginning 0) reg-end)
	    (goto-char (match-beginning 0))
	    (insert (format "%s  '%S" var-name var-val)))
	(goto-char (point-max))
	(if (not (bolp)) (insert "\n"))
	(insert (format "(setq %s '%S)\n" var-name var-val)))
      (save-buffer))
      (kill-buffer buf)
      (if erase-msg
	  (progn
	    (sit-for 2)
	    (message "")))
      ))

;; Save STRING in CUSTOM-FILE.  If PATTERN is non-nil, remove strings that
;; match this pattern.
(defun viper-save-string-in-file (string custom-file &optional pattern)
  (let ((buf (find-file-noselect (substitute-in-file-name custom-file))))
    (with-current-buffer buf
      (let (buffer-read-only)
	(goto-char (point-min))
	(if pattern (delete-matching-lines pattern))
	(goto-char (point-max))
	(if string (insert string))
	(save-buffer)))
    (kill-buffer buf)
    ))


;; This is a simple-minded check for whether a file is under version control.
;; If file,v exists but file doesn't, this file is considered to be not checked
;; in and not checked out for the purpose of patching (since patch won't be
;; able to read such a file anyway).
;; FILE is a string representing file name
;;(defun viper-file-under-version-control (file)
;;  (let* ((filedir (file-name-directory file))
;;	 (file-nondir (file-name-nondirectory file))
;;	 (trial (concat file-nondir ",v"))
;;	 (full-trial (concat filedir trial))
;;	 (full-rcs-trial (concat filedir "RCS/" trial)))
;;    (and (stringp file)
;;	 (file-exists-p file)
;;	 (or
;;	  (and
;;	   (file-exists-p full-trial)
;;	   ;; in FAT FS, `file,v' and `file' may turn out to be the same!
;;	   ;; don't be fooled by this!
;;	   (not (equal (file-attributes file)
;;		       (file-attributes full-trial))))
;;	  ;; check if a version is in RCS/ directory
;;	  (file-exists-p full-rcs-trial)))
;;       ))


(defsubst viper-file-checked-in-p (file)
  (and (featurep 'vc-hooks)
       ;; CVS files are considered not checked in
       ;; FIXME: Should this deal with more than CVS?
       (not (memq (vc-backend file) '(nil CVS)))
       (if (fboundp 'vc-state)
	   (and
	     (not (memq (vc-state file) '(edited needs-merge)))
	     (not (stringp (vc-state file))))
	 ;; XEmacs has no vc-state
	 (if (featurep 'xemacs) (not (vc-locking-user file))))))

;; checkout if visited file is checked in
(defun viper-maybe-checkout (buf)
  (let ((file (expand-file-name (buffer-file-name buf)))
	(checkout-function (key-binding "\C-x\C-q")))
    (if (and (viper-file-checked-in-p file)
	     (or (beep 1) t)
	     (y-or-n-p
	      (format
	       "File %s is checked in.  Check it out? "
	       (viper-abbreviate-file-name file))))
	(with-current-buffer buf
	  (command-execute checkout-function)))))




;;; Overlays
(defun viper-put-on-search-overlay (beg end)
  (if (viper-overlay-p viper-search-overlay)
      (viper-move-overlay viper-search-overlay beg end)
    (setq viper-search-overlay (viper-make-overlay beg end (current-buffer)))
    (viper-overlay-put
     viper-search-overlay 'priority viper-search-overlay-priority))
  (viper-overlay-put viper-search-overlay 'face viper-search-face))

;; Search

(defun viper-flash-search-pattern ()
  (if (not (viper-has-face-support-p))
      nil
    (viper-put-on-search-overlay (match-beginning 0) (match-end 0))
    (sit-for 2)
    (viper-overlay-put viper-search-overlay 'face nil)))

(defun viper-hide-search-overlay ()
  (if (not (viper-overlay-p viper-search-overlay))
      (progn
	(setq viper-search-overlay
	      (viper-make-overlay (point-min) (point-min) (current-buffer)))
	(viper-overlay-put
	 viper-search-overlay 'priority viper-search-overlay-priority)))
  (viper-overlay-put viper-search-overlay 'face nil))

;; Replace state

(defsubst viper-move-replace-overlay (beg end)
  (viper-move-overlay viper-replace-overlay beg end))

(defun viper-set-replace-overlay (beg end)
  (if (viper-overlay-live-p viper-replace-overlay)
      (viper-move-replace-overlay beg end)
    (setq viper-replace-overlay (viper-make-overlay beg end (current-buffer)))
    ;; never detach
    (viper-overlay-put
     viper-replace-overlay (if (featurep 'emacs) 'evaporate 'detachable) nil)
    (viper-overlay-put
     viper-replace-overlay 'priority viper-replace-overlay-priority)
    ;; If Emacs will start supporting overlay maps, as it currently supports
    ;; text-property maps, we could do away with viper-replace-minor-mode and
    ;; just have keymap attached to replace overlay.
    ;;(viper-overlay-put
    ;; viper-replace-overlay
    ;; (if (featurep 'xemacs) 'keymap 'local-map)
    ;; viper-replace-map)
    )
  (if (viper-has-face-support-p)
      (viper-overlay-put
       viper-replace-overlay 'face viper-replace-overlay-face))
  (viper-save-cursor-color 'before-replace-mode)
  (viper-change-cursor-color
   (viper-frame-value viper-replace-overlay-cursor-color)))


(defun viper-set-replace-overlay-glyphs (before-glyph after-glyph)
  (or (viper-overlay-live-p viper-replace-overlay)
      (viper-set-replace-overlay (point-min) (point-min)))
  (if (or (not (viper-has-face-support-p))
	  viper-use-replace-region-delimiters)
      (let ((before-name (if (featurep 'xemacs) 'begin-glyph 'before-string))
	    (after-name (if (featurep 'xemacs) 'end-glyph 'after-string)))
	(viper-overlay-put viper-replace-overlay before-name before-glyph)
	(viper-overlay-put viper-replace-overlay after-name after-glyph))))

(defun viper-hide-replace-overlay ()
  (viper-set-replace-overlay-glyphs nil nil)
  (viper-restore-cursor-color 'after-replace-mode)
  (viper-restore-cursor-color 'after-insert-mode)
  (if (viper-has-face-support-p)
      (viper-overlay-put viper-replace-overlay 'face nil)))


(defsubst viper-replace-start ()
  (viper-overlay-start viper-replace-overlay))
(defsubst viper-replace-end ()
  (viper-overlay-end viper-replace-overlay))


;; Minibuffer

(defun viper-set-minibuffer-overlay ()
  (viper-check-minibuffer-overlay)
  (when (viper-has-face-support-p)
    (viper-overlay-put
     viper-minibuffer-overlay 'face viper-minibuffer-current-face)
    (viper-overlay-put
     viper-minibuffer-overlay 'priority viper-minibuffer-overlay-priority)
    ;; never detach
    (viper-overlay-put
     viper-minibuffer-overlay
     (if (featurep 'emacs) 'evaporate 'detachable)
     nil)
    ;; make viper-minibuffer-overlay open-ended
    ;; In emacs, it is made open ended at creation time
    (when (featurep 'xemacs)
      (viper-overlay-put viper-minibuffer-overlay 'start-open nil)
      (viper-overlay-put viper-minibuffer-overlay 'end-open nil))))

(defun viper-check-minibuffer-overlay ()
  (if (viper-overlay-live-p viper-minibuffer-overlay)
      (viper-move-overlay
       viper-minibuffer-overlay
       (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) 1)
       (1+ (buffer-size)))
    (setq viper-minibuffer-overlay
	  (if (featurep 'xemacs)
	      (viper-make-overlay 1 (1+ (buffer-size)) (current-buffer))
	    ;; make overlay open-ended
	    (viper-make-overlay
	     (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) 1)
	     (1+ (buffer-size))
	     (current-buffer) nil 'rear-advance)))))


(defsubst viper-is-in-minibuffer ()
  (save-match-data
    (string-match "\*Minibuf-" (buffer-name))))



;;; XEmacs compatibility

(defun viper-abbreviate-file-name (file)
  (if (featurep 'xemacs)
      (abbreviate-file-name file t)    ; XEmacs requires addl argument
    (abbreviate-file-name file)))

;; Sit for VAL milliseconds.  XEmacs doesn't support the millisecond arg
;; in sit-for, so this function smooths out the differences.
(defsubst viper-sit-for-short (val &optional nodisp)
  (sit-for (/ val 1000.0) nodisp))

;; EVENT may be a single event of a sequence of events
(defsubst viper-ESC-event-p (event)
  (let ((ESC-keys '(?\e (control \[) escape))
	(key (viper-event-key event)))
    (member key ESC-keys)))

;; checks if object is a marker, has a buffer, and points to within that buffer
(defun viper-valid-marker (marker)
  (if (and (markerp marker) (marker-buffer marker))
      (let ((buf (marker-buffer marker))
	    (pos (marker-position marker)))
	(with-current-buffer buf
	  (and (<= pos (point-max)) (<= (point-min) pos))))))

(defsubst viper-mark-marker ()
  (if (featurep 'xemacs) (mark-marker t)
    (mark-marker)))

;; like (set-mark-command nil) but doesn't push twice, if (car mark-ring)
;; is the same as (mark t).
(defsubst viper-set-mark-if-necessary ()
  (setq mark-ring (delete (viper-mark-marker) mark-ring))
  (set-mark-command nil)
  (setq viper-saved-mark (point)))

;; In transient mark mode (zmacs mode), it is annoying when regions become
;; highlighted due to Viper's pushing marks.  So, we deactivate marks, unless
;; the user explicitly wants highlighting, e.g., by hitting '' or ``
(defun viper-deactivate-mark ()
  (if (featurep 'xemacs)
      (zmacs-deactivate-region)
    (deactivate-mark)))

(defsubst viper-leave-region-active ()
  (if (featurep 'xemacs) (setq zmacs-region-stays t)))

;; Check if arg is a valid character for register
;; TYPE is a list that can contain `letter', `Letter', and `digit'.
;; Letter means lowercase letters, Letter means uppercase letters, and
;; digit means digits from 1 to 9.
;; If TYPE is nil, then down/uppercase letters and digits are allowed.
(defun viper-valid-register (reg &optional type)
  (or type (setq type '(letter Letter digit)))
  (or (if (memq 'letter type)
	  (and (<= ?a reg) (<= reg ?z)))
      (if (memq 'digit type)
	  (and (<= ?1 reg) (<= reg ?9)))
      (if (memq 'Letter type)
	  (and (<= ?A reg) (<= reg ?Z)))
      ))



;; it is suggested that an event must be copied before it is assigned to
;; last-command-event in XEmacs
(defun viper-copy-event (event)
  (if (featurep 'xemacs) (copy-event event)
    event))

;; Uses different timeouts for ESC-sequences and others
(defun viper-fast-keysequence-p ()
  (not (viper-sit-for-short
	(if (viper-ESC-event-p last-input-event)
	    (viper-ESC-keyseq-timeout)
	  viper-fast-keyseq-timeout)
	t)))

;; like read-event, but in XEmacs also try to convert to char, if possible
(defun viper-read-event-convert-to-char ()
  (let (event)
    (if (featurep 'xemacs)
        (progn
          (setq event (next-command-event))
          (or (event-to-character event)
              event))
      (read-event))))

;; Viperized read-key-sequence
(defun viper-read-key-sequence (prompt &optional continue-echo)
  (let (inhibit-quit event keyseq)
    (setq keyseq (read-key-sequence prompt continue-echo))
    (setq event (if (featurep 'xemacs)
		    (elt keyseq 0) ; XEmacs returns vector of events
		  (elt (listify-key-sequence keyseq) 0)))
    (if (viper-ESC-event-p event)
	(let (unread-command-events)
	  (if (viper-fast-keysequence-p)
	      (let ((viper-vi-global-user-minor-mode  nil)
		    (viper-vi-local-user-minor-mode  nil)
		    (viper-vi-intercept-minor-mode nil)
		    (viper-insert-intercept-minor-mode nil)
		    (viper-replace-minor-mode nil) ; actually unnecessary
		    (viper-insert-global-user-minor-mode  nil)
		    (viper-insert-local-user-minor-mode  nil))
		;; Note: set unread-command-events only after testing for fast
		;; keysequence. Otherwise, viper-fast-keysequence-p will be
		;; always t -- whether there is anything after ESC or not
		(viper-set-unread-command-events keyseq)
		(setq keyseq (read-key-sequence nil)))
	    (viper-set-unread-command-events keyseq)
	    (setq keyseq (read-key-sequence nil)))))
    keyseq))


;; This function lets function-key-map convert key sequences into logical
;; keys.  This does a better job than viper-read-event when it comes to kbd
;; macros, since it enables certain macros to be shared between X and TTY modes
;; by correctly mapping key sequences for Left/Right/... (on an ascii
;; terminal) into logical keys left, right, etc.
(defun viper-read-key ()
  (let ((overriding-local-map viper-overriding-map)
	(inhibit-quit t)
	help-char key)
    (use-global-map viper-overriding-map)
    (unwind-protect
	(setq key (elt (viper-read-key-sequence nil) 0))
      (use-global-map global-map))
    key))


;; Emacs has a bug in eventp, which causes (eventp nil) to return (nil)
;; instead of nil, if '(nil) was previously inadvertently assigned to
;; unread-command-events
(defun viper-event-key (event)
  (or (and event (eventp event))
      (error "viper-event-key: Wrong type argument, eventp, %S" event))
  (when (if (featurep 'xemacs)
	 (or (key-press-event-p event) (mouse-event-p event)) ; xemacs
	 t ; emacs
	 )
    (let ((mod (event-modifiers event))
	  basis)
      (setq basis
	    (if (featurep 'xemacs)
	     ;; XEmacs
	     (cond ((key-press-event-p event)
		    (event-key event))
		   ((button-event-p event)
		    (concat "mouse-" (prin1-to-string (event-button event))))
		   (t
		    (error "viper-event-key: Unknown event, %S" event)))
	     ;; Emacs doesn't handle capital letters correctly, since
	     ;; \S-a isn't considered the same as A (it behaves as
	     ;; plain `a' instead).  So we take care of this here
	     (cond ((and (viper-characterp event) (<= ?A event) (<= event ?Z))
		    (setq mod nil
			  event event))
		   ;; Emacs has the oddity whereby characters 128+char
		   ;; represent M-char *if* this appears inside a string.
		   ;; So, we convert them manually to (meta char).
		   ((and (viper-characterp event)
			 (< ?\C-? event) (<= event 255))
		    (setq mod '(meta)
			  event (- event ?\C-? 1)))
		   ((and (null mod) (eq event 'return))
		    (setq event ?\C-m))
		   ((and (null mod) (eq event 'space))
		    (setq event ?\ ))
		   ((and (null mod) (eq event 'delete))
		    (setq event ?\C-?))
		   ((and (null mod) (eq event 'backspace))
		    (setq event ?\C-h))
		   (t (event-basic-type event)))
	     ) ; (featurep 'xemacs)
	    )
      (if (viper-characterp basis)
	  (setq basis
		(if (viper= basis ?\C-?)
		    (list 'control '\?) ; taking care of an emacs bug
		  (intern (char-to-string basis)))))
      (if mod
	  (append mod (list basis))
	basis))))

(defun viper-last-command-char ()
  (if (featurep 'xemacs)
      (event-to-character last-command-event)
    last-command-event))

(defun viper-key-to-emacs-key (key)
  (let (key-name char-p modifiers mod-char-list base-key base-key-name)
    (cond ((featurep 'xemacs) key)

	  ((symbolp key)
	   (setq key-name (symbol-name key))
	   (cond ((= (length key-name) 1) ; character event
		  (string-to-char key-name))
		 ;; Emacs doesn't recognize `return' and `escape' as events on
		 ;; dumb terminals, so we translate them into characters
		 ((and (featurep 'emacs) (not (viper-window-display-p))
		       (string= key-name "return"))
		  ?\C-m)
		 ((and (featurep 'emacs) (not (viper-window-display-p))
		       (string= key-name "escape"))
		  ?\e)
		 ;; pass symbol-event as is
		 (t key)))

	  ((listp key)
	   (setq modifiers (viper-subseq key 0 (1- (length key)))
		 base-key (viper-seq-last-elt key)
		 base-key-name (symbol-name base-key)
		 char-p (= (length base-key-name) 1))
	   (setq mod-char-list
		 (mapcar
		  (lambda (elt) (upcase (substring (symbol-name elt) 0 1)))
		  modifiers))
	   (if char-p
	       (setq key-name
		     (car (read-from-string
			   (concat
			    "?\\"
			    (mapconcat 'identity mod-char-list "-\\")
			    "-"
			    base-key-name))))
	     (setq key-name
		   (intern
		    (concat
		     (mapconcat 'identity mod-char-list "-")
		     "-"
		     base-key-name))))))
    ))


;; LIS is assumed to be a list of events of characters
(defun viper-eventify-list-xemacs (lis)
  (if (featurep 'xemacs)
      (mapcar
       (lambda (elt)
	 (cond ((viper-characterp elt) (character-to-event elt))
	       ((eventp elt)  elt)
	       (t (error
		   "viper-eventify-list-xemacs: can't convert to event, %S"
		   elt))))
       lis)))


;; Smooths out the difference between Emacs's unread-command-events
;; and XEmacs unread-command-event.  Arg is a character, an event, a list of
;; events or a sequence of keys.
;;
;; Due to the way unread-command-events in Emacs (not XEmacs), a non-event
;; symbol in unread-command-events list may cause Emacs to turn this symbol
;; into an event.  Below, we delete nil from event lists, since nil is the most
;; common symbol that might appear in this wrong context.
(defun viper-set-unread-command-events (arg)
  (if (featurep 'emacs)
      (setq
       unread-command-events
       (let ((new-events
	      (cond ((eventp arg) (list arg))
		    ((listp arg) arg)
		    ((sequencep arg)
		     (listify-key-sequence arg))
		    (t (error
			"viper-set-unread-command-events: Invalid argument, %S"
			arg)))))
	 (if (not (eventp nil))
	     (setq new-events (delq nil new-events)))
	 (append new-events unread-command-events)))
    ;; XEmacs
    (setq
     unread-command-events
     (append
      (cond ((viper-characterp arg) (list (character-to-event arg)))
	    ((eventp arg)  (list arg))
	    ((stringp arg) (mapcar 'character-to-event arg))
	    ((vectorp arg) (append arg nil)) ; turn into list
	    ((listp arg) (viper-eventify-list-xemacs arg))
	    (t (error
		"viper-set-unread-command-events: Invalid argument, %S" arg)))
      unread-command-events))))


;; Check if vec is a vector of key-press events representing characters
;; XEmacs only
(defun viper-event-vector-p (vec)
  (and (vectorp vec)
       (eval (cons 'and (mapcar (lambda (elt) (if (eventp elt) t)) vec)))))


;; check if vec is a vector of character symbols
(defun viper-char-symbol-sequence-p (vec)
  (and
   (sequencep vec)
   (eval
    (cons 'and
	  (mapcar (lambda (elt)
		    (and (symbolp elt) (= (length (symbol-name elt)) 1)))
		  vec)))))


(defun viper-char-array-p (array)
  (eval (cons 'and (mapcar 'viper-characterp array))))


;; Args can be a sequence of events, a string, or a Viper macro.  Will try to
;; convert events to keys and, if all keys are regular printable
;; characters, will return a string.  Otherwise, will return a string
;; representing a vector of converted events.  If the input was a Viper macro,
;; will return a string that represents this macro as a vector.
(defun viper-array-to-string (event-seq)
  (let (temp temp2)
    (cond ((stringp event-seq) event-seq)
	  ((viper-event-vector-p event-seq)
	    (setq temp (mapcar 'viper-event-key event-seq))
	    (cond ((viper-char-symbol-sequence-p temp)
		   (mapconcat 'symbol-name temp ""))
		  ((and (viper-char-array-p
			 (setq temp2 (mapcar 'viper-key-to-character temp))))
		   (mapconcat 'char-to-string temp2 ""))
		  (t (prin1-to-string (vconcat temp)))))
	  ((viper-char-symbol-sequence-p event-seq)
	   (mapconcat 'symbol-name event-seq ""))
	  ((and (vectorp event-seq)
		(viper-char-array-p
		 (setq temp (mapcar 'viper-key-to-character event-seq))))
	   (mapconcat 'char-to-string temp ""))
	  (t (prin1-to-string event-seq)))))

(defun viper-key-press-events-to-chars (events)
  (mapconcat (if (featurep 'xemacs)
		 (lambda (elt) (char-to-string (event-to-character elt))) ; xemacs
	       'char-to-string ; emacs
	       )
	     events
	     ""))


(defun viper-read-char-exclusive ()
  (let (char
	(echo-keystrokes 1))
    (while (null char)
      (condition-case nil
	  (setq char (read-char))
	(error
	 ;; skip event if not char
	 (viper-read-event))))
    char))

;; key is supposed to be in viper's representation, e.g., (control l), a
;; character, etc.
(defun viper-key-to-character (key)
  (cond ((eq key 'space) ?\ )
	((eq key 'delete) ?\C-?)
	((eq key 'return) ?\C-m)
	((eq key 'backspace) ?\C-h)
	((and (symbolp key)
	      (= 1 (length (symbol-name key))))
	 (string-to-char (symbol-name key)))
	((and (listp key)
	      (eq (car key) 'control)
	      (symbol-name (nth 1 key))
	      (= 1 (length (symbol-name (nth 1 key)))))
	 (read (format "?\\C-%s" (symbol-name (nth 1 key)))))
	(t key)))


(defun viper-setup-master-buffer (&rest other-files-or-buffers)
  "Set up the current buffer as a master buffer.
Arguments become related buffers.  This function should normally be used in
the `Local variables' section of a file."
  (setq viper-related-files-and-buffers-ring
	(make-ring (1+ (length other-files-or-buffers))))
  (mapc (lambda (elt)
	  (viper-ring-insert viper-related-files-and-buffers-ring elt))
	other-files-or-buffers)
  (viper-ring-insert viper-related-files-and-buffers-ring (buffer-name))
  )

;;; Movement utilities

;; Characters that should not be considered as part of the word, in reformed-vi
;; syntax mode.
;; Note: \\ (quoted \) must appear before `-' because this string is listified
;; into characters at some point and then put back to string. The result is
;; used in skip-chars-forward, which treats - specially. Here we achieve the
;; effect of quoting - and preventing it from being special.
(defconst viper-non-word-characters-reformed-vi
  "!@#$%^&*()\\-+=|\\~`{}[];:'\",<.>/?")
;; These are characters that are not to be considered as parts of a word in
;; Viper.
;; Set each time state changes and at loading time
(viper-deflocalvar viper-non-word-characters  nil)

;; must be buffer-local
(viper-deflocalvar viper-ALPHA-char-class "w"
  "String of syntax classes characterizing Viper's alphanumeric symbols.
In addition, the symbol `_' may be considered alphanumeric if
`viper-syntax-preference' is `strict-vi' or `reformed-vi'.")

(defconst viper-strict-ALPHA-chars "a-zA-Z0-9_"
  "Regexp matching the set of alphanumeric characters acceptable to strict
Vi.")
(defconst viper-strict-SEP-chars " \t\n"
  "Regexp matching the set of alphanumeric characters acceptable to strict
Vi.")
(defconst viper-strict-SEP-chars-sans-newline " \t"
  "Regexp matching the set of alphanumeric characters acceptable to strict
Vi.")

(defconst viper-SEP-char-class " -"
  "String of syntax classes for Vi separators.
Usually contains ` ', linefeed, TAB or formfeed.")


;; Set Viper syntax classes and related variables according to
;; `viper-syntax-preference'.
(defun viper-update-syntax-classes (&optional set-default)
  (let ((preference (cond ((eq viper-syntax-preference 'emacs)
			   "w")   ; Viper words have only Emacs word chars
			  ((eq viper-syntax-preference 'extended)
			   "w_")  ; Viper words have Emacs word & symbol chars
			  (t "w"))) ; Viper words are Emacs words plus `_'
	(non-word-chars (cond ((eq viper-syntax-preference 'reformed-vi)
			       (viper-string-to-list
				viper-non-word-characters-reformed-vi))
			      (t nil))))
    (if set-default
	(setq-default viper-ALPHA-char-class preference
		      viper-non-word-characters non-word-chars)
      (setq viper-ALPHA-char-class preference
	    viper-non-word-characters non-word-chars))
    ))

;; SYMBOL is used because customize requires it, but it is ignored, unless it
;; is `nil'.  If nil, use setq.
(defun viper-set-syntax-preference (&optional symbol value)
  "Set Viper syntax preference.
If called interactively or if SYMBOL is nil, sets syntax preference in current
buffer.  If called non-interactively, preferably via the customization widget,
sets the default value."
  (interactive)
  (or value
      (setq value
	    (completing-read
	     "Viper syntax preference: "
	     '(("strict-vi") ("reformed-vi") ("extended") ("emacs"))
	     nil 'require-match)))
  (if (stringp value) (setq value (intern value)))
  (or (memq value '(strict-vi reformed-vi extended emacs))
      (error "Invalid Viper syntax preference, %S" value))
  (if symbol
      (setq-default viper-syntax-preference value)
    (setq viper-syntax-preference value))
  (viper-update-syntax-classes))

(defcustom viper-syntax-preference 'reformed-vi
  "*Syntax type characterizing Viper's alphanumeric symbols.
Affects movement and change commands that deal with Vi-style words.
Works best when set in the hooks to various major modes.

`strict-vi' means Viper words are (hopefully) exactly as in Vi.

`reformed-vi' means Viper words are like Emacs words \(as determined using
Emacs syntax tables, which are different for different major modes\) with two
exceptions: the symbol `_' is always part of a word and typical Vi non-word
symbols, such as `,',:,\",),{, etc., are excluded.
This behaves very close to `strict-vi', but also works well with non-ASCII
characters from various alphabets.

`extended' means Viper word constituents are symbols that are marked as being
parts of words OR symbols in Emacs syntax tables.
This is most appropriate for major modes intended for editing programs.

`emacs' means Viper words are the same as Emacs words as specified by Emacs
syntax tables.
This option is appropriate if you like Emacs-style words."
  :type '(radio (const strict-vi) (const reformed-vi)
		 (const extended) (const emacs))
  :set 'viper-set-syntax-preference
  :group 'viper)
(make-variable-buffer-local 'viper-syntax-preference)


;; addl-chars are characters to be temporarily considered as alphanumerical
(defun viper-looking-at-alpha (&optional addl-chars)
  (or (stringp addl-chars) (setq addl-chars ""))
  (if (eq viper-syntax-preference 'reformed-vi)
      (setq addl-chars (concat addl-chars "_")))
  (let ((char (char-after (point))))
    (if char
	(if (eq viper-syntax-preference 'strict-vi)
	    (looking-at (concat "[" viper-strict-ALPHA-chars addl-chars "]"))
	  (or
	   ;; or one of the additional chars being asked to include
	   (viper-memq-char char (viper-string-to-list addl-chars))
	   (and
	    ;; not one of the excluded word chars (note:
	    ;; viper-non-word-characters is a list)
	    (not (viper-memq-char char viper-non-word-characters))
	    ;; char of the Viper-word syntax class
	    (viper-memq-char (char-syntax char)
			     (viper-string-to-list viper-ALPHA-char-class))))))
    ))

(defun viper-looking-at-separator ()
  (let ((char (char-after (point))))
    (if char
	(if (eq viper-syntax-preference 'strict-vi)
	    (viper-memq-char char (viper-string-to-list viper-strict-SEP-chars))
	  (or (eq char ?\n) ; RET is always a separator in Vi
	      (viper-memq-char (char-syntax char)
			       (viper-string-to-list viper-SEP-char-class)))))
    ))

(defsubst viper-looking-at-alphasep (&optional addl-chars)
  (or (viper-looking-at-separator) (viper-looking-at-alpha addl-chars)))

(defun viper-skip-alpha-forward (&optional addl-chars)
  (or (stringp addl-chars) (setq addl-chars ""))
  (viper-skip-syntax
   'forward
   (cond ((eq viper-syntax-preference 'strict-vi)
	  "")
	 (t viper-ALPHA-char-class))
   (cond ((eq viper-syntax-preference 'strict-vi)
	  (concat viper-strict-ALPHA-chars addl-chars))
	 (t addl-chars))))

(defun viper-skip-alpha-backward (&optional addl-chars)
  (or (stringp addl-chars) (setq addl-chars ""))
  (viper-skip-syntax
   'backward
   (cond ((eq viper-syntax-preference 'strict-vi)
	  "")
	 (t viper-ALPHA-char-class))
   (cond ((eq viper-syntax-preference 'strict-vi)
	  (concat viper-strict-ALPHA-chars addl-chars))
	 (t addl-chars))))

;; weird syntax tables may confuse strict-vi style
(defsubst viper-skip-all-separators-forward (&optional within-line)
  (if (eq viper-syntax-preference 'strict-vi)
      (if within-line
	  (skip-chars-forward viper-strict-SEP-chars-sans-newline)
	(skip-chars-forward viper-strict-SEP-chars))
    (viper-skip-syntax 'forward
		       viper-SEP-char-class
		       (or within-line "\n")
		       (if within-line (viper-line-pos 'end)))))

(defsubst viper-skip-all-separators-backward (&optional within-line)
  (if (eq viper-syntax-preference 'strict-vi)
      (if within-line
	  (skip-chars-backward viper-strict-SEP-chars-sans-newline)
	(skip-chars-backward viper-strict-SEP-chars))
    (viper-skip-syntax 'backward
		       viper-SEP-char-class
		       (or within-line "\n")
		       (if within-line (viper-line-pos 'start)))))
(defun viper-skip-nonseparators (direction)
  (viper-skip-syntax
   direction
   (concat "^" viper-SEP-char-class)
   nil
   (viper-line-pos (if (eq direction 'forward) 'end 'start))))


;; skip over non-word constituents and non-separators
(defun viper-skip-nonalphasep-forward ()
  (if (eq viper-syntax-preference 'strict-vi)
      (skip-chars-forward
       (concat "^" viper-strict-SEP-chars viper-strict-ALPHA-chars))
    (viper-skip-syntax
     'forward
     (concat "^" viper-ALPHA-char-class viper-SEP-char-class)
     ;; Emacs may consider some of these as words, but we don't want them
     viper-non-word-characters
     (viper-line-pos 'end))))

(defun viper-skip-nonalphasep-backward ()
  (if (eq viper-syntax-preference 'strict-vi)
      (skip-chars-backward
       (concat "^" viper-strict-SEP-chars viper-strict-ALPHA-chars))
    (viper-skip-syntax
     'backward
     (concat "^" viper-ALPHA-char-class viper-SEP-char-class)
     ;; Emacs may consider some of these as words, but we don't want them
     viper-non-word-characters
     (viper-line-pos 'start))))

;; Skip SYNTAX like skip-syntax-* and ADDL-CHARS like skip-chars-*
;; Return the number of chars traveled.
;; Both SYNTAX or ADDL-CHARS can be strings or lists of characters.
;; When SYNTAX is "w", then viper-non-word-characters are not considered to be
;; words, even if Emacs syntax table says they are.
(defun viper-skip-syntax (direction syntax addl-chars &optional limit)
  (let ((total 0)
	(local 1)
	(skip-chars-func
	 (if (eq direction 'forward)
	     'skip-chars-forward 'skip-chars-backward))
	(skip-syntax-func
	 (if (eq direction 'forward)
	     'viper-forward-char-carefully 'viper-backward-char-carefully))
	char-looked-at syntax-of-char-looked-at negated-syntax)
    (setq addl-chars
	  (cond ((listp addl-chars) (viper-charlist-to-string addl-chars))
		((stringp addl-chars) addl-chars)
		(t "")))
    (setq syntax
	  (cond ((listp syntax) syntax)
		((stringp syntax) (viper-string-to-list syntax))
		(t nil)))
    (if (memq ?^ syntax) (setq negated-syntax t))

    (while (and (not (= local 0))
		(cond ((eq direction 'forward)
		       (not (eobp)))
		      (t (not (bobp)))))
      (setq char-looked-at (viper-char-at-pos direction)
	    ;; if outside the range, set to nil
	    syntax-of-char-looked-at (if char-looked-at
					 (char-syntax char-looked-at)))
      (setq local
	    (+ (if (and
		    (cond ((and limit (eq direction 'forward))
			   (< (point) limit))
			  (limit ; backward & limit
			   (> (point) limit))
			  (t t)) ; no limit
		    ;; char under/before cursor has appropriate syntax
		    (if negated-syntax
			(not (memq syntax-of-char-looked-at syntax))
		      (memq syntax-of-char-looked-at syntax))
		    ;; if char-syntax class is "word", make sure it is not one
		    ;; of the excluded characters
		    (if (and (eq syntax-of-char-looked-at ?w)
			     (not negated-syntax))
			(not (viper-memq-char
			      char-looked-at viper-non-word-characters))
		      t))
		   (funcall skip-syntax-func 1)
		 0)
	       (funcall skip-chars-func addl-chars limit)))
      (setq total (+ total local)))
    total
    ))

;; tells when point is at the beginning of field
(defun viper-beginning-of-field ()
  (or (bobp)
      (not (eq (get-char-property (point) 'field)
	       (get-char-property (1- (point)) 'field)))))


;; this is copied from cl-extra.el
;; Return the subsequence of SEQ from START to END.
;; If END is omitted, it defaults to the length of the sequence.
;; If START or END is negative, it counts from the end.
(defun viper-subseq (seq start &optional end)
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))



;; Local Variables:
;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
;; End:

;;; viper-util.el ends here
