;;; kkc.el --- Kana Kanji converter    -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: i18n, mule, multilingual, Japanese

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

;; These routines provide a simple and easy-to-use converter from
;; Kana-string to Kana-Kanji-mixed-string.  This converter (here after
;; KKC) uses a SKK dictionary to get information how to convert
;; Kana-string.  Since KKC can't be fully automated, we need an
;; interaction with a user to decide the correct conversion.   For
;; that, we provide KKC major mode.

;;; Code:

(require 'ja-dic-utl)

(defvar kkc-input-method-title "漢"
  "String denoting KKC input method.
This string is shown at mode line when users are in KKC mode.")

(defvar kkc-init-file-name (convert-standard-filename "~/.kkcrc")
  "Name of a file which contains user's initial setup code for KKC.")

;; A flag to control a file specified by `kkc-init-file-name'.
;; The value nil means the file is not yet consulted.
;; The value t means the file has already been consulted but there's
;; no need of updating it yet.
;; Any other value means that we must update the file before exiting Emacs.
(defvar kkc-init-file-flag nil)

;; Cash data for `kkc-lookup-key'.  This may be initialized by loading
;; a file specified by `kkc-init-file-name'.  If any elements are
;; modified, the data is written out to the file when exiting Emacs.
(defvar kkc-lookup-cache nil)

;; Tag symbol of `kkc-lookup-cache'.
(defconst kkc-lookup-cache-tag 'kkc-lookup-cache-2)

(defun kkc-save-init-file ()
  "Save initial setup code for KKC to a file specified by `kkc-init-file-name'"
  (if (and kkc-init-file-flag
	   (not (eq kkc-init-file-flag t)))
      (let ((coding-system-for-write 'iso-2022-7bit)
	    (print-length nil))
	(write-region (format "(setq kkc-lookup-cache '%S)\n" kkc-lookup-cache)
		      nil
		      kkc-init-file-name))))

;; Sequence of characters to be used for indexes for shown list.  The
;; Nth character is for the Nth conversion in the list currently shown.
(defvar kkc-show-conversion-list-index-chars
  "1234567890")

(defun kkc-help ()
  "Show key bindings available while converting by KKC."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (substitute-command-keys "\\{kkc-keymap}"))))

(defvar kkc-keymap
  (let ((map (make-sparse-keymap))
	(len (length kkc-show-conversion-list-index-chars))
	(i 0))
    (while (< i len)
      (define-key map
	(char-to-string (aref kkc-show-conversion-list-index-chars i))
	'kkc-select-from-list)
      (setq i (1+ i)))
    (define-key map " " 'kkc-next)
    (define-key map "\r" 'kkc-terminate)
    (define-key map "\C-@" 'kkc-first-char-only)
    (define-key map "\C-n" 'kkc-next)
    (define-key map "\C-p" 'kkc-prev)
    (define-key map "\C-i" 'kkc-shorter)
    (define-key map "\C-o" 'kkc-longer)
    (define-key map "I" 'kkc-shorter-conversion)
    (define-key map "O" 'kkc-longer-phrase)
    (define-key map "\C-c" 'kkc-cancel)
    (define-key map "\C-?" 'kkc-cancel)
    (define-key map "\C-f" 'kkc-next-phrase)
    (define-key map "K" 'kkc-katakana)
    (define-key map "H" 'kkc-hiragana)
    (define-key map "l" 'kkc-show-conversion-list-or-next-group)
    (define-key map "L" 'kkc-show-conversion-list-or-prev-group)
    (define-key map [?\C- ] 'kkc-first-char-only)
    (define-key map [delete] 'kkc-cancel)
    (define-key map [return] 'kkc-terminate)
    (define-key map "\C-h" 'kkc-help)
    map)
  "Keymap for KKC (Kana Kanji Converter).")

;;; Internal variables used in KKC.

;; The current Kana string to be converted.
(defvar kkc-original-kana nil)

;; The current key sequence (vector of Kana characters) generated from
;; `kkc-original-kana'.
(defvar kkc-current-key nil)

;; List of the current conversions for `kkc-current-key'.
(defvar kkc-current-conversions nil)

;; Vector of the same length as `kkc-current-conversion'.  The first
;; element is a vector of:
;;	o index number of the first conversion shown previously,
;;	o index number of a conversion next of the last one shown previously,
;;	o the shown string itself.
;; The remaining elements are widths (including columns for index
;; numbers) of conversions stored in the same order as in
;; `kkc-current-conversion'.
(defvar kkc-current-conversions-width nil)

(defcustom kkc-show-conversion-list-count 4
  "Count of successive `kkc-next' or `kkc-prev' to show conversion list.
When you type SPC or C-p successively this count while using the input
method `japanese', the conversion candidates are shown in the echo
area while indicating the current selection by `<N>'."
  :group 'mule
  :type 'integer)

;; Count of successive invocations of `kkc-next'.
(defvar kkc-next-count nil)

;; Count of successive invocations of `kkc-prev'.
(defvar kkc-prev-count nil)

;; Provided that `kkc-current-key' is [A B C D E F G H I], the current
;; conversion target is [A B C D E F], and the sequence of which
;; conversion is found is [A B C D]:
;;
;;                                A B C D E F G H I
;; kkc-overlay-head (black):     |<--------->|
;; kkc-overlay-tail (underline):         |<------->|
;; kkc-length-head:              |<--------->|
;; kkc-length-converted:         |<----->|
;;
(defvar kkc-overlay-head nil)
(defvar kkc-overlay-tail nil)
(defvar kkc-length-head nil)
(defvar kkc-length-converted nil)

;; Cursor type (`box' or `bar') of the current frame.
(defvar kkc-cursor-type nil)

;; Lookup Japanese dictionary to set list of conversions in
;; kkc-current-conversions for key sequence kkc-current-key of length
;; LEN.  If no conversion is found in the dictionary, don't change
;; kkc-current-conversions and return nil.
;; Postfixes are handled only if POSTFIX is non-nil.
(defun kkc-lookup-key (len &optional postfix prefer-noun)
  ;; At first, prepare cache data if any.
  (unless kkc-init-file-flag
    (setq kkc-init-file-flag t
	  kkc-lookup-cache nil)
    (add-hook 'kill-emacs-hook 'kkc-save-init-file)
    (if (file-readable-p kkc-init-file-name)
	(condition-case nil
	    (load-file kkc-init-file-name)
	  (kkc-error "Invalid data in %s" kkc-init-file-name))))
  (or (and (nested-alist-p kkc-lookup-cache)
	   (eq (car kkc-lookup-cache) kkc-lookup-cache-tag))
      (setq kkc-lookup-cache (list kkc-lookup-cache-tag)
	    kkc-init-file-flag 'kkc-lookup-cache))
  (let ((entry (lookup-nested-alist kkc-current-key kkc-lookup-cache len 0 t)))
    (if (consp (car entry))
	(setq kkc-length-converted len
	      kkc-current-conversions-width nil
	      kkc-current-conversions (car entry))
      (setq entry (skkdic-lookup-key kkc-current-key len postfix prefer-noun))
      (if entry
	  (progn
	    (setq kkc-length-converted len
		  kkc-current-conversions-width nil
		  kkc-current-conversions (cons 1 entry))
	    (if postfix
		;; Store this conversions in the cache.
		(progn
		  (set-nested-alist kkc-current-key kkc-current-conversions
				    kkc-lookup-cache kkc-length-converted)
		  (setq kkc-init-file-flag 'kkc-lookup-cache)))
	    t)
	(if (= len 1)
	    (setq kkc-length-converted 1
		  kkc-current-conversions-width nil
		  kkc-current-conversions (cons 0 nil)))))))

(put 'kkc-error 'error-conditions '(kkc-error error))
(defun kkc-error (&rest args)
  (signal 'kkc-error (apply 'format args)))

(defvar kkc-converting nil)

;;;###autoload
(defvar kkc-after-update-conversion-functions nil
  "Functions to run after a conversion is selected in `japanese' input method.
With this input method, a user can select a proper conversion from
candidate list.  Each time he changes the selection, functions in this
list are called with two arguments; starting and ending buffer
positions that contains the current selection.")

;;;###autoload
(defun kkc-region (from to)
  "Convert Kana string in the current region to Kanji-Kana mixed string.
Users can select a desirable conversion interactively.
When called from a program, expects two arguments,
positions FROM and TO (integers or markers) specifying the target region.
When it returns, the point is at the tail of the selected conversion,
and the return value is the length of the conversion."
  (interactive "r")
  (setq kkc-original-kana (buffer-substring from to))
  (goto-char from)

  ;; Setup overlays.
  (if (overlayp kkc-overlay-head)
      (move-overlay kkc-overlay-head from to)
    (setq kkc-overlay-head (make-overlay from to nil nil t))
    (overlay-put kkc-overlay-head 'face 'highlight))
  (if (overlayp kkc-overlay-tail)
      (move-overlay kkc-overlay-tail to to)
    (setq kkc-overlay-tail (make-overlay to to nil nil t))
    (overlay-put kkc-overlay-tail 'face 'underline))

  (setq kkc-current-key (string-to-vector kkc-original-kana))
  (setq kkc-length-head (length kkc-current-key))
  (setq kkc-length-converted 0)

  (unwind-protect
      ;; At first convert the region to the first candidate.
      (let ((current-input-method-title kkc-input-method-title)
	    (input-method-function nil)
	    (modified-p (buffer-modified-p))
	    (first t))
	(while (not (kkc-lookup-key kkc-length-head nil first))
	  (setq kkc-length-head (1- kkc-length-head)
		first nil))
	(goto-char to)
	(kkc-update-conversion 'all)
	(setq kkc-next-count 1 kkc-prev-count 0)
	(if (and (>= kkc-next-count kkc-show-conversion-list-count)
		 (>= (length kkc-current-conversions) 3))
	    (kkc-show-conversion-list-or-next-group))

	;; Then, ask users to select a desirable conversion.
	(force-mode-line-update)
	(setq kkc-converting t)
	;; Hide "... loaded" message.
	(message nil)
	(while kkc-converting
	  (set-buffer-modified-p modified-p)
	  (let* ((overriding-terminal-local-map kkc-keymap)
		 (help-char nil)
		 (keyseq (read-key-sequence nil))
		 (cmd (lookup-key kkc-keymap keyseq)))
	    (if (commandp cmd)
		(condition-case err
		    (progn
		      (cond ((eq cmd 'kkc-next)
			     (setq kkc-next-count (1+ kkc-next-count)
				   kkc-prev-count 0))
			    ((eq cmd 'kkc-prev)
			     (setq kkc-prev-count (1+ kkc-prev-count)
				   kkc-next-count 0))
			    (t
			     (setq kkc-next-count 0 kkc-prev-count 0)))
		      (call-interactively cmd))
		  (kkc-error (message "%s" (cdr err)) (beep)))
	      ;; KEYSEQ is not defined in KKC keymap.
	      ;; Let's put the event back.
	      (setq unread-input-method-events
		    (append (string-to-list (this-single-command-raw-keys))
			    unread-input-method-events))
	      (kkc-terminate))))

	(force-mode-line-update)
	(goto-char (overlay-end kkc-overlay-tail))
	(- (overlay-start kkc-overlay-head) from))
    (delete-overlay kkc-overlay-head)
    (delete-overlay kkc-overlay-tail)))

(defun kkc-terminate ()
  "Exit from KKC mode by fixing the current conversion."
  (interactive)
  (goto-char (overlay-end kkc-overlay-tail))
  (move-overlay kkc-overlay-head (point) (point))
  (setq kkc-converting nil))

(defun kkc-cancel ()
  "Exit from KKC mode by canceling any conversions."
  (interactive)
  (goto-char (overlay-start kkc-overlay-head))
  (delete-region (overlay-start kkc-overlay-head)
		 (overlay-end kkc-overlay-tail))
  (insert kkc-original-kana)
  (setq kkc-converting nil))

(defun kkc-first-char-only ()
  "Select only the first character currently converted."
  (interactive)
  (goto-char (overlay-start kkc-overlay-head))
  (forward-char 1)
  (delete-region (point) (overlay-end kkc-overlay-tail))
  (kkc-terminate))

(defun kkc-next ()
  "Select the next candidate of conversion."
  (interactive)
  (let ((idx (1+ (car kkc-current-conversions))))
    (if (< idx 0)
	(setq idx 1))
    (if (>= idx (length kkc-current-conversions))
	(setq idx 0))
    (setcar kkc-current-conversions idx)
    (if (> idx 1)
	(progn
	  (set-nested-alist kkc-current-key kkc-current-conversions
			    kkc-lookup-cache kkc-length-converted)
	  (setq kkc-init-file-flag 'kkc-lookup-cache)))
    (if (or kkc-current-conversions-width
	    (>= kkc-next-count kkc-show-conversion-list-count))
	(kkc-show-conversion-list-update))
    (kkc-update-conversion)))

(defun kkc-prev ()
  "Select the previous candidate of conversion."
  (interactive)
  (let ((idx (1- (car kkc-current-conversions))))
    (if (< idx 0)
	(setq idx (1- (length kkc-current-conversions))))
    (setcar kkc-current-conversions idx)
    (if (> idx 1)
	(progn
	  (set-nested-alist kkc-current-key kkc-current-conversions
			    kkc-lookup-cache kkc-length-converted)
	  (setq kkc-init-file-flag 'kkc-lookup-cache)))
    (if (or kkc-current-conversions-width
	    (>= kkc-prev-count kkc-show-conversion-list-count))
	(kkc-show-conversion-list-update))
    (kkc-update-conversion)))

(defun kkc-select-from-list ()
  "Select one candidate from the list currently shown in echo area."
  (interactive)
  (let (idx)
    (if kkc-current-conversions-width
	(let ((len (length kkc-show-conversion-list-index-chars))
	      (maxlen (- (aref (aref kkc-current-conversions-width 0) 1)
			 (aref (aref kkc-current-conversions-width 0) 0)))
	      (i 0))
	  (if (> len maxlen)
	      (setq len maxlen))
	  (while (< i len)
	    (if (= (aref kkc-show-conversion-list-index-chars i)
		   last-input-event)
		(setq idx i i len)
	      (setq i (1+ i))))))
    (if idx
	(progn
	  (setcar kkc-current-conversions
		  (+ (aref (aref kkc-current-conversions-width 0) 0) idx))
	  (kkc-show-conversion-list-update)
	  (kkc-update-conversion))
      (setq unread-input-method-events
	    (cons last-input-event unread-input-method-events))
      (kkc-terminate))))

(defun kkc-katakana ()
  "Convert to Katakana."
  (interactive)
  (setcar kkc-current-conversions -1)
  (kkc-update-conversion 'all))

(defun kkc-hiragana ()
  "Convert to hiragana."
  (interactive)
  (setcar kkc-current-conversions 0)
  (kkc-update-conversion))

(defun kkc-shorter ()
  "Make the Kana string to be converted shorter."
  (interactive)
  (if (<= kkc-length-head 1)
      (kkc-error "Can't be shorter"))
  (setq kkc-length-head (1- kkc-length-head))
  (if (> kkc-length-converted kkc-length-head)
      (let ((len kkc-length-head))
	(setq kkc-length-converted 0)
	(while (not (kkc-lookup-key len))
	  (setq len (1- len)))))
  (kkc-update-conversion 'all))

(defun kkc-longer ()
  "Make the Kana string to be converted longer."
  (interactive)
  (if (>= kkc-length-head (length kkc-current-key))
      (kkc-error "Can't be longer"))
  (setq kkc-length-head (1+ kkc-length-head))
  ;; This time, try also entries with postfixes.
  (kkc-lookup-key kkc-length-head 'postfix)
  (kkc-update-conversion 'all))

(defun kkc-shorter-conversion ()
  "Make the Kana string to be converted shorter."
  (interactive)
  (if (<= kkc-length-converted 1)
      (kkc-error "Can't be shorter"))
  (let ((len (1- kkc-length-converted)))
    (setq kkc-length-converted 0)
    (while (not (kkc-lookup-key len))
      (setq len (1- len))))
  (kkc-update-conversion 'all))

(defun kkc-longer-phrase ()
  "Make the current phrase (BUNSETSU) longer without looking up dictionary."
  (interactive)
  (if (>= kkc-length-head (length kkc-current-key))
      (kkc-error "Can't be longer"))
  (setq kkc-length-head (1+ kkc-length-head))
  (kkc-update-conversion 'all))

(defun kkc-next-phrase ()
  "Fix the currently converted string and try to convert the remaining string."
  (interactive)
  (if (>= kkc-length-head (length kkc-current-key))
      (kkc-terminate)
    (setq kkc-length-head (- (length kkc-current-key) kkc-length-head))
    (goto-char (overlay-end kkc-overlay-head))
    (while (and (< (point) (overlay-end kkc-overlay-tail))
		(looking-at "\\CH"))
      (goto-char (match-end 0))
      (setq kkc-length-head (1- kkc-length-head)))
    (if (= kkc-length-head 0)
	(kkc-terminate)
      (let ((newkey (make-vector kkc-length-head 0))
	    (idx (- (length kkc-current-key) kkc-length-head))
	    (len kkc-length-head)
	    (i 0))
	;; For the moment, (setq kkc-original-kana (concat newkey))
	;; doesn't work.
	(setq kkc-original-kana "")
	(while (< i kkc-length-head)
	  (aset newkey i (aref kkc-current-key (+ idx i)))
	  (setq kkc-original-kana
		(concat kkc-original-kana (char-to-string (aref newkey i))))
	  (setq i (1+ i)))
	(setq kkc-current-key newkey)
	(setq kkc-length-converted 0)
	(while (and (not (kkc-lookup-key kkc-length-head nil
					 (< kkc-length-head len)))
		    (> kkc-length-head 1))
	  (setq kkc-length-head (1- kkc-length-head)))
	(let ((pos (point))
	      (tail (overlay-end kkc-overlay-tail)))
	  (move-overlay kkc-overlay-head pos tail)
	  (move-overlay kkc-overlay-tail tail tail))
	(kkc-update-conversion 'all)))))

;; We'll show users a list of available conversions in echo area with
;; index numbers so that users can select one conversion with the
;; number.

;; Set `kkc-current-conversions-width'.
(defun kkc-setup-current-conversions-width ()
  (let ((convs (cdr kkc-current-conversions))
	(len (length kkc-current-conversions))
	(idx 1))
    (setq kkc-current-conversions-width (make-vector len nil))
    ;; To tell `kkc-show-conversion-list-update' to generate
    ;; message from scratch.
    (aset kkc-current-conversions-width 0 (vector len -2 nil))
    ;; Fill the remaining slots.
    (while convs
      (aset kkc-current-conversions-width idx
	    (+ (string-width (car convs)) 4))
      (setq convs (cdr convs)
	    idx (1+ idx)))))

(defun kkc-show-conversion-list-or-next-group ()
  "Show list of available conversions in echo area with index numbers.
If the list is already shown, show the next group of conversions,
and change the current conversion to the first one in the group."
  (interactive)
  (if (< (length kkc-current-conversions) 3)
      (kkc-error "No alternative"))
  (if kkc-current-conversions-width
      (let ((next-idx (aref (aref kkc-current-conversions-width 0) 1)))
	(if (< next-idx (length kkc-current-conversions-width))
	    (setcar kkc-current-conversions next-idx)
	  (setcar kkc-current-conversions 1))
	(kkc-show-conversion-list-update)
	(kkc-update-conversion))
    (kkc-setup-current-conversions-width)
    (kkc-show-conversion-list-update)))

(defun kkc-show-conversion-list-or-prev-group ()
  "Show list of available conversions in echo area with index numbers.
If the list is already shown, show the previous group of conversions,
and change the current conversion to the last one in the group."
  (interactive)
  (if (< (length kkc-current-conversions) 3)
      (kkc-error "No alternative"))
  (if kkc-current-conversions-width
      (let ((this-idx (aref (aref kkc-current-conversions-width 0) 0)))
	(if (> this-idx 1)
	    (setcar kkc-current-conversions (1- this-idx))
	  (setcar kkc-current-conversions
		  (1- (length kkc-current-conversions-width))))
	(kkc-show-conversion-list-update)
	(kkc-update-conversion))
    (kkc-setup-current-conversions-width)
    (kkc-show-conversion-list-update)))

;; Update the conversion list shown in echo area.
(defun kkc-show-conversion-list-update ()
  (or kkc-current-conversions-width
      (kkc-setup-current-conversions-width))
  (let* ((current-idx (car kkc-current-conversions))
	 (first-slot (aref kkc-current-conversions-width 0))
	 (this-idx (aref first-slot 0))
	 (next-idx (aref first-slot 1))
	 (msg (aref first-slot 2)))
    (if (< current-idx this-idx)
	;; The currently selected conversion is before the list shown
	;; previously.  We must start calculation of message width
	;; from the start again.
	(setq this-idx 1 msg nil)
      (if (>= current-idx next-idx)
	  ;; The currently selected conversion is after the list shown
	  ;; previously.  We start calculation of message width from
	  ;; the conversion next of TO.
	  (setq this-idx next-idx msg nil)))
    (if (not msg)
	(let ((len (length kkc-current-conversions))
	      (max-width (window-width (minibuffer-window)))
	      (width-table kkc-current-conversions-width)
	      (width 0)
	      (idx this-idx)
	      (max-items (length kkc-show-conversion-list-index-chars))
	      l)
	  ;; Set THIS-IDX to the first index of conversion to be shown
	  ;; in MSG, and reflect it in kkc-current-conversions-width.
	  (while (<= idx current-idx)
	    (if (and (<= (+ width (aref width-table idx)) max-width)
		     (< (- idx this-idx) max-items))
		(setq width (+ width (aref width-table idx)))
	      (setq this-idx idx width (aref width-table idx)))
	    (setq idx (1+ idx)
		  l (cdr l)))
	  (aset first-slot 0 this-idx)
	  ;; Set NEXT-IDX to the next index of the last conversion
	  ;; shown in MSG, and reflect it in
	  ;; kkc-current-conversions-width.
	  (while (and (< idx len)
		      (<= (+ width (aref width-table idx)) max-width)
		      (< (- idx this-idx) max-items))
	    (setq width (+ width (aref width-table idx))
		  idx (1+ idx)
		  l (cdr l)))
	  (aset first-slot 1 (setq next-idx idx))
	  (setq l (nthcdr this-idx kkc-current-conversions))
	  (setq msg (format " %c %s"
			    (aref kkc-show-conversion-list-index-chars 0)
			    (propertize (car l)
					'kkc-conversion-index this-idx))
		idx (1+ this-idx)
		l (cdr l))
	  (while (< idx next-idx)
	    (setq msg (format "%s  %c %s"
			      msg
			      (aref kkc-show-conversion-list-index-chars
				    (- idx this-idx))
			      (propertize (car l)
					  'kkc-conversion-index idx))
		  idx (1+ idx)
		  l (cdr l)))
	  (aset first-slot 2 msg)))

    ;; Highlight the current conversion.
    (if (> current-idx 0)
	(let ((pos 3)
	      (limit (length msg)))
	  (remove-text-properties 0 (length msg) '(face nil) msg)
	  (while (not (eq (get-text-property pos 'kkc-conversion-index msg)
			  current-idx))
	    (setq pos (next-single-property-change pos 'kkc-conversion-index
						   msg limit)))
	  (put-text-property pos (next-single-property-change
				  pos 'kkc-conversion-index msg limit)
			     'face 'highlight msg)))
    (let ((message-log-max nil))
      (message "%s" msg))))

;; Update the conversion area with the latest conversion selected.
;; ALL if non nil means to update the whole area, else update only
;; inside quail-overlay-head.

(defun kkc-update-conversion (&optional all)
  (goto-char (overlay-start kkc-overlay-head))
  (cond ((= (car kkc-current-conversions) 0) ; Hiragana
	 (let ((i 0))
	   (while (< i kkc-length-converted)
	     (insert (aref kkc-current-key i))
	     (setq i (1+ i)))))
	((= (car kkc-current-conversions) -1) ; Katakana
	 (let ((i 0))
	   (while (< i kkc-length-converted)
	     (insert (japanese-katakana (aref kkc-current-key i)))
	     (setq i (1+ i)))))
	(t
	 (insert (nth (car kkc-current-conversions) kkc-current-conversions))))
  (delete-region (point) (overlay-start kkc-overlay-tail))
  (if all
      (let ((len (length kkc-current-key))
	    (i kkc-length-converted))
	(delete-region (overlay-start kkc-overlay-tail)
		       (overlay-end kkc-overlay-head))
	(while (< i kkc-length-head)
	  (if (= (car kkc-current-conversions) -1)
	      (insert (japanese-katakana (aref kkc-current-key i)))
	    (insert (aref kkc-current-key i)))
	  (setq i (1+ i)))
	(let ((pos (point)))
	  (while (< i len)
	    (insert (aref kkc-current-key i))
	    (setq i (1+ i)))
	  (move-overlay kkc-overlay-head
			(overlay-start kkc-overlay-head) pos)
	  (delete-region (point) (overlay-end kkc-overlay-tail)))))
  (unwind-protect
      (run-hook-with-args 'kkc-after-update-conversion-functions
			  (overlay-start kkc-overlay-head)
			  (overlay-end kkc-overlay-head))
    (goto-char (overlay-end kkc-overlay-tail))))

;;
(provide 'kkc)

;;; kkc.el ends here
