;;; re-builder.el --- building Regexps with visual feedback -*- lexical-binding: t -*-

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Detlev Zundel <dzu@gnu.org>
;; Keywords: matching, lisp, tools

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

;; When I have to come up with regular expressions that are more
;; complex than simple string matchers, especially if they contain sub
;; expressions, I find myself spending quite some time in the
;; `development cycle'.  `re-builder' aims to shorten this time span
;; so I can get on with the more interesting bits.

;; With it you can have immediate visual feedback about how well the
;; regexp behaves to your expectations on the intended data.

;; When called up `re-builder' attaches itself to the current buffer
;; which becomes its target buffer, where all the matching is done.
;; The active window is split so you have a view on the data while
;; authoring the RE.  If the edited expression is valid the matches in
;; the target buffer are marked automatically with colored overlays
;; (for non-color displays see below) giving you feedback over the
;; extents of the matched (sub) expressions.  The (non-)validity is
;; shown only in the modeline without throwing the errors at you.  If
;; you want to know the reason why RE Builder considers it as invalid
;; call `reb-force-update' ("\C-c\C-u") which should reveal the error.

;; The target buffer can be changed with `reb-change-target-buffer'
;; ("\C-c\C-b").  Changing the target buffer automatically removes
;; the overlays from the old buffer and displays the new one in the
;; target window.

;; The `re-builder' keeps the focus while updating the matches in the
;; target buffer so corrections are easy to incorporate.  If you are
;; satisfied with the result you can paste the RE to the kill-ring
;; with `reb-copy' ("\C-c\C-w"), quit the `re-builder' ("\C-c\C-q")
;; and use it wherever you need it.

;; As the automatic updates can take some time on large buffers, they
;; can be limited by `reb-auto-match-limit' so that they should not
;; have a negative impact on the editing.  Setting it to nil makes
;; even the auto updates go all the way.  Forcing an update overrides
;; this limit allowing an easy way to see all matches.

;; Currently `re-builder' understands three different forms of input,
;; namely `read', `string', and `rx' syntax.  Read
;; syntax and string syntax are both delimited by `"'s and behave
;; according to their name.  With the `string' syntax there's no need
;; to escape the backslashes and double quotes simplifying the editing
;; somewhat.  The other three allow editing of symbolic regular
;; expressions supported by the packages of the same name.

;; Editing symbolic expressions is done through a major mode derived
;; from `emacs-lisp-mode' so you'll get all the good stuff like
;; automatic indentation and font-locking etc.

;; When editing a symbolic regular expression, only the first
;; expression in the RE Builder buffer is considered, which helps
;; limiting the extent of the expression like the `"'s do for the text
;; modes.  For the `rx' syntax the function `rx-to-string' is applied to
;; the evaluated expression read.  So you can use quoted arguments
;; with something like '("findme") or you can construct arguments to
;; your hearts delight with a valid ELisp expression.  (The compiled
;; string form will be copied by `reb-copy')  If you want to take
;; a glance at the corresponding string you can temporarily change the
;; input syntax.

;; Changing the input syntax is transparent (for the obvious exception
;; non-symbolic -> symbolic) so you can change your mind as often as
;; you like.

;; There is also a shortcut function for toggling the
;; `case-fold-search' variable in the target buffer with an immediate
;; update.


;; Q: But what if my display cannot show colored overlays?
;; A: Then the cursor will flash around the matched text making it stand
;;    out.

;; Q: But how can I then make out the sub-expressions?
;; A: Thats where the `sub-expression mode' comes in.  In it only the
;;    digit keys are assigned to perform an update that will flash the
;;    corresponding subexp only.


;;; Code:

;; On XEmacs, load the overlay compatibility library
(unless (fboundp 'make-overlay)
  (require 'overlay))

;; User customizable variables
(defgroup re-builder nil
  "Options for the RE Builder."
  :group 'lisp
  :prefix "reb-")

(defcustom reb-blink-delay 0.5
  "Seconds to blink cursor for next/previous match in RE Builder."
  :group 're-builder
  :type 'number)

(defcustom reb-mode-hook nil
  "Hooks to run on entering RE Builder mode."
  :group 're-builder
  :type 'hook)

(defcustom reb-re-syntax 'read
  "Syntax for the REs in the RE Builder.
Can either be `read', `string', or `rx'."
  :group 're-builder
  :type '(choice (const :tag "Read syntax" read)
		 (const :tag "String syntax" string)
		 (const :tag "`rx' syntax" rx)))

(defcustom reb-auto-match-limit 200
  "Positive integer limiting the matches for RE Builder auto updates.
Set it to nil if you don't want limits here."
  :group 're-builder
  :type '(restricted-sexp :match-alternatives
			  (integerp 'nil)))


(defface reb-match-0
  '((((class color) (background light))
     :background "lightblue")
    (((class color) (background dark))
     :background "steelblue4")
    (t
     :inverse-video t))
  "Used for displaying the whole match."
  :group 're-builder)

(defface reb-match-1
  '((((class color) (background light))
     :background "aquamarine")
    (((class color) (background dark))
     :background "blue3")
    (t
     :inverse-video t))
  "Used for displaying the first matching subexpression."
  :group 're-builder)

(defface reb-match-2
  '((((class color) (background light))
     :background "springgreen")
    (((class color) (background dark))
     :background "chartreuse4")
    (t
     :inverse-video t))
  "Used for displaying the second matching subexpression."
  :group 're-builder)

(defface reb-match-3
  '((((min-colors 88) (class color) (background light))
     :background "yellow1")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "sienna4")
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

;; Internal variables below
(defvar reb-mode nil
  "Enables the RE Builder minor mode.")

(defvar reb-target-buffer nil
  "Buffer to which the RE is applied to.")

(defvar reb-target-window nil
  "Window to which the RE is applied to.")

(defvar reb-regexp nil
  "Last regexp used by RE Builder.")

(defvar reb-regexp-src nil
  "Last regexp used by RE Builder before processing it.
Except for Lisp syntax this is the same as `reb-regexp'.")

(defvar reb-overlays nil
  "List of overlays of the RE Builder.")

(defvar reb-window-config nil
  "Old window configuration.")

(defvar reb-subexp-mode nil
  "Indicates whether sub-exp mode is active.")

(defvar reb-subexp-displayed nil
  "Indicates which sub-exp is active.")

(defvar reb-mode-string ""
  "String in mode line for additional info.")

(defvar reb-valid-string ""
  "String in mode line showing validity of RE.")

(make-variable-buffer-local 'reb-overlays)
(make-variable-buffer-local 'reb-regexp)
(make-variable-buffer-local 'reb-regexp-src)

(defconst reb-buffer "*RE-Builder*"
  "Buffer to use for the RE Builder.")

;; Define the local "\C-c" keymap
(defvar reb-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'reb-toggle-case)
    (define-key map "\C-c\C-q" 'reb-quit)
    (define-key map "\C-c\C-w" 'reb-copy)
    (define-key map "\C-c\C-s" 'reb-next-match)
    (define-key map "\C-c\C-r" 'reb-prev-match)
    (define-key map "\C-c\C-i" 'reb-change-syntax)
    (define-key map "\C-c\C-e" 'reb-enter-subexp-mode)
    (define-key map "\C-c\C-b" 'reb-change-target-buffer)
    (define-key map "\C-c\C-u" 'reb-force-update)
    (define-key map [menu-bar reb-mode] (cons "Re-Builder" menu-map))
    (define-key menu-map [rq]
      '(menu-item "Quit" reb-quit
		  :help "Quit the RE Builder mode"))
    (define-key menu-map [rt]
      '(menu-item "Case sensitive" reb-toggle-case
		  :button (:toggle . (with-current-buffer
					 reb-target-buffer
				       (null case-fold-search)))
		  :help "Toggle case sensitivity of searches for RE Builder target buffer"))
    (define-key menu-map [rb]
      '(menu-item "Change target buffer..." reb-change-target-buffer
		  :help "Change the target buffer and display it in the target window"))
    (define-key menu-map [rs]
      '(menu-item "Change syntax..." reb-change-syntax
		  :help "Change the syntax used by the RE Builder"))
    (define-key menu-map [re]
      '(menu-item "Enter subexpression mode" reb-enter-subexp-mode
		  :help "Enter the subexpression mode in the RE Builder"))
    (define-key menu-map [ru]
      '(menu-item "Force update" reb-force-update
		  :help "Force an update in the RE Builder target window without a match limit"))
    (define-key menu-map [rn]
      '(menu-item "Go to next match" reb-next-match
		  :help "Go to next match in the RE Builder target window"))
    (define-key menu-map [rp]
      '(menu-item "Go to previous match" reb-prev-match
		  :help "Go to previous match in the RE Builder target window"))
    (define-key menu-map [rc]
      '(menu-item "Copy current RE" reb-copy
		  :help "Copy current RE into the kill ring for later insertion"))
    map)
  "Keymap used by the RE Builder.")

(define-derived-mode reb-mode nil "RE Builder"
  "Major mode for interactively building Regular Expressions."
  (set (make-local-variable 'blink-matching-paren) nil)
  (reb-mode-common))

(defvar reb-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use the same "\C-c" keymap as `reb-mode' and use font-locking from
    ;; `emacs-lisp-mode'
    (define-key map "\C-c" (lookup-key reb-mode-map "\C-c"))
    map))

(define-derived-mode reb-lisp-mode
  emacs-lisp-mode "RE Builder Lisp"
  "Major mode for interactively building symbolic Regular Expressions."
  ;; Pull in packages as needed
  (cond	((memq reb-re-syntax '(sregex rx)) ; rx-to-string is autoloaded
	 (require 'rx)))                   ; require rx anyway
  (reb-mode-common))

(defvar reb-subexp-mode-map
  (let ((m (make-keymap)))
    (suppress-keymap m)
    ;; Again share the "\C-c" keymap for the commands
    (define-key m "\C-c" (lookup-key reb-mode-map "\C-c"))
    (define-key m "q" 'reb-quit-subexp-mode)
    (dotimes (digit 10)
      (define-key m (int-to-string digit) 'reb-display-subexp))
    m)
  "Keymap used by the RE Builder for the subexpression mode.")

(defun reb-mode-common ()
  "Setup functions common to functions `reb-mode' and `reb-mode-lisp'."

  (setq	reb-mode-string  ""
	reb-valid-string ""
	mode-line-buffer-identification
	                 '(25 . ("%b" reb-mode-string reb-valid-string)))
  (reb-update-modestring)
  (add-hook 'after-change-functions 'reb-auto-update nil t)
  ;; At least make the overlays go away if the buffer is killed
  (add-hook 'kill-buffer-hook 'reb-kill-buffer nil t)
  (reb-auto-update nil nil nil))

(defun reb-color-display-p ()
  "Return t if display is capable of displaying colors."
  (eq 'color
      ;; emacs/xemacs compatibility
      (if (fboundp 'frame-parameter)
	  (frame-parameter (selected-frame) 'display-type)
	(if (fboundp 'frame-property)
	    (frame-property (selected-frame) 'display-type)))))

(defsubst reb-lisp-syntax-p ()
  "Return non-nil if RE Builder uses a Lisp syntax."
  (memq reb-re-syntax '(sregex rx)))

(defmacro reb-target-binding (symbol)
  "Return binding for SYMBOL in the RE Builder target buffer."
  `(with-current-buffer reb-target-buffer ,symbol))

(defun reb-initialize-buffer ()
  "Initialize the current buffer as a RE Builder buffer."
  (erase-buffer)
  (reb-insert-regexp)
  (goto-char (+ 2 (point-min)))
  (cond ((reb-lisp-syntax-p)
         (reb-lisp-mode))
        (t (reb-mode)))
  (reb-do-update))

(defun reb-mode-buffer-p ()
  "Return non-nil if the current buffer is a RE Builder buffer."
  (memq major-mode '(reb-mode reb-lisp-mode)))

;;; This is to help people find this in Apropos.
;;;###autoload
(defalias 'regexp-builder 're-builder)

;;;###autoload
(defun re-builder ()
  "Construct a regexp interactively.
This command makes the current buffer the \"target\" buffer of
the regexp builder.  It displays a buffer named \"*RE-Builder*\"
in another window, initially containing an empty regexp.

As you edit the regexp in the \"*RE-Builder*\" buffer, the
matching parts of the target buffer will be highlighted."
  (interactive)
  (if (and (string= (buffer-name) reb-buffer)
	   (reb-mode-buffer-p))
      (message "Already in the RE Builder")
    (when reb-target-buffer
      (reb-delete-overlays))
    (setq reb-target-buffer (current-buffer)
          reb-target-window (selected-window))
    (select-window (or (get-buffer-window reb-buffer)
		       (progn
			 (setq reb-window-config (current-window-configuration))
			 (split-window (selected-window) (- (window-height) 4)))))
    (switch-to-buffer (get-buffer-create reb-buffer))
    (reb-initialize-buffer)))

(defun reb-change-target-buffer (buf)
  "Change the target buffer and display it in the target window."
  (interactive "bSet target buffer to: ")

  (let ((buffer (get-buffer buf)))
    (if (not buffer)
        (error "No such buffer")
      (reb-delete-overlays)
      (setq reb-target-buffer buffer)
      (reb-do-update
       (if reb-subexp-mode reb-subexp-displayed nil))
      (reb-update-modestring))))

(defun reb-force-update ()
  "Force an update in the RE Builder target window without a match limit."
  (interactive)

  (let ((reb-auto-match-limit nil))
    (reb-update-overlays
     (if reb-subexp-mode reb-subexp-displayed nil))))

(defun reb-quit ()
  "Quit the RE Builder mode."
  (interactive)

  (setq reb-subexp-mode nil
	reb-subexp-displayed nil)
  (reb-delete-overlays)
  (bury-buffer)
  (set-window-configuration reb-window-config))

(defun reb-next-match ()
  "Go to next match in the RE Builder target window."
  (interactive)

  (reb-assert-buffer-in-window)
  (with-selected-window reb-target-window
    (if (not (re-search-forward reb-regexp (point-max) t))
	(message "No more matches")
      (reb-show-subexp
       (or (and reb-subexp-mode reb-subexp-displayed) 0)
       t))))

(defun reb-prev-match ()
  "Go to previous match in the RE Builder target window."
  (interactive)

  (reb-assert-buffer-in-window)
  (with-selected-window reb-target-window
    (let ((p (point)))
      (goto-char (1- p))
      (if (re-search-backward reb-regexp (point-min) t)
          (reb-show-subexp
           (or (and reb-subexp-mode reb-subexp-displayed) 0)
           t)
        (goto-char p)
        (message "No more matches")))))

(defun reb-toggle-case ()
  "Toggle case sensitivity of searches for RE Builder target buffer."
  (interactive)

  (with-current-buffer reb-target-buffer
    (setq case-fold-search (not case-fold-search)))
  (reb-update-modestring)
  (reb-auto-update nil nil nil t))

(defun reb-copy ()
  "Copy current RE into the kill ring for later insertion."
  (interactive)

  (reb-update-regexp)
  (let ((re (with-output-to-string
	      (print (reb-target-binding reb-regexp)))))
    (kill-new (substring re 1 (1- (length re))))
    (message "Regexp copied to kill-ring")))

;; The subexpression mode is not electric because the number of
;; matches should be seen rather than a prompt.
(defun reb-enter-subexp-mode ()
  "Enter the subexpression mode in the RE Builder."
  (interactive)
  (setq reb-subexp-mode t)
  (reb-update-modestring)
  (use-local-map reb-subexp-mode-map)
  (message "`0'-`9' to display subexpressions  `q' to quit subexp mode"))

(defun reb-show-subexp (subexp &optional pause)
  "Visually show limit of subexpression SUBEXP of recent search.
On color displays this just puts point to the end of the expression as
the match should already be marked by an overlay.
On other displays jump to the beginning and the end of it.
If the optional PAUSE is non-nil then pause at the end in any case."
  (with-selected-window reb-target-window
    (unless (reb-color-display-p)
      (goto-char (match-beginning subexp))
      (sit-for reb-blink-delay))
    (goto-char (match-end subexp))
    (when (or (not (reb-color-display-p)) pause)
      (sit-for reb-blink-delay))))

(defun reb-quit-subexp-mode ()
  "Quit the subexpression mode in the RE Builder."
  (interactive)
  (setq reb-subexp-mode nil
	reb-subexp-displayed nil)
  (reb-update-modestring)
  (use-local-map reb-mode-map)
  (reb-do-update))

(defun reb-change-syntax (&optional syntax)
  "Change the syntax used by the RE Builder.
Optional argument SYNTAX must be specified if called non-interactively."
  (interactive
   (list (intern
	  (completing-read "Select syntax: "
			   (mapcar (lambda (el) (cons (symbol-name el) 1))
				   '(read string sregex rx))
			   nil t (symbol-name reb-re-syntax)))))

  (if (memq syntax '(read string sregex rx))
      (let ((buffer (get-buffer reb-buffer)))
	(setq reb-re-syntax syntax)
	(when buffer
          (with-current-buffer buffer
            (reb-initialize-buffer))))
    (error "Invalid syntax: %s" syntax)))


;; Non-interactive functions below
(defun reb-do-update (&optional subexp)
  "Update matches in the RE Builder target window.
If SUBEXP is non-nil mark only the corresponding sub-expressions."

  (reb-assert-buffer-in-window)
  (reb-update-regexp)
  (reb-update-overlays subexp))

(defun reb-auto-update (_beg _end _lenold &optional force)
  "Called from `after-update-functions' to update the display.
BEG, END and LENOLD are passed in from the hook.
An actual update is only done if the regexp has changed or if the
optional fourth argument FORCE is non-nil."
  (let ((prev-valid reb-valid-string)
	(new-valid
	 (condition-case nil
	     (progn
	       (when (or (reb-update-regexp) force)
		 (reb-do-update))
	       "")
	   (error " *invalid*"))))
    (setq reb-valid-string new-valid)
    (force-mode-line-update)

    ;; Through the caching of the re a change invalidating the syntax
    ;; for symbolic expressions will not delete the overlays so we
    ;; catch it here
    (when (and (reb-lisp-syntax-p)
	       (not (string= prev-valid new-valid))
	       (string= prev-valid ""))
      (reb-delete-overlays))))

(defun reb-delete-overlays ()
  "Delete all RE Builder overlays in the `reb-target-buffer' buffer."
  (when (buffer-live-p reb-target-buffer)
    (with-current-buffer reb-target-buffer
      (mapc 'delete-overlay reb-overlays)
      (setq reb-overlays nil))))

(defun reb-assert-buffer-in-window ()
  "Assert that `reb-target-buffer' is displayed in `reb-target-window'."

  (if (not (eq reb-target-buffer (window-buffer reb-target-window)))
      (set-window-buffer reb-target-window reb-target-buffer)))

(defun reb-update-modestring ()
  "Update the variable `reb-mode-string' displayed in the mode line."
  (setq reb-mode-string
	(concat
	 (if reb-subexp-mode
             (format " (subexp %s)" (or reb-subexp-displayed "-"))
	   "")
	 (if (not (reb-target-binding case-fold-search))
	     " Case"
	   "")))
  (force-mode-line-update))

(defun reb-display-subexp (&optional subexp)
  "Highlight only subexpression SUBEXP in the RE Builder."
  (interactive)

  (setq reb-subexp-displayed
	(or subexp (string-to-number (format "%c" last-command-event))))
  (reb-update-modestring)
  (reb-do-update reb-subexp-displayed))

(defun reb-kill-buffer ()
  "When the RE Builder buffer is killed make sure no overlays stay around."

  (when (reb-mode-buffer-p)
    (reb-delete-overlays)))


;; The next functions are the interface between the regexp and
;; its textual representation in the RE Builder buffer.
;; They are the only functions concerned with the actual syntax
;; being used.
(defun reb-read-regexp ()
  "Read current RE."
  (save-excursion
    (cond ((eq reb-re-syntax 'read)
	   (goto-char (point-min))
	   (read (current-buffer)))
	  ((eq reb-re-syntax 'string)
	   (goto-char (point-min))
	   (re-search-forward "\"")
	   (let ((beg (point)))
	     (goto-char (point-max))
	     (re-search-backward "\"")
	     (buffer-substring-no-properties beg (point))))
	  ((reb-lisp-syntax-p)
	   (buffer-string)))))

(defun reb-empty-regexp ()
  "Return empty RE for current syntax."
  (cond ((reb-lisp-syntax-p) "'()")
	(t "")))

(defun reb-insert-regexp ()
  "Insert current RE."

  (let ((re (or (reb-target-binding reb-regexp)
		(reb-empty-regexp))))
  (cond ((eq reb-re-syntax 'read)
	 (print re (current-buffer)))
	((eq reb-re-syntax 'string)
	 (insert "\n\"" re "\""))
	;; For the Lisp syntax we need the "source" of the regexp
	((reb-lisp-syntax-p)
	 (insert (or (reb-target-binding reb-regexp-src)
		     (reb-empty-regexp)))))))

(defun reb-cook-regexp (re)
  "Return RE after processing it according to `reb-re-syntax'."
  (cond ((memq reb-re-syntax '(sregex rx))
	 (rx-to-string (eval (car (read-from-string re)))))
	(t re)))

(defun reb-update-regexp ()
  "Update the regexp for the target buffer.
Return t if the (cooked) expression changed."
  (let* ((re-src (reb-read-regexp))
	 (re (reb-cook-regexp re-src)))
    (with-current-buffer reb-target-buffer
      (let ((oldre reb-regexp))
	(prog1
	    (not (string= oldre re))
	  (setq reb-regexp re)
	  ;; Only update the source re for the lisp formats
	  (when (reb-lisp-syntax-p)
	    (setq reb-regexp-src re-src)))))))


;; And now the real core of the whole thing
(defun reb-count-subexps (re)
  "Return number of sub-expressions in the regexp RE."

  (let ((i 0) (beg 0))
    (while (string-match "\\\\(" re beg)
      (setq i (1+ i)
	    beg (match-end 0)))
    i))

(defun reb-update-overlays (&optional subexp)
  "Switch to `reb-target-buffer' and mark all matches of `reb-regexp'.
If SUBEXP is non-nil mark only the corresponding sub-expressions."
  (let* ((re (reb-target-binding reb-regexp))
	 (subexps (reb-count-subexps re))
	 (matches 0)
	 (submatches 0)
	 firstmatch)
    (with-current-buffer reb-target-buffer
      (reb-delete-overlays)
      (goto-char (point-min))
      (while (and (not (eobp))
		  (re-search-forward re (point-max) t)
		  (or (not reb-auto-match-limit)
		      (< matches reb-auto-match-limit)))
	(when (and (= 0 (length (match-string 0)))
		   (not (eobp)))
	  (forward-char 1))
	(let ((i 0)
	      suffix max-suffix)
	  (setq matches (1+ matches))
	  (while (<= i subexps)
	    (when (and (or (not subexp) (= subexp i))
		       (match-beginning i))
	      (let ((overlay (make-overlay (match-beginning i)
					   (match-end i)))
		    ;; When we have exceeded the number of provided faces,
		    ;; cycle thru them where `max-suffix' denotes the maximum
		    ;; suffix for `reb-match-*' that has been defined and
		    ;; `suffix' the suffix calculated for the current match.
		    (face
		     (cond
		       (max-suffix
			(if (= suffix max-suffix)
			    (setq suffix 1)
			  (setq suffix (1+ suffix)))
			(intern-soft (format "reb-match-%d" suffix)))
		       ((intern-soft (format "reb-match-%d" i)))
		       ((setq max-suffix (1- i))
			(setq suffix 1)
			;; `reb-match-1' must exist.
			'reb-match-1))))
		(unless firstmatch (setq firstmatch (match-data)))
		(setq reb-overlays (cons overlay reb-overlays)
		      submatches (1+ submatches))
		(overlay-put overlay 'face face)
		(overlay-put overlay 'priority i)))
	    (setq i (1+ i))))))
    (let ((count (if subexp submatches matches)))
      (message "%s %smatch%s%s"
	       (if (= 0 count) "No" (int-to-string count))
	       (if subexp "subexpression " "")
	       (if (= 1 count) "" "es")
	       (if (and reb-auto-match-limit
			(= reb-auto-match-limit count))
		   " (limit reached)" "")))
    (when firstmatch
      (store-match-data firstmatch)
      (reb-show-subexp (or subexp 0)))))

;; The End
(defun re-builder-unload-function ()
  "Unload the RE Builder library."
  (when (buffer-live-p (get-buffer reb-buffer))
    (with-current-buffer reb-buffer
      (remove-hook 'after-change-functions 'reb-auto-update t)
      (remove-hook 'kill-buffer-hook 'reb-kill-buffer t)
      (when (reb-mode-buffer-p)
	(reb-delete-overlays))))
  ;; continue standard unloading
  nil)

(provide 're-builder)

;;; re-builder.el ends here
