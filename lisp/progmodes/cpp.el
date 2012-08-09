;;; cpp.el --- highlight or hide text according to cpp conditionals

;; Copyright (C) 1994-1995, 2001-2012  Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: c, faces, tools

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

;; Parse a text for C preprocessor conditionals, and highlight or hide
;; the text inside the conditionals as you wish.

;; This package is inspired by Jim Coplien's delta editor for SCCS.

;;; Todo:

;; Should parse "#if" and "#elif" expressions and merge the faces
;; somehow.

;; Somehow it is sometimes possible to make changes near a read only
;; area which you can't undo.  Their are other strange effects in that
;; area.

;; The Edit buffer should -- optionally -- appear in its own frame.

;; Conditionals seem to be rear-sticky.  They shouldn't be.

;; Restore window configurations when exiting CPP Edit buffer.

;;; Code:

;;; Customization:
(defgroup cpp nil
  "Highlight or hide text according to cpp conditionals."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'c
  :prefix "cpp-")

(defcustom cpp-config-file (convert-standard-filename ".cpp.el")
  "*File name to save cpp configuration."
  :type 'file
  :group 'cpp)

(define-widget 'cpp-face 'lazy
  "Either a face or the special symbol 'invisible'."
  :type '(choice (const invisible) (face)))

(defcustom cpp-known-face 'invisible
  "*Face used for known cpp symbols."
  :type 'cpp-face
  :group 'cpp)

(defcustom cpp-unknown-face 'highlight
  "*Face used for unknown cpp symbols."
  :type 'cpp-face
  :group 'cpp)

(defcustom cpp-face-type 'light
  "*Indicate what background face type you prefer.
Can be either light or dark for color screens, mono for monochrome
screens, and none if you don't use a window system and don't have
a color-capable display."
  :options '(light dark mono nil)
  :type 'symbol
  :group 'cpp)

(defcustom cpp-known-writable t
  "*Non-nil means you are allowed to modify the known conditionals."
  :type 'boolean
  :group 'cpp)

(defcustom cpp-unknown-writable t
  "*Non-nil means you are allowed to modify the unknown conditionals."
  :type 'boolean
  :group 'cpp)

(defcustom cpp-edit-list nil
  "Alist of cpp macros and information about how they should be displayed.
Each entry is a list with the following elements:
0. The name of the macro (a string).
1. Face used for text that is `ifdef' the macro.
2. Face used for text that is `ifndef' the macro.
3. t, nil, or `both' depending on what text may be edited."
  :type '(repeat (list (string :tag "Macro")
		       (cpp-face :tag "True")
		       (cpp-face :tag "False")
		       (choice (const :tag "True branch writable" t)
			       (const :tag "False branch writable" nil)
			       (const :tag "Both branches writable" both))))
  :group 'cpp)

(defvar cpp-overlay-list nil)
;; List of cpp overlays active in the current buffer.
(make-variable-buffer-local 'cpp-overlay-list)

(defvar cpp-callback-data)
(defvar cpp-state-stack)

(defconst cpp-face-type-list
  '(("light color background" . light)
    ("dark color background" . dark)
    ("monochrome" . mono)
    ("tty" . none))
  "Alist of strings and names of the defined face collections.")

(defconst cpp-writable-list
  ;; Names used for the writable property.
  '(("writable" . t)
    ("read-only" . nil)))

(defvar cpp-button-event nil)
;; This will be t in the callback for `cpp-make-button'.

(defvar cpp-edit-buffer nil)
;; Real buffer whose cpp display information we are editing.
(make-variable-buffer-local 'cpp-edit-buffer)

(defconst cpp-branch-list
  ;; Alist of branches.
  '(("false" . nil)
    ("true" . t)
    ("both" . both)))

(defcustom cpp-face-default-list nil
  "Alist of faces you can choose from for cpp conditionals.
Each element has the form (STRING . FACE), where STRING
serves as a name (for `cpp-highlight-buffer' only)
and FACE is either a face (a symbol)
or a cons cell (background-color . COLOR)."
  :type '(repeat (cons string (choice face (cons (const background-color) string))))
  :group 'cpp)

(defcustom cpp-face-light-name-list
  '("light gray" "light blue" "light cyan" "light yellow" "light pink"
    "pale green" "beige" "orange" "magenta" "violet" "medium purple"
    "turquoise")
  "Background colors useful with dark foreground colors."
  :type '(repeat string)
  :group 'cpp)

(defcustom cpp-face-dark-name-list
  '("dim gray" "blue" "cyan" "yellow" "red"
    "dark green" "brown" "dark orange" "dark khaki" "dark violet" "purple"
    "dark turquoise")
  "Background colors useful with light foreground colors."
  :type '(repeat string)
  :group 'cpp)

(defcustom cpp-face-light-list nil
  "Alist of names and faces to be used for light backgrounds."
  :type '(repeat (cons string (choice face
				      (cons (const background-color) string))))
  :group 'cpp)

(defcustom cpp-face-dark-list nil
  "Alist of names and faces to be used for dark backgrounds."
  :type '(repeat (cons string (choice face
				      (cons (const background-color) string))))
  :group 'cpp)

(defcustom cpp-face-mono-list
  '(("bold" . bold)
    ("bold-italic" . bold-italic)
    ("italic" . italic)
    ("underline" . underline))
  "Alist of names and faces to be used for monochrome screens."
  :type '(repeat (cons string face))
  :group 'cpp)

(defcustom cpp-face-none-list
   '(("default" . default)
     ("invisible" . invisible))
   "Alist of names and faces available even if you don't use a window system."
  :type '(repeat (cons string cpp-face))
  :group 'cpp)

(defvar cpp-face-all-list
  (append cpp-face-light-list
	  cpp-face-dark-list
	  cpp-face-mono-list
	  cpp-face-none-list)
  "All faces used for highlighting text inside cpp conditionals.")

;;; Parse Buffer:

(defvar cpp-parse-symbols nil
  "List of cpp macros used in the local buffer.")
(make-variable-buffer-local 'cpp-parse-symbols)

(defconst cpp-parse-regexp
  ;; Regexp matching all tokens needed to find conditionals.
  (concat
   "'\\|\"\\|/\\*\\|//\\|"
   "\\(^[ \t]*#[ \t]*\\(ifdef\\|ifndef\\|if\\|"
   "elif\\|else\\|endif\\)\\b\\)"))

;;;###autoload
(defun cpp-highlight-buffer (arg)
  "Highlight C code according to preprocessor conditionals.
This command pops up a buffer which you should edit to specify
what kind of highlighting to use, and the criteria for highlighting.
A prefix arg suppresses display of that buffer."
  (interactive "P")
  (unless (or (eq t buffer-invisibility-spec)
	      (memq 'cpp buffer-invisibility-spec))
    (add-to-invisibility-spec 'cpp))
  (setq cpp-parse-symbols nil)
  (cpp-parse-reset)
  (if (null cpp-edit-list)
      (cpp-edit-load))
  (let (cpp-state-stack)
    (save-excursion
      (goto-char (point-min))
      (cpp-progress-message "Parsing...")
      (while (re-search-forward cpp-parse-regexp nil t)
	(cpp-progress-message "Parsing...%d%%"
			  (/ (* 100 (- (point) (point-min))) (buffer-size)))
	(let ((match (buffer-substring (match-beginning 0) (match-end 0))))
	  (cond ((or (string-equal match "'")
		     (string-equal match "\""))
		 (goto-char (match-beginning 0))
		 (condition-case nil
		     (forward-sexp)
		   (error (cpp-parse-error
			   "Unterminated string or character"))))
		((string-equal match "/*")
		 (or (search-forward "*/" nil t)
		     (error "Unterminated comment")))
		((string-equal match "//")
		 (skip-chars-forward "^\n\r"))
		(t
		 (end-of-line 1)
		 (let ((from (match-beginning 1))
		       (to (1+ (point)))
		       (type (buffer-substring (match-beginning 2)
					       (match-end 2)))
		       (expr (buffer-substring (match-end 1) (point))))
		   (cond ((string-equal type "ifdef")
			  (cpp-parse-open t expr from to))
			 ((string-equal type "ifndef")
			  (cpp-parse-open nil expr from to))
			 ((string-equal type "if")
			  (cpp-parse-open t expr from to))
			 ((string-equal type "elif")
			  (let (cpp-known-face cpp-unknown-face)
			    (cpp-parse-close from to))
			  (cpp-parse-open t expr from to))
			 ((string-equal type "else")
			  (or cpp-state-stack
			      (cpp-parse-error "Top level #else"))
			  (let ((entry (list (not (nth 0 (car cpp-state-stack)))
					     (nth 1 (car cpp-state-stack))
					     from to)))
			    (cpp-parse-close from to)
			    (setq cpp-state-stack (cons entry cpp-state-stack))))
			 ((string-equal type "endif")
			  (cpp-parse-close from to))
			 (t
			  (cpp-parse-error "Parser error"))))))))
      (message "Parsing...done"))
    (if cpp-state-stack
      (save-excursion
	(goto-char (nth 3 (car cpp-state-stack)))
	(cpp-parse-error "Unclosed conditional"))))
  (or arg
      (null cpp-parse-symbols)
      (cpp-parse-edit)))

(defun cpp-parse-open (branch expr begin end)
  "Push information about conditional-beginning onto `cpp-state-stack'."
  ;; Discard comments within this line.
  (while (string-match "\\b[ \t]*/\\*.*\\*/[ \t]*\\b" expr)
    (setq expr (concat (substring expr 0 (match-beginning 0))
		       (substring expr (match-end 0)))))
  ;; If a comment starts on this line and continues past, discard it.
  (if (string-match "\\b[ \t]*/\\*" expr)
      (setq expr (substring expr 0 (match-beginning 0))))
  ;; Delete any C++ comment from the line.
  (if (string-match "\\b[ \t]*\\(//.*\\)?$" expr)
      (setq expr (substring expr 0 (match-beginning 0))))
  (while (string-match "[ \t]+" expr)
      (setq expr (concat (substring expr 0 (match-beginning 0))
			 (substring expr (match-end 0)))))
  (setq cpp-state-stack (cons (list branch expr begin end) cpp-state-stack))
  (or (member expr cpp-parse-symbols)
      (setq cpp-parse-symbols
	    (cons expr cpp-parse-symbols)))
  (if (assoc expr cpp-edit-list)
      (cpp-make-known-overlay begin end)
    (cpp-make-unknown-overlay begin end)))

(defun cpp-parse-close (from to)
  ;; Pop top of cpp-state-stack and create overlay.
  (let ((entry (assoc (nth 1 (car cpp-state-stack)) cpp-edit-list))
	(branch (nth 0 (car cpp-state-stack)))
	(end (nth 3 (car cpp-state-stack))))
    (setq cpp-state-stack (cdr cpp-state-stack))
    (if entry
	(let ((face (nth (if branch 1 2) entry))
	      (read-only (eq (not branch) (nth 3 entry)))
	      (priority (length cpp-state-stack))
	      (overlay (make-overlay end from)))
	  (cpp-make-known-overlay from to)
	  (setq cpp-overlay-list (cons overlay cpp-overlay-list))
	  (if priority (overlay-put overlay 'priority priority))
	  (cond ((eq face 'invisible)
		 (cpp-make-overlay-hidden overlay))
		((eq face 'default))
		(t
		 (overlay-put overlay 'face face)))
	  (if read-only
	      (cpp-make-overlay-read-only overlay)
	    (cpp-make-overlay-sticky overlay)))
      (cpp-make-unknown-overlay from to))))

(defun cpp-parse-error (error)
  ;; Error message issued by the cpp parser.
  (error "%s at line %d" error (count-lines (point-min) (point))))

(defun cpp-parse-reset ()
  "Reset display of cpp conditionals to normal."
  (interactive)
  (while cpp-overlay-list
    (delete-overlay (car cpp-overlay-list))
    (setq cpp-overlay-list (cdr cpp-overlay-list))))

;;;###autoload
(defun cpp-parse-edit ()
  "Edit display information for cpp conditionals."
  (interactive)
  (or cpp-parse-symbols
      (cpp-highlight-buffer t))
  (let ((buffer (current-buffer)))
    (pop-to-buffer "*CPP Edit*")
    (cpp-edit-mode)
    (setq cpp-edit-buffer buffer)
    (cpp-edit-reset)))

;;; Overlays:

(defun cpp-make-known-overlay (start end)
  ;; Create an overlay for a known cpp command from START to END.
  (let ((overlay (make-overlay start end)))
    (if (eq cpp-known-face 'invisible)
	(cpp-make-overlay-hidden overlay)
      (or (eq cpp-known-face 'default)
	  (overlay-put overlay 'face cpp-known-face))
      (if cpp-known-writable
	  ()
	(overlay-put overlay 'modification-hooks '(cpp-signal-read-only))
	(overlay-put overlay 'insert-in-front-hooks '(cpp-signal-read-only))))
    (setq cpp-overlay-list (cons overlay cpp-overlay-list))))

(defun cpp-make-unknown-overlay (start end)
  ;; Create an overlay for an unknown cpp command from START to END.
  (let ((overlay (make-overlay start end)))
    (cond ((eq cpp-unknown-face 'invisible)
	   (cpp-make-overlay-hidden overlay))
	  ((eq cpp-unknown-face 'default))
	  (t
	   (overlay-put overlay 'face cpp-unknown-face)))
    (if cpp-unknown-writable
	()
      (overlay-put overlay 'modification-hooks '(cpp-signal-read-only))
      (overlay-put overlay 'insert-in-front-hooks '(cpp-signal-read-only)))
    (setq cpp-overlay-list (cons overlay cpp-overlay-list))))

(defun cpp-make-overlay-hidden (overlay)
  ;; Make overlay hidden and intangible.
  (overlay-put overlay 'invisible 'cpp)
  (overlay-put overlay 'modification-hooks '(cpp-signal-read-only))
  (overlay-put overlay 'insert-in-front-hooks '(cpp-signal-read-only)))

(defun cpp-make-overlay-read-only (overlay)
  ;; Make overlay read only.
  (overlay-put overlay 'modification-hooks '(cpp-signal-read-only))
  (overlay-put overlay 'insert-in-front-hooks '(cpp-signal-read-only))
  (overlay-put overlay 'insert-behind-hooks '(cpp-signal-read-only)))

(defun cpp-make-overlay-sticky (overlay)
  ;; Make OVERLAY grow when you insert text at either end.
  (overlay-put overlay 'insert-in-front-hooks '(cpp-grow-overlay))
  (overlay-put overlay 'insert-behind-hooks '(cpp-grow-overlay)))

(defun cpp-signal-read-only (overlay after start end &optional _len)
  ;; Only allow deleting the whole overlay.
  ;; Trying to change a read-only overlay.
  (if (and (not after)
	   (or (< (overlay-start overlay) start)
	       (> (overlay-end overlay) end)))
      (error "This text is read only")))

(defun cpp-grow-overlay (overlay after start end &optional _len)
  ;; Make OVERLAY grow to contain range START to END.
  (if after
      (move-overlay overlay
		    (min start (overlay-start overlay))
		    (max end (overlay-end overlay)))))

;;; Edit Buffer:

(defvar cpp-edit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [ down-mouse-2 ] 'cpp-push-button)
    (define-key map [ mouse-2 ] 'ignore)
    (define-key map " " 'scroll-up-command)
    (define-key map "\C-?" 'scroll-down-command)
    (define-key map [ delete ] 'scroll-down)
    (define-key map "\C-c\C-c" 'cpp-edit-apply)
    (define-key map "a" 'cpp-edit-apply)
    (define-key map "A" 'cpp-edit-apply)
    (define-key map "r" 'cpp-edit-reset)
    (define-key map "R" 'cpp-edit-reset)
    (define-key map "s" 'cpp-edit-save)
    (define-key map "S" 'cpp-edit-save)
    (define-key map "l" 'cpp-edit-load)
    (define-key map "L" 'cpp-edit-load)
    (define-key map "h" 'cpp-edit-home)
    (define-key map "H" 'cpp-edit-home)
    (define-key map "b" 'cpp-edit-background)
    (define-key map "B" 'cpp-edit-background)
    (define-key map "k" 'cpp-edit-known)
    (define-key map "K" 'cpp-edit-known)
    (define-key map "u" 'cpp-edit-unknown)
    (define-key map "u" 'cpp-edit-unknown)
    (define-key map "t" 'cpp-edit-true)
    (define-key map "T" 'cpp-edit-true)
    (define-key map "f" 'cpp-edit-false)
    (define-key map "F" 'cpp-edit-false)
    (define-key map "w" 'cpp-edit-write)
    (define-key map "W" 'cpp-edit-write)
    (define-key map "X" 'cpp-edit-toggle-known)
    (define-key map "x" 'cpp-edit-toggle-known)
    (define-key map "Y" 'cpp-edit-toggle-unknown)
    (define-key map "y" 'cpp-edit-toggle-unknown)
    (define-key map "q" 'bury-buffer)
    (define-key map "Q" 'bury-buffer)
    map)
  "Keymap for `cpp-edit-mode'.")



(defvar cpp-edit-symbols nil)
;; Symbols defined in the edit buffer.
(make-variable-buffer-local 'cpp-edit-symbols)

(define-derived-mode cpp-edit-mode fundamental-mode "CPP Edit"
  "Major mode for editing the criteria for highlighting cpp conditionals.
Click on objects to change them.
You can also use the keyboard accelerators indicated like this: [K]ey."
  (buffer-disable-undo)
  (auto-save-mode -1)
  (setq buffer-read-only t))

(defun cpp-edit-apply ()
  "Apply edited display information to original buffer."
  (interactive)
  (cpp-edit-home)
  (cpp-highlight-buffer t))

(defun cpp-edit-reset ()
  "Reset display information from original buffer."
  (interactive)
  (let ((buffer (current-buffer))
	(buffer-read-only nil)
	(start (window-start))
	(pos (point))
	symbols)
    (set-buffer cpp-edit-buffer)
    (setq symbols cpp-parse-symbols)
    (set-buffer buffer)
    (setq cpp-edit-symbols symbols)
    (erase-buffer)
    (insert "CPP Display Information for `")
    (cpp-make-button (buffer-name cpp-edit-buffer) 'cpp-edit-home)
    (insert "'\n\nClick mouse-2 on item you want to change or use\n"
	    "or switch to this buffer and type the keyboard equivalents.\n"
	    "Keyboard equivalents are indicated with brackets like [T]his.\n\n")
    (cpp-make-button "[H]ome (display the C file)" 'cpp-edit-home)
    (insert "  ")
    (cpp-make-button "[A]pply new settings" 'cpp-edit-apply)
    (insert "\n")
    (cpp-make-button "[S]ave settings" 'cpp-edit-save)
    (insert "  ")
    (cpp-make-button "[L]oad settings" 'cpp-edit-load)
    (insert "\n\n")

    (insert "[B]ackground: ")
    (cpp-make-button (car (rassq cpp-face-type cpp-face-type-list))
		     'cpp-edit-background)
    (insert "\n[K]nown conditionals: ")
    (cpp-make-button (cpp-face-name cpp-known-face)
		     'cpp-edit-known nil t)
    (insert " [X] ")
    (cpp-make-button (car (rassq cpp-known-writable cpp-writable-list))
		     'cpp-edit-toggle-known)
    (insert "\n[U]nknown conditionals: ")
    (cpp-make-button (cpp-face-name cpp-unknown-face)
		     'cpp-edit-unknown nil t)
    (insert " [Y] ")
    (cpp-make-button (car (rassq cpp-unknown-writable cpp-writable-list))
		     'cpp-edit-toggle-unknown)
    (insert (format "\n\n\n%39s: %14s %14s %7s\n\n" "Expression"
		    "[T]rue Face" "[F]alse Face" "[W]rite"))

    (setq symbols (reverse symbols))
    (while symbols
      (let*  ((symbol (car symbols))
	      (entry (assoc symbol cpp-edit-list))
	      (true (nth 1 entry))
	      (false (nth 2 entry))
	      (write (if entry (nth 3 entry) 'both)))
	(setq symbols (cdr symbols))

	(if (and entry			; Make default entries unknown.
		 (or (null true) (eq true 'default))
		 (or (null false) (eq false 'default))
		 (eq write 'both))
	    (setq cpp-edit-list (delq entry cpp-edit-list)
		  entry nil))

	(if (> (length symbol) 39)
	    (insert (substring symbol 0 39) ": ")
	  (insert (format "%39s: " symbol)))

	(cpp-make-button (cpp-face-name true)
			 'cpp-edit-true symbol t 14)
	(insert " ")
	(cpp-make-button (cpp-face-name false)
			 'cpp-edit-false symbol t 14)
	(insert " ")
	(cpp-make-button (car (rassq write cpp-branch-list))
			 'cpp-edit-write symbol nil 6)
	(insert "\n")))
    (insert "\n\n")
    (set-window-start nil start)
    (goto-char pos)))

(defun cpp-edit-load ()
  "Load cpp configuration."
  (interactive)
  (cond ((null init-file-user)
	 ;; If -q was specified, don't load any init files.
	 nil)
	((file-readable-p cpp-config-file)
	 (load-file cpp-config-file))
	((file-readable-p (concat "~/" cpp-config-file))
	 (load-file cpp-config-file)))
  (if (derived-mode-p 'cpp-edit-mode)
      (cpp-edit-reset)))

(defun cpp-edit-save ()
  "Save the current cpp configuration in a file."
  (interactive)
  (require 'pp)
  (with-current-buffer cpp-edit-buffer
    (let ((buffer (find-file-noselect cpp-config-file)))
      (set-buffer buffer)
      (erase-buffer)
      (pp (list 'setq 'cpp-known-face
		(list 'quote cpp-known-face)) buffer)
      (pp (list 'setq 'cpp-unknown-face
		(list 'quote cpp-unknown-face)) buffer)
      (pp (list 'setq 'cpp-face-type
		(list 'quote cpp-face-type)) buffer)
      (pp (list 'setq 'cpp-known-writable
		(list 'quote cpp-known-writable)) buffer)
      (pp (list 'setq 'cpp-unknown-writable
		(list 'quote cpp-unknown-writable)) buffer)
      (pp (list 'setq 'cpp-edit-list
		(list 'quote cpp-edit-list)) buffer)
      (write-file cpp-config-file))))

(defun cpp-edit-home ()
  "Switch back to original buffer."
  (interactive)
  (if cpp-button-event
      (read-event))
  (pop-to-buffer cpp-edit-buffer))

(defun cpp-edit-background ()
  "Change default face collection."
  (interactive)
  (call-interactively 'cpp-choose-default-face)
  (cpp-edit-reset))

(defun cpp-edit-known ()
  "Select default for known conditionals."
  (interactive)
  (setq cpp-known-face (cpp-choose-face "Known face" cpp-known-face))
  (cpp-edit-reset))

(defun cpp-edit-unknown ()
  "Select default for unknown conditionals."
  (interactive)
  (setq cpp-unknown-face (cpp-choose-face "Unknown face" cpp-unknown-face))
  (cpp-edit-reset))

(defun cpp-edit-toggle-known (arg)
  "Toggle writable status for known conditionals.
With optional argument ARG, make them writable if ARG is positive,
otherwise make them unwritable."
  (interactive "@P")
  (if (or (and (null arg) cpp-known-writable)
	  (<= (prefix-numeric-value arg) 0))
      (setq cpp-known-writable nil)
    (setq cpp-known-writable t))
  (cpp-edit-reset))

(defun cpp-edit-toggle-unknown (arg)
  "Toggle writable status for unknown conditionals.
With optional argument ARG, make them writable if ARG is positive,
otherwise make them unwritable."
  (interactive "@P")
  (if (or (and (null arg) cpp-unknown-writable)
	  (<= (prefix-numeric-value arg) 0))
      (setq cpp-unknown-writable nil)
    (setq cpp-unknown-writable t))
  (cpp-edit-reset))

(defun cpp-edit-true (symbol face)
  "Select SYMBOL's true FACE used for highlighting taken conditionals."
  (interactive
   (let ((symbol (cpp-choose-symbol)))
     (list symbol
	   (cpp-choose-face "True face"
			    (nth 1 (assoc symbol cpp-edit-list))))))
  (setcar (nthcdr 1 (cpp-edit-list-entry-get-or-create symbol)) face)
  (cpp-edit-reset))

(defun cpp-edit-false (symbol face)
  "Select SYMBOL's false FACE used for highlighting untaken conditionals."
  (interactive
   (let ((symbol (cpp-choose-symbol)))
     (list symbol
	   (cpp-choose-face "False face"
			    (nth 2 (assoc symbol cpp-edit-list))))))
  (setcar (nthcdr 2 (cpp-edit-list-entry-get-or-create symbol)) face)
  (cpp-edit-reset))

(defun cpp-edit-write (symbol branch)
  "Set which branches of SYMBOL should be writable to BRANCH.
BRANCH should be either nil (false branch), t (true branch) or 'both."
  (interactive (list (cpp-choose-symbol) (cpp-choose-branch)))
  (setcar (nthcdr 3 (cpp-edit-list-entry-get-or-create symbol)) branch)
  (cpp-edit-reset))

(defun cpp-edit-list-entry-get-or-create (symbol)
  ;; Return the entry for SYMBOL in `cpp-edit-list'.
  ;; If it does not exist, create it.
  (let ((entry (assoc symbol cpp-edit-list)))
    (or entry
	(setq entry (list symbol nil nil 'both nil)
	      cpp-edit-list (cons entry cpp-edit-list)))
    entry))

;;; Prompts:

(defun cpp-choose-symbol ()
  ;; Choose a symbol if called from keyboard, otherwise use the one clicked on.
  (if cpp-button-event
      cpp-callback-data
    (completing-read "Symbol: " cpp-edit-symbols nil t)))

(defun cpp-choose-branch ()
  ;; Choose a branch, either nil, t, or both.
  (if cpp-button-event
      (x-popup-menu cpp-button-event
		    (list "Branch" (cons "Branch" cpp-branch-list)))
    (cdr (assoc	(completing-read "Branch: " cpp-branch-list nil t)
		cpp-branch-list))))

(defun cpp-choose-face (prompt default)
  ;; Choose a face from cpp-face-default-list.
  ;; PROMPT is what to say to the user.
  ;; DEFAULT is the default face.
  (or (if cpp-button-event
	  (x-popup-menu cpp-button-event
			(list prompt (cons prompt cpp-face-default-list)))
	(let ((name (car (rassq default cpp-face-default-list))))
	  (cdr (assoc (completing-read (if name
					   (concat prompt
						   " (default " name "): ")
					 (concat prompt ": "))
				       cpp-face-default-list nil t)
		      cpp-face-all-list))))
      default))

(defun cpp-choose-default-face (type)
  ;; Choose default face list for screen of TYPE.
  ;; Type must be one of the types defined in `cpp-face-type-list'.
  (interactive (list (if cpp-button-event
			 (x-popup-menu cpp-button-event
				       (list "Screen type"
					     (cons "Screen type"
						   cpp-face-type-list)))
		       (cdr (assoc (completing-read "Screen type: "
						    cpp-face-type-list
						    nil t)
				   cpp-face-type-list)))))
  (cond ((null type))
	((eq type 'light)
	 (if cpp-face-light-list
	     ()
	   (setq cpp-face-light-list
		 (mapcar 'cpp-create-bg-face cpp-face-light-name-list))
	   (setq cpp-face-all-list
		 (append cpp-face-all-list cpp-face-light-list)))
	 (setq cpp-face-type 'light)
	 (setq cpp-face-default-list
	       (append cpp-face-light-list cpp-face-none-list)))
	((eq type 'dark)
	 (if cpp-face-dark-list
	     ()
	   (setq cpp-face-dark-list
		 (mapcar 'cpp-create-bg-face cpp-face-dark-name-list))
	   (setq cpp-face-all-list
		 (append cpp-face-all-list cpp-face-dark-list)))
	 (setq cpp-face-type 'dark)
	 (setq cpp-face-default-list
	       (append cpp-face-dark-list cpp-face-none-list)))
	((eq type 'mono)
	 (setq cpp-face-type 'mono)
	 (setq cpp-face-default-list
	       (append cpp-face-mono-list cpp-face-none-list)))
	(t
	 (setq cpp-face-type 'none)
	 (setq cpp-face-default-list cpp-face-none-list))))

;;; Buttons:

(defun cpp-make-button (name callback &optional data face padding)
  ;; Create a button at point.
  ;; NAME is the name of the button.
  ;; CALLBACK is the function to call when the button is pushed.
  ;; DATA will be made available to CALLBACK
  ;;in the free variable cpp-callback-data.
  ;; FACE means that NAME is the name of a face in `cpp-face-all-list'.
  ;; PADDING means NAME will be right justified at that length.
  (let ((name (format "%s" name))
	from to)
    (cond ((null padding)
	   (setq from (point))
	   (insert name))
	  ((> (length name) padding)
	   (setq from (point))
	   (insert (substring name 0 padding)))
	  (t
	   (insert (make-string (- padding (length name)) ? ))
	   (setq from (point))
	   (insert name)))
    (setq to (point))
    (setq face
	  (if face
	      (let ((check (cdr (assoc name cpp-face-all-list))))
		(if (memq check '(default invisible))
		    'bold
		  check))
	    'bold))
    (add-text-properties from to
			 (append (list 'face face)
				 '(mouse-face highlight)
				 '(help-echo "mouse-2: change/use this item")
				 (list 'cpp-callback callback)
				 (if data (list 'cpp-data data))))))

(defun cpp-push-button (event)
  ;; Pushed a CPP button.
  (interactive "@e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let ((pos (posn-point (event-start event))))
    (let ((cpp-callback-data (get-text-property pos 'cpp-data))
	  (fun (get-text-property pos 'cpp-callback))
	  (cpp-button-event event))
      (cond (fun
	     (call-interactively (get-text-property pos 'cpp-callback)))
	    ((lookup-key global-map [ down-mouse-2])
	     (call-interactively (lookup-key global-map [ down-mouse-2])))))))

;;; Faces:

(defun cpp-create-bg-face (color)
  ;; Create entry for face with background COLOR.
  (cons color (cons 'background-color color)))

(cpp-choose-default-face
 (if (or window-system (display-color-p)) cpp-face-type 'none))

(defun cpp-face-name (face)
  ;; Return the name of FACE from `cpp-face-all-list'.
  (let ((entry (rassq (if face face 'default) cpp-face-all-list)))
    (if entry
	(car entry)
      (format "<%s>" face))))

;;; Utilities:

(defvar cpp-progress-time 0)
;; Last time we issued a progress message.

(defun cpp-progress-message (&rest args)
  ;; Report progress at most once a second.  Take same ARGS as `message'.
  (let ((time (nth 1 (current-time))))
    (if (= time cpp-progress-time)
	()
      (setq cpp-progress-time time)
      (apply 'message args))))

(provide 'cpp)

;;; cpp.el ends here
