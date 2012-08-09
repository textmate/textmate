;;; hi-lock.el --- minor mode for interactive automatic highlighting

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: David M. Koppelman <koppel@ece.lsu.edu>
;; Keywords: faces, minor-mode, matching, display

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
;;
;;  With the hi-lock commands text matching interactively entered
;;  regexp's can be highlighted.  For example, `M-x highlight-regexp
;;  RET clearly RET RET' will highlight all occurrences of `clearly'
;;  using a yellow background face.  New occurrences of `clearly' will
;;  be highlighted as they are typed.  `M-x unhighlight-regexp RET'
;;  will remove the highlighting.  Any existing face can be used for
;;  highlighting and a set of appropriate faces is provided.  The
;;  regexps can be written into the current buffer in a form that will
;;  be recognized the next time the corresponding file is read (when
;;  file patterns is turned on).
;;
;;  Applications:
;;
;;    In program source code highlight a variable to quickly see all
;;    places it is modified or referenced:
;;    M-x highlight-regexp ground_contact_switches_closed RET RET
;;
;;    In a shell or other buffer that is showing lots of program
;;    output, highlight the parts of the output you're interested in:
;;    M-x highlight-regexp Total execution time [0-9]+ RET hi-blue-b RET
;;
;;    In buffers displaying tables, highlight the lines you're interested in:
;;    M-x highlight-lines-matching-regexp January 2000 RET hi-black-b RET
;;
;;    When writing text, highlight personal cliches.  This can be
;;    amusing.
;;    M-x highlight-phrase as can be seen RET RET
;;
;;  Setup:
;;
;;    Put the following code in your .emacs file.  This turns on
;;    hi-lock mode and adds a "Regexp Highlighting" entry
;;    to the edit menu.
;;
;;    (global-hi-lock-mode 1)
;;
;;    To enable the use of patterns found in files (presumably placed
;;    there by hi-lock) include the following in your .emacs file:
;;
;;    (setq hi-lock-file-patterns-policy 'ask)
;;
;;    If you get tired of being asked each time a file is loaded replace
;;    `ask' with a function that returns t if patterns should be read.
;;
;;    You might also want to bind the hi-lock commands to more
;;    finger-friendly sequences:

;;    (define-key hi-lock-map "\C-z\C-h" 'highlight-lines-matching-regexp)
;;    (define-key hi-lock-map "\C-zi" 'hi-lock-find-patterns)
;;    (define-key hi-lock-map "\C-zh" 'highlight-regexp)
;;    (define-key hi-lock-map "\C-zp" 'highlight-phrase)
;;    (define-key hi-lock-map "\C-zr" 'unhighlight-regexp)
;;    (define-key hi-lock-map "\C-zb" 'hi-lock-write-interactive-patterns))

;;    See the documentation for hi-lock-mode `C-h f hi-lock-mode' for
;;    additional instructions.

;; Sample file patterns:

; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
; Hi-lock: ( ("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))))
; Hi-lock: end

;;; Code:

(require 'font-lock)

(defgroup hi-lock nil
  "Interactively add and remove font-lock patterns for highlighting text."
  :link '(custom-manual "(emacs)Highlight Interactively")
  :group 'font-lock)

(defcustom hi-lock-file-patterns-range 10000
  "Limit of search in a buffer for hi-lock patterns.
When a file is visited and hi-lock mode is on, patterns starting
up to this limit are added to font-lock's patterns.  See documentation
of functions `hi-lock-mode' and `hi-lock-find-patterns'."
  :type 'integer
  :group 'hi-lock)

(defcustom hi-lock-highlight-range 200000
  "Size of area highlighted by hi-lock when font-lock not active.
Font-lock is not active in buffers that do their own highlighting,
such as the buffer created by `list-colors-display'.  In those buffers
hi-lock patterns will only be applied over a range of
`hi-lock-highlight-range' characters.  If font-lock is active then
highlighting will be applied throughout the buffer."
  :type 'integer
  :group 'hi-lock)

(defcustom hi-lock-exclude-modes
  '(rmail-mode mime/viewer-mode gnus-article-mode)
  "List of major modes in which hi-lock will not run.
For security reasons since font lock patterns can specify function
calls."
  :type '(repeat symbol)
  :group 'hi-lock)

(defcustom hi-lock-file-patterns-policy 'ask
  "Specify when hi-lock should use patterns found in file.
If `ask', prompt when patterns found in buffer; if bound to a function,
use patterns when function returns t (function is called with patterns
as first argument); if nil or `never' or anything else, don't use file
patterns."
  :type '(choice (const :tag "Do not use file patterns" never)
                 (const :tag "Ask about file patterns" ask)
                 (function :tag "Function to check file patterns"))
  :group 'hi-lock
  :version "22.1")

;; It can have a function value.
(put 'hi-lock-file-patterns-policy 'risky-local-variable t)

(defgroup hi-lock-faces nil
  "Faces for hi-lock."
  :group 'hi-lock
  :group 'faces)

(defface hi-yellow
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Default face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink
  '((((background dark)) (:background "pink" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green
  '((((min-colors 88) (background dark))
     (:background "green1" :foreground "black"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-blue
  '((((background dark)) (:background "light blue" :foreground "black"))
    (t (:background "light blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-black-b
  '((t (:weight bold)))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-blue-b
  '((((min-colors 88)) (:weight bold :foreground "blue1"))
    (t (:weight bold :foreground "blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green-b
  '((((min-colors 88)) (:weight bold :foreground "green1"))
    (t (:weight bold :foreground "green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-red-b
  '((((min-colors 88)) (:weight bold :foreground "red1"))
    (t (:weight bold :foreground "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-black-hb
  '((t (:weight bold :height 1.67 :inherit variable-pitch)))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defvar hi-lock-file-patterns nil
  "Patterns found in file for hi-lock.  Should not be changed.")

(defvar hi-lock-interactive-patterns nil
  "Patterns provided to hi-lock by user.  Should not be changed.")

(defvar hi-lock-face-defaults
  '("hi-yellow" "hi-pink" "hi-green" "hi-blue" "hi-black-b"
    "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb")
  "Default faces for hi-lock interactive functions.")

;(dolist (f hi-lock-face-defaults) (unless (facep f) (error "%s not a face" f)))

(define-obsolete-variable-alias 'hi-lock-face-history
                                'hi-lock-face-defaults
                                "23.1")

(define-obsolete-variable-alias 'hi-lock-regexp-history
                                'regexp-history
                                "23.1")

(defvar hi-lock-file-patterns-prefix "Hi-lock"
  "Search target for finding hi-lock patterns at top of file.")

(defvar hi-lock-archaic-interface-message-used nil
  "True if user alerted that `global-hi-lock-mode' is now the global switch.
Earlier versions of hi-lock used `hi-lock-mode' as the global switch;
the message is issued if it appears that `hi-lock-mode' is used assuming
that older functionality.  This variable avoids multiple reminders.")

(defvar hi-lock-archaic-interface-deduce nil
  "If non-nil, sometimes assume that `hi-lock-mode' means `global-hi-lock-mode'.
Assumption is made if `hi-lock-mode' used in the *scratch* buffer while
a library is being loaded.")

(make-variable-buffer-local 'hi-lock-interactive-patterns)
(put 'hi-lock-interactive-patterns 'permanent-local t)
(make-variable-buffer-local 'hi-lock-file-patterns)
(put 'hi-lock-file-patterns 'permanent-local t)

(defvar hi-lock-menu
  (let ((map (make-sparse-keymap "Hi Lock")))
    (define-key-after map [highlight-regexp]
      '(menu-item "Highlight Regexp..." highlight-regexp
        :help "Highlight text matching PATTERN (a regexp)."))

    (define-key-after map [highlight-phrase]
      '(menu-item "Highlight Phrase..." highlight-phrase
        :help "Highlight text matching PATTERN (a regexp processed to match phrases)."))

    (define-key-after map [highlight-lines-matching-regexp]
      '(menu-item "Highlight Lines..." highlight-lines-matching-regexp
        :help "Highlight lines containing match of PATTERN (a regexp)."))

    (define-key-after map [unhighlight-regexp]
      '(menu-item "Remove Highlighting..." unhighlight-regexp
        :help "Remove previously entered highlighting pattern."
        :enable hi-lock-interactive-patterns))

    (define-key-after map [hi-lock-write-interactive-patterns]
      '(menu-item "Patterns to Buffer" hi-lock-write-interactive-patterns
        :help "Insert interactively added REGEXPs into buffer at point."
        :enable hi-lock-interactive-patterns))

    (define-key-after map [hi-lock-find-patterns]
      '(menu-item "Patterns from Buffer" hi-lock-find-patterns
        :help "Use patterns (if any) near top of buffer."))
    map)
  "Menu for hi-lock mode.")

(defvar hi-lock-map
  (let ((map (make-sparse-keymap "Hi Lock")))
    (define-key map "\C-xwi" 'hi-lock-find-patterns)
    (define-key map "\C-xwl" 'highlight-lines-matching-regexp)
    (define-key map "\C-xwp" 'highlight-phrase)
    (define-key map "\C-xwh" 'highlight-regexp)
    (define-key map "\C-xwr" 'unhighlight-regexp)
    (define-key map "\C-xwb" 'hi-lock-write-interactive-patterns)
    map)
  "Key map for hi-lock.")

;; Visible Functions

;;;###autoload
(define-minor-mode hi-lock-mode
  "Toggle selective highlighting of patterns (Hi Lock mode).
With a prefix argument ARG, enable Hi Lock mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Issuing one the highlighting commands listed below will
automatically enable Hi Lock mode.  To enable Hi Lock mode in all
buffers, use `global-hi-lock-mode' or add (global-hi-lock-mode 1)
to your init file.  When Hi Lock mode is enabled, a \"Regexp
Highlighting\" submenu is added to the \"Edit\" menu.  The
commands in the submenu, which can be called interactively, are:

\\[highlight-regexp] REGEXP FACE
  Highlight matches of pattern REGEXP in current buffer with FACE.

\\[highlight-phrase] PHRASE FACE
  Highlight matches of phrase PHRASE in current buffer with FACE.
  (PHRASE can be any REGEXP, but spaces will be replaced by matches
  to whitespace and initial lower-case letters will become case insensitive.)

\\[highlight-lines-matching-regexp] REGEXP FACE
  Highlight lines containing matches of REGEXP in current buffer with FACE.

\\[unhighlight-regexp] REGEXP
  Remove highlighting on matches of REGEXP in current buffer.

\\[hi-lock-write-interactive-patterns]
  Write active REGEXPs into buffer as comments (if possible).  They may
  be read the next time file is loaded or when the \\[hi-lock-find-patterns] command
  is issued.  The inserted regexps are in the form of font lock keywords.
  (See `font-lock-keywords'.)  They may be edited and re-loaded with \\[hi-lock-find-patterns],
  any valid `font-lock-keywords' form is acceptable.  When a file is
  loaded the patterns are read if `hi-lock-file-patterns-policy' is
  'ask and the user responds y to the prompt, or if
  `hi-lock-file-patterns-policy' is bound to a function and that
  function returns t.

\\[hi-lock-find-patterns]
  Re-read patterns stored in buffer (in the format produced by \\[hi-lock-write-interactive-patterns]).

When hi-lock is started and if the mode is not excluded or patterns
rejected, the beginning of the buffer is searched for lines of the
form:
  Hi-lock: FOO
where FOO is a list of patterns.  These are added to the font lock
keywords already present.  The patterns must start before position
\(number of characters into buffer) `hi-lock-file-patterns-range'.
Patterns will be read until
 Hi-lock: end
is found.  A mode is excluded if it's in the list `hi-lock-exclude-modes'."
  :group 'hi-lock
  :lighter (:eval (if (or hi-lock-interactive-patterns
			  hi-lock-file-patterns)
		      " Hi" ""))
  :global nil
  :keymap hi-lock-map
  (when (and (equal (buffer-name) "*scratch*")
             load-in-progress
             (not (called-interactively-p 'interactive))
             (not hi-lock-archaic-interface-message-used))
    (setq hi-lock-archaic-interface-message-used t)
    (if hi-lock-archaic-interface-deduce
        (global-hi-lock-mode hi-lock-mode)
      (warn
       "Possible archaic use of (hi-lock-mode).
Use (global-hi-lock-mode 1) in .emacs to enable hi-lock for all buffers,
use (hi-lock-mode 1) for individual buffers.  For compatibility with Emacs
versions before 22 use the following in your .emacs file:

        (if (functionp 'global-hi-lock-mode)
            (global-hi-lock-mode 1)
          (hi-lock-mode 1))
")))
  (if hi-lock-mode
      ;; Turned on.
      (progn
	(unless font-lock-mode (font-lock-mode 1))
	(define-key-after menu-bar-edit-menu [hi-lock]
	  (cons "Regexp Highlighting" hi-lock-menu))
	(hi-lock-find-patterns)
	(add-hook 'font-lock-mode-hook 'hi-lock-font-lock-hook nil t))
    ;; Turned off.
    (when (or hi-lock-interactive-patterns
	      hi-lock-file-patterns)
      (when hi-lock-interactive-patterns
	(font-lock-remove-keywords nil hi-lock-interactive-patterns)
	(setq hi-lock-interactive-patterns nil))
      (when hi-lock-file-patterns
	(font-lock-remove-keywords nil hi-lock-file-patterns)
	(setq hi-lock-file-patterns nil))
      (remove-overlays nil nil 'hi-lock-overlay t)
      (when font-lock-fontified (font-lock-fontify-buffer)))
    (define-key-after menu-bar-edit-menu [hi-lock] nil)
    (remove-hook 'font-lock-mode-hook 'hi-lock-font-lock-hook t)))

;;;###autoload
(define-globalized-minor-mode global-hi-lock-mode
  hi-lock-mode turn-on-hi-lock-if-enabled
  :group 'hi-lock)

(defun turn-on-hi-lock-if-enabled ()
  (setq hi-lock-archaic-interface-message-used t)
  (unless (memq major-mode hi-lock-exclude-modes)
    (hi-lock-mode 1)))

;;;###autoload
(defalias 'highlight-lines-matching-regexp 'hi-lock-line-face-buffer)
;;;###autoload
(defun hi-lock-line-face-buffer (regexp &optional face)
  "Set face of all lines containing a match of REGEXP to FACE.

Interactively, prompt for REGEXP then FACE.  Buffer-local history
list maintained for regexps, global history maintained for faces.
\\<minibuffer-local-map>Use \\[previous-history-element] to retrieve previous history items,
and \\[next-history-element] to retrieve default values.
\(See info node `Minibuffer History'.)"
  (interactive
   (list
    (hi-lock-regexp-okay
     (read-regexp "Regexp to highlight line" (car regexp-history)))
    (hi-lock-read-face-name)))
  (or (facep face) (setq face 'hi-yellow))
  (unless hi-lock-mode (hi-lock-mode 1))
  (hi-lock-set-pattern
   ;; The \\(?:...\\) grouping construct ensures that a leading ^, +, * or ?
   ;; or a trailing $ in REGEXP will be interpreted correctly.
   (concat "^.*\\(?:" regexp "\\).*$") face))


;;;###autoload
(defalias 'highlight-regexp 'hi-lock-face-buffer)
;;;###autoload
(defun hi-lock-face-buffer (regexp &optional face)
  "Set face of each match of REGEXP to FACE.

Interactively, prompt for REGEXP then FACE.  Buffer-local history
list maintained for regexps, global history maintained for faces.
\\<minibuffer-local-map>Use \\[previous-history-element] to retrieve previous history items,
and \\[next-history-element] to retrieve default values.
\(See info node `Minibuffer History'.)"
  (interactive
   (list
    (hi-lock-regexp-okay
     (read-regexp "Regexp to highlight" (car regexp-history)))
    (hi-lock-read-face-name)))
  (or (facep face) (setq face 'hi-yellow))
  (unless hi-lock-mode (hi-lock-mode 1))
  (hi-lock-set-pattern regexp face))

;;;###autoload
(defalias 'highlight-phrase 'hi-lock-face-phrase-buffer)
;;;###autoload
(defun hi-lock-face-phrase-buffer (regexp &optional face)
  "Set face of each match of phrase REGEXP to FACE.

Whitespace in REGEXP converted to arbitrary whitespace and initial
lower-case letters made case insensitive."
  (interactive
   (list
    (hi-lock-regexp-okay
     (hi-lock-process-phrase
      (read-regexp "Phrase to highlight" (car regexp-history))))
    (hi-lock-read-face-name)))
  (or (facep face) (setq face 'hi-yellow))
  (unless hi-lock-mode (hi-lock-mode 1))
  (hi-lock-set-pattern regexp face))

(declare-function x-popup-menu "menu.c" (position menu))

;;;###autoload
(defalias 'unhighlight-regexp 'hi-lock-unface-buffer)
;;;###autoload
(defun hi-lock-unface-buffer (regexp)
  "Remove highlighting of each match to REGEXP set by hi-lock.

Interactively, prompt for REGEXP.  Buffer-local history of inserted
regexp's maintained.  Will accept only regexps inserted by hi-lock
interactive functions.  \(See `hi-lock-interactive-patterns'.\)
\\<minibuffer-local-must-match-map>Use \\[minibuffer-complete] to complete a partially typed regexp.
\(See info node `Minibuffer History'.\)"
  (interactive
   (if (and (display-popup-menus-p)
	    (listp last-nonmenu-event)
	    use-dialog-box)
       (catch 'snafu
	 (or
	  (x-popup-menu
	   t
	   (cons
	    `keymap
	    (cons "Select Pattern to Unhighlight"
		  (mapcar (lambda (pattern)
			    (list (car pattern)
				  (format
				   "%s (%s)" (car pattern)
				   (symbol-name
				    (car
				     (cdr (car (cdr (car (cdr pattern))))))))
				  (cons nil nil)
				  (car pattern)))
			  hi-lock-interactive-patterns))))
	  ;; If the user clicks outside the menu, meaning that they
	  ;; change their mind, x-popup-menu returns nil, and
	  ;; interactive signals a wrong number of arguments error.
	  ;; To prevent that, we return an empty string, which will
	  ;; effectively disable the rest of the function.
	  (throw 'snafu '(""))))
     (let ((history-list (mapcar (lambda (p) (car p))
                                 hi-lock-interactive-patterns)))
       (unless hi-lock-interactive-patterns
         (error "No highlighting to remove"))
       (list
        (completing-read "Regexp to unhighlight: "
                         hi-lock-interactive-patterns nil t
                         (car (car hi-lock-interactive-patterns))
                         (cons 'history-list 1))))))
  (let ((keyword (assoc regexp hi-lock-interactive-patterns)))
    (when keyword
      (font-lock-remove-keywords nil (list keyword))
      (setq hi-lock-interactive-patterns
            (delq keyword hi-lock-interactive-patterns))
      (remove-overlays
       nil nil 'hi-lock-overlay-regexp (hi-lock-string-serialize regexp))
      (when font-lock-fontified (font-lock-fontify-buffer)))))

;;;###autoload
(defun hi-lock-write-interactive-patterns ()
  "Write interactively added patterns, if any, into buffer at point.

Interactively added patterns are those normally specified using
`highlight-regexp' and `highlight-lines-matching-regexp'; they can
be found in variable `hi-lock-interactive-patterns'."
  (interactive)
  (if (null hi-lock-interactive-patterns)
      (error "There are no interactive patterns"))
  (let ((beg (point)))
    (mapc
     (lambda (pattern)
       (insert (format "%s: (%s)\n"
		       hi-lock-file-patterns-prefix
		       (prin1-to-string pattern))))
     hi-lock-interactive-patterns)
    (comment-region beg (point)))
  (when (> (point) hi-lock-file-patterns-range)
    (warn "Inserted keywords not close enough to top of file")))

;; Implementation Functions

(defun hi-lock-process-phrase (phrase)
  "Convert regexp PHRASE to a regexp that matches phrases.

Blanks in PHRASE replaced by regexp that matches arbitrary whitespace
and initial lower-case letters made case insensitive."
  (let ((mod-phrase nil))
    (setq mod-phrase
          (replace-regexp-in-string
           "\\<[a-z]" (lambda (m) (format "[%s%s]" (upcase m) m)) phrase))
    (setq mod-phrase
          (replace-regexp-in-string
           "\\s-+" "[ \t\n]+" mod-phrase nil t))))

(defun hi-lock-regexp-okay (regexp)
  "Return REGEXP if it appears suitable for a font-lock pattern.

Otherwise signal an error.  A pattern that matches the null string is
not suitable."
  (if (string-match regexp "")
      (error "Regexp cannot match an empty string")
    regexp))

(defun hi-lock-read-face-name ()
  "Read face name from minibuffer with completion and history."
  (intern (completing-read
           "Highlight using face: "
           obarray 'facep t
           (cons (car hi-lock-face-defaults)
                 (let ((prefix
                        (try-completion
                         (substring (car hi-lock-face-defaults) 0 1)
                         hi-lock-face-defaults)))
                   (if (and (stringp prefix)
                            (not (equal prefix (car hi-lock-face-defaults))))
                       (length prefix) 0)))
           'face-name-history
	   (cdr hi-lock-face-defaults))))

(defun hi-lock-set-pattern (regexp face)
  "Highlight REGEXP with face FACE."
  (let ((pattern (list regexp (list 0 (list 'quote face) t))))
    (unless (member pattern hi-lock-interactive-patterns)
      (push pattern hi-lock-interactive-patterns)
      (if font-lock-fontified
	  (progn
	    (font-lock-add-keywords nil (list pattern) t)
	    (font-lock-fontify-buffer))
        (let* ((serial (hi-lock-string-serialize regexp))
               (range-min (- (point) (/ hi-lock-highlight-range 2)))
               (range-max (+ (point) (/ hi-lock-highlight-range 2)))
               (search-start
                (max (point-min)
                     (- range-min (max 0 (- range-max (point-max))))))
               (search-end
                (min (point-max)
                     (+ range-max (max 0 (- (point-min) range-min))))))
          (save-excursion
            (goto-char search-start)
            (while (re-search-forward regexp search-end t)
              (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put overlay 'hi-lock-overlay t)
                (overlay-put overlay 'hi-lock-overlay-regexp serial)
                (overlay-put overlay 'face face))
              (goto-char (match-end 0)))))))))

(defun hi-lock-set-file-patterns (patterns)
  "Replace file patterns list with PATTERNS and refontify."
  (when (or hi-lock-file-patterns patterns)
    (font-lock-remove-keywords nil hi-lock-file-patterns)
    (setq hi-lock-file-patterns patterns)
    (font-lock-add-keywords nil hi-lock-file-patterns t)
    (font-lock-fontify-buffer)))

(defun hi-lock-find-patterns ()
  "Find patterns in current buffer for hi-lock."
  (interactive)
  (unless (memq major-mode hi-lock-exclude-modes)
    (let ((all-patterns nil)
          (target-regexp (concat "\\<" hi-lock-file-patterns-prefix ":")))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (re-search-forward target-regexp
			     (+ (point) hi-lock-file-patterns-range) t)
	  (beginning-of-line)
	  (while (and (re-search-forward target-regexp (+ (point) 100) t)
		      (not (looking-at "\\s-*end")))
            (condition-case nil
                (setq all-patterns (append (read (current-buffer)) all-patterns))
              (error (message "Invalid pattern list expression at %d"
                              (line-number-at-pos)))))))
      (when (and all-patterns
                 hi-lock-mode
                 (cond
                  ((eq this-command 'hi-lock-find-patterns) t)
                  ((functionp hi-lock-file-patterns-policy)
                   (funcall hi-lock-file-patterns-policy all-patterns))
                  ((eq hi-lock-file-patterns-policy 'ask)
                   (y-or-n-p "Add patterns from this buffer to hi-lock? "))
                  (t nil)))
        (hi-lock-set-file-patterns all-patterns)
        (if (called-interactively-p 'interactive)
            (message "Hi-lock added %d patterns." (length all-patterns)))))))

(defun hi-lock-font-lock-hook ()
  "Add hi-lock patterns to font-lock's."
  (when font-lock-fontified
    (font-lock-add-keywords nil hi-lock-file-patterns t)
    (font-lock-add-keywords nil hi-lock-interactive-patterns t)))

(defvar hi-lock-string-serialize-hash
  (make-hash-table :test 'equal)
  "Hash table used to assign unique numbers to strings.")

(defvar hi-lock-string-serialize-serial 1
  "Number assigned to last new string in call to `hi-lock-string-serialize'.
A string is considered new if it had not previously been used in a call to
`hi-lock-string-serialize'.")

(defun hi-lock-string-serialize (string)
  "Return unique serial number for STRING."
  (interactive)
  (let ((val (gethash string hi-lock-string-serialize-hash)))
    (if val val
      (puthash string
               (setq hi-lock-string-serialize-serial
                     (1+ hi-lock-string-serialize-serial))
               hi-lock-string-serialize-hash)
      hi-lock-string-serialize-serial)))

(defun hi-lock-unload-function ()
  "Unload the Hi-Lock library."
  (global-hi-lock-mode -1)
  ;; continue standard unloading
  nil)

(provide 'hi-lock)

;;; hi-lock.el ends here
