;;; mule-cmds.el --- commands for multilingual environment -*-coding: iso-2022-7bit -*-

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, i18n

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

(eval-when-compile (require 'cl))	; letf

(defvar dos-codepage)
(autoload 'widget-value "wid-edit")

(defvar mac-system-coding-system)

;;; MULE related key bindings and menus.

(defvar mule-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'set-buffer-file-coding-system)
    (define-key map "r" 'revert-buffer-with-coding-system)
    (define-key map "F" 'set-file-name-coding-system)
    (define-key map "t" 'set-terminal-coding-system)
    (define-key map "k" 'set-keyboard-coding-system)
    (define-key map "p" 'set-buffer-process-coding-system)
    (define-key map "x" 'set-selection-coding-system)
    (define-key map "X" 'set-next-selection-coding-system)
    (define-key map "\C-\\" 'set-input-method)
    (define-key map "c" 'universal-coding-system-argument)
    (define-key map "l" 'set-language-environment)
    map)
  "Keymap for Mule (Multilingual environment) specific commands.")

;; Keep "C-x C-m ..." for mule specific commands.
(define-key ctl-x-map "\C-m" mule-keymap)

(defvar describe-language-environment-map
  (let ((map (make-sparse-keymap "Describe Language Environment")))
    (define-key map
      [Default] `(menu-item ,(purecopy "Default") describe-specified-language-support))
    map))

(defvar setup-language-environment-map
  (let ((map (make-sparse-keymap "Set Language Environment")))
    (define-key map
      [Default] `(menu-item ,(purecopy "Default") setup-specified-language-environment))
    map))

(defvar set-coding-system-map
  (let ((map (make-sparse-keymap "Set Coding System")))
    (define-key-after map [universal-coding-system-argument]
      `(menu-item ,(purecopy "For Next Command") universal-coding-system-argument
        :help ,(purecopy "Coding system to be used by next command")))
    (define-key-after map [separator-1] menu-bar-separator)
    (define-key-after map [set-buffer-file-coding-system]
      `(menu-item ,(purecopy "For Saving This Buffer") set-buffer-file-coding-system
        :help ,(purecopy "How to encode this buffer when saved")))
    (define-key-after map [revert-buffer-with-coding-system]
      `(menu-item ,(purecopy "For Reverting This File Now")
        revert-buffer-with-coding-system
        :enable buffer-file-name
        :help ,(purecopy "Revisit this file immediately using specified coding system")))
    (define-key-after map [set-file-name-coding-system]
      `(menu-item ,(purecopy "For File Name") set-file-name-coding-system
        :help ,(purecopy "How to decode/encode file names")))
    (define-key-after map [separator-2] menu-bar-separator)

    (define-key-after map [set-keyboard-coding-system]
      `(menu-item ,(purecopy "For Keyboard") set-keyboard-coding-system
        :help ,(purecopy "How to decode keyboard input")))
    (define-key-after map [set-terminal-coding-system]
      `(menu-item ,(purecopy "For Terminal") set-terminal-coding-system
        :enable (null (memq initial-window-system '(x w32 ns)))
        :help ,(purecopy "How to encode terminal output")))
    (define-key-after map [separator-3] menu-bar-separator)

    (define-key-after map [set-selection-coding-system]
      `(menu-item ,(purecopy "For X Selections/Clipboard") set-selection-coding-system
        :visible (display-selections-p)
        :help ,(purecopy "How to en/decode data to/from selection/clipboard")))
    (define-key-after map [set-next-selection-coding-system]
      `(menu-item ,(purecopy "For Next X Selection") set-next-selection-coding-system
        :visible (display-selections-p)
        :help ,(purecopy "How to en/decode next selection/clipboard operation")))
    (define-key-after map [set-buffer-process-coding-system]
      `(menu-item ,(purecopy "For I/O with Subprocess") set-buffer-process-coding-system
        :visible (fboundp 'start-process)
        :enable (get-buffer-process (current-buffer))
        :help ,(purecopy "How to en/decode I/O from/to subprocess connected to this buffer")))
    map))

(defvar mule-menu-keymap
  (let ((map (make-sparse-keymap "Mule (Multilingual Environment)")))
    (define-key-after map [set-language-environment]
      `(menu-item  ,(purecopy "Set Language Environment") ,setup-language-environment-map))
    (define-key-after map [separator-mule] menu-bar-separator)

    (define-key-after map [toggle-input-method]
      `(menu-item ,(purecopy "Toggle Input Method") toggle-input-method))
    (define-key-after map [set-input-method]
      `(menu-item ,(purecopy "Select Input Method...") set-input-method))
    (define-key-after map [describe-input-method]
      `(menu-item ,(purecopy "Describe Input Method")  describe-input-method))
    (define-key-after map [separator-input-method] menu-bar-separator)

    (define-key-after map [set-various-coding-system]
      `(menu-item ,(purecopy "Set Coding Systems") ,set-coding-system-map
		  :enable (default-value 'enable-multibyte-characters)))
    (define-key-after map [view-hello-file]
      `(menu-item ,(purecopy "Show Multi-lingual Text") view-hello-file
        :enable (file-readable-p
                 (expand-file-name "HELLO" data-directory))
        :help ,(purecopy "Display file which says HELLO in many languages")))
    (define-key-after map [separator-coding-system] menu-bar-separator)

    (define-key-after map [describe-language-environment]
      `(menu-item ,(purecopy "Describe Language Environment")
            ,describe-language-environment-map
            :help ,(purecopy "Show multilingual settings for a specific language")))
    (define-key-after map [describe-input-method]
      `(menu-item ,(purecopy "Describe Input Method...") describe-input-method
        :help ,(purecopy "Keyboard layout for a specific input method")))
    (define-key-after map [describe-coding-system]
      `(menu-item ,(purecopy "Describe Coding System...") describe-coding-system))
    (define-key-after map [list-character-sets]
      `(menu-item ,(purecopy "List Character Sets") list-character-sets
        :help ,(purecopy "Show table of available character sets")))
    (define-key-after map [mule-diag]
      `(menu-item ,(purecopy "Show All of Mule Status") mule-diag
        :help ,(purecopy "Display multilingual environment settings")))
    map)
  "Keymap for Mule (Multilingual environment) menu specific commands.")

;; This should be a single character key binding because users use it
;; very frequently while editing multilingual text.  Now we can use
;; only two such keys: "\C-\\" and "\C-^", but the latter is not
;; convenient because it requires shifting on most keyboards.  An
;; alternative is "\C-\]" which is now bound to `abort-recursive-edit'
;; but it won't be used that frequently.
(define-key global-map "\C-\\" 'toggle-input-method)

;; This is no good because people often type Shift-SPC
;; meaning to type SPC.  -- rms.
;; ;; Here's an alternative key binding for X users (Shift-SPACE).
;; (define-key global-map [?\S- ] 'toggle-input-method)

;;; Mule related hyperlinks.
(defconst help-xref-mule-regexp-template
  (purecopy (concat "\\(\\<\\("
		    "\\(coding system\\)\\|"
		    "\\(input method\\)\\|"
		    "\\(character set\\)\\|"
		    "\\(charset\\)"
		    "\\)\\s-+\\)?"
		    ;; Note starting with word-syntax character:
		    "`\\(\\sw\\(\\sw\\|\\s_\\)+\\)'")))

(defun coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system which differs from CODING-SYSTEM in EOL conversion.
The returned coding system converts end-of-line by EOL-TYPE
but text as the same way as CODING-SYSTEM.
EOL-TYPE should be `unix', `dos', `mac', or nil.
If EOL-TYPE is nil, the returned coding system detects
how end-of-line is formatted automatically while decoding.

EOL-TYPE can be specified by an integer 0, 1, or 2.
They means `unix', `dos', and `mac' respectively."
  (if (symbolp eol-type)
      (setq eol-type (cond ((eq eol-type 'unix) 0)
			   ((eq eol-type 'dos) 1)
			   ((eq eol-type 'mac) 2)
			   (t eol-type))))
  ;; We call `coding-system-base' before `coding-system-eol-type',
  ;; because the coding-system may not be initialized until then.
  (let* ((base (coding-system-base coding-system))
	 (orig-eol-type (coding-system-eol-type coding-system)))
    (cond ((vectorp orig-eol-type)
	   (if (not eol-type)
	       coding-system
	     (aref orig-eol-type eol-type)))
	  ((not eol-type)
	   base)
	  ((= eol-type orig-eol-type)
	   coding-system)
	  ((progn (setq orig-eol-type (coding-system-eol-type base))
		  (vectorp orig-eol-type))
	   (aref orig-eol-type eol-type)))))

(defun coding-system-change-text-conversion (coding-system coding)
  "Return a coding system which differs from CODING-SYSTEM in text conversion.
The returned coding system converts text by CODING
but end-of-line as the same way as CODING-SYSTEM.
If CODING is nil, the returned coding system detects
how text is formatted automatically while decoding."
  (let ((eol-type (coding-system-eol-type coding-system)))
    (coding-system-change-eol-conversion
     (if coding coding 'undecided)
     (if (numberp eol-type) (aref [unix dos mac] eol-type)))))

;; Canonicalize the coding system name NAME by removing some prefixes
;; and delimiter characters.  Support function of
;; coding-system-from-name.
(defun canonicalize-coding-system-name (name)
  (if (string-match "^\\(ms\\|ibm\\|windows-\\)\\([0-9]+\\)$" name)
      ;; "ms950", "ibm950", "windows-950" -> "cp950"
      (concat "cp" (match-string 2 name))
    (if (string-match "^iso[-_ ]?[0-9]" name)
	;; "iso-8859-1" -> "8859-1", "iso-2022-jp" ->"2022-jp"
	(setq name (substring name (1- (match-end 0)))))
    (let ((idx (string-match "[-_ /]" name)))
      ;; Delete "-", "_", " ", "/" but do distinguish "16-be" and "16be".
      (while idx
	(if (and (>= idx 2)
		 (eq (string-match "16-[lb]e$" name (- idx 2))
		     (- idx 2)))
	    (setq idx (string-match "[-_ /]" name (match-end 0)))
	  (setq name (concat (substring name 0 idx) (substring name (1+ idx)))
		idx (string-match "[-_ /]" name idx))))
      name)))

(defun coding-system-from-name (name)
  "Return a coding system whose name matches with NAME (string or symbol)."
  (let (sym)
    (if (stringp name) (setq sym (intern name))
      (setq sym name name (symbol-name name)))
    (if (coding-system-p sym)
	sym
      (let ((eol-type
	     (if (string-match "-\\(unix\\|dos\\|mac\\)$" name)
		 (prog1 (intern (match-string 1 name))
		   (setq name (substring name 0 (match-beginning 0)))))))
	(setq name (canonicalize-coding-system-name (downcase name)))
	(catch 'tag
	  (dolist (elt (coding-system-list))
	    (if (string= (canonicalize-coding-system-name (symbol-name elt))
			 name)
		(throw 'tag (if eol-type (coding-system-change-eol-conversion
					  elt eol-type)
			      elt)))))))))

(defun toggle-enable-multibyte-characters (&optional arg)
  "Change whether this buffer uses multibyte characters.
With ARG, use multibyte characters if the ARG is positive.

Note that this command does not convert the byte contents of
the buffer; it only changes the way those bytes are interpreted.
In general, therefore, this command *changes* the sequence of
characters that the current buffer contains.

We suggest you avoid using this command unless you know what you are
doing.  If you use it by mistake, and the buffer is now displayed
wrong, use this command again to toggle back to the right mode."
  (interactive "P")
  (let ((new-flag
	 (if (null arg) (null enable-multibyte-characters)
	   (> (prefix-numeric-value arg) 0))))
    (set-buffer-multibyte new-flag))
  (force-mode-line-update))

(defun view-hello-file ()
  "Display the HELLO file, which lists many languages and characters."
  (interactive)
  ;; We have to decode the file in any environment.
  (letf ((coding-system-for-read 'iso-2022-7bit))
    (view-file (expand-file-name "HELLO" data-directory))))

(defun universal-coding-system-argument (coding-system)
  "Execute an I/O command using the specified coding system."
  (interactive
   (let ((default (and buffer-file-coding-system
		       (not (eq (coding-system-type buffer-file-coding-system)
				'undecided))
		       buffer-file-coding-system)))
     (list (read-coding-system
	    (if default
		(format "Coding system for following command (default %s): " default)
	      "Coding system for following command: ")
	    default))))
  (let* ((keyseq (read-key-sequence
		  (format "Command to execute with %s:" coding-system)))
	 (cmd (key-binding keyseq))
	 prefix)
    ;; read-key-sequence ignores quit, so make an explicit check.
    ;; Like many places, this assumes quit == C-g, but it need not be.
    (if (equal last-input-event ?\C-g)
	(keyboard-quit))
    (when (memq cmd '(universal-argument digit-argument))
      (call-interactively cmd)

      ;; Process keys bound in `universal-argument-map'.
      (while (progn
	       (setq keyseq (read-key-sequence nil t)
		     cmd (key-binding keyseq t))
	       (not (eq cmd 'universal-argument-other-key)))
	(let ((current-prefix-arg prefix-arg)
	      ;; Have to bind `last-command-event' here so that
	      ;; `digit-argument', for instance, can compute the
	      ;; prefix arg.
	      (last-command-event (aref keyseq 0)))
	  (call-interactively cmd)))

      ;; This is the final call to `universal-argument-other-key', which
      ;; set's the final `prefix-arg.
      (let ((current-prefix-arg prefix-arg))
	(call-interactively cmd))

      ;; Read the command to execute with the given prefix arg.
      (setq prefix prefix-arg
	    keyseq (read-key-sequence nil t)
	    cmd (key-binding keyseq)))

    (let ((coding-system-for-read coding-system)
	  (coding-system-for-write coding-system)
	  (coding-system-require-warning t)
	  (current-prefix-arg prefix))
      (message "")
      (call-interactively cmd))))

(defun set-default-coding-systems (coding-system)
  "Set default value of various coding systems to CODING-SYSTEM.
This sets the following coding systems:
  o coding system of a newly created buffer
  o default coding system for subprocess I/O
This also sets the following values:
  o default value used as `file-name-coding-system' for converting file names
      if CODING-SYSTEM is ASCII-compatible
  o default value for the command `set-terminal-coding-system'
  o default value for the command `set-keyboard-coding-system'
      if CODING-SYSTEM is ASCII-compatible"
  (check-coding-system coding-system)
  (setq-default buffer-file-coding-system coding-system)
  (if (fboundp 'ucs-set-table-for-input)
      (dolist (buffer (buffer-list))
	(or (local-variable-p 'buffer-file-coding-system buffer)
	    (ucs-set-table-for-input buffer))))

  (if (eq system-type 'darwin)
      ;; The file-name coding system on Darwin systems is always utf-8.
      (setq default-file-name-coding-system 'utf-8)
    (if (and (default-value 'enable-multibyte-characters)
	     (or (not coding-system)
		 (coding-system-get coding-system 'ascii-compatible-p)))
	(setq default-file-name-coding-system coding-system)))
  (setq default-terminal-coding-system coding-system)
  ;; Prevent default-terminal-coding-system from converting ^M to ^J.
  (setq default-keyboard-coding-system
	(coding-system-change-eol-conversion coding-system 'unix))
  ;; Preserve eol-type from existing default-process-coding-systems.
  ;; On non-unix-like systems in particular, these may have been set
  ;; carefully by the user, or by the startup code, to deal with the
  ;; users shell appropriately, so should not be altered by changing
  ;; language environment.
  (let ((output-coding
	 (coding-system-change-text-conversion
	  (car default-process-coding-system) coding-system))
	(input-coding
	 (coding-system-change-text-conversion
	  (cdr default-process-coding-system) coding-system)))
    (setq default-process-coding-system
	  (cons output-coding input-coding))))

(defun prefer-coding-system (coding-system)
  "Add CODING-SYSTEM at the front of the priority list for automatic detection.
This also sets the following coding systems:
  o coding system of a newly created buffer
  o default coding system for subprocess I/O
This also sets the following values:
  o default value used as `file-name-coding-system' for converting file names
  o default value for the command `set-terminal-coding-system'
  o default value for the command `set-keyboard-coding-system'

If CODING-SYSTEM specifies a certain type of EOL conversion, the coding
systems set by this function will use that type of EOL conversion.

A coding system that requires automatic detection of text+encoding
\(e.g. undecided, unix) can't be preferred.

To prefer, for instance, utf-8, say the following:

  \(prefer-coding-system 'utf-8)"
  (interactive "zPrefer coding system: ")
  (if (not (and coding-system (coding-system-p coding-system)))
      (error "Invalid coding system `%s'" coding-system))
  (if (memq (coding-system-type coding-system) '(raw-text undecided))
      (error "Can't prefer the coding system `%s'" coding-system))
  (let ((base (coding-system-base coding-system))
	(eol-type (coding-system-eol-type coding-system)))
    (set-coding-system-priority base)
    (and (called-interactively-p 'interactive)
	 (or (eq base coding-system)
	     (message "Highest priority is set to %s (base of %s)"
		      base coding-system)))
    ;; If they asked for specific EOL conversion, honor that.
    (if (memq eol-type '(0 1 2))
	(setq base
	      (coding-system-change-eol-conversion base eol-type)))
    (set-default-coding-systems base)
    (if (called-interactively-p 'interactive)
	(or (eq base default-file-name-coding-system)
	    (message "The default value of `file-name-coding-system' was not changed because the specified coding system is not suitable for file names.")))))

(defvar sort-coding-systems-predicate nil
  "If non-nil, a predicate function to sort coding systems.

It is called with two coding systems, and should return t if the first
one is \"less\" than the second.

The function `sort-coding-systems' use it.")

(defun sort-coding-systems (codings)
  "Sort coding system list CODINGS by a priority of each coding system.
Return the sorted list.  CODINGS is modified by side effects.

If a coding system is most preferred, it has the highest priority.
Otherwise, coding systems that correspond to MIME charsets have
higher priorities.  Among them, a coding system included in the
`coding-system' key of the current language environment has higher
priority.  See also the documentation of `language-info-alist'.

If the variable `sort-coding-systems-predicate' (which see) is
non-nil, it is used to sort CODINGS instead."
  (if sort-coding-systems-predicate
      (sort codings sort-coding-systems-predicate)
    (let* ((from-priority (coding-system-priority-list))
	   (most-preferred (car from-priority))
	   (lang-preferred (get-language-info current-language-environment
					      'coding-system))
	   (func (function
		  (lambda (x)
		    (let ((base (coding-system-base x)))
		      ;; We calculate the priority number 0..255 by
		      ;; using the 8 bits PMMLCEII as this:
		      ;; P: 1 if most preferred.
		      ;; MM: greater than 0 if mime-charset.
		      ;; L: 1 if one of the current lang. env.'s codings.
		      ;; C: 1 if one of codings listed in the category list.
		      ;; E: 1 if not XXX-with-esc
		      ;; II: if iso-2022 based, 0..3, else 1.
		      (logior
		       (lsh (if (eq base most-preferred) 1 0) 7)
		       (lsh
			(let ((mime (coding-system-get base :mime-charset)))
			   ;; Prefer coding systems corresponding to a
			   ;; MIME charset.
			   (if mime
			       ;; Lower utf-16 priority so that we
			       ;; normally prefer utf-8 to it, and put
			       ;; x-ctext below that.
			       (cond ((string-match-p "utf-16"
						      (symbol-name mime))
				      2)
				     ((string-match-p "^x-" (symbol-name mime))
				      1)
				     (t 3))
			     0))
			5)
		       (lsh (if (memq base lang-preferred) 1 0) 4)
		       (lsh (if (memq base from-priority) 1 0) 3)
		       (lsh (if (string-match-p "-with-esc\\'"
						(symbol-name base))
				0 1) 2)
		       (if (eq (coding-system-type base) 'iso-2022)
			   (let ((category (coding-system-category base)))
			     ;; For ISO based coding systems, prefer
			     ;; one that doesn't use designation nor
			     ;; locking/single shifting.
			       (cond
				((or (eq category 'coding-category-iso-8-1)
				     (eq category 'coding-category-iso-8-2))
				 2)
				((or (eq category 'coding-category-iso-7-tight)
				     (eq category 'coding-category-iso-7))
				 1)
				(t
				 0)))
			   1)
			 ))))))
      (sort codings (function (lambda (x y)
				(> (funcall func x) (funcall func y))))))))

(defun find-coding-systems-region (from to)
  "Return a list of proper coding systems to encode a text between FROM and TO.

If FROM is a string, find coding systems in that instead of the buffer.
All coding systems in the list can safely encode any multibyte characters
in the text.

If the text contains no multibyte characters, return a list of a single
element `undecided'."
  (let ((codings (find-coding-systems-region-internal from to)))
    (if (eq codings t)
	;; The text contains only ASCII characters.  Any coding
	;; systems are safe.
	'(undecided)
      ;; We need copy-sequence because sorting will alter the argument.
      (sort-coding-systems (copy-sequence codings)))))

(defun find-coding-systems-string (string)
  "Return a list of proper coding systems to encode STRING.
All coding systems in the list can safely encode any multibyte characters
in STRING.

If STRING contains no multibyte characters, return a list of a single
element `undecided'."
  (find-coding-systems-region string nil))

(defun find-coding-systems-for-charsets (charsets)
  "Return a list of proper coding systems to encode characters of CHARSETS.
CHARSETS is a list of character sets.

This only finds coding systems of type `charset', whose
`:charset-list' property includes all of CHARSETS (plus `ascii' for
ASCII-compatible coding systems).  It was used in older versions of
Emacs, but is unlikely to be what you really want now."
  ;; Deal with aliases.
  (setq charsets (mapcar (lambda (c)
			   (get-charset-property c :name))
			 charsets))
  (cond ((or (null charsets)
	     (and (= (length charsets) 1)
		  (eq 'ascii (car charsets))))
	 '(undecided))
	((or (memq 'eight-bit-control charsets)
	     (memq 'eight-bit-graphic charsets))
	 '(raw-text utf-8-emacs))
	(t
	 (let (codings)
	   (dolist (cs (coding-system-list t))
	     (let ((cs-charsets (and (eq (coding-system-type cs) 'charset)
				     (coding-system-charset-list cs)))
		   (charsets charsets))
	       (if (coding-system-get cs :ascii-compatible-p)
		   (add-to-list 'cs-charsets 'ascii))
	       (if (catch 'ok
		     (when cs-charsets
		       (while charsets
			 (unless (memq (pop charsets) cs-charsets)
			   (throw 'ok nil)))
		       t))
		   (push cs codings))))
	   (nreverse codings)))))

(defun find-multibyte-characters (from to &optional maxcount excludes)
  "Find multibyte characters in the region specified by FROM and TO.
If FROM is a string, find multibyte characters in the string.
The return value is an alist of the following format:
  ((CHARSET COUNT CHAR ...) ...)
where
  CHARSET is a character set,
  COUNT is a number of characters,
  CHARs are the characters found from the character set.
Optional 3rd arg MAXCOUNT limits how many CHARs are put in the above list.
Optional 4th arg EXCLUDES is a list of character sets to be ignored."
  (let ((chars nil)
	charset char)
    (if (stringp from)
	(if (multibyte-string-p from)
	    (let ((idx 0))
	      (while (setq idx (string-match-p "[^\000-\177]" from idx))
		(setq char (aref from idx)
		      charset (char-charset char))
		(unless (memq charset excludes)
		  (let ((slot (assq charset chars)))
		    (if slot
			(if (not (memq char (nthcdr 2 slot)))
			    (let ((count (nth 1 slot)))
			      (setcar (cdr slot) (1+ count))
			      (if (or (not maxcount) (< count maxcount))
				  (nconc slot (list char)))))
		      (setq chars (cons (list charset 1 char) chars)))))
		(setq idx (1+ idx)))))
      (if enable-multibyte-characters
	  (save-excursion
	    (goto-char from)
	    (while (re-search-forward "[^\000-\177]" to t)
	      (setq char (preceding-char)
		    charset (char-charset char))
	      (unless (memq charset excludes)
		(let ((slot (assq charset chars)))
		  (if slot
		      (if (not (member char (nthcdr 2 slot)))
			  (let ((count (nth 1 slot)))
			    (setcar (cdr slot) (1+ count))
			    (if (or (not maxcount) (< count maxcount))
				(nconc slot (list char)))))
		    (setq chars (cons (list charset 1 char) chars)))))))))
    (nreverse chars)))

(defun search-unencodable-char (coding-system)
  "Search forward from point for a character that is not encodable.
It asks which coding system to check.
If such a character is found, set point after that character.
Otherwise, don't move point.

When called from a program, the value is the position of the unencodable
character found, or nil if all characters are encodable."
  (interactive
   (list (let ((default (or buffer-file-coding-system 'us-ascii)))
	   (read-coding-system
	    (format "Coding-system (default %s): " default)
	    default))))
  (let ((pos (unencodable-char-position (point) (point-max) coding-system)))
    (if pos
	(goto-char (1+ pos))
      (message "All following characters are encodable by %s" coding-system))
    pos))

(defvar last-coding-system-specified nil
  "Most recent coding system explicitly specified by the user when asked.
This variable is set whenever Emacs asks the user which coding system
to use in order to write a file.  If you set it to nil explicitly,
then call `write-region', then afterward this variable will be non-nil
only if the user was explicitly asked and specified a coding system.")

(defvar select-safe-coding-system-accept-default-p nil
  "If non-nil, a function to control the behavior of coding system selection.
The meaning is the same as the argument ACCEPT-DEFAULT-P of the
function `select-safe-coding-system' (which see).  This variable
overrides that argument.")

(defun select-safe-coding-system-interactively (from to codings unsafe
						&optional rejected default)
  "Select interactively a coding system for the region FROM ... TO.
FROM can be a string, as in `write-region'.
CODINGS is the list of base coding systems known to be safe for this region,
  typically obtained with `find-coding-systems-region'.
UNSAFE is a list of coding systems known to be unsafe for this region.
REJECTED is a list of coding systems which were safe but for some reason
  were not recommended in the particular context.
DEFAULT is the coding system to use by default in the query."
  ;; At first, if some defaults are unsafe, record at most 11
  ;; problematic characters and their positions for them by turning
  ;;	(CODING ...)
  ;; into
  ;;	((CODING (POS . CHAR) (POS . CHAR) ...) ...)
  (if unsafe
      (setq unsafe
	    (mapcar #'(lambda (coding)
			(cons coding
			      (if (stringp from)
				  (mapcar #'(lambda (pos)
					      (cons pos (aref from pos)))
					  (unencodable-char-position
					   0 (length from) coding
					   11 from))
				(mapcar #'(lambda (pos)
					    (cons pos (char-after pos)))
					(unencodable-char-position
					 from to coding 11)))))
		    unsafe)))

  ;; Change each safe coding system to the corresponding
  ;; mime-charset name if it is also a coding system.  Such a name
  ;; is more friendly to users.
  (let ((l codings)
	mime-charset)
    (while l
      (setq mime-charset (coding-system-get (car l) :mime-charset))
      (if (and mime-charset (coding-system-p mime-charset)
	       (coding-system-equal (car l) mime-charset))
	  (setcar l mime-charset))
      (setq l (cdr l))))

  ;; Don't offer variations with locking shift, which you
  ;; basically never want.
  (let (l)
    (dolist (elt codings (setq codings (nreverse l)))
      (unless (or (eq 'coding-category-iso-7-else
		      (coding-system-category elt))
		  (eq 'coding-category-iso-8-else
		      (coding-system-category elt)))
	(push elt l))))

  ;; Remove raw-text, emacs-mule and no-conversion unless nothing
  ;; else is available.
  (setq codings
	(or (delq 'raw-text
		  (delq 'emacs-mule
			(delq 'no-conversion codings)))
	    '(raw-text emacs-mule no-conversion)))

  (let ((window-configuration (current-window-configuration))
	(bufname (buffer-name))
	coding-system)
    (save-excursion
      ;; If some defaults are unsafe, make sure the offending
      ;; buffer is displayed.
      (when (and unsafe (not (stringp from)))
	(pop-to-buffer bufname)
	(goto-char (apply 'min (mapcar #'(lambda (x) (car (cadr x)))
				       unsafe))))
      ;; Then ask users to select one from CODINGS while showing
      ;; the reason why none of the defaults are not used.
      (with-output-to-temp-buffer "*Warning*"
	(with-current-buffer standard-output
	  (if (and (null rejected) (null unsafe))
	      (insert "No default coding systems to try for "
		      (if (stringp from)
			  (format "string \"%s\"." from)
			(format "buffer `%s'." bufname)))
	    (insert
	     "These default coding systems were tried to encode"
	     (if (stringp from)
		 (concat " \"" (if (> (length from) 10)
				   (concat (substring from 0 10) "...\"")
				 (concat from "\"")))
	       (format " text\nin the buffer `%s'" bufname))
	     ":\n")
	    (let ((pos (point))
		  (fill-prefix "  "))
	      (dolist (x (append rejected unsafe))
		(princ "  ") (princ x))
	      (insert "\n")
	      (fill-region-as-paragraph pos (point)))
	    (when rejected
	      (insert "These safely encode the text in the buffer,
but are not recommended for encoding text in this context,
e.g., for sending an email message.\n ")
	      (dolist (x rejected)
		(princ " ") (princ x))
	      (insert "\n"))
	    (when unsafe
	      (insert (if rejected "The other coding systems"
			"However, each of them")
		      " encountered characters it couldn't encode:\n")
	      (dolist (coding unsafe)
		(insert (format "  %s cannot encode these:" (car coding)))
		(let ((i 0)
		      (func1
		       #'(lambda (bufname pos)
			   (when (buffer-live-p (get-buffer bufname))
			     (pop-to-buffer bufname)
			     (goto-char pos))))
		      (func2
		       #'(lambda (bufname pos coding)
			   (when (buffer-live-p (get-buffer bufname))
			     (pop-to-buffer bufname)
			     (if (< (point) pos)
				 (goto-char pos)
			       (forward-char 1)
			       (search-unencodable-char coding)
			       (forward-char -1))))))
		  (dolist (elt (cdr coding))
		    (insert " ")
		    (if (stringp from)
			(insert (if (< i 10) (cdr elt) "..."))
		      (if (< i 10)
			  (insert-text-button
			   (cdr elt)
			   :type 'help-xref
			   'face 'link
			   'help-echo
			   "mouse-2, RET: jump to this character"
			   'help-function func1
			   'help-args (list bufname (car elt)))
			(insert-text-button
			 "..."
			 :type 'help-xref
			 'face 'link
			 'help-echo
			 "mouse-2, RET: next unencodable character"
			 'help-function func2
			 'help-args (list bufname (car elt)
					  (car coding)))))
		    (setq i (1+ i))))
		(insert "\n"))
	      (insert (substitute-command-keys "\

Click on a character (or switch to this window by `\\[other-window]'\n\
and select the characters by RET) to jump to the place it appears,\n\
where `\\[universal-argument] \\[what-cursor-position]' will give information about it.\n"))))
	  (insert (substitute-command-keys "\nSelect \
one of the safe coding systems listed below,\n\
or cancel the writing with \\[keyboard-quit] and edit the buffer\n\
   to remove or modify the problematic characters,\n\
or specify any other coding system (and risk losing\n\
   the problematic characters).\n\n"))
	  (let ((pos (point))
		(fill-prefix "  "))
	    (dolist (x codings)
	      (princ "  ") (princ x))
	    (insert "\n")
	    (fill-region-as-paragraph pos (point)))))

      ;; Read a coding system.
      (setq coding-system
	    (read-coding-system
	     (format "Select coding system (default %s): " default)
	     default))
      (setq last-coding-system-specified coding-system))

    (kill-buffer "*Warning*")
    (set-window-configuration window-configuration)
    coding-system))

(defun select-safe-coding-system (from to &optional default-coding-system
				       accept-default-p file)
  "Ask a user to select a safe coding system from candidates.
The candidates of coding systems which can safely encode a text
between FROM and TO are shown in a popup window.  Among them, the most
proper one is suggested as the default.

The list of `buffer-file-coding-system' of the current buffer, the
default `buffer-file-coding-system', and the most preferred coding
system (if it corresponds to a MIME charset) is treated as the
default coding system list.  Among them, the first one that safely
encodes the text is normally selected silently and returned without
any user interaction.  See also the command `prefer-coding-system'.

However, the user is queried if the chosen coding system is
inconsistent with what would be selected by `find-auto-coding' from
coding cookies &c. if the contents of the region were read from a
file.  (That could lead to data corruption in a file subsequently
re-visited and edited.)

Optional 3rd arg DEFAULT-CODING-SYSTEM specifies a coding system or a
list of coding systems to be prepended to the default coding system
list.  However, if DEFAULT-CODING-SYSTEM is a list and the first
element is t, the cdr part is used as the default coding system list,
i.e. current `buffer-file-coding-system', default `buffer-file-coding-system',
and the most preferred coding system are not used.

Optional 4th arg ACCEPT-DEFAULT-P, if non-nil, is a function to
determine the acceptability of the silently selected coding system.
It is called with that coding system, and should return nil if it
should not be silently selected and thus user interaction is required.

Optional 5th arg FILE is the file name to use for this purpose.
That is different from `buffer-file-name' when handling `write-region'
\(for example).

The variable `select-safe-coding-system-accept-default-p', if non-nil,
overrides ACCEPT-DEFAULT-P.

Kludgy feature: if FROM is a string, the string is the target text,
and TO is ignored."
  (if (not (listp default-coding-system))
      (setq default-coding-system (list default-coding-system)))

  (let ((no-other-defaults nil)
	auto-cs)
    (unless (or (stringp from) find-file-literally)
      ;; Find an auto-coding that is specified for the current
      ;; buffer and file from the region FROM and TO.
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char from)
	  (setq auto-cs (find-auto-coding (or file buffer-file-name "")
					  (- to from)))
	  (if auto-cs
	      (if (coding-system-p (car auto-cs))
		  (setq auto-cs (car auto-cs))
		(display-warning
		 'mule
		 (format "\
Invalid coding system `%s' is specified
for the current buffer/file by the %s.
It is highly recommended to fix it before writing to a file."
			 (car auto-cs)
			 (if (eq (cdr auto-cs) :coding) ":coding tag"
			   (format "variable `%s'" (cdr auto-cs))))
		 :warning)
		(or (yes-or-no-p "Really proceed with writing? ")
		    (error "Save aborted"))
		(setq auto-cs nil))))))

    (if (eq (car default-coding-system) t)
	(setq no-other-defaults t
	      default-coding-system (cdr default-coding-system)))

    ;; Change elements of the list to (coding . base-coding).
    (setq default-coding-system
	  (mapcar (function (lambda (x) (cons x (coding-system-base x))))
		  default-coding-system))

    (if (and auto-cs (not no-other-defaults))
	;; If the file has a coding cookie, use it regardless of any
	;; other setting.
	(let ((base (coding-system-base auto-cs)))
	  (unless (memq base '(nil undecided))
            (setq default-coding-system (list (cons auto-cs base)))
            (setq no-other-defaults t))))

    (unless no-other-defaults
      ;; If buffer-file-coding-system is not nil nor undecided, append it
      ;; to the defaults.
      (if buffer-file-coding-system
	  (let ((base (coding-system-base buffer-file-coding-system)))
	    (or (eq base 'undecided)
		(rassq base default-coding-system)
		(setq default-coding-system
		      (append default-coding-system
			      (list (cons buffer-file-coding-system base)))))))

      (unless (and buffer-file-coding-system-explicit
		   (cdr buffer-file-coding-system-explicit))
	;; If default buffer-file-coding-system is not nil nor undecided,
	;; append it to the defaults.
	(when (default-value 'buffer-file-coding-system)
          (let ((base (coding-system-base
                       (default-value 'buffer-file-coding-system))))
            (or (eq base 'undecided)
                (rassq base default-coding-system)
                (setq default-coding-system
                      (append default-coding-system
                              (list (cons (default-value
                                            'buffer-file-coding-system)
                                          base)))))))

	;; If the most preferred coding system has the property mime-charset,
	;; append it to the defaults.
	(let ((preferred (coding-system-priority-list t))
	      base)
	  (and (coding-system-p preferred)
	       (setq base (coding-system-base preferred))
	       (coding-system-get preferred :mime-charset)
	       (not (rassq base default-coding-system))
	       (setq default-coding-system
		     (append default-coding-system
			     (list (cons preferred base))))))))

    (if select-safe-coding-system-accept-default-p
	(setq accept-default-p select-safe-coding-system-accept-default-p))

    ;; Decide the eol-type from the top of the default codings,
    ;; current buffer-file-coding-system, or default buffer-file-coding-system.
    (if default-coding-system
	(let ((default-eol-type (coding-system-eol-type
				 (caar default-coding-system))))
	  (if (and (vectorp default-eol-type) buffer-file-coding-system)
	      (setq default-eol-type (coding-system-eol-type
				      buffer-file-coding-system)))
	  (if (and (vectorp default-eol-type)
                   (default-value 'buffer-file-coding-system))
	      (setq default-eol-type
                    (coding-system-eol-type
                     (default-value 'buffer-file-coding-system))))
	  (if (and default-eol-type (not (vectorp default-eol-type)))
	      (dolist (elt default-coding-system)
		(setcar elt (coding-system-change-eol-conversion
			     (car elt) default-eol-type))))))

    (let ((codings (find-coding-systems-region from to))
	  (coding-system nil)
	  (tick (if (not (stringp from)) (buffer-chars-modified-tick)))
	  safe rejected unsafe)
      (if (eq (car codings) 'undecided)
	  ;; Any coding system is ok.
	  (setq coding-system (caar default-coding-system))
	;; Reverse the list so that elements are accumulated in safe,
	;; rejected, and unsafe in the correct order.
	(setq default-coding-system (nreverse default-coding-system))

	;; Classify the defaults into safe, rejected, and unsafe.
	(dolist (elt default-coding-system)
	  (if (or (eq (car codings) 'undecided)
		  (memq (cdr elt) codings))
	      (if (and (functionp accept-default-p)
		       (not (funcall accept-default-p (cdr elt))))
		  (push (car elt) rejected)
		(push (car elt) safe))
	    (push (car elt) unsafe)))
	(if safe
	    (setq coding-system (car safe))))

      ;; If all the defaults failed, ask a user.
      (when (not coding-system)
	(setq coding-system (select-safe-coding-system-interactively
			     from to codings unsafe rejected (car codings))))

      ;; Check we're not inconsistent with what `coding:' spec &c would
      ;; give when file is re-read.
      ;; But don't do this if we explicitly ignored the cookie
      ;; by using `find-file-literally'.
      (when (and auto-cs
		 (not (and
		       coding-system
		       (memq (coding-system-type coding-system) '(0 5)))))
	;; Merge coding-system and auto-cs as far as possible.
	(if (not coding-system)
	    (setq coding-system auto-cs)
	  (if (not auto-cs)
	      (setq auto-cs coding-system)
	    (let ((eol-type-1 (coding-system-eol-type coding-system))
		  (eol-type-2 (coding-system-eol-type auto-cs)))
	    (if (eq (coding-system-base coding-system) 'undecided)
		(setq coding-system (coding-system-change-text-conversion
				     coding-system auto-cs))
	      (if (eq (coding-system-base auto-cs) 'undecided)
		  (setq auto-cs (coding-system-change-text-conversion
				 auto-cs coding-system))))
	    (if (vectorp eol-type-1)
		(or (vectorp eol-type-2)
		    (setq coding-system (coding-system-change-eol-conversion
					 coding-system eol-type-2)))
	      (if (vectorp eol-type-2)
		  (setq auto-cs (coding-system-change-eol-conversion
				 auto-cs eol-type-1)))))))

	(if (and auto-cs
		 ;; Don't barf if writing a compressed file, say.
		 ;; This check perhaps isn't ideal, but is probably
		 ;; the best thing to do.
		 (not (auto-coding-alist-lookup (or file buffer-file-name "")))
		 (not (coding-system-equal coding-system auto-cs)))
	    (unless (yes-or-no-p
		     (format "Selected encoding %s disagrees with \
%s specified by file contents.  Really save (else edit coding cookies \
and try again)? " coding-system auto-cs))
	      (error "Save aborted"))))
      (when (and tick (/= tick (buffer-chars-modified-tick)))
	(error "Canceled because the buffer was modified"))
      coding-system)))

(setq select-safe-coding-system-function 'select-safe-coding-system)

(defun select-message-coding-system ()
  "Return a coding system to encode the outgoing message of the current buffer.
It at first tries the first coding system found in these variables
in this order:
  (1) local value of `buffer-file-coding-system'
  (2) value of `sendmail-coding-system'
  (3) value of `default-sendmail-coding-system'
  (4) default value of `buffer-file-coding-system'
If the found coding system can't encode the current buffer,
or none of them are bound to a coding system,
it asks the user to select a proper coding system."
  (let ((coding (or (and (local-variable-p 'buffer-file-coding-system)
			  buffer-file-coding-system)
		     sendmail-coding-system
		     default-sendmail-coding-system
		     (default-value 'buffer-file-coding-system))))
    (if (eq coding 'no-conversion)
	;; We should never use no-conversion for outgoing mail.
	(setq coding nil))
    (if (fboundp select-safe-coding-system-function)
	(funcall select-safe-coding-system-function
		 (point-min) (point-max) coding
		 (function (lambda (x) (coding-system-get x :mime-charset))))
      coding)))

;;; Language support stuff.

(defvar language-info-alist nil
  "Alist of language environment definitions.
Each element looks like:
	(LANGUAGE-NAME . ((KEY . INFO) ...))
where LANGUAGE-NAME is a string, the name of the language environment,
KEY is a symbol denoting the kind of information, and
INFO is the data associated with KEY.
Meaningful values for KEY include

  documentation      value is documentation of what this language environment
			is meant for, and how to use it.
  charset	     value is a list of the character sets mainly used
			by this language environment.
  sample-text	     value is an expression which is evalled to generate
                        a line of text written using characters appropriate
                        for this language environment.
  setup-function     value is a function to call to switch to this
			language environment.
  exit-function      value is a function to call to leave this
		        language environment.
  coding-system      value is a list of coding systems that are good for
			saving text written in this language environment.
			This list serves as suggestions to the user;
			in effect, as a kind of documentation.
  coding-priority    value is a list of coding systems for this language
			environment, in order of decreasing priority.
			This is used to set up the coding system priority
			list when you switch to this language environment.
  nonascii-translation
		     value is a charset of dimension one to use for
			converting a unibyte character to multibyte
			and vice versa.
  input-method       value is a default input method for this language
			environment.
  features           value is a list of features requested in this
			language environment.
  ctext-non-standard-encodings
		     value is a list of non-standard encoding names used
			in extended segments of CTEXT.  See the variable
			`ctext-non-standard-encodings' for more detail.

The following key takes effect only when multibyte characters are
globally disabled, i.e. the default value of `enable-multibyte-characters'
is nil (which is an obsolete and deprecated use):

  unibyte-display    value is a coding system to encode characters for
			the terminal.  Characters in the range of 160 to
			255 display not as octal escapes, but as non-ASCII
			characters in this language environment.")

(defun get-language-info (lang-env key)
  "Return information listed under KEY for language environment LANG-ENV.
KEY is a symbol denoting the kind of information.
For a list of useful values for KEY and their meanings,
see `language-info-alist'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((lang-slot (assoc-string lang-env language-info-alist t)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun set-language-info (lang-env key info)
  "Modify part of the definition of language environment LANG-ENV.
Specifically, this stores the information INFO under KEY
in the definition of this language environment.
KEY is a symbol denoting the kind of information.
INFO is the value for that information.

For a list of useful values for KEY and their meanings,
see `language-info-alist'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (set-language-info-internal lang-env key info)
  (if (equal lang-env current-language-environment)
      (cond ((eq key 'coding-priority)
	     (set-language-environment-coding-systems lang-env)
	     (set-language-environment-charset lang-env))
	    ((eq key 'input-method)
	     (set-language-environment-input-method lang-env))
	    ((eq key 'nonascii-translation)
	     (set-language-environment-nonascii-translation lang-env))
	    ((eq key 'charset)
	     (set-language-environment-charset lang-env))
	    ((and (not (default-value 'enable-multibyte-characters))
		  (or (eq key 'unibyte-syntax) (eq key 'unibyte-display)))
	     (set-language-environment-unibyte lang-env)))))

(defun set-language-info-internal (lang-env key info)
  "Internal use only.
Arguments are the same as `set-language-info'."
  (let (lang-slot key-slot)
    (setq lang-slot (assoc lang-env language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list lang-env)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq key-slot (assq key lang-slot))
    (if (null key-slot)			; If no slot for the key, add it.
	(progn
	  (setq key-slot (list key))
	  (setcdr lang-slot (cons key-slot (cdr lang-slot)))))
    (setcdr key-slot (purecopy info))
    ;; Update the custom-type of `current-language-environment'.
    (put 'current-language-environment 'custom-type
	 (cons 'choice (mapcar
			(lambda (lang)
			  (list 'const lang))
			(sort (mapcar 'car language-info-alist) 'string<))))))

(defun set-language-info-alist (lang-env alist &optional parents)
  "Store ALIST as the definition of language environment LANG-ENV.
ALIST is an alist of KEY and INFO values.  See the documentation of
`language-info-alist' for the meanings of KEY and INFO.

Optional arg PARENTS is a list of parent menu names; it specifies
where to put this language environment in the
Describe Language Environment and Set Language Environment menus.
For example, (\"European\") means to put this language environment
in the European submenu in each of those two menus."
  (cond ((symbolp lang-env)
	 (setq lang-env (symbol-name lang-env)))
	((stringp lang-env)
	 (setq lang-env (purecopy lang-env))))
  (let ((describe-map describe-language-environment-map)
	(setup-map setup-language-environment-map))
    (if parents
	(let ((l parents)
	      map parent-symbol parent prompt)
	  (while l
	    (if (symbolp (setq parent-symbol (car l)))
		(setq parent (symbol-name parent))
	      (setq parent parent-symbol parent-symbol (intern parent)))
	    (setq map (lookup-key describe-map (vector parent-symbol)))
	    ;; This prompt string is for define-prefix-command, so
	    ;; that the map it creates will be suitable for a menu.
	    (or map (setq prompt (format "%s Environment" parent)))
	    (if (not map)
		(progn
		  (setq map (intern (format "describe-%s-environment-map"
					    (downcase parent))))
		  (define-prefix-command map nil prompt)
		  (define-key-after describe-map (vector parent-symbol)
		    (cons parent map))))
	    (setq describe-map (symbol-value map))
	    (setq map (lookup-key setup-map (vector parent-symbol)))
	    (if (not map)
		(progn
		  (setq map (intern (format "setup-%s-environment-map"
					    (downcase parent))))
		  (define-prefix-command map nil prompt)
		  (define-key-after setup-map (vector parent-symbol)
		    (cons parent map))))
	    (setq setup-map (symbol-value map))
	    (setq l (cdr l)))))

    ;; Set up menu items for this language env.
    (let ((doc (assq 'documentation alist)))
      (when doc
	(define-key-after describe-map (vector (intern lang-env))
	  (cons lang-env 'describe-specified-language-support))))
    (define-key-after setup-map (vector (intern lang-env))
      (cons lang-env 'setup-specified-language-environment))

    (dolist (elt alist)
      (set-language-info-internal lang-env (car elt) (cdr elt)))

    (if (equal lang-env current-language-environment)
	(set-language-environment lang-env))))

(defun read-language-name (key prompt &optional default)
  "Read a language environment name which has information for KEY.
If KEY is nil, read any language environment.
Prompt with PROMPT.  DEFAULT is the default choice of language environment.
This returns a language environment name as a string."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(and key
				     (function (lambda (elm) (and (listp elm) (assq key elm)))))
				t nil nil default)))
    (if (and (> (length name) 0)
	     (or (not key)
		 (get-language-info name key)))
	name)))

;;; Multilingual input methods.
(defgroup leim nil
  "LEIM: Libraries of Emacs Input Methods."
  :group 'mule)

(defconst leim-list-file-name "leim-list.el"
  "Name of LEIM list file.
This file contains a list of libraries of Emacs input methods (LEIM)
in the format of Lisp expression for registering each input method.
Emacs loads this file at startup time.")

(defconst leim-list-header (format
";;; %s -- list of LEIM (Library of Emacs Input Method) -*-coding: utf-8;-*-
;;
;; This file is automatically generated.
;;
;; This file contains a list of LEIM (Library of Emacs Input Method)
;; methods in the same directory as this file.  Loading this file
;; registers all the input methods in Emacs.
;;
;; Each entry has the form:
;;   (register-input-method
;;    INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC
;;    TITLE DESCRIPTION
;;    ARG ...)
;; See the function `register-input-method' for the meanings of the arguments.
;;
;; If this directory is included in `load-path', Emacs automatically
;; loads this file at startup time.

"
				 leim-list-file-name)
  "Header to be inserted in LEIM list file.")

(defconst leim-list-entry-regexp "^(register-input-method"
  "Regexp matching head of each entry in LEIM list file.
See also the variable `leim-list-header'.")

(defvar update-leim-list-functions
  '(quail-update-leim-list-file)
  "List of functions to call to update LEIM list file.
Each function is called with one arg, LEIM directory name.")

(defun update-leim-list-file (&rest dirs)
  "Update LEIM list file in directories DIRS."
  (dolist (function update-leim-list-functions)
    (apply function dirs)))

(defvar current-input-method nil
  "The current input method for multilingual text.
If nil, that means no input method is activated now.")
(make-variable-buffer-local 'current-input-method)
(put 'current-input-method 'permanent-local t)

(defvar current-input-method-title nil
  "Title string of the current input method shown in mode line.")
(make-variable-buffer-local 'current-input-method-title)
(put 'current-input-method-title 'permanent-local t)

(define-widget 'mule-input-method-string 'string
  "String widget with completion for input method."
  :completions
  (lambda (string pred action)
    (let ((completion-ignore-case t))
      (complete-with-action action input-method-alist string pred)))
  :prompt-history 'input-method-history)

(defcustom default-input-method nil
  "Default input method for multilingual text (a string).
This is the input method activated automatically by the command
`toggle-input-method' (\\[toggle-input-method])."
  :link  '(custom-manual "(emacs)Input Methods")
  :group 'mule
  :type `(choice (const nil)
                 mule-input-method-string)
  :set-after '(current-language-environment))

(put 'input-method-function 'permanent-local t)

(defvar input-method-history nil
  "History list of input methods read from the minibuffer.

Maximum length of the history list is determined by the value
of `history-length', which see.")
(make-variable-buffer-local 'input-method-history)
(put 'input-method-history 'permanent-local t)

(defvar inactivate-current-input-method-function nil
  "Function to call for inactivating the current input method.
Every input method should set this to an appropriate value when activated.
This function is called with no argument.

This function should never change the value of `current-input-method'.
It is set to nil by the function `inactivate-input-method'.")
(make-variable-buffer-local 'inactivate-current-input-method-function)
(put 'inactivate-current-input-method-function 'permanent-local t)

(defvar describe-current-input-method-function nil
  "Function to call for describing the current input method.
This function is called with no argument.")
(make-variable-buffer-local 'describe-current-input-method-function)
(put 'describe-current-input-method-function 'permanent-local t)

(defvar input-method-alist nil
  "Alist of input method names vs how to use them.
Each element has the form:
   (INPUT-METHOD LANGUAGE-ENV ACTIVATE-FUNC TITLE DESCRIPTION ARGS...)
See the function `register-input-method' for the meanings of the elements.")
;;;###autoload
(put 'input-method-alist 'risky-local-variable t)

(defun register-input-method (input-method lang-env &rest args)
  "Register INPUT-METHOD as an input method for language environment LANG-ENV.

INPUT-METHOD and LANG-ENV are symbols or strings.
ACTIVATE-FUNC is a function to call to activate this method.
TITLE is a string to show in the mode line when this method is active.
DESCRIPTION is a string describing this method and what it is good for.
The ARGS, if any, are passed as arguments to ACTIVATE-FUNC.
All told, the arguments to ACTIVATE-FUNC are INPUT-METHOD and the ARGS.

This function is mainly used in the file \"leim-list.el\" which is
created at Emacs build time, registering all Quail input methods
contained in the Emacs distribution.

In case you want to register a new Quail input method by yourself, be
careful to use the same input method title as given in the third
parameter of `quail-define-package'.  (If the values are different, the
string specified in this function takes precedence.)

The commands `describe-input-method' and `list-input-methods' need
these duplicated values to show some information about input methods
without loading the relevant Quail packages.
\n(fn INPUT-METHOD LANG-ENV ACTIVATE-FUNC TITLE DESCRIPTION &rest ARGS)"
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env))
    (setq lang-env (purecopy lang-env)))
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method))
    (setq input-method (purecopy input-method)))
  (setq args (mapcar 'purecopy args))
  (let ((info (cons lang-env args))
	(slot (assoc input-method input-method-alist)))
    (if slot
	(setcdr slot info)
      (setq slot (cons input-method info))
      (setq input-method-alist (cons slot input-method-alist)))))

(defun read-input-method-name (prompt &optional default inhibit-null)
  "Read a name of input method from a minibuffer prompting with PROMPT.
If DEFAULT is non-nil, use that as the default,
and substitute it into PROMPT at the first `%s'.
If INHIBIT-NULL is non-nil, null input signals an error.

The return value is a string."
  (if default
      (setq prompt (format prompt default)))
  (let* ((completion-ignore-case t)
	 ;; As it is quite normal to change input method in the
	 ;; minibuffer, we must enable it even if
	 ;; enable-recursive-minibuffers is currently nil.
	 (enable-recursive-minibuffers t)
	 ;; This binding is necessary because input-method-history is
	 ;; buffer local.
	 (input-method (completing-read prompt input-method-alist
					nil t nil 'input-method-history
					default)))
    (if (and input-method (symbolp input-method))
	(setq input-method (symbol-name input-method)))
    (if (> (length input-method) 0)
	input-method
      (if inhibit-null
	  (error "No valid input method is specified")))))

(defun activate-input-method (input-method)
  "Switch to input method INPUT-METHOD for the current buffer.
If some other input method is already active, turn it off first.
If INPUT-METHOD is nil, deactivate any current input method."
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (and current-input-method
	   (not (string= current-input-method input-method)))
      (inactivate-input-method))
  (unless (or current-input-method (null input-method))
    (let ((slot (assoc input-method input-method-alist)))
      (if (null slot)
	  (error "Can't activate input method `%s'" input-method))
      (setq current-input-method-title nil)
      (let ((func (nth 2 slot)))
	(if (functionp func)
	    (apply (nth 2 slot) input-method (nthcdr 5 slot))
	  (if (and (consp func) (symbolp (car func)) (symbolp (cdr func)))
	      (progn
		(require (cdr func))
		(apply (car func) input-method (nthcdr 5 slot)))
	    (error "Can't activate input method `%s'" input-method))))
      (setq current-input-method input-method)
      (or (stringp current-input-method-title)
	  (setq current-input-method-title (nth 3 slot)))
      (unwind-protect
	  (run-hooks 'input-method-activate-hook)
	(force-mode-line-update)))))

(defun inactivate-input-method ()
  "Turn off the current input method."
  (when current-input-method
    (if input-method-history
	(unless (string= current-input-method (car input-method-history))
	  (setq input-method-history
		(cons current-input-method
		      (delete current-input-method input-method-history))))
      (setq input-method-history (list current-input-method)))
    (unwind-protect
	(progn
	  (setq input-method-function nil
		current-input-method-title nil)
	  (funcall inactivate-current-input-method-function))
      (unwind-protect
	  (run-hooks 'input-method-inactivate-hook)
	(setq current-input-method nil)
	(force-mode-line-update)))))

(defun set-input-method (input-method &optional interactive)
  "Select and activate input method INPUT-METHOD for the current buffer.
This also sets the default input method to the one you specify.
If INPUT-METHOD is nil, this function turns off the input method, and
also causes you to be prompted for a name of an input method the next
time you invoke \\[toggle-input-method].
When called interactively, the optional arg INTERACTIVE is non-nil,
which marks the variable `default-input-method' as set for Custom buffers.

To deactivate the input method interactively, use \\[toggle-input-method].
To deactivate it programmatically, use `inactivate-input-method'."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t)
	   t)))
  (activate-input-method input-method)
  (setq default-input-method input-method)
  (when interactive
    (customize-mark-as-set 'default-input-method))
  default-input-method)

(defvar toggle-input-method-active nil
  "Non-nil inside `toggle-input-method'.")

(defun toggle-input-method (&optional arg interactive)
  "Enable or disable multilingual text input method for the current buffer.
Only one input method can be enabled at any time in a given buffer.

The normal action is to enable an input method if none was enabled,
and disable the current one otherwise.  Which input method to enable
can be determined in various ways--either the one most recently used,
or the one specified by `default-input-method', or as a last resort
by reading the name of an input method in the minibuffer.

With a prefix argument ARG, read an input method name with the minibuffer
and enable that one.  The default is the most recent input method specified
\(not including the currently active input method, if any).

When called interactively, the optional argument INTERACTIVE is non-nil,
which marks the variable `default-input-method' as set for Custom buffers."

  (interactive "P\np")
  (if toggle-input-method-active
      (error "Recursive use of `toggle-input-method'"))
  (if (and current-input-method (not arg))
      (inactivate-input-method)
    (let ((toggle-input-method-active t)
	  (default (or (car input-method-history) default-input-method)))
      (if (and arg default (equal current-input-method default)
	       (> (length input-method-history) 1))
	  (setq default (nth 1 input-method-history)))
      (activate-input-method
       (if (or arg (not default))
	   (progn
	     (read-input-method-name
	      (if default "Input method (default %s): " "Input method: " )
	      default t))
	 default))
      (unless default-input-method
	(prog1
	    (setq default-input-method current-input-method)
	  (when interactive
	    (customize-mark-as-set 'default-input-method)))))))

(autoload 'help-buffer "help-mode")

(defun describe-input-method (input-method)
  "Describe input method INPUT-METHOD."
  (interactive
   (list (read-input-method-name
	  "Describe input method (default current choice): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (help-setup-xref (list #'describe-input-method
			 (or input-method current-input-method))
		   (called-interactively-p 'interactive))

  (if (null input-method)
      (describe-current-input-method)
    (let ((current current-input-method))
      (condition-case nil
	  (progn
	    (save-excursion
	      (activate-input-method input-method)
	      (describe-current-input-method))
	    (activate-input-method current))
	(error
	 (activate-input-method current)
	 (help-setup-xref (list #'describe-input-method input-method)
			  (called-interactively-p 'interactive))
	 (with-output-to-temp-buffer (help-buffer)
	   (let ((elt (assoc input-method input-method-alist)))
	     (princ (format
		     "Input method: %s (`%s' in mode line) for %s\n  %s\n"
		     input-method (nth 3 elt) (nth 1 elt) (nth 4 elt))))))))))

(defun describe-current-input-method ()
  "Describe the input method currently in use.
This is a subroutine for `describe-input-method'."
  (if current-input-method
      (if (and (symbolp describe-current-input-method-function)
	       (fboundp describe-current-input-method-function))
	  (funcall describe-current-input-method-function)
	(message "No way to describe the current input method `%s'"
		 current-input-method)
	(ding))
    (error "No input method is activated now")))

(defun read-multilingual-string (prompt &optional initial-input input-method)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second argument INITIAL-INPUT is non-nil, insert it in the
minibuffer initially.
Optional 3rd argument INPUT-METHOD specifies the input method to be activated
instead of the one selected last time.  It is a symbol or a string."
  (setq input-method
	(or input-method
	    current-input-method
	    default-input-method
	    (read-input-method-name "Input method: " nil t)))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((prev-input-method current-input-method))
    (unwind-protect
	(progn
	  (activate-input-method input-method)
	  (read-string prompt initial-input nil nil t))
      (activate-input-method prev-input-method))))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defcustom input-method-verbose-flag 'default
  "A flag to control extra guidance given by input methods.
The value should be nil, t, `complex-only', or `default'.

The extra guidance is done by showing list of available keys in echo
area.  When you use the input method in the minibuffer, the guidance
is shown at the bottom short window (split from the existing window).

If the value is t, extra guidance is always given, if the value is
nil, extra guidance is always suppressed.

If the value is `complex-only', only complex input methods such as
`chinese-py' and `japanese' give extra guidance.

If the value is `default', complex input methods always give extra
guidance, but simple input methods give it only when you are not in
the minibuffer.

See also the variable `input-method-highlight-flag'."
  :type '(choice (const :tag "Always" t) (const :tag "Never" nil)
		 (const complex-only) (const default))
  :group 'mule)

(defcustom input-method-highlight-flag t
  "If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence.
See also the variable `input-method-verbose-flag'."
  :type 'boolean
  :group 'mule)

(defcustom input-method-activate-hook nil
  "Normal hook run just after an input method is activated.

The variable `current-input-method' keeps the input method name
just activated."
  :type 'hook
  :group 'mule)

(defcustom input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.

The variable `current-input-method' still keeps the input method name
just inactivated."
  :type 'hook
  :group 'mule)

(defcustom input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text."
  :type 'hook
  :group 'mule)

(defvar input-method-exit-on-first-char nil
  "This flag controls when an input method returns.
Usually, the input method does not return while there's a possibility
that it may find a different translation if a user types another key.
But, if this flag is non-nil, the input method returns as soon as the
current key sequence gets long enough to have some valid translation.")

(defcustom input-method-use-echo-area nil
  "This flag controls how an input method shows an intermediate key sequence.
Usually, the input method inserts the intermediate key sequence,
or candidate translations corresponding to the sequence,
at point in the current buffer.
But, if this flag is non-nil, it displays them in echo area instead."
  :type 'hook
  :group 'mule)

(defvar input-method-exit-on-invalid-key nil
  "This flag controls the behavior of an input method on invalid key input.
Usually, when a user types a key which doesn't start any character
handled by the input method, the key is handled by turning off the
input method temporarily.  After that key, the input method is re-enabled.
But, if this flag is non-nil, the input method is never back on.")


(defcustom set-language-environment-hook nil
  "Normal hook run after some language environment is set.

When you set some hook function here, that effect usually should not
be inherited to another language environment.  So, you had better set
another function in `exit-language-environment-hook' (which see) to
cancel the effect."
  :type 'hook
  :group 'mule)

(defcustom exit-language-environment-hook nil
  "Normal hook run after exiting from some language environment.
When this hook is run, the variable `current-language-environment'
is still bound to the language environment being exited.

This hook is mainly used for canceling the effect of
`set-language-environment-hook' (which see)."
  :type 'hook
  :group 'mule)

(put 'setup-specified-language-environment 'apropos-inhibit t)

(defun setup-specified-language-environment ()
  "Switch to a specified language environment."
  (interactive)
  (let (language-name)
    (if (and (symbolp last-command-event)
	     (or (not (eq last-command-event 'Default))
		 (setq last-command-event 'English))
	     (setq language-name (symbol-name last-command-event)))
	(prog1
	    (set-language-environment language-name)
	  (customize-mark-as-set 'current-language-environment))
      (error "Bogus calling sequence"))))

(defcustom current-language-environment "English"
  "The last language environment specified with `set-language-environment'.
This variable should be set only with \\[customize], which is equivalent
to using the function `set-language-environment'."
  :link '(custom-manual "(emacs)Language Environments")
  :set (lambda (symbol value) (set-language-environment value))
  :get (lambda (x)
	 (or (car-safe (assoc-string
			(if (symbolp current-language-environment)
			    (symbol-name current-language-environment)
			  current-language-environment)
			language-info-alist t))
	     "English"))
  ;; custom type will be updated with `set-language-info'.
  :type (if language-info-alist
	    (cons 'choice (mapcar
			   (lambda (lang)
			     (list 'const lang))
			   (sort (mapcar 'car language-info-alist) 'string<)))
	  'string)
  :initialize 'custom-initialize-default
  :group 'mule)

(defun reset-language-environment ()
  "Reset multilingual environment of Emacs to the default status.

The default status is as follows:

  The default value of `buffer-file-coding-system' is nil.
  The default coding system for process I/O is nil.
  The default value for the command `set-terminal-coding-system' is nil.
  The default value for the command `set-keyboard-coding-system' is nil.

  The order of priorities of coding systems are as follows:
	utf-8
	iso-2022-7bit
	iso-latin-1
	iso-2022-7bit-lock
	iso-2022-8bit-ss2
	emacs-mule
	raw-text"
  (interactive)
  ;; This function formerly set default-enable-multibyte-characters to t,
  ;; but that is incorrect.  It should not alter the unibyte/multibyte choice.

  (set-coding-system-priority
   'utf-8
   'iso-2022-7bit
   'iso-latin-1
   'iso-2022-7bit-lock
   'iso-2022-8bit-ss2
   'emacs-mule
   'raw-text)

  (set-default-coding-systems nil)
  (setq default-sendmail-coding-system 'iso-latin-1)
  ;; On Darwin systems, this should be utf-8, but when this file is loaded
  ;; utf-8 is not yet defined, so we set it in set-locale-environment instead.
  (setq default-file-name-coding-system 'iso-latin-1)
  ;; Preserve eol-type from existing default-process-coding-systems.
  ;; On non-unix-like systems in particular, these may have been set
  ;; carefully by the user, or by the startup code, to deal with the
  ;; users shell appropriately, so should not be altered by changing
  ;; language environment.
  (let ((output-coding
	 ;; When bootstrapping, coding-systems are not defined yet, so
	 ;; we need to catch the error from check-coding-system.
	 (condition-case nil
	     (coding-system-change-text-conversion
	      (car default-process-coding-system) 'undecided)
	   (coding-system-error 'undecided)))
	(input-coding
	 (condition-case nil
	     (coding-system-change-text-conversion
	      (cdr default-process-coding-system) 'iso-latin-1)
	   (coding-system-error 'iso-latin-1))))
    (setq default-process-coding-system
	  (cons output-coding input-coding)))

  ;; Put the highest priority to the charset iso-8859-1 to prefer the
  ;; registry iso8859-1 over iso8859-2 in font selection.  It also
  ;; makes unibyte-display-via-language-environment to use iso-8859-1
  ;; as the unibyte charset.
  (set-charset-priority 'iso-8859-1)

  ;; Don't alter the terminal and keyboard coding systems here.
  ;; The terminal still supports the same coding system
  ;; that it supported a minute ago.
  ;; (set-terminal-coding-system-internal nil)
  ;; (set-keyboard-coding-system-internal nil)

  ;; Back in Emacs-20, it was necessary to provide some fallback implicit
  ;; conversion, because almost no packages handled coding-system issues.
  ;; Nowadays it'd just paper over bugs.
  ;; (set-unibyte-charset 'iso-8859-1)
  )

(reset-language-environment)

(defun set-display-table-and-terminal-coding-system (language-name &optional coding-system display)
  "Set up the display table and terminal coding system for LANGUAGE-NAME."
  (let ((coding (get-language-info language-name 'unibyte-display)))
    (if (and coding
	     (or (not coding-system)
		 (coding-system-equal coding coding-system)))
	(standard-display-european-internal)
      ;; The following 2 lines undo the 8-bit display that we set up
      ;; in standard-display-european-internal, which see.  This is in
      ;; case the user has used standard-display-european earlier in
      ;; this session.
      (when standard-display-table
	(dotimes (i 128)
	  (aset standard-display-table (+ i 128) nil))))
    (set-terminal-coding-system (or coding-system coding) display)))

(defun set-language-environment (language-name)
  "Set up multi-lingual environment for using LANGUAGE-NAME.
This sets the coding system priority and the default input method
and sometimes other things.  LANGUAGE-NAME should be a string
which is the name of a language environment.  For example, \"Latin-1\"
specifies the character set for the major languages of Western Europe.

If there is a prior value for `current-language-environment', this
runs the hook `exit-language-environment-hook'.  After setting up
the new language environment, it runs `set-language-environment-hook'."
  (interactive (list (read-language-name
		      nil
		      "Set language environment (default English): ")))
  (if language-name
      (if (symbolp language-name)
	  (setq language-name (symbol-name language-name)))
    (setq language-name "English"))
  (let ((slot (assoc-string language-name language-info-alist t)))
    (unless slot
      (error "Language environment not defined: %S" language-name))
    (setq language-name (car slot)))
  (if current-language-environment
      (let ((func (get-language-info current-language-environment
				     'exit-function)))
	(run-hooks 'exit-language-environment-hook)
	(if (functionp func) (funcall func))))

  (reset-language-environment)
  ;; The features might set up coding systems.
  (let ((required-features (get-language-info language-name 'features)))
    (while required-features
      (require (car required-features))
      (setq required-features (cdr required-features))))

  (setq current-language-environment language-name)

  (set-language-environment-coding-systems language-name)
  (set-language-environment-input-method language-name)
  (set-language-environment-nonascii-translation language-name)
  (set-language-environment-charset language-name)
  ;; Unibyte setups if necessary.
  (unless (default-value 'enable-multibyte-characters)
    (set-language-environment-unibyte language-name))

  (let ((func (get-language-info language-name 'setup-function)))
    (if (functionp func)
	(funcall func)))

  (setq current-iso639-language
	(or (get-language-info language-name 'iso639-language)
	    current-iso639-language))

  (run-hooks 'set-language-environment-hook)
  (force-mode-line-update t))

(define-widget 'charset 'symbol
  "An Emacs charset."
  :tag "Charset"
  :completions
  (lambda (string pred action)
    (let ((completion-ignore-case t))
      (completion-table-with-predicate
       obarray #'charsetp 'strict string pred action)))
  :value 'ascii
  :validate (lambda (widget)
	      (unless (charsetp (widget-value widget))
		(widget-put widget :error (format "Invalid charset: %S"
						  (widget-value widget)))
		widget))
  :prompt-history 'charset-history)

(defcustom language-info-custom-alist nil
  "Customizations of language environment parameters.
Value is an alist with elements like those of `language-info-alist'.
These are used to set values in `language-info-alist' which replace
the defaults.  A typical use is replacing the default input method for
the environment.  Use \\[describe-language-environment] to find the environment's settings.

This option is intended for use at startup.  Removing items doesn't
remove them from the language info until you next restart Emacs.

Setting this variable directly does not take effect.
See `set-language-info-alist' for use in programs."
  :group 'mule
  :version "23.1"
  :set (lambda (s v)
	 (custom-set-default s v)
	 ;; Can't do this before language environments are set up.
	 (when v
	   ;; modify language-info-alist
	   (dolist (elt v)
	     (set-language-info-alist (car elt) (cdr elt)))
	   ;; re-set the environment in case its parameters changed
	   (set-language-environment current-language-environment)))
  :type `(alist
	  :key-type (string :tag "Language environment"
			    :completions
                            (lambda (string pred action)
                              (let ((completion-ignore-case t))
                                (complete-with-action
                                 action language-info-alist string pred))))
	  :value-type
	  (alist :key-type symbol
		 :options ((documentation string)
			   (charset (repeat charset))
			   (sample-text string)
			   (setup-function function)
			   (exit-function function)
			   (coding-system (repeat coding-system))
			   (coding-priority (repeat coding-system))
			   (nonascii-translation charset)
			   (input-method mule-input-method-string)
			   (features (repeat symbol))
			   (unibyte-display coding-system)))))

(declare-function x-server-vendor "xfns.c" (&optional terminal))
(declare-function x-server-version "xfns.c" (&optional terminal))

(defun standard-display-european-internal ()
  ;; Actually set up direct output of non-ASCII characters.
  (standard-display-8bit (if (eq window-system 'pc) 128 160) 255)
  ;; Unibyte Emacs on MS-DOS wants to display all 8-bit characters with
  ;; the native font, and codes 160 and 146 stand for something very
  ;; different there.
  (or (and (eq window-system 'pc) (not (default-value
					 'enable-multibyte-characters)))
      (progn
	;; Most X fonts used to do the wrong thing for latin-1 code 160.
	(unless (and (eq window-system 'x)
		     ;; XFree86 4 has fixed the fonts.
		     (string= "The XFree86 Project, Inc" (x-server-vendor))
		     (> (aref (number-to-string (nth 2 (x-server-version))) 0)
			?3))
	  ;; Make non-line-break space display as a plain space.
	  (aset standard-display-table (unibyte-char-to-multibyte 160) [32]))
	;; Most Windows programs send out apostrophes as \222.  Most X fonts
	;; don't contain a character at that position.  Map it to the ASCII
	;; apostrophe.  [This is actually RIGHT SINGLE QUOTATION MARK,
	;; U+2019, normally from the windows-1252 character set.  XFree 4
	;; fonts probably have the appropriate glyph at this position,
	;; so they could use standard-display-8bit.  It's better to use a
	;; proper windows-1252 coding system.  --fx]
	(aset standard-display-table (unibyte-char-to-multibyte 146) [39]))))

(defun set-language-environment-coding-systems (language-name)
  "Do various coding system setups for language environment LANGUAGE-NAME."
  (let* ((priority (get-language-info language-name 'coding-priority))
	 (default-coding (car priority))
	 ;; If the default buffer-file-coding-system is nil, don't use
	 ;; coding-system-eol-type, because it treats nil as
	 ;; `no-conversion'.  The default buffer-file-coding-system is set
	 ;; to nil by reset-language-environment, and in that case we
	 ;; want to have here the native EOL type for each platform.
	 ;; FIXME: there should be a common code that runs both on
	 ;; startup and here to set the default EOL type correctly.
	 ;; Right now, DOS/Windows platforms set this on dos-w32.el,
	 ;; which works only as long as the order of loading files at
	 ;; dump time and calling functions at startup is not modified
	 ;; significantly, i.e. as long as this function is called
	 ;; _after_ the default buffer-file-coding-system was set by
	 ;; dos-w32.el.
	 (eol-type
          (coding-system-eol-type
           (or (default-value 'buffer-file-coding-system)
               (if (memq system-type '(windows-nt ms-dos)) 'dos 'unix)))))
    (when priority
      (set-default-coding-systems
       (if (memq eol-type '(0 1 2 unix dos mac))
	   (coding-system-change-eol-conversion default-coding eol-type)
	 default-coding))
      (setq default-sendmail-coding-system default-coding)
      (apply 'set-coding-system-priority priority))))

(defun set-language-environment-input-method (language-name)
  "Do various input method setups for language environment LANGUAGE-NAME."
  (let ((input-method (get-language-info language-name 'input-method)))
    (when input-method
      (setq default-input-method input-method)
      (if input-method-history
	  (setq input-method-history
		(cons input-method
		      (delete input-method input-method-history)))))))

(defun set-language-environment-nonascii-translation (language-name)
  "Do unibyte/multibyte translation setup for language environment LANGUAGE-NAME."
  ;; Note: For DOS, we assumed that the charset cpXXX is already
  ;; defined.
  (let ((nonascii (get-language-info language-name 'nonascii-translation)))
    (if (eq window-system 'pc)
	(setq nonascii (intern (format "cp%d" dos-codepage))))
    (or (and (charsetp nonascii)
	     (get-charset-property nonascii :ascii-compatible-p))
	(setq nonascii 'iso-8859-1))
    ;; Back in Emacs-20, it was necessary to provide some fallback implicit
    ;; conversion, because almost no packages handled coding-system issues.
    ;; Nowadays it'd just paper over bugs.
    ;; (set-unibyte-charset nonascii)
    ))

(defun set-language-environment-charset (language-name)
  "Do various charset setups for language environment LANGUAGE-NAME."
  ;; Put higher priorities to such charsets that are supported by the
  ;; coding systems of higher priorities in this environment.
  (let ((charsets (get-language-info language-name 'charset)))
    (dolist (coding (get-language-info language-name 'coding-priority))
      (let ((list (coding-system-charset-list coding)))
	(if (consp list)
	    (setq charsets (append charsets list)))))
    (if charsets
	(apply 'set-charset-priority charsets))))

(defun set-language-environment-unibyte (language-name)
  "Do various unibyte-mode setups for language environment LANGUAGE-NAME."
  (set-display-table-and-terminal-coding-system language-name))

(defun princ-list (&rest args)
  "Print all arguments with `princ', then print \"\\n\"."
  (mapc #'princ args)
  (princ "\n"))
(make-obsolete 'princ-list "use mapc and princ instead" "23.3")

(put 'describe-specified-language-support 'apropos-inhibit t)

;; Print language-specific information such as input methods,
;; charsets, and coding systems.  This function is intended to be
;; called from the menu:
;;   [menu-bar mule describe-language-environment LANGUAGE]
;; and should not run it by `M-x describe-current-input-method-function'.
(defun describe-specified-language-support ()
  "Describe how Emacs supports the specified language environment."
  (interactive)
  (let (language-name)
    (if (not (and (symbolp last-command-event)
		  (or (not (eq last-command-event 'Default))
		      (setq last-command-event 'English))
		  (setq language-name (symbol-name last-command-event))))
	(error "This command should only be called from the menu bar"))
    (describe-language-environment language-name)))

(defun describe-language-environment (language-name)
  "Describe how Emacs supports language environment LANGUAGE-NAME."
  (interactive
   (list (read-language-name
	  'documentation
	  "Describe language environment (default current choice): ")))
  (if (null language-name)
      (setq language-name current-language-environment))
  (if (or (null language-name)
	  (null (get-language-info language-name 'documentation)))
      (error "No documentation for the specified language"))
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (dolist (feature (get-language-info language-name 'features))
    (require feature))
  (let ((doc (get-language-info language-name 'documentation)))
    (help-setup-xref (list #'describe-language-environment language-name)
		     (called-interactively-p 'interactive))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	(insert language-name " language environment\n\n")
	(if (stringp doc)
	    (insert doc "\n\n"))
	(condition-case nil
	    (let ((str (eval (get-language-info language-name 'sample-text))))
	      (if (stringp str)
		  (insert "Sample text:\n  "
			  (replace-regexp-in-string "\n" "\n  " str)
			  "\n\n")))
	  (error nil))
	(let ((input-method (get-language-info language-name 'input-method))
	      (l (copy-sequence input-method-alist))
	      (first t))
	  (when (and input-method
		     (setq input-method (assoc input-method l)))
	    (insert "Input methods (default " (car input-method) ")\n")
	    (setq l (cons input-method (delete input-method l))
		  first nil))
	  (dolist (elt l)
	    (when (or (eq input-method elt)
		      (eq t (compare-strings language-name nil nil
					     (nth 1 elt) nil nil t)))
	      (when first
		(insert "Input methods:\n")
		(setq first nil))
	      (insert "  " (car elt))
	      (search-backward (car elt))
	      (help-xref-button 0 'help-input-method (car elt))
	      (goto-char (point-max))
	      (insert " (\""
		      (if (stringp (nth 3 elt)) (nth 3 elt) (car (nth 3 elt)))
		      "\" in mode line)\n")))
	  (or first
	      (insert "\n")))
	(insert "Character sets:\n")
	(let ((l (get-language-info language-name 'charset)))
	  (if (null l)
	      (insert "  nothing specific to " language-name "\n")
	    (while l
	      (insert "  " (symbol-name (car l)))
	      (search-backward (symbol-name (car l)))
	      (help-xref-button 0 'help-character-set (car l))
	      (goto-char (point-max))
	      (insert ": " (charset-description (car l)) "\n")
	      (setq l (cdr l)))))
	(insert "\n")
	(insert "Coding systems:\n")
	(let ((l (get-language-info language-name 'coding-system)))
	  (if (null l)
	      (insert "  nothing specific to " language-name "\n")
	    (while l
	      (insert "  " (symbol-name (car l)))
	      (search-backward (symbol-name (car l)))
	      (help-xref-button 0 'help-coding-system (car l))
	      (goto-char (point-max))
	      (insert " (`"
		      (coding-system-mnemonic (car l))
		      "' in mode line):\n\t"
		      (coding-system-doc-string (car l))
		      "\n")
	      (let ((aliases (coding-system-aliases (car l))))
		(when aliases
		  (insert "\t(alias:")
		  (while aliases
		    (insert " " (symbol-name (car aliases)))
		    (setq aliases (cdr aliases)))
		  (insert ")\n")))
	      (setq l (cdr l)))))))))

;;; Locales.

(defvar locale-translation-file-name nil
  "File name for the system's file of locale-name aliases, or nil if none.")

;; The following definitions might as well be marked as constants and
;; purecopied, since they're normally used on startup, and probably
;; should reflect the facilities of the base Emacs.
(defconst locale-language-names
  (purecopy
   '(
    ;; Locale names of the form LANGUAGE[_TERRITORY][.CODESET][@MODIFIER]
    ;; as specified in the Single Unix Spec, Version 2.
    ;; LANGUAGE is a language code taken from ISO 639:1988 (E/F)
    ;; with additions from ISO 639/RA Newsletter No.1/1989;
    ;; see Internet RFC 2165 (1997-06) and
    ;; http://www.evertype.com/standards/iso639/iso639-en.html
    ;; TERRITORY is a country code taken from ISO 3166
    ;; http://www.din.de/gremien/nas/nabd/iso3166ma/codlstp1/en_listp1.html.
    ;; CODESET and MODIFIER are implementation-dependent.

     ;; jasonr comments: MS Windows uses three letter codes for
     ;; languages instead of the two letter ISO codes that POSIX
     ;; uses. In most cases the first two letters are the same, so
     ;; most of the regexps in locale-language-names work. Japanese
     ;; and Chinese are exceptions, which are listed in the
     ;; non-standard section at the bottom of locale-language-names.

    ("aa_DJ" . "Latin-1") ; Afar
    ("aa" . "UTF-8")
    ;; ab Abkhazian
    ("af" . "Latin-1") ; Afrikaans
    ("am" "Ethiopic" utf-8) ; Amharic
    ("an" . "Latin-9") ; Aragonese
    ("ar" . "Arabic")
    ; as Assamese
    ; ay Aymara
    ("az" . "UTF-8") ; Azerbaijani
    ; ba Bashkir
    ("be" "Belarusian" cp1251) ; Belarusian [Byelorussian until early 1990s]
    ("bg" "Bulgarian" cp1251) ; Bulgarian
    ; bh Bihari
    ; bi Bislama
    ("bn" . "UTF-8") ; Bengali, Bangla
    ("bo" . "Tibetan")
    ("br" . "Latin-1") ; Breton
    ("bs" . "Latin-2") ; Bosnian
    ("byn" . "UTF-8")  ; Bilin; Blin
    ("ca" . "Latin-1") ; Catalan
    ; co Corsican
    ("cs" "Czech" iso-8859-2)
    ("cy" "Welsh" iso-8859-14)
    ("da" . "Latin-1") ; Danish
    ("de" "German" iso-8859-1)
    ; dv Divehi
    ; dz Bhutani
    ("el" "Greek" iso-8859-7)
    ;; Users who specify "en" explicitly typically want Latin-1, not ASCII.
    ;; That's actually what the GNU locales define, modulo things like
    ;; en_IN -- fx.
    ("en_IN" "English" utf-8) ; glibc uses utf-8 for English in India
    ("en" "English" iso-8859-1) ; English
    ("eo" . "Esperanto") ; Esperanto
    ("es" "Spanish" iso-8859-1)
    ("et" . "Latin-1") ; Estonian
    ("eu" . "Latin-1") ; Basque
    ("fa" . "UTF-8") ; Persian
    ("fi" . "Latin-1") ; Finnish
    ("fj" . "Latin-1") ; Fiji
    ("fo" . "Latin-1") ; Faroese
    ("fr" "French" iso-8859-1) ; French
    ("fy" . "Latin-1") ; Frisian
    ("ga" . "Latin-1") ; Irish Gaelic (new orthography)
    ("gd" . "Latin-9") ; Scots Gaelic
    ("gez" "Ethiopic" utf-8) ; Geez
    ("gl" . "Latin-1") ; Gallegan; Galician
    ; gn Guarani
    ("gu" . "UTF-8") ; Gujarati
    ("gv" . "Latin-1") ; Manx Gaelic
    ; ha Hausa
    ("he" "Hebrew" iso-8859-8)
    ("hi" "Devanagari" utf-8) ; Hindi
    ("hr" "Croatian" iso-8859-2) ; Croatian
    ("hu" . "Latin-2") ; Hungarian
    ; hy Armenian
    ; ia Interlingua
    ("id" . "Latin-1") ; Indonesian
    ; ie Interlingue
    ; ik Inupiak
    ("is" . "Latin-1") ; Icelandic
    ("it" "Italian" iso-8859-1) ; Italian
    ; iu Inuktitut
    ("iw" "Hebrew" iso-8859-8)
    ("ja" "Japanese" euc-jp)
    ; jw Javanese
    ("ka" "Georgian" georgian-ps) ; Georgian
    ; kk Kazakh
    ("kl" . "Latin-1") ; Greenlandic
    ; km Cambodian
    ("kn" "Kannada" utf-8)
    ("ko" "Korean" euc-kr)
    ; ks Kashmiri
    ; ku Kurdish
    ("kw" . "Latin-1") ; Cornish
    ; ky Kirghiz
    ("la" . "Latin-1") ; Latin
    ("lb" . "Latin-1") ; Luxemburgish
    ("lg" . "Laint-6") ; Ganda
    ; ln Lingala
    ("lo" "Lao" utf-8) ; Laothian
    ("lt" "Lithuanian" iso-8859-13)
    ("lv" . "Latvian") ; Latvian, Lettish
    ; mg Malagasy
    ("mi" . "Latin-7") ; Maori
    ("mk" "Cyrillic-ISO" iso-8859-5) ; Macedonian
    ("ml" "Malayalam" utf-8)
    ("mn" . "UTF-8") ; Mongolian
    ; mo Moldavian
    ("mr" "Devanagari" utf-8) ; Marathi
    ("ms" . "Latin-1") ; Malay
    ("mt" . "Latin-3") ; Maltese
    ; my Burmese
    ; na Nauru
    ("nb" . "Latin-1") ; Norwegian
    ("ne" "Devanagari" utf-8) ; Nepali
    ("nl" "Dutch" iso-8859-1)
    ("no" . "Latin-1") ; Norwegian
    ("oc" . "Latin-1") ; Occitan
    ("om_ET" . "UTF-8") ; (Afan) Oromo
    ("om" . "Latin-1") ; (Afan) Oromo
    ; or Oriya
    ("pa" . "UTF-8") ; Punjabi
    ("pl" . "Latin-2") ; Polish
    ; ps Pashto, Pushto
    ("pt" . "Latin-1") ; Portuguese
    ; qu Quechua
    ("rm" . "Latin-1") ; Rhaeto-Romanic
    ; rn Kirundi
    ("ro" "Romanian" iso-8859-2)
    ("ru_RU" "Russian" iso-8859-5)
    ("ru_UA" "Russian" koi8-u)
    ; rw Kinyarwanda
    ("sa" . "Devanagari") ; Sanskrit
    ; sd Sindhi
    ("se" . "UTF-8") ; Northern Sami
    ; sg Sangho
    ("sh" . "Latin-2") ; Serbo-Croatian
    ; si Sinhalese
    ("sid" . "UTF-8") ; Sidamo
    ("sk" "Slovak" iso-8859-2)
    ("sl" "Slovenian" iso-8859-2)
    ; sm Samoan
    ; sn Shona
    ("so_ET" "UTF-8") ; Somali
    ("so" "Latin-1") ; Somali
    ("sq" . "Latin-1") ; Albanian
    ("sr" . "Latin-2") ; Serbian (Latin alphabet)
    ; ss Siswati
    ("st" . "Latin-1") ;  Sesotho
    ; su Sundanese
    ("sv" "Swedish" iso-8859-1)		; Swedish
    ("sw" . "Latin-1") ; Swahili
    ("ta" "Tamil" utf-8)
    ("te" . "UTF-8") ; Telugu
    ("tg" "Tajik" koi8-t)
    ("th" "Thai" tis-620)
    ("ti" "Ethiopic" utf-8) ; Tigrinya
    ("tig_ER" . "UTF-8") ; Tigre
    ; tk Turkmen
    ("tl" . "Latin-1") ; Tagalog
    ; tn Setswana
    ; to Tonga
    ("tr" "Turkish" iso-8859-9)
    ; ts Tsonga
    ("tt" . "UTF-8") ; Tatar
    ; tw Twi
    ; ug Uighur
    ("uk" "Ukrainian" koi8-u)
    ("ur" . "UTF-8") ; Urdu
    ("uz_UZ@cyrillic" . "UTF-8"); Uzbek
    ("uz" . "Latin-1") ; Uzbek
    ("vi" "Vietnamese" utf-8)
    ; vo Volapuk
    ("wa" . "Latin-1") ; Walloon
    ; wo Wolof
    ("xh" . "Latin-1") ; Xhosa
    ("yi" . "Windows-1255") ; Yiddish
    ; yo Yoruba
    ; za Zhuang
    ("zh_HK" . "Chinese-Big5")
    ; zh_HK/BIG5-HKSCS \
    ("zh_TW" . "Chinese-Big5")
    ("zh_CN.GB2312" "Chinese-GB")
    ("zh_CN.GBK" "Chinese-GBK")
    ("zh_CN.GB18030" "Chinese-GB18030")
    ("zh_CN.UTF-8" . "Chinese-GBK")
    ("zh_CN" . "Chinese-GB")
    ("zh" . "Chinese-GB")
    ("zu" . "Latin-1") ; Zulu

    ;; ISO standard locales
    ("c$" . "ASCII")
    ("posix$" . "ASCII")

    ;; The "IPA" Emacs language environment does not correspond
    ;; to any ISO 639 code, so let it stand for itself.
    ("ipa$" . "IPA")

    ;; Nonstandard or obsolete language codes
    ("cz" . "Czech") ; e.g. Solaris 2.6
    ("ee" . "Latin-4") ; Estonian, e.g. X11R6.4
    ("iw" . "Hebrew") ; e.g. X11R6.4
    ("sp" . "Cyrillic-ISO") ; Serbian (Cyrillic alphabet), e.g. X11R6.4
    ("su" . "Latin-1") ; Finnish, e.g. Solaris 2.6
    ("jp" . "Japanese") ; e.g. MS Windows
    ("chs" . "Chinese-GBK") ; MS Windows Chinese Simplified
    ("cht" . "Chinese-BIG5") ; MS Windows Chinese Traditional
    ("gbz" . "UTF-8") ; MS Windows Dari Persian
    ("div" . "UTF-8") ; MS Windows Divehi (Maldives)
    ("wee" . "Latin-2") ; MS Windows Lower Sorbian
    ("wen" . "Latin-2") ; MS Windows Upper Sorbian
    ))
  "Alist of locale regexps vs the corresponding languages and coding systems.
Each element has this form:
  \(LOCALE-REGEXP LANG-ENV CODING-SYSTEM)
The first element whose LOCALE-REGEXP matches the start of a
downcased locale specifies the LANG-ENV \(language environment)
and CODING-SYSTEM corresponding to that locale.  If there is no
appropriate language environment, the element may have this form:
  \(LOCALE-REGEXP . LANG-ENV)
In this case, LANG-ENV is one of generic language environments for an
specific encoding such as \"Latin-1\" and \"UTF-8\".")

(defconst locale-charset-language-names
  (purecopy
   '((".*8859[-_]?1\\>" . "Latin-1")
     (".*8859[-_]?2\\>" . "Latin-2")
     (".*8859[-_]?3\\>" . "Latin-3")
     (".*8859[-_]?4\\>" . "Latin-4")
     (".*8859[-_]?9\\>" . "Latin-5")
     (".*8859[-_]?14\\>" . "Latin-8")
     (".*8859[-_]?15\\>" . "Latin-9")
     (".*utf\\(?:-?8\\)?\\>" . "UTF-8")
     ;; utf-8@euro exists, so put this last.  (@euro really specifies
     ;; the currency, rather than the charset.)
     (".*@euro\\>" . "Latin-9")))
  "List of pairs of locale regexps and charset language names.
The first element whose locale regexp matches the start of a downcased locale
specifies the language name whose charset corresponds to that locale.
This language name is used if the locale is not listed in
`locale-language-names'.")

(defconst locale-preferred-coding-systems
  (purecopy
   '((".*8859[-_]?1\\>" . iso-8859-1)
     (".*8859[-_]?2\\>" . iso-8859-2)
     (".*8859[-_]?3\\>" . iso-8859-3)
     (".*8859[-_]?4\\>" . iso-8859-4)
     (".*8859[-_]?9\\>" . iso-8859-9)
     (".*8859[-_]?14\\>" . iso-8859-14)
     (".*8859[-_]?15\\>" . iso-8859-15)
     (".*utf\\(?:-?8\\)?" . utf-8)
     ;; utf-8@euro exists, so put this after utf-8.  (@euro really
     ;; specifies the currency, rather than the charset.)
     (".*@euro" . iso-8859-15)
     ("koi8-?r" . koi8-r)
     ("koi8-?u" . koi8-u)
     ("tcvn" . tcvn)
     ("big5[-_]?hkscs" . big5-hkscs)
     ("big5" . big5)
     ("euc-?tw" . euc-tw)
     ("euc-?cn" . euc-cn)
     ("gb2312" . gb2312)
     ("gbk" . gbk)
     ("gb18030" . gb18030)
     ("ja.*[._]euc" . japanese-iso-8bit)
     ("ja.*[._]jis7" . iso-2022-jp)
     ("ja.*[._]pck" . japanese-shift-jis)
     ("ja.*[._]sjis" . japanese-shift-jis)
     ("jpn" . japanese-shift-jis)   ; MS-Windows uses this.
     ))
  "List of pairs of locale regexps and preferred coding systems.
The first element whose locale regexp matches the start of a downcased locale
specifies the coding system to prefer when using that locale.
This coding system is used if the locale specifies a specific charset.")

(defun locale-name-match (key alist)
  "Search for KEY in ALIST, which should be a list of regexp-value pairs.
Return the value corresponding to the first regexp that matches the
start of KEY, or nil if there is no match."
  (let (element)
    (while (and alist (not element))
      (if (string-match-p (concat "\\`\\(?:" (car (car alist)) "\\)") key)
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    (cdr element)))

(defun locale-charset-match-p (charset1 charset2)
  "Whether charset names (strings) CHARSET1 and CHARSET2 are equivalent.
Matching is done ignoring case and any hyphens and underscores in the
names.  E.g. `ISO_8859-1' and `iso88591' both match `iso-8859-1'."
  (setq charset1 (replace-regexp-in-string "[-_]" "" charset1))
  (setq charset2 (replace-regexp-in-string "[-_]" "" charset2))
  (eq t (compare-strings charset1 nil nil charset2 nil nil t)))

(defvar locale-charset-alist nil
  "Coding system alist keyed on locale-style charset name.
Used by `locale-charset-to-coding-system'.")

(defun locale-charset-to-coding-system (charset)
  "Find coding system corresponding to CHARSET.
CHARSET is any sort of non-Emacs charset name, such as might be used
in a locale codeset, or elsewhere.  It is matched to a coding system
first by case-insensitive lookup in `locale-charset-alist'.  Then
matches are looked for in the coding system list, treating case and
the characters `-' and `_' as insignificant.  The coding system base
is returned.  Thus, for instance, if charset \"ISO8859-2\",
`iso-latin-2' is returned."
  (or (car (assoc-string charset locale-charset-alist t))
      (let ((cs coding-system-alist)
	    c)
	(while (and (not c) cs)
	  (if (locale-charset-match-p charset (caar cs))
	      (setq c (intern (caar cs)))
	    (pop cs)))
	(if c (coding-system-base c)))))

;; Fixme: This ought to deal with the territory part of the locale
;; too, for setting things such as calendar holidays, ps-print paper
;; size, spelling dictionary.

(defun locale-translate (locale)
  "Expand LOCALE according to `locale-translation-file-name', if possible.
For example, translate \"swedish\" into \"sv_SE.ISO8859-1\"."
  (if locale-translation-file-name
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents locale-translation-file-name)
        (if (re-search-forward
             (concat "^" (regexp-quote locale) ":?[ \t]+") nil t)
            (buffer-substring (point) (line-end-position))
          locale))
    locale))

(defun set-locale-environment (&optional locale-name frame)
  "Set up multi-lingual environment for using LOCALE-NAME.
This sets the language environment, the coding system priority,
the default input method and sometimes other things.

LOCALE-NAME should be a string which is the name of a locale supported
by the system.  Often it is of the form xx_XX.CODE, where xx is a
language, XX is a country, and CODE specifies a character set and
coding system.  For example, the locale name \"ja_JP.EUC\" might name
a locale for Japanese in Japan using the `japanese-iso-8bit'
coding-system.  The name may also have a modifier suffix, e.g. `@euro'
or `@cyrillic'.

If LOCALE-NAME is nil, its value is taken from the environment
variables LC_ALL, LC_CTYPE and LANG (the first one that is set).

The locale names supported by your system can typically be found in a
directory named `/usr/share/locale' or `/usr/lib/locale'.  LOCALE-NAME
will be translated according to the table specified by
`locale-translation-file-name'.

If FRAME is non-nil, only set the keyboard coding system and the
terminal coding system for the terminal of that frame, and don't
touch session-global parameters like the language environment.

See also `locale-charset-language-names', `locale-language-names',
`locale-preferred-coding-systems' and `locale-coding-system'."
  (interactive "sSet environment for locale: ")

  ;; Do this at runtime for the sake of binaries possibly transported
  ;; to a system without X.
  (setq locale-translation-file-name
	(let ((files
	       '("/usr/share/X11/locale/locale.alias" ; e.g. X11R7
		 "/usr/lib/X11/locale/locale.alias" ; e.g. X11R6.4
		 "/usr/X11R6/lib/X11/locale/locale.alias" ; XFree86, e.g. RedHat 4.2
		 "/usr/openwin/lib/locale/locale.alias" ; e.g. Solaris 2.6
		 ;;
		 ;; The following name appears after the X-related names above,
		 ;; since the X-related names are what X actually uses.
		 "/usr/share/locale/locale.alias" ; GNU/Linux sans X
		 )))
	  (while (and files (not (file-exists-p (car files))))
	    (setq files (cdr files)))
	  (car files)))

  (let ((locale locale-name))

    (unless locale
      ;; Use the first of these three environment variables
      ;; that has a nonempty value.
      (let ((vars '("LC_ALL" "LC_CTYPE" "LANG")))
	(while (and vars
		    (= 0 (length locale))) ; nil or empty string
	  (setq locale (getenv (pop vars) frame)))))

    (when locale
      (setq locale (locale-translate locale))

      ;; Leave the system locales alone if the caller did not specify
      ;; an explicit locale name, as their defaults are set from
      ;; LC_MESSAGES and LC_TIME, not LC_CTYPE, and the user might not
      ;; want to set them to the same value as LC_CTYPE.
      (when locale-name
	(setq system-messages-locale locale)
	(setq system-time-locale locale))

      (if (string-match "^[a-z][a-z]" locale)
	  (setq current-iso639-language (intern (match-string 0 locale)))))

    (setq woman-locale
          (or system-messages-locale
              (let ((msglocale (getenv "LC_MESSAGES" frame)))
                (if (zerop (length msglocale))
                    locale
                  (locale-translate msglocale)))))

    (when locale
      (setq locale (downcase locale))

      (let ((language-name
	     (locale-name-match locale locale-language-names))
	    (charset-language-name
	     (locale-name-match locale locale-charset-language-names))
	    (default-eol-type (coding-system-eol-type
			       (default-value 'buffer-file-coding-system)))
	    (coding-system
	     (or (locale-name-match locale locale-preferred-coding-systems)
		 (when locale
		   (if (string-match "\\.\\([^@]+\\)" locale)
		       (locale-charset-to-coding-system
			(match-string 1 locale)))))))

	(if (consp language-name)
	    ;; locale-language-names specify both lang-env and coding.
	    ;; But, what specified in locale-preferred-coding-systems
	    ;; has higher priority.
	    (setq coding-system (or coding-system
				    (nth 1 language-name))
		  language-name (car language-name))
	  ;; Otherwise, if locale is not listed in locale-language-names,
	  ;; use what listed in locale-charset-language-names.
	  (if (not language-name)
	      (setq language-name charset-language-name)))

	;; If a specific EOL conversion was specified in the default
	;; buffer-file-coding-system, preserve it in the coding system
	;; we will be using from now on.
	(if (and (memq default-eol-type '(0 1 2 unix dos mac))
		 coding-system
		 (coding-system-p coding-system))
	    (setq coding-system (coding-system-change-eol-conversion
				 coding-system default-eol-type)))

	(when language-name

	  ;; Set up for this character set.  This is now the right way
	  ;; to do it for both unibyte and multibyte modes.
	  (unless frame
	    (set-language-environment language-name))

	  ;; If the default enable-multibyte-characters is nil,
	  ;; we are using single-byte characters,
	  ;; so the display table and terminal coding system are irrelevant.
	  (when (default-value 'enable-multibyte-characters)
	    (set-display-table-and-terminal-coding-system
	     language-name coding-system frame))

	  ;; Set the `keyboard-coding-system' if appropriate (tty
	  ;; only).  At least X and MS Windows can generate
	  ;; multilingual input.
	  ;; XXX This was disabled unless `window-system', but that
	  ;; leads to buggy behavior when a tty frame is opened
	  ;; later.  Setting the keyboard coding system has no adverse
	  ;; effect on X, so let's do it anyway. -- Lorentey
	  (let ((kcs (or coding-system
			 (car (get-language-info language-name
						 'coding-system)))))
	    (if kcs (set-keyboard-coding-system kcs frame)))

	  (unless frame
	    (setq locale-coding-system
		  (car (get-language-info language-name 'coding-priority)))))

	(when (and (not frame)
		   coding-system
		   (not (coding-system-equal coding-system
					     locale-coding-system)))
	  (prefer-coding-system coding-system)
	  ;; Fixme: perhaps prefer-coding-system should set this too.
	  ;; But it's not the time to do such a fundamental change.
	  (setq default-sendmail-coding-system coding-system)
	  (setq locale-coding-system coding-system))))

    ;; On Windows, override locale-coding-system,
    ;; default-file-name-coding-system, keyboard-coding-system,
    ;; terminal-coding-system with system codepage.
    (when (boundp 'w32-ansi-code-page)
      (let ((code-page-coding (intern (format "cp%d" w32-ansi-code-page))))
	(when (coding-system-p code-page-coding)
	  (unless frame (setq locale-coding-system code-page-coding))
	  (set-keyboard-coding-system code-page-coding frame)
	  (set-terminal-coding-system code-page-coding frame)
	  ;; Set default-file-name-coding-system last, so that Emacs
	  ;; doesn't try to use cpNNNN when it defines keyboard and
	  ;; terminal encoding.  That's because the above two lines
	  ;; will want to load code-pages.el, where cpNNNN are
	  ;; defined; if default-file-name-coding-system were set to
	  ;; cpNNNN while these two lines run, Emacs will want to use
	  ;; it for encoding the file name it wants to load.  And that
	  ;; will fail, since cpNNNN is not yet usable until
	  ;; code-pages.el finishes loading.
	  (setq default-file-name-coding-system code-page-coding))))

    (when (eq system-type 'darwin)
      ;; On Darwin, file names are always encoded in utf-8, no matter
      ;; the locale.
      (setq default-file-name-coding-system 'utf-8)
      ;; Mac OS X's Terminal.app by default uses utf-8 regardless of
      ;; the locale.
      (when (and (null window-system)
		 (equal (getenv "TERM_PROGRAM" frame) "Apple_Terminal"))
	(set-terminal-coding-system 'utf-8)
	(set-keyboard-coding-system 'utf-8)))

    ;; Default to A4 paper if we're not in a C, POSIX or US locale.
    ;; (See comments in Flocale_info.)
    (unless frame
      (let ((locale locale)
	    (paper (locale-info 'paper)))
	(if paper
	    ;; This will always be null at the time of writing.
	    (cond
	     ((equal paper '(216 279))
	      (setq ps-paper-type 'letter))
	     ((equal paper '(210 297))
	      (setq ps-paper-type 'a4)))
	  (let ((vars '("LC_ALL" "LC_PAPER" "LANG")))
	    (while (and vars (= 0 (length locale)))
	      (setq locale (getenv (pop vars) frame))))
	  (when locale
	    ;; As of glibc 2.2.5, these are the only US Letter locales,
	    ;; and the rest are A4.
	    (setq ps-paper-type
		  (or (locale-name-match locale '(("c$" . letter)
						  ("posix$" . letter)
						  (".._us" . letter)
						  (".._pr" . letter)
						  (".._ca" . letter)
						  ("enu$" . letter) ; Windows
						  ("esu$" . letter)
						  ("enc$" . letter)
						  ("frc$" . letter)))
		      'a4)))))))
  nil)

;;; Character property

(put 'char-code-property-table 'char-table-extra-slots 5)

(defun define-char-code-property (name table &optional docstring)
  "Define NAME as a character code property given by TABLE.
TABLE is a char-table of purpose `char-code-property-table' with
these extra slots:
  1st: NAME.
  2nd: Function to call to get a property value of a character.
    It is called with three arguments CHAR, VAL, and TABLE, where
    CHAR is a character, VAL is the value of (aref TABLE CHAR).
  3rd: Function to call to put a property value of a character.
    It is called with the same arguments as above.
  4th: Function to call to get a description string of a property value.
    It is called with one argument VALUE, a property value.
  5th: Data used by the above functions.

TABLE may be a name of file to load to build a char-table.  The
file should contain a call of `define-char-code-property' with a
char-table of the above format as the argument TABLE.

TABLE may also be nil, in which case no property value is pre-assigned.

Optional 3rd argument DOCSTRING is a documentation string of the property.

See also the documentation of `get-char-code-property' and
`put-char-code-property'."
  (or (symbolp name)
      (error "Not a symbol: %s" name))
  (if (char-table-p table)
      (or (and (eq (char-table-subtype table) 'char-code-property-table)
	       (eq (char-table-extra-slot table 0) name))
	  (error "Invalid char-table: %s" table))
    (or (stringp table)
	(error "Not a char-table nor a file name: %s" table)))
  (if (stringp table) (setq table (purecopy table)))
  (let ((slot (assq name char-code-property-alist)))
    (if slot
	(setcdr slot table)
      (setq char-code-property-alist
	    (cons (cons name table) char-code-property-alist))))
  (put name 'char-code-property-documentation (purecopy docstring)))

(defvar char-code-property-table
  (make-char-table 'char-code-property-table)
  "Char-table containing a property list of each character code.
This table is used for properties not listed in `char-code-property-alist'.
See also the documentation of `get-char-code-property' and
`put-char-code-property'.")

(defun get-char-code-property (char propname)
  "Return the value of CHAR's PROPNAME property."
  (let ((table (unicode-property-table-internal propname)))
    (if table
	(let ((func (char-table-extra-slot table 1)))
	  (if (functionp func)
	      (funcall func char (aref table char) table)
	    (get-unicode-property-internal table char)))
      (plist-get (aref char-code-property-table char) propname))))

(defun put-char-code-property (char propname value)
  "Store CHAR's PROPNAME property with VALUE.
It can be retrieved with `(get-char-code-property CHAR PROPNAME)'."
  (let ((table (unicode-property-table-internal propname)))
    (if table
	(let ((func (char-table-extra-slot table 2)))
	  (if (functionp func)
	      (funcall func char value table)
	    (put-unicode-property-internal table char value)))
      (let* ((plist (aref char-code-property-table char))
	     (x (plist-put plist propname value)))
	(or (eq x plist)
	    (aset char-code-property-table char x))))
    value))

(defun char-code-property-description (prop value)
  "Return a description string of character property PROP's value VALUE.
If there's no description string for VALUE, return nil."
  (let ((table (unicode-property-table-internal prop)))
    (if table
	(let ((func (char-table-extra-slot table 3)))
	  (if (functionp func)
	      (funcall func value))))))


;; Pretty description of encoded string

;; Alist of ISO 2022 control code vs the corresponding mnemonic string.
(defconst iso-2022-control-alist
  '((?\x1b . "ESC")
    (?\x0e . "SO")
    (?\x0f . "SI")
    (?\x8e . "SS2")
    (?\x8f . "SS3")
    (?\x9b . "CSI")))

(defun encoded-string-description (str coding-system)
  "Return a pretty description of STR that is encoded by CODING-SYSTEM."
  (setq str (string-as-unibyte str))
  (mapconcat
   (if (and coding-system (eq (coding-system-type coding-system) 'iso-2022))
       ;; Try to get a pretty description for ISO 2022 escape sequences.
       (function (lambda (x) (or (cdr (assq x iso-2022-control-alist))
				 (format "#x%02X" x))))
     (function (lambda (x) (format "#x%02X" x))))
   str " "))

(defun encode-coding-char (char coding-system &optional charset)
  "Encode CHAR by CODING-SYSTEM and return the resulting string.
If CODING-SYSTEM can't safely encode CHAR, return nil.
The 3rd optional argument CHARSET, if non-nil, is a charset preferred
on encoding."
  (let* ((str1 (string-as-multibyte (string char)))
	 (str2 (string-as-multibyte (string char char)))
	 (found (find-coding-systems-string str1))
	enc1 enc2 i1 i2)
    (if (and (consp found)
	     (eq (car found) 'undecided))
	str1
      (when (memq (coding-system-base coding-system) found)
	;; We must find the encoded string of CHAR.  But, just encoding
	;; CHAR will put extra control sequences (usually to designate
	;; ASCII charset) at the tail if type of CODING is ISO 2022.
	;; To exclude such tailing bytes, we at first encode one-char
	;; string and two-char string, then check how many bytes at the
	;; tail of both encoded strings are the same.

	(when charset
	  (put-text-property 0 1 'charset charset str1)
	  (put-text-property 0 2 'charset charset str2))
	(setq enc1 (encode-coding-string str1 coding-system)
	      i1 (length enc1)
	      enc2 (encode-coding-string str2 coding-system)
	      i2 (length enc2))
	(while (and (> i1 0) (= (aref enc1 (1- i1)) (aref enc2 (1- i2))))
	  (setq i1 (1- i1) i2 (1- i2)))

	;; Now (substring enc1 i1) and (substring enc2 i2) are the same,
	;; and they are the extra control sequences at the tail to
	;; exclude.
	(substring enc2 0 i2)))))

;; Backwards compatibility.  These might be better with :init-value t,
;; but that breaks loadup.
(define-minor-mode unify-8859-on-encoding-mode
  "Exists only for backwards compatibility."
  :group 'mule
  :global t)
;; Doc said "obsolete" in 23.1, this statement only added in 24.1.
(make-obsolete 'unify-8859-on-encoding-mode "don't use it." "23.1")

(define-minor-mode unify-8859-on-decoding-mode
  "Exists only for backwards compatibility."
  :group 'mule
  :global t)
;; Doc said "obsolete" in 23.1, this statement only added in 24.1.
(make-obsolete 'unify-8859-on-decoding-mode "don't use it." "23.1")

(defvar nonascii-insert-offset 0)
(make-obsolete-variable 'nonascii-insert-offset "do not use it." "23.1")
(defvar nonascii-translation-table nil)
(make-obsolete-variable 'nonascii-translation-table "do not use it." "23.1")

(defvar ucs-names nil
  "Alist of cached (CHAR-NAME . CHAR-CODE) pairs.")

(defun ucs-names ()
  "Return alist of (CHAR-NAME . CHAR-CODE) pairs cached in `ucs-names'."
  (or ucs-names
      (let ((bmp-ranges
	     '((#x0000 . #x33FF)
	       ;; (#x3400 . #x4DBF) CJK Ideographs Extension A
	       (#x4DC0 . #x4DFF)
	       ;; (#x4E00 . #x9FFF) CJK Unified Ideographs
	       (#xA000 . #xD7FF)
	       ;; (#xD800 . #xFAFF) Surrogate/Private
	       (#xFB00 . #xFFFD)))
	    (upper-ranges
	     '((#x10000 . #x134FF)
	       ;; (#x13500 . #x167FF) unused
	       (#x16800 . #x16A3F)
	       ;; (#x16A40 . #x1AFFF) unused
	       (#x1B000 . #x1B0FF)
	       ;; (#x1B100 . #x1CFFF) unused
	       (#x1D000 . #x1FFFF)
	       ;; (#x20000 . #xDFFFF) CJK Ideograph Extension A, B, etc, unused
	       (#xE0000 . #xE01FF)))
	    (gc-cons-threshold 10000000)
	    c end name names)
        (dolist (range bmp-ranges)
          (setq c (car range)
                end (cdr range))
	  (while (<= c end)
	    (if (setq name (get-char-code-property c 'name))
		(push (cons name c) names))
	    (if (setq name (get-char-code-property c 'old-name))
		(push (cons name c) names))
	    (setq c (1+ c))))
        (dolist (range upper-ranges)
          (setq c (car range)
                end (cdr range))
	  (while (<= c end)
	    (if (setq name (get-char-code-property c 'name))
		(push (cons name c) names))
	    (setq c (1+ c))))
        (setq ucs-names names))))

(defun read-char-by-name (prompt)
  "Read a character by its Unicode name or hex number string.
Display PROMPT and read a string that represents a character by its
Unicode property `name' or `old-name'.

This function returns the character as a number.

You can type a few of the first letters of the Unicode name and
use completion.  If you type a substring of the Unicode name
preceded by an asterisk `*' and use completion, it will show all
the characters whose names include that substring, not necessarily
at the beginning of the name.

This function also accepts a hexadecimal number of Unicode code
point or a number in hash notation, e.g. #o21430 for octal,
#x2318 for hex, or #10r8984 for decimal."
  (let* ((completion-ignore-case t)
	 (input (completing-read
                 prompt
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata (category . unicode-name))
                     (complete-with-action action (ucs-names) string pred))))))
    (cond
     ((string-match-p "\\`[0-9a-fA-F]+\\'" input)
      (string-to-number input 16))
     ((string-match-p "\\`#" input)
      (read input))
     (t
      (cdr (assoc-string input (ucs-names) t))))))

(defun ucs-insert (character &optional count inherit)
  "Insert COUNT copies of CHARACTER of the given Unicode code point.
Interactively, prompts for a Unicode character name or a hex number
using `read-char-by-name'.

You can type a few of the first letters of the Unicode name and
use completion.  If you type a substring of the Unicode name
preceded by an asterisk `*' and use completion, it will show all
the characters whose names include that substring, not necessarily
at the beginning of the name.

This function also accepts a hexadecimal number of Unicode code
point or a number in hash notation, e.g. #o21430 for octal,
#x2318 for hex, or #10r8984 for decimal.

The optional third arg INHERIT (non-nil when called interactively),
says to inherit text properties from adjoining text, if those
properties are sticky."
  (interactive
   (list (read-char-by-name "Unicode (name or hex): ")
	 (prefix-numeric-value current-prefix-arg)
	 t))
  (unless count (setq count 1))
  (if (and (stringp character)
	   (string-match-p "\\`[0-9a-fA-F]+\\'" character))
      (setq character (string-to-number character 16)))
  (cond
   ((null character)
    (error "Not a Unicode character"))
   ((not (integerp character))
    (error "Not a Unicode character code: %S" character))
   ((or (< character 0) (> character #x10FFFF))
    (error "Not a Unicode character code: 0x%X" character)))
  (if inherit
      (dotimes (i count) (insert-and-inherit character))
    (dotimes (i count) (insert character))))

(define-key ctl-x-map "8\r" 'ucs-insert)

;;; mule-cmds.el ends here
