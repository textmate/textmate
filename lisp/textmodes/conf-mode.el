;;; conf-mode.el --- Simple major mode for editing conf/ini/properties files

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: conf ini windows java

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
;; This mode is designed to edit many similar varieties of Conf/Ini files and
;; Java properties.  It started out from Aurélien Tisné's ini-mode.
;; `conf-space-keywords' were inspired by Robert Fitzgerald's any-ini-mode.


;;; Code:

(require 'newcomment)

(defvar outline-heading-end-regexp)

;; Variables:

(defgroup conf nil
  "Configuration files."
  :group 'data
  :version "22.1")

(defcustom conf-assignment-column 24
  "Align assignments to this column by default with \\[conf-align-assignments].
If this number is negative, the `=' comes before the whitespace.  Use 0 to
not align (only setting space according to `conf-assignment-space')."
  :type 'integer
  :group 'conf)

(defcustom conf-javaprop-assignment-column 32
  "Value for `conf-assignment-column' in Java properties buffers."
  :type 'integer
  :group 'conf)

(defcustom conf-colon-assignment-column (- (abs conf-assignment-column))
  "Value for `conf-assignment-column' in Java properties buffers."
  :type 'integer
  :group 'conf)

(defcustom conf-assignment-space t
  "Put at least one space around assignments when aligning."
  :type 'boolean
  :group 'conf)

(defcustom conf-colon-assignment-space nil
  "Value for `conf-assignment-space' in colon style Conf mode buffers."
  :type 'boolean
  :group 'conf)

(defvar conf-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-u" 'conf-unix-mode)
    (define-key map "\C-c\C-w" 'conf-windows-mode)
    (define-key map "\C-c\C-j" 'conf-javaprop-mode)
    (define-key map "\C-c\C-s" 'conf-space-keywords)
    (define-key map "\C-c " 'conf-space-keywords)
    (define-key map "\C-c\C-c" 'conf-colon-mode)
    (define-key map "\C-c:" 'conf-colon-mode)
    (define-key map "\C-c\C-x" 'conf-xdefaults-mode)
    (define-key map "\C-c\C-p" 'conf-ppd-mode)
    (define-key map "\C-c\C-q" 'conf-quote-normal)
    (define-key map "\C-c\"" 'conf-quote-normal)
    (define-key map "\C-c'" 'conf-quote-normal)
    (define-key map "\C-c\C-a" 'conf-align-assignments)
    (define-key map [menu-bar sh-script] (cons "Conf" menu-map))
    (define-key menu-map [conf-windows-mode]
      '(menu-item "Windows mode"
		  conf-windows-mode
		  :help "Conf Mode starter for Windows style Conf files"
		  :button (:radio . (eq major-mode 'conf-windows-mode))))
    (define-key menu-map [conf-javaprop-mode]
      '(menu-item "Java properties mode"
		  conf-javaprop-mode
		  :help "Conf Mode starter for Java properties files"
		  :button (:radio . (eq major-mode 'conf-javaprop-mode))))
    (define-key menu-map [conf-space-keywords]
      '(menu-item "Space keywords mode..."
		  conf-space-keywords
		  :help "Enter Conf Space mode using regexp KEYWORDS to match the keywords"
		  :button (:radio . (eq major-mode 'conf-space-keywords))))
    (define-key menu-map [conf-ppd-mode]
      '(menu-item "PPD mode"
		  conf-ppd-mode
		  :help "Conf Mode starter for Adobe/CUPS PPD files"
		  :button (:radio . (eq major-mode 'conf-ppd-mode))))
    (define-key menu-map [conf-colon-mode]
      '(menu-item "Colon mode"
		  conf-colon-mode
		  :help "Conf Mode starter for Colon files"
		  :button (:radio . (eq major-mode 'conf-colon-mode))))
    (define-key menu-map [conf-unix-mode]
      '(menu-item "Unix mode"
		  conf-unix-mode
		  :help "Conf Mode starter for Unix style Conf files"
		  :button (:radio . (eq major-mode 'conf-unix-mode))))
    (define-key menu-map [conf-xdefaults-mode]
      '(menu-item "Xdefaults mode"
		  conf-xdefaults-mode
		  :help "Conf Mode starter for Xdefaults files"
		  :button (:radio . (eq major-mode 'conf-xdefaults-mode))))
    (define-key menu-map [c-s0] '("--"))
    (define-key menu-map [conf-quote-normal]
      '(menu-item "Set quote syntax normal" conf-quote-normal
		  :help "Set the syntax of \' and \" to punctuation"))
    (define-key menu-map [conf-align-assignments]
      '(menu-item "Align assignments" conf-align-assignments
		  :help "Align assignments"))
    map)
  "Local keymap for `conf-mode' buffers.")

(defvar conf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?.  "_" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table in use in Windows style `conf-mode' buffers.")

(defvar conf-unix-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    ;; override
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use in Unix style `conf-mode' buffers.")

(defvar conf-javaprop-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?/  ". 124" table)
    (modify-syntax-entry ?*  ". 23b" table)
    table)
  "Syntax table in use in Java properties buffers.")

(defvar conf-ppd-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?*  ". 1" table)
    (modify-syntax-entry ?%  ". 2" table)
    ;; override
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use in PPD `conf-mode' buffers.")

(defvar conf-xdefaults-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?!  "<" table)
    ;; override
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use in Xdefaults style `conf-mode' buffers.")


(defvar conf-font-lock-keywords
  '(;; [section] (do this first because it may look like a parameter)
    ("^[ \t]*\\[\\(.+\\)\\]" 1 'font-lock-type-face)
    ;; var=val or var[index]=val
    ("^[ \t]*\\(.+?\\)\\(?:\\[\\(.*?\\)\\]\\)?[ \t]*="
     (1 'font-lock-variable-name-face)
     (2 'font-lock-constant-face nil t))
    ;; section { ... } (do this last because some assign ...{...)
    ("^[ \t]*\\([^=:\n]+?\\)[ \t\n]*{[^{}]*?$" 1 'font-lock-type-face prepend))
  "Keywords to highlight in Conf mode.")

(defvar conf-javaprop-font-lock-keywords
  '(;; var=val
    ("^[ \t]*\\(.+?\\)\\(?:\\.\\([0-9]+\\)\\(?:\\.\\(.+?\\)\\(?:\\.\\([0-9]+\\)\\(?:\\.\\(.+?\\)\\(?:\\.\\([0-9]+\\)\\(\\..+?\\)?\\)?\\)?\\)?\\)?\\)?\\([:= \t]\\|$\\)"
     (1 'font-lock-variable-name-face)
     (2 'font-lock-constant-face nil t)
     (3 'font-lock-variable-name-face nil t)
     (4 'font-lock-constant-face nil t)
     (5 'font-lock-variable-name-face nil t)
     (6 'font-lock-constant-face nil t)
     (7 'font-lock-variable-name-face nil t)))
  "Keywords to highlight in Conf Java Properties mode.")

(defvar conf-space-keywords-alist
  '(("\\`/etc/gpm/" . "key\\|name\\|foreground\\|background\\|border\\|head")
    ("\\`/etc/magic\\'" . "[^ \t]+[ \t]+\\(?:[bl]?e?\\(?:short\\|long\\)\\|byte\\|string\\)[^ \t]*")
    ("/mod\\(?:ules\\|probe\\)\\.conf" . "alias\\|in\\(?:clude\\|stall\\)\\|options\\|remove")
    ("/manpath\\.config" . "MAN\\(?:DATORY_MANPATH\\|PATH_MAP\\|DB_MAP\\)")
    ("/sensors\\.conf" . "chip\\|bus\\|label\\|compute\\|set\\|ignore")
    ("/sane\\(\\.d\\)?/" . "option\\|device\\|port\\|usb\\|sc\\(?:si\\|anner\\)")
    ("/resmgr\\.conf" . "class\\|add\\|allow\\|deny")
    ("/dictionary\\.lst\\'" . "DICT\\|HYPH\\|THES")
    ("/tuxracer/options" . "set"))
  "File-name-based settings for the variable `conf-space-keywords'.")

(defvar conf-space-keywords nil
  "Regexps for functions that may come before a space assignment.
This allows constructs such as
keyword var value
This variable is best set in the file local variables, or through
`conf-space-keywords-alist'.")
(put 'conf-space-keywords 'safe-local-variable 'stringp)

(defvar conf-space-font-lock-keywords
  `(;; [section] (do this first because it may look like a parameter)
    ("^[ \t]*\\[\\(.+\\)\\]" 1 'font-lock-type-face)
    ;; section { ... } (do this first because it looks like a parameter)
    ("^[ \t]*\\(.+?\\)[ \t\n]*{[^{}]*?$" 1 'font-lock-type-face)
    ;; var val
    (eval if conf-space-keywords
	  (list (concat "^[ \t]*\\(" conf-space-keywords "\\)[ \t]+\\([^\000- ]+\\)")
		'(1 'font-lock-keyword-face)
		'(2 'font-lock-variable-name-face))
	  '("^[ \t]*\\([^\000- ]+\\)" 1 'font-lock-variable-name-face)))
  "Keywords to highlight in Conf Space mode.")

(defvar conf-colon-font-lock-keywords
  `(;; [section] (do this first because it may look like a parameter)
    ("^[ \t]*\\[\\(.+\\)\\]" 1 'font-lock-type-face)
    ;; var: val
    ("^[ \t]*\\(.+?\\)[ \t]*:"
     (1 'font-lock-variable-name-face))
    ;; section { ... } (do this last because some assign ...{...)
    ("^[ \t]*\\([^:\n]+\\)[ \t\n]*{[^{}]*?$" 1 'font-lock-type-face prepend))
  "Keywords to highlight in Conf Colon mode.")

(defvar conf-assignment-sign ?=
  "Sign used for assignments (char or string).")

(defvar conf-assignment-regexp ".+?\\([ \t]*=[ \t]*\\)"
  "Regexp to recognize assignments.
It is anchored after the first sexp on a line.  There must be a
grouping for the assignment sign, including leading and trailing
whitespace.")


;; If anybody can figure out how to get the same effect by configuring
;; `align', I'd be glad to hear.
(defun conf-align-assignments (&optional arg)
  (interactive "P")
  "Align the assignments in the buffer or active region.
In Transient Mark mode, if the mark is active, operate on the
contents of the region.  Otherwise, operate on the whole buffer."
  (setq arg (if arg
		(prefix-numeric-value arg)
	      conf-assignment-column))
  (save-excursion
    (save-restriction
      (when (use-region-p)
	(narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((cs (comment-beginning)))	; go before comment if within
	  (if cs (goto-char cs)))
	(while (forward-comment 9))	; max-int?
	(when (and (not (eobp))
		   (looking-at conf-assignment-regexp))
	  (goto-char (match-beginning 1))
	  (delete-region (point) (match-end 1))
	  (if conf-assignment-sign
	      (if (>= arg 0)
		  (progn
		    (indent-to-column arg)
		    (or (not conf-assignment-space)
			(memq (char-before (point)) '(?\s ?\t)) (insert ?\s))
		    (insert conf-assignment-sign
			    (if (and conf-assignment-space (not (eolp))) ?\s "")))
		(insert (if conf-assignment-space ?\s "") conf-assignment-sign)
		(unless (eolp)
		  (indent-to-column (- arg))
		  (or (not conf-assignment-space)
		      (memq (char-before (point)) '(?\s ?\t)) (insert ?\s))))
	    (unless (eolp)
	      (if (>= (current-column) (abs arg))
		  (insert ?\s)
		(indent-to-column (abs arg))))))
	(forward-line)))))


(defun conf-quote-normal (arg)
  "Set the syntax of ' and \" to punctuation.
With prefix arg, only do it for ' if 1, or only for \" if 2.
This only affects the current buffer.  Some conf files use quotes
to delimit strings, while others allow quotes as simple parts of
the assigned value.  In those files font locking will be wrong,
and you can correct it with this command.  (Some files even do
both, i.e. quotes delimit strings, except when they are
unbalanced, but hey...)"
  (interactive "P")
  (let ((table (copy-syntax-table (syntax-table))))
    (when (or (not arg) (= (prefix-numeric-value arg) 1))
      (modify-syntax-entry ?\' "." table))
    (when (or (not arg) (= (prefix-numeric-value arg) 2))
      (modify-syntax-entry ?\" "." table))
    (set-syntax-table table)
    (when font-lock-mode
      (font-lock-fontify-buffer))))


(defun conf-outline-level ()
  (let ((depth 0)
	(pt (match-end 0)))
    (condition-case nil
	(while (setq pt (scan-lists pt -1 1)
		     depth (1+ depth)))
      (scan-error depth))))



;;;###autoload
(defun conf-mode ()
  "Mode for Unix and Windows Conf files and Java properties.
Most conf files know only three kinds of constructs: parameter
assignments optionally grouped into sections and comments.  Yet
there is a great range of variation in the exact syntax of conf
files.  See below for various wrapper commands that set up the
details for some of the most widespread variants.

This mode sets up font locking, outline, imenu and it provides
alignment support through `conf-align-assignments'.  If strings
come out wrong, try `conf-quote-normal'.

Some files allow continuation lines, either with a backslash at
the end of line, or by indenting the next line (further).  These
constructs cannot currently be recognized.

Because of this great variety of nuances, which are often not
even clearly specified, please don't expect it to get every file
quite right.  Patches that clearly identify some special case,
without breaking the general ones, are welcome.

If instead you start this mode with the generic `conf-mode'
command, it will parse the buffer.  It will generally well
identify the first four cases listed below.  If the buffer
doesn't have enough contents to decide, this is identical to
`conf-windows-mode' on Windows, elsewhere to `conf-unix-mode'.
See also `conf-space-mode', `conf-colon-mode', `conf-javaprop-mode',
`conf-ppd-mode' and `conf-xdefaults-mode'.

\\{conf-mode-map}"

  (interactive)
  ;; `conf-mode' plays two roles: it's the parent of several sub-modes
  ;; but it's also the function that chooses between those submodes.
  ;; To tell the difference between those two cases where the function
  ;; might be called, we check `delay-mode-hooks'.
  ;; (adopted from tex-mode.el)
  (if (not delay-mode-hooks)
      ;; try to guess sub-mode of conf-mode based on buffer content
      (let ((unix 0) (win 0) (equal 0) (colon 0) (space 0) (jp 0))
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (skip-chars-forward " \t\f")
	    (cond ((eq (char-after) ?\#) (setq unix (1+ unix)))
		  ((eq (char-after) ?\;) (setq win (1+ win)))
		  ((eq (char-after) ?\[))	; nop
		  ((eolp))			; nop
		  ((eq (char-after) ?}))	; nop
		  ;; recognize at most double spaces within names
		  ((looking-at "[^ \t\n=:]+\\(?:  ?[^ \t\n=:]+\\)*[ \t]*[=:]")
		   (if (eq (char-before (match-end 0)) ?=)
		       (setq equal (1+ equal))
		     (setq colon (1+ colon))))
		  ((looking-at "/[/*]") (setq jp (1+ jp)))
		  ((looking-at ".*{"))		; nop
		  ((setq space (1+ space))))
	    (forward-line)))
	(cond
         ((> jp (max unix win 3)) (conf-javaprop-mode))
         ((> colon (max equal space)) (conf-colon-mode))
         ((> space (max equal colon)) (conf-space-mode))
         ((or (> win unix) (and (= win unix) (eq system-type 'windows-nt)))
          (conf-windows-mode))
         (t (conf-unix-mode))))

    (kill-all-local-variables)
    (use-local-map conf-mode-map)
    (setq major-mode 'conf-mode
	  mode-name "Conf[?]")
    (set (make-local-variable 'font-lock-defaults)
         '(conf-font-lock-keywords nil t nil nil))
    ;; Let newcomment.el decide this for itself.
    ;; (set (make-local-variable 'comment-use-syntax) t)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    (set (make-local-variable 'outline-regexp)
	 "[ \t]*\\(?:\\[\\|.+[ \t\n]*{\\)")
    (set (make-local-variable 'outline-heading-end-regexp)
	 "[\n}]")
    (set (make-local-variable 'outline-level)
	 'conf-outline-level)
    (set-syntax-table conf-mode-syntax-table)
    (setq imenu-generic-expression
	  '(("Parameters" "^[ \t]*\\(.+?\\)[ \t]*=" 1)
	    ;; [section]
	    (nil "^[ \t]*\\[[ \t]*\\(.+\\)[ \t]*\\]" 1)
	    ;; section { ... }
	    (nil "^[ \t]*\\([^=:{} \t\n][^=:{}\n]+\\)[ \t\n]*{" 1)))
    (run-mode-hooks 'conf-mode-hook)))

(defun conf-mode-initialize (comment &optional font-lock)
  "Initializations for sub-modes of conf-mode.
COMMENT initializes `comment-start' and `comment-start-skip'.
The optional arg FONT-LOCK is the value for FONT-LOCK-KEYWORDS."
  (set (make-local-variable 'comment-start) comment)
  (set (make-local-variable 'comment-start-skip)
       (concat (regexp-quote comment-start) "+\\s *"))
  (if font-lock
      (set (make-local-variable 'font-lock-defaults)
           `(,font-lock nil t nil nil))))

;;;###autoload
(define-derived-mode conf-unix-mode conf-mode "Conf[Unix]"
  "Conf Mode starter for Unix style Conf files.
Comments start with `#'.
For details see `conf-mode'.  Example:

# Conf mode font-locks this right on Unix and with \\[conf-unix-mode]

\[Desktop Entry]
	 Encoding=UTF-8
	 Name=The GIMP
	 Name[ca]=El GIMP
	 Name[cs]=GIMP"
  (conf-mode-initialize "#"))

;;;###autoload
(define-derived-mode conf-windows-mode conf-mode "Conf[WinIni]"
  "Conf Mode starter for Windows style Conf files.
Comments start with `;'.
For details see `conf-mode'.  Example:

; Conf mode font-locks this right on Windows and with \\[conf-windows-mode]

\[ExtShellFolderViews]
Default={5984FFE0-28D4-11CF-AE66-08002B2E1262}
{5984FFE0-28D4-11CF-AE66-08002B2E1262}={5984FFE0-28D4-11CF-AE66-08002B2E1262}

\[{5984FFE0-28D4-11CF-AE66-08002B2E1262}]
PersistMoniker=file://Folder.htt"
  (conf-mode-initialize ";"))

;; Here are a few more or less widespread styles.  There are others, so
;; obscure, they are not covered.  E.g. RFC 2614 allows both Unix and Windows
;; comments.  Or the donkey has (* Pascal comments *) -- roll your own starter
;; if you need it.

;;;###autoload
(define-derived-mode conf-javaprop-mode conf-mode "Conf[JavaProp]"
  "Conf Mode starter for Java properties files.
Comments start with `#' but are also recognized with `//' or
between `/*' and `*/'.
For details see `conf-mode'.  Example:

# Conf mode font-locks this right with \\[conf-javaprop-mode] (Java properties)
// another kind of comment
/* yet another */

name:value
name=value
name value
x.1 =
x.2.y.1.z.1 =
x.2.y.1.z.2.zz ="
  (conf-mode-initialize "#" 'conf-javaprop-font-lock-keywords)
  (set (make-local-variable 'conf-assignment-column)
       conf-javaprop-assignment-column)
  (set (make-local-variable 'conf-assignment-regexp)
       ".+?\\([ \t]*[=: \t][ \t]*\\|$\\)")
  (setq comment-start-skip "\\(?:#+\\|/[/*]+\\)\\s *")
  (setq imenu-generic-expression
	'(("Parameters" "^[ \t]*\\(.+?\\)[=: \t]" 1))))

;;;###autoload
(define-derived-mode conf-space-mode conf-unix-mode "Conf[Space]"
  "Conf Mode starter for space separated conf files.
\"Assignments\" are with ` '.  Keywords before the parameters are
recognized according to the variable `conf-space-keywords-alist'.
Alternatively, you can specify a value for the file local variable
`conf-space-keywords'.
Use the function `conf-space-keywords' if you want to specify keywords
in an interactive fashion instead.

For details see `conf-mode'.  Example:

# Conf mode font-locks this right with \\[conf-space-mode] (space separated)

image/jpeg			jpeg jpg jpe
image/png			png
image/tiff			tiff tif

# Or with keywords (from a recognized file name):
class desktop
# Standard multimedia devices
add /dev/audio		desktop
add /dev/mixer		desktop"
  (conf-mode-initialize "#" 'conf-space-font-lock-keywords)
  (make-local-variable 'conf-assignment-sign)
  (setq conf-assignment-sign nil)
  (make-local-variable 'conf-space-keywords)
  (cond (buffer-file-name
	 ;; We set conf-space-keywords directly, but a value which is
	 ;; in the local variables list or interactively specified
	 ;; (see the function conf-space-keywords) takes precedence.
         (setq conf-space-keywords
	       (assoc-default buffer-file-name conf-space-keywords-alist
			      'string-match))))
  (conf-space-mode-internal)
  ;; In case the local variables list specifies conf-space-keywords,
  ;; recompute other things from that afterward.
  (add-hook 'hack-local-variables-hook 'conf-space-mode-internal nil t))

;;;###autoload
(defun conf-space-keywords (keywords)
  "Enter Conf Space mode using regexp KEYWORDS to match the keywords.
See `conf-space-mode'."
  (interactive "sConf Space keyword regexp: ")
  (delay-mode-hooks
    (conf-space-mode))
  (if (string-equal keywords "")
      (setq keywords nil))
  (setq conf-space-keywords keywords)
  (conf-space-mode-internal)
  (run-mode-hooks))

(defun conf-space-mode-internal ()
  (make-local-variable 'conf-assignment-regexp)
  (setq conf-assignment-regexp
	(if conf-space-keywords
	    (concat "\\(?:" conf-space-keywords "\\)[ \t]+.+?\\([ \t]+\\|$\\)")
	  ".+?\\([ \t]+\\|$\\)"))
  ;; If Font Lock is already enabled, reenable it with new
  ;; conf-assignment-regexp.
  (when (and font-lock-mode
	     (boundp 'font-lock-keywords)) ;see `normal-mode'
    (font-lock-add-keywords nil nil)
    (font-lock-mode 1))
  ;; Copy so that we don't destroy shared structure.
  (setq imenu-generic-expression (copy-sequence imenu-generic-expression))
  ;; Get rid of any existing Parameters element.
  (setq imenu-generic-expression
	(delq (assoc "Parameters" imenu-generic-expression)
	      imenu-generic-expression))
  ;; Add a new one based on conf-space-keywords.
  (setq imenu-generic-expression
	(cons `("Parameters"
		,(if conf-space-keywords
		     (concat "^[ \t]*\\(?:" conf-space-keywords
			     "\\)[ \t]+\\([^ \t\n]+\\)\\(?:[ \t]\\|$\\)")
		   "^[ \t]*\\([^ \t\n[]+\\)\\(?:[ \t]\\|$\\)")
		1)
	      imenu-generic-expression)))

;;;###autoload
(define-derived-mode conf-colon-mode conf-unix-mode "Conf[Colon]"
  "Conf Mode starter for Colon files.
\"Assignments\" are with `:'.
For details see `conf-mode'.  Example:

# Conf mode font-locks this right with \\[conf-colon-mode] (colon)

<Multi_key> <exclam> <exclam>		: \"\\241\"	exclamdown
<Multi_key> <c> <slash>			: \"\\242\"	cent"
  (conf-mode-initialize "#" 'conf-colon-font-lock-keywords)
  (set (make-local-variable 'conf-assignment-space)
       conf-colon-assignment-space)
  (set (make-local-variable 'conf-assignment-column)
       conf-colon-assignment-column)
  (set (make-local-variable 'conf-assignment-sign)
       ?:)
  (set (make-local-variable 'conf-assignment-regexp)
       ".+?\\([ \t]*:[ \t]*\\)")
  (setq imenu-generic-expression
	`(("Parameters" "^[ \t]*\\(.+?\\)[ \t]*:" 1)
	  ,@(cdr imenu-generic-expression))))

;;;###autoload
(define-derived-mode conf-ppd-mode conf-colon-mode "Conf[PPD]"
  "Conf Mode starter for Adobe/CUPS PPD files.
Comments start with `*%' and \"assignments\" are with `:'.
For details see `conf-mode'.  Example:

*% Conf mode font-locks this right with \\[conf-ppd-mode] (PPD)

*DefaultTransfer: Null
*Transfer Null.Inverse: \"{ 1 exch sub }\""
  (conf-mode-initialize "*%")
  ;; no sections, they match within PostScript code
  (setq imenu-generic-expression (list (car imenu-generic-expression))))

;;;###autoload
(define-derived-mode conf-xdefaults-mode conf-colon-mode "Conf[Xdefaults]"
  "Conf Mode starter for Xdefaults files.
Comments start with `!' and \"assignments\" are with `:'.
For details see `conf-mode'.  Example:

! Conf mode font-locks this right with \\[conf-xdefaults-mode] (.Xdefaults)

*background:			gray99
*foreground:			black"
  (conf-mode-initialize "!"))

(provide 'conf-mode)

;;; conf-mode.el ends here
