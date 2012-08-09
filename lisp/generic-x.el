;;; generic-x.el --- A collection of generic modes

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Author:  Peter Breton <pbreton@cs.umb.edu>
;; Created: Tue Oct 08 1996
;; Keywords: generic, comment, font-lock
;; Package: emacs

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
;; This file contains a collection of generic modes.
;;
;; INSTALLATION:
;;
;; Add this line to your .emacs file:
;;
;;   (require 'generic-x)
;;
;; You can decide which modes to load by setting the variable
;; `generic-extras-enable-list'.  Its default value is platform-
;; specific.  The recommended way to set this variable is through
;; customize:
;;
;;   M-x customize-option RET generic-extras-enable-list RET
;;
;; This lets you select generic modes from the list of available
;; modes.  If you manually set `generic-extras-enable-list' in your
;; .emacs, do it BEFORE loading generic-x with (require 'generic-x).
;;
;; You can also send in new modes; if the file types are reasonably
;; common, we would like to install them.
;;
;; DEFAULT GENERIC MODE:
;;
;; This file provides a hook which automatically puts a file into
;; `default-generic-mode' if the first few lines of a file in
;; fundamental mode start with a hash comment character.  To disable
;; this functionality, set the variable `generic-use-find-file-hook'
;; to nil BEFORE loading generic-x.  See the variables
;; `generic-lines-to-scan' and `generic-find-file-regexp' for
;; customization options.
;;
;; PROBLEMS WHEN USED WITH FOLDING MODE:
;;
;; [The following relates to the obsolete selective-display technique.
;; Folding mode should use invisible text properties instead.  -- Dave
;; Love]
;;
;; From Anders Lindgren <andersl@csd.uu.se>
;;
;; Problem summary: Wayne Adams has found a problem when using folding
;; mode in conjunction with font-lock for a mode defined in
;; `generic-x.el'.
;;
;; The problem, as Wayne described it, was that error messages of the
;; following form appeared when both font-lock and folding are used:
;;
;; >      - various msgs including "Fontifying region...(error Stack
;; > overflow in regexp matcher)" appear
;;
;; I have just tracked down the cause of the problem.  The regexp's in
;; `generic-x.el' do not take into account the way that folding hides
;; sections of the buffer.  The technique is known as
;; `selective-display' and has been available for a very long time (I
;; started using it back in the good old Emacs 18 days).  Basically, a
;; section is hidden by creating one very long line were the newline
;; character (C-j) is replaced by a linefeed (C-m) character.
;;
;; Many other hiding packages, besides folding, use the same technique,
;; the problem should occur when using them as well.
;;
;; The erroneous lines in `generic-x.el' look like the following (this
;; example is from the `ini' section):
;;
;;     '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)
;;       ("^\\(.*\\)="        1 'font-lock-variable-name-face)
;;
;; The intention of these lines is to highlight lines of the following
;; form:
;;
;; [foo]
;; bar = xxx
;;
;; However, since the `.' regexp symbol matches the linefeed character
;; the entire folded section is searched, resulting in a regexp stack
;; overflow.
;;
;; Solution suggestion: Instead of using ".", use the sequence
;; "[^\n\r]".  This will make the rules behave just as before, but
;; they will work together with selective-display.

;;; Code:

(eval-when-compile (require 'font-lock))

(defgroup generic-x nil
  "A collection of generic modes."
  :prefix "generic-"
  :group 'data
  :version "20.3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default-Generic mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom generic-use-find-file-hook t
  "If non-nil, add a hook to enter `default-generic-mode' automatically.
This is done if the first few lines of a file in fundamental mode
start with a hash comment character."
  :group 'generic-x
  :type  'boolean)

(defcustom generic-lines-to-scan 3
  "Number of lines that `generic-mode-find-file-hook' looks at.
Relevant when deciding whether to enter Default-Generic mode automatically.
This variable should be set to a small positive number."
  :group 'generic-x
  :type  'integer)

(defcustom generic-find-file-regexp "^#"
  "Regular expression used by `generic-mode-find-file-hook'.
Files in fundamental mode whose first few lines contain a match
for this regexp, should be put into Default-Generic mode instead.
The number of lines tested for the matches is specified by the
value of the variable `generic-lines-to-scan', which see."
  :group 'generic-x
  :type  'regexp)

(defcustom generic-ignore-files-regexp "[Tt][Aa][Gg][Ss]\\'"
  "Regular expression used by `generic-mode-find-file-hook'.
Files whose names match this regular expression should not be put
into Default-Generic mode, even if they have lines which match
the regexp in `generic-find-file-regexp'.  If the value is nil,
`generic-mode-find-file-hook' does not check the file names."
  :group 'generic-x
  :type  '(choice (const :tag "Don't check file names" nil) regexp))

;; This generic mode is always defined
(define-generic-mode default-generic-mode (list ?#) nil nil nil nil)

;; A more general solution would allow us to enter generic-mode for
;; *any* comment character, but would require us to synthesize a new
;; generic-mode on the fly. I think this gives us most of what we
;; want.
(defun generic-mode-find-file-hook ()
  "Hook function to enter Default-Generic mode automatically.

Done if the first few lines of a file in Fundamental mode start
with a match for the regexp in `generic-find-file-regexp', unless
the file's name matches the regexp which is the value of the
variable `generic-ignore-files-regexp'.

This hook will be installed if the variable
`generic-use-find-file-hook' is non-nil.  The variable
`generic-lines-to-scan' determines the number of lines to look at."
  (when (and (eq major-mode 'fundamental-mode)
	     (or (null generic-ignore-files-regexp)
		 (not (string-match-p
		       generic-ignore-files-regexp
		       (file-name-sans-versions buffer-file-name)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward generic-find-file-regexp
			       (save-excursion
				 (forward-line generic-lines-to-scan)
				 (point)) t)
	(goto-char (point-min))
	(default-generic-mode)))))

(and generic-use-find-file-hook
    (add-hook 'find-file-hook 'generic-mode-find-file-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Generic modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you add a generic mode to this file, put it in one of these four
;; lists as well.

(defconst generic-default-modes
  '(apache-conf-generic-mode
    apache-log-generic-mode
    hosts-generic-mode
    java-manifest-generic-mode
    java-properties-generic-mode
    javascript-generic-mode
    show-tabs-generic-mode
    vrml-generic-mode)
  "List of generic modes that are defined by default.")

(defconst generic-mswindows-modes
  '(bat-generic-mode
    inf-generic-mode
    ini-generic-mode
    rc-generic-mode
    reg-generic-mode
    rul-generic-mode)
  "List of generic modes that are defined by default on MS-Windows.")

(defconst generic-unix-modes
  '(alias-generic-mode
    etc-fstab-generic-mode
    etc-modules-conf-generic-mode
    etc-passwd-generic-mode
    etc-services-generic-mode
    etc-sudoers-generic-mode
    fvwm-generic-mode
    inetd-conf-generic-mode
    mailagent-rules-generic-mode
    mailrc-generic-mode
    named-boot-generic-mode
    named-database-generic-mode
    prototype-generic-mode
    resolve-conf-generic-mode
    samba-generic-mode
    x-resource-generic-mode
    xmodmap-generic-mode)
  "List of generic modes that are defined by default on Unix.")

(defconst generic-other-modes
  '(astap-generic-mode
    ibis-generic-mode
    pkginfo-generic-mode
    spice-generic-mode)
  "List of generic modes that are not defined by default.")

(defcustom generic-define-mswindows-modes
  (memq system-type '(windows-nt ms-dos))
  "Non-nil means the modes in `generic-mswindows-modes' will be defined.
This is a list of MS-Windows specific generic modes.  This variable
only affects the default value of `generic-extras-enable-list'."
  :group 'generic-x
  :type 'boolean
  :version "22.1")
(make-obsolete-variable 'generic-define-mswindows-modes 'generic-extras-enable-list "22.1")

(defcustom generic-define-unix-modes
  (not (memq system-type '(windows-nt ms-dos)))
  "Non-nil means the modes in `generic-unix-modes' will be defined.
This is a list of Unix specific generic modes.  This variable only
affects the default value of `generic-extras-enable-list'."
  :group 'generic-x
  :type 'boolean
  :version "22.1")
(make-obsolete-variable 'generic-define-unix-modes 'generic-extras-enable-list "22.1")

(defcustom generic-extras-enable-list
  (append generic-default-modes
	  (if generic-define-mswindows-modes generic-mswindows-modes)
	  (if generic-define-unix-modes generic-unix-modes)
	  nil)
  "List of generic modes to define.
Each entry in the list should be a symbol.  If you set this variable
directly, without using customize, you must reload generic-x to put
your changes into effect."
  :group 'generic-x
  :type (let (list)
	  (dolist (mode
		   (sort (append generic-default-modes
				 generic-mswindows-modes
				 generic-unix-modes
				 generic-other-modes
				 nil)
			 (lambda (a b)
			   (string< (symbol-name b)
				    (symbol-name a))))
		   (cons 'set list))
	    (push `(const ,mode) list)))
  :set (lambda (s v)
	 (set-default s v)
	 (unless load-in-progress
	   (load "generic-x")))
  :version "22.1")

;;; Apache
(when (memq 'apache-conf-generic-mode generic-extras-enable-list)

(define-generic-mode apache-conf-generic-mode
  '(?#)
  nil
  '(("^\\s-*\\(<.*>\\)"      1 font-lock-constant-face)
    ("^\\s-*\\(\\sw+\\)\\s-" 1 font-lock-variable-name-face))
  '("srm\\.conf\\'" "httpd\\.conf\\'" "access\\.conf\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\)" 1)
	      ("*Directories*" "^\\s-*<Directory\\s-*\\([^>]+\\)>" 1)
	      ("*Locations*"   "^\\s-*<Location\\s-*\\([^>]+\\)>" 1))))))
  "Generic mode for Apache or HTTPD configuration files."))

(when (memq 'apache-log-generic-mode generic-extras-enable-list)

(define-generic-mode apache-log-generic-mode
  nil
  nil
  ;; Hostname ? user date request return-code number-of-bytes
  '(("^\\([-a-zA-z0-9.]+\\) - [-A-Za-z]+ \\(\\[.*\\]\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face)))
  '("access_log\\'")
  nil
  "Generic mode for Apache log files."))

;;; Samba
(when (memq 'samba-generic-mode generic-extras-enable-list)

(define-generic-mode samba-generic-mode
  '(?\; ?#)
  nil
  '(("^\\(\\[.*\\]\\)" 1 font-lock-constant-face)
    ("^\\s-*\\(.+\\)=\\([^\r\n]*\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face)))
  '("smb\\.conf\\'")
  '(generic-bracket-support)
  "Generic mode for Samba configuration files."))

;;; Fvwm
;; This is pretty basic. Also, modes for other window managers could
;; be defined as well.
(when (memq 'fvwm-generic-mode generic-extras-enable-list)

(define-generic-mode fvwm-generic-mode
  '(?#)
  '("AddToMenu"
    "AddToFunc"
    "ButtonStyle"
    "EndFunction"
    "EndPopup"
    "Function"
    "IconPath"
    "Key"
    "ModulePath"
    "Mouse"
    "PixmapPath"
    "Popup"
    "Style")
  nil
  '("\\.fvwmrc\\'" "\\.fvwm2rc\\'")
  nil
  "Generic mode for FVWM configuration files."))

;;; X Resource
;; I'm pretty sure I've seen an actual mode to do this, but I don't
;; think it's standard with Emacs
(when (memq 'x-resource-generic-mode generic-extras-enable-list)

(define-generic-mode x-resource-generic-mode
  '(?!)
  nil
  '(("^\\([^:\n]+:\\)" 1 font-lock-variable-name-face))
  '("\\.Xdefaults\\'" "\\.Xresources\\'" "\\.Xenvironment\\'" "\\.ad\\'")
  nil
  "Generic mode for X Resource configuration files."))

(if (memq 'xmodmap-generic-mode generic-extras-enable-list)
(define-generic-mode xmodmap-generic-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "remove" "pointer")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files."))

;;; Hosts
(when (memq 'hosts-generic-mode generic-extras-enable-list)

(define-generic-mode hosts-generic-mode
  '(?#)
  '("localhost")
  '(("\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face))
  '("[hH][oO][sS][tT][sS]\\'")
  nil
  "Generic mode for HOSTS files."))

;;; Windows INF files

;; If i-g-m-f-f-h is defined, then so is i-g-m.
(declare-function ini-generic-mode "generic-x")

(when (memq 'inf-generic-mode generic-extras-enable-list)

(define-generic-mode inf-generic-mode
  '(?\;)
  nil
  '(("^\\(\\[.*\\]\\)" 1 font-lock-constant-face))
  '("\\.[iI][nN][fF]\\'")
  '(generic-bracket-support)
  "Generic mode for MS-Windows INF files."))

;;; Windows INI files
;; Should define escape character as well!
(when (memq 'ini-generic-mode generic-extras-enable-list)

(define-generic-mode ini-generic-mode
  '(?\;)
  nil
  '(("^\\(\\[.*\\]\\)" 1 font-lock-constant-face)
    ("^\\([^=\n\r]*\\)=\\([^\n\r]*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face)))
  '("\\.[iI][nN][iI]\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\[\\(.*\\)\\]" 1)
	      ("*Variables*" "^\\s-*\\([^=]+\\)\\s-*=" 1))))))
  "Generic mode for MS-Windows INI files.
You can use `ini-generic-mode-find-file-hook' to enter this mode
automatically for INI files whose names do not end in \".ini\".")

(defun ini-generic-mode-find-file-hook ()
  "Hook function to enter Ini-Generic mode automatically for INI files.
Done if the first few lines of a file in Fundamental mode look
like an INI file.  You can add this hook to `find-file-hook'."
  (and (eq major-mode 'fundamental-mode)
       (save-excursion
	 (goto-char (point-min))
	 (and (looking-at "^\\s-*\\[.*\\]")
	      (ini-generic-mode)))))
(defalias 'generic-mode-ini-file-find-file-hook 'ini-generic-mode-find-file-hook))

;;; Windows REG files
;;; Unfortunately, Windows 95 and Windows NT have different REG file syntax!
(when (memq 'reg-generic-mode generic-extras-enable-list)

(define-generic-mode reg-generic-mode
  '(?\;)
  '("key" "classes_root" "REGEDIT" "REGEDIT4")
  '(("\\(\\[.*\\]\\)"        1 font-lock-constant-face)
    ("^\\([^\n\r]*\\)\\s-*=" 1 font-lock-variable-name-face))
  '("\\.[rR][eE][gG]\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\s-*\\(.*\\)\\s-*=" 1))))))
  "Generic mode for MS-Windows Registry files."))

(declare-function w32-shell-name "w32-fns" ())

;;; DOS/Windows BAT files
(when (memq 'bat-generic-mode generic-extras-enable-list)

(define-generic-mode bat-generic-mode
  nil
  nil
  (eval-when-compile
    (list
     ;; Make this one first in the list, otherwise comments will
     ;; be over-written by other variables
     '("^[@ \t]*\\([rR][eE][mM][^\n\r]*\\)" 1 font-lock-comment-face t)
     '("^[ \t]*\\(::.*\\)"                  1 font-lock-comment-face t)
     '("^[@ \t]*\\([bB][rR][eE][aA][kK]\\|[vV][eE][rR][iI][fF][yY]\\)[ \t]+\\([oO]\\([nN]\\|[fF][fF]\\)\\)"
       (1 font-lock-builtin-face)
       (2 font-lock-constant-face t t))
     ;; Any text (except ON/OFF) following ECHO is a string.
     '("^[@ \t]*\\([eE][cC][hH][oO]\\)[ \t]+\\(\\([oO]\\([nN]\\|[fF][fF]\\)\\)\\|\\([^>|\r\n]+\\)\\)"
       (1 font-lock-builtin-face)
       (3 font-lock-constant-face t t)
       (5 font-lock-string-face t t))
     ;; These keywords appear as the first word on a line.  (Actually, they
     ;; can also appear after "if ..." or "for ..." clause, but since they
     ;; are frequently used in simple text, we punt.)
     ;; In `generic-bat-mode-setup-function' we make the keywords
     ;; case-insensitive
     (generic-make-keywords-list
      '("for"
	"if")
      font-lock-keyword-face "^[@ \t]*")
     ;; These keywords can be anywhere on a line
     ;; In `generic-bat-mode-setup-function' we make the keywords
     ;; case-insensitive
     (generic-make-keywords-list
      '("do"
	"exist"
	"errorlevel"
	"goto"
	"not")
      font-lock-keyword-face)
     ;; These are built-in commands.  Only frequently-used ones are listed.
     (generic-make-keywords-list
      '("CALL"	    "call"	 "Call"
	"CD"	    "cd"	 "Cd"
	"CLS"	    "cls"	 "Cls"
	"COPY"	    "copy"	 "Copy"
	"DEL"	    "del"	 "Del"
	"ECHO"	    "echo"	 "Echo"
	"MD"	    "md"	 "Md"
	"PATH"	    "path"	 "Path"
	"PAUSE"	    "pause"	 "Pause"
	"PROMPT"    "prompt"	 "Prompt"
	"RD"	    "rd"	 "Rd"
	"REN"	    "ren"	 "Ren"
	"SET"	    "set"	 "Set"
	"START"	    "start"	 "Start"
	"SHIFT"	    "shift"	 "Shift")
      font-lock-builtin-face "[ \t|\n]")
     '("^[ \t]*\\(:\\sw+\\)"         1 font-lock-function-name-face t)
     '("\\(%\\sw+%\\)"               1 font-lock-variable-name-face t)
     '("\\(%[0-9]\\)"                1 font-lock-variable-name-face t)
     '("[\t ]+\\([+-/][^\t\n\" ]+\\)" 1 font-lock-type-face)
     '("[ \t\n|]\\<\\([gG][oO][tT][oO]\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
     '("[ \t\n|]\\<\\([sS][eE][tT]\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*=?"
       (1 font-lock-builtin-face)
       (2 font-lock-variable-name-face t t))))
  '("\\.[bB][aA][tT]\\'"
    "\\.[cC][mM][dD]\\'"
    "\\`[cC][oO][nN][fF][iI][gG]\\."
    "\\`[aA][uU][tT][oO][eE][xX][eE][cC]\\.")
  '(generic-bat-mode-setup-function)
  "Generic mode for MS-Windows batch files.")

(defvar bat-generic-mode-syntax-table nil
  "Syntax table in use in `bat-generic-mode' buffers.")

(defvar bat-generic-mode-keymap (make-sparse-keymap)
  "Keymap for `bat-generic-mode'.")

(defun bat-generic-mode-compile ()
  "Run the current BAT file in a compilation buffer."
  (interactive)
  (let ((compilation-buffer-name-function
	 (function
	  (lambda (_ign)
	    (concat "*" (buffer-file-name) "*")))))
    (compile
     (concat (w32-shell-name) " -c " (buffer-file-name)))))

(eval-when-compile (require 'comint))
(defun bat-generic-mode-run-as-comint ()
  "Run the current BAT file in a comint buffer."
  (interactive)
  (require 'comint)
  (let* ((file (buffer-file-name))
	 (buf-name (concat "*" file "*")))
    (with-current-buffer (get-buffer-create buf-name)
      (erase-buffer)
      (comint-mode)
      (comint-exec
       buf-name
       file
       (w32-shell-name)
       nil
       (list "-c" file))
      (display-buffer buf-name))))

(define-key bat-generic-mode-keymap "\C-c\C-c" 'bat-generic-mode-compile)

;; Make underscores count as words
(unless bat-generic-mode-syntax-table
  (setq bat-generic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" bat-generic-mode-syntax-table))

;; bat-generic-mode doesn't use the comment functionality of
;; define-generic-mode because it has a three-letter comment-string,
;; so we do it here manually instead
(defun generic-bat-mode-setup-function ()
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (setq imenu-generic-expression '((nil "^:\\(\\sw+\\)" 1))
	parse-sexp-ignore-comments t
	comment-end ""
	comment-start "REM "
	comment-start-skip "[Rr][Ee][Mm] *")
  (set-syntax-table bat-generic-mode-syntax-table)
  ;; Make keywords case-insensitive
  (setq font-lock-defaults '(generic-font-lock-keywords nil t))
  (use-local-map bat-generic-mode-keymap)))

;;; Mailagent
;; Mailagent is a Unix mail filtering program.  Anyone wanna do a
;; generic mode for procmail?
(when (memq 'mailagent-rules-generic-mode generic-extras-enable-list)

(define-generic-mode mailagent-rules-generic-mode
  '(?#)
  '("SAVE" "DELETE" "PIPE" "ANNOTATE" "REJECT")
  '(("^\\(\\sw+\\)\\s-*="         1 font-lock-variable-name-face)
    ("\\s-/\\([^/]+\\)/[i, \t\n]" 1 font-lock-constant-face))
  '("\\.rules\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "\\s-/\\([^/]+\\)/[i, \t\n]" 1))))))
  "Generic mode for Mailagent rules files."))

;; Solaris/Sys V prototype files
(when (memq 'prototype-generic-mode generic-extras-enable-list)

(define-generic-mode prototype-generic-mode
  '(?#)
  nil
  '(("^\\([0-9]\\)?\\s-*\\([a-z]\\)\\s-+\\([A-Za-z_]+\\)\\s-+\\([^\n\r]*\\)$"
     (2 font-lock-constant-face)
     (3 font-lock-keyword-face))
    ("^\\([a-z]\\) \\([A-Za-z_]+\\)=\\([^\n\r]*\\)$"
     (1 font-lock-constant-face)
     (2 font-lock-keyword-face)
     (3 font-lock-variable-name-face))
    ("^\\(!\\s-*\\(search\\|include\\|default\\)\\)\\s-*\\([^\n\r]*\\)$"
     (1 font-lock-keyword-face)
     (3 font-lock-variable-name-face))
    ("^\\(!\\s-*\\sw+\\)=\\([^\n\r]*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)))
  '("prototype\\'")
  nil
  "Generic mode for Sys V prototype files."))

;; Solaris/Sys V pkginfo files
(when (memq 'pkginfo-generic-mode generic-extras-enable-list)

(define-generic-mode pkginfo-generic-mode
  '(?#)
  nil
  '(("^\\([A-Za-z_]+\\)=\\([^\n\r]*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)))
  '("pkginfo\\'")
  nil
  "Generic mode for Sys V pkginfo files."))

;; Javascript mode
;; Includes extra keywords from Armando Singer [asinger@MAIL.COLGATE.EDU]
(when (memq 'javascript-generic-mode generic-extras-enable-list)

(define-generic-mode javascript-generic-mode
  '("//" ("/*" . "*/"))
  '("break"
    "case"
    "continue"
    "default"
    "delete"
    "do"
    "else"
    "export"
    "for"
    "function"
    "if"
    "import"
    "in"
    "new"
    "return"
    "switch"
    "this"
    "typeof"
    "var"
    "void"
    "while"
    "with"
    ;; words reserved for ECMA extensions below
    "catch"
    "class"
    "const"
    "debugger"
    "enum"
    "extends"
    "finally"
    "super"
    "throw"
    "try"
    ;; Java Keywords reserved by JavaScript
    "abstract"
    "boolean"
    "byte"
    "char"
    "double"
    "false"
    "final"
    "float"
    "goto"
    "implements"
    "instanceof"
    "int"
    "interface"
    "long"
    "native"
    "null"
    "package"
    "private"
    "protected"
    "public"
    "short"
    "static"
    "synchronized"
    "throws"
    "transient"
    "true")
  '(("^\\s-*function\\s-+\\([A-Za-z0-9_]+\\)"
     (1 font-lock-function-name-face))
    ("^\\s-*var\\s-+\\([A-Za-z0-9_]+\\)"
     (1 font-lock-variable-name-face)))
  '("\\.js\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^function\\s-+\\([A-Za-z0-9_]+\\)" 1)
	      ("*Variables*" "^var\\s-+\\([A-Za-z0-9_]+\\)" 1))))))
  "Generic mode for JavaScript files."))

;; VRML files
(when (memq 'vrml-generic-mode generic-extras-enable-list)

(define-generic-mode vrml-generic-mode
  '(?#)
  '("DEF"
    "NULL"
    "USE"
    "Viewpoint"
    "ambientIntensity"
    "appearance"
    "children"
    "color"
    "coord"
    "coordIndex"
    "creaseAngle"
    "diffuseColor"
    "emissiveColor"
    "fieldOfView"
    "geometry"
    "info"
    "material"
    "normal"
    "orientation"
    "position"
    "shininess"
    "specularColor"
    "texCoord"
    "texture"
    "textureTransform"
    "title"
    "transparency"
    "type")
  '(("USE\\s-+\\([-A-Za-z0-9_]+\\)"
     (1 font-lock-constant-face))
    ("DEF\\s-+\\([-A-Za-z0-9_]+\\)\\s-+\\([A-Za-z0-9]+\\)\\s-*{"
     (1 font-lock-type-face)
     (2 font-lock-constant-face))
    ("^\\s-*\\([-A-Za-z0-9_]+\\)\\s-*{"
     (1 font-lock-function-name-face))
    ("^\\s-*\\(geometry\\|appearance\\|material\\)\\s-+\\([-A-Za-z0-9_]+\\)"
     (2 font-lock-variable-name-face)))
  '("\\.wrl\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([A-Za-z0-9_]+\\)\\s-*{" 1)
	      ("*Definitions*"
	       "DEF\\s-+\\([-A-Za-z0-9_]+\\)\\s-+\\([A-Za-z0-9]+\\)\\s-*{"
	       1))))))
  "Generic Mode for VRML files."))

;; Java Manifests
(when (memq 'java-manifest-generic-mode generic-extras-enable-list)

(define-generic-mode java-manifest-generic-mode
  '(?#)
  '("Name"
    "Digest-Algorithms"
    "Manifest-Version"
    "Required-Version"
    "Signature-Version"
    "Magic"
    "Java-Bean"
    "Depends-On")
  '(("^Name:\\s-+\\([^\n\r]*\\)$"
     (1 font-lock-variable-name-face))
    ("^\\(Manifest\\|Required\\|Signature\\)-Version:\\s-+\\([^\n\r]*\\)$"
     (2 font-lock-constant-face)))
  '("[mM][aA][nN][iI][fF][eE][sS][tT]\\.[mM][fF]\\'")
  nil
  "Generic mode for Java Manifest files."))

;; Java properties files
(when (memq 'java-properties-generic-mode generic-extras-enable-list)

(define-generic-mode java-properties-generic-mode
  '(?! ?#)
  nil
  (eval-when-compile
    (let ((java-properties-key
	   "\\(\\([-A-Za-z0-9_\\./]\\|\\(\\\\[ =:]\\)\\)+\\)")
	  (java-properties-value
	   "\\([^\r\n]*\\)"))
      ;; Property and value can be separated in a number of different ways:
      ;;   * whitespace
      ;;   * an equal sign
      ;;   * a colon
      (mapcar
       (function
	(lambda (elt)
	  (list
	   (concat "^" java-properties-key elt java-properties-value "$")
	   '(1 font-lock-constant-face)
	   '(4 font-lock-variable-name-face))))
       ;; These are the separators
       '(":\\s-*" "\\s-+" "\\s-*=\\s-*"))))
  nil
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([^#! \t\n\r=:]+\\)" 1))))))
  "Generic mode for Java properties files."))

;; C shell alias definitions
(when (memq 'alias-generic-mode generic-extras-enable-list)

(define-generic-mode alias-generic-mode
  '(?#)
  '("alias" "unalias")
  '(("^alias\\s-+\\([-A-Za-z0-9_]+\\)\\s-+"
     (1 font-lock-variable-name-face))
    ("^unalias\\s-+\\([-A-Za-z0-9_]+\\)\\s-*$"
     (1 font-lock-variable-name-face)))
  '("alias\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\(alias\\|unalias\\)\\s-+\\([-a-zA-Z0-9_]+\\)" 2))))))
  "Generic mode for C Shell alias files."))

;;; Windows RC files
;; Contributed by ACorreir@pervasive-sw.com (Alfred Correira)
(when (memq 'rc-generic-mode generic-extras-enable-list)

(define-generic-mode rc-generic-mode
  ;; '(?\/)
  '("//")
  '("ACCELERATORS"
    "AUTO3STATE"
    "AUTOCHECKBOX"
    "AUTORADIOBUTTON"
    "BITMAP"
    "BOTTOMMARGIN"
    "BUTTON"
    "CAPTION"
    "CHARACTERISTICS"
    "CHECKBOX"
    "CLASS"
    "COMBOBOX"
    "CONTROL"
    "CTEXT"
    "CURSOR"
    "DEFPUSHBUTTON"
    "DESIGNINFO"
    "DIALOG"
    "DISCARDABLE"
    "EDITTEXT"
    "EXSTYLE"
    "FONT"
    "GROUPBOX"
    "GUIDELINES"
    "ICON"
    "LANGUAGE"
    "LEFTMARGIN"
    "LISTBOX"
    "LTEXT"
    "MENUITEM SEPARATOR"
    "MENUITEM"
    "MENU"
    "MOVEABLE"
    "POPUP"
    "PRELOAD"
    "PURE"
    "PUSHBOX"
    "PUSHBUTTON"
    "RADIOBUTTON"
    "RCDATA"
    "RIGHTMARGIN"
    "RTEXT"
    "SCROLLBAR"
    "SEPARATOR"
    "STATE3"
    "STRINGTABLE"
    "STYLE"
    "TEXTINCLUDE"
    "TOOLBAR"
    "TOPMARGIN"
    "VERSIONINFO"
    "VERSION")
  ;; the choice of what tokens go where is somewhat arbitrary,
  ;; as is the choice of which value tokens are included, as
  ;; the choice of face for each token group
  (eval-when-compile
    (list
     (generic-make-keywords-list
      '("FILEFLAGSMASK"
	"FILEFLAGS"
	"FILEOS"
	"FILESUBTYPE"
	"FILETYPE"
	"FILEVERSION"
	"PRODUCTVERSION")
      font-lock-type-face)
     (generic-make-keywords-list
      '("BEGIN"
	"BLOCK"
	"END"
	"VALUE")
      font-lock-function-name-face)
     '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
     '("^#[ \t]*define[ \t]+\\(\\sw+\\)("       1 font-lock-function-name-face)
     '("^#[ \t]*\\(elif\\|if\\)\\>"
       ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
	(1 font-lock-constant-face)
	(2 font-lock-variable-name-face nil t)))
     '("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-constant-face)
       (2 font-lock-variable-name-face nil t))))
    '("\\.[rR][cC]\\'")
    nil
    "Generic mode for MS-Windows Resource files."))

;; InstallShield RUL files
;; Contributed by  Alfred.Correira@Pervasive.Com
;; Bugfixes by "Rolf Sandau" <Rolf.Sandau@marconi.com>
(when (memq 'rul-generic-mode generic-extras-enable-list)

(eval-when-compile

;;; build the regexp strings using regexp-opt
(defconst installshield-statement-keyword-list
  '("abort"
    "begin"
    "call"
    "case"
    "declare"
    "default"
    "downto"
    "elseif"
    "else"
    "endfor"
    "endif"
    "endswitch"
    "endwhile"
    "end"
    "exit"
    "external"
    "for"
    "function"
    ;; "goto" -- handled elsewhere
    "if"
    "program"
    "prototype"
    "repeat"
    "return"
    "step"
    "switch"
    "then"
    "to"
    "typedef"
    "until"
    "void"
    "while")
  "Statement keywords used in InstallShield 3 and 5.")

(defconst installshield-system-functions-list
  '("AddFolderIcon"
    "AddProfString"
    "AddressString"
    "AppCommand"
    "AskDestPath"
    "AskOptions"
    "AskPath"
    "AskText"
    "AskYesNo"
    "BatchDeleteEx"
    "BatchFileLoad"
    "BatchFileSave"
    "BatchFind"
    "BatchGetFileName"
    "BatchMoveEx"
    "BatchSetFileName"
    "ChangeDirectory"
    "CloseFile"
    "CmdGetHwndDlg"
    "ComponentAddItem"			; differs between IS3 and IS5
    "ComponentCompareSizeRequired"	; IS5 only
    "ComponentDialog"
    "ComponentError"			; IS5 only
    "ComponentFileEnum"			; IS5 only
    "ComponentFileInfo"			; IS5 only
    "ComponentFilterLanguage"		; IS5 only
    "ComponentFilterOS"			; IS5 only
    "ComponentGetData"			; IS5 only
    "ComponentGetItemInfo"		; IS3 only
    "ComponentGetItemSize"		; differs between IS3 and IS5
    "ComponentIsItemSelected"		; differs between IS3 and IS5
    "ComponentListItems"
    "ComponentMoveData"			; IS5 only
    "ComponentSelectItem"		; differs between IS3 and IS5
    "ComponentSetData"			; IS5 only
    "ComponentSetItemInfo"		; IS3 only
    "ComponentSetTarget"		; IS5 only
    "ComponentSetupTypeEnum"		; IS5 only
    "ComponentSetupTypeGetData"		; IS5 only
    "ComponentSetupTypeSet"		; IS5 only
    "ComponentTotalSize"
    "ComponentValidate"			; IS5 only
    "CompressAdd"			; IS3 only
    "CompressDel"			; IS3 only
    "CompressEnum"			; IS3 only
    "CompressGet"			; IS3 only
    "CompressInfo"			; IS3 only
    "CopyFile"
    "CreateDir"
    "CreateFile"
    "CreateProgramFolder"
    "DeinstallSetReference"		; IS5 only
    "DeinstallStart"
    "Delay"
    "DeleteDir"
    "DeleteFile"
    "DialogSetInfo"
    "Disable"
    "DoInstall"
    "Do"
    "Enable"
    "EnterDisk"
    "ExistsDir"
    "ExistsDisk"
    "ExitProgMan"
    "EzBatchAddPath"
    "EzBatchAddString"
    "EzBatchReplace"
    "EzConfigAddDriver"
    "EzConfigAddString"
    "EzConfigGetValue"
    "EzConfigSetValue"
    "EzDefineDialog"
    "FileCompare"
    "FileDeleteLine"
    "FileGrep"
    "FileInsertLine"
    "FileSetBeginDefine"		; IS3 only
    "FileSetEndDefine"			; IS3 only
    "FileSetPerformEz"			; IS3 only
    "FileSetPerform"			; IS3 only
    "FileSetReset"			; IS3 only
    "FileSetRoot"			; IS3 only
    "FindAllDirs"
    "FindAllFiles"
    "FindFile"
    "FindWindow"
    "GetDiskSpace"
    "GetDisk"
    "GetEnvVar"
    "GetExtents"
    "GetFileInfo"
    "GetLine"
    "GetProfInt"
    "GetProfString"
    "GetSystemInfo"
    "GetValidDrivesList"
    "GetVersion"
    "GetWindowHandle"
    "InstallationInfo"
    "Is"
    "LaunchApp"
    "LaunchAppAndWait"
    "ListAddItem"
    "ListAddString"
    "ListCount"
    "ListCreate"
    "ListDestroy"
    "ListFindItem"
    "ListFindString"
    "ListGetFirstItem"
    "ListGetFirstString"
    "ListGetNextItem"
    "ListGetNextString"
    "ListReadFromFile"
    "ListSetCurrentItem"
    "ListSetNextItem"
    "ListSetNextString"
    "ListSetIndex"
    "ListWriteToFile"
    "LongPathToQuote"
    "LongPathToShortPath"
    "MessageBox"
    "NumToStr"
    "OpenFileMode"
    "OpenFile"
    "ParsePath"
    "PathAdd"
    "PathDelete"
    "PathFind"
    "PathGet"
    "PathMove"
    "PathSet"
    "Path"
    "PlaceBitmap"
    "PlaceWindow"
    "PlayMMedia"			; IS5 only
    "ProgDefGroupType"
    "RegDBCreateKeyEx"
    "RegDBDeleteValue"
    "RegDBGetItem"
    "RegDBKeyExist"
    "RegDBSetItem"
    "RegDBGetKeyValueEx"
    "RegDBSetKeyValueEx"
    "RegDBSetDefaultRoot"
    "RenameFile"
    "ReplaceFolderIcon"
    "ReplaceProfString"
    "SdAskDestPath"
    "SdAskOptions"
    "SdAskOptionsList"
    "SdBitmap"
    "SdCloseDlg"
    "SdComponentAdvCheckSpace"
    "SdComponentAdvInit"
    "SdComponentAdvUpdateSpace"
    "SdComponentDialog"
    "SdComponentDialog2"
    "SdComponentDialogAdv"
    "SdComponentDialogEx"
    "SdComponentDlgCheckSpace"
    "SdComponentMult"
    "SdConfirmNewDir"
    "SdConfirmRegistration"
    "SdDiskSpace"
    "SdDisplayTopics"
    "SdDoStdButton"
    "SdEnablement"
    "SdError"
    "SdFinish"
    "SdFinishInit32"
    "SdFinishReboot"
    "SdGeneralInit"
    "SdGetItemName"
    "SdGetTextExtent"
    "SdGetUserCompanyInfo"
    "SdInit"
    "SdIsShellExplorer"
    "SdIsStdButton"
    "SdLicense"
    "SdMakeName"
    "SdOptionInit"
    "SdOptionSetState"
    "SdOptionsButtons"
    "SdOptionsButtonsInit"
    "SdPlugInProductName"
    "SdProductName"
    "SdRegEnableButton"
    "SdRegExEnableButton"
    "SdRegisterUser"
    "SdRegisterUserEx"
    "SdRemoveEndSpace"
    "SdSelectFolder"
    "SdSetSequentialItems"
    "SdSetStatic"
    "SdSetupTypeEx"			; IS5 only
    "SdSetupType"
    "SdShowAnyDialog"
    "SdShowDlgEdit1"
    "SdShowDlgEdit2"
    "SdShowDlgEdit3"
    "SdShowFileMods"
    "SdShowInfoList"
    "SdShowMsg"
    "SdStartCopy"
    "SdUnInit"
    "SdUpdateComponentSelection"
    "SdWelcome"
    "SendMessage"
    "SetColor"
    "SetFont"
    "SetDialogTitle"
    "SetDisplayEffect"			; IS5 only
    "SetFileInfo"
    "SetForegroundWindow"
    "SetStatusWindow"
    "SetTitle"
    "SetupType"
    "ShowProgramFolder"
    "Split"				; IS3 only
    "SprintfBox"
    "Sprintf"
    "StatusUpdate"
    "StrCompare"
    "StrFind"
    "StrGetTokens"
    "StrLength"
    "StrRemoveLastSlash"
    "StrToLower"
    "StrToNum"
    "StrToUpper"
    "StrSub"
    "VarRestore"
    "VarSave"
    "VerCompare"
    "VerGetFileVersion"
    "WaitOnDialog"
    "Welcome"
    "WriteLine"
    "WriteProfString"
    "XCopyFile")
  "System functions defined in InstallShield 3 and 5.")

(defconst installshield-system-variables-list
  '("BATCH_INSTALL"
    "CMDLINE"
    "COMMONFILES"
    "CORECOMPONENTHANDLING"
    "DIALOGCACHE"
    "ERRORFILENAME"
    "FOLDER_DESKTOP"
    "FOLDER_PROGRAMS"
    "FOLDER_STARTMENU"
    "FOLDER_STARTUP"
    "INFOFILENAME"
    "ISRES"
    "ISUSER"
    "ISVERSION"
    "MEDIA"
    "MODE"
    "PROGRAMFILES"
    "SELECTED_LANGUAGE"
    "SRCDIR"
    "SRCDISK"
    "SUPPORTDIR"
    "TARGETDIR"
    "TARGETDISK"
    "UNINST"
    "WINDIR"
    "WINDISK"
    "WINMAJOR"
    "WINSYSDIR"
    "WINSYSDISK")
  "System variables used in InstallShield 3 and 5.")

(defconst installshield-types-list
  '("BOOL"
    "BYREF"
    "CHAR"
    "HIWORD"
    "HWND"
    "INT"
    "LIST"
    "LONG"
    "LOWORD"
    "LPSTR"
    "NUMBER"
    "NUMBERLIST"
    "POINTER"
    "QUAD"
    "RGB"
    "SHORT"
    "STRINGLIST"
    "STRING")
  "Type keywords used in InstallShield 3 and 5.")

;;; some might want to skip highlighting these to improve performance
(defconst installshield-funarg-constants-list
  '("AFTER"
    "APPEND"
    "ALLCONTENTS"
    "BACKBUTTON"
    "BACKGROUNDCAPTION"
    "BACKGROUND"
    "BACK"
    "BASEMEMORY"
    "BEFORE"
    "BIOS"
    "BITMAPICON"
    "BK_BLUE"
    "BK_GREEN"
    "BK_RED"
    "BLUE"
    "BOOTUPDRIVE"
    "CANCEL"
    "CDROM_DRIVE"
    "CDROM"
    "CHECKBOX95"
    "CHECKBOX"
    "CHECKLINE"
    "CHECKMARK"
    "COLORS"
    "COMMANDEX"
    "COMMAND"
    "COMP_NORMAL"
    "COMP_UPDATE_DATE"
    "COMP_UPDATE_SAME"
    "COMP_UPDATE_VERSION"
    "COMPACT"
    "CONTINUE"
    "CPU"
    "CUSTOM"
    "DATE"
    "DEFWINDOWMODE"
    "DIR_WRITEABLE"
    "DIRECTORY"
    "DISABLE"
    "DISK_TOTALSPACE"
    "DISK"
    "DLG_OPTIONS"
    "DLG_PATH"
    "DLG_TEXT"
    "DLG_ASK_YESNO"
    "DLG_ENTER_DISK"
    "DLG_ERR"
    "DLG_INFO_ALTIMAGE"
    "DLG_INFO_CHECKSELECTION"
    "DLG_INFO_KUNITS"
    "DLG_INFO_USEDECIMAL"
    "DLG_MSG_INFORMATION"
    "DLG_MSG_SEVERE"
    "DLG_MSG_WARNING"
    "DLG_STATUS"
    "DLG_WARNING"
    "DLG_USER_CAPTION"
    "DRIVE"
    "ENABLE"
    "END_OF_FILE"
    "END_OF_LIST"
    "ENVSPACE"
    "EQUALS"
    "EXCLUDE_SUBDIR"
    "EXCLUSIVE"
    "EXISTS"
    "EXIT"
    "EXTENDED_MEMORY"
    "EXTENSION_ONLY"
    "FAILIFEXISTS"
    "FALSE"
    "FEEDBACK_FULL"
    "FILE_ATTR_ARCHIVED"
    "FILE_ATTR_DIRECTORY"
    "FILE_ATTR_HIDDEN"
    "FILE_ATTR_NORMAL"
    "FILE_ATTR_READONLY"
    "FILE_ATTR_SYSTEM"
    "FILE_ATTRIBUTE"
    "FILE_DATE"
    "FILE_LINE_LENGTH"
    "FILE_MODE_APPEND"
    "FILE_MODE_BINARYREADONLY"
    "FILE_MODE_BINARY"
    "FILE_MODE_NORMAL"
    "FILE_NO_VERSION"
    "FILE_NOT_FOUND"
    "FILE_SIZE"
    "FILE_TIME"
    "FILENAME_ONLY"
    "FILENAME"
    "FIXED_DRIVE"
    "FOLDER_DESKTOP"
    "FOLDER_PROGRAMS"
    "FOLDER_STARTMENU"
    "FOLDER_STARTUP"
    "FREEENVSPACE"
    "FULLWINDOWMODE"
    "FULL"
    "FONT_TITLE"
    "GREATER_THAN"
    "GREEN"
    "HKEY_CLASSES_ROOT"
    "HKEY_CURRENT_USER"
    "HKEY_LOCAL_MACHINE"
    "HKEY_USERS"
    "HOURGLASS"
    "INCLUDE_SUBDIR"
    "INDVFILESTATUS"
    "INFORMATION"
    "IS_WINDOWSNT"
    "IS_WINDOWS95"
    "IS_WINDOWS"
    "IS_WIN32S"
    "ISTYPE"
    "LANGUAGE_DRV"
    "LANGUAGE"
    "LESS_THAN"
    "LIST_NULL"
    "LISTFIRST"
    "LISTNEXT"
    "LOCKEDFILE"
    "LOGGING"
    "LOWER_LEFT"
    "LOWER_RIGHT"
    "MAGENTA"
    "MOUSE_DRV"
    "MOUSE"
    "NETWORK_DRV"
    "NETWORK"
    "NEXT"
    "NONEXCLUSIVE"
    "NORMALMODE"
    "NOSET"
    "NOTEXISTS"
    "NOWAIT"
    "NO"
    "OFF"
    "ONLYDIR"
    "ON"
    "OSMAJOR"
    "OSMINOR"
    "OS"
    "OTHER_FAILURE"
    "PARALLEL"
    "PARTIAL"
    "PATH_EXISTS"
    "PATH"
    "RED"
    "REGDB_APPPATH_DEFAULT"
    "REGDB_APPPATH"
    "REGDB_BINARY"
    "REGDB_ERR_CONNECTIONEXISTS"
    "REGDB_ERR_CORRUPTEDREGISTRY"
    "REGDB_ERR_INITIALIZATION"
    "REGDB_ERR_INVALIDHANDLE"
    "REGDB_ERR_INVALIDNAME"
    "REGDB_NUMBER"
    "REGDB_STRING_EXPAND"
    "REGDB_STRING_MULTI"
    "REGDB_STRING"
    "REGDB_UNINSTALL_NAME"
    "REMOTE_DRIVE"
    "REMOVEABLE_DRIVE"
    "REPLACE_ITEM"
    "REPLACE"
    "RESET"
    "RESTART"
    "ROOT"
    "SELFREGISTER"
    "SERIAL"
    "SET"
    "SEVERE"
    "SHAREDFILE"
    "SHARE"
    "SILENTMODE"
    "SRCTARGETDIR"
    "STATUSBAR"
    "STATUSDLG"
    "STATUSOLD"
    "STATUS"
    "STYLE_NORMAL"
    "SW_MAXIMIZE"
    "SW_MINIMIZE"
    "SW_RESTORE"
    "SW_SHOW"
    "SYS_BOOTMACHINE"
    "TIME"
    "TRUE"
    "TYPICAL"
    "UPPER_LEFT"
    "UPPER_RIGHT"
    "VALID_PATH"
    "VERSION"
    "VIDEO"
    "VOLUMELABEL"
    "YELLOW"
    "YES"
    "WAIT"
    "WARNING"
    "WINMAJOR"
    "WINMINOR"
    "WIN32SINSTALLED"
    "WIN32SMAJOR"
    "WIN32SMINOR")
  "Function argument constants used in InstallShield 3 and 5."))

(defvar rul-generic-mode-syntax-table nil
  "Syntax table to use in `rul-generic-mode' buffers.")

(setq rul-generic-mode-syntax-table
      (make-syntax-table c++-mode-syntax-table))

(modify-syntax-entry ?\r "> b" rul-generic-mode-syntax-table)
(modify-syntax-entry ?\n "> b" rul-generic-mode-syntax-table)

(modify-syntax-entry ?/  ". 124b" rul-generic-mode-syntax-table)
(modify-syntax-entry ?*  ". 23"   rul-generic-mode-syntax-table)

;; here manually instead
(defun generic-rul-mode-setup-function ()
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (setq imenu-generic-expression
	'((nil "^function\\s-+\\([A-Za-z0-9_]+\\)" 1))
	parse-sexp-ignore-comments t
	comment-end               "*/"
	comment-start	     "/*"
;;; 	comment-end               ""
;;; 	comment-start	     "//"
;;;	comment-start-skip	     ""
	)
  ;; (set-syntax-table rul-generic-mode-syntax-table)
  (setq font-lock-syntax-table rul-generic-mode-syntax-table))

;; moved mode-definition behind defun-definition to be warning-free - 15.11.02/RSan
(define-generic-mode rul-generic-mode
  ;; Using "/*" and "*/" doesn't seem to be working right
  '("//" ("/*" . "*/" ))
  (eval-when-compile installshield-statement-keyword-list)
  (eval-when-compile
    (list
     ;; preprocessor constructs
     '("#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
       1 font-lock-string-face)
     '("#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-reference-face)
       (2 font-lock-variable-name-face nil t))
     ;; indirect string constants
     '("\\(@[A-Za-z][A-Za-z0-9_]+\\)" 1 font-lock-builtin-face)
     ;; gotos
     '("[ \t]*\\(\\sw+:\\)"           1 font-lock-reference-face)
     '("\\<\\(goto\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-reference-face nil t))
     ;; system variables
     (generic-make-keywords-list
      installshield-system-variables-list
      font-lock-variable-name-face "[^_]" "[^_]")
     ;; system functions
     (generic-make-keywords-list
      installshield-system-functions-list
      font-lock-function-name-face "[^_]" "[^_]")
     ;; type keywords
     (generic-make-keywords-list
      installshield-types-list
      font-lock-type-face "[^_]" "[^_]")
     ;; function argument constants
     (generic-make-keywords-list
      installshield-funarg-constants-list
      font-lock-variable-name-face "[^_]" "[^_]"))) ; is this face the best choice?
  '("\\.[rR][uU][lL]\\'")
  '(generic-rul-mode-setup-function)
  "Generic mode for InstallShield RUL files.")

(define-skeleton rul-if
  "Insert an if statement."
  "condition: "
  "if(" str ") then" \n
  > _ \n
  ( "other condition, %s: "
    > "elseif(" str ") then" \n
    > \n)
  > "else" \n
  > \n
  resume:
  > "endif;")

(define-skeleton rul-function
  "Insert a function statement."
  "function: "
  "function " str " ()" \n
  ( "local variables, %s: "
    > "  " str ";" \n)
  > "begin" \n
  > _ \n
  resume:
  > "end;"))

;; Additions by ACorreir@pervasive-sw.com (Alfred Correira)
(when (memq 'mailrc-generic-mode generic-extras-enable-list)

(define-generic-mode mailrc-generic-mode
  '(?#)
  '("alias"
    "else"
    "endif"
    "group"
    "if"
    "ignore"
    "set"
    "source"
    "unset")
  '(("^\\s-*\\(alias\\|group\\)\\s-+\\([-A-Za-z0-9_]+\\)\\s-+\\([^\n\r#]*\\)\\(#.*\\)?$"
     (2 font-lock-constant-face)
     (3 font-lock-variable-name-face))
    ("^\\s-*\\(unset\\|set\\|ignore\\)\\s-+\\([-A-Za-z0-9_]+\\)=?\\([^\n\r#]*\\)\\(#.*\\)?$"
     (2 font-lock-constant-face)
     (3 font-lock-variable-name-face))
    ("^\\s-*\\(source\\)\\s-+\\([^\n\r#]*\\)\\(#.*\\)?$"
     (2 font-lock-variable-name-face)))
  '("\\.mailrc\\'")
  nil
  "Mode for mailrc files."))

;; Inetd.conf
(when (memq 'inetd-conf-generic-mode generic-extras-enable-list)

(define-generic-mode inetd-conf-generic-mode
  '(?#)
  '("stream"
    "dgram"
    "tcp"
    "udp"
    "wait"
    "nowait"
    "internal")
  '(("^\\([-A-Za-z0-9_]+\\)" 1 font-lock-type-face))
  '("/etc/inetd.conf\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\)" 1))))))))

;; Services
(when (memq 'etc-services-generic-mode generic-extras-enable-list)

(define-generic-mode etc-services-generic-mode
  '(?#)
  '("tcp"
    "udp"
    "ddp")
  '(("^\\([-A-Za-z0-9_]+\\)\\s-+\\([0-9]+\\)/"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face)))
  '("/etc/services\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\)" 1))))))))

;; Password and Group files
(when (memq 'etc-passwd-generic-mode generic-extras-enable-list)

(define-generic-mode etc-passwd-generic-mode
  nil              ;; No comment characters
  '("root")        ;; Only one keyword
  (eval-when-compile
    (list
     (list
      (concat
       "^"
       ;; User name -- Never blank!
       "\\([^:]+\\)"
       ":"
       ;; Password, UID and GID
       (mapconcat
	'identity
	(make-list 3 "\\([^:]+\\)")
	":")
       ":"
       ;; GECOS/Name -- might be blank
       "\\([^:]*\\)"
       ":"
       ;; Home directory and shell
       "\\([^:]+\\)"
       ":?"
       "\\([^:]*\\)"
       "$")
      '(1 font-lock-type-face)
      '(5 font-lock-variable-name-face)
      '(6 font-lock-constant-face)
      '(7 font-lock-warning-face))
     '("^\\([^:]+\\):\\([^:]*\\):\\([0-9]+\\):\\(.*\\)$"
       (1 font-lock-type-face)
       (4 font-lock-variable-name-face))))
  '("/etc/passwd\\'" "/etc/group\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\):" 1))))))))

;; Fstab
(when (memq 'etc-fstab-generic-mode generic-extras-enable-list)

(define-generic-mode etc-fstab-generic-mode
  '(?#)
  '("adfs"
    "affs"
    "autofs"
    "coda"
    "coherent"
    "cramfs"
    "devpts"
    "efs"
    "ext2"
    "ext3"
    "ext4"
    "hfs"
    "hpfs"
    "iso9660"
    "jfs"
    "minix"
    "msdos"
    "ncpfs"
    "nfs"
    "ntfs"
    "proc"
    "qnx4"
    "reiserfs"
    "romfs"
    "smbfs"
    "cifs"
    "usbdevfs"
    "sysv"
    "sysfs"
    "tmpfs"
    "udf"
    "ufs"
    "umsdos"
    "vfat"
    "xenix"
    "xfs"
    "swap"
    "auto"
    "ignore")
  '(("^\\([^# \t]+\\)\\s-+\\([^# \t]+\\)"
     (1 font-lock-type-face t)
     (2 font-lock-variable-name-face t)))
  '("/etc/[v]*fstab\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([^# \t]+\\)\\s-+" 1))))))))

;; /etc/sudoers
(when (memq 'etc-sudoers-generic-mode generic-extras-enable-list)

(define-generic-mode etc-sudoers-generic-mode
  '(?#)
  '("User_Alias" "Runas_Alias" "Host_Alias"  "Cmnd_Alias"
    "NOPASSWD" "PASSWD" "NOEXEC" "EXEC"
    "ALL")
  '(("\\<\\(root\\|su\\)\\>" 1 font-lock-warning-face)
    ("\\(\\*\\)" 1 font-lock-warning-face)
    ("\\<\\(%[A-Za-z0-9_]+\\)\\>" 1 font-lock-variable-name-face))
  '("/etc/sudoers\\'")
  nil
  "Generic mode for sudoers configuration files."))

;; From Jacques Duthen <jacques.duthen@sncf.fr>
(when (memq 'show-tabs-generic-mode generic-extras-enable-list)

(eval-when-compile

(defconst show-tabs-generic-mode-font-lock-defaults-1
  '(;; trailing spaces must come before...
    ("[ \t]+$" . 'show-tabs-space)
    ;; ...embedded tabs
    ("[^\n\t]\\(\t+\\)" (1 'show-tabs-tab))))

(defconst show-tabs-generic-mode-font-lock-defaults-2
  '(;; trailing spaces must come before...
    ("[ \t]+$" . 'show-tabs-space)
    ;; ...tabs
    ("\t+" . 'show-tabs-tab))))

(defface show-tabs-tab
  '((((class grayscale) (background light)) (:background "DimGray"   :weight bold))
    (((class grayscale) (background dark))  (:background "LightGray" :weight bold))
    (((class color)     (min-colors 88))    (:background "red1"))
    (((class color))                        (:background "red"))
    (t (:weight bold)))
  "Font Lock mode face used to highlight TABs."
  :group 'generic-x)
(define-obsolete-face-alias 'show-tabs-tab-face 'show-tabs-tab "22.1")

(defface show-tabs-space
  '((((class grayscale) (background light)) (:background "DimGray"   :weight bold))
    (((class grayscale) (background dark))  (:background "LightGray" :weight bold))
    (((class color)     (min-colors 88))    (:background "yellow1"))
    (((class color))                        (:background "yellow"))
    (t (:weight bold)))
  "Font Lock mode face used to highlight spaces."
  :group 'generic-x)
(define-obsolete-face-alias 'show-tabs-space-face 'show-tabs-space "22.1")

(define-generic-mode show-tabs-generic-mode
  nil ;; no comment char
  nil ;; no keywords
  (eval-when-compile show-tabs-generic-mode-font-lock-defaults-1)
  nil ;; no auto-mode-alist
  ;; '(show-tabs-generic-mode-hook-fun)
  nil
  "Generic mode to show tabs and trailing spaces."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DNS modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'named-boot-generic-mode generic-extras-enable-list)

(define-generic-mode named-boot-generic-mode
  ;; List of comment characters
  '(?\;)
  ;; List of keywords
  '("cache" "primary" "secondary" "forwarders" "limit" "options"
    "directory" "check-names")
  ;; List of additional font-lock-expressions
  '(("\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face)
    ("^directory\\s-+\\(.*\\)"              1 font-lock-variable-name-face)
    ("^\\(primary\\|cache\\)\\s-+\\([.A-Za-z]+\\)\\s-+\\(.*\\)"
     (2 font-lock-variable-name-face)
     (3 font-lock-constant-face)))
  ;; List of additional automode-alist expressions
  '("/etc/named.boot\\'")
  ;; List of set up functions to call
  nil))

(when (memq 'named-database-generic-mode generic-extras-enable-list)

(define-generic-mode named-database-generic-mode
  ;; List of comment characters
  '(?\;)
  ;; List of keywords
  '("IN" "NS" "CNAME" "SOA" "PTR" "MX" "A")
  ;; List of additional font-lock-expressions
  '(("\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face)
    ("^\\([.A-Za-z0-9]+\\)"                 1 font-lock-variable-name-face))
  ;; List of additional auto-mode-alist expressions
  nil
  ;; List of set up functions to call
  nil)

(defvar named-database-time-string "%Y%m%d%H"
  "Timestring for named serial numbers.")

(defun named-database-print-serial ()
  "Print a serial number based on the current date."
  (interactive)
  (insert (format-time-string named-database-time-string (current-time)))))

(when (memq 'resolve-conf-generic-mode generic-extras-enable-list)

(define-generic-mode resolve-conf-generic-mode
  ;; List of comment characters
  '(?#)
  ;; List of keywords
  '("nameserver" "domain" "search" "sortlist" "options")
  ;; List of additional font-lock-expressions
  nil
  ;; List of additional auto-mode-alist expressions
  '("/etc/resolv[e]?.conf\\'")
  ;; List of set up functions to call
  nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes for spice and common electrical engineering circuit netlist formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'spice-generic-mode generic-extras-enable-list)

(define-generic-mode spice-generic-mode
  nil
  '("and"
    "cccs"
    "ccvs"
    "delay"
    "nand"
    "nor"
    "npwl"
    "or"
    "par"
    "ppwl"
    "pwl"
    "vccap"
    "vccs"
    "vcr"
    "vcvs")
  '(("^\\s-*\\([*].*\\)"                1 font-lock-comment-face)
    (" \\(\\$ .*\\)$"                   1 font-lock-comment-face)
    ("^\\(\\$ .*\\)$"                   1 font-lock-comment-face)
    ("\\([*].*\\)"                      1 font-lock-comment-face)
    ("^\\([+]\\)"                       1 font-lock-string-face)
    ("^\\s-*\\([.]\\w+\\>\\)"           1 font-lock-keyword-face)
    ("\\(\\([.]\\|_\\|\\w\\)+\\)\\s-*=" 1 font-lock-variable-name-face)
    ("\\('[^']+'\\)"                    1 font-lock-string-face)
    ("\\(\"[^\"]+\"\\)"                 1 font-lock-string-face))
  '("\\.[sS][pP]\\'"
    "\\.[sS][pP][iI]\\'"
    "\\.[sS][pP][iI][cC][eE]\\'"
    "\\.[iI][nN][cC]\\'")
  (list
   'generic-bracket-support
   ;; Make keywords case-insensitive
   (function
    (lambda()
      (setq font-lock-defaults '(generic-font-lock-keywords nil t)))))
  "Generic mode for SPICE circuit netlist files."))

(when (memq 'ibis-generic-mode generic-extras-enable-list)

(define-generic-mode ibis-generic-mode
  '(?|)
  nil
  '(("[[]\\([^]]*\\)[]]"          1 font-lock-keyword-face)
    ("\\(\\(_\\|\\w\\)+\\)\\s-*=" 1 font-lock-variable-name-face))
  '("\\.[iI][bB][sS]\\'")
  '(generic-bracket-support)
  "Generic mode for IBIS circuit netlist files."))

(when (memq 'astap-generic-mode generic-extras-enable-list)

(define-generic-mode astap-generic-mode
  nil
  '("analyze"
    "description"
    "elements"
    "execution"
    "features"
    "functions"
    "ground"
    "model"
    "outputs"
    "print"
    "run"
    "controls"
    "table")
  '(("^\\s-*\\([*].*\\)"      1 font-lock-comment-face)
    (";\\s-*\\([*].*\\)"      1 font-lock-comment-face)
    ("^\\s-*\\([.]\\w+\\>\\)" 1 font-lock-keyword-face)
    ("\\('[^']+'\\)"          1 font-lock-string-face)
    ("\\(\"[^\"]+\"\\)"       1 font-lock-string-face)
    ("[(,]\\s-*\\(\\([.]\\|_\\|\\w\\)+\\)\\s-*=" 1 font-lock-variable-name-face))
  '("\\.[aA][pP]\\'"
    "\\.[aA][sS][xX]\\'"
    "\\.[aA][sS][tT][aA][pP]\\'"
    "\\.[pP][sS][pP]\\'"
    "\\.[dD][eE][cC][kK]\\'"
    "\\.[gG][oO][dD][aA][tT][aA]")
  (list
   'generic-bracket-support
   ;; Make keywords case-insensitive
   (function
    (lambda()
      (setq font-lock-defaults '(generic-font-lock-keywords nil t)))))
  "Generic mode for ASTAP circuit netlist files."))

(when (memq 'etc-modules-conf-generic-mode generic-extras-enable-list)

(define-generic-mode etc-modules-conf-generic-mode
  ;; List of comment characters
  '(?#)
  ;; List of keywords
  '("above"
    "alias"
    "below"
    "define"
    "depfile"
    "else"
    "elseif"
    "endif"
    "if"
    "include"
    "insmod_opt"
    "install"
    "keep"
    "options"
    "path"
    "generic_stringfile"
    "pcimapfile"
    "isapnpmapfile"
    "usbmapfile"
    "parportmapfile"
    "ieee1394mapfile"
    "pnpbiosmapfile"
    "probe"
    "probeall"
    "prune"
    "post-install"
    "post-remove"
    "pre-install"
    "pre-remove"
    "remove"
    "persistdir")
  ;; List of additional font-lock-expressions
  nil
  ;; List of additional automode-alist expressions
  '("/etc/modules.conf" "/etc/conf.modules")
  ;; List of set up functions to call
  nil))

(provide 'generic-x)

;;; generic-x.el ends here
