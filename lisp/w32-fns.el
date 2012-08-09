;;; w32-fns.el --- Lisp routines for 32-bit Windows

;; Copyright (C) 1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Geoff Voelker <voelker@cs.washington.edu>
;; Keywords: internal
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


;;; Code:
(require 'w32-vars)

(defvar explicit-shell-file-name)

;;;; Function keys

(declare-function set-message-beep "w32console.c")
(declare-function w32-get-clipboard-data "w32select.c")
(declare-function w32-get-locale-info "w32proc.c")
(declare-function w32-get-valid-locale-ids "w32proc.c")
(declare-function w32-set-clipboard-data "w32select.c")

;; Map all versions of a filename (8.3, longname, mixed case) to the
;; same buffer.
(setq find-file-visit-truename t)

(declare-function x-server-version "w32fns.c" (&optional display))

(defun w32-version ()
  "Return the MS-Windows version numbers.
The value is a list of three integers: the major and minor version
numbers, and the build number."
  (x-server-version))

(defun w32-using-nt ()
  "Return non-nil if running on a Windows NT descendant.
That includes all Windows systems except for 9X/Me."
  (and (eq system-type 'windows-nt) (getenv "SystemRoot")))

(defun w32-shell-name ()
  "Return the name of the shell being used."
  (or (bound-and-true-p shell-file-name)
      (getenv "ESHELL")
      (getenv "SHELL")
      (and (w32-using-nt) "cmd.exe")
      "command.com"))

(defun w32-system-shell-p (shell-name)
  (and shell-name
       (member (downcase (file-name-nondirectory shell-name))
	       w32-system-shells)))

(defun w32-shell-dos-semantics ()
  "Return non-nil if the interactive shell being used expects MS-DOS shell semantics."
  (or (w32-system-shell-p (w32-shell-name))
      (and (member (downcase (file-name-nondirectory (w32-shell-name)))
		   '("cmdproxy" "cmdproxy.exe"))
	   (w32-system-shell-p (getenv "COMSPEC")))))

(defvar w32-quote-process-args)  ;; defined in w32proc.c

(defun w32-check-shell-configuration ()
  "Check the configuration of shell variables on Windows.
This function is invoked after loading the init files and processing
the command line arguments.  It issues a warning if the user or site
has configured the shell with inappropriate settings."
  (interactive)
  (let ((prev-buffer (current-buffer))
	(buffer (get-buffer-create "*Shell Configuration*"))
	(system-shell))
    (set-buffer buffer)
    (erase-buffer)
    (if (w32-system-shell-p (getenv "ESHELL"))
	(insert (format "Warning! The ESHELL environment variable uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			(getenv "ESHELL"))))
    (if (w32-system-shell-p (getenv "SHELL"))
	(insert (format "Warning! The SHELL environment variable uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			(getenv "SHELL"))))
    (if (w32-system-shell-p shell-file-name)
	(insert (format "Warning! shell-file-name uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			shell-file-name)))
    (if (and (boundp 'explicit-shell-file-name)
	     (w32-system-shell-p explicit-shell-file-name))
	(insert (format "Warning! explicit-shell-file-name uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			explicit-shell-file-name)))
    (setq system-shell (> (buffer-size) 0))

    ;; Allow user to specify that they really do want to use one of the
    ;; "system" shells, despite the drawbacks, but still warn if
    ;; shell-command-switch doesn't match.
    (if w32-allow-system-shell
	(erase-buffer))

    (cond (system-shell
	   ;; System shells.
	   (if (string-equal "-c" shell-command-switch)
	       (insert "Warning! shell-command-switch is \"-c\".
You should set this to \"/c\" when using a system shell.\n\n"))
	   (if w32-quote-process-args
	       (insert "Warning! w32-quote-process-args is t.
You should set this to nil when using a system shell.\n\n")))
	  ;; Non-system shells.
	  (t
	   (if (string-equal "/c" shell-command-switch)
	       (insert "Warning! shell-command-switch is \"/c\".
You should set this to \"-c\" when using a non-system shell.\n\n"))
	   (if (not w32-quote-process-args)
	       (insert "Warning! w32-quote-process-args is nil.
You should set this to t when using a non-system shell.\n\n"))))
    (if (> (buffer-size) 0)
	(display-buffer buffer)
      (kill-buffer buffer))
    (set-buffer prev-buffer)))

(add-hook 'after-init-hook 'w32-check-shell-configuration)

;; Override setting chosen at startup.
(defun set-default-process-coding-system ()
  ;; Most programs on Windows will accept Unix line endings on input
  ;; (and some programs ported from Unix require it) but most will
  ;; produce DOS line endings on output.
  (setq default-process-coding-system
	(if (default-value 'enable-multibyte-characters)
	    '(undecided-dos . undecided-unix)
	  '(raw-text-dos . raw-text-unix)))
  ;; Make cmdproxy default to using DOS line endings for input,
  ;; because some Windows programs (including command.com) require it.
  (add-to-list 'process-coding-system-alist
	       `("[cC][mM][dD][pP][rR][oO][xX][yY]"
		 . ,(if (default-value 'enable-multibyte-characters)
			'(undecided-dos . undecided-dos)
		      '(raw-text-dos . raw-text-dos))))
  ;; plink needs DOS input when entering the password.
  (add-to-list 'process-coding-system-alist
	       `("[pP][lL][iI][nN][kK]"
		 . ,(if (default-value 'enable-multibyte-characters)
			'(undecided-dos . undecided-dos)
		      '(raw-text-dos . raw-text-dos)))))

(add-hook 'before-init-hook 'set-default-process-coding-system)


;;; Basic support functions for managing Emacs's locale setting

(defvar w32-valid-locales nil
  "List of locale ids known to be supported.")

;; This is the brute-force version; an efficient version is now
;; built-in though.
(if (not (fboundp 'w32-get-valid-locale-ids))
    (defun w32-get-valid-locale-ids ()
      "Return list of all valid Windows locale ids."
      (let ((i 65535)
	    locales)
	(while (> i 0)
	  (if (w32-get-locale-info i)
	      (setq locales (cons i locales)))
	  (setq i (1- i)))
	locales)))

(defun w32-list-locales ()
  "List the name and id of all locales supported by Windows."
  (interactive)
  (when (null w32-valid-locales)
    (setq w32-valid-locales (sort (w32-get-valid-locale-ids) #'<)))
  (with-output-to-temp-buffer "*Supported Locales*"
    (princ "LCID\tAbbrev\tFull name\n\n")
    (dolist (locale w32-valid-locales)
      (princ (format "%d\t%s\t%s\n"
		     locale
		     (w32-get-locale-info locale)
		     (w32-get-locale-info locale t))))))

;; Setup Info-default-directory-list to include the info directory
;; near where Emacs executable was installed.  We used to set INFOPATH,
;; but when this is set Info-default-directory-list is ignored.  We
;; also cannot rely upon what is set in paths.el because they assume
;; that configuration during build time is correct for runtime.
(defun w32-init-info ()
  (let* ((instdir (file-name-directory invocation-directory))
	 (dir1 (expand-file-name "../info/" instdir))
	 (dir2 (expand-file-name "../../../info/" instdir)))
    (if (file-exists-p dir1)
	(setq Info-default-directory-list
	      (append Info-default-directory-list (list dir1)))
      (if (file-exists-p dir2)
	  (setq Info-default-directory-list
		(append Info-default-directory-list (list dir2)))))))

(add-hook 'before-init-hook 'w32-init-info)

;; The variable source-directory is used to initialize Info-directory-list.
;; However, the common case is that Emacs is being used from a binary
;; distribution, and the value of source-directory is meaningless in that
;; case.  Even worse, source-directory can refer to a directory on a drive
;; on the build machine that happens to be a removable drive on the user's
;; machine.  When this happens, Emacs tries to access the removable drive
;; and produces the abort/retry/ignore dialog.  Since we do not use
;; source-directory, set it to something that is a reasonable approximation
;; on the user's machine.

;;(add-hook 'before-init-hook
;;	  (lambda ()
;;	    (setq source-directory (file-name-as-directory
;;				     (expand-file-name ".." exec-directory)))))

(defun w32-convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for MS-Windows.
This means to guarantee valid names and perhaps to canonicalize
certain patterns.

This function is called by `convert-standard-filename'.

Replace invalid characters and turn Cygwin names into native
names, and also turn slashes into backslashes if the shell
requires it (see `w32-shell-dos-semantics')."
  (save-match-data
    (let ((name
	   (if (string-match "\\`/cygdrive/\\([a-zA-Z]\\)/" filename)
               (replace-match "\\1:/" t nil filename)
             (copy-sequence filename)))
	  (start 0))
      ;; leave ':' if part of drive specifier
      (if (and (> (length name) 1)
	       (eq (aref name 1) ?:))
	  (setq start 2))
      ;; destructively replace invalid filename characters with !
      (while (string-match "[?*:<>|\"\000-\037]" name start)
	(aset name (match-beginning 0) ?!)
	(setq start (match-end 0)))
      ;; convert directory separators to Windows format
      ;; (but only if the shell in use requires it)
      (when (w32-shell-dos-semantics)
	(setq start 0)
	(while (string-match "/" name start)
	  (aset name (match-beginning 0) ?\\)
	  (setq start (match-end 0))))
      name)))

;;; Fix interface to (X-specific) mouse.el
(defun x-set-selection (type data)
  "Make an X selection of type TYPE and value DATA.
The argument TYPE (nil means `PRIMARY') says which selection, and
DATA specifies the contents.  TYPE must be a symbol.  \(It can also
be a string, which stands for the symbol with that name, but this
is considered obsolete.)  DATA may be a string, a symbol, an
integer (or a cons of two integers or list of two integers).

The selection may also be a cons of two markers pointing to the same buffer,
or an overlay.  In these cases, the selection is considered to be the text
between the markers *at whatever time the selection is examined*.
Thus, editing done in the buffer after you specify the selection
can alter the effective value of the selection.

The data may also be a vector of valid non-vector selection values.

The return value is DATA.

Interactively, this command sets the primary selection.  Without
prefix argument, it reads the selection in the minibuffer.  With
prefix argument, it uses the text of the region as the selection value.

Note that on MS-Windows, primary and secondary selections set by Emacs
are not available to other programs."
  (put 'x-selections (or type 'PRIMARY) data))

(defun x-get-selection (&optional type _data-type)
  "Return the value of an X Windows selection.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING') says
how to convert the data.

TYPE may be any symbol \(but nil stands for `PRIMARY').  However,
only a few symbols are commonly used.  They conventionally have
all upper-case names.  The most often used ones, in addition to
`PRIMARY', are `SECONDARY' and `CLIPBOARD'.

DATA-TYPE is usually `STRING', but can also be one of the symbols
in `selection-converter-alist', which see."
  (get 'x-selections (or type 'PRIMARY)))

;; x-selection-owner-p is used in simple.el
(defun x-selection-owner-p (&optional type)
  (and (memq type '(nil PRIMARY SECONDARY))
       (get 'x-selections (or type 'PRIMARY))))

(defun set-w32-system-coding-system (coding-system)
  "Set the coding system used by the Windows system to CODING-SYSTEM.
This is used for things like passing font names with non-ASCII
characters in them to the system.  For a list of possible values of
CODING-SYSTEM, use \\[list-coding-systems].

This function is provided for backward compatibility, since
`w32-system-coding-system' is now an alias for `locale-coding-system'."
  (interactive
   (list (let ((default locale-coding-system))
           (read-coding-system
            (format "Coding system for system calls (default %s): "
                    default)
            default))))
  (check-coding-system coding-system)
  (setq locale-coding-system coding-system))

;; locale-coding-system was introduced to do the same thing as
;; w32-system-coding-system. Use that instead.
(defvaralias 'w32-system-coding-system 'locale-coding-system)

;; Set to a system sound if you want a fancy bell.
(set-message-beep nil)

;; The "Windows" keys on newer keyboards bring up the Start menu
;; whether you want it or not - make Emacs ignore these keystrokes
;; rather than beep.
(global-set-key [lwindow] 'ignore)
(global-set-key [rwindow] 'ignore)

(defvar w32-charset-info-alist)		; w32font.c

(defun w32-add-charset-info (xlfd-charset windows-charset codepage)
  "Function to add character sets to display with Windows fonts.
Creates entries in `w32-charset-info-alist'.
XLFD-CHARSET is a string which will appear in the XLFD font name to
identify the character set.  WINDOWS-CHARSET is a symbol identifying
the Windows character set this maps to.  For the list of possible
values, see the documentation for `w32-charset-info-alist'.  CODEPAGE
can be a numeric codepage that Windows uses to display the character
set, t for Unicode output with no codepage translation or nil for 8
bit output with no translation."
  (add-to-list 'w32-charset-info-alist
               (cons xlfd-charset (cons windows-charset codepage))))

;; The last charset we add becomes the "preferred" charset for the return
;; value from w32-select-font etc, so list the most important charsets last.
(w32-add-charset-info "iso8859-14" 'w32-charset-ansi  28604)
(w32-add-charset-info "iso8859-15" 'w32-charset-ansi  28605)
;; The following two are included for pattern matching.
(w32-add-charset-info "jisx0201" 'w32-charset-shiftjis 932)
(w32-add-charset-info "jisx0208" 'w32-charset-shiftjis 932)
(w32-add-charset-info "jisx0201-latin" 'w32-charset-shiftjis 932)
(w32-add-charset-info "jisx0201-katakana" 'w32-charset-shiftjis 932)
(w32-add-charset-info "ksc5601.1989" 'w32-charset-hangeul 949)
(w32-add-charset-info "big5" 'w32-charset-chinesebig5 950)
(w32-add-charset-info "gb2312.1980" 'w32-charset-gb2312 936)
(w32-add-charset-info "ms-symbol" 'w32-charset-symbol nil)
(w32-add-charset-info "ms-oem" 'w32-charset-oem 437)
(w32-add-charset-info "ms-oemlatin" 'w32-charset-oem 850)
(w32-add-charset-info "iso8859-2" 'w32-charset-easteurope 28592)
(w32-add-charset-info "iso8859-3" 'w32-charset-turkish 28593)
(w32-add-charset-info "iso8859-4" 'w32-charset-baltic 28594)
(w32-add-charset-info "iso8859-6" 'w32-charset-arabic 28596)
(w32-add-charset-info "iso8859-7" 'w32-charset-greek 28597)
(w32-add-charset-info "iso8859-8" 'w32-charset-hebrew 1255)
(w32-add-charset-info "iso8859-9" 'w32-charset-turkish 1254)
(w32-add-charset-info "iso8859-13" 'w32-charset-baltic 1257)
(w32-add-charset-info "koi8-r" 'w32-charset-russian 20866)
(w32-add-charset-info "iso8859-5" 'w32-charset-russian 28595)
(w32-add-charset-info "tis620-2533" 'w32-charset-thai 874)
(w32-add-charset-info "windows-1258" 'w32-charset-vietnamese 1258)
(w32-add-charset-info "ksc5601.1992" 'w32-charset-johab 1361)
(w32-add-charset-info "mac-roman" 'w32-charset-mac 10000)
(w32-add-charset-info "iso10646-1" 'w32-charset-default t)

;;   ;; If Unicode Windows charset is not defined, use ansi fonts.
;;   (w32-add-charset-info "iso10646-1" 'w32-charset-ansi t))

;; Preferred names
(w32-add-charset-info "big5-0" 'w32-charset-chinesebig5 950)
(w32-add-charset-info "gb2312.1980-0" 'w32-charset-gb2312 936)
(w32-add-charset-info "jisx0208-sjis" 'w32-charset-shiftjis 932)
(w32-add-charset-info "ksc5601.1987-0" 'w32-charset-hangeul 949)
(w32-add-charset-info "tis620-0" 'w32-charset-thai 874)
(w32-add-charset-info "iso8859-1" 'w32-charset-ansi 1252)

(make-obsolete-variable 'w32-enable-italics
                        'w32-enable-synthesized-fonts "21.1")
(make-obsolete-variable 'w32-charset-to-codepage-alist
                        'w32-charset-info-alist "21.1")


;;;; Selections

;; We keep track of the last text selected here, so we can check the
;; current selection against it, and avoid passing back our own text
;; from x-selection-value.
(defvar x-last-selected-text nil)

(defun x-get-selection-value ()
  "Return the value of the current selection.
Consult the selection.  Treat empty strings as if they were unset."
  (if x-select-enable-clipboard
      (let (text)
	;; Don't die if x-get-selection signals an error.
	(condition-case c
	    (setq text (w32-get-clipboard-data))
	  (error (message "w32-get-clipboard-data:%s" c)))
	(if (string= text "") (setq text nil))
	(cond
	 ((not text) nil)
	 ((eq text x-last-selected-text) nil)
	 ((string= text x-last-selected-text)
	  ;; Record the newer string, so subsequent calls can use the 'eq' test.
	  (setq x-last-selected-text text)
	  nil)
	 (t
	  (setq x-last-selected-text text))))))

(defalias 'x-selection-value 'x-get-selection-value)

;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)


;;;; Support for build process

;; From autoload.el
(defvar autoload-make-program)
(defvar generated-autoload-file)

(defun w32-batch-update-autoloads ()
  "Like `batch-update-autoloads', but takes the name of the autoloads file
from the command line.

This is required because some Windows build environments, such as MSYS,
munge command-line arguments that include file names to a horrible mess
that Emacs is unable to cope with."
  (let ((generated-autoload-file
	 (expand-file-name (pop command-line-args-left)))
	;; I can only assume the same considerations may apply here...
	(autoload-make-program (pop command-line-args-left)))
    (batch-update-autoloads)))

(defun w32-append-code-lines (orig extra)
  "Append non-empty non-comment lines in the file EXTRA to the file ORIG.

This function saves all buffers and kills the Emacs session, without asking
for any permissions.

This is required because the Windows build environment is not required
to include Sed, which is used by leim/Makefile.in to do the job."
  (find-file orig)
  (goto-char (point-max))
  (insert-file-contents extra)
  (delete-matching-lines "^$\\|^;")
  (save-buffers-kill-emacs t))

;;; w32-fns.el ends here
