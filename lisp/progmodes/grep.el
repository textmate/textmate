;;; grep.el --- run `grep' and display the results

;; Copyright (C) 1985-1987, 1993-1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Maintainer: FSF
;; Keywords: tools, processes

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

;; This package provides the grep facilities documented in the Emacs
;; user's manual.

;;; Code:

(require 'compile)


(defgroup grep nil
  "Run `grep' and display the results."
  :group 'tools
  :group 'processes)

(defvar grep-host-defaults-alist nil
  "Default values depending on target host.
`grep-compute-defaults' returns default values for every local or
remote host `grep' runs.  These values can differ from host to
host.  Once computed, the default values are kept here in order
to avoid computing them again.")

(defun grep-apply-setting (symbol value)
  "Set SYMBOL to VALUE, and update `grep-host-defaults-alist'.
SYMBOL should be one of `grep-command', `grep-template',
`grep-use-null-device', `grep-find-command',
`grep-find-template', `grep-find-use-xargs', or
`grep-highlight-matches'."
  (when grep-host-defaults-alist
    (let* ((host-id
	    (intern (or (file-remote-p default-directory) "localhost")))
	   (host-defaults (assq host-id grep-host-defaults-alist))
	   (defaults (assq nil grep-host-defaults-alist)))
      (setcar (cdr (assq symbol host-defaults)) value)
      (setcar (cdr (assq symbol defaults)) value)))
  (set-default symbol value))

;;;###autoload
(defcustom grep-window-height nil
  "*Number of lines in a grep window.  If nil, use `compilation-window-height'."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1"
  :group 'grep)

(defcustom grep-highlight-matches 'auto-detect
  "Use special markers to highlight grep matches.

Some grep programs are able to surround matches with special
markers in grep output.  Such markers can be used to highlight
matches in grep mode.  This requires `font-lock-mode' to be active
in grep buffers, so if you have globally disabled font-lock-mode,
you will not get highlighting.

This option sets the environment variable GREP_COLORS to specify
markers for highlighting and GREP_OPTIONS to add the --color
option in front of any explicit grep options before starting
the grep.

When this option is `auto', grep uses `--color=auto' to highlight
matches only when it outputs to a terminal (when `grep' is the last
command in the pipe), thus avoiding the use of any potentially-harmful
escape sequences when standard output goes to a file or pipe.

To make grep highlight matches even into a pipe, you need the option
`always' that forces grep to use `--color=always' to unconditionally
output escape sequences.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults' when the default value is `auto-detect'.
To change the default value, use Customize or call the function
`grep-apply-setting'."
  :type '(choice (const :tag "Do not highlight matches with grep markers" nil)
		 (const :tag "Highlight matches with grep markers" t)
		 (const :tag "Use --color=always" always)
		 (const :tag "Use --color=auto" auto)
		 (other :tag "Not Set" auto-detect))
  :set 'grep-apply-setting
  :version "22.1"
  :group 'grep)

(defcustom grep-scroll-output nil
  "*Non-nil to scroll the *grep* buffer window as output appears.

Setting it causes the grep commands to put point at the end of their
output window so that the end of the output is always visible rather
than the beginning."
  :type 'boolean
  :version "22.1"
  :group 'grep)

;;;###autoload
(defcustom grep-command nil
  "The default grep command for \\[grep].
If the grep program used supports an option to always include file names
in its output (such as the `-H' option to GNU grep), it's a good idea to
include it when specifying `grep-command'.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set 'grep-apply-setting
  :group 'grep)

(defcustom grep-template nil
  "The default command to run for \\[lgrep].
The following place holders should be present in the string:
 <C> - place to put -i if case insensitive grep.
 <F> - file names and wildcards to search.
 <X> - file names and wildcards to exclude.
 <R> - the regular expression searched for.
 <N> - place to insert null-device.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set 'grep-apply-setting
  :version "22.1"
  :group 'grep)

(defcustom grep-use-null-device 'auto-detect
  "If t, append the value of `null-device' to `grep' commands.
This is done to ensure that the output of grep includes the filename of
any match in the case where only a single file is searched, and is not
necessary if the grep program used supports the `-H' option.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice (const :tag "Do Not Append Null Device" nil)
		 (const :tag "Append Null Device" t)
		 (other :tag "Not Set" auto-detect))
  :set 'grep-apply-setting
  :group 'grep)

;;;###autoload
(defcustom grep-find-command nil
  "The default find command for \\[grep-find].
In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set 'grep-apply-setting
  :group 'grep)

(defcustom grep-find-template nil
  "The default command to run for \\[rgrep].
The following place holders should be present in the string:
 <D> - base directory for find
 <X> - find options to restrict or expand the directory list
 <F> - find options to limit the files matched
 <C> - place to put -i if case insensitive grep
 <R> - the regular expression searched for.
In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set 'grep-apply-setting
  :version "22.1"
  :group 'grep)

(defcustom grep-files-aliases
  '(("all" .   "* .*")
    ("el" .    "*.el")
    ("ch" .    "*.[ch]")
    ("c" .     "*.c")
    ("cc" .    "*.cc *.cxx *.cpp *.C *.CC *.c++")
    ("cchh" .  "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
    ("hh" .    "*.hxx *.hpp *.[Hh] *.HH *.h++")
    ("h" .     "*.h")
    ("l" .     "[Cc]hange[Ll]og*")
    ("m" .     "[Mm]akefile*")
    ("tex" .   "*.tex")
    ("texi" .  "*.texi")
    ("asm" .   "*.[sS]"))
  "*Alist of aliases for the FILES argument to `lgrep' and `rgrep'."
  :type 'alist
  :group 'grep)

(defcustom grep-find-ignored-directories
  vc-directory-exclusion-list
  "*List of names of sub-directories which `rgrep' shall not recurse into.
If an element is a cons cell, the car is called on the search directory
to determine whether cdr should not be recursed into."
  :type '(choice (repeat :tag "Ignored directories" string)
		 (const :tag "No ignored directories" nil))
  :group 'grep)

(defcustom grep-find-ignored-files
  (cons ".#*" (delq nil (mapcar (lambda (s)
				  (unless (string-match-p "/\\'" s)
				    (concat "*" s)))
				completion-ignored-extensions)))
  "*List of file names which `rgrep' and `lgrep' shall exclude.
If an element is a cons cell, the car is called on the search directory
to determine whether cdr should not be excluded."
  :type '(choice (repeat :tag "Ignored file" string)
		 (const :tag "No ignored files" nil))
  :group 'grep)

(defcustom grep-error-screen-columns nil
  "*If non-nil, column numbers in grep hits are screen columns.
See `compilation-error-screen-columns'"
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1"
  :group 'grep)

;;;###autoload
(defcustom grep-setup-hook nil
  "List of hook functions run by `grep-process-setup' (see `run-hooks')."
  :type 'hook
  :group 'grep)

(defvar grep-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up-command)
    (define-key map "\^?" 'scroll-down-command)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)

    ;; Set up the menu-bar
    (define-key map [menu-bar grep]
      (cons "Grep" (make-sparse-keymap "Grep")))

    (define-key map [menu-bar grep compilation-kill-compilation]
      '(menu-item "Kill Grep" kill-compilation
		  :help "Kill the currently running grep process"))
    (define-key map [menu-bar grep compilation-separator2] '("----"))
    (define-key map [menu-bar grep compilation-compile]
      '(menu-item "Compile..." compile
		  :help "Compile the program including the current buffer.  Default: run `make'"))
    (define-key map [menu-bar grep compilation-rgrep]
      '(menu-item "Recursive grep..." rgrep
		  :help "User-friendly recursive grep in directory tree"))
    (define-key map [menu-bar grep compilation-lgrep]
      '(menu-item "Local grep..." lgrep
		  :help "User-friendly grep in a directory"))
    (define-key map [menu-bar grep compilation-grep-find]
      '(menu-item "Grep via Find..." grep-find
		  :help "Run grep via find, with user-specified args"))
    (define-key map [menu-bar grep compilation-grep]
      '(menu-item "Another grep..." grep
		  :help "Run grep, with user-specified args, and collect output in a buffer."))
    (define-key map [menu-bar grep compilation-recompile]
      '(menu-item "Repeat grep" recompile
		  :help "Run grep again"))
    (define-key map [menu-bar grep compilation-separator2] '("----"))
    (define-key map [menu-bar grep compilation-first-error]
      '(menu-item "First Match" first-error
		  :help "Restart at the first match, visit corresponding location"))
    (define-key map [menu-bar grep compilation-previous-error]
      '(menu-item "Previous Match" previous-error
		  :help "Visit the previous match and corresponding location"))
    (define-key map [menu-bar grep compilation-next-error]
      '(menu-item "Next Match" next-error
		  :help "Visit the next match and corresponding location"))
    map)
  "Keymap for grep buffers.
`compilation-minor-mode-map' is a cdr of this.")

(defvar grep-mode-tool-bar-map
  ;; When bootstrapping, tool-bar-map is not properly initialized yet,
  ;; so don't do anything.
  (when (keymapp (butlast tool-bar-map))
    (let ((map (butlast (copy-keymap tool-bar-map)))
	  (help (last tool-bar-map))) ;; Keep Help last in tool bar
      (tool-bar-local-item
       "left-arrow" 'previous-error-no-select 'previous-error-no-select map
       :rtl "right-arrow"
       :help "Goto previous match")
      (tool-bar-local-item
       "right-arrow" 'next-error-no-select 'next-error-no-select map
       :rtl "left-arrow"
       :help "Goto next match")
      (tool-bar-local-item
       "cancel" 'kill-compilation 'kill-compilation map
       :enable '(let ((buffer (compilation-find-buffer)))
		  (get-buffer-process buffer))
       :help "Stop grep")
      (tool-bar-local-item
       "refresh" 'recompile 'recompile map
       :help "Restart grep")
      (append map help))))

(defalias 'kill-grep 'kill-compilation)

;;;; TODO --- refine this!!

;; (defcustom grep-use-compilation-buffer t
;;   "When non-nil, grep specific commands update `compilation-last-buffer'.
;; This means that standard compile commands like \\[next-error] and \\[compile-goto-error]
;; can be used to navigate between grep matches (the default).
;; Otherwise, the grep specific commands like \\[grep-next-match] must
;; be used to navigate between grep matches."
;;   :type 'boolean
;;   :group 'grep)

;; override compilation-last-buffer
(defvar grep-last-buffer nil
  "The most recent grep buffer.
A grep buffer becomes most recent when you select Grep mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`compilation-last-buffer' rather than `grep-last-buffer'.")

;;;###autoload
(defconst grep-regexp-alist
  '(
    ;; Rule to match column numbers is commented out since no known grep
    ;; produces them
    ;; ("^\\(.+?\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)\\2\\(?:\\([1-9][0-9]*\\)\\(?:-\\([1-9][0-9]*\\)\\)?\\2\\)?"
    ;;  1 3 (4 . 5))
    ;; Note that we want to use as tight a regexp as we can to try and
    ;; handle weird file names (with colons in them) as well as possible.
    ;; E.g. we use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:"
    ;; in file names.
    ("^\\(.+?\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)\\2"
     1 3
     ;; Calculate column positions (col . end-col) of first grep match on a line
     ((lambda ()
	(when grep-highlight-matches
	  (let* ((beg (match-end 0))
		 (end (save-excursion (goto-char beg) (line-end-position)))
		 (mbeg (text-property-any beg end 'font-lock-face 'match)))
	    (when mbeg
	      (- mbeg beg)))))
      .
      (lambda ()
	(when grep-highlight-matches
	  (let* ((beg (match-end 0))
		 (end (save-excursion (goto-char beg) (line-end-position)))
		 (mbeg (text-property-any beg end 'font-lock-face 'match))
		 (mend (and mbeg (next-single-property-change mbeg 'font-lock-face nil end))))
	    (when mend
	      (- mend beg)))))))
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

(defvar grep-first-column 0		; bug#10594
  "Value to use for `compilation-first-column' in grep buffers.")

(defvar grep-error "grep hit"
  "Message to print when no matches are found.")

;; Reverse the colors because grep hits are not errors (though we jump there
;; with `next-error'), and unreadable files can't be gone to.
(defvar grep-hit-face	compilation-info-face
  "Face name to use for grep hits.")

(defvar grep-error-face	'compilation-error
  "Face name to use for grep error messages.")

(defvar grep-match-face	'match
  "Face name to use for grep matches.")

(defvar grep-context-face 'shadow
  "Face name to use for grep context lines.")

(defvar grep-mode-font-lock-keywords
   '(;; Command output lines.
     (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
      1 grep-error-face)
     ;; remove match from grep-regexp-alist before fontifying
     ("^Grep[/a-zA-z]* started.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ("^Grep[/a-zA-z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Grep[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 grep-error-face)
      (2 grep-error-face nil t))
     ("^.+?-[0-9]+-.*\n" (0 grep-context-face)))
   "Additional things to highlight in grep output.
This gets tacked on the end of the generated expressions.")

;;;###autoload
(defvar grep-program (purecopy "grep")
  "The default grep program for `grep-command' and `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar find-program (purecopy "find")
  "The default find program for `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar xargs-program (purecopy "xargs")
  "The default xargs program for `grep-find-command'.
See `grep-find-use-xargs'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar grep-find-use-xargs nil
  "How to invoke find and grep.
If `exec', use `find -exec {} ;'.
If `exec-plus' use `find -exec {} +'.
If `gnu', use `find -print0' and `xargs -0'.
Any other value means to use `find -print' and `xargs'.

This variable's value takes effect when `grep-compute-defaults' is called.")

;; History of grep commands.
;;;###autoload
(defvar grep-history nil "History list for grep.")
;;;###autoload
(defvar grep-find-history nil "History list for grep-find.")

;; History of lgrep and rgrep regexp and files args.
(defvar grep-regexp-history nil)
(defvar grep-files-history nil)

;;;###autoload
(defun grep-process-setup ()
  "Setup compilation variables and buffer for `grep'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
  (when (eq grep-highlight-matches 'auto-detect)
    (grep-compute-defaults))
  (unless (or (eq grep-highlight-matches 'auto-detect)
	      (null grep-highlight-matches)
	      ;; Don't output color escapes if they can't be
	      ;; highlighted with `font-lock-face' by `grep-filter'.
	      (null font-lock-mode))
    ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
    ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'
    (setenv "TERM" "emacs-grep")
    (setenv "GREP_OPTIONS"
	    (concat (getenv "GREP_OPTIONS")
		    " --color=" (if (eq grep-highlight-matches 'always)
				    "always" "auto")))
    ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions
    (setenv "GREP_COLOR" "01;31")
    ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions
    (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne"))
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     ;; This relies on the fact that `compilation-start'
	     ;; sets buffer-modified to nil before running the command,
	     ;; so the buffer is still unmodified if there is no output.
	     (cond ((and (zerop code) (buffer-modified-p))
		    '("finished (matches found)\n" . "matched"))
		   ((not (buffer-modified-p))
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code))))
  (run-hooks 'grep-setup-hook))

(defun grep-filter ()
  "Handle match highlighting escape sequences inserted by the grep process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight grep matches and delete marking sequences.
        (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[[0-9]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face grep-match-face)
                         t t))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(defun grep-probe (command args &optional func result)
  (let (process-file-side-effects)
    (equal (condition-case nil
	       (apply (or func 'process-file) command args)
	     (error nil))
	   (or result 0))))

;;;###autoload
(defun grep-compute-defaults ()
  ;; Keep default values.
  (unless grep-host-defaults-alist
    (add-to-list
     'grep-host-defaults-alist
     (cons nil
	   `((grep-command ,grep-command)
	     (grep-template ,grep-template)
	     (grep-use-null-device ,grep-use-null-device)
	     (grep-find-command ,grep-find-command)
	     (grep-find-template ,grep-find-template)
	     (grep-find-use-xargs ,grep-find-use-xargs)
	     (grep-highlight-matches ,grep-highlight-matches)))))
  (let* ((host-id
	  (intern (or (file-remote-p default-directory) "localhost")))
	 (host-defaults (assq host-id grep-host-defaults-alist))
	 (defaults (assq nil grep-host-defaults-alist)))
    ;; There are different defaults on different hosts.  They must be
    ;; computed for every host once.
    (dolist (setting '(grep-command grep-template
		       grep-use-null-device grep-find-command
		       grep-find-template grep-find-use-xargs
		       grep-highlight-matches))
      (set setting
	   (cadr (or (assq setting host-defaults)
		     (assq setting defaults)))))

    (unless (or (not grep-use-null-device) (eq grep-use-null-device t))
      (setq grep-use-null-device
	    (with-temp-buffer
	      (let ((hello-file (expand-file-name "HELLO" data-directory)))
		(not
		 (and (if grep-command
			  ;; `grep-command' is already set, so
			  ;; use that for testing.
			  (grep-probe grep-command
				      `(nil t nil "^English" ,hello-file)
				      #'call-process-shell-command)
			;; otherwise use `grep-program'
			(grep-probe grep-program
				    `(nil t nil "-nH" "^English" ,hello-file)))
		      (progn
			(goto-char (point-min))
			(looking-at
			 (concat (regexp-quote hello-file)
				 ":[0-9]+:English")))))))))
    (unless (and grep-command grep-find-command
		 grep-template grep-find-template)
      (let ((grep-options
	     (concat (if grep-use-null-device "-n" "-nH")
		     (if (grep-probe grep-program
				     `(nil nil nil "-e" "foo" ,null-device)
				     nil 1)
			 " -e"))))
	(unless grep-command
	  (setq grep-command
		(format "%s %s " grep-program grep-options)))
	(unless grep-template
	  (setq grep-template
		(format "%s <X> <C> %s <R> <F>" grep-program grep-options)))
	(unless grep-find-use-xargs
	  (setq grep-find-use-xargs
		(cond
		 ((grep-probe find-program
			      `(nil nil nil ,null-device "-exec" "echo"
				    "{}" "+"))
		  'exec-plus)
		 ((and
		   (grep-probe find-program `(nil nil nil ,null-device "-print0"))
		   (grep-probe xargs-program `(nil nil nil "-0" "-e" "echo")))
		  'gnu)
		 (t
		  'exec))))
	(unless grep-find-command
	  (setq grep-find-command
		(cond ((eq grep-find-use-xargs 'gnu)
		       ;; Windows shells need the program file name
		       ;; after the pipe symbol be quoted if they use
		       ;; forward slashes as directory separators.
		       (format "%s . -type f -print0 | \"%s\" -0 -e %s"
			       find-program xargs-program grep-command))
		      ((memq grep-find-use-xargs '(exec exec-plus))
		       (let ((cmd0 (format "%s . -type f -exec %s"
					   find-program grep-command))
			     (null (if grep-use-null-device
				       (format "%s " null-device)
				     "")))
			 (cons
			  (if (eq grep-find-use-xargs 'exec-plus)
			      (format "%s %s{} +" cmd0 null)
			    (format "%s {} %s%s" cmd0 null
				    (shell-quote-argument ";")))
			  (1+ (length cmd0)))))
		      (t
		       (format "%s . -type f -print | \"%s\" %s"
			       find-program xargs-program grep-command)))))
	(unless grep-find-template
	  (setq grep-find-template
		(let ((gcmd (format "%s <C> %s <R>"
				    grep-program grep-options))
		      (null (if grep-use-null-device
				(format "%s " null-device)
			      "")))
		  (cond ((eq grep-find-use-xargs 'gnu)
			 (format "%s . <X> -type f <F> -print0 | \"%s\" -0 -e %s"
				 find-program xargs-program gcmd))
			((eq grep-find-use-xargs 'exec)
			 (format "%s . <X> -type f <F> -exec %s {} %s%s"
				 find-program gcmd null
				 (shell-quote-argument ";")))
			((eq grep-find-use-xargs 'exec-plus)
			 (format "%s . <X> -type f <F> -exec %s %s{} +"
				 find-program gcmd null))
			(t
			 (format "%s . <X> -type f <F> -print | \"%s\" %s"
				 find-program xargs-program gcmd))))))))
    (when (eq grep-highlight-matches 'auto-detect)
      (setq grep-highlight-matches
	    (with-temp-buffer
	      (and (grep-probe grep-program '(nil t nil "--help"))
		   (progn
		     (goto-char (point-min))
		     (search-forward "--color" nil t))
		   ;; Windows and DOS pipes fail `isatty' detection in Grep.
		   (if (memq system-type '(windows-nt ms-dos))
		       'always 'auto)))))

    ;; Save defaults for this host.
    (setq grep-host-defaults-alist
	  (delete (assq host-id grep-host-defaults-alist)
		  grep-host-defaults-alist))
    (add-to-list
     'grep-host-defaults-alist
     (cons host-id
	   `((grep-command ,grep-command)
	     (grep-template ,grep-template)
	     (grep-use-null-device ,grep-use-null-device)
	     (grep-find-command ,grep-find-command)
	     (grep-find-template ,grep-find-template)
	     (grep-find-use-xargs ,grep-find-use-xargs)
	     (grep-highlight-matches ,grep-highlight-matches))))))

(defun grep-tag-default ()
  (or (and transient-mark-mode mark-active
	   (/= (point) (mark))
	   (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
		   (get major-mode 'find-tag-default-function)
		   'find-tag-default))
      ""))

(defun grep-default-command ()
  "Compute the default grep command for \\[universal-argument] \\[grep] to offer."
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
	(grep-default (or (car grep-history) grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
	       (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*"
		       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
	       grep-default)
	      ;; If the string is not yet complete.
	      (string-match "\\(\\)\\'" grep-default))
      ;; Maybe we will replace the pattern with the default tag.
      ;; But first, maybe replace the file name pattern.
      (condition-case nil
	  (unless (or (not (stringp buffer-file-name))
		      (when (match-beginning 2)
			(save-match-data
			  (string-match
			   (wildcard-to-regexp
			    (file-name-nondirectory
			     (match-string 3 grep-default)))
			   (file-name-nondirectory buffer-file-name)))))
	    (setq grep-default (concat (substring grep-default
						  0 (match-beginning 2))
				       " *."
				       (file-name-extension buffer-file-name))))
	;; In case wildcard-to-regexp gets an error
	;; from invalid data.
	(error nil))
      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t grep-default 1))))


;;;###autoload
(define-compilation-mode grep-mode "Grep"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'tool-bar-map) grep-mode-tool-bar-map)
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       grep-regexp-alist)
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-process-setup-function)
       'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns)
       grep-error-screen-columns)
  (add-hook 'compilation-filter-hook 'grep-filter nil t))


;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the *grep* \
buffer, to go to the lines where grep found
matches.  To kill the grep job before it finishes, type \\[kill-compilation].

For doing a recursive `grep', see the `rgrep' command.  For running
`grep' in a specific directory, see `lgrep'.

This command uses a special history list for its COMMAND-ARGS, so you
can easily repeat a grep command.

A prefix argument says to default the argument based upon the current
tag the cursor is over, substituting it into the last grep command
in the grep command history (or into `grep-command' if that history
list is empty)."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run grep (like this): "
                                 (if current-prefix-arg default grep-command)
                                 'grep-history
                                 (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
			 (concat command-args " " null-device)
		       command-args)
		     'grep-mode))


;;;###autoload
(defun grep-find (command-args)
  "Run grep via find, with user-specified args COMMAND-ARGS.
Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."
  (interactive
   (progn
     (grep-compute-defaults)
     (if grep-find-command
	 (list (read-shell-command "Run find (like this): "
                                   grep-find-command 'grep-find-history))
       ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when command-args
    (let ((null-device nil))		; see grep
      (grep command-args))))

;;;###autoload
(defalias 'find-grep 'grep-find)


;; User-friendly interactive API.

(defconst grep-expand-keywords
  '(("<C>" . (and cf (isearch-no-upper-case-p regexp t) "-i"))
    ("<D>" . dir)
    ("<F>" . files)
    ("<N>" . null-device)
    ("<X>" . excl)
    ("<R>" . (shell-quote-argument (or regexp ""))))
  "List of substitutions performed by `grep-expand-template'.
If car of an element matches, the cdr is evalled in to get the
substitution string.  Note dynamic scoping of variables.")

(defun grep-expand-template (template &optional regexp files dir excl)
  "Patch grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>."
  (let ((command template)
	(cf case-fold-search)
	(case-fold-search nil))
    (dolist (kw grep-expand-keywords command)
      (if (string-match (car kw) command)
	  (setq command
		(replace-match
		 (or (if (symbolp (cdr kw))
			 (symbol-value (cdr kw))
		       (save-match-data (eval (cdr kw))))
		     "")
		 t t command))))))

(defun grep-read-regexp ()
  "Read regexp arg for interactive grep."
  (let ((default (grep-tag-default)))
    (read-string
     (concat "Search for"
	     (if (and default (> (length default) 0))
		 (format " (default \"%s\"): " default) ": "))
     nil 'grep-regexp-history default)))

(defun grep-read-files (regexp)
  "Read files arg for interactive grep."
  (let* ((bn (or (buffer-file-name)
		 (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
	 (fn (and bn
		  (stringp bn)
		  (file-name-nondirectory bn)))
	 (default-alias
	   (and fn
		(let ((aliases (remove (assoc "all" grep-files-aliases)
				       grep-files-aliases))
		      alias)
		  (while aliases
		    (setq alias (car aliases)
			  aliases (cdr aliases))
		    (if (string-match (mapconcat
				       'wildcard-to-regexp
				       (split-string (cdr alias) nil t)
				       "\\|")
				      fn)
			(setq aliases nil)
		      (setq alias nil)))
		  (cdr alias))))
	 (default-extension
	   (and fn
		(let ((ext (file-name-extension fn)))
		  (and ext (concat "*." ext)))))
	 (default
	   (or default-alias
	       default-extension
	       (car grep-files-history)
	       (car (car grep-files-aliases))))
	 (files (completing-read
		 (concat "Search for \"" regexp
			 "\" in files"
			 (if default (concat " (default " default ")"))
			 ": ")
		 'read-file-name-internal
		 nil nil nil 'grep-files-history
		 (delete-dups
		  (delq nil (append (list default default-alias default-extension)
				    (mapcar 'car grep-files-aliases)))))))
    (and files
	 (or (cdr (assoc files grep-files-aliases))
	     files))))

;;;###autoload
(defun lgrep (regexp &optional files dir confirm)
  "Run grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-command
				   nil nil 'grep-history)))
      ((not grep-template)
       (error "grep.el: No `grep-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-directory-p dir) (file-readable-p dir))
      (setq dir default-directory))
    (let ((command regexp))
      (if (null files)
	  (if (string= command grep-command)
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command (grep-expand-template
		       grep-template
		       regexp
		       files
		       nil
		       (and grep-find-ignored-files
			    (concat " --exclude="
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument ignore))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (cdr ignore))))))
				     grep-find-ignored-files
				     " --exclude=")))))
	(when command
	  (if confirm
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start (if (and grep-use-null-device null-device)
				 (concat command " " null-device)
			       command)
			     'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))


(defvar find-name-arg)	    ; not autoloaded but defined in find-dired

;;;###autoload
(defun rgrep (regexp &optional files dir confirm)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in a buffer.  While the recursive grep is running,
you can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to visit the lines where matches were found.  To kill the job
before it finishes, type \\[kill-compilation].

This command shares argument histories with \\[lgrep] and \\[grep-find].

When called programmatically and FILES is nil, REGEXP is expected
to specify a command to run."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "Base directory: "
					  nil default-directory t))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-directory-p dir) (file-readable-p dir))
      (setq dir default-directory))
    (if (null files)
	(if (not (string= regexp (if (consp grep-find-command)
				     (car grep-find-command)
				   grep-find-command)))
	    (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (require 'find-dired)		; for `find-name-arg'
      (let ((command (grep-expand-template
		      grep-find-template
		      regexp
		      (concat (shell-quote-argument "(")
			      " " find-name-arg " "
			      (mapconcat #'shell-quote-argument
					 (split-string files)
					 (concat " -o " find-name-arg " "))
			      " "
			      (shell-quote-argument ")"))
		      dir
		      (concat
		       (and grep-find-ignored-directories
			    (concat "-type d "
				    (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -path "
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument
						 (concat "*/" ignore)))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (concat "*/"
							      (cdr ignore)))))))
				     grep-find-ignored-directories
				     " -o -path ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o "))
		       (and grep-find-ignored-files
			    (concat (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -name "
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument ignore))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (cdr ignore))))))
				     grep-find-ignored-files
				     " -o -name ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o "))))))
	(when command
	  (if confirm
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-find-history))
	    (add-to-history 'grep-find-history command))
	  (let ((default-directory dir))
	    (compilation-start command 'grep-mode))
	  ;; Set default-directory if we started rgrep in the *grep* buffer.
	  (if (eq next-error-last-buffer (current-buffer))
	      (setq default-directory dir)))))))

;;;###autoload
(defun zrgrep (regexp &optional files dir confirm grep-find-template)
  "Recursively grep for REGEXP in gzipped FILES in tree rooted at DIR.
Like `rgrep' but uses `zgrep' for `grep-program', sets the default
file name to `*.gz', and sets `grep-highlight-matches' to `always'."
  (interactive
   (progn
     ;; Compute standard default values.
     (grep-compute-defaults)
     ;; Compute the default zrgrep command by running `grep-compute-defaults'
     ;; for grep program "zgrep", but not changing global values.
     (let ((grep-program "zgrep")
	   ;; Don't change global values for variables computed
	   ;; by `grep-compute-defaults'.
	   (grep-find-template nil)
	   (grep-find-command nil)
	   (grep-host-defaults-alist nil)
	   ;; Use for `grep-read-files'
	   (grep-files-aliases '(("all" . "* .*")
				 ("gz"  . "*.gz"))))
       ;; Recompute defaults using let-bound values above.
       (grep-compute-defaults)
       (cond
	((and grep-find-command (equal current-prefix-arg '(16)))
	 (list (read-from-minibuffer "Run: " grep-find-command
				     nil nil 'grep-find-history)))
	((not grep-find-template)
	 (error "grep.el: No `grep-find-template' available"))
	(t (let* ((regexp (grep-read-regexp))
		  (files (grep-read-files regexp))
		  (dir (read-directory-name "Base directory: "
					    nil default-directory t))
		  (confirm (equal current-prefix-arg '(4))))
	     (list regexp files dir confirm grep-find-template)))))))
  ;; Set `grep-highlight-matches' to `always'
  ;; since `zgrep' puts filters in the grep output.
  (let ((grep-highlight-matches 'always))
    ;; `rgrep' uses the dynamically bound value `grep-find-template'
    ;; from the argument `grep-find-template' whose value is computed
    ;; in the `interactive' spec.
    (rgrep regexp files dir confirm)))

;;;###autoload
(defalias 'rzgrep 'zrgrep)

(provide 'grep)

;;; grep.el ends here
