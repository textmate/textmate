;;; ido.el --- interactively do things with buffers and files

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Based on: iswitchb by Stephen Eglen <stephen@cns.ed.ac.uk>
;; Keywords: extensions convenience

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

;; Ido - interactive do - switches between buffers and opens files and
;; directories with a minimum of keystrokes.  It is a superset of
;; iswitchb, the interactive buffer switching package by Stephen Eglen.

;; Interactive substring matching
;; ------------------------------
;;
;; As you type in a substring, the list of buffers or files currently
;; matching the substring are displayed as you type.  The list is
;; ordered so that the most recent buffers or files visited come at
;; the start of the list.
;;
;; The buffer or file at the start of the list will be the one visited
;; when you press RETURN.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer or file you want will be
;; at the top of the list.  Alternatively, you can use C-s and C-r (or
;; the right and left arrow keys) to rotate buffer or file names in the
;; list until the one you want is at the top of the list.
;;
;; Completion is also available so that you can see what is common to
;; all of the matching buffers or files as you type.
;;
;; Example:
;;
;; If I have two buffers called "123456" and "123", with "123456" the
;; most recent, when I use ido-switch-buffer, I first of all get
;; presented with the list of all the buffers
;;
;;       Buffer: {123456 | 123}
;;
;; If I then press 2:
;;       Buffer: 2[3]{123456 | 123}
;;
;; The list in {...} are the matching buffers, most recent first
;; (buffers visible in the current frame are put at the end of the
;; list by default).  At any time I can select the item at the head of
;; the list by pressing RET.  I can also put the first element at the
;; end of the list by pressing C-s or [right], or bring the last
;; element to the head of the list by pressing C-r or [left].
;;
;; The item in [...] indicates what can be added to my input by
;; pressing TAB.  In this case, I will get "3" added to my input.

;; So, I press TAB:
;;	 Buffer: 23{123456 | 123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;; However, if I type 4, I only have one match left:
;;       Buffer: 234[123456]
;;
;; Since there is only one matching buffer left, it is given in [] and
;; it is shown in the `ido-only-match' face (ForestGreen).  I can now
;; press TAB or RET to go to that buffer.
;;
;; If I want to create a new buffer named "234", I press C-j instead of
;; TAB or RET.
;;
;; If instead, I type "a":
;;       Buffer: 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" file would be
;; just to type 4 and then RET (assuming there isn't any newer buffer
;; with 4 in its name).

;; Likewise, if you use C-x C-f (ido-find-file), the list of files and
;; directories in the current directory is provided in the same
;; fashion as the buffers above.  The files and directories are
;; normally sorted in alphabetical order, but the most recently
;; visited directory is placed first to speed up navigating to
;; directories that you have visited recently.
;;
;; In addition to scrolling through the list using [right] and [left],
;; you can use [up] and [down] to quickly scroll the list to the next
;; or previous subdirectory.
;;
;; To go down into a subdirectory, and continue the file selection on
;; the files in that directory, simply move the directory to the head
;; of the list and hit RET.
;;
;; To go up to the parent directory, delete any partial file name
;; already specified (e.g. using [backspace]) and hit [backspace].
;;
;; To go to the root directory (on the current drive), enter two
;; slashes.  On MS-DOS or Windows, to select the root of another
;; drive, enter X:/ where X is the drive letter.  You can also visit
;; files on other hosts using the ange-ftp notations `/host:' and
;; `/user@host:'.  See the variable `ido-slow-ftp-hosts' if you want
;; to inhibit the ido substring matching for ftp access.
;;
;; If for some reason you cannot specify the proper file using
;; ido-find-file, you can press C-f to enter the normal find-file.
;; You can also press C-b to drop into ido-switch-buffer.

;; See the doc string of ido-switch-buffer and ido-find-file for full
;; keybindings and features.
;;  (describe-function 'ido-find-file)

;; Hidden buffers and files
;; ------------------------
;;
;; Normally, ido does not include hidden buffers (whose name starts
;; with a space) and hidden files and directories (whose name starts
;; with `.') in the list of possible completions.  However, if the
;; substring you enter does not match any of the visible buffers or
;; files, ido will automatically look for completions among the hidden
;; buffers or files.
;;
;; You can toggle display of the hidden buffers and files with C-a.

;; Additional functionality
;; ------------------------
;;
;; After C-x b, the buffer at the head of the list can be killed by
;; pressing C-k.  If the buffer needs saving, you will be queried
;; before the buffer is killed.
;;
;; Likewise, after C-x C-f, you can delete (i.e. physically remove)
;; the file at the head of the list with C-k.  You will always be
;; asked for confirmation before the file is deleted.
;;
;; If you enter C-x b to switch to a buffer visiting a given file, and
;; you find that the file you are after is not in any buffer, you can
;; press C-f to immediately drop into ido-find-file.  And you can
;; switch back to buffer selection with C-b.

;; Prefix matching
;; ---------------
;;
;; The standard way of completion with Unix-shells and Emacs is to insert a
;; PREFIX and then hitting TAB (or another completion key). Cause of this
;; behavior has become second nature to a lot of emacs users `ido' offers in
;; addition to the default substring-matching-method (look above) also the
;; prefix-matching-method. The kind of matching is the only difference to
;; the description of the substring-matching above.
;;
;; You can toggle prefix matching with C-p.
;;
;; Example:
;;
;; If you have again two Buffers "123456" and "123" then hitting "2" does
;; not match because "2" is not a PREFIX in any of the buffer-names.

;; Flexible matching
;; -----------------
;;
;; If you set ido-enable-flex-matching, ido will do a more flexible
;; matching (unless regexp matching is active) to find possible matches
;; among the available buffer or file names if no matches are found using
;; the normal prefix or substring matching.
;;
;; The flexible matching implies that any item which simply contains all
;; of the entered characters in the specified sequence will match.
;;
;; Example:
;;
;; If you have four files "alpha", "beta", "gamma", and "delta",
;; entering "aa" will match "alpha" and "gamma", while "ea" matches
;; "beta" and "delta".  If prefix matching is also active, "aa" only
;; matches "alpha", while "ea" does not match any files.

;; Regexp matching
;; ---------------
;;
;; There is limited provision for regexp matching within ido,
;; enabled through `ido-enable-regexp' (toggle with C-t).
;; This allows you to type `[ch]$' for example and see all file names
;; ending in `c' or `h'.
;;
;; Note: ido-style completion is inhibited when you enable regexp matching.


;; Customization
;; -------------
;;
;; Customize the `ido' group to change the `ido' functionality.
;;
;; To modify the keybindings, use the ido-setup-hook.  For example:
;;(add-hook 'ido-setup-hook 'ido-my-keys)
;;
;;(defun ido-my-keys ()
;;  "Add my keybindings for ido."
;;  (define-key ido-completion-map " " 'ido-next-match)
;;  )

;; Seeing all the matching buffers or files
;; ----------------------------------------
;;
;; If you have many matching files, they may not all fit onto one
;; line of the minibuffer.  Normally, the minibuffer window will grow
;; to show you more of the matching files (depending on the setting
;; of the variables `resize-mini-windows' and `max-mini-window-height').
;; If you want ido to behave differently from the default minibuffer
;; resizing behavior, set the variable `ido-max-window-height'.
;;
;; Also, to improve the responsiveness of ido, the maximum number of
;; matching items is limited to 12, but you can increase or removed
;; this limit via the `ido-max-prospects' variable.

;; To see a full list of all matching buffers in a separate buffer,
;; hit ? or press TAB when there are no further completions to the
;; substring.  Repeated TAB presses will scroll you through this
;; separate buffer.

;; Changing the list of files
;; --------------------------

;; By default, the list of current files is most recent first,
;; oldest last, with the exception that the files visible in the
;; current frame are put at the end of the list.  A hook exists to
;; allow other functions to order the list.  For example, if you add:
;;
;; (add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)
;;
;; then all files matching "Summary" are moved to the end of the
;; list.  (I find this handy for keeping the INBOX Summary and so on
;; out of the way.)  It also moves files matching "output\*$" to the
;; end of the list (these are created by AUCTeX when compiling.)
;; Other functions could be made available which alter the list of
;; matching files (either deleting or rearranging elements.)

;; Highlighting
;; ------------

;; The highlighting of matching items is controlled via ido-use-faces.
;; The faces used are ido-first-match, ido-only-match and
;; ido-subdir.
;; Coloring of the matching item was suggested by
;; Carsten Dominik (dominik@strw.leidenuniv.nl).

;; Replacement for read-buffer and read-file-name
;; ----------------------------------------------

;; ido-read-buffer and ido-read-file-name have been written to be drop
;; in replacements for the normal buffer and file name reading
;; functions `read-buffer' and `read-file-name'.

;; To use ido for all buffer and file selections in Emacs, customize the
;; variable `ido-everywhere'.

;; Using ido-like behavior in other lisp packages
;; -----------------------------------------------

;; If you don't want to rely on the `ido-everywhere' functionality,
;; ido-read-buffer, ido-read-file-name, and ido-read-directory-name
;; can be used by other packages to read a buffer name, a file name,
;; or a directory name in the `ido' way.

;;; Acknowledgements

;; Infinite amounts of gratitude goes to Stephen Eglen <stephen@cns.ed.ac.uk>
;; who wrote iswitch-buffer mode - from which I ripped off 99% of the code
;; for ido-switch-buffer and found the inspiration for ido-find-file.
;; The ido package would never have existed without his work.

;; Also thanks to Klaus Berndl, Rohit Namjoshi, Robert Fenk, Alex
;; Schroeder, Bill Benedetto, Stephen Eglen, and many others for bug
;; fixes and improvements.

;;; History

;; Since I discovered Stephen Eglen's excellent iswitchb package, I just
;; couldn't live without it, but once being addicted to switching buffers
;; with a minimum of keystrokes, I soon found that opening files in the
;; old-fashioned way was just too slow - so I decided to write a package
;; which could open files with the same speed and ease as iswitchb could
;; switch buffers.

;; I originally wrote a separate ifindf.el package based on a copy of
;; iswitchb.el, which did for opening files what iswitchb did for
;; switching buffers.  Along the way, I corrected a few errors in
;; ifindf which could have found its way back into iswitchb, but since
;; most of the functionality of the two package was practically
;; identical, I decided that the proper thing to do was to merge my
;; ifindf package back into iswitchb.
;;
;; This is basically what ido (interactively do) is all about; but I
;; found it awkward to merge my changes into the "iswitchb-" namespace,
;; so I invented a common "ido-" namespace for the merged packages.
;;
;; This version is based on ido.el version 1.57 released on
;; gnu.emacs.sources adapted for emacs 22.1 to use command remapping
;; and optionally hooking the read-buffer and read-file-name functions.
;;
;; Prefix matching was added by Klaus Berndl <klaus.berndl@sdm.de> based on
;; an idea of Yuji Minejima <ggb01164@nifty.ne.jp> and his mcomplete-package.


;;; Code:

(defvar recentf-list)

;;; User Variables
;;
;; These are some things you might want to change.

(defun ido-fractionp (n)
  (and (numberp n) (> n 0.0) (<= n 1.0)))

(defgroup ido nil
  "Switch between files using substrings."
  :group 'extensions
  :group 'convenience
  :version "22.1"
  :link '(emacs-commentary-link :tag "Commentary" "ido.el")
  :link '(emacs-library-link :tag "Lisp File" "ido.el"))

;;;###autoload
(defcustom ido-mode nil
  "Determines for which functional group \(buffer and files) ido behavior
should be enabled.  The following values are possible:
- `buffer': Turn only on ido buffer behavior \(switching, killing,
  displaying...)
- `file': Turn only on ido file behavior \(finding, writing, inserting...)
- `both': Turn on ido buffer and file behavior.
- `nil': Turn off any ido switching.

Setting this variable directly does not take effect;
use either \\[customize] or the function `ido-mode'."
  :set #'(lambda (_symbol value)
	   (ido-mode value))
  :initialize 'custom-initialize-default
  :require 'ido
  :link '(emacs-commentary-link "ido.el")
  :set-after '(ido-save-directory-list-file
	       ;; This will clear ido-unc-hosts-cache, so set it
	       ;; before loading history file.
	       ido-unc-hosts)
  :type '(choice (const :tag "Turn on only buffer" buffer)
                 (const :tag "Turn on only file" file)
                 (const :tag "Turn on both buffer and file" both)
                 (const :tag "Switch off all" nil))
  :group 'ido)

(defcustom ido-case-fold case-fold-search
  "Non-nil if searching of buffer and file names should ignore case."
  :type 'boolean
  :group 'ido)

(defcustom ido-ignore-buffers
  '("\\` ")
  "List of regexps or functions matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is `\\` '.  See the source file for
example functions that filter buffer names."
  :type '(repeat (choice regexp function))
  :group 'ido)

(defcustom ido-ignore-files
  '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")
  "List of regexps or functions matching file names to ignore.
For example, traditional behavior is not to list files whose names begin
with a #, for which the regexp is `\\`#'.  See the source file for
example functions that filter filenames."
  :type '(repeat (choice regexp function))
  :group 'ido)

(defcustom ido-ignore-extensions t
  "Non-nil means ignore files in `completion-ignored-extensions' list."
  :type 'boolean
  :group 'ido)

(defcustom ido-show-dot-for-dired nil
  "Non-nil means to always put . as the first item in file name lists.
This allows the current directory to be opened immediately with `dired'."
  :type 'boolean
  :group 'ido)

(defcustom ido-file-extensions-order nil
  "List of file extensions specifying preferred order of file selections.
Each element is either a string with `.' as the first char, an empty
string matching files without extension, or t which is the default order
for files with an unlisted file extension."
  :type '(repeat (choice string
			 (const :tag "Default order" t)))
  :group 'ido)

(defcustom ido-ignore-directories
  '("\\`CVS/" "\\`\\.\\./" "\\`\\./")
  "List of regexps or functions matching sub-directory names to ignore."
  :type '(repeat (choice regexp function))
  :group 'ido)

(defcustom ido-ignore-directories-merge nil
  "List of regexps or functions matching directory names to ignore during merge.
Directory names matched by one of the regexps in this list are not inserted
in merged file and directory lists."
  :type '(repeat (choice regexp function))
  :group 'ido)

;; Examples for setting the value of ido-ignore-buffers
;;(defun ido-ignore-c-mode (name)
;;  "Ignore all c mode buffers -- example function for ido."
;;  (with-current-buffer name
;;    (derived-mode-p 'c-mode)))
;;
;;(setq ido-ignore-buffers '("^ " ido-ignore-c-mode))

;; Examples for setting the value of ido-ignore-files
;;(setq ido-ignore-files '("^ " "\\.c\\'" "\\.h\\'"))

(defcustom ido-default-file-method  'raise-frame
  "How to visit a new file when using `ido-find-file'.
Possible values:
`selected-window' Show new file in selected window
`other-window'	  Show new file in another window (same frame)
`display'	  Display file in another window without selecting to it
`other-frame'	  Show new file in another frame
`maybe-frame'	  If a file is visible in another frame, prompt to ask if you
		  you want to see the file in the same window of the current
  		  frame or in the other frame
`raise-frame'     If a file is visible in another frame, raise that
		  frame; otherwise, visit the file in the same window"
    :type '(choice (const :tag "Visit in selected window" selected-window)
		   (const :tag "Visit in other window" other-window)
		   (const :tag "Display (no select) in other window" display)
		   (const :tag "Visit in other frame" other-frame)
		   (const :tag "Ask to visit in other frame" maybe-frame)
		   (const :tag "Raise frame if already visited" raise-frame))
    :group 'ido)

(defcustom ido-default-buffer-method  'raise-frame
  "How to switch to new buffer when using `ido-switch-buffer'.
See `ido-default-file-method' for details."
    :type '(choice (const :tag "Show in selected window" selected-window)
		   (const :tag "Show in other window" other-window)
		   (const :tag "Display (no select) in other window" display)
		   (const :tag "Show in other frame" other-frame)
		   (const :tag "Ask to show in other frame" maybe-frame)
		   (const :tag "Raise frame if already shown" raise-frame))
    :group 'ido)

(defcustom ido-enable-flex-matching nil
  "Non-nil means that `ido' will do flexible string matching.
Flexible matching means that if the entered string does not
match any item, any item containing the entered characters
in the given sequence will match."
  :type 'boolean
  :group 'ido)


(defcustom ido-enable-regexp nil
  "Non-nil means that `ido' will do regexp matching.
Value can be toggled within `ido' using `ido-toggle-regexp'."
  :type 'boolean
  :group 'ido)

(defcustom ido-enable-prefix nil
  "Non-nil means only match if the entered text is a prefix of file name.
This behavior is like the standard Emacs completion.
If nil, match if the entered text is an arbitrary substring.
Value can be toggled within `ido' using `ido-toggle-prefix'."
  :type 'boolean
  :group 'ido)

(defcustom ido-enable-dot-prefix nil
  "Non-nil means to match leading dot as prefix.
I.e. hidden files and buffers will match only if you type a dot
as first char even if `ido-enable-prefix' is nil."
  :type 'boolean
  :group 'ido)

(defcustom ido-confirm-unique-completion nil
  "Non-nil means that even a unique completion must be confirmed.
This means that \\[ido-complete] must always be followed by \\[ido-exit-minibuffer]
even when there is only one unique completion."
  :type 'boolean
  :group 'ido)

(defcustom ido-cannot-complete-command 'ido-completion-help
  "Command run when `ido-complete' can't complete any more.
The most useful values are `ido-completion-help', which pops up a
window with completion alternatives, or `ido-next-match' or
`ido-prev-match', which cycle the buffer list."
  :type 'function
  :group 'ido)


(defcustom ido-record-commands t
  "Non-nil means that `ido' will record commands in command history.
Note that the non-ido equivalent command is recorded."
  :type 'boolean
  :group 'ido)

(defcustom ido-max-prospects 12
  "Non-zero means that the prospect list will be limited to that number of items.
For a long list of prospects, building the full list for the minibuffer can take a
non-negligible amount of time; setting this variable reduces that time."
  :type 'integer
  :group 'ido)

(defcustom ido-max-file-prompt-width 0.35
  "Non-zero means that the prompt string be limited to that number of characters.
If value is a floating point number, it specifies a fraction of the frame width."
  :type '(choice
	  (integer :tag "Characters" :value 20)
	  (restricted-sexp :tag "Fraction of frame width"
			   :value 0.35
			   :match-alternatives (ido-fractionp)))
  :group 'ido)

(defcustom ido-max-window-height nil
  "Non-nil specifies a value to override `max-mini-window-height'."
  :type '(choice
	  (const :tag "Don't override" nil)
	  (integer :tag "Number of lines" :value 1)
	  (restricted-sexp
	   :tag "Fraction of window height"
	   :value 0.25
	   :match-alternatives (ido-fractionp)))
  :group 'ido)

(defcustom ido-enable-last-directory-history t
  "Non-nil means that `ido' will remember latest selected directory names.
See `ido-last-directory-list' and `ido-save-directory-list-file'."
  :type 'boolean
  :group 'ido)

(defcustom ido-max-work-directory-list 50
  "Maximum number of working directories to record.
This is the list of directories where files have most recently been opened.
See `ido-work-directory-list' and `ido-save-directory-list-file'."
  :type 'integer
  :group 'ido)

(defcustom ido-work-directory-list-ignore-regexps nil
  "List of regexps matching directories which should not be recorded.
Directory names matched by one of the regexps in this list are not inserted in
the `ido-work-directory-list' list."
  :type '(repeat regexp)
  :group 'ido)


(defcustom ido-use-filename-at-point nil
  "Non-nil means that ido shall look for a filename at point.
May use `ffap-guesser' to guess whether text at point is a filename.
If found, use that as the starting point for filename selection."
  :type '(choice
	  (const :tag "Disabled" nil)
	  (const :tag "Guess filename" guess)
	  (other :tag "Use literal filename" t))
  :group 'ido)


(defcustom ido-use-url-at-point nil
  "Non-nil means that ido shall look for a URL at point.
If found, call `find-file-at-point' to visit it."
  :type 'boolean
  :group 'ido)


(defcustom ido-enable-tramp-completion t
  "Non-nil means that ido shall perform tramp method and server name completion.
A tramp file name uses the following syntax: /method:user@host:filename."
  :type 'boolean
  :group 'ido)

(defcustom ido-record-ftp-work-directories t
  "Non-nil means record ftp file names in the work directory list."
  :type 'boolean
  :group 'ido)

(defcustom ido-merge-ftp-work-directories nil
  "If nil, merging ignores ftp file names in the work directory list."
  :type 'boolean
  :group 'ido)

(defcustom ido-cache-ftp-work-directory-time 1.0
  "Maximum time to cache contents of an ftp directory (in hours).
Use C-l in prompt to refresh list.
If zero, ftp directories are not cached."
  :type 'number
  :group 'ido)

(defcustom ido-slow-ftp-hosts nil
  "List of slow ftp hosts where ido prompting should not be used.
If an ftp host is on this list, ido automatically switches to the non-ido
equivalent function, e.g. `find-file' rather than `ido-find-file'."
  :type '(repeat string)
  :group 'ido)

(defcustom ido-slow-ftp-host-regexps nil
  "List of regexps matching slow ftp hosts (see `ido-slow-ftp-hosts')."
  :type '(repeat regexp)
  :group 'ido)

(defvar ido-unc-hosts-cache t
  "Cached value from `ido-unc-hosts' function.")

(defcustom ido-unc-hosts nil
  "List of known UNC host names to complete after initial //.
If value is a function, that function is called to search network for
hosts on first use of UNC path."
  :type '(choice (repeat :tag "List of UNC host names" string)
		 (function-item :tag "Use `NET VIEW'"
				:value ido-unc-hosts-net-view)
		 (function :tag "Your own function"))
  :set #'(lambda (symbol value)
	   (set symbol value)
	   (setq ido-unc-hosts-cache t))
  :group 'ido)

(defcustom ido-downcase-unc-hosts t
  "Non-nil if UNC host names should be downcased."
  :type 'boolean
  :group 'ido)

(defcustom ido-ignore-unc-host-regexps nil
  "List of regexps matching UNC hosts to ignore.
Case is ignored if `ido-downcase-unc-hosts' is set."
  :type '(repeat regexp)
  :group 'ido)

(defcustom ido-cache-unc-host-shares-time 8.0
  "Maximum time to cache shares of an UNC host (in hours).
Use C-l in prompt to refresh list.
If zero, UNC host shares are not cached."
  :type 'number
  :group 'ido)

(defcustom ido-max-work-file-list 10
  "Maximum number of names of recently opened files to record.
This is the list of the file names (sans directory) which have most recently
been opened.  See `ido-work-file-list' and `ido-save-directory-list-file'."
  :type 'integer
  :group 'ido)

(defcustom ido-work-directory-match-only t
  "Non-nil means to skip non-matching directories in the directory history.
When some text is already entered at the `ido-find-file' prompt, using
\\[ido-prev-work-directory] or \\[ido-next-work-directory] will skip directories
without any matching entries."
  :type 'boolean
  :group 'ido)

(defcustom ido-auto-merge-work-directories-length 0
  "Automatically switch to merged work directories during file name input.
The value is number of characters to type before switching to merged mode.
If zero, the switch happens when no matches are found in the current directory.
Automatic merging is disabled if the value is negative."
  :type 'integer
  :group 'ido)

(defcustom ido-auto-merge-delay-time 0.70
  "Delay in seconds to wait for more input before doing auto merge."
  :type 'number
  :group 'ido)

(defcustom ido-auto-merge-inhibit-characters-regexp "[][*?~]"
  "Regexp matching characters which should inhibit automatic merging.
When a (partial) file name matches this regexp, merging is inhibited."
  :type 'regexp
  :group 'ido)

(defcustom ido-merged-indicator "^"
  "The string appended to first choice if it has multiple directory choices."
  :type 'string
  :group 'ido)

(defcustom ido-max-dir-file-cache 100
  "Maximum number of working directories to be cached.
This is the size of the cache of `file-name-all-completions' results.
Each cache entry is time stamped with the modification time of the
directory.  Some systems, like Windows, have unreliable directory
modification times, so you may choose to disable caching on such
systems, or explicitly refresh the cache contents using the command
`ido-reread-directory' command (C-l) in the minibuffer.
See also `ido-dir-file-cache' and `ido-save-directory-list-file'."
  :type 'integer
  :group 'ido)

(defcustom ido-max-directory-size 30000
  "Maximum size (in bytes) for directories to use ido completion.
If you enter a directory with a size larger than this size, ido will
not provide the normal completion.  To show the completions, use C-a."
  :type '(choice (const :tag "No limit" nil)
		 (integer :tag "Size in bytes" 30000))
  :group 'ido)

(defcustom ido-rotate-file-list-default nil
  "Non-nil means that `ido' will always rotate file list to get default in front."
  :type 'boolean
  :group 'ido)

(defcustom ido-enter-matching-directory 'only
  "Additional methods to enter sub-directory of first/only matching item.
If value is 'first, enter first matching sub-directory when typing a slash.
If value is 'only, typing a slash only enters the sub-directory if it is
the only matching item.
If value is t, automatically enter a sub-directory when it is the only
matching item, even without typing a slash."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Slash enters first directory" first)
		 (const :tag "Slash enters first and only directory" only)
		 (other :tag "Always enter unique directory" t))
  :group 'ido)

(defcustom ido-create-new-buffer 'prompt
  "Specify whether a new buffer is created if no buffer matches substring.
Choices are 'always to create new buffers unconditionally, 'prompt to
ask user whether to create buffer, or 'never to never create new buffer."
  :type '(choice (const always)
		 (const prompt)
		 (const never))
  :group 'ido)

(defcustom ido-setup-hook  nil
  "Hook run after the ido variables and keymap have been setup.
The dynamic variable `ido-cur-item' contains the current type of item that
is read by ido; possible values are file, dir, buffer, and list.
Additional keys can be defined in `ido-completion-map'."
  :type 'hook
  :group 'ido)

(defcustom ido-separator nil
  "String used by ido to separate the alternatives in the minibuffer.
Obsolete.  Set 3rd element of `ido-decorations' instead."
  :type '(choice string (const nil))
  :group 'ido)

(defcustom ido-decorations '( "{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")
  "List of strings used by ido to display the alternatives in the minibuffer.
There are 11 elements in this list:
1st and 2nd elements are used as brackets around the prospect list,
3rd element is the separator between prospects (ignored if `ido-separator' is set),
4th element is the string inserted at the end of a truncated list of prospects,
5th and 6th elements are used as brackets around the common match string which
can be completed using TAB,
7th element is the string displayed when there are no matches, and
8th element is displayed if there is a single match (and faces are not used),
9th element is displayed when the current directory is non-readable,
10th element is displayed when directory exceeds `ido-max-directory-size',
11th element is displayed to confirm creating new file or buffer."
  :type '(repeat string)
  :group 'ido)

(defcustom ido-use-virtual-buffers nil
  "If non-nil, refer to past buffers as well as existing ones.
Essentially it works as follows: Say you are visiting a file and
the buffer gets cleaned up by midnight.el.  Later, you want to
switch to that buffer, but find it's no longer open.  With
virtual buffers enabled, the buffer name stays in the buffer
list (using the `ido-virtual' face, and always at the end), and if
you select it, it opens the file back up again.  This allows you
to think less about whether recently opened files are still open
or not.  Most of the time you can quit Emacs, restart, and then
switch to a file buffer that was previously open as if it still
were.
    This feature relies upon the `recentf' package, which will be
enabled if this variable is configured to a non-nil value."
  :version "24.1"
  :type 'boolean
  :group 'ido)

(defcustom ido-use-faces t
  "Non-nil means use ido faces to highlighting first match, only match and
subdirs in the alternatives."
  :type 'boolean
  :group 'ido)

(defface ido-first-match  '((t (:bold t)))
  "Face used by ido for highlighting first match."
  :group 'ido)

(defface ido-only-match  '((((class color))
                                 (:foreground "ForestGreen"))
                                (t (:italic t)))
  "Face used by ido for highlighting only match."
  :group 'ido)

(defface ido-subdir  '((((min-colors 88) (class color))
                             (:foreground "red1"))
			    (((class color))
                             (:foreground "red"))
                            (t (:underline t)))
  "Face used by ido for highlighting subdirs in the alternatives."
  :group 'ido)

(defface ido-virtual '((t (:inherit font-lock-builtin-face)))
  "Face used by ido for matching virtual buffer names."
  :version "24.1"
  :group 'ido)

(defface ido-indicator  '((((min-colors 88) (class color))
				(:foreground "yellow1"
				 :background "red1"
				 :width condensed))
			       (((class color))
				(:foreground "yellow"
				 :background "red"
				 :width condensed))
			       (t (:inverse-video t)))
  "Face used by ido for highlighting its indicators."
  :group 'ido)

(defface ido-incomplete-regexp
  '((t
     (:inherit font-lock-warning-face)))
  "Ido face for indicating incomplete regexps."
  :group 'ido)

(defcustom ido-make-file-list-hook  nil
  "List of functions to run when the list of matching files is created.
Each function on the list may modify the dynamically bound variable
`ido-temp-list' which contains the current list of matching files."
  :type 'hook
  :group 'ido)

(defcustom ido-make-dir-list-hook  nil
  "List of functions to run when the list of matching directories is created.
Each function on the list may modify the dynamically bound variable
`ido-temp-list' which contains the current list of matching directories."
  :type 'hook
  :group 'ido)

(defcustom ido-make-buffer-list-hook  nil
  "List of functions to run when the list of matching buffers is created.
Each function on the list may modify the dynamically bound variable
`ido-temp-list' which contains the current list of matching buffer names."
  :type 'hook
  :group 'ido)

(defcustom ido-rewrite-file-prompt-functions nil
  "List of functions to run when the find-file prompt is created.
Each function on the list may modify the following dynamically bound
variables:
  dirname   - the (abbreviated) directory name
		 to be modified by the hook functions
  max-width - the max width of the resulting dirname; nil means no limit
  prompt    - the basic prompt (e.g. \"Find File: \")
  literal   - the string shown if doing \"literal\" find; set to nil to omit
  vc-off    - the string shown if version control is inhibited; set to nil to omit
  prefix    - either nil or a fixed prefix for the dirname

The following variables are available, but should not be changed:
  `ido-current-directory' - the unabbreviated directory name
  item - equals `file' or `dir' depending on the current mode."
  :type 'hook
  :group 'ido)

(defvar ido-rewrite-file-prompt-rules nil
  "*Alist of rewriting rules for directory names in ido prompts.
A list of elements of the form (FROM . TO) or (FROM . FUNC), each
meaning to rewrite the directory name if matched by FROM by either
substituting the matched string by TO or calling the function FUNC
with the current directory name as its only argument and using the
return value as the new directory name.  In addition, each FUNC may
also modify the dynamic variables described for the variable
`ido-rewrite-file-prompt-functions'.")

(defcustom ido-completion-buffer "*Ido Completions*"
  "Name of completion buffer used by ido.
Set to nil to disable completion buffers popping up."
  :type 'string
  :group 'ido)

(defcustom ido-completion-buffer-all-completions nil
  "Non-nil means to show all completions in completion buffer.
Otherwise, only the current list of matches is shown."
  :type 'boolean
  :group 'ido)

(defvar ido-all-frames 'visible
  "*Argument to pass to `walk-windows' when finding visible files.
See documentation of `walk-windows' for useful values.")

(defcustom ido-minibuffer-setup-hook nil
  "Ido-specific customization of minibuffer setup.

This hook is run during minibuffer setup if `ido' is active.
It is intended for use in customizing ido for interoperation
with other packages.  For instance:

  \(add-hook 'ido-minibuffer-setup-hook
	    \(function
	     \(lambda ()
	       \(make-local-variable 'max-mini-window-height)
	       \(setq max-mini-window-height 3))))

will constrain Emacs to a maximum minibuffer height of 3 lines when
ido is running.  Copied from `icomplete-minibuffer-setup-hook'."
  :type 'hook
  :group 'ido)

(defcustom ido-save-directory-list-file (convert-standard-filename "~/.ido.last")
  "File in which the ido state is saved between invocations.
Variables stored are: `ido-last-directory-list', `ido-work-directory-list',
`ido-work-file-list', and `ido-dir-file-cache'.
Must be set before enabling ido mode."
  :type 'string
  :group 'ido)

(defcustom ido-read-file-name-as-directory-commands '()
  "List of commands which uses `read-file-name' to read a directory name.
When `ido-everywhere' is non-nil, the commands in this list will read
the directory using `ido-read-directory-name'."
  :type '(repeat symbol)
  :group 'ido)

(defcustom ido-read-file-name-non-ido '()
  "List of commands which shall not read file names the ido way.
When `ido-everywhere' is non-nil, the commands in this list will read
the file name using normal `read-file-name' style."
  :type '(repeat symbol)
  :group 'ido)

(defcustom ido-before-fallback-functions '()
  "List of functions to call before calling a fallback command.
The fallback command is passed as an argument to the functions."
  :type 'hook
  :group 'ido)

;;; Internal Variables

;; Persistent variables

(defvar ido-completion-map nil
  "Currently active keymap for ido commands.")

(defvar ido-common-completion-map nil
  "Keymap for all ido commands.")

(defvar ido-file-completion-map nil
  "Keymap for ido file commands.")

(defvar ido-file-dir-completion-map nil
  "Keymap for ido file and directory commands.")

(defvar ido-buffer-completion-map nil
  "Keymap for ido buffer commands.")

(defvar  ido-file-history nil
  "History of files selected using `ido-find-file'.")

(defvar  ido-buffer-history nil
  "History of buffers selected using `ido-switch-buffer'.")

(defvar ido-last-directory-list nil
  "List of last selected directory names.
See `ido-enable-last-directory-history' for details.")

(defvar ido-work-directory-list nil
  "List of actual working directory names.
The current directory is inserted at the front of this list whenever a
file is opened with `ido-find-file' and family.")

(defvar ido-work-file-list nil
  "List of actual work file names.
Opening a file with `ido-find-file' and similar functions
inserts the current file name (relative to its containing directory)
at the front of this list.")

(defvar ido-dir-file-cache nil
  "List of `file-name-all-completions' results.
Each element in the list is of the form (DIR (MTIME) FILE...).")

(defvar ido-ignore-item-temp-list nil
  "List of items to ignore in current ido invocation.
Intended to be let-bound by functions which call ido repeatedly.
Should never be set permanently.")

;; Temporary storage

(defvar ido-eoinput 1
  "Point where minibuffer input ends and completion info begins.
Copied from `icomplete-eoinput'.")
(make-variable-buffer-local 'ido-eoinput)

(defvar ido-common-match-string  nil
  "Stores the string that is common to all matching files.")

(defvar ido-rescan nil
  "Non-nil means we need to regenerate the list of matching items.")

(defvar ido-rotate nil
  "Non-nil means we are rotating list of matches.")

(defvar ido-text nil
  "Stores the users string as it is typed in.")

(defvar ido-text-init nil
  "The initial string for the users string it is typed in.")

(defvar ido-input-stack nil
  "Stores the users strings when user hits M-b/M-f.")

(defvar ido-matches nil
  "List of files currently matching `ido-text'.")

(defvar ido-report-no-match t
  "Report [No Match] when no completions matches `ido-text'.")

(defvar ido-exit nil
  "Flag to monitor how `ido-find-file' exits.
If equal to `takeprompt', we use the prompt as the file name to be
selected.")

(defvar ido-current-directory nil
  "Current directory for `ido-find-file'.")

(defvar ido-auto-merge-timer nil
  "Delay timer for auto merge.")

(defvar ido-use-mycompletion-depth 0
  "Non-nil means use `ido' completion feedback.
Is set by ido functions to the current `minibuffer-depth',
so that it doesn't interfere with other minibuffer usage.")

(defvar ido-incomplete-regexp nil
  "Non-nil if an incomplete regexp is entered.")

(defvar ido-initial-position nil
  "Non-nil means to explicitly cursor on entry to minibuffer.
Value is an integer which is number of chars to right of prompt.")

(defvar ido-virtual-buffers nil
  "List of virtual buffers, that is, past visited files.
This is a copy of `recentf-list', pared down and with faces applied.
Only used if `ido-use-virtual-buffers' is non-nil.")

;;; Variables with dynamic bindings.
;;; Declared here to keep the byte compiler quiet.

;; Stores the current ido item type ('file, 'dir, 'buffer, or 'list).
(defvar ido-cur-item)

;;; Stores the current default item
(defvar ido-default-item)

;; Stores the current list of items that will be searched through.
;; The list is ordered, so that the most interesting item comes first,
;; although by default, the files visible in the current frame are put
;; at the end of the list.  Created by `ido-make-item-list'.
(defvar ido-cur-list)

;; Stores the choice list for ido-completing-read
(defvar ido-choice-list)

;; Stores the list of items which are ignored when building
;; `ido-cur-list'.  It is in no specific order.
(defvar ido-ignored-list)

;; Remember if current directory is non-readable (so we cannot do completion).
(defvar ido-directory-nonreadable)

;; Remember if current directory is 'huge' (so we don't want to do completion).
(defvar ido-directory-too-big)

;; Keep current item list if non-nil.
(defvar ido-keep-item-list)

;; Process ido-ignore-* lists.
(defvar ido-process-ignore-lists)

;; Don't process ido-ignore- lists once.
(defvar ido-process-ignore-lists-inhibit)

;; Buffer from which ido was entered.
(defvar ido-entry-buffer)

;; Non-nil if matching file must be selected.
(defvar ido-require-match)

;; Non-nil if we should add [confirm] to prompt
(defvar ido-show-confirm-message)

;; Stores a temporary version of the file list being created.
(defvar ido-temp-list)

;; Non-nil if default list element should be rotated into place.
(defvar ido-rotate-temp)

;; Stores current index in ido-work-directory-list.
(defvar ido-work-directory-index)

;; Stores current index in ido-work-file-list.
(defvar ido-work-file-index)

;; Set when merged work directory list is in use.
(defvar ido-use-merged-list)

;; Set when merged work directory list not yet built.
(defvar ido-try-merged-list)

;; Saved state prior to last work directory merge.
;; Value is a list (ido-text dir cur-list ignored-list matches).
(defvar ido-pre-merge-state)

;; Original value of vc-handled-backends for use in ido-toggle-vc.
(defvar ido-saved-vc-hb)

;; Stores temporary state of literal find file.
(defvar ido-find-literal)

;; Set to 'ignore to inhibit switching between find-file/switch-buffer.
(defvar ido-context-switch-command)

;; Dynamically bound in ido-read-internal.
(defvar ido-completing-read)

;;; FUNCTIONS

(defun ido-active (&optional merge)
  (if merge
      ido-use-merged-list
    (and (boundp 'ido-completing-read)
	 (or (featurep 'xemacs)
	     (= ido-use-mycompletion-depth (minibuffer-depth))))))

(defvar ido-trace-enable nil)

(defun ido-trace (p &optional s retval)
  (if ido-trace-enable
      (let ((b (get-buffer-create " *IDO Trace*"))
	    (deactivate-mark deactivate-mark))
	(save-excursion
	  (save-restriction
	    (set-buffer b)
	    (insert p ": " (if (stringp s) s (format "%S" s)) "\n")))))
  retval)

(defun ido-toggle-trace (arg)
  (interactive "P")
  (setq ido-trace-enable (or arg (not ido-trace-enable)))
  (if ido-trace-enable
      (message "IDO trace on"))
  (let ((b (get-buffer " *IDO Trace*")))
    (if b
	(if ido-trace-enable
	    (kill-buffer b)
	  (pop-to-buffer b t t)
	  (setq truncate-lines t)))))

(defun ido-local-file-exists-p (file)
  "Tell if FILE exists locally."
  (let (file-name-handler-alist)
    (file-exists-p file)))

(defun ido-unc-hosts (&optional query)
  "Return list of UNC host names."
  (let ((hosts
	 (cond
	  ((listp ido-unc-hosts)
	   ido-unc-hosts)		;; static list or nil
	  ((listp ido-unc-hosts-cache)
	   ido-unc-hosts-cache)	;; result of net search
	  ((and query (fboundp ido-unc-hosts))
	   (message (propertize "Searching for UNC hosts..." 'face 'highlight))
	   (setq ido-unc-hosts-cache (funcall ido-unc-hosts))
	   (message nil)
	   ido-unc-hosts-cache)
	  (query
	   (setq ido-unc-hosts-cache nil))
	  (t (fboundp ido-unc-hosts)))))
    (when query
      (let ((case-fold-search ido-downcase-unc-hosts)
	    res host re-list re)
	(while hosts
	  (setq host (car hosts)
		hosts (cdr hosts)
		re-list (and ido-process-ignore-lists
			     ido-ignore-unc-host-regexps))
	  (while re-list
	    (setq re (car re-list)
		  re-list (cdr re-list))
	    (if (string-match re host)
		(setq re-list nil
		      host nil)))
	  (when host
	    (when ido-downcase-unc-hosts
	      (setq host (downcase host)))
	    (setq res (cons host res))))
	(setq hosts (sort res #'string<))))
    hosts))

(defun ido-unc-hosts-net-view ()
  "Query network for list of UNC host names using `NET VIEW'."
  (let (hosts)
    (with-temp-buffer
      (shell-command "net view" t)
      (goto-char (point-min))
      (while (re-search-forward "^\\\\\\\\\\([[:graph:]]+\\)" nil t)
	(setq hosts (cons (match-string 1) hosts))))
    hosts))

(defun ido-is-tramp-root (&optional dir)
  (and ido-enable-tramp-completion
       (string-match "\\`/[^/]+[@:]\\'"
		     (or dir ido-current-directory))))

(defun ido-is-unc-root (&optional dir)
  (and (ido-unc-hosts)
       (string-equal "//"
		     (or dir ido-current-directory))))

(defun ido-is-unc-host (&optional dir)
  (and (ido-unc-hosts)
       (string-match "\\`//[^/]+/\\'"
		     (or dir ido-current-directory))))

(defun ido-is-root-directory (&optional dir)
  (setq dir (or dir ido-current-directory))
  (or
   (string-equal "/" dir)
   (and (memq system-type '(windows-nt ms-dos))
	(string-match "\\`[a-zA-Z]:[/\\]\\'" dir))
   (if ido-enable-tramp-completion
       (ido-is-tramp-root dir)
     (string-match "\\`/[^:/][^:/]+:\\'" dir))))

(defun ido-is-ftp-directory (&optional dir)
  (string-match
   (if ido-enable-tramp-completion
       "\\`/[^/:][^/:]+:"  ;; like tramp-file-name-regexp-unified, but doesn't match single drive letters
     "\\`/[^/:][^/:]+:/")
   (or dir ido-current-directory)))

(defun ido-is-slow-ftp-host (&optional dir)
  (and (or ido-slow-ftp-hosts ido-slow-ftp-host-regexps)
       (setq dir (or dir ido-current-directory))
       ;; (featurep 'ange-ftp)
       ;; (ange-ftp-ftp-name dir)
       (string-match
	(if ido-enable-tramp-completion
	    "\\`/\\([^/]+[@:]\\)*\\([^@/:][^@/:]+\\):"
	  "\\`/\\([^/:]*@\\)?\\([^@/:][^@/:]+\\):/")
	dir)
       (let ((host (substring dir (match-beginning 2) (match-end 2))))
	 (or (member host ido-slow-ftp-hosts)
	     (let ((re ido-slow-ftp-host-regexps))
	       (while (and re (not (string-match (car re) host)))
		 (setq re (cdr re)))
	       re)))))

(defun ido-time-stamp (&optional time)
  ;; Time is a floating point number (fractions of 1 hour)
  (setq time (or time (current-time)))
  (/ (+ (* (car time) 65536.0) (car (cdr time))) 3600.0))

(defun ido-cache-ftp-valid (&optional time)
  (and (numberp ido-cache-ftp-work-directory-time)
       (> ido-cache-ftp-work-directory-time 0)
       (or (not time)
	   (< (- (ido-time-stamp) time) ido-cache-ftp-work-directory-time))))

(defun ido-cache-unc-valid (&optional time)
  (and (numberp ido-cache-unc-host-shares-time)
       (> ido-cache-unc-host-shares-time 0)
       (or (not time)
	   (< (- (ido-time-stamp) time) ido-cache-unc-host-shares-time))))

(defun ido-may-cache-directory (&optional dir)
  (setq dir (or dir ido-current-directory))
  (cond
   ((and (ido-is-root-directory dir)
	 (or ido-enable-tramp-completion
	     (memq system-type '(windows-nt ms-dos))))
    nil)
   ((ido-is-unc-host dir)
    (ido-cache-unc-valid))
   ((ido-is-ftp-directory dir)
    (ido-cache-ftp-valid))
   ((ido-directory-too-big-p dir)
    nil)
   (t t)))

(defun ido-pp (list &optional sep)
  (let ((print-level nil) (eval-expression-print-level nil)
	(print-length nil) (eval-expression-print-length nil))
    (insert "\n;; ----- " (symbol-name list) " -----\n(\n ")
    (setq list (symbol-value list))
    (while list
      (let* ((elt (car list))
	     (s (if (consp elt) (car elt) elt)))
	(if (and (stringp s) (= (length s) 0))
	    (setq s nil))
	(if s
	    (prin1 elt (current-buffer)))
	(if (and (setq list (cdr list)) s)
	    (insert (or sep "\n ")))))
    (insert "\n)\n")))

(defun ido-save-history ()
  "Save ido history and cache information between sessions."
  (interactive)
  (when (and ido-last-directory-list ido-save-directory-list-file)
    (let ((buf (get-buffer-create " *ido session*"))
	  (version-control 'never))
      (unwind-protect
	  (with-current-buffer buf
	    (erase-buffer)
	    (insert ";;; -*- coding: utf-8 -*-\n")
	    (setq buffer-file-coding-system 'utf-8)
	    (ido-pp 'ido-last-directory-list)
	    (ido-pp 'ido-work-directory-list)
	    (ido-pp 'ido-work-file-list)
	    (ido-pp 'ido-dir-file-cache "\n\n ")
	    (if (listp ido-unc-hosts-cache)
		(ido-pp 'ido-unc-hosts-cache)
	      (insert "\n;; ----- ido-unc-hosts-cache -----\nt\n"))
	    (write-file ido-save-directory-list-file nil))
	(kill-buffer buf)))))

(defun ido-load-history (&optional arg)
  "Load ido history and cache information from previous session.
With prefix argument, reload history unconditionally."
  (interactive "P")
  (if (or arg (and ido-save-directory-list-file (not ido-last-directory-list)))
      (let ((file (expand-file-name ido-save-directory-list-file))
	    buf)
	(when (file-readable-p file)
	  (setq buf (get-buffer-create " *ido session*"))
	  (unwind-protect
	      (with-current-buffer buf
		(erase-buffer)
		(insert-file-contents file)
		(condition-case nil
		    (setq ido-last-directory-list (read (current-buffer))
			  ido-work-directory-list (read (current-buffer))
			  ido-work-file-list (read (current-buffer))
			  ido-dir-file-cache (read (current-buffer))
			  ido-unc-hosts-cache (read (current-buffer)))
		  (error nil)))
	    (kill-buffer buf)))))
  (ido-wash-history))

(defun ido-wash-history ()
  "Clean-up ido history and cache information.
Removes badly formatted data and ignored directories."
  (interactive)
  ;; Check format of each of our lists, discard bogus elements
  (setq ido-last-directory-list
	(and (listp ido-last-directory-list)
	     (let ((l ido-last-directory-list) r)
	       (while l
		 (if (and (consp (car l))
			  (stringp (car (car l)))
			  (stringp (cdr (car l))))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))
  (setq ido-work-directory-list
	(and (listp ido-work-directory-list)
	     (let ((l ido-work-directory-list) r)
	       (while l
		 (if (and (stringp (car l))
			  (or ido-record-ftp-work-directories
			      (not (ido-is-ftp-directory (car l)))))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))
  (setq ido-work-file-list
	(and (listp ido-work-file-list)
	     (let ((l ido-work-file-list) r)
	       (while l
		 (if (stringp (car l))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))
  (setq ido-dir-file-cache
	(and (listp ido-dir-file-cache)
	     (let ((l ido-dir-file-cache) r)
	       (while l
		 (if (and (listp (car l))
			  (> (length (car l)) 2)
			  (let ((dir (car (car l)))
				(time (car (cdr (car l))))
				(files (cdr (cdr (car l)))))
			    (and
			     (stringp dir)
			     (consp time)
			     (cond
			      ((integerp (car time))
			       (and (/= (car time) 0)
				    (integerp (car (cdr time)))
				    (/= (car (cdr time)) 0)
				    (ido-may-cache-directory dir)))
			      ((eq (car time) 'ftp)
			       (and (numberp (cdr time))
				    (ido-is-ftp-directory dir)
				    (ido-cache-ftp-valid (cdr time))))
			      ((eq (car time) 'unc)
			       (and (numberp (cdr time))
				    (ido-is-unc-host dir)
				    (ido-cache-unc-valid (cdr time))))
			      (t nil))
			     (let ((s files) (ok t))
			       (while s
				 (if (stringp (car s))
				     (setq s (cdr s))
				   (setq s nil ok nil)))
			       ok))))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))

  ;; Remove ignored directories from work directory list
  ;; according to ido-work-directory-list-ignore-regexps
  (if ido-work-directory-list
      (let ((dirs (reverse ido-work-directory-list)))
	(setq ido-work-directory-list nil)
	(while dirs
	  (ido-record-work-directory (car dirs))
	  (setq dirs (cdr dirs)))))
  ;; Get rid of text properties
  (let ((l ido-last-directory-list) e)
    (while l
      (setq e (car l) l (cdr l))
      (set-text-properties 0 (length (car e)) nil (car e))
      (set-text-properties 0 (length (cdr e)) nil (cdr e))))
  (let ((l ido-work-directory-list) e)
    (while l
      (setq e (car l) l (cdr l))
      (set-text-properties 0 (length e) nil e)))
  (let ((l ido-work-file-list) e)
    (while l
      (setq e (car l) l (cdr l))
      (set-text-properties 0 (length e) nil e)))
  (let ((l ido-dir-file-cache) e d)
    (while l
      (setq e (car l) l (cdr l))
      (if (listp e)
	  (while e
	    (setq d (car e) e (cdr e))
	    (if (not (consp d))
		(set-text-properties 0 (length d) nil d)))))))


(defun ido-kill-emacs-hook ()
  ;; ido kill emacs hook
  (ido-save-history))

(defun ido-common-initialization ()
  (ido-init-completion-maps)
  (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
  (add-hook 'choose-completion-string-functions 'ido-choose-completion-string))

(define-minor-mode ido-everywhere
  "Toggle use of Ido for all buffer/file reading.
With a prefix argument ARG, enable this feature if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  :group 'ido
  (when (get 'ido-everywhere 'file)
    (setq read-file-name-function (car (get 'ido-everywhere 'file)))
    (put 'ido-everywhere 'file nil))
  (when (get 'ido-everywhere 'buffer)
    (setq read-buffer-function (car (get 'ido-everywhere 'buffer)))
    (put 'ido-everywhere 'buffer nil))
  (when ido-everywhere
    (when (memq ido-mode '(both file))
      (put 'ido-everywhere 'file (cons read-file-name-function nil))
      (setq read-file-name-function 'ido-read-file-name))
    (when (memq ido-mode '(both buffer))
      (put 'ido-everywhere 'buffer (cons read-buffer-function nil))
      (setq read-buffer-function 'ido-read-buffer))))

(defvar ido-minor-mode-map-entry nil)

;;;###autoload
(defun ido-mode (&optional arg)
  "Toggle ido mode on or off.
With ARG, turn ido-mode on if arg is positive, off otherwise.
Turning on ido-mode will remap (via a minor-mode keymap) the default
keybindings for the `find-file' and `switch-to-buffer' families of
commands to the ido versions of these functions.
However, if ARG arg equals 'files, remap only commands for files, or
if it equals 'buffers, remap only commands for buffer switching.
This function also adds a hook to the minibuffer."
  (interactive "P")
  (setq ido-mode
	(cond
	 ((null arg) (if ido-mode nil 'both))
	 ((eq arg t) 'both)
	 ((eq arg 'files) 'file)
	 ((eq arg 'buffers) 'buffer)
	 ((memq arg '(file buffer both)) arg)
	 ((> (prefix-numeric-value arg) 0) 'both)
	 (t nil)))

  (ido-everywhere (if ido-everywhere 1 -1))

  (when ido-mode
    (ido-common-initialization)
    (ido-load-history)

    (add-hook 'kill-emacs-hook 'ido-kill-emacs-hook)

    (let ((map (make-sparse-keymap)))
      (when (memq ido-mode '(file both))
	(define-key map [remap find-file] 'ido-find-file)
	(define-key map [remap find-file-read-only] 'ido-find-file-read-only)
	(define-key map [remap find-alternate-file] 'ido-find-alternate-file)
	(define-key map [remap write-file] 'ido-write-file)
	(define-key map [remap insert-file] 'ido-insert-file)
	(define-key map [remap list-directory] 'ido-list-directory)
	(define-key map [remap dired] 'ido-dired)
	(define-key map [remap find-file-other-window]
          'ido-find-file-other-window)
	(define-key map [remap find-file-read-only-other-window]
          'ido-find-file-read-only-other-window)
	(define-key map [remap find-file-other-frame]
          'ido-find-file-other-frame)
	(define-key map [remap find-file-read-only-other-frame]
          'ido-find-file-read-only-other-frame))

      (when (memq ido-mode '(buffer both))
	(define-key map [remap switch-to-buffer] 'ido-switch-buffer)
	(define-key map [remap switch-to-buffer-other-window]
          'ido-switch-buffer-other-window)
	(define-key map [remap switch-to-buffer-other-frame]
          'ido-switch-buffer-other-frame)
	(define-key map [remap insert-buffer] 'ido-insert-buffer)
	(define-key map [remap kill-buffer] 'ido-kill-buffer)
	(define-key map [remap display-buffer] 'ido-display-buffer))

      (if ido-minor-mode-map-entry
	  (setcdr ido-minor-mode-map-entry map)
	(setq ido-minor-mode-map-entry (cons 'ido-mode map))
	(add-to-list 'minor-mode-map-alist ido-minor-mode-map-entry))))

  (when (called-interactively-p 'any)
    (message "Ido mode %s" (if ido-mode "enabled" "disabled"))))


;;; IDO KEYMAP
(defun ido-init-completion-maps ()
  "Set up the completion keymaps used by `ido'."

  ;; Common map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-a" 'ido-toggle-ignore)
    (define-key map "\C-c" 'ido-toggle-case)
    (define-key map "\C-e" 'ido-edit-input)
    (define-key map "\t" 'ido-complete)
    (define-key map " " 'ido-complete-space)
    (define-key map "\C-j" 'ido-select-text)
    (define-key map "\C-m" 'ido-exit-minibuffer)
    (define-key map "\C-p" 'ido-toggle-prefix)
    (define-key map "\C-r" 'ido-prev-match)
    (define-key map "\C-s" 'ido-next-match)
    (define-key map "\C-t" 'ido-toggle-regexp)
    (define-key map "\C-z" 'ido-undo-merge-work-directory)
    (define-key map [(control ?\s)] 'ido-restrict-to-matches)
    (define-key map [(meta ?\s)] 'ido-take-first-match)
    (define-key map [(control ?@)] 'ido-restrict-to-matches)
    (define-key map [right] 'ido-next-match)
    (define-key map [left] 'ido-prev-match)
    (define-key map "?" 'ido-completion-help)
    ;; Magic commands.
    (define-key map "\C-b" 'ido-magic-backward-char)
    (define-key map "\C-f" 'ido-magic-forward-char)
    (define-key map "\C-d" 'ido-magic-delete-char)
    (set-keymap-parent map minibuffer-local-map)
    (setq ido-common-completion-map map))

  ;; File and directory map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-b" 'ido-enter-switch-buffer)
    (define-key map "\C-x\C-f" 'ido-fallback-command)
    (define-key map "\C-x\C-d" 'ido-enter-dired)
    (define-key map [down] 'ido-next-match-dir)
    (define-key map [up]   'ido-prev-match-dir)
    (define-key map [(meta up)] 'ido-prev-work-directory)
    (define-key map [(meta down)] 'ido-next-work-directory)
    (define-key map [backspace] 'ido-delete-backward-updir)
    (define-key map "\d"        'ido-delete-backward-updir)
    (define-key map [remap delete-backward-char] 'ido-delete-backward-updir) ; BS
    (define-key map [remap backward-kill-word] 'ido-delete-backward-word-updir)  ; M-DEL

    (define-key map [(control backspace)] 'ido-up-directory)
    (define-key map "\C-l" 'ido-reread-directory)
    (define-key map [(meta ?d)] 'ido-wide-find-dir-or-delete-dir)
    (define-key map [(meta ?b)] 'ido-push-dir)
    (define-key map [(meta ?v)] 'ido-push-dir-first)
    (define-key map [(meta ?f)] 'ido-wide-find-file-or-pop-dir)
    (define-key map [(meta ?k)] 'ido-forget-work-directory)
    (define-key map [(meta ?m)] 'ido-make-directory)
    (define-key map [(meta ?n)] 'ido-next-work-directory)
    (define-key map [(meta ?o)] 'ido-prev-work-file)
    (define-key map [(meta control ?o)] 'ido-next-work-file)
    (define-key map [(meta ?p)] 'ido-prev-work-directory)
    (define-key map [(meta ?s)] 'ido-merge-work-directories)
    (set-keymap-parent map ido-common-completion-map)
    (setq ido-file-dir-completion-map map))

  ;; File only map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-k" 'ido-delete-file-at-head)
    (define-key map "\C-o" 'ido-copy-current-word)
    (define-key map "\C-w" 'ido-copy-current-file-name)
    (define-key map [(meta ?l)] 'ido-toggle-literal)
    (set-keymap-parent map ido-file-dir-completion-map)
    (setq ido-file-completion-map map))

  ;; Buffer map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-f" 'ido-enter-find-file)
    (define-key map "\C-x\C-b" 'ido-fallback-command)
    (define-key map "\C-k" 'ido-kill-buffer-at-head)
    (define-key map "\C-o" 'ido-toggle-virtual-buffers)
    (set-keymap-parent map ido-common-completion-map)
    (setq ido-buffer-completion-map map)))


(defun ido-setup-completion-map ()
  "Set up the keymap for `ido'."

  ;; generated every time so that it can inherit new functions.
  (let ((map (make-sparse-keymap))
	(viper-p (if (boundp 'viper-mode) viper-mode)))

    (when viper-p
      (define-key map [remap viper-intercept-ESC-key] 'ignore))

    (cond
     ((memq ido-cur-item '(file dir))
      (when ido-context-switch-command
	(define-key map "\C-x\C-b" ido-context-switch-command)
	(define-key map "\C-x\C-d" 'ignore))
      (when viper-p
	(define-key map [remap viper-backward-char] 'ido-delete-backward-updir)
	(define-key map [remap viper-del-backward-char-in-insert] 'ido-delete-backward-updir)
	(define-key map [remap viper-delete-backward-word] 'ido-delete-backward-word-updir))
      (set-keymap-parent map
			 (if (eq ido-cur-item 'file)
			     ido-file-completion-map
			   ido-file-dir-completion-map)))

     ((eq ido-cur-item 'buffer)
      (when ido-context-switch-command
	(define-key map "\C-x\C-f" ido-context-switch-command))
      (set-keymap-parent map ido-buffer-completion-map))

     (t
      (set-keymap-parent map ido-common-completion-map)))

    (setq ido-completion-map map)))

(defun ido-final-slash (dir &optional fix-it)
  ;; return DIR if DIR has final slash.
  ;; else if FIX-IT is non-nil, return DIR/
  ;; else return nil.
  (setq dir (ido-name dir))
  (cond
   ((string-match "/\\'" dir) dir)
   ((ido-is-tramp-root dir) dir)
   (fix-it (concat dir "/"))
   (t nil)))

(defun ido-no-final-slash (s)
  ;; Remove optional final slash from string S
  (let ((l (1- (length s))))
    (if (and (> l 0) (eq (aref s l) ?/))
	(substring s 0 l)
      s)))

(defun ido-nonreadable-directory-p (dir)
  ;; Return t if dir is a directory, but not readable
  ;; Do not check for non-readable directories via tramp, as this causes a premature
  ;; connect on incomplete tramp paths (after entering just method:).
  (let ((ido-enable-tramp-completion nil))
    (and (ido-final-slash dir)
	 (not (ido-is-unc-host dir))
	 (file-directory-p dir)
	 (not (file-readable-p dir)))))

(defun ido-directory-too-big-p (dir)
  ;; Return t if dir is a directory, but too big to show
  ;; Do not check for non-readable directories via tramp, as this causes a premature
  ;; connect on incomplete tramp paths (after entering just method:).
  (let ((ido-enable-tramp-completion nil))
    (and (numberp ido-max-directory-size)
	 (ido-final-slash dir)
	 (not (ido-is-unc-host dir))
	 (file-directory-p dir)
	 (> (nth 7 (file-attributes dir)) ido-max-directory-size))))

(defun ido-set-current-directory (dir &optional subdir no-merge)
  ;; Set ido's current directory to DIR or DIR/SUBDIR
  (unless (and ido-enable-tramp-completion
	       (string-match "\\`/[^/]*@\\'" dir))
    (setq dir (ido-final-slash dir t)))
  (setq ido-use-merged-list nil
	ido-try-merged-list (not no-merge))
  (when subdir
    (setq dir (concat dir subdir))
    (unless (and ido-enable-tramp-completion
		 (string-match "\\`/[^/]*@\\'" dir))
      (setq dir (ido-final-slash dir t))))
  (and ido-completion-buffer
       (get-buffer ido-completion-buffer)
       (kill-buffer ido-completion-buffer))
  (cond
   ((equal dir ido-current-directory)
    nil)
   ((ido-is-unc-root dir)
    (ido-trace "unc" dir)
    (setq ido-current-directory dir)
    (setq ido-directory-nonreadable nil)
    (setq ido-directory-too-big nil)
    t)
   (t
    (ido-trace "cd" dir)
    (setq ido-current-directory dir)
    (and ido-completion-buffer
	 (get-buffer ido-completion-buffer)
	 (kill-buffer ido-completion-buffer))
    (setq ido-directory-nonreadable (ido-nonreadable-directory-p dir))
    (setq ido-directory-too-big (and (not ido-directory-nonreadable)
				     (ido-directory-too-big-p dir)))
    t)))

(defun ido-set-current-home (&optional dir)
  ;; Set ido's current directory to user's home directory
  (ido-set-current-directory (expand-file-name (or dir "~/"))))

(defun ido-record-command (command arg)
  ;; Add (command arg) to command-history if ido-record-commands is t
  (if ido-record-commands
      (let ((cmd (list command arg)))
	(if (or (not command-history)
		(not (equal cmd (car command-history))))
	    (setq command-history (cons cmd command-history))))))

(defun ido-make-prompt (item prompt)
  ;; Make the prompt for ido-read-internal
  (cond
   ((and (memq item '(file dir)) ido-current-directory)
    (let ((dirname (abbreviate-file-name ido-current-directory))
	  (max-width (if (and ido-max-file-prompt-width (floatp ido-max-file-prompt-width))
			 (floor (* (frame-width) ido-max-file-prompt-width))
		       ido-max-file-prompt-width))
	  (literal (and (boundp 'ido-find-literal) ido-find-literal "(literal) "))
	  (vc-off (and ido-saved-vc-hb (not vc-handled-backends) "[-VC] "))
	  (prefix nil)
	  (rule ido-rewrite-file-prompt-rules))
      (let ((case-fold-search nil))
	(while rule
	  (if (and (consp (car rule))
		   (string-match (car (car rule)) dirname))
	      (setq dirname
		    (if (stringp (cdr (car rule)))
			(replace-match (cdr (car rule)) t nil dirname)
		      (funcall (cdr (car rule)) dirname))))
	  (setq rule (cdr rule))))
      (run-hooks 'ido-rewrite-file-prompt-functions)
      (concat prompt
	      ; (if ido-process-ignore-lists "" "&")
	      (or literal "")
	      (or vc-off  "")
	      (or prefix "")
	      (let ((l (length dirname)))
		(if (and max-width (> max-width 0) (> l max-width))
		    (let* ((s (substring dirname (- max-width)))
			   (i (string-match "/" s)))
		      (concat "..." (if i (substring s i) s)))
		  dirname)))))
   (t prompt)))

;; Here is very briefly how ido-find-file works:
;;
;;  (ido-find-file)
;;    (ido-file-internal method)
;;       set ido-current-directory
;;       (ido-read-internal 'file ...)
;;          (while ...
;;             (ido-make-item-list ...)
;;             (ido-set-matches)
;;             (completing-read ... ido-text-init ...)
;;
;;               ... here user is allowed to type characters and commands
;;                   a command may set ido-exit and call (exit-minibuffer)
;;                   to make ido-read-internal do advanced tasks (or return)
;;
;;               ... ido-tidy and ido-exhibit are pre- and post-hooks
;;                   which are run before and after each user command.
;;
;;             return value from completing-read is stored in ido-final-text
;;             - ido-exit may cause further actions to be taken:
;;               'refresh - repeat loop (make-item-list, set-matches)
;;               'edit    - edit the prompt string, then repeat loop
;;               'keep    - repeat loop but don't (re)make-item-list
;;               'updir   - go up one directory, repeat loop
;;               else set ido-selected based on ido-final-text,
;;               optionally update ido-current-directory and repeat loop, or
;;               exit with the return value of ido-selected (file name)
;;       selected file name is returned from ido-read-internal,
;;       ido-exit and method determines what action is taken
;;       e.g. the file name may be ignored or joined with ido-current-directory, and
;;       the relevant function is called (find-file, write-file, etc).

(defun ido-read-internal (item prompt hist &optional default require-match initial)
  "Perform the `ido-read-buffer' and `ido-read-file-name' functions.
Return the name of a buffer or file selected.
PROMPT is the prompt to give to the user.
DEFAULT if given is the default item to start with.
If REQUIRE-MATCH is non-nil, an existing file must be selected.
If INITIAL is non-nil, it specifies the initial input string."
  (let
      ((ido-cur-item item)
       (ido-entry-buffer (current-buffer))
       (ido-process-ignore-lists t)
       (ido-process-ignore-lists-inhibit nil)
       (ido-set-default-item t)
       ido-default-item
       ido-selected
       ido-final-text
       (done nil)
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; Exported dynamic variables:
       ido-cur-list
       ido-ignored-list
       (ido-rotate-temp nil)
       (ido-keep-item-list nil)
       (ido-use-merged-list nil)
       (ido-try-merged-list t)
       (ido-pre-merge-state nil)
       (ido-case-fold ido-case-fold)
       (ido-enable-prefix ido-enable-prefix)
       (ido-enable-regexp ido-enable-regexp)
       (ido-show-confirm-message nil)
       )

    (ido-setup-completion-map)
    (setq ido-text-init initial)
    (setq ido-input-stack nil)

    (run-hooks 'ido-setup-hook)

    (while (not done)
      (ido-trace "\n_LOOP_" ido-text-init)
      (setq ido-exit nil)
      (setq ido-rescan t)
      (setq ido-rotate nil)
      (setq ido-text "")
      (when ido-set-default-item
	(setq ido-default-item
	      (cond
	       ((eq item 'buffer)
		(if (bufferp default) (buffer-name default) default))
	       ((stringp default)
		(if (memq item '(file dir))
		    (file-name-nondirectory default)
		  default))
	       ((eq item 'file)
		(and ido-enable-last-directory-history
		     (let ((d (assoc ido-current-directory ido-last-directory-list)))
		       (and d (cdr d)))))))
	(if (member ido-default-item ido-ignore-item-temp-list)
	    (setq ido-default-item nil))
	(ido-trace "new default" ido-default-item)
	(if ido-default-item
	    (setq ido-initial-position 0))
	(setq ido-set-default-item nil))

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists nil))

      (if (and ido-use-merged-list (memq ido-try-merged-list '(t wide)) (not ido-keep-item-list))
	  (let ((olist ido-cur-list)
		(oign ido-ignored-list)
		(omat ido-matches)
		(l (ido-make-merged-file-list ido-text-init
					      (eq ido-use-merged-list 'auto)
					      (eq ido-try-merged-list 'wide))))
	    (ido-trace "merged" l)
	    (cond
	     ((not l)
	      (if (eq ido-try-merged-list 'wide)
		  (setq ido-pre-merge-state
			(list "" ido-current-directory olist oign omat)
			ido-cur-list nil
			ido-ignored-list nil
			ido-matches nil
			ido-keep-item-list t
			ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
			ido-use-merged-list nil)
		(setq ido-cur-list olist
		      ido-ignored-list oign
		      ido-matches omat
		      ido-keep-item-list t
		      ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
		      ido-use-merged-list nil)))
	     ((eq l t)
	      (setq ido-use-merged-list nil))
	     ((eq l 'input-pending-p)
	      (setq ido-try-merged-list t
		    ido-use-merged-list nil))
	     (t
	      (setq ido-pre-merge-state
		    (list ido-text-init ido-current-directory olist oign omat))
	      (ido-set-current-directory (car (cdr (car l))))
	      (if (ido-final-slash ido-text-init)
		  (setq ido-text-init ""))
	      (setq ido-cur-list l
		    ido-ignored-list nil
		    ido-matches l
		    ido-rescan nil
		    ido-keep-item-list t
		    ido-use-merged-list t)
	      (ido-trace "Merged" t)
	      ))))

      (cond
       (ido-keep-item-list
	(setq ido-keep-item-list nil
	      ido-rescan nil))
       ((eq ido-cur-item 'file)
	(setq ido-ignored-list nil
	      ido-cur-list (and (not ido-directory-nonreadable)
				(not ido-directory-too-big)
				(ido-make-file-list ido-default-item))))
       ((eq ido-cur-item 'dir)
	(setq ido-ignored-list nil
	      ido-cur-list (and (not ido-directory-nonreadable)
				(not ido-directory-too-big)
				(ido-make-dir-list ido-default-item))))
       ((eq ido-cur-item 'buffer)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-buffer-list ido-default-item)))
       ((eq ido-cur-item 'list)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-choice-list ido-default-item)))
       (t nil))
      (setq ido-rotate-temp nil)

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists t
		ido-process-ignore-lists-inhibit nil))

      (ido-set-matches)
      (if (and ido-matches (eq ido-try-merged-list 'auto))
	  (setq ido-try-merged-list t))
      (let ((max-mini-window-height (or ido-max-window-height
					(and (boundp 'max-mini-window-height)
					     max-mini-window-height)))
	   (ido-completing-read t)
	   (ido-require-match require-match)
	   (ido-use-mycompletion-depth (1+ (minibuffer-depth)))
	   (show-paren-mode nil)
	   ;; Postpone history adding till later
	   (history-add-new-input nil))
	;; prompt the user for the file name
	(setq ido-exit nil)
	(setq ido-final-text
	      (catch 'ido
		(read-from-minibuffer (ido-make-prompt item prompt)
				      (prog1 ido-text-init
					(setq ido-text-init nil))
				      ido-completion-map nil hist))))
      (ido-trace "read-from-minibuffer" ido-final-text)
      (and ido-completion-buffer
	   (get-buffer ido-completion-buffer)
	   (kill-buffer ido-completion-buffer))

      (ido-trace "\n_EXIT_" ido-exit)

      (cond
       ((eq ido-exit 'refresh)
	(if (and (eq ido-use-merged-list 'auto)
		 (or (input-pending-p)))
	    (setq ido-use-merged-list nil
		  ido-keep-item-list t))
	nil)

       ((eq ido-exit 'done)
	(setq done t
	      ido-selected ido-text
	      ido-exit nil))

       ((memq ido-exit '(edit chdir))
	(cond
	 ((memq ido-cur-item '(file dir))
	  (let* ((read-file-name-function nil)
		 (edit (eq ido-exit 'edit))
		 (d ido-current-directory)
		 (f ido-text-init)
		 (new t))
	    (setq ido-text-init "")
	    (while new
	      (setq new (if edit
			    (condition-case nil
				(read-file-name (concat prompt "[EDIT] ")
						(expand-file-name d)
						(concat d f) nil f)
			      (quit (concat d f)))
			   f)
		    d (or (file-name-directory new) "/")
		    f (file-name-nondirectory new)
		    edit t)
	      (if (or
		   (file-directory-p d)
		   (and (yes-or-no-p (format "Create directory %s? " d))
			(condition-case nil
			    (progn (make-directory d t) t)
			  (error
			   (message "Could not create directory")
			   (sit-for 1)
			   nil))))
		  (progn
		    (ido-set-current-directory d nil (eq ido-exit 'chdir))
		    (setq ido-text-init f
			  new nil))))))
	 (t
	  (setq ido-text-init
		(condition-case nil
		    (read-string (concat prompt "[EDIT] ") ido-final-text)
		  (quit ido-final-text)))))

	nil)

       ((eq ido-exit 'keep)
	(setq ido-keep-item-list t))

       ((memq ido-exit '(dired fallback find-file switch-to-buffer insert-buffer insert-file))
	(setq done t))

       ((memq ido-exit '(updir push))
	;; cannot go up if already at the root-dir (Unix) or at the
	;; root-dir of a certain drive (Windows or MS-DOS).
        (if (ido-is-tramp-root)
	    (when (string-match "\\`\\(/\\([^/]+[:@]\\)*\\)\\([^/]+\\)[:@]\\'" ido-current-directory)
	      (setq ido-text-init (match-string 3 ido-current-directory))
	      (ido-set-current-directory (match-string 1 ido-current-directory))
	      (setq ido-set-default-item t))
	  (unless (ido-is-root-directory)
	    (when (eq ido-exit 'push)
	      (setq ido-input-stack (cons (cons ido-cur-item ido-text) ido-input-stack))
	      (setq ido-cur-item 'dir)
	      (setq ido-text-init (file-name-nondirectory (substring ido-current-directory 0 -1)))
	      (ido-trace "push" ido-input-stack))
	    (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1)))
	    (setq ido-set-default-item t))))

       ((eq ido-exit 'pop)
	(ido-trace "pop" ido-input-stack)
	(let ((elt (car ido-input-stack)))
	  (setq ido-input-stack (cdr ido-input-stack))
	  (ido-set-current-directory (concat ido-current-directory ido-text))
	  (setq ido-cur-item (car elt))
	  (setq ido-text-init (cdr elt))))

       ((eq ido-exit 'pop-all)
	(ido-trace "pop-all" ido-input-stack)
	(while ido-input-stack
	  (let ((elt (car ido-input-stack)))
	    (setq ido-input-stack (cdr ido-input-stack))
	    (ido-set-current-directory (concat ido-current-directory ido-text))
	    (setq ido-cur-item (car elt))
	    (setq ido-text-init (cdr elt)))))

       ;; Handling the require-match must be done in a better way.
       ((and require-match
	     (not (memq require-match '(confirm confirm-after-completion)))
	     (not (if ido-directory-too-big
		      (file-exists-p (concat ido-current-directory ido-final-text))
		    (ido-existing-item-p))))
	(error "Must specify valid item"))

       (t
	(setq ido-selected
	      (if (or (eq ido-exit 'takeprompt)
		      (null ido-matches))
		  ido-final-text
		;; else take head of list
		(ido-name (car ido-matches))))

	(cond
	 ((memq item '(buffer list))
	  (setq done t))

	 ((string-equal "./" ido-selected)
	  nil)

	 ((string-equal "../" ido-selected)
	  ;; cannot go up if already at the root-dir (Unix) or at the
	  ;; root-dir of a certain drive (Windows or MS-DOS).
	  (or (ido-is-root-directory)
	      (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1))))
	  (setq ido-set-default-item t))

	 ((and (string-match (if ido-enable-tramp-completion ".[:@]\\'" ".:\\'") ido-selected)
	       (ido-is-root-directory) ;; Ange-ftp or Tramp
	       (not (ido-local-file-exists-p ido-selected)))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (ido-trace "tramp prefix" ido-selected)
	  (if (ido-is-slow-ftp-host)
	      (setq ido-exit 'fallback
		    done t)
	    (setq ido-set-default-item t)))

	 ((or (string-match "[/\\][^/\\]" ido-selected)
	      (and (memq system-type '(windows-nt ms-dos))
		   (string-match "\\`[a-zA-Z]:" ido-selected)))
	  (ido-set-current-directory (file-name-directory ido-selected))
	  (setq ido-set-default-item t))

	 ((string-match "\\`~" ido-selected)
	  (ido-set-current-home ido-selected))

	 ((ido-final-slash ido-selected)
	  (if ido-enable-last-directory-history
	      (let ((x (assoc ido-current-directory ido-last-directory-list)))
		(if x
		    (setcdr x ido-selected)
		  (setq ido-last-directory-list
			(cons (cons ido-current-directory ido-selected) ido-last-directory-list)))))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (if ido-input-stack
	      ; automatically pop stack elements which match existing files or directories
	      (let (elt)
		(while (and (setq elt (car ido-input-stack))
			    (file-exists-p (concat ido-current-directory (cdr elt))))
		  (if (setq ido-input-stack (cdr ido-input-stack))
		      (ido-set-current-directory ido-current-directory (cdr elt))
		    (setq ido-text-init (cdr elt)))
		  (setq ido-cur-item (car elt))))
	    (setq ido-set-default-item t)))

	 (t
	  (setq done t))))))
    (add-to-history (cond
		     ((consp hist)
		      (or (car hist) 'minibuffer-history))
		     (hist hist)
		     (t 'minibuffer-history))
		    ido-selected)
    ido-selected))

(defun ido-edit-input ()
  "Edit absolute file name entered so far with ido; terminate by RET.
If cursor is not at the end of the user input, move to end of input."
  (interactive)
  (if (not (eobp))
      (end-of-line)
    (setq ido-text-init (if ido-matches (ido-name (car ido-matches)) ido-text))
    (setq ido-exit 'edit)
    (exit-minibuffer)))

;;; MAIN FUNCTIONS
(defun ido-buffer-internal (method &optional fallback prompt default initial switch-cmd)
  ;; Internal function for ido-switch-buffer and friends
  (if (not ido-mode)
      (progn
	(run-hook-with-args 'ido-before-fallback-functions
			    (or fallback 'switch-to-buffer))
	(call-interactively (or fallback 'switch-to-buffer)))
    (let* ((ido-context-switch-command switch-cmd)
	   (ido-current-directory nil)
	   (ido-directory-nonreadable nil)
	   (ido-directory-too-big nil)
	   (ido-use-virtual-buffers ido-use-virtual-buffers)
	   (require-match (confirm-nonexistent-file-or-buffer))
	   (buf (ido-read-internal 'buffer (or prompt "Buffer: ") 'ido-buffer-history default
				   require-match initial))
	   filename)

      ;; Choose the buffer name: either the text typed in, or the head
      ;; of the list of matches

      (cond
       ((eq ido-exit 'find-file)
	(ido-file-internal
	 (if (memq method '(other-window other-frame)) method ido-default-file-method)
	 nil nil nil nil ido-text))

       ((eq ido-exit 'insert-file)
	(ido-file-internal 'insert 'insert-file nil "Insert file: " nil ido-text 'ido-enter-insert-buffer))

       ((eq ido-exit 'fallback)
	(let ((read-buffer-function nil))
	  (setq this-command (or fallback 'switch-to-buffer))
	  (run-hook-with-args 'ido-before-fallback-functions this-command)
	  (call-interactively this-command)))

       ;; Check buf is non-nil.
       ((not buf) nil)
       ((= (length buf) 0) nil)

       ;; View buffer if it exists
       ((get-buffer buf)
	(add-to-history 'buffer-name-history buf)
	(if (eq method 'insert)
	    (progn
	      (ido-record-command 'insert-buffer buf)
	      (push-mark
	       (save-excursion
		 (insert-buffer-substring (get-buffer buf))
		 (point))))
	  (ido-visit-buffer buf method t)))

       ;; check for a virtual buffer reference
       ((and ido-use-virtual-buffers ido-virtual-buffers
	     (setq filename (assoc buf ido-virtual-buffers)))
	(ido-visit-buffer (find-file-noselect (cdr filename)) method t))

       ((and (eq ido-create-new-buffer 'prompt)
	     (null require-match)
	     (not (y-or-n-p (format "No buffer matching `%s', create one? " buf))))
	nil)

       ;; buffer doesn't exist
       ((and (eq ido-create-new-buffer 'never)
	     (null require-match))
	(message "No buffer matching `%s'" buf))

       ((and (eq ido-create-new-buffer 'prompt)
	     (null require-match)
	     (not (y-or-n-p (format "No buffer matching `%s', create one? " buf))))
	nil)

       ;; create a new buffer
       (t
	(add-to-history 'buffer-name-history buf)
	(setq buf (get-buffer-create buf))
	(if (fboundp 'set-buffer-major-mode)
	    (set-buffer-major-mode buf))
	(ido-visit-buffer buf method t))))))

(defun ido-record-work-directory (&optional dir)
  (when (and (numberp ido-max-work-directory-list) (> ido-max-work-directory-list 0))
    (if (and (setq dir (or dir ido-current-directory)) (> (length dir) 0))
	(let ((items ido-work-directory-list-ignore-regexps)
	      (case-fold-search nil))
	  (while (and items dir)
	    (if (string-match (car items) dir)
		(setq dir nil))
	    (setq items (cdr items)))
	  (if dir
	      (setq ido-work-directory-list (cons dir (delete dir ido-work-directory-list))))))
    (if (> (length ido-work-directory-list) ido-max-work-directory-list)
	(setcdr (nthcdr (1- ido-max-work-directory-list) ido-work-directory-list) nil))))

(defun ido-forget-work-directory ()
  (interactive)
  (when (and ido-current-directory ido-work-directory-list)
    (setq ido-work-directory-list (delete ido-current-directory ido-work-directory-list))
    (when ido-use-merged-list
      (ido-undo-merge-work-directory)
      (setq ido-exit 'refresh
	    ido-try-merged-list t
	    ido-use-merged-list t
	    ido-text-init ido-text
	    ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-record-work-file (name)
  ;; Save NAME in ido-work-file-list
  (when (and (numberp ido-max-work-file-list) (> ido-max-work-file-list 0))
    (or
     (and ido-work-file-list (equal (car ido-work-file-list) name))
     (setq ido-work-file-list (cons name (delete name ido-work-file-list))))
    (if (> (length ido-work-file-list) ido-max-work-file-list)
	(setcdr (nthcdr (1- ido-max-work-file-list) ido-work-file-list) nil))))

(defun ido-expand-directory (dir)
  ;; Expand DIR or use DEFAULT-DIRECTORY if nil.
  ;; Add final slash to result in case it was missing from DEFAULT-DIRECTORY.
  (ido-final-slash (expand-file-name (or dir default-directory)) t))

(defun ido-file-internal (method &optional fallback default prompt item initial switch-cmd)
  ;; Internal function for ido-find-file and friends
  (unless item
    (setq item 'file))
  (let ((ido-current-directory (ido-expand-directory default))
	(ido-context-switch-command switch-cmd)
	ido-directory-nonreadable ido-directory-too-big
	filename)

    (if (or (not ido-mode) (ido-is-slow-ftp-host))
	(setq filename t
	      ido-exit 'fallback)
      (setq ido-directory-nonreadable
	    (ido-nonreadable-directory-p ido-current-directory)
	    ido-directory-too-big
	    (and (not ido-directory-nonreadable)
		 (ido-directory-too-big-p ido-current-directory))))

    (when (and (eq item 'file)
	   (or ido-use-url-at-point ido-use-filename-at-point))
      (let (fn d)
	(require 'ffap)
	;; Duplicate code from ffap-guesser as we want different
	;; behavior for files and URLs.
	(cond
	 ((with-no-warnings
	    (and ido-use-url-at-point
		 ffap-url-regexp
		 (ffap-fixup-url (or (ffap-url-at-point)
				     (ffap-gopher-at-point)))))
	  (setq ido-exit 'ffap
		filename t))

	 ((and ido-use-filename-at-point
	       (setq fn (with-no-warnings
			  (if (eq ido-use-filename-at-point 'guess)
			      (ffap-guesser)
			    (ffap-string-at-point))))
	       (not (string-match "^http:/" fn))
	       (let ((absolute-fn (expand-file-name fn)))
		 (setq d (if (file-directory-p absolute-fn)
			     (file-name-as-directory absolute-fn)
			   (file-name-directory absolute-fn))))
	       (file-directory-p d))
	  (setq ido-current-directory d)
	  (setq initial (file-name-nondirectory fn))))))

    (let (ido-saved-vc-hb
	  (vc-handled-backends (and (boundp 'vc-handled-backends) vc-handled-backends))
	  (ido-work-directory-index -1)
	  (ido-work-file-index -1)
       	  (ido-find-literal nil))

      (unless filename
	(setq ido-saved-vc-hb vc-handled-backends)
	(let ((minibuffer-completing-file-name t))
	  (setq filename (ido-read-internal item
					    (or prompt "Find file: ")
					    'ido-file-history
					    (and (eq method 'alt-file) buffer-file-name)
					    (confirm-nonexistent-file-or-buffer) initial))))

      ;; Choose the file name: either the text typed in, or the head
      ;; of the list of matches

      (cond
       ((eq ido-exit 'fallback)
	;; Need to guard setting of default-directory here, since
	;; we don't want to change directory of current buffer.
	(let ((default-directory ido-current-directory)
	      (read-file-name-function nil))
	  (setq this-command (or fallback 'find-file))
	  (run-hook-with-args 'ido-before-fallback-functions this-command)
	  (call-interactively this-command)))

       ((eq ido-exit 'switch-to-buffer)
	(ido-buffer-internal
	 (if (memq method '(other-window other-frame)) method ido-default-buffer-method)
	 nil nil nil ido-text))

       ((eq ido-exit 'insert-buffer)
	(ido-buffer-internal 'insert 'insert-buffer "Insert buffer: " nil ido-text 'ido-enter-insert-file))

       ((eq ido-exit 'dired)
	(dired (concat ido-current-directory (or ido-text ""))))

       ((eq ido-exit 'ffap)
	(find-file-at-point))

       ((eq method 'alt-file)
	(ido-record-work-file filename)
	(setq default-directory ido-current-directory)
	(ido-record-work-directory)
	(find-alternate-file filename))

       ((memq method '(dired list-directory))
	(if (equal filename ".")
	    (setq filename ""))
	(let* ((dirname (ido-final-slash (concat ido-current-directory filename) t))
	       (file (substring dirname 0 -1)))
	  (cond
	   ((file-directory-p dirname)
	    (ido-record-command method dirname)
	    (ido-record-work-directory dirname)
	    (funcall method dirname))
	   ((file-directory-p ido-current-directory)
	    (cond
	     ((file-exists-p file)
	      (ido-record-command method ido-current-directory)
	      (ido-record-work-directory)
	      (funcall method ido-current-directory)
	      (if (eq method 'dired)
		  (with-no-warnings
		    (dired-goto-file (expand-file-name file)))))
	     ((string-match "[[*?]" filename)
	      (setq dirname (concat ido-current-directory filename))
	      (ido-record-command method dirname)
	      (ido-record-work-directory)
	      (funcall method dirname))
	     ((y-or-n-p (format "Directory %s does not exist.  Create it? " filename))
	      (ido-record-command method dirname)
	      (ido-record-work-directory dirname)
	      (make-directory-internal dirname)
	      (funcall method dirname))
	     (t
	      ;; put make-directory command on history
	      (ido-record-command 'make-directory dirname))))
	   (t (error "No such directory")))))

       ((eq method 'write)
	(ido-record-work-file filename)
	(setq default-directory ido-current-directory)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command 'write-file filename)
	(add-to-history 'file-name-history filename)
	(ido-record-work-directory)
	(write-file filename t))

       ((eq method 'read-only)
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command fallback filename)
	(ido-record-work-directory)
	(run-hook-with-args 'ido-before-fallback-functions fallback)
	(funcall fallback filename))

       ((eq method 'insert)
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command
	 (if ido-find-literal 'insert-file-literally 'insert-file)
	 filename)
	(add-to-history 'file-name-history filename)
	(ido-record-work-directory)
	(insert-file-1 filename
		       (if ido-find-literal
			   #'insert-file-contents-literally
			 #'insert-file-contents)))

       (filename
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command 'find-file filename)
	(add-to-history 'file-name-history filename)
	(ido-record-work-directory)
	(ido-visit-buffer (find-file-noselect filename nil ido-find-literal) method))))))

(defun ido-existing-item-p ()
  ;; Return non-nil if there is a matching item
  (not (null ido-matches)))

;;; COMPLETION CODE

(defun ido-set-common-completion  ()
  ;; Find common completion of `ido-text' in `ido-matches'
  ;; The result is stored in `ido-common-match-string'
  (let (val)
    (setq ido-common-match-string nil)
    (if (and ido-matches
	     (not ido-enable-regexp) ;; testing
             (stringp ido-text)
             (> (length ido-text) 0))
        (if (setq val (ido-find-common-substring ido-matches ido-text))
            (setq ido-common-match-string val)))
    val))

(defun ido-complete ()
  "Try and complete the current pattern amongst the file names."
  (interactive)
  (let (res)
    (cond
     (ido-incomplete-regexp
      ;; Do nothing
      )
     ((and (memq ido-cur-item '(file dir))
	   (string-match "[$]" ido-text))
      (let ((evar (substitute-in-file-name (concat ido-current-directory ido-text))))
	(if (not (file-exists-p (file-name-directory evar)))
	    (message "Expansion generates non-existing directory name")
	  (if (file-directory-p evar)
	      (ido-set-current-directory evar)
	    (let ((d (or (file-name-directory evar) "/"))
		  (f (file-name-nondirectory evar)))
	      (when (file-directory-p d)
		  (ido-set-current-directory d)
		  (setq ido-text-init f))))
	  (setq ido-exit 'refresh)
	  (exit-minibuffer))))

     (ido-directory-too-big
      (setq ido-directory-too-big nil)
      (setq ido-text-init ido-text)
      (setq ido-exit 'refresh)
      (exit-minibuffer))

     ((not ido-matches)
      (when ido-completion-buffer
	(call-interactively (setq this-command ido-cannot-complete-command))))

     ((and (= 1 (length ido-matches))
	   (not (and ido-enable-tramp-completion
		     (string-equal ido-current-directory "/")
		     (string-match ".[@:]\\'" (ido-name (car ido-matches)))))
		     (not (ido-local-file-exists-p (ido-name (car ido-matches)))))
      ;; only one choice, so select it.
      (if (not ido-confirm-unique-completion)
	  (exit-minibuffer)
	(setq ido-rescan (not ido-enable-prefix))
	(delete-region (minibuffer-prompt-end) (point))
	(insert (ido-name (car ido-matches)))))

     (t ;; else there could be some completions
      (setq res ido-common-match-string)
      (if (and (not (memq res '(t nil)))
	       (not (equal res ido-text)))
	  ;; found something to complete, so put it in the minibuffer.
	  (progn
	    ;; move exact match to front if not in prefix mode
	    (setq ido-rescan (not ido-enable-prefix))
	    (delete-region (minibuffer-prompt-end) (point))
	    (insert res))
	;; else nothing to complete
	(call-interactively (setq this-command ido-cannot-complete-command))
	)))))

(defun ido-complete-space ()
  "Try completion unless inserting the space makes sense."
  (interactive)
  (if (and (stringp ido-common-match-string)
	   (stringp ido-text)
	   (cond
	    ((> (length ido-common-match-string) (length ido-text))
	     (= (aref ido-common-match-string (length ido-text)) ? ))
	    (ido-matches
	     (let (insert-space
		   (re (concat (regexp-quote ido-text) " "))
		   (comp ido-matches))
	       (while comp
		 (if (string-match re (ido-name (car comp)))
		     (setq comp nil insert-space t)
		   (setq comp (cdr comp))))
	       insert-space))
	    (t nil)))
      (insert " ")
    (ido-complete)))

(defun ido-undo-merge-work-directory (&optional text try refresh)
  "Undo or redo last ido directory merge operation.
If no merge has yet taken place, toggle automatic merging option."
  (interactive)
  (cond
   (ido-pre-merge-state
    (ido-set-current-directory (nth 1 ido-pre-merge-state))
    (setq ido-text-init (or text (car ido-pre-merge-state))
	  ido-cur-list (nth 2 ido-pre-merge-state)
	  ido-ignored-list (nth 3 ido-pre-merge-state)
	  ido-matches (nth 4 ido-pre-merge-state)
	  ido-use-merged-list nil
	  ido-try-merged-list try
	  ido-keep-item-list (not refresh)
	  ido-rescan nil
	  ido-exit 'refresh
	  ido-pre-merge-state nil)
    (exit-minibuffer))
   (text
    nil)
   (ido-try-merged-list
    (setq ido-try-merged-list nil))
   (ido-matches
    (setq ido-try-merged-list t))
   ((not ido-use-merged-list)
    (ido-merge-work-directories))))

;;; Magic C-f

(defun ido-magic-forward-char (arg)
  "Move forward in user input or perform magic action.
If no user input is present, or at end of input, perform magic actions:
C-x C-b ... C-f  switch to `ido-find-file'.
C-x C-f ... C-f  fallback to non-ido `find-file'.
C-x C-d ... C-f  fallback to non-ido brief `dired'.
C-x d ... C-f    fallback to non-ido `dired'."
  (interactive "P")
  (cond
   ((or arg (not (eobp)))
    (forward-char (min (prefix-numeric-value arg)
		       (- (point-max) (point)))))
   ((memq ido-cur-item '(file dir))
    (ido-fallback-command))
   (ido-context-switch-command
    (call-interactively ido-context-switch-command))
   ((eq ido-cur-item 'buffer)
    (ido-enter-find-file))))

;;; Magic C-b

(defun ido-magic-backward-char (arg)
  "Move backward in user input or perform magic action.
If no user input is present, or at start of input, perform magic actions:
C-x C-f C-b  switch to `ido-switch-buffer'.
C-x C-d C-b  switch to `ido-switch-buffer'.
C-x d C-b    switch to `ido-switch-buffer'.
C-x C-b C-b  fallback to non-ido `switch-to-buffer'."
  (interactive "P")
  (cond
   ((or arg (> (point) (minibuffer-prompt-end)))
    (forward-char
     (- (min (prefix-numeric-value arg)
	     (- (point) (minibuffer-prompt-end))))))
   ((eq last-command this-command)
    (when (and (memq ido-cur-item '(file dir))
	       (not (bobp)))
      (ido-push-dir))) ; else do nothing
   ((eq ido-cur-item 'buffer)
    (ido-fallback-command))
   (ido-context-switch-command
    (call-interactively ido-context-switch-command))
   (t
    (ido-enter-switch-buffer))))

;;; Magic C-d

(defun ido-magic-delete-char (arg)
  "Delete following char in user input or perform magic action.
If at end of user input, perform magic actions:
C-x C-f ... C-d  enter `dired' on current directory."
  (interactive "P")
  (cond
   ((or arg (not (eobp)))
    (delete-char (min (prefix-numeric-value arg)
		       (- (point-max) (point)))))
   (ido-context-switch-command
    nil)
   ((memq ido-cur-item '(file dir))
    (ido-enter-dired))))


;;; TOGGLE FUNCTIONS

(defun ido-toggle-case ()
  "Toggle the value of `ido-case-fold'."
  (interactive)
  (setq ido-case-fold (not ido-case-fold))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-regexp ()
  "Toggle the value of `ido-enable-regexp'."
  (interactive)
  (setq ido-enable-regexp (not ido-enable-regexp))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-prefix ()
  "Toggle the value of `ido-enable-prefix'."
  (interactive)
  (setq ido-enable-prefix (not ido-enable-prefix))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-ignore ()
  "Toggle ignoring files specified with `ido-ignore-files'."
  (interactive)
  (if (and (not (eobp)) (> (point) (minibuffer-prompt-end)))
      (goto-char (minibuffer-prompt-end))
    (if ido-directory-too-big
	(progn
	  (message "Reading directory...")
	  (setq ido-directory-too-big nil))
      (setq ido-process-ignore-lists (not ido-process-ignore-lists)))
    (setq ido-text-init ido-text)
    (setq ido-exit 'refresh)
    (exit-minibuffer)))

(defun ido-toggle-vc ()
  "Disable version control for this file."
  (interactive)
  (if (and ido-mode (eq ido-cur-item 'file))
      (progn
	(setq vc-handled-backends
	      (if vc-handled-backends nil ido-saved-vc-hb))
	(setq ido-text-init ido-text)
	(setq ido-exit 'keep)
	(exit-minibuffer))))

(defun ido-toggle-literal ()
  "Toggle literal reading of this file."
  (interactive)
  (if (and ido-mode (eq ido-cur-item 'file))
      (progn
	(setq ido-find-literal (not ido-find-literal))
	(setq ido-text-init ido-text)
	(setq ido-exit 'keep)
	(exit-minibuffer))))

(defun ido-toggle-virtual-buffers ()
  "Toggle the use of virtual buffers.
See `ido-use-virtual-buffers' for explanation of virtual buffer."
  (interactive)
  (when (and ido-mode (eq ido-cur-item 'buffer))
    (setq ido-use-virtual-buffers (not ido-use-virtual-buffers))
    (setq ido-text-init ido-text)
    (setq ido-exit 'refresh)
    (exit-minibuffer)))

(defun ido-reread-directory ()
  "Read current directory again.
May be useful if cached version is no longer valid, but directory
timestamp has not changed (e.g. with ftp or on Windows)."
  (interactive)
  (if (and ido-mode (memq ido-cur-item '(file dir)))
      (progn
	(if (ido-is-unc-root)
	    (setq ido-unc-hosts-cache t)
	  (ido-remove-cached-dir ido-current-directory))
	(setq ido-text-init ido-text)
	(setq ido-rotate-temp t)
	(setq ido-exit 'refresh)
	(exit-minibuffer))))

(defun ido-exit-minibuffer ()
  "Exit minibuffer, but make sure we have a match if one is needed."
  (interactive)
  (if (and (or (not ido-require-match)
	       (if (memq ido-require-match '(confirm confirm-after-completion))
		   (if (or (eq ido-cur-item 'dir)
			   (eq last-command this-command))
		       t
		     (setq ido-show-confirm-message t)
		     nil))
               (ido-existing-item-p))
           (not ido-incomplete-regexp))
      (exit-minibuffer)))

(defun ido-select-text ()
  "Select the buffer or file named by the prompt.
If no buffer or file exactly matching the prompt exists, maybe create a new one."
  (interactive)
  (setq ido-exit 'takeprompt)
  (exit-minibuffer))

(defun ido-fallback-command ()
  "Fallback to non-ido version of current command."
  (interactive)
  (let ((i (length ido-text)))
    (while (> i 0)
      (push (aref ido-text (setq i (1- i))) unread-command-events)))
  (setq ido-exit 'fallback)
  (exit-minibuffer))

(defun ido-enter-find-file ()
  "Drop into `find-file' from buffer switching."
  (interactive)
  (setq ido-exit 'find-file)
  (exit-minibuffer))

(defun ido-enter-switch-buffer ()
  "Drop into `ido-switch-buffer' from file switching."
  (interactive)
  (setq ido-exit 'switch-to-buffer)
  (exit-minibuffer))

(defun ido-enter-dired ()
  "Drop into `dired' from file switching."
  (interactive)
  (setq ido-exit 'dired)
  (exit-minibuffer))

(defun ido-enter-insert-buffer ()
  "Drop into `insert-buffer' from insert file."
  (interactive)
  (setq ido-exit 'insert-buffer)
  (exit-minibuffer))

(defun ido-enter-insert-file ()
  "Drop into `insert-file' from insert buffer."
  (interactive)
  (setq ido-exit 'insert-file)
  (exit-minibuffer))


(defun ido-up-directory (&optional clear)
  "Go up one directory level."
  (interactive "P")
  (setq ido-text-init (if clear nil ido-text))
  (setq ido-exit 'updir)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

(defun ido-delete-backward-updir (count)
  "Delete char backwards, or at beginning of buffer, go up one level."
  (interactive "P")
  (cond
   ((= (minibuffer-prompt-end) (point))
    (if (not count)
	(ido-up-directory t)))
   ((and ido-pre-merge-state (string-equal (car ido-pre-merge-state) ido-text))
    (ido-undo-merge-work-directory (substring ido-text 0 -1) t t))
   ((eq this-original-command 'viper-backward-char)
    (funcall this-original-command (prefix-numeric-value count)))
   ((eq this-original-command 'viper-del-backward-char-in-insert)
    (funcall this-original-command))
   (t
    (delete-char (- (prefix-numeric-value count))))))

(defun ido-delete-backward-word-updir (count)
  "Delete all chars backwards, or at beginning of buffer, go up one level."
  (interactive "P")
  (if (= (minibuffer-prompt-end) (point))
      (if (not count)
	  (ido-up-directory t))
    (if (eq this-original-command 'viper-delete-backward-word)
	(funcall this-original-command (prefix-numeric-value count))
      (backward-kill-word (prefix-numeric-value count)))))

(defun ido-get-work-directory (&optional incr must-match)
  (let ((n (length ido-work-directory-list))
	(i ido-work-directory-index)
	(j 0)
	dir)
    (if (or (not ido-text) (= (length ido-text) 0))
	(setq must-match nil))
    (while (< j n)
      (setq i (+ i incr)
	    j (1+ j))
      (if (> incr 0)
	  (if (>= i n) (setq i 0))
	(if (< i 0) (setq i (1- n))))
      (setq dir (nth i ido-work-directory-list))
      (if (and dir
	       (not (equal dir ido-current-directory))
	       (file-directory-p dir)
	       (or (not must-match)
		   ;; TODO. check for nonreadable and too-big.
		   (ido-set-matches-1
		    (if (eq ido-cur-item 'file)
			(ido-make-file-list-1 dir)
		      (ido-make-dir-list-1 dir)))))
	  (setq j n)
	(setq dir nil)))
    (if dir
	(setq ido-work-directory-index i))
    dir))

(defun ido-prev-work-directory ()
  "Change to next working directory in list."
  (interactive)
  (let ((dir (ido-get-work-directory 1 ido-work-directory-match-only)))
    (when dir
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (setq ido-text-init ido-text)
      (setq ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-next-work-directory ()
  "Change to previous working directory in list."
  (interactive)
  (let ((dir (ido-get-work-directory -1 ido-work-directory-match-only)))
    (when dir
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (setq ido-text-init ido-text)
      (setq ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-merge-work-directories ()
  "Search (and merge) work directories for files matching the current input string."
  (interactive)
  (setq ido-use-merged-list t ido-try-merged-list t)
  (setq ido-exit 'refresh)
  (setq ido-text-init ido-text)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

(defun ido-wide-find-file (&optional file)
  "Prompt for FILE to search for using find, starting from current directory."
  (interactive)
  (unless file
    (let ((enable-recursive-minibuffers t))
      (setq file
	    (condition-case nil
		(read-string (concat "Wide find file: " ido-current-directory) ido-text)
	      (quit "")))))
  (when (> (length file) 0)
    (setq ido-use-merged-list t ido-try-merged-list 'wide)
    (setq ido-exit 'refresh)
    (setq ido-text-init file)
    (setq ido-rotate-temp t)
    (exit-minibuffer)))

(defun ido-wide-find-dir (&optional dir)
  "Prompt for DIR to search for using find, starting from current directory."
  (interactive)
  (unless dir
    (let ((enable-recursive-minibuffers t))
      (setq dir
	    (condition-case nil
		(read-string (concat "Wide find directory: " ido-current-directory) ido-text)
	      (quit "")))))
  (when (> (length dir) 0)
    (setq ido-use-merged-list t ido-try-merged-list 'wide)
    (setq ido-exit 'refresh)
    (setq ido-text-init (ido-final-slash dir t))
    (setq ido-rotate-temp t)
    (exit-minibuffer)))

(defun ido-wide-find-dir-or-delete-dir (&optional _dir)
  "Prompt for DIR to search for using find, starting from current directory.
If input stack is non-empty, delete current directory component."
  (interactive)
  (if ido-input-stack
      (ido-delete-backward-word-updir 1)
    (ido-wide-find-dir)))

(defun ido-take-first-match ()
  "Use first matching item as input text."
  (interactive)
  (when ido-matches
    (setq ido-text-init (ido-name (car ido-matches)))
    (setq ido-exit 'refresh)
    (exit-minibuffer)))

(defun ido-push-dir ()
  "Move to previous directory in file name, push current input on stack."
  (interactive)
  (setq ido-exit 'push)
  (exit-minibuffer))

(defun ido-push-dir-first ()
  "Move to previous directory in file name, push first match on stack."
  (interactive)
  (if ido-matches
      (setq ido-text (ido-name (car ido-matches))))
  (setq ido-exit 'push)
  (exit-minibuffer))

(defun ido-pop-dir (arg)
  "Pop directory from input stack back to input.
With \\[universal-argument], pop all elements."
  (interactive "P")
  (when ido-input-stack
    (setq ido-exit (if arg 'pop-all 'pop))
    (exit-minibuffer)))

(defun ido-wide-find-file-or-pop-dir (arg)
  (interactive "P")
  (if ido-input-stack
      (ido-pop-dir arg)
    (ido-wide-find-file)))

(defun ido-make-directory (&optional dir)
  "Prompt for DIR to create in current directory."
  (interactive)
  (unless dir
    (let ((enable-recursive-minibuffers t))
      (setq dir
	    (read-string (concat "Make directory: " ido-current-directory) ido-text))))
  (when (> (length dir) 0)
    (setq dir (concat ido-current-directory dir))
    (unless (file-exists-p dir)
      (make-directory dir t)
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (setq ido-text-init nil)
      (setq ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-get-work-file (incr)
  (let ((n (length ido-work-file-list))
	(i (+ ido-work-file-index incr))
	name)
    (if (> incr 0)
	(if (>= i n) (setq i 0))
      (if (< i 0) (setq i (1- n))))
    (setq name (nth i ido-work-file-list))
    (setq ido-work-file-index i)
    name))

(defun ido-prev-work-file ()
  "Change to next working file name in list."
  (interactive)
  (let ((name (ido-get-work-file 1)))
    (when name
      (setq ido-text-init name)
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(defun ido-next-work-file ()
  "Change to previous working file name in list."
  (interactive)
  (let ((name (ido-get-work-file -1)))
    (when name
      (setq ido-text-init name)
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(defun ido-copy-current-file-name (all)
  "Insert file name of current buffer.
If repeated, insert text from buffer instead."
  (interactive "P")
  (let* ((bfname (or (buffer-file-name ido-entry-buffer)
		     (buffer-name ido-entry-buffer)))
	 (name (and bfname (file-name-nondirectory bfname))))
    (when name
      (setq ido-text-init
	    (if (or all
		    (eq last-command this-command)
		    (not (equal (file-name-directory bfname) ido-current-directory))
		    (not (string-match "\\.[^.]*\\'" name)))
		name
	    (substring name 0 (1+ (match-beginning 0)))))
      (setq ido-exit 'refresh
	    ido-try-merged-list nil)
      (exit-minibuffer))))

(defun ido-copy-current-word (_all)
  "Insert current word (file or directory name) from current buffer."
  (interactive "P")
  (let ((word (with-current-buffer ido-entry-buffer
		(let ((p (point)) start-line end-line start-name)
		  (if (and mark-active (/= p (mark)))
		      (setq start-name (mark))
		    (beginning-of-line)
		    (setq start-line (point))
		    (end-of-line)
		    (setq end-line (point))
		    (goto-char p)
		    (if (re-search-backward "[^-_a-zA-Z0-9:./\\~@]" start-line 1)
			(forward-char 1))
		    (setq start-name (point))
		    (re-search-forward "[-_a-zA-Z0-9:./\\~@]*" end-line 1)
		    (if (= start-name (point))
			(setq start-name nil)))
		  (and start-name
		       (buffer-substring-no-properties start-name (point)))))))
    (if (cond
	 ((not word) nil)
	 ((string-match "\\`[~/]" word)
	  (setq ido-text-init word
		ido-try-merged-list nil
		ido-exit 'chdir))
	 ((string-match "/" word)
	  (setq ido-text-init (concat ido-current-directory word)
		ido-try-merged-list nil
		ido-exit 'chdir))
	 (t
	  (setq ido-text-init word
		ido-try-merged-list nil
		ido-exit 'refresh)))
	(exit-minibuffer))))

(defun ido-next-match ()
  "Put first element of `ido-matches' at the end of the list."
  (interactive)
  (if ido-matches
      (let ((next (cadr ido-matches)))
	(setq ido-cur-list (ido-chop ido-cur-list next))
	(setq ido-matches (ido-chop ido-matches next))
	(setq ido-rescan nil))))

(defun ido-prev-match ()
  "Put last element of `ido-matches' at the front of the list."
  (interactive)
  (if ido-matches
      (let ((prev (car (last ido-matches))))
	(setq ido-cur-list (ido-chop ido-cur-list prev))
	(setq ido-matches (ido-chop ido-matches prev))
	(setq ido-rescan nil))))

(defun ido-next-match-dir ()
  "Find next directory in match list.
If work directories have been merged, cycle through directories for
first matching file."
  (interactive)
  (if ido-use-merged-list
      (if ido-matches
	  (let* ((elt (car ido-matches))
		 (dirs (cdr elt)))
	    (when (> (length dirs) 1)
	      (setcdr elt (ido-chop dirs (cadr dirs))))
	    (setq ido-rescan nil)))
    (let ((cnt (length ido-matches))
	  (i 1))
      (while (and (< i cnt) (not (ido-final-slash (nth i ido-matches))))
	(setq i (1+ i)))
      (if (< i cnt)
	  (setq ido-cur-list (ido-chop ido-cur-list (nth i ido-matches)))))))

(defun ido-prev-match-dir ()
  "Find previous directory in match list.
If work directories have been merged, cycle through directories
for first matching file."
  (interactive)
  (if ido-use-merged-list
      (if ido-matches
	  (let* ((elt (car ido-matches))
		 (dirs (cdr elt)))
	    (when (> (length dirs) 1)
	      (setcdr elt (ido-chop dirs (car (last dirs)))))
	    (setq ido-rescan nil)))
    (let* ((cnt (length ido-matches))
	   (i (1- cnt)))
      (while (and (> i 0) (not (ido-final-slash (nth i ido-matches))))
	(setq i (1- i)))
      (if (> i 0)
	  (setq ido-cur-list (ido-chop ido-cur-list (nth i ido-matches)))))))

(defun ido-restrict-to-matches ()
  "Set current item list to the currently matched items."
  (interactive)
  (when ido-matches
    (setq ido-cur-list ido-matches
	  ido-text-init ""
	  ido-rescan nil
	  ido-exit 'keep)
    (exit-minibuffer)))

(defun ido-chop (items elem)
  "Remove all elements before ELEM and put them at the end of ITEMS."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car items))
      (if (equal next elem)
	  (setq ret (append items (nreverse sofar)))
	;; else
	(progn
	  (setq items (cdr items))
	  (setq sofar (cons next sofar)))))
    ret))

(defun ido-name (item)
  ;; Return file name for current item, whether in a normal list
  ;; or a merged work directory list.
  (if (consp item) (car item) item))


;;; CREATE LIST OF ALL CURRENT FILES

(defun ido-all-completions ()
  ;; Return unsorted list of all completions.
  (let ((ido-process-ignore-lists nil)
	(ido-directory-too-big nil))
    (cond
     ((eq ido-cur-item 'file)
      (ido-make-file-list-1 ido-current-directory))
     ((eq ido-cur-item 'dir)
      (ido-make-dir-list-1 ido-current-directory))
     ((eq ido-cur-item 'buffer)
      (ido-make-buffer-list-1))
     ((eq ido-cur-item 'list)
      ido-choice-list)
     (t nil))))


;; File list sorting

(defun ido-file-lessp (a b)
  ;; Simple compare two file names.
  (string-lessp (ido-no-final-slash a) (ido-no-final-slash b)))


(defun ido-file-extension-lessp (a b)
  ;; Compare file names according to ido-file-extensions-order list.
  (let ((n (compare-strings a 0 nil b 0 nil nil))
	lessp p)
    (if (eq n t)
	nil
      (if (< n 0)
	  (setq n (1- (- n))
		p a a b b p
		lessp t)
	(setq n (1- n)))
      (cond
       ((= n 0)
	lessp)
       ((= (aref a n) ?.)
	(ido-file-extension-aux a b n lessp))
       (t
	(while (and (> n 2) (/= (aref a n) ?.))
	  (setq n (1- n)))
	(if (> n 1)
	    (ido-file-extension-aux a b n lessp)
	  lessp))))))

(defun ido-file-extension-aux (a b n lessp)
  (let ((oa (ido-file-extension-order a n))
	(ob (ido-file-extension-order b n)))
    (cond
     ((and oa ob)
      (cond
       ((= oa ob)
	lessp)
       (lessp
	(> oa ob))
       (t
	(< oa ob))))
     (oa
      (not lessp))
     (ob
      lessp)
     (t
      lessp))))

(defun ido-file-extension-order (s n)
  (let ((l ido-file-extensions-order)
	(i 0) o do)
    (while l
      (cond
       ((eq (car l) t)
	(setq do i
	      l (cdr l)))
       ((eq (compare-strings s n nil (car l) 0 nil nil) t)
	(setq o i
	      l nil))
       (t
	(setq l (cdr l))))
      (setq i (1+ i)))
    (or o do)))


(defun ido-sort-merged-list (items promote)
  ;; Input is list of ("file" . "dir") cons cells.
  ;; Output is sorted list of ("file "dir" ...) lists
  (let ((l (sort items (lambda (a b) (string-lessp (car b) (car a)))))
	res a cur)
    (while l
      (setq a (car l)
	    l (cdr l))
      (if (and res (string-equal (car (car res)) (car a)))
	  (progn
	    (setcdr (car (if cur (cdr res) res)) (cons (cdr a) (cdr (car res))))
	    (if (and promote (string-equal ido-current-directory (cdr a)))
		(setq cur t)))
	(setq res (cons (list (car a) (cdr a)) res)
	      cur nil)))
    res))

(defun ido-wide-find-dirs-or-files (dir file &optional prefix finddir)
  ;; As ido-run-find-command, but returns a list of cons pairs ("file" . "dir")
  (let ((filenames
	 (split-string
	  (shell-command-to-string
	   (concat "find "
		   (shell-quote-argument dir)
		   " -name "
		   (shell-quote-argument
		    (concat (if prefix "" "*") file "*"))
		   " -type " (if finddir "d" "f") " -print"))))
	filename d f
	res)
    (while filenames
      (setq filename (car filenames)
	    filenames (cdr filenames))
      (if (and (file-name-absolute-p filename)
	       (file-exists-p filename))
	  (setq d (file-name-directory filename)
		f (file-name-nondirectory filename)
		res (cons (cons (if finddir (ido-final-slash f t) f) d) res))))
    res))

(defun ido-flatten-merged-list (items)
  ;; Create a list of directory names based on a merged directory list.
  (let (res)
    (while items
      (let* ((item (car items))
	     (file (car item))
	     (dirs (cdr item)))
	(while dirs
	  (setq res (cons (concat (car dirs) file) res)
		dirs (cdr dirs))))
      (setq items (cdr items)))
    res))


(defun ido-make-merged-file-list-1 (text auto wide)
  (let (res)
    (if (and (ido-final-slash text) ido-dir-file-cache)
	(if wide
	    (setq res (ido-wide-find-dirs-or-files
		       ido-current-directory (substring text 0 -1) ido-enable-prefix t))
	  ;; Use list of cached directories
	  (let ((re (concat (regexp-quote (substring text 0 -1)) "[^/:]*/\\'"))
		(dirs ido-dir-file-cache)
		dir b d f)
	    (if nil ;; simple
		(while dirs
		  (setq dir (car (car dirs))
			dirs (cdr dirs))
		  (when (and (string-match re dir)
			     (not (ido-ignore-item-p dir ido-ignore-directories-merge))
			     (file-directory-p dir))
		    (setq b (substring dir 0 -1)
			  f (concat (file-name-nondirectory b) "/")
			  d (file-name-directory b)
			  res (cons (cons f d) res))))
	      (while dirs
		(setq dir (car dirs)
		      d (car dir)
		      dirs (cdr dirs))
		(when (not (ido-ignore-item-p d ido-ignore-directories-merge))
		  (setq dir (cdr (cdr dir)))
		  (while dir
		    (setq f (car dir)
			  dir (cdr dir))
		    (if (and (string-match re f)
			     (not (ido-ignore-item-p f ido-ignore-directories)))
			(setq res (cons (cons f d) res)))))
		(if (and auto (input-pending-p))
		    (setq dirs nil
			  res t))))))
      (if wide
	  (setq res (ido-wide-find-dirs-or-files
		     ido-current-directory text ido-enable-prefix nil))
	(let ((ido-text text)
	      (dirs ido-work-directory-list)
	      (must-match (and text (> (length text) 0)))
	      dir fl)
	  (if (and auto (not (member ido-current-directory dirs)))
	      (setq dirs (cons ido-current-directory dirs)))
	  (while dirs
	    (setq dir (car dirs)
		  dirs (cdr dirs))
	    (when (and dir (stringp dir)
		       (or ido-merge-ftp-work-directories
			   (not (ido-is-ftp-directory dir)))
		       (file-directory-p dir)
		       ;; TODO. check for nonreadable and too-big.
		       (setq fl (if (eq ido-cur-item 'file)
				    (ido-make-file-list-1 dir t)
				  (ido-make-dir-list-1 dir t))))
	      (if must-match
		  (setq fl (ido-set-matches-1 fl)))
	      (if fl
		  (setq res (nconc fl res))))
	    (if (and auto (input-pending-p))
		(setq dirs nil
		      res t))))))
    res))

(defun ido-make-merged-file-list (text auto wide)
  (let (res)
    (message "Searching for `%s'...." text)
    (condition-case nil
	(if (eq t (setq res
			(while-no-input
			  (ido-make-merged-file-list-1 text auto wide))))
	    (setq res 'input-pending-p))
      (quit
       (setq res t
	     ido-try-merged-list nil
	     ido-use-merged-list nil)))
    (when (and res (listp res))
      (setq res (ido-sort-merged-list res auto)))
    (when (and (or ido-rotate-temp ido-rotate-file-list-default)
	       (listp res)
	       (> (length text) 0))
      (let ((elt (assoc text res)))
	(when (and elt (not (eq elt (car res))))
	  (setq res (delq elt res))
	  (setq res (cons elt res)))))
    (message nil)
    res))

(defun ido-make-buffer-list-1 (&optional frame visible)
  ;; Return list of non-ignored buffer names
  (delq nil
	(mapcar
	 (lambda (x)
	   (let ((name (buffer-name x)))
	     (if (not (or (ido-ignore-item-p name ido-ignore-buffers) (member name visible)))
		 name)))
	 (buffer-list frame))))

(defun ido-make-buffer-list (default)
  ;; Return the current list of buffers.
  ;; Currently visible buffers are put at the end of the list.
  ;; The hook `ido-make-buffer-list-hook' is run after the list has been
  ;; created to allow the user to further modify the order of the buffer names
  ;; in this list.  If DEFAULT is non-nil, and corresponds to an existing buffer,
  ;; it is put to the start of the list.
  (let* ((ido-current-buffers (ido-get-buffers-in-frames 'current))
	 (ido-temp-list (ido-make-buffer-list-1 (selected-frame) ido-current-buffers)))
    (if ido-temp-list
	(nconc ido-temp-list ido-current-buffers)
      (setq ido-temp-list ido-current-buffers))
    (if default
        (setq ido-temp-list
              (cons default (delete default ido-temp-list))))
    (if ido-use-virtual-buffers
	(ido-add-virtual-buffers-to-list))
    (run-hooks 'ido-make-buffer-list-hook)
    ido-temp-list))

(defun ido-add-virtual-buffers-to-list ()
  "Add recently visited files, and bookmark files, to the buffer list.
This is to make them appear as if they were \"virtual buffers\"."
  ;; If no buffers matched, and virtual buffers are being used, then
  ;; consult the list of past visited files, to see if we can find
  ;; the file which the user might thought was still open.
  (unless recentf-mode (recentf-mode 1))
  (setq ido-virtual-buffers nil)
  (let (name)
    (dolist (head recentf-list)
      (and (setq name (file-name-nondirectory head))
           (null (get-file-buffer head))
           (not (assoc name ido-virtual-buffers))
           (not (member name ido-temp-list))
           (not (ido-ignore-item-p name ido-ignore-buffers))
           ;;(file-exists-p head)
           (push (cons name head) ido-virtual-buffers))))
  (when ido-virtual-buffers
    (if ido-use-faces
	(dolist (comp ido-virtual-buffers)
	  (put-text-property 0 (length (car comp))
			     'face 'ido-virtual
			     (car comp))))
    (setq ido-temp-list
	  (nconc ido-temp-list
		 (nreverse (mapcar #'car ido-virtual-buffers))))))

(defun ido-make-choice-list (default)
  ;; Return the current list of choices.
  ;; If DEFAULT is non-nil, and corresponds to an element of choices,
  ;; it is put to the start of the list.
  (let ((ido-temp-list ido-choice-list))
    (if default
	(progn
	  (setq ido-temp-list
		(delete default ido-temp-list))
	  (setq ido-temp-list
		(cons default ido-temp-list))))
    ; (run-hooks 'ido-make-choice-list-hook)
    ido-temp-list))

(defun ido-to-end (items)
  ;; Move the elements from ITEMS to the end of `ido-temp-list'
  (mapc
   (lambda (elem)
     (setq ido-temp-list (delq elem ido-temp-list)))
   items)
  (if ido-temp-list
      (nconc ido-temp-list items)
    (setq ido-temp-list items)))

(defun ido-file-name-all-completions-1 (dir)
  (cond
   ((ido-nonreadable-directory-p dir) '())
   ;; do not check (ido-directory-too-big-p dir) here.
   ;; Caller must have done that if necessary.

   ((and ido-enable-tramp-completion
	 (string-match "\\`/[^/]+[:@]\\'" dir))
    ;; Strip method:user@host: part of tramp completions.
    ;; Tramp completions do not include leading slash.
    (let* ((len (1- (length dir)))
	   (non-essential t)
	   (compl
	    (or (file-name-all-completions "" dir)
		;; work around bug in ange-ftp.
		;; /ftp:user@host: => nil
		;; /ftp:user@host:./ => ok
		(and
		 (not (string= "/ftp:" dir))
		 (file-remote-p dir)
		 ;; tramp-ftp-file-name-p is available only when tramp
		 ;; has been loaded.
		 (fboundp 'tramp-ftp-file-name-p)
		 (funcall 'tramp-ftp-file-name-p dir)
		 (string-match ":\\'" dir)
		 (file-name-all-completions "" (concat dir "./"))))))
      (if (and compl
	       (> (length (car compl)) len)
	       (string= (substring (car compl) 0 len) (substring dir 1)))
	  (mapcar (lambda (c) (substring c len)) compl)
	compl)))
   (t
    (file-name-all-completions "" dir))))

(defun ido-file-name-all-completions (dir)
  ;; Return name of all files in DIR
  ;; Uses and updates ido-dir-file-cache
  (cond
   ((ido-is-unc-root dir)
    (mapcar
     (lambda (host)
       (if (string-match "/\\'" host) host (concat host "/")))
     (ido-unc-hosts t)))
   ((and (numberp ido-max-dir-file-cache) (> ido-max-dir-file-cache 0)
	 (stringp dir) (> (length dir) 0)
	 (ido-may-cache-directory dir))
    (let* ((cached (assoc dir ido-dir-file-cache))
	     (ctime (nth 1 cached))
	     (ftp (ido-is-ftp-directory dir))
	     (unc (ido-is-unc-host dir))
	     (attr (if (or ftp unc) nil (file-attributes dir)))
	     (mtime (nth 5 attr))
	     valid)
	(when cached 	    ; should we use the cached entry ?
	  (cond
	   (ftp
	    (setq valid (and (eq (car ctime) 'ftp)
			     (ido-cache-ftp-valid (cdr ctime)))))
	   (unc
	    (setq valid (and (eq (car ctime) 'unc)
			     (ido-cache-unc-valid (cdr ctime)))))
	   (t
	    (if attr
		(setq valid (and (= (car ctime) (car mtime))
				 (= (car (cdr ctime)) (car (cdr mtime))))))))
	  (unless valid
	    (setq ido-dir-file-cache (delq cached ido-dir-file-cache)
		  cached nil)))
	(unless cached
	  (cond
	   (unc
	    (setq mtime (cons 'unc (ido-time-stamp))))
	   ((and ftp (file-readable-p dir))
	    (setq mtime (cons 'ftp (ido-time-stamp)))))
	  (if mtime
	      (setq cached (cons dir (cons mtime (ido-file-name-all-completions-1 dir)))
		    ido-dir-file-cache (cons cached ido-dir-file-cache)))
	  (if (> (length ido-dir-file-cache) ido-max-dir-file-cache)
	      (setcdr (nthcdr (1- ido-max-dir-file-cache) ido-dir-file-cache) nil)))
	(and cached
	     (cdr (cdr cached)))))
   (t
    (ido-file-name-all-completions-1 dir))))

(defun ido-remove-cached-dir (dir)
  ;; Remove dir from ido-dir-file-cache
  (if (and ido-dir-file-cache
	   (stringp dir) (> (length dir) 0))
      (let ((cached (assoc dir ido-dir-file-cache)))
	(if cached
	    (setq ido-dir-file-cache (delq cached ido-dir-file-cache))))))


(defun ido-make-file-list-1 (dir &optional merged)
  ;; Return list of non-ignored files in DIR
  ;; If MERGED is non-nil, each file is cons'ed with DIR
  (and (or (ido-is-tramp-root dir) (ido-is-unc-root dir)
	   (file-directory-p dir))
       (delq nil
	     (mapcar
	      (lambda (name)
		(if (not (ido-ignore-item-p name ido-ignore-files t))
		    (if merged (cons name dir) name)))
	      (ido-file-name-all-completions dir)))))

(defun ido-make-file-list (default)
  ;; Return the current list of files.
  ;; Currently visible files are put at the end of the list.
  ;; The hook `ido-make-file-list-hook' is run after the list has been
  ;; created to allow the user to further modify the order of the file names
  ;; in this list.
  (let ((ido-temp-list (ido-make-file-list-1 ido-current-directory)))
    (setq ido-temp-list (sort ido-temp-list
			      (if ido-file-extensions-order
				  #'ido-file-extension-lessp
				#'ido-file-lessp)))
    (unless (ido-is-tramp-root ido-current-directory)
      (let ((default-directory ido-current-directory))
	(ido-to-end ;; move ftp hosts and visited files to end
	 (delq nil (mapcar
		    (lambda (x) (if (or (and (string-match ".:\\'" x)
					     (not (ido-local-file-exists-p x)))
					(and (not (ido-final-slash x))
					     (let (file-name-handler-alist)
					       (get-file-buffer x)))) x))
		    ido-temp-list)))))
    (ido-to-end  ;; move . files to end
     (delq nil (mapcar
		(lambda (x) (if (string-equal (substring x 0 1) ".") x))
		ido-temp-list)))
    (if (and default (member default ido-temp-list))
	(if (or ido-rotate-temp ido-rotate-file-list-default)
	    (unless (equal default (car ido-temp-list))
	      (let ((l ido-temp-list) k)
		(while (and l (cdr l) (not (equal default (car (cdr l)))))
		  (setq l (cdr l)))
		(setq k (cdr l))
		(setcdr l nil)
		(nconc k ido-temp-list)
		(setq ido-temp-list k)))
	  (setq ido-temp-list
		(delete default ido-temp-list))
	  (setq ido-temp-list
		(cons default ido-temp-list))))
    (when ido-show-dot-for-dired
      (setq ido-temp-list (delete "." ido-temp-list))
      (setq ido-temp-list (cons "." ido-temp-list)))
    (run-hooks 'ido-make-file-list-hook)
    ido-temp-list))

(defun ido-make-dir-list-1 (dir &optional merged)
  ;; Return list of non-ignored subdirs in DIR
  ;; If MERGED is non-nil, each subdir is cons'ed with DIR
  (and (or (ido-is-tramp-root dir) (file-directory-p dir))
       (delq nil
	     (mapcar
	      (lambda (name)
		(and (ido-final-slash name) (not (ido-ignore-item-p name ido-ignore-directories))
		     (if merged (cons name dir) name)))
	      (ido-file-name-all-completions dir)))))

(defun ido-make-dir-list (default)
  ;; Return the current list of directories.
  ;; The hook `ido-make-dir-list-hook' is run after the list has been
  ;; created to allow the user to further modify the order of the
  ;; directory names in this list.
  (let ((ido-temp-list (ido-make-dir-list-1 ido-current-directory)))
    (setq ido-temp-list (sort ido-temp-list #'ido-file-lessp))
    (ido-to-end  ;; move . files to end
     (delq nil (mapcar
		(lambda (x) (if (string-equal (substring x 0 1) ".") x))
		ido-temp-list)))
    (if (and default (member default ido-temp-list))
	(if (or ido-rotate-temp ido-rotate-file-list-default)
	    (unless (equal default (car ido-temp-list))
	      (let ((l ido-temp-list) k)
		(while (and l (cdr l) (not (equal default (car (cdr l)))))
		  (setq l (cdr l)))
		(setq k (cdr l))
		(setcdr l nil)
		(nconc k ido-temp-list)
		(setq ido-temp-list k)))
	  (setq ido-temp-list
		(delete default ido-temp-list))
	  (setq ido-temp-list
		(cons default ido-temp-list))))
    (setq ido-temp-list (delete "." ido-temp-list))
    (unless ido-input-stack
      (setq ido-temp-list (cons "." ido-temp-list)))
    (run-hooks 'ido-make-dir-list-hook)
    ido-temp-list))

;; List of the files visible in the current frame.
(defvar ido-bufs-in-frame)

(defun ido-get-buffers-in-frames (&optional current)
  ;; Return the list of buffers that are visible in the current frame.
  ;; If optional argument `current' is given, restrict searching to the
  ;; current frame, rather than all frames, regardless of value of
  ;; `ido-all-frames'.
  (let ((ido-bufs-in-frame nil))
    (walk-windows 'ido-get-bufname nil
		  (if current
		      nil
		    ido-all-frames))
    ido-bufs-in-frame))

(defun ido-get-bufname (win)
  ;; Used by `ido-get-buffers-in-frames' to walk through all windows
  (let ((buf (buffer-name (window-buffer win))))
	(unless (or (member buf ido-bufs-in-frame)
		    (member buf ido-ignore-item-temp-list))
	  ;; Only add buf if it is not already in list.
	  ;; This prevents same buf in two different windows being
	  ;; put into the list twice.
	  (setq ido-bufs-in-frame
		(cons buf ido-bufs-in-frame)))))

;;; FIND MATCHING ITEMS

(defun ido-set-matches-1 (items &optional do-full)
  ;; Return list of matches in items
  (let* ((case-fold-search  ido-case-fold)
	 (slash (and (not ido-enable-prefix) (ido-final-slash ido-text)))
	 (text (if slash (substring ido-text 0 -1) ido-text))
	 (rex0 (if ido-enable-regexp text (regexp-quote text)))
	 (rexq (concat rex0 (if slash ".*/" "")))
	 (re (if ido-enable-prefix (concat "\\`" rexq) rexq))
	 (full-re (and do-full (not ido-enable-regexp) (not (string-match "\$\\'" rex0))
		       (concat "\\`" rex0 (if slash "/" "") "\\'")))
	 (suffix-re (and do-full slash
			 (not ido-enable-regexp) (not (string-match "\$\\'" rex0))
			 (concat rex0 "/\\'")))
	 (prefix-re (and full-re (not ido-enable-prefix)
			 (concat "\\`" rexq)))
	 (non-prefix-dot (or (not ido-enable-dot-prefix)
			     (not ido-process-ignore-lists)
			     ido-enable-prefix
			     (= (length ido-text) 0)))
	 full-matches suffix-matches prefix-matches matches)
    (setq ido-incomplete-regexp nil)
    (condition-case error
        (mapc
         (lambda (item)
           (let ((name (ido-name item)))
	     (if (and (or non-prefix-dot
			  (if (= (aref ido-text 0) ?.)
			      (= (aref name 0) ?.)
			    (/= (aref name 0) ?.)))
		      (string-match re name))
		 (cond
		  ((and (eq ido-cur-item 'buffer)
			(or (not (stringp ido-default-item))
			    (not (string= name ido-default-item)))
			(string= name (buffer-name ido-entry-buffer)))
		   (setq matches (cons item matches)))
		  ((and full-re (string-match full-re name))
		   (setq full-matches (cons item full-matches)))
		  ((and suffix-re (string-match suffix-re name))
		   (setq suffix-matches (cons item suffix-matches)))
		  ((and prefix-re (string-match prefix-re name))
		   (setq prefix-matches (cons item prefix-matches)))
		  (t (setq matches (cons item matches))))))
	   t)
         items)
      (invalid-regexp
       (setq ido-incomplete-regexp t
             ;; Consider the invalid regexp message internally as a
             ;; special-case single match, and handle appropriately
             ;; elsewhere.
             matches (cdr error))))
    (when prefix-matches
      (ido-trace "prefix match" prefix-matches)
      ;; Bug#2042.
      (setq matches (nconc prefix-matches matches)))
    (when suffix-matches
      (ido-trace "suffix match" (list text suffix-re suffix-matches))
      (setq matches (nconc suffix-matches matches)))
    (when full-matches
      (ido-trace "full match" (list text full-re full-matches))
      (setq matches (nconc full-matches matches)))
    (when (and (null matches)
	       ido-enable-flex-matching
	       (> (length ido-text) 1)
	       (not ido-enable-regexp))
      (setq re (mapconcat #'regexp-quote (split-string ido-text "") ".*"))
      (if ido-enable-prefix
	  (setq re (concat "\\`" re)))
      (mapc
       (lambda (item)
	 (let ((name (ido-name item)))
	   (if (string-match re name)
	       (setq matches (cons item matches)))))
       items))
    matches))


(defun ido-set-matches ()
  ;; Set `ido-matches' to the list of items matching prompt
  (when ido-rescan
    (setq ido-matches (ido-set-matches-1 (reverse ido-cur-list) (not ido-rotate))
	  ido-rotate nil)))

(defun ido-ignore-item-p (name re-list &optional ignore-ext)
  ;; Return t if the buffer or file NAME should be ignored.
  (or (member name ido-ignore-item-temp-list)
      (and
       ido-process-ignore-lists re-list
       (save-match-data
	 (let ((ext-list (and ignore-ext ido-ignore-extensions
			      completion-ignored-extensions))
	       (case-fold-search ido-case-fold)
	       ignorep nextstr
	       (flen (length name)) slen)
	   (while ext-list
	     (setq nextstr (car ext-list))
	     (if (cond
		  ((stringp nextstr)
		   (and (>= flen (setq slen (length nextstr)))
			(string-equal (substring name (- flen slen)) nextstr)))
		  ((functionp nextstr) (funcall nextstr name))
		  (t nil))
		 (setq ignorep t
		       ext-list nil
		       re-list nil)
	       (setq ext-list (cdr ext-list))))
	   (while re-list
	     (setq nextstr (car re-list))
	     (if (cond
		  ((stringp nextstr) (string-match nextstr name))
		  ((functionp nextstr) (funcall nextstr name))
		  (t nil))
		 (setq ignorep t
		       re-list nil)
	       (setq re-list (cdr re-list))))
	   ;; return the result
	   (if ignorep
	       (setq ido-ignored-list (cons name ido-ignored-list)))
	   ignorep)))))

;; Private variable used by `ido-word-matching-substring'.
(defvar ido-change-word-sub)

(defun ido-find-common-substring (items subs)
  ;; Return common string following SUBS in each element of ITEMS.
  (let (res
        alist
        ido-change-word-sub)
    (setq ido-change-word-sub
          (if ido-enable-regexp
              subs
            (regexp-quote subs)))
    (setq res (mapcar #'ido-word-matching-substring items))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar #'ido-makealist res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let* ((completion-ignore-case ido-case-fold)
	   (comp (try-completion subs alist)))
      (if (eq comp t)
	  subs
	comp))))

(defun ido-word-matching-substring (word)
  ;; Return part of WORD before 1st match to `ido-change-word-sub'.
  ;; If `ido-change-word-sub' cannot be found in WORD, return nil.
  (let ((case-fold-search ido-case-fold))
    (let ((m (string-match ido-change-word-sub (ido-name word))))
      (if m
          (substring (ido-name word) m)
        ;; else no match
        nil))))

(defun ido-makealist (res)
  ;; Return dotted pair (RES . 1).
  (cons res 1))

(defun ido-choose-completion-string (choice &rest ignored)
  (when (ido-active)
    ;; Insert the completion into the buffer where completion was requested.
    (and ido-completion-buffer
	 (get-buffer ido-completion-buffer)
	 (kill-buffer ido-completion-buffer))
    (cond
     ((ido-active t) ;; ido-use-merged-list
      (setq ido-current-directory ""
	    ido-text choice
	    ido-exit 'done))
     ((not (ido-final-slash choice))
      (setq ido-text choice
	    ido-exit 'done))
     (t
      (ido-set-current-directory ido-current-directory choice)
      (setq ido-exit 'refresh)))
    (exit-minibuffer)
    t))

(defun ido-completion-help ()
  "Show possible completions in a *File Completions* buffer."
  (interactive)
  (setq ido-rescan nil)
  (let ((temp-buf (and ido-completion-buffer
		       (get-buffer ido-completion-buffer)))
	display-it full-list)
    (if (and (eq last-command this-command) temp-buf)
	;; scroll buffer
	(let (win (buf (current-buffer)))
	  (display-buffer temp-buf nil nil)
	  (set-buffer temp-buf)
	  (setq win (get-buffer-window temp-buf))
	  (if (pos-visible-in-window-p (point-max) win)
	      (if (or ido-completion-buffer-all-completions
		      (boundp 'ido-completion-buffer-full))
		  (set-window-start win (point-min))
		(with-no-warnings
		  (set (make-local-variable 'ido-completion-buffer-full) t))
		(setq full-list t
		      display-it t))
	    (scroll-other-window))
	  (set-buffer buf))
      (setq display-it t))
    (if (and ido-completion-buffer display-it)
	(with-output-to-temp-buffer ido-completion-buffer
	  (let ((completion-list (sort
				  (cond
				   (ido-directory-too-big
				    (message "Reading directory...")
				    (setq ido-directory-too-big nil
					  ido-ignored-list nil
					  ido-cur-list (ido-all-completions)
					  ido-rescan t)
				    (ido-set-matches)
				    (or ido-matches ido-cur-list))
				   (ido-use-merged-list
				    (ido-flatten-merged-list (or ido-matches ido-cur-list)))
				   ((or full-list ido-completion-buffer-all-completions)
				    (ido-all-completions))
				   (t
				    (copy-sequence (or ido-matches ido-cur-list))))
				  #'ido-file-lessp)))
	    (if (featurep 'xemacs)
		;; XEmacs extents are put on by default, doesn't seem to be
		;; any way of switching them off.
		;; This obscure code avoids a byte compiler warning in Emacs.
		(let ((f 'display-completion-list))
		  (funcall f completion-list
			   :help-string "ido "
			   :activate-callback
			   (lambda (x y z) (message "Doesn't work yet, sorry!"))))
	      ;; else running Emacs
	      ;;(add-hook 'completion-setup-hook 'completion-setup-function)
	      (display-completion-list completion-list)))))))

;;; KILL CURRENT BUFFER
(defun ido-kill-buffer-at-head ()
  "Kill the buffer at the head of `ido-matches'.
If cursor is not at the end of the user input, delete to end of input."
  (interactive)
  (if (not (eobp))
      (delete-region (point) (line-end-position))
    (let ((enable-recursive-minibuffers t)
	  (buf (ido-name (car ido-matches)))
	  (nextbuf (cadr ido-matches)))
      (cond
       ((get-buffer buf)
	;; If next match names a buffer use the buffer object; buffer
	;; name may be changed by packages such as uniquify.
	(when (and nextbuf (get-buffer nextbuf))
	  (setq nextbuf (get-buffer nextbuf)))
	(if (null (kill-buffer buf))
	    ;; Buffer couldn't be killed.
	    (setq ido-rescan t)
	  ;; Else `kill-buffer' succeeds so re-make the buffer list
	  ;; taking into account packages like uniquify may rename
	  ;; buffers.
	  (if (bufferp nextbuf)
	      (setq nextbuf (buffer-name nextbuf)))
	  (setq ido-default-item nextbuf
		ido-text-init ido-text
		ido-exit 'refresh)
	  (exit-minibuffer)))
       ;; Handle virtual buffers
       ((assoc buf ido-virtual-buffers)
	(setq recentf-list
	      (delete (cdr (assoc buf ido-virtual-buffers)) recentf-list))
	(setq ido-cur-list (delete buf ido-cur-list))
	(setq ido-rescan t))))))

;;; DELETE CURRENT FILE
(defun ido-delete-file-at-head ()
  "Delete the file at the head of `ido-matches'.
If cursor is not at the end of the user input, delete to end of input."
  (interactive)
  (if (not (eobp))
      (delete-region (point) (line-end-position))
    (let ((enable-recursive-minibuffers t)
	  (file (ido-name (car ido-matches))))
      (if file
	  (setq file (concat ido-current-directory file)))
      (when (and file
		 (file-exists-p file)
		 (not (file-directory-p file))
		 (file-writable-p ido-current-directory)
		 (yes-or-no-p (concat "Delete " file "? ")))
	(delete-file file)
	;; Check if file still exists.
	(if (file-exists-p file)
	    ;; file could not be deleted
	    (setq ido-rescan t)
	  ;; else file was killed so remove name from list.
	  (setq ido-cur-list (delq (car ido-matches) ido-cur-list)))))))


;;; VISIT CHOSEN BUFFER
(defun ido-visit-buffer (buffer method &optional record)
  "Switch to BUFFER according to METHOD.
Record command in `command-history' if optional RECORD is non-nil."
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (let (win newframe)
    (cond
     ((eq method 'kill)
      (if record
	  (ido-record-command 'kill-buffer buffer))
      (kill-buffer buffer))

     ((eq method 'other-window)
      (if record
	  (ido-record-command 'switch-to-buffer buffer))
      (switch-to-buffer-other-window buffer))

     ((eq method 'display)
      (display-buffer buffer))

     ((eq method 'other-frame)
      (switch-to-buffer-other-frame buffer)
      (select-frame-set-input-focus (selected-frame)))

     ((and (memq method '(raise-frame maybe-frame))
	   window-system
	   (setq win (ido-buffer-window-other-frame buffer))
	   (or (eq method 'raise-frame)
	       (y-or-n-p "Jump to frame? ")))
      (setq newframe (window-frame win))
      (select-frame-set-input-focus newframe)
      (select-window win))

     ;; (eq method 'selected-window)
     (t
      ;;  No buffer in other frames...
      (if record
	  (ido-record-command 'switch-to-buffer buffer))
      (switch-to-buffer buffer)
      ))))


(defun ido-buffer-window-other-frame  (buffer)
  ;; Return window pointer if BUFFER is visible in another frame.
  ;; If BUFFER is visible in the current frame, return nil.
  (let ((blist (ido-get-buffers-in-frames 'current)))
    ;;If the buffer is visible in current frame, return nil
    (if (member buffer blist)
	nil
      ;;  maybe in other frame or icon
      (get-buffer-window buffer 0) ; better than 'visible
      )))


;;; ----------- IDONIZED FUNCTIONS ------------

;;;###autoload
(defun ido-switch-buffer ()
  "Switch to another buffer.
The buffer is displayed according to `ido-default-buffer-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.

As you type in a string, all of the buffers matching the string are
displayed if substring-matching is used \(default).  Look at
`ido-enable-prefix' and `ido-toggle-prefix'.  When you have found the
buffer you want, it can then be selected.  As you type, most keys have
their normal keybindings, except for the following: \\<ido-buffer-completion-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[ido-select-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[ido-edit-input] Edit input string.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of buffer names.
\\[ido-completion-help] Show list of matching buffers in separate window.
\\[ido-enter-find-file] Drop into `ido-find-file'.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list.
\\[ido-toggle-ignore] Toggle ignoring buffers listed in `ido-ignore-buffers'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method))

;;;###autoload
(defun ido-switch-buffer-other-window ()
  "Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive)
  (ido-buffer-internal 'other-window 'switch-to-buffer-other-window))

;;;###autoload
(defun ido-display-buffer ()
  "Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive)
  (ido-buffer-internal 'display 'display-buffer nil nil nil 'ignore))

;;;###autoload
(defun ido-kill-buffer ()
  "Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive)
  (ido-buffer-internal 'kill 'kill-buffer "Kill buffer: " (buffer-name (current-buffer)) nil 'ignore))

;;;###autoload
(defun ido-insert-buffer ()
  "Insert contents of a buffer in current buffer after point.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive)
  (ido-buffer-internal 'insert 'insert-buffer "Insert buffer: " nil nil 'ido-enter-insert-file))

;;;###autoload
(defun ido-switch-buffer-other-frame ()
  "Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive)
  (if ido-mode
      (ido-buffer-internal 'other-frame)
    (call-interactively 'switch-to-buffer-other-frame)))

;;;###autoload
(defun ido-find-file-in-dir (dir)
  "Switch to another file starting from DIR."
  (interactive "DDir: ")
  (setq dir (file-name-as-directory dir))
  (ido-file-internal ido-default-file-method nil dir nil nil nil 'ignore))

;;;###autoload
(defun ido-find-file ()
  "Edit file with name obtained via minibuffer.
The file is displayed according to `ido-default-file-method' -- the
default is to show it in the same window, unless it is already
visible in another frame.

The file name is selected interactively by typing a substring.  As you
type in a string, all of the filenames matching the string are displayed
if substring-matching is used \(default).  Look at `ido-enable-prefix' and
`ido-toggle-prefix'.  When you have found the filename you want, it can
then be selected.  As you type, most keys have their normal keybindings,
except for the following: \\<ido-file-completion-map>

RET Select the file at the front of the list of matches.  If the
list is empty, possibly prompt to create new file.

\\[ido-select-text] Select the current prompt as the buffer or file.
If no buffer or file is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that
matches all files.  If there is only one match, select that file.
If there is no common suffix, show a list of all matching files
in a separate window.
\\[ido-edit-input] Edit input string (including directory).
\\[ido-prev-work-directory] or \\[ido-next-work-directory] go to previous/next directory in work directory history.
\\[ido-merge-work-directories] search for file in the work directory history.
\\[ido-forget-work-directory] removes current directory from the work directory history.
\\[ido-prev-work-file] or \\[ido-next-work-file] cycle through the work file history.
\\[ido-wide-find-file-or-pop-dir] and \\[ido-wide-find-dir-or-delete-dir] prompts and uses find to locate files or directories.
\\[ido-make-directory] prompts for a directory to create in current directory.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of file names.
\\[ido-toggle-literal] Toggle literal reading of this file.
\\[ido-completion-help] Show list of matching files in separate window.
\\[ido-toggle-ignore] Toggle ignoring files listed in `ido-ignore-files'."

  (interactive)
  (ido-file-internal ido-default-file-method))

;;;###autoload
(defun ido-find-file-other-window ()
  "Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'other-window 'find-file-other-window))

;;;###autoload
(defun ido-find-alternate-file ()
  "Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'alt-file 'find-alternate-file nil "Find alternate file: "))

;;;###autoload
(defun ido-find-file-read-only ()
  "Edit file read-only with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'read-only 'find-file-read-only nil "Find file read-only: "))

;;;###autoload
(defun ido-find-file-read-only-other-window ()
  "Edit file read-only in other window with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'read-only 'find-file-read-only-other-window nil "Find file read-only other window: "))

;;;###autoload
(defun ido-find-file-read-only-other-frame ()
  "Edit file read-only in other frame with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'read-only 'find-file-read-only-other-frame nil "Find file read-only other frame: "))

;;;###autoload
(defun ido-display-file ()
  "Display a file in another window but don't select it.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'display nil nil nil nil nil 'ignore))

;;;###autoload
(defun ido-find-file-other-frame ()
  "Switch to another file and show it in another frame.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'other-frame 'find-file-other-frame))

;;;###autoload
(defun ido-write-file ()
  "Write current buffer to a file.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (let ((ido-process-ignore-lists t)
	(ido-work-directory-match-only nil)
	(ido-ignore-files (cons "[^/]\\'" ido-ignore-files))
	(ido-report-no-match nil)
	(ido-confirm-unique-completion t)
	(ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'write 'write-file nil "Write file: " nil nil 'ignore)))

;;;###autoload
(defun ido-insert-file ()
  "Insert contents of file in current buffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (ido-file-internal 'insert 'insert-file nil "Insert file: " nil nil 'ido-enter-insert-buffer))

;;;###autoload
(defun ido-dired ()
  "Call `dired' the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (let ((ido-report-no-match nil)
	(ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'dired 'dired nil "Dired: " 'dir)))

(defun ido-list-directory ()
  "Call `list-directory' the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
  (interactive)
  (let ((ido-report-no-match nil)
	(ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'list-directory 'list-directory nil "List directory: " 'dir)))

;;; XEmacs hack for showing default buffer

;; The first time we enter the minibuffer, Emacs puts up the default
;; buffer to switch to, but XEmacs doesn't -- presumably there is a
;; subtle difference in the two versions of post-command-hook.  The
;; default is shown for both whenever we delete all of our text
;; though, indicating its just a problem the first time we enter the
;; function.  To solve this, we use another entry hook for emacs to
;; show the default the first time we enter the minibuffer.


;;; ICOMPLETE TYPE CODE

(defun ido-initiate-auto-merge (buffer)
  (ido-trace "\n*merge timeout*" buffer)
  (setq ido-auto-merge-timer nil)
  (when (and (buffer-live-p buffer)
	     (ido-active)
	     (boundp 'ido-eoinput) ido-eoinput)
    (let ((contents (buffer-substring-no-properties (minibuffer-prompt-end) ido-eoinput)))
      (ido-trace "request merge")
      (setq ido-use-merged-list 'auto
	    ido-text-init contents
	    ido-rotate-temp t
	    ido-exit 'refresh)
      (with-current-buffer buffer
	(ido-tidy))
      (throw 'ido contents))))

(defun ido-exhibit ()
  "Post command hook for `ido'."
  ;; Find matching files and display a list in the minibuffer.
  ;; Copied from `icomplete-exhibit' with two changes:
  ;; 1. It prints a default file name when there is no text yet entered.
  ;; 2. It calls my completion routine rather than the standard completion.

  (when (ido-active)
    (let ((contents (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))
	  (buffer-undo-list t)
	  try-single-dir-match
	  refresh)

      (when ido-trace-enable
	(ido-trace "\nexhibit" this-command)
	(ido-trace "dir" ido-current-directory)
	(ido-trace "contents" contents)
	(ido-trace "list" ido-cur-list)
	(ido-trace "matches" ido-matches)
	(ido-trace "rescan" ido-rescan))

      (save-excursion
	(goto-char (point-max))
	;; Register the end of input, so we know where the extra stuff (match-status info) begins:
	(unless (boundp 'ido-eoinput)
	  ;; In case it got wiped out by major mode business:
	  (make-local-variable 'ido-eoinput))
	(setq ido-eoinput (point))

	;; Handle explicit directory changes
	(cond
	 ((memq ido-cur-item '(buffer list))
	  )

	 ((= (length contents) 0)
	  )

	 ((= (length contents) 1)
	  (cond
	   ((and (ido-is-tramp-root) (string-equal contents "/"))
	    (ido-set-current-directory ido-current-directory contents)
	    (setq refresh t))
	   ((and (ido-unc-hosts) (string-equal contents "/")
		 (let ((ido-enable-tramp-completion nil))
		   (ido-is-root-directory)))
	    (ido-set-current-directory "//")
	    (setq refresh t))
	  ))

	 ((and (string-match (if ido-enable-tramp-completion ".[:@]\\'" ".:\\'") contents)
	       (ido-is-root-directory) ;; Ange-ftp or tramp
	       (not (ido-local-file-exists-p contents)))
	  (ido-set-current-directory ido-current-directory contents)
	  (when (ido-is-slow-ftp-host)
	    (setq ido-exit 'fallback)
	    (exit-minibuffer))
	  (setq refresh t))

	 ((ido-final-slash contents)  ;; xxx/
	  (ido-trace "final slash" contents)
	  (cond
	   ((string-equal contents "~/")
	    (ido-set-current-home)
	    (setq refresh t))
	   ((string-equal contents "../")
	    (ido-up-directory t)
	    (setq refresh t))
	   ((string-equal contents "./")
	    (setq refresh t))
	   ((string-match "\\`~[-_a-zA-Z0-9]+[$]?/\\'" contents)
	    (ido-trace "new home" contents)
	    (ido-set-current-home contents)
	    (setq refresh t))
	   ((string-match "[$][A-Za-z0-9_]+/\\'" contents)
	    (let ((exp (condition-case ()
			   (expand-file-name
			    (substitute-in-file-name (substring contents 0 -1))
			    ido-current-directory)
			 (error nil))))
	      (ido-trace contents exp)
	      (when (and exp (file-directory-p exp))
		(ido-set-current-directory (file-name-directory exp))
		(setq ido-text-init (file-name-nondirectory exp))
		(setq refresh t))))
	   ((and (memq system-type '(windows-nt ms-dos))
		 (string-equal (substring contents 1) ":/"))
	    (ido-set-current-directory (file-name-directory contents))
	    (setq refresh t))
	   ((string-equal (substring contents -2 -1) "/")
	    (ido-set-current-directory
	     (if (memq system-type '(windows-nt ms-dos))
		 (expand-file-name "/" ido-current-directory)
	       "/"))
	    (setq refresh t))
	   ((and (or ido-directory-nonreadable ido-directory-too-big)
		 (file-directory-p (concat ido-current-directory (file-name-directory contents))))
	    (ido-set-current-directory
	     (concat ido-current-directory (file-name-directory contents)))
	    (setq refresh t))
	   (t
	    (ido-trace "try single dir")
	    (setq try-single-dir-match t))))

	 ((and (string-equal (substring contents -2 -1) "/")
	       (not (string-match "[$]" contents)))
	  (ido-set-current-directory
	   (cond
	    ((= (length contents) 2)
	     "/")
	    (ido-matches
	     (concat ido-current-directory (ido-name (car ido-matches))))
	    (t
	     (concat ido-current-directory (substring contents 0 -1)))))
	  (setq ido-text-init (substring contents -1))
	  (setq refresh t))

	 ((and (not ido-use-merged-list)
	       (not (ido-final-slash contents))
	       (eq ido-try-merged-list t)
	       (numberp ido-auto-merge-work-directories-length)
	       (> ido-auto-merge-work-directories-length 0)
	       (= (length contents) ido-auto-merge-work-directories-length)
	       (not (and ido-auto-merge-inhibit-characters-regexp
			 (string-match ido-auto-merge-inhibit-characters-regexp contents)))
	       (not (input-pending-p)))
	  (setq ido-use-merged-list 'auto
		ido-text-init contents
		ido-rotate-temp t)
	  (setq refresh t))

	 (t nil))

	(when refresh
	  (ido-trace "refresh on /" ido-text-init)
	  (setq ido-exit 'refresh)
	  (exit-minibuffer))

	;; Update the list of matches
	(setq ido-text contents)
	(ido-set-matches)
	(ido-trace "new    " ido-matches)

	(when (and ido-enter-matching-directory
		   ido-matches
		   (or (eq ido-enter-matching-directory 'first)
		       (null (cdr ido-matches)))
		   (ido-final-slash (ido-name (car ido-matches)))
		   (or try-single-dir-match
		       (eq ido-enter-matching-directory t)))
	  (ido-trace "single match" (car ido-matches))
	  (ido-set-current-directory
	   (concat ido-current-directory (ido-name (car ido-matches))))
	  (setq ido-exit 'refresh)
	  (exit-minibuffer))

	(when (and (not ido-matches)
		   (not ido-directory-nonreadable)
		   (not ido-directory-too-big)
		   ;; ido-rescan ?
		   ido-process-ignore-lists
		   ido-ignored-list)
	  (let ((ido-process-ignore-lists nil)
		(ido-rotate ido-rotate)
		(ido-cur-list ido-ignored-list))
	    (ido-trace "try all" ido-ignored-list)
	    (ido-set-matches))
	  (when ido-matches
	    (ido-trace "found  " ido-matches)
	    (setq ido-rescan t)
	    (setq ido-process-ignore-lists-inhibit t)
	    (setq ido-text-init ido-text)
	    (setq ido-exit 'refresh)
	    (exit-minibuffer)))

	(when (and
	       ido-rescan
	       (not ido-matches)
	       (memq ido-cur-item '(file dir))
	       (not (ido-is-root-directory))
	       (> (length contents) 1)
	       (not (string-match "[$]" contents))
	       (not ido-directory-nonreadable)
	       (not ido-directory-too-big))
	  (ido-trace "merge?")
	  (if ido-use-merged-list
	      (ido-undo-merge-work-directory contents nil)
	    (when (and (eq ido-try-merged-list t)
		       (numberp ido-auto-merge-work-directories-length)
		       (= ido-auto-merge-work-directories-length 0)
		       (not (and ido-auto-merge-inhibit-characters-regexp
				 (string-match ido-auto-merge-inhibit-characters-regexp contents)))
		       (not (input-pending-p)))
	      (ido-trace "\n*start timer*")
	      (setq ido-auto-merge-timer
		    (run-with-timer ido-auto-merge-delay-time nil 'ido-initiate-auto-merge (current-buffer))))))

	(setq ido-rescan t)

	(if (and ido-use-merged-list
		 ido-matches
		 (not (string-equal (car (cdr (car ido-matches))) ido-current-directory)))
	    (progn
	      (ido-set-current-directory (car (cdr (car ido-matches))))
	      (setq ido-use-merged-list t
		    ido-exit 'keep
		    ido-text-init ido-text)
	      (exit-minibuffer)))

	;; Insert the match-status information:
	(ido-set-common-completion)
	(let ((inf (ido-completions contents)))
	  (setq ido-show-confirm-message nil)
	  (ido-trace "inf" inf)
	  (insert inf))
	))))

(defun ido-completions (name)
  ;; Return the string that is displayed after the user's text.
  ;; Modified from `icomplete-completions'.

  (let* ((comps ido-matches)
	 (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
		   ido-merged-indicator))
	 first)

    (if (and ind ido-use-faces)
	(put-text-property 0 1 'face 'ido-indicator ind))

    (if (and ido-use-faces comps)
	(let* ((fn (ido-name (car comps)))
	       (ln (length fn)))
	  (setq first (format "%s" fn))
	  (put-text-property 0 ln 'face
			     (if (= (length comps) 1)
                                 (if ido-incomplete-regexp
                                     'ido-incomplete-regexp
                                   'ido-only-match)
			       'ido-first-match)
			     first)
	  (if ind (setq first (concat first ind)))
	  (setq comps (cons first (cdr comps)))))

    (cond ((null comps)
	   (cond
	    (ido-show-confirm-message
	     (or (nth 10 ido-decorations) " [Confirm]"))
	    (ido-directory-nonreadable
	     (or (nth 8 ido-decorations) " [Not readable]"))
	    (ido-directory-too-big
	     (or (nth 9 ido-decorations) " [Too big]"))
	    (ido-report-no-match
	     (nth 6 ido-decorations))  ;; [No match]
	    (t "")))
	  (ido-incomplete-regexp
           (concat " " (car comps)))
	  ((null (cdr comps))		;one match
	   (concat (if (if (not ido-enable-regexp)
                           (= (length (ido-name (car comps))) (length name))
                         ;; We can't rely on the length of the input
                         ;; for regexps, so explicitly check for a
                         ;; complete match
                         (string-match name (ido-name (car comps)))
                         (string-equal (match-string 0 (ido-name (car comps)))
                                       (ido-name (car comps))))
                       ""
                     ;; when there is one match, show the matching file name in full
                     (concat (nth 4 ido-decorations)  ;; [ ... ]
                             (ido-name (car comps))
                             (nth 5 ido-decorations)))
		   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
	  (t				;multiple matches
	   (let* ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
		  (alternatives
		   (apply
		    #'concat
		    (cdr (apply
			  #'nconc
			  (mapcar
			   (lambda (com)
			     (setq com (ido-name com))
			     (setq items (1- items))
			     (cond
			      ((< items 0) ())
			      ((= items 0) (list (nth 3 ido-decorations))) ; " | ..."
			      (t
			       (list (or ido-separator (nth 2 ido-decorations)) ; " | "
				     (let ((str (substring com 0)))
				       (if (and ido-use-faces
						(not (string= str first))
						(ido-final-slash str))
					   (put-text-property 0 (length str) 'face 'ido-subdir str))
				       str)))))
			   comps))))))

	     (concat
	      ;; put in common completion item -- what you get by pressing tab
	      (if (and (stringp ido-common-match-string)
		       (> (length ido-common-match-string) (length name)))
		  (concat (nth 4 ido-decorations)   ;; [ ... ]
			  (substring ido-common-match-string (length name))
			  (nth 5 ido-decorations)))
	      ;; list all alternatives
	      (nth 0 ido-decorations)  ;; { ... }
	      alternatives
	      (nth 1 ido-decorations)))))))

(defun ido-minibuffer-setup ()
  "Minibuffer setup hook for `ido'."
  ;; Copied from `icomplete-minibuffer-setup-hook'.
  (when (ido-active)
    (add-hook 'pre-command-hook 'ido-tidy nil t)
    (add-hook 'post-command-hook 'ido-exhibit nil t)
    (when (featurep 'xemacs)
      (ido-exhibit)
      (goto-char (point-min)))
    (run-hooks 'ido-minibuffer-setup-hook)
    (when ido-initial-position
      (goto-char (+ (minibuffer-prompt-end) ido-initial-position))
      (setq ido-initial-position nil))))

(defun ido-tidy ()
  "Pre command hook for `ido'."
  ;; Remove completions display, if any, prior to new user input.
  ;; Copied from `icomplete-tidy'."

  (when ido-auto-merge-timer
    (ido-trace "\n*cancel timer*" this-command)
    (cancel-timer ido-auto-merge-timer)
    (setq ido-auto-merge-timer nil))

  (if (ido-active)
      (if (and (boundp 'ido-eoinput)
	       ido-eoinput)

	  (if (> ido-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq ido-eoinput (point-max))
	    (let ((buffer-undo-list t))
	      (delete-region ido-eoinput (point-max))))

	;; Reestablish the local variable 'cause minibuffer-setup is weird:
	(make-local-variable 'ido-eoinput)
	(setq ido-eoinput 1))))

(defun ido-summary-buffers-to-end ()
  ;; Move the summaries to the end of the buffer list.
  ;; This is an example function which can be hooked on to
  ;; `ido-make-buffer-list-hook'.  Any buffer matching the regexps
  ;; `Summary' or `output\*$'are put to the end of the list.
  (let ((summaries (delq nil (mapcar
			      (lambda (x)
				 (if (or
				      (string-match "Summary" x)
				      (string-match "output\\*\\'" x))
				     x))
			      ido-temp-list))))
    (ido-to-end summaries)))

;;; Helper functions for other programs

(put 'dired-do-rename 'ido 'ignore)
(put 'ibuffer-find-file 'ido 'find-file)
(put 'dired-other-window 'ido 'dir)

;;;###autoload
(defun ido-read-buffer (prompt &optional default require-match)
  "Ido replacement for the built-in `read-buffer'.
Return the name of a buffer selected.
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing buffer must be selected."
  (let* ((ido-current-directory nil)
	 (ido-directory-nonreadable nil)
	 (ido-directory-too-big nil)
	 (ido-context-switch-command 'ignore)
	 (buf (ido-read-internal 'buffer prompt 'ido-buffer-history default require-match)))
    (if (eq ido-exit 'fallback)
	(let ((read-buffer-function nil))
	  (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
	  (read-buffer prompt default require-match))
      buf)))

;;;###autoload
(defun ido-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Ido replacement for the built-in `read-file-name'.
Read file name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters."
  (let (filename)
    (cond
     ((or (eq predicate 'file-directory-p)
	  (eq (get this-command 'ido) 'dir)
	  (memq this-command ido-read-file-name-as-directory-commands))
      (setq filename
	    (ido-read-directory-name prompt dir default-filename mustmatch initial))
      (if (eq ido-exit 'fallback)
	  (setq filename 'fallback)))
     ((and (not (eq (get this-command 'ido) 'ignore))
	   (not (memq this-command ido-read-file-name-non-ido))
	   (or (null predicate) (eq predicate 'file-exists-p)))
      (let* (ido-saved-vc-hb
	     (ido-context-switch-command
	      (if (eq (get this-command 'ido) 'find-file) nil 'ignore))
	     (vc-handled-backends (and (boundp 'vc-handled-backends) vc-handled-backends))
	     (minibuffer-completing-file-name t)
	     (ido-current-directory (ido-expand-directory dir))
	     (ido-directory-nonreadable (not (file-readable-p ido-current-directory)))
	     (ido-directory-too-big (and (not ido-directory-nonreadable)
					 (ido-directory-too-big-p ido-current-directory)))
	     (ido-work-directory-index -1)
	     (ido-show-dot-for-dired (and ido-show-dot-for-dired
					  (not default-filename)))
	     (ido-work-file-index -1)
	     (ido-find-literal nil))
	(setq ido-exit nil)
	(setq filename
	      (ido-read-internal 'file prompt 'ido-file-history default-filename mustmatch initial))
	(cond
	 ((eq ido-exit 'fallback)
	  (setq filename 'fallback))
	 ((eq ido-exit 'dired)
	  (setq filename ido-current-directory))
	 (filename
	  (setq filename
		(concat ido-current-directory filename))))))
     (t
      (setq filename 'fallback)))
    (if (eq filename 'fallback)
	(let ((read-file-name-function nil))
	  (run-hook-with-args 'ido-before-fallback-functions 'read-file-name)
	  (read-file-name prompt dir default-filename mustmatch initial predicate))
      filename)))

;;;###autoload
(defun ido-read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  "Ido replacement for the built-in `read-directory-name'.
Read directory name, prompting with PROMPT and completing in directory DIR.
See `read-directory-name' for additional parameters."
  (let* (filename
	 (minibuffer-completing-file-name t)
	 (ido-context-switch-command 'ignore)
	 ido-saved-vc-hb
	 (ido-current-directory (ido-expand-directory dir))
	 (ido-directory-nonreadable (not (file-readable-p ido-current-directory)))
	 (ido-directory-too-big (and (not ido-directory-nonreadable)
				     (ido-directory-too-big-p ido-current-directory)))
	 (ido-work-directory-index -1)
	 (ido-work-file-index -1))
    (setq filename
	  (ido-read-internal 'dir prompt 'ido-file-history default-dirname mustmatch initial))
    (if filename
	(if (and (stringp filename) (string-equal filename "."))
	    ido-current-directory
	  (concat ido-current-directory filename)))))

;;;###autoload
(defun ido-completing-read (prompt choices &optional _predicate require-match
                            initial-input hist def _inherit-input-method)
  "Ido replacement for the built-in `completing-read'.
Read a string in the minibuffer with ido-style completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
CHOICES is a list of strings which are the possible completions.
PREDICATE and INHERIT-INPUT-METHOD is currently ignored; it is included
 to be compatible with `completing-read'.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of CHOICES or is null.
 If the input is null, `ido-completing-read' returns DEF, or an empty
 string if DEF is nil, regardless of the value of REQUIRE-MATCH.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
 with point positioned at the end.
HIST, if non-nil, specifies a history list.
DEF, if non-nil, is the default value."
  (let ((ido-current-directory nil)
	(ido-directory-nonreadable nil)
	(ido-directory-too-big nil)
	(ido-context-switch-command 'ignore)
	(ido-choice-list choices))
    ;; Initialize ido before invoking ido-read-internal
    (ido-common-initialization)
    (ido-read-internal 'list prompt hist def require-match initial-input)))

(defun ido-unload-function ()
  "Unload the Ido library."
  (ido-mode -1)
  (setq minor-mode-map-alist (assq-delete-all 'ido-mode minor-mode-map-alist))
  ;; continue standard unloading
  nil)

(provide 'ido)

;;; ido.el ends here
