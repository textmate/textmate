;;; iswitchb.el --- switch between buffers using substrings

;; Copyright (C) 1996-1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Stephen Eglen <stephen@gnu.org>
;; Maintainer: Stephen Eglen <stephen@gnu.org>
;; Keywords: completion convenience

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

;; Installation:
;; To get the functions in this package bound to keys, use
;; M-x iswitchb-mode or customize the option `iswitchb-mode'.
;; Alternatively, add the following line to your .emacs:
;; (iswitchb-mode 1)

;; As you type in a substring, the list of buffers currently matching
;; the substring is displayed as you type.  The list is ordered so
;; that the most recent buffers visited come at the start of the list.
;; The buffer at the start of the list will be the one visited when
;; you press return.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer you want will be at the
;; top of the list.  Alternatively, you can use C-s and C-r to rotate
;; buffer names in the list until the one you want is at the top of
;; the list.  Completion is also available so that you can see what is
;; common to all of the matching buffers as you type.

;; This code is similar to a couple of other packages.  Michael R Cook
;; <cook@sightpath.com> wrote a similar buffer switching package, but
;; does exact matching rather than substring matching on buffer names.
;; I also modified a couple of functions from icomplete.el to provide
;; the completion feedback in the minibuffer.

;;; Example

;; If I have two buffers called "123456" and "123", with "123456" the
;; most recent, when I use iswitchb, I first of all get presented with
;; the list of all the buffers
;;
;;       iswitch  {123456,123}
;;
;; If I then press 2:
;;       iswitch 2[3]{123456,123}
;;
;; The list in {} are the matching buffers, most recent first (buffers
;; visible in the current frame are put at the end of the list by
;; default).  At any time I can select the item at the head of the
;; list by pressing RET.  I can also put the first element at the end
;; of the list by pressing C-s, or put the last element at the head of
;; the list by pressing C-r.  The item in [] indicates what can be
;; added to my input by pressing TAB.  In this case, I will get "3"
;; added to my input.  So, press TAB:
;;	 iswitch 23{123456,123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;; However, if I type 4, I only have one match left:
;;       iswitch 234[123456] [Matched]
;;
;; Since there is only one matching buffer left, it is given in [] and we
;; see the text [Matched] afterwards.  I can now press TAB or RET to go
;; to that buffer.
;;
;; If however, I now type "a":
;;       iswitch 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" buffer would be
;; just to type 4 and then RET (assuming there isn't any newer buffer
;; with 4 in its name).

;; To see a full list of all matching buffers in a separate buffer,
;; hit ? or press TAB when there are no further completions to the
;; substring.  Repeated TAB presses will scroll you through this
;; separate buffer.

;; The buffer at the head of the list can be killed by pressing C-k.
;; If the buffer needs saving, you will be queried before the buffer
;; is killed.

;; If you find that the file you are after is not in a buffer, you can
;; press C-x C-f to immediately drop into find-file.

;; See the doc string of iswitchb for full keybindings and features.
;; (describe-function 'iswitchb)

;; Case matching: The case of strings when matching can be ignored or
;; used depending on the value of iswitchb-case (default is the same
;; as case-fold-search, normally t).  Imagine you have the following
;; buffers:
;;
;; INBOX *info* *scratch*
;;
;; Then these will be the matching buffers, depending on how you type
;; the two letters `in' and the value of iswitchb-case:
;;
;; iswitchb-case   user input  | matching buffers
;; ----------------------------------------------
;; nil             in          | *info*
;; t               in          | INBOX, *info*
;; t               IN          | INBOX
;; t               In          | [No match]

;;; Customization

;; See the User Variables section below for easy ways to change the
;; functionality of the program.  These are accessible using the
;; custom package.
;; To modify the keybindings, use something like:
;;
;;(add-hook 'iswitchb-mode-hook 'iswitchb-my-keys)
;;(defun iswitchb-my-keys ()
;;  "Add my keybindings for iswitchb."
;;  (define-key iswitchb-mode-map " " 'iswitchb-next-match))
;;
;; Seeing all the matching buffers
;;
;; If you have many matching buffers, they may not all fit onto one
;; line of the minibuffer.  In Emacs 21, the variable
;; `resize-mini-windows' controls how many lines of the minibuffer can
;; be seen.  For older versions of emacs, you can use
;; `resize-minibuffer-mode'.  You can also limit iswitchb so that it
;; only shows a certain number of lines -- see the documentation for
;; `iswitchb-minibuffer-setup-hook'.

;; Changing the list of buffers

;; By default, the list of current buffers is most recent first,
;; oldest last, with the exception that the buffers visible in the
;; current frame are put at the end of the list.  A hook exists to
;; allow other functions to order the list.  For example, if you add:
;;
;; (add-hook 'iswitchb-make-buflist-hook 'iswitchb-summaries-to-end)
;;
;; then all buffers matching "Summary" are moved to the end of the
;; list.  (I find this handy for keeping the INBOX Summary and so on
;; out of the way.)  It also moves buffers matching "output\*$" to the
;; end of the list (these are created by AUCTeX when compiling.)
;; Other functions could be made available which alter the list of
;; matching buffers (either deleting or rearranging elements.)

;; Font-Lock

;; font-lock is used to highlight the first matching buffer.  To
;; switch this off, set (setq iswitchb-use-faces nil).  Coloring of
;; the matching buffer name was suggested by Carsten Dominik
;; (dominik@strw.leidenuniv.nl)

;; Replacement for read-buffer

;; iswitchb-read-buffer has been written to be a drop in replacement
;; for the normal buffer selection routine `read-buffer'.  To use
;; iswitch for all buffer selections in Emacs, add:
;; (setq read-buffer-function 'iswitchb-read-buffer)
;; (This variable was introduced in Emacs 20.3.)
;; XEmacs users can get the same behavior by doing:
;; (defalias 'read-buffer 'iswitchb-read-buffer)
;; since `read-buffer' is defined in lisp.

;; Using iswitchb for other completion tasks.

;; Kin Cho (kin@neoscale.com) sent the following suggestion to use
;; iswitchb for other completion tasks.
;;
;; (defun my-icompleting-read (prompt choices)
;;   "Use iswitch as a completing-read replacement to choose from
;; choices.  PROMPT is a string to prompt with.  CHOICES is a list of
;; strings to choose from."
;;   (let ((iswitchb-make-buflist-hook
;;          (lambda ()
;;            (setq iswitchb-temp-buflist choices))))
;;     (iswitchb-read-buffer prompt)))
;;
;; example:
;; (my-icompleting-read "Which fruit? " '
;; 		     ("apple" "pineapple" "pear" "bananas" "oranges") )

;; Kin Cho also suggested the following defun.  Once you have a subset of
;; matching buffers matching your current prompt, you can then press
;; e.g. C-o to restrict matching to those buffers and clearing the prompt:
;; (defun iswitchb-exclude-nonmatching()
;;    "Make iswitchb work on only the currently matching names."
;;    (interactive)
;;    (setq iswitchb-buflist iswitchb-matches)
;;    (setq iswitchb-rescan t)
;;    (delete-minibuffer-contents))
;;
;; (add-hook 'iswitchb-define-mode-map-hook
;; 	     (lambda () (define-key
;; 			iswitchb-mode-map "\C-o"
;; 			'iswitchb-exclude-nonmatching)))

;; Other lisp packages extend iswitchb behavior to other tasks.  See
;; ido.el (by Kim Storm) and mcomplete.el (Yuji Minejima).

;; Window managers: Switching frames/focus follows mouse; Sawfish.

;; If you switch to a buffer that is visible in another frame,
;; iswitchb can switch focus to that frame.  If your window manager
;; uses "click to focus" policy for window selection, you should also
;; set focus-follows-mouse to nil.

;; iswitch functionality has also been implemented for switching
;; between windows in the Sawfish window manager.

;; Regexp matching

;; There is provision for regexp matching within iswitchb, enabled
;; through `iswitchb-regexp'.  This allows you to type `c$' for
;; example and see all buffer names ending in `c'.  No completion
;; mechanism is currently offered when regexp searching.

;;; TODO

;;; Acknowledgements

;; Thanks to Jari Aalto <jari.aalto@poboxes.com> for help with the
;; first version of this package, iswitch-buffer.  Thanks also to many
;; others for testing earlier versions.

;;; Code:

(require 'font-lock)

;;; User Variables
;;
;; These are some things you might want to change.

(defgroup iswitchb nil
  "Switch between buffers using substrings."
  :group 'convenience
  :group 'completion
  :link '(emacs-commentary-link :tag "Commentary" "iswitchb.el")
  :link '(url-link "http://www.anc.ed.ac.uk/~stephen/emacs/")
  :link '(emacs-library-link :tag "Lisp File" "iswitchb.el"))

(defcustom iswitchb-case case-fold-search
  "Non-nil if searching of buffer names should ignore case.
If this is non-nil but the user input has any upper case letters, matching
is temporarily case sensitive."
  :type 'boolean
  :group 'iswitchb)

(defcustom iswitchb-buffer-ignore
  '("^ ")
  "List of regexps or functions matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is `^ '.  See the source file for
example functions that filter buffer names."
  :type '(repeat (choice regexp function))
  :group 'iswitchb)
(put 'iswitchb-buffer-ignore 'risky-local-variable t)

(defcustom iswitchb-max-to-show nil
  "If non-nil, limit the number of names shown in the minibuffer.
If this value is N, and N is greater than the number of matching
buffers, the first N/2 and the last N/2 matching buffers are
shown.  This can greatly speed up iswitchb if you have a
multitude of buffers open."
  :type '(choice (const :tag "Show all" nil) integer)
  :group 'iswitchb)

(defcustom iswitchb-use-virtual-buffers nil
  "If non-nil, refer to past buffers when none match.
This feature relies upon the `recentf' package, which will be
enabled if this variable is configured to a non-nil value."
  :type 'boolean
  :require 'recentf
  :set (function
	(lambda (sym value)
	  (if value (recentf-mode 1))
	  (set sym value)))
  :group 'iswitchb)

(defvar iswitchb-virtual-buffers nil)

(defcustom iswitchb-cannot-complete-hook 'iswitchb-completion-help
  "Hook run when `iswitchb-complete' can't complete any more.
The most useful values are `iswitchb-completion-help', which pops up a
window with completion alternatives, or `iswitchb-next-match' or
`iswitchb-prev-match', which cycle the buffer list."
  :type 'hook
  :group 'iswitchb)

;; Examples for setting the value of iswitchb-buffer-ignore
;;(defun iswitchb-ignore-c-mode (name)
;;  "Ignore all c mode buffers -- example function for iswitchb."
;;  (with-current-buffer name
;;    (derived-mode-p 'c-mode)))

;;(setq iswitchb-buffer-ignore '("^ " iswitchb-ignore-c-mode))
;;(setq iswitchb-buffer-ignore '("^ " "\\.c\\'" "\\.h\\'"))

(defcustom iswitchb-default-method  'always-frame
    "How to switch to new buffer when using `iswitchb-buffer'.
Possible values:
`samewindow'	Show new buffer in same window
`otherwindow'	Show new buffer in another window (same frame)
`display'	Display buffer in another window without switching to it
`otherframe'	Show new buffer in another frame
`maybe-frame'	If a buffer is visible in another frame, prompt to ask if you
		you want to see the buffer in the same window of the current
  		frame or in the other frame.
`always-frame'  If a buffer is visible in another frame, raise that
		frame.  Otherwise, visit the buffer in the same window."
    :type '(choice (const samewindow)
		   (const otherwindow)
		   (const display)
		   (const otherframe)
		   (const maybe-frame)
		   (const always-frame))
    :group 'iswitchb)

(defcustom iswitchb-regexp nil
  "Non-nil means that `iswitchb' will do regexp matching.
Value can be toggled within `iswitchb' using `iswitchb-toggle-regexp'."
  :type 'boolean
  :group 'iswitchb)

(defcustom iswitchb-newbuffer t
  "Non-nil means create new buffer if no buffer matches substring.
See also `iswitchb-prompt-newbuffer'."
  :type 'boolean
  :group 'iswitchb)

(defcustom iswitchb-prompt-newbuffer t
  "Non-nil means prompt user to confirm before creating new buffer.
See also `iswitchb-newbuffer'."
  :type 'boolean
  :group 'iswitchb)

(define-obsolete-variable-alias 'iswitchb-use-fonts 'iswitchb-use-faces "22.1")

(defcustom iswitchb-use-faces t
  "Non-nil means use font-lock faces for showing first match."
  :type 'boolean
  :group 'iswitchb)

(defcustom iswitchb-use-frame-buffer-list nil
  "Non-nil means use the currently selected frame's buffer list."
  :type 'boolean
  :group 'iswitchb)

(defcustom iswitchb-make-buflist-hook  nil
  "Hook to run when list of matching buffers is created."
  :type 'hook
  :group 'iswitchb)

(defcustom iswitchb-delim ","
  "Delimiter to put between buffer names when displaying results."
  :type 'string
  :group 'iswitchb)

(defvar iswitchb-all-frames 'visible
  "*Argument to pass to `walk-windows' when finding visible buffers.
See documentation of `walk-windows' for useful values.")

(defcustom iswitchb-minibuffer-setup-hook nil
  "Iswitchb-specific customization of minibuffer setup.

This hook is run during minibuffer setup if `iswitchb' is active.
For instance:
\(add-hook 'iswitchb-minibuffer-setup-hook
	  '\(lambda () (set (make-local-variable 'max-mini-window-height) 3)))
will constrain the minibuffer to a maximum height of 3 lines when
iswitchb is running."
  :type 'hook
  :group 'iswitchb)

(defface iswitchb-single-match
  '((t
     (:inherit font-lock-comment-face)))
  "Iswitchb face for single matching buffer name."
  :version "22.1"
  :group 'iswitchb)

(defface iswitchb-current-match
  '((t
     (:inherit font-lock-function-name-face)))
  "Iswitchb face for current matching buffer name."
  :version "22.1"
  :group 'iswitchb)

(defface iswitchb-virtual-matches
  '((t
     (:inherit font-lock-builtin-face)))
  "Iswitchb face for matching virtual buffer names.
See also `iswitchb-use-virtual-buffers'."
  :version "22.1"
  :group 'iswitchb)

(defface iswitchb-invalid-regexp
  '((t
     (:inherit font-lock-warning-face)))
  "Iswitchb face for indicating invalid regexp. "
  :version "22.1"
  :group 'iswitchb)

;; Do we need the variable iswitchb-use-mycompletion?

;;; Internal Variables

(defvar iswitchb-method nil
  "Stores the method for viewing the selected buffer.
Its value is one of `samewindow', `otherwindow', `display', `otherframe',
`maybe-frame' or `always-frame'.  See `iswitchb-default-method' for
details of values.")

(defvar iswitchb-eoinput 1
  "Point where minibuffer input ends and completion info begins.
Copied from `icomplete-eoinput'.")
(make-variable-buffer-local 'iswitchb-eoinput)

(defvar iswitchb-buflist nil
  "Stores the current list of buffers that will be searched through.
The list is ordered, so that the most recent buffers come first,
although by default, the buffers visible in the current frame are put
at the end of the list.  Created by `iswitchb-make-buflist'.")

;; todo -- is this necessary?

(defvar iswitchb-use-mycompletion nil
  "Non-nil means use `iswitchb-buffer' completion feedback.
Should only be set to t by iswitchb functions, so that it doesn't
interfere with other minibuffer usage.")

(defvar iswitchb-change-word-sub nil
  "Private variable used by `iswitchb-word-matching-substring'.")

(defvar iswitchb-common-match-string  nil
  "Stores the string that is common to all matching buffers.")

(defvar iswitchb-rescan nil
  "Non-nil means we need to regenerate the list of matching buffers.")

(defvar iswitchb-text nil
  "Stores the users string as it is typed in.")

(defvar iswitchb-matches nil
  "List of buffers currently matching `iswitchb-text'.")

(defvar iswitchb-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "?" 'iswitchb-completion-help)
    (define-key map "\C-s" 'iswitchb-next-match)
    (define-key map "\C-r" 'iswitchb-prev-match)
    (define-key map "\t" 'iswitchb-complete)
    (define-key map "\C-j" 'iswitchb-select-buffer-text)
    (define-key map "\C-t" 'iswitchb-toggle-regexp)
    (define-key map "\C-x\C-f" 'iswitchb-find-file)
    (define-key map "\C-c" 'iswitchb-toggle-case)
    (define-key map "\C-k" 'iswitchb-kill-buffer)
    (define-key map "\C-m" 'iswitchb-exit-minibuffer)
    map)
  "Minibuffer keymap for `iswitchb-buffer'.")

(defvar iswitchb-global-map
  (let ((map (make-sparse-keymap)))
    (dolist (b '((switch-to-buffer . iswitchb-buffer)
                 (switch-to-buffer-other-window . iswitchb-buffer-other-window)
                 (switch-to-buffer-other-frame . iswitchb-buffer-other-frame)
                 (display-buffer . iswitchb-display-buffer)))
      (if (fboundp 'command-remapping)
          (define-key map (vector 'remap (car b)) (cdr b))
        (substitute-key-definition (car b) (cdr b) map global-map)))
    map)
  "Global keymap for `iswitchb-mode'.")

(defvar iswitchb-history nil
  "History of buffers selected using `iswitchb-buffer'.")

(defvar iswitchb-exit nil
  "Flag to monitor how `iswitchb-buffer' exits.
If equal to `takeprompt', we use the prompt as the buffer name to be
selected.")

(defvar iswitchb-buffer-ignore-orig nil
  "Stores original value of `iswitchb-buffer-ignore'.")

(defvar iswitchb-default nil
  "Default buffer for iswitchb.")

;; The following variables are needed to keep the byte compiler quiet.
(defvar iswitchb-require-match nil
  "Non-nil if matching buffer must be selected.")

(defvar iswitchb-temp-buflist nil
  "Stores a temporary version of the buffer list being created.")

(defvar iswitchb-bufs-in-frame nil
  "List of the buffers visible in the current frame.")

(defvar iswitchb-minibuf-depth nil
  "Value we expect to be returned by `minibuffer-depth' in the minibuffer.")

(defvar iswitchb-common-match-inserted nil
  "Non-nil if we have just inserted a common match in the minibuffer.")

(defvar iswitchb-invalid-regexp)

;;; FUNCTIONS

;;; ISWITCHB KEYMAP
(defun iswitchb-define-mode-map ()
  "Set up the keymap for `iswitchb-buffer'."
  (interactive)
  (let (map)
    ;; generated every time so that it can inherit new functions.
    ;;(or iswitchb-mode-map

    (setq map (copy-keymap minibuffer-local-map))
    (define-key map "?" 'iswitchb-completion-help)
    (define-key map "\C-s" 'iswitchb-next-match)
    (define-key map "\C-r" 'iswitchb-prev-match)
    (define-key map "\t" 'iswitchb-complete)
    (define-key map "\C-j" 'iswitchb-select-buffer-text)
    (define-key map "\C-t" 'iswitchb-toggle-regexp)
    (define-key map "\C-x\C-f" 'iswitchb-find-file)
    (define-key map "\C-n" 'iswitchb-toggle-ignore)
    (define-key map "\C-c" 'iswitchb-toggle-case)
    (define-key map "\C-k" 'iswitchb-kill-buffer)
    (define-key map "\C-m" 'iswitchb-exit-minibuffer)
    (setq iswitchb-mode-map map)
    (run-hooks 'iswitchb-define-mode-map-hook)))

(make-obsolete 'iswitchb-define-mode-map
	       "use M-x iswitchb-mode or customize the variable `iswitchb-mode'."
	       "21.1")

;;; MAIN FUNCTION
(defun iswitchb ()
  "Switch to buffer matching a substring.
As you type in a string, all of the buffers matching the string are
displayed.  When you have found the buffer you want, it can then be
selected.  As you type, most keys have their normal keybindings,
except for the following:
\\<iswitchb-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[iswitchb-select-buffer-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[iswitchb-next-match] Put the first element at the end of the list.
\\[iswitchb-prev-match] Put the last element at the start of the list.
\\[iswitchb-complete] Complete a common suffix to the current string that
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[iswitchb-toggle-regexp] Toggle regexp searching.
\\[iswitchb-toggle-case] Toggle case-sensitive searching of buffer names.
\\[iswitchb-completion-help] Show list of matching buffers in separate window.
\\[iswitchb-find-file] Exit iswitchb and drop into `find-file'.
\\[iswitchb-kill-buffer] Kill buffer at head of buffer list."
  ;;\\[iswitchb-toggle-ignore] Toggle ignoring certain buffers (see \
  ;;`iswitchb-buffer-ignore')

  (let* ((prompt "iswitch ")
         iswitchb-invalid-regexp
	 (buf (iswitchb-read-buffer prompt)))

    ;;(message "chosen text %s" iswitchb-final-text)
    ;; Choose the buffer name: either the text typed in, or the head
    ;; of the list of matches

    (cond ( (eq iswitchb-exit 'findfile)
	    (call-interactively 'find-file))
          (iswitchb-invalid-regexp
           (message "Won't make invalid regexp named buffer"))
	  (t
	   ;; View the buffer
	   ;;(message "go to buf %s" buf)
	   ;; Check buf is non-nil.
	   (if buf
	       (if (get-buffer buf)
		   ;; buffer exists, so view it and then exit
		   (iswitchb-visit-buffer buf)
		 ;; else buffer doesn't exist
		 (iswitchb-possible-new-buffer buf)))
	   ))))

(defun iswitchb-read-buffer (prompt &optional default require-match
				    start matches-set)
  "Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.
PROMPT is the prompt to give to the user.
DEFAULT if given is the default buffer to be selected, which will
go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing buffer must be selected.
If START is a string, the selection process is started with that
string.
If MATCHES-SET is non-nil, the buflist is not updated before
the selection process begins.  Used by isearchb.el."
  (let
      (
       buf-sel
       iswitchb-final-text
       (icomplete-mode nil) ;; prevent icomplete starting up
       )

    (iswitchb-define-mode-map)
    (setq iswitchb-exit nil)
    (setq iswitchb-default
	  (if (bufferp default)
	      (buffer-name default)
	    default))
    (setq iswitchb-text (or start ""))
    (unless matches-set
      (setq iswitchb-rescan t)
      (iswitchb-make-buflist iswitchb-default)
      (iswitchb-set-matches))
    (let
	((minibuffer-local-completion-map iswitchb-mode-map)
	 ;; Record the minibuffer depth that we expect to find once
	 ;; the minibuffer is set up and iswitchb-entryfn-p is called.
	 (iswitchb-minibuf-depth (1+ (minibuffer-depth)))
	 (iswitchb-require-match require-match))
      ;; prompt the user for the buffer name
      (setq iswitchb-final-text (completing-read
				 prompt		  ;the prompt
				 '(("dummy" . 1)) ;table
				 nil		  ;predicate
				 nil ;require-match [handled elsewhere]
				 start	;initial-contents
				 'iswitchb-history)))
    (if (and (not (eq iswitchb-exit 'usefirst))
	     (get-buffer iswitchb-final-text))
	;; This happens for example if the buffer was chosen with the mouse.
	(setq iswitchb-matches (list iswitchb-final-text)
	      iswitchb-virtual-buffers nil))

    ;; If no buffer matched, but a virtual buffer was selected, visit
    ;; that file now and act as though that buffer had been selected.
    (if (and iswitchb-virtual-buffers
	     (not (iswitchb-existing-buffer-p)))
	(let ((virt (car iswitchb-virtual-buffers))
	      (new-buf))
	  ;; Keep the name of the buffer returned by find-file-noselect, as
	  ;; the buffer 'virt' could be a symlink to a file of a different name.
	  (setq new-buf (buffer-name (find-file-noselect (cdr virt))))
	  (setq iswitchb-matches (list new-buf)
		iswitchb-virtual-buffers nil)))

    ;; Handling the require-match must be done in a better way.
    (if (and require-match
	     (not (iswitchb-existing-buffer-p)))
	(error "Must specify valid buffer"))

    (if (or (eq iswitchb-exit 'takeprompt)
	    (null iswitchb-matches))
	(setq buf-sel iswitchb-final-text)
      ;; else take head of list
      (setq buf-sel (car iswitchb-matches)))

    ;; Or possibly choose the default buffer
    (if  (equal iswitchb-final-text "")
	(setq buf-sel (car iswitchb-matches)))

    buf-sel))

(defun iswitchb-existing-buffer-p ()
  "Return non-nil if there is a matching buffer."
  (not (null iswitchb-matches)))

;;; COMPLETION CODE

(defun iswitchb-set-common-completion  ()
  "Find common completion of `iswitchb-text' in `iswitchb-matches'.
The result is stored in `iswitchb-common-match-string'."

  (let (val)
    (setq  iswitchb-common-match-string nil)
    (if (and iswitchb-matches
	     (not iswitchb-regexp) ;; testing
             (stringp iswitchb-text)
             (> (length iswitchb-text) 0))
        (if (setq val (iswitchb-find-common-substring
                       iswitchb-matches iswitchb-text))
            (setq iswitchb-common-match-string val)))
    val))

(defun iswitchb-complete ()
  "Try and complete the current pattern amongst the buffer names."
  (interactive)
  (let (res)
    (cond ((not  iswitchb-matches)
	   (run-hooks 'iswitchb-cannot-complete-hook))
          (iswitchb-invalid-regexp
           ;; Do nothing
           )
	  ((= 1 (length iswitchb-matches))
	   ;; only one choice, so select it.
	   (exit-minibuffer))

	  (t
	   ;; else there could be some completions
	   (setq res iswitchb-common-match-string)
	   (if (and (not (memq res '(t nil)))
		    (not (equal res iswitchb-text)))
	       ;; found something to complete, so put it in the minibuffer.
	       (progn
		 (setq iswitchb-rescan nil
                       iswitchb-common-match-inserted t)
		 (delete-region (minibuffer-prompt-end) (point))
		 (insert  res))
	     ;; else nothing to complete
	     (run-hooks 'iswitchb-cannot-complete-hook)
	     )))))

;;; TOGGLE FUNCTIONS

(defun iswitchb-toggle-case ()
  "Toggle the value of variable `iswitchb-case'."
  (interactive)
  (setq iswitchb-case (not iswitchb-case))
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t))

(defun iswitchb-toggle-regexp ()
  "Toggle the value of `iswitchb-regexp'."
  (interactive)
  (setq iswitchb-regexp (not iswitchb-regexp))
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t))

(defun iswitchb-toggle-ignore ()
  "Toggle ignoring buffers specified with `iswitchb-buffer-ignore'."
  (interactive)
  (if iswitchb-buffer-ignore
      (progn
        (setq iswitchb-buffer-ignore-orig iswitchb-buffer-ignore)
        (setq iswitchb-buffer-ignore nil))
    ;; else
    (setq iswitchb-buffer-ignore iswitchb-buffer-ignore-orig))
  (iswitchb-make-buflist iswitchb-default)
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t))

(defun iswitchb-exit-minibuffer ()
  "Exit minibuffer, but make sure we have a match if one is needed."
  (interactive)
  (if (or (not iswitchb-require-match)
	   (iswitchb-existing-buffer-p))
      (progn
	(setq iswitchb-exit 'usefirst)
	(throw 'exit nil))))

(defun iswitchb-select-buffer-text ()
  "Select the buffer named by the prompt.
If no buffer exactly matching the prompt exists, maybe create a new one."
  (interactive)
  (setq iswitchb-exit 'takeprompt)
  (exit-minibuffer))

(defun iswitchb-find-file ()
  "Drop into `find-file' from buffer switching."
  (interactive)
  (setq iswitchb-exit 'findfile)
  (exit-minibuffer))

(defvar recentf-list)

(defun iswitchb-next-match ()
  "Put first element of `iswitchb-matches' at the end of the list."
  (interactive)
  (let ((next  (cadr iswitchb-matches)))
    (if (and (null next) iswitchb-virtual-buffers)
	(setq recentf-list
	      (iswitchb-chop recentf-list
			     (cdr (cadr iswitchb-virtual-buffers))))
      (setq iswitchb-buflist (iswitchb-chop iswitchb-buflist next)))
    (setq iswitchb-rescan t)))

(defun iswitchb-prev-match ()
  "Put last element of `iswitchb-matches' at the front of the list."
  (interactive)
  (let ((prev  (car (last iswitchb-matches))))
    (if (and (null prev) iswitchb-virtual-buffers)
	(setq recentf-list
	      (iswitchb-chop recentf-list
			     (cdr (car (last iswitchb-virtual-buffers)))))
      (setq iswitchb-buflist (iswitchb-chop iswitchb-buflist prev)))
    (setq iswitchb-rescan t)))

(defun iswitchb-chop (list elem)
  "Remove all elements before ELEM and put them at the end of LIST."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car list))
      (if (equal next elem)
	  (setq ret (append list (nreverse sofar)))
	;; else
	(progn
	  (setq list (cdr list))
	  (setq sofar (cons next sofar)))))
    ret))

;;; CREATE LIST OF ALL CURRENT BUFFERS

(defun iswitchb-make-buflist (default)
  "Set `iswitchb-buflist' to the current list of buffers.
Currently visible buffers are put at the end of the list.
The hook `iswitchb-make-buflist-hook' is run after the list has been
created to allow the user to further modify the order of the buffer names
in this list.  If DEFAULT is non-nil, and corresponds to an existing buffer,
it is put to the start of the list."
  (setq iswitchb-buflist
	(let* ((iswitchb-current-buffers (iswitchb-get-buffers-in-frames))
	       (iswitchb-temp-buflist
		(delq nil
		      (mapcar
		       (lambda (x)
			 (let ((b-name (buffer-name x)))
			   (if (not
				(or
				 (iswitchb-ignore-buffername-p b-name)
				 (memq b-name iswitchb-current-buffers)))
			       b-name)))
		       (buffer-list (and iswitchb-use-frame-buffer-list
					 (selected-frame)))))))
	  (setq iswitchb-temp-buflist
		(nconc iswitchb-temp-buflist iswitchb-current-buffers))
	  (run-hooks 'iswitchb-make-buflist-hook)
	 ;; Should this be after the hooks, or should the hooks be the
	  ;; final thing to be run?
	  (if default
	      (progn
		(setq iswitchb-temp-buflist
		      (delete default iswitchb-temp-buflist))
		(setq iswitchb-temp-buflist
		      (cons default iswitchb-temp-buflist))))
	  iswitchb-temp-buflist)))

(defun iswitchb-to-end (lst)
  "Move the elements from LST to the end of `iswitchb-temp-buflist'."
  (dolist (elem lst)
    (setq iswitchb-temp-buflist (delq elem iswitchb-temp-buflist)))
  (setq iswitchb-temp-buflist (nconc iswitchb-temp-buflist lst)))

(defun iswitchb-get-buffers-in-frames (&optional current)
  "Return the list of buffers that are visible in the current frame.
If optional argument CURRENT is given, restrict searching to the
current frame, rather than all frames, regardless of value of
`iswitchb-all-frames'."
  (let ((iswitchb-bufs-in-frame nil))
    (walk-windows 'iswitchb-get-bufname nil
		  (if current
		      nil
		    iswitchb-all-frames))
    iswitchb-bufs-in-frame))

(defun iswitchb-get-bufname (win)
  "Used by `iswitchb-get-buffers-in-frames' to walk through all windows."
  (let ((buf (buffer-name (window-buffer win))))
	(if (not (member buf iswitchb-bufs-in-frame))
	    ;; Only add buf if it is not already in list.
	    ;; This prevents same buf in two different windows being
	    ;; put into the list twice.
	    (setq iswitchb-bufs-in-frame
		  (cons buf iswitchb-bufs-in-frame)))))

;;; FIND MATCHING BUFFERS

(defun iswitchb-set-matches ()
  "Set `iswitchb-matches' to the list of buffers matching prompt."
  (if iswitchb-rescan
      (setq iswitchb-matches
	    (let ((buflist iswitchb-buflist))
	      (iswitchb-get-matched-buffers iswitchb-text iswitchb-regexp
					    buflist))
	    iswitchb-virtual-buffers nil)))

(defun iswitchb-get-matched-buffers (regexp
				     &optional string-format buffer-list)
  "Return buffers matching REGEXP.
If STRING-FORMAT is nil, consider REGEXP as just a string.
BUFFER-LIST can be list of buffers or list of strings."
  (let ((case-fold-search (iswitchb-case))
         name ret)
    (if (null string-format) (setq regexp (regexp-quote regexp)))
    (setq iswitchb-invalid-regexp nil)
    (condition-case error
        (dolist (x buffer-list (nreverse ret))
          (setq name (if (stringp x) x (buffer-name x)))
          (when (and (string-match regexp name)
                     (not (iswitchb-ignore-buffername-p name)))
            (push name ret)))
      (invalid-regexp
       (setq iswitchb-invalid-regexp t)
       (cdr error)))))

(defun iswitchb-ignore-buffername-p (bufname)
  "Return t if the buffer BUFNAME should be ignored."
  (let ((data       (match-data))
        (re-list    iswitchb-buffer-ignore)
        ignorep
        nextstr)
    (while re-list
      (setq nextstr (car re-list))
      (cond
       ((stringp nextstr)
        (if (string-match nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil))))
       ((functionp nextstr)
        (if (funcall nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil)))))
      (setq re-list (cdr re-list)))
    (set-match-data data)

    ;; return the result
    ignorep))

(defun iswitchb-word-matching-substring (word)
  "Return part of WORD before 1st match to `iswitchb-change-word-sub'.
If `iswitchb-change-word-sub' cannot be found in WORD, return nil."
  (let ((case-fold-search (iswitchb-case)))
    (let ((m (string-match iswitchb-change-word-sub word)))
      (if m
          (substring word m)
        ;; else no match
        nil))))

(defun iswitchb-find-common-substring (lis subs)
  "Return common string following SUBS in each element of LIS."
  (let (res
        alist
        iswitchb-change-word-sub)
    (setq iswitchb-change-word-sub
          (if iswitchb-regexp
              subs
            (regexp-quote subs)))
    (setq res (mapcar 'iswitchb-word-matching-substring lis))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar 'iswitchb-makealist res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case (iswitchb-case)))

    (try-completion subs alist))))

(defun iswitchb-makealist (res)
  "Return dotted pair (RES . 1)."
  (cons res 1))

;; from Wayne Mesard <wmesard@esd.sgi.com>
(defun iswitchb-rotate-list (lis)
  "Destructively remove the last element from LIS.
Return the modified list with the last element prepended to it."
  (if (<= (length lis) 1)
      lis
    (let ((las lis)
          (prev lis))
      (while (consp (cdr las))
        (setq prev las
              las (cdr las)))
      (setcdr prev nil)
      (cons (car las) lis))))

(defun iswitchb-completion-help ()
  "Show possible completions in a *Completions* buffer."
  ;; we could allow this buffer to be used to select match, but I think
  ;; choose-completion-string will need redefining, so it just inserts
  ;; choice with out any previous input.
  (interactive)
  (setq iswitchb-rescan nil)
  (let ((buf (current-buffer))
	(temp-buf "*Completions*")
	(win))

    (if (and (eq last-command this-command)
             (not iswitchb-common-match-inserted))
	;; scroll buffer
	(progn
	  (set-buffer temp-buf)
	  (setq win (get-buffer-window temp-buf))
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win (point-min))
	    (scroll-other-window))
	  (set-buffer buf))

      (with-output-to-temp-buffer temp-buf
	(if (featurep 'xemacs)

	    ;; XEmacs extents are put on by default, doesn't seem to be
	    ;; any way of switching them off.
	    (display-completion-list (or iswitchb-matches iswitchb-buflist)
				     :help-string "iswitchb "
				     :activate-callback
				     (lambda (_x _y _z)
				       (message "doesn't work yet, sorry!")))
	  ;; else running Emacs
	  (display-completion-list (or iswitchb-matches iswitchb-buflist))))
      (setq iswitchb-common-match-inserted nil))))

;;; KILL CURRENT BUFFER

(defun iswitchb-kill-buffer ()
  "Kill the buffer at the head of `iswitchb-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        buf)

    (setq buf (car iswitchb-matches))
    ;; check to see if buf is non-nil.
    (if buf
	(let ((bufobjs (mapcar (lambda (name)
				 (or (get-buffer name) name))
			       iswitchb-buflist)))
	  (kill-buffer buf)

	  ;; Check if buffer exists.  XEmacs gnuserv.el makes alias
	  ;; for kill-buffer which does not return t if buffer is
	  ;; killed, so we can't rely on kill-buffer return value.
	  (if (get-buffer buf)
	      ;; buffer couldn't be killed.
	      (setq iswitchb-rescan t)
	    ;; Else `kill-buffer' succeeds so re-make the buffer list
	    ;; taking into account packages like uniquify may rename
	    ;; buffers, and try to preserve the ordering of buffers.
	    (setq iswitchb-buflist
		  (delq nil (mapcar (lambda (b)
				      (if (bufferp b)
					  (buffer-name b)
					b))
				    bufobjs))))))))

;;; VISIT CHOSEN BUFFER
(defun iswitchb-visit-buffer (buffer)
  "Visit buffer named BUFFER according to `iswitchb-method'."
  (let (win newframe)
    (cond
     ((eq iswitchb-method 'samewindow)
      (switch-to-buffer buffer))

     ((memq iswitchb-method '(always-frame maybe-frame))
      (cond
       ((and (setq win (iswitchb-window-buffer-p buffer))
	     (or (eq iswitchb-method 'always-frame)
		 (y-or-n-p "Jump to frame? ")))
	(setq newframe (window-frame win))
        (if (fboundp 'select-frame-set-input-focus)
            (select-frame-set-input-focus newframe)
          (raise-frame newframe)
          (select-frame newframe)
          )
	(select-window win))
       (t
	;;  No buffer in other frames...
	(switch-to-buffer buffer)
	)))

     ((eq iswitchb-method 'otherwindow)
      (switch-to-buffer-other-window buffer))

     ((eq iswitchb-method 'display)
      (display-buffer buffer))

     ((eq iswitchb-method 'otherframe)
      (progn
	(switch-to-buffer-other-frame buffer)
	(if (fboundp 'select-frame-set-input-focus)
            (select-frame-set-input-focus (selected-frame)))
	)))))

(defun iswitchb-possible-new-buffer (buf)
  "Possibly create and visit a new buffer called BUF."

  (let ((newbufcreated))
    (if (and iswitchb-newbuffer
	     (or
	      (not iswitchb-prompt-newbuffer)

	      (and iswitchb-prompt-newbuffer
		   (y-or-n-p
		    (format
		     "No buffer matching `%s', create one? "
		     buf)))))
	;; then create a new buffer
	(progn
	  (setq newbufcreated (get-buffer-create buf))
	  (if (fboundp 'set-buffer-major-mode)
	      (set-buffer-major-mode newbufcreated))
	  (iswitchb-visit-buffer newbufcreated))
      ;; else won't create new buffer
      (message "no buffer matching `%s'" buf))))

(defun iswitchb-window-buffer-p  (buffer)
  "Return window pointer if BUFFER is visible in another frame.
If BUFFER is visible in the current frame, return nil."
  (interactive)
  (let ((blist (iswitchb-get-buffers-in-frames 'current)))
    ;; If the buffer is visible in current frame, return nil
    (unless (member buffer blist)
      ;; maybe in other frame or icon
      (get-buffer-window buffer 0) ; better than 'visible
      )))

(defun iswitchb-buffer ()
  "Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method iswitchb-default-method)
  (iswitchb))

(defun iswitchb-buffer-other-window ()
  "Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method 'otherwindow)
  (iswitchb))

(defun iswitchb-display-buffer ()
  "Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method 'display)
  (iswitchb))

(defun iswitchb-buffer-other-frame ()
  "Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method 'otherframe)
  (iswitchb))

;;; XEmacs hack for showing default buffer

;; The first time we enter the minibuffer, Emacs puts up the default
;; buffer to switch to, but XEmacs doesn't -- presumably there is a
;; subtle difference in the two versions of post-command-hook.  The
;; default is shown for both whenever we delete all of our text
;; though, indicating its just a problem the first time we enter the
;; function.  To solve this, we use another entry hook for emacs to
;; show the default the first time we enter the minibuffer.

(defun iswitchb-init-XEmacs-trick ()
  "Display default buffer when first entering minibuffer.
This is a hack for XEmacs, and should really be handled by `iswitchb-exhibit'."
  (if (iswitchb-entryfn-p)
      (progn
	(iswitchb-exhibit)
	(goto-char (point-min)))))

;; add this hook for XEmacs only.
(if (featurep 'xemacs)
    (add-hook 'iswitchb-minibuffer-setup-hook
	      'iswitchb-init-XEmacs-trick))

;;; XEmacs / backspace key
;; For some reason, if the backspace key is pressed in XEmacs, the
;; line gets confused, so I've added a simple key definition to make
;; backspace act like the normal delete key.

(defun iswitchb-xemacs-backspacekey ()
  "Bind backspace to `backward-delete-char'."
  (define-key iswitchb-mode-map '[backspace] 'backward-delete-char)
  (define-key iswitchb-mode-map '[(meta backspace)] 'backward-kill-word))

(if (featurep 'xemacs)
    (add-hook 'iswitchb-define-mode-map-hook
	      'iswitchb-xemacs-backspacekey))

;;; ICOMPLETE TYPE CODE

(defun iswitchb-exhibit ()
  "Find matching buffers and display a list in the minibuffer.
Copied from `icomplete-exhibit' with two changes:
1. It prints a default buffer name when there is no text yet entered.
2. It calls my completion routine rather than the standard completion."
  (if iswitchb-use-mycompletion
      (let ((contents (buffer-substring (minibuffer-prompt-end) (point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'iswitchb-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'iswitchb-eoinput))
	  (setq iswitchb-eoinput (point))
	  ;; Update the list of matches
	  (setq iswitchb-text contents)
	  (iswitchb-set-matches)
	  (setq iswitchb-rescan t)
	  (iswitchb-set-common-completion)

	  ;; Insert the match-status information:
	  (insert (iswitchb-completions
		   contents))))))

(defvar most-len)
(defvar most-is-exact)

(defun iswitchb-output-completion (com)
  (if (= (length com) most-len)
      ;; Most is one exact match,
      ;; note that and leave out
      ;; for later indication:
      (ignore
       (setq most-is-exact t))
    (substring com most-len)))

(defun iswitchb-completions (name)
  "Return the string that is displayed after the user's text.
Modified from `icomplete-completions'."

  (let ((comps iswitchb-matches)
                                        ; "-determined" - only one candidate
        (open-bracket-determined "[")
        (close-bracket-determined "]")
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
	first)

    (if (and iswitchb-use-faces comps)
	(progn
	  (setq first (car comps))
	  (setq first (format "%s" first))
	  (put-text-property 0 (length first) 'face
			     (if (= (length comps) 1)
                                 (if iswitchb-invalid-regexp
                                     'iswitchb-invalid-regexp
                                   'iswitchb-single-match)
			       'iswitchb-current-match)
			     first)
	  (setq comps  (cons first (cdr comps)))))

    ;; If no buffers matched, and virtual buffers are being used, then
    ;; consult the list of past visited files, to see if we can find
    ;; the file which the user might thought was still open.
    (when (and iswitchb-use-virtual-buffers (null comps)
	       recentf-list)
      (setq iswitchb-virtual-buffers nil)
      (let ((head recentf-list) name)
	(while head
	  (if (and (setq name (file-name-nondirectory (car head)))
		   (string-match (if iswitchb-regexp
				     iswitchb-text
				   (regexp-quote iswitchb-text)) name)
		   (null (get-file-buffer (car head)))
		   (not (assoc name iswitchb-virtual-buffers))
		   (not (iswitchb-ignore-buffername-p name))
		   (file-exists-p (car head)))
	      (setq iswitchb-virtual-buffers
		    (cons (cons name (car head))
			  iswitchb-virtual-buffers)))
	  (setq head (cdr head)))
	(setq iswitchb-virtual-buffers (nreverse iswitchb-virtual-buffers)
	      comps (mapcar 'car iswitchb-virtual-buffers))
	(let ((comp comps))
	  (while comp
	    (put-text-property 0 (length (car comp))
			       'face 'iswitchb-virtual-matches
			       (car comp))
	    (setq comp (cdr comp))))))

    (cond ((null comps) (format " %sNo match%s"
				open-bracket-determined
				close-bracket-determined))

	  (iswitchb-invalid-regexp
           (concat " " (car comps)))
          ((null (cdr comps))		;one match
	   (concat
            (if (if (not iswitchb-regexp)
                    (= (length name)
                       (length (car comps)))
                  (string-match name (car comps))
                  (string-equal (match-string 0 (car comps))
                                (car comps)))
                ""
              (concat open-bracket-determined
			       ;; when there is one match, show the
			       ;; matching buffer name in full
			       (car comps)
			       close-bracket-determined))
		   (if (not iswitchb-use-faces) " [Matched]")))
	  (t				;multiple matches
	   (if (and iswitchb-max-to-show
		    (> (length comps) iswitchb-max-to-show))
	       (setq comps
		     (append
		      (let ((res nil)
			    (comp comps)
			    (end (/ iswitchb-max-to-show 2)))
			(while (>= (setq end (1- end)) 0)
			  (setq res (cons (car comp) res)
				comp (cdr comp)))
			(nreverse res))
		      (list "...")
		      (nthcdr (- (length comps)
				 (/ iswitchb-max-to-show 2)) comps))))
	   (let* (
		  ;;(most (try-completion name candidates predicate))
		  (most nil)
		  (most-len (length most))
		  most-is-exact
		  (alternatives
		   (mapconcat (if most 'iswitchb-output-completion
				'identity) comps iswitchb-delim)))

	     (concat

	      ;; put in common completion item -- what you get by
	      ;; pressing tab
	      (if (and (stringp iswitchb-common-match-string)
		       (> (length iswitchb-common-match-string) (length name)))
		  (concat open-bracket-determined
			  (substring iswitchb-common-match-string
				     (length name))
			  close-bracket-determined))
	      ;; end of partial matches...

	      ;; think this bit can be ignored.
	      (and (> most-len (length name))
		   (concat open-bracket-determined
			   (substring most (length name))
			   close-bracket-determined))

	      ;; list all alternatives
	      open-bracket-prospects
	      (if most-is-exact
		  (concat iswitchb-delim alternatives)
		alternatives)
	      close-bracket-prospects))))))

(defun iswitchb-minibuffer-setup ()
  "Set up minibuffer for `iswitchb-buffer'.
Copied from `icomplete-minibuffer-setup-hook'."
  (when (iswitchb-entryfn-p)
    (set (make-local-variable 'iswitchb-use-mycompletion) t)
    (add-hook 'pre-command-hook 'iswitchb-pre-command nil t)
    (add-hook 'post-command-hook 'iswitchb-post-command nil t)
    (run-hooks 'iswitchb-minibuffer-setup-hook)))

(defun iswitchb-pre-command ()
  "Run before command in `iswitchb-buffer'."
  (iswitchb-tidy))

(defun iswitchb-post-command ()
  "Run after command in `iswitchb-buffer'."
  (iswitchb-exhibit))

(defun iswitchb-tidy ()
  "Remove completions display, if any, prior to new user input.
Copied from `icomplete-tidy'."

  (if (and (boundp 'iswitchb-eoinput)
	   iswitchb-eoinput)

      (if (> iswitchb-eoinput (point-max))
	  ;; Oops, got rug pulled out from under us - reinit:
	  (setq iswitchb-eoinput (point-max))
	(let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	  (delete-region iswitchb-eoinput (point-max))))

    ;; Reestablish the local variable 'cause minibuffer-setup is weird:
    (make-local-variable 'iswitchb-eoinput)
    (setq iswitchb-eoinput 1)))

(defun iswitchb-entryfn-p ()
  "Return non-nil if we are using `iswitchb-buffer'."
  (eq iswitchb-minibuf-depth (minibuffer-depth)))

(defun iswitchb-summaries-to-end ()
  "Move the summaries to the end of the list.
This is an example function which can be hooked on to
`iswitchb-make-buflist-hook'.  Any buffer matching the regexps
`Summary' or `output\*$'are put to the end of the list."
  (let ((summaries (delq nil
			 (mapcar
			  (lambda (x)
			    (if (string-match "Summary\\|output\\*$" x)
				x))
			  iswitchb-temp-buflist))))
    (iswitchb-to-end summaries)))

(defun iswitchb-case ()
  "Return non-nil if we should ignore case when matching.
See the variable `iswitchb-case' for details."
  (if iswitchb-case
      (if (featurep 'xemacs)
	  (isearch-no-upper-case-p iswitchb-text)
	(isearch-no-upper-case-p iswitchb-text t))))

;;;###autoload
(define-minor-mode iswitchb-mode
  "Toggle Iswitchb mode.
With a prefix argument ARG, enable Iswitchb mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Iswitchb mode is a global minor mode that enables switching
between buffers using substrings.  See `iswitchb' for details."
  nil nil iswitchb-global-map :global t :group 'iswitchb
  (if iswitchb-mode
      (add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)))

(provide 'iswitchb)

;;; iswitchb.el ends here
