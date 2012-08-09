;;; isearch.el --- incremental search minor mode

;; Copyright (C) 1992-1997, 1999-2012  Free Software Foundation, Inc.

;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>
;; Maintainer: FSF
;; Keywords: matching
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

;; Instructions

;; For programmed use of isearch-mode, e.g. calling (isearch-forward),
;; isearch-mode behaves modally and does not return until the search
;; is completed.  It uses a recursive-edit to behave this way.

;; The key bindings active within isearch-mode are defined below in
;; `isearch-mode-map' which is given bindings close to the default
;; characters of the original isearch.el.  With `isearch-mode',
;; however, you can bind multi-character keys and it should be easier
;; to add new commands.  One bug though: keys with meta-prefix cannot
;; be longer than two chars.  Also see minibuffer-local-isearch-map
;; for bindings active during `isearch-edit-string'.

;; isearch-mode should work even if you switch windows with the mouse,
;; in which case isearch-mode is terminated automatically before the
;; switch.

;; The search ring and completion commands automatically put you in
;; the minibuffer to edit the string.  This gives you a chance to
;; modify the search string before executing the search.  There are
;; three commands to terminate the editing: C-s and C-r exit the
;; minibuffer and search forward and reverse respectively, while C-m
;; exits and does a nonincremental search.

;; Exiting immediately from isearch uses isearch-edit-string instead
;; of nonincremental-search, if search-nonincremental-instead is non-nil.
;; The name of this option should probably be changed if we decide to
;; keep the behavior.  No point in forcing nonincremental search until
;; the last possible moment.

;;; Code:


;; Some additional options and constants.

(defgroup isearch nil
  "Incremental search minor mode."
  :link '(emacs-commentary-link "isearch")
  :link '(custom-manual "(emacs)Incremental Search")
  :prefix "isearch-"
  :prefix "search-"
  :group 'matching)


(defcustom search-exit-option t
  "Non-nil means random control characters terminate incremental search."
  :type 'boolean
  :group 'isearch)

(defcustom search-slow-window-lines 1
  "Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines."
  :type 'integer
  :group 'isearch)

(defcustom search-slow-speed 1200
  "Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached."
  :type 'integer
  :group 'isearch)

(defcustom search-upper-case 'not-yanks
  "If non-nil, upper case chars disable case fold searching.
That is, upper and lower case chars must match exactly.
This applies no matter where the chars come from, but does not
apply to chars in regexps that are prefixed with `\\'.
If this value is `not-yanks', text yanked into the search string
in Isearch mode is always downcased."
  :type '(choice (const :tag "off" nil)
		 (const not-yanks)
		 (other :tag "on" t))
  :group 'isearch)

(defcustom search-nonincremental-instead t
  "If non-nil, do a nonincremental search instead of exiting immediately.
Actually, `isearch-edit-string' is called to let you enter the search
string, and RET terminates editing and does a nonincremental search."
  :type 'boolean
  :group 'isearch)

(defcustom search-whitespace-regexp (purecopy "\\s-+")
  "If non-nil, regular expression to match a sequence of whitespace chars.
This applies to regular expression incremental search.
When you put a space or spaces in the incremental regexp, it stands for
this, unless it is inside of a regexp construct such as [...] or *, + or ?.
You might want to use something like \"[ \\t\\r\\n]+\" instead.
In the Customization buffer, that is `[' followed by a space,
a tab, a carriage return (control-M), a newline, and `]+'.

When this is nil, each space you type matches literally, against one space."
  :type '(choice (const :tag "Find Spaces Literally" nil)
		 regexp)
  :group 'isearch)

(defcustom search-invisible 'open
  "If t incremental search can match hidden text.
A nil value means don't match invisible text.
When the value is `open', if the text matched is made invisible by
an overlay having an `invisible' property and that overlay has a property
`isearch-open-invisible', then incremental search will show the contents.
\(This applies when using `outline.el' and `hideshow.el'.)
See also `reveal-mode' if you want overlays to automatically be opened
whenever point is in one of them."
  :type '(choice (const :tag "Match hidden text" t)
		 (const :tag "Open overlays" open)
		 (const :tag "Don't match hidden text" nil))
  :group 'isearch)

(defcustom isearch-hide-immediately t
  "If non-nil, re-hide an invisible match right away.
This variable makes a difference when `search-invisible' is set to `open'.
It means that after search makes some invisible text visible
to show the match, it makes the text invisible again when the match moves.
Ordinarily the text becomes invisible again at the end of the search."
  :type 'boolean
  :group 'isearch)

(defcustom isearch-resume-in-command-history nil
  "If non-nil, `isearch-resume' commands are added to the command history.
This allows you to resume earlier Isearch sessions through the
command history."
  :type 'boolean
  :group 'isearch)

(defvar isearch-mode-hook nil
  "Function(s) to call after starting up an incremental search.")

(defvar isearch-update-post-hook nil
  "Function(s) to call after isearch has found matches in the buffer.")

(defvar isearch-mode-end-hook nil
  "Function(s) to call after terminating an incremental search.
When these functions are called, `isearch-mode-end-hook-quit'
is non-nil if the user quits the search.")

(defvar isearch-mode-end-hook-quit nil
  "Non-nil while running `isearch-mode-end-hook' if the user quits the search.")

(defvar isearch-message-function nil
  "Function to call to display the search prompt.
If nil, use `isearch-message'.")

(defvar isearch-wrap-function nil
  "Function to call to wrap the search when search is failed.
If nil, move point to the beginning of the buffer for a forward search,
or to the end of the buffer for a backward search.")

(defvar isearch-push-state-function nil
  "Function to save a function restoring the mode-specific Isearch state
to the search status stack.")

(defvar isearch-filter-predicate 'isearch-filter-visible
  "Predicate that filters the search hits that would normally be available.
Search hits that dissatisfy the predicate are skipped.  The function
has two arguments: the positions of start and end of text matched by
the search.  If this function returns nil, continue searching without
stopping at this match.")

;; Search ring.

(defvar search-ring nil
  "List of search string sequences.")
(defvar regexp-search-ring nil
  "List of regular expression search string sequences.")

(defcustom search-ring-max 16
  "Maximum length of search ring before oldest elements are thrown away."
  :type 'integer
  :group 'isearch)
(defcustom regexp-search-ring-max 16
  "Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'isearch)

(defvar search-ring-yank-pointer nil
  "Index in `search-ring' of last string reused.
It is nil if none yet.")
(defvar regexp-search-ring-yank-pointer nil
  "Index in `regexp-search-ring' of last string reused.
It is nil if none yet.")

(defcustom search-ring-update nil
  "Non-nil if advancing or retreating in the search ring should cause search.
Default value, nil, means edit the string instead."
  :type 'boolean
  :group 'isearch)

;;; isearch highlight customization.

(defcustom search-highlight t
  "Non-nil means incremental search highlights the current match."
  :type 'boolean
  :group 'isearch)

(defface isearch
  '((((class color) (min-colors 88) (background light))
     ;; The background must not be too dark, for that means
     ;; the character is hard to see when the cursor is there.
     (:background "magenta3" :foreground "lightskyblue1"))
    (((class color) (min-colors 88) (background dark))
     (:background "palevioletred2" :foreground "brown4"))
    (((class color) (min-colors 16))
     (:background "magenta4" :foreground "cyan1"))
    (((class color) (min-colors 8))
     (:background "magenta4" :foreground "cyan1"))
    (t (:inverse-video t)))
  "Face for highlighting Isearch matches."
  :group 'isearch
  :group 'basic-faces)
(defvar isearch-face 'isearch)

(defface isearch-fail
  '((((class color) (min-colors 88) (background light))
     (:background "RosyBrown1"))
    (((class color) (min-colors 88) (background dark))
     (:background "red4"))
    (((class color) (min-colors 16))
     (:background "red"))
    (((class color) (min-colors 8))
     (:background "red"))
    (((class color grayscale))
     :foreground "grey")
    (t (:inverse-video t)))
  "Face for highlighting failed part in Isearch echo-area message."
  :version "23.1"
  :group 'isearch)

(defcustom isearch-lazy-highlight t
  "Controls the lazy-highlighting during incremental search.
When non-nil, all text in the buffer matching the current search
string is highlighted lazily (see `lazy-highlight-initial-delay'
and `lazy-highlight-interval')."
  :type 'boolean
  :group 'lazy-highlight
  :group 'isearch)

;;; Lazy highlight customization.

(defgroup lazy-highlight nil
  "Lazy highlighting feature for matching strings."
  :prefix "lazy-highlight-"
  :version "21.1"
  :group 'isearch
  :group 'matching)

(define-obsolete-variable-alias 'isearch-lazy-highlight-cleanup
                                'lazy-highlight-cleanup
                                "22.1")

(defcustom lazy-highlight-cleanup t
  "Controls whether to remove extra highlighting after a search.
If this is nil, extra highlighting can be \"manually\" removed with
\\[lazy-highlight-cleanup]."
  :type 'boolean
  :group 'lazy-highlight)

(define-obsolete-variable-alias 'isearch-lazy-highlight-initial-delay
                                'lazy-highlight-initial-delay
                                "22.1")

(defcustom lazy-highlight-initial-delay 0.25
  "Seconds to wait before beginning to lazily highlight all matches."
  :type 'number
  :group 'lazy-highlight)

(define-obsolete-variable-alias 'isearch-lazy-highlight-interval
                                'lazy-highlight-interval
                                "22.1")

(defcustom lazy-highlight-interval 0 ; 0.0625
  "Seconds between lazily highlighting successive matches."
  :type 'number
  :group 'lazy-highlight)

(define-obsolete-variable-alias 'isearch-lazy-highlight-max-at-a-time
                                'lazy-highlight-max-at-a-time
                                "22.1")

(defcustom lazy-highlight-max-at-a-time 20
  "Maximum matches to highlight at a time (for `lazy-highlight').
Larger values may reduce Isearch's responsiveness to user input;
smaller values make matches highlight slowly.
A value of nil means highlight all matches."
  :type '(choice (const :tag "All" nil)
		 (integer :tag "Some"))
  :group 'lazy-highlight)

(defface lazy-highlight
  '((((class color) (min-colors 88) (background light))
     (:background "paleturquoise"))
    (((class color) (min-colors 88) (background dark))
     (:background "paleturquoise4"))
    (((class color) (min-colors 16))
     (:background "turquoise3"))
    (((class color) (min-colors 8))
     (:background "turquoise3"))
    (t (:underline t)))
  "Face for lazy highlighting of matches other than the current one."
  :group 'lazy-highlight
  :group 'basic-faces)
(define-obsolete-face-alias 'isearch-lazy-highlight-face 'lazy-highlight "22.1")
(define-obsolete-variable-alias 'isearch-lazy-highlight-face
                                'lazy-highlight-face
                                "22.1")
(defvar lazy-highlight-face 'lazy-highlight)

;; Define isearch help map.

(defvar isearch-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'isearch-other-control-char)
    (define-key map (char-to-string help-char) 'isearch-help-for-help)
    (define-key map [help] 'isearch-help-for-help)
    (define-key map [f1] 'isearch-help-for-help)
    (define-key map "?" 'isearch-help-for-help)
    (define-key map "b" 'isearch-describe-bindings)
    (define-key map "k" 'isearch-describe-key)
    (define-key map "m" 'isearch-describe-mode)
    (define-key map "q" 'help-quit)
    map)
  "Keymap for characters following the Help key for Isearch mode.")

(eval-when-compile (require 'help-macro))

(make-help-screen isearch-help-for-help-internal
  (purecopy "Type a help option: [bkm] or ?")
  "You have typed %THIS-KEY%, the help character.  Type a Help option:
\(Type \\<help-map>\\[help-quit] to exit the Help command.)

b           Display all Isearch key bindings.
k KEYS      Display full documentation of Isearch key sequence.
m           Display documentation of Isearch mode.

You can't type here other help keys available in the global help map,
but outside of this help window when you type them in Isearch mode,
they exit Isearch mode before displaying global help."
  isearch-help-map)

(defun isearch-help-for-help ()
  "Display Isearch help menu."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (isearch-help-for-help-internal))
  (isearch-update))

(defun isearch-describe-bindings ()
  "Show a list of all keys defined in Isearch mode, and their definitions.
This is like `describe-bindings', but displays only Isearch keys."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (with-help-window "*Help*"
      (with-current-buffer standard-output
	(princ "Isearch Mode Bindings:\n")
	(princ (substitute-command-keys "\\{isearch-mode-map}"))))))

(defun isearch-describe-key ()
  "Display documentation of the function invoked by isearch key."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (call-interactively 'describe-key))
  (isearch-update))

(defun isearch-describe-mode ()
  "Display documentation of Isearch mode."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (describe-function 'isearch-forward))
  (isearch-update))

(defalias 'isearch-mode-help 'isearch-describe-mode)


;; Define isearch-mode keymap.

(defvar isearch-mode-map
  (let ((i 0)
	(map (make-keymap)))
    (or (char-table-p (nth 1 map))
	(error "The initialization of isearch-mode-map must be updated"))
    ;; Make all multibyte characters search for themselves.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
			  'isearch-printing-char)
    ;; Make function keys, etc, which aren't bound to a scrolling-function
    ;; exit the search.
    (define-key map [t] 'isearch-other-control-char)
    ;; Control chars, by default, end isearch mode transparently.
    ;; We need these explicit definitions because, in a dense keymap,
    ;; the binding for t does not affect characters.
    ;; We use a dense keymap to save space.
    (while (< i ?\s)
      (define-key map (make-string 1 i) 'isearch-other-control-char)
      (setq i (1+ i)))

    ;; Single-byte printing chars extend the search string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'isearch-printing-char)
      (setq i (1+ i)))

    ;; To handle local bindings with meta char prefix keys, define
    ;; another full keymap.  This must be done for any other prefix
    ;; keys as well, one full keymap per char of the prefix key.  It
    ;; would be simpler to disable the global keymap, and/or have a
    ;; default local key binding for any key not otherwise bound.
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map)
      (define-key map [escape] meta-map))
    (define-key map (vector meta-prefix-char t) 'isearch-other-meta-char)

    ;; Several non-printing chars change the searching behavior.
    (define-key map "\C-s" 'isearch-repeat-forward)
    (define-key map "\C-r" 'isearch-repeat-backward)
    ;; Define M-C-s and M-C-r like C-s and C-r so that the same key
    ;; combinations can be used to repeat regexp isearches that can
    ;; be used to start these searches.
    (define-key map "\M-\C-s" 'isearch-repeat-forward)
    (define-key map "\M-\C-r" 'isearch-repeat-backward)
    (define-key map "\177" 'isearch-delete-char)
    (define-key map "\C-g" 'isearch-abort)

    ;; This assumes \e is the meta-prefix-char.
    (or (= ?\e meta-prefix-char)
	(error "Inconsistency in isearch.el"))
    (define-key map "\e\e\e" 'isearch-cancel)
    (define-key map  [escape escape escape] 'isearch-cancel)

    (define-key map "\C-q" 'isearch-quote-char)

    (define-key map "\r" 'isearch-exit)
    (define-key map "\C-j" 'isearch-printing-char)
    (define-key map "\t" 'isearch-printing-char)
    (define-key map [?\S-\ ] 'isearch-printing-char)

    (define-key map    "\C-w" 'isearch-yank-word-or-char)
    (define-key map "\M-\C-w" 'isearch-del-char)
    (define-key map "\M-\C-y" 'isearch-yank-char)
    (define-key map    "\C-y" 'isearch-yank-kill)
    (define-key map "\M-s\C-e" 'isearch-yank-line)

    (define-key map (char-to-string help-char) isearch-help-map)
    (define-key map [help] isearch-help-map)
    (define-key map [f1] isearch-help-map)

    (define-key map "\M-n" 'isearch-ring-advance)
    (define-key map "\M-p" 'isearch-ring-retreat)
    (define-key map "\M-y" 'isearch-yank-pop)

    (define-key map "\M-\t" 'isearch-complete)

    ;; Pass frame events transparently so they won't exit the search.
    ;; In particular, if we have more than one display open, then a
    ;; switch-frame might be generated by someone typing at another keyboard.
    (define-key map [switch-frame] nil)
    (define-key map [delete-frame] nil)
    (define-key map [iconify-frame] nil)
    (define-key map [make-frame-visible] nil)
    (define-key map [mouse-movement] nil)
    (define-key map [language-change] nil)

    ;; For searching multilingual text.
    (define-key map "\C-\\" 'isearch-toggle-input-method)
    (define-key map "\C-^" 'isearch-toggle-specified-input-method)

    ;; People expect to be able to paste with the mouse.
    (define-key map [mouse-2] #'isearch-mouse-2)
    (define-key map [down-mouse-2] nil)

    ;; Some bindings you may want to put in your isearch-mode-hook.
    ;; Suggest some alternates...
    (define-key map "\M-c" 'isearch-toggle-case-fold)
    (define-key map "\M-r" 'isearch-toggle-regexp)
    (define-key map "\M-e" 'isearch-edit-string)

    (define-key map "\M-sr" 'isearch-toggle-regexp)
    (define-key map "\M-sw" 'isearch-toggle-word)

    (define-key map [?\M-%] 'isearch-query-replace)
    (define-key map [?\C-\M-%] 'isearch-query-replace-regexp)
    (define-key map "\M-so" 'isearch-occur)
    (define-key map "\M-shr" 'isearch-highlight-regexp)

    map)
  "Keymap for `isearch-mode'.")

(defvar minibuffer-local-isearch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\r"    'isearch-nonincremental-exit-minibuffer)
    (define-key map "\M-\t" 'isearch-complete-edit)
    (define-key map "\C-s"  'isearch-forward-exit-minibuffer)
    (define-key map "\C-r"  'isearch-reverse-exit-minibuffer)
    (define-key map "\C-f"  'isearch-yank-char-in-minibuffer)
    (define-key map [right] 'isearch-yank-char-in-minibuffer)
    map)
  "Keymap for editing Isearch strings in the minibuffer.")

;; Internal variables declared globally for byte-compiler.
;; These are all set with setq while isearching
;; and bound locally while editing the search string.

(defvar isearch-forward nil)	; Searching in the forward direction.
(defvar isearch-regexp nil)	; Searching for a regexp.
(defvar isearch-word nil)	; Searching for words.
(defvar isearch-hidden nil) ; Non-nil if the string exists but is invisible.

(defvar isearch-cmds nil
  "Stack of search status sets.
Each set is a vector of the form:
 [STRING MESSAGE POINT SUCCESS FORWARD OTHER-END WORD
  INVALID-REGEXP WRAPPED BARRIER WITHIN-BRACKETS CASE-FOLD-SEARCH]")

(defvar isearch-string "")  ; The current search string.
(defvar isearch-message "") ; text-char-description version of isearch-string

(defvar isearch-message-prefix-add nil) ; Additional text for the message prefix
(defvar isearch-message-suffix-add nil) ; Additional text for the message suffix

(defvar isearch-success t)	; Searching is currently successful.
(defvar isearch-error nil)	; Error message for failed search.
(defvar isearch-other-end nil)	; Start (end) of match if forward (backward).
(defvar isearch-wrapped nil)	; Searching restarted from the top (bottom).
(defvar isearch-barrier 0
  "Recorded minimum/maximal point for the current search.")
(defvar isearch-just-started nil)
(defvar isearch-start-hscroll 0)	; hscroll when starting the search.

;; case-fold-search while searching.
;;   either nil, t, or 'yes.  'yes means the same as t except that mixed
;;   case in the search string is ignored.
(defvar isearch-case-fold-search nil)

(defvar isearch-last-case-fold-search nil)

;; Used to save default value while isearch is active
(defvar isearch-original-minibuffer-message-timeout nil)

(defvar isearch-adjusted nil)
(defvar isearch-slow-terminal-mode nil)
;; If t, using a small window.
(defvar isearch-small-window nil)
(defvar isearch-opoint 0)
;; The window configuration active at the beginning of the search.
(defvar isearch-window-configuration nil)

;; Flag to indicate a yank occurred, so don't move the cursor.
(defvar isearch-yank-flag nil)

;; A function to be called after each input character is processed.
;; (It is not called after characters that exit the search.)
;; It is only set from an optional argument to `isearch-mode'.
(defvar isearch-op-fun nil)

;;  Is isearch-mode in a recursive edit for modal searching.
(defvar isearch-recursive-edit nil)

;; Should isearch be terminated after doing one search?
(defvar isearch-nonincremental nil)

;; New value of isearch-forward after isearch-edit-string.
(defvar isearch-new-forward nil)

;; Accumulate here the overlays opened during searching.
(defvar isearch-opened-overlays nil)

;; The value of input-method-function when isearch is invoked.
(defvar isearch-input-method-function nil)

;; A flag to tell if input-method-function is locally bound when
;; isearch is invoked.
(defvar isearch-input-method-local-p nil)

;; Minor-mode-alist changes - kind of redundant with the
;; echo area, but if isearching in multiple windows, it can be useful.

(or (assq 'isearch-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(isearch-mode isearch-mode))))

(defvar isearch-mode nil) ;; Name of the minor mode, if non-nil.
(make-variable-buffer-local 'isearch-mode)

(define-key global-map "\C-s" 'isearch-forward)
(define-key esc-map "\C-s" 'isearch-forward-regexp)
(define-key global-map "\C-r" 'isearch-backward)
(define-key esc-map "\C-r" 'isearch-backward-regexp)
(define-key search-map "w" 'isearch-forward-word)

;; Entry points to isearch-mode.

(defun isearch-forward (&optional regexp-p no-recursive-edit)
  "\
Do incremental search forward.
With a prefix argument, do an incremental regular expression search instead.
\\<isearch-mode-map>
As you type characters, they add to the search string and are found.
The following non-printing keys are bound in `isearch-mode-map'.

Type \\[isearch-delete-char] to cancel last input item from end of search string.
Type \\[isearch-exit] to exit, leaving point at location found.
Type LFD (C-j) to match end of line.
Type \\[isearch-repeat-forward] to search again forward,\
 \\[isearch-repeat-backward] to search again backward.
Type \\[isearch-yank-word-or-char] to yank next word or character in buffer
  onto the end of the search string, and search for it.
Type \\[isearch-del-char] to delete character from end of search string.
Type \\[isearch-yank-char] to yank char from buffer onto end of search\
 string and search for it.
Type \\[isearch-yank-line] to yank rest of line onto end of search string\
 and search for it.
Type \\[isearch-yank-kill] to yank the last string of killed text.
Type \\[isearch-yank-pop] to replace string just yanked into search prompt
 with string killed before it.
Type \\[isearch-quote-char] to quote control character to search for it.
\\[isearch-abort] while searching or when search has failed cancels input\
 back to what has
 been found successfully.
\\[isearch-abort] when search is successful aborts and moves point to\
 starting point.

If you try to exit with the search string still empty, it invokes
 nonincremental search.

Type \\[isearch-toggle-case-fold] to toggle search case-sensitivity.
Type \\[isearch-toggle-regexp] to toggle regular-expression mode.
Type \\[isearch-toggle-word] to toggle word mode.
Type \\[isearch-edit-string] to edit the search string in the minibuffer.

Also supported is a search ring of the previous 16 search strings.
Type \\[isearch-ring-advance] to search for the next item in the search ring.
Type \\[isearch-ring-retreat] to search for the previous item in the search\
 ring.
Type \\[isearch-complete] to complete the search string using the search ring.

Type \\[isearch-query-replace] to run `query-replace' with string to\
 replace from last search string.
Type \\[isearch-query-replace-regexp] to run `query-replace-regexp'\
 with the last search string.
Type \\[isearch-occur] to run `occur' that shows\
 the last search string.
Type \\[isearch-highlight-regexp] to run `highlight-regexp'\
 that highlights the last search string.

Type \\[isearch-describe-bindings] to display all Isearch key bindings.
Type \\[isearch-describe-key] to display documentation of Isearch key.
Type \\[isearch-describe-mode] to display documentation of Isearch mode.

If an input method is turned on in the current buffer, that input
method is also active while you are typing characters to search.
To toggle the input method, type \\[isearch-toggle-input-method].  \
It also toggles the input
method in the current buffer.

To use a different input method for searching, type \
\\[isearch-toggle-specified-input-method],
and specify an input method you want to use.

The above keys, bound in `isearch-mode-map', are often controlled by
 options; do \\[apropos] on search-.* to find them.
Other control and meta characters terminate the search
 and are then executed normally (depending on `search-exit-option').
Likewise for function keys and mouse button events.

If this function is called non-interactively, it does not return to
the calling function until the search is done."

  (interactive "P\np")
  (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit)))

(defun isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  "\
Do incremental search forward for regular expression.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input is treated
as a regexp.  See the command `isearch-forward' for more information.

In regexp incremental searches, a space or spaces normally matches
any whitespace (the variable `search-whitespace-regexp' controls
precisely what that means).  If you want to search for a literal space
and nothing else, enter C-q SPC."
  (interactive "P\np")
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit)))

(defun isearch-forward-word (&optional not-word no-recursive-edit)
  "\
Do incremental search forward for a sequence of words.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input is treated
as a sequence of words without regard to how the words are separated.
See the command `isearch-forward' for more information."
  (interactive "P\np")
  (isearch-mode t nil nil (not no-recursive-edit) (null not-word)))

(defun isearch-backward (&optional regexp-p no-recursive-edit)
  "\
Do incremental search backward.
With a prefix argument, do a regular expression search instead.
See the command `isearch-forward' for more information."
  (interactive "P\np")
  (isearch-mode nil (not (null regexp-p)) nil (not no-recursive-edit)))

(defun isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  "\
Do incremental search backward for regular expression.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input is treated
as a regexp.  See the command `isearch-forward' for more information."
  (interactive "P\np")
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit)))


;; isearch-mode only sets up incremental search for the minor mode.
;; All the work is done by the isearch-mode commands.

;; Not used yet:
;;(defvar isearch-commands '(isearch-forward isearch-backward
;;			     isearch-forward-regexp isearch-backward-regexp)
;;  "List of commands for which isearch-mode does not recursive-edit.")


(defun isearch-mode (forward &optional regexp op-fun recursive-edit word-p)
  "Start Isearch minor mode.
It is called by the function `isearch-forward' and other related functions."

  ;; Initialize global vars.
  (setq isearch-forward forward
	isearch-regexp regexp
	isearch-word word-p
	isearch-op-fun op-fun
	isearch-last-case-fold-search isearch-case-fold-search
	isearch-case-fold-search case-fold-search
	isearch-string ""
	isearch-message ""
	isearch-cmds nil
	isearch-success t
	isearch-wrapped nil
	isearch-barrier (point)
	isearch-adjusted nil
	isearch-yank-flag nil
	isearch-error nil
	isearch-slow-terminal-mode (and (<= baud-rate search-slow-speed)
					(> (window-height)
					   (* 4
					      (abs search-slow-window-lines))))
	isearch-other-end nil
	isearch-small-window nil
	isearch-just-started t
	isearch-start-hscroll (window-hscroll)

	isearch-opoint (point)
	search-ring-yank-pointer nil
	isearch-opened-overlays nil
	isearch-input-method-function input-method-function
	isearch-input-method-local-p (local-variable-p 'input-method-function)
	regexp-search-ring-yank-pointer nil

	;; Save the original value of `minibuffer-message-timeout', and
	;; set it to nil so that isearch's messages don't get timed out.
	isearch-original-minibuffer-message-timeout minibuffer-message-timeout
	minibuffer-message-timeout nil)

  ;; We must bypass input method while reading key.  When a user type
  ;; printable character, appropriate input method is turned on in
  ;; minibuffer to read multibyte characters.
  (or isearch-input-method-local-p
      (make-local-variable 'input-method-function))
  (setq input-method-function nil)

  (looking-at "")
  (setq isearch-window-configuration
	(if isearch-slow-terminal-mode (current-window-configuration) nil))

  ;; Maybe make minibuffer frame visible and/or raise it.
  (let ((frame (window-frame (minibuffer-window))))
    (unless (memq (frame-live-p frame) '(nil t))
      (unless (frame-visible-p frame)
	(make-frame-visible frame))
      (if minibuffer-auto-raise
	  (raise-frame frame))))

  (setq	isearch-mode " Isearch")  ;; forward? regexp?
  (force-mode-line-update)

  (setq overriding-terminal-local-map isearch-mode-map)
  (run-hooks 'isearch-mode-hook)

  ;; Pushing the initial state used to be before running isearch-mode-hook,
  ;; but a hook might set `isearch-push-state-function' used in
  ;; `isearch-push-state' to save mode-specific initial state.  (Bug#4994)
  (isearch-push-state)

  (isearch-update)

  (add-hook 'mouse-leave-buffer-hook 'isearch-done)
  (add-hook 'kbd-macro-termination-hook 'isearch-done)

  ;; isearch-mode can be made modal (in the sense of not returning to
  ;; the calling function until searching is completed) by entering
  ;; a recursive-edit and exiting it when done isearching.
  (if recursive-edit
      (let ((isearch-recursive-edit t))
	(recursive-edit)))
  isearch-success)


;; Some high level utilities.  Others below.

(defun isearch-update ()
  "This is called after every isearch command to update the display.
The last thing it does is to run `isearch-update-post-hook'."
  (if (and (null unread-command-events)
	   (null executing-kbd-macro))
      (progn
        (if (not (input-pending-p))
	    (if isearch-message-function
		(funcall isearch-message-function)
	      (isearch-message)))
        (if (and isearch-slow-terminal-mode
                 (not (or isearch-small-window
                          (pos-visible-in-window-p))))
            (let ((found-point (point)))
              (setq isearch-small-window t)
              (move-to-window-line 0)
              (let ((window-min-height 1))
                (split-window nil (if (< search-slow-window-lines 0)
                                      (1+ (- search-slow-window-lines))
                                    (- (window-height)
                                       (1+ search-slow-window-lines)))))
              (if (< search-slow-window-lines 0)
                  (progn (vertical-motion (- 1 search-slow-window-lines))
                         (set-window-start (next-window) (point))
                         (set-window-hscroll (next-window)
                                             (window-hscroll))
                         (set-window-hscroll (selected-window) 0))
                (other-window 1))
              (goto-char found-point))
	  ;; Keep same hscrolling as at the start of the search when possible
	  (let ((current-scroll (window-hscroll)))
	    (set-window-hscroll (selected-window) isearch-start-hscroll)
	    (unless (pos-visible-in-window-p)
	      (set-window-hscroll (selected-window) current-scroll))))
	(if isearch-other-end
            (if (< isearch-other-end (point)) ; isearch-forward?
                (isearch-highlight isearch-other-end (point))
              (isearch-highlight (point) isearch-other-end))
          (isearch-dehighlight))
        ))
  (setq ;; quit-flag nil  not for isearch-mode
   isearch-adjusted nil
   isearch-yank-flag nil)
  (when isearch-lazy-highlight
    (isearch-lazy-highlight-new-loop))
  ;; We must prevent the point moving to the end of composition when a
  ;; part of the composition has just been searched.
  (setq disable-point-adjustment t)
  (run-hooks 'isearch-update-post-hook))

(defun isearch-done (&optional nopush edit)
  "Exit Isearch mode.
For successful search, pass no args.
For a failing search, NOPUSH is t.
For going to the minibuffer to edit the search string,
NOPUSH is t and EDIT is t."

  (if isearch-resume-in-command-history
      (let ((command `(isearch-resume ,isearch-string ,isearch-regexp
				      ,isearch-word ,isearch-forward
				      ,isearch-message
				      ',isearch-case-fold-search)))
	(unless (equal (car command-history) command)
	  (setq command-history (cons command command-history)))))

  (remove-hook 'mouse-leave-buffer-hook 'isearch-done)
  (remove-hook 'kbd-macro-termination-hook 'isearch-done)
  (setq isearch-lazy-highlight-start nil)

  ;; Called by all commands that terminate isearch-mode.
  ;; If NOPUSH is non-nil, we don't push the string on the search ring.
  (setq overriding-terminal-local-map nil)
  ;; (setq pre-command-hook isearch-old-pre-command-hook) ; for lemacs
  (setq minibuffer-message-timeout isearch-original-minibuffer-message-timeout)
  (isearch-dehighlight)
  (lazy-highlight-cleanup lazy-highlight-cleanup)
  (let ((found-start (window-start (selected-window)))
	(found-point (point)))
    (when isearch-window-configuration
      (set-window-configuration isearch-window-configuration)
      (if isearch-small-window
	  (goto-char found-point)
	;; set-window-configuration clobbers window-start; restore it.
	;; This has an annoying side effect of clearing the last_modiff
	;; field of the window, which can cause unwanted scrolling,
	;; so don't do it unless truly necessary.
	(set-window-start (selected-window) found-start t))))

  (setq isearch-mode nil)
  (if isearch-input-method-local-p
      (setq input-method-function isearch-input-method-function)
    (kill-local-variable 'input-method-function))

  (force-mode-line-update)

  ;; If we ended in the middle of some intangible text,
  ;; move to the further end of that intangible text.
  (let ((after (if (eobp) nil
		 (get-text-property (point) 'intangible)))
	(before (if (bobp) nil
		  (get-text-property (1- (point)) 'intangible))))
    (when (and before after (eq before after))
      (if isearch-forward
	  (goto-char (next-single-property-change (point) 'intangible))
	(goto-char (previous-single-property-change (point) 'intangible)))))

  (if (and (> (length isearch-string) 0) (not nopush))
      ;; Update the ring data.
      (isearch-update-ring isearch-string isearch-regexp))

  (let ((isearch-mode-end-hook-quit (and nopush (not edit))))
    (run-hooks 'isearch-mode-end-hook))

  ;; If there was movement, mark the starting position.
  ;; Maybe should test difference between and set mark only if > threshold.
  (if (/= (point) isearch-opoint)
      (or (and transient-mark-mode mark-active)
	  (progn
	    (push-mark isearch-opoint t)
	    (or executing-kbd-macro (> (minibuffer-depth) 0) edit
		(message "Mark saved where search started")))))

  (and (not edit) isearch-recursive-edit (exit-recursive-edit)))

(defun isearch-update-ring (string &optional regexp)
  "Add STRING to the beginning of the search ring.
REGEXP if non-nil says use the regexp search ring."
  (add-to-history
   (if regexp 'regexp-search-ring 'search-ring)
   string
   (if regexp regexp-search-ring-max search-ring-max)))

;; Switching buffers should first terminate isearch-mode.
;; ;; For Emacs 19, the frame switch event is handled.
;; (defun isearch-switch-frame-handler ()
;;   (interactive) ;; Is this necessary?
;;   ;; First terminate isearch-mode.
;;   (isearch-done)
;;   (isearch-clean-overlays)
;;   (handle-switch-frame (car (cdr last-command-event))))


;; The search status structure and stack.

(defsubst isearch-string-state (frame)
  "Return the search string in FRAME."
  (aref frame 0))
(defsubst isearch-message-state (frame)
  "Return the search string to display to the user in FRAME."
  (aref frame 1))
(defsubst isearch-point-state (frame)
  "Return the point in FRAME."
  (aref frame 2))
(defsubst isearch-success-state (frame)
  "Return the success flag in FRAME."
  (aref frame 3))
(defsubst isearch-forward-state (frame)
  "Return the searching-forward flag in FRAME."
  (aref frame 4))
(defsubst isearch-other-end-state (frame)
  "Return the other end of the match in FRAME."
  (aref frame 5))
(defsubst isearch-word-state (frame)
  "Return the search-by-word flag in FRAME."
  (aref frame 6))
(defsubst isearch-error-state (frame)
  "Return the regexp error message in FRAME, or nil if its regexp is valid."
  (aref frame 7))
(defsubst isearch-wrapped-state (frame)
  "Return the search-wrapped flag in FRAME."
  (aref frame 8))
(defsubst isearch-barrier-state (frame)
  "Return the barrier value in FRAME."
  (aref frame 9))
(defsubst isearch-case-fold-search-state (frame)
  "Return the case-folding flag in FRAME."
  (aref frame 10))
(defsubst isearch-pop-fun-state (frame)
  "Return the function restoring the mode-specific Isearch state in FRAME."
  (aref frame 11))

(defun isearch-top-state ()
  (let ((cmd (car isearch-cmds)))
    (setq isearch-string (isearch-string-state cmd)
	  isearch-message (isearch-message-state cmd)
	  isearch-success (isearch-success-state cmd)
	  isearch-forward (isearch-forward-state cmd)
	  isearch-other-end (isearch-other-end-state cmd)
	  isearch-word (isearch-word-state cmd)
	  isearch-error (isearch-error-state cmd)
	  isearch-wrapped (isearch-wrapped-state cmd)
	  isearch-barrier (isearch-barrier-state cmd)
	  isearch-case-fold-search (isearch-case-fold-search-state cmd))
    (if (functionp (isearch-pop-fun-state cmd))
	(funcall (isearch-pop-fun-state cmd) cmd))
    (goto-char (isearch-point-state cmd))))

(defun isearch-pop-state ()
  (setq isearch-cmds (cdr isearch-cmds))
  (isearch-top-state))

(defun isearch-push-state ()
  (setq isearch-cmds
	(cons (vector isearch-string isearch-message (point)
		      isearch-success isearch-forward isearch-other-end
		      isearch-word
		      isearch-error isearch-wrapped isearch-barrier
		      isearch-case-fold-search
		      (if isearch-push-state-function
			  (funcall isearch-push-state-function)))
	      isearch-cmds)))


;; Commands active while inside of the isearch minor mode.

(defun isearch-exit ()
  "Exit search normally.
However, if this is the first command after starting incremental
search and `search-nonincremental-instead' is non-nil, do a
nonincremental search instead via `isearch-edit-string'."
  (interactive)
  (if (and search-nonincremental-instead
	   (= 0 (length isearch-string)))
      (let ((isearch-nonincremental t))
	(isearch-edit-string)))
  (isearch-done)
  (isearch-clean-overlays))

(defvar minibuffer-history-symbol) ;; from external package gmhist.el

(defun isearch-fail-pos (&optional msg)
  "Return position of first mismatch in search string, or nil if none.
If MSG is non-nil, use `isearch-message', otherwise `isearch-string'."
  (let ((cmds isearch-cmds)
	(curr-msg (if msg isearch-message isearch-string))
	succ-msg)
    (when (or (not isearch-success) isearch-error)
      (while (or (not (isearch-success-state (car cmds)))
                 (isearch-error-state (car cmds)))
        (pop cmds))
      (setq succ-msg (and cmds (if msg (isearch-message-state (car cmds))
				 (isearch-string-state (car cmds)))))
      (if (and (stringp succ-msg)
	       (< (length succ-msg) (length curr-msg))
	       (equal succ-msg
		      (substring curr-msg 0 (length succ-msg))))
	  (length succ-msg)
	0))))

(defun isearch-edit-string ()
  "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring."

  ;; This code is very hairy for several reasons, explained in the code.
  ;; Mainly, isearch-mode must be terminated while editing and then restarted.
  ;; If there were a way to catch any change of buffer from the minibuffer,
  ;; this could be simplified greatly.
  ;; Editing doesn't back up the search point.  Should it?
  (interactive)
  (condition-case nil
      (progn
	(let ((isearch-nonincremental isearch-nonincremental)

	      ;; Locally bind all isearch global variables to protect them
	      ;; from recursive isearching.
	      ;; isearch-string -message and -forward are not bound
	      ;; so they may be changed.  Instead, save the values.
	      (isearch-new-string isearch-string)
	      (isearch-new-message isearch-message)
	      (isearch-new-forward isearch-forward)
	      (isearch-new-word isearch-word)
	      (isearch-new-case-fold isearch-case-fold-search)

	      (isearch-regexp isearch-regexp)
	      (isearch-op-fun isearch-op-fun)
	      (isearch-cmds isearch-cmds)
	      (isearch-success isearch-success)
	      (isearch-wrapped isearch-wrapped)
	      (isearch-barrier isearch-barrier)
	      (isearch-adjusted isearch-adjusted)
	      (isearch-yank-flag isearch-yank-flag)
	      (isearch-error isearch-error)
  ;;; Don't bind this.  We want isearch-search, below, to set it.
  ;;; And the old value won't matter after that.
  ;;;	    (isearch-other-end isearch-other-end)
  ;;; Perhaps some of these other variables should be bound for a
  ;;; shorter period, ending before the next isearch-search.
  ;;; But there doesn't seem to be a real bug, so let's not risk it now.
	      (isearch-opoint isearch-opoint)
	      (isearch-slow-terminal-mode isearch-slow-terminal-mode)
	      (isearch-small-window isearch-small-window)
	      (isearch-recursive-edit isearch-recursive-edit)
	      ;; Save current configuration so we can restore it here.
	      (isearch-window-configuration (current-window-configuration))

	      ;; This could protect the index of the search rings,
	      ;; but we can't reliably count the number of typed M-p
	      ;; in `read-from-minibuffer' to adjust the index accordingly.
	      ;; So when the following is commented out, `isearch-mode'
	      ;; below resets the index to the predictable value nil.
	      ;; (search-ring-yank-pointer search-ring-yank-pointer)
	      ;; (regexp-search-ring-yank-pointer regexp-search-ring-yank-pointer)

	      ;; Temporarily restore `minibuffer-message-timeout'.
	      (minibuffer-message-timeout
	       isearch-original-minibuffer-message-timeout)
	      (isearch-original-minibuffer-message-timeout
	       isearch-original-minibuffer-message-timeout)
	      old-point old-other-end)

	  ;; Actually terminate isearching until editing is done.
	  ;; This is so that the user can do anything without failure,
	  ;; like switch buffers and start another isearch, and return.
	  (condition-case nil
	      (isearch-done t t)
	    (exit nil))			; was recursive editing

	  ;; Save old point and isearch-other-end before reading from minibuffer
	  ;; that can change their values.
	  (setq old-point (point) old-other-end isearch-other-end)

	  (unwind-protect
	      (let* ((message-log-max nil)
		     ;; Don't add a new search string to the search ring here
		     ;; in `read-from-minibuffer'. It should be added only
		     ;; by `isearch-update-ring' called from `isearch-done'.
		     (history-add-new-input nil)
		     ;; Binding minibuffer-history-symbol to nil is a work-around
		     ;; for some incompatibility with gmhist.
		     (minibuffer-history-symbol))
		(setq isearch-new-string
                      (read-from-minibuffer
                       (isearch-message-prefix nil nil isearch-nonincremental)
		       (cons isearch-string (1+ (or (isearch-fail-pos)
						    (length isearch-string))))
                       minibuffer-local-isearch-map nil
                       (if isearch-regexp
			   (cons 'regexp-search-ring
				 (1+ (or regexp-search-ring-yank-pointer -1)))
			 (cons 'search-ring
			       (1+ (or search-ring-yank-pointer -1))))
                       nil t)
		      isearch-new-message
		      (mapconcat 'isearch-text-char-description
				 isearch-new-string "")))

	    ;; Set point at the start (end) of old match if forward (backward),
	    ;; so after exiting minibuffer isearch resumes at the start (end)
	    ;; of this match and can find it again.
	    (if (and old-other-end (eq old-point (point))
		     (eq isearch-forward isearch-new-forward))
		(goto-char old-other-end))

	    ;; Always resume isearching by restarting it.
	    (isearch-mode isearch-forward
			  isearch-regexp
			  isearch-op-fun
			  nil
			  isearch-word)

	    ;; Copy new local values to isearch globals
	    (setq isearch-string isearch-new-string
		  isearch-message isearch-new-message
		  isearch-forward isearch-new-forward
		  isearch-word isearch-new-word
		  isearch-case-fold-search isearch-new-case-fold))

	  ;; Empty isearch-string means use default.
	  (when (= 0 (length isearch-string))
	    (setq isearch-string (or (car (if isearch-regexp
					      regexp-search-ring
					    search-ring))
				     "")

		  isearch-message
		  (mapconcat 'isearch-text-char-description
			     isearch-string ""))
	    ;; After taking the last element, adjust ring to previous one.
	    (isearch-ring-adjust1 nil)))

	;; This used to push the state as of before this C-s, but it adds
	;; an inconsistent state where part of variables are from the
	;; previous search (e.g. `isearch-success'), and part of variables
	;; are just entered from the minibuffer (e.g. `isearch-string').
	;; (isearch-push-state)

	;; Reinvoke the pending search.
	(isearch-search)
	(isearch-push-state)		; this pushes the correct state
	(isearch-update)
	(if isearch-nonincremental
	    (progn
	      ;; (sit-for 1) ;; needed if isearch-done does: (message "")
	      (isearch-done)
	      ;; The search done message is confusing when the string
	      ;; is empty, so erase it.
	      (if (equal isearch-string "")
		  (message "")))))

    (quit  ; handle abort-recursive-edit
     (isearch-abort)  ;; outside of let to restore outside global values
     )))

(defun isearch-nonincremental-exit-minibuffer ()
  (interactive)
  (setq isearch-nonincremental t)
  (exit-minibuffer))

(defun isearch-forward-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward t)
  (exit-minibuffer))

(defun isearch-reverse-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward nil)
  (exit-minibuffer))

(defun isearch-cancel ()
  "Terminate the search and go back to the starting point."
  (interactive)
  (if (and isearch-push-state-function isearch-cmds)
      ;; For defined push-state function, restore the first state.
      ;; This calls pop-state function and restores original point.
      (let ((isearch-cmds (last isearch-cmds)))
	(isearch-top-state))
    (goto-char isearch-opoint))
  (isearch-done t)                      ; exit isearch
  (isearch-clean-overlays)
  (signal 'quit nil))                   ; and pass on quit signal

(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signaling quit.
Otherwise, revert to previous successful search and continue searching.
Use `isearch-exit' to quit without signaling."
  (interactive)
;;  (ding)  signal instead below, if quitting
  (discard-input)
  (if (and isearch-success (not isearch-error))
      ;; If search is successful and has no incomplete regexp,
      ;; move back to starting point and really do quit.
      (progn
        (setq isearch-success nil)
        (isearch-cancel))
    ;; If search is failing, or has an incomplete regexp,
    ;; rub out until it is once more successful.
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state))
    (isearch-update)))

(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (if (null (if isearch-regexp regexp-search-ring search-ring))
	      (setq isearch-error "No previous search string")
	    (setq isearch-string
		  (if isearch-regexp
		      (car regexp-search-ring)
		    (car search-ring))
		  isearch-message
		  (mapconcat 'isearch-text-char-description
			     isearch-string "")
		  isearch-case-fold-search isearch-last-case-fold-search)
	    ;; After taking the last element, adjust ring to previous one.
	    (isearch-ring-adjust1 nil))
	;; If already have what to search for, repeat it.
	(or isearch-success
	    (progn
	      ;; Set isearch-wrapped before calling isearch-wrap-function
	      (setq isearch-wrapped t)
	      (if isearch-wrap-function
		  (funcall isearch-wrap-function)
	        (goto-char (if isearch-forward (point-min) (point-max)))))))
    ;; C-s in reverse or C-r in forward, change direction.
    (setq isearch-forward (not isearch-forward)
	  isearch-success t))

  (setq isearch-barrier (point)) ; For subsequent \| if regexp.

  (if (equal isearch-string "")
      (setq isearch-success t)
    (if (and isearch-success
	     (equal (point) isearch-other-end)
	     (not isearch-just-started))
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (if isearch-forward (eobp) (bobp))
	    ;; If there's nowhere to advance to, fail (and wrap next time).
	    (progn
	      (setq isearch-success nil)
	      (ding))
	  (forward-char (if isearch-forward 1 -1))
	  (isearch-search))
      (isearch-search)))

  (isearch-push-state)
  (isearch-update))

(defun isearch-repeat-forward ()
  "Repeat incremental search forwards."
  (interactive)
  (isearch-repeat 'forward))

(defun isearch-repeat-backward ()
  "Repeat incremental search backwards."
  (interactive)
  (isearch-repeat 'backward))

(defun isearch-toggle-regexp ()
  "Toggle regexp searching on or off."
  ;; The status stack is left unchanged.
  (interactive)
  (setq isearch-regexp (not isearch-regexp))
  (if isearch-regexp (setq isearch-word nil))
  (setq isearch-success t isearch-adjusted t)
  (isearch-update))

(defun isearch-toggle-word ()
  "Toggle word searching on or off."
  (interactive)
  (setq isearch-word (not isearch-word))
  (setq isearch-success t isearch-adjusted t)
  (isearch-update))

(defun isearch-toggle-case-fold ()
  "Toggle case folding in searching on or off."
  (interactive)
  (setq isearch-case-fold-search
	(if isearch-case-fold-search nil 'yes))
  (let ((message-log-max nil))
    (message "%s%s [case %ssensitive]"
	     (isearch-message-prefix nil nil isearch-nonincremental)
	     isearch-message
	     (if isearch-case-fold-search "in" "")))
  (setq isearch-success t isearch-adjusted t)
  (sit-for 1)
  (isearch-update))

(defun isearch-query-replace (&optional delimited regexp-flag)
  "Start `query-replace' with string to replace from last search string.
The arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.  Note that using the prefix arg
is possible only when `isearch-allow-scroll' is non-nil, and it doesn't
always provide the correct matches for `query-replace', so the preferred
way to run word replacements from Isearch is `M-s w ... M-%'."
  (interactive
   (list current-prefix-arg))
  (barf-if-buffer-read-only)
  (if regexp-flag (setq isearch-regexp t))
  (let ((case-fold-search isearch-case-fold-search)
	;; set `search-upper-case' to nil to not call
	;; `isearch-no-upper-case-p' in `perform-replace'
	(search-upper-case nil)
	;; Set `isearch-recursive-edit' to nil to prevent calling
	;; `exit-recursive-edit' in `isearch-done' that terminates
	;; the execution of this command when it is non-nil.
	;; We call `exit-recursive-edit' explicitly at the end below.
	(isearch-recursive-edit nil))
    (isearch-done nil t)
    (isearch-clean-overlays)
    (if (and isearch-other-end
	     (< isearch-other-end (point))
             (not (and transient-mark-mode mark-active
                       (< (mark) (point)))))
        (goto-char isearch-other-end))
    (set query-replace-from-history-variable
         (cons isearch-string
               (symbol-value query-replace-from-history-variable)))
    (perform-replace
     isearch-string
     (query-replace-read-to
      isearch-string
      (concat "Query replace"
	      (if (or delimited isearch-word) " word" "")
	      (if isearch-regexp " regexp" "")
	      (if (and transient-mark-mode mark-active) " in region" ""))
      isearch-regexp)
     t isearch-regexp (or delimited isearch-word) nil nil
     (if (and transient-mark-mode mark-active) (region-beginning))
     (if (and transient-mark-mode mark-active) (region-end))))
  (and isearch-recursive-edit (exit-recursive-edit)))

(defun isearch-query-replace-regexp (&optional delimited)
  "Start `query-replace-regexp' with string to replace from last search string.
See `isearch-query-replace' for more information."
  (interactive
   (list current-prefix-arg))
  (isearch-query-replace delimited t))

(defun isearch-occur (regexp &optional nlines)
  "Run `occur' using the last search string as the regexp.
Interactively, REGEXP is constructed using the search string from the
last search command.  NLINES has the same meaning as in `occur'.

If the last search command was a word search, REGEXP is computed from
the search words, ignoring punctuation.  If the last search
command was a regular expression search, REGEXP is the regular
expression used in that search.  If the last search command searched
for a literal string, REGEXP is constructed by quoting all the special
characters in that string."
  (interactive
   (let* ((perform-collect (consp current-prefix-arg))
	  (regexp (cond
		   (isearch-word (word-search-regexp isearch-string))
		   (isearch-regexp isearch-string)
		   (t (regexp-quote isearch-string)))))
     (list regexp
	   (if perform-collect
	       ;; Perform collect operation
	       (if (zerop (regexp-opt-depth regexp))
		   ;; No subexpression so collect the entire match.
		   "\\&"
		 ;; Get the regexp for collection pattern.
		 (isearch-done nil t)
		 (isearch-clean-overlays)
		 (let ((default (car occur-collect-regexp-history)))
		   (read-string
		    (format "Regexp to collect (default %s): " default)
		    nil 'occur-collect-regexp-history default)))
	     ;; Otherwise normal occur takes numerical prefix argument.
	     (when current-prefix-arg
	       (prefix-numeric-value current-prefix-arg))))))
  (let ((case-fold-search isearch-case-fold-search)
	;; Set `search-upper-case' to nil to not call
	;; `isearch-no-upper-case-p' in `occur-1'.
	(search-upper-case nil)
	(search-spaces-regexp (if isearch-regexp search-whitespace-regexp)))
    (occur regexp nlines)))

(declare-function hi-lock-read-face-name "hi-lock" ())

(defun isearch-highlight-regexp ()
  "Run `highlight-regexp' with regexp from the current search string.
It exits Isearch mode and calls `hi-lock-face-buffer' with its regexp
argument from the last search regexp or a quoted search string,
and reads its face argument using `hi-lock-read-face-name'."
  (interactive)
  (let (
	;; Set `isearch-recursive-edit' to nil to prevent calling
	;; `exit-recursive-edit' in `isearch-done' that terminates
	;; the execution of this command when it is non-nil.
	;; We call `exit-recursive-edit' explicitly at the end below.
	(isearch-recursive-edit nil))
    (isearch-done nil t)
    (isearch-clean-overlays))
  (require 'hi-lock nil t)
  (let ((string (cond (isearch-regexp isearch-string)
		      ((if (and (eq isearch-case-fold-search t)
				search-upper-case)
			   (isearch-no-upper-case-p
			    isearch-string isearch-regexp)
			 isearch-case-fold-search)
		       ;; Turn isearch-string into a case-insensitive
		       ;; regexp.
		       (mapconcat
			(lambda (c)
			  (let ((s (string c)))
			    (if (string-match "[[:alpha:]]" s)
				(format "[%s%s]" (upcase s) (downcase s))
			      (regexp-quote s))))
			isearch-string ""))
		      (t (regexp-quote isearch-string)))))
    (hi-lock-face-buffer string (hi-lock-read-face-name)))
  (and isearch-recursive-edit (exit-recursive-edit)))


(defun isearch-delete-char ()
  "Discard last input item and move point back.
If no previous match was done, just beep."
  (interactive)
  (if (null (cdr isearch-cmds))
      (ding)
    (isearch-pop-state))
  (isearch-update))

(defun isearch-del-char (&optional arg)
  "Delete character from end of search string and search again.
If search string is empty, just beep."
  (interactive "p")
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string (substring isearch-string 0 (- (or arg 1)))
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Use the isearch-other-end as new starting point to be able
  ;; to find the remaining part of the search string again.
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defun isearch-yank-string (string)
  "Pull STRING into search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search
	   (eq 'not-yanks search-upper-case))
      (setq string (downcase string)))
  (if isearch-regexp (setq string (regexp-quote string)))
  ;; Don't move cursor in reverse search.
  (setq isearch-yank-flag t)
  (isearch-process-search-string
   string (mapconcat 'isearch-text-char-description string "")))

(defun isearch-yank-kill ()
  "Pull string from kill ring into search string."
  (interactive)
  (isearch-yank-string (current-kill 0)))

(defun isearch-yank-pop ()
  "Replace just-yanked search string with previously killed string."
  (interactive)
  (if (not (memq last-command '(isearch-yank-kill isearch-yank-pop)))
      ;; Fall back on `isearch-yank-kill' for the benefits of people
      ;; who are used to the old behavior of `M-y' in isearch mode. In
      ;; future, this fallback may be changed if we ever change
      ;; `yank-pop' to do something like the kill-ring-browser.
      (isearch-yank-kill)
    (isearch-pop-state)
    (isearch-yank-string (current-kill 1))))

(defun isearch-yank-x-selection ()
  "Pull current X selection into search string."
  (interactive)
  (isearch-yank-string (x-get-selection))
  ;; If `x-get-selection' returned the text from the active region,
  ;; then it "used" the mark which we should hence deactivate.
  (when select-active-regions (deactivate-mark)))


(defun isearch-mouse-2 (click)
  "Handle mouse-2 in Isearch mode.
For a click in the echo area, invoke `isearch-yank-x-selection'.
Otherwise invoke whatever the calling mouse-2 command sequence
is bound to outside of Isearch."
  (interactive "e")
  (let* ((w (posn-window (event-start click)))
	 (overriding-terminal-local-map nil)
	 (binding (key-binding (this-command-keys-vector) t)))
    (if (and (window-minibuffer-p w)
	     (not (minibuffer-window-active-p w))) ; in echo area
	(isearch-yank-x-selection)
      (when (functionp binding)
	(call-interactively binding)))))

(defun isearch-yank-internal (jumpform)
  "Pull the text from point to the point reached by JUMPFORM.
JUMPFORM is a lambda expression that takes no arguments and returns
a buffer position, possibly having moved point to that position.
For example, it might move point forward by a word and return point,
or it might return the position of the end of the line."
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
	  (goto-char isearch-other-end))
     (buffer-substring-no-properties (point) (funcall jumpform)))))

(defun isearch-yank-char-in-minibuffer (&optional arg)
  "Pull next character from buffer into end of search string in minibuffer."
  (interactive "p")
  (if (eobp)
      (insert
       (with-current-buffer (cadr (buffer-list))
         (buffer-substring-no-properties
          (point) (progn (forward-char arg) (point)))))
    (forward-char arg)))

(defun isearch-yank-char (&optional arg)
  "Pull next character from buffer into search string."
  (interactive "p")
  (isearch-yank-internal (lambda () (forward-char arg) (point))))

(declare-function subword-forward "subword" (&optional arg))
(defun isearch-yank-word-or-char ()
  "Pull next character, subword or word from buffer into search string.
Subword is used when `subword-mode' is activated. "
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (if (or (= (char-syntax (or (char-after) 0)) ?w)
             (= (char-syntax (or (char-after (1+ (point))) 0)) ?w))
	 (if (and (boundp 'subword-mode) subword-mode)
	     (subword-forward 1)
	   (forward-word 1))
       (forward-char 1)) (point))))

(defun isearch-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-word 1) (point))))

(defun isearch-yank-line ()
  "Pull rest of line from buffer into search string."
  (interactive)
  (isearch-yank-internal
   (lambda () (let ((inhibit-field-text-motion t))
		(line-end-position (if (eolp) 2 1))))))

(defun isearch-search-and-update ()
  ;; Do the search and update the display.
  (when (or isearch-success
	    ;; Unsuccessful regexp search may become successful by
	    ;; addition of characters which make isearch-string valid
	    isearch-regexp
	    ;; If the string was found but was completely invisible,
	    ;; it might now be partly visible, so try again.
	    (prog1 isearch-hidden (setq isearch-hidden nil)))
    ;; In reverse search, adding stuff at
    ;; the end may cause zero or many more chars to be
    ;; matched, in the string following point.
    ;; Allow all those possibilities without moving point as
    ;; long as the match does not extend past search origin.
    (if (and (not isearch-forward) (not isearch-adjusted)
	     (condition-case ()
		 (let ((case-fold-search isearch-case-fold-search))
		   (if (and (eq case-fold-search t) search-upper-case)
		       (setq case-fold-search
			     (isearch-no-upper-case-p isearch-string isearch-regexp)))
		   (looking-at (cond
				(isearch-word (word-search-regexp isearch-string t))
				(isearch-regexp isearch-string)
				(t (regexp-quote isearch-string)))))
	       (error nil))
	     (or isearch-yank-flag
		 (<= (match-end 0)
		     (min isearch-opoint isearch-barrier))))
	(progn
	  (setq isearch-success t
		isearch-error nil
		isearch-other-end (match-end 0))
	  (if (and (eq isearch-case-fold-search t) search-upper-case)
	      (setq isearch-case-fold-search
		    (isearch-no-upper-case-p isearch-string isearch-regexp))))
      ;; Not regexp, not reverse, or no match at point.
      (if (and isearch-other-end (not isearch-adjusted))
	  (goto-char (if isearch-forward isearch-other-end
		       (min isearch-opoint
			    isearch-barrier
			    (1+ isearch-other-end)))))
      (isearch-search)
      ))
  (isearch-push-state)
  (if isearch-op-fun (funcall isearch-op-fun))
  (isearch-update))


;; *, ?, }, and | chars can make a regexp more liberal.
;; They can make a regexp match sooner or make it succeed instead of failing.
;; So go back to place last successful search started
;; or to the last ^S/^R (barrier), whichever is nearer.
;; + needs no special handling because the string must match at least once.

(defun isearch-backslash (str)
  "Return t if STR ends in an odd number of backslashes."
  (= (mod (- (length str) (string-match "\\\\*\\'" str)) 2) 1))

(defun isearch-fallback (want-backslash &optional allow-invalid to-barrier)
  "Return point to previous successful match to allow regexp liberalization.
\\<isearch-mode-map>
Respects \\[isearch-repeat-forward] and \\[isearch-repeat-backward] by \
stopping at `isearch-barrier' as needed.

Do nothing if a backslash is escaping the liberalizing character.
If WANT-BACKSLASH is non-nil, invert this behavior (for \\} and \\|).

Do nothing if regexp has recently been invalid unless optional
ALLOW-INVALID non-nil.

If optional TO-BARRIER non-nil, ignore previous matches and go exactly
to the barrier."
  ;; (eq (not a) (not b)) makes all non-nil values equivalent
  (when (and isearch-regexp (eq (not (isearch-backslash isearch-string))
				(not want-backslash))
	     ;; We have to check 2 stack frames because the last might be
	     ;; invalid just because of a backslash.
	     (or (not isearch-error)
		 (not (isearch-error-state (cadr isearch-cmds)))
		 allow-invalid))
    (if to-barrier
	(progn (goto-char isearch-barrier)
	       (setq isearch-adjusted t))
      (let* ((stack isearch-cmds)
	     (previous (cdr stack))	; lookbelow in the stack
	     (frame (car stack)))
	;; Walk down the stack looking for a valid regexp (as of course only
	;; they can be the previous successful match); this conveniently
	;; removes all bracket-sets and groups that might be in the way, as
	;; well as partial \{\} constructs that the code below leaves behind.
	;; Also skip over postfix operators -- though horrid,
	;; 'ab?\{5,6\}+\{1,2\}*' is perfectly valid.
	(while (and previous
		    (or (isearch-error-state frame)
			(let* ((string (isearch-string-state frame))
			       (lchar (aref string (1- (length string)))))
			  ;; The operators aren't always operators; check
			  ;; backslashes.  This doesn't handle the case of
			  ;; operators at the beginning of the regexp not
			  ;; being special, but then we should fall back to
			  ;; the barrier anyway because it's all optional.
			  (if (isearch-backslash
			       (isearch-string-state (car previous)))
			      (eq lchar ?\})
			    (memq lchar '(?* ?? ?+))))))
	  (setq stack previous previous (cdr previous) frame (car stack)))
	(when stack
	  ;; `stack' now refers the most recent valid regexp that is not at
	  ;; all optional in its last term.  Now dig one level deeper and find
	  ;; what matched before that.
	  (let ((last-other-end
		 (or (and (car previous)
			  (isearch-other-end-state (car previous)))
		     isearch-barrier)))
	    (goto-char (if isearch-forward
			   (max last-other-end isearch-barrier)
			 (min last-other-end isearch-barrier)))
	    (setq isearch-adjusted t)))))))

(defun isearch-unread-key-sequence (keylist)
  "Unread the given key-sequence KEYLIST.
Scroll-bar or mode-line events are processed appropriately."
  (cancel-kbd-macro-events)
  (apply 'isearch-unread keylist)
  ;; If the event was a scroll-bar or mode-line click, the event will have
  ;; been prefixed by a symbol such as vertical-scroll-bar.  We must remove
  ;; it here, because this symbol will be attached to the event again next
  ;; time it gets read by read-key-sequence.
  ;;
  ;; (Old comment from isearch-other-meta-char: "Note that we don't have to
  ;; modify the event anymore in 21 because read_key_sequence no longer
  ;; modifies events to produce fake prefix keys.")
  (if (and (> (length keylist) 1)
           (symbolp (car keylist))
           (listp (cadr keylist))
           (not (numberp (posn-point
                          (event-start (cadr keylist)  )))))
      (pop unread-command-events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling within Isearch mode.  Alan Mackenzie (acm@muc.de), 2003/2/24
;;
;; The idea here is that certain vertical scrolling commands (like C-l
;; `recenter') should be usable WITHIN Isearch mode.  For a command to be
;; suitable, it must NOT alter the buffer, swap to another buffer or frame,
;; tamper with isearch's state, or move point.  It is unacceptable for the
;; search string to be scrolled out of the current window.  If a command
;; attempts this, we scroll the text back again.
;;
;; We implement this feature with a property called `isearch-scroll'.
;; If a command's symbol has the value t for this property or for the
;; `scroll-command' property, it is a scrolling command.  The feature
;; needs to be enabled by setting the customizable variable
;; `isearch-allow-scroll' to a non-nil value.
;;
;; The universal argument commands (e.g. C-u) in simple.el are marked
;; as scrolling commands, and isearch.el has been amended to allow
;; prefix arguments to be passed through to scrolling commands.  Thus
;; M-0 C-l will scroll point to the top of the window.
;;
;; Horizontal scrolling commands are currently not catered for.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the isearch-scroll property on some standard functions:
;; Scroll-bar functions:
(if (fboundp 'scroll-bar-toolkit-scroll)
    (put 'scroll-bar-toolkit-scroll 'isearch-scroll t))
(if (fboundp 'w32-handle-scroll-bar-event)
    (put 'w32-handle-scroll-bar-event 'isearch-scroll t))

;; Commands which scroll the window (some scroll commands
;; already have the `scroll-command' property on them):
(put 'recenter 'isearch-scroll t)
(put 'recenter-top-bottom 'isearch-scroll t)
(put 'reposition-window 'isearch-scroll t)

;; Commands which act on the other window
(put 'list-buffers 'isearch-scroll t)
(put 'scroll-other-window 'isearch-scroll t)
(put 'scroll-other-window-down 'isearch-scroll t)
(put 'beginning-of-buffer-other-window 'isearch-scroll t)
(put 'end-of-buffer-other-window 'isearch-scroll t)

;; Commands which change the window layout
(put 'delete-other-windows 'isearch-scroll t)
(put 'balance-windows 'isearch-scroll t)
(put 'split-window-right 'isearch-scroll t)
(put 'split-window-below 'isearch-scroll t)
(put 'enlarge-window 'isearch-scroll t)

;; Aliases for split-window-*
(put 'split-window-vertically 'isearch-scroll t)
(put 'split-window-horizontally 'isearch-scroll t)

;; Universal argument commands
(put 'universal-argument 'isearch-scroll t)
(put 'negative-argument 'isearch-scroll t)
(put 'digit-argument 'isearch-scroll t)

(defcustom isearch-allow-scroll nil
  "Whether scrolling is allowed during incremental search.
If non-nil, scrolling commands can be used in Isearch mode.
However, the current match will never scroll offscreen.
If nil, scrolling commands will first cancel Isearch mode."
  :type 'boolean
  :group 'isearch)

(defun isearch-string-out-of-window (isearch-point)
  "Test whether the search string is currently outside of the window.
Return nil if it's completely visible, or if point is visible,
together with as much of the search string as will fit; the symbol
`above' if we need to scroll the text downwards; the symbol `below',
if upwards."
  (let ((w-start (window-start))
        (w-end (window-end nil t))
        (w-L1 (save-excursion (move-to-window-line 1) (point)))
        (w-L-1 (save-excursion (move-to-window-line -1) (point)))
        start end)                  ; start and end of search string in buffer
    (if isearch-forward
        (setq end isearch-point  start (or isearch-other-end isearch-point))
      (setq start isearch-point  end (or isearch-other-end isearch-point)))
    (cond ((or (and (>= start w-start) (<= end w-end))
               (if isearch-forward
                   (and (>= isearch-point w-L-1) (< isearch-point w-end)) ; point on Line -1
                 (and (>= isearch-point w-start) (< isearch-point w-L1)))) ; point on Line 0
           nil)
          ((and (< start w-start)
                (< isearch-point w-L-1))
           'above)
          (t 'below))))

(defun isearch-back-into-window (above isearch-point)
  "Scroll the window to bring the search string back into view.
Restore point to ISEARCH-POINT in the process.  ABOVE is t when the
search string is above the top of the window, nil when it is beneath
the bottom."
  (let (start end)
    (if isearch-forward
        (setq end isearch-point  start (or isearch-other-end isearch-point))
      (setq start isearch-point  end (or isearch-other-end isearch-point)))
    (if above
        (progn
          (goto-char start)
          (recenter 0)
          (when (>= isearch-point (window-end nil t))
            (goto-char isearch-point)
            (recenter -1)))
      (goto-char end)
      (recenter -1)
      (when (< isearch-point (window-start))
        (goto-char isearch-point)
        (recenter 0))))
  (goto-char isearch-point))

(defun isearch-reread-key-sequence-naturally (keylist)
  "Reread key sequence KEYLIST with an inactive Isearch-mode keymap.
Return the key sequence as a string/vector."
  (isearch-unread-key-sequence keylist)
  (let (overriding-terminal-local-map)
    (read-key-sequence nil)))  ; This will go through function-key-map, if nec.

(defun isearch-lookup-scroll-key (key-seq)
  "If KEY-SEQ is bound to a scrolling command, return it as a symbol.
Otherwise return nil."
  (let* ((overriding-terminal-local-map nil)
         (binding (key-binding key-seq)))
    (and binding (symbolp binding) (commandp binding)
         (or (eq (get binding 'isearch-scroll) t)
	     (eq (get binding 'scroll-command) t))
         binding)))

(defalias 'isearch-other-control-char 'isearch-other-meta-char)

(defun isearch-other-meta-char (&optional arg)
  "Process a miscellaneous key sequence in Isearch mode.

Try to convert the current key-sequence to something usable in Isearch
mode, either by converting it with `function-key-map', downcasing a
key with C-<upper case>, or finding a \"scrolling command\" bound to
it.  \(In the last case, we may have to read more events.)  If so,
either unread the converted sequence or execute the command.

Otherwise, if `search-exit-option' is non-nil (the default) unread the
key-sequence and exit the search normally.  If it is the symbol
`edit', the search string is edited in the minibuffer and the meta
character is unread so that it applies to editing the string.

ARG is the prefix argument.  It will be transmitted through to the
scrolling command or to the command whose key-sequence exits
Isearch mode."
  (interactive "P")
  (let* ((key (if current-prefix-arg    ; not nec the same as ARG
                  (substring (this-command-keys) universal-argument-num-events)
                (this-command-keys)))
	 (main-event (aref key 0))
	 (keylist (listify-key-sequence key))
         scroll-command isearch-point)
    (cond ((and (= (length key) 1)
		(let ((lookup (lookup-key local-function-key-map key)))
		  (not (or (null lookup) (integerp lookup)
			   (keymapp lookup)))))
	   ;; Handle a function key that translates into something else.
	   ;; If the key has a global definition too,
	   ;; exit and unread the key itself, so its global definition runs.
	   ;; Otherwise, unread the translation,
	   ;; so that the translated key takes effect within isearch.
	   (cancel-kbd-macro-events)
	   (if (lookup-key global-map key)
	       (progn
		 (isearch-done)
		 (setq prefix-arg arg)
		 (apply 'isearch-unread keylist))
	     (setq keylist
		   (listify-key-sequence (lookup-key local-function-key-map key)))
	     (while keylist
	       (setq key (car keylist))
	       ;; If KEY is a printing char, we handle it here
	       ;; directly to avoid the input method and keyboard
	       ;; coding system translating it.
	       (if (and (integerp key)
			(>= key ?\s) (/= key 127) (< key 256))
		   (progn
		     (isearch-process-search-char key)
		     (setq keylist (cdr keylist)))
		 ;; As the remaining keys in KEYLIST can't be handled
		 ;; here, we must reread them.
		 (setq prefix-arg arg)
		 (apply 'isearch-unread keylist)
		 (setq keylist nil)))))
	  (
	   ;; Handle an undefined shifted control character
	   ;; by downshifting it if that makes it defined.
	   ;; (As read-key-sequence would normally do,
	   ;; if we didn't have a default definition.)
	   (let ((mods (event-modifiers main-event)))
	     (and (integerp main-event)
		  (memq 'shift mods)
		  (memq 'control mods)
		  (not (memq (lookup-key isearch-mode-map
					 (let ((copy (copy-sequence key)))
					   (aset copy 0
						 (- main-event
						    (- ?\C-\S-a ?\C-a)))
					   copy)
					 nil)
			     '(nil
			       isearch-other-control-char)))))
	   (setcar keylist (- main-event (- ?\C-\S-a ?\C-a)))
	   (cancel-kbd-macro-events)
	   (setq prefix-arg arg)
	   (apply 'isearch-unread keylist))
	  ((eq search-exit-option 'edit)
	   (setq prefix-arg arg)
	   (apply 'isearch-unread keylist)
	   (isearch-edit-string))
          ;; Handle a scrolling function.
          ((and isearch-allow-scroll
                (progn (setq key (isearch-reread-key-sequence-naturally keylist))
                       (setq keylist (listify-key-sequence key))
                       (setq main-event (aref key 0))
                       (setq scroll-command (isearch-lookup-scroll-key key))))
           ;; From this point onwards, KEY, KEYLIST and MAIN-EVENT hold a
           ;; complete key sequence, possibly as modified by function-key-map,
           ;; not merely the one or two event fragment which invoked
           ;; isearch-other-meta-char in the first place.
           (setq isearch-point (point))
           (setq prefix-arg arg)
           (command-execute scroll-command)
           (let ((ab-bel (isearch-string-out-of-window isearch-point)))
             (if ab-bel
                 (isearch-back-into-window (eq ab-bel 'above) isearch-point)
               (goto-char isearch-point)))
           (isearch-update))
	  ;; A mouse click on the isearch message starts editing the search string
	  ((and (eq (car-safe main-event) 'down-mouse-1)
		(window-minibuffer-p (posn-window (event-start main-event))))
	   ;; Swallow the up-event.
	   (read-event)
	   (isearch-edit-string))
	  (search-exit-option
	   (let (window)
	     (setq prefix-arg arg)
             (isearch-unread-key-sequence keylist)
             (setq main-event (car unread-command-events))

	     ;; If we got a mouse click event, that event contains the
	     ;; window clicked on. maybe it was read with the buffer
	     ;; it was clicked on.  If so, that buffer, not the current one,
	     ;; is in isearch mode.  So end the search in that buffer.

	     ;; ??? I have no idea what this if checks for, but it's
	     ;; obviously wrong for the case that a down-mouse event
	     ;; on another window invokes this function.  The event
	     ;; will contain the window clicked on and that window's
	     ;; buffer is certainly not always in Isearch mode.
	     ;;
	     ;; Leave the code in, but check for current buffer not
	     ;; being in Isearch mode for now, until someone tells
	     ;; what it's really supposed to do.
	     ;;
	     ;; --gerd 2001-08-10.

	     (if (and (not isearch-mode)
		      (listp main-event)
		      (setq window (posn-window (event-start main-event)))
		      (windowp window)
		      (or (> (minibuffer-depth) 0)
			  (not (window-minibuffer-p window))))
		 (with-current-buffer (window-buffer window)
		   (isearch-done)
		   (isearch-clean-overlays))
	       (isearch-done)
	       (isearch-clean-overlays)
               (setq prefix-arg arg))))
          (t;; otherwise nil
	   (isearch-process-search-string key key)))))

(defun isearch-quote-char ()
  "Quote special characters for incremental search."
  (interactive)
  (let ((char (read-quoted-char (isearch-message t))))
    ;; Assume character codes 0200 - 0377 stand for characters in some
    ;; single-byte character set, and convert them to Emacs
    ;; characters.
    (if (and isearch-regexp (= char ?\s))
	(if (subregexp-context-p isearch-string (length isearch-string))
	    (isearch-process-search-string "[ ]" " ")
	  (isearch-process-search-char char))
      (and enable-multibyte-characters
	   (>= char ?\200)
	   (<= char ?\377)
	   (setq char (unibyte-char-to-multibyte char)))
      (isearch-process-search-char char))))

(defun isearch-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (let ((char last-command-event))
    (if (= char ?\S-\ )
	(setq char ?\s))
    (if current-input-method
	(isearch-process-search-multibyte-characters char)
      (isearch-process-search-char char))))

(defun isearch-process-search-char (char)
  ;; * and ? are special in regexps when not preceded by \.
  ;; } and | are special in regexps when preceded by \.
  ;; Nothing special for + because it matches at least once.
  (cond
   ((memq char '(?* ??)) (isearch-fallback nil))
   ((eq   char ?\})      (isearch-fallback t t))
   ((eq   char ?|)       (isearch-fallback t nil t)))

  ;; Append the char to the search string, update the message and re-search.
  (isearch-process-search-string
   (char-to-string char)
   (if (>= char ?\200)
       (char-to-string char)
     (isearch-text-char-description char))))

(defun isearch-process-search-string (string message)
  (setq isearch-string (concat isearch-string string)
	isearch-message (concat isearch-message message))
  (isearch-search-and-update))


;; Search Ring

(defun isearch-ring-adjust1 (advance)
  ;; Helper for isearch-ring-adjust
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
	 (length (length ring))
	 (yank-pointer-name (if isearch-regexp
				'regexp-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (mod (+ (or yank-pointer (if advance 0 -1))
			 (if advance -1 1))
		      length)))
      (setq isearch-string (nth yank-pointer ring)
	    isearch-message (mapconcat 'isearch-text-char-description
				       isearch-string "")))))

(defun isearch-ring-adjust (advance)
  ;; Helper for isearch-ring-advance and isearch-ring-retreat
  (isearch-ring-adjust1 advance)
  (if search-ring-update
      (progn
	(isearch-search)
	(isearch-push-state)
	(isearch-update))
    ;; Otherwise, edit the search string instead.  Note that there is
    ;; no need to push the search state after isearch-edit-string here
    ;; since isearch-edit-string already pushes its state
    (isearch-edit-string)))

(defun isearch-ring-advance ()
  "Advance to the next search string in the ring."
  ;; This could be more general to handle a prefix arg, but who would use it.
  (interactive)
  (isearch-ring-adjust 'advance))

(defun isearch-ring-retreat ()
  "Retreat to the previous search string in the ring."
  (interactive)
  (isearch-ring-adjust nil))

(defun isearch-complete1 ()
  ;; Helper for isearch-complete and isearch-complete-edit
  ;; Return t if completion OK, nil if no completion exists.
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (completion-ignore-case case-fold-search)
         (completion (try-completion isearch-string ring)))
    (cond
     ((eq completion t)
      ;; isearch-string stays the same
      t)
     ((or completion ; not nil, must be a string
	  (= 0 (length isearch-string))) ; shouldn't have to say this
      (if (equal completion isearch-string)  ;; no extension?
	  (progn
	    (if completion-auto-help
		(with-output-to-temp-buffer "*Isearch completions*"
		  (display-completion-list
		   (all-completions isearch-string ring))))
	    t)
	(and completion
	     (setq isearch-string completion))))
     (t
      (message "No completion") ; waits a second if in minibuffer
      nil))))

(defun isearch-complete ()
  "Complete the search string from the strings on the search ring.
The completed string is then editable in the minibuffer.
If there is no completion possible, say so and continue searching."
  (interactive)
  (if (isearch-complete1)
      (progn (setq isearch-message
		   (mapconcat 'isearch-text-char-description
			      isearch-string ""))
	     (isearch-edit-string))
    ;; else
    (sit-for 1)
    (isearch-update)))

(defun isearch-complete-edit ()
  "Same as `isearch-complete' except in the minibuffer."
  (interactive)
  (setq isearch-string (field-string))
  (if (isearch-complete1)
      (progn
	(delete-field)
	(insert isearch-string))))


;; Message string

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; Generate and print the message string.
  (let ((cursor-in-echo-area ellipsis)
	(m isearch-message)
	(fail-pos (isearch-fail-pos t)))
    ;; Highlight failed part
    (when fail-pos
      (setq m (copy-sequence m))
      (add-text-properties fail-pos (length m) '(face isearch-fail) m)
      ;; Highlight failed trailing whitespace
      (when (string-match " +$" m)
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(face trailing-whitespace) m)))
    (setq m (concat
	     (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
	     m
	     (isearch-message-suffix c-q-hack ellipsis)))
    (if c-q-hack m (let ((message-log-max nil)) (message "%s" m)))))

(defun isearch-message-prefix (&optional _c-q-hack ellipsis nonincremental)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and isearch-error ellipsis
       (condition-case ()
	   (progn (re-search-forward isearch-string (point) t)
		  (setq isearch-error nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or isearch-success (setq ellipsis nil))
  (let ((m (concat (if isearch-success "" "failing ")
		   (if isearch-adjusted "pending " "")
		   (if (and isearch-wrapped
			    (not isearch-wrap-function)
			    (if isearch-forward
				(> (point) isearch-opoint)
			      (< (point) isearch-opoint)))
		       "over")
		   (if isearch-wrapped "wrapped ")
		   (if isearch-word "word " "")
		   (if isearch-regexp "regexp " "")
		   (if multi-isearch-next-buffer-current-function "multi " "")
		   (or isearch-message-prefix-add "")
		   (if nonincremental "search" "I-search")
		   (if isearch-forward "" " backward")
		   (if current-input-method
		       ;; Input methods for RTL languages use RTL
		       ;; characters for their title, and that messes
		       ;; up the display of search text after the prompt.
		       (bidi-string-mark-left-to-right
			(concat " [" current-input-method-title "]: "))
		     ": ")
		   )))
    (propertize (concat (upcase (substring m 0 1)) (substring m 1))
		'face 'minibuffer-prompt)))

(defun isearch-message-suffix (&optional c-q-hack _ellipsis)
  (concat (if c-q-hack "^Q" "")
	  (if isearch-error
	      (concat " [" isearch-error "]")
	    "")
	  (or isearch-message-suffix-add "")))


;; Searching

(defvar isearch-search-fun-function nil
  "Overrides the default `isearch-search-fun' behavior.
This variable's value should be a function, which will be called
with no arguments, and should return a function that takes three
arguments: STRING, BOUND, and NOERROR.

This returned function will be used by `isearch-search-string' to
search for the first occurrence of STRING or its translation.")

(defun isearch-search-fun ()
  "Return the function to use for the search.
Can be changed via `isearch-search-fun-function' for special needs."
  (if isearch-search-fun-function
      (funcall isearch-search-fun-function)
    (cond
     (isearch-word
      ;; Use lax versions to not fail at the end of the word while
      ;; the user adds and removes characters in the search string
      ;; (or when using nonincremental word isearch)
      (if (or isearch-nonincremental
	      (eq (length isearch-string)
		  (length (isearch-string-state (car isearch-cmds)))))
	  (if isearch-forward 'word-search-forward 'word-search-backward)
	(if isearch-forward 'word-search-forward-lax 'word-search-backward-lax)))
     (isearch-regexp
      (if isearch-forward 're-search-forward 're-search-backward))
     (t
      (if isearch-forward 'search-forward 'search-backward)))))

(defun isearch-search-string (string bound noerror)
  "Search for the first occurrence of STRING or its translation.
If found, move point to the end of the occurrence,
update the match data, and return point."
  (let* ((func (isearch-search-fun))
         (pos1 (save-excursion (funcall func string bound noerror)))
         pos2)
    (when (and
	   ;; Avoid "obsolete" warnings for translation-table-for-input.
	   (with-no-warnings
	     (char-table-p translation-table-for-input))
	   (multibyte-string-p string)
	   ;; Minor optimization.
	   (string-match-p "[^[:ascii:]]" string))
      (let ((translated
             (apply 'string
                    (mapcar (lambda (c)
                              (or
			       ;; Avoid "obsolete" warnings for
			       ;; translation-table-for-input.
			       (with-no-warnings
				 (aref translation-table-for-input c))
			       c))
                            string)))
            match-data)
        (when translated
          (save-match-data
            (save-excursion
              (if (setq pos2 (funcall func translated bound noerror))
                  (setq match-data (match-data t)))))
          (when (and pos2
                     (or (not pos1)
                         (if isearch-forward (< pos2 pos1) (> pos2 pos1))))
            (setq pos1 pos2)
            (set-match-data match-data)))))
    (when pos1
      ;; When using multiple buffers isearch, switch to the new buffer here,
      ;; because `save-excursion' above doesn't allow doing it inside funcall.
      (if (and multi-isearch-next-buffer-current-function
	       (buffer-live-p multi-isearch-current-buffer))
	  (switch-to-buffer multi-isearch-current-buffer))
      (goto-char pos1)
      pos1)))

(defun isearch-search ()
  ;; Do the search with the current search string.
  (if isearch-message-function
      (funcall isearch-message-function nil t)
    (isearch-message nil t))
  (if (and (eq isearch-case-fold-search t) search-upper-case)
      (setq isearch-case-fold-search
	    (isearch-no-upper-case-p isearch-string isearch-regexp)))
  (condition-case lossage
      (let ((inhibit-point-motion-hooks
	     (and (eq isearch-filter-predicate 'isearch-filter-visible)
		  search-invisible))
	    (inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search)
	    (search-spaces-regexp search-whitespace-regexp)
	    (retry t))
	(setq isearch-error nil)
	(while retry
	  (setq isearch-success
		(isearch-search-string isearch-string nil t))
	  ;; Clear RETRY unless the search predicate says
	  ;; to skip this search hit.
	  (if (or (not isearch-success)
		  (bobp) (eobp)
		  (= (match-beginning 0) (match-end 0))
		  (funcall isearch-filter-predicate
			   (match-beginning 0) (match-end 0)))
	      (setq retry nil)))
	(setq isearch-just-started nil)
	(if isearch-success
	    (setq isearch-other-end
		  (if isearch-forward (match-beginning 0) (match-end 0)))))

    (quit (isearch-unread ?\C-g)
	  (setq isearch-success nil))

    (invalid-regexp
     (setq isearch-error (car (cdr lossage)))
     (if (string-match
	  "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
	  isearch-error)
	 (setq isearch-error "incomplete input")))

    (search-failed
     (setq isearch-success nil)
     (setq isearch-error (nth 2 lossage)))

    (error
     ;; stack overflow in regexp search.
     (setq isearch-error (format "%s" lossage))))

  (if isearch-success
      nil
    ;; Ding if failed this time after succeeding last time.
    (and (isearch-success-state (car isearch-cmds))
	 (ding))
    (if (functionp (isearch-pop-fun-state (car isearch-cmds)))
        (funcall (isearch-pop-fun-state (car isearch-cmds)) (car isearch-cmds)))
    (goto-char (isearch-point-state (car isearch-cmds)))))


;; Called when opening an overlay, and we are still in isearch.
(defun isearch-open-overlay-temporary (ov)
  (if (not (null (overlay-get ov 'isearch-open-invisible-temporary)))
      ;; Some modes would want to open the overlays temporary during
      ;; isearch in their own way, they should set the
      ;; `isearch-open-invisible-temporary' to a function doing this.
      (funcall  (overlay-get ov 'isearch-open-invisible-temporary)  ov nil)
    ;; Store the values for the `invisible' and `intangible'
    ;; properties, and then set them to nil. This way the text hidden
    ;; by this overlay becomes visible.

    ;; Do we really need to set the `intangible' property to t? Can we
    ;; have the point inside an overlay with an `intangible' property?
    ;; In 19.34 this does not exist so I cannot test it.
    (overlay-put ov 'isearch-invisible (overlay-get ov 'invisible))
    (overlay-put ov 'isearch-intangible (overlay-get ov 'intangible))
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'intangible nil)))


;; This is called at the end of isearch.  It will open the overlays
;; that contain the latest match.  Obviously in case of a C-g the
;; point returns to the original location which surely is not contain
;; in any of these overlays, se we are safe in this case too.
(defun isearch-open-necessary-overlays (ov)
  (let ((inside-overlay (and  (> (point) (overlay-start ov))
			      (< (point) (overlay-end ov))))
	;; If this exists it means that the overlay was opened using
	;; this function, not by us tweaking the overlay properties.
	(fct-temp (overlay-get ov 'isearch-open-invisible-temporary)))
    (when (or inside-overlay (not fct-temp))
      ;; restore the values for the `invisible' and `intangible'
      ;; properties
      (overlay-put ov 'invisible (overlay-get ov 'isearch-invisible))
      (overlay-put ov 'intangible (overlay-get ov 'isearch-intangible))
      (overlay-put ov 'isearch-invisible nil)
      (overlay-put ov 'isearch-intangible nil))
    (if inside-overlay
	(funcall (overlay-get ov 'isearch-open-invisible)  ov)
      (if fct-temp
	  (funcall fct-temp ov t)))))

;; This is called when exiting isearch. It closes the temporary
;; opened overlays, except the ones that contain the latest match.
(defun isearch-clean-overlays ()
  (when isearch-opened-overlays
    (mapc 'isearch-open-necessary-overlays isearch-opened-overlays)
    (setq isearch-opened-overlays nil)))


(defun isearch-intersects-p (start0 end0 start1 end1)
  "Return t if regions START0..END0 and START1..END1 intersect."
  (or (and (>= start0 start1) (<  start0 end1))
      (and (>  end0 start1)   (<= end0 end1))
      (and (>= start1 start0) (<  start1 end0))
      (and (>  end1 start0)   (<= end1 end0))))


;; Verify if the current match is outside of each element of
;; `isearch-opened-overlays', if so close that overlay.

(defun isearch-close-unnecessary-overlays (begin end)
  (let ((overlays isearch-opened-overlays))
    (setq isearch-opened-overlays nil)
    (dolist (ov overlays)
      (if (isearch-intersects-p begin end (overlay-start ov) (overlay-end ov))
	  (push ov isearch-opened-overlays)
	(let ((fct-temp (overlay-get ov 'isearch-open-invisible-temporary)))
	  (if fct-temp
	      ;; If this exists it means that the overlay was opened
	      ;; using this function, not by us tweaking the overlay
	      ;; properties.
	      (funcall fct-temp ov t)
	    (overlay-put ov 'invisible (overlay-get ov 'isearch-invisible))
	    (overlay-put ov 'intangible (overlay-get ov 'isearch-intangible))
	    (overlay-put ov 'isearch-invisible nil)
	    (overlay-put ov 'isearch-intangible nil)))))))


(defun isearch-range-invisible (beg end)
  "Return t if all the text from BEG to END is invisible."
  (when (/= beg end)
    ;; Check that invisibility runs up to END.
    (save-excursion
      (goto-char beg)
      (let (;; can-be-opened keeps track if we can open some overlays.
	    (can-be-opened (eq search-invisible 'open))
	    ;; the list of overlays that could be opened
	    (crt-overlays nil))
	(when (and can-be-opened isearch-hide-immediately)
	  (isearch-close-unnecessary-overlays beg end))
	;; If the following character is currently invisible,
	;; skip all characters with that same `invisible' property value.
	;; Do that over and over.
	(while (and (< (point) end) (invisible-p (point)))
	  (if (invisible-p (get-text-property (point) 'invisible))
	      (progn
		(goto-char (next-single-property-change (point) 'invisible
							nil end))
		;; if text is hidden by an `invisible' text property
		;; we cannot open it at all.
		(setq can-be-opened nil))
	    (when can-be-opened
	      (let ((overlays (overlays-at (point)))
		    ov-list
		    o
		    invis-prop)
		(while overlays
		  (setq o (car overlays)
			invis-prop (overlay-get o 'invisible))
		  (if (invisible-p invis-prop)
		      (if (overlay-get o 'isearch-open-invisible)
			  (setq ov-list (cons o ov-list))
			;; We found one overlay that cannot be
			;; opened, that means the whole chunk
			;; cannot be opened.
			(setq can-be-opened nil)))
		  (setq overlays (cdr overlays)))
		(if can-be-opened
		    ;; It makes sense to append to the open
		    ;; overlays list only if we know that this is
		    ;; t.
		    (setq crt-overlays (append ov-list crt-overlays)))))
	    (goto-char (next-overlay-change (point)))))
	;; See if invisibility reaches up thru END.
	(if (>= (point) end)
	    (if (and can-be-opened (consp crt-overlays))
		(progn
		  (setq isearch-opened-overlays
			(append isearch-opened-overlays crt-overlays))
		  (mapc 'isearch-open-overlay-temporary crt-overlays)
		  nil)
	      (setq isearch-hidden t)))))))

(defun isearch-filter-visible (beg end)
  "Test whether the current search hit is visible at least partially.
Return non-nil if the text from BEG to END is visible to Isearch as
determined by `isearch-range-invisible' unless invisible text can be
searched too when `search-invisible' is t."
  (or (eq search-invisible t)
      (not (isearch-range-invisible beg end))))


;; General utilities

(defun isearch-no-upper-case-p (string regexp-flag)
  "Return t if there are no upper case chars in STRING.
If REGEXP-FLAG is non-nil, disregard letters preceded by `\\' (but not `\\\\')
since they have special meaning in a regexp."
  (let (quote-flag (i 0) (len (length string)) found)
    (while (and (not found) (< i len))
      (let ((char (aref string i)))
	(if (and regexp-flag (eq char ?\\))
	    (setq quote-flag (not quote-flag))
	  (if (and (not quote-flag) (not (eq char (downcase char))))
	      (setq found t))
	  (setq quote-flag nil)))
      (setq i (1+ i)))
    (not (or found
             ;; Even if there's no uppercase char, we want to detect the use
             ;; of [:upper:] or [:lower:] char-class, which indicates
             ;; clearly that the user cares about case distinction.
             (and regexp-flag (string-match "\\[:\\(upp\\|low\\)er:]" string)
                  (condition-case err
                      (progn
                        (string-match (substring string 0 (match-beginning 0))
                                      "")
                        nil)
                    (invalid-regexp
                     (equal "Unmatched [ or [^" (cadr err)))))))))

;; Portability functions to support various Emacs versions.

(defun isearch-text-char-description (c)
  (cond
   ((< c ?\s) (propertize (format "^%c" (+ c 64)) 'face 'escape-glyph))
   ((= c ?\^?) (propertize "^?" 'face 'escape-glyph))
   (t (char-to-string c))))

;; General function to unread characters or events.
;; Also insert them in a keyboard macro being defined.
(defun isearch-unread (&rest char-or-events)
  (mapc 'store-kbd-macro-event char-or-events)
  (setq unread-command-events
	(append char-or-events unread-command-events)))


;; Highlighting

(defvar isearch-overlay nil)

(defun isearch-highlight (beg end)
  (if search-highlight
      (if isearch-overlay
	  ;; Overlay already exists, just move it.
	  (move-overlay isearch-overlay beg end (current-buffer))
	;; Overlay doesn't exist, create it.
	(setq isearch-overlay (make-overlay beg end))
	;; 1001 is higher than lazy's 1000 and ediff's 100+
	(overlay-put isearch-overlay 'priority 1001)
	(overlay-put isearch-overlay 'face isearch-face))))

(defun isearch-dehighlight ()
  (when isearch-overlay
    (delete-overlay isearch-overlay)))

;; isearch-lazy-highlight feature
;; by Bob Glickstein <http://www.zanshin.com/~bobg/>

;; When active, *every* match for the current search string is
;; highlighted: the current one using the normal isearch match color
;; and all the others using `isearch-lazy-highlight'.  The extra
;; highlighting makes it easier to anticipate where the cursor will
;; land each time you press C-s or C-r to repeat a pending search.
;; Highlighting of these additional matches happens in a deferred
;; fashion using "idle timers," so the cycles needed do not rob
;; isearch of its usual snappy response.

;; IMPLEMENTATION NOTE: This depends on some isearch internals.
;; Specifically:
;;  - `isearch-update' is expected to be called (at least) every time
;;    the search string or window-start changes;
;;  - `isearch-string' is expected to contain the current search
;;    string as entered by the user;
;;  - the type of the current search is expected to be given by
;;    `isearch-word' and `isearch-regexp';
;;  - the direction of the current search is expected to be given by
;;    `isearch-forward';
;;  - the variable `isearch-error' is expected to be true
;;    only if `isearch-string' is an invalid regexp.

(defvar isearch-lazy-highlight-overlays nil)
(defvar isearch-lazy-highlight-wrapped nil)
(defvar isearch-lazy-highlight-start-limit nil)
(defvar isearch-lazy-highlight-end-limit nil)
(defvar isearch-lazy-highlight-start nil)
(defvar isearch-lazy-highlight-end nil)
(defvar isearch-lazy-highlight-timer nil)
(defvar isearch-lazy-highlight-last-string nil)
(defvar isearch-lazy-highlight-window nil)
(defvar isearch-lazy-highlight-window-start nil)
(defvar isearch-lazy-highlight-window-end nil)
(defvar isearch-lazy-highlight-case-fold-search nil)
(defvar isearch-lazy-highlight-regexp nil)
(defvar isearch-lazy-highlight-space-regexp nil)
(defvar isearch-lazy-highlight-word nil)
(defvar isearch-lazy-highlight-forward nil)
(defvar isearch-lazy-highlight-error nil)

(defun lazy-highlight-cleanup (&optional force)
  "Stop lazy highlighting and remove extra highlighting from current buffer.
FORCE non-nil means do it whether or not `lazy-highlight-cleanup'
is nil.  This function is called when exiting an incremental search if
`lazy-highlight-cleanup' is non-nil."
  (interactive '(t))
  (if (or force lazy-highlight-cleanup)
      (while isearch-lazy-highlight-overlays
        (delete-overlay (car isearch-lazy-highlight-overlays))
        (setq isearch-lazy-highlight-overlays
              (cdr isearch-lazy-highlight-overlays))))
  (when isearch-lazy-highlight-timer
    (cancel-timer isearch-lazy-highlight-timer)
    (setq isearch-lazy-highlight-timer nil)))

(define-obsolete-function-alias 'isearch-lazy-highlight-cleanup
                                'lazy-highlight-cleanup
                                "22.1")

(defun isearch-lazy-highlight-new-loop (&optional beg end)
  "Cleanup any previous `lazy-highlight' loop and begin a new one.
BEG and END specify the bounds within which highlighting should occur.
This is called when `isearch-update' is invoked (which can cause the
search string to change or the window to scroll).  It is also used
by other Emacs features."
  (when (and (null executing-kbd-macro)
             (sit-for 0)         ;make sure (window-start) is credible
             (or (not (equal isearch-string
                             isearch-lazy-highlight-last-string))
                 (not (eq (selected-window)
                          isearch-lazy-highlight-window))
		 (not (eq isearch-lazy-highlight-case-fold-search
			  isearch-case-fold-search))
		 (not (eq isearch-lazy-highlight-regexp
			  isearch-regexp))
		 (not (eq isearch-lazy-highlight-word
			  isearch-word))
                 (not (= (window-start)
                         isearch-lazy-highlight-window-start))
                 (not (= (window-end)   ; Window may have been split/joined.
			 isearch-lazy-highlight-window-end))
		 (not (eq isearch-forward
			  isearch-lazy-highlight-forward))
		 ;; In case we are recovering from an error.
		 (not (equal isearch-error
			     isearch-lazy-highlight-error))))
    ;; something important did indeed change
    (lazy-highlight-cleanup t) ;kill old loop & remove overlays
    (setq isearch-lazy-highlight-error isearch-error)
    ;; It used to check for `(not isearch-error)' here, but actually
    ;; lazy-highlighting might find matches to highlight even when
    ;; `isearch-error' is non-nil.  (Bug#9918)
    (setq isearch-lazy-highlight-start-limit beg
	  isearch-lazy-highlight-end-limit end)
    (setq isearch-lazy-highlight-window       (selected-window)
	  isearch-lazy-highlight-window-start (window-start)
	  isearch-lazy-highlight-window-end   (window-end)
	  isearch-lazy-highlight-start        (point)
	  isearch-lazy-highlight-end          (point)
	  isearch-lazy-highlight-wrapped      nil
	  isearch-lazy-highlight-last-string  isearch-string
	  isearch-lazy-highlight-case-fold-search isearch-case-fold-search
	  isearch-lazy-highlight-regexp       isearch-regexp
	  isearch-lazy-highlight-space-regexp search-whitespace-regexp
	  isearch-lazy-highlight-word         isearch-word
	  isearch-lazy-highlight-forward      isearch-forward)
      (unless (equal isearch-string "")
	(setq isearch-lazy-highlight-timer
	      (run-with-idle-timer lazy-highlight-initial-delay nil
				   'isearch-lazy-highlight-update)))))

(defun isearch-lazy-highlight-search ()
  "Search ahead for the next or previous match, for lazy highlighting.
Attempt to do the search exactly the way the pending Isearch would."
  (condition-case nil
      (let ((case-fold-search isearch-lazy-highlight-case-fold-search)
	    (isearch-regexp isearch-lazy-highlight-regexp)
	    (search-spaces-regexp isearch-lazy-highlight-space-regexp)
	    (isearch-word isearch-lazy-highlight-word)
	    (search-invisible nil)	; don't match invisible text
	    (retry t)
	    (success nil)
	    (isearch-forward isearch-lazy-highlight-forward)
	    (bound (if isearch-lazy-highlight-forward
		       (min (or isearch-lazy-highlight-end-limit (point-max))
			    (if isearch-lazy-highlight-wrapped
				isearch-lazy-highlight-start
			      (window-end)))
		     (max (or isearch-lazy-highlight-start-limit (point-min))
			  (if isearch-lazy-highlight-wrapped
			      isearch-lazy-highlight-end
			    (window-start))))))
	;; Use a loop like in `isearch-search'.
	(while retry
	  (setq success (isearch-search-string
			 isearch-lazy-highlight-last-string bound t))
	  ;; Clear RETRY unless the search predicate says
	  ;; to skip this search hit.
	  (if (or (not success)
		  (= (point) bound) ; like (bobp) (eobp) in `isearch-search'.
		  (= (match-beginning 0) (match-end 0))
		  (funcall isearch-filter-predicate
			   (match-beginning 0) (match-end 0)))
	      (setq retry nil)))
	success)
    (error nil)))

(defun isearch-lazy-highlight-update ()
  "Update highlighting of other matches for current search."
  (let ((max lazy-highlight-max-at-a-time)
        (looping t)
        nomore)
    (with-local-quit
      (save-selected-window
	(if (and (window-live-p isearch-lazy-highlight-window)
		 (not (eq (selected-window) isearch-lazy-highlight-window)))
	    (select-window isearch-lazy-highlight-window))
	(save-excursion
	  (save-match-data
	    (goto-char (if isearch-lazy-highlight-forward
			   isearch-lazy-highlight-end
			 isearch-lazy-highlight-start))
	    (while looping
	      (let ((found (isearch-lazy-highlight-search)))
		(when max
		  (setq max (1- max))
		  (if (<= max 0)
		      (setq looping nil)))
		(if found
		    (let ((mb (match-beginning 0))
			  (me (match-end 0)))
		      (if (= mb me)	;zero-length match
			  (if isearch-lazy-highlight-forward
			      (if (= mb (if isearch-lazy-highlight-wrapped
					    isearch-lazy-highlight-start
					  (window-end)))
				  (setq found nil)
				(forward-char 1))
			    (if (= mb (if isearch-lazy-highlight-wrapped
					  isearch-lazy-highlight-end
					(window-start)))
				(setq found nil)
			      (forward-char -1)))

			;; non-zero-length match
			(let ((ov (make-overlay mb me)))
			  (push ov isearch-lazy-highlight-overlays)
			  ;; 1000 is higher than ediff's 100+,
			  ;; but lower than isearch main overlay's 1001
			  (overlay-put ov 'priority 1000)
			  (overlay-put ov 'face lazy-highlight-face)
			  (overlay-put ov 'window (selected-window))))
		      (if isearch-lazy-highlight-forward
			  (setq isearch-lazy-highlight-end (point))
			(setq isearch-lazy-highlight-start (point)))))

		;; not found or zero-length match at the search bound
		(if (not found)
		    (if isearch-lazy-highlight-wrapped
			(setq looping nil
			      nomore  t)
		      (setq isearch-lazy-highlight-wrapped t)
		      (if isearch-lazy-highlight-forward
			  (progn
			    (setq isearch-lazy-highlight-end (window-start))
			    (goto-char (max (or isearch-lazy-highlight-start-limit (point-min))
					    (window-start))))
			(setq isearch-lazy-highlight-start (window-end))
			(goto-char (min (or isearch-lazy-highlight-end-limit (point-max))
					(window-end))))))))
	    (unless nomore
	      (setq isearch-lazy-highlight-timer
		    (run-at-time lazy-highlight-interval nil
				 'isearch-lazy-highlight-update)))))))))

(defun isearch-resume (string regexp word forward message case-fold)
  "Resume an incremental search.
STRING is the string or regexp searched for.
REGEXP non-nil means the resumed search was a regexp search.
WORD non-nil means resume a word search.
FORWARD non-nil means resume a forward search.
MESSAGE is the echo-area message recorded for the search resumed.
CASE-FOLD non-nil means the search was case-insensitive."
  (isearch-mode forward regexp nil nil word)
  (setq isearch-string string
	isearch-message message
	isearch-case-fold-search case-fold)
  (isearch-search)
  (isearch-update))

;;; isearch.el ends here
