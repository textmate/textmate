;;; follow.el --- synchronize windows showing the same buffer

;; Copyright (C) 1995-1997, 1999, 2001-2012 Free Software Foundation, Inc.

;; Author: Anders Lindgren <andersl@andersl.com>
;; Maintainer: FSF (Anders' email bounces, Sep 2005)
;; Created: 1995-05-25
;; Keywords: display, window, minor-mode, convenience

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

;;{{{ Documentation

;; `Follow mode' is a minor mode for Emacs and XEmacs that
;; combines windows into one tall virtual window.
;;
;; The feeling of a "virtual window" has been accomplished by the use
;; of two major techniques:
;;
;; * The windows always display adjacent sections of the buffer.
;;   This means that whenever one window is moved, all the
;;   others will follow.  (Hence the name Follow mode.)
;;
;; * Should the point (cursor) end up outside a window, another
;;   window displaying that point is selected, if possible.  This
;;   makes it possible to walk between windows using normal cursor
;;   movement commands.
;;
;; Follow mode comes to its prime when a large screen and two
;; side-by-side window are used.  The user can, with the help of Follow
;; mode, use two full-height windows as though they are one.
;; Imagine yourself editing a large function, or section of text,
;; and being able to use 144 lines instead of the normal 72... (your
;; mileage may vary).

;; To test this package, make sure `follow' is loaded, or will be
;; autoloaded when activated (see below).  Then do the following:
;;
;; * Find your favorite file (preferably a long one).
;;
;; * Resize Emacs so that it will be wide enough for two full size
;;   columns.  Delete the other windows and split the window with
;;   the commands `C-x 1 C-x 3'.
;;
;; * Give the command:
;;	M-x follow-mode <RETURN>
;;
;; * Now the display should look something like (assuming the text "71"
;;   is on line 71):
;;
;;		    +----------+----------+
;;		    |1         |73        |
;;		    |2         |74        |
;;		    |3         |75        |
;;		         ...        ...
;;		    |71        |143       |
;;		    |72        |144       |
;;		    +----------+----------+
;;
;;   As you can see, the right-hand window starts at line 73, the line
;;   immediately below the end of the left-hand window.  As long as
;;   `follow-mode' is active, the two windows will follow each other!
;;
;; * Play around and enjoy! Scroll one window and watch the other.
;;   Jump to the beginning or end.  Press `Cursor down' at the last
;;   line of the left-hand window.  Enter new lines into the
;;   text.  Enter long lines spanning several lines, or several
;;   windows.
;;
;; * Should you find `Follow' mode annoying, just type
;;	M-x follow-mode <RETURN>
;;   to turn it off.


;; The command `follow-delete-other-windows-and-split' maximizes the
;; visible area of the current buffer.
;;
;; I recommend adding it, and `follow-mode', to hotkeys in the global
;; key map.  To do so, add the following lines (replacing `[f7]' and
;; `[f8]' with your favorite keys) to the init file:
;;
;; (global-set-key [f8] 'follow-mode)
;; (global-set-key [f7] 'follow-delete-other-windows-and-split)


;; There exist two system variables that control the appearance of
;; lines wider than the window containing them.  The default is to
;; truncate long lines whenever a window isn't as wide as the frame.
;;
;; To make sure lines are never truncated, please place the following
;; lines in your init file:
;;
;; (setq truncate-lines nil)
;; (setq truncate-partial-width-windows nil)


;; Since the display of XEmacs is pixel-oriented, a line could be
;; clipped in half at the bottom of the window.
;;
;; To make XEmacs avoid clipping (normal) lines, please place the
;; following line in your init-file:
;;
;; (setq pixel-vertical-clip-threshold 30)


;; The correct way to configure Follow mode, or any other mode for
;; that matter, is to create one or more functions that do
;; whatever you would like to do.  These functions are then added to
;; a hook.
;;
;; When `Follow' mode is activated, functions stored in the hook
;; `follow-mode-hook' are called.  When it is deactivated
;; `follow-mode-off-hook' is run.
;;
;; The keymap `follow-key-map' contains key bindings activated by
;; `follow-mode'.
;;
;; Example:
;; (add-hook 'follow-mode-hook 'my-follow-mode-hook)
;;
;; (defun my-follow-mode-hook ()
;;    (define-key follow-mode-map "\C-ca" 'your-favorite-function)
;;    (define-key follow-mode-map "\C-cb" 'another-function))


;; Usage:
;;
;; To activate, issue the command "M-x follow-mode"
;; and press Return.  To deactivate, do it again.
;;
;; The following is a list of commands useful when follow-mode is active.
;;
;;	follow-scroll-up			 C-c . C-v
;;		Scroll text in a Follow mode window chain up.
;;
;;	follow-scroll-down			 C-c . v
;;		Like `follow-scroll-up', but in the other direction.
;;
;;	follow-delete-other-windows-and-split	 C-c . 1
;;		Maximize the visible area of the current buffer,
;;		and enter Follow mode. 	This is a very convenient
;;		way to start Follow mode, hence we recommend that
;;		this command be added to the global keymap.
;;
;;	follow-recenter				 C-c . C-l
;;		Place the point in the center of the middle window,
;;		or a specified number of lines from either top or bottom.
;;
;;	follow-switch-to-buffer			 C-c . b
;;		Switch buffer in all windows displaying the current buffer
;;		in this frame.
;;
;;	follow-switch-to-buffer-all		 C-c . C-b
;;		Switch buffer in all windows in the selected frame.
;;
;;	follow-switch-to-current-buffer-all
;;		Show the current buffer in all windows on the current
;;		frame and turn on `follow-mode'.
;;
;;	follow-first-window			 C-c . <
;;		Select the first window in the frame showing the same buffer.
;;
;;	follow-last-window			 C-c . >
;;		Select the last window in the frame showing the same buffer.
;;
;;	follow-next-window			 C-c . n
;;		Select the next window in the frame showing the same buffer.
;;
;;	follow-previous-window			 C-c . p
;;		Select the previous window showing the same buffer.


;; Well, it seems ok, but what if I really want to look at two different
;; positions in the text? Here are two simple methods to use:
;;
;; 1) Use multiple frames; `follow' mode only affects windows displayed
;;    in the same frame. (My apologies to you who can't use frames.)
;;
;; 2) Bind `follow-mode' to key so you can turn it off whenever
;;    you want to view two locations.  Of course, `follow' mode can
;;    be reactivated by hitting the same key again.
;;
;;    Example from my ~/.emacs:
;;	(global-set-key [f8] 'follow-mode)


;; Implementation:
;;
;; In an ideal world, follow mode would have been implemented in the
;; kernel of the display routines, making sure that the windows (using
;; follow mode) ALWAYS are aligned.  On planet Earth, however, we must
;; accept a solution where we ALMOST ALWAYS can make sure that the
;; windows are aligned.
;;
;; Follow mode does this in three places:
;; 1) After each user command.
;; 2) After a process output has been performed.
;; 3) When a scrollbar has been moved.
;;
;; This will cover most situations. (Let me know if there are other
;; situations that should be covered.)
;;
;; Note that only the selected window is checked, for the reason of
;; efficiency and code complexity.  (I.e. it is possible to make a
;; non-selected window unaligned.  It will, however, pop right back
;; when it is selected.)

;;}}}

;;; Code:

;;{{{ Preliminaries

;; Make the compiler shut up!
;; There are two strategies:
;; 1) Shut warnings off completely.
;; 2) Handle each warning separately.
;;
;; Since I would like to see real errors, I've selected the latter
;; method.
;;
;; The problem with undefined variables and functions has been solved
;; by using `set', `symbol-value' and `symbol-function' rather than
;; `setq' and direct references to variables and functions.
;;
;; For example:
;;	(if (boundp 'foo)   ... (symbol-value 'foo) )
;;	(set 'foo ...)   <-- XEmacs doesn't fall for this one.
;;	(funcall (symbol-function 'set) 'bar ...)
;;
;; Note: When this file is interpreted, `eval-when-compile' is
;; evaluated.  Since it doesn't hurt to evaluate it, but it is a bit
;; annoying, we test if the byte-compiler has been loaded.  This can,
;; of course, lead to some occasional unintended evaluation...
;;
;; Should someone come up with a better solution, please let me
;; know.

(require 'easymenu)

(eval-when-compile
  (if (or (featurep 'bytecomp)
	  (featurep 'byte-compile))
      (cond ((featurep 'xemacs)
	     ;; Make XEmacs shut up!  I'm using standard Emacs
	     ;; functions, they are NOT obsolete!
	     (if (eq (get 'force-mode-line-update 'byte-compile)
		     'byte-compile-obsolete)
		 (put 'force-mode-line-update 'byte-compile 'nil))
	     (if (eq (get 'frame-first-window 'byte-compile)
		     'byte-compile-obsolete)
		 (put 'frame-first-window 'byte-compile 'nil))))))

;;}}}
;;{{{ Variables

(defgroup follow nil
  "Synchronize windows showing the same buffer."
  :prefix "follow-"
  :group 'windows
  :group 'convenience)

(defcustom follow-mode-hook nil
  "Normal hook run by `follow-mode'."
  :type 'hook
  :group 'follow)

(defcustom follow-mode-off-hook nil
  "Hooks to run when Follow mode is turned off."
  :type 'hook
  :group 'follow)
(make-obsolete-variable 'follow-mode-off-hook 'follow-mode-hook "22.2")

;;{{{ Keymap/Menu

;; Define keys for the follow-mode minor mode map and replace some
;; functions in the global map.  All `follow' mode special functions
;; can be found on (the somewhat cumbersome) "C-c . <key>"
;; (Control-C dot <key>). (As of Emacs 19.29 the keys
;; C-c <punctuation character> are reserved for minor modes.)
;;
;; To change the prefix, redefine `follow-mode-prefix' before
;; `follow' is loaded, or see the section on `follow-mode-hook'
;; above for an example of how to bind the keys the way you like.
;;
;; Please note that the keymap is defined the first time this file is
;; loaded.  Also note that the only valid way to manipulate the
;; keymap is to use `define-key'.  Don't change it using `setq' or
;; similar!

(defcustom follow-mode-prefix "\C-c."
  "Prefix key to use for follow commands in Follow mode.
The value of this variable is checked as part of loading Follow mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'follow)

(defvar follow-mode-map
  (let ((mainmap (make-sparse-keymap))
        (map (make-sparse-keymap)))
    (define-key map "\C-v"	'follow-scroll-up)
    (define-key map "\M-v"	'follow-scroll-down)
    (define-key map "v"		'follow-scroll-down)
    (define-key map "1"		'follow-delete-other-windows-and-split)
    (define-key map "b"		'follow-switch-to-buffer)
    (define-key map "\C-b"	'follow-switch-to-buffer-all)
    (define-key map "\C-l"	'follow-recenter)
    (define-key map "<"		'follow-first-window)
    (define-key map ">"		'follow-last-window)
    (define-key map "n"		'follow-next-window)
    (define-key map "p"		'follow-previous-window)

    (define-key mainmap follow-mode-prefix map)

    ;; Replace the standard `end-of-buffer', when in Follow mode.  (I
    ;; don't see the point in trying to replace every function that
    ;; could be enhanced in Follow mode.  End-of-buffer is a special
    ;; case since it is very simple to define and it greatly enhances
    ;; the look and feel of Follow mode.)
    (define-key mainmap [remap end-of-buffer] 'follow-end-of-buffer)

    mainmap)
  "Minor mode keymap for Follow mode.")

;; When the mode is not activated, only one item is visible to activate
;; the mode.
(defun follow-menu-filter (menu)
  (if (bound-and-true-p follow-mode)
      menu
    '(["Follow mode"	follow-mode
       :style toggle :selected follow-mode])))

;; If there is a `tools' menu, we use it.  However, we can't add a
;; minor-mode specific item to it (it's broken), so we make the
;; contents ghosted when not in use, and add ourselves to the
;; global map.
(easy-menu-add-item nil '("Tools")
  '("Follow"
    ;; The Emacs code used to just gray out operations when follow-mode was
    ;; not enabled, whereas the XEmacs code used to remove it altogether.
    ;; Not sure which is preferable, but clearly the preference should not
    ;; depend on the flavor.
    :filter follow-menu-filter
    ["Scroll Up"	follow-scroll-up	follow-mode]
    ["Scroll Down"	follow-scroll-down	follow-mode]
    "--"
    ["Delete Other Windows and Split" follow-delete-other-windows-and-split follow-mode]
    "--"
    ["Switch To Buffer"	follow-switch-to-buffer	follow-mode]
    ["Switch To Buffer (all windows)" follow-switch-to-buffer-all follow-mode]
    "--"
    ["First Window"	follow-first-window	follow-mode]
    ["Last Window"	follow-last-window	follow-mode]
    ["Next Window"	follow-next-window	follow-mode]
    ["Previous Window"	follow-previous-window	follow-mode]
    "--"
    ["Recenter"		follow-recenter		follow-mode]
    "--"
    ["Follow mode"	follow-mode :style toggle :selected follow-mode]))

;;}}}

(defcustom follow-mode-line-text " Follow"
  "Text shown in the mode line when Follow mode is active.
Defaults to \" Follow\".  Examples of other values
are \" Fw\", or simply \"\"."
  :type 'string
  :group 'follow)

(defcustom follow-auto nil
  "Non-nil activates Follow mode whenever a file is loaded."
  :type 'boolean
  :group 'follow)

(defcustom follow-intercept-processes (fboundp 'start-process)
  "When non-nil, Follow mode will monitor process output."
  :type 'boolean
  :group 'follow)

(defvar follow-avoid-tail-recenter-p (not (featurep 'xemacs))
  "*When non-nil, patch Emacs so that tail windows won't be recentered.

A \"tail window\" is a window that displays only the end of
the buffer.  Normally it is practical for the user that empty
windows are recentered automatically.  However, when using
Follow mode it breaks the display when the end is displayed
in a window \"above\" the last window.  This is for
example the case when displaying a short page in info.

Must be set before Follow mode is loaded.

Please note that it is not possible to fully prevent Emacs from
recentering empty windows.  Please report if you find a repeatable
situation in which Emacs recenters empty windows.

XEmacs, as of 19.12, does not recenter windows, good!")

(defvar follow-cache-command-list
  '(next-line previous-line forward-char backward-char)
  "List of commands that don't require recalculation.

In order to be able to use the cache, a command should not change the
contents of the buffer, nor should it change selected window or current
buffer.

The commands in this list are checked at load time.

To mark other commands as suitable for caching, set the symbol
property `follow-mode-use-cache' to non-nil.")

(defvar follow-debug nil
  "*Non-nil when debugging Follow mode.")


;; Internal variables:

(defvar follow-internal-force-redisplay nil
  "True when Follow mode should redisplay the windows.")

(defvar follow-process-filter-alist '()
  "The original filters for processes intercepted by Follow mode.")

(defvar follow-active-menu nil
  "The menu visible when Follow mode is active.")

(defvar follow-deactive-menu nil
  "The menu visible when Follow mode is deactivated.")

(defvar follow-inside-post-command-hook nil
  "Non-nil when inside Follow modes `post-command-hook'.
Used by `follow-window-size-change'.")

(defvar follow-windows-start-end-cache nil
  "Cache used by `follow-window-start-end'.")

;;}}}
;;{{{ Debug messages

;; This inline function must be as small as possible!
;; Maybe we should define a macro that expands to nil if
;; the variable is not set.

(defsubst follow-debug-message (&rest args)
  "Like `message', but only active when `follow-debug' is non-nil."
  (if (and (boundp 'follow-debug) follow-debug)
      (apply 'message args)))

;;}}}
;;{{{ Cache

(dolist (cmd follow-cache-command-list)
  (put cmd 'follow-mode-use-cache t))

;;}}}

;;{{{ The mode

;;;###autoload
(defun turn-on-follow-mode ()
  "Turn on Follow mode.  Please see the function `follow-mode'."
  (follow-mode 1))


;;;###autoload
(defun turn-off-follow-mode ()
  "Turn off Follow mode.  Please see the function `follow-mode'."
  (follow-mode -1))

(put 'follow-mode 'permanent-local t)
;;;###autoload
(define-minor-mode follow-mode
  "Toggle Follow mode.
With a prefix argument ARG, enable Follow mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Follow mode is a minor mode that combines windows into one tall
virtual window.  This is accomplished by two main techniques:

* The windows always displays adjacent sections of the buffer.
  This means that whenever one window is moved, all the
  others will follow.  (Hence the name Follow mode.)

* Should the point (cursor) end up outside a window, another
  window displaying that point is selected, if possible.  This
  makes it possible to walk between windows using normal cursor
  movement commands.

Follow mode comes to its prime when used on a large screen and two
side-by-side windows are used.  The user can, with the help of Follow
mode, use two full-height windows as though they would have been
one.  Imagine yourself editing a large function, or section of text,
and being able to use 144 lines instead of the normal 72... (your
mileage may vary).

To split one large window into two side-by-side windows, the commands
`\\[split-window-right]' or \
`M-x follow-delete-other-windows-and-split' can be used.

Only windows displayed in the same frame follow each other.

If the variable `follow-intercept-processes' is non-nil, Follow mode
will listen to the output of processes and redisplay accordingly.
\(This is the default.)

This command runs the normal hook `follow-mode-hook'.

Keys specific to Follow mode:
\\{follow-mode-map}"
  :keymap follow-mode-map
  (when (and follow-mode follow-intercept-processes)
    (follow-intercept-process-output))
  (cond (follow-mode ; On
         ;; XEmacs: If this is non-nil, the window will scroll before
         ;; the point will have a chance to get into the next window.
         (when (boundp 'scroll-on-clipped-lines)
	   (setq scroll-on-clipped-lines nil))
         (force-mode-line-update)
         (add-hook 'post-command-hook 'follow-post-command-hook t))

        ((not follow-mode) ; Off
         (force-mode-line-update))))

;;}}}
;;{{{ Find file hook

;; This will start follow-mode whenever a new file is loaded, if
;; the variable `follow-auto' is non-nil.

(add-hook 'find-file-hook 'follow-find-file-hook t)

(defun follow-find-file-hook ()
  "Find-file hook for Follow mode.  See the variable `follow-auto'."
  (if follow-auto (follow-mode t)))

;;}}}

;;{{{ User functions

;;;
;;; User functions usable when in Follow mode.
;;;

;;{{{ Scroll

;; `scroll-up' and `-down', but for windows in Follow mode.
;;
;; Almost like the real thing, except when the cursor ends up outside
;; the top or bottom...  In our case however, we end up outside the
;; window and hence we are recentered.  Should we let `recenter' handle
;; the point position we would never leave the selected window.  To do
;; it ourselves we would need to do our own redisplay, which is easier
;; said than done.  (Why didn't I do a real display abstraction from
;; the beginning?)
;;
;; We must sometimes set `follow-internal-force-redisplay', otherwise
;; our post-command-hook will move our windows back into the old
;; position...  (This would also be corrected if we would have had a
;; good redisplay abstraction.)

(defun follow-scroll-up (&optional arg)
  "Scroll text in a Follow mode window chain up.

If called with no ARG, the `next-screen-context-lines' last lines of
the bottom window in the chain will be visible in the top window.

If called with an argument, scroll ARG lines up.
Negative ARG means scroll downward.

Works like `scroll-up' when not in Follow mode."
  (interactive "P")
  (cond ((not (and (boundp 'follow-mode) follow-mode))
	 (scroll-up arg))
	(arg
	 (save-excursion (scroll-up arg))
	 (setq follow-internal-force-redisplay t))
	(t
	 (let* ((windows (follow-all-followers))
		(end (window-end (car (reverse windows)))))
	   (if (eq end (point-max))
	       (signal 'end-of-buffer nil)
	     (select-window (car windows))
	     ;; `window-end' might return nil.
	     (if end
		 (goto-char end))
	     (vertical-motion (- next-screen-context-lines))
	     (set-window-start (car windows) (point)))))))


(defun follow-scroll-down (&optional arg)
  "Scroll text in a Follow mode window chain down.

If called with no ARG, the `next-screen-context-lines' top lines of
the top window in the chain will be visible in the bottom window.

If called with an argument, scroll ARG lines down.
Negative ARG means scroll upward.

Works like `scroll-up' when not in Follow mode."
  (interactive "P")
  (cond ((not (and (boundp 'follow-mode) follow-mode))
	 (scroll-up arg))
	(arg
	 (save-excursion (scroll-down arg)))
	(t
	 (let* ((windows (follow-all-followers))
		(win (car (reverse windows)))
		(start (window-start (car windows))))
	   (if (eq start (point-min))
	       (signal 'beginning-of-buffer nil)
	     (select-window win)
	     (goto-char start)
	     (vertical-motion (- (- (window-height win)
				    (if header-line-format 2 1)
				    next-screen-context-lines)))
	     (set-window-start win (point))
	     (goto-char start)
	     (vertical-motion (- next-screen-context-lines 1))
	     (setq follow-internal-force-redisplay t))))))

;;}}}
;;{{{ Buffer

;;;###autoload
(defun follow-delete-other-windows-and-split (&optional arg)
  "Create two side by side windows and enter Follow mode.

Execute this command to display as much as possible of the text
in the selected window.  All other windows, in the current
frame, are deleted and the selected window is split in two
side-by-side windows.  Follow mode is activated, hence the
two windows always will display two successive pages.
\(If one window is moved, the other one will follow.)

If ARG is positive, the leftmost window is selected.  If negative,
the rightmost is selected.  If ARG is nil, the leftmost window is
selected if the original window is the first one in the frame.

To bind this command to a hotkey, place the following line
in your `~/.emacs' file, replacing [f7] by your favorite key:
    (global-set-key [f7] 'follow-delete-other-windows-and-split)"
  (interactive "P")
  (let ((other (or (and (null arg)
			(not (eq (selected-window)
				 (frame-first-window (selected-frame)))))
		   (and arg
			(< (prefix-numeric-value arg) 0))))
	(start (window-start)))
    (delete-other-windows)
    (split-window-right)
    (if other
	(progn
	  (other-window 1)
	  (set-window-start (selected-window) start)
	  (setq follow-internal-force-redisplay t)))
    (follow-mode 1)))

(defun follow-switch-to-buffer (buffer)
  "Show BUFFER in all windows in the current Follow mode window chain."
  (interactive "BSwitch to Buffer: ")
  (let ((orig-window (selected-window))
	(windows (follow-all-followers)))
    (while windows
      (select-window (car windows))
      (switch-to-buffer buffer)
      (setq windows (cdr windows)))
    (select-window orig-window)))


(defun follow-switch-to-buffer-all (&optional buffer)
  "Show BUFFER in all windows on this frame.
Defaults to current buffer."
  (interactive (list (read-buffer "Switch to Buffer: "
				  (current-buffer))))
  (or buffer (setq buffer (current-buffer)))
  (let ((orig-window (selected-window)))
    (walk-windows
     (function
      (lambda (win)
	(select-window win)
	(switch-to-buffer buffer))))
    (select-window orig-window)
    (follow-redisplay)))


(defun follow-switch-to-current-buffer-all ()
  "Show current buffer in all windows on this frame, and enter Follow mode.

To bind this command to a hotkey place the following line
in your `~/.emacs' file:
	(global-set-key [f7] 'follow-switch-to-current-buffer-all)"
  (interactive)
  (or (and (boundp 'follow-mode) follow-mode)
      (follow-mode 1))
  (follow-switch-to-buffer-all))

;;}}}
;;{{{ Movement

;; Note, these functions are not very useful, at least not unless you
;; rebind the rather cumbersome key sequence `C-c . p'.

(defun follow-next-window ()
  "Select the next window showing the same buffer."
  (interactive)
  (let ((succ (cdr (follow-split-followers (follow-all-followers)))))
    (if succ
	(select-window (car succ))
      (error "%s" "No more windows"))))


(defun follow-previous-window ()
  "Select the previous window showing the same buffer."
  (interactive)
  (let ((pred (car (follow-split-followers (follow-all-followers)))))
    (if pred
	(select-window (car pred))
      (error "%s" "No more windows"))))


(defun follow-first-window ()
  "Select the first window in the frame showing the same buffer."
  (interactive)
  (select-window (car (follow-all-followers))))


(defun follow-last-window ()
  "Select the last window in the frame showing the same buffer."
  (interactive)
  (select-window (car (reverse (follow-all-followers)))))

;;}}}
;;{{{ Redraw

(defun follow-recenter (&optional arg)
  "Recenter the middle window around point.
Rearrange all other windows around the middle window.

With a positive argument, place the current line ARG lines
from the top.  With a negative argument, place it -ARG lines
from the bottom."
  (interactive "P")
  (if arg
      (let ((p (point))
	    (arg (prefix-numeric-value arg)))
	(if (>= arg 0)
	    ;; Recenter relative to the top.
	    (progn
	      (follow-first-window)
	      (goto-char p)
	      (recenter arg))
	  ;; Recenter relative to the bottom.
	  (follow-last-window)
	  (goto-char p)
	  (recenter arg)
	  ;; Otherwise, our post-command-hook will move the window
	  ;; right back.
	  (setq follow-internal-force-redisplay t)))
    ;; Recenter in the middle.
    (let* ((dest (point))
	   (windows (follow-all-followers))
	   (win (nth (/ (- (length windows) 1) 2) windows)))
      (select-window win)
      (goto-char dest)
      (recenter)
      ;;(setq follow-internal-force-redisplay t)
      )))


(defun follow-redraw ()
  "Arrange windows displaying the same buffer in successor order.
This function can be called even if the buffer is not in Follow mode.

Hopefully, there should be no reason to call this function when in
Follow mode since the windows should always be aligned."
  (interactive)
  (sit-for 0)
  (follow-redisplay))

;;}}}
;;{{{ End of buffer

(defun follow-end-of-buffer (&optional arg)
  "Move point to the end of the buffer, Follow mode style.

If the end is not visible, it will be displayed in the last possible
window in the Follow mode window chain.

The mark is left at the previous position.  With arg N, put point N/10
of the way from the true end."
  (interactive "P")
  (let ((followers (follow-all-followers))
	(pos (point)))
    (cond (arg
	   (select-window (car (reverse followers))))
	  ((follow-select-if-end-visible
	    (follow-windows-start-end followers)))
	  (t
	   (select-window (car (reverse followers)))))
    (goto-char pos)
    (with-no-warnings
      (end-of-buffer arg))))

;;}}}

;;}}}

;;{{{ Display

;;;; The display routines

;;{{{ Information gathering functions

(defun follow-all-followers (&optional testwin)
  "Return all windows displaying the same buffer as the TESTWIN.
The list contains only windows displayed in the same frame as TESTWIN.
If TESTWIN is nil the selected window is used."
  (or (window-live-p testwin)
      (setq testwin (selected-window)))
  (let* ((top (frame-first-window (window-frame testwin)))
	 (win top)
	 (done nil)
	 (windows '())
	 (buffer (window-buffer testwin)))
    (while (and (not done) win)
      (if (eq (window-buffer win) buffer)
	  (setq windows (cons win windows)))
      (setq win (next-window win 'not))
      (if (eq win top)
	  (setq done t)))
    (nreverse windows)))


(defun follow-split-followers (windows &optional win)
  "Split the WINDOWS into the sets: predecessors and successors.
Return `(PRED . SUCC)' where `PRED' and `SUCC' are ordered starting
from the selected window."
  (or win
      (setq win (selected-window)))
  (let ((pred '()))
    (while (not (eq (car windows) win))
      (setq pred (cons (car windows) pred))
      (setq windows (cdr windows)))
    (cons pred (cdr windows))))


;; This function is optimized function for speed!

(defun follow-calc-win-end (&optional win)
  "Calculate the presumed window end for WIN.

Actually, the position returned is the start of the next
window, normally is the end plus one.

If WIN is nil, the selected window is used.

Returns (end-pos end-of-buffer-p)"
  (if (featurep 'xemacs)
      ;; XEmacs can calculate the end of the window by using
      ;; the 'guarantee options. GOOD!
      (let ((end (window-end win t)))
	(if (= end (point-max (window-buffer win)))
	    (list end t)
	  (list (+ end 1) nil)))
    ;; Emacs: We have to calculate the end by ourselves.
    ;; This code works on both XEmacs and Emacs, but now
    ;; that XEmacs has got custom-written code, this could
    ;; be optimized for Emacs.
    (let (height buffer-end-p)
      (with-selected-window (or win (selected-window))
	(save-excursion
	  (goto-char (window-start))
	  (setq height
		(- (window-height)
		   (if header-line-format 2 1)))
	  (setq buffer-end-p
		(if (bolp)
		    (not (= height (vertical-motion height)))
		  (save-restriction
		    ;; Fix a mis-feature in `vertical-motion':
		    ;; The start of the window is assumed to
		    ;; coincide with the start of a line.
		    (narrow-to-region (point) (point-max))
		    (not (= height (vertical-motion height))))))
	  (list (point) buffer-end-p))))))


;; Can't use `save-window-excursion' since it triggers a redraw.
(defun follow-calc-win-start (windows pos win)
  "Calculate where WIN will start if the first in WINDOWS start at POS.

If WIN is nil the point below all windows is returned."
  (let (start)
    (while (and windows (not (eq (car windows) win)))
      (setq start (window-start (car windows)))
      (set-window-start (car windows) pos 'noforce)
      (setq pos (car (follow-calc-win-end (car windows))))
      (set-window-start (car windows) start 'noforce)
      (setq windows (cdr windows)))
    pos))


;; The result from `follow-windows-start-end' is cached when using
;; a handful simple commands, like cursor movement commands.

(defsubst follow-cache-valid-p (windows)
  "Test if the cached value of `follow-windows-start-end' can be used.
Note that this handles the case when the cache has been set to nil."
  (let ((res t)
	(cache follow-windows-start-end-cache))
    (while (and res windows cache)
      (setq res (and (eq (car windows)
			 (car (car cache)))
		     (eq (window-start (car windows))
			 (car (cdr (car cache))))))
      (setq windows (cdr windows))
      (setq cache (cdr cache)))
    (and res (null windows) (null cache))))


(defsubst follow-invalidate-cache ()
  "Force `follow-windows-start-end' to recalculate the end of the window."
  (setq follow-windows-start-end-cache nil))


;; Build a list of windows and their start and end positions.
;; Useful to avoid calculating start/end position whenever they are needed.
;; The list has the format:
;; ((Win Start End End-of-buffer-visible-p) ...)

;; Used to have a `save-window-excursion', but it obviously triggered
;; redraws of the display. Check if I used it for anything.


(defun follow-windows-start-end (windows)
  "Builds a list of (WIN START END BUFFER-END-P) for every window in WINDOWS."
  (if (follow-cache-valid-p windows)
      follow-windows-start-end-cache
    (let ((orig-win (selected-window))
	  win-start-end)
      (dolist (w windows)
	(select-window w)
	(push (cons w (cons (window-start) (follow-calc-win-end)))
	      win-start-end))
      (select-window orig-win)
      (setq follow-windows-start-end-cache (nreverse win-start-end)))))


(defsubst follow-pos-visible (pos win win-start-end)
  "Non-nil when POS is visible in WIN."
  (let ((wstart-wend-bend (cdr (assq win win-start-end))))
    (and (>= pos (car wstart-wend-bend))
	 (or (< pos (cadr wstart-wend-bend))
	     (nth 2 wstart-wend-bend)))))


;; By `aligned' we mean that for all adjacent windows, the end of the
;; first is equal with the start of the successor.  The first window
;; should start at a full screen line.

(defsubst follow-windows-aligned-p (win-start-end)
  "Non-nil if the follower windows are aligned."
  (let ((res t))
    (save-excursion
      (goto-char (window-start (caar win-start-end)))
      (unless (bolp)
	(vertical-motion 0 (caar win-start-end))
	(setq res (eq (point) (window-start (caar win-start-end))))))
    (while (and res (cdr win-start-end))
      ;; At least two followers left
      (setq res (eq (car (cdr (cdr (car win-start-end))))
		    (car (cdr (car (cdr win-start-end))))))
      (setq win-start-end (cdr win-start-end)))
    res))


;; Check if the point is visible in all windows. (So that
;; no one will be recentered.)

(defun follow-point-visible-all-windows-p (win-start-end)
  "Non-nil when the `window-point' is visible in all windows."
  (let ((res t))
    (while (and res win-start-end)
      (setq res (follow-pos-visible (window-point (car (car win-start-end)))
				    (car (car win-start-end))
				    win-start-end))
      (setq win-start-end (cdr win-start-end)))
    res))


;; Make sure WIN always starts at the beginning of a whole screen
;; line. If WIN is not aligned the start is updated which probably
;; will lead to a redisplay of the screen later on.
;;
;; This is used with the first window in a follow chain.  The reason
;; is that we want to detect that the point is outside the window.
;; (Without the update, the start of the window will move as the
;; user presses BackSpace, and the other window redisplay routines
;; will move the start of the window in the wrong direction.)

(defun follow-update-window-start (win)
  "Make sure that the start of WIN starts at a full screen line."
  (save-excursion
    (goto-char (window-start win))
    (unless (bolp)
      (vertical-motion 0 win)
      (unless (eq (point) (window-start win))
	(vertical-motion 1 win)
	(set-window-start win (point) 'noforce)))))

;;}}}
;;{{{ Selection functions

;; Make a window in WINDOWS selected if it currently
;; is displaying the position DEST.
;;
;; We don't select a window if it just has been moved.

(defun follow-select-if-visible (dest win-start-end)
  "Select and return a window, if DEST is visible in it.
Return the selected window."
  (let (win win-end)
    (while (and (not win) win-start-end)
      ;; Don't select a window that was just moved. This makes it
      ;; possible to later select the last window after a `end-of-buffer'
      ;; command.
      (when (follow-pos-visible dest (caar win-start-end) win-start-end)
	(setq win (caar win-start-end)
	      win-end (car (cddr (car win-start-end))))
	(select-window win))
      (setq win-start-end (cdr win-start-end)))
    ;; The last line of the window may be partially visible; if so,
    ;; and if point is visible in the next window, select the next
    ;; window instead.
    (and win
	 (/= dest (point-max))
    	 win-start-end
    	 (follow-pos-visible dest (caar win-start-end) win-start-end)
	 (save-excursion
	   (goto-char dest)
	   (vertical-motion 1 win)
	   (>= (point) win-end))
    	 (setq win (caar win-start-end))
    	 (select-window win))
    win))


;; Lets select a window showing the end. Make sure we only select it if
;; it wasn't just moved here. (I.e. M-> shall not unconditionally place
;; the point in the selected window.)
;;
;; (Compatibility kludge: in Emacs `window-end' is equal to `point-max';
;; in XEmacs, it is equal to `point-max + 1'. Should I really bother
;; checking `window-end' now when I check `end-of-buffer' explicitly?)

(defun follow-select-if-end-visible (win-start-end)
  "Select and return a window, if end is visible in it."
  (let ((win nil))
    (while (and (not win) win-start-end)
      ;; Don't select a window that was just moved. This makes it
      ;; possible to later select the last window after a `end-of-buffer'
      ;; command.
      (if (and (eq (point-max) (nth 2 (car win-start-end)))
	       (nth 3 (car win-start-end))
	       ;; `window-end' might return nil.
	       (let ((end (window-end (car (car win-start-end)))))
		 (and end
		      (eq (point-max) (min (point-max) end)))))
	  (progn
	    (setq win (car (car win-start-end)))
	    (select-window win)))
      (setq win-start-end (cdr win-start-end)))
    win))


;; Select a window that will display the point if the windows would
;; be redisplayed with the first window fixed. This is useful for
;; example when the user has pressed return at the bottom of a window
;; as the point is not visible in any window.

(defun follow-select-if-visible-from-first (dest windows)
  "Try to select one of WINDOWS without repositioning the topmost window.
If one of the windows in WINDOWS contains DEST, select it, call
`follow-redisplay', move point to DEST, and return that window.
Otherwise, return nil."
  (let (win end-pos-end-p)
    (save-excursion
      (goto-char (window-start (car windows)))
      ;; Make sure the line start in the beginning of a real screen
      ;; line.
      (vertical-motion 0 (car windows))
      (when (>= dest (point))
	;; At or below the start. Check the windows.
	(save-window-excursion
	  (let ((windows windows))
	    (while (and (not win) windows)
	      (set-window-start (car windows) (point) 'noforce)
	      (setq end-pos-end-p (follow-calc-win-end (car windows)))
	      (goto-char (car end-pos-end-p))
	      ;; Visible, if dest above end, or if eob is visible inside
	      ;; the window.
	      (if (or (car (cdr end-pos-end-p))
		      (< dest (point)))
		  (setq win (car windows))
		(setq windows (cdr windows))))))))
    (when win
      (select-window win)
      (follow-redisplay windows (car windows))
      (goto-char dest))
    win))


;;}}}
;;{{{ Redisplay

;; Redraw all the windows on the screen, starting with the top window.
;; The window used as as marker is WIN, or the selected window if WIN
;; is nil.  Start every window directly after the end of the previous
;; window, to make sure long lines are displayed correctly.

(defun follow-redisplay (&optional windows win preserve-win)
  "Reposition the WINDOWS around WIN.
Should the point be too close to the roof we redisplay everything
from the top.  WINDOWS should contain a list of windows to
redisplay; it is assumed that WIN is a member of the list.
Should WINDOWS be nil, the windows displaying the
same buffer as WIN, in the current frame, are used.
Should WIN be nil, the selected window is used.
If PRESERVE-WIN is non-nil, keep WIN itself unchanged while
repositioning the other windows."
  (or win (setq win (selected-window)))
  (or windows (setq windows (follow-all-followers win)))
  ;; Calculate the start of the first window.
  (let* ((old-win-start (window-start win))
	 (try-first-start (follow-estimate-first-window-start
			   windows win old-win-start))
	 (try-win-start (follow-calc-win-start
			 windows try-first-start win))
	 (start (cond ((= try-win-start old-win-start)
		       (follow-debug-message "exact")
		       try-first-start)
		      ((< try-win-start old-win-start)
		       (follow-debug-message "above")
		       (follow-calculate-first-window-start-from-above
			windows try-first-start win old-win-start))
		      (t
		       (follow-debug-message "below")
		       (follow-calculate-first-window-start-from-below
			windows try-first-start win old-win-start)))))
    (dolist (w windows)
      (unless (and preserve-win (eq w win))
	(set-window-start w start))
      (setq start (car (follow-calc-win-end w))))))


(defun follow-estimate-first-window-start (windows win start)
  "Estimate the position of the first window.
The estimate is computed by assuming that the window WIN, which
should be a member of WINDOWS, starts at position START."
  (let ((windows-before (car (follow-split-followers windows win))))
    (save-excursion
      (goto-char start)
      (vertical-motion 0 win)
      (dolist (w windows-before)
	(vertical-motion (- 1 (window-text-height w)) w))
      (point))))


;; Find the starting point, start at GUESS and search downward.
;; The returned point is always a point below GUESS.

(defun follow-calculate-first-window-start-from-above
       (windows guess win start)
  (save-excursion
    (let ((done nil)
	  win-start
	  res)
      (goto-char guess)
      (while (not done)
	(if (not (= (vertical-motion 1 (car windows)) 1))
	    ;; Hit bottom! (Can we really do this?)
	    ;; We'll keep it, since it ensures termination.
	    (progn
	      (setq done t)
	      (setq res (point-max)))
	  (setq win-start (follow-calc-win-start windows (point) win))
	  (if (>= win-start start)
	      (setq done t res (point)))))
      res)))


;; Find the starting point, start at GUESS and search upward.  Return
;; a point on the same line as GUESS, or above.
;;
;; (Is this ever used? I must make sure it works just in case it is
;; ever called.)

(defun follow-calculate-first-window-start-from-below
       (windows guess &optional win start)
  (setq win (or win (selected-window)))
  (setq start (or start (window-start win)))
  (save-excursion
    (let (done win-start res opoint)
      ;; Always calculate what happens when no line is displayed in the first
      ;; window. (The `previous' res is needed below!)
      (goto-char guess)
      (vertical-motion 0 (car windows))
      (setq res (point))
      (while (not done)
	(setq opoint (point))
	(if (not (= (vertical-motion -1 (car windows)) -1))
	    ;; Hit roof!
	    (setq done t res (point-min))
	  (setq win-start (follow-calc-win-start windows (point) win))
	  (cond ((>= (point) opoint)
		 ;; In some pathological cases, vertical-motion may
		 ;; return -1 even though point has not decreased.  In
		 ;; that case, avoid looping forever.
		 (setq done t res (point)))
		((= win-start start)	; Perfect match, use this value
		 (setq done t res (point)))
		((< win-start start)	; Walked to far, use previous result
		 (setq done t))
		(t			; Store result for next iteration
		 (setq res (point))))))
      res)))

;;}}}
;;{{{ Avoid tail recenter

;; This sets the window internal flag `force_start'. The effect is that
;; windows only displaying the tail aren't recentered.
;; Has to be called before every redisplay... (Great isn't it?)
;;
;; XEmacs doesn't recenter the tail, GOOD!
;;
;; A window displaying only the tail, is a window whose
;; window-start position is equal to (point-max) of the buffer it
;; displays.
;;
;; This function is also added to `post-command-idle-hook', introduced
;; in Emacs 19.30.  This is needed since the vaccine injected by the
;; call from `post-command-hook' only works until the next redisplay.
;; It is possible that the functions in the `post-command-idle-hook'
;; can cause a redisplay, and hence a new vaccine is needed.
;;
;; Sometimes, calling this function could actually cause a redisplay,
;; especially if it is placed in the debug filter section.  I must
;; investigate this further...

(defun follow-avoid-tail-recenter (&rest _rest)
  "Make sure windows displaying the end of a buffer aren't recentered.

This is done by reading and rewriting the start position of
non-first windows in Follow mode."
  (if follow-avoid-tail-recenter-p
      (let* ((orig-buffer (current-buffer))
	    (top (frame-first-window (selected-frame)))
	    (win top)
	    (who '())			; list of (buffer . frame)
	    start
	    pair)			; (buffer . frame)
	;; If the only window in the frame is a minibuffer
	;; window, `next-window' will never find it again...
	(if (window-minibuffer-p top)
	    nil
	  (while  ;; look, no body!
	      (progn
		(setq start (window-start win))
		(set-buffer (window-buffer win))
		(setq pair (cons (window-buffer win) (window-frame win)))
		(if (member pair who)
		    (if (and (boundp 'follow-mode) follow-mode
			     (eq (point-max) start))
			;; Write the same window start back, but don't
			;; set the NOFORCE flag.
			(set-window-start win start))
		  (setq who (cons pair who)))
		(setq win (next-window win 'not t))
		(not (eq win top))))  ;; Loop while this is true.
	  (set-buffer orig-buffer)))))

;;}}}

;;}}}
;;{{{ Post Command Hook

;; The magic little box. This function is called after every command.

;; This is not as complicated as it seems. It is simply a list of common
;; display situations and the actions to take, plus commands for redrawing
;; the screen if it should be unaligned.
;;
;; We divide the check into two parts; whether we are at the end or not.
;; This is due to the fact that the end can actually be visible
;; in several window even though they are aligned.

(defun follow-post-command-hook ()
  "Ensure that the windows in Follow mode are adjacent after each command."
  (unless (input-pending-p)
    (let ((follow-inside-post-command-hook t)
	  (win (selected-window)))
      ;; Work in the selected window, not in the current buffer.
      (with-current-buffer (window-buffer win)
	(unless (and (symbolp this-command)
		     (get this-command 'follow-mode-use-cache))
	  (follow-invalidate-cache))
	(when (and follow-mode
		   (not (window-minibuffer-p win)))
	  ;; The buffer shown in the selected window is in follow
	  ;; mode.  Find the current state of the display.
	  (let* ((windows (follow-all-followers win))
		 (dest (point))
		 (win-start-end (progn
				  (follow-update-window-start (car windows))
				  (follow-windows-start-end windows)))
		 (aligned (follow-windows-aligned-p win-start-end))
		 (visible (follow-pos-visible dest win win-start-end))
		 selected-window-up-to-date)
	    (unless (and aligned visible)
	      (follow-invalidate-cache))
	    (follow-avoid-tail-recenter)
	    ;; Select a window to display point.
	    (unless follow-internal-force-redisplay
	      (if (eq dest (point-max))
		  ;; At point-max, we have to be careful since the
		  ;; display can be aligned while `dest' can be
		  ;; visible in several windows.
		  (cond
		   ;; Select the current window, but only when the
		   ;; display is correct. (When inserting characters
		   ;; in a tail window, the display is not correct, as
		   ;; they are shown twice.)
		   ;;
		   ;; Never stick to the current window after a
		   ;; deletion.  The reason is cosmetic: when typing
		   ;; `DEL' in a window showing only the end of the
		   ;; file, a character would be removed from the
		   ;; window above, which is very unintuitive.
		   ((and visible
			 aligned
			 (not (memq this-command
				    '(backward-delete-char
				      delete-backward-char
				      backward-delete-char-untabify
				      kill-region))))
		    (follow-debug-message "Max: same"))
		   ;; If the end is visible, and the window doesn't
		   ;; seems like it just has been moved, select it.
		   ((follow-select-if-end-visible win-start-end)
		    (follow-debug-message "Max: end visible")
		    (setq visible t aligned nil)
		    (goto-char dest))
		   ;; Just show the end...
		   (t
		    (follow-debug-message "Max: default")
		    (select-window (car (reverse windows)))
		    (goto-char dest)
		    (setq visible nil aligned nil)))

		;; We're not at the end, here life is much simpler.
		(cond
		 ;; This is the normal case!
		 ;; It should be optimized for speed.
		 ((and visible aligned)
		  (follow-debug-message "same"))
		 ;; Pick a position in any window.  If the display is
		 ;; ok, this will pick the `correct' window.
		 ((follow-select-if-visible dest win-start-end)
		  (follow-debug-message "visible")
		  (goto-char dest)
		  ;; We have to perform redisplay, since scrolling is
		  ;; needed in case the line is partially visible.
		  (setq visible nil))
		 ;; Not visible anywhere else, lets pick this one.
		 ;; (Is this case used?)
		 (visible
		  (follow-debug-message "visible in selected."))
		 ;; Far out!
		 ((eq dest (point-min))
		  (follow-debug-message "min")
		  (select-window (car windows))
		  (goto-char dest)
		  (set-window-start (selected-window) (point-min))
		  (setq win-start-end (follow-windows-start-end windows))
		  (follow-invalidate-cache)
		  (setq visible t aligned nil))
		 ;; If we can position the cursor without moving the first
		 ;; window, do it. This is the case that catches `RET'
		 ;; at the bottom of a window.
		 ((follow-select-if-visible-from-first dest windows)
		  (follow-debug-message "Below first")
		  (setq visible t aligned t))
		 ;; None of the above. For simplicity, we stick to the
		 ;; selected window.
		 (t
		  (follow-debug-message "None")
		  (setq visible nil aligned nil))))
	      ;; If a new window has been selected, make sure that the
	      ;; old is not scrolled when the point is outside the
	      ;; window.
	      (unless (eq win (selected-window))
		(let ((p (window-point win)))
		  (set-window-start win (window-start win) nil)
		  (set-window-point win p))))
	    (unless visible
	      ;; If point may not be visible in the selected window,
	      ;; perform a redisplay; this ensures scrolling.
	      (redisplay)
	      (setq selected-window-up-to-date t)
	      (follow-avoid-tail-recenter)
	      (setq win-start-end (follow-windows-start-end windows))
	      (follow-invalidate-cache)
	      (setq aligned nil))
	    ;; Now redraw the windows around the selected window.
	    (unless (and (not follow-internal-force-redisplay)
			 (or aligned
			     (follow-windows-aligned-p win-start-end))
			 (follow-point-visible-all-windows-p
			  win-start-end))
	      (setq follow-internal-force-redisplay nil)
	      (follow-redisplay windows (selected-window)
				selected-window-up-to-date)
	      (setq win-start-end (follow-windows-start-end windows))
	      (follow-invalidate-cache)
	      ;; When the point ends up in another window. This
	      ;; happens when dest is in the beginning of the file and
	      ;; the selected window is not the first.  It can also,
	      ;; in rare situations happen when long lines are used
	      ;; and there is a big difference between the width of
	      ;; the windows.  (When scrolling one line in a wide
	      ;; window which will cause a move larger that an entire
	      ;; small window.)
	      (unless (follow-pos-visible dest win win-start-end)
		(follow-select-if-visible dest win-start-end)
		(goto-char dest)))

	    ;; If the region is visible, make it look good when spanning
	    ;; multiple windows.
	    (when (region-active-p)
	      (follow-maximize-region
	       (selected-window) windows win-start-end))))
	;; Whether or not the buffer was in follow mode, we must
	;; update the windows displaying the tail so that Emacs won't
	;; recenter them.
	(follow-avoid-tail-recenter)))))

;;}}}
;;{{{ The region

;; Tries to make the highlighted area representing the region look
;; good when spanning several windows.
;;
;; Not perfect, as the point can't be placed at window end, only at
;; end-1.  This will highlight a little bit in windows above
;; the current.

(defun follow-maximize-region (win windows win-start-end)
  "Make a highlighted region stretching multiple windows look good."
  (let* ((all (follow-split-followers windows win))
	 (pred (car all))
	 (succ (cdr all))
	 data)
    (while pred
      (setq data (assq (car pred) win-start-end))
      (set-window-point (car pred) (max (nth 1 data) (- (nth 2 data) 1)))
      (setq pred (cdr pred)))
    (while succ
      (set-window-point (car succ) (nth 1 (assq (car succ) win-start-end)))
      (setq succ (cdr succ)))))

;;}}}
;;{{{ Scroll bar

;;;; Scroll-bar support code.

;; Why is it needed? Well, if the selected window is in follow mode,
;; all its followers stick to it blindly. If one of them is scrolled,
;; it immediately returns to the original position when the mouse is
;; released. If the selected window is not a follower of the dragged
;; window the windows will be unaligned.

;; The advices don't get compiled. Aesthetically, this might be a
;; problem but in practical life it isn't.

;; Discussion: Now when the other windows in the chain follow the
;; dragged, should we really select it?

(cond ((fboundp 'scroll-bar-drag)
       ;;;
       ;;; Emacs style scrollbars.
       ;;;

       ;; Select the dragged window if it is a follower of the
       ;; selected window.
       ;;
       ;; Generate advices of the form:
       ;; (defadvice scroll-bar-drag (after follow-scroll-bar-drag activate)
       ;;   "Adviced by `follow-mode'."
       ;;   (follow-redraw-after-event (ad-get-arg 0)))
       (let ((cmds '(scroll-bar-drag
		     scroll-bar-drag-1	; Executed at every move.
		     scroll-bar-scroll-down
		     scroll-bar-scroll-up
		     scroll-bar-set-window-start)))
	 (while cmds
	   (eval
	    `(defadvice ,(intern (symbol-name (car cmds)))
		 (after
		  ,(intern (concat "follow-" (symbol-name (car cmds))))
		  activate)
		 "Adviced by Follow mode."
		 (follow-redraw-after-event (ad-get-arg 0))))
	   (setq cmds (cdr cmds))))


       (defun follow-redraw-after-event (event)
	 "Adviced by Follow mode."
	 (condition-case nil
	     (let* ((orig-win (selected-window))
		    (win (nth 0 (funcall
				 (symbol-function 'event-start) event)))
		    (fmode (assq 'follow-mode
				 (buffer-local-variables
				  (window-buffer win)))))
	       (if (and fmode (cdr fmode))
		   ;; The selected window is in follow-mode
		   (progn
		     ;; Recenter around the dragged window.
		     (select-window win)
		     (follow-redisplay)
		     (select-window orig-win))))
	   (error nil))))


      ((fboundp 'scrollbar-vertical-drag)
       ;;;
       ;;; XEmacs style scrollbars.
       ;;;

       ;; Advice all scrollbar functions on the form:
       ;;
       ;; (defadvice scrollbar-line-down
       ;;	(after follow-scrollbar-line-down activate)
       ;;   (follow-xemacs-scrollbar-support (ad-get-arg 0)))

      (let ((cmds '(scrollbar-line-down	; Window
		    scrollbar-line-up
		    scrollbar-page-down	; Object
		    scrollbar-page-up
		    scrollbar-to-bottom	; Window
		    scrollbar-to-top
		    scrollbar-vertical-drag ; Object
		    )))

	(while cmds
	  (eval
	   `(defadvice ,(intern (symbol-name (car cmds)))
		(after
		 ,(intern (concat "follow-" (symbol-name (car cmds))))
		 activate)
		"Adviced by `follow-mode'."
		(follow-xemacs-scrollbar-support (ad-get-arg 0))))
	  (setq cmds (cdr cmds))))


      (defun follow-xemacs-scrollbar-support (window)
	"Redraw windows showing the same buffer as shown in WINDOW.
WINDOW is either the dragged window, or a cons containing the
window as its first element.  This is called while the user drags
the scrollbar.

WINDOW can be an object or a window."
	(condition-case nil
	    (progn
	      (if (consp window)
		  (setq window (car window)))
	      (let ((fmode (assq 'follow-mode
				 (buffer-local-variables
				  (window-buffer window))))
		    (orig-win (selected-window)))
		(if (and fmode (cdr fmode))
		    (progn
		      ;; Recenter around the dragged window.
		      (select-window window)
		      (follow-redisplay)
		      (select-window orig-win)))))
	  (error nil)))))

;;}}}
;;{{{ Process output

;; The following sections installs a spy that listens to process
;; output and tries to reposition the windows whose buffers are in
;; Follow mode.  We play safe as much as possible...
;;
;; When follow-mode is activated all active processes are
;; intercepted.  All new processes that change their filter function
;; using `set-process-filter' are also intercepted.  The reason is
;; that a process can cause a redisplay recentering "tail" windows.
;; Note that it doesn't hurt to spy on more processes than needed.
;;
;; Technically, we set the process filter to `follow-generic-filter'.
;; The original filter is stored in `follow-process-filter-alist'.
;; Our generic filter calls the original filter, or inserts the
;; output into the buffer, if the buffer originally didn't have an
;; output filter.  It also makes sure that the windows connected to
;; the buffer are aligned.
;;
;; Discussion: How do we find processes that don't call
;; `set-process-filter'?  (How often are processes created in a
;; buffer after Follow mode are activated?)
;;
;; Discussion: Should we also advice `process-filter' to make our
;; filter invisible to others?

;;{{{ Advice for `set-process-filter'

;; Do not call this with 'follow-generic-filter as the name of the
;; filter...

(defadvice set-process-filter (before follow-set-process-filter activate)
  "Ensure process output will be displayed correctly in Follow mode buffers.

Follow mode inserts its own process filter to do its
magic stuff before the real process filter is called."
  (if follow-intercept-processes
      (progn
	(setq follow-process-filter-alist
	      (delq (assq (ad-get-arg 0) follow-process-filter-alist)
		    follow-process-filter-alist))
	(follow-tidy-process-filter-alist)
	(cond ((eq (ad-get-arg 1) t))
	      ((eq (ad-get-arg 1) nil)
	       (ad-set-arg 1 'follow-generic-filter))
	      (t
	       (setq follow-process-filter-alist
		     (cons (cons (ad-get-arg 0) (ad-get-arg 1))
			   follow-process-filter-alist))
	       (ad-set-arg 1 'follow-generic-filter))))))


(defun follow-call-set-process-filter (proc filter)
  "Call original `set-process-filter' without the Follow mode advice."
  (ad-disable-advice 'set-process-filter 'before
		     'follow-set-process-filter)
  (ad-activate 'set-process-filter)
  (prog1
      (set-process-filter proc filter)
    (ad-enable-advice 'set-process-filter 'before
		      'follow-set-process-filter)
    (ad-activate 'set-process-filter)))


(defadvice process-filter (after follow-process-filter activate)
  "Return the original process filter, not `follow-generic-filter'."
  (cond ((eq ad-return-value 'follow-generic-filter)
	 (setq ad-return-value
	       (cdr-safe (assq (ad-get-arg 0)
			       follow-process-filter-alist))))))


(defun follow-call-process-filter (proc)
  "Call original `process-filter' without the Follow mode advice."
  (ad-disable-advice 'process-filter 'after
		     'follow-process-filter)
  (ad-activate 'process-filter)
  (prog1
      (process-filter proc)
    (ad-enable-advice 'process-filter 'after
		      'follow-process-filter)
    (ad-activate 'process-filter)))


(defun follow-tidy-process-filter-alist ()
  "Remove old processes from `follow-process-filter-alist'."
  (let ((alist follow-process-filter-alist)
	(ps (process-list))
	(new ()))
    (while alist
      (if (and (not (memq (process-status (car (car alist)))
			  '(exit signal closed nil)))
	       (memq (car (car alist)) ps))
	  (setq new (cons (car alist) new)))
      (setq alist (cdr alist)))
    (setq follow-process-filter-alist new)))

;;}}}
;;{{{ Start/stop interception of processes.

;; Normally, all new processes are intercepted by our `set-process-filter'.
;; This is needed to intercept old processes that were started before we were
;; loaded, and processes we have forgotten by calling
;; `follow-stop-intercept-process-output'.

(defun follow-intercept-process-output ()
  "Intercept all active processes.

This is needed so that Follow mode can track all display events in the
system.  (See `follow-mode'.)"
  (interactive)
  (let ((list (process-list)))
    (while list
      (if (eq (process-filter (car list)) 'follow-generic-filter)
	  nil
	;; The custom `set-process-filter' defined above.
	(set-process-filter (car list) (process-filter (car list))))
      (setq list (cdr list))))
  (setq follow-intercept-processes t))


(defun follow-stop-intercept-process-output ()
  "Stop Follow mode from spying on processes.

All current spypoints are removed and no new will be added.

The effect is that Follow mode won't be able to handle buffers
connected to processes.

The only reason to call this function is if the Follow mode spy filter
would interfere with some other package.  If this happens, please
report this using the `report-emacs-bug' function."
  (interactive)
  (follow-tidy-process-filter-alist)
  (dolist (process (process-list))
    (when (eq (follow-call-process-filter process) 'follow-generic-filter)
      (follow-call-set-process-filter
       process
       (cdr-safe (assq process follow-process-filter-alist)))
      (setq follow-process-filter-alist
	    (delq (assq process follow-process-filter-alist)
		  follow-process-filter-alist))))
  (setq follow-intercept-processes nil))

;;}}}
;;{{{ The filter

;; The following section is a naive method to make buffers with
;; process output to work with Follow mode. Whenever the start of the
;; window displaying the buffer is moved, we move it back to its
;; original position and try to select a new window.  (If we fail,
;; the normal redisplay functions of Emacs will scroll it right
;; back!)

(defun follow-generic-filter (proc output)
  "Process output filter for process connected to buffers in Follow mode."
  (let* ((old-buffer (current-buffer))
	 (orig-win (selected-window))
	 (buf (process-buffer proc))
	 (win (and buf (if (eq buf (window-buffer orig-win))
			   orig-win
			 (get-buffer-window buf t))))
	 (return-to-orig-win (and win (not (eq win orig-win))))
	 (orig-window-start (and win (window-start win))))

    ;; If input is pending, the `sit-for' below won't redraw the
    ;; display. In that case, calling `follow-avoid-tail-recenter' may
    ;; provoke the process handling code to schedule a redisplay.
    ;(or (input-pending-p)
    ; (follow-avoid-tail-recenter))

    ;; Output the `output'.
    (let ((filter (cdr-safe (assq proc follow-process-filter-alist))))
      (cond
       ;; Call the original filter function
       (filter
	(funcall filter proc output))

       ;; No filter, but we've got a buffer. Just output into it.
       (buf
	(set-buffer buf)
	(if (not (marker-buffer (process-mark proc)))
	    (set-marker (process-mark proc) (point-max)))
	(let ((moving (= (point) (process-mark proc)))
	      deactivate-mark
	      (inhibit-read-only t))
	  (save-excursion
	    (goto-char (process-mark proc))
	    ;; `insert-before-markers' just in case the user's next
	    ;; command is M-y.
	    (insert-before-markers output)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc)))))))

    ;; If we're in follow mode, do our stuff.  Select a new window and
    ;; redisplay.  (Actually, it is redundant to check `buf', but I
    ;; feel it's more correct.)
    (if (and buf (window-live-p win))
	(progn
	  (set-buffer buf)
	  (if (and (boundp 'follow-mode) follow-mode)
	      (progn
		(select-window win)
		(let* ((windows (follow-all-followers win))
		       (win-start-end (follow-windows-start-end windows))
		       (new-window-start (window-start win))
		       (new-window-point (window-point win)))
		  (cond
		   ;; The start of the selected window was repositioned.
		   ;; Try to use the original start position and continue
		   ;; working with a window to the "right" in the window
		   ;; chain.  This will create the effect that the output
		   ;; starts in one window and continues into the next.

		   ;; If the display has changed so much that it is not
		   ;; possible to keep the original window fixed and still
		   ;; display the point then we give up and use the new
		   ;; window start.

		   ;; This case is typically used when the process filter
		   ;; tries to reposition the start of the window in order
		   ;; to view the tail of the output.
		   ((not (eq orig-window-start new-window-start))
		    (follow-debug-message "filter: Moved")
		    (set-window-start win orig-window-start)
		    (follow-redisplay windows win)
		    (setq win-start-end (follow-windows-start-end windows))
		    (follow-select-if-visible new-window-point
					      win-start-end)
		    (goto-char new-window-point)
		    (if (eq win (selected-window))
			(set-window-start win new-window-start))
		    (setq win-start-end (follow-windows-start-end windows)))
		   ;; Stick to this window, if point is visible in it.
		   ((pos-visible-in-window-p new-window-point)
		    (follow-debug-message "filter: Visible in window"))
		   ;; Avoid redisplaying the first window. If the
		   ;; point is visible at a window below,
		   ;; redisplay and select it.
		   ((follow-select-if-visible-from-first
		     new-window-point windows)
		    (follow-debug-message "filter: Seen from first")
		    (setq win-start-end
			  (follow-windows-start-end windows)))
		   ;; None of the above. We stick to the current window.
		   (t
		    (follow-debug-message "filter: nothing")))

		  ;; Here we have selected a window. Make sure the
		  ;; windows are aligned and the point is visible
		  ;; in the selected window.
		  (if (and (not (follow-pos-visible
				 (point) (selected-window) win-start-end))
			   (not return-to-orig-win))
		      (progn
			(sit-for 0)
			(setq win-start-end
			      (follow-windows-start-end windows))))

		  (if (or follow-internal-force-redisplay
			  (not (follow-windows-aligned-p win-start-end)))
		      (follow-redisplay windows)))))))

    ;; return to the original window.
    (if return-to-orig-win
	(select-window orig-win))
    ;; Restore the original buffer, unless the filter explicitly
    ;; changed buffer or killed the old buffer.
    (if (and (eq buf (current-buffer))
	     (buffer-name old-buffer))
	(set-buffer old-buffer)))

  (follow-invalidate-cache)

  ;; Normally, if the display has been changed, it is redrawn.  All
  ;; windows showing only the end of a buffer are unconditionally
  ;; recentered; we can't prevent that by calling
  ;; `follow-avoid-tail-recenter'.
  ;;
  ;; We force a redisplay here on our own, so Emacs does need to.
  ;; (However, redisplaying when there's input available just seems
  ;; to make things worse, so we exclude that case.)
  (if (and follow-avoid-tail-recenter-p
	   (not (input-pending-p)))
      (sit-for 0)))

;;}}}

;;}}}
;;{{{ Window size change

;; In Emacs 19.29, the functions in `window-size-change-functions' are
;; called every time a window in a frame changes size. Most notably, it
;; is called after the frame has been resized.
;;
;; We basically call our post-command-hook for every buffer that is
;; visible in any window in the resized frame, which is in follow-mode.
;;
;; Since this function can be called indirectly from
;; `follow-post-command-hook' we have a potential infinite loop.  We
;; handle this problem by simply not doing anything at all in this
;; situation.  The variable `follow-inside-post-command-hook' contains
;; information about whether the execution actually is inside the
;; post-command-hook or not.

(if (boundp 'window-size-change-functions)
    (add-hook 'window-size-change-functions 'follow-window-size-change))


(defun follow-window-size-change (frame)
  "Redraw all windows in FRAME, when in Follow mode."
  ;; Below, we call `post-command-hook'.  This makes sure that we
  ;; don't start a mutually recursive endless loop.
  (if follow-inside-post-command-hook
      nil
    (let ((buffers '())
	  (orig-window (selected-window))
	  (orig-buffer (current-buffer))
	  (orig-frame (selected-frame))
	  windows
	  buf)
      (select-frame frame)
      (unwind-protect
	  (walk-windows
	   (function
	    (lambda (win)
	      (setq buf (window-buffer win))
	      (if (memq buf buffers)
		  nil
		(set-buffer buf)
		(if (and (boundp 'follow-mode)
			 follow-mode)
		    (progn
		      (setq windows (follow-all-followers win))
		      (if (memq orig-window windows)
			  (progn
                            ;; Make sure we're redrawing around the
                            ;; selected window.
                            ;;
                            ;; We must be really careful not to do this
                            ;; when we are (indirectly) called by
                            ;; `post-command-hook'.
			    (select-window orig-window)
			    (follow-post-command-hook)
			    (setq orig-window (selected-window)))
			(follow-redisplay windows win))
		      (setq buffers (cons buf buffers))))))))
	(select-frame orig-frame)
	(set-buffer orig-buffer)
	(select-window orig-window)))))

;;}}}

;;{{{ XEmacs isearch

;; In XEmacs, isearch often finds matches in other windows than the
;; currently selected.  However, when exiting the old window
;; configuration is restored, with the exception of the beginning of
;; the start of the window for the selected window.  This is not much
;; help for us.
;;
;; We overwrite the stored window configuration with the current,
;; unless we are in `slow-search-mode', i.e. only a few lines
;; of text is visible.

(if (featurep 'xemacs)
    (defadvice isearch-done (before follow-isearch-done activate)
      (if (and (boundp 'follow-mode)
	       follow-mode
	       (boundp 'isearch-window-configuration)
	       isearch-window-configuration
	       (boundp 'isearch-slow-terminal-mode)
	       (not isearch-slow-terminal-mode))
	  (let ((buf (current-buffer)))
	    (setq isearch-window-configuration
		  (current-window-configuration))
	    (set-buffer buf)))))

;;}}}
;;{{{ Tail window handling

;; In Emacs (not XEmacs) windows showing nothing are sometimes
;; recentered.  When in Follow mode, this is not desirable for
;; non-first windows in the window chain.  This section tries to
;; make the windows stay where they should be.
;;
;; If the display is updated, all windows starting at (point-max) are
;; going to be recentered at the next redisplay, unless we do a
;; read-and-write cycle to update the `force' flag inside the windows.
;;
;; In 19.30, a new variable `window-scroll-functions' is called every
;; time a window is recentered.  It is not perfect for our situation,
;; since when it is called for a tail window, it is to late.  However,
;; if it is called for another window, we can try to update our
;; windows.
;;
;; By patching `sit-for' we can make sure that to catch all explicit
;; updates initiated by lisp programs.  Internal calls, on the other
;; hand, are not handled.
;;
;; Please note that the function `follow-avoid-tail-recenter' is also
;; called from other places, e.g. `post-command-hook' and
;; `post-command-idle-hook'.

;; If this function is called it is too late for this window, but
;; we might save other windows from being recentered.

(if (and follow-avoid-tail-recenter-p (boundp 'window-scroll-functions))
    (add-hook 'window-scroll-functions 'follow-avoid-tail-recenter t))


;;  This prevents all packages that calls `sit-for' directly
;;  to recenter tail windows.

(if follow-avoid-tail-recenter-p
    (defadvice sit-for (before follow-sit-for activate)
      "Adviced by Follow mode.

Avoid to recenter windows displaying only the end of a file as when
displaying a short file in two windows, using Follow mode."
      (follow-avoid-tail-recenter)))


;;  Without this advice, `mouse-drag-region' would start to recenter
;;  tail windows.

(if (and follow-avoid-tail-recenter-p
	 (fboundp 'move-overlay))
    (defadvice move-overlay (before follow-move-overlay activate)
      "Adviced by Follow mode.
Don't recenter windows showing only the end of a buffer.
This prevents `mouse-drag-region' from messing things up."
      (follow-avoid-tail-recenter)))

;;}}}
;;{{{ profile support

;; The following (non-evaluated) section can be used to
;; profile this package using `elp'.
;;
;; Invalid indentation on purpose!

(cond (nil
(setq elp-function-list
      '(window-end
	vertical-motion
	; sit-for  ;; elp can't handle advices...
	follow-mode
	follow-all-followers
	follow-split-followers
	follow-redisplay
	follow-estimate-first-window-start
	follow-calculate-first-window-start-from-above
	follow-calculate-first-window-start-from-below
	follow-calc-win-end
	follow-calc-win-start
	follow-pos-visible
	follow-windows-start-end
	follow-cache-valid-p
	follow-select-if-visible
	follow-select-if-visible-from-first
	follow-windows-aligned-p
	follow-point-visible-all-windows-p
	follow-avoid-tail-recenter
	follow-update-window-start
	follow-post-command-hook
	))))

;;}}}

;;{{{ The end

(defun follow-unload-function ()
  "Unload Follow mode library."
  (easy-menu-remove-item nil '("Tools") "Follow")
  (follow-stop-intercept-process-output)
  (dolist (group '((before
		    ;; XEmacs
		    isearch-done
		    ;; both
		    set-process-filter sit-for move-overlay)
		   (after
		    ;; Emacs
		    scroll-bar-drag scroll-bar-drag-1 scroll-bar-scroll-down
		    scroll-bar-scroll-up scroll-bar-set-window-start
		    ;; XEmacs
		    scrollbar-line-down scrollbar-line-up scrollbar-page-down
		    scrollbar-page-up scrollbar-to-bottom scrollbar-to-top
		    scrollbar-vertical-drag
		    ;; both
		    process-filter)))
    (let ((class (car group)))
      (dolist (fun (cdr group))
	(when (functionp fun)
	  (condition-case nil
	      (progn
		(ad-remove-advice fun class
				  (intern (concat "follow-" (symbol-name fun))))
		(ad-update fun))
	    (error nil))))))
  ;; continue standard processing
  nil)

;;
;; We're done!
;;

(provide 'follow)

;;}}}

;; /------------------------------------------------------------------------\
;; | "I [..] am rarely happier then when spending an entire day programming |
;; | my computer to perform automatically a task that it would otherwise    |
;; | take me a good ten seconds to do by hand. Ten seconds, I tell myself,  |
;; | is ten seconds. Time is valuable and ten seconds' worth of it is well  |
;; | worth the investment of a day's happy activity working out a way to    |
;; | save it".             -- Douglas Adams, "Last Chance to See"           |
;; \------------------------------------------------------------------------/

;;; follow.el ends here
