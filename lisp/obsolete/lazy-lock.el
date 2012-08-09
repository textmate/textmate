;;; lazy-lock.el --- lazy demand-driven fontification for fast Font Lock mode

;; Copyright (C) 1994-1998, 2001-2012 Free Software Foundation, Inc.

;; Author: Simon Marshall <simon@gnu.org>
;; Maintainer: FSF
;; Keywords: faces files
;; Version: 2.11
;; Obsolete-since: 22.1

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

;; Purpose:
;;
;; Lazy Lock mode is a Font Lock support mode.
;; It makes visiting buffers in Font Lock mode faster by making fontification
;; be demand-driven, deferred and stealthy, so that fontification only occurs
;; when, and where, necessary.
;;
;; See caveats and feedback below.
;; See also the fast-lock package.  (But don't use them at the same time!)

;; Installation:
;;
;; Put in your ~/.emacs:
;;
;; (setq font-lock-support-mode 'lazy-lock-mode)
;;
;; Start up a new Emacs and use font-lock as usual (except that you can use the
;; so-called "gaudier" fontification regexps on big files without frustration).
;;
;; In a buffer (which has `font-lock-mode' enabled) which is at least
;; `lazy-lock-minimum-size' characters long, buffer fontification will not
;; occur and only the visible portion of the buffer will be fontified.  Motion
;; around the buffer will fontify those visible portions not previously
;; fontified.  If stealth fontification is enabled, buffer fontification will
;; occur in invisible parts of the buffer after `lazy-lock-stealth-time'
;; seconds of idle time.  If on-the-fly fontification is deferred, on-the-fly
;; fontification will occur after `lazy-lock-defer-time' seconds of idle time.

;; User-visible differences with version 1:
;;
;; - Version 2 can defer on-the-fly fontification.  Therefore you need not, and
;; should not, use defer-lock.el with this version of lazy-lock.el.
;;
;; A number of variables have changed meaning:
;;
;; - A value of nil for the variable `lazy-lock-minimum-size' means never turn
;; on demand-driven fontification.  In version 1 this meant always turn on
;; demand-driven fontification.  If you really want demand-driven fontification
;; regardless of buffer size, set this variable to 0.
;;
;; - The variable `lazy-lock-stealth-lines' cannot have a nil value.  In
;; version 1 this meant use `window-height' as the maximum number of lines to
;; fontify as a stealth chunk.  This makes no sense; stealth fontification is
;; of a buffer, not a window.

;; Implementation differences with version 1:
;;
;; - Version 1 of lazy-lock.el is a bit of a hack.  Version 1 demand-driven
;; fontification, the core feature of lazy-lock.el, is implemented by placing a
;; function on `post-command-hook'.  This function fontifies where necessary,
;; i.e., where a window scroll has occurred.  However, there are a number of
;; problems with using `post-command-hook':
;;
;; (a) As the name suggests, `post-command-hook' is run after every command,
;;     i.e., frequently and regardless of whether scrolling has occurred.
;; (b) Scrolling can occur during a command, when `post-command-hook' is not
;;     run, i.e., it is not necessarily run after scrolling has occurred.
;; (c) When `post-command-hook' is run, there is nothing to suggest where
;;     scrolling might have occurred, i.e., which windows have scrolled.
;;
;; Thus lazy-lock.el's function is called almost as often as possible, usually
;; when it need not be called, yet it is not always called when it is needed.
;; Also, lazy-lock.el's function must check each window to see if a scroll has
;; occurred there.  Worse still, lazy-lock.el's function must fontify a region
;; twice as large as necessary to make sure the window is completely fontified.
;; Basically, `post-command-hook' is completely inappropriate for lazy-lock.el.
;;
;; Ideally, we want to attach lazy-lock.el's function to a hook that is run
;; only when scrolling occurs, e.g., `window-start' has changed, and tells us
;; as much information as we need, i.e., the window and its new buffer region.
;; Richard Stallman implemented a `window-scroll-functions' for Emacs 19.30.
;; Functions on it are run when `window-start' has changed, and are supplied
;; with the window and the window's new `window-start' position.  (It would be
;; better if it also supplied the window's new `window-end' position, but that
;; is calculated as part of the redisplay process, and the functions on
;; `window-scroll-functions' are run before redisplay has finished.)  Thus, the
;; hook deals with the above problems (a), (b) and (c).
;;
;; If only life was that easy.  Version 2 demand-driven fontification is mostly
;; implemented by placing a function on `window-scroll-functions'.  However,
;; not all scrolling occurs when `window-start' has changed.  A change in
;; window size, e.g., via C-x 1, or a significant deletion, e.g., of a number
;; of lines, causes text previously invisible (i.e., after `window-end') to
;; become visible without changing `window-start'.  Arguably, these events are
;; not scrolling events, but fontification must occur for lazy-lock.el to work.
;; Hooks `window-size-change-functions' and `redisplay-end-trigger-functions'
;; were added for these circumstances.
;;
;; (Ben Wing thinks these hooks are "horribly horribly kludgy", and implemented
;; a `pre-idle-hook', a `mother-of-all-post-command-hooks', for XEmacs 19.14.
;; He then hacked up a version 1 lazy-lock.el to use `pre-idle-hook' rather
;; than `post-command-hook'.  Whereas functions on `post-command-hook' are
;; called almost as often as possible, functions on `pre-idle-hook' really are
;; called as often as possible, even when the mouse moves and, on some systems,
;; while XEmacs is idle.  Thus, the hook deals with the above problem (b), but
;; unfortunately it makes (a) worse and does not address (c) at all.
;;
;; I freely admit that `redisplay-end-trigger-functions' and, to a much lesser
;; extent, `window-size-change-functions' are not pretty.  However, I feel that
;; a `window-scroll-functions' feature is cleaner than a `pre-idle-hook', and
;; the result is faster and smaller, less intrusive and more targeted, code.
;; Since `pre-idle-hook' is pretty much like `post-command-hook', there is no
;; point in making this version of lazy-lock.el work with it.  Anyway, that's
;; Lit 30 of my humble opinion.
;;
;; - Version 1 stealth fontification is also implemented by placing a function
;; on `post-command-hook'.  This function waits for a given amount of time,
;; and, if Emacs remains idle, fontifies where necessary.  Again, there are a
;; number of problems with using `post-command-hook':
;;
;; (a) Functions on `post-command-hook' are run sequentially, so this function
;;     can interfere with other functions on the hook, and vice versa.
;; (b) This function waits for a given amount of time, so it can interfere with
;;     various features that are dealt with by Emacs after a command, e.g.,
;;     region highlighting, asynchronous updating and keystroke echoing.
;; (c) Fontification may be required during a command, when `post-command-hook'
;;     is not run.  (Version 2 deferred fontification only.)
;;
;; Again, `post-command-hook' is completely inappropriate for lazy-lock.el.
;; Richard Stallman and Morten Welinder implemented internal Timers and Idle
;; Timers for Emacs 19.31.  Functions can be run independently at given times
;; or after given amounts of idle time.  Thus, the feature deals with the above
;; problems (a), (b) and (c).  Version 2 deferral and stealth are implemented
;; by functions on Idle Timers.  (A function on XEmacs' `pre-idle-hook' is
;; similar to an Emacs Idle Timer function with a fixed zero second timeout.)

;; - Version 1 has the following problems (relative to version 2):
;;
;; (a) It is slow when it does its job.
;; (b) It does not always do its job when it should.
;; (c) It slows all interaction (when it doesn't need to do its job).
;; (d) It interferes with other package functions on `post-command-hook'.
;; (e) It interferes with Emacs things within the read-eval loop.
;;
;; Ben's hacked-up lazy-lock.el 1.14 almost solved (b) but made (c) worse.
;;
;; - Version 2 has the following additional features (relative to version 1):
;;
;; (a) It can defer fontification (both on-the-fly and on-scrolling).
;; (b) It can fontify contextually (syntactically true on-the-fly).

;; Caveats:
;;
;; Lazy Lock mode does not work efficiently with Outline mode.
;; This is because when in Outline mode, although text may be not visible to
;; you in the window, the text is visible to Emacs Lisp code (not surprisingly)
;; and Lazy Lock fontifies it mercilessly.  Maybe it will be fixed one day.
;;
;; Because buffer text is not necessarily fontified, other packages that expect
;; buffer text to be fontified in Font Lock mode either might not work as
;; expected, or might not display buffer text as expected.  An example of the
;; latter is `occur', which copies lines of buffer text into another buffer.
;;
;; In Emacs 19.30, Lazy Lock mode does not ensure that an existing buffer is
;; fontified if it is made visible via a minibuffer-less command that replaces
;; an existing window's buffer (e.g., via the Buffers menu).  Upgrade!
;;
;; In Emacs 19.30, Lazy Lock mode does not work well with Transient Mark mode
;; or modes based on Comint mode (e.g., Shell mode), and also interferes with
;; the echoing of keystrokes in the minibuffer.  This is because of the way
;; deferral and stealth have to be implemented for Emacs 19.30.  Upgrade!
;;
;; Currently XEmacs does not have the features to support this version of
;; lazy-lock.el.  Maybe it will one day.

;; History:
;;
;; 1.15--2.00:
;; - Rewrite for Emacs 19.30 and the features rms added to support lazy-lock.el
;;   so that it could work correctly and efficiently.
;; - Many thanks to those who reported bugs, fixed bugs, made suggestions or
;;   otherwise contributed in the version 1 cycle; Jari Aalto, Kevin Broadey,
;;   Ulrik Dickow, Bill Dubuque, Bob Glickstein, Boris Goldowsky,
;;   Jonas Jarnestrom, David Karr, Michael Kifer, Erik Naggum, Rick Sladkey,
;;   Jim Thompson, Ben Wing, Ilya Zakharevich, and Richard Stallman.
;; 2.00--2.01:
;; - Made `lazy-lock-fontify-after-command' always `sit-for' and so redisplay
;; - Use `buffer-name' not `buffer-live-p' (Bill Dubuque hint)
;; - Made `lazy-lock-install' do `add-to-list' not `setq' of `current-buffer'
;; - Made `lazy-lock-fontify-after-install' loop over buffer list
;; - Made `lazy-lock-arrange-before-change' to arrange `window-end' triggering
;; - Made `lazy-lock-let-buffer-state' wrap both `befter-change-functions'
;; - Made `lazy-lock-fontify-region' do `condition-case' (Hyman Rosen report)
;; 2.01--2.02:
;; - Use `buffer-live-p' as `buffer-name' can barf (Richard Stanton report)
;; - Made `lazy-lock-install' set `font-lock-fontified' (Kevin Davidson report)
;; - Made `lazy-lock-install' add hooks only if needed
;; - Made `lazy-lock-unstall' add `font-lock-after-change-function' if needed
;; 2.02--2.03:
;; - Made `lazy-lock-fontify-region' do `condition-case' for `quit' too
;; - Made `lazy-lock-mode' respect the value of `font-lock-inhibit-thing-lock'
;; - Added `lazy-lock-after-unfontify-buffer'
;; - Removed `lazy-lock-fontify-after-install' hack
;; - Made `lazy-lock-fontify-after-scroll' not `set-buffer' to `window-buffer'
;; - Made `lazy-lock-fontify-after-trigger' not `set-buffer' to `window-buffer'
;; - Made `lazy-lock-fontify-after-idle' be interruptible (Scott Burson hint)
;; 2.03--2.04:
;; - Rewrite for Emacs 19.31 idle timers
;; - Renamed `buffer-windows' to `get-buffer-window-list'
;; - Removed `buffer-live-p'
;; - Made `lazy-lock-defer-after-change' always save `current-buffer'
;; - Made `lazy-lock-fontify-after-defer' just process buffers
;; - Made `lazy-lock-install-hooks' add hooks correctly (Kevin Broadey report)
;; - Made `lazy-lock-install' cope if `lazy-lock-defer-time' is a list
;; 2.04--2.05:
;; - Rewrite for Common Lisp macros
;; - Added `do-while' macro
;; - Renamed `lazy-lock-let-buffer-state' macro to `save-buffer-state'
;; - Returned `lazy-lock-fontify-after-install' hack (Darren Hall hint)
;; - Added `lazy-lock-defer-on-scrolling' functionality (Scott Byer hint)
;; - Made `lazy-lock-mode' wrap `font-lock-support-mode'
;; 2.05--2.06:
;; - Made `lazy-lock-fontify-after-defer' swap correctly (Scott Byer report)
;; 2.06--2.07:
;; - Added `lazy-lock-stealth-load' functionality (Rob Hooft hint)
;; - Made `lazy-lock-unstall' call `lazy-lock-fontify-region' if needed
;; - Made `lazy-lock-mode' call `lazy-lock-unstall' only if needed
;; - Made `lazy-lock-defer-after-scroll' do `set-window-redisplay-end-trigger'
;; - Added `lazy-lock-defer-contextually' functionality
;; - Added `lazy-lock-defer-on-the-fly' from `lazy-lock-defer-time'
;; - Renamed `lazy-lock-defer-driven' to `lazy-lock-defer-on-scrolling'
;; - Removed `lazy-lock-submit-bug-report' and bade farewell
;; 2.07--2.08:
;; - Made `lazy-lock-fontify-conservatively' fontify around `window-point'
;; - Made `save-buffer-state' wrap `inhibit-point-motion-hooks'
;; - Added Custom support
;; 2.08--2.09:
;; - Removed `byte-*' variables from `eval-when-compile' (Erik Naggum hint)
;; - Made various wrapping `inhibit-point-motion-hooks' (Vinicius Latorre hint)
;; - Made `lazy-lock-fontify-after-idle' wrap `minibuffer-auto-raise'
;; - Made `lazy-lock-fontify-after-defer' paranoid about deferred buffers
;; 2.09--2.10:
;; - Use `window-end' UPDATE arg for Emacs 20.4 and later.
;; - Made deferral `widen' before unfontifying (Dan Nicolaescu report)
;; - Use `lazy-lock-fontify-after-visage' for hideshow.el (Dan Nicolaescu hint)
;; - Use `other' widget where possible (Andreas Schwab fix)
;; 2.10--2.11:
;; - Used `with-temp-message' where possible to make messages temporary.

;;; Code:

(require 'font-lock)

(eval-when-compile
 ;; We don't do this at the top-level as we only use non-autoloaded macros.
 (require 'cl)
 ;;
 ;; We use this to preserve or protect things when modifying text properties.
 (defmacro save-buffer-state (varlist &rest body)
   "Bind variables according to VARLIST and eval BODY restoring buffer state."
   `(let* (,@(append varlist
		   '((modified (buffer-modified-p))
		     (buffer-undo-list t)
		     (inhibit-read-only t)
		     (inhibit-point-motion-hooks t)
		     (inhibit-modification-hooks t)
		     deactivate-mark
		     buffer-file-name
		     buffer-file-truename)))
     ,@body
     (when (and (not modified) (buffer-modified-p))
       (restore-buffer-modified-p nil))))
 (put 'save-buffer-state 'lisp-indent-function 1)
 ;;
 ;; We use this for clarity and speed.  Naughty but nice.
 (defmacro do-while (test &rest body)
   "(do-while TEST BODY...): eval BODY... and repeat if TEST yields non-nil.
The order of execution is thus BODY, TEST, BODY, TEST and so on
until TEST returns nil."
   `(while (progn ,@body ,test)))
 (put 'do-while 'lisp-indent-function (get 'while 'lisp-indent-function)))

(defgroup lazy-lock nil
  "Font Lock support mode to fontify lazily."
  :group 'font-lock)

(defvar lazy-lock-mode nil)			; Whether we are turned on.
(defvar lazy-lock-buffers nil)			; For deferral.
(defvar lazy-lock-timers (cons nil nil))	; For deferral and stealth.

;; User Variables:

(defcustom lazy-lock-minimum-size 25600
  "Minimum size of a buffer for demand-driven fontification.
On-demand fontification occurs if the buffer size is greater than this value.
If nil, means demand-driven fontification is never performed.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 25600) (c++-mode . 25600) (rmail-mode . 1048576))
means that the minimum size is 25K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "none" nil)
		 (integer :tag "size")
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . nil))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Size"
				      (const :tag "none" nil)
				      (integer :tag "size")))))
  :group 'lazy-lock)

(defcustom lazy-lock-defer-on-the-fly t
  "If non-nil, means fontification after a change should be deferred.
If nil, means on-the-fly fontification is performed.  This means when changes
occur in the buffer, those areas are immediately fontified.
If a list, it should be a list of `major-mode' symbol names for which deferred
fontification should occur.  The sense of the list is negated if it begins with
`not'.  For example:
 (c-mode c++-mode)
means that on-the-fly fontification is deferred for buffers in C and C++ modes
only, and deferral does not occur otherwise.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'lazy-lock)

(defcustom lazy-lock-defer-on-scrolling nil
  "If non-nil, means fontification after a scroll should be deferred.
If nil, means demand-driven fontification is performed.  This means when
scrolling into unfontified areas of the buffer, those areas are immediately
fontified.  Thus scrolling never presents unfontified areas.  However, since
fontification occurs during scrolling, scrolling may be slow.
If t, means defer-driven fontification is performed.  This means fontification
of those areas is deferred.  Thus scrolling may present momentarily unfontified
areas.  However, since fontification does not occur during scrolling, scrolling
will be faster than demand-driven fontification.
If any other value, e.g., `eventually', means demand-driven fontification is
performed until the buffer is fontified, then buffer fontification becomes
defer-driven.  Thus scrolling never presents unfontified areas until the buffer
is first fontified, after which subsequent scrolling may present future buffer
insertions momentarily unfontified.  However, since fontification does not
occur during scrolling after the buffer is first fontified, scrolling will
become faster.  (But, since contextual changes continually occur, such a value
makes little sense if `lazy-lock-defer-contextually' is non-nil.)

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (other :tag "eventually" eventually))
  :group 'lazy-lock)

(defcustom lazy-lock-defer-contextually 'syntax-driven
  "If non-nil, means deferred fontification should be syntactically true.
If nil, means deferred fontification occurs only on those lines modified.  This
means where modification on a line causes syntactic change on subsequent lines,
those subsequent lines are not refontified to reflect their new context.
If t, means deferred fontification occurs on those lines modified and all
subsequent lines.  This means those subsequent lines are refontified to reflect
their new syntactic context, either immediately or when scrolling into them.
If any other value, e.g., `syntax-driven', means deferred syntactically true
fontification occurs only if syntactic fontification is performed using the
buffer mode's syntax table, i.e., only if `font-lock-keywords-only' is nil.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (other :tag "syntax-driven" syntax-driven))
  :group 'lazy-lock)

(defcustom lazy-lock-defer-time 0.25
  "Time in seconds to delay before beginning deferred fontification.
Deferred fontification occurs if there is no input within this time.
If nil, means fontification is never deferred, regardless of the values of the
variables `lazy-lock-defer-on-the-fly', `lazy-lock-defer-on-scrolling' and
`lazy-lock-defer-contextually'.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-time 30
  "Time in seconds to delay before beginning stealth fontification.
Stealth fontification occurs if there is no input within this time.
If nil, means stealth fontification is never performed.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-lines (if font-lock-maximum-decoration 100 250)
  "Maximum size of a chunk of stealth fontification.
Each iteration of stealth fontification can fontify this number of lines.
To speed up input response during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable."
  :type '(integer :tag "lines")
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-load
  (if (condition-case nil (load-average) (error)) 200)
  "Load in percentage above which stealth fontification is suspended.
Stealth fontification pauses when the system short-term load average (as
returned by the function `load-average' if supported) goes above this level,
thus reducing the demand that stealth fontification makes on the system.
If nil, means stealth fontification is never suspended.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable.
See also `lazy-lock-stealth-nice'."
  :type (if (condition-case nil (load-average) (error))
	    '(choice (const :tag "never" nil)
		     (integer :tag "load"))
	  '(const :format "%t: unsupported\n" nil))
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-nice 0.125
  "Time in seconds to pause between chunks of stealth fontification.
Each iteration of stealth fontification is separated by this amount of time,
thus reducing the demand that stealth fontification makes on the system.
If nil, means stealth fontification is never paused.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could increase the value of this variable.
See also `lazy-lock-stealth-load'."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-verbose
  (and (not lazy-lock-defer-contextually) (not (null font-lock-verbose)))
  "If non-nil, means stealth fontification should show status messages."
  :type 'boolean
  :group 'lazy-lock)

;; User Functions:

;;;###autoload
(defun lazy-lock-mode (&optional arg)
  "Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive.  Enable it
automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'lazy-lock-mode)

For a newer font-lock support mode with similar functionality, see
`jit-lock-mode'.  Eventually, Lazy Lock mode will be deprecated in
JIT Lock's favor.

When Lazy Lock mode is enabled, fontification can be lazy in a number of ways:

- Demand-driven buffer fontification if `lazy-lock-minimum-size' is non-nil.
  This means initial fontification does not occur if the buffer is greater than
  `lazy-lock-minimum-size' characters in length.  Instead, fontification occurs
  when necessary, such as when scrolling through the buffer would otherwise
  reveal unfontified areas.  This is useful if buffer fontification is too slow
  for large buffers.

- Deferred scroll fontification if `lazy-lock-defer-on-scrolling' is non-nil.
  This means demand-driven fontification does not occur as you scroll.
  Instead, fontification is deferred until after `lazy-lock-defer-time' seconds
  of Emacs idle time, while Emacs remains idle.  This is useful if
  fontification is too slow to keep up with scrolling.

- Deferred on-the-fly fontification if `lazy-lock-defer-on-the-fly' is non-nil.
  This means on-the-fly fontification does not occur as you type.  Instead,
  fontification is deferred until after `lazy-lock-defer-time' seconds of Emacs
  idle time, while Emacs remains idle.  This is useful if fontification is too
  slow to keep up with your typing.

- Deferred context fontification if `lazy-lock-defer-contextually' is non-nil.
  This means fontification updates the buffer corresponding to true syntactic
  context, after `lazy-lock-defer-time' seconds of Emacs idle time, while Emacs
  remains idle.  Otherwise, fontification occurs on modified lines only, and
  subsequent lines can remain fontified corresponding to previous syntactic
  contexts.  This is useful where strings or comments span lines.

- Stealthy buffer fontification if `lazy-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `lazy-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

Basic Font Lock mode on-the-fly fontification behavior fontifies modified
lines only.  Thus, if `lazy-lock-defer-contextually' is non-nil, Lazy Lock mode
on-the-fly fontification may fontify differently, albeit correctly.  In any
event, to refontify some lines you can use \\[font-lock-fontify-block].

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `lazy-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `lazy-lock-stealth-nice' and `lazy-lock-stealth-lines', and
verbosity is controlled via the variable `lazy-lock-stealth-verbose'."
  (interactive "P")
  (let* ((was-on lazy-lock-mode)
	 (now-on (unless (memq 'lazy-lock-mode font-lock-inhibit-thing-lock)
		   (if arg (> (prefix-numeric-value arg) 0) (not was-on)))))
    (cond ((and now-on (not font-lock-mode))
	   ;; Turned on `lazy-lock-mode' rather than `font-lock-mode'.
           (message "Use font-lock-support-mode rather than calling lazy-lock-mode")
           (sit-for 2))
	  (now-on
	   ;; Turn ourselves on.
	   (set (make-local-variable 'lazy-lock-mode) t)
	   (lazy-lock-install))
	  (was-on
	   ;; Turn ourselves off.
	   (set (make-local-variable 'lazy-lock-mode) nil)
	   (lazy-lock-unstall)))))

;;;###autoload
(defun turn-on-lazy-lock ()
  "Unconditionally turn on Lazy Lock mode."
  (lazy-lock-mode t))

(defun lazy-lock-install ()
  (let ((min-size (font-lock-value-in-major-mode lazy-lock-minimum-size))
	(defer-change (and lazy-lock-defer-time lazy-lock-defer-on-the-fly))
	(defer-scroll (and lazy-lock-defer-time lazy-lock-defer-on-scrolling))
	(defer-context (and lazy-lock-defer-time lazy-lock-defer-contextually
			    (or (eq lazy-lock-defer-contextually t)
				(null font-lock-keywords-only)))))
    ;;
    ;; Tell Font Lock whether Lazy Lock will do fontification.
    (make-local-variable 'font-lock-fontified)
    (setq font-lock-fontified (and min-size (>= (buffer-size) min-size)))
    ;;
    ;; Add the text properties and fontify.
    (if (not font-lock-fontified)
	(lazy-lock-after-fontify-buffer)
      ;; Make sure we fontify in any existing windows showing the buffer.
      (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)))
	(lazy-lock-after-unfontify-buffer)
	(while windows
	  (lazy-lock-fontify-conservatively (car windows))
	  (setq windows (cdr windows)))))
    ;;
    ;; Add the fontification hooks.
    (lazy-lock-install-hooks
     font-lock-fontified
     (cond ((eq (car-safe defer-change) 'not)
	    (not (memq major-mode (cdr defer-change))))
	   ((listp defer-change)
	    (memq major-mode defer-change))
	   (t
	    defer-change))
     (eq defer-scroll t)
     defer-context)
    ;;
    ;; Add the fontification timers.
    (lazy-lock-install-timers
     (if (or defer-change defer-scroll defer-context) lazy-lock-defer-time)
     lazy-lock-stealth-time)))

(defun lazy-lock-install-hooks (fontifying
				defer-change defer-scroll defer-context)
  ;;
  ;; Add hook if lazy-lock.el is fontifying on scrolling or is deferring.
  (when (or fontifying defer-change defer-scroll defer-context)
    (add-hook 'window-scroll-functions (if defer-scroll
					   'lazy-lock-defer-after-scroll
					 'lazy-lock-fontify-after-scroll)
	      nil t))
  ;;
  ;; Add hook if lazy-lock.el is fontifying and is not deferring changes.
  (when (and fontifying (not defer-change) (not defer-context))
    (add-hook 'before-change-functions 'lazy-lock-arrange-before-change nil t))
  ;;
  ;; Replace Font Lock mode hook.
  (remove-hook 'after-change-functions 'font-lock-after-change-function t)
  (add-hook 'after-change-functions
	    (cond ((and defer-change defer-context)
		   'lazy-lock-defer-rest-after-change)
		  (defer-change
		   'lazy-lock-defer-line-after-change)
		  (defer-context
		   'lazy-lock-fontify-rest-after-change)
		  (t
		   'lazy-lock-fontify-line-after-change))
	    nil t)
  ;;
  ;; Add package-specific hook.
  (add-hook 'outline-view-change-hook 'lazy-lock-fontify-after-visage nil t)
  (add-hook 'hs-hide-hook 'lazy-lock-fontify-after-visage nil t))

(defun lazy-lock-install-timers (dtime stime)
  ;; Schedule or re-schedule the deferral and stealth timers.
  ;; The layout of `lazy-lock-timers' is:
  ;;  ((DEFER-TIME . DEFER-TIMER) (STEALTH-TIME . STEALTH-TIMER)
  ;; If an idle timeout has changed, cancel the existing idle timer (if there
  ;; is one) and schedule a new one (if the new idle timeout is non-nil).
  (unless (eq dtime (car (car lazy-lock-timers)))
    (let ((defer (car lazy-lock-timers)))
      (when (cdr defer)
	(cancel-timer (cdr defer)))
      (setcar lazy-lock-timers (cons dtime (and dtime
	      (run-with-idle-timer dtime t 'lazy-lock-fontify-after-defer))))))
  (unless (eq stime (car (cdr lazy-lock-timers)))
    (let ((stealth (cdr lazy-lock-timers)))
      (when (cdr stealth)
	(cancel-timer (cdr stealth)))
      (setcdr lazy-lock-timers (cons stime (and stime
	      (run-with-idle-timer stime t 'lazy-lock-fontify-after-idle)))))))

(defun lazy-lock-unstall ()
  ;;
  ;; If Font Lock mode is still enabled, make sure that the buffer is
  ;; fontified, and reinstall its hook.  We must do this first.
  (when font-lock-mode
    (when (lazy-lock-unfontified-p)
      (let ((verbose (if (numberp font-lock-verbose)
			 (> (buffer-size) font-lock-verbose)
		       font-lock-verbose)))
	(with-temp-message
	    (when verbose
	      (format "Fontifying %s..." (buffer-name)))
	  ;; Make sure we fontify etc. in the whole buffer.
	  (save-restriction
	    (widen)
	    (lazy-lock-fontify-region (point-min) (point-max))))))
    (add-hook 'after-change-functions 'font-lock-after-change-function nil t))
  ;;
  ;; Remove the text properties.
  (lazy-lock-after-unfontify-buffer)
  ;;
  ;; Remove the fontification hooks.
  (remove-hook 'window-scroll-functions 'lazy-lock-fontify-after-scroll t)
  (remove-hook 'window-scroll-functions 'lazy-lock-defer-after-scroll t)
  (remove-hook 'before-change-functions 'lazy-lock-arrange-before-change t)
  (remove-hook 'after-change-functions 'lazy-lock-fontify-line-after-change t)
  (remove-hook 'after-change-functions 'lazy-lock-fontify-rest-after-change t)
  (remove-hook 'after-change-functions 'lazy-lock-defer-line-after-change t)
  (remove-hook 'after-change-functions 'lazy-lock-defer-rest-after-change t)
  (remove-hook 'outline-view-change-hook 'lazy-lock-fontify-after-visage t)
  (remove-hook 'hs-hide-hook 'lazy-lock-fontify-after-visage t))

;; Hook functions.

;; Lazy Lock mode intervenes when (1) a previously invisible buffer region
;; becomes visible, i.e., for demand- or defer-driven on-the-scroll
;; fontification, (2) a buffer modification occurs, i.e., for defer-driven
;; on-the-fly fontification, (3) Emacs becomes idle, i.e., for fontification of
;; deferred fontification and stealth fontification, and (4) other special
;; occasions.

;; 1.  There are three ways whereby this can happen.
;;
;; (a) Scrolling the window, either explicitly (e.g., `scroll-up') or
;;     implicitly (e.g., `search-forward').  Here, `window-start' changes.
;;     Fontification occurs by adding `lazy-lock-fontify-after-scroll' (for
;;     demand-driven fontification) or `lazy-lock-defer-after-scroll' (for
;;     defer-driven fontification) to the hook `window-scroll-functions'.

(defun lazy-lock-fontify-after-scroll (window window-start)
  ;; Called from `window-scroll-functions'.
  ;; Fontify WINDOW from WINDOW-START following the scroll.
  (let ((inhibit-point-motion-hooks t))
    (lazy-lock-fontify-region window-start (window-end window t)))
  ;; A prior deletion that did not cause scrolling, followed by a scroll, would
  ;; result in an unnecessary trigger after this if we did not cancel it now.
  (set-window-redisplay-end-trigger window nil))

(defun lazy-lock-defer-after-scroll (window window-start)
  ;; Called from `window-scroll-functions'.
  ;; Defer fontification following the scroll.  Save the current buffer so that
  ;; we subsequently fontify in all windows showing the buffer.
  (unless (memq (current-buffer) lazy-lock-buffers)
    (push (current-buffer) lazy-lock-buffers))
  ;; A prior deletion that did not cause scrolling, followed by a scroll, would
  ;; result in an unnecessary trigger after this if we did not cancel it now.
  (set-window-redisplay-end-trigger window nil))

;; (b) Resizing the window, either explicitly (e.g., `enlarge-window') or
;;     implicitly (e.g., `delete-other-windows').  Here, `window-end' changes.
;;     Fontification occurs by adding `lazy-lock-fontify-after-resize' to the
;;     hook `window-size-change-functions'.

(defun lazy-lock-fontify-after-resize (frame)
  ;; Called from `window-size-change-functions'.
  ;; Fontify windows in FRAME following the resize.  We cannot use
  ;; `window-start' or `window-end' so we fontify conservatively.
  (save-excursion
    (save-selected-window
      (select-frame frame)
      (walk-windows (function (lambda (window)
		       (set-buffer (window-buffer window))
		       (when lazy-lock-mode
			 (lazy-lock-fontify-conservatively window))
		       (set-window-redisplay-end-trigger window nil)))
		    'nomini frame))))

;; (c) Deletion in the buffer.  Here, a `window-end' marker can become visible.
;;     Fontification occurs by adding `lazy-lock-arrange-before-change' to
;;     `before-change-functions' and `lazy-lock-fontify-after-trigger' to the
;;     hook `redisplay-end-trigger-functions'.  Before every deletion, the
;;     marker `window-redisplay-end-trigger' position is set to the soon-to-be
;;     changed `window-end' position.  If the marker becomes visible,
;;     `lazy-lock-fontify-after-trigger' gets called.  Ouch.  Note that we only
;;     have to deal with this eventuality if there is no on-the-fly deferral.

(defun lazy-lock-arrange-before-change (beg end)
  ;; Called from `before-change-functions'.
  ;; Arrange that if text becomes visible it will be fontified (if a deletion
  ;; is pending, text might become visible at the bottom).
  (unless (eq beg end)
    (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)) window)
      (while windows
	(setq window (car windows))
	(unless (markerp (window-redisplay-end-trigger window))
	  (set-window-redisplay-end-trigger window (make-marker)))
	(set-marker (window-redisplay-end-trigger window) (window-end window))
	(setq windows (cdr windows))))))

(defun lazy-lock-fontify-after-trigger (window trigger-point)
  ;; Called from `redisplay-end-trigger-functions'.
  ;; Fontify WINDOW from TRIGGER-POINT following the redisplay.
  ;; We could probably just use `lazy-lock-fontify-after-scroll' without loss:
  ;;  (inline (lazy-lock-fontify-after-scroll window (window-start window)))
  (let ((inhibit-point-motion-hooks t))
    (lazy-lock-fontify-region trigger-point (window-end window t))))

;; 2.  Modified text must be marked as unfontified so it can be identified and
;;     fontified later when Emacs is idle.  Deferral occurs by adding one of
;;     `lazy-lock-fontify-*-after-change' (for on-the-fly fontification) or
;;     `lazy-lock-defer-*-after-change' (for deferred fontification) to the
;;     hook `after-change-functions'.

(defalias 'lazy-lock-fontify-line-after-change
  ;; Called from `after-change-functions'.
  ;; Fontify the current change.
  'font-lock-after-change-function)

(defun lazy-lock-fontify-rest-after-change (beg end old-len)
  ;; Called from `after-change-functions'.
  ;; Fontify the current change and defer fontification of the rest of the
  ;; buffer.  Save the current buffer so that we subsequently fontify in all
  ;; windows showing the buffer.
  (lazy-lock-fontify-line-after-change beg end old-len)
  (save-buffer-state nil
    (unless (memq (current-buffer) lazy-lock-buffers)
      (push (current-buffer) lazy-lock-buffers))
    (save-restriction
      (widen)
      (remove-text-properties end (point-max) '(lazy-lock nil)))))

(defun lazy-lock-defer-line-after-change (beg end old-len)
  ;; Called from `after-change-functions'.
  ;; Defer fontification of the current change.  Save the current buffer so
  ;; that we subsequently fontify in all windows showing the buffer.
  (save-buffer-state nil
    (unless (memq (current-buffer) lazy-lock-buffers)
      (push (current-buffer) lazy-lock-buffers))
    (remove-text-properties (max (1- beg) (point-min))
			    (min (1+ end) (point-max))
			    '(lazy-lock nil))))

(defun lazy-lock-defer-rest-after-change (beg end old-len)
  ;; Called from `after-change-functions'.
  ;; Defer fontification of the rest of the buffer.  Save the current buffer so
  ;; that we subsequently fontify in all windows showing the buffer.
  (save-buffer-state nil
    (unless (memq (current-buffer) lazy-lock-buffers)
      (push (current-buffer) lazy-lock-buffers))
    (save-restriction
      (widen)
      (remove-text-properties (max (1- beg) (point-min))
			      (point-max)
			      '(lazy-lock nil)))))

;; 3.  Deferred fontification and stealth fontification are done from these two
;;     functions.  They are set up as Idle Timers.

(defun lazy-lock-fontify-after-defer ()
  ;; Called from `timer-idle-list'.
  ;; Fontify all windows where deferral has occurred for its buffer.
  (save-excursion
    (while (and lazy-lock-buffers (not (input-pending-p)))
      (let ((buffer (car lazy-lock-buffers)) windows)
	;; Paranoia: check that the buffer is still live and Lazy Lock mode on.
	(when (buffer-live-p buffer)
	  (set-buffer buffer)
	  (when lazy-lock-mode
	    (setq windows (get-buffer-window-list buffer 'nomini t))
	    (while windows
	      (lazy-lock-fontify-window (car windows))
	      (setq windows (cdr windows)))))
	(setq lazy-lock-buffers (cdr lazy-lock-buffers)))))
  ;; Add hook if fontification should now be defer-driven in this buffer.
  (when (and lazy-lock-mode lazy-lock-defer-on-scrolling
	     (memq 'lazy-lock-fontify-after-scroll window-scroll-functions)
	     (not (or (input-pending-p) (lazy-lock-unfontified-p))))
    (remove-hook 'window-scroll-functions 'lazy-lock-fontify-after-scroll t)
    (add-hook 'window-scroll-functions 'lazy-lock-defer-after-scroll nil t)))

(defun lazy-lock-fontify-after-idle ()
  ;; Called from `timer-idle-list'.
  ;; Fontify all buffers that need it, stealthily while idle.
  (unless (or executing-kbd-macro (window-minibuffer-p (selected-window)))
    ;; Loop over all buffers, fontify stealthily for each if necessary.
    (let ((buffers (buffer-list)) (continue t)
	  message message-log-max minibuffer-auto-raise)
      (save-excursion
	(do-while (and buffers continue)
	  (set-buffer (car buffers))
	  (if (not (and lazy-lock-mode (lazy-lock-unfontified-p)))
	      (setq continue (not (input-pending-p)))
	    ;; Fontify regions in this buffer while there is no input.
	    (with-temp-message
		(when lazy-lock-stealth-verbose
		  "Fontifying stealthily...")
	      (do-while (and (lazy-lock-unfontified-p) continue)
		(if (and lazy-lock-stealth-load
			 (> (car (load-average)) lazy-lock-stealth-load))
		    ;; Wait a while before continuing with the loop.
		    (progn
		      (when message
			(message "Fontifying stealthily...suspended")
			(setq message nil))
		      (setq continue (sit-for (or lazy-lock-stealth-time 30))))
		  ;; Fontify a chunk.
		  (when lazy-lock-stealth-verbose
		    (if message
			(message "Fontifying stealthily... %2d%% of %s"
				 (lazy-lock-percent-fontified) (buffer-name))
		      (message "Fontifying stealthily...")
		      (setq message t)))
		  ;; Current buffer may have changed during `sit-for'.
		  (set-buffer (car buffers))
		  (lazy-lock-fontify-chunk)
		  (setq continue (sit-for (or lazy-lock-stealth-nice 0)))))))
	  (setq buffers (cdr buffers)))))))

;; 4.  Special circumstances.

(defun lazy-lock-fontify-after-visage ()
  ;; Called from `outline-view-change-hook' and `hs-hide-hook'.
  ;; Fontify windows showing the current buffer, as its visibility has changed.
  ;; This is a conspiracy hack between lazy-lock.el, outline.el and
  ;; hideshow.el.
  (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)))
    (while windows
      (lazy-lock-fontify-conservatively (car windows))
      (setq windows (cdr windows)))))

(defun lazy-lock-after-fontify-buffer ()
  ;; Called from `font-lock-after-fontify-buffer'.
  ;; Mark the current buffer as fontified.
  ;; This is a conspiracy hack between lazy-lock.el and font-lock.el.
  (save-buffer-state nil
    (add-text-properties (point-min) (point-max) '(lazy-lock t))))

(defun lazy-lock-after-unfontify-buffer ()
  ;; Called from `font-lock-after-unfontify-buffer'.
  ;; Mark the current buffer as unfontified.
  ;; This is a conspiracy hack between lazy-lock.el and font-lock.el.
  (save-buffer-state nil
    (remove-text-properties (point-min) (point-max) '(lazy-lock nil))))

;; Fontification functions.

;; If packages want to ensure that some region of the buffer is fontified, they
;; should use this function.  For an example, see ps-print.el.
(defun lazy-lock-fontify-region (beg end)
  ;; Fontify between BEG and END, where necessary, in the current buffer.
  (save-restriction
    (widen)
    (when (setq beg (text-property-any beg end 'lazy-lock nil))
      (save-excursion
	(save-match-data
	  (save-buffer-state
	   ;; Ensure syntactic fontification is always correct.
	   (font-lock-beginning-of-syntax-function next)
	   ;; Find successive unfontified regions between BEG and END.
	   (condition-case data
	       (do-while beg
			 (setq next (or (text-property-any beg end 'lazy-lock t) end))
	  ;; Make sure the region end points are at beginning of line.
			 (goto-char beg)
			 (unless (bolp)
			   (beginning-of-line)
			   (setq beg (point)))
			 (goto-char next)
			 (unless (bolp)
			   (forward-line)
			   (setq next (point)))
		     ;; Fontify the region, then flag it as fontified.
			 (font-lock-fontify-region beg next)
			 (add-text-properties beg next '(lazy-lock t))
			 (setq beg (text-property-any next end 'lazy-lock nil)))
	     ((error quit) (message "Fontifying region...%s" data)))))))))

(defun lazy-lock-fontify-chunk ()
  ;; Fontify the nearest chunk, for stealth, in the current buffer.
  (let ((inhibit-point-motion-hooks t))
    (save-excursion
      (save-restriction
	(widen)
	;; Move to end of line in case the character at point is not fontified.
	(end-of-line)
	;; Find where the previous (next) unfontified regions end (begin).
	(let ((prev (previous-single-property-change (point) 'lazy-lock))
	      (next (text-property-any (point) (point-max) 'lazy-lock nil)))
	  ;; Fontify from the nearest unfontified position.
	  (if (or (null prev) (and next (< (- next (point)) (- (point) prev))))
	      ;; The next, or neither, region is the nearest not fontified.
	      (lazy-lock-fontify-region
	       (progn (goto-char (or next (point-min)))
		      (beginning-of-line)
		      (point))
	       (progn (goto-char (or next (point-min)))
		      (forward-line lazy-lock-stealth-lines)
		      (point)))
	    ;; The previous region is the nearest not fontified.
	    (lazy-lock-fontify-region
	     (progn (goto-char prev)
		    (forward-line (- lazy-lock-stealth-lines))
		    (point))
	     (progn (goto-char prev)
		    (forward-line)
		    (point)))))))))

(defun lazy-lock-fontify-window (window)
  ;; Fontify in WINDOW between `window-start' and `window-end'.
  ;; We can only do this when we can use `window-start' and `window-end'.
  (with-current-buffer (window-buffer window)
    (lazy-lock-fontify-region (window-start window) (window-end window))))

(defun lazy-lock-fontify-conservatively (window)
  ;; Fontify in WINDOW conservatively around point.
  ;; Where we cannot use `window-start' and `window-end' we do `window-height'
  ;; lines around point.  That way we guarantee to have done enough.
  (with-current-buffer (window-buffer window)
    (let ((inhibit-point-motion-hooks t))
      (lazy-lock-fontify-region
       (save-excursion
	 (goto-char (window-point window))
	 (vertical-motion (- (window-height window)) window) (point))
       (save-excursion
	 (goto-char (window-point window))
	 (vertical-motion (window-height window) window) (point))))))

(defun lazy-lock-unfontified-p ()
  ;; Return non-nil if there is anywhere still to be fontified.
  (save-restriction
    (widen)
    (text-property-any (point-min) (point-max) 'lazy-lock nil)))

(defun lazy-lock-percent-fontified ()
  ;; Return the percentage (of characters) of the buffer that are fontified.
  (save-restriction
    (widen)
    (let ((beg (point-min)) (size 0) next)
      ;; Find where the next fontified region begins.
      (while (setq beg (text-property-any beg (point-max) 'lazy-lock t))
	(setq next (or (text-property-any beg (point-max) 'lazy-lock nil)
		       (point-max)))
	(incf size (- next beg))
	(setq beg next))
      ;; Float because using integer multiplication will frequently overflow.
      (truncate (* (/ (float size) (point-max)) 100)))))

;; Version dependent workarounds and fixes.

(when (consp lazy-lock-defer-time)
  ;;
  ;; In 2.06.04 and below, `lazy-lock-defer-time' could specify modes and time.
  (with-output-to-temp-buffer "*Help*"
    (princ "The value of the variable `lazy-lock-defer-time' was\n ")
    (princ lazy-lock-defer-time)
    (princ "\n")
    (princ "This variable cannot now be a list of modes and time,\n")
    (princ "so instead use ")
    (princ (substitute-command-keys "\\[customize-option]"))
    (princ " to modify the variables, or put the forms:\n")
    (princ " (setq lazy-lock-defer-time ")
    (princ (cdr lazy-lock-defer-time))
    (princ ")\n")
    (princ " (setq lazy-lock-defer-on-the-fly '")
    (princ (car lazy-lock-defer-time))
    (princ ")\n")
    (princ "in your ~/.emacs.  ")
    (princ "The above forms have been evaluated for this editor session,\n")
    (princ "but you should use ")
    (princ (substitute-command-keys "\\[customize-option]"))
    (princ " or change your ~/.emacs now."))
  (setq lazy-lock-defer-on-the-fly (car lazy-lock-defer-time)
	lazy-lock-defer-time (cdr lazy-lock-defer-time)))

(when (boundp 'lazy-lock-defer-driven)
  ;;
  ;; In 2.06.04 and below, `lazy-lock-defer-driven' was the variable name.
  (with-output-to-temp-buffer "*Help*"
    (princ "The value of the variable `lazy-lock-defer-driven' is set to ")
    (if (memq lazy-lock-defer-driven '(nil t))
	(princ lazy-lock-defer-driven)
      (princ "`")
      (princ lazy-lock-defer-driven)
      (princ "'"))
    (princ ".\n")
    (princ "This variable is now called `lazy-lock-defer-on-scrolling',\n")
    (princ "so instead use ")
    (princ (substitute-command-keys "\\[customize-option]"))
    (princ " to modify the variable, or put the form:\n")
    (princ " (setq lazy-lock-defer-on-scrolling ")
    (unless (memq lazy-lock-defer-driven '(nil t))
      (princ "'"))
    (princ lazy-lock-defer-driven)
    (princ ")\n")
    (princ "in your ~/.emacs.  ")
    (princ "The above form has been evaluated for this editor session,\n")
    (princ "but you should use ")
    (princ (substitute-command-keys "\\[customize-option]"))
    (princ " or change your ~/.emacs now."))
  (setq lazy-lock-defer-on-scrolling lazy-lock-defer-driven))

;; Install ourselves:

(add-hook 'window-size-change-functions 'lazy-lock-fontify-after-resize)
(add-hook 'redisplay-end-trigger-functions 'lazy-lock-fontify-after-trigger)

(unless (assq 'lazy-lock-mode minor-mode-alist)
  (setq minor-mode-alist (append minor-mode-alist '((lazy-lock-mode nil)))))

;; Provide ourselves:

(provide 'lazy-lock)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:

;;; lazy-lock.el ends here
