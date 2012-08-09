;;; mouse.el --- window system-independent mouse support

;; Copyright (C) 1993-1995, 1999-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: hardware, mouse
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

;; This package provides various useful commands (including help
;; system access) through the mouse.  All this code assumes that mouse
;; interpretation has been abstracted into Emacs input events.
;;
;; The code is rather X-dependent.

;;; Code:

;;; Utility functions.

;; Indent track-mouse like progn.
(put 'track-mouse 'lisp-indent-function 0)

(defcustom mouse-yank-at-point nil
  "If non-nil, mouse yank commands yank at point instead of at click."
  :type 'boolean
  :group 'mouse)

(defcustom mouse-drag-copy-region nil
  "If non-nil, copy to kill-ring upon mouse adjustments of the region.

This affects `mouse-save-then-kill' (\\[mouse-save-then-kill]) in
addition to mouse drags."
  :type 'boolean
  :version "24.1"
  :group 'mouse)

(defcustom mouse-1-click-follows-link 450
  "Non-nil means that clicking Mouse-1 on a link follows the link.

With the default setting, an ordinary Mouse-1 click on a link
performs the same action as Mouse-2 on that link, while a longer
Mouse-1 click \(hold down the Mouse-1 button for more than 450
milliseconds) performs the original Mouse-1 binding \(which
typically sets point where you click the mouse).

If value is an integer, the time elapsed between pressing and
releasing the mouse button determines whether to follow the link
or perform the normal Mouse-1 action (typically set point).
The absolute numeric value specifies the maximum duration of a
\"short click\" in milliseconds.  A positive value means that a
short click follows the link, and a longer click performs the
normal action.  A negative value gives the opposite behavior.

If value is `double', a double click follows the link.

Otherwise, a single Mouse-1 click unconditionally follows the link.

Note that dragging the mouse never follows the link.

This feature only works in modes that specifically identify
clickable text as links, so it may not work with some external
packages.  See `mouse-on-link-p' for details."
  :version "22.1"
  :type '(choice (const :tag "Disabled" nil)
		 (const :tag "Double click" double)
                 (number :tag "Single click time limit" :value 450)
                 (other :tag "Single click" t))
  :group 'mouse)

(defcustom mouse-1-click-in-non-selected-windows t
  "If non-nil, a Mouse-1 click also follows links in non-selected windows.

If nil, a Mouse-1 click on a link in a non-selected window performs
the normal mouse-1 binding, typically selects the window and sets
point at the click position."
  :type 'boolean
  :version "22.1"
  :group 'mouse)



;; Provide a mode-specific menu on a mouse button.

(defun popup-menu (menu &optional position prefix)
  "Popup the given menu and call the selected option.
MENU can be a keymap, an easymenu-style menu or a list of keymaps as for
`x-popup-menu'.
POSITION can be a click event or ((XOFFSET YOFFSET) WINDOW) and defaults to
  the current mouse position.
PREFIX is the prefix argument (if any) to pass to the command."
  (let* ((map (cond
	       ((keymapp menu) menu)
	       ((and (listp menu) (keymapp (car menu))) menu)
	       (t (let* ((map (easy-menu-create-menu (car menu) (cdr menu)))
			 (filter (when (symbolp map)
				   (plist-get (get map 'menu-prop) :filter))))
		    (if filter (funcall filter (symbol-function map)) map)))))
	 event cmd)
    (unless position
      (let ((mp (mouse-pixel-position)))
	(setq position (list (list (cadr mp) (cddr mp)) (car mp)))))
    ;; The looping behavior was taken from lmenu's popup-menu-popup
    (while (and map (setq event
			  ;; map could be a prefix key, in which case
			  ;; we need to get its function cell
			  ;; definition.
			  (x-popup-menu position (indirect-function map))))
      ;; Strangely x-popup-menu returns a list.
      ;; mouse-major-mode-menu was using a weird:
      ;; (key-binding (apply 'vector (append '(menu-bar) menu-prefix events)))
      (setq cmd
	    (if (and (not (keymapp map)) (listp map))
		;; We were given a list of keymaps.  Search them all
		;; in sequence until a first binding is found.
		(let ((mouse-click (apply 'vector event))
		      binding)
		  (while (and map (null binding))
		    (setq binding (lookup-key (car map) mouse-click))
		    (if (numberp binding) ; `too long'
			(setq binding nil))
		    (setq map (cdr map)))
		  binding)
	      ;; We were given a single keymap.
	      (lookup-key map (apply 'vector event))))
      ;; Clear out echoing, which perhaps shows a prefix arg.
      (message "")
      ;; Maybe try again but with the submap.
      (setq map (if (keymapp cmd) cmd)))
    ;; If the user did not cancel by refusing to select,
    ;; and if the result is a command, run it.
    (when (and (null map) (commandp cmd))
      (setq prefix-arg prefix)
      ;; `setup-specified-language-environment', for instance,
      ;; expects this to be set from a menu keymap.
      (setq last-command-event (car (last event)))
      ;; mouse-major-mode-menu was using `command-execute' instead.
      (call-interactively cmd))))

(defun minor-mode-menu-from-indicator (indicator)
  "Show menu for minor mode specified by INDICATOR.
Interactively, INDICATOR is read using completion.
If there is no menu defined for the minor mode, then create one with
items `Turn Off' and `Help'."
  (interactive
   (list (completing-read
	  "Minor mode indicator: "
	  (describe-minor-mode-completion-table-for-indicator))))
  (let* ((minor-mode (lookup-minor-mode-from-indicator indicator))
         (mm-fun (or (get minor-mode :minor-mode-function) minor-mode)))
    (unless minor-mode (error "Cannot find minor mode for `%s'" indicator))
    (let* ((map (cdr-safe (assq minor-mode minor-mode-map-alist)))
           (menu (and (keymapp map) (lookup-key map [menu-bar]))))
      (setq menu
            (if menu
                (mouse-menu-non-singleton menu)
	      `(keymap
                ,indicator
                (turn-off menu-item "Turn Off minor mode" ,mm-fun)
                (help menu-item "Help for minor mode"
                      (lambda () (interactive)
                        (describe-function ',mm-fun))))))
      (popup-menu menu))))

(defun mouse-minor-mode-menu (event)
  "Show minor-mode menu for EVENT on minor modes area of the mode line."
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (minor-mode-menu-from-indicator indicator)))

(defun mouse-menu-major-mode-map ()
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* (;; Keymap from which to inherit; may be null.
	 (ancestor (mouse-menu-non-singleton
		    (and (current-local-map)
			 (local-key-binding [menu-bar]))))
	 ;; Make a keymap in which our last command leads to a menu or
	 ;; default to the edit menu.
	 (newmap (if ancestor
		     (make-sparse-keymap (concat (format-mode-line mode-name)
                                                 " Mode"))
		   menu-bar-edit-menu))
	 uniq)
    (if ancestor
	(set-keymap-parent newmap ancestor))
    newmap))

(defun mouse-menu-non-singleton (menubar)
  "Return menu keybar MENUBAR, or a lone submenu inside it.
If MENUBAR defines exactly one submenu, return just that submenu.
Otherwise, return MENUBAR."
  (if menubar
      (let (submap)
        (map-keymap
         (lambda (k v) (setq submap (if submap t (cons k v))))
         (keymap-canonicalize menubar))
        (if (eq submap t)
            menubar
          (lookup-key menubar (vector (car submap)))))))

(defun mouse-menu-bar-map ()
  "Return a keymap equivalent to the menu bar.
The contents are the items that would be in the menu bar whether or
not it is actually displayed."
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* ((local-menu (and (current-local-map)
			  (lookup-key (current-local-map) [menu-bar])))
	 (global-menu (lookup-key global-map [menu-bar]))
	 ;; If a keymap doesn't have a prompt string (a lazy
	 ;; programmer didn't bother to provide one), create it and
	 ;; insert it into the keymap; each keymap gets its own
	 ;; prompt.  This is required for non-toolkit versions to
	 ;; display non-empty menu pane names.
	 (minor-mode-menus
	  (mapcar
           (lambda (menu)
             (let* ((minor-mode (car menu))
                    (menu (cdr menu))
                    (title-or-map (cadr menu)))
               (or (stringp title-or-map)
                   (setq menu
                         (cons 'keymap
                               (cons (concat
                                      (capitalize (subst-char-in-string
                                                   ?- ?\s (symbol-name
                                                           minor-mode)))
                                      " Menu")
                                     (cdr menu)))))
               menu))
	   (minor-mode-key-binding [menu-bar])))
	 (local-title-or-map (and local-menu (cadr local-menu)))
	 (global-title-or-map (cadr global-menu)))
    (or (null local-menu)
	(stringp local-title-or-map)
	(setq local-menu (cons 'keymap
			       (cons (concat (format-mode-line mode-name)
                                             " Mode Menu")
				     (cdr local-menu)))))
    (or (stringp global-title-or-map)
	(setq global-menu (cons 'keymap
			        (cons "Global Menu"
				      (cdr global-menu)))))
    ;; Supplying the list is faster than making a new map.
    ;; FIXME: We have a problem here: we have to use the global/local/minor
    ;; so they're displayed in the expected order, but later on in the command
    ;; loop, they're actually looked up in the opposite order.
    (apply 'append
           global-menu
           local-menu
           minor-mode-menus)))

(defun mouse-major-mode-menu (event &optional prefix)
  "Pop up a mode-specific menu of mouse commands.
Default to the Edit menu if the major mode doesn't define a menu."
  (interactive "@e\nP")
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (popup-menu (mouse-menu-major-mode-map) event prefix))
(make-obsolete 'mouse-major-mode-menu 'mouse-menu-major-mode-map "23.1")

(defun mouse-popup-menubar (event prefix)
  "Pop up a menu equivalent to the menu bar for keyboard EVENT with PREFIX.
The contents are the items that would be in the menu bar whether or
not it is actually displayed."
  (interactive "@e \nP")
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (popup-menu (mouse-menu-bar-map) (unless (integerp event) event) prefix))
(make-obsolete 'mouse-popup-menubar 'mouse-menu-bar-map "23.1")

(defun mouse-popup-menubar-stuff (event prefix)
  "Popup a menu like either `mouse-major-mode-menu' or `mouse-popup-menubar'.
Use the former if the menu bar is showing, otherwise the latter."
  (interactive "@e\nP")
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (popup-menu
   (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
       (mouse-menu-bar-map)
     (mouse-menu-major-mode-map))
   event prefix))
(make-obsolete 'mouse-popup-menubar-stuff nil "23.1")

;; Commands that operate on windows.

(defun mouse-minibuffer-check (event)
  (let ((w (posn-window (event-start event))))
    (and (window-minibuffer-p w)
	 (not (minibuffer-window-active-p w))
	 (error "Minibuffer window is not active")))
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook))

(defun mouse-delete-window (click)
  "Delete the window you click on.
Do nothing if the frame has just one window.
This command must be bound to a mouse click."
  (interactive "e")
  (unless (one-window-p t)
    (mouse-minibuffer-check click)
    (delete-window (posn-window (event-start click)))))

(defun mouse-select-window (click)
  "Select the window clicked on; don't move point."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((oframe (selected-frame))
	(frame (window-frame (posn-window (event-start click)))))
    (select-window (posn-window (event-start click)))
    (raise-frame frame)
    (select-frame frame)
    (or (eq frame oframe)
	(set-mouse-position (selected-frame) (1- (frame-width)) 0))))

(defun mouse-tear-off-window (click)
  "Delete the window clicked on, and create a new frame displaying its buffer."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((window (posn-window (event-start click)))
	 (buf (window-buffer window))
	 (frame (make-frame)))
    (select-frame frame)
    (switch-to-buffer buf)
    (delete-window window)))

(defun mouse-delete-other-windows ()
  "Delete all windows except the one you click on."
  (interactive "@")
  (delete-other-windows))

(defun mouse-split-window-vertically (click)
  "Select Emacs window mouse is on, then split it vertically in half.
The window is split at the line clicked on.
This command must be bound to a mouse click."
  (interactive "@e")
  (mouse-minibuffer-check click)
  (let ((start (event-start click)))
    (select-window (posn-window start))
    (let ((new-height (1+ (cdr (posn-col-row (event-end click)))))
	  (first-line window-min-height)
	  (last-line (- (window-height) window-min-height)))
      (if (< last-line first-line)
	  (error "Window too short to split")
	(split-window-vertically
	 (min (max new-height first-line) last-line))))))

(defun mouse-split-window-horizontally (click)
  "Select Emacs window mouse is on, then split it horizontally in half.
The window is split at the column clicked on.
This command must be bound to a mouse click."
  (interactive "@e")
  (mouse-minibuffer-check click)
  (let ((start (event-start click)))
    (select-window (posn-window start))
    (let ((new-width (1+ (car (posn-col-row (event-end click)))))
	  (first-col window-min-width)
	  (last-col (- (window-width) window-min-width)))
      (if (< last-col first-col)
	  (error "Window too narrow to split")
	(split-window-horizontally
	 (min (max new-width first-col) last-col))))))

;; `mouse-drag-line' is now the common routine for handling all line
;; dragging events combining the earlier `mouse-drag-mode-line-1' and
;; `mouse-drag-vertical-line'.  It should improve the behavior of line
;; dragging wrt Emacs 23 as follows:

;; (1) Gratuitous error messages and restrictions have been (hopefully)
;; removed.  (The help-echo that dragging the mode-line can resize a
;; one-window-frame's window will still show through via bindings.el.)

;; (2) No gratuitous selection of other windows should happen.  (This
;; has not been completely fixed for mouse-autoselected windows yet.)

;; (3) Mouse clicks below a scroll-bar should pass through via unread
;; command events.

;; Note that `window-in-direction' replaces `mouse-drag-window-above'
;; and `mouse-drag-vertical-line-rightward-window' with Emacs 24.1.
(defun mouse-drag-line (start-event line)
  "Drag some line with the mouse.
START-EVENT is the starting mouse-event of the drag action.  LINE
must be one of the symbols header, mode, or vertical."
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let* ((echo-keystrokes 0)
	 (start (event-start start-event))
	 (window (posn-window start))
	 (frame (window-frame window))
	 (minibuffer-window (minibuffer-window frame))
         (on-link (and mouse-1-click-follows-link
		       (or mouse-1-click-in-non-selected-windows
			   (eq window (selected-window)))
		       (mouse-on-link-p start)))
	 (resize-minibuffer
	  ;; Resize the minibuffer window if it's on the same frame as
	  ;; and immediately below the position window and it's either
	  ;; active or `resize-mini-windows' is nil.
	  (and (eq line 'mode)
	       (eq (window-frame minibuffer-window) frame)
	       (= (nth 1 (window-edges minibuffer-window))
		  (nth 3 (window-edges window)))
	       (or (not resize-mini-windows)
		   (eq minibuffer-window (active-minibuffer-window)))))
	 (which-side
	  (and (eq line 'vertical)
	       (or (cdr (assq 'vertical-scroll-bars (frame-parameters frame)))
		   'right)))
	 done event mouse growth dragged)
    (cond
     ((eq line 'header)
      ;; Check whether header-line can be dragged at all.
      (if (window-at-side-p window 'top)
	  (setq done t)
	(setq window (window-in-direction 'above window t))))
     ((eq line 'mode)
      ;; Check whether mode-line can be dragged at all.
      (when (and (window-at-side-p window 'bottom)
		 (not resize-minibuffer))
	(setq done t)))
     ((eq line 'vertical)
      ;; Get the window to adjust for the vertical case.
      (setq window
	    (if (eq which-side 'right)
		;; If the scroll bar is on the window's right or there's
		;; no scroll bar at all, adjust the window where the
		;; start-event occurred.
		window
	      ;; If the scroll bar is on the start-event window's left,
	      ;; adjust the window on the left of it.
	      (window-in-direction 'left window t)))))

    ;; Start tracking.
    (track-mouse
      ;; Loop reading events and sampling the position of the mouse.
      (while (not done)
	(setq event (read-event))
	(setq mouse (mouse-position))
	;; Do nothing if
	;;   - there is a switch-frame event.
	;;   - the mouse isn't in the frame that we started in
	;;   - the mouse isn't in any Emacs frame
	;; Drag if
	;;   - there is a mouse-movement event
	;;   - there is a scroll-bar-movement event (??)
	;;     (same as mouse movement for our purposes)
	;; Quit if
	;;   - there is a keyboard event or some other unknown event.
	(cond
	 ((not (consp event))
	  (setq done t))
	 ((memq (car event) '(switch-frame select-window))
	  nil)
	 ((not (memq (car event) '(mouse-movement scroll-bar-movement)))
	  (when (consp event)
	    ;; Do not unread a drag-mouse-1 event to avoid selecting
	    ;; some other window.  For vertical line dragging do not
	    ;; unread mouse-1 events either (but only if we dragged at
	    ;; least once to allow mouse-1 clicks get through.
	    (unless (and dragged
			 (if (eq line 'vertical)
			     (memq (car event) '(drag-mouse-1 mouse-1))
			   (eq (car event) 'drag-mouse-1)))
	      (push event unread-command-events)))
	  (setq done t))
	 ((or (not (eq (car mouse) frame)) (null (car (cdr mouse))))
	  nil)
	 ((eq line 'vertical)
	  ;; Drag vertical divider (the calculations below are those
	  ;; from Emacs 23).
	  (setq growth
		(- (- (cadr mouse)
		      (if (eq which-side 'right) 0 2))
		   (nth 2 (window-edges window))
		   -1))
	  (unless (zerop growth)
	    ;; Remember that we dragged.
	    (setq dragged t))
	  (adjust-window-trailing-edge window growth t))
	 (t
	  ;; Drag horizontal divider (the calculations below are those
	  ;; from Emacs 23).
	  (setq growth
		(if (eq line 'mode)
		    (- (cddr mouse) (nth 3 (window-edges window)) -1)
		  ;; The window's top includes the header line!
		  (- (nth 3 (window-edges window)) (cddr mouse))))

	  (unless (zerop growth)
	    ;; Remember that we dragged.
	    (setq dragged t))

	  (if (eq line 'mode)
	      (adjust-window-trailing-edge window growth)
	    (adjust-window-trailing-edge window (- growth))))))

      ;; Presumably, if this was just a click, the last event should be
      ;; `mouse-1', whereas if this did move the mouse, it should be a
      ;; `drag-mouse-1'.  `dragged' nil tells us that we never dragged
      ;; and `on-link' tells us that there is a link to follow.
      (when (and on-link (not dragged)
		 (eq 'mouse-1 (car-safe (car unread-command-events))))
	;; If mouse-2 has never been done by the user, it doesn't
	;; have the necessary property to be interpreted correctly.
	(put 'mouse-2 'event-kind 'mouse-click)
	(setcar unread-command-events
		(cons 'mouse-2 (cdar unread-command-events)))))))

(defun mouse-drag-mode-line (start-event)
  "Change the height of a window by dragging on the mode line."
  (interactive "e")
  (mouse-drag-line start-event 'mode))

(defun mouse-drag-header-line (start-event)
  "Change the height of a window by dragging on the header line."
  (interactive "e")
  (mouse-drag-line start-event 'header))

(defun mouse-drag-vertical-line (start-event)
  "Change the width of a window by dragging on the vertical line."
  (interactive "e")
  (mouse-drag-line start-event 'vertical))

(defun mouse-set-point (event)
  "Move point to the position clicked on with the mouse.
This should be bound to a mouse click event type."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (posn-set-point (event-end event)))

(defvar mouse-last-region-beg nil)
(defvar mouse-last-region-end nil)
(defvar mouse-last-region-tick nil)

(defun mouse-region-match ()
  "Return non-nil if there's an active region that was set with the mouse."
  (and (mark t) mark-active
       (eq mouse-last-region-beg (region-beginning))
       (eq mouse-last-region-end (region-end))
       (eq mouse-last-region-tick (buffer-modified-tick))))

(defun mouse-set-region (click)
  "Set the region to the text dragged over, and copy to kill ring.
This should be bound to a mouse drag event.
See the `mouse-drag-copy-region' variable to control whether this
command alters the kill ring or not."
  (interactive "e")
  (mouse-minibuffer-check click)
  (select-window (posn-window (event-start click)))
  (let ((beg (posn-point (event-start click)))
	(end (posn-point (event-end click))))
    (and mouse-drag-copy-region (integerp beg) (integerp end)
	 ;; Don't set this-command to `kill-region', so a following
	 ;; C-w won't double the text in the kill ring.  Ignore
	 ;; `last-command' so we don't append to a preceding kill.
	 (let (this-command last-command deactivate-mark)
	   (copy-region-as-kill beg end)))
    (if (numberp beg) (goto-char beg))
    ;; On a text terminal, bounce the cursor.
    (or transient-mark-mode
	(window-system)
	(sit-for 1))
    (push-mark)
    (set-mark (point))
    (if (numberp end) (goto-char end))
    (mouse-set-region-1)))

(defun mouse-set-region-1 ()
  ;; Set transient-mark-mode for a little while.
  (unless (eq (car-safe transient-mark-mode) 'only)
    (setq transient-mark-mode
	  (cons 'only
		(unless (eq transient-mark-mode 'lambda)
		  transient-mark-mode))))
  (setq mouse-last-region-beg (region-beginning))
  (setq mouse-last-region-end (region-end))
  (setq mouse-last-region-tick (buffer-modified-tick)))

(defcustom mouse-scroll-delay 0.25
  "The pause between scroll steps caused by mouse drags, in seconds.
If you drag the mouse beyond the edge of a window, Emacs scrolls the
window to bring the text beyond that edge into view, with a delay of
this many seconds between scroll steps.  Scrolling stops when you move
the mouse back into the window, or release the button.
This variable's value may be non-integral.
Setting this to zero causes Emacs to scroll as fast as it can."
  :type 'number
  :group 'mouse)

(defcustom mouse-scroll-min-lines 1
  "The minimum number of lines scrolled by dragging mouse out of window.
Moving the mouse out the top or bottom edge of the window begins
scrolling repeatedly.  The number of lines scrolled per repetition
is normally equal to the number of lines beyond the window edge that
the mouse has moved.  However, it always scrolls at least the number
of lines specified by this variable."
  :type 'integer
  :group 'mouse)

(defun mouse-scroll-subr (window jump &optional overlay start)
  "Scroll the window WINDOW, JUMP lines at a time, until new input arrives.
If OVERLAY is an overlay, let it stretch from START to the far edge of
the newly visible text.
Upon exit, point is at the far edge of the newly visible text."
  (cond
   ((and (> jump 0) (< jump mouse-scroll-min-lines))
    (setq jump mouse-scroll-min-lines))
   ((and (< jump 0) (< (- jump) mouse-scroll-min-lines))
    (setq jump (- mouse-scroll-min-lines))))
  (let ((opoint (point)))
    (while (progn
	     (goto-char (window-start window))
	     (if (not (zerop (vertical-motion jump window)))
		 (progn
		   (set-window-start window (point))
		   (if (natnump jump)
		       (if (window-end window)
			   (progn
			     (goto-char (window-end window))
			     ;; window-end doesn't reflect the window's new
			     ;; start position until the next redisplay.
			     (vertical-motion (1- jump) window))
			 (vertical-motion (- (window-height window) 2)))
		     (goto-char (window-start window)))
		   (if overlay
		       (move-overlay overlay start (point)))
		   ;; Now that we have scrolled WINDOW properly,
		   ;; put point back where it was for the redisplay
		   ;; so that we don't mess up the selected window.
		   (or (eq window (selected-window))
		       (goto-char opoint))
		   (sit-for mouse-scroll-delay)))))
    (or (eq window (selected-window))
	(goto-char opoint))))

(defvar mouse-selection-click-count 0)

(defvar mouse-selection-click-count-buffer nil)

(defun mouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
In Transient Mark mode, the highlighting remains as long as the mark
remains active.  Otherwise, it remains until the next input event.

If the click is in the echo area, display the `*Messages*' buffer."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (mouse-drag-track start-event t))


(defun mouse-posn-property (pos property)
  "Look for a property at click position.
POS may be either a buffer position or a click position like
those returned from `event-start'.  If the click position is on
a string, the text property PROPERTY is examined.
If this is nil or the click is not on a string, then
the corresponding buffer position is searched for PROPERTY.
If PROPERTY is encountered in one of those places,
its value is returned."
  (if (consp pos)
      (let ((w (posn-window pos)) (pt (posn-point pos))
	    (str (posn-string pos)))
	(or (and str
		 (get-text-property (cdr str) property (car str)))
	    (and pt
		 (get-char-property pt property w))))
    (get-char-property pos property)))

(defun mouse-on-link-p (pos)
  "Return non-nil if POS is on a link in the current buffer.
POS must be a buffer position in the current buffer or a mouse
event location in the selected window (see `event-start').
However, if `mouse-1-click-in-non-selected-windows' is non-nil,
POS may be a mouse event location in any window.

A clickable link is identified by one of the following methods:

- If the character at POS has a non-nil `follow-link' text or
overlay property, the value of that property determines what to do.

- If there is a local key-binding or a keybinding at position POS
for the `follow-link' event, the binding of that event determines
what to do.

The resulting value determine whether POS is inside a link:

- If the value is `mouse-face', POS is inside a link if there
is a non-nil `mouse-face' property at POS.  Return t in this case.

- If the value is a function, FUNC, POS is inside a link if
the call \(FUNC POS) returns non-nil.  Return the return value
from that call.  Arg is \(posn-point POS) if POS is a mouse event.

- Otherwise, return the value itself.

The return value is interpreted as follows:

- If it is a string, the mouse-1 event is translated into the
first character of the string, i.e. the action of the mouse-1
click is the local or global binding of that character.

- If it is a vector, the mouse-1 event is translated into the
first element of that vector, i.e. the action of the mouse-1
click is the local or global binding of that event.

- Otherwise, the mouse-1 event is translated into a mouse-2 event
at the same position."
  (let ((action
	 (and (or (not (consp pos))
		  mouse-1-click-in-non-selected-windows
		  (eq (selected-window) (posn-window pos)))
	      (or (mouse-posn-property pos 'follow-link)
		  (key-binding [follow-link] nil t pos)))))
    (cond
     ((eq action 'mouse-face)
      (and (mouse-posn-property pos 'mouse-face) t))
     ((functionp action)
      ;; FIXME: This seems questionable if the click is not in a buffer.
      ;; Should we instead decide that `action' takes a `posn'?
      (if (consp pos)
	  (with-current-buffer (window-buffer (posn-window pos))
	    (funcall action (posn-point pos)))
	(funcall action pos)))
     (t action))))

(defun mouse-fixup-help-message (msg)
  "Fix help message MSG for `mouse-1-click-follows-link'."
  (let (mp pos)
    (if (and mouse-1-click-follows-link
	     (stringp msg)
	     (string-match-p "\\`mouse-2" msg)
	     (setq mp (mouse-pixel-position))
	     (consp (setq pos (cdr mp)))
	     (car pos) (>= (car pos) 0)
	     (cdr pos) (>= (cdr pos) 0)
	     (setq pos (posn-at-x-y (car pos) (cdr pos) (car mp)))
	     (windowp (posn-window pos)))
	(with-current-buffer (window-buffer (posn-window pos))
	  (if (mouse-on-link-p pos)
	      (setq msg (concat
		    (cond
		     ((eq mouse-1-click-follows-link 'double) "double-")
		     ((and (integerp mouse-1-click-follows-link)
			   (< mouse-1-click-follows-link 0)) "Long ")
		     (t ""))
		    "mouse-1" (substring msg 7)))))))
  msg)

(defun mouse-drag-track (start-event  &optional
				      do-mouse-drag-region-post-process)
    "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point.
DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
`mouse-drag-region'."
  (mouse-minibuffer-check start-event)
  (setq mouse-selection-click-count-buffer (current-buffer))
  (deactivate-mark)
  (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
	 (original-window (selected-window))
         ;; We've recorded what we needed from the current buffer and
         ;; window, now let's jump to the place of the event, where things
         ;; are happening.
         (_ (mouse-set-point start-event))
         (echo-keystrokes 0)
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
	 (start-window-start (window-start start-window))
	 (start-hscroll (window-hscroll start-window))
	 (bounds (window-edges start-window))
	 (make-cursor-line-fully-visible nil)
	 (top (nth 1 bounds))
	 (bottom (if (window-minibuffer-p start-window)
		     (nth 3 bounds)
		   ;; Don't count the mode line.
		   (1- (nth 3 bounds))))
	 (on-link (and mouse-1-click-follows-link
		       (or mouse-1-click-in-non-selected-windows
			   (eq start-window original-window))
                       ;; Use start-point before the intangibility
                       ;; treatment, in case we click on a link inside an
                       ;; intangible text.
                       (mouse-on-link-p start-posn)))
	 (click-count (1- (event-click-count start-event)))
	 (remap-double-click (and on-link
				  (eq mouse-1-click-follows-link 'double)
				  (= click-count 1)))
	 ;; Suppress automatic hscrolling, because that is a nuisance
	 ;; when setting point near the right fringe (but see below).
	 (automatic-hscrolling-saved automatic-hscrolling)
	 (automatic-hscrolling nil)
	 event end end-point)

    (setq mouse-selection-click-count click-count)
    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
	(goto-char start-point))
    (setq start-point (point))
    (if remap-double-click
	(setq click-count 0))

    ;; Activate the region, using `mouse-start-end' to determine where
    ;; to put point and mark (e.g., double-click will select a word).
    (setq transient-mark-mode
	  (if (eq transient-mark-mode 'lambda)
	      '(only)
	    (cons 'only transient-mark-mode)))
    (let ((range (mouse-start-end start-point start-point click-count)))
      (push-mark (nth 0 range) t t)
      (goto-char (nth 1 range)))

    ;; Track the mouse until we get a non-movement event.
    (track-mouse
      (while (progn
	       (setq event (read-event))
	       (or (mouse-movement-p event)
		   (memq (car-safe event) '(switch-frame select-window))))
	(unless (memq (car-safe event) '(switch-frame select-window))
	  ;; Automatic hscrolling did not occur during the call to
	  ;; `read-event'; but if the user subsequently drags the
	  ;; mouse, go ahead and hscroll.
	  (let ((automatic-hscrolling automatic-hscrolling-saved))
	    (redisplay))
	  (setq end (event-end event)
		end-point (posn-point end))
	  (if (and (eq (posn-window end) start-window)
		   (integer-or-marker-p end-point))
	      (mouse--drag-set-mark-and-point start-point
					      end-point click-count)
	    (let ((mouse-row (cdr (cdr (mouse-position)))))
	      (cond
	       ((null mouse-row))
	       ((< mouse-row top)
		(mouse-scroll-subr start-window (- mouse-row top)
				   nil start-point))
	       ((>= mouse-row bottom)
		(mouse-scroll-subr start-window (1+ (- mouse-row bottom))
				   nil start-point))))))))

    ;; Handle the terminating event if possible.
    (when (consp event)
      ;; Ensure that point is on the end of the last event.
      (when (and (setq end-point (posn-point (event-end event)))
		 (eq (posn-window end) start-window)
		 (integer-or-marker-p end-point)
		 (/= start-point end-point))
	(mouse--drag-set-mark-and-point start-point
					end-point click-count))

      ;; Find its binding.
      (let* ((fun (key-binding (vector (car event))))
	     (do-multi-click (and (> (event-click-count event) 0)
				  (functionp fun)
				  (not (memq fun '(mouse-set-point
						   mouse-set-region))))))
	(if (and (/= (mark) (point))
		 (not do-multi-click))

	    ;; If point has moved, finish the drag.
	    (let* (last-command this-command)
	      (and mouse-drag-copy-region
		   do-mouse-drag-region-post-process
		   (let (deactivate-mark)
		     (copy-region-as-kill (mark) (point)))))

	  ;; If point hasn't moved, run the binding of the
	  ;; terminating up-event.
	  (if do-multi-click
	      (goto-char start-point)
	    (deactivate-mark))
	  (when (and (functionp fun)
		     (= start-hscroll (window-hscroll start-window))
		     ;; Don't run the up-event handler if the window
		     ;; start changed in a redisplay after the
		     ;; mouse-set-point for the down-mouse event at
		     ;; the beginning of this function.  When the
		     ;; window start has changed, the up-mouse event
		     ;; contains a different position due to the new
		     ;; window contents, and point is set again.
		     (or end-point
			 (= (window-start start-window)
			    start-window-start)))
	    (when (and on-link
		       (= start-point (point))
		       (mouse--remap-link-click-p start-event event))
	      ;; If we rebind to mouse-2, reselect previous selected
	      ;; window, so that the mouse-2 event runs in the same
	      ;; situation as if user had clicked it directly.  Fixes
	      ;; the bug reported by juri@jurta.org on 2005-12-27.
	      (if (or (vectorp on-link) (stringp on-link))
		  (setq event (aref on-link 0))
		(select-window original-window)
		(setcar event 'mouse-2)
		;; If this mouse click has never been done by the
		;; user, it doesn't have the necessary property to be
		;; interpreted correctly.
		(put 'mouse-2 'event-kind 'mouse-click)))
	    (push event unread-command-events)))))))

(defun mouse--drag-set-mark-and-point (start click click-count)
  (let* ((range (mouse-start-end start click click-count))
	 (beg (nth 0 range))
	 (end (nth 1 range)))
    (cond ((eq (mark) beg)
	   (goto-char end))
	  ((eq (mark) end)
	   (goto-char beg))
	  ((< click (mark))
	   (set-mark end)
	   (goto-char beg))
	  (t
	   (set-mark beg)
	   (goto-char end)))))

(defun mouse--remap-link-click-p (start-event end-event)
  (or (and (eq mouse-1-click-follows-link 'double)
	   (= (event-click-count start-event) 2))
      (and
       (not (eq mouse-1-click-follows-link 'double))
       (= (event-click-count start-event) 1)
       (= (event-click-count end-event) 1)
       (or (not (integerp mouse-1-click-follows-link))
	   (let ((t0 (posn-timestamp (event-start start-event)))
		 (t1 (posn-timestamp (event-end   end-event))))
	     (and (integerp t0) (integerp t1)
		  (if (> mouse-1-click-follows-link 0)
		      (<= (- t1 t0) mouse-1-click-follows-link)
		    (< (- t0 t1) mouse-1-click-follows-link))))))))


;; Commands to handle xterm-style multiple clicks.
(defun mouse-skip-word (dir)
  "Skip over word, over whitespace, or over identical punctuation.
If DIR is positive skip forward; if negative, skip backward."
  (let* ((char (following-char))
	 (syntax (char-to-string (char-syntax char))))
    (cond ((string= syntax "w")
	   ;; Here, we can't use skip-syntax-forward/backward because
	   ;; they don't pay attention to word-separating-categories,
	   ;; and thus they will skip over a true word boundary.  So,
	   ;; we simulate the original behavior by using forward-word.
	   (if (< dir 0)
	       (if (not (looking-at "\\<"))
		   (forward-word -1))
	     (if (or (looking-at "\\<") (not (looking-at "\\>")))
		 (forward-word 1))))
	  ((string= syntax " ")
	   (if (< dir 0)
	       (skip-syntax-backward syntax)
	     (skip-syntax-forward syntax)))
	  ((string= syntax "_")
	   (if (< dir 0)
	       (skip-syntax-backward "w_")
	     (skip-syntax-forward "w_")))
	  ((< dir 0)
	   (while (and (not (bobp)) (= (preceding-char) char))
	     (forward-char -1)))
	  (t
	   (while (and (not (eobp)) (= (following-char) char))
	     (forward-char 1))))))

(defun mouse-start-end (start end mode)
  "Return a list of region bounds based on START and END according to MODE.
If MODE is 0 then set point to (min START END), mark to (max START END).
If MODE is 1 then set point to start of word at (min START END),
mark to end of word at (max START END).
If MODE is 2 then do the same for lines."
  (if (> start end)
      (let ((temp start))
        (setq start end
              end temp)))
  (setq mode (mod mode 3))
  (cond ((= mode 0)
	 (list start end))
        ((and (= mode 1)
              (= start end)
	      (char-after start)
              (= (char-syntax (char-after start)) ?\())
	 (list start
	       (save-excursion
		 (goto-char start)
		 (forward-sexp 1)
		 (point))))
        ((and (= mode 1)
              (= start end)
	      (char-after start)
              (= (char-syntax (char-after start)) ?\)))
	 (list (save-excursion
		 (goto-char (1+ start))
		 (backward-sexp 1)
		 (point))
	       (1+ start)))
	((and (= mode 1)
              (= start end)
	      (char-after start)
              (= (char-syntax (char-after start)) ?\"))
	 (let ((open (or (eq start (point-min))
			 (save-excursion
			   (goto-char (- start 1))
			   (looking-at "\\s(\\|\\s \\|\\s>")))))
	   (if open
	       (list start
		     (save-excursion
		       (condition-case nil
			   (progn
			     (goto-char start)
			     (forward-sexp 1)
			     (point))
			 (error end))))
	     (list (save-excursion
		     (condition-case nil
			 (progn
			   (goto-char (1+ start))
			   (backward-sexp 1)
			   (point))
		       (error end)))
		   (1+ start)))))
        ((= mode 1)
	 (list (save-excursion
		 (goto-char start)
		 (mouse-skip-word -1)
		 (point))
	       (save-excursion
		 (goto-char end)
		 (mouse-skip-word 1)
		 (point))))
        ((= mode 2)
	 (list (save-excursion
		 (goto-char start)
		 (line-beginning-position 1))
	       (save-excursion
		 (goto-char end)
		 (forward-line 1)
		 (point))))))

;; Subroutine: set the mark where CLICK happened,
;; but don't do anything else.
(defun mouse-set-mark-fast (click)
  (mouse-minibuffer-check click)
  (let ((posn (event-start click)))
    (select-window (posn-window posn))
    (if (numberp (posn-point posn))
	(push-mark (posn-point posn) t t))))

(defun mouse-undouble-last-event (events)
  (let* ((index (1- (length events)))
	 (last (nthcdr index events))
	 (event (car last))
	 (basic (event-basic-type event))
	 (old-modifiers (event-modifiers event))
	 (modifiers (delq 'double (delq 'triple (copy-sequence old-modifiers))))
	 (new
	  (if (consp event)
	      ;; Use reverse, not nreverse, since event-modifiers
	      ;; does not copy the list it returns.
	      (cons (event-convert-list (reverse (cons basic modifiers)))
		    (cdr event))
	    event)))
    (setcar last new)
    (if (and (not (equal modifiers old-modifiers))
	     (key-binding (apply 'vector events)))
	t
      (setcar last event)
      nil)))

;; Momentarily show where the mark is, if highlighting doesn't show it.

(defun mouse-set-mark (click)
  "Set mark at the position clicked on with the mouse.
Display cursor at that position for a second.
This must be bound to a mouse click."
  (interactive "e")
  (mouse-minibuffer-check click)
  (select-window (posn-window (event-start click)))
  ;; We don't use save-excursion because that preserves the mark too.
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point click)
	       (push-mark nil t t)
	       (or transient-mark-mode
		   (sit-for 1)))
      (goto-char point-save))))

(defun mouse-kill (click)
  "Kill the region between point and the mouse click.
The text is saved in the kill ring, as with \\[kill-region]."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn (event-start click))
	 (click-posn (posn-point posn)))
    (select-window (posn-window posn))
    (if (numberp click-posn)
	(kill-region (min (point) click-posn)
		     (max (point) click-posn)))))

(defun mouse-yank-at-click (click arg)
  "Insert the last stretch of killed text at the position clicked on.
Also move point to one end of the text thus inserted (normally the end),
and set mark at the beginning.
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (when select-active-regions
    ;; Without this, confusing things happen upon e.g. inserting into
    ;; the middle of an active region.
    (deactivate-mark))
  (or mouse-yank-at-point (mouse-set-point click))
  (setq this-command 'yank)
  (setq mouse-selection-click-count 0)
  (yank arg))

(defun mouse-yank-primary (click)
  "Insert the primary selection at the position clicked on.
Move point to the end of the inserted text, and set mark at
beginning.  If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  ;; Without this, confusing things happen upon e.g. inserting into
  ;; the middle of an active region.
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
  (or mouse-yank-at-point (mouse-set-point click))
  (let ((primary
	 (cond
	  ((eq system-type 'windows-nt)
	   ;; MS-Windows emulates PRIMARY in x-get-selection, but not
	   ;; in x-get-selection-value (the latter only accesses the
	   ;; clipboard).  So try PRIMARY first, in case they selected
	   ;; something with the mouse in the current Emacs session.
	   (or (x-get-selection 'PRIMARY)
	       (x-get-selection-value)))
	  ((fboundp 'x-get-selection-value) ; MS-DOS and X.
	   ;; On X, x-get-selection-value supports more formats and
	   ;; encodings, so use it in preference to x-get-selection.
	   (or (x-get-selection-value)
	       (x-get-selection 'PRIMARY)))
	  ;; FIXME: What about xterm-mouse-mode etc.?
	  (t
	   (x-get-selection 'PRIMARY)))))
    (unless primary
      (error "No selection is available"))
    (push-mark (point))
    (insert primary)))

(defun mouse-kill-ring-save (click)
  "Copy the region between point and the mouse click in the kill ring.
This does not delete the region; it acts like \\[kill-ring-save]."
  (interactive "e")
  (mouse-set-mark-fast click)
  (let (this-command last-command)
    (kill-ring-save (point) (mark t))))

;; This function used to delete the text between point and the mouse
;; whenever it was equal to the front of the kill ring, but some
;; people found that confusing.

;; The position of the last invocation of `mouse-save-then-kill'.
(defvar mouse-save-then-kill-posn nil)

(defun mouse-save-then-kill-delete-region (beg end)
  ;; We must make our own undo boundaries
  ;; because they happen automatically only for the current buffer.
  (undo-boundary)
  (if (or (= beg end) (eq buffer-undo-list t))
      ;; If we have no undo list in this buffer,
      ;; just delete.
      (delete-region beg end)
    ;; Delete, but make the undo-list entry share with the kill ring.
    ;; First, delete just one char, so in case buffer is being modified
    ;; for the first time, the undo list records that fact.
    (let (before-change-functions after-change-functions)
      (delete-region beg
		     (+ beg (if (> end beg) 1 -1))))
    (let ((buffer-undo-list buffer-undo-list))
      ;; Undo that deletion--but don't change the undo list!
      (let (before-change-functions after-change-functions)
	(primitive-undo 1 buffer-undo-list))
      ;; Now delete the rest of the specified region,
      ;; but don't record it.
      (setq buffer-undo-list t)
      (if (/= (length (car kill-ring)) (- (max end beg) (min end beg)))
	  (error "Lossage in mouse-save-then-kill-delete-region"))
      (delete-region beg end))
    (let ((tail buffer-undo-list))
      ;; Search back in buffer-undo-list for the string
      ;; that came from deleting one character.
      (while (and tail (not (stringp (car (car tail)))))
	(setq tail (cdr tail)))
      ;; Replace it with an entry for the entire deleted text.
      (and tail
	   (setcar tail (cons (car kill-ring) (min beg end))))))
  (undo-boundary))

(defun mouse-save-then-kill (click)
  "Set the region according to CLICK; the second time, kill it.
CLICK should be a mouse click event.

If the region is inactive, activate it temporarily.  Set mark at
the original point, and move point to the position of CLICK.

If the region is already active, adjust it.  Normally, do this by
moving point or mark, whichever is closer, to CLICK.  But if you
have selected whole words or lines, move point or mark to the
word or line boundary closest to CLICK instead.

If `mouse-drag-copy-region' is non-nil, this command also saves the
new region to the kill ring (replacing the previous kill if the
previous region was just saved to the kill ring).

If this command is called a second consecutive time with the same
CLICK position, kill the region (or delete it
if `mouse-drag-copy-region' is non-nil)"
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn     (event-start click))
	 (click-pt (posn-point posn))
	 (window   (posn-window posn))
	 (buf      (window-buffer window))
	 ;; Don't let a subsequent kill command append to this one.
	 (this-command this-command)
	 ;; Check if the user has multi-clicked to select words/lines.
	 (click-count
	  (if (and (eq mouse-selection-click-count-buffer buf)
		   (with-current-buffer buf (mark t)))
	      mouse-selection-click-count
	    0)))
    (cond
     ((not (numberp click-pt)) nil)
     ;; If the user clicked without moving point, kill the region.
     ;; This also resets `mouse-selection-click-count'.
     ((and (eq last-command 'mouse-save-then-kill)
	   (eq click-pt mouse-save-then-kill-posn)
	   (eq window (selected-window)))
      (if mouse-drag-copy-region
          ;; Region already saved in the previous click;
          ;; don't make a duplicate entry, just delete.
          (delete-region (mark t) (point))
        (kill-region (mark t) (point)))
      (setq mouse-selection-click-count 0)
      (setq mouse-save-then-kill-posn nil))

     ;; Otherwise, if there is a suitable region, adjust it by moving
     ;; one end (whichever is closer) to CLICK-PT.
     ((or (with-current-buffer buf (region-active-p))
	  (and (eq window (selected-window))
	       (mark t)
	       (or (and (eq last-command 'mouse-save-then-kill)
			mouse-save-then-kill-posn)
		   (and (memq last-command '(mouse-drag-region
					     mouse-set-region))
			(or mark-even-if-inactive
			    (not transient-mark-mode))))))
      (select-window window)
      (let* ((range (mouse-start-end click-pt click-pt click-count)))
	(if (< (abs (- click-pt (mark t)))
	       (abs (- click-pt (point))))
	    (set-mark (car range))
	  (goto-char (nth 1 range)))
	(setq deactivate-mark nil)
	(mouse-set-region-1)
        (when mouse-drag-copy-region
          ;; Region already copied to kill-ring once, so replace.
          (kill-new (filter-buffer-substring (mark t) (point)) t))
	;; Arrange for a repeated mouse-3 to kill the region.
	(setq mouse-save-then-kill-posn click-pt)))

     ;; Otherwise, set the mark where point is and move to CLICK-PT.
     (t
      (select-window window)
      (mouse-set-mark-fast click)
      (let ((before-scroll (with-current-buffer buf point-before-scroll)))
	(if before-scroll (goto-char before-scroll)))
      (exchange-point-and-mark)
      (mouse-set-region-1)
      (when mouse-drag-copy-region
        (kill-new (filter-buffer-substring (mark t) (point))))
      (setq mouse-save-then-kill-posn click-pt)))))


(global-set-key [M-mouse-1] 'mouse-start-secondary)
(global-set-key [M-drag-mouse-1] 'mouse-set-secondary)
(global-set-key [M-down-mouse-1] 'mouse-drag-secondary)
(global-set-key [M-mouse-3] 'mouse-secondary-save-then-kill)
(global-set-key [M-mouse-2] 'mouse-yank-secondary)

(defconst mouse-secondary-overlay
  (let ((ol (make-overlay (point-min) (point-min))))
    (delete-overlay ol)
    (overlay-put ol 'face 'secondary-selection)
    ol)
  "An overlay which records the current secondary selection.
It is deleted when there is no secondary selection.")

(defvar mouse-secondary-click-count 0)

;; A marker which records the specified first end for a secondary selection.
;; May be nil.
(defvar mouse-secondary-start nil)

(defun mouse-start-secondary (click)
  "Set one end of the secondary selection to the position clicked on.
Use \\[mouse-secondary-save-then-kill] to set the other end
and complete the secondary selection."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((posn (event-start click)))
    (with-current-buffer (window-buffer (posn-window posn))
      ;; Cancel any preexisting secondary selection.
      (delete-overlay mouse-secondary-overlay)
      (if (numberp (posn-point posn))
	  (progn
	    (or mouse-secondary-start
		(setq mouse-secondary-start (make-marker)))
	    (move-marker mouse-secondary-start (posn-point posn)))))))

(defun mouse-set-secondary (click)
  "Set the secondary selection to the text that the mouse is dragged over.
This must be bound to a mouse drag event."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((posn (event-start click))
	beg
	(end (event-end click)))
    (with-current-buffer (window-buffer (posn-window posn))
      (if (numberp (posn-point posn))
	  (setq beg (posn-point posn)))
      (move-overlay mouse-secondary-overlay beg (posn-point end))
      (x-set-selection
       'SECONDARY
       (buffer-substring (overlay-start mouse-secondary-overlay)
			 (overlay-end mouse-secondary-overlay))))))

(defun mouse-drag-secondary (start-event)
  "Set the secondary selection to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
The function returns a non-nil value if it creates a secondary selection."
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* ((echo-keystrokes 0)
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
	 (bounds (window-edges start-window))
	 (top (nth 1 bounds))
	 (bottom (if (window-minibuffer-p start-window)
		     (nth 3 bounds)
		   ;; Don't count the mode line.
		   (1- (nth 3 bounds))))
	 (click-count (1- (event-click-count start-event))))
    (with-current-buffer (window-buffer start-window)
      (setq mouse-secondary-click-count click-count)
      (if (> (mod click-count 3) 0)
	  ;; Double or triple press: make an initial selection
	  ;; of one word or line.
	  (let ((range (mouse-start-end start-point start-point click-count)))
	    (set-marker mouse-secondary-start nil)
	    (move-overlay mouse-secondary-overlay (car range) (nth 1 range)
			  (window-buffer start-window)))
	;; Single-press: cancel any preexisting secondary selection.
	(or mouse-secondary-start
	    (setq mouse-secondary-start (make-marker)))
	(set-marker mouse-secondary-start start-point)
	(delete-overlay mouse-secondary-overlay))
      (let (event end end-point)
	(track-mouse
	  (while (progn
		   (setq event (read-event))
		   (or (mouse-movement-p event)
		       (memq (car-safe event) '(switch-frame select-window))))

	    (if (memq (car-safe event) '(switch-frame select-window))
		nil
	      (setq end (event-end event)
		    end-point (posn-point end))
	      (cond
	       ;; Are we moving within the original window?
	       ((and (eq (posn-window end) start-window)
		     (integer-or-marker-p end-point))
		(let ((range (mouse-start-end start-point end-point
					      click-count)))
		  (if (or (/= start-point end-point)
			  (null (marker-position mouse-secondary-start)))
		      (progn
			(set-marker mouse-secondary-start nil)
			(move-overlay mouse-secondary-overlay
				      (car range) (nth 1 range))))))
               (t
                (let ((mouse-row (cdr (cdr (mouse-position)))))
                  (cond
                   ((null mouse-row))
                   ((< mouse-row top)
                    (mouse-scroll-subr start-window (- mouse-row top)
				       mouse-secondary-overlay start-point))
                   ((>= mouse-row bottom)
                    (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                       mouse-secondary-overlay start-point)))))))))

	(if (consp event)
	    (if (marker-position mouse-secondary-start)
		(save-window-excursion
		  (delete-overlay mouse-secondary-overlay)
		  (x-set-selection 'SECONDARY nil)
		  (select-window start-window)
		  (save-excursion
		    (goto-char mouse-secondary-start)
		    (sit-for 1)
		    nil))
	      (x-set-selection
	       'SECONDARY
	       (buffer-substring (overlay-start mouse-secondary-overlay)
				 (overlay-end mouse-secondary-overlay)))))))))

(defun mouse-yank-secondary (click)
  "Insert the secondary selection at the position clicked on.
Move point to the end of the inserted text.
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (or mouse-yank-at-point (mouse-set-point click))
  (let ((secondary (x-get-selection 'SECONDARY)))
    (if secondary
        (insert secondary)
      (error "No secondary selection"))))

(defun mouse-kill-secondary ()
  "Kill the text in the secondary selection.
This is intended more as a keyboard command than as a mouse command
but it can work as either one.

The current buffer (in case of keyboard use), or the buffer clicked on,
must be the one that the secondary selection is in.  This requirement
is to prevent accidents."
  (interactive)
  (let* ((keys (this-command-keys))
	 (click (elt keys (1- (length keys)))))
    (or (eq (overlay-buffer mouse-secondary-overlay)
	    (if (listp click)
		(window-buffer (posn-window (event-start click)))
	      (current-buffer)))
	(error "Select or click on the buffer where the secondary selection is")))
  (let (this-command)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (kill-region (overlay-start mouse-secondary-overlay)
		   (overlay-end mouse-secondary-overlay))))
  (delete-overlay mouse-secondary-overlay))

(defun mouse-secondary-save-then-kill (click)
  "Set the secondary selection and save it to the kill ring.
The second time, kill it.  CLICK should be a mouse click event.

If you have not called `mouse-start-secondary' in the clicked
buffer, activate the secondary selection and set it between point
and the click position CLICK.

Otherwise, adjust the bounds of the secondary selection.
Normally, do this by moving its beginning or end, whichever is
closer, to CLICK.  But if you have selected whole words or lines,
adjust to the word or line boundary closest to CLICK instead.

If this command is called a second consecutive time with the same
CLICK position, kill the secondary selection."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn     (event-start click))
	 (click-pt (posn-point posn))
	 (window   (posn-window posn))
	 (buf      (window-buffer window))
	 ;; Don't let a subsequent kill command append to this one.
	 (this-command this-command)
	 ;; Check if the user has multi-clicked to select words/lines.
	 (click-count
	  (if (eq (overlay-buffer mouse-secondary-overlay) buf)
	      mouse-secondary-click-count
	    0))
	 (beg (overlay-start mouse-secondary-overlay))
	 (end (overlay-end mouse-secondary-overlay)))

    (cond
     ((not (numberp click-pt)) nil)

     ;; If the secondary selection is not active in BUF, activate it.
     ((not (eq buf (or (overlay-buffer mouse-secondary-overlay)
		       (if mouse-secondary-start
			   (marker-buffer mouse-secondary-start)))))
      (select-window window)
      (setq mouse-secondary-start (make-marker))
      (move-marker mouse-secondary-start (point))
      (move-overlay mouse-secondary-overlay (point) click-pt buf)
      (kill-ring-save (point) click-pt))

     ;; If the user clicked without moving point, delete the secondary
     ;; selection.  This also resets `mouse-secondary-click-count'.
     ((and (eq last-command 'mouse-secondary-save-then-kill)
	   (eq click-pt mouse-save-then-kill-posn)
	   (eq window (selected-window)))
      (mouse-save-then-kill-delete-region beg end)
      (delete-overlay mouse-secondary-overlay)
      (setq mouse-secondary-click-count 0)
      (setq mouse-save-then-kill-posn nil))

     ;; Otherwise, if there is a suitable secondary selection overlay,
     ;; adjust it by moving one end (whichever is closer) to CLICK-PT.
     ((and beg (eq buf (overlay-buffer mouse-secondary-overlay)))
      (let* ((range (mouse-start-end click-pt click-pt click-count)))
	(if (< (abs (- click-pt beg))
	       (abs (- click-pt end)))
	    (move-overlay mouse-secondary-overlay (car range) end)
	  (move-overlay mouse-secondary-overlay beg (nth 1 range))))
      (setq deactivate-mark nil)
      (if (eq last-command 'mouse-secondary-save-then-kill)
	  ;; If the front of the kill ring comes from an immediately
	  ;; previous use of this command, replace the entry.
	  (kill-new
	   (buffer-substring (overlay-start mouse-secondary-overlay)
			     (overlay-end mouse-secondary-overlay))
	   t)
	(let (deactivate-mark)
	  (copy-region-as-kill (overlay-start mouse-secondary-overlay)
			       (overlay-end mouse-secondary-overlay))))
      (setq mouse-save-then-kill-posn click-pt))

     ;; Otherwise, set the secondary selection overlay.
     (t
      (select-window window)
      (if mouse-secondary-start
	  ;; All we have is one end of a selection, so put the other
	  ;; end here.
	  (let ((start (+ 0 mouse-secondary-start)))
	    (kill-ring-save start click-pt)
	    (move-overlay mouse-secondary-overlay start click-pt)))
      (setq mouse-save-then-kill-posn click-pt))))

  ;; Finally, set the window system's secondary selection.
  (let (str)
    (and (overlay-buffer mouse-secondary-overlay)
	 (setq str (buffer-substring (overlay-start mouse-secondary-overlay)
				     (overlay-end mouse-secondary-overlay)))
	 (> (length str) 0)
	 (x-set-selection 'SECONDARY str))))


(defcustom mouse-buffer-menu-maxlen 20
  "Number of buffers in one pane (submenu) of the buffer menu.
If we have lots of buffers, divide them into groups of
`mouse-buffer-menu-maxlen' and make a pane (or submenu) for each one."
  :type 'integer
  :group 'mouse)

(defcustom mouse-buffer-menu-mode-mult 4
  "Group the buffers by the major mode groups on \\[mouse-buffer-menu]?
This number which determines (in a hairy way) whether \\[mouse-buffer-menu]
will split the buffer menu by the major modes (see
`mouse-buffer-menu-mode-groups') or just by menu length.
Set to 1 (or even 0!) if you want to group by major mode always, and to
a large number if you prefer a mixed multitude.  The default is 4."
  :type 'integer
  :group 'mouse
  :version "20.3")

(defvar mouse-buffer-menu-mode-groups
  (mapcar (lambda (arg) (cons  (purecopy (car arg)) (purecopy (cdr arg))))
  '(("Info\\|Help\\|Apropos\\|Man" . "Help")
    ("\\bVM\\b\\|\\bMH\\b\\|Message\\|Mail\\|Group\\|Score\\|Summary\\|Article"
     . "Mail/News")
    ("\\<C\\>" . "C")
    ("ObjC" . "C")
    ("Text" . "Text")
    ("Outline" . "Text")
    ("\\(HT\\|SG\\|X\\|XHT\\)ML" . "SGML")
    ("log\\|diff\\|vc\\|cvs\\|Annotate" . "Version Control") ; "Change Management"?
    ("Threads\\|Memory\\|Disassembly\\|Breakpoints\\|Frames\\|Locals\\|Registers\\|Inferior I/O\\|Debugger"
     . "GDB")
    ("Lisp" . "Lisp")))
  "How to group various major modes together in \\[mouse-buffer-menu].
Each element has the form (REGEXP . GROUPNAME).
If the major mode's name string matches REGEXP, use GROUPNAME instead.")

(defun mouse-buffer-menu (event)
  "Pop up a menu of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((buffers (buffer-list))  alist menu split-by-major-mode sum-of-squares)
    ;; Make an alist of elements that look like (MENU-ITEM . BUFFER).
    (dolist (buf buffers)
      ;; Divide all buffers into buckets for various major modes.
      ;; Each bucket looks like (MODE NAMESTRING BUFFERS...).
      (with-current-buffer buf
        (let* ((adjusted-major-mode major-mode) elt)
          (dolist (group mouse-buffer-menu-mode-groups)
            (when (string-match (car group) (format-mode-line mode-name))
              (setq adjusted-major-mode (cdr group))))
          (setq elt (assoc adjusted-major-mode split-by-major-mode))
          (unless elt
            (setq elt (list adjusted-major-mode
                            (if (stringp adjusted-major-mode)
                                adjusted-major-mode
                              (format-mode-line mode-name nil nil buf)))
                  split-by-major-mode (cons elt split-by-major-mode)))
          (or (memq buf (cdr (cdr elt)))
              (setcdr (cdr elt) (cons buf (cdr (cdr elt))))))))
    ;; Compute the sum of squares of sizes of the major-mode buckets.
    (let ((tail split-by-major-mode))
      (setq sum-of-squares 0)
      (while tail
	(setq sum-of-squares
	      (+ sum-of-squares
		 (let ((len (length (cdr (cdr (car tail)))))) (* len len))))
	(setq tail (cdr tail))))
    (if (< (* sum-of-squares mouse-buffer-menu-mode-mult)
	   (* (length buffers) (length buffers)))
	;; Subdividing by major modes really helps, so let's do it.
	(let (subdivided-menus (buffers-left (length buffers)))
	  ;; Sort the list to put the most popular major modes first.
	  (setq split-by-major-mode
		(sort split-by-major-mode
		      (function (lambda (elt1 elt2)
				  (> (length elt1) (length elt2))))))
	  ;; Make a separate submenu for each major mode
	  ;; that has more than one buffer,
	  ;; unless all the remaining buffers are less than 1/10 of them.
	  (while (and split-by-major-mode
		      (and (> (length (car split-by-major-mode)) 3)
			   (> (* buffers-left 10) (length buffers))))
	    (let ((this-mode-list (mouse-buffer-menu-alist
				   (cdr (cdr (car split-by-major-mode))))))
	      (and this-mode-list
		   (setq subdivided-menus
			 (cons (cons
				(nth 1 (car split-by-major-mode))
				this-mode-list)
			       subdivided-menus))))
	    (setq buffers-left
		  (- buffers-left (length (cdr (car split-by-major-mode)))))
	    (setq split-by-major-mode (cdr split-by-major-mode)))
	  ;; If any major modes are left over,
	  ;; make a single submenu for them.
	  (if split-by-major-mode
	      (let ((others-list
		     (mouse-buffer-menu-alist
		      ;; we don't need split-by-major-mode any more,
		      ;; so we can ditch it with nconc.
		      (apply 'nconc (mapcar 'cddr split-by-major-mode)))))
		(and others-list
		     (setq subdivided-menus
			   (cons (cons "Others" others-list)
				 subdivided-menus)))))
	  (setq menu (cons "Buffer Menu" (nreverse subdivided-menus))))
      (progn
	(setq alist (mouse-buffer-menu-alist buffers))
	(setq menu (cons "Buffer Menu"
			 (mouse-buffer-menu-split "Select Buffer" alist)))))
    (let ((buf (x-popup-menu event menu))
	  (window (posn-window (event-start event))))
      (when buf
	(select-window
	 (if (framep window) (frame-selected-window window)
	   window))
	(switch-to-buffer buf)))))

(defun mouse-buffer-menu-alist (buffers)
  (let (tail
	(maxlen 0)
	head)
    (setq buffers
	  (sort buffers
		(function (lambda (elt1 elt2)
			    (string< (buffer-name elt1) (buffer-name elt2))))))
    (setq tail buffers)
    (while tail
      (or (eq ?\s (aref (buffer-name (car tail)) 0))
	  (setq maxlen
		(max maxlen
		     (length (buffer-name (car tail))))))
      (setq tail (cdr tail)))
    (setq tail buffers)
    (while tail
      (let ((elt (car tail)))
	(if (/= (aref (buffer-name elt) 0) ?\s)
	    (setq head
		  (cons
		   (cons
		    (format
		     (format "%%-%ds  %%s%%s  %%s" maxlen)
		     (buffer-name elt)
		     (if (buffer-modified-p elt) "*" " ")
		     (with-current-buffer elt
		       (if buffer-read-only "%" " "))
		     (or (buffer-file-name elt)
			 (with-current-buffer elt
			   (if list-buffers-directory
			       (expand-file-name
				list-buffers-directory)))
			 ""))
		    elt)
		   head))))
      (setq tail (cdr tail)))
    ;; Compensate for the reversal that the above loop does.
    (nreverse head)))

(defun mouse-buffer-menu-split (title alist)
  ;; If we have lots of buffers, divide them into groups of 20
  ;; and make a pane (or submenu) for each one.
  (if (> (length alist) (/ (* mouse-buffer-menu-maxlen 3) 2))
      (let ((alist alist) sublists next
	    (i 1))
	(while alist
	  ;; Pull off the next mouse-buffer-menu-maxlen buffers
	  ;; and make them the next element of sublist.
	  (setq next (nthcdr mouse-buffer-menu-maxlen alist))
	  (if next
	      (setcdr (nthcdr (1- mouse-buffer-menu-maxlen) alist)
		      nil))
	  (setq sublists (cons (cons (format "Buffers %d" i) alist)
			       sublists))
	  (setq i (1+ i))
	  (setq alist next))
	(nreverse sublists))
    ;; Few buffers--put them all in one pane.
    (list (cons title alist))))

(define-obsolete-function-alias
  'mouse-choose-completion 'choose-completion "23.2")

;; Font selection.

(defun font-menu-add-default ()
  (let* ((default (cdr (assq 'font (frame-parameters (selected-frame)))))
	 (font-alist x-fixed-font-alist)
	 (elt (or (assoc "Misc" font-alist) (nth 1 font-alist))))
    (if (assoc "Default" elt)
	(delete (assoc "Default" elt) elt))
    (setcdr elt
	    (cons (list "Default" default)
		  (cdr elt)))))

(defvar x-fixed-font-alist
  (list
   (purecopy "Font Menu")
   (cons
    (purecopy "Misc")
    (mapcar
     (lambda (arg) (cons  (purecopy (car arg)) (purecopy (cdr arg))))
     ;; For these, we specify the pixel height and width.
    '(("fixed" "fixed")
     ("6x10" "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" "6x10")
     ("6x12"
      "-misc-fixed-medium-r-semicondensed--12-*-*-*-c-60-iso8859-1" "6x12")
     ("6x13"
      "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-60-iso8859-1" "6x13")
     ("7x13" "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" "7x13")
     ("7x14" "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" "7x14")
     ("8x13" "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" "8x13")
     ("9x15" "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" "9x15")
     ("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" "10x20")
     ("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" "11x18")
     ("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" "12x24")
     ("")
     ("clean 5x8"
      "-schumacher-clean-medium-r-normal--8-*-*-*-c-50-iso8859-1")
     ("clean 6x8"
      "-schumacher-clean-medium-r-normal--8-*-*-*-c-60-iso8859-1")
     ("clean 8x8"
      "-schumacher-clean-medium-r-normal--8-*-*-*-c-80-iso8859-1")
     ("clean 8x10"
      "-schumacher-clean-medium-r-normal--10-*-*-*-c-80-iso8859-1")
     ("clean 8x14"
      "-schumacher-clean-medium-r-normal--14-*-*-*-c-80-iso8859-1")
     ("clean 8x16"
      "-schumacher-clean-medium-r-normal--16-*-*-*-c-80-iso8859-1")
     ("")
     ("sony 8x16" "-sony-fixed-medium-r-normal--16-*-*-*-c-80-iso8859-1")
     ;; We don't seem to have these; who knows what they are.
     ;; ("fg-18" "fg-18")
     ;; ("fg-25" "fg-25")
     ("lucidasanstypewriter-12" "-b&h-lucidatypewriter-medium-r-normal-sans-*-120-*-*-*-*-iso8859-1")
     ("lucidasanstypewriter-bold-14" "-b&h-lucidatypewriter-bold-r-normal-sans-*-140-*-*-*-*-iso8859-1")
     ("lucidasanstypewriter-bold-24"
      "-b&h-lucidatypewriter-bold-r-normal-sans-*-240-*-*-*-*-iso8859-1")
     ;; ("lucidatypewriter-bold-r-24" "-b&h-lucidatypewriter-bold-r-normal-sans-24-240-75-75-m-140-iso8859-1")
     ;; ("fixed-medium-20" "-misc-fixed-medium-*-*-*-20-*-*-*-*-*-*-*")
     )))

   (cons
    (purecopy "Courier")
    (mapcar
     (lambda (arg) (cons  (purecopy (car arg)) (purecopy (cdr arg))))
     ;; For these, we specify the point height.
     '(("8" "-adobe-courier-medium-r-normal--*-80-*-*-m-*-iso8859-1")
     ("10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-iso8859-1")
     ("12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-iso8859-1")
     ("14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-iso8859-1")
     ("18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1")
     ("24" "-adobe-courier-medium-r-normal--*-240-*-*-m-*-iso8859-1")
     ("8 bold" "-adobe-courier-bold-r-normal--*-80-*-*-m-*-iso8859-1")
     ("10 bold" "-adobe-courier-bold-r-normal--*-100-*-*-m-*-iso8859-1")
     ("12 bold" "-adobe-courier-bold-r-normal--*-120-*-*-m-*-iso8859-1")
     ("14 bold" "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1")
     ("18 bold" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-iso8859-1")
     ("24 bold" "-adobe-courier-bold-r-normal--*-240-*-*-m-*-iso8859-1")
     ("8 slant" "-adobe-courier-medium-o-normal--*-80-*-*-m-*-iso8859-1")
     ("10 slant" "-adobe-courier-medium-o-normal--*-100-*-*-m-*-iso8859-1")
     ("12 slant" "-adobe-courier-medium-o-normal--*-120-*-*-m-*-iso8859-1")
     ("14 slant" "-adobe-courier-medium-o-normal--*-140-*-*-m-*-iso8859-1")
     ("18 slant" "-adobe-courier-medium-o-normal--*-180-*-*-m-*-iso8859-1")
     ("24 slant" "-adobe-courier-medium-o-normal--*-240-*-*-m-*-iso8859-1")
     ("8 bold slant" "-adobe-courier-bold-o-normal--*-80-*-*-m-*-iso8859-1")
     ("10 bold slant" "-adobe-courier-bold-o-normal--*-100-*-*-m-*-iso8859-1")
     ("12 bold slant" "-adobe-courier-bold-o-normal--*-120-*-*-m-*-iso8859-1")
     ("14 bold slant" "-adobe-courier-bold-o-normal--*-140-*-*-m-*-iso8859-1")
     ("18 bold slant" "-adobe-courier-bold-o-normal--*-180-*-*-m-*-iso8859-1")
     ("24 bold slant" "-adobe-courier-bold-o-normal--*-240-*-*-m-*-iso8859-1")
    ))))
  "X fonts suitable for use in Emacs.")

(declare-function generate-fontset-menu "fontset" ())

(defun mouse-select-font ()
  "Prompt for a font name, using `x-popup-menu', and return it."
  (interactive)
  (unless (display-multi-font-p)
    (error "Cannot change fonts on this display"))
  (car
   (x-popup-menu
    (if (listp last-nonmenu-event)
	last-nonmenu-event
      (list '(0 0) (selected-window)))
    (append x-fixed-font-alist
	    (list (generate-fontset-menu))))))

(declare-function text-scale-mode "face-remap")

(defun mouse-set-font (&rest fonts)
  "Set the default font for the selected frame.
The argument FONTS is a list of font names; the first valid font
in this list is used.

When called interactively, pop up a menu and allow the user to
choose a font."
  (interactive
   (progn (unless (display-multi-font-p)
	    (error "Cannot change fonts on this display"))
	  (x-popup-menu
	   (if (listp last-nonmenu-event)
	       last-nonmenu-event
	     (list '(0 0) (selected-window)))
	   ;; Append list of fontsets currently defined.
	   (append x-fixed-font-alist (list (generate-fontset-menu))))))
  (if fonts
      (let (font)
	(while fonts
	  (condition-case nil
	      (progn
		(set-frame-font (car fonts))
		(setq font (car fonts))
		(setq fonts nil))
	    (error
	     (setq fonts (cdr fonts)))))
	(if (null font)
	    (error "Font not found")))))

(defvar mouse-appearance-menu-map nil)
(declare-function x-select-font "xfns.c" (&optional frame ignored)) ; USE_GTK
(declare-function buffer-face-mode-invoke "face-remap"
                  (face arg &optional interactive))
(declare-function font-face-attributes "font.c" (font &optional frame))

(defun mouse-appearance-menu (event)
  "Show a menu for changing the default face in the current buffer."
  (interactive "@e")
  (require 'face-remap)
  (when (display-multi-font-p)
    (with-selected-window (car (event-start event))
      (if mouse-appearance-menu-map
	  nil ; regenerate new fonts
	;; Initialize mouse-appearance-menu-map
	(setq mouse-appearance-menu-map
	      (make-sparse-keymap "Change Default Buffer Face"))
	(define-key mouse-appearance-menu-map [face-remap-reset-base]
	  '(menu-item "Reset to Default" face-remap-reset-base))
	(define-key mouse-appearance-menu-map [text-scale-decrease]
	  '(menu-item "Decrease Buffer Text Size" text-scale-decrease))
	(define-key mouse-appearance-menu-map [text-scale-increase]
	  '(menu-item "Increase Buffer Text Size" text-scale-increase))
	;; Font selector
	(if (functionp 'x-select-font)
	    (define-key mouse-appearance-menu-map [x-select-font]
	      '(menu-item "Change Buffer Font..." x-select-font))
	  ;; If the select-font is unavailable, construct a menu.
	  (let ((font-submenu (make-sparse-keymap "Change Text Font"))
		(font-alist (cdr (append x-fixed-font-alist
					 (list (generate-fontset-menu))))))
	    (dolist (family font-alist)
	      (let* ((submenu-name (car family))
		     (submenu-map (make-sparse-keymap submenu-name)))
		(dolist (font (cdr family))
		  (let ((font-name (car font))
			font-symbol)
		    (if (string= font-name "")
			(define-key submenu-map [space]
			  '("--"))
		      (setq font-symbol (intern (cadr font)))
		      (define-key submenu-map (vector font-symbol)
			(list 'menu-item (car font) font-symbol)))))
		(define-key font-submenu (vector (intern submenu-name))
		  (list 'menu-item submenu-name submenu-map))))
	    (define-key mouse-appearance-menu-map [font-submenu]
	      (list 'menu-item "Change Text Font" font-submenu)))))
      (let ((choice (x-popup-menu event mouse-appearance-menu-map)))
	(setq choice (nth (1- (length choice)) choice))
	(cond ((eq choice 'text-scale-increase)
	       (text-scale-increase 1))
	      ((eq choice 'text-scale-decrease)
	       (text-scale-increase -1))
	      ((eq choice 'face-remap-reset-base)
	       (text-scale-mode 0)
	       (buffer-face-mode 0))
	      (choice
	       ;; Either choice == 'x-select-font, or choice is a
	       ;; symbol whose name is a font.
	       (buffer-face-mode-invoke (font-face-attributes
					 (if (eq choice 'x-select-font)
					     (x-select-font)
					   (symbol-name choice)))
					t
					(called-interactively-p 'interactive))))))))


;;; Bindings for mouse commands.

(define-key global-map [down-mouse-1] 'mouse-drag-region)
(global-set-key [mouse-1]	'mouse-set-point)
(global-set-key [drag-mouse-1]	'mouse-set-region)

;; These are tested for in mouse-drag-region.
(global-set-key [double-mouse-1] 'mouse-set-point)
(global-set-key [triple-mouse-1] 'mouse-set-point)

(defun mouse--strip-first-event (_prompt)
  (substring (this-single-command-raw-keys) 1))

(define-key function-key-map [left-fringe mouse-1] 'mouse--strip-first-event)
(define-key function-key-map [right-fringe mouse-1] 'mouse--strip-first-event)

(global-set-key [mouse-2]	'mouse-yank-primary)
;; Allow yanking also when the corresponding cursor is "in the fringe".
(define-key function-key-map [right-fringe mouse-2] 'mouse--strip-first-event)
(define-key function-key-map [left-fringe mouse-2] 'mouse--strip-first-event)
(global-set-key [mouse-3]	'mouse-save-then-kill)
(define-key function-key-map [right-fringe mouse-3] 'mouse--strip-first-event)
(define-key function-key-map [left-fringe mouse-3] 'mouse--strip-first-event)

;; By binding these to down-going events, we let the user use the up-going
;; event to make the selection, saving a click.
(global-set-key [C-down-mouse-1] 'mouse-buffer-menu)
(if (not (eq system-type 'ms-dos))
    (global-set-key [S-down-mouse-1] 'mouse-appearance-menu))
;; C-down-mouse-2 is bound in facemenu.el.
(global-set-key [C-down-mouse-3]
  `(menu-item ,(purecopy "Menu Bar") ignore
    :filter (lambda (_)
              (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
                  (mouse-menu-bar-map)
                (mouse-menu-major-mode-map)))))

;; Binding mouse-1 to mouse-select-window when on mode-, header-, or
;; vertical-line prevents Emacs from signaling an error when the mouse
;; button is released after dragging these lines, on non-toolkit
;; versions.
(global-set-key [mode-line mouse-1] 'mouse-select-window)
(global-set-key [mode-line drag-mouse-1] 'mouse-select-window)
(global-set-key [mode-line down-mouse-1] 'mouse-drag-mode-line)
(global-set-key [header-line down-mouse-1] 'mouse-drag-header-line)
(global-set-key [header-line mouse-1] 'mouse-select-window)
(global-set-key [mode-line mouse-2] 'mouse-delete-other-windows)
(global-set-key [mode-line mouse-3] 'mouse-delete-window)
(global-set-key [mode-line C-mouse-2] 'mouse-split-window-horizontally)
(global-set-key [vertical-scroll-bar C-mouse-2] 'mouse-split-window-vertically)
(global-set-key [vertical-line C-mouse-2] 'mouse-split-window-vertically)
(global-set-key [vertical-line down-mouse-1] 'mouse-drag-vertical-line)
(global-set-key [vertical-line mouse-1] 'mouse-select-window)

(provide 'mouse)

;;; mouse.el ends here
