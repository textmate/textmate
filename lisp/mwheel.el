;;; mwheel.el --- Wheel mouse support

;; Copyright (C) 1998, 2000-2012  Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: mouse
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

;; This code will enable the use of the infamous 'wheel' on the new
;; crop of mice.  Under XFree86 and the XSuSE X Servers, the wheel
;; events are sent as button4/button5 events.

;; I for one would prefer some way of converting the button4/button5
;; events into different event types, like 'mwheel-up' or
;; 'mwheel-down', but I cannot find a way to do this very easily (or
;; portably), so for now I just live with it.

;; To enable this code, simply put this at the top of your .emacs
;; file:
;;
;; (mouse-wheel-mode 1)

;;; Code:

(require 'custom)
(require 'timer)

(defvar mouse-wheel-mode)

;; Setter function for mouse-button user-options.  Switch Mouse Wheel
;; mode off and on again so that the old button is unbound and
;; new button is bound to mwheel-scroll.

(defun mouse-wheel-change-button (var button)
  (set-default var button)
  ;; Sync the bindings.
  (when (bound-and-true-p mouse-wheel-mode) (mouse-wheel-mode 1)))

(defvar mouse-wheel-down-button 4)
(make-obsolete-variable 'mouse-wheel-down-button
                        'mouse-wheel-down-event
			"22.1")
(defcustom mouse-wheel-down-event
  (if (or (featurep 'w32-win) (featurep 'ns-win))
      'wheel-up
    (intern (format "mouse-%s" mouse-wheel-down-button)))
  "Event used for scrolling down."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defvar mouse-wheel-up-button 5)
(make-obsolete-variable 'mouse-wheel-up-button
                        'mouse-wheel-up-event
			"22.1")
(defcustom mouse-wheel-up-event
  (if (or (featurep 'w32-win) (featurep 'ns-win))
      'wheel-down
    (intern (format "mouse-%s" mouse-wheel-up-button)))
  "Event used for scrolling up."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defvar mouse-wheel-click-button 2)
(make-obsolete-variable 'mouse-wheel-click-button
                        'mouse-wheel-click-event
			"22.1")
(defcustom mouse-wheel-click-event
  (intern (format "mouse-%s" mouse-wheel-click-button))
  "Event that should be temporarily inhibited after mouse scrolling.
The mouse wheel is typically on the mouse-2 button, so it may easily
happen that text is accidentally yanked into the buffer when
scrolling with the mouse wheel.  To prevent that, this variable can be
set to the event sent when clicking on the mouse wheel button."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-inhibit-click-time 0.35
  "Time in seconds to inhibit clicking on mouse wheel button after scroll."
  :group 'mouse
  :type 'number)

(defcustom mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control) . nil))
  "Amount to scroll windows by when spinning the mouse wheel.
This is an alist mapping the modifier key to the amount to scroll when
the wheel is moved with the modifier key depressed.
Elements of the list have the form (MODIFIERS . AMOUNT) or just AMOUNT if
MODIFIERS is nil.

AMOUNT should be the number of lines to scroll, or nil for near full
screen.  It can also be a floating point number, specifying the fraction of
a full screen to scroll.  A near full screen is `next-screen-context-lines'
less than a full screen."
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines")
		  (float :tag "Fraction of window")
		  (cons
		   (repeat (choice :tag "modifier"
				   (const alt) (const control) (const hyper)
				   (const meta) (const shift) (const super)))
		   (choice :tag "scroll amount"
			   (const :tag "Full screen" :value nil)
			   (integer :tag "Specific # of lines")
			   (float :tag "Fraction of window"))))
          (repeat
           (cons
            (repeat (choice :tag "modifier"
			    (const alt) (const control) (const hyper)
                            (const meta) (const shift) (const super)))
            (choice :tag "scroll amount"
                    (const :tag "Full screen" :value nil)
                    (integer :tag "Specific # of lines")
                    (float :tag "Fraction of window")))))
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-progressive-speed t
  "If non-nil, the faster the user moves the wheel, the faster the scrolling.
Note that this has no effect when `mouse-wheel-scroll-amount' specifies
a \"near full screen\" scroll or when the mouse wheel sends key instead
of button events."
  :group 'mouse
  :type 'boolean)

(defcustom mouse-wheel-follow-mouse t
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people prefer it."
  :group 'mouse
  :type 'boolean)

(eval-and-compile
  (if (fboundp 'event-button)
      (fset 'mwheel-event-button 'event-button)
    (defun mwheel-event-button (event)
      (let ((x (event-basic-type event)))
	;; Map mouse-wheel events to appropriate buttons
	(if (eq 'mouse-wheel x)
	    (let ((amount (car (cdr (cdr (cdr event))))))
	      (if (< amount 0)
		  mouse-wheel-up-event
		mouse-wheel-down-event))
	  x))))

  (if (fboundp 'event-window)
      (fset 'mwheel-event-window 'event-window)
    (defun mwheel-event-window (event)
      (posn-window (event-start event)))))

(defvar mwheel-inhibit-click-event-timer nil
  "Timer running while mouse wheel click event is inhibited.")

(defun mwheel-inhibit-click-timeout ()
  "Handler for `mwheel-inhibit-click-event-timer'."
  (setq mwheel-inhibit-click-event-timer nil)
  (remove-hook 'pre-command-hook 'mwheel-filter-click-events))

(defun mwheel-filter-click-events ()
  "Discard `mouse-wheel-click-event' while scrolling the mouse."
  (if (eq (event-basic-type last-input-event) mouse-wheel-click-event)
      (setq this-command 'ignore)))

(defvar mwheel-scroll-up-function 'scroll-up
  "Function that does the job of scrolling upward.")

(defvar mwheel-scroll-down-function 'scroll-down
  "Function that does the job of scrolling downward.")

(defun mwheel-scroll (event)
  "Scroll up or down according to the EVENT.
This should only be bound to mouse buttons 4 and 5."
  (interactive (list last-input-event))
  (let* ((curwin (if mouse-wheel-follow-mouse
                     (prog1
                         (selected-window)
                       (select-window (mwheel-event-window event)))))
	 (buffer (window-buffer curwin))
	 (opoint (with-current-buffer buffer
		   (when (eq (car-safe transient-mark-mode) 'only)
		     (point))))
         (mods
	  (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
         (amt (assoc mods mouse-wheel-scroll-amount)))
    ;; Extract the actual amount or find the element that has no modifiers.
    (if amt (setq amt (cdr amt))
      (let ((list-elt mouse-wheel-scroll-amount))
	(while (consp (setq amt (pop list-elt))))))
    (if (floatp amt) (setq amt (1+ (truncate (* amt (window-height))))))
    (when (and mouse-wheel-progressive-speed (numberp amt))
      ;; When the double-mouse-N comes in, a mouse-N has been executed already,
      ;; So by adding things up we get a squaring up (1, 3, 6, 10, 15, ...).
      (setq amt (* amt (event-click-count event))))
    (unwind-protect
	(let ((button (mwheel-event-button event)))
	  (cond ((eq button mouse-wheel-down-event)
                 (condition-case nil (funcall mwheel-scroll-down-function amt)
                   ;; Make sure we do indeed scroll to the beginning of
                   ;; the buffer.
                   (beginning-of-buffer
                    (unwind-protect
                        (funcall mwheel-scroll-down-function)
                      ;; If the first scroll succeeded, then some scrolling
                      ;; is possible: keep scrolling til the beginning but
                      ;; do not signal an error.  For some reason, we have
                      ;; to do it even if the first scroll signaled an
                      ;; error, because otherwise the window is recentered
                      ;; for a reason that escapes me.  This problem seems
                      ;; to only affect scroll-down.  --Stef
                      (set-window-start (selected-window) (point-min))))))
		((eq button mouse-wheel-up-event)
                 (condition-case nil (funcall mwheel-scroll-up-function amt)
                   ;; Make sure we do indeed scroll to the end of the buffer.
                   (end-of-buffer (while t (funcall mwheel-scroll-up-function)))))
		(t (error "Bad binding in mwheel-scroll"))))
      (if curwin (select-window curwin)))
    ;; If there is a temporarily active region, deactivate it if
    ;; scrolling moves point.
    (when opoint
      (with-current-buffer buffer
	(when (/= opoint (point))
	  ;; Call `deactivate-mark' at the original position, so that
	  ;; the original region is saved to the X selection.
	  (let ((newpoint (point)))
	    (goto-char opoint)
	    (deactivate-mark)
	    (goto-char newpoint))))))
  (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
    (if mwheel-inhibit-click-event-timer
	(cancel-timer mwheel-inhibit-click-event-timer)
      (add-hook 'pre-command-hook 'mwheel-filter-click-events))
    (setq mwheel-inhibit-click-event-timer
	  (run-with-timer mouse-wheel-inhibit-click-time nil
			  'mwheel-inhibit-click-timeout))))

(put 'mwheel-scroll 'scroll-command t)

(defvar mwheel-installed-bindings nil)

(define-minor-mode mouse-wheel-mode
  "Toggle mouse wheel support (Mouse Wheel mode).
With a prefix argument ARG, enable Mouse Wheel mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :init-value t
  ;; We'd like to use custom-initialize-set here so the setup is done
  ;; before dumping, but at the point where the defcustom is evaluated,
  ;; the corresponding function isn't defined yet, so
  ;; custom-initialize-set signals an error.
  :initialize 'custom-initialize-delay
  :global t
  :group 'mouse
  ;; Remove previous bindings, if any.
  (while mwheel-installed-bindings
    (let ((key (pop mwheel-installed-bindings)))
      (when (eq (lookup-key (current-global-map) key) 'mwheel-scroll)
        (global-unset-key key))))
  ;; Setup bindings as needed.
  (when mouse-wheel-mode
    (dolist (event (list mouse-wheel-down-event mouse-wheel-up-event))
      (dolist (key (mapcar (lambda (amt) `[(,@(if (consp amt) (car amt)) ,event)])
                           mouse-wheel-scroll-amount))
        (global-set-key key 'mwheel-scroll)
        (push key mwheel-installed-bindings)))))

;;; Compatibility entry point
;; preloaded ;;;###autoload
(defun mwheel-install (&optional uninstall)
  "Enable mouse wheel support."
  (mouse-wheel-mode (if uninstall -1 1)))

(provide 'mwheel)

;;; mwheel.el ends here
