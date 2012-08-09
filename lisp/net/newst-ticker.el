;; newst-ticker.el --- modeline ticker for newsticker.

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newst-ticker.el
;; URL:         http://www.nongnu.org/newsticker
;; Keywords:    News, RSS, Atom
;; Time-stamp:  "6. Dezember 2009, 19:16:00 (ulf)"
;; Package:     newsticker

;; ======================================================================

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

;; ======================================================================

;;; Commentary:

;; See newsticker.el

;; ======================================================================
;;; Code:

(require 'newst-backend)

(defvar newsticker--ticker-timer nil
  "Timer for newsticker ticker.")

;;;###autoload
(defun newsticker-ticker-running-p ()
  "Check whether newsticker's actual ticker is running.
Return t if ticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not
empty."
  (timerp newsticker--ticker-timer))

;; customization group ticker
(defgroup newsticker-ticker nil
  "Settings for the headline ticker."
  :group 'newsticker)

(defun newsticker--set-customvar-ticker (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed -- restart ticker
    (when (newsticker-running-p)
      (message "Restarting ticker")
      (newsticker-stop-ticker)
      (newsticker--ticker-text-setup)
      (newsticker-start-ticker)
      (message ""))))

(defcustom newsticker-ticker-interval
  0.3
  "Time interval for displaying news items in the echo area (seconds).
If equal or less than 0 no messages are shown in the echo area.  For
smooth display (see `newsticker-scroll-smoothly') a value of 0.3 seems
reasonable.  For non-smooth display a value of 10 is a good starting
point."
  :type 'number
  :set 'newsticker--set-customvar-ticker
  :group 'newsticker-ticker)

(defcustom newsticker-scroll-smoothly
  t
  "Decides whether to flash or scroll news items.
If t the news headlines are scrolled (more-or-less) smoothly in the echo
area.  If nil one headline after another is displayed in the echo area.
The variable `newsticker-ticker-interval' determines how fast this
display moves/changes and whether headlines are shown in the echo area
at all.  If you change `newsticker-scroll-smoothly' you should also change
`newsticker-ticker-interval'."
  :type 'boolean
  :group 'newsticker-ticker)

(defcustom newsticker-hide-immortal-items-in-echo-area
  t
  "Decides whether to show immortal/non-expiring news items in the ticker.
If t the echo area will not show immortal items.  See also
`newsticker-hide-old-items-in-echo-area'."
  :type 'boolean
  :set 'newsticker--set-customvar-ticker
  :group 'newsticker-ticker)

(defcustom newsticker-hide-old-items-in-echo-area
  t
  "Decides whether to show only the newest news items in the ticker.
If t the echo area will show only new items, i.e. only items which have
been added between the last two retrievals."
  :type 'boolean
  :set 'newsticker--set-customvar-ticker
  :group 'newsticker-ticker)

(defcustom newsticker-hide-obsolete-items-in-echo-area
  t
  "Decides whether to show obsolete items items in the ticker.
If t the echo area will not show obsolete items.  See also
`newsticker-hide-old-items-in-echo-area'."
  :type 'boolean
  :set 'newsticker--set-customvar-ticker
  :group 'newsticker-ticker)

(defun newsticker--display-tick ()
  "Called from the display timer.
This function calls a display function, according to the variable
`newsticker-scroll-smoothly'."
  (if newsticker-scroll-smoothly
      (newsticker--display-scroll)
    (newsticker--display-jump)))

(defsubst newsticker--echo-area-clean-p ()
  "Check whether somebody is using the echo area / minibuffer.
Return t if echo area and minibuffer are unused."
  (not (or (active-minibuffer-window)
           (and (current-message)
                (not (string= (current-message)
                              newsticker--prev-message))))))

(defun newsticker--display-jump ()
  "Called from the display timer.
This function displays the next ticker item in the echo area, unless
there is another message displayed or the minibuffer is active."
  (let ((message-log-max nil));; prevents message text from being logged
    (when (newsticker--echo-area-clean-p)
      (setq newsticker--item-position (1+ newsticker--item-position))
      (when (>= newsticker--item-position (length newsticker--item-list))
        (setq newsticker--item-position 0))
      (setq newsticker--prev-message
            (nth newsticker--item-position newsticker--item-list))
      (message "%s" newsticker--prev-message))))

(defun newsticker--display-scroll ()
  "Called from the display timer.
This function scrolls the ticker items in the echo area, unless
there is another message displayed or the minibuffer is active."
  (when (newsticker--echo-area-clean-p)
    (let* ((width (- (frame-width) 1))
           (message-log-max nil);; prevents message text from being logged
           (i newsticker--item-position)
           subtext
           (s-text newsticker--scrollable-text)
           (l (length s-text)))
      ;; don't show anything if there is nothing to show
      (unless (< (length s-text) 1)
        ;; repeat the ticker string if it is shorter than frame width
        (while (< (length s-text) width)
          (setq s-text (concat s-text s-text)))
        ;; get the width of the printed string
        (setq l (length s-text))
        (cond ((< i (- l width))
               (setq subtext (substring s-text i (+ i width))))
              (t
               (setq subtext (concat
                              (substring s-text i l)
                              (substring s-text 0 (- width (- l i)))))))
        ;; Take care of multibyte strings, for which (string-width) is
        ;; larger than (length).
        ;; Actually, such strings may be smaller than (frame-width)
        ;; because return values of (string-width) are too large:
        ;; (string-width "<japanese character>") => 2
        (let ((t-width (1- (length subtext))))
          (while (> (string-width subtext) width)
            (setq subtext (substring subtext 0 t-width))
            (setq t-width (1- t-width))))
        ;; show the ticker text and save current position
        (message "%s" subtext)
        (setq newsticker--prev-message subtext)
        (setq newsticker--item-position (1+ i))
        (when (>= newsticker--item-position l)
          (setq newsticker--item-position 0))))))

;;;###autoload
(defun newsticker-start-ticker ()
  "Start newsticker's ticker (but not the news retrieval).
Start display timer for the actual ticker if wanted and not
running already."
  (interactive)
  (if (and (> newsticker-ticker-interval 0)
           (not newsticker--ticker-timer))
      (setq newsticker--ticker-timer
            (run-at-time newsticker-ticker-interval
                         newsticker-ticker-interval
                         'newsticker--display-tick))))

(defun newsticker-stop-ticker ()
  "Stop newsticker's ticker (but not the news retrieval)."
  (interactive)
  (when newsticker--ticker-timer
    (cancel-timer newsticker--ticker-timer)
    (setq newsticker--ticker-timer nil)))

;; ======================================================================
;;; Manipulation of ticker text
;; ======================================================================
(defun newsticker--ticker-text-setup ()
  "Build the ticker text which is scrolled or flashed in the echo area."
  ;; reset scrollable text
  (setq newsticker--scrollable-text "")
  (setq newsticker--item-list nil)
  (setq newsticker--item-position 0)
  ;; build scrollable text from cache data
  (let ((have-something nil))
    (mapc
     (lambda (feed)
       (let ((feed-name (symbol-name (car feed))))
         (let ((num-new (newsticker--stat-num-items (car feed) 'new))
               (num-old (newsticker--stat-num-items (car feed) 'old))
               (num-imm (newsticker--stat-num-items (car feed) 'immortal))
               (num-obs (newsticker--stat-num-items (car feed) 'obsolete)))
           (when (or (> num-new 0)
                     (and (> num-old 0)
                          (not newsticker-hide-old-items-in-echo-area))
                     (and (> num-imm 0)
                          (not newsticker-hide-immortal-items-in-echo-area))
                     (and (> num-obs 0)
                          (not newsticker-hide-obsolete-items-in-echo-area)))
             (setq have-something t)
             (mapc
              (lambda (item)
                (let ((title (replace-regexp-in-string
                              "[\r\n]+" " "
                              (newsticker--title item)))
                      (age (newsticker--age item)))
                  (unless (string= title newsticker--error-headline)
                    (when
                        (or (eq age 'new)
                            (and (eq age 'old)
                                 (not newsticker-hide-old-items-in-echo-area))
                            (and (eq age 'obsolete)
                                 (not
                                  newsticker-hide-obsolete-items-in-echo-area))
                            (and (eq age 'immortal)
                                 (not
                                  newsticker-hide-immortal-items-in-echo-area)))
                      (setq title (newsticker--remove-whitespace title))
                      ;; add to flash list
                      (add-to-list 'newsticker--item-list
                                   (concat feed-name ": " title) t)
                      ;; and to the scrollable text
                      (setq newsticker--scrollable-text
                            (concat newsticker--scrollable-text
                                    " " feed-name ": " title " +++"))))))
                (cdr feed))))))
     newsticker--cache)
    (when have-something
      (setq newsticker--scrollable-text
            (concat "+++ "
                    (format-time-string "%A, %H:%M"
                                        newsticker--latest-update-time)
                    " ++++++" newsticker--scrollable-text)))))

(defun newsticker--ticker-text-remove (feed title)
  "Remove the item of FEED with TITLE from the ticker text."
  ;; reset scrollable text
  (setq newsticker--item-position 0)
  (let ((feed-name (symbol-name feed))
        (t-title (replace-regexp-in-string "[\r\n]+" " " title)))
    ;; remove from flash list
    (setq newsticker--item-list (remove (concat feed-name ": " t-title)
                                        newsticker--item-list))
    ;; and from the scrollable text
    (setq newsticker--scrollable-text
          (replace-regexp-in-string
           (regexp-quote (concat " " feed-name ": " t-title " +++"))
           ""
           newsticker--scrollable-text))
    (if (string-match (concat "^\\+\\+\\+ [A-Z][a-z]+, "
                              "[012]?[0-9]:[0-9][0-9] \\+\\+\\+\\+\\+\\+$")
                              newsticker--scrollable-text)
        (setq newsticker--scrollable-text ""))))

(provide 'newst-ticker)

;;; newst-ticker.el ends here
