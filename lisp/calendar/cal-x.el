;;; cal-x.el --- calendar windows in dedicated frames

;; Copyright (C) 1994-1995, 2001-2012  Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;;         Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, dedicated frames
;; Package: calendar

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

;; See calendar.el.

;;; Code:

(require 'calendar)

(defcustom diary-frame-parameters
  '((name . "Diary") (title . "Diary") (height . 10) (width . 80)
    (unsplittable . t) (minibuffer . nil))
  "Parameters of the diary frame, if the diary is in its own frame.
Relevant if `calendar-setup' has the value `two-frames'."
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (unsplittable boolean) (minibuffer boolean)
             (vertical-scroll-bars boolean))
  :group 'calendar)

(defcustom calendar-frame-parameters
  '((name . "Calendar") (title . "Calendar") (height . 10) (width . 80)
    (unsplittable . t) (minibuffer . nil) (vertical-scroll-bars . nil))
  "Parameters of the calendar frame, if the calendar is in a separate frame.
Relevant if `calendar-setup' has the value `calendar-only' or `two-frames'."
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (unsplittable boolean) (minibuffer boolean)
             (vertical-scroll-bars boolean))
  :group 'calendar)

(defcustom calendar-and-diary-frame-parameters
  '((name . "Calendar") (title . "Calendar") (height . 28) (width . 80)
    (minibuffer . nil))
  "Parameters of the frame that displays both the calendar and the diary.
Relevant if `calendar-setup' has the value `one-frame'."
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (unsplittable boolean) (minibuffer boolean)
             (vertical-scroll-bars boolean))
  :group 'calendar)

(define-obsolete-variable-alias 'calendar-after-frame-setup-hooks
  'calendar-after-frame-setup-hook "23.1")

(defcustom calendar-after-frame-setup-hook nil
  "List of functions to be run after creating a calendar and/or diary frame."
  :type 'hook
  :group 'calendar-hooks)

;;; End of user options.

(defvar calendar-frame nil
  "Frame in which the calendar was last displayed.")

(defvar diary-frame nil
  "Frame in which the diary was last displayed.")

(defun calendar-frame-1 (frame)
  "Subroutine used by `calendar-frame-setup'.
Runs `calendar-after-frame-setup-hook', selects frame, iconifies if needed."
  (run-hooks 'calendar-after-frame-setup-hook)
  (select-frame frame)
  (if (eq 'icon (cdr (assoc 'visibility (frame-parameters frame))))
      (iconify-or-deiconify-frame)))

;; c-d-d is only called after (diary) has been run.
(defvar diary-display-function)

(defun calendar-dedicate-diary ()
  "Display and dedicate the window associated with the diary buffer."
  (set-window-dedicated-p
   (display-buffer
    (if (if (listp diary-display-function)
            (or (memq 'diary-fancy-display diary-display-function)
                (memq 'fancy-diary-display diary-display-function))
          (memq diary-display-function '(diary-fancy-display
                                         fancy-diary-display)))
        (progn
          ;; If there are no diary entries, there won't be a fancy-diary
          ;; to dedicate, so make a basic one.
          (or (get-buffer diary-fancy-buffer)
              (calendar-in-read-only-buffer diary-fancy-buffer
                (calendar-set-mode-line "Diary Entries")))
          diary-fancy-buffer)
      (get-file-buffer diary-file)))
   t))

;;;###cal-autoload
(defun calendar-frame-setup (config &optional prompt)
  "Display the calendar, and optionally the diary, in a separate frame.
CONFIG should be one of:
`calendar-only' - just the calendar, no diary
`one-frame'     - calendar and diary in a single frame
`two-frames'    - calendar and diary each in a separate frame

If CONFIG has any other value, or if the display is not capable of
multiple frames, then `calendar-basic-setup' is called.

If PROMPT is non-nil, prompt for the month and year to use."
  (if (not (and (display-multi-frame-p)
                (memq config '(calendar-only one-frame two-frames))))
      (calendar-basic-setup prompt)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    (unless (eq config 'calendar-only)
      (if (frame-live-p diary-frame) (delete-frame diary-frame)))
    (let ((calendar-view-diary-initially-flag (eq config 'one-frame))
          ;; For calendar-dedicate-diary in two-frames case.
          (pop-up-windows nil))
      (save-window-excursion
        ;; Do diary first so that calendar is always left current.
        (when (eq config 'two-frames)
          (calendar-frame-1
           (setq diary-frame (make-frame diary-frame-parameters)))
          (diary)
          (calendar-dedicate-diary))
        (calendar-frame-1
         (setq calendar-frame
               (make-frame (if (eq config 'one-frame)
                               calendar-and-diary-frame-parameters
                             calendar-frame-parameters))))
        (calendar-basic-setup prompt (not (eq config 'one-frame)))
        (set-window-buffer (selected-window) calendar-buffer)
        (set-window-dedicated-p (selected-window) t)
        (if (eq config 'one-frame)
            (calendar-dedicate-diary))))))


;;;###cal-autoload
(defun calendar-one-frame-setup (&optional prompt)
  "Display calendar and diary in a single dedicated frame.
See `calendar-frame-setup' for more information."
  (calendar-frame-setup 'one-frame prompt))

(make-obsolete 'calendar-one-frame-setup 'calendar-frame-setup "23.1")


;;;###cal-autoload
(defun calendar-only-one-frame-setup (&optional prompt)
  "Display calendar in a dedicated frame.
See `calendar-frame-setup' for more information."
  (calendar-frame-setup 'calendar-only prompt))

(make-obsolete 'calendar-only-one-frame-setup 'calendar-frame-setup "23.1")


;;;###cal-autoload
(defun calendar-two-frame-setup (&optional prompt)
  "Display calendar and diary in separate, dedicated frames.
See `calendar-frame-setup' for more information."
  (calendar-frame-setup 'two-frames prompt))

(make-obsolete 'calendar-two-frame-setup 'calendar-frame-setup "23.1")


;; Undocumented and probably useless.
(defvar cal-x-load-hook nil
  "Hook run on loading of the `cal-x' package.")
(make-obsolete-variable 'cal-x-load-hook "it will be removed in future." "23.1")

(run-hooks 'cal-x-load-hook)


(provide 'cal-x)

;;; cal-x.el ends here
