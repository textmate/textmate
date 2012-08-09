;;; ruler-mode.el --- display a ruler in the header line

;; Copyright (C) 2001-2012 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 24 Mar 2001
;; Version: 1.6
;; Keywords: convenience

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

;; This library provides a minor mode to display a ruler in the header
;; line.  It works from Emacs 21 onwards.
;;
;; You can use the mouse to change the `fill-column' `comment-column',
;; `goal-column', `window-margins' and `tab-stop-list' settings:
;;
;; [header-line (shift down-mouse-1)] set left margin end to the ruler
;; graduation where the mouse pointer is on.
;;
;; [header-line (shift down-mouse-3)] set right margin beginning to
;; the ruler graduation where the mouse pointer is on.
;;
;; [header-line down-mouse-2] Drag the `fill-column', `comment-column'
;; or `goal-column' to a ruler graduation.
;;
;; [header-line (control down-mouse-1)] add a tab stop to the ruler
;; graduation where the mouse pointer is on.
;;
;; [header-line (control down-mouse-3)] remove the tab stop at the
;; ruler graduation where the mouse pointer is on.
;;
;; [header-line (control down-mouse-2)] or M-x
;; `ruler-mode-toggle-show-tab-stops' toggle showing and visually
;; editing `tab-stop-list' setting.  The `ruler-mode-show-tab-stops'
;; option controls if the ruler shows tab stops by default.
;;
;; In the ruler the character `ruler-mode-current-column-char' shows
;; the `current-column' location, `ruler-mode-fill-column-char' shows
;; the `fill-column' location, `ruler-mode-comment-column-char' shows
;; the `comment-column' location, `ruler-mode-goal-column-char' shows
;; the `goal-column' and `ruler-mode-tab-stop-char' shows tab stop
;; locations.  Graduations in `window-margins' and `window-fringes'
;; areas are shown with a different foreground color.
;;
;; It is also possible to customize the following characters:
;;
;; - `ruler-mode-basic-graduation-char' character used for basic
;;   graduations ('.' by default).
;; - `ruler-mode-inter-graduation-char' character used for
;;   intermediate graduations ('!' by default).
;;
;; The following faces are customizable:
;;
;; - `ruler-mode-default' the ruler default face.
;; - `ruler-mode-fill-column' the face used to highlight the
;;   `fill-column' character.
;; - `ruler-mode-comment-column' the face used to highlight the
;;   `comment-column' character.
;; - `ruler-mode-goal-column' the face used to highlight the
;;   `goal-column' character.
;; - `ruler-mode-current-column' the face used to highlight the
;;   `current-column' character.
;; - `ruler-mode-tab-stop' the face used to highlight tab stop
;;   characters.
;; - `ruler-mode-margins' the face used to highlight graduations
;;   in the `window-margins' areas.
;; - `ruler-mode-fringes' the face used to highlight graduations
;;   in the `window-fringes' areas.
;; - `ruler-mode-column-number' the face used to highlight the
;;   numbered graduations.
;;
;; `ruler-mode-default' inherits from the built-in `default' face.
;; All `ruler-mode' faces inherit from `ruler-mode-default'.
;;
;; WARNING: To keep ruler graduations aligned on text columns it is
;; important to use the same font family and size for ruler and text
;; areas.
;;
;; You can override the ruler format by defining an appropriate
;; function as the buffer-local value of `ruler-mode-ruler-function'.

;; Installation
;;
;; To automatically display the ruler in specific major modes use:
;;
;;    (add-hook '<major-mode>-hook 'ruler-mode)
;;

;;; History:
;;

;;; Code:
(eval-when-compile
  (require 'wid-edit))
(require 'scroll-bar)
(require 'fringe)

(defgroup ruler-mode nil
  "Display a ruler in the header line."
  :version "22.1"
  :group 'convenience)

(defcustom ruler-mode-show-tab-stops nil
  "If non-nil the ruler shows tab stop positions.
Also allowing to visually change `tab-stop-list' setting using
<C-down-mouse-1> and <C-down-mouse-3> on the ruler to respectively add
or remove a tab stop.  \\[ruler-mode-toggle-show-tab-stops] or
<C-down-mouse-2> on the ruler toggles showing/editing of tab stops."
  :group 'ruler-mode
  :type 'boolean)

;; IMPORTANT: This function must be defined before the following
;; defcustoms because it is used in their :validate clause.
(defun ruler-mode-character-validate (widget)
  "Ensure WIDGET value is a valid character value."
  (save-excursion
    (let ((value (widget-value widget)))
      (unless (characterp value)
        (widget-put widget :error
                    (format "Invalid character value: %S" value))
        widget))))

(defcustom ruler-mode-fill-column-char (if (char-displayable-p ?¶)
                                           ?\¶
                                         ?\|)
  "Character used at the `fill-column' location."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-comment-column-char ?\#
  "Character used at the `comment-column' location."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-goal-column-char ?G
  "Character used at the `goal-column' location."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-current-column-char (if (char-displayable-p ?¦)
                                              ?\¦
                                            ?\@)
  "Character used at the `current-column' location."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-tab-stop-char ?\T
  "Character used at `tab-stop-list' locations."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-basic-graduation-char ?\.
  "Character used for basic graduations."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-inter-graduation-char ?\!
  "Character used for intermediate graduations."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-set-goal-column-ding-flag t
  "Non-nil means do `ding' when `goal-column' is set."
  :group 'ruler-mode
  :type 'boolean)

(defface ruler-mode-default
  '((((type tty))
     (:inherit default
               :background "grey64"
               :foreground "grey50"
               ))
    (t
     (:inherit default
               :background "grey76"
               :foreground "grey64"
               :box (:color "grey76"
                            :line-width 1
                            :style released-button)
               )))
  "Default face used by the ruler."
  :group 'ruler-mode)

(defface ruler-mode-pad
  '((((type tty))
     (:inherit ruler-mode-default
               :background "grey50"
               ))
    (t
     (:inherit ruler-mode-default
               :background "grey64"
               )))
  "Face used to pad inactive ruler areas."
  :group 'ruler-mode)

(defface ruler-mode-margins
  '((t
     (:inherit ruler-mode-default
               :foreground "white"
               )))
  "Face used to highlight margin areas."
  :group 'ruler-mode)

(defface ruler-mode-fringes
  '((t
     (:inherit ruler-mode-default
               :foreground "green"
               )))
  "Face used to highlight fringes areas."
  :group 'ruler-mode)

(defface ruler-mode-column-number
  '((t
     (:inherit ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight number graduations."
  :group 'ruler-mode)

(defface ruler-mode-fill-column
  '((t
     (:inherit ruler-mode-default
               :foreground "red"
               )))
  "Face used to highlight the fill column character."
  :group 'ruler-mode)

(defface ruler-mode-comment-column
  '((t
     (:inherit ruler-mode-default
               :foreground "red"
               )))
  "Face used to highlight the comment column character."
  :group 'ruler-mode)

(defface ruler-mode-goal-column
  '((t
     (:inherit ruler-mode-default
               :foreground "red"
               )))
  "Face used to highlight the goal column character."
  :group 'ruler-mode)

(defface ruler-mode-tab-stop
  '((t
     (:inherit ruler-mode-default
               :foreground "steelblue"
               )))
  "Face used to highlight tab stop characters."
  :group 'ruler-mode)

(defface ruler-mode-current-column
  '((t
     (:inherit ruler-mode-default
               :weight bold
               :foreground "yellow"
               )))
  "Face used to highlight the `current-column' character."
  :group 'ruler-mode)


(defsubst ruler-mode-full-window-width ()
  "Return the full width of the selected window."
  (let ((edges (window-edges)))
    (- (nth 2 edges) (nth 0 edges))))

(defsubst ruler-mode-window-col (n)
  "Return a column number relative to the selected window.
N is a column number relative to selected frame."
  (- n
     (car (window-edges))
     (or (car (window-margins)) 0)
     (fringe-columns 'left)
     (scroll-bar-columns 'left)))

(defun ruler-mode-mouse-set-left-margin (start-event)
  "Set left margin end to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (let* ((start (event-start start-event))
         (end   (event-end   start-event))
         col w lm rm)
    (when (eq start end) ;; mouse click
      (save-selected-window
        (select-window (posn-window start))
        (setq col (- (car (posn-col-row start)) (car (window-edges))
                     (scroll-bar-columns 'left))
              w   (- (ruler-mode-full-window-width)
                     (scroll-bar-columns 'left)
                     (scroll-bar-columns 'right)))
        (when (and (>= col 0) (< col w))
          (setq lm (window-margins)
                rm (or (cdr lm) 0)
                lm (or (car lm) 0))
          (message "Left margin set to %d (was %d)" col lm)
          (set-window-margins nil col rm))))))

(defun ruler-mode-mouse-set-right-margin (start-event)
  "Set right margin beginning to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (let* ((start (event-start start-event))
         (end   (event-end   start-event))
         col w lm rm)
    (when (eq start end) ;; mouse click
      (save-selected-window
        (select-window (posn-window start))
        (setq col (- (car (posn-col-row start)) (car (window-edges))
                     (scroll-bar-columns 'left))
              w   (- (ruler-mode-full-window-width)
                     (scroll-bar-columns 'left)
                     (scroll-bar-columns 'right)))
        (when (and (>= col 0) (< col w))
          (setq lm  (window-margins)
                rm  (or (cdr lm) 0)
                lm  (or (car lm) 0)
                col (- w col 1))
          (message "Right margin set to %d (was %d)" col rm)
          (set-window-margins nil lm col))))))

(defvar ruler-mode-dragged-symbol nil
  "Column symbol dragged in the ruler.
That is `fill-column', `comment-column', `goal-column', or nil when
nothing is dragged.")

(defun ruler-mode-mouse-grab-any-column (start-event)
  "Drag a column symbol on the ruler.
Start dragging on mouse down event START-EVENT, and update the column
symbol value with the current value of the ruler graduation while
dragging.  See also the variable `ruler-mode-dragged-symbol'."
  (interactive "e")
  (setq ruler-mode-dragged-symbol nil)
  (let* ((start (event-start start-event))
         col newc oldc)
    (save-selected-window
      (select-window (posn-window start))
      (setq col  (ruler-mode-window-col (car (posn-col-row start)))
            newc (+ col (window-hscroll)))
      (and
       (>= col 0) (< col (window-width))
       (cond

        ;; Handle the fill column.
        ((eq newc fill-column)
         (setq oldc fill-column
               ruler-mode-dragged-symbol 'fill-column)
         t) ;; Start dragging

        ;; Handle the comment column.
        ((eq newc comment-column)
         (setq oldc comment-column
               ruler-mode-dragged-symbol 'comment-column)
         t) ;; Start dragging

        ;; Handle the goal column.
        ;; A. On mouse down on the goal column character on the ruler,
        ;;    update the `goal-column' value while dragging.
        ;; B. If `goal-column' is nil, set the goal column where the
        ;;    mouse is clicked.
        ;; C. On mouse click on the goal column character on the
        ;;    ruler, unset the goal column.
        ((eq newc goal-column)          ; A. Drag the goal column.
         (setq oldc goal-column
               ruler-mode-dragged-symbol 'goal-column)
         t) ;; Start dragging

        ((null goal-column)             ; B. Set the goal column.
         (setq oldc goal-column
               goal-column newc)
         ;; mouse-2 coming AFTER drag-mouse-2 invokes `ding'.  This
         ;; `ding' flushes the next messages about setting goal
         ;; column.  So here I force fetch the event(mouse-2) and
         ;; throw away.
         (read-event)
         ;; Ding BEFORE `message' is OK.
         (when ruler-mode-set-goal-column-ding-flag
           (ding))
         (message "Goal column set to %d (click on %s again to unset it)"
                  newc
                  (propertize (char-to-string ruler-mode-goal-column-char)
                              'face 'ruler-mode-goal-column))
         nil) ;; Don't start dragging.
        )
       (if (eq 'click (ruler-mode-mouse-drag-any-column-iteration
                       (posn-window start)))
           (when (eq 'goal-column ruler-mode-dragged-symbol)
             ;; C. Unset the goal column.
             (set-goal-column t))
         ;; At end of dragging, report the updated column symbol.
         (message "%s is set to %d (was %d)"
                  ruler-mode-dragged-symbol
                  (symbol-value ruler-mode-dragged-symbol)
                  oldc))))))

(defun ruler-mode-mouse-drag-any-column-iteration (window)
  "Update the ruler while dragging the mouse.
WINDOW is the window where occurred the last down-mouse event.
Return the symbol `drag' if the mouse has been dragged, or `click' if
the mouse has been clicked."
  (let ((drags 0)
        event)
    (track-mouse
      (while (mouse-movement-p (setq event (read-event)))
        (setq drags (1+ drags))
        (when (eq window (posn-window (event-end event)))
          (ruler-mode-mouse-drag-any-column event)
          (force-mode-line-update))))
    (if (and (zerop drags) (eq 'click (car (event-modifiers event))))
        'click
      'drag)))

(defun ruler-mode-mouse-drag-any-column (start-event)
  "Update the value of the symbol dragged on the ruler.
Called on each mouse motion event START-EVENT."
  (let* ((start (event-start start-event))
         (end   (event-end   start-event))
         col newc)
    (save-selected-window
      (select-window (posn-window start))
      (setq col  (ruler-mode-window-col (car (posn-col-row end)))
            newc (+ col (window-hscroll)))
      (when (and (>= col 0) (< col (window-width)))
        (set ruler-mode-dragged-symbol newc)))))

(defun ruler-mode-mouse-add-tab-stop (start-event)
  "Add a tab stop to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (when ruler-mode-show-tab-stops
    (let* ((start (event-start start-event))
           (end   (event-end   start-event))
           col ts)
      (when (eq start end) ;; mouse click
        (save-selected-window
          (select-window (posn-window start))
          (setq col (ruler-mode-window-col (car (posn-col-row start)))
                ts  (+ col (window-hscroll)))
          (and (>= col 0) (< col (window-width))
               (not (member ts tab-stop-list))
               (progn
                 (message "Tab stop set to %d" ts)
                 (setq tab-stop-list (sort (cons ts tab-stop-list)
                                           #'<)))))))))

(defun ruler-mode-mouse-del-tab-stop (start-event)
  "Delete tab stop at the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (when ruler-mode-show-tab-stops
    (let* ((start (event-start start-event))
           (end   (event-end   start-event))
           col ts)
      (when (eq start end) ;; mouse click
        (save-selected-window
          (select-window (posn-window start))
          (setq col (ruler-mode-window-col (car (posn-col-row start)))
                ts  (+ col (window-hscroll)))
          (and (>= col 0) (< col (window-width))
               (member ts tab-stop-list)
               (progn
                 (message "Tab stop at %d deleted" ts)
                 (setq tab-stop-list (delete ts tab-stop-list)))))))))

(defun ruler-mode-toggle-show-tab-stops ()
  "Toggle showing of tab stops on the ruler."
  (interactive)
  (setq ruler-mode-show-tab-stops (not ruler-mode-show-tab-stops))
  (force-mode-line-update))

(defvar ruler-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line down-mouse-1]
      #'ignore)
    (define-key km [header-line down-mouse-3]
      #'ignore)
    (define-key km [header-line down-mouse-2]
      #'ruler-mode-mouse-grab-any-column)
    (define-key km [header-line (shift down-mouse-1)]
      #'ruler-mode-mouse-set-left-margin)
    (define-key km [header-line (shift down-mouse-3)]
      #'ruler-mode-mouse-set-right-margin)
    (define-key km [header-line (control down-mouse-1)]
      #'ruler-mode-mouse-add-tab-stop)
    (define-key km [header-line (control down-mouse-3)]
      #'ruler-mode-mouse-del-tab-stop)
    (define-key km [header-line (control down-mouse-2)]
      #'ruler-mode-toggle-show-tab-stops)
    (define-key km [header-line (shift mouse-1)]
      'ignore)
    (define-key km [header-line (shift mouse-3)]
      'ignore)
    (define-key km [header-line (control mouse-1)]
      'ignore)
    (define-key km [header-line (control mouse-3)]
      'ignore)
    (define-key km [header-line (control mouse-2)]
      'ignore)
    km)
  "Keymap for ruler minor mode.")

(defvar ruler-mode-header-line-format-old nil
  "Hold previous value of `header-line-format'.")

(defvar ruler-mode-ruler-function 'ruler-mode-ruler
  "Function to call to return ruler header line format.
This variable is expected to be made buffer-local by modes.")

(defconst ruler-mode-header-line-format
  '(:eval (funcall ruler-mode-ruler-function))
  "`header-line-format' used in ruler mode.
Call `ruler-mode-ruler-function' to compute the ruler value.")

;;;###autoload
(defvar ruler-mode nil
  "Non-nil if Ruler mode is enabled.
Use the command `ruler-mode' to change this variable.")
(make-variable-buffer-local 'ruler-mode)

(defun ruler--save-header-line-format ()
  "Install the header line format for Ruler mode.
Unless Ruler mode is already enabled, save the old header line
format first."
  (when (and (not ruler-mode)
	     (local-variable-p 'header-line-format)
	     (not (local-variable-p 'ruler-mode-header-line-format-old)))
    (set (make-local-variable 'ruler-mode-header-line-format-old)
	 header-line-format))
  (setq header-line-format ruler-mode-header-line-format))

;;;###autoload
(define-minor-mode ruler-mode
  "Toggle display of ruler in header line (Ruler mode).
With a prefix argument ARG, enable Ruler mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  nil nil
  ruler-mode-map
  :group 'ruler-mode
  :variable (ruler-mode
	     . (lambda (enable)
		 (when enable
		   (ruler--save-header-line-format))
		 (setq ruler-mode enable)))
  (if ruler-mode
      (add-hook 'post-command-hook 'force-mode-line-update nil t)
    ;; When `ruler-mode' is off restore previous header line format if
    ;; the current one is the ruler header line format.
    (when (eq header-line-format ruler-mode-header-line-format)
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'ruler-mode-header-line-format-old)
        (setq header-line-format ruler-mode-header-line-format-old)
        (kill-local-variable 'ruler-mode-header-line-format-old)))
    (remove-hook 'post-command-hook 'force-mode-line-update t)))

;; Add ruler-mode to the minor mode menu in the mode line
(define-key mode-line-mode-menu [ruler-mode]
  `(menu-item "Ruler" ruler-mode
              :button (:toggle . ruler-mode)))

(defconst ruler-mode-ruler-help-echo
  "\
S-mouse-1/3: set L/R margin, \
mouse-2: set goal column, \
C-mouse-2: show tabs"
  "Help string shown when mouse is over the ruler.
`ruler-mode-show-tab-stops' is nil.")

(defconst ruler-mode-ruler-help-echo-when-goal-column
  "\
S-mouse-1/3: set L/R margin, \
C-mouse-2: show tabs"
  "Help string shown when mouse is over the ruler.
`goal-column' is set and `ruler-mode-show-tab-stops' is nil.")

(defconst ruler-mode-ruler-help-echo-when-tab-stops
  "\
C-mouse1/3: set/unset tab, \
C-mouse-2: hide tabs"
  "Help string shown when mouse is over the ruler.
`ruler-mode-show-tab-stops' is non-nil.")

(defconst ruler-mode-fill-column-help-echo
  "drag-mouse-2: set fill column"
  "Help string shown when mouse is on the fill column character.")

(defconst ruler-mode-comment-column-help-echo
  "drag-mouse-2: set comment column"
  "Help string shown when mouse is on the comment column character.")

(defconst ruler-mode-goal-column-help-echo
  "\
drag-mouse-2: set goal column, \
mouse-2: unset goal column"
  "Help string shown when mouse is on the goal column character.")

(defconst ruler-mode-margin-help-echo
  "%s margin %S"
  "Help string shown when mouse is over a margin area.")

(defconst ruler-mode-fringe-help-echo
  "%s fringe %S"
  "Help string shown when mouse is over a fringe area.")

(defsubst ruler-mode-space (width &rest props)
  "Return a single space string of WIDTH times the normal character width.
Optional argument PROPS specifies other text properties to apply."
  (apply 'propertize " " 'display (list 'space :width width) props))

(defun ruler-mode-ruler ()
  "Compute and return a header line ruler."
  (let* ((w (window-width))
         (m (window-margins))
         (f (window-fringes))
         (i 0)
         (j (window-hscroll))
         ;; Setup the scrollbar, fringes, and margins areas.
         (lf (ruler-mode-space
              'left-fringe
              'face 'ruler-mode-fringes
              'help-echo (format ruler-mode-fringe-help-echo
                                 "Left" (or (car f) 0))))
         (rf (ruler-mode-space
              'right-fringe
              'face 'ruler-mode-fringes
              'help-echo (format ruler-mode-fringe-help-echo
                                 "Right" (or (cadr f) 0))))
         (lm (ruler-mode-space
              'left-margin
              'face 'ruler-mode-margins
              'help-echo (format ruler-mode-margin-help-echo
                                 "Left" (or (car m) 0))))
         (rm (ruler-mode-space
              'right-margin
              'face 'ruler-mode-margins
              'help-echo (format ruler-mode-margin-help-echo
                                 "Right" (or (cdr m) 0))))
         (sb (ruler-mode-space
              'scroll-bar
              'face 'ruler-mode-pad))
         ;; Remember the scrollbar vertical type.
         (sbvt (car (window-current-scroll-bars)))
         ;; Create an "clean" ruler.
         (ruler
          (propertize
           (string-to-multibyte
	    (make-string w ruler-mode-basic-graduation-char))
           'face 'ruler-mode-default
           'local-map ruler-mode-map
           'help-echo (cond
                       (ruler-mode-show-tab-stops
                        ruler-mode-ruler-help-echo-when-tab-stops)
                       (goal-column
                        ruler-mode-ruler-help-echo-when-goal-column)
                       (ruler-mode-ruler-help-echo))))
         k c)
    ;; Setup the active area.
    (while (< i w)
      ;; Graduations.
      (cond
       ;; Show a number graduation.
       ((= (mod j 10) 0)
        (setq c (number-to-string (/ j 10))
              m (length c)
              k i)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-column-number
         ruler)
        (while (and (> m 0) (>= k 0))
          (aset ruler k (aref c (setq m (1- m))))
          (setq k (1- k))))
       ;; Show an intermediate graduation.
       ((= (mod j 5) 0)
        (aset ruler i ruler-mode-inter-graduation-char)))
      ;; Special columns.
      (cond
       ;; Show the `current-column' marker.
       ((= j (current-column))
        (aset ruler i ruler-mode-current-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-current-column
         ruler))
       ;; Show the `goal-column' marker.
       ((and goal-column (= j goal-column))
        (aset ruler i ruler-mode-goal-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-goal-column
         ruler)
	(put-text-property
         i (1+ i) 'mouse-face 'mode-line-highlight
         ruler)
        (put-text-property
         i (1+ i) 'help-echo ruler-mode-goal-column-help-echo
         ruler))
       ;; Show the `comment-column' marker.
       ((= j comment-column)
        (aset ruler i ruler-mode-comment-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-comment-column
         ruler)
	(put-text-property
         i (1+ i) 'mouse-face 'mode-line-highlight
         ruler)
        (put-text-property
         i (1+ i) 'help-echo ruler-mode-comment-column-help-echo
         ruler))
       ;; Show the `fill-column' marker.
       ((= j fill-column)
        (aset ruler i ruler-mode-fill-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-fill-column
         ruler)
	(put-text-property
         i (1+ i) 'mouse-face 'mode-line-highlight
         ruler)
        (put-text-property
         i (1+ i) 'help-echo ruler-mode-fill-column-help-echo
         ruler))
       ;; Show the `tab-stop-list' markers.
       ((and ruler-mode-show-tab-stops (member j tab-stop-list))
        (aset ruler i ruler-mode-tab-stop-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-tab-stop
         ruler)))
      (setq i (1+ i)
            j (1+ j)))
    ;; Return the ruler propertized string.  Using list here,
    ;; instead of concat visually separate the different areas.
    (if (nth 2 (window-fringes))
        ;; fringes outside margins.
        (list "" (and (eq 'left sbvt) sb) lf lm
              ruler rm rf (and (eq 'right sbvt) sb))
      ;; fringes inside margins.
      (list "" (and (eq 'left sbvt) sb) lm lf
            ruler rf rm (and (eq 'right sbvt) sb)))))

(provide 'ruler-mode)

;; Local Variables:
;; coding: iso-latin-1
;; End:

;;; ruler-mode.el ends here
