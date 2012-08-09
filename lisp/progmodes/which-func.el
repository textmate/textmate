;;; which-func.el --- print current function in mode line

;; Copyright (C) 1994, 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;           (doesn't seem to be responsive any more)
;; Keywords: mode-line, imenu, tools

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

;; This package prints name of function where your current point is
;; located in mode line. It assumes that you work with imenu package
;; and imenu--index-alist is up to date.

;; KNOWN BUGS
;; ----------
;; Really this package shows not "function where the current point is
;; located now", but "nearest function which defined above the current
;; point". So if your current point is located after end of function
;; FOO but before begin of function BAR, FOO will be displayed in mode
;; line.
;; - if two windows display the same buffer, both windows
;;   show the same `which-func' information.

;; TODO LIST
;; ---------
;;     1. Dependence on imenu package should be removed.  Separate
;; function determination mechanism should be used to determine the end
;; of a function as well as the beginning of a function.
;;     2. This package should be realized with the help of overlay
;; properties instead of imenu--index-alist variable.

;;; History:

;; THANKS TO
;; ---------
;; Per Abrahamsen   <abraham@iesd.auc.dk>
;;     Some ideas (inserting  in mode-line,  using of post-command  hook
;;     and toggling this  mode) have  been   borrowed from  his  package
;;     column.el
;; Peter Eisenhauer <pipe@fzi.de>
;;     Bug fixing in case nested indexes.
;; Terry Tateyama   <ttt@ursa0.cs.utah.edu>
;;     Suggestion to use find-file-hook for first imenu
;;     index building.

;;; Code:

;; Variables for customization
;; ---------------------------
;;
(defvar which-func-unknown "???"
  "String to display in the mode line when current function is unknown.")

(defgroup which-func nil
  "Mode to display the current function name in the modeline."
  :group 'tools
  :version "20.3")

(defcustom which-func-modes
  '(emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode python-mode
		    makefile-mode sh-mode fortran-mode f90-mode ada-mode
		    diff-mode)
  "List of major modes for which Which Function mode should be used.
For other modes it is disabled.  If this is equal to t,
then Which Function mode is enabled in any major mode that supports it."
  :group 'which-func
  :type '(choice (const :tag "All modes" t)
		 (repeat (symbol :tag "Major mode"))))

(defcustom which-func-non-auto-modes nil
  "List of major modes where Which Function mode is inactive till Imenu is used.
This means that Which Function mode won't really do anything
until you use Imenu, in these modes.  Note that files
larger than `which-func-maxout' behave in this way too;
Which Function mode doesn't do anything until you use Imenu."
  :group 'which-func
  :type '(repeat (symbol :tag "Major mode")))

(defcustom which-func-maxout 500000
  "Don't automatically compute the Imenu menu if buffer is this big or bigger.
Zero means compute the Imenu menu regardless of size."
  :group 'which-func
  :type 'integer)

(defvar which-func-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'beginning-of-defun)
    (define-key map [mode-line mouse-2]
      (lambda ()
	(interactive)
	(if (eq (point-min) 1)
	    (narrow-to-defun)
	  (widen))))
    (define-key map [mode-line mouse-3] 'end-of-defun)
    map)
  "Keymap to display on mode line which-func.")

(defface which-func
  ;; Whether `font-lock-function-name-face' is an appropriate face to
  ;; inherit depends on the mode-line face; define several variants based
  ;; on the default mode-line face.
  '(;; The default mode-line face on a high-color display is a relatively
    ;; light color ("grey75"), and only the light-background variant of
    ;; `font-lock-function-name-face' is visible against it.
    (((class color) (min-colors 88) (background light))
     :inherit font-lock-function-name-face)
    ;; The default mode-line face on other display types is inverse-video;
    ;; it seems that only in the dark-background case is
    ;; `font-lock-function-name-face' visible against it.
    (((class grayscale mono) (background dark))
     :inherit font-lock-function-name-face)
    (((class color) (background light))
     :inherit font-lock-function-name-face)
    ;; If none of the above cases, use an explicit color chosen to contrast
    ;; well with the default mode-line face.
    (((class color) (min-colors 88) (background dark))
     :foreground "Blue1")
    (((background dark))
     :foreground "Blue1")
    (t
     :foreground "LightSkyBlue"))
  "Face used to highlight mode line function names."
  :group 'which-func)

(defcustom which-func-format
  `("["
    (:propertize which-func-current
		 local-map ,which-func-keymap
		 face which-func
		 ;;mouse-face highlight	; currently not evaluated :-(
		 help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
    "]")
  "Format for displaying the function in the mode line."
  :group 'which-func
  :type 'sexp)
;;;###autoload (put 'which-func-format 'risky-local-variable t)

(defvar which-func-imenu-joiner-function (lambda (x) (car (last x)))
  "Function to join together multiple levels of imenu nomenclature.
Called with a single argument, a list of strings giving the names
of the menus we had to traverse to get to the item.  Returns a
single string, the new name of the item.")

(defvar which-func-cleanup-function nil
  "Function to transform a string before displaying it in the mode line.
The function is called with one argument, the string to display.
Its return value is displayed in the modeline.
If nil, no function is called.  The default value is nil.

This feature can be useful if Imenu is set up to make more
detailed entries (e.g., containing the argument list of a function),
and you want to simplify them for the mode line
\(e.g., removing the parameter list to just have the function name.)")

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;
(require 'imenu)

(defvar which-func-table (make-hash-table :test 'eq :weakness 'key))

(defconst which-func-current
  '(:eval (replace-regexp-in-string
	   "%" "%%"
	   (gethash (selected-window) which-func-table which-func-unknown))))
;;;###autoload (put 'which-func-current 'risky-local-variable t)

(defvar which-func-mode nil
  "Non-nil means display current function name in mode line.
This makes a difference only if `which-function-mode' is non-nil.")
(make-variable-buffer-local 'which-func-mode)
;;(put 'which-func-mode 'permanent-local t)

(add-hook 'find-file-hook 'which-func-ff-hook t)

(defun which-func-ff-hook ()
  "File find hook for Which Function mode.
It creates the Imenu index for the buffer, if necessary."
  (setq which-func-mode
	(and which-function-mode
	     (or (eq which-func-modes t)
		 (member major-mode which-func-modes))))

  (condition-case err
      (if (and which-func-mode
	       (not (member major-mode which-func-non-auto-modes))
	       (or (null which-func-maxout)
		   (< buffer-saved-size which-func-maxout)
		   (= which-func-maxout 0)))
	  (setq imenu--index-alist
		(save-excursion (funcall imenu-create-index-function))))
    (error
     (unless (equal err '(error "This buffer cannot use `imenu-default-create-index-function'"))
       (message "which-func-ff-hook error: %S" err))
     (setq which-func-mode nil))))

(defun which-func-update ()
  ;; "Update the Which-Function mode display for all windows."
  ;; (walk-windows 'which-func-update-1 nil 'visible))
  (which-func-update-1 (selected-window)))

(defun which-func-update-1 (window)
  "Update the Which Function mode display for window WINDOW."
  (with-selected-window window
    (when which-func-mode
      (condition-case info
	  (let ((current (which-function)))
	    (unless (equal current (gethash window which-func-table))
	      (puthash window current which-func-table)
	      (force-mode-line-update)))
	(error
	 (setq which-func-mode nil)
	 (error "Error in which-func-update: %S" info))))))

;;;###autoload
(defun which-func-mode (&optional arg)
  (which-function-mode arg))
(make-obsolete 'which-func-mode 'which-function-mode "24.1")

(defvar which-func-update-timer nil)

;; This is the name people would normally expect.
;;;###autoload
(define-minor-mode which-function-mode
  "Toggle mode line display of current function (Which Function mode).
With a prefix argument ARG, enable Which Function mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Which Function mode is a global minor mode.  When enabled, the
current function name is continuously displayed in the mode line,
in certain major modes."
  :global t :group 'which-func
  (when (timerp which-func-update-timer)
    (cancel-timer which-func-update-timer))
  (setq which-func-update-timer nil)
  (if which-function-mode
      ;;Turn it on
      (progn
        (setq which-func-update-timer
              (run-with-idle-timer idle-update-delay t 'which-func-update))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq which-func-mode
                  (or (eq which-func-modes t)
                      (member major-mode which-func-modes))))))
    ;; Turn it off
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq which-func-mode nil)))))

(defvar which-function-imenu-failed nil
  "Locally t in a buffer if `imenu--make-index-alist' found nothing there.")

(defvar which-func-functions nil
  "List of functions for `which-function' to call with no arguments.
It calls them sequentially, and if any returns non-nil,
`which-function' uses that name and stops looking for the name.")

(defun which-function ()
  "Return current function name based on point.
Uses `which-func-functions', `imenu--index-alist'
or `add-log-current-defun'.
If no function name is found, return nil."
  (let ((name
	 ;; Try the `which-func-functions' functions first.
	 (run-hook-with-args-until-success 'which-func-functions)))

    ;; If Imenu is loaded, try to make an index alist with it.
    (when (and (null name)
	       (boundp 'imenu--index-alist) (null imenu--index-alist)
	       (null which-function-imenu-failed))
      (imenu--make-index-alist t)
      (unless imenu--index-alist
        (set (make-local-variable 'which-function-imenu-failed) t)))
    ;; If we have an index alist, use it.
    (when (and (null name)
	       (boundp 'imenu--index-alist) imenu--index-alist)
      (let ((alist imenu--index-alist)
            (minoffset (point-max))
            offset pair mark imstack namestack)
        ;; Elements of alist are either ("name" . marker), or
        ;; ("submenu" ("name" . marker) ... ). The list can be
        ;; arbitrarily nested.
        (while (or alist imstack)
          (if (null alist)
              (setq alist     (car imstack)
                    namestack (cdr namestack)
                    imstack   (cdr imstack))

            (setq pair (car-safe alist)
                  alist (cdr-safe alist))

            (cond
             ((atom pair))              ; Skip anything not a cons.

             ((imenu--subalist-p pair)
              (setq imstack   (cons alist imstack)
                    namestack (cons (car pair) namestack)
                    alist     (cdr pair)))

             ((number-or-marker-p (setq mark (cdr pair)))
              (when (and (>= (setq offset (- (point) mark)) 0)
                         (< offset minoffset)) ; Find the closest item.
                (setq minoffset offset
                      name (if (null which-func-imenu-joiner-function)
                               (car pair)
                             (funcall
                              which-func-imenu-joiner-function
                              (reverse (cons (car pair) namestack))))))))))))

    ;; Try using add-log support.
    (when (null name)
      (setq name (add-log-current-defun)))
    ;; Filter the name if requested.
    (when name
      (if which-func-cleanup-function
	  (funcall which-func-cleanup-function name)
	name))))

(provide 'which-func)

;;; which-func.el ends here
