;;; helper.el --- utility help package supporting help in electric modes

;; Copyright (C) 1985, 2001-2012 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: help
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

;;; Code:

;; hey, here's a helping hand.

;; Bind this to a string for <blank> in "... Other keys <blank>".
;; Helper-help uses this to construct help string when scrolling.
;; Defaults to "return"
(defvar Helper-return-blurb nil)

;; Keymap implementation doesn't work too well for non-standard loops.
;; But define it anyway for those who can use it.  Non-standard loops
;; will probably have to use Helper-help.  You can't autoload the
;; keymap either.


(defvar Helper-help-map nil)
(if Helper-help-map
    nil
  (setq Helper-help-map (make-keymap))
  ;(fillarray Helper-help-map 'undefined)
  (define-key Helper-help-map "m" 'Helper-describe-mode)
  (define-key Helper-help-map "b" 'Helper-describe-bindings)
  (define-key Helper-help-map "c" 'Helper-describe-key-briefly)
  (define-key Helper-help-map "k" 'Helper-describe-key)
  ;(define-key Helper-help-map "f" 'Helper-describe-function)
  ;(define-key Helper-help-map "v" 'Helper-describe-variable)
  (define-key Helper-help-map "?" 'Helper-help-options)
  (define-key Helper-help-map (char-to-string help-char) 'Helper-help-options)
  (fset 'Helper-help-map Helper-help-map))

(defun Helper-help-scroller ()
  (let ((blurb (or (and (boundp 'Helper-return-blurb)
			Helper-return-blurb)
		   "return")))
    (save-window-excursion
      (goto-char (window-start (selected-window)))
      (if (get-buffer-window "*Help*")
	  (pop-to-buffer "*Help*")
	(switch-to-buffer "*Help*"))
      (goto-char (point-min))
      (let ((continue t) state)
	(while continue
	  (setq state (+ (* 2 (if (pos-visible-in-window-p (point-max)) 1 0))
			 (if (pos-visible-in-window-p (point-min)) 1 0)))
	  (message
	    (nth state
		 '("Space forward, Delete back. Other keys %s"
		   "Space scrolls forward. Other keys %s"
		   "Delete scrolls back. Other keys %s"
		   "Type anything to %s"))
	    blurb)
	  (setq continue (read-event))
	  (cond ((and (memq continue '(?\s ?\C-v)) (< state 2))
		 (scroll-up))
		((= continue ?\C-l)
		 (recenter))
		((and (= continue ?\177) (zerop (% state 2)))
		 (scroll-down))
		(t (setq continue nil))))))))

(defun Helper-help-options ()
  "Describe help options."
  (interactive)
  (message "c (key briefly), m (mode), k (key), b (bindings)")
  ;(message "c (key briefly), m (mode), k (key), v (variable), f (function)")
  (sit-for 4))

(defun Helper-describe-key-briefly (key)
  "Briefly describe binding of KEY."
  (interactive "kDescribe key briefly: ")
  (describe-key-briefly key)
  (sit-for 4))

(defun Helper-describe-key (key)
  "Describe binding of KEY."
  (interactive "kDescribe key: ")
  (save-window-excursion (describe-key key))
  (Helper-help-scroller))

(defun Helper-describe-function ()
  "Describe a function.  Name read interactively."
  (interactive)
  (save-window-excursion (call-interactively 'describe-function))
  (Helper-help-scroller))

(defun Helper-describe-variable ()
  "Describe a variable.  Name read interactively."
  (interactive)
  (save-window-excursion (call-interactively 'describe-variable))
  (Helper-help-scroller))

(defun Helper-describe-mode ()
  "Describe the current mode."
  (interactive)
  (let ((name (format-mode-line mode-name))
	(documentation (documentation major-mode)))
    (with-current-buffer (get-buffer-create "*Help*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert name " Mode\n" documentation)
      (help-mode)))
  (Helper-help-scroller))

;;;###autoload
(defun Helper-describe-bindings ()
  "Describe local key bindings of current mode."
  (interactive)
  (message "Making binding list...")
  (save-window-excursion (describe-bindings))
  (Helper-help-scroller))

;;;###autoload
(defun Helper-help ()
  "Provide help for current mode."
  (interactive)
  (let ((continue t) c)
    (while continue
      (message "Help (Type ? for further options)")
      (setq c (read-key-sequence nil))
      (setq c (lookup-key Helper-help-map c))
      (cond ((eq c 'Helper-help-options)
	     (Helper-help-options))
	    ((commandp c)
	     (call-interactively c)
	     (setq continue nil))
	    (t
	     (ding)
	     (setq continue nil))))))

(provide 'helper)

;;; helper.el ends here
