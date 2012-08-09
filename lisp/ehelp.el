;;; ehelp.el --- bindings for electric-help mode -*- lexical-binding: t -*-

;; Copyright (C) 1986, 1995, 2000-2012  Free Software Foundation, Inc.

;; Author: Richard Mlynarik
;; (according to ack.texi and authors.el)
;; Maintainer: FSF
;; Keywords: help, extensions

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

;; This package provides a pre-packaged `Electric Help Mode' for
;; browsing on-line help screens.  There is one entry point,
;; `with-electric-help'; all you have to give it is a no-argument
;; function that generates the actual text of the help into the current
;; buffer.

;; To make this the default, you must do
;; (require 'ehelp)
;; (define-key global-map "\C-h" 'ehelp-command)
;; (define-key global-map [help] 'ehelp-command)
;; (define-key global-map [f1] 'ehelp-command)

;;; Code:

(require 'electric)

(defvar electric-help-form-to-execute nil)

(defgroup electric-help ()
  "Electric help facility."
  :version "21.1"
  :group 'help)

(defcustom electric-help-shrink-window t
  "If set, adjust help window sizes to buffer sizes when displaying help."
  :type 'boolean
  :group 'electric-help)

(defcustom electric-help-mode-hook nil
  "Hook run by `with-electric-help' after initializing the buffer."
  :type 'hook
  :group 'electric-help)

(put 'electric-help-undefined 'suppress-keymap t)

(defvar electric-help-map
  (let ((map (make-keymap)))
    ;; allow all non-self-inserting keys - search, scroll, etc, but
    ;; let M-x and C-x exit ehelp mode and retain buffer:
    (suppress-keymap map)
    (define-key map "\C-u" 'electric-help-undefined)
    (define-key map [?\C-0] 'electric-help-undefined)
    (define-key map [?\C-1] 'electric-help-undefined)
    (define-key map [?\C-2] 'electric-help-undefined)
    (define-key map [?\C-3] 'electric-help-undefined)
    (define-key map [?\C-4] 'electric-help-undefined)
    (define-key map [?\C-5] 'electric-help-undefined)
    (define-key map [?\C-6] 'electric-help-undefined)
    (define-key map [?\C-7] 'electric-help-undefined)
    (define-key map [?\C-8] 'electric-help-undefined)
    (define-key map [?\C-9] 'electric-help-undefined)
    (define-key map (char-to-string help-char) 'electric-help-help)
    (define-key map "?" 'electric-help-help)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    ;(define-key map "\C-g" 'electric-help-exit)
    (define-key map "Q" 'electric-help-exit)
    (define-key map "q" 'electric-help-exit)
    ;;a better key than this?
    (define-key map "R" 'electric-help-retain)
    (define-key map "r" 'electric-help-retain)
    (define-key map "\ex" 'electric-help-execute-extended)
    (define-key map "\C-x" 'electric-help-ctrl-x-prefix)
    map)
  "Keymap defining commands available in `electric-help-mode'.")

(defvar electric-help-orig-major-mode nil)
(make-variable-buffer-local 'electric-help-orig-major-mode)

(defun electric-help-mode ()
  "`with-electric-help' temporarily places its buffer in this mode.
\(On exit from `with-electric-help', the original `major-mode' is restored.)"
  (setq buffer-read-only t)
  (setq electric-help-orig-major-mode major-mode)
  (setq mode-name "Help")
  (setq major-mode 'help)
  (setq mode-line-buffer-identification '(" Help:  %b"))
  (use-local-map electric-help-map)
  (add-hook 'mouse-leave-buffer-hook 'electric-help-retain)
  (view-mode -1)
  ;; this is done below in with-electric-help
  ;(run-hooks 'electric-help-mode-hook)
  )

;;;###autoload
(defun with-electric-help (thunk &optional buffer noerase minheight)
  "Pop up an \"electric\" help buffer.
THUNK is a function of no arguments which is called to initialize the
contents of BUFFER.  BUFFER defaults to `*Help*'.  BUFFER will be
erased before THUNK is called unless NOERASE is non-nil.  THUNK will
be called while BUFFER is current and with `standard-output' bound to
the buffer specified by BUFFER.

If THUNK returns nil, we display BUFFER starting at the top, and shrink
the window to fit.  If THUNK returns non-nil, we don't do those things.

After THUNK has been called, this function \"electrically\" pops up a
window in which BUFFER is displayed and allows the user to scroll
through that buffer in `electric-help-mode'.  The window's height will
be at least MINHEIGHT if this value is non-nil.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit if `electric-help-shrink-window' is non-nil.
If THUNK returns non-nil, we don't do those things.

When the user exits (with `electric-help-exit', or otherwise), the help
buffer's window disappears (i.e., we use `save-window-excursion'), and
BUFFER is put back into its original major mode."
  (setq buffer (get-buffer-create (or buffer "*Help*")))
  (let ((one (one-window-p t))
	(config (current-window-configuration))
        (bury nil)
        (electric-help-form-to-execute nil))
    (unwind-protect
         (save-excursion
           (when one
	     (goto-char (window-start (selected-window))))
           (let ((pop-up-windows t))
             (pop-to-buffer buffer))
           (with-current-buffer buffer
             (when (and minheight (< (window-height) minheight))
	       (enlarge-window (- minheight (window-height))))
             (electric-help-mode)
	     (setq buffer-read-only nil)
	     (unless noerase
	       (erase-buffer)))
           (let ((standard-output buffer))
             (unless (funcall thunk)
	       (set-buffer buffer)
	       (set-buffer-modified-p nil)
	       (goto-char (point-min))
	       (when (and one electric-help-shrink-window)
		 (shrink-window-if-larger-than-buffer))))
           (set-buffer buffer)
           (run-hooks 'electric-help-mode-hook)
	   (setq buffer-read-only t)
           (if (eq (car-safe (electric-help-command-loop)) 'retain)
               (setq config (current-window-configuration))
	     (setq bury t))
	   ;; Remove the hook.
	   (when (memq 'electric-help-retain mouse-leave-buffer-hook)
	     (remove-hook 'mouse-leave-buffer-hook 'electric-help-retain)))
      (message "")
      (set-buffer buffer)
      (setq buffer-read-only nil)

      ;; Restore the original major mode saved by `electric-help-mode'.
      ;; We should really get a usable *Help* buffer when retaining
      ;; the electric one with `r'.  The problem is that a simple
      ;; call to `help-mode' won't cut it; e.g. RET is bound wrong
      ;; afterwards (`View-scroll-line-forward' instead of `help-follow').
      ;; That's because Help mode should be set with `with-help-window'
      ;; instead of the direct call to `help-mode'. But at least
      ;; RET works correctly on links after using `help-mode'.
      ;; This is satisfactory enough.
      (condition-case ()
          (funcall (or electric-help-orig-major-mode 'fundamental-mode))
        (error nil))

      (set-window-configuration config)
      (when bury
	;;>> Perhaps this shouldn't be done,
	;; so that when we say "Press space to bury" we mean it
	(replace-buffer-in-windows buffer)
	;; must do this outside of save-window-excursion
	(bury-buffer buffer))
      (eval electric-help-form-to-execute))))

(defun electric-help-command-loop ()
  (catch 'exit
    (if (pos-visible-in-window-p (point-max))
	(progn (message "%s" (substitute-command-keys "<<< Press Space to bury the help buffer, Press \\[electric-help-retain] to retain it >>>"))
	       (if (equal (setq unread-command-events (list (read-event)))
			  '(?\s))
		   (progn (setq unread-command-events nil)
			  (throw 'exit t)))))
    (let (up down both neither
	  (standard (and (eq (key-binding " " nil t)
			     'scroll-up)
			 (eq (key-binding "\^?" nil t)
			     'scroll-down)
			 (eq (key-binding "q" nil t)
			     'electric-help-exit)
			 (eq (key-binding "r" nil t)
			     'electric-help-retain))))
      (Electric-command-loop
        'exit
	(function (lambda ()
	  (sit-for 0) ;necessary if last command was end-of-buffer or
	              ;beginning-of-buffer - otherwise pos-visible-in-window-p
	              ;will yield a wrong result.
	  (let ((min (pos-visible-in-window-p (point-min)))
		(max (pos-visible-in-window-p (1- (point-max)))))
	    (cond (isearch-mode 'noprompt)
		  ((and min max)
		   (cond (standard "Press q to exit, r to retain ")
			 (neither)
			 (t (setq neither (substitute-command-keys "Press \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (min
		   (cond (standard "Press SPC to scroll, q to exit, r to retain ")
			 (up)
			 (t (setq up (substitute-command-keys "Press \\[scroll-up] to scroll, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (max
		   (cond (standard "Press DEL to scroll back, q to exit, r to retain ")
			 (down)
			 (t (setq down (substitute-command-keys "Press \\[scroll-down] to scroll back, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (t
		   (cond (standard "Press SPC to scroll, DEL to scroll back, q to exit, r to retain ")
			 (both)
			 (t (setq both (substitute-command-keys "Press \\[scroll-up] to scroll, \\[scroll-down] to scroll back, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))))))
		    t))))



;(defun electric-help-scroll-up (arg)
;  ">>>Doc"
;  (interactive "P")
;  (if (and (null arg) (pos-visible-in-window-p (point-max)))
;      (electric-help-exit)
;    (scroll-up arg)))

(defun electric-help-exit ()
  "Exit `with-electric-help', restoring the previous window/buffer configuration.
\(The *Help* buffer will be buried.)"
  (interactive)
  ;; Make sure that we don't throw twice, even if two events cause
  ;; calling this function:
  (if (memq 'electric-help-retain mouse-leave-buffer-hook)
      (progn
	(remove-hook 'mouse-leave-buffer-hook 'electric-help-retain)
	(throw 'exit t))))

(defun electric-help-retain ()
  "Exit `with-electric-help', retaining the current window/buffer configuration.
\(The *Help* buffer will not be selected, but \\[switch-to-buffer-other-window] RET
will select it.)"
  (interactive)
  ;; Make sure that we don't throw twice, even if two events cause
  ;; calling this function:
  (if (memq 'electric-help-retain mouse-leave-buffer-hook)
      (progn
	(remove-hook 'mouse-leave-buffer-hook 'electric-help-retain)
	(throw 'exit '(retain)))))


(defun electric-help-undefined ()
  (interactive)
  (error "%s is undefined -- Press %s to exit"
	 (mapconcat 'single-key-description (this-command-keys) " ")
	 (if (eq (key-binding "q" nil t) 'electric-help-exit)
	     "q"
	   (substitute-command-keys "\\[electric-help-exit]"))))


;>>> this needs to be hairified (recursive help, anybody?)
(defun electric-help-help ()
  (interactive)
  (if (and (eq (key-binding "q" nil t) 'electric-help-exit)
	   (eq (key-binding " " nil t) 'scroll-up)
	   (eq (key-binding "\^?" nil t) 'scroll-down)
	   (eq (key-binding "r" nil t) 'electric-help-retain))
      (message "SPC scrolls up, DEL scrolls down, q exits burying help buffer, r exits")
    (message "%s" (substitute-command-keys "\\[scroll-up] scrolls up, \\[scroll-down] scrolls down, \\[electric-help-exit] exits burying help buffer, \\[electric-help-retain] exits")))
  (sit-for 2))


;;;###autoload
(defun electric-helpify (fun &optional name)
  (let ((name (or name "*Help*")))
    (if (save-window-excursion
	  ;; kludge-o-rama
	  (let* ((p (symbol-function 'help-print-return-message))
		 (b (get-buffer name))
		 (m (buffer-modified-p b)))
	    (and b (not (get-buffer-window b))
		 (setq b nil))
	    (unwind-protect
		(progn
		  (message "%s..." (capitalize (symbol-name fun)))
		  ;; with-output-to-temp-buffer marks the buffer as unmodified.
		  ;; kludging excessively and relying on that as some sort
		  ;;  of indication leads to the following abomination...
		  ;;>> This would be doable without such icky kludges if either
		  ;;>> (a) there were a function to read the interactive
		  ;;>>     args for a command and return a list of those args.
		  ;;>>     (To which one would then just apply the command)
		  ;;>>     (The only problem with this is that interactive-p
		  ;;>>      would break, but that is such a misfeature in
		  ;;>>      any case that I don't care)
		  ;;>>     It is easy to do this for emacs-lisp functions;
		  ;;>>     the only problem is getting the interactive spec
		  ;;>>     for subrs
		  ;;>> (b) there were a function which returned a
		  ;;>>     modification-tick for a buffer.  One could tell
		  ;;>>     whether a buffer had changed by whether the
		  ;;>>     modification-tick were different.
		  ;;>>     (Presumably there would have to be a way to either
		  ;;>>      restore the tick to some previous value, or to
		  ;;>>      suspend updating of the tick in order to allow
		  ;;>>      things like momentary-string-display)
		  (and b
		       (with-current-buffer b
			 (set-buffer-modified-p t)))
		  (fset 'help-print-return-message 'ignore)
		  (call-interactively fun)
		  (and (get-buffer name)
		       (get-buffer-window (get-buffer name))
		       (or (not b)
			   (not (eq b (get-buffer name)))
			   (not (buffer-modified-p b)))))
	      (fset 'help-print-return-message p)
	      (and b (buffer-name b)
		   (with-current-buffer b
		     (set-buffer-modified-p m))))))
	(with-electric-help 'ignore name t))))



;; This is to be bound to M-x in ehelp mode. Retains ehelp buffer and then
;; continues with execute-extended-command.
(defun electric-help-execute-extended (_prefixarg)
  (interactive "p")
  (setq electric-help-form-to-execute '(execute-extended-command nil))
  (electric-help-retain))

;; This is to be buond to C-x in ehelp mode. Retains ehelp buffer and then
;; continues with ctrl-x prefix.
(defun electric-help-ctrl-x-prefix (_prefixarg)
  (interactive "p")
  (setq electric-help-form-to-execute '(progn (message nil) (setq unread-command-char ?\C-x)))
  (electric-help-retain))


(defun electric-describe-key ()
  (interactive)
  (electric-helpify 'describe-key))

(defun electric-describe-mode ()
  (interactive)
  (electric-helpify 'describe-mode))

(defun electric-view-lossage ()
  (interactive)
  (electric-helpify 'view-lossage))

;(defun electric-help-for-help ()
;  "See help-for-help"
;  (interactive)
;  )

(defun electric-describe-function ()
  (interactive)
  (electric-helpify 'describe-function))

(defun electric-describe-variable ()
  (interactive)
  (electric-helpify 'describe-variable))

(defun electric-describe-bindings ()
  (interactive)
  (electric-helpify 'describe-bindings))

(defun electric-describe-syntax ()
  (interactive)
  (electric-helpify 'describe-syntax))

(defun electric-command-apropos ()
  (interactive)
  (electric-helpify 'command-apropos "*Apropos*"))

;(define-key help-map "a" 'electric-command-apropos)

(defun electric-apropos ()
  (interactive)
  (electric-helpify 'apropos))


;;;; ehelp-map

(defvar ehelp-map
  (let ((map (copy-keymap help-map)))
    (substitute-key-definition 'apropos 'electric-apropos map)
    (substitute-key-definition 'command-apropos 'electric-command-apropos map)
    (substitute-key-definition 'describe-key 'electric-describe-key map)
    (substitute-key-definition 'describe-mode 'electric-describe-mode map)
    (substitute-key-definition 'view-lossage 'electric-view-lossage map)
    (substitute-key-definition 'describe-function 'electric-describe-function map)
    (substitute-key-definition 'describe-variable 'electric-describe-variable map)
    (substitute-key-definition 'describe-bindings 'electric-describe-bindings map)
    (substitute-key-definition 'describe-syntax 'electric-describe-syntax map)
    map))

;;;###(autoload 'ehelp-command "ehelp" "Prefix command for ehelp." t 'keymap)
(defalias 'ehelp-command ehelp-map)
(put 'ehelp-command 'documentation "Prefix command for ehelp.")

(provide 'ehelp)

;;; ehelp.el ends here
