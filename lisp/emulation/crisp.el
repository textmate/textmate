;;; crisp.el --- CRiSP/Brief Emacs emulator

;; Copyright (C) 1997-1999, 2001-2012  Free Software Foundation, Inc.

;; Author: Gary D. Foster <Gary.Foster@Corp.Sun.COM>
;; Keywords: emulations brief crisp

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

;; Keybindings and minor functions to duplicate the functionality and
;; finger-feel of the CRiSP/Brief editor.  This package is designed to
;; facilitate transitioning from Brief to (XE|E)macs with a minimum
;; amount of hassles.

;; Enable this package by putting (require 'crisp) in your .emacs and
;; use M-x crisp-mode to toggle it on or off.

;; This package will automatically load the scroll-all.el package if
;; you put (setq crisp-load-scroll-all t) in your .emacs before
;; loading this package.  If this feature is enabled, it will bind
;; meta-f1 to the scroll-all mode toggle.  The scroll-all package
;; duplicates the scroll-all feature in CRiSP.

;; Also, the default keybindings for brief/CRiSP override the M-x
;; key to exit the editor.  If you don't like this functionality, you
;; can prevent this behavior (or redefine it dynamically) by setting
;; the value of `crisp-override-meta-x' either in your .emacs or
;; interactively.  The default setting is t, which means that M-x will
;; by default run `save-buffers-kill-emacs' instead of the command
;; `execute-extended-command'.

;; Finally, if you want to change the string displayed in the modeline
;; when this mode is in effect, override the definition of
;; `crisp-mode-modeline-string' in your .emacs.  The default value is
;; " *Crisp*" which may be a bit lengthy if you have a lot of things
;; being displayed there.

;; All these overrides should go *before* the (require 'crisp) statement.

;;; Code:

(eval-when-compile (require 'cl))

;; local variables

(defgroup crisp nil
  "Emulator for CRiSP and Brief key bindings."
  :prefix "crisp-"
  :group 'emulations)

(defvar crisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(f1)]           'other-window)

    (define-key map [(f2) (down)]    'enlarge-window)
    (define-key map [(f2) (left)]    'shrink-window-horizontally)
    (define-key map [(f2) (right)]   'enlarge-window-horizontally)
    (define-key map [(f2) (up)]      'shrink-window)
    (define-key map [(f3) (down)]    'split-window-below)
    (define-key map [(f3) (right)]   'split-window-right)

    (define-key map [(f4)]           'delete-window)
    (define-key map [(control f4)]   'delete-other-windows)

    (define-key map [(f5)]           'search-forward-regexp)
    (define-key map [(f19)]          'search-forward-regexp)
    (define-key map [(meta f5)]      'search-backward-regexp)

    (define-key map [(f6)]           'query-replace)

    (define-key map [(f7)]           'start-kbd-macro)
    (define-key map [(meta f7)]      'end-kbd-macro)

    (define-key map [(f8)]           'call-last-kbd-macro)
    (define-key map [(meta f8)]      'save-kbd-macro)

    (define-key map [(f9)]           'find-file)
    (define-key map [(meta f9)]      'load-library)

    (define-key map [(f10)]          'execute-extended-command)
    (define-key map [(meta f10)]     'compile)

    (define-key map [(SunF37)]       'kill-buffer)
    (define-key map [(kp-add)]       'crisp-copy-line)
    (define-key map [(kp-subtract)]  'crisp-kill-line)
    ;; just to cover all the bases (GNU Emacs, for instance)
    (define-key map [(f24)]          'crisp-kill-line)
    (define-key map [(insert)]       'crisp-yank-clipboard)
    (define-key map [(f16)]          'crisp-set-clipboard) ; copy on Sun5 kbd
    (define-key map [(f20)]          'crisp-kill-region) ; cut on Sun5 kbd
    (define-key map [(f18)]          'crisp-yank-clipboard) ; paste on Sun5 kbd

    (define-key map [(control f)]    'fill-paragraph-or-region)
    (define-key map [(meta d)]       (lambda ()
                                       (interactive)
                                       (beginning-of-line) (kill-line)))
    (define-key map [(meta e)]       'find-file)
    (define-key map [(meta g)]       'goto-line)
    (define-key map [(meta h)]       'help)
    (define-key map [(meta i)]       'overwrite-mode)
    (define-key map [(meta j)]       'bookmark-jump)
    (define-key map [(meta l)]       'crisp-mark-line)
    (define-key map [(meta m)]       'set-mark-command)
    (define-key map [(meta n)]       'bury-buffer)
    (define-key map [(meta p)]       'crisp-unbury-buffer)
    (define-key map [(meta u)]       'undo)
    (define-key map [(f14)]          'undo)
    (define-key map [(meta w)]       'save-buffer)
    (define-key map [(meta x)]       'crisp-meta-x-wrapper)
    (define-key map [(meta ?0)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "0")))
    (define-key map [(meta ?1)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "1")))
    (define-key map [(meta ?2)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "2")))
    (define-key map [(meta ?3)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "3")))
    (define-key map [(meta ?4)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "4")))
    (define-key map [(meta ?5)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "5")))
    (define-key map [(meta ?6)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "6")))
    (define-key map [(meta ?7)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "7")))
    (define-key map [(meta ?8)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "8")))
    (define-key map [(meta ?9)]      (lambda ()
                                       (interactive)
                                       (bookmark-set "9")))

    (define-key map [(shift delete)]    'kill-word)
    (define-key map [(shift backspace)] 'backward-kill-word)
    (define-key map [(control left)]    'backward-word)
    (define-key map [(control right)]   'forward-word)

    (define-key map [(home)]            'crisp-home)
    (define-key map [(control home)]    (lambda ()
                                          (interactive)
                                          (move-to-window-line 0)))
    (define-key map [(meta home)]       'beginning-of-line)
    (define-key map [(end)]             'crisp-end)
    (define-key map [(control end)]     (lambda ()
                                          (interactive)
                                          (move-to-window-line -1)))
    (define-key map [(meta end)]        'end-of-line)
    map)
  "Local keymap for CRiSP emulation mode.
All the bindings are done here instead of globally to try and be
nice to the world.")

(defcustom crisp-mode-modeline-string " *CRiSP*"
  "String to display in the modeline when CRiSP emulation mode is enabled."
  :type 'string
  :group 'crisp)

;;;###autoload
(defcustom crisp-mode nil
  "Track status of CRiSP emulation mode.
A value of nil means CRiSP mode is not enabled.  A value of t
indicates CRiSP mode is enabled.

Setting this variable directly does not take effect;
use either M-x customize or the function `crisp-mode'."
  :set (lambda (symbol value) (crisp-mode (if value 1 0)))
  :initialize 'custom-initialize-default
  :require 'crisp
  :version "20.4"
  :type 'boolean
  :group 'crisp)

(defcustom crisp-override-meta-x t
  "Controls overriding the normal Emacs M-x key binding in the CRiSP emulator.
Normally the CRiSP emulator rebinds M-x to `save-buffers-exit-emacs', and
provides the usual M-x functionality on the F10 key.  If this variable
is non-nil, M-x will exit Emacs."
  :type 'boolean
  :group 'crisp)

(defcustom crisp-load-scroll-all nil
  "Controls loading of the Scroll Lock in the CRiSP emulator.
Its default behavior is to load and enable the Scroll Lock minor mode
package when enabling the CRiSP emulator.

If this variable is nil when you start the CRiSP emulator, it
does not load the scroll-all package."
  :type 'boolean
  :group 'crisp)

(defcustom crisp-load-hook nil
  "Hooks to run after loading the CRiSP emulator package."
  :type 'hook
  :group 'crisp)

(defcustom crisp-mode-hook nil
  "Hook run by the function `crisp-mode'."
  :type 'hook
  :group 'crisp)

(defconst crisp-version "1.34"
  "The version of the CRiSP emulator.")

(defconst crisp-mode-help-address "gfoster@suzieq.ml.org"
  "The email address of the CRiSP mode author/maintainer.")

;; Silence the byte-compiler.
(defvar crisp-last-last-command nil
  "The previous value of `last-command'.")

;; The cut and paste routines are different between XEmacs and Emacs
;; so we need to set up aliases for the functions.

(defalias 'crisp-set-clipboard
  (if (fboundp 'clipboard-kill-ring-save)
      'clipboard-kill-ring-save
    'copy-primary-selection))

(defalias 'crisp-kill-region
  (if (fboundp 'clipboard-kill-region)
      'clipboard-kill-region
    'kill-primary-selection))

(defalias 'crisp-yank-clipboard
  (if (fboundp 'clipboard-yank)
      'clipboard-yank
    'yank-clipboard-selection))

(defun crisp-region-active ()
  "Compatibility function to test for an active region."
  (if (featurep 'xemacs)
      zmacs-region-active-p
    mark-active))

(defun crisp-version (&optional arg)
  "Version number of the CRiSP emulator package.
If ARG, insert results at point."
  (interactive "P")
  (let ((foo (concat "CRiSP version " crisp-version)))
    (if arg
	(insert (message foo))
      (message foo))))

(defun crisp-mark-line (arg)
  "Set mark at the end of the line.
Arg works as in `end-of-line'."
  (interactive "p")
  (let (newmark)
    (save-excursion
      (end-of-line arg)
      (setq newmark (point)))
    (push-mark newmark nil t)))

(defun crisp-kill-line (arg)
  "Mark and kill line(s).
Marks from point to end of the current line (honoring prefix arguments),
copies the region to the kill ring and clipboard, and then deletes it."
  (interactive "*p")
  (if (crisp-region-active)
      (call-interactively 'crisp-kill-region)
    (crisp-mark-line arg)
    (call-interactively 'crisp-kill-region)))

(defun crisp-copy-line (arg)
  "Mark and copy line(s).
Marks from point to end of the current line (honoring prefix arguments),
copies the region to the kill ring and clipboard, and then deactivates
the region."
  (interactive "*p")
    (if (crisp-region-active)
	(call-interactively 'crisp-set-clipboard)
      (crisp-mark-line arg)
      (call-interactively 'crisp-set-clipboard))
    ;; clear the region after the operation is complete
    ;; XEmacs does this automagically, Emacs doesn't.
    (if (boundp 'mark-active)
	(setq mark-active nil)))

(defun crisp-home ()
  "\"Home\" the point, the way CRiSP would do it.
The first use moves point to beginning of the line.  Second
consecutive use moves point to beginning of the screen.  Third
consecutive use moves point to the beginning of the buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'crisp-home)
	  (eq crisp-last-last-command 'crisp-home))
     (goto-char (point-min)))
    ((eq last-command 'crisp-home)
     (move-to-window-line 0))
    (t
     (beginning-of-line)))
  (setq crisp-last-last-command last-command))

(defun crisp-end ()
  "\"End\" the point, the way CRiSP would do it.
The first use moves point to end of the line.  Second
consecutive use moves point to the end of the screen.  Third
consecutive use moves point to the end of the buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'crisp-end)
	  (eq crisp-last-last-command 'crisp-end))
     (goto-char (point-max)))
    ((eq last-command 'crisp-end)
     (move-to-window-line -1)
     (end-of-line))
    (t
     (end-of-line)))
  (setq crisp-last-last-command last-command))

(defun crisp-unbury-buffer ()
  "Go back one buffer."
  (interactive)
  (switch-to-buffer (car (last (buffer-list)))))

(defun crisp-meta-x-wrapper ()
  "Wrapper function to conditionally override the normal M-x bindings.
When `crisp-override-meta-x' is non-nil, M-x will exit Emacs (the
normal CRiSP binding) and when it is nil M-x will run
`execute-extended-command' (the normal Emacs binding)."
  (interactive)
  (if crisp-override-meta-x
      (save-buffers-kill-emacs)
    (call-interactively 'execute-extended-command)))

;;;###autoload
(define-minor-mode crisp-mode
  "Toggle CRiSP/Brief emulation (CRiSP mode).
With a prefix argument ARG, enable CRiSP mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :keymap crisp-mode-map
  :lighter crisp-mode-modeline-string
  (when crisp-mode
    ;; Make menu entries show M-u or f14 in preference to C-x u.
    (put 'undo :advertised-binding
         (list* [?\M-u] [f14] (get 'undo :advertised-binding)))
    ;; Force transient-mark-mode, so that the marking routines work as
    ;; expected.  If the user turns off transient mark mode, most
    ;; things will still work fine except the crisp-(copy|kill)
    ;; functions won't work quite as nicely when regions are marked
    ;; differently and could really confuse people.  Caveat emptor.
    (if (fboundp 'transient-mark-mode)
	(transient-mark-mode t))
    (if crisp-load-scroll-all
	(require 'scroll-all))
    (if (featurep 'scroll-all)
	(define-key crisp-mode-map [(meta f1)] 'scroll-all-mode))))

;; People might use Apropos on `brief'.
;;;###autoload
(defalias 'brief-mode 'crisp-mode)

;; Interaction with other packages.
(put 'crisp-home 'CUA 'move)
(put 'crisp-end  'CUA 'move)

(run-hooks 'crisp-load-hook)
(provide 'crisp)

;;; crisp.el ends here
