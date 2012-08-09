;;; font-core.el --- Core interface to font-lock

;; Copyright (C) 1992-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: languages, faces
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

;;; Code:

;; This variable is used by mode packages that support Font Lock mode by
;; defining their own keywords to use for `font-lock-keywords'.  (The mode
;; command should make it buffer-local and set it to provide the set up.)
(defvar font-lock-defaults nil
  "Defaults for Font Lock mode specified by the major mode.
Defaults should be of the form:

 (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST [SYNTAX-BEGIN ...]]]])

KEYWORDS may be a symbol (a variable or function whose value is the keywords
to use for fontification) or a list of symbols (specifying different levels
of fontification).

If KEYWORDS-ONLY is non-nil, syntactic fontification (strings and
comments) is not performed.

If CASE-FOLD is non-nil, the case of the keywords is ignored when fontifying.

If SYNTAX-ALIST is non-nil, it should be a list of cons pairs of the form
\(CHAR-OR-STRING . STRING) used to set the local Font Lock syntax table, for
keyword and syntactic fontification (see `modify-syntax-entry').

If SYNTAX-BEGIN is non-nil, it should be a function with no args used to move
backwards outside any enclosing syntactic block, for syntactic fontification.
Typical values are `beginning-of-line' (i.e., the start of the line is known to
be outside a syntactic block), or `beginning-of-defun' for programming modes or
`backward-paragraph' for textual modes (i.e., the mode-dependent function is
known to move outside a syntactic block).  If nil, the beginning of the buffer
is used as a position outside of a syntactic block, in the worst case.

\(See also Info node `(elisp)Font Lock Basics'.)

These item elements are used by Font Lock mode to set the variables
`font-lock-keywords', `font-lock-keywords-only',
`font-lock-keywords-case-fold-search', `font-lock-syntax-table' and
`font-lock-beginning-of-syntax-function', respectively.

Further item elements are alists of the form (VARIABLE . VALUE) and are in no
particular order.  Each VARIABLE is made buffer-local before set to VALUE.

Currently, appropriate variables include `font-lock-mark-block-function'.
If this is non-nil, it should be a function with no args used to mark any
enclosing block of text, for fontification via \\[font-lock-fontify-block].
Typical values are `mark-defun' for programming modes or `mark-paragraph' for
textual modes (i.e., the mode-dependent function is known to put point and mark
around a text block relevant to that mode).

Other variables include that for syntactic keyword fontification,
`font-lock-syntactic-keywords' and those for buffer-specialized fontification
functions, `font-lock-fontify-buffer-function',
`font-lock-unfontify-buffer-function', `font-lock-fontify-region-function',
`font-lock-unfontify-region-function', and `font-lock-inhibit-thing-lock'.")
;;;###autoload
(put 'font-lock-defaults 'risky-local-variable t)
(make-variable-buffer-local 'font-lock-defaults)

(defvar font-lock-function 'font-lock-default-function
  "A function which is called when `font-lock-mode' is toggled.
It will be passed one argument, which is the current value of
`font-lock-mode'.")

;; The mode for which font-lock was initialized, or nil if none.
(defvar font-lock-major-mode)

(define-minor-mode font-lock-mode
  "Toggle syntax highlighting in this buffer (Font Lock mode).
With a prefix argument ARG, enable Font Lock mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Font Lock mode is enabled, text is fontified as you type it:

 - Comments are displayed in `font-lock-comment-face';
 - Strings are displayed in `font-lock-string-face';
 - Certain other expressions are displayed in other faces according to the
   value of the variable `font-lock-keywords'.

To customize the faces (colors, fonts, etc.) used by Font Lock for
fontifying different parts of buffer text, use \\[customize-face].

You can enable Font Lock mode in any major mode automatically by turning on in
the major mode's hook.  For example, put in your ~/.emacs:

 (add-hook 'c-mode-hook 'turn-on-font-lock)

Alternatively, you can use Global Font Lock mode to automagically turn on Font
Lock mode in buffers whose major mode supports it and whose major mode is one
of `font-lock-global-modes'.  For example, put in your ~/.emacs:

 (global-font-lock-mode t)

Where major modes support different levels of fontification, you can use
the variable `font-lock-maximum-decoration' to specify which level you
generally prefer.  When you turn Font Lock mode on/off the buffer is
fontified/defontified, though fontification occurs only if the buffer is
less than `font-lock-maximum-size'.

To add your own highlighting for some major mode, and modify the highlighting
selected automatically via the variable `font-lock-maximum-decoration', you can
use `font-lock-add-keywords'.

To fontify a buffer, without turning on Font Lock mode and regardless of buffer
size, you can use \\[font-lock-fontify-buffer].

To fontify a block (the function or paragraph containing point, or a number of
lines around point), perhaps because modification on the current line caused
syntactic change on other lines, you can use \\[font-lock-fontify-block].

You can set your own default settings for some mode, by setting a
buffer local value for `font-lock-defaults', via its mode hook.

The above is the default behavior of `font-lock-mode'; you may specify
your own function which is called when `font-lock-mode' is toggled via
`font-lock-function'. "
  nil nil nil
  :after-hook (font-lock-initial-fontify)
  ;; Don't turn on Font Lock mode if we don't have a display (we're running a
  ;; batch job) or if the buffer is invisible (the name starts with a space).
  (when (or noninteractive (eq (aref (buffer-name) 0) ?\s))
    (setq font-lock-mode nil))
  (funcall font-lock-function font-lock-mode)
  ;; Arrange to unfontify this buffer if we change major mode later.
  (if font-lock-mode
      (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t)
    (remove-hook 'change-major-mode-hook 'font-lock-change-mode t)))

;; Get rid of fontification for the old major mode.
;; We do this when changing major modes.
(defun font-lock-change-mode ()
  (font-lock-mode -1))

(defun font-lock-defontify ()
  "Clear out all `font-lock-face' properties in current buffer.
A major mode that uses `font-lock-face' properties might want to put
this function onto `change-major-mode-hook'."
  (let ((modp (buffer-modified-p))
	(inhibit-read-only t))
    (save-restriction
      (widen)
      (remove-list-of-text-properties (point-min) (point-max)
				      '(font-lock-face)))
    (restore-buffer-modified-p modp)))

(defvar font-lock-set-defaults)
(defun font-lock-default-function (mode)
  ;; Turn on Font Lock mode.
  (when mode
    (set (make-local-variable 'char-property-alias-alist)
	 (copy-tree char-property-alias-alist))
    ;; Add `font-lock-face' as an alias for the `face' property.
    (let ((elt (assq 'face char-property-alias-alist)))
      (if elt
	  (unless (memq 'font-lock-face (cdr elt))
	    (setcdr elt (nconc (cdr elt) (list 'font-lock-face))))
	(push (list 'face 'font-lock-face) char-property-alias-alist))))
  ;; Turn off Font Lock mode.
  (unless mode
    ;; Remove `font-lock-face' as an alias for the `face' property.
    (set (make-local-variable 'char-property-alias-alist)
	 (copy-tree char-property-alias-alist))
    (let ((elt (assq 'face char-property-alias-alist)))
      (when elt
	(setcdr elt (remq 'font-lock-face (cdr elt)))
	(when (null (cdr elt))
	  (setq char-property-alias-alist
		(delq elt char-property-alias-alist))))))

  ;; Only do hard work if the mode has specified stuff in
  ;; `font-lock-defaults'.
  (when (font-lock-specified-p mode)
    (font-lock-mode-internal mode)))

(defun turn-on-font-lock ()
  "Turn on Font Lock mode (only if the terminal can display it)."
  (unless font-lock-mode
    (font-lock-mode)))

;;; Global Font Lock mode.

;; A few people have hassled in the past for a way to make it easier to turn on
;; Font Lock mode, without the user needing to know for which modes s/he has to
;; turn it on, perhaps the same way hilit19.el/hl319.el does.  I've always
;; balked at that way, as I see it as just re-molding the same problem in
;; another form.  That is; some person would still have to keep track of which
;; modes (which may not even be distributed with Emacs) support Font Lock mode.
;; The list would always be out of date.  And that person might have to be me.

;; Implementation.
;;
;; In a previous discussion the following hack came to mind.  It is a gross
;; hack, but it generally works.  We use the convention that major modes start
;; by calling the function `kill-all-local-variables', which in turn runs
;; functions on the hook variable `change-major-mode-hook'.  We attach our
;; function `font-lock-change-major-mode' to that hook.  Of course, when this
;; hook is run, the major mode is in the process of being changed and we do not
;; know what the final major mode will be.  So, `font-lock-change-major-mode'
;; only (a) notes the name of the current buffer, and (b) adds our function
;; `turn-on-font-lock-if-desired' to the hook variables
;; `after-change-major-mode-hook' and `post-command-hook' (for modes
;; that do not yet run `after-change-major-mode-hook').  By the time
;; the functions on the first of these hooks to be run are run, the new major
;; mode is assumed to be in place.  This way we get a Font Lock function run
;; when a major mode is turned on, without knowing major modes or their hooks.
;;
;; Naturally this requires that major modes run `kill-all-local-variables'
;; and `after-change-major-mode-hook', as they are supposed to.  For modes
;; that do not run `after-change-major-mode-hook' yet, `post-command-hook'
;; takes care of things if the mode is set directly or indirectly by
;; an interactive command; however, problems can occur if the mode is
;; set by a timer or process: in that case, proper handling of Font Lock mode
;; may be delayed until the next interactive command.

;; User interface.
;;
;; Although Global Font Lock mode is a pseudo-mode, I think that the user
;; interface should conform to the usual Emacs convention for modes, i.e., a
;; command to toggle the feature (`global-font-lock-mode') with a variable for
;; finer control of the mode's behavior (`font-lock-global-modes').
;;
;; The feature should not be enabled by loading font-lock.el, since other
;; mechanisms for turning on Font Lock mode, such as M-x font-lock-mode RET or
;; (add-hook 'c-mode-hook 'turn-on-font-lock), would cause Font Lock mode to be
;; turned on everywhere.  That would not be intuitive or informative because
;; loading a file tells you nothing about the feature or how to control it.  It
;; would also be contrary to the Principle of Least Surprise.  sm.

(defcustom font-lock-global-modes t
  "Modes for which Font Lock mode is automagically turned on.
Global Font Lock mode is controlled by the command `global-font-lock-mode'.
If nil, means no modes have Font Lock mode automatically turned on.
If t, all modes that support Font Lock mode have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which Font Lock
mode should be automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that Font Lock mode is turned on for buffers in C and C++ modes only."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'font-lock)

(defun turn-on-font-lock-if-desired ()
  (when (cond ((eq font-lock-global-modes t)
	       t)
	      ((eq (car-safe font-lock-global-modes) 'not)
	       (not (memq major-mode (cdr font-lock-global-modes))))
	      (t (memq major-mode font-lock-global-modes)))
    (let (inhibit-quit)
      (turn-on-font-lock))))

(define-globalized-minor-mode global-font-lock-mode
  font-lock-mode turn-on-font-lock-if-desired
  ;; What was this :extra-args thingy for?  --Stef
  ;; :extra-args (dummy)
  :initialize 'custom-initialize-delay
  :init-value (not (or noninteractive emacs-basic-display))
  :group 'font-lock
  :version "22.1")

;;; End of Global Font Lock mode.

(provide 'font-core)

;;; font-core.el ends here
