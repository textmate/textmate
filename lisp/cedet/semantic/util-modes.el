;;; semantic/util-modes.el --- Semantic minor modes

;; Copyright (C) 2000-2005, 2007-2012  Free Software Foundation, Inc.

;; Authors: Eric M. Ludlam <zappo@gnu.org>
;;          David Ponce <david@dponce.com>
;; Keywords: syntax

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
;;
;;  Semantic utility minor modes.
;;

;;; Code:

;; FIXME: compiling util-modes.el seems to require loading util-modes.el,
;; so if the previous compilation generated a file that fails to load,
;; recompiling fails to fix the problem.
(require 'semantic)

;;; Group for all semantic enhancing modes
(defgroup semantic-modes nil
  "Minor modes associated with the Semantic architecture."
  :group 'semantic)

;;;;
;;;; Semantic minor modes stuff
;;;;
(defcustom semantic-update-mode-line t
  "If non-nil, show enabled minor modes in the mode line.
Only minor modes that are not turned on globally are shown in the mode
line."
  :group 'semantic
  :type 'boolean
  :require 'semantic/util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         ;; Update status of all Semantic enabled buffers
         (semantic-mode-line-update)))

(defcustom semantic-mode-line-prefix
  (propertize "S" 'face 'bold)
  "Prefix added to minor mode indicators in the mode line."
  :group 'semantic
  :type 'string
  :require 'semantic/util-modes
  :initialize 'custom-initialize-default)

(defvar semantic-minor-modes-format nil
  "Mode line format showing Semantic minor modes which are locally enabled.
It is displayed in the mode line.")
(put 'semantic-minor-modes-format 'risky-local-variable t)

(defvar semantic-minor-mode-alist nil
  "Alist saying how to show Semantic minor modes in the mode line.
Like variable `minor-mode-alist'.")

(defun semantic-mode-line-update ()
  "Update mode line format of Semantic minor modes.
Only minor modes that are locally enabled are shown in the mode line."
  (setq semantic-minor-modes-format nil)
  (dolist (x semantic-minor-mode-alist)
    (setq minor-mode-alist (delq (assq (car x) minor-mode-alist)
                                 minor-mode-alist)))
  (when semantic-update-mode-line
    (let ((locals '()))
      ;; Select the minor modes that aren't enabled globally and who
      ;; have a non-empty "name".
      (dolist (x semantic-minor-mode-alist)
        (unless (or (memq (car x) semantic-init-hook)
                    (not (string-match "^[ ]*\\(.+\\)" (cadr x))))
          (push (list (car x) (concat "/" (match-string 1 (cadr x)))) locals)))
      ;; Then build the format spec.
      (when locals
        (let ((prefix (if (string-match "^[ ]*\\(.+\\)"
                                        semantic-mode-line-prefix)
                          (match-string 1 semantic-mode-line-prefix)
                        "S")))
          (setq semantic-minor-modes-format
                `((:eval (if (or ,@(mapcar 'car locals))
                             ,(concat " " prefix)))))
          ;; It would be easier to just put `locals' inside
          ;; semantic-minor-modes-format, but then things like
          ;; mode-line-minor-mode-help can't find the right major mode
          ;; any more.  So instead, we carefully put the minor modes
          ;; in minor-mode-alist.
          (let* ((elem (or (assq 'semantic-minor-modes-format
                                 minor-mode-alist)
                           ;; FIXME: This entry is meaningless for
                           ;; mode-line-minor-mode-help.
                           '(semantic-minor-modes-format
                           semantic-minor-modes-format)))
                 (tail (or (memq elem minor-mode-alist)
                           (setq minor-mode-alist
                                 (cons elem minor-mode-alist)))))
            (setcdr tail (nconc locals (cdr tail)))))))))

(defun semantic-desktop-ignore-this-minor-mode (buffer)
  "Installed as a minor-mode initializer for Desktop mode.
BUFFER is the buffer to not initialize a Semantic minor mode in."
  nil)

(defun semantic-add-minor-mode (toggle name)
  "Register a new Semantic minor mode.
TOGGLE is a symbol which is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.
It is also an interactive function to toggle the mode.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string."
  ;; Record how to display this minor mode in the mode line
  (let ((mm (assq toggle semantic-minor-mode-alist)))
    (if mm
        (setcdr mm (list name))
      (setq semantic-minor-mode-alist (cons (list toggle name)
                                       semantic-minor-mode-alist))))
  (semantic-mode-line-update)

  ;; Semantic minor modes don't work w/ Desktop restore.
  ;; This line will disable this minor mode from being restored
  ;; by Desktop.
  (when (boundp 'desktop-minor-mode-handlers)
    (add-to-list 'desktop-minor-mode-handlers
		 (cons toggle 'semantic-desktop-ignore-this-minor-mode))))

(defun semantic-toggle-minor-mode-globally (mode &optional arg)
  "Toggle minor mode MODE in every Semantic enabled buffer.
Return non-nil if MODE is turned on in every Semantic enabled buffer.
If ARG is positive, enable, if it is negative, disable.
MODE must be a valid minor mode defined in `minor-mode-alist' and must be
too an interactive function used to toggle the mode."
  ;; FIXME: All callers should pass a -1 or +1 argument.
  (or (and (fboundp mode) (or (assq mode minor-mode-alist) ;Needed?
			      (assq mode semantic-minor-mode-alist)))
      (error "Semantic minor mode %s not found" mode))
  ;; Add or remove the MODE toggle function from `semantic-init-hook'.
  (cond
   ;; Turn off if ARG < 0
   ((< arg 0) (remove-hook 'semantic-init-hook mode))
   ;; Turn on if ARG > 0
   ((> arg 0) (add-hook 'semantic-init-hook mode))
   ;; Otherwise just check MODE state
   (t
    (error "semantic-toggle-minor-mode-globally: arg should be -1 or 1")))
  ;; Update the minor mode format.
  (semantic-mode-line-update)
  ;; Then turn MODE on or off in every Semantic enabled buffer.
  (semantic-map-buffers #'(lambda () (funcall mode arg))))

;;;;
;;;; Minor mode to highlight areas that a user edits.
;;;;

;;;###autoload
(define-minor-mode global-semantic-highlight-edits-mode
  "Toggle global use of option `semantic-highlight-edits-mode'.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'semantic :group 'semantic-modes
  (semantic-toggle-minor-mode-globally
   'semantic-highlight-edits-mode
   (if global-semantic-highlight-edits-mode 1 -1)))

(defcustom semantic-highlight-edits-mode-hook nil
  "Hook run at the end of function `semantic-highlight-edits-mode'."
  :group 'semantic
  :type 'hook)

(defface semantic-highlight-edits-face
  '((((class color) (background dark))
     ;; Put this back to something closer to black later.
     (:background "gray20"))
    (((class color) (background light))
     (:background "gray90")))
  "Face used to show dirty tokens in `semantic-highlight-edits-mode'."
  :group 'semantic-faces)

(defun semantic-highlight-edits-new-change-hook-fcn (overlay)
  "Function set into `semantic-edits-new-change-hook'.
Argument OVERLAY is the overlay created to mark the change.
This function will set the face property on this overlay."
  (semantic-overlay-put overlay 'face 'semantic-highlight-edits-face))

(defvar semantic-highlight-edits-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for highlight-edits minor mode.")

;;;###autoload
(define-minor-mode semantic-highlight-edits-mode
  "Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
This mode will highlight those changes as they are made, and clear them
when the incremental parser accounts for those edits.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  :keymap semantic-highlight-edits-mode-map
  (if semantic-highlight-edits-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
	  (progn
	    ;; Disable minor mode if semantic stuff not available
	    (setq semantic-highlight-edits-mode nil)
	    (error "Buffer %s was not set up for parsing"
		   (buffer-name)))
	(semantic-make-local-hook 'semantic-edits-new-change-hooks)
	(add-hook 'semantic-edits-new-change-hooks
		  'semantic-highlight-edits-new-change-hook-fcn nil t))
    ;; Remove hooks
    (remove-hook 'semantic-edits-new-change-hooks
		 'semantic-highlight-edits-new-change-hook-fcn t)))

(semantic-add-minor-mode 'semantic-highlight-edits-mode
                         "e")

;;;;
;;;; Minor mode to show unmatched-syntax elements
;;;;

;;;###autoload
(define-minor-mode global-semantic-show-unmatched-syntax-mode
  "Toggle global use of option `semantic-show-unmatched-syntax-mode'.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'semantic :group 'semantic-modes
  ;; Not needed because it's autoloaded instead.
  ;; :require 'semantic/util-modes
  (semantic-toggle-minor-mode-globally
   'semantic-show-unmatched-syntax-mode
   (if global-semantic-show-unmatched-syntax-mode 1 -1)))

(defcustom semantic-show-unmatched-syntax-mode-hook nil
  "Hook run at the end of function `semantic-show-unmatched-syntax-mode'."
  :group 'semantic
  :type 'hook)

(defface semantic-unmatched-syntax-face
  '((((class color) (background dark))
     (:underline "red"))
    (((class color) (background light))
     (:underline "red")))
  "Face used to show unmatched syntax in.
The face is used in `semantic-show-unmatched-syntax-mode'."
  :group 'semantic-faces)

(defsubst semantic-unmatched-syntax-overlay-p (overlay)
  "Return non-nil if OVERLAY is an unmatched syntax one."
  (eq (semantic-overlay-get overlay 'semantic) 'unmatched))

(defun semantic-showing-unmatched-syntax-p ()
  "Return non-nil if an unmatched syntax overlay was found in buffer."
  (let ((ol (semantic-overlays-in (point-min) (point-max)))
        found)
    (while (and ol (not found))
      (setq found (semantic-unmatched-syntax-overlay-p (car ol))
            ol    (cdr ol)))
    found))

(defun semantic-show-unmatched-lex-tokens-fetch ()
  "Fetch a list of unmatched lexical tokens from the current buffer.
Uses the overlays which have accurate bounds, and rebuilds what was
originally passed in."
  (let ((ol (semantic-overlays-in (point-min) (point-max)))
	(ustc nil))
    (while ol
      (if (semantic-unmatched-syntax-overlay-p (car ol))
	  (setq ustc (cons (cons 'thing
				 (cons (semantic-overlay-start (car ol))
				       (semantic-overlay-end (car ol))))
			   ustc)))
      (setq ol (cdr ol)))
    (nreverse ustc))
  )

(defun semantic-clean-unmatched-syntax-in-region (beg end)
  "Remove all unmatched syntax overlays between BEG and END."
  (let ((ol (semantic-overlays-in beg end)))
    (while ol
      (if (semantic-unmatched-syntax-overlay-p (car ol))
	  (semantic-overlay-delete (car ol)))
      (setq ol (cdr ol)))))

(defsubst semantic-clean-unmatched-syntax-in-buffer ()
  "Remove all unmatched syntax overlays found in current buffer."
  (semantic-clean-unmatched-syntax-in-region
   (point-min) (point-max)))

(defsubst semantic-clean-token-of-unmatched-syntax (token)
  "Clean the area covered by TOKEN of unmatched syntax markers."
  (semantic-clean-unmatched-syntax-in-region
   (semantic-tag-start token) (semantic-tag-end token)))

(defun semantic-show-unmatched-syntax (syntax)
  "Function set into `semantic-unmatched-syntax-hook'.
This will highlight elements in SYNTAX as unmatched syntax."
  ;; This is called when `semantic-show-unmatched-syntax-mode' is
  ;; enabled.  Highlight the unmatched syntax, and then add a semantic
  ;; property to that overlay so we can add it to the official list of
  ;; semantic supported overlays.  This gets it cleaned up for errors,
  ;; buffer cleaning, and the like.
  (semantic-clean-unmatched-syntax-in-buffer) ;Clear previous highlighting
  (if syntax
      (let (o)
        (while syntax
          (setq o (semantic-make-overlay (semantic-lex-token-start (car syntax))
                                         (semantic-lex-token-end (car syntax))))
          (semantic-overlay-put o 'semantic 'unmatched)
          (semantic-overlay-put o 'face 'semantic-unmatched-syntax-face)
          (setq syntax (cdr syntax))))
    ))

(defun semantic-next-unmatched-syntax (point &optional bound)
  "Find the next overlay for unmatched syntax after POINT.
Do not search past BOUND if non-nil."
  (save-excursion
    (goto-char point)
    (let ((os point) (ol nil))
      (while (and os (< os (or bound (point-max))) (not ol))
	(setq os (semantic-overlay-next-change os))
	(when os
	  ;; Get overlays at position
	  (setq ol (semantic-overlays-at os))
	  ;; find the overlay that belongs to semantic
	  ;; and starts at the found position.
	  (while (and ol (listp ol))
	    (and (semantic-unmatched-syntax-overlay-p (car ol))
                 (setq ol (car ol)))
	    (if (listp ol)
                (setq ol (cdr ol))))))
      ol)))

(defvar semantic-show-unmatched-syntax-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-c,`" 'semantic-show-unmatched-syntax-next)
    km)
  "Keymap for command `semantic-show-unmatched-syntax-mode'.")

;;;###autoload
(define-minor-mode semantic-show-unmatched-syntax-mode
  "Minor mode to highlight unmatched lexical syntax tokens.
When a parser executes, some elements in the buffer may not match any
parser rules.  These text characters are considered unmatched syntax.
Often time, the display of unmatched syntax can expose coding
problems before the compiler is run.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-unmatched-syntax-mode-map}"
  :keymap semantic-show-unmatched-syntax-mode-map
  (if semantic-show-unmatched-syntax-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-show-unmatched-syntax-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Add hooks
        (semantic-make-local-hook 'semantic-unmatched-syntax-hook)
        (add-hook 'semantic-unmatched-syntax-hook
                  'semantic-show-unmatched-syntax nil t)
	(semantic-make-local-hook 'semantic-pre-clean-token-hooks)
	(add-hook 'semantic-pre-clean-token-hooks
		  'semantic-clean-token-of-unmatched-syntax nil t)
        ;; Show unmatched syntax elements
	(if (not (semantic--umatched-syntax-needs-refresh-p))
	    (semantic-show-unmatched-syntax
	     (semantic-unmatched-syntax-tokens))))
    ;; Remove hooks
    (remove-hook 'semantic-unmatched-syntax-hook
                 'semantic-show-unmatched-syntax t)
    (remove-hook 'semantic-pre-clean-token-hooks
		 'semantic-clean-token-of-unmatched-syntax t)
    ;; Cleanup unmatched-syntax highlighting
    (semantic-clean-unmatched-syntax-in-buffer)))

(semantic-add-minor-mode 'semantic-show-unmatched-syntax-mode
                         "u")

(defun semantic-show-unmatched-syntax-next ()
  "Move forward to the next occurrence of unmatched syntax."
  (interactive)
  (let ((o (semantic-next-unmatched-syntax (point))))
    (if o
	(goto-char (semantic-overlay-start o)))))


;;;;
;;;; Minor mode to display the parser state in the modeline.
;;;;

;;;###autoload
(define-minor-mode global-semantic-show-parser-state-mode
  "Toggle global use of option `semantic-show-parser-state-mode'.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'semantic
  ;; Not needed because it's autoloaded instead.
  ;; :require 'semantic/util-modes
  (semantic-toggle-minor-mode-globally
   'semantic-show-parser-state-mode
   (if global-semantic-show-parser-state-mode 1 -1)))

(defcustom semantic-show-parser-state-mode-hook nil
  "Hook run at the end of function `semantic-show-parser-state-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-show-parser-state-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for show-parser-state minor mode.")

;;;###autoload
(define-minor-mode semantic-show-parser-state-mode
  "Minor mode for displaying parser cache state in the modeline.
The cache can be in one of three states.  They are
Up to date, Partial reparse needed, and Full reparse needed.
The state is indicated in the modeline with the following characters:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `%'  ->  The cache is not currently parsable.
 `@'  ->  Auto-parse in progress (not set here.)
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  :keymap semantic-show-parser-state-mode-map
  (if semantic-show-parser-state-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-show-parser-state-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
	;; Set up mode line

	(when (not
	       (memq 'semantic-show-parser-state-string mode-line-modified))
	  (setq mode-line-modified
		(append mode-line-modified
			'(semantic-show-parser-state-string))))
	;; Add hooks
        (semantic-make-local-hook 'semantic-edits-new-change-hooks)
        (add-hook 'semantic-edits-new-change-hooks
                  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-edits-incremental-reparse-failed-hook)
	(add-hook 'semantic-edits-incremental-reparse-failed-hook
		  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
	(add-hook 'semantic-after-partial-cache-change-hook
		  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
	(add-hook 'semantic-after-toplevel-cache-change-hook
		  'semantic-show-parser-state-marker nil t)
	(semantic-show-parser-state-marker)

	(semantic-make-local-hook 'semantic-before-auto-parse-hooks)
	(add-hook 'semantic-before-auto-parse-hooks
		  'semantic-show-parser-state-auto-marker nil t)
	(semantic-make-local-hook 'semantic-after-auto-parse-hooks)
	(add-hook 'semantic-after-auto-parse-hooks
		  'semantic-show-parser-state-marker nil t)

	(semantic-make-local-hook 'semantic-before-idle-scheduler-reparse-hook)
	(add-hook 'semantic-before-idle-scheduler-reparse-hook
		  'semantic-show-parser-state-auto-marker nil t)
	(semantic-make-local-hook 'semantic-after-idle-scheduler-reparse-hook)
	(add-hook 'semantic-after-idle-scheduler-reparse-hook
		  'semantic-show-parser-state-marker nil t))
    ;; Remove parts of mode line
    (setq mode-line-modified
	  (delq 'semantic-show-parser-state-string mode-line-modified))
    ;; Remove hooks
    (remove-hook 'semantic-edits-new-change-hooks
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-edits-incremental-reparse-failed-hook
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-after-partial-cache-change-hook
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-after-toplevel-cache-change-hook
		 'semantic-show-parser-state-marker t)

    (remove-hook 'semantic-before-auto-parse-hooks
		 'semantic-show-parser-state-auto-marker t)
    (remove-hook 'semantic-after-auto-parse-hooks
		 'semantic-show-parser-state-marker t)

    (remove-hook 'semantic-before-idle-scheduler-reparse-hook
		 'semantic-show-parser-state-auto-marker t)
    (remove-hook 'semantic-after-idle-scheduler-reparse-hook
		 'semantic-show-parser-state-marker t)))

(semantic-add-minor-mode 'semantic-show-parser-state-mode
                         "")

(defvar semantic-show-parser-state-string nil
  "String showing the parser state for this buffer.
See `semantic-show-parser-state-marker' for details.")
(make-variable-buffer-local 'semantic-show-parser-state-string)

(defun semantic-show-parser-state-marker (&rest ignore)
  "Set `semantic-show-parser-state-string' to indicate parser state.
This marker is one of the following:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `%'  ->  The cache is not currently parsable.
 `@'  ->  Auto-parse in progress (not set here.)
Arguments IGNORE are ignored, and accepted so this can be used as a hook
in many situations."
  (setq semantic-show-parser-state-string
	(cond ((semantic-parse-tree-needs-rebuild-p)
	       "!")
	      ((semantic-parse-tree-needs-update-p)
	       "^")
	      ((semantic-parse-tree-unparseable-p)
	       "%")
	      (t
               "-")))
  ;;(message "Setup mode line indicator to [%s]" semantic-show-parser-state-string)
  )

(defun semantic-show-parser-state-auto-marker ()
  "Hook function run before an autoparse.
Set up `semantic-show-parser-state-marker' to show `@'
to indicate a parse in progress."
  (unless (semantic-parse-tree-up-to-date-p)
    (setq semantic-show-parser-state-string "@")
    ;; For testing.
    ;;(sit-for 1)
    ))


;;;;
;;;; Minor mode to make function decls sticky.
;;;;

;;;###autoload
(define-minor-mode global-semantic-stickyfunc-mode
  "Toggle global use of option `semantic-stickyfunc-mode'.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'semantic :group 'semantic-modes
  ;; Not needed because it's autoloaded instead.
  ;; :require 'semantic/util-modes
  (semantic-toggle-minor-mode-globally
   'semantic-stickyfunc-mode (if global-semantic-stickyfunc-mode 1 -1)))

(defcustom semantic-stickyfunc-mode-hook nil
  "Hook run at the end of function `semantic-stickyfunc-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-stickyfunc-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [ header-line down-mouse-1 ] 'semantic-stickyfunc-menu)
    km)
  "Keymap for stickyfunc minor mode.")

(defvar semantic-stickyfunc-popup-menu nil
  "Menu used if the user clicks on the header line used by stickyfunc mode.")

(easy-menu-define
  semantic-stickyfunc-popup-menu
  semantic-stickyfunc-mode-map
  "Stickyfunc Menu"
  '("Stickyfunc Mode"  :visible (progn nil)
    [ "Copy Headerline Tag" senator-copy-tag
      :active (semantic-current-tag)
      :help "Copy the current tag to the tag ring"]
    [ "Kill Headerline Tag" senator-kill-tag
      :active (semantic-current-tag)
      :help "Kill tag text to the kill ring, and copy the tag to the tag ring"
      ]
    [ "Copy Headerline Tag to Register" senator-copy-tag-to-register
      :active (semantic-current-tag)
      :help "Copy the current tag to a register"
      ]
    [ "Narrow To Headerline Tag" senator-narrow-to-defun
      :active (semantic-current-tag)
      :help "Narrow to the bounds of the current tag"]
    [ "Fold Headerline Tag" senator-fold-tag-toggle
      :active (semantic-current-tag)
      :style toggle
      :selected (let ((tag (semantic-current-tag)))
		  (and tag (semantic-tag-folded-p tag)))
      :help "Fold the current tag to one line"
      ]
    "---"
    [ "About This Header Line"
      (lambda () (interactive)
	(describe-function 'semantic-stickyfunc-mode)) t])
  )

(defcustom semantic-stickyfunc-indent-string
  (if (and window-system (not (featurep 'xemacs)))
      (concat
       (condition-case nil
	   ;; Test scroll bar location
	   (let ((charwidth (frame-char-width))
		 (scrollpos (frame-parameter (selected-frame)
					     'vertical-scroll-bars))
		 )
	     (if (or (eq scrollpos 'left)
		     ;; Now wait a minute.  If you turn scroll-bar-mode
		     ;; on, then off, the new value is t, not left.
		     ;; Will this mess up older emacs where the default
		     ;; was on the right?  I don't think so since they don't
		     ;; support a header line.
		     (eq scrollpos t))
		 (let ((w (when (boundp 'scroll-bar-width)
			    (symbol-value 'scroll-bar-width))))

		   (if (not w)
		       (setq w (frame-parameter (selected-frame)
						'scroll-bar-width)))

		   ;; in 21.2, the frame parameter is sometimes empty
		   ;; so we need to get the value here.
		   (if (not w)
		       (setq w (+ (get 'scroll-bar-width 'x-frame-parameter)
				  ;; In 21.4, or perhaps 22.1 the x-frame
				  ;; parameter is different from the frame
				  ;; parameter by only 1 pixel.
				  1)))

		   (if (not w)
		       "  "
		     (setq w (+ 2 w))   ; Some sort of border around
					; the scrollbar.
		     (make-string (/ w charwidth) ? )))
	       ""))
	 (error ""))
       (condition-case nil
	   ;; Test fringe size.
	   (let* ((f (window-fringes))
		  (fw (car f))
		  (numspace (/ fw (frame-char-width)))
		  )
	     (make-string numspace ? ))
	 (error
	  ;; Well, the fancy new Emacs functions failed.  Try older
	  ;; tricks.
	  (condition-case nil
	      ;; I'm not so sure what's up with the 21.1-21.3 fringe.
	      ;; It looks to be about 1 space wide.
	      (if (get 'fringe 'face)
		  " "
		"")
	    (error ""))))
       )
    ;; Not Emacs or a window system means no scrollbar or fringe,
    ;; and perhaps not even a header line to worry about.
    "")
  "String used to indent the stickyfunc header.
Customize this string to match the space used by scrollbars and
fringe so it does not appear that the code is moving left/right
when it lands in the sticky line."
  :group 'semantic
  :type 'string)

(defvar semantic-stickyfunc-old-hlf nil
  "Value of the header line when entering stickyfunc mode.")

(defconst semantic-stickyfunc-header-line-format
  (cond ((featurep 'xemacs)
	 nil)
	((>= emacs-major-version 22)
	 '(:eval (list
		  ;; Magic bit I found on emacswiki.
		  (propertize " " 'display '((space :align-to 0)))
		  (semantic-stickyfunc-fetch-stickyline))))
	((= emacs-major-version 21)
	 '(:eval (list semantic-stickyfunc-indent-string
		       (semantic-stickyfunc-fetch-stickyline))))
	(t nil))
  "The header line format used by stickyfunc mode.")

;;;###autoload
(define-minor-mode semantic-stickyfunc-mode
  "Minor mode to show the title of a tag in the header line.
Enables/disables making the header line of functions sticky.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') has a header line, meaning the
first line which describes the rest of the construct.  This first
line is what is displayed in the header line.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  ;; Don't need indicator.  It's quite visible
  :keymap semantic-stickyfunc-mode-map
  (if semantic-stickyfunc-mode
      (progn
	(unless (and (featurep 'semantic) (semantic-active-p))
	  ;; Disable minor mode if semantic stuff not available
	  (setq semantic-stickyfunc-mode nil)
	  (error "Buffer %s was not set up for parsing" (buffer-name)))
	(unless (boundp 'default-header-line-format)
	  ;; Disable if there are no header lines to use.
	  (setq semantic-stickyfunc-mode nil)
	  (error "Sticky Function mode requires Emacs 21"))
	;; Enable the mode
	;; Save previous buffer local value of header line format.
	(when (and (local-variable-p 'header-line-format (current-buffer))
		   (not (eq header-line-format
			    semantic-stickyfunc-header-line-format)))
	  (set (make-local-variable 'semantic-stickyfunc-old-hlf)
	       header-line-format))
	(setq header-line-format semantic-stickyfunc-header-line-format))
    ;; Disable sticky func mode
    ;; Restore previous buffer local value of header line format if
    ;; the current one is the sticky func one.
    (when (eq header-line-format semantic-stickyfunc-header-line-format)
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'semantic-stickyfunc-old-hlf (current-buffer))
	(setq header-line-format semantic-stickyfunc-old-hlf)
	(kill-local-variable 'semantic-stickyfunc-old-hlf)))))

(defvar semantic-stickyfunc-sticky-classes
  '(function type)
  "List of tag classes which stickyfunc will display in the header line.")
(make-variable-buffer-local 'semantic-stickyfunc-sticky-classes)

(defcustom semantic-stickyfunc-show-only-functions-p nil
  "Non-nil means don't show lines that aren't part of a tag.
If this is nil, then comments or other text between tags that is
1 line above the top of the current window will be shown."
  :group 'semantic
  :type 'boolean)

(defun semantic-stickyfunc-tag-to-stick ()
  "Return the tag to stick at the current point."
  (let ((tags (nreverse (semantic-find-tag-by-overlay (point)))))
    ;; Get rid of non-matching tags.
    (while (and tags
		(not (member
		      (semantic-tag-class (car tags))
		      semantic-stickyfunc-sticky-classes))
		)
      (setq tags (cdr tags)))
    (car tags)))

(defun semantic-stickyfunc-fetch-stickyline ()
  "Make the function at the top of the current window sticky.
Capture its function declaration, and place it in the header line.
If there is no function, disable the header line."
  (save-excursion
    (goto-char (window-start (selected-window)))
    (let* ((noshow (bobp))
	   (str
	    (progn
	      (forward-line -1)
	      (end-of-line)
	      ;; Capture this function
	      (let* ((tag (semantic-stickyfunc-tag-to-stick)))
		;; TAG is nil if there was nothing of the appropriate type there.
		(if (not tag)
		    ;; Set it to be the text under the header line
		    (if noshow
			""
		      (if semantic-stickyfunc-show-only-functions-p ""
			(buffer-substring (point-at-bol) (point-at-eol))
			))
		  ;; Go get the first line of this tag.
		  (goto-char (semantic-tag-start tag))
		  ;; Klaus Berndl <klaus.berndl@sdm.de>:
		  ;; goto the tag name; this is especially needed for languages
		  ;; like c++ where a often used style is like:
		  ;;     void
		  ;;     ClassX::methodM(arg1...)
		  ;;     {
		  ;;       ...
		  ;;     }
		  ;; Without going to the tag-name we would get"void" in the
		  ;; header line which is IMHO not really useful
		  (search-forward (semantic-tag-name tag) nil t)
		  (buffer-substring (point-at-bol) (point-at-eol))
		  ))))
	   (start 0))
      (while (string-match "%" str start)
	(setq str (replace-match "%%" t t str 0)
	      start (1+ (match-end 0)))
	)
      ;; In 21.4 (or 22.1) the header doesn't expand tabs.  Hmmmm.
      ;; We should replace them here.
      ;;
      ;; This hack assumes that tabs are kept smartly at tab boundaries
      ;; instead of in a tab boundary where it might only represent 4 spaces.
      (while (string-match "\t" str start)
	(setq str (replace-match "        " t t str 0)))
      str)))

(defun semantic-stickyfunc-menu (event)
  "Popup a menu that can help a user understand stickyfunc-mode.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (car (car (cdr event))))
	 )
    (select-window win t)
    (save-excursion
      (goto-char (window-start win))
      (sit-for 0)
      (popup-menu semantic-stickyfunc-popup-menu event)
      )
    (select-window startwin)))


(semantic-add-minor-mode 'semantic-stickyfunc-mode
                         "") ;; Don't need indicator.  It's quite visible



;;;;
;;;; Minor mode to make highlight the current function
;;;;

;; Highlight the first like of the function we are in if it is different
;; from the tag going off the top of the screen.

;;;###autoload
(define-minor-mode global-semantic-highlight-func-mode
  "Toggle global use of option `semantic-highlight-func-mode'.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'semantic :group 'semantic-modes
  ;; Not needed because it's autoloaded instead.
  ;; :require 'semantic/util-modes
  (semantic-toggle-minor-mode-globally
   'semantic-highlight-func-mode
   (if global-semantic-highlight-func-mode 1 -1)))

(defcustom semantic-highlight-func-mode-hook nil
  "Hook run at the end of function `semantic-highlight-func-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-highlight-func-mode-map
  (let ((km (make-sparse-keymap))
	(m3  (if (featurep 'xemacs) [ button3 ] [ mouse-3 ]))
	)
    (define-key km m3 'semantic-highlight-func-menu)
    km)
  "Keymap for highlight-func minor mode.")

(defvar semantic-highlight-func-popup-menu nil
  "Menu used if the user clicks on the header line used by `semantic-highlight-func-mode'.")

(easy-menu-define
  semantic-highlight-func-popup-menu
  semantic-highlight-func-mode-map
  "Highlight-Func Menu"
  '("Highlight-Func Mode"  :visible (progn nil)
    [ "Copy Tag" senator-copy-tag
      :active (semantic-current-tag)
      :help "Copy the current tag to the tag ring"]
    [ "Kill Tag" senator-kill-tag
      :active (semantic-current-tag)
      :help "Kill tag text to the kill ring, and copy the tag to the tag ring"
      ]
    [ "Copy Tag to Register" senator-copy-tag-to-register
      :active (semantic-current-tag)
      :help "Copy the current tag to a register"
      ]
    [ "Narrow To Tag" senator-narrow-to-defun
      :active (semantic-current-tag)
      :help "Narrow to the bounds of the current tag"]
    [ "Fold Tag" senator-fold-tag-toggle
      :active (semantic-current-tag)
      :style toggle
      :selected (let ((tag (semantic-stickyfunc-tag-to-stick)))
		  (and tag (semantic-tag-folded-p tag)))
      :help "Fold the current tag to one line"
      ]
    "---"
    [ "About This Tag" semantic-describe-tag t])
  )

(defun semantic-highlight-func-menu (event)
  "Popup a menu that displays things to do to the current tag.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (semantic-event-window event))
	 )
    (select-window win t)
    (save-excursion
      ;(goto-char (window-start win))
      (mouse-set-point event)
      (sit-for 0)
      (semantic-popup-menu semantic-highlight-func-popup-menu)
      )
    (select-window startwin)))

(defvar semantic-highlight-func-ct-overlay nil
  "Overlay used to highlight the tag the cursor is in.")
(make-variable-buffer-local 'semantic-highlight-func-ct-overlay)

(defface semantic-highlight-func-current-tag-face
  '((((class color) (background dark))
     ;; Put this back to something closer to black later.
     (:background "gray20"))
    (((class color) (background light))
     (:background "gray90")))
  "Face used to show the top of current function."
  :group 'semantic-faces)

;;;###autoload
(define-minor-mode semantic-highlight-func-mode
  "Minor mode to highlight the first line of the current tag.
Enables/disables making the current function's first line light up.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') is highlighted, meaning the
first line which describes the rest of the construct.

See `semantic-stickyfunc-mode' for putting a function in the
header line.  This mode recycles the stickyfunc configuration
classes list.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  :lighter nil ;; Don't need indicator.  It's quite visible.
  (if semantic-highlight-func-mode
      (progn
	(unless (and (featurep 'semantic) (semantic-active-p))
	  ;; Disable minor mode if semantic stuff not available
	  (setq semantic-highlight-func-mode nil)
	  (error "Buffer %s was not set up for parsing" (buffer-name)))
	;; Setup our hook
	(add-hook 'post-command-hook
                  'semantic-highlight-func-highlight-current-tag nil t))
    ;; Disable highlight func mode
    (remove-hook 'post-command-hook
                 'semantic-highlight-func-highlight-current-tag t)
    (semantic-highlight-func-highlight-current-tag t)))

(defun semantic-highlight-func-highlight-current-tag (&optional disable)
  "Highlight the current tag under point.
Optional argument DISABLE will turn off any active highlight.
If the current tag for this buffer is different from the last time this
function was called, move the overlay."
  (when (and (not (minibufferp))
	     (or (not semantic-highlight-func-ct-overlay)
		 (eq (semantic-overlay-buffer
		      semantic-highlight-func-ct-overlay)
		     (current-buffer))))
    (let* ((tag (semantic-stickyfunc-tag-to-stick))
	   (ol semantic-highlight-func-ct-overlay))
      (when (not ol)
	;; No overlay in this buffer.  Make one.
	(setq ol (semantic-make-overlay (point-min) (point-min)
					(current-buffer) t nil))
	(semantic-overlay-put ol 'highlight-func t)
	(semantic-overlay-put ol 'face 'semantic-highlight-func-current-tag-face)
	(semantic-overlay-put ol 'keymap semantic-highlight-func-mode-map)
	(semantic-overlay-put ol 'help-echo
			      "Current Function : mouse-3 - Context menu")
	(setq semantic-highlight-func-ct-overlay ol)
	)

      ;; TAG is nil if there was nothing of the appropriate type there.
      (if (or (not tag) disable)
	  ;; No tag, make the overlay go away.
	  (progn
	    (semantic-overlay-put ol 'tag nil)
	    (semantic-overlay-move ol (point-min) (point-min) (current-buffer))
	    )

	;; We have a tag, if it is the same, do nothing.
	(unless (eq (semantic-overlay-get ol 'tag) tag)
	  (save-excursion
	    (goto-char (semantic-tag-start tag))
	    (search-forward (semantic-tag-name tag) nil t)
	    (semantic-overlay-put ol 'tag tag)
	    (semantic-overlay-move ol (point-at-bol) (point-at-eol))
	    )
	  )
	)))
  nil)

(semantic-add-minor-mode 'semantic-highlight-func-mode
                         "") ;; Don't need indicator.  It's quite visible

(provide 'semantic/util-modes)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/util-modes"
;; End:

;;; semantic/util-modes.el ends here
