;;; icomplete.el --- minibuffer completion incremental feedback

;; Copyright (C) 1992-1994, 1997, 1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Ken Manheimer <klm@i.am>
;; Maintainer: Ken Manheimer <klm@i.am>
;; Created: Mar 1993 Ken Manheimer, klm@nist.gov - first release to usenet
;; Last update: Ken Manheimer <klm@i.am>, 11/18/1999.
;; Keywords: help, abbrev

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

;; Loading this package implements a more fine-grained minibuffer
;; completion feedback scheme.  Prospective completions are concisely
;; indicated within the minibuffer itself, with each successive
;; keystroke.

;; See `icomplete-completions' docstring for a description of the
;; icomplete display format.

;; See the `icomplete-minibuffer-setup-hook' docstring for a means to
;; customize icomplete setup for interoperation with other
;; minibuffer-oriented packages.

;; To activate icomplete mode, load the package and use the
;; `icomplete-mode' function.  You can subsequently deactivate it by
;; invoking the function icomplete-mode with a negative prefix-arg
;; (C-U -1 ESC-x icomplete-mode).  Also, you can prevent activation of
;; the mode during package load by first setting the variable
;; `icomplete-mode' to nil.  Icompletion can be enabled any time after
;; the package is loaded by invoking icomplete-mode without a prefix
;; arg.

;; Thanks to everyone for their suggestions for refinements of this
;; package.  I particularly have to credit Michael Cook, who
;; implemented an incremental completion style in his 'iswitch'
;; functions that served as a model for icomplete.  Some other
;; contributors: Noah Friedman (restructuring as minor mode), Colin
;; Rafferty (lemacs reconciliation), Lars Lindberg, RMS, and others.

;; klm.

;;; Code:

;;;_* Provide
(provide 'icomplete)


(defgroup icomplete nil
  "Show completions dynamically in minibuffer."
  :prefix "icomplete-"
  :group 'minibuffer)

(defvar icomplete-prospects-length 80)
(make-obsolete-variable
 'icomplete-prospects-length 'icomplete-prospects-height "23.1")

;;;_* User Customization variables
(defcustom icomplete-prospects-height
  ;; 20 is an estimated common size for the prompt + minibuffer content, to
  ;; try to guess the number of lines used up by icomplete-prospects-length.
  (+ 1 (/ (+ icomplete-prospects-length 20) (window-width)))
  "Maximum number of lines to use in the minibuffer."
  :type 'integer
  :group 'icomplete
  :version "23.1")

(defcustom icomplete-compute-delay .3
  "Completions-computation stall, used only with large-number completions.
See `icomplete-delay-completions-threshold'."
  :type 'number
  :group 'icomplete)

(defcustom icomplete-delay-completions-threshold 400
  "Pending-completions number over which to apply `icomplete-compute-delay'."
  :type 'integer
  :group 'icomplete)

(defcustom icomplete-max-delay-chars 3
  "Maximum number of initial chars to apply icomplete compute delay."
  :type 'integer
  :group 'icomplete)

(defcustom icomplete-show-key-bindings t
  "If non-nil, show key bindings as well as completion for sole matches."
  :type 'boolean
  :group 'icomplete)

(defcustom icomplete-minibuffer-setup-hook nil
  "Icomplete-specific customization of minibuffer setup.

This hook is run during minibuffer setup if icomplete is active.
It is intended for use in customizing icomplete for interoperation
with other features and packages.  For instance:

  \(add-hook 'icomplete-minibuffer-setup-hook
	    \(function
	     \(lambda ()
	       \(make-local-variable 'max-mini-window-height)
	       \(setq max-mini-window-height 3))))

will constrain Emacs to a maximum minibuffer height of 3 lines when
icompletion is occurring."
  :type 'hook
  :group 'icomplete)


;;;_* Initialization

;;;_ + Internal Variables
;;;_  = icomplete-eoinput nil
(defvar icomplete-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the list of completions.")

;;;_  = icomplete-pre-command-hook
(defvar icomplete-pre-command-hook nil
  "Incremental-minibuffer-completion pre-command-hook.

Is run in minibuffer before user input when `icomplete-mode' is non-nil.
Use `icomplete-mode' function to set it up properly for incremental
minibuffer completion.")
(add-hook 'icomplete-pre-command-hook 'icomplete-tidy)
;;;_  = icomplete-post-command-hook
(defvar icomplete-post-command-hook nil
  "Incremental-minibuffer-completion post-command-hook.

Is run in minibuffer after user input when `icomplete-mode' is non-nil.
Use `icomplete-mode' function to set it up properly for incremental
minibuffer completion.")
(add-hook 'icomplete-post-command-hook 'icomplete-exhibit)

(defun icomplete-get-keys (func-name)
  "Return strings naming keys bound to FUNC-NAME, or nil if none.
Examines the prior, not current, buffer, presuming that current buffer
is minibuffer."
  (when (commandp func-name)
    (save-excursion
      (let* ((sym (intern func-name))
	     (buf (other-buffer nil t))
	     (keys (with-current-buffer buf (where-is-internal sym))))
	(when keys
	  (concat "<"
		  (mapconcat 'key-description
			     (sort keys
				   #'(lambda (x y)
				       (< (length x) (length y))))
			     ", ")
		  ">"))))))
;;;_  = icomplete-with-completion-tables
(defvar icomplete-with-completion-tables '(internal-complete-buffer)
  "Specialized completion tables with which icomplete should operate.

Icomplete does not operate with any specialized completion tables
except those on this list.")

;;;_ > icomplete-mode (&optional prefix)
;;;###autoload
(define-minor-mode icomplete-mode
  "Toggle incremental minibuffer completion (Icomplete mode).
With a prefix argument ARG, enable Icomplete mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'icomplete
  (if icomplete-mode
      ;; The following is not really necessary after first time -
      ;; no great loss.
      (add-hook 'minibuffer-setup-hook 'icomplete-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook 'icomplete-minibuffer-setup)))

;;;_ > icomplete-simple-completing-p ()
(defun icomplete-simple-completing-p ()
  "Non-nil if current window is minibuffer that's doing simple completion.

Conditions are:
   the selected window is a minibuffer,
   and not in the middle of macro execution,
   and `minibuffer-completion-table' is not a symbol (which would
       indicate some non-standard, non-simple completion mechanism,
       like file-name and other custom-func completions)."

  (and (window-minibuffer-p (selected-window))
       (not executing-kbd-macro)
       minibuffer-completion-table
       (or (not (functionp minibuffer-completion-table))
           (eq icomplete-with-completion-tables t)
           (member minibuffer-completion-table
                   icomplete-with-completion-tables))))

;;;_ > icomplete-minibuffer-setup ()
(defun icomplete-minibuffer-setup ()
  "Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (set (make-local-variable 'completion-show-inline-help) nil)
    (add-hook 'pre-command-hook
	      (lambda () (run-hooks 'icomplete-pre-command-hook))
	      nil t)
    (add-hook 'post-command-hook
	      (lambda () (run-hooks 'icomplete-post-command-hook))
	      nil t)
    (run-hooks 'icomplete-minibuffer-setup-hook)))
;


;;;_* Completion

;;;_ > icomplete-tidy ()
(defun icomplete-tidy ()
  "Remove completions display \(if any) prior to new user input.
Should be run in on the minibuffer `pre-command-hook'.  See `icomplete-mode'
and `minibuffer-setup-hook'."
  (delete-overlay icomplete-overlay))

;;;_ > icomplete-exhibit ()
(defun icomplete-exhibit ()
  "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.  See `icomplete-mode'
and `minibuffer-setup-hook'."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (save-excursion
      (goto-char (point-max))
                                        ; Insert the match-status information:
      (if (and (> (point-max) (minibuffer-prompt-end))
	       buffer-undo-list		; Wait for some user input.
	       (or
		;; Don't bother with delay after certain number of chars:
		(> (- (point) (field-beginning)) icomplete-max-delay-chars)
		;; Don't delay if alternatives number is small enough:
		(and (sequencep minibuffer-completion-table)
		     (< (length minibuffer-completion-table)
			icomplete-delay-completions-threshold))
		;; Delay - give some grace time for next keystroke, before
		;; embarking on computing completions:
		(sit-for icomplete-compute-delay)))
	  (let ((text (while-no-input
			 (icomplete-completions
			  (field-string)
			  minibuffer-completion-table
			  minibuffer-completion-predicate
                         (not minibuffer-completion-confirm))))
		(buffer-undo-list t)
		deactivate-mark)
	    ;; Do nothing if while-no-input was aborted.
	    (when (stringp text)
              (move-overlay icomplete-overlay (point) (point) (current-buffer))
              ;; The current C cursor code doesn't know to use the overlay's
              ;; marker's stickiness to figure out whether to place the cursor
              ;; before or after the string, so let's spoon-feed it the pos.
              (put-text-property 0 1 'cursor t text)
              (overlay-put icomplete-overlay 'after-string text)))))))

;;;_ > icomplete-completions (name candidates predicate require-match)
(defun icomplete-completions (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
one of \(), \[], or \{} pairs.  The choice of brackets is as follows:

  \(...) - a single prospect is identified and matching is enforced,
  \[...] - a single prospect is identified but matching is optional, or
  \{...} - multiple prospects, separated by commas, are indicated, and
          further input is required to distinguish a single one.

The displays for unambiguous matches have ` [Matched]' appended
\(whether complete or not), or ` \[No matches]', if no eligible
matches exist.  \(Keybindings for uniquely matched commands
are exhibited within the square braces.)"

  (let* ((non-essential t)
         (md (completion--field-metadata (field-beginning)))
	 (comps (completion-all-sorted-completions))
         (last (if (consp comps) (last comps)))
         (base-size (cdr last))
         (open-bracket (if require-match "(" "["))
         (close-bracket (if require-match ")" "]")))
    ;; `concat'/`mapconcat' is the slow part.
    (if (not (consp comps))
        (format " %sNo matches%s" open-bracket close-bracket)
      (if last (setcdr last nil))
      (let* ((most-try
              (if (and base-size (> base-size 0))
                  (completion-try-completion
                   name candidates predicate (length name) md)
                ;; If the `comps' are 0-based, the result should be
                ;; the same with `comps'.
                (completion-try-completion
                 name comps nil (length name) md)))
	     (most (if (consp most-try) (car most-try)
                     (if most-try (car comps) "")))
             ;; Compare name and most, so we can determine if name is
             ;; a prefix of most, or something else.
	     (compare (compare-strings name nil nil
				       most nil nil completion-ignore-case))
	     (determ (unless (or (eq t compare) (eq t most-try)
				 (= (setq compare (1- (abs compare)))
				    (length most)))
		       (concat open-bracket
			       (cond
				((= compare (length name))
                                 ;; Typical case: name is a prefix.
				 (substring most compare))
				((< compare 5) most)
				(t (concat "..." (substring most compare))))
			       close-bracket)))
	     ;;"-prospects" - more than one candidate
	     (prospects-len (+ (length determ) 6 ;; take {,...} into account
                               (string-width (buffer-string))))
             (prospects-max
              ;; Max total length to use, including the minibuffer content.
              (* (+ icomplete-prospects-height
                    ;; If the minibuffer content already uses up more than
                    ;; one line, increase the allowable space accordingly.
                    (/ prospects-len (window-width)))
                 (window-width)))
             (prefix-len
              ;; Find the common prefix among `comps'.
	      ;; We can't use the optimization below because its assumptions
	      ;; aren't always true, e.g. when completion-cycling (bug#10850):
	      ;; (if (eq t (compare-strings (car comps) nil (length most)
	      ;; 			 most nil nil completion-ignore-case))
	      ;;     ;; Common case.
	      ;;     (length most)
	      ;; Else, use try-completion.
	      (let ((comps-prefix (try-completion "" comps)))
		(and (stringp comps-prefix)
		     (length comps-prefix)))) ;;)

	     prospects most-is-exact comp limit)
	(if (eq most-try t) ;; (or (null (cdr comps))
	    (setq prospects nil)
	  (while (and comps (not limit))
	    (setq comp
		  (if prefix-len (substring (car comps) prefix-len) (car comps))
		  comps (cdr comps))
	    (cond ((string-equal comp "") (setq most-is-exact t))
		  ((member comp prospects))
		  (t (setq prospects-len
                           (+ (string-width comp) 1 prospects-len))
		     (if (< prospects-len prospects-max)
			 (push comp prospects)
		       (setq limit t))))))
        ;; Restore the base-size info, since completion-all-sorted-completions
        ;; is cached.
        (if last (setcdr last base-size))
	(if prospects
	    (concat determ
		    "{"
		    (and most-is-exact ",")
		    (mapconcat 'identity (nreverse prospects) ",")
		    (and limit ",...")
		    "}")
	  (concat determ
		  " [Matched"
		  (let ((keys (and icomplete-show-key-bindings
				   (commandp (intern-soft most))
				   (icomplete-get-keys most))))
		    (if keys (concat "; " keys) ""))
		  "]"))))))

;;_* Local emacs vars.
;;Local variables:
;;allout-layout: (-2 :)
;;End:

;;; icomplete.el ends here
