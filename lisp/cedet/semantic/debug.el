;;; semantic/debug.el --- Language Debugger framework

;; Copyright (C) 2003-2005, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; To provide better support for debugging parsers, this framework
;; provides the interface for debugging.  The work of parsing and
;; controlling and stepping through the parsing work must be implemented
;; by the parser.
;;
;; Fortunately, the nature of language support files means that the parser
;; may not need to be instrumented first.
;;
;; The debugger uses EIEIO objects.  One object controls the user
;; interface, including stepping, data-view, queries.  A second
;; object implemented here represents the parser itself.  A third represents
;; a parser independent frame which knows how to highlight the parser buffer.
;; Each parser must implement the interface and override any methods as needed.
;;

(eval-when-compile (require 'cl))
(require 'semantic)
(require 'eieio)
(eval-when-compile (require 'semantic/find))

;;; Code:

;;;###autoload
(defvar semantic-debug-parser-source nil
  "For any buffer, the file name (no path) of the parser.
This would be a parser for a specific language, not the source
to one of the parser generators.")
;;;###autoload
(make-variable-buffer-local 'semantic-debug-parser-source)

;;;###autoload
(defvar semantic-debug-parser-class nil
  "Class to create when building a debug parser object.")
;;;###autoload
(make-variable-buffer-local 'semantic-debug-parser-class)

(defvar semantic-debug-enabled nil
  "Non-nil when debugging a parser.")

;;; Variables used during a debug session.
(defvar semantic-debug-current-interface nil
  "The debugger interface currently active for this buffer.")

(defvar semantic-debug-current-parser nil
  "The parser current active for this buffer.")

;;; User Interface Portion
;;
(defclass semantic-debug-interface ()
  ((parser-buffer :initarg :parser-buffer
		  :type buffer
		  :documentation
		  "The buffer containing the parser we are debugging.")
   (parser-local-map :initarg :parser-local-map
		     :type keymap
		     :documentation
		     "The local keymap originally in the PARSER buffer.")
   (parser-location :type marker
		    :documentation
		    "A marker representing where we are in the parser buffer.")
   (source-buffer :initarg :source-buffer
		  :type buffer
		  :documentation
		  "The buffer containing the source we are parsing.
The :parser-buffer defines a parser that can parse the text in the
:source-buffer.")
   (source-local-map :initarg :source-local-map
		     :type keymap
		     :documentation
		     "The local keymap originally in the SOURCE buffer.")
   (source-location :type marker
		    :documentation
		    "A marker representing where we are in the parser buffer.")
   (data-buffer :initarg :data-buffer
		:type buffer
		:documentation
		"Buffer being used to display some useful data.
These buffers are brought into view when layout occurs.")
   (current-frame :type semantic-debug-frame
		  :documentation
		  "The currently displayed frame.")
   (overlays :type list
	     :initarg nil
	     :documentation
	     "Any active overlays being used to show the debug position.")
   )
  "Controls action when in `semantic-debug-mode'")

;; Methods
(defmethod semantic-debug-set-frame ((iface semantic-debug-interface) frame)
  "Set the current frame on IFACE to FRAME."
  (if frame
      (oset iface current-frame frame)
    (slot-makeunbound iface 'current-frame)))

(defmethod semantic-debug-set-parser-location ((iface semantic-debug-interface) point)
  "Set the parser location in IFACE to POINT."
  (with-current-buffer (oref iface parser-buffer)
    (if (not (slot-boundp iface 'parser-location))
	(oset iface parser-location (make-marker)))
    (move-marker (oref iface parser-location) point))
  )

(defmethod semantic-debug-set-source-location ((iface semantic-debug-interface) point)
  "Set the source location in IFACE to POINT."
  (with-current-buffer (oref iface source-buffer)
    (if (not (slot-boundp iface 'source-location))
	(oset iface source-location (make-marker)))
    (move-marker (oref iface source-location) point))
  )

(defmethod semantic-debug-interface-layout ((iface semantic-debug-interface))
  "Layout windows in the current frame to facilitate debugging."
  (delete-other-windows)
  ;; Deal with the data buffer
  (when (slot-boundp iface 'data-buffer)
    (let ((lines (/ (frame-height (selected-frame)) 3))
	  (cnt (with-current-buffer (oref iface data-buffer)
		 (count-lines (point-min) (point-max))))
	  )
      ;; Set the number of lines to 1/3, or the size of the data buffer.
      (if (< cnt lines) (setq cnt lines))

      (split-window-vertically cnt)
      (switch-to-buffer (oref iface data-buffer))
      )
    (other-window 1))
  ;; Parser
  (switch-to-buffer (oref iface parser-buffer))
  (when (slot-boundp iface 'parser-location)
    (goto-char (oref iface parser-location)))
  (split-window-vertically)
  (other-window 1)
  ;; Source
  (switch-to-buffer (oref iface source-buffer))
  (when (slot-boundp iface 'source-location)
    (goto-char (oref iface source-location)))
  )

(defmethod semantic-debug-highlight-lexical-token ((iface semantic-debug-interface) token)
  "For IFACE, highlight TOKEN in the source buffer .
TOKEN is a lexical token."
  (set-buffer (oref iface :source-buffer))

  (object-add-to-list iface 'overlays
		      (semantic-lex-highlight-token token))

  (semantic-debug-set-source-location iface (semantic-lex-token-start token))
  )

(defmethod semantic-debug-highlight-rule ((iface semantic-debug-interface) nonterm &optional rule match)
  "For IFACE, highlight NONTERM in the parser buffer.
NONTERM is the name of the rule currently being processed that shows up
as a nonterminal (or tag) in the source buffer.
If RULE and MATCH indices are specified, highlight those also."
  (set-buffer (oref iface :parser-buffer))

  (let* ((rules (semantic-find-tags-by-class 'nonterminal (current-buffer)))
	 (nt (semantic-find-first-tag-by-name nonterm rules))
	 (o nil)
	 )
    (when nt
      ;; I know it is the first symbol appearing in the body of this token.
      (goto-char (semantic-tag-start nt))

      (setq o (semantic-make-overlay (point) (progn (forward-sexp 1) (point))))
      (semantic-overlay-put o 'face 'highlight)

      (object-add-to-list iface 'overlays o)

      (semantic-debug-set-parser-location iface (semantic-overlay-start o))

      (when (and rule match)

	;; Rule, an int, is the rule inside the nonterminal we are following.
	(re-search-forward ":\\s-*")
	(while (/= 0 rule)
	  (re-search-forward "^\\s-*|\\s-*")
	  (setq rule (1- rule)))

	;; Now find the match inside the rule
	(while (/= 0 match)
	  (forward-sexp 1)
	  (skip-chars-forward " \t")
	  (setq match (1- match)))

	;; Now highlight the thingy we find there.
	(setq o (semantic-make-overlay (point) (progn (forward-sexp 1) (point))))
	(semantic-overlay-put o 'face 'highlight)

	(object-add-to-list iface 'overlays o)

	;; If we have a match for a sub-rule, have the parser position
	;; move so we can see it in the output window for very long rules.
	(semantic-debug-set-parser-location iface (semantic-overlay-start o))

	))))

(defmethod semantic-debug-unhighlight ((iface semantic-debug-interface))
  "Remove all debugging overlays."
  (mapc 'semantic-overlay-delete (oref iface overlays))
  (oset iface overlays nil))

;; Call from the parser at a breakpoint
(defvar semantic-debug-user-command nil
  "The command the user is requesting.")

(defun semantic-debug-break (frame)
  "Stop parsing now at FRAME.
FRAME is an object that represents the parser's view of the
current state of the world.
This function enters a recursive edit.  It returns
on an `exit-recursive-edit', or if someone uses one
of the `semantic-debug-mode' commands.
It returns the command specified.  Parsers need to take action
on different types of return values."
  (save-window-excursion
    ;; Set up displaying information
    (semantic-debug-mode t)
    (unwind-protect
	(progn
	  (semantic-debug-frame-highlight frame)
	  (semantic-debug-interface-layout semantic-debug-current-interface)
	  (condition-case nil
	      ;; Enter recursive edit... wait for user command.
	      (recursive-edit)
	    (error nil)))
      (semantic-debug-unhighlight semantic-debug-current-interface)
      (semantic-debug-mode nil))
    ;; Find the requested user state.  Do something.
    (let ((returnstate semantic-debug-user-command))
      (setq semantic-debug-user-command nil)
      returnstate)
    ))

;;; Frame
;;
;; A frame can represent the state at a break point.
(defclass semantic-debug-frame ()
  (
   )
  "One frame representation.")

(defmethod semantic-debug-frame-highlight ((frame semantic-debug-frame))
  "Highlight one parser frame."

  )

(defmethod semantic-debug-frame-info ((frame semantic-debug-frame))
  "Display info about this one parser frame."

  )

;;; Major Mode
;;
(defvar semantic-debug-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "n" 'semantic-debug-next)
    (define-key km " " 'semantic-debug-next)
    (define-key km "s" 'semantic-debug-step)
    (define-key km "u" 'semantic-debug-up)
    (define-key km "d" 'semantic-debug-down)
    (define-key km "f" 'semantic-debug-fail-match)
    (define-key km "h" 'semantic-debug-print-state)
    (define-key km "s" 'semantic-debug-jump-to-source)
    (define-key km "p" 'semantic-debug-jump-to-parser)
    (define-key km "q" 'semantic-debug-quit)
    (define-key km "a" 'semantic-debug-abort)
    (define-key km "g" 'semantic-debug-go)
    (define-key km "b" 'semantic-debug-set-breakpoint)
    ;; Some boring bindings.
    (define-key km "e" 'eval-expression)

    km)
  "Keymap used when in semantic-debug-node.")

(defun semantic-debug-mode (onoff)
  "Turn `semantic-debug-mode' on and off.
Argument ONOFF is non-nil when we are entering debug mode.
\\{semantic-debug-mode-map}"
  (let ((iface semantic-debug-current-interface))
    (if onoff
	;; Turn it on
	(with-current-buffer (oref iface parser-buffer)
	  ;; Install our map onto this buffer
	  (use-local-map semantic-debug-mode-map)
	  ;; Make the buffer read only
	  (toggle-read-only 1)

	  (set-buffer (oref iface source-buffer))
	  ;; Use our map in the source buffer also
	  (use-local-map semantic-debug-mode-map)
	  ;; Make the buffer read only
	  (toggle-read-only 1)
	  ;; Hooks
	  (run-hooks 'semantic-debug-mode-hook)
	  )
      ;; Restore old mode information
      (with-current-buffer
          (oref semantic-debug-current-interface parser-buffer)
	(use-local-map
	 (oref semantic-debug-current-interface parser-local-map))
	)
      (with-current-buffer
          (oref semantic-debug-current-interface source-buffer)
	(use-local-map
	 (oref semantic-debug-current-interface source-local-map))
	)
      (run-hooks 'semantic-debug-exit-hook)
      )))

(defun semantic-debug ()
  "Parse the current buffer and run in debug mode."
  (interactive)
  (if semantic-debug-current-interface
      (error "You are already in a debug session"))
  (if (not semantic-debug-parser-class)
      (error "This major mode does not support parser debugging"))
  ;; Clear the cache to force a full reparse.
  (semantic-clear-toplevel-cache)
  ;; Do the parse
  (let ((semantic-debug-enabled t)
	;; Create an interface
	(semantic-debug-current-interface
	 (let ((parserb  (semantic-debug-find-parser-source)))
	   (semantic-debug-interface
	    "Debug Interface"
	    :parser-buffer parserb
	    :parser-local-map (with-current-buffer parserb
				(current-local-map))
	    :source-buffer (current-buffer)
	    :source-local-map (current-local-map)
	    )))
	;; Create a parser debug interface
	(semantic-debug-current-parser
	 (funcall semantic-debug-parser-class "parser"))
	)
    ;; We could recurse into a parser while debugging.
    ;; Is that a problem?
    (semantic-fetch-tags)
    ;; We should turn the auto-parser back on, but don't do it for
    ;; now until the debugger is working well.
    ))

(defun semantic-debug-find-parser-source ()
  "Return a buffer containing the parser source file for the current buffer.
The parser needs to be on the load path, or this routine returns nil."
  (if (not semantic-debug-parser-source)
      (error "No parser is associated with this buffer"))
  (let ((parser (locate-library semantic-debug-parser-source t)))
    (if parser
	(find-file-noselect parser)
      (error "Cannot find parser source.  It should be on the load-path"))))

;;; Debugger commands
;;
(defun semantic-debug-next ()
  "Perform one parser operation.
In the recursive parser, this steps past one match rule.
In other parsers, this may be just like `semantic-debug-step'."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-next parser)
    (exit-recursive-edit)
    )
  )

(defun semantic-debug-step ()
  "Perform one parser operation."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-step parser)
    (exit-recursive-edit)
    )
  )

(defun semantic-debug-up ()
  "Move highlighting representation up one level."
  (interactive)
  (message "Not implemented yet.")
  )

(defun semantic-debug-down ()
  "Move highlighting representation down one level."
  (interactive)
  (message "Not implemented yet.")
  )

(defun semantic-debug-fail-match ()
  "Artificially fail the current match."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-fail parser)
    (exit-recursive-edit)
    )
  )

(defun semantic-debug-print-state ()
  "Show interesting parser state."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-print-state parser)
    )
  )

(defun semantic-debug-jump-to-source ()
  "Move cursor to the source code being parsed at the current lexical token."
  (interactive)
  (let* ((interface semantic-debug-current-interface)
	 (buf (oref interface source-buffer)))
    (if (get-buffer-window buf)
	(progn
	  (select-frame (window-frame (get-buffer-window buf)))
	  (select-window (get-buffer-window buf)))
      ;; Technically, this should do a window layout operation
      (switch-to-buffer buf))
    )
  )

(defun semantic-debug-jump-to-parser ()
  "Move cursor to the parser being debugged."
  (interactive)
  (let* ((interface semantic-debug-current-interface)
	 (buf (oref interface parser-buffer)))
    (if (get-buffer-window buf)
	(progn
	  (select-frame (window-frame (get-buffer-window buf)))
	  (select-window (get-buffer-window buf)))
      ;; Technically, this should do a window layout operation
      (switch-to-buffer buf))
    )
  )

(defun semantic-debug-quit ()
  "Exit debug mode, blowing all stack, and leaving the parse incomplete.
Do not update any tokens already parsed."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-quit parser)
    (exit-recursive-edit)
    )
  )

(defun semantic-debug-abort ()
  "Abort one level of debug mode, blowing all stack."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-abort parser)
    (exit-recursive-edit)
    )
  )

(defun semantic-debug-go ()
  "Continue parsing till finish or breakpoint."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-go parser)
    (exit-recursive-edit)
    )
  )

(defun semantic-debug-set-breakpoint ()
  "Set a breakpoint at the current rule location."
  (interactive)
  (let ((parser semantic-debug-current-parser)
	;; Get the location as semantic tokens.
	(location (semantic-current-tag))
	)
    (if location
	(semantic-debug-parser-break parser location)
      (error "Not on a rule"))
    )
  )


;;; Debugger superclass
;;
(defclass semantic-debug-parser ()
  (
   )
  "Represents a parser and its state.
When implementing the debug parser you can add extra functionality
by overriding one of the command methods.  Be sure to use
`call-next-method' so that the debug command is saved, and passed
down to your parser later."
  :abstract t)

(defmethod semantic-debug-parser-next ((parser semantic-debug-parser))
  "Execute next for this PARSER."
  (setq semantic-debug-user-command 'next)
  )

(defmethod semantic-debug-parser-step ((parser semantic-debug-parser))
  "Execute a step for this PARSER."
  (setq semantic-debug-user-command 'step)
  )

(defmethod semantic-debug-parser-go ((parser semantic-debug-parser))
  "Continue execution in this PARSER until the next breakpoint."
  (setq semantic-debug-user-command 'go)
  )

(defmethod semantic-debug-parser-fail ((parser semantic-debug-parser))
  "Continue execution in this PARSER until the next breakpoint."
  (setq semantic-debug-user-command 'fail)
  )

(defmethod semantic-debug-parser-quit ((parser semantic-debug-parser))
  "Continue execution in this PARSER until the next breakpoint."
  (setq semantic-debug-user-command 'quit)
  )

(defmethod semantic-debug-parser-abort ((parser semantic-debug-parser))
  "Continue execution in this PARSER until the next breakpoint."
  (setq semantic-debug-user-command 'abort)
  )

(defmethod semantic-debug-parser-print-state ((parser semantic-debug-parser))
  "Print state for this PARSER at the current breakpoint."
  (with-slots (current-frame) semantic-debug-current-interface
    (when current-frame
      (semantic-debug-frame-info current-frame)
      )))

(defmethod semantic-debug-parser-break ((parser semantic-debug-parser))
  "Set a breakpoint for this PARSER."
  )

;; Stack stuff
(defmethod semantic-debug-parser-frames ((parser semantic-debug-parser))
  "Return a list of frames for the current parser.
A frame is of the form:
  ( .. .what ? .. )
"
  (error "Parser has not implemented frame values")
  )


(provide 'semantic/debug)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/debug"
;; End:

;;; semantic/debug.el ends here
