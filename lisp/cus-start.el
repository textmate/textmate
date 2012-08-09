;;; cus-start.el --- define customization properties of builtins
;;
;; Copyright (C) 1997, 1999-2012  Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: internal
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
;;
;; This file adds customize support for built-in variables.

;; While dumping Emacs, this file is loaded, but it only records
;; the standard values; it does not do the rest of the job.
;; Later on, if the user makes a customization buffer,
;; this file is loaded again with (require 'cus-start);
;; then it does the whole job.

;;; Code:

;; Elements of this list have the form:
;; SYMBOL GROUP TYPE VERSION REST...
;; SYMBOL is the name of the variable.
;; GROUP is the custom group to which it belongs (may also be a list
;; of groups)
;; TYPE is the defcustom :type.
;; VERSION is the defcustom :version (or nil).
;; REST is a set of :KEYWORD VALUE pairs.  Accepted :KEYWORDs are:
;; :standard - standard value for SYMBOL (else use current value)
;; :set - custom-set property
;; :risky - risky-local-variable property
;; :safe - safe-local-variable property
;; :tag - custom-tag property
(let ((all '(;; alloc.c
	     (gc-cons-threshold alloc integer)
	     (garbage-collection-messages alloc boolean)
	     ;; buffer.c
	     (mode-line-format mode-line sexp) ;Hard to do right.
	     (major-mode internal function)
	     (case-fold-search matching boolean)
	     (fill-column fill integer)
	     (left-margin fill integer)
	     (tab-width editing-basics integer)
	     (ctl-arrow display boolean)
	     (truncate-lines display boolean)
	     (word-wrap display boolean)
	     (selective-display-ellipses display boolean)
	     (indicate-empty-lines fringe boolean)
	     (indicate-buffer-boundaries
	      fringe
	      (choice
	       (const :tag "No indicators" nil)
	       (const :tag "On left, with arrows" left)
	       (const :tag "On right, with arrows" right)
	       (set :tag "Pick your own design"
		    :value ((t . nil))
		    :format "%{%t%}:\n%v\n%d"
		    :doc "You can specify a default and then override it \
for individual indicators.
Leaving \"Default\" unchecked is equivalent with specifying a default of
\"Do not show\"."
		    (choice :tag "Default"
			    :value (t . nil)
			    (const :tag "Do not show" (t . nil))
			    (const :tag "On the left" (t . left))
			    (const :tag "On the right" (t . right)))
		    (choice :tag "Top"
			    :value (top . left)
			    (const :tag "Do not show" (top . nil))
			    (const :tag "On the left" (top . left))
			    (const :tag "On the right" (top . right)))
		    (choice :tag "Bottom"
			    :value (bottom . left)
			    (const :tag "Do not show" (bottom . nil))
			    (const :tag "On the left" (bottom . left))
			    (const :tag "On the right" (bottom . right)))
		    (choice :tag "Up arrow"
			    :value (up . left)
			    (const :tag "Do not show" (up . nil))
			    (const :tag "On the left" (up . left))
			    (const :tag "On the right" (up . right)))
		    (choice :tag "Down arrow"
			    :value (down . left)
			    (const :tag "Do not show" (down . nil))
			    (const :tag "On the left" (down . left))
			    (const :tag "On the right" (down . right))))
	       (other :tag "On left, no arrows" t)))
	     (scroll-up-aggressively windows
				     (choice (const :tag "off" nil) number)
				     "21.1")
	     (scroll-down-aggressively windows
				       (choice (const :tag "off" nil) number)
				       "21.1")
	     (line-spacing display (choice (const :tag "none" nil) integer)
			   "22.1")
	     (cursor-in-non-selected-windows
	      cursor boolean nil
	      :tag "Cursor In Non-selected Windows"
	      :set (lambda (symbol value)
		     (set-default symbol value)
		     (force-mode-line-update t)))
	     (transient-mark-mode editing-basics boolean nil
				  :standard (not noninteractive)
				  :initialize custom-initialize-delay
				  :set custom-set-minor-mode)
	     (bidi-paragraph-direction
	      paragraphs
	      (choice
	       (const :tag "Left to Right" left-to-right)
	       (const :tag "Right to Left" right-to-left)
	       (const :tag "Dynamic, according to paragraph text" nil))
	      "24.1")
	     ;; callint.c
	     (mark-even-if-inactive editing-basics boolean)
	     ;; callproc.c
	     (shell-file-name execute file)
	     (exec-path execute
			(repeat (choice (const :tag "default directory" nil)
					(directory :format "%v"))))
	     ;; charset.c
	     (charset-map-path installation
			       (repeat (directory :format "%v")))
	     ;; coding.c
	     (inhibit-eol-conversion mule boolean)
	     (eol-mnemonic-undecided mule string)
	     (eol-mnemonic-unix mule string)
	     (eol-mnemonic-dos mule string)
	     (eol-mnemonic-mac mule string)
	     (file-coding-system-alist
	      mule
	      (alist
	       :key-type (regexp :tag "File regexp")
	       :value-type (choice
			    :value (undecided . undecided)
			    (cons :tag "Encoding/decoding pair"
				  :value (undecided . undecided)
				  (coding-system :tag "Decoding")
				  (coding-system :tag "Encoding"))
			    (coding-system
			     :tag "Single coding system"
			     :value undecided
			     :match (lambda (widget value)
				      (and value (not (functionp value)))))
			    (function :value ignore))))
	     ;; dired.c
	     (completion-ignored-extensions dired
					    (repeat (string :format "%v")))
	     ;; dispnew.c
	     (baud-rate display integer)
	     (inverse-video display boolean)
	     (visible-bell display boolean)
	     (no-redraw-on-reenter display boolean)
	     ;; editfns.c
	     (user-full-name mail string)
	     ;; eval.c
	     (max-specpdl-size limits integer)
	     (max-lisp-eval-depth limits integer)
	     (max-mini-window-height limits
				     (choice (const :tag "quarter screen" nil)
					     number) "23.1")
	     (debug-on-error debug
			     (choice (const :tag "off")
				     (repeat :menu-tag "When"
					     :value (nil)
					     (symbol :format "%v"))
				     (const :tag "always" t)))
	     (debug-ignored-errors debug (repeat (choice symbol regexp)))
	     (debug-on-quit debug boolean)
             ;; fileio.c
             (delete-by-moving-to-trash auto-save boolean "23.1")
	     (auto-save-visited-file-name auto-save boolean)
	     ;; filelock.c
	     (temporary-file-directory
	      ;; Darwin section added 24.1, does not seem worth :version bump.
	      files directory nil
	      :standard
	      (file-name-as-directory
	       ;; FIXME ? Should there be Ftemporary_file_directory to do this
	       ;; more robustly (cf set_local_socket in emacsclient.c).
	       ;; It could be used elsewhere, eg Fcall_process_region,
	       ;; server-socket-dir.  See bug#7135.
	       (cond ((memq system-type '(ms-dos windows-nt))
		      (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP")
			  "c:/temp"))
		     ((eq system-type 'darwin)
		      (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP")
			  ;; See bug#7135.
			  (let ((tmp (ignore-errors
				       (shell-command-to-string
					"getconf DARWIN_USER_TEMP_DIR"))))
			    (and (stringp tmp)
				 (setq tmp (replace-regexp-in-string
					    "\n\\'" "" tmp))
				 ;; Handles "getconf: Unrecognized variable..."
				 (file-directory-p tmp)
				 tmp))
			  "/tmp"))
		     (t
		      (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP")
			  "/tmp"))))
	      :initialize custom-initialize-delay)
	     ;; fns.c
	     (use-dialog-box menu boolean "21.1")
	     (use-file-dialog menu boolean "22.1")
	     (focus-follows-mouse frames boolean "20.3")
	     ;; frame.c
	     (default-frame-alist frames
	       (repeat (cons :format "%v"
			     (symbol :tag "Parameter")
			     (sexp :tag "Value"))))
	     (mouse-highlight mouse (choice (const :tag "disabled" nil)
					    (const :tag "always shown" t)
					    (other :tag "hidden by keypress" 1))
			      "22.1")
	     (make-pointer-invisible mouse boolean "23.2")
	     (menu-bar-mode frames boolean nil
			    ;; FIXME?
;			    :initialize custom-initialize-default
			    :set custom-set-minor-mode)
	     (tool-bar-mode (frames mouse) boolean nil
;			    :initialize custom-initialize-default
			    :set custom-set-minor-mode)
	     ;; fringe.c
	     (overflow-newline-into-fringe fringe boolean)
	     ;; image.c
	     (imagemagick-render-type image integer "24.1")
	     ;; indent.c
	     (indent-tabs-mode indent boolean)
	     ;; keyboard.c
	     (meta-prefix-char keyboard character)
	     (auto-save-interval auto-save integer)
	     (auto-save-timeout auto-save (choice (const :tag "off" nil)
						  (integer :format "%v")))
	     (echo-keystrokes minibuffer number)
	     (polling-period keyboard integer)
	     (double-click-time mouse (restricted-sexp
				       :match-alternatives (integerp 'nil 't)))
	     (double-click-fuzz mouse integer "22.1")
	     (inhibit-local-menu-bar-menus menu boolean)
	     (help-char keyboard character)
	     (help-event-list keyboard (repeat (sexp :format "%v")))
	     (menu-prompting menu boolean)
	     (select-active-regions killing
				    (choice (const :tag "always" t)
					    (const :tag "only shift-selection or mouse-drag" only)
					    (const :tag "off" nil))
				    "24.1")
	     (suggest-key-bindings keyboard (choice (const :tag "off" nil)
						    (integer :tag "time" 2)
						    (other :tag "on")))
             (debug-on-event debug
                             (choice (const :tag "None" nil)
                                     (const :tag "When sent SIGUSR1" sigusr1)
                                     (const :tag "When sent SIGUSR2" sigusr2))
                             "24.1")

;; This is not good news because it will use the wrong
;; version-specific directories when you upgrade.  We need
;; customization of the front of the list, maintaining the standard
;; value intact at the back.
;;; 	     (load-path environment
;;; 			(repeat (choice :tag "[Current dir?]"
;;; 					:format "%[Current dir?%] %v"
;;; 					(const :tag " current dir" nil)
;;;					(directory :format "%v"))))
	     ;; minibuf.c
	     (enable-recursive-minibuffers minibuffer boolean)
	     (history-length minibuffer
			     (choice (const :tag "Infinite" t) integer)
			     "22.1")
	     (history-delete-duplicates minibuffer boolean "22.1")
	     (read-buffer-completion-ignore-case minibuffer boolean "23.1")

	     (minibuffer-prompt-properties
	      minibuffer
	      (list
	       (checklist :inline t
			  (const :tag "Read-Only"
				 :doc "Prevent prompt from being modified"
				 :format "%t%n%h"
				 :inline t
				 (read-only t))
			  (const :tag "Don't Enter"
				 :doc "Prevent point from ever entering prompt"
				 :format "%t%n%h"
				 :inline t
				 (point-entered minibuffer-avoid-prompt)))
	       (repeat :inline t
		       :tag "Other Properties"
		       (list :inline t
			     :format "%v"
			     (symbol :tag "Property")
			     (sexp :tag "Value"))))
	      "21.1")
	     (minibuffer-auto-raise minibuffer boolean)
	     ;; options property set at end
	     (read-buffer-function minibuffer
				   (choice (const nil)
					   (function-item iswitchb-read-buffer)
					   function))
	     ;; msdos.c
	     (dos-unsupported-char-glyph display integer)
	     ;; nsterm.m
	     (ns-control-modifier
	      ns
	      (choice (const :tag "No modifier" nil)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "23.1")
	     (ns-right-control-modifier
	      ns
	      (choice (const :tag "No modifier (work as control)" none)
		      (const :tag "Use the value of ns-control-modifier"
			     left)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "24.0")
	     (ns-command-modifier
	      ns
	      (choice (const :tag "No modifier" nil)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "23.1")
	     (ns-right-command-modifier
	      ns
	      (choice (const :tag "No modifier (work as command)" none)
		      (const :tag "Use the value of ns-command-modifier"
			     left)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "24.0")
	     (ns-alternate-modifier
	      ns
	      (choice (const :tag "No modifier (work as alternate/option)" none)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "23.1")
	     (ns-right-alternate-modifier
	      ns
	      (choice (const :tag "No modifier (work as alternate/option)" none)
		      (const :tag "Use the value of ns-alternate-modifier"
			     left)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "23.3")
	     (ns-function-modifier
	      ns
	      (choice (const :tag "No modifier (work as function)" none)
		      (const control) (const meta)
		      (const alt) (const hyper)
		      (const super)) "23.1")
	     (ns-antialias-text ns boolean "23.1")
	     (ns-auto-hide-menu-bar ns boolean "24.0")
	     ;; process.c
	     (delete-exited-processes processes-basics boolean)
	     ;; syntax.c
	     (parse-sexp-ignore-comments editing-basics boolean)
	     (words-include-escapes editing-basics boolean)
	     (open-paren-in-column-0-is-defun-start editing-basics boolean
						    "21.1")
             ;; term.c
             (visible-cursor cursor boolean "22.1")
	     ;; undo.c
	     (undo-limit undo integer)
	     (undo-strong-limit undo integer)
	     (undo-outer-limit undo
			       (choice integer
				       (const :tag "No limit"
					      :format "%t\n%d"
					      :doc
					      "With this choice, \
the undo info for the current command never gets discarded.
This should only be chosen under exceptional circumstances,
since it could result in memory overflow and make Emacs crash."
					      nil))
			       "22.1")
	     ;; window.c
	     (temp-buffer-show-function windows (choice (const nil) function))
	     (next-screen-context-lines windows integer)
 	     (scroll-preserve-screen-position
 	      windows (choice
 		       (const :tag "Off (nil)" :value nil)
 		       (const :tag "Full screen (t)" :value t)
 		       (other :tag "Always" 1)) "22.1")
	     (recenter-redisplay
	      windows (choice
		       (const :tag "Never (nil)" :value nil)
		       (const :tag "Only on ttys" :value tty)
		       (other :tag "Always" t)) "23.1")
	     (window-combination-resize windows boolean "24.1")
	     (window-combination-limit windows boolean "24.1")
	     ;; xdisp.c
	     (show-trailing-whitespace whitespace-faces boolean nil
				       :safe booleanp)
	     (scroll-step windows integer)
	     (scroll-conservatively windows integer)
	     (scroll-margin windows integer)
	     (hscroll-margin windows integer "22.1")
	     (hscroll-step windows number "22.1")
	     (truncate-partial-width-windows display boolean "23.1")
	     (mode-line-inverse-video mode-line boolean)
	     (mode-line-in-non-selected-windows mode-line boolean "22.1")
	     (line-number-display-limit display
					(choice integer
						(const :tag "No limit" nil)))
	     (line-number-display-limit-width display integer "22.1")
	     (highlight-nonselected-windows display boolean)
	     (message-log-max debug (choice (const :tag "Disable" nil)
					    (integer :menu-tag "lines"
						     :format "%v")
					    (other :tag "Unlimited" t)))
	     (unibyte-display-via-language-environment mule boolean)
	     (blink-cursor-alist cursor alist "22.1")
	     (overline-margin display integer "22.1")
	     (underline-minimum-offset display integer "23.1")
             (mouse-autoselect-window
	      display (choice
		       (const :tag "Off (nil)" :value nil)
		       (const :tag "Immediate" :value t)
		       (number :tag "Delay by secs" :value 0.5)) "22.1")
             (tool-bar-style
	      frames (choice
		      (const :tag "Images" :value image)
		      (const :tag "Text" :value text)
		      (const :tag "Both" :value both)
		      (const :tag "Both-horiz" :value both-horiz)
		      (const :tag "Text-image-horiz" :value text-image-horiz)
		      (const :tag "System default" :value nil)) "23.3")
             (tool-bar-max-label-size frames integer "23.3")
	     (auto-hscroll-mode scrolling boolean "21.1")
	     (display-hourglass cursor boolean)
	     (hourglass-delay cursor number)

	     ;; xfaces.c
	     (scalable-fonts-allowed display boolean "22.1")
	     ;; xfns.c
	     (x-bitmap-file-path installation
				 (repeat (directory :format "%v")))
	     (x-gtk-use-old-file-dialog menu boolean "22.1")
	     (x-gtk-show-hidden-files menu boolean "22.1")
	     (x-gtk-file-dialog-help-text menu boolean "22.1")
	     (x-gtk-whole-detached-tool-bar x boolean "22.1")
	     (x-gtk-use-system-tooltips tooltip boolean "23.3")
	     ;; xterm.c
	     (x-use-underline-position-properties display boolean "22.1")
	     (x-underline-at-descent-line display boolean "22.1")
	     (x-stretch-cursor display boolean "21.1")
	     ;; xselect.c
	     (x-select-enable-clipboard-manager killing boolean "24.1")
	     ;; xsettings.c
	     (font-use-system-font font-selection boolean "23.2")))
      this symbol group type standard version native-p rest prop propval
      ;; This function turns a value
      ;; into an expression which produces that value.
      (quoter (lambda (sexp)
		(if (or (memq sexp '(t nil))
			(keywordp sexp)
			(and (listp sexp)
			     (memq (car sexp) '(lambda)))
			(stringp sexp)
			(numberp sexp))
		    sexp
		  (list 'quote sexp)))))
  (while all
    (setq this (car all)
	  all (cdr all)
	  symbol (nth 0 this)
	  group (nth 1 this)
	  type (nth 2 this)
	  version (nth 3 this)
	  rest (nthcdr 4 this)
	  ;; If we did not specify any standard value expression above,
	  ;; use the current value as the standard value.
	  standard (if (setq prop (memq :standard rest))
		       (cadr prop)
		     (if (default-boundp symbol)
			 (funcall quoter (default-value symbol))))
	  ;; Don't complain about missing variables which are
	  ;; irrelevant to this platform.
	  native-p (save-match-data
		     (cond
		      ((string-match "\\`dos-" (symbol-name symbol))
		       (eq system-type 'ms-dos))
		      ((string-match "\\`w32-" (symbol-name symbol))
		       (eq system-type 'windows-nt))
		      ((string-match "\\`ns-" (symbol-name symbol))
		       (featurep 'ns))
		      ((string-match "\\`x-.*gtk" (symbol-name symbol))
		       (featurep 'gtk))
		      ((string-match "clipboard-manager" (symbol-name symbol))
		       (boundp 'x-select-enable-clipboard-manager))
		      ((string-match "\\`x-" (symbol-name symbol))
		       (fboundp 'x-create-frame))
		      ((string-match "selection" (symbol-name symbol))
		       (fboundp 'x-selection-exists-p))
		      ((string-match "fringe" (symbol-name symbol))
		       (fboundp 'define-fringe-bitmap))
		      ((string-match "\\`imagemagick" (symbol-name symbol))
		       (fboundp 'imagemagick-types))
		      ((equal "font-use-system-font" (symbol-name symbol))
		       (featurep 'system-font-setting))
		      ;; Conditioned on x-create-frame, because that's
		      ;; the condition for loadup.el to preload tool-bar.el.
		      ((string-match "tool-bar-" (symbol-name symbol))
		       (fboundp 'x-create-frame))
		      (t t))))
    (if (not (boundp symbol))
	;; If variables are removed from C code, give an error here!
	(and native-p
	     (message "Note, built-in variable `%S' not bound" symbol))
      ;; Save the standard value, unless we already did.
      (or (get symbol 'standard-value)
	  (put symbol 'standard-value (list standard)))
      ;; We need these properties independent of whether cus-start is loaded.
      (if (setq prop (memq :safe rest))
	  (put symbol 'safe-local-variable (cadr prop)))
      (if (setq prop (memq :risky rest))
	  (put symbol 'risky-local-variable (cadr prop)))
      (if (setq prop (memq :set rest))
	  (put symbol 'custom-set (cadr prop)))
      ;; Note this is the _only_ initialize property we handle.
      (if (eq (cadr (memq :initialize rest)) 'custom-initialize-delay)
	  (push symbol custom-delayed-init-variables))
      ;; If this is NOT while dumping Emacs, set up the rest of the
      ;; customization info.  This is the stuff that is not needed
      ;; until someone does M-x customize etc.
      (unless purify-flag
	;; Add it to the right group(s).
	(if (listp group)
	    (dolist (g group)
	      (custom-add-to-group g symbol 'custom-variable))
	  (custom-add-to-group group symbol 'custom-variable))
	;; Set the type.
	(put symbol 'custom-type type)
	(if version (put symbol 'custom-version version))
	(while rest
	  (setq prop (car rest)
		propval (cadr rest)
		rest (nthcdr 2 rest))
	  (cond ((memq prop '(:standard :risky :safe :set))) ; handled above
		((eq prop :tag)
		 (put symbol 'custom-tag propval))))))))

(custom-add-to-group 'iswitchb 'read-buffer-function 'custom-variable)
(custom-add-to-group 'font-lock 'open-paren-in-column-0-is-defun-start
		     'custom-variable)

;; Record cus-start as loaded if we have set up all the info that we can.
;; Don't record it as loaded if we have only set up the standard values
;; and safe/risky properties.
(unless purify-flag
  (provide 'cus-start))

;;; cus-start.el ends here
