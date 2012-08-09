;;; cus-edit.el --- tools for customizing Emacs and Lisp packages
;;
;; Copyright (C) 1996-1997, 1999-2012  Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: FSF
;; Keywords: help, faces
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
;; This file implements the code to create and edit customize buffers.
;;
;; See `custom.el'.

;; No commands should have names starting with `custom-' because
;; that interferes with completion.  Use `customize-' for commands
;; that the user will run with M-x, and `Custom-' for interactive commands.

;; The identity of a customize option is represented by a Lisp symbol.
;; The following values are associated with an option.

;; 0. The current value.

;;    This is the value of the option as seen by "the rest of Emacs".

;;    Usually extracted by 'default-value', but can be extracted with
;;    different means if the option symbol has the 'custom-get'
;;    property.  Similarly, set-default (or the 'custom-set' property)
;;    can set it.

;; 1. The widget value.

;;    This is the value shown in the widget in a customize buffer.

;; 2. The customized value.

;;    This is the last value given to the option through customize.

;;    It is stored in the 'customized-value' property of the option, in a
;;    cons-cell whose car evaluates to the customized value.

;; 3. The saved value.

;;    This is last value saved from customize.

;;    It is stored in the 'saved-value' property of the option, in a
;;    cons-cell whose car evaluates to the saved value.

;; 4. The standard value.

;;    This is the value given in the 'defcustom' declaration.

;;    It is stored in the 'standard-value' property of the option, in a
;;    cons-cell whose car evaluates to the standard value.

;; 5. The "think" value.

;;    This is what customize thinks the current value should be.

;;    This is the customized value, if any such value exists, otherwise
;;    the saved value, if that exists, and as a last resort the standard
;;    value.

;; The reason for storing values unevaluated: This is so you can have
;; values that depend on the environment.  For example, you can have a
;; variable that has one value when Emacs is running under a window
;; system, and another value on a tty.  Since the evaluation is only done
;; when the variable is first initialized, this is only relevant for the
;; saved (and standard) values, but affect others values for
;; compatibility.

;; You can see (and modify and save) this unevaluated value by selecting
;; "Show Saved Lisp Expression" from the Lisp interface.  This will
;; give you the unevaluated saved value, if any, otherwise the
;; unevaluated standard value.

;; The possible states for a customize widget are:

;; 0. unknown

;;    The state has not been determined yet.

;; 1. modified

;;    The widget value is different from the current value.

;; 2. changed

;;    The current value is different from the "think" value.

;; 3. set

;;    The "think" value is the customized value.

;; 4. saved

;;    The "think" value is the saved value.

;; 5. standard

;;    The "think" value is the standard value.

;; 6. rogue

;;    There is no standard value.  This means that the variable was
;;    not defined with defcustom, nor handled in cus-start.el.  Most
;;    standard interactive Custom commands do not let you create a
;;    Custom buffer containing such variables.  However, such Custom
;;    buffers can be created, for instance, by calling
;;    `customize-apropos' with a prefix arg or by calling
;;    `customize-option' non-interactively.

;; 7. hidden

;;    There is no widget value.

;; 8. mismatch

;;    The widget value is not valid member of the :type specified for the
;;    option.

;;; Code:

(require 'cus-face)
(require 'wid-edit)

(defvar custom-versions-load-alist)	; from cus-load
(defvar recentf-exclude)		; from recentf.el

(condition-case nil
    (require 'cus-load)
  (error nil))

(condition-case nil
    (require 'cus-start)
  (error nil))

(put 'custom-define-hook 'custom-type 'hook)
(put 'custom-define-hook 'standard-value '(nil))
(custom-add-to-group 'customize 'custom-define-hook 'custom-variable)

;;; Customization Groups.

(defgroup emacs nil
  "Customization of the One True Editor."
  :link '(custom-manual "(emacs)Top"))

;; Most of these groups are stolen from `finder.el',
(defgroup editing nil
  "Basic text editing facilities."
  :group 'emacs)

(defgroup convenience nil
  "Convenience features for faster editing."
  :group 'emacs)

(defgroup files nil
  "Support for editing files."
  :group 'emacs)

(defgroup wp nil
  "Support for editing text files."
  :tag "Text"
  :group 'emacs)

(defgroup data nil
  "Support for editing binary data files."
  :group 'emacs)

(defgroup abbrev nil
  "Abbreviation handling, typing shortcuts, macros."
  :tag "Abbreviations"
  :group 'convenience)

(defgroup matching nil
  "Various sorts of searching and matching."
  :group 'editing)

(defgroup emulations nil
  "Emulations of other editors."
  :link '(custom-manual "(emacs)Emulation")
  :group 'editing)

(defgroup mouse nil
  "Mouse support."
  :group 'editing)

(defgroup outlines nil
  "Support for hierarchical outlining."
  :group 'wp)

(defgroup external nil
  "Interfacing to external utilities."
  :group 'emacs)

(defgroup comm nil
  "Communications, networking, and remote access to files."
  :tag "Communication"
  :group 'emacs)

(defgroup processes nil
  "Process, subshell, compilation, and job control support."
  :group 'external)

(defgroup programming nil
  "Support for programming in other languages."
  :group 'emacs)

(defgroup languages nil
  "Specialized modes for editing programming languages."
  :group 'programming)

(defgroup lisp nil
  "Lisp support, including Emacs Lisp."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages
  :group 'development)

(defgroup c nil
  "Support for the C language and related languages."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :link '(custom-manual "(ccmode)")
  :group 'languages)

(defgroup tools nil
  "Programming tools."
  :group 'programming)

(defgroup applications nil
  "Applications written in Emacs."
  :group 'emacs)

(defgroup calendar nil
  "Calendar and time management support."
  :group 'applications)

(defgroup mail nil
  "Modes for electronic-mail handling."
  :group 'applications)

(defgroup news nil
  "Support for netnews reading and posting."
  :link '(custom-manual "(gnus)")
  :group 'applications)

(defgroup games nil
  "Games, jokes and amusements."
  :group 'applications)

(defgroup development nil
  "Support for further development of Emacs."
  :group 'emacs)

(defgroup docs nil
  "Support for Emacs documentation."
  :group 'development)

(defgroup extensions nil
  "Emacs Lisp language extensions."
  :group 'development)

(defgroup internal nil
  "Code for Emacs internals, build process, defaults."
  :group 'development)

(defgroup maint nil
  "Maintenance aids for the Emacs development group."
  :tag "Maintenance"
  :group 'development)

(defgroup environment nil
  "Fitting Emacs with its environment."
  :group 'emacs)

(defgroup hardware nil
  "Support for interfacing with miscellaneous hardware."
  :group 'environment)

(defgroup terminals nil
  "Support for terminal types."
  :group 'environment)

(defgroup unix nil
  "Front-ends/assistants for, or emulators of, UNIX features."
  :group 'environment)

(defgroup i18n nil
  "Internationalization and alternate character-set support."
  :link '(custom-manual "(emacs)International")
  :group 'environment
  :group 'editing)

(defgroup x nil
  "The X Window system."
  :group 'environment)

(defgroup frames nil
  "Support for Emacs frames and window systems."
  :group 'environment)

(defgroup tex nil
  "Code related to the TeX formatter."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'wp)

(defgroup faces nil
  "Support for multiple fonts."
  :group 'emacs)

(defgroup help nil
  "Support for on-line help systems."
  :group 'emacs)

(defgroup multimedia nil
  "Non-textual support, specifically images and sound."
  :group 'emacs)

(defgroup local nil
  "Code local to your site."
  :group 'emacs)

(defgroup customize '((widgets custom-group))
  "Customization of the Customization support."
  :prefix "custom-"
  :group 'help)

(defgroup custom-faces nil
  "Faces used by customize."
  :group 'customize
  :group 'faces)

(defgroup custom-browse nil
  "Control customize browser."
  :prefix "custom-"
  :group 'customize)

(defgroup custom-buffer nil
  "Control customize buffers."
  :prefix "custom-"
  :group 'customize)

(defgroup custom-menu nil
  "Control customize menus."
  :prefix "custom-"
  :group 'customize)

(defgroup alloc nil
  "Storage allocation and gc for GNU Emacs Lisp interpreter."
  :tag "Storage Allocation"
  :group 'internal)

(defgroup undo nil
  "Undoing changes in buffers."
  :link '(custom-manual "(emacs)Undo")
  :group 'editing)

(defgroup mode-line nil
  "Content of the modeline."
  :group 'environment)

(defgroup editing-basics nil
  "Most basic editing facilities."
  :group 'editing)

(defgroup display nil
  "How characters are displayed in buffers."
  :group 'environment)

(defgroup execute nil
  "Executing external commands."
  :group 'processes)

(defgroup installation nil
  "The Emacs installation."
  :group 'environment)

(defgroup dired nil
  "Directory editing."
  :group 'environment)

(defgroup limits nil
  "Internal Emacs limits."
  :group 'internal)

(defgroup debug nil
  "Debugging Emacs itself."
  :group 'development)

(defgroup keyboard nil
  "Input from the keyboard."
  :group 'environment)

(defgroup mouse nil
  "Input from the mouse."
  :group 'environment)

(defgroup menu nil
  "Input from the menus."
  :group 'environment)

(defgroup dnd nil
  "Handling data from drag and drop."
  :group 'environment)

(defgroup auto-save nil
  "Preventing accidental loss of data."
  :group 'files)

(defgroup processes-basics nil
  "Basic stuff dealing with processes."
  :group 'processes)

(defgroup mule nil
  "MULE Emacs internationalization."
  :group 'i18n)

(defgroup windows nil
  "Windows within a frame."
  :link '(custom-manual "(emacs)Windows")
  :group 'environment)

;;; Custom mode keymaps

(defvar custom-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map [remap self-insert-command] 'Custom-no-edit)
    (define-key map "\^m" 'Custom-newline)
    (define-key map " " 'scroll-up-command)
    (define-key map "\177" 'scroll-down-command)
    (define-key map "\C-c\C-c" 'Custom-set)
    (define-key map "\C-x\C-s" 'Custom-save)
    (define-key map "q" 'Custom-buffer-done)
    (define-key map "u" 'Custom-goto-parent)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `Custom-mode'.")

(defvar custom-mode-link-map
  (let ((map (make-keymap)))
    (set-keymap-parent map custom-mode-map)
    (define-key map [down-mouse-2] nil)
    (define-key map [down-mouse-1] 'mouse-drag-region)
    (define-key map [mouse-2] 'widget-move-and-invoke)
    map)
  "Local keymap for links in `Custom-mode'.")

(defvar custom-field-keymap
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map "\C-c\C-c" 'Custom-set)
    (define-key map "\C-x\C-s" 'Custom-save)
    map)
  "Keymap used inside editable fields in customization buffers.")

(widget-put (get 'editable-field 'widget-type) :keymap custom-field-keymap)

;;; Utilities.

(defun custom-split-regexp-maybe (regexp)
  "If REGEXP is a string, split it to a list at `\\|'.
You can get the original back from the result with:
  (mapconcat 'identity result \"\\|\")

IF REGEXP is not a string, return it unchanged."
  (if (stringp regexp)
      (split-string regexp "\\\\|")
    regexp))

(defun custom-variable-prompt ()
  "Prompt for a custom variable, defaulting to the variable at point.
Return a list suitable for use in `interactive'."
   (let* ((v (variable-at-point))
	  (default (and (symbolp v) (custom-variable-p v) (symbol-name v)))
	  (enable-recursive-minibuffers t)
	  val)
     (setq val (completing-read
		(if default (format "Customize variable (default %s): " default)
		  "Customize variable: ")
		obarray 'custom-variable-p t nil nil default))
     (list (if (equal val "")
	       (if (symbolp v) v nil)
	     (intern val)))))

(defun custom-menu-filter (menu widget)
  "Convert MENU to the form used by `widget-choose'.
MENU should be in the same format as `custom-variable-menu'.
WIDGET is the widget to apply the filter entries of MENU on."
  (let ((result nil)
	current name action filter)
    (while menu
      (setq current (car menu)
	    name (nth 0 current)
	    action (nth 1 current)
	    filter (nth 2 current)
	    menu (cdr menu))
      (if (or (null filter) (funcall filter widget))
	  (push (cons name action) result)
	(push name result)))
    (nreverse result)))

;;; Unlispify.

(defvar custom-prefix-list nil
  "List of prefixes that should be ignored by `custom-unlispify'.")

(defcustom custom-unlispify-menu-entries t
  "Display menu entries as words instead of symbols if non-nil."
  :group 'custom-menu
  :type 'boolean)

(defcustom custom-unlispify-remove-prefixes nil
  "Non-nil means remove group prefixes from option names in buffer."
  :group 'custom-menu
  :group 'custom-buffer
  :type 'boolean)

(defun custom-unlispify-menu-entry (symbol &optional no-suffix)
  "Convert SYMBOL into a menu entry."
  (cond ((not custom-unlispify-menu-entries)
	 (symbol-name symbol))
	((get symbol 'custom-tag)
	 (if no-suffix
	     (get symbol 'custom-tag)
	   (concat (get symbol 'custom-tag) "...")))
	(t
	 (with-current-buffer (get-buffer-create " *Custom-Work*")
	   (erase-buffer)
	   (princ symbol (current-buffer))
	   (goto-char (point-min))
	   ;; FIXME: Boolean variables are not predicates, so they shouldn't
	   ;; end with `-p'.  -stef
	   ;; (when (and (eq (get symbol 'custom-type) 'boolean)
	   ;; 	      (re-search-forward "-p\\'" nil t))
	   ;;   (replace-match "" t t)
	   ;;   (goto-char (point-min)))
	   (if custom-unlispify-remove-prefixes
	       (let ((prefixes custom-prefix-list)
		     prefix)
		 (while prefixes
		   (setq prefix (car prefixes))
		   (if (search-forward prefix (+ (point) (length prefix)) t)
		       (progn
			 (setq prefixes nil)
			 (delete-region (point-min) (point)))
		     (setq prefixes (cdr prefixes))))))
	   (subst-char-in-region (point-min) (point-max) ?- ?\  t)
	   (capitalize-region (point-min) (point-max))
	   (unless no-suffix
	     (goto-char (point-max))
	     (insert "..."))
	   (buffer-string)))))

(defcustom custom-unlispify-tag-names t
  "Display tag names as words instead of symbols if non-nil."
  :group 'custom-buffer
  :type 'boolean)

(defun custom-unlispify-tag-name (symbol)
  "Convert SYMBOL into a menu entry."
  (let ((custom-unlispify-menu-entries custom-unlispify-tag-names))
    (custom-unlispify-menu-entry symbol t)))

(defun custom-prefix-add (symbol prefixes)
  "Add SYMBOL to list of ignored PREFIXES."
  (cons (or (get symbol 'custom-prefix)
	    (concat (symbol-name symbol) "-"))
	prefixes))

;;; Guess.

(defcustom custom-guess-name-alist
  '(("-p\\'" boolean)
    ("-flag\\'" boolean)
    ("-hook\\'" hook)
    ("-face\\'" face)
    ("-file\\'" file)
    ("-function\\'" function)
    ("-functions\\'" (repeat function))
    ("-list\\'" (repeat sexp))
    ("-alist\\'" (alist :key-type sexp :value-type sexp)))
  "Alist of (MATCH TYPE).

MATCH should be a regexp matching the name of a symbol, and TYPE should
be a widget suitable for editing the value of that symbol.  The TYPE
of the first entry where MATCH matches the name of the symbol will be
used.

This is used for guessing the type of variables not declared with
customize."
  :type '(repeat (group (regexp :tag "Match") (sexp :tag "Type")))
  :group 'custom-buffer)

(defcustom custom-guess-doc-alist
  '(("\\`\\*?Non-nil " boolean))
  "Alist of (MATCH TYPE).

MATCH should be a regexp matching a documentation string, and TYPE
should be a widget suitable for editing the value of a variable with
that documentation string.  The TYPE of the first entry where MATCH
matches the name of the symbol will be used.

This is used for guessing the type of variables not declared with
customize."
  :type '(repeat (group (regexp :tag "Match") (sexp :tag "Type")))
  :group 'custom-buffer)

(defun custom-guess-type (symbol)
  "Guess a widget suitable for editing the value of SYMBOL.
This is done by matching SYMBOL with `custom-guess-name-alist' and
if that fails, the doc string with `custom-guess-doc-alist'."
  (let ((name (symbol-name symbol))
	(names custom-guess-name-alist)
	current found)
    (while names
      (setq current (car names)
	    names (cdr names))
      (when (string-match (nth 0 current) name)
	(setq found (nth 1 current)
	      names nil)))
    (unless found
      (let ((doc (documentation-property symbol 'variable-documentation))
	    (docs custom-guess-doc-alist))
	(when doc
	  (while docs
	    (setq current (car docs)
		  docs (cdr docs))
	    (when (string-match (nth 0 current) doc)
	      (setq found (nth 1 current)
		    docs nil))))))
    found))

;;; Sorting.

;;;###autoload
(defcustom custom-browse-sort-alphabetically nil
  "If non-nil, sort customization group alphabetically in `custom-browse'."
  :type 'boolean
  :group 'custom-browse)

(defcustom custom-browse-order-groups nil
  "If non-nil, order group members within each customization group.
If `first', order groups before non-groups.
If `last', order groups after non-groups."
  :type '(choice (const first)
		 (const last)
		 (const :tag "none" nil))
  :group 'custom-browse)

(defcustom custom-browse-only-groups nil
  "If non-nil, show group members only within each customization group."
  :type 'boolean
  :group 'custom-browse)

;;;###autoload
(defcustom custom-buffer-sort-alphabetically t
  "Whether to sort customization groups alphabetically in Custom buffer."
  :type 'boolean
  :group 'custom-buffer
  :version "24.1")

(defcustom custom-buffer-order-groups 'last
  "If non-nil, order group members within each customization group.
If `first', order groups before non-groups.
If `last', order groups after non-groups."
  :type '(choice (const first)
		 (const last)
		 (const :tag "none" nil))
  :group 'custom-buffer)

;;;###autoload
(defcustom custom-menu-sort-alphabetically nil
  "If non-nil, sort each customization group alphabetically in menus."
  :type 'boolean
  :group 'custom-menu)

(defcustom custom-menu-order-groups 'first
  "If non-nil, order group members within each customization group.
If `first', order groups before non-groups.
If `last', order groups after non-groups."
  :type '(choice (const first)
		 (const last)
		 (const :tag "none" nil))
  :group 'custom-menu)

(defun custom-sort-items (items sort-alphabetically order-groups)
  "Return a sorted copy of ITEMS.
ITEMS should be a `custom-group' property.
If SORT-ALPHABETICALLY non-nil, sort alphabetically.
If ORDER-GROUPS is `first' order groups before non-groups, if `last' order
groups after non-groups, if nil do not order groups at all."
  (sort (copy-sequence items)
   (lambda (a b)
     (let ((typea (nth 1 a)) (typeb (nth 1 b))
	   (namea (nth 0 a)) (nameb (nth 0 b)))
       (cond ((not order-groups)
	      ;; Since we don't care about A and B order, maybe sort.
	      (when sort-alphabetically
		(string-lessp namea nameb)))
	     ((eq typea 'custom-group)
	      ;; If B is also a group, maybe sort.  Otherwise, order A and B.
	      (if (eq typeb 'custom-group)
		  (when sort-alphabetically
		    (string-lessp namea nameb))
		(eq order-groups 'first)))
	     ((eq typeb 'custom-group)
	      ;; Since A cannot be a group, order A and B.
	      (eq order-groups 'last))
	     (sort-alphabetically
	      ;; Since A and B cannot be groups, sort.
	      (string-lessp namea nameb)))))))

;;; Custom Mode Commands.

;; This variable is used by `custom-tool-bar-map', or directly by
;; `custom-buffer-create-internal' if `custom-buffer-verbose-help' is non-nil.

(defvar custom-commands
  '((" Set for current session " Custom-set t
     "Apply all settings in this buffer to the current session"
     "index"
     "Apply")
    (" Save for future sessions " Custom-save
     (or custom-file user-init-file)
     "Apply all settings in this buffer and save them for future Emacs sessions."
     "save"
     "Save")
    (" Undo edits " Custom-reset-current t
     "Restore all settings in this buffer to reflect their current values."
     "refresh"
     "Undo")
    (" Reset to saved " Custom-reset-saved t
     "Restore all settings in this buffer to their saved values (if any)."
     "undo"
     "Reset")
    (" Erase customizations " Custom-reset-standard
     (or custom-file user-init-file)
     "Un-customize all settings in this buffer and save them with standard values."
     "delete"
     "Uncustomize")
    (" Help for Customize " Custom-help t
     "Get help for using Customize."
     "help"
     "Help")
    (" Exit " Custom-buffer-done t "Exit Customize." "exit" "Exit")))

(defun Custom-help ()
  "Read the node on Easy Customization in the Emacs manual."
  (interactive)
  (info "(emacs)Easy Customization"))

(defvar custom-reset-menu
  '(("Undo Edits" . Custom-reset-current)
    ("Reset to Saved" . Custom-reset-saved)
    ("Erase Customizations (use standard values)" . Custom-reset-standard))
  "Alist of actions for the `Reset' button.
The key is a string containing the name of the action, the value is a
Lisp function taking the widget as an element which will be called
when the action is chosen.")

(defvar custom-options nil
  "Customization widgets in the current buffer.")

(defun custom-command-apply (fun query &optional strong-query)
  "Call function FUN on all widgets in `custom-options'.
If there is more than one widget, ask user for confirmation using
the query string QUERY, using `y-or-n-p' if STRONG-QUERY is nil,
and `yes-or-no-p' otherwise."
  (if (or (and (= 1 (length custom-options))
	       (memq (widget-type (car custom-options))
		     '(custom-variable custom-face)))
	  (funcall (if strong-query 'yes-or-no-p 'y-or-n-p) query))
      (progn (mapc fun custom-options) t)
    (message "Aborted")
    nil))

(defun Custom-set (&rest _ignore)
  "Set the current value of all edited settings in the buffer."
  (interactive)
  (custom-command-apply
   (lambda (child)
     (when (eq (widget-get child :custom-state) 'modified)
       (widget-apply child :custom-set)))
   "Set all values according to this buffer? "))

(defun Custom-save (&rest _ignore)
  "Set all edited settings, then save all settings that have been set.
If a setting was edited and set before, this saves it.  If a
setting was merely edited before, this sets it then saves it."
  (interactive)
  (when (custom-command-apply
	 (lambda (child)
	   (when (memq (widget-get child :custom-state)
		       '(modified set changed rogue))
	     (widget-apply child :custom-mark-to-save)))
	 "Save all settings in this buffer? " t)
    ;; Save changes to buffer and redraw.
    (custom-save-all)
    (dolist (child custom-options)
      (widget-apply child :custom-state-set-and-redraw))))

(defun custom-reset (_widget &optional event)
  "Select item from reset menu."
  (let* ((completion-ignore-case t)
	 (answer (widget-choose "Reset settings"
				custom-reset-menu
				event)))
    (if answer
	(funcall answer))))

(defun Custom-reset-current (&rest _ignore)
  "Reset all edited settings in the buffer to show their current values."
  (interactive)
  (custom-command-apply
   (lambda (widget)
     (if (memq (widget-get widget :custom-state) '(modified changed))
	 (widget-apply widget :custom-reset-current)))
   "Reset all settings' buffer text to show current values? "))

(defun Custom-reset-saved (&rest _ignore)
  "Reset all edited or set settings in the buffer to their saved value.
This also shows the saved values in the buffer."
  (interactive)
  (custom-command-apply
   (lambda (widget)
     (if (memq (widget-get widget :custom-state) '(modified set changed rogue))
	 (widget-apply widget :custom-reset-saved)))
   "Reset all settings (current values and buffer text) to saved values? "))

;; The next two variables are bound to '(t) by `Custom-reset-standard'
;; and `custom-group-reset-standard'.  If these variables are nil, both
;; `custom-variable-reset-standard' and `custom-face-reset-standard'
;; save, reset and redraw the handled widget immediately.  Otherwise,
;; they add the widget to the corresponding list and leave it to
;; `custom-reset-standard-save-and-update' to save, reset and redraw it.
(defvar custom-reset-standard-variables-list nil)
(defvar custom-reset-standard-faces-list nil)

;; The next function was excerpted from `custom-variable-reset-standard'
;; and `custom-face-reset-standard' and is used to avoid calling
;; `custom-save-all' repeatedly (and thus saving settings to file one by
;; one) when erasing all customizations.
(defun custom-reset-standard-save-and-update ()
  "Save settings and redraw after erasing customizations."
  (when (or (and custom-reset-standard-variables-list
		 (not (eq custom-reset-standard-variables-list  '(t))))
	    (and custom-reset-standard-faces-list
		 (not (eq custom-reset-standard-faces-list '(t)))))
    ;; Save settings to file.
    (custom-save-all)
    ;; Set state of and redraw variables.
    (dolist (widget custom-reset-standard-variables-list)
      (unless (eq widget t)
	(widget-put widget :custom-state 'unknown)
	(custom-redraw widget)))
    ;; Set state of and redraw faces.
    (dolist (widget custom-reset-standard-faces-list)
      (unless (eq widget t)
	(let* ((symbol (widget-value widget))
	       (child (car (widget-get widget :children)))
	       (comment-widget (widget-get widget :comment-widget)))
	  (put symbol 'face-comment nil)
	  (widget-value-set child
			    (custom-pre-filter-face-spec
			     (list (list t (custom-face-attributes-get
					    symbol nil)))))
	  ;; This call manages the comment visibility
	  (widget-value-set comment-widget "")
	  (custom-face-state-set widget)
	  (custom-redraw-magic widget))))))

(defun Custom-reset-standard (&rest _ignore)
  "Erase all customizations (either current or saved) in current buffer.
The immediate result is to restore them to their standard values.
This operation eliminates any saved values for the group members,
making them as if they had never been customized at all."
  (interactive)
  ;; Bind these temporarily.
  (let ((custom-reset-standard-variables-list '(t))
	(custom-reset-standard-faces-list '(t)))
    (custom-command-apply
     (lambda (widget)
       (and (or (null (widget-get widget :custom-standard-value))
		(widget-apply widget :custom-standard-value))
	    (memq (widget-get widget :custom-state)
		  '(modified set changed saved rogue))
	    (widget-apply widget :custom-mark-to-reset-standard)))
     "Erase all customizations for settings in this buffer? " t)
    (custom-reset-standard-save-and-update)))

;;; The Customize Commands

(defun custom-prompt-variable (prompt-var prompt-val &optional comment)
  "Prompt for a variable and a value and return them as a list.
PROMPT-VAR is the prompt for the variable, and PROMPT-VAL is the
prompt for the value.  The %s escape in PROMPT-VAL is replaced with
the name of the variable.

If the variable has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If the variable has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.
If the variable also has a `custom-get' property, that is used for finding
the current value of the variable, otherwise `symbol-value' is used.

If optional COMMENT argument is non-nil, also prompt for a comment and return
it as the third element in the list."
  (let* ((var (read-variable prompt-var))
	 (minibuffer-help-form '(describe-variable var))
	 (val
	  (let ((prop (get var 'variable-interactive))
		(type (get var 'custom-type))
		(prompt (format prompt-val var)))
	    (unless (listp type)
	      (setq type (list type)))
	    (cond (prop
		   ;; Use VAR's `variable-interactive' property
		   ;; as an interactive spec for prompting.
		   (call-interactively `(lambda (arg)
					  (interactive ,prop)
					  arg)))
		  (type
		   (widget-prompt-value type
					prompt
					(if (boundp var)
                                            (funcall
                                             (or (get var 'custom-get) 'symbol-value)
                                             var))
					(not (boundp var))))
		  (t
		   (eval-minibuffer prompt))))))
    (if comment
 	(list var val
 	      (read-string "Comment: " (get var 'variable-comment)))
      (list var val))))

;;;###autoload
(defun customize-set-value (variable value &optional comment)
  "Set VARIABLE to VALUE, and return VALUE.  VALUE is a Lisp object.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive (custom-prompt-variable "Set variable: "
				       "Set %s to value: "
				       current-prefix-arg))

  (cond ((string= comment "")
 	 (put variable 'variable-comment nil))
 	(comment
 	 (put variable 'variable-comment comment)))
  (set variable value))

;;;###autoload
(defun customize-set-variable (variable value &optional comment)
  "Set the default for VARIABLE to VALUE, and return VALUE.
VALUE is a Lisp object.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive (custom-prompt-variable "Set variable: "
				       "Set customized value for %s to: "
				       current-prefix-arg))
  (custom-load-symbol variable)
  (custom-push-theme 'theme-value variable 'user 'set (custom-quote value))
  (funcall (or (get variable 'custom-set) 'set-default) variable value)
  (put variable 'customized-value (list (custom-quote value)))
  (cond ((string= comment "")
 	 (put variable 'variable-comment nil)
 	 (put variable 'customized-variable-comment nil))
 	(comment
 	 (put variable 'variable-comment comment)
 	 (put variable 'customized-variable-comment comment)))
  value)

;;;###autoload
(defun customize-save-variable (variable value &optional comment)
  "Set the default for VARIABLE to VALUE, and save it for future sessions.
Return VALUE.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive (custom-prompt-variable "Set and save variable: "
				       "Set and save value for %s as: "
				       current-prefix-arg))
  (funcall (or (get variable 'custom-set) 'set-default) variable value)
  (put variable 'saved-value (list (custom-quote value)))
  (custom-push-theme 'theme-value variable 'user 'set (custom-quote value))
  (cond ((string= comment "")
 	 (put variable 'variable-comment nil)
 	 (put variable 'saved-variable-comment nil))
 	(comment
 	 (put variable 'variable-comment comment)
 	 (put variable 'saved-variable-comment comment)))
  (put variable 'customized-value nil)
  (put variable 'customized-variable-comment nil)
  (if (custom-file t)
      (custom-save-all)
    (message "Setting `%s' temporarily since \"emacs -q\" would overwrite customizations"
	     variable)
    (set variable value))
  value)

;; Some parts of Emacs might prompt the user to save customizations,
;; during startup before customizations are loaded.  This function
;; handles this corner case by avoiding calling `custom-save-variable'
;; too early, which could wipe out existing customizations.

;;;###autoload
(defun customize-push-and-save (list-var elts)
  "Add ELTS to LIST-VAR and save for future sessions, safely.
ELTS should be a list.  This function adds each entry to the
value of LIST-VAR using `add-to-list'.

If Emacs is initialized, call `customize-save-variable' to save
the resulting list value now.  Otherwise, add an entry to
`after-init-hook' to save it after initialization."
  (dolist (entry elts)
    (add-to-list list-var entry))
  (if after-init-time
      (let ((coding-system-for-read nil))
	(customize-save-variable list-var (eval list-var)))
    (add-hook 'after-init-hook
	      `(lambda ()
		 (customize-push-and-save ',list-var ',elts)))))

;;;###autoload
(defun customize ()
  "Select a customization buffer which you can use to set user options.
User options are structured into \"groups\".
Initially the top-level group `Emacs' and its immediate subgroups
are shown; the contents of those subgroups are initially hidden."
  (interactive)
  (customize-group 'emacs))

;;;###autoload
(defun customize-mode (mode)
  "Customize options related to the current major mode.
If a prefix \\[universal-argument] was given (or if the current major mode has no known group),
then prompt for the MODE to customize."
  (interactive
   (list
    (let ((completion-regexp-list '("-mode\\'"))
	  (group (custom-group-of-mode major-mode)))
      (if (and group (not current-prefix-arg))
	  major-mode
	(intern
	 (completing-read (if group
			      (format "Major mode (default %s): " major-mode)
			    "Major mode: ")
			  obarray
			  'custom-group-of-mode
			  t nil nil (if group (symbol-name major-mode))))))))
  (customize-group (custom-group-of-mode mode)))

(defun customize-read-group ()
  (let ((completion-ignore-case t))
    (completing-read "Customize group (default emacs): "
                     obarray
                     (lambda (symbol)
                       (or (and (get symbol 'custom-loads)
                                (not (get symbol 'custom-autoload)))
                           (get symbol 'custom-group)))
                     t)))

;;;###autoload
(defun customize-group (&optional group other-window)
  "Customize GROUP, which must be a customization group.
If OTHER-WINDOW is non-nil, display in another window."
  (interactive (list (customize-read-group)))
  (when (stringp group)
    (if (string-equal "" group)
	(setq group 'emacs)
      (setq group (intern group))))
  (let ((name (format "*Customize Group: %s*"
		      (custom-unlispify-tag-name group))))
    (cond
     ((null (get-buffer name))
      (funcall (if other-window
		   'custom-buffer-create-other-window
		 'custom-buffer-create)
	       (list (list group 'custom-group))
	       name
	       (concat " for group "
		       (custom-unlispify-tag-name group))))
     (other-window
      (switch-to-buffer-other-window name))
     (t
      (pop-to-buffer-same-window name)))))

;;;###autoload
(defun customize-group-other-window (&optional group)
  "Customize GROUP, which must be a customization group, in another window."
  (interactive (list (customize-read-group)))
  (customize-group group t))

;;;###autoload
(defalias 'customize-variable 'customize-option)

;;;###autoload
(defun customize-option (symbol)
  "Customize SYMBOL, which must be a user option variable."
  (interactive (custom-variable-prompt))
  (unless symbol
    (error "No variable specified"))
  (let ((basevar (indirect-variable symbol)))
    (custom-buffer-create (list (list basevar 'custom-variable))
			  (format "*Customize Option: %s*"
				  (custom-unlispify-tag-name basevar)))
    (unless (eq symbol basevar)
      (message "`%s' is an alias for `%s'" symbol basevar))))

;;;###autoload
(defalias 'customize-variable-other-window 'customize-option-other-window)

;;;###autoload
(defun customize-option-other-window (symbol)
  "Customize SYMBOL, which must be a user option variable.
Show the buffer in another window, but don't select it."
  (interactive (custom-variable-prompt))
  (unless symbol
    (error "No variable specified"))
  (let ((basevar (indirect-variable symbol)))
    (custom-buffer-create-other-window
     (list (list basevar 'custom-variable))
     (format "*Customize Option: %s*" (custom-unlispify-tag-name basevar)))
    (unless (eq symbol basevar)
      (message "`%s' is an alias for `%s'" symbol basevar))))

(defvar customize-changed-options-previous-release "23.4"
  "Version for `customize-changed-options' to refer back to by default.")

;; Packages will update this variable, so make it available.
;;;###autoload
(defvar customize-package-emacs-version-alist nil
  "Alist mapping versions of a package to Emacs versions.
We use this for packages that have their own names, but are released
as part of Emacs itself.

Each elements looks like this:

     (PACKAGE (PVERSION . EVERSION)...)

Here PACKAGE is the name of a package, as a symbol.  After
PACKAGE come one or more elements, each associating a
package version PVERSION with the first Emacs version
EVERSION in which it (or a subsequent version of PACKAGE)
was first released.  Both PVERSION and EVERSION are strings.
PVERSION should be a string that this package used in
the :package-version keyword for `defcustom', `defgroup',
and `defface'.

For example, the MH-E package updates this alist as follows:

     (add-to-list 'customize-package-emacs-version-alist
                  '(MH-E (\"6.0\" . \"22.1\") (\"6.1\" . \"22.1\")
                         (\"7.0\" . \"22.1\") (\"7.1\" . \"22.1\")
                         (\"7.2\" . \"22.1\") (\"7.3\" . \"22.1\")
                         (\"7.4\" . \"22.1\") (\"8.0\" . \"22.1\")))

The value of PACKAGE needs to be unique and it needs to match the
PACKAGE value appearing in the :package-version keyword.  Since
the user might see the value in a error message, a good choice is
the official name of the package, such as MH-E or Gnus.")

;;;###autoload
(defalias 'customize-changed 'customize-changed-options)

;;;###autoload
(defun customize-changed-options (&optional since-version)
  "Customize all settings whose meanings have changed in Emacs itself.
This includes new user option variables and faces, and new
customization groups, as well as older options and faces whose meanings
or default values have changed since the previous major Emacs release.

With argument SINCE-VERSION (a string), customize all settings
that were added or redefined since that version."

  (interactive
   (list
    (read-from-minibuffer
     (format "Customize options changed, since version (default %s): "
	     customize-changed-options-previous-release))))
  (if (equal since-version "")
      (setq since-version nil)
    (unless (condition-case nil
		(numberp (read since-version))
	      (error nil))
      (signal 'wrong-type-argument (list 'numberp since-version))))
  (unless since-version
    (setq since-version customize-changed-options-previous-release))

  ;; Load the information for versions since since-version.  We use
  ;; custom-load-symbol for this.
  (put 'custom-versions-load-alist 'custom-loads nil)
  (dolist (elt custom-versions-load-alist)
    (if (customize-version-lessp since-version (car elt))
	(dolist (load (cdr elt))
	  (custom-add-load 'custom-versions-load-alist load))))
  (custom-load-symbol 'custom-versions-load-alist)
  (put 'custom-versions-load-alist 'custom-loads nil)

  (let (found)
    (mapatoms
     (lambda (symbol)
        (let* ((package-version (get symbol 'custom-package-version))
               (version
                (or (and package-version
                         (customize-package-emacs-version symbol
                                                          package-version))
                    (get symbol 'custom-version))))
	 (if version
	     (when (customize-version-lessp since-version version)
	       (if (or (get symbol 'custom-group)
		       (get symbol 'group-documentation))
		   (push (list symbol 'custom-group) found))
	       (if (custom-variable-p symbol)
		   (push (list symbol 'custom-variable) found))
	       (if (custom-facep symbol)
		   (push (list symbol 'custom-face) found)))))))
    (if found
	(custom-buffer-create (custom-sort-items found t 'first)
			      "*Customize Changed Options*")
      (error "No user option defaults have been changed since Emacs %s"
	     since-version))))

(defun customize-package-emacs-version (symbol package-version)
  "Return the Emacs version in which SYMBOL's meaning last changed.
PACKAGE-VERSION has the form (PACKAGE . VERSION).  We use
`customize-package-emacs-version-alist' to find the version of
Emacs that is associated with version VERSION of PACKAGE."
  (let (package-versions emacs-version)
    ;; Use message instead of error since we want user to be able to
    ;; see the rest of the symbols even if a package author has
    ;; botched things up.
    (cond ((not (listp package-version))
           (message "Invalid package-version value for %s" symbol))
          ((setq package-versions (assq (car package-version)
                                        customize-package-emacs-version-alist))
           (setq emacs-version
                 (cdr (assoc (cdr package-version) package-versions)))
           (unless emacs-version
             (message "%s version %s not found in %s" symbol
                      (cdr package-version)
                      "customize-package-emacs-version-alist")))
          (t
           (message "Package %s version %s lists no corresponding Emacs version"
                    (car package-version)
                    (cdr package-version))))
    emacs-version))

(defun customize-version-lessp (version1 version2)
  ;; Why are the versions strings, and given that they are, why aren't
  ;; they converted to numbers and compared as such here?  -- fx

  ;; In case someone made a mistake and left out the quotes
  ;; in the :version value.
  (if (numberp version2)
      (setq version2 (prin1-to-string version2)))
  (let (major1 major2 minor1 minor2)
    (string-match "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?" version1)
    (setq major1 (read (or (match-string 1 version1)
			   "0")))
    (setq minor1 (read (or (match-string 3 version1)
			   "0")))
    (string-match "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?" version2)
    (setq major2 (read (or (match-string 1 version2)
			   "0")))
    (setq minor2 (read (or (match-string 3 version2)
			   "0")))
    (or (< major1 major2)
	(and (= major1 major2)
	     (< minor1 minor2)))))

;;;###autoload
(defun customize-face (&optional face other-window)
  "Customize FACE, which should be a face name or nil.
If FACE is nil, customize all faces.  If FACE is actually a
face-alias, customize the face it is aliased to.

If OTHER-WINDOW is non-nil, display in another window.

Interactively, when point is on text which has a face specified,
suggest to customize that face, if it's customizable."
  (interactive (list (read-face-name "Customize face" "all faces" t)))
  (if (member face '(nil ""))
      (setq face (face-list)))
  (if (and (listp face) (null (cdr face)))
      (setq face (car face)))
  (let ((display-fun (if other-window
			 'custom-buffer-create-other-window
		       'custom-buffer-create)))
    (if (listp face)
	(funcall display-fun
		 (custom-sort-items
		  (mapcar (lambda (s) (list s 'custom-face)) face)
		  t nil)
		 "*Customize Faces*")
      ;; If FACE is actually an alias, customize the face it is aliased to.
      (if (get face 'face-alias)
	  (setq face (get face 'face-alias)))
      (unless (facep face)
	(error "Invalid face %S" face))
      (funcall display-fun
	       (list (list face 'custom-face))
	       (format "*Customize Face: %s*"
		       (custom-unlispify-tag-name face))))))

;;;###autoload
(defun customize-face-other-window (&optional face)
  "Show customization buffer for face FACE in other window.
If FACE is actually a face-alias, customize the face it is aliased to.

Interactively, when point is on text which has a face specified,
suggest to customize that face, if it's customizable."
  (interactive (list (read-face-name "Customize face" "all faces" t)))
  (customize-face face t))

(defalias 'customize-customized 'customize-unsaved)

;;;###autoload
(defun customize-unsaved ()
  "Customize all options and faces set in this session but not saved."
  (interactive)
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(and (or (get symbol 'customized-face)
			 (get symbol 'customized-face-comment))
		     (custom-facep symbol)
		     (push (list symbol 'custom-face) found))
		(and (or (get symbol 'customized-value)
			 (get symbol 'customized-variable-comment))
		     (boundp symbol)
		     (push (list symbol 'custom-variable) found))))
    (if (not found)
	(error "No user options are set but unsaved")
      (custom-buffer-create (custom-sort-items found t nil)
			    "*Customize Unsaved*"))))

;;;###autoload
(defun customize-rogue ()
  "Customize all user variables modified outside customize."
  (interactive)
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(let ((cval (or (get symbol 'customized-value)
				(get symbol 'saved-value)
				(get symbol 'standard-value))))
		  (when (and cval 	;Declared with defcustom.
			     (default-boundp symbol) ;Has a value.
			     (not (equal (eval (car cval))
					 ;; Which does not match customize.
					 (default-value symbol))))
		    (push (list symbol 'custom-variable) found)))))
    (if (not found)
	(error "No rogue user options")
      (custom-buffer-create (custom-sort-items found t nil)
			    "*Customize Rogue*"))))
;;;###autoload
(defun customize-saved ()
  "Customize all saved options and faces."
  (interactive)
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(and (or (get symbol 'saved-face)
			 (get symbol 'saved-face-comment))
		     (custom-facep symbol)
		     (push (list symbol 'custom-face) found))
		(and (or (get symbol 'saved-value)
			 (get symbol 'saved-variable-comment))
		     (boundp symbol)
		     (push (list symbol 'custom-variable) found))))
    (if (not found )
	(error "No saved user options")
      (custom-buffer-create (custom-sort-items found t nil)
			    "*Customize Saved*"))))

(declare-function apropos-parse-pattern "apropos" (pattern))

;;;###autoload
(defun customize-apropos (pattern &optional type)
  "Customize all loaded options, faces and groups matching PATTERN.
PATTERN can be a word, a list of words (separated by spaces),
or a regexp (using some regexp special characters).  If it is a word,
search for matches for that word as a substring.  If it is a list of words,
search for matches for any two (or more) of those words.

If TYPE is `options', include only options.
If TYPE is `faces', include only faces.
If TYPE is `groups', include only groups.
If TYPE is t (interactively, with prefix arg), include variables
that are not customizable options, as well as faces and groups
\(but we recommend using `apropos-variable' instead)."
  (interactive (list (apropos-read-pattern "symbol") current-prefix-arg))
  (require 'apropos)
  (apropos-parse-pattern pattern)
  (let (found)
    (mapatoms
     `(lambda (symbol)
	(when (string-match apropos-regexp (symbol-name symbol))
	  ,(if (not (memq type '(faces options)))
	       '(if (get symbol 'custom-group)
		    (push (list symbol 'custom-group) found)))
	  ,(if (not (memq type '(options groups)))
	       '(if (custom-facep symbol)
		    (push (list symbol 'custom-face) found)))
	  ,(if (not (memq type '(groups faces)))
	       `(if (and (boundp symbol)
			 (eq (indirect-variable symbol) symbol)
			 (or (get symbol 'saved-value)
			     (custom-variable-p symbol)
			     ,(if (not (memq type '(nil options)))
				  '(get symbol 'variable-documentation))))
		    (push (list symbol 'custom-variable) found))))))
    (if (not found)
	(error "No %s matching %s"
	       (if (eq type t)
		   "items"
		 (format "customizable %s"
			 (if (memq type '(options faces groups))
			     (symbol-name type)
			   "items")))
	       pattern)
      (custom-buffer-create
       (custom-sort-items found t custom-buffer-order-groups)
       "*Customize Apropos*"))))

;;;###autoload
(defun customize-apropos-options (regexp &optional arg)
  "Customize all loaded customizable options matching REGEXP.
With prefix ARG, include variables that are not customizable options
\(but it is better to use `apropos-variable' if you want to find those)."
  (interactive "sCustomize options (regexp): \nP")
  (customize-apropos regexp (or arg 'options)))

;;;###autoload
(defun customize-apropos-faces (regexp)
  "Customize all loaded faces matching REGEXP."
  (interactive "sCustomize faces (regexp): \n")
  (customize-apropos regexp 'faces))

;;;###autoload
(defun customize-apropos-groups (regexp)
  "Customize all loaded groups matching REGEXP."
  (interactive "sCustomize groups (regexp): \n")
  (customize-apropos regexp 'groups))

;;; Buffer.

(defcustom custom-buffer-style 'links
  "Control the presentation style for customization buffers.
The value should be a symbol, one of:

brackets: groups nest within each other with big horizontal brackets.
links: groups have links to subgroups."
  :type '(radio (const brackets)
		(const links))
  :group 'custom-buffer)

(defcustom custom-buffer-done-kill nil
  "Non-nil means exiting a Custom buffer should kill it."
  :type 'boolean
  :version "22.1"
  :group 'custom-buffer)

(defcustom custom-buffer-indent 3
  "Number of spaces to indent nested groups."
  :type 'integer
  :group 'custom-buffer)

(defun custom-get-fresh-buffer (name)
  "Get a fresh new buffer with name NAME.
If the buffer already exist, clean it up to be like new.
Beware: it's not quite like new.  Good enough for custom, but maybe
not for everybody."
  ;; To be more complete, we should also kill all permanent-local variables,
  ;; but it's not needed for custom.
  (let ((buf (get-buffer name)))
    (when (and buf (buffer-local-value 'buffer-file-name buf))
      ;; This will check if the file is not saved.
      (kill-buffer buf)
      (setq buf nil))
    (if (null buf)
	(get-buffer-create name)
      (with-current-buffer buf
	(kill-all-local-variables)
	(run-hooks 'kill-buffer-hook)
	;; Delete overlays before erasing the buffer so the overlay hooks
	;; don't get run spuriously when we erase the buffer.
	(let ((ols (overlay-lists)))
	  (dolist (ol (nconc (car ols) (cdr ols)))
	    (delete-overlay ol)))
	(erase-buffer)
	buf))))

;;;###autoload
(defun custom-buffer-create (options &optional name description)
  "Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option."
  (pop-to-buffer-same-window (custom-get-fresh-buffer (or name "*Customization*")))
  (custom-buffer-create-internal options description))

;;;###autoload
(defun custom-buffer-create-other-window (options &optional name description)
  "Create a buffer containing OPTIONS, and display it in another window.
The result includes selecting that window.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option."
  (unless name (setq name "*Customization*"))
  (switch-to-buffer-other-window (custom-get-fresh-buffer name))
  (custom-buffer-create-internal options description))

(defcustom custom-reset-button-menu nil
  "If non-nil, only show a single reset button in customize buffers.
This button will have a menu with all three reset operations."
  :type 'boolean
  :group 'custom-buffer)

(defcustom custom-buffer-verbose-help t
  "If non-nil, include explanatory text in the customization buffer."
  :type 'boolean
  :group 'custom-buffer)

(defun Custom-buffer-done (&rest _ignore)
  "Exit current Custom buffer according to `custom-buffer-done-kill'."
  (interactive)
  (quit-window custom-buffer-done-kill))

(defvar custom-button nil
  "Face used for buttons in customization buffers.")

(defvar custom-button-mouse nil
  "Mouse face used for buttons in customization buffers.")

(defvar custom-button-pressed nil
  "Face used for pressed buttons in customization buffers.")

(defcustom custom-search-field t
  "If non-nil, show a search field in Custom buffers."
  :type 'boolean
  :version "24.1"
  :group 'custom-buffer)

(defcustom custom-raised-buttons (not (equal (face-valid-attribute-values :box)
					     '(("unspecified" . unspecified))))
  "If non-nil, indicate active buttons in a `raised-button' style.
Otherwise use brackets."
  :type 'boolean
  :version "21.1"
  :group 'custom-buffer
  :set (lambda (variable value)
	 (custom-set-default variable value)
	 (setq custom-button
	       (if value 'custom-button 'custom-button-unraised))
	 (setq custom-button-mouse
	       (if value 'custom-button-mouse 'highlight))
	 (setq custom-button-pressed
	       (if value
		   'custom-button-pressed
		 'custom-button-pressed-unraised))))

(defun custom-buffer-create-internal (options &optional _description)
  (Custom-mode)
  (let ((init-file (or custom-file user-init-file)))
    ;; Insert verbose help at the top of the custom buffer.
    (when custom-buffer-verbose-help
      (widget-insert (if init-file
			 "To apply changes, use the Save or Set buttons."
		       "Custom settings cannot be saved; maybe you started Emacs with `-q'.")
		     "\nFor details, see ")
      (widget-create 'custom-manual
		     :tag "Saving Customizations"
		     "(emacs)Saving Customizations")
      (widget-insert " in the ")
      (widget-create 'custom-manual
		     :tag "Emacs manual"
		     :help-echo "Read the Emacs manual."
		     "(emacs)Top")
      (widget-insert "."))
    (widget-insert "\n")

    ;; Insert the search field.
    (when custom-search-field
      (widget-insert "\n")
      (let* ((echo "Search for custom items.
You can enter one or more words separated by spaces,
or a regular expression.")
	     (search-widget
	      (widget-create
	       'editable-field
	       :size 40 :help-echo echo
	       :action `(lambda (widget &optional event)
			  (customize-apropos (split-string (widget-value widget)))))))
	(widget-insert " ")
	(widget-create-child-and-convert
	 search-widget 'push-button
	 :tag " Search "
	 :help-echo echo :action
	 (lambda (widget &optional _event)
	   (customize-apropos (split-string (widget-value (widget-get widget :parent))))))
	(widget-insert "\n")))

    ;; The custom command buttons are also in the toolbar, so for a
    ;; time they were not inserted in the buffer if the toolbar was in use.
    ;; But it can be a little confusing for the buffer layout to
    ;; change according to whether or nor the toolbar is on, not to
    ;; mention that a custom buffer can in theory be created in a
    ;; frame with a toolbar, then later viewed in one without.
    ;; So now the buttons are always inserted in the buffer.  (Bug#1326)
    (if custom-buffer-verbose-help
	(widget-insert "
 Operate on all settings in this buffer:\n"))
    (let ((button (lambda (tag action active help _icon _label)
		    (widget-insert " ")
		    (if (eval active)
			(widget-create 'push-button :tag tag
				       :help-echo help :action action))))
	  (commands custom-commands))
      (apply button (pop commands)) ; Set for current session
      (apply button (pop commands)) ; Save for future sessions
      (if custom-reset-button-menu
	  (progn
	    (widget-insert " ")
	    (widget-create 'push-button
			   :tag "Reset buffer"
			   :help-echo "Show a menu with reset operations."
			   :mouse-down-action 'ignore
			   :action 'custom-reset))
	(widget-insert "\n")
	(apply button (pop commands)) ; Undo edits
	(apply button (pop commands)) ; Reset to saved
	(apply button (pop commands)) ; Erase customization
	(widget-insert "  ")
	(pop commands) ; Help (omitted)
	(apply button (pop commands)))) ; Exit
    (widget-insert "\n\n"))

  ;; Now populate the custom buffer.
  (message "Creating customization items...")
  (buffer-disable-undo)
  (setq custom-options
	(if (= (length options) 1)
	    (mapcar (lambda (entry)
		      (widget-create (nth 1 entry)
				     :documentation-shown t
				     :custom-state 'unknown
				     :tag (custom-unlispify-tag-name
					   (nth 0 entry))
				     :value (nth 0 entry)))
		    options)
	  (let ((count 0)
		(length (length options)))
	    (mapcar (lambda (entry)
		      (prog2
			  (message "Creating customization items ...%2d%%"
				   (/ (* 100.0 count) length))
			  (widget-create (nth 1 entry)
					 :tag (custom-unlispify-tag-name
					       (nth 0 entry))
					 :value (nth 0 entry))
			(setq count (1+ count))
			(unless (eq (preceding-char) ?\n)
			  (widget-insert "\n"))
			(widget-insert "\n")))
		    options))))
  (unless (eq (preceding-char) ?\n)
    (widget-insert "\n"))
  (message "Creating customization items ...done")
  (message "Resetting customization items...")
  (unless (eq custom-buffer-style 'tree)
    (mapc 'custom-magic-reset custom-options))
  (message "Resetting customization items...done")
  (message "Creating customization setup...")
  (widget-setup)
  (buffer-enable-undo)
  (goto-char (point-min))
  (message "Creating customization setup...done"))

;;; The Tree Browser.

;;;###autoload
(defun customize-browse (&optional group)
  "Create a tree browser for the customize hierarchy."
  (interactive)
  (unless group
    (setq group 'emacs))
  (let ((name "*Customize Browser*"))
    (pop-to-buffer-same-window (custom-get-fresh-buffer name)))
  (Custom-mode)
  (widget-insert (format "\
%s buttons; type RET or click mouse-1
on a button to invoke its action.
Invoke [+] to expand a group, and [-] to collapse an expanded group.\n"
			 (if custom-raised-buttons
			     "`Raised' text indicates"
			   "Square brackets indicate")))


  (if custom-browse-only-groups
      (widget-insert "\
Invoke the [Group] button below to edit that item in another window.\n\n")
    (widget-insert "Invoke the ")
    (widget-create 'item
		   :format "%t"
		   :tag "[Group]"
		   :tag-glyph "folder")
    (widget-insert ", ")
    (widget-create 'item
		   :format "%t"
		   :tag "[Face]"
		   :tag-glyph "face")
    (widget-insert ", and ")
    (widget-create 'item
		   :format "%t"
		   :tag "[Option]"
		   :tag-glyph "option")
    (widget-insert " buttons below to edit that
item in another window.\n\n"))
  (let ((custom-buffer-style 'tree))
    (widget-create 'custom-group
		   :custom-last t
		   :custom-state 'unknown
		   :tag (custom-unlispify-tag-name group)
		   :value group))
  (widget-setup)
  (goto-char (point-min)))

(define-widget 'custom-browse-visibility 'item
  "Control visibility of items in the customize tree browser."
  :format "%[[%t]%]"
  :action 'custom-browse-visibility-action)

(defun custom-browse-visibility-action (widget &rest _ignore)
  (let ((custom-buffer-style 'tree))
    (custom-toggle-parent widget)))

(define-widget 'custom-browse-group-tag 'custom-group-link
  "Show parent in other window when activated."
  :tag "Group"
  :tag-glyph "folder"
  :action 'custom-browse-group-tag-action)

(defun custom-browse-group-tag-action (widget &rest _ignore)
  (let ((parent (widget-get widget :parent)))
    (customize-group-other-window (widget-value parent))))

(define-widget 'custom-browse-variable-tag 'custom-group-link
  "Show parent in other window when activated."
  :tag "Option"
  :tag-glyph "option"
  :action 'custom-browse-variable-tag-action)

(defun custom-browse-variable-tag-action (widget &rest _ignore)
  (let ((parent (widget-get widget :parent)))
    (customize-variable-other-window (widget-value parent))))

(define-widget 'custom-browse-face-tag 'custom-group-link
  "Show parent in other window when activated."
  :tag "Face"
  :tag-glyph "face"
  :action 'custom-browse-face-tag-action)

(defun custom-browse-face-tag-action (widget &rest _ignore)
  (let ((parent (widget-get widget :parent)))
    (customize-face-other-window (widget-value parent))))

(defconst custom-browse-alist '(("   " "space")
			      (" | " "vertical")
			      ("-\\ " "top")
			      (" |-" "middle")
			      (" `-" "bottom")))

(defun custom-browse-insert-prefix (prefix)
  "Insert PREFIX.  On XEmacs convert it to line graphics."
  ;; Fixme: do graphics.
  (if nil ; (featurep 'xemacs)
      (progn
	(insert "*")
	(while (not (string-equal prefix ""))
	  (let ((entry (substring prefix 0 3)))
	    (setq prefix (substring prefix 3))
	    (let ((overlay (make-overlay (1- (point)) (point) nil t nil))
		  (name (nth 1 (assoc entry custom-browse-alist))))
	      (overlay-put overlay 'end-glyph (widget-glyph-find name entry))
	      (overlay-put overlay 'start-open t)
	      (overlay-put overlay 'end-open t)))))
    (insert prefix)))

;;; Modification of Basic Widgets.
;;
;; We add extra properties to the basic widgets needed here.  This is
;; fine, as long as we are careful to stay within our own namespace.
;;
;; We want simple widgets to be displayed by default, but complex
;; widgets to be hidden.

;; This widget type is obsolete as of Emacs 24.1.
(widget-put (get 'item 'widget-type) :custom-show t)
(widget-put (get 'editable-field 'widget-type)
	    :custom-show (lambda (_widget value)
			   (let ((pp (pp-to-string value)))
			     (cond ((string-match "\n" pp)
				    nil)
				   ((> (length pp) 40)
				    nil)
				   (t t)))))
(widget-put (get 'menu-choice 'widget-type) :custom-show t)

;;; The `custom-manual' Widget.

(define-widget 'custom-manual 'info-link
  "Link to the manual entry for this customization option."
  :help-echo "Read the manual entry for this option."
  :keymap custom-mode-link-map
  :follow-link 'mouse-face
  :button-face 'custom-link
  :mouse-face 'highlight
  :pressed-face 'highlight
  :tag "Manual")

;;; The `custom-magic' Widget.

(defgroup custom-magic-faces nil
  "Faces used by the magic button."
  :group 'custom-faces
  :group 'custom-buffer)

(defface custom-invalid '((((class color))
			   (:foreground "yellow1" :background "red1"))
			  (t
			   (:weight bold :slant italic :underline t)))
  "Face used when the customize item is invalid."
  :group 'custom-magic-faces)
(define-obsolete-face-alias 'custom-invalid-face 'custom-invalid "22.1")

(defface custom-rogue '((((class color))
			 (:foreground "pink" :background "black"))
			(t
			 (:underline t)))
  "Face used when the customize item is not defined for customization."
  :group 'custom-magic-faces)
(define-obsolete-face-alias 'custom-rogue-face 'custom-rogue "22.1")

(defface custom-modified '((((min-colors 88) (class color))
			    (:foreground "white" :background "blue1"))
			   (((class color))
			    (:foreground "white" :background "blue"))
			   (t
			    (:slant italic :bold)))
  "Face used when the customize item has been modified."
  :group 'custom-magic-faces)
(define-obsolete-face-alias 'custom-modified-face 'custom-modified "22.1")

(defface custom-set '((((min-colors 88) (class color))
		       (:foreground "blue1" :background "white"))
		      (((class color))
		       (:foreground "blue" :background "white"))
		      (t
		       (:slant italic)))
  "Face used when the customize item has been set."
  :group 'custom-magic-faces)
(define-obsolete-face-alias 'custom-set-face 'custom-set "22.1")

(defface custom-changed '((((min-colors 88) (class color))
			   (:foreground "white" :background "blue1"))
			  (((class color))
			   (:foreground "white" :background "blue"))
			  (t
			   (:slant italic)))
  "Face used when the customize item has been changed."
  :group 'custom-magic-faces)
(define-obsolete-face-alias 'custom-changed-face 'custom-changed "22.1")

(defface custom-themed '((((min-colors 88) (class color))
			   (:foreground "white" :background "blue1"))
			  (((class color))
			   (:foreground "white" :background "blue"))
			  (t
			   (:slant italic)))
  "Face used when the customize item has been set by a theme."
  :group 'custom-magic-faces)

(defface custom-saved '((t (:underline t)))
  "Face used when the customize item has been saved."
  :group 'custom-magic-faces)
(define-obsolete-face-alias 'custom-saved-face 'custom-saved "22.1")

(defconst custom-magic-alist
  '((nil "#" underline "\
UNINITIALIZED, you should not see this.")
    (unknown "?" italic "\
UNKNOWN, you should not see this.")
    (hidden "-" default "\
HIDDEN, invoke \"Show\" in the previous line to show." "\
group now hidden, invoke \"Show\", above, to show contents.")
    (invalid "x" custom-invalid "\
INVALID, the displayed value cannot be set.")
    (modified "*" custom-modified "\
EDITED, shown value does not take effect until you set or save it." "\
something in this group has been edited but not set.")
    (set "+" custom-set "\
SET for current session only." "\
something in this group has been set but not saved.")
    (changed ":" custom-changed "\
CHANGED outside Customize." "\
something in this group has been changed outside customize.")
    (saved "!" custom-saved "\
SAVED and set." "\
something in this group has been set and saved.")
    (themed "o" custom-themed "\
THEMED." "\
visible group members are all at standard values.")
    (rogue "@" custom-rogue "\
NO CUSTOMIZATION DATA; not intended to be customized." "\
something in this group is not prepared for customization.")
    (standard " " nil "\
STANDARD." "\
visible group members are all at standard values."))
  "Alist of customize option states.
Each entry is of the form (STATE MAGIC FACE ITEM-DESC [ GROUP-DESC ]), where

STATE is one of the following symbols:

`nil'
   For internal use, should never occur.
`unknown'
   For internal use, should never occur.
`hidden'
   This item is not being displayed.
`invalid'
   This item is modified, but has an invalid form.
`modified'
   This item is modified, and has a valid form.
`set'
   This item has been set but not saved.
`changed'
   The current value of this item has been changed outside Customize.
`saved'
   This item is marked for saving.
`rogue'
   This item has no customization information.
`standard'
   This item is unchanged from the standard setting.

MAGIC is a string used to present that state.

FACE is a face used to present the state.

ITEM-DESC is a string describing the state for options.

GROUP-DESC is a string describing the state for groups.  If this is
left out, ITEM-DESC will be used.

The string %c in either description will be replaced with the
category of the item.  These are `group'. `option', and `face'.

The list should be sorted most significant first.")

(defcustom custom-magic-show 'long
  "If non-nil, show textual description of the state.
If `long', show a full-line description, not just one word."
  :type '(choice (const :tag "no" nil)
		 (const long)
		 (other :tag "short" short))
  :group 'custom-buffer)

(defcustom custom-magic-show-hidden '(option face)
  "Control whether the State button is shown for hidden items.
The value should be a list with the custom categories where the State
button should be visible.  Possible categories are `group', `option',
and `face'."
  :type '(set (const group) (const option) (const face))
  :group 'custom-buffer)

(defcustom custom-magic-show-button nil
  "Show a \"magic\" button indicating the state of each customization option."
  :type 'boolean
  :group 'custom-buffer)

(define-widget 'custom-magic 'default
  "Show and manipulate state for a customization option."
  :format "%v"
  :action 'widget-parent-action
  :notify 'ignore
  :value-get 'ignore
  :value-create 'custom-magic-value-create
  :value-delete 'widget-children-value-delete)

(defun widget-magic-mouse-down-action (widget &optional _event)
  ;; Non-nil unless hidden.
  (not (eq (widget-get (widget-get (widget-get widget :parent) :parent)
		       :custom-state)
	   'hidden)))

(defun custom-magic-value-create (widget)
  "Create compact status report for WIDGET."
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state))
	 (hidden (eq state 'hidden))
	 (entry (assq state custom-magic-alist))
	 (magic (nth 1 entry))
	 (face (nth 2 entry))
	 (category (widget-get parent :custom-category))
	 (text (or (and (eq category 'group)
			(nth 4 entry))
		   (nth 3 entry)))
	 (form (widget-get parent :custom-form))
	 children)
    (unless (eq state 'hidden)
      (while (string-match "\\`\\(.*\\)%c\\(.*\\)\\'" text)
	(setq text (concat (match-string 1 text)
			   (symbol-name category)
			   (match-string 2 text))))
      (when (and custom-magic-show
		 (or (not hidden)
		     (memq category custom-magic-show-hidden)))
	(insert "   ")
	(when (and (eq category 'group)
		   (not (and (eq custom-buffer-style 'links)
			     (> (widget-get parent :custom-level) 1))))
	  (insert-char ?\  (* custom-buffer-indent
			      (widget-get parent :custom-level))))
	(push (widget-create-child-and-convert
	       widget 'choice-item
	       :help-echo "Change the state of this item."
	       :format (if hidden "%t" "%[%t%]")
	       :button-prefix 'widget-push-button-prefix
	       :button-suffix 'widget-push-button-suffix
	       :mouse-down-action 'widget-magic-mouse-down-action
	       :tag " State ")
	      children)
	(insert ": ")
	(let ((start (point)))
	  (if (eq custom-magic-show 'long)
	      (insert text)
	    (insert (symbol-name state)))
	  (cond ((eq form 'lisp)
		 (insert " (lisp)"))
		((eq form 'mismatch)
		 (insert " (mismatch)")))
	  (put-text-property start (point) 'face 'custom-state))
	(insert "\n"))
      (when (and (eq category 'group)
		 (not (and (eq custom-buffer-style 'links)
			   (> (widget-get parent :custom-level) 1))))
	(insert-char ?\  (* custom-buffer-indent
			    (widget-get parent :custom-level))))
      (when custom-magic-show-button
	(when custom-magic-show
	  (let ((indent (widget-get parent :indent)))
	    (when indent
	      (insert-char ?  indent))))
	(push (widget-create-child-and-convert
	       widget 'choice-item
	       :mouse-down-action 'widget-magic-mouse-down-action
	       :button-face face
	       :button-prefix ""
	       :button-suffix ""
	       :help-echo "Change the state."
	       :format (if hidden "%t" "%[%t%]")
	       :tag (if (memq form '(lisp mismatch))
			(concat "(" magic ")")
		      (concat "[" magic "]")))
	      children)
	(insert " "))
      (widget-put widget :children children))))

(defun custom-magic-reset (widget)
  "Redraw the :custom-magic property of WIDGET."
  (let ((magic (widget-get widget :custom-magic)))
    (when magic
      (widget-value-set magic (widget-value magic)))))

;;; The `custom' Widget.

(defface custom-button
  '((((type x w32 ns) (class color))	; Like default modeline
     (:box (:line-width 2 :style released-button)
	   :background "lightgrey" :foreground "black"))
    (t
     nil))
  "Face for custom buffer buttons if `custom-raised-buttons' is non-nil."
  :version "21.1"
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-button-face 'custom-button "22.1")

(defface custom-button-mouse
  '((((type x w32 ns) (class color))
     (:box (:line-width 2 :style released-button)
	   :background "grey90" :foreground "black"))
    (t
     ;; This is for text terminals that support mouse, like GPM mouse
     ;; or the MS-DOS terminal: inverse-video makes the button stand
     ;; out on mouse-over.
     (:inverse-video t)))
  "Mouse face for custom buffer buttons if `custom-raised-buttons' is non-nil."
  :version "22.1"
  :group 'custom-faces)

(defface custom-button-unraised
  '((t :inherit underline))
  "Face for custom buffer buttons if `custom-raised-buttons' is nil."
  :version "22.1"
  :group 'custom-faces)

(setq custom-button
      (if custom-raised-buttons 'custom-button 'custom-button-unraised))

(setq custom-button-mouse
      (if custom-raised-buttons 'custom-button-mouse 'highlight))

(defface custom-button-pressed
  '((((type x w32 ns) (class color))
     (:box (:line-width 2 :style pressed-button)
	   :background "lightgrey" :foreground "black"))
    (t
     (:inverse-video t)))
  "Face for pressed custom buttons if `custom-raised-buttons' is non-nil."
  :version "21.1"
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-button-pressed-face
  'custom-button-pressed "22.1")

(defface custom-button-pressed-unraised
  '((default :inherit custom-button-unraised)
    (((class color) (background light)) :foreground "magenta4")
    (((class color) (background dark)) :foreground "violet"))
  "Face for pressed custom buttons if `custom-raised-buttons' is nil."
  :version "22.1"
  :group 'custom-faces)

(setq custom-button-pressed
  (if custom-raised-buttons
      'custom-button-pressed
    'custom-button-pressed-unraised))

(defface custom-documentation '((t nil))
  "Face used for documentation strings in customization buffers."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-documentation-face
  'custom-documentation "22.1")

(defface custom-state '((((class color)
			  (background dark))
			 (:foreground "lime green"))
			(((class color)
			  (background light))
			 (:foreground "dark green"))
			(t nil))
  "Face used for State descriptions in the customize buffer."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-state-face 'custom-state "22.1")

(defface custom-link
  '((t :inherit link))
  "Face for links in customization buffers."
  :version "22.1"
  :group 'custom-faces)

(define-widget 'custom 'default
  "Customize a user option."
  :format "%v"
  :convert-widget 'custom-convert-widget
  :notify 'custom-notify
  :custom-prefix ""
  :custom-level 1
  :custom-state 'hidden
  :documentation-property 'widget-subclass-responsibility
  :value-create 'widget-subclass-responsibility
  :value-delete 'widget-children-value-delete
  :value-get 'widget-value-value-get
  :validate 'widget-children-validate
  :match (lambda (_widget value) (symbolp value)))

(defun custom-convert-widget (widget)
  "Initialize :value and :tag from :args in WIDGET."
  (let ((args (widget-get widget :args)))
    (when args
      (widget-put widget :value (widget-apply widget
					      :value-to-internal (car args)))
      (widget-put widget :tag (custom-unlispify-tag-name (car args)))
      (widget-put widget :args nil)))
  widget)

(defun custom-notify (widget &rest args)
  "Keep track of changes."
  (let ((state (widget-get widget :custom-state)))
    (unless (eq state 'modified)
      (unless (memq state '(nil unknown hidden))
	(widget-put widget :custom-state 'modified))
      (custom-magic-reset widget)
      (apply 'widget-default-notify widget args))))

(defun custom-redraw (widget)
  "Redraw WIDGET with current settings."
  (let ((line (count-lines (point-min) (point)))
	(column (current-column))
	(pos (point))
	(from (marker-position (widget-get widget :from)))
	(to (marker-position (widget-get widget :to))))
    (save-excursion
      (widget-value-set widget (widget-value widget))
      (custom-redraw-magic widget))
    (when (and (>= pos from) (<= pos to))
      (condition-case nil
	  (progn
	    (goto-char (point-min))
	    (forward-line (if (> column 0)
			      (1- line)
			    line))
	    (move-to-column column))
	(error nil)))))

(defun custom-redraw-magic (widget)
  "Redraw WIDGET state with current settings."
  (while widget
    (let ((magic (widget-get widget :custom-magic)))
      (cond (magic
	     (widget-value-set magic (widget-value magic))
	     (when (setq widget (widget-get widget :group))
	       (custom-group-state-update widget)))
	    (t
	     (setq widget nil)))))
  (widget-setup))

(make-obsolete 'custom-show "this widget type is no longer supported." "24.1")
(defun custom-show (widget value)
  "Non-nil if WIDGET should be shown with VALUE by default."
  (let ((show (widget-get widget :custom-show)))
    (if (functionp show)
	(funcall show widget value)
      show)))

(defun custom-load-widget (widget)
  "Load all dependencies for WIDGET."
  (custom-load-symbol (widget-value widget)))

(defun custom-unloaded-symbol-p (symbol)
  "Return non-nil if the dependencies of SYMBOL have not yet been loaded."
  (let ((found nil)
	(loads (get symbol 'custom-loads))
	load)
    (while loads
      (setq load (car loads)
	    loads (cdr loads))
      (cond ((symbolp load)
	     (unless (featurep load)
	       (setq found t)))
	    ((assoc load load-history))
	    ((assoc (locate-library load) load-history)
	     (message nil))
	    (t
	     (setq found t))))
    found))

(defun custom-unloaded-widget-p (widget)
  "Return non-nil if the dependencies of WIDGET have not yet been loaded."
  (custom-unloaded-symbol-p (widget-value widget)))

(defun custom-toggle-hide (widget)
  "Toggle visibility of WIDGET."
  (custom-load-widget widget)
  (let ((state (widget-get widget :custom-state)))
    (cond ((memq state '(invalid modified set))
	   (error "There are unsaved changes"))
	  ((eq state 'hidden)
	   (widget-put widget :custom-state 'unknown))
	  (t
	   (widget-put widget :documentation-shown nil)
	   (widget-put widget :custom-state 'hidden)))
    (custom-redraw widget)
    (widget-setup)))

(defun custom-toggle-parent (widget &rest _ignore)
  "Toggle visibility of parent of WIDGET."
  (custom-toggle-hide (widget-get widget :parent)))

(defun custom-add-see-also (widget &optional prefix)
  "Add `See also ...' to WIDGET if there are any links.
Insert PREFIX first if non-nil."
  (let* ((symbol (widget-get widget :value))
	 (links (get symbol 'custom-links))
	 (many (> (length links) 2))
	 (buttons (widget-get widget :buttons))
	 (indent (widget-get widget :indent)))
    (when links
      (when indent
	(insert-char ?\  indent))
      (when prefix
	(insert prefix))
      (insert "See also ")
      (while links
	(push (widget-create-child-and-convert
	       widget (car links)
	       :button-face 'custom-link
	       :mouse-face 'highlight
	       :pressed-face 'highlight)
	      buttons)
	(setq links (cdr links))
	(cond ((null links)
	       (insert ".\n"))
	      ((null (cdr links))
	       (if many
		   (insert ", and ")
		 (insert " and ")))
	      (t
	       (insert ", "))))
      (widget-put widget :buttons buttons))))

(defun custom-add-parent-links (widget &optional initial-string _doc-initial-string)
  "Add \"Parent groups: ...\" to WIDGET if the group has parents.
The value is non-nil if any parents were found.
If INITIAL-STRING is non-nil, use that rather than \"Parent groups:\"."
  (let ((name (widget-value widget))
	(type (widget-type widget))
	(buttons (widget-get widget :buttons))
	(start (point))
	(parents nil))
    (insert (or initial-string "Groups:"))
    (mapatoms (lambda (symbol)
		(when (member (list name type) (get symbol 'custom-group))
		  (insert " ")
		  (push (widget-create-child-and-convert
			 widget 'custom-group-link
			 :tag (custom-unlispify-tag-name symbol)
			 symbol)
			buttons)
		  (setq parents (cons symbol parents)))))
    (if parents
        (insert "\n")
      (delete-region start (point)))
    (widget-put widget :buttons buttons)
    parents))

;;; The `custom-comment' Widget.

;; like the editable field
(defface custom-comment '((((type tty))
			   :background "yellow3"
			   :foreground "black")
			  (((class grayscale color)
			    (background light))
			   :background "gray85")
			  (((class grayscale color)
			    (background dark))
			   :background "dim gray")
			  (t
			   :slant italic))
  "Face used for comments on variables or faces."
  :version "21.1"
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-comment-face 'custom-comment "22.1")

;; like font-lock-comment-face
(defface custom-comment-tag
  '((((class color) (background dark)) (:foreground "gray80"))
    (((class color) (background light)) (:foreground "blue4"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (t (:weight bold)))
  "Face used for the comment tag on variables or faces."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-comment-tag-face 'custom-comment-tag "22.1")

(define-widget 'custom-comment 'string
  "User comment."
  :tag "Comment"
  :help-echo "Edit a comment here."
  :sample-face 'custom-comment-tag
  :value-face 'custom-comment
  :shown nil
  :create 'custom-comment-create)

(defun custom-comment-create (widget)
  (let* ((null-comment (equal "" (widget-value widget))))
    (if (or (widget-get (widget-get widget :parent) :comment-shown)
	    (not null-comment))
	(widget-default-create widget)
      ;; `widget-default-delete' expects markers in these slots --
      ;; maybe it shouldn't.
      (widget-put widget :from (point-marker))
      (widget-put widget :to (point-marker)))))

(defun custom-comment-hide (widget)
  (widget-put (widget-get widget :parent) :comment-shown nil))

;; Those functions are for the menu. WIDGET is NOT the comment widget. It's
;; the global custom one
(defun custom-comment-show (widget)
  (widget-put widget :comment-shown t)
  (custom-redraw widget)
  (widget-setup))

(defun custom-comment-invisible-p (widget)
  (let ((val (widget-value (widget-get widget :comment-widget))))
    (and (equal "" val)
	 (not (widget-get widget :comment-shown)))))

;;; The `custom-variable' Widget.

(defface custom-variable-tag
  `((((class color)
      (background dark))
     (:foreground "light blue" :weight bold))
    (((min-colors 88) (class color)
      (background light))
     (:foreground "blue1" :weight bold))
    (((class color)
      (background light))
     (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-variable-tag-face
  'custom-variable-tag "22.1")

(defface custom-variable-button '((t (:underline t :weight bold)))
  "Face used for pushable variable tags."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-variable-button-face
  'custom-variable-button "22.1")

(defcustom custom-variable-default-form 'edit
  "Default form of displaying variable values."
  :type '(choice (const edit)
		 (const lisp))
  :group 'custom-buffer
  :version "20.3")

(defun custom-variable-documentation (variable)
  "Return documentation of VARIABLE for use in Custom buffer.
Normally just return the docstring.  But if VARIABLE automatically
becomes buffer local when set, append a message to that effect."
  (if (and (local-variable-if-set-p variable)
	   (or (not (local-variable-p variable))
	       (with-temp-buffer
		 (local-variable-if-set-p variable))))
      (concat (documentation-property variable 'variable-documentation)
	      "\n
This variable automatically becomes buffer-local when set outside Custom.
However, setting it through Custom sets the default value.")
    (documentation-property variable 'variable-documentation)))

(define-widget 'custom-variable 'custom
  "A widget for displaying a Custom variable.
The following properties have special meanings for this widget:

:hidden-states should be a list of widget states for which the
  widget's initial contents are to be hidden.

:custom-form should be a symbol describing how to display and
  edit the variable---either `edit' (using edit widgets),
  `lisp' (as a Lisp sexp), or `mismatch' (should not happen);
  if nil, use the return value of `custom-variable-default-form'.

:shown-value, if non-nil, should be a list whose `car' is the
  variable value to display in place of the current value.

:custom-style describes the widget interface style; nil is the
  default style, while `simple' means a simpler interface that
  inhibits the magic custom-state widget."
  :format "%v"
  :help-echo "Set or reset this variable."
  :documentation-property #'custom-variable-documentation
  :custom-category 'option
  :custom-state nil
  :custom-menu 'custom-variable-menu-create
  :custom-form nil
  :value-create 'custom-variable-value-create
  :action 'custom-variable-action
  :hidden-states '(standard)
  :custom-set 'custom-variable-set
  :custom-mark-to-save 'custom-variable-mark-to-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-variable-reset-saved
  :custom-reset-standard 'custom-variable-reset-standard
  :custom-mark-to-reset-standard 'custom-variable-mark-to-reset-standard
  :custom-standard-value 'custom-variable-standard-value
  :custom-state-set-and-redraw 'custom-variable-state-set-and-redraw)

(defun custom-variable-type (symbol)
  "Return a widget suitable for editing the value of SYMBOL.
If SYMBOL has a `custom-type' property, use that.
Otherwise, try matching SYMBOL against `custom-guess-name-alist' and
try matching its doc string against `custom-guess-doc-alist'."
  (let* ((type (or (get symbol 'custom-type)
		   (and (not (get symbol 'standard-value))
			(custom-guess-type symbol))
		   'sexp))
	 (options (get symbol 'custom-options))
	 (tmp (if (listp type)
		  (copy-sequence type)
		(list type))))
    (when options
      (widget-put tmp :options options))
    tmp))

(defun custom-variable-value-create (widget)
  "Here is where you edit the variable's value."
  (custom-load-widget widget)
  (unless (widget-get widget :custom-form)
    (widget-put widget :custom-form custom-variable-default-form))
  (let* ((buttons (widget-get widget :buttons))
	 (children (widget-get widget :children))
	 (form (widget-get widget :custom-form))
	 (symbol (widget-get widget :value))
	 (tag (widget-get widget :tag))
	 (type (custom-variable-type symbol))
	 (conv (widget-convert type))
	 (get (or (get symbol 'custom-get) 'default-value))
	 (prefix (widget-get widget :custom-prefix))
	 (last (widget-get widget :custom-last))
	 (style (widget-get widget :custom-style))
	 (value (let ((shown-value (widget-get widget :shown-value)))
		  (cond (shown-value
			 (car shown-value))
			((default-boundp symbol)
			 (funcall get symbol))
			(t (widget-get conv :value)))))
	 (state (or (widget-get widget :custom-state)
		    (if (memq (custom-variable-state symbol value)
			      (widget-get widget :hidden-states))
			'hidden))))

    ;; If we don't know the state, see if we need to edit it in lisp form.
    (unless state
      (setq state (if (custom-show type value) 'unknown 'hidden)))
    (when (eq state 'unknown)
      (unless (widget-apply conv :match value)
	(setq form 'mismatch)))
    ;; Now we can create the child widget.
    (cond ((eq custom-buffer-style 'tree)
	   (insert prefix (if last " `--- " " |--- "))
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-variable-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((eq state 'hidden)
	   ;; Indicate hidden value.
	   (push (widget-create-child-and-convert
		  widget 'custom-visibility
		  :help-echo "Show the value of this option."
		  :on-glyph "down"
		  :on "Hide"
		  :off-glyph "right"
		  :off "Show Value"
		  :action 'custom-toggle-hide-variable
		  nil)
		 buttons)
	   (insert " ")
	   (push (widget-create-child-and-convert
		  widget 'item
		  :format "%{%t%} "
		  :sample-face 'custom-variable-tag
		  :tag tag
		  :parent widget)
		 buttons))
	  ((memq form '(lisp mismatch))
	   (push (widget-create-child-and-convert
		  widget 'custom-visibility
		  :help-echo "Hide the value of this option."
		  :on "Hide"
		  :off "Show"
		  :on-glyph "down"
		  :off-glyph "right"
		  :action 'custom-toggle-hide-variable
		  t)
		 buttons)
	   (insert " ")
	   ;; This used to try presenting the saved value or the
	   ;; standard value, but it seems more intuitive to present
	   ;; the current value (Bug#7600).
	   (let* ((value (cond ((default-boundp symbol)
				(custom-quote (funcall get symbol)))
			       (t
				(custom-quote (widget-get conv :value))))))
	     (insert (symbol-name symbol) ": ")
	     (push (widget-create-child-and-convert
		    widget 'sexp
		    :button-face 'custom-variable-button-face
		    :format "%v"
		    :tag (symbol-name symbol)
		    :parent widget
		    :value value)
		   children)))
	  (t
	   ;; Edit mode.
	   (push (widget-create-child-and-convert
		  widget 'custom-visibility
		  :help-echo "Hide or show this option."
		  :on "Hide"
		  :off "Show"
		  :on-glyph "down"
		  :off-glyph "right"
		  :action 'custom-toggle-hide-variable
		  t)
		 buttons)
	   (insert " ")
	   (let* ((format (widget-get type :format))
		  tag-format value-format)
	     (unless (string-match ":" format)
	       (error "Bad format"))
	     (setq tag-format (substring format 0 (match-end 0)))
	     (setq value-format (substring format (match-end 0)))
	     (push (widget-create-child-and-convert
		    widget 'item
		    :format tag-format
		    :action 'custom-tag-action
		    :help-echo "Change value of this option."
		    :mouse-down-action 'custom-tag-mouse-down-action
		    :button-face 'custom-variable-button
		    :sample-face 'custom-variable-tag
		    tag)
		   buttons)
	     (push (widget-create-child-and-convert
		    widget type
		    :format value-format
		    :value value)
		   children))))
    (unless (eq custom-buffer-style 'tree)
      (unless (eq (preceding-char) ?\n)
	(widget-insert "\n"))
      ;; Create the magic button.
      (unless (eq style 'simple)
	(let ((magic (widget-create-child-and-convert
		      widget 'custom-magic nil)))
	  (widget-put widget :custom-magic magic)
	  (push magic buttons)))
      (widget-put widget :buttons buttons)
      ;; Insert documentation.
      (widget-put widget :documentation-indent 3)
      (unless (and (eq style 'simple)
		   (eq state 'hidden))
	(widget-add-documentation-string-button
	 widget :visibility-widget 'custom-visibility))

      ;; The comment field
      (unless (eq state 'hidden)
	(let* ((comment (get symbol 'variable-comment))
	       (comment-widget
		(widget-create-child-and-convert
		 widget 'custom-comment
		 :parent widget
		 :value (or comment ""))))
	  (widget-put widget :comment-widget comment-widget)
	  ;; Don't push it !!! Custom assumes that the first child is the
	  ;; value one.
	  (setq children (append children (list comment-widget)))))
      ;; Update the rest of the properties.
      (widget-put widget :custom-form form)
      (widget-put widget :children children)
      ;; Now update the state.
      (if (eq state 'hidden)
	  (widget-put widget :custom-state state)
	(custom-variable-state-set widget))
      ;; See also.
      (unless (eq state 'hidden)
	(when (eq (widget-get widget :custom-level) 1)
	  (custom-add-parent-links widget))
	(custom-add-see-also widget)))))

(defun custom-toggle-hide-variable (visibility-widget &rest _ignore)
  "Toggle the visibility of a `custom-variable' parent widget.
By default, this signals an error if the parent has unsaved
changes.  If the parent has a `simple' :custom-style property,
the present value is saved to its :shown-value property instead."
  (let ((widget (widget-get visibility-widget :parent)))
    (unless (eq (widget-type widget) 'custom-variable)
      (error "Invalid widget type"))
    (custom-load-widget widget)
    (let ((state (widget-get widget :custom-state)))
      (if (eq state 'hidden)
	  (widget-put widget :custom-state 'unknown)
	;; In normal interface, widget can't be hidden if modified.
	(when (memq state '(invalid modified set))
	  (if (eq (widget-get widget :custom-style) 'simple)
	      (widget-put widget :shown-value
			  (list (widget-value
				 (car-safe
				  (widget-get widget :children)))))
	    (error "There are unsaved changes")))
	(widget-put widget :documentation-shown nil)
	(widget-put widget :custom-state 'hidden))
      (custom-redraw widget)
      (widget-setup))))

(defun custom-tag-action (widget &rest args)
  "Pass :action to first child of WIDGET's parent."
  (apply 'widget-apply (car (widget-get (widget-get widget :parent) :children))
	 :action args))

(defun custom-tag-mouse-down-action (widget &rest args)
  "Pass :mouse-down-action to first child of WIDGET's parent."
  (apply 'widget-apply (car (widget-get (widget-get widget :parent) :children))
	 :mouse-down-action args))

(defun custom-variable-state (symbol val)
  "Return the state of SYMBOL if its value is VAL.
If SYMBOL has a non-nil `custom-get' property, it overrides VAL.
Possible return values are `standard', `saved', `set', `themed',
`changed', and `rogue'."
  (let* ((get (or (get symbol 'custom-get) 'default-value))
	 (value (if (default-boundp symbol)
		    (funcall get symbol)
		  val))
	 (comment (get symbol 'variable-comment))
	 tmp
	 temp)
    (cond ((progn (setq tmp (get symbol 'customized-value))
		  (setq temp
			(get symbol 'customized-variable-comment))
		  (or tmp temp))
	   (if (condition-case nil
		   (and (equal value (eval (car tmp)))
			(equal comment temp))
		 (error nil))
	       'set
	     'changed))
	  ((progn (setq tmp (get symbol 'theme-value))
		  (setq temp (get symbol 'saved-variable-comment))
		  (or tmp temp))
	   (if (condition-case nil
		   (and (equal comment temp)
			(equal value
			       (eval
				(car (custom-variable-theme-value
				      symbol)))))
		 (error nil))
	       (cond
		((eq (caar tmp) 'user) 'saved)
		((eq (caar tmp) 'changed)
		 (if (condition-case nil
			 (and (null comment)
			      (equal value
				     (eval
				      (car (get symbol 'standard-value)))))
		       (error nil))
		     ;; The value was originally set outside
		     ;; custom, but it was set to the standard
		     ;; value (probably an autoloaded defcustom).
		     'standard
		   'changed))
		(t 'themed))
	     'changed))
	  ((setq tmp (get symbol 'standard-value))
	   (if (condition-case nil
		   (and (equal value (eval (car tmp)))
			(equal comment nil))
		 (error nil))
	       'standard
	     'changed))
	  (t 'rogue))))

(defun custom-variable-state-set (widget &optional state)
  "Set the state of WIDGET to STATE.
If STATE is nil, the value is computed by `custom-variable-state'."
  (widget-put widget :custom-state
	      (or state (custom-variable-state (widget-value widget)
					       (widget-get widget :value)))))

(defun custom-variable-standard-value (widget)
  (get (widget-value widget) 'standard-value))

(defvar custom-variable-menu
  `(("Set for Current Session" custom-variable-set
     (lambda (widget)
       (eq (widget-get widget :custom-state) 'modified)))
    ;; Note that in all the backquoted code in this file, we test
    ;; init-file-user rather than user-init-file.  This is in case
    ;; cus-edit is loaded by something in site-start.el, because
    ;; user-init-file is not set at that stage.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-10/msg00310.html
    ,@(when (or custom-file init-file-user)
	'(("Save for Future Sessions" custom-variable-save
	   (lambda (widget)
	     (memq (widget-get widget :custom-state)
		   '(modified set changed rogue))))))
    ("Undo Edits" custom-redraw
     (lambda (widget)
       (and (default-boundp (widget-value widget))
	    (memq (widget-get widget :custom-state) '(modified changed)))))
    ("Reset to Saved" custom-variable-reset-saved
     (lambda (widget)
       (and (or (get (widget-value widget) 'saved-value)
		(get (widget-value widget) 'saved-variable-comment))
	    (memq (widget-get widget :custom-state)
		  '(modified set changed rogue)))))
    ,@(when (or custom-file init-file-user)
	'(("Erase Customization" custom-variable-reset-standard
	   (lambda (widget)
	     (and (get (widget-value widget) 'standard-value)
		  (memq (widget-get widget :custom-state)
			'(modified set changed saved rogue)))))))
    ("Set to Backup Value" custom-variable-reset-backup
     (lambda (widget)
       (get (widget-value widget) 'backup-value)))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("Show Current Value" custom-variable-edit
     (lambda (widget)
       (eq (widget-get widget :custom-form) 'lisp)))
    ("Show Saved Lisp Expression" custom-variable-edit-lisp
     (lambda (widget)
       (eq (widget-get widget :custom-form) 'edit))))
  "Alist of actions for the `custom-variable' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-variable'
widget as an argument, and returns non-nil if ACTION is valid on that
widget.  If FILTER is nil, ACTION is always valid.")

(defun custom-variable-action (widget &optional event)
  "Show the menu for `custom-variable' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (unless (eq (widget-get widget :custom-state) 'modified)
      (custom-variable-state-set widget))
    (custom-redraw-magic widget)
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name
					   (widget-get widget :value)))
				  (custom-menu-filter custom-variable-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-variable-edit (widget)
  "Edit value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'edit)
  (custom-redraw widget))

(defun custom-variable-edit-lisp (widget)
  "Edit the Lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-variable-set (widget)
  "Set the current value for the variable being edited by WIDGET."
  (let* ((form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (child (car (widget-get widget :children)))
	 (symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget))
	 val)
    (cond ((eq state 'hidden)
	   (error "Cannot set hidden variable"))
	  ((setq val (widget-apply child :validate))
	   (goto-char (widget-get val :from))
	   (error "%s" (widget-get val :error)))
	  ((memq form '(lisp mismatch))
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (custom-comment-hide comment-widget))
	   (custom-variable-backup-value widget)
	   (custom-push-theme 'theme-value symbol 'user
			      'set (custom-quote (widget-value child)))
	   (funcall set symbol (eval (setq val (widget-value child))))
	   (put symbol 'customized-value (list val))
	   (put symbol 'variable-comment comment)
	   (put symbol 'customized-variable-comment comment))
	  (t
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (custom-comment-hide comment-widget))
	   (custom-variable-backup-value widget)
	   (custom-push-theme 'theme-value symbol 'user
			      'set (custom-quote (widget-value child)))
	   (funcall set symbol (setq val (widget-value child)))
	   (put symbol 'customized-value (list (custom-quote val)))
	   (put symbol 'variable-comment comment)
	   (put symbol 'customized-variable-comment comment)))
    (custom-variable-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-variable-mark-to-save (widget)
  "Set value and mark for saving the variable edited by WIDGET."
  (let* ((form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (child (car (widget-get widget :children)))
	 (symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget))
	 val)
    (cond ((eq state 'hidden)
	   (error "Cannot set hidden variable"))
	  ((setq val (widget-apply child :validate))
	   (goto-char (widget-get val :from))
	   (error "Saving %s: %s" symbol (widget-get val :error)))
	  ((memq form '(lisp mismatch))
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (custom-comment-hide comment-widget))
	   (put symbol 'saved-value (list (widget-value child)))
	   (custom-push-theme 'theme-value symbol 'user
			      'set (custom-quote (widget-value child)))
	   (funcall set symbol (eval (widget-value child)))
	   (put symbol 'variable-comment comment)
	   (put symbol 'saved-variable-comment comment))
	  (t
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (custom-comment-hide comment-widget))
	   (put symbol 'saved-value
		(list (custom-quote (widget-value child))))
	   (custom-push-theme 'theme-value symbol 'user
			      'set (custom-quote (widget-value child)))
	   (funcall set symbol (widget-value child))
	   (put symbol 'variable-comment comment)
	   (put symbol 'saved-variable-comment comment)))
    (put symbol 'customized-value nil)
    (put symbol 'customized-variable-comment nil)))

(defsubst custom-variable-state-set-and-redraw (widget)
  "Set state of variable widget WIDGET and redraw with current settings."
  (custom-variable-state-set widget)
  (custom-redraw-magic widget))

(defun custom-variable-save (widget)
  "Save value of variable edited by widget WIDGET."
  (custom-variable-mark-to-save widget)
  (custom-save-all)
  (custom-variable-state-set-and-redraw widget))

(defun custom-variable-reset-saved (widget)
  "Restore the saved value for the variable being edited by WIDGET.
This also updates the buffer to show that value.
The value that was current before this operation
becomes the backup value, so you can get it again."
  (let* ((symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (value (get symbol 'saved-value))
	 (comment (get symbol 'saved-variable-comment)))
    (cond ((or value comment)
	   (put symbol 'variable-comment comment)
	   (custom-variable-backup-value widget)
	   (custom-push-theme 'theme-value symbol 'user 'set (car-safe value))
	   (condition-case nil
	       (funcall set symbol (eval (car value)))
	     (error nil)))
	  (t
	   (error "No saved value for %s" symbol)))
    (put symbol 'customized-value nil)
    (put symbol 'customized-variable-comment nil)
    (widget-put widget :custom-state 'unknown)
    ;; This call will possibly make the comment invisible
    (custom-redraw widget)))

(defun custom-variable-mark-to-reset-standard (widget)
  "Mark to restore standard setting for the variable edited by widget WIDGET.
If `custom-reset-standard-variables-list' is nil, save, reset and
redraw the widget immediately."
  (let* ((symbol (widget-value widget)))
    (if (get symbol 'standard-value)
	(custom-variable-backup-value widget)
      (error "No standard setting known for %S" symbol))
    (put symbol 'variable-comment nil)
    (put symbol 'customized-value nil)
    (put symbol 'customized-variable-comment nil)
    (custom-push-theme 'theme-value symbol 'user 'reset)
    (custom-theme-recalc-variable symbol)
    (if (and custom-reset-standard-variables-list
	     (or (get symbol 'saved-value) (get symbol 'saved-variable-comment)))
	(progn
	  (put symbol 'saved-value nil)
	  (put symbol 'saved-variable-comment nil)
	  ;; Append this to `custom-reset-standard-variables-list' to
	  ;; have `custom-reset-standard-save-and-update' save setting
	  ;; to the file, update the widget's state, and redraw it.
	  (setq custom-reset-standard-variables-list
		(cons widget custom-reset-standard-variables-list)))
      (when (or (get symbol 'saved-value) (get symbol 'saved-variable-comment))
	(put symbol 'saved-value nil)
	(put symbol 'saved-variable-comment nil)
	(custom-save-all))
      (widget-put widget :custom-state 'unknown)
      ;; This call will possibly make the comment invisible
      (custom-redraw widget))))

(defun custom-variable-reset-standard (widget)
  "Restore standard setting for the variable edited by WIDGET.
This operation eliminates any saved setting for the variable,
restoring it to the state of a variable that has never been customized.
The value that was current before this operation
becomes the backup value, so you can get it again."
  (let (custom-reset-standard-variables-list)
    (custom-variable-mark-to-reset-standard widget)))

(defun custom-variable-backup-value (widget)
  "Back up the current value for WIDGET's variable.
The backup value is kept in the car of the `backup-value' property."
  (let* ((symbol (widget-value widget))
	 (get (or (get symbol 'custom-get) 'default-value))
	 (type (custom-variable-type symbol))
	 (conv (widget-convert type))
	 (value (if (default-boundp symbol)
		    (funcall get symbol)
		  (widget-get conv :value))))
    (put symbol 'backup-value (list value))))

(defun custom-variable-reset-backup (widget)
  "Restore the backup value for the variable being edited by WIDGET.
The value that was current before this operation
becomes the backup value, so you can use this operation repeatedly
to switch between two values."
  (let* ((symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (value (get symbol 'backup-value))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget)))
    (if value
	(progn
	  (custom-variable-backup-value widget)
	  (custom-push-theme 'theme-value symbol 'user 'set value)
	  (condition-case nil
	      (funcall set symbol (car value))
	     (error nil)))
      (error "No backup value for %s" symbol))
    (put symbol 'customized-value (list (custom-quote (car value))))
    (put symbol 'variable-comment comment)
    (put symbol 'customized-variable-comment comment)
    (custom-variable-state-set widget)
    ;; This call will possibly make the comment invisible
    (custom-redraw widget)))

;;; The `custom-visibility' Widget

(define-widget 'custom-visibility 'visibility
  "Show or hide a documentation string."
  :button-face 'custom-visibility
  :pressed-face 'custom-visibility
  :mouse-face 'highlight
  :pressed-face 'highlight
  :on-glyph nil
  :off-glyph nil)

(defface custom-visibility
  '((t :height 0.8 :inherit link))
  "Face for the `custom-visibility' widget."
  :version "23.1"
  :group 'custom-faces)

;;; The `custom-face-edit' Widget.

(define-widget 'custom-face-edit 'checklist
  "Widget for editing face attributes.
The following properties have special meanings for this widget:

:value is a plist of face attributes.

:default-face-attributes, if non-nil, is a plist of defaults for
face attributes (as specified by a `default' defface entry)."
  :format "%v"
  :extra-offset 3
  :button-args '(:help-echo "Control whether this attribute has any effect.")
  :value-to-internal 'custom-face-edit-fix-value
  :match (lambda (widget value)
	   (widget-checklist-match widget
				   (custom-face-edit-fix-value widget value)))
  :value-create 'custom-face-edit-value-create
  :convert-widget 'custom-face-edit-convert-widget
  :args (mapcar (lambda (att)
		  (list 'group :inline t
			:sibling-args (widget-get (nth 1 att) :sibling-args)
			(list 'const :format "" :value (nth 0 att))
			(nth 1 att)))
		custom-face-attributes))

(defun custom-face-edit-value-create (widget)
  (let* ((alist (widget-checklist-match-find
		 widget (widget-get widget :value)))
	 (args  (widget-get widget :args))
	 (show-all (widget-get widget :show-all-attributes))
	 (buttons  (widget-get widget :buttons))
	 (defaults (widget-checklist-match-find
		    widget
		    (widget-get widget :default-face-attributes)))
	 entry)
    (unless (looking-back "^ *")
      (insert ?\n))
    (insert-char ?\s (widget-get widget :extra-offset))
    (if (or alist defaults show-all)
	(dolist (prop args)
	  (setq entry (or (assq prop alist)
			  (assq prop defaults)))
	  (if (or entry show-all)
	      (widget-checklist-add-item widget prop entry)))
      (insert (propertize "-- Empty face --" 'face 'shadow) ?\n))
    (let ((indent (widget-get widget :indent)))
      (if indent (insert-char ?\s (widget-get widget :indent))))
    (push (widget-create-child-and-convert
	   widget 'visibility
	   :help-echo "Show or hide all face attributes."
	   :button-face 'custom-visibility
	   :pressed-face 'custom-visibility
	   :mouse-face 'highlight
	   :on "Hide Unused Attributes"    :off "Show All Attributes"
	   :on-glyph nil :off-glyph nil
	   :always-active t
	   :action 'custom-face-edit-value-visibility-action
	   show-all)
	  buttons)
    (insert ?\n)
    (widget-put widget :buttons buttons)
    (widget-put widget :children (nreverse (widget-get widget :children)))))

(defun custom-face-edit-value-visibility-action (widget &rest _ignore)
  ;; Toggle hiding of face attributes.
  (let ((parent (widget-get widget :parent)))
    (widget-put parent :show-all-attributes
		(not (widget-get parent :show-all-attributes)))
    (custom-redraw parent)))

(defun custom-face-edit-fix-value (_widget value)
  "Ignoring WIDGET, convert :bold and :italic in VALUE to new form.
Also change :reverse-video to :inverse-video."
  (custom-fix-face-spec value))

(defun custom-face-edit-convert-widget (widget)
  "Convert :args as widget types in WIDGET."
  (widget-put
   widget
   :args (mapcar (lambda (arg)
		   (widget-convert arg
				   :deactivate 'custom-face-edit-deactivate
				   :activate 'custom-face-edit-activate
				   :delete 'custom-face-edit-delete))
		 (widget-get widget :args)))
  widget)

(defconst custom-face-edit (widget-convert 'custom-face-edit)
  "Converted version of the `custom-face-edit' widget.")

(defun custom-face-edit-deactivate (widget)
  "Make face widget WIDGET inactive for user modifications."
  (unless (widget-get widget :inactive)
    (let ((tag (custom-face-edit-attribute-tag widget))
	  (from (copy-marker (widget-get widget :from)))
	  (value (widget-value widget))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (save-excursion
	(goto-char from)
	(widget-default-delete widget)
	(insert tag ": " (propertize "--" 'face 'shadow) "\n")
	(widget-put widget :inactive
		    (cons value (cons from (- (point) from))))))))

(defun custom-face-edit-activate (widget)
  "Make face widget WIDGET active for user modifications."
  (let ((inactive (widget-get widget :inactive))
	(inhibit-read-only t)
	(inhibit-modification-hooks t))
    (when (consp inactive)
      (save-excursion
	(goto-char (car (cdr inactive)))
	(delete-region (point) (+ (point) (cdr (cdr inactive))))
	(widget-put widget :inactive nil)
	(widget-apply widget :create)
	(widget-value-set widget (car inactive))
	(widget-setup)))))

(defun custom-face-edit-delete (widget)
  "Remove WIDGET from the buffer."
  (let ((inactive (widget-get widget :inactive))
	(inhibit-read-only t)
	(inhibit-modification-hooks t))
    (if (not inactive)
	;; Widget is alive, we don't have to do anything special
	(widget-default-delete widget)
      ;; WIDGET is already deleted because we did so to deactivate it;
      ;; now just get rid of the label we put in its place.
      (delete-region (car (cdr inactive))
		     (+ (car (cdr inactive)) (cdr (cdr inactive))))
      (widget-put widget :inactive nil))))


(defun custom-face-edit-attribute-tag (widget)
  "Return the first :tag property in WIDGET or one of its children."
  (let ((tag (widget-get widget :tag)))
    (or (and (not (equal tag "")) tag)
	(let ((children (widget-get widget :children)))
	  (while (and (null tag) children)
	    (setq tag (custom-face-edit-attribute-tag (pop children))))
	  tag))))

;;; The `custom-display' Widget.

(define-widget 'custom-display 'menu-choice
  "Select a display type."
  :tag "Display"
  :value t
  :help-echo "Specify frames where the face attributes should be used."
  :args '((const :tag "all" t)
	  (const :tag "defaults" default)
	  (checklist
	   :tag "specific display"
	   :offset 0
	   :extra-offset 9
	   :args ((group :sibling-args (:help-echo "\
Only match the specified window systems.")
			 (const :format "Type: "
				type)
			 (checklist :inline t
				    :offset 0
				    (const :format "X "
					   :sibling-args (:help-echo "\
The X11 Window System.")
					   x)
				    (const :format "PM "
					   :sibling-args (:help-echo "\
OS/2 Presentation Manager.")
					   pm)
				    (const :format "W32 "
					   :sibling-args (:help-echo "\
Windows NT/9X.")
					   w32)
				    (const :format "NS "
					   :sibling-args (:help-echo "\
GNUstep or Macintosh OS Cocoa interface.")
					   ns)
				    (const :format "DOS "
					   :sibling-args (:help-echo "\
Plain MS-DOS.")
					   pc)
				    (const :format "TTY%n"
					   :sibling-args (:help-echo "\
Plain text terminals.")
					   tty)))
		  (group :sibling-args (:help-echo "\
Only match the frames with the specified color support.")
			 (const :format "Class: "
				class)
			 (checklist :inline t
				    :offset 0
				    (const :format "Color "
					   :sibling-args (:help-echo "\
Match color frames.")
					   color)
				    (const :format "Grayscale "
					   :sibling-args (:help-echo "\
Match grayscale frames.")
					   grayscale)
				    (const :format "Monochrome%n"
					   :sibling-args (:help-echo "\
Match frames with no color support.")
					   mono)))
		  (group :sibling-args (:help-echo "\
The minimum number of colors the frame should support.")
			 (const :format "" min-colors)
			 (integer :tag "Minimum number of colors" ))
		  (group :sibling-args (:help-echo "\
Only match frames with the specified intensity.")
			 (const :format "\
Background brightness: "
				background)
			 (checklist :inline t
				    :offset 0
				    (const :format "Light "
					   :sibling-args (:help-echo "\
Match frames with light backgrounds.")
					   light)
				    (const :format "Dark\n"
					   :sibling-args (:help-echo "\
Match frames with dark backgrounds.")
					   dark)))
		  (group :sibling-args (:help-echo "\
Only match frames that support the specified face attributes.")
			 (const :format "Supports attributes:" supports)
			 (custom-face-edit :inline t :format "%n%v"))))))

;;; The `custom-face' Widget.

(defface custom-face-tag
  `((t :inherit custom-variable-tag))
  "Face used for face tags."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-face-tag-face 'custom-face-tag "22.1")

(defcustom custom-face-default-form 'selected
  "Default form of displaying face definition."
  :type '(choice (const all)
		 (const selected)
		 (const lisp))
  :group 'custom-buffer
  :version "20.3")

(define-widget 'custom-face 'custom
  "Widget for customizing a face.
The following properties have special meanings for this widget:

:value is the face name (a symbol).

:custom-form should be a symbol describing how to display and
  edit the face attributes---either `selected' (attributes for
  selected display only), `all' (all attributes), `lisp' (as a
  Lisp sexp), or `mismatch' (should not happen); if nil, use
  the return value of `custom-face-default-form'.

:custom-style describes the widget interface style; nil is the
  default style, while `simple' means a simpler interface that
  inhibits the magic custom-state widget.

:sample-indent, if non-nil, is the number of columns to which to
  indent the face sample (an integer).

:shown-value, if non-nil, is the face spec to display as the value
  of the widget, instead of the current face spec."
  :sample-face 'custom-face-tag
  :help-echo "Set or reset this face."
  :documentation-property #'face-doc-string
  :value-create 'custom-face-value-create
  :action 'custom-face-action
  :custom-category 'face
  :custom-form nil
  :custom-set 'custom-face-set
  :custom-mark-to-save 'custom-face-mark-to-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-face-reset-saved
  :custom-reset-standard 'custom-face-reset-standard
  :custom-mark-to-reset-standard 'custom-face-mark-to-reset-standard
  :custom-standard-value 'custom-face-standard-value
  :custom-state-set-and-redraw 'custom-face-state-set-and-redraw
  :custom-menu 'custom-face-menu-create)

(define-widget 'custom-face-all 'editable-list
  "An editable list of display specifications and attributes."
  :entry-format "%i %d %v"
  :insert-button-args '(:help-echo "Insert new display specification here.")
  :append-button-args '(:help-echo "Append new display specification here.")
  :delete-button-args '(:help-echo "Delete this display specification.")
  :args '((group :format "%v" custom-display custom-face-edit)))

(defconst custom-face-all (widget-convert 'custom-face-all)
  "Converted version of the `custom-face-all' widget.")

(defun custom-filter-face-spec (spec filter-index &optional default-filter)
  "Return a canonicalized version of SPEC using.
FILTER-INDEX is the index in the entry for each attribute in
`custom-face-attributes' at which the appropriate filter function can be
found, and DEFAULT-FILTER is the filter to apply for attributes that
don't specify one."
  (mapcar (lambda (entry)
	    ;; Filter a single face-spec entry
	    (let ((tests (car entry))
		  (unfiltered-attrs
		   ;; Handle both old- and new-style attribute syntax
		   (if (listp (car (cdr entry)))
		       (car (cdr entry))
		     (cdr entry)))
		  (filtered-attrs nil))
	      ;; Filter each face attribute
	      (while unfiltered-attrs
		(let* ((attr (pop unfiltered-attrs))
		       (pre-filtered-value (pop unfiltered-attrs))
		       (filter
			(or (nth filter-index (assq attr custom-face-attributes))
			    default-filter))
		       (filtered-value
			(if filter
			    (funcall filter pre-filtered-value)
			  pre-filtered-value)))
		  (push filtered-value filtered-attrs)
		  (push attr filtered-attrs)))
	      ;;
	      (list tests filtered-attrs)))
	  spec))

(defun custom-pre-filter-face-spec (spec)
  "Return SPEC changed as necessary for editing by the face customization widget.
SPEC must be a full face spec."
  (custom-filter-face-spec spec 2))

(defun custom-post-filter-face-spec (spec)
  "Return the customized SPEC in a form suitable for setting the face."
  (custom-filter-face-spec spec 3))

(defun custom-face-widget-to-spec (widget)
  "Return a face spec corresponding to WIDGET.
WIDGET should be a `custom-face' widget."
  (unless (eq (widget-type widget) 'custom-face)
    (error "Invalid widget"))
  (let ((child (car (widget-get widget :children))))
    (custom-post-filter-face-spec
     (if (eq (widget-type child) 'custom-face-edit)
	 `((t ,(widget-value child)))
       (widget-value child)))))

(defun custom-face-get-current-spec (face)
  (let ((spec (or (get face 'customized-face)
		  (get face 'saved-face)
		  (get face 'face-defface-spec)
		  ;; Attempt to construct it.
		  `((t ,(custom-face-attributes-get
			 face (selected-frame)))))))
    ;; If the user has changed this face in some other way,
    ;; edit it as the user has specified it.
    (if (not (face-spec-match-p face spec (selected-frame)))
	(setq spec `((t ,(face-attr-construct face (selected-frame))))))
    (custom-pre-filter-face-spec spec)))

(defun custom-toggle-hide-face (visibility-widget &rest _ignore)
  "Toggle the visibility of a `custom-face' parent widget.
By default, this signals an error if the parent has unsaved
changes.  If the parent has a `simple' :custom-style property,
the present value is saved to its :shown-value property instead."
  (let ((widget (widget-get visibility-widget :parent)))
    (unless (eq (widget-type widget) 'custom-face)
      (error "Invalid widget type"))
    (custom-load-widget widget)
    (let ((state (widget-get widget :custom-state)))
      (if (eq state 'hidden)
	  (widget-put widget :custom-state 'unknown)
	;; In normal interface, widget can't be hidden if modified.
	(when (memq state '(invalid modified set))
	  (if (eq (widget-get widget :custom-style) 'simple)
	      (widget-put widget :shown-value
			  (custom-face-widget-to-spec widget))
	    (error "There are unsaved changes")))
	(widget-put widget :documentation-shown nil)
	(widget-put widget :custom-state 'hidden))
      (custom-redraw widget)
      (widget-setup))))

(defun custom-face-value-create (widget)
  "Create a list of the display specifications for WIDGET."
  (let* ((buttons (widget-get widget :buttons))
	 (symbol  (widget-get widget :value))
	 (tag (or (widget-get widget :tag)
		  (prin1-to-string symbol)))
	 (hiddenp (eq (widget-get widget :custom-state) 'hidden))
	 (style   (widget-get widget :custom-style))
	 children)

    (if (eq custom-buffer-style 'tree)

	;; Draw a tree-style `custom-face' widget
	(progn
	  (insert (widget-get widget :custom-prefix)
		  (if (widget-get widget :custom-last) " `--- " " |--- "))
	  (push (widget-create-child-and-convert
		 widget 'custom-browse-face-tag)
		buttons)
	  (insert " " tag "\n")
	  (widget-put widget :buttons buttons))

      ;; Draw an ordinary `custom-face' widget
      (let ((opoint (point)))
	;; Visibility indicator.
	(push (widget-create-child-and-convert
	       widget 'custom-visibility
	       :help-echo "Hide or show this face."
	       :on "Hide" :off "Show"
	       :on-glyph "down" :off-glyph "right"
	       :action 'custom-toggle-hide-face
	       (not hiddenp))
	      buttons)
	;; Face name (tag).
	(insert " " tag)
	(widget-specify-sample widget opoint (point)))
      (insert
       (cond ((eq custom-buffer-style 'face) " ")
	     ((string-match "face\\'" tag)   ":")
	     (t " face: ")))

      ;; Face sample.
      (let ((sample-indent (widget-get widget :sample-indent))
	    (indent-tabs-mode nil))
	(and sample-indent
	     (<= (current-column) sample-indent)
	     (indent-to-column sample-indent)))
      (push (widget-create-child-and-convert
	     widget 'item
	     :format "[%{%t%}]"
	     :sample-face (let ((spec (widget-get widget :shown-value)))
			    (if spec (face-spec-choose spec) symbol))
	     :tag "sample")
	    buttons)
      (insert "\n")

      ;; Magic.
      (unless (eq (widget-get widget :custom-style) 'simple)
	(let ((magic (widget-create-child-and-convert
		      widget 'custom-magic nil)))
	  (widget-put widget :custom-magic magic)
	  (push magic buttons)))

      ;; Update buttons.
      (widget-put widget :buttons buttons)

      ;; Insert documentation.
      (unless (and hiddenp (eq style 'simple))
	(widget-put widget :documentation-indent 3)
	(widget-add-documentation-string-button
	 widget :visibility-widget 'custom-visibility)
	;; The comment field
	(unless hiddenp
	  (let* ((comment (get symbol 'face-comment))
		 (comment-widget
		  (widget-create-child-and-convert
		   widget 'custom-comment
		   :parent widget
		   :value (or comment ""))))
	    (widget-put widget :comment-widget comment-widget)
	    (push comment-widget children))))

      ;; Editor.
      (unless (eq (preceding-char) ?\n)
	(insert "\n"))
      (unless hiddenp
	(custom-load-widget widget)
	(unless (widget-get widget :custom-form)
	  (widget-put widget :custom-form custom-face-default-form))

	(let* ((spec (or (widget-get widget :shown-value)
			 (custom-face-get-current-spec symbol)))
	       (form (widget-get widget :custom-form))
	       (indent (widget-get widget :indent))
	       face-alist face-entry spec-default spec-match editor)

	  ;; Find a display in SPEC matching the selected display.
	  ;; This will use the usual face customization interface.
	  (setq face-alist spec)
	  (when (eq (car-safe (car-safe face-alist)) 'default)
	    (setq spec-default (pop face-alist)))

	  (while (and face-alist (listp face-alist) (null spec-match))
	    (setq face-entry (car face-alist))
	    (and (listp face-entry)
		 (face-spec-set-match-display (car face-entry)
					      (selected-frame))
		 (widget-apply custom-face-edit :match (cadr face-entry))
		 (setq spec-match face-entry))
	    (setq face-alist (cdr face-alist)))

	  ;; Insert the appropriate editing widget.
	  (setq editor
		(cond
		 ((and (eq form 'selected)
		       (or spec-match spec-default))
		  (when indent (insert-char ?\s indent))
		  (widget-create-child-and-convert
		   widget 'custom-face-edit
		   :value (cadr spec-match)
		   :default-face-attributes (cadr spec-default)))
		 ((and (not (eq form 'lisp))
		       (widget-apply custom-face-all :match spec))
		  (widget-create-child-and-convert
		   widget 'custom-face-all :value spec))
		 (t
		  (when indent
		    (insert-char ?\s indent))
		  (widget-create-child-and-convert
		   widget 'sexp :value spec))))
	  (custom-face-state-set widget)
	  (push editor children)
	  (widget-put widget :children children))))))

(defvar custom-face-menu
  `(("Set for Current Session" custom-face-set)
    ,@(when (or custom-file init-file-user)
	'(("Save for Future Sessions" custom-face-save)))
    ("Undo Edits" custom-redraw
     (lambda (widget)
       (memq (widget-get widget :custom-state) '(modified changed))))
    ("Reset to Saved" custom-face-reset-saved
     (lambda (widget)
       (or (get (widget-value widget) 'saved-face)
	   (get (widget-value widget) 'saved-face-comment))))
    ,@(when (or custom-file init-file-user)
	'(("Erase Customization" custom-face-reset-standard
	   (lambda (widget)
	     (get (widget-value widget) 'face-defface-spec)))))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("For Current Display" custom-face-edit-selected
     (lambda (widget)
       (not (eq (widget-get widget :custom-form) 'selected))))
    ("For All Kinds of Displays" custom-face-edit-all
     (lambda (widget)
       (not (eq (widget-get widget :custom-form) 'all))))
    ("Show Lisp Expression" custom-face-edit-lisp
     (lambda (widget)
       (not (eq (widget-get widget :custom-form) 'lisp)))))
  "Alist of actions for the `custom-face' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-face'
widget as an argument, and returns non-nil if ACTION is valid on that
widget.  If FILTER is nil, ACTION is always valid.")

(defun custom-face-edit-selected (widget)
  "Edit selected attributes of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'selected)
  (custom-redraw widget))

(defun custom-face-edit-all (widget)
  "Edit all attributes of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'all)
  (custom-redraw widget))

(defun custom-face-edit-lisp (widget)
  "Edit the Lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-face-state (face)
  "Return the current state of the face FACE.
This is one of `set', `saved', `changed', `themed', or `rogue'."
  (let* ((comment (get face 'face-comment))
	 (state
	  (cond
	   ((or (get face 'customized-face)
		(get face 'customized-face-comment))
	    (if (equal (get face 'customized-face-comment) comment)
		'set
	      'changed))
	   ((or (get face 'saved-face)
		(get face 'saved-face-comment))
	    (if (equal (get face 'saved-face-comment) comment)
		(cond
		 ((eq 'user (caar (get face 'theme-face)))
		  'saved)
		 ((eq 'changed (caar (get face 'theme-face)))
		  'changed)
		 (t 'themed))
	      'changed))
	   ((get face 'face-defface-spec)
	    (if (equal comment nil)
		'standard
	      'changed))
	   (t 'rogue))))
    ;; If the user called set-face-attribute to change the default for
    ;; new frames, this face is "set outside of Customize".
    (if (and (not (eq state 'rogue))
	     (get face 'face-modified))
	'changed
      state)))

(defun custom-face-state-set (widget)
  "Set the state of WIDGET."
  (widget-put widget :custom-state
	      (custom-face-state (widget-value widget))))

(defun custom-face-action (widget &optional event)
  "Show the menu for `custom-face' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (let* ((completion-ignore-case t)
	   (symbol (widget-get widget :value))
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name symbol))
				  (custom-menu-filter custom-face-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-face-set (widget)
  "Make the face attributes in WIDGET take effect."
  (let* ((symbol (widget-value widget))
	 (value  (custom-face-widget-to-spec widget))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget)))
    (when (equal comment "")
      (setq comment nil)
      ;; Make the comment invisible by hand if it's empty
      (custom-comment-hide comment-widget))
    (put symbol 'customized-face value)
    (custom-push-theme 'theme-face symbol 'user 'set value)
    (if (face-spec-choose value)
	(face-spec-set symbol value t)
      ;; face-set-spec ignores empty attribute lists, so just give it
      ;; something harmless instead.
      (face-spec-set symbol '((t :foreground unspecified)) t))
    (put symbol 'customized-face-comment comment)
    (put symbol 'face-comment comment)
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-mark-to-save (widget)
  "Mark for saving the face edited by WIDGET."
  (let* ((symbol (widget-value widget))
	 (value  (custom-face-widget-to-spec widget))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget)))
    (when (equal comment "")
      (setq comment nil)
      ;; Make the comment invisible by hand if it's empty
      (custom-comment-hide comment-widget))
    (custom-push-theme 'theme-face symbol 'user 'set value)
    (if (face-spec-choose value)
	(face-spec-set symbol value t)
      ;; face-set-spec ignores empty attribute lists, so just give it
      ;; something harmless instead.
      (face-spec-set symbol '((t :foreground unspecified)) t))
    (unless (eq (widget-get widget :custom-state) 'standard)
      (put symbol 'saved-face value))
    (put symbol 'customized-face nil)
    (put symbol 'face-comment comment)
    (put symbol 'customized-face-comment nil)
    (put symbol 'saved-face-comment comment)))

(defsubst custom-face-state-set-and-redraw (widget)
  "Set state of face widget WIDGET and redraw with current settings."
  (custom-face-state-set widget)
  (custom-redraw-magic widget))

(defun custom-face-save (widget)
  "Save the face edited by WIDGET."
  (custom-face-mark-to-save widget)
  (custom-save-all)
  (custom-face-state-set-and-redraw widget))

;; For backward compatibility.
(define-obsolete-function-alias 'custom-face-save-command 'custom-face-save
  "22.1")

(defun custom-face-reset-saved (widget)
  "Restore WIDGET to the face's default attributes."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (get symbol 'saved-face))
	 (comment (get symbol 'saved-face-comment))
	 (comment-widget (widget-get widget :comment-widget)))
    (unless (or value comment)
      (error "No saved value for this face"))
    (put symbol 'customized-face nil)
    (put symbol 'customized-face-comment nil)
    (custom-push-theme 'theme-face symbol 'user 'set value)
    (face-spec-set symbol value t)
    (put symbol 'face-comment comment)
    (widget-value-set child value)
    ;; This call manages the comment visibility
    (widget-value-set comment-widget (or comment ""))
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-standard-value (widget)
  (get (widget-value widget) 'face-defface-spec))

(defun custom-face-mark-to-reset-standard (widget)
  "Restore widget WIDGET to the face's standard attribute values.
If `custom-reset-standard-faces-list' is nil, save, reset and
redraw the widget immediately."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (get symbol 'face-defface-spec))
	 (comment-widget (widget-get widget :comment-widget)))
    (unless value
      (error "No standard setting for this face"))
    (put symbol 'customized-face nil)
    (put symbol 'customized-face-comment nil)
    (custom-push-theme 'theme-face symbol 'user 'reset)
    (face-spec-set symbol value t)
    (custom-theme-recalc-face symbol)
    (if (and custom-reset-standard-faces-list
	     (or (get symbol 'saved-face) (get symbol 'saved-face-comment)))
	;; Do this later.
	(progn
	  (put symbol 'saved-face nil)
	  (put symbol 'saved-face-comment nil)
	  ;; Append this to `custom-reset-standard-faces-list' and have
	  ;; `custom-reset-standard-save-and-update' save setting to the
	  ;; file, update the widget's state, and redraw it.
	  (setq custom-reset-standard-faces-list
		(cons widget custom-reset-standard-faces-list)))
      (when (or (get symbol 'saved-face) (get symbol 'saved-face-comment))
	(put symbol 'saved-face nil)
	(put symbol 'saved-face-comment nil)
	(custom-save-all))
      (put symbol 'face-comment nil)
      (widget-value-set child
			(custom-pre-filter-face-spec
			 (list (list t (custom-face-attributes-get
					symbol nil)))))
      ;; This call manages the comment visibility
      (widget-value-set comment-widget "")
      (custom-face-state-set widget)
      (custom-redraw-magic widget))))

(defun custom-face-reset-standard (widget)
  "Restore WIDGET to the face's standard attribute values.
This operation eliminates any saved attributes for the face,
restoring it to the state of a face that has never been customized."
  (let (custom-reset-standard-faces-list)
    (custom-face-mark-to-reset-standard widget)))

;;; The `face' Widget.

(defvar widget-face-prompt-value-history nil
  "History of input to `widget-face-prompt-value'.")

(define-widget 'face 'symbol
  "A Lisp face name (with sample)."
  :format "%{%t%}: (%{sample%}) %v"
  :tag "Face"
  :value 'default
  :sample-face-get 'widget-face-sample-face-get
  :notify 'widget-face-notify
  :match (lambda (_widget value) (facep value))
  :completions (apply-partially #'completion-table-with-predicate
                                obarray #'facep 'strict)
  :prompt-match 'facep
  :prompt-history 'widget-face-prompt-value-history
  :validate (lambda (widget)
	      (unless (facep (widget-value widget))
		(widget-put widget
			    :error (format "Invalid face: %S"
					   (widget-value widget)))
		widget)))

(defun widget-face-sample-face-get (widget)
  (let ((value (widget-value widget)))
    (if (facep value)
	value
      'default)))

(defun widget-face-notify (widget child &optional event)
  "Update the sample, and notify the parent."
  (overlay-put (widget-get widget :sample-overlay)
	       'face (widget-apply widget :sample-face-get))
  (widget-default-notify widget child event))


;;; The `hook' Widget.

(define-widget 'hook 'list
  "An Emacs Lisp hook."
  :value-to-internal (lambda (_widget value)
		       (if (and value (symbolp value))
			   (list value)
			 value))
  :match (lambda (widget value)
	   (or (symbolp value)
	       (widget-group-match widget value)))
  ;; Avoid adding undefined functions to the hook, especially for
  ;; things like `find-file-hook' or even more basic ones, to avoid
  ;; chaos.
  :set (lambda (symbol value)
	 (dolist (elt value)
	   (if (fboundp elt)
	       (add-hook symbol elt))))
  :convert-widget 'custom-hook-convert-widget
  :tag "Hook")

(defun custom-hook-convert-widget (widget)
  ;; Handle `:options'.
  (let* ((options (widget-get widget :options))
	 (other `(editable-list :inline t
				:entry-format "%i %d%v"
				(function :format " %v")))
	 (args (if options
		   (list `(checklist :inline t
				     ,@(mapcar (lambda (entry)
						 `(function-item ,entry))
					       options))
			 other)
		 (list other))))
    (widget-put widget :args args)
    widget))

;;; The `custom-group-link' Widget.

(define-widget 'custom-group-link 'link
  "Show parent in other window when activated."
  :button-face 'custom-link
  :mouse-face 'highlight
  :pressed-face 'highlight
  :help-echo "Create customization buffer for this group."
  :keymap custom-mode-link-map
  :follow-link 'mouse-face
  :action 'custom-group-link-action)

(defun custom-group-link-action (widget &rest _ignore)
  (customize-group (widget-value widget)))

;;; The `custom-group' Widget.

(defcustom custom-group-tag-faces nil
  ;; In XEmacs, this ought to play games with font size.
  ;; Fixme: make it do so in Emacs.
  "Face used for group tags.
The first member is used for level 1 groups, the second for level 2,
and so forth.  The remaining group tags are shown with `custom-group-tag'."
  :type '(repeat face)
  :group 'custom-faces)

(defface custom-group-tag-1
  `((((class color)
      (background dark))
     (:foreground "pink" :weight bold :height 1.2 :inherit variable-pitch))
    (((min-colors 88) (class color)
      (background light))
     (:foreground "red1" :weight bold :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "red" :weight bold :height 1.2 :inherit variable-pitch))
    (t (:weight bold)))
  "Face used for group tags."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-group-tag-face-1 'custom-group-tag-1 "22.1")

(defface custom-group-tag
  `((((class color)
      (background dark))
     (:foreground "light blue" :weight bold :height 1.2 :inherit variable-pitch))
    (((min-colors 88) (class color)
      (background light))
     (:foreground "blue1" :weight bold :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "blue" :weight bold :height 1.2 :inherit variable-pitch))
    (t (:weight bold)))
  "Face used for low level group tags."
  :group 'custom-faces)
(define-obsolete-face-alias 'custom-group-tag-face 'custom-group-tag "22.1")

(define-widget 'custom-group 'custom
  "Customize group."
  :format "%v"
  :sample-face-get 'custom-group-sample-face-get
  :documentation-property 'group-documentation
  :help-echo "Set or reset all members of this group."
  :value-create 'custom-group-value-create
  :action 'custom-group-action
  :custom-category 'group
  :custom-set 'custom-group-set
  :custom-mark-to-save 'custom-group-mark-to-save
  :custom-reset-current 'custom-group-reset-current
  :custom-reset-saved 'custom-group-reset-saved
  :custom-reset-standard 'custom-group-reset-standard
  :custom-mark-to-reset-standard 'custom-group-mark-to-reset-standard
  :custom-state-set-and-redraw 'custom-group-state-set-and-redraw
  :custom-menu 'custom-group-menu-create)

(defun custom-group-sample-face-get (widget)
  ;; Use :sample-face.
  (or (nth (1- (widget-get widget :custom-level)) custom-group-tag-faces)
      'custom-group-tag))

(define-widget 'custom-group-visibility 'visibility
  "An indicator and manipulator for hidden group contents."
  :create 'custom-group-visibility-create)

(defun custom-group-visibility-create (widget)
  (let ((visible (widget-value widget)))
    (if visible
	(insert "--------")))
  (widget-default-create widget))

(defun custom-group-members (symbol groups-only)
  "Return SYMBOL's custom group members.
If GROUPS-ONLY non-nil, return only those members that are groups."
  (if (not groups-only)
      (get symbol 'custom-group)
    (let (members)
      (dolist (entry (get symbol 'custom-group))
	(when (eq (nth 1 entry) 'custom-group)
	  (push entry members)))
      (nreverse members))))

(defun custom-group-value-create (widget)
  "Insert a customize group for WIDGET in the current buffer."
  (unless (eq (widget-get widget :custom-state) 'hidden)
    (custom-load-widget widget))
  (let* ((state (widget-get widget :custom-state))
	 (level (widget-get widget :custom-level))
	 ;; (indent (widget-get widget :indent))
	 (prefix (widget-get widget :custom-prefix))
	 (buttons (widget-get widget :buttons))
	 (tag (widget-get widget :tag))
	 (symbol (widget-value widget))
	 (members (custom-group-members symbol
					(and (eq custom-buffer-style 'tree)
					     custom-browse-only-groups)))
	 (doc (widget-docstring widget)))
    (cond ((and (eq custom-buffer-style 'tree)
		(eq state 'hidden)
		(or members (custom-unloaded-widget-p widget)))
	   (custom-browse-insert-prefix prefix)
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-visibility
		  ;; :tag-glyph "plus"
		  :tag "+")
		 buttons)
	   (insert "-- ")
	   ;; (widget-glyph-insert nil "-- " "horizontal")
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-group-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((and (eq custom-buffer-style 'tree)
		(zerop (length members)))
	   (custom-browse-insert-prefix prefix)
	   (insert "[ ]-- ")
	   ;; (widget-glyph-insert nil "[ ]" "empty")
	   ;; (widget-glyph-insert nil "-- " "horizontal")
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-group-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((eq custom-buffer-style 'tree)
	   (custom-browse-insert-prefix prefix)
	   (if (zerop (length members))
	       (progn
		 (custom-browse-insert-prefix prefix)
		 (insert "[ ]-- ")
		 ;; (widget-glyph-insert nil "[ ]" "empty")
		 ;; (widget-glyph-insert nil "-- " "horizontal")
		 (push (widget-create-child-and-convert
			widget 'custom-browse-group-tag)
		       buttons)
		 (insert " " tag "\n")
		 (widget-put widget :buttons buttons))
	     (push (widget-create-child-and-convert
		    widget 'custom-browse-visibility
		    ;; :tag-glyph "minus"
		    :tag "-")
		   buttons)
	     (insert "-\\ ")
	     ;; (widget-glyph-insert nil "-\\ " "top")
	     (push (widget-create-child-and-convert
		    widget 'custom-browse-group-tag)
		   buttons)
	     (insert " " tag "\n")
	     (widget-put widget :buttons buttons)
	     (message "Creating group...")
	     (let* ((members (custom-sort-items
			      members
			      ;; Never sort the top-level custom group.
			      (unless (eq symbol 'emacs)
				custom-browse-sort-alphabetically)
			      custom-browse-order-groups))
		    (prefixes (widget-get widget :custom-prefixes))
		    (custom-prefix-list (custom-prefix-add symbol prefixes))
		    (extra-prefix (if (widget-get widget :custom-last)
				      "   "
				    " | "))
		    (prefix (concat prefix extra-prefix))
		    children entry)
	       (while members
		 (setq entry (car members)
		       members (cdr members))
		 (push (widget-create-child-and-convert
			widget (nth 1 entry)
			:group widget
			:tag (custom-unlispify-tag-name (nth 0 entry))
			:custom-prefixes custom-prefix-list
			:custom-level (1+ level)
			:custom-last (null members)
			:value (nth 0 entry)
			:custom-prefix prefix)
		       children))
	       (widget-put widget :children (reverse children)))
	     (message "Creating group...done")))
	  ;; Nested style.
	  ((eq state 'hidden)
	   ;; Create level indicator.
	   ;; Create tag.
	   (if (eq custom-buffer-style 'links)
	       (push (widget-create-child-and-convert
		      widget 'custom-group-link
		      :tag tag
		      symbol)
		     buttons)
	     (insert-char ?\  (* custom-buffer-indent (1- level)))
	     (insert "-- ")
	     (push (widget-create-child-and-convert
		    widget 'custom-group-visibility
		    :help-echo "Show members of this group."
		    :action 'custom-toggle-parent
		    (not (eq state 'hidden)))
		   buttons))
	   (insert " : ")
	   ;; Create magic button.
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (if (and (eq custom-buffer-style 'links) (> level 1))
	       (widget-put widget :documentation-indent 0))
	   (widget-add-documentation-string-button
	    widget :visibility-widget 'custom-visibility))

	  ;; Nested style.
	  (t				;Visible.
	   ;; Draw a horizontal line (this works for both graphical
	   ;; and text displays):
	   (let ((p (point)))
	     (insert "\n")
	     (put-text-property p (1+ p) 'face '(:underline t))
	     (overlay-put (make-overlay p (1+ p))
			  'before-string
			  (propertize "\n" 'face '(:underline t)
				      'display '(space :align-to 999))))

	   ;; Add parent groups references above the group.
	   (when (eq level 1)
	     (if (custom-add-parent-links widget "Parent groups:")
		 (insert "\n")))
	   (insert-char ?\  (* custom-buffer-indent (1- level)))
	   ;; Create tag.
	   (let ((start (point)))
	     (insert tag " group: ")
	     (widget-specify-sample widget start (point)))
	   (cond
	    ((not doc)
	     (insert " Group definition missing. "))
	    ((< (length doc) 50)
	     (insert doc)))
	   ;; Create visibility indicator.
	   (unless (eq custom-buffer-style 'links)
	     (insert "--------")
	     (push (widget-create-child-and-convert
		    widget 'visibility
		    :help-echo "Hide members of this group."
		    :action 'custom-toggle-parent
		    (not (eq state 'hidden)))
		   buttons)
	     (insert " "))
	   (insert "\n")
	   ;; Create magic button.
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic
			 :indent 0
			 nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (when (and doc (>= (length doc) 50))
	     (widget-add-documentation-string-button
	      widget :visibility-widget 'custom-visibility))

	   ;; Parent groups.
	   (if nil  ;;; This should test that the buffer
		    ;;; was not made to display a group.
	       (when (eq level 1)
		 (insert-char ?\  custom-buffer-indent)
		 (custom-add-parent-links widget)))
	   (custom-add-see-also widget
				(make-string (* custom-buffer-indent level)
					     ?\ ))
	   ;; Members.
	   (message "Creating group...")
	   (let* ((members (custom-sort-items
			    members
			    ;; Never sort the top-level custom group.
			    (unless (eq symbol 'emacs)
			      custom-buffer-sort-alphabetically)
			    custom-buffer-order-groups))
		  (prefixes (widget-get widget :custom-prefixes))
		  (custom-prefix-list (custom-prefix-add symbol prefixes))
		  (len (length members))
		  (count 0)
		  (reporter (make-progress-reporter
			     "Creating group entries..." 0 len))
		  children)
	     (setq children
		   (mapcar
		    (lambda (entry)
		      (widget-insert "\n")
		      (progress-reporter-update reporter (setq count (1+ count)))
		      (let ((sym (nth 0 entry))
			    (type (nth 1 entry)))
			(prog1
			    (widget-create-child-and-convert
			     widget type
			     :group widget
			     :tag (custom-unlispify-tag-name sym)
			     :custom-prefixes custom-prefix-list
			     :custom-level (1+ level)
			     :value sym)
			  (unless (eq (preceding-char) ?\n)
			    (widget-insert "\n")))))
		    members))
	     (mapc 'custom-magic-reset children)
	     (widget-put widget :children children)
	     (custom-group-state-update widget)
	     (progress-reporter-done reporter))
	   ;; End line
	   (let ((p (1+ (point))))
	     (insert "\n\n")
	     (put-text-property p (1+ p) 'face '(:underline t))
	     (overlay-put (make-overlay p (1+ p))
			  'before-string
			  (propertize "\n" 'face '(:underline t)
				      'display '(space :align-to 999))))))))

(defvar custom-group-menu
  `(("Set for Current Session" custom-group-set
     (lambda (widget)
       (eq (widget-get widget :custom-state) 'modified)))
    ,@(when (or custom-file init-file-user)
	'(("Save for Future Sessions" custom-group-save
	   (lambda (widget)
	     (memq (widget-get widget :custom-state) '(modified set))))))
    ("Undo Edits" custom-group-reset-current
     (lambda (widget)
       (memq (widget-get widget :custom-state) '(modified))))
    ("Reset to Saved" custom-group-reset-saved
     (lambda (widget)
       (memq (widget-get widget :custom-state) '(modified set))))
    ,@(when (or custom-file init-file-user)
	'(("Erase Customization" custom-group-reset-standard
	   (lambda (widget)
	     (memq (widget-get widget :custom-state) '(modified set saved)))))))
  "Alist of actions for the `custom-group' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-group'
widget as an argument, and returns non-nil if ACTION is valid on that
widget.  If FILTER is nil, ACTION is always valid.")

(defun custom-group-action (widget &optional event)
  "Show the menu for `custom-group' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name
					   (widget-get widget :value)))
				  (custom-menu-filter custom-group-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-group-set (widget)
  "Set changes in all modified group members."
  (dolist (child (widget-get widget :children))
    (when (eq (widget-get child :custom-state) 'modified)
      (widget-apply child :custom-set))))

(defun custom-group-mark-to-save (widget)
  "Mark all modified group members for saving."
  (dolist (child (widget-get widget :children))
    (when (memq (widget-get child :custom-state) '(modified set))
      (widget-apply child :custom-mark-to-save))))

(defsubst custom-group-state-set-and-redraw (widget)
  "Set state of group widget WIDGET and redraw with current settings."
  (dolist (child (widget-get widget :children))
    (when (memq (widget-get child :custom-state) '(modified set))
      (widget-apply child :custom-state-set-and-redraw))))

(defun custom-group-save (widget)
  "Save all modified group members."
  (custom-group-mark-to-save widget)
  (custom-save-all)
  (custom-group-state-set-and-redraw widget))

(defun custom-group-reset-current (widget)
  "Reset all modified group members."
  (dolist (child (widget-get widget :children))
    (when (eq (widget-get child :custom-state) 'modified)
      (widget-apply child :custom-reset-current))))

(defun custom-group-reset-saved (widget)
  "Reset all modified or set group members."
  (dolist (child (widget-get widget :children))
    (when (memq (widget-get child :custom-state) '(modified set))
      (widget-apply child :custom-reset-saved))))

(defun custom-group-reset-standard (widget)
  "Reset all modified, set, or saved group members."
  (let ((custom-reset-standard-variables-list '(t))
	(custom-reset-standard-faces-list '(t)))
    (custom-group-mark-to-reset-standard widget)
    (custom-reset-standard-save-and-update)))

(defun custom-group-mark-to-reset-standard (widget)
  "Mark to reset all modified, set, or saved group members."
  (dolist (child (widget-get widget :children))
    (when (memq (widget-get child :custom-state)
		'(modified set saved))
      (widget-apply child :custom-mark-to-reset-standard))))

(defun custom-group-state-update (widget)
  "Update magic."
  (unless (eq (widget-get widget :custom-state) 'hidden)
    (let* ((children (widget-get widget :children))
	   (states (mapcar (lambda (child)
			     (widget-get child :custom-state))
			   children))
	   (magics custom-magic-alist)
	   (found 'standard))
      (while magics
	(let ((magic (car (car magics))))
	  (if (and (not (eq magic 'hidden))
		   (memq magic states))
	      (setq found magic
		    magics nil)
	    (setq magics (cdr magics)))))
      (widget-put widget :custom-state found)))
  (custom-magic-reset widget))

;;; Reading and writing the custom file.

;;;###autoload
(defcustom custom-file nil
  "File used for storing customization information.
The default is nil, which means to use your init file
as specified by `user-init-file'.  If the value is not nil,
it should be an absolute file name.

You can set this option through Custom, if you carefully read the
last paragraph below.  However, usually it is simpler to write
something like the following in your init file:

\(setq custom-file \"~/.emacs-custom.el\")
\(load custom-file)

Note that both lines are necessary: the first line tells Custom to
save all customizations in this file, but does not load it.

When you change this variable outside Custom, look in the
previous custom file \(usually your init file) for the
forms `(custom-set-variables ...)'  and `(custom-set-faces ...)',
and copy them (whichever ones you find) to the new custom file.
This will preserve your existing customizations.

If you save this option using Custom, Custom will write all
currently saved customizations, including the new one for this
option itself, into the file you specify, overwriting any
`custom-set-variables' and `custom-set-faces' forms already
present in that file.  It will not delete any customizations from
the old custom file.  You should do that manually if that is what you
want.  You also have to put something like `\(load \"CUSTOM-FILE\")
in your init file, where CUSTOM-FILE is the actual name of the
file.  Otherwise, Emacs will not load the file when it starts up,
and hence will not set `custom-file' to that file either."
  :type '(choice (const :tag "Your Emacs init file" nil)
		 (file :format "%t:%v%d"
		       :doc
		       "Please read entire docstring below before setting \
this through Custom.
Click on \"More\" \(or position point there and press RETURN)
if only the first line of the docstring is shown."))
  :group 'customize)

(defun custom-file (&optional no-error)
  "Return the file name for saving customizations."
  (if (null user-init-file)
      ;; Started with -q, i.e. the file containing Custom settings
      ;; hasn't been read.  Saving settings there won't make much
      ;; sense.
      (if no-error
	  nil
	(error "Saving settings from \"emacs -q\" would overwrite existing customizations"))
    (file-chase-links (or custom-file user-init-file))))

;; If recentf-mode is non-nil, this is defined.
(declare-function recentf-expand-file-name "recentf" (name))

;;;###autoload
(defun custom-save-all ()
  "Save all customizations in `custom-file'."
  (when (and (null custom-file) init-file-had-error)
    (error "Cannot save customizations; init file was not fully loaded"))
  (let* ((filename (custom-file))
	 (recentf-exclude
	  (if recentf-mode
	      (cons (concat "\\`"
			    (regexp-quote
			     (recentf-expand-file-name (custom-file)))
			    "\\'")
		    recentf-exclude)))
	 (old-buffer (find-buffer-visiting filename))
	 old-buffer-name)

    (with-current-buffer (let ((find-file-visit-truename t))
			   (or old-buffer (find-file-noselect filename)))
      ;; We'll save using file-precious-flag, so avoid destroying
      ;; symlinks.  (If we're not already visiting the buffer, this is
      ;; handled by find-file-visit-truename, above.)
      (when old-buffer
	(setq old-buffer-name (buffer-file-name))
	(set-visited-file-name (file-chase-links filename)))

      (unless (eq major-mode 'emacs-lisp-mode)
	(emacs-lisp-mode))
      (let ((inhibit-read-only t)
	    (print-length nil)
	    (print-level nil))
	(custom-save-variables)
	(custom-save-faces))
      (let ((file-precious-flag t))
	(save-buffer))
      (if old-buffer
	  (progn
	    (set-visited-file-name old-buffer-name)
	    (set-buffer-modified-p nil))
	(kill-buffer (current-buffer))))))

;;;###autoload
(defun customize-save-customized ()
  "Save all user options which have been set in this session."
  (interactive)
  (mapatoms (lambda (symbol)
	      (let ((face (get symbol 'customized-face))
		    (value (get symbol 'customized-value))
		    (face-comment (get symbol 'customized-face-comment))
		    (variable-comment
		     (get symbol 'customized-variable-comment)))
		(when face
		  (put symbol 'saved-face face)
		  (custom-push-theme 'theme-face symbol 'user 'set value)
		  (put symbol 'customized-face nil))
		(when value
		  (put symbol 'saved-value value)
		  (custom-push-theme 'theme-value symbol 'user 'set value)
		  (put symbol 'customized-value nil))
		(when variable-comment
		  (put symbol 'saved-variable-comment variable-comment)
		  (put symbol 'customized-variable-comment nil))
		(when face-comment
		  (put symbol 'saved-face-comment face-comment)
		  (put symbol 'customized-face-comment nil)))))
  ;; We really should update all custom buffers here.
  (custom-save-all))

;; Editing the custom file contents in a buffer.

(defun custom-save-delete (symbol)
  "Delete all calls to SYMBOL from the contents of the current buffer.
Leave point at the old location of the first such call,
or (if there were none) at the end of the buffer.

This function does not save the buffer."
  (goto-char (point-min))
  ;; Skip all whitespace and comments.
  (while (forward-comment 1))
  (or (eobp)
      (save-excursion (forward-sexp (buffer-size)))) ; Test for scan errors.
  (let (first)
    (catch 'found
      (while t ;; We exit this loop only via throw.
	;; Skip all whitespace and comments.
	(while (forward-comment 1))
	(let ((start (point))
	      (sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region start (point))
	    (unless first
	      (setq first (point)))))))
    (if first
	(goto-char first)
      ;; Move in front of local variables, otherwise long Custom
      ;; entries would make them ineffective.
      (let ((pos (point-max))
	    (case-fold-search t))
	(save-excursion
	  (goto-char (point-max))
	  (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
			   'move)
	  (when (search-forward "Local Variables:" nil t)
	    (setq pos (line-beginning-position))))
	(goto-char pos)))))

(defvar sort-fold-case) ; defined in sort.el

(defun custom-save-variables ()
  "Save all customized variables in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-set-variables)
    (let ((standard-output (current-buffer))
	  (saved-list (make-list 1 0))
	  sort-fold-case)
      ;; First create a sorted list of saved variables.
      (mapatoms
       (lambda (symbol)
	 (if (and (get symbol 'saved-value)
		  ;; ignore theme values
		  (or (null (get symbol 'theme-value))
		      (eq 'user (caar (get symbol 'theme-value)))))
	     (nconc saved-list (list symbol)))))
      (setq saved-list (sort (cdr saved-list) 'string<))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.\n")
      (dolist (symbol saved-list)
	(let ((spec (car-safe (get symbol 'theme-value)))
	      (value (get symbol 'saved-value))
	      (requests (get symbol 'custom-requests))
	      (now (and (not (custom-variable-p symbol))
			(or (boundp symbol)
			    (eq (get symbol 'force-value)
				'rogue))))
	      (comment (get symbol 'saved-variable-comment)))
	  ;; Check REQUESTS for validity.
	  (dolist (request requests)
	    (when (and (symbolp request) (not (featurep request)))
	      (message "Unknown requested feature: %s" request)
	      (setq requests (delq request requests))))
	  ;; Is there anything customized about this variable?
	  (when (or (and spec (eq (car spec) 'user))
		    comment
		    (and (null spec) (get symbol 'saved-value)))
	    ;; Output an element for this variable.
	    ;; It has the form (SYMBOL VALUE-FORM NOW REQUESTS COMMENT).
	    ;; SYMBOL is the variable name.
	    ;; VALUE-FORM is an expression to return the customized value.
	    ;; NOW if non-nil means always set the variable immediately
	    ;; when the customizations are reloaded.  This is used
	    ;; for rogue variables
	    ;; REQUESTS is a list of packages to load before setting the
	    ;; variable.  Each element of it will be passed to `require'.
	    ;; COMMENT is whatever comment the user has specified
	    ;; with the customize facility.
	    (unless (bolp)
	      (princ "\n"))
	    (princ " '(")
	    (prin1 symbol)
	    (princ " ")
	    (prin1 (car value))
	    (when (or now requests comment)
	      (princ " ")
	      (prin1 now)
	      (when (or requests comment)
		(princ " ")
		(prin1 requests)
		(when comment
		  (princ " ")
		  (prin1 comment))))
	    (princ ")"))))
      (if (bolp)
	  (princ " "))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

(defun custom-save-faces ()
  "Save all customized faces in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-reset-faces)
    (custom-save-delete 'custom-set-faces)
    (let ((standard-output (current-buffer))
	  (saved-list (make-list 1 0))
	  sort-fold-case)
      ;; First create a sorted list of saved faces.
      (mapatoms
       (lambda (symbol)
	 (if (and (get symbol 'saved-face)
		  (eq 'user (car (car-safe (get symbol 'theme-face)))))
	     (nconc saved-list (list symbol)))))
      (setq saved-list (sort (cdr saved-list) 'string<))
      ;; The default face must be first, since it affects the others.
      (if (memq 'default saved-list)
	  (setq saved-list (cons 'default (delq 'default saved-list))))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.\n")
      (dolist (symbol saved-list)
	(let ((spec (car-safe (get symbol 'theme-face)))
	      (value (get symbol 'saved-face))
	      (now (not (or (get symbol 'face-defface-spec)
			    (and (not (custom-facep symbol))
				 (not (get symbol 'force-face))))))
	      (comment (get symbol 'saved-face-comment)))
	  (when (or (and spec (eq (nth 0 spec) 'user))
		    comment
		    (and (null spec) (get symbol 'saved-face)))
	    ;; Don't print default face here.
	    (unless (bolp)
	      (princ "\n"))
	    (princ " '(")
	    (prin1 symbol)
	    (princ " ")
	    (prin1 value)
	    (when (or now comment)
	      (princ " ")
	      (prin1 now)
	      (when comment
		(princ " ")
		(prin1 comment)))
	    (princ ")"))))
      (if (bolp)
	  (princ " "))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

;;; The Customize Menu.

;;; Menu support

(defcustom custom-menu-nesting 2
  "Maximum nesting in custom menus."
  :type 'integer
  :group 'custom-menu)

(defun custom-face-menu-create (_widget symbol)
  "Ignoring WIDGET, create a menu entry for customization face SYMBOL."
  (vector (custom-unlispify-menu-entry symbol)
	  `(customize-face ',symbol)
	  t))

(defun custom-variable-menu-create (_widget symbol)
  "Ignoring WIDGET, create a menu entry for customization variable SYMBOL."
  (let ((type (get symbol 'custom-type)))
    (unless (listp type)
      (setq type (list type)))
    (if (and type (widget-get type :custom-menu))
	(widget-apply type :custom-menu symbol)
      (vector (custom-unlispify-menu-entry symbol)
	      `(customize-variable ',symbol)
	      t))))

;; Add checkboxes to boolean variable entries.
(widget-put (get 'boolean 'widget-type)
	    :custom-menu (lambda (_widget symbol)
			   (vector (custom-unlispify-menu-entry symbol)
				   `(customize-variable ',symbol)
				   ':style 'toggle
				   ':selected symbol)))

(defun custom-group-menu-create (_widget symbol)
  "Ignoring WIDGET, create a menu entry for customization group SYMBOL."
  `( ,(custom-unlispify-menu-entry symbol t)
     :filter (lambda (&rest junk)
	       (let* ((menu (custom-menu-create ',symbol)))
		 (if (consp menu) (cdr menu) menu)))))

;;;###autoload
(defun custom-menu-create (symbol)
  "Create menu for customization group SYMBOL.
The menu is in a format applicable to `easy-menu-define'."
  (let* ((deactivate-mark nil)
	 (item (vector (custom-unlispify-menu-entry symbol)
		       `(customize-group ',symbol)
		       t)))
    (if (and (or (not (boundp 'custom-menu-nesting))
		 (>= custom-menu-nesting 0))
	     (progn
	       (custom-load-symbol symbol)
	       (< (length (get symbol 'custom-group)) widget-menu-max-size)))
	(let ((custom-prefix-list (custom-prefix-add symbol
						     custom-prefix-list))
	      (members (custom-sort-items (get symbol 'custom-group)
					  custom-menu-sort-alphabetically
					  custom-menu-order-groups)))
	  `(,(custom-unlispify-menu-entry symbol t)
	    ,item
	    "--"
	    ,@(mapcar (lambda (entry)
			(widget-apply (if (listp (nth 1 entry))
					  (nth 1 entry)
					(list (nth 1 entry)))
				      :custom-menu (nth 0 entry)))
		      members)))
      item)))

;;;###autoload
(defun customize-menu-create (symbol &optional name)
  "Return a customize menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu.
Otherwise the menu will be named `Customize'.
The format is suitable for use with `easy-menu-define'."
  (unless name
    (setq name "Customize"))
  `(,name
    :filter (lambda (&rest junk)
	      (let ((menu (custom-menu-create ',symbol)))
		(if (consp menu) (cdr menu) menu)))))

;;; Toolbar and menubar support

(easy-menu-define
  Custom-mode-menu (list custom-mode-map custom-field-keymap)
  "Menu used in customization buffers."
  (nconc (list "Custom"
	       (customize-menu-create 'customize))
	 (mapcar (lambda (arg)
		   (let ((tag     (nth 0 arg))
			 (command (nth 1 arg))
			 (active  (nth 2 arg))
			 (help    (nth 3 arg)))
		     (vector tag command :active (eval active) :help help)))
		 custom-commands)))

(defvar tool-bar-map)

;;; `custom-tool-bar-map' used to be set up here.  This will fail to
;;; DTRT when `display-graphic-p' returns nil during compilation.  Hence
;;; we set this up lazily in `Custom-mode'.
(defvar custom-tool-bar-map nil
  "Keymap for toolbar in Custom mode.")

;;; The Custom Mode.

(defun Custom-no-edit (_pos &optional _event)
  "Invoke button at POS, or refuse to allow editing of Custom buffer."
  (interactive "@d")
  (error "You can't edit this part of the Custom buffer"))

(defun Custom-newline (pos &optional event)
  "Invoke button at POS, or refuse to allow editing of Custom buffer."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    ;; If there is no button at point, then use the one at the start
    ;; of the line, if it is a custom-group-link (bug#2298).
    (or button
	(if (setq button (get-char-property (line-beginning-position) 'button))
	    (or (eq (widget-type button) 'custom-group-link)
		(setq button nil))))
    (if button
	(widget-apply-action button event)
      (error "You can't edit this part of the Custom buffer"))))

(defun Custom-goto-parent ()
  "Go to the parent group listed at the top of this buffer.
If several parents are listed, go to the first of them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\nParent groups: " nil t)
	(let* ((button (get-char-property (point) 'button))
	       (parent (downcase (widget-get  button :tag))))
	  (customize-group parent)))))

(defcustom Custom-mode-hook nil
  "Hook called when entering Custom mode."
  :type 'hook
  :group 'custom-buffer)

(defun custom-state-buffer-message (widget)
  (if (eq (widget-get (widget-get widget :parent) :custom-state) 'modified)
      (message "To install your edits, invoke [State] and choose the Set operation")))

(defun custom--initialize-widget-variables ()
  (set (make-local-variable 'widget-documentation-face) 'custom-documentation)
  (set (make-local-variable 'widget-button-face) custom-button)
  (set (make-local-variable 'widget-button-pressed-face) custom-button-pressed)
  (set (make-local-variable 'widget-mouse-face) custom-button-mouse)
  ;; We need this because of the "More" button on docstrings.
  ;; Otherwise clicking on "More" can push point offscreen, which
  ;; causes the window to recenter on point, which pushes the
  ;; newly-revealed docstring offscreen; which is annoying.  -- cyd.
  (set (make-local-variable 'widget-button-click-moves-point) t)
  ;; When possible, use relief for buttons, not bracketing.  This test
  ;; may not be optimal.
  (when custom-raised-buttons
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))
  (setq show-trailing-whitespace nil))

(define-derived-mode Custom-mode nil "Custom"
  "Major mode for editing customization buffers.

The following commands are available:

\\<widget-keymap>\
Move to next button, link or editable field.     \\[widget-forward]
Move to previous button, link or editable field. \\[widget-backward]
\\<custom-field-keymap>\
Complete content of editable text field.   \\[widget-complete]
\\<custom-mode-map>\
Invoke button under the mouse pointer.     \\[widget-button-click]
Invoke button under point.                 \\[widget-button-press]
Set all options from current text.         \\[Custom-set]
Make values in current text permanent.     \\[Custom-save]
Make text match actual option values.      \\[Custom-reset-current]
Reset options to permanent settings.       \\[Custom-reset-saved]
Erase customizations; set options
  and buffer text to the standard values.  \\[Custom-reset-standard]

Entry to this mode calls the value of `Custom-mode-hook'
if that value is non-nil."
  (use-local-map custom-mode-map)
  (easy-menu-add Custom-mode-menu)
  (set (make-local-variable 'tool-bar-map)
       (or custom-tool-bar-map
	   ;; Set up `custom-tool-bar-map'.
	   (let ((map (make-sparse-keymap)))
	     (mapc
	      (lambda (arg)
		(tool-bar-local-item-from-menu
		 (nth 1 arg) (nth 4 arg) map custom-mode-map
		 :label (nth 5 arg)))
	      custom-commands)
	     (setq custom-tool-bar-map map))))
  (make-local-variable 'custom-options)
  (make-local-variable 'custom-local-buffer)
  (custom--initialize-widget-variables)
  (add-hook 'widget-edit-functions 'custom-state-buffer-message nil t))

(put 'Custom-mode 'mode-class 'special)

;; backward-compatibility
(defun custom-mode ()
  "Non-interactive variant of `Custom-mode'."
  (Custom-mode))
(make-obsolete 'custom-mode 'Custom-mode "23.1")
(put 'custom-mode 'mode-class 'special)
(define-obsolete-variable-alias 'custom-mode-hook 'Custom-mode-hook "23.1")

(dolist (regexp
	 '("^No user option defaults have been changed since Emacs "
	   "^Invalid face:? "
	   "^No \\(?:customized\\|rogue\\|saved\\) user options"
	   "^No customizable items matching "
	   "^There are unset changes"
	   "^Cannot set hidden variable"
	   "^No \\(?:saved\\|backup\\) value for "
	   "^No standard setting known for "
	   "^No standard setting for this face"
	   "^Saving settings from \"emacs -q\" would overwrite existing customizations"))
  (add-to-list 'debug-ignored-errors regexp))

;;; The End.

(provide 'cus-edit)

;;; cus-edit.el ends here
