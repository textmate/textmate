;;; srecode/mode.el --- Minor mode for managing and using SRecode templates

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Minor mode for working with SRecode template files.
;;
;; Depends on Semantic for minor-mode convenience functions.

(require 'mode-local)
(require 'srecode)
(require 'srecode/insert)
(require 'srecode/find)
(require 'srecode/map)
(require 'semantic/decorate)
(require 'semantic/wisent)

(eval-when-compile (require 'semantic/find))

;;; Code:

(defcustom srecode-minor-mode-hook nil
  "Hook run at the end of the function `srecode-minor-mode'."
  :group 'srecode
  :type 'hook)

;; We don't want to waste space.  There is a menu after all.
;;(add-to-list 'minor-mode-alist '(srecode-minor-mode ""))

(defvar srecode-prefix-key [(control ?c) ?/]
  "The common prefix key in srecode minor mode.")

(defvar srecode-prefix-map
  (let ((km (make-sparse-keymap)))
    ;; Basic template codes
    (define-key km "/" 'srecode-insert)
    (define-key km [insert] 'srecode-insert)
    (define-key km "." 'srecode-insert-again)
    (define-key km "E" 'srecode-edit)
    ;; Template indirect binding
    (let ((k ?a))
      (while (<= k ?z)
	(define-key km (format "%c" k) 'srecode-bind-insert)
	(setq k (1+ k))))
    km)
  "Keymap used behind the srecode prefix key in srecode minor mode.")

(defvar srecode-menu-bar
  (list
   "SRecoder"
   (semantic-menu-item
    ["Insert Template"
     srecode-insert
     :active t
     :help "Insert a template by name."
     ])
   (semantic-menu-item
    ["Insert Template Again"
     srecode-insert-again
     :active t
     :help "Run the same template as last time again."
     ])
   (semantic-menu-item
    ["Edit Template"
     srecode-edit
     :active t
     :help "Edit a template for this language by name."
     ])
   "---"
   '( "Insert ..." :filter srecode-minor-mode-templates-menu )
   `( "Generate ..." :filter srecode-minor-mode-generate-menu )
   "---"
    (semantic-menu-item
     ["Customize..."
      (customize-group "srecode")
      :active t
      :help "Customize SRecode options"
      ])
   (list
    "Debugging Tools..."
    (semantic-menu-item
     ["Dump Template MAP"
      srecode-get-maps
      :active t
      :help "Calculate (if needed) and display the current template file map."
      ])
    (semantic-menu-item
     ["Dump Tables"
      srecode-dump-templates
      :active t
      :help "Dump the current template table."
      ])
    (semantic-menu-item
     ["Dump Dictionary"
      srecode-dictionary-dump
      :active t
      :help "Calculate and dump a dictionary for point."
      ])
    (semantic-menu-item
     ["Show Macro Help"
      srecode-macro-help
      :active t
      :help "Display the different types of macros available."
      ])
    )
   )
  "Menu for srecode minor mode.")

(defvar srecode-minor-menu nil
  "Menu keymap build from `srecode-menu-bar'.")

(defcustom srecode-takeover-INS-key nil
  "Use the insert key for inserting templates."
  :group 'srecode
  :type 'boolean)

(defvar srecode-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km srecode-prefix-key srecode-prefix-map)
    (easy-menu-define srecode-minor-menu km "Srecode Minor Mode Menu"
                      srecode-menu-bar)
    (when srecode-takeover-INS-key
      (define-key km [insert] srecode-prefix-map))
    km)
  "Keymap for srecode minor mode.")

;;;###autoload
(define-minor-mode srecode-minor-mode
  "Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}"
  :keymap srecode-mode-map
  ;; If we are turning things on, make sure we have templates for
  ;; this mode first.
  (when srecode-minor-mode
    (when (not (apply
		'append
		(mapcar (lambda (map)
			  (srecode-map-entries-for-mode map major-mode))
			(srecode-get-maps))))
      (setq srecode-minor-mode nil))))

;;;###autoload
(define-minor-mode global-srecode-minor-mode
  "Toggle global use of srecode minor mode.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'srecode
  ;; Not needed because it's autoloaded instead.
  ;; :require 'srecode/mode
  (semantic-toggle-minor-mode-globally
   'srecode-minor-mode (if global-srecode-minor-mode 1 -1)))

;; Use the semantic minor mode magic stuff.
(semantic-add-minor-mode 'srecode-minor-mode "")

;;; Menu Filters
;;
(defun srecode-minor-mode-templates-menu (menu-def)
  "Create a menu item of cascading filters active for this mode.
MENU-DEF is the menu to bind this into."
  ;; Doing this SEGVs Emacs on windows.
  ;;(srecode-load-tables-for-mode major-mode)

  (let* ((modetable (srecode-get-mode-table major-mode))
	 (subtab (when modetable (oref modetable :tables)))
	 (context nil)
	 (active nil)
	 (ltab nil)
	 (temp nil)
	 (alltabs nil)
	 )
    (if (not subtab)
	;; No tables, show a "load the tables" option.
	(list (vector "Load Mode Tables..."
		      (lambda ()
			(interactive)
			(srecode-load-tables-for-mode major-mode))
		      ))
      ;; Build something
      (setq context (car-safe (srecode-calculate-context)))

      (while subtab
	(when (srecode-template-table-in-project-p (car subtab))
	  (setq ltab (oref (car subtab) templates))
	  (while ltab
	    (setq temp (car ltab))

	    ;; Do something with this template.

	    (let* ((ctxt (oref temp context))
		   (ctxtcons (assoc ctxt alltabs))
		   (bind (if (slot-boundp temp 'binding)
			     (oref temp binding)))
		   (name (object-name-string temp)))

	      (when (not ctxtcons)
		(if (string= context ctxt)
		    ;; If this context is not in the current list of contexts
		    ;; is equal to the current context, then manage the
		    ;; active list instead
		    (setq active
			  (setq ctxtcons (or active (cons ctxt nil))))
		  ;; This is not an active context, add it to alltabs.
		  (setq ctxtcons (cons ctxt nil))
		  (setq alltabs (cons ctxtcons alltabs))))

	      (let ((new (vector
			  (if bind
			      (concat name "   (" bind ")")
			    name)
			  `(lambda () (interactive)
			     (srecode-insert (concat ,ctxt ":" ,name)))
			  t)))

		(setcdr ctxtcons (cons
				  new
				  (cdr ctxtcons)))))

	    (setq ltab (cdr ltab))))
  	(setq subtab (cdr subtab)))

      ;; Now create the menu
      (easy-menu-filter-return
       (easy-menu-create-menu
	"Semantic Recoder Filters"
	(append (cdr active)
		alltabs)
	))
      )))

(defvar srecode-minor-mode-generators nil
  "List of code generators to be displayed in the srecoder menu.")

(defun srecode-minor-mode-generate-menu (menu-def)
  "Create a menu item of cascading filters active for this mode.
MENU-DEF is the menu to bind this into."
  ;; Doing this SEGVs Emacs on windows.
  ;;(srecode-load-tables-for-mode major-mode)
  (let ((allgeneratorapps nil))

    (dolist (gen srecode-minor-mode-generators)
      (setq allgeneratorapps
	    (cons (vector (cdr gen) (car gen))
		  allgeneratorapps))
      (message "Adding %S to srecode menu" (car gen))
      )

    (easy-menu-filter-return
     (easy-menu-create-menu
      "Semantic Recoder Generate Filters"
      allgeneratorapps)))
  )

;;; Minor Mode commands
;;
(defun srecode-bind-insert ()
  "Bound insert for Srecode macros.
This command will insert whichever srecode template has a binding
to the current key."
  (interactive)
  (srecode-load-tables-for-mode major-mode)
  (let* ((k last-command-event)
	 (ctxt (srecode-calculate-context))
	 ;; Find the template with the binding K
	 (template (srecode-template-get-table-for-binding
		    (srecode-table) k ctxt)))
    ;; test it.
    (when (not template)
      (error "No template bound to %c" k))
    ;; insert
    (srecode-insert template)
    ))

(defun srecode-edit (template-name)
  "Switch to the template buffer for TEMPLATE-NAME.
Template is chosen based on the mode of the starting buffer."
  ;; @todo - Get a template stack from the last run template, and show
  ;; those too!
  (interactive (list (srecode-read-template-name
		      "Template Name: "
		      (car srecode-read-template-name-history))))
  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
    (let ((temp (srecode-template-get-table (srecode-table) template-name)))
      (if (not temp)
	  (error "No Template named %s" template-name))
      ;; We need a template specific table, since tables chain.
      (let ((tab (oref temp :table))
	    (names nil)
	    )
	(find-file (oref tab :file))
	(setq names (semantic-find-tags-by-name (oref temp :object-name)
						(current-buffer)))
	(cond ((= (length names) 1)
	       (semantic-go-to-tag (car names))
	       (semantic-momentary-highlight-tag (car names)))
	      ((> (length names) 1)
	       (let* ((ctxt (semantic-find-tags-by-name (oref temp :context)
							(current-buffer)))
		      (cls (semantic-find-tags-by-class 'context ctxt))
		      )
		 (while (and names
			     (< (semantic-tag-start (car names))
				(semantic-tag-start (car cls))))
		   (setq names (cdr names)))
		 (if names
		     (progn
		       (semantic-go-to-tag (car names))
		       (semantic-momentary-highlight-tag (car names)))
		   (error "Can't find template %s" template-name))
		 ))
	      (t (error "Can't find template %s" template-name)))
	)))

(defun srecode-add-code-generator (function name &optional binding)
  "Add the srecoder code generator FUNCTION with NAME to the menu.
Optional BINDING specifies the keybinding to use in the srecoder map.
BINDING should be a capital letter.  Lower case letters are reserved
for individual templates.
Optional MODE specifies a major mode this function applies to.
Do not specify a mode if this function could be applied to most
programming modes."
  ;; Update the menu generating part.
  (let ((remloop nil))
    (while (setq remloop (assoc function srecode-minor-mode-generators))
      (setq srecode-minor-mode-generators
	    (remove remloop srecode-minor-mode-generators))))

  (add-to-list 'srecode-minor-mode-generators
	       (cons function name))

  ;; Remove this function from any old bindings.
  (when binding
    (let ((oldkey (where-is-internal function
				      (list srecode-prefix-map)
				      t t t)))
      (if (or (not oldkey)
	      (and (= (length oldkey) 1)
		   (= (length binding) 1)
		   (= (aref oldkey 0) (aref binding 0))))
	  ;; Its the same.
	  nil
	;; Remove the old binding
	(define-key srecode-prefix-map oldkey nil)
	)))

  ;; Update Keybindings
  (let ((oldbinding (lookup-key srecode-prefix-map binding)))

    ;; During development, allow overrides.
    (when (and oldbinding
	       (not (eq oldbinding function))
	       (or (eq this-command 'eval-defun) (eq this-command 'checkdoc-eval-defun))
	       (y-or-n-p (format "Override old binding %s? " oldbinding)))
      (setq oldbinding nil))

    (if (not oldbinding)
	(define-key srecode-prefix-map binding function)
      (if (eq function oldbinding)
	  nil
	;; Not the same.
	(message "Conflict binding %S binding to srecode map."
		 binding))))
  )

;; Add default code generators:
(srecode-add-code-generator 'srecode-document-insert-comment "Comments" "C")
(srecode-add-code-generator 'srecode-insert-getset "Get/Set" "G")

(provide 'srecode/mode)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/mode"
;; End:

;;; srecode/mode.el ends here
