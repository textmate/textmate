;;; semantic/decorate/mode.el --- Minor mode for decorating tags

;; Copyright (C) 2000-2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
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
;; A minor mode for use in decorating tags.
;;
;; There are two types of decorations that can be performed on a tag.
;; You can either highlight the full tag, or you can add an
;; independent decoration on some part of the tag body.
;;
;; For independent decoration in particular, managing them so that they
;; do not get corrupted is challenging.  This major mode and
;; corresponding macros will make handling those types of decorations
;; easier.
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'semantic)
(require 'semantic/decorate)
(require 'semantic/tag-ls)
(require 'semantic/util-modes)

;;; Styles List
;;
(defcustom semantic-decoration-styles nil
  "List of active decoration styles.
It is an alist of \(NAME . FLAG) elements, where NAME is a style name
and FLAG is non-nil if the style is enabled.
See also `define-semantic-decoration-style' which will automatically
add items to this list."
  :group 'semantic
  :type '(repeat (cons (string :tag "Decoration Name")
		       (boolean :tag "Enabled")))
  )

;;; Misc.
;;
(defsubst semantic-decorate-style-predicate (style)
  "Return the STYLE's predicate function."
  (intern (format "%s-p" style)))

(defsubst semantic-decorate-style-highlighter (style)
  "Return the STYLE's highlighter function."
  (intern (format "%s-highlight" style)))

;;; Base decoration API
;;
(defsubst semantic-decoration-p (object)
  "Return non-nil if OBJECT is a tag decoration."
  (and (semantic-overlay-p object)
       (semantic-overlay-get object 'semantic-decoration)))

(defsubst semantic-decoration-set-property (deco property value)
  "Set the DECO decoration's PROPERTY to VALUE.
Return DECO."
  (assert (semantic-decoration-p deco))
  (semantic-overlay-put deco property value)
  deco)

(defsubst semantic-decoration-get-property (deco property)
  "Return the DECO decoration's PROPERTY value."
  (assert (semantic-decoration-p deco))
  (semantic-overlay-get deco property))

(defsubst semantic-decoration-set-face (deco face)
  "Set the face of the decoration DECO to FACE.
Return DECO."
  (semantic-decoration-set-property deco 'face face))

(defsubst semantic-decoration-face (deco)
  "Return the face of the decoration DECO."
  (semantic-decoration-get-property deco 'face))

(defsubst semantic-decoration-set-priority (deco priority)
  "Set the priority of the decoration DECO to PRIORITY.
Return DECO."
  (assert (natnump priority))
  (semantic-decoration-set-property deco 'priority priority))

(defsubst semantic-decoration-priority (deco)
  "Return the priority of the decoration DECO."
  (semantic-decoration-get-property deco 'priority))

(defsubst semantic-decoration-move (deco begin end)
  "Move the decoration DECO on the region between BEGIN and END.
Return DECO."
  (assert (semantic-decoration-p deco))
  (semantic-overlay-move deco begin end)
  deco)

;;; Tag decoration
;;
(defun semantic-decorate-tag (tag begin end &optional face)
  "Add a new decoration on TAG on the region between BEGIN and END.
If optional argument FACE is non-nil, set the decoration's face to
FACE.
Return the overlay that makes up the new decoration."
  (let ((deco (semantic-tag-create-secondary-overlay tag)))
    ;; We do not use the unlink property because we do not want to
    ;; save the highlighting information in the DB.
    (semantic-overlay-put deco 'semantic-decoration t)
    (semantic-decoration-move deco begin end)
    (semantic-decoration-set-face deco face)
    deco))

(defun semantic-decorate-clear-tag (tag &optional deco)
  "Remove decorations from TAG.
If optional argument DECO is non-nil, remove only that decoration."
  (assert (or (null deco) (semantic-decoration-p deco)))
  ;; Clear primary decorations.
  ;; For now, just unhighlight the tag.  How to deal with other
  ;; primary decorations like invisibility, etc. ?  Maybe just
  ;; restoring default values will suffice?
  (semantic-unhighlight-tag tag)
  (semantic-tag-delete-secondary-overlay
   tag (or deco 'semantic-decoration)))

(defun semantic-decorate-tag-decoration (tag)
  "Return decoration found on TAG."
  (semantic-tag-get-secondary-overlay tag 'semantic-decoration))

;;; Global setup of active decorations
;;
(defun semantic-decorate-flush-decorations (&optional buffer)
  "Flush decorations found in BUFFER.
BUFFER defaults to the current buffer.
Should be used to flush decorations that might remain in BUFFER, for
example, after tags have been refreshed."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (o (semantic-overlays-in (point-min) (point-max)))
      (and (semantic-decoration-p o)
           (semantic-overlay-delete o)))))

(defun semantic-decorate-clear-decorations (tag-list)
  "Remove decorations found in tags in TAG-LIST."
  (dolist (tag tag-list)
    (semantic-decorate-clear-tag tag)
    ;; recurse over children
    (semantic-decorate-clear-decorations
     (semantic-tag-components-with-overlays tag))))

(defun semantic-decorate-add-decorations (tag-list)
  "Add decorations to tags in TAG-LIST.
Also make sure old decorations in the area are completely flushed."
  (dolist (tag tag-list)
    ;; Cleanup old decorations.
    (when (semantic-decorate-tag-decoration tag)
      ;; Note on below comment.   This happens more as decorations are refreshed
      ;; mid-way through their use.  Remove the message.

      ;; It would be nice if this never happened, but it still does
      ;; once in a while.  Print a message to help flush these
      ;; situations
      ;;(message "Decorations still on %s" (semantic-format-tag-name tag))
      (semantic-decorate-clear-tag tag))
    ;; Add new decorations.
    (dolist (style semantic-decoration-styles)
      (let ((pred (semantic-decorate-style-predicate   (car style)))
	    (high (semantic-decorate-style-highlighter (car style))))
	(and (cdr style)
	     (fboundp pred)
	     (funcall pred tag)
	     (fboundp high)
	     (funcall high tag))))
    ;; Recurse on the children of all tags
    (semantic-decorate-add-decorations
     (semantic-tag-components-with-overlays tag))))

;;; PENDING DECORATIONS
;;
;; Activities in Emacs may cause a decoration to change state.  Any
;; such identified change ought to be setup as PENDING.  This means
;; that the next idle step will do the decoration change, but at the
;; time of the state change, minimal work would be done.
(defvar semantic-decorate-pending-decoration-hook nil
  "Normal hook run to perform pending decoration changes.")

(semantic-varalias-obsolete 'semantic-decorate-pending-decoration-hooks
			    'semantic-decorate-pending-decoration-hook "23.2")

(defun semantic-decorate-add-pending-decoration (fcn &optional buffer)
  "Add a pending decoration change represented by FCN.
Applies only to the current BUFFER.
The setting of FCN will be removed after it is run."
  (save-excursion
    (when buffer (set-buffer buffer))
    (semantic-make-local-hook 'semantic-decorate-flush-pending-decorations)
    (add-hook 'semantic-decorate-pending-decoration-hook fcn nil t)))

(defun semantic-decorate-flush-pending-decorations (&optional buffer)
  "Flush any pending decorations for BUFFER.
Flush functions from `semantic-decorate-pending-decoration-hook'."
  (save-excursion
    (when buffer (set-buffer buffer))
    (run-hooks 'semantic-decorate-pending-decoration-hook)
    ;; Always reset the hooks
    (setq semantic-decorate-pending-decoration-hook nil)))


;;; DECORATION MODE
;;
;; Generic mode for handling basic highlighting and decorations.
;;

;;;###autoload
(define-minor-mode global-semantic-decoration-mode
  "Toggle global use of option `semantic-decoration-mode'.
Decoration mode turns on all active decorations as specified
by `semantic-decoration-styles'."
  :global t :group 'semantic :group 'semantic-modes
  ;; Not needed because it's autoloaded instead.
  ;; :require 'semantic/decorate/mode
  (semantic-toggle-minor-mode-globally
   'semantic-decoration-mode (if global-semantic-decoration-mode 1 -1)))

(defcustom semantic-decoration-mode-hook nil
  "Hook run at the end of function `semantic-decoration-mode'."
  :group 'semantic
  :type 'hook)

(define-minor-mode semantic-decoration-mode
  "Minor mode for decorating tags.
Decorations are specified in `semantic-decoration-styles'.
You can define new decoration styles with
`define-semantic-decoration-style'.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
;;
;;\\{semantic-decoration-map}"
  nil nil nil
  (if semantic-decoration-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-decoration-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Add hooks
        (semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
        (add-hook 'semantic-after-partial-cache-change-hook
                  'semantic-decorate-tags-after-partial-reparse nil t)
        (semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
        (add-hook 'semantic-after-toplevel-cache-change-hook
                  'semantic-decorate-tags-after-full-reparse nil t)
        ;; Add decorations to available tags.  The above hooks ensure
        ;; that new tags will be decorated when they become available.
        (semantic-decorate-add-decorations (semantic-fetch-available-tags)))
    ;; Remove decorations from available tags.
    (semantic-decorate-clear-decorations (semantic-fetch-available-tags))
    ;; Cleanup any leftover crap too.
    (semantic-decorate-flush-decorations)
    ;; Remove hooks
    (remove-hook 'semantic-after-partial-cache-change-hook
                 'semantic-decorate-tags-after-partial-reparse t)
    (remove-hook 'semantic-after-toplevel-cache-change-hook
                 'semantic-decorate-tags-after-full-reparse t)))

(semantic-add-minor-mode 'semantic-decoration-mode
                         "")

(defun semantic-decorate-tags-after-full-reparse (tag-list)
  "Add decorations after a complete reparse of the current buffer.
TAG-LIST is the list of tags recently parsed.
Flush all existing decorations and call `semantic-decorate-add-decorations' to
add decorations.
Called from `semantic-after-toplevel-cache-change-hook'."
  ;; Flush everything
  (semantic-decorate-flush-decorations)
  ;; Add it back on
  (semantic-decorate-add-decorations tag-list))

(defun semantic-decorate-tags-after-partial-reparse (tag-list)
  "Add decorations when new tags are created in the current buffer.
TAG-LIST is the list of newly created tags.
Call `semantic-decorate-add-decorations' to add decorations.
Called from `semantic-after-partial-cache-change-hook'."
  (semantic-decorate-add-decorations tag-list))


;;; Enable/Disable toggling
;;
(defun semantic-decoration-style-enabled-p (style)
  "Return non-nil if STYLE is currently enabled.
Return nil if the style is disabled, or does not exist."
  (let ((pair (assoc style semantic-decoration-styles)))
    (and pair (cdr pair))))

(defun semantic-toggle-decoration-style (name &optional arg)
  "Turn on/off the decoration style with NAME.
Decorations are specified in `semantic-decoration-styles'.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the decoration style is enabled."
  (interactive
   (list (completing-read "Decoration style: "
                          semantic-decoration-styles nil t)
         current-prefix-arg))
  (setq name (format "%s" name)) ;; Ensure NAME is a string.
  (unless (equal name "")
    (let* ((style (assoc name semantic-decoration-styles))
           (flag  (if arg
                      (> (prefix-numeric-value arg) 0)
                    (not (cdr style)))))
      (unless (eq (cdr style) flag)
        ;; Store the new flag.
        (setcdr style flag)
        ;; Refresh decorations is `semantic-decoration-mode' is on.
        (when semantic-decoration-mode
          (semantic-decoration-mode -1)
          (semantic-decoration-mode 1))
        (when (called-interactively-p 'interactive)
          (message "Decoration style %s turned %s" (car style)
                   (if flag "on" "off"))))
      flag)))

(defvar semantic-decoration-menu-cache nil
  "Cache of the decoration menu.")

(defun semantic-decoration-build-style-menu (style)
  "Build a menu item for controlling a specific decoration STYLE."
  (vector (car style)
	  `(lambda () (interactive)
	     (semantic-toggle-decoration-style
	      ,(car style)))
	  :style 'toggle
	  :selected `(semantic-decoration-style-enabled-p ,(car style))
	  ))

(defun semantic-build-decoration-mode-menu (&rest ignore)
  "Create a menu listing all the known decorations for toggling.
IGNORE any input arguments."
  (or semantic-decoration-menu-cache
      (setq semantic-decoration-menu-cache
	    (mapcar 'semantic-decoration-build-style-menu
		    (reverse semantic-decoration-styles))
	    )))


;;; Defining decoration styles
;;
(defmacro define-semantic-decoration-style (name doc &rest flags)
  "Define a new decoration style with NAME.
DOC is a documentation string describing the decoration style NAME.
It is appended to auto-generated doc strings.
An Optional list of FLAGS can also be specified.  Flags are:
  :enabled <value>  - specify the default enabled value for NAME.


This defines two new overload functions respectively called `NAME-p'
and `NAME-highlight', for which you must provide a default
implementation in respectively the functions `NAME-p-default' and
`NAME-highlight-default'.  Those functions are passed a tag.  `NAME-p'
must return non-nil to indicate that the tag should be decorated by
`NAME-highlight'.

To put primary decorations on a tag `NAME-highlight' must use
functions like `semantic-set-tag-face', `semantic-set-tag-intangible',
etc., found in the semantic-decorate library.

To add other kind of decorations on a tag, `NAME-highlight' must use
`semantic-decorate-tag', and other functions of the semantic
decoration API found in this library."
  (let ((predicate   (semantic-decorate-style-predicate   name))
        (highlighter (semantic-decorate-style-highlighter name))
	(defaultenable (if (plist-member flags :enabled)
			   (plist-get flags :enabled)
			 t))
	)
    `(progn
       ;; Clear the menu cache so that new items are added when
       ;; needed.
       (setq semantic-decoration-menu-cache nil)
       ;; Create an override method to specify if a given tag belongs
       ;; to this type of decoration
       (define-overloadable-function ,predicate (tag)
         ,(format "Return non-nil to decorate TAG with `%s' style.\n%s"
                  name doc))
       ;; Create an override method that will perform the highlight
       ;; operation if the -p method returns non-nil.
       (define-overloadable-function ,highlighter (tag)
         ,(format "Decorate TAG with `%s' style.\n%s"
                  name doc))
       ;; Add this to the list of primary decoration modes.
       (add-to-list 'semantic-decoration-styles
                    (cons ',(symbol-name name)
			  ,defaultenable))
       )))

;;; Predefined decoration styles
;;

;;; Tag boundaries highlighting
;;
(define-semantic-decoration-style semantic-tag-boundary
  "Place an overline in front of each long tag.
Does not provide overlines for prototypes.")

(defface semantic-tag-boundary-face
  '((((class color) (background dark))
     (:overline "cyan"))
    (((class color) (background light))
     (:overline "blue")))
  "*Face used to show long tags in.
Used by decoration style: `semantic-tag-boundary'."
  :group 'semantic-faces)

(defun semantic-tag-boundary-p-default (tag)
  "Return non-nil if TAG is a type, or a non-prototype function."
  (let ((c (semantic-tag-class tag)))
    (and
     (or
      ;; All types get a line?
      (eq c 'type)
      ;; Functions which aren't prototypes get a line.
      (and (eq c 'function)
           (not (semantic-tag-get-attribute tag :prototype-flag)))
      )
     ;; Note: The below restriction confused users.
     ;;
     ;; Nothing smaller than a few lines
     ;;(> (- (semantic-tag-end tag) (semantic-tag-start tag)) 150)
     ;; Random truth
     t)
    ))

(defun semantic-tag-boundary-highlight-default (tag)
  "Highlight the first line of TAG as a boundary."
  (when (bufferp (semantic-tag-buffer tag))
    (with-current-buffer (semantic-tag-buffer tag)
      (semantic-decorate-tag
       tag
       (semantic-tag-start tag)
       (save-excursion
	 (goto-char (semantic-tag-start tag))
	 (end-of-line)
	 (forward-char 1)
	 (point))
       'semantic-tag-boundary-face))
    ))

;;; Private member highlighting
;;
(define-semantic-decoration-style semantic-decoration-on-private-members
  "Highlight class members that are designated as PRIVATE access."
  :enabled nil)

(defface semantic-decoration-on-private-members-face
  '((((class color) (background dark))
     (:background "#200000"))
    (((class color) (background light))
     (:background "#8fffff")))
  "*Face used to show privately scoped tags in.
Used by the decoration style: `semantic-decoration-on-private-members'."
  :group 'semantic-faces)

(defun semantic-decoration-on-private-members-highlight-default (tag)
  "Highlight TAG as designated to have PRIVATE access.
Use a primary decoration."
  (semantic-set-tag-face
   tag 'semantic-decoration-on-private-members-face))

(defun semantic-decoration-on-private-members-p-default (tag)
  "Return non-nil if TAG has PRIVATE access."
  (and (member (semantic-tag-class tag) '(function variable))
       (eq (semantic-tag-protection tag) 'private)))

;;; Protected member highlighting
;;
(defface semantic-decoration-on-protected-members-face
  '((((class color) (background dark))
     (:background "#000020"))
    (((class color) (background light))
     (:background "#fffff8")))
  "*Face used to show protected scoped tags in.
Used by the decoration style: `semantic-decoration-on-protected-members'."
  :group 'semantic-faces)

(define-semantic-decoration-style semantic-decoration-on-protected-members
  "Highlight class members that are designated as PROTECTED access."
  :enabled nil)

(defun semantic-decoration-on-protected-members-p-default (tag)
  "Return non-nil if TAG has PROTECTED access."
  (and (member (semantic-tag-class tag) '(function variable))
       (eq (semantic-tag-protection tag) 'protected)))

(defun semantic-decoration-on-protected-members-highlight-default (tag)
  "Highlight TAG as designated to have PROTECTED access.
Use a primary decoration."
  (semantic-set-tag-face
   tag 'semantic-decoration-on-protected-members-face))

(provide 'semantic/decorate/mode)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/decorate/mode"
;; End:

;;; semantic/decorate/mode.el ends here
