;;; tool-bar.el --- setting up the tool bar

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: mouse frames
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

;; Provides `tool-bar-mode' to control display of the tool-bar and
;; bindings for the global tool bar with convenience functions
;; `tool-bar-add-item' and `tool-bar-add-item-from-menu'.

;; The normal global binding for [tool-bar] (below) uses the value of
;; `tool-bar-map' as the actual keymap to define the tool bar.  Modes
;; may either bind items under the [tool-bar] prefix key of the local
;; map to add to the global bar or may set `tool-bar-map'
;; buffer-locally to override it.  (Some items are removed from the
;; global bar in modes which have `special' as their `mode-class'
;; property.)

;; Todo: Somehow make tool bars easily customizable by the naive?

;;; Code:

;; The autoload cookie doesn't work when preloading.
;; Deleting it means invoking this command won't work
;; when you are on a tty.  I hope that won't cause too much trouble -- rms.
(define-minor-mode tool-bar-mode
  "Toggle the tool bar in all graphical frames (Tool Bar mode).
With a prefix argument ARG, enable Tool Bar mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Tool Bar mode if ARG is omitted or nil.

See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  :init-value t
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable tool-bar-mode
  (let ((val (if tool-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'tool-bar-lines val))
    ;; If the user has given `default-frame-alist' a `tool-bar-lines'
    ;; parameter, replace it.
    (if (assq 'tool-bar-lines default-frame-alist)
	(setq default-frame-alist
	      (cons (cons 'tool-bar-lines val)
		    (assq-delete-all 'tool-bar-lines
				     default-frame-alist)))))
  (and tool-bar-mode
       (= 1 (length (default-value 'tool-bar-map))) ; not yet setup
       (tool-bar-setup)))

;;;###autoload
;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tool-bar-mode-from-frame (&optional arg)
  "Toggle tool bar on or off, based on the status of the current frame.
See `tool-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tool-bar-mode (if (> (frame-parameter nil 'tool-bar-lines) 0) 0 1))
    (tool-bar-mode arg)))

(defvar tool-bar-map (make-sparse-keymap)
  "Keymap for the tool bar.
Define this locally to override the global tool bar.")

(global-set-key [tool-bar]
		`(menu-item ,(purecopy "tool bar") ignore
			    :filter tool-bar-make-keymap))

(declare-function image-mask-p "image.c" (spec &optional frame))

(defconst tool-bar-keymap-cache (make-hash-table :weakness t :test 'equal))

(defun tool-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tool-bar-map'.
Its main job is to figure out which images to use based on the display's
color capability and based on the available image libraries."
  (let ((key (cons (frame-terminal) tool-bar-map)))
    (or (gethash key tool-bar-keymap-cache)
	(puthash key (tool-bar-make-keymap-1) tool-bar-keymap-cache))))

(defun tool-bar-make-keymap-1 ()
  "Generate an actual keymap from `tool-bar-map', without caching."
  (mapcar (lambda (bind)
            (let (image-exp plist)
              (when (and (eq (car-safe (cdr-safe bind)) 'menu-item)
			 ;; For the format of menu-items, see node
			 ;; `Extended Menu Items' in the Elisp manual.
			 (setq plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					     bind))
			 (setq image-exp (plist-get plist :image))
			 (consp image-exp)
			 (not (eq (car image-exp) 'image))
			 (fboundp (car image-exp)))
		(if (not (display-images-p))
		    (setq bind nil)
		  (let ((image (eval image-exp)))
		    (unless (and image (image-mask-p image))
		      (setq image (append image '(:mask heuristic))))
		    (setq bind (copy-sequence bind)
			  plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					bind))
		    (plist-put plist :image image))))
	      bind))
	  tool-bar-map))

;;;###autoload
(defun tool-bar-add-item (icon def key &rest props)
  "Add an item to the tool bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item'."
  (apply 'tool-bar-local-item icon def key tool-bar-map props))

(defun tool-bar--image-expression (icon)
  "Return an expression that evaluates to an image spec for ICON."
  (let* ((fg (face-attribute 'tool-bar :foreground))
	 (bg (face-attribute 'tool-bar :background))
	 (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			(if (eq bg 'unspecified) nil (list :background bg))))
	 (xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
	 (xpm-lo-spec (list :type 'xpm :file
			    (concat "low-color/" icon ".xpm")))
	 (pbm-spec (append (list :type 'pbm :file
                                 (concat icon ".pbm")) colors))
	 (xbm-spec (append (list :type 'xbm :file
                                 (concat icon ".xbm")) colors)))
    `(find-image (cond ((not (display-color-p))
			',(list pbm-spec xbm-spec xpm-lo-spec xpm-spec))
		       ((< (display-color-cells) 256)
			',(list xpm-lo-spec xpm-spec pbm-spec xbm-spec))
		       (t
			',(list xpm-spec pbm-spec xbm-spec))))))

;;;###autoload
(defun tool-bar-local-item (icon def key map &rest props)
  "Add an item to the tool bar in map MAP.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'."
  (let* ((image-exp (tool-bar--image-expression icon)))
    (define-key-after map (vector key)
      `(menu-item ,(symbol-name key) ,def :image ,image-exp ,@props))))

;;;###autoload
(defun tool-bar-add-item-from-menu (command icon &optional map &rest props)
  "Define tool bar binding for COMMAND in keymap MAP using the given ICON.
This makes a binding for COMMAND in `tool-bar-map', copying its
binding from the menu bar in MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

MAP must contain appropriate binding for `[menu-bar]' which holds a keymap.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item-from-menu'."
  (apply 'tool-bar-local-item-from-menu command icon
	 (default-value 'tool-bar-map) map props))

;;;###autoload
(defun tool-bar-local-item-from-menu (command icon in-map &optional from-map &rest props)
  "Define local tool bar binding for COMMAND using the given ICON.
This makes a binding for COMMAND in IN-MAP, copying its binding from
the menu bar in FROM-MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

FROM-MAP must contain appropriate binding for `[menu-bar]' which
holds a keymap."
  (unless from-map
    (setq from-map global-map))
  (let* ((menu-bar-map (lookup-key from-map [menu-bar]))
	 (keys (where-is-internal command menu-bar-map))
	 (image-exp (tool-bar--image-expression icon))
	 submap key)
    ;; We'll pick up the last valid entry in the list of keys if
    ;; there's more than one.
    ;; FIXME: Aren't they *all* "valid"??  --Stef
    (dolist (k keys)
      ;; We're looking for a binding of the command in a submap of
      ;; the menu bar map, so the key sequence must be two or more
      ;; long.
      (if (and (vectorp k)
               (> (length k) 1))
          (let ((m (lookup-key menu-bar-map (substring k 0 -1)))
                ;; Last element in the bound key sequence:
                (kk (aref k (1- (length k)))))
            (if (and (keymapp m)
                     (symbolp kk))
                (setq submap m
                      key kk)))))
    (when (and (symbolp submap) (boundp submap))
      (setq submap (eval submap)))
    (let ((defn (assq key (cdr submap))))
      (if (eq (cadr defn) 'menu-item)
          (define-key-after in-map (vector key)
            (append (cdr defn) (list :image image-exp) props))
        (setq defn (cdr defn))
        (define-key-after in-map (vector key)
          (let ((rest (cdr defn)))
            ;; If the rest of the definition starts
            ;; with a list of menu cache info, get rid of that.
            (if (and (consp rest) (consp (car rest)))
                (setq rest (cdr rest)))
            (append `(menu-item ,(car defn) ,rest)
                    (list :image image-exp) props)))))))

;;; Set up some global items.  Additions/deletions up for grabs.

(defun tool-bar-setup ()
  (setq tool-bar-separator-image-expression
	(tool-bar--image-expression "separator"))
  (tool-bar-add-item-from-menu 'find-file "new" nil :label "New File"
			       :vert-only t)
  (tool-bar-add-item-from-menu 'menu-find-file-existing "open" nil
			       :label "Open" :vert-only t)
  (tool-bar-add-item-from-menu 'dired "diropen" nil :vert-only t)
  (tool-bar-add-item-from-menu 'kill-this-buffer "close" nil :vert-only t)
  (tool-bar-add-item-from-menu 'save-buffer "save" nil
			       :label "Save")
  (define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)
  (tool-bar-add-item-from-menu 'undo "undo" nil)
  (define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			       "cut" nil :vert-only t)
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			       "copy" nil :vert-only t)
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			       "paste" nil :vert-only t)
  (define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)
  (tool-bar-add-item-from-menu 'isearch-forward "search"
			       nil :label "Search" :vert-only t)
  ;;(tool-bar-add-item-from-menu 'ispell-buffer "spell")

  ;; There's no icon appropriate for News and we need a command rather
  ;; than a lambda for Read Mail.
  ;;(tool-bar-add-item-from-menu 'compose-mail "mail/compose")

  ;; Help button on a tool bar is rather non-standard...
  ;; (let ((tool-bar-map (default-value 'tool-bar-map)))
  ;;   (tool-bar-add-item "help" (lambda ()
  ;; 				(interactive)
  ;; 				(popup-menu menu-bar-help-menu))
  ;; 		       'help
  ;; 		       :help "Pop up the Help menu"))
)

(if (featurep 'move-toolbar)
    (defcustom tool-bar-position 'top
      "Specify on which side the tool bar shall be.
Possible values are `top' (tool bar on top), `bottom' (tool bar at bottom),
`left' (tool bar on left) and `right' (tool bar on right).
Customize `tool-bar-mode' if you want to show or hide the tool bar."
      :version "24.1"
      :type '(choice (const top)
		     (const bottom)
		     (const left)
		     (const right))
      :group 'frames
      :initialize 'custom-initialize-default
      :set (lambda (sym val)
	     (set-default sym val)
	     (modify-all-frames-parameters
	      (list (cons 'tool-bar-position val))))))


(provide 'tool-bar)

;;; tool-bar.el ends here
