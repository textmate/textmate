;;; tree-widget.el --- Tree widget

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 16 Feb 2001
;; Keywords: extensions

;; This file is part of GNU Emacs

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
;; This library provide a tree widget useful to display data
;; structures organized in a hierarchical order.
;;
;; The following properties are specific to the tree widget:
;;
;; :open
;;    Set to non-nil to expand the tree.  By default the tree is
;;    collapsed.
;;
;; :node
;;    Specify the widget used to represent the value of a tree node.
;;    By default this is an `item' widget which displays the
;;    tree-widget :tag property value if defined, or a string
;;    representation of the tree-widget value.
;;
;; :keep
;;    Specify a list of properties to keep when the tree is collapsed
;;    so they can be recovered when the tree is expanded.  This
;;    property can be used in child widgets too.
;;
;; :expander (obsoletes :dynargs)
;;    Specify a function to be called to dynamically provide the
;;    tree's children in response to an expand request.  This function
;;    will be passed the tree widget and must return a list of child
;;    widgets.  Child widgets returned by the :expander function are
;;    stored in the :args property of the tree widget.
;;
;; :expander-p
;;    Specify a predicate which must return non-nil to indicate that
;;    the :expander function above has to be called.  By default, to
;;    speed up successive expand requests, the :expander-p predicate
;;    return non-nil when the :args value is nil.  So, by default, to
;;    refresh child values, it is necessary to set the :args property
;;    to nil, then redraw the tree.
;;
;; :open-icon  (default `tree-widget-open-icon')
;; :close-icon (default `tree-widget-close-icon')
;; :empty-icon (default `tree-widget-empty-icon')
;; :leaf-icon  (default `tree-widget-leaf-icon')
;;    Those properties define the icon widgets associated to tree
;;    nodes.  Icon widgets must derive from the `tree-widget-icon'
;;    widget.  The :tag and :glyph-name property values are
;;    respectively used when drawing the text and graphic
;;    representation of the tree.  The :tag value must be a string
;;    that represent a node icon, like "[+]" for example.  The
;;    :glyph-name value must the name of an image found in the current
;;    theme, like "close" for example (see also the variable
;;    `tree-widget-theme').
;;
;; :guide      (default `tree-widget-guide')
;; :end-guide  (default `tree-widget-end-guide')
;; :no-guide   (default `tree-widget-no-guide')
;; :handle     (default `tree-widget-handle')
;; :no-handle  (default `tree-widget-no-handle')
;;    Those properties define `item'-like widgets used to draw the
;;    tree guide lines.  The :tag property value is used when drawing
;;    the text representation of the tree.  The graphic look and feel
;;    is given by the images named "guide", "no-guide", "end-guide",
;;    "handle", and "no-handle" found in the current theme (see also
;;    the variable `tree-widget-theme').
;;
;; These are the default :tag values for icons, and guide lines:
;;
;; open-icon    "[-]"
;; close-icon   "[+]"
;; empty-icon   "[X]"
;; leaf-icon    ""
;; guide        " |"
;; no-guide     "  "
;; end-guide    " `"
;; handle       "-"
;; no-handle    " "
;;
;; The text representation of a tree looks like this:
;;
;; [-] 1        (open-icon :node)
;;  |-[+] 1.0   (guide+handle+close-icon :node)
;;  |-[X] 1.1   (guide+handle+empty-icon :node)
;;  `-[-] 1.2   (end-guide+handle+open-icon :node)
;;     |- 1.2.1 (no-guide+no-handle+guide+handle+leaf-icon leaf)
;;     `- 1.2.2 (no-guide+no-handle+end-guide+handle+leaf-icon leaf)
;;
;; By default, images will be used instead of strings to draw a
;; nice-looking tree.  See the `tree-widget-image-enable',
;; `tree-widget-themes-directory', and `tree-widget-theme' options for
;; more details.

;;; History:
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'wid-edit)

;;; Customization
;;
(defgroup tree-widget nil
  "Customization support for the Tree Widget library."
  :version "22.1"
  :group 'widgets)

(defcustom tree-widget-image-enable (if (fboundp 'display-images-p)
                                        (display-images-p))
  "Non-nil means that tree-widget will try to use images."
  :type  'boolean
  :group 'tree-widget)

(defvar tree-widget-themes-load-path
  '(load-path
    (let ((dir (if (fboundp 'locate-data-directory)
                   (locate-data-directory "tree-widget") ;; XEmacs
                 data-directory)))
      (and dir (list dir (expand-file-name "images" dir))))
    )
  "List of locations in which to search for the themes sub-directory.
Each element is an expression that will be recursively evaluated until
it returns a single directory or a list of directories.
The default is to search in the `load-path' first, then in the
\"images\" sub directory in the data directory, then in the data
directory.
The data directory is the value of the variable `data-directory' on
Emacs, and what `(locate-data-directory \"tree-widget\")' returns on
XEmacs.")

(defcustom tree-widget-themes-directory "tree-widget"
  "Name of the directory in which to look for an image theme.
When nil use the directory where the tree-widget library is located.
When it is a relative name, search in all occurrences of that sub
directory in the path specified by `tree-widget-themes-load-path'.
The default is to use the \"tree-widget\" relative name."
  :type '(choice (const :tag "Default" "tree-widget")
                 (const :tag "Where is this library" nil)
                 (directory :format "%{%t%}:\n%v"))
  :group 'tree-widget)

(defcustom tree-widget-theme nil
  "Name of the theme in which to look for images.
This is a sub directory of the themes directory specified by the
`tree-widget-themes-directory' option.
The default theme is \"default\".  When an image is not found in a
theme, it is searched in its parent theme.

A complete theme must at least contain images with these file names
with a supported extension (see also `tree-widget-image-formats'):

\"guide\"
  A vertical guide line.
\"no-guide\"
  An invisible vertical guide line.
\"end-guide\"
  End of a vertical guide line.
\"handle\"
  Horizontal guide line that joins the vertical guide line to an icon.
\"no-handle\"
  An invisible handle.

Plus images whose name is given by the :glyph-name property of the
icon widgets used to draw the tree.  By default these images are used:

\"open\"
  Icon associated to an expanded tree.
\"close\"
  Icon associated to a collapsed tree.
\"empty\"
  Icon associated to an expanded tree with no child.
\"leaf\"
  Icon associated to a leaf node."
  :type '(choice (const  :tag "Default" nil)
                 (string :tag "Name"))
  :group 'tree-widget)

(defcustom tree-widget-image-properties-emacs
  '(:ascent center :mask (heuristic t))
  "Default properties of Emacs images."
  :type 'plist
  :group 'tree-widget)

(defcustom tree-widget-image-properties-xemacs
  nil
  "Default properties of XEmacs images."
  :type 'plist
  :group 'tree-widget)

(defcustom tree-widget-space-width 0.5
  "Amount of space between an icon image and a node widget.
Must be a valid space :width display property."
  :group 'tree-widget
  :type 'sexp)

;;; Image support
;;
(eval-and-compile ;; Emacs/XEmacs compatibility stuff
  (cond
   ;; XEmacs
   ((featurep 'xemacs)
    (defsubst tree-widget-use-image-p ()
      "Return non-nil if image support is currently enabled."
      (and tree-widget-image-enable
           widget-glyph-enable
           (console-on-window-system-p)))
    (defsubst tree-widget-create-image (type file &optional props)
      "Create an image of type TYPE from FILE, and return it.
Give the image the specified properties PROPS."
      (apply 'make-glyph `([,type :file ,file ,@props])))
    (defsubst tree-widget-image-formats ()
      "Return the alist of image formats/file name extensions.
See also the option `widget-image-file-name-suffixes'."
      (delq nil
            (mapcar
             #'(lambda (fmt)
                 (and (valid-image-instantiator-format-p (car fmt)) fmt))
             widget-image-file-name-suffixes)))
    )
   ;; Emacs
   (t
    (defsubst tree-widget-use-image-p ()
      "Return non-nil if image support is currently enabled."
      (and tree-widget-image-enable
           widget-image-enable
           (display-images-p)))
    (defsubst tree-widget-create-image (type file &optional props)
      "Create an image of type TYPE from FILE, and return it.
Give the image the specified properties PROPS."
      (apply 'create-image `(,file ,type nil ,@props)))
    (defsubst tree-widget-image-formats ()
      "Return the alist of image formats/file name extensions.
See also the option `widget-image-conversion'."
      (delq nil
            (mapcar
             #'(lambda (fmt)
                 (and (image-type-available-p (car fmt)) fmt))
             widget-image-conversion)))
    ))
  )

;; Buffer local cache of theme data.
(defvar tree-widget--theme nil)

(defsubst tree-widget-theme-name ()
  "Return the current theme name, or nil if no theme is active."
  (and tree-widget--theme (car (aref tree-widget--theme 0))))

(defsubst tree-widget-set-parent-theme (name)
  "Set to NAME the parent theme of the current theme.
The default parent theme is the \"default\" theme."
  (unless (member name (aref tree-widget--theme 0))
    (aset tree-widget--theme 0
          (append (aref tree-widget--theme 0) (list name)))
    ;; Load the theme setup from the first directory where the theme
    ;; is found.
    (catch 'found
      (dolist (dir (tree-widget-themes-path))
        (setq dir (expand-file-name name dir))
        (when (file-accessible-directory-p dir)
          (throw 'found
                 (load (expand-file-name
                        "tree-widget-theme-setup" dir) t)))))))

(defun tree-widget-set-theme (&optional name)
  "In the current buffer, set the theme to use for images.
The current buffer must be where the tree widget is drawn.
Optional argument NAME is the name of the theme to use.  It defaults
to the value of the variable `tree-widget-theme'.
Does nothing if NAME is already the current theme.

If there is a \"tree-widget-theme-setup\" library in the theme
directory, load it to setup a parent theme or the images properties.
Typically it should contain something like this:

  (tree-widget-set-parent-theme \"my-parent-theme\")
  (tree-widget-set-image-properties
   (if (featurep 'xemacs)
       '(:ascent center)
     '(:ascent center :mask (heuristic t))
     ))"
  (or name (setq name (or tree-widget-theme "default")))
  (unless (string-equal name (tree-widget-theme-name))
    (set (make-local-variable 'tree-widget--theme)
         (make-vector 4 nil))
      (tree-widget-set-parent-theme name)
      (tree-widget-set-parent-theme "default")))

(defun tree-widget--locate-sub-directory (name path &optional found)
  "Locate all occurrences of the sub-directory NAME in PATH.
Return a list of absolute directory names in reverse order, or nil if
not found."
  (condition-case err
      (dolist (elt path)
        (setq elt (eval elt))
        (cond
         ((stringp elt)
          (and (file-accessible-directory-p
                (setq elt (expand-file-name name elt)))
               (push elt found)))
         (elt
          (setq found (tree-widget--locate-sub-directory
                       name (if (atom elt) (list elt) elt) found)))))
    (error
     (message "In tree-widget--locate-sub-directory: %s"
              (error-message-string err))))
  found)

(defun tree-widget-themes-path ()
  "Return the path where to search for a theme.
It is specified in variable `tree-widget-themes-directory'.
Return a list of absolute directory names, or nil when no directory
has been found accessible."
  (let ((path (aref tree-widget--theme 1)))
    (cond
     ;; No directory was found.
     ((eq path 'void) nil)
     ;; The list of directories is available in the cache.
     (path)
     ;; Use the directory where this library is located.
     ((null tree-widget-themes-directory)
      (when (setq path (locate-library "tree-widget"))
        (setq path (file-name-directory path))
        (setq path (and (file-accessible-directory-p path)
                        (list path)))
        ;; Store the result in the cache for later use.
        (aset tree-widget--theme 1 (or path 'void))
        path))
     ;; Check accessibility of absolute directory name.
     ((file-name-absolute-p tree-widget-themes-directory)
      (setq path (expand-file-name tree-widget-themes-directory))
      (setq path (and (file-accessible-directory-p path)
                      (list path)))
      ;; Store the result in the cache for later use.
      (aset tree-widget--theme 1 (or path 'void))
      path)
     ;; Locate a sub-directory in `tree-widget-themes-load-path'.
     (t
      (setq path (nreverse (tree-widget--locate-sub-directory
                            tree-widget-themes-directory
                            tree-widget-themes-load-path)))
      ;; Store the result in the cache for later use.
      (aset tree-widget--theme 1 (or path 'void))
      path))))

(defconst tree-widget--cursors
  ;; Pointer shapes when the mouse pointer is over inactive
  ;; tree-widget images.  This feature works since Emacs 22, and
  ;; ignored on older versions, and XEmacs.
  '(
    ("guide"     . arrow)
    ("no-guide"  . arrow)
    ("end-guide" . arrow)
    ("handle"    . arrow)
    ("no-handle" . arrow)
    ))

(defsubst tree-widget-set-image-properties (props)
  "In current theme, set images properties to PROPS.
Does nothing if images properties have already been set for that
theme."
  (or (aref tree-widget--theme 2)
      (aset tree-widget--theme 2 props)))

(defsubst tree-widget-image-properties (name)
  "Return the properties of image NAME in current theme.
Default global properties are provided for respectively Emacs and
XEmacs in the variables `tree-widget-image-properties-emacs', and
`tree-widget-image-properties-xemacs'."
  ;; Add the pointer shape
  (cons :pointer
        (cons (or (cdr (assoc name tree-widget--cursors)) 'hand)
              (tree-widget-set-image-properties
               (if (featurep 'xemacs)
                   tree-widget-image-properties-xemacs
                 tree-widget-image-properties-emacs)))))

(defun tree-widget-lookup-image (name)
  "Look up in current theme for an image with NAME.
Search first in current theme, then in parent themes (see also the
function `tree-widget-set-parent-theme').
Return the first image found having a supported format, or nil if not
found."
  (let (file)
    (catch 'found
      (dolist (default-directory (tree-widget-themes-path))
        (dolist (dir (aref tree-widget--theme 0))
          (dolist (fmt (tree-widget-image-formats))
            (dolist (ext (cdr fmt))
              (setq file (expand-file-name (concat name ext) dir))
              (and (file-readable-p file)
                   (file-regular-p file)
                   (throw 'found
                          (tree-widget-create-image
                           (car fmt) file
                           (tree-widget-image-properties name))))))))
      nil)))

(defun tree-widget-find-image (name)
  "Find the image with NAME in current theme.
NAME is an image file name sans extension.
Return the image found, or nil if not found."
  (when (tree-widget-use-image-p)
    ;; Ensure there is an active theme.
    (tree-widget-set-theme (tree-widget-theme-name))
    (let ((image (assoc name (aref tree-widget--theme 3))))
      ;; The image NAME is found in the cache.
      (if image
          (cdr image)
        ;; Search the image in current, and default themes.
        (prog1
            (setq image (tree-widget-lookup-image name))
          ;; Store image reference in the cache for later use.
          (push (cons name image) (aref tree-widget--theme 3))))
      )))

;;; Widgets
;;
(defun tree-widget-button-click (event)
  "Move to the position clicked on, and if it is a button, invoke it.
EVENT is the mouse event received."
  (interactive "e")
  (mouse-set-point event)
  (let ((pos (widget-event-point event)))
    (if (get-char-property pos 'button)
        (widget-button-click event))))

(defvar tree-widget-button-keymap
  (let ((km (make-sparse-keymap)))
    (if (boundp 'widget-button-keymap)
        ;; XEmacs
        (progn
          (set-keymap-parent km widget-button-keymap)
          (define-key km [button1] 'tree-widget-button-click))
      ;; Emacs
      (set-keymap-parent km widget-keymap)
      (define-key km [down-mouse-1] 'tree-widget-button-click))
    km)
  "Keymap used inside node buttons.
Handle mouse button 1 click on buttons.")

(define-widget 'tree-widget-icon 'push-button
  "Basic widget other tree-widget icons are derived from."
  :format        "%[%t%]"
  :button-keymap tree-widget-button-keymap ; XEmacs
  :keymap        tree-widget-button-keymap ; Emacs
  :create        'tree-widget-icon-create
  :action        'tree-widget-icon-action
  :help-echo     'tree-widget-icon-help-echo
  )

(define-widget 'tree-widget-open-icon 'tree-widget-icon
  "Icon for an expanded tree-widget node."
  :tag        "[-]"
  :glyph-name "open"
  )

(define-widget 'tree-widget-empty-icon 'tree-widget-icon
  "Icon for an expanded tree-widget node with no child."
  :tag        "[X]"
  :glyph-name "empty"
  )

(define-widget 'tree-widget-close-icon 'tree-widget-icon
  "Icon for a collapsed tree-widget node."
  :tag        "[+]"
  :glyph-name "close"
  )

(define-widget 'tree-widget-leaf-icon 'tree-widget-icon
  "Icon for a tree-widget leaf node."
  :tag        ""
  :glyph-name "leaf"
  :button-face 'default
  )

(define-widget 'tree-widget-guide 'item
  "Vertical guide line."
  :tag       " |"
  ;;:tag-glyph (tree-widget-find-image "guide")
  :format    "%t"
  )

(define-widget 'tree-widget-end-guide 'item
  "End of a vertical guide line."
  :tag       " `"
  ;;:tag-glyph (tree-widget-find-image "end-guide")
  :format    "%t"
  )

(define-widget 'tree-widget-no-guide 'item
  "Invisible vertical guide line."
  :tag       "  "
  ;;:tag-glyph (tree-widget-find-image "no-guide")
  :format    "%t"
  )

(define-widget 'tree-widget-handle 'item
  "Horizontal guide line that joins a vertical guide line to a node."
  :tag       "-"
  ;;:tag-glyph (tree-widget-find-image "handle")
  :format    "%t"
  )

(define-widget 'tree-widget-no-handle 'item
  "Invisible handle."
  :tag       " "
  ;;:tag-glyph (tree-widget-find-image "no-handle")
  :format    "%t"
  )

(define-widget 'tree-widget 'default
  "Tree widget."
  :format         "%v"
  :convert-widget 'tree-widget-convert-widget
  :value-get      'widget-value-value-get
  :value-delete   'widget-children-value-delete
  :value-create   'tree-widget-value-create
  :action         'tree-widget-action
  :help-echo      'tree-widget-help-echo
  :expander-p     'tree-widget-expander-p
  :open-icon      'tree-widget-open-icon
  :close-icon     'tree-widget-close-icon
  :empty-icon     'tree-widget-empty-icon
  :leaf-icon      'tree-widget-leaf-icon
  :guide          'tree-widget-guide
  :end-guide      'tree-widget-end-guide
  :no-guide       'tree-widget-no-guide
  :handle         'tree-widget-handle
  :no-handle      'tree-widget-no-handle
  )

;;; Widget support functions
;;
(defun tree-widget-p (widget)
  "Return non-nil if WIDGET is a tree-widget."
  (let ((type (widget-type widget)))
    (while (and type (not (eq type 'tree-widget)))
      (setq type (widget-type (get type 'widget-type))))
    (eq type 'tree-widget)))

(defun tree-widget-node (widget)
  "Return WIDGET's :node child widget.
If not found, setup an `item' widget as default.
Signal an error if the :node widget is a tree-widget.
WIDGET is, or derives from, a tree-widget."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      ;; Setup an item widget as default :node.
      (setq node `(item :tag ,(or (widget-get widget :tag)
                                  (widget-princ-to-string
                                   (widget-value widget)))))
      (widget-put widget :node node))
    node))

(defun tree-widget-keep (arg widget)
  "Save in ARG the WIDGET's properties specified by :keep."
  (dolist (prop (widget-get widget :keep))
    (widget-put arg prop (widget-get widget prop))))

(defun tree-widget-children-value-save (widget &optional args node)
  "Save WIDGET children values.
WIDGET is, or derives from, a tree-widget.
Children properties and values are saved in ARGS if non-nil, else in
WIDGET's :args property value.  Properties and values of the
WIDGET's :node sub-widget are saved in NODE if non-nil, else in
WIDGET's :node sub-widget."
  (let ((args (cons (or node (widget-get widget :node))
                    (or args (widget-get widget :args))))
        (children (widget-get widget :children))
        arg child)
    (while (and args children)
      (setq arg      (car args)
            args     (cdr args)
            child    (car children)
            children (cdr children))
       (if (tree-widget-p child)
;;;; The child is a tree node.
           (progn
             ;; Backtrack :args and :node properties.
             (widget-put arg :args (widget-get child :args))
             (widget-put arg :node (widget-get child :node))
             ;; Save :open property.
             (widget-put arg :open (widget-get child :open))
             ;; The node is open.
             (when (widget-get child :open)
               ;; Save the widget value.
               (widget-put arg :value (widget-value child))
               ;; Save properties specified in :keep.
               (tree-widget-keep arg child)
               ;; Save children.
               (tree-widget-children-value-save
                child (widget-get arg :args) (widget-get arg :node))))
;;;; Another non tree node.
         ;; Save the widget value.
         (widget-put arg :value (widget-value child))
         ;; Save properties specified in :keep.
         (tree-widget-keep arg child)))))

;;; Widget creation
;;
(defvar tree-widget-before-create-icon-functions nil
  "Hooks run before to create a tree-widget icon.
Each function is passed the icon widget not yet created.
The value of the icon widget :node property is a tree :node widget or
a leaf node widget, not yet created.
This hook can be used to dynamically change properties of the icon and
associated node widgets.  For example, to dynamically change the look
and feel of the tree-widget by changing the values of the :tag
and :glyph-name properties of the icon widget.
This hook should be local in the buffer setup to display widgets.")

(defun tree-widget-icon-create (icon)
  "Create the ICON widget."
  (run-hook-with-args 'tree-widget-before-create-icon-functions icon)
  (widget-put icon :tag-glyph
              (tree-widget-find-image (widget-get icon :glyph-name)))
  ;; Ensure there is at least one char to display the image.
  (and (widget-get icon :tag-glyph)
       (equal "" (or (widget-get icon :tag) ""))
       (widget-put icon :tag " "))
  (widget-default-create icon)
  ;; Insert space between the icon and the node widget.
  (insert-char ?  1)
  (put-text-property
   (1- (point)) (point)
   'display (list 'space :width tree-widget-space-width)))

(defun tree-widget-convert-widget (widget)
  "Convert :args as widget types in WIDGET."
  (let ((tree (widget-types-convert-widget widget)))
    ;; Compatibility
    (widget-put tree :expander (or (widget-get tree :expander)
                                   (widget-get tree :dynargs)))
    tree))

(defvar widget-glyph-enable) ; XEmacs

(defun tree-widget-value-create (tree)
  "Create the TREE tree-widget."
  (let* ((node   (tree-widget-node tree))
         (flags  (widget-get tree :tree-widget--guide-flags))
         (indent (widget-get tree :indent))
         ;; Setup widget's image support.  Looking up for images, and
         ;; setting widgets' :tag-glyph is done here, to allow to
         ;; dynamically change the image theme.
         (widget-image-enable (tree-widget-use-image-p))     ; Emacs
         (widget-glyph-enable widget-image-enable)           ; XEmacs
         children buttons)
    (and indent (not (widget-get tree :parent))
         (insert-char ?\  indent))
    (if (widget-get tree :open)
;;;; Expanded node.
        (let ((args     (widget-get tree :args))
              (guide    (widget-get tree :guide))
              (noguide  (widget-get tree :no-guide))
              (endguide (widget-get tree :end-guide))
              (handle   (widget-get tree :handle))
              (nohandle (widget-get tree :no-handle))
              (guidi    (tree-widget-find-image "guide"))
              (noguidi  (tree-widget-find-image "no-guide"))
              (endguidi (tree-widget-find-image "end-guide"))
              (handli   (tree-widget-find-image "handle"))
              (nohandli (tree-widget-find-image "no-handle")))
          ;; Request children at run time, when requested.
          (when (and (widget-get tree :expander)
                     (widget-apply tree :expander-p))
            (setq args (mapcar 'widget-convert
                               (widget-apply tree :expander)))
            (widget-put tree :args args))
          ;; Defer the node widget creation after icon creation.
          (widget-put tree :node (widget-convert node))
          ;; Create the icon widget for the expanded tree.
          (push (widget-create-child-and-convert
                 tree (widget-get tree (if args :open-icon :empty-icon))
                 ;; Pass the node widget to child.
                 :node (widget-get tree :node))
                buttons)
          ;; Create the tree node widget.
          (push (widget-create-child tree (widget-get tree :node))
                children)
          ;; Update the icon :node with the created node widget.
          (widget-put (car buttons) :node (car children))
          ;; Create the tree children.
          (while args
            (setq node (car args)
                  args (cdr args))
            (and indent (insert-char ?\  indent))
            ;; Insert guide lines elements from previous levels.
            (dolist (f (reverse flags))
              (widget-create-child-and-convert
               tree (if f guide noguide)
               :tag-glyph (if f guidi noguidi))
              (widget-create-child-and-convert
               tree nohandle :tag-glyph nohandli))
            ;; Insert guide line element for this level.
            (widget-create-child-and-convert
             tree (if args guide endguide)
             :tag-glyph (if args guidi endguidi))
            ;; Insert the node handle line
            (widget-create-child-and-convert
             tree handle :tag-glyph handli)
            (if (tree-widget-p node)
                ;; Create a sub-tree node.
                (push (widget-create-child-and-convert
                       tree node :tree-widget--guide-flags
                       (cons (if args t) flags))
                      children)
              ;; Create the icon widget for a leaf node.
              (push (widget-create-child-and-convert
                     tree (widget-get tree :leaf-icon)
                     ;; At this point the node widget isn't yet created.
                     :node (setq node (widget-convert
                                       node :tree-widget--guide-flags
                                       (cons (if args t) flags)))
                     :tree-widget--leaf-flag t)
                    buttons)
              ;; Create the leaf node widget.
              (push (widget-create-child tree node) children)
              ;; Update the icon :node with the created node widget.
              (widget-put (car buttons) :node (car children)))))
;;;; Collapsed node.
      ;; Defer the node widget creation after icon creation.
      (widget-put tree :node (widget-convert node))
      ;; Create the icon widget for the collapsed tree.
      (push (widget-create-child-and-convert
             tree (widget-get tree :close-icon)
             ;; Pass the node widget to child.
             :node (widget-get tree :node))
            buttons)
      ;; Create the tree node widget.
      (push (widget-create-child tree (widget-get tree :node))
            children)
      ;; Update the icon :node with the created node widget.
      (widget-put (car buttons) :node (car children)))
    ;; Save widget children and buttons.  The tree-widget :node child
    ;; is the first element in :children.
    (widget-put tree :children (nreverse children))
    (widget-put tree :buttons  buttons)))

;;; Widget callbacks
;;
(defsubst tree-widget-leaf-node-icon-p (icon)
  "Return non-nil if ICON is a leaf node icon.
That is, if its :node property value is a leaf node widget."
  (widget-get icon :tree-widget--leaf-flag))

(defun tree-widget-icon-action (icon &optional event)
  "Handle the ICON widget :action.
If ICON :node is a leaf node it handles the :action.  The tree-widget
parent of ICON handles the :action otherwise.
Pass the received EVENT to :action."
  (let ((node (widget-get icon (if (tree-widget-leaf-node-icon-p icon)
                                   :node :parent))))
    (widget-apply node :action event)))

(defun tree-widget-icon-help-echo (icon)
  "Return the help-echo string of ICON.
If ICON :node is a leaf node it handles the :help-echo.  The tree-widget
parent of ICON handles the :help-echo otherwise."
  (let* ((node (widget-get icon (if (tree-widget-leaf-node-icon-p icon)
                                    :node :parent)))
         (help-echo (widget-get node :help-echo)))
    (if (functionp help-echo)
        (funcall help-echo node)
      help-echo)))

(defvar tree-widget-after-toggle-functions nil
  "Hooks run after toggling a tree-widget expansion.
Each function is passed a tree-widget.  If the value of the :open
property is non-nil the tree has been expanded, else collapsed.
This hook should be local in the buffer setup to display widgets.")

(defun tree-widget-action (tree &optional _event)
  "Handle the :action of the TREE tree-widget.
That is, toggle expansion of the TREE tree-widget.
Ignore the EVENT argument."
  (let ((open (not (widget-get tree :open))))
    (or open
        ;; Before to collapse the node, save children values so next
        ;; open can recover them.
        (tree-widget-children-value-save tree))
    (widget-put tree :open open)
    (widget-value-set tree open)
    (run-hook-with-args 'tree-widget-after-toggle-functions tree)))

(defun tree-widget-help-echo (tree)
  "Return the help-echo string of the TREE tree-widget."
  (if (widget-get tree :open)
      "Collapse node"
    "Expand node"))

(defun tree-widget-expander-p (tree)
  "Return non-nil if the TREE tree-widget :expander has to be called.
That is, if TREE :args is nil."
  (null (widget-get tree :args)))

(provide 'tree-widget)

;;; tree-widget.el ends here
