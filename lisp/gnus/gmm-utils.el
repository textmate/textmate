;;; gmm-utils.el --- Utility functions for Gnus, Message and MML

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Reiner Steib <reiner.steib@gmx.de>
;; Keywords: news

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

;; This library provides self-contained utility functions.  The functions are
;; used in Gnus, Message and MML, but within this library there are no
;; dependencies on Gnus, Message, or MML.

;;; Code:

(defgroup gmm nil
  "Utility functions for Gnus, Message and MML."
  :prefix "gmm-"
  :version "22.1" ;; Gnus 5.10.9
  :group 'lisp)

;; Helper functions from `gnus-utils.el': gmm-verbose, gmm-message, gmm-error

(defcustom gmm-verbose 7
  "Integer that says how verbose gmm should be.
The higher the number, the more messages will flash to say what
it did.  At zero, it will be totally mute; at five, it will
display most important messages; and at ten, it will keep on
jabbering all the time."
  :type 'integer
  :group 'gmm)

;;;###autoload
(defun gmm-regexp-concat (regexp)
  "Potentially concat a list of regexps into a single one.
The concatenation is done with logical ORs."
  (cond ((null regexp)
	 nil)
	((stringp regexp)
	 regexp)
	((listp regexp)
	 (mapconcat (lambda (elt) (concat "\\(" elt "\\)"))
		    regexp
		    "\\|"))))

;;;###autoload
(defun gmm-message (level &rest args)
  "If LEVEL is lower than `gmm-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages
3 - non-serious error messages
5 - messages for things that take a long time
7 - not very important messages on stuff
9 - messages inside loops."
  (if (<= level gmm-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

;;;###autoload
(defun gmm-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gmm-verbose'.
ARGS are passed to `message'."
  (when (<= (floor level) gmm-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

;;;###autoload
(defun gmm-widget-p (symbol)
  "Non-nil if SYMBOL is a widget."
  (get symbol 'widget-type))

(autoload 'widget-create-child-value "wid-edit")
(autoload 'widget-convert "wid-edit")
(autoload 'widget-default-get "wid-edit")

;; Copy of the `nnmail-lazy' code from `nnmail.el':
(define-widget 'gmm-lazy 'default
  "Base widget for recursive datastructures.

This is a copy of the `lazy' widget in Emacs 22.1 provided for compatibility."
  :format "%{%t%}: %v"
  :convert-widget 'widget-value-convert-widget
  :value-create (lambda (widget)
                  (let ((value (widget-get widget :value))
                        (type (widget-get widget :type)))
                    (widget-put widget :children
                                (list (widget-create-child-value
                                       widget (widget-convert type) value)))))
  :value-delete 'widget-children-value-delete
  :value-get (lambda (widget)
               (widget-value (car (widget-get widget :children))))
  :value-inline (lambda (widget)
                  (widget-apply (car (widget-get widget :children))
                                :value-inline))
  :default-get (lambda (widget)
                 (widget-default-get
                  (widget-convert (widget-get widget :type))))
  :match (lambda (widget value)
           (widget-apply (widget-convert (widget-get widget :type))
                         :match value))
  :validate (lambda (widget)
              (widget-apply (car (widget-get widget :children)) :validate)))

;; Note: The format of `gmm-tool-bar-item' may change if some future Emacs
;; version will provide customizable tool bar buttons using a different
;; interface.

;; TODO: Extend API so that the "Command" entry can be a function or a plist.
;; In case of a list it should have the format...
;;
;;  (:none command-without-modifier
;;   :shift command-with-shift-pressed
;;   :control command-with-ctrl-pressed
;;   :control-shift command-with-control-and-shift-pressed
;;   ;; mouse-2 and mouse-3 can't be used in Emacs yet.
;;   :mouse-2 command-on-mouse-2-press
;;   :mouse-3 command-on-mouse-3-press) ;; typically a menu of related commands
;;
;; Combinations of mouse-[23] plus shift and/or control might be overkill.
;;
;; Then use (plist-get rs-command :none), (plist-get rs-command :shift)

(define-widget 'gmm-tool-bar-item (if (gmm-widget-p 'lazy) 'lazy 'gmm-lazy)
  "Tool bar list item."
  :tag "Tool bar item"
  :type '(choice
	  (list :tag "Command and Icon"
		(function :tag "Command")
		(string :tag "Icon file")
		(choice
		 (const :tag "Default map" nil)
		 ;; Note: Usually we need non-nil attributes if map is t.
		 (const :tag "No menu" t)
		 (sexp :tag "Other map"))
		(plist :inline t :tag "Properties"))
	  (list :tag "Separator"
		(const :tag "No command" gmm-ignore)
		(string :tag "Icon file")
		(const :tag "No map")
		(plist :inline t :tag "Properties"))))

(define-widget 'gmm-tool-bar-zap-list (if (gmm-widget-p 'lazy) 'lazy 'gmm-lazy)
  "Tool bar zap list."
  :tag "Tool bar zap list"
  :type '(choice (const :tag "Zap all" t)
		 (const :tag "Keep all" nil)
		 (list
		  ;; :value
		  ;; Work around (bug in customize?), see
		  ;; <news:v9is48jrj1.fsf@marauder.physik.uni-ulm.de>
		  ;; (new-file open-file dired kill-buffer write-file
		  ;; 	    print-buffer customize help)
		  (set :inline t
		       (const new-file)
		       (const open-file)
		       (const dired)
		       (const kill-buffer)
		       (const save-buffer)
		       (const write-file)
		       (const undo)
		       (const cut)
		       (const copy)
		       (const paste)
		       (const search-forward)
		       (const print-buffer)
		       (const customize)
		       (const help))
		  (repeat :inline t
			  :tag "Other"
			  (symbol :tag "Icon item")))))

;; (defun gmm-color-cells (&optional display)
;;   "Return the number of color cells supported by DISPLAY.
;; Compatibility function."
;;   ;; `display-color-cells' doesn't return more than 256 even if color depth is
;;   ;; > 8 in Emacs 21.
;;   ;;
;;   ;; Feel free to add proper XEmacs support.
;;   (let* ((cells (and (fboundp 'display-color-cells)
;; 		     (display-color-cells display)))
;; 	 (plane (and (fboundp 'x-display-planes)
;; 		     (ash 1 (x-display-planes))))
;; 	 (none -1))
;;     (max (if (integerp cells) cells none)
;; 	 (if (integerp plane) plane none))))

(defcustom gmm-tool-bar-style
  (if (and (boundp 'tool-bar-mode)
	   tool-bar-mode
	   (and (fboundp 'display-visual-class)
		(not (memq (display-visual-class)
			   (list 'static-gray 'gray-scale
				 'static-color 'pseudo-color)))))
      'gnome
    'retro)
  "Preferred tool bar style."
  :type '(choice (const :tag "GNOME style" gnome)
		 (const :tag "Retro look"  retro))
  :group 'gmm)

(defvar tool-bar-map)

;;;###autoload
(defun gmm-tool-bar-from-list (icon-list zap-list default-map)
  "Make a tool bar from ICON-LIST.

Within each entry of ICON-LIST, the first element is a menu
command, the second element is an icon file name and the third
element is a test function.  You can use \\[describe-key]
<menu-entry> to find out the name of a menu command.  The fourth
and all following elements are passed as the PROPS argument to the
function `tool-bar-local-item'.

If ZAP-LIST is a list, remove those item from the default
`tool-bar-map'.  If it is t, start with a new sparse map.  You
can use \\[describe-key] <icon> to find out the name of an icon
item.  When \\[describe-key] <icon> shows \"<tool-bar> <new-file>
runs the command find-file\", then use `new-file' in ZAP-LIST.

DEFAULT-MAP specifies the default key map for ICON-LIST."
  (let (;; For Emacs 21, we must let-bind `tool-bar-map'.  In Emacs 22, we
	;; could use some other local variable.
	(tool-bar-map (if (eq zap-list t)
			  (make-sparse-keymap)
			(copy-keymap tool-bar-map))))
    (when (listp zap-list)
      ;; Zap some items which aren't relevant for this mode and take up space.
      (dolist (key zap-list)
	(define-key tool-bar-map (vector key) nil)))
    (mapc (lambda (el)
	    (let ((command (car el))
		  (icon (nth 1 el))
		  (fmap (or (nth 2 el) default-map))
		  (props  (cdr (cdr (cdr el)))) )
	      ;; command may stem from different from-maps:
	      (cond ((eq command 'gmm-ignore)
		     ;; The dummy `gmm-ignore', see `gmm-tool-bar-item'
		     ;; widget.  Suppress tooltip by adding `:enable nil'.
		     (if (fboundp 'tool-bar-local-item)
			 (apply 'tool-bar-local-item icon nil nil
				tool-bar-map :enable nil props)
		       ;; (tool-bar-local-item ICON DEF KEY MAP &rest PROPS)
		       ;; (tool-bar-add-item ICON DEF KEY &rest PROPS)
		       (apply 'tool-bar-add-item icon nil nil :enable nil props)))
		    ((equal fmap t) ;; Not a menu command
		     (apply 'tool-bar-local-item
			    icon command
			    (intern icon) ;; reuse icon or fmap here?
			    tool-bar-map props))
		    (t ;; A menu command
		     (apply 'tool-bar-local-item-from-menu
			    ;; (apply 'tool-bar-local-item icon def key
			    ;; tool-bar-map props)
			    command icon tool-bar-map (symbol-value fmap)
			    props)))
	      t))
	  (if (symbolp icon-list)
	      (eval icon-list)
	    icon-list))
    tool-bar-map))

(defmacro defun-gmm (name function arg-list &rest body)
  "Create function NAME.
If FUNCTION exists, then NAME becomes an alias for FUNCTION.
Otherwise, create function NAME with ARG-LIST and BODY."
  (let ((defined-p (fboundp function)))
    (if defined-p
        `(defalias ',name ',function)
      `(defun ,name ,arg-list ,@body))))

(defun-gmm gmm-image-search-load-path
  image-search-load-path (file &optional path)
  "Emacs 21 and XEmacs don't have `image-search-load-path'.
This function returns nil on those systems."
  nil)

;; Cf. `mh-image-load-path-for-library' in `mh-compat.el'.

(defun-gmm gmm-image-load-path-for-library
  image-load-path-for-library (library image &optional path no-error)
  "Return a suitable search path for images used by LIBRARY.

It searches for IMAGE in `image-load-path' (excluding
\"`data-directory'/images\") and `load-path', followed by a path
suitable for LIBRARY, which includes \"../../etc/images\" and
\"../etc/images\" relative to the library file itself, and then
in \"`data-directory'/images\".

Then this function returns a list of directories which contains
first the directory in which IMAGE was found, followed by the
value of `load-path'.  If PATH is given, it is used instead of
`load-path'.

If NO-ERROR is non-nil and a suitable path can't be found, don't
signal an error.  Instead, return a list of directories as before,
except that nil appears in place of the image directory.

Here is an example that uses a common idiom to provide
compatibility with versions of Emacs that lack the variable
`image-load-path':

    ;; Shush compiler.
    (defvar image-load-path)

    (let* ((load-path (image-load-path-for-library \"mh-e\" \"mh-logo.xpm\"))
           (image-load-path (cons (car load-path)
                                  (when (boundp 'image-load-path)
                                    image-load-path))))
      (mh-tool-bar-folder-buttons-init))"
  (unless library (error "No library specified"))
  (unless image   (error "No image specified"))
  (let (image-directory image-directory-load-path)
    ;; Check for images in image-load-path or load-path.
    (let ((img image)
          (dir (or
                ;; Images in image-load-path.
                (gmm-image-search-load-path image) ;; "gmm-" prefix!
                ;; Images in load-path.
                (locate-library image)))
          parent)
      ;; Since the image might be in a nested directory (for
      ;; example, mail/attach.pbm), adjust `image-directory'
      ;; accordingly.
      (when dir
        (setq dir (file-name-directory dir))
        (while (setq parent (file-name-directory img))
          (setq img (directory-file-name parent)
                dir (expand-file-name "../" dir))))
      (setq image-directory-load-path dir))

    ;; If `image-directory-load-path' isn't Emacs's image directory,
    ;; it's probably a user preference, so use it.  Then use a
    ;; relative setting if possible; otherwise, use
    ;; `image-directory-load-path'.
    (cond
     ;; User-modified image-load-path?
     ((and image-directory-load-path
           (not (equal image-directory-load-path
                       (file-name-as-directory
                        (expand-file-name "images" data-directory)))))
      (setq image-directory image-directory-load-path))
     ;; Try relative setting.
     ((let (library-name d1ei d2ei)
        ;; First, find library in the load-path.
        (setq library-name (locate-library library))
        (if (not library-name)
            (error "Cannot find library %s in load-path" library))
        ;; And then set image-directory relative to that.
        (setq
         ;; Go down 2 levels.
         d2ei (file-name-as-directory
               (expand-file-name
                (concat (file-name-directory library-name) "../../etc/images")))
         ;; Go down 1 level.
         d1ei (file-name-as-directory
               (expand-file-name
                (concat (file-name-directory library-name) "../etc/images"))))
        (setq image-directory
              ;; Set it to nil if image is not found.
              (cond ((file-exists-p (expand-file-name image d2ei)) d2ei)
                    ((file-exists-p (expand-file-name image d1ei)) d1ei)))))
     ;; Use Emacs's image directory.
     (image-directory-load-path
      (setq image-directory image-directory-load-path))
     (no-error
      (message "Could not find image %s for library %s" image library))
     (t
      (error "Could not find image %s for library %s" image library)))

    ;; Return an augmented `path' or `load-path'.
    (nconc (list image-directory)
           (delete image-directory (copy-sequence (or path load-path))))))

(defun gmm-customize-mode (&optional mode)
  "Customize customization group for MODE.
If mode is nil, use `major-mode' of the current buffer."
  (interactive)
  (customize-group
   (or mode
       (intern (let ((mode (symbol-name major-mode)))
		 (string-match "^\\(.+\\)-mode$" mode)
		 (match-string 1 mode))))))

(defun gmm-write-region (start end filename &optional append visit
			       lockname mustbenew)
  "Compatibility function for `write-region'.

In XEmacs, the seventh argument of `write-region' specifies the
coding-system."
  (if (and mustbenew (featurep 'xemacs))
      (if (file-exists-p filename)
	  (signal 'file-already-exists (list "File exists" filename))
	(write-region start end filename append visit lockname))
    (write-region start end filename append visit lockname mustbenew)))

(provide 'gmm-utils)

;;; gmm-utils.el ends here
