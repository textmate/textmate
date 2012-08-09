;;; facemenu.el --- create a face menu for interactively adding fonts to text

;; Copyright (C) 1994-1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Keywords: faces
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

;; This file defines a menu of faces (bold, italic, etc) which allows you to
;; set the face used for a region of the buffer.  Some faces also have
;; keybindings, which are shown in the menu.
;;
;; The menu also contains submenus for indentation and justification-changing
;; commands.

;;; Usage:
;; Selecting a face from the menu or typing the keyboard equivalent will
;; change the region to use that face.  If you use transient-mark-mode and the
;; region is not active, the face will be remembered and used for the next
;; insertion.  It will be forgotten if you move point or make other
;; modifications before inserting or typing anything.
;;
;; Faces can be selected from the keyboard as well.
;; The standard keybindings are M-o (or ESC o) + letter:
;; M-o i = "set italic",  M-o b = "set bold", etc.

;;; Customization:
;; An alternative set of keybindings that may be easier to type can be set up
;; using "Alt" or "Hyper" keys.  This requires that you either have or create
;; an Alt or Hyper key on your keyboard.  On my keyboard, there is a key
;; labeled "Alt", but to make it act as an Alt key I have to put this command
;; into my .xinitrc:
;;    xmodmap -e "add Mod3 = Alt_L"
;; Or, I can make it into a Hyper key with this:
;;    xmodmap -e "keysym Alt_L = Hyper_L" -e "add Mod2 = Hyper_L"
;; Check with local X-perts for how to do it on your system.
;; Then you can define your keybindings with code like this in your .emacs:
;;   (setq facemenu-keybindings
;;    '((default     . [?\H-d])
;;      (bold        . [?\H-b])
;;      (italic      . [?\H-i])
;;      (bold-italic . [?\H-l])
;;      (underline   . [?\H-u])))
;;   (facemenu-update)
;;   (setq facemenu-keymap global-map)
;;   (define-key global-map [?\H-c] 'facemenu-set-foreground) ; set fg color
;;   (define-key global-map [?\H-C] 'facemenu-set-background) ; set bg color
;;
;; The order of the faces that appear in the menu and their keybindings can be
;; controlled by setting the variables `facemenu-keybindings' and
;; `facemenu-new-faces-at-end'.  List faces that you want to use in documents
;; in `facemenu-listed-faces'.

;;; Known Problems:
;; Bold and Italic do not combine to create bold-italic if you select them
;; both, although most other combinations (eg bold + underline + some color)
;; do the intuitive thing.
;;
;; There is at present no way to display what the faces look like in
;; the menu itself.
;;
;; `list-faces-display' shows the faces in a different order than
;; this menu, which could be confusing.  I do /not/ sort the list
;; alphabetically, because I like the default order: it puts the most
;; basic, common fonts first.
;;
;; Please send me any other problems, comments or ideas.

;;; Code:

(eval-when-compile
  (require 'help)
  (require 'button))

;; Global bindings:
(define-key global-map [C-down-mouse-2] 'facemenu-menu)
(define-key global-map "\M-o" 'facemenu-keymap)

(defgroup facemenu nil
  "Create a face menu for interactively adding fonts to text."
  :group 'faces
  :prefix "facemenu-")

(defcustom facemenu-keybindings
  (mapcar 'purecopy
  '((default     . "d")
    (bold        . "b")
    (italic      . "i")
    (bold-italic . "l") ; {bold} intersect {italic} = {l}
    (underline   . "u")))
  "Alist of interesting faces and keybindings.
Each element is itself a list: the car is the name of the face,
the next element is the key to use as a keyboard equivalent of the menu item;
the binding is made in `facemenu-keymap'.

The faces specifically mentioned in this list are put at the top of
the menu, in the order specified.  All other faces which are defined
in `facemenu-listed-faces' are listed after them, but get no
keyboard equivalents.

If you change this variable after loading facemenu.el, you will need to call
`facemenu-update' to make it take effect."
  :type '(repeat (cons face string))
  :group 'facemenu)

(defcustom facemenu-new-faces-at-end t
  "Where in the menu to insert newly-created faces.
This should be nil to put them at the top of the menu, or t to put them
just before \"Other\" at the end."
  :type 'boolean
  :group 'facemenu)

(defvar facemenu-unlisted-faces
  `(modeline region secondary-selection highlight scratch-face
    ,(purecopy "^font-lock-") ,(purecopy "^gnus-") ,(purecopy "^message-")
    ,(purecopy "^ediff-") ,(purecopy "^term-") ,(purecopy "^vc-")
    ,(purecopy "^widget-") ,(purecopy "^custom-") ,(purecopy "^vm-"))
  "*List of faces that are of no interest to the user.")
(make-obsolete-variable 'facemenu-unlisted-faces 'facemenu-listed-faces
			"22.1,\n  and has no effect on the Face menu")

(defcustom facemenu-listed-faces nil
  "List of faces to include in the Face menu.
Each element should be a symbol, the name of a face.
The \"basic \" faces in `facemenu-keybindings' are automatically
added to the Face menu, and need not be in this list.

This value takes effect when you load facemenu.el.  If the
list includes symbols which are not defined as faces, they
are ignored; however, subsequently defining or creating
those faces adds them to the menu then.  You can call
`facemenu-update' to recalculate the menu contents, such as
if you change the value of this variable,

If this variable is t, all faces that you apply to text
using the face menu commands (even by name), and all faces
that you define or create, are added to the menu.  You may
find it useful to set this variable to t temporarily while
you define some faces, so that they will be added.  However,
if the value is no longer t and you call `facemenu-update',
it will remove any faces not explicitly in the list."
  :type '(choice (const :tag "List all faces" t)
		 (const :tag "None" nil)
		 (repeat symbol))
  :group 'facemenu
  :version "22.1")

(defvar facemenu-face-menu
  (let ((map (make-sparse-keymap "Face")))
    (define-key map "o" (cons (purecopy "Other...") 'facemenu-set-face))
    map)
  "Menu keymap for faces.")
(defalias 'facemenu-face-menu facemenu-face-menu)
(put 'facemenu-face-menu 'menu-enable '(facemenu-enable-faces-p))

(defvar facemenu-foreground-menu
  (let ((map (make-sparse-keymap "Foreground Color")))
    (define-key map "o" (cons (purecopy "Other...") 'facemenu-set-foreground))
    map)
  "Menu keymap for foreground colors.")
(defalias 'facemenu-foreground-menu facemenu-foreground-menu)
(put 'facemenu-foreground-menu 'menu-enable '(facemenu-enable-faces-p))

(defvar facemenu-background-menu
  (let ((map (make-sparse-keymap "Background Color")))
    (define-key map "o" (cons (purecopy "Other...") 'facemenu-set-background))
    map)
  "Menu keymap for background colors.")
(defalias 'facemenu-background-menu facemenu-background-menu)
(put 'facemenu-background-menu 'menu-enable '(facemenu-enable-faces-p))

;;; Condition for enabling menu items that set faces.
(defun facemenu-enable-faces-p ()
  ;; Enable the facemenu if facemenu-add-face-function is defined
  ;; (e.g. in Tex-mode and SGML mode), or if font-lock is off.
  (or (not (and font-lock-mode font-lock-defaults))
      facemenu-add-face-function))

(defvar facemenu-special-menu
  (let ((map (make-sparse-keymap "Special")))
    (define-key map [?s] (cons (purecopy "Remove Special")
			       'facemenu-remove-special))
    (define-key map [?t] (cons (purecopy "Intangible")
			       'facemenu-set-intangible))
    (define-key map [?v] (cons (purecopy "Invisible")
			       'facemenu-set-invisible))
    (define-key map [?r] (cons (purecopy "Read-Only")
			       'facemenu-set-read-only))
    map)
  "Menu keymap for non-face text-properties.")
(defalias 'facemenu-special-menu facemenu-special-menu)

(defvar facemenu-justification-menu
  (let ((map (make-sparse-keymap "Justification")))
    (define-key map [?c] (cons (purecopy "Center") 'set-justification-center))
    (define-key map [?b] (cons (purecopy "Full") 'set-justification-full))
    (define-key map [?r] (cons (purecopy "Right") 'set-justification-right))
    (define-key map [?l] (cons (purecopy "Left") 'set-justification-left))
    (define-key map [?u] (cons (purecopy "Unfilled") 'set-justification-none))
    map)
  "Submenu for text justification commands.")
(defalias 'facemenu-justification-menu facemenu-justification-menu)

(defvar facemenu-indentation-menu
  (let ((map (make-sparse-keymap "Indentation")))
    (define-key map [decrease-right-margin]
      (cons (purecopy "Indent Right Less") 'decrease-right-margin))
    (define-key map [increase-right-margin]
      (cons (purecopy "Indent Right More") 'increase-right-margin))
    (define-key map [decrease-left-margin]
      (cons (purecopy "Indent Less") 'decrease-left-margin))
    (define-key map [increase-left-margin]
      (cons (purecopy "Indent More") 'increase-left-margin))
    map)
  "Submenu for indentation commands.")
(defalias 'facemenu-indentation-menu facemenu-indentation-menu)

;; This is split up to avoid an overlong line in loaddefs.el.
(defvar facemenu-menu nil
  "Facemenu top-level menu keymap.")
(setq facemenu-menu (make-sparse-keymap "Text Properties"))
(let ((map facemenu-menu))
  (define-key map [dc] (cons (purecopy "Display Colors") 'list-colors-display))
  (define-key map [df] (cons (purecopy "Display Faces") 'list-faces-display))
  (define-key map [dp] (cons (purecopy "Describe Properties")
			     'describe-text-properties))
  (define-key map [ra] (list 'menu-item (purecopy "Remove Text Properties")
			     'facemenu-remove-all
			     :enable 'mark-active))
  (define-key map [rm] (list 'menu-item (purecopy "Remove Face Properties")
			     'facemenu-remove-face-props
			     :enable 'mark-active))
  (define-key map [s1] (list (purecopy "--"))))
(let ((map facemenu-menu))
  (define-key map [in] (cons (purecopy "Indentation")
			     'facemenu-indentation-menu))
  (define-key map [ju] (cons (purecopy "Justification")
			     'facemenu-justification-menu))
  (define-key map [s2] (list (purecopy "--")))
  (define-key map [sp] (cons (purecopy "Special Properties")
			     'facemenu-special-menu))
  (define-key map [bg] (cons (purecopy "Background Color")
			     'facemenu-background-menu))
  (define-key map [fg] (cons (purecopy "Foreground Color")
			     'facemenu-foreground-menu))
  (define-key map [fc] (cons (purecopy "Face")
			     'facemenu-face-menu)))
(defalias 'facemenu-menu facemenu-menu)

(defvar facemenu-keymap
  (let ((map (make-sparse-keymap "Set face")))
    (define-key map "o" (cons (purecopy "Other...") 'facemenu-set-face))
    (define-key map "\M-o" 'font-lock-fontify-block)
    map)
  "Keymap for face-changing commands.
`Facemenu-update' fills in the keymap according to the bindings
requested in `facemenu-keybindings'.")
(defalias 'facemenu-keymap facemenu-keymap)


(defcustom facemenu-add-face-function nil
  "Function called at beginning of text to change or nil.
This function is passed the FACE to set and END of text to change, and must
return a string which is inserted.  It may set `facemenu-end-add-face'."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'facemenu)

(defcustom facemenu-end-add-face nil
  "String to insert or function called at end of text to change or nil.
This function is passed the FACE to set, and must return a string which is
inserted."
  :type '(choice (const :tag "None" nil)
		 string
		 function)
  :group 'facemenu)

(defcustom facemenu-remove-face-function nil
  "When non-nil, this is a function called to remove faces.
This function is passed the START and END of text to change.
May also be t meaning to use `facemenu-add-face-function'."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Use add-face" t)
		 function)
  :group 'facemenu)

;;; Internal Variables

(defvar facemenu-color-alist nil
  "Alist of colors, used for completion.
If this is nil, then the value of (defined-colors) is used.")

(defun facemenu-update ()
  "Add or update the \"Face\" menu in the menu bar.
You can call this to update things if you change any of the menu configuration
variables."
  (interactive)

  ;; Add each defined face to the menu.
  (facemenu-iterate 'facemenu-add-new-face
		    (facemenu-complete-face-list facemenu-keybindings)))

(defun facemenu-set-face (face &optional start end)
  "Apply FACE to the region or next character typed.

If the region is active (normally true except in Transient
Mark mode) and nonempty, and there is no prefix argument,
this command applies FACE to the region.  Otherwise, it applies FACE
to the faces to use for the next character
inserted.  (Moving point or switching buffers before typing
a character to insert cancels the specification.)

If FACE is `default', to \"apply\" it means clearing
the list of faces to be used.  For any other value of FACE,
to \"apply\" it means putting FACE at the front of the list
of faces to be used, and removing any faces further
along in the list that would be completely overridden by
preceding faces (including FACE).

This command can also add FACE to the menu of faces,
if `facemenu-listed-faces' says to do that."
  (interactive (list (progn
		       (barf-if-buffer-read-only)
		       (read-face-name "Use face"))
		     (if (and mark-active (not current-prefix-arg))
			 (region-beginning))
		     (if (and mark-active (not current-prefix-arg))
			 (region-end))))
  (facemenu-add-new-face face)
  (facemenu-add-face face start end))

(defun facemenu-set-foreground (color &optional start end)
  "Set the foreground COLOR of the region or next character typed.
This command reads the color in the minibuffer.

If the region is active (normally true except in Transient Mark mode)
and there is no prefix argument, this command sets the region to the
requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification."
  (interactive (list (progn
		       (barf-if-buffer-read-only)
		       (read-color "Foreground color: "))
		     (if (and mark-active (not current-prefix-arg))
			 (region-beginning))
		     (if (and mark-active (not current-prefix-arg))
			 (region-end))))
  (facemenu-set-face-from-menu
   (facemenu-add-new-color color 'facemenu-foreground-menu)
   start end))

(defun facemenu-set-background (color &optional start end)
  "Set the background COLOR of the region or next character typed.
This command reads the color in the minibuffer.

If the region is active (normally true except in Transient Mark mode)
and there is no prefix argument, this command sets the region to the
requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification."
  (interactive (list (progn
		       (barf-if-buffer-read-only)
		       (read-color "Background color: "))
		     (if (and mark-active (not current-prefix-arg))
			 (region-beginning))
		     (if (and mark-active (not current-prefix-arg))
			 (region-end))))
  (facemenu-set-face-from-menu
   (facemenu-add-new-color color 'facemenu-background-menu)
   start end))

(defun facemenu-set-face-from-menu (face start end)
  "Set the FACE of the region or next character typed.
This function is designed to be called from a menu; FACE is determined
using the event type of the menu entry.  If FACE is a symbol whose
name starts with \"fg:\" or \"bg:\", then this functions sets the
foreground or background to the color specified by the rest of the
symbol's name.  Any other symbol is considered the name of a face.

If the region is active (normally true except in Transient Mark mode)
and there is no prefix argument, this command sets the region to the
requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before typing a character
to insert cancels the specification."
  (interactive (list last-command-event
		     (if (and mark-active (not current-prefix-arg))
			 (region-beginning))
		     (if (and mark-active (not current-prefix-arg))
			 (region-end))))
  (barf-if-buffer-read-only)
  (facemenu-add-face
   (let ((fn (symbol-name face)))
     (if (string-match "\\`\\([fb]\\)g:\\(.+\\)" fn)
	 (list (list (if (string= (match-string 1 fn) "f")
			 :foreground
		       :background)
		     (match-string 2 fn)))
       face))
   start end))

(defun facemenu-set-invisible (start end)
  "Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "r")
  (add-text-properties start end '(invisible t)))

(defun facemenu-set-intangible (start end)
  "Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "r")
  (add-text-properties start end '(intangible t)))

(defun facemenu-set-read-only (start end)
  "Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "r")
  (add-text-properties start end '(read-only t)))

(defun facemenu-remove-face-props (start end)
  "Remove `face' and `mouse-face' text properties."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties
     start end '(face nil mouse-face nil))))

(defun facemenu-remove-all (start end)
  "Remove all text properties from the region."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (set-text-properties start end nil)))

(defun facemenu-remove-special (start end)
  "Remove all the \"special\" text properties from the region.
These special properties include `invisible', `intangible' and `read-only'."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties
     start end '(invisible nil intangible nil read-only nil))))

(defalias 'facemenu-read-color 'read-color)

(defcustom list-colors-sort nil
  "Color sort order for `list-colors-display'.
`nil' means default implementation-dependent order (defined in `x-colors').
`name' sorts by color name.
`rgb' sorts by red, green, blue components.
`(rgb-dist . COLOR)' sorts by the RGB distance to the specified color.
`hsv' sorts by hue, saturation, value.
`(hsv-dist . COLOR)' sorts by the HSV distance to the specified color
and excludes grayscale colors."
  :type '(choice (const :tag "Unsorted" nil)
		 (const :tag "Color Name" name)
		 (const :tag "Red-Green-Blue" rgb)
		 (cons :tag "Distance on RGB cube"
		       (const :tag "Distance from Color" rgb-dist)
		       (color :tag "Source Color Name"))
		 (const :tag "Hue-Saturation-Value" hsv)
		 (cons :tag "Distance on HSV cylinder"
		       (const :tag "Distance from Color" hsv-dist)
		       (color :tag "Source Color Name")))
  :group 'facemenu
  :version "24.1")

(defun list-colors-sort-key (color)
  "Return a list of keys for sorting colors depending on `list-colors-sort'.
COLOR is the name of the color.  When return value is nil,
filter out the color from the output."
  (require 'color)
  (cond
   ((null list-colors-sort) color)
   ((eq list-colors-sort 'name)
    (downcase color))
   ((eq list-colors-sort 'rgb)
    (color-values color))
   ((eq (car-safe list-colors-sort) 'rgb-dist)
    (color-distance color (cdr list-colors-sort)))
   ((eq list-colors-sort 'hsv)
    (apply 'color-rgb-to-hsv (color-name-to-rgb color)))
   ((eq (car-safe list-colors-sort) 'hsv-dist)
    (let* ((c-rgb (color-name-to-rgb color))
	   (c-hsv (apply 'color-rgb-to-hsv c-rgb))
	   (o-hsv (apply 'color-rgb-to-hsv
			 (color-name-to-rgb (cdr list-colors-sort)))))
      (unless (and (eq (nth 0 c-rgb) (nth 1 c-rgb)) ; exclude grayscale
		   (eq (nth 1 c-rgb) (nth 2 c-rgb)))
	;; 3D Euclidean distance (sqrt is not needed for sorting)
	(+ (expt (- 180 (abs (- 180 (abs (- (nth 0 c-hsv) ; wrap hue
					    (nth 0 o-hsv)))))) 2)
	   (expt (- (nth 1 c-hsv) (nth 1 o-hsv)) 2)
	   (expt (- (nth 2 c-hsv) (nth 2 o-hsv)) 2)))))))

(defun list-colors-display (&optional list buffer-name callback)
  "Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list of
colors that the current display can handle.  Customize
`list-colors-sort' to change the order in which colors are shown.

If the optional argument BUFFER-NAME is nil, it defaults to *Colors*.

If the optional argument CALLBACK is non-nil, it should be a
function to call each time the user types RET or clicks on a
color.  The function should accept a single argument, the color name."
  (interactive)
  (when (and (null list) (> (display-color-cells) 0))
    (setq list (list-colors-duplicates (defined-colors)))
    (when list-colors-sort
      ;; Schwartzian transform with `(color key1 key2 key3 ...)'.
      (setq list (mapcar
		  'car
		  (sort (delq nil (mapcar
				   (lambda (c)
				     (let ((key (list-colors-sort-key
						 (car c))))
				       (when key
					 (cons c (if (consp key) key
						   (list key))))))
				   list))
			(lambda (a b)
			  (let* ((a-keys (cdr a))
				 (b-keys (cdr b))
				 (a-key (car a-keys))
				 (b-key (car b-keys)))
			    ;; Skip common keys at the beginning of key lists.
			    (while (and a-key b-key (equal a-key b-key))
			      (setq a-keys (cdr a-keys) a-key (car a-keys)
				    b-keys (cdr b-keys) b-key (car b-keys)))
			    (cond
			     ((and (numberp a-key) (numberp b-key))
			      (< a-key b-key))
			     ((and (stringp a-key) (stringp b-key))
			      (string< a-key b-key)))))))))
    (when (memq (display-visual-class) '(gray-scale pseudo-color direct-color))
      ;; Don't show more than what the display can handle.
      (let ((lc (nthcdr (1- (display-color-cells)) list)))
	(if lc
	    (setcdr lc nil)))))
  (unless buffer-name
    (setq buffer-name "*Colors*"))
  (with-help-window buffer-name
    (with-current-buffer standard-output
      (erase-buffer)
      (list-colors-print list callback)
      (set-buffer-modified-p nil)
      (setq truncate-lines t)))
  (when callback
    (pop-to-buffer buffer-name)
    (message "Click on a color to select it.")))

(defun list-colors-print (list &optional callback)
  (let ((callback-fn
	 (if callback
	     `(lambda (button)
		(funcall ,callback (button-get button 'color-name))))))
    (dolist (color list)
      (if (consp color)
	  (if (cdr color)
	      (setq color (sort color (lambda (a b)
					(string< (downcase a)
						 (downcase b))))))
	(setq color (list color)))
      (let* ((opoint (point))
	     (color-values (color-values (car color)))
	     (light-p (>= (apply 'max color-values)
			  (* (car (color-values "white")) .5))))
	(insert (car color))
	(indent-to 22)
	(put-text-property opoint (point) 'face `(:background ,(car color)))
	(put-text-property
	 (prog1 (point)
	   (insert " ")
	   ;; Insert all color names.
	   (insert (mapconcat 'identity color ",")))
	 (point)
	 'face (list :foreground (car color)))
	(insert (propertize " " 'display '(space :align-to (- right 9))))
	(insert " ")
	(insert (propertize
		 (apply 'format "#%02x%02x%02x"
			(mapcar (lambda (c) (lsh c -8))
				color-values))
		 'mouse-face 'highlight
		 'help-echo
		 (let ((hsv (apply 'color-rgb-to-hsv
				   (color-name-to-rgb (car color)))))
		   (format "H:%d S:%d V:%d"
			   (nth 0 hsv) (nth 1 hsv) (nth 2 hsv)))))
	(when callback
	  (make-text-button
	   opoint (point)
	   'follow-link t
	   'mouse-face (list :background (car color)
			     :foreground (if light-p "black" "white"))
	   'color-name (car color)
	   'action callback-fn)))
      (insert "\n"))
    (goto-char (point-min))))


(defun list-colors-duplicates (&optional list)
  "Return a list of colors with grouped duplicate colors.
If a color has no duplicates, then the element of the returned list
has the form '(COLOR-NAME).  The element of the returned list with
duplicate colors has the form '(COLOR-NAME DUPLICATE-COLOR-NAME ...).
This function uses the predicate `facemenu-color-equal' to compare
color names.  If the optional argument LIST is non-nil, it should
be a list of colors to display.  Otherwise, this function uses
a list of colors that the current display can handle."
  (let* ((list (mapcar 'list (or list (defined-colors))))
	 (l list))
    (while (cdr l)
      (if (and (facemenu-color-equal (car (car l)) (car (car (cdr l))))
               ;; On MS-Windows, there are logical colors that might have
               ;; the same value but different names and meanings.  For
               ;; example, `SystemMenuText' (the color w32 uses for the
               ;; text in menu entries) and `SystemWindowText' (the default
               ;; color w32 uses for the text in windows and dialogs) may
               ;; be the same display color and be adjacent in the list.
               ;; These system colors all have names prefixed with "System",
               ;; which is hardcoded in w32fns.c (SYSTEM_COLOR_PREFIX).
               ;; This makes them different to any other color.  Bug#9722
	       (not (and (eq system-type 'windows-nt)
			 (string-match-p "^System" (car (car l))))))
	  (progn
	    (setcdr (car l) (cons (car (car (cdr l))) (cdr (car l))))
	    (setcdr l (cdr (cdr l))))
	(setq l (cdr l))))
    list))

(defun facemenu-color-equal (a b)
  "Return t if colors A and B are the same color.
A and B should be strings naming colors.
This function queries the display system to find out what the color
names mean.  It returns nil if the colors differ or if it can't
determine the correct answer."
  (cond ((equal a b) t)
	((equal (color-values a) (color-values b)))))


(defvar facemenu-self-insert-data nil)

(defun facemenu-post-self-insert-function ()
  (when (and (car facemenu-self-insert-data)
             (eq last-command (cdr facemenu-self-insert-data)))
    (put-text-property (1- (point)) (point)
                       'face (car facemenu-self-insert-data))
    (setq facemenu-self-insert-data nil))
  (remove-hook 'post-self-insert-hook 'facemenu-post-self-insert-function))

(defun facemenu-set-self-insert-face (face)
  "Arrange for the next self-inserted char to have face `face'."
  (setq facemenu-self-insert-data (cons face this-command))
  (add-hook 'post-self-insert-hook 'facemenu-post-self-insert-function))

(defun facemenu-add-face (face &optional start end)
  "Add FACE to text between START and END.
If START is nil or START to END is empty, add FACE to next typed character
instead.  For each section of that region that has a different face property,
FACE will be consed onto it, and other faces that are completely hidden by
that will be removed from the list.
If `facemenu-add-face-function' and maybe `facemenu-end-add-face' are non-nil,
they are used to set the face information.

As a special case, if FACE is `default', then the region is left with NO face
text property.  Otherwise, selecting the default face would not have any
effect.  See `facemenu-remove-face-function'."
  (interactive "*xFace: \nr")
  (cond
   ((and (eq face 'default)
         (not (eq facemenu-remove-face-function t)))
    (if facemenu-remove-face-function
        (funcall facemenu-remove-face-function start end)
      (if (and start (< start end))
          (remove-text-properties start end '(face default))
        (facemenu-set-self-insert-face 'default))))
   (facemenu-add-face-function
    (save-excursion
      (if end (goto-char end))
      (save-excursion
        (if start (goto-char start))
        (insert-before-markers
         (funcall facemenu-add-face-function face end)))
      (if facemenu-end-add-face
          (insert (if (stringp facemenu-end-add-face)
                      facemenu-end-add-face
                    (funcall facemenu-end-add-face face))))))
   ((and start (< start end))
    (let ((part-start start) part-end)
      (while (not (= part-start end))
        (setq part-end (next-single-property-change part-start 'face
                                                    nil end))
        (let ((prev (get-text-property part-start 'face)))
          (put-text-property part-start part-end 'face
                             (if (null prev)
                                 face
                               (facemenu-active-faces
                                (cons face
                                      (if (listp prev)
                                          prev
                                        (list prev)))
                                ;; Specify the selected frame
                                ;; because nil would mean to use
                                ;; the new-frame default settings,
                                ;; and those are usually nil.
                                (selected-frame)))))
        (setq part-start part-end))))
   (t
    (facemenu-set-self-insert-face
     (if (eq last-command (cdr facemenu-self-insert-data))
         (cons face (if (listp (car facemenu-self-insert-data))
                        (car facemenu-self-insert-data)
                      (list (car facemenu-self-insert-data))))
       face))))
  (unless (facemenu-enable-faces-p)
    (message "Font-lock mode will override any faces you set in this buffer")))

(defun facemenu-active-faces (face-list &optional frame)
  "Return from FACE-LIST those faces that would be used for display.
This means each face attribute is not specified in a face earlier in FACE-LIST
and such a face is therefore active when used to display text.
If the optional argument FRAME is given, use the faces in that frame; otherwise
use the selected frame.  If t, then the global, non-frame faces are used."
  (let* ((mask-atts (copy-sequence
		     (if (consp (car face-list))
			 (face-attributes-as-vector (car face-list))
		       (or (internal-lisp-face-p (car face-list) frame)
			   (check-face (car face-list))))))
	 (active-list (list (car face-list)))
	 (face-list (cdr face-list))
	 (mask-len (length mask-atts)))
    (while face-list
      (if (let ((face-atts
		 (if (consp (car face-list))
		     (face-attributes-as-vector (car face-list))
		   (or (internal-lisp-face-p (car face-list) frame)
		       (check-face (car face-list)))))
		(i mask-len)
		(useful nil))
	    (while (>= (setq i (1- i)) 0)
	      (and (not (memq (aref face-atts i) '(nil unspecified)))
		   (memq (aref mask-atts i) '(nil unspecified))
		   (aset mask-atts i (setq useful t))))
	    useful)
	  (setq active-list (cons (car face-list) active-list)))
      (setq face-list (cdr face-list)))
    (nreverse active-list)))

(defun facemenu-add-new-face (face)
  "Add FACE (a face) to the Face menu if `facemenu-listed-faces' says so.
This is called whenever you create a new face, and at other times."
  (let* (name
	 symbol
	 menu docstring
	 (key (cdr (assoc face facemenu-keybindings)))
	 function menu-val)
    (if (symbolp face)
	(setq name (symbol-name face)
	      symbol face)
      (setq name face
	    symbol (intern name)))
    (setq menu 'facemenu-face-menu)
    (setq docstring
	  (purecopy (format "Select face `%s' for subsequent insertion.
If the mark is active and there is no prefix argument,
apply face `%s' to the region instead.
This command was defined by `facemenu-add-new-face'."
		  name name)))
    (cond ((facemenu-iterate ; check if equivalent face is already in the menu
	    (lambda (m) (and (listp m)
			     (symbolp (car m))
			     ;; Avoid error in face-equal
			     ;; when a non-face is erroneously present.
			     (facep (car m))
			     (face-equal (car m) symbol)))
	    (cdr (symbol-function menu))))
	  ;; Faces with a keyboard equivalent.  These go at the front.
	  (key
	   (setq function (intern (concat "facemenu-set-" name)))
	   (fset function
		 `(lambda ()
		    ,docstring
		    (interactive)
		    (facemenu-set-face
		     (quote ,symbol)
		     (if (and mark-active (not current-prefix-arg))
			 (region-beginning))
		     (if (and mark-active (not current-prefix-arg))
			 (region-end)))))
	   (define-key 'facemenu-keymap key (cons name function))
	   (define-key menu key (cons name function)))
	  ;; Faces with no keyboard equivalent.  Figure out where to put it:
	  ((or (eq t facemenu-listed-faces)
	       (memq symbol facemenu-listed-faces))
	   (setq key (vector symbol)
		 function 'facemenu-set-face-from-menu
		 menu-val (symbol-function menu))
	   (if (and facemenu-new-faces-at-end
		    (> (length menu-val) 3))
	       (define-key-after menu-val key (cons name function)
		 (car (nth (- (length menu-val) 3) menu-val)))
	     (define-key menu key (cons name function))))))
  nil) ; Return nil for facemenu-iterate

(defun facemenu-add-new-color (color menu)
  "Add COLOR (a color name string) to the appropriate Face menu.
MENU should be `facemenu-foreground-menu' or `facemenu-background-menu'.
Return the event type (a symbol) of the added menu entry.

This is called whenever you use a new color."
  (let (symbol)
    (unless (color-defined-p color)
      (error "Color `%s' undefined" color))
    (cond ((eq menu 'facemenu-foreground-menu)
	   (setq symbol (intern (concat "fg:" color))))
	  ((eq menu 'facemenu-background-menu)
	   (setq symbol (intern (concat "bg:" color))))
	  (t (error "MENU should be `facemenu-foreground-menu' or `facemenu-background-menu'")))
    (unless (facemenu-iterate ; Check if color is already in the menu.
	     (lambda (m) (and (listp m)
			      (eq (car m) symbol)))
	     (cdr (symbol-function menu)))
      ;; Color is not in the menu.  Figure out where to put it.
      (let ((key (vector symbol))
	    (function 'facemenu-set-face-from-menu)
	    (menu-val (symbol-function menu)))
	(if (and facemenu-new-faces-at-end
		 (> (length menu-val) 3))
	    (define-key-after menu-val key (cons color function)
	      (car (nth (- (length menu-val) 3) menu-val)))
	  (define-key menu key (cons color function)))))
    symbol))

(defun facemenu-complete-face-list (&optional oldlist)
  "Return list of all faces that look different.
Starts with given ALIST of faces, and adds elements only if they display
differently from any face already on the list.
The faces on ALIST will end up at the end of the returned list, in reverse
order."
  (let ((list (nreverse (mapcar 'car oldlist))))
    (facemenu-iterate
     (lambda (new-face)
       (if (not (memq new-face list))
	   (setq list (cons new-face list)))
       nil)
     (nreverse (face-list)))
    list))

(defun facemenu-iterate (func list)
  "Apply FUNC to each element of LIST until one returns non-nil.
Returns the non-nil value it found, or nil if all were nil."
  (while (and list (not (funcall func (car list))))
    (setq list (cdr list)))
  (car list))

(facemenu-update)

(provide 'facemenu)

;;; facemenu.el ends here
