;;; easymenu.el --- support the easymenu interface for defining a menu

;; Copyright (C) 1994, 1996, 1998-2012 Free Software Foundation, Inc.

;; Keywords: emulations
;; Author: Richard Stallman <rms@gnu.org>
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

;; This is compatible with easymenu.el by Per Abrahamsen
;; but it is much simpler as it doesn't try to support other Emacs versions.
;; The code was mostly derived from lmenu.el.

;;; Code:

(eval-when-compile (require 'cl))

(defvar easy-menu-precalculate-equivalent-keybindings nil
  "Determine when equivalent key bindings are computed for easy-menu menus.
It can take some time to calculate the equivalent key bindings that are shown
in a menu.  If the variable is on, then this calculation gives a (maybe
noticeable) delay when a mode is first entered.  If the variable is off, then
this delay will come when a menu is displayed the first time.  If you never use
menus, turn this variable off, otherwise it is probably better to keep it on.")
(make-obsolete-variable
 'easy-menu-precalculate-equivalent-keybindings nil "23.1")

(defsubst easy-menu-intern (s)
  (if (stringp s) (intern s) s))

;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.

If SYMBOL is non-nil, store the menu keymap in the value of SYMBOL,
and define SYMBOL as a function to pop up the menu, with DOC as its doc string.
If SYMBOL is nil, just store the menu keymap into MAPS.

The first element of MENU must be a string.  It is the menu bar item name.
It may be followed by the following keyword argument pairs

   :filter FUNCTION

FUNCTION is a function with one argument, the rest of menu items.
It returns the remaining items of the displayed menu.

   :visible INCLUDE

INCLUDE is an expression; this menu is only visible if this
expression has a non-nil value.  `:included' is an alias for `:visible'.

   :active ENABLE

ENABLE is an expression; the menu is enabled for selection whenever
this expression's value is non-nil.  `:enable' is an alias for `:active'.

The rest of the elements in MENU, are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form:

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbols defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.
This is normally not needed because keyboard equivalents are usually
computed automatically.
KEYS is expanded with `substitute-command-keys' before it is used.

   :key-sequence KEYS

KEYS is nil, a string or a vector; nil or a keyboard equivalent to this
menu item.
This is a hint that will considerably speed up Emacs's first display of
a menu.  Use `:key-sequence nil' when you know that this menu item has no
keyboard equivalent.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection whenever
this expression's value is non-nil.  `:enable' is an alias for `:active'.

   :visible INCLUDE

INCLUDE is an expression; this item is only visible if this
expression has a non-nil value.  `:included' is an alias for `:visible'.

   :label FORM

FORM is an expression that will be dynamically evaluated and whose
value will be used for the menu entry's text label (the default is NAME).

   :suffix FORM

FORM is an expression that will be dynamically evaluated and whose
value will be concatenated to the menu entry's label.

   :style STYLE

STYLE is a symbol describing the type of menu item.  The following are
defined:

toggle: A checkbox.
        Prepend the name with `(*) ' or `( ) ' depending on if selected or not.
radio: A radio button.
       Prepend the name with `[X] ' or `[ ] ' depending on if selected or not.
button: Surround the name with `[' and `]'.  Use this for an item in the
        menu bar itself.
anything else means an ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.

   :help HELP

HELP is a string, the help to display for the menu item.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list with the same format as MENU.  This is a submenu."
  (declare (indent defun))
  `(progn
     ,(if symbol `(defvar ,symbol nil ,doc))
     (easy-menu-do-define (quote ,symbol) ,maps ,doc ,menu)))

(defun easy-menu-binding (menu &optional item-name)
  "Return a binding suitable to pass to `define-key'.
This is expected to be bound to a mouse event."
  ;; Under Emacs this is almost trivial, whereas under XEmacs this may
  ;; involve defining a function that calls popup-menu.
  (let ((props (if (symbolp menu)
                   (prog1 (get menu 'menu-prop)
                     (setq menu (symbol-function menu))))))
    (cons 'menu-item
          (cons (if (eq :label (car props))
                    (prog1 (cadr props)
                      (setq props (cddr props)))
                  (or item-name
                      (if (keymapp menu)
                          (keymap-prompt menu))
                      ""))
                (cons menu props)))))

;;;###autoload
(defun easy-menu-do-define (symbol maps doc menu)
  ;; We can't do anything that might differ between Emacs dialects in
  ;; `easy-menu-define' in order to make byte compiled files
  ;; compatible.  Therefore everything interesting is done in this
  ;; function.
  (let ((keymap (easy-menu-create-menu (car menu) (cdr menu))))
    (when symbol
      (set symbol keymap)
      (defalias symbol
	`(lambda (event) ,doc (interactive "@e")
	   ;; FIXME: XEmacs uses popup-menu which calls the binding
	   ;; while x-popup-menu only returns the selection.
	   (x-popup-menu event
			 (or (and (symbolp ,symbol)
				  (funcall
				   (or (plist-get (get ,symbol 'menu-prop)
						  :filter)
				       'identity)
				   (symbol-function ,symbol)))
			     ,symbol)))))
    (dolist (map (if (keymapp maps) (list maps) maps))
      (define-key map
        (vector 'menu-bar (easy-menu-intern (car menu)))
        (easy-menu-binding keymap (car menu))))))

(defun easy-menu-filter-return (menu &optional name)
 "Convert MENU to the right thing to return from a menu filter.
MENU is a menu as computed by `easy-menu-define' or `easy-menu-create-menu' or
a symbol whose value is such a menu.
In Emacs a menu filter must return a menu (a keymap), in XEmacs a filter must
return a menu items list (without menu name and keywords).
This function returns the right thing in the two cases.
If NAME is provided, it is used for the keymap."
 (cond
  ((and (not (keymapp menu)) (consp menu))
   ;; If it's a cons but not a keymap, then it can't be right
   ;; unless it's an XEmacs menu.
   (setq menu (easy-menu-create-menu (or name "") menu)))
  ((vectorp menu)
   ;; It's just a menu entry.
   (setq menu (cdr (easy-menu-convert-item menu)))))
 menu)

(defvar easy-menu-avoid-duplicate-keys t
  "Dynamically scoped var to register already used keys in a menu.
If it holds a list, this is expected to be a list of keys already seen in the
menu we're processing.  Else it means we're not processing a menu.")

;;;###autoload
(defun easy-menu-create-menu (menu-name menu-items)
  "Create a menu called MENU-NAME with items described in MENU-ITEMS.
MENU-NAME is a string, the name of the menu.  MENU-ITEMS is a list of items
possibly preceded by keyword pairs as described in `easy-menu-define'."
  (let ((menu (make-sparse-keymap menu-name))
        (easy-menu-avoid-duplicate-keys nil)
	prop keyword arg label enable filter visible help)
    ;; Look for keywords.
    (while (and menu-items
		(cdr menu-items)
		(keywordp (setq keyword (car menu-items))))
      (setq arg (cadr menu-items))
      (setq menu-items (cddr menu-items))
      (case keyword
       (:filter
	(setq filter `(lambda (menu)
			(easy-menu-filter-return (,arg menu) ,menu-name))))
       ((:enable :active) (setq enable (or arg ''nil)))
       (:label (setq label arg))
       (:help (setq help arg))
       ((:included :visible) (setq visible (or arg ''nil)))))
    (if (equal visible ''nil)
	nil				; Invisible menu entry, return nil.
      (if (and visible (not (easy-menu-always-true-p visible)))
	  (setq prop (cons :visible (cons visible prop))))
      (if (and enable (not (easy-menu-always-true-p enable)))
	  (setq prop (cons :enable (cons enable prop))))
      (if filter (setq prop (cons :filter (cons filter prop))))
      (if help (setq prop (cons :help (cons help prop))))
      (if label (setq prop (cons :label (cons label prop))))
      (setq menu (if filter
                     ;; The filter expects the menu in its XEmacs form and the
                     ;; pre-filter form will only be passed to the filter
                     ;; anyway, so we'd better not convert it at all (it will
                     ;; be converted on the fly by easy-menu-filter-return).
                     menu-items
                   (append menu (mapcar 'easy-menu-convert-item menu-items))))
      (when prop
	(setq menu (easy-menu-make-symbol menu 'noexp))
	(put menu 'menu-prop prop))
      menu)))


;; Known button types.
(defvar easy-menu-button-prefix
  '((radio . :radio) (toggle . :toggle)))

(defvar easy-menu-converted-items-table (make-hash-table :test 'equal))

(defun easy-menu-convert-item (item)
  "Memoize the value returned by `easy-menu-convert-item-1' called on ITEM.
This makes key-shortcut-caching work a *lot* better when this
conversion is done from within a filter.
This also helps when the NAME of the entry is recreated each time:
since the menu is built and traversed separately, the lookup
would always fail because the key is `equal' but not `eq'."
  (let* ((cache (gethash item easy-menu-converted-items-table))
	 (result (or cache (easy-menu-convert-item-1 item)))
	 (key (car-safe result)))
    (when (and (listp easy-menu-avoid-duplicate-keys) (symbolp key))
      ;; Merging multiple entries with the same name is sometimes what we
      ;; want, but not when the entries are actually different (e.g. same
      ;; name but different :suffix as seen in cal-menu.el) and appear in
      ;; the same menu.  So we try to detect and resolve conflicts.
      (while (memq key easy-menu-avoid-duplicate-keys)
	;; We need to use some distinct object, ideally a symbol, ideally
	;; related to the `name'.  Uninterned symbols do not work (they
	;; are apparently turned into strings and re-interned later on).
	(setq key (intern (format "%s-%d" (symbol-name key)
				  (length easy-menu-avoid-duplicate-keys))))
	(setq result (cons key (cdr result))))
      (push key easy-menu-avoid-duplicate-keys))

    (unless cache (puthash item result easy-menu-converted-items-table))
    result))

(defun easy-menu-convert-item-1 (item)
  "Parse an item description and convert it to a menu keymap element.
ITEM defines an item as in `easy-menu-define'."
  (let (name command label prop remove)
    (cond
     ((stringp item)			; An item or separator.
      (setq label item))
     ((consp item)			; A sub-menu
      (setq label (setq name (car item)))
      (setq command (cdr item))
      (if (not (keymapp command))
	  (setq command (easy-menu-create-menu name command)))
      (if (null command)
	  ;; Invisible menu item. Don't insert into keymap.
	  (setq remove t)
	(when (and (symbolp command) (setq prop (get command 'menu-prop)))
	  (when (eq :label (car prop))
	    (setq label (cadr prop))
	    (setq prop (cddr prop)))
	  (setq command (symbol-function command)))))
     ((vectorp item)			; An item.
      (let* ((ilen (length item))
	     (active (if (> ilen 2) (or (aref item 2) ''nil) t))
	     (no-name (not (symbolp (setq command (aref item 1)))))
	     cache cache-specified)
	(setq label (setq name (aref item 0)))
	(if no-name (setq command (easy-menu-make-symbol command)))
	(if (keywordp active)
	    (let ((count 2)
		  keyword arg suffix visible style selected keys)
	      (setq active nil)
	      (while (> ilen count)
		(setq keyword (aref item count))
		(setq arg (aref item (1+ count)))
		(setq count (+ 2 count))
		(case keyword
                  ((:included :visible) (setq visible (or arg ''nil)))
                  (:key-sequence (setq cache arg cache-specified t))
                  (:keys (setq keys arg no-name nil))
                  (:label (setq label arg))
                  ((:active :enable) (setq active (or arg ''nil)))
                  (:help (setq prop (cons :help (cons arg prop))))
                  (:suffix (setq suffix arg))
                  (:style (setq style arg))
                  (:selected (setq selected (or arg ''nil)))))
	      (if suffix
		  (setq label
			(if (stringp suffix)
			    (if (stringp label) (concat label " " suffix)
			      `(concat ,label ,(concat " " suffix)))
			  (if (stringp label)
			      `(concat ,(concat label " ") ,suffix)
			    `(concat ,label " " ,suffix)))))
	      (cond
	       ((eq style 'button)
		(setq label (if (stringp label) (concat "[" label "]")
			      `(concat "[" ,label "]"))))
	       ((and selected
		     (setq style (assq style easy-menu-button-prefix)))
		(setq prop (cons :button
				 (cons (cons (cdr style) selected) prop)))))
	      (when (stringp keys)
                (if (string-match "^[^\\]*\\(\\\\\\[\\([^]]+\\)]\\)[^\\]*$"
                                  keys)
                    (let ((prefix
                           (if (< (match-beginning 0) (match-beginning 1))
                               (substring keys 0 (match-beginning 1))))
                          (postfix
                           (if (< (match-end 1) (match-end 0))
                               (substring keys (match-end 1))))
                          (cmd (intern (match-string 2 keys))))
                      (setq keys (and (or prefix postfix)
                                      (cons prefix postfix)))
                      (setq keys
                            (and (or keys (not (eq command cmd)))
                                 (cons cmd keys))))
                  (setq cache-specified nil))
                (if keys (setq prop (cons :keys (cons keys prop)))))
	      (if (and visible (not (easy-menu-always-true-p visible)))
		  (if (equal visible ''nil)
		      ;; Invisible menu item. Don't insert into keymap.
		      (setq remove t)
		    (setq prop (cons :visible (cons visible prop)))))))
	(if (and active (not (easy-menu-always-true-p active)))
	    (setq prop (cons :enable (cons active prop))))
	(if (and (or no-name cache-specified)
		 (or (null cache) (stringp cache) (vectorp cache)))
	    (setq prop (cons :key-sequence (cons cache prop))))))
     (t (error "Invalid menu item in easymenu")))
    ;; `intern' the name so as to merge multiple entries with the same name.
    ;; It also makes it easier/possible to lookup/change menu bindings
    ;; via keymap functions.
    (let ((key (easy-menu-intern name)))
      (cons key
            (and (not remove)
                 (cons 'menu-item
                       (cons label
                             (and name
                                  (cons command prop)))))))))

(defun easy-menu-define-key (menu key item &optional before)
  "Add binding in MENU for KEY => ITEM.  Similar to `define-key-after'.
If KEY is not nil then delete any duplications.
If ITEM is nil, then delete the definition of KEY.

Optional argument BEFORE is nil or a key in MENU.  If BEFORE is not nil,
put binding before the item in MENU named BEFORE; otherwise,
if a binding for KEY is already present in MENU, just change it;
otherwise put the new binding last in MENU.
BEFORE can be either a string (menu item name) or a symbol
\(the fake function key for the menu item).
KEY does not have to be a symbol, and comparison is done with equal."
  (if (symbolp menu) (setq menu (indirect-function menu)))
  (let ((inserted (null item))		; Fake already inserted.
	tail done)
    (while (not done)
      (cond
       ((or (setq done (or (null (cdr menu)) (keymapp (cdr menu))))
	    (and before (easy-menu-name-match before (cadr menu))))
	;; If key is nil, stop here, otherwise keep going past the
	;; inserted element so we can delete any duplications that come
	;; later.
	(if (null key) (setq done t))
	(unless inserted		; Don't insert more than once.
	  (setcdr menu (cons (cons key item) (cdr menu)))
	  (setq inserted t)
	  (setq menu (cdr menu)))
	(setq menu (cdr menu)))
       ((and key (equal (car-safe (cadr menu)) key))
	(if (or inserted		; Already inserted or
		(and before		;  wanted elsewhere and
		     (setq tail (cddr menu)) ; not last item and not
		     (not (keymapp tail))
		     (not (easy-menu-name-match
			   before (car tail))))) ; in position
	    (setcdr menu (cddr menu))	; Remove item.
	  (setcdr (cadr menu) item)	; Change item.
	  (setq inserted t)
	  (setq menu (cdr menu))))
       (t (setq menu (cdr menu)))))))

(defun easy-menu-name-match (name item)
  "Return t if NAME is the name of menu item ITEM.
NAME can be either a string, or a symbol.
ITEM should be a keymap binding of the form (KEY . MENU-ITEM)."
  (if (consp item)
      (if (symbolp name)
	  (eq (car-safe item) name)
	(if (stringp name)
	    ;; Match against the text that is displayed to the user.
	    (or (condition-case nil (member-ignore-case name item)
		  (error nil))		;`item' might not be a proper list.
		;; Also check the string version of the symbol name,
		;; for backwards compatibility.
		(eq (car-safe item) (intern name)))))))

(defun easy-menu-always-true-p (x)
  "Return true if form X never evaluates to nil."
  (if (consp x) (and (eq (car x) 'quote) (cadr x))
    (or (eq x t) (not (symbolp x)))))

(defvar easy-menu-item-count 0)

(defun easy-menu-make-symbol (callback &optional noexp)
  "Return a unique symbol with CALLBACK as function value.
When non-nil, NOEXP indicates that CALLBACK cannot be an expression
\(i.e. does not need to be turned into a function)."
  (let ((command
	 (make-symbol (format "menu-function-%d" easy-menu-item-count))))
    (setq easy-menu-item-count (1+ easy-menu-item-count))
    (fset command
	  (if (or (keymapp callback) (commandp callback)
                  ;; `functionp' is probably not needed.
                  (functionp callback) noexp)
              callback
	    `(lambda () (interactive) ,callback)))
    command))

;;;###autoload
(defun easy-menu-change (path name items &optional before map)
  "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu that
should contain a submenu named NAME.
ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that submenu.

If MAP is specified, it should normally be a keymap; nil stands for the local
menu-bar keymap.  It can also be a symbol, which has earlier been used as the
first argument in a call to `easy-menu-define', or the value of such a symbol.

If the menu located by PATH has no submenu named NAME, add one.
If the optional argument BEFORE is present, add it just before
the submenu named BEFORE, otherwise add it at the end of the menu.

To implement dynamic menus, either call this from
`menu-bar-update-hook' or use a menu filter."
  (easy-menu-add-item map path (easy-menu-create-menu name items) before))

;; XEmacs needs the following two functions to add and remove menus.
;; In Emacs this is done automatically when switching keymaps, so
;; here easy-menu-remove is a noop.
(defalias 'easy-menu-remove 'ignore
  "Remove MENU from the current menu bar.
Contrary to XEmacs, this is a nop on Emacs since menus are automatically
\(de)activated when the corresponding keymap is (de)activated.

\(fn MENU)")

(defun easy-menu-add (menu &optional map)
  "Add the menu to the menubar.
On Emacs, menus are already automatically activated when the
corresponding keymap is activated.  On XEmacs this is needed to
actually add the menu to the current menubar.

You should call this once the menu and keybindings are set up
completely and menu filter functions can be expected to work."
  )

(defun add-submenu (menu-path submenu &optional before in-menu)
  "Add submenu SUBMENU in the menu at MENU-PATH.
If BEFORE is non-nil, add before the item named BEFORE.
If IN-MENU is non-nil, follow MENU-PATH in IN-MENU.
This is a compatibility function; use `easy-menu-add-item'."
  (easy-menu-add-item (or in-menu (current-global-map))
		      (cons "menu-bar" menu-path)
		      submenu before))

(defun easy-menu-add-item (map path item &optional before)
  "To the submenu of MAP with path PATH, add ITEM.

If an item with the same name is already present in this submenu,
then ITEM replaces it.  Otherwise, ITEM is added to this submenu.
In the latter case, ITEM is normally added at the end of the submenu.
However, if BEFORE is a string and there is an item in the submenu
with that name, then ITEM is added before that item.

MAP should normally be a keymap; nil stands for the local menu-bar keymap.
It can also be a symbol, which has earlier been used as the first
argument in a call to `easy-menu-define', or the value of such a symbol.

PATH is a list of strings for locating the submenu where ITEM is to be
added.  If PATH is nil, MAP itself is used.  Otherwise, the first
element should be the name of a submenu directly under MAP.  This
submenu is then traversed recursively with the remaining elements of PATH.

ITEM is either defined as in `easy-menu-define' or a non-nil value returned
by `easy-menu-item-present-p' or `easy-menu-remove-item' or a menu defined
earlier by `easy-menu-define' or `easy-menu-create-menu'."
  (setq map (easy-menu-get-map map path
			       (and (null map) (null path)
				    (stringp (car-safe item))
				    (car item))))
  (if (and (consp item) (consp (cdr item)) (eq (cadr item) 'menu-item))
      ;; This is a value returned by `easy-menu-item-present-p' or
      ;; `easy-menu-remove-item'.
      (easy-menu-define-key map (easy-menu-intern (car item))
			    (cdr item) before)
    (if (or (keymapp item)
	    (and (symbolp item) (keymapp (symbol-value item))
		 (setq item (symbol-value item))))
	;; Item is a keymap, find the prompt string and use as item name.
	(setq item (cons (keymap-prompt item) item)))
    (setq item (easy-menu-convert-item item))
    (easy-menu-define-key map (easy-menu-intern (car item)) (cdr item) before)))

(defun easy-menu-item-present-p (map path name)
  "In submenu of MAP with path PATH, return non-nil if item NAME is present.
MAP and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be looked for."
  (easy-menu-return-item (easy-menu-get-map map path) name))

(defun easy-menu-remove-item (map path name)
  "From submenu of MAP with path PATH remove item NAME.
MAP and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be removed."
  (setq map (easy-menu-get-map map path))
  (let ((ret (easy-menu-return-item map name)))
    (if ret (easy-menu-define-key map (easy-menu-intern name) nil))
    ret))

(defun easy-menu-return-item (menu name)
  "In menu MENU try to look for menu item with name NAME.
If a menu item is found, return (NAME . item), otherwise return nil.
If item is an old format item, a new format item is returned."
  ;; The call to `lookup-key' also calls the C function `get_keyelt' which
  ;; looks inside a menu-item to only return the actual command.  This is
  ;; not what we want here.  We should either add an arg to lookup-key to be
  ;; able to turn off this "feature", or else we could use map-keymap here.
  ;; In the mean time, I just use `assq' which is an OK approximation since
  ;; menus are rarely built from vectors or char-tables.
  (let ((item (or (cdr (assq name menu))
                  (lookup-key menu (vector (easy-menu-intern name)))))
	ret enable cache label)
    (cond
     ((stringp (car-safe item))
      ;; This is the old menu format. Convert it to new format.
      (setq label (car item))
      (when (stringp (car (setq item (cdr item)))) ; Got help string
	(setq ret (list :help (car item)))
	(setq item (cdr item)))
      (when (and (consp item) (consp (car item))
		 (or (null (caar item)) (numberp (caar item))))
	(setq cache (car item))		; Got cache
	(setq item (cdr item)))
      (and (symbolp item) (setq enable (get item 'menu-enable))	; Got enable
	   (setq ret (cons :enable (cons enable ret))))
      (if cache (setq ret (cons cache ret)))
      (cons name (cons 'menu-enable (cons label (cons item ret)))))
     (item ; (or (symbolp item) (keymapp item) (eq (car-safe item) 'menu-item))
      (cons name item))			; Keymap or new menu format
     )))

(defun easy-menu-lookup-name (map name)
  "Lookup menu item NAME in keymap MAP.
Like `lookup-key' except that NAME is not an array but just a single key
and that NAME can be a string representing the menu item's name."
  (or (lookup-key map (vector (easy-menu-intern name)))
      (when (stringp name)
	;; `lookup-key' failed and we have a menu item name: look at the
	;; actual menu entries's names.
	(catch 'found
	  (map-keymap (lambda (key item)
			(if (condition-case nil (member name item)
			      (error nil))
			    ;; Found it!!  Look for it again with
			    ;; `lookup-key' so as to handle inheritance and
			    ;; to extract the actual command/keymap bound to
			    ;; `name' from the item (via get_keyelt).
			    (throw 'found (lookup-key map (vector key)))))
		      map)))))

(defun easy-menu-get-map (map path &optional to-modify)
  "Return a sparse keymap in which to add or remove an item.
MAP and PATH are as defined in `easy-menu-add-item'.

TO-MODIFY, if non-nil, is the name of the item the caller
wants to modify in the map that we return.
In some cases we use that to select between the local and global maps."
  (setq map
	(catch 'found
	  (if (and map (symbolp map) (not (keymapp map)))
	      (setq map (symbol-value map)))
	  (let ((maps (if map (if (keymapp map) (list map) map)
			(current-active-maps))))
	    ;; Look for PATH in each map.
	    (unless map (push 'menu-bar path))
	    (dolist (name path)
	      (setq maps
		    (delq nil (mapcar (lambda (map)
					(setq map (easy-menu-lookup-name
						   map name))
					(and (keymapp map) map))
				      maps))))

	    ;; Prefer a map that already contains the to-be-modified entry.
	    (when to-modify
	      (dolist (map maps)
		(when (easy-menu-lookup-name map to-modify)
		  (throw 'found map))))
	    ;; Use the first valid map.
	    (when maps (throw 'found (car maps)))

	    ;; Otherwise, make one up.
	    ;; Hardcoding current-local-map is lame, but it's difficult
	    ;; to know what the caller intended for us to do ;-(
	    (let* ((name (if path (format "%s" (car (reverse path)))))
		   (newmap (make-sparse-keymap name)))
	      (define-key (or map (current-local-map))
		(apply 'vector (mapcar 'easy-menu-intern path))
		(if name (cons name newmap) newmap))
	      newmap))))
  (or (keymapp map) (error "Malformed menu in easy-menu: (%s)" map))
  map)

(provide 'easymenu)

;;; easymenu.el ends here
