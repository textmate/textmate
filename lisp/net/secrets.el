;;; secrets.el --- Client interface to gnome-keyring and kwallet.

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm password passphrase

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

;; This package provides an implementation of the Secret Service API
;; <http://www.freedesktop.org/wiki/Specifications/secret-storage-spec>.
;; This API is meant to make GNOME-Keyring- and KWallet-like daemons
;; available under a common D-BUS interface and thus increase
;; interoperability between GNOME, KDE and other applications having
;; the need to securely store passwords and other confidential
;; information.

;; In order to activate this package, you must add the following code
;; into your .emacs:
;;
;;   (require 'secrets)
;;
;; Afterwards, the variable `secrets-enabled' is non-nil when there is
;; a daemon providing this interface.

;; The atomic objects to be managed by the Secret Service API are
;; secret items, which are something an application wishes to store
;; securely.  A good example is a password that an application needs
;; to save and use at a later date.

;; Secret items are grouped in collections.  A collection is similar
;; in concept to the terms 'keyring' or 'wallet'.  A common collection
;; is called "login".  A collection is stored permanently under the
;; user's permissions, and can be accessed in a user session context.

;; A collection can have an alias name.  The use case for this is to
;; set the alias "default" for a given collection, making it
;; transparent for clients, which collection is used.  Other aliases
;; are not supported (yet).  Since an alias is visible to all
;; applications, this setting shall be performed with care.

;; A list of all available collections is available by
;;
;;   (secrets-list-collections)
;;    => ("session" "login" "ssh keys")

;; The "default" alias could be set to the "login" collection by
;;
;;   (secrets-set-alias "login" "default")

;; An alias can also be dereferenced
;;
;;   (secrets-get-alias "default")
;;    => "login"

;; Collections can be created and deleted.  As already said,
;; collections are used by different applications.  Therefore, those
;; operations shall also be performed with care.  Common collections,
;; like "login", shall not be changed except adding or deleting secret
;; items.
;;
;;   (secrets-delete-collection "my collection")
;;   (secrets-create-collection "my collection")

;; There exists a special collection called "session", which has the
;; lifetime of the corresponding client session (aka Emacs's
;; lifetime).  It is created automatically when Emacs uses the Secret
;; Service interface, and it is deleted when Emacs is killed.
;; Therefore, it can be used to store and retrieve secret items
;; temporarily.  This shall be preferred over creation of a persistent
;; collection, when the information shall not live longer than Emacs.
;; The session collection can be addressed either by the string
;; "session", or by `nil', whenever a collection parameter is needed.

;; As already said, a collection is a group of secret items.  A secret
;; item has a label, the "secret" (which is a string), and a set of
;; lookup attributes.  The attributes can be used to search and
;; retrieve a secret item at a later date.

;; A list of all available secret items of a collection is available by
;;
;;   (secrets-list-items "my collection")
;;    => ("this item" "another item")

;; Secret items can be added or deleted to a collection.  In the
;; following examples, we use the special collection "session", which
;; is bound to Emacs's lifetime.
;;
;;   (secrets-delete-item "session" "my item")
;;   (secrets-create-item "session" "my item" "geheim"
;;                        :user "joe" :host "remote-host")

;; The string "geheim" is the secret of the secret item "my item".
;; The secret string can be retrieved from items:
;;
;;   (secrets-get-secret "session" "my item")
;;    => "geheim"

;; The lookup attributes, which are specified during creation of a
;; secret item, must be a key-value pair.  Keys are keyword symbols,
;; starting with a colon; values are strings.  They can be retrieved
;; from a given secret item:
;;
;;   (secrets-get-attribute "session" "my item" :host)
;;    => "remote-host"
;;
;;   (secrets-get-attributes "session" "my item")
;;    => ((:user . "joe") (:host ."remote-host"))

;; The lookup attributes can be used for searching of items.  If you,
;; for example, are looking for all secret items for the user "joe",
;; you would perform
;;
;;   (secrets-search-items "session" :user "joe")
;;    => ("my item" "another item")

;; Interactively, collections, items and their attributes could be
;; inspected by the command `secrets-show-secrets'.

;;; Code:

;; It has been tested with GNOME Keyring 2.29.92.  An implementation
;; for KWallet will be available at
;; svn://anonsvn.kde.org/home/kde/trunk/playground/base/ksecretservice;
;; not tested yet.

;; Pacify byte-compiler.  D-Bus support in the Emacs core can be
;; disabled with configuration option "--without-dbus".  Declare used
;; subroutines and variables of `dbus' therefore.
(eval-when-compile
  (require 'cl))

(declare-function dbus-call-method "dbusbind.c")
(declare-function dbus-register-signal "dbusbind.c")
(defvar dbus-debug)

(require 'dbus)

(autoload 'tree-widget-set-theme "tree-widget")
(autoload 'widget-create-child-and-convert "wid-edit")
(autoload 'widget-default-value-set "wid-edit")
(autoload 'widget-field-end "wid-edit")
(autoload 'widget-member "wid-edit")
(defvar tree-widget-after-toggle-functions)

(defvar secrets-enabled nil
  "Whether there is a daemon offering the Secret Service API.")

(defvar secrets-debug t
  "Write debug messages")

(defconst secrets-service "org.freedesktop.secrets"
  "The D-Bus name used to talk to Secret Service.")

(defconst secrets-path "/org/freedesktop/secrets"
  "The D-Bus root object path used to talk to Secret Service.")

(defconst secrets-empty-path "/"
  "The D-Bus object path representing an empty object.")

(defsubst secrets-empty-path (path)
  "Check, whether PATH is a valid object path.
It returns t if not."
  (or (not (stringp path))
      (string-equal path secrets-empty-path)))

(defconst secrets-interface-service "org.freedesktop.Secret.Service"
  "The D-Bus interface managing sessions and collections.")

;; <interface name="org.freedesktop.Secret.Service">
;;   <property name="Collections" type="ao" access="read"/>
;;   <method name="OpenSession">
;;     <arg name="algorithm" type="s" direction="in"/>
;;     <arg name="input"     type="v" direction="in"/>
;;     <arg name="output"    type="v" direction="out"/>
;;     <arg name="result"    type="o" direction="out"/>
;;   </method>
;;   <method name="CreateCollection">
;;     <arg name="props"      type="a{sv}" direction="in"/>
;;     <arg name="collection" type="o"     direction="out"/>
;;     <arg name="prompt"     type="o"     direction="out"/>
;;   </method>
;;   <method name="SearchItems">
;;     <arg name="attributes" type="a{ss}" direction="in"/>
;;     <arg name="unlocked"   type="ao"    direction="out"/>
;;     <arg name="locked"     type="ao"    direction="out"/>
;;   </method>
;;   <method name="Unlock">
;;     <arg name="objects"  type="ao" direction="in"/>
;;     <arg name="unlocked" type="ao" direction="out"/>
;;     <arg name="prompt"   type="o"  direction="out"/>
;;   </method>
;;   <method name="Lock">
;;     <arg name="objects" type="ao" direction="in"/>
;;     <arg name="locked"  type="ao" direction="out"/>
;;     <arg name="Prompt"  type="o"  direction="out"/>
;;   </method>
;;   <method name="GetSecrets">
;;     <arg name="items"   type="ao"          direction="in"/>
;;     <arg name="session" type="o"           direction="in"/>
;;     <arg name="secrets" type="a{o(oayay)}" direction="out"/>
;;   </method>
;;   <method name="ReadAlias">
;;     <arg name="name"       type="s" direction="in"/>
;;     <arg name="collection" type="o" direction="out"/>
;;   </method>
;;   <method name="SetAlias">
;;     <arg name="name"       type="s" direction="in"/>
;;     <arg name="collection" type="o" direction="in"/>
;;   </method>
;;   <signal name="CollectionCreated">
;;     <arg name="collection" type="o"/>
;;   </signal>
;;   <signal name="CollectionDeleted">
;;     <arg name="collection" type="o"/>
;;   </signal>
;; </interface>

(defconst secrets-interface-collection "org.freedesktop.Secret.Collection"
  "A collection of items containing secrets.")

;; <interface name="org.freedesktop.Secret.Collection">
;;   <property name="Items"    type="ao" access="read"/>
;;   <property name="Label"    type="s"  access="readwrite"/>
;;   <property name="Locked"   type="s"  access="read"/>
;;   <property name="Created"  type="t"  access="read"/>
;;   <property name="Modified" type="t"  access="read"/>
;;   <method name="Delete">
;;     <arg name="prompt" type="o" direction="out"/>
;;   </method>
;;   <method name="SearchItems">
;;     <arg name="attributes" type="a{ss}" direction="in"/>
;;     <arg name="results"    type="ao"    direction="out"/>
;;   </method>
;;   <method name="CreateItem">
;;     <arg name="props"   type="a{sv}"   direction="in"/>
;;     <arg name="secret"  type="(oayay)" direction="in"/>
;;     <arg name="replace" type="b"       direction="in"/>
;;     <arg name="item"    type="o"       direction="out"/>
;;     <arg name="prompt"  type="o"       direction="out"/>
;;   </method>
;;   <signal name="ItemCreated">
;;     <arg name="item" type="o"/>
;;   </signal>
;;   <signal name="ItemDeleted">
;;     <arg name="item" type="o"/>
;;   </signal>
;;   <signal name="ItemChanged">
;;     <arg name="item" type="o"/>
;;   </signal>
;; </interface>

(defconst secrets-session-collection-path
  "/org/freedesktop/secrets/collection/session"
  "The D-Bus temporary session collection object path.")

(defconst secrets-interface-prompt "org.freedesktop.Secret.Prompt"
  "A session tracks state between the service and a client application.")

;; <interface name="org.freedesktop.Secret.Prompt">
;;   <method name="Prompt">
;;     <arg name="window-id" type="s" direction="in"/>
;;   </method>
;;   <method name="Dismiss"></method>
;;   <signal name="Completed">
;;     <arg name="dismissed" type="b"/>
;;     <arg name="result"    type="v"/>
;;   </signal>
;; </interface>

(defconst secrets-interface-item "org.freedesktop.Secret.Item"
  "A collection of items containing secrets.")

;; <interface name="org.freedesktop.Secret.Item">
;;   <property name="Locked"     type="b"     access="read"/>
;;   <property name="Attributes" type="a{ss}" access="readwrite"/>
;;   <property name="Label"      type="s"     access="readwrite"/>
;;   <property name="Created"    type="t"     access="read"/>
;;   <property name="Modified"   type="t"     access="read"/>
;;   <method name="Delete">
;;     <arg name="prompt" type="o" direction="out"/>
;;   </method>
;;   <method name="GetSecret">
;;     <arg name="session" type="o"       direction="in"/>
;;     <arg name="secret"  type="(oayay)" direction="out"/>
;;   </method>
;;   <method name="SetSecret">
;;     <arg name="secret" type="(oayay)" direction="in"/>
;;   </method>
;; </interface>
;;
;; STRUCT	secret
;;   OBJECT PATH  session
;;   ARRAY BYTE	  parameters
;;   ARRAY BYTE	  value

(defconst secrets-interface-item-type-generic "org.freedesktop.Secret.Generic"
  "The default item type we are using.")

(defconst secrets-interface-session "org.freedesktop.Secret.Session"
  "A session tracks state between the service and a client application.")

;; <interface name="org.freedesktop.Secret.Session">
;;   <method name="Close"></method>
;; </interface>

;;; Sessions.

(defvar secrets-session-path secrets-empty-path
  "The D-Bus session path of the active session.
A session path `secrets-empty-path' indicates there is no open session.")

(defun secrets-close-session ()
  "Close the secret service session, if any."
  (dbus-ignore-errors
    (dbus-call-method
     :session secrets-service secrets-session-path
     secrets-interface-session "Close"))
  (setq secrets-session-path secrets-empty-path))

(defun secrets-open-session (&optional reopen)
  "Open a new session with \"plain\" algorithm.
If there exists another active session, and REOPEN is nil, that
session will be used.  The object path of the session will be
returned, and it will be stored in `secrets-session-path'."
  (when reopen (secrets-close-session))
  (when (secrets-empty-path secrets-session-path)
    (setq secrets-session-path
	  (cadr
	   (dbus-call-method
	    :session secrets-service secrets-path
	    secrets-interface-service "OpenSession" "plain" '(:variant "")))))
  (when secrets-debug
    (message "Secret Service session: %s" secrets-session-path))
  secrets-session-path)

;;; Prompts.

(defvar secrets-prompt-signal nil
  "Internal variable to catch signals from `secrets-interface-prompt'.")

(defun secrets-prompt (prompt)
  "Handle the prompt identified by object path PROMPT."
  (unless (secrets-empty-path prompt)
    (let ((object
	   (dbus-register-signal
	    :session secrets-service prompt
	    secrets-interface-prompt "Completed" 'secrets-prompt-handler)))
      (dbus-call-method
       :session secrets-service prompt
       secrets-interface-prompt "Prompt" (frame-parameter nil 'window-id))
      (unwind-protect
	  (progn
	    ;; Wait until the returned prompt signal has put the
	    ;; result into `secrets-prompt-signal'.
	    (while (null secrets-prompt-signal)
	      (read-event nil nil 0.1))
	    ;; Return the object(s).  It is a variant, so we must use a car.
	    (car secrets-prompt-signal))
	;; Cleanup.
	(setq secrets-prompt-signal nil)
	(dbus-unregister-object object)))))

(defun secrets-prompt-handler (&rest args)
  "Handler for signals emitted by `secrets-interface-prompt'."
  ;; An empty object path is always identified as `secrets-empty-path'
  ;; or `nil'.  Either we set it explicitly, or it is returned by the
  ;; "Completed" signal.
  (if (car args) ;; dismissed
      (setq secrets-prompt-signal (list secrets-empty-path))
    (setq secrets-prompt-signal (cadr args))))

;;; Collections.

(defvar secrets-collection-paths nil
  "Cached D-Bus object paths of available collections.")

(defun secrets-collection-handler (&rest args)
  "Handler for signals emitted by `secrets-interface-service'."
  (cond
   ((string-equal (dbus-event-member-name last-input-event) "CollectionCreated")
    (add-to-list 'secrets-collection-paths (car args)))
   ((string-equal (dbus-event-member-name last-input-event) "CollectionDeleted")
    (setq secrets-collection-paths
	  (delete (car args) secrets-collection-paths)))))

(defun secrets-get-collections ()
  "Return the object paths of all available collections."
  (setq secrets-collection-paths
	(or secrets-collection-paths
	    (dbus-get-property
	     :session secrets-service secrets-path
	     secrets-interface-service "Collections"))))

(defun secrets-get-collection-properties (collection-path)
  "Return all properties of collection identified by COLLECTION-PATH."
  (unless (secrets-empty-path collection-path)
    (dbus-get-all-properties
     :session secrets-service collection-path
     secrets-interface-collection)))

(defun secrets-get-collection-property (collection-path property)
  "Return property PROPERTY of collection identified by COLLECTION-PATH."
  (unless (or (secrets-empty-path collection-path) (not (stringp property)))
    (dbus-get-property
     :session secrets-service collection-path
     secrets-interface-collection property)))

(defun secrets-list-collections ()
  "Return a list of collection names."
  (mapcar
   (lambda (collection-path)
     (if (string-equal collection-path secrets-session-collection-path)
	 "session"
       (secrets-get-collection-property collection-path "Label")))
   (secrets-get-collections)))

(defun secrets-collection-path (collection)
  "Return the object path of collection labeled COLLECTION.
If COLLECTION is nil, return the session collection path.
If there is no such COLLECTION, return nil."
  (or
   ;; The "session" collection.
   (if (or (null collection) (string-equal "session" collection))
       secrets-session-collection-path)
   ;; Check for an alias.
   (let ((collection-path
	  (dbus-call-method
	   :session secrets-service secrets-path
	   secrets-interface-service "ReadAlias" collection)))
     (unless (secrets-empty-path collection-path)
       collection-path))
   ;; Check the collections.
   (catch 'collection-found
     (dolist (collection-path (secrets-get-collections) nil)
       (when (string-equal
	      collection
	      (secrets-get-collection-property collection-path "Label"))
	 (throw 'collection-found collection-path))))))

(defun secrets-create-collection (collection)
  "Create collection labeled COLLECTION if it doesn't exist.
Return the D-Bus object path for collection."
  (let ((collection-path (secrets-collection-path collection)))
    ;; Create the collection.
    (when (secrets-empty-path collection-path)
      (setq collection-path
	    (secrets-prompt
	     (cadr
	      ;; "CreateCollection" returns the prompt path as second arg.
	      (dbus-call-method
	       :session secrets-service secrets-path
	       secrets-interface-service "CreateCollection"
	       `(:array (:dict-entry "Label" (:variant ,collection))))))))
    ;; Return object path of the collection.
    collection-path))

(defun secrets-get-alias (alias)
  "Return the collection name ALIAS is referencing to.
For the time being, only the alias \"default\" is supported."
  (secrets-get-collection-property
   (dbus-call-method
    :session secrets-service secrets-path
    secrets-interface-service "ReadAlias" alias)
   "Label"))

(defun secrets-set-alias (collection alias)
  "Set ALIAS as alias of collection labeled COLLECTION.
For the time being, only the alias \"default\" is supported."
  (let ((collection-path (secrets-collection-path collection)))
    (unless (secrets-empty-path collection-path)
      (dbus-call-method
       :session secrets-service secrets-path
       secrets-interface-service "SetAlias"
       alias :object-path collection-path))))

(defun secrets-delete-alias (alias)
  "Delete ALIAS, referencing to a collection."
  (dbus-call-method
   :session secrets-service secrets-path
   secrets-interface-service "SetAlias"
   alias :object-path secrets-empty-path))

(defun secrets-unlock-collection (collection)
  "Unlock collection labeled COLLECTION.
If successful, return the object path of the collection."
  (let ((collection-path (secrets-collection-path collection)))
    (unless (secrets-empty-path collection-path)
      (secrets-prompt
       (cadr
	(dbus-call-method
	 :session secrets-service secrets-path secrets-interface-service
	 "Unlock" `(:array :object-path ,collection-path)))))
    collection-path))

(defun secrets-delete-collection (collection)
  "Delete collection labeled COLLECTION."
  (let ((collection-path (secrets-collection-path collection)))
    (unless (secrets-empty-path collection-path)
      (secrets-prompt
       (dbus-call-method
	:session secrets-service collection-path
	secrets-interface-collection "Delete")))))

;;; Items.

(defun secrets-get-items (collection-path)
  "Return the object paths of all available items in COLLECTION-PATH."
  (unless (secrets-empty-path collection-path)
    (secrets-open-session)
    (dbus-get-property
     :session secrets-service collection-path
     secrets-interface-collection "Items")))

(defun secrets-get-item-properties (item-path)
  "Return all properties of item identified by ITEM-PATH."
  (unless (secrets-empty-path item-path)
    (dbus-get-all-properties
     :session secrets-service item-path
     secrets-interface-item)))

(defun secrets-get-item-property (item-path property)
  "Return property PROPERTY of item identified by ITEM-PATH."
  (unless (or (secrets-empty-path item-path) (not (stringp property)))
    (dbus-get-property
     :session secrets-service item-path
     secrets-interface-item property)))

(defun secrets-list-items (collection)
  "Return a list of all item labels of COLLECTION."
  (let ((collection-path (secrets-unlock-collection collection)))
    (unless (secrets-empty-path collection-path)
      (mapcar
       (lambda (item-path)
	 (secrets-get-item-property item-path "Label"))
       (secrets-get-items collection-path)))))

(defun secrets-search-items (collection &rest attributes)
  "Search items in COLLECTION with ATTRIBUTES.
ATTRIBUTES are key-value pairs.  The keys are keyword symbols,
starting with a colon.  Example:

  \(secrets-create-item \"Tramp collection\" \"item\" \"geheim\"
   :method \"sudo\" :user \"joe\" :host \"remote-host\"\)

The object paths of the found items are returned as list."
  (let ((collection-path (secrets-unlock-collection collection))
	result props)
    (unless (secrets-empty-path collection-path)
      ;; Create attributes list.
      (while (consp (cdr attributes))
	(unless (keywordp (car attributes))
	  (error 'wrong-type-argument (car attributes)))
	(setq props (add-to-list
		     'props
		     (list :dict-entry
			   (substring (symbol-name (car attributes)) 1)
			   (cadr attributes))
		     'append)
	      attributes (cddr attributes)))
      ;; Search.  The result is a list of two lists, the object paths
      ;; of the unlocked and the locked items.
      (setq result
	    (dbus-call-method
	     :session secrets-service collection-path
	     secrets-interface-collection "SearchItems"
	     (if props
		 (cons :array props)
	       '(:array :signature "{ss}"))))
      ;; Return the found items.
      (mapcar
       (lambda (item-path) (secrets-get-item-property item-path "Label"))
       (append (car result) (cadr result))))))

(defun secrets-create-item (collection item password &rest attributes)
  "Create a new item in COLLECTION with label ITEM and password PASSWORD.
ATTRIBUTES are key-value pairs set for the created item.  The
keys are keyword symbols, starting with a colon.  Example:

  \(secrets-create-item \"Tramp collection\" \"item\" \"geheim\"
   :method \"sudo\" :user \"joe\" :host \"remote-host\"\)

The object path of the created item is returned."
  (unless (member item (secrets-list-items collection))
    (let ((collection-path (secrets-unlock-collection collection))
	  result props)
      (unless (secrets-empty-path collection-path)
	;; Create attributes list.
	(while (consp (cdr attributes))
	  (unless (keywordp (car attributes))
	    (error 'wrong-type-argument (car attributes)))
	  (setq props (add-to-list
		       'props
		       (list :dict-entry
			     (substring (symbol-name (car attributes)) 1)
			     (cadr attributes))
		       'append)
		attributes (cddr attributes)))
	;; Create the item.
	(setq result
	      (dbus-call-method
	       :session secrets-service collection-path
	       secrets-interface-collection "CreateItem"
	       ;; Properties.
	       (append
		`(:array
		  (:dict-entry "Label" (:variant ,item))
		  (:dict-entry
		   "Type" (:variant ,secrets-interface-item-type-generic)))
		(when props
		  `((:dict-entry
		     "Attributes" (:variant ,(append '(:array) props))))))
	       ;; Secret.
	       `(:struct :object-path ,secrets-session-path
			 (:array :signature "y") ;; no parameters.
			 ,(dbus-string-to-byte-array password))
	       ;; Do not replace. Replace does not seem to work.
	       nil))
	(secrets-prompt (cadr result))
	;; Return the object path.
	(car result)))))

(defun secrets-item-path (collection item)
  "Return the object path of item labeled ITEM in COLLECTION.
If there is no such item, return nil."
  (let ((collection-path (secrets-unlock-collection collection)))
    (catch 'item-found
      (dolist (item-path (secrets-get-items collection-path))
	(when (string-equal item (secrets-get-item-property item-path "Label"))
	  (throw 'item-found item-path))))))

(defun secrets-get-secret (collection item)
  "Return the secret of item labeled ITEM in COLLECTION.
If there is no such item, return nil."
  (let ((item-path (secrets-item-path collection item)))
    (unless (secrets-empty-path item-path)
      (dbus-byte-array-to-string
       (caddr
	(dbus-call-method
	 :session secrets-service item-path secrets-interface-item
	 "GetSecret" :object-path secrets-session-path))))))

(defun secrets-get-attributes (collection item)
  "Return the lookup attributes of item labeled ITEM in COLLECTION.
If there is no such item, or the item has no attributes, return nil."
  (unless (stringp collection) (setq collection "default"))
  (let ((item-path (secrets-item-path collection item)))
    (unless (secrets-empty-path item-path)
      (mapcar
       (lambda (attribute)
	 (cons (intern (concat ":" (car attribute))) (cadr attribute)))
       (dbus-get-property
	:session secrets-service item-path
	secrets-interface-item "Attributes")))))

(defun secrets-get-attribute (collection item attribute)
  "Return the value of ATTRIBUTE of item labeled ITEM in COLLECTION.
If there is no such item, or the item doesn't own this attribute, return nil."
  (cdr (assoc attribute (secrets-get-attributes collection item))))

(defun secrets-delete-item (collection item)
  "Delete ITEM in COLLECTION."
  (let ((item-path (secrets-item-path collection item)))
    (unless (secrets-empty-path item-path)
      (secrets-prompt
       (dbus-call-method
	:session secrets-service item-path
	secrets-interface-item "Delete")))))

;;; Visualization.

(define-derived-mode secrets-mode nil "Secrets"
  "Major mode for presenting password entries retrieved by Security Service.
In this mode, widgets represent the search results.

\\{secrets-mode-map}"
  ;; Keymap.
  (setq secrets-mode-map (copy-keymap special-mode-map))
  (set-keymap-parent secrets-mode-map widget-keymap)
  (define-key secrets-mode-map "z" 'kill-this-buffer)

  ;; When we toggle, we must set temporary widgets.
  (set (make-local-variable 'tree-widget-after-toggle-functions)
       '(secrets-tree-widget-after-toggle-function))

  (when (not (called-interactively-p 'interactive))
    ;; Initialize buffer.
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;; It doesn't make sense to call it interactively.
(put 'secrets-mode 'disabled t)

;; The very first buffer created with `secrets-mode' does not have the
;; keymap etc.  So we create a dummy buffer.  Stupid.
(with-temp-buffer (secrets-mode))

;; We autoload `secrets-show-secrets' only on systems with D-Bus support.
;;;###autoload(when (featurep 'dbusbind)
;;;###autoload  (autoload 'secrets-show-secrets "secrets" nil t))

(defun secrets-show-secrets ()
  "Display a list of collections from the Secret Service API.
The collections are in tree view, that means they can be expanded
to the corresponding secret items, which could also be expanded
to their attributes."
  (interactive)

  ;; Check, whether the Secret Service API is enabled.
  (if (null secrets-enabled)
      (message "Secret Service not available")

    ;; Create the search buffer.
    (with-current-buffer (get-buffer-create "*Secrets*")
      (switch-to-buffer-other-window (current-buffer))
      ;; Initialize buffer with `secrets-mode'.
      (secrets-mode)
      (secrets-show-collections))))

(defun secrets-show-collections ()
  "Show all available collections."
  (let ((inhibit-read-only t)
	(alias (secrets-get-alias "default")))
    (erase-buffer)
    (tree-widget-set-theme "folder")
    (dolist (coll (secrets-list-collections))
      (widget-create
     `(tree-widget
       :tag ,coll
       :collection ,coll
       :open nil
       :sample-face bold
       :expander secrets-expand-collection)))))

(defun secrets-expand-collection (widget)
  "Expand items of collection shown as WIDGET."
  (let ((coll (widget-get widget :collection)))
    (mapcar
     (lambda (item)
       `(tree-widget
	 :tag ,item
	 :collection ,coll
	 :item ,item
	 :open nil
	 :sample-face bold
	 :expander secrets-expand-item))
     (secrets-list-items coll))))

(defun secrets-expand-item (widget)
  "Expand password and attributes of item shown as WIDGET."
  (let* ((coll (widget-get widget :collection))
	 (item (widget-get widget :item))
	 (attributes (secrets-get-attributes coll item))
	 ;; padding is needed to format attribute names.
	 (padding
	  (apply
	   'max
	   (cons
	    (1+ (length "password"))
	    (mapcar
	     ;; Attribute names have a leading ":", which will be suppressed.
	     (lambda (attribute) (length (symbol-name (car attribute))))
	     attributes)))))
    (cons
     ;; The password widget.
     `(editable-field :tag "password"
		      :secret ?*
		      :value ,(secrets-get-secret coll item)
		      :sample-face widget-button-pressed
		      ;; We specify :size in order to limit the field.
		      :size 0
		      :format ,(concat
				"%{%t%}:"
				(make-string (- padding (length "password")) ? )
				"%v\n"))
     (mapcar
      (lambda (attribute)
	(let ((name (substring (symbol-name (car attribute)) 1))
	      (value (cdr attribute)))
	  ;; The attribute widget.
	  `(editable-field :tag ,name
			   :value ,value
			   :sample-face widget-documentation
			   ;; We specify :size in order to limit the field.
			   :size 0
			   :format ,(concat
				     "%{%t%}:"
				     (make-string (- padding (length name)) ? )
				     "%v\n"))))
      attributes))))

(defun secrets-tree-widget-after-toggle-function (widget &rest ignore)
  "Add a temporary widget to show the password."
  (dolist (child (widget-get widget :children))
    (when (widget-member child :secret)
      (goto-char (widget-field-end child))
      (widget-insert " ")
      (widget-create-child-and-convert
       child 'push-button
       :notify 'secrets-tree-widget-show-password
       "Show password")))
  (widget-setup))

(defun secrets-tree-widget-show-password (widget &rest ignore)
  "Show password, and remove temporary widget."
  (let ((parent (widget-get widget :parent)))
    (widget-put parent :secret nil)
    (widget-default-value-set parent (widget-get parent :value))
    (widget-setup)))

;;; Initialization.

(when (dbus-ping :session secrets-service 100)

  ;; We must reset all variables, when there is a new instance of the
  ;; "org.freedesktop.secrets" service.
  (dbus-register-signal
   :session dbus-service-dbus dbus-path-dbus
   dbus-interface-dbus "NameOwnerChanged"
   (lambda (&rest args)
     (when secrets-debug (message "Secret Service has changed: %S" args))
     (setq secrets-session-path secrets-empty-path
	   secrets-prompt-signal nil
	   secrets-collection-paths nil))
   secrets-service)

  ;; We want to refresh our cache, when there is a change in
  ;; collections.
  (dbus-register-signal
   :session secrets-service secrets-path
   secrets-interface-service "CollectionCreated"
   'secrets-collection-handler)

  (dbus-register-signal
   :session secrets-service secrets-path
   secrets-interface-service "CollectionDeleted"
   'secrets-collection-handler)

  ;; We shall inform, whether the secret service is enabled on this
  ;; machine.
  (setq secrets-enabled t))

(provide 'secrets)

;;; TODO:

;; * secrets-debug should be structured like auth-source-debug to
;;   prevent leaking sensitive information.  Right now I don't see
;;   anything sensitive though.
;; * Check, whether the dh-ietf1024-aes128-cbc-pkcs7 algorithm can be
;;   used for the transfer of the secrets.  Currently, we use the
;;   plain algorithm.
