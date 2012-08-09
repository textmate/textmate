;;; dbus.el --- Elisp bindings for D-Bus.

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hardware

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

;; This package provides language bindings for the D-Bus API.  D-Bus
;; is a message bus system, a simple way for applications to talk to
;; one another.  See <http://dbus.freedesktop.org/> for details.

;; Low-level language bindings are implemented in src/dbusbind.c.

;;; Code:

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Declare used subroutines and variables.
(declare-function dbus-call-method "dbusbind.c")
(declare-function dbus-call-method-asynchronously "dbusbind.c")
(declare-function dbus-init-bus "dbusbind.c")
(declare-function dbus-method-return-internal "dbusbind.c")
(declare-function dbus-method-error-internal "dbusbind.c")
(declare-function dbus-register-signal "dbusbind.c")
(declare-function dbus-register-method "dbusbind.c")
(declare-function dbus-send-signal "dbusbind.c")
(defvar dbus-debug)
(defvar dbus-registered-objects-table)

;; Pacify byte compiler.
(eval-when-compile
  (require 'cl))

(require 'xml)

(defconst dbus-service-dbus "org.freedesktop.DBus"
  "The bus name used to talk to the bus itself.")

(defconst dbus-path-dbus "/org/freedesktop/DBus"
  "The object path used to talk to the bus itself.")

(defconst dbus-interface-dbus "org.freedesktop.DBus"
  "The interface exported by the object with `dbus-service-dbus' and `dbus-path-dbus'.")

(defconst dbus-interface-peer (concat dbus-interface-dbus ".Peer")
  "The interface for peer objects.")

(defconst dbus-interface-introspectable
  (concat dbus-interface-dbus ".Introspectable")
  "The interface supported by introspectable objects.")

(defconst dbus-interface-properties (concat dbus-interface-dbus ".Properties")
  "The interface for property objects.")

(defconst dbus-service-emacs "org.gnu.Emacs"
  "The well known service name of Emacs.")

(defconst dbus-path-emacs "/org/gnu/Emacs"
  "The object path head used by Emacs.")

(defconst dbus-message-type-invalid 0
  "This value is never a valid message type.")

(defconst dbus-message-type-method-call 1
  "Message type of a method call message.")

(defconst dbus-message-type-method-return 2
  "Message type of a method return message.")

(defconst dbus-message-type-error 3
  "Message type of an error reply message.")

(defconst dbus-message-type-signal 4
  "Message type of a signal message.")

(defmacro dbus-ignore-errors (&rest body)
  "Execute BODY; signal D-Bus error when `dbus-debug' is non-nil.
Otherwise, return result of last form in BODY, or all other errors."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (dbus-error (when dbus-debug (signal (car err) (cdr err))))))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<dbus-ignore-errors\\>"))

(defvar dbus-event-error-hooks nil
  "Functions to be called when a D-Bus error happens in the event handler.
Every function must accept two arguments, the event and the error variable
caught in `condition-case' by `dbus-error'.")


;;; Hash table of registered functions.

(defvar dbus-return-values-table (make-hash-table :test 'equal)
  "Hash table for temporary storing arguments of reply messages.
A key in this hash table is a list (BUS SERIAL).  BUS is either a
Lisp symbol, `:system' or `:session', or a string denoting the
bus address.  SERIAL is the serial number of the reply message.
See `dbus-call-method-non-blocking-handler' and
`dbus-call-method-non-blocking'.")

(defun dbus-list-hash-table ()
  "Returns all registered member registrations to D-Bus.
The return value is a list, with elements of kind (KEY . VALUE).
See `dbus-registered-objects-table' for a description of the
hash table."
  (let (result)
    (maphash
     (lambda (key value) (add-to-list 'result (cons key value) 'append))
     dbus-registered-objects-table)
    result))

(defun dbus-unregister-object (object)
  "Unregister OBJECT from D-Bus.
OBJECT must be the result of a preceding `dbus-register-method',
`dbus-register-property' or `dbus-register-signal' call.  It
returns `t' if OBJECT has been unregistered, `nil' otherwise.

When OBJECT identifies the last method or property, which is
registered for the respective service, Emacs releases its
association to the service from D-Bus."
  ;; Check parameter.
  (unless (and (consp object) (not (null (car object))) (consp (cdr object)))
    (signal 'wrong-type-argument (list 'D-Bus object)))

  ;; Find the corresponding entry in the hash table.
  (let* ((key (car object))
	 (value (cadr object))
	 (bus (car key))
	 (service (car value))
	 (entry (gethash key dbus-registered-objects-table))
	 ret)
    ;; key has the structure (BUS INTERFACE MEMBER).
    ;; value has the structure (SERVICE PATH [HANDLER]).
    ;; entry has the structure ((UNAME SERVICE PATH MEMBER [RULE]) ...).
    ;; MEMBER is either a string (the handler), or a cons cell (a
    ;; property value).  UNAME and property values are not taken into
    ;; account for comparison.

    ;; Loop over the registered functions.
    (dolist (elt entry)
      (when (equal
	     value
	     (butlast (cdr elt) (- (length (cdr elt)) (length value))))
	(setq ret t)
	;; Compute new hash value.  If it is empty, remove it from the
	;; hash table.
	(unless (puthash key (delete elt entry) dbus-registered-objects-table)
	  (remhash key dbus-registered-objects-table))
	;; Remove match rule of signals.
	(let ((rule (nth 4 elt)))
	  (when (stringp rule)
	    (setq service nil) ; We do not need to unregister the service.
	    (dbus-call-method
	     bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
	     "RemoveMatch" rule)))))
    ;; Check, whether there is still a registered function or property
    ;; for the given service.  If not, unregister the service from the
    ;; bus.
    (when service
      (dolist (elt entry)
	(let (found)
	  (maphash
	   (lambda (k v)
	     (dolist (e v)
	       (ignore-errors
		 (when (and (equal bus (car k)) (string-equal service (cadr e)))
		   (setq found t)))))
	   dbus-registered-objects-table)
	  (unless found
	    (dbus-call-method
	     bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
	     "ReleaseName" service)))))
    ;; Return.
    ret))

(defun dbus-unregister-service (bus service)
  "Unregister all objects related to SERVICE from D-Bus BUS.
BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.  SERVICE must be a known service name.

The function returns a keyword, indicating the result of the
operation.  One of the following keywords is returned:

`:released': Service has become the primary owner of the name.

`:non-existent': Service name does not exist on this bus.

`:not-owner': We are neither the primary owner nor waiting in the
queue of this service."

  (maphash
   (lambda (key value)
     (dolist (elt value)
       (ignore-errors
	 (when (and (equal bus (car key)) (string-equal service (cadr elt)))
	   (unless
	       (puthash key (delete elt value) dbus-registered-objects-table)
	     (remhash key dbus-registered-objects-table))))))
   dbus-registered-objects-table)
  (let ((reply (dbus-call-method
		bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
		"ReleaseName" service)))
    (case reply
      (1 :released)
      (2 :non-existent)
      (3 :not-owner)
      (t (signal 'dbus-error (list "Could not unregister service" service))))))

(defun dbus-call-method-non-blocking-handler (&rest args)
  "Handler for reply messages of asynchronous D-Bus message calls.
It calls the function stored in `dbus-registered-objects-table'.
The result will be made available in `dbus-return-values-table'."
  (puthash (list (dbus-event-bus-name last-input-event)
		 (dbus-event-serial-number last-input-event))
	   (if (= (length args) 1) (car args) args)
	   dbus-return-values-table))

(defun dbus-call-method-non-blocking
  (bus service path interface method &rest args)
  "Call METHOD on the D-Bus BUS, but don't block the event queue.
This is necessary for communicating to registered D-Bus methods,
which are running in the same Emacs process.

The arguments are the same as in `dbus-call-method'.

usage: (dbus-call-method-non-blocking
         BUS SERVICE PATH INTERFACE METHOD
         &optional :timeout TIMEOUT &rest ARGS)"

  (let ((key
	 (apply
	  'dbus-call-method-asynchronously
	  bus service path interface method
	  'dbus-call-method-non-blocking-handler args)))
    ;; Wait until `dbus-call-method-non-blocking-handler' has put the
    ;; result into `dbus-return-values-table'.
    (while (eq (gethash key dbus-return-values-table :ignore) :ignore)
      (read-event nil nil 0.1))

    ;; Cleanup `dbus-return-values-table'.  Return the result.
    (prog1
	(gethash key dbus-return-values-table nil)
      (remhash key dbus-return-values-table))))

(defun dbus-name-owner-changed-handler (&rest args)
  "Reapplies all member registrations to D-Bus.
This handler is applied when a \"NameOwnerChanged\" signal has
arrived.  SERVICE is the object name for which the name owner has
been changed.  OLD-OWNER is the previous owner of SERVICE, or the
empty string if SERVICE was not owned yet.  NEW-OWNER is the new
owner of SERVICE, or the empty string if SERVICE loses any name owner.

usage: (dbus-name-owner-changed-handler service old-owner new-owner)"
  (save-match-data
    ;; Check the arguments.  We should silently ignore it when they
    ;; are wrong.
    (if (and (= (length args) 3)
	     (stringp (car args))
	     (stringp (cadr args))
	     (stringp (caddr args)))
	(let ((service (car args))
	      (old-owner (cadr args))
	      (new-owner (caddr args)))
	  ;; Check whether SERVICE is a known name.
	  (when (not (string-match "^:" service))
	    (maphash
	     (lambda (key value)
               (dolist (elt value)
                 ;; key has the structure (BUS INTERFACE MEMBER).
                 ;; elt has the structure (UNAME SERVICE PATH HANDLER).
                 (when (string-equal old-owner (car elt))
                   ;; Remove old key, and add new entry with changed name.
                   (dbus-unregister-object (list key (cdr elt)))
                   ;; Maybe we could arrange the lists a little bit better
                   ;; that we don't need to extract every single element?
                   (dbus-register-signal
                    ;; BUS      SERVICE     PATH
                    (nth 0 key) (nth 1 elt) (nth 2 elt)
                    ;; INTERFACE MEMBER     HANDLER
                    (nth 1 key) (nth 2 key) (nth 3 elt)))))
	     (copy-hash-table dbus-registered-objects-table))))
      ;; The error is reported only in debug mode.
      (when  dbus-debug
	(signal
	 'dbus-error
	 (cons
	  (format "Wrong arguments of %s.NameOwnerChanged" dbus-interface-dbus)
	  args))))))

;; Register the handler.
(when nil ;ignore-errors
  (dbus-register-signal
   :system dbus-service-dbus dbus-path-dbus dbus-interface-dbus
   "NameOwnerChanged" 'dbus-name-owner-changed-handler)
  (dbus-register-signal
   :session dbus-service-dbus dbus-path-dbus dbus-interface-dbus
   "NameOwnerChanged" 'dbus-name-owner-changed-handler))


;;; D-Bus type conversion.

(defun dbus-string-to-byte-array (string)
  "Transforms STRING to list (:array :byte c1 :byte c2 ...).
STRING shall be UTF8 coded."
  (if (zerop (length string))
      '(:array :signature "y")
    (let (result)
      (dolist (elt (string-to-list string) (append '(:array) result))
	(setq result (append result (list :byte elt)))))))

(defun dbus-byte-array-to-string (byte-array)
  "Transforms BYTE-ARRAY into UTF8 coded string.
BYTE-ARRAY must be a list of structure (c1 c2 ...)."
  (apply 'string byte-array))

(defun dbus-escape-as-identifier (string)
  "Escape an arbitrary STRING so it follows the rules for a C identifier.
The escaped string can be used as object path component, interface element
component, bus name component or member name in D-Bus.

The escaping consists of replacing all non-alphanumerics, and the
first character if it's a digit, with an underscore and two
lower-case hex digits:

   \"0123abc_xyz\\x01\\xff\" -> \"_30123abc_5fxyz_01_ff\"

i.e. similar to URI encoding, but with \"_\" taking the role of \"%\",
and a smaller allowed set. As a special case, \"\" is escaped to
\"_\".

Returns the escaped string.  Algorithm taken from
telepathy-glib's `tp-escape-as-identifier'."
  (if (zerop (length string))
      "_"
    (replace-regexp-in-string
     "^[0-9]\\|[^A-Za-z0-9]"
     (lambda (x) (format "_%2x" (aref x 0)))
     string)))

(defun dbus-unescape-from-identifier (string)
  "Retrieve the original string from the encoded STRING.
STRING must have been coded with `dbus-escape-as-identifier'"
  (if (string-equal string "_")
      ""
    (replace-regexp-in-string
     "_.."
     (lambda (x) (format "%c" (string-to-number (substring x 1) 16)))
     string)))


;;; D-Bus events.

(defun dbus-check-event (event)
  "Checks whether EVENT is a well formed D-Bus event.
EVENT is a list which starts with symbol `dbus-event':

  (dbus-event BUS TYPE SERIAL SERVICE PATH INTERFACE MEMBER HANDLER &rest ARGS)

BUS identifies the D-Bus the message is coming from.  It is
either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.  TYPE is the D-Bus message type which
has caused the event, SERIAL is the serial number of the received
D-Bus message.  SERVICE and PATH are the unique name and the
object path of the D-Bus object emitting the message.  INTERFACE
and MEMBER denote the message which has been sent.  HANDLER is
the function which has been registered for this message.  ARGS
are the arguments passed to HANDLER, when it is called during
event handling in `dbus-handle-event'.

This function raises a `dbus-error' signal in case the event is
not well formed."
  (when dbus-debug (message "DBus-Event %s" event))
  (unless (and (listp event)
	       (eq (car event) 'dbus-event)
	       ;; Bus symbol.
	       (or (symbolp (nth 1 event))
		   (stringp (nth 1 event)))
	       ;; Type.
	       (and (natnump (nth 2 event))
		    (< dbus-message-type-invalid (nth 2 event)))
	       ;; Serial.
	       (natnump (nth 3 event))
	       ;; Service.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 4 event)))
	       ;; Object path.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 5 event)))
	       ;; Interface.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 6 event)))
	       ;; Member.
	       (or (= dbus-message-type-method-return (nth 2 event))
		   (= dbus-message-type-error (nth 2 event))
		   (stringp (nth 7 event)))
	       ;; Handler.
	       (functionp (nth 8 event)))
    (signal 'dbus-error (list "Not a valid D-Bus event" event))))

;;;###autoload
(defun dbus-handle-event (event)
  "Handle events from the D-Bus.
EVENT is a D-Bus event, see `dbus-check-event'.  HANDLER, being
part of the event, is called with arguments ARGS.
If the HANDLER returns a `dbus-error', it is propagated as return message."
  (interactive "e")
  (condition-case err
      (let (result)
	;; We ignore not well-formed events.
	(dbus-check-event event)
	;; Error messages must be propagated.
	(when (= dbus-message-type-error (nth 2 event))
	  (signal 'dbus-error (nthcdr 9 event)))
	;; Apply the handler.
	(setq result (apply (nth 8 event) (nthcdr 9 event)))
	;; Return a message when it is a message call.
	(when (= dbus-message-type-method-call (nth 2 event))
	  (dbus-ignore-errors
	    (if (eq result :ignore)
		(dbus-method-return-internal
		 (nth 1 event) (nth 3 event) (nth 4 event))
	      (apply 'dbus-method-return-internal
		     (nth 1 event) (nth 3 event) (nth 4 event)
		     (if (consp result) result (list result)))))))
    ;; Error handling.
    (dbus-error
     ;; Return an error message when it is a message call.
     (when (= dbus-message-type-method-call (nth 2 event))
       (dbus-ignore-errors
	 (dbus-method-error-internal
	  (nth 1 event) (nth 3 event) (nth 4 event) (cadr err))))
     ;; Propagate D-Bus error messages.
     (run-hook-with-args 'dbus-event-error-hooks event err)
     (when (or dbus-debug (= dbus-message-type-error (nth 2 event)))
       (signal (car err) (cdr err))))))

(defun dbus-event-bus-name (event)
  "Return the bus name the event is coming from.
The result is either a Lisp symbol, `:system' or `:session', or a
string denoting the bus address.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function raises a `dbus-error' signal
in case the event is not well formed."
  (dbus-check-event event)
  (nth 1 event))

(defun dbus-event-message-type (event)
  "Return the message type of the corresponding D-Bus message.
The result is a number.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function raises a `dbus-error' signal
in case the event is not well formed."
  (dbus-check-event event)
  (nth 2 event))

(defun dbus-event-serial-number (event)
  "Return the serial number of the corresponding D-Bus message.
The result is a number.  The serial number is needed for
generating a reply message.  EVENT is a D-Bus event, see
`dbus-check-event'.  This function raises a `dbus-error' signal
in case the event is not well formed."
  (dbus-check-event event)
  (nth 3 event))

(defun dbus-event-service-name (event)
  "Return the name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 4 event))

(defun dbus-event-path-name (event)
  "Return the object path of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 5 event))

(defun dbus-event-interface-name (event)
  "Return the interface name of the D-Bus object the event is coming from.
The result is a string.  EVENT is a D-Bus event, see `dbus-check-event'.
This function raises a `dbus-error' signal in case the event is
not well formed."
  (dbus-check-event event)
  (nth 6 event))

(defun dbus-event-member-name (event)
  "Return the member name the event is coming from.
It is either a signal name or a method name. The result is a
string.  EVENT is a D-Bus event, see `dbus-check-event'.  This
function raises a `dbus-error' signal in case the event is not
well formed."
  (dbus-check-event event)
  (nth 7 event))


;;; D-Bus registered names.

(defun dbus-list-activatable-names (&optional bus)
  "Return the D-Bus service names which can be activated as list.
If BUS is left nil, `:system' is assumed.  The result is a list
of strings, which is `nil' when there are no activatable service
names at all."
  (dbus-ignore-errors
    (dbus-call-method
     (or bus :system) dbus-service-dbus
     dbus-path-dbus dbus-interface-dbus "ListActivatableNames")))

(defun dbus-list-names (bus)
  "Return the service names registered at D-Bus BUS.
The result is a list of strings, which is `nil' when there are no
registered service names at all.  Well known names are strings
like \"org.freedesktop.DBus\".  Names starting with \":\" are
unique names for services."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus "ListNames")))

(defun dbus-list-known-names (bus)
  "Retrieve all services which correspond to a known name in BUS.
A service has a known name if it doesn't start with \":\"."
  (let (result)
    (dolist (name (dbus-list-names bus) result)
      (unless (string-equal ":" (substring name 0 1))
	(add-to-list 'result name 'append)))))

(defun dbus-list-queued-owners (bus service)
  "Return the unique names registered at D-Bus BUS and queued for SERVICE.
The result is a list of strings, or `nil' when there are no
queued name owners service names at all."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus
     dbus-interface-dbus "ListQueuedOwners" service)))

(defun dbus-get-name-owner (bus service)
  "Return the name owner of SERVICE registered at D-Bus BUS.
The result is either a string, or `nil' if there is no name owner."
  (dbus-ignore-errors
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus
     dbus-interface-dbus "GetNameOwner" service)))

(defun dbus-ping (bus service &optional timeout)
  "Check whether SERVICE is registered for D-Bus BUS.
TIMEOUT, a nonnegative integer, specifies the maximum number of
milliseconds `dbus-ping' must return.  The default value is 25,000.

Note, that this autoloads SERVICE if it is not running yet.  If
it shall be checked whether SERVICE is already running, one shall
apply

  \(member service \(dbus-list-known-names bus))"
  ;; "Ping" raises a D-Bus error if SERVICE does not exist.
  ;; Otherwise, it returns silently with `nil'.
  (condition-case nil
      (not
       (if (natnump timeout)
	   (dbus-call-method
	    bus service dbus-path-dbus dbus-interface-peer
	    "Ping" :timeout timeout)
	 (dbus-call-method
	  bus service dbus-path-dbus dbus-interface-peer "Ping")))
    (dbus-error nil)))


;;; D-Bus introspection.

(defun dbus-introspect (bus service path)
  "Return all interfaces and sub-nodes of SERVICE,
registered at object path PATH at bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.  SERVICE must be a known service name,
and PATH must be a valid object path.  The last two parameters
are strings.  The result, the introspection data, is a string in
XML format."
  ;; We don't want to raise errors.  `dbus-call-method-non-blocking'
  ;; is used, because the handler can be registered in our Emacs
  ;; instance; caller an callee would block each other.
  (dbus-ignore-errors
    (funcall
     (if noninteractive 'dbus-call-method 'dbus-call-method-non-blocking)
     bus service path dbus-interface-introspectable "Introspect")))

(defun dbus-introspect-xml (bus service path)
  "Return the introspection data of SERVICE in D-Bus BUS at object path PATH.
The data are a parsed list.  The root object is a \"node\",
representing the object path PATH.  The root object can contain
\"interface\" and further \"node\" objects."
  ;; We don't want to raise errors.
  (xml-node-name
   (ignore-errors
     (with-temp-buffer
       (insert (dbus-introspect bus service path))
       (xml-parse-region (point-min) (point-max))))))

(defun dbus-introspect-get-attribute (object attribute)
  "Return the ATTRIBUTE value of D-Bus introspection OBJECT.
ATTRIBUTE must be a string according to the attribute names in
the D-Bus specification."
  (xml-get-attribute-or-nil object (intern attribute)))

(defun dbus-introspect-get-node-names (bus service path)
  "Return all node names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings.  The node names stand for further
object paths of the D-Bus service."
  (let ((object (dbus-introspect-xml bus service path))
	result)
    (dolist (elt (xml-get-children object 'node) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-all-nodes (bus service path)
  "Return all node names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings, which are further object paths of SERVICE."
  (let ((result (list path)))
    (dolist (elt
             (dbus-introspect-get-node-names bus service path)
             result)
      (setq elt (expand-file-name elt path))
      (setq result
            (append result (dbus-introspect-get-all-nodes bus service elt))))))

(defun dbus-introspect-get-interface-names (bus service path)
  "Return all interface names of SERVICE in D-Bus BUS at object path PATH.
It returns a list of strings.

There will be always the default interface
\"org.freedesktop.DBus.Introspectable\".  Another default
interface is \"org.freedesktop.DBus.Properties\".  If present,
\"interface\" objects can also have \"property\" objects as
children, beside \"method\" and \"signal\" objects."
  (let ((object (dbus-introspect-xml bus service path))
	result)
    (dolist (elt (xml-get-children object 'interface) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-interface (bus service path interface)
  "Return the INTERFACE of SERVICE in D-Bus BUS at object path PATH.
The return value is an XML object.  INTERFACE must be a string,
element of the list returned by `dbus-introspect-get-interface-names'.
The resulting \"interface\" object can contain \"method\", \"signal\",
\"property\" and \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-xml bus service path) 'interface)))
    (while (and elt
		(not (string-equal
		      interface
		      (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-method-names (bus service path interface)
  "Return a list of strings of all method names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (let ((object (dbus-introspect-get-interface bus service path interface))
	result)
    (dolist (elt (xml-get-children object 'method) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-method (bus service path interface method)
  "Return method METHOD of interface INTERFACE as XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
METHOD must be a string, element of the list returned by
`dbus-introspect-get-method-names'.  The resulting \"method\"
object can contain \"arg\" and \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-get-interface bus service path interface)
	      'method)))
    (while (and elt
		(not (string-equal
		      method (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-signal-names (bus service path interface)
  "Return a list of strings of all signal names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (let ((object (dbus-introspect-get-interface bus service path interface))
	result)
    (dolist (elt (xml-get-children object 'signal) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-signal (bus service path interface signal)
  "Return signal SIGNAL of interface INTERFACE as XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
SIGNAL must be a string, element of the list returned by
`dbus-introspect-get-signal-names'.  The resulting \"signal\"
object can contain \"arg\" and \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-get-interface bus service path interface)
	      'signal)))
    (while (and elt
		(not (string-equal
		      signal (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-property-names (bus service path interface)
  "Return a list of strings of all property names of INTERFACE.
SERVICE is a service of D-Bus BUS at object path PATH."
  (let ((object (dbus-introspect-get-interface bus service path interface))
	result)
    (dolist (elt (xml-get-children object 'property) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-property (bus service path interface property)
  "This function returns PROPERTY of INTERFACE as XML object.
It must be located at SERVICE in D-Bus BUS at object path PATH.
PROPERTY must be a string, element of the list returned by
`dbus-introspect-get-property-names'.  The resulting PROPERTY
object can contain \"annotation\" children."
  (let ((elt (xml-get-children
	      (dbus-introspect-get-interface bus service path interface)
	      'property)))
    (while (and elt
		(not (string-equal
		      property
		      (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-annotation-names
  (bus service path interface &optional name)
  "Return all annotation names as list of strings.
If NAME is `nil', the annotations are children of INTERFACE,
otherwise NAME must be a \"method\", \"signal\", or \"property\"
object, where the annotations belong to."
  (let ((object
	 (if name
	     (or (dbus-introspect-get-method bus service path interface name)
		 (dbus-introspect-get-signal bus service path interface name)
		 (dbus-introspect-get-property bus service path interface name))
	   (dbus-introspect-get-interface bus service path interface)))
	result)
    (dolist (elt (xml-get-children object 'annotation) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-annotation
  (bus service path interface name annotation)
  "Return ANNOTATION as XML object.
If NAME is `nil', ANNOTATION is a child of INTERFACE, otherwise
NAME must be the name of a \"method\", \"signal\", or
\"property\" object, where the ANNOTATION belongs to."
  (let ((elt (xml-get-children
	      (if name
		  (or (dbus-introspect-get-method
		       bus service path interface name)
		      (dbus-introspect-get-signal
		       bus service path interface name)
		      (dbus-introspect-get-property
		       bus service path interface name))
		(dbus-introspect-get-interface bus service path interface))
	      'annotation)))
    (while (and elt
		(not (string-equal
		      annotation
		      (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-argument-names (bus service path interface name)
  "Return a list of all argument names as list of strings.
NAME must be a \"method\" or \"signal\" object.

Argument names are optional, the function can return `nil'
therefore, even if the method or signal has arguments."
  (let ((object
	 (or (dbus-introspect-get-method bus service path interface name)
	     (dbus-introspect-get-signal bus service path interface name)))
	result)
    (dolist (elt (xml-get-children object 'arg) result)
      (add-to-list
       'result (dbus-introspect-get-attribute elt "name") 'append))))

(defun dbus-introspect-get-argument (bus service path interface name arg)
  "Return argument ARG as XML object.
NAME must be a \"method\" or \"signal\" object.  ARG must be a string,
element of the list returned by `dbus-introspect-get-argument-names'."
  (let ((elt (xml-get-children
	      (or (dbus-introspect-get-method bus service path interface name)
		  (dbus-introspect-get-signal bus service path interface name))
	      'arg)))
    (while (and elt
		(not (string-equal
		      arg (dbus-introspect-get-attribute (car elt) "name"))))
      (setq elt (cdr elt)))
    (car elt)))

(defun dbus-introspect-get-signature
  (bus service path interface name &optional direction)
  "Return signature of a `method' or `signal', represented by NAME, as string.
If NAME is a `method', DIRECTION can be either \"in\" or \"out\".
If DIRECTION is `nil', \"in\" is assumed.

If NAME is a `signal', and DIRECTION is non-`nil', DIRECTION must
be \"out\"."
  ;; For methods, we use "in" as default direction.
  (let ((object (or (dbus-introspect-get-method
		     bus service path interface name)
		    (dbus-introspect-get-signal
		     bus service path interface name))))
    (when (and (string-equal
		"method" (dbus-introspect-get-attribute object "name"))
	       (not (stringp direction)))
      (setq direction "in"))
    ;; In signals, no direction is given.
    (when (string-equal "signal" (dbus-introspect-get-attribute object "name"))
      (setq direction nil))
    ;; Collect the signatures.
    (mapconcat
     (lambda (x)
       (let ((arg (dbus-introspect-get-argument
                   bus service path interface name x)))
         (if (or (not (stringp direction))
                 (string-equal
                  direction
                  (dbus-introspect-get-attribute arg "direction")))
             (dbus-introspect-get-attribute arg "type")
           "")))
     (dbus-introspect-get-argument-names bus service path interface name)
     "")))


;;; D-Bus properties.

(defun dbus-get-property (bus service path interface property)
  "Return the value of PROPERTY of INTERFACE.
It will be checked at BUS, SERVICE, PATH.  The result can be any
valid D-Bus value, or `nil' if there is no PROPERTY."
  (dbus-ignore-errors
    ;; "Get" returns a variant, so we must use the `car'.
    (car
     (funcall
      (if noninteractive 'dbus-call-method 'dbus-call-method-non-blocking)
      bus service path dbus-interface-properties
      "Get" :timeout 500 interface property))))

(defun dbus-set-property (bus service path interface property value)
  "Set value of PROPERTY of INTERFACE to VALUE.
It will be checked at BUS, SERVICE, PATH.  When the value has
been set successful, the result is VALUE.  Otherwise, `nil' is
returned."
  (dbus-ignore-errors
    ;; "Set" requires a variant.
    (funcall
     (if noninteractive 'dbus-call-method 'dbus-call-method-non-blocking)
     bus service path dbus-interface-properties
     "Set" :timeout 500 interface property (list :variant value))
    ;; Return VALUE.
    (dbus-get-property bus service path interface property)))

(defun dbus-get-all-properties (bus service path interface)
  "Return all properties of INTERFACE at BUS, SERVICE, PATH.
The result is a list of entries.  Every entry is a cons of the
name of the property, and its value.  If there are no properties,
`nil' is returned."
  (dbus-ignore-errors
    ;; "GetAll" returns "a{sv}".
    (let (result)
      (dolist (dict
	       (funcall
		(if noninteractive
		    'dbus-call-method
		  'dbus-call-method-non-blocking)
		bus service path dbus-interface-properties
		"GetAll" :timeout 500 interface)
	       result)
	(add-to-list 'result (cons (car dict) (caadr dict)) 'append)))))

(defun dbus-register-property
  (bus service path interface property access value
   &optional emits-signal dont-register-service)
  "Register property PROPERTY on the D-Bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name of the D-Bus.  It must be a
known name (See discussion of DONT-REGISTER-SERVICE below).

PATH is the D-Bus object path SERVICE is registered (See
discussion of DONT-REGISTER-SERVICE below).  INTERFACE is the
name of the interface used at PATH, PROPERTY is the name of the
property of INTERFACE.  ACCESS indicates, whether the property
can be changed by other services via D-Bus.  It must be either
the symbol `:read' or `:readwrite'.  VALUE is the initial value
of the property, it can be of any valid type (see
`dbus-call-method' for details).

If PROPERTY already exists on PATH, it will be overwritten.  For
properties with access type `:read' this is the only way to
change their values.  Properties with access type `:readwrite'
can be changed by `dbus-set-property'.

The interface \"org.freedesktop.DBus.Properties\" is added to
PATH, including a default handler for the \"Get\", \"GetAll\" and
\"Set\" methods of this interface.  When EMITS-SIGNAL is non-nil,
the signal \"PropertiesChanged\" is sent when the property is
changed by `dbus-set-property'.

When DONT-REGISTER-SERVICE is non-nil, the known name SERVICE is
not registered.  This means that other D-Bus clients have no way
of noticing the newly registered property.  When interfaces are
constructed incrementally by adding single methods or properties
at a time, DONT-REGISTER-SERVICE can be used to prevent other
clients from discovering the still incomplete interface."
  (unless (member access '(:read :readwrite))
    (signal 'dbus-error (list "Access type invalid" access)))

  ;; Register SERVICE.
  (unless (or dont-register-service
	      (member service (dbus-list-names bus)))
    (dbus-call-method
     bus dbus-service-dbus dbus-path-dbus dbus-interface-dbus
     "RequestName" service 0))

  ;; Add handlers for the three property-related methods.
  (dbus-register-method
   bus service path dbus-interface-properties "Get"
   'dbus-property-handler 'dont-register)
  (dbus-register-method
   bus service path dbus-interface-properties "GetAll"
   'dbus-property-handler 'dont-register)
  (dbus-register-method
   bus service path dbus-interface-properties "Set"
   'dbus-property-handler 'dont-register)

  ;; Register the name SERVICE with BUS.
  (unless dont-register-service
    (dbus-register-service bus service))

  ;; Send the PropertiesChanged signal.
  (when emits-signal
    (dbus-send-signal
     bus service path dbus-interface-properties "PropertiesChanged"
     (list (list :dict-entry property (list :variant value)))
     '(:array)))

  ;; Create a hash table entry.  We use nil for the unique name,
  ;; because the property might be accessed from anybody.
  (let ((key (list bus interface property))
	(val
	 (list
	  (list
	   nil service path
	   (cons
	    (if emits-signal (list access :emits-signal) (list access))
	    value)))))
    (puthash key val dbus-registered-objects-table)

    ;; Return the object.
    (list key (list service path))))

(defun dbus-property-handler (&rest args)
  "Default handler for the \"org.freedesktop.DBus.Properties\" interface.
It will be registered for all objects created by `dbus-register-object'."
  (let ((bus (dbus-event-bus-name last-input-event))
	(service (dbus-event-service-name last-input-event))
	(path (dbus-event-path-name last-input-event))
	(method (dbus-event-member-name last-input-event))
	(interface (car args))
	(property (cadr args)))
    (cond
     ;; "Get" returns a variant.
     ((string-equal method "Get")
      (let ((entry (gethash (list bus interface property)
			    dbus-registered-objects-table)))
	(when (string-equal path (nth 2 (car entry)))
	  (list (list :variant (cdar (last (car entry))))))))

     ;; "Set" expects a variant.
     ((string-equal method "Set")
      (let* ((value (caar (cddr args)))
	     (entry (gethash (list bus interface property)
			     dbus-registered-objects-table))
	     ;; The value of the hash table is a list; in case of
	     ;; properties it contains just one element (UNAME SERVICE
	     ;; PATH OBJECT).  OBJECT is a cons cell of a list, which
	     ;; contains a list of annotations (like :read,
	     ;; :read-write, :emits-signal), and the value of the
	     ;; property.
	     (object (car (last (car entry)))))
	(unless (consp object)
	  (signal 'dbus-error
		  (list "Property not registered at path" property path)))
	(unless (member :readwrite (car object))
	  (signal 'dbus-error
		  (list "Property not writable at path" property path)))
	(puthash (list bus interface property)
		 (list (append (butlast (car entry))
			       (list (cons (car object) value))))
		 dbus-registered-objects-table)
	;; Send the "PropertiesChanged" signal.
	(when (member :emits-signal (car object))
	  (dbus-send-signal
	   bus service path dbus-interface-properties "PropertiesChanged"
	   (list (list :dict-entry property (list :variant value)))
	   '(:array)))
	;; Return empty reply.
	:ignore))

     ;; "GetAll" returns "a{sv}".
     ((string-equal method "GetAll")
      (let (result)
	(maphash
	 (lambda (key val)
	   (when (and (equal (butlast key) (list bus interface))
		      (string-equal path (nth 2 (car val)))
		      (not (functionp (car (last (car val))))))
	     (add-to-list
	      'result
	      (list :dict-entry
		    (car (last key))
		    (list :variant (cdar (last (car val))))))))
	 dbus-registered-objects-table)
	;; Return the result, or an empty array.
	(list :array (or result '(:signature "{sv}"))))))))

 
;; Initialize :system and :session buses.  This adds their file
;; descriptors to input_wait_mask, in order to detect incoming
;; messages immediately.
(when (featurep 'dbusbind)
  (dbus-ignore-errors
    (dbus-init-bus :system)
    (dbus-init-bus :session)))

(provide 'dbus)

;;; dbus.el ends here
