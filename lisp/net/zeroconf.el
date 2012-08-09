;;; zeroconf.el --- Service browser using Avahi.

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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

;; This package provides an interface to the Avahi, the zeroconf
;; daemon under GNU/Linux.  The communication mean with Avahi is
;; D-Bus.

;; In order to activate this package, you must add the following code
;; into your .emacs:

;;   (require 'zeroconf)
;;   (zeroconf-init "dns-sd.org")

;; "dns-sd.org" is an example the domain you wish to resolve services
;; for.  It can also be nil or "", which means the default local
;; domain "local".

;; The `zeroconf-init' function installs several handlers, which are
;; activated by D-Bus signals sent from the Avahi daemon.
;; Immediately, when a service is added or removed in the domain, a
;; corresponding handler in Emacs is called.

;; Service Discovery
;; -----------------

;; The main purpose of zeroconf is service discovery.  This means,
;; that services are detected as soon as they appear or disappear in a
;; given domain.  A service is offered by a network device.  It is
;; assigned to a service type.

;; In order to see all offered service types of the initialized
;; domain, you can call

;;   (zeroconf-list-service-types)

;; Service types are described at <http://www.dns-sd.org/ServiceTypes.html>.
;; Detected services for a given service type, let's say "_ipp._tcp",
;; are listed by

;;   (zeroconf-list-services "_ipp._tcp")

;; It is possible to register an own handler (function) to be called
;; when a service has been added or removed in the domain.  The
;; service type "_ipp._tcp" is used for printer services supporting
;; the Internet Printing Protocol.

;;   (defun my-add-printer (service)
;;     (message "Printer `%s' detected" (zeroconf-service-name service)))

;;   (defun my-remove-printer (service)
;;     (message "Printer `%s' removed" (zeroconf-service-name service)))

;;   (zeroconf-service-add-hook "_ipp._tcp" :new     'my-add-printer)
;;   (zeroconf-service-add-hook "_ipp._tcp" :removed 'my-remove-printer)

;; There are several functions returning information about a service,
;; see the doc string of `zeroconf-service-add-hook'.

;; Service Publishing
;; ------------------

;; The function `zeroconf-publish-service' publishes a new service to
;; the Avahi daemon.  Although the domain, where to the service is
;; published, can be specified by this function, it is usually the
;; default domain "local" (also written as nil or "").

;;   (zeroconf-publish-service
;;    "Example service" ;; Service name.
;;    "_example._tcp"   ;; Service type.
;;    nil               ;; Default domain ("local").
;;    nil               ;; Default host (concat (getenv "HOST") ".local").
;;    111               ;; Port number of the host, the service is offered.
;;    "1.2.3.4"         ;; IPv4 address of the host.
;;    '("version=1.0"   ;; TXT fields describing the service.
;;      "abc=456"))

;; The lifetime of a published service is the lifetime of Emacs.

;;; Code:

;;  Pacify byte-compiler.  D-Bus support in the Emacs core can be
;; disabled with configuration option "--without-dbus".  Declare used
;; subroutines and variables of `dbus' therefore.
(eval-when-compile
  (require 'cl))

(declare-function dbus-call-method "dbusbind.c")
(declare-function dbus-register-signal "dbusbind.c")
(defvar dbus-debug)

(require 'dbus)

(defvar zeroconf-debug nil
  "Write messages during service discovery")

(defconst zeroconf-service-avahi "org.freedesktop.Avahi"
  "The D-Bus name used to talk to Avahi.")

(defconst zeroconf-path-avahi "/"
  "The D-Bus root object path used to talk to Avahi.")

(defvar zeroconf-path-avahi-service-type-browser nil
  "The D-Bus object path used to talk to the Avahi service type browser.")

(defvar zeroconf-path-avahi-service-browser-hash (make-hash-table :test 'equal)
  "The D-Bus object paths used to talk to the Avahi service browser.")

(defvar zeroconf-path-avahi-service-resolver-hash (make-hash-table :test 'equal)
  "The D-Bus object paths used to talk to the Avahi service resolver.")

;; Methods: "Free", "Commit", "Reset", "GetState", "IsEmpty",
;; "AddService", "AddServiceSubtype", "UpdateServiceTxt", "AddAddress"
;; and "AddRecord".
;; Signals: "StateChanged".
(defconst zeroconf-interface-avahi-entry-group
  (concat zeroconf-service-avahi ".EntryGroup")
  "The D-Bus entry group interface exported by Avahi.")

;; Methods: "GetVersionString", "GetAPIVersion", "GetHostName",
;; "SetHostName", "GetHostNameFqdn", "GetDomainName",
;; "IsNSSSupportAvailable", "GetState", "GetLocalServiceCookie",
;; "GetAlternativeHostName", "GetAlternativeServiceName",
;; "GetNetworkInterfaceNameByIndex", "GetNetworkInterfaceIndexByName",
;; "ResolveHostName", "ResolveAddress", "ResolveService",
;; "EntryGroupNew", "DomainBrowserNew", "ServiceTypeBrowserNew",
;; "ServiceBrowserNew", "ServiceResolverNew", "HostNameResolverNew",
;; "AddressResolverNew" and "RecordBrowserNew".
;; Signals: "StateChanged".
(defconst zeroconf-interface-avahi-server
  (concat zeroconf-service-avahi ".Server")
  "The D-Bus server interface exported by Avahi.")

;; Methods: "Free".
;; Signals: "ItemNew", "ItemRemove", "CacheExhausted", "AllForNow" and
;; "Failure".
(defconst zeroconf-interface-avahi-service-type-browser
  (concat zeroconf-service-avahi ".ServiceTypeBrowser")
  "The D-Bus service type browser interface exported by Avahi.")

;; Methods: "Free".
;; Signals: "ItemNew", "ItemRemove", "CacheExhausted", "AllForNow" and
;; "Failure".
(defconst zeroconf-interface-avahi-service-browser
  (concat zeroconf-service-avahi ".ServiceBrowser")
  "The D-Bus service browser interface exported by Avahi.")

;; Methods: "Free".
;; Available signals are "Found" and "Failure".
(defconst zeroconf-interface-avahi-service-resolver
  (concat zeroconf-service-avahi ".ServiceResolver")
  "The D-Bus service resolver interface exported by Avahi.")

(defconst zeroconf-avahi-interface-unspec -1
  "Wildcard Avahi interface spec.")

(defconst zeroconf-avahi-protocol-unspec -1
  "Wildcard Avahi protocol spec.")

(defconst zeroconf-avahi-protocol-inet4 0
  "Avahi INET4 address protocol family.")

(defconst zeroconf-avahi-protocol-inet6 1
  "Avahi INET6 address protocol family.")

(defconst zeroconf-avahi-domain-unspec ""
  "Empty Avahi domain.")

(defvar zeroconf-avahi-current-domain zeroconf-avahi-domain-unspec
  "Domain name services are resolved for.")

(defconst zeroconf-avahi-flags-unspec 0
  "No Avahi flags.")


;;; Services retrieval.

(defvar zeroconf-services-hash (make-hash-table :test 'equal)
  "Hash table of discovered Avahi services.

The key of an entry is the concatenation of the service name and
service type of a discovered service.  The value is the service
itself.  The format of a service is

  \(INTERFACE PROTOCOL NAME TYPE DOMAIN FLAGS\)

The INTERFACE is a number, which represents the network interface
the service is located at.  The corresponding network interface
name, like \"eth0\", can be retrieved with the function
`zeroconf-get-interface-name'.

PROTOCOL describes the used network protocol family the service
can be accessed.  `zeroconf-avahi-protocol-inet4' means INET4,
`zeroconf-avahi-protocol-inet6' means INET6.  An unspecified
protocol family is coded with `zeroconf-avahi-protocol-unspec'.

NAME is the string the service is known at Avahi.  A service can
be known under the same name for different service types.

Each TYPE stands for a discovered service type of Avahi.  The
format is described in RFC 2782.  It is of the form

  \"_APPLICATION-PROTOCOL._TRANSPORT-PROTOCOL\".

TRANSPORT-PROTOCOL must be either \"tcp\" or \"udp\".
APPLICATION-PROTOCOL must be a protocol name as specified in URL
`http://www.dns-sd.org/ServiceTypes.html'.  Typical service types
are \"_workstation._tcp\" or \"_printer._tcp\".

DOMAIN is the domain name the service is registered in, like \"local\".

FLAGS, an integer, is used inside Avahi.  When publishing a
service (see `zeroconf-publish-service', the flag 0 is used.")

(defvar zeroconf-resolved-services-hash (make-hash-table :test 'equal)
  "Hash table of resolved Avahi services.
The key of an entry is the concatenation of the service name and
service type of a resolved service.  The value is the service
itself.  The format of a service is

  \(INTERFACE PROTOCOL NAME TYPE DOMAIN HOST APROTOCOL ADDRESS PORT TXT FLAGS\)

INTERFACE, PROTOCOL, NAME, TYPE, DOMAIN and FLAGS have the same
meaning as in `zeroconf-services-hash'.

HOST is the host name the service is registered.  It is a fully
qualified name, i.e., it contains DOMAIN.

APROTOCOL stands for the network protocol family ADDRESS is
encoded (`zeroconf-avahi-protocol-inet4' means INET4,
`zeroconf-avahi-protocol-inet6' means INET6).  It can be
different from PROTOCOL, when an address resolution has been
requested for another protocol family but the default one.

ADDRESS is the service address, encoded according to the
APROTOCOL network protocol family.  PORT is the corresponding
port the service can be reached on ADDRESS.

TXT is an array of strings, describing additional attributes of
the service.  Usually, every string is a key=value pair.  The
supported keys depend on the service type.")

(defun zeroconf-list-service-names ()
  "Returns all discovered Avahi service names as list."
  (let (result)
    (maphash
     (lambda (key value) (add-to-list 'result (zeroconf-service-name value)))
     zeroconf-services-hash)
    result))

(defun zeroconf-list-service-types ()
  "Returns all discovered Avahi service types as list."
  (let (result)
    (maphash
     (lambda (key value) (add-to-list 'result (zeroconf-service-type value)))
     zeroconf-services-hash)
    result))

(defun zeroconf-list-services (type)
  "Returns all discovered Avahi services for a given service type TYPE.
The service type is one of the returned values of
`zeroconf-list-service-types'.  The return value is a list
\(SERVICE1 SERVICE2 ...\).  See `zeroconf-services-hash' for the
format of SERVICE."
  (let (result)
    (maphash
     (lambda (key value)
       (when (equal type (zeroconf-service-type value))
	 (add-to-list 'result value)))
     zeroconf-services-hash)
    result))

(defvar zeroconf-service-added-hooks-hash (make-hash-table :test 'equal)
  "Hash table of hooks for newly added services.
The key of an entry is a service type.")

(defvar zeroconf-service-removed-hooks-hash (make-hash-table :test 'equal)
  "Hash table of hooks for removed services.
The key of an entry is a service type.")

(defun zeroconf-service-add-hook (type event function)
  "Add FUNCTION to the hook of service type TYPE.

EVENT must be either :new or :removed, indicating whether
FUNCTION shall be called when a new service has been newly
detected, or removed.

FUNCTION must accept one argument SERVICE, which identifies the
new service.  Initially, when EVENT is :new, FUNCTION is called
for all already detected services of service type TYPE.

The attributes of SERVICE can be retrieved via the functions

  `zeroconf-service-interface'
  `zeroconf-service-protocol'
  `zeroconf-service-name'
  `zeroconf-service-type'
  `zeroconf-service-domain'
  `zeroconf-service-flags'
  `zeroconf-service-host'
  `zeroconf-service-aprotocol'
  `zeroconf-service-address'
  `zeroconf-service-port'
  `zeroconf-service-txt'"

  (cond
   ((equal event :new)
    (let ((l-hook (gethash type zeroconf-service-added-hooks-hash nil)))
      (add-hook 'l-hook function)
      (puthash type l-hook zeroconf-service-added-hooks-hash)
      (dolist (service (zeroconf-list-services type))
	(funcall function service))))
   ((equal event :removed)
    (let ((l-hook (gethash type zeroconf-service-removed-hooks-hash nil)))
      (add-hook 'l-hook function)
      (puthash type l-hook zeroconf-service-removed-hooks-hash)))
   (t (error "EVENT must be either `:new' or `:removed'"))))

(defun zeroconf-service-remove-hook (type event function)
  "Remove FUNCTION from the hook of service type TYPE.

EVENT must be either :new or :removed and has to match the event
type used when registering FUNCTION."
  (let* ((table (cond
		 ((equal event :new)
		  zeroconf-service-added-hooks-hash)
		 ((equal event :removed)
		  zeroconf-service-removed-hooks-hash)
		 (t (error "EVENT must be either `:new' or `:removed'"))))
	 (l-hook (gethash type table nil)))
    (remove-hook 'l-hook function)
    (if l-hook
	(puthash type l-hook table)
      (remhash type table))))

(defun zeroconf-get-host ()
  "Returns the local host name as string."
  (dbus-call-method
   :system zeroconf-service-avahi zeroconf-path-avahi
   zeroconf-interface-avahi-server "GetHostName"))

(defun zeroconf-get-domain ()
  "Returns the domain name as string."
  (dbus-call-method
   :system zeroconf-service-avahi zeroconf-path-avahi
   zeroconf-interface-avahi-server "GetDomainName"))

(defun zeroconf-get-host-domain ()
  "Returns the local host name FQDN as string."
  (dbus-call-method
   :system zeroconf-service-avahi zeroconf-path-avahi
   zeroconf-interface-avahi-server "GetHostNameFqdn"))

(defun zeroconf-get-interface-name (number)
  "Return the interface name of internal interface NUMBER."
  (dbus-call-method
   :system zeroconf-service-avahi zeroconf-path-avahi
   zeroconf-interface-avahi-server "GetNetworkInterfaceNameByIndex"
   :int32 number))

(defun zeroconf-get-interface-number (name)
  "Return the internal interface number of interface NAME."
  (dbus-call-method
   :system zeroconf-service-avahi zeroconf-path-avahi
   zeroconf-interface-avahi-server "GetNetworkInterfaceIndexByName"
   name))

(defun zeroconf-get-service (name type)
  "Return the service description of service NAME as list.
NAME must be a string.  The service must be of service type
TYPE. The resulting list has the format

  \(INTERFACE PROTOCOL NAME TYPE DOMAIN FLAGS\)."
  ;; Due to the service browser, all known services are kept in
  ;; `zeroconf-services-hash'.
  (gethash (concat name "/" type) zeroconf-services-hash nil))

(defun zeroconf-resolve-service (service)
  "Return all service attributes SERVICE as list.
NAME must be a string.  The service must be of service type
TYPE. The resulting list has the format

  \(INTERFACE PROTOCOL NAME TYPE DOMAIN HOST APROTOCOL ADDRESS PORT TXT FLAGS\)."
  (let* ((name (zeroconf-service-name service))
	 (type (zeroconf-service-type service))
	 (key (concat name "/" type)))

    (or
     ;; Check whether we know this service already.
     (gethash key zeroconf-resolved-services-hash nil)

     ;; Resolve the service.  We don't propagate D-Bus errors.
     (dbus-ignore-errors
       (let* ((result
	       (dbus-call-method
		:system zeroconf-service-avahi zeroconf-path-avahi
		zeroconf-interface-avahi-server "ResolveService"
		zeroconf-avahi-interface-unspec
		zeroconf-avahi-protocol-unspec
		name type
		zeroconf-avahi-current-domain
		zeroconf-avahi-protocol-unspec
		zeroconf-avahi-flags-unspec))
	      (elt (nth 9 result))) ;; TXT.
	 ;; The TXT field has the signature "aay".  Transform to "as".
	 (while elt
	   (setcar elt (dbus-byte-array-to-string (car elt)))
	   (setq elt (cdr elt)))

	 (when nil ;; We discard it, no use so far.
	 ;; Register a service resolver.
	 (let ((object-path (zeroconf-register-service-resolver name type)))
	   ;; Register the signals.
	   (dolist (member '("Found" "Failure"))
	     (dbus-register-signal
	      :system zeroconf-service-avahi object-path
	      zeroconf-interface-avahi-service-resolver member
	      'zeroconf-service-resolver-handler)))
	 )

	 ;; Return the resolved service.
	 (puthash key result zeroconf-resolved-services-hash))))))

(defun zeroconf-service-interface (service)
  "Return the internal interface number of SERVICE."
  (nth 0 service))

(defun zeroconf-service-protocol (service)
  "Return the protocol number of SERVICE."
  (nth 1 service))

(defun zeroconf-service-name (service)
  "Return the service name of SERVICE."
  (nth 2 service))

(defun zeroconf-service-type (service)
  "Return the type name of SERVICE."
  (nth 3 service))

(defun zeroconf-service-domain (service)
  "Return the domain name of SERVICE."
  (nth 4 service))

(defun zeroconf-service-flags (service)
  "Return the flags of SERVICE."
  (nth 5 service))

(defun zeroconf-service-host (service)
  "Return the host name of SERVICE."
  (nth 5 (zeroconf-resolve-service service)))

(defun zeroconf-service-aprotocol (service)
  "Return the aprotocol number of SERVICE."
  (nth 6 (zeroconf-resolve-service service)))

(defun zeroconf-service-address (service)
  "Return the IP address of SERVICE."
  (nth 7 (zeroconf-resolve-service service)))

(defun zeroconf-service-port (service)
  "Return the port number of SERVICE."
  (nth 8 (zeroconf-resolve-service service)))

(defun zeroconf-service-txt (service)
  "Return the text strings of SERVICE."
  (nth 9 (zeroconf-resolve-service service)))


;;; Services signaling.

;; Register for the service type browser.  Service registrations will
;; happen in `zeroconf-service-type-browser-handler', when there is an
;; "ItemNew" signal from the service type browser.
(defun zeroconf-init (&optional domain)
  "Instantiate an Avahi service type browser for domain DOMAIN.
DOMAIN is a string, like \"dns-sd.org\" or \"local\".  When
DOMAIN is nil, the local domain is used."
  (when (and (or (null domain) (stringp domain))
	     (dbus-ping :system zeroconf-service-avahi)
	     (dbus-call-method
	      :system zeroconf-service-avahi zeroconf-path-avahi
	      zeroconf-interface-avahi-server "GetVersionString"))

    ;; Reset all stored values.
    (setq zeroconf-path-avahi-service-type-browser nil
	  zeroconf-avahi-current-domain (or domain
					    zeroconf-avahi-domain-unspec))
    (clrhash zeroconf-path-avahi-service-browser-hash)
    (clrhash zeroconf-path-avahi-service-resolver-hash)
    (clrhash zeroconf-services-hash)
    (clrhash zeroconf-resolved-services-hash)
    (clrhash zeroconf-service-added-hooks-hash)
    (clrhash zeroconf-service-removed-hooks-hash)

    ;; Register a service type browser.
    (let ((object-path (zeroconf-register-service-type-browser)))
      ;; Register the signals.
      (dolist (member '("ItemNew" "ItemRemove" "Failure"))
	(dbus-register-signal
	 :system zeroconf-service-avahi object-path
	 zeroconf-interface-avahi-service-type-browser member
	 'zeroconf-service-type-browser-handler)))

    ;; Register state changed signal.
    (dbus-register-signal
     :system zeroconf-service-avahi zeroconf-path-avahi
     zeroconf-interface-avahi-service-type-browser "StateChanged"
     'zeroconf-service-type-browser-handler)))

(defun zeroconf-register-service-type-browser ()
  "Register a service type browser at the Avahi daemon."
  (or zeroconf-path-avahi-service-type-browser
      (setq zeroconf-path-avahi-service-type-browser
	    (dbus-call-method
	     :system zeroconf-service-avahi zeroconf-path-avahi
	     zeroconf-interface-avahi-server "ServiceTypeBrowserNew"
	     zeroconf-avahi-interface-unspec
	     zeroconf-avahi-protocol-unspec
	     zeroconf-avahi-current-domain
	     zeroconf-avahi-flags-unspec))))

(defun zeroconf-service-type-browser-handler (&rest val)
  "Registered service type browser handler at the Avahi daemon."
  (when zeroconf-debug
    (message "zeroconf-service-type-browser-handler: %s %S"
	     (dbus-event-member-name last-input-event) val))
  (cond
   ((string-equal (dbus-event-member-name last-input-event) "ItemNew")
    ;; Parameters: (interface protocol type domain flags)
    ;; Register a service browser.
    (let ((object-path (zeroconf-register-service-browser (nth-value 2 val))))
      ;; Register the signals.
      (dolist (member '("ItemNew" "ItemRemove" "Failure"))
	(dbus-register-signal
	 :system zeroconf-service-avahi object-path
	 zeroconf-interface-avahi-service-browser member
	 'zeroconf-service-browser-handler))))))

(defun zeroconf-register-service-browser (type)
  "Register a service browser at the Avahi daemon."
  (or (gethash type zeroconf-path-avahi-service-browser-hash nil)
      (puthash type
	       (dbus-call-method
		:system zeroconf-service-avahi zeroconf-path-avahi
		zeroconf-interface-avahi-server "ServiceBrowserNew"
		zeroconf-avahi-interface-unspec
		zeroconf-avahi-protocol-unspec
		type
		zeroconf-avahi-current-domain
		zeroconf-avahi-flags-unspec)
	       zeroconf-path-avahi-service-browser-hash)))

(defun zeroconf-service-browser-handler (&rest val)
  "Registered service browser handler at the Avahi daemon."
  ;; Parameters: (interface protocol name type domain flags)
  (when zeroconf-debug
    (message "zeroconf-service-browser-handler: %s %S"
	     (dbus-event-member-name last-input-event) val))
  (let* ((name (zeroconf-service-name val))
	 (type (zeroconf-service-type val))
	 (key (concat name "/" type))
	 (ahook (gethash type zeroconf-service-added-hooks-hash nil))
	 (rhook (gethash type zeroconf-service-removed-hooks-hash nil)))
    (cond
     ((string-equal (dbus-event-member-name last-input-event) "ItemNew")
      ;; Add new service.
      (puthash key val zeroconf-services-hash)
      (run-hook-with-args 'ahook val))

     ((string-equal (dbus-event-member-name last-input-event) "ItemRemove")
      ;; Remove the service.
      (remhash key zeroconf-services-hash)
      (remhash key zeroconf-resolved-services-hash)
      (run-hook-with-args 'rhook val)))))

(defun zeroconf-register-service-resolver (name type)
  "Register a service resolver at the Avahi daemon."
  (let ((key (concat name "/" type)))
    (or (gethash key zeroconf-path-avahi-service-resolver-hash nil)
	(puthash key
		 (dbus-call-method
		  :system zeroconf-service-avahi zeroconf-path-avahi
		  zeroconf-interface-avahi-server "ServiceResolverNew"
		  zeroconf-avahi-interface-unspec
		  zeroconf-avahi-protocol-unspec
		  name type
		  zeroconf-avahi-current-domain
		  zeroconf-avahi-protocol-unspec
		  zeroconf-avahi-flags-unspec)
		 zeroconf-resolved-services-hash))))

(defun zeroconf-service-resolver-handler (&rest val)
  "Registered service resolver handler at the Avahi daemon."
  ;; Parameters: (interface protocol name type domain host aprotocol
  ;;              address port txt flags)
  ;; The "TXT" field has the signature "aay".  Transform to "as".
  (let ((elt (nth 9 val)))
    (while elt
      (setcar elt (dbus-byte-array-to-string (car elt)))
      (setq elt (cdr elt))))
  (when zeroconf-debug
    (message "zeroconf-service-resolver-handler: %s %S"
	     (dbus-event-member-name last-input-event) val))
  (cond
   ;; A new service has been detected.  Add it to
   ;; `zeroconf-resolved-services-hash'.
   ((string-equal (dbus-event-member-name last-input-event) "Found")
    (puthash
     (concat (zeroconf-service-name val) "/" (zeroconf-service-type val))
     val zeroconf-resolved-services-hash))))


;;; Services publishing.

(defun zeroconf-publish-service (name type domain host port address txt)
  "Publish a service at the Avahi daemon.
For the description of arguments, see `zeroconf-resolved-services-hash'."
  ;; NAME and TYPE must not be empty.
  (when (zerop (length name))
    (error "Invalid argument NAME: %s" name))
  (when (zerop (length type))
    (error "Invalid argument TYPE: %s" type))

  ;; Set default values for DOMAIN, HOST and PORT.
  (when (zerop (length domain))
    (setq domain (zeroconf-get-domain)))
  (when (zerop (length host))
    (setq host (zeroconf-get-host-domain)))
  (when (null port)
    (setq port 0))

  ;; Create an entry in the daemon.
  (let ((object-path
	 (dbus-call-method
	  :system zeroconf-service-avahi zeroconf-path-avahi
	  zeroconf-interface-avahi-server "EntryGroupNew"))
	result)

    ;; The TXT field has the signature "as".  Transform to "aay".
    (dolist (elt txt)
      (add-to-list 'result (dbus-string-to-byte-array elt)))

    ;; Add the service.
    (dbus-call-method
     :system zeroconf-service-avahi object-path
     zeroconf-interface-avahi-entry-group "AddService"
     zeroconf-avahi-interface-unspec
     zeroconf-avahi-protocol-unspec
     zeroconf-avahi-flags-unspec
     name type domain host :uint16 port (append '(:array) result))

    ;; Add the address.
    (unless (zerop (length address))
      (dbus-call-method
       :system zeroconf-service-avahi object-path
       zeroconf-interface-avahi-entry-group "AddAddress"
       zeroconf-avahi-interface-unspec
       zeroconf-avahi-protocol-unspec
       zeroconf-avahi-flags-unspec
       host address))

    ;; Make it persistent in the daemon.
    (dbus-call-method
     :system zeroconf-service-avahi object-path
     zeroconf-interface-avahi-entry-group "Commit")))

(provide 'zeroconf)

;;; zeroconf.el ends here
