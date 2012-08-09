;;; xesam.el --- Xesam interface to search engines.

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: tools, hypermedia

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

;; This package provides an interface to Xesam, a D-Bus based "eXtEnsible
;; Search And Metadata specification".  It has been tested with
;;
;; xesam-glib 0.3.4, xesam-tools 0.6.1
;; beagle 0.3.7, beagle-xesam 0.2
;; strigi 0.5.11

;; The precondition for this package is a D-Bus aware Emacs.  This is
;; configured per default, when Emacs is built on a machine running
;; D-Bus.  Furthermore, there must be at least one search engine
;; running, which supports the Xesam interface.  Beagle and strigi have
;; been tested; tracker, pinot and recoll are also said to support
;; Xesam.  You can check the existence of such a search engine by
;;
;;   (dbus-list-queued-owners :session "org.freedesktop.xesam.searcher")

;; In order to start a search, you must load xesam.el:
;;
;;   (require 'xesam)

;; xesam.el supports two types of queries, which are explained *very* short:
;;
;; * Full text queries.  Just search keys shall be given, like
;;
;;     hello world
;;
;;   A full text query in xesam.el is restricted to files.
;;
;; * Xesam End User Search Language queries.  The Xesam query language
;;   is described at <http://xesam.org/main/XesamUserSearchLanguage>,
;;   which must be consulted for the whole features.
;;
;;   A query string consists of search keys, collectors, selectors,
;;   and phrases.  Search keys are words like in a full text query:
;;
;;     hello word
;;
;;   A selector is a tuple <keyword><relation>.  <keyword> can be any
;;   predefined Xesam keyword, the most common keywords are "ext"
;;   (file name extension), "format " (mime type), "tag" (user
;;   keywords) and "type" (types of items, like "audio", "file",
;;   "picture", "attachment").  <relation> is a comparison to a value,
;;   which must be a string (relation ":" or "=") or number (relation
;;   "<=", ">=", "<", ">"):
;;
;;     type:attachment ext=el
;;
;;   A collector is one of the items "AND", "and", "&&", "OR", "or",
;;   "||", or "-".  The default collector on multiple terms is "AND";
;;   "-" means "AND NOT".
;;
;;     albinus -type:file
;;
;;   A phrase is a string enclosed in quotes, with appended modifiers
;;   (single letters).  Examples of modifiers are "c" (case
;;   sensitive), "C" (case insensitive), "e" (exact match), "r"
;;   (regular expression):
;;
;;     "Hello world"c

;; You can customize, whether you want to apply a Xesam user query, or
;; a full text query.  Note, that not every search engine supports
;; both query types.
;;
;;   (setq xesam-query-type 'fulltext-query)
;;
;; Another option to be customized is the number of hits to be
;; presented at once.
;;
;;   (setq xesam-hits-per-page 50)

;; A search can be started by the command
;;
;;   M-x xesam-search
;;
;; When several search engines are registered, the engine to be used
;; can be selected via minibuffer completion.  Afterwards, the query
;; shall be entered in the minibuffer.

;; Search results are presented in a new buffer.  This buffer has the
;; major mode `xesam-mode', with the following keybindings:

;;   SPC	`scroll-up'
;;   DEL	`scroll-down'
;;   <		`beginning-of-buffer'
;;   >		`end-of-buffer'
;;   q		`quit-window'
;;   z		`kill-this-buffer'
;;   g		`revert-buffer'

;; The search results are represented by widgets.  Navigation commands
;; are the usual widget navigation commands:

;;   TAB	`widget-forward'
;;   <backtab>	`widget-backward'

;; Applying RET, <down-mouse-1>, or <down-mouse-2> on a URL belonging
;; to the widget, brings up more details of the search hit.  The way,
;; how this hit is presented, depends on the type of the hit.  HTML
;; files are opened via `browse-url'.  Local files are opened in a new
;; buffer, with highlighted search hits (highlighting can be toggled
;; by `xesam-minor-mode' in that buffer).

;;; Code:

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Declare used subroutines and variables.
(declare-function dbus-call-method "dbusbind.c")
(declare-function dbus-register-signal "dbusbind.c")

(require 'dbus)

;; Pacify byte compiler.
(eval-when-compile
  (require 'cl))

;; Widgets are used to highlight the search results.
(require 'widget)
(require 'wid-edit)

;; `run-at-time' is used in the signal handler.
(require 'timer)

;; The default search field is "xesam:url".  It must be inspected.
(require 'url)

(defgroup xesam nil
  "Xesam compatible interface to search engines."
  :group 'extensions
  :group 'comm
  :version "23.1")

(defcustom xesam-query-type 'user-query
  "Xesam query language type."
  :group 'xesam
  :type '(choice
	  (const :tag "Xesam user query" user-query)
	  (const :tag "Xesam fulltext query" fulltext-query)))

(defcustom xesam-hits-per-page 20
  "Number of search hits to be displayed in the result buffer."
  :group 'xesam
  :type 'integer)

(defface xesam-mode-line '((t :inherit mode-line-emphasis))
  "Face to highlight mode line."
  :group 'xesam)

(defface xesam-highlight '((t :inherit match))
  "Face to highlight query entries.
It will be overlaid by `widget-documentation-face', so it shall
be different at least in one face property not set in that face."
  :group 'xesam)

(defvar xesam-debug nil
  "Insert debug information in the help echo.")

(defconst xesam-service-search "org.freedesktop.xesam.searcher"
  "The D-Bus name used to talk to Xesam.")

(defconst xesam-path-search "/org/freedesktop/xesam/searcher/main"
  "The D-Bus object path used to talk to Xesam.")

;; Methods: "NewSession", "SetProperty", "GetProperty",
;; "CloseSession", "NewSearch", "StartSearch", "GetHitCount",
;; "GetHits", "GetHitData", "CloseSearch" and "GetState".
;; Signals: "HitsAdded", "HitsRemoved", "HitsModified", "SearchDone"
;; and "StateChanged".
(defconst xesam-interface-search "org.freedesktop.xesam.Search"
  "The D-Bus Xesam search interface.")

(defconst xesam-all-fields
  '("xesam:35mmEquivalent" "xesam:aimContactMedium" "xesam:aperture"
    "xesam:aspectRatio" "xesam:attachmentEncoding" "xesam:attendee"
    "xesam:audioBitrate" "xesam:audioChannels" "xesam:audioCodec"
    "xesam:audioCodecType" "xesam:audioSampleFormat" "xesam:audioSampleRate"
    "xesam:author" "xesam:bcc" "xesam:birthDate" "xesam:blogContactURL"
    "xesam:cameraManufacturer" "xesam:cameraModel" "xesam:cc" "xesam:ccdWidth"
    "xesam:cellPhoneNumber" "xesam:characterCount" "xesam:charset"
    "xesam:colorCount" "xesam:colorSpace" "xesam:columnCount" "xesam:comment"
    "xesam:commentCharacterCount" "xesam:conflicts" "xesam:contactMedium"
    "xesam:contactName" "xesam:contactNick" "xesam:contactPhoto"
    "xesam:contactURL" "xesam:contains" "xesam:contenKeyword"
    "xesam:contentComment" "xesam:contentCreated" "xesam:contentModified"
    "xesam:contentType" "xesam:contributor" "xesam:copyright" "xesam:creator"
    "xesam:definesClass" "xesam:definesFunction" "xesam:definesGlobalVariable"
    "xesam:deletionTime" "xesam:depends" "xesam:description" "xesam:device"
    "xesam:disclaimer" "xesam:documentCategory" "xesam:duration"
    "xesam:emailAddress" "xesam:eventEnd" "xesam:eventLocation"
    "xesam:eventStart" "xesam:exposureBias" "xesam:exposureProgram"
    "xesam:exposureTime" "xesam:faxPhoneNumber" "xesam:fileExtension"
    "xesam:fileSystemType" "xesam:flashUsed" "xesam:focalLength"
    "xesam:focusDistance" "xesam:formatSubtype" "xesam:frameCount"
    "xesam:frameRate" "xesam:freeSpace" "xesam:gender" "xesam:generator"
    "xesam:generatorOptions" "xesam:group" "xesam:hash" "xesam:hash"
    "xesam:height" "xesam:homeEmailAddress" "xesam:homePhoneNumber"
    "xesam:homePostalAddress" "xesam:homepageContactURL"
    "xesam:horizontalResolution" "xesam:icqContactMedium" "xesam:id"
    "xesam:imContactMedium" "xesam:interests" "xesam:interlaceMode"
    "xesam:isEncrypted" "xesam:isImportant" "xesam:isInProgress"
    "xesam:isPasswordProtected" "xesam:isRead" "xesam:isoEquivalent"
    "xesam:jabberContactMedium" "xesam:keyword" "xesam:language" "xesam:legal"
    "xesam:license" "xesam:licenseType" "xesam:lineCount" "xesam:links"
    "xesam:mailingPostalAddress" "xesam:maintainer" "xesam:md5Hash"
    "xesam:mediaCodec" "xesam:mediaCodecBitrate" "xesam:mediaCodecType"
    "xesam:meteringMode" "xesam:mimeType" "xesam:mountPoint"
    "xesam:msnContactMedium" "xesam:name" "xesam:occupiedSpace"
    "xesam:orientation" "xesam:originalLocation" "xesam:owner"
    "xesam:pageCount" "xesam:permissions" "xesam:phoneNumber"
    "xesam:physicalAddress" "xesam:pixelFormat" "xesam:primaryRecipient"
    "xesam:programmingLanguage" "xesam:rating" "xesam:receptionTime"
    "xesam:recipient" "xesam:related" "xesam:remoteUser" "xesam:rowCount"
    "xesam:sampleBitDepth" "xesam:sampleFormat" "xesam:secondaryRecipient"
    "xesam:sha1Hash" "xesam:size" "xesam:skypeContactMedium"
    "xesam:sourceCreated" "xesam:sourceModified" "xesam:storageSize"
    "xesam:subject" "xesam:supercedes" "xesam:title" "xesam:to"
    "xesam:totalSpace" "xesam:totalUncompressedSize" "xesam:url"
    "xesam:usageIntensity" "xesam:userComment" "xesam:userKeyword"
    "xesam:uuid" "xesam:version" "xesam:verticalResolution"
    "xesam:videoBitrate"
    "xesam:videoCodec" "xesam:videoCodecType" "xesam:whiteBalance"
    "xesam:width" "xesam:wordCount" "xesam:workEmailAddress"
    "xesam:workPhoneNumber" "xesam:workPostalAddress"
    "xesam:yahooContactMedium")
  "All fields from the Xesam Core Ontology.
This defconst can be used to check for a new search engine, which
fields are supported.")

(defconst xesam-user-query
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<request xmlns=\"http://freedesktop.org/standards/xesam/1.0/query\">
  <userQuery>
    %s
  </userQuery>
</request>"
  "The Xesam user query XML.")

(defconst xesam-fulltext-query
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<request xmlns=\"http://freedesktop.org/standards/xesam/1.0/query\">
  <query content=\"xesam:Document\" source=\"xesam:File\">
    <fullText>
      <string>%s</string>
    </fullText>
  </query>
</request>"
  "The Xesam fulltext query XML.")

(declare-function dbus-get-unique-name "dbusbind.c" (bus))

(defvar xesam-dbus-unique-names
  (list (cons :system (dbus-get-unique-name :system))
	(cons :session (dbus-get-unique-name :session)))
  "The unique names, under which Emacs is registered at D-Bus.")

(defun xesam-dbus-call-method (&rest args)
  "Apply a D-Bus method call.
`dbus-call-method' is preferred, because it performs better.
If the target D-Bus service is owned by Emacs, this
is not applicable, and `dbus-call-method-non-blocking' must be
used instead.  ARGS are identical to the argument list of both
functions."
  (apply
   ;; The first argument is the bus, the second argument the targt service.
   (if (string-equal (cdr (assoc (car args) xesam-dbus-unique-names))
		     (cadr args))
       'dbus-call-method-non-blocking 'dbus-call-method)
   args))

(defun xesam-get-property (engine property)
  "Return the PROPERTY value of ENGINE."
  ;; "GetProperty" returns a variant, so we must use the car.
  (car (xesam-dbus-call-method
	:session (car engine) xesam-path-search
	xesam-interface-search  "GetProperty"
	(xesam-get-cached-property engine "session") property)))

(defun xesam-set-property (engine property value)
  "Set the PROPERTY of ENGINE to VALUE.
VALUE can be a string, a non-negative integer, a boolean
value (nil or t), or a list of them.  It returns the new value of
PROPERTY in the search engine.  This new value can be different
from VALUE, depending on what the search engine accepts."
  ;; "SetProperty" returns a variant, so we must use the car.
  (car (xesam-dbus-call-method
	:session (car engine) xesam-path-search
	xesam-interface-search  "SetProperty"
	(xesam-get-cached-property engine "session") property
	;; The value must be a variant.  It can be only a string, an
	;; unsigned int, a boolean, or an array of them.  So we need
	;; no type keyword; we let the type check to the search
	;; engine.
	(list :variant value))))

(defvar xesam-minibuffer-vendor-history nil
  "Interactive vendor history.")

(defvar xesam-minibuffer-query-history nil
  "Interactive query history.")

;; Pacify byte compiler.
(defvar xesam-vendor nil)
(make-variable-buffer-local 'xesam-vendor)
(put 'xesam-vendor 'permanent-local t)

(defvar xesam-engine nil)
(defvar xesam-search nil)
(defvar xesam-type nil)
(defvar xesam-query nil)
(defvar xesam-xml-string nil)
(defvar xesam-objects nil)
(defvar xesam-current nil)
(defvar xesam-count nil)
(defvar xesam-to nil)
(defvar xesam-notify-function nil)
(defvar xesam-refreshing nil)


;;; Search engines.

(defvar xesam-search-engines nil
  "List of available Xesam search engines.
Every entry is an association list, with a car denoting the
unique D-Bus service name of the engine.  The rest of the entry
are cached associations of engine attributes, like the session
identifier, and the display name.  Example:

  \(\(\":1.59\"
    \(\"session\" . \"0t1214948020ut358230u0p2698r3912347765k3213849828\")
    \(\"vendor.display\" . \"Tracker Xesam Service\"))
   \(\":1.27\"
    \(\"session\" . \"strigisession1369133069\")
    \(\"vendor.display\" . \"Strigi Desktop Search\")))

A Xesam-compatible search engine is identified as a queued D-Bus
service of the known service `xesam-service-search'.")

(defun xesam-get-cached-property (engine property)
  "Return the PROPERTY value of ENGINE from the cache.
If PROPERTY is not existing, retrieve it from ENGINE first."
  ;; If the property has not been cached yet, we retrieve it from the
  ;; engine, and cache it.
  (unless (assoc property engine)
    (xesam-set-cached-property
     engine property (xesam-get-property engine property)))
  (cdr (assoc property engine)))

(defun xesam-set-cached-property (engine property value)
  "Set the PROPERTY of ENGINE to VALUE in the cache."
  (setcdr engine (append (cdr engine) (list (cons property value)))))

(defun xesam-delete-search-engine (&rest args)
  "Remove service from `xesam-search-engines'."
  (setq xesam-search-engines
	(delete (assoc (car args) xesam-search-engines) xesam-search-engines)))

(defvar dbus-debug)

(defun xesam-search-engines ()
  "Return Xesam search engines, stored in `xesam-search-engines'.
The first search engine is the name owner of `xesam-service-search'.
If there is no registered search engine at all, the function returns `nil'."
  (let ((services (dbus-ignore-errors
		    (dbus-list-queued-owners
		     :session xesam-service-search)))
	engine vendor-id hit-fields)
    (dolist (service services)
      (unless (assoc-string service xesam-search-engines)

	;; Open a new session, and add it to the search engines list.
	(add-to-list 'xesam-search-engines (list service) 'append)
	(setq engine (assoc service xesam-search-engines))

	;; Add the session string.
	(xesam-set-cached-property
	 engine "session"
	 (xesam-dbus-call-method
	  :session service xesam-path-search
	  xesam-interface-search "NewSession"))

	;; Unset the "search.live" property; we don't want to be
	;; informed by changed results.
	(xesam-set-property engine "search.live" nil)

	;; Check the vendor properties.
	(setq vendor-id (xesam-get-property engine "vendor.id")
	      hit-fields (xesam-get-property engine "hit.fields"))

	;; Usually, `hit.fields' shall describe supported fields.
	;; That is not the case now, so we set it ourselves.
	;; Hopefully, this will change later.
	(setq hit-fields
	      (case (intern vendor-id)
		(Beagle
		 '("xesam:mimeType" "xesam:url"))
		(Strigi
		 '("xesam:author" "xesam:cc" "xesam:charset"
		   "xesam:contentType" "xesam:fileExtension"
		   "xesam:id" "xesam:lineCount" "xesam:links"
		   "xesam:mimeType" "xesam:name" "xesam:size"
		   "xesam:sourceModified" "xesam:subject" "xesam:to"
		   "xesam:url"))
		(TrackerXesamSession
		 '("xesam:relevancyRating" "xesam:url"))
		(Debbugs
		 '("xesam:keyword" "xesam:owner" "xesam:title"
		   "xesam:url" "xesam:sourceModified" "xesam:mimeType"
		   "debbugs:key"))
		;; xesam-tools yahoo service.
		(t '("xesam:contentModified" "xesam:mimeType" "xesam:summary"
		     "xesam:title" "xesam:url" "yahoo:displayUrl"))))

	(xesam-set-property engine "hit.fields" hit-fields)
	(xesam-set-property engine "hit.fields.extended" '("xesam:snippet"))

	;; Let us notify, when the search engine disappears.
	(dbus-register-signal
	 :session dbus-service-dbus dbus-path-dbus
	 dbus-interface-dbus "NameOwnerChanged"
	 'xesam-delete-search-engine service))))
  xesam-search-engines)


;;; Search buffers.

(defvar xesam-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent xesam-mode-map widget-keymap)
    map))

(define-derived-mode xesam-mode special-mode "Xesam"
  "Major mode for presenting search results of a Xesam search.
In this mode, widgets represent the search results.

\\{xesam-mode-map}
Turning on Xesam mode runs the normal hook `xesam-mode-hook'.  It
can be used to set `xesam-notify-function', which must a search
engine specific, widget :notify function to visualize xesam:url."
  (set (make-local-variable 'xesam-notify-function) nil)
  ;; Maybe we implement something useful, later on.
  (set (make-local-variable 'revert-buffer-function) 'ignore)
  ;; `xesam-engine', `xesam-search', `xesam-type', `xesam-query', and
  ;; `xesam-xml-string' will be set in `xesam-new-search'.
  (set (make-local-variable 'xesam-engine) nil)
  (set (make-local-variable 'xesam-search) nil)
  (set (make-local-variable 'xesam-type) "")
  (set (make-local-variable 'xesam-query) "")
  (set (make-local-variable 'xesam-xml-string) "")
  (set (make-local-variable 'xesam-objects) nil)
  ;; `xesam-current' is the last hit put into the search buffer,
  (set (make-local-variable 'xesam-current) 0)
  ;; `xesam-count' is the number of hits reported by the search engine.
  (set (make-local-variable 'xesam-count) 0)
  ;; `xesam-to' is the upper hit number to be presented.
  (set (make-local-variable 'xesam-to) xesam-hits-per-page)
  ;; `xesam-notify-function' can be a search engine specific function
  ;; to visualize xesam:url.  It can be overwritten in `xesam-mode'.
  (set (make-local-variable 'xesam-notify-function) nil)
  ;; `xesam-refreshing' is an indicator, whether the buffer is just
  ;; being updated.  Needed, because `xesam-refresh-search-buffer'
  ;; can be triggered by an event.
  (set (make-local-variable 'xesam-refreshing) nil)
  ;; Mode line position returns hit counters.
  (set (make-local-variable 'mode-line-position)
       (list '(-3 "%p%")
	     '(10 (:eval (format " (%d/%d)" xesam-current xesam-count)))))
  ;; Header line contains the query string.
  (set (make-local-variable 'header-line-format)
       (list '(20
	       (:eval
		(list "Type: "
		      (propertize xesam-type 'face 'xesam-mode-line))))
	     '(10
	       (:eval
		(list " Query: "
		      (propertize
		       xesam-query
		       'face 'xesam-mode-line
		       'help-echo (when xesam-debug xesam-xml-string)))))))

  (when (not (called-interactively-p 'interactive))
    ;; Initialize buffer.
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;; It doesn't make sense to call it interactively.
(put 'xesam-mode 'disabled t)

;; The very first buffer created with `xesam-mode' does not have the
;; keymap etc.  So we create a dummy buffer.  Stupid.
(with-temp-buffer (xesam-mode))

(define-minor-mode xesam-minor-mode
  "Toggle Xesam minor mode.
With a prefix argument ARG, enable Xesam minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Xesam minor mode is enabled, all text which matches a
previous Xesam query in this buffer is highlighted."
  :group 'xesam
  :init-value nil
  :lighter " Xesam"
  (when (local-variable-p 'xesam-query)
    ;; Run only if the buffer is related to a Xesam search.
    (save-excursion
      (if xesam-minor-mode
	  ;; Highlight hits.
	  (let ((query-regexp (regexp-opt (split-string xesam-query nil t) t))
		(case-fold-search t))
	    ;; I have no idea whether people will like setting
	    ;; `isearch-case-fold-search' and `query-regexp'.  Maybe
	    ;; this shall be controlled by a custom option.
	    (unless isearch-case-fold-search (isearch-toggle-case-fold))
	    (isearch-update-ring query-regexp t)
	    ;; Create overlays.
	    (goto-char (point-min))
	    (while (re-search-forward query-regexp nil t)
	      (overlay-put
	       (make-overlay
		(match-beginning 0) (match-end 0)) 'face 'xesam-highlight)))
	;; Remove overlays.
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (delete-overlay ov))))))

(defun xesam-buffer-name (service search)
  "Return the buffer name where to present search results.
SERVICE is the D-Bus unique service name of the Xesam search engine.
SEARCH is the search identification in that engine.  Both must be strings."
  (format "*%s/%s*" service search))

(defun xesam-highlight-string (string)
  "Highlight text enclosed by <b> and </b>.
Return propertized STRING."
  (while (string-match "\\(.*\\)\\(<b>\\)\\(.*\\)\\(</b>\\)\\(.*\\)" string)
    (setq string
	  (format
	   "%s%s%s"
	   (match-string 1 string)
	   (propertize (match-string 3 string) 'face 'xesam-highlight)
	   (match-string 5 string))))
  string)

(defun xesam-refresh-entry (engine entry)
  "Refreshes one entry in the search buffer."
  (let* ((result (nth (1- xesam-current) xesam-objects))
	 widget)

    ;; Create widget.
    (setq widget (widget-convert 'link))
    (when xesam-debug
      (widget-put widget :help-echo ""))

    ;; Take all results.
    (dolist (field (xesam-get-cached-property engine "hit.fields"))
      (when (cond
	     ((stringp (caar result)) (not (zerop (length (caar result)))))
	     ((numberp (caar result)) (not (zerop (caar result))))
	     ((caar result) t))
	(when xesam-debug
	  (widget-put
	   widget :help-echo
	   (format "%s%s: %s\n"
		   (widget-get widget :help-echo) field (caar result))))
	(widget-put widget (intern (concat ":" field)) (caar result)))
      (setq result (cdr result)))

    ;; Strigi doesn't return URLs in xesam:url.  We must fix this.
    (when
	(not (url-type (url-generic-parse-url (widget-get widget :xesam:url))))
      (widget-put
       widget :xesam:url (concat "file://" (widget-get widget :xesam:url))))

    ;; Strigi returns xesam:size as string.  We must fix this.
    (when (and (widget-member widget :xesam:size)
	       (stringp (widget-get widget :xesam:size)))
      (widget-put
       widget :xesam:size (string-to-number (widget-get widget :xesam:url))))

    ;; First line: :tag.
    (cond
     ((widget-member widget :xesam:title)
      (widget-put widget :tag (widget-get widget :xesam:title)))
     ((widget-member widget :xesam:subject)
      (widget-put widget :tag (widget-get widget :xesam:subject)))
     ((widget-member widget :xesam:mimeType)
      (widget-put widget :tag (widget-get widget :xesam:mimeType)))
     ((widget-member widget :xesam:name)
      (widget-put widget :tag (widget-get widget :xesam:name))))

    ;; Highlight the search items.
    (when (widget-member widget :tag)
      (widget-put
       widget :tag (xesam-highlight-string (widget-get widget :tag))))

    ;; Last Modified.
    (when (and (widget-member widget :xesam:sourceModified)
	       (not
		(zerop
		 (string-to-number (widget-get widget :xesam:sourceModified)))))
      (widget-put
       widget :tag
       (format
	"%s\nLast Modified: %s"
	(or (widget-get widget :tag) "")
	(format-time-string
	 "%d %B %Y, %T"
	 (seconds-to-time
	  (string-to-number (widget-get widget :xesam:sourceModified)))))))

    ;; Second line: :value.
    (widget-put widget :value (widget-get widget :xesam:url))

    (cond
     ;; A search engine can set `xesam-notify-function' via
     ;; `xesam-mode-hooks'.
     (xesam-notify-function
      (widget-put widget :notify xesam-notify-function))

     ;; In case of HTML, we use a URL link.
     ((and (widget-member widget :xesam:mimeType)
	   (string-equal "text/html" (widget-get widget :xesam:mimeType)))
      (setcar widget 'url-link))

     ;; For local files, we will open the file as default action.
     ((string-match "file"
		    (url-type (url-generic-parse-url
			       (widget-get widget :xesam:url))))
      (widget-put
       widget :notify
       (lambda (widget &rest ignore)
	 (let ((query xesam-query))
	   (find-file
	    (url-filename (url-generic-parse-url (widget-value widget))))
	   (set (make-local-variable 'xesam-query) query)
	   (xesam-minor-mode 1))))
      (widget-put
       widget :value
       (url-filename (url-generic-parse-url (widget-get widget :xesam:url))))))

    ;; Third line: :doc.
    (cond
     ((widget-member widget :xesam:summary)
      (widget-put widget :doc (widget-get widget :xesam:summary)))
     ((widget-member widget :xesam:snippet)
      (widget-put widget :doc (widget-get widget :xesam:snippet))))

    (when (widget-member widget :doc)
      (with-temp-buffer
	(insert
	 (xesam-highlight-string (widget-get widget :doc)))
	(fill-region-as-paragraph (point-min) (point-max))
	(widget-put widget :doc (buffer-string)))
      (widget-put widget :help-echo (widget-get widget :doc)))

    ;; Format the widget.
    (widget-put
     widget :format
     (format "%d. %s%%[%%v%%]\n%s\n" xesam-current
	     (if (widget-member widget :tag) "%{%t%}\n" "")
	     (if (widget-member widget :doc) "%h" "")))

    ;; Write widget.
    (goto-char (point-max))
    (widget-default-create widget)
    (set-buffer-modified-p nil)
    (force-mode-line-update)
    (redisplay)))

(defun xesam-get-hits (engine search hits)
  "Retrieve hits from ENGINE."
  (with-current-buffer (xesam-buffer-name (car engine) search)
    (setq xesam-objects
	  (append xesam-objects
		  (xesam-dbus-call-method
		   :session (car engine) xesam-path-search
		   xesam-interface-search "GetHits" search hits)))))

(defun xesam-refresh-search-buffer (engine search)
  "Refreshes the buffer, presenting results of SEARCH."
  (with-current-buffer (xesam-buffer-name (car engine) search)
    ;; Work only if nobody else is here.
    (unless (or xesam-refreshing (>= xesam-current xesam-to))
      (setq xesam-refreshing t)
      (unwind-protect
	  (let (widget)

	    ;; Retrieve needed hits for visualization.
	    (while (> (min xesam-to xesam-count) (length xesam-objects))
	      (xesam-get-hits
	       engine search
	       (min xesam-hits-per-page
		    (- (min xesam-to xesam-count) (length xesam-objects)))))

	    ;; Add all result widgets.
	    (while (< xesam-current (min xesam-to xesam-count))
	      (setq xesam-current (1+ xesam-current))
	      (xesam-refresh-entry engine search))

	    ;; Add "NEXT" widget.
	    (when (> xesam-count xesam-to)
	      (goto-char (point-max))
	      (widget-create
	       'link
	       :notify
	       (lambda (widget &rest ignore)
		 (setq xesam-to (+ xesam-to xesam-hits-per-page))
		 (widget-delete widget)
		 (xesam-refresh-search-buffer xesam-engine xesam-search))
	       "NEXT")
	      (widget-beginning-of-line))

	    ;; Prefetch next hits.
	    (when (> (min (+ xesam-hits-per-page xesam-to) xesam-count)
		     (length xesam-objects))
	      (xesam-get-hits
	       engine search
	       (min xesam-hits-per-page
		    (- (min (+ xesam-hits-per-page xesam-to) xesam-count)
		       (length xesam-objects)))))

	    ;; Add "DONE" widget.
	    (when (= xesam-current xesam-count)
	      (goto-char (point-max))
	      (widget-create 'link :notify 'ignore "DONE")
	      (widget-beginning-of-line)))

	;; Return with save settings.
	(setq xesam-refreshing nil)))))


;;; Search functions.

(defun xesam-signal-handler (&rest args)
  "Handles the different D-Bus signals of a Xesam search."
  (let* ((service (dbus-event-service-name last-input-event))
	 (member (dbus-event-member-name last-input-event))
	 (search (nth 0 args))
	 (buffer (xesam-buffer-name service search)))

    (when (get-buffer buffer)
      (with-current-buffer buffer
	(cond

	 ((string-equal member "HitsAdded")
	  (setq xesam-count (+ xesam-count (nth 1 args)))
	  ;; We use `run-at-time' in order to not block the event queue.
	  (run-at-time
	   0 nil
	   'xesam-refresh-search-buffer
	   (assoc service xesam-search-engines) search))

	 ((string-equal member "SearchDone")
	  (setq mode-line-process
		(propertize " Done" 'face 'xesam-mode-line))
	  (force-mode-line-update)))))))

(defun xesam-kill-buffer-function ()
  "Send the CloseSearch indication."
  (when (and (eq major-mode 'xesam-mode) (stringp xesam-search))
    (ignore-errors ;; The D-Bus service could have disappeared.
      (xesam-dbus-call-method
       :session (car xesam-engine) xesam-path-search
       xesam-interface-search "CloseSearch" xesam-search))))

(defun xesam-new-search (engine type query)
  "Create a new search session.
ENGINE identifies the search engine.  TYPE is the query type, it
can be either `fulltext-query', or `user-query'.  QUERY is a
string in the Xesam query language.  A string, identifying the
search, is returned."
  (let* ((service (car engine))
	 (session (xesam-get-cached-property engine "session"))
	 (xml-string
	  (format
	   (if (eq type 'user-query) xesam-user-query xesam-fulltext-query)
	   (url-insert-entities-in-string query)))
	 (search (xesam-dbus-call-method
		  :session service xesam-path-search
		  xesam-interface-search "NewSearch" session xml-string)))

    ;; Let us notify for relevant signals.  We ignore "HitsRemoved",
    ;; "HitsModified" and "StateChanged"; there is nothing to do for
    ;; us.
    (dbus-register-signal
     :session service xesam-path-search
     xesam-interface-search "HitsAdded"
     'xesam-signal-handler search)
    (dbus-register-signal
     :session service xesam-path-search
     xesam-interface-search "SearchDone"
     'xesam-signal-handler search)

    ;; Create the search buffer.
    (with-current-buffer
	(generate-new-buffer (xesam-buffer-name service search))
      (switch-to-buffer-other-window (current-buffer))
      ;; Initialize buffer with `xesam-mode'.  `xesam-vendor' must be
      ;; set before calling `xesam-mode', because we want to give the
      ;; hook functions a chance to identify their search engine.
      (setq xesam-vendor (xesam-get-cached-property engine "vendor.id"))
      (xesam-mode)
      (setq xesam-engine engine
	    xesam-search search
	    ;; `xesam-type', `xesam-query' and `xesam-xml-string'
	    ;; are displayed in the header line.
	    xesam-type (symbol-name type)
	    xesam-query query
	    xesam-xml-string xml-string
	    xesam-objects nil
	    ;; The buffer identification shall indicate the search
	    ;; engine.  The `help-echo' property is used for debug
	    ;; information, when applicable.
	    mode-line-buffer-identification
	    (if (not xesam-debug)
		(list 12 (propertized-buffer-identification xesam-vendor))
	      (propertize
	       xesam-vendor
	       'help-echo
	       (mapconcat
		(lambda (x)
		  (format "%s: %s" x (xesam-get-cached-property engine x)))
		'("vendor.id" "vendor.version" "vendor.display" "vendor.xesam"
		  "vendor.ontology.fields" "vendor.ontology.contents"
		  "vendor.ontology.sources" "vendor.extensions"
		  "vendor.ontologies" "vendor.maxhits")
		"\n"))))
      (add-hook 'kill-buffer-hook 'xesam-kill-buffer-function)
      (force-mode-line-update))

    ;; Start the search.
    (xesam-dbus-call-method
     :session (car engine) xesam-path-search
     xesam-interface-search "StartSearch" search)

    ;; Return search id.
    search))

;;;###autoload
(defun xesam-search (engine query)
  "Perform an interactive search.
ENGINE is the Xesam search engine to be applied, it must be one of the
entries of `xesam-search-engines'.  QUERY is the search string in the
Xesam user query language.  If the search engine does not support
the Xesam user query language, a Xesam fulltext search is applied.

The default search engine is the first entry in `xesam-search-engines'.
Example:

  (xesam-search (car (xesam-search-engines)) \"emacs\")"
  (interactive
   (let* ((vendors (mapcar
		    (lambda (x) (xesam-get-cached-property x "vendor.display"))
		    (xesam-search-engines)))
	  (vendor
	   (if (> (length vendors) 1)
	       (completing-read
		"Enter search engine: " vendors nil t
		(try-completion "" vendors) 'xesam-minibuffer-vendor-history)
	     (car vendors))))
     (list
      ;; ENGINE.
      (when vendor
	(dolist (elt (xesam-search-engines) engine)
	  (when (string-equal
		 (xesam-get-cached-property elt "vendor.display") vendor)
	    (setq engine elt))))
      ;; QUERY.
      (when vendor
	(read-from-minibuffer
	 "Enter search string: " nil nil nil
	 'xesam-minibuffer-query-history)))))

  (if (null engine)
      (message "No search engine running")
    (if (zerop (length query))
	(message "No query applied")
      (xesam-new-search engine xesam-query-type query))))

(provide 'xesam)

;;; TODO:

;; * Buffer highlighting needs better analysis of query string.
;; * Accept input while retrieving prefetched hits. `run-at-time'?
;; * With prefix, let's choose search engine.
;; * Minibuffer completion for user queries.
;; * `revert-buffer-function' implementation.
;;
;; * Mid term
;;   - If available, use ontologies for field selection.
;;   - Search engines for Emacs bugs database, wikipedia, google,
;;     yahoo, ebay, ...
;;   - Construct complex queries via widgets, like in mairix.el.

;;; xesam.el ends here
