;;; mm-decode.el --- Functions for decoding MIME things

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;;; Code:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(require 'mail-parse)
(require 'mm-bodies)
(eval-when-compile (require 'cl)
		   (require 'term))

(autoload 'gnus-map-function "gnus-util")
(autoload 'gnus-replace-in-string "gnus-util")
(autoload 'gnus-read-shell-command "gnus-util")

(autoload 'mm-inline-partial "mm-partial")
(autoload 'mm-inline-external-body "mm-extern")
(autoload 'mm-extern-cache-contents "mm-extern")
(autoload 'mm-insert-inline "mm-view")

(defvar gnus-current-window-configuration)

(add-hook 'gnus-exit-gnus-hook 'mm-destroy-postponed-undisplay-list)

(defgroup mime-display ()
  "Display of MIME in mail and news articles."
  :link '(custom-manual "(emacs-mime)Display Customization")
  :version "21.1"
  :group 'mail
  :group 'news
  :group 'multimedia)

(defgroup mime-security ()
  "MIME security in mail and news articles."
  :link '(custom-manual "(emacs-mime)Display Customization")
  :group 'mail
  :group 'news
  :group 'multimedia)

;;; Convenience macros.

(defmacro mm-handle-buffer (handle)
  `(nth 0 ,handle))
(defmacro mm-handle-type (handle)
  `(nth 1 ,handle))
(defsubst mm-handle-media-type (handle)
  (if (stringp (car handle))
      (car handle)
    (car (mm-handle-type handle))))
(defsubst mm-handle-media-supertype (handle)
  (car (split-string (mm-handle-media-type handle) "/")))
(defsubst mm-handle-media-subtype (handle)
  (cadr (split-string (mm-handle-media-type handle) "/")))
(defmacro mm-handle-encoding (handle)
  `(nth 2 ,handle))
(defmacro mm-handle-undisplayer (handle)
  `(nth 3 ,handle))
(defmacro mm-handle-set-undisplayer (handle function)
  `(setcar (nthcdr 3 ,handle) ,function))
(defmacro mm-handle-disposition (handle)
  `(nth 4 ,handle))
(defmacro mm-handle-description (handle)
  `(nth 5 ,handle))
(defmacro mm-handle-cache (handle)
  `(nth 6 ,handle))
(defmacro mm-handle-set-cache (handle contents)
  `(setcar (nthcdr 6 ,handle) ,contents))
(defmacro mm-handle-id (handle)
  `(nth 7 ,handle))
(defmacro mm-handle-multipart-original-buffer (handle)
  `(get-text-property 0 'buffer (car ,handle)))
(defmacro mm-handle-multipart-from (handle)
  `(get-text-property 0 'from (car ,handle)))
(defmacro mm-handle-multipart-ctl-parameter (handle parameter)
  `(get-text-property 0 ,parameter (car ,handle)))

(defmacro mm-make-handle (&optional buffer type encoding undisplayer
				    disposition description cache
				    id)
  `(list ,buffer ,type ,encoding ,undisplayer
	 ,disposition ,description ,cache ,id))

(defcustom mm-text-html-renderer
  (cond ((fboundp 'libxml-parse-html-region) 'shr)
	((executable-find "w3m") 'gnus-w3m)
	((executable-find "links") 'links)
	((executable-find "lynx") 'lynx)
	((locate-library "w3") 'w3)
	((locate-library "html2text") 'html2text)
	(t nil))
  "Render of HTML contents.
It is one of defined renderer types, or a rendering function.
The defined renderer types are:
`shr': use the built-in Gnus HTML renderer;
`gnus-w3m': use Gnus renderer based on w3m;
`w3m': use emacs-w3m;
`w3m-standalone': use plain w3m;
`links': use links;
`lynx': use lynx;
`w3': use Emacs/W3;
`html2text': use html2text;
nil    : use external viewer (default web browser)."
  :version "24.1"
  :type '(choice (const shr)
                 (const gnus-w3m)
                 (const w3)
                 (const w3m :tag "emacs-w3m")
		 (const w3m-standalone :tag "standalone w3m" )
		 (const links)
		 (const lynx)
		 (const html2text)
		 (const nil :tag "External viewer")
		 (function))
  :group 'mime-display)

(defcustom mm-inline-text-html-with-images nil
  "If non-nil, Gnus will allow retrieving images in HTML contents with
the <img> tags.  It has no effect on Emacs/w3.  See also the
documentation for the `mm-w3m-safe-url-regexp' variable."
  :version "22.1"
  :type 'boolean
  :group 'mime-display)

(defcustom mm-w3m-safe-url-regexp "\\`cid:"
  "Regexp matching URLs which are considered to be safe.
Some HTML mails might contain a nasty trick used by spammers, using
the <img> tag which is far more evil than the [Click Here!] button.
It is most likely intended to check whether the ominous spam mail has
reached your eyes or not, in which case the spammer knows for sure
that your email address is valid.  It is done by embedding an
identifier string into a URL that you might automatically retrieve
when displaying the image.  The default value is \"\\\\`cid:\" which only
matches parts embedded to the Multipart/Related type MIME contents and
Gnus will never connect to the spammer's site arbitrarily.  You may
set this variable to nil if you consider all urls to be safe."
  :version "22.1"
  :type '(choice (regexp :tag "Regexp")
		 (const :tag "All URLs are safe" nil))
  :group 'mime-display)

(defcustom mm-inline-text-html-with-w3m-keymap t
  "If non-nil, use emacs-w3m command keys in the article buffer."
  :version "22.1"
  :type 'boolean
  :group 'mime-display)

(defcustom mm-enable-external t
  "Indicate whether external MIME handlers should be used.

If t, all defined external MIME handlers are used.  If nil, files are saved by
`mailcap-save-binary-file'.  If it is the symbol `ask', you are prompted
before the external MIME handler is invoked."
  :version "22.1"
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :group 'mime-display)

(defcustom mm-inline-media-tests
  '(("image/p?jpeg"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'jpeg handle)))
    ("image/png"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'png handle)))
    ("image/gif"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'gif handle)))
    ("image/tiff"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'tiff handle)))
    ("image/xbm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/x-xbitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/xpm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/x-xpixmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/bmp"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'bmp handle)))
    ("image/x-portable-bitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'pbm handle)))
    ("text/plain" mm-inline-text identity)
    ("text/enriched" mm-inline-text identity)
    ("text/richtext" mm-inline-text identity)
    ("text/x-patch" mm-display-patch-inline identity)
    ;; In case mime.types uses x-diff (as does Debian's mime-support-3.40).
    ("text/x-diff" mm-display-patch-inline identity)
    ("application/emacs-lisp" mm-display-elisp-inline identity)
    ("application/x-emacs-lisp" mm-display-elisp-inline identity)
    ("application/x-shellscript" mm-display-shell-script-inline identity)
    ("application/x-sh" mm-display-shell-script-inline identity)
    ("text/x-sh" mm-display-shell-script-inline identity)
    ("application/javascript" mm-display-javascript-inline identity)
    ("text/dns" mm-display-dns-inline identity)
    ("text/x-org" mm-display-org-inline identity)
    ("text/html"
     mm-inline-text-html
     (lambda (handle)
       mm-text-html-renderer))
    ("text/x-vcard"
     mm-inline-text-vcard
     (lambda (handle)
       (or (featurep 'vcard)
	   (locate-library "vcard"))))
    ("message/delivery-status" mm-inline-text identity)
    ("message/rfc822" mm-inline-message identity)
    ("message/partial" mm-inline-partial identity)
    ("message/external-body" mm-inline-external-body identity)
    ("text/.*" mm-inline-text identity)
    ("audio/wav" mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
	    (device-sound-enabled-p))))
    ("audio/au"
     mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
	    (device-sound-enabled-p))))
    ("application/pgp-signature" ignore identity)
    ("application/x-pkcs7-signature" ignore identity)
    ("application/pkcs7-signature" ignore identity)
    ("application/x-pkcs7-mime" ignore identity)
    ("application/pkcs7-mime" ignore identity)
    ("multipart/alternative" ignore identity)
    ("multipart/mixed" ignore identity)
    ("multipart/related" ignore identity)
    ("image/.*"
     mm-inline-image
     (lambda (handle)
       (and (mm-valid-image-format-p 'imagemagick)
	    (mm-with-unibyte-buffer
	      (mm-insert-part handle)
	      (let ((image
		     (ignore-errors
		       (if (fboundp 'create-image)
			   (create-image (buffer-string) 'imagemagick 'data-p)
			 (mm-create-image-xemacs (mm-handle-media-subtype handle))))))
		(when image
		  (setcar (cdr handle) (list "image/imagemagick"))
		  (mm-image-fit-p handle)))))))
    ;; Disable audio and image
    ("audio/.*" ignore ignore)
    ("image/.*" ignore ignore)
    ;; Default to displaying as text
    (".*" mm-inline-text mm-readable-p))
  "Alist of media types/tests saying whether types can be displayed inline."
  :type '(repeat (list (regexp :tag "MIME type")
		       (function :tag "Display function")
		       (function :tag "Display test")))
  :group 'mime-display)

(defcustom mm-inlined-types
  '("image/.*" "text/.*" "message/delivery-status" "message/rfc822"
    "message/partial" "message/external-body" "application/emacs-lisp"
    "application/x-emacs-lisp"
    "application/pgp-signature" "application/x-pkcs7-signature"
    "application/pkcs7-signature" "application/x-pkcs7-mime"
    "application/pkcs7-mime"
    ;; Mutt still uses this even though it has already been withdrawn.
    "application/pgp")
  "List of media types that are to be displayed inline.
See also `mm-inline-media-tests', which says how to display a media
type inline."
  :type '(repeat regexp)
  :group 'mime-display)

(defcustom mm-keep-viewer-alive-types
  '("application/postscript" "application/msword" "application/vnd.ms-excel"
    "application/pdf" "application/x-dvi")
  "List of media types for which the external viewer will not be killed
when selecting a different article."
  :version "22.1"
  :type '(repeat regexp)
  :group 'mime-display)

(defcustom mm-automatic-display
  '("text/plain" "text/enriched" "text/richtext" "text/html" "text/x-verbatim"
    "text/x-vcard" "image/.*" "message/delivery-status" "multipart/.*"
    "message/rfc822" "text/x-patch" "text/dns" "application/pgp-signature"
    "application/emacs-lisp" "application/x-emacs-lisp"
    "application/x-pkcs7-signature"
    "application/pkcs7-signature" "application/x-pkcs7-mime"
    "application/pkcs7-mime"
    ;; Mutt still uses this even though it has already been withdrawn.
    "application/pgp\\'"
     "text/x-org")
  "A list of MIME types to be displayed automatically."
  :type '(repeat regexp)
  :group 'mime-display)

(defcustom mm-attachment-override-types '("text/x-vcard"
					  "application/pkcs7-mime"
					  "application/x-pkcs7-mime"
					  "application/pkcs7-signature"
					  "application/x-pkcs7-signature")
  "Types to have \"attachment\" ignored if they can be displayed inline."
  :type '(repeat regexp)
  :group 'mime-display)

(defcustom mm-inline-override-types nil
  "Types to be treated as attachments even if they can be displayed inline."
  :type '(repeat regexp)
  :group 'mime-display)

(defcustom mm-automatic-external-display nil
  "List of MIME type regexps that will be displayed externally automatically."
  :type '(repeat regexp)
  :group 'mime-display)

(defcustom mm-discouraged-alternatives nil
  "List of MIME types that are discouraged when viewing multipart/alternative.
Viewing agents are supposed to view the last possible part of a message,
as that is supposed to be the richest.  However, users may prefer other
types instead, and this list says what types are most unwanted.  If,
for instance, text/html parts are very unwanted, and text/richtext are
somewhat unwanted, then the value of this variable should be set
to:

 (\"text/html\" \"text/richtext\")

Adding \"image/.*\" might also be useful.  Spammers use it as the
preferred part of multipart/alternative messages.  See also
`gnus-buttonized-mime-types', to which adding \"multipart/alternative\"
enables you to choose manually one of two types those mails include."
  :type '(repeat regexp) ;; See `mm-preferred-alternative-precedence'.
  :group 'mime-display)

(defcustom mm-tmp-directory
  (if (fboundp 'temp-directory)
      (temp-directory)
    (if (boundp 'temporary-file-directory)
	temporary-file-directory
      "/tmp/"))
  "Where mm will store its temporary files."
  :type 'directory
  :group 'mime-display)

(defcustom mm-inline-large-images nil
  "If t, then all images fit in the buffer.
If 'resize, try to resize the images so they fit."
  :type '(radio
          (const :tag "Inline large images as they are." t)
          (const :tag "Resize large images." resize)
          (const :tag "Do not inline large images." nil))
  :group 'mime-display)

(defcustom mm-file-name-rewrite-functions
  '(mm-file-name-delete-control mm-file-name-delete-gotchas)
  "List of functions used for rewriting file names of MIME parts.
Each function takes a file name as input and returns a file name.

Ready-made functions include `mm-file-name-delete-control',
`mm-file-name-delete-gotchas' (you should not remove these two
functions), `mm-file-name-delete-whitespace',
`mm-file-name-trim-whitespace', `mm-file-name-collapse-whitespace',
`mm-file-name-replace-whitespace', `capitalize', `downcase',
`upcase', and `upcase-initials'."
  :type '(list (set :inline t
		    (const mm-file-name-delete-control)
		    (const mm-file-name-delete-gotchas)
		    (const mm-file-name-delete-whitespace)
		    (const mm-file-name-trim-whitespace)
		    (const mm-file-name-collapse-whitespace)
		    (const mm-file-name-replace-whitespace)
		    (const capitalize)
		    (const downcase)
		    (const upcase)
		    (const upcase-initials)
	       (repeat :inline t
		       :tag "Function"
		       function)))
  :version "23.1" ;; No Gnus
  :group 'mime-display)


(defvar mm-path-name-rewrite-functions nil
  "*List of functions for rewriting the full file names of MIME parts.
This is used when viewing parts externally, and is meant for
transforming the absolute name so that non-compliant programs can find
the file where it's saved.

Each function takes a file name as input and returns a file name.")

(defvar mm-file-name-replace-whitespace nil
  "String used for replacing whitespace characters; default is `\"_\"'.")

(defcustom mm-default-directory nil
  "The default directory where mm will save files.
If not set, `default-directory' will be used."
  :type '(choice directory (const :tag "Default" nil))
  :group 'mime-display)

(defcustom mm-attachment-file-modes 384
  "Set the mode bits of saved attachments to this integer."
  :version "22.1"
  :type 'integer
  :group 'mime-display)

(defcustom mm-external-terminal-program "xterm"
  "The program to start an external terminal."
  :version "22.1"
  :type 'string
  :group 'mime-display)

;;; Internal variables.

(defvar mm-last-shell-command "")
(defvar mm-content-id-alist nil)
(defvar mm-postponed-undisplay-list nil)

;; According to RFC2046, in particular, in a digest, the default
;; Content-Type value for a body part is changed from "text/plain" to
;; "message/rfc822".
(defvar mm-dissect-default-type "text/plain")

(autoload 'mml2015-verify "mml2015")
(autoload 'mml2015-verify-test "mml2015")
(autoload 'mml-smime-verify "mml-smime")
(autoload 'mml-smime-verify-test "mml-smime")

(defvar mm-verify-function-alist
  '(("application/pgp-signature" mml2015-verify "PGP" mml2015-verify-test)
    ("application/x-gnus-pgp-signature" mm-uu-pgp-signed-extract-1 "PGP"
     mm-uu-pgp-signed-test)
    ("application/pkcs7-signature" mml-smime-verify "S/MIME"
     mml-smime-verify-test)
    ("application/x-pkcs7-signature" mml-smime-verify "S/MIME"
     mml-smime-verify-test)))

(defcustom mm-verify-option 'never
  "Option of verifying signed parts.
`never', not verify; `always', always verify;
`known', only verify known protocols.  Otherwise, ask user.

When set to `always' or `known', you should add
\"multipart/signed\" to `gnus-buttonized-mime-types' to see
result of the verification."
  :version "22.1"
  :type '(choice (item always)
		 (item never)
		 (item :tag "only known protocols" known)
		 (item :tag "ask" nil))
  :group 'mime-security)

(autoload 'mml2015-decrypt "mml2015")
(autoload 'mml2015-decrypt-test "mml2015")

(defvar mm-decrypt-function-alist
  '(("application/pgp-encrypted" mml2015-decrypt "PGP" mml2015-decrypt-test)
    ("application/x-gnus-pgp-encrypted" mm-uu-pgp-encrypted-extract-1 "PGP"
     mm-uu-pgp-encrypted-test)))

(defcustom mm-decrypt-option nil
  "Option of decrypting encrypted parts.
`never', not decrypt; `always', always decrypt;
`known', only decrypt known protocols.  Otherwise, ask user."
  :version "22.1"
  :type '(choice (item always)
		 (item never)
		 (item :tag "only known protocols" known)
		 (item :tag "ask" nil))
  :group 'mime-security)

(defvar mm-viewer-completion-map
  (let ((map (make-sparse-keymap 'mm-viewer-completion-map)))
    (set-keymap-parent map minibuffer-local-completion-map)
    ;; Should we bind other key to minibuffer-complete-word?
    (define-key map " " 'self-insert-command)
    map)
  "Keymap for input viewer with completion.")

(defvar mm-viewer-completion-map
  (let ((map (make-sparse-keymap 'mm-viewer-completion-map)))
    (set-keymap-parent map minibuffer-local-completion-map)
    ;; Should we bind other key to minibuffer-complete-word?
    (define-key map " " 'self-insert-command)
    map)
  "Keymap for input viewer with completion.")

;;; The functions.

(defun mm-alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(defun mm-keep-viewer-alive-p (handle)
  "Say whether external viewer for HANDLE should stay alive."
  (let ((types mm-keep-viewer-alive-types)
	(type (mm-handle-media-type handle))
	ty)
    (catch 'found
      (while (setq ty (pop types))
	(when (string-match ty type)
	  (throw 'found t))))))

(defun mm-handle-set-external-undisplayer (handle function)
  "Set the undisplayer for HANDLE to FUNCTION.
Postpone undisplaying of viewers for types in
`mm-keep-viewer-alive-types'."
  (if (mm-keep-viewer-alive-p handle)
      (let ((new-handle (copy-sequence handle)))
	(mm-handle-set-undisplayer new-handle function)
	(mm-handle-set-undisplayer handle nil)
	(push new-handle mm-postponed-undisplay-list))
    (mm-handle-set-undisplayer handle function)))

(defun mm-destroy-postponed-undisplay-list ()
  (when mm-postponed-undisplay-list
    (message "Destroying external MIME viewers")
    (mm-destroy-parts mm-postponed-undisplay-list)))

(autoload 'message-fetch-field "message")

(defun mm-dissect-buffer (&optional no-strict-mime loose-mime from)
  "Dissect the current buffer and return a list of MIME handles."
  (save-excursion
    (let (ct ctl type subtype cte cd description id result)
      (save-restriction
	(mail-narrow-to-head)
	(when (or no-strict-mime
		  loose-mime
		  (mail-fetch-field "mime-version"))
	  (setq ct (mail-fetch-field "content-type")
		ctl (and ct (mail-header-parse-content-type ct))
		cte (mail-fetch-field "content-transfer-encoding")
                cd (or (mail-fetch-field "content-disposition")
                       (when (and ctl
                                  (eq 'mm-inline-text
                                      (cadr (mm-assoc-string-match
                                             mm-inline-media-tests
                                             (car ctl)))))
                         "inline"))
		;; Newlines in description should be stripped so as
		;; not to break the MIME tag into two or more lines.
		description (message-fetch-field "content-description")
		id (mail-fetch-field "content-id"))
	  (unless from
	    (setq from (mail-fetch-field "from")))
	  ;; FIXME: In some circumstances, this code is running within
	  ;; an unibyte macro.  mail-extract-address-components
	  ;; creates unibyte buffers. This `if', though not a perfect
	  ;; solution, avoids most of them.
	  (if from
	      (setq from (cadr (mail-extract-address-components from))))
	  (if description
	      (setq description (mail-decode-encoded-word-string
				 description)))))
      (if (or (not ctl)
	      (not (string-match "/" (car ctl))))
	  (mm-dissect-singlepart
	   (list mm-dissect-default-type)
	   (and cte (intern (downcase (mail-header-strip cte))))
	   no-strict-mime
	   (and cd (mail-header-parse-content-disposition cd))
	   description)
	(setq type (split-string (car ctl) "/"))
	(setq subtype (cadr type)
	      type (car type))
	(setq
	 result
	 (cond
	  ((equal type "multipart")
	   (let ((mm-dissect-default-type (if (equal subtype "digest")
					      "message/rfc822"
					    "text/plain"))
		 (start (cdr (assq 'start (cdr ctl)))))
	     (add-text-properties 0 (length (car ctl))
				  (mm-alist-to-plist (cdr ctl)) (car ctl))

	     ;; what really needs to be done here is a way to link a
	     ;; MIME handle back to it's parent MIME handle (in a multilevel
	     ;; MIME article).  That would probably require changing
	     ;; the mm-handle API so we simply store the multipart buffer
	     ;; name as a text property of the "multipart/whatever" string.
	     (add-text-properties 0 (length (car ctl))
				  (list 'buffer (mm-copy-to-buffer)
					'from from
					'start start)
				  (car ctl))
	     (cons (car ctl) (mm-dissect-multipart ctl from))))
	  (t
	   (mm-possibly-verify-or-decrypt
	    (mm-dissect-singlepart
	     ctl
	     (and cte (intern (downcase (mail-header-strip cte))))
	     no-strict-mime
	     (and cd (mail-header-parse-content-disposition cd))
	     description id)
	    ctl from))))
	(when id
	  (when (string-match " *<\\(.*\\)> *" id)
	    (setq id (match-string 1 id)))
	  (push (cons id result) mm-content-id-alist))
	result))))

(defun mm-dissect-singlepart (ctl cte &optional force cdl description id)
  (when (or force
	    (if (equal "text/plain" (car ctl))
		(assoc 'format ctl)
	      t))
    (mm-make-handle
     (mm-copy-to-buffer) ctl cte nil cdl description nil id)))

(defun mm-dissect-multipart (ctl from)
  (goto-char (point-min))
  (let* ((boundary (concat "\n--" (mail-content-type-get ctl 'boundary)))
	 (close-delimiter (concat (regexp-quote boundary) "--[ \t]*$"))
	 start parts
	 (end (save-excursion
		(goto-char (point-max))
		(if (re-search-backward close-delimiter nil t)
		    (match-beginning 0)
		  (point-max)))))
    (setq boundary (concat (regexp-quote boundary) "[ \t]*$"))
    (while (and (< (point) end) (re-search-forward boundary end t))
      (goto-char (match-beginning 0))
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (setq parts (nconc (list (mm-dissect-buffer t nil from)) parts)))))
      (end-of-line 2)
      (or (looking-at boundary)
	  (forward-line 1))
      (setq start (point)))
    (when (and start (< start end))
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (setq parts (nconc (list (mm-dissect-buffer t nil from)) parts)))))
    (mm-possibly-verify-or-decrypt (nreverse parts) ctl from)))

(defun mm-copy-to-buffer ()
  "Copy the contents of the current buffer to a fresh buffer."
  (let ((obuf (current-buffer))
        (mb (mm-multibyte-p))
        beg)
    (goto-char (point-min))
    (search-forward-regexp "^\n" nil t)
    (setq beg (point))
    (with-current-buffer
          (generate-new-buffer " *mm*")
      ;; Preserve the data's unibyteness (for url-insert-file-contents).
      (mm-set-buffer-multibyte mb)
      (insert-buffer-substring obuf beg)
      (current-buffer))))

(defun mm-display-parts (handle &optional no-default)
  (if (stringp (car handle))
      (mapcar 'mm-display-parts (cdr handle))
    (if (bufferp (car handle))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mm-display-part handle)
	  (goto-char (point-max)))
      (mapcar 'mm-display-parts handle))))

(autoload 'mailcap-parse-mailcaps "mailcap")
(autoload 'mailcap-mime-info "mailcap")

(defun mm-display-part (handle &optional no-default force)
  "Display the MIME part represented by HANDLE.
Returns nil if the part is removed; inline if displayed inline;
external if displayed external."
  (save-excursion
    (mailcap-parse-mailcaps)
    (if (and (not force)
	     (mm-handle-displayed-p handle))
	(mm-remove-part handle)
      (let* ((ehandle (if (equal (mm-handle-media-type handle)
				 "message/external-body")
			  (progn
			    (unless (mm-handle-cache handle)
			      (mm-extern-cache-contents handle))
			    (mm-handle-cache handle))
			handle))
	     (type (mm-handle-media-type ehandle))
	     (method (mailcap-mime-info type))
	     (filename (or (mail-content-type-get
			    (mm-handle-disposition handle) 'filename)
			   (mail-content-type-get
			    (mm-handle-type handle) 'name)
			   "<file>"))
	     (external mm-enable-external))
	(if (and (mm-inlinable-p ehandle)
		 (mm-inlined-p ehandle))
	    (progn
	      (forward-line 1)
	      (mm-display-inline handle)
	      'inline)
	  (when (or method
		    (not no-default))
	    (if (and (not method)
		     (equal "text" (car (split-string type "/"))))
		(progn
		  (forward-line 1)
		  (mm-insert-inline handle (mm-get-part handle))
		  'inline)
	      (setq external
                    (and method ;; If nil, we always use "save".
		       (stringp method) ;; 'mailcap-save-binary-file
		       (or (eq mm-enable-external t)
			   (and (eq mm-enable-external 'ask)
				(y-or-n-p
				 (concat
				  "Display part (" type
				  ") using external program"
				  ;; Can non-string method ever happen?
				  (if (stringp method)
				      (concat
				       " \"" (format method filename) "\"")
				    "")
                                    "? "))))))
	      (if external
		  (mm-display-external
		   handle (or method 'mailcap-save-binary-file))
		(mm-display-external
		 handle 'mailcap-save-binary-file)))))))))

(declare-function gnus-configure-windows "gnus-win" (setting &optional force))
(defvar mailcap-mime-extensions)	; mailcap-mime-info autoloads

(defun mm-display-external (handle method)
  "Display HANDLE using METHOD."
  (let ((outbuf (current-buffer)))
    (mm-with-unibyte-buffer
      (if (functionp method)
	  (let ((cur (current-buffer)))
	    (if (eq method 'mailcap-save-binary-file)
		(progn
		  (set-buffer (generate-new-buffer " *mm*"))
		  (setq method nil))
	      (mm-insert-part handle)
	      (mm-add-meta-html-tag handle)
	      (let ((win (get-buffer-window cur t)))
		(when win
		  (select-window win)))
	      (switch-to-buffer (generate-new-buffer " *mm*")))
	    (buffer-disable-undo)
	    (mm-set-buffer-file-coding-system mm-binary-coding-system)
	    (insert-buffer-substring cur)
	    (goto-char (point-min))
	    (when method
	      (message "Viewing with %s" method))
	    (let ((mm (current-buffer))
		  (non-viewer (assq 'non-viewer
				    (mailcap-mime-info
				     (mm-handle-media-type handle) t))))
	      (unwind-protect
		  (if method
		      (funcall method)
		    (mm-save-part handle))
		(when (and (not non-viewer)
			   method)
		  (mm-handle-set-undisplayer handle mm)))))
	;; The function is a string to be executed.
	(mm-insert-part handle)
	(mm-add-meta-html-tag handle)
	(let* ((dir (mm-make-temp-file
		     (expand-file-name "emm." mm-tmp-directory) 'dir))
	       (filename (or
			  (mail-content-type-get
			   (mm-handle-disposition handle) 'filename)
			  (mail-content-type-get
			   (mm-handle-type handle) 'name)))
	       (mime-info (mailcap-mime-info
			   (mm-handle-media-type handle) t))
	       (needsterm (or (assoc "needsterm" mime-info)
			      (assoc "needsterminal" mime-info)))
	       (copiousoutput (assoc "copiousoutput" mime-info))
	       file buffer)
	  ;; We create a private sub-directory where we store our files.
	  (set-file-modes dir #o700)
	  (if filename
	      (setq file (expand-file-name
			  (gnus-map-function mm-file-name-rewrite-functions
					     (file-name-nondirectory filename))
			  dir))
	    ;; Use nametemplate (defined in RFC1524) if it is specified
	    ;; in mailcap.
	    (let ((suffix (cdr (assoc "nametemplate" mime-info))))
	      (if (and suffix
		       (string-match "\\`%s\\(\\..+\\)\\'" suffix))
		  (setq suffix (match-string 1 suffix))
		;; Otherwise, use a suffix according to
		;; `mailcap-mime-extensions'.
		(setq suffix (car (rassoc (mm-handle-media-type handle)
					  mailcap-mime-extensions))))
	      (setq file (mm-make-temp-file (expand-file-name "mm." dir)
					    nil suffix))))
	  (let ((coding-system-for-write mm-binary-coding-system))
	    (write-region (point-min) (point-max) file nil 'nomesg))
	  ;; The file is deleted after the viewer exists.  If the users edits
	  ;; the file, changes will be lost.  Set file to read-only to make it
	  ;; clear.
	  (set-file-modes file #o400)
	  (message "Viewing with %s" method)
	  (cond
	   (needsterm
	    (let ((command (mm-mailcap-command
			    method file (mm-handle-type handle))))
	      (unwind-protect
		  (if window-system
		      (start-process "*display*" nil
				     mm-external-terminal-program
				     "-e" shell-file-name
				     shell-command-switch command)
		    (require 'term)
		    (require 'gnus-win)
		    (set-buffer
		     (setq buffer
			   (make-term "display"
				      shell-file-name
				      nil
				      shell-command-switch command)))
		    (term-mode)
		    (term-char-mode)
		    (set-process-sentinel
		     (get-buffer-process buffer)
		     `(lambda (process state)
			(if (eq 'exit (process-status process))
			    (gnus-configure-windows
			     ',gnus-current-window-configuration))))
		    (gnus-configure-windows 'display-term))
		(mm-handle-set-external-undisplayer handle (cons file buffer)))
	      (message "Displaying %s..." command))
	    'external)
	   (copiousoutput
	    (with-current-buffer outbuf
	      (forward-line 1)
	      (mm-insert-inline
	       handle
	       (unwind-protect
		   (progn
		     (call-process shell-file-name nil
				   (setq buffer
					 (generate-new-buffer " *mm*"))
				   nil
				   shell-command-switch
				   (mm-mailcap-command
				    method file (mm-handle-type handle)))
		     (if (buffer-live-p buffer)
			 (with-current-buffer buffer
			   (buffer-string))))
		 (progn
		   (ignore-errors (delete-file file))
		   (ignore-errors (delete-directory
				   (file-name-directory file)))
		   (ignore-errors (kill-buffer buffer))))))
	    'inline)
	   (t
	    ;; Deleting the temp file should be postponed for some wrappers,
	    ;; shell scripts, and so on, which might exit right after having
	    ;; started a viewer command as a background job.
	    (let ((command (mm-mailcap-command
			    method file (mm-handle-type handle))))
	      (unwind-protect
		  (progn
		    (start-process "*display*"
				   (setq buffer
					 (generate-new-buffer " *mm*"))
				   shell-file-name
				   shell-command-switch command)
		    (set-process-sentinel
		     (get-buffer-process buffer)
		     (lexical-let ;; Don't use `let'.
			 ;; Function used to remove temp file and directory.
			 ((fn `(lambda nil
				 ;; Don't use `ignore-errors'.
				 (condition-case nil
				     (delete-file ,file)
				   (error))
				 (condition-case nil
				     (delete-directory
				      ,(file-name-directory file))
				   (error))))
			  ;; Form uses to kill the process buffer and
			  ;; remove the undisplayer.
			  (fm `(progn
				 (kill-buffer ,buffer)
				 ,(macroexpand
				   (list 'mm-handle-set-undisplayer
					 (list 'quote handle)
					 nil))))
			  ;; Message to be issued when the process exits.
			  (done (format "Displaying %s...done" command))
			  ;; In particular, the timer object (which is
			  ;; a vector in Emacs but is a list in XEmacs)
			  ;; requires that it is lexically scoped.
			  (timer (run-at-time 30.0 nil 'ignore)))
		       (if (featurep 'xemacs)
			   (lambda (process state)
			     (when (eq 'exit (process-status process))
			       (if (memq timer itimer-list)
				   (set-itimer-function timer fn)
				 (funcall fn))
			       (ignore-errors (eval fm))
			       (message "%s" done)))
			 (lambda (process state)
			   (when (eq 'exit (process-status process))
			     (if (memq timer timer-list)
				 (timer-set-function timer fn)
			       (funcall fn))
			     (ignore-errors (eval fm))
			     (message "%s" done)))))))
		(mm-handle-set-external-undisplayer
		 handle (cons file buffer)))
	      (message "Displaying %s..." command))
	    'external)))))))

(defun mm-mailcap-command (method file type-list)
  (let ((ctl (cdr type-list))
	(beg 0)
	(uses-stdin t)
	out sub total)
    (while (string-match "%{\\([^}]+\\)}\\|'%s'\\|\"%s\"\\|%s\\|%t\\|%%"
			 method beg)
      (push (substring method beg (match-beginning 0)) out)
      (setq beg (match-end 0)
	    total (match-string 0 method)
	    sub (match-string 1 method))
      (cond
       ((string= total "%%")
	(push "%" out))
       ((or (string= total "%s")
	    ;; We do our own quoting.
	    (string= total "'%s'")
	    (string= total "\"%s\""))
	(setq uses-stdin nil)
	(push (shell-quote-argument
	       (gnus-map-function mm-path-name-rewrite-functions file)) out))
       ((string= total "%t")
	(push (shell-quote-argument (car type-list)) out))
       (t
	(push (shell-quote-argument (or (cdr (assq (intern sub) ctl)) "")) out))))
    (push (substring method beg (length method)) out)
    (when uses-stdin
      (push "<" out)
      (push (shell-quote-argument
	     (gnus-map-function mm-path-name-rewrite-functions file))
	    out))
    (mapconcat 'identity (nreverse out) "")))

(defun mm-remove-parts (handles)
  "Remove the displayed MIME parts represented by HANDLES."
  (if (and (listp handles)
	   (bufferp (car handles)))
      (mm-remove-part handles)
    (let (handle)
      (while (setq handle (pop handles))
	(cond
	 ((stringp handle)
	  (when (buffer-live-p (get-text-property 0 'buffer handle))
	    (kill-buffer (get-text-property 0 'buffer handle))))
	 ((and (listp handle)
	       (stringp (car handle)))
	  (mm-remove-parts (cdr handle)))
	 (t
	  (mm-remove-part handle)))))))

(defun mm-destroy-parts (handles)
  "Remove the displayed MIME parts represented by HANDLES."
  (if (and (listp handles)
	   (bufferp (car handles)))
      (mm-destroy-part handles)
    (let (handle)
      (while (setq handle (pop handles))
	(cond
	 ((stringp handle)
	  (when (buffer-live-p (get-text-property 0 'buffer handle))
	    (kill-buffer (get-text-property 0 'buffer handle))))
	 ((and (listp handle)
	       (stringp (car handle)))
	  (mm-destroy-parts handle))
	 (t
	  (mm-destroy-part handle)))))))

(defun mm-remove-part (handle)
  "Remove the displayed MIME part represented by HANDLE."
  (when (listp handle)
    (let ((object (mm-handle-undisplayer handle)))
      (ignore-errors
	(cond
	 ;; Internally displayed part.
	 ((mm-annotationp object)
          (if (featurep 'xemacs)
              (delete-annotation object)))
	 ((or (functionp object)
	      (and (listp object)
		   (eq (car object) 'lambda)))
	  (funcall object))
	 ;; Externally displayed part.
	 ((consp object)
	  (condition-case ()
	      (while (get-buffer-process (cdr object))
		(interrupt-process (get-buffer-process (cdr object)))
		(message "Waiting for external displayer to die...")
		(sit-for 1))
	    (quit)
	    (error))
	  (ignore-errors (and (cdr object) (kill-buffer (cdr object))))
	  (message "Waiting for external displayer to die...done")
	  (ignore-errors (delete-file (car object)))
	  (ignore-errors (delete-directory (file-name-directory
					    (car object)))))
	 ((bufferp object)
	  (when (buffer-live-p object)
	    (kill-buffer object)))))
      (mm-handle-set-undisplayer handle nil))))

(defun mm-display-inline (handle)
  (let* ((type (mm-handle-media-type handle))
	 (function (cadr (mm-assoc-string-match mm-inline-media-tests type))))
    (funcall function handle)
    (goto-char (point-min))))

(defun mm-assoc-string-match (alist type)
  (dolist (elem alist)
    (when (string-match (car elem) type)
      (return elem))))

(defun mm-automatic-display-p (handle)
  "Say whether the user wants HANDLE to be displayed automatically."
  (let ((methods mm-automatic-display)
	(type (mm-handle-media-type handle))
	method result)
    (while (setq method (pop methods))
      (when (and (not (mm-inline-override-p handle))
		 (string-match method type))
	(setq result t
	      methods nil)))
    result))

(defun mm-inlinable-p (handle &optional type)
  "Say whether HANDLE can be displayed inline.
TYPE is the mime-type of the object; it defaults to the one given
in HANDLE."
  (unless type (setq type (mm-handle-media-type handle)))
  (let ((alist mm-inline-media-tests)
	test)
    (while alist
      (when (string-match (caar alist) type)
	(setq test (caddar alist)
	      alist nil)
	(setq test (funcall test handle)))
      (pop alist))
    test))

(defun mm-inlined-p (handle)
  "Say whether the user wants HANDLE to be displayed inline."
  (let ((methods mm-inlined-types)
	(type (mm-handle-media-type handle))
	method result)
    (while (setq method (pop methods))
      (when (and (not (mm-inline-override-p handle))
		 (string-match method type))
	(setq result t
	      methods nil)))
    result))

(defun mm-attachment-override-p (handle)
  "Say whether HANDLE should have attachment behavior overridden."
  (let ((types mm-attachment-override-types)
	(type (mm-handle-media-type handle))
	ty)
    (catch 'found
      (while (setq ty (pop types))
	(when (and (string-match ty type)
		   (mm-inlinable-p handle))
	  (throw 'found t))))))

(defun mm-inline-override-p (handle)
  "Say whether HANDLE should have inline behavior overridden."
  (let ((types mm-inline-override-types)
	(type (mm-handle-media-type handle))
	ty)
    (catch 'found
      (while (setq ty (pop types))
	(when (string-match ty type)
	  (throw 'found t))))))

(defun mm-automatic-external-display-p (type)
  "Return the user-defined method for TYPE."
  (let ((methods mm-automatic-external-display)
	method result)
    (while (setq method (pop methods))
      (when (string-match method type)
	(setq result t
	      methods nil)))
    result))

(defun mm-destroy-part (handle)
  "Destroy the data structures connected to HANDLE."
  (when (listp handle)
    (mm-remove-part handle)
    (when (buffer-live-p (mm-handle-buffer handle))
      (kill-buffer (mm-handle-buffer handle)))))

(defun mm-handle-displayed-p (handle)
  "Say whether HANDLE is displayed or not."
  (mm-handle-undisplayer handle))

;;;
;;; Functions for outputting parts
;;;

(defmacro mm-with-part (handle &rest forms)
  "Run FORMS in the temp buffer containing the contents of HANDLE."
  ;; The handle-buffer's content is a sequence of bytes, not a sequence of
  ;; chars, so the buffer should be unibyte.  It may happen that the
  ;; handle-buffer is multibyte for some reason, in which case now is a good
  ;; time to adjust it, since we know at this point that it should
  ;; be unibyte.
  `(let* ((handle ,handle))
     (when (and (mm-handle-buffer handle)
		(buffer-name (mm-handle-buffer handle)))
       (with-temp-buffer
	 (mm-disable-multibyte)
	 (insert-buffer-substring (mm-handle-buffer handle))
	 (mm-decode-content-transfer-encoding
	  (mm-handle-encoding handle)
	  (mm-handle-media-type handle))
	 ,@forms))))
(put 'mm-with-part 'lisp-indent-function 1)
(put 'mm-with-part 'edebug-form-spec '(body))

(defun mm-get-part (handle &optional no-cache)
  "Return the contents of HANDLE as a string.
If NO-CACHE is non-nil, cached contents of a message/external-body part
are ignored."
  (if (and (not no-cache)
	   (equal (mm-handle-media-type handle) "message/external-body"))
      (progn
	(unless (mm-handle-cache handle)
	  (mm-extern-cache-contents handle))
	(with-current-buffer (mm-handle-buffer (mm-handle-cache handle))
	  (buffer-string)))
    (mm-with-part handle
      (buffer-string))))

(defun mm-insert-part (handle &optional no-cache)
  "Insert the contents of HANDLE in the current buffer.
If NO-CACHE is non-nil, cached contents of a message/external-body part
are ignored."
  (let ((text (cond ((eq (mail-content-type-get (mm-handle-type handle)
						'charset)
			 'gnus-decoded)
		     (with-current-buffer (mm-handle-buffer handle)
		       (buffer-string)))
		    ((mm-multibyte-p)
		     (mm-string-to-multibyte (mm-get-part handle no-cache)))
		    (t
		     (mm-get-part handle no-cache)))))
    (save-restriction
      (widen)
      (goto-char
       (prog1
	   (point)
	 (if (and (eq (get-char-property (max (point-min) (1- (point))) 'face)
		      'mm-uu-extract)
		  (eq (get-char-property 0 'face text) 'mm-uu-extract))
	     ;; Separate the extracted parts that have the same faces.
	     (insert "\n" text)
	   (insert text)))))))

(defun mm-file-name-delete-whitespace (file-name)
  "Remove all whitespace characters from FILE-NAME."
  (while (string-match "\\s-+" file-name)
    (setq file-name (replace-match "" t t file-name)))
  file-name)

(defun mm-file-name-trim-whitespace (file-name)
  "Remove leading and trailing whitespace characters from FILE-NAME."
  (when (string-match "\\`\\s-+" file-name)
    (setq file-name (substring file-name (match-end 0))))
  (when (string-match "\\s-+\\'" file-name)
    (setq file-name (substring file-name 0 (match-beginning 0))))
  file-name)

(defun mm-file-name-collapse-whitespace (file-name)
  "Collapse multiple whitespace characters in FILE-NAME."
  (while (string-match "\\s-\\s-+" file-name)
    (setq file-name (replace-match " " t t file-name)))
  file-name)

(defun mm-file-name-replace-whitespace (file-name)
  "Replace whitespace characters in FILE-NAME with underscores.
Set the option `mm-file-name-replace-whitespace' to any other
string if you do not like underscores."
  (let ((s (or mm-file-name-replace-whitespace "_")))
    (while (string-match "\\s-" file-name)
      (setq file-name (replace-match s t t file-name))))
  file-name)

(defun mm-file-name-delete-control (filename)
  "Delete control characters from FILENAME."
  (gnus-replace-in-string filename "[\x00-\x1f\x7f]" ""))

(defun mm-file-name-delete-gotchas (filename)
  "Delete shell gotchas from FILENAME."
  (setq filename (gnus-replace-in-string filename "[<>|]" ""))
  (gnus-replace-in-string filename "^[.-]+" ""))

(defun mm-save-part (handle &optional prompt)
  "Write HANDLE to a file.
PROMPT overrides the default one used to ask user for a file name."
  (let ((filename (or (mail-content-type-get
		       (mm-handle-disposition handle) 'filename)
		      (mail-content-type-get
		       (mm-handle-type handle) 'name)))
	file)
    (when filename
      (setq filename (gnus-map-function mm-file-name-rewrite-functions
					(file-name-nondirectory filename))))
    (setq file
          (read-file-name
	   (or prompt
	       (format "Save MIME part to (default %s): "
		       (or filename "")))
	   (or mm-default-directory default-directory)
	   (expand-file-name (or filename "")
			     (or mm-default-directory default-directory))))
    (if (file-directory-p file)
	(setq file (expand-file-name filename file))
      (setq file (expand-file-name
		  file (or mm-default-directory default-directory))))
    (setq mm-default-directory (file-name-directory file))
    (and (or (not (file-exists-p file))
	     (yes-or-no-p (format "File %s already exists; overwrite? "
				  file)))
	 (progn
	   (mm-save-part-to-file handle file)
	   file))))

(defun mm-add-meta-html-tag (handle &optional charset force-charset)
  "Add meta html tag to specify CHARSET of HANDLE in the current buffer.
CHARSET defaults to the one HANDLE specifies.  Existing meta tag that
specifies charset will not be modified unless FORCE-CHARSET is non-nil.
Return t if meta tag is added or replaced."
  (when (equal (mm-handle-media-type handle) "text/html")
    (when (or charset
	      (setq charset (mail-content-type-get (mm-handle-type handle)
						   'charset)))
      (setq charset (format "\
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=%s\">" charset))
      (let ((case-fold-search t))
	(goto-char (point-min))
	(if (re-search-forward "\
<meta\\s-+http-equiv=[\"']?content-type[\"']?\\s-+content=[\"']\
text/\\(\\sw+\\)\\(?:\;\\s-*charset=\\(.+\\)\\)?[\"'][^>]*>" nil t)
	    (if (and (not force-charset)
		     (match-beginning 2)
		     (string-match "\\`html\\'" (match-string 1)))
		;; Don't modify existing meta tag.
		nil
	      ;; Replace it with the one specifying charset.
	      (replace-match charset)
	      t)
	  (if (re-search-forward "<head>\\s-*" nil t)
	      (insert charset "\n")
	    (re-search-forward "<html\\(?:\\s-+[^>]+\\|\\s-*\\)>\\s-*" nil t)
	    (insert "<head>\n" charset "\n</head>\n"))
	  t)))))

(defun mm-save-part-to-file (handle file)
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (mm-add-meta-html-tag handle)
    (let ((current-file-modes (default-file-modes)))
      (set-default-file-modes mm-attachment-file-modes)
      (unwind-protect
	  ;; Don't re-compress .gz & al.  Arguably we should make
	  ;; `file-name-handler-alist' nil, but that would chop
	  ;; ange-ftp, which is reasonable to use here.
	  (mm-write-region (point-min) (point-max) file nil nil nil 'binary t)
	(set-default-file-modes current-file-modes)))))

(defun mm-pipe-part (handle &optional cmd)
  "Pipe HANDLE to a process.
Use CMD as the process."
  (let ((name (mail-content-type-get (mm-handle-type handle) 'name))
	(command (or cmd
		     (gnus-read-shell-command
		      "Shell command on MIME part: " mm-last-shell-command))))
    (mm-with-unibyte-buffer
      (mm-insert-part handle)
      (mm-add-meta-html-tag handle)
      (let ((coding-system-for-write 'binary))
	(shell-command-on-region (point-min) (point-max) command nil)))))

(autoload 'gnus-completing-read "gnus-util")

(defun mm-interactively-view-part (handle)
  "Display HANDLE using METHOD."
  (let* ((type (mm-handle-media-type handle))
	 (methods
	  (mapcar (lambda (i) (cdr (assoc 'viewer i)))
		  (mailcap-mime-info type 'all)))
	 (method (let ((minibuffer-local-completion-map
			mm-viewer-completion-map))
		   (completing-read "Viewer: " methods))))
    (when (string= method "")
      (error "No method given"))
    (if (string-match "^[^% \t]+$" method)
	(setq method (concat method " %s")))
    (mm-display-external handle method)))

(defun mm-preferred-alternative (handles &optional preferred)
  "Say which of HANDLES are preferred."
  (let ((prec (if preferred (list preferred)
		(mm-preferred-alternative-precedence handles)))
	p h result type handle)
    (while (setq p (pop prec))
      (setq h handles)
      (while h
	(setq handle (car h))
	(setq type (mm-handle-media-type handle))
	(when (and (equal p type)
		   (mm-automatic-display-p handle)
		   (or (stringp (car handle))
		       (not (mm-handle-disposition handle))
		       (equal (car (mm-handle-disposition handle))
			      "inline")))
	  (setq result handle
		h nil
		prec nil))
	(pop h)))
    result))

(defun mm-preferred-alternative-precedence (handles)
  "Return the precedence based on HANDLES and `mm-discouraged-alternatives'."
  (setq handles (reverse handles))
  (dolist (disc (reverse mm-discouraged-alternatives))
    (dolist (handle (copy-sequence handles))
      (when (string-match disc (mm-handle-media-type handle))
	(setq handles (nconc (delete handle handles) (list handle))))))
  ;; Remove empty parts.
  (dolist (handle (copy-sequence handles))
    (when (and (bufferp (mm-handle-buffer handle))
	       (not (with-current-buffer (mm-handle-buffer handle)
		      (goto-char (point-min))
		      (re-search-forward "[^ \t\n]" nil t))))
      (setq handles (nconc (delete handle handles) (list handle)))))
  (mapcar #'mm-handle-media-type handles))

(defun mm-get-content-id (id)
  "Return the handle(s) referred to by ID."
  (cdr (assoc id mm-content-id-alist)))

(defconst mm-image-type-regexps
  '(("/\\*.*XPM.\\*/" . xpm)
    ("P[1-6]" . pbm)
    ("GIF8" . gif)
    ("\377\330" . jpeg)
    ("\211PNG\r\n" . png)
    ("#define" . xbm)
    ("\\(MM\0\\*\\)\\|\\(II\\*\0\\)" . tiff)
    ("%!PS" . postscript))
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to auto-detect image types.
When the first bytes of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE.")

;; Steal from image.el. image-type-from-data suffers multi-line matching bug.
(defun mm-image-type-from-buffer ()
  "Determine the image type from data in the current buffer.
Value is a symbol specifying the image type or nil if type cannot
be determined."
  (let ((types mm-image-type-regexps)
	type)
    (goto-char (point-min))
    (while (and types (null type))
      (let ((regexp (car (car types)))
	    (image-type (cdr (car types))))
	(when (looking-at regexp)
	  (setq type image-type))
	(setq types (cdr types))))
    type))

(defun mm-get-image (handle)
  "Return an image instance based on HANDLE."
  (let ((type (mm-handle-media-subtype handle))
	spec)
    ;; Allow some common translations.
    (setq type
	  (cond
	   ((equal type "x-pixmap")
	    "xpm")
	   ((equal type "x-xbitmap")
	    "xbm")
	   ((equal type "x-portable-bitmap")
	    "pbm")
	   (t type)))
    (or (mm-handle-cache handle)
	(mm-with-unibyte-buffer
	  (mm-insert-part handle)
	  (prog1
	      (setq spec
		    (ignore-errors
		      ;; Avoid testing `make-glyph' since W3 may define
		      ;; a bogus version of it.
		      (if (fboundp 'create-image)
			  (create-image (buffer-string)
					(or (mm-image-type-from-buffer)
					    (intern type))
					'data-p)
			(mm-create-image-xemacs type))))
	    (mm-handle-set-cache handle spec))))))

(defun mm-create-image-xemacs (type)
  (when (featurep 'xemacs)
    (cond
     ((equal type "xbm")
      ;; xbm images require special handling, since
      ;; the only way to create glyphs from these
      ;; (without a ton of work) is to write them
      ;; out to a file, and then create a file
      ;; specifier.
      (let ((file (mm-make-temp-file
		   (expand-file-name "emm" mm-tmp-directory)
		   nil ".xbm")))
	(unwind-protect
	    (progn
	      (write-region (point-min) (point-max) file)
	      (make-glyph (list (cons 'x file))))
	  (ignore-errors
	    (delete-file file)))))
     (t
      (make-glyph
       (vector
	(or (mm-image-type-from-buffer)
	    (intern type))
	:data (buffer-string)))))))

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun mm-image-fit-p (handle)
  "Say whether the image in HANDLE will fit the current window."
  (let ((image (mm-get-image handle)))
    (or (not image)
	(if (featurep 'xemacs)
	    ;; XEmacs's glyphs can actually tell us about their width, so
	    ;; let's be nice and smart about them.
	    (or mm-inline-large-images
		(and (<= (glyph-width image) (window-pixel-width))
		     (<= (glyph-height image) (window-pixel-height))))
	  (let* ((size (image-size image))
		 (w (car size))
		 (h (cdr size)))
	    (or mm-inline-large-images
		(and (<= h (1- (window-height))) ; Don't include mode line.
		     (<= w (window-width)))))))))

(defun mm-valid-image-format-p (format)
  "Say whether FORMAT can be displayed natively by Emacs."
  (cond
   ;; Handle XEmacs
   ((fboundp 'valid-image-instantiator-format-p)
    (valid-image-instantiator-format-p format))
   ;; Handle Emacs
   ((fboundp 'image-type-available-p)
    (and (display-graphic-p)
	 (image-type-available-p format)))
   ;; Nobody else can do images yet.
   (t
    nil)))

(defun mm-valid-and-fit-image-p (format handle)
  "Say whether FORMAT can be displayed natively and HANDLE fits the window."
  (and (mm-valid-image-format-p format)
       (mm-image-fit-p handle)))

(defun mm-find-part-by-type (handles type &optional notp recursive)
  "Search in HANDLES for part with TYPE.
If NOTP, returns first non-matching part.
If RECURSIVE, search recursively."
  (let (handle)
    (while handles
      (if (and recursive (stringp (caar handles)))
	  (if (setq handle (mm-find-part-by-type (cdar handles) type
						 notp recursive))
	      (setq handles nil))
	(if (if notp
		(not (equal (mm-handle-media-type (car handles)) type))
	      (equal (mm-handle-media-type (car handles)) type))
	    (setq handle (car handles)
		  handles nil)))
      (setq handles (cdr handles)))
    handle))

(defun mm-find-raw-part-by-type (ctl type &optional notp)
  (goto-char (point-min))
  (let* ((boundary (concat "--" (mm-handle-multipart-ctl-parameter ctl
								   'boundary)))
	 (close-delimiter (concat "^" (regexp-quote boundary) "--[ \t]*$"))
	 start
	 (end (save-excursion
		(goto-char (point-max))
		(if (re-search-backward close-delimiter nil t)
		    (match-beginning 0)
		  (point-max))))
	 result)
    (setq boundary (concat "^" (regexp-quote boundary) "[ \t]*$"))
    (while (and (not result)
		(re-search-forward boundary end t))
      (goto-char (match-beginning 0))
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (1- (point)))
	    (when (let* ((ct (mail-fetch-field "content-type"))
			 (ctl (and ct (mail-header-parse-content-type ct))))
		    (if notp
			(not (equal (car ctl) type))
		      (equal (car ctl) type)))
	      (setq result (buffer-string))))))
      (forward-line 1)
      (setq start (point)))
    (when (and (not result) start)
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (when (let* ((ct (mail-fetch-field "content-type"))
		       (ctl (and ct (mail-header-parse-content-type ct))))
		  (if notp
		      (not (equal (car ctl) type))
		    (equal (car ctl) type)))
	    (setq result (buffer-string))))))
    result))

(defvar mm-security-handle nil)

(defsubst mm-set-handle-multipart-parameter (handle parameter value)
  ;; HANDLE could be a CTL.
  (when handle
    (put-text-property 0 (length (car handle)) parameter value
		       (car handle))))

(autoload 'mm-view-pkcs7 "mm-view")

(defun mm-possibly-verify-or-decrypt (parts ctl &optional from)
  (let ((type (car ctl))
	(subtype (cadr (split-string (car ctl) "/")))
	(mm-security-handle ctl) ;; (car CTL) is the type.
	protocol func functest)
    (cond
     ((or (equal type "application/x-pkcs7-mime")
	  (equal type "application/pkcs7-mime"))
      (with-temp-buffer
	(when (and (cond
		    ((eq mm-decrypt-option 'never) nil)
		    ((eq mm-decrypt-option 'always) t)
		    ((eq mm-decrypt-option 'known) t)
		    (t (y-or-n-p
			(format "Decrypt (S/MIME) part? "))))
		   (mm-view-pkcs7 parts from))
	  (setq parts (mm-dissect-buffer t)))))
     ((equal subtype "signed")
      (unless (and (setq protocol
			 (mm-handle-multipart-ctl-parameter ctl 'protocol))
		   (not (equal protocol "multipart/mixed")))
	;; The message is broken or draft-ietf-openpgp-multsig-01.
	(let ((protocols mm-verify-function-alist))
	  (while protocols
	    (if (and (or (not (setq functest (nth 3 (car protocols))))
			 (funcall functest parts ctl))
		     (mm-find-part-by-type parts (caar protocols) nil t))
		(setq protocol (caar protocols)
		      protocols nil)
	      (setq protocols (cdr protocols))))))
      (setq func (nth 1 (assoc protocol mm-verify-function-alist)))
      (when (cond
	     ((eq mm-verify-option 'never) nil)
	     ((eq mm-verify-option 'always) t)
	     ((eq mm-verify-option 'known)
	      (and func
		   (or (not (setq functest
				  (nth 3 (assoc protocol
						mm-verify-function-alist))))
		       (funcall functest parts ctl))))
	     (t
	      (y-or-n-p
	       (format "Verify signed (%s) part? "
		       (or (nth 2 (assoc protocol mm-verify-function-alist))
			   (format "protocol=%s" protocol))))))
	(save-excursion
	  (if func
	      (setq parts (funcall func parts ctl))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (format "Unknown sign protocol (%s)" protocol))))))
     ((equal subtype "encrypted")
      (unless (setq protocol
		    (mm-handle-multipart-ctl-parameter ctl 'protocol))
	;; The message is broken.
	(let ((parts parts))
	  (while parts
	    (if (assoc (mm-handle-media-type (car parts))
		       mm-decrypt-function-alist)
		(setq protocol (mm-handle-media-type (car parts))
		      parts nil)
	      (setq parts (cdr parts))))))
      (setq func (nth 1 (assoc protocol mm-decrypt-function-alist)))
      (when (cond
	     ((eq mm-decrypt-option 'never) nil)
	     ((eq mm-decrypt-option 'always) t)
	     ((eq mm-decrypt-option 'known)
	      (and func
		   (or (not (setq functest
				  (nth 3 (assoc protocol
						mm-decrypt-function-alist))))
		       (funcall functest parts ctl))))
	     (t
	      (y-or-n-p
	       (format "Decrypt (%s) part? "
		       (or (nth 2 (assoc protocol mm-decrypt-function-alist))
			   (format "protocol=%s" protocol))))))
	(save-excursion
	  (if func
	      (setq parts (funcall func parts ctl))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (format "Unknown encrypt protocol (%s)" protocol))))))
     (t nil))
    parts))

(defun mm-multiple-handles (handles)
  (and (listp handles)
       (> (length handles) 1)
       (or (listp (car handles))
	   (stringp (car handles)))))

(defun mm-complicated-handles (handles)
  (and (listp (car handles))
       (> (length handles) 1)))

(defun mm-merge-handles (handles1 handles2)
  (append
   (if (listp (car handles1))
       handles1
     (list handles1))
   (if (listp (car handles2))
       handles2
     (list handles2))))

(defun mm-readable-p (handle)
  "Say whether the content of HANDLE is readable."
  (and (< (with-current-buffer (mm-handle-buffer handle)
	    (buffer-size)) 10000)
       (mm-with-unibyte-buffer
	 (mm-insert-part handle)
	 (and (eq (mm-body-7-or-8) '7bit)
	      (not (mm-long-lines-p 76))))))

(declare-function libxml-parse-html-region "xml.c"
		  (start end &optional base-url))
(declare-function shr-insert-document "shr" (dom))
(defvar shr-blocked-images)
(defvar gnus-inhibit-images)
(autoload 'gnus-blocked-images "gnus-art")

(defun mm-shr (handle)
  ;; Require since we bind its variables.
  (require 'shr)
  (let ((article-buffer (current-buffer))
	(shr-content-function (lambda (id)
				(let ((handle (mm-get-content-id id)))
				  (when handle
				    (mm-with-part handle
				      (buffer-string))))))
	shr-inhibit-images shr-blocked-images charset char)
    (if (and (boundp 'gnus-summary-buffer)
	     (bufferp gnus-summary-buffer)
	     (buffer-name gnus-summary-buffer))
	(with-current-buffer gnus-summary-buffer
	  (setq shr-inhibit-images gnus-inhibit-images
		shr-blocked-images (gnus-blocked-images)))
      (setq shr-inhibit-images gnus-inhibit-images
	    shr-blocked-images (gnus-blocked-images)))
    (unless handle
      (setq handle (mm-dissect-buffer t)))
    (setq charset (mail-content-type-get (mm-handle-type handle) 'charset))
    (save-restriction
      (narrow-to-region (point) (point))
      (shr-insert-document
       (mm-with-part handle
	 (insert (prog1
		     (if (and charset
			      (setq charset
				    (mm-charset-to-coding-system charset))
			      (not (eq charset 'ascii)))
			 (mm-decode-coding-string (buffer-string) charset)
		       (mm-string-as-multibyte (buffer-string)))
		   (erase-buffer)
		   (mm-enable-multibyte)))
	 (goto-char (point-min))
	 (setq case-fold-search t)
	 (while (re-search-forward
		 "&#\\(?:x\\([89][0-9a-f]\\)\\|\\(1[2-5][0-9]\\)\\);" nil t)
	   (when (setq char
		       (cdr (assq (if (match-beginning 1)
				      (string-to-number (match-string 1) 16)
				    (string-to-number (match-string 2)))
				  mm-extra-numeric-entities)))
	     (replace-match (char-to-string char))))
	 ;; Remove "soft hyphens".
	 (goto-char (point-min))
	 (while (search-forward "­" nil t)
	   (replace-match "" t t))
	 (libxml-parse-html-region (point-min) (point-max))))
      (mm-handle-set-undisplayer
       handle
       `(lambda ()
	  (let ((inhibit-read-only t))
	    (delete-region ,(point-min-marker)
			   ,(point-max-marker))))))))

(defun mm-handle-filename (handle)
  "Return filename of HANDLE if any."
  (or (mail-content-type-get (mm-handle-type handle)
                             'name)
      (mail-content-type-get (mm-handle-disposition handle)
                             'filename)))

(provide 'mm-decode)

;;; mm-decode.el ends here
