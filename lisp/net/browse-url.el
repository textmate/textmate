;;; browse-url.el --- pass a URL to a WWW browser

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Denis Howe <dbh@doc.ic.ac.uk>
;; Maintainer: FSF
;; Created: 03 Apr 1995
;; Keywords: hypertext, hypermedia, mouse

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

;; This package provides functions which read a URL (Uniform Resource
;; Locator) from the minibuffer, defaulting to the URL around point,
;; and ask a World-Wide Web browser to load it.  It can also load the
;; URL associated with the current buffer.  Different browsers use
;; different methods of remote control so there is one function for
;; each supported browser.  If the chosen browser is not running, it
;; is started.  Currently there is support for the following browsers,
;; some of them probably now obsolete:

;; Function                           Browser     Earliest version
;; browse-url-mozilla                 Mozilla     Don't know
;; browse-url-firefox                 Firefox     Don't know (tried with 1.0.1)
;; browse-url-chromium                Chromium    3.0
;; browse-url-galeon                  Galeon      Don't know
;; browse-url-epiphany                Epiphany    Don't know
;; browse-url-netscape                Netscape    1.1b1
;; browse-url-mosaic                  XMosaic/mMosaic <= 2.4
;; browse-url-cci                     XMosaic     2.5
;; browse-url-w3                      w3          0
;; browse-url-w3-gnudoit              w3 remotely
;; browse-url-text-*	              Any text browser     0
;; browse-url-generic                 arbitrary
;; browse-url-default-windows-browser MS-Windows browser
;; browse-url-default-macosx-browser  Mac OS X browser
;; browse-url-xdg-open                Free Desktop xdg-open on Gnome, KDE, Xfce4, LXDE
;; browse-url-gnome-moz               GNOME interface to Mozilla
;; browse-url-kde                     KDE konqueror (kfm)
;; browse-url-elinks                  Elinks      Don't know (tried with 0.12.GIT)

;; [A version of the Netscape browser is now free software
;; <URL:http://www.mozilla.org/>, albeit not GPLed, so it is
;; reasonable to have that as the default.]

;; Note that versions of Netscape before 1.1b1 did not have remote
;; control.  <URL:http://www.netscape.com/newsref/std/x-remote.html>.

;; Browsers can cache Web pages so it may be necessary to tell them to
;; reload the current page if it has changed (e.g. if you have edited
;; it).  There is currently no perfect automatic solution to this.

;; Netscape allows you to specify the id of the window you want to
;; control but which window DO you want to control and how do you
;; discover its id?

;; William M.  Perry's excellent "w3" WWW browser for
;; Emacs <URL:ftp://cs.indiana.edu/pub/elisp/w3/>
;; has a function w3-follow-url-at-point, but that
;; doesn't let you edit the URL like browse-url.
;; The `gnuserv' package that can be used to control it in another
;; Emacs process is available from
;; <URL:ftp://ftp.splode.com/pub/users/friedman/packages/>.

;; Lynx is now distributed by the FSF.  See also
;; <URL:http://lynx.browser.org/>.

;; Free graphical browsers that could be used by `browse-url-generic'
;; include Chimera <URL:ftp://ftp.cs.unlv.edu/pub/chimera> and
;; <URL:http://www.unlv.edu/chimera/>, Arena
;; <URL:ftp://ftp.yggdrasil.com/pub/dist/web/arena> and Amaya
;; <URL:ftp://ftp.w3.org/pub/amaya>.  mMosaic
;; <URL:ftp://ftp.enst.fr/pub/mbone/mMosaic/>,
;; <URL:http://www.enst.fr/~dauphin/mMosaic/> (with development
;; support for Java applets and multicast) can be used like Mosaic by
;; setting `browse-url-mosaic-program' appropriately.

;; I [Denis Howe, not Dave Love] recommend Nelson Minar
;; <nelson@santafe.edu>'s excellent html-helper-mode.el for editing
;; HTML and thank Nelson for his many useful comments on this code.
;; <URL:http://www.santafe.edu/%7Enelson/hhm-beta/>

;; See also hm--html-menus <URL:http://www.tnt.uni-hannover.de/%7Emuenkel/
;; software/own/hm--html-menus/>.  For composing correct HTML see also
;; PSGML the general SGML structure editor package
;; <URL:ftp://ftp.lysator.liu.se/pub/sgml>; hm--html-menus can be used
;; with this.

;; This package generalizes function html-previewer-process in Marc
;; Andreessen's html-mode (LCD modes/html-mode.el.Z).  See also the
;; ffap.el package.  The huge hyperbole package also contains similar
;; functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help!

;; Can you write and test some code for the Macintrash and Windoze
;; Netscape remote control APIs?  (See the URL above).

;; Do any other browsers have remote control?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage

;; To display the URL at or before point:
;; M-x browse-url-at-point RET
;; or, similarly but with the opportunity to edit the URL extracted from
;; the buffer, use:
;; M-x browse-url

;; To display a URL by shift-clicking on it, put this in your ~/.emacs
;; file:
;;      (global-set-key [S-mouse-2] 'browse-url-at-mouse)
;; (Note that using Shift-mouse-1 is not desirable because
;; that event has a standard meaning in Emacs.)

;; To display the current buffer in a web browser:
;; M-x browse-url-of-buffer RET

;; To display the current region in a web browser:
;; M-x browse-url-of-region RET

;; In Dired, to display the file named on the current line:
;; M-x browse-url-of-dired-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization (~/.emacs)

;; To see what variables are available for customization, type
;; `M-x set-variable browse-url TAB'.  Better, use
;; `M-x customize-group browse-url'.

;; Bind the browse-url commands to keys with the `C-c C-z' prefix
;; (as used by html-helper-mode):
;;	(global-set-key "\C-c\C-z." 'browse-url-at-point)
;;	(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
;;	(global-set-key "\C-c\C-zr" 'browse-url-of-region)
;;	(global-set-key "\C-c\C-zu" 'browse-url)
;;	(global-set-key "\C-c\C-zv" 'browse-url-of-file)
;;	(add-hook 'dired-mode-hook
;;		  (lambda ()
;;	             (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))

;; Browse URLs in mail messages under RMAIL by clicking mouse-2:
;;	(add-hook 'rmail-mode-hook (lambda () ; rmail-mode startup
;;	  (define-key rmail-mode-map [mouse-2] 'browse-url-at-mouse)))
;; Alternatively, add `goto-address' to `rmail-show-message-hook'.

;; Gnus provides a standard feature to activate URLs in article
;; buffers for invocation of browse-url.

;; Use the Emacs w3 browser when not running under X11:
;;	(or (eq window-system 'x)
;;	    (setq browse-url-browser-function 'browse-url-w3))

;; To always save modified buffers before displaying the file in a browser:
;;	(setq browse-url-save-file t)

;; To get round the Netscape caching problem, you could EITHER have
;; write-file in html-helper-mode make Netscape reload the document:
;;
;;	(autoload 'browse-url-netscape-reload "browse-url"
;;	  "Ask a WWW browser to redisplay the current file." t)
;;	(add-hook 'html-helper-mode-hook
;;		  (lambda ()
;;		     (add-hook 'local-write-file-hooks
;;			       (lambda ()
;;				  (let ((local-write-file-hooks))
;;				    (save-buffer))
;;				  (browse-url-netscape-reload)
;;				  t)			; => file written by hook
;;			       t)))			; append to l-w-f-hooks
;;
;; OR have browse-url-of-file ask Netscape to load and then reload the
;; file:
;;
;;	(add-hook 'browse-url-of-file-hook 'browse-url-netscape-reload)

;; You may also want to customize browse-url-netscape-arguments, e.g.
;;	(setq browse-url-netscape-arguments '("-install"))
;;
;; or similarly for the other browsers.

;; To invoke different browsers for different URLs:
;;      (setq browse-url-browser-function '(("^mailto:" . browse-url-mail)
;;      				    ("." . browse-url-netscape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(eval-when-compile (require 'cl))

(defgroup browse-url nil
  "Use a web browser to look at a URL."
  :prefix "browse-url-"
  :link '(emacs-commentary-link "browse-url")
  :group 'external
  :group 'comm)

;;;###autoload
(defcustom browse-url-browser-function
  'browse-url-default-browser
  "Function to display the current buffer in a WWW browser.
This is used by the `browse-url-at-point', `browse-url-at-mouse', and
`browse-url-of-file' commands.

If the value is not a function it should be a list of pairs
\(REGEXP . FUNCTION).  In this case the function called will be the one
associated with the first REGEXP which matches the current URL.  The
function is passed the URL and any other args of `browse-url'.  The last
regexp should probably be \".\" to specify a default browser."
  :type '(choice
	  (function-item :tag "Emacs W3" :value  browse-url-w3)
	  (function-item :tag "W3 in another Emacs via `gnudoit'"
			 :value  browse-url-w3-gnudoit)
	  (function-item :tag "Mozilla" :value  browse-url-mozilla)
	  (function-item :tag "Firefox" :value browse-url-firefox)
	  (function-item :tag "Chromium" :value browse-url-chromium)
	  (function-item :tag "Galeon" :value  browse-url-galeon)
	  (function-item :tag "Epiphany" :value  browse-url-epiphany)
	  (function-item :tag "Netscape" :value  browse-url-netscape)
	  (function-item :tag "Mosaic" :value  browse-url-mosaic)
	  (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
	  (function-item :tag "Text browser in an xterm window"
			 :value browse-url-text-xterm)
	  (function-item :tag "Text browser in an Emacs window"
			 :value browse-url-text-emacs)
	  (function-item :tag "KDE" :value browse-url-kde)
	  (function-item :tag "Elinks" :value browse-url-elinks)
	  (function-item :tag "Specified by `Browse Url Generic Program'"
			 :value browse-url-generic)
	  (function-item :tag "Default Windows browser"
			 :value browse-url-default-windows-browser)
	  (function-item :tag "Default Mac OS X browser"
			 :value browse-url-default-macosx-browser)
	  (function-item :tag "GNOME invoking Mozilla"
			 :value browse-url-gnome-moz)
	  (function-item :tag "Default browser"
			 :value browse-url-default-browser)
	  (function :tag "Your own function")
	  (alist :tag "Regexp/function association list"
		 :key-type regexp :value-type function))
  :version "24.1"
  :group 'browse-url)

(defcustom browse-url-mailto-function 'browse-url-mail
  "Function to display mailto: links.
This variable uses the same syntax as the
`browse-url-browser-function' variable.  If the
`browse-url-mailto-function' variable is nil, that variable will
be used instead."
  :type '(choice
	  (function-item :tag "Emacs Mail" :value browse-url-mail)
	  (function-item :tag "None" nil))
  :version "24.1"
  :group 'browse-url)

(defcustom browse-url-netscape-program "netscape"
  ;; Info about netscape-remote from Karl Berry.
  "The name by which to invoke Netscape.

The free program `netscape-remote' from
<URL:http://home.netscape.com/newsref/std/remote.c> is said to start
up very much quicker than `netscape'.  Reported to compile on a GNU
system, given vroot.h from the same directory, with cc flags
 -DSTANDALONE -L/usr/X11R6/lib -lXmu -lX11."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-netscape-arguments nil
  "A list of strings to pass to Netscape as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-netscape-startup-arguments browse-url-netscape-arguments
  "A list of strings to pass to Netscape when it starts up.
Defaults to the value of `browse-url-netscape-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-browser-display nil
  "The X display for running the browser, if not same as Emacs's."
  :type '(choice string (const :tag "Default" nil))
  :group 'browse-url)

(defcustom browse-url-mozilla-program "mozilla"
  "The name by which to invoke Mozilla."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-mozilla-arguments nil
  "A list of strings to pass to Mozilla as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-mozilla-startup-arguments browse-url-mozilla-arguments
  "A list of strings to pass to Mozilla when it starts up.
Defaults to the value of `browse-url-mozilla-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-firefox-program
  (let ((candidates '("firefox" "iceweasel" "icecat")))
    (while (and candidates (not (executable-find (car candidates))))
      (setq candidates (cdr candidates)))
    (or (car candidates) "firefox"))
  "The name by which to invoke Firefox."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-firefox-arguments nil
  "A list of strings to pass to Firefox as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-firefox-startup-arguments browse-url-firefox-arguments
  "A list of strings to pass to Firefox when it starts up.
Defaults to the value of `browse-url-firefox-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-chromium-program
  (let ((candidates '("chromium" "chromium-browser")))
    (while (and candidates (not (executable-find (car candidates))))
      (setq candidates (cdr candidates)))
    (or (car candidates) "chromium"))
  "The name by which to invoke Chromium."
  :type 'string
  :version "24.1"
  :group 'browse-url)

(defcustom browse-url-chromium-arguments nil
  "A list of strings to pass to Chromium as arguments."
  :type '(repeat (string :tag "Argument"))
  :version "24.1"
  :group 'browse-url)

(defcustom browse-url-galeon-program "galeon"
  "The name by which to invoke Galeon."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-galeon-arguments nil
  "A list of strings to pass to Galeon as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-galeon-startup-arguments browse-url-galeon-arguments
  "A list of strings to pass to Galeon when it starts up.
Defaults to the value of `browse-url-galeon-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-epiphany-program "epiphany"
  "The name by which to invoke Epiphany."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-epiphany-arguments nil
  "A list of strings to pass to Epiphany as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-epiphany-startup-arguments browse-url-epiphany-arguments
  "A list of strings to pass to Epiphany when it starts up.
Defaults to the value of `browse-url-epiphany-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;; GNOME means of invoking either Mozilla or Netscape.
(defvar browse-url-gnome-moz-program "gnome-moz-remote")

(defcustom browse-url-gnome-moz-arguments '()
  "A list of strings passed to the GNOME mozilla viewer as arguments."
  :version "21.1"
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-mozilla-new-window-is-tab nil
  "Whether to open up new windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-mozilla' is asked to open it in a new window."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-firefox-new-window-is-tab nil
  "Whether to open up new windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-firefox' is asked to open it in a new window.

This option is currently ignored on MS-Windows, since the necessary
functionality is not available there."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-galeon-new-window-is-tab nil
  "Whether to open up new windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-galeon' is asked to open it in a new window."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-epiphany-new-window-is-tab nil
  "Whether to open up new windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-epiphany' is asked to open it in a new window."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-netscape-new-window-is-tab nil
  "Whether to open up new windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new
window if `browse-url-netscape' is asked to open it in a new
window."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-new-window-flag nil
  "Non-nil means always open a new browser window with appropriate browsers.
Passing an interactive argument to \\[browse-url], or specific browser
commands reverses the effect of this variable.  Requires Netscape version
1.1N or later or XMosaic version 2.5 or later if using those browsers."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-mosaic-program "xmosaic"
  "The name by which to invoke Mosaic (or mMosaic)."
  :type 'string
  :version "20.3"
  :group 'browse-url)

(defcustom browse-url-mosaic-arguments nil
  "A list of strings to pass to Mosaic as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-mosaic-pidfile "~/.mosaicpid"
  "The name of the pidfile created by Mosaic."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-filename-alist
  `(("^/\\(ftp@\\|anonymous@\\)?\\([^:]+\\):/*" . "ftp://\\2/")
    ;; The above loses the username to avoid the browser prompting for
    ;; it in anonymous cases.  If it's not anonymous the next regexp
    ;; applies.
    ("^/\\([^:@]+@\\)?\\([^:]+\\):/*" . "ftp://\\1\\2/")
    ,@(if (memq system-type '(windows-nt ms-dos cygwin))
          '(("^\\([a-zA-Z]:\\)[\\/]" . "file:///\\1/")
            ("^[\\/][\\/]+" . "file://")))
    ("^/+" . "file:///"))
  "An alist of (REGEXP . STRING) pairs used by `browse-url-of-file'.
Any substring of a filename matching one of the REGEXPs is replaced by
the corresponding STRING using `replace-match', not treating STRING
literally.  All pairs are applied in the order given.  The default
value converts ange-ftp/EFS-style file names into ftp URLs and prepends
`file:' to any file name beginning with `/'.

For example, adding to the default a specific translation of an ange-ftp
address to an HTTP URL:

    (setq browse-url-filename-alist
	  '((\"/webmaster@webserver:/home/www/html/\" .
	     \"http://www.acme.co.uk/\")
            (\"^/\\(ftp@\\|anonymous@\\)?\\([^:]+\\):/*\" . \"ftp://\\2/\")
            (\"^/\\([^:@]+@\\)?\\([^:]+\\):/*\" . \"ftp://\\1\\2/\")
	    (\"^/+\" . \"file:/\")))"
  :type '(repeat (cons :format "%v"
                       (regexp :tag "Regexp")
                       (string :tag "Replacement")))
  :version "23.1"
  :group 'browse-url)

(defcustom browse-url-save-file nil
  "If non-nil, save the buffer before displaying its file.
Used by the `browse-url-of-file' command."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-of-file-hook nil
  "Run after `browse-url-of-file' has asked a browser to load a file.

Set this to `browse-url-netscape-reload' to force Netscape to load the
file rather than displaying a cached copy."
  :type 'hook
  :options '(browse-url-netscape-reload)
  :group 'browse-url)

(defcustom browse-url-CCI-port 3003
  "Port to access XMosaic via CCI.
This can be any number between 1024 and 65535 but must correspond to
the value set in the browser."
  :type 'integer
  :group 'browse-url)

(defcustom browse-url-CCI-host "localhost"
  "Host to access XMosaic via CCI.
This should be the host name of the machine running XMosaic with CCI
enabled.  The port number should be set in `browse-url-CCI-port'."
  :type 'string
  :group 'browse-url)

(defvar browse-url-temp-file-name nil)
(make-variable-buffer-local 'browse-url-temp-file-name)

(defcustom browse-url-xterm-program "xterm"
  "The name of the terminal emulator used by `browse-url-text-xterm'.
This might, for instance, be a separate color version of xterm."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-xterm-args nil
  "A list of strings defining options for `browse-url-xterm-program'.
These might set its size, for instance."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-gnudoit-program "gnudoit"
  "The name of the `gnudoit' program used by `browse-url-w3-gnudoit'."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-gnudoit-args '("-q")
  "A list of strings defining options for `browse-url-gnudoit-program'.
These might set the port, for instance."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-generic-program nil
  "The name of the browser program used by `browse-url-generic'."
  :type '(choice string (const :tag "None" nil))
  :group 'browse-url)

(defcustom browse-url-generic-args nil
  "A list of strings defining options for `browse-url-generic-program'."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-temp-dir temporary-file-directory
  "The name of a directory for browse-url's temporary files.
Such files are generated by functions like `browse-url-of-region'.
You might want to set this to somewhere with restricted read permissions
for privacy's sake."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-netscape-version 3
  "The version of Netscape you are using.
This affects how URL reloading is done; the mechanism changed
incompatibly at version 4."
  :type 'number
  :group 'browse-url)

(defcustom browse-url-text-browser "lynx"
  "The name of the text browser to invoke."
  :type 'string
  :group 'browse-url
  :version "23.1")

(defcustom browse-url-text-emacs-args (and (not window-system)
					   '("-show_cursor"))
  "A list of strings defining options for a text browser in an Emacs buffer.

The default is none in a window system, otherwise `-show_cursor' to
indicate the position of the current link in the absence of
highlighting, assuming the normal default for showing the cursor."
  :type '(repeat (string :tag "Argument"))
  :version "23.1"
  :group 'browse-url)

(defcustom browse-url-text-input-field 'avoid
  "Action on selecting an existing text browser buffer at an input field.
What to do when sending a new URL to an existing text browser buffer in Emacs
if the browser cursor is on an input field (in which case the `g' command
would be entered as data).  Such fields are recognized by the
underlines ____.  Allowed values: nil: disregard it, `warn': warn the
user and don't emit the URL, `avoid': try to avoid the field by moving
down (this *won't* always work)."
  :type '(choice (const :tag "Move to try to avoid field" :value avoid)
                 (const :tag "Disregard" :value nil)
                 (const :tag "Warn, don't emit URL" :value warn))
  :version "23.1"
  :group 'browse-url)

(defcustom browse-url-text-input-attempts 10
  "How many times to try to move down from a series of text browser input fields."
  :type 'integer
  :version "23.1"
  :group 'browse-url)

(defcustom browse-url-text-input-delay 0.2
  "Seconds to wait for a text browser between moves down from an input field."
  :type 'number
  :version "23.1"
  :group 'browse-url)

(defcustom browse-url-kde-program "kfmclient"
  "The name by which to invoke the KDE web browser."
  :type 'string
  :version "21.1"
  :group 'browse-url)

(defcustom browse-url-kde-args '("openURL")
  "A list of strings defining options for `browse-url-kde-program'."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-elinks-wrapper '("xterm" "-e")
  "Wrapper command prepended to the Elinks command-line."
  :type '(repeat (string :tag "Wrapper"))
  :group 'browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL encoding

(defun browse-url-url-encode-chars (text chars)
  "URL-encode the chars in TEXT that match CHARS.
CHARS is a regexp-like character alternative (e.g., \"[)$]\")."
  (let ((encoded-text (copy-sequence text))
	(s 0))
    (while (setq s (string-match chars encoded-text s))
      (setq encoded-text
	    (replace-match (format "%%%x"
				   (string-to-char (match-string 0 encoded-text)))
			   t t encoded-text)
	    s (1+ s)))
    encoded-text))

(defun browse-url-encode-url (url)
  "Escape annoying characters in URL.
The annoying characters are those that can mislead a web browser
regarding its parameter treatment."
  ;; FIXME: Is there an actual example of a web browser getting
  ;; confused?  (This used to encode commas, but at least Firefox
  ;; handles commas correctly and doesn't accept encoded commas.)
  (browse-url-url-encode-chars url "[)$]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL input

(defun browse-url-url-at-point ()
  (let ((url (thing-at-point 'url)))
    (set-text-properties 0 (length url) nil url)
    url))

;; Having this as a separate function called by the browser-specific
;; functions allows them to be stand-alone commands, making it easier
;; to switch between browsers.

(defun browse-url-interactive-arg (prompt)
  "Read a URL from the minibuffer, prompting with PROMPT.
If `transient-mark-mode' is non-nil and the mark is active,
it defaults to the current region, else to the URL at or before
point.  If invoked with a mouse button, it moves point to the
position clicked before acting.

This function returns a list (URL NEW-WINDOW-FLAG)
for use in `interactive'."
  (let ((event (elt (this-command-keys) 0)))
    (and (listp event) (mouse-set-point event)))
  (list (read-string prompt (or (and transient-mark-mode mark-active
				     ;; rfc2396 Appendix E.
				     (replace-regexp-in-string
				      "[\t\r\f\n ]+" ""
				      (buffer-substring-no-properties
				       (region-beginning) (region-end))))
				(browse-url-url-at-point)))
	(not (eq (null browse-url-new-window-flag)
		 (null current-prefix-arg)))))

;; called-interactive-p needs to be called at a function's top-level, hence
;; this macro.  We use that rather than interactive-p because
;; use in a keyboard macro should not change this behavior.
(defmacro browse-url-maybe-new-window (arg)
  `(if (or noninteractive (not (called-interactively-p 'any)))
       ,arg
     browse-url-new-window-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse current buffer

;;;###autoload
(defun browse-url-of-file (&optional file)
  "Ask a WWW browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL with function
`browse-url-file-url'.  Pass the URL to a browser using the
`browse-url' function then run `browse-url-of-file-hook'."
  (interactive)
  (or file
      (setq file (buffer-file-name))
      (error "Current buffer has no file"))
  (let ((buf (get-file-buffer file)))
    (if buf
	(with-current-buffer buf
	  (cond ((not (buffer-modified-p)))
		(browse-url-save-file (save-buffer))
		(t (message "%s modified since last save" file))))))
  (browse-url (browse-url-file-url file))
  (run-hooks 'browse-url-of-file-hook))

(defun browse-url-file-url (file)
  "Return the URL corresponding to FILE.
Use variable `browse-url-filename-alist' to map filenames to URLs."
  ;; De-munge Cygwin filenames before passing them to Windows browser.
  (if (eq system-type 'cygwin)
      (let ((winfile (with-output-to-string
		       (call-process "cygpath" nil standard-output
				     nil "-m" file))))
	(setq file (substring winfile 0 -1))))
  (let ((coding (and (default-value 'enable-multibyte-characters)
		     (or file-name-coding-system
			 default-file-name-coding-system))))
    (if coding (setq file (encode-coding-string file coding))))
  (setq file (browse-url-url-encode-chars file "[*\"()',=;?% ]"))
  (dolist (map browse-url-filename-alist)
    (when (and map (string-match (car map) file))
      (setq file (replace-match (cdr map) t nil file))))
  file)

;;;###autoload
(defun browse-url-of-buffer (&optional buffer)
  "Ask a WWW browser to display BUFFER.
Display the current buffer if BUFFER is nil.  Display only the
currently visible part of BUFFER (from a temporary file) if buffer is
narrowed."
  (interactive)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let ((file-name
	   ;; Ignore real name if restricted
	   (and (= (- (point-max) (point-min)) (buffer-size))
		(or buffer-file-name
		    (and (boundp 'dired-directory) dired-directory)))))
      (or file-name
	  (progn
	    (or browse-url-temp-file-name
		(setq browse-url-temp-file-name
		      (convert-standard-filename
		       (make-temp-file
			(expand-file-name "burl" browse-url-temp-dir)
			nil ".html"))))
	    (setq file-name browse-url-temp-file-name)
	    (write-region (point-min) (point-max) file-name nil 'no-message)))
      (browse-url-of-file file-name))))

(defun browse-url-delete-temp-file (&optional temp-file-name)
  ;; Delete browse-url-temp-file-name from the file system
  ;; If optional arg TEMP-FILE-NAME is non-nil, delete it instead
  (let ((file-name (or temp-file-name browse-url-temp-file-name)))
    (if (and file-name (file-exists-p file-name))
	(delete-file file-name))))

(add-hook 'kill-buffer-hook 'browse-url-delete-temp-file)

(declare-function dired-get-filename "dired"
		  (&optional localp no-error-if-not-filep))

;;;###autoload
(defun browse-url-of-dired-file ()
  "In Dired, ask a WWW browser to display the file named on this line."
  (interactive)
  (let ((tem (dired-get-filename t t)))
    (if tem
	(browse-url-of-file (expand-file-name tem))
      (error "No file on this line"))))

;;;###autoload
(defun browse-url-of-region (min max)
  "Ask a WWW browser to display the current region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region min max)
      (browse-url-of-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-independent commands

;; A generic command to call the current browse-url-browser-function

;;;###autoload
(defun browse-url (url &rest args)
  "Ask a WWW browser to load URL.
Prompts for a URL, defaulting to the URL at or before point.  Variable
`browse-url-browser-function' says which browser to use.
If the URL is a mailto: URL, consult `browse-url-mailto-function'
first, if that exists."
  (interactive (browse-url-interactive-arg "URL: "))
  (unless (called-interactively-p 'interactive)
    (setq args (or args (list browse-url-new-window-flag))))
  (let ((process-environment (copy-sequence process-environment))
	(function (or (and (string-match "\\`mailto:" url)
			   browse-url-mailto-function)
		      browse-url-browser-function))
	;; Ensure that `default-directory' exists and is readable (b#6077).
	(default-directory (if (and (file-directory-p default-directory)
				    (file-readable-p default-directory))
			       default-directory
			     (expand-file-name "~/"))))
    ;; When connected to various displays, be careful to use the display of
    ;; the currently selected frame, rather than the original start display,
    ;; which may not even exist any more.
    (if (stringp (frame-parameter (selected-frame) 'display))
        (setenv "DISPLAY" (frame-parameter (selected-frame) 'display)))
    (if (and (consp function)
	     (not (functionp function)))
	;; The `function' can be an alist; look down it for first match
	;; and apply the function (which might be a lambda).
	(catch 'done
	  (dolist (bf function)
	    (when (string-match (car bf) url)
	      (apply (cdr bf) url args)
	      (throw 'done t)))
	  (error "No browse-url-browser-function matching URL %s"
		 url))
      ;; Unbound symbols go down this leg, since void-function from
      ;; apply is clearer than wrong-type-argument from dolist.
      (apply function url args))))

;;;###autoload
(defun browse-url-at-point (&optional arg)
  "Ask a WWW browser to load the URL at or before point.
Doesn't let you edit the URL like `browse-url'.  Variable
`browse-url-browser-function' says which browser to use."
  (interactive "P")
  (let ((url (browse-url-url-at-point)))
    (if url
	(browse-url url (if arg
			    (not browse-url-new-window-flag)
			  browse-url-new-window-flag))
      (error "No URL found"))))

;;;###autoload
(defun browse-url-at-mouse (event)
  "Ask a WWW browser to load a URL clicked with the mouse.
The URL is the one around or before the position of the mouse click
but point is not changed.  Doesn't let you edit the URL like
`browse-url'.  Variable `browse-url-browser-function' says which browser
to use."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    ;; This handles browse-url-new-window-flag properly
    ;; when it gets no arg.
    (browse-url-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-specific commands

;; --- Default MS-Windows browser ---

(defvar dos-windows-version)
(declare-function w32-shell-execute "w32fns.c")    ;; Defined in C.

(defun browse-url-default-windows-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (cond ((eq system-type 'ms-dos)
	 (if dos-windows-version
	     (shell-command (concat "start " (shell-quote-argument url)))
	   (error "Browsing URLs is not supported on this system")))
	((eq system-type 'cygwin)
	 (call-process "cygstart" nil nil nil url))
	(t (w32-shell-execute "open" url))))

(defun browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open " url) nil "open" url))

;; --- Netscape ---

(defun browse-url-process-environment ()
  "Set DISPLAY in the environment to the X display the browser will use.
This is either the value of variable `browse-url-browser-display' if
non-nil, or the same display as Emacs if different from the current
environment, otherwise just use the current environment."
  (let ((display (or browse-url-browser-display (browse-url-emacs-display))))
    (if display
	(cons (concat "DISPLAY=" display) process-environment)
      process-environment)))

(defun browse-url-emacs-display ()
  "Return the X display Emacs is running on.
This is nil if the display is the same as the DISPLAY environment variable.

Actually Emacs could be using several displays; this just returns the
one showing the selected frame."
  (let ((display (cdr-safe (assq 'display (frame-parameters)))))
    (and (not (equal display (getenv "DISPLAY")))
         display)))

(defun browse-url-default-browser (url &rest args)
  "Find a suitable browser and ask it to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (apply
   (cond
    ((memq system-type '(windows-nt ms-dos cygwin))
     'browse-url-default-windows-browser)
    ((memq system-type '(darwin))
     'browse-url-default-macosx-browser)
    ((browse-url-can-use-xdg-open) 'browse-url-xdg-open)
    ((executable-find browse-url-gnome-moz-program) 'browse-url-gnome-moz)
    ((executable-find browse-url-mozilla-program) 'browse-url-mozilla)
    ((executable-find browse-url-firefox-program) 'browse-url-firefox)
    ((executable-find browse-url-chromium-program) 'browse-url-chromium)
    ((executable-find browse-url-galeon-program) 'browse-url-galeon)
    ((executable-find browse-url-kde-program) 'browse-url-kde)
    ((executable-find browse-url-netscape-program) 'browse-url-netscape)
    ((executable-find browse-url-mosaic-program) 'browse-url-mosaic)
    ((executable-find browse-url-xterm-program) 'browse-url-text-xterm)
    ((locate-library "w3") 'browse-url-w3)
    (t
     (lambda (&rest ignore) (error "No usable browser found"))))
   url args))

(defun browse-url-can-use-xdg-open ()
  "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser.
This requires you to be running either Gnome, KDE, Xfce4 or LXDE."
  (and (getenv "DISPLAY")
       (executable-find "xdg-open")
       ;; xdg-open may call gnome-open and that does not wait for its child
       ;; to finish.  This child may then be killed when the parent dies.
       ;; Use nohup to work around.  See bug#7166, bug#8917, bug#9779 and
       ;; http://lists.gnu.org/archive/html/emacs-devel/2009-07/msg00279.html
       (executable-find "nohup")
       (or (getenv "GNOME_DESKTOP_SESSION_ID")
	   ;; GNOME_DESKTOP_SESSION_ID is deprecated, check on Dbus also.
	   (condition-case nil
	       (eq 0 (call-process
		      "dbus-send" nil nil nil
				  "--dest=org.gnome.SessionManager"
				  "--print-reply"
				  "/org/gnome/SessionManager"
				  "org.gnome.SessionManager.CanShutdown"))
	     (error nil))
	   (equal (getenv "KDE_FULL_SESSION") "true")
	   (condition-case nil
	       (eq 0 (call-process
		      "/bin/sh" nil nil nil
		      "-c"
		      ;; FIXME use string-match rather than grep.
		      "xprop -root _DT_SAVE_MODE|grep xfce4"))
	     (error nil))
	   (member (getenv "DESKTOP_SESSION") '("LXDE" "Lubuntu"))
	   (equal (getenv "XDG_CURRENT_DESKTOP") "LXDE"))))


;;;###autoload
(defun browse-url-xdg-open (url &optional ignored)
  "Pass the specified URL to the \"xdg-open\" command.
xdg-open is a desktop utility that calls your preferred web browser.
The optional argument IGNORED is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "xdg-open" nil 0 nil url))

;;;###autoload
(defun browse-url-netscape (url &optional new-window)
  "Ask the Netscape WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-netscape-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
	 (process
	  (apply 'start-process
		 (concat "netscape " url) nil
		 browse-url-netscape-program
		 (append
		  browse-url-netscape-arguments
		  (if (eq window-system 'w32)
		      (list url)
		    (append
		     (if new-window '("-noraise"))
		     (list "-remote"
			   (concat "openURL(" url
				   (if (browse-url-maybe-new-window
					new-window)
				       (if browse-url-netscape-new-window-is-tab
					   ",new-tab"
					 ",new-window"))
				   ")"))))))))
    (set-process-sentinel process
			  `(lambda (process change)
			     (browse-url-netscape-sentinel process ,url)))))

(defun browse-url-netscape-sentinel (process url)
  "Handle a change to the process communicating with Netscape."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Netscape not running - start it
	(message "Starting %s..." browse-url-netscape-program)
	(apply 'start-process (concat "netscape" url) nil
	       browse-url-netscape-program
	       (append browse-url-netscape-startup-arguments (list url))))))

(defun browse-url-netscape-reload ()
  "Ask Netscape to reload its current document.
How depends on `browse-url-netscape-version'."
  (interactive)
  ;; Backwards incompatibility reported by
  ;; <peter.kruse@psychologie.uni-regensburg.de>.
  (browse-url-netscape-send (if (>= browse-url-netscape-version 4)
				"xfeDoCommand(reload)"
			      "reload")))

(defun browse-url-netscape-send (command)
  "Send a remote control command to Netscape."
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process "netscape" nil
           browse-url-netscape-program
           (append browse-url-netscape-arguments
                   (list "-remote" command)))))

;;;###autoload
(defun browse-url-mozilla (url &optional new-window)
  "Ask the Mozilla WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-mozilla-arguments' are also passed to Mozilla.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Mozilla window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-mozilla-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (process
	  (apply 'start-process
		 (concat "mozilla " url) nil
		 browse-url-mozilla-program
		 (append
		  browse-url-mozilla-arguments
		  (list "-remote"
			(concat "openURL("
				url
				(if (browse-url-maybe-new-window
				     new-window)
				    (if browse-url-mozilla-new-window-is-tab
					",new-tab"
				      ",new-window"))
				")"))))))
    (set-process-sentinel process
			  `(lambda (process change)
			     (browse-url-mozilla-sentinel process ,url)))))

(defun browse-url-mozilla-sentinel (process url)
  "Handle a change to the process communicating with Mozilla."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Mozilla is not running - start it
	(message "Starting %s..." browse-url-mozilla-program)
	(apply 'start-process (concat "mozilla " url) nil
	       browse-url-mozilla-program
	       (append browse-url-mozilla-startup-arguments (list url))))))

;;;###autoload
(defun browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-firefox-arguments' are also passed to
Firefox.

When called interactively, if variable
`browse-url-new-window-flag' is non-nil, load the document in a
new Firefox window, otherwise use a random existing one.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument
NEW-WINDOW is used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is
ignored.  Firefox for Windows does not support the \"-remote\"
command line parameter.  Therefore, the
`browse-url-new-window-flag' and `browse-url-firefox-new-window-is-tab'
are ignored as well.  Firefox on Windows will always open the requested
URL in a new window."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
	 (use-remote
	  (not (memq system-type '(windows-nt ms-dos))))
	 (process
	  (apply 'start-process
		 (concat "firefox " url) nil
		 browse-url-firefox-program
		 (append
		  browse-url-firefox-arguments
		  (if use-remote
		      (list "-remote"
			    (concat
			     "openURL("
			     url
			     (if (browse-url-maybe-new-window new-window)
				 (if browse-url-firefox-new-window-is-tab
				     ",new-tab"
				   ",new-window"))
			     ")"))
		    (list url))))))
    ;; If we use -remote, the process exits with status code 2 if
    ;; Firefox is not already running.  The sentinel runs firefox
    ;; directly if that happens.
    (when use-remote
      (set-process-sentinel process
			    `(lambda (process change)
			       (browse-url-firefox-sentinel process ,url))))))

(defun browse-url-firefox-sentinel (process url)
  "Handle a change to the process communicating with Firefox."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Firefox is not running - start it
	(message "Starting Firefox...")
	(apply 'start-process (concat "firefox " url) nil
	       browse-url-firefox-program
	       (append browse-url-firefox-startup-arguments (list url))))))

;;;###autoload
(defun browse-url-chromium (url &optional new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "chromium " url) nil
	   browse-url-chromium-program
	   (append
	    browse-url-chromium-arguments
	    (list url)))))

;;;###autoload
(defun browse-url-galeon (url &optional new-window)
  "Ask the Galeon WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-galeon-arguments' are also passed to Galeon.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Galeon window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-galeon-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
			 (concat "galeon " url)
			 nil
			 browse-url-galeon-program
			 (append
			  browse-url-galeon-arguments
                          (if (browse-url-maybe-new-window new-window)
			      (if browse-url-galeon-new-window-is-tab
				  '("--new-tab")
				'("--new-window" "--noraise"))
                            '("--existing"))
                          (list url)))))
    (set-process-sentinel process
			  `(lambda (process change)
			     (browse-url-galeon-sentinel process ,url)))))

(defun browse-url-galeon-sentinel (process url)
  "Handle a change to the process communicating with Galeon."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Galeon is not running - start it
	(message "Starting %s..." browse-url-galeon-program)
	(apply 'start-process (concat "galeon " url) nil
	       browse-url-galeon-program
	       (append browse-url-galeon-startup-arguments (list url))))))

(defun browse-url-epiphany (url &optional new-window)
  "Ask the Epiphany WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-galeon-arguments' are also passed to Epiphany.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Epiphany window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-epiphany-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
			 (concat "epiphany " url)
			 nil
			 browse-url-epiphany-program
			 (append
			  browse-url-epiphany-arguments
                          (if (browse-url-maybe-new-window new-window)
			      (if browse-url-epiphany-new-window-is-tab
				  '("--new-tab")
				'("--new-window" "--noraise"))
                            '("--existing"))
                          (list url)))))
    (set-process-sentinel process
			  `(lambda (process change)
			     (browse-url-epiphany-sentinel process ,url)))))

(defun browse-url-epiphany-sentinel (process url)
  "Handle a change to the process communicating with Epiphany."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Epiphany is not running - start it
	(message "Starting %s..." browse-url-epiphany-program)
	(apply 'start-process (concat "epiphany " url) nil
	       browse-url-epiphany-program
	       (append browse-url-epiphany-startup-arguments (list url))))))

(defvar url-handler-regexp)

;;;###autoload
(defun browse-url-emacs (url &optional new-window)
  "Ask Emacs to load URL into a buffer and show it in another window."
  (interactive (browse-url-interactive-arg "URL: "))
  (require 'url-handlers)
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    ;; Ignore `new-window': with all other browsers the URL is always shown
    ;; in another window than the current Emacs one since it's shown in
    ;; another application's window.
    ;; (if new-window (find-file-other-window url) (find-file url))
    (find-file-other-window url)))

;;;###autoload
(defun browse-url-gnome-moz (url &optional new-window)
  "Ask Mozilla/Netscape to load URL via the GNOME program `gnome-moz-remote'.
Default to the URL around or before point.  The strings in variable
`browse-url-gnome-moz-arguments' are also passed.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser window, otherwise use an
existing one.  A non-nil interactive prefix argument reverses the
effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply 'start-process (concat "gnome-moz-remote " url)
	 nil
	 browse-url-gnome-moz-program
	 (append
	  browse-url-gnome-moz-arguments
	  (if (browse-url-maybe-new-window new-window)
	      '("--newwin"))
	  (list "--raise" url))))

;; --- Mosaic ---

;;;###autoload
(defun browse-url-mosaic (url &optional new-window)
  "Ask the XMosaic WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-mosaic-arguments' are also passed to Mosaic and the
program is invoked according to the variable
`browse-url-mosaic-program'.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Mosaic window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "Mosaic URL: "))
  (let ((pidfile (expand-file-name browse-url-mosaic-pidfile))
	pid)
    (if (file-readable-p pidfile)
	(save-excursion
	  (find-file pidfile)
	  (goto-char (point-min))
	  (setq pid (read (current-buffer)))
	  (kill-buffer nil)))
    (if (and pid (zerop (signal-process pid 0))) ; Mosaic running
	(save-excursion
	  (find-file (format "/tmp/Mosaic.%d" pid))
	  (erase-buffer)
	  (insert (if (browse-url-maybe-new-window new-window)
		      "newwin\n"
		    "goto\n")
		  url "\n")
	  (save-buffer)
	  (kill-buffer nil)
	  ;; Send signal SIGUSR to Mosaic
	  (message "Signaling Mosaic...")
	  (signal-process pid 'SIGUSR1)
	  ;; Or you could try:
	  ;; (call-process "kill" nil 0 nil "-USR1" (int-to-string pid))
	  (message "Signaling Mosaic...done")
	  )
      ;; Mosaic not running - start it
      (message "Starting %s..." browse-url-mosaic-program)
      (apply 'start-process "xmosaic" nil browse-url-mosaic-program
	     (append browse-url-mosaic-arguments (list url)))
      (message "Starting %s...done" browse-url-mosaic-program))))

;; --- Mosaic using CCI ---

;;;###autoload
(defun browse-url-cci (url &optional new-window)
  "Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point.

This function only works for XMosaic version 2.5 or later.  You must
select `CCI' from XMosaic's File menu, set the CCI Port Address to the
value of variable `browse-url-CCI-port', and enable `Accept requests'.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "Mosaic URL: "))
  (open-network-stream "browse-url" " *browse-url*"
		       browse-url-CCI-host browse-url-CCI-port)
  ;; Todo: start browser if fails
  (process-send-string "browse-url"
		       (concat "get url (" url ") output "
			       (if (browse-url-maybe-new-window new-window)
				   "new"
				 "current")
			       "\r\n"))
  (process-send-string "browse-url" "disconnect\r\n")
  (delete-process "browse-url"))

;; --- W3 ---

;; External.
(declare-function w3-fetch-other-window "ext:w3m" (&optional url))
(declare-function w3-fetch              "ext:w3m" (&optional url target))

;;;###autoload
(defun browse-url-w3 (url &optional new-window)
  "Ask the w3 WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "W3 URL: "))
  (require 'w3)			; w3-fetch-other-window not autoloaded
  (if (browse-url-maybe-new-window new-window)
      (w3-fetch-other-window url)
    (w3-fetch url)))

;;;###autoload
(defun browse-url-w3-gnudoit (url &optional new-window)
  ;; new-window ignored
  "Ask another Emacs running gnuserv to load the URL using the W3 browser.
The `browse-url-gnudoit-program' program is used with options given by
`browse-url-gnudoit-args'.  Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "W3 URL: "))
  (apply 'start-process (concat "gnudoit:" url) nil
	 browse-url-gnudoit-program
	 (append browse-url-gnudoit-args
		 (list (concat "(w3-fetch \"" url "\")")
		       "(raise-frame)"))))

;; --- Lynx in an xterm ---

;;;###autoload
(defun browse-url-text-xterm (url &optional new-window)
  ;; new-window ignored
  "Ask a text browser to load URL.
URL defaults to the URL around or before point.
This runs the text browser specified by `browse-url-text-browser'.
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'."
  (interactive (browse-url-interactive-arg "Text browser URL: "))
  (apply #'start-process `(,(concat browse-url-text-browser url)
			   nil ,browse-url-xterm-program
			   ,@browse-url-xterm-args "-e" ,browse-url-text-browser
			   ,url)))

;; --- Lynx in an Emacs "term" window ---

(declare-function term-char-mode "term" ())
(declare-function term-send-down "term" ())
(declare-function term-send-string "term" (proc str))

;;;###autoload
(defun browse-url-text-emacs (url &optional new-buffer)
  "Ask a text browser to load URL.
URL defaults to the URL around or before point.
This runs the text browser specified by `browse-url-text-browser'.
With a prefix argument, it runs a new browser process in a new buffer.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser process in a new term window,
otherwise use any existing one.  A non-nil interactive prefix argument
reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "Text browser URL: "))
  (let* ((system-uses-terminfo t)     ; Lynx uses terminfo
	 ;; (term-term-name "vt100") ; ??
	 (buf (get-buffer "*text browser*"))
	 (proc (and buf (get-buffer-process buf)))
	 (n browse-url-text-input-attempts))
    (require 'term)
    (if (and (browse-url-maybe-new-window new-buffer) buf)
	;; Rename away the OLD buffer. This isn't very polite, but
	;; term insists on working in a buffer named *lynx* and would
	;; choke on *lynx*<1>
	(progn (set-buffer buf)
	       (rename-uniquely)))
    (if (or (browse-url-maybe-new-window new-buffer)
	    (not buf)
	    (not proc)
	    (not (memq (process-status proc) '(run stop))))
	;; start a new text browser
	(progn
          (setq buf
                (apply #'make-term
                       `(,browse-url-text-browser
			 ,browse-url-text-browser
			 nil ,@browse-url-text-emacs-args
			 ,url)))
          (switch-to-buffer buf)
          (term-char-mode)
          (set-process-sentinel
           (get-buffer-process buf)
           ;; Don't leave around a dead one (especially because of its
           ;; munged keymap.)
           (lambda (process event)
             (if (not (memq (process-status process) '(run stop)))
                 (let ((buf (process-buffer process)))
                   (if buf (kill-buffer buf)))))))
      ;; Send the url to the text browser in the old buffer
      (let ((win (get-buffer-window buf t)))
	(if win
	    (select-window win)
	  (switch-to-buffer buf)))
      (if (eq (following-char) ?_)
	  (cond ((eq browse-url-text-input-field 'warn)
		 (error "Please move out of the input field first"))
		((eq browse-url-text-input-field 'avoid)
		 (while (and (eq (following-char) ?_) (> n 0))
		   (term-send-down)	; down arrow
		   (sit-for browse-url-text-input-delay))
		 (if (eq (following-char) ?_)
		     (error "Cannot move out of the input field, sorry")))))
      (term-send-string proc (concat "g"    ; goto
				     "\C-u" ; kill default url
				     url
				     "\r")))))

;; --- mailto ---

(autoload 'rfc2368-parse-mailto-url "rfc2368")

;;;###autoload
(defun browse-url-mail (url &optional new-window)
  "Open a new mail message buffer within Emacs for the RFC 2368 URL.
Default to using the mailto: URL around or before point as the
recipient's address.  Supplying a non-nil interactive prefix argument
will cause the mail to be composed in another window rather than the
current one.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil use `compose-mail-other-window', otherwise `compose-mail'.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "Mailto URL: "))
  (save-excursion
    (let* ((alist (rfc2368-parse-mailto-url url))
	   (to (assoc "To" alist))
	   (subject (assoc "Subject" alist))
	   (body (assoc "Body" alist))
	   (rest (delq to (delq subject (delq body alist))))
	   (to (cdr to))
	   (subject (cdr subject))
	   (body (cdr body))
	   (mail-citation-hook (unless body mail-citation-hook)))
      (if (browse-url-maybe-new-window new-window)
	  (compose-mail-other-window to subject rest nil
				     (list 'insert-buffer (current-buffer)))
	(compose-mail to subject rest nil nil
		      (list 'insert-buffer (current-buffer))))
      (when body
	(goto-char (point-min))
	(unless (or (search-forward (concat "\n" mail-header-separator "\n")
				    nil 'move)
		    (bolp))
	  (insert "\n"))
	(goto-char (prog1
		       (point)
		     (insert (replace-regexp-in-string "\r\n" "\n" body))
		     (unless (bolp)
		       (insert "\n"))))))))

;; --- Random browser ---

;;;###autoload
(defun browse-url-generic (url &optional new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not browse-url-generic-program)
      (error "No browser defined (`browse-url-generic-program')"))
  (apply 'call-process browse-url-generic-program nil
	 0 nil
	 (append browse-url-generic-args (list url))))

;;;###autoload
(defun browse-url-kde (url &optional new-window)
  "Ask the KDE WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "KDE URL: "))
  (message "Sending URL to KDE...")
  (apply #'start-process (concat "KDE " url) nil browse-url-kde-program
	 (append browse-url-kde-args (list url))))

(defun browse-url-elinks-new-window (url)
  "Ask the Elinks WWW browser to load URL in a new window."
  (let ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (append (list (concat "elinks:" url)
			 nil)
		   browse-url-elinks-wrapper
		   (list "elinks" url)))))

;;;###autoload
(defun browse-url-elinks (url &optional new-window)
  "Ask the Elinks WWW browser to load URL.
Default to the URL around the point.

The document is loaded in a new tab of a running Elinks or, if
none yet running, a newly started instance.

The Elinks command will be prepended by the program+arguments
from `browse-url-elinks-wrapper'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (if new-window
      (browse-url-elinks-new-window url)
    (let ((process-environment (browse-url-process-environment))
	  (elinks-ping-process (start-process "elinks-ping" nil
					      "elinks" "-remote" "ping()")))
      (set-process-sentinel elinks-ping-process
			    `(lambda (process change)
			       (browse-url-elinks-sentinel process ,url))))))

(defun browse-url-elinks-sentinel (process url)
  "Determines if Elinks is running or a new one has to be started."
  (let ((exit-status (process-exit-status process)))
    ;; Try to determine if an instance is running or if we have to
    ;; create a new one.
    (case exit-status
	  (5
	   ;; No instance, start a new one.
	   (browse-url-elinks-new-window url))
	  (0
	   ;; Found an instance, open URL in new tab.
	   (let ((process-environment (browse-url-process-environment)))
	     (start-process (concat "elinks:" url) nil
			    "elinks" "-remote"
			    (concat "openURL(\"" url "\",new-tab)"))))
	  (otherwise
	   (error "Unrecognized exit-code %d of process `elinks'"
		  exit-status)))))

(provide 'browse-url)

;;; browse-url.el ends here
