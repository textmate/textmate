;;; gnus-art.el --- article mode commands for Gnus

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))
(eval-when-compile
  (require 'cl))
(defvar tool-bar-map)
(defvar w3m-minor-mode-map)

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-spec)
(require 'gnus-int)
(require 'gnus-win)
(require 'mm-bodies)
(require 'mail-parse)
(require 'mm-decode)
(require 'mm-view)
(require 'wid-edit)
(require 'mm-uu)
(require 'message)
(require 'mouse)

(autoload 'gnus-msg-mail "gnus-msg" nil t)
(autoload 'gnus-button-mailto "gnus-msg")
(autoload 'gnus-button-reply "gnus-msg" nil t)
(autoload 'parse-time-string "parse-time" nil nil)
(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'mm-url-insert-file-contents-external "mm-url")
(autoload 'mm-extern-cache-contents "mm-extern")

(defgroup gnus-article nil
  "Article display."
  :link '(custom-manual "(gnus)Article Buffer")
  :group 'gnus)

(defgroup gnus-article-treat nil
  "Treating article parts."
  :link '(custom-manual "(gnus)Article Hiding")
  :group 'gnus-article)

(defgroup gnus-article-hiding nil
  "Hiding article parts."
  :link '(custom-manual "(gnus)Article Hiding")
  :group 'gnus-article)

(defgroup gnus-article-highlight nil
  "Article highlighting."
  :link '(custom-manual "(gnus)Article Highlighting")
  :group 'gnus-article
  :group 'gnus-visual)

(defgroup gnus-article-signature nil
  "Article signatures."
  :link '(custom-manual "(gnus)Article Signature")
  :group 'gnus-article)

(defgroup gnus-article-headers nil
  "Article headers."
  :link '(custom-manual "(gnus)Hiding Headers")
  :group 'gnus-article)

(defgroup gnus-article-washing nil
  "Special commands on articles."
  :link '(custom-manual "(gnus)Article Washing")
  :group 'gnus-article)

(defgroup gnus-article-emphasis nil
  "Fontisizing articles."
  :link '(custom-manual "(gnus)Article Fontisizing")
  :group 'gnus-article)

(defgroup gnus-article-saving nil
  "Saving articles."
  :link '(custom-manual "(gnus)Saving Articles")
  :group 'gnus-article)

(defgroup gnus-article-mime nil
  "Worshiping the MIME wonder."
  :link '(custom-manual "(gnus)Using MIME")
  :group 'gnus-article)

(defgroup gnus-article-buttons nil
  "Pushable buttons in the article buffer."
  :link '(custom-manual "(gnus)Article Buttons")
  :group 'gnus-article)

(defgroup gnus-article-various nil
  "Other article options."
  :link '(custom-manual "(gnus)Misc Article")
  :group 'gnus-article)

(defcustom gnus-ignored-headers
  (mapcar
   (lambda (header)
     (concat "^" header ":"))
   '("Path" "Expires" "Date-Received" "References" "Xref" "Lines"
     "Relay-Version" "Message-ID" "Approved" "Sender" "Received"
     "X-UIDL" "MIME-Version" "Return-Path" "In-Reply-To"
     "Content-Type" "Content-Transfer-Encoding" "X-WebTV-Signature"
     "X-MimeOLE" "X-MSMail-Priority" "X-Priority" "X-Loop"
     "X-Authentication-Warning" "X-MIME-Autoconverted" "X-Face"
     "X-Attribution" "X-Originating-IP" "Delivered-To"
     "NNTP-[-A-Za-z]+" "Distribution" "X-no-archive" "X-Trace"
     "X-Complaints-To" "X-NNTP-Posting-Host" "X-Orig.*"
     "Abuse-Reports-To" "Cache-Post-Path" "X-Article-Creation-Date"
     "X-Poster" "X-Mail2News-Path" "X-Server-Date" "X-Cache"
     "Originator" "X-Problems-To" "X-Auth-User" "X-Post-Time"
     "X-Admin" "X-UID" "Resent-[-A-Za-z]+" "X-Mailing-List"
     "Precedence" "Original-[-A-Za-z]+" "X-filename" "X-Orcpt"
     "Old-Received" "X-Pgp" "X-Auth" "X-From-Line"
     "X-Gnus-Article-Number" "X-Majordomo" "X-Url" "X-Sender"
     "MBOX-Line" "Priority" "X400-[-A-Za-z]+"
     "Status" "X-Gnus-Mail-Source" "Cancel-Lock"
     "X-FTN" "X-EXP32-SerialNo" "Encoding" "Importance"
     "Autoforwarded" "Original-Encoded-Information-Types" "X-Ya-Pop3"
     "X-Face-Version" "X-Vms-To" "X-ML-NAME" "X-ML-COUNT"
     "Mailing-List" "X-finfo" "X-md5sum" "X-md5sum-Origin"
     "X-Sun-Charset" "X-Accept-Language" "X-Envelope-Sender"
     "List-[A-Za-z]+" "X-Listprocessor-Version"
     "X-Received" "X-Distribute" "X-Sequence" "X-Juno-Line-Breaks"
     "X-Notes-Item" "X-MS-TNEF-Correlator" "x-uunet-gateway"
     "X-Received" "Content-length" "X-precedence"
     "X-Authenticated-User" "X-Comment" "X-Report" "X-Abuse-Info"
     "X-HTTP-Proxy" "X-Mydeja-Info" "X-Copyright" "X-No-Markup"
     "X-Abuse-Info" "X-From_" "X-Accept-Language" "Errors-To"
     "X-BeenThere" "X-Mailman-Version" "List-Help" "List-Post"
     "List-Subscribe" "List-Id" "List-Unsubscribe" "List-Archive"
     "X-Content-length" "X-Posting-Agent" "Original-Received"
     "X-Request-PGP" "X-Fingerprint" "X-WRIEnvto" "X-WRIEnvfrom"
     "X-Virus-Scanned" "X-Delivery-Agent" "Posted-Date" "X-Gateway"
     "X-Local-Origin" "X-Local-Destination" "X-UserInfo1"
     "X-Received-Date" "X-Hashcash" "Face" "X-DMCA-Notifications"
     "X-Abuse-and-DMCA-Info" "X-Postfilter" "X-Gpg-.*" "X-Disclaimer"
     "Envelope-To" "X-Spam-Score" "System-Type" "X-Injected-Via-Gmane"
     "X-Gmane-NNTP-Posting-Host" "Jabber-ID" "Archived-At"
     "Envelope-Sender" "Envelope-Recipients"))
  "*All headers that start with this regexp will be hidden.
This variable can also be a list of regexps of headers to be ignored.
If `gnus-visible-headers' is non-nil, this variable will be ignored."
  :type '(choice regexp
		 (repeat regexp))
  :group 'gnus-article-hiding)

(defcustom gnus-visible-headers
  "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:"
  "*All headers that do not match this regexp will be hidden.
This variable can also be a list of regexp of headers to remain visible.
If this variable is non-nil, `gnus-ignored-headers' will be ignored."
  :type '(choice
	  (repeat :value-to-internal (lambda (widget value)
				       (custom-split-regexp-maybe value))
		  :match (lambda (widget value)
			   (or (stringp value)
			       (widget-editable-list-match widget value)))
		  regexp)
	  (const :tag "Use gnus-ignored-headers" nil)
	  regexp)
  :group 'gnus-article-hiding)

(defcustom gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:"
    "^Followup-To:" "^To:" "^Cc:" "^Date:" "^Organization:")
  "*This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list."
  :type '(repeat regexp)
  :group 'gnus-article-hiding)

(defcustom gnus-boring-article-headers '(empty followup-to reply-to)
  "Headers that are only to be displayed if they have interesting data.
Possible values in this list are:

  'empty       Headers with no content.
  'newsgroups  Newsgroup identical to Gnus group.
  'to-address  To identical to To-address.
  'to-list     To identical to To-list.
  'cc-list     CC identical to To-list.
  'followup-to Followup-to identical to Newsgroups.
  'reply-to    Reply-to identical to From.
  'date        Date less than four days old.
  'long-to     To and/or Cc longer than 1024 characters.
  'many-to     Multiple To and/or Cc."
  :type '(set (const :tag "Headers with no content." empty)
	      (const :tag "Newsgroups identical to Gnus group." newsgroups)
	      (const :tag "To identical to To-address." to-address)
	      (const :tag "To identical to To-list." to-list)
	      (const :tag "CC identical to To-list." cc-list)
	      (const :tag "Followup-to identical to Newsgroups." followup-to)
	      (const :tag "Reply-to identical to From." reply-to)
	      (const :tag "Date less than four days old." date)
	      (const :tag "To and/or Cc longer than 1024 characters." long-to)
	      (const :tag "Multiple To and/or Cc headers." many-to))
  :group 'gnus-article-hiding)

(defcustom gnus-article-skip-boring nil
  "Skip over text that is not worth reading.
By default, if you set this t, then Gnus will display citations and
signatures, but will never scroll down to show you a page consisting
only of boring text.  Boring text is controlled by
`gnus-article-boring-faces'."
  :version "22.1"
  :type 'boolean
  :group 'gnus-article-hiding)

(defcustom gnus-signature-separator '("^-- $" "^-- *$")
  "Regexp matching signature separator.
This can also be a list of regexps.  In that case, it will be checked
from head to tail looking for a separator.  Searches will be done from
the end of the buffer."
  :type '(choice :format "%{%t%}: %[Value Menu%]\n%v"
		 (regexp)
		 (repeat :tag "List of regexp" regexp))
  :group 'gnus-article-signature)

(defcustom gnus-signature-limit nil
  "Provide a limit to what is considered a signature.
If it is a number, no signature may not be longer (in characters) than
that number.  If it is a floating point number, no signature may be
longer (in lines) than that number.  If it is a function, the function
will be called without any parameters, and if it returns nil, there is
no signature in the buffer.  If it is a string, it will be used as a
regexp.  If it matches, the text in question is not a signature.

This can also be a list of the above values."
  :type '(choice (const nil)
		 (integer :value 200)
		 (number :value 4.0)
		 function
		 (regexp :value ".*"))
  :group 'gnus-article-signature)

(defcustom gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text."
  :type 'sexp
  :group 'gnus-article-hiding)

;; Fixme: This isn't the right thing for mixed graphical and non-graphical
;; frames in a session.
(defcustom gnus-article-x-face-command
  (if (featurep 'xemacs)
      (if (or (gnus-image-type-available-p 'xface)
	      (gnus-image-type-available-p 'pbm))
	  'gnus-display-x-face-in-from
	"{ echo \
'/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */'\
; uncompface; } | icontopbm | ee -")
    (if (gnus-image-type-available-p 'pbm)
	'gnus-display-x-face-in-from
      "{ echo \
'/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */'\
; uncompface; } | icontopbm | display -"))
  "*String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.  The compressed face will be piped to this command."
  :type `(choice string
		 (function-item gnus-display-x-face-in-from)
		 function)
  :version "21.1"
  :group 'gnus-picon
  :group 'gnus-article-washing)

(defcustom gnus-article-x-face-too-ugly nil
  "Regexp matching posters whose face shouldn't be shown automatically."
  :type '(choice regexp (const nil))
  :group 'gnus-article-washing)

(defcustom gnus-article-banner-alist nil
  "Banner alist for stripping.
For example,
     ((egroups . \"^[ \\t\\n]*-------------------+\\\\( \\\\(e\\\\|Yahoo! \\\\)Groups Sponsor -+\\\\)?....\\n\\\\(.+\\n\\\\)+\"))"
  :version "21.1"
  :type '(repeat (cons symbol regexp))
  :group 'gnus-article-washing)

(gnus-define-group-parameter
 banner
 :variable-document
 "Alist of regexps (to match group names) and banner."
 :variable-group gnus-article-washing
 :parameter-type
 '(choice :tag "Banner"
	  :value nil
	  (const :tag "Remove signature" signature)
	  (symbol :tag "Item in `gnus-article-banner-alist'" none)
	  regexp
	  (const :tag "None" nil))
 :parameter-document
 "If non-nil, specify how to remove `banners' from articles.

Symbol `signature' means to remove signatures delimited by
`gnus-signature-separator'.  Any other symbol is used to look up a
regular expression to match the banner in `gnus-article-banner-alist'.
A string is used as a regular expression to match the banner
directly.")

(defcustom gnus-article-address-banner-alist nil
  "Alist of mail addresses and banners.
Each element has the form (ADDRESS . BANNER), where ADDRESS is a regexp
to match a mail address in the From: header, BANNER is one of a symbol
`signature', an item in `gnus-article-banner-alist', a regexp and nil.
If ADDRESS matches author's mail address, it will remove things like
advertisements.  For example:

\((\"@yoo-hoo\\\\.co\\\\.jp\\\\'\" . \"\\n_+\\nDo You Yoo-hoo!\\\\?\\n.*\\n.*\\n\"))
"
  :type '(repeat
	  (cons
	   (regexp :tag "Address")
	   (choice :tag "Banner" :value nil
		   (const :tag "Remove signature" signature)
		   (symbol :tag "Item in `gnus-article-banner-alist'" none)
		   regexp
		   (const :tag "None" nil))))
  :version "22.1"
  :group 'gnus-article-washing)

(defmacro gnus-emphasis-custom-with-format (&rest body)
  `(let ((format "\
\\(\\s-\\|^\\|\\=\\|[-\"]\\|\\s(\\)\\(%s\\(\\w+\\(\\s-+\\w+\\)*[.,]?\\)%s\\)\
\\(\\([-,.;:!?\"]\\|\\s)\\)+\\s-\\|[?!.]\\s-\\|\\s)\\|\\s-\\)"))
     ,@body))

(defun gnus-emphasis-custom-value-to-external (value)
  (gnus-emphasis-custom-with-format
   (if (consp (car value))
       (list (format format (car (car value)) (cdr (car value)))
	     2
	     (if (nth 1 value) 2 3)
	     (nth 2 value))
     value)))

(defun gnus-emphasis-custom-value-to-internal (value)
  (gnus-emphasis-custom-with-format
   (let ((regexp (concat "\\`"
			 (format (regexp-quote format)
				 "\\([^()]+\\)" "\\([^()]+\\)")
			 "\\'"))
	 pattern)
     (if (string-match regexp (setq pattern (car value)))
	 (list (cons (match-string 1 pattern) (match-string 2 pattern))
	       (= (nth 2 value) 2)
	       (nth 3 value))
       value))))

(defcustom gnus-emphasis-alist
  (let ((types
	 '(("\\*" "\\*" bold nil 2)
	   ("_" "_" underline)
	   ("/" "/" italic)
	   ("_/" "/_" underline-italic)
	   ("_\\*" "\\*_" underline-bold)
	   ("\\*/" "/\\*" bold-italic)
	   ("_\\*/" "/\\*_" underline-bold-italic))))
    (nconc
     (gnus-emphasis-custom-with-format
      (mapcar (lambda (spec)
		(list (format format (car spec) (cadr spec))
		      (or (nth 3 spec) 2)
		      (or (nth 4 spec) 3)
		      (intern (format "gnus-emphasis-%s" (nth 2 spec)))))
	      types))
     '(;; I've never seen anyone use this strikethru convention whereas I've
       ;; several times seen it triggered by normal text.  --Stef
       ;; Miles suggests that this form is sometimes used but for italics,
       ;; so maybe we should map it to `italic'.
       ;; ("\\(\\s-\\|^\\)\\(-\\(\\(\\w\\|-[^-]\\)+\\)-\\)\\(\\s-\\|[?!.,;]\\)"
       ;; 2 3 gnus-emphasis-strikethru)
       ("\\(\\s-\\|^\\)\\(_\\(\\(\\w\\|_[^_]\\)+\\)_\\)\\(\\s-\\|[?!.,;]\\)"
	2 3 gnus-emphasis-underline))))
  "*Alist that says how to fontify certain phrases.
Each item looks like this:

  (\"_\\\\(\\\\w+\\\\)_\" 0 1 'underline)

The first element is a regular expression to be matched.  The second
is a number that says what regular expression grouping used to find
the entire emphasized word.  The third is a number that says what
regexp grouping should be displayed and highlighted.  The fourth
is the face used for highlighting."
  :type
  '(repeat
    (menu-choice
     :format "%[Customizing Style%]\n%v"
     :indent 2
     (group :tag "Default"
	    :value ("" 0 0 default)
	    :value-create
	    (lambda (widget)
	      (let ((value (widget-get
			    (cadr (widget-get (widget-get widget :parent)
					      :args))
			    :value)))
		(if (not (eq (nth 2 value) 'default))
		    (widget-put
		     widget
		     :value
		     (gnus-emphasis-custom-value-to-external value))))
	      (widget-group-value-create widget))
	    regexp
	    (integer :format "Match group: %v")
	    (integer :format "Emphasize group: %v")
	    face)
     (group :tag "Simple"
	    :value (("_" . "_") nil default)
	    (cons :format "%v"
		  (regexp :format "Start regexp: %v")
		  (regexp :format "End regexp: %v"))
	    (boolean :format "Show start and end patterns: %[%v%]\n"
		     :on " On " :off " Off ")
	    face)))
  :get (lambda (symbol)
	 (mapcar 'gnus-emphasis-custom-value-to-internal
		 (default-value symbol)))
  :set (lambda (symbol value)
	 (set-default symbol (mapcar 'gnus-emphasis-custom-value-to-external
				     value)))
  :group 'gnus-article-emphasis)

(defcustom gnus-emphasize-whitespace-regexp "^[ \t]+\\|[ \t]*\n"
  "A regexp to describe whitespace which should not be emphasized.
Typical values are \"^[ \\t]+\\\\|[ \\t]*\\n\" and \"[ \\t]+\\\\|[ \\t]*\\n\".
The former avoids underlining of leading and trailing whitespace,
and the latter avoids underlining any whitespace at all."
  :version "21.1"
  :group 'gnus-article-emphasis
  :type 'regexp)

(defface gnus-emphasis-bold '((t (:bold t)))
  "Face used for displaying strong emphasized text (*word*)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-italic '((t (:italic t)))
  "Face used for displaying italic emphasized text (/word/)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline '((t (:underline t)))
  "Face used for displaying underlined emphasized text (_word_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-bold '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-italic '((t (:italic t :underline t)))
  "Face used for displaying underlined italic emphasized text (_/word/_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-bold-italic '((t (:bold t :italic t)))
  "Face used for displaying bold italic emphasized text (/*word*/)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-bold-italic
  '((t (:bold t :italic t :underline t)))
  "Face used for displaying underlined bold italic emphasized text.
Example: (_/*word*/_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-strikethru (if (featurep 'xemacs)
				      '((t (:strikethru t)))
				    '((t (:strike-through t))))
  "Face used for displaying strike-through text (-word-)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-highlight-words
  '((t (:background "black" :foreground "yellow")))
  "Face used for displaying highlighted words."
  :group 'gnus-article-emphasis)

(defcustom gnus-article-time-format "%a, %d %b %Y %T %Z"
  "Format for display of Date headers in article bodies.
See `format-time-string' for the possible values.

The variable can also be function, which should return a complete Date
header.  The function is called with one argument, the time, which can
be fed to `format-time-string'."
  :type '(choice string function)
  :link '(custom-manual "(gnus)Article Date")
  :group 'gnus-article-washing)

(defcustom gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving.
This will be overridden by the `:headers' property that the symbol of
the saver function, which is specified by `gnus-default-article-saver',
might have."
  :group 'gnus-article-saving
  :type 'boolean)

(defcustom gnus-prompt-before-saving 'always
  "*This variable says how much prompting is to be done when saving articles.
If it is nil, no prompting will be done, and the articles will be
saved to the default files.  If this variable is `always', each and
every article that is saved will be preceded by a prompt, even when
saving large batches of articles.  If this variable is neither nil not
`always', there the user will be prompted once for a file name for
each invocation of the saving commands."
  :group 'gnus-article-saving
  :type '(choice (item always)
		 (item :tag "never" nil)
		 (sexp :tag "once" :format "%t\n" :value t)))

(defcustom gnus-saved-headers gnus-visible-headers
  "Headers to keep if `gnus-save-all-headers' is nil.
If `gnus-save-all-headers' is non-nil, this variable will be ignored.
If that variable is nil, however, all headers that match this regexp
will be kept while the rest will be deleted before saving.  This and
`gnus-save-all-headers' will be overridden by the `:headers' property
that the symbol of the saver function, which is specified by
`gnus-default-article-saver', might have."
  :group 'gnus-article-saving
  :type 'regexp)

;; Note that "Rmail format" is mbox since Emacs 23, but Babyl before.
(defcustom gnus-default-article-saver 'gnus-summary-save-in-rmail
  "A function to save articles in your favorite format.
The function will be called by way of the `gnus-summary-save-article'
command, and friends such as `gnus-summary-save-article-rmail'.

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format)
* gnus-summary-save-body-in-file (article body)
* gnus-summary-save-in-vm (use VM's folder format)
* gnus-summary-write-to-file (article format -- overwrite)
* gnus-summary-write-body-to-file (article body -- overwrite)
* gnus-summary-save-in-pipe (article format)

The symbol of each function may have the following properties:

* :decode
The value non-nil means save decoded articles.  This is meaningful
only with `gnus-summary-save-in-file', `gnus-summary-save-body-in-file',
`gnus-summary-write-to-file', `gnus-summary-write-body-to-file', and
`gnus-summary-save-in-pipe'.

* :function
The value specifies an alternative function which appends, not
overwrites, articles to a file.  This implies that when saving many
articles at a time, `gnus-prompt-before-saving' is bound to t and all
articles are saved in a single file.  This is meaningful only with
`gnus-summary-write-to-file' and `gnus-summary-write-body-to-file'.

* :headers
The value specifies the symbol of a variable of which the value
specifies headers to be saved.  If it is omitted,
`gnus-save-all-headers' and `gnus-saved-headers' control what
headers should be saved."
  :group 'gnus-article-saving
  :type '(radio (function-item gnus-summary-save-in-rmail)
		(function-item gnus-summary-save-in-mail)
		(function-item gnus-summary-save-in-folder)
		(function-item gnus-summary-save-in-file)
		(function-item gnus-summary-save-body-in-file)
		(function-item gnus-summary-save-in-vm)
		(function-item gnus-summary-write-to-file)
		(function-item gnus-summary-write-body-to-file)
		(function-item gnus-summary-save-in-pipe)
		(function)))

(defcustom gnus-article-save-coding-system
  (or (and (mm-coding-system-p 'utf-8) 'utf-8)
      (and (mm-coding-system-p 'iso-2022-7bit) 'iso-2022-7bit)
      (and (mm-coding-system-p 'emacs-mule) 'emacs-mule)
      (and (mm-coding-system-p 'escape-quoted) 'escape-quoted))
  "Coding system used to save decoded articles to a file.

The recommended coding systems are `utf-8', `iso-2022-7bit' and so on,
which can safely encode any characters in text.  This is used by the
commands including:

* gnus-summary-save-article-file
* gnus-summary-save-article-body-file
* gnus-summary-write-article-file
* gnus-summary-write-article-body-file

and the functions to which you may set `gnus-default-article-saver':

* gnus-summary-save-in-file
* gnus-summary-save-body-in-file
* gnus-summary-write-to-file
* gnus-summary-write-body-to-file

Those commands and functions save just text displayed in the article
buffer to a file if the value of this variable is non-nil.  Note that
buttonized MIME parts will be lost in a saved file in that case.
Otherwise, raw articles will be saved."
  :group 'gnus-article-saving
  :type `(choice
	  :format "%{%t%}:\n %[Value Menu%] %v"
	  (const :tag "Save raw articles" nil)
	  ,@(delq nil
		  (mapcar
		   (lambda (arg) (if (mm-coding-system-p (nth 3 arg)) arg))
		   '((const :tag "UTF-8" utf-8)
		     (const :tag "iso-2022-7bit" iso-2022-7bit)
		     (const :tag "Emacs internal" emacs-mule)
		     (const :tag "escape-quoted" escape-quoted))))
	  (symbol :tag "Coding system")))

(defcustom gnus-rmail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-mail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-folder-save-name 'gnus-folder-save-name
  "A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-file-save-name 'gnus-numeric-save-name
  "A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional
LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-split-methods
  '((gnus-article-archive-name)
    (gnus-article-nndoc-name))
  "*Variable used to suggest where articles are to be saved.
For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))

This variable is an alist where the key is the match and the
value is a list of possible files to save in if the match is
non-nil.

If the match is a string, it is used as a regexp match on the
article.  If the match is a symbol, that symbol will be funcalled
from the buffer of the article to be saved with the newsgroup as the
parameter.  If it is a list, it will be evalled in the same buffer.

If this form or function returns a string, this string will be used as a
possible file name; and if it returns a non-nil list, that list will be
used as possible file names."
  :group 'gnus-article-saving
  :type '(repeat (choice (list :value (fun) function)
			 (cons :value ("" "") regexp (repeat string))
			 (sexp :value nil))))

(defcustom gnus-page-delimiter "^\^L"
  "*Regexp describing what to use as article page delimiters.
The default value is \"^\^L\", which is a form linefeed at the
beginning of a line."
  :type 'regexp
  :group 'gnus-article-various)

(defcustom gnus-article-mode-line-format "Gnus: %g %S%m"
  "*The format specification for the article mode line.
See `gnus-summary-mode-line-format' for a closer description.

The following additional specs are available:

%w  The article washing status.
%m  The number of MIME parts in the article."
  :version "24.1"
  :type 'string
  :group 'gnus-article-various)

(defcustom gnus-article-mode-hook nil
  "*A hook for Gnus article mode."
  :type 'hook
  :group 'gnus-article-various)

(when (featurep 'xemacs)
  ;; Extracted from gnus-xmas-define in order to preserve user settings
  (when (fboundp 'turn-off-scroll-in-place)
    (add-hook 'gnus-article-mode-hook 'turn-off-scroll-in-place))
  ;; Extracted from gnus-xmas-redefine in order to preserve user settings
  (add-hook 'gnus-article-mode-hook 'gnus-xmas-article-menu-add))

(defcustom gnus-article-menu-hook nil
  "*Hook run after the creation of the article mode menu."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-copy-article-ignored-headers nil
  "List of headers to be removed when copying an article.
Each element is a regular expression."
  :version "23.1" ;; No Gnus
  :type '(repeat regexp)
  :group 'gnus-article-various)

(make-obsolete-variable 'gnus-article-hide-pgp-hook nil
			"Gnus 5.10 (Emacs 22.1)")

(defface gnus-button
  '((t (:weight bold)))
  "Face used for highlighting a button in the article buffer."
  :group 'gnus-article-buttons)

(defcustom gnus-article-button-face 'gnus-button
  "Face used for highlighting buttons in the article buffer.

An article button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it."
  :type 'face
  :group 'gnus-article-buttons)

(defcustom gnus-article-mouse-face 'highlight
  "Face used for mouse highlighting in the article buffer.

Article buttons will be displayed in this face when the cursor is
above them."
  :type 'face
  :group 'gnus-article-buttons)

(defcustom gnus-signature-face 'gnus-signature
  "Face used for highlighting a signature in the article buffer.
Obsolete; use the face `gnus-signature' for customizations instead."
  :type 'face
  :group 'gnus-article-highlight
  :group 'gnus-article-signature)

(defface gnus-signature
  '((t
     (:italic t)))
  "Face used for highlighting a signature in the article buffer."
  :group 'gnus-article-highlight
  :group 'gnus-article-signature)
;; backward-compatibility alias
(put 'gnus-signature-face 'face-alias 'gnus-signature)
(put 'gnus-signature-face 'obsolete-face "22.1")

(defface gnus-header-from
  '((((class color)
      (background dark))
     (:foreground "PaleGreen1"))
    (((class color)
      (background light))
     (:foreground "red3"))
    (t
     (:italic t)))
  "Face used for displaying from headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)
;; backward-compatibility alias
(put 'gnus-header-from-face 'face-alias 'gnus-header-from)
(put 'gnus-header-from-face 'obsolete-face "22.1")

(defface gnus-header-subject
  '((((class color)
      (background dark))
     (:foreground "SeaGreen1"))
    (((class color)
      (background light))
     (:foreground "red4"))
    (t
     (:bold t :italic t)))
  "Face used for displaying subject headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)
;; backward-compatibility alias
(put 'gnus-header-subject-face 'face-alias 'gnus-header-subject)
(put 'gnus-header-subject-face 'obsolete-face "22.1")

(defface gnus-header-newsgroups
  '((((class color)
      (background dark))
     (:foreground "yellow" :italic t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :italic t))
    (t
     (:italic t)))
  "Face used for displaying newsgroups headers.
In the default setup this face is only used for crossposted
articles."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)
;; backward-compatibility alias
(put 'gnus-header-newsgroups-face 'face-alias 'gnus-header-newsgroups)
(put 'gnus-header-newsgroups-face 'obsolete-face "22.1")

(defface gnus-header-name
  '((((class color)
      (background dark))
     (:foreground "SpringGreen2"))
    (((class color)
      (background light))
     (:foreground "maroon"))
    (t
     (:bold t)))
  "Face used for displaying header names."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)
;; backward-compatibility alias
(put 'gnus-header-name-face 'face-alias 'gnus-header-name)
(put 'gnus-header-name-face 'obsolete-face "22.1")

(defface gnus-header-content
  '((((class color)
      (background dark))
     (:foreground "SpringGreen1" :italic t))
    (((class color)
      (background light))
     (:foreground "indianred4" :italic t))
    (t
     (:italic t)))  "Face used for displaying header content."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)
;; backward-compatibility alias
(put 'gnus-header-content-face 'face-alias 'gnus-header-content)
(put 'gnus-header-content-face 'obsolete-face "22.1")

(defcustom gnus-header-face-alist
  '(("From" nil gnus-header-from)
    ("Subject" nil gnus-header-subject)
    ("Newsgroups:.*," nil gnus-header-newsgroups)
    ("" gnus-header-name gnus-header-content))
  "*Controls highlighting of article headers.

An alist of the form (HEADER NAME CONTENT).

HEADER is a regular expression which should match the name of a
header and NAME and CONTENT are either face names or nil.

The name of each header field will be displayed using the face
specified by the first element in the list where HEADER matches
the header name and NAME is non-nil.  Similarly, the content will
be displayed by the first non-nil matching CONTENT face."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight
  :type '(repeat (list (regexp :tag "Header")
		       (choice :tag "Name"
			       (item :tag "skip" nil)
			       (face :value default))
		       (choice :tag "Content"
			       (item :tag "skip" nil)
			       (face :value default)))))

(defcustom gnus-face-properties-alist (if (featurep 'xemacs)
					  '((xface . (:face gnus-x-face)))
					'((pbm . (:face gnus-x-face))
					  (png . nil)))
  "Alist of image types and properties applied to Face and X-Face images.
Here are examples:

;; Specify the altitude of Face images in the From header.
\(setq gnus-face-properties-alist
      '((pbm . (:face gnus-x-face :ascent 80))
	(png . (:ascent 80))))

;; Show Face images as pressed buttons.
\(setq gnus-face-properties-alist
      '((pbm . (:face gnus-x-face :relief -2))
	(png . (:relief -2))))

See the manual for the valid properties for various image types.
Currently, `pbm' is used for X-Face images and `png' is used for Face
images in Emacs.  Only the `:face' property is effective on the `xface'
image type in XEmacs if it is built with the libcompface library."
  :version "23.1" ;; No Gnus
  :group 'gnus-article-headers
  :type '(repeat (cons :format "%v" (symbol :tag "Image type") plist)))

(defcustom gnus-article-decode-hook
  '(article-decode-charset article-decode-encoded-words
			   article-decode-group-name article-decode-idna-rhs)
  "*Hook run to decode charsets in articles."
  :group 'gnus-article-headers
  :type 'hook)

(defcustom gnus-display-mime-function 'gnus-display-mime
  "Function to display MIME articles."
  :group 'gnus-article-mime
  :type 'function)

(defvar gnus-decode-header-function 'mail-decode-encoded-word-region
  "Function used to decode headers.")

(defvar gnus-decode-address-function 'mail-decode-encoded-address-region
  "Function used to decode addresses.")

(defvar gnus-article-dumbquotes-map
  '((?\200 "EUR")
    (?\202 ",")
    (?\203 "f")
    (?\204 ",,")
    (?\205 "...")
    (?\213 "<")
    (?\214 "OE")
    (?\221 "`")
    (?\222 "'")
    (?\223 "``")
    (?\224 "\"")
    (?\225 "*")
    (?\226 "-")
    (?\227 "--")
    (?\230 "~")
    (?\231 "(TM)")
    (?\233 ">")
    (?\234 "oe")
    (?\264 "'"))
  "Table for MS-to-Latin1 translation.")

(defcustom gnus-ignored-mime-types nil
  "List of MIME types that should be ignored by Gnus."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(repeat regexp))

(defcustom gnus-unbuttonized-mime-types '(".*/.*")
  "List of MIME types that should not be given buttons when rendered inline.
See also `gnus-buttonized-mime-types' which may override this variable.
This variable is only used when `gnus-inhibit-mime-unbuttonizing' is nil."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(repeat regexp))

(defcustom gnus-buttonized-mime-types nil
  "List of MIME types that should be given buttons when rendered inline.
If set, this variable overrides `gnus-unbuttonized-mime-types'.
To see e.g. security buttons you could set this to
`(\"multipart/signed\")'.  You could also add \"multipart/alternative\" to
this list to display radio buttons that allow you to choose one of two
media types those mails include.  See also `mm-discouraged-alternatives'.
This variable is only used when `gnus-inhibit-mime-unbuttonizing' is nil."
  :version "22.1"
  :group 'gnus-article-mime
  :type '(repeat regexp))

(defcustom gnus-inhibit-mime-unbuttonizing nil
  "If non-nil, all MIME parts get buttons.
When nil (the default value), then some MIME parts do not get buttons,
as described by the variables `gnus-buttonized-mime-types' and
`gnus-unbuttonized-mime-types'."
  :version "22.1"
  :group 'gnus-article-mime
  :type 'boolean)

(defcustom gnus-body-boundary-delimiter "_"
  "String used to delimit header and body.
This variable is used by `gnus-article-treat-body-boundary' which can
be controlled by `gnus-treat-body-boundary'."
  :version "22.1"
  :group 'gnus-article-various
  :type '(choice (item :tag "None" :value nil)
		 string))

(defcustom gnus-picon-databases '("/usr/lib/picon" "/usr/local/faces"
				  "/usr/share/picons")
  "Defines the location of the faces database.
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html"
  :version "22.1"
  :type '(repeat directory)
  :link '(url-link :tag "download"
		   "http://www.cs.indiana.edu/picons/ftp/index.html")
  :link '(custom-manual "(gnus)Picons")
  :group 'gnus-picon)

(defun gnus-picons-installed-p ()
  "Say whether picons are installed on your machine."
  (let ((installed nil))
    (dolist (database gnus-picon-databases)
      (when (file-exists-p database)
	(setq installed t)))
    installed))

(defcustom gnus-article-mime-part-function nil
  "Function called with a MIME handle as the argument.
This is meant for people who want to do something automatic based
on parts -- for instance, adding Vcard info to a database."
  :group 'gnus-article-mime
  :type '(choice (const nil)
		 function))

(defcustom gnus-mime-multipart-functions nil
  "An alist of MIME types to functions to display them."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(repeat (cons :format "%v" (string :tag "MIME type") function)))

(defcustom gnus-article-date-headers '(combined-lapsed)
  "A list of Date header formats to display.
Valid formats are `ut' (universal time), `local' (local time
zone), `english' (readable English), `lapsed' (elapsed time),
`combined-lapsed' (both the original date and the elapsed time),
`original' (the original date header), `iso8601' (ISO8601
format), and `user-defined' (a user-defined format defined by the
`gnus-article-time-format' variable).

You have as many date headers as you want in the article buffer.
Some of these headers are updated automatically.  See
`gnus-article-update-date-headers' for details."
  :version "24.1"
  :group 'gnus-article-headers
  :type '(repeat
	  (item :tag "Universal time (UT)" :value 'ut)
	  (item :tag "Local time zone" :value 'local)
	  (item :tag "Readable English" :value 'english)
	  (item :tag "Elapsed time" :value 'lapsed)
	  (item :tag "Original and elapsed time" :value 'combined-lapsed)
	  (item :tag "Original date header" :value 'original)
	  (item :tag "ISO8601 format" :value 'iso8601)
	  (item :tag "User-defined" :value 'user-defined)))

(defcustom gnus-article-update-date-headers nil
  "A number that says how often to update the date header (in seconds).
If nil, don't update it at all."
  :version "24.1"
  :group 'gnus-article-headers
  :type '(choice
	  (item :tag "Don't update" :value nil)
	  integer))

(defcustom gnus-article-mime-match-handle-function 'undisplayed-alternative
  "Function called with a MIME handle as the argument.
This is meant for people who want to view first matched part.
For `undisplayed-alternative' (default), the first undisplayed
part or alternative part is used.  For `undisplayed', the first
undisplayed part is used.  For a function, the first part which
the function return t is used.  For nil, the first part is
used."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(choice
	  (item :tag "first" :value nil)
	  (item :tag "undisplayed" :value undisplayed)
	  (item :tag "undisplayed or alternative"
		:value undisplayed-alternative)
	  (function)))

(defcustom gnus-mime-action-alist
  '(("save to file" . gnus-mime-save-part)
    ("save and strip" . gnus-mime-save-part-and-strip)
    ("replace with file" . gnus-mime-replace-part)
    ("delete part" . gnus-mime-delete-part)
    ("display as text" . gnus-mime-inline-part)
    ("view the part" . gnus-mime-view-part)
    ("pipe to command" . gnus-mime-pipe-part)
    ("toggle display" . gnus-article-press-button)
    ("toggle display" . gnus-article-view-part-as-charset)
    ("view as type" . gnus-mime-view-part-as-type)
    ("view internally" . gnus-mime-view-part-internally)
    ("view externally" . gnus-mime-view-part-externally))
  "An alist of actions that run on the MIME attachment."
  :group 'gnus-article-mime
  :type '(repeat (cons (string :tag "name")
		       (function))))

(defcustom gnus-auto-select-part 1
  "Advance to next MIME part when deleting or stripping parts.

When 0, point will be placed on the same part as before.  When
positive (negative), move point forward (backwards) this many
parts.  When nil, redisplay article."
  :version "23.1" ;; No Gnus
  :group 'gnus-article-mime
  :type '(choice (const nil :tag "Redisplay article.")
		 (const 1 :tag "Next part.")
		 (const 0 :tag "Current part.")
		 integer))

;;;
;;; The treatment variables
;;;

(defvar gnus-part-display-hook nil
  "Hook called on parts that are to receive treatment.")

(defvar gnus-article-treat-custom
  '(choice (const :tag "Off" nil)
	   (const :tag "On" t)
	   (const :tag "Header" head)
	   (const :tag "First" first)
	   (const :tag "Last" last)
	   (integer :tag "Less")
	   (repeat :tag "Groups" regexp)
	   (sexp :tag "Predicate")))

(defvar gnus-article-treat-head-custom
  '(choice (const :tag "Off" nil)
	   (const :tag "Header" head)))

(defvar gnus-article-treat-types '("text/plain" "text/x-verbatim"
				   "text/x-patch")
  "Parts to treat.")

(defvar gnus-inhibit-treatment nil
  "Whether to inhibit treatment.")

(defcustom gnus-treat-highlight-signature '(or t (typep "text/x-vcard"))
  "Highlight the signature.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'gnus-treat-highlight-signature 'highlight t)

(defcustom gnus-treat-buttonize 100000
  "Add buttons.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'gnus-treat-buttonize 'highlight t)

(defcustom gnus-treat-buttonize-head 'head
  "Add buttons to the head.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-buttonize-head 'highlight t)

(defcustom gnus-treat-date 'head
  "Display dates according to the `gnus-article-date-headers' variable.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "24.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-emphasize 50000
  "Emphasize text.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'gnus-treat-emphasize 'highlight t)

(defcustom gnus-treat-strip-cr nil
  "Remove carriage returns.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-unsplit-urls nil
  "Remove newlines from within URLs.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-leading-whitespace nil
  "Remove leading whitespace in headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-hide-headers 'head
  "Hide headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-hide-boring-headers nil
  "Hide boring headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-hide-signature nil
  "Hide the signature.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fill-article nil
  "Fill the article.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-hide-citation nil
  "Hide cited text.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'.

See `gnus-article-highlight-citation' for variables used to
control what it hides."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-hide-citation-maybe nil
  "Hide cited text according to certain conditions.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'.

See `gnus-cite-hide-percentage' and `gnus-cite-hide-absolute' for
how to control what it hides."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-list-identifiers 'head
  "Strip list identifiers from `gnus-list-identifiers`.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "21.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(gnus-define-group-parameter
 list-identifier
 :variable-document
 "Alist of regexps and correspondent identifiers."
 :variable-group gnus-article-washing
 :parameter-type
 '(choice :tag "Identifier"
	  :value nil
	  (symbol :tag "Item in `gnus-list-identifiers'" none)
	  regexp
	  (const :tag "None" nil))
 :parameter-document
 "If non-nil, specify how to remove `identifiers' from articles' subject.

Any symbol is used to look up a regular expression to match the
banner in `gnus-list-identifiers'.  A string is used as a regular
expression to match the identifier directly.")

(make-obsolete-variable 'gnus-treat-strip-pgp nil
			"Gnus 5.10 (Emacs 22.1)")

(defcustom gnus-treat-strip-pem nil
  "Strip PEM signatures.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-banner t
  "Strip banners from articles.
The banner to be stripped is specified in the `banner' group parameter.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-highlight-headers 'head
  "Highlight the headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-highlight-headers 'highlight t)

(defcustom gnus-treat-highlight-citation t
  "Highlight cited text.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'gnus-treat-highlight-citation 'highlight t)

(defcustom gnus-treat-strip-headers-in-body t
  "Strip the X-No-Archive header line from the beginning of the body.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "21.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-trailing-blank-lines nil
  "Strip trailing blank lines.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'.

When set to t, it also strips trailing blanks in all MIME parts.
Consider to use `last' instead."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-leading-blank-lines nil
  "Strip leading blank lines.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'.

When set to t, it also strips trailing blanks in all MIME parts."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-multiple-blank-lines nil
  "Strip multiple blank lines.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-unfold-headers 'head
  "Unfold folded header lines.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-article-unfold-long-headers nil
  "If non-nil, allow unfolding headers even if the header is long.
If it is a regexp, only long headers matching this regexp are unfolded.
If it is t, all long headers are unfolded.

This variable has no effect if `gnus-treat-unfold-headers' is nil."
  :version "23.1" ;; No Gnus
  :group 'gnus-article-treat
  :type '(choice (const nil)
		 (const :tag "all" t)
		 (regexp)))

(defcustom gnus-treat-fold-headers nil
  "Fold headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fold-newsgroups 'head
  "Fold the Newsgroups and Followup-To headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-overstrike t
  "Treat overstrike highlighting.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'gnus-treat-overstrike 'highlight t)

(defcustom gnus-treat-ansi-sequences (if (locate-library "ansi-color") t)
  "Treat ANSI SGR control sequences.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(make-obsolete-variable 'gnus-treat-display-xface
			'gnus-treat-display-x-face "Emacs 22.1")

(defcustom gnus-treat-display-x-face
  (and (not noninteractive)
       (gnus-image-type-available-p 'xbm)
       (if (featurep 'xemacs)
	   (featurep 'xface)
	 (condition-case nil
             (and (string-match "^0x" (shell-command-to-string "uncompface"))
                  (executable-find "icontopbm"))
           ;; shell-command-to-string may signal an error, e.g. if
           ;; shell-file-name is not found.
           (error nil)))
       'head)
  "Display X-Face headers.
Valid values are nil and `head'.
See Info node `(gnus)Customizing Articles' and Info node
`(gnus)X-Face' for details."
  :group 'gnus-article-treat
  :version "21.1"
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)X-Face")
  :type gnus-article-treat-head-custom
  :set (lambda (symbol value)
	 (set-default
	  symbol
	  (cond ((or (boundp symbol) (get symbol 'saved-value))
		 value)
		((boundp 'gnus-treat-display-xface)
		 (message "\
** gnus-treat-display-xface is an obsolete variable;\
 use gnus-treat-display-x-face instead")
		 (default-value 'gnus-treat-display-xface))
		((get 'gnus-treat-display-xface 'saved-value)
		 (message "\
** gnus-treat-display-xface is an obsolete variable;\
 use gnus-treat-display-x-face instead")
		 (eval (car (get 'gnus-treat-display-xface 'saved-value))))
		(t
		 value)))))
(put 'gnus-treat-display-x-face 'highlight t)

(defcustom gnus-treat-display-face
  (and (not noninteractive)
       (gnus-image-type-available-p 'png)
       'head)
  "Display Face headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Face' for details."
  :group 'gnus-article-treat
  :version "22.1"
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)X-Face")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-display-face 'highlight t)

(defcustom gnus-treat-display-smileys (gnus-image-type-available-p 'xpm)
  "Display smileys.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Smileys' for details."
  :group 'gnus-article-treat
  :version "21.1"
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)Smileys")
  :type gnus-article-treat-custom)
(put 'gnus-treat-display-smileys 'highlight t)

(defcustom gnus-treat-from-picon
  (if (and (gnus-image-type-available-p 'xpm)
	   (gnus-picons-installed-p))
      'head nil)
  "Display picons in the From header.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Picons' for details."
  :version "22.1"
  :group 'gnus-article-treat
  :group 'gnus-picon
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)Picons")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-from-picon 'highlight t)

(defcustom gnus-treat-mail-picon
  (if (and (gnus-image-type-available-p 'xpm)
	   (gnus-picons-installed-p))
      'head nil)
  "Display picons in To and Cc headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Picons' for details."
  :version "22.1"
  :group 'gnus-article-treat
  :group 'gnus-picon
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)Picons")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-mail-picon 'highlight t)

(defcustom gnus-treat-newsgroups-picon
  (if (and (gnus-image-type-available-p 'xpm)
	   (gnus-picons-installed-p))
      'head nil)
  "Display picons in the Newsgroups and Followup-To headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Picons' for details."
  :version "22.1"
  :group 'gnus-article-treat
  :group 'gnus-picon
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)Picons")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-newsgroups-picon 'highlight t)

(defcustom gnus-treat-from-gravatar nil
  "Display gravatars in the From header.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Gravatars' for details."
  :version "24.1"
  :group 'gnus-article-treat
  :group 'gnus-gravatar
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)Gravatars")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-from-gravatar 'highlight t)

(defcustom gnus-treat-mail-gravatar nil
  "Display gravatars in To and Cc headers.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' and Info
node `(gnus)Gravatars' for details."
  :version "24.1"
  :group 'gnus-article-treat
  :group 'gnus-gravatar
  :link '(custom-manual "(gnus)Customizing Articles")
  :link '(custom-manual "(gnus)Gravatars")
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-mail-gravatar 'highlight t)

(defcustom gnus-treat-body-boundary
  (if (or gnus-treat-newsgroups-picon
	  gnus-treat-mail-picon
	  gnus-treat-from-picon
          gnus-treat-from-gravatar
          gnus-treat-mail-gravatar)
      ;; If there's much decoration, the user might prefer a boundary.
      'head
    nil)
  "Draw a boundary at the end of the headers.
Valid values are nil and `head'.
See Info node `(gnus)Customizing Articles' for details."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-capitalize-sentences nil
  "Capitalize sentence-starting words.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "21.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-wash-html nil
  "Format as HTML.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fill-long-lines '(typep "text/plain")
  "Fill long lines.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "24.1"
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defcustom gnus-treat-x-pgp-sig nil
  "Verify X-PGP-Sig.
To automatically treat X-PGP-Sig, set it to head.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "22.1"
  :group 'gnus-article-treat
  :group 'mime-security
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defvar gnus-article-encrypt-protocol-alist
  '(("PGP" . mml2015-self-encrypt)))

;; Set to nil if more than one protocol added to
;; gnus-article-encrypt-protocol-alist.
(defcustom gnus-article-encrypt-protocol "PGP"
  "The protocol used for encrypt articles.
It is a string, such as \"PGP\". If nil, ask user."
  :version "22.1"
  :type 'string
  :group 'mime-security)

(defcustom gnus-use-idna (and (condition-case nil (require 'idna) (file-error))
			      (mm-coding-system-p 'utf-8)
			      (executable-find idna-program))
  "Whether IDNA decoding of headers is used when viewing messages.
This requires GNU Libidn, and by default only enabled if it is found."
  :version "22.1"
  :group 'gnus-article-headers
  :type 'boolean)

(defcustom gnus-article-over-scroll nil
  "If non-nil, allow scrolling the article buffer even when there no more text."
  :version "22.1"
  :group 'gnus-article
  :type 'boolean)

(defcustom gnus-inhibit-images nil
  "Non-nil means inhibit displaying of images inline in the article body."
  :version "24.1"
  :group 'gnus-article
  :type 'boolean)

(defcustom gnus-blocked-images 'gnus-block-private-groups
  "Images that have URLs matching this regexp will be blocked.
This can also be a function to be evaluated.  If so, it will be
called with the group name as the parameter, and should return a
regexp."
  :version "24.1"
  :group 'gnus-art
  :type 'regexp)

;;; Internal variables

(defvar gnus-english-month-names
  '("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defvar article-goto-body-goes-to-point-min-p nil)
(defvar gnus-article-wash-types nil)
(defvar gnus-article-emphasis-alist nil)
(defvar gnus-article-image-alist nil)

(defvar gnus-article-mime-handle-alist-1 nil)
(defvar gnus-treatment-function-alist
  '((gnus-treat-strip-cr gnus-article-remove-cr)
    (gnus-treat-x-pgp-sig gnus-article-verify-x-pgp-sig)
    (gnus-treat-strip-banner gnus-article-strip-banner)
    (gnus-treat-strip-headers-in-body gnus-article-strip-headers-in-body)
    (gnus-treat-highlight-signature gnus-article-highlight-signature)
    (gnus-treat-buttonize gnus-article-add-buttons)
    (gnus-treat-fill-article gnus-article-fill-cited-article)
    (gnus-treat-fill-long-lines gnus-article-fill-cited-long-lines)
    (gnus-treat-unsplit-urls gnus-article-unsplit-urls)
    (gnus-treat-display-x-face gnus-article-display-x-face)
    (gnus-treat-display-face gnus-article-display-face)
    (gnus-treat-hide-headers gnus-article-maybe-hide-headers)
    (gnus-treat-hide-boring-headers gnus-article-hide-boring-headers)
    (gnus-treat-hide-signature gnus-article-hide-signature)
    (gnus-treat-strip-list-identifiers gnus-article-hide-list-identifiers)
    (gnus-treat-leading-whitespace gnus-article-remove-leading-whitespace)
    (gnus-treat-from-picon gnus-treat-from-picon)
    (gnus-treat-mail-picon gnus-treat-mail-picon)
    (gnus-treat-newsgroups-picon gnus-treat-newsgroups-picon)
    (gnus-treat-strip-pem gnus-article-hide-pem)
    (gnus-treat-date gnus-article-treat-date)
    (gnus-treat-from-gravatar gnus-treat-from-gravatar)
    (gnus-treat-mail-gravatar gnus-treat-mail-gravatar)
    (gnus-treat-highlight-headers gnus-article-highlight-headers)
    (gnus-treat-highlight-signature gnus-article-highlight-signature)
    (gnus-treat-strip-trailing-blank-lines
     gnus-article-remove-trailing-blank-lines)
    (gnus-treat-strip-leading-blank-lines
     gnus-article-strip-leading-blank-lines)
    (gnus-treat-strip-multiple-blank-lines
     gnus-article-strip-multiple-blank-lines)
    (gnus-treat-overstrike gnus-article-treat-overstrike)
    (gnus-treat-ansi-sequences gnus-article-treat-ansi-sequences)
    (gnus-treat-unfold-headers gnus-article-treat-unfold-headers)
    (gnus-treat-fold-newsgroups gnus-article-treat-fold-newsgroups)
    (gnus-treat-fold-headers gnus-article-treat-fold-headers)
    (gnus-treat-buttonize-head gnus-article-add-buttons-to-head)
    (gnus-treat-display-smileys gnus-treat-smiley)
    (gnus-treat-capitalize-sentences gnus-article-capitalize-sentences)
    (gnus-treat-wash-html gnus-article-wash-html)
    (gnus-treat-emphasize gnus-article-emphasize)
    (gnus-treat-hide-citation gnus-article-hide-citation)
    (gnus-treat-hide-citation-maybe gnus-article-hide-citation-maybe)
    (gnus-treat-highlight-citation gnus-article-highlight-citation)
    (gnus-treat-body-boundary gnus-article-treat-body-boundary)))

(defvar gnus-article-mime-handle-alist nil)
(defvar article-lapsed-timer nil)
(defvar gnus-article-current-summary nil)

(defvar gnus-article-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    ;; This causes the citation match run O(2^n).
    ;; (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?> ")<" table)
    (modify-syntax-entry ?< "(>" table)
    ;; make M-. in article buffers work for `foo' strings
    (modify-syntax-entry ?' " " table)
    (modify-syntax-entry ?` " " table)
    table)
  "Syntax table used in article mode buffers.
Initialized from `text-mode-syntax-table.")

(defvar gnus-save-article-buffer nil)

(defvar gnus-number-of-articles-to-be-saved nil)

(defvar gnus-inhibit-hiding nil)

(defvar gnus-article-edit-mode nil)

;;; Macros for dealing with the article buffer.

(defmacro gnus-with-article-headers (&rest forms)
  `(with-current-buffer gnus-article-buffer
     (save-restriction
       (let ((inhibit-read-only t)
	     (inhibit-point-motion-hooks t)
	     (case-fold-search t))
	 (article-narrow-to-head)
	 ,@forms))))

(put 'gnus-with-article-headers 'lisp-indent-function 0)
(put 'gnus-with-article-headers 'edebug-form-spec '(body))

(defmacro gnus-with-article-buffer (&rest forms)
  `(when (buffer-live-p (get-buffer gnus-article-buffer))
     (with-current-buffer gnus-article-buffer
       (let ((inhibit-read-only t))
         ,@forms))))

(put 'gnus-with-article-buffer 'lisp-indent-function 0)
(put 'gnus-with-article-buffer 'edebug-form-spec '(body))

(defun gnus-article-goto-header (header)
  "Go to HEADER, which is a regular expression."
  (re-search-forward (concat "^\\(" header "\\):") nil t))

(defsubst gnus-article-hide-text (b e props)
  "Set text PROPS on the B to E region, extending `intangible' 1 past B."
  (gnus-add-text-properties-when 'article-type nil b e props)
  (when (memq 'intangible props)
    (put-text-property
     (max (1- b) (point-min))
     b 'intangible (cddr (memq 'intangible props)))))

(defsubst gnus-article-unhide-text (b e)
  "Remove hidden text properties from region between B and E."
  (remove-text-properties b e gnus-hidden-properties)
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun gnus-article-hide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (gnus-add-wash-type type)
  (gnus-article-hide-text
   b e (cons 'article-type (cons type gnus-hidden-properties))))

(defun gnus-article-unhide-text-type (b e type)
  "Unhide text of TYPE between B and E."
  (gnus-delete-wash-type type)
  (remove-text-properties
   b e (cons 'article-type (cons type gnus-hidden-properties)))
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun gnus-article-hide-text-of-type (type)
  "Hide text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min))
	  (e (point-max)))
      (while (setq b (text-property-any b e 'article-type type))
	(add-text-properties b (incf b) gnus-hidden-properties)))))

(defun gnus-article-delete-text-of-type (type)
  "Delete text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min)))
      (if (eq type 'multipart)
	  ;; Remove MIME buttons associated with multipart/alternative parts.
	  (progn
	    (goto-char b)
	    (while (if (get-text-property (point) 'gnus-part)
		       (setq b (point))
		     (when (setq b (next-single-property-change (point)
								'gnus-part))
		       (goto-char b)
		       t))
	      (end-of-line)
	      (skip-chars-forward "\n")
	      (when (eq (get-text-property b 'article-type) 'multipart)
		(delete-region b (point)))))
	(while (setq b (text-property-any b (point-max) 'article-type type))
	  (delete-region
	   b (or (text-property-not-all b (point-max) 'article-type type)
		 (point-max))))))))

(defun gnus-article-delete-invisible-text ()
  "Delete all invisible text in the current buffer."
  (save-excursion
    (let ((b (point-min)))
      (while (setq b (text-property-any b (point-max) 'invisible t))
	(delete-region
	 b (or (text-property-not-all b (point-max) 'invisible t)
	       (point-max)))))))

(defun gnus-article-text-type-exists-p (type)
  "Say whether any text of type TYPE exists in the buffer."
  (text-property-any (point-min) (point-max) 'article-type type))

(defsubst gnus-article-header-rank ()
  "Give the rank of the string HEADER as given by `gnus-sorted-header-list'."
  (let ((list gnus-sorted-header-list)
	(i 1))
    (while list
      (if (looking-at (car list))
	  (setq list nil)
	(setq list (cdr list))
	(incf i)))
      i))

(defun article-hide-headers (&optional arg delete)
  "Hide unwanted headers and possibly sort them as well."
  (interactive)
  ;; This function might be inhibited.
  (unless gnus-inhibit-hiding
    (let ((inhibit-read-only t)
	  (case-fold-search t)
	  (max (1+ (length gnus-sorted-header-list)))
	  (inhibit-point-motion-hooks t)
	  (cur (current-buffer))
	  ignored visible beg)
      (save-excursion
	;; `gnus-ignored-headers' and `gnus-visible-headers' may be
	;; group parameters, so we should go to the summary buffer.
	(when (prog1
		  (condition-case nil
		      (progn (set-buffer gnus-summary-buffer) t)
		    (error nil))
		(setq ignored (when (not gnus-visible-headers)
				(cond ((stringp gnus-ignored-headers)
				       gnus-ignored-headers)
				      ((listp gnus-ignored-headers)
				       (mapconcat 'identity
						  gnus-ignored-headers
						  "\\|"))))
		      visible (cond ((stringp gnus-visible-headers)
				     gnus-visible-headers)
				    ((and gnus-visible-headers
					  (listp gnus-visible-headers))
				     (mapconcat 'identity
						gnus-visible-headers
						"\\|")))))
	  (set-buffer cur))
	(save-restriction
	  ;; First we narrow to just the headers.
	  (article-narrow-to-head)
	  ;; Hide any "From " lines at the beginning of (mail) articles.
	  (while (looking-at "From ")
	    (forward-line 1))
	  (unless (bobp)
	    (delete-region (point-min) (point)))
	  ;; Then treat the rest of the header lines.
	  ;; Then we use the two regular expressions
	  ;; `gnus-ignored-headers' and `gnus-visible-headers' to
	  ;; select which header lines is to remain visible in the
	  ;; article buffer.
	  (while (re-search-forward "^[^ \t:]*:" nil t)
	    (beginning-of-line)
	    ;; Mark the rank of the header.
	    (put-text-property
	     (point) (1+ (point)) 'message-rank
	     (if (or (and visible (looking-at visible))
		     (and ignored
			  (not (looking-at ignored))))
		 (gnus-article-header-rank)
	       (+ 2 max)))
	    (forward-line 1))
	  (message-sort-headers-1)
	  (when (setq beg (text-property-any
			   (point-min) (point-max) 'message-rank (+ 2 max)))
	    ;; We delete the unwanted headers.
	    (gnus-add-wash-type 'headers)
	    (add-text-properties (point-min) (+ 5 (point-min))
				 '(article-type headers dummy-invisible t))
	    (delete-region beg (point-max))))))))

(defun article-hide-boring-headers (&optional arg)
  "Toggle hiding of headers that aren't very interesting.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (when (and (not (gnus-article-check-hidden-text 'boring-headers arg))
	     (not gnus-show-all-headers))
    (save-excursion
      (save-restriction
	(let ((inhibit-read-only t)
	      (inhibit-point-motion-hooks t))
	  (article-narrow-to-head)
	  (dolist (elem gnus-boring-article-headers)
	    (goto-char (point-min))
	    (cond
	     ;; Hide empty headers.
	     ((eq elem 'empty)
	      (while (re-search-forward "^[^: \t]+:[ \t]*\n[^ \t]" nil t)
		(forward-line -1)
		(gnus-article-hide-text-type
		 (point-at-bol)
		 (progn
		   (end-of-line)
		   (if (re-search-forward "^[^ \t]" nil t)
		       (match-beginning 0)
		     (point-max)))
		 'boring-headers)))
	     ;; Hide boring Newsgroups header.
	     ((eq elem 'newsgroups)
	      (when (gnus-string-equal
		     (gnus-fetch-field "newsgroups")
		     (gnus-group-real-name
		      (if (boundp 'gnus-newsgroup-name)
			  gnus-newsgroup-name
			"")))
		(gnus-article-hide-header "newsgroups")))
	     ((eq elem 'to-address)
	      (let ((to (message-fetch-field "to"))
		    (to-address
		     (gnus-parameter-to-address
		      (if (boundp 'gnus-newsgroup-name)
			  gnus-newsgroup-name ""))))
		(when (and to to-address
			   (ignore-errors
			     (gnus-string-equal
			      ;; only one address in To
			      (nth 1 (mail-extract-address-components to))
			      to-address)))
		  (gnus-article-hide-header "to"))))
	     ((eq elem 'to-list)
	      (let ((to (message-fetch-field "to"))
		    (to-list
		     (gnus-parameter-to-list
		      (if (boundp 'gnus-newsgroup-name)
			  gnus-newsgroup-name ""))))
		(when (and to to-list
			   (ignore-errors
			     (gnus-string-equal
			      ;; only one address in To
			      (nth 1 (mail-extract-address-components to))
			      to-list)))
		  (gnus-article-hide-header "to"))))
	     ((eq elem 'cc-list)
	      (let ((cc (message-fetch-field "cc"))
		    (to-list
		     (gnus-parameter-to-list
		      (if (boundp 'gnus-newsgroup-name)
			  gnus-newsgroup-name ""))))
		(when (and cc to-list
			   (ignore-errors
			     (gnus-string-equal
			      ;; only one address in CC
			      (nth 1 (mail-extract-address-components cc))
			      to-list)))
		  (gnus-article-hide-header "cc"))))
	     ((eq elem 'followup-to)
	      (when (gnus-string-equal
		     (message-fetch-field "followup-to")
		     (message-fetch-field "newsgroups"))
		(gnus-article-hide-header "followup-to")))
	     ((eq elem 'reply-to)
	      (if (gnus-group-find-parameter
		   gnus-newsgroup-name 'broken-reply-to)
		  (gnus-article-hide-header "reply-to")
		(let ((from (message-fetch-field "from"))
		      (reply-to (message-fetch-field "reply-to")))
		  (when
		      (and
		       from reply-to
		       (ignore-errors
			 (equal
			  (sort (mapcar
				 (lambda (x) (downcase (cadr x)))
				 (mail-extract-address-components from t))
				'string<)
			  (sort (mapcar
				 (lambda (x) (downcase (cadr x)))
				 (mail-extract-address-components reply-to t))
				'string<))))
		    (gnus-article-hide-header "reply-to")))))
	     ((eq elem 'date)
	      (let ((date (with-current-buffer gnus-original-article-buffer
			    ;; If date in `gnus-article-buffer' is localized
			    ;; (`gnus-treat-date-user-defined'),
			    ;; `days-between' might fail.
			    (message-fetch-field "date"))))
		(when (and date
			   (< (days-between (current-time-string) date)
			      4))
		  (gnus-article-hide-header "date"))))
	     ((eq elem 'long-to)
	      (let ((to (message-fetch-field "to"))
		    (cc (message-fetch-field "cc")))
		(when (> (length to) 1024)
		  (gnus-article-hide-header "to"))
		(when (> (length cc) 1024)
		  (gnus-article-hide-header "cc"))))
	     ((eq elem 'many-to)
	      (let ((to-count 0)
		    (cc-count 0))
		(goto-char (point-min))
		(while (re-search-forward "^to:" nil t)
		  (setq to-count (1+ to-count)))
		(when (> to-count 1)
		  (while (> to-count 0)
		    (goto-char (point-min))
		    (save-restriction
		      (re-search-forward "^to:" nil nil to-count)
		      (forward-line -1)
		      (narrow-to-region (point) (point-max))
		      (gnus-article-hide-header "to"))
		    (setq to-count (1- to-count))))
		(goto-char (point-min))
		(while (re-search-forward "^cc:" nil t)
		  (setq cc-count (1+ cc-count)))
		(when (> cc-count 1)
		  (while (> cc-count 0)
		    (goto-char (point-min))
		    (save-restriction
		      (re-search-forward "^cc:" nil nil cc-count)
		      (forward-line -1)
		      (narrow-to-region (point) (point-max))
		      (gnus-article-hide-header "cc"))
		    (setq cc-count (1- cc-count)))))))))))))

(defun gnus-article-hide-header (header)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" header ":") nil t)
      (gnus-article-hide-text-type
       (point-at-bol)
       (progn
	 (end-of-line)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (match-beginning 0)
	   (point-max)))
       'boring-headers))))

(defvar gnus-article-normalized-header-length 40
  "Length of normalized headers.")

(defun article-normalize-headers ()
  "Make all header lines 40 characters long."
  (interactive)
  (let ((inhibit-read-only t)
	column)
    (save-excursion
      (save-restriction
	(article-narrow-to-head)
	(while (not (eobp))
	  (cond
	   ((< (setq column (- (point-at-eol) (point)))
	       gnus-article-normalized-header-length)
	    (end-of-line)
	    (insert (make-string
		     (- gnus-article-normalized-header-length column)
		     ? )))
	   ((> column gnus-article-normalized-header-length)
	    (gnus-put-text-property
	     (progn
	       (forward-char gnus-article-normalized-header-length)
	       (point))
	     (point-at-eol)
	     'invisible t))
	   (t
	    ;; Do nothing.
	    ))
	  (forward-line 1))))))

(defun article-treat-dumbquotes ()
  "Translate M****s*** sm*rtq**t*s and other symbols into proper text.
Note that this function guesses whether a character is a sm*rtq**t* or
not, so it should only be used interactively.

Sm*rtq**t*s are M****s***'s unilateral extension to the
iso-8859-1 character map in an attempt to provide more quoting
characters.  If you see something like \\222 or \\264 where
you're expecting some kind of apostrophe or quotation mark, then
try this wash."
  (interactive)
  (article-translate-strings gnus-article-dumbquotes-map))

(defvar org-entities)

(defun article-treat-non-ascii ()
  "Translate many Unicode characters into their ASCII equivalents."
  (interactive)
  (require 'org-entities)
  (let ((table (make-char-table (if (featurep 'xemacs) 'generic))))
    (dolist (elem org-entities)
      (when (and (listp elem)
		 (= (length (nth 6 elem)) 1))
	(if (featurep 'xemacs)
	    (put-char-table (aref (nth 6 elem) 0) (nth 4 elem) table)
	  (set-char-table-range table (aref (nth 6 elem) 0) (nth 4 elem)))))
    (save-excursion
      (when (article-goto-body)
	(let ((inhibit-read-only t)
	      replace props)
	  (while (not (eobp))
	    (if (not (setq replace (if (featurep 'xemacs)
				       (get-char-table (following-char) table)
				     (aref table (following-char)))))
		(forward-char 1)
	      (if (prog1
		      (setq props (text-properties-at (point)))
		    (delete-char 1))
		  (add-text-properties (point) (progn (insert replace) (point))
				       props)
		(insert replace)))))))))

(defun article-translate-characters (from to)
  "Translate all characters in the body of the article according to FROM and TO.
FROM is a string of characters to translate from; to is a string of
characters to translate to."
  (save-excursion
    (when (article-goto-body)
      (let ((inhibit-read-only t)
	    (x (make-string 225 ?x))
	    (i -1))
	(while (< (incf i) (length x))
	  (aset x i i))
	(setq i 0)
	(while (< i (length from))
	  (aset x (aref from i) (aref to i))
	  (incf i))
	(translate-region (point) (point-max) x)))))

(defun article-translate-strings (map)
  "Translate all string in the body of the article according to MAP.
MAP is an alist where the elements are on the form (\"from\" \"to\")."
  (save-excursion
    (when (article-goto-body)
      (let ((inhibit-read-only t))
	(dolist (elem map)
	  (let ((from (car elem))
		(to (cadr elem)))
	    (save-excursion
	      (if (stringp from)
		  (while (search-forward from nil t)
		    (replace-match to))
		(while (not (eobp))
		  (if (eq (following-char) from)
		      (progn
			(delete-char 1)
			(insert to))
		    (forward-char 1)))))))))))

(defun article-treat-overstrike ()
  "Translate overstrikes into bold text."
  (interactive)
  (save-excursion
    (when (article-goto-body)
      (let ((inhibit-read-only t))
	(while (search-forward "\b" nil t)
	  (let ((next (char-after))
		(previous (char-after (- (point) 2))))
	    ;; We do the boldification/underlining by hiding the
	    ;; overstrikes and putting the proper text property
	    ;; on the letters.
	    (cond
	     ((eq next previous)
	      (gnus-article-hide-text-type (- (point) 2) (point) 'overstrike)
	      (put-text-property (point) (1+ (point)) 'face 'bold))
	     ((eq next ?_)
	      (gnus-article-hide-text-type
	       (1- (point)) (1+ (point)) 'overstrike)
	      (put-text-property
	       (- (point) 2) (1- (point)) 'face 'underline))
	     ((eq previous ?_)
	      (gnus-article-hide-text-type (- (point) 2) (point) 'overstrike)
	      (put-text-property
	       (point) (1+ (point)) 'face 'underline)))))))))

(defun article-treat-ansi-sequences ()
  "Translate ANSI SGR control sequences into overlays or extents."
  (interactive)
  (save-excursion
    (when (article-goto-body)
      (let ((inhibit-read-only t))
	(ansi-color-apply-on-region (point) (point-max))))))

(defun gnus-article-treat-unfold-headers ()
  "Unfold folded message headers.
Only the headers that fit into the current window width will be
unfolded."
  (interactive)
  (gnus-with-article-headers
    (let (length)
      (while (not (eobp))
	(save-restriction
	  (mail-header-narrow-to-field)
	  (let* ((header (buffer-string))
		 (unfoldable
		  (or (equal gnus-article-unfold-long-headers t)
		      (and (stringp gnus-article-unfold-long-headers)
			   (string-match gnus-article-unfold-long-headers header)))))
	    (with-temp-buffer
	      (insert header)
	      (goto-char (point-min))
	      (while (re-search-forward "\n[\t ]" nil t)
		(replace-match " " t t)))
	    (setq length (- (point-max) (point-min) 1))
	    (when (or unfoldable
		      (< length (window-width)))
	      (while (re-search-forward "\n[\t ]" nil t)
		(replace-match " " t t))))
	  (goto-char (point-max)))))))

(defun gnus-article-treat-fold-headers ()
  "Fold message headers."
  (interactive)
  (gnus-with-article-headers
    (while (not (eobp))
      (save-restriction
	(mail-header-narrow-to-field)
	(mail-header-fold-field)
	(goto-char (point-max))))))

(defun gnus-treat-smiley ()
  "Toggle display of textual emoticons (\"smileys\") as small graphical icons."
  (interactive)
  (gnus-with-article-buffer
    (if (memq 'smiley gnus-article-wash-types)
	(gnus-delete-images 'smiley)
      (article-goto-body)
      (let ((images (smiley-region (point) (point-max))))
	(when images
	  (gnus-add-wash-type 'smiley)
	  (dolist (image images)
	    (gnus-add-image 'smiley image)))))))

(defun gnus-article-remove-images ()
  "Remove all images from the article buffer."
  (interactive)
  (gnus-with-article-buffer
    (save-restriction
      (widen)
      (dolist (elem gnus-article-image-alist)
	(gnus-delete-images (car elem))))))

(autoload 'w3m-toggle-inline-images "w3m")

(defun gnus-article-show-images ()
  "Show any images that are in the HTML-rendered article buffer.
This only works if the article in question is HTML."
  (interactive)
  (gnus-with-article-buffer
    (save-restriction
      (widen)
      (if (eq mm-text-html-renderer 'w3m)
	  (let ((mm-inline-text-html-with-images nil))
	    (w3m-toggle-inline-images))
	(dolist (region (gnus-find-text-property-region (point-min) (point-max)
							'image-displayer))
	  (destructuring-bind (start end function) region
	    (funcall function (get-text-property start 'image-url)
		     start end)))))))

(defun gnus-article-treat-fold-newsgroups ()
  "Unfold folded message headers.
Only the headers that fit into the current window width will be
unfolded."
  (interactive)
  (gnus-with-article-headers
    (while (gnus-article-goto-header "newsgroups\\|followup-to")
      (save-restriction
	(mail-header-narrow-to-field)
	(while (re-search-forward ", *" nil t)
	  (replace-match ", " t t))
	(mail-header-fold-field)
	(goto-char (point-max))))))

(defcustom gnus-article-truncate-lines (default-value 'truncate-lines)
  "Value of `truncate-lines' in Gnus Article buffer.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles'."
  :version "23.1" ;; No Gnus
  :group 'gnus-article
  ;; :link '(custom-manual "(gnus)Customizing Articles")
  :type 'boolean)

(defun gnus-article-toggle-truncate-lines (&optional arg)
  "Toggle whether to fold or truncate long lines in article the buffer.
If ARG is non-nil and not a number, toggle
`gnus-article-truncate-lines' too.  If ARG is a number, truncate
long lines if and only if arg is positive."
  (interactive "P")
  (cond
   ((and (numberp arg) (> arg 0))
    (setq gnus-article-truncate-lines t))
   ((numberp arg)
    (setq gnus-article-truncate-lines nil))
   (arg
    (setq gnus-article-truncate-lines
	  (not gnus-article-truncate-lines))))
  (gnus-with-article-buffer
    (cond
     ((and (numberp arg) (> arg 0))
      (setq truncate-lines nil))
     ((numberp arg)
      (setq truncate-lines t)))
    ;; In versions of Emacs 22 (CVS) before 2006-05-26,
    ;; `toggle-truncate-lines' needs an argument.
    (toggle-truncate-lines)))

(defun gnus-article-treat-body-boundary ()
  "Place a boundary line at the end of the headers."
  (interactive)
  (when (and gnus-body-boundary-delimiter
	     (> (length gnus-body-boundary-delimiter) 0))
    (gnus-with-article-headers
      (goto-char (point-max))
      (let ((start (point)))
	(insert "X-Boundary: ")
	(gnus-add-text-properties start (point) '(invisible t intangible t))
       (insert (let (str (max (window-width)))
                 (if (featurep 'xemacs)
                     (setq max (1- max)))
                 (while (>= max (length str))
		    (setq str (concat str gnus-body-boundary-delimiter)))
                 (substring str 0 max))
		"\n")
	(gnus-put-text-property start (point) 'gnus-decoration 'header)))))

(defun article-fill-long-lines ()
  "Fill lines that are wider than the window width."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (width (window-width (get-buffer-window (current-buffer)))))
      (save-restriction
	(article-goto-body)
	(let ((adaptive-fill-mode nil)) ;Why?  -sm
	  (while (not (eobp))
	    (end-of-line)
	    (when (>= (current-column) (min fill-column width))
	      (narrow-to-region (min (1+ (point)) (point-max))
				(point-at-bol))
              (let ((goback (point-marker)))
                (fill-paragraph nil)
                (goto-char (marker-position goback)))
	      (widen))
	    (forward-line 1)))))))

(defun article-capitalize-sentences ()
  "Capitalize the first word in each sentence."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (paragraph-start "^[\n\^L]"))
      (article-goto-body)
      (while (not (eobp))
	(capitalize-word 1)
	(forward-sentence)))))

(defun article-remove-cr ()
  "Remove trailing CRs and then translate remaining CRs into LFs."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" t t)))))

(defun article-remove-trailing-blank-lines ()
  "Remove all trailing blank lines from the article."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (delete-region
       (point)
       (progn
	 (while (and (not (bobp))
		     (looking-at "^[ \t]*$")
		     (not (gnus-annotation-in-region-p
			   (point) (point-at-eol))))
	   (forward-line -1))
	 (forward-line 1)
	 (point))))))

(defvar gnus-face-properties-alist)

(defun article-display-face (&optional force)
  "Display any Face headers in the header."
  (interactive (list 'force))
  (let ((wash-face-p buffer-read-only))
    (gnus-with-article-headers
      ;; When displaying parts, this function can be called several times on
      ;; the same article, without any intended toggle semantic (as typing `W
      ;; D d' would have). So face deletion must occur only when we come from
      ;; an interactive command, that is when the *Article* buffer is
      ;; read-only.
      (if (and wash-face-p (memq 'face gnus-article-wash-types))
	  (gnus-delete-images 'face)
	(let ((from (message-fetch-field "from"))
	      face faces)
	  (save-current-buffer
	    (when (and wash-face-p
		       (gnus-buffer-live-p gnus-original-article-buffer)
		       (not (re-search-forward "^Face:[\t ]*" nil t)))
	      (set-buffer gnus-original-article-buffer))
	    (save-restriction
	      (mail-narrow-to-head)
	      (when (or force
			;; Check whether this face is censored.
			(not (and gnus-article-x-face-too-ugly
				  (or from
				      (setq from (message-fetch-field "from")))
				  (string-match gnus-article-x-face-too-ugly
						from))))
		(while (gnus-article-goto-header "Face")
		  (push (mail-header-field-value) faces)))))
	  (when faces
	    (goto-char (point-min))
	    (let (png image)
	      (unless (setq from (gnus-article-goto-header "from"))
		(insert "From:")
		(setq from (point))
		(insert " [no `from' set]\n"))
	      (while faces
		(when (setq png (gnus-convert-face-to-png (pop faces)))
		  (setq image
			(apply 'gnus-create-image png 'png t
			       (cdr (assq 'png gnus-face-properties-alist))))
		  (goto-char from)
		  (gnus-add-wash-type 'face)
		  (gnus-add-image 'face image)
		  (gnus-put-image image nil 'face))))))))))

(defun article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (let ((wash-face-p buffer-read-only))	;; When type `W f'
    (gnus-with-article-headers
      ;; Delete the old process, if any.
      (when (process-status "article-x-face")
	(delete-process "article-x-face"))
      ;; See the comment in `article-display-face'.
      (if (and wash-face-p (memq 'xface gnus-article-wash-types))
	  ;; We have already displayed X-Faces, so we remove them
	  ;; instead.
	  (gnus-delete-images 'xface)
	;; Display X-Faces.
	(let ((from (message-fetch-field "from"))
	      x-faces face)
	  (save-current-buffer
	    (when (and wash-face-p
		       (gnus-buffer-live-p gnus-original-article-buffer)
		       (not (re-search-forward "^X-Face:[\t ]*" nil t)))
	      ;; If type `W f', use gnus-original-article-buffer,
	      ;; otherwise use the current buffer because displaying
	      ;; RFC822 parts calls this function too.
	      (set-buffer gnus-original-article-buffer))
	    (save-restriction
	      (mail-narrow-to-head)
	      (and gnus-article-x-face-command
		   (or force
		       ;; Check whether this face is censored.
		       (not (and gnus-article-x-face-too-ugly
				 (or from
				     (setq from (message-fetch-field "from")))
				 (string-match gnus-article-x-face-too-ugly
					       from))))
		   (while (gnus-article-goto-header "X-Face")
		     (push (mail-header-field-value) x-faces)))))
	  (when x-faces
	    ;; We display the face.
	    (cond ((functionp gnus-article-x-face-command)
		   ;; The command is a lisp function, so we call it.
		   (mapc gnus-article-x-face-command x-faces))
		  ((stringp gnus-article-x-face-command)
		   ;; The command is a string, so we interpret the command
		   ;; as a, well, command, and fork it off.
		   (let ((process-connection-type nil))
		     (gnus-set-process-query-on-exit-flag
		      (start-process
		       "article-x-face" nil shell-file-name
		       shell-command-switch gnus-article-x-face-command)
		      nil)
		     ;; Sending multiple EOFs to xv doesn't work,
		     ;; so we only do a single external face.
		     (with-temp-buffer
		       (insert (car x-faces))
		       (process-send-region "article-x-face"
					    (point-min) (point-max)))
		     (process-send-eof "article-x-face")))
		  (t
		   (error "`%s' set to `%s' is not a function"
			  gnus-article-x-face-command
			  'gnus-article-x-face-command)))))))))

(defun article-decode-mime-words ()
  "Decode all MIME-encoded words in the article."
  (interactive)
  (gnus-with-article-buffer
    (let ((inhibit-point-motion-hooks t)
	  (mail-parse-charset gnus-newsgroup-charset)
	  (mail-parse-ignored-charsets
	   (with-current-buffer gnus-summary-buffer
	     gnus-newsgroup-ignored-charsets)))
      (mail-decode-encoded-word-region (point-min) (point-max)))))

(defun article-decode-charset (&optional prompt)
  "Decode charset-encoded text in the article.
If PROMPT (the prefix), prompt for a coding system to use."
  (interactive "P")
  (let ((inhibit-point-motion-hooks t) (case-fold-search t)
	(inhibit-read-only t)
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-excursion (condition-case nil
			     (set-buffer gnus-summary-buffer)
			   (error))
			 gnus-newsgroup-ignored-charsets))
	ct cte ctl charset format)
    (save-excursion
      (save-restriction
	(article-narrow-to-head)
	(setq ct (message-fetch-field "Content-Type" t)
	      cte (message-fetch-field "Content-Transfer-Encoding" t)
	      ctl (and ct (mail-header-parse-content-type ct))
	      charset (cond
		       (prompt
			(mm-read-coding-system "Charset to decode: "))
		       (ctl
			(mail-content-type-get ctl 'charset)))
	      format (and ctl (mail-content-type-get ctl 'format)))
	(when cte
	  (setq cte (mail-header-strip cte)))
	(if (and ctl (not (string-match "/" (car ctl))))
	    (setq ctl nil))
	(goto-char (point-max)))
      (forward-line 1)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(when (and (eq mail-parse-charset 'gnus-decoded)
		   (eq (mm-body-7-or-8) '8bit))
	  ;; The text code could have been decoded.
	  (setq charset mail-parse-charset))
	(when (and (or (not ctl)
		       (equal (car ctl) "text/plain"))
		   (not format)) ;; article with format will decode later.
	  (mm-decode-body
	   charset (and cte (intern (downcase
				     (gnus-strip-whitespace cte))))
	   (car ctl)))))))

(defun article-decode-encoded-words ()
  "Remove encoded-word encoding from headers."
  (let ((inhibit-point-motion-hooks t)
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-excursion (condition-case nil
			     (set-buffer gnus-summary-buffer)
			   (error))
			 gnus-newsgroup-ignored-charsets))
	(inhibit-read-only t)
	end start)
    (goto-char (point-min))
    (when (search-forward "\n\n" nil 'move)
      (forward-line -1))
    (setq end (point))
    (while (not (bobp))
      (while (progn
	       (forward-line -1)
	       (and (not (bobp))
		    (memq (char-after) '(?\t ? )))))
      (setq start (point))
      (if (looking-at "\
\\(?:Resent-\\)?\\(?:From\\|Cc\\|To\\|Bcc\\|\\(?:In-\\)?Reply-To\\|Sender\
\\|Mail-Followup-To\\|Mail-Copies-To\\|Approved\\):")
	  (funcall gnus-decode-address-function start end)
	(funcall gnus-decode-header-function start end))
      (goto-char (setq end start)))))

(defun article-decode-group-name ()
  "Decode group names in Newsgroups, Followup-To and Xref headers."
  (let ((inhibit-point-motion-hooks t)
	(inhibit-read-only t)
	(method (gnus-find-method-for-group gnus-newsgroup-name))
	regexp)
    (when (and (or gnus-group-name-charset-method-alist
		   gnus-group-name-charset-group-alist)
	       (gnus-buffer-live-p gnus-original-article-buffer))
      (save-restriction
	(article-narrow-to-head)
	(dolist (header '("Newsgroups" "Followup-To" "Xref"))
	  (with-current-buffer gnus-original-article-buffer
	    (goto-char (point-min)))
	  (setq regexp (concat "^" header
			       ":\\([^\n]*\\(?:\n[\t ]+[^\n]+\\)*\\)\n"))
	  (while (re-search-forward regexp nil t)
	    (replace-match (save-match-data
			     (gnus-decode-newsgroups
			      ;; XXX how to use data in article buffer?
			      (with-current-buffer gnus-original-article-buffer
				(re-search-forward regexp nil t)
				(match-string 1))
			      gnus-newsgroup-name method))
			   t t nil 1))
	  (goto-char (point-min)))))))

(autoload 'idna-to-unicode "idna")

(defun article-decode-idna-rhs ()
  "Decode IDNA strings in RHS in various headers in current buffer.
The following headers are decoded: From:, To:, Cc:, Reply-To:,
Mail-Reply-To: and Mail-Followup-To:."
  (when gnus-use-idna
    (save-restriction
      (let ((inhibit-point-motion-hooks t)
	    (inhibit-read-only t))
	(article-narrow-to-head)
	(goto-char (point-min))
	(while (re-search-forward "@[^ \t\n\r,>]*\\(xn--[-A-Za-z0-9.]*\\)[ \t\n\r,>]" nil t)
	  (let (ace unicode)
	    (when (save-match-data
		    (and (setq ace (match-string 1))
			 (save-excursion
			   (and (re-search-backward "^[^ \t]" nil t)
				(looking-at "From\\|To\\|Cc\\|Reply-To\\|Mail-Reply-To\\|Mail-Followup-To")))
			 (setq unicode (idna-to-unicode ace))))
	      (unless (string= ace unicode)
		(replace-match unicode nil nil nil 1)))))))))

(defun article-de-quoted-unreadable (&optional force read-charset)
  "Translate a quoted-printable-encoded article.
If FORCE, decode the article whether it is marked as quoted-printable
or not.
If READ-CHARSET, ask for a coding system."
  (interactive (list 'force current-prefix-arg))
  (save-excursion
    (let ((inhibit-read-only t) type charset)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (with-current-buffer gnus-original-article-buffer
	    (setq type
		  (gnus-fetch-field "content-transfer-encoding"))
	    (let* ((ct (gnus-fetch-field "content-type"))
		   (ctl (and ct (mail-header-parse-content-type ct))))
	      (setq charset (and ctl
				 (mail-content-type-get ctl 'charset)))
	      (if (stringp charset)
		  (setq charset (intern (downcase charset)))))))
      (if read-charset
	  (setq charset (mm-read-coding-system "Charset: " charset)))
      (unless charset
	(setq charset gnus-newsgroup-charset))
      (when (or force
		(and type (let ((case-fold-search t))
			    (string-match "quoted-printable" type))))
	(article-goto-body)
	(quoted-printable-decode-region
	 (point) (point-max) (mm-charset-to-coding-system charset))))))

(defun article-de-base64-unreadable (&optional force read-charset)
  "Translate a base64 article.
If FORCE, decode the article whether it is marked as base64 not.
If READ-CHARSET, ask for a coding system."
  (interactive (list 'force current-prefix-arg))
  (save-excursion
    (let ((inhibit-read-only t) type charset)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (with-current-buffer gnus-original-article-buffer
	    (setq type
		  (gnus-fetch-field "content-transfer-encoding"))
	    (let* ((ct (gnus-fetch-field "content-type"))
		   (ctl (and ct (mail-header-parse-content-type ct))))
	      (setq charset (and ctl
				 (mail-content-type-get ctl 'charset)))
	      (if (stringp charset)
		  (setq charset (intern (downcase charset)))))))
      (if read-charset
	  (setq charset (mm-read-coding-system "Charset: " charset)))
      (unless charset
	(setq charset gnus-newsgroup-charset))
      (when (or force
		(and type (let ((case-fold-search t))
			    (string-match "base64" type))))
	(article-goto-body)
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (base64-decode-region (point-min) (point-max))
	  (mm-decode-coding-region
	   (point-min) (point-max) (mm-charset-to-coding-system charset)))))))

(eval-when-compile
  (require 'rfc1843))

(defun article-decode-HZ ()
  "Translate a HZ-encoded article."
  (interactive)
  (require 'rfc1843)
  (save-excursion
    (let ((inhibit-read-only t))
      (rfc1843-decode-region (point-min) (point-max)))))

(defun article-unsplit-urls ()
  "Remove the newlines that some other mailers insert into URLs."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(\\(https?\\|ftp\\)://\\S-+\\) *\n\\(\\S-+\\)" nil t)
	(replace-match "\\1\\3" t)))
    (when (interactive-p)
      (gnus-treat-article nil))))

(defun article-wash-html ()
  "Format an HTML article."
  (interactive)
  (let ((handles nil)
	(buffer-read-only nil))
    (when (gnus-buffer-live-p gnus-original-article-buffer)
      (with-current-buffer gnus-original-article-buffer
	(setq handles (mm-dissect-buffer t t))))
    (article-goto-body)
    (delete-region (point) (point-max))
    (mm-enable-multibyte)
    (mm-inline-text-html handles)))

(defvar gnus-article-browse-html-temp-list nil
  "List of temporary files created by `gnus-article-browse-html-parts'.
Internal variable.")

(defcustom gnus-article-browse-delete-temp 'ask
  "What to do with temporary files from `gnus-article-browse-html-parts'.
If nil, don't delete temporary files.  If it is t, delete them on
exit from the summary buffer.  If it is the symbol `file', query
on each file, if it is `ask' ask once when exiting from the
summary buffer."
  :group 'gnus-article
  :version "23.1" ;; No Gnus
  :type '(choice (const :tag "Don't delete" nil)
		 (const :tag "Don't ask" t)
		 (const :tag "Ask" ask)
		 (const :tag "Ask for each file" file)))

;; Cf. mm-postponed-undisplay-list / mm-destroy-postponed-undisplay-list.

(defun gnus-article-browse-delete-temp-files (&optional how)
  "Delete temp-files created by `gnus-article-browse-html-parts'."
  (when (and gnus-article-browse-html-temp-list
	     (progn
	       (or how (setq how gnus-article-browse-delete-temp))
	       (if (eq how 'ask)
		   (let ((files (length gnus-article-browse-html-temp-list)))
		     (gnus-y-or-n-p
		      (if (= files 1)
			  "Delete the temporary HTML file? "
			(format "Delete all %s temporary HTML files? "
				files))))
		 how)))
    (dolist (file gnus-article-browse-html-temp-list)
      (cond ((file-directory-p file)
	     (when (or (not (eq how 'file))
		       (gnus-y-or-n-p
			(format
			 "Delete temporary HTML file(s) in directory `%s'? "
			 (file-name-as-directory file))))
	       (gnus-delete-directory file)))
	    ((file-exists-p file)
	     (when (or (not (eq how 'file))
		       (gnus-y-or-n-p
			(format "Delete temporary HTML file `%s'? " file)))
	       (delete-file file)))))
    ;; Also remove file from the list when not deleted or if file doesn't
    ;; exist anymore.
    (setq gnus-article-browse-html-temp-list nil))
  gnus-article-browse-html-temp-list)

(defun gnus-article-browse-html-save-cid-content (cid handles directory)
  "Find CID content in HANDLES and save it in a file in DIRECTORY.
Return file name."
  (save-match-data
    (let (file type)
      (catch 'found
	(dolist (handle handles)
	  (cond
	   ((not (listp handle)))
	   ((equal (mm-handle-media-supertype handle) "multipart")
	    (when (setq file (gnus-article-browse-html-save-cid-content
			      cid handle directory))
	      (throw 'found file)))
	   ((equal (concat "<" cid ">") (mm-handle-id handle))
	    (setq file
		  (expand-file-name
                   (or (mm-handle-filename handle)
                       (concat
                        (make-temp-name "cid")
                        (car (rassoc (car (mm-handle-type handle)) mailcap-mime-extensions))))
                   directory))
	    (mm-save-part-to-file handle file)
	    (throw 'found file))))))))

(defun gnus-article-browse-html-parts (list &optional header)
  "View all \"text/html\" parts from LIST.
Recurse into multiparts.  The optional HEADER that should be a decoded
message header will be added to the bodies of the \"text/html\" parts."
  ;; Internal function used by `gnus-article-browse-html-article'.
  (let (type file charset content cid-dir tmp-file showed)
    ;; Find and show the html-parts.
    (dolist (handle list)
      ;; If HTML, show it:
      (cond ((not (listp handle)))
	    ((or (equal (car (setq type (mm-handle-type handle))) "text/html")
		 (and (equal (car type) "message/external-body")
		      (or header
			  (setq file (mm-handle-filename handle)))
		      (or (mm-handle-cache handle)
			  (condition-case code
			      (progn (mm-extern-cache-contents handle) t)
			    (error
			     (gnus-message 3 "%s" (error-message-string code))
			     (when (>= gnus-verbose 3) (sit-for 2))
			     nil)))
		      (progn
			(setq handle (mm-handle-cache handle)
			      type (mm-handle-type handle))
			(equal (car type) "text/html"))))
	     (setq charset (mail-content-type-get type 'charset)
		   content (mm-get-part handle))
	     (with-temp-buffer
	       (if (eq charset 'gnus-decoded)
		   (mm-enable-multibyte)
		 (mm-disable-multibyte))
	       (insert content)
	       ;; resolve cid contents
	       (let ((case-fold-search t)
		     cid-file)
		 (goto-char (point-min))
		 (while (re-search-forward "\
<img[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*src=\"\\(cid:\\([^\"]+\\)\\)\""
					   nil t)
		   (unless cid-dir
		     (setq cid-dir (mm-make-temp-file "cid" t))
		     (add-to-list 'gnus-article-browse-html-temp-list cid-dir))
		   (setq file nil
			 content nil)
		   (when (setq cid-file
			       (gnus-article-browse-html-save-cid-content
				(match-string 2)
				(with-current-buffer gnus-article-buffer
				  gnus-article-mime-handles)
				cid-dir))
		     (when (eq system-type 'cygwin)
		       (setq cid-file
			     (concat "/" (substring
					  (with-output-to-string
					    (call-process "cygpath" nil
							  standard-output
							  nil "-m" cid-file))
					  0 -1))))
		     (replace-match (concat "file://" cid-file)
				    nil nil nil 1))))
	       (unless content (setq content (buffer-string))))
	     (when (or charset header (not file))
	       (setq tmp-file (mm-make-temp-file
			       ;; Do we need to care for 8.3 filenames?
			       "mm-" nil ".html")))
	     ;; Add a meta html tag to specify charset and a header.
	     (cond
	      (header
	       (let (title eheader body hcharset coding force-charset)
		 (with-temp-buffer
		   (mm-enable-multibyte)
		   (setq case-fold-search t)
		   (insert header "\n")
		   (setq title (message-fetch-field "subject"))
		   (goto-char (point-min))
		   (while (re-search-forward "\\(<\\)\\|\\(>\\)\\|&" nil t)
		     (replace-match (cond ((match-beginning 1) "&lt;")
					  ((match-beginning 2) "&gt;")
					  (t "&amp;"))))
		   (goto-char (point-min))
		   (insert "<pre>\n")
		   (goto-char (point-max))
		   (insert "</pre>\n<hr>\n")
		   ;; We have to examine charset one by one since
		   ;; charset specified in parts might be different.
		   (if (eq charset 'gnus-decoded)
		       (setq charset 'utf-8
			     eheader (mm-encode-coding-string (buffer-string)
							      charset)
			     title (when title
				     (mm-encode-coding-string title charset))
			     body (mm-encode-coding-string content charset)
			     force-charset t)
		     (setq hcharset (mm-find-mime-charset-region (point-min)
								 (point-max)))
		     (cond ((= (length hcharset) 1)
			    (setq hcharset (car hcharset)
				  coding (mm-charset-to-coding-system
					  hcharset)))
			   ((> (length hcharset) 1)
			    (setq hcharset 'utf-8
				  coding hcharset)))
		     (if coding
			 (if charset
			     (progn
			       (setq body
				     (mm-charset-to-coding-system charset))
			       (if (eq coding body)
				   (setq eheader (mm-encode-coding-string
						  (buffer-string) coding)
					 title (when title
						 (mm-encode-coding-string
						  title coding))
					 body content)
				 (setq charset 'utf-8
				       eheader (mm-encode-coding-string
						(buffer-string) charset)
				       title (when title
					       (mm-encode-coding-string
						title charset))
				       body (mm-encode-coding-string
					     (mm-decode-coding-string
					      content body)
					     charset)
				       force-charset t)))
			   (setq charset hcharset
				 eheader (mm-encode-coding-string
					  (buffer-string) coding)
				 title (when title
					 (mm-encode-coding-string
					  title coding))
				 body content))
		       (setq eheader (mm-string-as-unibyte (buffer-string))
			     body content)))
		   (erase-buffer)
		   (mm-disable-multibyte)
		   (insert body)
		   (when charset
		     (mm-add-meta-html-tag handle charset force-charset))
		   (when title
		     (goto-char (point-min))
		     (unless (search-forward "<title>" nil t)
		       (re-search-forward "<head>\\s-*" nil t)
		       (insert "<title>" title "</title>\n")))
		   (goto-char (point-min))
		   (or (re-search-forward
			"<body\\(?:\\s-+[^>]+\\|\\s-*\\)>\\s-*" nil t)
		       (re-search-forward
			"</head\\(?:\\s-+[^>]+\\|\\s-*\\)>\\s-*" nil t))
		   (insert eheader)
		   (mm-write-region (point-min) (point-max)
				    tmp-file nil nil nil 'binary t))))
	      (charset
	       (mm-with-unibyte-buffer
		 (insert (if (eq charset 'gnus-decoded)
			     (mm-encode-coding-string content
						      (setq charset 'utf-8))
			   content))
		 (if (or (mm-add-meta-html-tag handle charset)
			 (not file))
		     (mm-write-region (point-min) (point-max)
				      tmp-file nil nil nil 'binary t)
		   (setq tmp-file nil))))
	      (tmp-file
	       (mm-save-part-to-file handle tmp-file)))
	     (when tmp-file
	       (add-to-list 'gnus-article-browse-html-temp-list tmp-file))
	     (add-hook 'gnus-summary-prepare-exit-hook
		       'gnus-article-browse-delete-temp-files)
	     (add-hook 'gnus-exit-gnus-hook
		       (lambda  ()
			 (gnus-article-browse-delete-temp-files t)))
	     ;; FIXME: Warn if there's an <img> tag?
	     (browse-url-of-file (or tmp-file (expand-file-name file)))
	     (setq showed t))
	    ;; If multipart, recurse
	    ((equal (mm-handle-media-supertype handle) "multipart")
	     (when (gnus-article-browse-html-parts handle header)
	       (setq showed t)))
	    ((equal (mm-handle-media-type handle) "message/rfc822")
	     (mm-with-multibyte-buffer
	       (mm-insert-part handle)
	       (setq handle (mm-dissect-buffer t t))
	       (when (and (bufferp (car handle))
			  (stringp (car (mm-handle-type handle))))
		 (setq handle (list handle)))
	       (when header
		 (article-decode-encoded-words)
		 (let ((gnus-visible-headers
			(or (get 'gnus-visible-headers 'standard-value)
			    gnus-visible-headers)))
		   (article-hide-headers))
		 (goto-char (point-min))
		 (search-forward "\n\n" nil 'move)
		 (skip-chars-backward "\t\n ")
		 (setq header (buffer-substring (point-min) (point)))))
	     (when (prog1
		       (gnus-article-browse-html-parts handle header)
		     (mm-destroy-parts handle))
	       (setq showed t)))))
    showed))

(defun gnus-article-browse-html-article (&optional arg)
  "View \"text/html\" parts of the current article with a WWW browser.
Inline images embedded in a message using the cid scheme, as they are
generally considered to be safe, will be processed properly.
The message header is added to the beginning of every html part unless
the prefix argument ARG is given.

Warning: Spammers use links to images (using the http scheme) in HTML
articles to verify whether you have read the message.  As
`gnus-article-browse-html-article' passes the HTML content to the
browser without eliminating these \"web bugs\" you should only
use it for mails from trusted senders.

If you always want to display HTML parts in the browser, set
`mm-text-html-renderer' to nil.

This command creates temporary files to pass HTML contents including
images if any to the browser, and deletes them when exiting the group
\(if you want)."
  ;; Cf. `mm-w3m-safe-url-regexp'
  (interactive "P")
  (if arg
      (gnus-summary-show-article)
    (let ((gnus-visible-headers (or (get 'gnus-visible-headers 'standard-value)
				    gnus-visible-headers))
	  ;; As we insert a <hr>, there's no need for the body boundary.
	  (gnus-treat-body-boundary nil))
      (gnus-summary-show-article)))
  (with-current-buffer gnus-article-buffer
    (let ((header (unless arg
		    (save-restriction
		      (widen)
		      (buffer-substring-no-properties
		       (goto-char (point-min))
		       (if (search-forward "\n\n" nil t)
			   (match-beginning 0)
			 (goto-char (point-max))
			 (skip-chars-backward "\t\n ")
			 (point))))))
	  parts)
      (set-buffer gnus-original-article-buffer)
      (setq parts (mm-dissect-buffer t t))
      ;; If singlepart, enforce a list.
      (when (and (bufferp (car parts))
		 (stringp (car (mm-handle-type parts))))
	(setq parts (list parts)))
      ;; Process the list
      (unless (gnus-article-browse-html-parts parts header)
	(gnus-error 3 "Mail doesn't contain a \"text/html\" part!"))
      (mm-destroy-parts parts)
      (unless arg
	(gnus-summary-show-article)))))

(defun article-hide-list-identifiers ()
  "Remove list identifies from the Subject header.
The `gnus-list-identifiers' variable specifies what to do."
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (regexp (gnus-group-get-list-identifiers gnus-newsgroup-name))
        (inhibit-read-only t))
    (when regexp
      (save-excursion
	(save-restriction
	  (article-narrow-to-head)
	  (goto-char (point-min))
	  (while (re-search-forward
		  (concat "^Subject: +\\(R[Ee]: +\\)*\\(" regexp " *\\)")
		  nil t)
	    (delete-region (match-beginning 2) (match-end 0))
	    (beginning-of-line))
	  (when (re-search-forward
		 "^Subject: +\\(\\(R[Ee]: +\\)+\\)R[Ee]: +" nil t)
	    (delete-region (match-beginning 1) (match-end 1))))))))

(defun article-hide-pem (&optional arg)
  "Toggle hiding of any PEM headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'pem arg)
    (save-excursion
      (let ((inhibit-read-only t) end)
	(goto-char (point-min))
	;; Hide the horrendously ugly "header".
	(when (and (search-forward
		    "\n-----BEGIN PRIVACY-ENHANCED MESSAGE-----\n"
		    nil t)
		   (setq end (1+ (match-beginning 0))))
	  (gnus-add-wash-type 'pem)
	  (gnus-article-hide-text-type
	   end
	   (if (search-forward "\n\n" nil t)
	       (match-end 0)
	     (point-max))
	   'pem)
	  ;; Hide the trailer as well
	  (when (search-forward "\n-----END PRIVACY-ENHANCED MESSAGE-----\n"
				nil t)
	    (gnus-article-hide-text-type
	     (match-beginning 0) (match-end 0) 'pem)))))))

(defun article-strip-banner ()
  "Strip the banners specified by the `banner' group parameter and by
`gnus-article-address-banner-alist'."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((inhibit-point-motion-hooks t))
	(when (gnus-parameter-banner gnus-newsgroup-name)
	  (article-really-strip-banner
	   (gnus-parameter-banner gnus-newsgroup-name)))
	(when gnus-article-address-banner-alist
	  ;; Note that the From header is decoded here, so it is
	  ;; required that the *-extract-address-components function
	  ;; supports non-ASCII text.
	  (let ((from (save-restriction
			(widen)
			(article-narrow-to-head)
			(mail-fetch-field "from"))))
	    (when (and from
		       (setq from
			     (cadr (funcall gnus-extract-address-components
					    from))))
	      (catch 'found
		(dolist (pair gnus-article-address-banner-alist)
		  (when (string-match (car pair) from)
		    (throw 'found
			   (article-really-strip-banner (cdr pair)))))))))))))

(defun article-really-strip-banner (banner)
  "Strip the banner specified by the argument."
  (save-excursion
    (save-restriction
      (let ((inhibit-point-motion-hooks t)
	    (gnus-signature-limit nil)
	    (inhibit-read-only t))
	(article-goto-body)
	(cond
	 ((eq banner 'signature)
	  (when (gnus-article-narrow-to-signature)
	    (widen)
	    (forward-line -1)
	    (delete-region (point) (point-max))))
	 ((symbolp banner)
	  (if (setq banner (cdr (assq banner gnus-article-banner-alist)))
	      (while (re-search-forward banner nil t)
		(delete-region (match-beginning 0) (match-end 0)))))
	 ((stringp banner)
	  (while (re-search-forward banner nil t)
	    (delete-region (match-beginning 0) (match-end 0)))))))))

(defun article-babel ()
  "Translate article using an online translation service."
  (interactive)
  (require 'babel)
  (gnus-with-article-buffer
    (when (article-goto-body)
      (let* ((start (point))
	     (end (point-max))
	     (orig (buffer-substring start end))
	     (trans (babel-as-string orig)))
	(save-restriction
	  (narrow-to-region start end)
	  (delete-region start end)
	  (insert trans))))))

(defun article-hide-signature (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'signature arg)
    (save-excursion
      (save-restriction
	(let ((inhibit-read-only t))
	  (when (gnus-article-narrow-to-signature)
	    (gnus-article-hide-text-type
	     (point-min) (point-max) 'signature))))))
  (gnus-set-mode-line 'article))

(defun article-strip-headers-in-body ()
  "Strip offensive headers from bodies."
  (interactive)
  (save-excursion
    (article-goto-body)
    (let ((case-fold-search t))
      (when (looking-at "x-no-archive:")
	(gnus-delete-line)))))

(defun article-strip-leading-blank-lines ()
  "Remove all blank lines from the beginning of the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  (inhibit-read-only t))
      (when (article-goto-body)
	(while (and (not (eobp))
		    (looking-at "[ \t]*$"))
	  (gnus-delete-line))))))

(defun article-narrow-to-head ()
  "Narrow the buffer to the head of the message.
Point is left at the beginning of the narrowed-to region."
  (narrow-to-region
   (goto-char (point-min))
   (cond
    ;; Absolutely no headers displayed.
    ((looking-at "\n")
     (point))
    ;; Normal headers.
    ((search-forward "\n\n" nil 1)
     (1- (point)))
    ;; Nothing but headers.
    (t
     (point-max))))
  (goto-char (point-min)))

(defun article-goto-body ()
  "Place point at the start of the body."
  (goto-char (point-min))
  (cond
   ;; This variable is only bound when dealing with separate
   ;; MIME body parts.
   (article-goto-body-goes-to-point-min-p
    t)
   ((search-forward "\n\n" nil t)
    t)
   (t
    (goto-char (point-max))
    nil)))

(defun article-strip-multiple-blank-lines ()
  "Replace consecutive blank lines with one empty line."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  (inhibit-read-only t))
      ;; First make all blank lines empty.
      (article-goto-body)
      (while (re-search-forward "^[ \t]+$" nil t)
	(unless (gnus-annotation-in-region-p
		 (match-beginning 0) (match-end 0))
	  (replace-match "" nil t)))
      ;; Then replace multiple empty lines with a single empty line.
      (article-goto-body)
      (while (re-search-forward "\n\n\\(\n+\\)" nil t)
	(unless (gnus-annotation-in-region-p
		 (match-beginning 0) (match-end 0))
	  (delete-region (match-beginning 1) (match-end 1)))))))

(defun article-strip-leading-space ()
  "Remove all white space from the beginning of the lines in the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  (inhibit-read-only t))
      (article-goto-body)
      (while (re-search-forward "^[ \t]+" nil t)
	(replace-match "" t t)))))

(defun article-strip-trailing-space ()
  "Remove all white space from the end of the lines in the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  (inhibit-read-only t))
      (article-goto-body)
      (while (re-search-forward "[ \t]+$" nil t)
	(replace-match "" t t)))))

(defun article-strip-blank-lines ()
  "Strip leading, trailing and multiple blank lines."
  (interactive)
  (article-strip-leading-blank-lines)
  (article-remove-trailing-blank-lines)
  (article-strip-multiple-blank-lines))

(defun article-strip-all-blank-lines ()
  "Strip all blank lines."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  (inhibit-read-only t))
      (article-goto-body)
      (while (re-search-forward "^[ \t]*\n" nil t)
	(replace-match "" t t)))))

(defun gnus-article-narrow-to-signature ()
  "Narrow to the signature; return t if a signature is found, else nil."
  (let ((inhibit-point-motion-hooks t))
    (when (gnus-article-search-signature)
      (forward-line 1)
      ;; Check whether we have some limits to what we consider
      ;; to be a signature.
      (let ((limits (if (listp gnus-signature-limit) gnus-signature-limit
		      (list gnus-signature-limit)))
	    limit limited)
	(while (setq limit (pop limits))
	  (if (or (and (integerp limit)
		       (< (- (point-max) (point)) limit))
		  (and (floatp limit)
		       (< (count-lines (point) (point-max)) limit))
		  (and (functionp limit)
		       (funcall limit))
		  (and (stringp limit)
		       (not (re-search-forward limit nil t))))
	      ()			; This limit did not succeed.
	    (setq limited t
		  limits nil)))
	(unless limited
	  (narrow-to-region (point) (point-max))
	  t)))))

(defun gnus-article-search-signature ()
  "Search the current buffer for the signature separator.
Put point at the beginning of the signature separator."
  (let ((cur (point)))
    (goto-char (point-max))
    (if (if (stringp gnus-signature-separator)
	    (re-search-backward gnus-signature-separator nil t)
	  (let ((seps gnus-signature-separator))
	    (while (and seps
			(not (re-search-backward (car seps) nil t)))
	      (pop seps))
	    seps))
	t
      (goto-char cur)
      nil)))

(defun gnus-article-hidden-arg ()
  "Return the current prefix arg as a number, or 0 if no prefix."
  (list (if current-prefix-arg
	    (prefix-numeric-value current-prefix-arg)
	  0)))

(defun gnus-article-check-hidden-text (type arg)
  "Return nil if hiding is necessary.
Arg can be nil or a number.  nil and positive means hide, negative
means show, 0 means toggle."
  (save-excursion
    (save-restriction
      (let ((hide (gnus-article-hidden-text-p type)))
	(cond
	 ((or (null arg)
	      (> arg 0))
	  nil)
	 ((< arg 0)
	  (gnus-article-show-hidden-text type)
	  t)
	 (t
	  (if (eq hide 'hidden)
	      (progn
		(gnus-article-show-hidden-text type)
		t)
	    nil)))))))

(defun gnus-article-hidden-text-p (type)
  "Say whether the current buffer contains hidden text of type TYPE."
  (let ((pos (text-property-any (point-min) (point-max) 'article-type type)))
    (while (and pos
		(not (get-text-property pos 'invisible))
		(not (get-text-property pos 'dummy-invisible)))
      (setq pos
	    (text-property-any (1+ pos) (point-max) 'article-type type)))
    (if pos
	'hidden
      nil)))

(defun gnus-article-show-hidden-text (type &optional dummy)
  "Show all hidden text of type TYPE.
Originally it is hide instead of DUMMY."
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (gnus-remove-text-properties-when
     'article-type type
     (point-min) (point-max)
     (cons 'article-type (cons type
			       gnus-hidden-properties)))
    (gnus-delete-wash-type type)))

(defconst article-time-units
  `((year . ,(* 365.25 24 60 60))
    (week . ,(* 7 24 60 60))
    (day . ,(* 24 60 60))
    (hour . ,(* 60 60))
    (minute . 60)
    (second . 1))
  "Mapping from time units to seconds.")

(defun gnus-article-forward-header ()
  "Move point to the start of the next header.
If the current header is a continuation header, this can be several
lines forward."
  (let ((ended nil))
    (while (not ended)
      (forward-line 1)
      (if (looking-at "[ \t]+[^ \t]")
	  (forward-line 1)
	(setq ended t)))))

(defun article-treat-date ()
  (article-date-ut (if (gnus-buffer-live-p gnus-summary-buffer)
		       (with-current-buffer gnus-summary-buffer
			 gnus-article-date-headers)
		     gnus-article-date-headers)
		   t))

(defun article-date-ut (&optional type highlight date-position)
  "Convert DATE date to TYPE in the current article.
The default type is `ut'.  See `gnus-article-date-headers' for
possible values."
  (interactive (list 'ut t))
  (let* ((case-fold-search t)
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (first t)
	 (visible-date (mail-fetch-field "Date"))
	 pos date bface eface)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Date:" nil t)
	(setq bface (get-text-property (point-at-bol) 'face)
	      eface (get-text-property (1- (point-at-eol)) 'face)))
      ;; Delete any old Date headers.
      (if date-position
	  (progn
	    (goto-char date-position)
	    (setq date (get-text-property (point) 'original-date))
	    (delete-region (point)
			   (progn
			     (gnus-article-forward-header)
			     (point)))
	    (article-transform-date date type bface eface))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (or (get-text-property (setq pos (point)) 'original-date)
		     (and (setq pos (next-single-property-change
				     (point) 'original-date))
			  (goto-char pos)))
	    (narrow-to-region pos (if (search-forward "\n\n" nil t)
				      (1+ (match-beginning 0))
				    (point-max)))
	    (goto-char (point-min))
	    (while (re-search-forward "^Date:" nil t)
	      (setq date (get-text-property (match-beginning 0) 'original-date))
	      (delete-region (point-at-bol) (progn
					      (gnus-article-forward-header)
					      (point))))
	    (when (and (not date)
		       visible-date)
	      (setq date visible-date))
	    (when date
	      (article-transform-date date type bface eface))
	    (goto-char (point-max))
	    (widen)))))))

(defun article-transform-date (date type bface eface)
  (dolist (this-type (cond
		      ((null type)
		       (list 'ut))
		      ((atom type)
		       (list type))
		      (t
		       type)))
    (insert (article-make-date-line date (or this-type 'ut)) "\n")
    (forward-line -1)
    (beginning-of-line)
    (put-text-property (point) (1+ (point))
		       'original-date date)
    (put-text-property (point) (1+ (point))
		       'gnus-date-type this-type)
    ;; Do highlighting.
    (when (looking-at "\\([^:]+\\): *\\(.*\\)$")
      (put-text-property (match-beginning 1) (1+ (match-end 1))
			 'face bface)
      (put-text-property (match-beginning 2) (match-end 2)
			 'face eface))
    (forward-line 1)))

(defun article-make-date-line (date type)
  "Return a DATE line of TYPE."
  (unless (memq type '(local ut original user-defined iso8601 lapsed english
			     combined-lapsed))
    (error "Unknown conversion type: %s" type))
  (condition-case ()
      (let ((time (ignore-errors (date-to-time date))))
	(cond
	 ;; Convert to the local timezone.
	 ((eq type 'local)
	  (concat "Date: " (message-make-date time)))
	 ;; Convert to Universal Time.
	 ((eq type 'ut)
	  (concat "Date: "
		  (substring
		   (message-make-date
		    (let* ((e (parse-time-string date))
			   (tm (apply 'encode-time e))
			   (ms (car tm))
			   (ls (- (cadr tm) (car (current-time-zone time)))))
		      (cond ((< ls 0) (list (1- ms) (+ ls 65536)))
			    ((> ls 65535) (list (1+ ms) (- ls 65536)))
			    (t (list ms ls)))))
		   0 -5)
		  "UT"))
	 ;; Get the original date from the article.
	 ((eq type 'original)
	  (concat "Date: " (if (string-match "\n+$" date)
			       (substring date 0 (match-beginning 0))
			     date)))
	 ;; Let the user define the format.
	 ((eq type 'user-defined)
	  (let ((format (or (condition-case nil
				(with-current-buffer gnus-summary-buffer
				  gnus-article-time-format)
			      (error nil))
			    gnus-article-time-format)))
	    (if (functionp format)
		(funcall format time)
	      (concat "Date: " (format-time-string format time)))))
	 ;; ISO 8601.
	 ((eq type 'iso8601)
	  (let ((tz (car (current-time-zone time))))
	    (concat
	     "Date: "
	     (format-time-string "%Y%m%dT%H%M%S" time)
	     (format "%s%02d%02d"
		     (if (> tz 0) "+" "-") (/ (abs tz) 3600)
		     (/ (% (abs tz) 3600) 60)))))
	 ;; Do a lapsed format.
	 ((eq type 'lapsed)
	  (concat "Date: " (article-lapsed-string time)))
	 ;; A combined date/lapsed format.
	 ((eq type 'combined-lapsed)
	  (let ((date-string (article-make-date-line date 'original))
		(segments 3)
		lapsed-string)
	    (while (and
                    time
		    (setq lapsed-string
			  (concat " (" (article-lapsed-string time segments) ")"))
		    (> (+ (length date-string)
			  (length lapsed-string))
		       (+ fill-column 6))
		    (> segments 0))
	      (setq segments (1- segments)))
	    (if (> segments 0)
		(concat date-string lapsed-string)
	      date-string)))
	 ;; Display the date in proper English
	 ((eq type 'english)
	  (let ((dtime (decode-time time)))
	    (concat
	     "Date: the "
	     (number-to-string (nth 3 dtime))
	     (let ((digit (% (nth 3 dtime) 10)))
	       (cond
		((memq (nth 3 dtime) '(11 12 13)) "th")
		((= digit 1) "st")
		((= digit 2) "nd")
		((= digit 3) "rd")
		(t "th")))
	     " of "
	     (nth (1- (nth 4 dtime)) gnus-english-month-names)
	     " "
	     (number-to-string (nth 5 dtime))
	     " at "
	     (format "%02d" (nth 2 dtime))
	     ":"
	     (format "%02d" (nth 1 dtime)))))))
    (foo
     (format "Date: %s (from Gnus)" date))))

(defun article-lapsed-string (time &optional max-segments)
  ;; If the date is seriously mangled, the timezone functions are
  ;; liable to bug out, so we ignore all errors.
  (let* ((now (current-time))
	 (real-time (subtract-time now time))
	 (real-sec (and real-time
			(+ (* (float (car real-time)) 65536)
			   (cadr real-time))))
	 (sec (and real-time (abs real-sec)))
	 (segments 0)
	 num prev)
    (unless max-segments
      (setq max-segments (length article-time-units)))
    (cond
     ((null real-time)
      "Unknown")
     ((zerop sec)
      "Now")
     (t
      (concat
       ;; This is a bit convoluted, but basically we go
       ;; through the time units for years, weeks, etc,
       ;; and divide things to see whether that results
       ;; in positive answers.
       (mapconcat
	(lambda (unit)
	  (if (or (zerop (setq num (ffloor (/ sec (cdr unit)))))
		  (>= segments max-segments))
	      ;; The (remaining) seconds are too few to
	      ;; be divided into this time unit.
	      ""
	    ;; It's big enough, so we output it.
	    (setq sec (- sec (* num (cdr unit))))
	    (prog1
		(concat (if prev ", " "") (int-to-string
					   (floor num))
			" " (symbol-name (car unit))
			(if (> num 1) "s" ""))
	      (setq prev t
		    segments (1+ segments)))))
	article-time-units "")
       ;; If dates are odd, then it might appear like the
       ;; article was sent in the future.
       (if (> real-sec 0)
	   " ago"
	 " in the future"))))))

(defun article-date-local (&optional highlight)
  "Convert the current article date to the local timezone."
  (interactive (list t))
  (article-date-ut 'local highlight))

(defun article-date-english (&optional highlight)
  "Convert the current article date to something that is proper English."
  (interactive (list t))
  (article-date-ut 'english highlight))

(defun article-date-original (&optional highlight)
  "Convert the current article date to what it was originally.
This is only useful if you have used some other date conversion
function and want to see what the date was before converting."
  (interactive (list t))
  (article-date-ut 'original highlight))

(defun article-date-lapsed (&optional highlight)
  "Convert the current article date to time lapsed since it was sent."
  (interactive (list t))
  (article-date-ut 'lapsed highlight))

(defun article-date-combined-lapsed (&optional highlight)
  "Convert the current article date to time lapsed since it was sent."
  (interactive (list t))
  (article-date-ut 'combined-lapsed highlight))

(defun article-update-date-lapsed ()
  "Function to be run from a timer to update the lapsed time line."
  (save-match-data
    (let ((buffer (current-buffer)))
      (ignore-errors
	(walk-windows
	 (lambda (w)
	   (set-buffer (window-buffer w))
	   (when (eq major-mode 'gnus-article-mode)
	     (let ((old-line (count-lines (point-min) (point)))
		   (old-column (- (point) (line-beginning-position)))
		   (window-start
		    (window-start (get-buffer-window (current-buffer)))))
	       (goto-char (point-min))
	       (while (re-search-forward "^Date:" nil t)
		 (let ((type (get-text-property (match-beginning 0)
						'gnus-date-type)))
		   (when (memq type '(lapsed combined-lapsed user-format))
		     (when (and window-start
				(not (= window-start
					(save-excursion
					  (forward-line 1)
					  (point)))))
		       (setq window-start nil))
		     (save-excursion
		       (article-date-ut type t (match-beginning 0)))
		     (forward-line 1)
		     (when window-start
		       (set-window-start (get-buffer-window (current-buffer))
					 (point))))))
	       (goto-char (point-min))
	       (when (> old-column 0)
		 (setq old-line (1- old-line)))
	       (forward-line old-line)
	       (end-of-line)
	       (when (> (current-column) old-column)
		 (beginning-of-line)
		 (forward-char old-column)))))
	 nil 'visible))
      (set-buffer buffer))))

(defun gnus-start-date-timer (&optional n)
  "Start a timer to update the Date headers in the article buffers.
The numerical prefix says how frequently (in seconds) the function
is to run."
  (interactive "p")
  (unless n
    (setq n 1))
  (gnus-stop-date-timer)
  (setq article-lapsed-timer
	(run-at-time 1 n 'article-update-date-lapsed)))

(defun gnus-stop-date-timer ()
  "Stop the Date timer."
  (interactive)
  (when article-lapsed-timer
    (nnheader-cancel-timer article-lapsed-timer)
    (setq article-lapsed-timer nil)))

(defun article-date-user (&optional highlight)
  "Convert the current article date to the user-defined format.
This format is defined by the `gnus-article-time-format' variable."
  (interactive (list t))
  (article-date-ut 'user highlight))

(defun article-date-iso8601 (&optional highlight)
  "Convert the current article date to ISO8601."
  (interactive (list t))
  (article-date-ut 'iso8601 highlight))

(defmacro gnus-article-save-original-date (&rest forms)
  "Save the original date as a text property and evaluate FORMS."
  `(let* ((case-fold-search t)
	  (start (progn
		   (goto-char (point-min))
		   (when (and (re-search-forward "^date:[\t\n ]+" nil t)
			      (not (bolp)))
		     (match-end 0))))
	  (date (when (and start
			   (re-search-forward "[\t ]*\n\\(?:[^\t ]\\|\\'\\)"
					      nil t))
		  (buffer-substring-no-properties start
						  (match-beginning 0)))))
     (goto-char (point-max))
     (skip-chars-backward "\n")
     (put-text-property (point-min) (point) 'original-date date)
     ,@forms
     (goto-char (point-max))
     (skip-chars-backward "\n")
     (put-text-property (point-min) (point) 'original-date date)))

;; (defun article-show-all ()
;;   "Show all hidden text in the article buffer."
;;   (interactive)
;;   (save-excursion
;;     (let ((inhibit-read-only t))
;;       (gnus-article-unhide-text (point-min) (point-max)))))

(defun article-remove-leading-whitespace ()
  "Remove excessive whitespace from all headers."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((inhibit-read-only t))
	(article-narrow-to-head)
	(goto-char (point-min))
	(while (re-search-forward "^[^ :]+: \\([ \t]+\\)" nil t)
	  (delete-region (match-beginning 1) (match-end 1)))))))

(defun article-emphasize (&optional arg)
  "Emphasize text according to `gnus-emphasis-alist'."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'emphasis arg)
    (save-excursion
      (let ((alist (or
		    (condition-case nil
			(with-current-buffer gnus-summary-buffer
			  gnus-article-emphasis-alist)
		      (error))
		    gnus-emphasis-alist))
	    (inhibit-read-only t)
	    (props (append '(article-type emphasis)
			   gnus-hidden-properties))
	    regexp elem beg invisible visible face)
	(article-goto-body)
	(setq beg (point))
	(while (setq elem (pop alist))
	  (goto-char beg)
	  (setq regexp (car elem)
		invisible (nth 1 elem)
		visible (nth 2 elem)
		face (nth 3 elem))
	  (while (re-search-forward regexp nil t)
	    (when (and (match-beginning visible) (match-beginning invisible))
	      (gnus-article-hide-text
	       (match-beginning invisible) (match-end invisible) props)
	      (gnus-article-unhide-text-type
	       (match-beginning visible) (match-end visible) 'emphasis)
	      (gnus-put-overlay-excluding-newlines
	       (match-beginning visible) (match-end visible) 'face face)
	      (gnus-add-wash-type 'emphasis)
	      (goto-char (match-end invisible)))))))))

(defun gnus-article-setup-highlight-words (&optional highlight-words)
  "Setup newsgroup emphasis alist."
  (unless gnus-article-emphasis-alist
    (let ((name (and gnus-newsgroup-name
		     (gnus-group-real-name gnus-newsgroup-name))))
      (make-local-variable 'gnus-article-emphasis-alist)
      (setq gnus-article-emphasis-alist
	    (nconc
	     (let ((alist gnus-group-highlight-words-alist) elem highlight)
	       (while (setq elem (pop alist))
		 (when (and name (string-match (car elem) name))
		   (setq alist nil
			 highlight (copy-sequence (cdr elem)))))
	       highlight)
	     (copy-sequence highlight-words)
	     (if gnus-newsgroup-name
		 (copy-sequence (gnus-group-find-parameter
				 gnus-newsgroup-name 'highlight-words t)))
	     gnus-emphasis-alist)))))

(defvar gnus-summary-article-menu)
(defvar gnus-summary-post-menu)

;;; Saving functions.

(defun gnus-article-save (save-buffer file &optional num)
  "Save the currently selected article."
  (when (or (get gnus-default-article-saver :headers)
	    (not gnus-save-all-headers))
    ;; Remove headers according to `gnus-saved-headers' or the value
    ;; of the `:headers' property that the saver function might have.
    (let ((gnus-visible-headers
	   (or (symbol-value (get gnus-default-article-saver :headers))
	       gnus-saved-headers gnus-visible-headers))
	  ;; Ignore group parameter.  See `article-hide-headers'.
	  (gnus-summary-buffer nil))
      (with-current-buffer save-buffer
	(article-hide-headers 1 t))))
  (save-window-excursion
    (if (not gnus-default-article-saver)
	(error "No default saver is defined")
      ;; !!! Magic!  The saving functions all save
      ;; `gnus-save-article-buffer' (or so they think), but we
      ;; bind that variable to our save-buffer.
      (set-buffer gnus-article-buffer)
      (let* ((gnus-save-article-buffer save-buffer)
	     (filename
	      (cond
	       ((not gnus-prompt-before-saving) 'default)
	       ((eq gnus-prompt-before-saving 'always) nil)
	       (t file)))
	     (gnus-number-of-articles-to-be-saved
	      (when (eq gnus-prompt-before-saving t)
		num)))			; Magic
	(set-buffer gnus-article-current-summary)
	(funcall gnus-default-article-saver filename)))))

(defun gnus-read-save-file-name (prompt &optional filename
					function group headers variable
					dir-var)
  (let ((default-name
	  (funcall function group headers (symbol-value variable)))
	result)
    (setq result
	  (expand-file-name
	   (cond
	    ((eq filename 'default)
	     default-name)
	    ((eq filename t)
	     default-name)
	    (filename filename)
	    (t
	     (when (symbol-value dir-var)
	       (setq default-name (expand-file-name
				   (file-name-nondirectory default-name)
				   (symbol-value dir-var))))
	     (let* ((split-name (gnus-get-split-value gnus-split-methods))
		    (prompt
		     (format prompt
			     (if (and gnus-number-of-articles-to-be-saved
				      (> gnus-number-of-articles-to-be-saved 1))
				 (format "these %d articles"
					 gnus-number-of-articles-to-be-saved)
			       "this article")))
		    (file
		     ;; Let the split methods have their say.
		     (cond
		      ;; No split name was found.
		      ((null split-name)
		       (read-file-name
			(concat prompt " (default "
				(file-name-nondirectory default-name) "): ")
			(file-name-directory default-name)
			default-name))
		      ;; A single group name is returned.
		      ((stringp split-name)
		       (setq default-name
			     (funcall function split-name headers
				      (symbol-value variable)))
		       (read-file-name
			(concat prompt " (default "
				(file-name-nondirectory default-name) "): ")
			(file-name-directory default-name)
			default-name))
		      ;; A single split name was found
		      ((= 1 (length split-name))
		       (let* ((name (expand-file-name
				     (car split-name)
				     gnus-article-save-directory))
			      (dir (cond ((file-directory-p name)
					  (file-name-as-directory name))
					 ((file-exists-p name) name)
					 (t gnus-article-save-directory))))
			 (read-file-name
			  (concat prompt " (default " name "): ")
			  dir name)))
		      ;; A list of splits was found.
		      (t
		       (setq split-name (nreverse split-name))
		       (let (result)
			 (let ((file-name-history
				(nconc split-name file-name-history)))
			   (setq result
				 (expand-file-name
				  (read-file-name
				   (concat prompt " (`M-p' for defaults): ")
				   gnus-article-save-directory
				   (car split-name))
				  gnus-article-save-directory)))
			 (car (push result file-name-history)))))))
	       ;; Create the directory.
	       (gnus-make-directory (file-name-directory file))
	       ;; If we have read a directory, we append the default file name.
	       (when (file-directory-p file)
		 (setq file (expand-file-name (file-name-nondirectory
					       default-name)
					      (file-name-as-directory file))))
	       ;; Possibly translate some characters.
	       (nnheader-translate-file-chars file))))))
    (gnus-make-directory (file-name-directory result))
    (when variable
      (set variable result))
    (when dir-var
      (set dir-var (file-name-directory result)))
    result))

(defun gnus-article-archive-name (group)
  "Return the first instance of an \"Archive-name\" in the current buffer."
  (let ((case-fold-search t))
    (when (re-search-forward "archive-name: *\\([^ \n\t]+\\)[ \t]*$" nil t)
      (nnheader-concat gnus-article-save-directory
		       (match-string 1)))))

(defun gnus-article-nndoc-name (group)
  "If GROUP is an nndoc group, return the name of the parent group."
  (when (eq (car (gnus-find-method-for-group group)) 'nndoc)
    (gnus-group-get-parameter group 'save-article-group)))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in rmail file" filename
		  gnus-rmail-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-rmail))
  (with-current-buffer gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	;; Note that unlike gnus-summary-save-in-mail, there is no
	;; check to see if filename is Babyl.  Rmail in Emacs 23 does
	;; not use Babyl.
	(gnus-output-to-rmail filename))))
  filename)

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in Unix mail file" filename
		  gnus-mail-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-mail))
  (with-current-buffer gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(if (and (file-readable-p filename)
		 (file-regular-p filename)
		 (mail-file-babyl-p filename))
	    (gnus-output-to-rmail filename)
	  (gnus-output-to-mail filename)))))
  filename)

(put 'gnus-summary-save-in-file :decode t)
(put 'gnus-summary-save-in-file :headers 'gnus-saved-headers)
(defun gnus-summary-save-in-file (&optional filename overwrite)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in file" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-file))
  (with-current-buffer gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(when (and overwrite
		   (file-exists-p filename))
	  (delete-file filename))
	(gnus-output-to-file filename))))
  filename)

(put 'gnus-summary-write-to-file :decode t)
(put 'gnus-summary-write-to-file :function 'gnus-summary-save-in-file)
(put 'gnus-summary-write-to-file :headers 'gnus-saved-headers)
(defun gnus-summary-write-to-file (&optional filename)
  "Write this article to a file, overwriting it if the file exists.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in file" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers nil 'gnus-newsgroup-last-directory))
  (gnus-summary-save-in-file filename t))

(put 'gnus-summary-save-body-in-file :decode t)
(defun gnus-summary-save-body-in-file (&optional filename overwrite)
  "Append this article body to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s body in file" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-file))
  (with-current-buffer gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(when (article-goto-body)
	  (narrow-to-region (point) (point-max)))
	(when (and overwrite
		   (file-exists-p filename))
	  (delete-file filename))
	(gnus-output-to-file filename))))
  filename)

(put 'gnus-summary-write-body-to-file :decode t)
(put 'gnus-summary-write-body-to-file
     :function 'gnus-summary-save-body-in-file)
(defun gnus-summary-write-body-to-file (&optional filename)
  "Write this article body to a file, overwriting it if the file exists.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s body in file" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers nil 'gnus-newsgroup-last-directory))
  (gnus-summary-save-body-in-file filename t))

(put 'gnus-summary-save-in-pipe :decode t)
(put 'gnus-summary-save-in-pipe :headers 'gnus-saved-headers)
(defun gnus-summary-save-in-pipe (&optional command raw)
  "Pipe this article to subprocess COMMAND.
Valid values for COMMAND include:
  a string
    The executable command name and possibly arguments.
  nil
    You will be prompted for the command in the minibuffer.
  the symbol `default'
    It will be replaced with the command which the variable
    `gnus-summary-pipe-output-default-command' holds or the command
    last used for saving.
Non-nil value for RAW overrides `:decode' and `:headers' properties
and the raw article including all headers will be piped."
  (let ((article (gnus-summary-article-number))
	(decode (unless raw
		  (get 'gnus-summary-save-in-pipe :decode)))
	save-buffer default)
    (if article
	(if (vectorp (gnus-summary-article-header article))
	    (save-current-buffer
	      (gnus-summary-select-article decode decode nil article)
	      (insert-buffer-substring
	       (prog1
		   (if decode
		       gnus-article-buffer
		     gnus-original-article-buffer)
		 (setq save-buffer
		       (nnheader-set-temp-buffer " *Gnus Save*"))))
	      ;; Remove unwanted headers.
	      (when (and (not raw)
			 (or (get 'gnus-summary-save-in-pipe :headers)
			     (not gnus-save-all-headers)))
		(let ((gnus-visible-headers
		       (or (symbol-value (get 'gnus-summary-save-in-pipe
					      :headers))
			   gnus-saved-headers gnus-visible-headers))
		      (gnus-summary-buffer nil))
		  (article-hide-headers 1 t))))
	  (error "%d is not a real article" article))
      (error "No article to pipe"))
    (setq default (or gnus-summary-pipe-output-default-command
		      gnus-last-shell-command))
    (unless (stringp command)
      (setq command
	    (if (and (eq command 'default) default)
		default
	      (gnus-read-shell-command "Shell command on this article: "
				       default))))
    (when (string-equal command "")
      (if default
	  (setq command default)
	(error "A command is required")))
    (with-current-buffer save-buffer
      (save-restriction
	(widen)
	(shell-command-on-region (point-min) (point-max) command nil)))
    (gnus-kill-buffer save-buffer))
  (setq gnus-summary-pipe-output-default-command command))

(defun gnus-summary-pipe-to-muttprint (&optional command)
  "Pipe this article to muttprint."
  (unless (stringp command)
    (setq command (read-string
		   "Print using command: " gnus-summary-muttprint-program
		   nil gnus-summary-muttprint-program)))
  (let ((gnus-summary-pipe-output-default-command
	 gnus-summary-pipe-output-default-command))
    (gnus-summary-save-in-pipe command))
  (setq gnus-summary-muttprint-program command))

;;; Article file names when saving.

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name."
  (when (not (zerop (length newsgroup)))
    (concat (char-to-string (upcase (aref newsgroup 0)))
	    (substring newsgroup 1))))

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.  Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (file-relative-name
	  (expand-file-name "news" (gnus-newsgroup-directory-form newsgroup))
	  default-directory))
       gnus-article-save-directory)))

(defun gnus-sender-save-name (newsgroup headers &optional last-file)
  "Generate file name from sender."
  (let ((from (mail-header-from headers)))
    (expand-file-name
     (if (and from (string-match "\\([^ <]+\\)@" from))
	 (match-string 1 from)
       "nobody")
     gnus-article-save-directory)))

(defun article-verify-x-pgp-sig ()
  "Verify X-PGP-Sig."
  ;; <ftp://ftp.isc.org/pub/pgpcontrol/FORMAT>
  (interactive)
  (if (gnus-buffer-live-p gnus-original-article-buffer)
      (let ((sig (with-current-buffer gnus-original-article-buffer
		   (gnus-fetch-field "X-PGP-Sig")))
	    items info headers)
	(when (and sig
		   mml2015-use
		   (mml2015-clear-verify-function))
	  (with-temp-buffer
	    (insert-buffer-substring gnus-original-article-buffer)
	    (setq items (split-string sig))
	    (message-narrow-to-head)
	    (let ((inhibit-point-motion-hooks t)
		  (case-fold-search t))
	      ;; Don't verify multiple headers.
	      (setq headers (mapconcat (lambda (header)
					 (concat header ": "
						 (mail-fetch-field header)
						 "\n"))
				       (split-string (nth 1 items) ",") "")))
	    (delete-region (point-min) (point-max))
	    (insert "-----BEGIN PGP SIGNED MESSAGE-----\n\n")
	    (insert "X-Signed-Headers: " (nth 1 items) "\n")
	    (insert headers)
	    (widen)
	    (forward-line)
	    (while (not (eobp))
	      (if (looking-at "^-")
		  (insert "- "))
	      (forward-line))
	    (insert "\n-----BEGIN PGP SIGNATURE-----\n")
	    (insert "Version: " (car items) "\n\n")
	    (insert (mapconcat 'identity (cddr items) "\n"))
	    (insert "\n-----END PGP SIGNATURE-----\n")
	    (let ((mm-security-handle (list (format "multipart/signed"))))
	      (mml2015-clean-buffer)
	      (let ((coding-system-for-write (or gnus-newsgroup-charset
						 'iso-8859-1)))
		(funcall (mml2015-clear-verify-function)))
	      (setq info
		    (or (mm-handle-multipart-ctl-parameter
			 mm-security-handle 'gnus-details)
			(mm-handle-multipart-ctl-parameter
			 mm-security-handle 'gnus-info)))))
	  (when info
	    (let ((inhibit-read-only t) bface eface)
	      (save-restriction
		(message-narrow-to-head)
		(goto-char (point-max))
		(forward-line -1)
		(setq bface (get-text-property (point-at-bol) 'face)
		      eface (get-text-property (1- (point-at-eol)) 'face))
		(message-remove-header "X-Gnus-PGP-Verify")
		(if (re-search-forward "^X-PGP-Sig:" nil t)
		    (forward-line)
		  (goto-char (point-max)))
		(narrow-to-region (point) (point))
		(insert "X-Gnus-PGP-Verify: " info "\n")
		(goto-char (point-min))
		(forward-line)
		(while (not (eobp))
		  (if (not (looking-at "^[ \t]"))
		      (insert " "))
		  (forward-line))
		;; Do highlighting.
		(goto-char (point-min))
		(when (looking-at "\\([^:]+\\): *")
		  (put-text-property (match-beginning 1) (1+ (match-end 1))
				     'face bface)
		  (put-text-property (match-end 0) (point-max)
				     'face eface)))))))))

(autoload 'canlock-verify "canlock" nil t) ;; for XEmacs.

(defun article-verify-cancel-lock ()
  "Verify Cancel-Lock header."
  (interactive)
  (if (gnus-buffer-live-p gnus-original-article-buffer)
      (canlock-verify gnus-original-article-buffer)))

(eval-and-compile
  (mapc
   (lambda (func)
     (let (afunc gfunc)
       (if (consp func)
	   (setq afunc (car func)
		 gfunc (cdr func))
	 (setq afunc func
	       gfunc (intern (format "gnus-%s" func))))
       (defalias gfunc
	 (when (fboundp afunc)
	   `(lambda (&optional interactive &rest args)
	      ,(documentation afunc t)
	      (interactive (list t))
	      (with-current-buffer gnus-article-buffer
		(if interactive
		    (call-interactively ',afunc)
		  (apply ',afunc args))))))))
   '(article-hide-headers
     article-verify-x-pgp-sig
     article-verify-cancel-lock
     article-hide-boring-headers
     article-treat-overstrike
     article-treat-ansi-sequences
     article-fill-long-lines
     article-capitalize-sentences
     article-remove-cr
     article-remove-leading-whitespace
     article-display-x-face
     article-display-face
     article-de-quoted-unreadable
     article-de-base64-unreadable
     article-decode-HZ
     article-wash-html
     article-unsplit-urls
     article-hide-list-identifiers
     article-strip-banner
     article-babel
     article-hide-pem
     article-hide-signature
     article-strip-headers-in-body
     article-remove-trailing-blank-lines
     article-strip-leading-blank-lines
     article-strip-multiple-blank-lines
     article-strip-leading-space
     article-strip-trailing-space
     article-strip-blank-lines
     article-strip-all-blank-lines
     article-date-local
     article-date-english
     article-date-iso8601
     article-date-original
     article-treat-date
     article-date-ut
     article-decode-mime-words
     article-decode-charset
     article-decode-encoded-words
     article-date-user
     article-date-lapsed
     article-date-combined-lapsed
     article-emphasize
     article-treat-dumbquotes
     article-treat-non-ascii
     article-normalize-headers
     ;;(article-show-all . gnus-article-show-all-headers)
     )))

;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(set-keymap-parent gnus-article-mode-map widget-keymap)

(gnus-define-keys gnus-article-mode-map
  " " gnus-article-goto-next-page
  "\177" gnus-article-goto-prev-page
  [delete] gnus-article-goto-prev-page
  [backspace] gnus-article-goto-prev-page
  "\C-c^" gnus-article-refer-article
  "h" gnus-article-show-summary
  "s" gnus-article-show-summary
  "\C-c\C-m" gnus-article-mail
  "?" gnus-article-describe-briefly
  "<" beginning-of-buffer
  ">" end-of-buffer
  "\C-c\C-i" gnus-info-find-node
  "\C-c\C-b" gnus-bug
  "R" gnus-article-reply-with-original
  "F" gnus-article-followup-with-original
  "\C-hk" gnus-article-describe-key
  "\C-hc" gnus-article-describe-key-briefly
  "\C-hb" gnus-article-describe-bindings

  "e" gnus-article-read-summary-keys
  "\C-d" gnus-article-read-summary-keys
  "\M-*" gnus-article-read-summary-keys
  "\M-#" gnus-article-read-summary-keys
  "\M-^" gnus-article-read-summary-keys
  "\M-g" gnus-article-read-summary-keys)

(substitute-key-definition
 'undefined 'gnus-article-read-summary-keys gnus-article-mode-map)

(gnus-define-keys (gnus-article-send-map "S" gnus-article-mode-map)
  "W" gnus-article-wide-reply-with-original)
(if (featurep 'xemacs)
    (set-keymap-default-binding gnus-article-send-map
				'gnus-article-read-summary-send-keys)
  (define-key gnus-article-send-map [t] 'gnus-article-read-summary-send-keys))

(defun gnus-article-make-menu-bar ()
  (unless (boundp 'gnus-article-commands-menu)
    (gnus-summary-make-menu-bar))
  (unless (boundp 'gnus-article-article-menu)
    (easy-menu-define
     gnus-article-article-menu gnus-article-mode-map ""
     '("Article"
       ["Scroll forwards" gnus-article-goto-next-page t]
       ["Scroll backwards" gnus-article-goto-prev-page t]
       ["Show summary" gnus-article-show-summary t]
       ["Fetch Message-ID at point" gnus-article-refer-article t]
       ["Mail to address at point" gnus-article-mail t]
       ["Send a bug report" gnus-bug t]))

    (easy-menu-define
     gnus-article-treatment-menu gnus-article-mode-map ""
     ;; Fixme: this should use :active (and maybe :visible).
     '("Treatment"
       ["Hide headers" gnus-article-hide-headers t]
       ["Hide signature" gnus-article-hide-signature t]
       ["Hide citation" gnus-article-hide-citation t]
       ["Treat overstrike" gnus-article-treat-overstrike t]
       ["Treat ANSI sequences" gnus-article-treat-ansi-sequences t]
       ["Remove carriage return" gnus-article-remove-cr t]
       ["Remove leading whitespace" gnus-article-remove-leading-whitespace t]
       ["Remove quoted-unreadable" gnus-article-de-quoted-unreadable t]
       ["Remove base64" gnus-article-de-base64-unreadable t]
       ["Treat html" gnus-article-wash-html t]
       ["Remove newlines from within URLs" gnus-article-unsplit-urls t]
       ["Decode HZ" gnus-article-decode-HZ t]))

    ;; Note "Commands" menu is defined in gnus-sum.el for consistency

    ;; Note "Post" menu is defined in gnus-sum.el for consistency

    (gnus-run-hooks 'gnus-article-menu-hook)))

(defvar bookmark-make-record-function)
(defvar shr-put-image-function)

(defun gnus-article-mode ()
  "Major mode for displaying an article.

All normal editing commands are switched off.

The following commands are available in addition to all summary mode
commands:
\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node"
  (interactive)
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq mode-name "Article")
  (setq major-mode 'gnus-article-mode)
  (make-local-variable 'minor-mode-alist)
  (use-local-map gnus-article-mode-map)
  (when (gnus-visual-p 'article-menu 'menu)
    (gnus-article-make-menu-bar)
    (when gnus-summary-tool-bar-map
      (set (make-local-variable 'tool-bar-map) gnus-summary-tool-bar-map)))
  (gnus-update-format-specifications nil 'article-mode)
  (set (make-local-variable 'page-delimiter) gnus-page-delimiter)
  (set (make-local-variable 'gnus-page-broken) nil)
  (make-local-variable 'gnus-article-current-summary)
  (make-local-variable 'gnus-article-mime-handles)
  (make-local-variable 'gnus-article-decoded-p)
  (make-local-variable 'gnus-article-mime-handle-alist)
  (make-local-variable 'gnus-article-wash-types)
  (make-local-variable 'gnus-article-image-alist)
  (make-local-variable 'gnus-article-charset)
  (make-local-variable 'gnus-article-ignored-charsets)
  (set (make-local-variable 'bookmark-make-record-function)
       'gnus-summary-bookmark-make-record)
  ;; Prevent Emacs 22 from displaying non-break space with `nobreak-space'
  ;; face.
  (set (make-local-variable 'nobreak-char-display) nil)
  ;; Enable `gnus-article-remove-images' to delete images shr.el renders.
  (set (make-local-variable 'shr-put-image-function) 'gnus-shr-put-image)
  (setq cursor-in-non-selected-windows nil)
  (gnus-set-default-directory)
  (buffer-disable-undo)
  (setq buffer-read-only t
	show-trailing-whitespace nil)
  (set-syntax-table gnus-article-mode-syntax-table)
  (mm-enable-multibyte)
  (gnus-run-mode-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize the article buffer."
  (let* ((name (if gnus-single-article-buffer "*Article*"
		 (concat "*Article "
			 (gnus-group-decoded-name gnus-newsgroup-name)
			 "*")))
	 (original
	  (progn (string-match "\\*Article" name)
		 (concat " *Original Article"
			 (substring name (match-end 0))))))
    (setq gnus-article-buffer name)
    (setq gnus-original-article-buffer original)
    (setq gnus-article-mime-handle-alist nil)
    (with-current-buffer gnus-summary-buffer
      ;; This might be a variable local to the summary buffer.
      (unless gnus-single-article-buffer
	(setq gnus-article-buffer name)
	(setq gnus-original-article-buffer original)
	(gnus-set-global-variables)))
    (gnus-article-setup-highlight-words)
    ;; Init original article buffer.
    (with-current-buffer (gnus-get-buffer-create gnus-original-article-buffer)
      (mm-enable-multibyte)
      (setq major-mode 'gnus-original-article-mode)
      (make-local-variable 'gnus-original-article))
    (if (and (get-buffer name)
	     (with-current-buffer name
	       (if gnus-article-edit-mode
		   (if (y-or-n-p "Article mode edit in progress; discard? ")
		       (progn
			 (set-buffer-modified-p nil)
			 (gnus-kill-buffer name)
			 (message "")
			 nil)
		     (error "Action aborted"))
		 t)))
	(with-current-buffer name
	  (set (make-local-variable 'gnus-article-edit-mode) nil)
	  (gnus-article-stop-animations)
	  (when gnus-article-mime-handles
	    (mm-destroy-parts gnus-article-mime-handles)
	    (setq gnus-article-mime-handles nil))
	  ;; Set it to nil in article-buffer!
	  (setq gnus-article-mime-handle-alist nil)
	  (buffer-disable-undo)
	  (setq buffer-read-only t)
	  (unless (eq major-mode 'gnus-article-mode)
	    (gnus-article-mode))
	  (setq truncate-lines gnus-article-truncate-lines)
	  (current-buffer))
      (with-current-buffer (gnus-get-buffer-create name)
	(gnus-article-mode)
	(setq truncate-lines gnus-article-truncate-lines)
	(make-local-variable 'gnus-summary-buffer)
	(setq gnus-summary-buffer
	      (gnus-summary-buffer-name gnus-newsgroup-name))
	(gnus-summary-set-local-parameters gnus-newsgroup-name)
	(when article-lapsed-timer
	  (gnus-stop-date-timer))
	(when gnus-article-update-date-headers
	  (gnus-start-date-timer gnus-article-update-date-headers))
	(current-buffer)))))

(defun gnus-article-stop-animations ()
  (dolist (timer (and (boundp 'timer-list)
		      timer-list))
    (when (eq (elt timer 5) 'image-animate-timeout)
      (cancel-timer timer))))

(defun gnus-stop-downloads ()
  (when (boundp 'url-queue)
    (set (intern "url-queue" obarray) nil)))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (let ((article-window (gnus-get-buffer-window gnus-article-buffer t)))
    (when article-window
      (set-window-start
       article-window
       (with-current-buffer gnus-article-buffer
	 (goto-char (point-min))
	 (if (not line)
	     (point-min)
	   (gnus-message 6 "Moved to bookmark")
	   (search-forward "\n\n" nil t)
	   (forward-line line)
	   (point)))))))

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (unless (eq major-mode 'gnus-summary-mode)
      (set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    (let* ((gnus-article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (gnus-tmp-internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	;; Deactivate active regions.
	(when (and (boundp 'transient-mark-mode)
		   transient-mark-mode)
	  (setq mark-active nil))
	(if (not (setq result (let ((inhibit-read-only t))
				(gnus-request-article-this-buffer
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (when (and (numberp article)
			 (not (memq article gnus-newsgroup-sparse)))
		(setq gnus-article-current
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(if (and (memq article gnus-newsgroup-undownloaded)
			 (not (gnus-online (gnus-find-method-for-group
					    gnus-newsgroup-name))))
		    (progn
		      (gnus-summary-set-agent-mark article)
		      (message "Message marked for downloading"))
		  (gnus-summary-mark-article article gnus-canceled-mark)
		  (unless (memq article gnus-newsgroup-sparse)
		    (gnus-error 1 "No such article (may have expired or been canceled)")))))
	  (if (or (eq result 'pseudo)
		  (eq result 'nneething))
	      (progn
		(with-current-buffer summary-buffer
		  (push article gnus-newsgroup-history)
		  (setq gnus-last-article gnus-current-article
			gnus-current-article 0
			gnus-current-headers nil
			gnus-article-current nil)
		  (if (eq result 'nneething)
		      (gnus-configure-windows 'summary)
		    (gnus-configure-windows 'article))
		  (gnus-set-global-variables))
		(let ((gnus-article-mime-handle-alist-1
		       gnus-article-mime-handle-alist))
		  (gnus-set-mode-line 'article)))
	    ;; The result from the `request' was an actual article -
	    ;; or at least some text that is now displayed in the
	    ;; article buffer.
	    (when (and (numberp article)
		       (not (eq article gnus-current-article)))
	      ;; Seems like a new article has been selected.
	      ;; `gnus-current-article' must be an article number.
	      (with-current-buffer summary-buffer
		(push article gnus-newsgroup-history)
		(setq gnus-last-article gnus-current-article
		      gnus-current-article article
		      gnus-current-headers
		      (gnus-summary-article-header gnus-current-article)
		      gnus-article-current
		      (cons gnus-newsgroup-name gnus-current-article))
		(unless (vectorp gnus-current-headers)
		  (setq gnus-current-headers nil))
		(gnus-summary-goto-subject gnus-current-article)
		(when (gnus-summary-show-thread)
		  ;; If the summary buffer really was folded, the
		  ;; previous goto may not actually have gone to
		  ;; the right article, but the thread root instead.
		  ;; So we go again.
		  (gnus-summary-goto-subject gnus-current-article))
		(gnus-run-hooks 'gnus-mark-article-hook)
		(gnus-set-mode-line 'summary)
		(when (gnus-visual-p 'article-highlight 'highlight)
		  (gnus-run-hooks 'gnus-visual-mark-article-hook))
		;; Set the global newsgroup variables here.
		(gnus-set-global-variables)
		(setq gnus-have-all-headers
		      (or all-headers gnus-show-all-headers))))
	    (save-excursion
	      (gnus-configure-windows 'article))
	    (when (or (numberp article)
		      (stringp article))
	      (gnus-article-prepare-display)
	      ;; Do page break.
	      (goto-char (point-min))
	      (when gnus-break-pages
		(gnus-narrow-to-page)))
	    (let ((gnus-article-mime-handle-alist-1
		   gnus-article-mime-handle-alist))
	      (gnus-set-mode-line 'article))
	    (article-goto-body)
	    (unless (bobp)
	      (forward-line -1))
	    (set-window-point (get-buffer-window (current-buffer)) (point))
	    (gnus-configure-windows 'article)
	    (gnus-run-hooks 'gnus-article-prepare-hook)
	    t))))))

;;;###autoload
(defun gnus-article-prepare-display ()
  "Make the current buffer look like a nice article."
  ;; Hooks for getting information from the article.
  ;; This hook must be called before being narrowed.
  (let ((gnus-article-buffer (current-buffer))
	buffer-read-only
	(inhibit-read-only t))
    (unless (eq major-mode 'gnus-article-mode)
      (gnus-article-mode))
    (setq buffer-read-only nil
	  gnus-article-wash-types nil
	  gnus-article-image-alist nil)
    (gnus-run-hooks 'gnus-tmp-internal-hook)
    (when gnus-display-mime-function
      (funcall gnus-display-mime-function))))

;;;
;;; Gnus Sticky Article Mode
;;;

(define-derived-mode gnus-sticky-article-mode gnus-article-mode "StickyArticle"
  "Mode for sticky articles."
  ;; Release bindings that won't work.
  (substitute-key-definition 'gnus-article-read-summary-keys 'undefined
			     gnus-sticky-article-mode-map)
  (substitute-key-definition 'gnus-article-refer-article 'undefined
			     gnus-sticky-article-mode-map)
  (dolist (k '("e" "h" "s" "F" "R"))
    (define-key gnus-sticky-article-mode-map k nil))
  (define-key gnus-sticky-article-mode-map "k" 'gnus-kill-sticky-article-buffer)
  (define-key gnus-sticky-article-mode-map "q" 'bury-buffer)
  (define-key gnus-sticky-article-mode-map "\C-hc" 'describe-key-briefly)
  (define-key gnus-sticky-article-mode-map "\C-hk" 'describe-key))

(defun gnus-sticky-article (arg)
  "Make the current article sticky.
If a prefix ARG is given, ask for a name for this sticky article buffer."
  (interactive "P")
  (gnus-summary-show-thread)
  (gnus-summary-select-article nil nil 'pseudo)
  (let (new-art-buf-name)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (setq new-art-buf-name
	    (concat
	     "*Sticky Article: "
	     (if arg
		 (read-from-minibuffer "Sticky article buffer name: ")
	       (gnus-with-article-headers
		 (gnus-article-goto-header "subject")
		 (setq new-art-buf-name
		       (buffer-substring-no-properties
			(line-beginning-position) (line-end-position)))
		 (goto-char (point-min))
		 (gnus-article-goto-header "from")
		 (setq new-art-buf-name
		       (concat
			new-art-buf-name ", "
			(buffer-substring-no-properties
			 (line-beginning-position) (line-end-position))))
		 (goto-char (point-min))
		 (gnus-article-goto-header "date")
		 (setq new-art-buf-name
		       (concat
			new-art-buf-name ", "
			(buffer-substring-no-properties
			 (line-beginning-position) (line-end-position))))))
	     "*"))
      (if (and (gnus-buffer-live-p new-art-buf-name)
	       (with-current-buffer new-art-buf-name
		 (eq major-mode 'gnus-sticky-article-mode)))
	  (switch-to-buffer new-art-buf-name)
	(setq new-art-buf-name (rename-buffer new-art-buf-name t)))
      (gnus-sticky-article-mode))
    (setq gnus-article-buffer new-art-buf-name))
  (gnus-summary-recenter)
  (gnus-summary-position-point))

(defun gnus-kill-sticky-article-buffer (&optional buffer)
  "Kill the given sticky article BUFFER.
If none is given, assume the current buffer and kill it if it has
`gnus-sticky-article-mode'."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (when (eq major-mode 'gnus-sticky-article-mode)
      (gnus-kill-buffer buffer))))

(defun gnus-kill-sticky-article-buffers (arg)
  "Kill all sticky article buffers.
If a prefix ARG is given, ask for confirmation."
  (interactive "P")
  (dolist (buf (gnus-buffers))
    (with-current-buffer buf
      (when (eq major-mode 'gnus-sticky-article-mode)
	(if (not arg)
	    (gnus-kill-buffer buf)
	  (when (yes-or-no-p (concat "Kill buffer " (buffer-name buf) "? "))
	    (gnus-kill-buffer buf)))))))

;;;
;;; Gnus MIME viewing functions
;;;

(defvar gnus-mime-button-line-format "%{%([%p. %d%T]%)%}%e\n"
  "Format of the MIME buttons.

Valid specifiers include:
%t  The MIME type
%T  MIME type, along with additional info
%n  The `name' parameter
%d  The description, if any
%l  The length of the encoded part
%p  The part identifier number
%e  Dots if the part isn't displayed

General format specifiers can also be used.  See Info node
`(gnus)Formatting Variables'.")

(defvar gnus-mime-button-line-format-alist
  '((?t gnus-tmp-type ?s)
    (?T gnus-tmp-type-long ?s)
    (?n gnus-tmp-name ?s)
    (?d gnus-tmp-description ?s)
    (?p gnus-tmp-id ?s)
    (?l gnus-tmp-length ?d)
    (?e gnus-tmp-dots ?s)))

(defvar gnus-mime-button-commands
  '((gnus-article-press-button "\r" "Toggle Display")
    (gnus-mime-view-part "v" "View Interactively...")
    (gnus-mime-view-part-as-type "t" "View As Type...")
    (gnus-mime-view-part-as-charset "C" "View As charset...")
    (gnus-mime-save-part "o" "Save...")
    (gnus-mime-save-part-and-strip "\C-o" "Save and Strip")
    (gnus-mime-replace-part "r" "Replace part")
    (gnus-mime-delete-part "d" "Delete part")
    (gnus-mime-copy-part "c" "View As Text, In Other Buffer")
    (gnus-mime-inline-part "i" "View As Text, In This Buffer")
    (gnus-mime-view-part-internally "E" "View Internally") ;; Why `E'?
    (gnus-mime-view-part-externally "e" "View Externally")
    (gnus-mime-print-part "p" "Print")
    (gnus-mime-pipe-part "|" "Pipe To Command...")
    (gnus-mime-action-on-part "." "Take action on the part...")))

(defun gnus-article-mime-part-status ()
  (if gnus-article-mime-handle-alist-1
      (if (eq 1 (length gnus-article-mime-handle-alist-1))
	  " (1 part)"
	(format " (%d parts)" (length gnus-article-mime-handle-alist-1)))
    ""))

(defvar gnus-mime-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map gnus-mouse-2 'gnus-article-push-button)
    (define-key map gnus-down-mouse-3 'gnus-mime-button-menu)
    (dolist (c gnus-mime-button-commands)
      (define-key map (cadr c) (car c)))
    map))

(easy-menu-define
  gnus-mime-button-menu gnus-mime-button-map "MIME button menu."
  `("MIME Part"
    ,@(mapcar (lambda (c)
		(vector (caddr c) (car c) :active t))
	      gnus-mime-button-commands)))

(defvar gnus-url-button-commands
  '((gnus-article-copy-string "u" "Copy URL to kill ring")))

(defvar gnus-url-button-map
  (let ((map (make-sparse-keymap)))
    (dolist (c gnus-url-button-commands)
      (define-key map (cadr c) (car c)))
    map))

(easy-menu-define
  gnus-url-button-menu gnus-url-button-map "URL button menu."
  `("Url Button"
    ,@(mapcar (lambda (c)
		(vector (caddr c) (car c) :active t))
	      gnus-url-button-commands)))

(defmacro gnus-bind-safe-url-regexp (&rest body)
  "Bind `mm-w3m-safe-url-regexp' according to `gnus-safe-html-newsgroups'."
  `(let ((mm-w3m-safe-url-regexp
	  (let ((group (if (and (eq major-mode 'gnus-article-mode)
				(gnus-buffer-live-p
				 gnus-article-current-summary))
			   (with-current-buffer gnus-article-current-summary
			     gnus-newsgroup-name)
			 gnus-newsgroup-name)))
	    (if (cond ((not group)
		       ;; Maybe we're in a mml-preview buffer
		       ;; and no group is selected.
		       t)
		      ((stringp gnus-safe-html-newsgroups)
		       (string-match gnus-safe-html-newsgroups group))
		      ((consp gnus-safe-html-newsgroups)
		       (member group gnus-safe-html-newsgroups)))
		nil
	      mm-w3m-safe-url-regexp))))
     ,@body))

(defun gnus-mime-button-menu (event prefix)
 "Construct a context-sensitive menu of MIME commands."
 (interactive "e\nP")
 (save-window-excursion
   (let ((pos (event-start event)))
     (select-window (posn-window pos))
     (goto-char (posn-point pos))
     (gnus-article-check-buffer)
     (popup-menu gnus-mime-button-menu nil prefix))))

(defun gnus-mime-view-all-parts (&optional handles)
  "View all the MIME parts."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (let ((handles (or handles gnus-article-mime-handles))
	  (mail-parse-charset gnus-newsgroup-charset)
	  (mail-parse-ignored-charsets
	   (with-current-buffer gnus-summary-buffer
	     gnus-newsgroup-ignored-charsets)))
      (when handles
	(mm-remove-parts handles)
	(goto-char (point-min))
	(or (search-forward "\n\n") (goto-char (point-max)))
	(let ((inhibit-read-only t))
	  (delete-region (point) (point-max))
	  (gnus-bind-safe-url-regexp (mm-display-parts handles)))))))

(defun gnus-article-jump-to-part (n)
  "Jump to MIME part N."
  (interactive "P")
  (let ((parts (with-current-buffer gnus-article-buffer
		 (length gnus-article-mime-handle-alist))))
    (when (zerop parts)
      (error "No such part"))
    (pop-to-buffer gnus-article-buffer)
    (or n
	(setq n (if (= parts 1)
		    1
		  (read-number (format "Jump to part (1..%s): " parts)))))
    (unless (and (integerp n) (<= n parts) (>= n 1))
      (setq n
	    (progn
	      (gnus-message 7 "Invalid part `%s', using %s instead."
			    n parts)
	      parts)))
    (gnus-message 9 "Jumping to part %s." n)
    (cond ((>= gnus-auto-select-part 1)
	   (while (and (<= n parts)
		       (not (gnus-article-goto-part n)))
	     (setq n (1+ n))))
	  ((< gnus-auto-select-part 0)
	   (while (and (>= n 1)
		       (not (gnus-article-goto-part n)))
	     (setq n (1- n))))
	  (t
	   (gnus-article-goto-part n)))))

(defvar gnus-mime-buttonized-part-id nil
  "ID of a mime part that should be buttonized.
`gnus-mime-save-part-and-strip' and `gnus-mime-delete-part' bind it.")

(eval-when-compile
  (defsubst gnus-article-edit-part (handles &optional current-id)
    "Edit an article in order to delete a mime part.
This function is exclusively used by `gnus-mime-save-part-and-strip'
and `gnus-mime-delete-part', and not provided at run-time normally."
    (gnus-article-edit-article
     `(lambda ()
	(buffer-disable-undo)
	(erase-buffer)
	(let ((mail-parse-charset (or gnus-article-charset
				      ',gnus-newsgroup-charset))
	      (mail-parse-ignored-charsets
	       (or gnus-article-ignored-charsets
		   ',gnus-newsgroup-ignored-charsets))
	      (mbl mml-buffer-list))
	  (setq mml-buffer-list nil)
	  (insert-buffer-substring gnus-original-article-buffer)
	  (mime-to-mml ',handles)
	  (setq gnus-article-mime-handles nil)
	  (let ((mbl1 mml-buffer-list))
	    (setq mml-buffer-list mbl)
	    (set (make-local-variable 'mml-buffer-list) mbl1))
	  (gnus-make-local-hook 'kill-buffer-hook)
	  (add-hook 'kill-buffer-hook 'mml-destroy-buffers t t)))
     `(lambda (no-highlight)
	(let ((mail-parse-charset (or gnus-article-charset
				      ',gnus-newsgroup-charset))
	      (message-options message-options)
	      (message-options-set-recipient)
	      (mail-parse-ignored-charsets
	       (or gnus-article-ignored-charsets
		   ',gnus-newsgroup-ignored-charsets)))
	  (mml-to-mime)
	  (mml-destroy-buffers)
	  (remove-hook 'kill-buffer-hook
		       'mml-destroy-buffers t)
	  (kill-local-variable 'mml-buffer-list))
	(gnus-summary-edit-article-done
	 ,(or (mail-header-references gnus-current-headers) "")
	 ,(gnus-group-read-only-p)
	 ,gnus-summary-buffer no-highlight))
     t)
    ;; Force buttonizing this part.
    (let ((gnus-mime-buttonized-part-id current-id))
      (gnus-article-edit-done))
    (gnus-configure-windows 'article)
    (when (and current-id (integerp gnus-auto-select-part))
      (gnus-article-jump-to-part
       (min (max (+ current-id gnus-auto-select-part) 1)
	    (with-current-buffer gnus-article-buffer
	      (length gnus-article-mime-handle-alist)))))))

(defun gnus-mime-replace-part (file)
  "Replace MIME part under point with an external body."
  ;; Useful if file has already been saved to disk
  (interactive
   (list
    (read-file-name "Replace MIME part with file: "
                    (or mm-default-directory default-directory)
                    nil nil)))
  (gnus-mime-save-part-and-strip file))

(defun gnus-mime-save-part-and-strip (&optional file)
  "Save the MIME part under point then replace it with an external body.
If FILE is given, use it for the external part."
  (interactive)
  (gnus-article-check-buffer)
  (when (gnus-group-read-only-p)
    (error "The current group does not support deleting of parts"))
  (when (mm-complicated-handles gnus-article-mime-handles)
    (error "\
The current article has a complicated MIME structure, giving up..."))
  (let* ((data (get-text-property (point) 'gnus-data))
	 (id (get-text-property (point) 'gnus-part))
	 param
	 (handles gnus-article-mime-handles))
    (unless file
      (setq file
	    (and data (mm-save-part data "Delete MIME part and save to: "))))
    (when file
      (with-current-buffer (mm-handle-buffer data)
	(erase-buffer)
	(insert "Content-Type: " (mm-handle-media-type data))
	(mml-insert-parameter-string (cdr (mm-handle-type data))
				     '(charset))
	;; Add a filename for the sake of saving the part again.
	(mml-insert-parameter
	 (mail-header-encode-parameter "name" (file-name-nondirectory file)))
	(insert "\n")
	(insert "Content-ID: " (message-make-message-id) "\n")
	(insert "Content-Transfer-Encoding: binary\n")
	(insert "\n"))
      (setcdr data
	      (cdr (mm-make-handle nil
				   `("message/external-body"
				     (access-type . "LOCAL-FILE")
				     (name . ,file)))))
      ;; (set-buffer gnus-summary-buffer)
      (gnus-article-edit-part handles id))))

;; A function like `gnus-summary-save-parts' (`X m', `<MIME> <Extract all
;; parts...>') but with stripping would be nice.

(defun gnus-mime-delete-part ()
  "Delete the MIME part under point.
Replace it with some information about the removed part."
  (interactive)
  (gnus-article-check-buffer)
  (when (gnus-group-read-only-p)
    (error "The current group does not support deleting of parts"))
  (when (mm-complicated-handles gnus-article-mime-handles)
    (error "\
The current article has a complicated MIME structure, giving up..."))
  (when (or gnus-expert-user
	    (gnus-yes-or-no-p "\
Deleting parts may malfunction or destroy the article; continue? "))
    (let* ((data (get-text-property (point) 'gnus-data))
	   (id (get-text-property (point) 'gnus-part))
	   (handles gnus-article-mime-handles)
	   (description
	    (let ((desc (mm-handle-description data)))
	      (when desc
		(mail-decode-encoded-word-string desc))))
	   (filename (or (mm-handle-filename data) "(none)"))
	   (type (mm-handle-media-type data)))
      (unless data
	(error "No MIME part under point"))
      (with-current-buffer (mm-handle-buffer data)
	(let ((bsize (buffer-size)))
	  (erase-buffer)
	  (insert
	   (concat
	    ",----\n"
	    "| The following attachment has been deleted:\n"
	    "|\n"
	    "| Type:           " type "\n"
	    "| Filename:       " filename "\n"
	    "| Size (encoded): " (format "%s byte%s\n"
					 bsize (if (= bsize 1)
						   ""
						 "s"))
	    (when description
	      (concat    "| Description:    " description "\n"))
	    "`----\n"))
	  (setcdr data
		  (cdr (mm-make-handle
			nil `("text/plain" (charset . gnus-decoded)) nil nil
			(list "attachment")
			(format "Deleted attachment (%s bytes)" bsize))))))
      ;; (set-buffer gnus-summary-buffer)
      (gnus-article-edit-part handles id))))

(defun gnus-mime-save-part ()
  "Save the MIME part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (mm-save-part data))))

(defun gnus-mime-pipe-part (&optional cmd)
  "Pipe the MIME part under point to a process.
Use CMD as the process."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (mm-pipe-part data cmd))))

(defun gnus-mime-view-part ()
  "Interactively choose a viewing method for the MIME part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (setq gnus-article-mime-handles
	    (mm-merge-handles
	     gnus-article-mime-handles (setq data (copy-sequence data))))
      (mm-interactively-view-part data))))

(defun gnus-mime-view-part-as-type-internal ()
  (gnus-article-check-buffer)
  (let* ((handle (get-text-property (point) 'gnus-data))
	 (name (or
		;; Content-Type: foo/bar; name=...
		(mail-content-type-get (mm-handle-type handle) 'name)
		;; Content-Disposition: attachment; filename=...
		(cdr (assq 'filename (cdr (mm-handle-disposition handle))))))
	 (def-type (and name (mm-default-file-encoding name))))
    (or (and def-type (cons def-type 0))
	(and handle
	     (equal (mm-handle-media-supertype handle) "text")
	     '("text/plain" . 0))
	'("application/octet-stream" . 0))))

(defun gnus-mime-view-part-as-type (&optional mime-type pred)
  "Choose a MIME media type, and view the part as such.
If non-nil, PRED is a predicate to use during completion to limit the
available media-types."
  (interactive)
  (unless mime-type
    (setq mime-type
	  (let ((default (gnus-mime-view-part-as-type-internal)))
	    (gnus-completing-read
	     "View as MIME type"
	     (if pred
		 (gnus-remove-if-not pred (mailcap-mime-types))
	       (mailcap-mime-types))
	     nil nil nil
	     (car default)))))
  (gnus-article-check-buffer)
  (let ((handle (get-text-property (point) 'gnus-data)))
    (when handle
      (when (equal (mm-handle-media-type handle) "message/external-body")
	(unless (mm-handle-cache handle)
	  (mm-extern-cache-contents handle))
	(setq handle (mm-handle-cache handle)))
      (setq handle
	    (mm-make-handle (mm-handle-buffer handle)
			    (cons mime-type (cdr (mm-handle-type handle)))
			    (mm-handle-encoding handle)
			    (mm-handle-undisplayer handle)
			    (mm-handle-disposition handle)
			    (mm-handle-description handle)
			    nil
			    (mm-handle-id handle)))
      (setq gnus-article-mime-handles
	    (mm-merge-handles gnus-article-mime-handles handle))
      (when (mm-handle-displayed-p handle)
	(mm-remove-part handle))
      (gnus-mm-display-part handle))))

(defun gnus-mime-copy-part (&optional handle arg)
  "Put the MIME part under point into a new buffer.
If `auto-compression-mode' is enabled, compressed files like .gz and .bz2
are decompressed."
  (interactive (list nil current-prefix-arg))
  (gnus-article-check-buffer)
  (unless handle
    (setq handle (get-text-property (point) 'gnus-data)))
  (when handle
    (let ((filename (mm-handle-filename handle))
	  contents dont-decode charset coding-system)
      (mm-with-unibyte-buffer
	(mm-insert-part handle)
	(setq contents (or (condition-case nil
			       (mm-decompress-buffer filename nil 'sig)
			     (error
			      (setq dont-decode t)
			      nil))
			   (buffer-string))))
      (setq filename (cond (filename (file-name-nondirectory filename))
			   (dont-decode "*raw data*")
			   (t "*decoded*")))
      (cond
       (dont-decode)
       ((not arg)
	(unless (setq charset (mail-content-type-get
			       (mm-handle-type handle) 'charset))
	  (unless (setq coding-system (mm-with-unibyte-buffer
					(insert contents)
					(mm-find-buffer-file-coding-system)))
	    (setq charset gnus-newsgroup-charset))))
       ((numberp arg)
	(setq charset (or (cdr (assq arg
				     gnus-summary-show-article-charset-alist))
			  (mm-read-coding-system "Charset: ")))))
      (switch-to-buffer (generate-new-buffer filename))
      (if (or coding-system
	      (and charset
		   (setq coding-system (mm-charset-to-coding-system charset))
		   (not (eq coding-system 'ascii))))
	  (progn
	    (mm-enable-multibyte)
	    (insert (mm-decode-coding-string contents coding-system))
	    (setq buffer-file-coding-system
		  (if (boundp 'last-coding-system-used)
		      (symbol-value 'last-coding-system-used)
		    coding-system)))
	(mm-disable-multibyte)
	(insert contents)
	(setq buffer-file-coding-system mm-binary-coding-system))
      ;; We do it this way to make `normal-mode' set the appropriate mode.
      (unwind-protect
	  (progn
	    (setq buffer-file-name (expand-file-name filename))
	    (normal-mode))
	(setq buffer-file-name nil))
      (goto-char (point-min)))))

(defun gnus-mime-print-part (&optional handle filename)
  "Print the MIME part under point."
  (interactive (list nil (ps-print-preprint current-prefix-arg)))
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (contents (and handle (mm-get-part handle)))
	 (file (mm-make-temp-file (expand-file-name "mm." mm-tmp-directory)))
	 (printer (mailcap-mime-info (mm-handle-media-type handle) "print")))
    (when contents
	(if printer
	    (unwind-protect
		(progn
		  (mm-save-part-to-file handle file)
		  (call-process shell-file-name nil
				(generate-new-buffer " *mm*")
				nil
				shell-command-switch
				(mm-mailcap-command
				 printer file (mm-handle-type handle))))
	      (delete-file file))
	  (with-temp-buffer
	    (insert contents)
	    (gnus-print-buffer))
	  (ps-despool filename)))))

(defun gnus-mime-inline-part (&optional handle arg)
  "Insert the MIME part under point into the current buffer.
Compressed files like .gz and .bz2 are decompressed."
  (interactive (list nil current-prefix-arg))
  (gnus-article-check-buffer)
  (unless handle
    (setq handle (get-text-property (point) 'gnus-data)))
  (when handle
    (let ((b (point))
	  (inhibit-read-only t)
	  contents charset coding-system)
      (if (and (not arg) (mm-handle-undisplayer handle))
	  (mm-remove-part handle)
	(mm-with-unibyte-buffer
	  (mm-insert-part handle)
	  (setq contents
		(or (mm-decompress-buffer (mm-handle-filename handle) nil t)
		    (buffer-string))))
	(cond
	 ((not arg)
	  (unless (setq charset (mail-content-type-get
				 (mm-handle-type handle) 'charset))
	    (unless (setq coding-system
			  (mm-with-unibyte-buffer
			    (insert contents)
			    (mm-find-buffer-file-coding-system)))
	      (setq charset gnus-newsgroup-charset))))
	 ((numberp arg)
	  (if (mm-handle-undisplayer handle)
	      (mm-remove-part handle))
	  (setq charset
		(or (cdr (assq arg
			       gnus-summary-show-article-charset-alist))
		    (mm-read-coding-system "Charset: "))))
	 (t
	  (if (mm-handle-undisplayer handle)
	      (mm-remove-part handle))))
	(forward-line 2)
        (mm-display-inline handle)
	(goto-char b)))))

(defun gnus-mime-set-charset-parameters (handle charset)
  "Set CHARSET to parameters in HANDLE.
CHARSET may either be a string or a symbol."
  (unless (stringp charset)
    (setq charset (symbol-name charset)))
  (if (stringp (car handle))
      (dolist (h (cdr handle))
	(gnus-mime-set-charset-parameters h charset))
    (let* ((type (mm-handle-type (if (equal (mm-handle-media-type handle)
					    "message/external-body")
				     (progn
				       (unless (mm-handle-cache handle)
					 (mm-extern-cache-contents handle))
				       (mm-handle-cache handle))
				   handle)))
	   (param (assq 'charset (cdr type))))
      (if param
	  (setcdr param charset)
	(setcdr type (cons (cons 'charset charset) (cdr type)))))))

(defun gnus-mime-view-part-as-charset (&optional handle arg)
  "Insert the MIME part under point into the current buffer using the
specified charset."
  (interactive (list nil current-prefix-arg))
  (gnus-article-check-buffer)
  (let ((handle (or handle (get-text-property (point) 'gnus-data)))
	(fun (get-text-property (point) 'gnus-callback))
	(gnus-newsgroup-ignored-charsets 'gnus-all)
	charset form preferred parts)
    (when handle
      (when (prog1
		(and fun
		     (setq charset
			   (or (cdr (assq
				     arg
				     gnus-summary-show-article-charset-alist))
			       (mm-read-coding-system "Charset: "))))
	      (if (mm-handle-undisplayer handle)
		  (mm-remove-part handle)))
	(gnus-mime-set-charset-parameters handle charset)
	(when (and (consp (setq form (cdr-safe fun)))
		   (setq form (ignore-errors
				(assq 'gnus-mime-display-alternative form)))
		   (setq preferred (caddr form))
		   (progn
		     (when (eq (car preferred) 'quote)
		       (setq preferred (cadr preferred)))
		     (not (equal preferred
				 (get-text-property (point) 'gnus-data))))
		   (setq parts (get-text-property (point) 'gnus-part))
		   (setq parts (cdr (assq parts
					  gnus-article-mime-handle-alist)))
		   (equal (mm-handle-media-type parts) "multipart/alternative")
		   (setq parts (reverse (cdr parts))))
	  (setcar (cddr form)
		  (list 'quote (or (cadr (member preferred parts))
				   (car parts)))))
	(funcall fun handle)))))

(defun gnus-mime-view-part-externally (&optional handle)
  "View the MIME part under point with an external viewer."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (mm-user-display-methods nil)
	 (mm-inlined-types nil)
	 (mail-parse-charset gnus-newsgroup-charset)
	 (mail-parse-ignored-charsets
          (with-current-buffer gnus-summary-buffer
            gnus-newsgroup-ignored-charsets))
         (type (mm-handle-media-type handle))
         (method (mailcap-mime-info type))
         (mm-enable-external t))
    (if (not (stringp method))
	(gnus-mime-view-part-as-type
	 nil (lambda (type) (stringp (mailcap-mime-info type))))
      (when handle
	(mm-display-part handle nil t)))))

(defun gnus-mime-view-part-internally (&optional handle)
  "View the MIME part under point with an internal viewer.
If no internal viewer is available, use an external viewer."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (mm-inlined-types '(".*"))
	 (mm-inline-large-images t)
	 (mail-parse-charset gnus-newsgroup-charset)
	 (mail-parse-ignored-charsets
	  (with-current-buffer gnus-summary-buffer
	    gnus-newsgroup-ignored-charsets))
	 (inhibit-read-only t))
    (if (not (mm-inlinable-p handle))
        (gnus-mime-view-part-as-type
         nil (lambda (type) (mm-inlinable-p handle type)))
      (when handle
	(gnus-bind-safe-url-regexp (mm-display-part handle))))))

(defun gnus-mime-action-on-part (&optional action)
  "Do something with the MIME attachment at \(point\)."
  (interactive
   (list (gnus-completing-read "Action" (mapcar 'car gnus-mime-action-alist) t)))
  (gnus-article-check-buffer)
  (let ((action-pair (assoc action gnus-mime-action-alist)))
    (if action-pair
	(funcall (cdr action-pair)))))

(defun gnus-article-part-wrapper (n function &optional no-handle interactive)
  "Call FUNCTION on MIME part N.
Unless NO-HANDLE, call FUNCTION with N-th MIME handle as its only argument.
If INTERACTIVE, call FUNCTION interactively."
  (let (window frame)
    ;; Check whether the article is displayed.
    (unless (and (gnus-buffer-live-p gnus-article-buffer)
		 (setq window (get-buffer-window gnus-article-buffer t))
		 (frame-visible-p (setq frame (window-frame window))))
      (error "No article is displayed"))
    (with-current-buffer gnus-article-buffer
      ;; Check whether the article displays the right contents.
      (unless (with-current-buffer gnus-summary-buffer
		(eq gnus-current-article (gnus-summary-article-number)))
	(error "You should select the right article first"))
      (if n
	  (setq n (prefix-numeric-value n))
	(let ((pt (point)))
	  (setq n (or (get-text-property pt 'gnus-part)
		      (and (not (bobp))
			   (get-text-property (1- pt) 'gnus-part))
		      (get-text-property (prog2
					     (forward-line 1)
					     (point)
					   (goto-char pt))
					 'gnus-part)
		      (get-text-property
		       (or (and (setq pt (previous-single-property-change
					  pt 'gnus-part))
				(1- pt))
			   (next-single-property-change	(point) 'gnus-part)
			   (point))
		       'gnus-part)
		      1))))
      ;; Check whether the specified part exists.
      (when (> n (length gnus-article-mime-handle-alist))
	(error "No such part")))
    (unless
	(progn
	  ;; To select the window is needed so that the cursor
	  ;; might be visible on the MIME button.
	  (select-window (prog1
			     window
			   (setq window (selected-window))
			   ;; Article may be displayed in the other frame.
			   (gnus-select-frame-set-input-focus
			    (prog1
				frame
			      (setq frame (selected-frame))))))
	  (when (gnus-article-goto-part n)
	    ;; We point the cursor and the arrow at the MIME button
	    ;; when the `function' prompt the user for something.
	    (unless (and (pos-visible-in-window-p)
			 (> (count-lines (point) (window-end))
			    (/ (1- (window-height)) 3)))
	      (recenter (/ (1- (window-height)) 3)))
	    (let ((cursor-in-non-selected-windows t)
		  (overlay-arrow-string "=>")
		  (overlay-arrow-position (point-marker)))
	      (unwind-protect
		  (cond
		   ((and no-handle interactive)
		    (call-interactively function))
		   (no-handle
		    (funcall function))
		   (interactive
		    (call-interactively
		     function (get-text-property (point) 'gnus-data)))
		   (t
		    (funcall function
			     (get-text-property (point) 'gnus-data))))
		(set-marker overlay-arrow-position nil)
		(unless gnus-auto-select-part
		  (gnus-select-frame-set-input-focus frame)
		  (select-window window))))
	    t))
      (if gnus-inhibit-mime-unbuttonizing
	  ;; This is the default though the program shouldn't reach here.
	  (error "No such part")
	;; The part which doesn't have the MIME button is selected.
	;; So, we display all the buttons and redo it.
	(let ((gnus-inhibit-mime-unbuttonizing t))
	  (gnus-summary-show-article)
	  (gnus-article-part-wrapper n function no-handle))))))

(defun gnus-article-pipe-part (n)
  "Pipe MIME part N, which is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'mm-pipe-part))

(defun gnus-article-save-part (n)
  "Save MIME part N, which is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'mm-save-part))

(defun gnus-article-interactively-view-part (n)
  "View MIME part N interactively, which is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'mm-interactively-view-part))

(defun gnus-article-copy-part (n)
  "Copy MIME part N, which is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-copy-part))

(defun gnus-article-view-part-as-charset (n)
  "View MIME part N using a specified charset.
N is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-view-part-as-charset))

(defun gnus-article-view-part-externally (n)
  "View MIME part N externally, which is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-view-part-externally))

(defun gnus-article-inline-part (n)
  "Inline MIME part N, which is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-inline-part))

(defun gnus-article-save-part-and-strip (n)
  "Save MIME part N and replace it with an external body.
N is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-save-part-and-strip t))

(defun gnus-article-replace-part (n)
  "Replace MIME part N with an external body.
N is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-replace-part t t))

(defun gnus-article-delete-part (n)
  "Delete MIME part N and add some information about the removed part.
N is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-delete-part t))

(defun gnus-article-view-part-as-type (n)
  "Choose a MIME media type, and view part N as such.
N is the numerical prefix."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-mime-view-part-as-type t))

(defun gnus-article-mime-match-handle-first (condition)
  (if condition
      (let (n)
	(dolist (ihandle gnus-article-mime-handle-alist)
	  (if (and (cond
		    ((functionp condition)
		     (funcall condition (cdr ihandle)))
		    ((eq condition 'undisplayed)
		     (not (or (mm-handle-undisplayer (cdr ihandle))
			      (equal (mm-handle-media-type (cdr ihandle))
				     "multipart/alternative"))))
		    ((eq condition 'undisplayed-alternative)
		     (not (mm-handle-undisplayer (cdr ihandle))))
		    (t t))
		   (gnus-article-goto-part (car ihandle))
		   (or (not n) (< (car ihandle) n)))
	      (setq n (car ihandle))))
	(or n 1))
    1))

(defun gnus-article-view-part (&optional n)
  "View MIME part N, which is the numerical prefix.
If the part is already shown, hide the part.  If N is nil, view
all parts."
  (interactive "P")
  (with-current-buffer gnus-article-buffer
    (or (numberp n) (setq n (gnus-article-mime-match-handle-first
			     gnus-article-mime-match-handle-function)))
    (when (> n (length gnus-article-mime-handle-alist))
      (error "No such part"))
    (let ((handle (cdr (assq n gnus-article-mime-handle-alist))))
      (when (gnus-article-goto-part n)
	(if (equal (car handle) "multipart/alternative")
	    (gnus-article-press-button)
	  (when (eq (gnus-mm-display-part handle) 'internal)
	    (gnus-set-window-start)))))))

(defsubst gnus-article-mime-total-parts ()
  (if (bufferp (car gnus-article-mime-handles))
      1 ;; single part
    (1- (length gnus-article-mime-handles))))

(defun gnus-mm-display-part (handle)
  "Display HANDLE and fix MIME button."
  (let ((id (get-text-property (point) 'gnus-part))
	(point (point))
	(inhibit-read-only t))
    (forward-line 1)
    (prog1
	(let ((window (selected-window))
	      (mail-parse-charset gnus-newsgroup-charset)
	      (mail-parse-ignored-charsets
	       (if (gnus-buffer-live-p gnus-summary-buffer)
		   (with-current-buffer gnus-summary-buffer
		     gnus-newsgroup-ignored-charsets)
		 nil)))
	  (save-excursion
	    (unwind-protect
		(let ((win (gnus-get-buffer-window (current-buffer) t))
		      (beg (point)))
		  (when win
		    (select-window win))
		  (goto-char point)
		  (forward-line)
		  (if (mm-handle-displayed-p handle)
		      ;; This will remove the part.
		      (mm-display-part handle)
		    (save-restriction
		      (narrow-to-region (point)
					(if (eobp) (point) (1+ (point))))
		      (gnus-bind-safe-url-regexp (mm-display-part handle))
		      ;; We narrow to the part itself and
		      ;; then call the treatment functions.
		      (goto-char (point-min))
		      (forward-line 1)
		      (narrow-to-region (point) (point-max))
		      (gnus-treat-article
		       nil id
		       (gnus-article-mime-total-parts)
		       (mm-handle-media-type handle)))))
	      (if (window-live-p window)
		  (select-window window)))))
      (goto-char point)
      (gnus-delete-line)
      (gnus-insert-mime-button
       handle id (list (mm-handle-displayed-p handle)))
      (goto-char point))))

(defun gnus-article-goto-part (n)
  "Go to MIME part N."
  (when gnus-break-pages
    (widen))
  (prog1
      (let ((start (text-property-any (point-min) (point-max) 'gnus-part n))
	    part handle end next handles)
	(when start
	  (goto-char start)
	  (if (setq handle (get-text-property start 'gnus-data))
	      start
	    ;; Go to the displayed subpart, assuming this is
	    ;; multipart/alternative.
	    (setq part start
		  end (point-at-eol))
	    (while (and (not handle)
			part
			(< part end)
			(setq next (text-property-not-all part end
							  'gnus-data nil)))
	      (setq part next
		    handle (get-text-property part 'gnus-data))
	      (push (cons handle part) handles)
	      (unless (mm-handle-displayed-p handle)
		(setq handle nil
		      part (text-property-any part end 'gnus-data nil))))
	    (unless handle
	      ;; No subpart is displayed, so we find preferred one.
	      (setq part
		    (cdr (assq (mm-preferred-alternative
				(nreverse (mapcar 'car handles)))
			       handles))))
	    (if part
		(goto-char (1+ part))
	      start))))
    (when gnus-break-pages
      (gnus-narrow-to-page))))

(defun gnus-insert-mime-button (handle gnus-tmp-id &optional displayed)
  (let ((gnus-tmp-name
	 (or (mm-handle-filename handle)
	     (mail-content-type-get (mm-handle-type handle) 'url)
	     ""))
	(gnus-tmp-type (mm-handle-media-type handle))
	(gnus-tmp-description (or (mm-handle-description handle) ""))
	(gnus-tmp-dots
	 (if (if displayed (car displayed)
	       (mm-handle-displayed-p handle))
	     "" "..."))
	(gnus-tmp-length (with-current-buffer (mm-handle-buffer handle)
			   (buffer-size)))
	gnus-tmp-type-long b e)
    (when (string-match ".*/" gnus-tmp-name)
      (setq gnus-tmp-name (replace-match "" t t gnus-tmp-name)))
    (setq gnus-tmp-type-long (concat gnus-tmp-type
				     (and (not (equal gnus-tmp-name ""))
					  (concat "; " gnus-tmp-name))))
    (unless (equal gnus-tmp-description "")
      (setq gnus-tmp-type-long (concat " --- " gnus-tmp-type-long)))
    (unless (bolp)
      (insert "\n"))
    (setq b (point))
    (gnus-eval-format
     gnus-mime-button-line-format gnus-mime-button-line-format-alist
     `(keymap ,gnus-mime-button-map
	      gnus-callback gnus-mm-display-part
	      gnus-part ,gnus-tmp-id
	      article-type annotation
	      gnus-data ,handle
	      rear-nonsticky t))
    (setq e (if (bolp)
		;; Exclude a newline.
		(1- (point))
	      (point)))
    (when gnus-article-button-face
      (gnus-overlay-put (gnus-make-overlay b e nil t)
			'face gnus-article-button-face))
    (widget-convert-button
     'link b e
     :mime-handle handle
     :action 'gnus-widget-press-button
     :button-keymap gnus-mime-button-map
     :help-echo
     (lambda (widget)
       ;; Needed to properly clear the message due to a bug in
       ;; wid-edit (XEmacs only).
       (if (boundp 'help-echo-owns-message)
	   (setq help-echo-owns-message t))
       (format
	"%S: %s the MIME part; %S: more options"
	(aref gnus-mouse-2 0)
	(if (mm-handle-displayed-p (widget-get widget :mime-handle))
	    "hide" "show")
	(aref gnus-down-mouse-3 0))))))

(defun gnus-widget-press-button (elems el)
  (goto-char (widget-get elems :from))
  (gnus-article-press-button))

(defvar gnus-displaying-mime nil)

(defun gnus-display-mime (&optional ihandles)
  "Display the MIME parts."
  (save-excursion
    (save-selected-window
      (let ((window (get-buffer-window gnus-article-buffer))
	    (point (point)))
	(when window
	  (select-window window)
	  ;; We have to do this since selecting the window
	  ;; may change the point.  So we set the window point.
	  (set-window-point window point)))
      (let ((handles ihandles)
	    (inhibit-read-only t)
	    handle)
	(cond (handles)
	      ((setq handles (mm-dissect-buffer nil gnus-article-loose-mime))
	       (when gnus-article-emulate-mime
		 (mm-uu-dissect-text-parts handles)))
	      (gnus-article-emulate-mime
	       (setq handles (mm-uu-dissect))))
	(when (and (not ihandles)
		   (not gnus-displaying-mime))
	  ;; Top-level call; we clean up.
	  (when gnus-article-mime-handles
	    (mm-destroy-parts gnus-article-mime-handles)
	    (setq gnus-article-mime-handle-alist nil));; A trick.
	  (setq gnus-article-mime-handles handles)
	  ;; We allow users to glean info from the handles.
	  (when gnus-article-mime-part-function
	    (gnus-mime-part-function handles)))
	(if (and handles
		 (or (not (stringp (car handles)))
		     (cdr handles)))
	    (progn
	      (when (and (not ihandles)
			 (not gnus-displaying-mime))
		;; Clean up for mime parts.
		(article-goto-body)
		(delete-region (point) (point-max)))
	      (let ((gnus-displaying-mime t))
		(gnus-mime-display-part handles)))
	  (save-restriction
	    (article-goto-body)
	    (narrow-to-region (point) (point-max))
	    (gnus-treat-article nil 1 1 "text/plain")
	    (widen)))
	(unless ihandles
	  ;; Highlight the headers.
	  (save-excursion
	    (save-restriction
	      (article-goto-body)
	      (narrow-to-region (point-min) (point))
	      (gnus-article-save-original-date
	       (gnus-treat-article 'head)))))))
    ;; Cope with broken MIME messages.
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))))

(defcustom gnus-mime-display-multipart-as-mixed nil
  "Display \"multipart\" parts as  \"multipart/mixed\".

If t, it overrides nil values of
`gnus-mime-display-multipart-alternative-as-mixed' and
`gnus-mime-display-multipart-related-as-mixed'."
  :group 'gnus-article-mime
  :type 'boolean)

(defcustom gnus-mime-display-multipart-alternative-as-mixed nil
  "Display \"multipart/alternative\" parts as  \"multipart/mixed\"."
  :version "22.1"
  :group 'gnus-article-mime
  :type 'boolean)

(defcustom gnus-mime-display-multipart-related-as-mixed nil
  "Display \"multipart/related\" parts as  \"multipart/mixed\".

If displaying \"text/html\" is discouraged \(see
`mm-discouraged-alternatives'\) images or other material inside a
\"multipart/related\" part might be overlooked when this variable is nil."
  :version "22.1"
  :group 'gnus-article-mime
  :type 'boolean)

(defun gnus-mime-display-part (handle)
  (cond
   ;; Maybe a broken MIME message.
   ((null handle))
   ;; Single part.
   ((not (stringp (car handle)))
    (gnus-mime-display-single handle))
   ;; User-defined multipart
   ((cdr (assoc (car handle) gnus-mime-multipart-functions))
    (funcall (cdr (assoc (car handle) gnus-mime-multipart-functions))
	     handle))
   ;; multipart/alternative
   ((and (equal (car handle) "multipart/alternative")
	 (not (or gnus-mime-display-multipart-as-mixed
		  gnus-mime-display-multipart-alternative-as-mixed)))
    (let ((id (1+ (length gnus-article-mime-handle-alist))))
      (push (cons id handle) gnus-article-mime-handle-alist)
      (gnus-mime-display-alternative (cdr handle) nil nil id)))
   ;; multipart/related
   ((and (equal (car handle) "multipart/related")
	 (not (or gnus-mime-display-multipart-as-mixed
		  gnus-mime-display-multipart-related-as-mixed)))
    ;;;!!!We should find the start part, but we just default
    ;;;!!!to the first part.
    ;;(gnus-mime-display-part (cadr handle))
    ;;;!!! Most multipart/related is an HTML message plus images.
    ;;;!!! Unfortunately we are unable to let W3 display those
    ;;;!!! included images, so we just display it as a mixed multipart.
    ;;(gnus-mime-display-mixed (cdr handle))
    ;;;!!! No, w3 can display everything just fine.
    (gnus-mime-display-part (cadr handle)))
   ((equal (car handle) "multipart/signed")
    (gnus-add-wash-type 'signed)
    (gnus-mime-display-security handle))
   ((equal (car handle) "multipart/encrypted")
    (gnus-add-wash-type 'encrypted)
    (gnus-mime-display-security handle))
   ;; Other multiparts are handled like multipart/mixed.
   (t
    (gnus-mime-display-mixed (cdr handle)))))

(defun gnus-mime-part-function (handles)
  (if (stringp (car handles))
      (mapcar 'gnus-mime-part-function (cdr handles))
    (funcall gnus-article-mime-part-function handles)))

(defun gnus-mime-display-mixed (handles)
  (mapcar 'gnus-mime-display-part handles))

(defun gnus-mime-display-single (handle)
  (let ((type (mm-handle-media-type handle))
	(ignored gnus-ignored-mime-types)
	(not-attachment t)
	(move nil)
	display text)
    (catch 'ignored
      (progn
	(while ignored
	  (when (string-match (pop ignored) type)
	    (throw 'ignored nil)))
	(if (and (not (and (if (gnus-buffer-live-p gnus-summary-buffer)
			       (with-current-buffer gnus-summary-buffer
				 gnus-inhibit-images)
			     gnus-inhibit-images)
			   (string-match "\\`image/" type)))
		 (setq not-attachment
		       (and (not (mm-inline-override-p handle))
			    (or (not (mm-handle-disposition handle))
				(equal (car (mm-handle-disposition handle))
				       "inline")
				(mm-attachment-override-p handle))))
		 (mm-automatic-display-p handle)
		 (or (and
		      (mm-inlinable-p handle)
		      (mm-inlined-p handle))
		     (mm-automatic-external-display-p type)))
	    (setq display t)
	  (when (equal (mm-handle-media-supertype handle) "text")
	    (setq text t)))
	(let ((id (1+ (length gnus-article-mime-handle-alist)))
	      beg)
	  (push (cons id handle) gnus-article-mime-handle-alist)
	  (when (and display
		     (equal (mm-handle-media-supertype handle) "message"))
	    (insert-char
	     ?\n
	     (cond ((not (bolp)) 2)
		   ((or (bobp) (eq (char-before (1- (point))) ?\n)) 0)
		   (t 1))))
	  (when (or (not display)
		    (not (gnus-unbuttonized-mime-type-p type))
		    (eq id gnus-mime-buttonized-part-id))
	    (gnus-insert-mime-button
	     handle id (list (or display (and not-attachment text))))
	    (gnus-article-insert-newline)
	    ;; Remember modify the number of forward lines.
	    (setq move t))
	  (setq beg (point))
	  (cond
	   (display
	    (when move
	      (forward-line -1)
	      (setq beg (point)))
	    (let ((mail-parse-charset gnus-newsgroup-charset)
		  (mail-parse-ignored-charsets
		   (save-excursion (condition-case ()
				       (set-buffer gnus-summary-buffer)
				     (error))
				   gnus-newsgroup-ignored-charsets)))
	      (gnus-bind-safe-url-regexp (mm-display-part handle t)))
	    (goto-char (point-max)))
	   ((and text not-attachment)
	    (when move
	      (forward-line -1)
	      (setq beg (point)))
	    (gnus-article-insert-newline)
	    (mm-display-inline handle)
	    (goto-char (point-max))))
	  ;; Do highlighting.
	  (save-excursion
	    (save-restriction
	      (narrow-to-region beg (point))
	      (if (eq handle gnus-article-mime-handles)
		  ;; The format=flowed case.
		  (gnus-treat-article nil 1 1 (mm-handle-media-type handle))
		;; Don't count signature parts that are never displayed.
		;; The part number should be re-calculated supposing this
		;; might be a message/rfc822 part.
		(let (handles)
		  (dolist (part gnus-article-mime-handles)
		    (unless (or (stringp part)
				(equal (car (mm-handle-type part))
				       "application/pgp-signature"))
		      (push part handles)))
		  (gnus-treat-article
		   nil (length (memq handle handles)) (length handles)
		   (mm-handle-media-type handle)))))))))))

(defun gnus-unbuttonized-mime-type-p (type)
  "Say whether TYPE is to be unbuttonized."
  (unless gnus-inhibit-mime-unbuttonizing
    (when (catch 'found
	    (let ((types gnus-unbuttonized-mime-types))
	      (while types
		(when (string-match (pop types) type)
		  (throw 'found t)))))
      (not (catch 'found
	     (let ((types gnus-buttonized-mime-types))
	       (while types
		 (when (string-match (pop types) type)
		   (throw 'found t)))))))))

(defun gnus-article-insert-newline ()
  "Insert a newline, but mark it as undeletable."
  (gnus-put-text-property
   (point) (progn (insert "\n") (point)) 'gnus-undeletable t))

(defun gnus-mime-display-alternative (handles &optional preferred ibegend id)
  (let* ((preferred (or preferred (mm-preferred-alternative handles)))
	 (ihandles handles)
	 (point (point))
	 handle (inhibit-read-only t) from props begend not-pref)
    (save-window-excursion
      (save-restriction
	(when ibegend
	  (narrow-to-region (car ibegend)
			    (or (cdr ibegend)
				(progn
				  (goto-char (car ibegend))
				  (forward-line 2)
				  (point))))
	  (delete-region (point-min) (point-max))
	  (mm-remove-parts handles))
	(setq begend (list (point-marker)))
	;; Do the toggle.
	(unless (setq not-pref (cadr (member preferred ihandles)))
	  (setq not-pref (car ihandles)))
	(when (or ibegend
		  (not preferred)
		  (not (gnus-unbuttonized-mime-type-p
			"multipart/alternative")))
	  (gnus-add-text-properties
	   (setq from (point))
	   (progn
	     (insert (format "%d.  " id))
	     (point))
	   `(gnus-callback
	     (lambda (handles)
	       (unless ,(not ibegend)
		 (setq gnus-article-mime-handle-alist
		       ',gnus-article-mime-handle-alist))
	       (gnus-mime-display-alternative
		',ihandles ',not-pref ',begend ,id))
	     keymap ,gnus-mime-button-map
	     ,gnus-mouse-face-prop ,gnus-article-mouse-face
	     face ,gnus-article-button-face
	     gnus-part ,id
	     article-type multipart
	     rear-nonsticky t))
	  (widget-convert-button 'link from (point)
				 :action 'gnus-widget-press-button
				 :button-keymap gnus-widget-button-keymap)
	  ;; Do the handles
	  (while (setq handle (pop handles))
	    (gnus-add-text-properties
	     (setq from (point))
	     (progn
	       (insert (format "(%c) %-18s"
			       (if (equal handle preferred) ?* ? )
			       (mm-handle-media-type handle)))
	       (point))
	     `(gnus-callback
	       (lambda (handles)
		 (unless ,(not ibegend)
		   (setq gnus-article-mime-handle-alist
			 ',gnus-article-mime-handle-alist))
		 (gnus-mime-display-alternative
		  ',ihandles ',handle ',begend ,id))
	       keymap ,gnus-mime-button-map
	       ,gnus-mouse-face-prop ,gnus-article-mouse-face
	       face ,gnus-article-button-face
	       gnus-part ,id
	       gnus-data ,handle
	       rear-nonsticky t))
	    (widget-convert-button 'link from (point)
				   :action 'gnus-widget-press-button
				   :button-keymap gnus-widget-button-keymap)
	    (insert "  "))
	  (insert "\n\n"))
	(when preferred
	  (if (stringp (car preferred))
	      (gnus-display-mime preferred)
	    (let ((mail-parse-charset gnus-newsgroup-charset)
		  (mail-parse-ignored-charsets
		   (with-current-buffer gnus-summary-buffer
		     gnus-newsgroup-ignored-charsets)))
	      (gnus-bind-safe-url-regexp (mm-display-part preferred))
	      ;; Do highlighting.
	      (save-excursion
		(save-restriction
		  (narrow-to-region (car begend) (point-max))
		  (gnus-treat-article
		   nil (length gnus-article-mime-handle-alist)
		   (gnus-article-mime-total-parts)
		   (mm-handle-media-type preferred))))))
	  (goto-char (point-max))
	  (setcdr begend (point-marker)))))
    (when ibegend
      (goto-char point))))

(defconst gnus-article-wash-status-strings
  (let ((alist '((cite "c" "Possible hidden citation text"
		       " " "All citation text visible")
		 (headers "h" "Hidden headers"
			  " " "All headers visible.")
		 (pgp "p" "Encrypted or signed message status hidden"
		      " " "No hidden encryption nor digital signature status")
		 (signature "s" "Signature has been hidden"
			    " " "Signature is visible")
		 (overstrike "o" "Overstrike (^H) characters applied"
			     " " "No overstrike characters applied")
		 (emphasis "e" "/*_Emphasis_*/ characters applied"
			   " " "No /*_emphasis_*/ characters applied")))
	result)
    (dolist (entry alist result)
      (let ((key (nth 0 entry))
	    (on (copy-sequence (nth 1 entry)))
	    (on-help (nth 2 entry))
	    (off (copy-sequence (nth 3 entry)))
	    (off-help (nth 4 entry)))
	(put-text-property 0 1 'help-echo on-help on)
	(put-text-property 0 1 'help-echo off-help off)
	(push (list key on off) result))))
  "Alist of strings describing wash status in the mode line.
Each entry has the form (KEY ON OF), where the KEY is a symbol
representing the particular washing function, ON is the string to use
in the article mode line when the washing function is active, and OFF
is the string to use when it is inactive.")

(defun gnus-article-wash-status-entry (key value)
  (let ((entry (assoc key gnus-article-wash-status-strings)))
    (if value (nth 1 entry) (nth 2 entry))))

(defun gnus-article-wash-status ()
  "Return a string which display status of article washing."
  (with-current-buffer gnus-article-buffer
    (let ((cite (memq 'cite gnus-article-wash-types))
	  (headers (memq 'headers gnus-article-wash-types))
	  (boring (memq 'boring-headers gnus-article-wash-types))
	  (pgp (memq 'pgp gnus-article-wash-types))
	  (pem (memq 'pem gnus-article-wash-types))
	  (signed (memq 'signed gnus-article-wash-types))
	  (encrypted (memq 'encrypted gnus-article-wash-types))
	  (signature (memq 'signature gnus-article-wash-types))
	  (overstrike (memq 'overstrike gnus-article-wash-types))
	  (emphasis (memq 'emphasis gnus-article-wash-types)))
      (concat
       (gnus-article-wash-status-entry 'cite cite)
       (gnus-article-wash-status-entry 'headers (or headers boring))
       (gnus-article-wash-status-entry 'pgp (or pgp pem signed encrypted))
       (gnus-article-wash-status-entry 'signature signature)
       (gnus-article-wash-status-entry 'overstrike overstrike)
       (gnus-article-wash-status-entry 'emphasis emphasis)))))

(defun gnus-add-wash-type (type)
  "Add a washing of TYPE to the current status."
  (add-to-list 'gnus-article-wash-types type))

(defun gnus-delete-wash-type (type)
  "Add a washing of TYPE to the current status."
  (setq gnus-article-wash-types (delq type gnus-article-wash-types)))

(defun gnus-add-image (category image)
  "Add IMAGE of CATEGORY to the list of displayed images."
  (let ((entry (assq category gnus-article-image-alist)))
    (unless entry
      (setq entry (list category))
      (push entry gnus-article-image-alist))
    (nconc entry (list image))))

(defun gnus-delete-images (category)
  "Delete all images in CATEGORY."
  (let ((entry (assq category gnus-article-image-alist)))
    (dolist (image (cdr entry))
      (gnus-remove-image image category))
    (setq gnus-article-image-alist (delq entry gnus-article-image-alist))
    (gnus-delete-wash-type category)))

(defalias 'gnus-article-hide-headers-if-wanted 'gnus-article-maybe-hide-headers)

(defun gnus-article-maybe-hide-headers ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatibility."
  (when (and (or (not (gnus-buffer-live-p gnus-summary-buffer))
		 (not (with-current-buffer gnus-summary-buffer
			gnus-have-all-headers)))
	     (not gnus-inhibit-hiding))
    (gnus-article-hide-headers)))

(declare-function shr-put-image "shr" (data alt))

(defun gnus-shr-put-image (data alt)
  "Put image DATA with a string ALT.  Enable image to be deleted."
  (let ((image (shr-put-image data (propertize (or alt "*")
					       'gnus-image-category 'shr))))
    (when image
      (gnus-add-image 'shr image))))

;;; Article savers.

(defun gnus-output-to-file (file-name)
  "Append the current article to a file named FILE-NAME.
If `gnus-article-save-coding-system' is non-nil, it is used to encode
text and used as the value of the coding cookie which is added to the
top of a file.  Otherwise, this function saves a raw article without
the coding cookie."
  (let* ((artbuf (current-buffer))
	 (file-name-coding-system nnmail-pathname-coding-system)
	 (coding gnus-article-save-coding-system)
	 (coding-system-for-read (if coding
				     nil ;; Rely on the coding cookie.
				   mm-text-coding-system))
	 (coding-system-for-write (or coding
				      mm-text-coding-system-for-write
				      mm-text-coding-system))
	 (exists (file-exists-p file-name)))
    (with-temp-buffer
      (when exists
	(insert-file-contents file-name)
	(goto-char (point-min))
	;; Remove the existing coding cookie.
	(when (looking-at "X-Gnus-Coding-System: .+\n\n")
	  (delete-region (match-beginning 0) (match-end 0))))
      (goto-char (point-max))
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (when coding
	;; If the coding system is not suitable to encode the text,
	;; ask a user for a proper one.
	(when (fboundp 'select-safe-coding-system)
	  (setq coding (coding-system-base
			(save-window-excursion
			  (select-safe-coding-system (point-min) (point-max)
						     coding))))
	  (setq coding-system-for-write
		(or (cdr (assq coding '((mule-utf-8 . utf-8))))
		    coding)))
	(goto-char (point-min))
	;; Add the coding cookie.
	(insert (format "X-Gnus-Coding-System: -*- coding: %s; -*-\n\n"
			coding-system-for-write)))
      (if exists
	  (progn
	    (write-region (point-min) (point-max) file-name nil 'no-message)
	    (message "Appended to %s" file-name))
	(write-region (point-min) (point-max) file-name))))
  t)

(defun gnus-narrow-to-page (&optional arg)
  "Narrow the article buffer to a page.
If given a numerical ARG, move forward ARG pages."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (with-current-buffer gnus-article-buffer
    (widen)
    ;; Remove any old next/prev buttons.
    (when (gnus-visual-p 'page-marker)
      (let ((inhibit-read-only t))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next)))
    (let (st nd pt)
      (when (save-excursion
	      (cond ((< arg 0)
		     (if (re-search-backward page-delimiter nil 'move (abs arg))
			 (prog1
			     (setq nd (match-beginning 0)
				   pt nd)
			   (when (re-search-backward page-delimiter nil t)
			     (setq st (match-end 0))))
		       (when (re-search-forward page-delimiter nil t)
			 (setq nd (match-beginning 0)
			       pt (point-min)))))
		    ((> arg 0)
		     (if (re-search-forward page-delimiter nil 'move arg)
			 (prog1
			     (setq st (match-end 0)
				   pt st)
			   (when (re-search-forward page-delimiter nil t)
			     (setq nd (match-beginning 0))))
		       (when (re-search-backward page-delimiter nil t)
			 (setq st (match-end 0)
			       pt (point-max)))))
		    (t
		     (when (re-search-backward page-delimiter nil t)
		       (goto-char (setq st (match-end 0))))
		     (when (re-search-forward page-delimiter nil t)
		       (setq nd (match-beginning 0)))
		     (or st nd))))
	(setq gnus-page-broken t)
	(when pt (goto-char pt))
	(narrow-to-region (or st (point-min)) (or nd (point-max)))
	(when (gnus-visual-p 'page-marker)
	  (save-excursion
	    (when nd
	      (goto-char nd)
	      (gnus-insert-next-page-button))
	    (when st
	      (goto-char st)
	      (gnus-insert-prev-page-button))))))))

;; Article mode commands

(defun gnus-article-goto-next-page ()
  "Show the next page of the article."
  (interactive)
  (when (gnus-article-next-page)
    (goto-char (point-min))
    (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))))


(defun gnus-article-goto-prev-page ()
  "Show the previous page of the article."
  (interactive)
  (if (save-restriction (widen) (bobp)) ;; Real beginning-of-buffer?
      (gnus-article-read-summary-keys nil (gnus-character-to-event ?p))
    (gnus-article-prev-page nil)))

;; This is cleaner but currently breaks `gnus-pick-mode':
;;
;; (defun gnus-article-goto-next-page ()
;;   "Show the next page of the article."
;;   (interactive)
;;   (gnus-eval-in-buffer-window gnus-summary-buffer
;;     (gnus-summary-next-page)))
;;
;; (defun gnus-article-goto-prev-page ()
;;   "Show the next page of the article."
;;   (interactive)
;;   (gnus-eval-in-buffer-window gnus-summary-buffer
;;     (gnus-summary-prev-page)))

(defun gnus-article-next-page (&optional lines)
  "Show the next page of the current article.
If end of article, return non-nil.  Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "p")
  (move-to-window-line (if (featurep 'xemacs) -1 (- -1 scroll-margin)))
  (if (and (not (and gnus-article-over-scroll
		     (> (count-lines (window-start) (point-max))
			(if (featurep 'xemacs)
			    (or lines (1- (window-height)))
			  (+ (or lines (1- (window-height))) scroll-margin)))))
	   (save-excursion
	     (end-of-line)
	     (and (pos-visible-in-window-p)	;Not continuation line.
		  (>= (point) (point-max)))))
      ;; Nothing in this page.
      (if (or (not gnus-page-broken)
	      (save-excursion
		(save-restriction
		  (widen)
		  (forward-line)
		  (eobp)))) ;Real end-of-buffer?
	  (progn
	    (when gnus-article-over-scroll
	      (gnus-article-next-page-1 lines))
	    t)			;Nothing more.
	(gnus-narrow-to-page 1)		;Go to next page.
	nil)
    ;; More in this page.
    (gnus-article-next-page-1 lines)
    nil))

(defun gnus-article-beginning-of-window ()
  "Move point to the beginning of the window.
In Emacs, the point is placed at the line number which `scroll-margin'
specifies."
  (if (featurep 'xemacs)
      (move-to-window-line 0)
    ;; There is an obscure bug in Emacs that makes it impossible to
    ;; scroll past big pictures in the article buffer.  Try to fix
    ;; this by adding a sanity check by counting the lines visible.
    (when (> (count-lines (window-start) (window-end)) 30)
      (move-to-window-line
       (min (max 0 scroll-margin)
	    (max 1 (- (window-height)
		      (if mode-line-format 1 0)
		      (if header-line-format 1 0)
		      2)))))))

(defun gnus-article-next-page-1 (lines)
  (condition-case ()
      (let ((scroll-in-place nil)
	    (auto-window-vscroll nil))
	(scroll-up lines))
    (end-of-buffer
     ;; Long lines may cause an end-of-buffer error.
     (goto-char (point-max))))
  (gnus-article-beginning-of-window))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "p")
  (move-to-window-line 0)
  (if (and gnus-page-broken
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1)	;Go to previous page.
	(goto-char (point-max))
	(recenter (if gnus-article-over-scroll
		      (if lines
			  (max (if (featurep 'xemacs)
				   lines
				 (+ lines scroll-margin))
			       3)
			(- (window-height) 2))
		    -1)))
    (prog1
	(condition-case ()
	    (let ((scroll-in-place nil))
	      (scroll-down lines))
	  (beginning-of-buffer
	   (goto-char (point-min))))
      (gnus-article-beginning-of-window))))

(defun gnus-article-only-boring-p ()
  "Decide whether there is only boring text remaining in the article.
Something \"interesting\" is a word of at least two letters that does
not have a face in `gnus-article-boring-faces'."
  (when (and gnus-article-skip-boring
	     (boundp 'gnus-article-boring-faces)
	     (symbol-value 'gnus-article-boring-faces))
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
	(catch 'only-boring
	  (while (re-search-forward "\\b\\w\\w" nil t)
	    (forward-char -1)
	    (when (not (gnus-intersection
			(gnus-faces-at (point))
			(symbol-value 'gnus-article-boring-faces)))
	      (throw 'only-boring nil)))
	  (throw 'only-boring t))))))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (save-excursion
    (re-search-backward "[ \t]\\|^" (point-at-bol) t)
    (re-search-forward "<?news:<?\\|<" (point-at-eol) t)
    (if (re-search-forward "[^@ ]+@[^ \t>]+" (point-at-eol) t)
	(let ((msg-id (concat "<" (match-string 0) ">")))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-refer-article msg-id))
      (error "No references around point"))))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (if (not (gnus-buffer-live-p gnus-summary-buffer))
      (error "There is no summary buffer for this article buffer")
    (gnus-article-set-globals)
    (gnus-configure-windows 'article)
    (gnus-summary-goto-subject gnus-current-article)
    (gnus-summary-position-point)))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6 "%s" (substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-goto-next-page]:Next page	 \\[gnus-article-goto-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

(defun gnus-article-check-buffer ()
  "Beep if not in an article buffer."
  (unless (equal major-mode 'gnus-article-mode)
    (error "Command invoked outside of a Gnus article buffer")))

(defun gnus-article-read-summary-keys (&optional arg key not-restore-window)
  "Read a summary buffer key sequence and execute it from the article buffer."
  (interactive "P")
  (gnus-article-check-buffer)
  (let ((nosaves
	 '("q" "Q"  "c" "r" "\C-c\C-f" "m"  "a" "f"
	   "Zc" "ZC" "ZE" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP"
	   "=" "^" "\M-^" "|"))
	(nosave-but-article
	 '("A " "A<" "A>" "AM" "AP" "AR" "AT" "A\C-?" "A\M-\r" "A\r" "Ab" "Ae"
	   "An" "Ap" [?A (meta return)] [?A delete]))
	(nosave-in-article
	 '("AS" "\C-d"))
	(up-to-top
	 '("n" "Gn" "p" "Gp"))
	keys new-sum-point)
    (with-current-buffer gnus-article-current-summary
      (let (gnus-pick-mode)
	(setq unread-command-events (nconc unread-command-events
					   (list (or key last-command-event)))
	      keys (if (featurep 'xemacs)
		       (events-to-keys (read-key-sequence nil t))
		     (read-key-sequence nil t)))))

    (message "")

    (cond
     ((eq (aref keys (1- (length keys))) ?\C-h)
      (gnus-article-describe-bindings (substring keys 0 -1)))
     ((or (member keys nosaves)
	  (member keys nosave-but-article)
	  (member keys nosave-in-article))
      (let (func)
	(save-window-excursion
	  (pop-to-buffer gnus-article-current-summary)
	  ;; We disable the pick minor mode commands.
	  (let (gnus-pick-mode)
	    (setq func (key-binding keys t))))
	(if (or (not func)
		(numberp func))
	    (ding)
	  (unless (member keys nosave-in-article)
	    (set-buffer gnus-article-current-summary))
	  (when (get func 'disabled)
	    (error "Function %s disabled" func))
	  (call-interactively func)
	  (setq new-sum-point (point)))
	(when (member keys nosave-but-article)
	  (pop-to-buffer gnus-article-buffer))))
     (t
      ;; These commands should restore window configuration.
      (let ((obuf (current-buffer))
	    (owin (current-window-configuration))
	    win func in-buffer selected new-sum-start new-sum-hscroll err)
	(cond (not-restore-window
	       (pop-to-buffer gnus-article-current-summary)
	       (setq win (selected-window)))
	      ((setq win (get-buffer-window gnus-article-current-summary))
	       (select-window win))
	      (t
	       (let ((summary-buffer gnus-article-current-summary))
		 (gnus-configure-windows 'article)
		 (unless (setq win (get-buffer-window summary-buffer 'visible))
		   (let ((gnus-buffer-configuration
			  '((article ((vertical 1.0
						(summary 0.25 point)
						(article 1.0)))))))
		     (gnus-configure-windows 'article))
		   (setq win (get-buffer-window summary-buffer 'visible)))
		 (gnus-select-frame-set-input-focus (window-frame win))
		 (select-window win))))
	(setq in-buffer (current-buffer))
	;; We disable the pick minor mode commands.
	(setq func (let (gnus-pick-mode)
		     (key-binding keys t)))
	(when (get func 'disabled)
	  (error "Function %s disabled" func))
	(if (and func
		 (functionp func)
		 (condition-case code
		     (progn
		       (call-interactively func)
		       t)
		   (error
		    (setq err code)
		    nil)))
	    (progn
	      (when (eq win (selected-window))
		(setq new-sum-point (point)
		      new-sum-start (window-start win)
		      new-sum-hscroll (window-hscroll win)))
	      (when (or (eq in-buffer (current-buffer))
			(when (eq obuf (current-buffer))
			  (set-buffer in-buffer)
			  t))
		(setq selected (gnus-summary-select-article))
		(set-buffer obuf)
		(unless not-restore-window
		  (set-window-configuration owin))
		(when (and (eq selected 'old)
			   new-sum-point)
		  (set-window-start (get-buffer-window (current-buffer))
				    1)
		  (set-window-point (get-buffer-window (current-buffer))
				    (if (article-goto-body)
					(1- (point))
				      (point))))
		(when (and (not not-restore-window)
			   new-sum-point
			   (window-live-p win)
			   (with-current-buffer (window-buffer win)
			     (eq major-mode 'gnus-summary-mode)))
		  (set-window-point win new-sum-point)
		  (set-window-start win new-sum-start)
		  (set-window-hscroll win new-sum-hscroll))))
	  (set-window-configuration owin)
	  (if err
	      (signal (car err) (cdr err))
	    (ding))))))))

(defun gnus-article-read-summary-send-keys ()
  (interactive)
  (let ((unread-command-events (list (gnus-character-to-event ?S))))
    (gnus-article-read-summary-keys)))

(defun gnus-article-describe-key (key)
  "Display documentation of the function invoked by KEY.
KEY is a string or a vector."
  (interactive (list (let ((cursor-in-echo-area t)) ;; better for XEmacs.
		       (read-key-sequence "Describe key: "))))
  (gnus-article-check-buffer)
  (if (memq (key-binding key t) '(gnus-article-read-summary-keys
				  gnus-article-read-summary-send-keys))
      (with-current-buffer gnus-article-current-summary
	(setq unread-command-events
	      (if (featurep 'xemacs)
		  (append key nil)
		(mapcar (lambda (x) (if (and (integerp x) (>= x 128))
					(list 'meta (- x 128))
				      x))
			key)))
	(let ((cursor-in-echo-area t)
	      gnus-pick-mode)
	  (describe-key (read-key-sequence nil t))))
    (describe-key key)))

(defun gnus-article-describe-key-briefly (key &optional insert)
  "Display documentation of the function invoked by KEY.
KEY is a string or a vector."
  (interactive (list (let ((cursor-in-echo-area t)) ;; better for XEmacs.
		       (read-key-sequence "Describe key: "))
		     current-prefix-arg))
  (gnus-article-check-buffer)
  (if (memq (key-binding key t) '(gnus-article-read-summary-keys
				  gnus-article-read-summary-send-keys))
      (with-current-buffer gnus-article-current-summary
	(setq unread-command-events
	      (if (featurep 'xemacs)
		  (append key nil)
		(mapcar (lambda (x) (if (and (integerp x) (>= x 128))
					(list 'meta (- x 128))
				      x))
			key)))
	(let ((cursor-in-echo-area t)
	      gnus-pick-mode)
	  (describe-key-briefly (read-key-sequence nil t) insert)))
    (describe-key-briefly key insert)))

;;`gnus-agent-mode' in gnus-agent.el will define it.
(defvar gnus-agent-summary-mode)
(defvar gnus-draft-mode)
;; Calling help-buffer will autoload help-mode.
(defvar help-xref-stack-item)
;; Emacs 22 doesn't load it in the batch mode.
(eval-when-compile
  (autoload 'help-buffer "help-mode"))

(defun gnus-article-describe-bindings (&optional prefix)
  "Show a list of all defined keys, and their definitions.
The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix."
  (interactive)
  (gnus-article-check-buffer)
  (let ((keymap (copy-keymap gnus-article-mode-map))
	(map (copy-keymap gnus-article-send-map))
	(sumkeys (where-is-internal 'gnus-article-read-summary-keys))
	parent agent draft)
    (define-key keymap "S" map)
    (define-key map [t] nil)
    (with-current-buffer gnus-article-current-summary
      (set-keymap-parent
       keymap
       (if (setq parent (keymap-parent gnus-article-mode-map))
	   (prog1
	       (setq parent (copy-keymap parent))
	     (set-keymap-parent parent (current-local-map)))
	 (current-local-map)))
      (set-keymap-parent map (key-binding "S"))
      (let (key def gnus-pick-mode)
	(while sumkeys
	  (setq key (pop sumkeys))
	  (cond ((and (vectorp key) (= (length key) 1)
		      (consp (setq def (aref key 0)))
		      (numberp (car def)) (numberp (cdr def)))
		 (when (< (max (car def) (cdr def)) 128)
		   (setq sumkeys
			 (append (mapcar
				  #'vector
				  (nreverse (gnus-uncompress-range def)))
				 sumkeys))))
		((setq def (key-binding key))
		 (unless (eq def 'undefined)
		   (define-key keymap key def))))))
      (when (boundp 'gnus-agent-summary-mode)
	(setq agent gnus-agent-summary-mode))
      (when (boundp 'gnus-draft-mode)
	(setq draft gnus-draft-mode)))
    (with-temp-buffer
      (use-local-map keymap)
      (set (make-local-variable 'gnus-agent-summary-mode) agent)
      (set (make-local-variable 'gnus-draft-mode) draft)
      (describe-bindings prefix))
    (let ((item `((lambda (prefix)
		    (with-current-buffer ,(current-buffer)
		      (gnus-article-describe-bindings prefix)))
		  ,prefix)))
      (with-current-buffer (let (help-xref-following) (help-buffer))
	(setq help-xref-stack-item item)))))

(defun gnus-article-reply-with-original (&optional wide)
  "Start composing a reply mail to the current message.
The text in the region will be yanked.  If the region isn't active,
the entire article will be yanked."
  (interactive)
  (let ((article (cdr gnus-article-current))
	contents)
    (if (not (gnus-region-active-p))
	(with-current-buffer gnus-summary-buffer
	  (gnus-summary-reply (list (list article)) wide))
      (setq contents (buffer-substring (point) (mark t)))
      ;; Deactivate active regions.
      (when (and (boundp 'transient-mark-mode)
		 transient-mark-mode)
	(setq mark-active nil))
      (with-current-buffer gnus-summary-buffer
	(gnus-summary-reply
	 (list (list article contents)) wide)))))

(defun gnus-article-wide-reply-with-original ()
  "Start composing a wide reply mail to the current message.
The text in the region will be yanked.  If the region isn't active,
the entire article will be yanked."
  (interactive)
  (gnus-article-reply-with-original t))

(defun gnus-article-followup-with-original ()
  "Compose a followup to the current article.
The text in the region will be yanked.  If the region isn't active,
the entire article will be yanked."
  (interactive)
  (let ((article (cdr gnus-article-current))
	contents)
      (if (not (gnus-region-active-p))
	  (with-current-buffer gnus-summary-buffer
	    (gnus-summary-followup (list (list article))))
	(setq contents (buffer-substring (point) (mark t)))
	;; Deactivate active regions.
	(when (and (boundp 'transient-mark-mode)
		   transient-mark-mode)
	  (setq mark-active nil))
	(with-current-buffer gnus-summary-buffer
	  (gnus-summary-followup
	   (list (list article contents)))))))

(defun gnus-article-hide (&optional arg force)
  "Hide all the gruft in the current article.
This means that signatures, cited text and (some) headers will be
hidden.
If given a prefix, show the hidden text instead."
  (interactive (append (gnus-article-hidden-arg) (list 'force)))
  (gnus-article-hide-headers arg)
  (gnus-article-hide-list-identifiers arg)
  (gnus-article-hide-citation-maybe arg force)
  (gnus-article-hide-signature arg))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if article highlighting is requested."
  (when (gnus-visual-p 'article-highlight 'highlight)
    (gnus-article-highlight-some)))

(defun gnus-check-group-server ()
  ;; Make sure the connection to the server is alive.
  (unless (gnus-server-opened
	   (gnus-find-method-for-group gnus-newsgroup-name))
    (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
    (gnus-request-group gnus-newsgroup-name t)))

(eval-when-compile
  (autoload 'nneething-get-file-name "nneething"))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer."
  (let (do-update-line sparse-header)
    (prog1
	(save-excursion
	  (erase-buffer)
	  (gnus-kill-all-overlays)
	  (setq group (or group gnus-newsgroup-name))

	  ;; Using `gnus-request-article' directly will insert the article into
	  ;; `nntp-server-buffer' - so we'll save some time by not having to
	  ;; copy it from the server buffer into the article buffer.

	  ;; We only request an article by message-id when we do not have the
	  ;; headers for it, so we'll have to get those.
	  (when (stringp article)
	    (gnus-read-header article))

	  ;; If the article number is negative, that means that this article
	  ;; doesn't belong in this newsgroup (possibly), so we find its
	  ;; message-id and request it by id instead of number.
	  (when (and (numberp article)
		     gnus-summary-buffer
		     (get-buffer gnus-summary-buffer)
		     (gnus-buffer-exists-p gnus-summary-buffer))
	    (with-current-buffer gnus-summary-buffer
	      (let ((header (gnus-summary-article-header article)))
		(when (< article 0)
		  (cond
		   ((memq article gnus-newsgroup-sparse)
		    ;; This is a sparse gap article.
		    (setq do-update-line article)
		    (setq article (mail-header-id header))
		    (setq sparse-header (gnus-read-header article))
		    (setq gnus-newsgroup-sparse
			  (delq article gnus-newsgroup-sparse)))
		   ((vectorp header)
		    ;; It's a real article.
		    (setq article (mail-header-id header)))
		   (t
		    ;; It is an extracted pseudo-article.
		    (setq article 'pseudo)
		    (gnus-request-pseudo-article header))))

		(let ((method (gnus-find-method-for-group
			       gnus-newsgroup-name)))
		  (when (and (eq (car method) 'nneething)
			     (vectorp header))
		    (let ((dir (nneething-get-file-name
				(mail-header-id header))))
		      (when (and (stringp dir)
				 (file-directory-p dir))
			(setq article 'nneething)
			(gnus-group-enter-directory dir))))))))

	  (cond
	   ;; Refuse to select canceled articles.
	   ((and (numberp article)
		 gnus-summary-buffer
		 (get-buffer gnus-summary-buffer)
		 (gnus-buffer-exists-p gnus-summary-buffer)
		 (eq (cdr (with-current-buffer gnus-summary-buffer
			    (assq article gnus-newsgroup-reads)))
		     gnus-canceled-mark))
	    nil)
	   ;; We first check `gnus-original-article-buffer'.
	   ((and (get-buffer gnus-original-article-buffer)
		 (numberp article)
		 (with-current-buffer gnus-original-article-buffer
		   (and (equal (car gnus-original-article) group)
			(eq (cdr gnus-original-article) article))))
            ;; `insert-buffer-substring' would incorrectly use the
            ;; equivalent of string-make-multibyte which amount to decoding
            ;; with locale-coding-system, causing failure of
            ;; subsequent decoding.
            (insert (mm-string-to-multibyte
                     (with-current-buffer gnus-original-article-buffer
                       (buffer-substring (point-min) (point-max)))))
	    'article)
	   ;; Check the backlog.
	   ((and gnus-keep-backlog
		 (gnus-backlog-request-article group article (current-buffer)))
	    'article)
	   ;; Check asynchronous pre-fetch.
	   ((gnus-async-request-fetched-article group article (current-buffer))
	    (gnus-async-prefetch-next group article gnus-summary-buffer)
	    (when (and (numberp article) gnus-keep-backlog)
	      (gnus-backlog-enter-article group article (current-buffer)))
	    'article)
	   ;; Check the cache.
	   ((and gnus-use-cache
		 (numberp article)
		 (gnus-cache-request-article article group))
	    'article)
	   ;; Check the agent cache.
	   ((gnus-agent-request-article article group)
	    'article)
	   ;; Get the article and put into the article buffer.
	   ((or (stringp article)
		(numberp article))
	    (let ((gnus-override-method gnus-override-method)
		  (methods (and (stringp article)
				(with-current-buffer gnus-summary-buffer
				  (gnus-refer-article-methods))))
		  (backend (car (gnus-find-method-for-group
				 gnus-newsgroup-name)))
		  result
		  (inhibit-read-only t))
	      (when (and (null gnus-override-method)
			 methods)
		(setq gnus-override-method (pop methods)))
	      (while (not result)
		(erase-buffer)
		(gnus-kill-all-overlays)
		(let ((gnus-newsgroup-name group))
		  (gnus-check-group-server))
		(cond
		 ((gnus-request-article article group (current-buffer))
		  (when (numberp article)
		    (gnus-async-prefetch-next group article
					      gnus-summary-buffer)
		    (when gnus-keep-backlog
		      (gnus-backlog-enter-article
		       group article (current-buffer)))
		    (when (and gnus-agent
			       (gnus-agent-group-covered-p group))
		      (gnus-agent-store-article article group)))
		  (setq result 'article))
		 (methods
		  (setq gnus-override-method (pop methods)))
		 ((not (string-match "^400 "
				     (nnheader-get-report backend)))
		  ;; If we get 400 server disconnect, reconnect and
		  ;; retry; otherwise, assume the article has expired.
		  (setq result 'done))))
	      (and (eq result 'article) 'article)))
	   ;; It was a pseudo.
	   (t article)))

      ;; Associate this article with the current summary buffer.
      (setq gnus-article-current-summary gnus-summary-buffer)

      ;; Take the article from the original article buffer
      ;; and place it in the buffer it's supposed to be in.
      (when (and (get-buffer gnus-article-buffer)
		 (equal (buffer-name (current-buffer))
			(buffer-name (get-buffer gnus-article-buffer))))
	(save-excursion
	  (if (get-buffer gnus-original-article-buffer)
	      (set-buffer gnus-original-article-buffer)
	    (set-buffer (gnus-get-buffer-create gnus-original-article-buffer))
	    (buffer-disable-undo)
	    (setq major-mode 'gnus-original-article-mode)
	    (setq buffer-read-only t))
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert-buffer-substring gnus-article-buffer))
	  (setq gnus-original-article (cons group article)))

	;; Decode charsets.
	(run-hooks 'gnus-article-decode-hook)
	;; Mark article as decoded or not.
	(setq gnus-article-decoded-p gnus-article-decode-hook))

      ;; Update sparse articles.
      (when (and do-update-line
		 (or (numberp article)
		     (stringp article)))
	(let ((buf (current-buffer)))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-update-article do-update-line sparse-header)
	  (gnus-summary-goto-subject do-update-line nil t)
	  (set-window-point (gnus-get-buffer-window (current-buffer) t)
			    (point))
	  (set-buffer buf))))))

(defun gnus-block-private-groups (group)
  (if (gnus-news-group-p group)
      ;; Block nothing in news groups.
      nil
    ;; Block everything anywhere else.
    "."))

(defun gnus-blocked-images ()
  (if (functionp gnus-blocked-images)
      (funcall gnus-blocked-images gnus-newsgroup-name)
    gnus-blocked-images))

;;;
;;; Article editing
;;;

(defcustom gnus-article-edit-mode-hook nil
  "Hook run in article edit mode buffers."
  :group 'gnus-article-various
  :type 'hook)

(defvar gnus-article-edit-done-function nil)

(defvar gnus-article-edit-mode-map nil)

;; Should we be using derived.el for this?
(unless gnus-article-edit-mode-map
  (setq gnus-article-edit-mode-map (make-keymap))
  (set-keymap-parent gnus-article-edit-mode-map text-mode-map)

  (gnus-define-keys gnus-article-edit-mode-map
    "\C-c?"    describe-mode
    "\C-c\C-c" gnus-article-edit-done
    "\C-c\C-k" gnus-article-edit-exit
    "\C-c\C-f\C-t" message-goto-to
    "\C-c\C-f\C-o" message-goto-from
    "\C-c\C-f\C-b" message-goto-bcc
    ;;"\C-c\C-f\C-w" message-goto-fcc
    "\C-c\C-f\C-c" message-goto-cc
    "\C-c\C-f\C-s" message-goto-subject
    "\C-c\C-f\C-r" message-goto-reply-to
    "\C-c\C-f\C-n" message-goto-newsgroups
    "\C-c\C-f\C-d" message-goto-distribution
    "\C-c\C-f\C-f" message-goto-followup-to
    "\C-c\C-f\C-m" message-goto-mail-followup-to
    "\C-c\C-f\C-k" message-goto-keywords
    "\C-c\C-f\C-u" message-goto-summary
    "\C-c\C-f\C-i" message-insert-or-toggle-importance
    "\C-c\C-f\C-a" message-generate-unsubscribed-mail-followup-to
    "\C-c\C-b" message-goto-body
    "\C-c\C-i" message-goto-signature

    "\C-c\C-t" message-insert-to
    "\C-c\C-n" message-insert-newsgroups
    "\C-c\C-o" message-sort-headers
    "\C-c\C-e" message-elide-region
    "\C-c\C-v" message-delete-not-region
    "\C-c\C-z" message-kill-to-signature
    "\M-\r" message-newline-and-reformat
    "\C-c\C-a" mml-attach-file
    "\C-a" message-beginning-of-line
    "\t" message-tab
    "\M-;" comment-region)

  (gnus-define-keys (gnus-article-edit-wash-map
		     "\C-c\C-w" gnus-article-edit-mode-map)
    "f" gnus-article-edit-full-stops))

(easy-menu-define
  gnus-article-edit-mode-field-menu gnus-article-edit-mode-map ""
  '("Field"
    ["Fetch To" message-insert-to t]
    ["Fetch Newsgroups" message-insert-newsgroups t]
    "----"
    ["To" message-goto-to t]
    ["From" message-goto-from t]
    ["Subject" message-goto-subject t]
    ["Cc" message-goto-cc t]
    ["Reply-To" message-goto-reply-to t]
    ["Summary" message-goto-summary t]
    ["Keywords" message-goto-keywords t]
    ["Newsgroups" message-goto-newsgroups t]
    ["Followup-To" message-goto-followup-to t]
    ["Mail-Followup-To" message-goto-mail-followup-to t]
    ["Distribution" message-goto-distribution t]
    ["Body" message-goto-body t]
    ["Signature" message-goto-signature t]))

(define-derived-mode gnus-article-edit-mode message-mode "Article Edit"
  "Major mode for editing articles.
This is an extended text-mode.

\\{gnus-article-edit-mode-map}"
  (make-local-variable 'gnus-article-edit-done-function)
  (make-local-variable 'gnus-prev-winconf)
  (set (make-local-variable 'font-lock-defaults)
       '(message-font-lock-keywords t))
  (set (make-local-variable 'mail-header-separator) "")
  (set (make-local-variable 'gnus-article-edit-mode) t)
  (easy-menu-add message-mode-field-menu message-mode-map)
  (mml-mode)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (widen))

(defun gnus-article-edit (&optional force)
  "Edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (when (and (not force)
	     (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing"))
  (gnus-article-date-original)
  (gnus-article-edit-article
   'ignore
   `(lambda (no-highlight)
      'ignore
      (gnus-summary-edit-article-done
       ,(or (mail-header-references gnus-current-headers) "")
       ,(gnus-group-read-only-p) ,gnus-summary-buffer no-highlight))))

(defun gnus-article-edit-article (start-func exit-func &optional quiet)
  "Start editing the contents of the current article buffer."
  (let ((winconf (current-window-configuration)))
    (set-buffer gnus-article-buffer)
    (let ((message-auto-save-directory
	   ;; Don't associate the article buffer with a draft file.
	   nil))
      (gnus-article-edit-mode))
    (funcall start-func)
    (set-buffer-modified-p nil)
    (gnus-configure-windows 'edit-article)
    (setq gnus-article-edit-done-function exit-func)
    (setq gnus-prev-winconf winconf)
    (unless quiet
      (gnus-message 6 "C-c C-c to end edits"))))

(defun gnus-article-edit-done (&optional arg)
  "Update the article edits and exit."
  (interactive "P")
  (let ((func gnus-article-edit-done-function)
	(buf (current-buffer))
	(start (window-start))
	(p (point))
	(winconf gnus-prev-winconf))
    (widen) ;; Widen it in case that users narrowed the buffer.
    (funcall func arg)
    (set-buffer buf)
    ;; The cache and backlog have to be flushed somewhat.
    (when gnus-keep-backlog
      (gnus-backlog-remove-article
       (car gnus-article-current) (cdr gnus-article-current)))
    ;; Flush original article as well.
    (gnus-flush-original-article-buffer)
    (when gnus-use-cache
      (gnus-cache-update-article
       (car gnus-article-current) (cdr gnus-article-current)))
    ;; We remove all text props from the article buffer.
    (kill-all-local-variables)
    (set-text-properties (point-min) (point-max) nil)
    (gnus-article-mode)
    (set-window-configuration winconf)
    (set-buffer buf)
    (set-window-start (get-buffer-window buf) start)
    (set-window-point (get-buffer-window buf) (point)))
  (gnus-summary-show-article))

(defun gnus-flush-original-article-buffer ()
  (when (get-buffer gnus-original-article-buffer)
    (with-current-buffer gnus-original-article-buffer
      (setq gnus-original-article nil))))

(defun gnus-article-edit-exit ()
  "Exit the article editing without updating."
  (interactive)
  (when (or (not (buffer-modified-p))
	    (yes-or-no-p "Article modified; kill anyway? "))
    (let ((curbuf (current-buffer))
	  (p (point))
	  (window-start (window-start)))
      (erase-buffer)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (insert-buffer-substring gnus-original-article-buffer))
      (let ((winconf gnus-prev-winconf))
	(kill-all-local-variables)
	(gnus-article-mode)
	(set-window-configuration winconf)
	;; Tippy-toe some to make sure that point remains where it was.
	(save-current-buffer
	  (set-buffer curbuf)
	  (set-window-start (get-buffer-window (current-buffer)) window-start)
	  (goto-char p))))
    (gnus-summary-show-article)))

(defun gnus-article-edit-full-stops ()
  "Interactively repair spacing at end of sentences."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^$" nil t)
    (let ((case-fold-search nil))
      (query-replace-regexp "\\([.!?][])}]* \\)\\([[({A-Z]\\)" "\\1 \\2"))))

;;;
;;; Article highlights
;;;

;; Written by Per Abrahamsen <abraham@iesd.auc.dk>.

;;; Internal Variables:

(defcustom gnus-button-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (if (string-match "[[:digit:]]" "1") ;; Support POSIX?
       (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	     (punct "!?:;.,"))
	 (concat
	  "\\(?:"
	  ;; Match paired parentheses, e.g. in Wikipedia URLs:
	  ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
	  "[" chars punct "]+" "(" "[" chars punct "]+" "[" chars "]*)" "[" chars "]*"
	  "\\|"
	  "[" chars punct     "]+" "[" chars "]"
	  "\\)"))
     (concat ;; XEmacs 21.4 doesn't support POSIX.
      "\\([-a-z0-9_=!?#$@~%&*+\\/:;.,]\\|\\w\\)+"
      "\\([-a-z0-9_=#$@~%&*+\\/]\\|\\w\\)"))
   "\\)")
  "Regular expression that matches URLs."
  :group 'gnus-article-buttons
  :type 'regexp)

(defcustom gnus-button-valid-fqdn-regexp
  message-valid-fqdn-regexp
  "Regular expression that matches a valid FQDN."
  :version "22.1"
  :group 'gnus-article-buttons
  :type 'regexp)

;; Regexp suggested by Felix Wiemann in <87oeuomcz9.fsf@news2.ososo.de>
(defcustom gnus-button-valid-localpart-regexp
  "[a-z0-9$%(*-=?[_][^<>\")!;:,{}\n\t @]*"
  "Regular expression that matches a localpart of mail addresses or MIDs."
  :version "22.1"
  :group 'gnus-article-buttons
  :type 'regexp)

(defcustom gnus-button-man-handler 'manual-entry
  "Function to use for displaying man pages.
The function must take at least one argument with a string naming the
man page."
  :version "22.1"
  :type '(choice (function-item :tag "Man" manual-entry)
		 (function-item :tag "Woman" woman)
		 (function :tag "Other"))
  :group 'gnus-article-buttons)

(defcustom gnus-button-mid-or-mail-regexp
  (concat "\\b\\(<?" gnus-button-valid-localpart-regexp "@"
	  gnus-button-valid-fqdn-regexp
	  ">?\\)\\b")
  "Regular expression that matches a message ID or a mail address."
  :version "22.1"
  :group 'gnus-article-buttons
  :type 'regexp)

(defcustom gnus-button-prefer-mid-or-mail 'gnus-button-mid-or-mail-heuristic
  "What to do when the button on a string as \"foo123@bar.invalid\" is pushed.
Strings like this can be either a message ID or a mail address.  If it is one
of the symbols `mid' or `mail', Gnus will always assume that the string is a
message ID or a mail address, respectively.  If this variable is set to the
symbol `ask', always query the user what do do.  If it is a function, this
function will be called with the string as its only argument.  The function
must return `mid', `mail', `invalid' or `ask'."
  :version "22.1"
  :group 'gnus-article-buttons
  :type '(choice (function-item :tag "Heuristic function"
				gnus-button-mid-or-mail-heuristic)
		 (const ask)
		 (const mid)
		 (const mail)))

(defcustom gnus-button-mid-or-mail-heuristic-alist
  '((-10.0 . ".+\\$.+@")
    (-10.0 . "#")
    (-10.0 . "\\*")
    (-5.0  . "\\+[^+]*\\+.*@") ;; # two plus signs
    (-5.0  . "@[Nn][Ee][Ww][Ss]") ;; /\@news/i
    (-5.0  . "@.*[Dd][Ii][Aa][Ll][Uu][Pp]") ;; /\@.*dialup/i;
    (-1.0  . "^[^a-z]+@")
    ;;
    (-5.0  . "\\.[0-9][0-9]+.*@") ;; "\.[0-9]{2,}.*\@"
    (-5.0  . "[a-z].*[A-Z].*[a-z].*[A-Z].*@") ;; "([a-z].*[A-Z].*){2,}\@"
    (-3.0  . "[A-Z][A-Z][a-z][a-z].*@")
    (-5.0  . "\\...?.?@") ;; (-5.0 . "\..{1,3}\@")
    ;;
    (-2.0  . "^[0-9]")
    (-1.0  . "^[0-9][0-9]")
    ;;
    ;; -3.0 /^[0-9][0-9a-fA-F]{2,2}/;
    (-3.0  . "^[0-9][0-9a-fA-F][0-9a-fA-F][^0-9a-fA-F]")
    ;; -5.0 /^[0-9][0-9a-fA-F]{3,3}/;
    (-5.0  . "^[0-9][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][^0-9a-fA-F]")
    ;;
    (-3.0  .  "[0-9][0-9][0-9][0-9][0-9][^0-9].*@") ;; "[0-9]{5,}.*\@"
    (-3.0  .  "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][^0-9].*@")
    ;;       "[0-9]{8,}.*\@"
    (-3.0
     . "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].*@")
    ;; "[0-9]{12,}.*\@"
    ;; compensation for TDMA dated mail addresses:
    (25.0  . "-dated-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]+.*@")
    ;;
    (-20.0 . "\\.fsf@")	;; Gnus
    (-20.0 . "^slrn")
    (-20.0 . "^Pine")
    (-20.0 . "^alpine\\.")
    (-20.0 . "_-_") ;; Subject change in thread
    ;;
    (-20.0 . "\\.ln@") ;; leafnode
    (-30.0 . "@ID-[0-9]+\\.[a-zA-Z]+\\.dfncis\\.de")
    (-30.0 . "@4[Aa][Xx]\\.com") ;; Forte Agent
    ;;
    ;; (5.0 . "") ;; $local_part_len <= 7
    (10.0  . "^[^0-9]+@")
    (3.0   . "^[^0-9]+[0-9][0-9]?[0-9]?@")
    ;;      ^[^0-9]+[0-9]{1,3}\@ digits only at end of local part
    (3.0   . "\@stud")
    ;;
    (2.0   . "[a-z][a-z][._-][A-Z][a-z].*@")
    ;;
    (0.5   . "^[A-Z][a-z]")
    (0.5   . "^[A-Z][a-z][a-z]")
    (1.5   . "^[A-Z][a-z][A-Z][a-z][^a-z]") ;; ^[A-Z][a-z]{3,3}
    (2.0   . "^[A-Z][a-z][A-Z][a-z][a-z][^a-z]")) ;; ^[A-Z][a-z]{4,4}
  "An alist of \(RATE . REGEXP\) pairs for `gnus-button-mid-or-mail-heuristic'.

A negative RATE indicates a message IDs, whereas a positive indicates a mail
address.  The REGEXP is processed with `case-fold-search' set to nil."
  :version "22.1"
  :group 'gnus-article-buttons
  :type '(repeat (cons (number :tag "Rate")
		       (regexp :tag "Regexp"))))

(defun gnus-button-mid-or-mail-heuristic (mid-or-mail)
  "Guess whether MID-OR-MAIL is a message ID or a mail address.
Returns `mid' if MID-OR-MAIL is a message IDs, `mail' if it's a mail
address, `ask' if unsure and `invalid' if the string is invalid."
  (let ((case-fold-search nil)
	(list gnus-button-mid-or-mail-heuristic-alist)
	(result 0) rate regexp lpartlen elem)
    (setq lpartlen
	  (length (gnus-replace-in-string mid-or-mail "^\\(.*\\)@.*$" "\\1")))
    (gnus-message 8 "`%s', length of local part=`%s'." mid-or-mail lpartlen)
    ;; Certain special cases...
    (when (string-match
	   (concat
	    "^0[0-9]+-[0-9][0-9][0-9][0-9]@t-online\\.de$\\|"
	    "^[0-9]+\\.[0-9]+@compuserve\\|"
	    "@public\\.gmane\\.org")
	   mid-or-mail)
      (gnus-message 8 "`%s' is a known mail address." mid-or-mail)
      (setq result 'mail))
    (when (string-match "@.*@\\| " mid-or-mail)
      (gnus-message 8 "`%s' is invalid." mid-or-mail)
      (setq result 'invalid))
    ;; Nothing more to do, if result is not a number here...
    (when (numberp result)
      (while list
	(setq elem (car list)
	      rate (car elem)
	      regexp (cdr elem)
	      list (cdr list))
	(when (string-match regexp mid-or-mail)
	  (setq result (+ result rate))
	  (gnus-message
	   9 "`%s' matched `%s', rate `%s', result `%s'."
	   mid-or-mail regexp rate result)))
      (when (<= lpartlen 7)
	(setq result (+ result 5.0))
	(gnus-message 9 "`%s' matched (<= lpartlen 7), result `%s'."
		      mid-or-mail result))
      (when (>= lpartlen 12)
	(gnus-message 9 "`%s' matched (>= lpartlen 12)" mid-or-mail)
	(cond
	 ((string-match "[0-9][^0-9]+[0-9].*@" mid-or-mail)
	  ;; Long local part should contain realname if e-mail address,
	  ;; too many digits: message-id.
	  ;; $score -= 5.0 + 0.1 * $local_part_len;
	  (setq rate (* -1.0 (+ 5.0 (* 0.1 lpartlen))))
	  (setq result (+ result rate))
	  (gnus-message
	   9 "Many digits in `%s', rate `%s', result `%s'."
	   mid-or-mail rate result))
	 ((string-match "[^aeiouy][^aeiouy][^aeiouy][^aeiouy]+.*\@"
			mid-or-mail)
	  ;; Too few vowels [^aeiouy]{4,}.*\@
	  (setq result (+ result -5.0))
	  (gnus-message
	   9 "Few vowels in `%s', rate `%s', result `%s'."
	   mid-or-mail -5.0 result))
	 (t
	  (setq result (+ result 5.0))
	  (gnus-message
	   9 "`%s', rate `%s', result `%s'." mid-or-mail 5.0 result)))))
    (gnus-message 8 "`%s': Final rate is `%s'." mid-or-mail result)
    ;; Maybe we should make this a customizable alist: (condition . 'result)
    (cond
     ((symbolp result) result)
     ;; Now convert number into proper results:
     ((< result -10.0) 'mid)
     ((> result  10.0) 'mail)
     (t 'ask))))

(defun gnus-button-handle-mid-or-mail (mid-or-mail)
  (let* ((pref gnus-button-prefer-mid-or-mail) guessed
	 (url-mid (concat "news" ":" mid-or-mail))
	 (url-mailto (concat "mailto" ":" mid-or-mail)))
    (gnus-message 9 "mid-or-mail=%s" mid-or-mail)
    (when (fboundp pref)
      (setq guessed
	    ;; get rid of surrounding angles...
	    (funcall pref
		     (gnus-replace-in-string mid-or-mail "^<\\|>$" "")))
      (if (or (eq 'mid guessed) (eq 'mail guessed))
	  (setq pref guessed)
	(setq pref 'ask)))
    (if (eq pref 'ask)
	(save-window-excursion
	  (if (y-or-n-p (concat "Is <" mid-or-mail "> a mail address? "))
	      (setq pref 'mail)
	    (setq pref 'mid))))
    (cond ((eq pref 'mid)
	   (gnus-message 8 "calling `gnus-button-handle-news' %s" url-mid)
	   (gnus-button-handle-news url-mid))
	  ((eq pref 'mail)
	   (gnus-message 8 "calling `gnus-url-mailto'  %s" url-mailto)
	   (gnus-url-mailto url-mailto))
	  (t (gnus-message 3 "Invalid string.")))))

(defun gnus-button-handle-custom (fun arg)
  "Call function FUN on argument ARG.
Both FUN and ARG are supposed to be strings.  ARG will be passed
as a symbol to FUN."
  (funcall (intern fun)
	   (if (string-match "^customize-apropos" fun)
	       arg
	     (intern arg))))

(defvar gnus-button-handle-describe-prefix "^\\(C-h\\|<?[Ff]1>?\\)")

(defun gnus-button-handle-describe-function (url)
  "Call `describe-function' when pushing the corresponding URL button."
  (describe-function
   (intern
    (gnus-replace-in-string url gnus-button-handle-describe-prefix ""))))

(defun gnus-button-handle-describe-variable (url)
  "Call `describe-variable' when pushing the corresponding URL button."
  (describe-variable
   (intern
    (gnus-replace-in-string url gnus-button-handle-describe-prefix ""))))

(defun gnus-button-handle-symbol (url)
"Display help on variable or function.
Calls `describe-variable' or `describe-function'."
  (let ((sym (intern url)))
    (cond
     ((fboundp sym) (describe-function sym))
     ((boundp sym) (describe-variable sym))
     (t (gnus-message 3 "`%s' is not a known function of variable." url)))))

(defun gnus-button-handle-describe-key (url)
  "Call `describe-key' when pushing the corresponding URL button."
  (let* ((key-string
	  (gnus-replace-in-string url gnus-button-handle-describe-prefix ""))
	 (keys (ignore-errors (eval `(kbd ,key-string)))))
    (if keys
	(describe-key keys)
      (gnus-message 3 "Invalid key sequence in button: %s" key-string))))

(defun gnus-button-handle-apropos (url)
  "Call `apropos' when pushing the corresponding URL button."
  (apropos (gnus-replace-in-string url gnus-button-handle-describe-prefix "")))

(defun gnus-button-handle-apropos-command (url)
  "Call `apropos' when pushing the corresponding URL button."
  (apropos-command
   (gnus-replace-in-string url gnus-button-handle-describe-prefix "")))

(defun gnus-button-handle-apropos-variable (url)
  "Call `apropos' when pushing the corresponding URL button."
  (funcall
   (if (fboundp 'apropos-variable) 'apropos-variable 'apropos)
   (gnus-replace-in-string url gnus-button-handle-describe-prefix "")))

(defun gnus-button-handle-apropos-documentation (url)
  "Call `apropos' when pushing the corresponding URL button."
  (funcall
   (if (fboundp 'apropos-documentation) 'apropos-documentation 'apropos)
   (gnus-replace-in-string url gnus-button-handle-describe-prefix "")))

(defun gnus-button-handle-library (url)
  "Call `locate-library' when pushing the corresponding URL button."
  (gnus-message 9 "url=`%s'" url)
  (let* ((lib (locate-library url))
	 (file (gnus-replace-in-string (or lib "") "\.elc" ".el")))
    (if (not lib)
	(gnus-message 1 "Cannot locale library `%s'." url)
      (find-file-read-only file))))

(defcustom gnus-button-man-level 5
  "*Integer that says how many man-related buttons Gnus will show.
The higher the number, the more buttons will appear and the more false
positives are possible.  Note that you can set this variable local to
specific groups.  Setting it higher in Unix groups is probably a good idea.
See Info node `(gnus)Group Parameters' and the variable `gnus-parameters' on
how to set variables in specific groups."
  :version "22.1"
  :group 'gnus-article-buttons
  :link '(custom-manual "(gnus)Group Parameters")
  :type 'integer)

(defcustom gnus-button-emacs-level 5
  "*Integer that says how many emacs-related buttons Gnus will show.
The higher the number, the more buttons will appear and the more false
positives are possible.  Note that you can set this variable local to
specific groups.  Setting it higher in Emacs or Gnus related groups is
probably a good idea.  See Info node `(gnus)Group Parameters' and the variable
`gnus-parameters' on how to set variables in specific groups."
  :version "22.1"
  :group 'gnus-article-buttons
  :link '(custom-manual "(gnus)Group Parameters")
  :type 'integer)

(defcustom gnus-button-message-level 5
  "*Integer that says how many buttons for news or mail messages will appear.
The higher the number, the more buttons will appear and the more false
positives are possible."
  ;; mail addresses, MIDs, URLs for news, ...
  :version "22.1"
  :group 'gnus-article-buttons
  :type 'integer)

(defcustom gnus-button-browse-level 5
  "*Integer that says how many buttons for browsing will appear.
The higher the number, the more buttons will appear and the more false
positives are possible."
  ;; stuff handled by `browse-url' or `gnus-button-embedded-url'
  :version "22.1"
  :group 'gnus-article-buttons
  :type 'integer)

(defcustom gnus-button-alist
  '(("<\\(url:[>\n\t ]*?\\)?\\(nntp\\|news\\):[>\n\t ]*\\([^>\n\t ]*@[^>\n\t ]*\\)>"
     0 (>= gnus-button-message-level 0) gnus-button-handle-news 3)
    ((concat "\\b\\(nntp\\|news\\):\\("
	     gnus-button-valid-localpart-regexp "@[a-z0-9.-]+[a-z]\\)")
     0 t gnus-button-handle-news 2)
    ("\\(\\b<\\(url:[>\n\t ]*\\)?\\(nntp\\|news\\):[>\n\t ]*\\(//\\)?\\([^>\n\t ]*\\)>\\)"
     1 (>= gnus-button-message-level 0) gnus-button-fetch-group 5)
    ("\\b\\(nntp\\|news\\):\\(//\\)?\\([^'\">\n\t ]+\\)"
     0 (>= gnus-button-message-level 0) gnus-button-fetch-group 3)
    ;; RFC 2392 (Don't allow `/' in domain part --> CID)
    ("\\bmid:\\(//\\)?\\([^'\">\n\t ]+@[^'\">\n\t /]+\\)"
     0 (>= gnus-button-message-level 0) gnus-button-message-id 2)
    ("\\bin\\( +article\\| +message\\)? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)"
     2 (>= gnus-button-message-level 0) gnus-button-message-id 3)
    ("\\b\\(mid\\|message-id\\):? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)"
     2 (>= gnus-button-message-level 0) gnus-button-message-id 3)
    ("\\(<URL: *\\)mailto: *\\([^> \n\t]+\\)>"
     0 (>= gnus-button-message-level 0) gnus-url-mailto 2)
    ;; RFC 2368 (The mailto URL scheme)
    ("\\bmailto:\\([-a-z.@_+0-9%=?&/]+\\)"
     0 (>= gnus-button-message-level 0) gnus-url-mailto 1)
    ("\\bmailto:\\([^ \n\t]+\\)"
     0 (>= gnus-button-message-level 0) gnus-url-mailto 1)
    ;; Info Konqueror style <info:/foo/bar baz>.
    ;; Must come before " Gnus home-grown style".
    ("\\binfo://?\\([^'\">\n\t]+\\)"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-info-url 1)
   ;; Info, Gnus home-grown style (deprecated) <info://foo/bar+baz>
    ("\\binfo://\\([^'\">\n\t ]+\\)"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-info-url 1)
    ;; Info GNOME style <info:foo#bar_baz>
    ("\\binfo:\\([^('\n\t\r \"><][^'\n\t\r \"><]*\\)"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-info-url-gnome 1)
    ;; Info KDE style <info:(foo)bar baz>
    ("<\\(info:\\(([^)]+)[^>\n\r]*\\)\\)>"
     1 (>= gnus-button-emacs-level 1) gnus-button-handle-info-url-kde 2)
    ("\\((Info-goto-node\\|(info\\)[ \t\n]*\\(\"[^\"]*\"\\))" 0
     (>= gnus-button-emacs-level 1) gnus-button-handle-info-url 2)
    ("\\b\\(C-h\\|<?[Ff]1>?\\)[ \t\n]+i[ \t\n]+d?[ \t\n]?m[ \t\n]+[^ ]+ ?[^ ]+[ \t\n]+RET\\([ \t\n]+i[ \t\n]+[^ ]+ ?[^ ]+[ \t\n]+RET\\([ \t\n,]*\\)\\)?"
     ;; Info links like `C-h i d m Gnus RET' or `C-h i d m Gnus RET i partial RET'
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-info-keystrokes 0)
    ;; This is custom
    ("M-x[ \t\n]\\(customize-[^ ]+\\)[ \t\n]RET[ \t\n]\\([^ ]+\\)[ \t\n]RET\\>" 0
     (>= gnus-button-emacs-level 1) gnus-button-handle-custom 1 2)
    ;; Emacs help commands
    ("M-x[ \t\n]+apropos[ \t\n]+RET[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     ;; regexp doesn't match arguments containing ` '.
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-apropos 1)
    ("M-x[ \t\n]+apropos-command[ \t\n]+RET[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-apropos-command 1)
    ("M-x[ \t\n]+apropos-variable[ \t\n]+RET[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-apropos-variable 1)
    ("M-x[ \t\n]+apropos-documentation[ \t\n]+RET[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-apropos-documentation 1)
    ;; The following entries may lead to many false positives so don't enable
    ;; them by default (use a high button level).
    ("/\\([a-z][-a-z0-9]+\\.el\\)\\>[^.?]"
     ;; Exclude [.?] for URLs in gmane.emacs.cvs
     1 (>= gnus-button-emacs-level 8) gnus-button-handle-library 1)
    ("`\\([a-z][-a-z0-9]+\\.el\\)'"
     1 (>= gnus-button-emacs-level 8) gnus-button-handle-library 1)
    ("`\\([a-z][a-z0-9]+-[a-z0-9]+-[-a-z0-9]*[a-z]\\|\\(gnus\\|message\\)-[-a-z]+\\)'"
     0 (>= gnus-button-emacs-level 8) gnus-button-handle-symbol 1)
    ("`\\([a-z][a-z0-9]+-[a-z]+\\)'"
     0 (>= gnus-button-emacs-level 9) gnus-button-handle-symbol 1)
    ("(setq[ \t\n]+\\([a-z][a-z0-9]+-[-a-z0-9]+\\)[ \t\n]+.+)"
     1 (>= gnus-button-emacs-level 7) gnus-button-handle-describe-variable 1)
    ("\\bM-x[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     1 (>= gnus-button-emacs-level 7) gnus-button-handle-describe-function 1)
    ("\\b\\(C-h\\|<?[Ff]1>?\\)[ \t\n]+f[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-describe-function 2)
    ("\\b\\(C-h\\|<?[Ff]1>?\\)[ \t\n]+v[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+RET\\>"
     0 (>= gnus-button-emacs-level 1) gnus-button-handle-describe-variable 2)
    ("`\\(\\(C-h\\|<?[Ff]1>?\\)[ \t\n]+k[ \t\n]+\\([^']+\\)\\)'"
     ;; Unlike the other regexps we really have to require quoting
     ;; here to determine where it ends.
     1 (>= gnus-button-emacs-level 1) gnus-button-handle-describe-key 3)
    ;; This is how URLs _should_ be embedded in text (RFC 1738, RFC 2396)...
    ("<URL: *\\([^\n<>]*\\)>"
     1 (>= gnus-button-browse-level 0) gnus-button-embedded-url 1)
    ;; RFC 2396 (2.4.3., delims) ...
    ("\"URL: *\\([^\n\"]*\\)\""
     1 (>= gnus-button-browse-level 0) gnus-button-embedded-url 1)
    ;; Raw URLs.
    (gnus-button-url-regexp
     0 (>= gnus-button-browse-level 0) browse-url 0)
    ;; man pages
    ("\\b\\([a-z][a-z]+([1-9])\\)\\W"
     0 (and (>= gnus-button-man-level 1) (< gnus-button-man-level 3))
     gnus-button-handle-man 1)
    ;; more man pages: resolv.conf(5), iso_8859-1(7), xterm(1x)
    ("\\b\\([a-z][-_.a-z0-9]+([1-9])\\)\\W"
     0 (and (>= gnus-button-man-level 3) (< gnus-button-man-level 5))
     gnus-button-handle-man 1)
    ;; even more: Apache::PerlRun(3pm), PDL::IO::FastRaw(3pm),
    ;; SoWWWAnchor(3iv), XSelectInput(3X11), X(1), X(7)
    ("\\b\\(\\(?:[a-z][-+_.:a-z0-9]+([1-9][X1a-z]*)\\)\\|\\b\\(?:X([1-9])\\)\\)\\W"
     0 (>= gnus-button-man-level 5) gnus-button-handle-man 1)
    ;; Recognizing patches to .el files.  This is somewhat obscure,
    ;; but considering the percentage of Gnus users who hack Emacs
    ;; Lisp files...
    ("^--- \\([^ .]+\\.el\\).*\n.*\n@@ -?\\([0-9]+\\)" 1
     (>= gnus-button-message-level 4) gnus-button-patch 1 2)
    ("^\\*\\*\\* \\([^ .]+\\.el\\).*\n.*\n\\*+\n\\*\\*\\* \\([0-9]+\\)" 1
     (>= gnus-button-message-level 4) gnus-button-patch 1 2)
    ;; MID or mail: To avoid too many false positives we don't try to catch
    ;; all kind of allowed MIDs or mail addresses.  Domain part must contain
    ;; at least one dot.  TLD must contain two or three chars or be a know TLD
    ;; (info|name|...).  Put this entry near the _end_ of `gnus-button-alist'
    ;; so that non-ambiguous entries (see above) match first.
    (gnus-button-mid-or-mail-regexp
     0 (>= gnus-button-message-level 5) gnus-button-handle-mid-or-mail 1))
  "*Alist of regexps matching buttons in article bodies.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string (case insensitive) matching text around the button (can
also be Lisp expression evaluating to a string),
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a Lisp expression which must eval to true for the button to
be added,
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function."
  :group 'gnus-article-buttons
  :type '(repeat (list (choice regexp variable sexp)
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))
(put 'gnus-button-alist 'risky-local-variable t)

(defcustom gnus-header-button-alist
  '(("^\\(References\\|Message-I[Dd]\\|^In-Reply-To\\):" "<[^<>]+>"
     0 (>= gnus-button-message-level 0) gnus-button-message-id 0)
    ("^\\(From\\|Reply-To\\):" ": *\\(.+\\)$"
     1 (>= gnus-button-message-level 0) gnus-button-reply 1)
    ("^\\(Cc\\|To\\):" "[^ \t\n<>,()\"]+@[^ \t\n<>,()\"]+"
     0 (>= gnus-button-message-level 0) gnus-msg-mail 0)
    ("^X-[Uu][Rr][Ll]:" gnus-button-url-regexp
     0 (>= gnus-button-browse-level 0) browse-url 0)
    ("^Subject:" gnus-button-url-regexp
     0 (>= gnus-button-browse-level 0) browse-url 0)
    ("^[^:]+:" gnus-button-url-regexp
     0 (>= gnus-button-browse-level 0) browse-url 0)
    ("^OpenPGP:.*url=" gnus-button-url-regexp
     0 (>= gnus-button-browse-level 0) gnus-button-openpgp 0)
    ("^[^:]+:" "\\bmailto:\\([-a-z.@_+0-9%=?&/]+\\)"
     0 (>= gnus-button-message-level 0) gnus-url-mailto 1)
    ("^[^:]+:" "\\(<\\(url: \\)?\\(nntp\\|news\\):\\([^>\n ]*\\)>\\)"
     1 (>= gnus-button-message-level 0) gnus-button-message-id 4))
  "*Alist of headers and regexps to match buttons in article heads.

This alist is very similar to `gnus-button-alist', except that each
alist has an additional HEADER element first in each entry:

\(HEADER REGEXP BUTTON FORM CALLBACK PAR)

HEADER is a regexp to match a header.  For a fuller explanation, see
`gnus-button-alist'."
  :group 'gnus-article-buttons
  :group 'gnus-article-headers
  :type '(repeat (list (regexp :tag "Header")
		       (choice regexp variable)
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))
(put 'gnus-header-button-alist 'risky-local-variable t)

;;; Commands:

(defun gnus-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
	 (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (goto-char pos)
    (when fun
      (funcall fun data))))

(defun gnus-article-press-button ()
  "Check text at point for a callback function.
If the text at point has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive)
  (let ((data (get-text-property (point) 'gnus-data))
	(fun (get-text-property (point) 'gnus-callback)))
    (when fun
      (funcall fun data))))

(defun gnus-article-highlight (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-citation',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-citation force)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons)
  (gnus-article-add-buttons-to-head))

(defun gnus-article-highlight-some (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons))

(defun gnus-article-highlight-headers ()
  "Highlight article headers as specified by `gnus-header-face-alist'."
  (interactive)
  (gnus-with-article-headers
    (let (regexp header-face field-face from hpoints fpoints)
      (dolist (entry gnus-header-face-alist)
	(goto-char (point-min))
	(setq regexp (concat "^\\("
			     (if (string-equal "" (nth 0 entry))
				 "[^\t ]"
			       (nth 0 entry))
			     "\\)")
	      header-face (nth 1 entry)
	      field-face (nth 2 entry))
	(while (and (re-search-forward regexp nil t)
		    (not (eobp)))
	  (beginning-of-line)
	  (setq from (point))
	  (unless (search-forward ":" nil t)
	    (forward-char 1))
	  (when (and header-face
		     (not (memq (point) hpoints)))
	    (push (point) hpoints)
	    (gnus-put-text-property from (point) 'face header-face))
	  (when (and field-face
		     (not (memq (setq from (point)) fpoints)))
	    (push from fpoints)
	    (if (re-search-forward "^[^ \t]" nil t)
		(forward-char -2)
	      (goto-char (point-max)))
	    (gnus-put-text-property from (point) 'face field-face)))))))

(defun gnus-article-highlight-signature ()
  "Highlight the signature in an article.
It does this by highlighting everything after
`gnus-signature-separator' using the face `gnus-signature'."
  (interactive)
  (gnus-with-article-buffer
    (let ((inhibit-point-motion-hooks t))
      (save-restriction
	(when (and gnus-signature-face
		   (gnus-article-narrow-to-signature))
	  (gnus-overlay-put (gnus-make-overlay (point-min) (point-max) nil t)
			    'face gnus-signature-face)
	  (widen)
	  (gnus-article-search-signature)
	  (let ((start (match-beginning 0))
		(end (set-marker (make-marker) (1+ (match-end 0)))))
	    (gnus-article-add-button start (1- end) 'gnus-signature-toggle
				     end)))))))

(defun gnus-button-in-region-p (b e prop)
  "Say whether PROP exists in the region."
  (text-property-not-all b e prop nil))

(defun gnus-article-add-buttons ()
  "Find external references in the article and make buttons of them.
\"External references\" are things like Message-IDs and URLs, as
specified by `gnus-button-alist'."
  (interactive)
  (gnus-with-article-buffer
    (let ((inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-button-alist)
	  beg entry regexp)
      ;; We skip the headers.
      (article-goto-body)
      (setq beg (point))
      (while (setq entry (pop alist))
	(setq regexp (eval (car entry)))
	(goto-char beg)
	(while (re-search-forward regexp nil t)
	  (let ((start (match-beginning (nth 1 entry)))
		(end (match-end (nth 1 entry)))
		(from (match-beginning 0)))
	    (when (and (eval (nth 2 entry))
		       (not (gnus-button-in-region-p
			     start end 'gnus-callback)))
	      ;; That optional form returned non-nil, so we add the
	      ;; button.
	      (setq from (set-marker (make-marker) from))
	      (unless (and (eq (car entry) 'gnus-button-url-regexp)
			   (gnus-article-extend-url-button from start end))
		(gnus-article-add-button start end
					 'gnus-button-push (list from entry))
		(gnus-put-text-property
		 start end
		 'gnus-string (buffer-substring-no-properties
			       start end))))))))))

(defun gnus-article-extend-url-button (beg start end)
  "Extend url button if url is folded into two or more lines.
Return non-nil if button is extended.  BEG is a marker that points to
the beginning position of a text containing url.  START and END are
the endpoints of a url button before it is extended.  The concatenated
url is put as the `gnus-button-url' overlay property on the button."
  (let ((opoint (point))
	(points (list start end))
	url delim regexp)
    (prog1
	(when (and (progn
		     (goto-char end)
		     (not (looking-at "[\t ]*[\">]")))
		   (progn
		     (goto-char start)
		     (string-match
		      "\\(?:\"\\|\\(<\\)\\)[\t ]*\\(?:url[\t ]*:[\t ]*\\)?\\'"
		      (buffer-substring (point-at-bol) start)))
		   (progn
		     (setq url (list (buffer-substring start end))
			   delim (if (match-beginning 1) ">" "\""))
		     (beginning-of-line)
		     (setq regexp (concat
				   (when (and (looking-at
					       message-cite-prefix-regexp)
					      (< (match-end 0) start))
				     (regexp-quote (match-string 0)))
				   "\
\[\t ]*\\(?:\\([^\t\n \">]+\\)[\t ]*$\\|\\([^\t\n \">]*\\)[\t ]*"
				   delim "\\)"))
		     (while (progn
			      (forward-line 1)
			      (and (looking-at regexp)
				   (prog1
				       (match-beginning 1)
				     (push (or (match-string 2)
					       (match-string 1))
					   url)
				     (push (setq end (or (match-end 2)
							 (match-end 1)))
					   points)
				     (push (or (match-beginning 2)
					       (match-beginning 1))
					   points)))))
		     (match-beginning 2)))
	  (let (gnus-article-mouse-face widget-mouse-face)
	    (while points
	      (gnus-article-add-button (pop points) (pop points)
				       'gnus-button-push beg)))
	  (let ((overlay (gnus-make-overlay start end)))
	    (gnus-overlay-put overlay 'evaporate t)
	    (gnus-overlay-put overlay 'gnus-button-url
			      (list (mapconcat 'identity (nreverse url) "")))
	    (when gnus-article-mouse-face
	      (gnus-overlay-put overlay 'mouse-face gnus-article-mouse-face)))
	  t)
      (goto-char opoint))))

;; Add buttons to the head of an article.
(defun gnus-article-add-buttons-to-head ()
  "Add buttons to the head of the article."
  (interactive)
  (gnus-with-article-headers
    (let (beg end)
      (dolist (entry gnus-header-button-alist)
	;; Each alist entry.
	(goto-char (point-min))
	(while (re-search-forward (car entry) nil t)
	  ;; Each header matching the entry.
	  (setq beg (match-beginning 0))
	  (setq end (or (and (re-search-forward "^[^ \t]" nil t)
			     (match-beginning 0))
			(point-max)))
	  (goto-char beg)
	  (while (re-search-forward (eval (nth 1 entry)) end t)
	    ;; Each match within a header.
	    (let* ((entry (cdr entry))
		   (start (match-beginning (nth 1 entry)))
		   (end (match-end (nth 1 entry)))
		   (form (nth 2 entry)))
	      (goto-char (match-end 0))
	      (when (eval form)
		(gnus-article-add-button
		 start end (nth 3 entry)
		 (buffer-substring (match-beginning (nth 4 entry))
				   (match-end (nth 4 entry)))))))
	  (goto-char end))))))

;;; External functions:

(defun gnus-article-add-button (from to fun &optional data text)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (when gnus-article-button-face
    (gnus-overlay-put (gnus-make-overlay from to nil t)
		      'face gnus-article-button-face))
  (gnus-add-text-properties
   from to
   (nconc (and gnus-article-mouse-face
	       (list gnus-mouse-face-prop gnus-article-mouse-face))
	  (list 'gnus-callback fun)
	  (and data (list 'gnus-data data))))
  (widget-convert-button 'link from to :action 'gnus-widget-press-button
			 :help-echo (or text "Follow the link")
			 :keymap gnus-url-button-map
			 :button-keymap gnus-widget-button-keymap))

(defun gnus-article-copy-string ()
  "Copy the string in the button to the kill ring."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-string)))
    (when data
      (with-temp-buffer
	(insert data)
	(copy-region-as-kill (point-min) (point-max))
	(message "Copied %s" data)))))

;;; Internal functions:

(defun gnus-article-set-globals ()
  (with-current-buffer gnus-summary-buffer
    (gnus-set-global-variables)))

(defun gnus-signature-toggle (end)
  (gnus-with-article-buffer
    (let ((inhibit-point-motion-hooks t))
      (if (text-property-any end (point-max) 'article-type 'signature)
	  (progn
	    (gnus-delete-wash-type 'signature)
	    (gnus-remove-text-properties-when
	     'article-type 'signature end (point-max)
	     (cons 'article-type (cons 'signature
				       gnus-hidden-properties))))
	(gnus-add-wash-type 'signature)
	(gnus-add-text-properties-when
	 'article-type nil end (point-max)
	 (cons 'article-type (cons 'signature
				   gnus-hidden-properties)))))
    (let ((gnus-article-mime-handle-alist-1 gnus-article-mime-handle-alist))
      (gnus-set-mode-line 'article))))

(defun gnus-button-push (marker-and-entry)
  ;; Push button starting at MARKER.
  (save-excursion
    (let* ((marker (car marker-and-entry))
           (entry (cadr marker-and-entry))
           (regexp (car entry))
           (inhibit-point-motion-hooks t))
      (goto-char marker)
      ;; This is obviously true, or something bad is happening :)
      ;; But we need it to have the match-data
      (when (looking-at (or (if (symbolp regexp)
                                (symbol-value regexp)
                              regexp)))
        (let ((fun (nth 3 entry))
              (args (or (and (eq (car entry) 'gnus-button-url-regexp)
                             (get-char-property marker 'gnus-button-url))
                        (mapcar (lambda (group)
                                  (let ((string (match-string group)))
                                    (set-text-properties
                                     0 (length string) nil string)
                                    string))
                                (nthcdr 4 entry)))))

          (cond
           ((fboundp fun)
            (apply fun args))
           ((and (boundp fun)
                 (fboundp (symbol-value fun)))
            (apply (symbol-value fun) args))
           (t
            (gnus-message 1 "You must define `%S' to use this button"
                          (cons fun args)))))))))

(defun gnus-parse-news-url (url)
  (let (scheme server port group message-id articles)
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (when (looking-at "\\([A-Za-z]+\\):")
	(setq scheme (match-string 1))
	(goto-char (match-end 0)))
      (when (looking-at "//\\([^:/]+\\)\\(:?\\)\\([0-9]+\\)?/")
	(setq server (match-string 1))
	(setq port (if (stringp (match-string 3))
		       (string-to-number (match-string 3))
		     (match-string 3)))
	(goto-char (match-end 0)))

      (cond
       ((looking-at "\\(.*@.*\\)")
	(setq message-id (match-string 1)))
       ((looking-at "\\([^/]+\\)/\\([-0-9]+\\)")
	(setq group (match-string 1)
	      articles (split-string (match-string 2) "-")))
       ((looking-at "\\([^/]+\\)/?")
	(setq group (match-string 1)))
       (t
	(error "Unknown news URL syntax"))))
    (list scheme server port group message-id articles)))

(defun gnus-button-handle-news (url)
  "Fetch a news URL."
  (destructuring-bind (scheme server port group message-id articles)
      (gnus-parse-news-url url)
    (cond
     (message-id
      (with-current-buffer gnus-summary-buffer
	(if server
	    (let ((gnus-refer-article-method
		   (nconc (list (list 'nntp server))
			  gnus-refer-article-method))
		  (nntp-port-number (or port "nntp")))
	      (gnus-message 7 "Fetching %s with %s"
			    message-id gnus-refer-article-method)
	      (gnus-summary-refer-article message-id))
	  (gnus-summary-refer-article message-id))))
     (group
      (gnus-button-fetch-group url)))))

(defun gnus-button-patch (library line)
  "Visit an Emacs Lisp library LIBRARY on line LINE."
  (interactive)
  (let ((file (locate-library (file-name-nondirectory library))))
    (unless file
      (error "Couldn't find library %s" library))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))))

(defun gnus-button-handle-man (url)
  "Fetch a man page."
  (gnus-message 9 "`%s' `%s'" gnus-button-man-handler url)
  (when (eq gnus-button-man-handler 'woman)
    (setq url (gnus-replace-in-string url "([1-9][X1a-z]*).*\\'" "")))
  (gnus-message 9 "`%s' `%s'" gnus-button-man-handler url)
  (funcall gnus-button-man-handler url))

(defun gnus-button-handle-info-url (url)
  "Fetch an info URL."
  (setq url (mm-subst-char-in-string ?+ ?\  url))
  (cond
   ((string-match "^\\([^:/]+\\)?/\\(.*\\)" url)
    (gnus-info-find-node
     (concat "(" (or (gnus-url-unhex-string (match-string 1 url))
		     "Gnus")
	     ")" (gnus-url-unhex-string (match-string 2 url)))))
   ((string-match "([^)\"]+)[^\"]+" url)
    (setq url
	  (gnus-replace-in-string
	   (gnus-replace-in-string url "[\n\t ]+" " ") "\"" ""))
    (gnus-info-find-node url))
   (t (error "Can't parse %s" url))))

(defun gnus-button-handle-info-url-gnome (url)
  "Fetch GNOME style info URL."
  (setq url (mm-subst-char-in-string ?_ ?\  url))
  (if (string-match "\\([^#]+\\)#?\\(.*\\)" url)
      (gnus-info-find-node
       (concat "("
	       (gnus-url-unhex-string
		 (match-string 1 url))
	       ")"
	       (or (gnus-url-unhex-string
		    (match-string 2 url))
		   "Top")))
    (error "Can't parse %s" url)))

(defun gnus-button-handle-info-url-kde (url)
  "Fetch KDE style info URL."
  (gnus-info-find-node (gnus-url-unhex-string url)))

;; (info) will autoload info.el
(declare-function Info-menu "info" (menu-item &optional fork))
(declare-function Info-index-next "info" (num))

(defun gnus-button-handle-info-keystrokes (url)
  "Call `info' when pushing the corresponding URL button."
  ;; For links like `C-h i d m gnus RET part RET , ,', `C-h i d m CC Mode RET'.
  (let (node indx comma)
    (if (string-match
	 (concat "\\b\\(C-h\\|<?[Ff]1>?\\)[ \t\n]+i[ \t\n]+d?[ \t\n]?m[ \t\n]+"
		 "\\([^ ]+ ?[^ ]+\\)[ \t\n]+RET"
		 "\\([ \t\n]+i[ \t\n]+[^ ]+ ?[^ ]+[ \t\n]+RET\\>"
		 "\\(?:[ \t\n,]*\\)\\)?")
	 url)
	(setq node (match-string 2 url)
	      indx (match-string 3 url))
      (error "Can't parse %s" url))
    (info)
    (Info-directory)
    (Info-menu node)
    (when (> (length indx) 0)
      (string-match (concat "[ \t\n]+i[ \t\n]+\\([^ ]+ ?[^ ]+\\)[ \t\n]+RET\\>"
			    "\\([ \t\n,]*\\)")
		    indx)
      (setq comma (match-string 2 indx))
      (setq indx  (match-string 1 indx))
      (Info-index indx)
      (when comma
	(dotimes (i (with-temp-buffer
		      (insert comma)
		      ;; Note: the XEmacs version of `how-many' takes
		      ;; no optional argument.
		      (goto-char (point-min))
		      (how-many ",")))
	  (Info-index-next 1)))
      nil)))

(autoload 'pgg-snarf-keys-region "pgg")
;; Called after pgg-snarf-keys-region, which autoloads pgg.el.
(declare-function pgg-display-output-buffer "pgg" (start end status))

(defun gnus-button-openpgp (url)
  "Retrieve and add an OpenPGP key given URL from an OpenPGP header."
  (with-temp-buffer
    (mm-url-insert-file-contents-external url)
    (pgg-snarf-keys-region (point-min) (point-max))
    (pgg-display-output-buffer nil nil nil)))

(defun gnus-button-message-id (message-id)
  "Fetch MESSAGE-ID."
  (with-current-buffer gnus-summary-buffer
    (gnus-summary-refer-article message-id)))

(defun gnus-button-fetch-group (address &rest ignore)
  "Fetch GROUP specified by ADDRESS."
  (when (string-match "\\`\\(nntp\\|news\\):\\(//\\)?\\(.*\\)\\'"
		      address)
    ;; Allow to use `gnus-button-fetch-group' in `browse-url-browser-function'
    ;; for nntp:// and news://
    (setq address (match-string 3 address)))
  (if (not (string-match "[:/]" address))
      ;; This is just a simple group url.
      (gnus-group-read-ephemeral-group address gnus-select-method)
    (if (not
	 (string-match
	  "^\\([^:/]+\\)\\(:\\([^/]+\\)\\)?/\\([^/]+\\)\\(/\\([0-9]+\\)\\)?"
	  address))
	(error "Can't parse %s" address)
      (gnus-group-read-ephemeral-group
       (match-string 4 address)
       `(nntp ,(match-string 1 address)
	      (nntp-address ,(match-string 1 address))
	      (nntp-port-number ,(if (match-end 3)
				     (match-string 3 address)
				   "nntp")))
       nil nil nil
       (and (match-end 6) (list (string-to-number (match-string 6 address))))))))

(defun gnus-url-parse-query-string (query &optional downcase)
  (let (retval pairs cur key val)
    (setq pairs (split-string query "&"))
    (while pairs
      (setq cur (car pairs)
	    pairs (cdr pairs))
      (if (not (string-match "=" cur))
	  nil                           ; Grace
	(setq key (gnus-url-unhex-string (substring cur 0 (match-beginning 0)))
	      val (gnus-url-unhex-string (substring cur (match-end 0) nil) t))
	(if downcase
	    (setq key (downcase key)))
	(setq cur (assoc key retval))
	(if cur
	    (setcdr cur (cons val (cdr cur)))
	  (setq retval (cons (list key val) retval)))))
    retval))

(defun gnus-url-mailto (url)
  ;; Send mail to someone
  (setq url (replace-regexp-in-string "\n" " " url))
  (when (string-match "mailto:/*\\(.*\\)" url)
    (setq url (substring url (match-beginning 1) nil)))
  (let (to args subject func)
    (setq args (gnus-url-parse-query-string
		(if (string-match "^\\?" url)
		    (substring url 1)
		  (if (string-match "^\\([^?]+\\)\\?\\(.*\\)" url)
		      (concat "to=" (match-string 1 url) "&"
			      (match-string 2 url))
		    (concat "to=" url))))
	  subject (cdr-safe (assoc "subject" args)))
    (gnus-msg-mail)
    (while args
      (setq func (intern-soft (concat "message-goto-" (downcase (caar args)))))
      (if (fboundp func)
	  (funcall func)
	(message-position-on-field (caar args)))
      (insert (gnus-replace-in-string
	       (mapconcat 'identity (reverse (cdar args)) ", ")
	       "\r\n" "\n" t))
      (setq args (cdr args)))
    (if subject
	(message-goto-body)
      (message-goto-subject))))

(defun gnus-button-embedded-url (address)
  "Activate ADDRESS with `browse-url'."
  (browse-url (gnus-strip-whitespace address)))

;;; Next/prev buttons in the article buffer.

(defvar gnus-next-page-line-format "%{%(Next page...%)%}\n")
(defvar gnus-prev-page-line-format "%{%(Previous page...%)%}\n")

(defvar gnus-prev-page-map
  (let ((map (make-sparse-keymap)))
    (define-key map gnus-mouse-2 'gnus-button-prev-page)
    (define-key map "\r" 'gnus-button-prev-page)
    map))

(defvar gnus-next-page-map
  (let ((map (make-sparse-keymap)))
    (define-key map gnus-mouse-2 'gnus-button-next-page)
    (define-key map "\r" 'gnus-button-next-page)
    map))

(defun gnus-insert-prev-page-button ()
  (let ((b (point)) e
	(inhibit-read-only t))
    (gnus-eval-format
     gnus-prev-page-line-format nil
     `(keymap ,gnus-prev-page-map
	      gnus-prev t
	      gnus-callback gnus-article-button-prev-page
	      article-type annotation))
    (setq e (if (bolp)
		;; Exclude a newline.
		(1- (point))
	      (point)))
    (when gnus-article-button-face
      (gnus-overlay-put (gnus-make-overlay b e nil t)
                        'face gnus-article-button-face))
    (widget-convert-button
     'link b e
     :action 'gnus-button-prev-page
     :button-keymap gnus-prev-page-map)))

(defun gnus-button-next-page (&optional args more-args)
  "Go to the next page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-button-prev-page (&optional args more-args)
  "Go to the prev page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defun gnus-insert-next-page-button ()
  (let ((b (point)) e
	(inhibit-read-only t))
    (gnus-eval-format gnus-next-page-line-format nil
		      `(keymap ,gnus-next-page-map
                               gnus-next t
                               gnus-callback gnus-article-button-next-page
                               article-type annotation))
    (setq e (if (bolp)
		;; Exclude a newline.
		(1- (point))
	      (point)))
    (when gnus-article-button-face
      (gnus-overlay-put (gnus-make-overlay b e nil t)
                        'face gnus-article-button-face))
    (widget-convert-button
     'link b e
     :action 'gnus-button-next-page
     :button-keymap gnus-next-page-map)))

(defun gnus-article-button-next-page (arg)
  "Go to the next page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-article-button-prev-page (arg)
  "Go to the prev page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defvar gnus-decode-header-methods
  '(mail-decode-encoded-word-region)
  "List of methods used to decode headers.

This variable is a list of FUNCTION or (REGEXP . FUNCTION).  If item
is FUNCTION, FUNCTION will be applied to all newsgroups.  If item is a
\(REGEXP . FUNCTION), FUNCTION will be only apply to the newsgroups
whose names match REGEXP.

For example:
\((\"chinese\" . gnus-decode-encoded-word-region-by-guess)
 mail-decode-encoded-word-region
 (\"chinese\" . rfc1843-decode-region))
")

(defvar gnus-decode-header-methods-cache nil)

(defun gnus-multi-decode-header (start end)
  "Apply the functions from `gnus-encoded-word-methods' that match."
  (unless (and gnus-decode-header-methods-cache
	       (eq gnus-newsgroup-name
		   (car gnus-decode-header-methods-cache)))
    (setq gnus-decode-header-methods-cache (list gnus-newsgroup-name))
    (dolist (x gnus-decode-header-methods)
      (if (symbolp x)
	  (nconc gnus-decode-header-methods-cache (list x))
	(if (and gnus-newsgroup-name
		 (string-match (car x) gnus-newsgroup-name))
	    (nconc gnus-decode-header-methods-cache
		   (list (cdr x)))))))
  (let ((xlist gnus-decode-header-methods-cache))
    (pop xlist)
    (save-restriction
      (narrow-to-region start end)
      (while xlist
	(funcall (pop xlist) (point-min) (point-max))))))

;;;
;;; Treatment top-level handling.
;;;

(defvar gnus-inhibit-article-treatments nil)

(defun gnus-treat-article (gnus-treat-condition
			   &optional part-number total-parts gnus-treat-type)
  (let ((gnus-treat-length (- (point-max) (point-min)))
	(alist gnus-treatment-function-alist)
	(article-goto-body-goes-to-point-min-p t)
	(treated-type
	 (or (not gnus-treat-type)
	     (catch 'found
	       (let ((list gnus-article-treat-types))
		 (while list
		   (when (string-match (pop list) gnus-treat-type)
		     (throw 'found t)))))))
	(highlightp (gnus-visual-p 'article-highlight 'highlight))
	val elem)
    (gnus-run-hooks 'gnus-part-display-hook)
    (dolist (elem alist)
      (setq val
	    (save-excursion
	      (when (gnus-buffer-live-p gnus-summary-buffer)
		(set-buffer gnus-summary-buffer))
	      (symbol-value (car elem))))
      (when (and (or (consp val)
		     treated-type)
		 (or (not gnus-inhibit-article-treatments)
		     (eq gnus-treat-condition 'head))
		 (gnus-treat-predicate val)
		 (or (not (get (car elem) 'highlight))
		     highlightp))
	(save-restriction
	  (funcall (cadr elem)))))))

;; Dynamic variables.
(defvar part-number)
(defvar total-parts)
(defvar gnus-treat-type)
(defvar gnus-treat-condition)
(defvar gnus-treat-length)

(defun gnus-treat-predicate (val)
  (cond
   ((null val)
    nil)
   (gnus-treat-condition
    (eq gnus-treat-condition val))
   ((and (listp val)
	 (stringp (car val)))
    (apply 'gnus-or (mapcar `(lambda (s)
			       (string-match s ,(or gnus-newsgroup-name "")))
			    val)))
   ((listp val)
    (let ((pred (pop val)))
      (cond
       ((eq pred 'or)
	(apply 'gnus-or (mapcar 'gnus-treat-predicate val)))
       ((eq pred 'and)
	(apply 'gnus-and (mapcar 'gnus-treat-predicate val)))
       ((eq pred 'not)
	(not (gnus-treat-predicate (car val))))
       ((eq pred 'typep)
	(equal (car val) gnus-treat-type))
       (t
	(error "%S is not a valid predicate" pred)))))
   ((eq val t)
    t)
   ((eq val 'head)
    nil)
   ((eq val 'first)
    (eq part-number 1))
   ((eq val 'last)
    (eq part-number total-parts))
   ((numberp val)
    (< gnus-treat-length val))
   (t
    (error "%S is not a valid value" val))))

(defun gnus-article-encrypt-body (protocol &optional n)
  "Encrypt the article body."
  (interactive
   (list
    (or gnus-article-encrypt-protocol
	(gnus-completing-read "Encrypt protocol"
                              (mapcar 'car gnus-article-encrypt-protocol-alist)
                              t))
    current-prefix-arg))
  ;; User might hit `K E' instead of `K e', so prompt once.
  (when (and gnus-article-encrypt-protocol
	     gnus-novice-user)
    (unless (gnus-y-or-n-p "Really encrypt article(s)? ")
      (error "Encrypt aborted")))
  (let ((func (cdr (assoc protocol gnus-article-encrypt-protocol-alist))))
    (unless func
      (error "Can't find the encrypt protocol %s" protocol))
    (if (member gnus-newsgroup-name '("nndraft:delayed"
				      "nndraft:drafts"
				      "nndraft:queue"))
	(error "Can't encrypt the article in group %s"
	       gnus-newsgroup-name))
    (gnus-summary-iterate n
      (with-current-buffer gnus-summary-buffer
	(let ((mail-parse-charset gnus-newsgroup-charset)
	      (mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets)
	      (summary-buffer gnus-summary-buffer)
	      references point)
	  (gnus-set-global-variables)
	  (when (gnus-group-read-only-p)
	    (error "The current newsgroup does not support article encrypt"))
	  (gnus-summary-show-article t)
	  (setq references
	      (or (mail-header-references gnus-current-headers) ""))
	  (set-buffer gnus-article-buffer)
	  (let* ((inhibit-read-only t)
		 (headers
		  (mapcar (lambda (field)
			    (and (save-restriction
				   (message-narrow-to-head)
				   (goto-char (point-min))
				   (search-forward field nil t))
				 (prog2
				     (message-narrow-to-field)
				     (buffer-string)
				   (delete-region (point-min) (point-max))
				   (widen))))
			  '("Content-Type:" "Content-Transfer-Encoding:"
			    "Content-Disposition:"))))
	    (message-narrow-to-head)
	    (message-remove-header "MIME-Version")
	    (goto-char (point-max))
	    (setq point (point))
	    (insert (apply 'concat headers))
	    (widen)
	    (narrow-to-region point (point-max))
	    (let ((message-options message-options))
	      (message-options-set 'message-sender user-mail-address)
	      (message-options-set 'message-recipients user-mail-address)
	      (message-options-set 'message-sign-encrypt 'not)
	      (funcall func))
	    (goto-char (point-min))
	    (insert "MIME-Version: 1.0\n")
	    (widen)
	    (gnus-summary-edit-article-done
	     references nil summary-buffer t))
	  (when gnus-keep-backlog
	    (gnus-backlog-remove-article
	     (car gnus-article-current) (cdr gnus-article-current)))
	  (gnus-flush-original-article-buffer)
	  (when gnus-use-cache
	    (gnus-cache-update-article
	     (car gnus-article-current) (cdr gnus-article-current))))))))

(defvar gnus-mime-security-button-line-format "%{%([[%t:%i]%D]%)%}\n"
  "The following specs can be used:
%t  The security MIME type
%i  Additional info
%d  Details
%D  Details if button is pressed")

(defvar gnus-mime-security-button-end-line-format "%{%([[End of %t]%D]%)%}\n"
  "The following specs can be used:
%t  The security MIME type
%i  Additional info
%d  Details
%D  Details if button is pressed")

(defvar gnus-mime-security-button-line-format-alist
  '((?t gnus-tmp-type ?s)
    (?i gnus-tmp-info ?s)
    (?d gnus-tmp-details ?s)
    (?D gnus-tmp-pressed-details ?s)))

(defvar gnus-mime-security-button-commands
  '((gnus-article-press-button "\r" "Show Detail")
    (undefined "v")
    (undefined "t")
    (undefined "C")
    (gnus-mime-security-save-part "o" "Save...")
    (undefined "\C-o")
    (undefined "r")
    (undefined "d")
    (undefined "c")
    (undefined "i")
    (undefined "E")
    (undefined "e")
    (undefined "p")
    (gnus-mime-security-pipe-part "|" "Pipe To Command...")
    (undefined ".")))

(defvar gnus-mime-security-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map gnus-mouse-2 'gnus-article-push-button)
    (define-key map gnus-down-mouse-3 'gnus-mime-security-button-menu)
    (dolist (c gnus-mime-security-button-commands)
      (define-key map (cadr c) (car c)))
    map))

(easy-menu-define
  gnus-mime-security-button-menu gnus-mime-security-button-map
  "Security button menu."
  `("Security Part"
    ,@(delq nil
	    (mapcar (lambda (c)
		      (unless (eq (car c) 'undefined)
			(vector (caddr c) (car c) :active t)))
		    gnus-mime-security-button-commands))))

(defun gnus-mime-security-button-menu (event prefix)
  "Construct a context-sensitive menu of security commands."
  (interactive "e\nP")
  (save-window-excursion
    (let ((pos (event-start event)))
      (select-window (posn-window pos))
      (goto-char (posn-point pos))
      (gnus-article-check-buffer)
      (popup-menu gnus-mime-security-button-menu nil prefix))))

(defvar gnus-mime-security-details-buffer nil)

(defvar gnus-mime-security-button-pressed nil)

(defvar gnus-mime-security-show-details-inline t
  "If non-nil, show details in the article buffer.")

(defun gnus-mime-security-verify-or-decrypt (handle)
  (mm-remove-parts (cdr handle))
  (let ((region (mm-handle-multipart-ctl-parameter handle 'gnus-region))
	point (inhibit-read-only t))
    (if region
	(goto-char (car region)))
    (setq point (point))
    (with-current-buffer (mm-handle-multipart-original-buffer handle)
      (let* ((mm-verify-option 'known)
	     (mm-decrypt-option 'known)
	     (nparts (mm-possibly-verify-or-decrypt (cdr handle) handle)))
	(unless (eq nparts (cdr handle))
	  (mm-destroy-parts (cdr handle))
	  (setcdr handle nparts))))
    (gnus-mime-display-security handle)
    (when region
      (delete-region (point) (cdr region))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil))
    (goto-char point)))

(defun gnus-mime-security-show-details (handle)
  (let ((details (mm-handle-multipart-ctl-parameter handle 'gnus-details)))
    (if (not details)
	(gnus-message 5 "No details.")
      (if gnus-mime-security-show-details-inline
	  (let ((gnus-mime-security-button-pressed
		 (not (get-text-property (point) 'gnus-mime-details)))
		(gnus-mime-security-button-line-format
		 (get-text-property (point) 'gnus-line-format))
		(inhibit-read-only t))
	    (forward-char -1)
	    (while (eq (get-text-property (point) 'gnus-line-format)
		       gnus-mime-security-button-line-format)
	      (forward-char -1))
	    (forward-char)
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (gnus-insert-mime-security-button handle))
	    (delete-region (point)
			   (or (text-property-not-all
				(point) (point-max)
				'gnus-line-format
				gnus-mime-security-button-line-format)
			       (point-max))))
	;; Not inlined.
	(if (gnus-buffer-live-p gnus-mime-security-details-buffer)
	    (with-current-buffer gnus-mime-security-details-buffer
	      (erase-buffer)
	      t)
	  (setq gnus-mime-security-details-buffer
		(gnus-get-buffer-create "*MIME Security Details*")))
	(with-current-buffer gnus-mime-security-details-buffer
	  (insert details)
	  (goto-char (point-min)))
	(pop-to-buffer gnus-mime-security-details-buffer)))))

(defun gnus-mime-security-press-button (handle)
  (save-excursion
    (if (mm-handle-multipart-ctl-parameter handle 'gnus-info)
	(gnus-mime-security-show-details handle)
      (gnus-mime-security-verify-or-decrypt handle))))

(defun gnus-insert-mime-security-button (handle &optional displayed)
  (let* ((protocol (mm-handle-multipart-ctl-parameter handle 'protocol))
	 (gnus-tmp-type
	  (concat
	   (or (nth 2 (assoc protocol mm-verify-function-alist))
	       (nth 2 (assoc protocol mm-decrypt-function-alist))
	       "Unknown")
	   (if (equal (car handle) "multipart/signed")
	       " Signed" " Encrypted")
	   " Part"))
	 (gnus-tmp-info
	  (or (mm-handle-multipart-ctl-parameter handle 'gnus-info)
	      "Undecided"))
	 (gnus-tmp-details
	  (mm-handle-multipart-ctl-parameter handle 'gnus-details))
	 gnus-tmp-pressed-details
	 b e)
    (setq gnus-tmp-details
	  (if gnus-tmp-details
	      (concat "\n" gnus-tmp-details)
	    ""))
    (setq gnus-tmp-pressed-details
	  (if gnus-mime-security-button-pressed gnus-tmp-details ""))
    (unless (bolp)
      (insert "\n"))
    (setq b (point))
    (gnus-eval-format
     gnus-mime-security-button-line-format
     gnus-mime-security-button-line-format-alist
     `(keymap ,gnus-mime-security-button-map
	 gnus-callback gnus-mime-security-press-button
	 gnus-line-format ,gnus-mime-security-button-line-format
	 gnus-mime-details ,gnus-mime-security-button-pressed
	 article-type annotation
	 gnus-data ,handle))
    (setq e (if (bolp)
		;; Exclude a newline.
		(1- (point))
	      (point)))
    (when gnus-article-button-face
      (gnus-overlay-put (gnus-make-overlay b e nil t)
                        'face gnus-article-button-face))
    (widget-convert-button
     'link b e
     :mime-handle handle
     :action 'gnus-widget-press-button
     :button-keymap gnus-mime-security-button-map
     :help-echo
     (lambda (widget)
       ;; Needed to properly clear the message due to a bug in
       ;; wid-edit (XEmacs only).
       (when (boundp 'help-echo-owns-message)
	 (setq help-echo-owns-message t))
       (format
	"%S: show detail; %S: more options"
	(aref gnus-mouse-2 0)
	(aref gnus-down-mouse-3 0))))))

(defun gnus-mime-display-security (handle)
  (save-restriction
    (narrow-to-region (point) (point))
    (unless (gnus-unbuttonized-mime-type-p (car handle))
      (gnus-insert-mime-security-button handle))
    (gnus-mime-display-part (cadr handle))
    (unless (bolp)
      (insert "\n"))
    (unless (gnus-unbuttonized-mime-type-p (car handle))
      (let ((gnus-mime-security-button-line-format
	     gnus-mime-security-button-end-line-format))
	(gnus-insert-mime-security-button handle)))
    (mm-set-handle-multipart-parameter
     handle 'gnus-region
     (cons (set-marker (make-marker) (point-min))
	   (set-marker (make-marker) (point-max))))
    (goto-char (point-max))))

(defun gnus-mime-security-run-function (function)
  "Run FUNCTION with the security part under point."
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data))
	buffer handle)
    (when (and (stringp (car-safe data))
	       (setq buffer (mm-handle-multipart-original-buffer data))
	       (setq handle (cadr data)))
      (if (bufferp (mm-handle-buffer handle))
	  (progn
	    (setq handle (cons buffer (copy-sequence (cdr handle))))
	    (mm-handle-set-undisplayer handle nil))
	(setq handle (mm-make-handle
		      buffer
		      (mm-handle-multipart-ctl-parameter handle 'protocol)
		      nil nil nil nil nil nil)))
      (funcall function handle))))

(defun gnus-mime-security-save-part ()
  "Save the security part under point."
  (interactive)
  (gnus-mime-security-run-function 'mm-save-part))

(defun gnus-mime-security-pipe-part ()
  "Pipe the security part under point to a process."
  (interactive)
  (gnus-mime-security-run-function 'mm-pipe-part))

(gnus-ems-redefine)

(provide 'gnus-art)

(run-hooks 'gnus-art-load-hook)

;;; gnus-art.el ends here
