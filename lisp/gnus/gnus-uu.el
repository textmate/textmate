;;; gnus-uu.el --- extract (uu)encoded files in Gnus

;; Copyright (C) 1985-1987, 1993-1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Created: 2 Oct 1993
;; Keyword: news

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

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-art)
(require 'message)
(require 'gnus-msg)
(require 'mm-decode)
(require 'yenc)

(defgroup gnus-extract nil
  "Extracting encoded files."
  :prefix "gnus-uu-"
  :group 'gnus)

(defgroup gnus-extract-view nil
  "Viewing extracted files."
  :group 'gnus-extract)

(defgroup gnus-extract-archive nil
  "Extracting encoded archives."
  :group 'gnus-extract)

(defgroup gnus-extract-post nil
  "Extracting encoded archives."
  :prefix "gnus-uu-post"
  :group 'gnus-extract)

;; Default viewing action rules

(defcustom gnus-uu-default-view-rules
  '(("\\.te?xt$\\|\\.doc$\\|read.*me\\|\\.c?$\\|\\.h$\\|\\.bat$\\|\\.asm$\\|makefile" "cat %s | sed 's/\r$//'")
    ("\\.pas$" "cat %s | sed 's/\r$//'")
    ("\\.[1-9]$" "groff -mandoc -Tascii %s | sed s/\b.//g")
    ("\\.\\(jpe?g\\|gif\\|tiff?\\|p[pgb]m\\|xwd\\|xbm\\|pcx\\)$" "display")
    ("\\.tga$" "tgatoppm %s | ee -")
    ("\\.\\(wav\\|aiff\\|hcom\\|u[blw]\\|s[bfw]\\|voc\\|smp\\)$"
     "sox -v .5 %s -t .au -u - > /dev/audio")
    ("\\.au$" "cat %s > /dev/audio")
    ("\\.midi?$" "playmidi -f")
    ("\\.mod$" "str32")
    ("\\.ps$" "ghostview")
    ("\\.dvi$" "xdvi")
    ("\\.html$" "xmosaic")
    ("\\.mpe?g$" "mpeg_play")
    ("\\.\\(flc\\|fli\\|rle\\|iff\\|pfx\\|avi\\|sme\\|rpza\\|dl\\|qt\\|rsrc\\|mov\\)$" "xanim")
    ("\\.\\(tar\\|arj\\|zip\\|zoo\\|arc\\|gz\\|Z\\|lzh\\|ar\\|lha\\)$"
     "gnus-uu-archive"))
  "*Default actions to be taken when the user asks to view a file.
To change the behavior, you can either edit this variable or set
`gnus-uu-user-view-rules' to something useful.

For example:

To make gnus-uu use 'xli' to display JPEG and GIF files, put the
following in your .emacs file:

  (setq gnus-uu-user-view-rules '((\"jpg$\\\\|gif$\" \"xli\")))

Both these variables are lists of lists with two string elements.  The
first string is a regular expression.  If the file name matches this
regular expression, the command in the second string is executed with
the file as an argument.

If the command string contains \"%s\", the file name will be inserted
at that point in the command string.  If there's no \"%s\" in the
command string, the file name will be appended to the command string
before executing.

There are several user variables to tailor the behavior of gnus-uu to
your needs.  First we have `gnus-uu-user-view-rules', which is the
variable gnus-uu first consults when trying to decide how to view a
file.  If this variable contains no matches, gnus-uu examines the
default rule variable provided in this package.  If gnus-uu finds no
match here, it uses `gnus-uu-user-view-rules-end' to try to make a
match."
  :group 'gnus-extract-view
  :type '(repeat (group regexp (string :tag "Command"))))

(defcustom gnus-uu-user-view-rules nil
  "What actions are to be taken to view a file.
See the documentation on the `gnus-uu-default-view-rules' variable for
details."
  :group 'gnus-extract-view
  :type '(repeat (group regexp (string :tag "Command"))))

(defcustom gnus-uu-user-view-rules-end
  '(("" "file"))
  "*What actions are to be taken if no rule matched the file name.
See the documentation on the `gnus-uu-default-view-rules' variable for
details."
  :group 'gnus-extract-view
  :type '(repeat (group regexp (string :tag "Command"))))

;; Default unpacking commands

(defcustom gnus-uu-default-archive-rules
  '(("\\.tar$" "tar xf")
    ("\\.zip$" "unzip -o")
    ("\\.ar$" "ar x")
    ("\\.arj$" "unarj x")
    ("\\.zoo$" "zoo -e")
    ("\\.\\(lzh\\|lha\\)$" "lha x")
    ("\\.Z$" "uncompress")
    ("\\.gz$" "gunzip")
    ("\\.arc$" "arc -x"))
  "*See `gnus-uu-user-archive-rules'."
  :group 'gnus-extract-archive
  :type '(repeat (group regexp (string :tag "Command"))))

(defvar gnus-uu-destructive-archivers
  (list "uncompress" "gunzip"))

(defcustom gnus-uu-user-archive-rules nil
  "A list that can be set to override the default archive unpacking commands.
To use, for instance, 'untar' to unpack tar files and 'zip -x' to
unpack zip files, say the following:
  (setq gnus-uu-user-archive-rules
    '((\"\\\\.tar$\" \"untar\")
      (\"\\\\.zip$\" \"zip -x\")))"
  :group 'gnus-extract-archive
  :type '(repeat (group regexp (string :tag "Command"))))

(defcustom gnus-uu-ignore-files-by-name nil
  "*A regular expression saying what files should not be viewed based on name.
If, for instance, you want gnus-uu to ignore all .au and .wav files,
you could say something like

  (setq gnus-uu-ignore-files-by-name \"\\\\.au$\\\\|\\\\.wav$\")

Note that this variable can be used in conjunction with the
`gnus-uu-ignore-files-by-type' variable."
  :group 'gnus-extract
  :type '(choice (const :tag "off" nil)
		 (regexp :format "%v")))

(defcustom gnus-uu-ignore-files-by-type nil
  "*A regular expression saying what files that shouldn't be viewed, based on MIME file type.
If, for instance, you want gnus-uu to ignore all audio files and all mpegs,
you could say something like

  (setq gnus-uu-ignore-files-by-type \"audio/\\\\|video/mpeg\")

Note that this variable can be used in conjunction with the
`gnus-uu-ignore-files-by-name' variable."
  :group 'gnus-extract
  :type '(choice (const :tag "off" nil)
		 (regexp :format "%v")))

;; Pseudo-MIME support

(defconst gnus-uu-ext-to-mime-list
  '(("\\.gif$" "image/gif")
    ("\\.jpe?g$" "image/jpeg")
    ("\\.tiff?$" "image/tiff")
    ("\\.xwd$" "image/xwd")
    ("\\.pbm$" "image/pbm")
    ("\\.pgm$" "image/pgm")
    ("\\.ppm$" "image/ppm")
    ("\\.xbm$" "image/xbm")
    ("\\.pcx$" "image/pcx")
    ("\\.tga$" "image/tga")
    ("\\.ps$" "image/postscript")
    ("\\.fli$" "video/fli")
    ("\\.wav$" "audio/wav")
    ("\\.aiff$" "audio/aiff")
    ("\\.hcom$" "audio/hcom")
    ("\\.voc$" "audio/voc")
    ("\\.smp$" "audio/smp")
    ("\\.mod$" "audio/mod")
    ("\\.dvi$" "image/dvi")
    ("\\.mpe?g$" "video/mpeg")
    ("\\.au$" "audio/basic")
    ("\\.\\(te?xt\\|doc\\|c\\|h\\)$" "text/plain")
    ("\\.\\(c\\|h\\)$" "text/source")
    ("read.*me" "text/plain")
    ("\\.html$" "text/html")
    ("\\.bat$" "text/bat")
    ("\\.[1-6]$" "text/man")
    ("\\.flc$" "video/flc")
    ("\\.rle$" "video/rle")
    ("\\.pfx$" "video/pfx")
    ("\\.avi$" "video/avi")
    ("\\.sme$" "video/sme")
    ("\\.rpza$" "video/prza")
    ("\\.dl$" "video/dl")
    ("\\.qt$" "video/qt")
    ("\\.rsrc$" "video/rsrc")
    ("\\..*$" "unknown/unknown")))

;; Various variables users may set

(defcustom gnus-uu-tmp-dir
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	("/tmp/"))
  "*Variable saying where gnus-uu is to do its work.
Default is \"/tmp/\"."
  :group 'gnus-extract
  :type 'directory)

(defcustom gnus-uu-do-not-unpack-archives nil
  "*Non-nil means that gnus-uu won't peek inside archives looking for files to display.
Default is nil."
  :group 'gnus-extract-archive
  :type 'boolean)

(defcustom gnus-uu-ignore-default-view-rules nil
  "*Non-nil means that gnus-uu will ignore the default viewing rules.
Only the user viewing rules will be consulted.  Default is nil."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-uu-grabbed-file-functions nil
  "Functions run on each file after successful decoding.
They will be called with the name of the file as the argument.
Likely functions you can use in this list are `gnus-uu-grab-view'
and `gnus-uu-grab-move'."
  :group 'gnus-extract
  :options '(gnus-uu-grab-view gnus-uu-grab-move)
  :type 'hook)

(defcustom gnus-uu-ignore-default-archive-rules nil
  "*Non-nil means that gnus-uu will ignore the default archive unpacking commands.
Only the user unpacking commands will be consulted.  Default is nil."
  :group 'gnus-extract-archive
  :type 'boolean)

(defcustom gnus-uu-kill-carriage-return t
  "*Non-nil means that gnus-uu will strip all carriage returns from articles.
Default is t."
  :group 'gnus-extract
  :type 'boolean)

(defcustom gnus-uu-view-with-metamail nil
  "*Non-nil means that files will be viewed with metamail.
The gnus-uu viewing functions will be ignored and gnus-uu will try
to guess at a content-type based on file name suffixes.  Default
it nil."
  :group 'gnus-extract
  :type 'boolean)

(defcustom gnus-uu-unmark-articles-not-decoded nil
  "*Non-nil means that gnus-uu will mark articles that were unsuccessfully decoded as unread.
Default is nil."
  :group 'gnus-extract
  :type 'boolean)

(defcustom gnus-uu-correct-stripped-uucode nil
  "*Non-nil means that gnus-uu will *try* to fix uuencoded files that have had trailing spaces deleted.
Default is nil."
  :group 'gnus-extract
  :type 'boolean)

(defcustom gnus-uu-save-in-digest nil
  "*Non-nil means that gnus-uu, when asked to save without decoding, will save in digests.
If this variable is nil, gnus-uu will just save everything in a
file without any embellishments.  The digesting almost conforms to RFC1153 -
no easy way to specify any meaningful volume and issue numbers were found,
so I simply dropped them."
  :group 'gnus-extract
  :type 'boolean)

(defcustom gnus-uu-pre-uudecode-hook nil
  "Hook run before sending a message to uudecode."
  :group 'gnus-extract
  :type 'hook)

(defcustom gnus-uu-digest-headers
  '("^Date:" "^From:" "^To:" "^Cc:" "^Subject:" "^Message-ID:" "^Keywords:"
    "^Summary:" "^References:" "^Content-Type:" "^Content-Transfer-Encoding:"
    "^MIME-Version:" "^Content-Disposition:" "^Content-Description:"
    "^Content-ID:")
  "*List of regexps to match headers included in digested messages.
The headers will be included in the sequence they are matched.  If nil
include all headers."
  :group 'gnus-extract
  :type '(repeat regexp))

(defcustom gnus-uu-save-separate-articles nil
  "*Non-nil means that gnus-uu will save articles in separate files."
  :group 'gnus-extract
  :type 'boolean)

(defcustom gnus-uu-be-dangerous 'ask
  "*Specifies what to do if unusual situations arise during decoding.
If nil, be as conservative as possible.  If t, ignore things that
didn't work, and overwrite existing files.  Otherwise, ask each time."
  :group 'gnus-extract
  :type '(choice (const :tag "conservative" nil)
		 (const :tag "ask" ask)
		 (const :tag "liberal" t)))

;; Internal variables

(defvar gnus-uu-saved-article-name nil)

(defvar gnus-uu-begin-string "^begin[ \t]+0?[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defvar gnus-uu-end-string "^end[ \t]*$")

(defvar gnus-uu-body-line "^M")
(let ((i 61))
  (while (> (setq i (1- i)) 0)
    (setq gnus-uu-body-line (concat gnus-uu-body-line "[^a-z]")))
  (setq gnus-uu-body-line (concat gnus-uu-body-line ".?$")))

;"^M.............................................................?$"

(defvar gnus-uu-shar-begin-string "^#! */bin/sh")

(defvar gnus-uu-shar-name-marker
  "begin 0?[0-7][0-7][0-7][ \t]+\\(\\(\\w\\|[.\\:]\\)*\\b\\)")

(defvar gnus-uu-postscript-begin-string "^%!PS-")
(defvar gnus-uu-postscript-end-string "^%%EOF$")

(defvar gnus-uu-file-name nil)
(defvar gnus-uu-uudecode-process nil)
(defvar gnus-uu-binhex-article-name nil)
(defvar gnus-uu-yenc-article-name nil)

(defvar gnus-uu-work-dir nil)

(defvar gnus-uu-output-buffer-name " *Gnus UU Output*")

(defvar gnus-uu-default-dir gnus-article-save-directory)
(defvar gnus-uu-digest-from-subject nil)
(defvar gnus-uu-digest-buffer nil)

;; Commands.

(defun gnus-uu-decode-uu (&optional n)
  "Uudecodes the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-uustrip-article n))

(defun gnus-uu-decode-uu-and-save (n dir)
  "Decodes and saves the resulting file."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-directory-name "Uudecode and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-uustrip-article n dir nil nil t))

(defun gnus-uu-decode-unshar (&optional n)
  "Unshars the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-unshar-article n nil nil 'scan t))

(defun gnus-uu-decode-unshar-and-save (n dir)
  "Unshars and saves the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-directory-name "Unshar and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-unshar-article n dir nil 'scan t))

(defun gnus-uu-decode-save (n file)
  "Saves the current article."
  (interactive
   (list current-prefix-arg
	 (if gnus-uu-save-separate-articles
	     (read-directory-name
	      "Save articles in dir: " gnus-uu-default-dir gnus-uu-default-dir)
	   (read-file-name
	    "Save article in file: " gnus-uu-default-dir gnus-uu-default-dir))))
  (setq gnus-uu-saved-article-name file)
  (gnus-uu-decode-with-method 'gnus-uu-save-article n nil t))

(defun gnus-uu-decode-binhex (n dir)
  "Unbinhexes the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-directory-name "Unbinhex and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir))))
  (setq gnus-uu-binhex-article-name
	(mm-make-temp-file (expand-file-name "binhex" gnus-uu-work-dir)))
  (gnus-uu-decode-with-method 'gnus-uu-binhex-article n dir))

(defun gnus-uu-decode-yenc (n dir)
  "Decode the yEnc-encoded current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-directory-name "yEnc decode and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir))))
  (setq gnus-uu-yenc-article-name nil)
  (gnus-uu-decode-with-method 'gnus-uu-yenc-article n dir nil t))

(defun gnus-uu-decode-uu-view (&optional n)
  "Uudecodes and views the current article."
  (interactive "P")
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-uu n)))

(defun gnus-uu-decode-uu-and-save-view (n dir)
  "Decodes, views and saves the resulting file."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Uudecode, view and save in dir: "
			 gnus-uu-default-dir
			 gnus-uu-default-dir t)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-uu-and-save n dir)))

(defun gnus-uu-decode-unshar-view (&optional n)
  "Unshars and views the current article."
  (interactive "P")
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-unshar n)))

(defun gnus-uu-decode-unshar-and-save-view (n dir)
  "Unshars and saves the current article."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Unshar, view and save in dir: "
			 gnus-uu-default-dir
			 gnus-uu-default-dir t)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-unshar-and-save n dir)))

(defun gnus-uu-decode-save-view (n file)
  "Saves and views the current article."
  (interactive
   (list current-prefix-arg
	 (if gnus-uu-save-separate-articles
	     (read-directory-name "Save articles in dir: "
				  gnus-uu-default-dir gnus-uu-default-dir)
	   (read-file-name "Save articles in file: "
			   gnus-uu-default-dir gnus-uu-default-dir))))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-save n file)))

(defun gnus-uu-decode-binhex-view (n file)
  "Unbinhexes and views the current article."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Unbinhex, view and save in dir: "
			 gnus-uu-default-dir gnus-uu-default-dir)))
  (setq gnus-uu-binhex-article-name
	(mm-make-temp-file (expand-file-name "binhex" gnus-uu-work-dir)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-binhex n file)))


;; Digest and forward articles

(defun gnus-uu-digest-mail-forward (&optional n post)
  "Digests and forwards all articles in this series."
  (interactive "P")
  (let ((gnus-uu-save-in-digest t)
	(file (mm-make-temp-file (nnheader-concat gnus-uu-tmp-dir "forward")))
	(message-forward-as-mime message-forward-as-mime)
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets)
	gnus-uu-digest-buffer subject from)
    (if (and n (not (numberp n)))
	(setq message-forward-as-mime (not message-forward-as-mime)
	      n nil))
    (let ((gnus-article-reply (gnus-summary-work-articles n)))
      (when (and (not n)
		 (= (length gnus-article-reply) 1))
	;; The case where neither a number of articles nor a region is
	;; specified.
	(gnus-summary-top-thread)
	(setq gnus-article-reply (nreverse (gnus-uu-find-articles-matching))))
      (gnus-setup-message 'forward
	(setq gnus-uu-digest-from-subject nil)
	(setq gnus-uu-digest-buffer
	      (gnus-get-buffer-create " *gnus-uu-forward*"))
	;; Specify articles to be forwarded.  Note that they should be
	;; reversed; see `gnus-uu-get-list-of-articles'.
	(let ((gnus-newsgroup-processable (reverse gnus-article-reply)))
	  (gnus-uu-decode-save n file)
	  (setq gnus-article-reply gnus-newsgroup-processable))
	;; Restore the value of `gnus-newsgroup-processable' to which
	;; it should be set when it is not `let'-bound.
	(setq gnus-newsgroup-processable (reverse gnus-article-reply))
	(switch-to-buffer gnus-uu-digest-buffer)
	(let ((fs gnus-uu-digest-from-subject))
	  (when fs
	    (setq from (caar fs)
		  subject (gnus-simplify-subject-fuzzy (cdar fs))
		  fs (cdr fs))
	    (while (and fs (or from subject))
	      (when from
		(unless (string= from (caar fs))
		  (setq from nil)))
	      (when subject
		(unless (string= (gnus-simplify-subject-fuzzy (cdar fs))
				 subject)
		  (setq subject nil)))
	      (setq fs (cdr fs))))
	  (unless subject
	    (setq subject "Digested Articles"))
	  (unless from
	    (setq from
		  (if (gnus-news-group-p gnus-newsgroup-name)
		      gnus-newsgroup-name
		    "Various"))))
	(goto-char (point-min))
	(when (re-search-forward "^Subject: ")
	  (delete-region (point) (point-at-eol))
	  (insert subject))
	(goto-char (point-min))
	(when (re-search-forward "^From:")
	  (delete-region (point) (point-at-eol))
	  (insert " " from))
	(let ((message-forward-decoded-p t))
	  (message-forward post t))))
    (setq gnus-uu-digest-from-subject nil)))

(defun gnus-uu-digest-post-forward (&optional n)
  "Digest and forward to a newsgroup."
  (interactive "P")
  (gnus-uu-digest-mail-forward n t))

;; Process marking.

(defun gnus-message-process-mark (unmarkp new-marked)
  (let ((old (- (length gnus-newsgroup-processable) (length new-marked))))
    (gnus-message 6 "%d mark%s %s%s"
		  (length new-marked)
		  (if (= (length new-marked) 1) "" "s")
		  (if unmarkp "removed" "added")
		  (cond
		   ((and (zerop old)
			 (not unmarkp))
		    "")
		   (unmarkp
		    (format ", %d remain marked"
			    (length gnus-newsgroup-processable)))
		   (t
		    (format ", %d already marked" old))))))

(defun gnus-new-processable (unmarkp articles)
  (if unmarkp
      (gnus-intersection gnus-newsgroup-processable articles)
    (gnus-set-difference articles gnus-newsgroup-processable)))

(defun gnus-uu-mark-by-regexp (regexp &optional unmark)
  "Set the process mark on articles whose subjects match REGEXP.
When called interactively, prompt for REGEXP.
Optional UNMARK non-nil means unmark instead of mark."
  (interactive "sMark (regexp): \nP")
  (save-excursion
    (let* ((articles (gnus-uu-find-articles-matching regexp))
	   (new-marked (gnus-new-processable unmark articles)))
      (while articles
	(if unmark
	    (gnus-summary-remove-process-mark (pop articles))
	  (gnus-summary-set-process-mark (pop articles))))
      (gnus-message-process-mark unmark new-marked)))
  (gnus-summary-position-point))

(defun gnus-uu-unmark-by-regexp (regexp)
  "Remove the process mark from articles whose subjects match REGEXP.
When called interactively, prompt for REGEXP."
  (interactive "sUnmark (regexp): ")
  (gnus-uu-mark-by-regexp regexp t))

(defun gnus-uu-mark-series (&optional silent)
  "Mark the current series with the process mark."
  (interactive)
  (let* ((articles (gnus-uu-find-articles-matching))
	 (l (length articles)))
    (while articles
      (gnus-summary-set-process-mark (car articles))
      (setq articles (cdr articles)))
    (unless silent
      (gnus-message 6 "Marked %d articles" l))
    (gnus-summary-position-point)
    l))

(defun gnus-uu-mark-region (beg end &optional unmark)
  "Set the process mark on all articles between point and mark."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if unmark
	  (gnus-summary-remove-process-mark (gnus-summary-article-number))
	(gnus-summary-set-process-mark (gnus-summary-article-number)))
      (forward-line 1)))
  (gnus-summary-position-point))

(defun gnus-uu-unmark-region (beg end)
  "Remove the process mark from all articles between point and mark."
  (interactive "r")
  (gnus-uu-mark-region beg end t))

(defun gnus-uu-mark-buffer ()
  "Set the process mark on all articles in the buffer."
  (interactive)
  (gnus-uu-mark-region (point-min) (point-max)))

(defun gnus-uu-unmark-buffer ()
  "Remove the process mark on all articles in the buffer."
  (interactive)
  (gnus-uu-mark-region (point-min) (point-max) t))

(defun gnus-uu-mark-thread ()
  "Marks all articles downwards in this thread."
  (interactive)
  (gnus-save-hidden-threads
    (let ((level (gnus-summary-thread-level)))
      (while (and (gnus-summary-set-process-mark
		   (gnus-summary-article-number))
		  (zerop (gnus-summary-next-subject 1 nil t))
		  (> (gnus-summary-thread-level) level)))))
  (gnus-summary-position-point))

(defun gnus-uu-unmark-thread ()
  "Unmarks all articles downwards in this thread."
  (interactive)
  (let ((level (gnus-summary-thread-level)))
    (while (and (gnus-summary-remove-process-mark
		 (gnus-summary-article-number))
		(zerop (gnus-summary-next-subject 1))
		(> (gnus-summary-thread-level) level))))
  (gnus-summary-position-point))

(defun gnus-uu-invert-processable ()
  "Invert the list of process-marked articles."
  (interactive)
  (let ((data gnus-newsgroup-data)
	number)
    (save-excursion
      (while data
	(if (memq (setq number (gnus-data-number (pop data)))
		  gnus-newsgroup-processable)
	    (gnus-summary-remove-process-mark number)
	  (gnus-summary-set-process-mark number)))))
  (gnus-summary-position-point))

(defun gnus-uu-mark-over (&optional score)
  "Mark all articles with a score over SCORE (the prefix)."
  (interactive "P")
  (let ((score (or score gnus-summary-default-score 0))
	(data gnus-newsgroup-data))
    (save-excursion
      (while data
	(when (> (or (cdr (assq (gnus-data-number (car data))
				gnus-newsgroup-scored))
		     gnus-summary-default-score 0)
		 score)
	  (gnus-summary-set-process-mark (caar data)))
	(setq data (cdr data))))
    (gnus-summary-position-point)))

(defun gnus-uu-mark-sparse ()
  "Mark all series that have some articles marked."
  (interactive)
  (let ((marked (nreverse gnus-newsgroup-processable))
	subject articles total headers)
    (unless marked
      (error "No articles marked with the process mark"))
    (setq gnus-newsgroup-processable nil)
    (save-excursion
      (while marked
	(and (vectorp (setq headers
			    (gnus-summary-article-header (car marked))))
	     (setq subject (mail-header-subject headers)
		   articles (gnus-uu-find-articles-matching
			     (gnus-uu-reginize-string subject))
		   total (nconc total articles)))
	(while articles
	  (gnus-summary-set-process-mark (car articles))
	  (setcdr marked (delq (car articles) (cdr marked)))
	  (setq articles (cdr articles)))
	(setq marked (cdr marked)))
      (setq gnus-newsgroup-processable (nreverse total)))
    (gnus-summary-position-point)))

(defun gnus-uu-mark-all ()
  "Mark all articles in \"series\" order."
  (interactive)
  (setq gnus-newsgroup-processable nil)
  (save-excursion
    (let ((data gnus-newsgroup-data)
	  (count 0)
	  number)
      (while data
	(when (and (not (memq (setq number (gnus-data-number (car data)))
			      gnus-newsgroup-processable))
		   (vectorp (gnus-data-header (car data))))
	  (gnus-summary-goto-subject number)
	  (setq count (+ count (gnus-uu-mark-series t))))
	(setq data (cdr data)))
      (gnus-message 6 "Marked %d articles" count)))
  (gnus-summary-position-point))

;; All PostScript functions written by Erik Selberg <speed@cs.washington.edu>.

(defun gnus-uu-decode-postscript (&optional n)
  "Gets PostScript of the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-decode-postscript-article n))

(defun gnus-uu-decode-postscript-view (&optional n)
  "Gets and views the current article."
  (interactive "P")
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-postscript n)))

(defun gnus-uu-decode-postscript-and-save (n dir)
  "Extracts PostScript and saves the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-directory-name "Save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-decode-postscript-article
			      n dir nil nil t))

(defun gnus-uu-decode-postscript-and-save-view (n dir)
  "Decodes, views and saves the resulting file."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Where do you want to save the file(s)? "
			 gnus-uu-default-dir
			 gnus-uu-default-dir t)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-postscript-and-save n dir)))


;; Internal functions.

(defun gnus-uu-decode-with-method (method n &optional save not-insert
					  scan cdir)
  (gnus-uu-initialize scan)
  (when save
    (setq gnus-uu-default-dir save))
  ;; Create the directory we save to.
  (when (and scan cdir save
	     (not (file-exists-p save)))
    (make-directory save t))
  (let ((articles (gnus-uu-get-list-of-articles n))
	files)
    (setq files (gnus-uu-grab-articles articles method t))
    (let ((gnus-current-article (car articles)))
      (when scan
	(setq files (gnus-uu-scan-directory gnus-uu-work-dir))))
    (when save
      (gnus-uu-save-files files save))
    (when (eq gnus-uu-do-not-unpack-archives nil)
      (setq files (gnus-uu-unpack-files files)))
    (setq files (nreverse (gnus-uu-get-actions files)))
    (or not-insert (not gnus-insert-pseudo-articles)
	(gnus-summary-insert-pseudos files save))))

(defun gnus-uu-scan-directory (dir &optional rec)
  "Return a list of all files under DIR."
  (let ((files (directory-files dir t))
	out file)
    (while (setq file (pop files))
      (unless (member (file-name-nondirectory file) '("." ".."))
	(push (list (cons 'name file)
		    (cons 'article gnus-current-article))
	      out)
	(when (file-directory-p file)
	  (setq out (nconc (gnus-uu-scan-directory file t) out)))))
    (if rec
	out
      (nreverse out))))

(defun gnus-uu-save-files (files dir)
  "Save FILES in DIR."
  (let ((len (length files))
	(reg (concat "^" (regexp-quote gnus-uu-work-dir)))
	to-file file fromdir)
    (while (setq file (cdr (assq 'name (pop files))))
      (when (file-exists-p file)
	(string-match reg file)
	(setq fromdir (substring file (match-end 0)))
	(if (file-directory-p file)
	    (gnus-make-directory (concat dir fromdir))
	  (setq to-file (concat dir fromdir))
	  (when (or (not (file-exists-p to-file))
		    (eq gnus-uu-be-dangerous t)
		    (and gnus-uu-be-dangerous
			 (gnus-y-or-n-p (format "%s exists; overwrite? "
						to-file))))
	    (copy-file file to-file t t)))))
    (gnus-message 5 "Saved %d file%s" len (if (= len 1) "" "s"))))

;; Functions for saving and possibly digesting articles without
;; any decoding.

;; Function called by gnus-uu-grab-articles to treat each article.
(defun gnus-uu-save-article (buffer in-state)
  (cond
   (gnus-uu-save-separate-articles
    (with-current-buffer buffer
      (let ((coding-system-for-write mm-text-coding-system))
	(gnus-write-buffer
	 (concat gnus-uu-saved-article-name gnus-current-article)))
      (cond ((eq in-state 'first) (list gnus-uu-saved-article-name 'begin))
	    ((eq in-state 'first-and-last) (list gnus-uu-saved-article-name
						 'begin 'end))
	    ((eq in-state 'last) (list 'end))
	    (t (list 'middle)))))
   ((not gnus-uu-save-in-digest)
    (with-current-buffer buffer
      (write-region (point-min) (point-max) gnus-uu-saved-article-name t)
      (cond ((eq in-state 'first) (list gnus-uu-saved-article-name 'begin))
	    ((eq in-state 'first-and-last) (list gnus-uu-saved-article-name
						 'begin 'end))
	    ((eq in-state 'last) (list 'end))
	    (t (list 'middle)))))
   (t
    (let ((header (gnus-summary-article-header)))
      (push (cons (mail-header-from header)
		  (mail-header-subject header))
	    gnus-uu-digest-from-subject))
    (let ((name (file-name-nondirectory gnus-uu-saved-article-name))
	  beg subj headers headline sorthead body end-string state)
      (if (or (eq in-state 'first)
	      (eq in-state 'first-and-last))
	  (progn
	    (setq state (list 'begin))
	    (with-current-buffer (gnus-get-buffer-create "*gnus-uu-body*")
	      (erase-buffer))
	    (with-current-buffer (gnus-get-buffer-create "*gnus-uu-pre*")
	      (erase-buffer)
	      (insert (format
		       "Date: %s\nFrom: %s\nSubject: %s Digest\n\n"
		       (message-make-date) name name))
	      (when (and message-forward-as-mime gnus-uu-digest-buffer)
		(insert
		 "<#mml type=message/rfc822>\nSubject: Topics\n\n<#/mml>\n")
		(forward-line -1))
	      (insert "Topics:\n")))
	(when (not (eq in-state 'end))
	  (setq state (list 'middle))))
      (with-current-buffer "*gnus-uu-body*"
	(goto-char (setq beg (point-max)))
	(save-excursion
	  (save-restriction
	    (set-buffer buffer)
	    (let (buffer-read-only)
	      (set-text-properties (point-min) (point-max) nil)
	      ;; These two are necessary for XEmacs 19.12 fascism.
	      (put-text-property (point-min) (point-max) 'invisible nil)
	      (put-text-property (point-min) (point-max) 'intangible nil))
	    (when (and message-forward-as-mime
		       message-forward-show-mml
		       gnus-uu-digest-buffer)
	      (mm-enable-multibyte)
	      (mime-to-mml))
	    (goto-char (point-min))
	    (search-forward "\n\n")
	    (unless (and message-forward-as-mime gnus-uu-digest-buffer)
	      ;; Quote all 30-dash lines.
	      (save-excursion
		(while (re-search-forward "^-" nil t)
		  (beginning-of-line)
		  (delete-char 1)
		  (insert "- "))))
	    (setq body (buffer-substring (1- (point)) (point-max)))
	    (narrow-to-region (point-min) (point))
	    (if (not (setq headers gnus-uu-digest-headers))
		(setq sorthead (buffer-string))
	      (while headers
		(setq headline (car headers))
		(setq headers (cdr headers))
		(goto-char (point-min))
		(while (re-search-forward headline nil t)
		  (setq sorthead
			(concat sorthead
				(buffer-substring
				 (match-beginning 0)
				 (or (and (re-search-forward "^[^ \t]" nil t)
					  (1- (point)))
				     (progn (forward-line 1) (point)))))))))
	    (widen)))
	(if (and message-forward-as-mime gnus-uu-digest-buffer)
	  (if message-forward-show-mml
	      (progn
		(insert "\n<#mml type=message/rfc822>\n")
		(insert sorthead) (goto-char (point-max))
		(insert body) (goto-char (point-max))
		(insert "\n<#/mml>\n"))
	    (let ((buf (mml-generate-new-buffer " *mml*")))
	      (with-current-buffer buf
		(insert sorthead)
		(goto-char (point-min))
		(when (re-search-forward "^Subject: \\(.*\\)$" nil t)
		  (setq subj (buffer-substring (match-beginning 1)
					       (match-end 1))))
		(goto-char (point-max))
		(insert body))
	      (insert "\n<#part type=message/rfc822"
		      " buffer=\"" (buffer-name buf) "\">\n")))
	  (insert sorthead) (goto-char (point-max))
	  (insert body) (goto-char (point-max))
	  (insert (concat "\n" (make-string 30 ?-) "\n\n")))
	(goto-char beg)
	(when (re-search-forward "^Subject: \\(.*\\)$" nil t)
	  (setq subj (buffer-substring (match-beginning 1) (match-end 1))))
	(when subj
	  (with-current-buffer "*gnus-uu-pre*"
	    (insert (format "   %s\n" subj)))))
      (when (or (eq in-state 'last)
		(eq in-state 'first-and-last))
	(if (and message-forward-as-mime gnus-uu-digest-buffer)
	    (with-current-buffer gnus-uu-digest-buffer
	      (erase-buffer)
	      (insert-buffer-substring "*gnus-uu-pre*")
	      (goto-char (point-max))
	      (insert-buffer-substring "*gnus-uu-body*"))
	  (with-current-buffer "*gnus-uu-pre*"
	    (insert (format "\n\n%s\n\n" (make-string 70 ?-)))
	    (if gnus-uu-digest-buffer
		(with-current-buffer gnus-uu-digest-buffer
		  (erase-buffer)
		  (insert-buffer-substring "*gnus-uu-pre*"))
	      (let ((coding-system-for-write mm-text-coding-system))
		(gnus-write-buffer gnus-uu-saved-article-name))))
	  (with-current-buffer "*gnus-uu-body*"
	    (goto-char (point-max))
	    (insert
	     (concat (setq end-string (format "End of %s Digest" name))
		     "\n"))
	    (insert (concat (make-string (length end-string) ?*) "\n"))
	    (if gnus-uu-digest-buffer
		(with-current-buffer gnus-uu-digest-buffer
		  (goto-char (point-max))
		  (insert-buffer-substring "*gnus-uu-body*"))
	      (let ((coding-system-for-write mm-text-coding-system)
		    (file-name-coding-system nnmail-pathname-coding-system))
		(write-region
		 (point-min) (point-max) gnus-uu-saved-article-name t)))))
	(gnus-kill-buffer "*gnus-uu-pre*")
	(gnus-kill-buffer "*gnus-uu-body*")
	(push 'end state))
      (if (memq 'begin state)
	  (cons gnus-uu-saved-article-name state)
	state)))))

;; Binhex treatment - not very advanced.

(defvar gnus-uu-binhex-body-line
  "^[^:]...............................................................$")
(defvar gnus-uu-binhex-begin-line
  "^:...............................................................$")
(defvar gnus-uu-binhex-end-line
  ":$")

(defun gnus-uu-binhex-article (buffer in-state)
  (let (state start-char)
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (when (not (re-search-forward gnus-uu-binhex-begin-line nil t))
	(when (not (re-search-forward gnus-uu-binhex-body-line nil t))
	  (setq state (list 'wrong-type))))

      (if (memq 'wrong-type state)
	  ()
	(beginning-of-line)
	(setq start-char (point))
	(if (looking-at gnus-uu-binhex-begin-line)
	    (progn
	      (setq state (list 'begin))
	      (write-region (point-min) (point-min)
			    gnus-uu-binhex-article-name))
	  (setq state (list 'middle)))
	(goto-char (point-max))
	(re-search-backward (concat gnus-uu-binhex-body-line "\\|"
				    gnus-uu-binhex-end-line)
			    nil t)
	(when (looking-at gnus-uu-binhex-end-line)
	  (setq state (if (memq 'begin state)
			  (cons 'end state)
			(list 'end))))
	(beginning-of-line)
	(forward-line 1)
	(when (file-exists-p gnus-uu-binhex-article-name)
	  (mm-append-to-file start-char (point) gnus-uu-binhex-article-name))))
    (if (memq 'begin state)
	(cons gnus-uu-binhex-article-name state)
      state)))

;; yEnc

(defun gnus-uu-yenc-article (buffer in-state)
  (with-current-buffer gnus-original-article-buffer
    (widen)
    (let ((file-name (yenc-extract-filename))
	  state start-char)
      (when (not file-name)
	(setq state (list 'wrong-type)))

      (if (memq 'wrong-type state)
	  ()
	(when (yenc-first-part-p)
	  (setq gnus-uu-yenc-article-name
		(expand-file-name file-name gnus-uu-work-dir))
	  (push 'begin state))
	(when (yenc-last-part-p)
	  (push 'end state))
	(unless state
	  (push 'middle state))
	(mm-with-unibyte-buffer
	  (insert-buffer-substring gnus-original-article-buffer)
	  (yenc-decode-region (point-min) (point-max))
	  (when (and (member 'begin state)
		     (file-exists-p gnus-uu-yenc-article-name))
	    (delete-file gnus-uu-yenc-article-name))
	  (mm-append-to-file (point-min) (point-max)
			     gnus-uu-yenc-article-name)))
      (if (memq 'begin state)
	  (cons file-name state)
	state))))

;; PostScript

(defun gnus-uu-decode-postscript-article (process-buffer in-state)
  (let ((state (list 'ok))
	start-char end-char file-name)
    (with-current-buffer process-buffer
      (goto-char (point-min))
      (if (not (re-search-forward gnus-uu-postscript-begin-string nil t))
	  (setq state (list 'wrong-type))
	(beginning-of-line)
	(setq start-char (point))
	(if (not (re-search-forward gnus-uu-postscript-end-string nil t))
	    (setq state (list 'wrong-type))
	  (setq end-char (point))
	  (set-buffer (gnus-get-buffer-create gnus-uu-output-buffer-name))
	  (insert-buffer-substring process-buffer start-char end-char)
	  (setq file-name (concat gnus-uu-work-dir
				  (cdr gnus-article-current) ".ps"))
	  (write-region (point-min) (point-max) file-name)
	  (setq state (list file-name 'begin 'end)))))
    state))


;; Find actions.

(defun gnus-uu-get-actions (files)
  (let ((ofiles files)
	action name)
    (while files
      (setq name (cdr (assq 'name (car files))))
      (and
       (setq action (gnus-uu-get-action name))
       (setcar files (nconc (list (if (string= action "gnus-uu-archive")
				      (cons 'action "file")
				    (cons 'action action))
				  (cons 'execute (gnus-uu-command
						  action name)))
			    (car files))))
      (setq files (cdr files)))
    ofiles))

(defun gnus-uu-get-action (file-name)
  (let (action)
    (setq action
	  (gnus-uu-choose-action
	   file-name
	   (append
	    gnus-uu-user-view-rules
	    (if gnus-uu-ignore-default-view-rules
		nil
	      gnus-uu-default-view-rules)
	    gnus-uu-user-view-rules-end)))
    (when (and (not (string= (or action "") "gnus-uu-archive"))
	       gnus-uu-view-with-metamail)
      (when (setq action
		  (gnus-uu-choose-action file-name gnus-uu-ext-to-mime-list))
	(setq action (format "metamail -d -b -c \"%s\"" action))))
    action))


;; Functions for treating subjects and collecting series.

(defun gnus-uu-reginize-string (string)
  ;; Takes a string and puts a \ in front of every special character;
  ;; replaces the last thing that looks like "2/3" with "[0-9]+/3"
  ;; or, if it can't find something like that, tries "2 of 3", then
  ;; finally just replaces the next to last number with "[0-9]+".
  (with-current-buffer (gnus-get-buffer-create gnus-uu-output-buffer-name)
    (buffer-disable-undo)
    (erase-buffer)
    (insert (regexp-quote string))

    (setq case-fold-search nil)

    (end-of-line)
    (if (re-search-backward "\\([^0-9]\\)[0-9]+/\\([0-9]+\\)" nil t)
	(replace-match "\\1[0-9]+/\\2")

      (end-of-line)
      (if (re-search-backward "\\([^0-9]\\)[0-9]+[ \t]*of[ \t]*\\([0-9]+\\)"
			      nil t)
	  (replace-match "\\1[0-9]+ of \\2")

	(end-of-line)
	(if (re-search-backward "\\([^0-9]\\)[0-9]+\\([^0-9]+\\)[0-9]+"
				nil t)
	    (replace-match "\\1[0-9]+\\2[0-9]+" t nil nil nil))))

    (goto-char (point-min))
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match "[ \t]+" t t))

    (buffer-string)))

(defun gnus-uu-get-list-of-articles (n)
  ;; If N is non-nil, the article numbers of the N next articles
  ;; will be returned.
  ;; If any articles have been marked as processable, they will be
  ;; returned.
  ;; Failing that, articles that have subjects that are part of the
  ;; same "series" as the current will be returned.
  (let (articles)
    (cond
     (n
      (setq n (prefix-numeric-value n))
      (let ((backward (< n 0))
	    (n (abs n)))
	(save-excursion
	  (while (and (> n 0)
		      (push (gnus-summary-article-number)
			    articles)
		      (gnus-summary-search-forward nil nil backward))
	    (setq n (1- n))))
	(nreverse articles)))
     (gnus-newsgroup-processable
      (reverse gnus-newsgroup-processable))
     (t
      (gnus-uu-find-articles-matching)))))

(defun gnus-uu-string< (l1 l2)
  (string< (car l1) (car l2)))

(defun gnus-uu-find-articles-matching
  (&optional subject only-unread do-not-translate)
  ;; Finds all articles that matches the regexp SUBJECT.  If it is
  ;; nil, the current article name will be used.  If ONLY-UNREAD is
  ;; non-nil, only unread articles are chosen.  If DO-NOT-TRANSLATE is
  ;; non-nil, article names are not equalized before sorting.
  (let ((subject (or subject
		     (gnus-uu-reginize-string (gnus-summary-article-subject))))
	list-of-subjects)
    (save-excursion
      (when subject
	;; Collect all subjects matching subject.
	(let ((case-fold-search t)
	      (data gnus-newsgroup-data)
	      subj mark d)
	  (while data
	    (setq d (pop data))
	    (and (not (gnus-data-pseudo-p d))
		 (or (not only-unread)
		     (= (setq mark (gnus-data-mark d))
			gnus-unread-mark)
		     (= mark gnus-ticked-mark)
		     (= mark gnus-dormant-mark))
		 (setq subj (mail-header-subject (gnus-data-header d)))
		 (string-match subject subj)
		 (push (cons subj (gnus-data-number d))
		       list-of-subjects))))

	;; Expand numbers, sort, and return the list of article
	;; numbers.
	(mapcar 'cdr
		(sort (gnus-uu-expand-numbers
		       list-of-subjects
		       (not do-not-translate))
		      'gnus-uu-string<))))))

(defun gnus-uu-expand-numbers (string-list &optional translate)
  ;; Takes a list of strings and "expands" all numbers in all the
  ;; strings.  That is, this function makes all numbers equal length by
  ;; prepending lots of zeroes before each number.  This is to ease later
  ;; sorting to find out what sequence the articles are supposed to be
  ;; decoded in.  Returns the list of expanded strings.
  (let ((out-list string-list)
	string)
    (with-current-buffer (gnus-get-buffer-create gnus-uu-output-buffer-name)
      (buffer-disable-undo)
      (while string-list
	(erase-buffer)
	(insert (caar string-list))
	;; Translate multiple spaces to one space.
	(goto-char (point-min))
	(while (re-search-forward "[ \t]+" nil t)
	  (replace-match " "))
	;; Translate all characters to "a".
	(goto-char (point-min))
	(when translate
	  (while (re-search-forward "[A-Za-z]" nil t)
	    (replace-match "a" t t)))
	;; Expand numbers.
	(goto-char (point-min))
	(while (re-search-forward "[0-9]+" nil t)
	  (ignore-errors
	    (replace-match
	     (format "%06d"
		     (string-to-number (buffer-substring
				     (match-beginning 0) (match-end 0)))))))
	(setq string (buffer-substring (point-min) (point-max)))
	(setcar (car string-list) string)
	(setq string-list (cdr string-list))))
    out-list))


;; `gnus-uu-grab-articles' is the general multi-article treatment
;; function.  It takes a list of articles to be grabbed and a function
;; to apply to each article.
;;
;; The function to be called should take two parameters.  The first
;; parameter is the article buffer.  The function should leave the
;; result, if any, in this buffer.  Most treatment functions will just
;; generate files...
;;
;; The second parameter is the state of the list of articles, and can
;; have four values: `first', `middle', `last' and `first-and-last'.
;;
;; The function should return a list.  The list may contain the
;; following symbols:
;; `error' if an error occurred
;; `begin' if the beginning of an encoded file has been received
;;   If the list returned contains a `begin', the first element of
;;   the list *must* be a string with the file name of the decoded
;;   file.
;; `end' if the end of an encoded file has been received
;; `middle' if the article was a body part of an encoded file
;; `wrong-type' if the article was not a part of an encoded file
;; `ok', which can be used everything is ok

(defvar gnus-uu-has-been-grabbed nil)

(defun gnus-uu-unmark-list-of-grabbed (&optional dont-unmark-last-article)
  (let (art)
    (if (not (and gnus-uu-has-been-grabbed
		  gnus-uu-unmark-articles-not-decoded))
	()
      (when dont-unmark-last-article
	(setq art (car gnus-uu-has-been-grabbed))
	(setq gnus-uu-has-been-grabbed (cdr gnus-uu-has-been-grabbed)))
      (while gnus-uu-has-been-grabbed
	(gnus-summary-tick-article (car gnus-uu-has-been-grabbed) t)
	(setq gnus-uu-has-been-grabbed (cdr gnus-uu-has-been-grabbed)))
      (when dont-unmark-last-article
	(setq gnus-uu-has-been-grabbed (list art))))))

;; This function takes a list of articles and a function to apply to
;; each article grabbed.
;;
;; This function returns a list of files decoded if the grabbing and
;; the process-function has been successful and nil otherwise.
(defun gnus-uu-grab-articles (articles process-function
				       &optional sloppy limit no-errors)
  (require 'gnus-async)
  (let ((state 'first)
	(gnus-asynchronous nil)
	(gnus-inhibit-treatment t)
	has-been-begin article result-file result-files process-state
	gnus-summary-display-article-function
	gnus-article-prepare-hook gnus-display-mime-function
	article-series files)

    (while (and articles
		(not (memq 'error process-state))
		(or sloppy
		    (not (memq 'end process-state))))

      (setq article (pop articles))
      (when (vectorp (gnus-summary-article-header article))
	(push article article-series)

	(unless articles
	  (if (eq state 'first)
	      (setq state 'first-and-last)
	    (setq state 'last)))

	(let ((part (gnus-uu-part-number article)))
	  (gnus-message 6 "Getting article %d%s..."
			article (if (string= part "") "" (concat ", " part))))
	(gnus-summary-display-article article)

	;; Push the article to the processing function.
	(with-current-buffer gnus-original-article-buffer
	  (let ((buffer-read-only nil))
	    (with-current-buffer gnus-summary-buffer
	      (setq process-state
		    (funcall process-function
			     gnus-original-article-buffer state)))))

	(gnus-summary-remove-process-mark article)

	;; If this is the beginning of a decoded file, we push it
	;; on to a list.
	(when (or (memq 'begin process-state)
		  (and (or (eq state 'first)
			   (eq state 'first-and-last))
		       (memq 'ok process-state)))
	  (when has-been-begin
	    ;; If there is a `result-file' here, that means that the
	    ;; file was unsuccessfully decoded, so we delete it.
	    (when (and result-file
		       (file-exists-p result-file)
		       (not gnus-uu-be-dangerous)
		       (or (eq gnus-uu-be-dangerous t)
			   (gnus-y-or-n-p
			    (format "Delete unsuccessfully decoded file %s? "
				    result-file))))
	      (delete-file result-file)))
	  (when (memq 'begin process-state)
	    (setq result-file (car process-state)))
	  (setq has-been-begin t))

	;; Check whether we have decoded one complete file.
	(when (memq 'end process-state)
	  (setq article-series nil)
	  (setq has-been-begin nil)
	  (if (stringp result-file)
	      (setq files (list result-file))
	    (setq files result-file))
	  (setq result-file (car files))
	  (while files
	    (push (list (cons 'name (pop files))
			(cons 'article article))
		  result-files))
	  ;; Allow user-defined functions to be run on this file.
	  (when gnus-uu-grabbed-file-functions
	    (let ((funcs gnus-uu-grabbed-file-functions))
	      (unless (listp funcs)
		(setq funcs (list funcs)))
	      (while funcs
		(funcall (pop funcs) result-file))))
	  (setq result-file nil)
	  ;; Check whether we have decoded enough articles.
	  (and limit (= (length result-files) limit)
	       (setq articles nil)))

	;; If this is the last article to be decoded, and
	;; we still haven't reached the end, then we delete
	;; the partially decoded file.
	(and (or (eq state 'last) (eq state 'first-and-last))
	     (not (memq 'end process-state))
	     result-file
	     (file-exists-p result-file)
	     (not gnus-uu-be-dangerous)
	     (or (eq gnus-uu-be-dangerous t)
		 (gnus-y-or-n-p
		  (format "Delete incomplete file %s? " result-file)))
	     (delete-file result-file))

	;; If this was a file of the wrong sort, then
	(when (and (or (memq 'wrong-type process-state)
		       (memq 'error process-state))
		   gnus-uu-unmark-articles-not-decoded)
	  (gnus-summary-tick-article article t))

	;; Set the new series state.
	(if (and (not has-been-begin)
		 (not sloppy)
		 (or (memq 'end process-state)
		     (memq 'middle process-state)))
	    (progn
	      (setq process-state (list 'error))
	      (gnus-message 2 "No begin part at the beginning")
	      (sleep-for 2))
	  (setq state 'middle))))

      ;; When there are no result-files, then something must be wrong.
    (if result-files
	(message "")
      (cond
       ((not has-been-begin)
	(gnus-message 2 "Wrong type file"))
       ((memq 'error process-state)
	(gnus-message 2 "An error occurred during decoding"))
       ((not (or (memq 'ok process-state)
		 (memq 'end process-state)))
	(gnus-message 2 "End of articles reached before end of file")))
      ;; Make unsuccessfully decoded articles unread.
      (when gnus-uu-unmark-articles-not-decoded
	(while article-series
	  (gnus-summary-tick-article (pop article-series) t))))

    ;; The original article buffer is hosed, shoot it down.
    (gnus-kill-buffer gnus-original-article-buffer)
    (setq gnus-current-article nil)
    result-files))

(defun gnus-uu-grab-view (file)
  "View FILE using the gnus-uu methods."
  (let ((action (gnus-uu-get-action file)))
    (gnus-execute-command
     (if (string-match "%" action)
	 (format action file)
       (concat action " " file))
     (eq gnus-view-pseudos 'not-confirm))))

(defun gnus-uu-grab-move (file)
  "Move FILE to somewhere."
  (when gnus-uu-default-dir
    (let ((to-file (concat (file-name-as-directory gnus-uu-default-dir)
			   (file-name-nondirectory file))))
      (rename-file file to-file)
      (unless (file-exists-p file)
	(make-symbolic-link to-file file)))))

(defun gnus-uu-part-number (article)
  (let* ((header (gnus-summary-article-header article))
	 (subject (and header (mail-header-subject header)))
	 (part nil))
    (if subject
	(while (string-match "[0-9]+/[0-9]+\\|[0-9]+[ \t]+of[ \t]+[0-9]+"
			     subject)
	  (setq part (match-string 0 subject))
	  (setq subject (substring subject (match-end 0)))))
    (or part
	(while (string-match "[0-9]+[^0-9]+[0-9]+" subject)
	  (setq part (match-string 0 subject))
	  (setq subject (substring subject (match-end 0)))))
    (or part "")))

(defun gnus-uu-uudecode-sentinel (process event)
  (delete-process (get-process process)))

(defun gnus-uu-uustrip-article (process-buffer in-state)
  ;; Uudecodes a file asynchronously.
  (with-current-buffer process-buffer
    (let ((state (list 'wrong-type))
	  process-connection-type case-fold-search buffer-read-only
	  files start-char)
      (goto-char (point-min))

      ;; Deal with ^M at the end of the lines.
      (when gnus-uu-kill-carriage-return
	(save-excursion
	  (while (search-forward "\r" nil t)
	    (delete-char -1))))

      (while (or (re-search-forward gnus-uu-begin-string nil t)
		 (re-search-forward gnus-uu-body-line nil t))
	(setq state (list 'ok))
	;; Ok, we are at the first uucoded line.
	(beginning-of-line)
	(setq start-char (point))

	(if (not (looking-at gnus-uu-begin-string))
	    (setq state (list 'middle))
	  ;; This is the beginning of a uuencoded article.
	  ;; We replace certain characters that could make things messy.
	  (setq gnus-uu-file-name
		(gnus-map-function
		 mm-file-name-rewrite-functions
		 (file-name-nondirectory (match-string 1))))
	  (replace-match (concat "begin 644 " gnus-uu-file-name) t t)

	  ;; Remove any non gnus-uu-body-line right after start.
	  (forward-line 1)
	  (while (and (not (eobp))
		      (not (looking-at gnus-uu-body-line)))
	    (gnus-delete-line))

	  ;; If a process is running, we kill it.
	  (when (and gnus-uu-uudecode-process
		     (memq (process-status gnus-uu-uudecode-process)
			   '(run stop)))
	    (delete-process gnus-uu-uudecode-process)
	    (gnus-uu-unmark-list-of-grabbed t))

	  ;; Start a new uudecoding process.
	  (let ((cdir default-directory))
	    (unwind-protect
		(progn
		  (cd gnus-uu-work-dir)
		  (setq gnus-uu-uudecode-process
			(start-process
			 "*uudecode*"
			 (gnus-get-buffer-create gnus-uu-output-buffer-name)
			 shell-file-name shell-command-switch
			 (format "cd %s %s uudecode" gnus-uu-work-dir
				 gnus-shell-command-separator))))
	      (cd cdir)))
	  (set-process-sentinel
	   gnus-uu-uudecode-process 'gnus-uu-uudecode-sentinel)
	  (setq state (list 'begin))
	  (push (concat gnus-uu-work-dir gnus-uu-file-name) files))

	;; We look for the end of the thing to be decoded.
	(if (re-search-forward gnus-uu-end-string nil t)
	    (push 'end state)
	  (goto-char (point-max))
	  (re-search-backward gnus-uu-body-line nil t))

	(forward-line 1)

	(when gnus-uu-uudecode-process
	  (when (memq (process-status gnus-uu-uudecode-process) '(run stop))
	    ;; Try to correct mishandled uucode.
	    (when gnus-uu-correct-stripped-uucode
	      (gnus-uu-check-correct-stripped-uucode start-char (point)))
	    (gnus-run-hooks 'gnus-uu-pre-uudecode-hook)

	    ;; Send the text to the process.
	    (condition-case nil
		(process-send-region
		 gnus-uu-uudecode-process start-char (point))
	      (error
	       (progn
		 (delete-process gnus-uu-uudecode-process)
		 (gnus-message 2 "gnus-uu: Couldn't uudecode")
		 (setq state (list 'wrong-type)))))

	    (if (memq 'end state)
		(progn
		  ;; Send an EOF, just in case.
		  (ignore-errors
		    (process-send-eof gnus-uu-uudecode-process))
		  (while (memq (process-status gnus-uu-uudecode-process)
			       '(open run))
		    (accept-process-output gnus-uu-uudecode-process 1)))
	      (when (or (not gnus-uu-uudecode-process)
			(not (memq (process-status gnus-uu-uudecode-process)
				   '(run stop))))
		(setq state (list 'wrong-type)))))))

      (if (memq 'begin state)
	  (cons (if (= (length files) 1) (car files) files) state)
	state))))

(defvar gnus-uu-unshar-warning
  "*** WARNING ***

Shell archives are an archaic method of bundling files for distribution
across computer networks.  During the unpacking process, arbitrary commands
are executed on your system, and all kinds of nasty things can happen.
Please examine the archive very carefully before you instruct Emacs to
unpack it.  You can browse the archive buffer using \\[scroll-other-window].

If you are unsure what to do, please answer \"no\"."
  "Text of warning message displayed by `gnus-uu-unshar-article'.
Make sure that this text consists only of few text lines.  Otherwise,
Gnus might fail to display all of it.")


;; This function is used by `gnus-uu-grab-articles' to treat
;; a shared article.
(defun gnus-uu-unshar-article (process-buffer in-state)
  (let ((state (list 'ok))
	start-char)
    (with-current-buffer process-buffer
      (goto-char (point-min))
      (if (not (re-search-forward gnus-uu-shar-begin-string nil t))
	  (setq state (list 'wrong-type))
	(save-window-excursion
	  (save-excursion
	    (switch-to-buffer (current-buffer))
	    (delete-other-windows)
	    (let ((buffer (get-buffer-create (generate-new-buffer-name
					      "*Warning*"))))
	      (unless
		  (unwind-protect
		      (with-current-buffer buffer
			(insert (substitute-command-keys
				 gnus-uu-unshar-warning))
			(goto-char (point-min))
			(display-buffer buffer)
			(yes-or-no-p "This is a shell archive, unshar it? "))
		    (kill-buffer buffer))
		(setq state (list 'error))))))
	(unless (memq 'error state)
	  (beginning-of-line)
	  (setq start-char (point))
	  (call-process-region
	   start-char (point-max) shell-file-name nil
	   (gnus-get-buffer-create gnus-uu-output-buffer-name) nil
	   shell-command-switch
	   (concat "cd " gnus-uu-work-dir " "
		   gnus-shell-command-separator  " sh")))))
    state))

;; Returns the name of what the shar file is going to unpack.
(defun gnus-uu-find-name-in-shar ()
  (let ((oldpoint (point))
	res)
    (goto-char (point-min))
    (when (re-search-forward gnus-uu-shar-name-marker nil t)
      (setq res (buffer-substring (match-beginning 1) (match-end 1))))
    (goto-char oldpoint)
    res))

;; `gnus-uu-choose-action' chooses what action to perform given the name
;; and `gnus-uu-file-action-list'.  Returns either nil if no action is
;; found, or the name of the command to run if such a rule is found.
(defun gnus-uu-choose-action (file-name file-action-list &optional no-ignore)
  (let ((action-list (copy-sequence file-action-list))
	(case-fold-search t)
	rule action)
    (and
     (unless no-ignore
       (and (not
	     (and gnus-uu-ignore-files-by-name
		  (string-match gnus-uu-ignore-files-by-name file-name)))
	    (not
	     (and gnus-uu-ignore-files-by-type
		  (string-match gnus-uu-ignore-files-by-type
				(or (gnus-uu-choose-action
				     file-name gnus-uu-ext-to-mime-list t)
				    ""))))))
     (while (not (or (eq action-list ()) action))
       (setq rule (car action-list))
       (setq action-list (cdr action-list))
       (when (string-match (car rule) file-name)
	 (setq action (cadr rule)))))
    action))

(defun gnus-uu-treat-archive (file-path)
  ;; Unpacks an archive.  Returns t if unpacking is successful.
  (let ((did-unpack t)
	action command dir)
    (setq action (gnus-uu-choose-action
		  file-path (append gnus-uu-user-archive-rules
				    (if gnus-uu-ignore-default-archive-rules
					nil
				      gnus-uu-default-archive-rules))))

    (when (not action)
      (error "No unpackers for the file %s" file-path))

    (string-match "/[^/]*$" file-path)
    (setq dir (substring file-path 0 (match-beginning 0)))

    (when (member action gnus-uu-destructive-archivers)
      (copy-file file-path (concat file-path "~") t))

    (setq command (format "cd %s ; %s" dir (gnus-uu-command action file-path)))

    (with-current-buffer (gnus-get-buffer-create gnus-uu-output-buffer-name)
      (erase-buffer))

    (gnus-message 5 "Unpacking: %s..." (gnus-uu-command action file-path))

    (if (eq 0 (call-process shell-file-name nil
			   (gnus-get-buffer-create gnus-uu-output-buffer-name)
			   nil shell-command-switch command))
	(message "")
      (gnus-message 2 "Error during unpacking of archive")
      (setq did-unpack nil))

    (when (member action gnus-uu-destructive-archivers)
      (rename-file (concat file-path "~") file-path t))

    did-unpack))

(defun gnus-uu-dir-files (dir)
  (let ((dirs (directory-files dir t "[^/][^\\.][^\\.]?$"))
	files file)
    (while dirs
      (if (file-directory-p (setq file (car dirs)))
	  (setq files (append files (gnus-uu-dir-files file)))
	(push file files))
      (setq dirs (cdr dirs)))
    files))

(defun gnus-uu-unpack-files (files &optional ignore)
  ;; Go through FILES and look for files to unpack.
  (let* ((totfiles (gnus-uu-ls-r gnus-uu-work-dir))
	 (ofiles files)
	 file did-unpack)
    (while files
      (setq file (cdr (assq 'name (car files))))
      (when (and (not (member file ignore))
		 (equal (gnus-uu-get-action (file-name-nondirectory file))
			"gnus-uu-archive"))
	(push file did-unpack)
	(unless (gnus-uu-treat-archive file)
	  (gnus-message 2 "Error during unpacking of %s" file))
	(let* ((newfiles (gnus-uu-ls-r gnus-uu-work-dir))
	       (nfiles newfiles))
	  (while nfiles
	    (unless (member (car nfiles) totfiles)
	      (push (list (cons 'name (car nfiles))
			  (cons 'original file))
		    ofiles))
	    (setq nfiles (cdr nfiles)))
	  (setq totfiles newfiles)))
      (setq files (cdr files)))
    (if did-unpack
	(gnus-uu-unpack-files ofiles (append did-unpack ignore))
      ofiles)))

(defun gnus-uu-ls-r (dir)
  (let* ((files (gnus-uu-directory-files dir t))
	 (ofiles files))
    (while files
      (when (file-directory-p (car files))
	(setq ofiles (delete (car files) ofiles))
	(setq ofiles (append ofiles (gnus-uu-ls-r (car files)))))
      (setq files (cdr files)))
    ofiles))

;; Various stuff

(defun gnus-uu-directory-files (dir &optional full)
  (let (files out file)
    (setq files (directory-files dir full))
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (unless (member (file-name-nondirectory file) '("." ".."))
	(push file out)))
    (setq out (nreverse out))
    out))

(defun gnus-uu-check-correct-stripped-uucode (start end)
  (save-excursion
    (let (found beg length)
      (unless gnus-uu-correct-stripped-uucode
	(goto-char start)

	(if (re-search-forward " \\|`" end t)
	    (progn
	      (goto-char start)
	      (while (not (eobp))
		(progn
		  (when (looking-at "\n")
		    (replace-match ""))
		  (forward-line 1))))

	  (while (not (eobp))
	    (unless (looking-at (concat gnus-uu-begin-string "\\|"
					gnus-uu-end-string))
	      (when (not found)
		(setq length (- (point-at-eol) (point-at-bol))))
	      (setq found t)
	      (beginning-of-line)
	      (setq beg (point))
	      (end-of-line)
	      (unless (= length (- (point) beg))
		(insert (make-string (- length (- (point) beg)) ? ))))
	    (forward-line 1)))))))

(defvar gnus-uu-tmp-alist nil)

(defun gnus-uu-initialize (&optional scan)
  (let (entry)
    (if (and (not scan)
	     (when (setq entry (assoc gnus-newsgroup-name gnus-uu-tmp-alist))
	       (if (file-exists-p (cdr entry))
		   (setq gnus-uu-work-dir (cdr entry))
		 (setq gnus-uu-tmp-alist (delq entry gnus-uu-tmp-alist))
		 nil)))
	t
      (setq gnus-uu-tmp-dir (file-name-as-directory
			     (expand-file-name gnus-uu-tmp-dir)))
      (if (not (file-directory-p gnus-uu-tmp-dir))
	  (error "Temp directory %s doesn't exist" gnus-uu-tmp-dir)
	(when (not (file-writable-p gnus-uu-tmp-dir))
	  (error "Temp directory %s can't be written to"
		 gnus-uu-tmp-dir)))

      (setq gnus-uu-work-dir
	    (mm-make-temp-file (concat gnus-uu-tmp-dir "gnus") 'dir))
      (gnus-set-file-modes gnus-uu-work-dir 448)
      (setq gnus-uu-work-dir (file-name-as-directory gnus-uu-work-dir))
      (push (cons gnus-newsgroup-name gnus-uu-work-dir)
	    gnus-uu-tmp-alist))))


;; Kills the temporary uu buffers, kills any processes, etc.
(defun gnus-uu-clean-up ()
  (let (buf)
    (and gnus-uu-uudecode-process
	 (memq (process-status (or gnus-uu-uudecode-process "nevair"))
	       '(stop run))
	 (delete-process gnus-uu-uudecode-process))
    (when (setq buf (get-buffer gnus-uu-output-buffer-name))
      (kill-buffer buf))))

;; Inputs an action and a filename and returns a full command, making sure
;; that the filename will be treated as a single argument when the shell
;; executes the command.
(defun gnus-uu-command (action file)
  (let ((quoted-file (shell-quote-argument file)))
    (if (string-match "%s" action)
	(format action quoted-file)
      (concat action " " quoted-file))))

(defun gnus-uu-delete-work-dir (&optional dir)
  "Delete recursively all files and directories under `gnus-uu-work-dir'."
  (if dir
      (gnus-message 7 "Deleting directory %s..." dir)
    (setq dir gnus-uu-work-dir))
  (when (and dir
	     (file-exists-p dir))
    (let ((files (directory-files dir t nil t))
	  file)
      (while (setq file (pop files))
	(unless (member (file-name-nondirectory file) '("." ".."))
	  (if (file-directory-p file)
	      (gnus-uu-delete-work-dir file)
	    (gnus-message 9 "Deleting file %s..." file)
            (condition-case err
                (delete-file file)
              (error (gnus-message 3 "Deleting file %s failed... %s" file err))))))
      (condition-case err
          (delete-directory dir)
        (error (gnus-message 3 "Deleting directory %s failed... %s" file err))))
    (gnus-message 7 "")))

;; Initializing

(add-hook 'gnus-exit-group-hook 'gnus-uu-clean-up)
(add-hook 'gnus-exit-group-hook	'gnus-uu-delete-work-dir)



;;;
;;; uuencoded posting
;;;

;; Any function that is to be used as and encoding method will take two
;; parameters: PATH-NAME and FILE-NAME.  (E.g. "/home/gaga/spiral.jpg"
;; and "spiral.jpg", respectively.) The function should return nil if
;; the encoding wasn't successful.
(defcustom gnus-uu-post-encode-method 'gnus-uu-post-encode-uuencode
  "Function used for encoding binary files.
There are three functions supplied with gnus-uu for encoding files:
`gnus-uu-post-encode-uuencode', which does straight uuencoding;
`gnus-uu-post-encode-mime', which encodes with base64 and adds MIME
headers; and `gnus-uu-post-encode-mime-uuencode', which encodes with
uuencode and adds MIME headers."
  :group 'gnus-extract-post
  :type '(radio (function-item gnus-uu-post-encode-uuencode)
		(function-item gnus-uu-post-encode-mime)
		(function-item gnus-uu-post-encode-mime-uuencode)
		(function :tag "Other")))

(defcustom gnus-uu-post-include-before-composing nil
  "Non-nil means that gnus-uu will ask for a file to encode before you compose the article.
If this variable is t, you can either include an encoded file with
\\[gnus-uu-post-insert-binary-in-article] or have one included for you when you post the article."
  :group 'gnus-extract-post
  :type 'boolean)

(defcustom gnus-uu-post-length 990
  "Maximum length of an article.
The encoded file will be split into how many articles it takes to
post the entire file."
  :group 'gnus-extract-post
  :type 'integer)

(defcustom gnus-uu-post-threaded nil
  "Non-nil means that gnus-uu will post the encoded file in a thread.
This may not be smart, as no other decoder I have seen are able to
follow threads when collecting uuencoded articles.  (Well, I have seen
one package that does that - gnus-uu, but somehow, I don't think that
counts...)  The default is nil."
  :group 'gnus-extract-post
  :type 'boolean)

(defcustom gnus-uu-post-separate-description t
  "Non-nil means that the description will be posted in a separate article.
The first article will typically be numbered (0/x).  If this variable
is nil, the description the user enters will be included at the
beginning of the first article, which will be numbered (1/x).  Default
is t."
  :group 'gnus-extract-post
  :type 'boolean)

(defvar gnus-uu-post-binary-separator "--binary follows this line--")
(defvar gnus-uu-post-message-id nil)
(defvar gnus-uu-post-inserted-file-name nil)
(defvar gnus-uu-winconf-post-news nil)

(defun gnus-uu-post-news ()
  "Compose an article and post an encoded file."
  (interactive)
  (setq gnus-uu-post-inserted-file-name nil)
  (setq gnus-uu-winconf-post-news (current-window-configuration))

  (gnus-summary-post-news)

  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map))
  ;;(local-set-key "\C-c\C-c" 'gnus-summary-edit-article-done)
  (local-set-key "\C-c\C-c" 'gnus-uu-post-news-inews)
  (local-set-key "\C-c\C-s" 'gnus-uu-post-news-inews)
  (local-set-key "\C-c\C-i" 'gnus-uu-post-insert-binary-in-article)

  (when gnus-uu-post-include-before-composing
    (save-excursion (setq gnus-uu-post-inserted-file-name
			  (gnus-uu-post-insert-binary)))))

(defun gnus-uu-post-insert-binary-in-article ()
  "Inserts an encoded file in the buffer.
The user will be asked for a file name."
  (interactive)
  (save-excursion
    (setq gnus-uu-post-inserted-file-name (gnus-uu-post-insert-binary))))

;; Encodes with uuencode and substitutes all spaces with backticks.
(defun gnus-uu-post-encode-uuencode (path file-name)
  (when (gnus-uu-post-encode-file "uuencode" path file-name)
    (goto-char (point-min))
    (forward-line 1)
    (while (search-forward " " nil t)
      (replace-match "`"))
    t))

;; Encodes with uuencode and adds MIME headers.
(defun gnus-uu-post-encode-mime-uuencode (path file-name)
  (when (gnus-uu-post-encode-uuencode path file-name)
    (gnus-uu-post-make-mime file-name "x-uue")
    t))

;; Encodes with base64 and adds MIME headers
(defun gnus-uu-post-encode-mime (path file-name)
  (when (eq 0 (call-process shell-file-name nil t nil shell-command-switch
			    (format "%s %s -o %s" "mmencode" path file-name)))
    (gnus-uu-post-make-mime file-name "base64")
    t))

;; Adds MIME headers.
(defun gnus-uu-post-make-mime (file-name encoding)
  (goto-char (point-min))
  (insert (format "Content-Type: %s; name=\"%s\"\n"
		  (gnus-uu-choose-action file-name gnus-uu-ext-to-mime-list)
		  file-name))
  (insert (format "Content-Transfer-Encoding: %s\n\n" encoding))
  (save-restriction
    (set-buffer gnus-message-buffer)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line -1)
    (narrow-to-region (point-min) (point))
    (unless (mail-fetch-field "mime-version")
      (widen)
      (insert "MIME-Version: 1.0\n"))
    (widen)))

;; Encodes a file PATH with COMMAND, leaving the result in the
;; current buffer.
(defun gnus-uu-post-encode-file (command path file-name)
  (eq 0 (call-process shell-file-name nil t nil shell-command-switch
		      (format "%s %s %s" command path file-name))))

(defun gnus-uu-post-news-inews ()
  "Posts the composed news article and encoded file.
If no file has been included, the user will be asked for a file."
  (interactive)

  (let (file-name)

    (if gnus-uu-post-inserted-file-name
	(setq file-name gnus-uu-post-inserted-file-name)
      (setq file-name (gnus-uu-post-insert-binary)))

    (gnus-uu-post-encoded file-name gnus-uu-post-threaded))
  (setq gnus-uu-post-inserted-file-name nil)
  (when gnus-uu-winconf-post-news
    (set-window-configuration gnus-uu-winconf-post-news)))

;; Asks for a file to encode, encodes it and inserts the result in
;; the current buffer.  Returns the file name the user gave.
(defun gnus-uu-post-insert-binary ()
  (let ((uuencode-buffer-name "*uuencode buffer*")
	file-path uubuf file-name)

    (setq file-path (read-file-name
		     "What file do you want to encode? "))
    (when (not (file-exists-p file-path))
      (error "%s: No such file" file-path))

    (goto-char (point-max))
    (insert (format "\n%s\n" gnus-uu-post-binary-separator))

    ;; #### Unix-specific?
    (when (string-match "^~/" file-path)
      (setq file-path (concat "$HOME" (substring file-path 1))))
    ;; #### Unix-specific?
    (if (string-match "/[^/]*$" file-path)
	(setq file-name (substring file-path (1+ (match-beginning 0))))
      (setq file-name file-path))

    (unwind-protect
	(if (with-current-buffer
		(setq uubuf (gnus-get-buffer-create uuencode-buffer-name))
	      (erase-buffer)
	      (funcall gnus-uu-post-encode-method file-path file-name))
	    (insert-buffer-substring uubuf)
	  (error "Encoding unsuccessful"))
      (kill-buffer uubuf))
    file-name))

;; Posts the article and all of the encoded file.
(defun gnus-uu-post-encoded (file-name &optional threaded)
  (let ((send-buffer-name "*uuencode send buffer*")
	(encoded-buffer-name "*encoded buffer*")
	(top-string "[ cut here %s (%s %d/%d) %s gnus-uu ]")
	(separator (concat mail-header-separator "\n\n"))
	uubuf length parts header i end beg
	beg-line minlen post-buf whole-len beg-binary end-binary)

    (setq post-buf (current-buffer))

    (goto-char (point-min))
    (when (not (re-search-forward
		(if gnus-uu-post-separate-description
		    (concat "^" (regexp-quote gnus-uu-post-binary-separator)
			    "$")
		  (concat "^" (regexp-quote mail-header-separator) "$"))
		nil t))
      (error "Internal error: No binary/header separator"))
    (beginning-of-line)
    (forward-line 1)
    (setq beg-binary (point))
    (setq end-binary (point-max))

    (with-current-buffer
	(setq uubuf (gnus-get-buffer-create encoded-buffer-name))
      (erase-buffer)
      (insert-buffer-substring post-buf beg-binary end-binary)
      (goto-char (point-min))
      (setq length (count-lines (point-min) (point-max)))
      (setq parts (/ length gnus-uu-post-length))
      (unless (< (% length gnus-uu-post-length) 4)
	(incf parts)))

    (when gnus-uu-post-separate-description
      (forward-line -1))
    (delete-region (point) (point-max))

    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "$") nil t)
    (setq header (buffer-substring (point-min) (point-at-bol)))

    (goto-char (point-min))
    (when gnus-uu-post-separate-description
      (when (re-search-forward "^Subject: " nil t)
	(end-of-line)
	(insert (format " (0/%d)" parts)))
      (save-excursion
	(message-send))
      (setq gnus-uu-post-message-id (message-fetch-field "message-id")))

    (save-excursion
      (setq i 1)
      (setq beg 1)
      (while (not (> i parts))
	(set-buffer (gnus-get-buffer-create send-buffer-name))
	(erase-buffer)
	(insert header)
	(when (and threaded gnus-uu-post-message-id)
	  (insert "References: " gnus-uu-post-message-id "\n"))
	(insert separator)
	(setq whole-len
	      (- 62 (length (format top-string "" file-name i parts ""))))
	(when (> 1 (setq minlen (/ whole-len 2)))
	  (setq minlen 1))
	(setq
	 beg-line
	 (format top-string
		 (make-string minlen ?-)
		 file-name i parts
		 (make-string
		  (if (= 0 (% whole-len 2)) (1- minlen) minlen) ?-)))

	(goto-char (point-min))
	(when (re-search-forward "^Subject: " nil t)
	  (end-of-line)
	  (insert (format " (%d/%d)" i parts)))

	(goto-char (point-max))
	(with-current-buffer uubuf
	  (goto-char beg)
	  (if (= i parts)
	      (goto-char (point-max))
	    (forward-line gnus-uu-post-length))
	  (when (and (= (1+ i) parts) (< (count-lines (point) (point-max)) 4))
	    (forward-line -4))
	  (setq end (point)))
	(insert-buffer-substring uubuf beg end)
	(insert beg-line "\n")
	(setq beg end)
	(incf i)
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "$") nil t)
	(beginning-of-line)
	(forward-line 2)
	(when (re-search-forward
	       (concat "^" (regexp-quote gnus-uu-post-binary-separator) "$")
	       nil t)
	  (replace-match "")
	  (forward-line 1))
	(insert beg-line)
	(insert "\n")
	(let (message-sent-message-via)
	  (save-excursion
	    (message-send))
	  (setq gnus-uu-post-message-id
		(concat (message-fetch-field "references") " "
			(message-fetch-field "message-id"))))))

    (gnus-kill-buffer send-buffer-name)
    (gnus-kill-buffer encoded-buffer-name)

    (when (not gnus-uu-post-separate-description)
      (set-buffer-modified-p nil)
      (bury-buffer))))

(provide 'gnus-uu)

;;; gnus-uu.el ends here
