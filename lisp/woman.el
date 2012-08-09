;;; woman.el --- browse UN*X manual pages `wo (without) man'

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: Francis J. Wright <F.J.Wright@qmul.ac.uk>
;; Maintainer: FSF
;; Keywords: help, unix
;; Adapted-By: Eli Zaretskii <eliz@gnu.org>
;; Version: 0.551
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/WoMan/

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

;; WoMan implements a subset of the formatting performed by the Emacs
;; `man' (or `manual-entry') command to format a UN*X manual `page'
;; for display, but without calling any external programs.  It is
;; intended to emulate the whole of the -man macro package, plus those
;; ?roff requests that are most commonly used in man pages.  However,
;; the emulation is modified to include the reformatting done by the
;; Emacs `man' command.  No hyphenation is performed.

;; Advantages

;;   Much more direct, does not require any external programs.
;;   Supports completion on man page names.

;; Disadvantages

;;   Not a complete emulation.  Currently no support for eqn or tbl.
;;   Slightly slower for large man pages (but usually faster for
;;   small- and medium-size pages).

;; This browser works quite well on simple well-written man files.  It
;; works less well on idiosyncratic files that `break the rules' or
;; use the more obscure ?roff requests directly.  Current test results
;; are available in the file woman.status.

;; WoMan supports the use of compressed man files via
;; `auto-compression-mode' by turning it on if necessary.  But you may
;; need to adjust the user option `woman-file-compression-regexp'.

;; Read on for (currently) the only documentation for WoMan!

;; See also the documentation for the WoMan interactive commands and
;; user option variables, all of which begin with the prefix `woman-'.
;; This can be done most easily by loading WoMan and then running the
;; command `woman-mini-help', or selecting the WoMan menu option `Mini
;; Help' when WoMan is running.

;; WoMan is still under development!  Please let me know what doesn't
;; work -- I am adding and improving functionality as testing shows
;; that it is necessary.  See below for guidance on reporting bugs.

;; Recommended use
;; ===============

;; Put this in your .emacs:
;;   (autoload 'woman "woman"
;;             "Decode and browse a UN*X man page." t)
;;   (autoload 'woman-find-file "woman"
;;             "Find, decode and browse a specific UN*X man-page file." t)

;; Then either (1 -- *RECOMMENDED*): If the `MANPATH' environment
;; variable is set then WoMan will use it; otherwise you may need to
;; reset the Lisp variable `woman-manpath', and you may also want to
;; set the Lisp variable `woman-path'.  Please see the online
;; documentation for these variables.  Now you can execute the
;; extended command `woman', enter or select a manual entry topic,
;; using completion, and if necessary select a filename, using
;; completion.  By default, WoMan suggests the word nearest to the
;; cursor in the current buffer as the topic.

;; Or (2): Execute the extended command `woman-find-file' and enter a
;; filename, using completion.  This mode of execution may be useful
;; for temporary files outside the standard UN*X manual directory
;; structure.

;; Or (3): Put the next two sexpr's in your .emacs:
;; (autoload 'woman-dired-find-file "woman"
;;   "In dired, run the WoMan man-page browser on this file." t)
;; (add-hook 'dired-mode-hook
;;          (lambda ()
;;            (define-key dired-mode-map "W" 'woman-dired-find-file)))
;; and open the directory containing the man page file using dired,
;; put the cursor on the file, and press `W'.

;; In each case, the result should (!) be a buffer in Man mode showing
;; a formatted manual entry.  When called from WoMan, Man mode should
;; work as advertised, but modified where necessary in the context of
;; WoMan.  (However, `Man' will still invoke the standard Emacs
;; manual-browsing facility rather than `WoMan' -- this is
;; intentional!)

;; (By default, WoMan will automatically define the dired keys "W" and
;; "w" when it loads, but only if they are not already defined.  This
;; behavior is controlled by the user option `woman-dired-keys'.
;; Note that the `dired-x' (dired extra) package binds
;; `dired-copy-filename-as-kill' to the key "w" (as pointed out by Jim
;; Davidson), although "W" appears to be really unused.  The `dired-x'
;; package will over-write the WoMan binding to "w", whereas (by
;; default) WoMan will not overwrite the `dired-x' binding.)

;; The following is based on suggestions by Guy Gascoigne-Piggford and
;; Juanma Barranquero.  If you really want to square the man-woman
;; circle then you might care to define the following bash function in
;; .bashrc:

;;   man() { gnudoit -q '(raise-frame (selected-frame)) (woman' \"$1\" ')' ; }

;; If you use Microsoft COMMAND.COM then you can create a file called
;; man.bat somewhere in your path containing the two lines:

;;   @echo off
;;   gnudoit -q (raise-frame (selected-frame)) (woman \"%1\")

;; and then (e.g. from a command prompt or the Run... option in the
;; Start menu) just execute

;;   man man_page_name


;; Using the word at point as the default topic
;; ============================================

;; The `woman' command uses the word nearest to point in the current
;; buffer as the default topic to look up if it matches the name of a
;; manual page installed on the system.  The default topic can also be
;; used without confirmation by setting the user-option
;; `woman-use-topic-at-point' to t; thanks to Benjamin Riefenstahl for
;; suggesting this functionality.

;; The variable `woman-use-topic-at-point' can be rebound locally,
;; which may be useful to provide special private key bindings, e.g.

;;  (global-set-key "\C-cw"
;;  		  (lambda ()
;;  		    (interactive)
;;  		    (let ((woman-use-topic-at-point t))
;;  		      (woman)))))


;; Customization, Hooks and Imenu
;; ==============================

;; WoMan supports the GNU Emacs 20+ customization facility, and puts
;; a customization group called `WoMan' in the `Help' group under the
;; top-level `Emacs' group.  In order to be able to customize WoMan
;; without first loading it, add the following sexp to your .emacs:

;;  (defgroup woman nil
;;     "Browse UNIX manual pages `wo (without) man'."
;;     :tag "WoMan" :group 'help :load "woman")


;; WoMan currently runs two hooks: `woman-pre-format-hook' immediately
;; before formatting a buffer and `woman-post-format-hook' immediately
;; after formatting a buffer.  These hooks can be used for special
;; customizations that require code to be executed, etc., although
;; most customization should be possible by setting WoMan user option
;; variables, e.g. in `.emacs' and should NOT require the use of the
;; hooks.  `woman-pre-format-hook' might be appropriate for face
;; customization, whereas `woman-post-format-hook' might be
;; appropriate for installing a dynamic menu using `imenu' (although
;; it is better to use the built-in WoMan imenu support).

;; The WoMan menu provides an option to make a contents menu for the
;; current man page (using imenu).  Alternatively, if you set the
;; variable `woman-imenu' to `t' then WoMan will do it automatically
;; for every man page.  The menu title is the value of the variable
;; `woman-imenu-title', which is "CONTENTS" by default.  By default,
;; the menu shows manual sections and subsections, but you can change
;; this by changing the value of `woman-imenu-generic-expression'.
;; This facility is not yet widely tested and may be fooled by obscure
;; man pages that `break the rules'.

;; WoMan is configured not to replace spaces in an imenu *Completion*
;; buffer.  For further documentation of the use of imenu, such as
;; menu sorting, see the source file imenu.el, which is distributed
;; with GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Howard Melman made (essentially) the following suggestions, which
;; are slightly different from the expression that I currently use.
;; You may prefer one of Howard's suggestions, which I think assume
;; that `case-fold-search' is `t' (which it is by default):

;; (setq woman-imenu-generic-expression
;;       '((nil "^\\(   \\)?\\([A-Z][A-Z ]+[A-Z]\\)[ \t]*$" 2)))

;; will give support for .SH and .SS, though it won't show the heading
;; name hierarchy.  If you just want .SH in the imenu then use:

;; (setq woman-imenu-generic-expression
;;       '((nil "^\\([A-Z][A-Z ]+[A-Z]\\)[ \t]*$" 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Vertical spacing and blank lines
;; ================================

;; The number of consecutive blank lines in the formatted buffer
;; should be either 0 or 1.  A blank line should leave a space like
;; .sp 1 (p. 14).  Current policy is to output vertical space only
;; immediately before text is output.


;; Horizontal and vertical spacing and resolution
;; ==============================================

;; WoMan currently assumes 10 characters per inch horizontally, hence
;; a horizontal resolution of 24 basic units, and 5 lines per inch
;; vertically, hence a vertical resolution of 48 basic units.  (nroff
;; uses 240 per inch).


;; The *WoMan-Log* buffer
;; ======================

;; This is modeled on the byte-compiler.  It logs all files formatted
;; by WoMan, and if WoMan finds anything that it cannot handle then it
;; writes a warning to this buffer.  If the variable `woman-show-log'
;; is non-nil (by default it is `nil') then WoMan automatically
;; displays this buffer.  Many WoMan warnings can be completely
;; ignored, because they are reporting the fact that WoMan has ignored
;; requests that it is correct to ignore.  In some future version this
;; level of paranoia will be reduced, but not until WoMan is more
;; reliable.  At present, all warnings should be treated with some
;; suspicion.  Uninterpreted escape sequences are also logged (in some
;; cases).

;; Uninterpreted ?roff requests can optionally be left in the
;; formatted buffer to indicate precisely where they occur by
;; resetting the variable `woman-ignore' to `nil' (by default it is
;; `t').

;; Automatic initiation of woman decoding

;; (Probably not a good idea.  If you use it, be careful!)

;; Put something like this in your .emacs.  The call to
;; set-visited-file-name is to avoid font-locking triggered by
;; automatic major mode selection.

;; (autoload 'woman-decode-region "woman")

;; (setq format-alist
;;       (cons
;;        '(man "UN*X man-page source format" "\\.\\(TH\\|ig\\) "
;; 	     woman-decode-region nil nil
;; 	     (lambda (arg)
;; 		set-visited-file-name
;; 		(file-name-sans-extension buffer-file-name)))))
;;       format-alist))


;; Reporting Bugs
;; ==============

;; If WoMan fails completely, or formats a file incorrectly
;; (i.e. obviously wrongly or significantly differently from man) or
;; inelegantly, then please

;; (a) check that you are running the latest version of woman.el
;;     available from my web site (see above),

;; (b) check that the problem is not already described in the file
;;     woman.status, also available from my web site.

;; If both of the above are true then please email me the entry from
;; the *WoMan-Log* buffer relating to the problem file, together with
;; a brief description of the problem.  Please indicate where you got
;; the source file from, but do not send it to me unless I ask you to!
;; Thanks.  (There is at present no automated bug-reporting facility
;; for WoMan.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE:

;; CASE-DEPENDENCE OF FILENAMES.  By default, WoMan ignores case in
;; file pathnames only when it seems appropriate.  MS-Windows users
;; who want complete case independence should set the NTEmacs variable
;; `w32-downcase-file-names' to `t' and use all lower case when
;; setting WoMan file paths.

;; (1) INCOMPATIBLE CHANGE!  WoMan no longer uses a persistent topic
;; cache by default.  (It caused too much confusion!)  Explicitly set
;; the variable `woman-cache-filename' to save the cache between Emacs
;; sessions.  This is recommended only if the command `woman' is too
;; slow the first time that it is run in an Emacs session, while it
;; builds its cache in main memory, which MAY be VERY slow.

;; (2) The user option `woman-cache-level' controls the amount of
;; information cached (in main memory and, optionally, saved to disc).

;; (3) UPDATING THE CACHE.  A prefix argument always causes the
;; `woman' command (only) to rebuild its topic cache, and to re-save
;; it to `woman-cache-filename' if this variable has a non-nil value.
;; This is necessary if the NAMES (not contents) of any of the
;; directories or files in the paths specified by `woman-manpath' or
;; `woman-path' change.  If WoMan user options that affect the cache
;; are changed then WoMan will automatically update its cache file on
;; disc (if one is in use) the next time it is run in a new Emacs
;; session.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TO DO
;; =====

;; Reconsider case sensitivity of file names.
;; MUST PROCESS .if, .nr IN ORDER ENCOUNTERED IN FILE! (rcsfile, mf).
;; Allow general delimiter in `\v', cf. `\h'.
;; Improve major-mode documentation.
;; Pre-process conditionals in macro bodies if possible for speed?
;; Emulate more complete preprocessor support for tbl (.TS/.TE)
;; Emulate some preprocessor support for eqn (.EQ/.EN)
;; Re-write filling and adjusting code!
;; Allow word wrap at comma (for long option lists)?
;; Buffer list handling not quite right.
;; Make 10 or 12 pitch (cpi) optional -- 12 => ll = 78
;; Use unpaddable space for tabbing?
;; Tidy up handling of fonts when filling and adjusting
;;   -- see text/text properties?
;; Improve speed
;; Add font-lock support (for quoted strings, etc.)?
;; Optionally save large files in enriched format?
;; Add apropos facility by searching NAME (?) entry in man files?
;; Documentation -- optional auto-display of formatted WoMan man page?
;; Implement a bug reporter?
;; Support diversion and traps (to some extent) - for Tcl/tk pages?
;; Add a menu of WoMan buffers?
;; Fix .fc properly?


;; Implementation strategy [this description is now well out of date!]
;; -- three main passes, each to process respectively:

;;   1) non-breaking `.' requests including font macros
;;   2) \ escape sequences, mainly special characters and font changes
;;   3) breaking `.' requests, mainly filling and justification

;; For each pass, a control function finds and pre-processes the
;; escape or request and then calls the appropriate function to
;; perform the required formatting.  Based originally on enriched.el
;; and format.el.

;; The background information that made this project possible is
;; freely available courtesy of Bell Labs from
;; http://cm.bell-labs.com/7thEdMan/


;; Acknowledgements
;; ================

;; For Heather, Kathryn and Madelyn, the women in my life
;; (although they will probably never use it)!

;; I also thank the following for helpful suggestions, bug reports,
;; code fragments, general interest, etc.:
;;   Jari Aalto <jari.aalto@cs.tpu.fi>
;;   Dean Andrews <dean@dra.com>
;;   Juanma Barranquero <lekktu@gmail.com>
;;   Karl Berry <kb@cs.umb.edu>
;;   Jim Chapman <jchapman@netcomuk.co.uk>
;;   Kin Cho <kin@neoscale.com>
;;   Frederic Corne <frederic.corne@erli.fr>
;;   Peter Craft <craft@alacritech.com>
;;   Charles Curley <ccurley@trib.com>
;;   Jim Davidson <jdavidso@teknowledge.com>
;;   Kevin D'Elia <Kevin.DElia@mci.com>
;;   John Fitch <jpff@maths.bath.ac.uk>
;;   Hans Frosch <jwfrosch@rish.b17c.ingr.com>
;;   Guy Gascoigne-Piggford <ggp@informix.com>
;;   Brian Gorka <gorkab@sanchez.com>
;;   Nicolai Henriksen <nhe@lyngso-industri.dk>
;;   Thomas Herchenroeder <the@software-ag.de>
;;   Alexander Hinds <ahinds@thegrid.net>
;;   Stefan Hornburg <sth@hacon.de>
;;   Theodore Jump <tjump@cais.com>
;;   David Kastrup <dak@gnu.org>
;;   Paul Kinnucan <paulk@mathworks.com>
;;   Jonas Linde <jonas@init.se>
;;   Andrew McRae <andrewm@optimation.co.nz>
;;   Howard Melman <howard@silverstream.com>
;;   Dennis Pixton <dennis@math.binghamton.edu>
;;   T. V. Raman <raman@Adobe.COM>
;;   Bruce Ravel <bruce.ravel@nist.gov>
;;   Benjamin Riefenstahl <benny@crocodial.de>
;;   Kevin Ruland <kruland@seistl.com>
;;   Tom Schutter <tom@platte.com>
;;   Wei-Xue Shi <wxshi@ma.neweb.ne.jp>
;;   Fabio Somenzi <fabio@joplin.colorado.edu>
;;   Karel Sprenger <ks@ic.uva.nl>
;;   Chris Szurgot <szurgot@itribe.net>
;;   Paul A. Thompson <pat@po.cwru.edu>
;;   Arrigo Triulzi <arrigo@maths.qmw.ac.uk>
;;   Geoff Voelker <voelker@cs.washington.edu>
;;   Eli Zaretskii <eliz@gnu.org>


;;; Code:

(defvar woman-version "0.551 (beta)" "WoMan version information.")

(require 'man)
(require 'button)
(define-button-type 'WoMan-xref-man-page
  :supertype 'Man-abstract-xref-man-page
  'func (lambda (arg)
	  (woman
	   ;; `woman' cannot deal with arguments that contain a
	   ;; section name, like close(2), so strip the section name.
	   (if (string-match Man-reference-regexp arg)
	       (substring arg 0 (match-end 1))
	     arg))))

(eval-when-compile			; to avoid compiler warnings
  (require 'dired)
  (require 'cl)
  (require 'apropos))

(defun woman-mapcan (fn x)
  "Return concatenated list of FN applied to successive `car' elements of X.
FN must return a list, cons or nil.  Useful for splicing into a list."
  ;; Based on the Standard Lisp function MAPCAN but with args swapped!
  ;; More concise implementation than the recursive one.  -- dak
  (apply #'nconc (mapcar fn x)))

(defun woman-parse-colon-path (paths)
  "Explode search path string PATHS into a list of directory names.
Allow Cygwin colon-separated search paths on Microsoft platforms.
Replace null components by calling `woman-parse-man.conf'.
As a special case, if PATHS is nil then replace it by calling
`woman-parse-man.conf'."
  ;; Based on suggestions by Jari Aalto and Eli Zaretskii.
  ;; parse-colon-path returns nil for a null path component and
  ;; an empty substring of MANPATH denotes the default list.
  (if (memq system-type '(windows-nt ms-dos))
      (cond ((null paths)
	     (mapcar 'woman-Cyg-to-Win (woman-parse-man.conf)))
	    ((string-match ";" paths)
	     ;; Assume DOS-style path-list...
	     (woman-mapcan		; splice list into list
	      (lambda (x)
		(if x
		    (list x)
		  (mapcar 'woman-Cyg-to-Win (woman-parse-man.conf))))
	      (parse-colon-path paths)))
	    ((string-match "\\`[a-zA-Z]:" paths)
	     ;; Assume single DOS-style path...
	     (list paths))
	    (t
	     ;; Assume UNIX/Cygwin-style path-list...
	     (woman-mapcan		; splice list into list
	      (lambda (x)
		(mapcar 'woman-Cyg-to-Win
			(if x (list x) (woman-parse-man.conf))))
	      (let ((path-separator ":"))
		(parse-colon-path paths)))))
    ;; Assume host-default-style path-list...
    (woman-mapcan			; splice list into list
     (lambda (x) (if x (list x) (woman-parse-man.conf)))
     (parse-colon-path (or paths "")))))

(defun woman-Cyg-to-Win (file)
  "Convert an absolute filename FILE from Cygwin to Windows form."
  ;; MANPATH_MAP conses are not converted since they presumably map
  ;; Cygwin to Cygwin form.
  (if (consp file)
      file
    ;; Code taken from w32-symlinks.el
    (if (eq (aref file 0) ?/)
	;; Try to use Cygwin mount table via `cygpath.exe'.
	(condition-case nil
	    (with-temp-buffer
	      ;; cygpath -m file
	      (call-process "cygpath" nil t nil "-m" file)
	      (buffer-substring 1 (buffer-size)))
	  (error
	   ;; Assume no `cygpath' program available.
	   ;; Hack /cygdrive/x/ or /x/ or (obsolete) //x/ to x:/
	   (when (string-match "\\`\\(/cygdrive\\|/\\)?/./" file)
	     (if (match-beginning 1)		; /cygdrive/x/ or //x/ -> /x/
		 (setq file (substring file (match-end 1))))
	     (aset file 0 (aref file 1))	; /x/ -> xx/
	     (aset file 1 ?:))		; xx/ -> x:/
	   file))
      file)))


;;; User options:

;; NB: Group identifiers must be lowercase!

(defgroup woman nil
  "Browse UNIX manual pages `wo (without) man'."
  :tag "WoMan"
  :group 'help)

(defcustom woman-show-log nil
  "If non-nil then show the *WoMan-Log* buffer if appropriate.
I.e. if any warning messages are written to it.  Default is nil."
  :type 'boolean
  :group 'woman)

(defcustom woman-pre-format-hook nil
  "Hook run by WoMan immediately before formatting a buffer.
Change only via `Customization' or the function `add-hook'."
  :type 'hook
  :group 'woman)

(defcustom woman-post-format-hook nil
  "Hook run by WoMan immediately after formatting a buffer.
Change only via `Customization' or the function `add-hook'."
  :type 'hook
  :group 'woman)


;; Interface options

(defgroup woman-interface nil
  "Interface options for browsing UNIX manual pages `wo (without) man'."
  :tag "WoMan Interface"
  :group 'woman)

(defcustom woman-man.conf-path
  (let ((path '("/usr/lib" "/etc")))
    (cond ((eq system-type 'windows-nt)
	   (mapcar 'woman-Cyg-to-Win path))
	  ((eq system-type 'darwin)
	   (cons "/usr/share/misc" path))
	  (t path)))
  "List of dirs to search and/or files to try for man config file.
A trailing separator (`/' for UNIX etc.) on directories is
optional, and the filename is used if a directory specified is
the first to start with \"man\" and has an extension starting
with \".conf\".  If MANPATH is not set but a config file is found
then it is parsed instead to provide a default value for
`woman-manpath'."
  :type '(repeat string)
  :group 'woman-interface)

(defun woman-parse-man.conf ()
  "Parse if possible configuration file for man command.
Used only if MANPATH is not set or contains null components.
Look in `woman-man.conf-path' and return a value for `woman-manpath'.
Concatenate data from all lines in the config file of the form
  MANPATH  /usr/man
or
  MANDATORY_MANPATH  /usr/man
or
  OPTIONAL_MANPATH  /usr/man
or
  MANPATH_MAP /opt/bin /opt/man"
  ;; Functionality suggested by Charles Curley.
  (let ((path woman-man.conf-path)
	file manpath)
    (while (and
	    path
	    (not (and
		  (file-readable-p (setq file (car path)))
		  ;; If not a file then find the file:
		  (or (not (file-directory-p file))
		      (and
		       (setq file
			     (directory-files file t "\\`man.*\\.conf[a-z]*\\'" t))
		       (file-readable-p (setq file (car file)))))
		  ;; Parse the file -- if no MANPATH data ignore it:
		  (with-temp-buffer
		    (insert-file-contents file)
		    (while (re-search-forward
			    ;; `\(?: ... \)' is a "shy group"
			    "\
^[ \t]*\\(?:\\(?:MANDATORY_\\|OPTIONAL_\\)?MANPATH[ \t]+\\(\\S-+\\)\\|\
MANPATH_MAP[ \t]+\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\)" nil t)
		      (add-to-list 'manpath
				   (if (match-beginning 1)
				       (match-string 1)
				     (cons (match-string 2)
					   (match-string 3)))))
		    manpath))
		 ))
      (setq path (cdr path)))
    (nreverse manpath)))

;; Autoload so set-locale-environment can operate on it.
;;;###autoload
(defcustom woman-locale nil
  "String specifying a manual page locale, or nil.
If a manual page is available in the specified locale
\(e.g. \"sv_SE.ISO8859-1\"), it will be offered in preference to the
default version.  Normally, `set-locale-environment' sets this at startup."
  :type '(choice string (const nil))
  :group 'woman-interface
  :version "23.1")

;; FIXME Is this a sensible list of alternatives?
(defun woman-expand-locale (locale)
  "Expand a locale into a list suitable for man page lookup.
Expands a locale of the form LANGUAGE_TERRITORY.CHARSET into the list:
LANGUAGE_TERRITORY.CHARSET LANGUAGE_TERRITORY LANGUAGE.CHARSET LANGUAGE.
The TERRITORY and CHARSET portions may be absent."
  (string-match "\\([^._]*\\)\\(_[^.]*\\)?\\(\\..*\\)?" locale)
  (let ((lang (match-string 1 locale))
        (terr (match-string 2 locale))
        (charset (match-string 3 locale)))
    (delq nil (list locale
                    (and charset terr (concat lang terr))
                    (and charset terr (concat lang charset))
                    (if (or charset terr) lang)))))

(defun woman-manpath-add-locales (manpath)
  "Add locale-specific subdirectories to the elements of MANPATH.
MANPATH is a list of the form of `woman-manpath'.  Returns a list
with those locale-specific subdirectories specified by the action
of `woman-expand-locale' on `woman-locale' added, where they exist."
  (if (zerop (length woman-locale))
      manpath
    (let ((subdirs (woman-expand-locale woman-locale))
          lst dir)
      (dolist (elem manpath (nreverse lst))
        (dolist (sub subdirs)
          (when (file-directory-p
                 (setq dir
                       ;; Use f-n-a-d because parse-colon-path does.
                       (file-name-as-directory
                        (expand-file-name sub (substitute-in-file-name
                                               (if (consp elem)
                                                   (cdr elem)
                                                 elem))))))
            (add-to-list 'lst (if (consp elem)
                                  (cons (car elem) dir)
                                dir))))
        ;; Non-locale-specific has lowest precedence.
        (add-to-list 'lst elem)))))

(defcustom woman-manpath
  ;; Locales could also be added in woman-expand-directory-path.
  (or (woman-manpath-add-locales
       (woman-parse-colon-path (getenv "MANPATH")))
      '("/usr/man" "/usr/share/man" "/usr/local/man"))
  "List of DIRECTORY TREES to search for UN*X manual files.
Each element should be the name of a directory that contains
subdirectories of the form `man?', or more precisely subdirectories
selected by the value of `woman-manpath-man-regexp'.  Non-directory
and unreadable files are ignored.

Elements can also be a cons cell indicating a mapping from PATH
to manual trees: if such an element's car is equal to a path
element of the environment variable PATH, the cdr of the cons
cell is included in the directory tree search.

If not set then the environment variable MANPATH is used.  If no such
environment variable is found, the default list is determined by
consulting the man configuration file if found, which is determined by
the user option `woman-man.conf-path'.  An empty substring of MANPATH
denotes the default list.

Any environment variables (names must have the UN*X-style form $NAME,
e.g. $HOME, $EMACSDATA, $emacs_dir) are evaluated first but each
element must evaluate to a SINGLE directory name.  Trailing `/'s are
ignored.  (Specific directories in `woman-path' are also searched.)

Microsoft platforms:
I recommend including drive letters explicitly, e.g.

  (\"C:/Cygwin/usr/man/\" \"C:/Cygwin/usr/local/man\").

The MANPATH environment variable may be set using DOS semi-colon-
separated or UN*X/Cygwin colon-separated syntax (but not mixed)."
  :type '(repeat (choice string (cons string string)))
  :version "23.1"                    ; added woman-manpath-add-locales
  :group 'woman-interface)

(defcustom woman-manpath-man-regexp "[Mm][Aa][Nn]"
  "Regexp to match man directories UNDER `woman-manpath' directories.
These normally have names of the form `man?'.  Its default value is
\"[Mm][Aa][Nn]\", which is case-insensitive mainly for the benefit of
Microsoft platforms.  Its purpose is to avoid `cat?', `.', `..', etc."
  ;; Based on a suggestion by Wei-Xue Shi.
  :type 'string
  :group 'woman-interface)

(defcustom woman-path
  (if (eq system-type 'ms-dos) '("$DJDIR/info" "$DJDIR/man/cat[1-9onlp]"))
  "List of SPECIFIC DIRECTORIES to search for UN*X manual files.
For example

  (\"/emacs/etc\").

These directories are searched in addition to the directory trees
specified in `woman-manpath'.  Each element should be a directory
string or nil, which represents the current directory when the path is
expanded and cached.  However, the last component (only) of each
directory string is treated as a regexp \(Emacs, not shell) and the
string is expanded into a list of matching directories.  Non-directory
and unreadable files are ignored.  The default value is nil.

Any environment variables (which must have the UN*X-style form $NAME,
e.g. $HOME, $EMACSDATA, $emacs_dir) are evaluated first but each
element must evaluate to a SINGLE directory name (regexp, see above).
For example

  (\"$EMACSDATA\") [or equivalently (\"$emacs_dir/etc\")].

Trailing `/'s are discarded.  (The directory trees in `woman-manpath'
are also searched.)  On Microsoft platforms I recommend including
drive letters explicitly."
  :type '(repeat (choice string (const nil)))
  :group 'woman-interface)

(defcustom woman-cache-level 2
  "The level of topic caching.
1 - cache only the topic and directory lists
    (the only level before version 0.34 - only for compatibility);
2 - cache also the directories for each topic
    (faster, without using much more memory);
3 - cache also the actual filenames for each topic
    (fastest, but uses twice as much memory).
The default value is currently 2, a good general compromise.
If the `woman' command is slow to find files then try 3, which may be
particularly beneficial with large remote-mounted man directories.
Run the `woman' command with a prefix argument or delete the cache
file `woman-cache-filename' for a change to take effect.
\(Values < 1 behave like 1; values > 3 behave like 3.)"
  :type '(choice (const :tag "Minimal" 1)
		 (const :tag "Default" 2)
		 (const :tag "Maximal" 3))
  :group 'woman-interface)

(defcustom woman-cache-filename nil
  "The full pathname of the WoMan directory and topic cache file.
It is used to save and restore the cache between sessions.  This is
especially useful with remote-mounted man page files!  The default
value of nil suppresses this action.  The `standard' non-nil
filename is \"~/.wmncach.el\".  Remember that a prefix argument forces
the `woman' command to update and re-write the cache."
  :type '(choice (const :tag "None" nil)
		 (const :tag "~/.wmncach.el" "~/.wmncach.el")
		 file)
  :group 'woman-interface)

(defcustom woman-dired-keys t
  "List of `dired' mode keys to define to run WoMan on current file.
E.g. '(\"w\" \"W\"), or any non-null atom to automatically define
\"w\" and \"W\" if they are unbound, or nil to do nothing.
Default is t."
  :type '(choice (const :tag "None" nil)
		 (repeat string)
		 (other :tag "Auto" t))
  :group 'woman-interface)

(defcustom woman-imenu-generic-expression
  '((nil "\n\\([A-Z].*\\)" 1) ; SECTION, but not TITLE
    ("*Subsections*" "^   \\([A-Z].*\\)" 1))
  "Imenu support for Sections and Subsections.
An alist with elements of the form (MENU-TITLE REGEXP INDEX) --
see the documentation for `imenu-generic-expression'."
  :type 'sexp
  :group 'woman-interface)

(defcustom woman-imenu nil
  "If non-nil then WoMan adds a Contents menu to the menubar.
It does this by calling `imenu-add-to-menubar'.  Default is nil."
  :type 'boolean
  :group 'woman-interface)

(defcustom woman-imenu-title "CONTENTS"
  "The title to use if WoMan adds a Contents menu to the menubar.
Default is \"CONTENTS\"."
  :type 'string
  :group 'woman-interface)

(defcustom woman-use-topic-at-point-default nil
  ;; `woman-use-topic-at-point' may be let-bound when woman is loaded,
  ;; in which case its global value does not get defined.
  ;; `woman-file-name' sets it to this value if it is unbound.
  "Default value for `woman-use-topic-at-point'."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'woman-interface)

(defcustom woman-use-topic-at-point woman-use-topic-at-point-default
  "Control use of the word at point as the default topic.
If non-nil the `woman' command uses the word at point automatically,
without interactive confirmation, if it exists as a topic."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'woman-interface)

(defvar woman-file-regexp nil
  "Regexp used to select (possibly compressed) man source files, e.g.
\"\\.\\([0-9lmnt]\\w*\\)\\(\\.\\(g?z\\|bz2\\|xz\\)\\)?\\'\".
Built automatically from the customizable user options
`woman-uncompressed-file-regexp' and `woman-file-compression-regexp'.")

(defvar woman-uncompressed-file-regexp)	; for the compiler
(defvar woman-file-compression-regexp)	; for the compiler

(defun set-woman-file-regexp (symbol value)
  "Bind SYMBOL to VALUE and set `woman-file-regexp' as per user customizations.
Used as :set cookie by Customize when customizing the user options
`woman-uncompressed-file-regexp' and `woman-file-compression-regexp'."
  (set-default symbol value)
  (and (boundp 'woman-uncompressed-file-regexp)
       (boundp 'woman-file-compression-regexp)
       (setq woman-file-regexp
	     (concat woman-uncompressed-file-regexp
		     "\\("
		     (substring woman-file-compression-regexp 0 -2)
		     "\\)?\\'"))))

(defcustom woman-uncompressed-file-regexp
  "\\.\\([0-9lmnt]\\w*\\)"		; disallow no extension
  "Do not change this unless you are sure you know what you are doing!
Regexp used to select man source files (ignoring any compression extension).

The SysV standard man pages use two character suffixes, and this is
becoming more common in the GNU world.  For example, the man pages
in the ncurses package include `toe.1m', `form.3x', etc.

Note: an optional compression regexp will be appended, so this regexp
MUST NOT end with any kind of string terminator such as $ or \\'."
  :type 'regexp
  :set 'set-woman-file-regexp
  :group 'woman-interface)

(defcustom woman-file-compression-regexp
  "\\.\\(g?z\\|bz2\\|xz\\)\\'"
  "Do not change this unless you are sure you know what you are doing!
Regexp used to match compressed man file extensions for which
decompressors are available and handled by auto-compression mode,
e.g. \"\\\\.\\\\(g?z\\\\|bz2\\\\|xz\\\\)\\\\'\" for `gzip', `bzip2', or `xz'.
Should begin with \\. and end with \\' and MUST NOT be optional."
  ;; Should be compatible with car of
  ;; `jka-compr-file-name-handler-entry', but that is unduly
  ;; complicated, includes an inappropriate extension (.tgz) and is
  ;; not loaded by default!
  :version "24.1"                       ; added xz
  :type 'regexp
  :set 'set-woman-file-regexp
  :group 'woman-interface)

(defcustom woman-use-own-frame nil
  "If non-nil then use a dedicated frame for displaying WoMan windows.
Only useful when run on a graphic display such as X or MS-Windows."
  :type 'boolean
  :group 'woman-interface)


;; Formatting options

(defgroup woman-formatting nil
  "Formatting options for browsing UNIX manual pages `wo (without) man'."
  :tag "WoMan Formatting"
  :group 'woman)

(defcustom woman-fill-column 65
  "Right margin for formatted text -- default is 65."
  :type 'integer
  :group 'woman-formatting)

(defcustom woman-fill-frame nil
  ;; Based loosely on a suggestion by Theodore Jump:
  "If non-nil then most of the window width is used."
  :type 'boolean
  :group 'woman-formatting)

(defcustom woman-default-indent 5
  "Default prevailing indent set by -man macros -- default is 5.
Set this variable to 7 to emulate GNU man formatting."
  :type 'integer
  :group 'woman-formatting)

(defcustom woman-bold-headings t
  "If non-nil then embolden section and subsection headings.  Default is t.
Heading emboldening is NOT standard `man' behavior."
  :type 'boolean
  :group 'woman-formatting)

(defcustom woman-ignore t
  "If non-nil then unrecognized requests etc. are ignored.  Default is t.
This gives the standard ?roff behavior.  If nil then they are left in
the buffer, which may aid debugging."
  :type 'boolean
  :group 'woman-formatting)

(defcustom woman-preserve-ascii t
  "If non-nil, preserve ASCII characters in the WoMan buffer.
Otherwise, to save time, some backslashes and spaces may be
represented differently (as the values of the variables
`woman-escaped-escape-char' and `woman-unpadded-space-char'
respectively) so that the buffer content is strictly wrong even though
it should display correctly.  This should be irrelevant unless the
buffer text is searched, copied or saved to a file."
  ;; This option should probably be removed!
  :type 'boolean
  :group 'woman-formatting)

(defcustom woman-emulation 'nroff
  "WoMan emulation, currently either nroff or troff.  Default is nroff.
Troff emulation is experimental and largely untested.
\(Add groff later?)"
  :type '(choice (const nroff) (const troff))
  :group 'woman-formatting)


;; Faces:

(defgroup woman-faces nil
  "Face options for browsing UNIX manual pages `wo (without) man'."
  :tag "WoMan Faces"
  :group 'woman
  :group 'faces)

(defcustom woman-fontify
  (or (and (fboundp 'display-color-p) (display-color-p))
      (and (fboundp 'display-graphic-p) (display-graphic-p))
      (x-display-color-p))
  "If non-nil then WoMan assumes that face support is available.
It defaults to a non-nil value if the display supports either colors
or different fonts."
  :type 'boolean
  :group 'woman-faces)

(defface woman-italic
  '((t :inherit italic))
  "Face for italic font in man pages."
  :group 'woman-faces)
(define-obsolete-face-alias 'woman-italic-face 'woman-italic "22.1")

(defface woman-bold
  '((t :inherit bold))
  "Face for bold font in man pages."
  :group 'woman-faces)
(define-obsolete-face-alias 'woman-bold-face 'woman-bold "22.1")

(defface woman-unknown
  '((t :inherit font-lock-warning-face))
  "Face for all unknown fonts in man pages."
  :group 'woman-faces)
(define-obsolete-face-alias 'woman-unknown-face 'woman-unknown "22.1")

(defface woman-addition
  '((t :inherit font-lock-builtin-face))
  "Face for all WoMan additions to man pages."
  :group 'woman-faces)
(define-obsolete-face-alias 'woman-addition-face 'woman-addition "22.1")

(defun woman-default-faces ()
  "Set foreground colors of italic and bold faces to their default values."
  (interactive)
  (face-spec-set 'woman-italic (face-user-default-spec 'woman-italic))
  (face-spec-set 'woman-bold (face-user-default-spec 'woman-bold)))

(defun woman-monochrome-faces ()
  "Set foreground colors of italic and bold faces to that of the default face.
This is usually either black or white."
  (interactive)
  (set-face-foreground 'woman-italic 'unspecified)
  (set-face-foreground 'woman-bold 'unspecified))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimental font support, initially only for MS-Windows.
(defconst woman-font-support
  (eq window-system 'w32)		; Support X later!
  "If non-nil then non-ASCII characters and symbol font supported.")

(defun woman-select-symbol-fonts (fonts)
  "Select symbol fonts from a list FONTS of font name strings."
  (let (symbol-fonts)
    ;; With NTEmacs 20.5, the PATTERN option to `x-list-fonts' does
    ;; not seem to work and fonts may be repeated, so ...
    (dolist (font fonts)
      (and (string-match "-Symbol-" font)
	   (not (member font symbol-fonts))
	   (setq symbol-fonts (cons font symbol-fonts))))
    symbol-fonts))

(declare-function x-list-fonts "xfaces.c"
		  (pattern &optional face frame maximum width))

(when woman-font-support
  (make-face 'woman-symbol)

  ;; Set the symbol font only if `woman-use-symbol-font' is true, to
  ;; avoid unnecessarily upsetting the line spacing in NTEmacs 20.5!

  (defcustom woman-use-extended-font t
    "If non-nil then may use non-ASCII characters from the default font."
    :type 'boolean
    :group 'woman-faces)

  (defcustom woman-use-symbol-font nil
    "If non-nil then may use the symbol font.
It is off by default, mainly because it may change the line spacing
\(in NTEmacs 20.5)."
    :type 'boolean
    :group 'woman-faces)

  (defconst woman-symbol-font-list
    (or (woman-select-symbol-fonts (x-list-fonts "*" 'default))
	(woman-select-symbol-fonts (x-list-fonts "*")))
    "Symbol font(s), preferably same size as default when WoMan was loaded.")

  (defcustom woman-symbol-font (car woman-symbol-font-list)
    "A string describing the symbol font to use for special characters.
It should be compatible with, and the same size as, the default text font.
Under MS-Windows, the default is
  \"-*-Symbol-normal-r-*-*-*-*-96-96-p-*-ms-symbol\"."
    :type `(choice
	    ,@(mapcar (lambda (x) (list 'const x))
		      woman-symbol-font-list)
	    string)
    :group 'woman-faces)

  )

;; For non windows-nt ...
(defvar woman-use-extended-font nil)
(defvar woman-use-symbol-font nil)
(defvar woman-symbol-font nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Internal variables:

(defconst woman-justify-list
  '(left right center full)
  "Justify styles for `fill-region-as-paragraph'.")
(defconst woman-adjust-left 0		; == adjust off, noadjust
  "Adjustment indicator `l' -- adjust left margin only.")
(defconst woman-adjust-right 1
  "Adjustment indicator `r' -- adjust right margin only.")
(defconst woman-adjust-center 2
  "Adjustment indicator `c' -- center.")
(defconst woman-adjust-both 3		; default -- adj,both
  "Adjustment indicator `b' or `n' -- adjust both margins.")

(defvar woman-adjust woman-adjust-both
  "Current adjustment number-register value.")
(defvar woman-adjust-previous woman-adjust
  "Previous adjustment number-register value.")
(defvar woman-justify
  (nth woman-adjust woman-justify-list)	; use vector?
  "Current justification style for `fill-region-as-paragraph'.")
(defvar woman-justify-previous woman-justify
  "Previous justification style for `fill-region-as-paragraph'.")

(defvar woman-left-margin woman-default-indent
  "Current left margin.")
(defvar woman-prevailing-indent woman-default-indent
  "Current prevailing indent.")
(defvar woman-interparagraph-distance 1
  "Interparagraph distance in lines.
Set by .PD; used by .SH, .SS, .TP, .LP, .PP, .P, .IP, .HP.")
(defvar woman-leave-blank-lines nil
  "Blank lines to leave as vertical space.")
(defconst woman-tab-width 5
  "Default tab width set by -man macros.")
(defvar woman-nofill nil
  "Current fill mode: nil for filling.")
(defvar woman-RS-left-margin nil
  "Left margin stack for nested use of `.RS/.RE'.")
(defvar woman-RS-prevailing-indent nil
  "Prevailing indent stack for nested use of `.RS/.RE'.")
(defvar woman-nospace nil
  "Current no-space mode: nil for normal spacing.
Set by `.ns' request; reset by any output or `.rs' request")
;; Used for message logging
(defvar WoMan-current-file nil)		; bound in woman-really-find-file
(defvar WoMan-Log-header-point-max nil)

(defsubst woman-reset-nospace ()
  "Set `woman-nospace' to nil."
  (setq woman-nospace nil))

(defconst woman-request-regexp "^[.'][ \t]*\\(\\S +\\) *"
  ;; Was "^\\.[ \t]*\\([a-z0-9]+\\) *" but cvs.1 uses a macro named
  ;; "`" and CGI.man uses a macro named "''"!
  ;; CGI.man uses ' as control character in places -- it *should*
  ;; suppress breaks!
  ;; Could end with "\\( +\\|$\\)" instead of " *"
  "Regexp to match a ?roff request plus trailing white space.")

(defvar woman-imenu-done nil
  "Buffer-local: set to true if function `woman-imenu' has been called.")
(make-variable-buffer-local 'woman-imenu-done)

;; From imenu.el -- needed when reformatting a file in its old buffer.
;; The latest buffer index used to update the menu bar menu.
(eval-when-compile
  (require 'imenu))
(make-variable-buffer-local 'imenu--last-menubar-index-alist)

(defvar woman-buffer-alist nil
  "An alist representing WoMan buffers that are already decoded.
Each element is of the form (FILE-NAME . BUFFER-NAME).")

(defvar woman-buffer-number 0
  "Ordinal number of current buffer entry in `woman-buffer-alist'.
The ordinal numbers start from 0.")

(defvar woman-if-conditions-true '(?n ?e ?o)
  "List of one-character built-in condition names that are true.
Should include ?e, ?o (page even/odd) and either ?n (nroff) or ?t (troff).
Default is '(?n ?e ?o).  Set via `woman-emulation'.")


;;; Specialized utility functions:

;;; Fast deletion without saving on the kill ring (cf. simple.el):

(defun woman-delete-line (&optional arg)
  "Delete rest of current line; if all blank then delete thru newline.
With a numeric argument ARG, delete that many lines from point.
Negative arguments delete lines backward."
  ;; This is a non-interactive version of kill-line in simple.el that
  ;; deletes instead of killing and assumes kill-whole-line is nil,
  ;; which is essential!
  (delete-region (point)
		 (progn
		   (if arg
		       (forward-line arg)
		     (if (eobp)
			 (signal 'end-of-buffer nil))
		     (if (looking-at "[ \t]*$")
			 (forward-line 1)
		       (end-of-line)))
		 (point))))

(defsubst woman-delete-whole-line ()
  "Delete current line from beginning including eol."
  (beginning-of-line)
  (woman-delete-line 1))

(defsubst woman-delete-following-space ()
  "Delete all spaces and tabs FOLLOWING point (cf. `delete-horizontal-space')."
  ;; cf. delete-horizontal-space in simple.el:
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defsubst woman-delete-match (subexp)
  "Delete subexpression SUBEXP of buffer text matched by last search."
  (delete-region (match-beginning subexp) (match-end subexp)))

;; delete-char does not kill by default
;; delete-backward-char does not kill by default
;; delete-horizontal-space does not kill
;; delete-blank-lines does not kill


;;; File handling:

(defvar woman-expanded-directory-path nil
  "Expanded directory list cache.  Resetting to nil forces update.")

(defvar woman-topic-all-completions nil
  "Expanded topic alist cache.  Resetting to nil forces update.")

;;;###autoload
(defun woman (&optional topic re-cache)
  "Browse UN*X man page for TOPIC (Without using external Man program).
The major browsing mode used is essentially the standard Man mode.
Choose the filename for the man page using completion, based on the
topic selected from the directories specified in `woman-manpath' and
`woman-path'.  The directory expansions and topics are cached for
speed, but a non-nil interactive argument forces the caches to be
updated (e.g. to re-interpret the current directory).

Used non-interactively, arguments are optional: if given then TOPIC
should be a topic string and non-nil RE-CACHE forces re-caching."
  (interactive (list nil current-prefix-arg))
  ;; The following test is for non-interactive calls via gnudoit etc.
  (if (or (not (stringp topic)) (string-match "\\S " topic))
      (let ((file-name (woman-file-name topic re-cache)))
	(if file-name
	    (woman-find-file file-name)
	  (message
	   "WoMan Error: No matching manual files found in search path")
	  (ding)))
    (message "WoMan Error: No topic specified in non-interactive call")
    (ding)))

;; Allow WoMan to be called via the standard Help menu:
(define-key-after menu-bar-manuals-menu [woman]
  '(menu-item "Read Man Page (WoMan)..." woman
	:help "Man-page documentation Without Man") t)

(defvar woman-cached-data nil
  "A list of cached data used to determine cache validity.
Set from the cache by `woman-read-directory-cache'.")

(defun woman-cached-data ()
  "Generate a list of data used to determine cache validity.
Called both to generate and to check the cache!"
  ;; Must use substituted paths because values of env vars may change!
  (list woman-cache-level
	(let (lst path)
	  (dolist (dir woman-manpath (nreverse lst))
	    (when (consp dir)
	      (unless path
		(setq path
		      (split-string (getenv "PATH") path-separator t)))
	      (setq dir (and (member (car dir) path) (cdr dir))))
	    (when dir (add-to-list 'lst (substitute-in-file-name dir)))))
	(mapcar 'substitute-in-file-name woman-path)))

(defun woman-read-directory-cache ()
  "Load the directory and topic cache.
It is loaded from the file named by the variable `woman-cache-filename'.
Return t if the file exists, nil otherwise."
  (and
   woman-cache-filename
   (load woman-cache-filename t nil t)	; file exists
   (equal woman-cached-data (woman-cached-data)))) ; cache valid

(defun woman-write-directory-cache ()
  "Save the directory and topic cache.
It is saved to the file named by the variable `woman-cache-filename'."
  (if woman-cache-filename
      (with-current-buffer (generate-new-buffer "WoMan tmp buffer")
	;; Make a temporary buffer; name starting with space "hides" it.
	(let ((standard-output (current-buffer))
	      (backup-inhibited t))
	  ;; (switch-to-buffer standard-output t) ; only for debugging
	  (buffer-disable-undo standard-output)
	  (princ
	   ";;; WoMan directory and topic cache -- generated automatically\n")
	  (print
	   ;; For data validity check:
	   `(setq woman-cached-data ',(woman-cached-data)))
	  (print
	   `(setq woman-expanded-directory-path
		  ',woman-expanded-directory-path))
	  (print
	   `(setq woman-topic-all-completions
		  ',woman-topic-all-completions))
	  (write-file woman-cache-filename) ; write CURRENT buffer
	  (kill-buffer standard-output)
	  ))))

(defvaralias 'woman-topic-history 'Man-topic-history)
(defvar woman-file-history nil "File-name read history.")

(defun woman-file-name (topic &optional re-cache)
  "Get the name of the UN*X man-page file describing a chosen TOPIC.
When `woman' is called interactively, the word at point may be
automatically used as the topic, if the value of the user option
`woman-use-topic-at-point' is non-nil.  Return nil if no file can
be found.  Optional argument RE-CACHE, if non-nil, forces the
cache to be re-read."
  ;; Handle the caching of the directory and topic lists:
  (unless (and (not re-cache)
	       (or
		(and woman-expanded-directory-path woman-topic-all-completions)
		(woman-read-directory-cache)))
    (message "Building list of manual directory expansions...")
    (setq woman-expanded-directory-path
	  (woman-expand-directory-path woman-manpath woman-path))
    (message "Building completion list of all manual topics...")
    (setq woman-topic-all-completions
	  (woman-topic-all-completions woman-expanded-directory-path))
    (woman-write-directory-cache))
  ;; There is a problem in that I want to offer case-insensitive
  ;; completions, but to return only a case-sensitive match.  This
  ;; does not seem to work properly by default, so I re-do the
  ;; completion if necessary.
  (let (files)
    (or (stringp topic)
	(and (if (boundp 'woman-use-topic-at-point)
		 woman-use-topic-at-point
	       ;; Was let-bound when file loaded, so ...
	       (setq woman-use-topic-at-point woman-use-topic-at-point-default))
	     (setq topic (or (current-word t) "")) ; only within or adjacent to word
	     (test-completion topic woman-topic-all-completions))
	(setq topic
	      (let* ((word-at-point (current-word))
		     (default
		       (when (and word-at-point
				  (test-completion
				   word-at-point woman-topic-all-completions))
			 word-at-point)))
		(completing-read
		 (if default
		     (format "Manual entry (default %s): " default)
		   "Manual entry: ")
		 woman-topic-all-completions nil 1
		 nil
		 'woman-topic-history
		 default))))
    ;; Note that completing-read always returns a string.
    (unless (= (length topic) 0)
      (cond
       ((setq files (woman-file-name-all-completions topic)))
       ;; Complete topic more carefully, i.e. use the completion
       ;; rather than the string entered by the user:
       ((setq files (all-completions topic woman-topic-all-completions))
	(while (/= (length topic) (length (car files)))
	  (setq files (cdr files)))
	(setq files (woman-file-name-all-completions (car files)))))
      (cond
       ((null files) nil)		; no file found for topic.
       ((null (cdr files)) (car (car files))) ; only 1 file for topic.
       (t
	;; Multiple files for topic, so must select 1.
	;; Unread the command event (TAB = ?\t = 9) that runs the command
	;; `minibuffer-complete' in order to automatically complete the
	;; minibuffer contents as far as possible.
	(setq unread-command-events '(9)) ; and delete any type-ahead!
	(completing-read "Manual file: " files nil 1
			 (try-completion "" files) 'woman-file-history))))))

(defun woman-select (predicate list)
  "Select unique elements for which PREDICATE is true in LIST.
\(Note that this function changes the value of LIST.)"
  ;; Intended to be fast by avoiding recursion and list copying.
  (while (and list
	      (or
	       (member (car list) (cdr list))
	       (not (funcall predicate (car list)))))
    (setq list (cdr list)))
  (if list
      (let ((newlist list) cdr_list)
	(while (setq cdr_list (cdr list))
	  (if (and
	       (not (member (car cdr_list) (cdr cdr_list)))
	       (funcall predicate (car cdr_list)))
	      (setq list cdr_list)
	    (setcdr list (cdr cdr_list))))
	newlist)))

(defun woman-file-readable-p (dir)
  "Return t if DIR is readable, otherwise log a warning."
  (or (file-readable-p dir)
      (WoMan-warn "Ignoring unreadable `manpath' directory tree `%s'!" dir)))

(defun woman-directory-files (head dir)
  "Return a sorted list of files in directory HEAD matching regexp in DIR.
Value is a sorted list of the absolute pathnames of all the files in
directory HEAD, or the current directory if HEAD is nil, that match the
regexp that is the final component of DIR.  Log a warning if list is empty."
  (or (directory-files
       (or head (directory-file-name default-directory)) ; was "."
       t
       (file-name-nondirectory dir))
      (WoMan-warn "No directories match `woman-path' entry `%s'!" dir)))

(defun woman-file-accessible-directory-p (dir)
  "Return t if DIR is accessible, otherwise log a warning."
  (or (file-accessible-directory-p dir)
      (WoMan-warn "Ignoring inaccessible `man-page' directory `%s'!" dir)))

(defun woman-expand-directory-path (path-dirs path-regexps)
  "Expand the manual directories in PATH-DIRS and PATH-REGEXPS.
PATH-DIRS should be a list of general manual directories (like
`woman-manpath'), while PATH-REGEXPS should be a list of specific
manual directory regexps (like `woman-path').
Ignore any paths that are unreadable or not directories."
  ;; Allow each path to be a single string or a list of strings:
  (if (not (listp path-dirs)) (setq path-dirs (list path-dirs)))
  (if (not (listp path-regexps)) (setq path-regexps (list path-regexps)))
  (let (head dirs path)
    (dolist (dir path-dirs)
      (when (consp dir)
	(unless path
	  (setq path (split-string (getenv "PATH") path-separator t)))
	(setq dir (and (member (car dir) path)
		       (cdr dir))))
      (if (and dir (woman-file-readable-p dir))
	  ;; NB: `parse-colon-path' creates null elements for
	  ;; redundant (semi-)colons and trailing `/'s!
	  ;; If does not actually matter here if dir ends with `/'.
	  ;; Need regexp "man" here to avoid "cat?", `.', `..', etc.
	  (setq dir (woman-canonicalize-dir dir)
		dirs (nconc dirs (directory-files
				  dir t woman-manpath-man-regexp)))))
    (dolist (dir path-regexps)
      (if (or (null dir)
	      (null (setq dir (woman-canonicalize-dir dir)
			  head (file-name-directory dir)))
	      (woman-file-readable-p head))
	  (setq dirs
		(if dir
		    (nconc dirs (woman-directory-files head dir))
		  (cons (directory-file-name default-directory) dirs))
		;; was "." -- at head of list for later filtering
		)))
    (woman-select 'woman-file-accessible-directory-p dirs)))

(defun woman-canonicalize-dir (dir)
  "Canonicalize the directory name DIR.
Any UN*X-style environment variables are evaluated first."
  (setq dir (expand-file-name (substitute-in-file-name dir)))
  ;; A path that ends with / matches all directories in it,
  ;; including `.' and `..', so remove any trailing / !!!
  (if (string= (substring dir -1) "/")
      (setq dir (substring dir 0 -1)))
  (if (memq system-type '(windows-nt ms-dos cygwin)) ; what else?
      ;; Match capitalization used by `file-name-directory':
      (setq dir (concat (file-name-directory dir)
			(file-name-nondirectory dir))))
  dir)

(defsubst woman-not-member (dir path)
  "Return t if DIR is not a member of the list PATH, nil otherwise.
If DIR is `.' it is first replaced by the current directory."
  (not (member dir path)))

(defun woman-topic-all-completions (path)
  "Return an alist of the man files in all man directories in the list PATH.
The cdr of each alist element is the path-index / filename."
  ;; Support 3 levels of caching: each element of the alist `files'
  ;; will be a list of the first `woman-cache-level' elements of the
  ;; following list: (topic path-index filename).  This alist `files'
  ;; is re-processed by `woman-topic-all-completions-merge'.
  (let (dir files (path-index 0))	; indexing starts at zero
    (while path
      (setq dir (pop path))
      (if (woman-not-member dir path)	; use each directory only once!
	  (push (woman-topic-all-completions-1 dir path-index)
		files))
      (setq path-index (1+ path-index)))
    ;; Uniquify topics:
    ;; Concatenate all lists with a single nconc call to
    ;; avoid retraversing the first lists repeatedly  -- dak
    (woman-topic-all-completions-merge
     (apply #'nconc files))))

(defun woman-topic-all-completions-1 (dir path-index)
  "Return an alist of the man topics in directory DIR with index PATH-INDEX.
A topic is a filename sans type-related extensions.
Support 3 levels of caching: each element of the alist will be a list
of the first `woman-cache-level' elements from the following list:
\(topic path-index filename)."
  ;; This function used to check that each file in the directory was
  ;; not itself a directory, but this is very slow and should be
  ;; unnecessary.  So let us assume that `woman-file-regexp' will
  ;; filter out any directories, which probably should not be there
  ;; anyway, i.e. it is a user error!
  ;;
  ;; Don't sort files: we do that when merging, anyway.  -- dak
  (let (newlst (lst (directory-files dir nil woman-file-regexp t))
	       ;; Make an explicit regexp for stripping extension and
	       ;; compression extension: file-name-sans-extension is a
	       ;; far too costly function.  -- dak
	       (ext (format "\\(\\.[^.\\/]*\\)?\\(%s\\)?\\'"
			    woman-file-compression-regexp)))
    ;; Use a loop instead of mapcar in order to avoid the speed
    ;; penalty of binding function arguments.  -- dak
      (dolist (file lst newlst)
	(push
	 (cons
	  (if (string-match ext file)
	      (substring file 0 (match-beginning 0))
	    file)
	  (and (> woman-cache-level 1)
	       (cons
		path-index
		(and (> woman-cache-level 2)
		     (list file)))))
	 newlst))))

(defun woman-topic-all-completions-merge (alist)
  "Merge the alist ALIST so that the keys are unique.
Also make each path-info component into a list.
\(Note that this function changes the value of ALIST.)"
  ;; Replaces unreadably "optimized" O(n^2) implementation.
  ;; Instead we use sorting to merge stuff efficiently.  -- dak
  (let (newalist)
    ;; Sort list into reverse order
    (setq alist (sort alist (lambda(x y) (string< (car y) (car x)))))
    ;; merge duplicate keys.
    (if (> woman-cache-level 1)
	(dolist (elt alist)
	  (if (equal (car elt) (caar newalist))
	      (unless (member (cdr elt) (cdar newalist))
		(setcdr (car newalist) (cons (cdr elt)
					     (cdar newalist))))
	    (setcdr elt (list (cdr elt)))
	    (push elt newalist)))
      ;; woman-cache-level = 1 => elements are single-element lists ...
      (dolist (elt alist)
	(unless (equal (car elt) (caar newalist))
	  (push elt newalist))))
    newalist))

(defun woman-file-name-all-completions (topic)
  "Return an alist of the files in all man directories that match TOPIC."
  ;; Support 3 levels of caching: each element of
  ;; woman-topic-all-completions is a list of one of the forms:
  ;;   (topic)
  ;;   (topic (path-index) (path-index) ... )
  ;;   (topic (path-index filename) (path-index filename) ... )
  ;; where there are no duplicates in the value lists.
  ;; Topic must match first `word' of filename, so ...
  (let ((topic-regexp
	 (concat
	  "\\`" (regexp-quote topic)	; first `word'
	  "\\(\\..+\\)*"		; optional subsequent `words'
	  woman-file-regexp))		; extension
	(topics woman-topic-all-completions)
	(path woman-expanded-directory-path)
	dir files)
    (if (cdr (car topics))
	;; Use cached path-info to locate files for each topic:
	(let ((path-info (cdr (assoc topic topics)))
	      filename)
	  (dolist (elt path-info)
	    (setq dir (nth (car elt) path)
		  filename (car (cdr elt))
		  files (nconc files
			       ;; Find the actual file name:
			       (if filename
				   (list (concat dir "/" filename))
				 (directory-files dir t topic-regexp)
				 )))))
      ;; Search path for the files for each topic:
      (while path
	(setq dir (car path)
	      path (cdr path))
	(if (woman-not-member dir path)	; use each directory only once!
	    (setq files (nconc files
			       (directory-files dir t topic-regexp))))))
    (mapcar 'list files)))


;;; dired support

(defun woman-dired-define-key (key)
  "Bind the argument KEY to the command `woman-dired-find-file'."
  (define-key dired-mode-map key 'woman-dired-find-file))

(defsubst woman-dired-define-key-maybe (key)
  "If KEY is undefined in Dired, bind it to command `woman-dired-find-file'."
  (if (or (eq (lookup-key dired-mode-map key) 'undefined)
	  (null (lookup-key dired-mode-map key)))
      (woman-dired-define-key key)))

(defun woman-dired-define-keys ()
  "Define dired keys to run WoMan according to `woman-dired-keys'."
  (if woman-dired-keys
      (if (listp woman-dired-keys)
	  (mapc 'woman-dired-define-key woman-dired-keys)
	(woman-dired-define-key-maybe "w")
	(woman-dired-define-key-maybe "W")))
  (define-key-after (lookup-key dired-mode-map [menu-bar immediate])
    [woman] '("Read Man Page (WoMan)" . woman-dired-find-file) 'view))

(if (featurep 'dired)
    (woman-dired-define-keys)
  (add-hook 'dired-mode-hook 'woman-dired-define-keys))

;;;###autoload
(defun woman-dired-find-file ()
  "In dired, run the WoMan man-page browser on this file."
  (interactive)
  ;; dired-get-filename is defined in dired.el
  (woman-find-file (dired-get-filename)))


;;; tar-mode support

(defvar global-font-lock-mode)  ; defined in font-core.el

(defun woman-tar-extract-file ()
  "In tar mode, run the WoMan man-page browser on this file."
  (interactive)
  (or (eq major-mode 'tar-mode)
      (error "`woman-tar-extract-file' can be used only in `tar-mode'"))
  (buffer-disable-undo)
  (let (global-font-lock-mode)
    (funcall (symbol-function 'tar-extract)) ; defined in tar-mode
    (let ((WoMan-current-file buffer-file-name)) ; used for message logging
      (rename-buffer
       (woman-make-bufname (file-name-nondirectory buffer-file-name)))
      (woman-process-buffer)
      (goto-char (point-min)))))

;; There is currently no `tar-mode-hook' so use ...
(eval-after-load "tar-mode"
  '(progn
    (define-key tar-mode-map "w" 'woman-tar-extract-file)
    (define-key-after (lookup-key tar-mode-map [menu-bar immediate])
      [woman] '("Read Man Page (WoMan)" . woman-tar-extract-file) 'view)))


(defvar woman-last-file-name nil
  "The full pathname of the last file formatted by WoMan.")

(defun woman-reformat-last-file ()
  "Reformat last file, e.g. after changing fill column."
  (interactive)
  (if woman-last-file-name
      (woman-find-file woman-last-file-name t)
    (call-interactively 'woman-find-file)))

;;;###autoload
(defun woman-find-file (file-name &optional reformat)
  "Find, decode and browse a specific UN*X man-page source file FILE-NAME.
Use existing buffer if possible; reformat only if prefix arg given.
When called interactively, optional argument REFORMAT forces reformatting
of an existing WoMan buffer formatted earlier.
No external programs are used, except that `gunzip' will be used to
decompress the file if appropriate.  See the documentation for the
`woman' command for further details."
  (interactive "fBrowse UN*X manual file: \nP")
  (setq woman-last-file-name
	(setq file-name (expand-file-name file-name)))	; to canonicalize
  (let ((alist-tail woman-buffer-alist) exists)
    (setq woman-buffer-number 0)
    (while (and alist-tail (not (string= file-name (car (car alist-tail)))))
      (setq alist-tail (cdr alist-tail)
	    woman-buffer-number (1+ woman-buffer-number)))
    (or (and (setq exists
		   (and alist-tail (WoMan-find-buffer))) ; buffer exists
	     (not reformat))
	;; Format new buffer or reformat current buffer:
	(let* ((bufname (file-name-nondirectory file-name))
	       (case-fold-search t)
	       (compressed
		(not (not (string-match woman-file-compression-regexp bufname)))))
	  (if compressed
	      (setq bufname (file-name-sans-extension bufname)))
	  (setq bufname (if exists
			    (buffer-name)
			  (woman-make-bufname bufname)))
	  (woman-really-find-file file-name compressed bufname)
	  (or exists
	      (setq woman-buffer-alist
		    (cons (cons file-name bufname) woman-buffer-alist)
		    woman-buffer-number 0)))))
  (Man-build-section-alist)
  (Man-build-references-alist)
  (goto-char (point-min)))

(defun woman-make-bufname (bufname)
  "Create an unambiguous buffer name from BUFNAME."
  ;; See Bug#5038.  Any compression extension has already been removed.
  ;; Go from eg "host.conf.5" to "5 host.conf".
  (let ((dot (string-match "\\.[^.]*\\'" bufname)))
    (if dot (setq bufname (concat
			   (substring bufname (1+ dot)) " "
			   (substring bufname 0 dot))))
    (generate-new-buffer-name		; ensure uniqueness
     (concat "*WoMan " bufname "*"))))

(defvar woman-frame nil
  "Dedicated frame used for displaying WoMan windows.")

(defun woman-really-find-file (filename compressed bufname)
  "Find, decompress, and decode a UN*X man page FILENAME.
If COMPRESSED is non-nil, turn on auto-compression mode to decompress
the file if necessary.  Set buffer name BUFNAME and major mode.
Do not call directly!"
  (let ((WoMan-current-file filename))	; used for message logging
    (if woman-use-own-frame
	(select-frame
	 (or (and (frame-live-p woman-frame) woman-frame)
	     (setq woman-frame (make-frame)))))
    (set-buffer (get-buffer-create bufname))
    (condition-case nil
        (switch-to-buffer (current-buffer))
      (error (pop-to-buffer (current-buffer))))
    (buffer-disable-undo)
    (setq buffer-read-only nil)
    (erase-buffer)			; NEEDED for reformat
    (woman-insert-file-contents filename compressed)
    ;; Set buffer's default directory to that of the file.
    (setq default-directory (file-name-directory filename))
    (set (make-local-variable 'backup-inhibited) t)
    (set-visited-file-name "")
    (woman-process-buffer)))

(defun woman-process-buffer ()
  "The second half of `woman-really-find-file'!"
  (interactive)
  ;; Check (crudely) that this really is likely to be in UN*X
  ;; man-page source format, assuming we are at point-min:
  (goto-char (point-min))
  (if (re-search-forward "^[.']" 1000 t)
      (woman-decode-buffer)
    (message
     "File appears to be pre-formatted -- using source file may be better.")
    (woman-man-buffer))
  (woman-mode))

(defun woman-man-buffer ()
  "Post-process an nroff-preformatted man buffer."
  ;; Kill all leading whitespace:
  (if (looking-at "\\s-+") (woman-delete-match 0))
  ;; Delete all page footer/header pairs:
  (re-search-forward ".*")		; match header
  ;; Footer conventionally has page number at right, so ...
  (let ((regex (concat
		"^.*[0-9]\n\\s-*"	; footer and following blank lines
		(regexp-quote (match-string 0))	; header
		"\\s-*\n")))		; following blank lines
    (while (re-search-forward regex nil 1) ; finish at eob
      (woman-delete-match 0)))
  ;; Delete last text line (footer) and all following blank lines:
  (re-search-backward "\\S-")
  (beginning-of-line)
  (if (looking-at ".*[0-9]$")
      (delete-region (point) (point-max)))

  ;; Squeeze multiple blank lines:
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\n\\([ \t]*\n\\)+" nil t)
    (replace-match "\n" t t))

  ;; CJK characters are underlined by double-sized "__".
  ;; (Code lifted from man.el, with trivial changes.)
  (if (< (buffer-size) (position-bytes (point-max)))
      ;; Multibyte characters exist.
      (progn
	(goto-char (point-min))
	(while (search-forward "__\b\b" nil t)
	  (backward-delete-char 4)
	  (woman-set-face (point) (1+ (point)) 'woman-italic))
	(goto-char (point-min))
	(while (search-forward "\b\b__" nil t)
	  (backward-delete-char 4)
	  (woman-set-face (1- (point)) (point) 'woman-italic))))

  ;; Interpret overprinting to indicate bold face:
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\)\\(\\(+\\1\\)+\\)" nil t)
    (woman-delete-match 2)
    (woman-set-face (1- (point)) (point) 'woman-bold))

  ;; Interpret underlining to indicate italic face:
  ;; (Must be AFTER emboldening to interpret bold _ correctly!)
  (goto-char (point-min))
  (while (search-forward "_" nil t)
    (delete-char -2)
    (woman-set-face (point) (1+ (point)) 'woman-italic))

  ;; Leave any other uninterpreted ^H's in the buffer for now!  (They
  ;; might indicate composite special characters, which could be
  ;; interpreted if I knew what to expect.)

  ;; Optionally embolden section and subsection headings
  ;; (cf. `woman-imenu-generic-expression'):
  (cond
   (woman-bold-headings
    (goto-char (point-min))
    (forward-line)
    (while (re-search-forward "^\\(   \\)?\\([A-Z].*\\)" nil t)
      (woman-set-face (match-beginning 2) (match-end 2) 'woman-bold)))))

(defun woman-insert-file-contents (filename compressed)
  "Insert file FILENAME into the current buffer.
If COMPRESSED is t, or is non-nil and the filename implies compression,
then turn on auto-compression mode to decompress the file.
Leave point at end of new text.  Return length of inserted text."
  ;; Leaves point at end of inserted text in GNU Emacs 20.3, but at
  ;; start in 19.34!
  (save-excursion
    (let ((case-fold-search t))
      ;; Co-operate with auto-compression mode:
      (if (and compressed
	       (or (eq compressed t)
		   (string-match woman-file-compression-regexp filename))
	       ;; (not auto-compression-mode)
	       (not (rassq 'jka-compr-handler file-name-handler-alist)) )
	  ;; (error "Compressed file requires Auto File Decompression turned on")
	  (auto-compression-mode 1))
      (nth 1
	   (condition-case ()
	       (insert-file-contents filename nil)
	     (file-error
	      ;; Run find-file-not-found-hooks until one returns non-nil.
	      ;; (run-hook-with-args-until-success 'find-file-not-found-hooks)
	      (insert "\n***** File " filename " not found! *****\n\n")))))))


;;; Major mode (Man) interface:

(defvar woman-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map Man-mode-map)

    (define-key map "R" 'woman-reformat-last-file)
    (define-key map "w" 'woman)
    (define-key map "\en" 'WoMan-next-manpage)
    (define-key map "\ep" 'WoMan-previous-manpage)
    (define-key map [M-mouse-2] 'woman-follow-word)

    ;; We don't need to call `man' when we are in `woman-mode'.
    (define-key map [remap man] 'woman)
    (define-key map [remap man-follow] 'woman-follow)
    map)
  "Keymap for woman mode.")

(defun woman-follow (topic)
  "Get a Un*x manual page of the item under point and put it in a buffer."
  (interactive (list (Man-default-man-entry)))
  (if (or (not topic)
	  (string= topic ""))
      (error "No item under point")
    (woman (if (string-match Man-reference-regexp topic)
	       (substring topic 0 (match-end 1))
	     topic))))

(defun woman-follow-word (event)
  "Run WoMan with word under mouse as topic.
Argument EVENT is the invoking mouse event."
  (interactive "e")			; mouse event
  (goto-char (posn-point (event-start event)))
  (woman (or (current-word t) "")))

;; WoMan menu bar and pop-up menu:
(easy-menu-define
  woman-menu				; (SYMBOL MAPS DOC MENU)
  ;; That comment was moved after the symbol `woman-menu' to make
  ;; find-function-search-for-symbol work. -- rost
 woman-mode-map
 "WoMan Menu"
 `("WoMan"
   ["WoMan..." woman t]			; [NAME CALLBACK ENABLE]
   "--"
   ["Next Section" Man-next-section t]
   ["Previous Section" Man-previous-section t]
   ["Goto Section..." Man-goto-section t]
   ["Goto See-Also Section" Man-goto-see-also-section t]
   ["Follow Reference..." Man-follow-manual-reference t]
   "--"
   ["Previous WoMan Buffer" WoMan-previous-manpage t]
   ["Next WoMan Buffer" WoMan-next-manpage t]
   ["Bury WoMan Buffer" Man-quit t]
   ["Kill WoMan Buffer" Man-kill t]
   "--"
   ;; ["Toggle Fill Frame Width" woman-toggle-fill-frame t]
   ["Use Full Frame Width" woman-toggle-fill-frame
    :active t :style toggle :selected woman-fill-frame]
   ["Reformat Last Man Page" woman-reformat-last-file t]
   ["Use Monochrome Main Faces" woman-monochrome-faces t]
   ["Use Default Main Faces" woman-default-faces t]
   ["Make Contents Menu" (woman-imenu t) (not woman-imenu-done)]
   "--"
   ["Describe (Wo)Man Mode" describe-mode t]
   ["Mini Help" woman-mini-help t]
   ,@(if (fboundp 'customize-group)
	 '(["Customize..." (customize-group 'woman) t]))
   ["Show Version" (message "WoMan %s" woman-version) t]
   "--"
   ("Advanced"
    ["View Source" (view-file woman-last-file-name) woman-last-file-name]
    ["Show Log" (switch-to-buffer-other-window "*WoMan-Log*" t) t]
    ["Extended Font" woman-toggle-use-extended-font
     :included woman-font-support
     :active t :style toggle :selected woman-use-extended-font]
    ["Symbol Font" woman-toggle-use-symbol-font
     :included woman-font-support
     :active t :style toggle :selected woman-use-symbol-font]
    ["Font Map" woman-display-extended-fonts
     :included woman-font-support
     :active woman-use-symbol-font]
   "--"
   "Emulation"
   ["nroff" (woman-reset-emulation 'nroff)
    :active t :style radio :selected (eq woman-emulation 'nroff)]
   ["troff" (woman-reset-emulation 'troff)
    :active t :style radio :selected (eq woman-emulation 'troff)]
   )
   ))

(defun woman-toggle-use-extended-font ()
  "Toggle `woman-use-extended-font' and reformat, for menu use."
  (interactive)
  (setq woman-use-extended-font (not woman-use-extended-font))
  (woman-reformat-last-file))

(defun woman-toggle-use-symbol-font ()
  "Toggle `woman-use-symbol-font' and reformat, for menu use."
  (interactive)
  (setq woman-use-symbol-font (not woman-use-symbol-font))
  (woman-reformat-last-file))

(defun woman-reset-emulation (value)
  "Reset `woman-emulation' to VALUE and reformat, for menu use."
  (interactive)
  (setq woman-emulation value)
  (woman-reformat-last-file))

(defvar bookmark-make-record-function)
(put 'woman-mode 'mode-class 'special)

(defun woman-mode ()
  "Turn on (most of) Man mode to browse a buffer formatted by WoMan.
WoMan is an ELisp emulation of much of the functionality of the Emacs
`man' command running the standard UN*X man and ?roff programs.
WoMan author: F.J.Wright@Maths.QMW.ac.uk
WoMan version: see `woman-version'.
See `Man-mode' for additional details."
  (let ((Man-build-page-list (symbol-function 'Man-build-page-list))
	(Man-strip-page-headers (symbol-function 'Man-strip-page-headers))
	(Man-unindent (symbol-function 'Man-unindent))
	(Man-goto-page (symbol-function 'Man-goto-page)))
    ;; Prevent inappropriate operations:
    (fset 'Man-build-page-list 'ignore)
    (fset 'Man-strip-page-headers 'ignore)
    (fset 'Man-unindent 'ignore)
    (fset 'Man-goto-page 'ignore)
    (unwind-protect
	(delay-mode-hooks (Man-mode))
      ;; Restore the status quo:
      (fset 'Man-build-page-list Man-build-page-list)
      (fset 'Man-strip-page-headers Man-strip-page-headers)
      (fset 'Man-unindent Man-unindent)
      (fset 'Man-goto-page Man-goto-page)
      (setq tab-width woman-tab-width)))
  (setq major-mode 'woman-mode
	mode-name "WoMan")
  ;; Don't show page numbers like Man-mode does.  (Online documents do
  ;; not have pages)
  (kill-local-variable 'mode-line-buffer-identification)
  (use-local-map woman-mode-map)
  ;; Imenu support:
  (set (make-local-variable 'imenu-generic-expression)
       ;; `make-local-variable' in case imenu not yet loaded!
       woman-imenu-generic-expression)
  (set (make-local-variable 'imenu-space-replacement) " ")
  ;; Bookmark support.
  (set (make-local-variable 'bookmark-make-record-function)
       'woman-bookmark-make-record)
  ;; For reformat ...
  ;; necessary when reformatting a file in its old buffer:
  (setq imenu--last-menubar-index-alist nil)
  ;; necessary to avoid re-installing the same imenu:
  (setq woman-imenu-done nil)
  (if woman-imenu (woman-imenu))
  (let ((inhibit-read-only t))
    (Man-highlight-references 'WoMan-xref-man-page))
  (set-buffer-modified-p nil)
  (run-mode-hooks 'woman-mode-hook))

(defun woman-imenu (&optional redraw)
  "Add a \"Contents\" menu to the menubar.
Optional argument REDRAW, if non-nil, forces mode line to be updated."
  (interactive)
  (if woman-imenu-done
      ;; This is PRIMARILY to avoid a bug in imenu-add-to-menubar that
      ;; causes it to corrupt the menu bar if it is run more than once
      ;; in the same buffer.
      ()
    (setq woman-imenu-done t)
    (imenu-add-to-menubar woman-imenu-title)
    (if redraw (force-mode-line-update))))

(defun woman-toggle-fill-frame ()
  "Toggle formatting to fill (most of) the width of the current frame."
  (interactive)
  (setq woman-fill-frame (not woman-fill-frame))
  (message "Woman fill column set to %s."
	   (if woman-fill-frame "frame width" woman-fill-column)))

(defun woman-mini-help ()
  "Display WoMan commands and user options in an `apropos' buffer."
  ;; Based on apropos-command in apropos.el
  (interactive)
  (require 'apropos)
  (let ((message
	 (let ((standard-output (get-buffer-create "*Apropos*")))
	   (help-print-return-message 'identity))))
    (setq apropos-accumulator
	  (apropos-internal "woman"
			    (lambda (symbol)
			      (and
			       (or (commandp symbol)
				   (user-variable-p symbol))
			       (not (get symbol 'apropos-inhibit))))))
    ;; Find documentation strings:
    (let ((p apropos-accumulator)
	  doc symbol)
      (while p
	(setcar p (list			; must have 3 elements:
		   (setq symbol (car p)) ; 1. name
		   (if (functionp symbol) ; 2. command doc
		       (if (setq doc (documentation symbol t))
			   (substring doc 0 (string-match "\n" doc))
			 "(not documented)"))
		   (if (user-variable-p symbol)	; 3. variable doc
		       (if (setq doc (documentation-property
				      symbol 'variable-documentation t))
			   (substring doc 0 (string-match "\n" doc))))))
	(setq p (cdr p))))
    ;; Output the result:
    (and (apropos-print t nil)
	 message
	 (message "%s" message))))


(defun WoMan-getpage-in-background (topic)
  "Use TOPIC to start WoMan from `Man-follow-manual-reference'."
  ;; topic is a string, generally of the form "section topic"
  (let ((s (string-match " " topic)))
    (if s (setq topic (substring topic (1+ s))))
    (woman topic)))

(defvar WoMan-Man-start-time nil
  "Used to record formatting time used by the `man' command.")

;; Both advices are disabled because "a file in Emacs should not put
;; advice on a function in Emacs" (see Info node "(elisp)Advising
;; Functions").  Counting the formatting time is useful for
;; developing, but less applicable for daily use.  The advice for
;; `Man-getpage-in-background' can be discarded, because the
;; key-binding in `woman-mode-map' has been remapped to call `woman'
;; but `man'.  Michael Albinus <michael.albinus@gmx.de>

;; (defadvice Man-getpage-in-background
;;   (around Man-getpage-in-background-advice (topic) activate)
;;   "Use WoMan unless invoked outside a WoMan buffer or invoked explicitly.
;; Otherwise use Man and record start of formatting time."
;;   (if (and (eq major-mode 'woman-mode)
;; 	   (not (eq (caar command-history) 'man)))
;;       (WoMan-getpage-in-background topic)
;;     ;; Initiates man processing
;;     (setq WoMan-Man-start-time (current-time))
;;     ad-do-it))

;; (defadvice Man-bgproc-sentinel
;;   (after Man-bgproc-sentinel-advice activate)
;;   ;; Terminates man processing
;;   "Report formatting time."
;;   (let* ((time (current-time))
;; 	 (time (+ (* (- (car time) (car WoMan-Man-start-time)) 65536)
;; 		  (- (cadr time) (cadr WoMan-Man-start-time)))))
;;     (message "Man formatting done in %d seconds" time)))


;;; Buffer handling:

(defun WoMan-previous-manpage ()
  "Find the previous WoMan buffer."
  ;; Assumes currently in a WoMan buffer!
  (interactive)
  (WoMan-find-buffer)			; find current existing buffer
  (if (null (cdr woman-buffer-alist))
      (error "No previous WoMan buffer"))
  (if (>= (setq woman-buffer-number (1+ woman-buffer-number))
	 (length woman-buffer-alist))
      (setq woman-buffer-number 0))
  (if (WoMan-find-buffer)
      ()
    (if (< (setq woman-buffer-number (1- woman-buffer-number)) 0)
	(setq woman-buffer-number (1- (length woman-buffer-alist))))
    (WoMan-previous-manpage)))

(defun WoMan-next-manpage ()
  "Find the next WoMan buffer."
  ;; Assumes currently in a WoMan buffer!
  (interactive)
  (WoMan-find-buffer)			; find current existing buffer
  (if (null (cdr woman-buffer-alist))
      (error "No next WoMan buffer"))
  (if (< (setq woman-buffer-number (1- woman-buffer-number)) 0)
      (setq woman-buffer-number (1- (length woman-buffer-alist))))
  (if (WoMan-find-buffer)
      ()
    (WoMan-next-manpage)))

(defun WoMan-find-buffer ()
  "Switch to buffer corresponding to `woman-buffer-number' and return it.
If such a buffer does not exist then remove its association from the
alist in `woman-buffer-alist' and return nil."
  (if (zerop woman-buffer-number)
      (let ((buffer (get-buffer (cdr (car woman-buffer-alist)))))
	(if buffer
	    (switch-to-buffer buffer)
	  ;; Delete alist element:
	  (setq woman-buffer-alist (cdr woman-buffer-alist))
	  nil))
    (let* ((prev-ptr (nthcdr (1- woman-buffer-number) woman-buffer-alist))
	   (buffer (get-buffer (cdr (car (cdr prev-ptr))))))
      (if buffer
	  (switch-to-buffer buffer)
	;; Delete alist element:
	(setcdr prev-ptr (cdr (cdr prev-ptr)))
	(if (>= woman-buffer-number (length woman-buffer-alist))
	    (setq woman-buffer-number 0))
	nil))))


;;; Syntax and display tables:

(defconst woman-escaped-escape-char ?
  ;; An arbitrary unused control character
  "Internal character representation of escaped escape characters.")
(defconst woman-escaped-escape-string
  (char-to-string woman-escaped-escape-char)
  "Internal string representation of escaped escape characters.")

(defconst woman-unpadded-space-char ?
  ;; An arbitrary unused control character
  "Internal character representation of unpadded space characters.")
(defconst woman-unpadded-space-string
  (char-to-string woman-unpadded-space-char)
  "Internal string representation of unpadded space characters.")

(defvar woman-syntax-table
  (let ((st (make-syntax-table)))
    ;; The following internal chars must NOT have whitespace syntax:
    (modify-syntax-entry woman-unpadded-space-char "." st)
    (modify-syntax-entry woman-escaped-escape-char "." st)
    st)
  "Syntax table to support special characters used internally by WoMan.")

(defun woman-set-buffer-display-table ()
  "Set up a display table for a WoMan buffer.
This display table is used for displaying internal special characters, but
does not interfere with any existing display table, e.g. for displaying
European characters."
  (setq buffer-display-table
	;; The following test appears to be necessary on some
	;; non-Windows platforms, e.g. Solaris 2.6 when running on a
	;; tty.  Thanks to T. V. Raman <raman@Adobe.COM>.
	;; The MS-DOS terminal also sets standard-display-table to
	;; a non-nil value.
	(if standard-display-table	; default is nil !!!
	    (copy-sequence standard-display-table)
	  (make-display-table)))
  ;; Display the following internal chars correctly:
  (aset buffer-display-table woman-unpadded-space-char [?\ ])
  (aset buffer-display-table woman-escaped-escape-char [?\\]))


;;; The main decoding driver:

(defvar font-lock-mode)			; for the compiler

(defun woman-decode-buffer ()
  "Decode a buffer in UN*X man-page source format.
No external programs are used."
  (interactive)				; mainly for testing
  (WoMan-log-begin)
  (run-hooks 'woman-pre-format-hook)
  (and (boundp 'font-lock-mode) font-lock-mode (font-lock-mode -1))
  ;; (fundamental-mode)
  (let ((start-time (current-time))
	time)
    (message "WoMan formatting buffer...")
;  (goto-char (point-min))
;  (cond
;   ((re-search-forward "^\\.[ \t]*TH" nil t) ; wrong format if not found?
;    (beginning-of-line)
;    (delete-region (point-min) (point))) ; potentially dangerous!
;   (t (message "WARNING: .TH request not found -- not man-page format?")))
    (woman-decode-region (point-min) (point-max))
    (setq time (float-time (time-since start-time)))
    (message "WoMan formatting buffer...done in %g seconds" time)
    (WoMan-log-end time))
  (run-hooks 'woman-post-format-hook))

(defvar woman-string-alist		; rebound in woman-decode-region
  '(("S" . "") ("R" . "(Reg.)") ("Tm" . "(TM)")
    ("lq" . "\"") ("rq" . "\"")
    ("''" . "\"")			; needed for gcc.1
    (".T" . "")				; output device from -T option?
    )
  "Alist of strings predefined in the -man macro package `tmac.an'.")

(defvar woman-negative-vertical-space nil ; rebound in woman-decode-region
  "Set to t if .sp N with N < 0 encountered.")

(defun woman-pre-process-region (from to)
  "Pre-process escapes and comments in the region of text between FROM and TO.
To be called on original buffer and any .so insertions."
  ;; Hide escaped escapes \\ and printable escapes \e very early
  ;; (to be re-instated as just \ very late!):
  (goto-char from)
  ;; .eo turns off escape character processing
  (while (re-search-forward "\\(\\\\[\\e]\\)\\|^\\.eo" to t) ; \\
    (if (match-beginning 1)
	(replace-match woman-escaped-escape-string t t)
      (woman-delete-whole-line)
      ;; .ec turns on escape character processing (and sets the
      ;; escape character to its argument, if any, which I'm ignoring
      ;; for now!)
      (while (and (re-search-forward "\\(\\\\\\)\\|^\\.ec" to t) ; \
		  (match-beginning 1))
	(replace-match woman-escaped-escape-string t t))
      ;; ***** Need test for .ec arg and warning here! *****
      (woman-delete-whole-line)))

  ;; Delete comments .\"<anything>, \"<anything> and null requests.
  ;; (However, should null . requests cause a break?)
  (goto-char from)
  (while (re-search-forward "^[.'][ \t]*\\(\\\\\".*\\)?\n\\|\\\\\".*" to t)
    (woman-delete-match 0)))

(defun woman-non-underline-faces ()
  "Prepare non-underlined versions of underlined faces."
  (let ((face-list (face-list)))
    (dolist (face face-list)
      (let ((face-name (symbol-name face)))
	(if (and (string-match "\\`woman-" face-name)
		 (face-underline-p face))
	    (let ((face-no-ul (intern (concat face-name "-no-ul"))))
	      (copy-face face face-no-ul)
	      (set-face-underline-p face-no-ul nil)))))))

;; Preprocessors
;; =============

;; This information is based on documentation for the man command by
;; Graeme W. Wilford <G.Wilford@ee.surrey.ac.uk>

;; First, the environment variable $MANROFFSEQ is interrogated, and if
;; not set then the initial line of the nroff file is parsed for a
;; preprocessor string. To contain a valid preprocessor string, the
;; first line must resemble
;;
;; '\" <string>
;;
;; where string can be any combination of the following letters that
;; specify the sequence of preprocessors to run before nroff or
;; troff/groff.  Not all installations will have a full set of
;; preprocessors.  Some of the preprocessors and the letters used to
;; designate them are: eqn (e), grap (g), pic (p), tbl (t), vgrind
;; (v), refer (r).  This option overrides the $MANROFFSEQ environment
;; variable.  zsoelim is always run as the very first preprocessor.

(defvar woman-emulate-tbl nil
  "True if WoMan should emulate the tbl preprocessor.
This applies to text between .TE and .TS directives.
Currently set only from '\" t in the first line of the source file.")

(defun woman-decode-region (from _to)
  "Decode the region between FROM and TO in UN*X man-page source format."
  ;; Suitable for use in format-alist.
  ;; But this requires care to control major mode implied font locking.
  ;; Must return the new end of file.  See format.el for details.
  ;; NB: The `to' argument is bogus: it is not currently used, and if
  ;; it were it would need to be a marker rather than a position!
  ;; First force the correct environment:
  (let ((case-fold-search nil)		; This is necessary!
	(woman-string-alist woman-string-alist)
	(woman-fill-column woman-fill-column)
	woman-negative-vertical-space)
    (setq woman-left-margin woman-default-indent
	  woman-prevailing-indent woman-default-indent
	  woman-interparagraph-distance 1
	  woman-leave-blank-lines nil
	  woman-RS-left-margin nil
	  woman-RS-prevailing-indent nil
	  woman-adjust woman-adjust-both
	  woman-justify (nth woman-adjust woman-justify-list)
	  woman-nofill nil)

    (setq woman-if-conditions-true
	  (cons (string-to-char (symbol-name woman-emulation)) '(?e ?o)))

    ;; Prepare non-underlined versions of underlined faces:
    (woman-non-underline-faces)
    ;; Set font of `woman-symbol' face to `woman-symbol-font' if
    ;; `woman-symbol-font' is well defined.
    (and woman-use-symbol-font
	 (stringp woman-symbol-font)
	 (set-face-font 'woman-symbol woman-symbol-font
			(and (frame-live-p woman-frame) woman-frame)))

    ;; Set syntax and display tables:
    (set-syntax-table woman-syntax-table)
    (woman-set-buffer-display-table)

    ;; Based loosely on a suggestion by Theodore Jump:
    (if (or woman-fill-frame
	    (not (and (integerp woman-fill-column) (> woman-fill-column 0))))
	(setq woman-fill-column (- (window-width) woman-default-indent)))

    ;; Check for preprocessor requests:
    (goto-char from)
    (if (looking-at "'\\\\\"[ \t]*\\([a-z]+\\)")
	(let ((letters (append (match-string 1) nil)))
	  (if (memq ?t letters)
	      (setq woman-emulate-tbl t
		    letters (delete ?t letters)))
	  (if letters
	      (WoMan-warn "Unhandled preprocessor request letters %s"
			  (concat letters)))
	  (woman-delete-line 1)))

    (woman-pre-process-region from nil)
    ;; Process ignore requests, macro definitions,
    ;; conditionals and switch source requests:
    (woman0-roff-buffer from)

    ;; Check for macro sets that woman cannot handle.  We can only
    ;; because do this after processing source-switch directives.
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (unless (and (re-search-forward "^\\.SH[ \n]" (point-max) t)
		   (progn (goto-char (point-min))
			  (re-search-forward "^\\.TH[ \n]" (point-max) t))
		   (progn (goto-char (point-min))
			  (not (re-search-forward "^\\.\\([pnil]p\\|sh\\)[ \n]"
						  (point-max) t))))
	(error "WoMan can only format man pages written with the usual `-man' macros")))

    ;; Process \k escapes BEFORE changing tab width (?):
    (goto-char from)
    (woman-mark-horizonal-position)

    ;; Set buffer-local variables:
    (setq fill-column woman-fill-column
	  tab-width woman-tab-width)

    ;; Hide unpaddable and digit-width spaces \(space) and \0:
    (goto-char from)
    (while (re-search-forward "\\\\[ 0]" nil t)
      (replace-match woman-unpadded-space-string t t))

    ;; Discard optional hyphen \%; concealed newlines \<newline>;
    ;; point-size change function \sN,\s+N, \s-N:
    (goto-char from)
    (while (re-search-forward "\\\\\\([%\n]\\|s[-+]?[0-9]+\\)" nil t)
      (woman-delete-match 0))

    ;; BEWARE: THIS SHOULD PROBABLY ALL BE DONE MUCH LATER!!!!!
    ;; Process trivial escapes \-, \`, \.
    ;; (\' must be done after tab processing!):
    (goto-char from)
    (while (re-search-forward "\\\\\\([-`.]\\)" nil t)
      (replace-match "\\1"))
    ;; NB: Must keep ALL zero-width characters \&, \|, and \^ until
    ;; ALL requests processed!

    ;; Process no-break requests and macros (including font-change macros):
    (goto-char from)
    (woman1-roff-buffer)

    ;; Process strings and special character escapes \(xx:
    ;; (Must do this BEFORE fontifying!)
    (goto-char from)
    (woman-strings)
    ;; Special chars moved after translation in
    ;; `woman2-process-escapes' (for pic.1):
;    (goto-char from)
;    (woman-special-characters)

    ;; Process standard font-change requests and escapes:
    (goto-char from)
    (woman-change-fonts)

    ;; 1/2 em vertical motion \d, \u and general local vertical motion
    ;; \v'+/-N' simulated using TeX ^ and _ symbols for now.
    (goto-char from)
    (let ((first t))			; assume no nesting!
      (while (re-search-forward "\\\\\\([du]\\|v'[^']*'\\)" nil t)
	(let* ((esc (match-string 1))
	       (repl (if (or (= (aref esc 0) ?u)
			     (and (>= (length esc) 2) (= (aref esc 2) ?-)))
			 "^" "_")))
	  (cond (first
		 (replace-match repl nil t)
		 (put-text-property (1- (point)) (point) 'face 'woman-addition)
		 (WoMan-warn
		  "Initial vertical motion escape \\%s simulated" esc)
		 (WoMan-log
		  "      by TeX `%s' in woman-addition-face!" repl))
		(t
		 (woman-delete-match 0)
		 (WoMan-warn
		  "Terminal vertical motion escape \\%s ignored!" esc)))
	  (setq first (not first)))))

    ;; Process formatting macros
    (goto-char from)
    (woman2-roff-buffer)

    ;; Go back and process negative vertical space if necessary:
    (if woman-negative-vertical-space
	(woman-negative-vertical-space from))

    (if woman-preserve-ascii
	;; Re-instate escaped escapes to just `\' and unpaddable
	;; spaces to just `space', without inheriting any text
	;; properties.  This is not necessary, UNLESS the buffer is to
	;; be saved as ASCII.
	(progn
	  (goto-char from)
	  (while (search-forward woman-escaped-escape-string nil t)
	    (delete-char -1) (insert ?\\))
	  (goto-char from)
	  (while (search-forward woman-unpadded-space-string nil t)
	    (delete-char -1) (insert ?\ ))))

    ;; Must return the new end of file if used in format-alist.
    (point-max)))

(defun woman-horizontal-escapes (to)
  "Process \\h'+/-N' local horizontal motion escapes upto TO.
Implements arbitrary forward and non-overlapping backward motion.
Preserves location of `point'."
  ;; Moved from `woman-decode-region' for version 0.50.
  ;; N may include width escape \w'...' (but may already be processed!
  (let ((from (point)))
    (while (re-search-forward
	    ;; Delimiter can be a special char escape sequence \(.. or
	    ;; a single normal char (usually '):
	    "\\\\h\\(\\\\(..\\|.\\)\\(|\\)?"
	    to t)
      (let ((from (match-beginning 0))
	    (delim (regexp-quote (match-string 1)))
	    (absolute (match-beginning 2)) ; absolute position?
	    (N (woman-parse-numeric-arg)) ; distance
	    to
	    msg)			; for warning
	(if (not (looking-at delim))
	    ;; Warn but leave escape in buffer unprocessed:
	    (WoMan-warn
	     "Local horizontal motion (%s) delimiter error!"
	     (buffer-substring from (1+ (point)))) ; point at end of arg
	  (setq to (match-end 0)
		;; For possible warning -- save before deleting:
		msg (buffer-substring from to))
	  (delete-region from to)
	  (if absolute			; make relative
	      (setq N (- N (current-column))))
	  (if (>= N 0)
	      ;; Move forward by inserting hard spaces:
	      (insert-char woman-unpadded-space-char N)
	    ;; Move backwards by deleting space,
	    ;; first backwards then forwards:
	    (while (and
		    (<= (setq N (1+ N)) 0)
		    (cond ((memq (preceding-char) '(?\  ?\t))
			   (delete-char -1) t)
			  ((memq (following-char) '(?\  ?\t))
			   (delete-char 1) t)
			  (t nil))))
	    (if (<= N 0)
		(WoMan-warn
		 "Negative horizontal motion (%s) would overwrite!" msg))))))
    (goto-char from)))



;; Process ignore requests (.ig), conditionals (.if etc.),
;; source-switch (.so), macro definitions (.de etc.) and macro
;; expansions.

(defvar woman0-if-to)			; marker bound in woman0-roff-buffer
(defvar woman0-macro-alist)		; bound in woman0-roff-buffer
(defvar woman0-search-regex)		; bound in woman0-roff-buffer
(defvar woman0-search-regex-start	; bound in woman0-roff-buffer
  "^[.'][ \t]*\\(ig\\|if\\|ie\\|el\\|so\\|rn\\|de\\|am")
(defconst woman0-search-regex-end "\\)\\([ \t]+\\|$\\)")
;; May need other terminal characters, e.g. \, but NOT \n!
;; Alternatively, force maximal match (Posix?)

(defvar woman0-rename-alist)		; bound in woman0-roff-buffer

(defun woman0-roff-buffer (from)
  "Process conditional-type requests and user-defined macros.
Start at FROM and re-scan new text as appropriate."
  (goto-char from)
  (let ((woman0-if-to (make-marker))
	woman-request woman0-macro-alist
	(woman0-search-regex-start woman0-search-regex-start)
	(woman0-search-regex
	 (concat woman0-search-regex-start woman0-search-regex-end))
	processed-first-hunk
	woman0-rename-alist)
    (set-marker-insertion-type woman0-if-to t)
    (while (re-search-forward woman0-search-regex nil t)
      (setq woman-request (match-string 1))

      ;; Process escape sequences prior to first request (Bug#7843).
      (unless processed-first-hunk
	(setq processed-first-hunk t)
	(let ((process-escapes-to-marker (point-marker)))
	  (set-marker-insertion-type process-escapes-to-marker t)
	  (save-match-data
	    (save-excursion
	      (goto-char from)
	      (woman2-process-escapes process-escapes-to-marker)))))

      (cond ((string= woman-request "ig") (woman0-ig))
	    ((string= woman-request "if") (woman0-if "if"))
	    ((string= woman-request "ie") (woman0-if "ie"))
	    ((string= woman-request "el") (woman0-el))
	    ((string= woman-request "so") (woman0-so))
	    ((string= woman-request "rn") (woman0-rn))
	    ((string= woman-request "de") (woman0-de))
	    ((string= woman-request "am") (woman0-de 'append))
	    (t                      (woman0-macro woman-request))))
    (set-marker woman0-if-to nil)
    (woman0-rename)
    ;; Should now re-run `woman0-roff-buffer' if any renaming was
    ;; done, but let's just hope this is not necessary for now!
    ))

(defun woman0-ig ()
  ".ig yy -- Discard input up to `.yy', which defaults to `..')."
  ;; The terminal request MUST begin with . (not ')!
  (looking-at "\\(\\S +\\)?")
  (beginning-of-line)
  (let ((yy (or (match-string 1) "."))
	(from (point)))
    (if (re-search-forward
	 (concat "^\\.[ \t]*" (regexp-quote yy) ".*\n") nil t)
	(delete-region from (point))
      (WoMan-warn
       "ig request ignored -- terminator `.%s' not found!" yy)
      (woman-delete-line 1))))

(defsubst woman0-process-escapes (from to)
  "Process escapes within an if/ie condition between FROM and TO."
  (woman-strings to)
  (goto-char from)			; necessary!
  ;; Strip font-change escapes:
  (while (re-search-forward "\\\\f\\(\\[[^]]+\\]\\|(..\\|.\\)" to t)
    (woman-delete-match 0))
  (goto-char from)			; necessary!
  (woman2-process-escapes to 'numeric))

;; request does not appear to be used dynamically by any callees.
(defun woman0-if (request)
  ".if/ie c anything -- Discard unless c evaluates to true.
Remember condition for use by a subsequent `.el'.
REQUEST is the invoking directive without the leading dot."
  ;; c evaluates to a one-character built-in condition name or
  ;; 'string1'string2' or a number > 0, prefix ! negates.
  ;; \{ ... \} for multi-line use.
  ;; Leaves point at start of new text.
  (woman-delete-match 0)
  ;; (delete-horizontal-space)
  ;; Process escapes in condition:
  (let ((from (point)) negated n (c 0))
    (set-marker woman0-if-to
		(save-excursion (skip-syntax-forward "^ ") (point)))
    ;; Process condition:
    (if (setq negated (= (following-char) ?!)) (delete-char 1))
    (cond
     ;; ((looking-at "[no]") (setq c t))     ; accept n(roff) and o(dd page)
     ;; ((looking-at "[te]") (setq c nil))   ; reject t(roff) and e(ven page)
     ((looking-at "[ntoe]")
      (setq c (memq (following-char) woman-if-conditions-true)))
     ;; Unrecognized letter so reject:
     ((looking-at "[A-Za-z]") (setq c nil)
      (WoMan-warn "%s %s -- unrecognized condition name rejected!"
		  request (match-string 0)))
     ;; Accept strings if identical:
     ((save-restriction
	(narrow-to-region from woman0-if-to)
	;; String delimiter can be any non-numeric character,
	;; including a special character escape:
	(looking-at "\\(\\\\(..\\|[^0-9]\\)\\(.*\\)\\1\\(.*\\)\\1\\'"))
      (let ((end1 (copy-marker (match-end 2) t))) ; End of first string.
	;; Delete 2nd and 3rd delimiters to avoid processing them:
	(delete-region (match-end 3) woman0-if-to)
	(delete-region (match-end 2) (match-beginning 3))
	(goto-char (match-end 1))
	(woman0-process-escapes (point) woman0-if-to)
	(setq c (string= (buffer-substring (point) end1)
			 (buffer-substring end1 woman0-if-to)))
	(set-marker end1 nil)
	(goto-char from)))
     ;; Accept numeric value if > 0:
     ((numberp (setq n (progn
			 (woman0-process-escapes from woman0-if-to)
			 (woman-parse-numeric-arg))))
      (setq c (> n 0))
      (goto-char from)))
    (if (eq c 0)
	(woman-if-ignore woman0-if-to request) ; ERROR!
      (woman-if-body request woman0-if-to (eq c negated)))))

;; request is not used dynamically by any callees.
(defun woman-if-body (request to delete) ; should be reversed as `accept'?
  "Process if-body, including \\{ ... \\}.
REQUEST is the invoking directive without the leading dot.
If TO is non-nil then delete the if-body.
If DELETE is non-nil then delete from point."
  ;; Assume concealed newlines already processed.
  (let ((from (point)))
    (if to (delete-region (point) to))
    (delete-horizontal-space)
    (cond (;;(looking-at "[^{\n]*\\\\{\\s *") ; multi-line
	   ;; allow escaped newlines:
	   (looking-at "[^{\n]*\\(\\\\\n\\)*\\\\{\\s *\\(\\\\\n\\)*") ; multi-line
	   ;; including preceding .if(s) and following newline
	   (let ((from (point)))
	     (woman-delete-match 0)
	     ;; Allow for nested \{ ... \} -- BUT BEWARE that this
	     ;; algorithm only supports one level of nesting!
	     (while
		 (and (re-search-forward
		       ;; "\\(\\\\{\\)\\|\\(\n[.']\\)?[ \t]*\\\\}[ \t]*"
		       ;; Interpret bogus `el \}' as `el \{',
		       ;; especially for Tcl/Tk man pages:
		       "\\(\\\\{\\|el[ \t]*\\\\}\\)\\|\\(\n[.']\\)?[ \t]*\\\\}[ \t]*")
		      (match-beginning 1))
	       (re-search-forward "\\\\}"))
	     (delete-region (if delete from (match-beginning 0)) (point))
	     (if (looking-at "^$") (delete-char 1))
	     ))
	  (delete (woman-delete-line 1))) ; single-line
    ;; Process matching .el anything:
    (cond ((string= request "ie")
	   ;; Discard unless previous .ie c `evaluated to false'.
	   ;; IIUC, an .ie must be followed by an .el.
	   ;; (An if with no else uses .if rather than .ie.)
	   ;; TODO warn if no .el found?
	   ;; The .el should come immediately after the .ie (modulo
	   ;; comments etc), but this searches to eob.
	   (cond ((re-search-forward "^[.'][ \t]*el[ \t]*" nil t)
		  (woman-delete-match 0)
		  (woman-if-body "el" nil (not delete)))))
;;; FIXME neither the comment nor the code here make sense to me.
;;; This branch was executed for an else (any else, AFAICS).
;;; At this point, the else in question has already been processed above.
;;; The re-search will find the _next_ else, if there is one, and
;;; delete it.  If there is one, it belongs to another if block.  (Bug#9447)
;;; woman0-el does not need this bit either.
	  ;; Got here after processing a single-line `.ie' as a body
	  ;; clause to be discarded:
;;;	  ((string= request "el")
;;;	   (cond ((re-search-forward "^[.'][ \t]*el[ \t]*" nil t)
;;;		  (woman-delete-match 0)
;;;		  (woman-if-body "el" nil t)))))
          )
    (goto-char from)))

(defun woman0-el ()
  "Isolated .el request -- should not happen!"
  (WoMan-warn "el request without matching `ie' rejected!")
  (cond (woman-ignore
	 (woman-delete-match 0)
	 (delete-horizontal-space)
	 (woman-if-body "el" nil t))
	(t				; Ignore -- leave in buffer
	 ;; This does not work too well, but it's only for debugging!
	 (skip-chars-forward "^ \t")
	 (if (looking-at "[ \t]*\\{") (search-forward "\\}"))
	 (forward-line 1))))

;; request is not used dynamically by any callees.
(defun woman-if-ignore (to request)
  "Ignore but warn about an if request ending at TO, named REQUEST."
  (WoMan-warn-ignored request "ignored -- condition not handled!")
  (if woman-ignore
      (woman-if-body request to t)
    ;; Ignore -- leave in buffer
    ;; This does not work too well, but it's only for debugging!
    (skip-chars-forward "^ \t")
    (if (looking-at "[ \t]*\\{") (search-forward "\\}"))
    (forward-line 1)))

(defun woman0-so ()
  ".so filename -- Switch source file.  `.so' requests may be nested."
  ;; Leaves point at start of new text.
  ;; (skip-chars-forward " \t")
  (let* ((beg (point))
	 (end (progn (woman-forward-arg 'unquote) (point)))
	 (name (buffer-substring beg end))
	 (filename name))
    ;; If the specified file does not exist in this ...
    (or (file-exists-p filename)
	;; or the parent directory ...
 	(file-exists-p
 	 (setq filename (concat "../" name)))
	;; then use the WoMan search mechanism to find the filename ...
	(setq filename
	      (woman-file-name
	       (file-name-sans-extension
		(file-name-nondirectory name))))
	;; Cannot find the file, so ...
	(kill-buffer (current-buffer))
	(error "File `%s' not found" name))
    (beginning-of-line)
    (woman-delete-line 1)
    (let* ((from (point))
           (length (woman-insert-file-contents filename 0))
           (to (copy-marker (+ from length) t)))
      (woman-pre-process-region from to)
      (set-marker to nil)
      (goto-char from))))


;;; Process macro definitions:

(defun woman0-rn ()
  "Process .rn xx yy -- rename macro xx to yy."
  ;; For now, done backwards AFTER all macro expansion.
  ;; Should also allow requests and strings to be renamed!
  (if (eolp)				; ignore if no argument
      ()
    (let* ((beg (point))
	   (end (progn (woman-forward-arg 'unquote 'concat) (point)))
	   (old (buffer-substring beg end))
	   new)
      (if (eolp)			; ignore if no argument
	  ()
	(setq beg (point)
	      end (progn (woman-forward-arg 'unquote) (point))
	      new (buffer-substring beg end)
	      woman0-rename-alist (cons (cons new old) woman0-rename-alist)))))
  (woman-delete-whole-line))

(defun woman0-rename ()
  "Effect renaming required by .rn requests."
  ;; For now, do this backwards AFTER all macro expansion.
  (dolist (new woman0-rename-alist)
    (let ((old (cdr new))
          (new (car new)))
      (goto-char (point-min))
      (setq new (concat "^[.'][ \t]*" (regexp-quote new)))
      (setq old (concat "." old))
      (while (re-search-forward new nil t)
	(replace-match old nil t)))))

(defconst woman-unescape-regex
  (concat woman-escaped-escape-string
	  "\\(" woman-escaped-escape-string "\\)?"))

(defsubst woman-unescape (macro)
  "Replace escape sequences in the body of MACRO.
Replaces || by |, but | by \, where | denotes the internal escape."
  (let (start)
    (while (setq start (string-match woman-unescape-regex macro start))
      (setq macro
	    (if (match-beginning 1)
		(replace-match "" t t macro 1)
	      (replace-match "\\" t t macro))
	    start (1+ start)))
    macro))

(defun woman0-de (&optional append)
  "Process .de/am xx yy -- (re)define/append macro xx; end at `..'.
\(Should be up to call of yy, which defaults to `.')
Optional argument APPEND, if non-nil, means append macro."
  ;; Modeled on woman-strings.  BEWARE: Processing of .am is a hack!
  ;; Add support for .rm?
  ;; (skip-chars-forward " \t")
  (if (eolp)				; ignore if no argument
      ()
    (looking-at "[^ \t\n]+")		; macro name
    (let* ((macro (match-string 0)) from
	   (previous (assoc macro woman0-macro-alist)))
      (if (not previous)
	  (setq woman0-search-regex-start
		(concat woman0-search-regex-start "\\|" (regexp-quote macro))
		woman0-search-regex
		(concat woman0-search-regex-start woman0-search-regex-end)
		))
      ;; Macro body runs from start of next line to line
      ;; beginning with `..'."
      ;; The terminal request MUST begin with `.' (not ')!
      (forward-line)
      (setq from (point))
      (re-search-forward "^\\.[ \t]*\\.")
      (beginning-of-line)
      (let ((body (woman-unescape (buffer-substring from (point)))))
	(if (and append previous)
	    (setq previous (cdr previous)
		  body (concat body (cdr previous))
		  append (car previous)
		  ))
	(setq macro (cons macro (cons append body))))
      ;; This should be an update, but consing a new string
      ;; onto the front of the alist has the same effect:
      (setq woman0-macro-alist (cons macro woman0-macro-alist))
      (forward-line)
      (delete-region from (point))
      (backward-char)))			; return to end of .de/am line
  (beginning-of-line)			; delete .de/am line
  (woman-delete-line 1))

;; request may be used dynamically (woman-interpolate-macro calls
;; woman-forward-arg).
(defun woman0-macro (woman-request)
  "Process the macro call named WOMAN-REQUEST."
  ;; Leaves point at start of new text.
  (let ((macro (assoc woman-request woman0-macro-alist)))
    (if macro
	(woman-interpolate-macro (cdr macro))
      ;; SHOULD DELETE THE UNINTERPRETED REQUEST!!!!!
      ;; Output this message once only per call (cf. strings)?
      (WoMan-warn "Undefined macro %s not interpolated!" woman-request))))

(defun woman-interpolate-macro (macro)
  "Interpolate (.de) or append (.am) expansion of MACRO into the buffer."
  ;; Could make this more efficient by checking which arguments are
  ;; actually used in the expansion!
  (skip-chars-forward " \t")
  ;; Process arguments:
  (let ((argno 0) (append (car macro))
	argno-string formal-arg from actual-arg start)
    (setq macro (cdr macro))
    (while (not (eolp))
      ;; Get next actual arg:
      (setq argno (1+ argno))
      (setq argno-string (format "%d" argno))
      (setq formal-arg (concat "\\\\\\$" argno-string)) ; regexp
      (setq from (point))
      (woman-forward-arg 'unquote 'noskip)
      (setq actual-arg (buffer-substring from (point)))
      (skip-chars-forward " \t")  ; now skip following whitespace!
      ;; Replace formal arg with actual arg:
      (setq start nil)
      (while (setq start (string-match formal-arg macro start))
	(setq macro (replace-match actual-arg t t macro))))
    ;; Delete any remaining formal arguments:
    (setq start nil)
    (while
	(setq start (string-match "\\\\\\$." macro start))
      (setq macro (replace-match "" t t macro)))
    ;; Replace .$ number register with actual arg:
    ;; (Do this properly via register mechanism later!)
    (setq start nil)
    (while
	(setq start (string-match "\\\\n(\\.\\$" macro start)) ; regexp
      (setq macro (replace-match argno-string t t macro)))
    (if append
	(forward-char)
      (beginning-of-line)
      (woman-delete-line 1))
    (save-excursion			; leave point at start of new text
      (insert macro))))


;;; Process strings:

(defun woman-match-name ()
  "Match and move over name of form: x, (xx or [xxx...].
Applies to number registers, fonts, strings/macros/diversions, and
special characters."
  (cond ((= (following-char) ?\[ )
	 (forward-char)
	 (re-search-forward "[^]]+")
	 (forward-char))		; skip closing ]
	((= (following-char) ?\( )
	 (forward-char)
	 (re-search-forward ".."))
	(t (re-search-forward "."))))

(defun woman-strings (&optional to)
  "Process ?roff string requests and escape sequences up to buffer position TO.
Strings are defined/updated by `.ds xx string' requests and
interpolated by `\*x' and `\*(xx' escapes."
  ;; Add support for .as and .rm?
  (while
      ;; Find .ds requests and \* escapes:
      (re-search-forward "\\(^[.'][ \t]*ds\\)\\|\\\\\\*" to t)
    (cond ((match-beginning 1)		; .ds
	   (skip-chars-forward " \t")
	   (if (eolp)			; ignore if no argument
	       ()
	     (re-search-forward "[^ \t\n]+")
	     (let ((string (match-string 0)))
	       (skip-chars-forward " \t")
;		 (setq string
;		       (cons string
;			     ;; hack (?) for CGI.man!
;			     (cond ((looking-at "\"\"") "\"")
;				   ((looking-at ".*") (match-string 0)))
;			     ))
	       ;; Above hack causes trouble in arguments!
	       (looking-at ".*")
	       (setq string (cons string (match-string 0)))
	       ;; This should be an update, but consing a new string
	       ;; onto the front of the alist has the same effect:
	       (setq woman-string-alist (cons string woman-string-alist))
	       ))
	   (beginning-of-line)
	   (woman-delete-line 1))
	  (t				; \*
	   (let ((beg (match-beginning 0)))
	     (woman-match-name)
	     (let* ((stringname (match-string 0))
		   (string (assoc stringname woman-string-alist)))
	       (cond (string
		      (delete-region beg (point))
		      ;; Temporary hack in case string starts with a
		      ;; control character:
		      (if (bolp) (insert-before-markers "\\&"))
		      (insert-before-markers (cdr string)))
		     (t
		      (WoMan-warn "Undefined string %s not interpolated!"
			       stringname)
		      (cond (woman-ignore
			     ;; Output above message once only per call
			     (delete-region beg (point))
			     (setq woman-string-alist
				   (cons (cons stringname "")
					 woman-string-alist))))))))))))


;;; Process special character escapes \(xx:

(defconst woman-special-characters
  ;; To be built heuristically as required!
  ;; MUST insert all characters as strings for correct conversion to
  ;; multibyte representation!
  '(("em" "--"  "\276" . t)		; 3/4 Em dash
    ("bu" "*"   "\267" . t)		; bullet
    ("fm" "'")				; foot mark
    ("co" "(C)" "\251")			; copyright

    ("pl" "+"      "+" . t)		; math plus
    ("mi" "-"      "-" . t)		; math minus
    ("**" "*"      "*" . t)		; math star
    ("aa" "'"   "\242" . t)		; acute accent
    ("ul" "_")				; underrule

    ("*S" "Sigma"  "S" . t)		; Sigma

    (">=" ">="  "\263" . t)		; >=
    ("<=" "<="  "\243" . t)		; <=
    ("->" "->"  "\256" . t)		; right arrow
    ("<-" "<-"  "\254" . t)		; left arrow
    ("mu" " x " "\264" . t)		; multiply
    ("+-" "+/-" "\261" . t)		; plus-minus
    ("bv" "|")				; bold vertical

    ;; groff etc. extensions:
    ;; List these via eg man -Tdvi groff_char > groff_char.dvi.
    ("lq" "\"")
    ("rq" "\"")
    ("aq" "'")
    ("ha" "^")
    ("ti" "~")
    ("oq" "‘")                          ; u2018
    ("cq" "’")                          ; u2019
    ("hy" "‐")                          ; u2010
    )
  "Alist of special character codes with ASCII and extended-font equivalents.
Each alist elements has the form
   (input-string ascii-string extended-font-string . use-symbol-font)
where
 * `\\(input-string' is the ?roff encoding,
 * `ascii-string' is the (multi-character) ASCII simulation,
 * `extended-font-string' is the single-character string representing
    the character position in the extended 256-character font, and
 * `use-symbol-font' is t to indicate use of the symbol font or nil,
    i.e. omitted, to indicate use of the default font.
Any element may be nil.  Avoid control character codes (0 to \\37, \\180
to \\237) in `extended-font-string' for now, since they can be
displayed only with a modified display table.

Use the WoMan command `woman-display-extended-fonts' or a character
map accessory to help construct this alist.")

(defsubst woman-replace-match (newtext &optional face)
  "Replace text matched by last search with NEWTEXT and return t.
Set NEWTEXT in face FACE if specified."
  (woman-delete-match 0)
  (insert-before-markers newtext)
  (if face (put-text-property (1- (point)) (point) 'face 'woman-symbol))
  t)

(defun woman-special-characters (to)
  "Process special character escapes \\(xx, \\[xxx] up to buffer position TO.
\(This must be done AFTER translation, which may use special characters.)"
  (while (re-search-forward "\\\\\\(?:(\\(..\\)\\|\\[\\([[^]]+\\)\\]\\)" to t)
    (let* ((name (or (match-string-no-properties 1)
		     (match-string-no-properties 2)))
	   (replacement (assoc name woman-special-characters)))
      (unless
	  (and
	   replacement
	   (cond ((and (cddr replacement)
		       (if (nthcdr 3 replacement)
			   ;; Need symbol font:
			   (if woman-use-symbol-font
			       (woman-replace-match (nth 2 replacement)
						    'woman-symbol))
			 ;; Need extended font:
			 (if woman-use-extended-font
			     (woman-replace-match (nth 2 replacement))))))
		 ((cadr replacement)	; Use ASCII simulation
		  (woman-replace-match (cadr replacement)))))
	(WoMan-warn (concat "Special character "
			    (if (match-beginning 1) "\\(%s" "\\[%s]")
			    " not interpolated!") name)
	(if woman-ignore (woman-delete-match 0))))))

(defun woman-display-extended-fonts ()
  "Display table of glyphs of graphic characters and their octal codes.
All the octal codes in the ranges [32..127] and [160..255] are displayed
together with the corresponding glyphs from the default and symbol fonts.
Useful for constructing the alist variable `woman-special-characters'."
  (interactive)
  (with-output-to-temp-buffer "*WoMan Extended Font Map*"
    (with-current-buffer standard-output
      (let ((i 32))
	(while (< i 256)
	  (insert (format "\\%03o " i) (string i) " " (string i))
	  (put-text-property (1- (point)) (point)
			     'face 'woman-symbol)
	  (insert "   ")
	  (setq i (1+ i))
	  (when (= i 128) (setq i 160) (insert "\n"))
	  (if (zerop (% i 8)) (insert "\n")))))
    (help-print-return-message)))


;;; Formatting macros that do not cause a break:

;; Bound locally by woman[012]-roff-buffer, and also, annoyingly and
;; confusingly, as a function argument.  Use dynamically in
;; woman-unquote and woman-forward-arg.
(defvar woman-request)

(defun woman-unquote (to)
  "Delete any double-quote characters between point and TO.
Leave point at TO (which should be a marker)."
  (let (in-quote)
    (while (search-forward "\"" to 1)
      (if (and in-quote (looking-at "\""))
	  ;; Repeated double-quote represents single double-quote
	  (delete-char 1)
	(if (or in-quote (looking-at ".*\"")) ; paired
	    (delete-char -1))
	(setq in-quote (not in-quote))
	))
    (if in-quote
	(WoMan-warn "Unpaired \" in .%s arguments." woman-request))))

(defsubst woman-unquote-args ()
  "Delete any double-quote characters up to the end of the line."
  (woman-unquote (save-excursion (end-of-line) (point-marker))))

(defun woman1-roff-buffer ()
  "Process non-breaking requests."
  (let ((case-fold-search t)
	woman-request fn woman1-unquote)
    (while
	;; Find next control line:
	(re-search-forward woman-request-regexp nil t)
      (cond
       ;; Construct woman function to call:
       ((setq fn (intern-soft
		  (concat "woman1-"
			  (setq woman-request (match-string 1)))))
	(if (get fn 'notfont)		; not a font-change request
	    (funcall fn)
	  ;; Delete request or macro name:
	  (woman-delete-match 0)
	  ;; If no args then apply to next line else unquote args
	  ;; (woman1-unquote is used by called function):
	  (setq woman1-unquote (not (eolp)))
	  (if (eolp) (delete-char 1))
;	    ;; Hide leading control character in unquoted argument:
;	    (cond ((memq (following-char) '(?. ?'))
;		   (insert "\\&")
;		   (beginning-of-line)))
	  ;; Call the appropriate function:
	  (funcall fn)
	  ;; Hide leading control character in quoted argument (only):
	  (if (and woman1-unquote (memq (following-char) '(?. ?')))
	      (insert "\\&"))))))))

;;; Font-changing macros:

(defun woman1-B ()
  ".B -- Set words of current line in bold font."
  (woman1-B-or-I ".ft B\n"))

(defun woman1-I ()
  ".I -- Set words of current line in italic font."
  (woman1-B-or-I ".ft I\n"))

(defvar woman1-unquote)          ; bound locally by woman1-roff-buffer

(defun woman1-B-or-I (B-or-I)
  ".B/I -- Set words of current line in bold/italic font.
B-OR-I is the appropriate complete control line."
  ;; Should NOT concatenate the arguments!
  (insert B-or-I) ; because it might be a control line
  ;; Return to bol to process .SM/.B, .B/.if etc.
  ;; or start of first arg to hide leading control char.
  (save-excursion
    (if woman1-unquote
	(woman-unquote-args)
      (while (looking-at "^[.']") (forward-line))
      (end-of-line)
      (delete-horizontal-space))
    (insert "\\fR")))

(defun woman1-SM ()
  ".SM -- Set the current line in small font, i.e. IGNORE!"
  nil)

(defalias 'woman1-SB 'woman1-B)
;; .SB -- Set the current line in small bold font, i.e. just embolden!
;; (This is what /usr/local/share/groff/tmac/tmac.an does.  The
;; Linux man.7 is wrong about this!)

(defun woman1-BI ()
  ".BI -- Join words of current line alternating bold and italic fonts."
  (woman1-alt-fonts (list "\\fB" "\\fI")))

(defun woman1-BR ()
  ".BR -- Join words of current line alternating bold and Roman fonts."
  (woman1-alt-fonts (list "\\fB" "\\fR")))

(defun woman1-IB ()
  ".IB -- Join words of current line alternating italic and bold fonts."
  (woman1-alt-fonts (list "\\fI" "\\fB")))

(defun woman1-IR ()
   ".IR -- Join words of current line alternating italic and Roman fonts."
 (woman1-alt-fonts (list "\\fI" "\\fR")))

(defun woman1-RB ()
   ".RB -- Join words of current line alternating Roman and bold fonts."
  (woman1-alt-fonts (list "\\fR" "\\fB")))

(defun woman1-RI ()
   ".RI -- Join words of current line alternating Roman and italic fonts."
  (woman1-alt-fonts (list "\\fR" "\\fI")))

(defun woman1-alt-fonts (fonts)
  "Join words using alternating fonts in FONTS, which MUST be a dynamic list."
  (nconc fonts fonts)			; circular list!
  (insert (car fonts))
  ;; Return to start of first arg to hide leading control char:
  (save-excursion
    (setq fonts (cdr fonts))
    ;; woman1-unquote is bound in woman1-roff-buffer.
    (woman-forward-arg woman1-unquote 'concat)
    (while (not (eolp))
      (insert (car fonts))
      (setq fonts (cdr fonts))
      (woman-forward-arg woman1-unquote 'concat))
    (insert "\\fR")))

(defun woman-forward-arg (&optional unquote concat)
  "Move forward over one ?roff argument, optionally unquoting and/or joining.
If optional arg UNQUOTE is non-nil then delete any argument quotes.
If optional arg CONCAT is non-nil then join arguments."
  (if (eq (following-char) ?\")
      (progn
	(if unquote (delete-char 1) (forward-char))
	(re-search-forward "\"\\|$")
	;; Repeated double-quote represents single double-quote
	(while (eq (following-char) ?\") ; paired
	  (if unquote (delete-char 1) (forward-char))
	  (re-search-forward "\"\\|$"))
	(if (eq (preceding-char) ?\")
	    (if unquote (delete-char -1))
	  (WoMan-warn "Unpaired \" in .%s arguments." woman-request)))
    ;; (re-search-forward "[^\\\n] \\|$")	; inconsistent
    (skip-syntax-forward "^ "))
  (cond ((null concat) (skip-chars-forward " \t")) ; don't skip eol!
	((eq concat 'noskip))  ; do not skip following whitespace
	(t (woman-delete-following-space))))


;; The following requests are not explicit font-change requests and
;; so are flagged `notfont' to turn off automatic request deletion
;; and further processing.

(put 'woman1-TP 'notfont t)
(defun woman1-TP ()
  ".TP -- After tag line, reset font to Roman for paragraph body."
  ;; Same for .IP, but forward only 1 line?
  (save-excursion
    ;; May be an `irrelevant' control line in the way, so ...
    (forward-line)
    (forward-line (if (looking-at "\\.\\S-+[ \t]*$") 2 1))
    ;; May be looking at control line, so ...
    (insert ".ft R\n")))

(put 'woman1-ul 'notfont t)
(defun woman1-ul ()
  ".ul N -- Underline (italicize) the next N input lines, default N = 1."
  (let ((N (if (eolp) 1 (woman-parse-numeric-arg)))) ; woman-get-numeric-arg ?
    (woman-delete-whole-line)
    (insert ".ft I\n")
    (forward-line N)
    (insert ".ft R\n")))

;;; Other non-breaking requests:

;; Hyphenation
;; Warnings commented out.

(put 'woman1-nh 'notfont t)
(defun woman1-nh ()
  ".nh -- No hyphenation, i.e. IGNORE!"
  ;; Must be handled here to avoid breaking!
  ;; (WoMan-log-1 ".nh request ignored -- hyphenation not supported!")
  (woman-delete-whole-line))

(put 'woman1-hy 'notfont t)
(defun woman1-hy ()
  ".hy N -- Set hyphenation mode to N, i.e. IGNORE!"
  ;; (WoMan-log-1 ".hy request ignored -- hyphenation not supported!")
  (woman-delete-whole-line))

(put 'woman1-hc 'notfont t)
(defun woman1-hc ()
  ".hc c -- Set hyphenation character to c, i.e. delete it!"
  (let ((c (char-to-string (following-char))))
    ;; (WoMan-log
     ;; "Hyphenation character %s deleted -- hyphenation not supported!" c)
    (woman-delete-whole-line)
    (setq c (concat "\\(" c "\\)\\|^[.'][ \t]*hc"))
    (save-excursion
      (while (and (re-search-forward c nil t)
		  (match-beginning 1))
	(delete-char -1)))))

(put 'woman1-hw 'notfont t)
(defun woman1-hw ()
  ".hw words -- Set hyphenation exception words, i.e. IGNORE!"
  ;; (WoMan-log-1 ".hw request ignored -- hyphenation not supported!")
  (woman-delete-whole-line))

;;; Other non-breaking requests correctly ignored by nroff:

(put 'woman1-ps 'notfont t)
(defalias 'woman1-ps 'woman-delete-whole-line)
  ;; .ps -- Point size -- IGNORE!

(put 'woman1-ss 'notfont t)
(defalias 'woman1-ss 'woman-delete-whole-line)
  ;; .ss -- Space-character size -- IGNORE!

(put 'woman1-cs 'notfont t)
(defalias 'woman1-cs 'woman-delete-whole-line)
  ;; .cs -- Constant character space (width) mode -- IGNORE!

(put 'woman1-ne 'notfont t)
(defalias 'woman1-ne 'woman-delete-whole-line)
  ;; .ne -- Need vertical space -- IGNORE!

(put 'woman1-vs 'notfont t)
(defalias 'woman1-vs 'woman-delete-whole-line)
  ;; .vs -- Vertical base line spacing -- IGNORE!

(put 'woman1-bd 'notfont t)
(defalias 'woman1-bd 'woman-delete-whole-line)
  ;; .bd -- Embolden font -- IGNORE!

;;; Non-breaking SunOS-specific macros:

(defun woman1-TX ()
  ".TX t p -- Resolve SunOS abbrev t and join to p (usually punctuation)."
  (insert "SunOS ")
  (woman-forward-arg 'unquote 'concat))

(put 'woman1-IX 'notfont t)
(defalias 'woman1-IX 'woman-delete-whole-line)
  ;; .IX -- Index macro, for Sun internal use -- IGNORE!


;;; Direct font selection:

(defconst woman-font-alist
  '(("R" . default)
    ("I" . woman-italic)
    ("B" . woman-bold)
    ("P" . previous)
    ("1" . default)
    ("2" . woman-italic)
    ("3" . woman-bold)			; used in bash.1
    )
  "Alist of ?roff font indicators and woman font variables and names.")

(defun woman-change-fonts ()
  "Process font changes."
  ;; ***** NEEDS REVISING IF IT WORKS OK *****
  ;; Paragraph .LP/PP/HP/IP/TP and font .B/.BI etc. macros reset font.
  ;; Should .SH/.SS reset font?
  ;; Font size setting macros (?) should reset font.
  (let ((font-alist woman-font-alist) ; for local updating
	(previous-pos (point))
	(previous-font 'default)
	(current-font 'default))
    (while
	;; Find font requests, paragraph macros and font escapes:
	(re-search-forward
	 "^[.'][ \t]*\\(\\(\\ft\\)\\|\\(.P\\)\\)\\|\\(\\\\f\\)" nil 1)
      (let (font beg notfont fescape)
	;; Match font indicator and leave point at end of sequence:
	(cond ((match-beginning 2)
	       ;; .ft request found
	       (setq beg (match-beginning 0))
	       (skip-chars-forward " \t")
	       (if (eolp)		; default is previous font
		   (setq font previous-font)
		 (looking-at "[^ \t\n]+"))
	       (forward-line))		; end of control line and \n
	      ((match-beginning 3)
	       ;; Macro that resets font found
	       (setq font 'default))
	      ((match-beginning 4)
	       ;; \f escape found
	       (setq beg (match-beginning 0)
                     fescape t)
	       (woman-match-name))
	      (t (setq notfont t)))
	(unless notfont
	  ;; Get font name:
	  (or font
	      (let ((fontstring (match-string 0)))
		(setq font (assoc fontstring font-alist)
		      ;; NB: font-alist contains VARIABLE NAMES.
		      font (if font
			       (cdr font)
			     (WoMan-warn "Unknown font %s." fontstring)
			     ;; Output this message once only per call ...
			     (setq font-alist
				   (cons (cons fontstring 'woman-unknown)
					 font-alist))
			     'woman-unknown)
		      )))
	  ;; Delete font control line or escape sequence:
	  (cond (beg (delete-region beg (point))
		     (if (eq font 'previous) (setq font previous-font))))
          ;; Deal with things like \fB.cvsrc\fR at the start of a line.
          ;; After removing the font control codes, this would
          ;; otherwise match woman-request-regexp. The "\\&" which is
          ;; inserted to prevent this is removed by woman2-process-escapes.
          (and fescape
               (looking-at woman-request-regexp)
               (insert "\\&"))
	  (woman-set-face previous-pos (point) current-font)
	  (if beg
	      ;; Explicit font control
	      (setq previous-pos (point)
		    previous-font current-font)
	    ;; Macro that resets font
	    ;; (forward-line)		; DOES NOT WORK!  but unnecessary?
	    ;; Must process font changes in any paragraph tag!
	    (setq previous-pos (point)
		  previous-font 'default))
	  (setq current-font font)
	  )))
    ;; Set font after last request up to eob:
    (woman-set-face previous-pos (point) current-font)))

(defun woman-set-face (from to face)
  "Set the face of the text from FROM to TO to face FACE.
Ignore the default face and underline only word characters."
  (or (eq face 'default)		; ignore
      (not woman-fontify)
      (if (face-underline-p face)
	  (save-excursion
	    (let ((face-no-ul (intern (concat (symbol-name face) "-no-ul"))))
	      (goto-char from)
	      (while (< (point) to)
		(skip-syntax-forward "w" to)
		(put-text-property from (point) 'face face)
		(setq from (point))
		(skip-syntax-forward "^w" to)
		(put-text-property from (point) 'face face-no-ul)
		(setq from (point))
		)))
	(put-text-property from to 'face face))))


;;; Output translation:

;; This is only set by woman2-tr.  It is bound locally in woman2-roff-buffer.
;; It is also used by woman-translate.  woman-translate may be called
;; outside the scope of woman2-roff-buffer (by experiment).  Therefore
;; this used to be globally bound to nil, to avoid an error.  Instead
;; we can use bound-and-true-p in woman-translate.
(defvar woman-translations)
;; A list of the form (\"[ace]\" (a . b) (c . d) (e . ?\ )) or nil.

(defun woman-get-next-char ()
  "Return and delete next char in buffer, including special chars."
  (if ;;(looking-at "\\\\(\\(..\\)")
      ;; Match special \(xx and strings \*[xxx], \*(xx, \*x:
      (looking-at "\\\\\\((..\\|\\*\\(\\[[^]]+\\]\\|(..\\|.\\)\\)")
      (prog1 (match-string 0)
	(woman-delete-match 0))
    (prog1 (char-to-string (following-char))
      (delete-char 1))))

(defun woman2-tr (to)
  ".tr abcde -- Translate a -> b, c -> d, ..., e -> space.
Format paragraphs upto TO.  Supports special chars.
\(Breaks, but should not.)"
  ;; This should be an update, but consing onto the front of the alist
  ;; has the same effect and match duplicates should not matter.
  ;; Initialize translation data structures:
  (let ((matches (car woman-translations))
	(alist (cdr woman-translations))
	a b)
    ;; `matches' must be a string:
    (setq matches
	  (concat (if matches (substring matches 1 -1)) "]"))
    ;; Process .tr arguments:
    (while (not (eolp))			; (looking-at "[ \t]*$") ???
      (setq a (woman-get-next-char))
      (if (eolp)
	  (setq b " ")
	(setq b (woman-get-next-char)))
      (setq matches
	    (if (= (length a) 1)
		(concat a matches)
	      (concat matches "\\|\\" a))
	    alist (cons (cons a b) alist)))
    (delete-char 1)			; no blank line
    ;; Rebuild translations list:
    (setq matches
	  (if (= (string-to-char matches) ?\])
	      (substring matches 3)
	    (concat "[" matches))
	  woman-translations (cons matches alist))
    ;; Format any following text:
    (woman2-format-paragraphs to)))

(defsubst woman-translate (to)
  "Translate up to marker TO.  Do this last of all transformations."
  (if (bound-and-true-p woman-translations)
      (let ((matches (car woman-translations))
	    (alist (cdr woman-translations))
	    ;; Translations are case-sensitive, eg ".tr ab" does not
	    ;; affect "A" (bug#6849).
	    (case-fold-search nil))
	(while (re-search-forward matches to t)
	  ;; Done like this to retain text properties and
	  ;; support translation of special characters:
	  (insert-before-markers-and-inherit
	   (cdr (assoc
		 (buffer-substring-no-properties
		  (match-beginning 0) (match-end 0))
		 alist)))
	  (woman-delete-match 0)))))


;;; Registers:

(defvar woman-registers			; these are all read-only
  '((".H" 24) (".V" 48)			; resolution in basic units
    (".g" 0)				; not groff
    ;; (Iff emulating groff need to implement groff italic correction
    ;; \/, e.g. for pic.1)
    (".i" left-margin)			; current indent
    (".j" woman-adjust)			; current adjustment
    (".l" fill-column)			; current line length
    (".s" 12)				; current point size
    (".u" (if woman-nofill 0 1))	; 1/0 in fill/nofill mode
    (".v" 48)				; current vertical line spacing
    )
  "Register alist: the key is the register name as a string.
Each element has the form (KEY VALUE . INC) -- inc may be nil.
Also bound locally in `woman2-roff-buffer'.")

(defun woman-mark-horizonal-position ()
  "\\kx -- Store current horizontal position in INPUT LINE in register x."
  (while (re-search-forward "\\\\k\\(.\\)" nil t)
    (goto-char (match-beginning 0))
    (setq woman-registers
	  (cons (list (match-string 1) (current-column))
		woman-registers))
    (woman-delete-match 0)))

(defsubst woman2-process-escapes-to-eol (&optional numeric)
  "Process remaining escape sequences up to eol.
Handle numeric arguments specially if optional argument NUMERIC is non-nil."
  (woman2-process-escapes (copy-marker (line-end-position) t) numeric))

(defun woman2-nr (to)
  ".nr R +/-N M -- Assign +/-N (wrt to previous value, if any) to register R.
The increment for auto-incrementing is set to M.
Format paragraphs upto TO.  (Breaks, but should not!)"
  (let* ((name (buffer-substring
		(point)
		(progn (skip-syntax-forward "^ ") (point))))
	 (pm (progn			; increment
	       (skip-chars-forward " \t")
	       (when (memq (char-after) '(?+ ?-))
		 (forward-char) (char-before))))
	 (value (if (eolp)		; no value
		    nil			; to be interpreted as zero
		  (woman2-process-escapes-to-eol 'numeric)
		  (woman-parse-numeric-arg)))
	 (inc (progn			; auto-increment
		(skip-chars-forward " \t")
		(if (eolp)		; no value
		    nil			; to be interpreted as zero ???
		  (woman-parse-numeric-arg))))
	 (oldvalue (assoc name woman-registers)))
    (when oldvalue
      (setq oldvalue (cdr oldvalue))	; (value . inc)
      (unless inc (setq inc (cdr oldvalue))))
    (cond ((null value)
	   (setq value 0)		; correct?
	   (WoMan-warn "nr %s -- null value assigned as zero!" name))
	  ((symbolp value)
	   (setq value (list 'quote value))))
    (if pm				; increment old value
	(setq oldvalue (if oldvalue (car oldvalue) 0)
	      value (if (eq pm ?+)
			(+ oldvalue value)
		      (- oldvalue value))))
    (setq woman-registers
	  (cons (cons name (cons value inc)) woman-registers))
    (woman-delete-whole-line)
    (woman2-format-paragraphs to)))


;;; Numeric (and "non-text") request arguments:

(defsubst woman-get-numeric-arg ()
  "Get the value of a numeric argument at or after point.
The argument can include the width function and scale indicators.
Assumes 10 characters per inch.  Does not move point."
  (woman2-process-escapes-to-eol 'numeric)
  (save-excursion (woman-parse-numeric-arg)))

(defun woman-parse-numeric-arg ()
  "Get the value of a numeric expression at or after point.
Unlike `woman-get-numeric-arg', leaves point after the argument.
The expression may be an argument in quotes."
  (if (= (following-char) ?\") (forward-char))
  ;; Allow leading +/-:
  (let ((value (if (looking-at "[+-]") 0 (woman-parse-numeric-value)))
	op)
    (while (cond
	    ((looking-at "[+-/*%]")	; arithmetic operators
	     (forward-char)
	     (setq op (intern-soft (match-string 0)))
	     (setq value (funcall op value (woman-parse-numeric-value))))
	    ((looking-at "[<=>]=?")	; relational operators
	     (goto-char (match-end 0))
	     (setq op (intern-soft
                       (if (string-equal (match-string 0) "==")
                           "="
                         (match-string 0))))
	     (setq value (if (funcall op value (woman-parse-numeric-value))
			     1 0)))
	    ((memq (setq op (following-char)) '(?& ?:)) ; Boolean and / or
	     (forward-char)
	     (setq value
		   ;; and / or are special forms, not functions, in ELisp
		   (if (eq op ?&)
		       ;; and
		       (if (> value 0)
			   (if (> (woman-parse-numeric-value) 0) 1 0)
			 ;; skip second operand
			 (prog1 0 (woman-parse-numeric-value)))
		     ;; or
		     (if (> value 0)
			 ;; skip second operand
			 (prog1 1 (woman-parse-numeric-value))
		       (if (> (woman-parse-numeric-value) 0) 1 0))
		     )))
	    ))
;    (if (looking-at "[ \t\nRC\)\"]")	; R, C are tab types
;	()
;      (WoMan-warn "Unimplemented numerical operator `%c' in %s"
;		  (following-char)
;		  (buffer-substring
;		   (line-beginning-position)
;		   (line-end-position)))
;      (skip-syntax-forward "^ "))
    value
    ))

(defun woman-parse-numeric-value ()
  "Get a single numeric value at or after point.
The value can be a number register or width function (which assumes 10
characters per inch) and can include scale indicators.  It may be an
expression in parentheses.  Leaves point after the value."
  ;; Must replace every \' by some different single character first
  ;; before calling this function by calling
  ;; (woman2-process-escapes-to-eol 'numeric)
  (if (eq (following-char) ?\()
      ;; Treat parenthesized expression as a single value.
      (let (n)
	(forward-char)
	(setq n (woman-parse-numeric-arg))
	(skip-syntax-forward " ")
	(if (eq (following-char) ?\))
	    (forward-char)
	  (WoMan-warn "Parenthesis confusion in numeric expression!"))
	n)
    (let ((n (cond ((looking-at "[-+]?[.0-9]+")	; single number
		    ;; currently needed to set match-end, even though
		    ;; string-to-number returns 0 if number not parsed.
		    (string-to-number (match-string 0)))
		   ((looking-at "\\\\n\\([-+]\\)?\\(?:\
\\[\\([^]]+\\)\\]\\|\(\\(..\\)\\|\\(.\\)\\)")
		    ;; interpolate number register, maybe auto-incremented
		    (let* ((pm (match-string-no-properties 1))
			   (name (or (match-string-no-properties 2)
				     (match-string-no-properties 3)
				     (match-string-no-properties 4)))
			   (value (assoc name woman-registers)))
		      (if value
			  (let (inc)
			    (setq value (cdr value) ; (value . inc)
				  inc (cdr value)
				  ;; eval internal (.X) registers
				  ;; stored as lisp variable names:
				  value (eval (car value)))
			    (if (and pm inc) ; auto-increment
				(setq value
				      (funcall (intern-soft pm) value inc)
				      woman-registers
				      (cons (cons name (cons value inc))
					    woman-registers)))
			    value)
			(WoMan-warn "Undefined register %s defaulted to 0."
				    name)
			0)		; default to zero
		      ))
		   ((re-search-forward
		     ;; Delimiter can be special char escape \[xxx],
		     ;; \(xx or single normal char (usually '):
		     "\\=\\\\w\\(\\\\\\[[^]]+\\]\\|\\\\(..\\|.\\)" nil t)
		    (let ((from (match-end 0))
			  (delim (regexp-quote (match-string 1))))
		      (if (re-search-forward delim nil t)
			  ;; Return width of string:
			  (- (match-beginning 0) from)
			(WoMan-warn "Width escape delimiter error!")))))))
      (if (null n)
	  ;; ERROR -- should handle this better!
	  (progn
	    (WoMan-warn "Numeric/register argument error: %s"
			(buffer-substring
			 (point)
			 (line-end-position)))
	    (skip-syntax-forward "^ ")
	    0)
	(goto-char (match-end 0))
	;; Check for scale factor:
	(if
	    (cond
	     ((looking-at "\\s ") nil)	; stay put!
	     ((looking-at "[mnuv]"))	; ignore for now
	     ((looking-at "i") (setq n (* n 10))) ; inch
	     ((looking-at "c") (setq n (* n 3.9))) ; cm
	     ((looking-at "P") (setq n (* n 1.7))) ; Pica
	     ((looking-at "p") (setq n (* n 0.14))) ; point
	     ;; NB: May be immediately followed by + or -, etc.,
	     ;; in which case do nothing and return nil.
	     )
	    (goto-char (match-end 0)))
	(if (numberp n) (round n) n)))))


;;; VERTICAL FORMATTING -- Formatting macros that cause a break:

;; Vertical spacing philosophy:
;; Delete all vertical space as it is encountered.  Then insert
;; vertical space only before text, as required.

(defun woman2-roff-buffer ()
  "Process breaks.  Format paragraphs and headings."
  (let ((case-fold-search t)
	(to (make-marker))
	(canonically-space-region
	 (symbol-function 'canonically-space-region))
	(insert-and-inherit (symbol-function 'insert-and-inherit))
	(set-text-properties (symbol-function 'set-text-properties))
	(woman-registers woman-registers)
	fn woman-request woman-translations
	tab-stop-list)
    (set-marker-insertion-type to t)
    ;; ?roff does not squeeze multiple spaces, but does fill, so...
    (fset 'canonically-space-region 'ignore)
    ;; Try to avoid spaces inheriting underlines from preceding text!
    (fset 'insert-and-inherit (symbol-function 'insert))
    (fset 'set-text-properties 'ignore)
    (unwind-protect
	(while
	    ;; Find next control line:
            (re-search-forward woman-request-regexp nil t)
          (cond
           ;; Construct woman function to call:
           ((setq fn (intern-soft
                      (concat "woman2-"
                              (setq woman-request (match-string 1)))))
            ;; Delete request or macro name:
            (woman-delete-match 0))
           ;; Unrecognized request:
           ((prog1 nil
              ;; (WoMan-warn ".%s request ignored!" woman-request)
              (WoMan-warn-ignored woman-request "ignored!")
              ;; (setq fn 'woman2-LP)
              ;; AVOID LEAVING A BLANK LINE!
              ;; (setq fn 'woman2-format-paragraphs)
              ))
           ;; .LP assumes it is at eol and leaves a (blank) line,
           ;; so leave point at end of line before paragraph:
           ((or (looking-at "[ \t]*$") ; no argument
                woman-ignore)          ; ignore all
            ;; (beginning-of-line) (kill-line)
            ;; AVOID LEAVING A BLANK LINE!
            (beginning-of-line) (woman-delete-line 1))
           (t (end-of-line) (insert ?\n))
           )
           (if (not (or fn
                        (and (not (memq (following-char) '(?. ?')))
                             (setq fn 'woman2-format-paragraphs))))
               ()
             ;; Find next control line:
             (set-marker to (woman-find-next-control-line))
             ;; Call the appropriate function:
             (funcall fn to)))
      (if (not (eobp))			; This should not happen, but ...
	  (woman2-format-paragraphs (copy-marker (point-max) t)
                                    woman-left-margin))
      (fset 'canonically-space-region canonically-space-region)
      (fset 'set-text-properties set-text-properties)
      (fset 'insert-and-inherit insert-and-inherit)
      (set-marker to nil))))

(defun woman-find-next-control-line ()
  "Find and return start of next control line."
;  (let ((to (save-excursion
;	      (re-search-forward "^\\." nil t))))
;    (if to (1- to) (point-max)))
  (let (to)
    (save-excursion
      ;; Must handle
      ;; ...\c
      ;; .br (and other requests?)
      ;; by deleting both the \c and the following request.
      ;; BEWARE THAT THIS CODE MAY BE UNRELIABLE!!!!!
      (while
	  (and
	   (setq to (re-search-forward "\\(\\\\c\\)?\n[.']" nil t))
	   (match-beginning 1)
	   (looking-at "br"))
	(goto-char (match-beginning 0))
	(woman-delete-line 2)))
    (if to (1- to) (point-max))))

(defun woman2-PD (to)
  ".PD d -- Set the interparagraph distance to d.
Round to whole lines, default 1 line.  Format paragraphs upto TO.
\(Breaks, but should not.)"
  ;; .ie \\n[.$] .nr PD (v;\\$1)
  ;; .el .nr PD .4v>?\n[.V]
  (woman-set-interparagraph-distance)
  (woman2-format-paragraphs to))

(defun woman-set-interparagraph-distance ()
  "Set the interparagraph distance from a .PD request at point."
  (setq woman-interparagraph-distance
	(if (eolp) 1 (woman-get-numeric-arg)))
  ;; Should allow .PD 0 to set zero line spacing
  (woman-delete-line 1))		; ignore remaining args

(defsubst woman-interparagraph-space ()
  "Set variable `woman-leave-blank-lines' from `woman-interparagraph-distance'."
  (setq woman-leave-blank-lines woman-interparagraph-distance))

(defun woman2-TH (to)
  ".TH n c x v m -- Begin a man page.  Format paragraphs upto TO.
n is the name of the page in chapter c\; x is extra commentary\;
v alters page foot left; m alters page head center.
\(Should set prevailing indent and tabs to 5.)"
  (woman-forward-arg 'unquote 'concat)
  (insert ?\()
  (woman-forward-arg 'unquote 'concat)
  (insert ?\))
  (let ((start (point)) here)
    (while (not (eolp))
      (cond ((looking-at "\"\"[ \t]")
	     (delete-char 2)))
      (delete-horizontal-space)
      (setq here (point))
      (insert " -- ")
      (woman-forward-arg 'unquote 'concat)
      ;; Delete repeated arguments:
      (if (string-equal (buffer-substring here (point))
			(buffer-substring start here))
	  (delete-region here (point)))))
  ;; Embolden heading (point is at end of heading):
  (woman-set-face (line-beginning-position) (point) 'woman-bold)
  (forward-line)
  (delete-blank-lines)
  (setq woman-left-margin woman-default-indent)
  (setq woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defun woman2-SH (to)
  ".SH -- Sub-head.  Leave blank line and subhead.
Format paragraphs upto TO.  Set prevailing indent to 5."
  (if (eolp)				; If no args then
      (delete-char 1)			; apply to next line
    (woman-unquote-args)		; else unquote to end of heading
    (beginning-of-line))
  (woman2-process-escapes-to-eol)
  (woman-leave-blank-lines woman-interparagraph-distance)
  (setq woman-leave-blank-lines nil)
  ;; Optionally embolden heading (point is at beginning of heading):
  (if woman-bold-headings
      (woman-set-face (point) (line-end-position) 'woman-bold))
  (forward-line)
  (setq woman-left-margin woman-default-indent
	woman-nofill nil)		; fill output lines
  (setq woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defun woman2-SS (to)
  ".SS -- Sub-sub-head.  Like .SH but indent heading 3 spaces.
Format paragraphs upto TO."
  (if (eolp)				; If no args then
      (delete-char 1))			; apply to next line.
  (insert "   ")
  (beginning-of-line)
  (woman2-SH to))

(defun woman2-LP (to)
  ".LP,.PP -- Begin paragraph.  Set prevailing indent to 5.
Leave 1 blank line.  Format paragraphs upto TO."
  (woman-delete-line 1)			; ignore any arguments
  (woman-interparagraph-space)
  (setq woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defalias 'woman2-PP 'woman2-LP)
(defalias 'woman2-P 'woman2-LP)

(defun woman2-ns (to)
  ".ns -- Turn on no-space mode.  Format paragraphs upto TO."
  ;; Should not cause a break!
  (woman-delete-line 1)			; ignore argument(s)
  (setq woman-nospace t)
  (woman2-format-paragraphs to))

(defun woman2-rs (to)
  ".rs -- Turn off no-space mode.  Format paragraphs upto TO."
  ;; Should not cause a break!
  (woman-delete-line 1)			; ignore argument(s)
  (setq woman-nospace nil)
  (woman2-format-paragraphs to))

(defun woman2-sp (to)
  ".sp N -- If N > 0 then leave 1 blank line.  Format paragraphs upto TO."
  (let ((N (if (eolp) 1 (woman-get-numeric-arg))))
    (if (>= N 0)
	(woman-delete-line 1)		; ignore argument(s)
      (setq woman-negative-vertical-space t)
      (insert ".sp ")
      (forward-line))
    (setq woman-leave-blank-lines N)
    (woman2-format-paragraphs to)))

(defun woman-negative-vertical-space (from)
  ".sp N with N < 0 => overlap following with preceding lines at FROM."
  ;; Run by woman-decode-region if necessary -- not usually required.
  (WoMan-warn "Negative vertical spacing support is experimental!")
  (goto-char from)
  (while
      ;; Find next control line:
      (re-search-forward "^\\.sp " nil t)
    (let ((N (woman-get-numeric-arg))
	  overlap overwritten)
      (woman-delete-whole-line)
      (setq from (point)
	    overlap (buffer-substring from
				      (progn (forward-line (- N)) (point))))
      (delete-region from (point))
      (forward-line N)
      (let ((imax (length overlap))
	    (i 0) c)
	(while (< i imax)
	  (setq c (aref overlap i))
	  (cond ((eq c ?\n)		; skip
		 (forward-line))
		((eolp)			; extend line
		 ;; Insert character INCLUDING TEXT PROPERTIES:
		 ;; (insert (substring overlap i (1+ i)))
		 (let ((eol (string-match "\n" overlap i)))
		   (insert (substring overlap i eol))
		   (setq i (or eol imax)))
		 )
		((eq c ?\ )		; skip
		 (forward-char))
		((eq c ?\t)		; skip
		 (if (eq (following-char) ?\t)
		     (forward-char)	; both tabs, just skip
		   (dotimes (i woman-tab-width)
                     (if (eolp)
                         (insert ?\ )	; extend line
                       (forward-char)) ; skip
		     )))
		(t
		 (if (or (eq (following-char) ?\ ) ; overwrite OK
			 overwritten) ; warning only once per ".sp -"
		     ()
		   (setq overwritten t)
		   (WoMan-warn
		    "Character(s) overwritten by negative vertical spacing in line %d"
		    (count-lines 1 (point))))
		 (delete-char 1) (insert (substring overlap i (1+ i)))))
	  (setq i (1+ i)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following function should probably do ALL width and number
;; register interpolation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun woman2-process-escapes (to &optional numeric)
  "Process remaining escape sequences up to marker TO, preserving point.
Optional argument NUMERIC, if non-nil, means the argument is numeric."
  (assert (and (markerp to) (marker-insertion-type to)))
  ;; The first two cases below could be merged (maybe)!
  (let ((from (point)))
    ;; Discard zero width filler character used to hide leading dots
    ;; and zero width characters.
    (while (re-search-forward "\\\\[&|^]" to t)
      (woman-delete-match 0)
      ;; If on a line by itself, consume newline as well (Bug#3651).
      (and (eq (char-before (match-beginning 0)) ?\n)
	   (eq (char-after (match-beginning 0)) ?\n)
	   (delete-char 1)))

    (goto-char from)
    ;; Interrupt text processing -- CONTINUE current text with the
    ;; next text line (after any control lines, unless processing to
    ;; eol):
    (while (re-search-forward "\\\\c.*\n?" to t)
      (woman-delete-match 0))
    ;; but do not delete the final newline ...
    (if (and (or (eobp) (= (point) to)) (not (bolp)))
	(insert-before-markers ?\n))
    (goto-char from)
    (woman-translate to)
    (goto-char from)
    (woman-special-characters to)
    (goto-char from)
    ;; Printable version of the current escape character, ASSUMED to be `\'
    ;; This must be done LAST of all escape processing!
    ;; Done like this to preserve any text properties of the `\'
    (while (search-forward "\\" to t)
      (let ((c (following-char)))
	;; Some other escapes, such as \f, are handled in
	;; `woman0-process-escapes'.
	(cond ((eq c ?')		; \' -> '
	       (delete-char -1)
	       (cond (numeric		; except in numeric args, \' -> `
		      (delete-char 1)
		      (insert ?`))))
	      ((eq c ?\( ))		; uninterpreted special character
    					; \(.. -- do nothing
	      ((eq c ?t)		; non-interpreted tab \t
	       (delete-char 1)
	       (delete-char -1)
	       (insert "\t"))
	      ((and numeric
		    (memq c '(?w ?n ?h)))) ; leave \w, \n, \h (?????)
	      ((eq c ?l) (woman-horizontal-line)))))
    (goto-char from)
    ;; Process non-default tab settings:
    (cond (tab-stop-list
	   (while (search-forward "\t" to t)
	     (woman-tab-to-tab-stop))
	   (goto-char from)))

    ;; Must replace \' by something before processing \w, done above.

    ;; Replace all `\w' and `\n' escapes:
    ;; (This may be a bit too recursive!)
    (while (re-search-forward "\\\\[nw]" to t)
      (let ((from (match-beginning 0)) N)
	(goto-char from)
	(setq N (woman-parse-numeric-value))
	(delete-region from (point))
	;; Interpolate value:
	(insert-before-markers (number-to-string N))))
    (goto-char from)))

(defun woman-horizontal-line ()
  "\\l'Nc' -- Draw a horizontal line of length N using character c, default _."
  (delete-char -1)
  (delete-char 1)
  (looking-at "\\(.\\)\\(.*\\)\\1")
  (forward-char 1)
  (let* ((to (match-end 2))
         (from (match-beginning 0))
         (N (woman-parse-numeric-arg))
         (c (if (< (point) to) (following-char) ?_)))
    (delete-region from to)
    (delete-char 1)
    (insert (make-string N c))))

;;; 4. Text Filling, Adjusting, and Centering

(defun woman2-br (to)
  ".br -- Break.  Leave no blank line.  Format paragraphs upto TO."
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

(defun woman2-fi (to)
  ".fi -- Fill subsequent output lines.  Leave no blank line.
Format paragraphs upto TO."
  (setq woman-nofill nil)
  (woman-delete-line 1)			; ignore any arguments
  ;; Preserve any final blank line in the nofill region:
  (save-excursion
    (forward-line -1)
    (if (looking-at "[ \t]*$") (setq woman-leave-blank-lines 1)))
  (woman2-format-paragraphs to))

(defun woman2-nf (to)
  ".nf -- Nofill.  Subsequent lines are neither filled nor adjusted.
Input text lines are copied directly to output lines without regard
for the current line length.  Format paragraphs up to TO."
  (setq woman-nofill t)
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

(defun woman2-ad (to)
  ".ad c -- Line adjustment is begun (once fill mode is on).
Set justification mode to c if specified.
Format paragraphs upto TO.  (Breaks, but should not.)"
  ;; c = l -- left, r -- right, c -- center, b or n -- both,
  ;; absent -- unchanged.  Initial mode adj,both.
  (setq woman-adjust
	(cond ((eolp) woman-adjust-previous)
	      ((eq (following-char) ?l) woman-adjust-left)
	      ((eq (following-char) ?r) woman-adjust-right)
	      ((eq (following-char) ?c) woman-adjust-center)
	      ((memq (following-char) '(?b ?n)) woman-adjust-both)
	      (t (woman-get-numeric-arg))
	      )
	woman-justify (nth woman-adjust woman-justify-list))
  (woman-delete-line 1)			; ignore any remaining arguments
  (woman2-format-paragraphs to))

(defun woman2-na (to)
  ".na -- No adjusting.  Format paragraphs upto TO.
\(Breaks, but should not.)"
  (setq woman-adjust-previous woman-adjust
	woman-justify-previous woman-justify
	woman-adjust woman-adjust-left	; fill but do not adjust
	woman-justify (nth woman-adjust woman-justify-list))
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

;;; The main formatting functions:

(defun woman-leave-blank-lines (&optional leave)
  "Delete all blank lines around point.
Leave one blank line if optional argument LEAVE is non-nil and
non-zero, or if LEAVE is nil and variable `woman-leave-blank-lines' is
non-nil and non-zero."
  ;; ***** It may suffice to delete only lines ABOVE point! *****
  ;; NOTE: Function arguments are evaluated left to right
  ;; (*note (elisp)Function Forms::.).
  (delete-region
   (save-excursion
     (if (not (eq (skip-syntax-backward " ") 0))
	 (forward-line))			; forward-char ?
     (point))
   (progn (skip-syntax-forward " ")
	  (beginning-of-line)
	  (point)))
  (unless woman-nospace
    (if (or (null leave) (eq leave 0))
	;; output any `pending' vertical space ...
	(setq leave woman-leave-blank-lines))
    (if (and leave (> leave 0)) (insert-before-markers ?\n)))
  (setq woman-leave-blank-lines nil))

;; `fill-region-as-paragraph' in `fill.el' appears to be the principal
;; text filling function, so that is what I use here.

(defvar woman-temp-indent nil)

(defun woman2-format-paragraphs (to &optional new-left)
  "Indent, fill and adjust paragraphs upto TO to current left margin.
If optional arg NEW-LEFT is non-nil then reset current left margin.
If `woman-nofill' is non-nil then indent without filling or adjusting."
  ;; Blank space should only ever be output before text.
  (if new-left (setq left-margin new-left))
  (if (looking-at "^\\s *$")
      ;; A blank line should leave a space like .sp 1 (p. 14).
      (setq woman-leave-blank-lines 1))
  (skip-syntax-forward " ")
  ;; Successive control lines are sufficiently common to be worth a
  ;; special case (maybe):
  (unless (>= (point) to)
    (woman-reset-nospace)
    (woman2-process-escapes to 'numeric)
    (if woman-nofill
	;; Indent without filling or adjusting ...
	(progn
	  (woman-leave-blank-lines)
	  (when woman-temp-indent
	    (indent-to woman-temp-indent)
	    (forward-line))
	  (indent-rigidly (point) to left-margin)
	  (woman-horizontal-escapes to))
      ;; Fill and justify ...
      ;; Blank lines and initial spaces cause a break.
      (while (< (point) to)
	(woman-leave-blank-lines)
	(let ((from (point)))
	  ;; Indent first lin  of paragraph:
	  (indent-to (or woman-temp-indent left-margin))
	  (woman-horizontal-escapes to) ; 7 October 1999
	  ;; Find the beginning of the next paragraph:
	  (forward-line)
	  (and (re-search-forward "\\(^\\s *$\\)\\|\\(^\\s +\\)" to 1)
	       ;; A blank line should leave a space like .sp 1 (p. 14).
	       (eolp)
	       (skip-syntax-forward " ")
	       (setq woman-leave-blank-lines 1))
	  ;; This shouldn't happen, but in case it does (e.g. for
	  ;; badly-formatted manfiles with no terminating newline),
	  ;; avoid an infinite loop.
	  (unless (and (eolp) (eobp))
	    (beginning-of-line))
	  ;; If a single short line then just leave it.
	  ;; This is necessary to preserve some table layouts.
	  ;; PROBABLY NOT NECESSARY WITH SQUEEZE MODIFICATION !!!!!
	  (when (or (> (count-lines from (point)) 1)
		    (save-excursion
		      (backward-char)
		      (> (current-column) fill-column)))
	    ;; NOSQUEEZE has no effect if JUSTIFY is full, so redefine
	    ;; canonically-space-region, see above.
	    (if (and woman-temp-indent (< woman-temp-indent left-margin))
		(let ((left-margin woman-temp-indent))
		  (fill-region-as-paragraph from (point) woman-justify)
		  (save-excursion
		    (goto-char from)
		    (forward-line)
		    (setq from (point)))))
	    (fill-region-as-paragraph from (point) woman-justify)))))
    (setq woman-temp-indent nil)))


;;; Tagged, indented and hanging paragraphs:

(defun woman2-TP (to)
  ".TP i -- Set prevailing indent to i.  Format paragraphs upto TO.
Begin indented paragraph with hanging tag given by next text line.
If tag doesn't fit, place it on a separate line."
  (let ((i (woman2-get-prevailing-indent)))
    (woman-leave-blank-lines woman-interparagraph-distance)
    (woman2-tagged-paragraph to i)))

(defun woman2-IP (to)
  ".IP x i -- Same as .TP with tag x.  Format paragraphs upto TO."
  (woman-interparagraph-space)
  (if (eolp)				; no args
      ;; Like LP without resetting prevailing indent
      (woman2-format-paragraphs to (+ woman-left-margin
				      woman-prevailing-indent))
    (woman-forward-arg 'unquote)
    (let ((i (woman2-get-prevailing-indent 'leave-eol)))
      (beginning-of-line)
      (woman-leave-blank-lines)		; must be here,
      ;;
      ;; The cvs.1 manpage contains some (possibly buggy) syntax that
      ;; confuses woman, although the man program displays it ok.
      ;; Most problems are caused by IP followed by another request on
      ;; the next line. Without the following hack, the second request
      ;; gets displayed raw in the output. Note that
      ;; woman2-tagged-paragraph also contains a hack for similar
      ;; issues (eg IP followed by SP).
      ;;
      ;; i) For IP followed by one or more IPs, we ignore all but the
      ;; last (mimic man). The hack in w-t-p would only work for two
      ;; consecutive IPs, and would use the first.
      ;; ii) For IP followed by SP followed by one or more requests,
      ;; do nothing. At least in cvs.1, there is usually another IP in
      ;; there somewhere.
      (unless (or (looking-at "^\\.IP")
                  (and (looking-at "^\\.sp")
                       (save-excursion
                         (and (zerop (forward-line 1))
                              (looking-at woman-request-regexp)))))
        (woman2-tagged-paragraph to i)))))

(defun woman-find-next-control-line-carefully ()
  "Find and return start of next control line, even if already there!"
  (if (looking-at "^[.']")
      (point)
    (woman-find-next-control-line)))

(defun woman2-tagged-paragraph (to i)
  "Begin indented paragraph with hanging tag given by current text line.
If tag doesn't fit, leave it on separate line.
Format paragraphs upto TO.  Set prevailing indent to I."
  (if (not (looking-at "\\s *$"))	; non-empty tag
      (setq woman-leave-blank-lines nil))

  ;; Temporary hack for bash.1, cvs.1 and groff_mmse.7 until code is revised
  ;; to process all requests uniformly.
  ;; This hack deals with IP requests followed by other requests (eg
  ;; SP) on the very next line. We skip over the SP, otherwise it gets
  ;; inserted raw in the rendered output.
  (cond ((and (= (point) to)
              (looking-at "^[.'][ \t]*\\(PD\\|br\\|ta\\|sp\\) *"))
         (if (member (match-string 1) '("br" "sp"))
             (woman-delete-line 1)
           (woman-delete-match 0)
           (if (string= (match-string 1) "ta") ; for GetInt.3
               (woman2-ta to)
             (woman-set-interparagraph-distance)))
         (set-marker to (woman-find-next-control-line-carefully))))

  (let ((tag (point)))
    (woman-reset-nospace)
    ;; Format the tag:
    (woman2-process-escapes-to-eol)
    ;; TIDY UP THE FOLLOWING CODE
    ;; (indent-to woman-left-margin)
    (setq left-margin woman-left-margin)
    (forward-line)
    (fill-region-as-paragraph (save-excursion (forward-line -1) (point))
			      (point) woman-justify)

    ;; Temporary hack for bash.1 until all requests processed uniformly:
    (cond ((and (= (point) to) (looking-at "^[.'][ \t]*PD *"))
	   (woman-delete-match 0)
	   (woman-set-interparagraph-distance)
	   (set-marker to (woman-find-next-control-line-carefully))
	   ))

    ;; Format the paragraph body, if there is one!  Set indented left
    ;; margin anyway, because the paragraph body may begin with a
    ;; control line:
    (setq left-margin (+ woman-left-margin i))
    (cond ((< (point) to)
	   (woman2-format-paragraphs to)
	   (goto-char tag)  (end-of-line)
	   (cond ((> (setq i (- left-margin (current-column))) 0)
		  (delete-char 1)
		  (delete-horizontal-space)
		  ;; Necessary to avoid spaces inheriting underlines.
		  ;; Cannot simply delete (current-column) whitespace
		  ;; characters because some may be tabs!
		  (insert-char ?\s i)))
	   (goto-char to)))))

(defun woman2-HP (to)
  ".HP i -- Set prevailing indent to i.  Format paragraphs upto TO.
Begin paragraph with hanging indent."
  (let ((i (woman2-get-prevailing-indent)))
    (woman-interparagraph-space)
    (setq woman-temp-indent woman-left-margin)
    (woman2-format-paragraphs to (+ woman-left-margin i))))

(defun woman2-get-prevailing-indent (&optional leave-eol)
  "Set prevailing indent to integer argument at point, and return it.
If no argument then return the existing prevailing indent.
Delete line from point and eol unless LEAVE-EOL is non-nil."
  (if (eolp)
      (or leave-eol (delete-char 1))
    (let ((i (woman-get-numeric-arg)))
      (woman-delete-line) (or leave-eol (delete-char 1))
      ;; i = 0 if the argument was not a number
      ;; FIXME should this be >= 0? How else to reset to 0 indent?
      (if (> i 0) (setq woman-prevailing-indent i))))
  woman-prevailing-indent)

(defmacro woman-push (value stack)
  "Push VALUE onto STACK."
  `(setq ,stack (cons ,value ,stack)))

(defmacro woman-pop (variable stack)
  "Pop into VARIABLE the value at the top of STACK.
Allow for mismatched requests!"
  `(if ,stack
       (setq ,variable (car ,stack)
	     ,stack (cdr ,stack))))

(defun woman2-RS (to)
  ".RS i -- Start relative indent, move left margin in distance i.
Set prevailing indent to 5 for nested indents.  Format paragraphs upto TO."
  (woman-push woman-left-margin woman-RS-left-margin)
  (woman-push woman-prevailing-indent woman-RS-prevailing-indent)
  (setq woman-left-margin (+ woman-left-margin
			     (woman2-get-prevailing-indent))
	woman-prevailing-indent woman-default-indent)
  (woman2-format-paragraphs to woman-left-margin))

(defun woman2-RE (to)
  ".RE -- End of relative indent.  Format paragraphs upto TO.
Set prevailing indent to amount of starting .RS."
  (woman-pop woman-left-margin woman-RS-left-margin)
  (woman-pop woman-prevailing-indent woman-RS-prevailing-indent)
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to woman-left-margin))


;;; Line Length and Indenting:

(defun woman-set-arg (arg &optional previous)
  "Reset, increment or decrement argument ARG, which must be quoted.
If no argument then use value of optional arg PREVIOUS if non-nil,
otherwise set PREVIOUS.  Delete the whole remaining control line."
  (if (eolp)				; space already skipped
      (set arg (if previous (eval previous) 0))
    (if previous (set previous (eval arg)))
    (woman2-process-escapes-to-eol 'numeric)
    (let ((pm (if (looking-at "[+-]")
		(prog1 (following-char)
		  (forward-char 1))))
	(i (woman-parse-numeric-arg)))
    (cond ((null pm) (set arg i))
	  ((= pm ?+) (set arg (+ (eval arg) i)))
	  ((= pm ?-) (set arg (- (eval arg) i)))
	  ))
    (beginning-of-line))
  (woman-delete-line 1))		; ignore any remaining arguments

;; NEED TO RATIONALIZE NAMES FOR PREVIOUS VALUES!
(defvar woman-ll-fill-column woman-fill-column)
(defvar woman-in-left-margin woman-left-margin)

(defun woman2-ll (to)
  ".ll +/-N -- Set, increment or decrement line length.
Format paragraphs upto TO.  (Breaks, but should not.)"
  (woman-set-arg 'fill-column 'woman-ll-fill-column)
  (woman2-format-paragraphs to))

(defun woman2-in (to)
  ".in +/-N -- Set, increment or decrement the indent.
Format paragraphs upto TO."
  (woman-set-arg 'left-margin 'woman-in-left-margin)
  (woman2-format-paragraphs to))

(defun woman2-ti (to)
  ".ti +/-N -- Temporary indent.  Format paragraphs upto TO."
  ;; Ignore if no argument.
  ;; Indent next output line only wrt current indent.
  ;; Current indent is not changed.
  (setq woman-temp-indent left-margin)
  (woman-set-arg 'woman-temp-indent)
  (woman2-format-paragraphs to nil))


;;; Tabs, Leaders, and Fields:

(defun woman2-ta (to)
  ".ta Nt ... -- Set tabs, left type, unless t=R(right), C(centered).
\(Breaks, but should not.)  The tab stops are separated by spaces\;
a value preceded by + represents an increment to the previous stop value.
Format paragraphs upto TO."
  (setq tab-stop-list nil)
  (woman2-process-escapes-to-eol 'numeric)
  (save-excursion
    (let ((tab-stop 0))
      (while (not (eolp))
	(let ((plus (cond ((eq (following-char) ?+) (forward-char 1) t)))
	      (i (woman-parse-numeric-arg)))
	  (setq tab-stop (if plus (+ tab-stop i) i)))
	(if (memq (following-char) '(?R ?C))
	    (setq tab-stop (cons tab-stop (following-char))))
	(setq tab-stop-list (cons tab-stop tab-stop-list))
	(skip-syntax-forward "^ ")	; skip following R, C, `;', etc.
	(skip-chars-forward " \t")
	)))
  (woman-delete-line 1)			; ignore any remaining arguments
  (setq tab-stop-list (reverse tab-stop-list))
  (woman2-format-paragraphs to))

(defsubst woman-get-tab-stop (tab-stops)
  "If TAB-STOPS is a cons, return its car, else return TAB-STOPS."
  (if (consp tab-stops) (car tab-stops) tab-stops))

(defun woman-tab-to-tab-stop ()
  "Insert spaces to next defined tab-stop column.
The variable `tab-stop-list' is a list whose elements are either left
tab stop columns or pairs (COLUMN . TYPE) where TYPE is R or C."
  ;; Based on tab-to-tab-stop in indent.el.
  ;; R & C tabs probably not quite right!
  (delete-char -1)
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column)
			 (woman-get-tab-stop (car tabs))))
      (setq tabs (cdr tabs)))
    (if tabs
	(let* ((tab (car tabs))
	       (type (and (consp tab) (cdr tab)))
	       eol n)
	  (if type
	      (setq tab (woman-get-tab-stop tab)
		    eol (line-end-position)
		    n (save-excursion
			(search-forward "\t" eol t))
		    n (- (if n (1- n) eol) (point))
		    tab (- tab (if (eq type ?C) (/ n 2) n))) )
	  (setq n (- tab (current-column)))
	  (insert-char ?\s n))
      (insert ?\ ))))

(defun woman2-DT (to)
  ".DT -- Restore default tabs.  Format paragraphs upto TO.
\(Breaks, but should not.)"
  ;; Currently just terminates special tab processing.
  (setq tab-stop-list nil)
  (woman-delete-line 1)			; ignore any arguments
  (woman2-format-paragraphs to))

(defun woman2-fc (to)
  ".fc a b -- Set field delimiter a and pad character b.
Format paragraphs upto TO.
A VERY FIRST ATTEMPT to make fields at least readable!
Needs doing properly!"
  (if (eolp)
      (woman-delete-whole-line)		; ignore!
    (let ((delim (following-char))
	  (pad ?\ ) end)		; pad defaults to space
      (forward-char)
      (skip-chars-forward " \t")
      (or (eolp) (setq pad (following-char)))
      (woman-delete-whole-line)
      (save-excursion
	(if (re-search-forward "^[.'][ \t]*fc\\s " nil t)
	    (setq end (match-beginning 0))))
      ;; A field is contained between a pair of field delimiter
      ;; characters and consists of sub-strings separated by padding
      ;; indicator characters:
      (setq delim (string delim ?[ ?^ delim ?] ?* delim))
      (save-excursion
	(while (re-search-forward delim end t)
	  (goto-char (match-beginning 0))
	  (delete-char 1)
	  (insert woman-unpadded-space-char)
	  (goto-char (match-end 0))
	  (delete-char -1)
	  (insert-before-markers woman-unpadded-space-char)
	  (subst-char-in-region
	   (match-beginning 0) (match-end 0)
	   pad woman-unpadded-space-char t)))))
  (woman2-format-paragraphs to))


;;; Preliminary table support (.TS/.TE)

(defun woman2-TS (to)
  ".TS -- Start of table code for the tbl processor.
Format paragraphs upto TO."
  ;; This is a preliminary hack that seems to suffice for lilo.8.
  (woman-delete-line 1)			; ignore any arguments
  (when woman-emulate-tbl
    ;; Assumes column separator is \t and intercolumn spacing is 3.
    ;; The first line may optionally be a list of options terminated by
    ;; a semicolon.  Currently, just delete it:
    (if (looking-at ".*;[ \t]*$") (woman-delete-line 1)) ;
    ;; The following lines must specify the format of each line of the
    ;; table and end with a period.  Currently, just delete them:
    (while (not (looking-at ".*\\.[ \t]*$")) (woman-delete-line 1))
    (woman-delete-line 1)
    ;; For each column, find its width and align it:
    (let ((start (point)) (col 1))
      (while (prog1 (search-forward "\t" to t) (goto-char start))
	;; Find current column width:
	(while (< (point) to)
	  (when (search-forward "\t" to t)
	    (backward-char)
	    (if (> (current-column) col) (setq col (current-column))))
	  (forward-line))
	;; Align current column:
	(goto-char start)
	(setq col (+ col 3))		; intercolumn space
	(while (< (point) to)
	  (when (search-forward "\t" to t)
	    (delete-char -1)
	    (insert-char ?\  (- col (current-column))))
	  (forward-line))
	(goto-char start))))
  ;; Format table with no filling or adjusting (cf. woman2-nf):
  (setq woman-nofill t)
  (woman2-format-paragraphs to))

(defalias 'woman2-TE 'woman2-fi)
  ;; ".TE -- End of table code for the tbl processor."
  ;; Turn filling and adjusting back on.


;;; WoMan message logging:

;; The basis for this logging code was shamelessly pirated from bytecomp.el
;; by Jamie Zawinski <jwz@lucid.com> & Hallvard Furuseth <hbf@ulrik.uio.no>

(defun WoMan-log-begin ()
  "Log the beginning of formatting in *WoMan-Log*."
  (let ((WoMan-current-buffer (buffer-name)))
    (with-current-buffer (get-buffer-create "*WoMan-Log*")
      (or (eq major-mode 'view-mode) (view-mode 1))
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert "\n\^L\nFormatting "
	      (if (stringp WoMan-current-file)
		  (concat "file " WoMan-current-file)
		(concat "buffer " WoMan-current-buffer))
	      " at " (current-time-string) "\n")
      (setq WoMan-Log-header-point-max (point-max)))))

(defun WoMan-log (format &rest args)
  "Log a message out of FORMAT control string and optional ARGS."
  (WoMan-log-1 (apply 'format format args)))

(defun WoMan-warn (format &rest args)
  "Log a warning message out of FORMAT control string and optional ARGS."
  (setq format (apply 'format format args))
  (WoMan-log-1 (concat "**  " format)))

;; request is not used dynamically by any callees.
(defun WoMan-warn-ignored (request ignored)
  "Log a warning message about ignored directive REQUEST.
IGNORED is a string appended to the log message."
  (let ((tail
	 (buffer-substring (point)
			   (line-end-position))))
    (if (and (> (length tail) 0)
	     (/= (string-to-char tail) ?\ ))
	(setq tail (concat " " tail)))
    (WoMan-log-1
     (concat "**  " request tail "  request " ignored))))

(defun WoMan-log-end (time)
  "Log the end of formatting in *WoMan-Log*.
TIME specifies the time it took to format the man page, to be printed
with the message."
  (WoMan-log-1 (format "Formatting time %g seconds." time) 'end))

(defun WoMan-log-1 (string &optional end)
  "Log a message STRING in *WoMan-Log*.
If optional argument END is non-nil then make buffer read-only after
logging the message."
  (with-current-buffer (get-buffer-create "*WoMan-Log*")
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (or end (insert "  "))  (insert string "\n")
    (if end
	(setq buffer-read-only t)
      (if woman-show-log
	  (select-window			; to return to
	   (prog1 (selected-window)	; WoMan window
	     (select-window (display-buffer (current-buffer)))
	     (cond (WoMan-Log-header-point-max
		    (goto-char WoMan-Log-header-point-max)
		    (forward-line -1)
		    (recenter 0))))))))
  nil)					; for woman-file-readable-p etc.

;;; Bookmark Woman support.
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))

;; FIXME: woman.el and man.el should be better integrated so, for
;; example, bookmarks of one can be used with the other.

(defun woman-bookmark-make-record ()
  "Make a bookmark entry for a Woman buffer."
  `(,(Man-default-bookmark-title)
    ,@(bookmark-make-record-default 'no-file)
    (location . ,(concat "woman " woman-last-file-name))
    ;; Use the same form as man's bookmarks, as much as possible.
    (man-args . ,woman-last-file-name)
    (handler . woman-bookmark-jump)))

;;;###autoload
(defun woman-bookmark-jump (bookmark)
  "Default bookmark handler for Woman buffers."
  (let* ((file (bookmark-prop-get bookmark 'man-args))
         ;; FIXME: we need woman-find-file-noselect, since
         ;; save-window-excursion can't protect us from the case where
         ;; woman-find-file creates a new frame.
         (buf  (save-window-excursion
                 (woman-find-file file) (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(provide 'woman)


;; Local Variables:
;; coding: utf-8
;; End:

;;; woman.el ends here
