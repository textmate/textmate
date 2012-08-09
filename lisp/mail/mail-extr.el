;;; mail-extr.el --- extract full name and address from RFC 822 mail header -*- coding: utf-8 -*-

;; Copyright (C) 1991-1994, 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@cs.bu.edu>
;; Maintainer: FSF
;; Keywords: mail
;; Package: mail-utils

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

;; The entry point of this code is
;;
;;    mail-extract-address-components: (address &optional all)
;;
;;    Given an RFC-822 ADDRESS, extract full name and canonical address.
;;    Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
;;    If no name can be extracted, FULL-NAME will be nil.
;;    ADDRESS may be a string or a buffer.  If it is a buffer, the visible
;;     (narrowed) portion of the buffer will be interpreted as the address.
;;     (This feature exists so that the clever caller might be able to avoid
;;     consing a string.)
;;    If ADDRESS contains more than one RFC-822 address, only the first is
;;     returned.
;;
;;    If ALL is non-nil, that means return info about all the addresses
;;     that are found in ADDRESS.  The value is a list of elements of
;;     the form (FULL-NAME CANONICAL-ADDRESS), one per address.
;;
;; This code is more correct (and more heuristic) parser than the code in
;; rfc822.el.  And despite its size, it's fairly fast.
;;
;; There are two main benefits:
;;
;; 1. Higher probability of getting the correct full name for a human than
;;    any other package we know of.  (On the other hand, it will cheerfully
;;    mangle non-human names/comments.)
;; 2. Address part is put in a canonical form.
;;
;; The interface is not yet carved in stone; please give us suggestions.
;;
;; We have an extensive test-case collection of funny addresses if you want to
;; work with the code.  Developing this code requires frequent testing to
;; make sure you're not breaking functionality.  The test cases aren't included
;; because they are over 100K.
;;
;; If you find an address that mail-extr fails on, please send it to the
;; maintainer along with what you think the correct results should be.  We do
;; not consider it a bug if mail-extr mangles a comment that does not
;; correspond to a real human full name, although we would prefer that
;; mail-extr would return the comment as-is.
;;
;; Features:
;;
;; * Full name handling:
;;
;;   * knows where full names can be found in an address.
;;   * avoids using empty comments and quoted text.
;;   * extracts full names from mailbox names.
;;   * recognizes common formats for comments after a full name.
;;   * puts a period and a space after each initial.
;;   * understands & referring to the mailbox name, capitalized.
;;   * strips name prefixes like "Prof.", etc.
;;   * understands what characters can occur in names (not just letters).
;;   * figures out middle initial from mailbox name.
;;   * removes funny nicknames.
;;   * keeps suffixes such as Jr., Sr., III, etc.
;;   * reorders "Last, First" type names.
;;
;; * Address handling:
;;
;;   * parses rfc822 quoted text, comments, and domain literals.
;;   * parses rfc822 multi-line headers.
;;   * does something reasonable with rfc822 GROUP addresses.
;;   * handles many rfc822 noncompliant and garbage addresses.
;;   * canonicalizes addresses (after stripping comments/phrases outside <>).
;;     * converts ! addresses into .UUCP and %-style addresses.
;;     * converts rfc822 ROUTE addresses to %-style addresses.
;;     * truncates %-style addresses at leftmost fully qualified domain name.
;;     * handles local relative precedence of ! vs. % and @ (untested).
;;
;; It does almost no string creation.  It primarily uses the built-in
;; parsing routines with the appropriate syntax tables.  This should
;; result in greater speed.
;;
;; TODO:
;;
;; * handle all test cases.  (This will take forever.)
;; * software to pick the correct header to use (eg., "Senders-Name:").
;; * multiple addresses in the "From:" header (almost all of the necessary
;;   code is there).
;; * flag to not treat `,' as an address separator.  (This is useful when
;;   there is a "From:" header but no "Sender:" header, because then there
;;   is only allowed to be one address.)
;; * mailbox name does not necessarily contain full name.
;; * fixing capitalization when it's all upper or lowercase.  (Hard!)
;; * some of the domain literal handling is missing.  (But I've never even
;;   seen one of these in a mail address, so maybe no big deal.)
;; * arrange to have syntax tables byte-compiled.
;; * speed hacks.
;; * delete unused variables.
;; * arrange for testing with different relative precedences of ! vs. @
;;   and %.
;; * insert documentation strings!
;; * handle X.400-gatewayed addresses according to RFC 1148.

;;; Change Log:
;;
;; Thu Feb 17 17:57:33 1994  Jamie Zawinski (jwz@lucid.com)
;;
;;	* merged with jbw's latest version
;;
;; Wed Feb  9 21:56:27 1994  Jamie Zawinski (jwz@lucid.com)
;;
;;      * high-bit chars in comments weren't treated as word syntax
;;
;; Sat Feb  5 03:13:40 1994  Jamie Zawinski (jwz@lucid.com)
;;
;;      * call replace-match with fixed-case arg
;;
;; Thu Dec 16 21:56:45 1993  Jamie Zawinski (jwz@lucid.com)
;;
;;      * some more cleanup, doc, added provide
;;
;; Tue Mar 23 21:23:18 1993  Joe Wells  (jbw at csd.bu.edu)
;;
;; 	* Made mail-full-name-prefixes a user-customizable variable.
;;        Allow passing the address as a buffer as well as a string.
;;        Allow [ and ] as name characters (Finnish character set).
;;
;; Mon Mar 22 21:20:56 1993  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Handle "null" addresses.  Handle = used for spacing in mailbox
;; 	  name.  Fix bug in handling of ROUTE-ADDR-type addresses that are
;; 	  missing their brackets.  Handle uppercase "JR".  Extract full
;; 	  names from X.400 addresses encoded in RFC-822.  Fix bug in
;;        handling of multiple addresses where first has trailing comment.
;;        Handle more kinds of telephone extension lead-ins.
;;
;; Mon Mar 22 20:16:57 1993  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Handle HZ encoding for embedding GB encoded chinese characters.
;;
;; Mon Mar 22 00:46:12 1993  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Fixed too broad matching of ham radio call signs.  Fixed bug in
;; 	  handling an unmatched ' in a name string.  Enhanced recognition
;; 	  of when . in the mailbox name terminates the name portion.
;; 	  Narrowed conversion of . to space to only the necessary
;; 	  situation.  Deal with VMS's stupid date stamps.  Handle a unique
;; 	  way of introducing an alternate address.  Fixed spacing bug I
;; 	  introduced in switching last name order.  Fixed bug in handling
;; 	  address with ! and % but no @.  Narrowed the cases in which
;; 	  certain trailing words are discarded.
;;
;; Sun Mar 21 21:41:06 1993  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Fixed bugs in handling GROUP addresses.  Certain words in the
;; 	  middle of a name no longer terminate it.  Handle LISTSERV list
;;        names.  Ignore comment field containing mailbox name.
;;
;; Sun Mar 21 14:39:38 1993  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Moved variant-method code back into main function.  Handle
;; 	underscores as spaces in comments.  Handle leading nickname.  Add
;; 	flag to ignore single-word names.  Other changes.
;;
;; Mon Feb  1 22:23:31 1993  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Added in changes by Rod Whitby and Jamie Zawinski.  This
;;        includes the flag mail-extr-guess-middle-initial and the fix for
;;        handling multiple addresses correctly.  (Whitby just changed
;;	  a > to a <.)
;;
;; Mon Apr  6 23:59:09 1992  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Cleaned up some more.  Release version 1.0 to world.
;;
;; Sun Apr  5 19:39:08 1992  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Cleaned up full name extraction extensively.
;;
;; Sun Feb  2 14:45:24 1992  Joe Wells  (jbw at bigbird.bu.edu)
;;
;; 	* Total rewrite.  Integrated mail-canonicalize-address into
;; 	mail-extract-address-components.  Now handles GROUP addresses more
;; 	or less correctly.  Better handling of lots of different cases.
;;
;; Fri Jun 14 19:39:50 1991
;;	* Created.

;;; Code:


(defgroup mail-extr nil
  "Extract full name and address from RFC 822 mail header."
  :prefix "mail-extr-"
  :group 'mail)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User configuration variable definitions.
;;

(defcustom mail-extr-guess-middle-initial nil
  "*Whether to try to guess middle initial from mail address.
If true, then when we see an address like \"John Smith <jqs@host.com>\"
we will assume that \"John Q. Smith\" is the fellow's name."
  :type 'boolean
  :group 'mail-extr)

(defcustom mail-extr-ignore-single-names nil
  "*Whether to ignore a name that is just a single word.
If true, then when we see an address like \"Idiot <dumb@stupid.com>\"
we will act as though we couldn't find a full name in the address."
  :type 'boolean
  :version "22.1"
  :group 'mail-extr)

(defcustom mail-extr-ignore-realname-equals-mailbox-name t
"*Whether to ignore a name that is equal to the mailbox name.
If true, then when the address is like \"Single <single@address.com>\"
we will act as though we couldn't find a full name in the address."
  :type 'boolean
  :group 'mail-extr)

;; Matches a leading title that is not part of the name (does not
;; contribute to uniquely identifying the person).
(defcustom mail-extr-full-name-prefixes
  (purecopy
   "\\(Prof\\|D[Rr]\\|Mrs?\\|Rev\\|Rabbi\\|SysOp\\|LCDR\\)\\.?[ \t\n]")
  "*Matches prefixes to the full name that identify a person's position.
These are stripped from the full name because they do not contribute to
uniquely identifying the person."
  :type 'regexp
  :group 'mail-extr)

(defcustom mail-extr-@-binds-tighter-than-! nil
  "*Whether the local mail transport agent looks at ! before @."
  :type 'boolean
  :group 'mail-extr)

(defcustom mail-extr-mangle-uucp nil
  "*Whether to throw away information in UUCP addresses
by translating things like \"foo!bar!baz@host\" into \"baz@bar.UUCP\"."
  :type 'boolean
  :group 'mail-extr)

;;----------------------------------------------------------------------
;; what orderings are meaningful?????
;;(defvar mail-operator-precedence-list '(?! ?% ?@))
;; Right operand of a % or a @ must be a domain name, period.  No other
;; operators allowed.  Left operand of a @ is an address relative to that
;; site.

;; Left operand of a ! must be a domain name.  Right operand is an
;; arbitrary address.
;;----------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constant definitions.
;;

;; Any character that can occur in a name, not counting characters that
;; separate parts of a multipart name (hyphen and period).
;; Yes, there are weird people with digits in their names.
;; You will also notice the consideration for the
;; Swedish/Finnish/Norwegian character set.
(defconst mail-extr-all-letters-but-separators
  (purecopy "][[:alnum:]{|}'~`"))

;; Any character that can occur in a name in an RFC822 address including
;; the separator (hyphen and possibly period) for multipart names.
;; #### should . be in here?
(defconst mail-extr-all-letters
  (purecopy (concat mail-extr-all-letters-but-separators "---")))

;; Any character that can start a name.
;; Keep this set as minimal as possible.
(defconst mail-extr-first-letters (purecopy "[:alpha:]"))

;; Any character that can end a name.
;; Keep this set as minimal as possible.
(defconst mail-extr-last-letters (purecopy "[:alpha:]`'."))

(defconst mail-extr-leading-garbage "\\W+")

;; (defconst mail-extr-non-name-chars
;;   (purecopy (concat "^" mail-extr-all-letters ".")))
;; (defconst mail-extr-non-begin-name-chars
;;   (purecopy (concat "^" mail-extr-first-letters)))
;; (defconst mail-extr-non-end-name-chars
;;   (purecopy (concat "^" mail-extr-last-letters)))

;; Matches an initial not followed by both a period and a space.
;; (defconst mail-extr-bad-initials-pattern
;;   (purecopy
;;    (format "\\(\\([^%s]\\|\\`\\)[%s]\\)\\(\\.\\([^ ]\\)\\| \\|\\([^%s .]\\)\\|\\'\\)"
;;            mail-extr-all-letters mail-extr-first-letters mail-extr-all-letters)))

;; Matches periods used instead of spaces.  Must not match the period
;; following an initial.
(defconst mail-extr-bad-dot-pattern
  (purecopy
   (format "\\([%s][%s]\\)\\.+\\([%s]\\)"
	   mail-extr-all-letters
	   mail-extr-last-letters
	   mail-extr-first-letters)))

;; Matches an embedded or leading nickname that should be removed.
;; (defconst mail-extr-nickname-pattern
;;   (purecopy
;;    (format "\\([ .]\\|\\`\\)[\"'`\[\(]\\([ .%s]+\\)[\]\"'\)] "
;;            mail-extr-all-letters)))

;; Matches the occurrence of a generational name suffix, and the last
;; character of the preceding name.  This is important because we want to
;; keep such suffixes: they help to uniquely identify the person.
;; *** Perhaps this should be a user-customizable variable.  However, the
;; *** regular expression is fairly tricky to alter, so maybe not.
(defconst mail-extr-full-name-suffix-pattern
  (purecopy
   (format
    "\\(,? ?\\([JjSs][Rr]\\.?\\|V?I+V?\\)\\)\\([^%s]\\([^%s]\\|\\'\\)\\|\\'\\)"
    mail-extr-all-letters mail-extr-all-letters)))

(defconst mail-extr-roman-numeral-pattern (purecopy "V?I+V?\\b"))

;; Matches a trailing uppercase (with other characters possible) acronym.
;; Must not match a trailing uppercase last name or trailing initial
(defconst mail-extr-weird-acronym-pattern
  (purecopy "\\([A-Z]+[-_/]\\|[A-Z][A-Z][A-Z]?\\b\\)"))

;; Matches a mixed-case or lowercase name (not an initial).
;; #### Match Latin1 lower case letters here too?
;; (defconst mail-extr-mixed-case-name-pattern
;;   (purecopy
;;    (format
;;     "\\b\\([a-z][%s]*[%s]\\|[%s][%s]*[a-z][%s]*[%s]\\|[%s][%s]*[a-z]\\)"
;;     mail-extr-all-letters mail-extr-last-letters
;;     mail-extr-first-letters mail-extr-all-letters mail-extr-all-letters
;;     mail-extr-last-letters mail-extr-first-letters mail-extr-all-letters)))

;; Matches a trailing alternative address.
;; #### Match Latin1 letters here too?
;; #### Match _ before @ here too?
(defconst mail-extr-alternative-address-pattern
  (purecopy "\\(aka *\\)?[a-zA-Z.]+[!@][a-zA-Z.]"))

;; Matches a variety of trailing comments not including comma-delimited
;; comments.
(defconst mail-extr-trailing-comment-start-pattern
  (purecopy " [-{]\\|--\\|[+@#></\;]"))

;; Matches a name (not an initial).
;; This doesn't force a word boundary at the end because sometimes a
;; comment is separated by a `-' with no preceding space.
(defconst mail-extr-name-pattern
  (purecopy (format "\\b[%s][%s]*[%s]"
		    mail-extr-first-letters
		    mail-extr-all-letters
		    mail-extr-last-letters)))

(defconst mail-extr-initial-pattern
  (purecopy (format "\\b[%s]\\([. ]\\|\\b\\)" mail-extr-first-letters)))

;; Matches a single name before a comma.
;; (defconst mail-extr-last-name-first-pattern
;;   (purecopy (concat "\\`" mail-extr-name-pattern ",")))

;; Matches telephone extensions.
(defconst mail-extr-telephone-extension-pattern
  (purecopy
   "\\(\\([Ee]xt\\|\\|[Tt]ph\\|[Tt]el\\|[Xx]\\).?\\)? *\\+?[0-9][- 0-9]+"))

;; Matches ham radio call signs.
;; Help from: Mat Maessen N2NJZ <maessm@rpi.edu>, Mark Feit
;; <mark@era.com>, Michael Covington <mcovingt@ai.uga.edu>.
;; Examples: DX504 DX515 K5MRU K8DHK KA9WGN KD3FU KD6EUI KD6HBW
;; KE9TV KF0NV N1API N3FU N3GZE N3IGS N4KCC N7IKQ N9HHU W4YHF W6ANK WA2SUH
;; WB7VZI N2NJZ NR3G KJ4KK AB4UM AL7NI KH6OH WN3KBT N4TMI W1A N0NZO
(defconst mail-extr-ham-call-sign-pattern
  (purecopy "\\b\\(DX[0-9]+\\|[AKNW][A-Z]?[0-9][A-Z][A-Z]?[A-Z]?\\)"))

;; Possible trailing suffixes: "\\(/\\(KT\\|A[AEG]\\|[R0-9]\\)\\)?"
;; /KT == Temporary Technician (has CSC but not "real" license)
;; /AA == Temporary Advanced
;; /AE == Temporary Extra
;; /AG == Temporary General
;; /R  == repeater
;; /#  == stations operating out of home district
;; I don't include these in the regexp above because I can't imagine
;; anyone putting them with their name in an e-mail address.

;; Matches normal single-part name
(defconst mail-extr-normal-name-pattern
  (purecopy (format "\\b[%s][%s]+[%s]"
		    mail-extr-first-letters
		    mail-extr-all-letters-but-separators
		    mail-extr-last-letters)))

;; Matches a single word name.
;; (defconst mail-extr-one-name-pattern
;;   (purecopy (concat "\\`" mail-extr-normal-name-pattern "\\'")))

;; Matches normal two names with missing middle initial
;; The first name is not allowed to have a hyphen because this can cause
;; false matches where the "middle initial" is actually the first letter
;; of the second part of the first name.
(defconst mail-extr-two-name-pattern
  (purecopy
   (concat "\\`\\(" mail-extr-normal-name-pattern
	   "\\|" mail-extr-initial-pattern
	   "\\) +\\(" mail-extr-name-pattern "\\)\\(,\\|\\'\\)")))

(defconst mail-extr-listserv-list-name-pattern
  (purecopy "Multiple recipients of list \\([-A-Z]+\\)"))

(defconst mail-extr-stupid-vms-date-stamp-pattern
  (purecopy
   "[0-9][0-9]-[JFMASOND][aepuco][nbrylgptvc]-[0-9][0-9][0-9][0-9] [0-9]+ *"))

;;; HZ -- GB (PRC Chinese character encoding) in ASCII embedding protocol
;;
;; In ASCII mode, a byte is interpreted as an ASCII character, unless a '~' is
;; encountered. The character '~' is an escape character. By convention, it
;; must be immediately followed ONLY by '~', '{' or '\n' (<LF>), with the
;; following special meaning.
;;
;; o The escape sequence '~~' is interpreted as a '~'.
;; o The escape-to-GB sequence '~{' switches the mode from ASCII to GB.
;; o The escape sequence '~\n' is a line-continuation marker to be consumed
;;   with no output produced.
;;
;; In GB mode, characters are interpreted two bytes at a time as (pure) GB
;; codes until the escape-from-GB code '~}' is read. This code switches the
;; mode from GB back to ASCII.  (Note that the escape-from-GB code '~}'
;; ($7E7D) is outside the defined GB range.)
(defconst mail-extr-hz-embedded-gb-encoded-chinese-pattern
  (purecopy "~{\\([^~].\\|~[^\}]\\)+~}"))

;; The leading optional lowercase letters are for a bastardized version of
;; the encoding, as is the optional nature of the final slash.
(defconst mail-extr-x400-encoded-address-pattern
  (purecopy "[a-z]?[a-z]?\\(/[A-Za-z]+\\(\\.[A-Za-z]+\\)?=[^/]+\\)+/?\\'"))

(defconst mail-extr-x400-encoded-address-field-pattern-format
  (purecopy "/%s=\\([^/]+\\)\\(/\\|\\'\\)"))

(defconst mail-extr-x400-encoded-address-surname-pattern
  ;; S stands for Surname (family name).
  (purecopy
   (format mail-extr-x400-encoded-address-field-pattern-format "[Ss]")))

(defconst mail-extr-x400-encoded-address-given-name-pattern
  ;; G stands for Given name.
  (purecopy
   (format mail-extr-x400-encoded-address-field-pattern-format "[Gg]")))

(defconst mail-extr-x400-encoded-address-full-name-pattern
  ;; PN stands for Personal Name.  When used it represents the combination
  ;; of the G and S fields.
  ;; "The one system I used having this field asked it with the prompt
  ;; `Personal Name'.  But they mapped it into G and S on outgoing real
  ;; X.400 addresses.  As they mapped G and S into PN on incoming..."
  (purecopy
   (format mail-extr-x400-encoded-address-field-pattern-format "[Pp][Nn]")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Syntax tables used for quick parsing.
;;

(defconst mail-extr-address-syntax-table (make-syntax-table))
(defconst mail-extr-address-comment-syntax-table (make-syntax-table))
(defconst mail-extr-address-domain-literal-syntax-table (make-syntax-table))
(defconst mail-extr-address-text-comment-syntax-table (make-syntax-table))
(defconst mail-extr-address-text-syntax-table (make-syntax-table))
(mapc
 (lambda (pair)
   (let ((syntax-table (symbol-value (car pair))))
     (dolist (item (cdr pair))
       (if (eq 2 (length item))
	   ;; modifying syntax of a single character
	   (modify-syntax-entry (car item) (car (cdr item)) syntax-table)
	 ;; modifying syntax of a range of characters
	 (let ((char (nth 0 item))
	       (bound (nth 1 item))
	       (syntax (nth 2 item)))
	   (while (<= char bound)
	     (modify-syntax-entry char syntax syntax-table)
	     (setq char (1+ char))))))))
 '((mail-extr-address-syntax-table
    (?\000 ?\037 "w")			;control characters
    (?\040	 " ")			;SPC
    (?! ?~	 "w")			;printable characters
    (?\177	 "w")			;DEL
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\( ".")
    (?\) ".")
    (?<  ".")
    (?>  ".")
    (?@  ".")
    (?,  ".")
    (?\; ".")
    (?:  ".")
    (?\\ "\\")
    (?\" "\"")
    (?.  ".")
    (?\[ ".")
    (?\] ".")
    ;; % and ! aren't RFC822 characters, but it is convenient to pretend
    (?%  ".")
    (?!  ".") ;; this needs to be word-constituent when not in .UUCP mode
    )
   (mail-extr-address-comment-syntax-table
    (?\000 ?\377 "w")
    (?\040 " ")
    (?\240 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\( "\(\)")
    (?\) "\)\(")
    (?\\ "\\"))
   (mail-extr-address-domain-literal-syntax-table
    (?\000 ?\377 "w")
    (?\040 " ")
    (?\240 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\[ "\(\]")			;??????
    (?\] "\)\[")			;??????
    (?\\ "\\"))
   (mail-extr-address-text-comment-syntax-table
    (?\000 ?\377 "w")
    (?\040 " ")
    (?\240 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?\( "\(\)")
    (?\) "\)\(")
    (?\[ "\(\]")
    (?\] "\)\[")
    (?\{ "\(\}")
    (?\} "\)\{")
    (?\\ "\\")
    (?\" "\"")
    ;; (?\' "\)\`")
    ;; (?\` "\(\'")
    )
   (mail-extr-address-text-syntax-table
    (?\000 ?\177 ".")
    (?\200 ?\377 "w")
    (?\040 " ")
    (?\t " ")
    (?\r " ")
    (?\n " ")
    (?A ?Z "w")
    (?a ?z "w")
    (?-    "w")
    (?\}   "w")
    (?\{   "w")
    (?|    "w")
    (?\'   "w")
    (?~    "w")
    (?0 ?9 "w"))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions and macros.
;;

;; Fixme: There are Latin-1 nbsp below.  If such characters should be
;; included, this is the wrong thing to do -- it should use syntax (or
;; regexp char classes).

(defsubst mail-extr-skip-whitespace-forward ()
  ;; v19 fn skip-syntax-forward is more tasteful, but not byte-coded.
  (skip-chars-forward " \t\n\r "))

(defsubst mail-extr-skip-whitespace-backward ()
  ;; v19 fn skip-syntax-backward is more tasteful, but not byte-coded.
  (skip-chars-backward " \t\n\r "))


(defsubst mail-extr-undo-backslash-quoting (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; undo \ quoting
      (while (search-forward "\\" nil t)
	(delete-char -1)
	(or (eobp)
	    (forward-char 1))))))

(defsubst mail-extr-nuke-char-at (pos)
  (save-excursion
    (goto-char pos)
    (delete-char 1)
    (insert ?\ )))

(put 'mail-extr-nuke-outside-range
     'edebug-form-spec '(symbolp &optional form form atom))

(defmacro mail-extr-nuke-outside-range (list-symbol
					beg-symbol end-symbol
					&optional no-replace)
  "Delete all elements outside BEG..END in LIST.
LIST-SYMBOL names a variable holding a list of buffer positions
BEG-SYMBOL and END-SYMBOL name variables delimiting a range
Each element of LIST-SYMBOL which lies outside of the range is
 deleted from the list.
Unless NO-REPLACE is true, at each of the positions in LIST-SYMBOL
 which lie outside of the range, one character at that position is
 replaced with a SPC."
  (or (memq no-replace '(t nil))
      (error "no-replace must be t or nil, evaluable at macroexpand-time"))
  `(let ((temp ,list-symbol)
	   ch)
       (while temp
	 (setq ch (car temp))
	 (when (or (> ch ,end-symbol)
		   (< ch ,beg-symbol))
	   ,@(if no-replace
		   nil
		 `((mail-extr-nuke-char-at ch)))
	   (setcar temp nil))
	 (setq temp (cdr temp)))
       (setq ,list-symbol (delq nil ,list-symbol))))

(defun mail-extr-demarkerize (marker)
  ;; if arg is a marker, destroys the marker, then returns the old value.
  ;; otherwise returns the arg.
  (if (markerp marker)
      (let ((temp (marker-position marker)))
	(set-marker marker nil)
	temp)
    marker))

(defun mail-extr-markerize (pos)
  ;; coerces pos to a marker if non-nil.
  (if (or (markerp pos) (null pos))
      pos
    (copy-marker pos)))

(defsubst mail-extr-safe-move-sexp (arg)
  ;; Safely skip over one balanced sexp, if there is one.  Return t if success.
  (condition-case error
      (progn
	(goto-char (or (scan-sexps (point) arg) (point)))
	t)
    (error
     ;; #### kludge kludge kludge kludge kludge kludge kludge !!!
     (if (string-equal (nth 1 error) "Unbalanced parentheses")
	 nil
       (while t
	 (signal (car error) (cdr error)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The main function to grind addresses
;;

(defvar disable-initial-guessing-flag)	; dynamic assignment
(defvar mailextr-cbeg)			; dynamic assignment
(defvar mailextr-cend)			; dynamic assignment
(defvar mail-extr-all-top-level-domains) ; Defined below.

;;;###autoload
(defun mail-extract-address-components (address &optional all)
  "Given an RFC-822 address ADDRESS, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).  If no
name can be extracted, FULL-NAME will be nil.  Also see
`mail-extr-ignore-single-names' and
`mail-extr-ignore-realname-equals-mailbox-name'.

If the optional argument ALL is non-nil, then ADDRESS can contain zero
or more recipients, separated by commas, and we return a list of
the form ((FULL-NAME CANONICAL-ADDRESS) ...) with one element for
each recipient.  If ALL is nil, then if ADDRESS contains more than
one recipients, all but the first is ignored.

ADDRESS may be a string or a buffer.  If it is a buffer, the visible
\(narrowed) portion of the buffer will be interpreted as the address.
\(This feature exists so that the clever caller might be able to avoid
consing a string.)"
  (let ((canonicalization-buffer (get-buffer-create " *canonical address*"))
	(extraction-buffer (get-buffer-create " *extract address components*"))
	value-list)

    (with-current-buffer (get-buffer-create extraction-buffer)
      (fundamental-mode)
      (buffer-disable-undo extraction-buffer)
      (set-syntax-table mail-extr-address-syntax-table)
      (widen)
      (erase-buffer)
      (setq case-fold-search nil)

      ;; Insert extra space at beginning to allow later replacement with <
      ;; without having to move markers.
      (insert ?\ )

      ;; Insert the address itself.
      (cond ((stringp address)
	     (insert address))
	    ((bufferp address)
	     (insert-buffer-substring address))
	    (t
	     (error "Invalid address: %s" address)))

      (set-text-properties (point-min) (point-max) nil)

      (with-current-buffer (get-buffer-create canonicalization-buffer)
	(fundamental-mode)
	(buffer-disable-undo canonicalization-buffer)
	(setq case-fold-search nil))


      ;; Unfold multiple lines.
      (goto-char (point-min))
      (while (re-search-forward "\\([^\\]\\(\\\\\\\\\\)*\\)\n[ \t]" nil t)
	(replace-match "\\1 " t))

      ;; Loop over addresses until we have as many as we want.
      (while (and (or all (null value-list))
		  (progn (goto-char (point-min))
			 (skip-chars-forward " \t")
			 (not (eobp))))
	(let (char
	      end-of-address
	      <-pos >-pos @-pos colon-pos comma-pos !-pos %-pos \;-pos
	      group-:-pos group-\;-pos route-addr-:-pos
	      record-pos-symbol
	      first-real-pos last-real-pos
	      phrase-beg phrase-end
	      ;; Dynamically set in mail-extr-voodoo.
	      mailextr-cbeg mailextr-cend
	      quote-beg quote-end
	      atom-beg atom-end
	      mbox-beg mbox-end
	      \.-ends-name
	      temp
	      ;;	name-suffix
	      fi mi li			; first, middle, last initial
	      saved-%-pos saved-!-pos saved-@-pos
	      domain-pos \.-pos insert-point
	      ;;	mailbox-name-processed-flag
	      disable-initial-guessing-flag) ; dynamically set from -voodoo

	  (set-syntax-table mail-extr-address-syntax-table)
	  (goto-char (point-min))

	  ;; Insert extra space at beginning to allow later replacement with <
	  ;; without having to move markers.
	  (or (eq (following-char) ?\ )
	      (insert ?\ ))

	  ;; First pass grabs useful information about address.
	  (while (progn
		   (mail-extr-skip-whitespace-forward)
		   (not (eobp)))
	    (setq char (char-after (point)))
	    (or first-real-pos
		(if (not (eq char ?\())
		    (setq first-real-pos (point))))
	    (cond
	     ;; comment
	     ((eq char ?\()
	      (set-syntax-table mail-extr-address-comment-syntax-table)
	      ;; only record the first non-empty comment's position
	      (if (and (not mailextr-cbeg)
		       (save-excursion
			 (forward-char 1)
			 (mail-extr-skip-whitespace-forward)
			 (not (eq ?\) (char-after (point))))))
		  (setq mailextr-cbeg (point)))
	      ;; TODO: don't record if unbalanced
	      (or (mail-extr-safe-move-sexp 1)
		  (forward-char 1))
	      (set-syntax-table mail-extr-address-syntax-table)
	      (if (and mailextr-cbeg
		       (not mailextr-cend))
		  (setq mailextr-cend (point))))
	     ;; quoted text
	     ((eq char ?\")
	      ;; only record the first non-empty quote's position
	      (if (and (not quote-beg)
		       (save-excursion
			 (forward-char 1)
			 (mail-extr-skip-whitespace-forward)
			 (not (eq ?\" (char-after (point))))))
		  (setq quote-beg (point)))
	      ;; TODO: don't record if unbalanced
	      (or (mail-extr-safe-move-sexp 1)
		  (forward-char 1))
	      (if (and quote-beg
		       (not quote-end))
		  (setq quote-end (point))))
	     ;; domain literals
	     ((eq char ?\[)
	      (set-syntax-table mail-extr-address-domain-literal-syntax-table)
	      (or (mail-extr-safe-move-sexp 1)
		  (forward-char 1))
	      (set-syntax-table mail-extr-address-syntax-table))
	     ;; commas delimit addresses when outside < > pairs.
	     ((and (eq char ?,)
		   (or (and (null <-pos)
			    ;; Handle ROUTE-ADDR address that is missing its <.
			    (not (eq ?@ (char-after (1+ (point))))))
		       (and >-pos
			    ;; handle weird munged addresses
			    ;; BUG FIX: This test was reversed.  Thanks to the
			    ;; brilliant Rod Whitby <rwhitby@research.canon.oz.au>
			    ;; for discovering this!
			    (< (car (last <-pos)) (car >-pos)))))
	      ;; The argument contains more than one address.
	      ;; Temporarily hide everything after this one.
	      (setq end-of-address (copy-marker (1+ (point)) t))
	      (narrow-to-region (point-min) (1+ (point)))
	      (delete-char 1)
	      (setq char ?\() ; HAVE I NO SHAME??
	      )
	     ;; record the position of various interesting chars, determine
	     ;; validity later.
	     ((setq record-pos-symbol
		    (cdr (assq char
			       '((?< . <-pos) (?> . >-pos) (?@ . @-pos)
				 (?: . colon-pos) (?, . comma-pos) (?! . !-pos)
				 (?% . %-pos) (?\; . \;-pos)))))
	      (set record-pos-symbol
		   (cons (point) (symbol-value record-pos-symbol)))
	      (forward-char 1))
	     ((eq char ?.)
	      (forward-char 1))
	     ((memq char '(
			   ;; comment terminator invalid
			   ?\)
			   ;; domain literal terminator invalid
			   ?\]
			   ;; \ allowed only within quoted strings,
			   ;; domain literals, and comments
			   ?\\
			   ))
	      (mail-extr-nuke-char-at (point))
	      (forward-char 1))
	     (t
	      ;; Do `(forward-word 1)', recognizing non-ASCII characters
	      ;; except Latin-1 nbsp as words.
	      (while (progn
		       (skip-chars-forward "^\000-\177 ")
		       (and (not (eobp))
			    (eq ?w (char-syntax (char-after)))
			    (progn
			      (forward-word 1)
			      (and (not (eobp))
				   (> (char-after) ?\177)
				   (not (eq (char-after) ? )))))))))
	    (or (eq char ?\()
		;; At the end of first address of a multiple address header.
		(and (eq char ?,)
		     (eobp))
		(setq last-real-pos (point))))

	  ;; Use only the leftmost <, if any.  Replace all others with spaces.
	  (while (cdr <-pos)
	    (mail-extr-nuke-char-at (car <-pos))
	    (setq <-pos (cdr <-pos)))

	  ;; Use only the rightmost >, if any.  Replace all others with spaces.
	  (while (cdr >-pos)
	    (mail-extr-nuke-char-at (nth 1 >-pos))
	    (setcdr >-pos (nthcdr 2 >-pos)))

	  ;; If multiple @s and a :, but no < and >, insert around buffer.
	  ;; Example: @foo.bar.dom,@xxx.yyy.zzz:mailbox@aaa.bbb.ccc
	  ;; This commonly happens on the UUCP "From " line.  Ugh.
	  (when (and (> (length @-pos) 1)
		      (eq 1 (length colon-pos))	;TODO: check if between last two @s
		      (not \;-pos)
		      (not <-pos))
	    (goto-char (point-min))
	    (delete-char 1)
	    (setq <-pos (list (point)))
	    (insert ?<))

	  ;; If < but no >, insert > in rightmost possible position
	  (when (and <-pos (null >-pos))
	    (goto-char (point-max))
	    (setq >-pos (list (point)))
	    (insert ?>))

	  ;; If > but no <, replace > with space.
	  (when (and >-pos (null <-pos))
	    (mail-extr-nuke-char-at (car >-pos))
	    (setq >-pos nil))

	  ;; Turn >-pos and <-pos into non-lists
	  (setq >-pos (car >-pos)
		<-pos (car <-pos))

	  ;; Trim other punctuation lists of items outside < > pair to handle
	  ;; stupid MTAs.
	  (when <-pos			; don't need to check >-pos also
	    ;; handle bozo software that violates RFC 822 by sticking
	    ;; punctuation marks outside of a < > pair
	    (mail-extr-nuke-outside-range @-pos <-pos >-pos t)
	    ;; RFC 822 says nothing about these two outside < >, but
	    ;; remove those positions from the lists to make things
	    ;; easier.
	    (mail-extr-nuke-outside-range !-pos <-pos >-pos t)
	    (mail-extr-nuke-outside-range %-pos <-pos >-pos t))

	  ;; Check for : that indicates GROUP list and for : part of
	  ;; ROUTE-ADDR spec.
	  ;; Can't possibly be more than two :.  Nuke any extra.
	  (while colon-pos
	    (setq temp (car colon-pos)
		  colon-pos (cdr colon-pos))
	    (cond ((and <-pos >-pos
			(> temp <-pos)
			(< temp >-pos))
		   (if (or route-addr-:-pos
			   (< (length @-pos) 2)
			   (> temp (car @-pos))
			   (< temp (nth 1 @-pos)))
		       (mail-extr-nuke-char-at temp)
		     (setq route-addr-:-pos temp)))
		  ((or (not <-pos)
		       (and <-pos
			    (< temp <-pos)))
		   (setq group-:-pos temp))))

	  ;; Nuke any ; that is in or to the left of a < > pair or to the left
	  ;; of a GROUP starting :.  Also, there may only be one ;.
	  (while \;-pos
	    (setq temp (car \;-pos)
		  \;-pos (cdr \;-pos))
	    (cond ((and <-pos >-pos
			(> temp <-pos)
			(< temp >-pos))
		   (mail-extr-nuke-char-at temp))
		  ((and (or (not group-:-pos)
			    (> temp group-:-pos))
			(not group-\;-pos))
		   (setq group-\;-pos temp))))

	  ;; Nuke unmatched GROUP syntax characters.
	  (when (and group-:-pos (not group-\;-pos))
	    ;; *** Do I really need to erase it?
	    (mail-extr-nuke-char-at group-:-pos)
	    (setq group-:-pos nil))
	  (when (and group-\;-pos (not group-:-pos))
	    ;; *** Do I really need to erase it?
	    (mail-extr-nuke-char-at group-\;-pos)
	    (setq group-\;-pos nil))

	  ;; Handle junk like ";@host.company.dom" that sendmail adds.
	  ;; **** should I remember comment positions?
	  (when group-\;-pos
	    ;; this is fine for now
	    (mail-extr-nuke-outside-range !-pos group-:-pos group-\;-pos t)
	    (mail-extr-nuke-outside-range @-pos group-:-pos group-\;-pos t)
	    (mail-extr-nuke-outside-range %-pos group-:-pos group-\;-pos t)
	    (mail-extr-nuke-outside-range comma-pos group-:-pos group-\;-pos t)
	    (and last-real-pos
		 (> last-real-pos (1+ group-\;-pos))
		 (setq last-real-pos (1+ group-\;-pos)))
	    ;; *** This may be wrong:
	    (and mailextr-cend
		 (> mailextr-cend group-\;-pos)
		 (setq mailextr-cend nil
		       mailextr-cbeg nil))
	    (and quote-end
		 (> quote-end group-\;-pos)
		 (setq quote-end nil
		       quote-beg nil))
	    ;; This was both wrong and unnecessary:
	    ;;(narrow-to-region (point-min) group-\;-pos)

	    ;; *** The entire handling of GROUP addresses seems rather lame.
	    ;; *** It deserves a complete rethink, except that these addresses
	    ;; *** are hardly ever seen.
	    )

	  ;; Any commas must be between < and : of ROUTE-ADDR.  Nuke any
	  ;; others.
	  ;; Hell, go ahead and nuke all of the commas.
	  ;; **** This will cause problems when we start handling commas in
	  ;; the PHRASE part .... no it won't ... yes it will ... ?????
	  (mail-extr-nuke-outside-range comma-pos 1 1)

	  ;; can only have multiple @s inside < >.  The fact that some MTAs
	  ;; put de-bracketed ROUTE-ADDRs in the UUCP-style "From " line is
	  ;; handled above.

	  ;; Locate PHRASE part of ROUTE-ADDR.
	  (when <-pos
	    (goto-char <-pos)
	    (mail-extr-skip-whitespace-backward)
	    (setq phrase-end (point))
	    (goto-char (or ;;group-:-pos
			(point-min)))
	    (mail-extr-skip-whitespace-forward)
	    (if (< (point) phrase-end)
		(setq phrase-beg (point))
	      (setq phrase-end nil)))

	  ;; handle ROUTE-ADDRS with real ROUTEs.
	  ;; If there are multiple @s, then we assume ROUTE-ADDR syntax, and
	  ;; any % or ! must be semantically meaningless.
	  ;; TODO: do this processing into canonicalization buffer
	  (when route-addr-:-pos
	    (setq !-pos nil
		  %-pos nil
		  >-pos (copy-marker >-pos)
		  route-addr-:-pos (copy-marker route-addr-:-pos))
	    (goto-char >-pos)
	    (insert-before-markers ?X)
	    (goto-char (car @-pos))
	    (while (setq @-pos (cdr @-pos))
	      (delete-char 1)
	      (setq %-pos (cons (point-marker) %-pos))
	      (insert "%")
	      (goto-char (1- >-pos))
	      (save-excursion
		(insert-buffer-substring extraction-buffer
					 (car @-pos) route-addr-:-pos)
		(delete-region (car @-pos) route-addr-:-pos))
	      (or (cdr @-pos)
		  (setq saved-@-pos (list (point)))))
	    (setq @-pos saved-@-pos)
	    (goto-char >-pos)
	    (delete-char -1)
	    (mail-extr-nuke-char-at route-addr-:-pos)
	    (mail-extr-demarkerize route-addr-:-pos)
	    (setq route-addr-:-pos nil
		  >-pos (mail-extr-demarkerize >-pos)
		  %-pos (mapcar 'mail-extr-demarkerize %-pos)))

	  ;; de-listify @-pos
	  (setq @-pos (car @-pos))

	  ;; TODO: remove comments in the middle of an address

	  (with-current-buffer canonicalization-buffer
	    (widen)
	    (erase-buffer)
	    (insert-buffer-substring extraction-buffer)

	    (if <-pos
		(narrow-to-region (progn
				    (goto-char (1+ <-pos))
				    (mail-extr-skip-whitespace-forward)
				    (point))
				  >-pos)
	      (if (and first-real-pos last-real-pos)
		  (narrow-to-region first-real-pos last-real-pos)
		;; ****** Oh no!  What if the address is completely empty!
		;; *** Is this correct?
		(narrow-to-region (point-max) (point-max))))

	    (and @-pos %-pos
		 (mail-extr-nuke-outside-range %-pos (point-min) @-pos))
	    (and %-pos !-pos
		 (mail-extr-nuke-outside-range !-pos (point-min) (car %-pos)))
	    (and @-pos !-pos (not %-pos)
		 (mail-extr-nuke-outside-range !-pos (point-min) @-pos))

	    ;; Error condition:?? (and %-pos (not @-pos))

	    ;; WARNING: THIS CODE IS DUPLICATED BELOW.
	    (when (and %-pos (not @-pos))
	      (goto-char (car %-pos))
	      (delete-char 1)
	      (setq @-pos (point))
	      (insert "@")
	      (setq %-pos (cdr %-pos)))

	    (when (and mail-extr-mangle-uucp !-pos)
	      ;; **** I don't understand this save-restriction and the
	      ;; narrow-to-region inside it.  Why did I do that?
	      (save-restriction
		(cond ((and @-pos
			    mail-extr-@-binds-tighter-than-!)
		       (goto-char @-pos)
		       (setq %-pos (cons (point) %-pos)
			     @-pos nil)
		       (delete-char 1)
		       (insert "%")
		       (setq insert-point (point-max)))
		      (mail-extr-@-binds-tighter-than-!
		       (setq insert-point (point-max)))
		      (%-pos
		       (setq insert-point (car (last %-pos))
			     saved-%-pos (mapcar 'mail-extr-markerize %-pos)
			     %-pos nil
			     @-pos (mail-extr-markerize @-pos)))
		      (@-pos
		       (setq insert-point @-pos)
		       (setq @-pos (mail-extr-markerize @-pos)))
		      (t
		       (setq insert-point (point-max))))
		(narrow-to-region (point-min) insert-point)
		(setq saved-!-pos (car !-pos))
		(while !-pos
		  (goto-char (point-max))
		  (cond ((and (not @-pos)
			      (not (cdr !-pos)))
			 (setq @-pos (point))
			 (insert-before-markers "@ "))
			(t
			 (setq %-pos (cons (point) %-pos))
			 (insert-before-markers "% ")))
		  (backward-char 1)
		  (insert-buffer-substring
		   (current-buffer)
		   (if (nth 1 !-pos)
		       (1+ (nth 1 !-pos))
		     (point-min))
		   (car !-pos))
		  (delete-char 1)
		  (or (save-excursion
			(mail-extr-safe-move-sexp -1)
			(mail-extr-skip-whitespace-backward)
			(eq ?. (preceding-char)))
		      (insert-before-markers
		       (if (save-excursion
			     (mail-extr-skip-whitespace-backward)
			     (eq ?. (preceding-char)))
			   ""
			 ".")
		       "uucp"))
		  (setq !-pos (cdr !-pos))))
	      (and saved-%-pos
		   (setq %-pos (append (mapcar 'mail-extr-demarkerize
					       saved-%-pos)
				       %-pos)))
	      (setq @-pos (mail-extr-demarkerize @-pos))
	      (narrow-to-region (1+ saved-!-pos) (point-max)))

	    ;; WARNING: THIS CODE IS DUPLICATED ABOVE.
	    (when (and %-pos (not @-pos))
	      (goto-char (car %-pos))
	      (delete-char 1)
	      (setq @-pos (point))
	      (insert "@")
	      (setq %-pos (cdr %-pos)))

	    (when (setq %-pos (nreverse %-pos))	; implies @-pos valid
	      (setq temp %-pos)
	      (catch 'truncated
		(while temp
		  (goto-char (or (nth 1 temp)
				 @-pos))
		  (mail-extr-skip-whitespace-backward)
		  (save-excursion
		    (mail-extr-safe-move-sexp -1)
		    (setq domain-pos (point))
		    (mail-extr-skip-whitespace-backward)
		    (setq \.-pos (eq ?. (preceding-char))))
		  (when (and \.-pos
			     ;; #### string consing
			     (let ((s (intern-soft
				       (buffer-substring domain-pos (point))
				       mail-extr-all-top-level-domains)))
			       (and s (get s 'domain-name))))
		    (narrow-to-region (point-min) (point))
		    (goto-char (car temp))
		    (delete-char 1)
		    (setq @-pos (point))
		    (setcdr temp nil)
		    (setq %-pos (delq @-pos %-pos))
		    (insert "@")
		    (throw 'truncated t))
		  (setq temp (cdr temp)))))
	    (setq mbox-beg (point-min)
		  mbox-end (if %-pos (car %-pos)
			     (or @-pos
				 (point-max))))

	    (when @-pos
	      ;; Make the domain-name part lowercase since it's case
	      ;; insensitive anyway.
	      (downcase-region (1+ @-pos) (point-max))))

	  ;; Done canonicalizing address.
	  ;; We are now back in extraction-buffer.

	  ;; Decide what part of the address to search to find the full name.
	  (cond (
		 ;; Example: "First M. Last" <fml@foo.bar.dom>
		 (and phrase-beg
		      (eq quote-beg phrase-beg)
		      (<= quote-end phrase-end))
		 (narrow-to-region (1+ quote-beg) (1- quote-end))
		 (mail-extr-undo-backslash-quoting (point-min) (point-max)))

		;; Example: First Last <fml@foo.bar.dom>
		(phrase-beg
		 (narrow-to-region phrase-beg phrase-end))

		;; Example: fml@foo.bar.dom (First M. Last)
		(mailextr-cbeg
		 (narrow-to-region (1+ mailextr-cbeg) (1- mailextr-cend))
		 (mail-extr-undo-backslash-quoting (point-min) (point-max))

		 ;; Deal with spacing problems
		 (goto-char (point-min))
;;;	     (cond ((not (search-forward " " nil t))
;;;		    (goto-char (point-min))
;;;		    (cond ((search-forward "_" nil t)
;;;			   ;; Handle the *idiotic* use of underlines as spaces.
;;;			   ;; Example: fml@foo.bar.dom (First_M._Last)
;;;			   (goto-char (point-min))
;;;			   (while (search-forward "_" nil t)
;;;			     (replace-match " " t)))
;;;			  ((search-forward "." nil t)
;;;			   ;; Fix . used as space
;;;			   ;; Example: danj1@cb.att.com (daniel.jacobson)
;;;			   (goto-char (point-min))
;;;			   (while (re-search-forward mail-extr-bad-dot-pattern nil t)
;;;			     (replace-match "\\1 \\2" t))))))
		 )

		;; Otherwise we try to get the name from the mailbox portion
		;; of the address.
		;; Example: First_M_Last@foo.bar.dom
		(t
		 ;; *** Work in canon buffer instead?  No, can't.  Hmm.
		 (goto-char (point-max))
		 (narrow-to-region (point) (point))
		 (insert-buffer-substring canonicalization-buffer
					  mbox-beg mbox-end)
		 (goto-char (point-min))

		 ;; Example: First_Last.XXX@foo.bar.dom
		 (setq \.-ends-name (re-search-forward "[_0-9]" nil t))

		 (goto-char (point-min))

		 (if (not mail-extr-mangle-uucp)
		     (modify-syntax-entry ?! "w" (syntax-table)))

		 (while (progn
			  (mail-extr-skip-whitespace-forward)
			  (not (eobp)))
		   (setq char (char-after (point)))
		   (cond
		    ((eq char ?\")
		     (setq quote-beg (point))
		     (or (mail-extr-safe-move-sexp 1)
			 ;; TODO: handle this error condition!!!!!
			 (forward-char 1))
		     ;; take into account deletions
		     (setq quote-end (- (point) 2))
		     (save-excursion
		       (backward-char 1)
		       (delete-char 1)
		       (goto-char quote-beg)
		       (or (eobp)
			   (delete-char 1)))
		     (mail-extr-undo-backslash-quoting quote-beg quote-end)
		     (or (eq ?\  (char-after (point)))
			 (insert " "))
		     ;;		 (setq mailbox-name-processed-flag t)
		     (setq \.-ends-name t))
		    ((eq char ?.)
		     (if (memq (char-after (1+ (point))) '(?_ ?=))
			 (progn
			   (forward-char 1)
			   (delete-char 1)
			   (insert ?\ ))
		       (if \.-ends-name
			   (narrow-to-region (point-min) (point))
			 (delete-char 1)
			 (insert " ")))
		     ;;		 (setq mailbox-name-processed-flag t)
		     )
		    ((memq (char-syntax char) '(?. ?\\))
		     (delete-char 1)
		     (insert " ")
		     ;;		 (setq mailbox-name-processed-flag t)
		     )
		    (t
		     (setq atom-beg (point))
		     (forward-word 1)
		     (setq atom-end (point))
		     (goto-char atom-beg)
		     (save-restriction
		       (narrow-to-region atom-beg atom-end)
		       (cond

			;; Handle X.400 addresses encoded in RFC-822.
			;; *** Shit!  This has to handle the case where it is
			;; *** embedded in a quote too!
			;; *** Shit!  The input is being broken up into atoms
			;; *** by periods!
			((looking-at mail-extr-x400-encoded-address-pattern)

			 ;; Copy the contents of the individual fields that
			 ;; might hold name data to the beginning.
			 (mapc
			  (lambda (field-pattern)
			    (when
				(save-excursion
				  (re-search-forward field-pattern nil t))
			      (insert-buffer-substring (current-buffer)
						       (match-beginning 1)
						       (match-end 1))
			      (insert " ")))
			  (list mail-extr-x400-encoded-address-given-name-pattern
				mail-extr-x400-encoded-address-surname-pattern
				mail-extr-x400-encoded-address-full-name-pattern))

			 ;; Discard the rest, since it contains stuff like
			 ;; routing information, not part of a name.
			 (mail-extr-skip-whitespace-backward)
			 (delete-region (point) (point-max))

			 ;; Handle periods used for spacing.
			 (while (re-search-forward mail-extr-bad-dot-pattern nil t)
			   (replace-match "\\1 \\2" t))

			 ;;		     (setq mailbox-name-processed-flag t)
			 )

			;; Handle normal addresses.
			(t
			 (goto-char (point-min))
			 ;; Handle _ and = used for spacing.
			 (while (re-search-forward "\\([^_=]+\\)[_=]" nil t)
			   (replace-match "\\1 " t)
			   ;;		       (setq mailbox-name-processed-flag t)
			   )
			 (goto-char (point-max))))))))

		 ;; undo the dirty deed
		 (if (not mail-extr-mangle-uucp)
		     (modify-syntax-entry ?! "." (syntax-table)))
		 ;;
		 ;; If we derived the name from the mailbox part of the address,
		 ;; and we only got one word out of it, don't treat that as a
		 ;; name.  "foo@bar" --> (nil "foo@bar"), not ("foo" "foo@bar")
		 ;; (if (not mailbox-name-processed-flag)
		 ;;     (delete-region (point-min) (point-max)))
		 ))

	  (set-syntax-table mail-extr-address-text-syntax-table)

	  (mail-extr-voodoo mbox-beg mbox-end canonicalization-buffer)
	  (goto-char (point-min))

	  ;; If name is "First Last" and userid is "F?L", then assume
	  ;; the middle initial is the second letter in the userid.
	  ;; Initial code by Jamie Zawinski <jwz@lucid.com>
	  ;; *** Make it work when there's a suffix as well.
	  (goto-char (point-min))
	  (when (and mail-extr-guess-middle-initial
		     (not disable-initial-guessing-flag)
		     (eq 3 (- mbox-end mbox-beg))
		     (progn
		       (goto-char (point-min))
		       (looking-at mail-extr-two-name-pattern)))
	    (setq fi (char-after (match-beginning 0))
		  li (char-after (match-beginning 3)))
	    (with-current-buffer canonicalization-buffer
	      ;; char-equal is ignoring case here, so no need to upcase
	      ;; or downcase.
	      (let ((case-fold-search t))
		(and (char-equal fi (char-after mbox-beg))
		     (char-equal li (char-after (1- mbox-end)))
		     (setq mi (char-after (1+ mbox-beg))))))
	    (when (and mi
		       ;; TODO: use better table than syntax table
		       (eq ?w (char-syntax mi)))
	      (goto-char (match-beginning 3))
	      (insert (upcase mi) ". ")))

	  ;; Nuke name if it is the same as mailbox name.
	  (let ((buffer-length (- (point-max) (point-min)))
		(i 0)
		(names-match-flag t))
	    (when (and (> buffer-length 0)
		       (eq buffer-length (- mbox-end mbox-beg)))
	      (goto-char (point-max))
	      (insert-buffer-substring canonicalization-buffer
				       mbox-beg mbox-end)
	      (while (and names-match-flag
			  (< i buffer-length))
		(or (eq (downcase (char-after (+ i (point-min))))
			(downcase
			 (char-after (+ i buffer-length (point-min)))))
		    (setq names-match-flag nil))
		(setq i (1+ i)))
	      (delete-region (+ (point-min) buffer-length) (point-max))
	      (and names-match-flag
			   mail-extr-ignore-realname-equals-mailbox-name
			   (narrow-to-region (point) (point)))))

	  ;; Nuke name if it's just one word.
	  (goto-char (point-min))
	  (and mail-extr-ignore-single-names
	       (not (re-search-forward "[- ]" nil t))
	       (narrow-to-region (point) (point)))

	  ;; Record the result
	  (setq value-list
		(cons (list (if (not (= (point-min) (point-max)))
				(buffer-string))
			    (with-current-buffer canonicalization-buffer
			      (if (not (= (point-min) (point-max)))
				  (buffer-string))))
		      value-list))

	  ;; Unless one address is all we wanted,
	  ;; delete this one from extraction-buffer
	  ;; and get ready to extract the next address.
	  (when all
	    (if end-of-address
		(narrow-to-region 1 end-of-address)
	      (widen))
	    (delete-region (point-min) (point-max))
	    (widen))
	  )))
    (if all (nreverse value-list) (car value-list))
    ))

(defcustom mail-extr-disable-voodoo "\\cj"
  "*If it is a regexp, names matching it will never be modified.
If it is neither nil nor a string, modifying of names will never take
place.  It affects how `mail-extract-address-components' works."
  :type '(choice (regexp :size 0)
		 (const :tag "Always enabled" nil)
		 (const :tag "Always disabled" t))
  :group 'mail-extr)

(defun mail-extr-voodoo (mbox-beg mbox-end canonicalization-buffer)
  (unless (and mail-extr-disable-voodoo
	       (or (not (stringp mail-extr-disable-voodoo))
		   (progn
		     (goto-char (point-min))
		     (re-search-forward mail-extr-disable-voodoo nil t))))
    (let ((word-count 0)
	  (case-fold-search nil)
	  mixed-case-flag lower-case-flag ;;upper-case-flag
	  suffix-flag last-name-comma-flag
	  initial
	  begin-again-flag
	  drop-this-word-if-trailing-flag
	  drop-last-word-if-trailing-flag
	  word-found-flag
	  this-word-beg last-word-beg
	  name-beg name-end
	  name-done-flag
	  )
      (save-excursion
	(set-syntax-table mail-extr-address-text-syntax-table)

	;; Get rid of comments.
	(goto-char (point-min))
	(while (not (eobp))
	  ;; Initialize for this iteration of the loop.
	  (skip-chars-forward "^({[\"'`")
	  (let ((cbeg (point)))
	    (set-syntax-table mail-extr-address-text-comment-syntax-table)
	    (if (memq (following-char) '(?\' ?\`))
		(search-forward "'" nil 'move
				(if (eq ?\' (following-char)) 2 1))
	      (or (mail-extr-safe-move-sexp 1)
		  (goto-char (point-max))))
	    (set-syntax-table mail-extr-address-text-syntax-table)
	    (when (eq (char-after cbeg) ?\()
	      ;; Delete the comment itself.
	      (delete-region cbeg (point))
	      ;; Canonicalize whitespace where the comment was.
	      (skip-chars-backward " \t")
	      (if (looking-at "\\([ \t]+$\\|[ \t]+,\\)")
		  (replace-match "")
		(setq cbeg (point))
		(skip-chars-forward " \t")
		(if (bobp)
		    (delete-region (point) cbeg)
		  (just-one-space))))))

	;; This was moved above.
	;; Fix . used as space
	;; But it belongs here because it occurs not only as
	;;   rypens@reks.uia.ac.be (Piet.Rypens)
	;; but also as
	;;   "Piet.Rypens" <rypens@reks.uia.ac.be>
	;;(goto-char (point-min))
	;;(while (re-search-forward mail-extr-bad-dot-pattern nil t)
	;;  (replace-match "\\1 \\2" t))

	(unless (search-forward " " nil t)
	  (goto-char (point-min))
	  (cond ((search-forward "_" nil t)
		 ;; Handle the *idiotic* use of underlines as spaces.
		 ;; Example: fml@foo.bar.dom (First_M._Last)
		 (goto-char (point-min))
		 (while (search-forward "_" nil t)
		   (replace-match " " t)))
		((search-forward "." nil t)
		 ;; Fix . used as space
		 ;; Example: danj1@cb.att.com (daniel.jacobson)
		 (goto-char (point-min))
		 (while (re-search-forward mail-extr-bad-dot-pattern nil t)
		   (replace-match "\\1 \\2" t)))))

	;; Loop over the words (and other junk) in the name.
	(goto-char (point-min))
	(while (not name-done-flag)

	  (when word-found-flag
	    ;; Last time through this loop we skipped over a word.
	    (setq last-word-beg this-word-beg)
	    (setq drop-last-word-if-trailing-flag
		  drop-this-word-if-trailing-flag)
	    (setq word-found-flag nil))

	  (when begin-again-flag
	    ;; Last time through the loop we found something that
	    ;; indicates we should pretend we are beginning again from
	    ;; the start.
	    (setq word-count 0)
	    (setq last-word-beg nil)
	    (setq drop-last-word-if-trailing-flag nil)
	    (setq mixed-case-flag nil)
	    (setq lower-case-flag nil)
	    ;;	       (setq upper-case-flag nil)
	    (setq begin-again-flag nil))

	  ;; Initialize for this iteration of the loop.
	  (mail-extr-skip-whitespace-forward)
	  (if (eq word-count 0) (narrow-to-region (point) (point-max)))
	  (setq this-word-beg (point))
	  (setq drop-this-word-if-trailing-flag nil)

	  ;; Decide what to do based on what we are looking at.
	  (cond

	   ;; Delete title
	   ((and (eq word-count 0)
		 (looking-at mail-extr-full-name-prefixes))
	    (goto-char (match-end 0))
	    (narrow-to-region (point) (point-max)))

	   ;; Stop after name suffix
	   ((and (>= word-count 2)
		 (looking-at mail-extr-full-name-suffix-pattern))
	    (mail-extr-skip-whitespace-backward)
	    (setq suffix-flag (point))
	    (if (eq ?, (following-char))
		(forward-char 1)
	      (insert ?,))
	    ;; Enforce at least one space after comma
	    (or (eq ?\  (following-char))
		(insert ?\ ))
	    (mail-extr-skip-whitespace-forward)
	    (cond ((memq (following-char) '(?j ?J ?s ?S))
		   (capitalize-word 1)
		   (if (eq (following-char) ?.)
		       (forward-char 1)
		     (insert ?.)))
		  (t
		   (upcase-word 1)))
	    (setq word-found-flag t)
	    (setq name-done-flag t))

	   ;; Handle SCA names
	   ((looking-at "MKA \\(.+\\)")	; "Mundanely Known As"
	    (goto-char (match-beginning 1))
	    (narrow-to-region (point) (point-max))
	    (setq begin-again-flag t))

	   ;; Check for initial last name followed by comma
	   ((and (eq ?, (following-char))
		 (eq word-count 1))
	    (forward-char 1)
	    (setq last-name-comma-flag t)
	    (or (eq ?\  (following-char))
		(insert ?\ )))

	   ;; Stop before trailing comma-separated comment
	   ;; THIS CASE MUST BE AFTER THE PRECEDING CASES.
	   ;; *** This case is redundant???
	   ;;((eq ?, (following-char))
	   ;; (setq name-done-flag t))

	   ;; Delete parenthesized/quoted comment/nickname
	   ((memq (following-char) '(?\( ?\{ ?\[ ?\" ?\' ?\`))
	    (setq mailextr-cbeg (point))
	    (set-syntax-table mail-extr-address-text-comment-syntax-table)
	    (cond ((memq (following-char) '(?\' ?\`))
		   (or (search-forward "'" nil t
				       (if (eq ?\' (following-char)) 2 1))
		       (delete-char 1)))
		  (t
		   (or (mail-extr-safe-move-sexp 1)
		       (goto-char (point-max)))))
	    (set-syntax-table mail-extr-address-text-syntax-table)
	    (setq mailextr-cend (point))
	    (cond
	     ;; Handle case of entire name being quoted
	     ((and (eq word-count 0)
		   (looking-at " *\\'")
		   (>= (- mailextr-cend mailextr-cbeg) 2))
	      (narrow-to-region (1+ mailextr-cbeg) (1- mailextr-cend))
	      (goto-char (point-min)))
	     (t
	      ;; Handle case of quoted initial
	      (if (and (or (= 3 (- mailextr-cend mailextr-cbeg))
			   (and (= 4 (- mailextr-cend mailextr-cbeg))
				(eq ?. (char-after (+ 2 mailextr-cbeg)))))
		       (not (looking-at " *\\'")))
		  (setq initial (char-after (1+ mailextr-cbeg)))
		(setq initial nil))
	      (delete-region mailextr-cbeg mailextr-cend)
	      (if initial
		  (insert initial ". ")))))

	   ;; Handle *Stupid* VMS date stamps
	   ((looking-at mail-extr-stupid-vms-date-stamp-pattern)
	    (replace-match "" t))

	   ;; Handle Chinese characters.
	   ((looking-at mail-extr-hz-embedded-gb-encoded-chinese-pattern)
	    (goto-char (match-end 0))
	    (setq word-found-flag t))

	   ;; Skip initial garbage characters.
	   ;; THIS CASE MUST BE AFTER THE PRECEDING CASES.
	   ((and (eq word-count 0)
		 (looking-at mail-extr-leading-garbage))
	    (goto-char (match-end 0))
	    ;; *** Skip backward over these???
	    ;; (skip-chars-backward "& \"")
	    (narrow-to-region (point) (point-max)))

	   ;; Various stopping points
	   ((or

	     ;; Stop before ALL CAPS acronyms, if preceded by mixed-case
	     ;; words.  Example: XT-DEM.
	     (and (>= word-count 2)
		  mixed-case-flag
		  (looking-at mail-extr-weird-acronym-pattern)
		  (not (looking-at mail-extr-roman-numeral-pattern)))

	     ;; Stop before trailing alternative address
	     (looking-at mail-extr-alternative-address-pattern)

	     ;; Stop before trailing comment not introduced by comma
	     ;; THIS CASE MUST BE AFTER AN EARLIER CASE.
	     (looking-at mail-extr-trailing-comment-start-pattern)

	     ;; Stop before telephone numbers
	     (and (>= word-count 1)
		  (looking-at mail-extr-telephone-extension-pattern)))
	    (setq name-done-flag t))

	   ;; Delete ham radio call signs
	   ((looking-at mail-extr-ham-call-sign-pattern)
	    (delete-region (match-beginning 0) (match-end 0)))

	   ;; Fixup initials
	   ((looking-at mail-extr-initial-pattern)
	    (or (eq (following-char) (upcase (following-char)))
		(setq lower-case-flag t))
	    (forward-char 1)
	    (if (eq ?. (following-char))
		(forward-char 1)
	      (insert ?.))
	    (or (eq ?\  (following-char))
		(insert ?\ ))
	    (setq word-found-flag t))

	   ;; Handle BITNET LISTSERV list names.
	   ((and (eq word-count 0)
		 (looking-at mail-extr-listserv-list-name-pattern))
	    (narrow-to-region (match-beginning 1) (match-end 1))
	    (setq word-found-flag t)
	    (setq name-done-flag t))

	   ;; Handle & substitution, when & is last and is not first.
	   ((and (> word-count 0)
		 (eq ?\  (preceding-char))
		 (eq (following-char) ?&)
		 (eq (1+ (point)) (point-max)))
	    (delete-char 1)
	    (capitalize-region
	     (point)
	     (progn
	       (insert-buffer-substring canonicalization-buffer
					mbox-beg mbox-end)
	       (point)))
	    (setq disable-initial-guessing-flag t)
	    (setq word-found-flag t))

	   ;; Handle & between names, as in "Bob & Susie".
	   ((and (> word-count 0) (eq (following-char) ?\&))
	    (setq name-beg (point))
	    (setq name-end (1+ name-beg))
	    (setq word-found-flag t)
	    (goto-char name-end))

	   ;; Regular name words
	   ((looking-at mail-extr-name-pattern)
	    (setq name-beg (point))
	    (setq name-end (match-end 0))

	    ;; Certain words will be dropped if they are at the end.
	    (and (>= word-count 2)
		 (not lower-case-flag)
		 (or
		  ;; Trailing 4-or-more letter lowercase words preceded by
		  ;; mixed case or uppercase words will be dropped.
		  (looking-at "[[:lower:]]\\{4,\\}[ \t]*\\'")
		  ;; Drop a trailing word which is terminated with a period.
		  (eq ?. (char-after (1- name-end))))
		 (setq drop-this-word-if-trailing-flag t))

	    ;; Set the flags that indicate whether we have seen a lowercase
	    ;; word, a mixed case word, and an uppercase word.
	    (if (re-search-forward "[[:lower:]]" name-end t)
		(if (progn
		      (goto-char name-beg)
		      (re-search-forward "[[:upper:]]" name-end t))
		    (setq mixed-case-flag t)
		  (setq lower-case-flag t))
	      ;;	    (setq upper-case-flag t)
	      )

	    (goto-char name-end)
	    (setq word-found-flag t))

	   ;; Allow a number as a word, if it doesn't mean anything else.
	   ((looking-at "[0-9]+\\>")
	    (setq name-beg (point))
	    (setq name-end (match-end 0))
	    (goto-char name-end)
	    (setq word-found-flag t))

	   (t
	    (setq name-done-flag t)
	    ))

	  ;; Count any word that we skipped over.
	  (if word-found-flag
	      (setq word-count (1+ word-count))))

	;; If the last thing in the name is 2 or more periods, or one or more
	;; other sentence terminators (but not a single period) then keep them
	;; and the preceding word.  This is for the benefit of whole sentences
	;; in the name field: it's better behavior than dropping the last word
	;; of the sentence...
	(if (and (not suffix-flag)
		 (looking-at "\\(\\.+\\|[?!;:.][?!;:.]+\\|[?!;:][?!;:.]*\\)\\'"))
	    (goto-char (setq suffix-flag (point-max))))

	;; Drop everything after point and certain trailing words.
	(narrow-to-region (point-min)
			  (or (and drop-last-word-if-trailing-flag
				   last-word-beg)
			      (point)))

	;; Xerox's mailers SUCK!!!!!!
	;; We simply refuse to believe that any last name is PARC or ADOC.
	;; If it looks like that is the last name, that there is no meaningful
	;; here at all.  Actually I guess it would be best to map patterns
	;; like foo.hoser@xerox.com into foo@hoser.xerox.com, but I don't
	;; actually know that that is what's going on.
	(unless suffix-flag
	  (goto-char (point-min))
	  (let ((case-fold-search t))
	    (if (looking-at "[-A-Za-z_]+[. ]\\(PARC\\|ADOC\\)\\'")
		(erase-buffer))))

	;; If last name first put it at end (but before suffix)
	(when last-name-comma-flag
	  (goto-char (point-min))
	  (search-forward ",")
	  (setq name-end (1- (point)))
	  (goto-char (or suffix-flag (point-max)))
	  (or (eq ?\  (preceding-char))
	      (insert ?\ ))
	  (insert-buffer-substring (current-buffer) (point-min) name-end)
	  (goto-char name-end)
	  (skip-chars-forward "\t ,")
	  (narrow-to-region (point) (point-max)))

	;; Delete leading and trailing junk characters.
	;; *** This is probably completely unneeded now.
	;;(goto-char (point-max))
	;;(skip-chars-backward mail-extr-non-end-name-chars)
	;;(if (eq ?. (following-char))
	;;    (forward-char 1))
	;;(narrow-to-region (point)
	;;                  (progn
	;;                    (goto-char (point-min))
	;;                    (skip-chars-forward mail-extr-non-begin-name-chars)
	;;                    (point)))

	;; Compress whitespace
	(goto-char (point-min))
	(while (re-search-forward "[ \t\n]+" nil t)
	  (replace-match (if (eobp) "" " ") t))
	))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Table of top-level domain names.
;;
;; This is used during address canonicalization; be careful of format changes.
;; Keep in mind that the country abbreviations follow ISO-3166.  There is
;; a U.S. FIPS that specifies a different set of two-letter country
;; abbreviations.
;;
;; Updated by the RIPE Network Coordination Centre.
;;
;; Source: ISO 3166 Maintenance Agency
;; http://www.iso.org/iso/en/prods-services/iso3166ma/02iso-3166-code-lists/list-en1-semic.txt
;; http://www.iana.org/domain-names.htm
;; http://www.iana.org/cctld/cctld-whois.htm
;; Latest change: 2007/11/15

(defconst mail-extr-all-top-level-domains
  (let ((ob (make-vector 739 0)))
    (mapc
     (lambda (x)
       (put (intern (downcase (car x)) ob)
	    'domain-name
	    (if (nth 2 x)
		(format (nth 2 x) (nth 1 x))
	      (nth 1 x))))
     '(
       ;; ISO 3166 codes:
       ("ac" "Ascension Island")
       ("ad" "Andorra")
       ("ae" "United Arab Emirates")
       ("af" "Afghanistan")
       ("ag" "Antigua and Barbuda")
       ("ai" "Anguilla")
       ("al" "Albania")
       ("am" "Armenia")
       ("an" "Netherlands Antilles")
       ("ao" "Angola")
       ("aq" "Antarctica")		; continent
       ("ar" "Argentina"	"Argentine Republic")
       ("as" "American Samoa")
       ("at" "Austria"		"The Republic of %s")
       ("au" "Australia")
       ("aw" "Aruba")
       ("ax" "Aland Islands")
       ("az" "Azerbaijan")
       ("ba" "Bosnia-Herzegovina")
       ("bb" "Barbados")
       ("bd" "Bangladesh")
       ("be" "Belgium"		"The Kingdom of %s")
       ("bf" "Burkina Faso")
       ("bg" "Bulgaria")
       ("bh" "Bahrain")
       ("bi" "Burundi")
       ("bj" "Benin")
       ("bl" "Saint Barthelemy")
       ("bm" "Bermuda")
       ("bn" "Brunei Darussalam")
       ("bo" "Bolivia"		"Republic of %s")
       ("br" "Brazil"		"The Federative Republic of %s")
       ("bs" "Bahamas")
       ("bt" "Bhutan")
       ("bv" "Bouvet Island")
       ("bw" "Botswana")
       ("by" "Belarus")
       ("bz" "Belize")
       ("ca" "Canada")
       ("cc" "Cocos (Keeling) Islands")
       ("cd" "Congo"            "The Democratic Republic of the %s")
       ("cf" "Central African Republic")
       ("cg" "Congo")
       ("ch" "Switzerland"	"The Swiss Confederation")
       ("ci" "Ivory Coast")		; Cote D'ivoire
       ("ck" "Cook Islands")
       ("cl" "Chile"		"The Republic of %s")
       ("cm" "Cameroon")		; In .fr domain
       ("cn" "China"		"The People's Republic of %s")
       ("co" "Colombia")
       ("cr" "Costa Rica"	"The Republic of %s")
       ("cu" "Cuba")
       ("cv" "Cape Verde")
       ("cx" "Christmas Island")
       ("cy" "Cyprus")
       ("cz" "Czech Republic")
       ("de" "Germany")
       ("dj" "Djibouti")
       ("dk" "Denmark")
       ("dm" "Dominica")
       ("do" "Dominican Republic"	"The %s")
       ("dz" "Algeria")
       ("ec" "Ecuador"		"The Republic of %s")
       ("ee" "Estonia")
       ("eg" "Egypt"		"The Arab Republic of %s")
       ("eh" "Western Sahara")
       ("er" "Eritrea")
       ("es" "Spain"		"The Kingdom of %s")
       ("et" "Ethiopia")
       ("eu" "European Union")
       ("fi" "Finland"		"The Republic of %s")
       ("fj" "Fiji")
       ("fk" "Falkland Islands (Malvinas)")
       ("fm" "Micronesia"	"Federated States of %s")
       ("fo" "Faroe Islands")
       ("fr" "France")
       ("ga" "Gabon")
       ("gb" "United Kingdom")
       ("gd" "Grenada")
       ("ge" "Georgia")
       ("gf" "French Guiana")
       ("gg" "Guernsey")
       ("gh" "Ghana")
       ("gi" "Gibraltar")
       ("gl" "Greenland")
       ("gm" "Gambia")
       ("gn" "Guinea")
       ("gp" "Guadeloupe (Fr.)")
       ("gq" "Equatorial Guinea")
       ("gr" "Greece"		"The Hellenic Republic (%s)")
       ("gs" "South Georgia and The South Sandwich Islands")
       ("gt" "Guatemala")
       ("gu" "Guam (U.S.)")
       ("gw" "Guinea-Bissau")
       ("gy" "Guyana")
       ("hk" "Hong Kong")
       ("hm" "Heard Island and McDonald Islands")
       ("hn" "Honduras")
       ("hr" "Croatia"		"Croatia (Hrvatska)")
       ("ht" "Haiti")
       ("hu" "Hungary"		"The Hungarian Republic")
       ("id" "Indonesia")
       ("ie" "Ireland")
       ("il" "Israel"		"The State of %s")
       ("im" "Isle of Man"	"The %s") ; NOT in ISO 3166-1 of 2001-02-26
       ("in" "India"		"The Republic of %s")
       ("io" "British Indian Ocean Territory")
       ("iq" "Iraq")
       ("ir" "Iran"		"Islamic Republic of %s")
       ("is" "Iceland"		"The Republic of %s")
       ("it" "Italy"		"The Italian Republic")
       ("je" "Jersey")
       ("jm" "Jamaica")
       ("jo" "Jordan")
       ("jp" "Japan")
       ("ke" "Kenya")
       ("kg" "Kyrgyzstan")
       ("kh" "Cambodia")
       ("ki" "Kiribati")
       ("km" "Comoros")
       ("kn" "Saint Kitts and Nevis")
       ("kp" "Korea (North)"	"Democratic People's Republic of Korea")
       ("kr" "Korea (South)"	"Republic of Korea")
       ("kw" "Kuwait")
       ("ky" "Cayman Islands")
       ("kz" "Kazakhstan")
       ("la" "Lao People's Democratic Republic")
       ("lb" "Lebanon")
       ("lc" "Saint Lucia")
       ("li" "Liechtenstein")
       ("lk" "Sri Lanka"	"The Democratic Socialist Republic of %s")
       ("lr" "Liberia")
       ("ls" "Lesotho")
       ("lt" "Lithuania")
       ("lu" "Luxembourg")
       ("lv" "Latvia")
       ("ly" "Libyan Arab Jamahiriya")
       ("ma" "Morocco")
       ("mc" "Monaco")
       ("md" "Moldova"		"The Republic of %s")
       ("me" "Montenegro")
       ("mf" "Saint Martin (French part)")
       ("mg" "Madagascar")
       ("mh" "Marshall Islands")
       ("mk" "Macedonia"	"The Former Yugoslav Republic of %s")
       ("ml" "Mali")
       ("mm" "Myanmar")
       ("mn" "Mongolia")
       ("mo" "Macao")
       ("mp" "Northern Mariana Islands")
       ("mq" "Martinique")
       ("mr" "Mauritania")
       ("ms" "Montserrat")
       ("mt" "Malta")
       ("mu" "Mauritius")
       ("mv" "Maldives")
       ("mw" "Malawi")
       ("mx" "Mexico"		"The United Mexican States")
       ("my" "Malaysia")
       ("mz" "Mozambique")
       ("na" "Namibia")
       ("nc" "New Caledonia (Fr.)")
       ("ne" "Niger")			; In .fr domain
       ("nf" "Norfolk Island")
       ("ng" "Nigeria")
       ("ni" "Nicaragua"	"The Republic of %s")
       ("nl" "Netherlands"	"The Kingdom of the %s")
       ("no" "Norway"		"The Kingdom of %s")
       ("np" "Nepal")			; Via .in domain
       ("nr" "Nauru")
       ("nu" "Niue")
       ("nz" "New Zealand")
       ("om" "Oman")
       ("pa" "Panama")
       ("pe" "Peru")
       ("pf" "French Polynesia")
       ("pg" "Papua New Guinea")
       ("ph" "Philippines"	"The Republic of the %s")
       ("pk" "Pakistan")
       ("pl" "Poland")
       ("pm" "Saint Pierre and Miquelon")
       ("pn" "Pitcairn")
       ("pr" "Puerto Rico (U.S.)")
       ("ps" "Palestinian Territory, Occupied")
       ("pt" "Portugal"		"The Portuguese Republic")
       ("pw" "Palau")
       ("py" "Paraguay")
       ("qa" "Qatar")
       ("re" "Reunion (Fr.)")		; In .fr domain
       ("ro" "Romania")
       ("rs" "Serbia")
       ("ru" "Russia"		"Russian Federation")
       ("rw" "Rwanda")
       ("sa" "Saudi Arabia")
       ("sb" "Solomon Islands")
       ("sc" "Seychelles")
       ("sd" "Sudan")
       ("se" "Sweden"		"The Kingdom of %s")
       ("sg" "Singapore"	"The Republic of %s")
       ("sh" "Saint Helena")
       ("si" "Slovenia")
       ("sj" "Svalbard and Jan Mayen") ; In .no domain
       ("sk" "Slovakia"		"The Slovak Republic")
       ("sl" "Sierra Leone")
       ("sm" "San Marino")
       ("sn" "Senegal")
       ("so" "Somalia")
       ("sr" "Suriname")
       ("st" "Sao Tome and Principe")
       ("su" "U.S.S.R." "The Union of Soviet Socialist Republics")
       ("sv" "El Salvador")
       ("sy" "Syrian Arab Republic")
       ("sz" "Swaziland")
       ("tc" "Turks and Caicos Islands")
       ("td" "Chad")
       ("tf" "French Southern Territories")
       ("tg" "Togo")
       ("th" "Thailand"		"The Kingdom of %s")
       ("tj" "Tajikistan")
       ("tk" "Tokelau")
       ("tl" "East Timor")
       ("tm" "Turkmenistan")
       ("tn" "Tunisia")
       ("to" "Tonga")
       ("tp" "East Timor")
       ("tr" "Turkey"		"The Republic of %s")
       ("tt" "Trinidad and Tobago")
       ("tv" "Tuvalu")
       ("tw" "Taiwan"		"%s, Province of China")
       ("tz" "Tanzania"		"United Republic of %s")
       ("ua" "Ukraine")
       ("ug" "Uganda")
       ("uk" "United Kingdom"	"The %s of Great Britain and Northern Ireland")
       ("um" "United States Minor Outlying Islands")
       ("us" "United States"	"The %s of America")
       ("uy" "Uruguay"		"The Eastern Republic of %s")
       ("uz" "Uzbekistan")
       ("va" "Holy See (Vatican City State)")
       ("vc" "Saint Vincent and the Grenadines")
       ("ve" "Venezuela"	"The Republic of %s")
       ("vg" "Virgin Islands, British")
       ("vi" "Virgin Islands, U.S.")
       ("vn" "Vietnam")
       ("vu" "Vanuatu")
       ("wf" "Wallis and Futuna")
       ("ws" "Samoa")
       ("ye" "Yemen")
       ("yt" "Mayotte")
       ("yu" "Yugoslavia"	"Yugoslavia, AKA Serbia-Montenegro")
       ("za" "South Africa"	"The Republic of %s")
       ("zm" "Zambia")
       ("zw" "Zimbabwe"		"Republic of %s")
       ;; Generic Domains:
       ("aero" t                "Air Transport Industry")
       ("asia" t                "Pan-Asia and Asia Pacific community")
       ("biz" t                 "Businesses")
       ("cat" t                 "Catalan language and culture")
       ("com" t			"Commercial")
       ("coop" t                "Cooperative Associations")
       ("info" t                "Info")
       ("jobs" t                "Employment")
       ("mobi" t                "Mobile products")
       ("museum" t              "Museums")
       ("name" t                "Individuals")
       ("net" t			"Network")
       ("org" t			"Non-profit Organization")
       ("pro" t                 "Credentialed professionals")
       ("tel" t                 "Contact data")
       ("travel" t              "Travel industry")
       ;;("bitnet" t		"Because It's Time NET")
       ("gov" t			"United States Government")
       ("edu" t			"Educational")
       ("mil" t			"United States Military")
       ("int" t			"International Treaties")
       ;;("nato" t		"North Atlantic Treaty Organization")
       ("uucp" t		"Unix to Unix CoPy")
       ;; Infrastructure Domains:
       ("arpa" t		"Advanced Research Projects Agency (U.S. DoD)")
       ))
    ob))

;;;###autoload
(defun what-domain (domain)
  "Convert mail domain DOMAIN to the country it corresponds to."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Domain: "
			    mail-extr-all-top-level-domains nil t))))
  (or (setq domain (intern-soft (downcase domain)
				mail-extr-all-top-level-domains))
      (error "No such domain"))
  (message "%s: %s" (upcase (symbol-name domain)) (get domain 'domain-name)))


;(let ((all nil))
;  (mapatoms #'(lambda (x)
;		(if (and (boundp x)
;			 (string-match "^mail-extr-" (symbol-name x)))
;		    (setq all (cons x all)))))
;  (setq all (sort all #'string-lessp))
;  (cons 'setq
;	(apply 'nconc (mapcar #'(lambda (x)
;				  (list x (symbol-value x)))
;			      all))))


(provide 'mail-extr)

;;; mail-extr.el ends here
