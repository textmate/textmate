;;; snmp-mode.el --- SNMP & SNMPv2 MIB major mode

;; Copyright (C) 1995, 1998, 2001-2012 Free Software Foundation, Inc.

;; Author: Paul D. Smith <psmith@BayNetworks.com>
;; Keywords: data

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

;; INTRODUCTION
;; ------------
;; This package provides a major mode for editing SNMP MIBs.  It
;; provides all the modern Emacs 19 bells and whistles: default
;; fontification via font-lock, imenu search functions, etc.
;;
;; SNMP mode also uses tempo, a textual boilerplate insertion package
;; distributed with Emacs, to add in boilerplate SNMP MIB structures.
;; See tempo.el for more details about tempo.
;;
;; If you want to change or add new tempo templates, use the tempo tag
;; list `snmp-tempo-tags' (or `snmpv2-tempo-tags'): this list is
;; automatically installed when snmp-mode (or snmpv2-mode) is entered.
;;
;; The SNMPv2 mode in this version has been enhanced thanks to popular
;; demand.
;;
;; I'm very interested in new tempo macros for both v1 and v2, and any
;; other suggestions for enhancements (different syntax table items, new
;; keybindings, etc.)
;;
;;
;; USAGE
;; -----
;; Mostly, use it as you would any other mode.  There's a very
;; simplistic auto-indent feature; hopefully it'll help more than get in
;; your way.  For the most part it tries to indent to the same level as
;; the previous line.  It will try to recognize some very simple tokens
;; on the previous line that tell it to use extra indent or outdent.
;;
;; Templates
;; ---------
;; To use the Tempo templates, type the Tempo tag (or a unique prefix)
;; and use C-c C-i (C-c TAB) to complete it; if you don't have
;; tempo-interactive set to nil it will ask you to fill in values.
;; Fields with predefined values (SYNTAX, STATUS, etc.) will do
;; completing-reads on a list of valid values; use the normal SPC or TAB
;; to complete.
;;
;; Currently the following templates are available:
;;
;;  objectType -- Defines an OBJECT-TYPE macro.
;;
;;  tableType  -- Defines both a Table and Entry OBJECT-TYPE, and a
;;                SEQUENCE for the ASN.1 Entry definition.
;;
;; Once the template is done, you can use C-cC-f and C-cC-b to move back
;; and forth between the Tempo sequence points to fill in the rest of
;; the information.
;;
;; Font Lock
;; ------------
;;
;; If you want font-lock in your MIB buffers, add this:
;;
;;  (add-hook 'snmp-common-mode-hook 'turn-on-font-lock)
;;
;; Enabling global-font-lock-mode is also sufficient.
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'imenu)   ; Need this stuff when compiling for imenu macros, etc.
  (require 'tempo))

;;;----------------------------------------------------------------------------
;;
;;                          Customize these:
;;
;;;----------------------------------------------------------------------------

(defgroup snmp nil
  "Mode for editing SNMP MIB files."
  :group 'data
  :version "20.4")

(defcustom snmp-special-indent t
  "If non-nil, use a simple heuristic to try to guess the right indentation.
If nil, then no special indentation is attempted."
  :type 'boolean
  :group 'snmp)

(defcustom snmp-indent-level 4
  "Indentation level for SNMP MIBs."
  :type 'integer
  :group 'snmp)

(defcustom snmp-tab-always-indent nil
  "Non-nil means TAB should always reindent the current line.
A value of nil means reindent if point is within the initial line indentation;
otherwise insert a TAB."
  :type 'boolean
  :group 'snmp)

(defcustom snmp-completion-ignore-case t
  "Non-nil means that case differences are ignored during completion.
A value of nil means that case is significant.
This is used during Tempo template completion."
  :type 'boolean
  :group 'snmp)

(defcustom snmp-common-mode-hook nil
  "Hook(s) evaluated when a buffer enters either SNMP or SNMPv2 mode."
  :type 'hook
  :group 'snmp)

(defcustom snmp-mode-hook nil
  "Hook(s) evaluated when a buffer enters SNMP mode."
  :type 'hook
  :group 'snmp)

(defcustom snmpv2-mode-hook nil
  "Hook(s) evaluated when a buffer enters SNMPv2 mode."
  :type 'hook
  :group 'snmp)

(defvar snmp-tempo-tags nil
  "*Tempo tags for SNMP mode.")

(defvar snmpv2-tempo-tags nil
  "*Tempo tags for SNMPv2 mode.")


;; Enable fontification for SNMP MIBs
;;

;; These are pretty basic fontifications.  Note we assume these macros
;; are first on a line (except whitespace), to speed up fontification.
;;
(defvar snmp-font-lock-keywords-1
  (list
   ;; OBJECT-TYPE, TRAP-TYPE, and OBJECT-IDENTIFIER macros
   '("^[ \t]*\\([a-z][-a-zA-Z0-9]+\\)[ \t]+\\(\\(MODULE-\\(COMPLIANCE\\|IDENTITY\\)\\|OBJECT-\\(COMPLIANCE\\|GROUP\\|IDENTITY\\|TYPE\\)\\|TRAP-\\(GROUP\\|TYPE\\)\\)\\|\\(OBJECT\\)[ \t]+\\(IDENTIFIER\\)[ \t]*::=\\)"
     (1 font-lock-variable-name-face) (3 font-lock-keyword-face nil t)
     (7 font-lock-keyword-face nil t) (8 font-lock-keyword-face nil t))

   ;; DEFINITIONS clause
   '("^[ \t]*\\([A-Z][-a-zA-Z0-9]+\\)[ \t]+\\(DEFINITIONS\\)[ \t]*::="
     (1 font-lock-function-name-face) (2 font-lock-keyword-face))
   )
  "Basic SNMP MIB mode expression highlighting.")

(defvar snmp-font-lock-keywords-2
  (append
   '(("ACCESS\\|BEGIN\\|DE\\(FVAL\\|SCRIPTION\\)\\|END\\|FROM\\|I\\(MPORTS\\|NDEX\\)\\|S\\(TATUS\\|YNTAX\\)"
      (0 font-lock-keyword-face)))
   snmp-font-lock-keywords-1)
  "Medium SNMP MIB mode expression highlighting.")

(defvar snmp-font-lock-keywords-3
  (append
   '(("\\([^\n]+\\)[ \t]+::=[ \t]+\\(SEQUENCE\\)[ \t]+{"
      (1 font-lock-reference-face) (2 font-lock-keyword-face))
     ("::=[ \t]*{[ \t]*\\([a-z0-9].*[ \t]+\\)?\\([0-9]+\\)[ \t]*}"
      (1 font-lock-reference-face nil t) (2 font-lock-variable-name-face)))
   snmp-font-lock-keywords-2)
  "Gaudy SNMP MIB mode expression highlighting.")

(defvar snmp-font-lock-keywords snmp-font-lock-keywords-1
  "Default SNMP MIB mode expression highlighting.")


;; These lists are used for the completion capabilities in the tempo
;; templates.
;;

(defvar snmp-mode-syntax-list nil
  "Predefined types for SYNTAX clauses.")

(defvar snmp-rfc1155-types
  '("INTEGER" "OCTET STRING" "OBJECT IDENTIFIER" "NULL" "IpAddress"
    "NetworkAddress" "Counter" "Gauge" "TimeTicks" "Opaque")
  "Types from RFC 1155 v1 SMI.")

(defvar snmp-rfc1213-types
  '("DisplayString")
  "Types from RFC 1213 MIB-II.")

(defvar snmp-rfc1902-types
  '("INTEGER" "OCTET STRING" "OBJECT IDENTIFIER" "Integer32"
    "IpAddress" "Counter32" "Gauge32" "Unsigned32" "TimeTicks"
    "Opaque" "Counter64")
  "Types from RFC 1902 v2 SMI.")

(defvar snmp-rfc1903-types
  '("DisplayString" "PhysAddress" "MacAddress" "TruthValue"
    "TestAndIncr" "AutonomousType" "InstancePointer"
    "VariablePointer" "RowPointer" "RowStatus" "TimeStamp"
    "TimeInterval" "DateAndTime" "StorageType" "TDomain"
    "TAddress")
  "Types from RFC 1903 Textual Conventions.")


(defvar snmp-mode-access-list nil
  "Predefined values for ACCESS clauses.")

(defvar snmp-rfc1155-access
  '("read-only" "read-write" "write-only" "not-accessible")
  "ACCESS values from RFC 1155 v1 SMI.")

(defvar snmp-rfc1902-access
  '("read-only" "read-write" "read-create" "not-accessible"
    "accessible-for-notify")
  "ACCESS values from RFC 1155 v1 SMI.")


(defvar snmp-mode-status-list nil
  "Predefined values for STATUS clauses.")

(defvar snmp-rfc1212-status
  '("mandatory" "obsolete" "deprecated")
  "STATUS values from RFC 1212 v1 SMI.")

(defvar snmp-rfc1902-status
  '("current" "obsolete" "deprecated")
  "STATUS values from RFC 1902 v2 SMI.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;----------------------------------------------------------------------------
;;
;;                  Nothing to customize below here.
;;
;;;----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create abbrev table for SNMP MIB mode
;;
(defvar snmp-mode-abbrev-table nil
  "Abbrev table in use in SNMP mode.")
(define-abbrev-table 'snmp-mode-abbrev-table ())


;; Create abbrev table for SNMPv2 mode
;;
(defvar snmpv2-mode-abbrev-table nil
  "Abbrev table in use in SNMPv2 mode.")
(define-abbrev-table 'snmpv2-mode-abbrev-table ())


;; Set up our keymap
;;
(defvar snmp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177"         'backward-delete-char-untabify)
    (define-key map "\C-c\C-i"     'tempo-complete-tag)
    (define-key map "\C-c\C-f"     'tempo-forward-mark)
    (define-key map "\C-c\C-b"     'tempo-backward-mark)
    map)
  "Keymap used in SNMP mode.")


;; Set up our syntax table
;;
(defvar snmp-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\  "\\"     st)
    (modify-syntax-entry ?-   "_ 1234" st)
    (modify-syntax-entry ?\n  ">"      st)
    (modify-syntax-entry ?\^m ">"      st)
    (modify-syntax-entry ?_   "."      st)
    (modify-syntax-entry ?:   "."      st)
    (modify-syntax-entry ?=   "."      st)
    st)
  "Syntax table used for buffers in SNMP mode.")

;; Set up the stuff that's common between snmp-mode and snmpv2-mode
;;
(defun snmp-common-mode (name mode abbrev font-keywords imenu-index tempo-tags)
  (kill-all-local-variables)

  ;; Become the current major mode
  (setq mode-name name)
  (setq major-mode mode)

  ;; Activate keymap, syntax table, and abbrev table
  (use-local-map snmp-mode-map)
  (set-syntax-table snmp-mode-syntax-table)
  (setq local-abbrev-table abbrev)

  ;; Set up paragraphs (?)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  ;; Set up comments
  (make-local-variable 'comment-start)
  (setq comment-start "-- ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  ;; Set up indentation
  (if snmp-special-indent
      (set (make-local-variable 'indent-line-function) 'snmp-indent-line))
  (set (make-local-variable 'tab-always-indent) snmp-tab-always-indent)

  ;; Font Lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults (cons font-keywords '(nil nil ((?- . "w 1234")))))

  ;; Imenu
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function imenu-index)

  ;; Tempo
  (tempo-use-tag-list tempo-tags)
  (make-local-variable 'tempo-match-finder)
  (setq tempo-match-finder "\\b\\(.+\\)\\=")
  (make-local-variable 'tempo-interactive)
  (setq tempo-interactive t)

  ;; Miscellaneous customization
  (make-local-variable 'require-final-newline)
  (setq require-final-newline mode-require-final-newline))


;; SNMPv1 MIB Editing Mode.
;;
;;;###autoload
(defun snmp-mode ()
  "Major mode for editing SNMP MIBs.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments start with -- and end with newline or another --.
Delete converts tabs to spaces as it moves back.
\\{snmp-mode-map}
Turning on snmp-mode runs the hooks in `snmp-common-mode-hook', then
`snmp-mode-hook'."
  (interactive)

  (snmp-common-mode "SNMP" 'snmp-mode
                   snmp-mode-abbrev-table
                   '(snmp-font-lock-keywords
                     snmp-font-lock-keywords-1
                     snmp-font-lock-keywords-2
                     snmp-font-lock-keywords-3)
                   'snmp-mode-imenu-create-index
                   'snmp-tempo-tags)

  ;; Completion lists
  (make-local-variable 'snmp-mode-syntax-list)
  (setq snmp-mode-syntax-list (append snmp-rfc1155-types
                                     snmp-rfc1213-types
                                     snmp-mode-syntax-list))
  (make-local-variable 'snmp-mode-access-list)
  (setq snmp-mode-access-list snmp-rfc1155-access)
  (make-local-variable 'snmp-mode-status-list)
  (setq snmp-mode-status-list snmp-rfc1212-status)

  ;; Run hooks
  (run-mode-hooks 'snmp-common-mode-hook 'snmp-mode-hook))


;;;###autoload
(defun snmpv2-mode ()
  "Major mode for editing SNMPv2 MIBs.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments start with -- and end with newline or another --.
Delete converts tabs to spaces as it moves back.
\\{snmp-mode-map}
Turning on snmp-mode runs the hooks in `snmp-common-mode-hook',
then `snmpv2-mode-hook'."
  (interactive)

  (snmp-common-mode "SNMPv2" 'snmpv2-mode
                   snmpv2-mode-abbrev-table
                   '(snmp-font-lock-keywords
                     snmp-font-lock-keywords-1
                     snmp-font-lock-keywords-2
                     snmp-font-lock-keywords-3)
                   'snmp-mode-imenu-create-index
                   'snmpv2-tempo-tags)

  ;; Completion lists
  (make-local-variable 'snmp-mode-syntax-list)
  (setq snmp-mode-syntax-list (append snmp-rfc1902-types
                                     snmp-rfc1903-types
                                     snmp-mode-syntax-list))
  (make-local-variable 'snmp-mode-access-list)
  (setq snmp-mode-access-list snmp-rfc1902-access)
  (make-local-variable 'snmp-mode-status-list)
  (setq snmp-mode-status-list snmp-rfc1902-status)

  ;; Run hooks
  (run-mode-hooks 'snmp-common-mode-hook 'snmpv2-mode-hook))


;;;----------------------------------------------------------------------------
;;
;;                           Indentation Setup
;;
;;;----------------------------------------------------------------------------

(defvar snmp-macro-open
  "[a-zA-Z][-a-zA-Z0-9]*[ \t]*\\(OBJECT\\|TRAP\\)-\\(TYPE\\|GROUP\\)\
\\|DESCRIPTION\\|IMPORTS\\|MODULE\\(-IDENTITY\\|-COMPLIANCE\\)\
\\|.*::=[ \t]*\\(BEGIN\\|TEXTUAL-CONVENTION\\)[ \t]*$")

(defvar snmp-macro-close
  "::=[ \t]*{\\|\\(END\\|.*[;\"]\\)[ \t]*$")

(defun snmp-calculate-indent ()
  "Calculate the current line indentation in SNMP MIB code.

We use a very simple scheme: if the previous non-empty line was a \"macro
open\" string, add `snmp-indent-level' to it.  If it was a \"macro close\"
string, subtract `snmp-indent-level'.  Otherwise, use the same indentation
as the previous non-empty line.  Note comments are considered empty
lines for the purposes of this function."
  (let ((empty (concat "\\([ \t]*\\)\\(" comment-start-skip "\\|$\\)"))
        (case-fold-search nil)) ; keywords must be in uppercase
    (save-excursion
      (while (and (>= (forward-line -1) 0)
                  (looking-at empty)))
      (skip-chars-forward " \t")
      (+ (current-column)
         ;; Are we looking at a macro open string?  If so, add more.
         (cond ((looking-at snmp-macro-open)
                snmp-indent-level)
               ;; macro close string?  If so, remove some.
               ((looking-at snmp-macro-close)
                (- snmp-indent-level))
               ;; Neither; just stay here.
               (t 0))))))

(defun snmp-indent-line ()
  "Indent current line as SNMP MIB code."
  (let ((indent (snmp-calculate-indent))
        (pos (- (point-max) (point)))
        shift-amt beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
        nil
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))


;;;----------------------------------------------------------------------------
;;
;;                              Imenu Setup
;;
;;;----------------------------------------------------------------------------

(defvar snmp-clause-regexp
  "^[ \t]*\\([a-zA-Z][-a-zA-Z0-9]*\\)[ \t\n]*\
\\(TRAP-TYPE\\|::=\\|OBJECT\\(-TYPE[ \t\n]+SYNTAX\\|[ \t\n]+IDENTIFIER[ \t\n]*::=\\)\\)")

(defun snmp-mode-imenu-create-index ()
  (let ((index-alist '())
	(index-oid-alist '())
	(index-tc-alist '())
	(index-table-alist '())
	(index-trap-alist '())
        (case-fold-search nil) ; keywords must be uppercase
	prev-pos token end)
    (goto-char (point-min))
    (imenu-progress-message prev-pos 0)
    ;; Search for a useful MIB item (that's not in a comment)
    (save-match-data
      (while (re-search-forward snmp-clause-regexp nil t)
        (imenu-progress-message prev-pos)
        (setq
         end (match-end 0)
         token (cons (match-string 1)
                     (set-marker (make-marker) (match-beginning 1))))
        (goto-char (match-beginning 2))
        (cond ((looking-at "OBJECT-TYPE[ \t\n]+SYNTAX")
               (push token index-alist))
              ((looking-at "OBJECT[ \t\n]+IDENTIFIER[ \t\n]*::=")
               (push token index-oid-alist))
              ((looking-at "::=[ \t\n]*SEQUENCE[ \t\n]*{")
               (push token index-table-alist))
              ((looking-at "TRAP-TYPE")
               (push token index-trap-alist))
              ((looking-at "::=")
               (push token index-tc-alist)))
        (goto-char end)))
    ;; Create the menu
    (imenu-progress-message prev-pos 100)
    (setq index-alist (nreverse index-alist))
    (and index-tc-alist
 	 (push (cons "Textual Conventions" (nreverse index-tc-alist))
  	       index-alist))
    (and index-trap-alist
	 (push (cons "Traps" (nreverse index-trap-alist))
               index-alist))
    (and index-table-alist
 	 (push (cons "Tables" (nreverse index-table-alist))
  	       index-alist))
    (and index-oid-alist
 	 (push (cons "Object IDs" (nreverse index-oid-alist))
  	       index-alist))
    index-alist))


;;;----------------------------------------------------------------------------
;;
;;                              Tempo Setup
;;
;;;----------------------------------------------------------------------------

(require 'tempo)

;; Perform a completing-read with info given
;;
(defun snmp-completing-read (prompt table &optional pred require init hist)
  "Read from the minibuffer, with completion.
Like `completing-read', but the variable `snmp-completion-ignore-case'
controls whether case is significant."
  (let ((completion-ignore-case snmp-completion-ignore-case))
    (completing-read prompt table pred require init hist)))

;; OBJECT-TYPE macro template
;;
(tempo-define-template "snmp-object-type"
  '(> (P "Object Label: ") " OBJECT-TYPE" n>
    "SYNTAX  "
    (if tempo-interactive
        (snmp-completing-read "Syntax: " snmp-mode-syntax-list nil nil)
      p) n>
    "ACCESS  "
    (if tempo-interactive
        (snmp-completing-read "Access: " snmp-mode-access-list nil t)
      p) n>
    "STATUS  "
    (if tempo-interactive
        (snmp-completing-read "Status: " snmp-mode-status-list nil t)
      p) n>
    "DESCRIPTION" n> "\"" p "\"" n>
    (P "Default Value: " defval t)
    (if (string= "" (tempo-lookup-named 'defval))
        nil
      '(l "DEFVAL { " (s defval) " }" n>))
    "::= { " (p "OID: ") " }" n)
  "objectType"
  "Insert an OBJECT-TYPE macro."
  'snmp-tempo-tags)

;; Table macro template
;;
(tempo-define-template "snmp-table-type"
  ;; First the table OBJECT-TYPE
  '(> (P "Table Name: " table)
    (P "Entry Name: " entry t)
    (let* ((entry (tempo-lookup-named 'entry))
           (seq (copy-sequence entry)))
      (aset entry 0 (downcase (aref entry 0)))
      (aset seq 0 (upcase (aref seq 0)))
      (tempo-save-named 'obj-entry entry)
      (tempo-save-named 'seq-entry seq)
      nil)
    " OBJECT-TYPE" n>
    "SYNTAX  SEQUENCE OF "
    (s seq-entry) n>
    "ACCESS  not-accessible" n>
    "STATUS  mandatory" n>
    "DESCRIPTION" n> "\"" p "\"" n>
    "::= { " (p "OID: ") " }" n n>
   ;; Next the row OBJECT-TYPE
    (s obj-entry) " OBJECT-TYPE" n>
    "SYNTAX  " (s seq-entry) n>
    "ACCESS  not-accessible" n>
    "STATUS  mandatory" n>
    "DESCRIPTION" n> "\"" p "\"" n>
    "INDEX   { " (p "Index List: ") " }" n>
    "::= {" (s table) " 1 }" n n>
   ;; Finally the SEQUENCE type
    (s seq-entry) " ::= SEQUENCE {" n> p n> "}" n)
  "tableType"
  "Insert an SNMP table."
  'snmp-tempo-tags)


;; v2 SMI OBJECT-TYPE macro template
;;
(tempo-define-template "snmpv2-object-type"
  '(> (P "Object Label: ") " OBJECT-TYPE" n>
    "SYNTAX      "
    (if tempo-interactive
        (snmp-completing-read "Syntax: " snmp-mode-syntax-list nil nil)
      p) n>
    "MAX-ACCESS  "
    (if tempo-interactive
        (snmp-completing-read "Max Access: " snmp-mode-access-list nil t)
      p) n>
    "STATUS      "
    (if tempo-interactive
        (snmp-completing-read "Status: " snmp-mode-status-list nil t)
      p) n>
    "DESCRIPTION" n> "\"" p "\"" n>
    (P "Default Value: " defval t)
    (if (string= "" (tempo-lookup-named 'defval))
        nil
      '(l "DEFVAL { " (s defval) " }" n>))
    "::= { " (p "OID: ") " }" n)
  "objectType"
  "Insert an v2 SMI OBJECT-TYPE macro."
  'snmpv2-tempo-tags)

;; v2 SMI Table macro template
;;
(tempo-define-template "snmpv2-table-type"
  ;; First the table OBJECT-TYPE
  '(> (P "Table Name: " table)
    (P "Entry Name: " entry t)
    (let* ((entry (tempo-lookup-named 'entry))
           (seq (copy-sequence entry)))
      (aset entry 0 (downcase (aref entry 0)))
      (aset seq 0 (upcase (aref seq 0)))
      (tempo-save-named 'obj-entry entry)
      (tempo-save-named 'seq-entry seq)
      nil)
    " OBJECT-TYPE" n>
    "SYNTAX      SEQUENCE OF "
    (s seq-entry) n>
    "MAX-ACCESS  not-accessible" n>
    "STATUS      current" n>
    "DESCRIPTION" n> "\"" p "\"" n>
    "::= { " (p "OID: ") " }" n n>
   ;; Next the row OBJECT-TYPE
    (s obj-entry) " OBJECT-TYPE" n>
    "SYNTAX      " (s seq-entry) n>
    "MAX-ACCESS  not-accessible" n>
    "STATUS      current" n>
    "DESCRIPTION" n> "\"" p "\"" n>
    "INDEX { " (p "Index List: ") " }" n>
    "::= { " (s table) " 1 }" n n>
   ;; Finally the SEQUENCE type
    (s seq-entry) " ::= SEQUENCE {" n> p n> "}" n)
  "tableType"
  "Insert an v2 SMI SNMP table."
  'snmpv2-tempo-tags)

;; v2 SMI TEXTUAL-CONVENTION macro template
;;
(tempo-define-template "snmpv2-textual-convention"
  '(> (P "Textual Convention Type: ") " ::= TEXTUAL-CONVENTION" n>
    "STATUS  "
    (if tempo-interactive
        (snmp-completing-read "Status: " snmp-mode-status-list nil t)
      p) n>
    "DESCRIPTION" n> "\"" p "\"" n>
    "SYNTAX  "
    (if tempo-interactive
        (snmp-completing-read "Syntax: " snmp-mode-syntax-list nil nil)
      p) n> )
  "textualConvention"
  "Insert an v2 SMI TEXTUAL-CONVENTION macro."
  'snmpv2-tempo-tags)


(provide 'snmp-mode)

;;; snmp-mode.el ends here
