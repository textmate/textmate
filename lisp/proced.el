;;; proced.el --- operate on system processes like dired

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Roland Winkler <winkler@gnu.org>
;; Keywords: Processes, Unix

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

;; Proced makes an Emacs buffer containing a listing of the current
;; system processes.  You can use the normal Emacs commands to move around
;; in this buffer, and special Proced commands to operate on the processes
;; listed.  See `proced-mode' for getting started.
;;
;; To do:
;; - interactive temporary customizability of flags in `proced-grammar-alist'
;; - allow "sudo kill PID", "renice PID"
;;
;; Thoughts and Ideas
;; - Currently, `process-attributes' returns the list of
;;   command-line arguments of a process as one concatenated string.
;;   This format is compatible with `shell-command'.  Also, under
;;   MS-Windows, the command-line arguments are actually stored as a
;;   single string, so that it is impossible to reverse-engineer it back
;;   into separate arguments.  Alternatively, `process-attributes'
;;   could (try to) return a list of strings that correspond to individual
;;   command-line arguments.  Then one could feed such a list of
;;   command-line arguments into `call-process' or `start-process'.
;;   Are there real-world applications when such a feature would be useful?
;;   What about something like `proced-restart-pid'?

;;; Code:

(require 'time-date)                 ; for `with-decoded-time-value'

(defgroup proced nil
  "Proced mode."
  :group 'processes
  :group 'unix
  :prefix "proced-")

(defcustom proced-signal-function 'signal-process
  "Name of signal function.
It can be an elisp function (usually `signal-process') or a string specifying
the external command (usually \"kill\")."
  :group 'proced
  :type '(choice (function :tag "function")
                 (string :tag "command")))

(defcustom proced-signal-list
  '( ;; signals supported on all POSIX compliant systems
    ("HUP" . "   (1.  Hangup)")
    ("INT" . "   (2.  Terminal interrupt)")
    ("QUIT" . "  (3.  Terminal quit)")
    ("ABRT" . "  (6.  Process abort)")
    ("KILL" . "  (9.  Kill - cannot be caught or ignored)")
    ("ALRM" . "  (14. Alarm Clock)")
    ("TERM" . "  (15. Termination)")
    ;; POSIX 1003.1-2001
    ;; Which systems do not support these signals so that we can
    ;; exclude them from `proced-signal-list'?
    ("CONT" . "  (Continue executing)")
    ("STOP" . "  (Stop executing / pause - cannot be caught or ignored)")
    ("TSTP" . "  (Terminal stop / pause)"))
  "List of signals, used for minibuffer completion."
  :group 'proced
  :type '(repeat (cons (string :tag "signal name")
                       (string :tag "description"))))

;; For which attributes can we use a fixed width of the output field?
;; A fixed width speeds up formatting, yet it can make
;; `proced-grammar-alist' system-dependent.
;; (If proced runs like top(1) we want it to be fast.)
;;
;; If it is impossible / unlikely that an attribute has the same value
;; for two processes, then sorting can be based on one ordinary (fast)
;; predicate like `<'.  Otherwise, a list of proced predicates can be used
;; to refine the sort.
;;
;; It would be neat if one could temporarily override the following
;; predefined rules.
(defcustom proced-grammar-alist
  '( ;; attributes defined in `process-attributes'
    (euid    "EUID"    "%d" right proced-< nil (euid pid) (nil t nil))
    (user    "User"    nil left proced-string-lessp nil (user pid) (nil t nil))
    (egid    "EGID"    "%d" right proced-< nil (egid euid pid) (nil t nil))
    (group   "Group"   nil left proced-string-lessp nil (group user pid) (nil t nil))
    (comm    "Command" nil left proced-string-lessp nil (comm pid) (nil t nil))
    (state   "Stat"    nil left proced-string-lessp nil (state pid) (nil t nil))
    (ppid    "PPID"    "%d" right proced-< nil (ppid pid)
             ((lambda (ppid) (proced-filter-parents proced-process-alist ppid))
              "refine to process parents"))
    (pgrp    "PGrp"    "%d" right proced-< nil (pgrp euid pid) (nil t nil))
    (sess    "Sess"    "%d" right proced-< nil (sess pid) (nil t nil))
    (ttname  "TTY"     proced-format-ttname left proced-string-lessp nil (ttname pid) (nil t nil))
    (tpgid   "TPGID"   "%d" right proced-< nil (tpgid pid) (nil t nil))
    (minflt  "MinFlt"  "%d" right proced-< nil (minflt pid) (nil t t))
    (majflt  "MajFlt"  "%d" right proced-< nil (majflt pid) (nil t t))
    (cminflt "CMinFlt" "%d" right proced-< nil (cminflt pid) (nil t t))
    (cmajflt "CMajFlt" "%d" right proced-< nil (cmajflt pid) (nil t t))
    (utime   "UTime"   proced-format-time right proced-time-lessp t (utime pid) (nil t t))
    (stime   "STime"   proced-format-time right proced-time-lessp t (stime pid) (nil t t))
    (time    "Time"   proced-format-time right proced-time-lessp t (time pid) (nil t t))
    (cutime  "CUTime"  proced-format-time right proced-time-lessp t (cutime pid) (nil t t))
    (cstime  "CSTime"  proced-format-time right proced-time-lessp t (cstime pid) (nil t t))
    (ctime   "CTime"  proced-format-time right proced-time-lessp t (ctime pid) (nil t t))
    (pri     "Pr"      "%d" right proced-< t (pri pid) (nil t t))
    (nice    "Ni"      "%3d" 3 proced-< t (nice pid) (t t nil))
    (thcount "THCount" "%d" right proced-< t (thcount pid) (nil t t))
    (start   "Start"   proced-format-start 6 proced-time-lessp nil (start pid) (t t nil))
    (vsize   "VSize"   "%d" right proced-< t (vsize pid) (nil t t))
    (rss     "RSS"     "%d" right proced-< t (rss pid) (nil t t))
    (etime   "ETime"   proced-format-time right proced-time-lessp t (etime pid) (nil t t))
    (pcpu    "%CPU"    "%.1f" right proced-< t (pcpu pid) (nil t t))
    (pmem    "%Mem"    "%.1f" right proced-< t (pmem pid) (nil t t))
    (args    "Args"    proced-format-args left proced-string-lessp nil (args pid) (nil t nil))
    ;;
    ;; attributes defined by proced (see `proced-process-attributes')
    (pid     "PID"     "%d" right proced-< nil (pid)
             ((lambda (ppid) (proced-filter-children proced-process-alist ppid))
              "refine to process children"))
    ;; process tree
    (tree    "Tree"   proced-format-tree left nil nil nil nil))
  "Alist of rules for handling Proced attributes.

Each element has the form

  (KEY NAME FORMAT JUSTIFY PREDICATE REVERSE SORT-SCHEME REFINER).

Symbol KEY is the car of a process attribute.

String NAME appears in the header line.

FORMAT specifies the format for displaying the attribute values.  It can
be a string passed to `format'.  It can be a function called with one
argument, the value of the attribute.  The value nil means take as is.

If JUSTIFY is an integer, its modulus gives the width of the attribute
values formatted with FORMAT.  If JUSTIFY is positive, NAME appears
right-justified, otherwise it appears left-justified.  If JUSTIFY is 'left
or 'right, the field width is calculated from all field values in the listing.
If JUSTIFY is 'left, the field values are formatted left-justified and
right-justified otherwise.

PREDICATE is the predicate for sorting and filtering the process listing
based on attribute KEY.  PREDICATE takes two arguments P1 and P2,
the corresponding attribute values of two processes.  PREDICATE should
return 'equal if P1 has same rank like P2.  Any other non-nil value says
that P1 is \"less than\" P2, or nil if not.
If PREDICATE is nil the attribute cannot be sorted.

PREDICATE defines an ascending sort order.  REVERSE is non-nil if the sort
order is descending.

SORT-SCHEME is a list (KEY1 KEY2 ...) defining a hierarchy of rules
for sorting the process listing.  KEY1, KEY2, ... are KEYs appearing as cars
of `proced-grammar-alist'.  First the PREDICATE of KEY1 is evaluated.
If it yields non-equal, it defines the sort order for the corresponding
processes.  If it evaluates to 'equal the PREDICATE of KEY2 is evaluated, etc.

REFINER can be a list of flags (LESS-B EQUAL-B LARGER-B) used by the command
`proced-refine' (see there) to refine the listing based on attribute KEY.
This command compares the value of attribute KEY of every process with
the value of attribute KEY of the process at the position of point
using PREDICATE.
If PREDICATE yields non-nil, the process is accepted if LESS-B is non-nil.
If PREDICATE yields 'equal, the process is accepted if EQUAL-B is non-nil.
If PREDICATE yields nil, the process is accepted if LARGER-B is non-nil.

REFINER can also be a list (FUNCTION HELP-ECHO).
FUNCTION is called with one argument, the PID of the process at the position
of point.  The function must return a list of PIDs that is used for the refined
listing.  HELP-ECHO is a string that is shown when mouse is over this field.

If REFINER is nil no refinement is done."
  :group 'proced
  :type '(repeat (list :tag "Attribute"
                       (symbol :tag "Key")
                       (string :tag "Header")
                       (choice :tag "Format"
                               (const :tag "None" nil)
                               (string :tag "Format String")
                               (function :tag "Formatting Function"))
                       (choice :tag "Justification"
                               (const :tag "left" left)
                               (const :tag "right" right)
                               (integer :tag "width"))
                       (choice :tag "Predicate"
                               (const :tag "None" nil)
                               (function :tag "Function"))
                       (boolean :tag "Descending Sort Order")
                       (repeat :tag "Sort Scheme" (symbol :tag "Key"))
                       (choice :tag "Refiner"
                               (const :tag "None" nil)
                               (list (function :tag "Refinement Function")
                                     (string :tag "Help echo"))
                               (list :tag "Refine Flags"
                                     (boolean :tag "Less")
                                     (boolean :tag "Equal")
                                     (boolean :tag "Larger"))))))

(defcustom proced-custom-attributes nil
  "List of functions defining custom attributes.
This variable extends the functionality of `proced-process-attributes'.
Each function is called with one argument, the list of attributes
of a system process.  It returns a cons cell of the form (KEY . VALUE)
like `process-attributes'.  This cons cell is appended to the list
returned by `proced-process-attributes'.
If the function returns nil, the value is ignored."
  :group 'proced
  :type '(repeat (function :tag "Attribute")))

;; Formatting and sorting rules are defined "per attribute".  If formatting
;; and / or sorting should use more than one attribute, it appears more
;; transparent to define a new derived attribute, so that formatting and
;; sorting can use them consistently.  (Are there exceptions to this rule?
;; Would it be advantageous to have yet more general methods available?)
;; Sorting can also be based on attributes that are invisible in the listing.

(defcustom proced-format-alist
  '((short user pid tree pcpu pmem start time (args comm))
    (medium user pid tree pcpu pmem vsize rss ttname state start time (args comm))
    (long user euid group pid tree pri nice pcpu pmem vsize rss ttname state
          start time (args comm))
    (verbose user euid group egid pid ppid tree pgrp sess pri nice pcpu pmem
             state thcount vsize rss ttname tpgid minflt majflt cminflt cmajflt
             start time utime stime ctime cutime cstime etime (args comm)))
  "Alist of formats of listing.
The car of each element is a symbol, the name of the format.
The cdr is a list of attribute keys appearing in `proced-grammar-alist'.
An element of this list may also be a list of attribute keys that specifies
alternatives.  If the first attribute is absent for a process, use the second
one, etc."
  :group 'proced
  :type '(alist :key-type (symbol :tag "Format Name")
                :value-type (repeat :tag "Keys"
                                    (choice (symbol :tag "")
                                            (repeat :tag "Alternative Keys"
                                                    (symbol :tag ""))))))

(defcustom proced-format 'short
  "Current format of Proced listing.
It can be the car of an element of `proced-format-alist'.
It can also be a list of keys appearing in `proced-grammar-alist'."
  :group 'proced
  :type '(choice (symbol :tag "Format Name")
                 (repeat :tag "Keys" (symbol :tag ""))))
(make-variable-buffer-local 'proced-format)

;; FIXME: is there a better name for filter `user' that does not coincide
;; with an attribute key?
(defcustom proced-filter-alist
  `((user (user . ,(concat "\\`" (regexp-quote (user-real-login-name)) "\\'")))
    (user-running (user . ,(concat "\\`" (regexp-quote (user-real-login-name)) "\\'"))
                  (state . "\\`[Rr]\\'"))
    (all)
    (all-running (state . "\\`[Rr]\\'"))
    (emacs (fun-all . (lambda (list)
                        (proced-filter-children list ,(emacs-pid))))))
  "Alist of process filters.
The car of each element is a symbol, the name of the filter.
The cdr is a list of elementary filters that are applied to every process.
A process is displayed if it passes all elementary filters of a selected
filter.

An elementary filter can be one of the following:
\(KEY . REGEXP)   If value of attribute KEY matches REGEXP,
                 accept this process.
\(KEY . FUN)      Apply function FUN to attribute KEY.  Accept this process,
                 if FUN returns non-nil.
\(function . FUN) For each process, apply function FUN to list of attributes
                 of each.  Accept the process if FUN returns non-nil.
\(fun-all . FUN)  Apply function FUN to entire process list.
                 FUN must return the filtered list."
  :group 'proced
  :type '(repeat (cons :tag "Filter"
                       (symbol :tag "Filter Name")
                       (repeat :tag "Filters"
                               (choice (cons :tag "Key . Regexp" (symbol :tag "Key") regexp)
                                       (cons :tag "Key . Function" (symbol :tag "Key") function)
                                       (cons :tag "Function" (const :tag "Key: function" function) function)
                                       (cons :tag "Fun-all" (const :tag "Key: fun-all" fun-all) function))))))

(defcustom proced-filter 'user
  "Current filter of proced listing.
It can be the car of an element of `proced-filter-alist'.
It can also be a list of elementary filters as in the cdrs of the elements
of `proced-filter-alist'."
  :group 'proced
  :type '(choice (symbol :tag "Filter Name")
                 (repeat :tag "Filters"
                         (choice (cons :tag "Key . Regexp" (symbol :tag "Key") regexp)
                                 (cons :tag "Key . Function" (symbol :tag "Key") function)
                                 (cons :tag "Function" (const :tag "Key: function" function) function)
                                 (cons :tag "Fun-all" (const :tag "Key: fun-all" fun-all) function)))))
(make-variable-buffer-local 'proced-filter)

(defcustom proced-sort 'pcpu
  "Current sort scheme for proced listing.
It must be the KEY of an element of `proced-grammar-alist'.
It can also be a list of KEYs as in the SORT-SCHEMEs of the elements
of `proced-grammar-alist'."
  :group 'proced
  :type '(choice (symbol :tag "Sort Scheme")
                 (repeat :tag "Key List" (symbol :tag "Key"))))
(make-variable-buffer-local 'proced-sort)

(defcustom proced-descend t
  "Non-nil if proced listing is sorted in descending order."
  :group 'proced
  :type '(boolean :tag "Descending Sort Order"))
(make-variable-buffer-local 'proced-descend)

(defcustom proced-goal-attribute 'args
  "If non-nil, key of the attribute that defines the `goal-column'."
  :group 'proced
  :type '(choice (const :tag "none" nil)
                 (symbol :tag "key")))

(defcustom proced-auto-update-interval 5
  "Time interval in seconds for auto updating Proced buffers."
  :group 'proced
  :type 'integer)

(defcustom proced-auto-update-flag nil
  "Non-nil for auto update of a Proced buffer.
Can be changed interactively via `proced-toggle-auto-update'."
  :group 'proced
  :type 'boolean)
(make-variable-buffer-local 'proced-auto-update-flag)

(defcustom proced-tree-flag nil
  "Non-nil for display of Proced buffer as process tree."
  :group 'proced
  :type 'boolean)
(make-variable-buffer-local 'proced-tree-flag)

(defcustom proced-post-display-hook nil
  "Normal hook run after displaying or updating a Proced buffer.
May be used to adapt the window size via `fit-window-to-buffer'."
  :type 'hook
  :options '(fit-window-to-buffer)
  :group 'proced)

(defcustom proced-after-send-signal-hook nil
  "Normal hook run after sending a signal to processes by `proced-send-signal'.
May be used to revert the process listing."
  :type 'hook
  :options '(proced-revert)
  :group 'proced)

;; Internal variables

(defvar proced-available (not (null (list-system-processes)))
  "Non-nil means Proced is known to work on this system.")

(defvar proced-process-alist nil
  "Alist of processes displayed by Proced.
The car of each element is the PID, and the cdr is a list of
cons pairs, see `proced-process-attributes'.")
(make-variable-buffer-local 'proced-process-alist)

(defvar proced-sort-internal nil
  "Sort scheme for listing (internal format).
It is a list of lists (KEY PREDICATE REVERSE).")

(defvar proced-marker-char ?*		; the answer is 42
  "In Proced, the current mark character.")

;; Faces and font-lock code taken from dired,
;; but face variables are deprecated for new code.
(defgroup proced-faces nil
  "Faces used by Proced."
  :group 'proced
  :group 'faces)

(defface proced-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for Proced marks."
  :group 'proced-faces)

(defface proced-marked
  '((t (:inherit error)))
  "Face used for marked processes."
  :group 'proced-faces)

(defface proced-sort-header
  '((t (:inherit font-lock-keyword-face)))
  "Face used for header of attribute used for sorting."
  :group 'proced-faces)

(defvar proced-re-mark "^[^ \n]"
  "Regexp matching a marked line.
Important: the match ends just after the marker.")

(defvar proced-header-line nil
  "Headers in Proced buffer as a string.")
(make-variable-buffer-local 'proced-header-line)

(defvar proced-temp-alist nil
  "Temporary alist (internal variable).")

(defvar proced-process-tree nil
  "Proced process tree (internal variable).")

(defvar proced-tree-depth nil
  "Internal variable for depth of Proced process tree.")

(defvar proced-auto-update-timer nil
  "Stores if Proced auto update timer is already installed.")

(defvar proced-log-buffer "*Proced log*"
  "Name of Proced Log buffer.")

(defconst proced-help-string
  "(n)ext, (p)revious, (m)ark, (u)nmark, (k)ill, (q)uit (type ? for more help)"
  "Help string for Proced.")

(defconst proced-header-help-echo
  "mouse-1, mouse-2: sort by attribute %s%s (%s)"
  "Help string shown when mouse is over a sortable header.")

(defconst proced-field-help-echo
  "mouse-2, RET: refine by attribute %s %s"
  "Help string shown when mouse is over a refinable field.")

(defvar proced-font-lock-keywords
  `(;; (Any) proced marks.
    (,proced-re-mark . 'proced-mark)
    ;; Processes marked with `proced-marker-char'
    ;; Should we make sure that only certain attributes are font-locked?
    (,(concat "^[" (char-to-string proced-marker-char) "]")
     ".+" (proced-move-to-goal-column) nil (0 'proced-marked))))

(defvar proced-mode-map
  (let ((km (make-sparse-keymap)))
    ;; moving
    (define-key km " " 'next-line)
    (define-key km "n" 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km "\C-n" 'next-line)
    (define-key km "\C-p" 'previous-line)
    (define-key km "\C-?" 'previous-line)
    (define-key km [down] 'next-line)
    (define-key km [up] 'previous-line)
    ;; marking
    (define-key km "d" 'proced-mark) ; Dired compatibility ("delete")
    (define-key km "m" 'proced-mark)
    (put 'proced-mark :advertised-binding "m")
    (define-key km "u" 'proced-unmark)
    (define-key km "\177" 'proced-unmark-backward)
    (define-key km "M" 'proced-mark-all)
    (define-key km "U" 'proced-unmark-all)
    (define-key km "t" 'proced-toggle-marks)
    (define-key km "C" 'proced-mark-children)
    (define-key km "P" 'proced-mark-parents)
    ;; filtering
    (define-key km "f"  'proced-filter-interactive)
    (define-key km [mouse-2] 'proced-refine)
    (define-key km "\C-m" 'proced-refine)
    ;; sorting
    (define-key km "sc" 'proced-sort-pcpu)
    (define-key km "sm" 'proced-sort-pmem)
    (define-key km "sp" 'proced-sort-pid)
    (define-key km "ss" 'proced-sort-start)
    (define-key km "sS" 'proced-sort-interactive)
    (define-key km "st" 'proced-sort-time)
    (define-key km "su" 'proced-sort-user)
    ;; similar to `Buffer-menu-sort-by-column'
    (define-key km [header-line mouse-1] 'proced-sort-header)
    (define-key km [header-line mouse-2] 'proced-sort-header)
    (define-key km "T" 'proced-toggle-tree)
    ;; formatting
    (define-key km "F"  'proced-format-interactive)
    ;; operate
    (define-key km "o" 'proced-omit-processes)
    (define-key km "x" 'proced-send-signal) ; Dired compatibility
    (define-key km "k" 'proced-send-signal) ; kill processes
    ;; misc
    (define-key km "h" 'describe-mode)
    (define-key km "?" 'proced-help)
    (define-key km [remap undo] 'proced-undo)
    (define-key km [remap advertised-undo] 'proced-undo)
    ;; Additional keybindings are inherited from `special-mode-map'
    km)
  "Keymap for Proced commands.")

(easy-menu-define
  proced-menu proced-mode-map "Proced Menu"
  `("Proced"
    ["Mark" proced-mark
     :help "Mark Current Process"]
    ["Unmark" proced-unmark
     :help "Unmark Current Process"]
    ["Mark All" proced-mark-all
     :help "Mark All Processes"]
    ["Unmark All" proced-unmark-all
     :help "Unmark All Process"]
    ["Toggle Marks" proced-toggle-marks
     :help "Marked Processes Become Unmarked, and Vice Versa"]
    ["Mark Children" proced-mark-children
     :help "Mark Current Process and its Children"]
    ["Mark Parents" proced-mark-parents
     :help "Mark Current Process and its Parents"]
    "--"
    ("Filters"
     :help "Select Filter for Process Listing"
     ,@(mapcar (lambda (el)
                 (let ((filter (car el)))
                   `[,(symbol-name filter)
                     (proced-filter-interactive ',filter)
                     :style radio
                     :selected (eq proced-filter ',filter)]))
               proced-filter-alist))
    ("Sorting"
     :help "Select Sort Scheme"
     ["Sort..." proced-sort-interactive
      :help "Sort Process List"]
     "--"
     ["Sort by %CPU" proced-sort-pcpu]
     ["Sort by %MEM" proced-sort-pmem]
     ["Sort by PID" proced-sort-pid]
     ["Sort by START" proced-sort-start]
     ["Sort by TIME" proced-sort-time]
     ["Sort by USER" proced-sort-user])
    ("Formats"
     :help "Select Format for Process Listing"
     ,@(mapcar (lambda (el)
                 (let ((format (car el)))
                   `[,(symbol-name format)
                     (proced-format-interactive ',format)
                     :style radio
                     :selected (eq proced-format ',format)]))
               proced-format-alist))
    ["Tree Display" proced-toggle-tree
     :style toggle
     :selected (eval proced-tree-flag)
     :help "Display Proced Buffer as Process Tree"]
    "--"
    ["Omit Marked Processes" proced-omit-processes
     :help "Omit Marked Processes in Process Listing."]
    "--"
    ["Revert" revert-buffer
     :help "Revert Process Listing"]
    ["Auto Update" proced-toggle-auto-update
     :style toggle
     :selected (eval proced-auto-update-flag)
     :help "Auto Update of Proced Buffer"]
    ["Send signal" proced-send-signal
     :help "Send Signal to Marked Processes"]))

;; helper functions
(defun proced-marker-regexp ()
  "Return regexp matching `proced-marker-char'."
  ;; `proced-marker-char' must appear in column zero
  (concat "^" (regexp-quote (char-to-string proced-marker-char))))

(defun proced-success-message (action count)
  "Display success message for ACTION performed for COUNT processes."
  (message "%s %s process%s" action count (if (= 1 count) "" "es")))

;; Unlike dired, we do not define our own commands for vertical motion.
;; If `goal-column' is set, `next-line' and `previous-line' are fancy
;; commands to satisfy our modest needs.  If `proced-goal-attribute'
;; and/or `goal-column' are not set, `next-line' and `previous-line'
;; are really what we need to preserve the column of point.
;; We use `proced-move-to-goal-column' for "non-interactive" cases only
;; to get a well-defined position of point.

(defun proced-move-to-goal-column ()
  "Move to `goal-column' if non-nil.  Return position of point."
  (beginning-of-line)
  (unless (eobp)
    (if goal-column
        (forward-char goal-column)
      (forward-char 2)))
  (point))

(defun proced-header-line ()
  "Return header line for Proced buffer."
  (list (propertize " " 'display '(space :align-to 0))
        (if (<= (window-hscroll) (length proced-header-line))
            (replace-regexp-in-string ;; preserve text properties
             "\\(%\\)" "\\1\\1"
             (substring proced-header-line (window-hscroll))))))

(defun proced-pid-at-point ()
  "Return pid of system process at point.
Return nil if point is not on a process line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^. .")
        (get-text-property (match-end 0) 'proced-pid))))

;; proced mode

(define-derived-mode proced-mode special-mode "Proced"
  "Mode for displaying system processes and sending signals to them.
Type \\[proced] to start a Proced session.  In a Proced buffer
type \\<proced-mode-map>\\[proced-mark] to mark a process for later commands.
Type \\[proced-send-signal] to send signals to marked processes.

The initial content of a listing is defined by the variable `proced-filter'
and the variable `proced-format'.
The variable `proced-filter' specifies which system processes are displayed.
The variable `proced-format' specifies which attributes are displayed for
each process.  Type \\[proced-filter-interactive] and \\[proced-format-interactive]
to change the values of `proced-filter' and `proced-format'.
The current value of the variable `proced-filter' is indicated in the
mode line.

The sort order of Proced listings is defined by the variable `proced-sort'.
Type \\[proced-sort-interactive] or click on a header in the header line
to change the sort scheme.  The current sort scheme is indicated in the
mode line, using \"+\" or \"-\" for ascending or descending sort order.

Type \\[proced-toggle-tree] to toggle whether the listing is
displayed as process tree.

An existing Proced listing can be refined by typing \\[proced-refine].
Refining an existing listing does not update the variable `proced-filter'.

The attribute-specific rules for formatting, filtering, sorting, and refining
are defined in `proced-grammar-alist'.

After displaying or updating a Proced buffer, Proced runs the normal hook
`proced-post-display-hook'.

\\{proced-mode-map}"
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (setq buffer-read-only t
        truncate-lines t
        header-line-format '(:eval (proced-header-line)))
  (add-hook 'post-command-hook 'force-mode-line-update nil t)
  (set (make-local-variable 'revert-buffer-function) 'proced-revert)
  (set (make-local-variable 'font-lock-defaults)
       '(proced-font-lock-keywords t nil nil beginning-of-line))
  (if (and (not proced-auto-update-timer) proced-auto-update-interval)
      (setq proced-auto-update-timer
            (run-at-time t proced-auto-update-interval
                         'proced-auto-update-timer))))

;;;###autoload
(defun proced (&optional arg)
  "Generate a listing of UNIX system processes.
If invoked with optional ARG the window displaying the process
information will be displayed but not selected.
Runs the normal hook `proced-post-display-hook'.

See `proced-mode' for a description of features available in Proced buffers."
  (interactive "P")
  (unless proced-available
    (error "Proced is not available on this system"))
  (let ((buffer (get-buffer-create "*Proced*")) new)
    (set-buffer buffer)
    (setq new (zerop (buffer-size)))
    (when new
      (proced-mode)
      ;; `proced-update' runs `proced-post-display-hook' only if the
      ;; Proced buffer has been selected.  Yet the following call of
      ;; `proced-update' is for an empty Proced buffer that has not
      ;; yet been selected.  Therefore we need to call
      ;; `proced-post-display-hook' below.
      (proced-update t))
    (if arg
        (progn
          (display-buffer buffer)
          (with-current-buffer buffer
            (proced-update t)))
      (pop-to-buffer buffer)
      (proced-update t)
      (message
       (substitute-command-keys
        "Type \\<proced-mode-map>\\[quit-window] to quit, \\[proced-help] for help")))))

(defun proced-auto-update-timer ()
  "Auto-update Proced buffers using `run-at-time'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (eq major-mode 'proced-mode)
               proced-auto-update-flag)
          (proced-update t t)))))

(defun proced-toggle-auto-update (arg)
  "Change whether this Proced buffer is updated automatically.
With prefix ARG, update this buffer automatically if ARG is positive,
otherwise do not update.  Sets the variable `proced-auto-update-flag'.
The time interval for updates is specified via `proced-auto-update-interval'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq proced-auto-update-flag
        (cond ((eq arg 'toggle) (not proced-auto-update-flag))
              (arg (> (prefix-numeric-value arg) 0))
              (t (not proced-auto-update-flag))))
  (message "Proced auto update %s"
           (if proced-auto-update-flag "enabled" "disabled")))

;;; Mark

(defun proced-mark (&optional count)
  "Mark the current (or next COUNT) processes."
  (interactive "p")
  (proced-do-mark t count))

(defun proced-unmark (&optional count)
  "Unmark the current (or next COUNT) processes."
  (interactive "p")
  (proced-do-mark nil count))

(defun proced-unmark-backward (&optional count)
  "Unmark the previous (or COUNT previous) processes."
  ;; Analogous to `dired-unmark-backward',
  ;; but `ibuffer-unmark-backward' behaves different.
  (interactive "p")
  (proced-do-mark nil (- (or count 1))))

(defun proced-do-mark (mark &optional count)
  "Mark the current (or next COUNT) processes using MARK."
  (or count (setq count 1))
  (let ((backward (< count 0))
	buffer-read-only)
    (setq count (1+ (if (<= 0 count) count
                      (min (1- (line-number-at-pos)) (abs count)))))
    (beginning-of-line)
    (while (not (or (zerop (setq count (1- count))) (eobp)))
      (proced-insert-mark mark backward))
    (proced-move-to-goal-column)))

(defun proced-toggle-marks ()
  "Toggle marks: marked processes become unmarked, and vice versa."
  (interactive)
  (let ((mark-re (proced-marker-regexp))
        buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at mark-re)
               (proced-insert-mark nil))
              ((looking-at " ")
               (proced-insert-mark t))
              (t
               (forward-line 1)))))))

(defun proced-insert-mark (mark &optional backward)
  "If MARK is non-nil, insert `proced-marker-char'.
If BACKWARD is non-nil, move one line backwards before inserting the mark.
Otherwise move one line forward after inserting the mark."
  (if backward (forward-line -1))
  (insert (if mark proced-marker-char ?\s))
  (delete-char 1)
  (unless backward (forward-line)))

(defun proced-mark-all ()
  "Mark all processes.
If `transient-mark-mode' is turned on and the region is active,
mark the region."
  (interactive)
  (proced-do-mark-all t))

(defun proced-unmark-all ()
  "Unmark all processes.
If `transient-mark-mode' is turned on and the region is active,
unmark the region."
  (interactive)
  (proced-do-mark-all nil))

(defun proced-do-mark-all (mark)
  "Mark all processes using MARK.
If `transient-mark-mode' is turned on and the region is active,
mark the region."
  (let* ((count 0)
         (proced-marker-char (if mark proced-marker-char ?\s))
         (marker-re (proced-marker-regexp))
         end buffer-read-only)
    (save-excursion
      (if (use-region-p)
          ;; Operate even on those lines that are only partially a part
          ;; of region.  This appears most consistent with
          ;; `proced-move-to-goal-column'.
          (progn (setq end (save-excursion
                             (goto-char (region-end))
                             (unless (looking-at "^") (forward-line))
                             (point)))
                 (goto-char (region-beginning))
                 (unless (looking-at "^") (beginning-of-line)))
        (goto-char (point-min))
        (setq end (point-max)))
      (while (< (point) end)
        (unless (looking-at marker-re)
          (setq count (1+ count))
          (insert proced-marker-char)
          (delete-char 1))
        (forward-line))
      (proced-success-message (if mark "Marked" "Unmarked") count))))

(defun proced-mark-children (ppid &optional omit-ppid)
  "Mark child processes of process PPID.
Also mark process PPID unless prefix OMIT-PPID is non-nil."
  (interactive (list (proced-pid-at-point) current-prefix-arg))
  (proced-mark-process-alist
   (proced-filter-children proced-process-alist ppid omit-ppid)))

(defun proced-mark-parents (cpid &optional omit-cpid)
  "Mark parent processes of process CPID.
Also mark CPID unless prefix OMIT-CPID is non-nil."
  (interactive (list (proced-pid-at-point) current-prefix-arg))
  (proced-mark-process-alist
   (proced-filter-parents proced-process-alist cpid omit-cpid)))

(defun proced-mark-process-alist (process-alist &optional quiet)
  "Mark processes in PROCESS-ALIST.
If QUIET is non-nil suppress status message."
  (let ((count 0))
    (if process-alist
        (let (buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (assq (proced-pid-at-point) process-alist)
                (insert proced-marker-char)
                (delete-char 1)
                (setq count (1+ count)))
              (forward-line)))))
    (unless quiet
      (proced-success-message "Marked" count))))

;; Mostly analog of `dired-do-kill-lines'.
;; However, for negative args the target lines of `dired-do-kill-lines'
;; include the current line, whereas `dired-mark' for negative args operates
;; on the preceding lines.  Here we are consistent with `dired-mark'.
(defun proced-omit-processes (&optional arg quiet)
  "Omit marked processes.
With prefix ARG, omit that many lines starting with the current line.
\(A negative argument omits backward.)
If `transient-mark-mode' is turned on and the region is active,
omit the processes in region.
If QUIET is non-nil suppress status message.
Returns count of omitted lines."
  (interactive "P")
  (let ((mark-re (proced-marker-regexp))
        (count 0)
        buffer-read-only)
    (cond ((use-region-p) ;; Omit active region
           (let ((lines (count-lines (region-beginning) (region-end))))
             (save-excursion
               (goto-char (region-beginning))
               (while (< count lines)
                 (proced-omit-process)
                 (setq count (1+ count))))))
          ((not arg) ;; Omit marked lines
           (save-excursion
             (goto-char (point-min))
             (while (and (not (eobp))
                         (re-search-forward mark-re nil t))
               (proced-omit-process)
               (setq count (1+ count)))))
          ((< 0 arg) ;; Omit forward
           (while (and (not (eobp)) (< count arg))
             (proced-omit-process)
             (setq count (1+ count))))
          ((< arg 0) ;; Omit backward
           (while (and (not (bobp)) (< count (- arg)))
             (forward-line -1)
             (proced-omit-process)
             (setq count (1+ count)))))
    (unless (zerop count) (proced-move-to-goal-column))
    (unless quiet (proced-success-message "Omitted" count))
    count))

(defun proced-omit-process ()
  "Omit process from listing point is on.
Update `proced-process-alist' accordingly."
  (setq proced-process-alist
        (assq-delete-all (proced-pid-at-point) proced-process-alist))
  (delete-region (line-beginning-position)
                 (save-excursion (forward-line) (point))))

;;; Filtering

(defun proced-filter (process-alist filter-list)
  "Apply FILTER-LIST to PROCESS-ALIST.
Return the filtered process list."
  (if (symbolp filter-list)
      (setq filter-list (cdr (assq filter-list proced-filter-alist))))
  (dolist (filter filter-list)
    (let (new-alist)
      (cond ( ;; apply function to entire process list
             (eq (car filter) 'fun-all)
             (setq new-alist (funcall (cdr filter) process-alist)))
            ( ;; apply predicate to each list of attributes
             (eq (car filter) 'function)
             (dolist (process process-alist)
               (if (funcall (car filter) (cdr process))
                   (push process new-alist))))
            (t ;; apply predicate to specified attribute
             (let ((fun (if (stringp (cdr filter))
                            `(lambda (val)
                               (string-match ,(cdr filter) val))
                          (cdr filter)))
                   value)
               (dolist (process process-alist)
                 (setq value (cdr (assq (car filter) (cdr process))))
                 (if (and value (funcall fun value))
                     (push process new-alist))))))
      (setq process-alist new-alist)))
  process-alist)

(defun proced-filter-interactive (scheme)
  "Filter Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no filtering.
Set variable `proced-filter' to SCHEME.  Revert listing."
  (interactive
   (let ((scheme (completing-read "Filter: "
                                  proced-filter-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme)))))
  ;; only update if necessary
  (unless (eq proced-filter scheme)
    (setq proced-filter scheme)
    (proced-update t)))

(defun proced-filter-parents (process-alist pid &optional omit-pid)
  "For PROCESS-ALIST return list of parent processes of PID.
This list includes PID unless OMIT-PID is non-nil."
  (let ((parent-list (unless omit-pid (list (assq pid process-alist))))
        (process (assq pid process-alist))
        ppid)
    (while (and (setq ppid (cdr (assq 'ppid (cdr process))))
                ;; Ignore a PPID that equals PID.
                (/= ppid pid)
                ;; Accept only PPIDs that correspond to members in PROCESS-ALIST.
                (setq process (assq ppid process-alist)))
      (setq pid ppid)
      (push process parent-list))
    parent-list))

(defun proced-filter-children (process-alist ppid &optional omit-ppid)
  "For PROCESS-ALIST return list of child processes of PPID.
This list includes PPID unless OMIT-PPID is non-nil."
  (let ((proced-temp-alist (proced-children-alist process-alist))
        new-alist)
    (dolist (pid (proced-children-pids ppid))
      (push (assq pid process-alist) new-alist))
    (if omit-ppid
        (assq-delete-all ppid new-alist)
      new-alist)))

;;; Process tree

(defun proced-children-alist (process-alist)
  "Return children alist for PROCESS-ALIST.
The children alist has elements (PPID PID1 PID2 ...).
PPID is a parent PID.  PID1, PID2, ... are the child processes of PPID.
The children alist inherits the sorting order of PROCESS-ALIST.
The list of children does not include grandchildren."
  ;; The PPIDs inherit the sorting order of PROCESS-ALIST.
  (let ((process-tree (mapcar (lambda (a) (list (car a))) process-alist))
        ppid)
    (dolist (process process-alist)
      (setq ppid (cdr (assq 'ppid (cdr process))))
      (if (and ppid
               ;; Ignore a PPID that equals PID.
               (/= ppid (car process))
               ;; Accept only PPIDs that correspond to members in PROCESS-ALIST.
               (assq ppid process-alist))
          (let ((temp-alist process-tree) elt)
            (while (setq elt (pop temp-alist))
              (when (eq ppid (car elt))
                (setq temp-alist nil)
                (setcdr elt (cons (car process) (cdr elt))))))))
    ;; The child processes inherit the sorting order of PROCESS-ALIST.
    (setq process-tree
          (mapcar (lambda (a) (cons (car a) (nreverse (cdr a))))
                  process-tree))))

(defun proced-children-pids (ppid)
  "Return list of children PIDs of PPID (including PPID)."
  (let ((cpids (cdr (assq ppid proced-temp-alist))))
    (if cpids
        (cons ppid (apply 'append (mapcar 'proced-children-pids cpids)))
      (list ppid))))

(defun proced-process-tree (process-alist)
  "Return process tree for PROCESS-ALIST.
It is an alist of alists where the car of each alist is a parent process
and the cdr is a list of child processes according to the ppid attribute
of these processes.
The process tree inherits the sorting order of PROCESS-ALIST."
  (let ((proced-temp-alist (proced-children-alist process-alist))
        pid-alist proced-process-tree)
    (while (setq pid-alist (pop proced-temp-alist))
      (push (proced-process-tree-internal pid-alist) proced-process-tree))
    (nreverse proced-process-tree)))

(defun proced-process-tree-internal (pid-alist)
  "Helper function for `proced-process-tree'."
  (let ((cpid-list (cdr pid-alist)) cpid-alist cpid)
    (while (setq cpid (car cpid-list))
      (if (setq cpid-alist (assq cpid proced-temp-alist))
          ;; Unprocessed part of process tree that needs to be
          ;; analyzed recursively.
          (progn
            (setq proced-temp-alist
                  (assq-delete-all cpid proced-temp-alist))
            (setcar cpid-list (proced-process-tree-internal cpid-alist)))
        ;; We already processed this subtree and take it "as is".
        (setcar cpid-list (assq cpid proced-process-tree))
        (setq proced-process-tree
              (assq-delete-all cpid proced-process-tree)))
      (pop cpid-list)))
  pid-alist)

(defun proced-toggle-tree (arg)
  "Toggle the display of the process listing as process tree.
With prefix ARG, display as process tree if ARG is positive, otherwise
do not display as process tree.  Sets the variable `proced-tree-flag'.

The process tree is generated from the selected processes in the
Proced buffer (that is, the processes in `proced-process-alist').
All processes that do not have a parent process in this list
according to their ppid attribute become the root of a process tree.
Each parent process is followed by its child processes.
The process tree inherits the chosen sorting order of the process listing,
that is, child processes of the same parent process are sorted using
the selected sorting order."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq proced-tree-flag
        (cond ((eq arg 'toggle) (not proced-tree-flag))
              (arg (> (prefix-numeric-value arg) 0))
              (t (not proced-tree-flag))))
  (proced-update)
  (message "Proced process tree display %s"
           (if proced-tree-flag "enabled" "disabled")))

(defun proced-tree (process-alist)
  "Rearrange PROCESS-ALIST as process tree.
If `proced-tree-flag' is non-nil, rearrange PROCESS-ALIST such that
every processes is followed by its child processes.  Each process
gets a tree attribute that specifies the depth of the process in the tree.
A root process is a process with no parent within PROCESS-ALIST according
to its value of the ppid attribute.  It has depth 0.

If `proced-tree-flag' is nil, remove the tree attribute.
Return the rearranged process list."
  (if proced-tree-flag
      ;; add tree attribute
      (let ((process-tree (proced-process-tree process-alist))
            (proced-tree-depth 0)
            (proced-temp-alist process-alist)
            proced-process-tree pt)
        (while (setq pt (pop process-tree))
          (proced-tree-insert pt))
        (nreverse proced-process-tree))
    ;; remove tree attribute
    (let ((process-alist process-alist))
      (while process-alist
        (setcar process-alist
                (assq-delete-all 'tree (car process-alist)))
        (pop process-alist)))
    process-alist))

(defun proced-tree-insert (process-tree)
  "Helper function for `proced-tree'."
  (let ((pprocess (assq (car process-tree) proced-temp-alist)))
    (push (append (list (car pprocess))
                  (list (cons 'tree proced-tree-depth))
                  (cdr pprocess))
          proced-process-tree)
    (if (cdr process-tree)
        (let ((proced-tree-depth (1+ proced-tree-depth)))
          (mapc 'proced-tree-insert (cdr process-tree))))))

;; Refining

;; Filters are used to select the processes in a new listing.
;; Refiners are used to narrow down further (interactively) the processes
;; in an existing listing.

(defun proced-refine (&optional event)
  "Refine Proced listing by comparing with the attribute value at point.
Optional EVENT is the location of the Proced field.

Refinement is controlled by the REFINER defined for each attribute ATTR
in `proced-grammar-alist'.

If REFINER is a list of flags and point is on a process's value of ATTR,
this command compares the value of ATTR of every process with the value
of ATTR of the process at the position of point.

The predicate for the comparison of two ATTR values is defined
in `proced-grammar-alist'.  For each return value of the predicate
a refine flag is defined in `proced-grammar-alist'.  One can select
processes for which the value of ATTR is \"less than\", \"equal\",
and / or \"larger\" than ATTR of the process point is on.  A process
is included in the new listing if the refine flag for the corresponding
return value of the predicate is non-nil.
The help-echo string for `proced-refine' uses \"+\" or \"-\" to indicate
the current values of these refine flags.

If REFINER is a cons pair (FUNCTION . HELP-ECHO), FUNCTION is called
with one argument, the PID of the process at the position of point.
The function must return a list of PIDs that is used for the refined
listing.  HELP-ECHO is a string that is shown when mouse is over this field.

This command refines an already existing process listing generated initially
based on the value of the variable `proced-filter'.  It does not change
this variable.  It does not revert the listing.  If you frequently need
a certain refinement, consider defining a new filter in `proced-filter-alist'."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let ((key (get-text-property (point) 'proced-key))
        (pid (get-text-property (point) 'proced-pid)))
    (if (and key pid)
        (let* ((grammar (assq key proced-grammar-alist))
               (refiner (nth 7 grammar)))
          (when refiner
            (cond ((functionp (car refiner))
                   (setq proced-process-alist (funcall (car refiner) pid)))
                  ((consp refiner)
                   (let ((predicate (nth 4 grammar))
                         (ref (cdr (assq key (cdr (assq pid proced-process-alist)))))
                         val new-alist)
                     (dolist (process proced-process-alist)
                       (setq val (funcall predicate (cdr (assq key (cdr process))) ref))
                       (if (cond ((not val) (nth 2 refiner))
                                 ((eq val 'equal) (nth 1 refiner))
                                 (val (car refiner)))
                           (push process new-alist)))
                     (setq proced-process-alist new-alist))))
            ;; Do not revert listing.
            (proced-update)))
      (message "No refiner defined here."))))

;; Proced predicates for sorting and filtering are based on a three-valued
;; logic:
;; Predicates take two arguments P1 and P2, the corresponding attribute
;; values of two processes.  Predicates should return 'equal if P1 has
;; same rank like P2.  Any other non-nil value says that P1 is "less than" P2,
;; or nil if not.

(defun proced-< (num1 num2)
  "Return t if NUM1 less than NUM2.
Return `equal' if NUM1 equals NUM2.  Return nil if NUM1 greater than NUM2."
  (if (= num1 num2)
      'equal
    (< num1 num2)))

(defun proced-string-lessp (s1 s2)
  "Return t if string S1 is less than S2 in lexicographic order.
Return `equal' if S1 and S2 have identical contents.
Return nil otherwise."
  (if (string= s1 s2)
      'equal
    (string-lessp s1 s2)))

(defun proced-time-lessp (t1 t2)
  "Return t if time value T1 is less than time value T2.
Return `equal' if T1 equals T2.  Return nil otherwise."
  (with-decoded-time-value ((high1 low1 micro1 t1)
			    (high2 low2 micro2 t2))
    (cond ((< high1 high2))
          ((< high2 high1) nil)
          ((< low1 low2))
          ((< low2 low1) nil)
          ((< micro1 micro2))
          ((< micro2 micro1) nil)
          (t 'equal))))

;;; Sorting

(defsubst proced-xor (b1 b2)
  "Return the logical exclusive or of args B1 and B2."
  (and (or b1 b2)
       (not (and b1 b2))))

(defun proced-sort-p (p1 p2)
  "Predicate for sorting processes P1 and P2."
  (if (not (cdr proced-sort-internal))
      ;; only one predicate: fast scheme
      (let* ((sorter (car proced-sort-internal))
             (k1 (cdr (assq (car sorter) (cdr p1))))
             (k2 (cdr (assq (car sorter) (cdr p2)))))
        ;; if the attributes are undefined, we should really abort sorting
        (if (and k1 k2)
            (proced-xor (funcall (nth 1 sorter) k1 k2)
                        (nth 2 sorter))))
    (let ((sort-list proced-sort-internal) sorter predicate k1 k2)
      (catch 'done
        (while (setq sorter (pop sort-list))
          (setq k1 (cdr (assq (car sorter) (cdr p1)))
                k2 (cdr (assq (car sorter) (cdr p2)))
                predicate
                (if (and k1 k2)
                    (funcall (nth 1 sorter) k1 k2)))
          (if (not (eq predicate 'equal))
              (throw 'done (proced-xor predicate (nth 2 sorter)))))
        (eq t predicate)))))

(defun proced-sort (process-alist sorter descend)
  "Sort PROCESS-ALIST using scheme SORTER.
SORTER is a scheme like `proced-sort'.
DESCEND is non-nil if the first element of SORTER is sorted
in descending order.
Return the sorted process list."
  ;; translate SORTER into a list of lists (KEY PREDICATE REVERSE)
  (setq proced-sort-internal
        (mapcar (lambda (arg)
                  (let ((grammar (assq arg proced-grammar-alist)))
                    (unless (nth 4 grammar)
                      (error "Attribute %s not sortable" (car grammar)))
                    (list arg (nth 4 grammar) (nth 5 grammar))))
                (cond ((listp sorter) sorter)
                      ((and (symbolp sorter)
                            (nth 6 (assq sorter proced-grammar-alist))))
                      ((symbolp sorter) (list sorter))
                      (t (error "Sorter undefined %s" sorter)))))
  (if proced-sort-internal
      (progn
        ;; splice DESCEND into the list
        (setcar proced-sort-internal
                (list (caar proced-sort-internal)
                      (nth 1 (car proced-sort-internal)) descend))
        (sort process-alist 'proced-sort-p))
    process-alist))

(defun proced-sort-interactive (scheme &optional arg)
  "Sort Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no sorting.

Prefix ARG controls sort order:
- If prefix ARG is positive (negative), sort in ascending (descending) order.
- If ARG is nil or 'no-arg and SCHEME is equal to the previous sorting scheme,
  reverse the sorting order.
- If ARG is nil or 'no-arg and SCHEME differs from the previous sorting scheme,
  adopt the sorting order defined for SCHEME in `proced-grammar-alist'.

Set variable `proced-sort' to SCHEME.  The current sort scheme is displayed
in the mode line, using \"+\" or \"-\" for ascending or descending order."
  (interactive
   (let* (choices
          (scheme (completing-read "Sort attribute: "
                                   (dolist (grammar proced-grammar-alist choices)
                                     (if (nth 4 grammar)
                                         (push (list (car grammar)) choices)))
                                   nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           ;; like 'toggle in `define-derived-mode'
           (or current-prefix-arg 'no-arg))))

  (setq proced-descend
        ;; If `proced-sort-interactive' is called repeatedly for the same
        ;; sort key, the sort order is reversed.
        (cond ((and (eq arg 'no-arg) (equal proced-sort scheme))
               (not proced-descend))
              ((eq arg 'no-arg)
               (nth 5 (assq (if (consp scheme) (car scheme) scheme)
                            proced-grammar-alist)))
              (arg (< (prefix-numeric-value arg) 0))
              ((equal proced-sort scheme)
               (not proced-descend))
              (t (nth 5 (assq (if (consp scheme) (car scheme) scheme)
                                   proced-grammar-alist))))
        proced-sort scheme)
  (proced-update))

(defun proced-sort-pcpu (&optional arg)
  "Sort Proced buffer by percentage CPU time (%CPU).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)))
  (proced-sort-interactive 'pcpu arg))

(defun proced-sort-pmem (&optional arg)
  "Sort Proced buffer by percentage memory usage (%MEM).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)))
  (proced-sort-interactive 'pmem arg))

(defun proced-sort-pid (&optional arg)
  "Sort Proced buffer by PID.
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)))
  (proced-sort-interactive 'pid arg))

(defun proced-sort-start (&optional arg)
  "Sort Proced buffer by time the command started (START).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)))
  (proced-sort-interactive 'start arg))

(defun proced-sort-time (&optional arg)
  "Sort Proced buffer by CPU time (TIME).
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)))
  (proced-sort-interactive 'time arg))

(defun proced-sort-user (&optional arg)
  "Sort Proced buffer by USER.
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list (or current-prefix-arg 'no-arg)))
  (proced-sort-interactive 'user arg))

(defun proced-sort-header (event &optional arg)
  "Sort Proced listing based on an attribute.
EVENT is a mouse event with starting position in the header line.
It is converted to the corresponding attribute key.
This command updates the variable `proced-sort'.
Prefix ARG controls sort order, see `proced-sort-interactive'."
  (interactive (list last-input-event (or last-prefix-arg 'no-arg)))
  (let ((start (event-start event))
        col key)
    (save-selected-window
      (select-window (posn-window start))
      (setq col (+ (1- (car (posn-actual-col-row start)))
                   (window-hscroll)))
      (when (and (<= 0 col) (< col (length proced-header-line)))
        (setq key (get-text-property col 'proced-key proced-header-line))
        (if key
            (proced-sort-interactive key arg)
          (message "No sorter defined here."))))))

;;; Formatting

(defun proced-format-time (time)
  "Format time interval TIME."
  (let* ((ftime (float-time time))
         (days (truncate ftime 86400))
         (ftime (mod ftime 86400))
         (hours (truncate ftime 3600))
         (ftime (mod ftime 3600))
         (minutes (truncate ftime 60))
         (seconds (mod ftime 60)))
    (cond ((< 0 days)
           (format "%d-%02d:%02d:%02d" days hours minutes seconds))
          ((< 0 hours)
           (format "%02d:%02d:%02d" hours minutes seconds))
          (t
           (format "%02d:%02d" minutes seconds)))))

(defun proced-format-start (start)
  "Format time START.
The return string is always 6 characters wide."
  (let ((d-start (decode-time start))
        (d-current (decode-time)))
    (cond ( ;; process started in previous years
           (< (nth 5 d-start) (nth 5 d-current))
           (format-time-string "  %Y" start))
          ;; process started today
          ((and (= (nth 3 d-start) (nth 3 d-current))
                (= (nth 4 d-start) (nth 4 d-current)))
           (format-time-string " %H:%M" start))
          (t ;; process started this year
           (format-time-string "%b %e" start)))))

(defun proced-format-ttname (ttname)
  "Format attribute TTNAME, omitting path \"/dev/\"."
  ;; Does this work for all systems?
  (substring ttname (if (string-match "\\`/dev/" ttname)
                        (match-end 0) 0)))

(defun proced-format-tree (tree)
  "Format attribute TREE."
  (concat (make-string tree ?\s) (number-to-string tree)))

;; Proced assumes that every process occupies only one line in the listing.
(defun proced-format-args (args)
  "Format attribute ARGS.
Replace newline characters by \"^J\" (two characters)."
  (replace-regexp-in-string "\n" "^J" args))

(defun proced-format (process-alist format)
  "Display PROCESS-ALIST using FORMAT."
  (if (symbolp format)
      (setq format (cdr (assq format proced-format-alist))))

  ;; Not all systems give us all attributes.  We take `emacs-pid' as a
  ;; representative process PID.  If FORMAT contains a list of alternative
  ;; attributes, we take the first attribute that is non-nil for `emacs-pid'.
  ;; If none of the alternatives is non-nil, the attribute is ignored
  ;; in the listing.
  (let ((standard-attributes
         (car (proced-process-attributes (list (emacs-pid)))))
        new-format fmi)
    (if (and proced-tree-flag
             (assq 'ppid standard-attributes))
        (push (cons 'tree 0) standard-attributes))
    (dolist (fmt format)
      (if (symbolp fmt)
          (if (assq fmt standard-attributes)
              (push fmt new-format))
        (while (setq fmi (pop fmt))
          (when (assq fmi standard-attributes)
            (push fmi new-format)
            (setq fmt nil)))))
    (setq format (nreverse new-format)))

  (insert (make-string (length process-alist) ?\n))
  (let ((whitespace " ") (unknown "?")
        (sort-key (if (consp proced-sort) (car proced-sort) proced-sort))
        header-list grammar)
    ;; Loop over all attributes
    (while (setq grammar (assq (pop format) proced-grammar-alist))
      (let* ((key (car grammar))
             (fun (cond ((stringp (nth 2 grammar))
                         `(lambda (arg) (format ,(nth 2 grammar) arg)))
                        ((not (nth 2 grammar)) 'identity)
                        ( t (nth 2 grammar))))
             (whitespace (if format whitespace ""))
             ;; Text properties:
             ;; We use the text property `proced-key' to store in each
             ;; field the corresponding key.
             ;; Of course, the sort predicate appearing in help-echo
             ;; is only part of the story.  But it gives the main idea.
             (hprops
              (if (nth 4 grammar)
                  (let ((descend (if (eq key sort-key) proced-descend (nth 5 grammar))))
                    `(proced-key ,key mouse-face highlight
                                 help-echo ,(format proced-header-help-echo
                                                    (if descend "-" "+")
                                                    (nth 1 grammar)
                                                    (if descend "descending" "ascending"))))))
             (refiner (nth 7 grammar))
             (fprops
              (cond ((functionp (car refiner))
                     `(proced-key ,key mouse-face highlight
                                  help-echo ,(format "mouse-2, RET: %s"
                                                     (nth 1 refiner))))
                    ((consp refiner)
                     `(proced-key ,key mouse-face highlight
                                  help-echo ,(format "mouse-2, RET: refine by attribute %s %s"
                                                     (nth 1 grammar)
                                                     (mapconcat (lambda (s)
                                                                  (if s "+" "-"))
                                                                refiner ""))))))
             value)

        ;; highlight the header of the sort column
        (if (eq key sort-key)
            (setq hprops (append '(face proced-sort-header) hprops)))
        (goto-char (point-min))
        (cond ( ;; fixed width of output field
               (numberp (nth 3 grammar))
               (dolist (process process-alist)
                 (end-of-line)
                 (setq value (cdr (assq key (cdr process))))
                 (insert (if value
                             (apply 'propertize (funcall fun value) fprops)
                           (format (concat "%" (number-to-string (nth 3 grammar)) "s")
                                   unknown))
                         whitespace)
                 (forward-line))
               (push (format (concat "%" (number-to-string (nth 3 grammar)) "s")
                             (apply 'propertize (nth 1 grammar) hprops))
                     header-list))

              ( ;; last field left-justified
               (and (not format) (eq 'left (nth 3 grammar)))
               (dolist (process process-alist)
                 (end-of-line)
                 (setq value (cdr (assq key (cdr process))))
                 (insert (if value (apply 'propertize (funcall fun value) fprops)
                           unknown))
                 (forward-line))
               (push (apply 'propertize (nth 1 grammar) hprops) header-list))

              (t ;; calculated field width
               (let ((width (length (nth 1 grammar)))
                     field-list value)
                 (dolist (process process-alist)
                   (setq value (cdr (assq key (cdr process))))
                   (if value
                       (setq value (apply 'propertize (funcall fun value) fprops)
                             width (max width (length value))
                             field-list (cons value field-list))
                     (push unknown field-list)
                     (setq width (max width (length unknown)))))
                 (let ((afmt (concat "%" (if (eq 'left (nth 3 grammar)) "-" "")
                                     (number-to-string width) "s")))
                   (push (format afmt (apply 'propertize (nth 1 grammar) hprops))
                         header-list)
                   (dolist (value (nreverse field-list))
                     (end-of-line)
                     (insert (format afmt value) whitespace)
                     (forward-line))))))))

    ;; final cleanup
    (goto-char (point-min))
    (dolist (process process-alist)
      ;; We use the text property `proced-pid' to store in each line
      ;; the corresponding pid
      (put-text-property (point) (line-end-position) 'proced-pid (car process))
      (forward-line))
    ;; Set header line
    (setq proced-header-line
          (mapconcat 'identity (nreverse header-list) whitespace))
    (if (string-match "[ \t]+$" proced-header-line)
        (setq proced-header-line (substring proced-header-line 0
                                            (match-beginning 0))))
    ;; (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun proced-format-interactive (scheme &optional revert)
  "Format Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no formatting.
Set variable `proced-format' to SCHEME.
With prefix REVERT non-nil revert listing."
  (interactive
   (let ((scheme (completing-read "Format: "
                                  proced-format-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           current-prefix-arg)))
  ;; only update if necessary
  (when (or (not (eq proced-format scheme)) revert)
    (setq proced-format scheme)
    (proced-update revert)))

;; generate listing

(defun proced-process-attributes (&optional pid-list)
  "Return alist of attributes for each system process.
This alist can be customized via `proced-custom-attributes'.
Optional arg PID-LIST is a list of PIDs of system process that are analyzed.
If no attributes are known for a process (possibly because it already died)
the process is ignored."
  ;; Should we make it customizable whether processes with empty attribute
  ;; lists are ignored?  When would such processes be of interest?
  (let (process-alist attributes attr)
    (dolist (pid (or pid-list (list-system-processes)) process-alist)
      (when (setq attributes (process-attributes pid))
        (setq attributes (cons (cons 'pid pid) attributes))
        (dolist (fun proced-custom-attributes)
          (if (setq attr (funcall fun attributes))
              (push attr attributes)))
        (push (cons pid attributes) process-alist)))))

(defun proced-update (&optional revert quiet)
  "Update the Proced process information.  Preserves point and marks.
With prefix REVERT non-nil, revert listing.
Suppress status information if QUIET is nil.
After updating a displayed Proced buffer run the normal hook
`proced-post-display-hook'."
  ;; This is the main function that generates and updates the process listing.
  (interactive "P")
  (setq revert (or revert (not proced-process-alist)))
  (or quiet (message (if revert "Updating process information..."
                       "Updating process display...")))
  (if revert ;; evaluate all processes
      (setq proced-process-alist (proced-process-attributes)))
  ;; filtering and sorting
  (setq proced-process-alist
        (proced-sort (proced-filter proced-process-alist proced-filter)
                     proced-sort proced-descend))

  ;; display as process tree?
  (setq proced-process-alist
        (proced-tree proced-process-alist))

  ;; It is useless to keep undo information if we revert, filter, or
  ;; refine the listing so that `proced-process-alist' has changed.
  ;; We could keep the undo information if we only re-sort the buffer.
  ;; Would that be useful?  Re-re-sorting is easy, too.
  (if (consp buffer-undo-list)
      (setq buffer-undo-list nil))
  (let ((buffer-undo-list t)
        ;; If point is on a field, we try to return point to that field.
        ;; Otherwise we try to return to the same column
        (old-pos (let ((pid (proced-pid-at-point))
                       (key (get-text-property (point) 'proced-key)))
                   (list pid key ; can both be nil
                         (if key
                             (if (get-text-property (1- (point)) 'proced-key)
                                 (- (point) (previous-single-property-change
                                             (point) 'proced-key))
                               0)
                           (current-column)))))
        buffer-read-only mp-list)
    ;; remember marked processes (whatever the mark was)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\S-\\)" nil t)
      (push (cons (save-match-data (proced-pid-at-point))
                  (match-string-no-properties 1)) mp-list))

    ;; generate listing
    (erase-buffer)
    (proced-format proced-process-alist proced-format)
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line))
    (setq proced-header-line (concat "  " proced-header-line))
    (if revert (set-buffer-modified-p nil))

    ;; set `goal-column'
    (let ((grammar (assq proced-goal-attribute proced-grammar-alist)))
      (setq goal-column ;; set to nil if no match
            (if (and grammar
                     (not (zerop (buffer-size)))
                     (string-match (regexp-quote (nth 1 grammar))
                                   proced-header-line))
                (if (nth 3 grammar)
                    (match-beginning 0)
                  (match-end 0)))))

    ;; Restore process marks and buffer position (if possible).
    ;; Sometimes this puts point in the middle of the proced buffer
    ;; where it is not interesting.  Is there a better / more flexible solution?
    (goto-char (point-min))
    (let (pid mark new-pos)
      (if (or mp-list (car old-pos))
          (while (not (eobp))
            (setq pid (proced-pid-at-point))
            (when (setq mark (assq pid mp-list))
              (insert (cdr mark))
              (delete-char 1)
              (beginning-of-line))
            (when (eq (car old-pos) pid)
              (if (nth 1 old-pos)
                  (let ((limit (line-end-position)) pos)
                    (while (and (not new-pos)
                                (setq pos (next-property-change (point) nil limit)))
                      (goto-char pos)
                      (when (eq (nth 1 old-pos)
                                (get-text-property (point) 'proced-key))
                        (forward-char (min (nth 2 old-pos)
                                           (- (next-property-change (point))
                                              (point))))
                        (setq new-pos (point))))
                    (unless new-pos
                      ;; we found the process, but the field of point
                      ;; is not listed anymore
                      (setq new-pos (proced-move-to-goal-column))))
                (setq new-pos (min (+ (line-beginning-position) (nth 2 old-pos))
                                   (line-end-position)))))
            (forward-line)))
      (if new-pos
          (goto-char new-pos)
        (goto-char (point-min))
        (proced-move-to-goal-column)))
    ;; update modeline
    ;; Does the long `mode-name' clutter the modeline?  It would be nice
    ;; to have some other location for displaying the values of the various
    ;; flags that affect the behavior of proced (flags one might want
    ;; to change on the fly).  Where??
    (setq mode-name
          (concat "Proced"
                  (if proced-filter
                      (concat ": " (symbol-name proced-filter))
                    "")
                  (if proced-sort
                      (let* ((key (if (consp proced-sort) (car proced-sort)
                                    proced-sort))
                             (grammar (assq key proced-grammar-alist)))
                        (concat " by " (if proced-descend "-" "+")
                                (nth 1 grammar)))
                    "")))
    (force-mode-line-update)
    ;; run `proced-post-display-hook' only for a displayed buffer.
    (if (get-buffer-window) (run-hooks 'proced-post-display-hook))
    ;; done
    (or quiet (input-pending-p)
        (message (if revert "Updating process information...done."
                   "Updating process display...done.")))))

(defun proced-revert (&rest _args)
  "Reevaluate the process listing based on the currently running processes.
Preserves point and marks."
  (proced-update t))

(defun proced-send-signal (&optional signal)
  "Send a SIGNAL to the marked processes.
If no process is marked, operate on current process.
SIGNAL may be a string (HUP, INT, TERM, etc.) or a number.
If SIGNAL is nil display marked processes and query interactively for SIGNAL.
After sending the signal, this command runs the normal hook
`proced-after-send-signal-hook'."
  (interactive)
  (let ((regexp (proced-marker-regexp))
        process-alist)
    ;; collect marked processes
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (proced-pid-at-point)
                    ;; How much info should we collect here?
                    (buffer-substring-no-properties
                     (+ 2 (line-beginning-position))
                     (line-end-position)))
              process-alist)))
    (setq process-alist
          (if process-alist
              (nreverse process-alist)
            ;; take current process
            (list (cons (proced-pid-at-point)
                        (buffer-substring-no-properties
                         (+ 2 (line-beginning-position))
                         (line-end-position))))))
    (unless signal
      ;; Display marked processes (code taken from `dired-mark-pop-up').
      (let ((bufname  " *Marked Processes*") ; use leading space in buffer name
					; to make this buffer ephemeral
            (header-line (substring-no-properties proced-header-line)))
        (with-current-buffer (get-buffer-create bufname)
          (setq truncate-lines t
                proced-header-line header-line ; inherit header line
                header-line-format '(:eval (proced-header-line)))
          (add-hook 'post-command-hook 'force-mode-line-update nil t)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (buffer-disable-undo)
            (setq buffer-read-only t)
            (dolist (process process-alist)
              (insert "  " (cdr process) "\n"))
            (delete-char -1)
            (goto-char (point-min)))
          (save-window-excursion
            ;; Analogous to `dired-pop-to-buffer'
            ;; Don't split window horizontally.  (Bug#1806)
            (let (split-width-threshold)
              (pop-to-buffer (current-buffer)))
            (fit-window-to-buffer (get-buffer-window) nil 1)
            (let* ((completion-ignore-case t)
                   (pnum (if (= 1 (length process-alist))
                             "1 process"
                           (format "%d processes" (length process-alist))))
                   (completion-extra-properties
                    '(:annotation-function
                      (lambda (s) (cdr (assoc s proced-signal-list))))))
              (setq signal
                    (completing-read (concat "Send signal [" pnum
                                             "] (default TERM): ")
                                     proced-signal-list
                                     nil nil nil nil "TERM")))))))
    ;; send signal
    (let ((count 0)
          failures)
      ;; Why not always use `signal-process'?  See
      ;; http://lists.gnu.org/archive/html/emacs-devel/2008-03/msg02955.html
      (if (functionp proced-signal-function)
          ;; use built-in `signal-process'
          (let ((signal (if (stringp signal)
                            (if (string-match "\\`[0-9]+\\'" signal)
                                (string-to-number signal)
                              (make-symbol signal))
                          signal)))   ; number
            (dolist (process process-alist)
              (condition-case err
                  (if (zerop (funcall
                              proced-signal-function (car process) signal))
                      (setq count (1+ count))
                    (proced-log "%s\n" (cdr process))
                    (push (cdr process) failures))
                (error ; catch errors from failed signals
                 (proced-log "%s\n" err)
                 (proced-log "%s\n" (cdr process))
                 (push (cdr process) failures)))))
        ;; use external system call
        (let ((signal (concat "-" (if (numberp signal)
                                      (number-to-string signal) signal))))
          (dolist (process process-alist)
            (with-temp-buffer
              (condition-case nil
                  (if (zerop (call-process
                              proced-signal-function nil t nil
                              signal (number-to-string (car process))))
                      (setq count (1+ count))
                    (proced-log (current-buffer))
                    (proced-log "%s\n" (cdr process))
                    (push (cdr process) failures))
                (error ; catch errors from failed signals
                 (proced-log (current-buffer))
                 (proced-log "%s\n" (cdr process))
                 (push (cdr process) failures)))))))
      (if failures
          ;; Proced error message are not always very precise.
          ;; Can we issue a useful one-line summary in the
          ;; message area (using FAILURES) if only one signal failed?
          (proced-log-summary
           signal
           (format "%d of %d signal%s failed"
                   (length failures) (length process-alist)
                   (if (= 1 (length process-alist)) "" "s")))
        (proced-success-message "Sent signal to" count)))
    ;; final clean-up
    (run-hooks 'proced-after-send-signal-hook)))

;; similar to `dired-why'
(defun proced-why ()
  "Pop up a buffer with error log output from Proced.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer proced-log-buffer)
      (save-selected-window
        ;; move `proced-log-buffer' to the front of the buffer list
        (select-window (display-buffer (get-buffer proced-log-buffer)))
        (setq truncate-lines t)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (goto-char (point-max))
        (forward-line -1)
        (backward-page 1)
        (recenter 0))))

;; similar to `dired-log'
(defun proced-log (log &rest args)
  "Log a message or the contents of a buffer.
If LOG is a string and there are more args, it is formatted with
those ARGS.  Usually the LOG string ends with a \\n.
End each bunch of errors with (proced-log t signal):
this inserts the current time, buffer and signal at the start of the page,
and \f (formfeed) at the end."
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create proced-log-buffer)
      (goto-char (point-max))
      (let (buffer-read-only)
	(cond ((stringp log)
	       (insert (if args
			   (apply 'format log args)
			 log)))
	      ((bufferp log)
	       (insert-buffer-substring log))
	      ((eq t log)
	       (backward-page 1)
	       (unless (bolp)
		 (insert "\n"))
	       (insert (current-time-string)
		       "\tBuffer `" (buffer-name obuf) "', "
                       (format "signal `%s'\n" (car args)))
	       (goto-char (point-max))
	       (insert "\f\n")))))))

;; similar to `dired-log-summary'
(defun proced-log-summary (signal string)
  "State a summary of SIGNAL's failures, in echo area and log buffer.
STRING is an overall summary of the failures."
  (message "Signal %s: %s--type ? for details" signal string)
  ;; Log a summary describing a bunch of errors.
  (proced-log (concat "\n" string "\n"))
  (proced-log t signal))

(defun proced-help ()
  "Provide help for the Proced user."
  (interactive)
  (proced-why)
  (if (eq last-command 'proced-help)
      (describe-mode)
    (message proced-help-string)))

(defun proced-undo ()
  "Undo in a Proced buffer.
This doesn't recover killed processes, it just undoes changes in the Proced
buffer.  You can use it to recover marks."
  (interactive)
  (let (buffer-read-only)
    (undo))
  (message "Change in Proced buffer undone.
Killed processes cannot be recovered by Emacs."))

(provide 'proced)

;;; proced.el ends here
