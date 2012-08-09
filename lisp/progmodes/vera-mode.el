;;; vera-mode.el --- major mode for editing Vera files

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author:      Reto Zimmermann <reto@gnu.org>
;; Maintainer:  Reto Zimmermann <reto@gnu.org>
;; Version:     2.28
;; Keywords:    languages vera
;; WWW:         http://www.iis.ee.ethz.ch/~zimmi/emacs/vera-mode.html

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file on 18/3/2008, and the maintainer agreed that when a bug is
;; filed in the Emacs bug reporting system against this file, a copy
;; of the bug report be sent to the maintainer's email address.

(defconst vera-version "2.18"
  "Vera Mode version number.")

(defconst vera-time-stamp "2007-06-21"
  "Vera Mode time stamp for last update.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides a simple Emacs major mode for editing Vera code.
;; It includes the following features:

;;   - Syntax highlighting
;;   - Indentation
;;   - Word/keyword completion
;;   - Block commenting
;;   - Works under GNU Emacs and XEmacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation

;; See comment string of function `vera-mode' or type `C-h m' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation

;; Prerequisites:  GNU Emacs 20.X/21.X, XEmacs 20.X/21.X

;; Put `vera-mode.el' into the `site-lisp' directory of your Emacs installation
;; or into an arbitrary directory that is added to the load path by the
;; following line in your Emacs start-up file (`.emacs'):

;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; If you already have the compiled `vera-mode.elc' file, put it in the same
;; directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  vera-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f batch-byte-compile vera-mode.el

;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):

;;   (autoload 'vera-mode "vera-mode" "Vera Mode" t)
;;   (setq auto-mode-alist (cons '("\\.vr[hi]?\\'" . vera-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup vera nil
  "Customizations for Vera Mode."
  :prefix "vera-"
  :version "22.2"
  :group 'languages)

(defcustom vera-basic-offset 2
  "*Amount of basic offset used for indentation."
  :type 'integer
  :group 'vera)

(defcustom vera-underscore-is-part-of-word nil
  "*Non-nil means consider the underscore character `_' as part of word.
An identifier containing underscores is then treated as a single word in
select and move operations.  All parts of an identifier separated by underscore
are treated as single words otherwise."
  :type 'boolean
  :group 'vera)

(defcustom vera-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line.
If nil, TAB always indents current line."
  :type 'boolean
  :group 'vera)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar vera-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Backspace/delete key bindings.
    (define-key map [backspace] 'backward-delete-char-untabify)
    (unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
      (define-key map [delete]       'delete-char)
      (define-key map [(meta delete)] 'kill-word))
    ;; Standard key bindings.
    (define-key map "\M-e"     'vera-forward-statement)
    (define-key map "\M-a"     'vera-backward-statement)
    (define-key map "\M-\C-e"  'vera-forward-same-indent)
    (define-key map "\M-\C-a"  'vera-backward-same-indent)
    ;; Mode specific key bindings.
    (define-key map "\C-c\t"   'indent-according-to-mode)
    (define-key map "\M-\C-\\" 'vera-indent-region)
    (define-key map "\C-c\C-c" 'vera-comment-uncomment-region)
    (define-key map "\C-c\C-f" 'vera-fontify-buffer)
    (define-key map "\C-c\C-v" 'vera-version)
    (define-key map "\M-\t"    'tab-to-tab-stop)
    ;; Electric key bindings.
    (define-key map "\t"       'vera-electric-tab)
    (define-key map "\r"       'vera-electric-return)
    (define-key map " "        'vera-electric-space)
    (define-key map "{"        'vera-electric-opening-brace)
    (define-key map "}"        'vera-electric-closing-brace)
    (define-key map "#"        'vera-electric-pound)
    (define-key map "*"        'vera-electric-star)
    (define-key map "/"        'vera-electric-slash)
    map)
  "Keymap for Vera Mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu

(require 'easymenu)

(easy-menu-define vera-mode-menu vera-mode-map
  "Menu keymap for Vera Mode."
  '("Vera"
    ["(Un)Comment Out Region"	vera-comment-uncomment-region (mark)]
    "--"
    ["Move Forward Statement"	vera-forward-statement t]
    ["Move Backward Statement"	vera-backward-statement t]
    ["Move Forward Same Indent" vera-forward-same-indent t]
    ["Move Backward Same Indent" vera-backward-same-indent t]
    "--"
    ["Indent Line"		indent-according-to-mode t]
    ["Indent Region"		vera-indent-region (mark)]
    ["Indent Buffer"		vera-indent-buffer t]
    "--"
    ["Fontify Buffer"		vera-fontify-buffer t]
    "--"
    ["Documentation"		describe-mode]
    ["Version"			vera-version t]
    ["Bug Report..."		vera-submit-bug-report t]
    "--"
    ("Options"
     ["Indentation Offset..." (customize-option 'vera-basic-offset) t]
     ["Underscore is Part of Word"
      (customize-set-variable 'vera-underscore-is-part-of-word
			      (not vera-underscore-is-part-of-word))
      :style toggle :selected vera-underscore-is-part-of-word]
     ["Use Intelligent Tab"
      (customize-set-variable 'vera-intelligent-tab
			      (not vera-intelligent-tab))
      :style toggle :selected vera-intelligent-tab]
     "--"
     ["Save Options" customize-save-customized t]
     "--"
     ["Customize..."		vera-customize t])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar vera-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; punctuation
    (modify-syntax-entry ?\# "."    syntax-table)
    (modify-syntax-entry ?\$ "."    syntax-table)
    (modify-syntax-entry ?\% "."    syntax-table)
    (modify-syntax-entry ?\& "."    syntax-table)
    (modify-syntax-entry ?\' "."    syntax-table)
    (modify-syntax-entry ?\* "."    syntax-table)
    (modify-syntax-entry ?\- "."    syntax-table)
    (modify-syntax-entry ?\+ "."    syntax-table)
    (modify-syntax-entry ?\. "."    syntax-table)
    (modify-syntax-entry ?\/ "."    syntax-table)
    (modify-syntax-entry ?\: "."    syntax-table)
    (modify-syntax-entry ?\; "."    syntax-table)
    (modify-syntax-entry ?\< "."    syntax-table)
    (modify-syntax-entry ?\= "."    syntax-table)
    (modify-syntax-entry ?\> "."    syntax-table)
    (modify-syntax-entry ?\\ "."    syntax-table)
    (modify-syntax-entry ?\| "."    syntax-table)
    ;; string
    (modify-syntax-entry ?\" "\""   syntax-table)
    ;; underscore
    (when vera-underscore-is-part-of-word
      (modify-syntax-entry ?\_ "w"    syntax-table))
    ;; escape
    (modify-syntax-entry ?\\ "\\"   syntax-table)
    ;; parentheses to match
    (modify-syntax-entry ?\( "()"   syntax-table)
    (modify-syntax-entry ?\) ")("   syntax-table)
    (modify-syntax-entry ?\[ "(]"   syntax-table)
    (modify-syntax-entry ?\] ")["   syntax-table)
    (modify-syntax-entry ?\{ "(}"   syntax-table)
    (modify-syntax-entry ?\} "){"   syntax-table)
    ;; comment
    (if (featurep 'xemacs)
	(modify-syntax-entry ?\/ ". 1456" syntax-table) ; XEmacs
      (modify-syntax-entry ?\/ ". 124b" syntax-table)) ; Emacs
    (modify-syntax-entry ?\* ". 23" syntax-table)
    ;; newline and CR
    (modify-syntax-entry ?\n "> b"    syntax-table)
    (modify-syntax-entry ?\^M "> b"   syntax-table)
    syntax-table)
  "Syntax table used in `vera-mode' buffers.")

(defvar vera-mode-ext-syntax-table
  (let ((syntax-table (copy-syntax-table vera-mode-syntax-table)))
    ;; extended syntax table including '_' (for simpler search regexps)
    (modify-syntax-entry ?_ "w" syntax-table)
    syntax-table)
  "Syntax table extended by `_' used in `vera-mode' buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition

;;;###autoload (add-to-list 'auto-mode-alist (cons (purecopy "\\.vr[hi]?\\'")  'vera-mode))

;;;###autoload
(define-derived-mode vera-mode prog-mode "Vera"
  "Major mode for editing Vera code.

Usage:
------

  INDENTATION:  Typing `TAB' at the beginning of a line indents the line.
    The amount of indentation is specified by option `vera-basic-offset'.
    Indentation can be done for an entire region \(`M-C-\\') or buffer (menu).
    `TAB' always indents the line if option `vera-intelligent-tab' is nil.

  WORD/COMMAND COMPLETION:  Typing `TAB' after a (not completed) word looks
    for a word in the buffer or a Vera keyword that starts alike, inserts it
    and adjusts case.  Re-typing `TAB' toggles through alternative word
    completions.

    Typing `TAB' after a non-word character inserts a tabulator stop (if not
    at the beginning of a line).  `M-TAB' always inserts a tabulator stop.

  COMMENTS:  `C-c C-c' comments out a region if not commented out, and
    uncomments a region if already commented out.

  HIGHLIGHTING (fontification):  Vera keywords, predefined types and
    constants, function names, declaration names, directives, as well as
    comments and strings are highlighted using different colors.

  VERA VERSION:  OpenVera 1.4 and Vera version 6.2.8.


Maintenance:
------------

To submit a bug report, use the corresponding menu entry within Vera Mode.
Add a description of the problem and include a reproducible test case.

Feel free to send questions and enhancement requests to <reto@gnu.org>.

Official distribution is at
URL `http://www.iis.ee.ethz.ch/~zimmi/emacs/vera-mode.html'


                                                  The Vera Mode Maintainer
                                               Reto Zimmermann <reto@gnu.org>

Key bindings:
-------------

\\{vera-mode-map}"
  ;; set local variables
  (require 'cc-cmds)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  (set (make-local-variable 'comment-end-skip) " *\\*+/\\| *\n")
  (set (make-local-variable 'comment-indent-function) 'c-comment-indent)
  (set (make-local-variable 'paragraph-start) "^$")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'vera-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; initialize font locking
  (set (make-local-variable 'font-lock-defaults)
       '(vera-font-lock-keywords nil nil ((?\_ . "w"))))
  ;; add menu (XEmacs)
  (easy-menu-add vera-mode-menu)
  ;; miscellaneous
  (message "Vera Mode %s.  Type C-c C-h for documentation." vera-version))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vera definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords

(defconst vera-keywords
  '(
    "after" "all" "any" "around" "assoc_index" "assoc_size" "async"
    "bad_state" "bad_trans" "before" "begin" "big_endian" "bind"
    "bin_activation" "bit_normal" "bit_reverse" "break" "breakpoint"
    "case" "casex" "casez" "class" "constraint" "continue"
    "coverage" "coverage_block" "coverage_def" "coverage_depth"
    "coverage_goal" "coverage_group" "coverage_option" "coverage_val"
    "cross_num_print_missing" "cross_auto_bin_max" "cov_comment"
    "default" "depth" "dist" "do"
    "else" "end" "enum" "exhaustive" "export" "extends" "extern"
    "for" "foreach" "fork" "function"
    "hdl_task" "hdl_node" "hide"
    "if" "illegal_self_transition" "illegal_state" "illegal_transition"
    "in" "interface" "invisible"
    "join"
    "little_endian" "local"
    "m_bad_state" "m_bad_trans" "m_state" "m_trans"
    "negedge" "new" "newcov" "non_rand" "none" "not" "null"
    "or" "ordered"
    "packed" "port" "posedge" "proceed" "prod" "prodget" "prodset"
    "program" "protected" "public"
    "rand" "randc" "randcase" "randseq" "repeat" "return" "rules"
    "sample" "sample_event" "shadow" "soft" "state" "static" "super"
    "task" "terminate" "this" "trans" "typedef"
    "unpacked"
    "var" "vca" "vector" "verilog_node" "verilog_task"
    "vhdl_node" "vhdl_task" "virtual" "virtuals" "visible" "void"
    "while" "wildcard" "with"
    )
  "List of Vera keywords.")

(defconst vera-types
  '(
    "integer" "bit" "reg" "string" "bind_var" "event"
    "inout" "input" "output"
    "ASYNC" "CLOCK"
    "NDRIVE" "NHOLD" "NRX" "NRZ" "NR0" "NR1" "NSAMPLE"
    "PDRIVE" "PHOLD" "PRX" "PRZ" "PR0" "PR1" "PSAMPLE"
    )
  "List of Vera predefined types.")

(defconst vera-q-values
  '(
    "gnr" "grx" "grz" "gr0" "gr1"
    "nr" "rx" "rz" "r0" "r1"
    "snr" "srx" "srz" "sr0" "sr1"
    )
  "List of Vera predefined VCA q_values.")

(defconst vera-functions
  '(
    ;; system functions and tasks
    "alloc"
    "call_func" "call_task" "cast_assign" "close_conn" "cm_coverage"
    "cm_get_coverage" "cm_get_limit"
    "coverage_backup_database_file" "coverage_save_database"
    "delay"
    "error" "error_mode" "error_wait" "exit"
    "fclose" "feof" "ferror" "fflush" "flag" "fopen" "fprintf" "freadb"
    "freadb" "freadh" "freadstr"
    "get_bind" "get_bind_id" "get_conn_err" "get_cycle" "get_env"
    "get_memsize" "get_plus_arg" "get_systime" "get_time" "get_time_unit"
    "getstate"
    "initstate"
    "lock_file"
    "mailbox_get" "mailbox_put" "mailbox_receive" "mailbox_send"
    "make_client" "make_server"
    "os_command"
    "printf" "psprintf"
    "query" "query_str" "query_x"
    "rand48" "random" "region_enter" "region_exit" "rewind"
    "semaphore_get" "semaphore_put" "setstate" "signal_connect" "simwave_plot"
    "srandom" "sprintf" "sscanf" "stop" "suspend_thread" "sync"
    "timeout" "trace" "trigger"
    "unit_delay" "unlock_file" "up_connections"
    "urand48" "urandom" "urandom_range"
    "vera_bit_reverse" "vera_crc" "vera_pack" "vera_pack_big_endian"
    "vera_plot" "vera_report_profile" "vera_unpack" "vera_unpack_big_endian"
    "vsv_call_func" "vsv_call_task" "vsv_close_conn" "vsv_get_conn_err"
    "vsv_make_client" "vsv_make_server" "vsv_up_connections"
    "vsv_wait_for_done" "vsv_wait_for_input"
    "wait_child" "wait_var"
    ;; class methods
    "Configure" "DisableTrigger" "DoAction" "EnableCount" "EnableTrigger"
    "Event" "GetAssert" "GetCount" "GetFirstAssert" "GetName" "GetNextAssert"
    "Wait"
    "atobin" "atohex" "atoi" "atooct"
    "backref" "bittostr" "capacity" "compare" "constraint_mode"
    "delete"
    "empty"
    "find" "find_index" "first" "first_index"
    "get_at_least" "get_auto_bin" "get_cov_weight" "get_coverage_goal"
    "get_cross_bin_max" "get_status" "get_status_msg" "getc"
    "hash"
    "icompare" "insert" "inst_get_at_least" "inst_get_auto_bin_max"
    "inst_get_collect" "inst_get_cov_weight" "inst_get_coverage_goal"
    "inst_getcross_bin_max" "inst_query" "inst_set_at_least"
    "inst_set_auto_bin_max" "inst_set_bin_activation" "inst_set_collect"
    "inst_set_cov_weight" "inst_set_coverage_goal" "inst_set_cross_bin_max"
    "itoa"
    "last" "last_index" "len" "load"
    "match" "max" "max_index" "min" "min_index"
    "object_compare" "object_copy" "object_print"
    "pack" "pick_index" "pop_back" "pop_front" "post_pack" "post_randomize"
    "post_unpack" "postmatch" "pre_pack" "pre_randomize" "prematch" "push_back"
    "push_front" "putc"
    "query" "query_str"
    "rand_mode" "randomize" "reserve" "reverse" "rsort"
    "search" "set_at_least" "set_auto_bin_max" "set_bin_activation"
    "set_cov_weight" "set_coverage_goal" "set_cross_bin_max" "set_name" "size"
    "sort" "substr" "sum"
    "thismatch" "tolower" "toupper"
    "unique_index" "unpack"
    ;; empty methods
    "new" "object_compare"
    "post_boundary" "post_pack" "post_randomize" "post_unpack" "pre-randomize"
    "pre_boundary" "pre_pack" "pre_unpack"
    )
  "List of Vera predefined system functions, tasks and class methods.")

(defconst vera-constants
  '(
    "ALL" "ANY"
    "BAD_STATE" "BAD_TRANS"
    "CALL" "CHECK" "CHGEDGE" "CLEAR" "COPY_NO_WAIT" "COPY_WAIT"
    "CROSS" "CROSS_TRANS"
    "DEBUG" "DELETE"
    "EC_ARRAYX" "EC_CODE_END" "EC_CONFLICT" "EC_EVNTIMOUT" "EC_EXPECT"
    "EC_FULLEXPECT" "EC_MBXTMOUT" "EC_NEXPECT" "EC_RETURN" "EC_RGNTMOUT"
    "EC_SCONFLICT" "EC_SEMTMOUT" "EC_SEXPECT" "EC_SFULLEXPECT" "EC_SNEXTPECT"
    "EC_USERSET" "EQ" "EVENT"
    "FAIL" "FIRST" "FORK"
    "GE" "GOAL" "GT" "HAND_SHAKE" "HI" "HIGH" "HNUM"
    "LE" "LIC_EXIT" "LIC_PRERR" "LIC_PRWARN" "LIC_WAIT" "LO" "LOAD" "LOW" "LT"
    "MAILBOX" "MAX_COM"
    "NAME" "NE" "NEGEDGE" "NEXT" "NO_OVERLAP" "NO_OVERLAP_STATE"
    "NO_OVERLAP_TRANS" "NO_VARS" "NO_WAIT" "NUM" "NUM_BIN" "NUM_DET"
    "OFF" "OK" "OK_LAST" "ON" "ONE_BLAST" "ONE_SHOT" "ORDER"
    "PAST_IT" "PERCENT" "POSEDGE" "PROGRAM"
    "RAWIN" "REGION" "REPORT"
    "SAMPLE" "SAVE" "SEMAPHORE" "SET" "SILENT" "STATE" "STR"
    "STR_ERR_OUT_OF_RANGE" "STR_ERR_REGEXP_SYNTAX" "SUM"
    "TRANS"
    "VERBOSE"
    "WAIT"
    "stderr" "stdin" "stdout"
    )
  "List of Vera predefined constants.")

(defconst vera-rvm-types
  '(
    "VeraListIterator_VeraListIterator_rvm_log"
    "VeraListIterator_rvm_data" "VeraListIterator_rvm_log"
    "VeraListNodeVeraListIterator_rvm_log" "VeraListNodervm_data"
    "VeraListNodervm_log" "VeraList_VeraListIterator_rvm_log"
    "VeraList_rvm_data" "VeraList_rvm_log"
    "rvm_broadcast" "rvm_channel_class" "rvm_data" "rvm_data" "rvm_env"
    "rvm_log" "rvm_log_modifier" "rvm_log_msg" "rvm_log_msg" "rvm_log_msg_info"
    "rvm_log_watchpoint" "rvm_notify" "rvm_notify_event"
    "rvm_notify_event_config" "rvm_scheduler" "rvm_scheduler_election"
    "rvm_watchdog" "rvm_watchdog_port" "rvm_xactor" "rvm_xactor_callbacks"
   )
  "List of Vera-RVM keywords.")

(defconst vera-rvm-functions
  '(
    "extern_rvm_atomic_gen" "extern_rvm_channel" "extern_rvm_scenario_gen"
    "rvm_OO_callback" "rvm_atomic_gen" "rvm_atomic_gen_callbacks_decl"
    "rvm_atomic_gen_decl" "rvm_atomic_scenario_decl" "rvm_channel"
    "rvm_channel_" "rvm_channel_decl" "rvm_command" "rvm_cycle" "rvm_debug"
    "rvm_error" "rvm_fatal" "rvm_note" "rvm_protocol" "rvm_report"
    "rvm_scenario_decl" "rvm_scenario_election_decl" "rvm_scenario_gen"
    "rvm_scenario_gen_callbacks_decl" "rvm_scenario_gen_decl"
    "rvm_trace" "rvm_transaction" "rvm_user" "rvm_verbose" "rvm_warning"
   )
  "List of Vera-RVM functions.")

(defconst vera-rvm-constants
  '(
   "RVM_NUMERIC_VERSION_MACROS" "RVM_VERSION" "RVM_MINOR" "RVM_PATCH"
   "rvm_channel__SOURCE" "rvm_channel__SINK" "rvm_channel__NO_ACTIVE"
   "rvm_channel__ACT_PENDING" "rvm_channel__ACT_STARTED"
   "rvm_channel__ACT_COMPLETED" "rvm_channel__FULL" "rvm_channel__EMPTY"
   "rvm_channel__PUT" "rvm_channel__GOT" "rvm_channel__PEEKED"
   "rvm_channel__ACTIVATED" "rvm_channel__STARTED" "rvm_channel__COMPLETED"
   "rvm_channel__REMOVED" "rvm_channel__LOCKED" "rvm_channel__UNLOCKED"
   "rvm_data__EXECUTE" "rvm_data__STARTED" "rvm_data__ENDED"
   "rvm_env__CFG_GENED" "rvm_env__BUILT" "rvm_env__DUT_CFGED"
   "rvm_env__STARTED" "rvm_env__RESTARTED" "rvm_env__ENDED" "rvm_env__STOPPED"
   "rvm_env__CLEANED" "rvm_env__DONE" "rvm_log__DEFAULT" "rvm_log__UNCHANGED"
   "rvm_log__FAILURE_TYP" "rvm_log__NOTE_TYP" "rvm_log__DEBUG_TYP"
   "rvm_log__REPORT_TYP" "rvm_log__NOTIFY_TYP" "rvm_log__TIMING_TYP"
   "rvm_log__XHANDLING_TYP" "rvm_log__PROTOCOL_TYP" "rvm_log__TRANSACTION_TYP"
   "rvm_log__COMMAND_TYP" "rvm_log__CYCLE_TYP" "rvm_log__USER_TYP_0"
   "rvm_log__USER_TYP_1" "rvm_log__USER_TYP_2" "rvm_log__USER_TYP_3"
   "rvm_log__DEFAULT_TYP" "rvm_log__ALL_TYPES" "rvm_log__FATAL_SEV"
   "rvm_log__ERROR_SEV" "rvm_log__WARNING_SEV" "rvm_log__NORMAL_SEV"
   "rvm_log__TRACE_SEV" "rvm_log__DEBUG_SEV" "rvm_log__VERBOSE_SEV"
   "rvm_log__HIDDEN_SEV" "rvm_log__IGNORE_SEV" "rvm_log__DEFAULT_SEV"
   "rvm_log__ALL_SEVERITIES" "rvm_log__CONTINUE" "rvm_log__COUNT_AS_ERROR"
   "rvm_log__DEBUGGER" "rvm_log__DUMP" "rvm_log__STOP" "rvm_log__ABORT"
   "rvm_notify__ONE_SHOT_TRIGGER" "rvm_notify__ONE_BLAST_TRIGGER"
   "rvm_notify__HAND_SHAKE_TRIGGER" "rvm_notify__ON_OFF_TRIGGER"
   "rvm_xactor__XACTOR_IDLE" "rvm_xactor__XACTOR_BUSY"
   "rvm_xactor__XACTOR_STARTED" "rvm_xactor__XACTOR_STOPPED"
   "rvm_xactor__XACTOR_RESET" "rvm_xactor__XACTOR_SOFT_RST"
   "rvm_xactor__XACTOR_FIRM_RST" "rvm_xactor__XACTOR_HARD_RST"
   "rvm_xactor__XACTOR_PROTOCOL_RST" "rvm_broadcast__AFAP"
   "rvm_broadcast__ALAP" "rvm_watchdog__TIMEOUT"
   "rvm_env__DUT_RESET" "rvm_log__INTERNAL_TYP"
   "RVM_SCHEDULER_IS_XACTOR" "RVM_BROADCAST_IS_XACTOR"
  )
  "List of Vera-RVM predefined constants.")

;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst vera-keywords-regexp
  (concat "\\<\\(" (regexp-opt vera-keywords) "\\)\\>")
  "Regexp for Vera keywords.")

(defconst vera-types-regexp
  (concat "\\<\\(" (regexp-opt vera-types) "\\)\\>")
  "Regexp for Vera predefined types.")

(defconst vera-q-values-regexp
  (concat "\\<\\(" (regexp-opt vera-q-values) "\\)\\>")
  "Regexp for Vera predefined VCA q_values.")

(defconst vera-functions-regexp
  (concat "\\<\\(" (regexp-opt vera-functions) "\\)\\>")
  "Regexp for Vera predefined system functions, tasks and class methods.")

(defconst vera-constants-regexp
  (concat "\\<\\(" (regexp-opt vera-constants) "\\)\\>")
  "Regexp for Vera predefined constants.")

(defconst vera-rvm-types-regexp
  (concat "\\<\\(" (regexp-opt vera-rvm-types) "\\)\\>")
  "Regexp for Vera-RVM keywords.")

(defconst vera-rvm-functions-regexp
  (concat "\\<\\(" (regexp-opt vera-rvm-functions) "\\)\\>")
  "Regexp for Vera-RVM predefined system functions, tasks and class methods.")

(defconst vera-rvm-constants-regexp
  (concat "\\<\\(" (regexp-opt vera-rvm-constants) "\\)\\>")
  "Regexp for Vera-RVM predefined constants.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XEmacs compatibility
(when (featurep 'xemacs)
  (require 'font-lock)
  (copy-face 'font-lock-reference-face 'font-lock-constant-face)
  (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face))

(defun vera-font-lock-match-item (limit)
  "Match, and move over, any declaration item after point.
Adapted from `font-lock-match-c-style-declaration-item-and-skip-to-next'."
  (condition-case nil
      (save-restriction
	(narrow-to-region (point-min) limit)
	;; match item
	(when (looking-at "\\s-*\\(\\w+\\)")
	  (save-match-data
	    (goto-char (match-end 1))
	    ;; move to next item
	    (if (looking-at "\\(\\s-*\\(\\[[^]]*\\]\\s-*\\)?,\\)")
		(goto-char (match-end 1))
	      (end-of-line) t))))
    (error t)))

(defvar vera-font-lock-keywords
  (list
   ;; highlight keywords
   (list vera-keywords-regexp 1 'font-lock-keyword-face)
   ;; highlight types
   (list vera-types-regexp 1 'font-lock-type-face)
   ;; highlight RVM types
   (list vera-rvm-types-regexp 1 'font-lock-type-face)
   ;; highlight constants
   (list vera-constants-regexp 1 'font-lock-constant-face)
   ;; highlight RVM constants
   (list vera-rvm-constants-regexp 1 'font-lock-constant-face)
   ;; highlight q_values
   (list vera-q-values-regexp 1 'font-lock-constant-face)
   ;; highlight predefined functions, tasks and methods
   (list vera-functions-regexp 1 'vera-font-lock-function)
   ;; highlight predefined RVM functions
   (list vera-rvm-functions-regexp 1 'vera-font-lock-function)
   ;; highlight functions
   '("\\<\\(\\w+\\)\\s-*(" 1 font-lock-function-name-face)
   ;; highlight various declaration names
   '("^\\s-*\\(port\\|program\\|task\\)\\s-+\\(\\w+\\)\\>"
     2 font-lock-function-name-face)
   '("^\\s-*bind\\s-+\\(\\w+\\)\\s-+\\(\\w+\\)\\>"
     (1 font-lock-function-name-face) (2 font-lock-function-name-face))
   ;; highlight interface declaration names
   '("^\\s-*\\(class\\|interface\\)\\s-+\\(\\w+\\)\\>"
     2 vera-font-lock-interface)
   ;; highlight variable name definitions
   (list (concat "^\\s-*" vera-types-regexp "\\s-*\\(\\[[^]]+\\]\\s-+\\)?")
	 '(vera-font-lock-match-item nil nil (1 font-lock-variable-name-face)))
   (list (concat "^\\s-*" vera-rvm-types-regexp "\\s-*\\(\\[[^]]+\\]\\s-+\\)?")
	 '(vera-font-lock-match-item nil nil (1 font-lock-variable-name-face)))
   ;; highlight numbers
   '("\\([0-9]*'[bdoh][0-9a-fA-FxXzZ_]+\\)" 1 vera-font-lock-number)
   ;; highlight filenames in #include directives
   '("^#\\s-*include\\s-*\\(<[^>\"\n]*>?\\)"
     1 font-lock-string-face)
   ;; highlight directives and directive names
   '("^#\\s-*\\(\\w+\\)\\>[ \t!]*\\(\\w+\\)?"
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t))
   ;; highlight `@', `$' and `#'
   '("\\([@$#]\\)" 1 font-lock-keyword-face)
   ;; highlight @ and # definitions
   '("@\\s-*\\(\\w*\\)\\(\\s-*,\\s-*\\(\\w+\\)\\)?\\>[^.]"
     (1 vera-font-lock-number) (3 vera-font-lock-number nil t))
   ;; highlight interface signal name
   '("\\(\\w+\\)\\.\\w+" 1 vera-font-lock-interface)
   )
  "Regular expressions to highlight in Vera Mode.")

(defvar vera-font-lock-number 'vera-font-lock-number
  "Face name to use for @ definitions.")

(defvar vera-font-lock-function 'vera-font-lock-function
  "Face name to use for predefined functions and tasks.")

(defvar vera-font-lock-interface 'vera-font-lock-interface
  "Face name to use for interface names.")

(defface vera-font-lock-number
  '((((class color) (background light)) (:foreground "Gold4"))
    (((class color) (background dark)) (:foreground "BurlyWood1"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight @ definitions."
  :group 'font-lock-highlighting-faces)

(defface vera-font-lock-function
  '((((class color) (background light)) (:foreground "DarkCyan"))
    (((class color) (background dark)) (:foreground "Orchid1"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight predefined functions and tasks."
  :group 'font-lock-highlighting-faces)

(defface vera-font-lock-interface
  '((((class color) (background light)) (:foreground "Grey40"))
    (((class color) (background dark)) (:foreground "Grey80"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight interface names."
  :group 'font-lock-highlighting-faces)

(defalias 'vera-fontify-buffer 'font-lock-fontify-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vera-echo-syntactic-information-p nil
  "If non-nil, syntactic info is echoed when the line is indented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offset functions

(defconst vera-offsets-alist
  '((comment        . vera-lineup-C-comments)
    (comment-intro  . vera-lineup-comment)
    (string         . -1000)
    (directive      . -1000)
    (block-open     . 0)
    (block-intro    . +)
    (block-close    . 0)
    (arglist-intro  . +)
    (arglist-cont   . +)
    (arglist-cont-nonempty . 0)
    (arglist-close  . 0)
    (statement      . 0)
    (statement-cont . +)
    (substatement   . +)
    (else-clause    . 0))
  "Association list of syntactic element symbols and indentation offsets.
Adapted from `c-offsets-alist'.")

(defun vera-evaluate-offset (offset langelem symbol)
  "OFFSET can be a number, a function, a variable, a list, or one of
the symbols + or -."
  (cond
   ((eq offset '+)         (setq offset vera-basic-offset))
   ((eq offset '-)         (setq offset (- vera-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 vera-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- vera-basic-offset))))
   ((eq offset '*)         (setq offset (/ vera-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- vera-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (vera-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		0
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset))))
  offset)

(defun vera-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
vera-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol vera-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(setq offset 0
	      relpos 0)
      (setq offset (vera-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (line-beginning-position)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (vera-evaluate-offset offset langelem symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help functions

(defsubst vera-point (position)
  "Return the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol  -- beginning of line
  eol  -- end of line
  boi  -- back to indentation
  ionl -- indentation of next line
  iopl -- indentation of previous line
  bonl -- beginning of next line
  bopl -- beginning of previous line
This function does not modify point or mark."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl) (forward-line -1) (back-to-indentation))
     ((eq position 'ionl) (forward-line 1) (back-to-indentation))
     (t (error "Unknown buffer position requested: %s" position)))
    (point)))

(defun vera-in-literal (&optional lim)
  "Determine if point is in a Vera literal."
  (save-excursion
    (let ((state (parse-partial-sexp (or lim (point-min)) (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun vera-skip-forward-literal ()
  "Skip forward literal and return t if within one."
  (let ((state (save-excursion
                 (if (fboundp 'syntax-ppss)
                     (syntax-ppss)
                   (parse-partial-sexp (point-min) (point))))))
    (when (nth 8 state)
      ;; Inside a string or comment.
      (goto-char (nth 8 state))
      (if (nth 3 state)
          ;; A string.
          (condition-case nil (forward-sexp 1)
            ;; Can't find end of string: it extends til end of buffer.
            (error (goto-char (point-max))))
        ;; A comment.
        (forward-comment 1))
      t)))

(defun vera-skip-backward-literal ()
  "Skip backward literal and return t if within one."
  (let ((state (save-excursion
                 (if (fboundp 'syntax-ppss)
                     (syntax-ppss)
                   (parse-partial-sexp (point-min) (point))))))
    (when (nth 8 state)
      ;; Inside a string or comment.
      (goto-char (nth 8 state))
      t)))

(defsubst vera-re-search-forward (regexp &optional bound noerror)
  "Like `re-search-forward', but skips over matches in literals."
  (let (ret)
    (while (and (setq ret (re-search-forward regexp bound noerror))
                (vera-skip-forward-literal)
                (if bound (< (point) bound) t)))
    ret))

(defsubst vera-re-search-backward (regexp &optional bound noerror)
  "Like `re-search-backward', but skips over matches in literals."
  (let (ret)
    (while (and (setq ret (re-search-backward regexp bound noerror))
                (vera-skip-backward-literal)
                (if bound (> (point) bound) t)))
    ret))

(defun vera-forward-syntactic-ws (&optional lim skip-directive)
  "Forward skip of syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region (point) lim)
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum)
	(when (and skip-directive (looking-at "^\\s-*#"))
	  (end-of-line))))))

(defun vera-backward-syntactic-ws (&optional lim skip-directive)
  "Backward skip over syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (when (< lim (point))
	(narrow-to-region lim (point))
	(while (/= here (point))
	  (setq here (point))
	  (forward-comment hugenum)
	  (when (and skip-directive
		     (save-excursion (back-to-indentation)
				     (= (following-char) ?\#)))
	    (beginning-of-line)))))))

(defmacro vera-prepare-search (&rest body)
  "Execute BODY with a syntax table that includes '_'."
  `(with-syntax-table vera-mode-ext-syntax-table ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment indentation functions

(defsubst vera-langelem-col (langelem &optional preserve-point)
  "Convenience routine to return the column of LANGELEM's relpos.
Leaves point at the relpos unless PRESERVE-POINT is non-nil."
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here)))))

(defun vera-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Nicked from `c-lineup-C-comments'."
  (save-excursion
    (let ((here (point))
	  (stars (progn (back-to-indentation)
			(skip-chars-forward "*")))
	  (langelem-col (vera-langelem-col langelem)))
      (back-to-indentation)
      (if (not (re-search-forward "/\\([*]+\\)" (vera-point 'eol) t))
	  (progn
	    (if (not (looking-at "[*]+"))
		(progn
		  ;; we now have to figure out where this comment begins.
		  (goto-char here)
		  (back-to-indentation)
		  (if (looking-at "[*]+/")
		      (progn (goto-char (match-end 0))
			     (forward-comment -1))
		    (goto-char (cdr langelem))
		    (back-to-indentation))))
	    (- (current-column) langelem-col))
	(if (zerop stars)
	    (progn
	      (skip-chars-forward " \t")
	      (- (current-column) langelem-col))
	  ;; how many stars on comment opening line?  if greater than
	  ;; on current line, align left.  if less than or equal,
	  ;; align right.  this should also pick up Javadoc style
	  ;; comments.
	  (if (> (length (match-string 1)) stars)
	      (progn
		(back-to-indentation)
		(- (current-column) -1 langelem-col))
	    (- (current-column) stars langelem-col)))))))

(defun vera-lineup-comment (langelem)
  "Line up a comment start."
  (save-excursion
    (back-to-indentation)
    (if (bolp)
	;; not indent if at beginning of line
	-1000
      ;; otherwise indent accordingly
      (goto-char (cdr langelem))
      (current-column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move functions

(defconst vera-beg-block-re "{\\|\\<\\(begin\\|fork\\)\\>")

(defconst vera-end-block-re "}\\|\\<\\(end\\|join\\(\\s-+\\(all\\|any\\|none\\)\\)?\\)\\>")

(defconst vera-beg-substatement-re "\\<\\(else\\|for\\|if\\|repeat\\|while\\)\\>")

(defun vera-corresponding-begin (&optional recursive)
  "Find corresponding block begin if cursor is at a block end."
  (while (and (vera-re-search-backward
	       (concat "\\(" vera-end-block-re "\\)\\|" vera-beg-block-re)
	       nil t)
	      (match-string 1))
    (vera-corresponding-begin t))
  (unless recursive (vera-beginning-of-substatement)))

(defun vera-corresponding-if ()
  "Find corresponding `if' if cursor is at `else'."
  (while (and (vera-re-search-backward "}\\|\\<\\(if\\|else\\)\\>" nil t)
	      (not (equal (match-string 0) "if")))
    (if (equal (match-string 0) "else")
	(vera-corresponding-if)
      (forward-char)
      (backward-sexp))))

(defun vera-beginning-of-statement ()
  "Go to beginning of current statement."
  (let (pos)
    (while
	(progn
	  ;; search for end of previous statement
	  (while
	      (and (vera-re-search-backward
		    (concat "[);]\\|" vera-beg-block-re
			    "\\|" vera-end-block-re) nil t)
		   (equal (match-string 0) ")"))
	    (forward-char)
	    (backward-sexp))
	  (setq pos (match-beginning 0))
	  ;; go back to beginning of current statement
	  (goto-char (or (match-end 0) 0))
	  (vera-forward-syntactic-ws nil t)
	  (when (looking-at "(")
	    (forward-sexp)
	    (vera-forward-syntactic-ws nil t))
	  ;; if "else" found, go to "if" and search again
	  (when (looking-at "\\<else\\>")
	    (vera-corresponding-if)
	    (setq pos (point))
	    t))
      ;; if search is repeated, go to beginning of last search
      (goto-char pos))))

(defun vera-beginning-of-substatement ()
  "Go to beginning of current substatement."
  (let ((lim (point))
	pos)
  ;; go to beginning of statement
    (vera-beginning-of-statement)
    (setq pos (point))
    ;; go forward all substatement opening statements until at LIM
    (while (and (< (point) lim)
		(vera-re-search-forward vera-beg-substatement-re lim t))
      (setq pos (match-beginning 0)))
    (vera-forward-syntactic-ws nil t)
    (when (looking-at "(")
      (forward-sexp)
      (vera-forward-syntactic-ws nil t))
    (when (< (point) lim)
      (setq pos (point)))
    (goto-char pos)))

(defun vera-forward-statement ()
  "Move forward one statement."
  (interactive)
  (vera-prepare-search
   (while (and (vera-re-search-forward
		(concat "[(;]\\|" vera-beg-block-re "\\|" vera-end-block-re)
		nil t)
	       (equal (match-string 0) "("))
     (backward-char)
     (forward-sexp))
   (vera-beginning-of-substatement)))

(defun vera-backward-statement ()
  "Move backward one statement."
  (interactive)
  (vera-prepare-search
   (vera-backward-syntactic-ws nil t)
   (unless (= (preceding-char) ?\))
     (backward-char))
   (vera-beginning-of-substatement)))

(defun vera-forward-same-indent ()
  "Move forward to next line with same indent."
  (interactive)
  (let ((pos (point))
	(indent (current-indentation)))
    (beginning-of-line 2)
    (while (and (not (eobp))
		(or (looking-at "^\\s-*$")
		    (> (current-indentation) indent)))
      (beginning-of-line 2))
    (if (= (current-indentation) indent)
	(back-to-indentation)
      (message "No following line with same indent found in this block")
      (goto-char pos))))

(defun vera-backward-same-indent ()
  "Move backward to previous line with same indent."
  (interactive)
  (let ((pos (point))
	(indent (current-indentation)))
    (beginning-of-line -0)
    (while (and (not (bobp))
		(or (looking-at "^\\s-*$")
		    (> (current-indentation) indent)))
      (beginning-of-line -0))
    (if (= (current-indentation) indent)
	(back-to-indentation)
      (message "No preceding line with same indent found in this block")
      (goto-char pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax analysis

(defmacro vera-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in SYMBOL to the syntax list.
try to increase performance by using this macro."
  `(setq syntax (cons (cons ,symbol ,(or relpos 0)) syntax)))

(defun vera-guess-basic-syntax ()
  "Determine syntactic context of current line of code."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  syntax state placeholder)
      ;; determine syntax state
      (setq state (parse-partial-sexp (point-min) (point)))
      (cond
       ;; CASE 1: in a comment?
       ((nth 4 state)
	;; skip empty lines
	(while (and (zerop (forward-line -1))
		    (looking-at "^\\s-*$")))
	(vera-add-syntax 'comment (vera-point 'boi)))
       ;; CASE 2: in a string?
       ((nth 3 state)
	(vera-add-syntax 'string))
       ;; CASE 3: at a directive?
       ((save-excursion (back-to-indentation) (= (following-char) ?\#))
	(vera-add-syntax 'directive (point)))
       ;; CASE 4: after an opening parenthesis (argument list continuation)?
       ((and (nth 1 state)
	     (or (= (char-after (nth 1 state)) ?\()
		 ;; also for concatenation (opening '{' and ',' on eol/eopl)
		 (and (= (char-after (nth 1 state)) ?\{)
		      (or (save-excursion
			    (vera-backward-syntactic-ws) (= (char-before) ?,))
			  (save-excursion
			    (end-of-line) (= (char-before) ?,))))))
	(goto-char (1+ (nth 1 state)))
	;; is there code after the opening parenthesis on the same line?
	(if (looking-at "\\s-*$")
	    (vera-add-syntax 'arglist-cont (vera-point 'boi))
	  (vera-add-syntax 'arglist-cont-nonempty (point))))
       ;; CASE 5: at a block closing?
       ((save-excursion (back-to-indentation) (looking-at vera-end-block-re))
	;; look for the corresponding begin
	(vera-corresponding-begin)
	(vera-add-syntax 'block-close (vera-point 'boi)))
       ;; CASE 6: at a block intro (the first line after a block opening)?
       ((and (save-excursion
	       (vera-backward-syntactic-ws nil t)
	       ;; previous line ends with a block opening?
	       (or (/= (skip-chars-backward "{") 0) (backward-word 1))
	       (when (looking-at vera-beg-block-re)
		 ;; go to beginning of substatement
		 (vera-beginning-of-substatement)
		 (setq placeholder (point))))
	     ;; not if "fork" is followed by "{"
	     (save-excursion
	       (not (and (progn (back-to-indentation) (looking-at "{"))
			 (progn (goto-char placeholder)
				(looking-at "\\<fork\\>"))))))
	(goto-char placeholder)
	(vera-add-syntax 'block-intro (vera-point 'boi)))
       ;; CASE 7: at the beginning of an else clause?
       ((save-excursion (back-to-indentation) (looking-at "\\<else\\>"))
	;; find corresponding if
	(vera-corresponding-if)
	(vera-add-syntax 'else-clause (vera-point 'boi)))
       ;; CASE 8: at the beginning of a statement?
       ;; is the previous command completed?
       ((or (save-excursion
	      (vera-backward-syntactic-ws nil t)
	      (setq placeholder (point))
	      ;; at the beginning of the buffer?
	      (or (bobp)
		  ;; previous line ends with a semicolon or
		  ;; is a block opening or closing?
		  (when (or (/= (skip-chars-backward "{};") 0)
			    (progn (back-to-indentation)
				   (looking-at (concat vera-beg-block-re "\\|"
						       vera-end-block-re))))
		    ;; if at a block closing, go to beginning
		    (when (looking-at vera-end-block-re)
		      (vera-corresponding-begin))
		    ;; go to beginning of the statement
		    (vera-beginning-of-statement)
		    (setq placeholder (point)))
		  ;; at a directive?
		  (when (progn (back-to-indentation) (looking-at "#"))
		    ;; go to previous statement
		    (vera-beginning-of-statement)
		    (setq placeholder (point)))))
	    ;; at a block opening?
	    (when (save-excursion (back-to-indentation)
				  (looking-at vera-beg-block-re))
	      ;; go to beginning of the substatement
	      (vera-beginning-of-substatement)
	      (setq placeholder (point))))
	(goto-char placeholder)
	(vera-add-syntax 'statement (vera-point 'boi)))
       ;; CASE 9: at the beginning of a substatement?
       ;; is this line preceded by a substatement opening statement?
       ((save-excursion (vera-backward-syntactic-ws nil t)
			(when (= (preceding-char) ?\)) (backward-sexp))
			(backward-word 1)
			(setq placeholder (point))
			(looking-at vera-beg-substatement-re))
	(goto-char placeholder)
	(vera-add-syntax 'substatement (vera-point 'boi)))
       ;; CASE 10: it must be a statement continuation!
       (t
	;; go to beginning of statement
	(vera-beginning-of-substatement)
	(vera-add-syntax 'statement-cont (vera-point 'boi))))
      ;; special case: look for a comment start
      (goto-char indent-point)
      (skip-chars-forward " \t")
      (when (looking-at comment-start)
	(vera-add-syntax 'comment-intro))
      ;; return syntax
      syntax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation functions

(defun vera-indent-line ()
  "Indent the current line as Vera code.
Return the amount of indentation change (in columns)."
  (interactive)
  (vera-prepare-search
   (let* ((syntax (vera-guess-basic-syntax))
	  (pos (- (point-max) (point)))
	  (indent (apply '+ (mapcar 'vera-get-offset syntax)))
	  (shift-amt  (- (current-indentation) indent)))
     (when vera-echo-syntactic-information-p
       (message "syntax: %s, indent= %d" syntax indent))
     (unless (zerop shift-amt)
       (beginning-of-line)
       (delete-region (point) (vera-point 'boi))
       (indent-to indent))
     (if (< (point) (vera-point 'boi))
	 (back-to-indentation)
       ;; If initial point was within line's indentation, position after
       ;; the indentation.  Else stay at same point in text.
       (when (> (- (point-max) pos) (point))
	 (goto-char (- (point-max) pos))))
     shift-amt)))

(defun vera-indent-buffer ()
  "Indent whole buffer as Vera code.
Calls `indent-region' for whole buffer."
  (interactive)
  (message "Indenting buffer...")
  (indent-region (point-min) (point-max) nil)
  (message "Indenting buffer...done"))

(defun vera-indent-region (start end column)
  "Indent region as Vera code."
  (interactive "r\nP")
  (message "Indenting region...")
  (indent-region start end column)
  (message "Indenting region...done"))

(defsubst vera-indent-block-closing ()
  "If previous word is a block closing or `else', indent line again."
  (when (= (char-syntax (preceding-char)) ?w)
    (save-excursion
      (backward-word 1)
      (when (and (not (vera-in-literal))
		 (looking-at (concat vera-end-block-re "\\|\\<else\\>")))
	(indent-according-to-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electrifications

(defun vera-electric-tab (&optional prefix)
  "Do what I mean (indent, expand, tab, change indent, etc..).
If preceding character is part of a word or a paren then `hippie-expand',
else if right of non whitespace on line then `tab-to-tab-stop',
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'.
If `vera-intelligent-tab' is nil, always indent line."
  (interactive "*P")
  (if vera-intelligent-tab
      (progn
	(cond ((and (not (featurep 'xemacs)) (use-region-p))
	       (vera-indent-region (region-beginning) (region-end) nil))
	      ((memq (char-syntax (preceding-char)) '(?w ?_))
	       (let ((case-fold-search t)
		     (case-replace nil)
		     (hippie-expand-only-buffers
		      (or (and (boundp 'hippie-expand-only-buffers)
			       hippie-expand-only-buffers)
			  '(vera-mode))))
		 (vera-expand-abbrev prefix)))
	      ((> (current-column) (current-indentation))
	       (tab-to-tab-stop))
	      ((and (or (eq last-command 'vera-electric-tab)
			(eq last-command 'vera-electric-return))
		    (/= 0 (current-indentation)))
	       (backward-delete-char-untabify vera-basic-offset nil))
	      (t (indent-according-to-mode)))
	(setq this-command 'vera-electric-tab))
    (indent-according-to-mode)))

(defun vera-electric-return ()
  "Insert newline and indent.  Indent current line if it is a block closing."
  (interactive)
  (vera-indent-block-closing)
  (newline-and-indent))

(defun vera-electric-space (arg)
  "Insert a space.  Indent current line if it is a block closing."
  (interactive "*P")
  (unless arg
    (vera-indent-block-closing))
  (self-insert-command (prefix-numeric-value arg)))

(defun vera-electric-opening-brace (arg)
  "Outdent opening brace."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (unless arg
    (indent-according-to-mode)))

(defun vera-electric-closing-brace (arg)
  "Outdent closing brace."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (unless arg
    (indent-according-to-mode)))

(defun vera-electric-pound (arg)
  "Insert `#' and indent as directive it first character of line."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (unless arg
    (save-excursion
      (backward-char)
      (skip-chars-backward " \t")
      (when (bolp)
	(delete-horizontal-space)))))

(defun vera-electric-star (arg)
  "Insert a star character.  Nicked from `c-electric-star'."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (if (and (not arg)
	   (memq (vera-in-literal) '(comment))
	   (eq (char-before) ?*)
	   (save-excursion
	     (forward-char -1)
	     (skip-chars-backward "*")
	     (if (eq (char-before) ?/)
		 (forward-char -1))
	     (skip-chars-backward " \t")
	     (bolp)))
      (indent-according-to-mode)))

(defun vera-electric-slash (arg)
  "Insert a slash character.  Nicked from `c-electric-slash'."
  (interactive "*P")
  (let* ((ch (char-before))
	 (indentp (and (not arg)
		       (eq last-command-event ?/)
		       (or (and (eq ch ?/)
				(not (vera-in-literal)))
			   (and (eq ch ?*)
				(vera-in-literal))))))
    (self-insert-command (prefix-numeric-value arg))
    (when indentp
      (indent-according-to-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization (for expansion of Vera commands)

(defvar vera-abbrev-list
  (append (list nil) vera-keywords
	  (list nil) vera-types
	  (list nil) vera-functions
	  (list nil) vera-constants
	  (list nil) vera-rvm-types
	  (list nil) vera-rvm-functions
	  (list nil) vera-rvm-constants)
  "Predefined abbreviations for Vera.")

(defvar vera-expand-upper-case nil)

(eval-when-compile (require 'hippie-exp))

(defun vera-try-expand-abbrev (old)
  "Try expanding abbreviations from `vera-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list vera-abbrev-list)
		(sel-abbrev-list '()))
	    (while abbrev-list
	      (when (or (not (stringp (car abbrev-list)))
			(string-match
			 (concat "^" he-search-string) (car abbrev-list)))
		(setq sel-abbrev-list
		      (cons (car abbrev-list) sel-abbrev-list)))
	      (setq abbrev-list (cdr abbrev-list)))
	    (nreverse sel-abbrev-list))))
  (while (and he-expand-list
	      (or (not (stringp (car he-expand-list)))
		  (he-string-member (car he-expand-list) he-tried-table t)))
    (unless (stringp (car he-expand-list))
      (setq vera-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if vera-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; function for expanding abbrevs and dabbrevs
(defalias 'vera-expand-abbrev
  (make-hippie-expand-function '(try-expand-dabbrev
				 try-expand-dabbrev-all-buffers
				 vera-try-expand-abbrev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun vera-comment-uncomment-region (beg end &optional _arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at comment-start-skip)
      (comment-region beg end '(4))
    (comment-region beg end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vera-customize ()
  "Call the customize function with `vera' as argument."
  (interactive)
  (customize-group 'vera))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other

;; remove ".vr" from `completion-ignored-extensions'
(setq completion-ignored-extensions
      (delete ".vr" completion-ignored-extensions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vera-mode-help-address "Reto Zimmermann <reto@gnu.org>"
  "Address for Vera Mode bug reports.")

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun vera-submit-bug-report ()
  "Submit via mail a bug report on Vera Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on Vera Mode? ")
   (require 'reporter)
   (let ((reporter-prompt-for-summary-p t))
     (reporter-submit-bug-report
      vera-mode-help-address
      (concat "Vera Mode " vera-version)
      (list
       ;; report all important variables
       'vera-basic-offset
       'vera-underscore-is-part-of-word
       'vera-intelligent-tab
       )
      nil nil
      "Hi Reto,"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vera-version ()
  "Echo the current version of Vera Mode in the minibuffer."
  (interactive)
  (message "Vera Mode %s (%s)" vera-version vera-time-stamp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vera-mode)

;;; vera-mode.el ends here
