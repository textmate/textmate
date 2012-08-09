;;; f90.el --- Fortran-90 mode (free format)  -*- lexical-binding: t -*-

;; Copyright (C) 1995-1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Torbjörn Einarsson <Torbjorn.Einarsson@era.ericsson.se>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: fortran, f90, languages

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

;; Major mode for editing F90 programs in FREE FORMAT.
;; The minor language revision F95 is also supported (with font-locking).
;; Some/many (?) aspects of F2003 are supported.
;; Some aspects of F2008 are supported.

;; Knows about continuation lines, named structured statements, and other
;; features in F90 including HPF (High Performance Fortran) structures.
;; The basic feature provides accurate indentation of F90 programs.
;; In addition, there are many more features like automatic matching of all
;; end statements, an auto-fill function to break long lines, a join-lines
;; function which joins continued lines, etc.

;; To facilitate typing, a fairly complete list of abbreviations is provided.
;; All abbreviations begin with the backquote character "`"
;; For example, `i expands to integer (if abbrev-mode is on).

;; There are two separate features for altering the appearance of code:
;;   1) Upcasing or capitalizing of all keywords.
;;   2) Colors/fonts using font-lock-mode.
;; Automatic upcase or downcase of keywords is controlled by the variable
;; f90-auto-keyword-case.

;; The indentations of lines starting with ! is determined by the first of the
;; following matches (values in the left column are the defaults):

;; start-string/regexp  indent         variable holding start-string/regexp
;;    !!!                  0
;;    !hpf\\$ (re)         0              f90-directive-comment-re
;;    !!$                  0              f90-comment-region
;;    !      (re)        as code          f90-indented-comment-re
;;    default            comment-column

;; Ex: Here is the result of 3 different settings of f90-indented-comment-re
;;     f90-indented-comment-re  !-indentation      !!-indentation
;;          !                    as code             as code
;;          !!                   comment-column      as code
;;          ![^!]                as code             comment-column
;; Trailing comments are indented to comment-column with indent-for-comment.
;; The function f90-comment-region toggles insertion of
;; the variable f90-comment-region in every line of the region.

;; One common convention for free vs. fixed format is that free format files
;; have the ending .f90 or .f95 while fixed format files have the ending .f.
;; Emacs automatically loads Fortran files in the appropriate mode based
;; on extension. You can modify this by adjusting the variable auto-mode-alist.
;; For example:
;; (add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))

;; Once you have entered f90-mode, you may get more info by using
;; the command describe-mode (C-h m). For online help use
;; C-h f <Name of function you want described>, or
;; C-h v <Name of variable you want described>.

;; To customize f90-mode for your taste, use, for example:
;; (you don't have to specify values for all the parameters below)
;;
;; (add-hook 'f90-mode-hook
;;       ;; These are the default values.
;;       (lambda () (setq f90-do-indent 3
;;                        f90-if-indent 3
;;                        f90-type-indent 3
;;                        f90-program-indent 2
;;                        f90-continuation-indent 5
;;                        f90-comment-region "!!$"
;;                        f90-directive-comment-re "!hpf\\$"
;;                        f90-indented-comment-re "!"
;;                        f90-break-delimiters "[-+\\*/><=,% \t]"
;;                        f90-break-before-delimiters t
;;                        f90-beginning-ampersand t
;;                        f90-smart-end 'blink
;;                        f90-auto-keyword-case nil
;;                        f90-leave-line-no nil
;;                        indent-tabs-mode nil
;;                        f90-font-lock-keywords f90-font-lock-keywords-2
;;                  )
;;       ;; These are not default.
;;       (abbrev-mode 1)             ; turn on abbreviation mode
;;       (f90-add-imenu-menu)        ; extra menu with functions etc.
;;       (if f90-auto-keyword-case   ; change case of all keywords on startup
;;           (f90-change-keywords f90-auto-keyword-case))
;;       ))
;;
;; in your .emacs file. You can also customize the lists
;; f90-font-lock-keywords, etc.
;;
;; The auto-fill and abbreviation minor modes are accessible from the F90 menu,
;; or by using M-x auto-fill-mode and M-x abbrev-mode, respectively.

;; Remarks
;; 1) Line numbers are by default left-justified. If f90-leave-line-no is
;;    non-nil, the line numbers are never touched.
;; 2) Multi-; statements like "do i=1,20 ; j=j+i ; end do" are not handled
;;    correctly, but I imagine them to be rare.
;; 3) Regexps for hilit19 are no longer supported.
;; 4) For FIXED FORMAT code, use fortran mode.
;; 5) This mode does not work under emacs-18.x.
;; 6) Preprocessor directives, i.e., lines starting with # are left-justified
;;    and are untouched by all case-changing commands. There is, at present, no
;;    mechanism for treating multi-line directives (continued by \ ).
;; 7) f77 do-loops do 10 i=.. ; ; 10 continue are not correctly indented.
;;    You are urged to use f90-do loops (with labels if you wish).
;; 8) The highlighting mode under XEmacs is not as complete as under Emacs.

;; List of user commands
;;   f90-previous-statement         f90-next-statement
;;   f90-beginning-of-subprogram    f90-end-of-subprogram   f90-mark-subprogram
;;   f90-comment-region
;;   f90-indent-line                f90-indent-new-line
;;   f90-indent-region    (can be called by calling indent-region)
;;   f90-indent-subprogram
;;   f90-break-line                 f90-join-lines
;;   f90-fill-region
;;   f90-insert-end
;;   f90-upcase-keywords            f90-upcase-region-keywords
;;   f90-downcase-keywords          f90-downcase-region-keywords
;;   f90-capitalize-keywords        f90-capitalize-region-keywords
;;   f90-add-imenu-menu
;;   f90-font-lock-1, f90-font-lock-2, f90-font-lock-3, f90-font-lock-4

;; Original author's thanks
;; Thanks to all the people who have tested the mode. Special thanks to Jens
;; Bloch Helmers for encouraging me to write this code, for creative
;; suggestions as well as for the lists of hpf-commands.
;; Also thanks to the authors of the fortran and pascal modes, on which some
;; of this code is built.

;;; Code:

;; TODO
;; 1. Any missing F2003 syntax?
;; 2. Have "f90-mode" just recognize F90 syntax, then derived modes
;; "f95-mode", "f2003-mode" for the language revisions.
;; 3. Support for align.
;; Font-locking:
;; 1. OpenMP, OpenMPI?, preprocessor highlighting.
;; 2. integer_name = 1
;; 3. Labels for "else" statements (F2003)?

(defvar comment-auto-fill-only-comments)
(defvar font-lock-keywords)

;; User options

(defgroup f90 nil
  "Major mode for editing free format Fortran 90,95 code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defgroup f90-indent nil
  "Indentation in free format Fortran."
  :prefix "f90-"
  :group  'f90)


(defcustom f90-do-indent 3
  "Extra indentation applied to DO blocks."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent)

(defcustom f90-if-indent 3
  "Extra indentation applied to IF, SELECT CASE, WHERE and FORALL blocks."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent)

(defcustom f90-type-indent 3
  "Extra indentation applied to TYPE, ENUM, INTERFACE and BLOCK DATA blocks."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent)

(defcustom f90-program-indent 2
  "Extra indentation applied to PROGRAM, MODULE, SUBROUTINE, FUNCTION blocks."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent)

(defcustom f90-associate-indent 2
  "Extra indentation applied to ASSOCIATE blocks."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent
  :version "23.1")

(defcustom f90-critical-indent 2
  "Extra indentation applied to BLOCK, CRITICAL blocks."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent
  :version "24.1")

(defcustom f90-continuation-indent 5
  "Extra indentation applied to continuation lines."
  :type  'integer
  :safe  'integerp
  :group 'f90-indent)

(defcustom f90-comment-region "!!$"
  "String inserted by \\[f90-comment-region] at start of each line in region."
  :type  'string
  :safe  'stringp
  :group 'f90-indent)

(defcustom f90-indented-comment-re "!"
  "Regexp matching comments to indent as code."
  :type  'regexp
  :safe  'stringp
  :group 'f90-indent)

(defcustom f90-directive-comment-re "!hpf\\$"
  "Regexp of comment-like directive like \"!HPF\\\\$\", not to be indented."
  :type  'regexp
  :safe  'stringp
  :group 'f90-indent)

(defcustom f90-beginning-ampersand t
  "Non-nil gives automatic insertion of \& at start of continuation line."
  :type  'boolean
  :safe  'booleanp
  :group 'f90)

(defcustom f90-smart-end 'blink
  "Qualification of END statements according to the matching block start.
For example, the END that closes an IF block is changed to END
IF.  If the block has a label, this is added as well.  Allowed
values are 'blink, 'no-blink, and nil.  If nil, nothing is done.
The other two settings have the same effect, but 'blink
additionally blinks the cursor to the start of the block."
  :type  '(choice (const blink) (const no-blink) (const nil))
  :safe  (lambda (value) (memq value '(blink no-blink nil)))
  :group 'f90)

(defcustom f90-break-delimiters "[-+\\*/><=,% \t]"
  "Regexp matching delimiter characters at which lines may be broken.
There are some common two-character tokens where one or more of
the members matches this regexp.  Although Fortran allows breaks
within lexical tokens (provided the next line has a beginning ampersand),
the constant `f90-no-break-re' ensures that such tokens are not split."
  :type 'regexp
  :safe 'stringp
  :group 'f90)

(defcustom f90-break-before-delimiters t
  "Non-nil causes `f90-do-auto-fill' to break lines before delimiters."
  :type 'boolean
  :safe 'booleanp
  :group 'f90)

(defcustom f90-auto-keyword-case nil
  "Automatic case conversion of keywords.
The options are 'downcase-word, 'upcase-word, 'capitalize-word and nil."
  :type  '(choice (const downcase-word) (const upcase-word)
                  (const capitalize-word) (const nil))
  :safe (lambda (value) (memq value '(downcase-word
                                      capitalize-word upcase-word nil)))
  :group 'f90)

(defcustom f90-leave-line-no nil
  "If non-nil, line numbers are not left justified."
  :type  'boolean
  :safe  'booleanp
  :group 'f90)

(defcustom f90-mode-hook nil
  "Hook run when entering F90 mode."
  :type    'hook
  ;; Not the only safe options, but some common ones.
  :safe    (lambda (value) (member value '((f90-add-imenu-menu) nil)))
  :options '(f90-add-imenu-menu)
  :group   'f90)

;; User options end here.

(defconst f90-keywords-re
  (regexp-opt '("allocatable" "allocate" "assign" "assignment" "backspace"
                "block" "call" "case" "character" "close" "common" "complex"
                "contains" "continue" "cycle" "data" "deallocate"
                "dimension" "do" "double" "else" "elseif" "elsewhere" "end"
                "enddo" "endfile" "endif" "entry" "equivalence" "exit"
                "external" "forall" "format" "function" "goto" "if"
                "implicit" "include" "inquire" "integer" "intent"
                "interface" "intrinsic" "logical" "module" "namelist" "none"
                "nullify" "only" "open" "operator" "optional" "parameter"
                "pause" "pointer" "precision" "print" "private" "procedure"
                "program" "public" "read" "real" "recursive" "result" "return"
                "rewind" "save" "select" "sequence" "stop" "subroutine"
                "target" "then" "type" "use" "where" "while" "write"
                ;; F95 keywords.
                "elemental" "pure"
                ;; F2003
                "abstract" "associate" "asynchronous" "bind" "class"
                "deferred" "enum" "enumerator" "extends" "extends_type_of"
                "final" "generic" "import" "non_intrinsic" "non_overridable"
                "nopass" "pass" "protected" "same_type_as" "value" "volatile"
                ;; F2008.
                "contiguous" "submodule" "concurrent" "codimension"
                "sync all" "sync memory" "critical" "image_index"
                ) 'words)
  "Regexp used by the function `f90-change-keywords'.")

(defconst f90-keywords-level-3-re
  (regexp-opt
   '("allocatable" "allocate" "assign" "assignment" "backspace"
     "close" "deallocate" "dimension" "endfile" "entry" "equivalence"
     "external" "inquire" "intent" "intrinsic" "nullify" "only" "open"
     ;; FIXME operator and assignment should be F2003 procedures?
     "operator" "optional" "parameter" "pause" "pointer" "print" "private"
     "public" "read" "recursive" "result" "rewind" "save" "select"
     "sequence" "target" "write"
     ;; F95 keywords.
     "elemental" "pure"
     ;; F2003. asynchronous separate.
     "abstract" "deferred" "import" "final" "non_intrinsic" "non_overridable"
     "nopass" "pass" "protected" "value" "volatile"
     ;; F2008.
     ;; "concurrent" is only in the sense of "do [,] concurrent", but given
     ;; the [,] it's simpler to just do every instance (cf "do while").
     "contiguous" "concurrent" "codimension" "sync all" "sync memory"
     ) 'words)
  "Keyword-regexp for font-lock level >= 3.")

(defconst f90-procedures-re
  (concat "\\<"
          (regexp-opt
           '("abs" "achar" "acos" "adjustl" "adjustr" "aimag" "aint"
             "all" "allocated" "anint" "any" "asin" "associated"
             "atan" "atan2" "bit_size" "btest" "ceiling" "char" "cmplx"
             "conjg" "cos" "cosh" "count" "cshift" "date_and_time" "dble"
             "digits" "dim" "dot_product" "dprod" "eoshift" "epsilon"
             "exp" "exponent" "floor" "fraction" "huge" "iachar" "iand"
             "ibclr" "ibits" "ibset" "ichar" "ieor" "index" "int" "ior"
             "ishft" "ishftc" "kind" "lbound" "len" "len_trim" "lge" "lgt"
             "lle" "llt" "log" "log10" "logical" "matmul" "max"
             "maxexponent" "maxloc" "maxval" "merge" "min" "minexponent"
             "minloc" "minval" "mod" "modulo" "mvbits" "nearest" "nint"
             "not" "pack" "precision" "present" "product" "radix"
             ;; Real is taken out here to avoid highlighting declarations.
             "random_number" "random_seed" "range" ;; "real"
             "repeat" "reshape" "rrspacing" "scale" "scan"
             "selected_int_kind" "selected_real_kind" "set_exponent"
             "shape" "sign" "sin" "sinh" "size" "spacing" "spread" "sqrt"
             "sum" "system_clock" "tan" "tanh" "tiny" "transfer"
             "transpose" "trim" "ubound" "unpack" "verify"
             ;; F95 intrinsic functions.
             "null" "cpu_time"
             ;; F2003.
             "move_alloc" "command_argument_count" "get_command"
             "get_command_argument" "get_environment_variable"
             "selected_char_kind" "wait" "flush" "new_line"
             "extends" "extends_type_of" "same_type_as" "bind"
             ;; F2003 ieee_arithmetic intrinsic module.
             "ieee_support_underflow_control" "ieee_get_underflow_mode"
             "ieee_set_underflow_mode"
             ;; F2003 iso_c_binding intrinsic module.
             "c_loc" "c_funloc" "c_associated" "c_f_pointer"
             "c_f_procpointer"
             ;; F2008.
             "bge" "bgt" "ble" "blt" "dshiftl" "dshiftr" "leadz" "popcnt"
             "poppar" "trailz" "maskl" "maskr" "shifta" "shiftl" "shiftr"
             "merge_bits" "iall" "iany" "iparity" "storage_size"
             "bessel_j0" "bessel_j1" "bessel_jn"
             "bessel_y0" "bessel_y1" "bessel_yn"
             "erf" "erfc" "erfc_scaled" "gamma" "hypot" "log_gamma"
             "norm2" "parity" "findloc" "is_contiguous"
             "sync images" "lock" "unlock" "image_index"
             "lcobound" "ucobound" "num_images" "this_image"
             ;; F2008 iso_fortran_env module.
             "compiler_options" "compiler_version"
             ;; F2008 iso_c_binding module.
             "c_sizeof"
             ) t)
          ;; A left parenthesis to avoid highlighting non-procedures.
          "[ \t]*(")
  "Regexp whose first part matches F90 intrinsic procedures.")

(defconst f90-operators-re
  (concat "\\."
          (regexp-opt '("and" "eq" "eqv" "false" "ge" "gt" "le" "lt" "ne"
                        "neqv" "not" "or" "true") t)
          "\\.")
  "Regexp matching intrinsic operators.")

(defconst f90-hpf-keywords-re
  (regexp-opt
   ;; Intrinsic procedures.
   '("all_prefix" "all_scatter" "all_suffix" "any_prefix"
     "any_scatter" "any_suffix" "copy_prefix" "copy_scatter"
     "copy_suffix" "count_prefix" "count_scatter" "count_suffix"
     "grade_down" "grade_up"
     "hpf_alignment" "hpf_distribution" "hpf_template" "iall" "iall_prefix"
     "iall_scatter" "iall_suffix" "iany" "iany_prefix" "iany_scatter"
     "iany_suffix" "ilen" "iparity" "iparity_prefix"
     "iparity_scatter" "iparity_suffix" "leadz" "maxval_prefix"
     "maxval_scatter" "maxval_suffix" "minval_prefix" "minval_scatter"
     "minval_suffix" "number_of_processors" "parity"
     "parity_prefix" "parity_scatter" "parity_suffix" "popcnt" "poppar"
     "processors_shape" "product_prefix" "product_scatter"
     "product_suffix" "sum_prefix" "sum_scatter" "sum_suffix"
     ;; Directives.
     "align" "distribute" "dynamic" "independent" "inherit" "processors"
     "realign" "redistribute" "template"
     ;; Keywords.
     "block" "cyclic" "extrinsic" "new" "onto" "pure" "with") 'words)
  "Regexp for all HPF keywords, procedures and directives.")

(defconst f90-constants-re
  (regexp-opt '( ;; F2003 iso_fortran_env constants.
                "iso_fortran_env"
                "input_unit" "output_unit" "error_unit"
                "iostat_end" "iostat_eor"
                "numeric_storage_size" "character_storage_size"
                "file_storage_size"
                ;; F2003 iso_c_binding constants.
                "iso_c_binding"
                "c_int" "c_short" "c_long" "c_long_long" "c_signed_char"
                "c_size_t"
                "c_int8_t" "c_int16_t" "c_int32_t" "c_int64_t"
                "c_int_least8_t" "c_int_least16_t" "c_int_least32_t"
                "c_int_least64_t"
                "c_int_fast8_t" "c_int_fast16_t" "c_int_fast32_t"
                "c_int_fast64_t"
                "c_intmax_t" "c_intptr_t"
                "c_float" "c_double" "c_long_double"
                "c_float_complex" "c_double_complex" "c_long_double_complex"
                "c_bool" "c_char"
                "c_null_char" "c_alert" "c_backspace" "c_form_feed"
                "c_new_line" "c_carriage_return" "c_horizontal_tab"
                "c_vertical_tab"
                "c_ptr" "c_funptr" "c_null_ptr" "c_null_funptr"
                "ieee_exceptions"
                "ieee_arithmetic"
                "ieee_features"
                ;; F2008 iso_fortran_env constants.
                "character_kinds" "int8" "int16" "int32" "int64"
                "integer_kinds" "iostat_inquire_internal_unit"
                "logical_kinds" "real_kinds" "real32" "real64" "real128"
                "lock_type" "atomic_int_kind" "atomic_logical_kind"
                ) 'words)
  "Regexp for Fortran intrinsic constants.")

;; cf f90-looking-at-type-like.
(defun f90-typedef-matcher (limit)
  "Search for the start/end of the definition of a derived type, up to LIMIT.
Set the match data so that subexpression 1,2 are the TYPE, and
type-name parts, respectively."
  (let (found l)
    (while (and (re-search-forward "\\<\\(\\(?:end[ \t]*\\)?type\\)\\>[ \t]*"
                                   limit t)
                (not (setq found
                           (progn
                             (setq l (match-data))
                             (unless (looking-at "\\(is\\>\\|(\\)")
                               (when (if (looking-at "\\(\\sw+\\)")
                                         (goto-char (match-end 0))
                                       (re-search-forward
                                        "[ \t]*::[ \t]*\\(\\sw+\\)"
                                        (line-end-position) t))
                                 ;; 0 is wrong, but we don't use it.
                                 (set-match-data
                                  (append l (list (match-beginning 1)
                                                  (match-end 1))))
                                 t)))))))
    found))

(defvar f90-font-lock-keywords-1
  (list
   ;; Special highlighting of "module procedure".
   '("\\<\\(module[ \t]*procedure\\)\\>\\([^()\n]*::\\)?[ \t]*\\([^&!\n]*\\)"
     (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
   ;; Highlight definition of derived type.
;;;    '("\\<\\(\\(?:end[ \t]*\\)?type\\)\\>\\([^()\n]*::\\)?[ \t]*\\(\\sw+\\)"
;;;      (1 font-lock-keyword-face) (3 font-lock-function-name-face))
   '(f90-typedef-matcher
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   ;; F2003.  Prevent operators being highlighted as functions.
   '("\\<\\(\\(?:end[ \t]*\\)?interface[ \t]*\\(?:assignment\\|operator\\|\
read\\|write\\)\\)[ \t]*(" (1 font-lock-keyword-face t))
   ;; Other functions and declarations.  Named interfaces = F2003.
   ;; F2008: end submodule submodule_name.
   '("\\<\\(\\(?:end[ \t]*\\)?\\(program\\|\\(?:sub\\)?module\\|\
function\\|associate\\|subroutine\\|interface\\)\\|use\\|call\\)\
\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
   ;; F2008: submodule (parent_name) submodule_name.
   '("\\<\\(submodule\\)\\>[ \t]*([^)\n]+)[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
   ;; F2003.
   '("\\<\\(use\\)[ \t]*,[ \t]*\\(\\(?:non_\\)?intrinsic\\)[ \t]*::[ \t]*\
\\(\\sw+\\)"
     (1 font-lock-keyword-face) (2 font-lock-keyword-face)
     (3 font-lock-function-name-face))
   "\\<\\(\\(end[ \t]*\\)?block[ \t]*data\\|contains\\)\\>"
   ;; "abstract interface" is F2003.
   '("\\<abstract[ \t]*interface\\>" (0 font-lock-keyword-face t)))
  "This does fairly subdued highlighting of comments and function calls.")

;; NB not explicitly handling this, yet it seems to work.
;; type(...) function foo()
(defun f90-typedec-matcher (limit)
  "Search for the declaration of variables of derived type, up to LIMIT.
Set the match data so that subexpression 1,2 are the TYPE(...),
and variable-name parts, respectively."
  ;; Matcher functions must return nil only when there are no more
  ;; matches within the search range.
  (let (found l)
    (while (and (re-search-forward "\\<\\(type\\|class\\)[ \t]*(" limit t)
                (not
                 (setq found
                       (condition-case nil
                           (progn
                             ;; Set l after this to just highlight
                             ;; the "type" part.
                             (backward-char 1)
                             ;; Needed for: type( foo(...) ) :: bar
                             (forward-sexp)
                             (setq l (list (match-beginning 0) (point)))
                             (skip-chars-forward " \t")
                             (when
                                 (re-search-forward
                                  ;; type (foo) bar, qux
                                  (if (looking-at "\\sw+")
                                      "\\([^&!\n]+\\)"
                                    ;; type (foo), stuff :: bar, qux
                                    "::[ \t]*\\([^&!\n]+\\)")
                                  (line-end-position) t)
                               (set-match-data
                                (append (list (car l) (match-end 1))
                                        l (list (match-beginning 1)
                                                (match-end 1))))
                               t))
                         (error nil))))))
    found))

(defvar f90-font-lock-keywords-2
  (append
   f90-font-lock-keywords-1
   (list
    ;; Variable declarations (avoid the real function call).
    ;; NB by accident (?), this correctly fontifies the "integer" in:
    ;; integer () function foo ()
    ;; because "() function foo ()" matches \\3.
    ;; The "pure" part does not really belong here, but was added to
    ;; exploit that hack.
    ;; The "function foo" bit is correctly fontified by keywords-1.
    ;; TODO ? actually check for balanced parens in that case.
    '("^[ \t0-9]*\\(?:pure\\|elemental\\)?[ \t]*\
\\(real\\|integer\\|c\\(haracter\\|omplex\\)\\|\
enumerator\\|generic\\|procedure\\|logical\\|double[ \t]*precision\\)\
\\(.*::\\|[ \t]*(.*)\\)?\\([^&!\n]*\\)"
      (1 font-lock-type-face t) (4 font-lock-variable-name-face t))
    ;; Derived type/class variables.
    ;; TODO ? If we just highlighted the "type" part, rather than
    ;; "type(...)", this could be in the previous expression. And this
    ;; would be consistent with integer( kind=8 ), etc.
    '(f90-typedec-matcher
      (1 font-lock-type-face) (2 font-lock-variable-name-face))
    ;; "real function foo (args)". Must override previous.  Note hack
    ;; to get "args" unhighlighted again. Might not always be right,
    ;; but probably better than leaving them as variables.
    ;; NB not explicitly handling this case:
    ;; integer( kind=1 ) function foo()
    ;; thanks to the happy accident described above.
    ;; Not anchored, so don't need to worry about "pure" etc.
    '("\\<\\(\\(real\\|integer\\|c\\(haracter\\|omplex\\)\\|\
logical\\|double[ \t]*precision\\|\
\\(?:type\\|class\\)[ \t]*([ \t]*\\sw+[ \t]*)\\)[ \t]*\\)\
\\(function\\)\\>[ \t]*\\(\\sw+\\)[ \t]*\\(([^&!\n]*)\\)"
      (1 font-lock-type-face t) (4 font-lock-keyword-face t)
      (5 font-lock-function-name-face t) (6 'default t))
    ;; enum (F2003; must be followed by ", bind(C)").
    '("\\<\\(enum\\)[ \t]*," (1 font-lock-keyword-face))
    ;; end do, enum (F2003), if, select, where, and forall constructs.
    ;; block, critical (F2008).
    ;; Note that "block data" may get somewhat mixed up with F2008 blocks,
    ;; but since the former is obsolete I'm not going to worry about it.
    '("\\<\\(end[ \t]*\\(do\\|if\\|enum\\|select\\|forall\\|where\\|\
block\\|critical\\)\\)\\>\
\\([ \t]+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face) (3 font-lock-constant-face nil t))
    '("^[ \t0-9]*\\(\\(\\sw+\\)[ \t]*:[ \t]*\\)?\\(\\(if\\|\
do\\([ \t]*while\\)?\\|select[ \t]*\\(?:case\\|type\\)\\|where\\|\
forall\\|block\\|critical\\)\\)\\>"
      (2 font-lock-constant-face nil t) (3 font-lock-keyword-face))
    ;; Implicit declaration.
    '("\\<\\(implicit\\)[ \t]*\\(real\\|integer\\|c\\(haracter\\|omplex\\)\
\\|enumerator\\|procedure\\|\
logical\\|double[ \t]*precision\\|type[ \t]*(\\sw+)\\|none\\)[ \t]*"
      (1 font-lock-keyword-face) (2 font-lock-type-face))
    '("\\<\\(namelist\\|common\\)[ \t]*\/\\(\\sw+\\)?\/"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    "\\<else\\([ \t]*if\\|where\\)?\\>"
    '("\\(&\\)[ \t]*\\(!\\|$\\)"  (1 font-lock-keyword-face))
    "\\<\\(then\\|continue\\|format\\|include\\|stop\\|return\\)\\>"
    '("\\<\\(exit\\|cycle\\)[ \t]*\\(\\sw+\\)?\\>"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    '("\\<\\(case\\)[ \t]*\\(default\\|(\\)" . 1)
    ;; F2003 "class default".
    '("\\<\\(class\\)[ \t]*default" . 1)
    ;; F2003 "type is" in a "select type" block.
    '("\\<\\(\\(type\\|class\\)[ \t]*is\\)[ \t]*(" (1 font-lock-keyword-face t))
    '("\\<\\(do\\|go[ \t]*to\\)\\>[ \t]*\\([0-9]+\\)"
      (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ;; Line numbers (lines whose first character after number is letter).
    '("^[ \t]*\\([0-9]+\\)[ \t]*[a-z]+" (1 font-lock-constant-face t))))
  "Highlights declarations, do-loops and other constructs.")

(defvar f90-font-lock-keywords-3
  (append f90-font-lock-keywords-2
          (list
           f90-keywords-level-3-re
           f90-operators-re
           ;; FIXME why isn't this font-lock-builtin-face, which
           ;; otherwise we hardly use, as in fortran.el?
           (list f90-procedures-re '(1 font-lock-keyword-face keep))
           "\\<real\\>"                 ; avoid overwriting real defs
           ;; As an attribute, but not as an optional argument.
           '("\\<\\(asynchronous\\)[ \t]*[^=]" . 1)))
  "Highlights all F90 keywords and intrinsic procedures.")

(defvar f90-font-lock-keywords-4
  (append f90-font-lock-keywords-3
          (list (cons f90-constants-re 'font-lock-constant-face)
                f90-hpf-keywords-re))
  "Highlights all F90 and HPF keywords and constants.")

(defvar f90-font-lock-keywords
  f90-font-lock-keywords-2
  "*Default expressions to highlight in F90 mode.
Can be overridden by the value of `font-lock-maximum-decoration'.")


(defvar f90-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\! "<"  table) ; begin comment
    (modify-syntax-entry ?\n ">"  table) ; end comment
    ;; FIXME: This goes against the convention: it should be "_".
    (modify-syntax-entry ?_  "w"  table) ; underscore in names
    (modify-syntax-entry ?\' "\"" table) ; string quote
    (modify-syntax-entry ?\" "\"" table) ; string quote
    ;; FIXME: We used to set ` to word syntax for the benefit of abbrevs, but
    ;; we do not need it any more.  Not sure if it should be "_" or "." now.
    (modify-syntax-entry ?\` "_"  table)
    (modify-syntax-entry ?\r " "  table) ; return is whitespace
    (modify-syntax-entry ?+  "."  table) ; punctuation
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?%  "."  table) ; bug#8820
    ;; I think that the f95 standard leaves the behavior of \
    ;; unspecified, but that f2k will require it to be non-special.
    ;; Use `f90-backslash-not-special' to change.
    (modify-syntax-entry ?\\ "\\" table) ; escape chars
    table)
  "Syntax table used in F90 mode.")

(defvar f90-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "`"        'f90-abbrev-start)
    (define-key map "\C-c;"    'f90-comment-region)
    (define-key map "\C-\M-a"  'f90-beginning-of-subprogram)
    (define-key map "\C-\M-e"  'f90-end-of-subprogram)
    (define-key map "\C-\M-h"  'f90-mark-subprogram)
    (define-key map "\C-\M-n"  'f90-end-of-block)
    (define-key map "\C-\M-p"  'f90-beginning-of-block)
    (define-key map "\C-\M-q"  'f90-indent-subprogram)
    (define-key map "\C-j"     'f90-indent-new-line) ; LFD equals C-j
;;;    (define-key map "\r"       'newline)
    (define-key map "\C-c\r"   'f90-break-line)
;;;  (define-key map [M-return] 'f90-break-line)
    (define-key map "\C-c\C-a" 'f90-previous-block)
    (define-key map "\C-c\C-e" 'f90-next-block)
    (define-key map "\C-c\C-d" 'f90-join-lines)
    (define-key map "\C-c\C-f" 'f90-fill-region)
    (define-key map "\C-c\C-p" 'f90-previous-statement)
    (define-key map "\C-c\C-n" 'f90-next-statement)
    (define-key map "\C-c]"    'f90-insert-end)
    (define-key map "\C-c\C-w" 'f90-insert-end)
    ;; Standard tab binding will call this, and also handle regions.
;;;    (define-key map "\t"       'f90-indent-line)
    (define-key map ","        'f90-electric-insert)
    (define-key map "+"        'f90-electric-insert)
    (define-key map "-"        'f90-electric-insert)
    (define-key map "*"        'f90-electric-insert)
    (define-key map "/"        'f90-electric-insert)

    (easy-menu-define f90-menu map "Menu for F90 mode."
      `("F90"
        ("Customization"
         ,(custom-menu-create 'f90)
         ;; FIXME useless?
         ["Set"  Custom-set :active t
          :help "Set current value of all edited settings in the buffer"]
         ["Save" Custom-save :active t
          :help "Set and save all edited settings"]
         ["Reset to Current" Custom-reset-current :active t
          :help "Reset all edited settings to current"]
         ["Reset to Saved" Custom-reset-saved :active t
          :help "Reset all edited or set settings to saved"]
         ["Reset to Standard Settings" Custom-reset-standard :active t
          :help "Erase all customizations in buffer"]
         )
        "--"
        ["Indent Subprogram" f90-indent-subprogram t]
        ["Mark Subprogram" f90-mark-subprogram :active t :help
         "Mark the end of the current subprogram, move point to the start"]
        ["Beginning of Subprogram" f90-beginning-of-subprogram :active t
         :help "Move point to the start of the current subprogram"]
        ["End of Subprogram" f90-end-of-subprogram :active t
         :help "Move point to the end of the current subprogram"]
        "--"
        ["(Un)Comment Region" f90-comment-region :active mark-active
         :help "Comment or uncomment the region"]
        ["Indent Region" f90-indent-region :active mark-active]
        ["Fill Region" f90-fill-region :active mark-active
         :help "Fill long lines in the region"]
        "--"
        ["Break Line at Point" f90-break-line :active t
         :help "Break the current line at point"]
        ["Join with Previous Line" f90-join-lines :active t
         :help "Join the current line to the previous one"]
        ["Insert Block End" f90-insert-end :active t
         :help "Insert an end statement for the current code block"]
        "--"
        ("Highlighting"
         :help "Fontify this buffer to varying degrees"
         ["Toggle font-lock-mode" font-lock-mode :selected font-lock-mode
          :style toggle :help "Fontify text in this buffer"]
         "--"
         ["Light highlighting (level 1)"    f90-font-lock-1 t]
         ["Moderate highlighting (level 2)" f90-font-lock-2 t]
         ["Heavy highlighting (level 3)"    f90-font-lock-3 t]
         ["Maximum highlighting (level 4)"  f90-font-lock-4 t]
         )
        ("Change Keyword Case"
         :help "Change the case of keywords in the buffer or region"
         ["Upcase Keywords (buffer)"     f90-upcase-keywords     t]
         ["Capitalize Keywords (buffer)" f90-capitalize-keywords t]
         ["Downcase Keywords (buffer)"   f90-downcase-keywords   t]
         "--"
         ["Upcase Keywords (region)"     f90-upcase-region-keywords
          mark-active]
         ["Capitalize Keywords (region)" f90-capitalize-region-keywords
          mark-active]
         ["Downcase Keywords (region)"   f90-downcase-region-keywords
          mark-active]
         )
        "--"
        ["Toggle Auto Fill" auto-fill-mode :selected auto-fill-function
         :style toggle
         :help "Automatically fill text while typing in this buffer"]
        ["Toggle Abbrev Mode" abbrev-mode :selected abbrev-mode
         :style toggle :help "Expand abbreviations while typing in this buffer"]
        ["Add Imenu Menu" f90-add-imenu-menu
         :active   (not (lookup-key (current-local-map) [menu-bar index]))
         :included (fboundp 'imenu-add-to-menubar)
         :help "Add an index menu to the menu-bar"
         ]))
    map)
  "Keymap used in F90 mode.")


(defun f90-font-lock-n (n)
  "Set `font-lock-keywords' to F90 level N keywords."
  (font-lock-mode 1)
  (setq font-lock-keywords
        (symbol-value (intern-soft (format "f90-font-lock-keywords-%d" n))))
  (font-lock-fontify-buffer))

(defun f90-font-lock-1 ()
  "Set `font-lock-keywords' to `f90-font-lock-keywords-1'."
  (interactive)
  (f90-font-lock-n 1))

(defun f90-font-lock-2 ()
  "Set `font-lock-keywords' to `f90-font-lock-keywords-2'."
  (interactive)
  (f90-font-lock-n 2))

(defun f90-font-lock-3 ()
  "Set `font-lock-keywords' to `f90-font-lock-keywords-3'."
  (interactive)
  (f90-font-lock-n 3))

(defun f90-font-lock-4 ()
  "Set `font-lock-keywords' to `f90-font-lock-keywords-4'."
  (interactive)
  (f90-font-lock-n 4))

;; Regexps for finding program structures.
(defconst f90-blocks-re
  (concat "\\(block[ \t]*data\\|"
          (regexp-opt '("do" "if" "interface" "function" "module" "program"
                        "select" "subroutine" "type" "where" "forall"
                        ;; F2003.
                        "enum" "associate"
                        ;; F2008.
                        "submodule" "block" "critical"))
          "\\)\\>")
  "Regexp potentially indicating a \"block\" of F90 code.")

(defconst f90-program-block-re
  (regexp-opt '("program" "module" "subroutine" "function" "submodule") 'paren)
  "Regexp used to locate the start/end of a \"subprogram\".")

;; "class is" is F2003.
(defconst f90-else-like-re
  "\\(else\\([ \t]*if\\|where\\)?\\|case[ \t]*\\(default\\|(\\)\\|\
\\(class\\|type\\)[ \t]*is[ \t]*(\\|class[ \t]*default\\)"
  "Regexp matching an ELSE IF, ELSEWHERE, CASE, CLASS/TYPE IS statement.")

(defconst f90-end-if-re
  (concat "end[ \t]*"
          (regexp-opt '("if" "select" "where" "forall") 'paren)
          "\\>")
  "Regexp matching the end of an IF, SELECT, WHERE, FORALL block.")

(defconst f90-end-type-re
  "end[ \t]*\\(type\\|enum\\|interface\\|block[ \t]*data\\)\\>"
  "Regexp matching the end of a TYPE, ENUM, INTERFACE, BLOCK DATA section.")

(defconst f90-end-associate-re
  "end[ \t]*associate\\>"
  "Regexp matching the end of an ASSOCIATE block.")

;; This is for a TYPE block, not a variable of derived TYPE.
;; Hence no need to add CLASS for F2003.
(defconst f90-type-def-re
  ;; type word
  ;; type :: word
  ;; type, stuff :: word
  ;; type, bind(c) :: word
  ;; NOT "type ("
  "\\<\\(type\\)\\>\\(?:\\(?:[^()\n]*\\|\
.*,[ \t]*bind[ \t]*([ \t]*c[ \t]*)[ \t]*\\)::\\)?[ \t]*\\(\\sw+\\)"
  "Regexp matching the definition of a derived type.")

(defconst f90-typeis-re
  "\\<\\(class\\|type\\)[ \t]*is[ \t]*("
  "Regexp matching a CLASS/TYPE IS statement.")

(defconst f90-no-break-re
  (regexp-opt '("**" "//" "=>" ">=" "<=" "==" "/=" "(/" "/)") 'paren)
  "Regexp specifying two-character tokens not to split when breaking lines.
Each token has one or more of the characters from `f90-break-delimiters'.
Note that if only one of the characters is from that variable,
then the presence of the token here allows a line-break before or
after the other character, where a break would not normally be
allowed.  This minor issue currently only affects \"(/\" and \"/)\".")

(defvar f90-cache-position nil
  "Temporary position used to speed up region operations.")
(make-variable-buffer-local 'f90-cache-position)


;; Hideshow support.
(defconst f90-end-block-re
  (concat "^[ \t0-9]*\\<end[ \t]*"
          (regexp-opt '("do" "if" "forall" "function" "interface"
                        "module" "program" "select" "subroutine"
                        "type" "where" "enum" "associate" "submodule"
                        "block" "critical") t)
          "\\>")
  "Regexp matching the end of an F90 \"block\", from the line start.
Used in the F90 entry in `hs-special-modes-alist'.")

;; Ignore the fact that FUNCTION, SUBROUTINE, WHERE, FORALL have a
;; following "(".  DO, CASE, IF can have labels.
(defconst f90-start-block-re
  (concat
   "^[ \t0-9]*"                         ; statement number
   "\\(\\("
   "\\(\\sw+[ \t]*:[ \t]*\\)?"          ; structure label
   "\\(do\\|select[ \t]*\\(case\\|type\\)\\|"
   ;; See comments in fortran-start-block-re for the problems of IF.
   "if[ \t]*(\\(.*\\|"
   ".*\n\\([^if]*\\([^i].\\|.[^f]\\|.\\>\\)\\)\\)\\<then\\|"
   ;; Distinguish WHERE block from isolated WHERE.
   "\\(where\\|forall\\)[ \t]*(.*)[ \t]*\\(!\\|$\\)\\)\\)"
   "\\|"
   ;; Avoid F2003 "type is" in "select type",
   ;; and also variables of derived type "type (foo)".
   ;; "type, foo" must be a block (?).
   "type[ \t,]\\("
   "[^i(!\n\"\& \t]\\|"                 ; not-i(
   "i[^s!\n\"\& \t]\\|"                 ; i not-s
   "is\\sw\\)\\|"
   ;; "abstract interface" is F2003; "submodule" is F2008.
   "program\\|\\(?:abstract[ \t]*\\)?interface\\|\\(?:sub\\)?module\\|"
   ;; "enum", but not "enumerator".
   "function\\|subroutine\\|enum[^e]\\|associate\\|block\\|critical"
   "\\)"
   "[ \t]*")
  "Regexp matching the start of an F90 \"block\", from the line start.
A simple regexp cannot do this in fully correct fashion, so this
tries to strike a compromise between complexity and flexibility.
Used in the F90 entry in `hs-special-modes-alist'.")

;; hs-special-modes-alist is autoloaded.
(add-to-list 'hs-special-modes-alist
             `(f90-mode ,f90-start-block-re ,f90-end-block-re
                        "!" f90-end-of-block nil))


;; Imenu support.
;; FIXME trivial to extend this to enum. Worth it?
(defun f90-imenu-type-matcher ()
  "Search backward for the start of a derived type.
Set subexpression 1 in the match-data to the name of the type."
  (let (found)
    (while (and (re-search-backward "^[ \t0-9]*type[ \t]*" nil t)
                (not (setq found
                           (save-excursion
                             (goto-char (match-end 0))
                             (unless (looking-at "\\(is\\>\\|(\\)")
                               (or (looking-at "\\(\\sw+\\)")
                                   (re-search-forward
                                    "[ \t]*::[ \t]*\\(\\sw+\\)"
                                    (line-end-position) t))))))))
    found))

(defvar f90-imenu-generic-expression
  (let ((good-char "[^!\"\&\n \t]") (not-e "[^e!\n\"\& \t]")
        (not-n "[^n!\n\"\& \t]") (not-d "[^d!\n\"\& \t]")
        ;; (not-ib "[^i(!\n\"\& \t]") (not-s "[^s!\n\"\& \t]")
        )
    (list
     '(nil "^[ \t0-9]*program[ \t]+\\(\\sw+\\)" 1)
     '("Submodules" "^[ \t0-9]*submodule[ \t]*([^)\n]+)[ \t]*\
\\(\\sw+\\)[ \t]*\\(!\\|$\\)" 1)
     '("Modules" "^[ \t0-9]*module[ \t]+\\(\\sw+\\)[ \t]*\\(!\\|$\\)" 1)
     (list "Types" 'f90-imenu-type-matcher 1)
     ;; Does not handle: "type[, stuff] :: foo".
;;;      (format "^[ \t0-9]*type[ \t]+\\(\\(%s\\|i%s\\|is\\sw\\)\\sw*\\)"
;;;              not-ib not-s)
;;;      1)
     ;; Can't get the subexpression numbers to match in the two branches.
;;;      (format "^[ \t0-9]*type\\([ \t]*,.*\\(::\\)[ \t]*\\(\\sw+\\)\\|[ \t]+\\(\\(%s\\|i%s\\|is\\sw\\)\\sw*\\)\\)" not-ib not-s)
;;;      3)
     (list
      "Procedures"
      (concat
       "^[ \t0-9]*"
       "\\("
       ;; At least three non-space characters before function/subroutine.
       ;; Check that the last three non-space characters do not spell E N D.
       "[^!\"\&\n]*\\("
       not-e good-char good-char "\\|"
       good-char not-n good-char "\\|"
       good-char good-char not-d "\\)"
       "\\|"
       ;; Less than three non-space characters before function/subroutine.
       good-char "?" good-char "?"
       "\\)"
       "[ \t]*\\(function\\|subroutine\\)[ \t]+\\(\\sw+\\)")
      4)))
  "Value for `imenu-generic-expression' in F90 mode.")

(defun f90-add-imenu-menu ()
  "Add an imenu menu to the menubar."
  (interactive)
  (if (lookup-key (current-local-map) [menu-bar index])
      (message "%s" "F90-imenu already exists.")
    (imenu-add-to-menubar "F90-imenu")
    (redraw-frame (selected-frame))))


;; Abbrevs have generally two letters, except standard types `c, `i, `r, `t.
(define-abbrev-table 'f90-mode-abbrev-table
  (mapcar (lambda (e) (list (car e) (cdr e) nil :system t))
          '(("`al"  . "allocate"     )
            ("`ab"  . "allocatable"  )
            ("`ai"  . "abstract interface")
            ("`as"  . "assignment"   )
            ("`asy" . "asynchronous" )
            ("`ba"  . "backspace"    )
            ("`bd"  . "block data"   )
            ("`bl"  . "block"        )
            ("`c"   . "character"    )
            ("`cl"  . "close"        )
            ("`cm"  . "common"       )
            ("`cx"  . "complex"      )
            ("`cn"  . "contains"     )
            ("`cr"  . "critical"     )
            ("`cy"  . "cycle"        )
            ("`de"  . "deallocate"   )
            ("`df"  . "define"       )
            ("`di"  . "dimension"    )
            ("`dp"  . "double precision")
            ("`dw"  . "do while"     )
            ("`el"  . "else"         )
            ("`eli" . "else if"      )
            ("`elw" . "elsewhere"    )
            ("`em"  . "elemental"    )
            ("`e"   . "enumerator"   )
            ("`eq"  . "equivalence"  )
            ("`ex"  . "external"     )
            ("`ey"  . "entry"        )
            ("`fl"  . "forall"       )
            ("`fo"  . "format"       )
            ("`fu"  . "function"     )
            ("`fa"  . ".false."      )
            ("`im"  . "implicit none")
            ("`in"  . "include"      )
            ("`i"   . "integer"      )
            ("`it"  . "intent"       )
            ("`if"  . "interface"    )
            ("`lo"  . "logical"      )
            ("`mo"  . "module"       )
            ("`na"  . "namelist"     )
            ("`nu"  . "nullify"      )
            ("`op"  . "optional"     )
            ("`pa"  . "parameter"    )
            ("`po"  . "pointer"      )
            ("`pr"  . "print"        )
            ("`pi"  . "private"      )
            ("`pm"  . "program"      )
            ("`pr"  . "protected"    )
            ("`pu"  . "public"       )
            ("`r"   . "real"         )
            ("`rc"  . "recursive"    )
            ("`rt"  . "return"       )
            ("`rw"  . "rewind"       )
            ("`se"  . "select"       )
            ("`sq"  . "sequence"     )
            ("`su"  . "subroutine"   )
            ("`ta"  . "target"       )
            ("`tr"  . ".true."       )
            ("`t"   . "type"         )
            ("`vo"  . "volatile"     )
            ("`wh"  . "where"        )
            ("`wr"  . "write"        )))
  "Abbrev table for F90 mode."
  ;; Accept ` as the first char of an abbrev.  Also allow _ in abbrevs.
  :regexp "\\(?:[^[:word:]_`]\\|^\\)\\(`?[[:word:]_]+\\)[^[:word:]_]*")

;;;###autoload
(define-derived-mode f90-mode prog-mode "F90"
  "Major mode for editing Fortran 90,95 code in free format.
For fixed format code, use `fortran-mode'.

\\[f90-indent-line] indents the current line.
\\[f90-indent-new-line] indents current line and creates a new\
 indented line.
\\[f90-indent-subprogram] indents the current subprogram.

Type `? or `\\[help-command] to display a list of built-in\
 abbrevs for F90 keywords.

Key definitions:
\\{f90-mode-map}

Variables controlling indentation style and extra features:

`f90-do-indent'
  Extra indentation within do blocks (default 3).
`f90-if-indent'
  Extra indentation within if/select/where/forall blocks (default 3).
`f90-type-indent'
  Extra indentation within type/enum/interface/block-data blocks (default 3).
`f90-program-indent'
  Extra indentation within program/module/subroutine/function blocks
  (default 2).
`f90-associate-indent'
  Extra indentation within associate blocks (default 2).
`f90-critical-indent'
  Extra indentation within critical/block blocks (default 2).
`f90-continuation-indent'
  Extra indentation applied to continuation lines (default 5).
`f90-comment-region'
  String inserted by function \\[f90-comment-region] at start of each
  line in region (default \"!!!$\").
`f90-indented-comment-re'
  Regexp determining the type of comment to be intended like code
  (default \"!\").
`f90-directive-comment-re'
  Regexp of comment-like directive like \"!HPF\\\\$\", not to be indented
  (default \"!hpf\\\\$\").
`f90-break-delimiters'
  Regexp holding list of delimiters at which lines may be broken
  (default \"[-+*/><=,% \\t]\").
`f90-break-before-delimiters'
  Non-nil causes `f90-do-auto-fill' to break lines before delimiters
  (default t).
`f90-beginning-ampersand'
  Automatic insertion of \& at beginning of continuation lines (default t).
`f90-smart-end'
  From an END statement, check and fill the end using matching block start.
  Allowed values are 'blink, 'no-blink, and nil, which determine
  whether to blink the matching beginning (default 'blink).
`f90-auto-keyword-case'
  Automatic change of case of keywords (default nil).
  The possibilities are 'downcase-word, 'upcase-word, 'capitalize-word.
`f90-leave-line-no'
  Do not left-justify line numbers (default nil).

Turning on F90 mode calls the value of the variable `f90-mode-hook'
with no args, if that value is non-nil."
  :group 'f90
  :abbrev-table f90-mode-abbrev-table
  (set (make-local-variable 'indent-line-function) 'f90-indent-line)
  (set (make-local-variable 'indent-region-function) 'f90-indent-region)
  (set (make-local-variable 'comment-start) "!")
  (set (make-local-variable 'comment-start-skip) "!+ *")
  (set (make-local-variable 'comment-indent-function) 'f90-comment-indent)
  (set (make-local-variable 'abbrev-all-caps) t)
  (set (make-local-variable 'normal-auto-fill-function) 'f90-do-auto-fill)
  (setq indent-tabs-mode nil)           ; auto buffer local
  (set (make-local-variable 'font-lock-defaults)
       '((f90-font-lock-keywords f90-font-lock-keywords-1
                                 f90-font-lock-keywords-2
                                 f90-font-lock-keywords-3
                                 f90-font-lock-keywords-4)
         nil t))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       f90-imenu-generic-expression)
  (set (make-local-variable 'beginning-of-defun-function)
       'f90-beginning-of-subprogram)
  (set (make-local-variable 'end-of-defun-function) 'f90-end-of-subprogram)
  (set (make-local-variable 'add-log-current-defun-function)
       #'f90-current-defun))


;; Inline-functions.
(defsubst f90-in-string ()
  "Return non-nil if point is inside a string.
Checks from `point-min', or `f90-cache-position', if that is non-nil
and lies before point."
  (let ((beg-pnt
         (if (and f90-cache-position (> (point) f90-cache-position))
             f90-cache-position
           (point-min))))
    (nth 3 (parse-partial-sexp beg-pnt (point)))))

(defsubst f90-in-comment ()
  "Return non-nil if point is inside a comment.
Checks from `point-min', or `f90-cache-position', if that is non-nil
and lies before point."
  (let ((beg-pnt
         (if (and f90-cache-position (> (point) f90-cache-position))
             f90-cache-position
           (point-min))))
    (nth 4 (parse-partial-sexp beg-pnt (point)))))

(defsubst f90-line-continued ()
  "Return t if the current line is a continued one.
This includes comment lines embedded in continued lines, but
not the last line of a continued statement."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at "[ \t]*\\(!\\|$\\)") (zerop (forward-line -1))))
    (end-of-line)
    (while (f90-in-comment)
      (search-backward "!" (line-beginning-position))
      (skip-chars-backward "!"))
    (skip-chars-backward " \t")
    (= (preceding-char) ?&)))

;; GM this is not right, eg a continuation line starting with a number.
;; Need f90-code-start-position function.
;; And yet, things seems to work with this...
;; cf f90-indent-line
;;     (beginning-of-line)           ; digits after & \n are not line-nos
;;     (if (not (save-excursion (and (f90-previous-statement)
;;                                   (f90-line-continued))))
;;         (f90-indent-line-no)
(defsubst f90-current-indentation ()
  "Return indentation of current line.
Line-numbers are considered whitespace characters."
  (save-excursion (beginning-of-line) (skip-chars-forward " \t0-9")))

(defsubst f90-indent-to (col &optional no-line-number)
  "Indent current line to column COL.
If optional argument NO-LINE-NUMBER is nil, jump over a possible
line-number before indenting."
  (beginning-of-line)
  (or no-line-number
      (skip-chars-forward " \t0-9"))
  (delete-horizontal-space)
  ;; Leave >= 1 space after line number.
  (indent-to col (if (zerop (current-column)) 0 1)))

(defsubst f90-get-present-comment-type ()
  "If point lies within a comment, return the string starting the comment.
For example, \"!\" or \"!!\", followed by the appropriate amount of
whitespace, if any."
  ;; Include the whitespace for consistent auto-filling of comment blocks.
  (save-excursion
    (when (f90-in-comment)
      (beginning-of-line)
      (re-search-forward "!+[ \t]*" (line-end-position))
      (while (f90-in-string)
        (re-search-forward "!+[ \t]*" (line-end-position)))
      (match-string-no-properties 0))))

(defsubst f90-equal-symbols (a b)
  "Compare strings A and B neglecting case and allowing for nil value."
  (equal (if a (downcase a) nil)
         (if b (downcase b) nil)))

(defsubst f90-looking-at-do ()
  "Return (\"do\" NAME) if a do statement starts after point.
NAME is nil if the statement has no label."
  (if (looking-at "\\(\\(\\sw+\\)[ \t]*:\\)?[ \t]*\\(do\\)\\>")
      (list (match-string 3) (match-string 2))))

(defsubst f90-looking-at-select-case ()
  "Return (\"select\" NAME) if a select statement starts after point.
NAME is nil if the statement has no label."
  (if (looking-at "\\(\\(\\sw+\\)[ \t]*:\\)?[ \t]*\
\\(select\\)[ \t]*\\(case\\|type\\)[ \t]*(")
      (list (match-string 3) (match-string 2))))

(defsubst f90-looking-at-if-then ()
  "Return (\"if\" NAME) if an if () then statement starts after point.
NAME is nil if the statement has no label."
  (save-excursion
    (when (looking-at "\\(\\(\\sw+\\)[ \t]*:\\)?[ \t]*\\(if\\)\\>")
      (let ((struct (match-string 3))
            (label (match-string 2))
            (pos (scan-lists (point) 1 0)))
        (and pos (goto-char pos))
        (skip-chars-forward " \t")
        (if (or (looking-at "then\\>")
                (when (f90-line-continued)
                  (f90-next-statement)
                  (skip-chars-forward " \t0-9&")
                  (looking-at "then\\>")))
            (list struct label))))))

;; FIXME label?
(defsubst f90-looking-at-associate ()
  "Return (\"associate\") if an associate block starts after point."
  (if (looking-at "\\<\\(associate\\)[ \t]*(")
      (list (match-string 1))))

(defsubst f90-looking-at-critical ()
  "Return (KIND NAME) if a critical or block block starts after point."
  (if (looking-at "\\(\\(\\sw+\\)[ \t]*:\\)?[ \t]*\\(critical\\|block\\)\\>")
      (let ((struct (match-string 3))
            (label (match-string 2)))
        (if (or (not (string-equal "block" struct))
                (save-excursion
                  (skip-chars-forward " \t")
                  (not (looking-at "data\\>"))))
            (list struct label)))))

(defsubst f90-looking-at-end-critical ()
  "Return non-nil if a critical or block block ends after point."
  (if (looking-at "end[ \t]*\\(critical\\|block\\)\\>")
      (or (not (string-equal "block" (match-string 1)))
          (save-excursion
            (skip-chars-forward " \t")
            (not (looking-at "data\\>"))))))

(defsubst f90-looking-at-where-or-forall ()
  "Return (KIND NAME) if a where or forall block starts after point.
NAME is nil if the statement has no label."
  (save-excursion
    (when (looking-at "\\(\\(\\sw+\\)[ \t]*:\\)?[ \t]*\
\\(where\\|forall\\)\\>")
      (let ((struct (match-string 3))
            (label (match-string 2))
            (pos (scan-lists (point) 1 0)))
        (and pos (goto-char pos))
        (skip-chars-forward " \t")
        (if (looking-at "\\(!\\|$\\)") (list struct label))))))

(defsubst f90-looking-at-type-like ()
  "Return (KIND NAME) if a type/enum/interface/block-data starts after point.
NAME is non-nil only for type and certain interfaces."
  (cond
   ((save-excursion
      (and (looking-at "\\<type\\>[ \t]*")
           (goto-char (match-end 0))
           (not (looking-at "\\(is\\>\\|(\\)"))
           (or (looking-at "\\(\\sw+\\)")
               (re-search-forward "[ \t]*::[ \t]*\\(\\sw+\\)"
                                  (line-end-position) t))))
    (list "type" (match-string 1)))
;;;    ((and (not (looking-at f90-typeis-re))
;;;          (looking-at f90-type-def-re))
;;;     (list (match-string 1) (match-string 2)))
   ((looking-at "\\<\\(interface\\)\\>[ \t]*")
    (list (match-string 1)
          (save-excursion
            (goto-char (match-end 0))
            (if (or (looking-at "\\(operator\\|assignment\\|read\\|\
write\\)[ \t]*([^)\n]*)")
                    (looking-at "\\sw+"))
                (match-string 0)))))
   ((looking-at "\\(enum\\|block[ \t]*data\\)\\>")
    (list (match-string 1) nil))
   ((looking-at "abstract[ \t]*\\(interface\\)\\>")
    (list (match-string 1) nil))))

(defsubst f90-looking-at-program-block-start ()
  "Return (KIND NAME) if a program block with name NAME starts after point."
;;;NAME is nil for an un-named main PROGRAM block."
  (cond
   ((looking-at "\\(program\\)[ \t]+\\(\\sw+\\)\\>")
    (list (match-string 1) (match-string 2)))
   ((and (not (looking-at "module[ \t]*procedure\\>"))
         (looking-at "\\(module\\)[ \t]+\\(\\sw+\\)\\>"))
    (list (match-string 1) (match-string 2)))
   ((looking-at "\\(submodule\\)[ \t]*([^)\n]+)[ \t]*\\(\\sw+\\)\\>")
    (list (match-string 1) (match-string 2)))
   ((and (not (looking-at "end[ \t]*\\(function\\|subroutine\\)"))
         (looking-at "[^!'\"\&\n]*\\(function\\|subroutine\\)[ \t]+\
\\(\\sw+\\)"))
    (list (match-string 1) (match-string 2)))))
;; Following will match an un-named main program block; however
;; one needs to check if there is an actual PROGRAM statement after
;; point (and before any END program). Adding this will require
;; change to eg f90-calculate-indent.
;;;   ((save-excursion
;;;     (not (f90-previous-statement)))
;;;    '("program" nil))))

(defsubst f90-looking-at-program-block-end ()
  "Return (KIND NAME) if a block with name NAME ends after point."
  (cond ((looking-at "end[ \t]*\\(interface\\)[ \t]*\\(\
\\(?:assignment\\|operator\\|read\\|write\\)[ \t]*([^)\n]*)\\)")
         (list (match-string 1) (match-string 2)))
        ((looking-at (concat "end[ \t]*" f90-blocks-re
                             "?\\([ \t]+\\(\\sw+\\)\\)?\\>"))
        (list (match-string 1) (match-string 3)))))

(defsubst f90-comment-indent ()
  "Return the indentation to be used for a comment starting at point.
Used for `comment-indent-function' by F90 mode.
\"!!!\", `f90-directive-comment-re', variable `f90-comment-region' return 0.
`f90-indented-comment-re' (if not trailing code) calls `f90-calculate-indent'.
All others return `comment-column', leaving at least one space after code."
  (cond ((looking-at "!!!") 0)
        ((and f90-directive-comment-re
              (looking-at f90-directive-comment-re)) 0)
        ((looking-at (regexp-quote f90-comment-region)) 0)
        ((and (looking-at f90-indented-comment-re)
              ;; Don't attempt to indent trailing comment as code.
              (save-excursion
                (skip-chars-backward " \t")
                (bolp)))
         (f90-calculate-indent))
        (t (save-excursion
             (skip-chars-backward " \t")
             (max (if (bolp) 0 (1+ (current-column))) comment-column)))))

(defsubst f90-present-statement-cont ()
  "Return continuation properties of present statement.
Possible return values are:
single - statement is not continued.
begin  - current line is the first in a continued statement.
end    - current line is the last in a continued statement
middle - current line is neither first nor last in a continued statement.
Comment lines embedded amongst continued lines return 'middle."
  (let (pcont cont)
    (save-excursion
      (setq pcont (if (f90-previous-statement) (f90-line-continued))))
    (setq cont (f90-line-continued))
    (cond ((and (not pcont) (not cont)) 'single)
          ((and (not pcont) cont)       'begin)
          ((and pcont       (not cont)) 'end)
          ((and pcont       cont)       'middle)
          (t (error "The impossible occurred")))))

(defsubst f90-indent-line-no ()
  "If `f90-leave-line-no' is nil, left-justify a line number.
Leaves point at the first non-blank character after the line number.
Call from beginning of line."
  (and (null f90-leave-line-no) (looking-at "[ \t]+[0-9]")
       (delete-horizontal-space))
  (skip-chars-forward " \t0-9"))

(defsubst f90-no-block-limit ()
  "Return nil if point is at the edge of a code block.
Searches line forward for \"function\" or \"subroutine\",
if all else fails."
  (save-excursion
    (not (or (looking-at "end")
             (looking-at "\\(do\\|if\\|else\\(if\\|where\\)?\
\\|select[ \t]*\\(case\\|type\\)\\|case\\|where\\|forall\\|\
block\\|critical\\)\\>")
             (looking-at "\\(program\\|\\(?:sub\\)?module\\|\
\\(?:abstract[ \t]*\\)?interface\\|block[ \t]*data\\)\\>")
             (looking-at "\\(contains\\|\\sw+[ \t]*:\\)")
             (looking-at f90-type-def-re)
             (re-search-forward "\\(function\\|subroutine\\)"
                                (line-end-position) t)))))

(defsubst f90-update-line ()
  "Change case of current line as per `f90-auto-keyword-case'."
  (if f90-auto-keyword-case
      (f90-change-keywords f90-auto-keyword-case
                           (line-beginning-position) (line-end-position))))

(defun f90-electric-insert (&optional arg)
  "Change keyword case and auto-fill line as operators are inserted."
  (interactive "*p")
  (self-insert-command arg)
  (if auto-fill-function (f90-do-auto-fill) ; also updates line
    (f90-update-line)))

;; Behave like self-insert-command for delete-selection-mode (bug#5593).
(put 'f90-electric-insert 'delete-selection t)

(defun f90-get-correct-indent ()
  "Get correct indent for a line starting with line number.
Does not check type and subprogram indentation."
  (let ((epnt (line-end-position)) icol)
    (save-excursion
      (while (and (f90-previous-statement)
                  (or (memq (f90-present-statement-cont) '(middle end))
                      (looking-at "[ \t]*[0-9]"))))
      (setq icol (current-indentation))
      (beginning-of-line)
      (when (re-search-forward "\\(if\\|do\\|select\\|where\\|forall\\)"
                               (line-end-position) t)
        (beginning-of-line)
        (skip-chars-forward " \t")
        (cond ((f90-looking-at-do)
               (setq icol (+ icol f90-do-indent)))
              ((or (f90-looking-at-if-then)
                   (f90-looking-at-where-or-forall)
                   (f90-looking-at-select-case))
               (setq icol (+ icol f90-if-indent)))
              ;; FIXME this makes no sense, because this section/function is
              ;; only for if/do/select/where/forall ?
              ((f90-looking-at-associate)
               (setq icol (+ icol f90-associate-indent))))
        (end-of-line))
      (while (re-search-forward
              "\\(if\\|do\\|select\\|where\\|forall\\)" epnt t)
        (beginning-of-line)
        (skip-chars-forward " \t0-9")
        (cond ((f90-looking-at-do)
               (setq icol (+ icol f90-do-indent)))
              ((or (f90-looking-at-if-then)
                   (f90-looking-at-where-or-forall)
                   (f90-looking-at-select-case))
               (setq icol (+ icol f90-if-indent)))
              ;; FIXME this makes no sense, because this section/function is
              ;; only for if/do/select/where/forall ?
              ((f90-looking-at-associate)
               (setq icol (+ icol f90-associate-indent)))
              ((looking-at f90-end-if-re)
               (setq icol (- icol f90-if-indent)))
              ((looking-at f90-end-associate-re)
               (setq icol (- icol f90-associate-indent)))
              ((f90-looking-at-end-critical)
               (setq icol (- icol f90-critical-indent)))
              ((looking-at "end[ \t]*do\\>")
               (setq icol (- icol f90-do-indent))))
        (end-of-line))
      icol)))

(defun f90-calculate-indent ()
  "Calculate the indent column based on previous statements."
  (interactive)
  (let (icol cont (case-fold-search t) (pnt (point)))
    (save-excursion
      (if (not (f90-previous-statement))
          ;; If f90-previous-statement returns nil, we must have been
          ;; called from on or before the first line of the first statement.
          (setq icol (if (or (save-excursion
                               (goto-char pnt)
                               (beginning-of-line)
                               ;; Preprocessor line before code statement.
                               (looking-at "[ \t]*#"))
                             (progn
                               ;; f90-previous-statement has moved us over
                               ;; comment/blank lines, so we need to get
                               ;; back to the first code statement.
                               (when (looking-at "[ \t]*\\([!#]\\|$\\)")
                                 (f90-next-statement))
                               (skip-chars-forward " \t0-9")
                               (f90-looking-at-program-block-start)))
                         0
                       ;; No explicit PROGRAM start statement.
                       f90-program-indent))
        (setq cont (f90-present-statement-cont))
        (if (eq cont 'end)
            (while (not (eq 'begin (f90-present-statement-cont)))
              (f90-previous-statement)))
        (cond ((eq cont 'begin)
               (setq icol (+ (f90-current-indentation)
                             f90-continuation-indent)))
              ((eq cont 'middle) (setq icol (current-indentation)))
              (t (setq icol (f90-current-indentation))
                 (skip-chars-forward " \t")
                 (if (looking-at "[0-9]")
                     (setq icol (f90-get-correct-indent))
                   (cond ((or (f90-looking-at-if-then)
                              (f90-looking-at-where-or-forall)
                              (f90-looking-at-select-case)
                              (looking-at f90-else-like-re))
                          (setq icol (+ icol f90-if-indent)))
                         ((f90-looking-at-do)
                          (setq icol (+ icol f90-do-indent)))
                         ((f90-looking-at-type-like)
                          (setq icol (+ icol f90-type-indent)))
                         ((f90-looking-at-associate)
                          (setq icol (+ icol f90-associate-indent)))
                         ((f90-looking-at-critical)
                          (setq icol (+ icol f90-critical-indent)))
                         ((or (f90-looking-at-program-block-start)
                              (looking-at "contains[ \t]*\\($\\|!\\)"))
                          (setq icol (+ icol f90-program-indent)))))
                 (goto-char pnt)
                 (beginning-of-line)
                 (cond ((looking-at "[ \t]*$"))
                       ((looking-at "[ \t]*#") ; check for cpp directive
                        (setq icol 0))
                       (t
                        (skip-chars-forward " \t0-9")
                        (cond ((or (looking-at f90-else-like-re)
                                   (looking-at f90-end-if-re))
                               (setq icol (- icol f90-if-indent)))
                              ((looking-at "end[ \t]*do\\>")
                               (setq icol (- icol f90-do-indent)))
                              ((looking-at f90-end-type-re)
                               (setq icol (- icol f90-type-indent)))
                              ((looking-at f90-end-associate-re)
                               (setq icol (- icol f90-associate-indent)))
                              ((f90-looking-at-end-critical)
                               (setq icol (- icol f90-critical-indent)))
                              ((or (looking-at "contains[ \t]*\\(!\\|$\\)")
                                   (f90-looking-at-program-block-end))
                               (setq icol (- icol f90-program-indent))))))))))
    icol))

(defun f90-previous-statement ()
  "Move point to beginning of the previous F90 statement.
If no previous statement is found (i.e. if called from the first
statement in the buffer), move to the start of the buffer and
return nil.  A statement is a line which is neither blank nor a
comment."
  (interactive)
  (let (not-first-statement)
    (beginning-of-line)
    (while (and (setq not-first-statement (zerop (forward-line -1)))
                (looking-at "[ \t0-9]*\\(!\\|$\\|#\\)")))
    not-first-statement))

(defun f90-next-statement ()
  "Move point to beginning of the next F90 statement.
Return nil if no later statement is found."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
                      (and (zerop (forward-line 1))
                           (not (eobp))))
                (looking-at "[ \t0-9]*\\(!\\|$\\|#\\)")))
    not-last-statement))

(defun f90-beginning-of-subprogram ()
  "Move point to the beginning of the current subprogram.
Return (TYPE NAME), or nil if not found."
  (interactive)
  (let ((count 1) (case-fold-search t) matching-beg)
    (beginning-of-line)
    (while (and (> count 0)
                (re-search-backward f90-program-block-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((setq matching-beg (f90-looking-at-program-block-start))
             (setq count (1- count)))
            ((f90-looking-at-program-block-end)
             (setq count (1+ count)))))
    (beginning-of-line)
    (if (zerop count)
        matching-beg
      ;; Note this includes the case of an un-named main program,
      ;; in which case we go to (point-min).
      (if (called-interactively-p 'interactive)
	  (message "No beginning found"))
      nil)))

(defun f90-end-of-subprogram ()
  "Move point to the end of the current subprogram.
Return (TYPE NAME), or nil if not found."
  (interactive)
  (let ((case-fold-search t)
        (count 1)
        matching-end)
    (end-of-line)
    (while (and (> count 0)
                (re-search-forward f90-program-block-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((f90-looking-at-program-block-start)
             (setq count (1+ count)))
            ((setq matching-end (f90-looking-at-program-block-end))
             (setq count (1- count))))
      (end-of-line))
    ;; This means f90-end-of-subprogram followed by f90-start-of-subprogram
    ;; has a net non-zero effect, which seems odd.
;;;    (forward-line 1)
    (if (zerop count)
        matching-end
      (if (called-interactively-p 'interactive)
	  (message "No end found"))
      nil)))


(defun f90-end-of-block (&optional num)
  "Move point forward to the end of the current code block.
With optional argument NUM, go forward that many balanced blocks.
If NUM is negative, go backward to the start of a block.  Checks
for consistency of block types and labels (if present), and
completes outermost block if `f90-smart-end' is non-nil.
Interactively, pushes mark before moving point."
  (interactive "p")
  ;; Can move some distance.
  (if (called-interactively-p 'any) (push-mark (point) t))
  (and num (< num 0) (f90-beginning-of-block (- num)))
  (let ((f90-smart-end (if f90-smart-end 'no-blink)) ; for final match-end
        (case-fold-search t)
        (count (or num 1))
        start-list start-this start-type start-label end-type end-label)
    (end-of-line)                       ; probably want this
    (while (and (> count 0) (re-search-forward f90-blocks-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((or (f90-in-string) (f90-in-comment)))
            ((setq start-this
                   (or
                    (f90-looking-at-do)
                    (f90-looking-at-select-case)
                    (f90-looking-at-type-like)
                    (f90-looking-at-associate)
                    (f90-looking-at-critical)
                    (f90-looking-at-program-block-start)
                    (f90-looking-at-if-then)
                    (f90-looking-at-where-or-forall)))
             (setq start-list (cons start-this start-list) ; not add-to-list!
                   count (1+ count)))
            ((looking-at (concat "end[ \t]*" f90-blocks-re
                                 "[ \t]*\\(\\sw+\\)?"))
             (setq end-type (match-string 1)
                   end-label (match-string 2)
                   count (1- count))
             ;; Check any internal blocks.
             (when start-list
               (setq start-this (car start-list)
                     start-list (cdr start-list)
                     start-type (car start-this)
                     start-label (cadr start-this))
               (or (f90-equal-symbols start-type end-type)
                   (error "End type `%s' does not match start type `%s'"
                          end-type start-type))
               (or (f90-equal-symbols start-label end-label)
                   (error "End label `%s' does not match start label `%s'"
                          end-label start-label)))))
      (end-of-line))
    (if (> count 0) (error "Missing block end"))
    ;; Check outermost block.
    (when f90-smart-end
      (save-excursion
        (beginning-of-line)
        (skip-chars-forward " \t0-9")
        (f90-match-end)))))

(defun f90-beginning-of-block (&optional num)
  "Move point backwards to the start of the current code block.
With optional argument NUM, go backward that many balanced blocks.
If NUM is negative, go forward to the end of a block.
Checks for consistency of block types and labels (if present).
Does not check the outermost block, because it may be incomplete.
Interactively, pushes mark before moving point."
  (interactive "p")
  (if (called-interactively-p 'any) (push-mark (point) t))
  (and num (< num 0) (f90-end-of-block (- num)))
  (let ((case-fold-search t)
        (count (or num 1))
        end-list end-this end-type end-label
        start-this start-type start-label)
    (beginning-of-line)                 ; probably want this
    (while (and (> count 0) (re-search-backward f90-blocks-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((or (f90-in-string) (f90-in-comment)))
            ((looking-at (concat "end[ \t]*" f90-blocks-re
                                 "[ \t]*\\(\\sw+\\)?"))
             (setq end-list (cons (list (match-string 1) (match-string 2))
                                  end-list)
                   count (1+ count)))
            ((setq start-this
                   (or
                    (f90-looking-at-do)
                    (f90-looking-at-select-case)
                    (f90-looking-at-type-like)
                    (f90-looking-at-associate)
                    (f90-looking-at-critical)
                    (f90-looking-at-program-block-start)
                    (f90-looking-at-if-then)
                    (f90-looking-at-where-or-forall)))
             (setq start-type (car start-this)
                   start-label (cadr start-this)
                   count (1- count))
             ;; Check any internal blocks.
             (when end-list
               (setq end-this (car end-list)
                     end-list (cdr end-list)
                     end-type (car end-this)
                     end-label (cadr end-this))
               (or (f90-equal-symbols start-type end-type)
                   (error "Start type `%s' does not match end type `%s'"
                          start-type end-type))
               (or (f90-equal-symbols start-label end-label)
                   (error "Start label `%s' does not match end label `%s'"
                          start-label end-label))))))
    ;; Includes an un-named main program block.
    (if (> count 0) (error "Missing block start"))))

(defun f90-next-block (&optional num)
  "Move point forward to the next end or start of a code block.
With optional argument NUM, go forward that many blocks.
If NUM is negative, go backwards.
A block is a subroutine, if-endif, etc."
  (interactive "p")
  (let ((case-fold-search t)
        (count (if num (abs num) 1)))
    (while (and (> count 0)
                (if (> num 0) (re-search-forward f90-blocks-re nil 'move)
                  (re-search-backward f90-blocks-re nil 'move)))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((or (f90-in-string) (f90-in-comment)))
            ((or
              (looking-at "end[ \t]*")
              (f90-looking-at-do)
              (f90-looking-at-select-case)
              (f90-looking-at-type-like)
              (f90-looking-at-associate)
              (f90-looking-at-critical)
              (f90-looking-at-program-block-start)
              (f90-looking-at-if-then)
              (f90-looking-at-where-or-forall))
             (setq count (1- count))))
      (if (> num 0) (end-of-line)
        (beginning-of-line)))))


(defun f90-previous-block (&optional num)
  "Move point backward to the previous end or start of a code block.
With optional argument NUM, go backward that many blocks.
If NUM is negative, go forwards.
A block is a subroutine, if-endif, etc."
  (interactive "p")
  (f90-next-block (- (or num 1))))


(defun f90-mark-subprogram ()
  "Put mark at end of F90 subprogram, point at beginning, push mark."
  (interactive)
  (let ((pos (point)) program)
    (f90-end-of-subprogram)
    (push-mark)
    (goto-char pos)
    (setq program (f90-beginning-of-subprogram))
    (if (featurep 'xemacs)
        (zmacs-activate-region)
      (setq mark-active t
            deactivate-mark nil))
    program))

(defun f90-comment-region (beg-region end-region)
  "Comment/uncomment every line in the region.
Insert the variable `f90-comment-region' at the start of every line
in the region, or, if already present, remove it."
  (interactive "*r")
  (let ((end (copy-marker end-region)))
    (goto-char beg-region)
    (beginning-of-line)
    (if (looking-at (regexp-quote f90-comment-region))
        (delete-region (point) (match-end 0))
      (insert f90-comment-region))
    (while (and (zerop (forward-line 1))
                (< (point) end))
      (if (looking-at (regexp-quote f90-comment-region))
          (delete-region (point) (match-end 0))
        (insert f90-comment-region)))
    (set-marker end nil)))

(defun f90-indent-line (&optional no-update)
  "Indent current line as F90 code.
Unless optional argument NO-UPDATE is non-nil, call `f90-update-line'
after indenting."
  (interactive "*P")
  (let ((case-fold-search t)
        (pos (point-marker))
        indent no-line-number)
    (beginning-of-line)           ; digits after & \n are not line-nos
    (if (not (save-excursion (and (f90-previous-statement)
                                  (f90-line-continued))))
        (f90-indent-line-no)
      (setq no-line-number t)
      (skip-chars-forward " \t"))
    (if (looking-at "!")
        (setq indent (f90-comment-indent))
      (and f90-smart-end (looking-at "end")
           (f90-match-end))
      (setq indent (f90-calculate-indent)))
    (or (= indent (current-column))
        (f90-indent-to indent no-line-number))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (and (< (point) pos)
         (goto-char pos))
    (if auto-fill-function
        ;; GM NO-UPDATE not honored, since this calls f90-update-line.
        (f90-do-auto-fill)
      (or no-update (f90-update-line)))
    (set-marker pos nil)))

(defun f90-indent-new-line ()
  "Re-indent current line, insert a newline and indent the newline.
An abbrev before point is expanded if the variable `abbrev-mode' is non-nil.
If run in the middle of a line, the line is not broken."
  (interactive "*")
  (if abbrev-mode (expand-abbrev))
  (beginning-of-line)             ; reindent where likely to be needed
  (f90-indent-line)                ; calls indent-line-no, update-line
  (end-of-line)
  (delete-horizontal-space)             ; destroy trailing whitespace
  (let ((string (f90-in-string))
        (cont (f90-line-continued)))
    (and string (not cont) (insert "&"))
    (newline)
    (if (or string (and cont f90-beginning-ampersand)) (insert "&")))
  (f90-indent-line 'no-update))         ; nothing to update


;; TODO not add spaces to empty lines at the start.
;; Why is second line getting extra indent over first?
(defun f90-indent-region (beg-region end-region)
  "Indent every line in region by forward parsing."
  (interactive "*r")
  (let ((end-region-mark (copy-marker end-region))
        (save-point (point-marker))
        (case-fold-search t)
        block-list ind-lev ind-curr ind-b cont struct beg-struct end-struct)
    (goto-char beg-region)
    ;; First find a line which is not a continuation line or comment.
    (beginning-of-line)
    (while (and (looking-at "[ \t]*[0-9]*\\(!\\|#\\|[ \t]*$\\)")
                (progn (f90-indent-line 'no-update)
                       (zerop (forward-line 1)))
                (< (point) end-region-mark)))
    (setq cont (f90-present-statement-cont))
    (while (and (memq cont '(middle end))
                (f90-previous-statement))
      (setq cont (f90-present-statement-cont)))
    ;; Process present line for beginning of block.
    (setq f90-cache-position (point))
    (f90-indent-line 'no-update)
    (setq ind-lev (f90-current-indentation)
          ind-curr ind-lev)
    (beginning-of-line)
    (skip-chars-forward " \t0-9")
    (setq struct nil
          ind-b (cond ((setq struct (f90-looking-at-do)) f90-do-indent)
                      ((or (setq struct (f90-looking-at-if-then))
                           (setq struct (f90-looking-at-select-case))
                           (setq struct (f90-looking-at-where-or-forall))
                           (looking-at f90-else-like-re))
                       f90-if-indent)
                      ((setq struct (f90-looking-at-type-like))
                       f90-type-indent)
                      ((setq struct (f90-looking-at-associate))
                       f90-associate-indent)
                      ((setq struct (f90-looking-at-critical))
                       f90-critical-indent)
                      ((or (setq struct (f90-looking-at-program-block-start))
                           (looking-at "contains[ \t]*\\($\\|!\\)"))
                       f90-program-indent)))
    (if ind-b (setq ind-lev (+ ind-lev ind-b)))
    (if struct (setq block-list (cons struct block-list)))
    (while (and (f90-line-continued) (zerop (forward-line 1))
                (< (point) end-region-mark))
      (if (looking-at "[ \t]*!")
          (f90-indent-to (f90-comment-indent))
        (or (= (current-indentation)
               (+ ind-curr f90-continuation-indent))
            (f90-indent-to (+ ind-curr f90-continuation-indent) 'no-line-no))))
    ;; Process all following lines.
    (while (and (zerop (forward-line 1)) (< (point) end-region-mark))
      (beginning-of-line)
      (f90-indent-line-no)
      (setq f90-cache-position (point))
      (cond ((looking-at "[ \t]*$") (setq ind-curr 0))
            ((looking-at "[ \t]*#") (setq ind-curr 0))
            ((looking-at "!") (setq ind-curr (f90-comment-indent)))
            ((f90-no-block-limit) (setq ind-curr ind-lev))
            ((looking-at f90-else-like-re) (setq ind-curr
                                                 (- ind-lev f90-if-indent)))
            ((looking-at "contains[ \t]*\\($\\|!\\)")
             (setq ind-curr (- ind-lev f90-program-indent)))
            ((setq ind-b
                   (cond ((setq struct (f90-looking-at-do)) f90-do-indent)
                         ((or (setq struct (f90-looking-at-if-then))
                              (setq struct (f90-looking-at-select-case))
                              (setq struct (f90-looking-at-where-or-forall)))
                          f90-if-indent)
                         ((setq struct (f90-looking-at-type-like))
                          f90-type-indent)
                         ((setq struct (f90-looking-at-associate))
                          f90-associate-indent)
                         ((setq struct (f90-looking-at-critical))
                          f90-critical-indent)
                         ((setq struct (f90-looking-at-program-block-start))
                          f90-program-indent)))
             (setq ind-curr ind-lev)
             (if ind-b (setq ind-lev (+ ind-lev ind-b)))
             (setq block-list (cons struct block-list)))
            ((setq end-struct (f90-looking-at-program-block-end))
             (setq beg-struct (car block-list)
                   block-list (cdr block-list))
             (if f90-smart-end
                 (save-excursion
                   (f90-block-match (car beg-struct) (cadr beg-struct)
                                    (car end-struct) (cadr end-struct))))
             (setq ind-b
                   (cond ((looking-at f90-end-if-re) f90-if-indent)
                         ((looking-at "end[ \t]*do\\>")  f90-do-indent)
                         ((looking-at f90-end-type-re) f90-type-indent)
                         ((looking-at f90-end-associate-re)
                          f90-associate-indent)
                         ((f90-looking-at-end-critical) f90-critical-indent)
                         ((f90-looking-at-program-block-end)
                          f90-program-indent)))
             (if ind-b (setq ind-lev (- ind-lev ind-b)))
             (setq ind-curr ind-lev))
            (t (setq ind-curr ind-lev)))
      ;; Do the indentation if necessary.
      (or (= ind-curr (current-column))
          (f90-indent-to ind-curr))
      (while (and (f90-line-continued) (zerop (forward-line 1))
                  (< (point) end-region-mark))
        (if (looking-at "[ \t]*!")
            (f90-indent-to (f90-comment-indent))
          (or (= (current-indentation)
                 (+ ind-curr f90-continuation-indent))
              (f90-indent-to
               (+ ind-curr f90-continuation-indent) 'no-line-no)))))
    ;; Restore point, etc.
    (setq f90-cache-position nil)
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)
    (if (featurep 'xemacs)
        (zmacs-deactivate-region)
      (deactivate-mark))))

(defun f90-indent-subprogram ()
  "Properly indent the subprogram containing point."
  (interactive "*")
  (save-excursion
    (let ((program (f90-mark-subprogram)))
      (if program
          (progn
            (message "Indenting %s %s..."
                     (car program) (cadr program))
            (indent-region (point) (mark) nil)
            (message "Indenting %s %s...done"
                     (car program) (cadr program)))
        (message "Indenting the whole file...")
        (indent-region (point) (mark) nil)
        (message "Indenting the whole file...done")))))

(defun f90-break-line (&optional no-update)
  "Break line at point, insert continuation marker(s) and indent.
Unless in a string or comment, or if the optional argument NO-UPDATE
is non-nil, call `f90-update-line' after inserting the continuation marker."
  (interactive "*P")
  (cond ((f90-in-string)
         (insert "&\n&"))
        ((f90-in-comment)
         (delete-horizontal-space) ; remove trailing whitespace
         (insert "\n" (f90-get-present-comment-type)))
        (t (insert "&")
           (or no-update (f90-update-line))
           (newline 1)
           ;; FIXME also need leading ampersand if split lexical token (eg ==).
           ;; Or respect f90-no-break-re.
           (if f90-beginning-ampersand (insert "&"))))
  (indent-according-to-mode))

(defun f90-find-breakpoint ()
  "From `fill-column', search backward for break-delimiter."
  ;; Commented text more likely than commented code.
  (if (f90-in-comment)
      (re-search-backward "\\s-" (line-beginning-position))
    (re-search-backward f90-break-delimiters (line-beginning-position))
    (if (not f90-break-before-delimiters)
        (forward-char (if (looking-at f90-no-break-re) 2 1))
      (backward-char)
      (or (looking-at f90-no-break-re)
        (forward-char)))))

(defun f90-do-auto-fill ()
  "Break line if non-white characters beyond `fill-column'.
Update keyword case first."
  (interactive "*")
  ;; Break line before or after last delimiter (non-word char) if
  ;; position is beyond fill-column.
  ;; Will not break **, //, or => (as specified by f90-no-break-re).
  (f90-update-line)
  ;; Need this for `f90-electric-insert' and other f90- callers.
  (unless (and (boundp 'comment-auto-fill-only-comments)
               comment-auto-fill-only-comments
               (not (f90-in-comment)))
    (while (> (current-column) fill-column)
      (let ((pos-mark (point-marker)))
        (move-to-column fill-column)
        (or (f90-in-string) (f90-find-breakpoint))
        (f90-break-line)
        (goto-char pos-mark)
        (set-marker pos-mark nil)))))

(defun f90-join-lines (&optional arg)
  "Join current line to previous, fix whitespace, continuation, comments.
With optional argument ARG, join current line to following line.
Like `join-line', but handles F90 syntax."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (when (eq (preceding-char) ?\n)
    (skip-chars-forward " \t")
    (if (looking-at "\&") (delete-char 1))
    (beginning-of-line)
    (delete-region (point) (1- (point)))
    (skip-chars-backward " \t")
    (and (eq (preceding-char) ?&) (delete-char -1))
    (and (f90-in-comment)
         (looking-at "[ \t]*!+")
         (replace-match ""))
    (or (f90-in-string)
        (fixup-whitespace))))

(defun f90-fill-region (beg-region end-region)
  "Fill every line in region by forward parsing.  Join lines if possible."
  (interactive "*r")
  (let ((end-region-mark (copy-marker end-region))
        (go-on t)
        f90-smart-end f90-auto-keyword-case auto-fill-function)
    (goto-char beg-region)
    (while go-on
      ;; Join as much as possible.
      (while (progn
               (end-of-line)
               (skip-chars-backward " \t")
               (eq (preceding-char) ?&))
        (f90-join-lines 'forward))
      ;; Chop the line if necessary.
      (while (> (save-excursion (end-of-line) (current-column))
                fill-column)
        (move-to-column fill-column)
        (f90-find-breakpoint)
        (f90-break-line 'no-update))
      (setq go-on (and (< (point) end-region-mark)
                       (zerop (forward-line 1)))
            f90-cache-position (point)))
    (setq f90-cache-position nil)
    (set-marker end-region-mark nil)
    (if (featurep 'xemacs)
        (zmacs-deactivate-region)
      (deactivate-mark))))

(defun f90-block-match (beg-block beg-name end-block end-name)
  "Match end-struct with beg-struct and complete end-block if possible.
BEG-BLOCK is the type of block as indicated at the start (e.g., do).
BEG-NAME is the block start name (may be nil).
END-BLOCK is the type of block as indicated at the end (may be nil).
END-NAME is the block end name (may be nil).
Leave point at the end of line."
  ;; Hack to deal with the case when this is called from
  ;; f90-indent-region on a program block without an explicit PROGRAM
  ;; statement at the start. Should really be an error (?).
  (or beg-block (setq beg-block "program"))
  (search-forward "end" (line-end-position))
  (catch 'no-match
    (if (and end-block (f90-equal-symbols beg-block end-block))
        (search-forward end-block)
      (if end-block
          (progn
            (message "END %s does not match %s." end-block beg-block)
            (end-of-line)
            (throw 'no-match nil))
        (message "Inserting %s." beg-block)
        (insert (concat " " beg-block))))
    (if (f90-equal-symbols beg-name end-name)
        (and end-name (search-forward end-name))
      (cond ((and beg-name (not end-name))
             (message "Inserting %s." beg-name)
             (insert (concat " " beg-name)))
            ((and beg-name end-name)
             (message "Replacing %s with %s." end-name beg-name)
             (search-forward end-name)
             (replace-match beg-name))
            ((and (not beg-name) end-name)
             (message "Deleting %s." end-name)
             (search-forward end-name)
             (replace-match ""))))
    (or (looking-at "[ \t]*!") (delete-horizontal-space))))

(defun f90-match-end ()
  "From an end block statement, find the corresponding block and name."
  (interactive)
  (let ((count 1)
        (top-of-window (window-start))
        (end-point (point))
        (case-fold-search t)
        matching-beg beg-name end-name beg-block end-block end-struct)
    (when (save-excursion (beginning-of-line) (skip-chars-forward " \t0-9")
                          (setq end-struct (f90-looking-at-program-block-end)))
      (setq end-block (car end-struct)
            end-name  (cadr end-struct))
      (save-excursion
        (beginning-of-line)
        (while (and (> count 0)
                    (not (= (line-beginning-position) (point-min))))
          (re-search-backward f90-blocks-re nil 'move)
          (beginning-of-line)
          ;; GM not a line number if continued line.
;;;          (skip-chars-forward " \t")
;;;          (skip-chars-forward "0-9")
          (skip-chars-forward " \t0-9")
          (cond ((or (f90-in-string) (f90-in-comment)))
                ((setq matching-beg
                       (or
                        (f90-looking-at-do)
                        (f90-looking-at-if-then)
                        (f90-looking-at-where-or-forall)
                        (f90-looking-at-select-case)
                        (f90-looking-at-type-like)
                        (f90-looking-at-associate)
                        (f90-looking-at-critical)
                        (f90-looking-at-program-block-start)
                        ;; Interpret a single END without a block
                        ;; start to be the END of a program block
                        ;; without an initial PROGRAM line.
                        (if (= (line-beginning-position) (point-min))
                            '("program" nil))))
                 (setq count (1- count)))
                ((looking-at (concat "end[ \t]*" f90-blocks-re))
                 (setq count (1+ count)))))
        (if (> count 0)
            (message "No matching beginning.")
          (f90-update-line)
          (if (eq f90-smart-end 'blink)
              (if (< (point) top-of-window)
                  (message "Matches %s: %s"
                           (what-line)
                           (buffer-substring
                            (line-beginning-position)
                            (line-end-position)))
                (sit-for blink-matching-delay)))
          (setq beg-block (car matching-beg)
                beg-name (cadr matching-beg))
          (goto-char end-point)
          (beginning-of-line)
          (f90-block-match beg-block beg-name end-block end-name))))))

(defun f90-insert-end ()
  "Insert a complete end statement matching beginning of present block."
  (interactive "*")
  (let ((f90-smart-end (or f90-smart-end 'blink)))
    (insert "end")
    (f90-indent-new-line)))

;; Abbrevs and keywords.

(defun f90-abbrev-start ()
  "Typing `\\[help-command] or `? lists all the F90 abbrevs.
Any other key combination is executed normally."
  (interactive "*")
  (insert last-command-event)
  (let (char event)
    (if (fboundp 'next-command-event) ; XEmacs
        (setq event (next-command-event)
              char (and (fboundp 'event-to-character)
                        (event-to-character event)))
      (setq event (read-event)
            char event))
    ;; Insert char if not equal to `?', or if abbrev-mode is off.
    (if (and abbrev-mode (memq char (list ?? help-char)))
        (f90-abbrev-help)
      (setq unread-command-events (list event)))))

(defun f90-abbrev-help ()
  "List the currently defined abbrevs in F90 mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (f90-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun f90-prepare-abbrev-list-buffer ()
  "Create a buffer listing the F90 mode abbreviations."
  (with-current-buffer (get-buffer-create "*Abbrevs*")
    (erase-buffer)
    (insert-abbrev-table-description 'f90-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun f90-upcase-keywords ()
  "Upcase all F90 keywords in the buffer."
  (interactive "*")
  (f90-change-keywords 'upcase-word))

(defun f90-capitalize-keywords ()
  "Capitalize all F90 keywords in the buffer."
  (interactive "*")
  (f90-change-keywords 'capitalize-word))

(defun f90-downcase-keywords ()
  "Downcase all F90 keywords in the buffer."
  (interactive "*")
  (f90-change-keywords 'downcase-word))

(defun f90-upcase-region-keywords (beg end)
  "Upcase all F90 keywords in the region."
  (interactive "*r")
  (f90-change-keywords 'upcase-word beg end))

(defun f90-capitalize-region-keywords (beg end)
  "Capitalize all F90 keywords in the region."
  (interactive "*r")
  (f90-change-keywords 'capitalize-word beg end))

(defun f90-downcase-region-keywords (beg end)
  "Downcase all F90 keywords in the region."
  (interactive "*r")
  (f90-change-keywords 'downcase-word beg end))

;; Change the keywords according to argument.
(defun f90-change-keywords (change-word &optional beg end)
  "Change the case of F90 keywords in the region (if specified) or buffer.
CHANGE-WORD should be one of 'upcase-word, 'downcase-word, 'capitalize-word."
  (save-excursion
    (setq beg (or beg (point-min))
          end (or end (point-max)))
    (let ((keyword-re
           (concat "\\("
                   f90-keywords-re "\\|" f90-procedures-re "\\|"
                   f90-hpf-keywords-re "\\|" f90-operators-re "\\)"))
          (ref-point (point-min))
          (modified (buffer-modified-p))
          state saveword back-point)
      (goto-char beg)
      (unwind-protect
          (while (re-search-forward keyword-re end t)
            (unless (progn
                      (setq state (parse-partial-sexp ref-point (point)))
                      (or (nth 3 state) (nth 4 state)
                          ;; GM f90-directive-comment-re?
                          (save-excursion ; check for cpp directive
                            (beginning-of-line)
                            (skip-chars-forward " \t0-9")
                            (looking-at "#"))))
              (setq ref-point (point)
                    back-point (save-excursion (backward-word 1) (point))
                    saveword (buffer-substring back-point ref-point))
              (funcall change-word -1)
              (or (string= saveword (buffer-substring back-point ref-point))
                  (setq modified t))))
        (or modified (restore-buffer-modified-p nil))))))


(defun f90-current-defun ()
  "Function to use for `add-log-current-defun-function' in F90 mode."
  (save-excursion
    (nth 1 (f90-beginning-of-subprogram))))

(defun f90-backslash-not-special (&optional all)
  "Make the backslash character (\\) be non-special in the current buffer.
With optional argument ALL, change the default for all present
and future F90 buffers.  F90 mode normally treats backslash as an
escape character."
  (or (derived-mode-p 'f90-mode)
      (error "This function should only be used in F90 buffers"))
  (when (equal (char-syntax ?\\ ) ?\\ )
    (or all (set-syntax-table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?\\ ".")))


(provide 'f90)

;; Local Variables:
;; coding: utf-8
;; End:

;;; f90.el ends here
